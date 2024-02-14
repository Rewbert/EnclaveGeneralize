{-# LANGUAGE PatternSynonyms #-}
{- | This module is a pile of burning crap, as I manually wrote out the entire
AST stuff. There are nicer ways of doing these things, but I learned TH as I went along
and found myself too deep in to go back and rework it. -}
module GenModule where

import           Derulo
import           Language.Haskell.TH.Ppr    (pprint)
import           Language.Haskell.TH.Syntax

import           Data.List
import           System.Environment         (getArgs)

-- | An endpoint can assume the role of a client or a server
-- A client will never wait for a message, unless it has first sent out a request
-- a server continuously waits to service requests
data Role = Client | Server deriving (Eq, Show)

-- | An endpoint has a name, a role, and a location
-- TODO this is where we are going to add security labels
data Endpoint = Endpoint
  { -- | Name of the endpoint
    name     :: String,
    -- | Role of the endpoint
    role     :: GenModule.Role,
    -- | ip-socket pair for the endpoint
    location :: (String, Int)
  } deriving Show

configure :: Q [Dec]
configure = do
  args <- runIO getArgs
  runIO $ putStrLn $ show args
  actors <- runIO configFromJSON
  return $ genModule actors

configFromJSON :: IO Actors
configFromJSON = do
  contents <- readFile "config.json"
  case readJSON contents of
    Just json -> case parseContents json of
      Just actors -> return actors
      Nothing     -> error "error parsing configuration..."
    Nothing -> error "error parsing configuration..."

parseContents :: JSON -> Maybe Actors
parseContents (Object objects) = do
  let [("endpoints", Object endpoints), ("focused", String focused)] = sort objects
  xs <- mapM parseEndpoint endpoints
  let (current, dummies) = partition (\ep -> name ep == focused) xs
  case current of
    [current] -> return (current, dummies)
    _         -> Nothing
  where
    parseEndpoint :: (String, JSON) -> Maybe Endpoint
    parseEndpoint (name, Object attributes) = do
      role <- findRole
      location <- findLocation
      return $ Endpoint name role location
      where
        findRole :: Maybe GenModule.Role
        findRole = case filter (\(n,_) -> n == "role") attributes of
          []                     -> Nothing
          [(_, String "client")] -> Just Client
          [(_, String "server")] -> Just Server
          _                      -> Nothing

        findLocation :: Maybe (String, Int)
        findLocation = case filter (\(n,_) -> n == "location") attributes of
          [] -> Nothing
          [(_, Object [("ip", String ip), ("port", Number port _)])] -> Just (ip, fromInteger port)
          [(_, Object [("port", Number port _), ("ip", String ip)])] -> Just (ip, fromInteger port)
          _ -> Nothing

-- | A whole application is made up of different endpoints, collectively referred to as the
-- actors of a system. One actor is the one we are currently compiling code for, and the others
-- are the remote endpoints. The list of endpoints does not contain the current compilation target.
type Actors = (Endpoint, [Endpoint])

genModule :: Actors -> [Dec]
genModule (current, dummies) = case role current of
  Client -> concat
    [ generateCurrentClient current,
      concatMap generateDummyClient dummyClients,
      concatMap generateDummyEnclave dummyEnclaves,
      [dummySecurable],
      dummyMkSecureInstances dummyEnclaves,
      correctRunClient,
      correctRunClients,
      clientRunApp,
      constants
    ]
  Server -> concat
    [ generateCurrentEnclave current, -- FIXME fix so that we don't generate MonadIO here
      concatMap generateDummyClient dummyClients,
      concatMap generateDummyEnclave dummyEnclaves,
      [concreteSecurable current],
      concreteMkSecureInstances current dummyEnclaves,
      [dummyRunClients],
      enclaveRunApp current,
      constants
    ]
  where
    dummyClients :: [Endpoint]
    dummyClients = filter (\ep -> role ep == Client) dummies

    dummyEnclaves :: [Endpoint]
    dummyEnclaves = filter (\ep -> role ep == Server) dummies

    constants :: [Dec]
    constants = [gatewayClass, inEnclaveClass] <> closures

-- * Example

client1 :: Endpoint
client1 = Endpoint "Client1" Client ("127.0.0.1", 8000)

client2 :: Endpoint
client2 = Endpoint "Client2" Client ("127.0.0.1", 8001)

enclave1 :: Endpoint
enclave1 = Endpoint "Enclave1" Server ("127.0.0.1", 8002)

enclave2 :: Endpoint
enclave2 = Endpoint "Enclave2" Server ("127.0.0.1", 8003)

-- | Example configuration
-- FIXME this should be read and parsed from a JSON file or something
clientactors :: Actors
clientactors =
  ( client1,
    [ client2,
      enclave1,
      enclave2
    ]
  )

enclaveactors :: Actors
enclaveactors =
  ( enclave1,
    [ client1,
      client2,
      enclave2
    ]
  )

pattern NormalType :: b -> (Bang, b)
pattern NormalType t = (Bang NoSourceUnpackedness NoSourceStrictness, t)

pattern NormalTV :: Name -> TyVarBndr ()
pattern NormalTV v = PlainTV v ()

pattern Instance :: Name -> Name -> [Dec] -> Dec
pattern Instance f ep defs = InstanceD Nothing [] (AppT (ConT f) (ConT ep)) defs

pattern AppE3 :: Exp -> Exp -> Exp -> Exp
pattern AppE3 e1 e2 e3 = AppE (AppE e1 e2) e3

pattern AppE4 :: Exp -> Exp -> Exp -> Exp -> Exp
pattern AppE4 e1 e2 e3 e4 = AppE (AppE (AppE e1 e2) e3) e4

-- * Generate current Client

generateCurrentClient :: Endpoint -> [Dec]
generateCurrentClient ep = createNewtype ep : clientShouldRun ep : concat [concreteMonadStack ep, correctGateway ep, generateClientAction ep]

generateDummyClient :: Endpoint -> [Dec]
generateDummyClient ep =  createDummyType ep : clientShouldNotRun ep : concat [dummyMonadStack ep, dummyGateway ep, generateClientAction ep]

generateCurrentEnclave :: Endpoint -> [Dec]
generateCurrentEnclave ep = createNewtype ep : concat [concreteMonadStack ep, inConcreteEnclave ep, correctGateway ep, [concreteServerRef ep]]

generateDummyEnclave :: Endpoint -> [Dec]
generateDummyEnclave ep = createDummyType ep : concat [dummyMonadStack ep, inDummyEnclave ep, dummyGateway ep, [dummyServerRef ep]]

closures :: [Dec]
closures = secureD : secureAp

-- * Generate action impl
generateClientAction :: Endpoint -> [Dec]
generateClientAction ep = [funTy,fun]
  where
    -- liftClientN ca = liftIO $ do
    --   ref <- newIORef undefined
    --   toIO $ ca >>= liftIO . writeIORef ref
    --   readIORef ref
    funTy = SigD funName $ forAll $ ((-->) `AppT` clientA) `AppT` monadA
      where
        forAll = ForallT [] ctx
        ctx = [AppT (ConT . mkName $ "MonadIO") monad]
        (-->) = ArrowT
        a = VarT . mkName $ "a"
        monad = VarT . mkName $ "m"
        monadA = AppT monad a
        client = ConT . mkName . name $ ep
        clientA = AppT client a
    fun = FunD funName
      [ Clause [VarP (mkName "ca")]
               (NormalB $ AppE (VarE (mkName "liftIO")) $ DoE Nothing [stm1, stm2, stm3]) []
      ]
    stm1 = BindS (VarP $ mkName "ref") (AppE newIORef undefined)
    stm2 = NoBindS $ AppE (VarE $ mkName "toIO")
      (InfixE (Just . VarE $ mkName "ca") (VarE $ mkName ">>=")
        (Just $ InfixE (Just liftIO) dot (Just $ AppE writeIORef ref))
      )
    stm3 = NoBindS $ AppE readIORef ref

    funName = mkName $ "lift" <> name ep
    liftIO = VarE $ mkName "liftIO"
    writeIORef = VarE $ mkName "writeIORef"
    newIORef = VarE $ mkName "newIORef"
    readIORef = VarE $ mkName "readIORef"
    dot = VarE $ mkName "."
    ref = VarE $ mkName "ref"
    undefined = VarE (mkName "undefined")


-- | Generates a newtype for the current compilation target. E.g
-- will generate
--
-- @
-- newtype Client1 a = Client1 (IO a)
-- @
createNewtype :: Endpoint -> Dec
createNewtype ep = NewtypeD [] (mkName (name ep)) [NormalTV (mkName "a")] Nothing (NormalC (mkName (name ep)) [NormalType ioA]) []

ioA :: Type
ioA = AppT (ConT $ mkName "IO") (VarT $ mkName "a")

ioUnit :: Type
ioUnit = AppT (ConT $ mkName "IO") (TupleT 0)

createDummyType :: Endpoint -> Dec
createDummyType ep = DataD [] (mkName $ name ep) [NormalTV (mkName "a")] Nothing [NormalC (mkName $ name ep) []] []

-- * Variables

-- Declare variables ma, x, k, x', f as names, patterns, and expressions.

a :: Name
a = mkName "a"

aP :: Pat
aP = VarP a

aE :: Exp
aE = VarE a

aT :: Type
aT = VarT a

b :: Name
b = mkName "b"

bP :: Pat
bP = VarP b

bE :: Exp
bE = VarE b

bT :: Type
bT = VarT b

ma :: Name
ma = mkName "ma"

maP :: Pat
maP = VarP ma

maE :: Exp
maE = VarE ma

m :: Name
m = mkName "m"

mP :: Pat
mP = VarP m

mE :: Exp
mE = VarE m

mT :: Type
mT = VarT m

x :: Name
x = mkName "x"

xP :: Pat
xP = VarP x

xE :: Exp
xE = VarE x

xs :: Name
xs = mkName "xs"

xsP :: Pat
xsP = VarP xs

xsE :: Exp
xsE = VarE xs

k :: Name
k = mkName "k"

kP :: Pat
kP = VarP k

kE :: Exp
kE = VarE k

x' :: Name
x' = mkName "x'"

xP' :: Pat
xP' = VarP x'

xE' :: Exp
xE' = VarE x'

f :: Name
f = mkName "f"

fP :: Pat
fP = VarP f

fE :: Exp
fE = VarE f

id :: Name
id = mkName "id"

idP :: Pat
idP = VarP GenModule.id

idE :: Exp
idE = VarE $ GenModule.id

ip :: Name
ip = mkName "ip"

ipP :: Pat
ipP = VarP ip

ipE :: Exp
ipE = VarE ip

port :: Name
port = mkName "port"

portP :: Pat
portP = VarP port

portE :: Exp
portE = VarE port

args :: Name
args = mkName "args"

argsP :: Pat
argsP = VarP args

argsE :: Exp
argsE = VarE args

arg :: Name
arg = mkName "arg"

argP :: Pat
argP = VarP arg

argE :: Exp
argE = VarE arg

loc :: Name
loc = mkName "loc"

locP :: Pat
locP = VarP loc

locE :: Exp
locE = VarE loc


-- * Concrete instances

-- | Create instances for Functor, Applicative, Monad, and MonadIO
concreteMonadStack :: Endpoint -> [Dec]
concreteMonadStack ep = [concreteFunctor ep, concreteApplicative ep, concreteMonad ep, concreteMonadIO ep]

-- | Concrete functor instance for a client
concreteFunctor :: Endpoint -> Dec
concreteFunctor ep = Instance (mkName "Functor") (mkName $ name ep) [def]
  where
    def :: Dec
    def = FunD fmap [Clause [fP, ConP (mkName $ name ep) [] [maP]] body []]

    fmap :: Name
    fmap = mkName "fmap"

    body :: Body
    body = NormalB $ AppE (ConE $ mkName $ name ep) (AppE3 (VarE fmap) fE maE)

concreteApplicative :: Endpoint -> Dec
concreteApplicative ep = Instance (mkName "Applicative") (mkName $ name ep) [pure, ap]
  where
    pure :: Dec
    pure = FunD pureN [Clause [xP] pureBody []]

    pureN :: Name
    pureN = mkName "pure"

    epC :: Exp
    epC = ConE $ mkName $ name ep

    pureBody :: Body
    pureBody = NormalB $ AppE epC (AppE (VarE pureN) xE)

    ap :: Dec
    ap = FunD apN [Clause apLHS apRHS []]

    apN :: Name
    apN = mkName "<*>"

    apLHS :: [Pat]
    apLHS = [ConP (mkName $ name ep) [] [fP], ConP (mkName $ name ep) [] [xP]]

    apRHS :: Body
    apRHS = NormalB $ AppE (ConE $ mkName $ name ep) (UInfixE fE (VarE apN) xE)

concreteMonad :: Endpoint -> Dec
concreteMonad ep = Instance (mkName "Monad") (mkName $ name ep) [bind]
  where
    bind :: Dec
    bind = FunD bindN [Clause bindLHS bindRHS []]

    bindN :: Name
    bindN = mkName ">>="

    bindLHS :: [Pat]
    bindLHS = [ConP (mkName $ name ep) [] [maP], kP]

    epC :: Exp
    epC = ConE $ mkName $ name ep

    bindRHS :: Body
    bindRHS = NormalB $ AppE epC $ DoE Nothing [stm1, stm2, stm3]
      where
        stm1 :: Stmt
        stm1 = BindS xP maE

        stm2 :: Stmt
        stm2 = LetS [FunD x' [Clause [] (NormalB $ CaseE (AppE kE xE) [Match (ConP (mkName $ name ep) [] [xP']) (NormalB xE') []]) []]]

        stm3 :: Stmt
        stm3 = NoBindS xE'

concreteMonadIO :: Endpoint -> Dec
concreteMonadIO ep = Instance (mkName "MonadIO") (mkName $ name ep) [dec]
  where
    dec :: Dec
    dec = FunD liftIO [Clause liftIOLHS liftIORHS []]

    liftIO :: Name
    liftIO = mkName "liftIO"

    liftIOLHS :: [Pat]
    liftIOLHS = [VarP $ mkName "io"]

    liftIORHS :: Body
    liftIORHS = NormalB $ AppE (ConE $ mkName $ name ep) (VarE $ mkName "io")

-- * Dummy instances

-- | Create dummy instances for Functor, Applicative, Monad, and MonadIO
dummyMonadStack :: Endpoint -> [Dec]
dummyMonadStack ep = [dummyFunctor ep, dummyApplicative ep, dummyMonad ep, dummyMonadIO ep]
  where
    dummyBody :: Body
    dummyBody = NormalB $ ConE $ mkName $ name ep

    dummyFunctor :: Endpoint -> Dec
    dummyFunctor ep = Instance (mkName "Functor") (mkName $ name ep) [dummyFmap]
      where
        dummyFmap :: Dec
        dummyFmap = FunD (mkName "fmap") [Clause [WildP, WildP] dummyBody []]

    dummyApplicative :: Endpoint -> Dec
    dummyApplicative ep = Instance (mkName "Applicative") (mkName $ name ep) [dummyPure, dummyAp]
      where
        dummyPure :: Dec
        dummyPure = FunD (mkName "pure") [Clause [WildP] dummyBody []]

        dummyAp :: Dec
        dummyAp = FunD (mkName "<*>") [Clause [WildP, WildP] dummyBody []]

    dummyMonad :: Endpoint -> Dec
    dummyMonad ep = Instance (mkName "Monad") (mkName $ name ep) [dummyBind]
      where
        dummyBind :: Dec
        dummyBind = FunD (mkName ">>=") [Clause [WildP, WildP] dummyBody []]

    dummyMonadIO :: Endpoint -> Dec
    dummyMonadIO ep = Instance (mkName "MonadIO") (mkName $ name ep) [dummyLiftIO]
      where
        dummyLiftIO :: Dec
        dummyLiftIO = FunD (mkName "liftIO") [Clause [WildP] dummyBody []]

-- * ShouldRun

-- | Instance for a meaningful client
clientShouldRun :: Endpoint -> Dec
clientShouldRun ep = Instance (mkName "ShouldRun") (mkName $ name ep) [shouldIRun, toIO]
  where
    shouldIRun :: Dec
    shouldIRun = FunD (mkName "shouldIRun") [Clause [WildP] (NormalB $ ConE $ mkName "True") []]

    toIO :: Dec
    toIO = FunD (mkName "toIO") [Clause lhs rhs []]
      where
        lhs :: [Pat]
        lhs = [ConP (mkName $ name ep) [] [VarP $ mkName "io"]]

        rhs :: Body
        rhs = NormalB $ UInfixE (VarE $ mkName "io") (VarE $ mkName ">>") (AppE (VarE $ mkName "return") (TupE []))

-- | Instance for a dummy client
clientShouldNotRun :: Endpoint -> Dec
clientShouldNotRun ep = Instance (mkName "ShouldRun") (mkName $ name ep) [shouldIRun, toIO]
  where
    shouldIRun :: Dec
    shouldIRun = FunD (mkName "shouldIRun") [Clause [WildP] (NormalB $ ConE $ mkName "False") []]

    toIO :: Dec
    toIO = FunD (mkName "toIO") [Clause lhs rhs []]
      where
        lhs :: [Pat]
        lhs = [WildP]

        rhs :: Body
        rhs = NormalB $ AppE (VarE $ mkName "return") (TupE [])

-- * Gateway

gatewayClass :: Dec
gatewayClass = ClassD [] (mkName "GateWay") [PlainTV (mkName "m2") ()] [] [dec]
  where
    dec :: Dec
    dec = SigD (mkName "gateway") (ForallT [] constraints typ)
      where
        foralls :: [TyVarBndr Specificity]
        foralls = [PlainTV a SpecifiedSpec]

        constraints :: [Pred]
        constraints = [AppT (ConT $ mkName "Binary") aT]

        typ :: Type
        typ = AppT (AppT ArrowT (AppT (ConT $ mkName "Secure") (AppT mT aT))) (AppT (VarT $ mkName "m2") aT)

-- class GateWay m2 where
--   gateway :: Binary a => Secure (m a) -> m2 a

-- | Generate a meaningfull gateway and the GateWay instance
correctGateway :: Endpoint -> [Dec]
correctGateway ep = [SigD gatewayN (ForallT tyVars constraints fullT), dec, inst]
  where
    gatewayN :: Name
    gatewayN = mkName $ "gateway" <> name ep

    tyVars :: [TyVarBndr Specificity]
    tyVars = [PlainTV aN SpecifiedSpec, PlainTV mN SpecifiedSpec]

    constraints :: Cxt
    constraints = [AppT (ConT $ mkName "Binary") a]

    aN :: Name
    aN = mkName "a"

    a :: Type
    a = VarT aN

    mN :: Name
    mN = mkName "m"

    m :: Type
    m = VarT mN

    secureMA :: Type
    secureMA = AppT (ConT $ mkName "Secure") (AppT m a)

    domainT :: Type
    domainT = AppT (ConT $ mkName $ name ep) a

    fullT :: Type
    fullT = AppT (AppT (ConT $ mkName "->") secureMA) domainT

    dec :: Dec
    dec = FunD gatewayN [Clause lhs rhs []]
      where
        lhs :: [Pat]
        lhs = [ConP (mkName "Secure") [] [idP, TupP [ipP, portP], argsP]]

        rhs :: Body
        rhs = NormalB $ AppE (ConE $ mkName $ name ep) (AppE4 (VarE $ mkName "tryConnect") ipE portE (TupE [Just idE, Just argsE]))

    inst :: Dec
    inst = Instance (mkName "GateWay") (mkName $ name ep) [FunD (mkName "gateway") [Clause [xP] (NormalB $ AppE (VarE gatewayN) xE) []]]

-- | Generate a dummy gateway for an endpoint that has no semantics, as well as the GateWay instance
dummyGateway :: Endpoint -> [Dec]
dummyGateway ep = [SigD gatewayN (ForallT tyVars [] (AppT (ConT $ mkName $ name ep) (VarT $ mkName "a"))), dec, inst]
  where
    gatewayN :: Name
    gatewayN = mkName $ "gateway" <> name ep

    tyVars :: [TyVarBndr Specificity]
    tyVars = [PlainTV (mkName "a") SpecifiedSpec]

    dec :: Dec
    dec = FunD gatewayN [Clause [] (NormalB $ ConE $ mkName $ name ep) []]

    inst :: Dec
    inst = Instance (mkName "GateWay") (mkName $ name ep) [FunD (mkName "gateway") [Clause [WildP] (NormalB $ VarE gatewayN) []]]

-- * Secure

secureD :: Dec
secureD = DataD [] secureN [NormalTV (mkName "a")] Nothing [NormalC secureN [field1, field2, field3]] []
  where
    secureN :: Name
    secureN = mkName "Secure"

    field1 :: (Bang, Type)
    field1 = NormalType $ ConT $ mkName "Int"

    field2 :: (Bang, Type)
    field2 = NormalType $ AppT (AppT (TupleT 2) (ConT $ mkName "String")) (ConT $ mkName "Int")

    field3 :: (Bang, Type)
    field3 = NormalType $ AppT ListT (ConT $ mkName "B.ByteString")

secureAp :: [Dec]
secureAp = [sig, FunD secureApN [Clause [leftArg, rightArg] body []]]
  where
    secureApN :: Name
    secureApN = mkName "<@>"

    leftArg :: Pat
    leftArg = ConP (mkName "Secure") [] [idP, locP, argsP]

    rightArg :: Pat
    rightArg = argP

    body :: Body
    body = NormalB $ AppE4 (ConE $ mkName "Secure") idE locE (AppE3 (ConE $ mkName ":") (AppE (VarE $ mkName "encode") argE) argsE)

    sig :: Dec
    sig = SigD secureApN (ForallT tyVars context (AppT (AppT ArrowT field1t) (AppT (AppT ArrowT aT) domainT)))

    tyVars :: [TyVarBndr Specificity]
    tyVars = [PlainTV a SpecifiedSpec, PlainTV b SpecifiedSpec]

    context :: Cxt
    context = [AppT (ConT $ mkName "Binary") aT]

    a :: Name
    a = mkName "a"

    aT :: Type
    aT = VarT a

    b :: Name
    b = mkName "b"

    bT :: Type
    bT = VarT b

    field1t :: Type
    field1t = AppT (ConT $ mkName "Secure") (AppT (AppT (ConT $ mkName "->") aT) bT)

    domainT :: Type
    domainT = AppT (ConT $ mkName "Secure") bT

-- * RunClient

correctRunClient :: [Dec]
correctRunClient = [tysig, dec]
  where
    runClientN :: Name
    runClientN = mkName "runClient"

    tysig :: Dec
    tysig = SigD runClientN (ForallT [PlainTV (mkName "a") SpecifiedSpec] [] (AppT (AppT ArrowT ioA) ioUnit))

    dec :: Dec
    dec = FunD runClientN [Clause [maP] (NormalB $ DoE Nothing [stm1, stm2]) []]
      where
        stm1 :: Stmt
        stm1 = BindS xP maE

        stm2 :: Stmt
        stm2 = NoBindS $ UInfixE (VarE $ mkName "return") (VarE $ mkName "$") (UInfixE xE (VarE $ mkName "seq") (TupE []))

correctRunClients :: [Dec]
correctRunClients = [tysig, decs]
  where
    runClientsN :: Name
    runClientsN = mkName "runClients"

    tysig :: Dec
    tysig = SigD runClientsN (AppT (AppT ArrowT (AppT ListT (ConT $ mkName "ClientEndpoint"))) (AppT (ConT $ mkName "App") (TupleT 0)))

    decs :: Dec
    decs = FunD runClientsN [clause1, clause2]

    clause1 :: Clause
    clause1 = Clause [ListP []] (NormalB $ AppE (VarE $ mkName "return") (TupE [])) []

    clause2 :: Clause
    clause2 = Clause [InfixP (ConP (mkName "App.Endpoint") [] [SigP xP (AppT mT aT)]) (mkName ":") xsP] (NormalB wholeCase) []
      where
        scrut :: Exp
        scrut = AppE (VarE $ mkName "shouldIRun") (AppTypeE (ConE $ mkName "Proxy") mT)

        trueBranch :: Match
        trueBranch = Match (ConP (mkName "True") [] []) (NormalB $ UInfixE left (VarE $ mkName "$") right) []
          where
            left :: Exp
            left = VarE $ mkName "liftIO"

            right :: Exp
            right = AppE (VarE $ mkName "runClient") (AppE (VarE $ mkName "toIO") xE)

        falseBranch :: Match
        falseBranch = Match (ConP (mkName "False") [] []) (NormalB $ AppE (VarE $ mkName "runClients") xsE) []

        wholeCase :: Exp
        wholeCase = CaseE scrut [trueBranch, falseBranch]

dummyRunClients :: Dec
dummyRunClients = FunD (mkName "runClients") [Clause [WildP] (NormalB $ AppE (VarE $ mkName "return") (TupE [])) []]

-- * InEnclave

inEnclaveClass :: Dec
inEnclaveClass = ClassD [] (mkName "InEnclave") [NormalTV f] [] [dec]
  where
    dec :: Dec
    dec = SigD (mkName "inEnclave") ty

    ty :: Type
    ty = ForallT [] constraints typ
      where
        constraints :: Cxt
        constraints = [AppT (ConT $ mkName "Securable") aT, AppT (AppT (ConT $ mkName "~") (VarT f)) (AppT (ConT $ mkName "Remote") aT)]

        typ :: Type
        typ = AppT (AppT ArrowT aT) (AppT (ConT $ mkName "App") (AppT (ConT $ mkName "Secure") aT))

inConcreteEnclave :: Endpoint -> [Dec]
inConcreteEnclave ep = [sig, dec, inst]
  where
    inEnclaveN :: Name
    inEnclaveN = mkName $ "in" <> name ep

    aN :: Name
    aN = mkName "a"

    sig :: Dec
    sig = SigD inEnclaveN (ForallT foralls constraints typ)
      where
        foralls :: [TyVarBndr Specificity]
        foralls = [PlainTV aN SpecifiedSpec]

        constraints :: Cxt
        constraints = [AppT (ConT$ mkName "Securable") (VarT aN)]

        typ :: Type
        typ = AppT (AppT ArrowT aT) (AppT (ConT $ mkName "App") (AppT (ConT $ mkName "Secure") aT))

    dec :: Dec
    dec = FunD inEnclaveN [Clause [fP] (NormalB $ DoE Nothing [stm1, stm2, stm3]) []]
      where
        stm1 :: Stmt
        stm1 = BindS xP (VarE $ mkName "ST.get")

        stm2 :: Stmt
        stm2 = NoBindS $ UInfixE (VarE $ mkName "ST.put") (VarE $ mkName "$") recupd
          where
            recupd :: Exp
            recupd = RecUpdE xE [field1, field2]
              where
                field1 :: FieldExp
                field1 = (mkName "counter", UInfixE (AppE (VarE $ mkName "counter") xE) (VarE $ mkName "+") (LitE $ IntegerL 1))

                field2 :: FieldExp
                field2 = (mkName "funs", AppE4 (VarE $ mkName "Map.insert") (AppE (VarE $ mkName "counter") xE) fun (AppE (VarE $ mkName "funs") xE))

                fun :: Exp
                fun = LamE [xsP] $ CaseE (AppE3 (VarE $ mkName "mkSecure") fE xsE) [Match (ConP (mkName $ name ep) [] [maP]) (NormalB maE) []]

        stm3 :: Stmt
        stm3 = NoBindS $ UInfixE (VarE $ mkName "return") (VarE $ mkName "$") rightop
          where
            rightop :: Exp
            rightop = AppE4 (ConE $ mkName "Secure") (AppE (VarE $ mkName "counter") xE) (TupE [Just ip, Just port]) (ListE [])

            ip :: Exp
            ip = LitE $ StringL $ fst $ GenModule.location ep

            port :: Exp
            port = LitE $ IntegerL $ toInteger $ snd $ GenModule.location ep

    inst :: Dec
    inst = Instance (mkName "InEnclave") (mkName $ name ep) [dec]
      where
        dec :: Dec
        dec = FunD (mkName "inEnclave") [Clause [xP] (NormalB $ AppE (VarE inEnclaveN) xE) []]

-- instance InEnclave Enclave1 where
--   inEnclave' x = inEnclave1 x

-- inEnclave1 :: forall a. (Securable a) => a -> App (Secure a)
-- inEnclave1 f = do
--   st <- ST.get
--   ST.put $ st {counter = counter st + 1, funs = Map.insert (counter st) (\bs -> let Enclave1 ma = mkSecure f bs in ma) (funs st)}
--   return $ SecureDummy

inDummyEnclave :: Endpoint -> [Dec]
inDummyEnclave ep = inEnclave ep
  where
    inEnclave :: Endpoint -> [Dec]
    inEnclave ep = [sigT, dec, inst]
      where
        inEnclaveN :: Name
        inEnclaveN = mkName $ "in" <> name ep

        aN :: Name
        aN = mkName "a"

        sigT :: Dec
        sigT = SigD inEnclaveN $ ForallT foralls constraints typ
          where
            foralls :: [TyVarBndr Specificity]
            foralls = [PlainTV aN SpecifiedSpec]

            constraints :: Cxt
            constraints = [AppT (ConT $ mkName "Securable") (VarT aN)]

            typ :: Type
            typ = (AppT (ConT $ mkName "App") (AppT (ConT $ mkName "Secure") (VarT aN)))

        dec :: Dec
        dec = FunD inEnclaveN [Clause [] (NormalB $ DoE Nothing [stm1, stm2, stm3]) []]
          where
            stm1 :: Stmt
            stm1 = BindS xP (VarE $ mkName "ST.get")

            stm2 :: Stmt
            stm2 = NoBindS $ UInfixE (VarE $ mkName "ST.put") (VarE $ mkName "$") (RecUpdE xE [(counterN, UInfixE (AppE (VarE counterN) xE) (VarE $ mkName "+") (LitE $ IntegerL 1))])
              where
                counterN :: Name
                counterN = mkName "counter"

            stm3 :: Stmt
            stm3 = NoBindS tle
              where
                tle :: Exp
                tle = UInfixE (VarE $ mkName "return") (VarE $ mkName "$") rightop

                rightop :: Exp
                rightop = AppE4 (ConE $ mkName "Secure") (AppE (VarE $ mkName "counter") xE) (TupE [Just ip, Just port]) (ListE [])

                ip :: Exp
                ip = LitE $ StringL $ fst $ GenModule.location ep

                port :: Exp
                port = LitE $ IntegerL $ toInteger $ snd $ GenModule.location ep

        inst :: Dec
        inst = InstanceD Nothing [] (AppT (ConT (mkName "InEnclave")) (ConT $ mkName $ name ep)) [dec]
          where
            dec :: Dec
            dec = FunD (mkName "inEnclave") [Clause [WildP] (NormalB $ VarE inEnclaveN) []]

-- * Securable

dummySecurable :: Dec
dummySecurable = classD
  where
    securableN :: Name
    securableN = mkName "Securable"

    a :: Name
    a = mkName "a"

    mkSecure :: Name
    mkSecure = mkName "mkSecure"

    classD :: Dec
    classD = ClassD [] securableN [NormalTV a] [] [SigD mkSecure sig]
      where
        sig :: Type
        sig = AppT (AppT ArrowT (VarT a)) (AppT (AppT ArrowT (AppT ListT bs)) (ConT bsN))

        bsN :: Name
        bsN = mkName "B.ByteString"

        bs :: Type
        bs = ConT bsN

-- | Takes a list of dummy enclaves as input and produces the corresponding mkSecure instances
dummyMkSecureInstances :: [Endpoint] -> [Dec]
dummyMkSecureInstances eps = map dummyInst eps ++ [appDec]
  where
    securableN :: Name
    securableN = mkName "Securable"

    dummyInst :: Endpoint -> Dec
    dummyInst ep = InstanceD Nothing [binaryConstraint] (AppT (ConT securableN) (AppT (ConT $ mkName $ name ep) aT)) [dec]

    binaryConstraint :: Type
    binaryConstraint = AppT (ConT $ mkName "Binary") aT

    secureConstraint :: Type
    secureConstraint = AppT (ConT securableN) bT

    dec :: Dec
    dec = FunD (mkName "mkSecure") [Clause [WildP] (NormalB $ LamE [WildP] (AppE (VarE $ mkName "encode") (LitE $ CharL '\0'))) []]

    appDec :: Dec
    appDec = InstanceD Nothing [binaryConstraint, secureConstraint] typ [dec]
      where
        typ :: Type
        typ = AppT (ConT securableN) (AppT (AppT ArrowT aT) bT)

concreteSecurable :: Endpoint -> Dec
concreteSecurable ep = ClassD [] (mkName "Securable") [NormalTV a] [] [sig]
  where
    sig :: Dec
    sig = SigD (mkName "mkSecure") (AppT (AppT ArrowT aT) domainfun)
      where
        bsT :: Type
        bsT = ConT $ mkName "B.ByteString"

        domainfun :: Type
        domainfun = AppT (AppT ArrowT (AppT ListT bsT)) (AppT (ConT $ mkName $ name ep) bsT)

-- | Takes the current endpoint and the other dummy enclave endpoints, and produces the corresponding
-- mkSecure instances
concreteMkSecureInstances :: Endpoint -> [Endpoint] -> [Dec]
concreteMkSecureInstances ep eps = correctInstance : appDec : map dummyDec eps
  where
    securableN :: Name
    securableN = mkName "Securable"

    binaryConstraint :: Type
    binaryConstraint = AppT (ConT $ mkName "Binary") aT

    secureConstraint :: Type
    secureConstraint = AppT (ConT securableN) bT

    correctInstance :: Dec
    correctInstance = InstanceD Nothing [binaryConstraint] (AppT (ConT securableN) (AppT (ConT $ mkName $ name ep) aT)) [dec]
      where
        dec :: Dec
        dec = FunD (mkName "mkSecure") [Clause [mP] (NormalB $ LamE [WildP] (AppE3 (VarE $ mkName "fmap") (VarE $ mkName "encode") mE)) []]

    appDec :: Dec
    appDec = InstanceD Nothing [binaryConstraint, secureConstraint] typ [dec]
      where
        typ :: Type
        typ = AppT (ConT securableN) (AppT (AppT ArrowT aT) bT)

        dec :: Dec
        dec = FunD (mkName "mkSecure") [Clause [fP] (NormalB $ LamE [lamargs] lambdy) []]
          where
            lamargs :: Pat
            lamargs = UInfixP xP (mkName ":") xsP

            lambdy :: Exp
            lambdy = AppE3 (VarE $ mkName "mkSecure") (UInfixE fE (VarE $ mkName "$") (AppE (VarE $ mkName "decode") xE)) xsE

    dummyDec :: Endpoint -> Dec
    dummyDec ep = InstanceD Nothing [binaryConstraint] typ [dec]
      where
        typ :: Type
        typ = AppT (ConT securableN) (AppT (ConT $ mkName $ name ep) aT)

        dec :: Dec
        dec = FunD (mkName "mkSecure") [Clause [WildP] (NormalB $ LamE [WildP] (AppE (VarE $ mkName "error") (LitE $ StringL "this should not be invoked"))) []]

-- * Reference management

concreteServerRef :: Endpoint -> Dec
concreteServerRef ep = Instance (mkName "ServerRef") (mkName $ name ep) decs
  where
    decs :: [Dec]
    decs = [newRef, setRef, getRef]

    newRef :: Dec
    newRef = FunD (mkName "newRef") [Clause [xP] (NormalB $ DoE Nothing [stm1, stm2]) []]
      where
        stm1 :: Stmt
        stm1 = BindS aP (UInfixE (VarE $ mkName "liftIO") (VarE $ mkName "$") (AppE (VarE $ mkName "newIORef") xE))

        stm2 :: Stmt
        stm2 = NoBindS $ UInfixE (VarE $ mkName "return") (VarE $ mkName "$") (UInfixE (VarE $ mkName "return") (VarE $ mkName "$") (AppE (ConE $ mkName "Ref") aE))

    setRef :: Dec
    setRef = FunD (mkName "setRef") [Clause pats body []]
      where
        pats :: [Pat]
        pats = [ConP (mkName "Ref") [] [xP], aP]

        body :: Body
        body = NormalB $ UInfixE (ConE $ mkName $ name ep) (VarE $ mkName "$") (AppE3 (VarE $ mkName "writeIORef") xE aE)

    getRef :: Dec
    getRef = FunD (mkName "getRef") [Clause pats body []]
      where
        pats :: [Pat]
        pats = [ConP (mkName "Ref") [] [xP]]

        body :: Body
        body = NormalB $ UInfixE (ConE $ mkName $ name ep) (VarE $ mkName "$") (AppE (VarE $ mkName "readIORef") xE)

dummyServerRef :: Endpoint -> Dec
dummyServerRef ep = Instance (mkName "ServerRef") (mkName $ name ep) decs
  where
    decs :: [Dec]
    decs = [newRef, setRef, getRef]

    newRef :: Dec
    newRef = FunD (mkName "newRef") [Clause [WildP] (NormalB $ AppE (VarE $ mkName "return") (ConE $ mkName $ name ep)) []]

    setRef :: Dec
    setRef = FunD (mkName "setRef") [Clause [WildP, WildP] (NormalB $ ConE $ mkName $ name ep) []]

    getRef :: Dec
    getRef = FunD (mkName "getRef") [Clause [WildP] (NormalB $ ConE $ mkName $ name ep) []]

-- * RunApp

runAppN :: Name
runAppN = mkName "runApp"

sig :: Dec
sig = SigD runAppN (AppT (AppT ArrowT (AppT (ConT $ mkName "App") aT)) ioUnit)

clientRunApp :: [Dec]
clientRunApp = [sig, dec]
  where
    dec :: Dec
    dec = FunD runAppN [Clause [xP] (NormalB $ DoE Nothing [dbgStm, stm2]) []]
      where
        dbgStm = NoBindS $ AppE (VarE $ mkName "putStrLn") (LitE . StringL $ "Running: Client")
        stm2 = NoBindS $ AppE (VarE $ mkName "runAppClient") xE

enclaveRunApp :: Endpoint -> [Dec]
enclaveRunApp ep = [sig, dec]
  where
    dec :: Dec
    dec = FunD runAppN [Clause [xP] (NormalB $ DoE Nothing [dbgStm, stm1, stm2]) []]
      where
        dbgStm :: Stmt
        dbgStm = NoBindS $
          AppE (VarE $ mkName "putStrLn")
          (LitE . StringL $ "Running: " <> name ep)

        stm1 :: Stmt
        stm1 = BindS aP (AppE (VarE $ mkName "runAppServer") xE)

        stm2 :: Stmt
        stm2 = NoBindS $ AppE3 (VarE $ mkName "serveForever") (TupE [Just ip, Just port]) aE
          where
            ip :: Exp
            ip = LitE $ StringL $ fst $ GenModule.location ep

            port :: Exp
            port = LitE $ StringL $ show $ snd $ GenModule.location ep
