module Plugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = return $ todos ++ [CoreDoPluginPass "HasTEE isolation verification pass" pass]

pass :: ModGuts -> CoreM ModGuts
pass guts = do dflags <- getDynFlags
               bindsOnlyPass (mapM (printBind dflags)) guts
  where
    printBind :: DynFlags -> CoreBind -> CoreM CoreBind
    printBind flags bnd = do
        putMsgS $ "FROM ROBERTS PLUGIN: " ++ showSDoc flags (ppr bnd)
        return bnd