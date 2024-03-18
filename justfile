run-client:
    cat config-base.json | jq '.focused = "Client1"' > config.json
    cabal clean
    cabal run exploittest

run-overlord:
    cat config-base.json | jq '.focused = "Overlord"' > config.json
    cabal clean
    cabal run exploittest

run-worker1:
    cat config-base.json | jq '.focused = "Worker1"' > config.json
    cabal clean
    cabal run exploittest

run-worker2:
    cat config-base.json | jq '.focused = "Worker2"' > config.json
    cabal clean
    cabal run exploittest
