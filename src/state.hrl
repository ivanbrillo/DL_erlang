-record(state, {
    pythonModelPID :: pid(),
    pythonUiPID :: pid(),
    initializedNodes = [] :: [pid()],
    dbLoadedNodes = [] :: [pid()],
    distributedNodes = [] :: [pid()],
    weightsNodes = [] :: [pid()],
    trainNodes = [] :: [pid()]
}).