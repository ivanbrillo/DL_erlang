-record(mstate, {
    pythonModelPID :: pid(),
    javaUiPid :: pid(),
    currentUpNodes = [] :: [{pid(), node()}],
    previousInitializedNodes = [] :: [{pid(), node()}],
    terminateTraining = false
}).


-record(nstate, {
    masterPid :: pid(),
    masterNode :: node(),
    pythonPid :: pid(),
    termination_timer
}).
