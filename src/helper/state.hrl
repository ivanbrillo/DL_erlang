-record(mstate, {
    pythonModelPID :: pid(),
    pythonUiPID :: pid(),
    currentUpNodes = [] :: [{pid(), node()}],
    previousInitializedNodes = [] :: [{pid(), node()}]
}).


-record(nstate, {
    masterPid :: pid(),
    masterNode :: node(),
    pythonPid :: pid(),
    termination_timer
}).
