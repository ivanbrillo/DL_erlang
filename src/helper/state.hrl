-record(state, {
    pythonModelPID :: pid(),
    pythonUiPID :: pid(),
    initialUpNodes = [] :: [{pid(), node()}],
    currentUpNodes = [] :: [{pid(), node()}]
}).
