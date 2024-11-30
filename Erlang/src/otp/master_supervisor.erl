-module(master_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1, start_link_shell/1, terminate/1]).

start_link(JavaPid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [JavaPid]).

start_link_shell(JavaPid) ->
    {ok, Pid} = start_link(JavaPid),
    unlink(Pid),
    {ok, Pid}.

terminate(MasterSup) ->
    gen_server:stop(erlang_master),  % exit with normal exit code, so its not restarted since the policy is transient
    exit(MasterSup, shutdown).


init([JavaPid]) ->
    SupFlags = #{
        strategy => one_for_one,  % Restart only the failed child
        intensity => 10,          % Max 10 restarts
        period => 60              % Within 60 seconds
    },

    MasterChild = #{
        id => erlang_master,
        start => {master_api, start_link, [JavaPid]},
        restart => transient,     % Restart only if exception occurs (not when exit reasons is shutdown or normal)
        shutdown => 10000,        % Time to wait for graceful shutdown
        type => worker,
        modules => [master_api]
    },

    {ok, {SupFlags, [MasterChild]}}.
