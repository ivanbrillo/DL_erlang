-module(master_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1, start_link_shell/1, terminate/0]).

start_link(JavaPid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [JavaPid]).

start_link_shell(JavaPid) ->
    {ok, Pid} = start_link(JavaPid),
    unlink(Pid),
    {ok, started}.

terminate() ->
    io:format("--- MASTER SUPERVISOR: Terminating Procedure ---~n"),
    %gen_server:stop(erlang_master, normal).  % exit with normal exit code, so its not restarted since the policy is transient
    gen_server:call(erlang_master, stop).



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
