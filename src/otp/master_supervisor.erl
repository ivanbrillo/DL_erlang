-module(master_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1, start_link_shell/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link_shell() ->
    {ok, Pid} = start_link(),
    unlink(Pid).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Restart only the failed child
        intensity => 10,          % Max 10 restarts
        period => 60              % Within 60 seconds
    },

    MasterChild = #{
        id => master,
        start => {master_api, start_link, []},
        restart => permanent,     % Always restart
        shutdown => 10000,        % Time to wait for graceful shutdown
        type => worker,
        modules => [master_api]
    },

    {ok, {SupFlags, [MasterChild]}}.
