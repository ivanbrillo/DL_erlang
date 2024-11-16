-module(master_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1, start_link_test/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link_test() ->
    {ok, Pid} = start_link(),
    unlink(Pid).


init([]) ->

    % process_flag(trap_exit, true),
    % Define the supervisor flags
    SupFlags = #{
        strategy => one_for_one,  % Restart only the failed child
        intensity => 10,          % Max 10 restarts
        period => 60             % Within 60 seconds
    },

    % Define the master process spec
    MasterChild = #{
        id => master,
        start => {master, start_link, []},
        restart => permanent,     % Always restart
        shutdown => 5000,        % Time to wait for graceful shutdown
        type => worker,
        modules => [master]
    },

    % Return supervisor specification
    {ok, {SupFlags, [MasterChild]}}.
