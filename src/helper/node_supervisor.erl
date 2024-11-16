-module(node_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(MasterPid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MasterPid]).

init([MasterPid]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % All children are dynamically added
        intensity => 5,                  % Max 5 restarts
        period => 30                     % Within 30 seconds
    },

    NodeChild = #{
        id => node,
        start => {node, start_link, [MasterPid]},
        restart => transient,            % Restart only if terminated abnormally
        shutdown => 5000,
        type => worker,
        modules => [node]
    },

    {ok, {SupFlags, [NodeChild]}}.