-module(http_server_sup).

-behaviour(supervisor).

-export([start_link/0]). % API.
-export([init/1]). % supervisor.

-define(SUPERVISOR, ?MODULE).


%% API.

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

%-spec init([])-> {ok, {{one_for_one, 10, 10}, [{_, _, _, _, _, _}, ...]}}.
init([])->
    Procs = [{http_server, {http_server, start_link, []},
              permanent, 5000, worker, [http_server]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.
