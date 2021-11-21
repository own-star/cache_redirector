-module(http_server).
-behaviour(gen_server).
% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
% API
-export([start_link/0]).
-export([get_profile/0]).


-define(HTTPC_MAX_PROFILE, 200).






get_profile()->
    gen_server:call(?MODULE, get_profile, 30000).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).




init([])->

    Queue =
    lists:foldl(fun(Item, Acc)->
        Profile = to_profile(Item),
        {ok, _Pid} = inets:start(httpc, [{profile, Profile}]),
        queue:in(Profile, Acc)
    end, queue:new(), lists:seq(1, ?HTTPC_MAX_PROFILE)),

    log:info("HTTPC start ~p", [ok]),

    {ok, Queue}.




handle_call(get_profile, _From, Queue) ->

    {{value, Profile}, Queue2} = queue:out(Queue),

    {reply, Profile, queue:in(Profile, Queue2)};




% handle_call generic fallback
handle_call(_Request, _From, State) ->
    {reply, undefined, State}.



% manual shutdown
handle_cast(stop, State) ->
        {stop, normal, State};



% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
    {noreply, State}.



% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    terminated.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%%%%%%%%%%%%%%%% LOCAL


to_profile(Item)->
    Value = list_to_atom("httpc_profile_" ++ integer_to_list(Item)),
    % io:format("RANDOM VAAAAAAAAAAAAAAALUE ~p~n", [Value]),
    Value.
