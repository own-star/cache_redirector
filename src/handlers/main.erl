-module(main).

-include("cr.hrl").

-export([init/2]).
-export([search_links/6]).

init(Req0, _) ->
    log:info("[main] AppName: ~p", [?APP_NAME]),
    {ok, ReqData, Req} = cowboy_req:read_body(Req0),
    log:info("[main] ReqData: ~p", [ReqData]),
    Path = cowboy_req:path(Req),
    log:info("[main] Path: ~p", [Path]),
    Method = to_method(cowboy_req:method(Req)),
    log:info("[main] Method: ~p", [Method]),
    Headers0 = cowboy_req:headers(Req),
    Protocol = data:get(<<"x-forwarded-proto">>, Headers0, <<"https">>),
    Cookie = data:get(<<"cookie">>, Headers0),
    ContentType = data:get(<<"content-type">>, Headers0),
    log:info("[main] Protocol: ~p", [Protocol]),
    log:info("Req: ~p", [Req]),
    Headers = http_service:to_headers(#{<<"cookie">> => Cookie, <<"content-type">> => ContentType}),
    log:info("[main] Headers: ~p", [Headers]),
    {ok, TargetSchema} = application:get_env(?APP_NAME, target_schema),
    {ok, TargetBin} = application:get_env(?APP_NAME, target),
    case  http_service:Method(<<TargetSchema/binary, TargetBin/binary, Path/binary>>, ReqData, Headers) of 
    	{ok, MainPage, RespHeaders} ->
%    	    log:info("Headers: ~p", [RespHeaders]),
	    Target = binary_to_list(TargetBin),
	    {ok, MyHost} = application:get_env(?APP_NAME, my_host),
	    {ok, NoProxy} = application:get_env(?APP_NAME, no_proxy),
	    NewPage = search_links(MainPage, Protocol, Target, TargetBin, MyHost, NoProxy),
    	    Resp = cowboy_req:reply(200, data:to_map(RespHeaders), NewPage, Req),
      	    {ok, Resp, []};
	Other ->
	    log:info("Other: ~p", [Other]),
	    {ok, <<>>, []}
    end.



%%%%%%%%%%% LOCAL

search_links(<<31,139,8,0,0, _/binary>> = Page, Protocol, Target, TargetBin, MyHost, NoProxy) ->
	log:info("[main] Page: ~ts", [zlib:gunzip(Page)]),
    search_links(zlib:gunzip(Page), Protocol, Target, TargetBin, MyHost, NoProxy, <<>>, <<>>);
search_links(Page, Protocol, Target, TargetBin, MyHost, NoProxy) ->
    search_links(Page, Protocol, Target, TargetBin, MyHost, NoProxy, <<>>, <<>>).


search_links(<<X, Rest/binary>>, Protocol, <<X, Target/binary>>, TargetBin, MyHost, NoProxy, TargetAcc, Acc) ->
    log:info("[main] match X: ~p, Acc: ~p", [X, TargetAcc]),
    search_links(Rest, Protocol, Target, TargetBin, MyHost, NoProxy, <<TargetAcc/binary, X>>, Acc);
search_links(Rest, Protocol, <<>>, TargetBin, MyHost, NoProxy, _TargetAcc, Acc) ->
    log:info("[SearchLink] Url: ~p -> ~p ", [TargetBin, MyHost]),
    case check_no_proxy(Rest, NoProxy) of
        {true, NpItem, NewRest} ->
            search_links(NewRest, Protocol, TargetBin, TargetBin, MyHost, NoProxy, <<>>, <<Acc/binary, TargetBin/binary, NpItem/binary>>);
        _ ->
            search_links(Rest, Protocol, TargetBin, TargetBin, MyHost, NoProxy, <<>>, <<Acc/binary, MyHost/binary>>)
    end;

search_links(<<X, Rest/binary>>, Protocol, Target, TargetBin, MyHost, NoProxy, <<>>, Acc) ->
    search_links(Rest, Protocol, Target, TargetBin, MyHost, NoProxy, <<>>, <<Acc/binary, X>>);
search_links(<<X, Rest/binary>>, Protocol, Target, TargetBin, MyHost, NoProxy, TargetAcc, Acc) ->
    search_links(Rest, Protocol, Target, TargetBin, MyHost, NoProxy, <<>>, <<Acc/binary, TargetAcc/binary, X>>);
search_links(<<>>, _, _, _, _, _, _, Acc) ->
    Acc.

check_no_proxy(Rest, [NpItem|T]) ->
    log:info("[main] check_no_proxy Testing: ~p", [NpItem]),
    case check_match(Rest, NpItem) of
        {true, NewRest} ->
            log:info("[main] check_no_proxy Match: ~p", [NpItem]),
            {true, NpItem, NewRest};
        {_, _} ->
            check_no_proxy(Rest, T)
    end;
check_no_proxy(Rest, []) ->
    {false, Rest}.

check_match(Rest, <<>>) ->
    {true, Rest};
check_match(<<X, Rest/binary>>, <<X, ItemRest/binary>>) ->
    check_match(Rest, ItemRest);
check_match(_, _) ->
    {false, <<>>}.

to_method(<<"POST">>) ->
	post;
to_method(_) ->
	get.
