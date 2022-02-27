-module(main).

-include("cr.hrl").

-export([init/2]).
-export([search_links/4]).

init(Req0, _) ->
    {ok, ReqData, Req} = cowboy_req:read_body(Req0),
    log:debug("[main] ReqData: ~p", [ReqData]),
    Path = cowboy_req:path(Req),
    log:info("[main] Path: ~p", [Path]),
    Qs = cowboy_req:qs(Req),
    log:info("[main] Qs: ~p", [Qs]),
    Method = to_method(cowboy_req:method(Req)),
    log:debug("[main] Method: ~p", [Method]),
    Headers0 = cowboy_req:headers(Req),
    Cookie = data:get(<<"cookie">>, Headers0),
    ContentType = data:get(<<"content-type">>, Headers0),
    UserAgent = data:get(<<"user-agent">>, Headers0),
    log:debug("Req: ~p", [Req]),
    Headers = http_service:to_headers(#{<<"cookie">> => Cookie,
                                        <<"content-type">> => ContentType,
                                        <<"user-agent">> => UserAgent
                                       }),
    log:debug("[main] Headers: ~p", [Headers]),
    {ok, TargetSchema} = application:get_env(?APP_NAME, target_schema),
    {ok, TargetBin} = application:get_env(?APP_NAME, target),
    case  http_service:Method(<<TargetSchema/binary, TargetBin/binary, Path/binary, "?", Qs/binary>>, ReqData, Headers) of 
    	{ok, MainPage, RespHeaders} ->
	    {ok, MyHost} = application:get_env(?APP_NAME, my_host),
	    {ok, NoProxy} = application:get_env(?APP_NAME, no_proxy),
	    NewPage = search_links(MainPage, TargetBin, MyHost, NoProxy),
    	    Resp = cowboy_req:reply(200, data:to_map(RespHeaders), NewPage, Req),
      	    {ok, Resp, []};
	Other ->
	    log:info("Other: ~p", [Other]),
	    {ok, <<>>, []}
    end.



%%%%%%%%%%% LOCAL

search_links(<<31,139,8,0,0, _/binary>> = Page, Target, MyHost, NoProxy) ->
%	log:debug("[main] Page: ~ts", [zlib:gunzip(Page)]),
    search_links(zlib:gunzip(Page), Target, MyHost, NoProxy, <<>>);
search_links(Page, Target, MyHost, NoProxy) ->
    search_links(Page, Target, MyHost, NoProxy, <<>>).


search_links(<<"//", Rest/binary>>, Target, MyHost, NoProxy, Acc) ->
    case check_match(Rest, Target) of
        {true, NewRest, _} ->
            case check_no_proxy(NewRest, NoProxy) of
                {true, NpItem, NewRest1} ->
                    search_links(NewRest1, Target, MyHost, NoProxy, <<Acc/binary, "//", Target/binary, NpItem/binary>>);
                _ ->
                    search_links(NewRest, Target, MyHost, NoProxy, <<Acc/binary, "//", MyHost/binary>>)
            end;
        {false, NewRest, Matched} ->
            search_links(NewRest, Target, MyHost, NoProxy, <<Acc/binary, "//", Matched/binary>>)
    end;

search_links(<<".", Rest/binary>>, Target, MyHost, NoProxy, Acc) ->
    case check_match(Rest, Target) of
        {true, NewRest, _} ->
            case check_no_proxy(NewRest, NoProxy) of
                {true, NpItem, NewRest1} ->
                    search_links(NewRest1, Target, MyHost, NoProxy, <<Acc/binary, ".", Target/binary, NpItem/binary>>);
                _ ->
                    search_links(NewRest, Target, MyHost, NoProxy, <<Acc/binary, ".", MyHost/binary>>)
            end;
        {false, NewRest, Matched} ->
            search_links(NewRest, Target, MyHost, NoProxy, <<Acc/binary, ".", Matched/binary>>)
    end;

search_links(<<X, Rest/binary>>, Target, MyHost, NoProxy, Acc) ->
    search_links(Rest, Target, MyHost, NoProxy, <<Acc/binary, X>>);
search_links(<<>>, _, _, _, Acc) ->
    Acc.

check_no_proxy(Rest, [NpItem|T]) ->
    case check_match(Rest, NpItem) of
        {true, NewRest, _} ->
            {true, NpItem, NewRest};
        {_, _, _} ->
            check_no_proxy(Rest, T)
    end;
check_no_proxy(Rest, []) ->
    {false, Rest}.

check_match(Rest, Item) ->
    check_match(Rest, Item, <<>>).

check_match(Rest, <<>>, _) ->
    {true, Rest, <<>>};
check_match(<<X, Rest/binary>>, <<X, ItemRest/binary>>, Acc) ->
    check_match(Rest, ItemRest, <<Acc/binary, X>>);
check_match(Rest, _, Acc) ->
    {false, Rest, Acc}.

to_method(<<"POST">>) ->
	post;
to_method(_) ->
	get.
