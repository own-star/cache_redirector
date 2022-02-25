-module(main).

-include("cr.hrl").

-export([init/2]).
%-export([get_url/3, post_url/4]).
%-export([get_links/1]).

init(Req0, _) ->
    {ok, ReqData, Req} = cowboy_req:read_body(Req0),
    log:info("[main] ReqData: ~p", [ReqData]),
%    GET = cowboy_req:parse_qs(Req),
%    log:info("[main] GET: ~p", [GET]),
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
	    NewPage = search_links(MainPage, Protocol, Target, TargetBin, MyHost),
    	    Resp = cowboy_req:reply(200, data:to_map(RespHeaders), NewPage, Req),
      	    {ok, Resp, []};
	Other ->
	    log:info("Other: ~p", [Other]),
	    {ok, <<>>, []}
    end.

%get_url(<<"//", Rest/binary>>, Protocol, Headers) ->
%    get_url(<<"https://", Rest/binary>>, Protocol, Headers);
%get_url(Url, Protocol, Headers) ->
%    log:info("[main] Url: ~p", [Url]),
%    case http_service:get(Url, Headers) of
%        {ok, Page, RespHeaders} ->
%	    {ok, Target} = application:get_env(?APP_NAME, target),
%	    {ok, TargetBin} = application:get_env(?APP_NAME, target_bin),
%	    {ok, MyHost} = application:get_env(?APP_NAME, my_host),
%	    {search_links(Page, Protocol, Target, TargetBin, MyHost), RespHeaders};
%        Other ->
%            log:info("[main] HttpResp: ~p", [Other]),
%            <<>>
%    end.
%
%post_url(<<"//", Rest/binary>>, Protocol, Data, Headers) ->
%    post_url(<<"https://", Rest/binary>>, Protocol, Data, Headers);
%post_url(Url, Protocol, Data, Headers) ->
%    log:info("[main] Url: ~p", [Url]),
%    case http_service:post(Url, Data, Headers) of
%        {ok, Page, RespHeaders} ->
%	    {ok, Target} = application:get_env(?APP_NAME, target),
%	    {ok, TargetBin} = application:get_env(?APP_NAME, target_bin),
%	    {ok, MyHost} = application:get_env(?APP_NAME, my_host),
%            {search_links(Page, Protocol, Target, TargetBin, MyHost), RespHeaders};
%        Other ->
%            log:info("[main] HttpResp: ~p", [Other]),
%            <<>>
%    end.



%%%%%%%%%%% LOCAL

search_links(<<31,139,8,0,0, _/binary>> = Page, Protocol, Target, TargetBin, MyHost) ->
	log:info("[main] Page: ~ts", [zlib:gunzip(Page)]),
    search_links(zlib:gunzip(Page), Protocol, Target, TargetBin, MyHost, <<>>);
search_links(Page, Protocol, Target, TargetBin, MyHost) ->
    search_links(Page, Protocol, Target, TargetBin, MyHost, <<>>).


%search_links(<<"https://", ?TARGET_ROOT_LIST, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
%    log:info("[SearchLink] Root Url: ~p -> ~p ", [TargetBin, MyHost]),
%    search_links(Rest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, Protocol/binary, "://", ?LOCAL_ROOT/binary>>);
%
%search_links(<<"http://", ?TARGET_ROOT_LIST, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
%    log:info("[SearchLink] Root Url: ~p -> ~p ", [TargetBin, MyHost]),
%    search_links(Rest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, Protocol/binary, "://", ?LOCAL_ROOT/binary>>);

search_links(<<Target, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
    log:info("[SearchLink] Url: ~p -> ~p ", [TargetBin, MyHost]),
    search_links(Rest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, MyHost/binary>>);

%search_links(<<"'https://", Target, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
%    {Link, NewRest} = get_link(Rest),
%    FullLink = <<"https://", TargetBin/binary, Link/binary>>,
%    Key = get_key(FullLink),
%    log:info("[SearchLink] Key: ~p, Uno Url: ~p", [Key, FullLink]),
%    search_links(NewRest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, "'", Protocol/binary, "://", MyHost/binary, "/link/", Key/binary, "'">>);
%
%search_links(<<"srcset=\"https://", Target, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
%    {Link, NewRest} = get_link(Rest),
%    FullLink = <<"https://", TargetBin/binary, Link/binary>>,
%    Set = get_set(FullLink, Protocol, MyHost),
%    log:info("[SearchLink] Set: ~p, Url: ~p", [Set, FullLink]),
%    search_links(NewRest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, "srcset=\"", Set/binary, "\"">>);
%
%search_links(<<"\"https:\\/\\/", Target, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
%    {Link, NewRest} = get_link(Rest),
%    FullLink = <<"https://", TargetBin/binary, Link/binary>>,
%    Key = get_key(FullLink),
%    log:info("[SearchLink] Key: ~p, Slashes Url: ~p", [Key, FullLink]),
%    search_links(NewRest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, "\"", Protocol/binary, ":\\/\\/", MyHost/binary, "\\/link\\/", Key/binary, "\"">>);
%
%search_links(<<"\"http://", Target, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
%    {Link, NewRest} = get_link(Rest),
%    FullLink = <<"http://", TargetBin/binary, Link/binary>>,
%    Key = get_key(FullLink),
%    log:info("[SearchLink] Key: ~p, Url: ~p", [Key, FullLink]),
%    search_links(NewRest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, "\"", Protocol/binary, "://", MyHost/binary, "/link/", Key/binary, "\"">>);
search_links(<<X, Rest/binary>>, Protocol, Target, TargetBin, MyHost, Acc) ->
    search_links(Rest, Protocol, Target, TargetBin, MyHost, <<Acc/binary, X>>);
search_links(<<>>, _, _, _, _, Acc) ->
    Acc.

%get_link(Bin) ->
%    get_link(Bin, <<>>).
%
%get_link(<<"\"", Rest/binary>>, Acc) ->
%    {Acc, Rest};
%get_link(<<"\'", Rest/binary>>, Acc) ->
%    {Acc, Rest};
%get_link(<<")", Rest/binary>>, Acc) ->
%    {Acc, Rest};
%get_link(<<"\\", Rest/binary>>, Acc) ->
%    get_link(Rest, Acc);
%get_link(<<X, Rest/binary>>, Acc) ->
%    get_link(Rest, <<Acc/binary, X>>).
%
%get_key(Link) ->
%    case ets:lookup(ets_link_to_url, Link) of
%        [{Link, Key}] ->
%            Key;
%        _ ->
%            Key = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
%            ets:insert(ets_link_to_url, {Link, Key}),
%            ets:insert(ets_link_to_url, {Key, Link}),
%            Key
%    end.
%
%get_set(Set, Protocol, MyHost) ->
%    lists:foldl(fun(Link0, Acc) ->
%                        log:info("[SearchLink] {get_set} Link0: ~p", [Link0]),
%                        case binary:split(strip_head(Link0), <<32>>) of
%                            [Link, W] ->
%                                Key = get_key(Link),
%                                Url = <<Protocol/binary, "://", MyHost/binary, "/link/", Key/binary, " ", W/binary>>,
%                                log:info("[SearchLink] {get_set} Url: ~p, W: ~p", [Url, W]),
%                                case Acc of
%                                    <<>> -> Url;
%                                    _ -> <<Acc/binary, ", ", Url/binary>>
%                                end;
%                            [Link] ->
%                                Key = get_key(Link),
%                                Url = <<Protocol/binary, "://", MyHost/binary, "/link/", Key/binary>>,
%                                log:info("[SearchLink] {get_set} Url: ~p", [Url]),
%                                case Acc of
%                                    <<>> -> Url;
%                                    _ -> <<Acc/binary, ", ", Url/binary>>
%                                end
%                        end
%                end, <<>>, binary:split(Set, <<",">>, [global])).
%
%strip_head(<<32, Rest/binary>>) ->
%    strip_head(Rest);
%strip_head(Bin) ->
%    Bin.

to_method(<<"POST">>) ->
	post;
to_method(_) ->
	get.
