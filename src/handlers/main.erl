-module(main).

-include("cr.hrl").

-export([init/2]).
-export([get_url/1, post_url/3]).
%-export([get_links/1]).

init(Req, _) ->
    {ok, MainPage, _} = http_service:getURL(<<"https://", ?TARGET/binary, "/">>),
    NewPage = search_links(MainPage),
    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, NewPage, Req),
    {ok, Resp, []}.

get_url(<<"//", Rest/binary>>) ->
    get_url(<<"https://", Rest/binary>>);
get_url(Url) ->
    log:info("[main] Url: ~p", [Url]),
    case http_service:getURL(Url) of
        {ok, Page, _} ->
            search_links(Page);
        Other ->
            log:info("[main] HttpResp: ~p", [Other]),
            <<>>
    end.

post_url(<<"//", Rest/binary>>, Data, Headers) ->
    post_url(<<"https://", Rest/binary>>, Data, Headers);
post_url(Url, Data, Headers) ->
    log:info("[main] Url: ~p", [Url]),
    case http_service:post(Url, Data, Headers) of
        {ok, Page, _} ->
            search_links(Page);
        Other ->
            log:info("[main] HttpResp: ~p", [Other]),
            <<>>
    end.



%%%%%%%%%%% LOCAL


search_links(Page) ->
    search_links(Page, <<>>).

search_links(<<"\"https://my.", ?TARGET_LIST, Rest/binary>>, Acc) ->
    {Link, NewRest} = get_link(Rest),
    FullLink = <<"https://my.", ?TARGET/binary, Link/binary>>,
    Key = get_key(FullLink),
    log:info("[SearchLink] Key: ~p, My Url: ~p", [Key, FullLink]),
    search_links(NewRest, <<Acc/binary, "\"http://", ?MY_HOST/binary, "/link/", Key/binary, "\"">>);

search_links(<<"'https://", ?TARGET_LIST, "/wp-content/plugins/colibri-page-builder/extend-builder/assets/static/css/theme.css", Rest/binary>>, Acc) ->
    {Qs, NewRest} = get_link(Rest),
    log:info("[SearchLink] ScriptUrl: ~p", [<<"'https://", ?MY_HOST/binary, "/wp-content/plugins/colibri-page-builder/extend-builder/assets/static/css/theme.css", Qs/binary, "'">>]),
    search_links(NewRest, <<Acc/binary, "'http://", ?MY_HOST/binary, "/wp-content/plugins/colibri-page-builder/extend-builder/assets/static/css/theme.css", Qs/binary, "'">>);

search_links(<<"'https://", ?TARGET_LIST, Rest/binary>>, Acc) ->
    {Link, NewRest} = get_link(Rest),
    FullLink = <<"https://", ?TARGET/binary, Link/binary>>,
    Key = get_key(FullLink),
    log:info("[SearchLink] Key: ~p, Uno Url: ~p", [Key, FullLink]),
    search_links(NewRest, <<Acc/binary, "'http://", ?MY_HOST/binary, "/link/", Key/binary, "'">>);

search_links(<<"srcset=\"https://", ?TARGET_LIST, Rest/binary>>, Acc) ->
    {Link, NewRest} = get_link(Rest),
    FullLink = <<"https://", ?TARGET/binary, Link/binary>>,
    Set = get_set(FullLink),
    log:info("[SearchLink] Set: ~p, Url: ~p", [Set, FullLink]),
    search_links(NewRest, <<Acc/binary, "srcset=\"", Set/binary, "\"">>);

search_links(<<"\"https:\\/\\/", ?TARGET_LIST, Rest/binary>>, Acc) ->
    {Link, NewRest} = get_link(Rest),
    FullLink = <<"https://", ?TARGET/binary, Link/binary>>,
    Key = get_key(FullLink),
    log:info("[SearchLink] Key: ~p, Slashes Url: ~p", [Key, FullLink]),
    search_links(NewRest, <<Acc/binary, "\"http:\\/\\/", ?MY_HOST/binary, "\\/link\\/", Key/binary, "\"">>);

search_links(<<"\"https://", ?TARGET_LIST, Rest/binary>>, Acc) ->
    {Link, NewRest} = get_link(Rest),
    FullLink = <<"https://", ?TARGET/binary, Link/binary>>,
    Key = get_key(FullLink),
%    log:info("[SearchLink] Key: ~p, Double Url: ~p", [Key, FullLink]),
    search_links(NewRest, <<Acc/binary, "\"http://", ?MY_HOST/binary, "/link/", Key/binary, "\"">>);

search_links(<<"(https://", ?TARGET_LIST, Rest/binary>>, Acc) ->
    {Link, NewRest} = get_link(Rest),
    FullLink = <<"https://", ?TARGET/binary, Link/binary>>,
    Key = get_key(FullLink),
    log:info("[SearchLink] Key: ~p, Breckets Url: ~p", [Key, FullLink]),
    search_links(NewRest, <<Acc/binary, "(http://", ?MY_HOST/binary, "/link/", Key/binary, ")">>);

search_links(<<"\"http://", ?TARGET_LIST, Rest/binary>>, Acc) ->
    {Link, NewRest} = get_link(Rest),
    FullLink = <<"http://", ?TARGET/binary, Link/binary>>,
    Key = get_key(FullLink),
    log:info("[SearchLink] Key: ~p, Url: ~p", [Key, FullLink]),
    search_links(NewRest, <<Acc/binary, "\"http://", ?MY_HOST/binary, "/link/", Key/binary, "\"">>);
search_links(<<X, Rest/binary>>, Acc) ->
    search_links(Rest, <<Acc/binary, X>>);
search_links(<<>>, Acc) ->
    Acc.

get_link(Bin) ->
    get_link(Bin, <<>>).

get_link(<<"\"", Rest/binary>>, Acc) ->
    {Acc, Rest};
get_link(<<"\'", Rest/binary>>, Acc) ->
    {Acc, Rest};
get_link(<<")", Rest/binary>>, Acc) ->
    {Acc, Rest};
get_link(<<"\\", Rest/binary>>, Acc) ->
    get_link(Rest, Acc);
get_link(<<X, Rest/binary>>, Acc) ->
    get_link(Rest, <<Acc/binary, X>>).

get_key(Link) ->
    case ets:lookup(ets_link_to_url, Link) of
        [{Link, Key}] ->
            Key;
        _ ->
            Key = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
            ets:insert(ets_link_to_url, {Link, Key}),
            ets:insert(ets_link_to_url, {Key, Link}),
            Key
    end.

get_set(Set) ->
    lists:foldl(fun(Link0, Acc) ->
                        log:info("[SearchLink] {get_set} Link0: ~p", [Link0]),
                        case binary:split(strip_head(Link0), <<32>>) of
                            [Link, W] ->
                                Key = get_key(Link),
                                Url = <<"http://", ?MY_HOST/binary, "/link/", Key/binary, " ", W/binary>>,
                                log:info("[SearchLink] {get_set} Url: ~p, W: ~p", [Url, W]),
                                case Acc of
                                    <<>> -> Url;
                                    _ -> <<Acc/binary, ", ", Url/binary>>
                                end;
                            [Link] ->
                                Key = get_key(Link),
                                Url = <<"http://", ?MY_HOST/binary, "/link/", Key/binary>>,
                                log:info("[SearchLink] {get_set} Url: ~p", [Url]),
                                case Acc of
                                    <<>> -> Url;
                                    _ -> <<Acc/binary, ", ", Url/binary>>
                                end
                        end
                end, <<>>, binary:split(Set, <<",">>, [global])).

strip_head(<<32, Rest/binary>>) ->
    strip_head(Rest);
strip_head(Bin) ->
    Bin.
