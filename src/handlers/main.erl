-module(main).

-include("cr.hrl").

-export([init/2]).
-export([get_url/1, post_url/3]).
-export([get_links/1]).

init(Req, _) ->
    {ok, MainPage, _} = http_service:getURL(<<"https://", ?TARGET/binary, "/">>),
    Tree0 = mochiweb_html:parse(MainPage),
    Tree = get_links(Tree0),
%    log:info("[main] MainPage: ~p", [Tree]),
    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, list_to_binary(mochiweb_html:to_html(Tree)), Req),
    {ok, Resp, []}.

get_url(<<"//", Rest/binary>>) ->
    get_url(<<"https://", Rest/binary>>);
get_url(Url) ->
    log:info("[main] Url: ~p", [Url]),
    case http_service:getURL(Url) of
        {ok, Page, _} ->
            case strip_head(Page) of
                "" ->
                    log:info("[main] EmptyResp"),
                    <<>>;
                <<>> ->
                    log:info("[main] EmptyResp"),
                    <<>>;
                <<"<", _/binary>> ->
                    %log:info("[main] Page: ~p", [Page]),
                    try
                        Tree0 = mochiweb_html:parse(Page),
                        Tree = get_links(Tree0),
                        list_to_binary(mochiweb_html:to_html(Tree))
                    catch _ ->
                      <<>>
                    end;
                _ ->
                    log:info("[main] InvalidPage: ~p", [Url]),
                    Page
            end;
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
            case strip_head(Page) of
                "" ->
                    log:info("[main] EmptyResp"),
                    <<>>;
                <<>> ->
                    log:info("[main] EmptyResp"),
                    <<>>;
                <<"<", _/binary>> ->
                    %log:info("[main] Page: ~p", [Page]),
                    try
                        Tree0 = mochiweb_html:parse(Page),
                        Tree = get_links(Tree0),
                        list_to_binary(mochiweb_html:to_html(Tree))
                    catch _ ->
                      <<>>
                    end;
                _ ->
                    log:info("[main] InvalidPage: ~p", [Url]),
                    Page
            end;
        Other ->
            log:info("[main] HttpResp: ~p", [Other]),
            <<>>
    end.



%%%%%%%%%%% LOCAL

%get_links(Tree) ->
%    get_links(Tree, []).

get_links({<<"link">>, Tags, Body}) ->
    log:info("Link: ~p", [proplists:get_value(<<"href">>, Tags, <<>>)]),
    {<<"link">>, handle_links(Tags), Body};
get_links({<<"li">>, Tags, Body}) ->
    log:info("Li: ~p", [proplists:get_value(<<"href">>, Tags, <<>>)]),
    {<<"li">>, handle_links(Tags), Body};
get_links({<<"a">>, Tags, Body}) ->
    log:info("A: ~p", [proplists:get_value(<<"href">>, Tags, <<>>)]),
    {<<"a">>, handle_links(Tags), Body};
get_links({<<"form">>, Tags, Body}) ->
    log:info("Form: ~p", [proplists:get_value(<<"action">>, Tags, <<>>)]),
    {<<"form">>, handle_links(Tags), Body};
get_links({<<"script">>, Tags, Body}) ->
    log:info("Script: ~p", [Tags]),
    {<<"script">>, handle_links(Tags), Body};
get_links({_, _, []} = Object) ->
    Object;
get_links({_, _} = Object) ->
    Object;
get_links(Object) when is_binary(Object) ->
    Object;
get_links({Name, Tags, List}) ->
%    log:info("Name: ~p", [Name]),
    {Name, Tags, get_links(List)};
get_links([Object|Tail]) ->
%    log:info("Object: ~p", [Object]),
    [get_links(Object)|get_links(Tail)];
get_links([]) ->
    [].

handle_links([]) ->
    [];
handle_links(Tags) ->
    handle_links(Tags, []).


handle_links([{<<"href">>, Url}|Tail], Acc) ->
    case re:run(Url, ?TARGET) of
        nomatch ->
            handle_links(Tail, [{<<"href">>, Url}|Acc]);
        _ ->
            Uuid =
            case ets:lookup(ets_link_to_url, Url) of
                [{Url, Key}] ->
                    Key;
                _ ->
                    Key = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                    ets:insert(ets_link_to_url, {Url, Key}),
                    ets:insert(ets_link_to_url, {Key, Url}),
                    Key
            end,
            log:info("[HandleLink] {href} Key: ~p, Url: ~p", [Key, Url]),
            handle_links(Tail, [{<<"href">>, <<"http://", ?MY_HOST/binary, "/link/", Uuid/binary>>}|Acc])
    end;
handle_links([{<<"data-search-href">>, Url}|Tail], Acc) ->
    case re:run(Url, ?TARGET) of
        nomatch ->
            handle_links(Tail, [{<<"data-search-href">>, Url}|Acc]);
        _ ->
            Uuid =
            case ets:lookup(ets_link_to_url, Url) of
                [{Url, Key}] ->
                    Key;
                _ ->
                    Key = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                    ets:insert(ets_link_to_url, {Url, Key}),
                    ets:insert(ets_link_to_url, {Key, Url}),
                    Key
            end,
            handle_links(Tail, [{<<"data-search-href">>, <<"http://", ?MY_HOST/binary, "/link/", Uuid/binary>>}|Acc])
    end;
handle_links([{<<"action">>, Url}|Tail], Acc) ->
    case re:run(Url, ?TARGET) of
        nomatch ->
            handle_links(Tail, [{<<"action">>, Url}|Acc]);
        _ ->
            Uuid =
            case ets:lookup(ets_link_to_url, Url) of
                [{Url, Key}] ->
                    Key;
                _ ->
                    Key = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                    ets:insert(ets_link_to_url, {Url, Key}),
                    ets:insert(ets_link_to_url, {Key, Url}),
                    Key
            end,
            handle_links(Tail, [{<<"action">>, <<"http://", ?MY_HOST/binary, "/link/", Uuid/binary>>}|Acc])
    end;
handle_links([{<<"src">>, Url}|Tail], Acc) ->
    case re:run(Url, ?TARGET) of
        nomatch ->
            handle_links(Tail, [{<<"src">>, Url}|Acc]);
        _ ->
            Uuid =
            case ets:lookup(ets_link_to_url, Url) of
                [{Url, Key}] ->
                    Key;
                _ ->
                    Key = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                    ets:insert(ets_link_to_url, {Url, Key}),
                    ets:insert(ets_link_to_url, {Key, Url}),
                    Key
            end,
            handle_links(Tail, [{<<"src">>, <<"http://", ?MY_HOST/binary, "/link/", Uuid/binary>>}|Acc])
    end;

handle_links([Object|Tail], Acc) ->
    handle_links(Tail, [Object|Acc]);
handle_links([], Acc) ->
    lists:reverse(Acc).


strip_head(<<10, Rest/binary>>) ->
    strip_head(Rest);
strip_head(<<13, Rest/binary>>) ->
    strip_head(Rest);
strip_head(<<9, Rest/binary>>) ->
    strip_head(Rest);
strip_head(<<32, Rest/binary>>) ->
    strip_head(Rest);
strip_head(Rest) ->
    Rest.

