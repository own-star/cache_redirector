-module(main).

-include("cr.hrl").

-export([init/2]).
-export([get_url/1]).
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
        {ok, "", _} ->
            log:info("[main] EmptyResp"),
            <<>>;
        {ok, <<>>, _} ->
            log:info("[main] EmptyResp"),
            <<>>;
        {ok, Page, _} ->
            %log:info("[main] Page: ~p", [Page]),
            try
                Tree0 = mochiweb_html:parse(Page),
                Tree = get_links(Tree0),
                list_to_binary(mochiweb_html:to_html(Tree))
            catch _ ->
                      <<>>
            end;
        Other ->
            log:info("[main] HttpResp: ~p", [Other]),
            <<>>
    end.


%%%%%%%%%%% LOCAL

%get_links(Tree) ->
%    get_links(Tree, []).

get_links({<<"link">>, Tags, Body}) ->
%    log:info("Link: ~p", [proplists:get_value(<<"href">>, Tags, <<>>)]),
    {<<"link">>, handle_links(Tags), Body};
get_links({<<"a">>, Tags, Body}) ->
%    log:info("Link: ~p", [proplists:get_value(<<"href">>, Tags, <<>>)]),
    {<<"a">>, handle_links(Tags), Body};
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
%            Uuid = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
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
%            ets:insert(ets_link_to_url, {Uuid, Url}),
            handle_links(Tail, [{<<"href">>, <<"http://", ?MY_HOST/binary, "/link/", Uuid/binary>>}|Acc])
    end;
handle_links([Object|Tail], Acc) ->
    handle_links(Tail, [Object|Acc]);
handle_links([], Acc) ->
    lists:reverse(Acc).
