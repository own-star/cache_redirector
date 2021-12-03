-module(link).


-export([init/2]).


init(Req, _) ->
    PathList = cowboy_req:path_info(Req),
    Method = cowboy_req:method(Req),
    [Path|_] = PathList,
    Qs = cowboy_req:qs(Req),
    log:info("Path: ~p, PathList: ~p, Qs: ~p", [Path, PathList, Qs]),
    case ets:lookup(ets_link_to_url, Path) of
        [{Path, Url}] ->
            log:info("Url: ~p", [Url]),
            case Method of
                <<"GET">> when Qs =:= <<>> ->
                    Body = main:get_url(Url),
                    Response = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
                    {ok, Response, []};
                <<"GET">> ->
                    Body = main:get_url(<<Url/binary, "?", Qs/binary>>),
                    Response = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
                    {ok, Response, []};
                <<"POST">> ->
                    Headers = cowboy_req:headers(Req),
                    {ok, Data, Req2} = cowboy_req:read_body(Req),
                    {Body, RespHeaders} = main:post_url(Url, Data, Headers),
                    Response = cowboy_req:reply(200, maps:form_list(RespHeaders), Body, Req2),
                    {ok, Response, []};
                _ ->
                    {ok, cowboy_req:reply(405, #{}, <<>>, Req), undefined}
            end;
        _ ->
            {ok, cowboy_req:reply(404, #{}, <<>>, Req), undefined}
    end.
