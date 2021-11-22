-module(link).


-export([init/2]).


init(Req, _) ->
    PathList = cowboy_req:path_info(Req),
    Method = cowboy_req:method(Req),
    [Path|_] = PathList,
    log:info("Path: ~p, PathList: ~p", [Path, PathList]),
    case ets:lookup(ets_link_to_url, Path) of
        [{Path, Url}] ->
            log:info("Url: ~p", [Url]),
            case Method of
                <<"GET">> ->
                    Body = main:get_url(Url),
                    Response = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
                    {ok, Response, []};
                <<"POST">> ->
                    Headers = cowboy_req:headers(Req),
                    {ok, Data, Req2} = cowboy_req:read_body(Req),
                    Body = main:post_url(Url, Data, Headers),
                    Response = cowboy_req:reply(200, Headers, Body, Req2),
                    {ok, Response, []};
                _ ->
                    {ok, cowboy_req:reply(405, #{}, <<>>, Req), undefined}
            end;
        _ ->
            {ok, cowboy_req:reply(404, #{}, <<>>, Req), undefined}
    end.
