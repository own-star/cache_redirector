-module(link).


-export([init/2]).


init(Req, _) ->
    PathList = cowboy_req:path_info(Req),
    [Path|_] = PathList,
    log:info("Path: ~p", [Path]),
    case ets:lookup(ets_link_to_url, Path) of
        [{Path, Url}] ->
            log:info("Url: ~p", [Url]),
            Body = main:get_url(Url),
            Response = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
            {ok, Response, []};
        _ ->
            {ok, cowboy_req:reply(404, #{}, <<>>, Req), undefined}
    end.
