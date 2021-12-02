-module(login).

-include("cr.hrl").

-export([init/2]).

init(Req, _) ->
    Headers = cowboy_req:headers(Req),
    NewHeaders = Headers#{<<"host">> => <<"hightech.trade">>,
                          <<"origin">> => <<"https://my.hightech.trade">>,
                          <<"referer">> => <<"https://my.hightech.trade/login">>},
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    log:info("[LOGIN] Headers: ~p", [NewHeaders]),
    case http_service:post(<<"https://my.", ?TARGET/binary, "/auth/login">>, Data, NewHeaders) of
        {ok, Json, _} ->
            Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Json, Req2),
            {ok, Resp, []};
        {error, Code, _} ->
            {ok, cowboy_req:reply(Code, #{}, <<>>, Req2), undefined}
    end.
