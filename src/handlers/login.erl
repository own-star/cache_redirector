-module(login).

-include("cr.hrl").

-export([init/2]).

init(Req, _) ->
    Headers = cowboy_req:headers(Req),
    NewHeaders = Headers#{
                          <<":authority">> => <<"my.hightech.trade">>,
                          <<":method">> => <<"POST">>,
                          <<":path">> => <<"/auth/login">>,
                          <<":scheme">> => <<"https">>,
                          <<"authority">> => <<"my.hightech.trade">>,
                          <<"method">> => <<"POST">>,
                          <<"path">> => <<"/auth/login">>,
                          <<"scheme">> => <<"https">>,
                          <<"host">> => <<"hightech.trade">>,
                          <<"origin">> => <<"https://my.hightech.trade">>,
                          <<"referer">> => <<"https://my.hightech.trade/login">>},
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    log:info("[LOGIN] Headers: ~p", [NewHeaders]),
    case http_service2:post(<<"https://my.", ?TARGET/binary, "/auth/login">>, Data, maps:to_list(NewHeaders)) of
        {ok, Json, _} ->
            Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Json, Req2),
            {ok, Resp, []};
        {error, Code, Body, _} ->
            {ok, cowboy_req:reply(Code, #{}, Body, Req2), undefined}
    end.
