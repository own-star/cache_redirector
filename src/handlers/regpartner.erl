-module(login).

-include("cr.hrl").

-export([init/2]).

init(Req, _) ->
    Headers0 = cowboy_req:headers(Req),
    Headers = maps:without([<<"host">>, <<"referer">>, <<"origin">>], Headers0),
%    NewHeaders = [{"content-type", "application/x-www-form-urlencoded; charset=UTF-8"}],
    NewHeaders = http_service:to_headers(Headers),
%    NewHeaders = Headers#{
%                          <<":authority">> => <<"my.hightech.trade">>,
%                          <<":method">> => <<"POST">>,
%                          <<":path">> => <<"/auth/login">>,
%                          <<":scheme">> => <<"https">>,
%                          <<"authority">> => <<"my.hightech.trade">>,
%                          <<"method">> => <<"POST">>,
%                          <<"path">> => <<"/auth/login">>,
%                          <<"scheme">> => <<"https">>,
%                          <<"host">> => <<"hightech.trade">>,
%                          <<"origin">> => <<"https://my.hightech.trade">>,
%                          <<"referer">> => <<"https://my.hightech.trade/login">>,
%                          <<"sec-ch-ua">> => <<"\" Not;A Brand\";v=\"99\", \"Google Chrome\";v=\"91\", \"Chromium\";v=\"91\"">>,
%                          <<"sec-ch-ua-mobile">> => <<"?0">>,
%                          <<"sec-fetch-dest">> => <<"empty">>,
%                          <<"sec-fetch-mode">> => <<"cors">>,
%                          <<"sec-fetch-site">> => <<"same-origin">>
%                  },
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    log:info("[LOGIN] Headers: ~p", [NewHeaders]),
    case http_service:post(<<"https://my.", ?TARGET/binary, "/auth/regPartner">>, Data, NewHeaders) of
%        Body ->
%            Response = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Body, Req2),
%            {ok, Response, []};
        {ok, Json, RespHeaders} ->
            Resp = cowboy_req:reply(200,  maps:from_list(RespHeaders), Json, Req2),
            {ok, Resp, []};
        {ok, Code, RespHeaders, Body} ->
            Resp = cowboy_req:reply(Code, maps:from_list(RespHeaders), Body, Req2),
            {ok, Resp, undefined};
        {error, Code, Body, _} ->
            {ok, cowboy_req:reply(Code, #{}, Body, Req2), undefined}
    end.



