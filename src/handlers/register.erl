-module(register).

-include("cr.hrl").

-export([init/2]).

init(Req, _) ->
%    Headers = cowboy_req:headers(Req),
    Headers = [{"content-type", "application/x-www-form-urlencoded; charset=UTF-8"}],
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    {ok, Json, RespHeaders} = http_service:post(<<"https://my.", ?TARGET/binary, "/auth/register">>, Data, Headers),
    Resp = cowboy_req:reply(200, maps:from_list(RespHeaders), Json, Req2),
    {ok, Resp, []}.
