-module(countries).

-include("cr.hrl").

-export([init/2]).

init(Req, _) ->
    {ok, Json, _} = http_service:getURL(<<"https://my.", ?TARGET/binary, "/auth/countries">>),
    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Json, Req),
    {ok, Resp, []}.
