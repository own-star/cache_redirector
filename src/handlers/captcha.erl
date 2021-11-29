-module(captcha).

-include("cr.hrl").

-export([init/2]).

init(Req, _) ->
    {ok, Img, _} = http_service:getURL(<<"https://my.", ?TARGET/binary, "/captcha.png">>),
    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"image/png">>}, Img, Req),
    {ok, Resp, []}.
