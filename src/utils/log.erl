-module(log).



-compile(export_all).



debug(Message)->
    logger:debug(Message, #{node => node()}).
debug(Message, Params)->
    logger:debug(Message, Params, #{node => node()}).



info(Message)->
    logger:info(Message, #{node => node()}).
info(Message, Params)->
    logger:info(Message, Params, #{node => node()}).



notice(Message)->
    logger:info(Message, #{node => node()}).
notice(Message, Params)->
    logger:info(Message, Params, #{node => node()}).



warning(Message)->
    logger:info(Message).
warning(Message, Params)->
    logger:info(Message, Params).


error(Message)->
    logger:error(Message, #{node => node()}).
error(Message, Params)->
    logger:error(Message, Params, #{node => node()}).



alert(Message)->
    logger:info(Message, #{node => node()}).
alert(Message, Params)->
    logger:info(Message, Params, #{node => node()}).



critical(Message)->
    logger:critical(Message, #{node => node()}).
critical(Message, Params)->
    logger:critical(Message, Params, #{node => node()}).






pay(Message)->
    logger:info(Message, #{node => node()}).
pay(Message, Params)->
    logger:info(Message, Params, #{node => node()}).


pay_map(Action, Map, Result)->
    Message = "[API_LOG] [~p] [~p] ~p ~p",
    Params  = [
            data:get(<<"public_key">>, Map, null),
            data:get(<<"order_id">>, Map, null),
            Action,
            Result
    ],
    logger:info(Message, Params).



pay_error(Message)->
    logger:info(Message, #{node => node()}).
pay_error(Message, Params)->
    logger:info(Message, Params, #{node => node()}).

request(Body, Path) ->
    log:info("[REQ ~s] ~ts", [Path, mask_body(Body)]),
    ok.

request(ReqId, Action, Body) ->
    log:info("[REQ ~s] {~s} ~tp", [ReqId, Action, mask_body(Body)]),
    ok.

response(Response, Path, _) ->
    log:info("[RES][~s] ~ts", [Path, mask_body(Response)]),
    ok.


response(ReqId, Action, RespBody, _) ->
    log:info("[RES][~s] {~s}, ~tp", [ReqId, Action, mask_body(RespBody)]),
    ok.

mask_body(<<>>)->
    <<>>;
mask_body(Body) when is_binary(Body)->
    try
        lists:foldl(fun({Key, Val}, AccBody)->
            re:replace(AccBody, Key, Val, [{return, binary}, global])
        end, Body, [
            % ecommerce
            {<<"\"value\":\"([a-zA-Z0-9_!@#$&%^*();:,.<>]+)">>,
             <<"\"value\":\"*******">>},
            {<<"\"expd\":\"([0-9]{4})\"">>, <<"\"expd\":\"****\"">>},
            {<<"\"sid\":\"([0-9a-z]{20})\"">>, <<"\"sid\":\"**********\"">>},
            {<<"sid='([0-9a-z]{20})'">>, <<"sid='**********'">>},
            {<<"session='([0-9a-z]{20})'">>, <<"session='**********'">>},
            {<<"p24_password=([a-zA-Z0-9_!@#$%^*();:,.<>]+)">>,
             <<"p24_password=*******">>},
            {<<"cardcvv2=([0-9]{3})">>, <<"cardcvv2=***">>},
            {<<"cardexpdate=([0-9]{4})">>, <<"cardexpdate=****">>},
            {<<"([2-6]{1})([0-9]{3})([0-9]{8})([0-9]{4})">>,
             <<"\\g1\\g2********\\g4">>}
        ])
    catch T:R:S ->
        log:error("[MASK_BODY_ERR] ~p ~p ~p", [T, R, S]),
        Body
    end;
mask_body(Body)when is_list(Body)->
    try
        mask_body(list_to_binary(Body))
    catch _:_:_ ->
          lists:foldl(fun(V, AccList) ->
                              [mask_body(V)| AccList]
                      end,
          [], Body)
    end;

%    Body;
mask_body(Body) when is_map(Body) ->
    maps:fold(fun(K, V, AccBody) ->
                      case K of
                          <<"card">> ->
                              AccBody#{K => db_utils:mask(V)};
                          <<"can">> ->
                              AccBody#{K => db_utils:mask(V)};
                          <<"pan">> ->
                              AccBody#{K => db_utils:mask(V)};
                          <<"sender_card">> ->
                              AccBody#{K => db_utils:mask(V)};
                          <<"receiver_card">> ->
                              AccBody#{K => db_utils:mask(V)};
                          <<"card_exp_month">> ->
                              AccBody#{K => <<"**">>};
                          <<"card_exp_year">> ->
                              AccBody#{K => <<"**">>};
                          <<"private_key", _/binary>> ->
                              AccBody#{K => <<"******">>};
                          _ when is_map(V) ->
                              AccBody#{K => mask_body(V)};
                          _ ->
                              AccBody#{K => V}
                      end

              end, Body, Body);

mask_body(_)->
    <<>>.
