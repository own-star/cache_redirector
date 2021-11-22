-module(http_service).


-export([getURL/1, getURL/2]).
-export([post/2, post/3, post/4]).

-define(ALERT_TIMEOUT, 3000).


% API

getURL(URL)->
    getURL(URL, []).
getURL(URL, Headers)->
    getURL(URL, Headers, #{}).
getURL(URL, Headers, LogOptions)->
    request(get, URL, <<>>, Headers, LogOptions).


post(URL, Data)->
    post(URL, Data, []).
post(URL, Data, Headers)->
    post(URL, Data, Headers, #{}).
post(URL, Data, Headers, LogOptions)->
    request(post, URL, Data, Headers, LogOptions).




%%%%%%%%%%% LOCAL

request(Method, URL0, BodyCli, HeadersCli, LogOptions)->

    HTTPOptions = [{connect_timeout, 5000}, {timeout, 50000}, {ssl, [
                        {verify, verify_none},
                        %{server_name_indication, disable},
                        {ciphers, ssl:cipher_suites(all)}
                    ]}],

    URL =
    case is_binary(URL0) of
        true -> binary_to_list(URL0);
        _    -> URL0
    end,

    Start  = erlang:timestamp(),
    Result = request_run(Method, URL, HeadersCli,
                         BodyCli, HTTPOptions, LogOptions),
    Time   = trunc( timer:now_diff(erlang:timestamp(), Start)/1000),
    Res =
    case Result of
        {ok, 200, Body} ->

                {ok, Body, Time};

        {ok, Code, _Body} ->

                {error, Code, Time};

        {error, Error}  ->

                {throw, Error, Time}
    end,

    Res.





request_run(post, URL, Headers, Body, HTTPOptions, _LogOptions)->

    {ContentType, ReqBody0} =
    case is_list(Body) of
        true ->
                {"application/x-www-form-urlencoded", compose_body(Body)};
        _   ->
                case data:get("content-type", Headers, null) of
                    null ->
                            exam_content_type(Body);
                    ContType   ->

                            {to_list(ContType), Body}

                end
    end,

    ReqBody = to_binary(ReqBody0),
    log:info("[HTTP_SERVICE_REQ {post} ~ts] ~ts", [to_binary(URL), ReqBody]),
    HttpProfile = http_server:get_profile(),
    Start  = erlang:system_time(milli_seconds),
    Result = httpc:request(post, {to_list(URL), to_headers(Headers),
                                  ContentType, ReqBody},
                           HTTPOptions, [{body_format, binary}], HttpProfile),
    Time   = erlang:system_time(milli_seconds) - Start,

    case Result of

        {ok, {{_, Status, _}, _, Response}} ->

%            log:info("[HTTP_SERVICE_RES ~p ~pms] ~ts", [Status, Time, Response]),
            {ok, Status, Response};

        Error   ->
            log:info("[HTTP_SERVICE_ERR ~pms] ~p", [Time, Error]),
            {error, Error}
    end;




request_run(get, URL, Headers, _ReqBody, HTTPOptions, _LogOptions)->

    log:info("[HTTP_SERVICE_REQ {get} ~ts]", [URL]),
    HttpProfile = http_server:get_profile(),
    Start  = erlang:system_time(milli_seconds),
    Result = httpc:request(get, {URL, to_headers(Headers)}, HTTPOptions, [{body_format, binary}], HttpProfile),
    Time   = erlang:system_time(milli_seconds) - Start,

    case Result of

        {ok, {{_, Status, _}, _, Response}} ->
%            log:info("[HTTP_SERVICE_RES ~p ~pms] ~ts", [Status, Time, Response]),
            {ok, Status, Response};

        Error   ->
            log:info("[HTTP_SERVICE_ERR ~pms] ~p", [Time, Error]),
            {error, Error}
    end.


to_headers(Headers)->
    lists:map(fun({Key, Val})->
        {util:to_list(Key), util:to_list(Val)}
    end, Headers).


exam_content_type(<<"<", _/binary>> = Body) ->
    {"text/xml", Body};
exam_content_type(Body) ->
    {"application/json", Body}.




to_list(null)->
    "";
to_list(undefined)->
    "";
to_list(Value) when is_list(Value)->
    Value;
to_list(Value) when is_binary(Value)->
    binary_to_list(Value);
to_list(Value) when is_integer(Value)->
    integer_to_list(Value);
to_list(Value) when is_float(Value)->
    case io_lib:format("~.2f", [Value]) of
        [AmountList] ->
            AmountList;
        FlatAmountList ->
            FlatAmountList
    end.



to_binary(null)->
    <<>>;
to_binary(undefined)->
    <<>>;
to_binary(Value) when is_binary(Value)->
    Value;
to_binary(Value) when is_list(Value)->
    list_to_binary(Value);
to_binary(Value) when is_integer(Value)->
    integer_to_binary(Value);
to_binary(Value) when is_float(Value)->
    case io_lib:format("~.2f", [Value]) of
        [AmountList] ->
            list_to_binary(AmountList);
        FlatAmountList ->
            list_to_binary(FlatAmountList)
    end.


compose_body(Args) ->
    lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
            [],
            [to_list(K) ++ "=" ++
             to_list(cow_qs:urlencode(to_binary(V))) || {K, V} <- Args]
        )
    ).



