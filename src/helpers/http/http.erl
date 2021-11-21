-module(http).


-export([getURL/1, getURL/2]).
-export([post/2, post/3, post/4]).

-define(ALERT_TIMEOUT, 3000).


% API

getURL(URL)->
    getURL(URL, []).
getURL(URL, Headers)->
    request(get, URL, <<>>, Headers, [{timeout, 50000}]).

post(URL, Data)->
    post(URL, Data, []).
post(URL, Data, Headers)->
    post(URL, Data, Headers, [{timeout, 50000}]).
post(URL, Data, Headers, HTTPOptions)->
    request(post, URL, Data, Headers, HTTPOptions).




%%%%%%%%%%% LOCAL

request(Method, URL0, BodyCli, HeadersCli, HTTPOptions0)->

    HTTPOptions = [{connect_timeout, 5000} | HTTPOptions0],

    URL =
    case is_binary(URL0) of
        true -> binary_to_list(URL0);
        _    -> URL0
    end,

    %log:info("[HTTP_START] ~p", [URL]),

    Start  = erlang:timestamp(),
    Result = request_run(Method, URL, HeadersCli, BodyCli, HTTPOptions),
    Time   = trunc( timer:now_diff(erlang:timestamp(), Start)/1000),


    Res =
    case Result of
        {ok, 200, Body} ->

                log(200, URL, Time),
                {ok, Body, Time};

        {ok, Code, _Body} ->

                log(Code, URL, Time),
                {error, Code, Time};

        {error, Error}  ->

                %log:error("[HTTP ERROR] error ~p time: ~pms ~p~n",
                           %[URL, Time, Error]),
                {throw, Error, Time}
    end,

    Res.







% подключаемся к серверу
request_run(post, URL, Headers, Body, HTTPOptions)->

    {ContentType, ReqBody} = to_body(Headers, Body),

    Res = request_post(URL, Headers, ContentType, ReqBody, HTTPOptions),

    %%io:format("Res ~p~n", [Res]),

    case Res of

        % В erlang есть баг
        % https://elixirforum.com/t/18-3-4-erlang-tls1-2-iis-issues/2857/6
        % Сервера Microsoft IIS не устанавливают соединение, если используется
        % несколько версий протокала TLS
        % Как временное решение, можно установить один протокол,
        % например tlsv1.2
        % Поэтому пробуем переотправить запрос с протоколом tlsv1.2
        % Ждем решения проблемы в erlang
        % В erlang 17 такой проблемы нет

        % для тестого примера можно использовать
        % http_callback:post("https://graph.microsoft.io/en-us/docs", <<>>).
        % или
        % http_callback:post(
        % "https://api.assetpayments.com/LiqPayListner/HandleResponce", <<>>).
        % ответ сейчас:
        % {error,{error,
        % {failed_connect,[
        % {to_address,{"api.assetpayments.com",443}}, {inet,[inet],closed}]}}}


        {error,{error,
                {failed_connect,
                 [{to_address,{_,Port}},
                  {inet,[inet],closed}]}}} when Port =/= 80 ->

                HTTPOptions2 = [{ssl, [{versions, ['tlsv1.2']}]} | HTTPOptions],
                request_post(URL, Headers, ContentType, ReqBody, HTTPOptions2);

        _   ->
                Res

    end;








request_run(get, URL, Headers, _Body, HTTPOptions)->

    Res = httpc:request(get, {URL, Headers}, HTTPOptions,
                        [{body_format, binary}]),
    case Res of
        {ok, {{_,Code, _}, _, Response}} ->

                    {ok, Code, Response};

        Other   ->
                    {error, Other}
    end.






request_post(URL, Headers, ContentType, ReqBody, HTTPOptions)->

    %%io:format("URL ~p~n", [URL]),
    %%io:format("Headers ~p~n", [Headers]),
    %%io:format("ContentType ~p~n", [ContentType]),
    %%io:format("ReqBody ~p~n", [ReqBody]),
    %%io:format("HTTPOptions ~p~n", [HTTPOptions]),

    Res = httpc:request(post,
                        {to_list(URL), Headers, ContentType, to_list(ReqBody)},
                        HTTPOptions, [{body_format, binary}]),

    %%io:format("Res ~p~n", [Res]),

    case Res of
        {ok, {{_,Code, _}, _, Response}} ->

                    {ok, Code, Response};

        Other   ->
                    {error, Other}
    end.







to_body(Headers, Body)->
    case is_list(Body) of
        true ->
                {"application/x-www-form-urlencoded", compose_body(Body)};
        _   ->
                case data:get("content-type", Headers, null) of
                    null ->
                        add_type(Body);
%
%                            case Body of
%                                <<"<", _/binary>> -> {"text/xml", Body};
%                                _                 -> {"application/json", Body}
%                            end;
%
                    ContType   ->

                            {to_list(ContType), Body}

                end
    end.





% логируем HTTP запросы на API
log(200, URL, Time) when Time>?ALERT_TIMEOUT->
    log:alert("[HTTP WARNING] ~s 200 time: ~pms~n", [URL, Time]);
log(200, _URL, _Time)->
    ok; %log:info("[HTTP OK] ~s 200 time: ~pms~n", [_URL, _Time]);
log(_Code, _URL, _Time)->
    %log:error("[HTTP ERROR] ~s ~p time: ~pms~n", [URL, Code, Time])
    ok.




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
    [AmountList] = io_lib:format("~.2f", [Value]),
    AmountList.



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
    [AmountList] = io_lib:format("~.2f", [Value]),
    list_to_binary(AmountList).


%  compose_body
compose_body(Args) ->
    lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
            [],
            [to_list(K) ++ "="
             ++ to_list(cow_qs:urlencode(to_binary(V))) || {K, V} <- Args]
            %[to_list(K) ++ "=" ++ url_encode2(to_list(V)) || {K, V} <- Args]
        )
    ).


add_type(<<"<", _/binary>> = Body) ->
    {"text/xml", Body};
add_type(Body) ->
    {"application/json", Body}.
