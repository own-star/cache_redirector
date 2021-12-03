-module(http_service2).


-export([post/2, post/3, post/4]).
-export([getURL/1, getURL/2, getURL/3]).
-export([get/1, get/2, get/3]).

post(Url, ReqData) ->
    post(Url, ReqData, []).
post(Url, ReqData, Headers) ->
    post(Url, ReqData, Headers, []).
post(Url, ReqData, Headers, Options) ->
    request(post, Url, ReqData, Headers, Options).

getURL(Url) ->
    get(Url, []).
getURL(Url, Headers) ->
    get(Url, Headers, []).
getURL(Url, Headers, Options) ->
    request(get, Url, <<>>, Headers, Options).

get(Url) ->
    get(Url, []).
get(Url, Headers) ->
    get(Url, Headers, []).
get(Url, Headers, Options) ->
    request(get, Url, <<>>, Headers, Options).



%%%%%%%%%%%%  LOCAL   %%%%%%%%%%%%

request(Method, Url, ReqData, Headers, Options) ->
    log:info("[HTTP_SERVICE2_REQ ~ts] ~ts", [to_binary(Url), ReqData]),
    {Host, Port, _} = parse_url(Url),
    PoolName = pool_name(Host, Port),
    Size = 5,
    PoolOptions = [],
%    {Size, PoolOptions} = to_http(Host),
    case hackney_pool:find_pool(PoolName) of
        undefined ->
            hackney_pool:start_pool(PoolName, [{pool_size, Size} | PoolOptions]),
            log:info("[HTTP_SERVICE2] {start_pool} PoolName: ~p, PoolSize: ~p", [PoolName, Size]);
        _ ->
            ok
    end,
    Start  = erlang:system_time(millisecond),
    Resp = hackney:request(Method, Url, Headers, ReqData, [{pool, PoolName}, {connect_timeout, 5000}, with_body | Options]),
    Time = erlang:system_time(millisecond) - Start,
    case Resp of
        {ok, 200, _RespHeaders, RespBody} ->
            log:info("[HTTP_SERVICE2_RES ~p ~pms] ~ts",
                     [200, Time, RespBody]),
            {ok, RespBody, Time};
        {ok, Code, RespHeaders, RespBody} ->
            log:info("[HTTP_SERVICE2_RES ~p ~pms] ~ts",
                     [Code, Time, RespBody]),
            {ok, Code, RespHeaders, RespBody};
        {error, Err} ->
            log:info("[HTTP_SERVICE2_ERR ~pms] ~p", [Time, Err]),
            {throw, Err, Time}
    end.
 




%%%%%%  HELP FUNCTIONS %%%%%%

parse_url(Url) when is_list(Url) ->
    parse_url(list_to_binary(Url));

parse_url(<<"http://", Rest/binary>>) ->
    parse_url(http, Rest);
parse_url(<<"https://", Rest/binary>>) ->
    parse_url(https, Rest);
parse_url(_) ->
    {error, invalid_url}.

%% private
parse_url(Protocol, Rest) ->
    {Host, Path} =
    case binary:split(Rest, <<"/">>, [trim]) of
        [Host2] ->
            {Host2, <<"/">>};
        [Host2, Path2] ->
            {Host2, <<"/", Path2/binary>>}
    end,

    {Hostname, Port} =
    case binary:split(Host, <<":">>, [trim]) of
        [Host] ->
            case Protocol of
                http ->
                    {Host, 80};
                https ->
                    {Host, 443}
            end;
        [Hostname2, Port2] ->
            {Hostname2, binary_to_integer(Port2)}
    end,

    {
        Hostname,
        Port,
        Path
    }.

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



%to_http(Host)->
%    SettingsAll = app_settings:get(http_pools),
%    case lists:keyfind(Host, 1, SettingsAll) of
%        false ->
%            {_, Settings} = lists:keyfind(<<"default">>, 1, SettingsAll),
%            normalize_settings(Settings);
%
%        {_, Settings}  ->
%            normalize_settings(Settings)
%    end.
%
%normalize_settings({Size, Options}) when is_map(Options) ->
%    {Size, maps:to_list(Options)};
%normalize_settings(Settings) ->
%    Settings.

pool_name(Host, Port) ->
    list_to_atom(binary_to_list(Host) ++ ":" ++ integer_to_list(Port)).

