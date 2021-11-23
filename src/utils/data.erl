-module(data).

-export([
         to_binary/1,
         getr/2, get/2, get/3
        ]).

getr(Key, ListOrMap)->
    case get(Key, ListOrMap) of
        undefined -> throw({throw_error, err_missing, #{<<"key">> => Key}});
        <<>>      -> throw({throw_error, err_empty, #{<<"key">> => Key}});
        Val       -> Val
    end.

get(Key, ListOrMap)->
    get(Key, ListOrMap, undefined).

get(Key, List, Default) when is_list(List) ->
    case proplists:get_value(Key, List, Default) of
        null -> Default;
        undefined -> Default;
        Val -> Val
    end;
get(Key, Map, Default) when is_map(Map) ->
    case maps:get(Key, Map, Default) of
        null -> Default;
        undefined -> Default;
        Val -> Val
    end;
get(_, _, Default) ->
    Default.



to_binary(Val) when is_binary(Val) ->
    Val;
to_binary(Val) when is_list(Val) ->
    list_to_binary(Val);
to_binary(Val) when is_integer(Val) ->
    integer_to_binary(Val);
to_binary(Val) when is_float(Val) ->
    float_to_binary(Val);
to_binary(Val) when is_atom(Val) ->
    list_to_binary(atom_to_list(Val));
to_binary(Term) ->
    term_to_binary(Term).
