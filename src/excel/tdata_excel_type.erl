-module(tdata_excel_type).

-export([
    get_type_checks/1
]).

%% @ => check

%% int integer
%% float
%% bool boolean
%% str string
%% bin binary
%% atom

get_type_checks(Bin) ->
    Str0 = string:strip(string:to_lower(binary_to_list(Bin))),
    {IsCheck, String} =
        case Str0 of
            [$@ | Str] ->
                {true, Str};
            _ ->
                {false, Str0}
        end,
    case get_type(String) of
        {ok, Type} ->
            get_type_checks(IsCheck, Type);
        Error -> Error
    end.

get_type("int") -> {ok, integer};
get_type("integer") -> {ok, integer};
get_type("float") -> {ok, float};
get_type("str") -> {ok, string};
get_type("string") -> {ok, string};
get_type("bool") -> {ok, boolean};
get_type("boolean") -> {ok, boolean};
get_type("bin") -> {ok, binary};
get_type("binary") -> {ok, binary};
get_type("atom") -> {ok, atom};
get_type(Type) -> {undef_type, Type}.

get_type_checks(IsCheck, integer) ->
    [fun(Cell) when is_integer(Cell) -> {ok, Cell};
        (<<>>) when IsCheck -> not_integer;
        (Cell) ->
            Int =
                try
                    binary_to_integer(Cell)
                catch
                    _:_ -> {not_integer, Cell}
                end,
            case is_integer(Int) of
                false when IsCheck ->
                    Int;
                false ->
                    {ok, 0};
                _ ->
                    {ok, Int}
            end
     end];
get_type_checks(IsCheck, float) ->
    [fun(Cell) when is_integer(Cell) -> {ok, Cell};
        (Cell) when is_float(Cell) -> {ok, Cell};
        (<<>>) when IsCheck -> not_float;
        (Cell) ->
            Float =
                try
                    to_float(Cell)
                catch
                    _:_ -> {not_float, Cell}
                end,
            case is_float(Float) of
                false when IsCheck ->
                    Float;
                false ->
                    {ok, 0};
                _ ->
                    {ok, Float}
            end
     end];
get_type_checks(IsCheck, boolean) ->
    [fun(Cell) ->
        case to_boolean(Cell) of
            undefined when IsCheck ->
                {not_boolean, Cell};
            undefined ->
                {ok, false};
            Bool ->
                {ok, Bool}
        end
     end];
get_type_checks(true, string) ->
    [check_not_empty, fun to_string/1];
get_type_checks(_IsCheck, string) ->
    [fun to_string/1];
get_type_checks(true, binary) ->
    [check_not_empty];
get_type_checks(_IsCheck, binary) ->
    [];
get_type_checks(IsCheck, atom) ->
    [fun(<<>>) when IsCheck -> not_atom;
        (<<>>) -> {ok, undefined};
        (Cell) ->
            Atom =
                try
                    binary_to_atom(Cell, utf8)
                catch
                    _:_ -> {not_atom, Cell}
                end,
            case is_atom(Atom) of
                false when IsCheck ->
                    Atom;
                false ->
                    {ok, undefined};
                _ ->
                    {ok, Atom}
            end
     end].

to_float(B) ->
    L = binary_to_list(B),
    case lists:member($., L) of
        true -> list_to_float(L);
        false -> list_to_float(L ++ ".0")  %% list_to_float("1") gives a badarg
    end.

to_string(Cell) ->
    {ok, binary_to_list(Cell)}.

to_boolean(<<"TRUE">>) -> true;
to_boolean(<<"True">>) -> true;
to_boolean(<<"true">>) -> true;
to_boolean(<<"1">>) -> true;
to_boolean(1) -> true;
to_boolean(<<"FALSE">>) -> false;
to_boolean(<<"False">>) -> false;
to_boolean(<<"false">>) -> false;
to_boolean(<<"0">>) -> false;
to_boolean(0) -> false;
to_boolean(_) -> undefined.
