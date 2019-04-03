-module(tdata_excel_util).

-export([
    check_integer/1,
    check_float/1,
    check_not_empty/1,

    cell_to_fun/1,
    cell_trans_punctuation/1,

    trans_punctuation/1,
    trans_punctuation/2
]).

check_integer(Cell) when is_integer(Cell) -> {ok, Cell};
check_integer(_Cell) -> not_integer.

check_float(Cell) when is_float(Cell) -> {ok, Cell};
check_float(Cell) when is_integer(Cell) -> {ok, Cell};
check_float(_Cell) -> not_float.

check_not_empty(<<>>) -> empty;
check_not_empty([]) -> empty;
check_not_empty(Cell) -> {ok, Cell}.

cell_to_fun(Fun) ->
    fun(Cell) ->
        {ok, Fun(Cell)}
    end.

cell_trans_punctuation(Cell) ->
    {ok, trans_punctuation(Cell)}.

%% trans double byte punctuation
%% 转换双字节标点符号
trans_punctuation(Binary) ->
    TransPunctuationList = [
        {<<"。"/utf8>>, <<".">>},
        {<<"；"/utf8>>, <<";">>},
        {<<"，"/utf8>>, <<",">>},
        {<<"："/utf8>>, <<":">>},
        {<<"“"/utf8>>, <<"\"">>},
        {<<"”"/utf8>>, <<"\"">>},
        {<<"（"/utf8>>, <<"(">>},
        {<<"）"/utf8>>, <<")">>},
        {<<"【"/utf8>>, <<"[">>},
        {<<"】"/utf8>>, <<"]">>},
        {<<"？"/utf8>>, <<"?">>}
    ],
    trans_punctuation(Binary, TransPunctuationList).
trans_punctuation(Binary, [{Old, New}|List]) ->
    NewBinary = binary:replace(Binary, Old, New, [global]),
    trans_punctuation(NewBinary, List);
trans_punctuation(Binary, []) -> Binary.