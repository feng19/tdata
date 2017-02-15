-module(tdata_util).

-export([
    check_integer/1,
    check_float/1,
    check_not_empty/1,

    cell_trans_double_byte_punctuation/1,

    key_delete_all/3,
    trans_double_byte_punctuation/1,
    trans_double_byte_punctuation/2
]).

check_integer(Cell) when is_integer(Cell) -> {ok, Cell};
check_integer(_Cell) -> not_integer.

check_float(Cell) when is_float(Cell) -> {ok, Cell};
check_float(Cell) when is_integer(Cell) -> {ok, Cell};
check_float(_Cell) -> not_float.

check_not_empty(<<>>) -> empty;
check_not_empty([]) -> empty;
check_not_empty(Cell) -> {ok, Cell}.

cell_trans_double_byte_punctuation(Cell) ->
    {ok, trans_double_byte_punctuation(Cell)}.

key_delete_all(Key, N, [H|T]) when element(N, H)==Key ->
    key_delete_all(Key, N, T);
key_delete_all(Key, N, [H|T]) ->
    [H|key_delete_all(Key, N, T)];
key_delete_all(_Key, _N, []) -> [].

%% trans double byte punctuation
%% 转换双字节标点符号
trans_double_byte_punctuation(Binary) ->
    TransPunctuationList = [
        {<<"。"/utf8>>, <<".">>},
        {<<"；"/utf8>>, <<";">>},
        {<<"，"/utf8>>, <<",">>},
        {<<"："/utf8>>, <<":">>},
        {<<"“"/utf8>>, <<"\"">>},
        {<<"”"/utf8>>, <<"\"">>},
        {<<"（"/utf8>>, <<"(">>},
        {<<"）"/utf8>>, <<")">>},
        {<<"？"/utf8>>, <<"?">>}
    ],
    trans_double_byte_punctuation(Binary, TransPunctuationList).
trans_double_byte_punctuation(Binary, [{Old, New}|List]) ->
    NewBinary = binary:replace(Binary, Old, New, [global]),
    trans_double_byte_punctuation(NewBinary, List);
trans_double_byte_punctuation(Binary, []) -> Binary.