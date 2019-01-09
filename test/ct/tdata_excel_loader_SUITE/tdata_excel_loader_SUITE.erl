-module(tdata_excel_loader_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

all() ->
    [
        sheet_name,
        all_sheet_name,
        skip_comments,
        skip_n_comments,
        groups,
        same_groups,
        checks,
        only_rows
    ].

sheet_name(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "sheet_name.xlsx"),
    LoadSheetsOpts = #{<<"Sheet1">> => #{}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    1 = maps:size(Sheets),
    true = maps:is_key(<<"Sheet1">>, Sheets),
    ok.

all_sheet_name(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "sheet_name.xlsx"),
    LoadSheetsOpts = #{all => #{}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    2 = maps:size(Sheets),
    true = maps:is_key(<<"Sheet1">>, Sheets),
    true = maps:is_key(<<"Sheet2">>, Sheets),
    ok.

skip_comments(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "skip_comments.xlsx"),
    LoadSheetsOpts = #{<<"Sheet1">> => #{skip_comments => true}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    #{<<"Sheet1">> := #{comments := [<<"comments">>]}} = Sheets,
    ok.

skip_n_comments(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "skip_comments.xlsx"),
    LoadSheetsOpts = #{<<"Sheet2">> => #{skip_comments => 2}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    #{<<"Sheet2">> := #{comments := [[<<"comments1">>], [<<"comments2">>]]}} = Sheets,
    ok.

groups(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "groups.xlsx"),
    LoadSheetsOpts = #{<<"Sheet1">> => #{groups => [level_1, level_2]}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    Rows = #{level_1 => #{1 => #{level_2 => #{
        21 => [#{level_1 => 1, level_2 => 21, level_3 => 31}, #{level_1 => 1, level_2 => 21, level_3 => 32}],
        22 => [#{level_1 => 1, level_2 => 22, level_3 => 33}, #{level_1 => 1, level_2 => 22, level_3 => 34}]
    }}}},
    #{<<"Sheet1">> := #{rows := Rows}} = Sheets,
    ok.

same_groups(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "groups.xlsx"),
    LoadSheetsOpts = #{<<"Sheet2">> => #{groups => [[level_1, level_2]]}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    Rows = #{level_1 => #{1 => [
        #{level_1 => 1, level_2 => 2, level_3 => 31},
        #{level_1 => 1, level_2 => 2, level_3 => 32},
        #{level_1 => 1, level_2 => 2, level_3 => 33},
        #{level_1 => 1, level_2 => 2, level_3 => 34}
    ]}},
    #{<<"Sheet2">> := #{rows := Rows}} = Sheets,
    ok.

checks(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "checks.xlsx"),
    LoadSheetsOpts = #{<<"Sheet1">> => #{checks => [
        {int, [check_integer]},
        {double_int, [check_integer, tdata_util:cell_to_fun(fun double/1)]},
        {str, [check_not_empty]}
    ]}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    Rows = [
        #{double_int => 2, int => 1, str => <<"abc1">>},
        #{double_int => 4, int => 2, str => <<"abc2">>},
        #{double_int => 6, int => 3, str => <<"abc3">>}
    ],
    #{<<"Sheet1">> := #{rows := Rows}} = Sheets,
    ok.

double(Cell) -> Cell * 2.

only_rows(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "checks.xlsx"),
    LoadSheetsOpts = #{<<"Sheet1">> => #{
        only_rows => true,
        checks => [
            {int, [check_integer]},
            {double_int, [check_integer, tdata_util:cell_to_fun(fun double/1)]},
            {str, [check_not_empty]}
        ]
    }},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    Rows = [
        #{double_int => 2, int => 1, str => <<"abc1">>},
        #{double_int => 4, int => 2, str => <<"abc2">>},
        #{double_int => 6, int => 3, str => <<"abc3">>}
    ],
    #{<<"Sheet1">> := Rows} = Sheets,
    ok.