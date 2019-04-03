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
        same_level_groups,
        checks,
        only_rows,
        type_comment
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
    Rows = #{
        1 => #{
            11 => [
                #{level_1 => 1, level_2 => 11, level_3 => 111},
                #{level_1 => 1, level_2 => 11, level_3 => 112}
            ],
            12 => [
                #{level_1 => 1, level_2 => 12, level_3 => 113},
                #{level_1 => 1, level_2 => 12, level_3 => 114}
            ]
        },
        2 => #{
            21 => [
                #{level_1 => 2, level_2 => 21, level_3 => 221},
                #{level_1 => 2, level_2 => 21, level_3 => 222}
            ],
            22 => [
                #{level_1 => 2, level_2 => 22, level_3 => 223},
                #{level_1 => 2, level_2 => 22, level_3 => 224}
            ]
        }
    },
    #{<<"Sheet1">> := #{rows := Rows}} = Sheets,
    ok.

same_level_groups(Config) -> % same level group
    ExcelFile = filename:join(?config(data_dir, Config), "groups.xlsx"),
    LoadSheetsOpts = #{<<"Sheet2">> => #{groups => [[level_1, level_2]]}},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    Rows = #{
        1 => [
            #{level_1 => 1, level_2 => 11, level_3 => 111},
            #{level_1 => 1, level_2 => 11, level_3 => 112},
            #{level_1 => 1, level_2 => 11, level_3 => 113},
            #{level_1 => 1, level_2 => 11, level_3 => 114}
        ],
        2 => [
            #{level_1 => 2, level_2 => 22, level_3 => 221},
            #{level_1 => 2, level_2 => 22, level_3 => 222},
            #{level_1 => 2, level_2 => 22, level_3 => 223},
            #{level_1 => 2, level_2 => 22, level_3 => 224}
        ]
    },
    #{<<"Sheet2">> := #{rows := Rows}} = Sheets,
    ok.

checks(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "checks.xlsx"),
    LoadSheetsOpts = #{<<"Sheet1">> => #{checks => [
        {int, [check_integer]},
        {double_int, [check_integer, tdata_excel_util:cell_to_fun(fun double/1)]},
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
            {double_int, [check_integer, tdata_excel_util:cell_to_fun(fun double/1)]},
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

type_comment(Config) ->
    ExcelFile = filename:join(?config(data_dir, Config), "types.xlsx"),
    LoadSheetsOpts = #{<<"Sheet1">> => #{
        type_comment => true,
        only_rows => true,
        checks => [
            {int_check, [tdata_excel_util:cell_to_fun(fun double/1)]}
        ]
    }},
    {ok, Sheets} = tdata_excel_loader:load_sheets(ExcelFile, LoadSheetsOpts),
    Rows = [
        #{atom => a, atom_check => a, bin => <<"b1">>, bin_alias => <<"b1">>,
            bin_check => <<"b1">>, bool => true, bool_alias => true,
            bool_check => true, float => 1.1, float_check => 1.1, int => 1,
            int_alias => 0, int_check => 2, str => "a1", str_alias => "a1",
            str_check => "a1"},
        #{atom => b, atom_check => b, bin => <<"b2">>, bin_alias => <<"b2">>,
            bin_check => <<"b2">>, bool => false, bool_alias => false,
            bool_check => false, float => 0, float_check => 2.1, int => 2,
            int_alias => 2, int_check => 4, str => "a2", str_alias => "a2",
            str_check => "a2"},
        #{atom => c, atom_check => c, bin => <<"b3">>, bin_alias => <<"b3">>,
            bin_check => <<"b3">>, bool => true, bool_alias => false,
            bool_check => true, float => 3.1, float_check => 3.1, int => 3,
            int_alias => 3, int_check => 6, str => "a3", str_alias => "a3",
            str_check => "a3"},
        #{atom => d, atom_check => d, bin => <<"b4">>, bin_alias => <<"b4">>,
            bin_check => <<"b4">>, bool => false, bool_alias => false,
            bool_check => false, float => 4.1, float_check => 4.1, int => 4,
            int_alias => 4, int_check => 8, str => "a4", str_alias => [],
            str_check => "a4"},
        #{atom => undefined, atom_check => e, bin => <<"b5">>, bin_alias => <<>>,
            bin_check => <<"b5">>, bool => true, bool_alias => true,
            bool_check => true, float => 5.1, float_check => 5.1, int => 5,
            int_alias => 5, int_check => 10, str => "a5", str_alias => "a5",
            str_check => "a5"}
    ],
    #{<<"Sheet1">> := Rows} = Sheets,
%%    ct:print("~p", [Sheets]),
    ok.