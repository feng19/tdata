%% Automatically generated, do not edit
%% Source Files: config.xlsx
-module(config).

-export([records/1, groups/2, same_level_groups/2]).

records(1) ->
    #{atom => a,atom_check => a,bin => <<"b1">>,bin_alias => <<"b1">>,
  bin_check => <<"b1">>,bool => true,bool_alias => true,bool_check => true,
  float => 1.1,float_check => 1.1,int => 1,int_alias => 0,int_check => 1,
  str => "a1",str_alias => "a1",str_check => "a1"};
records(2) ->
    #{atom => b,atom_check => b,bin => <<"b2">>,bin_alias => <<"b2">>,
  bin_check => <<"b2">>,bool => false,bool_alias => false,bool_check => false,
  float => 0,float_check => 2.1,int => 2,int_alias => 2,int_check => 2,
  str => "a2",str_alias => "a2",str_check => "a2"};
records(3) ->
    #{atom => c,atom_check => c,bin => <<"b3">>,bin_alias => <<"b3">>,
  bin_check => <<"b3">>,bool => true,bool_alias => false,bool_check => true,
  float => 3.1,float_check => 3.1,int => 3,int_alias => 3,int_check => 3,
  str => "a3",str_alias => "a3",str_check => "a3"};
records(4) ->
    #{atom => d,atom_check => d,bin => <<"b4">>,bin_alias => <<"b4">>,
  bin_check => <<"b4">>,bool => false,bool_alias => false,bool_check => false,
  float => 4.1,float_check => 4.1,int => 4,int_alias => 4,int_check => 4,
  str => "a4",str_alias => [],str_check => "a4"};
records(5) ->
    #{atom => undefined,atom_check => e,bin => <<"b5">>,bin_alias => <<>>,
  bin_check => <<"b5">>,bool => true,bool_alias => true,bool_check => true,
  float => 5.1,float_check => 5.1,int => 5,int_alias => 5,int_check => 5,
  str => "a5",str_alias => "a5",str_check => "a5"}.

groups(1, 11) ->
    [111,112];
groups(1, 12) ->
    [113,114];
groups(2, 21) ->
    [221,222];
groups(2, 22) ->
    [223,224].

same_level_groups(1, 11) ->
    [111,112,113,114];
same_level_groups(2, 22) ->
    [221,222,223,224].
