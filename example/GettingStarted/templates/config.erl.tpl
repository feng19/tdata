-module({{module_name}}).

-export([records/1, groups/2, same_level_groups/2]).

{{#records}}
records({{index}}) ->
    {{{data}}}{{#is_last}}.{{/is_last}}{{^is_last}};{{/is_last}}
{{/records}}

{{#groups}}
groups({{level_1}}, {{level_2}}) ->
    {{{level_3}}}{{#is_last}}.{{/is_last}}{{^is_last}};{{/is_last}}
{{/groups}}

{{#same_level_groups}}
same_level_groups({{level_1}}, {{level_2}}) ->
    {{{level_3}}}{{#is_last}}.{{/is_last}}{{^is_last}};{{/is_last}}
{{/same_level_groups}}