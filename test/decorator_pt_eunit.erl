-module(decorator_pt_eunit).

-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, decorator_pt_core}]).

-export([demo_one/1,
    demo_two/1,
    my_decorator/3,
    my_decorator_adder/3]).


my_decorator_test_() ->
    [
        ?_assertEqual(decorated,    demo_one(undecorated)),
        ?_assertEqual(undecorated,  demo_two(undecorated)),
        ?_assertEqual(1,         demo_three(0)),    % 0+2 < 3; so 2-1
        ?_assertEqual(3,         demo_three(1))     % 1+2 not < 3; so 3
    ].

my_decorator(Fun, Input, _Opts) ->
    fun() -> erlang:apply(Fun, [[decorated]]) end.

my_decorator_adder(Fun, [Input], _Opts) ->
    fun() -> erlang:apply(Fun, [[Input+2]]) end.

-decorate({?MODULE, my_decorator, []}).
demo_one(Input) ->
    Input.

demo_two(Input) ->
    Input.

-decorate({?MODULE, my_decorator_adder, []}).
demo_three(Input) when Input < 3 ->
    Input-1;
demo_three(Input) ->
    Input.
