-module(decorator_pt_eunit).

-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, decorator_pt_core}]).

-export([demo_one/1, demo_two/1, my_decorator/3]).


my_decorator_test_() ->
    [
        ?_assertEqual(decorated,    demo_one(undecorated)),
        ?_assertEqual(undecorated,  demo_two(undecorated))
    ].

my_decorator(Fun, Input, _Opts) ->
    fun() -> erlang:apply(Fun, [[decorated]]) end.

-decorate({?MODULE, my_decorator, []}).
demo_one(Input) ->
    Input.

demo_two(Input) ->
    Input.
