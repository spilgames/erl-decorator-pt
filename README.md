<a href="https://travis-ci.org/alertlogic/erl-decorator-pt" target="_blank"><img src="https://travis-ci.org/alertlogic/erl-decorator-pt.svg?branch=master"/></a>

# erl-decorator-pt

Transformations in Erlang syntax allowing cache decorator directives above functions to cache them
using [erl-cache](https://github.com/alertlogic/erl-cache).

This is a fork of [spilgames/erl-decorator-pt](https://github.com/spilgames/erl-decorator-pt) for [Alert Logic](https://github.com/alertlogic).

Purpose
=======

This application implements a set of transformations in the erlang syntax tree allowing the
following:

* The *decorator* directive on top of functions in order to add extra functionality to them

* The *?FUNCTION* and *?ARITY* macros, returning an atom with the function name corresponding to the
  current scope and the amount of arguments that function was invoked with. These macros can be used
  in your decorators.

Usage
=====

There are many ways to include these macros in your projects. Just remember macros and macro
debugging in particular are not the simplest tools in the world. _A great power comes with a great
responsibility._

The most common way of using this application is:

1. Make it available to your application via rebar

2. Create an .hrl file loading hooking the decorator_pt_core in the compilation process and defining
   a macro with a representative name for your decorator. i.e. my_decorator.hrl

```
-compile([{parse_transform, decorator_pt_core}]).

-define(MY_DECORATOR(Options), -decorate({MyCbModule, MyCbFun, {?MODULE, ?FUNCTION, Options}})).

```

3. Define the callback function in the callback module to your liking. Keep in mind what the
   signature of your callback should be:

```
my_cb_fun(OriginalFun::function(), Args::[tern()],
          {OriginalModule::atom(), OriginalFunctionName::atom(), Opts::[term()]) ->
    FunReturningResult::fun(() -> term()).
```

4. When using your decorator, include *my_decorator.hrl* and *decorator_pt.hrl* in that order.

```
-include_lib("my_app/include/my_decorator.hrl").
-include_lib("decorator_pt/include/decorator_pt.hrl").

[...]

?MY_DECORATOR([{opt1, val}]).
my_decorated_fun() ->
    ok.

```
