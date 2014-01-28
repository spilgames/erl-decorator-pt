-module(decorator_pt_fun).
-export([parse_transform/2]).

%trick to include parse_transform, without transforming this particular module!
-define(NO_PARSE_TRANSFORM, true).
-include("decorator_pt.hrl").

%% @doc Start the parse transformation
%%      http://erlang.org/doc/man/erl_syntax.html
%%      http://www.erlang.org/doc/man/erl_parse.html
-spec parse_transform(erl_parse:abstract_form(), compile:option()) -> erl_parse:abstract_form().
%% @end
parse_transform(Forms, _Options) ->
    [parse_form(F) || F <- Forms].

%% @doc Parse individual forms. Now only interested in 'function'
%%      Applies a function to each node in syntax tree, replacing the original
%%      http://erlang.org/doc/man/erl_syntax_lib.html#map-2
%% @spec parse_form( erl_syntax:syntaxTree() ) -> erl_syntax:syntaxTree()
%% @end
parse_form({function, _, FName, FArity, _} = T) ->
    erl_syntax_lib:map(fun(TE) -> parse_macro(FName, FArity, TE) end, T);
parse_form(T) ->
    T.

%% @doc Rewrites the placeholders ?FUNCTION and ?ARITY to the current
%%      function name and arity.
%%
%% @spec parse_macro(any(), any(), erl_syntax:syntaxTree() ) ->
%%          erl_syntax:syntaxTree()
%% @end
parse_macro(FName, FArity, T) ->
    erl_syntax:revert(
        case erl_syntax:type(T) of
            atom ->
                case erl_syntax:atom_value(T) of
                    ?FUNCTION   -> erl_syntax:atom(FName);
                    ?ARITY      -> erl_syntax:integer(FArity);
                    _ -> T
                end;
            _ ->
                T
        end
    ).
