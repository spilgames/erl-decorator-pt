-ifndef(__PT_TRANSFORM_HRL).
-define(__PT_TRANSFORM_HRL, 1).

%% When used in a file where more than 1 parse transform is being executed, this macro will not
%% properly signal the original function name but the one assigned to it in the previous
%% transformation
-define(FUNCTION, '__function_macro__').

-define(ARITY, '__function_arity__').

-ifdef(NO_PARSE_TRANSFORM).
-else.
-compile({parse_transform, decorator_pt_fun}).
-endif.

-endif. % __PT_TRANSFORM_HRL
