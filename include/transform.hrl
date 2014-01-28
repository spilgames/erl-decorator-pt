-ifndef(__PT_TRANSFORM_HRL).
-define(__PT_TRANSFORM_HRL, 1).
-define(FUNCTION, '__function_macro__').
-define(ARITY, '__function_arity__').

-ifdef(NO_PARSE_TRANSFORM).
-else.
-compile({parse_transform, pt_transform}).
-endif.

-endif. % __PT_TRANSFORM_HRL
