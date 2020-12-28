-module(bughunt).
-export([test/1]).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-type vector()    :: [integer(),...].
-type expr()      :: vector()
                   | {vector_op(),     expr(), expr()}
                   | {scalar_op(), int_expr(), expr()}.
-type int_expr()  :: integer()
                   | {norm_op(), expr()}.
-type vector_op() :: 'add' | 'sub' | 'dot'.
-type scalar_op() :: 'mul' | 'div'.
-type norm_op()   :: 'norm_one' | 'norm_inf'.


-spec evaluate(expr()) -> vector() | 'error'.
evaluate(E) ->
    try evaluate_(E, 1) of
        V -> V
    catch
        _ -> 'error'
    end.

% evaluate_/1 may throw an error which must be caught by evaluate/1
-spec evaluate_(expr(), integer()) -> vector().
evaluate_(X, Level) -> 
    Evaluated = case X of 
        {Op, E1, E2} -> 
            case Op of
                'add' -> v_add(evaluate_(E1, Level+1), evaluate_(E2, Level+1));
                'sub' -> v_sub(evaluate_(E1, Level+1), evaluate_(E2, Level+1));
                'dot' -> v_dot(evaluate_(E1, Level+1), evaluate_(E2, Level+1));
                'mul' -> v_mul(int_evaluate_(E1, Level+1), evaluate_(E2, Level+1)); 
                'div' -> v_div(int_evaluate_(E1, Level+1), evaluate_(E2, Level+1))
            end;
        _ -> X
    end,
    case length(Evaluated) < 1 orelse length(Evaluated) > 100 orelse Level > 100 of
        true -> throw(error);
        false -> Evaluated
    end.

-spec int_evaluate_(int_expr(), integer()) -> integer().
int_evaluate_({Op, E}, Level) ->
    Evaluated = case Op of
        'norm_one' -> norm_one(evaluate_(E, Level + 1));
        'norm_inf' -> norm_inf(evaluate_(E, Level + 1))
    end,
    case Level > 100 of
        true -> throw(error);
        false -> Evaluated
    end;
int_evaluate_(X, _) -> X.

-spec v_add(vector(), vector()) -> vector().
v_add(V1, V2) when length(V1) =/= length(V2) -> throw(error);
v_add(V1, V2) -> [X1 + X2 || {X1, X2} <- lists:zip(V1, V2)].

-spec v_sub(vector(), vector()) -> vector().
v_sub(V1, V2) when length(V1) =/= length(V2) -> throw(error);
v_sub(V1, V2) -> [X1 - X2 || {X1, X2} <- lists:zip(V1, V2)].

-spec v_dot(vector(), vector()) -> vector().
v_dot(V1, V2) when length(V1) =/= length(V2) -> throw(error);
v_dot(V1, V2) -> [X1 * X2 || {X1, X2} <- lists:zip(V1, V2)].

-spec norm_one(vector()) -> integer().
norm_one(V) -> lists:sum(lists:map(fun abs/1, V)).

-spec norm_inf(vector()) -> integer().
norm_inf(V) -> lists:max(lists:map(fun abs/1, V)).

-spec v_mul(integer(), vector()) -> vector().
v_mul(I, V) -> [X * I || X <- V].

-spec v_div(integer(), vector()) -> vector().
v_div(I, _) when I =:= 0 -> throw(error);
v_div(I, V) -> [X div I || X <- V].

% PROPERTIES

call_vector(X, Input) ->
    try
        (vectors:vector(X))(Input)
    catch
        _:_ -> 'crash'
    end.

% expr(0) -> vector(3, integer());
% expr(Levels) ->
%     union([expr1(Levels), expr2(Levels)]).

% expr1(Levels) ->
%     ?LET(Op, union(['add', 'sub', 'dot']), {Op, expr(Levels-1), expr(Levels-1)}).

% expr2(Levels) -> 
%     ?LET(ScalarOp, union(['mul', 'div']), {ScalarOp, int_expr(Levels-1), expr(Levels-1)}).

% int_expr(0) -> integer();
% int_expr(Levels) -> 
%     ?LET(Norm, union(['norm_one', 'norm_inf']), {Norm, expr(Levels-1)}).

expr(0) -> vector(3, integer());
expr(Levels) ->
    union([expr1(Levels), expr2(Levels)]).

expr1(Levels) ->
    ?LET(Op, union(['add', 'sub', 'dot']), {Op, expr(0), expr(Levels-1)}).

expr2(Levels) -> 
    ?LET(ScalarOp, union(['mul', 'div']), {ScalarOp, int_expr(Levels-1), expr(0)}).

int_expr(0) -> integer();
int_expr(Levels) -> 
    ?LET(Norm, union(['norm_one', 'norm_inf']), {Norm, expr(Levels-1)}).

vec_op(Op, SameSize) ->
    case SameSize of
        true -> ?LET(I, range(0, 200), {Op, vector(I, integer()), vector(I, integer())});
        false -> ?LET({I, J}, {range(0, 200), range(0, 200)}, {Op, vector(I, integer()), vector(J, integer())})
    end.



vec(Hi, Lo) ->
    ?LET(I, range(Hi, Lo), vector(I, integer())).

all_props(X) ->
    [
        {?FORALL(Input, vec(100, 200), call_vector(X, Input) =:= evaluate(Input)), "Vector with >100 elements"},
        {?FORALL(Input, [], call_vector(X, Input) =:= evaluate(Input)), "Vector with 0 elements"},
        
        {?FORALL(Input, vec_op('add', true), call_vector(X, Input) =:= evaluate(Input)), "Addition of vectors"},
        {?FORALL(Input, vec_op('sub', true), call_vector(X, Input) =:= evaluate(Input)), "Subtraction of vectors"},
        {?FORALL(Input, vec_op('dot', true), call_vector(X, Input) =:= evaluate(Input)), "Dot product of vectors"},
        
        {?FORALL(Input, vec_op('add', false), call_vector(X, Input) =:= evaluate(Input)), "Addition of not equally sized vectors"},
        {?FORALL(Input, vec_op('sub', false), call_vector(X, Input) =:= evaluate(Input)), "Subtraction of not equally sized vectors"},
        {?FORALL(Input, vec_op('dot', false), call_vector(X, Input) =:= evaluate(Input)), "Dot product of not equally sized vectors"},
        
        {?FORALL(Input, {'mul', integer(), vec(0, 200)}, call_vector(X, Input) =:= evaluate(Input)), "Multiplication"},
        {?FORALL(Input, {'div', 0, vec(1, 100)}, call_vector(X, Input) =:= evaluate(Input)), "Division by zero"},
        {?FORALL(Input, {'div', integer(), vec(0, 200)}, call_vector(X, Input) =:= evaluate(Input)), "Division"},

        {?FORALL(Input, {'mul', {'norm_one', vec(0, 200)}, vec(0, 200)}, call_vector(X, Input) =:= evaluate(Input)), "Norm one"},
        {?FORALL(Input, {'mul', {'norm_inf', vec(0, 200)}, vec(0, 200)}, call_vector(X, Input) =:= evaluate(Input)), "Norm inf"},
        
        {?FORALL(Input, expr(100), call_vector(X, Input) =:= evaluate(Input)), "Nested expression with >100 levels"},
        {?FORALL(Input, ?LET(Levels, range(1, 10), expr(Levels)), call_vector(X, Input) =:= evaluate(Input)), "Nested expression"}
    ].

% test(_, []) ->
%     'correct';
% ORIGINAL
% test(X, [{Prop, Comment}|Props]) -> 
%     Y = proper:quickcheck(Prop, [quiet]),
%     case Y of 
%         false -> Input = lists:last(proper:counterexample()), {Input, evaluate(Input), call_vector(X, Input), Comment};
%         true -> test(X, Props)
%     end.
%TODO: JUST FOR TESTING
test(_, []) ->
    io:format("correct~n");
test(X, [{Prop, Comment}|Props]) -> 
    Y = proper:quickcheck(Prop, [quiet, {numtests, 1000}]),
    case Y of 
        false -> Comment;
        true -> test(X, Props)
    end.

test(X) ->
    test(X, all_props(X)).

test_all() ->
    lists:foreach(fun(X) -> io:format("Evaluator ~p: ~p~n", [X, test(X)]) end, lists:seq(1, 50)).

% test_all() ->
%     lists:foreach(fun(X) -> test(X) end, lists:seq(1, 50)).
