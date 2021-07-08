-module(reverse_hash).
-export([solve/4]).

solve(Fun, Inputs, P, Schedulers) ->
    Hi = 134217727,
    Threads = Schedulers,
    Step = Hi div Threads,
    Boundaries = lists:seq(1, Hi-100, Step) ++ [Hi],
    Subproblems = lists:zip(lists:droplast(Boundaries), tl(Boundaries)),
    Parent = self(),
    InputsMap = maps:from_list([{X, true} || X <- Inputs]),
    Children = lists:map(fun(X) -> 
        spawn(fun() -> 
            solve_subproblem(Fun, InputsMap, X, [], Parent) end)
        end,
        Subproblems), receive
        finish_up -> timer:sleep(800), lists:map(fun(X) -> X ! finish_up end, Children), loop(0, Threads, [], P, Children)
    end.

loop(Cnt, S, Acc, P, _) when Cnt =:= S -> P ! {reply, Acc};
loop(Cnt, S, Acc, P, Children) ->
    receive
        Res -> loop(Cnt+1, S, Res ++ Acc, P, Children)
    end.

solve_subproblem(_, _, {Lo, Hi}, Acc, P) when Lo =:= Hi -> P ! Acc;
solve_subproblem(Fun, Inputs, {Lo, Hi}, Acc, P) -> 
    receive
        finish_up -> P ! Acc
    after 0 ->
        Hash = Fun(Lo),
	case maps:is_key(Hash, Inputs) of
            true -> NewAcc = [{Hash, Lo} | Acc];
            false -> NewAcc = Acc
        end,
        solve_subproblem(Fun, Inputs, {Lo+1, Hi}, NewAcc, P)
    end.
