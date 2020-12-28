-module(rally).
-export([rally/3, test/0]).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([debug_info]).

bfs([Front|Fronts], NewFronts, Step, Visited, A, B, T) ->
    case sets:is_element(Front, Visited) of 
        true -> bfs(Fronts, NewFronts, Step, Visited, A, B, T);
        false ->
            case expand(Front, NewFronts, A, B, T) of
                success -> Step+1;
                New -> bfs(Fronts, New, Step, sets:add_element(Front, Visited), A, B, T)
            end
    end;
bfs([], NewFronts, Step, Visited, A, B, T) ->
    bfs(NewFronts, [], Step+1, Visited, A, B, T).

expand({Unit, Speed}, Acc, A, B, T) when A >= -B -> 
    case addNode(Unit, Speed+A, T, Acc) of
            success -> success;
            NewAcc -> expand({Unit, Speed}, NewAcc, A-10, B, T)
    end;
expand(_, Acc, _, _, _) -> Acc.


addNode(_, NewSpeed, _, Acc) when NewSpeed =< 0 -> Acc;
addNode(Unit, NewSpeed, T, Acc) -> 
    begin
        Destination = Unit + (NewSpeed div 10),
        case is_valid_in_range(NewSpeed, Unit+1, Destination, T) of
            true -> 
                case Destination >= array:size(T) of
                    true -> success;
                    false -> [{Destination, NewSpeed}| Acc]
                end;
            false -> Acc
        end
    end.

is_valid_in_range(NewSpeed, Unit, Destination, T) ->
    case Unit >= array:size(T) of
        true -> true;
        false -> 
            case Unit > Destination of
                true -> true;
                false -> 
                    case NewSpeed =< array:get(Unit, T) of
                        true -> is_valid_in_range(NewSpeed, Unit+1, Destination, T);
                        false -> false
                    end
            end
    end.

to_array([{N, V}|Rest], Acc) ->
    case (N =:= 0 andalso V =:= 0) of
        true -> array:from_list(lists:reverse(Acc));
        false -> to_array(Rest, add_n(N, V, Acc))
    end.

add_n(N, V, Acc) -> 
    case N of
        0 -> Acc;
        _ -> add_n(N-1, V, [V|Acc])
    end.

rally(A, B, Track) -> 
    bfs([{-1, 0}], [], 0, sets:new(), A, B, to_array(Track, [])).

% UNIT TESTS

rally_test_() ->
    [testcase1(), testcase2(), testcase3()].

testcase1() ->
    [?_assertEqual(rally(30, 10, [{10,100},{5,70},{3,40},{6,100},{0,0}]), 5)].

testcase2() ->
    [?_assertEqual(rally(40, 50, [{15,100},{0,0}]), 3)].

testcase3() ->
    [?_assertEqual(rally(40, 20, [{1,50},{1,40},{1,30},{1,20},{1,10},{1,20},{1,30},{1,40},{1,50},{0,0}]), 5)].

% PROPERTY TESTS

integer_10to240() ->
    ?LET(I, range(1,24), 10*I).

generate_track() ->
    ?LET(Track, list({pos_integer(), integer_10to240()}), Track++[{0,0}]).

prop_upper_bound() ->
    ?FORALL([A, B, T], [integer_10to240(), integer_10to240(), generate_track()], rally(A, B, T) =< lists:sum([X || {X,_} <- T]) + 1).

prop_lower_bound() ->
    ?FORALL([A, B, T], [integer_10to240(), integer_10to240(), generate_track()], rally(A, B, T) >= lists:sum([X || {X,_} <- T])/24).

to_graph(A, B, T) ->
    Tr = to_array(T, []),
    L = length(array:to_list(Tr)) - 1,
    G = digraph:new(),
    digraph:add_vertex(G, {-1, 0}), % start
    digraph:add_vertex(G, {-2, 0}), % last
    digraph:add_vertex(G, {-3, 0}), % end
    digraph:add_edge(G, {-2, 0}, {-3, 0}),
    add_all_vertices(G, A, B, Tr, 0, L),
    add_all_edges(G, -1, L, A, B, Tr).

% Add all vertices to the graph with unit Curr (and all possible speeds)
add_all_vertices(G, A, B, T, Curr, Length) ->
    Aux = fun Aux(From, To) ->
        case From > To of
            true -> true;
            false -> 
                digraph:add_vertex(G, {Curr, From}),
                Aux(From+10, To)
        end
    end,
    case Curr > Length of
        true -> G;
        false ->
            case Curr =/= -1 of
                true -> Aux(10, array:get(Curr, T));
                false -> false
            end,
            add_all_vertices(G, A, B, T, Curr+1, Length)
    end.

% Add all edges to the graph that start from unit Curr
add_all_edges(G, Curr, Length, A, B, T) ->
    % Add edges starting on Curr unit with speed Speed and new speeds in range [From, To]
    Add_adjacent_edges = fun Add_adjacent_edges(Speed, From, To) ->
        case From > To of
            true -> true;
            false ->
                NewSpeed = Speed + From,
                NewIndex = Curr + NewSpeed div 10,
                case is_valid_in_range(NewSpeed, Curr+1, NewIndex, T) of
                    false -> true;
                    true -> 
                        case NewIndex > Length of
                            false -> 
                                case NewIndex =:= Length of 
                                    true ->  digraph:add_edge(G, {Curr, Speed}, {-2, 0});
                                    false -> digraph:add_edge(G, {Curr, Speed}, {NewIndex, NewSpeed})
                                end;
                            true -> digraph:add_edge(G, {Curr, Speed}, {-3, 0})
                        end,
                        Add_adjacent_edges(Speed, From+10, To)
                end
        end
    end,
    % Add edges starting starting on Curr with speeds [From, To]
    Walk = fun Walk(From, To) ->
        case From > To of
            true -> true;
            false ->
                Add_adjacent_edges(From, max(-B, -From+10), A),
                Walk(From+10, To)
        end
    end,
    case Curr > Length of
        true -> G;
        false ->
            case Curr =/= -1 of
                true -> Walk(10, array:get(Curr, T));
                false -> Add_adjacent_edges(0, 10, A)
            end,
            add_all_edges(G, Curr+1, Length, A, B, T)
    end.

get_num_steps_correct(A, B, T) ->
    G = to_graph(A, B, T),
    length(digraph:get_short_path(G, {-1, 0}, {-3,0})) - 1.


prop_correct_bfs() -> 
    ?FORALL([A, B, T], [integer_10to240(), integer_10to240(), generate_track()], rally(A, B, T) =:= get_num_steps_correct(A, B, T)).
