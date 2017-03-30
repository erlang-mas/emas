%%%-----------------------------------------------------------------------------
%%% @doc Genetic operators for Ackley function optimization problem.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_ackley_ops).

-include("emas.hrl").

-behaviour(emas_genetic_ops).

%%% API
-export ([solution/1,
          evaluation/2,
          recombination/3,
          mutation/2,
          config/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
solution(#sim_params{problem_size = Dim}) ->
    S = [-50 + rand:uniform() * 100 || _ <- lists:seq(1, Dim)],
    erlang:term_to_binary(S).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
evaluation(BS, _SP) ->
    S = erlang:binary_to_term(BS),
    A = 20,
    B = 0.2,
    C = 2 * math:pi(),
    D = length(S),
    Sum1 = lists:foldl(fun(X, Sum) -> Sum + X * X end, 0.0, S),
    Sum2 = lists:foldl(fun(X, Sum) -> Sum + math:cos(C * X) end, 0.0, S),
    Exp1 = math:exp(-B * math:sqrt(1 / D * Sum1)),
    Exp2 = math:exp(1 / D * Sum2),
    -(-A * Exp1 - Exp2 + A + math:exp(1)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
recombination(B1, B2, _SP) ->
    S1 = erlang:binary_to_term(B1),
    S2 = erlang:binary_to_term(B2),
    Features = lists:zip(S1, S2),
    {S3, S4} = lists:unzip([recombine(F1, F2) || {F1, F2} <- Features]),
    {erlang:term_to_binary(S3), erlang:term_to_binary(S4)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
mutation(B, SP = #sim_params{mutation_rate = MutationRate}) ->
    S = erlang:binary_to_term(B),
    MutationsCount = emas_utils:average_number(MutationRate, S),
    Indexes = [rand:uniform(length(S)) || _ <- lists:seq(1, MutationsCount)],
    Mutated = mutate(S, lists:usort(Indexes), 1, [], SP),
    erlang:term_to_binary(Mutated).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
config() -> none.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Chooses a random value between the two initial features.
%%------------------------------------------------------------------------------
recombine(F1, F2) ->
    A = erlang:min(F1, F2),
    B = (erlang:max(F1, F2) - erlang:min(F1, F2)),
    {A + rand:uniform() * B, A + rand:uniform() * B}.

%%------------------------------------------------------------------------------
%% @private
%% Iterates over subsequent solution features starting at the first one. Applies
%% mutation to features whose indexes match randomly selected mutation indexes
%% derived from the calling method. Other indexes are copied to the acumulator.
%%------------------------------------------------------------------------------
mutate(Solution, [], _Index, Acc, _SP) ->
    lists:reverse(Acc, Solution);
mutate([Gene|Solution], [I|Indexes], I, Acc, SP) ->
    mutate(Solution, Indexes, I+1, [mutate_feature(Gene, SP)|Acc], SP);
mutate([Gene|Solution], [I|Indexes], Index, Acc, SP) ->
    mutate(Solution, [I|Indexes], Index + 1, [Gene|Acc], SP).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
mutate_feature(F, #sim_params{mutation_range = MutationRange}) ->
    Range = MutationRange * mutation_variation(),
    F + Range * math:tan(math:pi() * (rand:uniform() - 0.5)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
mutation_variation() ->
    case rand:uniform() of
        X when X < 0.2 -> 5.0;
        X when X < 0.4 -> 0.2;
        _ -> 1.0
    end.
