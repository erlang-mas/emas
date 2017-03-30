%%%-----------------------------------------------------------------------------
%%% @doc Genetic operators for Rastrigin function optimization problem.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_rastrigin_ops).

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
%% @doc Generates random solution, as a vector of numbers in range [-50, 50].
%% @end
%%------------------------------------------------------------------------------
solution(#sim_params{problem_size = Dim}) ->
    S = [-50 + rand:uniform() * 100 || _ <- lists:seq(1, Dim)],
    erlang:term_to_binary(S).

%%------------------------------------------------------------------------------
%% @doc Evaluates solution by computing the Rastrigin function.
%% @end
%%------------------------------------------------------------------------------
evaluation(B, _SP) ->
    S = erlang:binary_to_term(B),
    - lists:foldl(fun(X, Sum) ->
                      Sum + 10 + X * X - 10 * math:cos(2 * math:pi() * X)
                  end, 0.0, S).

%%------------------------------------------------------------------------------
%% @doc Continuously recombines every pair of features for given pair of
%%      solutions.
%% @end
%%------------------------------------------------------------------------------
recombination(B1, B2, _SP) ->
    S1 = erlang:binary_to_term(B1),
    S2 = erlang:binary_to_term(B2),
    Features = lists:zip(S1, S2),
    {S3, S4} = lists:unzip([recombine(F1, F2) || {F1, F2} <- Features]),
    {erlang:term_to_binary(S3), erlang:term_to_binary(S4)}.

%%------------------------------------------------------------------------------
%% @doc Mutates features at random indices.
%% @end
%%------------------------------------------------------------------------------
mutation(B, SP = #sim_params{mutation_rate = MutationRate}) ->
    S = erlang:binary_to_term(B),
    MutationsCount = emas_utils:average_number(MutationRate, S),
    Indexes = [rand:uniform(length(S)) || _ <- lists:seq(1, MutationsCount)],
    Mutated = mutate(S, lists:usort(Indexes), 1, [], SP),
    erlang:term_to_binary(Mutated).

%%------------------------------------------------------------------------------
%% @doc Operators config.
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
