%%%-----------------------------------------------------------------------------
%%% @doc Common-use utility functions.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_utils).

%%% API
-export([group_by_key/1,
         pairs/1,
         shuffle/1,
         average_number/2,
         format/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Groupes listed tuples by the first element:
%%      [{K1, V1}, {K2, V2}, {K1, V3}, {K2, V4}] =>
%%      [{K1, [V1, V3]}, {K2, [V2, V4]}].
%% @end
%%------------------------------------------------------------------------------
group_by_key(L) ->
    Fold = fun({K, V}, D) -> dict:append(K, V, D) end,
    Dict = lists:foldl(Fold, dict:new(), L),
    dict:to_list(Dict).

%%------------------------------------------------------------------------------
%% @doc Splits list into pairs.
%% @end
%%------------------------------------------------------------------------------
pairs(L) -> pairs(L, []).

%%------------------------------------------------------------------------------
%% @doc Associates each element in the list with a random number. The list is
%%      then sorted based on the generated number. Repeats the process log(n)
%%      times to ensure a fair shuffle.
%% @end
%%------------------------------------------------------------------------------
shuffle([]) -> [];
shuffle(List) ->
   randomize(round(math:log(length(List)) + 0.5), List).

%%------------------------------------------------------------------------------
%% @doc Computes an average number of elements that are chosen with given
%%      probability.
%% @end
%%------------------------------------------------------------------------------
average_number(Probability, List) ->
    case Probability * length(List) of
        N when N == 0 ->
            0;
        N when N < 1 ->
            case rand:uniform() < N of
                true -> 1;
                false -> 0
            end;
        N when N >= 1 ->
            trunc(N)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns string that represents data formatted in accordance with
%%      pattern.
%% @end
%%------------------------------------------------------------------------------
format(Pattern, Data) ->
    lists:flatten(io_lib:format(Pattern, Data)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
pairs([], Acc) -> Acc;
pairs([A], Acc) -> [{A} | Acc];
pairs([A, B | L], Acc) -> pairs(L, [{A, B} | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
randomize(List) ->
   D = lists:map(fun(A) ->
                    {rand:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)),
   D1.
