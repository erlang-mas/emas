%%%-----------------------------------------------------------------------------
%%% @doc Common-use utility functions.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_utils).

%%% API
-export([average_number/2,
         format/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Computes an average number of elements that are chosen with given
%%      probability.
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
%%------------------------------------------------------------------------------
format(Pattern, Data) ->
    lists:flatten(io_lib:format(Pattern, Data)).
