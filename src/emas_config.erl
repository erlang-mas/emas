%%%-----------------------------------------------------------------------------
%%% @doc Configuration utilities.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_config).

-include("emas.hrl").

%% API
-export([get_env/1,
         get_env/2,
         fetch_all/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Retrieves value from application environment, crashes if value is not
%% set.
%% @end
%%------------------------------------------------------------------------------
get_env(Key) ->
    element(2, {ok, _} = application:get_env(emas, Key)).

%%------------------------------------------------------------------------------
%% @doc Retrieves value from application environment, provides default if value
%% is not set.
%% @end
%%------------------------------------------------------------------------------
get_env(Key, Default) ->
    case application:get_env(emas, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

%%------------------------------------------------------------------------------
%% @doc Builds complete simulation params record.
%% @end
%%------------------------------------------------------------------------------
fetch_all() ->
    #sim_params{
        genetic_ops             = get_env(genetic_ops),
        problem_size            = get_env(problem_size),
        initial_energy          = get_env(initial_energy),
        reproduction_threshold  = get_env(reproduction_threshold),
        migration_threshold     = get_env(migration_threshold),
        reproduction_transfer   = get_env(reproduction_transfer),
        fight_transfer          = get_env(fight_transfer),
        mutation_rate           = get_env(mutation_rate),
        mutation_range          = get_env(mutation_range),
        mutation_chance         = get_env(mutation_chance),
        recombination_chance    = get_env(recombination_chance),
        fight_number            = get_env(fight_number)
    }.
