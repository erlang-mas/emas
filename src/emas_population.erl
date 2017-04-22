%%%-----------------------------------------------------------------------------
%%% @doc EMAS population behaviour.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_population).

-include("emas.hrl").

-behaviour(mas_population).

%%% API
-export([]).

%%% MAS population callbacks
-export([init/2,
         initial_agent/1,
         behaviours/0,
         behaviour/2,
         apply_behaviour/3,
         preprocess/2,
         postprocess/2,
         metrics/2,
         terminate/2]).

-record(state, {initial_energy  :: integer(),
                sim_params      :: sim_params()}).

%%%=============================================================================
%%% MAS population callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Agents, SP) ->
    InitialEnergy = total_energy(Agents),
    {Agents, #state{initial_energy = InitialEnergy,
                    sim_params = SP}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
initial_agent(SP = #sim_params{initial_energy = InitialEnergy}) ->
    S = emas_genetic:solution(SP),
    {S, emas_genetic:evaluation(S, SP), InitialEnergy}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviours() ->
    [death, fight, reproduction].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviour({_, _, 0}, _State) ->
    death;
behaviour({_, _, Energy}, #state{sim_params = SP}) ->
    #sim_params{reproduction_threshold = RT,
                migration_threshold = MT,
                migration_probability = MP} = SP,
    case Energy of
        E when E < RT -> fight;
        E when E < MT -> reproduction;
        E when E >= MT ->
            case rand:uniform() < MP of
                true -> migration;
                false -> reproduction
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
apply_behaviour(death, _Agents, _State) ->
    [];
apply_behaviour(fight, Agents, #state{sim_params = SP}) ->
    lists:flatmap(fun(Pair) ->
                      emas_evolution:do_fight(Pair, SP)
                  end, emas_utils:pairs(Agents));
apply_behaviour(reproduction, Agents,  #state{sim_params = SP}) ->
    lists:flatmap(fun(Pair) ->
                      emas_evolution:do_reproduce(Pair, SP)
    end, emas_utils:pairs(Agents)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
preprocess(Agents, State) ->
    #state{initial_energy = InitialEnergy, sim_params = SP} = State,
    CurrentEnergy = total_energy(Agents),
    MP = migration_probability(CurrentEnergy, InitialEnergy),
    NewSP = SP#sim_params{migration_probability = MP},
    {Agents, State#state{sim_params = NewSP}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
postprocess(Agents, State) ->
    {Agents, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
metrics(Agents, _State) ->
    [{total_energy, total_energy(Agents)},
     {best_fitness, best_fitness(Agents)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Agents, _State) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
migration_probability(CurrentEnergy, InitialEnergy) ->
    case CurrentEnergy / InitialEnergy of
        E when E < 0.8 -> 0.0;
        E when E < 1.2 -> 0.01;
        E when E >= 1.2 -> 0.1
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
total_energy(Agents) ->
    lists:foldl(fun ({_, _, E}, Acc) -> Acc + E end, 0, Agents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
best_fitness(Agents) ->
    lists:max([F || {_, F, _} <- Agents]).
