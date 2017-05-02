%%%-----------------------------------------------------------------------------
%%% @doc EMAS population behaviour.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_population).

-include("emas.hrl").

-behaviour(mas_population).

%%% MAS population callbacks
-export([init_agent/1,
         init/2,
         step/2,
         measure/2,
         terminate/2]).

-record(state, {sim_params          :: sim_params(),
                initial_energy      :: integer(),
                behaviours_counter  :: mas:counter()}).

%%%=============================================================================
%%% MAS population callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init_agent(SP = #sim_params{initial_energy = InitialEnergy}) ->
    S = emas_genetic:solution(SP),
    {S, emas_genetic:evaluation(S, SP), InitialEnergy}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Agents, SP) ->
    InitialEnergy = total_energy(Agents),
    BehavioursCounter = mas_counter:new([fight, reproduction, death]),
    {Agents, #state{sim_params = SP,
                    initial_energy = InitialEnergy,
                    behaviours_counter = BehavioursCounter}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
step(Agents, State = #state{behaviours_counter = Counter}) ->
    TaggedAgents = determine_behaviours(Agents, State),
    Arenas = form_arenas(TaggedAgents),
    ProcessedArenas = process_arenas(Arenas, State),
    NewAgents = extract_agents(ProcessedArenas),
    Emigrants = fetch_arena(migration, Arenas),
    BehaviourCounts = count_behaviours(Arenas),
    NewCounter = mas_counter:update(BehaviourCounts, Counter),
    {NewAgents, Emigrants, State#state{behaviours_counter = NewCounter}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
measure(Agents, State) ->
    #state{behaviours_counter = Counter} = State,
    Metrics = [{total_energy, total_energy(Agents)},
               {best_fitness, best_fitness(Agents)} | dict:to_list(Counter)],
    NewCounter = mas_counter:reset(Counter),
    {Metrics, State#state{behaviours_counter = NewCounter}}.

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
total_energy([]) -> 0;
total_energy(Agents) ->
    lists:foldl(fun ({_, _, E}, Acc) -> Acc + E end, 0, Agents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
best_fitness([]) -> unknown;
best_fitness(Agents) ->
    lists:max([F || {_, F, _} <- Agents]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
determine_behaviours(Agents, State = #state{sim_params = SP}) ->
    MP = migration_probability(Agents, State),
    [{behaviour(Agent, SP, MP), Agent} || Agent <- Agents].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviour({_, _, 0}, _SP, _MP) ->
    death;
behaviour({_, _, Energy}, SP, MP) ->
    #sim_params{reproduction_threshold = RT,
                migration_threshold = MT} = SP,
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
form_arenas(TaggedAgents) ->
    emas_utils:group_by_key(TaggedAgents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_arenas(Arenas, #state{sim_params = SP}) ->
    [apply_meetings(Arena, SP) || Arena <- Arenas].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
apply_meetings({fight, Agents}, SP) ->
    lists:flatmap(fun(Pair) -> emas_evolution:do_fight(Pair, SP) end,
                  emas_utils:pairs(Agents));
apply_meetings({reproduction, Agents}, SP) ->
    lists:flatmap(fun(Pair) -> emas_evolution:do_reproduce(Pair, SP) end,
                  emas_utils:pairs(Agents));
apply_meetings({death, _Agents}, _SP) ->
    [];
apply_meetings({migration, _Agents}, _SP) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
extract_agents(Arenas) ->
    emas_utils:shuffle(lists:flatten(Arenas)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
fetch_arena(Behaviour, Arenas) ->
    case lists:keyfind(Behaviour, 1, Arenas) of
        {_, Agents} -> Agents;
        false -> []
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
migration_probability(Agents, State) ->
    #state{initial_energy = InitialEnergy,
           sim_params = SP} = State,
    #sim_params{migration_probability = MP} = SP,
    CurrentEnergy = total_energy(Agents),
    case CurrentEnergy / InitialEnergy of
        E when E < 0.8 -> 0.0;
        E when E < 0.9 -> 0.5 * MP;
        E when E < 1.1 -> MP;
        E when E < 1.2 -> 1.5 * MP;
        E when E >= 1.2 -> 2 * MP
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
count_behaviours(Arenas) ->
    [{Behaviour, length(Agents)} || {Behaviour, Agents} <- Arenas].
