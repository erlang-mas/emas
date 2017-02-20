%%%-----------------------------------------------------------------------------
%%% @doc EMAS population behaviour.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_population).

-include_lib("emas.hrl").

-behaviour(mas_population).

%%% MAS population callbacks
-export([sim_params/0,
         initial_agent/1,
         behaviour/2,
         behaviours/0,
         meeting/2]).

%%%=============================================================================
%%% MAS population callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sim_params() ->
    emas_config:fetch_all().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
initial_agent(SP) ->
    S = emas_genetic:solution(SP),
    {S, emas_genetic:evaluation(S, SP), SP#sim_params.initial_energy}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviour({_, _, 0}, _SP) ->
    death;
behaviour({_, _, Energy}, #sim_params{reproduction_threshold = RT}) ->
    case Energy > RT of
        true  -> reproduction;
        false -> fight
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviours() ->
    [reproduction, fight, death].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
meeting({death, _}, _SP) ->
    [];
meeting({reproduction, Agents}, SP) ->
    lists:flatmap(fun(Pair) ->
                      emas_evolution:do_reproduce(Pair, SP)
                  end, emas_evolution:optional_pairs(Agents, []));
meeting({fight, Agents}, SP) ->
  lists:flatmap(fun(Pair) ->
                    emas_evolution:do_fight(Pair, SP)
                end, emas_evolution:optional_pairs(Agents, [])).
