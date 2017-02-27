%%%-----------------------------------------------------------------------------
%%% @doc EMAS simulation behaviour.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_simulation).

-include_lib("emas.hrl").

-behaviour(mas_simulation).

%%% MAS simulation callbacks
-export([simulation_setup/1,
         simulation_teardown/1,
         simulation_result/2]).

%%%=============================================================================
%%% MAS simulation callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_setup(SP) ->
    setup_exometer(SP),
    subscribe_metrics(SP).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_teardown(_SP) ->
    unsubscribe_metrics().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_result(_SP, Agents) ->
    extract_best_solution(Agents).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_exometer(SP) ->
    mas_reporter:setup(SP#sim_params.logs_dir),
    exometer_admin:set_default(['_'], emas_fitness_entry_nif,
                              [{module, emas_fitness_entry_nif}]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
subscribe_metrics(SP) ->
    Metric = [global, fitness],
    exometer:new(Metric, emas_fitness_entry_nif, []),
    exometer_report:subscribe(exometer_report_fs, Metric, fitness,
                              SP#sim_params.write_interval).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unsubscribe_metrics() ->
    Metric = [global, fitness],
    exometer_report:unsubscribe_all(exometer_report_fs, Metric).
    % exometer:delete(Metric).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
extract_best_solution(Agents) ->
   ArgMax = fun (A = {_, F, _}, {_, AccF, _}) when F > AccF ->
                    A;
                (_, Acc) ->
                    Acc
            end,
   {_Sol, _Fit, _Energy} = lists:foldl(ArgMax, hd(Agents), tl(Agents)).
