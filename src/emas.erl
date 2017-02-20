%%%-----------------------------------------------------------------------------
%%% @doc Starts MAS simulation, gathers results and extracts best solution.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas).

-include_lib("emas.hrl").

%%% API functions
-export([start/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start(Time) ->
    application:ensure_all_started(emas),
    SP = emas_config:fetch_all(),
    setup_exometer(SP),
    subscribe_metrics(SP),
    application:start(mas),
    timer:sleep(Time),
    {agents, Agents} = mas:get_results(),
    application:stop(mas),
    unsubscribe_metrics(),
    extract_best_solution(Agents).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_exometer(SP) ->
    exometer_report:add_reporter(exometer_report_fs,
                                 [{base_dir, SP#sim_params.logs_dir}]),
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
