%%%-----------------------------------------------------------------------------
%%% @doc EMAS runner script.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas).

-include("emas.hrl").

%%% API
-export([main/1, start/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

main([]) ->
    usage();
main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Opts, _NonOptArgs}} ->
            start(Opts);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            usage()
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(Opts) ->
    setup_distribution(),
    application:load(mas),
    application:load(emas),
    application:set_env(mas, population_mod, emas_population),
    setup_app_env(mas, Opts),
    setup_app_env(emas, Opts),
    SP = emas_config:fetch_all(),
    Time = get_opt(time, Opts),
    mas:start(),
    mas_logger:info("EMAS configuration: ~p", [emas_config:get_all()]),
    mas:start_simulation(SP, Time),
    handle_result().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_distribution() ->
    {ok, _} = net_kernel:start([generate_node_name(), shortnames]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_node_name() ->
    Name = emas_utils:format("emas-~3..0B", [rand:uniform(100)]),
    list_to_atom(Name).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_app_env(App, Opts) ->
    Props = [element(1, Spec) || Spec <- option_spec_list(App)],
    [set_app_prop(App, proplists:lookup(Prop, Opts)) || Prop <- Props].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_app_prop(App, {Key, Value}) ->
    application:set_env(App, Key, Value);
set_app_prop(_App, none) -> none.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_opt(Key, Opts) ->
    proplists:get_value(Key, Opts).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_result() ->
    receive
        {results, Result} ->
            io:format("Simulation result: ~p~n", [Result]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]);
        _ -> io:format("Unknown simulation result~n", [])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
usage() ->
    getopt:usage(option_spec_list(), escript:script_name()).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
option_spec_list() ->
    Spec = option_spec_list(mas) ++ option_spec_list(emas),
    lists:usort(Spec).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
option_spec_list(mas) ->
    [
     {population_count,
      $i,
      "population-count",
      integer,
      "Number of populations (islands)"},

     {population_size,
      undefined,
      "population-size",
      integer,
      "Size of single population"},

     {node_migration_probability,
      undefined,
      "node-migration-probability",
      float,
      "Node migration probability"},

     {topology,
      $t,
      "topology",
      atom,
      "Topology of connections between populations"},

     {nodes_topology,
      undefined,
      "nodes-topology",
      atom,
      "Topology of connections between nodes"},

     {logs_dir,
      $o,
      "output",
      string,
      "Logs output directory"},

     {debug,
      undefined,
      "debug",
      boolean,
      "Enables debug mode"}
    ];
option_spec_list(emas) ->
    [
     {time,
      $t,
      "time",
      integer,
      "Duration of the simulation"},

     {problem_size,
      $s,
      "problem-size",
      integer,
      "Problem size"},

     {genetic_ops,
      undefined,
      "genetic-ops",
      atom,
      "Genetic operators module"},

     {reproduction_threshold,
      undefined,
      "reproduction-threshold",
      integer,
      "Reproduction threshold"},

     {migration_threshold,
      undefined,
      "migration-threshold",
      integer,
      "Migration threshold"},

     {reproduction_transfer,
      undefined,
      "reproduction-transfer",
      integer,
      "Reproduction transfer"},

     {fight_transfer,
      undefined,
      "fight-transfer",
      integer,
      "Fight transfer"},

     {migration_probability,
      undefined,
      "migration-probability",
      float,
      "Migration probability"}
    ].
