%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczego agenta.

-module(agent).
-export([start/4]).
-record(arenas,{fight,reproduction,migration}).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec start(RingPid,BarPid,PortPid) -> ok
%% @doc Funkcja generujaca dane i startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac.
start(ProblemSize,Ring,Bar,Port) when is_integer(ProblemSize) ->
  random:seed(erlang:now()),
  S = genetic:solution(ProblemSize),
  Agent = {S,genetic:evaluation(S),config:initialEnergy()},
  Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
  loop(Agent,Arenas);

%% @spec start(Agent,RingPid,BarPid,PortPid) -> ok
%% @doc Funkcja startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac oraz dane agenta.
start(Agent,Ring,Bar,Port)  when is_tuple(Agent)  ->
  random:seed(erlang:now()),
  Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
  loop(Agent,Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec loop(Agent,Arenas) -> ok
%% @doc Funkcja cyklu zycia agenta. Jego zachowanie jest zalezne od jego
%% energii. Rekurencja kreci sie w nieskonczonosc, poki energia nie osiagnie 0.
loop(Agent,Arenas) ->
  case misc_util:behavior(Agent) of
    death ->
      exit(dying);
    reproduction ->
      {Solution,Fitness,_} = Agent,
      NewEnergy = arenas:call(Agent,Arenas#arenas.reproduction),
      loop({Solution,Fitness,NewEnergy},Arenas);
    fight ->
      {Solution,Fitness,_} = Agent,
      NewEnergy = arenas:call(Agent,Arenas#arenas.fight),
      loop({Solution,Fitness,NewEnergy},Arenas);
    migration ->
      [Ring,Bar,Port] = port:call(Arenas#arenas.migration), %arenas:call(emigration,Arenas#arenas.migration),
      loop(Agent,#arenas{fight = Ring, reproduction = Bar, migration = Port})
  end.