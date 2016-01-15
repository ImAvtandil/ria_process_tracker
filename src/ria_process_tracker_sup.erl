-module(ria_process_tracker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([
  make_specs/2,
  start_child/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Options) ->
  supervisor:start_child(example_sup, [Options]).

init([example])->
  Children =
    [
      {   undefined,                               % Id       = internal id
        {example, start_link, []},               % StartFun = {M, F, A}
        temporary,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
      }
    ],
  {ok, {{simple_one_for_one, 1000, 1000}, Children}};

init([]) ->
  Childs = make_specs(application:get_env(ria_process_tracker, trackers_list), []),
  io:format("~p", [Childs]),
  Example = [
    {example_sup,
      {supervisor, start_link, [{local, example_sup}, ?MODULE, [example]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],
  Procs = Childs++Example,
  {ok, {{one_for_one, 1, 5}, Procs}}.


make_specs(undefined, SpecList) ->
  SpecList;

make_specs({ok,TrackersList}, SpecList) ->
  make_specs(TrackersList, SpecList);

make_specs([Params | Tail], SpecList) ->
  {Name, Module, Function} = Params,
  NameList = atom_to_list(Name),
  FullNameList = NameList ++ "_tracker",
  FullName = list_to_atom(FullNameList),
  Spec = {FullName,
    {process_tracker, start_link, [{FullName, Module, Function}]},
    permanent,
    infinity,
    worker,
    [process_tracker]
  },
  make_specs(Tail, SpecList ++ [Spec]);

make_specs([], SpecList) ->
  SpecList.
