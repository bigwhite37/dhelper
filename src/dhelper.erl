-module(dhelper).
-export([switch/0, start/1, start/3, startd/2]).

-define(START, 8). %% START = string:len("Elixir.") + 1
-define(END, 5).   %% END = string:len(".beam")
-define(SRCSUFFIXLEN, 11). %% len of "defmodule"

-define(BEAMD(PName), filename:join(filename:absname(""), ["_build/dev/lib/", string:to_lower(PName), "/ebin"])).
-define(SRC,          filename:join(filename:absname(""), "lib")).
-define(DSRC(PName),  filename:join(filename:absname(""), ["deps/", string:to_lower(PName), "/lib"])).

-define(LOGGER_SRC, "/usr/local/lib/elixir/lib/logger/lib").
-define(LOGGER_EBIN, "/usr/local/lib/elixir/lib/logger/ebin").

switch() ->
    start(?LOGGER_EBIN, ?LOGGER_SRC, "Logger").

start(Project_Name) ->
  start(?BEAMD(Project_Name), ?SRC, Project_Name),
  debugger:start().

startd(Project_Name, Deps) when is_list(Deps) ->
  start(?BEAMD(Project_Name), ?SRC, Project_Name),
  ElixirDeps = lists:filter(fun(X) -> is_elixir_project(?DSRC(X)) end, Deps),
  lists:map(fun(X) -> start(?BEAMD(X), ?DSRC(X), X) end, ElixirDeps),
  debugger:start().

start(BeamDir, SrcDir, Project_Name) ->
  %% BeamFile is a filename without path info.
  BeamFMPair = get_file_names(BeamDir),
  %% SrcFile is an abs path
  SrcFMPair = pair_src_module(SrcDir, Project_Name),
  BeamSrcPair = lists:foldl(fun({Module, SrcFile}, Acc) ->
                              {Module, BeamFile} = lists:keyfind(Module, 1, BeamFMPair),
                              [{Module, BeamFile, SrcFile} | Acc]
                            end, [], SrcFMPair),
                      
  lists:foreach(fun({Module, Beam, SrcPath}) ->
                  BeamPath = filename:join(BeamDir, Beam),
                  {ok, BeamBin} = file:read_file(BeamPath),
                  M = list_to_atom(lists:flatten(["Elixir.", atom_to_list(Module)])),
                  int:i({M, SrcPath, BeamPath, BeamBin})
                end, BeamSrcPair).

is_elixir_project(SrcPath) ->
  case file:list_dir(SrcPath) of
    {ok, _} -> true;
    {error, enoent} -> false
  end.

pair_src_module(SrcDir, Project_Name) ->
  io:format("~p~n~n", [SrcDir]),
  Srcs = os:cmd(lists:flatten(["find ", SrcDir, " | grep '\\.ex'"])),
  %% filename in FileNames is a abs path
  FileNames = string:tokens(Srcs, "\n"),
  %% returns like this [[{module_atom, file_string}, {module_atom, file_string}], [...] ...]
  Results = lists:map(fun(File) -> build_sm_tuple(File, Project_Name) end, FileNames),
  lists:flatten(Results).

build_sm_tuple(File, Project_Name) ->
  {ok, Content} = file:open(File, read),
  {ok, MP} = re:compile("defmodule", []),
  Modules = find_module(file:read_line(Content), Content, MP, [], Project_Name),
  file:close(Content),
  lists:map(fun(Item) -> {list_to_atom(Item), File} end, Modules).

find_module(eof, _, _, Modules, _) ->
  Modules;
find_module({ok, Data}, Content, MP, Modules, Project_Name)->
  case re:run(Data, MP, []) of
    nomatch ->
      find_module(file:read_line(Content), Content, MP, Modules, Project_Name);
    {match, _} ->
      case start_with(Data, "defmodule " ++ Project_Name) or start_with(Data, "defmodule Mix.") of
        true -> find_module(file:read_line(Content), Content, MP, [module_name(Data) | Modules], Project_Name);
        false -> find_module(file:read_line(Content), Content, MP, Modules, Project_Name)
      end
  end.

module_name(RawModuleName) ->
  ModuleName = string:strip(RawModuleName),
  string:sub_string(ModuleName, ?SRCSUFFIXLEN, string:len(ModuleName) - 4).

get_file_names(Path) ->
  case filelib:is_dir(Path) of
    false -> io:format("the path must be a dir~n");
    true -> modules_file_names(file:list_dir(Path))
  end.

modules_file_names({ok, RawFileList}) ->
  %% drop the file named "xxx.app"
  Matcher = fun(Item) ->
              case re:run(Item, "([a-z][A-Z])*.app") of
                {match, _} -> false;
                nomatch -> true
              end
            end,
  FileList = lists:filter(Matcher, RawFileList),
  lists:map(fun(FileName) -> handle_file_Name(FileName) end, FileList).

handle_file_Name(FileName) ->
  Module = string:sub_string(FileName, ?START, string:len(FileName) - ?END),
  {list_to_atom(Module), FileName}.

start_with(SourceStr, HeadStr) ->
  HeadStr =:= string:substr(SourceStr, 1, string:len(HeadStr)).
