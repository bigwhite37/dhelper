-module(dhelper).
-export([start/1, start/3]).

-define(START, 8). %% START = string:len("Elixir.") + 1
-define(END, 5).   %% END = string:len(".beam)
-define(SRCSUFFIXLEN, 11). %% len of "defmodule"

start(Project_Name) ->
  start(filename:join(filename:absname(""), "_build/dev/lib/" ++ string:to_lower(Project_Name) ++ "/ebin"),
        filename:join(filename:absname(""), "lib"), Project_Name),
  debugger:start().

start(BeamDir, SrcDir, Project_Name) ->
  %% BeamFile is a filename without path info.
  BeamFMPair = get_file_names(BeamDir),
  %% SrcFile is an abs path
  SrcFMPair = pair_src_module(SrcDir, Project_Name),
  handle_start(pair_beam_src(BeamFMPair, SrcFMPair, []), BeamDir).

handle_start([], _) ->
  ok;
handle_start([{Module, Beam, SrcPath} | MBSList], BeamDir) ->
  BeamPath = filename:join(BeamDir, Beam),
  {ok, BeamBin} = file:read_file(BeamPath),
  M = list_to_atom(lists:flatten(["Elixir.", atom_to_list(Module)])),
  int:i({M, SrcPath, BeamPath, BeamBin}),
  handle_start(MBSList, BeamDir).

pair_beam_src(_, [], Acc) ->
  Acc;
pair_beam_src(BeamFMPair, [{Module, SrcFile} | SrcRest], Acc) ->
  io:format("~p~n", [atom_to_list(Module)]),
  {Module, BeamFile} = lists:keyfind(Module, 1, BeamFMPair),
  pair_beam_src(BeamFMPair, SrcRest, [{Module, BeamFile, SrcFile} | Acc]).

pair_src_module(SrcDir, Project_Name) ->
  Srcs = os:cmd(lists:flatten(["find ", SrcDir, " | grep .ex"])),
  %% filenae in FileNames is a abs path
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
      case start_with(Data, "defmodule " ++ Project_Name ++ ".") or start_with(Data, "defmodule Mix.") of
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
  FileList = lists:droplast(lists:sort(RawFileList)),
  lists:map(fun(FileName) -> handle_file_Name(FileName) end, FileList).

handle_file_Name(FileName) ->
  Module = string:sub_string(FileName, ?START, string:len(FileName) - ?END),
  {list_to_atom(Module), FileName}.

start_with(SourceStr, HeadStr) ->
  HeadStr =:= string:substr(SourceStr, 1, string:len(HeadStr)).
