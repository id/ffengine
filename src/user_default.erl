%%% ============================================================================
%%% @doc User customizations to the erlang shell
%%% @end
%%% ============================================================================
-module(user_default).
-author("Ivan Dyachkov <ivan@dyachkov.org>").

-export([help/0]).

-export([ lm/0
        , mm/0
        ]).

%%%_* Types ====================================================================
-type reason() :: nofile
                | sticky_directory
                | badarg
                | badfile
                | not_purged.

%%%_* API ======================================================================
%% see http://erlang.org/pipermail/erlang-questions/2006-January/018650.html
help() ->
  shell_default:help(),
  io:format("** user extended commands **\n"),
  io:format("mm()       -- load all modified modules\n"),
  io:format("lm()       -- list modified modules\n"),
  ok.

-spec lm() -> [{module, atom()} | {error, reason()}].
lm() ->
  load_modified_modules().

-spec mm() -> [atom()].
mm() ->
  modified_modules().

%%%_* Internal =================================================================
-spec load_modified_modules() -> [{module, atom()} | {error, reason()}].
load_modified_modules() ->
  [c:l(M) || M <- modified_modules()].

-spec modified_modules() -> [atom()].
modified_modules() ->
  [M || {M, _} <- code:all_loaded(), module_modified(M) =:= true].

-spec module_modified(atom()) -> boolean() | removed.
module_modified(Module) ->
  case code:is_loaded(Module) of
    false             -> false;
    {file, preloaded} -> false;
    {file, []}        -> false; % compiled from abstract forms, no file
    {file, Path}      ->
      CompileOpts = erlang:get_module_info(Module, compile),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src         = proplists:get_value(source, CompileOpts),
      module_modified(Path, CompileTime, Src)
  end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
  case find_module_file(Path) of
    non_existing -> removed;
    ModPath      ->
      case beam_lib:chunks(ModPath, ["CInf"]) of
        {ok, {_, [{_, CB}]}} ->
          CompileOpts = binary_to_term(CB),
          CompileTime = proplists:get_value(time, CompileOpts),
          Src         = proplists:get_value(source, CompileOpts),
          not ((CompileTime =:= PrevCompileTime) and (Src =:= PrevSrc));
        _ ->
          false
      end
  end.

find_module_file(Path) ->
  case file:read_file_info(Path) of
    {ok, _} -> Path;
    _       -> code:where_is_file(filename:basename(Path)) % path was changed?
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
