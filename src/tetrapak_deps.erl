-module(tetrapak_deps).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([run/2]).

-export([loadpathes/0, load_app/1]).

-include_lib("tetrapak/include/tetrapak.hrl").

-record(lib_manager, {
    lib_dir,
    cache,
    current_app
}).

app() ->
    application:load(tetrapak_deps),
    loadpathes(),
    {is_erlang_app(), false}.

tasks(tasks) ->
    [
     {"tetrapak:startapp",  ?MODULE, "Start the current application"},
     {"deps:download", ?MODULE, "Download application dependencies"},
     {"deps:build",  ?MODULE, "Install application dependencies"},
     {"force:deps:build",  ?MODULE, "Install application dependencies"}
    ];

tasks(_) ->
    [].

run("deps:download", _) ->
    DepsDirs = deps_dirs(),
    Acc = lists:foldl(fun(App, Acc) -> download_app(App, Acc) end, [], DepsDirs),
    {done, [{deps, lists:reverse(Acc)}]};


run("deps:build", _) ->
    tetrapak:require("deps:download"),
    Deps = tetrapak:get("deps:download:deps"),
    deps_build(lists:reverse(Deps));

run("force:deps:build", _) ->
    deps_build(deps_dirs());

run("tetrapak:startapp", Extra) ->
    [load_app(Dir) || {_, _, Dir} <- deps_dirs()],
    tetrapak_task_shell:run("tetrapak:startapp", Extra).

% --------------------------------------------------------------------------------------------------
% -- Helpers

load_app(Dir) ->
    code:add_patha(filename:join(Dir, "ebin")),
    {ok, BaseConfig} = application:get_env(tetrapak, config),
    ProjectConfig = tetrapak_task_boot:tetrapak_config(Dir, #config{values = BaseConfig}).

deps_build(Deps) ->
    [install_app(DepDir) || DepDir <- Deps],
    done.

deps_dirs() ->
    Manager  = init_manager(),
    Deps     = get_deps(),
    [build_dir(Dep, Manager) || Dep <- Deps].

init_manager() ->
    {ok, ErlLibs} = application:get_env(tetrapak_deps, erl_libs),
    ErlLibsDir = filename:join(os:getenv("HOME"), ErlLibs),
    file:make_dir(ErlLibsDir),
    #lib_manager{lib_dir = ErlLibsDir,
                 cache = load_cache(ErlLibsDir),
                 current_app = appname()}.

get_deps() ->
    case filelib:is_file(tetrapak:path("tetrapak/config.ini")) of
        true ->
            expand_shortcuts(tetrapak:config("dev.deps", []));
        false ->
            []
    end.

%rebar_deps() ->
%    case file:consult(tetrapak:path("rebar.config")) of
%        {ok, Config} ->
%            [{App, Conf} || {App, _, Conf} <- proplists:get_value(deps, Config, [])];
%        _ ->
%            []
%    end.

expand_shortcuts(Deps) ->
    {ok, Shortcuts} = application:get_env(tetrapak_deps, shortcuts),
    ConfiguredShortcuts = tetrapak:config("dev.shortcuts", []),
    [expand(Dep, Shortcuts ++ ConfiguredShortcuts) || Dep <- Deps].

expand({App, Info0} = Dep, Shortcuts) ->
    Type = element(1, Info0),
    case lists:keyfind(Type, 1, Shortcuts) of
        false ->
            Dep;
        {Type, BuildinType, Shortcut} ->
            Repo = element(2, Info0),
            Info1 = setelement(1, Info0, BuildinType),
            {App, setelement(2, Info1, Shortcut ++ Repo)}
    end.

load_cache(ErlLibsDir) ->
    File = filename:join(ErlLibsDir, "tetrapak.deps"),
    case file:consult(File) of
        {error, enoent} ->
            [];
        {ok, [Apps]} ->
            Apps;
        {error, Error} ->
            io:format(user, "error: ~p~n", [Error]),
            io:format("dependency infos corrupted, deleting it...~n", []),
            tpk_file:delete(File),
            []
    end.

appname() ->
    [AppSrc] = filelib:wildcard(filename:join(tetrapak:path("src/"), "*.app.src")),
    Name = filename:basename(AppSrc, ".app.src"),
    list_to_atom(Name).

build_dir({App, {git, Repo}}, Manager) -> build_dir({App, {git, Repo, "master"}}, Manager);
build_dir({App, {git, Repo, Info}}, #lib_manager{lib_dir = LibDir}) ->
    {App, {git, Repo, Info}, filename:join(LibDir, build_folder_name(atom_to_list(App), Info))}.

build_folder_name(AppName, Value) -> AppName ++ "-" ++ cut(Value).

cut(Value) when length(Value) > 8 -> string:substr(Value, 1, 8);
cut(Value) -> Value.

download_app({_App, {git, Repo, Info}, Dir} = Dep, Acc) ->
    case filelib:is_dir(Dir) of
        true ->
            Acc;
        false ->
            {ok, _} = git:download(Repo, Dir, Info),
            [Dep | Acc]
    end.

install_app({_App, _, Dir}) ->
    tetrapak_context:add_directory(tetrapak_task:context(), Dir),
    ok = tetrapak_task:require_all(Dir, ["tetrapak:boot"]),
    ok = tetrapak_task:require_all(Dir, ["deps", "build"]).

loadpathes() ->
    [check_deps(Path) || Path <- code:get_path()].
check_deps(EbinPath) ->
    [check_deps_new(Path) || Path <- filelib:wildcard(filename:join([EbinPath, "..", "deps", "*", "ebin"]))].
check_deps_new(EbinPath) ->
    code:add_pathz(EbinPath),
    check_deps(EbinPath).

is_erlang_app() ->
    tpk_file:exists_in("src", "*.app.src") orelse
    tpk_file:exists_in("ebin", "*.app") orelse
    filelib:is_file(filename:join([tetrapak:path("tetrapak"), "config.ini"])).
