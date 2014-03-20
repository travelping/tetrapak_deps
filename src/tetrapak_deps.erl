-module(tetrapak_deps).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([run/2]).

-export([loadpathes/0]).

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
     {"tetrapak:deps",       ?MODULE, "Get dependencies"},
     {"tetrapak:depsboot",   ?MODULE, "Apply boot on all dependencies"},
     {"tetrapak:load:path",  ?MODULE, "Load application dependencies"},
     {"tetrapak:load:deps",  ?MODULE, "Load application dependencies", [{run_before, ["build"]}]},
     {"deps:download",       ?MODULE, "Download application dependencies"},
     {"deps:build",          ?MODULE, "Install application dependencies"},
     {"force:deps:download", ?MODULE, "Download application dependencies", [{run_before, ["force:deps:build"]}]},
     {"force:deps:build",    ?MODULE, "Install application dependencies"}
    ];

tasks(_) ->
    [].

run("tetrapak:deps", _) ->
    {done, [{info, deps_dirs()}]};

run("tetrapak:depsboot", _) ->
    on_deps(fun boot_dir_rec/1),
    done;

run("tetrapak:load:path", _) ->
    code:add_patha(tetrapak:path("ebin")),
    done;

run("tetrapak:load:deps", _) ->
    on_deps(fun({_, _, Dir}) ->
                    case filelib:is_dir(Dir) of
                        true ->
                            ok = tetrapak_task:require_all(["tetrapak:depsboot"]),
                            ok = tetrapak_task:require_all(Dir, ["tetrapak:load:path", "tetrapak:load:deps"]);
                        false ->
                            ok
                    end
            end),
    done;

run("deps:download", _) ->
    deps_download(false);

run("deps:build", _) ->
    require_download(),
    deps_build(tetrapak:get("deps:download:deps"), false);

run("force:deps:download", _) ->
    deps_download(true);

run("force:deps:build", _) ->
    ok = tetrapak:require("tetrapak:depsboot"),
    deps_build(tetrapak:get("tetrapak:deps:info"), true);

run("tetrapak:startapp", Extra) ->
    tetrapak_task_shell:run("tetrapak:startapp", Extra).

% --------------------------------------------------------------------------------------------------
% -- Helpers

deps_download(Force) ->
    Acc = lists:foldl(fun(App, Acc) -> download_app(App, Acc, Force) end, [], tetrapak:get("tetrapak:deps:info")),
    {done, [{deps, lists:reverse(Acc)}]}.

require_download() ->
    require_download("deps:download").
require_download(Task) ->
    ok = tetrapak:require(Task),
    ok = tetrapak:require("tetrapak:depsboot").

on_deps(Fun) ->
    [Fun(Dep) || Dep <- tetrapak:get("tetrapak:deps:info")].

deps_build(Deps, Force) ->
    [install_app(DepDir, Force) || DepDir <- Deps],
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
                 current_app = tetrapak:get("info:application:name")}.

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
            io:format("error: ~p~n", [Error]),
            io:format("dependency infos corrupted, deleting it...~n", []),
            tpk_file:delete(File),
            []
    end.

build_dir({App, {git, Repo}}, Manager) -> build_dir({App, {git, Repo, "master"}}, Manager);
build_dir({App, {git, Repo, Info}}, #lib_manager{lib_dir = LibDir}) ->
    {App, {git, Repo, Info}, filename:join(LibDir, build_folder_name(atom_to_list(App), Info))}.

build_folder_name(AppName, Value) -> AppName ++ "-" ++ cut(Value).

cut(Value) when length(Value) > 8 -> string:substr(Value, 1, 8);
cut(Value) -> Value.

download_app({_App, {git, Repo, Info}, Dir} = Dep, Acc, Force) ->
    case {filelib:is_dir(Dir), Force} of
        {true, false} ->
            Acc;
        {Exists, _} ->
            Force andalso Exists andalso tpk_file:delete(Dir),
            {ok, _} = git:download(Repo, Dir, Info),
            download_app_rec(Dir, Force),
            [Dep | Acc]
    end.

download_app_rec(Dir, Force) ->
    boot_dir(Dir),
    tetrapak_task:require_all(Dir, [deps_task("deps:download", Force)]).

boot_dir_rec(Dir) ->
    ok = tetrapak_task:require_all(boot_dir(Dir), ["tetrapak:depsboot"]).

boot_dir({_, _, Dir}) ->
    boot_dir(Dir);
boot_dir(Dir) ->
    tetrapak_context:add_directory(tetrapak_task:context(), Dir),
    ok = tetrapak_task:require_all(Dir, ["tetrapak:boot"]),
    Dir.

install_app({_App, _, Dir}, Force) ->
    [ok = tetrapak_task:require_all(Dir, [Task]) || Task <- [deps_task("deps", Force), "build", "tetrapak:load:path"]].

deps_task(Name, false) -> Name;
deps_task(Name, true) -> "force:" ++ Name.

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
