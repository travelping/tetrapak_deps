#!/usr/bin/env escript
%%! -hidden -connect_all false -smp disable -kernel inet_dist_use_interface {127,0,0,1}

main([]) ->
    case os:cmd("whereis erl") of
        ""   ->
            io:format("'whereis erl' was not found~n", []);
        Path ->
            "\nlre" ++ Rest = lists:reverse(Path),
            Link = lists:reverse(Rest) ++ "tetrapak",
            Target = filename:join([code:lib_dir(tetrapak), "bin", "tetrapak"]),
            case file:make_symlink(Link, Target) of
                ok ->
                    io:format("Link created: ~s -> ~s~n", [Link, Target]);
                {error, eexist} ->
                    file:delete(Target),
                    file:make_symlink(Link, Target),
                    io:format("Link exists, new created: ~s -> ~s~n", [Link, Target]);
                {error, eacces} ->
                    io:format("Create link error, permission denied: ~s -> ~s~n", [Link, Target]),
                    erlang:halt(2)
            end
    end.
