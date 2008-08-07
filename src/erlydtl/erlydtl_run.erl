%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
-module(erlydtl_run).

-export([run/1, compile/1]).
-import(erlydtl_report, [report/2, error/1]).


compile(Args) ->
    F = fun () ->
                case parse_args(Args) of
                    [File] when is_list(File) ->
                        Module = filename:basename(File, ".dtl"),
                        interpret_compile(File, erlydtl_compiler:compile(File, list_to_atom(Module)));
                    [File, Module] when is_list(File), is_atom(Module) ->
                        interpret_compile(File, erlydtl_compiler:compile(File, Module, []));
                    E ->
                        report("Invalid arguments to compile: ~p", [E]),
                        error("Compile failed.")
                end
        end,
    run(F).

interpret_compile(_, ok) -> ok;
interpret_compile(File, {error, {{Line, _Char}, _Module, [Error, Arg]}}) ->
    erlydtl_report:error(Line, File, {"~s: ~s", [Error, lists:flatten(Arg)]});
interpret_compile(File, Error) ->
    erlydtl_report:error(File, {"Compile error: ~p", [Error]}).
        
run(F) ->
    wait_init(),
    case catch {ok, F()} of
	{ok, _} ->
	    shutdown_ok();
	{'EXIT', E} ->
	    report("erlydtl terminated abnormally: ~P.", [E, 10]),
	    shutdown_error();
	Thrown ->
	    report("internal error: throw without catch in erlydtl: ~P.",
		   [Thrown, 15]),
	    shutdown_error()
    end.

wait_init() ->
    case erlang:whereis(code_server) of
	undefined ->
	    erlang:yield(),
	    wait_init();
	_ ->
	    ok
    end.

%% When and if a function init:stop/1 becomes generally available, we
%% can use that instead of delay-and-pray when there is an error.

shutdown_ok() ->
    %% shut down emulator nicely, signalling "normal termination"
    init:stop().

shutdown_error() ->
    %% delay 1 second to allow I/O to finish
    receive after 1000 -> ok end,
    %% stop emulator the hard way with a nonzero exit value
    halt(1).


parse_args([A | As]) when is_atom(A) ->
    [parse_arg(atom_to_list(A)) | parse_args(As)];
parse_args([A | As]) ->
    [parse_arg(A) | parse_args(As)];
parse_args([]) ->
    [].

parse_arg(A) ->
    case catch {ok, edoc_lib:parse_expr(A, 1)} of
	{ok, Expr} ->
	    case catch erl_parse:normalise(Expr) of
		{'EXIT', _} ->
		    report("bad argument: '~s':", [A]),
		    exit(error);
		Term ->
		    Term
	    end;
	{error, _, D} ->
	    report("error parsing argument '~s'", [A]),
	    error(D),
	    exit(error)
    end.
