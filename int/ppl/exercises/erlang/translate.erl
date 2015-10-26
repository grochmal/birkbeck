-module(translate).
-export([doc/0, loop/0, translate/2]).

loop() ->
    receive
	{From, "casa"} ->
	    From ! "house",
	    loop();
	{From, "blanca"} ->
	    From ! "white",
	    loop();
	{From, _} ->
	    From ! "I don't understand",
	    loop()
    end.

translate(To, Word) ->
    To ! {self(), Word},
    receive
	Translation -> Translation
    end.

doc() ->
    process_flag(trap_exit, true),
    receive
	{monitor, Process} ->
	    link(Process),
	    register(tr, Pid),
	    io:format("Monitoring process ~p~n", [Process]),
	    doc();
	{'EXIT', From, Reason} ->
	    io:format("Process ~p died: ~p~n", [From, Reason]),
	    Pid = spawn(fun loop/0),
	    io:format("Restarting as ~p ~p~n", [Pid, tr]),
	    self() ! {monitor, Pid},
	    link(Pid),
	    doc()
    end.

