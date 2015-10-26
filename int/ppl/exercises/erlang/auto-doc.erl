-module(auto_doc).
-export([doc/0, echome/2]).

% 1> Doc = spawn(fun  auto_doc:doc/0).
% 2> Doc ! {monitor, Doc}.

echome(To, Word) ->
    To ! {self(), Word},
    receive
	Translation -> Translation
    end.

doc() ->
    process_flag(trap_exit, true),
    receive
	newme ->
	    NewDoc = spawn(fun doc/0),
	    link(NewDoc),
	    NewDoc ! {monitor, self()},
	    io:format("newme: monitoring process ~p~n", [NewDoc]),
	    DocPid = whereis(doc),
	    if is_pid(DocPid) -> unregister(doc),
				 register(doc, NewDoc);
	       true           -> register(doc, NewDoc)
	    end,
	    doc();
	{monitor, Process} ->
	    if Process == self() -> self() ! newme;
	       true              -> link(Process),
				    io:format("monitor: watch process ~p~n",
					      [Process])
	    end,
	    doc();
	{From, Word} ->
	    From ! Word,
	    doc();
	die ->
	    io:format("Doc dying~n"),
	    exit("arrrrrrrrgh....");
	{'EXIT', From, Reason} ->
	    io:format("Process ~p died: ~p~n", [From, Reason]),
	    self() ! newme,
	    io:format("Restarting as ~p~n", [doc]),
	    doc()
    end.

