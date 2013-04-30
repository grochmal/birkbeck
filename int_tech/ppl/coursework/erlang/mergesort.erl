-module(mergesort).
-export([tests/0, tests/1, sendsync/2, splitmerge/0, fullmergesort/0]).

% receives a PID and an unsorted list, send the list sorted back to PID
sorter() ->
    receive
	{From, List} ->
	    From ! lists:sort(List)
	    % and die!  sorter() is not called here,
	    % as the process is always spawned to perform 1 sort only
    end.

% merger process, based on examples in http://www.erlang.org/doc/man/,
% concatenates N responses into a list
merger(N) -> merger(N, []).
merger(0, Acc) -> Acc;
merger(N, Acc) ->
    Elem = receive
	Res -> Res
    end,
    merger(N - 1, [Elem | Acc]).

% parallel sorter, uses two separate processes to sort the slices of the list
% then merges the resulting sorted lists together
splitmerge() ->
    receive
	{From, List} ->
	    Left   = spawn(fun sorter/0),
	    Right  = spawn(fun sorter/0),
	    {Ll, Lr} = lists:split(length(List) div 2, List),
	    Left  ! {self(), Ll},
	    Right ! {self(), Lr},
	    [Lsort, Rsort] = merger(2),
	    % Lsort and Rsort above aren't necesarily in the left/right order
	    % they were sent to the sorter processes, but both lists are sorted
	    % and that's all lists:merge needs (the order do not matter)
	    Sorted = lists:merge(Lsort, Rsort),
	    From ! Sorted,
	    splitmerge()
    end.

% as from Bruce Tate's book, send messages syncronously, useful for testing
sendsync(To, Msg) ->
    To ! {self(), Msg},
    Sorted = receive
	Reply -> Reply
    end,
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, Sorted),
    Sorted.

% structure of each test
test_struct(Pid, List) ->
    io:format("Sorting ~p~n", [List]),
    Sorted = sendsync(Pid, List),
    io:format("Output ~p~n", [Sorted]),
    Sorted = lists:sort(List),         % only succeeds if the sort was correct
    io:format("Sorted OK~n~n").

% test cases for the list sorting
tests() -> tests(fun splitmerge/0).
tests(Fun) ->
    Pid = spawn(Fun),
    Tests = [[1,2,1,2,1,2,1], [1,2,3,4,5,6], [], [3,2,1], [13,12,1,13]],
    lists:foreach(fun(X) -> test_struct(Pid, X) end, Tests).

% Extra algorithm:  As this is an exercise let me try to write the full
% mergesort algorithm as well.
%
% The full mergesort algotithm shall divide the list into elements and then
% merge all elements together using a different process for each merge process.
% For a list of N elements separated into N lists of 1 element the number of
% merges needed is always N-1; it does not matter how we merge the list.  But
% it matters for the performance of the mergesort that we have as many merges
% happening in parallel as possible.  For example for a list of 7 elements
% we can merge it in different ways:
%
%  o o o o o o o      o o o o o o o      o o o o o o o
%  oo oo oo o    (3)  oo o o o o o  (1)  oo o o o oo   (2)
%  oooo ooo      (2)  ooo o o o o   (1)  oo oo ooo     (2)
%  ooooooo       (1)  oooo o o o    (1)  oo ooooo      (1)
%                     ooooo o o     (1)  ooooooo       (1)
%                     oooooo o      (1)
%                     ooooooo       (1)
%
% The numbers in brakets represents the number of parallel merges that happen
% in each step, note that the sum is always 6.  The best case is to execute
% as many parallel merges as possible in each step.  This is easy to implement
% when the number of lists to merge in the current step is even, but if the
% number is odd one list will be left behind.  The algotithm below would merge
% this 7 element list as follows:
%
% o o o o o o o      last element is left behind and we merge a 6 element list
% oo oo oo o    (3), the before last element is left behind as well
% oooo oo o     (1), now we merge the before last element
% oooooo o      (1), and finally the last element is merged in
% ooooooo       (1)
%
% Unless otherwise stated all the code below is based on the article:
% http://www.planeterlang.org/en/planet/article/Parallel_merge_sort_in_Erlang/

% parallel map, used to transform the incomming list into lists of 1 element
pmap(Fun, List) ->
    Parent = self(),
    Refs = lists:map(fun(Elem) ->                  % spawn all processes
			 Ref = make_ref(),
			 spawn(fun() -> Parent ! {Ref, Fun(Elem)} end),
			 Ref
		     end, List),
    lists:map(fun(Ref) -> receive                  % collect the reslts
			      {Ref, Elem} -> Elem
			  end
	      end, Refs).

% as it is an exercise i will write the merge function by hand as well, this
% function is based on http://en.literateprograms.org/Merge_sort_%28Erlang%29
merge(Ls1, Ls2) -> merge(Ls1, Ls2, []).
merge(Ls1, [], Acc) -> lists:reverse(Acc) ++ Ls1;
merge([], Ls2, Acc) -> lists:reverse(Acc) ++ Ls2;
merge(Ls1 = [H1|T1], Ls2 = [H2|T2], Acc) ->
    {H, Ls1r, Ls2r} = case H1 < H2 of
			  true  -> {H1, T1, Ls2};
			  false -> {H2, Ls1, T2}
		      end,
    merge(Ls1r, Ls2r, [H|Acc]).

% collects the merged lists, the order of the merges do not matter
collect(N) -> collect([], N).
collect(Acc, 0) -> Acc;
collect(Acc, N) ->
    List = receive
	       Res -> Res
	   end,
    collect([List | Acc], N - 1).

% merge in parallel
merge_pl(Lists) -> merge_pl(Lists, 0).
merge_pl([], 0) -> [];
merge_pl([], N) ->       % here we advance one step in merging the lists
    Lists = collect(N),
    merge_pl(Lists);
merge_pl([L], N) ->             % the number of lists in this step was odd,
    merge(L, merge_pl([], N));  % leave one list behind
merge_pl([L1, L2 | Tail], N) ->
    Pid = self(),
    spawn(fun() -> Pid ! merge(L1, L2) end),
    merge_pl(Tail, N + 1).

% and the function that performs the full parallel mergesort,
% tests/1 can be used to test it
fullmergesort() ->
    receive
	{From, List} ->
	    Lslists = pmap(fun(X) -> [X] end, List),
	    Sorted = merge_pl(Lslists),
	    From ! Sorted,
	    fullmergesort()
    end.

