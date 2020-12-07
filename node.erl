- module(node).
- export([init/7, activeThread/2, passiveThread/2]).
- record(state, {id, buffer, view, c, h, s, pushPull, peerSelection, tree}).

% Create the initial state of a node and launch the active and passive threads
init(Id, C, PeerS, PushPull, H, S, TreePid) ->
    State = #state{id = Id, buffer = [], view = [], c = C, h=H, s=S, pushPull=PushPull, peerSelection=PeerS, tree=TreePid},
    ActivePid = spawn(node, activeThread, [State,-1]),
    PassivePid = spawn(node, passiveThread, [State,ActivePid]),
    ActivePid ! {hello, PassivePid},
    {Id,ActivePid,PassivePid}.


activeThread(State,PassivePid) ->
    receive 
        {hello, From} -> % Get the Pid of the Passive thread when started.
            activeThread(State, From);
        {updateState, NewState} -> % Adopt the changes of the state of the passive thread.
            activeThread(NewState, PassivePid);
        {time, Round} -> % Begin a new Round.
            if   
                State#state.peerSelection == rand ->
                    [Peer,_,_] = randPeerSelection(State#state.view);
                State#state.peerSelection == tail ->
                    [Peer,_,_] = tailPeerSelection(State#state.view)
            end,
            Buffer_temp = [[PassivePid,0,State#state.id]],
            View_temp = permute(State#state.view),
            View = moveOld(View_temp,State#state.h),
            Buffer = appendFirst(Buffer_temp, View, (State#state.c div 2) -1),
            Peer ! {push, self(), Buffer},
            if
                State#state.pushPull -> %For push-pull policy, wait the reply of the passive thread of the neighbor.
                    receive
                        {push,_,Bufferp} ->
                            NewView = select(View, State#state.c, State#state.h, State#state.s, Bufferp)
                    after
                        1000 -> % Peer didn't respond it's probably crashed.
                            NewView = View 
                    end;
                true ->
                    NewView = View
            end,
            file:write_file("node.log", io_lib:fwrite("~w ~w ~w~n",[State#state.id,NewView, Round]),[append]), % Write the View of the node in a log file.
            NewState = #state{id = State#state.id, buffer = Buffer, view = increaseAge(NewView), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            PassivePid ! {updateState, NewState},
            activeThread(NewState, PassivePid)
    end.


passiveThread(State, ActivePid) ->
    receive 
        {tree} -> % Get the Neighbors from the binaryTree.
            Neigh = binaryTreeServer:getNeighbors(State#state.tree, [self(),State#state.id]),
            NewState = #state{id = State#state.id, buffer = State#state.buffer, view = zeroPading(Neigh), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            ActivePid ! {updateState, NewState},
            passiveThread(NewState, ActivePid);
        {updateState, NewState} ->  % Adopt the changes of the state of the active thread.
            passiveThread(NewState, ActivePid);
        {recover, {Id,_,Pid}} -> % recover a crashed node.
            NewState = #state{id = State#state.id, buffer = State#state.buffer, view = zeroPading([[Pid,Id]]), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            ActivePid ! {updateState, NewState},
            passiveThread(NewState, ActivePid);
        {push, From, Bufferp} -> % receive the view of another node.
            if 
               State#state.pushPull -> % For push-pull policy, reply to the active thread of the sender.
                    Buffer_temp = [[self(),0,State#state.id]],
                    View_temp = permute(State#state.view),
                    View = moveOld(View_temp,State#state.h),
                    Buffer = appendFirst(Buffer_temp, View, (State#state.c div 2) -1),
                    From ! {push, self(), Buffer};
                true ->
                    Buffer = State#state.buffer,
                    View  = State#state.view
            end,
            NewView = select(View, State#state.c, State#state.h, State#state.s, Bufferp),
            NewState = #state{id = State#state.id, buffer = Buffer, view = increaseAge(NewView), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            ActivePid ! {updateState, NewState},
            passiveThread(NewState, ActivePid)
    end.

% Add an age field containing 0 in a view without age.
zeroPading([[Pid,Id]|T]) ->
    [[Pid,0,Id]] ++ zeroPading(T);

zeroPading([]) ->
    [].

% Select a random element from a list.
randPeerSelection(View) ->
    I = rand:uniform(length(View)),
    lists:nth(I,View).

% Select elements from a view using the algorithm provided.
select(View, C, H, S, Buffer) ->
    ViewAppend = View ++ Buffer,
    ViewNoDup = filterDub(ViewAppend),
    ViewNoOld = filterOld(ViewNoDup, erlang:max(erlang:min(H, length(ViewNoDup)-C),0)),
    ViewNoHead = lists:sublist(ViewNoOld,erlang:max(erlang:min(S,length(ViewNoOld)-C),0) + 1, length(ViewNoOld)),
    filterRandom(ViewNoHead,erlang:max(length(ViewNoHead)-C,0)).

% Remove N random elements from a list.
filterRandom(List, 0) ->
    List;

filterRandom(List, N) -> 
    I = rand:uniform(length(List)),
    filterRandom(delete(List,I),N-1).

% Remove the element at index N from a list.
delete([],0) ->
    [];

delete([_|T], 0) ->
    T;

delete([H|T], N) ->
    [H] ++ delete(T,N-1).

% Remove the N oldest element of the view.
filterOld(List,N) ->
    Sorted = lists:sort(fun([_|[A|_]],[_|[B|_]]) -> A =< B end, List),
    Oldest = lists:sublist(Sorted,length(Sorted) - N + 1, length(Sorted)),
    filter(List,Oldest).


% Remove elements with the same Pid from the view.
filterDub(List) ->
    filterDub(List,[],[]).

filterDub([],_,_) ->
    [];

filterDub([[Pid|[Age|H]]|T], Filter, []) ->
    [[Pid|[Age|H]]] ++ filterDub(T, Filter ++ [H], Filter ++ [H]);

filterDub([[_|[_|H]]|T], Filter, [H|_]) -> 
    filterDub(T, Filter, Filter);

filterDub(List, Filter, [_|T]) ->
    filterDub(List, Filter, T).

% Return the last element of a list.
tailPeerSelection(View) ->
    lists:last(View).

% Move the H oldest elements at the end of the view.
moveOld(View,H) ->
    Sorted = lists:sort(fun([_|[A|_]],[_|[B|_]]) -> A =< B end, View),
    Oldest = lists:sublist(Sorted,erlang:max(length(Sorted) - H,0) + 1, length(Sorted)),
    Filt = filter(View,Oldest),
    Filt ++ Oldest. 

% Copy the View without the oldest elements.
filter(View, [H|T]) ->
    filter(lists:delete(H,View),T);

filter(View,[]) ->
    View.

% Makes a random permutation of the list.
permute(View) ->
    permutations(View,[]).

permutations([],P) -> 
    P;
% Get a random element of the list, put it in a new list and delete it from the original.
% The result is a random permution of the original list.
permutations(L1,P) ->
    Elem=randPeerSelection(L1),
    permutations(lists:delete(Elem,L1),P++[Elem]).

% Append N elements from View to Buffer. 
appendFirst(Buffer, View, N) ->
    First = lists:sublist(View,N),
    Buffer ++ First.

% Increase the age of all elements in View by 1.
increaseAge(View) ->
    lists:map(fun([P,A,Id]) -> [P,A+1,Id] end, View).