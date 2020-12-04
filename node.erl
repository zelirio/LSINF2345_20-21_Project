- module(node).
- export([init/7, activeThread/2, passiveThread/2]).
- record(state, {id ,log, buffer, view, c, h, s, pushPull, peerSelection, tree}).

init(Id, C, PeerS, PushPull, H, S, TreePid) ->
    State = #state{id = Id, log = [], buffer = [], view = [], c = C, h=H, s=S, pushPull=PushPull, peerSelection=PeerS, tree=TreePid},
    ActivePid = spawn(node, activeThread, [State,-1]),
    PassivePid = spawn(node, passiveThread, [State,ActivePid]),
    ActivePid ! {hello, PassivePid},
    io:format("~w ~w ~w ~n",[Id,ActivePid,PassivePid]),
    {Id,ActivePid,PassivePid}.


activeThread(State,PassivePid) ->
    receive 
        {hello, From} ->
            activeThread(State, From);
        {updateState, NewState} ->
            activeThread(NewState, PassivePid);
        {time, Round} ->
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
            %io:format(" ~w envoie a ~w~n",[State#state.id,Peer]),
            Peer ! {push, self(), Buffer},
            if
                State#state.pushPull ->
                    receive
                        {push,_,Bufferp} ->
                            NewView = select(View, State#state.c, State#state.h, State#state.s, Bufferp)
                    after
                        1000 ->
                            io:format("timeout ~w~n",[State#state.id]),
                            NewView = View 
                    end;
                true ->
                    NewView = View
            end,
            %io:format("~w ~w~n",[State#state.id,NewView]),
            file:write_file("node.log", io_lib:fwrite("~w ~w ~w~n",[State#state.id,NewView, Round]),[append]),
            NewState = #state{id = State#state.id, log = [], buffer = Buffer, view = increaseAge(NewView), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            PassivePid ! {updateState, NewState},
            activeThread(NewState, PassivePid)
    end.


passiveThread(State, ActivePid) ->
    receive 
        {tree} ->
            Neigh = binaryTreeServer:getNeighbors(State#state.tree, [self(),State#state.id]),
            NewState = #state{id = State#state.id, log = [], buffer = State#state.buffer, view = zeroPading(Neigh), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            ActivePid ! {updateState, NewState},
            passiveThread(NewState, ActivePid);
        {updateState, NewState} ->
            passiveThread(NewState, ActivePid);
        {recover, {Id,_,Pid}} ->
            NewState = #state{id = State#state.id, log = [], buffer = State#state.buffer, view = zeroPading([[Pid,Id]]), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            ActivePid ! {updateState, NewState},
            passiveThread(NewState, ActivePid);
        {push, From, Bufferp} -> 
            if 
               State#state.pushPull ->
                    Buffer_temp = [[self(),0,State#state.id]],
                    View_temp = permute(State#state.view),
                    View = moveOld(View_temp,State#state.h),
                    Buffer = appendFirst(Buffer_temp, View, (State#state.c div 2) -1),
                    From ! {push, self(), Buffer};
                true ->
                    Buffer = State#state.buffer,
                    View  = State#state.view
            end,
            %io:format("~w ~w~n",[State#state.id,Bufferp]),
            NewView = select(View, State#state.c, State#state.h, State#state.s, Bufferp),
            NewState = #state{id = State#state.id, log = [], buffer = Buffer, view = increaseAge(NewView), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection, tree=State#state.tree},
            ActivePid ! {updateState, NewState},
            passiveThread(NewState, ActivePid)
    end.

zeroPading([[Pid,Id]|T]) ->
    [[Pid,0,Id]] ++ zeroPading(T);

zeroPading([]) ->
    [].

randPeerSelection(View) ->
    I = rand:uniform(length(View)),
    lists:nth(I,View).

select(View, C, H, S, Buffer) ->
    ViewAppend = View ++ Buffer,
    ViewNoDup = filterDub(ViewAppend),
    ViewNoOld = filterOld(ViewNoDup, erlang:max(erlang:min(H, length(ViewNoDup)-C),0)),
    ViewNoHead = lists:sublist(ViewNoOld,erlang:max(erlang:min(S,length(ViewNoOld)-C),0) + 1, length(ViewNoOld)),
    filterRandom(ViewNoHead,erlang:max(length(ViewNoHead)-C,0)).

filterRandom(List, 0) ->
    List;

filterRandom(List, N) -> 
    I = rand:uniform(length(List)),
    filterRandom(delete(List,I),N-1).

delete([],0) ->
    [];

delete([_|T], 0) ->
    T;

delete([H|T], N) ->
    [H] ++ delete(T,N-1).

filterOld(List,N) ->
    Sorted = lists:sort(fun([_|[A|_]],[_|[B|_]]) -> A =< B end, List),
    Oldest = lists:sublist(Sorted,length(Sorted) - N + 1, length(Sorted)),
    filter(List,Oldest).

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


tailPeerSelection(View) ->
    lists:last(View).

moveOld(View,H) ->
    Sorted = lists:sort(fun([_|[A|_]],[_|[B|_]]) -> A =< B end, View),
    Oldest = lists:sublist(Sorted,erlang:max(length(Sorted) - H,0) + 1, length(Sorted)),
    Filt = filter(View,Oldest),
    Filt ++ Oldest.

filter(View, [H|T]) ->
    filter(lists:delete(H,View),T);

filter(View,[]) ->
    View.

permute(View) ->
    permutations(View,[]).

permutations([],P) -> 
    P;

permutations(L1,P) ->
    Elem=randPeerSelection(L1),
    permutations(lists:delete(Elem,L1),P++[Elem]).

appendFirst(Buffer, View, N) ->
    First = lists:sublist(View,N),
    Buffer ++ First.

increaseAge(View) ->
    lists:map(fun([P,A,Id]) -> [P,A+1,Id] end, View).