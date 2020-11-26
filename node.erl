- module(node).
- export([init/6, activeThread/2, passiveThread/2, counter/2, permute/1]).
- record(state, {id ,log, buffer, view, c, h, s, pushPull, peerSelection}).

init(Id, C, PeerS, PushPull, H, S) ->
    State = #state{id = Id, log = [], buffer = [], view = [], c = C, h=H, s=S, pushPull=PushPull, peerSelection=PeerS},
    ActivePid = spawn(node, activeThread, [State,-1]),
    spawn(node, counter, [3000, ActivePid]),
    PassivePid = spawn(node, passiveThread, [State,ActivePid]),
    ActivePid ! {hello, PassivePid},
    {Id,ActivePid,PassivePid}.

counter(Timeout, Pid) ->
    timer:sleep(Timeout),
    Pid ! {time},
    counter(Timeout, Pid).

activeThread(State,PassivePid) ->
    receive 
        {hello, From} ->
            activeThread(State, From);
        {updateState, NewState} ->
            activeThread(NewState, PassivePid);
        {time} ->
            if 
                State#state.peerSelection =:= rand ->
                    Peer = randPeerSelection(State#state.view);
                State#state.peerSelection =:= tail ->
                    Peer = tailPeerSelection(State#state.view)
            end,
            Buffer_temp = [[self(),0]],
            View_temp = permute(State#state.view),
            View = moveOld(View_temp,State#state.h),
            Buffer = appendFirst(Buffer_temp, View, (State#state.c / 2) -1),
            Peer ! {push, self(), Buffer},
            if
                State#state.pushPull ->
                    receive
                        {push,_,Bufferp} ->
                            select(View, State#state.c, State#state.h, State#state.s, Bufferp)
                    end
            end,
            NewState = #state{id = State#state.id, log = [], buffer = Buffer, view = increaseAge(View), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection},
            PassivePid ! {updateState, NewState},
            activeThread(NewState, PassivePid)
    end.


passiveThread(State, ActivePid) ->
    receive 
        {push, From, Bufferp} -> 
            if 
               State#state.pushPull ->
                    Buffer_temp = [[self(),0]],
                    View_temp = permute(State#state.view),
                    View = moveOld(View_temp,State#state.h),
                    Buffer = appendFirst(Buffer_temp, View, (State#state.c / 2) -1),
                    From ! {push, self(), Buffer}
            end,
        select(View, State#state.c, State#state.h, State#state.s, Bufferp),
        NewState = #state{id = State#state.id, log = [], buffer = Buffer, view = increaseAge(View), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection},
        ActivePid ! {updateState, NewState},
        passiveThread(NewState, ActivePid)
    end.

randPeerSelection(View) ->
    I = rand:uniform(length(View)),
    lists:nth(I,View).

select(View, C, H, S, Buffer) ->
    42.

tailPeerSelection(View) ->
    lists:last(View).

moveOld(View,H) ->
    Sorted = lists:sort(fun([_,A],[_,B]) -> A =< B end, View),
    Oldest = lists:sublist(Sorted,length(Sorted) - H + 1, length(Sorted)),
    Filt = filter(View,Oldest,[]),
    Filt ++ Oldest.

filter([E|T], Oldest, Acc) ->
    C = contains(E,Oldest),
    if 
        C ->
            filter(T,Oldest,Acc);
        true ->
            filter(T,Oldest,Acc ++ [E])
    end;

filter([],_,Acc) ->
    Acc.

contains(E,[A|T]) ->
    if 
        E == A -> 
            true;
        true -> 
            contains(E,T)
    end;

contains(_,[]) ->
    false.

permute(View) ->
    randPeerSelection(permutations(View)).

permutations([]) -> 
    [[]];

permutations(L1) ->
    Permutations =
          fun(Start, Rest) ->
            Perm_Rest = permutations(Rest),  
            lists:map(fun(X) -> [Start | X] end, Perm_Rest)
            end,
    [ X2  || X1 <- L1, X2 <- Permutations(X1, L1--[X1])].

appendFirst(Buffer, View, N) ->
    First = lists:sublist(View,N),
    Buffer ++ First.


increaseAge(View) ->
    lists:map(fun(A) -> A+1 end, View).