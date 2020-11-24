- module(node).
- export([init/6, activeThread/2, passiveThread/2, counter/2]).
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
            View = moveOld(View_temp),
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
                    View = moveOld(View_temp),
                    Buffer = appendFirst(Buffer_temp, View, (State#state.c / 2) -1),
                    From ! {push, self(), Buffer}
            end,
        select(View, State#state.c, State#state.h, State#state.s, Bufferp),
        NewState = #state{id = State#state.id, log = [], buffer = Buffer, view = increaseAge(View), c = State#state.c, h=State#state.h, s=State#state.h, pushPull=State#state.pushPull, peerSelection=State#state.peerSelection},
        ActivePid ! {updateState, NewState},
        passiveThread(NewState, ActivePid)
    end.

randPeerSelection(View) ->
    42.

select(View, C, H, S, Buffer) ->
    42.

tailPeerSelection(View) ->
    42.

moveOld(View) ->
    42.

permute(View) ->
    42.

appendFirst(Buffer, View, N) ->
    42.

increaseAge(View) ->
    42.