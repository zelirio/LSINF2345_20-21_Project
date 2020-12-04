- module(main).
-export([start/6]).

counter(Timeout,Round,Nodes, N, C, PeerS, PushPull, H, S, Tree, Dead) ->
    timer:sleep(Timeout),
    %file:write_file("node.log", io_lib:fwrite("Round ~w~n",[Round]),[append]),
    if
        Round==30 ->
            NewNodes = addNodes((N*60) div 100, Tree, C, PeerS, PushPull, H, S, (N*40) div 100),
            notif(NewNodes),
            timer:sleep(100),
            NewList = Nodes ++ NewNodes,
            time(NewList, Round),
            counter(Timeout, Round+1,NewList, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==60 ->
            NewNodes = addNodes((N*80) div 100, Tree, C, PeerS, PushPull, H, S, (N*60) div 100),
            notif(NewNodes),
            timer:sleep(100),
            NewList = Nodes ++ NewNodes,
            time(NewList, Round),
            counter(Timeout, Round+1,NewList, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==90 ->
            NewNodes = addNodes(N, Tree, C, PeerS, PushPull, H, S, (N*80) div 100),
            notif(NewNodes),
            timer:sleep(100),
            NewList = Nodes ++ NewNodes,
            time(NewList, Round),
            counter(Timeout, Round+1,NewList, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==120 ->
            {NewNodes, DeadNodes} = crash((N*60) div 100, Nodes,[]),
            time(NewNodes, Round),
            counter(Timeout, Round+1,NewNodes, N, C, PeerS, PushPull, H, S, Tree, DeadNodes);
        Round==150 ->
            [P|_] = Nodes,
            NewNodes = recover((length(Dead)*60) div 100, Nodes, Dead, P, C, PeerS, PushPull, H, S, Tree),
            timer:sleep(100),
            time(NewNodes, Round),
            counter(Timeout, Round+1,NewNodes, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==180 ->
            crash(length(Nodes), Nodes,[]),
            fini;
        true -> 
            time(Nodes, Round),
            counter(Timeout, Round+1,Nodes, N, C, PeerS, PushPull, H, S, Tree, Dead)
    end.

recover(0, Nodes, _, _, _, _, _, _, _, _) ->
    Nodes;

recover(N, Nodes, [H|T], P, C, PeerS, PushPull, Heal, S, Tree) ->
     {Id,ActivePid,PassivePid} = node:init(H, C, PeerS, PushPull, Heal, S,Tree),
     PassivePid ! {recover,P},
     recover(N-1, Nodes ++ [{Id,ActivePid,PassivePid}], T, P, C, PeerS, PushPull, Heal, S, Tree).

crash(0,Nodes,Dead) ->
    {Nodes,Dead};

crash(N,Nodes,Dead) ->
    I = rand:uniform(length(Nodes)),
    Elem = lists:nth(I,Nodes),
    {Id,APid, PPid} = Elem,
    exit(APid, normal),
    exit(PPid, normal),
    crash(N-1, lists:delete(Elem,Nodes), Dead ++ [Id]).

time([{_,Pid,_}], Round) ->
    Pid ! {time, Round};

time([{_,Pid,_}|T], Round) ->
    Pid ! {time, Round},
    time(T,Round).

start(N, C, PeerS, PushPull, H, S) ->
    [Nodes,Tree] = init((N*40) div 100, PeerS, C, PushPull, H, S),
    notif(Nodes),
    counter(3000,0,Nodes, N, C, PeerS, PushPull, H, S, Tree, []).

init(N, PeerS, C, PushPull,H, S) -> 
    {ok, Pid} = binaryTreeServer:start_link(server),
    [init(N, Pid, C, PeerS, PushPull,H, S), Pid].

init(1, Pid, C, PeerS, PushPull, H, S) -> 
    {Id,ActivePid,PassivePid} = node:init(1, C, PeerS, PushPull, H, S,Pid),
    binaryTreeServer:add(Pid,[PassivePid,1]),
    [{Id,ActivePid,PassivePid}];

init(N, Pid, C, PeerS, PushPull, H, S) ->
    {Id,ActivePid,PassivePid} = node:init(N, C, PeerS, PushPull, H, S,Pid),
    _ = binaryTreeServer:add(Pid,[PassivePid,N]),
    [{Id,ActivePid,PassivePid}] ++ init(N-1,Pid, C, PeerS, PushPull, H, S).

notif([]) ->
    42;

notif({_,_,PassivePid}) ->
    %io:format("notif ~w ~n",[PassivePid]),
    PassivePid ! {tree},
    42;

notif([{_,_,PassivePid}|T]) ->
    io:format("notif ~w ~n",[PassivePid]),
    PassivePid ! {tree},
    notif(T).

addNodes(N, Pid, C, PeerS, PushPull, H, S, Offset) ->
    if 
        N == Offset+1 ->
            {Id,ActivePid,PassivePid} = node:init(Offset+1, C, PeerS, PushPull, H, S,Pid),
            _ = binaryTreeServer:add(Pid,[PassivePid,Offset+1]),
            [{Id,ActivePid,PassivePid}];
        true ->
            {Id,ActivePid,PassivePid} = node:init(N, C, PeerS, PushPull, H, S,Pid),
            _ = binaryTreeServer:add(Pid,[PassivePid,N]),
            [{Id,ActivePid,PassivePid}] ++ addNodes(N-1,Pid, C, PeerS, PushPull, H, S, Offset)
    end.