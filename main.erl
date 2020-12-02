- module(main).
-export([start/6]).

counter(Timeout,Round,Nodes, N, C, PeerS, PushPull, H, S, Tree) ->
    timer:sleep(Timeout),
    file:write_file("node.log", io_lib:fwrite("Round ~w~n",[Round]),[append]),
    if
        Round==30 ->
            NewNodes = addNodes((N*60) div 100, Tree, C, PeerS, PushPull, H, S, (N*40) div 100),
            notif(NewNodes),
            counter(Timeout, Round+1,Nodes ++ NewNodes, N, C, PeerS, PushPull, H, S, Tree);
        Round==60 ->
            NewNodes = addNodes((N*80) div 100, Tree, C, PeerS, PushPull, H, S, (N*60) div 100),
            notif(NewNodes),
            counter(Timeout, Round+1,Nodes ++ NewNodes, N, C, PeerS, PushPull, H, S, Tree);
        Round==90 ->
            NewNodes = addNodes(N, Tree, C, PeerS, PushPull, H, S, (N*80) div 100),
            notif(NewNodes),
            counter(Timeout, Round+1,Nodes ++ NewNodes, N, C, PeerS, PushPull, H, S, Tree);
        Round==120 ->
            kaboom;
        Round==150 ->
            fzombi;
        Round==180 ->
            fini;
        true -> 
            counter(Timeout, Round+1,Nodes, N, C, PeerS, PushPull, H, S, Tree)
    end.

start(N, C, PeerS, PushPull, H, S) ->
    [Nodes,Tree] = init((N*40) div 100, PeerS, C, PushPull, H, S),
    notif(Nodes),
    counter(3000,1,Nodes, N, C, PeerS, PushPull, H, S, Tree).

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