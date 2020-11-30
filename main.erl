- module(main).
-export([start/6]).

start(N, C, PeerS, PushPull, H, S) ->
    Nodes = init(N, PeerS, C, PushPull, H, S),
    notif(Nodes).

init(N, PeerS, C, PushPull,H, S) -> 
    {ok, Pid} = binaryTreeServer:start_link(server),
    init(N, Pid, C, PeerS, PushPull,H, S).

init(1, Pid, C, PeerS, PushPull, H, S) -> 
    {Id,ActivePid,PassivePid} = node:init(1, C, PeerS, PushPull, H, S,Pid),
    binaryTreeServer:add(Pid,PassivePid),
    {Id,ActivePid,PassivePid};

init(N, Pid, C, PeerS, PushPull, H, S) ->
    {Id,ActivePid,PassivePid} = node:init(N, C, PeerS, PushPull, H, S,Pid),
    _ = binaryTreeServer:add(Pid,PassivePid),
    [{Id,ActivePid,PassivePid}] ++ init(N-1,Pid, C, PeerS, PushPull, H, S).

notif({_,_,PassivePid}) ->
    PassivePid ! {tree},
    42;

notif([{_,_,PassivePid}|T]) ->
    PassivePid ! {tree},
    notif(T).