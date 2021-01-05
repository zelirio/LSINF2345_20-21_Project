- module(main).
-export([start/6, start2/0]).


% Main loop that counts the number of round, notifiy every node when a new round begins and follows the steps of the given scenario.
counter(Timeout,Round,Nodes, N, C, PeerS, PushPull, H, S, Tree, Dead) ->
    timer:sleep(Timeout),
    if
        Round==30 ->
            NewNodes = addNodes((N*60) div 100, Tree, C, PeerS, PushPull, H, S, (N*40) div 100),
            notif(NewNodes),
            timer:sleep(100),   % Wait for all new nodes to start properly.
            NewList = Nodes ++ NewNodes,
            time(NewList, Round),
            counter(Timeout, Round+1,NewList, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==60 ->
            NewNodes = addNodes((N*80) div 100, Tree, C, PeerS, PushPull, H, S, (N*60) div 100),
            notif(NewNodes),
            timer:sleep(100),   % Wait for all new nodes to start properly.
            NewList = Nodes ++ NewNodes,
            time(NewList, Round),
            counter(Timeout, Round+1,NewList, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==90 ->
            NewNodes = addNodes(N, Tree, C, PeerS, PushPull, H, S, (N*80) div 100),
            notif(NewNodes),
            timer:sleep(100),   % Wait for all new nodes to start properly.
            NewList = Nodes ++ NewNodes,
            time(NewList, Round),
            counter(Timeout, Round+1,NewList, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==120 ->
            {NewNodes, DeadNodes} = crash((N*60) div 100, Nodes,[]),
            time(NewNodes, Round),
            counter(Timeout, Round+1,NewNodes, N, C, PeerS, PushPull, H, S, Tree, DeadNodes);
        Round==150 ->
            [P|_] = Nodes, % get the first node in the list. the recovered nodes will use it as a referent.
            NewNodes = recover((length(Dead)*60) div 100, Nodes, Dead, P, C, PeerS, PushPull, H, S, Tree),
            timer:sleep(100),   % Wait for all recovered nodes to start properly
            time(NewNodes, Round),
            counter(Timeout, Round+1,NewNodes, N, C, PeerS, PushPull, H, S, Tree, Dead);
        Round==180 -> % ends the simulation by crashing all nodes and returning done.
            crash(length(Nodes), Nodes,[]),
            done;
        true ->
            time(Nodes, Round),
            counter(Timeout, Round+1,Nodes, N, C, PeerS, PushPull, H, S, Tree, Dead)
    end.

% recover the crashed nodes in round 150 and return the new list with all the alive nodes.
recover(0, Nodes, _, _, _, _, _, _, _, _) ->
    Nodes;

recover(N, Nodes, [H|T], P, C, PeerS, PushPull, Heal, S, Tree) ->
     {Id,ActivePid,PassivePid} = node:init(H, C, PeerS, PushPull, Heal, S,Tree),
     PassivePid ! {recover,P},
     recover(N-1, Nodes ++ [{Id,ActivePid,PassivePid}], T, P, C, PeerS, PushPull, Heal, S, Tree).

% Crashes N random nodes and delete them from the list of alive nodes.
% Returns the new list of alive nodes and a list with the ids of the crashed nodes.
crash(0,Nodes,Dead) ->
    {Nodes,Dead};

crash(N,Nodes,Dead) ->
    I = rand:uniform(length(Nodes)),
    Elem = lists:nth(I,Nodes),
    {Id,APid, PPid} = Elem,
    exit(APid, normal),
    exit(PPid, normal),
    crash(N-1, lists:delete(Elem,Nodes), Dead ++ [Id]).


% Notifies the nodes that a new round begins.
time([{_,Pid,_}], Round) ->
    Pid ! {time, Round};

time([{_,Pid,_}|T], Round) ->
    Pid ! {time, Round},
    time(T,Round).


start2()->start(128, 7, tail, true, 4, 3).
% Function to start the simulation.
% Should be called with the parameters you want for the gossip alogorithm.
start(N, C, PeerS, PushPull, H, S) ->
    [Nodes,Tree] = init((N*40) div 100, PeerS, C, PushPull, H, S),
    notif(Nodes),
    counter(3000,0,Nodes, N, C, PeerS, PushPull, H, S, Tree, []).

% Start the binaryTree before starting the nodes.
init(N, PeerS, C, PushPull,H, S) ->
    {ok, Pid} = binaryTreeServer:start_link(server),
    [init(N, Pid, C, PeerS, PushPull,H, S), Pid].

% Start N nodes and add its id and the pid of the passive thread in the tree.
% Returns a list with the ids and pids of every new node.
init(1, Pid, C, PeerS, PushPull, H, S) ->
    {Id,ActivePid,PassivePid} = node:init(1, C, PeerS, PushPull, H, S,Pid),
    binaryTreeServer:add(Pid,[PassivePid,1]),
    [{Id,ActivePid,PassivePid}];

init(N, Pid, C, PeerS, PushPull, H, S) ->
    {Id,ActivePid,PassivePid} = node:init(N, C, PeerS, PushPull, H, S,Pid),
    _ = binaryTreeServer:add(Pid,[PassivePid,N]),
    [{Id,ActivePid,PassivePid}] ++ init(N-1,Pid, C, PeerS, PushPull, H, S).

% Notify the nodes that the binaryTree is ready for them to ask who are their neigbors.
notif([]) ->
    42;

notif({_,_,PassivePid}) ->
    PassivePid ! {tree},
    42;

notif([{_,_,PassivePid}|T]) ->
    PassivePid ! {tree},
    notif(T).

% Start N new nodes in the middle of the simulation.
% Returns the list of added nodes.
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
