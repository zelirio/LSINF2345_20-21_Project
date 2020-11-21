- module(main).
-export([start/5]).


start(N, rand, true, H, S) -> 
   init(N);

start(N, rand, false, H, S) -> 
    init(N);

start(N, tail, true, H, S) -> 
    init(N);

start(N, PeerS, PushPull, H, S) ->
    init(N).

init(N) -> 
    {ok, Pid} = binaryTreeServer:start_link(server),
    init(N, Pid).

init(1, Pid) -> 
    binaryTreeServer:add(Pid,1);

init(N, Pid) ->
    _ = binaryTreeServer:add(Pid,N),
    init(N-1,Pid).