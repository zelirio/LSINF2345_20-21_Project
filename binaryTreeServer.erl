-module(binaryTreeServer).
-export[add/2,getNeighbors/2].
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {root}).
start(Name) ->
    gen_server:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{root={node,nil}}}.

handle_call({add, Node}, _From, State) ->
    NewState = add_recursive(Node,State#state.root),
    {reply,NewState,State#state{root=NewState}};

handle_call({getNeighbors, Node}, _From, State) ->
    {reply,getNeighbors_recurive(Node,State#state.root),State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

add_recursive(Node, {node,nil}) ->
    {node,Node,1,{node,nil},{node,nil}};

add_recursive(Node, {node, Root, Rank, {node,nil}, R}) ->
    {node, Root, Rank+1,{node, Node, 1, {node,nil}, {node,nil}}, R};

add_recursive(Node, {node, Root, Rank, L, {node,nil}}) ->
    {node, Root, Rank+1,L ,{node, Node, 1, {node,nil},{node,nil}}};

add_recursive(Node, {node, Root, RankRoot, {node, NodeL, RankL,LL,RL}, {node, NodeR, RankR,LR,RR}}) ->
    if
    RankL < RankR ->
        {node, Root, RankRoot+1,add_recursive(Node,{node,NodeL,RankL,LL,RL}), {node, NodeR, RankR,LR,RR}};
    true ->
        {node, Root, RankRoot+1, {node, NodeL, RankL,LL,RL},add_recursive(Node,{node,NodeR,RankR,LR,RR})}
    end.

getNeighbors_recurive(Node, {node, Node, _, {node, NodeL, _,_,_}, {node, NodeR, _,_,_}}) ->
    [NodeL,NodeR];

getNeighbors_recurive(Node, Root) ->
    getNeighbors_recurive(Node, Root , Root).

getNeighbors_recurive(_,{node,nil} , _) ->
    [];

getNeighbors_recurive(Node,{node, Node, _, {node, NodeL, _,_,_}, {node, NodeR, _,_,_}}, Parent) ->
    [NodeL, NodeR, Parent];

getNeighbors_recurive(Node,{node, Node, _, {node,nil}, {node, nil}}, Parent) ->
    [Parent];

getNeighbors_recurive(Node,{node, Node, _, {node, nil}, {node, NodeR, _,_,_}}, Parent) ->
    [NodeR, Parent];

getNeighbors_recurive(Node,{node, Node, _, {node, NodeL, _,_,_}, {node, nil}}, Parent) ->
    [NodeL, Parent];

getNeighbors_recurive(_,{node, _, _, {node,nil}, {node, nil}}, _) ->
    [];

getNeighbors_recurive(Node,{node, N, _, {node, NodeL, RankL , LL, LR}, {node, nil}}, _) ->
    getNeighbors_recurive(Node,{node, NodeL, RankL , LL, LR},N);

getNeighbors_recurive(Node,{node, N, _,{node, nil}, {node, NodeR, RankR , RL, RR} }, _) ->
    getNeighbors_recurive(Node,{node, NodeR, RankR , RL, RR},N);

getNeighbors_recurive(Node,{node, N, _, {node, NodeL, RankL , LL, LR}, {node, NodeR, RankR, RL, RR}}, _) ->
    List1 = getNeighbors_recurive(Node,{node, NodeL, RankL , LL, LR},N),
    if List1 =/= [] ->
        List1;
    true ->
        getNeighbors_recurive(Node,{node, NodeR, RankR, RL, RR},N)
    end.

add(Pid,Node) -> 
    gen_server:call(Pid,{add, Node}).

getNeighbors(Pid,Node) ->
    gen_server:call(Pid,{getNeighbors, Node}). 

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
