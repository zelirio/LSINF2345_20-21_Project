-module(binaryTreeServer).
-export[add/2,getNeighbors/2,start_link/1].
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {root}). % State with the root of the tree.
start(Name) ->
    gen_server:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

% Start the server with an empty root.
init(_Args) ->
    {ok, #state{root={node,nil}}}.

% When add is called : compute the new state, update it and send it back.
handle_call({add, Node}, _From, State) ->
    NewState = add_recursive(Node,State#state.root),
    {reply,NewState,State#state{root=NewState}};

% When getNeighbors is called : reply with the neighbors, state remains the same.
handle_call({getNeighbors, Node}, _From, State) ->
    {reply,getNeighbors_recursive(Node,State#state.root),State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

% When on a leaf, add a binary node with rank 1.
add_recursive(Node, {node,nil}) ->
    {node,Node,1,{node,nil},{node,nil}};

% If left is a leaf, add node on the left of current node.
add_recursive(Node, {node, Root, Rank, {node,nil}, R}) ->
    {node, Root, Rank+1,{node, Node, 1, {node,nil}, {node,nil}}, R};

% If right is a leaf, add node on the right of current node.
add_recursive(Node, {node, Root, Rank, L, {node,nil}}) ->
    {node, Root, Rank+1,L ,{node, Node, 1, {node,nil},{node,nil}}};

% If neither sub-tree is a leaf, add 1 to current rank and add the node to the sub-tree with the lowest rank (right if equal).
add_recursive(Node, {node, Root, RankRoot, {node, NodeL, RankL,LL,RL}, {node, NodeR, RankR,LR,RR}}) ->
    if
    RankL < RankR ->
        {node, Root, RankRoot+1,add_recursive(Node,{node,NodeL,RankL,LL,RL}), {node, NodeR, RankR,LR,RR}};
    true ->
        {node, Root, RankRoot+1, {node, NodeL, RankL,LL,RL},add_recursive(Node,{node,NodeR,RankR,LR,RR})}
    end.

% If tree is empty: returns nothing.
getNeighbors_recursive(_, {node, nil}) ->
    [];

% Wanted node is at the root without sub-trees: returns nothing.
getNeighbors_recursive(Node, {node, Node, _, {node, nil}, {node, nil}}) ->
    [];

% Wanted node is at the root and left node is empty: returns left sub-tree.
getNeighbors_recursive(Node, {node, Node, _, {node, nil}, {node, NodeR, _,_,_}}) ->
    [NodeR];

% Wanted node is at the root and right node is empty: returns right sub-tree.
getNeighbors_recursive(Node, {node, Node, _, {node, NodeL, _,_,_}, {node, nil}}) ->
    [NodeL];

% Wanted node is at the root: neighbors are the two sub-trees.
getNeighbors_recursive(Node, {node, Node, _, {node, NodeL, _,_,_}, {node, NodeR, _,_,_}}) ->
    [NodeL,NodeR];

% Start recursion on the root of the tree with the root as parent.
getNeighbors_recursive(Node, Root) ->
    getNeighbors_recursive(Node, Root , Root).

% If on a leaf: return nothing.
getNeighbors_recursive(_,{node,nil} , _) ->
    [];

% Node is found without sub-trees: return parent node.
getNeighbors_recursive(Node,{node, Node, _, {node,nil}, {node, nil}}, Parent) ->
    [Parent];

% Node not found: return nothing.
getNeighbors_recursive(_,{node, _, _, {node,nil}, {node, nil}}, _) ->
    [];

% Node is found with 2 sub-trees: return both + parent node.
getNeighbors_recursive(Node,{node, Node, _, {node, NodeL, _,_,_}, {node, NodeR, _,_,_}}, Parent) ->
    [NodeL, NodeR, Parent];

% Node is found without left sub-tree: return right + parent node.
getNeighbors_recursive(Node,{node, Node, _, {node, nil}, {node, NodeR, _,_,_}}, Parent) ->
    [NodeR, Parent];

% Node is found without right sub-tree: return left + parent node.
getNeighbors_recursive(Node,{node, Node, _, {node, NodeL, _,_,_}, {node, nil}}, Parent) ->
    [NodeL, Parent];

% Not on the wanted node and no right sub-tree: look in left sub-tree.
getNeighbors_recursive(Node,{node, N, _, {node, NodeL, RankL , LL, LR}, {node, nil}}, _) ->
    getNeighbors_recursive(Node,{node, NodeL, RankL , LL, LR},N);

% Not on the wanted node and no left sub-tree: look in right sub-tree.
getNeighbors_recursive(Node,{node, N, _,{node, nil}, {node, NodeR, RankR , RL, RR} }, _) ->
    getNeighbors_recursive(Node,{node, NodeR, RankR , RL, RR},N);

% Not on the wanted node: look in left sub-tree and if no result look in right sub-tree.
getNeighbors_recursive(Node,{node, N, _, {node, NodeL, RankL , LL, LR}, {node, NodeR, RankR, RL, RR}}, _) ->
    List1 = getNeighbors_recursive(Node,{node, NodeL, RankL , LL, LR},N),
    if List1 =/= [] ->  % Node not found in left sub-tree.
        List1;
    true ->
        getNeighbors_recursive(Node,{node, NodeR, RankR, RL, RR},N)
    end.

add(Pid,Node) ->
    gen_server:call(Pid,{add, Node}).

getNeighbors(Pid,Node) ->
    gen_server:call(Pid,{getNeighbors, Node}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
