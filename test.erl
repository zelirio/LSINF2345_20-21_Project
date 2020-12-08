- module(test).
- export([testTree/0]).
- include_lib("eunit/include/eunit.hrl").

testTree() ->
    {ok, TreePid} = binaryTreeServer:start_link(test),
    ?assertEqual([],binaryTreeServer:getNeighbors(TreePid,0)), % Tree is empty.
    ?assertEqual({node,0,1,{node,nil},{node,nil}},binaryTreeServer:add(TreePid, 0)), % add first node.
    ?assertEqual([],binaryTreeServer:getNeighbors(TreePid,0)), % get neighbors of the root.
    ?assertEqual({node,0,2,{node,1,1,{node, nil}, {node,nil}},{node,nil}},binaryTreeServer:add(TreePid, 1)), % add a second node.
    ?assertEqual([0],binaryTreeServer:getNeighbors(TreePid,1)), % node knows its parent.
    binaryTreeServer:add(TreePid,2),
    binaryTreeServer:add(TreePid,3),
    binaryTreeServer:add(TreePid,4),
    binaryTreeServer:add(TreePid,5),
    binaryTreeServer:add(TreePid,6),
    binaryTreeServer:add(TreePid,7),
    binaryTreeServer:add(TreePid,8),
    ?assertEqual(
    {node,0,10,
      {node,1,4,
            {node,4,1,{node,nil},{node,nil}},
            {node,6,2,{node,8,1,{node,nil},{node,nil}},{node,nil}}},
      {node,2,5,
            {node,3,2,{node,9,1,{node,nil},{node,nil}},{node,nil}},
            {node,5,2,{node,7,1,{node,nil},{node,nil}},{node,nil}}}},binaryTreeServer:add(TreePid,9)), % Tree is balanced.
    ?assertEqual([1,2],binaryTreeServer:getNeighbors(TreePid,0)), % Neighbors of the root.
    ?assertEqual([3],binaryTreeServer:getNeighbors(TreePid,9)), % Neighbors of a leaf.
    ?assertEqual([3,5,0],binaryTreeServer:getNeighbors(TreePid,2)), % Neighbors of a node in the middle.
    ?assertEqual([],binaryTreeServer:getNeighbors(TreePid,10)). % Neighbors of a node not in the tree.
