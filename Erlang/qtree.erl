-module(qtree).
-compile(export_all).
%%% @author Allen McPherson <mcpherson@lanl.gov>
%%%  [http://losalamosal.me]
%%% @doc Quadtree operations--test with line/box intersection
%%% @end

-define(EMPTY_NODE, #node{}).
-define(UNIT_BOX, #box{xBL=0.0, yBL=0.0, xTR=1.0, yTR=1.0}).
-define(NEW_SEG(X1, Y1, X2, Y2), #seg{x1=X1, y1=Y1, x2=X2, y2=Y2}).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p:  ~p~n", 
                              [?MODULE, ?LINE, ??Var, Var])).
-endif.

%-type offset() :: byte().
%-type leaf() :: #leaf{}.
%-type inner() :: #inner{}.

%-type tree() :: leaf() | inner() | 'undefined'.
%-type key() :: binary().
%-type value() :: binary().
%-type path() :: binary().
%-type access_fun() :: fun((at | child_at | keys, path()) -> tree()).
%-type serial_fun() :: fun((at | child_at | keys, path()) -> binary()).

-record(box, {xBL, yBL, xTR, yTR}).
-record(seg, {x1, y1, x2, y2}).
-record(rect, {minX=0.0, minY=0.0, maxX=1.0, maxY=1.0}).
-record(segment, {x0=0.0, y0=0.0, x1=1.0, y1=1.0}).
-record(node, {geom=#rect{}, parent, nw, ne, se, sw}).
-record(nnode, {geom=#rect{}, parent=nil, kids=[]}).

%% ===================================================================
%% Public API
%% ===================================================================

%% ===================================================================
%% Internal functions
%% ===================================================================

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%

run() ->
    Seg = {1.1, 1.1, 1.9, 1.9},  %  endpoints
    Box = {0.0, 0.0, 1.0, 1.0},    %  BL, TR corners
    S = {seg, {1.1, 1.1, 1.9, 1.9}},  %  endpoints
    G = {geom, {0.0, 0.0, 1.0, 1.0}},    %  BL, TR corners
    N = {node, G, {parent, {nil}}, {children, {nil, nil, nil, nil}}},
    ?PRINT(N),
    BBox = ?UNIT_BOX,
    SSeg = ?NEW_SEG(-0.1, 0.1, 0.1, -0.1),
    R = #rect{minX=-1.0, minY=-1.0, maxX=1.0, maxY=1.0},
    Qtree = #node{geom=R},
    ?PRINT(Qtree),
    ?PRINT(R),
    updateTree(Qtree, Seg, 4),
    isIntersect(Seg, Box).


%% =====================================================================
%% XML-BASED LAYOUT ENGINE
%% =====================================================================

%%  Call initially with the head of a tree. Then recursively call
%%  on internal nodes.  Return new tree.  This is a SERIAL update.
updateTree(Node, _Segment, 0) ->
    Node;
updateTree(Node, Segment, Decomp) ->
    ?PRINT(Node#node.geom),
    isIntersect(Node#node.geom, Segment),
    %%  if segment intersects node
    %%    true
    %%      if leaf node
    %%        true  recurse
    %%        false continue
    %%    false
    %%      if leaf node
    %%        true  continue
    %%        false replace with new leaf node
    %%
    updateTree(?EMPTY_NODE, Segment, Decomp-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intersection testing %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  @ doc Returns true if the line segment intersects the rectangle.
%%  Implements segment/AABB intersection test from http://goo.gl/XU6n9N
%%  (user37968's answer).
isIntersect(Segment, Rectangle) ->
    {SX0, SY0, SX1, SY1} = Segment,
    {BX0, BY0, BX1, BY1} = Rectangle,
    io:format("~p, ~p, ~p, ~p~n", 
           [(SX0 > BX1) and (SX1 > BX1),
            (SX0 < BX0) and (SX1 < BX0),
            (SY0 > BY1) and (SY1 > BY1),
            (SY0 < BY0) and (SY1 < BY0)]),
    %  If any of these four lines are true there is NO intersection
    Check1 =
        if
            (SX0 > BX1) and (SX1 > BX1) -> false;
            (SX0 < BX0) and (SX1 < BX0) -> false;
            (SY0 > BY1) and (SY1 > BY1) -> false;
            (SY0 < BY0) and (SY1 < BY0) -> false;
            true -> true
        end,
    F = fun(X,Y) -> (SY1-SY0)*X + (SX0-SX1)*Y + (SX1*SY0-SX0*SY1) end,
    Res = [F(BX0,BY0), F(BX0,BY1), F(BX1,BY1), F(BX1,BY0)],
    io:format("res: ~p~n", [Res]),
    io:format("<0: ~p~n", [lists:all(fun(X) -> X < 0 end, Res)]),
    io:format(">0: ~p~n", [lists:all(fun(X) -> X > 0 end, Res)]),
    Check2 = lists:all(fun(X) -> X < 0 end, Res) and
             lists:all(fun(X) -> X > 0 end, Res),
    ?PRINT(Check1),
    ?PRINT(Check2),
    ?PRINT(Check1 or Check2),
    Check1 or Check2.


%%  Operations on the tree:
%%  - coarsen  (requires delete)
%%  - refine   (requires 'new', to a given 'depth')
%%  - traverse (concurrent; for scheduling, coarsen, refine)
init() ->
    #node{}.
 
loop() ->
    loop({-1.0, 1.0, 0.0, 0.0}, {0.0, 0.0, 1.0, 1.0}, 10).
loop(_Seg, _Box, 0) -> 
    ok;
loop(Seg, Box, N) ->
    io:format("~p: ~p~n", [N, isIntersect(Seg, Box)]),
    {SX0, SY0, SX1, SY1} = Seg,
    loop({SX0+0.1, SY0, SX1+0.1, SY1}, Box, N-1).

%-spec buildFull(n()) -> tree().
buildFull(N) ->
    Root = #node{},
    insert4(N, Root),
    Root.

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

insert4(0, _) ->
    done;
insert4(N, Node)->
    Node#node{nw = insert4(N-1, ?EMPTY_NODE)},
    Node#node{ne = insert4(N-1, ?EMPTY_NODE)},
    Node#node{se = insert4(N-1, ?EMPTY_NODE)},
    Node#node{sw = insert4(N-1, ?EMPTY_NODE)}.
    

    
    
