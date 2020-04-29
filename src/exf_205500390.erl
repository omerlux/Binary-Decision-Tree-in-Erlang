%%%-------------------------------------------------------------------
%%% @author omerlux
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28 April 2019 12:19 PM
%%%-------------------------------------------------------------------
-module(exf_205500390). % TODO: Change name exf_205500390
-author("omerlux").

%%API
-export([exp_to_bdd/2, solve_bdd/2, randomBoolFunc/1]).
%%%-------------------------------------------------------------------
%% computeExp  - COMPUTES the EXPRESSION {Op, BF} when BF is bool function or a variable
computeExp({'not',Var}) when Var=:=1 -> 0;
computeExp({'not',Var}) when Var=:=0 -> 1;
computeExp({'not',BF}) -> BF_ans = computeExp(BF), % computing BF first
  case BF_ans of
    BF -> {'not',BF};               % there is nothing to change if the BF_ans=BF
    _ -> computeExp({'not',BF_ans})    % BF isn't a variable now
  end;
computeExp({Op,Tuple}) ->  % Op is or/and
  case Op of
    'or' ->   % defining the or expression
      case Tuple of
        {A,B} when A=:=1 orelse B=:=1 -> 1;
        {0,0} -> 0;
        {A,B} when A=:=0 -> computeExp(B);
        {A,B} when B=:=0 -> computeExp(A);
        {A,B} when A=:=B -> computeExp(A);
        {A,B} -> A_ans = computeExp(A), B_ans = computeExp(B), % computing A and B first
          case {A_ans,B_ans} of
            {A,B} -> {'or',{A,B}};                % the answer did not change, cannot minimize farther
            {A,_} -> computeExp({'or',{A,B_ans}});   % B has changed - try minimize it
            {_,B} -> computeExp({'or',{A_ans,B}});   % A has changed - try minimize it
            {_,_} -> computeExp({'or',{A_ans,B_ans}})% both changed - minimize
          end
      end;
    'and' ->   % defining the and experession
      case Tuple of
        {A,B} when A=:=0 orelse B=:=0 -> 0;
        {1,1} -> 1;
        {A,B} when A=:=1 -> computeExp(B);
        {A,B} when B=:=1 -> computeExp(A);
        {A,B} when A=:=B -> computeExp(A);
        {A,B} -> A_ans = computeExp(A), B_ans = computeExp(B), % computing A and B before
          case {A_ans,B_ans} of
            {A,B} -> {'and',{A,B}};                % the answer did not change, cannot minimize farther
            {A,_} -> computeExp({'and',{A,B_ans}});   % B has changed - try minimize it
            {_,B} -> computeExp({'and',{A_ans,B}});   % A has changed - try minimize it
            {_,_} -> computeExp({'and',{A_ans,B_ans}})% both changed - minimize
          end
      end
  end;
computeExp(BF) -> BF.        % when there are only variables in the expression - cannot minimize


%% assignVar - return a tuple {A,B} where all VAR will CHANGE into the VALUE
assignVar(Var,Value,Var) -> Value;                                                 % only a variable
assignVar(_,_,Other) when not is_tuple(Other), not is_list(Other) -> Other;        % assign only for lists and tuples
assignVar(Var,Value,{A,B}) -> {assignVar(Var,Value,A),assignVar(Var,Value,B)};     % tuple
assignVar(_,_,[]) -> [];                                                           % empty list
assignVar(Var,Value,[H|T]) -> [assignVar(Var,Value,H)|assignVar(Var,Value,T)].     % list of tuples


%% exp2Tree - returns a binary tree using tuples from the boolean function given
% each node will be from the form { {Value,Left,Right}, #Height, #Nodes, #Leaves}
exp2Tree({BF},_) when is_number(BF) -> {BF,0,0,0};   % BF is a number (1 value tree)
exp2Tree(BF,_) when is_number(BF) -> {BF,0,0,0};     % BF is a number
exp2Tree(BF,[]) -> {BF,0,0,0};                       % BF is a leaf
exp2Tree(BF,[H|T]) ->                                % H will be the next node, wil spread to 0 or 1
  createNode(
    H,                                               % Node Value
    exp2Tree( computeExp( assignVar(H,0,BF) ),T ),   % Left Node - replacing H with 0 and minimizing
    exp2Tree( computeExp( assignVar(H,1,BF) ),T )    % Right Node - replacing H with 1 and minimizing
  ).

%% createNode - creates a node in a tree - { {Value,Left,Right}, #Height, #Nodes, #Leaves}
% * Left and Right can be the numbers 0 or 1
% ** element (N,Tuple) = The method returns the Nth element in the tuple.
createNode(_,{Value,_,_,_},{Value,_,_,_}) when is_number(Value) -> {Value,0,0,0}; %cmnt below
      % 2 Nodes are 1 or 0, we can minimize the tree
createNode(Value,{L,_,_,_},{R,_,_,_}) when is_number(L) and is_number(R) -> {{Value,L,R},0,1,1}; %cmnt below
      % 2 Nodes are 0/1, creating a node with values corresponding # values
createNode(_,Node,Node) -> Node; % both sons are the same tree (not number), replacing current node with one of them
createNode(Val,L,R) -> {
  {Val,element(1,L),element(1,R)},      % the {Value,Left,Right} tuple
  1 + max(element(2,L),element(2,R)),   % summing the heights
  1 + element(3,L) + element(3,R),      % summing the nodes
  element(4,L) + element(4,L)           % summing the leaves
}.


%% getVars - returns the Xi variable in a list
getVars(BoolFunc) -> cleanDuplicates( getVars(BoolFunc,[]) ).               % getting vars and cleaning duplicates
getVars(BoolFunc,List) ->
  case BoolFunc of
    {'not', Var} when not is_tuple(Var) -> [Var|List];                       % Var will be variable
    {'not', BF} -> getVars(BF,List);                                         % BF will be bool func
    {_, {BF1, BF2}} when is_tuple(BF1) -> getVars(BF1,List)++getVars(BF2,[]); % BF1,2 will be a bool func (_ is the operator)
    {_, {Var1, BF2}} when is_tuple(BF2) -> getVars(BF2,[Var1|List]);          % BF2 is bool func, Var1 is variable
    {_, {Var1, Var2}} -> [Var1|[Var2|List]];                                  % Var1, Var2 are variables
    {Var} when is_number(Var) -> [];                                        % don't need numbers
    Var -> [Var|List]                                                       % last variable can be alone i.e. {'not',x1}
  end.


%% cleanDuplicates - returns a clean list with only 1 rehearsal
cleanDuplicates([]) -> [];
cleanDuplicates([H|T]) -> [H| [X || X<-cleanDuplicates(T), X=/=H] ]. % clean X which already in the list


%% listPerms - returns all the list permutations (for lists which are like [x1,x2...xn])
listPerms([]) -> [[]];
listPerms(L) -> [[H|T] || H<-L, T<-listPerms(L--[H])]. % every element in list will be the head and so on


%%% MAIN FUNCTION:
%% exp_to_bdd - The function receives a Boolean function and returns the corresponding BDD tree
% representation for that Boolean function, by the ordering it'll chose the most efficient one
exp_to_bdd(BoolFunc, Ordering) ->
  Start = os:timestamp(), % saving time
  case Ordering of
    tree_height ->   % 2 is the place of the height
      Best_BDD = getMinOrder( BoolFunc, 2), % get the best bdd, from all kinds of bdds by method 'Ordering'
      Error = 0;
    num_of_nodes ->  % 3 is the place of the number of nodes
      Best_BDD = getMinOrder( BoolFunc, 3),
      Error = 0;
    num_of_leafs ->  % 4 is the place of the number of leaves
      Best_BDD = getMinOrder( BoolFunc, 4),
      Error = 0;
    _ -> Best_BDD = Ordering,
      Error =1
  end,
  case Error of
    0 -> io:format("Total time taken: ~f milliseconds~nAnd the answer is: ",
      [timer:now_diff(os:timestamp(), Start) / math:pow(10,3)]);
    1 -> io:fwrite("WRONG ORDERING METHOD!~nGiven the corresponding value: ")
  end,
  Best_BDD.


%% getAllBdds - returning all bdds by all the permutations of the variables in BoolFunc
getAllBdds(BoolFunc, PermList) -> getAllBdds(BoolFunc, PermList, []). % collecting the bdds into list
getAllBdds(_, [], All_BDDs) -> All_BDDs;                                                            % no more permutations to make
getAllBdds({BoolFunc}, _, []) when is_number(BoolFunc) -> {BoolFunc};                               % only 1 node...
getAllBdds(BoolFunc, [H|T], All_BDDs) -> getAllBdds(BoolFunc, T, [exp2Tree(BoolFunc, H)|All_BDDs]). % adding new bdd to the list


%% getMinOrder - choosing the best bdd by 'Ordering' method (Ordering is a number now)
% * { {Left,Value,Right}, #Height, #Nodes, #Leaves}
getMinOrder(BoolFunc,Ordering) when is_tuple(BoolFunc) -> % first get all the permutations
  getMinOrder( getAllBdds(BoolFunc,listPerms(getVars(BoolFunc))) , Ordering,[]).
getMinOrder([H|T],Order,[]) -> getMinOrder(T,Order,H);                           % first bdd for reference
getMinOrder([],_,BestBDD) -> element(1,BestBDD);                                 % returns the list which is the BDD
getMinOrder([H|T],Order,BestBDD) when element(Order,H)<element(Order,BestBDD) -> % there is a better BDD which is H
      getMinOrder([T],Order,H);
getMinOrder([_|T],Order,BestBDD) -> getMinOrder(T,Order,BestBDD).                % the head isn't better than BestBDD


%%% MAIN FUNCTION:
%% solve_bdd - solving bdd tree with the variables it got assigned
solve_bdd(BddTree,Assign) ->
  Start = os:timestamp(), % saving time
  FixedAssign = assignVar(true,1,assignVar(false,0,Assign)),    % changing true and false to 1,0
  Bdd_solution = solveIt(BddTree,FixedAssign),                  % solving...
  io:format("Total time taken: ~f milliseconds~n", [timer:now_diff(os:timestamp(), Start) / math:pow(10,3)]),
  Bdd_solution.


%% solveIt - returns a solution, for a BddTree, with the list of value assignments
% * lists:keyfind(X,1,List) - returns the tuple which has the variable X
solveIt({Var,Left,Right},Assigns) ->
  VarTuple = lists:keyfind(Var,1,Assigns),  % finding the relevant assignment
  case is_tuple(VarTuple) of
    true -> case element(2, VarTuple) of    % Var has assignment
              1 -> solveIt(Right,Assigns);  % 1 = right side of the tree
              0 -> solveIt(Left,Assigns)    % 0 = left side of the tree
            end;
    false ->  % no special assignment for Var - continue to the sons
      NewLeft = solveIt(Left,Assigns), NewRight = solveIt(Right,Assigns),
            case {NewLeft,NewRight} of
              {Node,Node} -> Node;               % both sub-trees are identical - minimizing the tree
              {_,_} -> {Var,NewLeft,NewRight}    % different, create the tree again
            end
  end;
solveIt(BddTree,_) -> BddTree.              % no more assignments could happen / this is a number already


%%% EXTRAS
%% randomBoolFunc - creating a random boolean function with max recursive clause N
randomBoolFunc(0) -> randomVar();   % do not continue!
randomBoolFunc(N) ->                % N is the maximum recursive clause
  case rand:uniform(16) of
    1 -> {randomOp(), randomBoolFunc(N-1),randomBoolFunc(N-1)};
    2 -> {randomOp(), randomVar(),randomBoolFunc(N-1)};
    3 -> {randomOp(), randomBoolFunc(N-1), randomVar()};
    4 -> {randomOp(), randomVar(), randomVar()};

    5 -> {randomOp(), {'not',randomBoolFunc(N-1)},randomBoolFunc(N-1)};
    6 -> {randomOp(), {'not',randomVar()},randomBoolFunc(N-1)};
    7 -> {randomOp(), {'not',randomBoolFunc(N-1)}, randomVar()};
    8 -> {randomOp(), {'not',randomVar()}, randomVar()};

    9 -> {randomOp(), randomBoolFunc(N-1),{'not',randomBoolFunc(N-1)}};
    10 -> {randomOp(), randomVar(),{'not',randomBoolFunc(N-1)}};
    11 -> {randomOp(), randomBoolFunc(N-1), {'not',randomVar()}};
    12 -> {randomOp(), randomVar(), {'not',randomVar()}};

    13 -> {randomOp(), {'not',randomBoolFunc(N-1)},{'not',randomBoolFunc(N-1)}};
    14 -> {randomOp(), {'not',randomVar()},{'not',randomBoolFunc(N-1)}};
    15 -> {randomOp(), {'not',randomBoolFunc(N-1)}, {'not',randomVar()}};
    16 -> {randomOp(), {'not',randomVar()}, {'not',randomVar()}}
  end.

%% randomVar - randomize a variable
randomVar() ->
  case rand:uniform(4) of
    1 -> x1;
    2 -> x2;
    3 -> x3;
    4 -> x4
  end.

%% randomVar - randomize a variable
randomOp() ->
  case rand:uniform(2) of
    1 -> 'and';
    2 -> 'or'
  end.
