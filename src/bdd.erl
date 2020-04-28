%%%-------------------------------------------------------------------
%%% @author omerlux
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28 April 2019 12:19 PM
%%%-------------------------------------------------------------------
-module(bdd). % TODO: Change name exf_205500390
-author("omerlux").

%%API
-export([getVars/1,compExp/1,assignVar/3]).
%%%-------------------------------------------------------------------
%% compExp  - computes the expression {Op, BF} when BF is bool function or a variable
compExp({'not',Var}) when Var=:=1 -> 0;
compExp({'not',Var}) when Var=:=0 -> 1;
compExp({'not',BF}) -> compExp({'not',compExp(BF)});    %BF isn't a variable now
compExp({Op,Tuple}) ->  % Op is or/and
  case Op of
    'or' ->   % defining the or expression
      case Tuple of
        {A,B} when A=:=1 orelse B=:=1 -> 1;
        {0,0} -> 0;
        {A,B} when A=:=0 -> compExp(B);
        {A,B} when B=:=0 -> compExp(A);
        {A,B} when A=:=B -> compExp(A);
        {A,B} -> compExp({'or',{compExp(A),compExp(B)}}) % they are both bool func - recursive result
      end;
    'and' ->   % defining the and experession
      case Tuple of
        {A,B} when A=:=0 orelse B=:=0 -> 0;
        {1,1} -> 1;
        {A,B} when A=:=1 -> compExp(B);
        {A,B} when B=:=1 -> compExp(A);
        {A,B} when A=:=B -> compExp(A);
        {A,B} -> compExp({'or',{compExp(A),compExp(B)}}) % they are both bool func - recursive result
      end
  end;
compExp({Var}) -> Var.    % for a single variable


%% assignVar - return a tuple {A,B} where all Var will change into the Value
assignVar(Var,Value,Var) -> Value;
assignVar(_,_,Other) when not is_tuple(Other) -> Other;
assignVar(Var,Value,{A,B}) -> {assignVar(Var,Value,A),assignVar(Var,Value,B)}.


%% expToThree - returns a binary three using tuples from the boolean function given
% each node will be from the form { {Left/Value/Right}, #Height, #Nodes, #Leaves}
% Node = {Val}, {Left,Right}, {Left/Right, {Val}
expToThree()


%% getVars - returns the X variable of the list
getVars(BoolFunc) -> cleanDuplicates( getVars(BoolFunc,[]) ).               % getting vars and cleaning duplicates
getVars(BoolFunc,List) ->
  case BoolFunc of
    {'not',Var} when not is_tuple(Var) -> [Var|List];                       % Var will be variable
    {'not',BF} -> getVars(BF,List);                                         % BF will be bool func
    {_,{BF1,BF2}} when is_tuple(BF1) -> getVars(BF1,List)++getVars(BF2,[]); % BF1,2 will be a bool func (_ is the operator)
    {_,{Var1,BF2}} when is_tuple(BF2) -> getVars(BF2,[Var1|List]);          % BF2 is bool func, Var1 is variable
    {_,{Var1,Var2}} -> [Var1|[Var2|List]];                                  % Var1, Var2 are variables
    Var -> [Var|List]                                                       % last variable can be alone i.e. {'not',x1}
  end.


%% cleanDuplicates - returns a clean list with only 1 rehearsal
cleanDuplicates([]) -> [];
cleanDuplicates([H|T]) -> [H| [X || X<-cleanDuplicates(T), X=/=H] ]. % clean X which already in the list


%% listPerms - returns all the list permutations (for lists which are like [x1,x2...xn])
listPerms([]) -> [[]];
listPerms(L) -> [[H|T] || H<-L, T<-listPerms(L--[H])]. % every element in list will be the head and so on



