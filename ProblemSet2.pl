%Rajan Jassal 1369879
:- use_module(library(clpfd)).

%Question 1
%This function will to clpfd to find the variables S1,S2,S3,S4 that would be the squares that make up the natural number N
%The range for the variables is set to be 0 to N so clpfd knows what range of numbers the answer can be in. Then it is left
%to clpfd to figure out the solution(s). Note this list of squares will be returned lowest to highest in the list
fourSquares(N, [S1,S2,S3,S4]) :- Vars = [S1,S2,S3,S4], Vars ins 0..N , N #= S1*S1 + S2*S2 + S3*S3 + S4*S4, S1 #=< S2, S2 #=< S3, S3 #=< S4, label(Vars).

%Question 2
%The disarm function will take a list L1 and L2 and see if there is a disarm solution available(as specified in the questions)
disarm([],[],_).


%For a non empty case disarm will pass the variables it is given into disarm2. The Solution variable is put in the place where disarm2 
%would return the correct solution so it will represent the answer. It will also give disarm2 two empty lists
%as disarm will need to append to these values in order keep a temporary list it passes along filled with the partial solution
%Please also note that the cut at the end will mean only one solution is returned. The program is able to find all solutions without
%the cut but duplicates (with different orientations) will be returned and the the specification only states only the first solution matters
disarm(L1,L2,Solution) :- disarm2(L1,L2,[],[], Solution), !.

%This will be the end case for the recursion of disarm2. If both lists are empty then disarm2 was able to 
%find a solution(which will be in the passed down Solution variable). It will put this solution in ReturnedList.
disarm2([],[], Solution,_,ReturnedList):- ReturnedList = Solution,!.

%This is the main function of disarm2 using regular prolog. The general idea of this function is that there are two cases for each
%valid solution element: Either one element of L1 and 2 elements of L2 are taken out or two elements of L2 and one of L1 are taken out
% These two cases are separated by the or in the middle of the function to check if either is true at a given state
%The clause before the or(;) is the case two elements of L1 are taken out and one of L2 is taken out. To do this first all the subsets of 
%L1 are found that are of size 2 and only the subsets that add to one element of L2 are kept. This sum also has to be less then the old value which 
%will be passed in the recursion(e.g. the past sum that was taken as a element of the solution). Now that the values from L1 and L2 are possible solutions
%they will be taken out of L1 and L2(with deleteFirstElement function) and appended to the current passed down solution. This new partially complete solution and the new L1 and L2's (without the soltion elements)
%will be passed into disarm2 again to see if more of a solution can be created. They key factor is that if both L1 and L2 are passed in empty that means
%that all their elements were able to be put into the Solution so a valid solution should exist. Note the case after the or is the case of L2 being two elements
%and L1 being one so the two statements are the same with L1 and L2 switched
%
%This function should try to build a solution list and if it ever fails it would go back to a previous function call and try a different combination of 
%L1 and L2 to make a solution. This should repeat for all possible combinations of L1 and L2 until either a solution is reached else false is returned  
disarm2(L1,L2, Solution,OldValue,ReturnedList) :- (xsubset(X,L1), length(X, SubsetSize), SubsetSize=2, member(X2,L2), sumlist(X,L1Sum),
									L1Sum = X2 ,(OldValue = [] ; L1Sum @=< OldValue ), nth0(0,X,FirstElementL1), nth0(1,X,SecondElementL1),
									deleteFirstElement(FirstElementL1,L1,TempL1), deleteFirstElement(SecondElementL1,TempL1,NewL1),
									deleteFirstElement(X2,L2,NewL2), NewSolution = [[X|[[X2]]]|Solution], disarm2(NewL1,NewL2,NewSolution,L1Sum,ReturnedList))
									;
									(xsubset(Subsets2,L2), length(Subsets2, SubsetSize2), SubsetSize2=2, member(Member2,L1), sumlist(Subsets2,L2Sum), L2Sum = Member2,  
									(OldValue = [] ; L2Sum @=< OldValue ), nth0(0,Subsets2,FirstElementL2), nth0(1,Subsets2,SecondElementL2), deleteFirstElement(FirstElementL2,L2,TempL2),
									 deleteFirstElement(SecondElementL2,TempL2,NewL2), deleteFirstElement(Member2,L1,NewL1), NewSolution = [[[Member2]|[Subsets2]]|Solution], 
									 disarm2(NewL1,NewL2,NewSolution,L2Sum,ReturnedList)).

%This us a helper function for disarm2. It will remove the element Element (first argument) only once 
%from the list passed in as the second argument. This new list with the element removed once will be placed in 
%the third argument of the function 
deleteFirstElement(_, [], []).
deleteFirstElement(Element,[Element|T],T) :- !.
deleteFirstElement(Element,[H|T],[H|ReturnedList]) :- deleteFirstElement(Element,T,ReturnedList).

%Functions given in assignment 4
xsubset([], _).
xsubset([X|Xs], Set) :-
  xappend(_, [X|Set1], Set),
  xsubset(Xs, Set1).

xappend([], L, L).
xappend([H|T], L, [H|R]) :-
  xappend(T, L, R).

test(L,L1,X) :- X = [L|L1].
