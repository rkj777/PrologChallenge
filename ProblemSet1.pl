%Rajan Jassal 1369879

%Question 1
%Preicates for xreverse
%The case of reversing a empty list 
xreverse([],[]).

%Starting the process of reversing two lists by passing the lists to
%a helper function with an empty list to store the result as the list
%is reversed
xreverse(L1,L2) :- xreverse1(L1,[],L2).

%The case to end the the recursion of the helper function 
%and placed the passed along list into the answer
xreverse1([],L2,L2).

%Helper function that will move along the passed in list and
%store the elements in a passed down list which will result in the 
%correct answer after going through the elements of the original list
xreverse1([H1|T1],PassedDownList,ReversedList) :-  xreverse1(T1,[H1|PassedDownList], ReversedList).


%Question 2
%Predicates for xunique

%Handling the case of an empty list 
xunique([],[]).

%If there is a match of elements (in the correct order) between list
%one and list two then all the duplicate elements will be removed from list one.
%This resulting list will be passed to xunique to see if the next non duplicate element in 
%list one matches the next element in list two
xunique([H1|T1], [H2|T2]) :- H1 = H2, removeElement(H1, T1, Z), xunique(Z,T2).

%Remove helper functions for xunique. Removes the duplicates element X in the list
%passed in as the second argument and puts the resulting list with no items X in Z
removeElement(_, [],[]).
removeElement(X, [H1|T1], Z) :- X == H1, removeElement(X,T1,Z).
removeElement(X, [H1|T1], Z) :- X \== H1 ,removeElement(X,T1,NewList), Z = [H1|NewList].


%Question 3
%Predicates for xunion

%Will combine the two lists(L1 and L2) and remove duplicate elements (in the order of L1 and L2) and place the list in L.
% To start the two lists L1 and L2 will be combined (in the order L1 and L2). Then all duplicates will be removed with xunique 
xunion(L1,L2,L) :- append(L1,L2, Combined) , xunique(Combined,L). 


%Question 4
%Predicates for remove last 

%Remove last will take L and remove the last element resulting in the list
%L1. The last element will be put in Last. 
%This function works by calling two helper functions. One will find the last element
% of L and the second will find the list L with the last element removed
removeLast([],[],[]).
removeLast(L,L1,Last) :- lastElement(L,Last), lastRemoved(L, L1).

%LastElement will find the last element of a list passed in and putting it in Y
lastElement([], []).

%This function will check if the end of a list has been reached. If not it will 
%pop off the first element and pass the list with one less element into lastElement again 
lastElement([H|T], Y) :- T \== [] , lastElement(T,Y).

%If the list has only one element left that element will be returned as the last element
lastElement([H|T2], Y) :- T2==[], H=Y.

%lastRemoved will take in a list (First element) and return a list with the last element removed 
lastRemoved([],[]).
%This function will use the helper function lastRemoved2 that will build a list without the last element.
%This will need to be reversed in order to be the correct list so reverse1 is called
lastRemoved([H|T], ResultList) :- lastRemoved2([H|T],[] , TemporaryList), xreverse(TemporaryList,ResultList). 

%These functions will go down the list and add each element to the result list except the last element 
lastRemoved2([H|T], Members, ResultList) :- T \==[], lastRemoved2(T, [H|Members],ResultList).
lastRemoved2([H|T], Members, ResultList) :- T == [], ResultList=Members.


node(a).
node(b).
node(c).
node(d).
node(e).

edge(a,b).
edge(b,c).
edge(c,a).
edge(d,a).
edge(a,e).

%Qustion 5
%Part1
%Clique L will find all the cliques of the graph and put them in L
%To do this it will find all the nodes and subsets and see which are cliques through allConnected
clique(L) :- findall(X,node(X),Nodes),
             xsubset(L,Nodes), allConnected(L).

%All connected will check if nodes all connected to all other nodes in the list
%This is done by cycling through the nodes in connect
allConnected([]).
allConnected([H|T]) :- connect(H,T) , allConnected(T).

%connect will check to see if a node A is connected to all other nodes in the given 
%list (second argument). This is done by checking to see if there is an edge between the first node
%in the list and then checking the rest of the list recursively. Terminates when no more nodes to check against 
connect(A, []).
connect(A,[H|T]) :- (edge(A,H) ; edge(H,A)), connect(A,T).

%Part B

%maxclique will find the maxclique of a given size N that isn't a subset of a bigger clique
%NSizedClique and greaterThanNSizedCliques functions will get all the cliques that are of size n and size > n
%and pass these lists onto removeSubSetCliques which will remove cliques of N size that are a subset of a clique of
%greater that n size  
maxclique(N, Cliques):- nSizedCliques(N,NSizedList), greaterThanNSizedCliques(N,GNSizedList), removeSubSetCliques(NSizedList,GNSizedList,Cliques).

%removeSubSetCliques will get rid of all the cliques from the N sized list of cliques that are SubSets of cliques greater in size then them. It will
%take in a list of cliques that are n sized and a list that are >N sized. It will remove any elements form the nSizedCliques that are SubSets of the 
%>N sized cliques by recursively going through the >N sized cliques
removeSubSetCliques(NSizedList,[], ResultList) :- ResultList = NSizedList.
removeSubSetCliques(NSizedList,[H1|T1], ResultList) :- findall(X, xsubset(X,H1),TemporaryList), removeMember(NSizedList,TemporaryList,[],NewNsizedList), 
					removeSubSetCliques(NewNsizedList,T1,ResultList).

%removeMember will remove anything that is a member of of the subset list passed and return the initially passed in list without
%any members  of the subset list passed in 
removeMember([], SubSets, PassedDownList, ReturnedList) :- ReturnedList = PassedDownList.
removeMember([H1|T1], SubSets, PassedDownList, ReturnedList) :- member(H1,SubSets), removeMember(T1, SubSets, PassedDownList, ReturnedList).
removeMember([H1|T1], SubSets, PassedDownList, ReturnedList) :- \+member(H1,SubSets),append([H1],PassedDownList,SubList), removeMember(T1, SubSets, SubList, ReturnedList).

%nSizedCliques will return all the cliques that are of size N in one list (NsizedList)
%It will first  find all the cliques and pass them to removeNotEqual which will remove all the cliques not equal to size N
%This will be returned in NsizedList
nSizedCliques(N,NSizedList) :- findall(X, clique(X), TemporaryList), removeNotEqual(TemporaryList,N,[],NSizedList).

%removeNotEqual will remove all the cliques not equal to size N. To do this it will recursively go down the 
%list of all cliques passed in and only put the cliques of size N into PassedDownList. The termination case of no
%cliques left will cause the pass down list with only N sized cliques to be given to the result list.
removeNotEqual([],N,PassedDownList,ResultList) :- ResultList = PassedDownList.

%If the clique size is N then it will be appended to PassedDownList
removeNotEqual([H1|T1],N,PassedDownList, ResultList) :- length(H1, ListSize), ListSize = N,
				 append([H1],PassedDownList,SubList), removeNotEqual(T1,N,SubList,ResultList).
%If the clique size is not N then the rest of the list will be checked
removeNotEqual([H1|T1],N,PassedDownList, ResultList) :- length(H1, ListSize), ListSize \= N, removeNotEqual(T1,N,PassedDownList,ResultList).


%removeNotEqual will remove all the cliques not greater than size N. To do this it will recursively go down the 
%list of all cliques passed in and only put the cliques of size > N into PassedDownList. The termination case of no
%cliques left will cause the pass down list with only >N sized cliques to be given to the result list.
greaterThanNSizedCliques(N,GNSizedList) :- findall(X, clique(X), TemporaryList), removeNotGreater(TemporaryList,N,[],GNSizedList).
removeNotGreater([],N,PassedDownList,ResultList) :- ResultList = PassedDownList.
%If the clique size is >N then it will be appended to PassedDownList
removeNotGreater([H1|T1],N,PassedDownList, ResultList) :- length(H1, ListSize), ListSize > N,
				 append([H1],PassedDownList,SubList), removeNotGreater(T1,N,SubList,ResultList).
%If the clique size is not N then the rest of the list will be checked
removeNotGreater([H1|T1],N,PassedDownList, ResultList) :- length(H1, ListSize), (ListSize < N ; ListSize = N), removeNotGreater(T1,N,PassedDownList,ResultList).


%Functions given to be used by problem 5
xsubset([], _).
xsubset([X|Xs], Set) :-
  xappend(_, [X|Set1], Set),
  xsubset(Xs, Set1).

xappend([], L, L).
xappend([H|T], L, [H|R]) :-
  xappend(T, L, R).


