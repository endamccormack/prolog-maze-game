
:- dynamic connection/2, theGoal/1, numberedSquare/4.

%Broke it towards the end so didnt quite finish, ran out of time but prolog is heavy logic so hopefully
% mark it like maths. Majority of it is done

%making a seperate representation of the data around the connections, giving all items a number 
%then getting the tranversable members and giving them other tranversable connections, after that all
% you had to do is run it through any of the search algorithms breadthfirst as an example bellow
% giving it the starting point and its goal

buildConnections:-
  %get all squares and give them numbers so we can create connections
  setof([Row,Col,Item], square(Row,Col,Item,_), AllSquares),
  giveAllSquaresValues(AllSquares, 1),

  %get all the squares that you can travers through and create connections between them for search
  setof([Row,Col,Number], numberedSquare(Row,Col,h,Number),HotSquares),
  setof([Row,Col,Number], numberedSquare(Row,Col,e,Number),EmptySquares),
  setof([Row,Col,Number], numberedSquare(Row,Col,p,Number),PlayerSquare),
  setof([Row,Col,Number], numberedSquare(Row,Col,x,Number),ExitSquare),
  setof([Row,Col,Number], numberedSquare(Row,Col,g,Number),GlassesSquare),
  append(HotSquares, EmptySquares, EmptyHotSquares ),
  append(PlayerSquare, EmptyHotSquares, EmptyHotPlayerSquares),
  append(ExitSquare, EmptyHotPlayerSquares, EmptyHotPlayerExitSquares),
  append(GlassesSquare, EmptyHotPlayerExitSquares, AllTraversableSquares),
  createConnections(AllTraversableSquares).

giveAllSquaresValues([], Number).

giveAllSquaresValues([[Row,Col,Item]|T], Number):-
  asserta(numberedSquare(Row,Col,Item, Number)),
  NewNumber is Number + 1,
  giveAllSquaresValues(T, NewNumber).

createConnections([[Row,Col,Number]|T]):-
  maxRowCol(Max),
  setof([AdjacentRow, AdjacentCol],adjacent(Row,Col,AdjacentRow, AdjacentCol, Max), AdjacentCells),
  createAdjacentConnections(Row,Col,AdjacentCells).

createAdjacentConnections(_,_,[]).
createAdjacentConnections(Row,Col,[[AR,AC]|T]):-
  numberedSquare(Row,Col,_,Number),
  numberedSquare(AR,AC,_,AdjacentNumber),
  asserta(connection(Number, AdjacentNumber)),
  createAdjacentConnections(Row,Col,T).

createAdjacentConnections(Row,Col,[[_,_]|T]):-
  %if fail continue anyway
  createAdjacentConnections(Row,Col,T).

solve( [StartRow, StartColumn], Solution)  :-
  breadthfirst( [ [StartRow, StartColumn] ], Solution).

breadthfirst( [ [Node | Path] | _], [Node | Path])  :-
  write('Checking top node of path for goal '), write([Node | Path]),nl,
  goal( Node).

breadthfirst( [Path | Paths], Solution)  :-
  write('Extending Path :'),write(Path),nl,
  extend( Path, NewPaths),
  write('Extended Path :'),write(NewPaths),nl,
  append( Paths,NewPaths,  Paths1),
  write('Queue of Paths :'),write(Paths1),nl,
  breadthfirst( Paths1, Solution).

extend( [Node | Path], NewPaths)  :-
  bagof( [NewNode, Node | Path],
         ( s( Node, NewNode),
          \+ member( NewNode, [Node | Path] ) ),
         NewPaths),
  !.

extend( _, [] ).              % bagof failed: Node has no successor
