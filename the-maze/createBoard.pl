%this is the code for most interactions with the board
:- dynamic square/3.
:- ensure_loaded('adjacent.pl').

createBoard(N) :-
	%get rid of squares if there are any
     retractall(square(_,_,_,_)),
     %create board and add walls
     createBoard(N,N),
     addWalls(N,N).

createBoard(0,_).

createBoard(Row,Col) :-
 createCol(Row,Col),
 NextRow is Row -1,
 createBoard(NextRow,Col).

%self explanatory creating columns and rows in a generic fashion for the allocated grid size
createCol(Row,1) :-
      asserta(square(Row,1,e,u)).

createCol(CurrentRow,CurrentCol) :-
       asserta(
       square(CurrentRow,CurrentCol,e,u)
	     ),
        NextCol is CurrentCol -1,
        createCol(CurrentRow,NextCol).

%place items
place(Piece,Row,Col) :-
 retract(square(Row,Col,_,Seen)),
 assert(square(Row,Col,Piece,Seen)).

%place around item
place_around(Piece1,Piece2,Bound) :-
	square(R,C,Piece1,_),
	setof([Ar,Ac],adjacent(R,C,Ar,Ac,Bound),
	      ListofAdjacentSquares),
	place_all(Piece2,ListofAdjacentSquares).

place_all(_,[]).

place_all(Piece,[[R,C]|T]) :-
	square(R,C,_,S),
	assertz(square(R,C,Piece,S)),
	place_all(Piece,T).

showBoard:-
	writeln('point one'),
	setof([Row,Col,Item,Seen], square(Row, Col, Item, Seen),TheSquares),
	writeln('Set of done'),
	showBoard(TheSquares).

showBoard([]):-
	write_legend.

showBoard([[A,1,C,S]|OtherSquares]) :-
	nl,
	write_coordinate(A,1,C,S),
	showBoard(OtherSquares).

showBoard([[A,B,C,S]|OtherSquares]) :-
	write_coordinate(A,B,C,S),
	showBoard(OtherSquares).

%a legend of what means what
write_legend:-
	nl,nl,
	write('[Row Column Item] E = Empty, '),
	ansi_format([bold,fg(magenta)], 'w = Visible Wall', []),
	write(', '),
	ansi_format([bold,fg(yellow)], 	'h = Hot', []),
	write(', '),
	ansi_format([bold,fg(red)], 	'b = Blocked', []),
	write(', '),
	ansi_format([bold,fg(red)], 	'f = Fire', []),
	write(', '),
	ansi_format([bold,fg(green)], 'g = Glasses', []),
	write(', '),
	ansi_format([bold,fg(blue)], 	'p = Player', []),
	write(', '),
	ansi_format([bold,fg(green)], 	'x = Exit', []),
	nl.

%funky coloring of different items so its easier for the user to understand
write_coordinate(R,C,w,s):-
	ansi_format([bold,fg(magenta)], '[~w ', [R]),
	ansi_format([bold,fg(magenta)], '~w ', [C]),
	ansi_format([bold,fg(magenta)], 'I=~w', [w]),
	ansi_format([bold,fg(magenta)], ']', [  ]).

write_coordinate(R,C,e,s):-
	ansi_format([bold,fg(cyan)], '[~w ', [R]),
	ansi_format([bold,fg(cyan)], '~w ', [C]),
	ansi_format([bold,fg(cyan)], 'I=~w', [e]),
	ansi_format([bold,fg(cyan)], ']', [  ]).

write_coordinate(R,C,b,s):-
	ansi_format([bold,fg(red)], '[~w ', [R]),
	ansi_format([bold,fg(red)], '~w ', [C]),
	ansi_format([bold,fg(red)], 'I=~w ', [b]),
	ansi_format([bold,fg(red)], ']', [  ]).

write_coordinate(R,C,h,s):-
	ansi_format([bold,fg(yellow)], '[~w ', [R]),
	ansi_format([bold,fg(yellow)], '~w ', [C]),
	ansi_format([bold,fg(yellow)], 'I=~w', [h]),
	ansi_format([bold,fg(yellow)], ']', [  ]).

write_coordinate(R,C,f,s):-
	ansi_format([bold,fg(red)], '[~w ', [R]),
	ansi_format([bold,fg(red)], '~w ', [C]),
	ansi_format([bold,fg(red)], 'I=~w', [f]),
	ansi_format([bold,fg(red)], ']', [  ]).

write_coordinate(R,C,g,s):-
	ansi_format([bold,fg(green)], '[~w ', [R]),
	ansi_format([bold,fg(green)], '~w ', [C]),
	ansi_format([bold,fg(green)], 'I=~w', [g]),
	ansi_format([bold,fg(green)], ']', [  ]).

write_coordinate(R,C,x,s):-
	ansi_format([bold,fg(green)], '[~w ', [R]),
	ansi_format([bold,fg(green)], '~w ', [C]),
	ansi_format([bold,fg(green)], 'I=~w', [x]),
	ansi_format([bold,fg(green)], ']', [  ]).

write_coordinate(R,C,p,_):-
	ansi_format([bold,fg(blue)], '[~w ', [R]),
	ansi_format([bold,fg(blue)], '~w ', [C]),
	ansi_format([bold,fg(blue)], 'I=~w', [p]),
	ansi_format([bold,fg(blue)], ']', [ ]).

write_coordinate(R,C,I,S):-
	write('['), write(R),
	write(' '), write(C),
	write(' I='), write(I),
	write(']').

addWalls(0,_).

addWalls(Position, Max):-
	%walls there no matter what
	%left wall
	%place(w,Position,1),
	retract(square(Position,1,_,S1)),
	asserta(square(Position,1,w,S1)),

	%top wall
	%place(w,1,Position),
	retract(square(1,Position,_,S2)),
	asserta(square(1,Position,w,S2)),

	%right wall
	%place(w,Position, Max),
	retract(square(Position,Max,_,S3)),
	asserta(square(Position,Max,w,S3)),

	%bottom wall
	%place(w,Max,Position),
	retract(square(Max,Position,_,S4)),
	asserta(square(Max,Position,w,S4)),

	NewGridNumber is Position - 1,
	addWalls(NewGridNumber, Max).

%add and exit randomly on a wall
addExit([Row,Col]):-
	retract(square(Row,Col,_,S)),
	asserta(square(Row,Col,x,S)).

addExit(Max):-
	setof([Row,Col], square(Row,Col,w,_), Walls),
	delete(Walls, [1,1], Walls1),
	delete(Walls1, [1,Max], Walls2),
	delete(Walls2, [Max,1], Walls3),
	delete(Walls3, [Max,Max], Walls4),
	length(Walls4, Length),
	random(0, Length, Index),
	nth0(Index, Walls4, Elem),
	addExit(Elem).

%add random blocks that the player cant get past
addBlocks:-
	random(3, 4, NumberOfBlocks),
	addBlocks(NumberOfBlocks).

addBlocks(0).
addBlocks(NumberOfBlocks):-
	setof([R,C,e,S], square(R,C,e,S), EmptySquares),
	randomlyPlace(b, NumberOfBlocks, EmptySquares).

addPlayer:-
	setof([R,C,e,S], square(R,C,e,S), EmptySquares),
	randomlyPlace(p, 1, EmptySquares).

addGlasses:-
	setof([R,C,e,S], square(R,C,e,S), EmptySquares),
	randomlyPlace(g, 1, EmptySquares).

addFirePits:-
	random(1, 4, NumberOfPits),
	addFirePits(NumberOfPits).

addFirePits(NumberOfPits):-
	%place pits randomly
	randomlyPlaceFirePitAndWarms(f,NumberOfPits).

%lots of helper methods for custom placements of items etc and a test at the end for my debugging purposes

randomlyPlaceFirePitAndWarms(Item, [Row,Col,CurrentItem,Seen]):-
	place(Item, Row, Col).

randomlyPlaceFirePitAndWarms(Item, 0).
randomlyPlaceFirePitAndWarms(Item, Count):-
	%get all free spaces
	setof([R,C,e,S], square(R,C,e,S), EmptySquares),
	length(EmptySquares, LengthOfList),
	random(0, LengthOfList, NumberOfElement),
	nth0(NumberOfElement, EmptySquares, [Row,Col,CurrentItem, Seen]),
	randomlyPlace(Item, [Row,Col,CurrentItem, Seen]),
	delete(EmptySquares, [Row,Col,CurrentItem, Seen], NewListOfSquares),

	%place around it
	%get adjacent squares
	last(EmptySquares, [Bound,_,_,_]),%last should be bound of list
	setof([Ar,Ac],adjacent(Row,Col,Ar,Ac,Bound),ListofAdjacentSquares),
	placeInAdjacentSquaresIfSquaresEmpty(ListofAdjacentSquares, h),

	%recurse
	NewCount is Count - 1,
	randomlyPlaceFirePitAndWarms(Item, NewCount).

placeInAdjacentSquaresIfSquaresEmpty([], _).

placeInAdjacentSquaresIfSquaresEmpty([R,C,e,S], ItemToPlace):-
	retract(square(R,C,e,S)),
	assert(square(R,C,ItemToPlace,S)).

placeInAdjacentSquaresIfSquaresEmpty([[R,C]|T], ItemToPlace):-
	square(R,C,I,S),
	placeInAdjacentSquaresIfSquaresEmpty([R,C,I,S], ItemToPlace),
	placeInAdjacentSquaresIfSquaresEmpty(T, ItemToPlace).

placeInAdjacentSquaresIfSquaresEmpty([_,_,_,_], ItemToPlace).

randomlyPlace(p, [Row,Col,CurrentItem,Seen]):-
	place(Item, Row, Col).

randomlyPlace(Item, [Row,Col,CurrentItem,Seen]):-
	place(Item, Row, Col).

randomlyPlace(Item, 0, ListOfSquares).
randomlyPlace(Item, Count, ListOfSquares):-
	length(ListOfSquares, LengthOfList),
	random(0, LengthOfList, NumberOfElement),
	nth0(NumberOfElement, ListOfSquares, Elem),
	randomlyPlace(Item, Elem),
	delete(ListOfSquares, Elem, NewListOfSquares),
	NewCount is Count - 1,
	randomlyPlace(Item, NewCount, NewListOfSquares).

test_squares :-
	createBoard(10),
	addExit(10),
	addBlocks,
	addFirePits,
	addPlayer,
	addGlasses,
	setof([Row,Col,Item,Seen], square(Row, Col, Item, Seen),TheSquares),
	showBoard(TheSquares).
