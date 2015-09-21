% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

:- ensure_loaded('createBoard.pl').

:- dynamic lastGridSquare/1.
lastGridSquare(e).

%cool opening title

start :-
	writeln('sdSS_SSSSSSbs   .S    S.     sSSs         .S_SsS_S.    .S_SSSs     sdSSSSSSSbs    sSSs  '),
	writeln('YSSS~S%SSSSSP  .SS    SS.   d%%SP        .SS~S*S~SS.  .SS~SSSSS    YSSSSSSSS%S   d%%SP  '),
	writeln('     S%S       S%S    S%S  d%S           S%S  Y  S%S  S%S   SSSS          S%S   d%S     '),
	writeln('     S%S       S%S    S%S  S%S           S%S     S%S  S%S    S%S         S&S    S%S     '),
	writeln('     S&S       S%S SSSS%S  S&S           S%S     S%S  S%S SSSS%S        S&S     S&S     '),
	writeln('     S&S       S&S  SSS&S  S&S_Ss        S&S     S&S  S&S  SSS%S        S&S     S&S_Ss  '),
	writeln('     S&S       S&S    S&S  S&S~SP        S&S     S&S  S&S    S&S       S&S      S&S~SP  '),
	writeln('     S&S       S&S    S&S  S&S           S&S     S&S  S&S    S&S      S*S       S&S     '),
	writeln('     S*S       S*S    S*S  S*b           S*S     S*S  S*S    S&S     S*S        S*b     '),
	writeln('     S*S       S*S    S*S  S*S.          S*S     S*S  S*S    S*S   .s*S         S*S.    '),
	writeln('     S*S       S*S    S*S   SSSbs        S*S     S*S  S*S    S*S   sY*SSSSSSSP   SSSbs  '),
	writeln('     S*S       SSS    S*S    YSSP        SSS     S*S  SSS    S*S  sY*SSSSSSSSP    YSSP  '),
	writeln('     SP               SP                         SP          SP			 '),
	writeln('     Y                Y                          Y           Y                          '),

	writeln('What sized square grid would you like? (minimum 10) or quit to quit.'),
	read(GridSize),
	valid(GridSize),
	createCompleteBoard(GridSize).

%if an invalid range is entered tell them to enter a valid one
start :-
	oops.

oops :-
	writeln('Not Valid, number greater than 10 please followed by fullstop(.)'),
	read(GridSize),
	valid(GridSize),
	createCompleteBoard(GridSize).

oops :-
	oops.

%set up the board given a grid size
createCompleteBoard(SizeOfGrid) :-
	createBoard(SizeOfGrid),
	addExit(SizeOfGrid),
	addBlocks,
	addFirePits,
	addPlayer,
	addGlasses,
	square(Row,Col,p,_),
	setAdjacentSquaresToSeen(Row,Col),
	menu.

%the main menu, only really use move
menu :-
	setof([Row,Col,Item,Seen], square(Row, Col, Item, Seen),TheSquares),
	showBoard(TheSquares),
	writeln('Enter Choice followed by full stop and return'),
	writeln('move,place,listing,find,quit.....'),
	read(Choice),
	member(Choice,[move,place,find,quit,listing]),
	action(Choice),Choice == quit.

%catch for bad input
menu :-
	menu.

%helper to get to max number in a list
maxRowCol(Max):-
	setof([Row,Col,Item,Seen], square(Row, Col, Item, Seen),TheSquares),
	last(TheSquares, [TheLastRow,_,_,_]),
	writeln(TheLastRow),
	Max is TheLastRow.

%the move action which gets the player, the max size of grid its adjacent spots, direction from user,
%makes sure inputs correct, converts the english to a math move and move
action(move):-
	square(PlayerRow, PlayerColumn, p, _),
	maxRowCol(MaxRowCol),
	setof([AvailableRow, AvailableColumn], adjacent(PlayerRow, PlayerColumn, AvailableRow, AvailableColumn, MaxRowCol), ListOfAdjacent),
	writeln('What direction would you like to move in (up,down,right,left)?'),
	read(Direction),
	member(Direction, [up,down,right,left]),
	convertDirection(Direction, [NewRow, NewCol]),
	member([NewRow, NewCol],ListOfAdjacent),
	moveToPlace(NewRow, NewCol).

%if the direction is wrong input
action(move):-
	writeln('The direction needs to be one of the directions mentioned'),
	action(move).

%old stuff from pre english input
action(move):-
	writeln('Sorry that is not a valid square to move into'),
	action(move).

%First way I did it
action(place) :-
	writeln('Row'),
	read(Row),
	writeln('Col'),
	read(Col),
	writeln('new item'),
	read(NewItem),
	retract(location(Row,Col,empty)),
	assert(location(Row,Col,NewItem)),
	write(' Replaced '),write(Item),
	write(' with '),
	write(NewItem),nl,
	write(Item).

action(find) :-
	read(Row),
	read(Col),
	read(ItemtoFind),
	location(Row,Col,ItemtoFind),
	writeln('Found').

%a proper quit
action(quit) :-
	abort.

action(listing) :-
	setof([Row,Col,Item,Seen], square(Row, Col, Item, Seen),TheSquares),
	showBoard(TheSquares),
	menu.

moveToPlace(Row,Col):-
	square(Row,Col, Item, Seen),
	moveToPlace(Row, Col, Item),
	menu.

%different movements for different items and how to react to them eg firepits kill you and end the game
moveToPlace(Row, Col, f):-
	writeln('Unfortunately you walked into a fire pit and died.'),
	ansi_format([bold,fg(red)], 'GAME OVER', []),
	action(quit).

moveToPlace(Row, Col, b):-
	writeln('You cant go into a blocked(b) square'),
	menu.

moveToPlace(Row, Col, w):-
	writeln('You cant go into a walled(w) square'),
	menu.

moveToPlace(Row, Col, h):-
	returnPreviousToOldState,
	moveThePlayer(Row,Col),
	writeln('Hot square, this means there is a firepit nearby, be careful'),
	menu.

moveToPlace(Row, Col, e):-
	returnPreviousToOldState,
	moveThePlayer(Row,Col),
	writeln('Empty square'),
	menu.

moveToPlace(Row, Col, g):-
	ansi_format([bold,fg(green)],'You  found the glasses! Congratulations you win.', []),
	setAllSquaresToSeen,
	setof([Row,Col,Item,Seen], square(Row, Col, Item, Seen),TheSquares),
	showBoard(TheSquares),
	menu.

moveToPlace(Row, Col, x):-
	writeln('Sorry, you found the exit but you must first find the glasses before you can leave, please try another'),
	menu.

%putting a square back after the robot moves off the square
returnPreviousToOldState:-
	square(Row,Col,p,Seen),
	lastGridSquare(TheItem),
	retract(square(Row,Col,p,Seen)),
	asserta(square(Row,Col,TheItem,s)).

%moving the player and setting adjacents to seeen
moveThePlayer(Row,Col):-
	square(Row,Col,Item,Seen),
	retract(lastGridSquare(_)),
	asserta(lastGridSquare(Item)),
	retract(square(Row,Col,Item,Seen)),
	asserta(square(Row,Col,p,s)),
	setAdjacentSquaresToSeen(Row,Col).


%helper methods

setAllSquaresToSeen:-
	setof([Row,Col,Item,Seen], square(Row, Col, Item, Seen),TheSquares),
	setAllSquaresToSeen(TheSquares).

setAllSquaresToSeen([]).

setAllSquaresToSeen([[Row,Col,Item,Seen]|T]):-
	retract(square(Row,Col,Item,Seen)),
	asserta(square(Row,Col,Item,s)),
	setAllSquaresToSeen(T).

setAdjacentSquaresToSeen(Row,Col):-
	square(Row,Col,Item,Seen),
	maxRowCol(MaxRowCol),
	setof([AvailableRow, AvailableColumn], adjacent(Row, Col, AvailableRow, AvailableColumn, MaxRowCol), ListOfAdjacent),
	setAdjacentSquaresToSeen(ListOfAdjacent).

setAdjacentSquaresToSeen([]).

setAdjacentSquaresToSeen([[Row,Col]|T]):-
	square(Row,Col,Item,Seen),
	retract(square(Row,Col,Item,Seen)),
	asserta(square(Row,Col,Item,s)),
	setAdjacentSquaresToSeen(T).

validRange(row,1,Max) :-
	setof(Row,location(Row,_,_),List),
	reverse(List,[Max|_]).

validRange(col,1,Max) :-
	setof(Col,location(_,Col,_),List),
	reverse(List,[Max|_]).

valid(quit):-
	action(quit).

valid(Item) :-
	integer(Item),
	Item >= 10.

convertDirection(up, [Row,Col]):-
	square(PlayerRow, PlayerCol, p, _),
	Row is PlayerRow - 1,
	Col is PlayerCol.

convertDirection(down, [Row,Col]):-
	square(PlayerRow, PlayerCol, p, _),
	Row is PlayerRow + 1,
	Col is PlayerCol.

convertDirection(left, [Row,Col]):-
	square(PlayerRow, PlayerCol, p, _),
	Row is PlayerRow,
	Col is PlayerCol - 1.

convertDirection(right, [Row,Col]):-
	square(PlayerRow, PlayerCol, p, _),
	Row is PlayerRow,
	Col is PlayerCol + 1.
