
(* some exceptions and data types *)

exception None_is_none
exception Square_is_empty
exception Piece_not_on_board
exception Bad_arguments
exception Unknown_rules
exception Unknown_piece
exception Unknown_color
exception Unknown_command
exception Wrong_color
exception New_game
exception Continue
exception Quit

type equality = Same | Different
type validity = Valid | Invalid
type reachability = Reachable | Not_reachable

exception Boundedness
type boundedness = Within_bounds |  Out_of_bounds

exception Emptiness
type emptiness = Empty | Not_empty

exception Wrong_move
type typeOfMove = Improper_move | Simple_move | Capture_move | Special_move | In_passing
type lengthOfmove = Steps of int | Adjacent_square | Same_square




(* two simple functions to work with 'a option type *)
let (<--) x = match x with (Some y) -> y | None -> raise None_is_none
and (-->) x = Some x


(* addition and subtraction of integer pairs *)
let (++) (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)
and (--) (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)


(* remove blanks from a string *)
let removeBlanks x =
  let x = Str.global_replace (Str.regexp "^[ \t]+\\|[ \t]+$") "" x in
  let x = Str.global_replace (Str.regexp "[ \t][ \t]+") " " x in
  let x = String.lowercase x in x


(* return a string of a pair *)
let string_of_position (i, j) f g = (f i) ^ (g j)




class type

['a] piece =
object
  method getColor : string
  method getType : string
  method getMarker : string
  method getPosition : int * int
  method isOnBoard : unit -> bool
  method setupOnBoard : 'a board -> int * int -> unit
  method removeFromBoard : unit -> unit
  method checkMove : int * int -> typeOfMove
  method pieceCouldBeCaptured : int * int -> bool
  method getMoved : unit -> bool
  method setMoved : unit -> unit
  method unsetMoved : unit -> unit
end

and

['a] board =
object
  method makePiece : int * int -> string -> string -> unit
  method getPiece : int * int -> 'a piece
  method grabPiece : int * int -> 'a piece
  method putPiece : int * int -> 'a piece -> unit
  method emptySquareInPosition : int * int -> unit
  method squareIsEmpty : int * int -> emptiness
  method isItValidPosition : int * int -> boundedness
  method checkColorInPosition : int * int -> string -> equality
  method setBoardStatus : emptiness -> unit
  method getBoardStatus : unit -> emptiness
  method setPiecesList : 'a piece list -> unit
  method getPiecesList : unit -> 'a piece list
  method getHistory : unit -> 'a history
  method setHistory : 'a history -> unit
  method print : unit -> unit
end

and

['a] rules =
object
  method getBoardSize : unit -> int * int
  method getNumberOfPlayers : unit -> int
  method getFirstPlayer : unit -> string
  method getBoard : unit -> 'a board
  method setupBoard : 'a board -> 'a history -> unit
  method getMove : 'a board -> string -> string -> 'a move
  method translatePositions : string -> 'a piece list -> string -> (int * int) list
  method translatePromotion : string -> string
end

and

['a] move =
object
  method doIt : unit -> unit
  method undoIt : unit -> string
  method getTypeOfMove : unit -> typeOfMove
  method isCaptureMove : unit -> bool
  method getCapturedPiece : unit -> 'a piece
  method checkThisMove : string -> string -> int * int -> int * int -> bool
  method print : unit -> unit
end

and

['a] captured =
object
  method addPiece : 'a piece -> unit
  method delLastOne : unit -> unit
  method howManyPieces : unit -> int
end

and

['a] history =
object
  method getLastMove : unit -> 'a move
  method addMove : 'a move -> unit
  method undoMove : int -> string
  method howManyMoves : unit -> int
  method howManyCapturedPieces : unit -> int
  method print : int -> unit
end

and

['a] game =
object
  method init : unit -> unit
  method play : unit -> unit
end




class virtual ['a] commonGame =
object
  method virtual private restart : unit -> unit
  method virtual private print : unit -> unit
  method virtual private history : int -> unit
  method virtual private undo : int -> unit
  method virtual private move : string -> unit
end




class virtual ['a] commonPiece pieceColor pieceType pieceMarker =
object (self : 'a #piece)

  method getColor = pieceColor
  method getType = pieceType
  method getMarker = pieceMarker


  val mutable pieceBoard : 'a board option = None

  method private getBoard =
    (<--) pieceBoard (* could raise None_is_none *)

  method private setBoard =
    fun newBoard -> pieceBoard <- ((-->) newBoard)


  val mutable piecePosition : (int * int) option =  None

  method getPosition =
    (<--) piecePosition (* could raise None_is_none *)

  method private setPosition =
    fun newPosition -> piecePosition <- ((-->) newPosition)


  method isOnBoard () =
    if pieceBoard = None then false else true


  method setupOnBoard newBoard newPosition =
    self#setBoard newBoard ;
    self#setPosition newPosition


  method removeFromBoard () =
    pieceBoard <- None ;
    piecePosition <- None

end




class virtual ['a] commonBoard =
object (self : 'a #board)

  val virtual xsize : int
  val virtual ysize : int
  val virtual plane : 'a piece option array array

  val virtual shouldBeEmpty : bool
  val mutable emptyStatus = Empty


  val mutable piecesOnBoard : 'a piece list = []

  method setPiecesList piecesList =
    piecesOnBoard <- piecesList

  method getPiecesList () = piecesOnBoard


  val mutable curHistory : 'a history option = None

  method getHistory () =
    (<--) curHistory (* could raise None_is_none *)

  method setHistory newHistory =
    curHistory <- ((-->) newHistory)


  method getPiece (i, j) =
    try
      let curPiece = (<--) plane.(i).(j) in
      (* could raise None_is_none *)
      curPiece
    with
      | None_is_none -> raise Square_is_empty


  method grabPiece (i, j) =

    let curPiece = self#getPiece (i, j) in
    (* could raise Square_is_empty *)

    self#emptySquareInPosition (i, j) ;
    curPiece#removeFromBoard () ;

    curPiece


  method putPiece (i, j) curPiece =
    curPiece#setupOnBoard (self :> 'a board) (i, j) ;
    plane.(i).(j) <- (-->) curPiece


  method emptySquareInPosition (i, j) =
    plane.(i).(j) <- None


  method squareIsEmpty (i, j) =
    try
      let _ = self#getPiece (i, j) in
      (* could raise Square_is_empty *)
      Not_empty
    with
      | Square_is_empty -> Empty


  method isItValidPosition (i, j) =
    if 0 <= i && i < xsize && 0 <= j && j < ysize then
      Within_bounds
    else
      Out_of_bounds


  method checkColorInPosition (i, j) expectedColor =

    let pieceColor = (self#getPiece (i, j))#getColor in
    (* could raise Square_is_empty *)

    if expectedColor = pieceColor then
      Same
    else
      Different


  method setBoardStatus newStatus =
    emptyStatus <- newStatus


  method getBoardStatus () = emptyStatus


  method print () =

    let rec concatenateString n s =
      if n > 0 then s ^ concatenateString (n - 1) s else ""
    in

    let markerString (i, j) =
      try
	let curMarker = (self#getPiece (i, j))#getMarker in
	curMarker
      with
	| Square_is_empty -> " "
    in

    let rec print (i, j) =
      if j >= 0 then
	if i < xsize then
	  (
	    print_string ("| " ^ (markerString (i, j)) ^ " ") ;
	    print (i + 1, j)
	  )
	else
	  (
	    print_string "|\n" ;
	    print_string ((concatenateString xsize "+---") ^ "+\n") ;
	    print (0, j - 1)
	  )
    in

    print_string ((concatenateString xsize "+---") ^ "+\n") ;
    print (0, ysize - 1)

end




class virtual ['a] commonRules =
object (self : 'a #rules)

  val virtual boardSize : int * int
  val virtual numberOfPlayers : int
  val virtual firstPlayer : string
  val virtual emptyBoard : bool

  val virtual pieceColors : string list
  val virtual pieceTypes : string list

  method private getPieceColors = pieceColors
  method private getPieceTypes = pieceTypes

  method getBoardSize () = boardSize
  method getNumberOfPlayers () = numberOfPlayers
  method getFirstPlayer () = firstPlayer

  method virtual private getPieceMarker : string * string -> string
  method virtual private getPiecePositions : string * string -> (int * int) list
  method virtual private buildPiece : string -> string -> 'a piece
  method virtual private buildMove : 'a board -> string -> string -> 'a move


  method getBoard () : 'a board =
    let size = self#getBoardSize () in
  object (selfBoard)

    inherit ['a] commonBoard

    val xsize = fst size
    val ysize = snd size

    val plane = Array.make_matrix (fst size) (snd size) None

    val shouldBeEmpty = emptyBoard

    method makePiece piecePosition pieceColor pieceType =
      let newPiece = self#buildPiece pieceColor pieceType in
      selfBoard#putPiece piecePosition newPiece

  end


  method setupBoard (theBoard : 'a board) (theHistory : 'a history) =

    let buildPiecesList () =
      (
	let thirdFun x y z resultList =

	  let i = fst z and j = snd z in
	  let newPiece = self#buildPiece x y in

	  match theBoard#squareIsEmpty (i, j) with
	    | Empty ->

	      theBoard#putPiece (i, j) newPiece ;
	      newPiece :: resultList

	    | Not_empty -> raise Emptiness
	in

	let secondFun x y resultList =
	  List.fold_right (thirdFun x y) (self#getPiecePositions (x, y)) resultList
	in

	let firstFun x resultList =
	  List.fold_right (secondFun x) self#getPieceTypes resultList
	in

	List.fold_right firstFun self#getPieceColors []
      )
    in

    let piecesList = buildPiecesList () in
    (* could raise Emptiness *)
    if List.length piecesList <> 0 then
      (
	theBoard#setHistory theHistory ;
	theBoard#setBoardStatus Not_empty ;
	theBoard#setPiecesList piecesList
      )


  method getMove theBoard thePositions thePlayer =
    self#buildMove theBoard thePositions thePlayer

end




class chess : [chess] rules =
object (self)

  inherit [chess] commonRules

  val boardSize = (8, 8)
  val numberOfPlayers = 2
  val firstPlayer = "White"
  val emptyBoard = false

  val pieceColors = ["White"; "Black"]
  val pieceTypes = ["King"; "Queen"; "Rook"; "Bishop"; "Knight"; "Pawn"]


  method private getPieceMarker (pieceColor, pieceType) =

    let getMarker x =
      match x with
	| "King" -> "K"
	| "Queen" -> "Q"
	| "Rook" -> "R"
	| "Bishop" -> "B"
	| "Knight" -> "N"
	| "Pawn" -> "P"
	| _ -> raise Unknown_piece
    in

    match pieceColor with
      |	"White" -> String.lowercase (getMarker pieceType)
      | "Black" -> String.uppercase (getMarker pieceType)
      | _ -> raise Unknown_color


  method private getPieceType x =
    match x with
      | "K" -> "King"
      | "Q" -> "Queen"
      | "R" -> "Rook"
      | "B" -> "Bishop"
      | "N" -> "Knight"
      | "P" -> "Pawn"
      | _ -> raise Unknown_piece


  method private getPiecePositions x =

    match x with
      | ("White", "King") -> [(4, 0)]
      | ("White", "Queen") -> [(3, 0)]
      | ("White", "Rook") -> [(0, 0); (7, 0)]
      | ("White", "Bishop") -> [(2, 0); (5, 0)]
      | ("White", "Knight") -> [(1, 0); (6, 0)]
      | ("White", "Pawn") -> [(0, 1); (1, 1); (2, 1); (3, 1);
			      (4, 1); (5, 1); (6, 1); (7, 1)]
      | ("Black", "King") -> [(4, 7)]
      | ("Black", "Queen") -> [(3, 7)]
      | ("Black", "Rook") -> [(0, 7); (7, 7)]
      | ("Black", "Bishop") -> [(2, 7); (5, 7)]
      | ("Black", "Knight") -> [(1, 7); (6, 7)]
      | ("Black", "Pawn") -> [(0, 6); (1, 6); (2, 6); (3, 6);
			      (4, 6); (5, 6); (6, 6); (7, 6)]
      | _ -> raise Unknown_piece


  method private getCheckDestFun x empty initial passing =
    match x with
      | (_, "King") ->
	(
	  fun (dx, dy) ->
	    if (abs dx = 0 || abs dx = 1) && (abs dy = 0 || abs dy = 1) then
	      (self#getCheckDestFun ("", "Queen") empty initial passing) (dx, dy)
	    else
	      Invalid
	)
      | (_, "Queen") ->
	(
	  fun (dx, dy) ->
	    if self#getCheckDestFun ("", "Rook")
	      empty initial passing (dx, dy) = Valid
	    || self#getCheckDestFun ("", "Bishop")
	      empty initial passing (dx, dy) = Valid
	    then Valid else Invalid
	)
      | (_, "Rook") ->
	(
	  fun (dx, dy) ->
	    if (dx = 0 && dy <> 0) || (dx <> 0 && dy = 0)
	    then Valid else Invalid
	)
      | (_, "Bishop") ->
	(
	  fun (dx, dy) ->
	    if abs dx = abs dy
	    then Valid else Invalid
	)
      | (_, "Knight") ->
	(
	  fun (dx, dy) ->
	    if (abs dx = 1 && abs dy = 2) || (abs dx = 2 && abs dy = 1)
	    then Valid else Invalid
	)
      | ("White", "Pawn") ->
	(
	  fun (dx, dy) ->

	    let pawnValidMove (dx, dy) =
	      match (empty, initial, passing) with
		| (Empty, true, _) -> (dx = 0 && (dy = 1 || dy = 2))
		| (Empty, false, false) -> (dx = 0 && dy = 1)
		| (Empty, false, true)
		| (Not_empty, _, _) -> ((dx = -1 || dx = 1) && dy = 1)
	    in

	    if pawnValidMove (dx, dy)
	    then Valid else Invalid
	)
      | ("Black", "Pawn") ->
	(
	  fun (dx, dy) ->

	    let pawnValidMove (dx, dy) =
	      match (empty, initial, passing) with
		| (Empty, true, _) -> (dx = 0 && (dy = -1 || dy = -2))
		| (Empty, false, false) -> (dx = 0 && dy = -1)
		| (Empty, false, true)
		| (Not_empty, _, _) -> ((dx = -1 || dx = 1) && dy = -1)
	    in

	    if pawnValidMove (dx, dy)
	    then Valid else Invalid
	)
      | _ -> raise Unknown_piece


  method private intOfBoardFile x =
    match x with
      | "a" -> 0 | "b" -> 1 | "c" -> 2 | "d" -> 3
      | "e" -> 4 | "f" -> 5 | "g" -> 6 | "h" -> 7
      | _ -> raise Bad_arguments


  method private boardFileOfInt x =
    match x with
      | 0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d"
      | 4 -> "e" | 5 -> "f" | 6 -> "g" | 7 -> "h"
      | _ -> raise Bad_arguments


  method private buildPiece pieceColor pieceType =
    let pieceMarker = self#getPieceMarker (pieceColor, pieceType) in
    let pieceCheckDest = self#getCheckDestFun (pieceColor, pieceType) in
  object (selfPiece)

    inherit [chess] commonPiece pieceColor pieceType pieceMarker


    val mutable movedPiece = false

    method setMoved () = movedPiece <- true
    method unsetMoved () = movedPiece <- false
    method getMoved () = movedPiece


    method private directionIsEmpty pStart pEnd =

      let getDirection (i, j) =
	let reduceNumber x =
	  match x with
	    | 0 -> 0
	    | _ -> (x / (abs x))
	in
	(reduceNumber i, reduceNumber j)
      in

      let rec allSquaresAreEmpty (i, j) curDirection n =
	let curPosition = (i, j) ++ curDirection in
      	match n with
      	  | 1 -> Empty
      	  | n ->
	    match selfPiece#getBoard#squareIsEmpty curPosition with
	      | Empty -> allSquaresAreEmpty curPosition curDirection (n - 1)
	      | Not_empty -> Not_empty
      in

      let howManySteps (i, j) =
	let steps = max (abs i) (abs j) in
	match steps with
	  | 0 -> Same_square
	  | 1 -> Adjacent_square
	  | n -> Steps n
      in

      let pDiff = pEnd -- pStart in

      let direction = getDirection pDiff
      and steps = howManySteps pDiff
      in

      match steps with
	| Same_square -> Not_reachable
	| Adjacent_square -> Reachable
	| Steps n ->
	  if allSquaresAreEmpty pStart direction n = Empty
	  || pieceType = "Knight"
	  then
	    Reachable
	  else
	    Not_reachable


    method private checkInPassing pStart pEnd =
      try
	(
	  let pDiff = pEnd -- pStart
	  and lastMove = (selfPiece#getBoard#getHistory ())#getLastMove ()
	  in

	  let opposingPawnStart = pEnd ++ (0, snd pDiff)
	  and opposingPawnEnd = pEnd -- (0, snd pDiff)
	  in


	  match (selfPiece#getColor, selfPiece#getType) with
	    | ("White", "Pawn") ->

	      lastMove#checkThisMove
		"Black" "Pawn" opposingPawnStart opposingPawnEnd

	    | ("Black", "Pawn") ->

	      lastMove#checkThisMove
		"White" "Pawn" opposingPawnStart opposingPawnEnd

	    | _ -> false
	)
      with
	| Emptiness -> false


    method private destinationIsValid pStart pEnd =

      let pDiff = pEnd -- pStart

      and initialOrNot = not (selfPiece#getMoved ())

      and emptyOrNot = selfPiece#getBoard#squareIsEmpty pEnd

      and passingOrNot = selfPiece#checkInPassing pStart pEnd

      in

      pieceCheckDest emptyOrNot initialOrNot passingOrNot pDiff


    method pieceCouldBeCaptured position =

      let piecesOnBoard =
	List.fold_right
	  (
	    fun curPiece listResult ->
	      if curPiece#isOnBoard () then
		curPiece :: listResult
	      else
		listResult
	  ) (selfPiece#getBoard#getPiecesList ()) []
      in

      let searchOpponentPieces =
	List.fold_right
	  (
	    fun curPiece listResult ->
	      if curPiece#getColor <> selfPiece#getColor
	      && curPiece#checkMove position = Capture_move then
		curPiece :: listResult
	      else
		listResult
	  ) piecesOnBoard []
      in

      if List.length searchOpponentPieces <> 0 then
	true
      else
	false


    method checkMove pEnd =

      let pStart = selfPiece#getPosition in

      let emptyOrNot = selfPiece#getBoard#squareIsEmpty pEnd in

      let validOrNot = selfPiece#destinationIsValid pStart pEnd in

      let reachableOrNot =
	if validOrNot = Valid then
	  selfPiece#directionIsEmpty pStart pEnd
	else
	  Not_reachable
      in

      let passingOrNot = selfPiece#checkInPassing pStart pEnd in

      match (emptyOrNot, validOrNot, reachableOrNot) with
	| (Empty, Valid, Reachable) ->
	  if not passingOrNot then
	    Simple_move
	  else
	    In_passing
	| (Not_empty, Valid, Reachable) ->
	  let destPiece = selfPiece#getBoard#getPiece pEnd in
          (* could NOT raise Square_is_empty *)
	  if destPiece#getColor <> selfPiece#getColor then
	    Capture_move
	  else if destPiece#getType = "King" && selfPiece#getType = "Rook"
	       && not (destPiece#getMoved ())
	       && not (selfPiece#getMoved ())
	       && not (selfPiece#pieceCouldBeCaptured pStart)
	       && not (selfPiece#pieceCouldBeCaptured pEnd)
	  then
	    Special_move
	  else
	    Improper_move
	| (_, Invalid, _) -> Improper_move
	| (_, _, Not_reachable) -> Improper_move

  end


  method translatePositions x setOfPieces curPlayer =
  (* a move must be written in short algebraic notation *)

    let translatePoint x =
      if Str.string_match (Str.regexp "^[a-h][1-8]$") x 0 then
	let p = (self#intOfBoardFile (String.sub x 0 1), ((int_of_string (String.sub x 1 1)) - 1)) in
	p
      else
	raise Bad_arguments
    in

    let translateDoublePoint x =
      if Str.string_match (Str.regexp "^[a-h][1-8][a-h][1-8]$") x 0 then
	let startEnd = Str.global_replace (Str.regexp "-") "" x in
	let pStart = translatePoint (String.sub startEnd 0 2) in
	let pEnd = translatePoint (String.sub startEnd 2 2) in
	[pStart; pEnd]
      else
	raise Bad_arguments
    in

    let isPawn x =
      if Str.string_match (Str.regexp "^[KQNRB]") x 0 then
	false
      else
	true
    in

    let searchPiece setOfPieces pieceColor pieceType pEnd =

      let filterPieceFun curPiece resultList =
	if curPiece#getColor = pieceColor
	&& curPiece#getType = pieceType
	&& curPiece#isOnBoard ()
        && curPiece#checkMove pEnd <> Improper_move
	then curPiece :: resultList
	else resultList
      in

      List.fold_right filterPieceFun setOfPieces []
    in

    let searchUnambiguously setOfPieces pieceType pEnd fstSnd halfStart =
      let piecesToMove = searchPiece setOfPieces curPlayer pieceType pEnd in

      let filterPieceFun curPiece resultList =
	if fstSnd (curPiece#getPosition) = halfStart then
	  curPiece :: resultList
	else
	  resultList
      in

      List.fold_right filterPieceFun piecesToMove []
    in

    let printAmbiguousMoves listOfPieces =
      print_endline "This move is ambiguous! Available pieces are:" ;
      List.iter
	(
	  fun x ->
	    let pieceColor = x#getColor
	    and pieceType = x#getType
	    and piecePosition = string_of_position
	      x#getPosition
	      (self#boardFileOfInt)
	      (fun x -> string_of_int (x + 1))
	    in
	    print_endline
	      (
		pieceColor ^ " " ^
		pieceType ^ " in " ^
		piecePosition
	      )
	)
	listOfPieces
    in

    let x = Str.global_replace (Str.regexp "[x+]") "" x in

    let pieceType = if isPawn x then "Pawn" else self#getPieceType (String.sub x 0 1)
    and x =
      if isPawn x || (Str.string_match (Str.regexp "^O") x 0) then
	x
      else
	(String.sub x 1 ((String.length x) - 1))
    in

    if Str.string_match (Str.regexp "^[a-h][1-8]\\(=[KQNRB]\\)?$") x 0 then

      let pEnd = translatePoint (String.sub x 0 2) in

      let pieceToMove =
	searchPiece setOfPieces curPlayer pieceType pEnd
      in

      match List.length pieceToMove with
	| 1 -> let pStart = (List.hd pieceToMove)#getPosition in
	       [pStart; pEnd]
	| 0 -> raise Wrong_move
	| _ ->
	  (
	    printAmbiguousMoves pieceToMove ;
	    raise Wrong_move
	  )

    else if Str.string_match (Str.regexp "^[a-h][a-h][1-8]$") x 0 then

      let pieceFile = self#intOfBoardFile (String.sub x 0 1)
      and pEnd = translatePoint (String.sub x 1 2)
      in

      let pieceToMove =
	searchUnambiguously setOfPieces pieceType pEnd (fst) pieceFile
      in

      match List.length pieceToMove with
	| 1 -> let pStart = (List.hd pieceToMove)#getPosition in
	       [pStart; pEnd]
	| 0 -> raise Wrong_move
	| _ ->
	  (
	    printAmbiguousMoves pieceToMove ;
	    raise Wrong_move
	  )

    else if Str.string_match (Str.regexp "^[1-8][a-h][1-8]$") x 0 then

      let pieceRank = (int_of_string (String.sub x 0 1)) - 1
      and pEnd = translatePoint (String.sub x 1 2)
      in

      let pieceToMove =
	searchUnambiguously setOfPieces pieceType pEnd (snd) pieceRank
      in

      match List.length pieceToMove with
	| 1 -> let pStart = (List.hd pieceToMove)#getPosition in
	       [pStart; pEnd]
	| 0 -> raise Wrong_move
	| _ ->
	  (
	    printAmbiguousMoves pieceToMove ;
	    raise Wrong_move
	  )

    else if Str.string_match (Str.regexp "^[a-h][1-8][a-h][1-8]$") x 0 then

      translateDoublePoint x

    else if Str.string_match (Str.regexp "^O-O-O$") x 0 then

      let kingPosition =
	List.hd (self#getPiecePositions (curPlayer, "King"))
      in

      let rookPosition =
	(* let searchQueensideRook =  *)
	List.hd (self#getPiecePositions (curPlayer, "Rook"))
      in

      [rookPosition; kingPosition]

    else if Str.string_match (Str.regexp "^O-O$") x 0 then

      let kingPosition =
	List.hd (self#getPiecePositions (curPlayer, "King"))
      in

      let rookPosition =
	List.hd (List.tl (self#getPiecePositions (curPlayer, "Rook")))
      in

      [rookPosition; kingPosition]

    else
      raise Bad_arguments


  method translatePromotion x =

    let x = Str.global_replace (Str.regexp "[x+]") "" x in

    if Str.string_match (Str.regexp "^[a-h][1-8]=[KQNRB]$") x 0 then
      let pieceMarker = (String.sub x 3 1) in
      self#getPieceType pieceMarker
    else
      ""

  method private buildMove theBoard thePositions thePlayer =
  object (selfMove)

    val curBoard = theBoard
    method private getBoard = curBoard

    val mutable pStart = (0, 0)
    val mutable pEnd = (0, 0)

    val mutable moveResult = Improper_move
    method getTypeOfMove () = moveResult

    method isCaptureMove () =
      match moveResult with
	| Capture_move | In_passing -> true
	| Simple_move | Special_move | Improper_move -> false

    val mutable curCapturedPiece : 'a piece option = None
    method getCapturedPiece () = (<--) curCapturedPiece

    val mutable curPiece = None
    method private getCurPiece () = (<--) curPiece

    val mutable firstPieceMove = false
    method private setFirstMove () = firstPieceMove <- true
    method private getFirstMove () = firstPieceMove


    initializer
      let piecesList = curBoard#getPiecesList () in
      let translatedPositions = self#translatePositions thePositions piecesList thePlayer in
      pStart <- List.hd translatedPositions ;
      pEnd <- List.hd (List.tl translatedPositions) ;

      if selfMove#getBoard#isItValidPosition pStart = Out_of_bounds
      || selfMove#getBoard#isItValidPosition pEnd = Out_of_bounds
      then
	raise Boundedness

      else if selfMove#getBoard#checkColorInPosition pStart thePlayer = Different
      then
	raise Wrong_color

      else
	curPiece <- (-->) (selfMove#getBoard#getPiece pStart)


    method checkThisMove pieceColor pieceType pieceStart pieceEnd =
      if (selfMove#getCurPiece ())#getColor = pieceColor
      && (selfMove#getCurPiece ())#getType = pieceType
      && pieceStart = pStart
      && pieceEnd = pEnd
      then true else false


    method private kingCouldBeCaptured () =
      let piecesOnBoard = curBoard#getPiecesList () in

      let theKing = List.hd
	(
	  List.fold_right
	    (
	      fun curPiece listResult ->
		if curPiece#getColor = thePlayer
		&& curPiece#getType = "King"
		  && curPiece#isOnBoard ()
		then
		  curPiece :: listResult
		else
		  listResult
	    ) piecesOnBoard []
	)
      in

      theKing#pieceCouldBeCaptured theKing#getPosition


    method private isPromotion pieceType pEnd =
      if self#translatePromotion thePositions <> ""
      && pieceType = "Pawn"
      && ((snd pEnd = (snd boardSize) - 1) || (snd pEnd = 0))
      then true else false


    method private makePromotion () =
      let newPieceType = self#translatePromotion thePositions in
      let newPiece = self#buildPiece thePlayer newPieceType in

      let _ = selfMove#getBoard#grabPiece pEnd in
      selfMove#getBoard#putPiece pEnd newPiece ;

      let newPiecesList =
	(selfMove#getBoard#getPiece pEnd) ::
	  (selfMove#getBoard#getPiecesList ()) in
      selfMove#getBoard#setPiecesList newPiecesList


    method private revertPromotion () =
      let _ = selfMove#getBoard#grabPiece pEnd in
      selfMove#getBoard#putPiece pEnd (selfMove#getCurPiece ()) ;

      let newPiecesList = List.tl (selfMove#getBoard#getPiecesList ()) in
      selfMove#getBoard#setPiecesList newPiecesList


    method doIt () =
      (
      let result = (selfMove#getCurPiece ())#checkMove pEnd in

      match result with
	| Simple_move ->

	  let grabbedPiece = selfMove#getBoard#grabPiece pStart in
    	  selfMove#getBoard#putPiece pEnd grabbedPiece ;
	  moveResult <- result ;
	  if not (grabbedPiece#getMoved ()) then
	    (
	      grabbedPiece#setMoved () ;
	      selfMove#setFirstMove ()
	    ) ;

	  let curPieceType = (selfMove#getCurPiece ())#getType in
	  if selfMove#isPromotion curPieceType pEnd then
	    selfMove#makePromotion ()

	| Capture_move ->

	  let grabbedPiece = selfMove#getBoard#grabPiece pStart in
	  let capturedPiece = selfMove#getBoard#grabPiece pEnd in
    	  selfMove#getBoard#putPiece pEnd grabbedPiece ;
	  curCapturedPiece <- (-->) capturedPiece ;
	  moveResult <- result ;
	  if not (grabbedPiece#getMoved ()) then
	    (
	      grabbedPiece#setMoved () ;
	      selfMove#setFirstMove ()
	    ) ;

	  let curPieceType = (selfMove#getCurPiece ())#getType in
	  if selfMove#isPromotion curPieceType pEnd then
	    selfMove#makePromotion ()

	| Special_move -> (* castling *)

	  let grabbedKing = selfMove#getBoard#grabPiece pEnd in
	  let grabbedRook = selfMove#getBoard#grabPiece pStart in

	  let pDiff = pStart -- pEnd in

	  let newKingPosition =
	    if fst pDiff < 0 then
	      pEnd ++ (-2, 0)
	    else
	      pEnd ++ (+2, 0)
	  in

	  let newRookPosition =
	    if fst pDiff < 0 then
	      newKingPosition ++ (+1, 0)
	    else
	      newKingPosition ++ (-1, 0)
	  in

	  selfMove#getBoard#putPiece newKingPosition grabbedKing ;
	  selfMove#getBoard#putPiece newRookPosition grabbedRook ;
	  moveResult <- result ;
	  if not (grabbedKing#getMoved ()) && not (grabbedRook#getMoved ()) then
	    (
	      grabbedKing#setMoved () ;
	      grabbedRook#setMoved () ;
	      selfMove#setFirstMove ()
	    )

	| In_passing ->

	  let pDiff = pEnd -- pStart in
	  let opposingPawn = pEnd -- (0, snd pDiff) in

	  let grabbedPiece = selfMove#getBoard#grabPiece pStart
	  and capturedPiece = selfMove#getBoard#grabPiece opposingPawn
	  in

    	  selfMove#getBoard#putPiece pEnd grabbedPiece ;
	  curCapturedPiece <- (-->) capturedPiece ;
	  moveResult <- result ;
	  if not (grabbedPiece#getMoved ()) then
	    (
	      grabbedPiece#setMoved () ;
	      selfMove#setFirstMove ()
	    )

	| Improper_move -> raise Wrong_move
      ) ;

      if selfMove#kingCouldBeCaptured () then
	(
	  let _ = selfMove#undoIt () in
	  print_endline "King check!" ;
	  raise Wrong_move
	)


    method undoIt () =
      match moveResult with
	| Simple_move ->

	  let curPieceType = (selfMove#getCurPiece ())#getType in
	  if selfMove#isPromotion curPieceType pEnd then
	    selfMove#revertPromotion () ;

	  let grabbedPiece = selfMove#getBoard#grabPiece pEnd in
	  selfMove#getBoard#putPiece pStart grabbedPiece ;
	  if selfMove#getFirstMove () then
	    (
	      grabbedPiece#unsetMoved ()
	    ) ;
	  grabbedPiece#getColor

	| Capture_move ->

	  let curPieceType = (selfMove#getCurPiece ())#getType in
	  if selfMove#isPromotion curPieceType pEnd then
	    selfMove#revertPromotion () ;

	  let grabbedPiece = selfMove#getBoard#grabPiece pEnd in
	  selfMove#getBoard#putPiece pStart grabbedPiece ;
	  selfMove#getBoard#putPiece pEnd ((<--) curCapturedPiece) ;
	  if selfMove#getFirstMove () then
	    (
	      grabbedPiece#unsetMoved ()
	    ) ;
	  grabbedPiece#getColor

	| Special_move ->

	  let pDiff = pStart -- pEnd in

	  let curKingPosition =
	    if fst pDiff < 0 then
	      pEnd ++ (-2, 0)
	    else
	      pEnd ++ (+2, 0)
	  in

	  let curRookPosition =
	    if fst pDiff < 0 then
	      curKingPosition ++ (+1, 0)
	    else
	      curKingPosition ++ (-1, 0)
	  in

	  let grabbedKing = selfMove#getBoard#grabPiece curKingPosition
	  and grabbedRook = selfMove#getBoard#grabPiece curRookPosition
	  in

	  selfMove#getBoard#putPiece pEnd grabbedKing ;
	  selfMove#getBoard#putPiece pStart grabbedRook ;
	  if selfMove#getFirstMove () then
	    (
	      grabbedKing#unsetMoved () ;
	      grabbedRook#unsetMoved ()
	    ) ;
	  grabbedKing#getColor

	| In_passing ->

	  let pDiff = pEnd -- pStart in
	  let opposingPawn = pEnd -- (0, snd pDiff) in

	  let grabbedPiece = selfMove#getBoard#grabPiece pEnd in

	  selfMove#getBoard#putPiece pStart grabbedPiece ;
	  selfMove#getBoard#putPiece opposingPawn ((<--) curCapturedPiece) ;
	  if selfMove#getFirstMove () then
	    (
	      grabbedPiece#unsetMoved ()
	    ) ;
	  grabbedPiece#getColor

	| Improper_move -> raise Wrong_move


    method print () =
      let rec printMove moveType =
	match moveType with
	  | Simple_move ->
	    let movedPiece = selfMove#getCurPiece () in

	    let pieceColor = movedPiece#getColor
	    and pieceType = movedPiece#getType
	    in

	    let pStartStr = string_of_position pStart
	      (self#boardFileOfInt)
	      (fun x -> string_of_int (x + 1))
	    in

	    let pEndStr = string_of_position pEnd
	      (self#boardFileOfInt)
	      (fun x -> string_of_int (x + 1))
	    in

	    print_string
	      (
		pieceColor ^ " " ^ pieceType ^
		  " from " ^ pStartStr ^
		  " to " ^ pEndStr
	      )

	| Capture_move ->

	  printMove Simple_move ;
	  print_string
	    (
	      ": it capture a " ^
		(selfMove#getCapturedPiece ())#getType
	    )

	| Special_move ->

	  let pDiff = pStart -- pEnd in

	  let whichSide =
	    if fst pDiff > 0 then
	      "kingside"
	    else
	      "queenside"
	  in

	  print_string
	    (
	      (selfMove#getCurPiece ())#getColor ^
	      " makes castling on " ^ whichSide
	    )

	| In_passing ->

	  printMove Simple_move ;
	  print_string
	    (
	      ": it capture a " ^
		(selfMove#getCapturedPiece ())#getType ^
		" en passant"
	    )

	| Improper_move -> raise Wrong_move

      in

      printMove moveResult ;
      print_newline ()

  end

end




class ['a] capturedPieces : ['a] captured =
object

  constraint 'a = 'b piece

  val mutable setOfPieces : 'a list = []


  method addPiece x =
    setOfPieces <- x :: setOfPieces


  method delLastOne () =
    match setOfPieces with
      | [] -> setOfPieces <- []
      | _ :: t -> setOfPieces <- t


  method private revertPiece (pieceColor, pieceType) =

    let rec revert x (o, l) =
      match (o, l) with
	  (_, []) -> (None, [])
	| (_, h :: t) ->
	  if h#getColor = pieceColor && h#getType = pieceType then
	    ((-->) h, t)
	  else
	    let result = revert x (None, t) in
	    (fst result, h :: (snd result))
    in

    let result = revert (pieceColor, pieceType) (None, setOfPieces) in
    let pieceFound = fst result in
    let purgedList = snd result in

    if pieceFound <> None then
      (
	setOfPieces <- purgedList ;
	(<--) pieceFound
      )
    else
      raise Emptiness


  method howManyPieces () =
    List.length setOfPieces

end




class ['a] gameHistory : ['a] history =
object (self)

  val mutable setOfMoves : 'a move list = []

  val curCaptured : 'a piece captured = new capturedPieces
  method private getCaptured = curCaptured


  method private areThereMoves () =
    match setOfMoves with
      |	[] -> Empty
      | _ -> Not_empty


  method getLastMove () =
    match setOfMoves with
      |	[] -> raise Emptiness
      | h :: _ -> h


  method private popLastMove () =
    match setOfMoves with
      |	[] -> raise Emptiness
      | h :: t -> (setOfMoves <- t; h)


  method addMove newMove =
    setOfMoves <- newMove :: setOfMoves ;
    if newMove#isCaptureMove () then
      self#getCaptured#addPiece (newMove#getCapturedPiece ())


  method undoMove n =

    let rec undoMove l n p =
      match (l, n, p) with
	| (_, 0, p) | ([], _, p) -> p
	| (_ :: t, n, _) ->
	  (
	    let lastOne = self#popLastMove () in
	    if lastOne#isCaptureMove () then
	      self#getCaptured#delLastOne () ;
	    undoMove t (n - 1) (lastOne#undoIt ())
	  )
    in

    undoMove setOfMoves n ""


  method howManyMoves () =
    List.length setOfMoves


  method howManyCapturedPieces () =
    self#getCaptured#howManyPieces ()


  method print n =

    let rec print i l n =
      match (l, n) with
	| (_, 0) | ([], _) -> ()
	| (h :: t, n) ->
	  (
	    print_string ((string_of_int i) ^ " ") ;
	    h#print () ;
	    print (i - 1) t (n - 1)
	  )
    in

    print (self#howManyMoves ()) setOfMoves n

end




class ['a] boardgame (gameRules : 'a) : ['a] game =
object (self)

  inherit ['a] commonGame

  constraint 'a = 'b #rules

  val curRules = gameRules
  val curBoard = gameRules#getBoard ()
  val curHistory : 'a history = new gameHistory
  val mutable curPlayer : string = gameRules#getFirstPlayer ()

  method private getRules = curRules
  method private getBoard = curBoard
  method private getHistory = curHistory


  method private nextOne () =
    match curPlayer with
      | "White" -> curPlayer <- "Black"
      | "Black" -> curPlayer <- "White"
      | _ -> raise Unknown_color


  method private prevOne () = self#nextOne ()


  method init () =
    self#getRules#setupBoard curBoard curHistory ;
    self#print ()


  method private print () =
    self#getBoard#print () ;
    print_string ("Current player: " ^ curPlayer ^ "\n") ;
    print_string ("Captured pieces: " ^
    		     string_of_int (self#getHistory#howManyCapturedPieces ()) ^ "\n")


  method private history n =
    if self#getHistory#howManyMoves () <> 0 then
      self#getHistory#print n
    else
      print_string "Make the first move!\n"


  method private undo n =
    if self#getHistory#howManyMoves () <> 0 then
      curPlayer <- self#getHistory#undoMove n
    else
      print_string "Make the first move!\n"


  method private move (positions : string) =
    let curMove = self#getRules#getMove self#getBoard positions curPlayer in
    curMove#doIt () ;
    self#getHistory#addMove curMove ;
    self#nextOne ()


  method private restart () =
    let numberOfMoves = self#getHistory#howManyMoves () in
    if numberOfMoves <> 0 then
      self#undo numberOfMoves ;
    self#print ()


  method private play () =
    while true do
      try
	self#insertCommand ()
      with
	| Unknown_command -> print_string "Unknown command\n"
    done


  method private insertCommand () : unit =
    print_string "$ " ;
    let curCommand = read_line () in
    let tokenizedCommand = Str.split (Str.regexp "[ \t]+") curCommand in

    let makeMoves listOfMoves =
      try
	let isMove x =
	  if Str.string_match (Str.regexp "^[0-9]") x 0
	  then false else true
	in

	if List.length listOfMoves > 0 then
	  List.iter (
	    fun x ->
	      if isMove x then
		(self#move x ; self#print () )
	  ) listOfMoves

	else
	  raise Bad_arguments
      with
	| Boundedness -> print_endline "Positions are out of bounds!"
	| Wrong_move -> print_endline "It's a wrong move, try again!"
	| Square_is_empty -> print_endline "Have a look on the board, this square is empty!"
	| Wrong_color -> print_endline "You should grab one of your pieces!"
 	| Bad_arguments -> print_endline "Wrong syntax!"
	| Failure (s) -> print_endline s
	| Invalid_argument (s) -> print_endline s
    in

    match tokenizedCommand with
      | "move" :: t ->
	  makeMoves t

      | "history" :: t ->
	(
	  try
	    if t = [] then
	      let numberOfMoves = self#getHistory#howManyMoves () in
	      self#history numberOfMoves
	    else
	      let movesToPrint = int_of_string (List.hd t) in
	      self#history movesToPrint
	  with
	    | Failure ("int_of_string") -> print_string "Wrong syntax: history [number of moves]\n"
	)

      | "undo" :: t ->
	(
	  try
	    if t = [] then
	      self#undo 1
	    else
	      let movesToUndo = int_of_string (List.hd t) in
	      self#undo movesToUndo
	  with
	    | Failure ("int_of_string") -> print_string "Wrong syntax: history [number of moves]\n"
	)

      | "print" :: [] ->
	self#print ()

      | "restart" :: [] ->
	self#restart ()

      | "new" :: [] ->
	raise New_game

      | "quit" :: [] ->
	raise Quit

      | "help" :: [] ->
	(
	  print_string "move x : x must be written in short algebraic notation\n" ;
	  print_string "undo [number of moves]\n" ;
	  print_string "history [number of moves]\n" ;
	  print_string "print : print the board\n" ;
	  print_string "restart : restart current game\n" ;
	  print_string "new : choose a new game\n" ;
	  print_string "test file_name\n" ;
	  print_string "quit : quit the program\n"
	)

      | "test" :: t ->
	(
	  try

	    if List.length t <= 0 then
	      raise Bad_arguments ;

	    let fileToRead = List.hd t in

	    let input_line inChannel =
	      try Some (input_line inChannel)
	      with End_of_file -> None
	    in

	    let read_lines inChannel =
	      let rec read_lines result =
		match input_line inChannel with
		  | Some line -> read_lines (line :: result)
		  | None -> (List.rev result)
	      in
	      read_lines []
	    in

	    let inChannel = open_in fileToRead in
	    let lines = read_lines inChannel in
	    let _ = close_in inChannel in

	    let listOfMoves =
	      List.fold_right
		(
		  fun x listResult ->
		    if Str.string_match (Str.regexp "^[0-9a-zA-Z]") x 0 then
		      (Str.split (Str.regexp "[ \t]+") x) @ listResult
		    else
		      listResult
		) lines []
	    in

	    makeMoves listOfMoves

	  with
 	    | Bad_arguments -> print_endline "Wrong syntax: test (file name)"
	    | Sys_error (s) -> print_endline ("Error: " ^ s)
	)

      | [] -> ()

      | _ -> raise Unknown_command

end



class main =
object (self : 'self)

  method private gameRules x =
    match x with
      | "chess" -> new chess
      (* | "italian draughts" -> new draughts *)
      (* | "go" -> new go *)
      | _ -> raise Not_found


  method private chooseRules () =
    print_string "Choose the game? " ;
    let gameName = removeBlanks (read_line ()) in
    if gameName <> "" then
      self#gameRules gameName
    else
      raise Continue


  initializer
    (
      try
	(* let newRules = self#chooseRules () in *)
	let newRules = new chess in
	let newGame = new boardgame newRules in
	newGame#init () ;
	newGame#play ()
      with
	| Continue | New_game -> ()
	| Quit | End_of_file -> exit 0
	| Not_found -> print_string "Unknown rules\n"

    ) ;
    let _ = new main in ()

end




let _ = new main

