#nowarn "40"

(* difficult
0 0 8   0 9 0   0 5 6
0 0 0   0 0 0   0 2 0
9 0 0   5 0 3   1 0 0

0 1 0   0 7 8   6 0 5
0 0 0   0 0 0   0 0 0
6 0 5   1 4 0   0 8 0

0 0 1   6 0 7   0 0 3
0 2 0   0 0 0   0 0 0
4 6 0   0 1 0   5 0 0 
*)
let rec grid1 = "..8.9..56.......2.9..5.31...1..786.5.........6.514..8...16.7..3.2.......46..1.5.."
    
(* fiendish
0 0 4   7 0 2   0 8 0
3 5 0   0 0 9   0 6 0
0 0 0   0 0 0   0 0 1

8 9 0   5 0 6   0 0 7
0 0 0   0 0 0   0 0 0
6 0 0   3 0 4   0 5 9

7 0 0   0 0 0   0 0 0
0 8 0   2 0 0   0 1 6
0 2 0   6 0 5   4 0 0 
*)
and grid2 = "..47.2.8.35...9.6.........189.5.6..7.........6..3.4.597.........8.2...16.2.6.54.."    


and readDigit = function 
 | '.' -> 0
 | '1' -> 1
 | '2' -> 2
 | '3' -> 3
 | '4' -> 4
 | '5' -> 5
 | '6' -> 6
 | '7' -> 7
 | '8' -> 8
 | '9' -> 9
 | _   -> failwith "Illegal initialiser character"
 
and readGrid str = 
  [| for c in str -> readDigit c |]
 
and grid = readGrid grid1

(* deep copy *)
and initgrid = [ for x in grid do yield x ]
   
(* grid index to column axis *)
and g2c (g : int) = g % 9
(* grid index to row axis *)
and g2r (g : int) = g /9
(* grid index to box axis *)
and g2b (g : int) = (g / 27) * 3 + ((g / 3) % 3)
(* box base indices *)
and bb x = [| 0; 3; 6; 27; 30; 33; 54; 57; 60 |].[x]
(* box element offset *)
and bo x = [| 0; 1; 2; 9; 10; 11; 18; 19; 20 |].[x]

(* Find placed digits allowed at a grid point, g
   The list of allowed digits is always sorted   *)
and allowed g = 
  let r = g2r g * 9      (* Row base *)
  let c = g2c g          (* Col base *)
  let b = g |> g2b |> bb (* Box base *)
  in 
    if grid.[g] <> 0
    then []              // Already determined
    (* Enumerate illegal values and invert for the candidate set *)
    else invert [ for i in 0..8 do yield! [ grid.[i + r]; grid.[9 * i + c]; grid.[bo i + b] ] ] 

(* Invert a list of digits into an ordered result *)
and invert lst =
  let s = set lst 
  [ for i in 1..9 do if not (s.Contains(i)) then yield i ]
  
(* Solve the grid from cell g onwards with continuation cont *)
and solveFrom g (grid : int array) init cont =
  if g = 81 // Exit when we are done
  then cont grid
  else
    (* If this cell is empty *)
    if grid.[g] = 0
    then
      (* Get the candidates and try them one-by-one *)
      let pd = g |> allowed in
        for digit in pd do
            (* Place trial digit *)
            grid.[g] <- digit
            (* Try to solve the rest *)
            solveFrom (g + 1) grid init cont 
            (* No good, revert to a previous *)
            grid.[g] <- initgrid.[g]
        done
    else
      (* Preset cell - try the next one *)
      solveFrom (g + 1) grid init cont


(* Dump the completed grid *)
and printGrid = fun (grid : int array) ->
  for y = 0 to 8 do
    for x = 0 to 8 do
      printf "  %1d" grid.[x + 9 * y] 
    done;
    printf "\r\n"
  done;

(* Start the fans please! *)     
solveFrom 0 grid initgrid (fun (grid) -> printGrid grid)
System.Console.ReadLine () |> ignore

(*
Terminology

Cell        A single digit cell
Row         A row of 9 cells (or 3 cells if further restricted to a box)
Column      A column of 9 cells (or 3 cells if further restricted to a box)
Box         One of 9 3x3 blocks of cells
Boxrow      A row of 3 boxes
Boxcolumn   A column of 3 boxes
Group       A row, column or box of 9 cells with distinct digits

Other tests

If digit d only appears in one row/column in a box then it may be eliminated from that row/column in boxes on the same boxcolumn/boxrow

If digit d only appears in two rows/columns across two boxes in the same boxcolumn/boxrow then it may be eliminated from both rows/columns
in the third box in the boxcolumn/boxrow

If the union of the possible digits in n cells of a group has n members then those digits can be eliminated from all other cells of the group

If the only possible ocurrences of a subset of n digits exist within a subset of n cells of the group then all other digits may be eliminated
from that subset of cells. 

(in both prior cases it can be deduced that the n digits fill the n cells even if the precise placement is unknown)
 
*)