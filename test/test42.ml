(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

type arrtype1 = int array
type rectype1 = { mutable name : string;
                  mutable address : string;
                  mutable id : int;
                  mutable age : int }
type arrtype2 = rectype1 array
type rectype2 = { mutable name2 : string;
                  mutable dates2 : arrtype1 }
type arrtype3 = string array

let x = ref 9
let arr1 = [0] ** 10
let arr2 = [{name="aname"; address="somewhere"; id=0; age=0}] ** 5
let arr3 = [""] ** 100
let rec1 = {name="Kapoios"; address="Kapou"; id=02432; age=44}
let rec2 = {name2="Allos"; dates2=[1900]**3}

let _ =
  x := 10;
  arr1.(0) <- 1;
  arr1.(9) <- 3;
  arr2.(3).name <- "xyz";
  arr2.(1).age <- 23;
  arr3.(34) <- "sfd";

  rec1.name <- "sdf";
  rec2.dates2.(0) <- 2323;
  rec2.dates2.(2) <- 2323


