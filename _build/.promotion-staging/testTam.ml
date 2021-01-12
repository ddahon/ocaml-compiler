open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  (*Sys.remove tamfile;    *)(* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)

let%expect_test "testprintint" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintint.rat";
  [%expect{| 42 |}]

let%expect_test "testprintbool" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintbool.rat";
  [%expect{| true |}]

let%expect_test "testprintrat" =
   runtam "../../fichiersRat/src-rat-tam-test/testprintrat.rat";
   [%expect{| [4/5] |}]

let%expect_test "testaddint" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddint.rat";
  [%expect{| 42 |}]

let%expect_test "testaddrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddrat.rat";
  [%expect{| [7/6] |}]

let%expect_test "testmultint" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultint.rat";
  [%expect{| 440 |}]

let%expect_test "testmultrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultrat.rat";
  [%expect{| [14/3] |}]

let%expect_test "testnum" =
  runtam "../../fichiersRat/src-rat-tam-test/testnum.rat";
  [%expect{| 4 |}]

let%expect_test "testdenom" =
  runtam "../../fichiersRat/src-rat-tam-test/testdenom.rat";
  [%expect{| 7 |}]

let%expect_test "testwhile1" =
  runtam "../../fichiersRat/src-rat-tam-test/testwhile1.rat";
  [%expect{| 19 |}]

let%expect_test "testif1" =
  runtam "../../fichiersRat/src-rat-tam-test/testif1.rat";
  [%expect{| 18 |}]

let%expect_test "testif2" =
  runtam "../../fichiersRat/src-rat-tam-test/testif2.rat";
  [%expect{| 21 |}]

let%expect_test "factiter" =
  runtam "../../fichiersRat/src-rat-tam-test/factiter.rat";
  [%expect{| 120 |}]

let%expect_test "complique" =
  runtam "../../fichiersRat/src-rat-tam-test/complique.rat";
  [%expect{| [9/4][27/14][27/16][3/2] |}]

let%expect_test "factfun1" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun1.rat";
  [%expect{| 1 |}]

let%expect_test "factfun2" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun2.rat";
  [%expect{| 7 |}]

let%expect_test "factfun3" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun3.rat";
  [%expect{| 10 |}]

let%expect_test "factfun4" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun4.rat";
  [%expect{| 10 |}]

let%expect_test "factfuns" =
  runtam "../../fichiersRat/src-rat-tam-test/testfuns.rat";
  [%expect{| 28 |}]

let%expect_test "factrec" =
  runtam "../../fichiersRat/src-rat-tam-test/factrec.rat";
  [%expect{| 120 |}]

(* Tests de la surcharge *)
let%expect_test "testSurchargeSimple" =
  runtam "../../fichiersRat/src-rat-tam-test/testSurchargeSimple.rat";
  [%expect{| 3 |}]

let%expect_test "testSurchargeComplexe" =
  runtam "../../fichiersRat/src-rat-tam-test/testSurchargeComplexe.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Exceptions.TypeInattendu(1, 2)")
  Raised at file "passeTypeRat.ml", line 162, characters 11-38
  Called from file "list.ml", line 88, characters 20-23
  Called from file "list.ml", line 88, characters 32-39
  Called from file "passeTypeRat.ml", line 167, characters 11-51
  Called from file "compilateur.ml", line 25, characters 16-35
  Called from file "compilateur.ml", line 97, characters 28-57
  Called from file "testTam.ml", line 9, characters 16-32
  Called from file "testTam.ml" (inlined), line 21, characters 15-46
  Called from file "testTam.ml", line 111, characters 2-71
  Called from file "collector/expect_test_collector.ml", line 253, characters 12-19 |}]