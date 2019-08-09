open OUnit
open Command
open State
open Board

(** Command tests *)
let command_tests = [
  "valid start command" >:: (fun _ ->
      assert_equal (Command.parse "  start  ") Start
    );

  "valid start command upper case" >:: (fun _ ->
      assert_equal (Command.parse "  START  ") Start
    );

  "invalid start command" >:: (fun _ ->
      assert_raises Malformed (fun _ -> Command.parse "start foo")
    );

  "valid build command" >:: (fun _ ->
      assert_equal (Command.parse "build settlement 1 1 1")
        (Build (Settlement,[1;1;1]))
    );

  "invalid build command 1" >:: (fun _ ->
      assert_raises Malformed (fun _ -> Command.parse "build cornell 1 1 1")
    );

  "invalid build command 2" >:: (fun _ ->
      assert_raises Malformed (fun _ -> Command.parse "build settlement 1")
    );

  "valid costs command" >:: (fun _ ->
      assert_equal (Command.parse "  costs  ") Costs
    );

  "invalid costs command" >:: (fun _ ->
      assert_raises Malformed (fun _ -> Command.parse "costs foo")
    );

  "valid costs command" >:: (fun _ ->
      assert_equal (Command.parse "  costs  ") Costs
    );

  "invalid costs command" >:: (fun _ ->
      assert_raises Malformed (fun _ -> Command.parse "costs foo")
    );

]
(** Board Tests *)
let st = State.init

let tiles = State.sort_by_gridnum (State.get_tiles st)
let loc1 = (List.nth tiles 16)::(List.nth tiles 23)::(List.nth tiles 24)::[]

let rec print_tuple_list lst =
  match lst with
  | [] -> ""
  | (h1,h2)::t -> "("^(string_of_int h1)^", "^(string_of_int h2)^")"^" "
                  ^print_tuple_list t

let board_tests = [
  "get verts tile 0" >:: (fun _ ->
      assert_equal [(60,10);(67,10);(71,13);
                    (67,16);(60,16);(56,13)] (Board.get_hex_vertices 
                                                (List.nth tiles 0))
        ~printer:print_tuple_list
    );
  "get verts tile 16" >:: (fun _ ->
      assert_equal [(82,22);(89,22);(93,25);
                    (89,28);(82,28);(78,25)] (Board.get_hex_vertices 
                                                (List.nth tiles 16))
        ~printer:print_tuple_list
    );
  "get verts tile 23" >:: (fun _ ->
      assert_equal [(93,19);(100,19);(104,22);
                    (100,25);(93,25);(89,22)] (Board.get_hex_vertices 
                                                 (List.nth tiles 23))
        ~printer:print_tuple_list
    );
  "get verts tile 24" >:: (fun _ ->
      assert_equal [(93,25);(100,25);(104,28);
                    (100,31);(93,31);(89,28)] (Board.get_hex_vertices
                                                 (List.nth tiles 24))
        ~printer:print_tuple_list
    );
  "compute vertex list 16 23 24" >:: (fun _ ->
      assert_equal [[(82,22);(89,22);(93,25);
                     (89,28);(82,28);(78,25)];
                    [(93,19);(100,19);(104,22);
                     (100,25);(93,25);(89,22)];
                    [(93,25);(100,25);(104,28);
                     (100,31);(93,31);(89,28)]
                   ] (Board.compute_vertex_list [] loc1) 
    );
  "are verts equal (82,22) true" >:: (fun _ ->
      assert_equal true (Board.are_verts_equal (82,22) (82,22))
    );
  "are verts equal false" >:: (fun _ ->
      assert_equal false (Board.are_verts_equal (0,1) (1,0))
    );
  "is_in_list empty list" >:: (fun _ ->
      assert_equal false (Board.is_in_list (1,1) [])
    );
  "is_in_list one element in one element list" >:: (fun _ ->
      assert_equal true (Board.is_in_list (1,2) [(1,2)])
    );
  "is_in_list one element in multi element list" >:: (fun _ ->
      assert_equal true (Board.is_in_list (2,2) [(1,1);(2,2);(3,3)])
    );
  "is_in_list one element not in multi element list" >:: (fun _ ->
      assert_equal false (Board.is_in_list (3,4) [(3,3);(4,4)])
    );
  "check_for_vert_in_lists not in any list" >:: (fun _ ->
      assert_equal false (Board.check_for_vert_in_lists (1,1) [
          [(2,2)];
          [(3,3)];
          [(4,4)]
        ]));
  "check_for_vert_in_lists in every single elt list" >:: (fun _ ->
      assert_equal true (Board.check_for_vert_in_lists (1,1) [
          [(1,1)];
          [(1,1)];
          [(1,1)]
        ]));
  "check_for_vert_in_lists in every multi-elt list" >:: (fun _ ->
      assert_equal true (Board.check_for_vert_in_lists (1,1) [
          [(2,1);(3,3);(1,1)];
          [(1,1);(2,2);(4,4)];
          [(8,9);(4,3);(1,1)]
        ]));
  "check_for_vert_in_lists not in every real list" >:: (fun _ ->
      assert_equal false (Board.check_for_vert_in_lists (1,1) [
          [(82,22);(89,22);(93,25);
           (89,28);(82,28);(78,25)];
          [(93,19);(100,19);(104,22);
           (100,25);(93,25);(89,22)];
          [(93,25);(100,25);(104,28);
           (100,31);(93,31);(89,28)]
        ]));
  "check_for_vert_in_lists in every real list" >:: (fun _ ->
      assert_equal true (Board.check_for_vert_in_lists (93,25) [
          [(82,22);(89,22);(93,25);
           (89,28);(82,28);(78,25)];
          [(93,19);(100,19);(104,22);
           (100,25);(93,25);(89,22)];
          [(93,25);(100,25);(104,28);
           (100,31);(93,31);(89,28)]
        ]))
]

let suite =
  "test suite for A2"  >::: List.flatten [
    command_tests;
    board_tests
  ]

let _ = run_test_tt_main suite
