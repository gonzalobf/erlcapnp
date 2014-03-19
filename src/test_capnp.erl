%% @author bucko
%% @doc @todo Add description to test_capnp.


-module(test_capnp).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

test() ->
	Schema = capnp_bootstrap:load_raw_schema("/home/bucko/eclipse-workspace/capnp/data/tests/test1.raw"),
	<<0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,76,0,0,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestBoringInteger">>, 76}, Schema)),
	<<0,0,0,0,5,0,0,0,0,0,0,0,4,0,0,0,3,0,0,0,0,0,0,0,6,0,0,0,2,0,8,255,246,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestMultipleIntegers">>, 3, 6, 2, 8, -1, -10, 17}, Schema)),
	<<0,0,0,0,3,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,76,0,0,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestBoringPointer">>, {<<"test1.capnp:TestBoringInteger">>, 76}}, Schema)),
	<<0,0,0,0,10,0,0,0,0,0,0,0,1,0,2,0,255,254,0,0,0,0,0,0,4,0,0,0,0,0,1,0,8,0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,76,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,6,0,0,0,2,0,8,255,246,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestLessBoringPointer">>, {<<"test1.capnp:TestBoringPointer">>, {<<"test1.capnp:TestBoringInteger">>, 76}}, -257, {<<"test1.capnp:TestMultipleIntegers">>, 3, 6, 2, 8, -1, -10, 17}}, Schema)),
	<<0,0,0,0,5,0,0,0,0,0,0,0,0,0,2,0,5,0,0,0,42,0,0,0,5,0,0,0,26,0,0,0,70,111,111,33,0,0,0,0,102,111,111,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestTextType">>, <<"Foo!">>, <<"foo">>}, Schema)),
	<<0,0,0,0,11,0,0,0,0,0,0,0,0,0,2,0,5,0,0,0,22,0,0,0,17,0,0,0,22,0,0,0,9,0,0,0,50,0,0,0,1,0,0,0,50,0,0,0,70,111,111,32,50,0,0,0,70,111,111,32,49,0,0,0,9,0,0,0,34,0,0,0,1,0,0,0,34,0,0,0,102,111,111,50,0,0,0,0,102,111,111,49,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestTextList">>, [<<"Foo 1">>, <<"Foo 2">>], [<<"foo1">>, <<"foo2">>]}, Schema)),
	<<0,0,0,0,30,0,0,0,0,0,0,0,0,0,2,0,5,0,0,0,22,0,0,0,41,0,0,0,38,0,0,0,21,0,0,0,22,0,0,0,1,0,0,0,22,0,0,0,9,0,0,0,50,0,0,0,1,0,0,0,50,0,0,0,66,97,114,32,50,0,0,0,66,97,114,32,49,0,0,0,9,0,0,0,50,0,0,0,1,0,0,0,50,0,0,0,70,111,111,32,50,0,0,0,70,111,111,32,49,0,0,0,49,0,0,0,22,0,0,0,13,0,0,0,30,0,0,0,9,0,0,0,6,0,0,0,1,0,0,0,14,0,0,0,1,0,0,0,2,0,0,0,25,0,0,0,34,0,0,0,17,0,0,0,34,0,0,0,1,0,0,0,170,0,0,0,84,104,105,115,32,105,115,32,97,32,108,111,110,103,32,98,105,110,97,114,121,0,0,0,98,97,114,50,0,0,0,0,98,97,114,49,0,0,0,0,9,0,0,0,34,0,0,0,1,0,0,0,34,0,0,0,102,111,111,50,0,0,0,0,102,111,111,49,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestTextListList">>, [[<<"Foo 1">>, <<"Foo 2">>], [<<"Bar 1">>, <<"Bar 2">>]], [[<<"foo1">>, <<"foo2">>], [<<"bar1">>, <<"bar2">>, <<"This is a long binary">>], [], [<<>>]]}, Schema)),
	<<0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestEnum">>, <<"testEnum2">>}, Schema)),
	<<0,0,0,0,8,0,0,0,0,0,0,0,0,0,2,0,5,0,0,0,29,0,0,0,13,0,0,0,28,0,0,0,3,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,1,0,2,0,6,0,7,0,8,0,9,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestShortList">>, [{<<"test1.capnp:TestBoringInteger">>, N} || N <- [3, 4, 5]], [{<<"test1.capnp:SimpleShortStruct">>, A, B} || {A, B} <- [{1, 2}, {6, 7}, {8, 9}]]}, Schema)),
	<<0,0,0,0,8,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,30,0,0,0,16,0,0,0,1,0,0,0,8,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,94,0,0,0,0,0,0,0,95,0,0,0,0,0,0,0,93,0,0,0,0,0,0,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestPointerList">>, [{<<"test1.capnp:TestBoringPointer">>, {<<"test1.capnp:TestBoringInteger">>, A}} || A <- [93, 95, 94]]}, Schema)),
	<<0,0,0,0,13,0,0,0,0,0,0,0,0,0,2,0,5,0,0,0,71,0,0,0,37,0,0,0,7,0,0,0,8,0,0,0,4,0,0,0,3,0,0,0,0,0,0,0,6,0,0,0,2,0,8,255,246,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0,97,0,0,0,0,0,0,0,97,0,0,0,97,0,97,97,97,0,0,0,0,0,0,0,97,0,0,0,0,0,0,0,0,0,0,0,1,0,2,0>>
		= capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestCompositeList">>, [{<<"test1.capnp:TestMultipleIntegers">>, 3, 6, 2, 8, -1, -10, 17}, {<<"test1.capnp:TestMultipleIntegers">>, 97, 97, 97, 97, 97, 97, 97}], []}, Schema)).
% TODO RHS
%capnp_writer:envelope(capnp_writer:to_bytes({<<"test1.capnp:TestPrimitiveList">>, [true, false, true], [1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11]}, Schema)).


%% ====================================================================
%% Internal functions
%% ====================================================================


