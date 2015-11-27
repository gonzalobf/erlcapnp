%% @author bucko
%% @doc @todo Add description to capnp_compile.


-module(capnp_compile).

-include_lib("capnp.hrl").
-include_lib("capnp_raw.hrl").
-include_lib("capnp_bootstrap.hrl").

-export([
		to_ast/1,
		to_ast/2,
		load_directly/2,
		self_contained_source/2,
		output_source_with_include/3,
		source_with_include/3,
		output_header/2,
		header_only/2
	]).

-record(field_info, {offset, type, name, default, discriminant}).
-record(native_type, {type, width, extra, binary_options, list_tag}).
-record(ptr_type, {type, extra}).
-record(group_type, {type_id}).

-compile({parse_transform, uberpt}).

load_directly(SchemaFile, ModuleName) ->
	% We go via source to make sure we didn't generate anything kooky.
	Source = self_contained_source(SchemaFile, ModuleName),
	{ok, Tokens, _} = erl_scan:string(Source),
	Forms = split_forms(Tokens, []),
	{ok, ModuleName, BinData, []} = compile:forms(Forms, [debug_info, return]),
	code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", BinData).

self_contained_source(SchemaFile, ModuleName) ->
	{Recs, Funs} = to_ast(SchemaFile),
	Line = 0,
	ModuleFileName = atom_to_list(ModuleName) ++ ".erl",
	Forms = [{attribute,1,file,{ModuleFileName,1}},{attribute,Line,module,ModuleName},{attribute,Line,compile,[export_all]}] ++ Recs ++ massage_bool_list() ++ Funs ++ [{eof,Line}],
	erl_prettypr:format(erl_syntax:form_list(Forms), [{paper, 200}, {ribbon, 200}]).

output_source_with_include(SchemaFile, ModuleName, Path) ->
	io:format("~s", [source_with_include(SchemaFile, ModuleName, Path)]).

source_with_include(SchemaFile, ModuleName, Path) ->
	{_Recs, Funs} = to_ast(SchemaFile),
	Line = 0,
	ModuleFileName = atom_to_list(ModuleName) ++ ".erl",
	IncludeFileName = Path ++ "/" ++ atom_to_list(ModuleName) ++ ".hrl",
	Forms = [{attribute,1,file,{ModuleFileName,1}},{attribute,Line,module,ModuleName},{attribute,Line,include_lib,IncludeFileName},{attribute,Line,compile,[export_all]}] ++ massage_bool_list() ++ Funs ++ [{eof,Line}],
	erl_prettypr:format(erl_syntax:form_list(Forms), [{paper, 200}, {ribbon, 200}]).

output_header(SchemaFile, ModuleName) ->
	io:format("~s", [header_only(SchemaFile, ModuleName)]).

header_only(SchemaFile, ModuleName) ->
	{Recs, _Funs} = to_ast(SchemaFile),
	IncludeFileName = atom_to_list(ModuleName) ++ ".hrl",
	Forms = [{attribute,1,file,{IncludeFileName,1}}] ++ Recs ++ [{eof,0}],
	erl_prettypr:format(erl_syntax:form_list(Forms), [{paper, 200}, {ribbon, 200}]).

to_ast(SchemaFile) when is_list(SchemaFile) ->
	Schema = capnp_bootstrap:load_raw_schema(SchemaFile),
	Tasks = [ {generate_name, Name} || {_, #'capnp::namespace::Node'{''={{1, struct}, _}, displayName=Name}} <- dict:to_list(Schema#capnp_context.by_id)  ],
	to_ast(Tasks, Schema).

to_ast(Name, SchemaFile) when is_list(SchemaFile) ->
	Schema = capnp_bootstrap:load_raw_schema(SchemaFile),
	to_ast(Name, Schema);
to_ast(Name, Schema) when is_binary(Name) ->
	to_ast([{generate_name, Name}], Schema);
to_ast(Tasks, Schema) ->
	to_ast(Tasks, sets:new(), [], [], Schema).

split_forms([Dot={dot,_}|Rest], Acc) ->
	{ok, Form} = erl_parse:parse_form(lists:reverse([Dot|Acc])),
	[Form | split_forms(Rest, [])];
split_forms([Other|Rest], Acc) ->
	split_forms(Rest, [Other|Acc]);
split_forms([], []) ->
	[].

to_ast([], _Done, Recs, Funs, _Schema) ->
	{Recs, Funs};
to_ast([Job|Rest], Done, Recs, Funs, Schema) ->
	case sets:is_element(Job, Done) of
		true ->
			to_ast(Rest, Done, Recs, Funs, Schema);
		false ->
			{NewRecs, NewFuns, NewJobs} = do_job(Job, Schema),
			to_ast(NewJobs ++ Rest, sets:add_element(Job, Done), NewRecs ++ Recs, NewFuns ++ Funs, Schema)
	end.

do_job({generate_name, TypeName}, Schema) ->
	TypeId = dict:fetch(TypeName, Schema#capnp_context.name_to_id),
	{[], [], [{generate, TypeId}]};
do_job({generate, TypeId}, Schema) ->
	generate_basic(TypeId, Schema);
do_job({generate_text, TextType}, _Schema) ->
	% Needed when we have a list of text.
	generate_text().

generate_basic(TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				fields=Fields,
				isGroup=IsGroup,
				discriminantCount=_DiscriminantCount % 0 if there is no anonymous union.
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	% Start by finding the bit offsets of each field, so that we can order them.
	AllFields = [ field_info(Field, Schema) || Field <- Fields ],
	{NotUnionFields, UnionFields} = lists:partition(fun (#field_info{discriminant=undefined}) -> true; (_) -> false end, AllFields),
	{ExplicitGroups, Slots} = lists:partition(fun (#field_info{type=#group_type{}}) -> true; (_) -> false end, NotUnionFields),
	Groups = case UnionFields of
		[] ->
			ExplicitGroups;
		_ ->
			[#field_info{name= <<>>, type=#group_type{type_id=TypeId}}|ExplicitGroups]
	end,
	{PtrFields, DataFields} = lists:partition(fun (#field_info{type=#native_type{}}) -> false; (_) -> true end, Slots),
	% We like these sorted by offset.
	SortedDataFields = lists:sort(DataFields),
	SortedPtrFields = lists:sort(PtrFields),

	Line = 0, % TODO

	RecDef = generate_record_def(Line, TypeId, SortedDataFields, SortedPtrFields, Groups, Schema),

	{RecDefs, NonUnionFunDefs} = case UnionFields of
		[] ->
			{[RecDef], [
				generate_decode_fun(Line, TypeId, SortedDataFields, SortedPtrFields, Schema),
				generate_encode_fun(Line, TypeId, Groups, SortedDataFields, SortedPtrFields, Schema)
			]};
		_ when NotUnionFields == [], IsGroup /= 0 ->
			{[], []};
		_ ->
			{[RecDef], [
				generate_decode_fun(Line, TypeId, SortedDataFields, SortedPtrFields, Schema),
				generate_encode_fun(Line, TypeId, Groups, SortedDataFields, SortedPtrFields, Schema)
			]}
	end,

	UnionFunDefs = case UnionFields of
		[] ->
			[];
		_ ->
			[generate_union_encode_fun(Line, TypeId, UnionFields, Schema)]
	end,

	EnvelopeFunDef = case IsGroup of
		0 ->
			[generate_envelope_fun(Line, TypeId, Schema)];
		1 ->
			[]
	end,

	ExtraTypes1 = [ {generate_name, TypeName} || #field_info{type=#ptr_type{type=struct, extra={TypeName, _, _}}} <- SortedPtrFields ],
	ExtraTypes2 = [ {generate_name, TypeName} || #field_info{type=#ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={TypeName, _, _}}}}} <- SortedPtrFields ],
	ExtraTypes3 = [ {generate, GroupTypeId} || #field_info{type=#group_type{type_id=GroupTypeId}} <- AllFields ],
	ExtraTypes4 = [ {generate_text, TextType} || #field_info{type=#ptr_type{type=list, extra={text, TextType}}} <- SortedPtrFields ],
	{RecDefs, NonUnionFunDefs ++ UnionFunDefs ++ EnvelopeFunDef, ExtraTypes1 ++ ExtraTypes2 ++ ExtraTypes3 ++ ExtraTypes4}.

generate_record_def(Line, TypeId, SortedDataFields, SortedPtrFields, Groups, Schema) ->
	% TODO types, too!
	RecordName = record_name(TypeId, Schema),
	{attribute, Line, record, {RecordName, [{record_field, Line, {atom, Line, field_name(Info)}} || Info=#field_info{} <- SortedDataFields ++ SortedPtrFields ++ Groups]}}.

generate_decode_fun(Line, TypeId, SortedDataFields, SortedPtrFields, Schema) ->
	% Should be function decode_<Name>(<<>>, StartOffset, CompleteMessage) -> #<Name>{}
	% We just take the binary for now and break if we get a pointer type.
	{_, DWords, PWords} = node_name(TypeId, Schema),
	DataMatcher = generate_data_binary(0, SortedDataFields, decode, DWords),
	PtrMatcher = generate_ptr_binary(0, SortedPtrFields, decode, PWords),
	{function, Line, decoder_name(TypeId, Schema), 1,
		[{clause, Line,
				[{bin, Line, DataMatcher ++ PtrMatcher}],
				[],
				[{record, Line, record_name(TypeId, Schema),
						[{record_field, Line, {atom, Line, list_to_atom(binary_to_list(FieldName))}, decoder(Type, var_p(Line, "Var", FieldName), Line)} || #field_info{name=FieldName, type=Type} <- SortedDataFields ++ SortedPtrFields ]
					}]
			}]}.


generate_encode_fun(Line, TypeId, Groups, SortedDataFields, SortedPtrFields, Schema) ->
	% We're going to encode by encoding each type as close to its contents as possible.
	% So a #a{b=#c{}} looks like [ enc(a), enc(b) ].
	% This doesn't quite work for lists, as the complete list must be inlined first.
	% So PtrOffsetWordsFromEnd0 will be 0 for structs, or the distance to the end of the list for lists.
	%
	% function encode_<Type>(#<Type>{}, PtrOffsetWordsFromEnd0) ->
	%     {DataLen0, PtrLen0} = (constant)
	%     ExtraLen0 = 0,
	%
	%     {DataLen1, PtrLen1, ExtraLen1, Data1, Extra1} = encode_<Type1>(<InputData1>, 0),
	%     PtrOffset1 = PtrOffsetWordsFromEnd0 + (PtrLen0 - 1), % Distance /from/ end plus distance /to/ end
	%     Ptr1 = struct_ptr(PtrOffset1, DataLen1, PtrLen1),
	%     PtrOffsetWordsFromEnd1 = PtrOffsetWordsFromEnd0 + DataLen1 + PtrLen1 + ExtraLen1,
	%
	%     {DataLen2, PtrLen2, ExtraLen2, Data2, Extra2} = encode_<Type2>(<InputData2>, 0),
	%     PtrOffset2 = PtrOffsetWordsFromEnd1 + (PtrLen0 - 2),
	%     Ptr2 = struct_ptr(PtrOffset2, DataLen2, PtrLen2),
	%     PtrOffsetWordsFromEnd2 = PtrOffsetWordsFromEnd1 + DataLen2 + PtrLen2 + ExtraLen2,
	%
	%     MyPtrs = << X:64/unsigned-little-integer || X <- [Ptr1, Ptr2, ...] >>,
	%     {DataLen, PtrLen, PtrOffsetWordsFromEndN - PtrOffsetWordsFromEnd0, [MyData, MyPtrs], [Data1, Extra1, Data2, Extra2, ...]}.
	%
	EncodeBody = encode_function_body(Line, TypeId, Groups, SortedDataFields, SortedPtrFields, Schema),

	{function, Line, encoder_name(TypeId, Schema), 2,
		[{clause, Line,
				[
					{record, Line, record_name(TypeId, Schema),
						[{record_field, Line, {atom, Line, list_to_atom(binary_to_list(FieldName))}, var_p(Line, "Var", FieldName)} || #field_info{name=FieldName} <- SortedDataFields ++ SortedPtrFields ++ Groups ]
					},
					{var, Line, 'PtrOffsetWordsFromEnd0'}
				],
				[],
				EncodeBody
			}]}.

generate_text() ->
	Line = 0,
	ListVar = {var, Line, 'List'},
	OffsetVar = {var, Line, 'Offset'},
	EncodeBody = ast_encode_text_only_({in, [ListVar, OffsetVar]}, {out, []}, {temp_suffix, ""}),

	Func = {function, Line, 'encode_text', 2,
		[{clause, Line,
				[ ListVar, OffsetVar ],
				[],
				EncodeBody
			}]},
	{[], [Func], []}.

-ast_fragment2([]).
ast_encode_text_only_(
		{in, [List, Offset]},
		{out, []},
		{temp, []}
		) ->
	DataLen = iolist_size(List) + 1,
	Data = [List, <<0:8, 0:(-DataLen band 7 * 8)/unsigned-little-integer>>],
	Ptr = 1 bor (Offset bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
	{DataLen + 7 bsr 3, <<Ptr:64/unsigned-little-integer>>, Data}.

encode_function_body(Line, TypeId, Groups, SortedDataFields, SortedPtrFields, Schema) ->
	{NoGroupEncodeBody, NoGroupExtraLen, NoGroupBodyData, NoGroupExtraData} = encode_function_body_inline(Line, TypeId, SortedDataFields, SortedPtrFields, Schema),
	if
		Groups =:= [] ->
			% Easy case.
			NoGroupEncodeBody ++ [ {tuple, Line, [NoGroupExtraLen, NoGroupBodyData, NoGroupExtraData]} ];
		true ->
			NoGroupBodyDataInt = {var, Line, 'NoGroupBodyDataAsInt'},
			#'capnp::namespace::Node'{
				''={{1, struct},
					#'capnp::namespace::Node::::struct'{
						dataWordCount=DWords,
						pointerCount=PWords
					}
				}
			} = dict:fetch(TypeId, Schema#capnp_context.by_id),
			BodyLengthInt = {integer, Line, (PWords+DWords)*64},
			ToIntBody = [{match, Line, {bin, Line, [{bin_element, Line, NoGroupBodyDataInt, BodyLengthInt, [integer]}]}, NoGroupBodyData}],
			{FullEncodeBody, ExtraLen, SumBodyData, ExtraData} = group_encoder(NoGroupEncodeBody ++ ToIntBody, NoGroupExtraLen, NoGroupBodyDataInt, NoGroupExtraData, Groups, Line, BodyLengthInt, Schema),
			BodyData = {bin, Line, [{bin_element, Line, SumBodyData, BodyLengthInt, [integer]}]},
			FullEncodeBody ++ [ {tuple, Line, [ExtraLen, BodyData, ExtraData]} ]
	end.

generate_union_encode_fun(Line, TypeId, UnionFields, Schema) ->
	EncodeBody = union_encode_function_body(Line, TypeId, UnionFields, Schema),

	{function, Line, inner_encoder_name(TypeId, Schema), 2,
		[{clause, Line,
				[
					{tuple, Line, [
						{var, Line, 'VarDiscriminant'},
						{var, Line, 'Var'}
					]},
					{var, Line, 'PtrOffsetWordsFromEnd0'}
				],
				[],
				EncodeBody
			}]}.

union_encode_function_body(Line, TypeId, UnionFields, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				discriminantOffset=DiscriminantOffset
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	DiscriminantField = #field_info{name= <<"Discriminant">>, offset=DiscriminantOffset*16, type=builtin_info(uint16)},
	Sorted = lists:sort(fun (#field_info{discriminant=X}, #field_info{discriminant=Y}) -> X < Y end, UnionFields),
	Expected = lists:seq(0, length(Sorted)-1),
	Expected = [ X || #field_info{discriminant=X} <- Sorted ],
	EncoderClauses = [ {clause, Line, [{integer, Line, X}], [], generate_union_encoder(Line, DiscriminantField, Field, TypeId, Schema)} || Field=#field_info{discriminant=X} <- Sorted ],
	%{'case',8, {var,8,'A'}, [{clause,9,[{integer,9,1}],[],[{atom,9,foo}]}, {clause,10, [{integer,10,2}], [[{atom,10,true}]], [{atom,10,bar}]}]}
	[{'case', Line, {var, Line, 'VarDiscriminant'}, EncoderClauses}].

generate_union_encoder(Line, DiscriminantField, Field=#field_info{type=#native_type{}}, TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	[{tuple, Line, [{integer, Line, 0}, {bin, Line, generate_data_binary(0, lists:sort([DiscriminantField, Field#field_info{name={override, 'Var'}}]), encode, DWords) ++ generate_ptr_binary(0, [], encode, PWords)}, {nil, Line}]}];
generate_union_encoder(Line, DiscriminantField, Field=#field_info{type=Type=#ptr_type{}}, TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	ast_encode_ptr(1, 1, Type, <<>>, Line) ++
	[{tuple, Line, [
				{op, Line, '-', {var, Line, 'PtrOffsetWordsFromEnd1'}, {var, Line, 'PtrOffsetWordsFromEnd0'}},
				{bin, Line, generate_data_binary(0, [DiscriminantField], encode, DWords) ++ generate_ptr_binary(0, [Field#field_info{name= <<>>}], encode, PWords)},
				to_list(Line, [{var, Line, 'Data1'}, {var, Line, 'Extra1'}])
	]}];
generate_union_encoder(Line, #field_info{offset=Offset}, #field_info{type=#group_type{type_id=GroupTypeId}}, TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	ast_group_in_union_(
		{in, [{atom, Line, inner_encoder_name(GroupTypeId, Schema)}, {integer, Line, Offset}, {integer, Line, (DWords+PWords)*64}]},
		{out, []},
		{temp_suffix, ""}
	).

% We're not too careful about variable clashes here, since each call to this is made in an independent block.
-ast_fragment2([]).
ast_group_in_union_(
		{in, [EncodeFun, DiscriminantOffset, BitLength]},
		{out, []},
		{temp, []}
		) ->
	{ExtraLen, <<DataInt:BitLength/little-unsigned-integer>>, ExtraData} = EncodeFun(Var, PtrOffsetWordsFromEnd0),
	{ExtraLen, <<(DataInt bor (VarDiscriminant bsl DiscriminantOffset)):BitLength/little-unsigned-integer>>, ExtraData}.


% Combining directly to a binary as we do here is faster than using 'bsl' on the values as integers, by about a factor of 3 on a 6 element list (0.13 micros vs 0.38 micros).
% The difference becomes larger with more elements.
encode_function_body_inline(Line, TypeId, SortedDataFields, SortedPtrFields, Schema) ->
	{_, DWords, PWords} = node_name(TypeId, Schema),
	DataMaker = generate_data_binary(0, SortedDataFields, encode, DWords),
	PtrMaker = generate_ptr_binary(0, SortedPtrFields, encode, PWords),
	EncodePointers = lists:append([ ast_encode_ptr(N, length(SortedPtrFields), Type, FieldName, Line) || {N, #field_info{name=FieldName, type=Type=#ptr_type{}}} <- lists:zip(lists:seq(1, length(SortedPtrFields)), SortedPtrFields) ]),
	{
		EncodePointers,
		{op, Line, '-', var_p(Line, "PtrOffsetWordsFromEnd", length(SortedPtrFields)), {var, Line, 'PtrOffsetWordsFromEnd0'}}, % Extra len that we added
		{bin, Line, DataMaker ++ PtrMaker},
		to_list(Line, lists:append([ [ var_p(Line, "Data", N), var_p(Line, "Extra", N) ] || N <- lists:seq(1, length(SortedPtrFields)) ])) % Extra data that we added
	}.


% This is the part of the encoder body that's inserted after we've encoded the pointers and such.
% It boils down to "call the encoders for each group, then convert everything to big integers and add them together".
% This is somewhat faster than generating the individual binary parts and flattening them,
% though slower than simply spitting out the binary parts in an iolist.
% The time to bor two binaries of length 256 bits is about 0.3 micros for my test script. 1024 bits takes about a micro per bor.
% For comparison, the loop framework was consuming about 0.01 micros per test.
%
% 'bor' has a slight (probably not even statistically significant) speed advantage over '+', and is equivalent since the parts are supposed to be independent.
%
% The iolist solution, while faster as we're ultimately going to return an io_list anyway, is complicated by bit patterns, since:
% - Erlang shuffles the bits relative to our ideal little-endian order (we can fix this just by mangling the offsets), and
% - bitstrings aren't iolists, so we'd still need to add code to combine the bitstrings into binaries before we return.
group_encoder(InitialBody, InitialExtraLen, InitialBodyData, InitialExtraData, [], _Line, _BodyLengthInt, _Schema) ->
	{InitialBody, InitialExtraLen, InitialBodyData, InitialExtraData};
group_encoder(InitialBody, InitialExtraLen, InitialBodyDataInt, InitialExtraData, [#field_info{name=FieldName, type=#group_type{type_id=TypeId}}|T], Line, BodyLengthInt, Schema) ->
	MatchVar = var_p(Line, "Var", FieldName),
	NewExtraLen = var_p(Line, "ExtraDataLen", FieldName),
	NewBodyData = var_p(Line, "BodyData", FieldName),
	NewExtraData = var_p(Line, "ExtraData", FieldName),
	GroupEncodeFun = {atom, Line, inner_encoder_name(TypeId, Schema)},
	ExtraBody = ast_encode_group_(
		{in, [MatchVar, GroupEncodeFun, InitialExtraLen]},
		{out, [NewExtraLen, NewBodyData, NewExtraData]},
		{temp_suffix, binary_to_list(FieldName)}
	),
	NewBodyDataInt = var_p(Line, "BodyDataAsIntFrom", FieldName),
	MeldBody = [{match, Line, {bin, Line, [{bin_element, Line, NewBodyDataInt, BodyLengthInt, [integer]}]}, NewBodyData}],
	group_encoder(InitialBody ++ ExtraBody ++ MeldBody, {op, Line, '+', InitialExtraLen, NewExtraLen}, {op, Line, 'bor', InitialBodyDataInt, NewBodyDataInt}, {cons, Line, InitialExtraData, NewExtraData}, T, Line, BodyLengthInt, Schema).

-ast_fragment2([]).
ast_encode_group_(
		{in, [MatchVar, GroupEncodeFun, InitialExtraLen]},
		{out, [NewExtraLen, NewBodyData, NewExtraData]},
		{temp, []}
	) ->
	{NewExtraLen, NewBodyData, NewExtraData} = GroupEncodeFun(MatchVar, InitialExtraLen).

to_list(Line, List) ->
	lists:foldr(fun (Item, SoFar) -> {cons, Line, Item, SoFar} end, {nil, Line}, List).

encoder_name(TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				isGroup=IsGroup
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	{TypeName, _, _} = node_name(TypeId, Schema),
	GroupBit = case IsGroup of 1 -> "group_"; 0 -> "" end,
	list_to_atom("encode_" ++ GroupBit ++ binary_to_list(TypeName)).

inner_encoder_name(TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	{TypeName, _, _} = node_name(TypeId, Schema),
	list_to_atom("encode_group_" ++ binary_to_list(TypeName)).

decoder_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	list_to_atom("decode_" ++ binary_to_list(TypeName)).

record_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	list_to_atom(binary_to_list(TypeName)).

envelope_fun_name(TypeId, Schema) ->
	{TypeName, _, _} = node_name(TypeId, Schema),
	list_to_atom("envelope_" ++ binary_to_list(TypeName)).

field_name(#field_info{name=FieldName}) ->
	list_to_atom(binary_to_list(FieldName)).


generate_envelope_fun(Line, TypeId, Schema) ->
	{function, Line, envelope_fun_name(TypeId, Schema), 1,
		[{clause, Line,
				[
					{var, Line, 'Input'}
				],
				[],
				generate_envelope_body(Line, TypeId, Schema)
			}]}.

generate_envelope_body(Line, TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	EncodeFun = {atom, Line, encoder_name(TypeId, Schema)},
	ast_envelope_({integer, Line, DWords}, {integer, Line, PWords}, {integer, Line, 1+DWords+PWords}, {var, Line, 'Input'}, EncodeFun).

-ast_fragment([]).
ast_envelope_(DWordsInt, PWordsInt, CommonLenInt, InputVar, EncodeFun) ->
	{ExtraDataLen, MainData, ExtraData} = EncodeFun(InputVar, 0),
	list_to_binary([
			<<
				% Segment envelope
				0:32/unsigned-little-integer, % Segcount - 1. Always 0 for us.
				(CommonLenInt+ExtraDataLen):32/unsigned-little-integer, % Seglen = 1 + DWords + PWords + ExtraLen

				% Pointer to first struct
				0:32/unsigned-little-integer, % Offset of data, starting from end of this pointer, after the struct-type tag. Always 0 for us.
				DWordsInt:16/unsigned-little-integer, % Number of data words in the struct.
				PWordsInt:16/unsigned-little-integer % Number of pointer words in the struct.
			>>,

			% Now the actual data. This may be an io_list, so we can't just /binary it in.
			MainData,
			ExtraData
	]).

ast_encode_ptr_common(N, PtrLen0, AstFun, ExtraInputParams, VarName, Line) ->
	[OldPtrOffsetWordsFromEndVar, NewPtrOffsetWordsFromEndVar] = [ var_p(Line, "PtrOffsetWordsFromEnd", OldOrNew) || OldOrNew <- [N-1, N] ],
	OffsetToEndInt = {integer, Line, PtrLen0 - N},
	MatchVar = var_p(Line, "Var", VarName),
	CommonIn = [OldPtrOffsetWordsFromEndVar, OffsetToEndInt, MatchVar],

	PtrVar = var_p(Line, "Ptr", VarName),
	[DataVar, ExtraVar] = [ var_p(Line, VN, N) || VN <- ["Data", "Extra"] ],
	CommonOut = [NewPtrOffsetWordsFromEndVar, PtrVar, DataVar, ExtraVar],

	AstFun(
		{in, CommonIn ++ ExtraInputParams},
		{out, CommonOut},
		{temp_suffix, temp_suffix(VarName)}
	).

temp_suffix({override, _Name}) ->
	"Temp";
temp_suffix(Name) ->
	binary_to_list(Name).

% This is just a variable aligning function which passes through to ast_encode_ptr_
ast_encode_ptr(N, PtrLen0, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}, VarName, Line) ->
	EncodeFun = {atom, Line, list_to_atom("encode_" ++ binary_to_list(TypeName))},
	StructHeaderNumbers = {integer, Line, (DataLen bsl 32) + (PtrLen bsl 48)},
	StructLen = {integer, Line, DataLen + PtrLen},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_struct_/3, [EncodeFun, StructHeaderNumbers, StructLen], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=text_or_data, extra=text}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_text_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=text_or_data, extra=data}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_data_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={primitive, bool}}, VarName, Line) ->
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_bool_list_/3, [], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={primitive, Type}}, VarName, Line) ->
	#native_type{list_tag=WidthType, width=Width, binary_options=BinType} = Type,
	WidthTypeInt = {integer, Line, WidthType},
	WidthInt = {integer, Line, Width},
	% I can't << X || ... >> is invalid syntax, and << <<X>> || ... >> doesn't let me specify encoding types, so I
	% have to write the whole comprehension by hand! Woe!
	MatchVar = var_p(Line, "Var", VarName),
	EncodedX = {bc, Line, {bin, Line, [{bin_element, Line, encoder(Type, {var, Line, 'X'}, Line), WidthInt, BinType}]}, [{generate,199,{var,Line,'X'}, MatchVar}]},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_primitive_list_/3, [WidthInt, WidthTypeInt, EncodedX], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={struct, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}}}, VarName, Line) ->
	EncodeFun = {atom, Line, list_to_atom("encode_" ++ binary_to_list(TypeName))},
	StructSizePreformatted = {integer, Line, (DataLen bsl 32) + (PtrLen bsl 48)},
	StructLen = {integer, Line, DataLen + PtrLen},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_struct_list_/3, [EncodeFun, StructSizePreformatted, StructLen], VarName, Line);
ast_encode_ptr(N, PtrLen0, #ptr_type{type=list, extra={text, TextType}}, VarName, Line) ->
	EncodeFun = {atom, Line, encode_text},
	StructSizePreformatted = {integer, Line, 1 bsl 48},
	StructLen = {integer, Line, 1},
	ast_encode_ptr_common(N, PtrLen0, fun ast_encode_struct_list_/3, [EncodeFun, StructSizePreformatted, StructLen], VarName, Line).

% This are the fragment that are inserted for each pointer.
% All of them should have the same 'out' params:
% They should assign PtrOffsetWordsFromEnd{N} to include the length of all of the stuff they're encoding, plus PtrOffsetWordsFromEnd{N-1}.
% They should assign Ptr{VarName} to a valid 64-bit integer to encode into the pointer.
% They should assign Data{N} to their personal data.
% They should assign Extra{N} to any extra data they are adding.
% The distinction between Data{N} and Extra{N} is important for list contents; Data should contain /just/ the thing that would be put into the list; Extra /just/ the thing that isn't.
-ast_fragment2([]).
ast_encode_struct_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, EncodeFun, StructSizePreformatted, StructLen]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [ExtraLen]}
	) ->
	{ExtraLen, MainData, ExtraData} = EncodeFun(ValueToEncode, 0),
	PointerAsInt = ((OldOffsetFromEnd + OffsetToEnd) bsl 2) + StructSizePreformatted,
	NewOffsetFromEnd = OldOffsetFromEnd + ExtraLen + StructLen.

-ast_fragment2([]).
ast_encode_text_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	ExtraData = <<>>,
	DataLen = iolist_size(ValueToEncode)+1,
	% We need to add a null terminator.
	% Then we need to add (-L band 7) bytes of padding for alignment.
	MainData = [ValueToEncode, <<0:8, 0:((-DataLen band 7)*8)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 7) bsr 3).

-ast_fragment2([]).
ast_encode_data_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	ExtraData = <<>>,
	DataLen = iolist_size(ValueToEncode),
	% Then we need to add (-L band 7) bytes of padding for alignment.
	MainData = [ValueToEncode, <<0:((-DataLen band 7)*8)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (2 bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 7) bsr 3).

-ast_fragment2([]).
ast_encode_primitive_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, Width, WidthType, EncodedX]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen]}
	) ->
	ExtraData = <<>>,
	DataLen = length(ValueToEncode),
	% We need to add (-L*W band 63) bytes of padding for alignment.
	MainData = [EncodedX, <<0:(-DataLen*Width band 63)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (WidthType bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen * Width + 63) bsr 6).

-ast_fragment2([]).
ast_encode_bool_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen, DataFixed]}
	) ->
	ExtraData = <<>>,
	DataLen = length(ValueToEncode),
	% Because Erlang will reverse blocks of 8 bools, we pre-reverse them here.
	DataFixed = massage_bool_list([ if Y =:= true -> 1; true -> 0 end || Y <- ValueToEncode ]),
	% We need to add (-L band 63) bytes of padding for alignment.
	MainData = [ << <<X:1>> || X <- DataFixed >>, <<0:(-length(DataFixed) band 63)/unsigned-little-integer>>],
	PointerAsInt = 1 bor ((OldOffsetFromEnd + OffsetToEnd) bsl 2) bor (1 bsl 32) bor (DataLen bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd + ((DataLen + 63) bsr 6).

-ast_fragment2([]).
ast_encode_struct_list_(
		{in, [OldOffsetFromEnd, OffsetToEnd, ValueToEncode, EncodeFun, StructSizePreformatted, StructLen]},
		{out, [NewOffsetFromEnd, PointerAsInt, MainData, ExtraData]},
		{temp, [DataLen, FinalOffset]}
	) ->
	DataLen = length(ValueToEncode),
	% We need to add (-L band 7) bytes of padding for alignment.
	{FinalOffset, MainData, ExtraData} = lists:foldl(fun
			(Element, {Offset, DataAcc, ExtraAcc}) ->
				% Call encode_Name once for each struct element
				{ExtraLen, ThisData, ThisExtra} = EncodeFun(Element, Offset - StructLen), % The function wants an offset from the /end/ of this struct
				% Must remember to account for the fact that next element starts StructLen further along.
				{ExtraLen + Offset - StructLen, [DataAcc, ThisData], [ExtraAcc | ThisExtra]}
		end, {
			DataLen * StructLen, % This is the offset from the start of the first embedded struct, to the first word that will be after all embedded structs.
			[<< ((DataLen bsl 2) + StructSizePreformatted):64/unsigned-little-integer >>], % Struct lists start with a tag word.
			[] % Extra accumulator starts empty!
		}, ValueToEncode),
	FinalOffset = round(iolist_size(ExtraData) / 8),
	PointerAsInt = 1 bor ((OffsetToEnd + OldOffsetFromEnd) bsl 2) bor (7 bsl 32) bor ((DataLen * StructLen) bsl 35),
	NewOffsetFromEnd = OldOffsetFromEnd
				+ 1 % tag word
				+ DataLen * StructLen % list contents
				+ FinalOffset. % extra data length.

massage_bool_list() ->
	Var = {var, 0, 'List'},
	[{function, 0, massage_bool_list, 1,
		[{clause, 0,
				[
					Var
				],
				[],
				ast_massage_bool_list_(Var)
			}]}].

-ast_fragment([]).
ast_massage_bool_list_(List) ->
	try lists:split(8, List) of
		{First, Last} ->
			lists:reverse(First) ++ massage_bool_list(Last)
	catch
		error:badarg ->
			lists:reverse(List ++ lists:duplicate(-length(List) band 7, 0))
	end.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_binary(A) -> binary_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A);
to_list(A) when is_list(A) -> A.

var_p(Line, _Prepend, {override, Name}) ->
	{var, Line, Name};
var_p(Line, Prepend, Value) ->
	{var, Line, list_to_atom(to_list(Prepend) ++ to_list(Value))}.

% Need to be careful to gather bit-size elements /backwards/ inside each byte.
% Ignore for now.
% Also support only ints for now.
generate_data_binary(DesiredOffset, [#field_info{offset=DesiredOffset, type=Type=#native_type{width=Size, binary_options=BinType}, name=Name}|Rest], Direction, DWords) ->
	% Match an integer.
	Line = 0, % TODO
	RawVar = var_p(Line, "Var", Name),
	Var = case Direction of
		encode ->
			encoder(Type, RawVar, Line);
		decode ->
			RawVar
	end,
	[{bin_element, Line, Var, {integer, Line, Size}, BinType}|generate_data_binary(DesiredOffset+Size, Rest, Direction, DWords)];
generate_data_binary(CurrentOffset, [#field_info{type=#native_type{width=0}}|Rest], Direction, DWords) ->
	% Just skip over void entries. They always have offset 0 and mess everything else up...
	generate_data_binary(CurrentOffset, Rest, Direction, DWords);
generate_data_binary(CurrentOffset, Rest=[#field_info{offset=DesiredOffset}|_], Direction, DWords) ->
	% Generate filler junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, DesiredOffset-CurrentOffset}, [integer]}|generate_data_binary(DesiredOffset, Rest, Direction, DWords)];
generate_data_binary(CurrentOffset, [], _Direction, DWords) when CurrentOffset == DWords * 64 ->
	[];
generate_data_binary(CurrentOffset, [], Direction, DWords) ->
	% Generate terminal junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, 64*DWords-CurrentOffset}, [integer]}].

generate_ptr_binary(DesiredOffset, [#field_info{offset=DesiredOffset, type=#ptr_type{}, name=Name}|Rest], Direction, PWords) ->
	% Match an integer.
	Line = 0, % TODO
	Var = case Direction of
		encode ->
			var_p(Line, "Ptr", Name);
		decode ->
			var_p(Line, "Var", Name)
	end,
	[{bin_element, Line, Var, {integer, Line, 64}, [little,unsigned,integer]}|generate_ptr_binary(DesiredOffset+64, Rest, Direction, PWords)];
generate_ptr_binary(CurrentOffset, Rest=[#field_info{offset=DesiredOffset}|_], Direction, PWords) ->
	% Generate filler junk. We only get this in unions.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, DesiredOffset-CurrentOffset}, [integer]}|generate_ptr_binary(DesiredOffset, Rest, Direction, PWords)];
generate_ptr_binary(CurrentOffset, [], _Direction, PWords) when CurrentOffset == PWords * 64 ->
	[];
generate_ptr_binary(CurrentOffset, [], Direction, PWords) ->
	% Generate terminal junk.
	Line = 0, % TODO
	[{bin_element, Line, junkterm(Line, Direction), {integer, Line, 64*PWords-CurrentOffset}, [integer]}].

junkterm(Line, decode) ->
	{var, Line, '_'};
junkterm(Line, encode) ->
	{integer, Line, 0}.

% The code we generate to construct data to put into a binary.
encoder(#native_type{type=integer}, Var, _Line) ->
	Var;
encoder(#native_type{type=void}, Var, _Line) ->
	Var;
encoder(#native_type{type=float}, Var, _Line) ->
	Var;
encoder(#native_type{type=boolean}, Var, Line) ->
	{'if',Line,[{clause,Line,[],[[Var]],[{integer,Line,1}]},{clause,Line,[],[[{atom,Line,true}]],[{integer,Line,0}]}]};
encoder(#native_type{type=enum, extra=Enumerants}, Var, Line) ->
	{Numbered, _Len} = lists:mapfoldl(fun (Elt, N) -> {{N, Elt}, N+1} end, 0, Enumerants),
	% case Var of V1 -> 0; ... end
	{'case', Line, Var, [{clause, Line, [{atom, Line, Name}], [], [{integer, Line, Number}]} || {Number, Name} <- Numbered ]};
encoder(#ptr_type{}, Var, _Line) ->
	Var.

% The code we generate after matching a binary to put data into a record.
decoder(#native_type{type=integer}, Var, _Line) ->
	Var;
decoder(#native_type{type=float}, Var, _Line) ->
	Var;
decoder(#native_type{type=boolean}, Var, Line) ->
	{'if',Line,[{clause,Line,[],[[{op,Line,'=:=',Var,{integer,Line,1}}]],[{atom,Line,true}]},{clause,Line,[],[[{atom,Line,true}]],[{atom,Line,false}]}]};
decoder(#native_type{type=enum, extra=Enumerants}, Var, Line) ->
	Tuple = {tuple, Line, [{atom, Line, Name} || Name <- Enumerants ]},
	% erlang:element(Var+1, {V1, ...})
	{call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,element}},[{op,Line,'+',Var,{integer,14,1}},Tuple]};
decoder(#ptr_type{}, Var, _Line) ->
	Var.

field_info(#'capnp::namespace::Field'{
		discriminantValue=DiscriminantValue,
		name=Name,
		''={{0,slot},
			#'capnp::namespace::Field::::slot'{
				offset=N,
				defaultValue=#'capnp::namespace::Value'{''={{_,TypeClass},DefaultValue}},
				type=Type=#'capnp::namespace::Type'{''={{_,TypeClass},_TypeDescription}}
			}
		}
	}, Schema) ->
	{Size, Info} = type_info(Type, Schema),
	#field_info{offset=Size*N, type=Info, name=Name, discriminant=if DiscriminantValue =:= 65535 -> undefined; true -> DiscriminantValue end, default=DefaultValue};
field_info(#'capnp::namespace::Field'{
		discriminantValue=DiscriminantValue,
		name=Name,
		''={{1,group},
			#'capnp::namespace::Field::::group'{
				typeId=TypeId
			}
		}
	}, _Schema) ->
	% Groups and unions.
	#field_info{offset=undefined, type=#group_type{type_id=TypeId}, name=Name, discriminant=if DiscriminantValue =:= 65535 -> undefined; true -> DiscriminantValue end}.

type_info(#'capnp::namespace::Type'{''={{_,TypeClass},TypeDescription}}, Schema) ->
	type_info(TypeClass, TypeDescription, Schema).

% Pointer types (composite/list)
type_info(TextType, void, _Schema) when TextType =:= text; TextType =:= data ->
	{64, #ptr_type{type=text_or_data, extra=TextType}};
type_info(anyPointer, void, _Schema) ->
	{64, #ptr_type{type=unknown}}; % Not really possible
type_info(struct, #'capnp::namespace::Type::::struct'{typeId=TypeId}, Schema) when is_integer(TypeId) ->
	{TypeName, DataLen, PtrLen} = node_name(TypeId, Schema),
	{64, #ptr_type{type=struct, extra={TypeName, DataLen, PtrLen}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,enum},#'capnp::namespace::Type::::enum'{typeId=TypeId}}}}, Schema) ->
	% List of enums.
	EnumerantNames = enumerant_names(TypeId, Schema),
	{64, #ptr_type{type=list, extra={primitive, #native_type{type=enum, extra=EnumerantNames, width=16, binary_options=[little,unsigned,integer], list_tag=3}}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,TextType},void}}}, _Schema) when TextType =:= text; TextType =:= data ->
	% List of text types; this is a list-of-lists.
	{64, #ptr_type{type=list, extra={text, TextType}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,bool},void}}}, _Schema) ->
	% List of bools. While this /could/ encode a list of 1-bit ints, erlang makes it hard by reversing our bits.
	% So we need to special case it!
	{64, #ptr_type{type=list, extra={primitive, bool}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,PrimitiveType},void}}}, _Schema) ->
	% List of any normal primitive type.
	{64, #ptr_type{type=list, extra={primitive, builtin_info(PrimitiveType)}}};
type_info(list, #'capnp::namespace::Type::::list'{elementType=#'capnp::namespace::Type'{''={{_,PtrType},_LTypeDescription}}}, _Schema) when PtrType =:= list; PtrType =:= text; PtrType =:= data ->
	% List of list, or list-of-(text or data) -- all three are lists of lists of lists.
	erlang:error({not_implemented, list, list}); % TODO
type_info(list, #'capnp::namespace::Type::::list'{elementType=InnerType=#'capnp::namespace::Type'{''={{_,struct},_}}}, Schema) ->
	% List of structs.
	% These will be encoded in-line.
	{64, TypeInfo} = type_info(InnerType, Schema),
	{64, #ptr_type{type=list, extra={struct, TypeInfo}}};
type_info(list, #'capnp::namespace::Type'{''={{_,anyPointer},void}}, _Schema) ->
	erlang:error({not_implemented, list, anyPointer}); % TODO
type_info(list, #'capnp::namespace::Type'{''={{_,interface},_LTypeId}}, _Schema) ->
	erlang:error({not_implemented, list, interface}); % TODO
% TODO decoders for pointers.
% Data types
type_info(enum, #'capnp::namespace::Type::::enum'{typeId=TypeId}, Schema) when is_integer(TypeId) ->
	EnumerantNames = enumerant_names(TypeId, Schema),
	{16, #native_type{type=enum, extra=EnumerantNames, width=16, binary_options=[little,unsigned,integer], list_tag=3}};
type_info(TypeClass, void, _Schema) ->
	Info1 = #native_type{width=Size1} = builtin_info(TypeClass),
	{Size1, Info1};
% Catchall
type_info(TypeClass, TypeDescription, _Schema) ->
	io:format("Unknown: ~p~n", [{TypeClass, TypeDescription}]),
	{64, #ptr_type{type=unknown}}.

enumerant_names(TypeId, Schema) ->
	#'capnp::namespace::Node'{
		''={{2, enum},
			#'capnp::namespace::Node::::enum'{
				enumerants=Enumerants
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	[ list_to_atom(binary_to_list(EName)) || #'capnp::namespace::Enumerant'{name=EName} <- Enumerants ].

node_name(TypeId, Schema) when is_integer(TypeId) ->
	#'capnp::namespace::Node'{
		displayName=Name,
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	{Name, DWords, PWords}.

% {BroadType, Bits, BinaryType}
builtin_info(int64) -> #native_type{type=integer, width=64, binary_options=[little, signed, integer], list_tag=5};
builtin_info(int32) -> #native_type{type=integer, width=32, binary_options=[little, signed, integer], list_tag=4};
builtin_info(int16) -> #native_type{type=integer, width=16, binary_options=[little, signed, integer], list_tag=3};
builtin_info(int8) -> #native_type{type=integer, width=8, binary_options=[little, signed, integer], list_tag=2};
builtin_info(uint64) -> #native_type{type=integer, width=64, binary_options=[little, unsigned, integer], list_tag=5};
builtin_info(uint32) -> #native_type{type=integer, width=32, binary_options=[little, unsigned, integer], list_tag=4};
builtin_info(uint16) -> #native_type{type=integer, width=16, binary_options=[little, unsigned, integer], list_tag=3};
builtin_info(uint8) -> #native_type{type=integer, width=8, binary_options=[little, unsigned, integer], list_tag=8};
builtin_info(float32) -> #native_type{type=float, width=32, binary_options=[little, float], list_tag=4};
builtin_info(float64) -> #native_type{type=float, width=64, binary_options=[little, float], list_tag=5};
builtin_info(bool) -> #native_type{type=boolean, width=1, binary_options=[integer], list_tag=1};
builtin_info(void) -> #native_type{type=void, width=0, binary_options=[integer], list_tag=0}.
