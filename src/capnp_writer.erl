%% @author bucko
%% @doc @todo Add description to capnp_writer.


-module(capnp_writer).

-include_lib("capnp.hrl").
-include_lib("capnp_raw.hrl").
-include_lib("capnp_bootstrap.hrl").

-export([
		envelope/1,
		to_bytes/2
	]).

envelope(Bytes) ->
	% 32-bit int, num segs - 1
	% segs * 32 bit int, word-size of segs
	% Then the data!
	% TODO assumption: That we have less than 8 * 2^32 bytes of data and only one segment.
	<<0:?UInt32, (round(erlang:byte_size(Bytes)/8)):?UInt32, Bytes/binary>>.

% Convert a record into a segment, starting with the pointer to the binary.
to_bytes(Rec, Schema) ->
	Name = element(1, Rec),
	TypeId = dict:fetch(Name, Schema#capnp_context.name_to_id),
	{DWords, PWords, Raw, _Size} = to_bytes(Schema, TypeId, Rec),
	list_to_binary([<<(struct_pointer(0, DWords, PWords)):?UInt64>>, Raw]).

% OffsetAfterHere is num words /after/ the pointer that the data segment starts. It will therefore often be zero.
struct_pointer(OffsetAfterHere, DWords, PWords) ->
	(OffsetAfterHere bsl 2) bor (DWords bsl 32) bor (PWords bsl 48).

% SizeTag is 0-index into bit sizes: {0, 1, 8, 16, 32, 64, 64(Pointer), 64-ish(Composite)}
% ElementCount is /word/ count in Composite case; we actually just special case this below.
plain_list_pointer(OffsetAfterHere, SizeTag, ElementCount) ->
	1 bor (OffsetAfterHere bsl 2) bor (SizeTag bsl 32) bor (ElementCount bsl 35).

% First elt is a pointer. Second is a list tag which looks a bit like a struct pointer; it should go on the start of the list elements.
composite_list_pointer(OffsetAfterHere, DWords, PWords, ElementCount) ->
	{
		1 bor (OffsetAfterHere bsl 2) bor (7 bsl 32) bor ((ElementCount*(DWords+PWords)) bsl 35),
		struct_pointer(ElementCount, DWords, PWords) % It's a bit ugly to call struct_pointer here, but it /does/ do the right thing.
	}.

% Convert a record into a byte stream. Each sub-structure will be placed immediately after this one in left-to-right order.
% Returns the data/pointer words in our structure, an unflattened io_list of the encoded structure and the total encoded length (in words).
to_bytes(Schema, TypeId, Obj) ->
	#'capnp::namespace::Node'{
		''={{1, struct},
			#'capnp::namespace::Node::::struct'{
				dataWordCount=DWords,
				pointerCount=PWords,
				fields=Fields
			}
		}
	} = dict:fetch(TypeId, Schema#capnp_context.by_id),
	{Acc, AccSize} = encode_parts(Fields, tl(tuple_to_list(Obj)), list_to_tuple(lists:duplicate(DWords, 0)), list_to_tuple(lists:duplicate(PWords, 0)), 0, [], Schema),
	{DWords, PWords, Acc, AccSize}.

encode_parts([
			#'capnp::namespace::Field'{
				''={{0,slot},
					#'capnp::namespace::Field::::slot'{
						offset=N,
						defaultValue=#'capnp::namespace::Value'{''={{_,TypeClass},DefaultValue}},
						type=#'capnp::namespace::Type'{''={{_,TypeClass},TypeDescription}}
					}
				}   
			}
		|RestFields], [Value|RestValues], DataSeg, PointerSeg, ExtraDataLength, ExtraData, Schema) ->
	{NewDataSeg, NewPointerSeg, NewOffset, NewExtraData} = encode_field(TypeClass, TypeDescription, DefaultValue, N, Value, DataSeg, PointerSeg, ExtraDataLength, ExtraData, Schema),
	encode_parts(RestFields, RestValues, NewDataSeg, NewPointerSeg, NewOffset, [ExtraData|NewExtraData], Schema);
encode_parts(_, [], DataSeg, PointerSeg, ExtraDataLength, ExtraData, _Schema) ->
	% Offset is total data length of everything /extra/ we've put in.
	{[flatten_seg(DataSeg), flatten_seg(PointerSeg), ExtraData], ExtraDataLength + tuple_size(DataSeg) + tuple_size(PointerSeg)}.

encode_field(TypeClass, TypeDescription, DefaultValue, N, Value, DataSeg, PointerSeg, ExtraDataLength, ExtraData, Schema) ->
	case {TypeClass, TypeDescription} of
		{anyPointer, void} ->
			erlang:error(not_implemented);
		{text, void} ->
			% TODO encode as simple list
			erlang:error(not_implemented);
		{data, void} ->
			% TODO encode as simple list
			erlang:error(not_implemented);
		{_, void} ->
			{Shifts, Encoded} = encode(TypeClass, Value, DefaultValue, N),
			NewDataSeg = insert((N bsl (Shifts - 6)), DataSeg, Encoded),
			{NewDataSeg, PointerSeg, ExtraDataLength, []};
		{struct, TypeId} when is_integer(TypeId)-> % TODO are these working fine in bootstrap_capnp? They're a :group.
			{DWords, PWords, Data, TotalWords} = to_bytes(Schema, TypeId, Value),
			% We're going to jam the new data on the end of the accumulator, so we must add the length of every structure we've added so far.
			% We also need to include the length of every pointer /after/ this one. Not that the first pointer is N=0.
			Pointer = struct_pointer(ExtraDataLength + (tuple_size(PointerSeg) - (N + 1)), DWords, PWords),
			NewPointerSeg = insert(N, PointerSeg, Pointer),
			{DataSeg, NewPointerSeg, ExtraDataLength + TotalWords, Data};
		{list, #'capnp::namespace::Type'{''={{_,list},_LTypeDescription}}} ->
			% TODO Encode as pointers!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,text},void}}} ->
			% TODO Encode as pointers!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,data},void}}} ->
			% TODO Encode as pointers!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,struct},_LTypeId}}} ->
			% TODO Encode as composite!
			% TODO Can also encode a /small/ struct as a smaller size.
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,anyPointer},void}}} ->
			% TODO Encode as pointers!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,interface},_LTypeId}}} ->
			% TODO Encode as ???!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,enum},_LTypeId}}} ->
			% TODO Encode as ints!
			erlang:error(not_implemented);
		{list, #'capnp::namespace::Type'{''={{_,_IntOrVoidType},void}}} ->
			% TODO Encode as ints!
			erlang:error(not_implemented)
		% TODO nested constructs (ie. groups)
		% TODO unions (discriminantValue/discriminantOffset)
	end.

% S is in "integer generations"; we're going to use it to to work out how much to bsl.
encode(Type, Value, Default, Offset) ->
	{Shifts, ValueToWrite} = encode(Type, Value, Default),
	% Multiply Offset by the value size, and the result with 63.
	% This gives the offset within this word that the value will appear at.
	% Now shift the encoded value by that amount to align it to where we'd
	% hope it gets written.
	{Shifts, ValueToWrite bsl ((Offset bsl Shifts) band 63)}.

% This step is somewhat complicated by erlang's lack of love for little endian data.
% Basically, booleans end up in the wrong part of their byte.
% Still need a lot of experiment to find the fastest way of writing this code. 
encode(void, _, _) ->
	{0, 0};
encode(int8, N, V) ->
	encode_integer(1 bsl 7, 3, N, V);
encode(int16, N, V) ->
	encode_integer(1 bsl 15, 4, N, V);
encode(int32, N, V) ->
	encode_integer(1 bsl 31, 5, N, V);
encode(int64, N, V) ->
	encode_integer(1 bsl 63, 6, N, V);
encode(bool, N, V) ->
	encode_uinteger(1, 0, N, V);
encode(uint8, N, V) ->
	encode_uinteger(1 bsl 8, 3, N, V);
encode(uint16, N, V) ->
	encode_uinteger(1 bsl 16, 4, N, V);
encode(uint32, N, V) ->
	encode_uinteger(1 bsl 32, 5, N, V);
encode(uint64, N, V) ->
	encode_uinteger(1 bsl 64, 6, N, V);
encode(_, _, _) ->
	{0, 0}.

encode_integer(Max, Size, Value, Default) when is_integer(Value), Value < 0, Value >= -Max ->
	{Size, (Value+Max*2) bxor Default};
encode_integer(Max, Size, Value, Default) when is_integer(Value), Value >= 0, Value < Max ->
	{Size, Value bxor Default}.

encode_uinteger(Max, Size, Value, Default) when is_integer(Value), Value >= 0, Value < Max ->
	{Size, Value bxor Default}.

insert(Offset, DataSeg, Value) ->
	% TODO setelement is sad
	setelement(Offset+1, DataSeg, element(Offset+1, DataSeg) bor Value).

flatten_seg(L) ->
	<< <<A:?UInt64>> || A <- tuple_to_list(L) >>.