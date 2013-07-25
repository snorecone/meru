-module(meru_transform).

-export([parse_transform/2]).

-import(erl_syntax, [
    attribute_name/1,
    attribute_arguments/1,
    list_elements/1,
    atom_value/1,
    tuple_elements/1,
    integer_value/1,
    binary_fields/1,
    binary_field_body/1
]).

-record(state, {
    record,
    bucket,
    keyfun,
    mergefun
}).

parse_transform(Forms, Opts) ->
    % dbg:tracer(),
    % dbg:tpl(?MODULE,x),
    % dbg:p(all,[c]),
    Context = parse_trans:initial_context(Forms, Opts),
    State = parse_trans:do_inspect(fun inspect/4, #state{}, Forms, Context),
    Result = parse_trans:revert(export_funs(add_funs(Forms, State, Context), State, Context)),
    parse_trans:optionally_pretty_print(Result, Opts, Context),
    Result.

inspect(attribute, Form, _Context, Acc) ->
    case atom_value(attribute_name(Form)) of
        meru_bucket ->
            undefined = Acc#state.bucket,
            BucketName = parse_trans:revert_form(hd(attribute_arguments(Form))),
            io:format("~p~n", [BucketName]),
            {false, Acc#state{ bucket = BucketName }};
        meru_keyfun ->
            undefined = Acc#state.keyfun,
            KeyFunName = atom_value(hd(attribute_arguments(Form))),
            {false, Acc#state{ keyfun = KeyFunName }};
        meru_mergefun ->
            undefined = Acc#state.mergefun,
            MergeFunName = atom_value(hd(attribute_arguments(Form))),
            {false, Acc#state{ mergefun = MergeFunName }};
        meru_record ->
            undefined = Acc#state.record,
            RecordName = atom_value(hd(attribute_arguments(Form))),
            {false, Acc#state{ record = RecordName }};
        _ ->
            {false, Acc}
    end;
inspect(_, _, _, Acc) ->
    {false, Acc}.

add_funs(Forms, State, Context) ->
    lists:foldl(fun (FunName, Acc) ->
        add_fun(FunName, Acc, State, Context)
    end, Forms, [get]).

add_fun(get, Forms, #state{ record = _RecordName, bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function, 1, get, 1, [{clause,1,
                [{var,1,'Key'}],
                [],
                [{call,1,
                    {remote,1,{atom,1,meru_riak},{atom,1,get}},
                        [BucketName, {call,1,{atom,1,KeyFunName},[{var,1,'Key'}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context).
% add_fun(put, Forms, #state{ record = RecordName, bucket = BucketName, keyfun = KeyFunName, mergefun = MergeFunName }, Context) ->
%     Forms;
% add_fun(put_merge_2, Forms, #state{ record = RecordName, bucket = BucketName, keyfun = KeyFunName, mergefun = MergeFunName }, Context) ->
%     Forms;
% add_fun(put_merge_3, Forms, #state{ record = RecordName, bucket = BucketName, keyfun = KeyFunName, mergefun = MergeFunName }, Context) ->
%     Forms;
% add_fun(delete, Forms, #state{ record = RecordName, bucket = BucketName, keyfun = KeyFunName, mergefun = MergeFunName }, Context) ->
%     Forms.

export_funs(Forms, _State, Context) ->
    % Exports = [{attribute, 1, export, [{get, 1}, {put, 1}, {put_merge, 2}, {put_merge, 3}, {delete, 1}]}],
    Exports = [{attribute, 1, export, [{get, 1}]}],
    parse_trans:do_insert_forms(above, Exports, Forms, Context).

% binary_value(BinaryTree) ->
%     list_to_binary([integer_value(binary_field_body(B)) || B <- binary_fields(BinaryTree)]).

