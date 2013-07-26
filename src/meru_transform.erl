-module(meru_transform).

-export([parse_transform/2]).

-import(erl_syntax, [
    attribute_name/1,
    attribute_arguments/1,
    list_elements/1,
    list/1,
    atom_value/1,
    tuple_elements/1,
    integer_value/1,
    binary_fields/1,
    binary_field_body/1,
    record_field_name/1,
    record_field_value/1
]).

-record(state, {
    record,
    records = orddict:new(),
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
        record ->
            [Name, {_, _, _, Fields0}] = attribute_arguments(Form),
            Fields = parse_trans:revert_form(list([record_field_name(F) || F <- Fields0])),
            Values0 = [begin FV = record_field_value(F), 
                if FV == none -> {atom, 1, undefined}; true -> FV end end || F <- Fields0],
            Values = parse_trans:revert_form(list(Values0)),
            {false, Acc#state{ records = orddict:store(atom_value(Name), {Fields, Values}, Acc#state.records) }};
        _ ->
            {false, Acc}
    end;
inspect(_, _, _, Acc) ->
    {false, Acc}.

add_funs(Forms, State, Context) ->
    lists:foldl(fun (FunName, Acc) ->
        add_fun(FunName, Acc, State, Context)
    end, Forms, [record_to_proplist, proplist_to_record, get, put]).

add_fun(get, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function, 1, get, 1, [{clause,1,
        [{var,1,'Key'}],
        [],
        [{'case',2,
          {call,2,
              {remote,2,{atom,2,meru_riak},{atom,2,get}},
              [BucketName,
               {call,2,{atom,2,KeyFunName},[{var,2,'Key'}]}]},
          [{clause,3,
               [{tuple,3,[{atom,3,error},{var,3,'Error'}]}],
               [],
               [{tuple,3,[{atom,3,error},{var,3,'Error'}]}]},
           {clause,4,
               [{tuple,4,[{atom,4,ok},{var,4,'RObj'}]}],
               [],
               [{tuple,5,
                    [{atom,5,ok},
                     {call,5,
                         {atom,5,proplist_to_record},
                         [{call,5,
                              {atom,5,binary_to_term},
                              [{call,5,
                                   {remote,5,
                                       {atom,5,riakc_obj},
                                       {atom,5,get_value}},
                                   [{var,5,'RObj'}]}]}]}]}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun(put, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function, 1, put, 1, [{clause,1,
         [{var,1,'Record'}],
         [],
         [{match,1,
                 {var,1,'Key'},
                 {call,1,{atom,1,KeyFunName},[{var,1,'Record'}]}},
          {match,1,
                 {atom,1,ok},
                 {call,1,
                       {remote,1,{atom,1,meru_riak},{atom,1,put}},
                       [{call,1,
                              {remote,1,{atom,1,riakc_obj},{atom,1,new}},
                              [BucketName,
                               {var,1,'Key'},
                               {call,1,
                                     {atom,1,record_to_proplist},
                                     [{var,1,'Record'}]}]}]}},
          {tuple,1,[{atom,1,ok},{var,1,'Key'}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
% add_fun(put_merge_2, Forms, #state{ record = RecordName, bucket = BucketName, keyfun = KeyFunName, mergefun = MergeFunName }, Context) ->
%     Forms;
% add_fun(put_merge_3, Forms, #state{ record = RecordName, bucket = BucketName, keyfun = KeyFunName, mergefun = MergeFunName }, Context) ->
%     Forms;
% add_fun(delete, Forms, #state{ record = RecordName, bucket = BucketName, keyfun = KeyFunName, mergefun = MergeFunName }, Context) ->
%     Forms.

add_fun(record_to_proplist, Forms, #state{ record = RecordName, records = Records }, Context) ->
    {RecordFields, _} = orddict:fetch(RecordName, Records),
    Form = [{function, 1, record_to_proplist, 1, [{clause,1,
      [{var, 1, 'Rec'}],
      [],
      [{call, 1,
        {remote, 1, {atom, 1, lists}, {atom, 1, zip}},
        [RecordFields,
         {call, 1,
          {atom, 1, tl},
          [{call, 1, {atom, 1, tuple_to_list}, [{var, 1, 'Rec'}]}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun(proplist_to_record, Forms, #state{ record = RecordName, records = Records }, Context) ->
    {RecordFields, RecordValues} = orddict:fetch(RecordName, Records),
    Form = [{function, 1, proplist_to_record, 1, [{clause,1,
      [{var,1,'Proplist'}],
      [],
      [{match,2,
        {var,2,'Fields'},
        RecordFields},
       {match,3,
        {var,3,'Defaults'},
        RecordValues},
       {match,4,
        {var,4,'FieldsWithDefaults'},
        {call,4,
         {remote,4,{atom,4,lists},{atom,4,zip}},
         [{var,4,'Fields'},{var,4,'Defaults'}]}},
       {match,5,
        {var,5,'Recordlist'},
        {call,5,
         {remote,5,{atom,5,lists},{atom,5,reverse}},
         [{call,5,
           {remote,5,{atom,5,lists},{atom,5,foldl}},
           [{'fun',5,
             {clauses,
              [{clause,5,
                [{tuple,5,[{var,5,'K'},{var,5,'Default'}]},{var,5,'Acc'}],
                [],
                [{cons,6,
                  {call,6,
                   {remote,6,{atom,6,proplists},{atom,6,get_value}},
                   [{var,6,'K'},{var,6,'Proplist'},{var,6,'Default'}]},
                  {var,6,'Acc'}}]}]}},
            {nil,7},
            {var,7,'FieldsWithDefaults'}]}]}},
       {call,8,
        {atom,8,list_to_tuple},
        [{cons,8,{atom,8,RecordName},{var,8,'Recordlist'}}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context).

export_funs(Forms, _State, Context) ->
    % Exports = [{attribute, 1, export, [{get, 1}, {put, 1}, {put_merge, 2}, {put_merge, 3}, {delete, 1}]}],
    Exports = [{attribute, 1, export, [{get, 1}, {put, 1}, {record_to_proplist, 1}, {proplist_to_record, 1}]}],
    parse_trans:do_insert_forms(above, Exports, Forms, Context).

