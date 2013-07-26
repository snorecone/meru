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

-define(RIAK, meru_riak).
-define(FUNS, [{record_to_proplist, 1}, {proplist_to_record, 1}, {new, 0}, {new, 1}, {get, 1}, {put, 1}, {put_merge, 2}, {put_merge, 3}, {delete, 1}]).

parse_transform(Forms, Opts) ->
    % dbg:tracer(),
    % dbg:tpl(?MODULE,x),
    % dbg:p(all,[c]),
    Context = parse_trans:initial_context(Forms, Opts),
    State = parse_trans:do_inspect(fun inspect/4, #state{}, Forms, Context),
    Result = parse_trans:revert(no_auto_import(export_funs(add_funs(Forms, State, Context), State, Context), Context)),
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
    end, Forms, ?FUNS).

add_fun({get, 1}, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function,1,get,1,
      [{clause,32,
        [{var,32,'Key'}],
        [],
        [{'case',33,
          {call,33,
           {remote,33,{atom,33,?RIAK},{atom,33,get}},
           [BucketName,
            {call,33,{atom,33,KeyFunName},[{var,33,'Key'}]}]},
          [{clause,34,
            [{tuple,34,[{atom,34,ok},{var,34,'RObj'}]}],
            [],
            [{tuple,35,
              [{atom,35,ok},
               {call,35,
                {atom,35,proplist_to_record},
                [{call,35,
                  {atom,35,binary_to_term},
                  [{call,35,
                    {remote,35,{atom,35,riakc_obj},{atom,35,get_value}},
                    [{var,35,'RObj'}]}]}]}]}]},
           {clause,36,[{var,36,'Error'}],[],[{var,37,'Error'}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put, 1}, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function,1,put,1,
         [{clause,42,
              [{var,42,'Record'}],
              [],
              [{match,43,
                   {var,43,'Key'},
                   {call,43,{atom,43,KeyFunName},[{var,43,'Record'}]}},
               {'case',44,
                   {call,44,
                       {remote,44,{atom,44,?RIAK},{atom,44,put}},
                       [{call,44,
                            {remote,44,{atom,44,riakc_obj},{atom,44,new}},
                            [BucketName,
                             {var,44,'Key'},
                             {call,45,
                                 {atom,45,term_to_binary},
                                 [{call,45,
                                      {atom,45,record_to_proplist},
                                      [{var,45,'Record'}]}]}]}]},
                   [{clause,46,
                        [{atom,46,ok}],
                        [],
                        [{tuple,46,[{atom,46,ok},{var,46,'Key'}]}]},
                    {clause,47,[{var,47,'Error'}],[],[{var,47,'Error'}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put_merge, 2}, Forms, #state{ mergefun = MergeFunName }, Context) ->
    Form = [{function,1,put_merge,2,
         [{clause,52,
              [{var,52,'Record'},{var,52,'Options'}],
              [],
              [{'case',53,
                   {call,53,{atom,53,get},[{var,53,'Record'}]},
                   [{clause,54,
                        [{tuple,54,[{atom,54,ok},{var,54,'OldRecord'}]}],
                        [],
                        [{call,54,
                             {atom,54,put},
                             [{call,54,
                                  {atom,54,MergeFunName},
                                  [{var,54,'OldRecord'},
                                   {var,54,'Record'},
                                   {var,54,'Options'}]}]}]},
                    {clause,55,
                        [{var,55,'_Error'}],
                        [],
                        [{call,55,
                             {atom,55,put},
                             [{call,55,
                                  {atom,55,MergeFunName},
                                  [{atom,55,notfound},
                                   {var,55,'Record'},
                                   {var,55,'Options'}]}]}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put_merge, 3}, Forms, #state{ mergefun = MergeFunName }, Context) ->
    Form = [{function,1,put_merge,3,
         [{clause,60,
              [{var,60,'Key'},{var,60,'Record'},{var,60,'Options'}],
              [],
              [{'case',61,
                   {call,61,{atom,61,get},[{var,61,'Key'}]},
                   [{clause,62,
                        [{tuple,62,[{atom,62,ok},{var,62,'OldRecord'}]}],
                        [],
                        [{call,62,
                             {atom,62,put},
                             [{call,62,
                                  {atom,62,MergeFunName},
                                  [{var,62,'OldRecord'},
                                   {var,62,'Record'},
                                   {var,62,'Options'}]}]}]},
                    {clause,63,
                        [{var,63,'_Error'}],
                        [],
                        [{call,63,
                             {atom,63,put},
                             [{call,63,
                                  {atom,63,MergeFunName},
                                  [{atom,63,notfound},
                                   {var,63,'Record'},
                                   {var,63,'Options'}]}]}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({delete, 1}, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function,1,delete,1,
         [{clause,68,
              [{var,68,'Record'}],
              [],
              [{match,69,
                   {var,69,'Key'},
                   {call,69,{atom,69,KeyFunName},[{var,69,'Record'}]}},
               {'case',70,
                   {call,70,
                       {remote,70,{atom,70,?RIAK},{atom,70,delete}},
                       [BucketName,{var,70,'Key'}]},
                   [{clause,71,
                        [{atom,71,ok}],
                        [],
                        [{tuple,71,[{atom,71,ok},{var,71,'Key'}]}]},
                    {clause,72,[{var,72,'Error'}],[],[{var,72,'Error'}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({record_to_proplist, 1}, Forms, #state{ record = RecordName, records = Records }, Context) ->
    {RecordFields, _} = orddict:fetch(RecordName, Records),
    Form = [{function,1,record_to_proplist,1,
         [{clause,17,
              [{var,17,'Record'}],
              [],
              [{call,18,
                   {remote,18,{atom,18,lists},{atom,18,zip}},
                   [RecordFields,
                    {call,18,
                        {atom,18,tl},
                        [{call,18,
                             {atom,18,tuple_to_list},
                             [{var,18,'Record'}]}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({proplist_to_record, 1}, Forms, #state{ record = RecordName, records = Records }, Context) ->
    {RecordFields, RecordValues} = orddict:fetch(RecordName, Records),
    Form = [{function,1,proplist_to_record,1,
      [{clause,7,
        [{var,7,'Proplist'}],
        [],
        [{call,8,
          {atom,8,list_to_tuple},
          [{cons,9,
            {atom,9,RecordName},
            {call,10,
             {remote,10,{atom,10,lists},{atom,10,reverse}},
             [{call,11,
               {remote,11,{atom,11,lists},{atom,11,foldl}},
               [{'fun',11,
                 {clauses,
                  [{clause,11,
                    [{tuple,11,[{var,11,'K'},{var,11,'Default'}]},{var,11,'Acc'}],
                    [],
                    [{cons,12,
                      {call,12,
                       {remote,12,{atom,12,proplists},{atom,12,get_value}},
                       [{var,12,'K'},{var,12,'Proplist'},{var,12,'Default'}]},
                      {var,12,'Acc'}}]}]}},
                {nil,13},
                {call,13,
                 {remote,13,{atom,13,lists},{atom,13,zip}},
                 [RecordFields,
                  RecordValues]}]}]}}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({new, 0}, Forms, _State, Context) ->
    Form = [{function,1,new,0,[{clause,22,[],[],[{call,23,{atom,23,new},[{nil,23}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({new, 1}, Forms, _State, Context) ->
    Form = [{function,1,new,1,
           [{clause,27,
                    [{var,27,'Proplist'}],
                    [],
                    [{call,28,
                           {atom,28,proplist_to_record},
                           [{var,28,'Proplist'}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context).

export_funs(Forms, _State, Context) ->
    Exports = [{attribute, 1, export, ?FUNS}],
    parse_trans:do_insert_forms(above, Exports, Forms, Context).

no_auto_import(Forms, Context) ->
    NoAuto = [{attribute, 1, compile, {no_auto_import, [{get, 1}]}}],
    parse_trans:do_insert_forms(above, NoAuto, Forms, Context).
