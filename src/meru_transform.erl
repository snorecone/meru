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
-define(FUNS, [{record_to_proplist, 1}, {proplist_to_record, 1}, {new, 0}, {new, 1}, {get, 1}, {get, 2}, {put, 1}, {put, 2}, {put_merge, 2}, {put_merge, 3}, {put_merge, 4}, {delete, 1}, {delete, 2}]).

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

add_fun({get, 1}, Forms, _State, Context) ->
    Form = [{function,1,get,1,
     [{clause,32,
          [{var,32,'Key'}],
          [],
          [{call,33,
               {remote,33,{atom,33,?RIAK},{atom,33,transaction}},
               [{'fun',33,
                    {clauses,
                        [{clause,33,
                             [{var,33,'Pid'}],
                             [],
                             [{call,34,
                                  {atom,34,get},
                                  [{var,34,'Pid'},{var,34,'Key'}]}]}]}}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({get, 2}, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function,1,get,2,
  [{clause,39,
    [{var,39,'Pid'},{var,39,'Key'}],
    [],
    [{'case',40,
      {call,40,
       {remote,40,{atom,40,?RIAK},{atom,40,get}},
       [{var,40,'Pid'},
        BucketName,
        {call,40,{atom,40,KeyFunName},[{var,40,'Key'}]}]},
      [{clause,41,
        [{tuple,41,[{atom,41,ok},{var,41,'RObj'}]}],
        [],
        [{tuple,42,
          [{atom,42,ok},
           {call,42,
            {atom,42,proplist_to_record},
            [{call,42,
              {atom,42,binary_to_term},
              [{call,42,
                {remote,42,{atom,42,riakc_obj},{atom,42,get_value}},
                [{var,42,'RObj'}]}]}]}]}]},
       {clause,43,[{var,43,'Error'}],[],[{var,44,'Error'}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put, 1}, Forms, _State, Context) ->
    Form = [{function,1,put,1,
     [{clause,49,
          [{var,49,'Record'}],
          [],
          [{call,50,
               {remote,50,{atom,50,?RIAK},{atom,50,transaction}},
               [{'fun',50,
                    {clauses,
                        [{clause,50,
                             [{var,50,'Pid'}],
                             [],
                             [{call,51,
                                  {atom,51,put},
                                  [{var,51,'Pid'},
                                   {var,51,'Record'}]}]}]}}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put, 2}, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function,1,put,2,
     [{clause,56,
          [{var,56,'Pid'},{var,56,'Record'}],
          [],
          [{match,57,
               {var,57,'Key'},
               {call,57,{atom,57,KeyFunName},[{var,57,'Record'}]}},
           {'case',58,
               {call,58,
                   {remote,58,{atom,58,?RIAK},{atom,58,put}},
                   [{var,58,'Pid'},
                    {call,58,
                        {remote,58,{atom,58,riakc_obj},{atom,58,new}},
                        [BucketName,
                         {var,58,'Key'},
                         {call,59,
                             {atom,59,term_to_binary},
                             [{call,59,
                                  {atom,59,record_to_proplist},
                                  [{var,59,'Record'}]}]}]}]},
               [{clause,60,
                    [{atom,60,ok}],
                    [],
                    [{tuple,60,
                         [{atom,60,ok},{var,60,'Key'},{var,60,'Record'}]}]},
                {clause,61,[{var,61,'Error'}],[],[{var,61,'Error'}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put_merge, 2}, Forms, _State, Context) ->
    Form = [{function,1,put_merge,2,
     [{clause,66,
          [{var,66,'Record'},{var,66,'Options'}],
          [],
          [{call,67,
               {remote,67,{atom,67,?RIAK},{atom,67,transaction}},
               [{'fun',67,
                    {clauses,
                        [{clause,67,
                             [{var,67,'Pid'}],
                             [],
                             [{call,68,
                                  {atom,68,put_merge},
                                  [{var,68,'Pid'},
                                   {var,68,'Record'},
                                   {var,68,'Options'}]}]}]}}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put_merge, 3}, Forms, #state{ mergefun = MergeFunName }, Context) ->
    Form = [{function,1,put_merge,3,
     [{clause,73,
          [{var,73,'Pid'},{var,73,'Record'},{var,73,'Options'}],
          [[{call,73,{atom,73,is_pid},[{var,73,'Pid'}]}]],
          [{'case',74,
               {call,74,{atom,74,get},[{var,74,'Pid'},{var,74,'Record'}]},
               [{clause,75,
                    [{tuple,75,[{atom,75,ok},{var,75,'OldRecord'}]}],
                    [],
                    [{call,75,
                         {atom,75,put},
                         [{var,75,'Pid'},
                          {call,75,
                              {atom,75,MergeFunName},
                              [{var,75,'OldRecord'},
                               {var,75,'Record'},
                               {var,75,'Options'}]}]}]},
                {clause,76,
                    [{var,76,'_Error'}],
                    [],
                    [{call,76,
                         {atom,76,put},
                         [{var,76,'Pid'},
                          {call,76,
                              {atom,76,MergeFunName},
                              [{atom,76,notfound},
                               {var,76,'Record'},
                               {var,76,'Options'}]}]}]}]}]},
      {clause,78,
          [{var,78,'Key'},{var,78,'Record'},{var,78,'Options'}],
          [],
          [{call,79,
               {remote,79,{atom,79,?RIAK},{atom,79,transaction}},
               [{'fun',79,
                    {clauses,
                        [{clause,79,
                             [{var,79,'Pid'}],
                             [],
                             [{call,80,
                                  {atom,80,put_merge},
                                  [{var,80,'Pid'},
                                   {var,80,'Key'},
                                   {var,80,'Record'},
                                   {var,80,'Options'}]}]}]}}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({put_merge, 4}, Forms, #state{ mergefun = MergeFunName }, Context) ->
    Form = [{function,1,put_merge,4,
     [{clause,85,
          [{var,85,'Pid'},{var,85,'Key'},{var,85,'Record'},{var,85,'Options'}],
          [],
          [{'case',86,
               {call,86,{atom,86,get},[{var,86,'Pid'},{var,86,'Key'}]},
               [{clause,87,
                    [{tuple,87,[{atom,87,ok},{var,87,'OldRecord'}]}],
                    [],
                    [{call,87,
                         {atom,87,put},
                         [{var,87,'Pid'},
                          {call,87,
                              {atom,87,MergeFunName},
                              [{var,87,'OldRecord'},
                               {var,87,'Record'},
                               {var,87,'Options'}]}]}]},
                {clause,88,
                    [{var,88,'_Error'}],
                    [],
                    [{call,88,
                         {atom,88,put},
                         [{var,88,'Pid'},
                          {call,88,
                              {atom,88,MergeFunName},
                              [{atom,88,notfound},
                               {var,88,'Record'},
                               {var,88,'Options'}]}]}]}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({delete, 1}, Forms, _State, Context) ->
    Form = [{function,1,delete,1,
     [{clause,93,
          [{var,93,'Record'}],
          [],
          [{call,94,
               {remote,94,{atom,94,?RIAK},{atom,94,transaction}},
               [{'fun',94,
                    {clauses,
                        [{clause,94,
                             [{var,94,'Pid'}],
                             [],
                             [{call,95,
                                  {atom,95,delete},
                                  [{var,95,'Pid'},
                                   {var,95,'Record'}]}]}]}}]}]}]}],
    parse_trans:do_insert_forms(above, Form, Forms, Context);
add_fun({delete, 2}, Forms, #state{ bucket = BucketName, keyfun = KeyFunName }, Context) ->
    Form = [{function,1,delete,2,
     [{clause,100,
          [{var,100,'Pid'},{var,100,'Record'}],
          [],
          [{match,101,
               {var,101,'Key'},
               {call,101,{atom,101,KeyFunName},[{var,101,'Record'}]}},
           {'case',102,
               {call,102,
                   {remote,102,{atom,102,?RIAK},{atom,102,delete}},
                   [{var,102,'Pid'},
                    BucketName,
                    {var,102,'Key'}]},
               [{clause,103,
                    [{atom,103,ok}],
                    [],
                    [{tuple,103,[{atom,103,ok},{var,103,'Key'}]}]},
                {clause,104,[{var,104,'Error'}],[],[{var,104,'Error'}]}]}]}]}],
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
    NoAuto = [{attribute, 1, compile, {no_auto_import, [{get, 1}, {put, 2}]}}],
    parse_trans:do_insert_forms(above, NoAuto, Forms, Context).
