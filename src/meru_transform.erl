-module(meru_transform).

-export([parse_transform/2]).

-import(erl_syntax, [
    attribute_name/1,
    attribute_arguments/1,
    list_elements/1,
    atom_value/1,
    tuple_elements/1,
    integer_value/1
]).

-record(state, {
    record,
    funs = ordsets:new()
}).

parse_transform(Forms, Opts) ->
    Context = parse_trans:initial_context(Forms, Opts),
    State = parse_trans:do_inspect(fun inspect/4, #state{}, Forms, Context),
    Result = parse_trans:revert(add_get_function(Forms, State, Context)),
    parse_trans:optionally_pretty_print(Result, Opts, Context),
    Result.

inspect(attribute, Form, _Context, Acc) ->
    case atom_value(attribute_name(Form)) of
        meru_record ->
            undefined = Acc#state.record,
            RecordName = hd([atom_value(V) ||
                V <- list_elements(hd(attribute_arguments(Form)))]),
            {false, Acc#state{record = RecordName}};
        _ ->
            {false, Acc}
    end;
inspect(_, _, _, Acc) ->
    {false, Acc}.

add_get_function(Forms, #state{ record = RecordName }, Context) ->
    Form = {function, 1, get, 1,
            [{clause, 1, 
                [{var, 1, 'Key'}],
                [],
                [{tuple, 1, [{atom, 1, RecordName},
                             {var, 1, 'Key'}]}]}]},
    parse_trans:do_insert_forms(above, [Form], Forms, Context).
