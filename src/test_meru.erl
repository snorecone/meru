-module(test_meru).
-compile({parse_transform, meru_transform}).

-meru_record([hello]).

-export([
    test/0
]).

test() -> ok.
% test_meru:get(<<"key">>).
% test_meru:delete(<<"key">>).

