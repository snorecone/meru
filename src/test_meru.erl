-module(test_meru).
-compile({parse_transform, meru_transform}).

-meru_record([hello]).


% test_meru:get(<<"key">>).
% test_meru:delete(<<"key">>).

