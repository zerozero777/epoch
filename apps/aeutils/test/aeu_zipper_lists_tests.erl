%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aeu_zipper_lists module
%%% @end
%%%=============================================================================
-module(aeu_zipper_lists_tests).
-compile({no_auto_import, [length/1]}).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TM, aeu_zipper_lists).

zipper_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun({L, Zl}) ->
              {"Get current value",
               fun() ->
                       ?assertEqual({[], L}, Zl),
                       ?assertEqual(1, ?TM:current(Zl))
               end}
      end,
      fun({L, Zl}) ->
              {"Length",
               fun() ->
                       ?assertEqual(erlang:length(L), ?TM:length(Zl))
               end}
      end,
      fun({_L, Zl}) ->
              {"Next",
               fun() ->
                       {ok, Zl2} = repeat(3, fun({ok, Zl0}) ->
                                                     ?TM:next(Zl0)
                                             end, {ok, Zl}),
                       ?assertEqual({[3,2,1], [4,5,6,7,8,9,10]}, Zl2),
                       ?assertEqual(?TM:length(Zl), ?TM:length(Zl2)),
                       Zl3 = ?TM:next_loop(?TM:next_loop(?TM:next_loop(Zl))),
                       ?assertEqual({[3,2,1], [4,5,6,7,8,9,10]}, Zl3),
                       ?assertEqual(?TM:length(Zl), ?TM:length(Zl3)),
                       ?assertEqual({[3,2,1], [4,5,6,7,8,9,10]},
                                    ?TM:next_loop(?TM:next_loop(?TM:next_loop(Zl)))),
                       ?assertEqual({error, end_of_list}, repeat(?TM:length(Zl),
                                                                 fun({ok, Zl0}) ->
                                                                         ?TM:next(Zl0)
                                                                 end,
                                                                 {ok, Zl})),
                       ?assertEqual(Zl, repeat(?TM:length(Zl),
                                               fun(Zl0) ->
                                                       ?TM:next_loop(Zl0)
                                               end,
                                               Zl))
               end}
      end,
      fun({_L, Zl}) ->
              {"Previous",
               fun() ->
                       {ok, ZlN} = ?TM:next(Zl),
                       ?assertEqual({ok, Zl}, ?TM:prev(ZlN)),
                       ?assertEqual(Zl, ?TM:prev_loop(?TM:next_loop(Zl))),
                       {ok, Zl2} = repeat(?TM:length(Zl) - 1,
                                          fun({ok, Zl0}) ->
                                                  ?TM:next(Zl0)
                                          end,
                                          {ok, Zl}),
                       ?assertEqual({error, end_of_list}, ?TM:prev(Zl)),
                       ?assertEqual(Zl2, ?TM:prev_loop(Zl)),
                       ?assertEqual(?TM:length(Zl2), ?TM:length(Zl)),
                       ?assertEqual({[7,6,5,4,3,2,1], [8,9,10]},
                                    ?TM:prev_loop(?TM:prev_loop(?TM:prev_loop(Zl)))),
                       ?assertEqual({ok, Zl}, repeat(9, fun({ok, Zl0}) ->
                                                                ?TM:prev(Zl0)
                                                        end, {ok, Zl2})),
                       ?assertEqual(Zl, repeat(10, fun(Zl0) ->
                                                           ?TM:prev_loop(Zl0)
                                                   end, Zl))
               end}
      end]
    }.

indexed_zipper_test_() ->
    {foreach,
     fun setup2/0,
     fun teardown/1,
     [fun({_L, Zl}) ->
              {"Get current value",
               fun() ->
                       ?assertEqual({1, 0}, ?TM:current(Zl)),
                       ?assertEqual({4, 3}, ?TM:current(?TM:next_loop(
                                                           ?TM:next_loop(
                                                              ?TM:next_loop(Zl)))))
               end}
      end,
      fun({_L, Zl}) ->
              {"Find index",
               fun() ->
                       {ok, Zl6} = ?TM:find(6, Zl),
                       ?assertEqual({ok, 7}, ?TM:current(Zl6)),
                       ?assertEqual({error, end_of_list}, ?TM:find(11, Zl)),
                       ?assertEqual({error, end_of_list}, ?TM:find(-1, Zl))
               end}
      end]
    }.

setup() ->
    L = [1,2,3,4,5,6,7,8,9,10],
    {L, ?TM:new(L)}.

setup2() ->
    L = [1,2,3,4,5,6,7,8,9,10],
    {L, ?TM:new_indexed(L)}.

teardown(_) ->
    ok.

repeat(0, _Fun, Val) ->
    Val;
repeat(N, Fun, Val) when N > 0 ->
    repeat(N - 1, Fun, Fun(Val)).

-endif.
