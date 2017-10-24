%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   List optimized for traversal, like https://github.com/ferd/zippers
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_zipper_lists).
-compile({no_auto_import,[length/1]}).

-export([new/1,
         current/1,
         length/1,
         next/1,
         prev/1,
         next_loop/1,
         prev_loop/1,
         new_indexed/1,
         find/2]).


-type zipper_list(T) :: {list(T), list(T)}.
-export_type([zipper_list/1]).


-spec new(list(T)) -> zipper_list(T) when T :: any().
new([_ | _] = L) ->
    %% Make sure we always have a current element
    {[], L}.

-spec current(zipper_list(T)) -> T.
current({_, [H | _]}) ->
    H.

length({L1, L2}) ->
    erlang:length(L1) + erlang:length(L2).

next({_, [_]}) ->
    {error, end_of_list};
next({L, [H | T]}) ->
    {ok, {[H | L], T}}.

next_loop({L, [X]}) ->
    {[], lists:reverse(L) ++ [X]};
next_loop({L, [H | T]}) ->
    {[H | L], T}.

prev({[], _}) ->
    {error, end_of_list};
prev({[H | T], L}) ->
    {ok, {T, [H | L]}}.

prev_loop({[], L}) ->
    {L1, L2} = lists:split(erlang:length(L) - 1, L),
    {lists:reverse(L1), L2};
prev_loop({[H | T], L}) ->
    {T, [H | L]}.

new_indexed([_ | _] = L) ->
    new(lists:zip(L, lists:seq(0, erlang:length(L) - 1))).

find(I, {_, [{IC, _} | _]} = Zl) when is_integer(IC),
                                            is_integer(IC) ->
    case I of
        IC ->
            {ok, Zl};
        _ when I < IC ->
            case prev(Zl) of
                {ok, Zl2} ->
                    find(I, Zl2);
                Err ->
                    Err
            end;
        _ when I > IC ->
            case next(Zl) of
                {ok, Zl2} ->
                    find(I, Zl2);
                Err ->
                    Err
            end
    end.
