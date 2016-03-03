-module(jackpot).
-export([start/0, requester/5, responder/2]).

start() ->
  spawn(jackpot, requester, [self(), top, 1, 50, 5]),
  spawn(jackpot, requester, [self(), bottom, 1, 10, 2]),
  receive {top, TopValues} ->
    receive {bottom, BottomValues} ->
      io:write(lists:sort(TopValues)),
      io:write(lists:sort(BottomValues))
    end
  end.

spawnResponders(0, _) ->
  done;
spawnResponders(Number, Value) ->
  spawn(jackpot, responder, [self(), Value]),
  spawnResponders(Number - 1, Value + 1).

collectResponses(0, Values) ->
  Values;
collectResponses(Number, Values) ->
  receive Value ->
    collectResponses(Number - 1, [Value | Values])
  end.

requester(Main, Kind, MinValue, MaxValue, Number) ->
  spawnResponders(MaxValue - MinValue + 1, MinValue),
  Values = collectResponses(Number, []),
  Main ! {Kind, Values}.

fib(0) ->
  0;
fib(1) ->
  1;
fib(N) ->
  erlang:yield(),
  fib(N - 1) + fib(N - 2).

responder(Requester, Value) ->
  fib(30),
  Requester ! Value.

