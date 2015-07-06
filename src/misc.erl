-module(misc).
-compile([export_all]).
-include("const.hrl").

%%% -------------------------------------------------------------------
%%% 字符串处理操作
%%% -------------------------------------------------------------------
do_string_to_term(String) ->
  %% io:format("~p ~n", [String]),
  TmpString1 = string:strip(String, both, $.),
  TmpString = string:strip(TmpString1, both, $"),
  NewString = TmpString ++ ".",
  %% io:format("new ~p~n", [NewString]),
  {ok, Tokens,_EndLine} = erl_scan:string(NewString),
  case erl_parse:parse_exprs(Tokens) of
    {ok, AbsForm} ->
      {value, Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
      Value;
    _ ->
      %% Trace=get_stacktrace(),
      %% ?ERROR("string_to_term ~p", [Trace]),
      ok
  end.


string_to_term(String) ->
  case catch do_string_to_term(String) of
    {'EXIT', _} ->
      ?ERROR("String to term error:~p~n", [String]);
    No -> No
  end.

binary_to_term(Binary) ->
  List = binary_to_list(Binary),
  string_to_term(List).

term_to_string(Term) ->
  R = io_lib:format("~w", [Term]),
  lists:flatten(R).

to_term(Param) when is_binary(Param) ->
  misc:binary_to_term(Param);
to_term(Param) when is_list(Param) ->
  lists:map(fun(X) ->
                to_term(X)
            end, Param);
to_term(Param) when is_tuple(Param) ->
  tuple_to_term(Param).

tuple_to_term(Element) ->
  tuple_to_term(1, Element, {}).
tuple_to_term(Number, Element, NewElement) ->
  case catch element(Number, Element) of
    {'EXIT', _} ->
      NewElement;
    BTuple ->
      tuple_to_term(
        Number + 1,
        Element,
        erlang:append_element(NewElement, to_term(BTuple))
       )
  end.

to_list(Param) when is_binary(Param)  ->
  binary_to_list(Param);
to_list(Param) when is_list(Param)  ->
  lists:map(fun(X) ->
                to_list(X)
            end, Param);
to_list(Param) when is_tuple(Param) ->
  misc:tuple_to_list(Param).

tuple_to_list(Element) ->
  tuple_to_list(1, Element, {}).
tuple_to_list(Number, Element, NewElement) ->
  case catch element(Number, Element) of
    {'EXIT', _} ->
      NewElement;
    BTuple ->
      tuple_to_list(
        Number + 1,
        Element,
        erlang:append_element(NewElement, to_list(BTuple))
       )
  end.

term_to_binary(Term) ->
  R= io_lib:format("~p",[Term]),
  list_to_binary(lists:flatten(R)).

