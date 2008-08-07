%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc HTML Form validation functions
%% @end
%%%-------------------------------------------------------------------
-module(erlydtl.forms.form_validator).

-include_lib("eunit.hrl").

%% API
-export([validate/2,
         is_valid/1,
         has_error/1,
         invalid_fields/1,
         field_errors/2,
         errors/1,
         valid_fields/2]).

%%====================================================================
%% API
%%====================================================================

%% @spec validate(Rules, Data) -> [Result]
%% Rules = [form_validation_rule()]
%% Data = [{Key::string(), Value::string()}]
%% Result = {Key::string(), [predicate_result()]}
validate(Rules,Data) when is_list(Rules), is_list(Data) ->
    lists:map(fun (Rule) -> validate_rule(Rule, Data) end,
              Rules).

invalid_fields(Results) ->
    [Field || {Field, Errors} <- Results,
              length(Errors) >= 1].

is_valid(Results) ->
    invalid_fields(Results) =:= [].

has_error(Results) ->
    is_valid(Results) =:= false.

field_errors(Name, Results) ->
    case lists:keysearch(Name, 1, Results) of
        false -> erlang:error(no_such_field);
        {value, V} -> V
    end.

errors(Results) ->
    [Field || Field = {_, Errors} <- Results,
              length(Errors) >= 1].

valid_fields(Results, Data) ->
    Fields = [ Field || {Field, []} <- Results],
    [ {K,V} || {K,V} <- Data,
               lists:member(K, Fields) ].

%%====================================================================
%% Internal functions
%%====================================================================

validate_rule({Name, Predicates}, Data) when is_list(Name), is_list(Predicates) ->
    {Name,
     lists:flatmap(fun (Predicate) ->
                           case validate_predicate(Predicate, Name, Data) of
                               true -> [];
                               false -> [{error, Predicate, false}];
                               {error, Reason} -> [{error, Predicate, Reason}]
                           end
                  end, normalize_predicates(Predicates))}.

%% @spec validate_predicate(Predicate, Name, Data) -> true | false | {error, Reason}
%% @doc Checks a single condition in a rule.
%% @end
validate_predicate({duplication, [Field|Duplicates]}, _Name, Data)
  when is_list(Duplicates), length(Duplicates) >= 1 ->
    FieldValue = proplists:get_value(Field, Data),
    same_value(Field, FieldValue, Duplicates, Data);
    
validate_predicate(Predicate, Name, Data) ->
    validate_predicate_simple(Predicate, proplists:get_value(Name, Data)).

%% @private
validate_predicate_simple(not_empty, L) when is_list(L), length(L) > 0 -> true;
validate_predicate_simple(not_empty, _) -> false;
validate_predicate_simple(string, L) when is_list(L) -> true;
validate_predicate_simple(string, _) -> false;
validate_predicate_simple({length, [Exact]}, L) ->
    validate_predicate_simple({length, [Exact, Exact]}, L);
validate_predicate_simple({length, [Min,Max]}, L) when is_list(L) -> 
    case length(L) of
        Len when Min =< Len, Len =< Max -> true;
        _ -> false
    end;
validate_predicate_simple({length, [_Min,_Max]}, _L) -> false;
validate_predicate_simple({predicate, P}, L) -> P(L);
validate_predicate_simple(email_address, L) when is_list(L) ->
    email_address:validate(L);
validate_predicate_simple({regex, RE}, L) ->
    case rvre:match(L, RE) of
        nomatch -> false;
        {match, _} -> true;
        {error, R} -> {error, R}
    end;
validate_predicate_simple(P, _) -> erlang:error({not_implemented, P}).

%% @private
normalize_predicates(Predicates) when is_list(Predicates) ->
    proplists:normalize(Predicates, [{expand, [{password, string}]}]).

%% @private
same_value(_Field, _FieldValue, [], _Data) -> true;
same_value(Field, FieldValue, [Duplicate|Rest], Data) ->
    case proplists:get_value(Duplicate, Data) of
        undefined -> {error, {missing, Field}};
        V when V =:= FieldValue ->
            same_value(Field, FieldValue, Rest, Data);
        _ -> {error, {different_value, Field, Duplicate}}
    end.
