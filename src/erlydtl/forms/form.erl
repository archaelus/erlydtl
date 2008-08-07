%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Form generation, validation and processing library
%% @end
%%%-------------------------------------------------------------------
-module(erlydtl.forms.form).

-include_lib("eunit.hrl").

%% API
-export([create/4,
         render/1,
         text/2,
         text/3,
         password/2,
         password/3,
         submit/1,
         validate/2,
         rules/1]).

-record(form, {title, action, fields = [], rules = []}).
-record(vform, {form, data, validation_result}).
-record(field, {name, type, title, rules = []}).

%%====================================================================
%% API
%%====================================================================

create(Title, Action, Fields, Rules) ->
    #form{title=Title,
          action=Action,
          fields=Fields,
          rules=Rules}.

text(Title, Rules) ->
    text(Title, field_name("txt", Title), Rules).

text(Title, Name, Rules) ->
    #field{type=text, title=Title, name=Name, rules=Rules}.

password(Title, Rules) ->
    password(Title, field_name("pw", Title), Rules).

password(Title, Name, Rules) ->
    #field{type=password, title=Title, name=Name, rules=Rules}.

submit(Name) ->
    #field{type=submit, title=Name, name=Name}.

render(F = #form{}) ->
    render_form(F, []).

render(F = #form{}, ValidFields) ->
    render_form(F, ValidFields).

validate(Form, Data) ->
    form_validator:validate(rules(Form), Data).

rules(#form{fields=Fields, rules=FormRules}) ->
    [{Name,Rules}
     || #field{name=Name,rules=Rules} <- Fields]
        ++ FormRules.

type_description(text) ->
    "Text field: ";
type_description(password) ->
    "Password field: ".

%%====================================================================
%% Internal functions
%%====================================================================

render_form(#form{title=Title,
                  action=Action,
                  fields=Fields}, ValidFields) ->
    form_template:render([{action, Action},
                          {title, Title},
                          {fields, [ T || {ok, T} <- lists:map(fun (F) -> render_field(F, ValidFields) end, Fields)]}]).

render_field(#field{type=submit, name=Name, title=Title}, _Fixme) ->
    field_template:render([{type, "submit"},
                           {description, "Submit button: " ++ Name},
                           {name, Name},
                           {value, Title}]);
render_field(#field{type=Type, name=Name, title=Title}, _Fixme) ->
    named_field_template:render([{name, Name},
                                 {title, Title},
                                 {type, atom_to_list(Type)},
                                 {description, type_description(Type)}]).

field_name(Prefix, Title) ->
    S = lists:filter(fun (C) when $A =< C, C =< $Z;
                         $a =< C, C =< $z;
                         $0 =< C, C =< $9;
                         C =:= $_;
                         C =:= $.;
                         C =:= $- ->
                             true;
                         (_) -> false
                     end,
                     Title),
    Prefix ++ string:to_lower(S).

field_name_test() ->
    ?assertMatch("txtpassword", field_name("txt", "Password:")),
    ?assertMatch("txtpassword", field_name("txt", "pass word")).

create_test() ->
    ?assertMatch(#form{},
                 create("Setup Information", "",
                        [text("User Name:", [{length, [3,30]}]),
                         text("Email address:", [email_address]),
                         password("Password:", "txtpassword", [{length, [8,infinity]}]),
                         password("Confirm Password:", "txtpasswordc", []),
                         submit("Signup")],
                        [{"passwords", [{duplication, ["txtpassword", "txtpasswordc"]}]}])).
