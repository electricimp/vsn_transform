-module(vsn_transform).
-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    parse_transform(Forms,
                    proplists:get_value(vsn, Options, undefined),
                    proplists:get_value(vsn_command, Options, undefined)).

parse_transform(Forms, undefined, undefined) ->
    Forms;
parse_transform(Forms, Vsn, undefined) ->
    apply_vsn(Forms, ensure_string(Vsn));
parse_transform(Forms, undefined, Command) ->
    Output = os:cmd(ensure_string(Command)),
    % Strip trailing whitespace.
    Vsn = re:replace(Output, "[ \t\n]$", "", [global, {return, list}]),
    apply_vsn(Forms, Vsn);
parse_transform(_Forms, _, _) ->
    error(both_vsn_and_command).

apply_vsn(Forms, Vsn) ->
    HasVsn = lists:any(
               fun({attribute, _, vsn, _V}) ->
                       true;
                  (_) -> false
               end, Forms),
    % Lie about the line number, so we don't offset everything else.
    Line = 1,
    VsnAttr = {attribute, Line, vsn, Vsn},
    apply_vsn(HasVsn, VsnAttr, Forms).

apply_vsn(true, _VsnAttr, Forms) ->
    Forms;
apply_vsn(false, VsnAttr,
          [{attribute, _, file, _F} = FileAttr,
           {attribute, _, module, _M} = ModuleAttr | Rest]) ->
    % We know that file and module have to come first; we want to insert
    % immediately afterwards, so we can get away with this:
    [FileAttr, ModuleAttr, VsnAttr | Rest];
apply_vsn(false, _VsnAttr, _Forms) ->
    % Does your source file have a module attribute?
    error(no_module_attribute).

ensure_string(S) when is_list(S) -> S;
ensure_string(S) when is_binary(S) -> binary_to_list(S).
