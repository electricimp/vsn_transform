-module(vsn_transform).
-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    Command = proplists:get_value(vsn_command, Options, undefined),
    parse_transform(Forms, Command, Options).

parse_transform(Forms, undefined, _Options) ->
    Forms;
parse_transform(Forms, Command, _Options) ->
    Output = os:cmd(Command),
    % Strip trailing whitespace.
    Vsn = re:replace(Output, "[ \t\n]$", "", [global, {return, list}]),
    HasVsn = lists:any(
               fun({attribute, _, vsn, _V}) ->
                       true;
                  (_) -> false
               end, Forms),
    % Lie about the line number, so we don't offset everything else.
    VsnAttr = {attribute, 1, vsn, Vsn},
    apply_vsn(HasVsn, VsnAttr, Forms).

apply_vsn(true, _VsnAttr, Forms) ->
    Forms;
apply_vsn(false, VsnAttr,
          [{attribute, _, file, _F} = FileAttr,
           {attribute, _, module, _M} = ModuleAttr | Rest]) ->
    % We know that file and module have to come first; we want to insert
    % immediately afterwards, so we can get away with this:
    [FileAttr, ModuleAttr, VsnAttr | Rest].
