# vsn_transform

Erlang parse transform to add `-vsn` attributes from (e.g.) git.

## Motivation

By default, an Erlang module has a `vsn` attribute that's the MD5 checksum of the module. You'd probably prefer
something easier to read, such as `1.0.3`. You can control this version by adding a `-vsn` module attribute, for
example:

```erlang
-module(foo).
-vsn("1.0.2").

% ...
```

But that's tedious. So we wrote a parse transform that allows you to set the version attribute at build time.

## Using it with rebar3

Add it to your `rebar.config` file, as follows:

    {deps, [
        {vsn_transform, ".*",
            {git, "https://github.com/rlipscombe/vsn_transform.git"}}
    ]}.

    {erl_opts, [
        {parse_transform, vsn_transform},
        {vsn_command, "git describe --tags"}
    ]}.

Obviously, you can change the command as needed. You might want to use `git rev-parse --short HEAD`, for example.

### Setting it explicitly

Note that the parse transform runs the command once for each file being compiled, which can sometimes be slow. If you'd
prefer, you can set the version explicitly:

        {vsn, "1.0.2"}

You lose some flexibility, because the value's now hard-coded in `rebar.config`. If you'd prefer to use an environment
variable, create a `rebar.config.script` file. See `examples/rebar_script_example`.

## Did it work?

You can check it with the following:

    lists:map(
        fun({Mod, Path}) ->
            Vsn = proplists:get_value(vsn, Mod:module_info(attributes)),
            {Mod, Vsn}
        end, code:all_loaded()).

Or with:

    beam_lib:version("path/to/some.beam").

## License

Apache 2.0
