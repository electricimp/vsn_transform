# vsn_transform

Erlang parse transform to add '-vsn' attributes from (e.g.) git

## Using it

Add it to your `rebar.config` file, as follows:

    {deps, [
        {vsn_transform, ".*",
            {git, "https://github.com/electricimp/vsn_transform.git"}}
    ]}.

    {erl_opts, [
        {parse_transform, vsn_transform},
        {vsn_command, "git describe --tags"}
    ]}

Obviously, you can change the command as needed. You might want to use `git
rev-parse --short HEAD`, for example.

At Electric Imp, we use a custom script that generates version strings that
look like "138d21b - release-30.22 - Fri Oct 17 12:15:01 2014".

We specify it as follows:

        {vsn_command, "$BASE_DIR/git-vsn --friendly"}

...where `BASE_DIR` is an environment variable set in the top-level Makefile.

## Did it work?

You can check it with the following:

    lists:map(
        fun({Mod, Path}) ->
            Vsn = proplists:get_value(vsn, Mod:module_info(attributes)),
            {Mod, Vsn} 
        end, code:all_loaded()).

## License

Apache 2.0
