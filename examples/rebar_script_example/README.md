# rebar_config_example

Example of using `vsn_transform` with a `rebar.config.script` file.

```
$ GIT_VSN=1.0.2 rebar3 compile
$ rebar3 shell
1> beam_lib:version("_build/default/lib/rebar_script_example/ebin/rebar_script_example.beam").
```

Note that you can't use `VSN` as the environment variable, because rebar3 crashes, but only if you've installed it with
`rebar3 local install`.
