# rebar_example

Example of using `vsn_transform` with a simple `rebar.config` file.

```
$ rebar3 compile
$ rebar3 shell
1> beam_lib:version("_build/default/lib/rebar_example/ebin/rebar_example.beam").
```
