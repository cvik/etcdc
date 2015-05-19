-module(etcdc_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

config_test_() ->
    {setup,
     fun setup_config/0,
     fun cleanup_config/1,
     {inorder, [{timeout, 120, {"name", fun() -> a_test end}}]}}.

setup_config() -> ok.
cleanup_config(_) -> ok.
