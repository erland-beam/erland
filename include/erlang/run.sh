export COMPILED_BINARY=./_build/default/bin/testing;
rm \$COMPILED_BINARY >/dev/null 2>&1;
rebar3 do compile, escriptize || { exit 1; };
\$COMPILED_BINARY;
