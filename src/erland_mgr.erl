-module(erland_mgr).

-callback create(Name :: binary(), Id :: binary(), Listener :: pid()) -> ok.
-callback content(
    Name :: binary(),
    Deps :: list({Name :: binary(), Version :: binary()}),
    Content :: binary(),
    Id :: binary(),
    Listener :: pid()
) -> ok.
-callback run(Name :: binary(), Id :: binary(), Listener :: pid()) -> ok.
