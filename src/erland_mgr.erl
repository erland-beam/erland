-module(erland_mgr).

-callback create(Name :: binary(), Id :: binary(), Listener :: pid()) -> term().
-callback set(
    Name :: binary(),
    Deps :: map(),
    Content :: binary(),
    Id :: binary(),
    Listener :: pid()
) -> term().
-callback run(Name :: binary(), Id :: binary(), Listener :: pid()) -> term().
