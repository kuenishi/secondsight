-record(package_v1, {
          event_type :: atom(),
          event :: term(),
          timestamp :: erlang:timestamp(),
          node :: atom(),
          ring_members :: [atom()]
         }).
-define(PACKAGE, #package_v1).

-define(ENDPOINT_HOST, "localhost").
-define(ENDPOINT_PORT, 8080).

%% TODO depricate and use binary and secure protocol
-define(ENDPOINT_URL, "http://localhost:8080/v1/emit/").
%% TODO use secure authorization
%% Authorization: sesame
-define(AUTH_KEY, "sesame").

%% keep how many message?
%% -define(BUFFER_SIZE, 1).
%% -define(FLUSH_PERIOD, 1024). %% ms
-define(POKE_INTERVAL, 1024*10). %% ms

