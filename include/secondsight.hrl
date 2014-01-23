-record(package_v1, {
          event_type :: atom(),
          event :: term(),
          timestamp :: erlang:timestamp(),
          node :: atom(),
          ring_members :: [atom()]
         }).
-define(PACKAGE, #package_v1).
