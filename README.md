# secondsight

Let us not ask "could you give me the result of riak-debug" and
"could you show me your graphs of Riak's stats?" This also enables
us to proactively find Riak cluster status.

1. Riak => data => secondsignt =(metrics)=> CSE
2. CSE => useful insight => users => happy

# design

## Riak's log collector

- put a gen_event handler .beam into basho-patches and boot it.
 - receives all events [1]
 - also periodically probes riak_kv:stat there [2]
 - and transfer to secondsight endpoint as JSON

- secondsight listens to their PUT requests [3]
 - stores into Riak w/ search on
 - enable 2i if on (to long data query)

- secondsight runs query when GET API comes
 - design API [4]
 - what query to search [5]

- viewers press F5 to update
- or choose metrics with button?

[1] - [5] are short term TODOs

## long-term TODESIGN

- Best Representation to understand situation
- What else data we need?
- Automatic Prediction?
- Authorization
- Active probe from secondsight into Riak cluster
- Cool coloring and design
