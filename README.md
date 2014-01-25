# secondsight

Let us not ask "could you give me the result of riak-debug" and
"could you show me your graphs of Riak's stats?" This also enables
us to proactively find Riak cluster status.

1. Riak => data => secondsignt =(metrics)=> CSE
2. CSE => useful insight => users => happy

# howto

1. compile
2. copy `secondsight_event.beam` and `secondsight_probe.beam` to basho-patches directory
3. restart riak or type `l(secondsight_event), l(secondsight_probe)` in attached console of riak
4. type `secondsight_event:add_to_lager(), secondsight_probe:start_link()` at attached console of riak
5. see what happens in console.log

# design

## Riak's log collector

- put a gen_event handler .beam into basho-patches and boot it.
 - receives all events [1]
 - also periodically probes riak_kv:stat there [2]
 - and transfer to secondsight endpoint as JSON

- secondsight listens to their PUT requests [3]
 - stores into Riak w/ search on
 - enable 2i if on (to long data query)

 - namespace: types = cluster name/id (global)
              buckets/logs/keys/<name>-<timestamp>
              buckets/stats/keys/<name>-<timestamp>
              buckets/configs/keys/<timestamp> - data/body

- secondsight runs query when GET API comes
 - design API [4]
 - what query to search [5]

- viewers press F5 to update
- or choose metrics with button?

[1] - [5] are short term TODOs

## Qs

- leveldb looks more suitable than bitcask (or much better backend?)
- `gen_event:add_handler(lager_event, secondsight_prober, []).` seems
  successful in delivering log to secondsight server but failing in
  periodical probing with `riak_kv_stat:get_stats/0`. come on!
- for `riak.conf` , it's currently impossible to register custom gen_event handler

## long-term TODESIGN

- create Solr Schema for secondsight, rather than dynamic schema?
- Best Representation to understand situation
- What else data we need?
- Automatic Prediction?
- Authorization
- Active probe from secondsight into Riak cluster
- Cool coloring and design
