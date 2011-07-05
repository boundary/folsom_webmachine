### folsom_webmachine

This is an application that exposes folsom metrics via http using webmachine.


#### api

Query the list of available metrics:

        $ curl http://localhost:5565/_metrics

Query a specific metric:
        
        $ curl http://localhost:5565/_metrics/name

Create a new metric:

        $ curl -X PUT http://localhost:5565/_metrics -d '{"id": "a", "type": "counter"}' -H "Content-Type: application/json"
        
Update a metric:

        $ curl -X PUT http://localhost:5565/_metrics/a -d '{"value" : {"inc": 1}}' -H "Content-Type: application/json"

Delete a metric:

        $ curl -X DELETE http://localhost:5565/_metrics/a

Query Erlang VM metrics:
        
        $ curl http://localhost:5565/_memory

Query Erlang VM stats:
        
        $ curl http://localhost:5565/_statistics

Query Erlang VM information:

        $ curl http://localhost:5565/_system       

#### output

        $ curl http://localhost:5565/_metrics/a
        {"value":{"min":1,"max":1000,"mean":322.2,"median":100,"variance":185259.19999999998,"standard_deviation":430.4174717643325,"skewness":1.2670136514902162,"kurtosis":-1.2908313302242205,"percentile":{"75":500,"95":1000,"99":1000,"999":1000},"histogram":{"10":2,"20":0,"30":0,"50":0,"100":1,"200":0,"300":0,"400":0,"500":1,"1000":1,"99999999999999":0}}}      
        
         
         $ curl http://localhost:5565/_metrics/test
         
         {"value":{"1303483997384193":{"event":"asdfasdf"}}}
         
         
         $ curl http://localhost:5565/_memory
         {"total":11044608,"processes":3240936,"processes_used":3233888,"system":7803672,"atom":532137,"atom_used":524918,"binary":696984,"code":4358030,"ets":385192}