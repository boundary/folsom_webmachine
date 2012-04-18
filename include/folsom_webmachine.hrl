-define(DISPATCH, [
                   {["_system"],folsom_webmachine_system_resource,[]},
                   {["_memory"],folsom_webmachine_memory_resource,[]},
                   {["_statistics"],folsom_webmachine_statistics_resource,[]},
                   {["_process"],folsom_webmachine_process_resource,[]},
                   {["_port"],folsom_webmachine_port_resource,[]},
                   {["_ping"],folsom_webmachine_ping_resource,[]},
                   {["_health"], folsom_webmachine_health_resource, []},
                   {["_metrics"],folsom_webmachine_metrics_resource,[]},
                   {["_metrics",id],folsom_webmachine_metrics_resource,[]},
                   {["_ets"],folsom_webmachine_ets_resource,[]},
                   {["_dets"],folsom_webmachine_dets_resource,[]}
                  ]).
