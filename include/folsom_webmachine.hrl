-define(DISPATCH, [{["_system"],folsom_webmachine_system_resource,[]},
                  {["_memory"],folsom_webmachine_memory_resource,[]},
                  {["_statistics"],folsom_webmachine_statistics_resource,[]},
                  {["_process"],folsom_webmachine_process_resource,[]},
                  {["_socket"],folsom_webmachine_socket_resource,[]},
                  {["_metrics"],folsom_webmachine_metrics_resource,[]},
                  {["_metrics",id],folsom_webmachine_metrics_resource,[]}]).
