{application,db,
             [{description,[]},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib,protobuffs,riakc]},
              {mod,{db_app,[]}},
              {env,[{riak,{pb,{"127.0.0.1",8081}}}]},
              {modules,[db_app,db_c,db_obj]}]}.
