%%%-------------------------------------------------------------------
%%% @copyright
%%% Copyright (C) 2011 by Bermuda Triangle
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_utils_test).

-include_lib("eunit/include/eunit.hrl").

parallel_startup_order_test() ->
    ClustConf = [{host, "stephan.pcs", "system_manager",
                  [{release, web_frontend, web_frontend, []},
                   {release, xmpp_frontend, xmpp_frontend, []}
                  ]},
                 {host, "jd.pcs", "system_manager",
                  [{release, backend, backend,
                    [{db,[{riak,{pb,{"dilshod.pcs",8081}}},
                          {db_workers,50}]}
                    ]}]},
                 {host, "dilshod.pcs", "system_manager",
                  [{release, riak, riak, []}]},
                 {host, "tiina.pcs", "system_manager",
                  [{release, backend, backend,
                    [{db,[{riak,{pb,{"andre.pcs",8081}}},
                          {db_workers,50}]}
                    ]}]},
                 {host, "andre.pcs", "system_manager",
                  [{release, riak, riak, []}]}
                ],
    Expected = [
                [{"andre.pcs", "system_manager",riak, riak},
                 {"dilshod.pcs", "system_manager",riak, riak}],
                [{"tiina.pcs", "system_manager",backend, backend}],
                [{"jd.pcs", "system_manager",backend, backend}],
                [{"stephan.pcs", "system_manager",xmpp_frontend, xmpp_frontend},
                 {"stephan.pcs", "system_manager",web_frontend, web_frontend}]
               ],
    Actual = cluster_utils:parallel_startup_order(ClustConf),
    ?assertEqual(Expected, Actual),

    Conf2 = [
             {host, "jd.pcs", "system_manager",
              [{release, backend, backend,
                [{db,[{riak,{pb,{"dilshod.pcs",8081}}},
                      {db_workers,50}]}
                ]}]},
             {host, "tiina.pcs", "system_manager",
              [{release, backend, backend,
                [{db,[{riak,{pb,{"andre.pcs",8081}}},
                      {db_workers,50}]}
                ]}]},
             {host, "andre.pcs", "system_manager",
              [{release, backend, backend,
                [{db,[{riak,{pb,{"andre.pcs",8081}}},
                      {db_workers,50}]}
                ]}]}
            ],
    Expected2 = [
                 [{"andre.pcs", "system_manager",backend, backend}],
                 [{"tiina.pcs", "system_manager",backend, backend},
                  {"jd.pcs", "system_manager",backend, backend}]
                ],
    Actual2 = cluster_utils:parallel_startup_order(Conf2),
    ?assertEqual(Expected2, Actual2).

generate_startup_order_test() ->
    ClustConf = [{host, "stephan.pcs", "system_manager",
                  [{release, web_frontend, web_frontend, []},
                   {release, xmpp_frontend, xmpp_frontend, []}
                  ]},
                 {host, "jd.pcs", "system_manager",
                  [{release, backend, backend,
                    [{db,[{riak,{pb,{"dilshod.pcs",8081}}},
                          {db_workers,50}]}
                    ]}]},
                 {host, "dilshod.pcs", "system_manager",
                  [{release, riak, riak, []}]},
                 {host, "tiina.pcs", "system_manager",
                  [{release, backend, backend,
                    [{db,[{riak,{pb,{"andre.pcs",8081}}},
                          {db_workers,50}]}
                    ]}]},
                 {host, "andre.pcs", "system_manager",
                  [{release, riak, riak, []}]}
                ],
    % actually we don't care whether dilshod's or andre's riak comes first,
    % but sort is stable so they should come in the order in the ClustConf.
    % same goes for any other release type.
    Expected = [
                {"andre.pcs", "system_manager",riak, riak},
                {"dilshod.pcs", "system_manager",riak, riak},
                {"tiina.pcs", "system_manager",backend, backend},
                {"jd.pcs", "system_manager",backend, backend},
                {"stephan.pcs", "system_manager",xmpp_frontend, xmpp_frontend},
                {"stephan.pcs", "system_manager",web_frontend, web_frontend}
               ],
    Actual = cluster_utils:generate_startup_order(ClustConf),
    ?assertEqual(Expected, Actual).

preprocess_clustconf_empty_test() ->
    ClustConf = [],
    Expected = [],
    Actual = cluster_utils:preprocess_clustconf(ClustConf),
    ?assertEqual(Expected, Actual).

preprocess_clustconf_no_releases_test() ->
    ClustConf = [{host, "host1.tld", "system_manager", []}],
    Expected = [{host, "host1.tld", "system_manager", []}],
    Actual = cluster_utils:preprocess_clustconf(ClustConf),
    ?assertEqual(Expected, Actual).

preprocess_clustconf_one_backend_test() ->
    ClustConf = [{host, "host1.tld", "system_manager",
                  [{release, backend, backend, []}]}],
    Expected = [{host, "host1.tld", "system_manager",
                 [{release, backend, backend,
                   [{controller_app, [{backend_nodes,
                                       ['backend@host1.tld']}]
                    },
                    {game, [{backend_nodes,
                             ['backend@host1.tld']}]
                    }]
                  }]
                }],
    Actual = cluster_utils:preprocess_clustconf(ClustConf),
    ?assertEqual(Expected, Actual).

preprocess_clustconf_node_prefix_test() ->
    ClustConf = [{host, "host1.tld", "sys_mgr",
                  [
                   {release, backend, backend1, []},
                   {release, riak, bob, []}
                  ]
                 }
                ],
    Expected = [{host, "host1.tld", "sys_mgr",
                 [
                  {release, backend, backend1,
                   [{controller_app, [{backend_nodes,
                                       ['backend1@host1.tld']}
                                     ]
                    },
                    {game, [{backend_nodes,
                             ['backend1@host1.tld']}
                           ]
                    }]
                  },
                  {release, riak, bob, []}
                 ]
                }],
    Actual = cluster_utils:preprocess_clustconf(ClustConf),
    ?assertEqual(Expected, Actual).

preprocess_clustconf_one_back_one_front_test() ->
    ClustConf = [{host, "host1.tld", "system_manager",
                  [{release, backend, backend, []}]},
                 {host, "host2.tld", "system_manager",
                  [{release, smtp_frontend, smtp_frontend, []}]}],
    Expected = [{host, "host1.tld", "system_manager",
                 [{release, backend, backend,
                   [{controller_app, [{backend_nodes,
                                       ['backend@host1.tld']
                                      }
                                     ]
                    },
                    {game, [{backend_nodes,
                             ['backend@host1.tld']
                            }
                           ]
                    }
                   ]
                  }
                 ]
                },
                {host, "host2.tld", "system_manager",
                 [{release, smtp_frontend, smtp_frontend,
                   [{controller_app, [{backend_nodes,
                                       ['backend@host1.tld']
                                      }
                                     ]
                    }
                   ]
                  }
                 ]
                }],
    Actual = cluster_utils:preprocess_clustconf(ClustConf),
    ?assertEqual(Expected, Actual).

preprocess_clustconf_realistic_test() ->
    ClustConf = [{host, "stephan.pcs", "system_manager",
                  [{release, web_frontend, web_frontend, []},
                   {release, xmpp_frontend, xmpp_frontend, []}
                  ]
                 },
                 {host, "jd.pcs", "system_manager",
                  [{release, backend, backend,
                    [{db,[{riak,{pb,{"dilshod.pcs",8081}}},
                          {db_workers,50}
                         ]
                     }
                    ]
                   } % end backend release
                  ]
                 },
                 {host, "dilshod.pcs", "system_manager",
                  [{release, riak, riak, []}]},
                 {host, "tiina.pcs", "system_manager",
                  [{release, backend, backend,
                    [{db,[{riak,{pb,{"andre.pcs",8081}}},
                          {db_workers,50}
                         ]
                     }
                    ]
                   } % end backend release
                  ]
                 },
                 {host, "andre.pcs", "system_manager",
                  [{release, riak, riak, []}]}],
    Expected = [{host, "stephan.pcs", "system_manager",
                 [{release, web_frontend, web_frontend,
                   [{controller_app,
                     [{backend_nodes, ['backend@tiina.pcs',
                                       'backend@jd.pcs']
                      }
                     ]
                    }
                   ]
                  },
                  {release, xmpp_frontend, xmpp_frontend,
                   [{controller_app,
                     [{backend_nodes, ['backend@tiina.pcs',
                                       'backend@jd.pcs']
                      }
                     ]
                    }
                   ]
                  }
                 ]
                },
                {host, "jd.pcs", "system_manager",
                 [{release, backend, backend,
                   [{db,[{riak,{pb,{"dilshod.pcs",8081}
                               }
                         },
                         {db_workers,50}
                        ]
                    },
                    {controller_app, [{backend_nodes, ['backend@tiina.pcs',
                                                       'backend@jd.pcs']
                                      }
                                     ]
                    },
                    {game, [{backend_nodes, ['backend@tiina.pcs',
                                             'backend@jd.pcs']
                            }
                           ]
                    }
                   ]
                  }
                 ]
                },
                {host, "dilshod.pcs", "system_manager",
                 [{release, riak, riak, []}]},
                {host, "tiina.pcs", "system_manager",
                 [{release, backend, backend,
                   [{db,[{riak,{pb,{"andre.pcs",8081}
                               }
                         },
                         {db_workers,50}]},
                    {controller_app, [{backend_nodes, ['backend@tiina.pcs',
                                                       'backend@jd.pcs']
                                      }
                                     ]
                    },
                    {game, [{backend_nodes, ['backend@tiina.pcs',
                                             'backend@jd.pcs']
                            }
                           ]
                    }
                   ]
                  }
                 ]
                },
                {host, "andre.pcs", "system_manager",
                 [{release, riak, riak, []}]}
               ],
    Actual = cluster_utils:preprocess_clustconf(ClustConf),
    %% I'm leaving this here for debugging.
    %% It seems the easiest way to debug the cluster config behemoth
    %% is to write the files to disk and diff them.
    %% If you need to debug these, uncomment the file_write calls
    %% and do soemthing like diff .eunit/expected .eunit/actual .
    %file:write_file("expected", io_lib:format("~p", [Expected])),
    %file:write_file("actual", io_lib:format("~p", [Actual])),
    ?assertEqual(Expected, Actual).
