{application, rrpc,
 [{description, "REST Reverse Polish Calculator."},
  {vsn, "0.1"},
  {registered, [rrpc]},
  {applications, [kernel,
                  stdlib]},
  {mod, { rrpc_app, []}},
  {env, []}
 ]}.
