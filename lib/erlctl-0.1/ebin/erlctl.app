{application,
  erlctl,
  [
    {description, "ErlCtl Control Framework"},
    {vsn, "0.1"},
    {modules, [erlctl_app,erlctl_sup]},
    {registered, [erlctl_sup]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {erlctl_app,[]}},
    {env, []}
 ]
}.
