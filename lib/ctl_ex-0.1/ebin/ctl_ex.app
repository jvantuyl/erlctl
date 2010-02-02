{application,
  ctl_ex,
  [
    {description, "Example Application for ErlCtl Framework"},
    {vsn, "0.1"},
    {modules, [ctl_ex_app,ctl_ex_sup,ctl_ex,ctl_ex_cli]},
    {registered, [ctl_ex_sup,ctl_ex]},
    {applications, [kernel, stdlib, sasl, erlctl]},
    {mod, {ctl_ex_app,[]}},
    {env, []}
 ]
}.
