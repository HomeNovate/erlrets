{application, erlrets,
 [
  {description, "Erlang Driver Client for RETS"},
  {vsn, "0.1"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib, 
                  lager
                 ]},
  {mod, { erlrets_app, []}},
  {modules,[ erlrets_driver_nif, 
             erlrets, 
             erlrets_app, 
             erlrets_sup]}
 ]}.
