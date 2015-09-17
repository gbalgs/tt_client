{application, gb_util,
 [{description, "util tool!"},
  {id, "gb_util"},
  {vsn, "0.1"},
  {modules, [gb_util, gb_util_app, gb_util_config, gb_util_dynamic_compile, gb_util_interval_timer, gb_util_proto, gb_util_sup, gb_util_tc]},
  {registered, []},
  {applications, [
		kernel,
		stdlib
	]},
  {mod, {gb_util_app, []}},
  {env, []}
  ]}.
