-define(RAD_ROVER, 0.5).
-define(RAD_MARTIAN, 0.4).

-record(mob, {x, y, r = ?RAD_ROVER, dir = 0, speed = 0}).
-record(init, {x_limit, y_limit, time_limit,
	       min_sensor, max_sensor,
	       max_speed, max_turn, max_hard_turn}).
-record(vstate, { time, vctl, vmob }).
