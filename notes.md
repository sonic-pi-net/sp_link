

* Perhaps remove the link_ from the erlang function names? Might be sort of redundant since it is alrady in package sp_link...?

* Useful links:
- https://github.com/Ableton/link/blob/master/include/ableton/Link.hpp
- http://ableton.github.io/link/

Session State is a class that contains:
-


The API is:
- link_enable
- link_is_enabled

- link_get_tempo
- link_set_tempo

- link_get_num_peers

- link_get_beat_at_time
- link_get_phase_at_time
- link_get_time_at_beat
- link_request_beat_at_time
- link_force_beat_at_time

- link_set_is_playing
- link_get_is_playing
- link_get_time_for_is_playing

- link_is_start_stop_sync_enabled
- link_start_stop_sync_enable

- link_request_beat_at_start_playing_time
- link_set_is_playing_and_request_beat_at_time

- link_get_current_time_microseconds: an aux function to get current time in microseconds. See _clock()_ method in Link. See __sp_link_get_current_time_microseconds__

- link_set_link_callback_pid: a function to register a callback for the Link callbacks. See __sp_link_set_link_callback_pid_nif__
The callbacks are:
- setNumPeersCallback
- setTempoCallback
- setStartStopCallback

Need C99 compliant compiler, since I am using C bools, that should not be an issue in this day and age, but keep an eye

* When passing parameters from erlang, careful that integers are not converted to double automatically, so need to be explicit

* To be notified and be able to affect start / stop status you need to enable Start / Stop sync, by calling __link_start_stop_sync_enable(true)__