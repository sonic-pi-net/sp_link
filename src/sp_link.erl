-module(sp_link).
-export([link_init/1, link_deinit/0, link_enable/1, link_is_enabled/0, link_set_tempo/2, link_get_tempo/0, link_get_num_peers/0,
    link_start_stop_sync_enable/1, link_is_start_stop_sync_enabled/0, link_set_is_playing/2, link_is_playing/0, link_get_time_for_is_playing/0,
    link_get_beat_at_time/2, link_get_phase_at_time/2, link_get_time_at_beat/2, link_request_beat_at_time/3, link_force_beat_at_time/3,
    link_request_beat_at_start_playing_time/2, link_set_is_playing_and_request_beat_at_time/4, link_set_link_callback_pid/1,
    link_get_current_time_microseconds/0, set_log_level/1]).
-on_load(init/0).

init() ->
    case os:type() of
    {win32, _} ->
        ok = erlang:load_nif("D:/projects/sp_link/build/Debug/libsp_link", 0);
    _Else ->
        ok = erlang:load_nif("/home/luis/projects/sp_link/build/libsp_link", 0)
    end.

link_init(_) ->
    exit(nif_library_not_loaded).
link_deinit() ->
    exit(nif_library_not_loaded).
link_enable(_) ->
    exit(nif_library_not_loaded).
link_is_enabled() ->
    exit(nif_library_not_loaded).
link_set_tempo(_, _) ->
    exit(nif_library_not_loaded).
link_get_tempo() ->
    exit(nif_library_not_loaded).
link_get_num_peers() ->
    exit(nif_library_not_loaded).
link_start_stop_sync_enable(_) ->
    exit(nif_library_not_loaded).
link_is_start_stop_sync_enabled() ->
    exit(nif_library_not_loaded).
link_set_is_playing(_, _) ->
    exit(nif_library_not_loaded).
link_is_playing() ->
    exit(nif_library_not_loaded).
link_get_time_for_is_playing() ->
    exit(nif_library_not_loaded).
link_get_beat_at_time(_, _) ->
    exit(nif_library_not_loaded).
link_get_phase_at_time(_, _) ->
    exit(nif_library_not_loaded).
link_get_time_at_beat(_, _) ->
    exit(nif_library_not_loaded).
link_request_beat_at_time(_, _, _) ->
    exit(nif_library_not_loaded).
link_force_beat_at_time(_, _, _) ->
    exit(nif_library_not_loaded).
link_request_beat_at_start_playing_time(_, _) ->
    exit(nif_library_not_loaded).
link_set_is_playing_and_request_beat_at_time(_, _, _, _) ->
    exit(nif_library_not_loaded).
link_set_link_callback_pid(_) ->
    exit(nif_library_not_loaded).
link_get_current_time_microseconds() ->
    exit(nif_library_not_loaded).
set_log_level(_) ->
    exit(nif_library_not_loaded).
