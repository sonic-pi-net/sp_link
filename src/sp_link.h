#pragma once

#include <stdbool.h>
#include <erl_nif.h>

#ifdef WIN32
#define DllExport   __declspec( dllexport )
#else
#define DllExport
#endif

#ifdef __cplusplus
extern "C" {
#endif

    // TODO: These are exported for C tests. Once we are happy that it's working they should not be exported
    /**
     * Initialize the sp_link library. Must be called before anything else.
     *
     * @return 0 if ok, < 0 if error
     */
    DllExport int sp_link_init(double bpm);

    /**
     * Deinitialize the sp_link library.
     */
    DllExport void sp_link_deinit();


    DllExport int sp_link_enable(bool enable);

    DllExport int sp_link_is_enabled(bool* enabled);

    DllExport int sp_link_set_tempo(double bpm, long long micros);

    DllExport int sp_link_get_tempo(double* bpm);

    DllExport int sp_link_get_num_peers(int* num_peers);

    DllExport int sp_link_is_start_stop_sync_enabled(bool* enabled);

    DllExport int sp_link_start_stop_sync_enable(bool enable);

    DllExport int sp_link_set_is_playing(bool is_playing, long long micros);

    DllExport int sp_link_is_playing(bool* is_playing);

    DllExport int sp_link_get_time_for_is_playing(long long* micros);

    DllExport int sp_link_get_beat_at_time(long long micros, double quantum, double* beat);

    DllExport int sp_link_get_phase_at_time(long long micros, double quantum, double* phase);

    DllExport int sp_link_get_time_at_beat(double beat, double quantum, long long* micros);

    DllExport int sp_link_request_beat_at_time(double beat, long long micros, double quantum);

    DllExport int sp_link_force_beat_at_time(double beat, long long micros, double quantum);

    DllExport int sp_link_set_is_playing_and_request_beat_at_time(bool is_playing, long long micros, double beat, double quantum);

    DllExport int sp_link_get_current_time_microseconds(long long* micros);


    /************** Functions for the erlang integration below ***************/

    // Erlang NIFs. The NIF parameters are always the same, I will only explain the parameters as unpacked from erlang.
    /**
     * Initialize the sp_link library. Must be called before anything else.
     *
     * @return 0 if ok, < 0 if error
     */
    DllExport ERL_NIF_TERM sp_link_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Deinitialize the sp_link library.
     */
    DllExport ERL_NIF_TERM sp_link_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_is_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_set_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_set_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_time_for_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_num_peers_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_start_stop_sync_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_is_start_stop_sync_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_phase_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_time_at_beat_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_force_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_request_beat_at_start_playing_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_set_is_playing_and_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_current_time_microseconds_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#ifdef __cplusplus
}
#endif

