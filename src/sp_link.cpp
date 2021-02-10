// MIT License

// Copyright (c) 2016-2021 Luis Lloret

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <stdexcept>
#include <chrono>
#include <iostream>
#include <atomic>
#include <vector>

#include "ableton/Link.hpp"
#include "sp_link.h"
#include "version.h"
#include "monitorlogger.h"

using namespace std;

static int g_monitor_level = 6;
static atomic<bool> g_initialized{ false };
static ErlNifPid g_link_erlang_callback_pid;
static atomic<bool> g_callback_registered{ false };
static ableton::Link* g_link = nullptr;

// This auxiliary functions are used to relay the Link callbacks to an erlang process
int send_to_erlang_num_peers(int num_peers)
{
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM term1;
    ERL_NIF_TERM term2;
    ERL_NIF_TERM tuple;

    term1 = enif_make_atom(msg_env, "cb_num_peers");
    term2 = enif_make_int(msg_env, num_peers);
    tuple = enif_make_tuple2(msg_env, term1, term2);
    int rc = enif_send(NULL, &g_link_erlang_callback_pid, msg_env, tuple);
    enif_free_env(msg_env);
    return rc;
}

int send_to_erlang_tempo(double bpm)
{
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM term1;
    ERL_NIF_TERM term2;
    ERL_NIF_TERM tuple;

    term1 = enif_make_atom(msg_env, "cb_tempo");
    term2 = enif_make_double(msg_env, bpm);
    tuple = enif_make_tuple2(msg_env, term1, term2);
    int rc = enif_send(NULL, &g_link_erlang_callback_pid, msg_env, tuple);
    enif_free_env(msg_env);
    return rc;
}

int send_to_erlang_start_stop(bool is_playing)
{
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM term1;
    ERL_NIF_TERM term2;
    ERL_NIF_TERM tuple;

    term1 = enif_make_atom(msg_env, "cb_start_stop");
    term2 = enif_make_atom(msg_env, is_playing ? "true" : "false");
    tuple = enif_make_tuple2(msg_env, term1, term2);
    int rc = enif_send(NULL, &g_link_erlang_callback_pid, msg_env, tuple);
    enif_free_env(msg_env);
    return rc;
}


// The Link callbacks
std::function<void(size_t)> peer_count_callback = [](std::size_t num_peers) {
    if (g_callback_registered){
        send_to_erlang_num_peers(num_peers);
    }
};


std::function<void(double)> tempo_callback = [](double bpm) {
    if (g_callback_registered){
        send_to_erlang_tempo(bpm);
    }
};


std::function<void(bool)> start_stop_callback = [](bool is_playing) {
    if (g_callback_registered){
        send_to_erlang_start_stop(is_playing);
    }

};


// Need to pass the bpm, as Link requires it.
int sp_link_init(double bpm)
{
    if (g_initialized){
        return 0;
    }
    g_link = new ableton::Link(bpm);
    g_initialized = true;
    //MonitorLogger::getInstance().setLogLevel(g_monitor_level);

    g_link->setNumPeersCallback(peer_count_callback);
    g_link->setStartStopCallback(start_stop_callback);
    g_link->setTempoCallback(tempo_callback);

    return 0;
}

void sp_link_deinit()
{
    if (!g_initialized){
        return;
    }
    delete g_link;
    g_initialized = false;
}


int sp_link_enable(bool enable)
{
    if (!g_initialized){
        return -1;
    }

    g_link->enable(enable);
    return 0;
}


int sp_link_is_enabled(bool* enabled)
{
    if (!g_initialized){
        return -1;
    }

    *enabled = g_link->isEnabled() ? 1 : 0;
    return 0;
}


int sp_link_set_tempo(double bpm, ErlNifSInt64 micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.setTempo(bpm, std::chrono::microseconds(micros));
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_get_tempo(double* bpm)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *bpm = state.tempo();
    return 0;
}


int sp_link_set_is_playing(bool is_playing, ErlNifSInt64 micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.setIsPlaying(is_playing, std::chrono::microseconds(micros));
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_is_playing(bool* is_playing)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *is_playing = state.isPlaying();
    return 0;
}


int sp_link_get_time_for_is_playing(ErlNifSInt64* micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *micros = state.timeForIsPlaying().count();
    return 0;
}


int sp_link_get_num_peers(int* num_peers)
{
    if (!g_initialized){
        return -1;
    }

    *num_peers = static_cast<int>(g_link->numPeers());
    return 0;
}


int sp_link_start_stop_sync_enable(bool enable)
{
    if (!g_initialized){
        return -1;
    }

    g_link->enableStartStopSync(enable);
    return 0;
}


int sp_link_is_start_stop_sync_enabled(bool* enabled)
{
    if (!g_initialized){
        return -1;
    }

    *enabled = g_link->isStartStopSyncEnabled();
    return 0;
}


int sp_link_get_beat_at_time(ErlNifSInt64 micros, double quantum, double* beat)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *beat = state.beatAtTime(std::chrono::microseconds(micros), quantum);
    return 0;
}


int sp_link_get_phase_at_time(ErlNifSInt64 micros, double quantum, double* beat)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *beat = state.phaseAtTime(std::chrono::microseconds(micros), quantum);
    return 0;
}


int sp_link_get_time_at_beat(double beat, double quantum, ErlNifSInt64* micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *micros = state.timeAtBeat(beat, quantum).count();
    return 0;
}


int sp_link_request_beat_at_time(double beat, ErlNifSInt64 micros, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.requestBeatAtTime(beat, std::chrono::microseconds(micros), quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_force_beat_at_time(double beat, ErlNifSInt64 micros, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.forceBeatAtTime(beat, std::chrono::microseconds(micros), quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_request_beat_at_start_playing_time(double beat, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.requestBeatAtStartPlayingTime(beat, quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_set_is_playing_and_request_beat_at_time(bool is_playing, ErlNifSInt64 micros, double beat, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.setIsPlayingAndRequestBeatAtTime(is_playing, std::chrono::microseconds(micros), beat, quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


// TODO: Keep in mind that different C++ clocks might have different values. Make sure that this is consistent with other clocks (need to test)
int sp_link_get_current_time_microseconds(ErlNifSInt64* micros)
{
    if (!g_initialized){
        return -1;
    }

    *micros = g_link->clock().micros().count();
    return 0;
}




// NIF functions
ERL_NIF_TERM sp_link_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double bpm;
    int rc = enif_get_double(env, argv[0], &bpm);
    if (!rc){
        return enif_make_badarg(env);
    }
    rc = sp_link_init(bpm);
    return enif_make_atom(env, (rc == 0 ? "ok" : "error"));
}

ERL_NIF_TERM sp_link_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sp_link_deinit();
    return enif_make_atom(env, "ok");
}


ERL_NIF_TERM sp_link_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        return enif_make_badarg(env);
    }
    bool enable = false;

    if (strcmp(atom, "true") == 0){
        enable = true;
    }
    rc = sp_link_enable(enable);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_is_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool enabled;
    int rc = sp_link_is_enabled(&enabled);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_atom(env, enabled ? "true" : "false");
}


ERL_NIF_TERM sp_link_set_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double bpm;
    ErlNifSInt64 micros;
    int rc = enif_get_double(env, argv[0], &bpm);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_atom(env, "error");
    }

    rc = sp_link_set_tempo(bpm, 0);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_get_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double bpm;
    int rc = sp_link_get_tempo(&bpm);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_double(env, bpm);
}


ERL_NIF_TERM sp_link_set_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    ErlNifSInt64 micros;
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }
    bool is_playing = false;

    if (strcmp(atom, "true") == 0){
        is_playing = true;
    }
    rc = sp_link_set_is_playing(is_playing, micros);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool is_playing;
    int rc = sp_link_is_playing(&is_playing);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_atom(env, is_playing ? "true" : "false");
}


ERL_NIF_TERM sp_link_get_time_for_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    int rc = sp_link_get_time_for_is_playing(&micros);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_int64(env, micros);
}


ERL_NIF_TERM sp_link_get_num_peers_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int num_peers;
    int rc = sp_link_get_num_peers(&num_peers);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_int(env, num_peers);
}


ERL_NIF_TERM sp_link_start_stop_sync_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        return enif_make_badarg(env);
    }
    bool enable = false;

    if (strcmp(atom, "true") == 0){
        enable = true;
    }
    rc = sp_link_start_stop_sync_enable(enable);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");

}

ERL_NIF_TERM sp_link_is_start_stop_sync_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool enabled;
    int rc = sp_link_is_start_stop_sync_enabled(&enabled);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_atom(env, enabled ? "true" : "false");
}

ERL_NIF_TERM sp_link_get_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_int64(env, argv[0], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    double beat;
    rc = sp_link_get_beat_at_time(micros, quantum, &beat);
    if (rc == 0){
        return enif_make_double(env, beat);
    }
    else{
        return enif_make_atom(env, "error");
    }
}


ERL_NIF_TERM sp_link_get_phase_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_int64(env, argv[0], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    double phase;
    rc = sp_link_get_phase_at_time(micros, quantum, &phase);
    if (rc == 0){
        return enif_make_double(env, phase);
    }
    else{
        return enif_make_atom(env, "error");
    }
}


ERL_NIF_TERM sp_link_get_time_at_beat_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    ErlNifSInt64 micros;
    rc = sp_link_get_time_at_beat(beat, quantum, &micros);
    if (rc == 0){
        return enif_make_int64(env, micros);
    }
    else{
        return enif_make_atom(env, "error");
    }

}


ERL_NIF_TERM sp_link_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[2], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_request_beat_at_time(beat, micros, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_force_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[2], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_force_beat_at_time(beat, micros, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_request_beat_at_start_playing_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_request_beat_at_start_playing_time(beat, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_set_is_playing_and_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    bool is_playing = false;
    ErlNifSInt64 micros;
    double beat;
    double quantum;
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        enif_make_badarg(env);
    }

    if (strcmp(atom, "true") == 0){
        is_playing = true;
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[2], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[3], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_set_is_playing_and_request_beat_at_time(is_playing, micros, beat, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_set_link_callback_pid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_pid(env, argv[0])){
        return enif_make_badarg(env);
    }

    int rc = enif_get_local_pid(env, argv[0], &g_link_erlang_callback_pid);
    if (rc){
        g_callback_registered = true;
    }
    return enif_make_atom(env, (rc ? "ok" : "error"));
}



ERL_NIF_TERM sp_link_set_log_level_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int rc = enif_get_int(env, argv[0], &g_monitor_level);
    if (!rc){
        enif_make_badarg(env);
    }
    MonitorLogger::getInstance().setLogLevel(g_monitor_level);
    return enif_make_atom(env, (rc ? "ok" : "error"));
}


ERL_NIF_TERM sp_link_get_current_time_microseconds_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    int rc = sp_link_get_current_time_microseconds(&micros);
    if (rc == 0){
        return enif_make_int64(env, micros);
    }
    else{
        return enif_make_atom(env, "error");
    }
}


static ErlNifFunc nif_funcs[] = {
    {"link_init", 1, sp_link_init_nif},
    {"link_deinit", 0, sp_link_deinit_nif},
    {"link_enable", 1, sp_link_enable_nif},
    {"link_is_enabled", 0, sp_link_is_enabled_nif},
    {"link_set_tempo", 2, sp_link_set_tempo_nif},
    {"link_get_tempo", 0, sp_link_get_tempo_nif},
    {"link_set_is_playing", 2, sp_link_set_is_playing_nif},
    {"link_is_playing", 0, sp_link_is_playing_nif},
    {"link_get_time_for_is_playing", 0, sp_link_get_time_for_is_playing_nif},
    {"link_get_num_peers", 0, sp_link_get_num_peers_nif},
    {"link_start_stop_sync_enable", 1, sp_link_start_stop_sync_enable_nif},
    {"link_is_start_stop_sync_enabled", 0, sp_link_is_start_stop_sync_enabled_nif},
    {"link_get_beat_at_time", 2, sp_link_get_beat_at_time_nif},
    {"link_get_phase_at_time", 2, sp_link_get_phase_at_time_nif},
    {"link_get_time_at_beat", 2, sp_link_get_time_at_beat_nif},
    {"link_request_beat_at_time", 3, sp_link_request_beat_at_time_nif},
    {"link_force_beat_at_time", 3, sp_link_force_beat_at_time_nif},
    {"link_request_beat_at_start_playing_time", 2, sp_link_request_beat_at_start_playing_time_nif},
    {"link_set_is_playing_and_request_beat_at_time", 4, sp_link_set_is_playing_and_request_beat_at_time_nif},
    {"link_set_link_callback_pid", 1, sp_link_set_link_callback_pid_nif},
    {"set_log_level", 1, sp_link_set_log_level_nif},
    {"link_get_current_time_microseconds", 0, sp_link_get_current_time_microseconds_nif}
};

ERL_NIF_INIT(sp_link, nif_funcs, NULL, NULL, NULL, NULL);
