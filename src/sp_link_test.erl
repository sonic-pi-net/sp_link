-module(sp_link_test).
-export([start/0, link_process/0, test_callbacks/0, test_tempo_changes_from_erlang/0]).


link_process() ->
    receive
        {cb_num_peers, Peers} when is_integer(Peers) ->
            io:format("Received cb_num_peers message -> ~p peers~n", [Peers]);
        {cb_tempo, Tempo} when is_float(Tempo) ->
            io:format("Received cb_tempo message -> ~p bpm~n", [Tempo]);
        {cb_start_stop, Start} when is_atom(Start) ->
            io:format("Received cb_start_stop message-> ~p~n", [Start]);
        X ->
            io:format("Received something (not what was expected)->~p~n", [X])

    end,
    link_process().

test_callbacks() ->
    io:fwrite("Spawning and setting up Erlang Link callback process~n"),
    Pid = spawn(sp_link_test, link_process, []),
    sp_link:link_set_link_callback_pid(Pid),

    io:fwrite("Now go into Ableton Live (or other Link enabled SW or device) and change tempo, settings, play / stop (make sure that Link and Start / stop sync are enabled there)~n"),
    io:fwrite("You should see the callbacks triggering~n"),

    io:fwrite("Waiting 20 seconds for callbacks~n"),
    timer:sleep(20000).


% Make sure the tempo is passed as a float. Otherwise, badarg
test_tempo_changes_from_erlang() ->
    sp_link:link_set_tempo(99.0, 0),
    timer:sleep(2000),
    sp_link:link_set_tempo(212.0, 0),
    timer:sleep(2000),
    sp_link:link_set_tempo(67.5, 0),
    timer:sleep(2000),
    sp_link:link_set_tempo(20.0, 0),
    timer:sleep(2000),
    sp_link:link_set_tempo(500.99, 0),
    timer:sleep(2000),
    
    GetTempo = sp_link:link_get_tempo(),
    io:fwrite("Getting the tempo to check. Got ~p~n", [GetTempo]).
    

start() ->
%    cd("d:/projects/sp_link/src").
    compile:file(sp_link),

    io:fwrite("Init and enabling Link~n"),
    sp_link:link_init(120.0),
    sp_link:link_enable(true),

    io:fwrite("Enabling Link start / stop synchronization~n"),
    sp_link:link_start_stop_sync_enable(true),

    io:fwrite("Testing NIF function to return link clock in microseconds.~n"),
    Micros = sp_link:link_get_current_time_microseconds(),
    io:fwrite("Micros: ~p~n", [Micros]),
    
    io:fwrite("Testing callbacks (comment if you do not want to test them anymore)~n"),
    test_callbacks(),

    io:fwrite("Now we do some tempo changes and see if Ableton Link gets them~n"),
    test_tempo_changes_from_erlang(),
    
    sp_link:link_enable(false),
    sp_link:link_deinit().
