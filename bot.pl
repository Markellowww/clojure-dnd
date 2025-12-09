#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).
:- use_module(library(thread)).

main :-
    sleep(5),
    client(localhost, 3333).

client(Host, Port) :-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream, []),
        (   thread_create(reader_thread(Stream), _, [detached(true)]),
            bot(Stream)
        ),
        close(Stream)
    ).

reader_thread(Stream) :-
    repeat,
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  true, !
    ;   format('Server: ~s~n', [Line]),
        fail
    ).

send_command(Stream, Command) :-
    format(Stream, '~s~n', [Command]),
    flush_output(Stream),
    sleep(1).

random_move(Stream) :-
    send_command(Stream, "look"),
    sleep(2),
    random_member(Dir, [north, south, east, west]),
    format(atom(MoveCmd), "move ~w", [Dir]),
    send_command(Stream, MoveCmd).

chat_message(Stream) :-
    random_member(Message, [
        "Hello World!",
        "helloworld(print)",
        "hmmmm",
        "Interesting place :)",
        "Drupal Coder the BEST!"
    ]),
    format(atom(ChatCmd), "say ~s", [Message]),
    send_command(Stream, ChatCmd).

bot(Stream) :-
    send_command(Stream, "Prolog Bot"),

    repeat,
        random_move(Stream),
        chat_message(Stream),
        sleep(5),
        fail.