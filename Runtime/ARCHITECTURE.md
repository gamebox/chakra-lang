# Chakra Runtime Architecture

## Root Process

The root process creates a child process on every available core, or to a number of cores specified by the `--num_cores` flag, whichever is less.  These child processes will each be given a data structure they can use to talk to the root process and other siblings.  Once all cores are created, it assigns them each to a separate core.

The first `Command` that is run, is to create the Main actor.

After that command is dispatched, the root process will endlessly loop and wait on messages it receives from child processes, or on data it receives from other open pipes(open files, stdin, network sockets, etc).

## Main Actor

The main actor will run on the first child, if there is one, by first calling `init`, assigning it's state to a state heap variable(as a void pointer), and then running any `Command`s it returns in sequence. This will be the catalyst for everything that happens in the running life of the program.  If no subsequent actors are created and there is nothing queued in the system to be returned to the main actor(on a timeout, or an open file handle), the program will exit.

## Cross-process communication

A pipe is setup between all pids for communication, and they are given a `channel_t` struct that they can read `Envelope`s (`msg_t`s wrapped up with an identifier for the recipient Actor) from, and send `Envelopes`s to.  This struct contains a single read File Descriptor, and a write File Descriptor for each other child process.

## Child Processes

Ch