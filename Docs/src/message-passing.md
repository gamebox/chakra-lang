# Message Passing in Chakra

An Actor in Chakra in *defined* as a struct with a function,
conventionally called `init`, that received some arbitrary arguments and
returns a tuple with the first element being its initial state and the
second element being a list of \~Command\~s that should be runtime once
the new Actor has been created. This struct may also have any number of
other functions defined that take as their first argument the current
state of the Actor, and may take a number of additional arguments.

An actor is created as such

``` chakra
spawn(definition, return-message, arg)
```

This function returns a `Command`, and the new actor will have the
function passed as the `return-message` argument called when the new
Actor is spawned. This function must take have the signature `(
Ref) -> ?msg` and it will be passed the reference to the
created actor.

Once you have a Ref to an actor, sending messages is done as shown
below:

``` chakra
send(Ref, message) ; { (&<?msg>, ?msg) -> ! }
```

These functions also return a `Command`, and therefore must be returned
from the body of an actor definition's `init` or another *message
functions* (which is defined as a function that is part of an actor
definition struct and has the signature `(?state, [...args]) -> (?state,
[Command])`) in order for it to be performed. Not that `message` must be
a function that is part of the actor definition struct that defined the
Ref's actor, and that message functions can only take between 0 and 5
arguments.
