An Actor in Chakra in **defined** as a struct with a function,
conventionally called \`init\`, that received some arbitrary arguments
and returns a tuple with the first element being its initial state and
the second element being a list of \\\~Command\\\~s that should be
runtime once the new Actor has been created. This struct may also have
any number of other functions defined that take as their first argument
the current state of the Actor, and may take a number of additional
arguments.

An actor is created as such

\`\`\` chakra spawn(definition, return-message) spawn1(definition,
return-message, arg) spawn2(definition, return-message, arg1, arg2)
spawn3(definition, return-message, arg1, arg2, arg3) spawn4(definition,
return-message, arg1, arg2, arg3, arg4) spawn5(definition,
return-message, arg1, arg2, arg3, arg4, arg5) \`\`\`

These functions return a \`Command\`, and the new actor will have the
function passed as the \`return-message\` argument called when the new
Actor is spawned. This function must take have the signature \`(?state,
Ref) -\> (?state, \[Command\])\` and it will be passed the reference to
the created actor.

Once you have a Ref to an actor, sending messages is done as shown
below:

\`\`\` chakra send(Ref, message) ; (Ref, (?state) -\> (?state,
\[Command\])) -\> Command send1(Ref, message, arg) ; (Ref, (?state, ?a)
-\> (?state, \[Command\]), ?a) -\> Command send2(Ref, message, arg1,
arg2) ; (Ref, (?state, ?a, ?b) -\> (?state, \[Command\]), ?a, ?b) -\>
Command send3(Ref, message, arg1, arg2, arg3) ; (Ref, (?state, ?a, ?b,
?c) -\> (?state, \[Command\]), ?a, ?b, ?c) -\> Command send4(Ref,
message, arg1, arg2, arg3, arg4) ; (Ref, (?state, ?a, ?b, ?c, ?d) -\>
(?state, \[Command\]), ?a, ?b, ?c, ?d) -\> Command send5(Ref, message,
arg1, arg2, arg3, arg4, arg5) ; (Ref, (?state, ?a, ?b, ?c, ?d, ?e) -\>
(?state, \[Command\]), ?a, ?b, ?c, ?d, ?e) -\> Command \`\`\`

These functions also return a \`Command\`, and therefore must be
returned from the body of an actor definition\'s \`init\` or another
**message functions** (which is defined as a function that is part of an
actor definition struct and has the signature \`(?state, \[...args\])
-\> (?state, \[Command\])\`) in order for it to be performed. Not that
\`message\` must be a function that is part of the actor definition
struct that defined the Ref\'s actor, and that message functions can
only take between 0 and 5 arguments.
