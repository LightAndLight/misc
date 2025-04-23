---- MODULE duplicates ----
EXTENDS Integers, Sequences, TLC

S == (1..10)

(* --algorithm dup
variables
    seq \in S \X S \X S \X S;
    seen = {};
    index = 1;
    no_duplicates = TRUE;

fair process main = 0
begin
    Start:
    while index <= Len(seq) do
        if seq[index] \in seen
        then
            no_duplicates := FALSE;
        else
            seen := {seq[index]} \union seen;
        end if;
        index := index + 1; \* comment out this line to observe a termination failure
    end while;
end process;
end algorithm;
*)
\* BEGIN TRANSLATION (chksum(pcal) = "b750049c" /\ chksum(tla) = "a0944505")
VARIABLES seq, seen, index, no_duplicates, pc

vars == << seq, seen, index, no_duplicates, pc >>

ProcSet == {0}

Init == (* Global variables *)
        /\ seq \in S \X S \X S \X S
        /\ seen = {}
        /\ index = 1
        /\ no_duplicates = TRUE
        /\ pc = [self \in ProcSet |-> "Start"]

Start == /\ pc[0] = "Start"
         /\ IF index <= Len(seq)
               THEN /\ IF seq[index] \in seen
                          THEN /\ no_duplicates' = FALSE
                               /\ seen' = seen
                          ELSE /\ seen' = ({seq[index]} \union seen)
                               /\ UNCHANGED no_duplicates
                    /\ index' = index + 1
                    /\ pc' = [pc EXCEPT ![0] = "Start"]
               ELSE /\ pc' = [pc EXCEPT ![0] = "Done"]
                    /\ UNCHANGED << seen, index, no_duplicates >>
         /\ seq' = seq

main == Start

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == main
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(main)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 

====
