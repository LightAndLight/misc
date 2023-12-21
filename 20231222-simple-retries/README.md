# Simple retries

*2023-12-22*

My version of <https://matklad.github.io/2023/12/21/retry-loop.html>:

> You want to retry some action. The action either succeeds or fails. Some, but not all, failures are transient and can be retried after a timeout. If a failure persists after a bounded number of retries, it should be propagated.
>
> The runtime sequence of event we want to see is:
>
> ```
> action()
> sleep()
> action()
> sleep()
> action()
> ```
>
> It has that mightily annoying a-loop-and-a-half shape.
>
> Here’s the set of properties I would like to see in a solution:
>
> 1. No useless sleep. A naive loop would sleep one extra time before reporting a retry failure, but we don’t want to do that.
> 2. In the event of a retry failure, the underlying error is reported. I don’t want to see just that all attempts failed, I want to see an actual error from the last attempt.
> 3. Obvious upper bound: I don’t want to write a while (true) loop with a break in the middle. If I am to do at most 5 attempts, I want to see a for (0..5) loop. Don’t ask me why.
> 4. No syntactic redundancy — there should be a single call to action and a single sleep in the source code.
>
> [My] solution achieves 1-3, fails at 4, and relies on a somewhat esoteric language feature — for/else.

My solution *also* doesn't achieve 4, but I think that's due to the nature of the problem. I think
of the task as an initial attempt followed by zero or more delayed attempts.