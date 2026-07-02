# Repository Instructions

- When an assert fails, fix the underlying bug. Do not remove or weaken the assert to make the failure go away.
- Use fail-fast semantics. Anything unexpected or unexplained should result in an error instead of being silently ignored, guessed around, or converted into a best-effort optional action.
- Raise configuration or authentication issues with the user before working around them. Do not silently bypass broken credentials, remotes, or local configuration.
- Surface ambiguous parser/analyzer issues before implementing a workaround. If a BGA log encoding, Keldon/BGA semantic mapping, version boundary, or replay mismatch requires inference rather than a direct mechanical fix, stop and explain the evidence, uncertainty, and proposed options before changing behavior.
- Code architecture should reflect how we think about the problem. Write code for humans first and the compiler/interpreter second: prefer explicit domain concepts, typed/stateful boundaries, and phase-oriented structure over stringly-typed control flow or incidental helper piles.
- When inspecting another branch, always compare it against its merge base with `origin/master`, not against current `master` directly. Use `git merge-base origin/master <branch>` and diff from that commit to the branch tip.
