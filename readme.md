# `advent-of-code-2022`

Might as well.

```
npm install
npm run start
```

will build and run the latest solve.

:sparkles:

## Goals:

* Try to do everything as idiomatically as possible, other than "trusting the input a little too much". I know that I can 
trust the input to be well-formed, so I'm doing irresponsible stuff like parsing a list of `Strings` and then filter-mapping
from a `List (Result (List Parser.DeadEnd) a)` to a `List a`
* Don't touch the input; parse the input into a data structure, and go from there
* No external packages that aren't in the `elm/` namespace; I'm rebuilding any helper functions on my own in `src/Util.elm`
* Clean up solves over time to make them more readable or nicer, but without changing the fundamental structure of the code