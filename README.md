A library for the analysis of maven projects.

To parse a pom:

```haskell
findPomsIn "example" >>=  sequence . parsePoms
```

...
