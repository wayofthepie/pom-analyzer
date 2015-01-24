A library for the analysis of maven projects.

To parse a pom:

```haskell
liftM head $ ( findPomsIn $ "example") >>=  sequence . parsePoms
```

...
