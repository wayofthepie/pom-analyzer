A library for the analysis of maven projects.

To parse a pom:

```
liftM head (( findPomsIn $ "example") >>= \fs -> sequence . parsePoms $ fs)
```

...
