# Profiling

```
# memory
stack install --profile
stack exec --profile -- locest ... +RTS -hc -l
eventlog2html locest.eventlog

# runtime
stack exec --profile -- locest ... +RTS -p
profiteur locest.prof
```