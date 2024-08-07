# Profiling

```
# compile for profiling
stack install --profile

# memory
locest ... +RTS -hc -l
eventlog2html locest.eventlog

# runtime
locest ... +RTS -p
profiteur locest.prof
```