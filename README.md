fast
====

This library aims to provide the fastest possible implementation of some every day routines.

The contained functions avoid GC allocations and input validation. They may use SSE or stack allocations to reach a high throughput so that in some cases a 20 fold speed increase can be achieved.