fast
====

This library aims to provide the fastest possible implementation of some every day routines.

The contained functions avoid GC allocations and input validation. They may use SSE or stack allocations to reach a high throughput so that in some cases a 20 fold speed increase can be achieved.

#### Benchmark
A benchmark is included and can be run through dub, e.g.:

    dub --build=release --compiler=gdc

#### Examples
##### SSE3 accelerated splitting around '/' and '\'
```d
string rest = pathname
string element;

import fast.string;
while (rest.split!`or(=\,=/)`(element, rest))
{
    // `element' is now the next directory.
    // `rest' is what remains after the \ or /.
}
// `element` is now the file name part of the path.
```
##### Calling Windows API functions.
```d
void createHardlink(string from, string to)
{
    import fast.cstring : wcharPtr;
    CreateHardLinkW(wcharPtr!to, wcharPtr!from, null);
}
```
##### Calling Linux API functions.
```d
void createHardlink(string from, string to)
{
    import fast.cstring : charPtr;
    link(charPtr!from, charPtr!to);
}
```
