This test requires a locking mechanism, since it is circular.

We try to redo "c", which redo-ichanges a bunch of things, which
finally redo-ifchange on "c". The second redo-ifchange should wait
for the first redo "c" to terminate instead of forking another process.

We can skip this test for now and re-add it when we start to support
parallel builds.
