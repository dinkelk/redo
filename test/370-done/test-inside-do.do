# Test: redo-done works when called from inside a .do script

echo "v1" > source.txt
redo wrapper.txt

# wrapper.txt should exist
if [ ! -e wrapper.txt ]; then
    echo "FAIL: wrapper.txt was not created" >&2
    exit 1
fi

# target.txt should have been marked done by the wrapper
CONTENT="$(cat target.txt)"
if [ "$CONTENT" != "sub content" ]; then
    echo "FAIL: target.txt content wrong: $CONTENT" >&2
    exit 1
fi

# target.txt should be up-to-date now
../flush-cache
redo-ifchange target.txt
CONTENT="$(cat target.txt)"
if [ "$CONTENT" != "sub content" ]; then
    echo "FAIL: target.txt was rebuilt after redo-done! Content: $CONTENT" >&2
    exit 1
fi

echo "PASS: test-inside-do" >&2
