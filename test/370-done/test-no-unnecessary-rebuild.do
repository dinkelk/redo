# Test: After redo-done, a dependent target should NOT be rebuilt
# if the redo-done target hasn't actually changed.
#
# This tests the scenario where:
# 1. Build everything (redo-done target + redo-ifchange final)
# 2. Call redo-done again on the UNCHANGED target, then check final
#    — final should still be up-to-date because target.txt didn't change.

# Setup source
echo "v1" > source.txt

# Build target.txt externally and register it
echo "externally built" > target.txt
redo-done target.txt source.txt

# Build final.txt which depends on target.txt
redo final.txt

# Flush the session cache to simulate a new session
../flush-cache

# Call redo-done again on the UNCHANGED target.txt
# (simulates an external build tool that found nothing to rebuild)
redo-done target.txt source.txt

# final.txt should still be up to date since target.txt didn't change
redo-ifchange final.txt
CONTENT="$(cat final.txt)"
if [ "$CONTENT" != "final built from target" ]; then
    echo "FAIL: final.txt was unnecessarily rebuilt after unchanged redo-done! Content: $CONTENT" >&2
    exit 1
fi

echo "PASS: test-no-unnecessary-rebuild" >&2
