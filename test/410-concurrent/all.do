exec >&2

echo
echo "  concurrent: same target from two processes"

# Test 1: Two processes build the same target simultaneously.
# The target takes ~1s to build. Both processes should produce correct output.
rm -f shared
redo shared &
PID1=$!
redo shared &
PID2=$!
FAIL=0
wait $PID1 || FAIL=1
wait $PID2 || FAIL=1
if [ "$FAIL" -ne 0 ]; then
    echo "FAIL: one or both concurrent builds of same target failed" >&2
    exit 1
fi
RESULT=$(cat shared)
if [ "$RESULT" != "shared-output" ]; then
    echo "FAIL: shared target corrupted: got '$RESULT'" >&2
    exit 1
fi
echo "  concurrent: same target OK"

echo "  concurrent: overlapping dependency chains"

# Test 2: Two chains that share a common deep dependency.
# chain_a -> mid_a -> deep
# chain_b -> mid_b -> deep
# Build both chains concurrently. Both should get correct results.
rm -f chain_a chain_b mid_a mid_b deep
redo chain_a &
PID1=$!
redo chain_b &
PID2=$!
FAIL=0
wait $PID1 || FAIL=1
wait $PID2 || FAIL=1
if [ "$FAIL" -ne 0 ]; then
    echo "FAIL: one or both dependency chain builds failed" >&2
    exit 1
fi
RESULT_A=$(cat chain_a)
RESULT_B=$(cat chain_b)
if [ "$RESULT_A" != "chain_a:mid_a:deep" ]; then
    echo "FAIL: chain_a corrupted: got '$RESULT_A'" >&2
    exit 1
fi
if [ "$RESULT_B" != "chain_b:mid_b:deep" ]; then
    echo "FAIL: chain_b corrupted: got '$RESULT_B'" >&2
    exit 1
fi
echo "  concurrent: overlapping chains OK"

echo "  concurrent: stress test (4 processes, shared targets)"

# Test 3: 4 processes all building the same set of targets.
# Verify no corruption after all complete.
rm -f stress
redo stress &
redo stress &
redo stress &
redo stress &
FAIL=0
wait || FAIL=1
if [ "$FAIL" -ne 0 ]; then
    echo "FAIL: stress test had failures" >&2
    exit 1
fi
RESULT=$(cat stress)
if [ "$RESULT" != "stress-output" ]; then
    echo "FAIL: stress target corrupted: got '$RESULT'" >&2
    exit 1
fi
echo "  concurrent: stress test OK"

echo "  concurrent: simultaneous builds of different targets with shared dep"

# Test 4: Multiple independent targets that all depend on one slow source.
# Build all concurrently. Verifies the DB handles multiple writers updating
# different targets' dep records at the same time.
rm -f result_a result_b result_c slow_dep
redo result_a &
PID1=$!
redo result_b &
PID2=$!
redo result_c &
PID3=$!
FAIL=0
wait $PID1 || FAIL=1
wait $PID2 || FAIL=1
wait $PID3 || FAIL=1
if [ "$FAIL" -ne 0 ]; then
    echo "FAIL: multi-target concurrent build had failures" >&2
    exit 1
fi
if [ "$(cat result_a)" != "a:slow_dep_val" ]; then
    echo "FAIL: result_a wrong: got '$(cat result_a)'" >&2
    exit 1
fi
if [ "$(cat result_b)" != "b:slow_dep_val" ]; then
    echo "FAIL: result_b wrong: got '$(cat result_b)'" >&2
    exit 1
fi
if [ "$(cat result_c)" != "c:slow_dep_val" ]; then
    echo "FAIL: result_c wrong: got '$(cat result_c)'" >&2
    exit 1
fi
echo "  concurrent: shared dep OK"

echo "  concurrent: rapid-fire rebuild (detect stale reads)"

# Test 5: Build a target, force-invalidate it by touching the .do file,
# then launch concurrent rebuilds. Without locking, one process may read
# the stale output while the other is mid-rebuild.
rm -f rapid
redo rapid
FIRST=$(cat rapid)
# Touch the .do file to force rebuild
sleep 1.1
touch rapid.do
redo rapid &
PID1=$!
redo rapid &
PID2=$!
FAIL=0
wait $PID1 || FAIL=1
wait $PID2 || FAIL=1
if [ "$FAIL" -ne 0 ]; then
    echo "FAIL: rapid-fire rebuild had failures" >&2
    exit 1
fi
RESULT=$(cat rapid)
if [ "$RESULT" != "rapid-output" ]; then
    echo "FAIL: rapid target corrupted: got '$RESULT'" >&2
    exit 1
fi
echo "  concurrent: rapid-fire rebuild OK"

echo "  concurrent: all tests passed"
