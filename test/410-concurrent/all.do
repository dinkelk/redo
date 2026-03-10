exec >&2
. ../skip-if-minimal-do.sh

# Test 1: Two separate redo invocations building overlapping targets in parallel
echo "Test 1: concurrent redo invocations on overlapping targets"
rm -f result_a result_b shared
redo result_a &
pid_a=$!
redo result_b &
pid_b=$!
set +e
wait $pid_a; rc_a=$?
wait $pid_b; rc_b=$?
set -e
if [ "$rc_a" -ne 0 ]; then echo "FAIL: redo result_a exited with $rc_a" >&2; exit 1; fi
if [ "$rc_b" -ne 0 ]; then echo "FAIL: redo result_b exited with $rc_b" >&2; exit 2; fi
[ -e result_a ] || { echo "FAIL: result_a not created" >&2; exit 3; }
[ -e result_b ] || { echo "FAIL: result_b not created" >&2; exit 4; }
[ -e shared ]   || { echo "FAIL: shared not created" >&2; exit 5; }
[ "$(cat result_a)" = "result_a" ] || { echo "FAIL: result_a wrong content" >&2; exit 6; }
[ "$(cat result_b)" = "result_b" ] || { echo "FAIL: result_b wrong content" >&2; exit 7; }
[ "$(cat shared)" = "shared" ]     || { echo "FAIL: shared wrong content" >&2; exit 8; }
echo "  PASS"

# Test 2: Rebuild after concurrent access verifies DB consistency
echo "Test 2: rebuild after concurrent access (DB consistency)"
rm -f result_a result_b shared
redo result_a result_b
[ "$(cat result_a)" = "result_a" ] || { echo "FAIL: rebuild result_a wrong" >&2; exit 9; }
[ "$(cat result_b)" = "result_b" ] || { echo "FAIL: rebuild result_b wrong" >&2; exit 10; }
[ "$(cat shared)" = "shared" ]     || { echo "FAIL: rebuild shared wrong" >&2; exit 11; }
echo "  PASS"

# Test 3: Stress test - 8 concurrent builds of same target
echo "Test 3: stress test - 8 concurrent builds of same target"
for i in 1 2 3 4 5 6 7 8; do
    rm -f stress
    redo stress &
done
set +e; wait; set -e
[ -e stress ] || { echo "FAIL: stress target not created" >&2; exit 12; }
[ "$(cat stress)" = "stress" ] || { echo "FAIL: stress wrong content" >&2; exit 13; }
echo "  PASS"

# Test 4: Concurrent builds with deep dependency chains
echo "Test 4: concurrent builds with deep dependency chains"
rm -f chain_a chain_b mid_a mid_b deep
redo chain_a &
pid_a=$!
redo chain_b &
pid_b=$!
set +e
wait $pid_a; rc_a=$?
wait $pid_b; rc_b=$?
set -e
if [ "$rc_a" -ne 0 ]; then echo "FAIL: chain_a exited with $rc_a" >&2; exit 14; fi
if [ "$rc_b" -ne 0 ]; then echo "FAIL: chain_b exited with $rc_b" >&2; exit 15; fi
[ "$(cat chain_a)" = "chain_a" ] || { echo "FAIL: chain_a wrong" >&2; exit 16; }
[ "$(cat chain_b)" = "chain_b" ] || { echo "FAIL: chain_b wrong" >&2; exit 17; }
[ "$(cat deep)" = "deep" ]       || { echo "FAIL: deep wrong" >&2; exit 18; }
echo "  PASS"

# Test 5: Rapid sequential rebuilds after concurrent access
# Ensures DB state is fully consistent after contention
echo "Test 5: rapid sequential rebuilds after concurrent stress"
for i in 1 2 3 4 5; do
    rm -f result_a result_b shared
    redo result_a result_b || { echo "FAIL: sequential rebuild $i failed" >&2; exit 19; }
done
[ "$(cat result_a)" = "result_a" ] || { echo "FAIL: final result_a wrong" >&2; exit 20; }
[ "$(cat result_b)" = "result_b" ] || { echo "FAIL: final result_b wrong" >&2; exit 21; }
echo "  PASS"
