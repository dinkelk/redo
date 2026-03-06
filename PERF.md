# Redo Performance Optimization Tracking

## Benchmark

**Target:** `command_router test_all` clean build in adamant (perf/36 Python branch)
**Machine:** Docker container, 8-core x86_64, 16GB RAM
**Procedure:** Clear `~/.redo`, `/tmp/redo-*`, `/tmp/tmp.*`, all `build/` dirs, clear `__pycache__`, recompile `.pyc`, then `time redo test_all`

## Baseline

**redo version:** commit e11d2ea (master) + redo-done + race fix (feature/redo-done, commit 1084159)
**Result:** ~19.3s (3 runs: 19.3s, 19.1s, 19.5s)

## Architecture Notes

redo's "database" is filesystem-based:
- Each target gets an MD5-hashed directory under `~/.redo/database/`
- Dependencies stored as directory entries (not files)
- Every DB operation (`doesDatabaseExist`, `isBuilt`, `getStamp`, etc.) acquires a file lock
- Session cache (`isBuilt`/`isClean`/`isDirty`) also stored as filesystem directories with locks
- `findDoFile` walks up directory tree checking for `.do` files
- `getKey` computes MD5 hash of target path on every call

---

## Optimization Branches

(Results will be added as experiments are run)
