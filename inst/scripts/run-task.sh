#!/usr/bin/env bash
#
# Run one cs9 task detached, and record how it ended.
#
# This is the non-interactive counterpart to
# run_task_sequentially_as_callr_bg_using_load_all(). Use that one in Positron,
# where a person watches a live console. Use this one when nobody is watching:
# a script, a cron entry, an agent driving a machine over SSH.
#
# The difference is not style. TaskJob passes supervise = TRUE, so callr kills
# the task when the parent R process exits, and it streams output through
# later::later(), which needs an event loop that Rscript does not drive. Both
# properties are right for a live console and wrong for a session that starts
# the work and lets go of it.
#
# USAGE
#   run-task.sh [options] <task_name>
#
#     -d, --dir <path>     implementation package directory. Default ".".
#     -s, --ss <expr>      R expression naming the surveillance system in the
#                          child process. Default "global$ss".
#     -r, --runs <path>    where to write the log, status and lock files.
#                          Default "$CS9_RUN_DIR", else "$HOME/.cs9/task-runs".
#     -h, --help           print this usage and exit.
#
# THE CONTRACT
#   The script prints four lines and returns at once:
#
#     TASK=<name>
#     LOG=<path>        every line of stdout and stderr, provenance header first
#     STATUS=<path>     written ONLY when the task ends. Holds the exit code.
#     PID=<pid>
#
#   Wait on STATUS, never on the log. A log cannot separate "finished cleanly"
#   from "died at line 4", so a caller reading the tail of a log is guessing:
#
#     until [ -f "$STATUS" ]; do sleep 30; done; cat "$STATUS"
#
#   It fails in the safe direction. No file means still running, never "done".
#
#   If the process is killed hard, no status is ever written. The lock still
#   holds the pid, so `kill -0 $(cat <lock>)` separates "still running" from
#   "died without reporting".
#
# EXIT CODES OF THIS SCRIPT, which are not the task's
#   0  the task started. Read STATUS for how it ended.
#   2  usage error, or no R package at --dir.
#   3  refused: that task is already running from that package.
#
# The task inherits the environment, so bound a development run the usual way
# for your implementation package.

set -u

usage() {
  sed -n '2,/^set -u/p' "$0" | sed 's/^# \{0,1\}//; $d'
}

PKG="."
SS_PREFIX='global$ss'
RUN_DIR=${CS9_RUN_DIR:-$HOME/.cs9/task-runs}
TASK=""

while [ $# -gt 0 ]; do
  case "$1" in
    -d|--dir)  PKG=${2:-}; shift 2 ;;
    -s|--ss)   SS_PREFIX=${2:-}; shift 2 ;;
    -r|--runs) RUN_DIR=${2:-}; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    -*)        echo "unknown option: $1" >&2; exit 2 ;;
    *)         if [ -n "$TASK" ]; then echo "unexpected argument: $1" >&2; exit 2; fi
               TASK=$1; shift ;;
  esac
done

if [ -z "$TASK" ]; then
  echo "usage: run-task.sh [options] <task_name>   (run-task.sh --help)" >&2
  exit 2
fi

# A DESCRIPTION is what makes the directory an R package. Check it rather than
# the directory alone, so a wrong --dir fails here and not inside R.
if [ ! -f "$PKG/DESCRIPTION" ]; then
  echo "no R package at $PKG (no DESCRIPTION)" >&2
  exit 2
fi

PKG_ABS=$(cd "$PKG" && pwd)
PKG_NAME=$(sed -n 's/^Package: *//p' "$PKG/DESCRIPTION" | head -1 | tr -d '\r')
PKG_VER=$(sed -n 's/^Version: *//p' "$PKG/DESCRIPTION" | head -1 | tr -d '\r')

mkdir -p "$RUN_DIR"

# The lock is per package AND per task. Two implementation packages may define
# the same task name, and they are different runs.
LOCK="$RUN_DIR/${PKG_NAME}_${TASK}.running"
if [ -f "$LOCK" ]; then
  RUNNING_PID=$(cat "$LOCK" 2>/dev/null || echo "")
  if [ -n "$RUNNING_PID" ] && kill -0 "$RUNNING_PID" 2>/dev/null; then
    echo "REFUSED: $PKG_NAME task $TASK is already running as pid $RUNNING_PID" >&2
    exit 3
  fi
  rm -f "$LOCK"
fi

TS=$(date +%Y%m%d_%H%M%S)
BASE="$RUN_DIR/${PKG_NAME}_${TASK}_${TS}"
LOG="$BASE.log"
STATUS="$BASE.status"
DRIVER="$BASE.R"

# Which code ran. A log that reports only a duration cannot answer that later,
# and an implementation package copied out of its checkout has no commit to
# read afterwards. Record it now, while the answer is still available.
GIT_DESC="not a git repository"
if git -C "$PKG_ABS" rev-parse --git-dir > /dev/null 2>&1; then
  GIT_SHA=$(git -C "$PKG_ABS" rev-parse --short HEAD 2>/dev/null || echo unknown)
  GIT_BRANCH=$(git -C "$PKG_ABS" rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)
  if [ -n "$(git -C "$PKG_ABS" status --porcelain 2>/dev/null)" ]; then
    GIT_DESC="$GIT_BRANCH $GIT_SHA (uncommitted changes present)"
  else
    GIT_DESC="$GIT_BRANCH $GIT_SHA (clean tree)"
  fi
fi

{
  echo "task:     $TASK"
  echo "package:  $PKG_NAME $PKG_VER at $PKG_ABS"
  echo "source:   $GIT_DESC"
  echo "system:   $SS_PREFIX"
  echo "started:  $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "host:     $(hostname)"
  echo "---"
} > "$LOG"

cat > "$DRIVER" <<EOF
setwd("$PKG_ABS")
cat("cs9:     ", as.character(utils::packageVersion("cs9")), "\n", sep = "")
devtools::load_all("$PKG_ABS")
$SS_PREFIX\$run_task("$TASK")
EOF

# setsid detaches the worker into its own session, so closing the calling shell
# does not take it down. The worker writes its OWN pid into the lock: setsid can
# fork again, so \$! in this shell is not reliably the process doing the work.
# Positional arguments carry the paths, which keeps one level of quoting out.
setsid bash -c '
  echo $$ > "$4"
  Rscript "$1" >> "$2" 2>&1
  echo $? > "$3"
  rm -f "$4"
' _ "$DRIVER" "$LOG" "$STATUS" "$LOCK" < /dev/null > /dev/null 2>&1 &

for _ in 1 2 3 4 5 6 7 8 9 10; do
  [ -f "$LOCK" ] && break
  sleep 0.2
done

echo "TASK=$TASK"
echo "LOG=$LOG"
echo "STATUS=$STATUS"
echo "PID=$(cat "$LOCK" 2>/dev/null || echo unknown)"
