# herd-claude — launch several Claude Code instances at once inside a herdr session.
#
#   herd-claude [N] [--tabs|--worktrees|--grid] [--session NAME] [--no-attach] [DIR]
#
# Default: 3 Claude instances, one per herdr tab, in the current directory, then
# attach to the herdr TUI. Modes:
#   --tabs       one Claude per tab (default)
#   --worktrees  one Claude per isolated git worktree off the current repo
#   --grid       Claudes tiled as split panes in a single tab
# Override the launched command with HERD_CLAUDE_CMD (default: claude).
{
  writeShellScriptBin,
  lib,
  herdr,
  jq,
  git,
  coreutils,
  gnugrep,
}:

writeShellScriptBin "herd-claude" ''
  export PATH="${lib.makeBinPath [ herdr jq git coreutils gnugrep ]}:$PATH"

  N=3
  MODE=tabs
  DIR="$PWD"
  SESSION="claude"
  ATTACH=1
  CMD="''${HERD_CLAUDE_CMD:-claude}"

  while [ $# -gt 0 ]; do
    case "$1" in
      --tabs)      MODE=tabs ;;
      --worktrees) MODE=worktrees ;;
      --grid)      MODE=grid ;;
      --no-attach) ATTACH=0 ;;
      --session)   shift; SESSION="$1" ;;
      -h|--help)
        echo "usage: herd-claude [N] [--tabs|--worktrees|--grid] [--session NAME] [--no-attach] [DIR]"
        echo "  default: 3 tabs, one Claude each, in the current directory"
        exit 0 ;;
      [0-9]*)      N="$1" ;;
      *)           DIR="$1" ;;
    esac
    shift
  done

  hd() { herdr --session "$SESSION" "$@"; }
  pane_of() { jq -r '.result.root_pane.pane_id // empty'; }

  # Ensure the session's headless server is running.
  if ! hd status 2>/dev/null | grep -q 'status: running'; then
    setsid herdr --session "$SESSION" server >/dev/null 2>&1 </dev/null &
    for _ in $(seq 1 50); do
      hd status 2>/dev/null | grep -q 'status: running' && break
      sleep 0.1
    done
  fi

  run_claude() { # $1 = pane_id
    [ -n "$1" ] && hd pane run "$1" "$CMD" >/dev/null 2>&1
  }

  case "$MODE" in
    tabs|grid)
      first=$(hd workspace create --cwd "$DIR" --label claude | pane_of)
      run_claude "$first"
      last="$first"
      i=2
      while [ "$i" -le "$N" ]; do
        if [ "$MODE" = grid ]; then
          split=down; [ $((i % 2)) -eq 0 ] && split=right
          pid=$(hd pane split --pane "$last" --direction "$split" --cwd "$DIR" | pane_of)
        else
          pid=$(hd tab create --cwd "$DIR" --label "claude-$i" | pane_of)
        fi
        run_claude "$pid"
        last="$pid"
        i=$((i + 1))
      done
      ;;
    worktrees)
      if ! git -C "$DIR" rev-parse --git-dir >/dev/null 2>&1; then
        echo "herd-claude: --worktrees requires a git repo (DIR=$DIR)" >&2
        exit 1
      fi
      base=$(git -C "$DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || echo main)
      i=1
      while [ "$i" -le "$N" ]; do
        branch="claude/s$i"
        pid=$(hd worktree create --cwd "$DIR" --branch "$branch" --base "$base" --label "$branch" | pane_of)
        run_claude "$pid"
        i=$((i + 1))
      done
      ;;
  esac

  if [ "$ATTACH" = 1 ]; then
    exec herdr --session "$SESSION"
  else
    echo "herd-claude: started $N Claude pane(s) in session '$SESSION' (mode=$MODE)."
    echo "attach with:  herdr --session $SESSION"
  fi
''
