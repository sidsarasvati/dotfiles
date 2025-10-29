#
# atlas - Launch Claude Code with workflow commands
#
# Smart launcher for Claude Code that handles both main atlas
# wake and project-specific wakes.
#
# The main atlas wake requires navigating to the secondbrain-logseq
# repository, while project wakes use /wake-project which handles
# navigation automatically.
#
# Usage:
#   atlas                 # cd to secondbrain, run /wake (main atlas)
#   atlas multiplex       # Run /wake-project multiplex
#   atlas rai-b2b         # Run /wake-project rai-b2b
#
function atlas() {
  if [ $# -eq 0 ]; then
    # No arguments: cd to secondbrain and invoke /wake for main atlas
    cd ~/Code/sid/secondbrain-logseq && ~/.local/bin/claude "/wake"
  else
    # With argument: invoke /wake-project with project name
    # (wake-project handles navigation automatically)
    ~/.local/bin/claude "/wake-project $1"
  fi
}
