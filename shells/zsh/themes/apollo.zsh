zstyle ':apollo:apollo:core:modules:left' modules 'root_indicator' 'context' 'virtualenv' 'newline' 'dir' 'ruler'
zstyle ':apollo:apollo:core:modules:right' modules 'command_execution_time' 'status' 'newline' 'background_jobs' 'git' 'date' 'clock'

zstyle ':apollo:apollo:*' fg_color 1
zstyle ':apollo:apollo:*' verbose yes

zstyle ':apollo:apollo:*:*:(context|command_execution_time|background_jobs|date|clock|status|git):*' fg_color 15
zstyle ':apollo:apollo:*:*:(context|dir|command_execution_time|background_jobs|date|clock|status|git):*' style "bold"

zstyle ':apollo:apollo:*:*:(virtualenv|root_indicator|context|dir|git|):*' fg_color 15

zstyle ':apollo:apollo:*:*:(command_execution_time|background_jobs|date|clock|status):*:surround:(left|right)' fg_color 4
zstyle ':apollo:apollo:*:*:(virtualenv|quota|public_ip|root_indicator|context|dir|git):*:surround:(left|right)' fg_color 4
zstyle ':apollo:apollo:*:*:(virtualenv|quota|public_ip|root_indicator|context|dir|git):*:surround:(left|right)' style bold

zstyle ':apollo:apollo:*:*:*:*:surround:left' text "( "
zstyle ':apollo:apollo:*:*:*:*:surround:right' text " )"

zstyle ':apollo:apollo:*:*:background_jobs:*:left:label' text "Jobs: "

zstyle ':apollo:apollo:*:*:date:*' elements "month" "day" ", " "year"
zstyle ':apollo:example:*:*:date:*:dow' verbose "false"

zstyle ':apollo:apollo:*:*:clock:*' elements "24hour" ":" "min" " " "timezone"
zstyle ':apollo:example:*:*:clock:*' verbose "false"

zstyle ':apollo:apollo:*:*:command_execution_time:*' min_duration "1"
zstyle ':apollo:apollo:*:*:command_execution_time:*' precision "2"

zstyle ':apollo:apollo:*:*:context:*:sep' text "@"


#zstyle ':apollo:apollo:*:*:dir:*' bookmark_patterns "/home/????*/*/html;/html"
#zstyle ':apollo:apollo:*:*:dir:*' bookmarks "apollo=$HOME/apollo-zsh-theme"
zstyle ':apollo:apollo:*:*:dir:*' last_count "5"
zstyle ':apollo:apollo:*:*:dir:*' shorten_length "auto"
zstyle ':apollo:apollo:*:*:dir:*' shorten_string ""
zstyle ':apollo:apollo:*:*:dir:*:last' fg_color 11
zstyle ':apollo:apollo:*:*:dir:*:sep' fg_color 2
zstyle ':apollo:apollo:*:*:dir:*:sep' text "/"
zstyle ':apollo:apollo:*:*:dir:*:shortened' fg_color 8

zstyle ':apollo:apollo:*:*:git:*' elements "local_branch" "action" " " "commit_hash" " " "remote_branch" " " "modified" "|" "untracked" "|" "stash_count"
zstyle ':apollo:apollo:*:*:git:*:action:left:label' text " "
zstyle ':apollo:apollo:*:*:git:*:local_branch' fg_color 2
zstyle ':apollo:apollo:*:*:git:*:remote_branch' fg_color 1
zstyle ':apollo:apollo:*:*:git:*:untracked' fg_color 5

zstyle ':apollo:apollo:*:*:status:*' pipe_status "true"
zstyle ':apollo:apollo:*:*:status:bad:*' fg_color 1
zstyle ':apollo:apollo:*:*:status:bad:*' style bold

zstyle ':apollo:apollo:core:*:ruler' text "──"

zstyle ':apollo:example:core:cache' disable

zstyle ':apollo:apollo:core:decorations' enabled "true"

zstyle ':apollo:apollo:core:links' enabled "true"
zstyle ':apollo:apollo:core:links:*:*:none' text ""
zstyle ':apollo:apollo:core:links:*:left:top' text "╭─"
zstyle ':apollo:apollo:core:links:*:left:mid' text "├─"
zstyle ':apollo:apollo:core:links:*:left:str' text "│ "
zstyle ':apollo:apollo:core:links:*:left:bot' text "╰─"
zstyle ':apollo:apollo:core:links:*:right:top' text "─╮"
zstyle ':apollo:apollo:core:links:*:right:mid' text "─┤"
zstyle ':apollo:apollo:core:links:*:right:str' text " │"
zstyle ':apollo:apollo:core:links:*:right:bot' text "─╯"

zstyle ':apollo:apollo:core:prompt:end' fg_color 2
zstyle ':apollo:apollo:core:prompt:end' text "> "

zstyle ':apollo:*:core:pipestatus' fix "true"
