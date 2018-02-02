# ===== Basics

# If you type foo, and it isn't a command, and it is a directory in your cdpath, go there
setopt auto_cd

# Directory stack configuration
unsetopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

# Allow comments even in interactive shells (especially for Muness)
# setopt INTERACTIVE_COMMENTS

# ===== Completion 

# Allow completion from within a word/phrase
setopt complete_in_word 

# When completing from the middle of a word, move the cursor to the end of the word
setopt always_to_end            

# ===== Prompt

# Enable parameter expansion, command substitution, and arithmetic expansion in the prompt
setopt prompt_subst

unsetopt menu_complete
setopt auto_menu
