function venv \
    --description "Enter a Python virtual environment (venv)." \
    --argument-names venv_name
    if test -n "$venv_name"
        source $HOME/.venv/$venv_name/bin/activate.fish
    else
        echo Error: no venv name given.
    end
end
