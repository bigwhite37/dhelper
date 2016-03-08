# Elixir Debug
Using for elixir by debugger module in erlang.

Eesure your elixir code in a mix project.

Now you can use this module to debug deps lib together.

## Usage
```elixir
:dhelper.start 'beam_dir', 'source_dir', 'your_project_name'
:debugger.start
```
### or
```elixir
# make sure you are in project directory.
:dhelper.start 'your_project_name'
```

### Debug with deps
```elixir
# make sure you are in project directory.
:dhelper.startd 'your_project_name', ['dep_name1', 'dep_name2']
```

If your catch something should not heppend, try below to add `Logger` to debugger
```elixir
:dhelper.switch
```
