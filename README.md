# Elixir Debug
Using to start debugger for elixir by debugger module in erlang.

Eesure your elixir code in a mix project.

## Usage
```elixir
:dhelper.start 'beam_dir', 'source_dir', 'your_project_name'
:debugger.start
```
### or
```elixir
%% make sure you are in project directory.
:dhelper.start 'your_project_name'
```
