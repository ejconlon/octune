stack_build := "stack build --fast"
src_dirs := "app src test"

# No default tasks
default:
  just --list

# Build and run tests on file change
watch target="":
  {{ stack_build }} --test --file-watch {{ target }}

# Build and run tests
test target="":
  {{ stack_build }} --test {{ target }}

# Build only
build target="":
  {{ stack_build }} --test --no-run-tests {{ target }}

# Enter repl
repl target="":
  stack ghci --test --ghci-options "-XOverloadedStrings" {{ target }}

# Clean stack work
clean:
  stack clean --full

# Open browser with generated docs
docs:
  stack haddock --open

# Install tool deps
deps:
  stack build --copy-compiler-tool hlint fourmolu apply-refact hp2pretty eventlog2html

# Format with fourmolu
format:
  stack exec -- fourmolu --mode inplace {{ src_dirs }}

# Lint with hlint
lint:
  stack exec -- hlint {{ src_dirs }}

# Apply hlint suggestions
lint-apply:
  find {{ src_dirs }} -name '*.hs' | xargs -t -I % stack exec -- hlint % --refactor --refactor-options="--inplace"

# Generate wav for a single file
sample-gen target:
  mkdir -p out
  stack exec octune -- -o out/{{target}} samples/{{target}}.otn

# Generate wav for a directory
sample-gen-dir target:
  mkdir -p out
  stack exec octune -- -o out/{{target}} samples/{{target}}/*.otn

# Profile wav gen for a single file
sample-gen-prof target: clean-profile
  mkdir -p out
  stack build --profile
  stack exec --profile octune -- +RTS -p -RTS -o out/{{target}} samples/{{target}}.otn
  stack exec profiteur -- octune.prof

# Catch exceptions
sample-gen-dbg-dir target: clean-profile
  mkdir -p out
  stack build --profile
  stack exec --profile octune -- +RTS -xc -RTS -o out/{{target}} samples/{{target}}/*.otn

# Profile heap
sample-gen-heap-dir target: clean-profile
  mkdir -p out
  stack build --profile
  stack exec --profile octune -- +RTS -hc -xc -RTS -o out/{{target}} samples/{{target}}/*.otn || true
  stack exec hp2pretty -- --key octune.hp.txt octune.hp
  rm -f C*.svg

# event_args := "-hc -l -pa -xc --eventlog-flush-interval=0.1"
event_args := "-hc -l -pa -xc"

# Profile by event log
sample-gen-event-dir target: clean-profile
  mkdir -p out
  stack build --trace --profile
  stack exec --trace --profile octune -- +RTS {{event_args}} -RTS -o out/{{target}} samples/{{target}}/*.otn || true
  stack exec eventlog2html -- octune.eventlog

# Profile by event log
sample-gen-event target: clean-profile
  mkdir -p out
  stack build --trace --profile
  stack exec --trace --profile octune -- +RTS {{event_args}} -RTS -o out/{{target}} samples/{{target}}.otn
  stack exec eventlog2html -- octune.eventlog

clean-profile:
  rm -f octune.hp octune.prof octune.prof.html octune.hp octune.hp.txt octune.eventlog octune.eventlog.html octune.svg

test-pat pat:
  {{ stack_build }} --test --ta="--pattern" --ta="\"{{pat}}\""

test-replay pat replay:
  {{ stack_build }} --test --ta="--pattern" --ta="\"{{pat}}\"" --ta="--hedgehog-replay" --ta="\"{{replay}}\""