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
  stack ghci --test --ghci-options "-XOverloadedStrings -XOverloadedLists" {{ target }}

# Clean stack work
clean:
  stack clean --full

# Open browser with generated docs
docs:
  stack haddock --open

# Install tool deps
deps:
  stack build --copy-compiler-tool hlint fourmolu apply-refact

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
sample-gen-prof target:
  mkdir -p out
  stack build --profile
  stack exec --profile octune -- +RTS -p -RTS -o out/{{target}} samples/{{target}}.otn
  stack exec profiteur -- octune.prof
