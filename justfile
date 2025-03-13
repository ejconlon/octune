stack_build := "stack build --fast"
src_dirs := "src test"

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

# Run the executable
sample-gen target:
  mkdir -p out
  stack exec octune -- -o out/{{target}} samples/{{target}}.otn
