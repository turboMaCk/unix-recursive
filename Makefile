System/Posix/**/*.hs: gen/*.*
	./gen/generate.sh

.PHONY: generate
generate: System/Posix/**/*.hs

.PHONY: bin
bin: System/Posix/**/*.hs bin/*.hs
	stack build --flag unix-recursive:bin

test/workdir:
	./test/setup.sh

.PHONY: test
test: System/Posix/**/*.hs test/workdir
	stack test

.PHONY: clean-test-workdir
clean-test-workdir:
	$(RM) -r test/workdir

.PHONY: bench
bench: System/Posix/**/*.hs bin/*.hs bench/*.hs
	stack bench

.PHONY: docs
docs: System/Posix/**/*.hs
	stack haddock
