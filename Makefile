USE=stack

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
	$(USE) test

.PHONY: clean-test-workdir
clean-test-workdir:
	$(RM) -r test/workdir

.PHONY: bench
bench: System/Posix/**/*.hs bin/*.hs bench/*.hs
	$(USE) bench

.PHONY: docs
docs: System/Posix/**/*.hs
	$(USE) haddock
