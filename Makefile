System/Posix/**/*.hs: gen/*.*
	./gen/generate.sh

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

bench: System/Posix/**/*.hs bin/*.hs bench/*.hs
bench:
	stack bench
