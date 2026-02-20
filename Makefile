fmt:
	stack exec -- ormolu --mode inplace hs/**/*.hs

fmt-check:
	stack exec -- ormolu --mode check hs/**/*.hs

lint:
	stack exec -- hlint hs

lint-fix:
	for f in hs/**/*.hs; do \
		stack exec -- hlint $$f --refactor --refactor-options="--inplace"; \
	done

build:
	stack build

test:
	stack test