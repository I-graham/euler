.PHONY: clean run

clean:
	@rm -rf ./target
	@mkdir target
	@rm -rf ./builds
	@mkdir builds

run: target/P$(P)
	@./target/P$(P)

target/%: %.hs
	@ghc -main-is $* -O $< -outputdir builds -o $@