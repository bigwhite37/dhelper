compile:
	rm -rf ebin
	mkdir ebin
	erlc -o ./ebin src/dhelper.erl

clean: rm -rf *.beam erl_crash.dump
