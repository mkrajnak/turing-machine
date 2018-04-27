# non deterministic Turing machine simulation, project for FLP course at FIT BUT
# Martin Krajnak, xkrajn02@stud.fit.vurbr.cz

all:
	swipl -o flp18-log -g main -c turing.pl
clean:
	rm -rf flp18-log *.zip
zip:
	zip -r flp-log-xkrajn02.zip turing.pl Makefile
