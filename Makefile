ERL = erl
EFLAGS = -pa ../netspire-core/ebin

all: compile

compile:
	$(ERL) $(EFLAGS) -make

clean:
	find . -name "*~" -exec rm -f {} \;

