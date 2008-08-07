ERL=erl
ERLC=erlc
EUNIT_ROOT=/Users/nem/projects/erlang/eunit

PARSER=src/erlydtl/erlydtl_parser

TEMPLATES=$(wildcard src/erlydtl/forms/*.dtl)
TEMPLATE_OBJS:=$(TEMPLATES:src/erlydtl/forms/%.dtl=ebin/erlydtl/forms/%.beam)

all: main $(TEMPLATE_OBJS)

ebin:
	mkdir ebin

main: ebin ebin/erlydtl/forms $(PARSER).erl
	$(ERL) -pa $(EUNIT_ROOT)/ebin -make 

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl src/erlydtl/erlydtl_parser.yrl

run:
	$(ERL) -pa ebin


test:
	$(ERL) -noshell -pa ebin \
		-s erlydtl_unittests run_tests \
		-s erlydtl_dateformat_tests run_tests \
		-s init stop
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump $(PARSER).erl

ebin/erlydtl/forms:
	mkdir ebin/erlydtl/forms

ebin/erlydtl/forms/%.beam: src/erlydtl/forms/%.dtl
	$(ERL) -pa ebin -noshell -s erlydtl_run compile \"$<\" erlydtl.forms.$(shell basename $< .dtl)
