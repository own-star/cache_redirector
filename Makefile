PROJECT = cr
NODE_NAME=${PROJECT}@$(shell ip route get 1 | awk '{ print $$7;exit}')
#PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: all test clean

all: deps compile release

run: compile release debug

deps:
	@$(REBAR3) get-deps

compile:
	@$(REBAR3) as local compile

release:
	@$(REBAR3) as local release -n ${PROJECT}

tests: eunit proper

eunit:
	@$(REBAR3) as testing eunit

ct:
	@$(REBAR3) as testing ct

#	@$(REBAR3) as testing ct --suite test/ct/test_suite.erl

proper:
	@$(REBAR3) as testing proper

cover: tests
	@$(REBAR3) as testing covertool generate
	@$(REBAR3) as testing cover --verbose

analyze: xref dialyzer

xref:
	@$(REBAR3) as testing xref

lint:
	@$(REBAR3) as testing lint

dialyzer:
	@$(REBAR3) as testing dialyzer

typer:
	typer -DAPP_NAME=$(PROJECT) -r src

clean:
	@$(REBAR3) clean

cleanall:
	rm -rf ./_build && rm -rf ./rebar.lock

production_release:
	@$(REBAR3) as production release -n $(PROJECT)

dev_release:
	@$(REBAR3) as dev release -n $(PROJECT)

dev_pci_release:
	@$(REBAR3) as dev_pci release -n $(PROJECT)

uat_pci_release:
	@$(REBAR3) as uat_pci release -n $(PROJECT)

production_pci_release:
	@$(REBAR3) as production_pci release -n $(PROJECT)

test_release:
	@$(REBAR3) as testing release -n $(PROJECT)

debug:
	ERL_FLAGS="-args_file ./config/vm.args.debug" $(REBAR3) as local shell --name ${NODE_NAME}

# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
