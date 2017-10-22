.PHONY: all clean deps clean-deps
.DEFAULT_GOAL := all

VIRTUALENV_DIR ?= .env
VIRTUALENV_BIN = $(VIRTUALENV_DIR)/bin
DEPS_UPTODATE = $(VIRTUALENV_DIR)/.deps-uptodate
DEPENDENCIES = requirements.txt

VIRTUALENV := $(shell command -v virtualenv 2> /dev/null)
PIP := $(shell command -v pip 2> /dev/null)

.ensure-virtualenv: .ensure-pip
ifndef VIRTUALENV
	$(error "virtualenv is not installed. Install with `pip install [--user] virtualenv`.")
endif
	touch .ensure-virtualenv

.ensure-pip:
ifndef PIP
	$(error "pip is not installed. Install with `python -m [--user] ensurepip`.")
endif
	touch .ensure-pip

$(VIRTUALENV_BIN)/pip: .ensure-virtualenv
	virtualenv $(VIRTUALENV_DIR)

$(DEPS_UPTODATE): $(DEPENDENCIES) $(VIRTUALENV_BIN)/pip
	$(VIRTUALENV_BIN)/pip install -r $(DEPENDENCIES)
	touch $(DEPS_UPTODATE)

deps: $(DEPS_UPTODATE)


SVGS = $(patsubst %.diag,%.svg,$(wildcard *.diag))

%.svg: %.diag $(DEPS_UPTODATE)
	seqdiag -Tsvg $< -o $@

all: $(SVGS)

clean:
	rm -f $(SVGS)
