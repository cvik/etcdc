PROJECT = etcdc
TAG = $(shell cat src/$(PROJECT).app.src | \
	    sed -n -e 's/{vsn, \"\(.*\)\"},/\1/p' | \
		xargs)

DEPS = lhttpc \
	   lejson

dep_lejson = git git@github.com:campanja-forks/lejson.git 0.2.3
dep_lhttpc = git https://github.com/esl/lhttpc.git otp-17-compat

AUTOPATCH +=

include erlang.mk

.PHONY:

all::
	@erlc -o ebin -pa ebin test/*.erl
