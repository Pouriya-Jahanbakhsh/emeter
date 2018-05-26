REBAR = ./rebar3
APP_VER := $(shell awk '$$1 == ",{vsn," {print substr($$2, 2, length($$2)-3)}' src/emeter.app.src)) 
BUILD_PATH = _build
LIB_PATH = $(BUILD_PATH)/default/lib
REL_PATH = $(BUILD_PATH)/default/rel/emeter

.PHONY: all compile web-compile shell release console start stop tar clean distclean

all: web-compile compile

compile:
	@ $(REBAR) compile

web-compile:
	@ npm install
	@ ./node_modules/.bin/gulp deploy
	@ escript build_web_assets.script ./assets/index.html ./assets/main.css ./assets/app.js ./LICENSE


shell: compile
	@ erl -pa $(LIB_PATH)/cowboy/ebin   \
	          $(LIB_PATH)/cowlib/ebin   \
	          $(LIB_PATH)/director/ebin \
	          $(LIB_PATH)/emeter/ebin   \
	          $(LIB_PATH)/goldrush/ebin \
	          $(LIB_PATH)/jiffy/ebin    \
	          $(LIB_PATH)/lager/ebin    \
	          $(LIB_PATH)/ranch/ebin    \
	          $(LIB_PATH)/jsone/ebin

release:
	@ $(REBAR) release
	@ $(MAKE) check-inetrc

console: check-release
	@ $(REL_PATH)/bin/emeter console


start: check-release
	@ $(REL_PATH)/bin/emeter start


stop: check-release
	@ $(REL_PATH)/bin/emeter stop


tar: check-release 
	@ cd $(REL_PATH) && cd ../ && tar czf emeter.tar.gz emeter


clean:
	@ $(REBAR) clean

distclean: clean
	@ rm -rf $(BUILD_PATH)


check-release:
ifeq ($(wildcard $(REL_PATH)/bin/.*),)
	@ $(MAKE) release
endif

check-inetrc:
ifneq ($(wildcard $(inetrc)),)
	@ cp $(inetrc) $(REL_PATH)/releases/
	@ echo "\n-kernel inetrc '\"./releases/inetrc\"'" >> $(REL_PATH)/releases/$(strip $(APP_VER))/vm.args
endif
