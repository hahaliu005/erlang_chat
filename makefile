PROJECT = my_chat
DEPS = cowboy
include erlang.mk

LOAD_PATH = \
	    ebin \
	    deps/*/ebin \

COOKIE=4SZe3DECXwAdHbz0Yzm7rWrc5AG0MjiSs90WGgsfXJ3Mx0tLuEXtfa9ud3ZOOSQ

# 部分配置参数
OPTS = \
       -pa $(LOAD_PATH) \
       -env ERL_MAX_ETS_TABLES 10000 \
       -setcookie $(COOKIE) \
       +A 4 +K true +P 120000   \
       -smp disable \
       -kernel inet_dist_listen_min 48808 \
       inet_dist_listen_max 48988 \

# 附加erl启动配置
ADD_ARGS = -args_file config/erl_args.conf

NODE=$(shell cat ./config/node_name.conf)
ifeq ($(NODE),)
  NODE = my_chat@127.0.0.1
endif

REBAR_CONFIG = rebar.config

ERL_CALL=erl_call -c $(COOKIE) -name $(NODE) -e

compile:
	$(REBAR) get-deps compile

REBAR := ./rebar --config config/$(REBAR_CONFIG)

s:
	erl $(OPTS) $(ADD_ARGS) -name $(NODE) -s my_chat_app -hidden

loader_s:
	echo "spt_reloader:start()." | \
		$(ERL_CALL)
	echo end


