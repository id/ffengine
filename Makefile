PROJECT = ffengine
DEPS = cowboy jiffy bcrypt poolboy epgsql lager erlpass
TEST_DEPS = inets
dep_epgsql = git https://github.com/epgsql/epgsql.git devel

NO_MAKEDEP ?= 1

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

