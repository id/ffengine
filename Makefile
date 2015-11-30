PROJECT = ffengine
DEPS = cowboy jiffy bcrypt poolboy epgsql lager erlpass

dep_bcrypt = git https://github.com/opscode/erlang-bcrypt.git master
dep_epgsql = git https://github.com/epgsql/epgsql.git devel

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

