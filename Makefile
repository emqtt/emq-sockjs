PROJECT = emq_sockjs
PROJECT_DESCRIPTION = SockJS(Stomp) Plugin
PROJECT_VERSION = 1.2

DEPS = sockjs emq_stomp
dep_sockjs = git https://github.com/emqtt/sockjs-erlang.git master
dep_emq_stomp = git https://github.com/emqtt/emqttd_stomp emq20

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd emq20

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
