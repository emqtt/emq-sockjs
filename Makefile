PROJECT = emqttd_sockjs
PROJECT_DESCRIPTION = SockJS(Stomp) Plugin for emqttd broker
PROJECT_VERSION = 1.1

DEPS = sockjs

#dep_emqttd       = git https://github.com/emqtt/emqttd plus
#dep_emqttd_stomp = git https://github.com/emqtt/emqttd_stomp plus
dep_sockjs       = git https://github.com/emqtt/sockjs-erlang.git master

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app:: rebar.config
