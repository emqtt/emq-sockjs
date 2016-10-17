
emq_sockjs
==========

The plugin could make web browsers(even IE6) talk to the EMQ broker with the awesome Stomp/SockJS library.

The Web Apps could pub/sub messages to/from the EMQ broker and communicate with other MQTT clients.

Build
-----

```
make && make tests
```

Configuration
-------------

**Default port is 61616**

```erlang
[
  {emq_sockjs, [

    {sockjs, []},

    {cowboy_listener, {stomp_sockjs, 61616, 4}},

    %% TODO: unused...
    {stomp, [
      {frame, [
        {max_headers,       10},
        {max_header_length, 1024},
        {max_body_length,   8192}
      ]}
    ]}

  ]}
].
```

Load
----

```
./bin/emqttd_ctl plugins load emq_sockjs
```

Demo
----

http://localhost:61616/index.html

Author
-----------

Contact <feng@emqtt.io> if any issues.

