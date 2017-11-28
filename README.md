### ditax_router

It is an erlang application to route data from one incoming point to different outs. All outs should be defined in config (`destinations` list). Incoming point defined as `source`.

Destinations have a parameter which defines is this destination is obligatory. I call it `main` for the obligatory one and `additional` for the optional ones.

You can have only one obligatory destination and any number of additionals.

If `main` is down the whole application stops. If `additional` is down you'll get the log message and that's all. 

Every destination handler can have own supervisor which defines its restart behaviour. This supervisor is optional and could be defined in `initialize/3` function. Besides, feel free to return from `initialize/3` module as a child for the main supervisor. Don't forget to use `simple_one_for_one` if you're planning to start the handler from `ditax_router_controller`.