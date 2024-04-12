-ifdef(debug).

-define(LOG(MSG, ARGS), io:format(MSG, ARGS)).

-else.

-define(LOG(X, Y), true).

-endif.
