%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Header for common functions.
%%% @end
%%% Created : 29. Apr 2019 18:47
%%%-------------------------------------------------------------------
-author("Attila Makra").

%% =============================================================================
%% Types
%% =============================================================================

-type time_unit() :: 'second'| 'millisecond'| 'microsecond'| 'nanosecond' | 'native'.
-type timestamp() :: {time_unit(), integer()}.
-type fork_retval() :: {ok | error | throw, term()}.