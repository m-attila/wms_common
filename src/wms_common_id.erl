%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2019 20:20
%%%-------------------------------------------------------------------
-module(wms_common_id).
-author("Attila Makra").

-define(CHARS,
  <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>).

%% API
-export([generate_unique_id/1]).

% -----------------------------------------------------------------------------
%% Generate instance ID
%% -----------------------------------------------------------------------------

-spec generate_unique_id(pos_integer()) ->
  binary().
generate_unique_id(ReqSize) ->
  [_ | Host] =
    re:split(atom_to_list(node()),
             "@",
             [{return, list}, {parts, 2}]),
  SysTime = integer_to_binary(os:system_time()),


  Temp = <<SysTime/binary, (list_to_binary(Host))/binary>>,

  Rest = ReqSize - byte_size(Temp),

  Plain = case Rest =< 0 of
            true ->
              Temp;
            false ->
              <<Temp/binary, (crypto:strong_rand_bytes(Rest))/binary>>
          end,
  decode(Plain, ?CHARS, byte_size(?CHARS), <<>>).


-spec decode(binary(), binary(), pos_integer(), binary()) ->
  binary().
decode(<<>>, _, _, Accu) ->
  Accu;
decode(<<V:8/integer, Rest/binary>>, Codes, LenghOfCodes, Accu) ->
  High = V div LenghOfCodes,
  Low = V rem LenghOfCodes,

  NewAccu =
    case {High, Low} of
      {0, Low} ->
        <<Accu/binary, (binary:at(Codes, Low))/integer>>;
      {High, Low} ->
        <<Accu/binary,
          (binary:at(Codes, Low))/integer,
          (binary:at(Codes, High))/integer>>
    end,
  decode(Rest, Codes, LenghOfCodes, NewAccu).
