-module(dns).

-include_lib("kernel/src/inet_dns.hrl").
-define(REAL_DNS_SERVER, {8,8,8,8}).

-export([start/0]).

real_q(Type, Domain, NS, 0) ->
  real_q(Type, Domain, NS, random:uniform(65535));

real_q(Type, Domain, NS, ReqID) ->
  Query = inet_dns:encode(
            #dns_rec{
                     header = #dns_header{
                                          id = ReqID,
                                          opcode = 'query',
                                          rd = 1,
                                          aa = 1
                                         },
                     qdlist = [#dns_query{
                                          domain = Domain,
                                          type = Type,
                                          class = in
                                         }]
                    }),
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  gen_udp:send(Socket, NS, 53, Query),
  {ok, {NS, 53, Reply}} = gen_udp:recv(Socket, 65535),
  {ok, R} = inet_dns:decode(Reply),
  gen_udp:close(Socket),
  R.

build_answer_list(QDList) ->
  build_answer_list(QDList, [], [], []).

build_answer_list(QDList, Answer, Authority, Resource) when length(QDList) == 0 ->
  {Answer, Authority, Resource};

build_answer_list(QDList, Answer, Authority, Resource) ->
  [Question|Tail] = QDList,
  
  RealData = real_q(Question#dns_query.type, Question#dns_query.domain, ?REAL_DNS_SERVER, 0),
  io:format("-> ~p~n", [Question#dns_query.domain]),
  case Question#dns_query.type of
    a ->
      Response =
        case Question#dns_query.domain of
          "grahamalot.com" ->
            #dns_rr{ 
                    domain = Question#dns_query.domain,
                    class = Question#dns_query.class,
                    type = Question#dns_query.type,
                    ttl = 900,
                    data = {127, 0, 0, 1}
                   };
          _ ->
            RealData#dns_rec.anlist
        end,
      build_answer_list(Tail, [Response] ++ Answer, RealData#dns_rec.nslist ++ Authority, RealData#dns_rec.arlist ++ Resource);
    _Other ->
      build_answer_list(Tail, RealData#dns_rec.anlist ++ Answer, RealData#dns_rec.nslist ++ Authority, RealData#dns_rec.arlist ++ Resource)
  end.

listen_for_dns_query(Socket) ->
  {ok, {IP, Port, Payload}} = gen_udp:recv(Socket, 65535),
  {ok, DecPayload} = inet_dns:decode(Payload),
  
  case DecPayload#dns_rec.anlist of
    [] ->
      {Answer, Authority, Resource} = build_answer_list( DecPayload#dns_rec.qdlist ),
      Header = DecPayload#dns_rec.header,
      Response = DecPayload#dns_rec{header = Header#dns_header{rcode = 3, aa = true, qr = true},
                                    anlist = lists:flatten(Answer),
                                    nslist = lists:flatten(Authority),
                                    arlist = lists:flatten(Resource)},
      gen_udp:send(Socket, IP, Port, inet_dns:encode(Response));
    _ ->
      ok
  end,
  listen_for_dns_query(Socket).

start() ->
  {ok,Socket} = gen_udp:open(53,[{ip, {192,168,1,111}}, binary, {active, false}]),
  listen_for_dns_query(Socket).
    