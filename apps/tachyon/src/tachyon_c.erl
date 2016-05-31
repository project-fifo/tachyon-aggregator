-module(tachyon_c).


-export([c/2]).

%% <<_HostSize:32/integer,   _Host:_HostSize/binary,
%%   _UuidSize:32/integer,   _Uuid:_UuidSize/binary,
%%   _SnapTime:64/integer,
%%   _NameSize:32/integer,   _Name:_NameSize/binary,
%%   _ModuleSize:32/integer, _Module:_ModuleSize/binary,
%%   _ClassSize:32/integer,  _Class:_ClassSize/binary,
%%   _Instance:64/integer,
%%   _KeySize:32/integer, Key:_KeySize/binary
%%   _Type, V:64/intege>>

c(Module, File)  ->
    case file:read_file(File) of
        {ok, Content}  ->
            case tl_lexer:string(binary_to_list(Content)) of
                {ok, L, _} ->
                    case tl_parser:parse(L) of
                        {ok, Matches} ->
                            M1 = [c1(Target, lists:map(fun expand/1, Data)) ||
                                     {Target, Data} <- Matches, Target /= fn],
                            {ok, [header(Module), $\n,
                                  M1,
                                  "match(_, State) ->\n",
                                  mk_target(ignore), ".\n"] ++
                                 [[Data, $\n] ||
                                     {fn, Data} <- Matches]};
                        E1 ->
                            {L, E1}
                    end;
                E2 ->
                    E2
            end;
        E3 ->
            E3
    end.


c1(raw, Data) ->
    Data;

c1(Target, Data) ->
    Data1 = case Target of
                {_, Ts} ->
                    Data ++ [T || T <- Ts, is_atom(T)] ++
                        [T || {_, T} <- Ts];
                _ ->
                    Data
            end,
    Ignore = Target == ignore,
    Instance = (not proplists:get_bool(instance, Data1)) orelse Ignore,
    I = "        ",
    R = [
         "<<", mk_bin(Data1, host,   Ignore), ",\n",
         I,    mk_bin(Data1, uuid,   Ignore), ",\n",
         I,    ignore(Ignore), "SnapTime:64/integer,\n",
         I,    mk_bin(Data1, name,   Ignore), ",\n",
         I,    mk_bin(Data1, module, Ignore), ",\n",
         I,    mk_bin(Data1, class,  Ignore), ",\n",
         I,    ignore(Instance), "Instance:64/integer,\n",
         I,    mk_bin(Data1, key,    Ignore), ",\n"
        ],
    R1 = case Ignore of
             true ->
                 R ++ I ++ "_/binary>>";
             false ->
                 R ++ I ++ "$i, V:64/integer>>"
         end,
    R2 = ["match(", R1, ", State) ->\n",
          mk_dimensions(Target, Data),
          mk_target(Target), ";\n\n"],
    R2.

mk_dimensions(ignore, _Data) ->
    "";
mk_dimensions({_Collection, Target}, Data) ->
    D1 = [target_to_dim(T) || T <- Target, not is_list(T)],
    D2 = [constant_to_dim(C) || C <- Data],
    D = string:join(D1 ++ D2, ",\n                  "),
    ["    Dimensions = [", D, "],\n"] .


target_to_dim({_, E}) ->
    target_to_dim(E);
target_to_dim(E) when is_atom(E) ->
    ["{<<\"kstat\">>, <<\"", a2l(E), "\">>, ", to_cap(E), "}"].

constant_to_dim({N, V}) ->
    ["{<<\"kstat\">>, <<\"", a2l(N), "\">>, <<\"", V, "\">>}"].

expand(gz) ->
    {uuid, "global"};
expand(Other) ->
    Other.

mk_target(ignore) ->
    "    tachyon_mps:provide(),\n"
        "    {ok, State}";

mk_target({Bucket, L}) ->
    L1 = [mk_elem(E) || E <- L],
    ["    Bucket = <<\"", a2l(Bucket), "\">>,\n"
     "    Key = [", string:join(L1, ", "), "],\n"
     "    Collection = Bucket,\n"
     "    Metric = Key,\n",
     case [mk_elem(Fn) || Fn = {_, _} <- L] of
         [] ->
             "    Ignore = false,\n";
         FNs ->
             FNs1 = [["do_ignore(", F, $)] || F <- FNs],
             ["    Ignore = ",
              string:join(FNs1, "\n      orelse "),
              "\n"]
     end,
     "    putd(Ignore, Collection, Metric, Bucket, Key, Dimensions),\n"
     "    putb(Ignore, Bucket, Key, SnapTime, V, State)"
    ].

mk_elem(instance) ->
    "integer_to_binary(Instance)";
mk_elem({Fn, A}) when is_atom(A) ->
    [a2l(Fn), $(, to_cap(a2l(A)), $)];
mk_elem(A) when is_atom(A) ->
    to_cap(a2l(A));
mk_elem(L) when is_list(L) ->
    ["<<\"", L, "\">>"].


mk_bin(Data, Key, Ignore) ->
    case proplists:get_value(Key, Data) of
        undefined ->
            mk_bin(a2l(Key), true);
        true ->
            mk_bin(to_cap(a2l(Key)), Ignore);
        Val when is_list(Val) ->
            mk_bin(Val);
        Name when is_atom(Name) ->
            mk_bin(to_cap(a2l(Name)), Ignore)
    end.

mk_bin(Val) ->
    Size = integer_to_list(length(Val)),
    [Size, ":32/integer, ",
     $", Val, $"].

mk_bin(Name, Ignore) ->
    SizeName = [$_, Name, "Size"],
    [SizeName, ":32/integer, ",
     ignore(Ignore), Name, $:, SizeName, "/binary"].


ignore(true) ->
    $_;
ignore(_) ->
    "".

header(Module) ->
    ["-module(", a2l(Module) ,").\n"
     "-behaviour(ensq_channel_behaviour).\n"
     "-record(state, {host, port, connections = #{}}).\n"
     "-export([init/0, response/2, message/3, error/2]).\n"
     "init() ->\n"
     "    {ok, {Host, Port}} = application:get_env(tachyon, ddb_ip),\n"
     "    {ok, #state{host = Host, port = Port}}.\n"
     "response(_Msg, State) ->\n"
     "    {ok, State}.\n"
     "error(_Msg, State) ->\n"
     "    {ok, State}.\n"
     "message(M, _, State) ->\n"
     "    match(M, State).\n"
     "putb(true, _, _, _, _, State) ->"
     "    {ok, State};\n"
     "putb(_, Bucket, Metric, Time, Value,\n"
     "     State = #state{host = H, port = P, connections = Cs}) ->\n"
     "    C1 = case maps:find(Bucket, Cs) of\n"
     "             {ok, C} ->\n"
     "                 C;\n"
     "             error ->\n"
     "                 {ok, CN0} = ddb_tcp:connect(H, P),\n"
     "                 {ok, CN1} = ddb_tcp:stream_mode(Bucket, 2, CN0),\n"
     "                 CN1\n"
     "         end,\n"
     "    tachyon_mps:send(),\n"
     "    tachyon_mps:provide(),\n"
     "    tachyon_mps:handle(),\n"
     "    Metric2 = dproto:metric_from_list(Metric),\n"
     "    case ddb_tcp:send(Metric2, Time, mmath_bin:from_list([Value]), C1) of\n"
     "        {ok, C2} ->\n"
     "            Cs1 = maps:put(Bucket, C2, Cs),\n"
     "            {ok, State#state{connections = Cs1}};\n"
     "        {error, _, C2} ->\n"
     "            Cs1 = maps:put(Bucket, C2, Cs),\n"
     "            {ok, State#state{connections = Cs1}}\n"
     "    end.\n"
     "do_ignore(ignore) -> true;\n"
     "do_ignore(_) -> false.\n",
     case application:get_env(dqe_idx, lookup_module, dqe_idx_ddb) of
         dqe_idx_ddb ->
             ["putd(_Ignore, _, _, _, _, _) ->\n"
              "    ok.\n"];
         _ ->
             ["putd(true, _, _, _, _, _) ->\n"
              "    ok;\n"
              "putd(_, Collection, MetricL, Bucket, KeyL, Dimensiosn) ->"
              "    Metric = dproto:metric_from_list(MetricL),\n"
              "    Key = dproto:metric_from_list(KeyL),\n"
              "    tachyon_idx:put(Collection, Metric, Bucket, "
              "Key, Dimensiosn)."]
     end].
to_cap(A) when is_atom(A) ->
    to_cap(a2l(A));

to_cap([C | R]) ->
    [C1] = string:to_upper([C]),
    [C1 | R].

a2l(A) ->
    atom_to_list(A).
