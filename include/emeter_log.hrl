-ifdef(emeter_use_lager).
    -compile([{parse_transform, lager_transform}]).
    -define(log(Type_, Txt_, Args_), lager:Type_(Txt_, Args_)).
-else.
    -define(log(Type_, Txt_, Args_)
           ,begin Line_ = ?LINE,
                  Func_ =
                      case Type_ of
                          error ->
                              error_report;
                          warning ->
                              warning_report;
                          _ ->
                              info_report
                      end,
                  error_logger:Func_(<<"EMeter("
                                      ,(erlang:atom_to_binary(?MODULE, 'utf8'))/binary
                                      ,":"
                                      ,(erlang:atom_to_binary(?FUNCTION_NAME, 'utf8'))/binary
                                      ,"/"
                                      ,(erlang:integer_to_binary(?FUNCTION_ARITY))/binary
                                      ,"-"
                                      ,(erlang:integer_to_binary(Line_))/binary
                                      ,"): "
                                      ,Txt_/binary>>
                                    ,Args_)
            end).
-endif.


-define(error(Txt), ?error(Txt, [])).
-define(error(Txt, Args), ?log(error, Txt, Args)).

-define(warning(Txt), ?warning(Txt, [])).
-define(warning(Txt, Args), ?log(warning, Txt, Args)).

-define(info(Txt), ?info(Txt, [])).
-define(info(Txt, Args), ?log(info, Txt, Args)).