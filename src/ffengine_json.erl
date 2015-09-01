%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(ffengine_json).

%% API
-export([ encode/1
        , decode/1
        ]).

%%%_ * API ---------------------------------------------------------------------
encode({})   -> do_encode({[]});
encode([{}]) -> do_encode({[]});
encode(Term) -> do_encode(Term).

decode(Bin) -> unwrap(do_decode(Bin)).

%%%_ * Internal functions ------------------------------------------------------
do_encode(Term) -> jiffy:encode(Term).

do_decode(Bin) -> jiffy:decode(Bin).

unwrap({[]})   -> [{}];
unwrap({Term}) -> Term;
unwrap(Term)   -> Term.
