
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TRUE
    | SLASH
    | RETURN
    | RAT
    | PV
    | PRINT
    | PO
    | PLUS
    | PF
    | NUM
    | MULT
    | INT
    | INF
    | IF
    | ID of (
# 11 "parser.mly"
       (string)
# 26 "parser.ml"
  )
    | FALSE
    | EQUAL
    | EOF
    | ENTIER of (
# 10 "parser.mly"
       (int)
# 34 "parser.ml"
  )
    | ELSE
    | DENOM
    | CONST
    | CO
    | CF
    | CALL
    | BOOL
    | AO
    | AF
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState88
  | MenhirState81
  | MenhirState79
  | MenhirState75
  | MenhirState73
  | MenhirState68
  | MenhirState63
  | MenhirState52
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState43
  | MenhirState41
  | MenhirState38
  | MenhirState35
  | MenhirState32
  | MenhirState28
  | MenhirState22
  | MenhirState17
  | MenhirState16
  | MenhirState13
  | MenhirState12
  | MenhirState8
  | MenhirState7
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 3 "parser.mly"
  

open Type
open Ast.AstSyntax

# 96 "parser.ml"

let rec _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.programme) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (lfi : (Ast.AstSyntax.programme))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.AstSyntax.programme) = 
# 56 "parser.mly"
                          (lfi)
# 115 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.AstSyntax.programme)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (lf : (Ast.AstSyntax.fonction))), _, (lfi : (Ast.AstSyntax.programme))) = _menhir_stack in
        let _v : (Ast.AstSyntax.programme) = 
# 59 "parser.mly"
                          (let (Programme (lf1,li))=lfi in (Programme (lf::lf1,li)))
# 134 "parser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_cp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.AstSyntax.expression))), _, (le : (Ast.AstSyntax.expression list))) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression list) = 
# 104 "parser.mly"
                (e1::le)
# 151 "parser.ml"
         in
        _menhir_goto_cp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (n : (
# 11 "parser.mly"
       (string)
# 166 "parser.ml"
            ))), _, (lp : (Ast.AstSyntax.expression list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.expression) = 
# 88 "parser.mly"
                          (AppelFonction (n,lp))
# 174 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_is : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.bloc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.bloc) = 
# 64 "parser.mly"
                          (li)
# 205 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState41 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _1 = () in
                let _v : (Ast.AstSyntax.instruction) = 
# 76 "parser.mly"
                                    (TantQue (exp,li))
# 217 "parser.ml"
                 in
                _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
            | MenhirState47 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | AO ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState49 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))), _, (li1 : (Ast.AstSyntax.bloc))), _, (li2 : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _4 = () in
                let _1 = () in
                let _v : (Ast.AstSyntax.instruction) = 
# 75 "parser.mly"
                                    (Conditionnelle (exp,li1,li2))
# 251 "parser.ml"
                 in
                _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
            | MenhirState3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (_1 : (
# 11 "parser.mly"
       (string)
# 260 "parser.ml"
                ))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _v : (Ast.AstSyntax.programme) = 
# 60 "parser.mly"
                          (Programme ([],li))
# 265 "parser.ml"
                 in
                _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i1 : (Ast.AstSyntax.instruction))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
        let _v : (Ast.AstSyntax.bloc) = 
# 68 "parser.mly"
                          (i1::li)
# 283 "parser.ml"
         in
        _menhir_goto_is _menhir_env _menhir_stack _menhir_s _v
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RETURN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_i : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | CONST ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ID _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | PRINT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | RAT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | AF | RETURN ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_goto_dp : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Type.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 367 "parser.ml"
        ))), _, (lp : ((Type.typ * string) list))) = _menhir_stack in
        let _v : ((Type.typ * string) list) = 
# 80 "parser.mly"
                          ((t,n)::lp)
# 372 "parser.ml"
         in
        _menhir_goto_dp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | CONST ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | ID _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | IF ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | PRINT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | RAT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | WHILE ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | RETURN ->
                    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_e : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | CO ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | DENOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | ENTIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | ID _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | NUM ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PO ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PF ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.expression) = 
# 89 "parser.mly"
                          (Rationnel(e1,e2))
# 514 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.AstSyntax.expression) = 
# 91 "parser.mly"
                          (Denominateur e1)
# 531 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.AstSyntax.expression) = 
# 90 "parser.mly"
                          (Numerateur e1)
# 542 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | INF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | MULT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.expression) = 
# 100 "parser.mly"
                          (exp)
# 641 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.expression) = 
# 96 "parser.mly"
                          (Binaire (Plus,e1,e2))
# 693 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.expression) = 
# 97 "parser.mly"
                          (Binaire (Mult,e1,e2))
# 718 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.expression) = 
# 99 "parser.mly"
                          (Binaire (Inf,e1,e2))
# 743 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.expression) = 
# 98 "parser.mly"
                          (Binaire (Equ,e1,e2))
# 768 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AO ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.AstSyntax.instruction) = 
# 74 "parser.mly"
                                    (Affichage (e1))
# 803 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AO ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (n : (
# 11 "parser.mly"
       (string)
# 835 "parser.ml"
            ))), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.AstSyntax.instruction) = 
# 72 "parser.mly"
                                    (Affectation (n,e1))
# 842 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 863 "parser.ml"
            ))), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (Ast.AstSyntax.instruction) = 
# 71 "parser.mly"
                                    (Declaration (t,n,e1))
# 870 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 896 "parser.ml"
                ))), _, (p : ((Type.typ * string) list))), _, (li : (Ast.AstSyntax.bloc))), _, (exp : (Ast.AstSyntax.expression))) = _menhir_stack in
                let _11 = () in
                let _10 = () in
                let _8 = () in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _v : (Ast.AstSyntax.fonction) = 
# 62 "parser.mly"
                                                         (Fonction(t,n,p,li,exp))
# 907 "parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | ID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | RAT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.AstSyntax.expression list) = 
# 103 "parser.mly"
                ([])
# 946 "parser.ml"
     in
    _menhir_goto_cp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.AstSyntax.bloc) = 
# 67 "parser.mly"
                          ([])
# 955 "parser.ml"
     in
    _menhir_goto_is _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1049 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | CO ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | DENOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | ENTIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | ID _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | NUM ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PO ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ENTIER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | PV ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), (n : (
# 11 "parser.mly"
       (string)
# 1120 "parser.ml"
                    ))), (e : (
# 10 "parser.mly"
       (int)
# 1124 "parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (Ast.AstSyntax.instruction) = 
# 73 "parser.mly"
                                    (Constante (n,e))
# 1132 "parser.ml"
                     in
                    _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Type.typ * string) list) = 
# 79 "parser.mly"
                          ([])
# 1170 "parser.ml"
     in
    _menhir_goto_dp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.AstSyntax.expression) = 
# 93 "parser.mly"
                          (True)
# 1182 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1247 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 11 "parser.mly"
       (string)
# 1255 "parser.ml"
    )) = _v in
    let _v : (Ast.AstSyntax.expression) = 
# 92 "parser.mly"
                          (Ident n)
# 1260 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.AstSyntax.expression) = 
# 94 "parser.mly"
                          (False)
# 1272 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (int)
# 1279 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (
# 10 "parser.mly"
       (int)
# 1287 "parser.ml"
    )) = _v in
    let _v : (Ast.AstSyntax.expression) = 
# 95 "parser.mly"
                          (Entier e)
# 1292 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | PF ->
                _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | CONST ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | ID _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | IF ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | PRINT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RAT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | AF ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState79 | MenhirState68 | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CALL ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | CO ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | DENOM ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | ENTIER _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
                | FALSE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | ID _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
                | NUM ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | PO ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | TRUE ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | RAT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | PF ->
                    _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | RAT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PF ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Type.typ) = 
# 85 "parser.mly"
          (Rat)
# 1686 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Type.typ) = 
# 84 "parser.mly"
          (Int)
# 1698 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1705 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AO ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Type.typ) = 
# 83 "parser.mly"
          (Bool)
# 1727 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.AstSyntax.programme) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RAT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 1771 "parser.ml"
