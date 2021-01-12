(* Module de la passe de typage *)
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Tds
  open Ast
  open AstPlacement
  open Type
  open Code

  type t1 = AstPlacement.programme
  type t2 = string
    
  let taille_variables_declarees i = 
    match i with
    | AstType.Declaration (_, info) -> 
    begin
      match info_ast_to_info info with
      | InfoVar (_, t, _, _) -> getTaille t  
      | _ -> failwith("Erreur interne")
    end
    | _ -> 0

  let rec analyse_code_expression e = 
    match e with
    | AstType.AppelFonction(info, le) ->
      begin
      let code_arguments = String.concat "" (List.map analyse_code_expression le) in
      match info_ast_to_info info with
      | InfoFun (n, _, _) -> code_arguments ^ "CALL (ST) " ^ n ^ "\n"
      | _ -> failwith("Erreur interne")
      end
    | Rationnel (e1, e2) -> (analyse_code_expression e1) ^ (analyse_code_expression e2)
    | Numerateur e1 -> (analyse_code_expression e1) ^ "POP (0) 1\n"
    | Denominateur e1 -> (analyse_code_expression e1) ^ "POP (1) 1\n"
    | Ident info ->
      begin
      match info_ast_to_info info with
        | InfoVar(_, t, dep, reg) -> 
          "LOAD " ^ "(" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int dep) ^ "[" ^ reg ^ "]\n"
        | InfoConst (_, v) -> "LOADL " ^ string_of_int v
        | _ -> failwith("Erreur interne")
      end
    | True -> "LOADL 1\n"
    | False -> "LOADL 0\n"
    | Entier i -> "LOADL " ^ (string_of_int i) ^ "\n"
    | Binaire (b, e1, e2) ->
      begin
        let code_e1 = analyse_code_expression e1 in
        let code_e2 = analyse_code_expression e2 in
        code_e1 ^ code_e2 ^
        match b with
        | PlusInt -> "SUBR IAdd\n"
        | PlusRat -> "CALL (ST) RAdd\n"
        | MultInt -> "SUBR IMul\n"
        | MultRat -> "CALL (ST) RMUL\n"
        | EquInt -> "SUBR IEq\n"
        | EquBool -> "SUBR BEq\n"
        | Inf -> "SUBR ILss\n"
      end

  let rec analyse_code_instruction i = 
    match i with
    | AstType.Declaration (e, info) ->
      begin
        match info_ast_to_info info with
        | InfoVar (_, t, dep, reg) -> 
          "PUSH " ^ string_of_int (getTaille t) ^ "\n" ^ 
          analyse_code_expression e ^
          "STORE (" ^ string_of_int (getTaille t) ^ ") " ^ string_of_int dep ^ "[" ^ reg ^ "]"
        | _ -> failwith("Erreur interne")
      end
    | AstType.Affectation (e, info) ->
      begin
      match info_ast_to_info info with
      | InfoVar (_, t, dep, reg) ->
        analyse_code_expression e ^ 
        "STORE (" ^ string_of_int (getTaille t) ^ ") " ^ string_of_int dep ^ "[" ^ reg ^ "]"
      | _ -> failwith("Erreur interne")
      end
      | AffichageInt e -> (analyse_code_expression e) ^ "SUBR IOut\n"
      | AffichageRat e -> (analyse_code_expression e) ^ "CALL (ST) ROUT\n"
      | AffichageBool e -> (analyse_code_expression e) ^ "SUBR BOUT\n"
      | Conditionnelle (cond, t, e) ->
        let lelse = getEtiquette () in
        let lfinelse = getEtiquette () in
        analyse_code_expression cond ^ 
        "JUMPIF (0) " ^ lelse ^ "\n" ^
        analyse_code_bloc t ^
        "JUMP " ^ lfinelse ^ "\n" ^
        lelse ^ "\n" ^
        analyse_code_bloc e ^
        lfinelse ^ "\n"
      | TantQue (c, b) -> 
        let ldebut = getEtiquette () in
        let lfin = getEtiquette () in
        let code_cond = analyse_code_expression c in
        let code_b = analyse_code_bloc b in
        ldebut ^ "\n" ^
        code_cond ^ 
        "JUMPIF (0) " ^ lfin ^ "\n" ^
        code_b ^ 
        "JUMP " ^ ldebut ^ "\n" ^
        lfin ^ "\n"
      | Empty -> ""
 
  and analyse_code_bloc li = 
  let taille = List.fold_right(fun i ti -> (taille_variables_declarees i) + ti) li 0 in 
  let popfinal = "POP (0) " ^ (string_of_int taille) ^ "\n" in
  (analyse_code_li li) ^ popfinal

  and analyse_code_li li = 
    String.concat "" (List.map analyse_code_instruction li) 
  
  let analyse_code_fonction (Fonction (info, _, li, e)) = 
    match info_ast_to_info info with
    | InfoFun(nom, typeRet, typeParams) ->
    begin
      let taille_var_locales = List.fold_right (fun i ti -> (taille_variables_declarees i ) + ti) li 0 in
      let taille_params = List.fold_right (fun p tp -> (getTaille p) + tp) typeParams 0 in
      nom ^ "\n" ^
      analyse_code_li li ^
      analyse_code_expression e ^
      "POP (" ^ string_of_int(getTaille typeRet) ^ ") " ^ string_of_int taille_var_locales ^ "\n" ^
      "RETURN (" ^ string_of_int (getTaille typeRet) ^ ") " ^ string_of_int taille_params ^ "\n"
    end
    | _ -> failwith("Erreur interne")


let analyser (AstPlacement.Programme (fonctions,prog)) =
  getEntete() ^ 
  String.concat "" (List.map analyse_code_fonction fonctions) ^ "\n" ^
  "main\n" ^
  analyse_code_bloc prog ^
  "HALT"

end