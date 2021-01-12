(* Module de la passe de typage *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstType
  open Type

  type t1 = AstTds.programme
  type t2 = AstType.programme

  (* analyse_type_expression : AstTds.expression -> AstType.expression *)
  (* Paramètre e : l'expression à analyser *)
  (* Vérifie le bon typage et tranforme l'expression
  en une expression de type AstType.expression *)
  (* Retourne la nouvelle expression et son type sous la forme (e, typ) *)
  (* Erreur si mauvais typage *)
  let rec analyse_type_expression e = 
    match e with
      | AstTds.AppelFonction (linfo, le) ->
        begin
          let l = List.map analyse_type_expression le in
          let nle = List.map fst l in
          let ltype = List.map snd l in
          (* Vérifie si la fonction est compatible avec les paramètres *)
          (* Retourne l'appel de fonction correspondant à la fonction si celle ci est compatible *)
          (* Retourne None sinon *)
          let signature_compatible info = 
            match info_ast_to_info info with 
              | InfoFun (_, _, typeParams) -> (est_compatible_list ltype typeParams)
              | _ -> failwith("Erreur interne : passe de résolution des identifiants invalide")
            in
            (* On cherche la bonne fonction parmi les différentes variantes *)
            match List.find_opt signature_compatible linfo with
            | Some info -> 
              begin
                match info_ast_to_info info with
                | InfoFun (_, typeRet, _) -> AppelFonction (info, nle), typeRet
                | _ -> failwith("Erreur interne : passe de résolution des identifiants invalide")
              end
            | None -> raise(TypesParametresInattendus(ltype, ltype)) (* TODO : modifier l'exception pour prendre en parametre touts les types de la surcharge *)
            
        end
      | AstTds.Rationnel (e1, e2) -> 
        begin
          let (ne1, t1) = analyse_type_expression e1 in
          let (ne2, t2) = analyse_type_expression e2 in
          match t1 with
            | Int -> 
            begin
              match t2 with
                | Int -> (Rationnel(ne1, ne2), Rat)
                | _ -> raise(TypeInattendu(t2, Int))
            end
            | _ -> raise(TypeInattendu(t1, Int))
        end
      | AstTds.Numerateur e1 ->
        begin
          let (ne1, t1) = analyse_type_expression e1 in
            match t1 with
              | Rat -> (Numerateur(ne1), Int)
              | _ -> raise(TypeInattendu(t1, Rat))
        end
      | AstTds.Denominateur e1 -> 
        begin
          let (ne1, t1) = analyse_type_expression e1 in
          match t1 with
            | Rat -> (Denominateur ne1, Int)
            | _ -> raise(TypeInattendu(t1, Rat))
        end
      | AstTds.Ident info ->
        begin
          match info_ast_to_info info with
            | InfoVar (_, t, _, _) -> (Ident info, t)
            | InfoConst(_, _) -> (Ident info, Int)
            | _ -> failwith("Erreur interne : symbol not found")
        end
      | AstTds.Binaire (b, e1, e2) ->
        begin
          let (ne1, t1) = analyse_type_expression e1 in
          let (ne2, t2) = analyse_type_expression e2 in
          match t1, b, t2 with
            | Int, AstSyntax.Plus, Int -> (Binaire(PlusInt, ne1, ne2), Int)
            | Rat, AstSyntax.Plus, Rat -> (Binaire(PlusRat, ne1, ne2), Rat)
            | Int, AstSyntax.Equ, Int -> (Binaire(EquInt, ne1, ne2), Bool)
            | Bool, AstSyntax.Equ, Bool -> (Binaire(EquBool, ne1, ne2), Bool)
            | Int, AstSyntax.Mult, Int -> (Binaire(MultInt, ne1, ne2), Int)
            | Rat, AstSyntax.Mult, Rat -> (Binaire(MultRat, ne1, ne2), Rat)
            | Int, AstSyntax.Inf, Int -> (Binaire(Inf, ne1, ne2), Bool)
            | _ -> raise(TypeBinaireInattendu(b, t1, t2))
        end
      | AstTds.True -> (True, Bool)
      | AstTds.False -> (False, Bool)
      | AstTds.Entier(n) -> (Entier n, Int)

  let rec analyse_type_instruction i = 
    match i with
      | AstTds.Declaration(t, e, info) ->
      begin
        let (ne, te) = analyse_type_expression e in 
        if (est_compatible te t) then 
          begin
          modifier_type_info t info;
          Declaration(ne, info)
          end
        else raise(TypeInattendu(te, t))
      end
      | AstTds.Affectation(e, info) ->
      begin
        let (ne, te) = analyse_type_expression e in
        match info_ast_to_info info with
          | InfoVar(_, t, _, _) -> 
            if (est_compatible te t) then Affectation(ne, info)
            else raise(TypeInattendu(te, t))
          | InfoConst(n, _) -> raise(MauvaiseUtilisationIdentifiant n)
          | InfoFun(n, _, _) -> raise(MauvaiseUtilisationIdentifiant n)
      end 
      | AstTds.Affichage(e) ->
      begin
        let (ne, te) = analyse_type_expression e in
        match te with 
          | Int -> AffichageInt ne
          | Bool -> AffichageBool ne
          | Rat -> AffichageRat ne 
          | _ -> failwith("Erreur interne : Affichage d'une variable Undefined")
      end
      | AstTds.Conditionnelle(c, t, e) ->
      begin
        let (nc, tc) = (analyse_type_expression c) in
        if (est_compatible tc Bool) then 
        begin
          let nt = (analyse_type_bloc t) in
          let ne = (analyse_type_bloc e) in
          Conditionnelle(nc, nt, ne)
        end
        else raise(TypeInattendu(tc, Bool))
      end
      | AstTds.TantQue(c, b) ->
      begin
        let (nc, tc) = analyse_type_expression c in
        if (est_compatible tc Bool) then 
        begin
          let nb = analyse_type_bloc b in
          TantQue(nc, nb)
        end
        else raise(TypeInattendu(tc, Bool))
      end
      | AstTds.Empty -> Empty 

  and analyse_type_bloc b = List.map analyse_type_instruction b

  let analyse_type_fonction (AstTds.Fonction(t, info, lp, li, e))  = 
    match info_ast_to_info info with
    | InfoFun _ -> 
    begin
      let nli = analyse_type_bloc li in
      let (ne, te) = analyse_type_expression e in
      let nlp = List.map snd lp in
      if (est_compatible te t) then Fonction(info, nlp, nli, ne)
      else raise(TypeInattendu(te, t))
    end
    | _ -> failwith("Erreur interne : erreur lors de la passe TDS")

let analyser (AstTds.Programme (fonctions,prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  Programme (nf,nb)
end