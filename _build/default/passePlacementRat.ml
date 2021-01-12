(* Module de la passe de typage *)
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Ast
  open AstPlacement
  open Type

  type t1 = AstType.programme
  type t2 = AstPlacement.programme

  let rec analyse_placement_instruction i base reg =
  match i with
  | AstType.Declaration (_, info) -> 
  begin
    match info_ast_to_info info with
    | InfoVar (_, t, _, _) -> 
      (modifier_adresse_info base reg info);
      (getTaille t)
    | _ -> failwith("Erreur interne : passe de typage incorrecte")
  end
  | AstType.Conditionnelle (_, t, e) ->
  begin
    analyse_placement_bloc t base reg;
    analyse_placement_bloc e base reg;
    0
  end
  | AstType.TantQue (_, b) ->
  begin
    analyse_placement_bloc b base reg;
    0
  end
  | _ -> 0

  and analyse_placement_bloc li base reg = 
  let aux base i = base + (analyse_placement_instruction i base reg) in
  match List.fold_left aux base li with
  | _ -> ()

  let analyse_placement_parametre info base = 
  match info_ast_to_info info with
  | InfoVar (_, t, _, _) ->
  begin
    modifier_adresse_info (base-(getTaille t)) "LB" info;
    getTaille t;
  end
  | _ -> failwith("Erreur interne : parameter not found")
    
  let rec analyse_placement_parametres lp = 
    match lp with 
    | [] -> 0
    | t::q ->
      let tailleq = analyse_placement_parametres q in
      let taillet = analyse_placement_parametre t (- tailleq) in
      tailleq + taillet

  let analyse_placement_fonction (AstType.Fonction(n, lp, li, e)) = 
    let _ = analyse_placement_parametres lp in
    analyse_placement_bloc li 3 "LB";
    Fonction(n, lp, li, e)

let analyser (AstType.Programme (fonctions,prog)) =
  let nf = List.map analyse_placement_fonction fonctions in
  analyse_placement_bloc prog 0 "SB";
  Programme (nf, prog)
end