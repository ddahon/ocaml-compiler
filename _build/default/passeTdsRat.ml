(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open Type

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme


(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = 

  match e with
    | AstSyntax.AppelFonction (id, le) ->
        begin
          let verifier_bonne_utilisation info = 
            match info_ast_to_info info with
              | InfoFun _ -> info
              | _ -> raise (MauvaiseUtilisationIdentifiant (id))
          in
          match chercherGlobalementFonction tds id with
            | Some linfo -> AppelFonction(List.map verifier_bonne_utilisation linfo, List.map (analyse_tds_expression tds) le)
            | _ -> raise (IdentifiantNonDeclare (id))
        end
    | AstSyntax.Ident n ->
        begin
          match chercherGlobalement tds n with
            | Some info ->
              begin
                match info_ast_to_info info with
                  | InfoVar _ ->
                    (Ident info)
                  | InfoConst (_, v) ->
                    (Entier v)
                  | _ -> raise (MauvaiseUtilisationIdentifiant (n))
              end
            | None -> raise (IdentifiantNonDeclare (n))
        end
    | AstSyntax.Rationnel (e1, e2) ->
      Rationnel (analyse_tds_expression tds e1, analyse_tds_expression tds e2)
    | AstSyntax.Binaire (op, e1, e2) ->
      Binaire (op, analyse_tds_expression tds e1, analyse_tds_expression tds e2)
    | AstSyntax.Numerateur e ->
      Numerateur (analyse_tds_expression tds e)
    | AstSyntax.Denominateur e ->
      Denominateur (analyse_tds_expression tds e)
    | AstSyntax.True ->
      True
    | AstSyntax.False ->
      False
    | AstSyntax.Entier entier ->
      Entier (entier) 

(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
            Declaration (t, ne, ia) 
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale, 
            il a donc déjà été déclaré dans le bloc courant *) 
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (n,e) ->
      begin
        match chercherGlobalement tds n with
        | None -> 
          (* L'identifiant n'est pas trouvé dans la tds globale. *) 
          raise (IdentifiantNonDeclare n)
        | Some info -> 
          (* L'identifiant est trouvé dans la tds globale, 
          il a donc déjà été déclaré. L'information associée est récupérée. *) 
          begin
            match info_ast_to_info info with
            | InfoVar _ -> 
              (* Vérification de la bonne utilisation des identifiants dans l'expression *)
              (* et obtention de l'expression transformée *) 
              let ne = analyse_tds_expression tds e in
              (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information 
              et l'expression remplacée par l'expression issue de l'analyse *)
               Affectation (ne, info)
            |  _ ->
              (* Modification d'une constante ou d'une fonction *)  
              raise (MauvaiseUtilisationIdentifiant n) 
          end
      end
  | AstSyntax.Constante (n,v) -> 
      begin
        match chercherLocalement tds n with
        | None -> 
        (* L'identifiant n'est pas trouvé dans la tds locale, 
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Ajout dans la tds de la constante *)
        ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
        Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e -> 
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li,e)) = 
  let definir_fonction () = 
    (* Création d'une tds fille complémentaire à la mère pour les paramètres de la fonction *)
    let tds_fille = creerTDSFille maintds in
    (* Ajout des paramètres dans la tds *)
    let ajout_param tds (t, n) = 
        match chercherLocalement tds n with
          | Some _ -> raise (DoubleDeclaration n) (* Paramètre déjà déclaré *)
          | None -> 
            (*  L'identifiant n'existe pas encore : on peut l'ajouter *)
            ajouter tds n (info_to_info_ast (InfoVar(n, t, 0, "")));      
    in 
    List.iter (ajout_param tds_fille) lp;

    (* Ajout de la fonction dans la tds *)
    let typeliste = List.map fst lp in
    let nom = info_to_info_ast (InfoFun(n, t, typeliste)) in
    ajouter maintds n nom;
    
    (* Analyse du corps de la fonction *)
    let corps = List.map (analyse_tds_instruction tds_fille) li in
    (* Création de l'objet AstTds.Fonction *)
    let param_to_info (t, id) = (t, (info_to_info_ast (InfoVar(id, t, 0, "")))) in 
    let params = List.map param_to_info lp in
    let expression_retour = analyse_tds_expression tds_fille e in
    Fonction(t, nom, params, corps, expression_retour)
    in
  match chercherGlobalementFonction maintds n with
    | Some linfo -> begin
      
      let est_deja_declaree info = 
        match info_ast_to_info info with
        | InfoFun(_, _, lt) -> (est_compatible_list lt (List.map fst lp))
        | _ -> failwith "Erreur interne"  
      in
      if (List.exists est_deja_declaree linfo) then raise (DoubleDeclaration n)
      else definir_fonction() (* Fonction pas encore déclarée avec les types donnés *)
    end
    | None -> definir_fonction() (* Fonction pas encore déclarée *)
      

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds prog in
  Programme (nf,nb)

end
