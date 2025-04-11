(** Type monadique : représente une computation qui prend un joueur et renvoie un résultat + un joueur modifié *)
type 'a t = Joueur.t -> 'a * Joueur.t

(** Injecte une valeur dans la monade *)
val return : 'a -> 'a t

(** Chaîne deux opérations monadiques *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** Syntaxe let* pour plus de lisibilité *)
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

(** Récupère le joueur courant dans la monade *)
val get : Joueur.t t

(** Met à jour le vector_velocity du joueur *)
val set_vector_velocity : float * float -> unit t

val add_vector_velocity : float * float -> unit t

val airb : bool -> unit t

val deplacement : float * float -> unit t

val set_grappin : bool -> float * float -> unit t

(** Définit les points de vie du joueur *)
val set_health : int -> unit t

(** Modifie les points de vie du joueur en les incrémentant ou décrémentant *)
val add_health : int -> unit t

(** Définit le carburant du jetpack *)
val set_carburant : int -> unit t

(** Modifie le carburant du jetpack (positif ou négatif) *)
val add_carburant : int -> unit t
