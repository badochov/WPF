module type COUNTER = sig
  (* typ licznika *)
  type counter

  (* zwraca nowy licznik ustawiony na 0 *)
  val make : unit -> counter

  (* zwiększa dany licznik o jeden *)
  val inc : counter -> int

  (* resetuje wszystki liczniki *)
  val reset : unit -> unit
end

module Counter : COUNTER = struct
  type counter = int ref

  (* trzyma liste referencji do niezerowych liczników *)
  let nonzero = ref []

  (* unit bo inaczej to była by zawsze ta sama referencja *)
  let make () = ref 0

  (* zwiększa licznik jeśli to jego pierwsze zwiększenie wrzuca go do listy
     niezerowych liczników *)
  let inc c =
    if !c = 1 then nonzero := c :: !nonzero;
    incr c;
    !c


  (* zeruje wszystkie niezerowe liczniki *)
  let reset () =
    List.iter (fun r -> r := 0) !nonzero;
    nonzero := []
end
