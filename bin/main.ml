(** Other Utilities *)
(*****************************)
(*****************************)

let clamp min max value =
  if value < min then min else if value > max then max else value

let clamp0To100 value = clamp 0 100 value

let rec getInputNumber () =
  print_endline "Please enter your lucky number";

  let opt =
    let luckyInput = read_line () in
    int_of_string_opt luckyInput
  in
  match opt with
  | None ->
      print_endline "Invalid input. \n";
      getInputNumber ()
  | Some n -> n

(** Other Utilities *)

(*****************************)
(*****************************)

(*****************************)
(*****************************)
type pet = {
  name : string;
  fullness : int;
  happiness : int;
  energy : int;
}
(** Pet Record and Utilities *)

let wellness pet = (pet.fullness + pet.happiness + pet.energy) / 3

let mood pet =
  if pet.fullness <= 20 then "hungry"
  else if pet.energy <= 20 then "sleepy"
  else if pet.happiness <= 20 then "lonely"
  else if wellness pet >= 75 then "delighted"
  else "content"

let narrative mood =
  if mood = "hungry" then "your cat meows for food"
  else if mood = "sleepy" then "your cat yawns tiredly"
  else if mood = "lonely" then "your cat is so lonely :("
  else if mood = "delighted" then "your cat is so well cared for and loved"
  else "your cat is doing alright."

let printPet pet =
  print_endline
    ("Your pet is named " ^ pet.name ^ ", and has " ^ string_of_int pet.fullness
   ^ " fullness, "
    ^ string_of_int pet.happiness
    ^ " happiness, and " ^ string_of_int pet.energy
    ^ " energy. Your pet's wellness score is at "
    ^ string_of_int (wellness pet)
    ^ ". Your pet's mood is " ^ mood pet ^ ". "
    ^ narrative (mood pet))

let feed pet =
  print_endline "You feed your pet \n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness + 25;
    happiness = clamp0To100 pet.happiness + 5;
    energy = clamp0To100 pet.energy - 5;
  }

let play pet =
  print_endline "You play with your pet \n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness - 10;
    happiness = clamp0To100 pet.happiness + 20;
    energy = clamp0To100 pet.energy - 15;
  }

let nap pet =
  print_endline "Your pet naps \n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness - 10;
    happiness = clamp0To100 pet.happiness - 5;
    energy = clamp0To100 pet.energy + 25;
  }

let chinScratch pet =
  print_endline "You scratch your pet's chin\n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness - 15;
    happiness = clamp0To100 pet.happiness + 30;
    energy = clamp0To100 pet.energy - 25;
  }

let printOptions () =
  print_endline
    "\n\
     You have these options: \n\n\
     1. Feed your pet \n\n\
     2. Play with your pet \n\n\
     3. Let your pet nap \n\n\
     4. Scratch your cat's chin \n\n\
     5. Check on your pet \n\n\
     6. End the day early"

(** Pet Record and Utilities *)
(*****************************)
(*****************************)

let endDay pet =
  print_endline "The day has ended. Final Status Report: \n";
  printPet pet;
  if pet.fullness = 0 || pet.energy = 0 || pet.happiness = 0 then
    print_endline "Your pet needs immediate tender loving care"
  else if wellness pet >= 75 then
    print_endline
      "your cat is thriving so much chasing mice and climbing trees, it \
       becomes a lion"
  else if wellness pet >= 55 then
    print_endline
      "your pet is doing well, ascending to become the prized cat of Cornell"
  else if wellness pet >= 35 then
    print_endline
      "your pet is frazzled, running away and becoming someone else's cat"
  else
    print_endline "your pet is neglected horribly, and vanishes away from you";
  exit 0

let () = print_endline "Welcome, your pet is a cat, because I like cats"
let () = print_endline "Please enter your cat's name: \n"
let input = read_line ()
let _ = Random.init (getInputNumber ())
let userPet = { name = input; fullness = 50; happiness = 50; energy = 50 }
let () = printPet userPet

(** [main_loop name turn fullness happiness energy] runs the virtual-pet
    simulation beginning on turn [turn], with the pet named [name] having the
    given attribute values. *)
let rec main_loop name turn fullness happiness energy =
  let userPet = { name; fullness; happiness; energy } in

  if turn > 5 then endDay userPet
  else print_endline ("Turn " ^ string_of_int turn);
  printOptions ();

  let option = read_line () in
  let userPet =
    if option = "1" then feed userPet
    else if option = "2" then play userPet
    else if option = "3" then nap userPet
    else if option = "4" then chinScratch userPet
    else if option = "5" then (
      printPet userPet;

      main_loop userPet.name turn userPet.fullness userPet.happiness
        userPet.energy
      (*this functions as an early return*))
    else if option = "6" then endDay userPet (*userPet*)
    else (
      print_endline "invalid input. \n";
      main_loop userPet.name turn userPet.fullness userPet.happiness
        userPet.energy)
    (*this functions as an early return*)
  in
  main_loop userPet.name (turn + 1) userPet.fullness userPet.happiness
    userPet.energy

let _ = main_loop input 1 50 50 50
