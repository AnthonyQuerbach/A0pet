(** Other Utilities *)

(*****************************)
(*****************************)

(**clamps [value] to be between [min] and [max] inclusive. Requires [value] [min],
  and [max] to be the same type. Functions has no side-effects and returns the
  clamped value which has the same type as the input*)
let clamp min max value =
  if value < min then min else if value > max then max else value

(**clamps [value] to be between 0 and 100 inclusive. Requires [value] to be an
  integer. Functions has no side effects and returns a clamped integer*)
let clamp0To100 value = clamp 0 100 value

(**Continuously prompts the user for a numerical input until a valid integer
  input is provided. Side effects: prints prompts to the command line and
  recieves inputs from the command line. Halts program progress until a valid 
  input is provided. Returns a valid input as an integer. No preconditions.  *)
let rec getInputNumber () =
  print_endline "Please enter your lucky number[Enter only a number]:";

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



(** Pet Record and Utilities *)
(*****************************)
(*****************************)


(**Pet record that stores [name], [fullness], [happiness], and [energy]*)
type pet = {
  name : string;
  fullness : int;
  happiness : int;
  energy : int;
}
(**Takes in parameter [pet] : pet, and returns the wellness score : int 
which is the mean of pet's fullness, happiness, and energy. No pre-conditions 
or side effects. *)
let wellness pet = (pet.fullness + pet.happiness + pet.energy) / 3

(**Takes in parameter [pet] : pet and returns a string description of the 
[pet]'s mood based on its fullness, energy, happiness, and wellness. No side 
effects or pre-conditions. *)
let mood pet =
  if pet.fullness <= 20 then "hungry"
  else if pet.energy <= 20 then "sleepy"
  else if pet.happiness <= 20 then "lonely"
  else if wellness pet >= 75 then "delighted"
  else "content"


  (**Takes in parameter [mood] : string. Pre-conditions: [mood] must be "hungry", 
  "sleepy", "lonely", "delighted", or "content". Returns a string description
  of what happens when something is of that mood. No side effects. *)
let narrative mood =
  if mood = "hungry" then "your cat meows for food"
  else if mood = "sleepy" then "your cat yawns tiredly"
  else if mood = "lonely" then "your cat is so lonely :("
  else if mood = "delighted" then "your cat is so well cared for and loved"
  else "your cat is doing alright."

  (**Takes in [pet] : pet and then prints a long description of the [pet]'s 
  statistics, mood, and narrative based on their stats. No side effects 
  besides printing. *)
let printPet pet =
  print_endline
    ("\n\n\n    Your pet is named " ^ pet.name ^ ", and has "
   ^ string_of_int pet.fullness ^ " fullness, "
    ^ string_of_int pet.happiness
    ^ " happiness, and " ^ string_of_int pet.energy
    ^ " energy. Your pet's wellness score is at "
    ^ string_of_int (wellness pet)
    ^ ". Your pet's mood is " ^ mood pet ^ ". "
    ^ narrative (mood pet))


    (** Takes in parameter [pet] : pet, and creates then returns a new pet 
with the same stats as [pet], but modifying stats +25 fullness, +5 happiness, 
and -5 energy, all clamped from 0 to 100. No preconditions. *)
let feed pet =
  print_endline "You feed your pet \n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness + 25;
    happiness = clamp0To100 pet.happiness + 5;
    energy = clamp0To100 pet.energy - 5;
  }

    (** Takes in parameter [pet] : pet, and creates then returns a new pet 
with the same stats as [pet], but modifying stats -10 fullness, +20 happiness, 
and -15 energy, all clamped from 0 to 100. No preconditions. *)
let play pet =
  print_endline "You play with your pet \n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness - 10;
    happiness = clamp0To100 pet.happiness + 20;
    energy = clamp0To100 pet.energy - 15;
  }

    (** Takes in parameter [pet] : pet, and creates then returns a new pet 
with the same stats as [pet], but modifying stats -10 fullness, -5 happiness, 
and +25 energy, all clamped from 0 to 100. No preconditions. *)
let nap pet =
  print_endline "Your pet naps \n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness - 10;
    happiness = clamp0To100 pet.happiness - 5;
    energy = clamp0To100 pet.energy + 25;
  }

    (** Takes in parameter [pet] : pet, and creates then returns a new pet 
with the same stats as [pet], but modifying stats -15 fullness, +30 happiness, 
and -25 energy, all clamped from 0 to 100. No preconditions. *)
let chinScratch pet =
  print_endline "You scratch your pet's chin\n";
  {
    name = pet.name;
    fullness = clamp0To100 pet.fullness - 15;
    happiness = clamp0To100 pet.happiness + 30;
    energy = clamp0To100 pet.energy - 25;
  }

  (**Takes in parameters [eventNum] : int and [userPet] : pet. Pre-condition: 
  [eventNum] must be between 0 and 4 inclusive. Based on what the [eventNum] is,
  an event will happen that creates a new pet with the same or similar but 
  slightly modified stats, and then returns that new pet. Side effect: prints
  a message to the console describing the event. *)
let randomEvent eventNum userPet =
  if eventNum = 0 then (
    print_endline
      ("Nothing unusual happened this turn to " ^ userPet.name
     ^ ". They are just a very cute cat\n");
    userPet)
  else if eventNum = 1 then (
    print_endline
      (userPet.name ^ " found a snack! Your cat loved the cat treats");
    { userPet with fullness = clamp0To100 userPet.fullness + 10 })
  else if eventNum = 2 then (
    print_endline
      (userPet.name ^ " got the zoomies. Your cat races around the house");
    {
      userPet with
      happiness = clamp0To100 userPet.happiness + 15;
      energy = clamp0To100 userPet.energy - 10;
    })
  else if eventNum = 3 then (
    print_endline
      (userPet.name
     ^ " took a sudden nap. Although as a cat, maybe it wasn't so sudden");
    {
      userPet with
      fullness = clamp0To100 userPet.fullness - 5;
      energy = clamp0To100 userPet.energy + 15;
    })
  else if eventNum = 4 then (
    print_endline
      (userPet.name
     ^ " experienced a minor mishap. Cats sometimes fall off the kitchen \
        counters");
    { userPet with happiness = clamp0To100 userPet.happiness - 10 })


    (*execcution should never reach this path*)
  else userPet

  (**Prints a list of options. No paramters or pre-conditions. *)
let printOptions () =
  print_endline
    "\n\
     You have these options[enter only a single number 1-6]: \n\n\
     1. Feed your pet \n\n\
     2. Play with your pet \n\n\
     3. Let your pet nap \n\n\
     4. Scratch your cat's chin \n\n\
     5. Check on your pet \n\n\
     6. End the day early\n\n"

(** Pet Record and Utilities *)
(*****************************)
(*****************************)




(**Takes in parameter [pet] : pet and ends the day using [pet]'s properties. 
No preconditions, but major side effects: prints various messages to the 
command line and also exits the program. Every execution of this function
terminates the program. *)
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

  
let () = print_endline "Title: Pet Project \n"
let () = print_endline "Welcome, your pet is a cat, because I like cats"
let () = print_endline "Please enter your cat's name [text]: \n"
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
  else print_endline ("\nTurn " ^ string_of_int turn);
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
  let randomInt = Random.int 5 in
  let userPet = randomEvent randomInt userPet in
  main_loop userPet.name (turn + 1) userPet.fullness userPet.happiness
    userPet.energy

let _ = main_loop input 1 50 50 50
