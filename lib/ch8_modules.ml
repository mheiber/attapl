(* shadow abstraction-breaking builtins *)
let ( = ), ( <> ), compare = Int.(equal, (fun n1 n2 -> not (equal n1 n2)), compare)

(** examples from section 8.1 *)
module S8_1 = struct
  (* Note that in the `Syntax` section there is a typo. Should be
    "`m.x` is an external reference to the value component of m labeled x" *)

  (*
    corresponds to this in syntax of
    Advanced Types and Programming Languages (ATTAPL):

    ```
    module m = mod {
        type X = Nat
        val x = 5
    }
    ```
    *)
  module M = struct
    type x = int

    let x = 5
  end

  (* 
       In ATTAPL syntax: 
       ```
       module n = mod {
           type X = forall W: *. W * W
           val f = forall y: X(Nat). plus y.1 y.2
       }
       ```
   *)
  module N = struct
    type 'w x = 'w * 'w

    let f (y : int x) = fst y + snd y
  end

  (*
       In ATTAPL syntax: 
       ```
        signature I = sig {
            type X = Nat
            val x: X
        }
       ```
    *)
  module type I = sig
    type x = int

    val x : x
  end

  module _ : I = M

  module type J = sig
    type 'w x = 'w * 'w

    val f : int x -> int
  end

  module _ : J = N
end

(* section 8.2 *)
(** examples from section 8.1 *)

module S8_2 = struct
  module M = struct
    type x = int

    let x = 5
  end

  module N = struct
    type 'w x = 'w * 'w

    let f (y : int x) = fst y + snd y
  end

  module type I = sig
    type x = int

    val x : x
  end

  module _ : I = M

  module type J = sig
    type 'w x = 'w * 'w

    val f : int x -> int
  end

  module _ : J = N
end

(* section 8.2 *)

module S8_2_b = struct
  module type I = sig
    type t = int
    type u = int * int

    val x : u
  end

  module M : I = struct
    type t = int
    type u = int * int

    let x : t * t = 3, 4
  end

  (*
Background notes for reading group

## Subsumption rule

x: T   T <: U
----------------
x: U

if x  has type T and T is a subtype of U
---------------------then
    x has type U

important for modules for information hiding:
for `module m: I`, only the names in `I` are exposed.
"factory pattern" in OOP can similarly use
subsumption for information-hiding

```
        class A {
           class Hidden {...}
           static method create(): SomeInterface {
                return new Hidden()
           }

        }
```

## structural vs. nominal subptying

> For reading group: much more about this in Types and Programming Languages (TAPL)

### OO-style nominal vs structural subtyping:

interface I1 { val x : int }
class A implements I1 { val x : int = 3}
interface I2 { val x : int }

x : I1 = new A() // OK
y : I2 = new A() // Error with nominal subtyping, OK with structural

### modules:

Although I don't know of any
programming langauges with nominal subtyping for modules,
the following code would error with nominal and be OK with structural.
Note the new `implements` keyword.

```
signature I = sig {}

module m: I = mod implements I {} (* OK *)
module n: I = {} (* Error with nominal subtyping, OK with structural *)
```

*)
end

module S8_2b = struct
  module type I = sig
    type t = int
    type u = t * t
    type v = int
  end

  module type J = sig
    type t = int
    type u = int * t
  end

  (*
    That the next functor below typechecks shows I matches J.
    This is similar to the following evidence (using the core language) that type a is a subtype of type b 
        type a = ....
        type b = ...
        let f: a -> b = fun x -> x
    *)
  module Provide_I_Matches_J (M : I) : J = struct
    include M
  end

  (* ex8.2.1 *** Question: How much of the development in the rest of the chapter
       can be carried out in a nominal setting?

       Answer: (WIP, really this question should come last)
       - The "Principal Signatures" section says that with nominal subtyping 
       the inferred signature won't in general be the smallest signature in the subtyping hierarchy.
       But I thought that with nominal subtyping, all subtyping relationship have to be listed explicitly ("extends" or "implements" in Java-speak).
        In Java the type of an instance of `class A extends Object { int x = 3 }` is `A`, `Object` and nothing else iirc, with `A being the smallest type.  So what would the "signature" of `module m = mod { val x : int }` be with nominal module typing? If every module induces a signature (just as 
        every class in Java induces a type) then `m : M`. If so, then `M` is the principal type.
        - The sealing, abstract type, and determinism stuff is iuc the same with nominal typing.
        - 

    *)

  (* Demonstrate point about side effects in last example of 8.2.
       This example uses first-class modules, but that's not the point:
     *)
  let eval_count = ref 0

  let example () =
    let module E1 = struct
      let () = incr eval_count
    end
    in
    ()
  ;;

  let%test _ = !eval_count = 0

  let () =
    example ();
    example ()
  ;;

  let%test "fisrt-class module body evaluation" = !eval_count = 2
  (* The same behavior can be illustrated  with functors: *)

  let make_eval_count = ref 0

  module Make (M : sig end) = struct
    let () = incr make_eval_count
  end

  let%test _ = !make_eval_count = 0

  module M = Make ()
  module _ = Make ()

  let%test "functor body evaluation" = !make_eval_count = 2
end

module S8_4 = struct
  (* Interesting definitions: "dependently-typed" here means that type checking requires
       "testing equivalence of runtime expressions" and is mutually exclusive with "statically typed"
    *and* the module language in ATTAPL is merely statically typed due to a restriction from Dreyer+Crary+Harper-2003.

    Aside on this Dreyer+Crary+Harper-2003 citation:
        ATTAPL says that the restriction is to only allow type selection from separable modules. However, neither ATTAPL *nor* D+C+H2003
        use the terminology "separable module." Instead, D+C+H2003 preserve the phase distinction by restricting module type equivalence checks
        to "determinate" modules. Where, in their system, a module is "determinate" iff it is "pure" and a module is "pure" iff it has no
        computational effects, where sealing is (iuc) a computational effect.

    Aside on "phase distinction":
        - From the Cardelli 1988 a "phased" type system is one in which types cannot refer to non-compile-time values. ATTAPL
        says that semantically dependent types don't "respect the phase distinction."
        Cardelli shows how to have phased dependent types, but (importantly) this is apparent contradition with ATTAPL is only
        because, afaict, Cardelli's dependent types are syntactic dependent types, and the phased types Cardelli allows
        can only refer to compile-time values and so are not dependent types in the sense of ATTAPL.
        - ATTAPL says "phase distinction" was introduced by Cardelli but that ATTAPL uses the term in the sense of Harper+Mitchell+Moggi1990.
        However the HMM1990 says they are using the terminology in the sense of the Cardelli with the added note that they name the two phases "compile time"
        and "run-time." But Cardelli (in the informal presentation) uses the same terminology (just not hyphenating "run time").
        So there's no difference afaict and ATTAPL could have just said they are using the Cardelli definition.
        - Implications:
            - Cardelli says "The loss of phase distinctions manifests itself as the inability to perform compilation, since compilers are based on a syntactic translation phase which strips out type information, followed by a type-free execution phase."
                - If this were true, then no compiled programming language could have semantically dependent types. But I think we can if we drop the assumption that all types are stripped out in compilation: languages like Idris and Agda have runtime type information. So, in the terminology of TAPL, does a language with semantic dependent types not "respect the phase distinction"? 
    *)

  (* For the reading group: On comparison of modules to existentials: probably best saved for reading once we've gone further with modules, especially sections 8.3 and 8.5 of ATTAPL. Types and Programming Languages 24.0 to 24.2 is short and is a good reading for understanding the comparison.
       Worth doing as a follow-on once we've gone further with modules, imo.
     *)
  (* Exercise 8.1
        

       signature_match K J =def:
        for all components c in J:
            case ty_match: if c is a type then K.c ~< J.c
            or val_match: c is a value and (typeof K.c) ~< (typeof J.c)
        where 
             ~< is an infix operator for type_match
            type_match t1 t2 =def:
                ty1 = ty2 or ty2 is abstract

       check K matches J and check K matches I
       (I assuming K is the module type of module k)


        K matches J by signature_match:
            K.X ~< J.X by type_match
                unfold K.X, unfold J.X, Nat ~< Nat by refl
            K.Y ~< J.Y by type_match
                unfold each, K.X ~< J.X (previously shown)
            typeof K.c ~< (typeof J.c) by val_match
                unfold (typeof J.c), Nat ~< Nat by refl
            typeof K.f = typeof J.f
                fully unfold each, Nat -> Nat ~< Nat -> Nat by refl


        check K matches I by signature_match:
            K.X ~< I.X by type_match
                fully unfold, X ~< abstract type
            K.Y ~< I.Y by type_match
                fully unfold, show Nat -> Nat ~< abstract -> Nat
                    Nat ~< Nat
                    and Nat ~< abstract
            typeof K.c ~< (typeof J.c) by val_match
                unfold (typeof J.c), Nat ~< abstract
            typeof K.f = typeof J.f
                fully unfold each, show Nat -> Nat ~< Nat -> abstract
                    Nat ~< Nat
                    and Nat ~< abstract
     *)

end

module S8_5 = struct
  (* exercise 8.5.2
       check that M implements I

       pasting for convenience:

          ```
          signature I = sig {
               type X
               type Y = X→Nat
               val c : X
               val f : Y
           }
           ```

       check that M implements I by signature_match:
           M.X ~< I.X
                _ ~< abstract
            M.Y ~< I.Y
                Nat -> Nat  ~< abstract -> Nat
                    Nat ~< Nat
                    and Nat ~< abstract

            typeof M.c ~< typeof I.c
                Nat ~< abstract
            typeof M.f ~< typeof I.f
                fully unfold, Nat -> Nat ~< abstract -> Nat
                    Nat ~< Nat
                    Nat ~< abstract

     *)

  (* 8.5, part on "Determinacy and Abstraction"
       The definition of "representation independence" here is quite vague, which is unfortunate for such an important concept. ATTAPL defines it as:
    "... representation independence ... ensures that the behavior of clients are insulated from the de- tails of the implementation of an abstraction".
    But what is "behavior"? Surely it's OK for a module implementation of a dictionary data structure to switch from lists of tuples to a btree map, which would speed up some clients. I am guessing that doesn't count as a change in "behavior" in the sense of ATAPl.
    As best I can tell by very quickly skimming a few papers, two programs have the same "behavior" if they both diverge or both evaluate to the same value (from an operational point of view). This doesn't account for things like IO and nondeterminism, maybe a full story would involve program traces or something. The Reynolds paper is quite meaty and I didn't glean much from a skim.
    **However** exercise 8.5.5 suggests that representation-independence is about type safety: restricting what programs can be accepted by the type checker to prevent undesired runtime behavior. This is different. Arg!

     *)
  (* Exercise 8.5.3

    module m = {
        type X = Bool
        val x : X = true
        val plus : X -> X -> X = and
    }
    module n = {
        type X = Nat
        val x : X = 1
        val plus : X -> X -> X = ( + )
    }
    let module x1 = if cond then m else n in
    let module x2 = if cond then m else n in
    x2.plus x2.x x1.x (* not type-correct, may apply a nat operation to a bool argument *)


     *)
  (** Translated to OCaml. Note that OCaml is a bit different from the ATTAPL language. *)
  module Exercise8_5_3 = struct
    module type S = sig
      type t

      val plus : t -> t -> t
      val x : t

      (* Only used to illustrate runtime behavior *)
      val show : t -> string
    end

    module M = struct
      type t = bool

      let x : t = true
      let plus : t -> t -> t = ( && )
      let show : t -> string = Printf.sprintf "zo %b\n"
    end

    module N = struct
      type t = int

      let x : t = 0
      let plus : t -> t -> t = ( + )
      let show : t -> string = Printf.sprintf "yo %d\n"
    end

    let () =
      let went = ref false in
      let create () : (module S) =
        if not !went
        then (
          went := true;
          (module M))
        else (module N)
      in
      let module M1 = (val create ()) in
      let module M2 = (val create ()) in
      (* Un-comment the next line of code
               to see OCaml's compile-time error.
               Note that OCaml handles this kind of case differently
               from the ATTAPL langauge, there's discussion at the end of chapter 8 on
               the differences.
             *)
      (* let _ = M2.plus M2.x M1.x in *)
      (* Un-comment the next line of code to see OCaml's runtime behavior
               if we tell the type checker to let us ignore the type mismatch.
               On my machine, OCaml prints `1` even though there's no `1` in the source code:
                The raw bytes for `true` print as `1` when we tell the printer to print an int.
                So there's no "runtime type error" here but there is "runtime doing something funky
                due to type mismatch"
             *)
      (* let () = print_endline (M2.show (M2.plus M2.x (Obj.magic M1.x))) in *)
      ()
    ;;
  end

  (* 8.5 part last few paragraphs about determinacy and abstraction
    I don't understand how `m` is determinate:

    copying for convenience:
    ````
    module m = if ....
                then mod { type X = Int }
                else mod { type X = Bool }
    ```
    the text says that it's determinate if sealed with an interface that hides "the type component". The text doesn't say what "determinate" means in general, though. I assume that ATTAPL means "a module expression is determinate if all type components are either known statically or abstract" which is vague but better than nothing.

    But neither `m` example has a seal in the text between 8.5.3 and 8.5.4! The text says that in the second example `m` is determinate even though the `m` in the first `m` example is indeterminate.

    I'm guessing this is a typo and that the second `m` example in the text between 8.5.3 and 8.5.4 was meant to include sealing, like this:

    ```
    signature J = {}
    module m : J = if ....
                then mod { type X = Int }
                else mod { type X = Bool }
    ```

    In that case I see what ATTAPL means by `m` being determinate even though the conditional is not, since the binding forces determinacy. And then I also think I understand 8.5.4

    I'm still not totally sure which of these two interpretations of ATTAPL is accurate:
    - the type checker forbids indeterminate module bindings in user code: the code above with unsealed `m` would be a type error
    - the type checker auto-seals on behalf of the user. The source code for `m` above (without sealing in the source code) is elaborated to something like the second version, which seals `m` with `J`.

     *)
  (* Exercise 8.5.4

    Copying the solution from the back of the book:

    "Solution: In a call-by-name setting, variables may no longer be considered determinate, because they stand for unevaluated module expressions. Therefore we cannot “determinize” an indeterminate module expression by binding it to a variable, with the result that there is no way to use its type components."

    Hay was made of the phase distinction, so what does the call-by-name evaluation strategy have to do with the type-checking strategy for module types? I don't see how a call-by-name evaluation strategy is incompatible with a typing judgment that says that in a conditional all branches must match the same module type.

     *)
  (* Exercise 8.5.4

    "Show that if sealed modules were determinate, then representation independence could be violated. That is, find a well-typed term t whose type correctness is destroyed by replacing one implementation of an abstract type with another."

    iuc we did this already for exercise 8.5.3 the well-typed term is
    x2.plus x2.x x1.x

    where x1 and x2 implement the same interface but have distinct implementations for an abstract type component of the interface.

    ```
     *)
  (* Exercise 8.5.6
    "Why would it be bad for two copies of M:>I to induce interchangeable abstract type components?"

    The only examples I can come up with involve impurity. For example, if a module body generates a random unique id and then compares inputs against the unique id then the behavior will diverge between different copies.

    The solution in the back of the book involves copies of a module hashing things differently, which is pretty similar.

    *)
  (* Exercise 8.5.7
    Two implementations of `Point` in terms of 2-tuples
    could differ on which tuple slot was for the `x` value and which is
    for the `y` value. Conflating their abstract types could lead
    to silly behavior like translating in the x plane having no effect.

    A more concise example is a counter: one implementation uses
    negative ints, the other uses positive ints, and the get_magnitude
    function gives the expected result as long as abstract types are not conflated.

    Here's the Point example in OCaml:
     *)
  module Exercise8_5_7 = struct
    module type Point = sig
      type t

      val origin : t
      val translate_x : int -> t -> t
      val get_x : t -> int
      (* val translate_y : int -> t -> t *)
      (* omitted for brevity *)
      (* val get_y : t -> int *)
      (* omitted for brevity *)
    end

    module Point_v1 : Point = struct
      type t = int * int (*  (x, y)  *)

      let origin = 0, 0
      let get_x (x, _) = x
      let translate_x amount (x, y) = x + amount, y
    end

    module Point_v2 : Point = struct
      type t = int * int (*  (y, x)  *)

      let origin = 0, 0
      let get_x (_, x) = x
      let translate_x amount (y, x) = y, x + amount
    end

    let%test "what happens when abstract types conflated" =
      let translate_amount = 1 in
      let origin = Point_v1.origin in
      let x =
        origin
        |> Point_v1.translate_x translate_amount
        (* tell type-checker to let us lie about the type *)
        |> (Obj.magic Point_v2.get_x : Point_v1.t -> int)
      in
      x <> translate_amount
    ;;
  end

  (* Exercise 8.5.8
    I'll just explain how to do this.
    We need the module body to be evaluated multiple times,
    and the only way to do that so far is with a module expression.
    And we want differences in behavior, so we need a side effect.

    Let module body evaluation generate a random seed, and use that
    seed in the hash function.
    Then the same value may hash differently 
    for the two modules, producing weird behavior such
    as not finding a value for a key you just inserted or getting back
    a value for the wrong key.


    ```
    type key = string
    type value = int

    signature S = sig {
        type t
        val set : t -> key -> value -> unit
        val get : t -> key -> value
    }

    create = lambda x: unit.
        module m : S = mod {
            ...
            val seed = create_random_seed ()
            val hash_fun key = ... seed ...
            val get t key =
                let hash = hash_fun key in 
                ...
        }

     *)

  (* Notes on the avoidance problem

    I found the explanation from counterexamples.org more helpful the book: https://counterexamples.org/avoidance.html

    *)

  (* Exercise 8.5.9

    Question copied and adapted for convenience

    Consider signature I that closes over local module variable `m` and therefore
    must avoid `m`:

    ```
    signature I = sig {
        type X = λW:*. m.Z
        type Y = m.Z
    }
    ```
    Q: Show that I has infinitely many super-signatures that avoid `m` but no least super-signature.

    I didn't find the clever solution that's in the back of the book on my own. It takes advantage of the specific structure of the example.

    Q extra credit:
        Find a similar example where the core language is full F_sub.


    Finding a smallest super-signature is for core language fsub is often easier, since we can use Top type for covariant positions and Bottom type for contravariant positions.

    Avoiding `m`:


    ```
    sig { val f = m.Z -> m.Z } matches smallest sig { val f = Bottom -> Top }
    ```
    TODO: I give up. An answer of sorts is probably in Ghelli+Pierce 97

    ````
     *)
end

let () = print_endline "done"
