# lambda-pie

Implementation of the simply typed lambda calculus λ->, System F λ2, and dependently typed λ-calculus λΠ.

### How to Use

#### Compile

```
stack build
```

#### Run

```
stack run simply
```

or

```
stack run dependently
```

or

```
stack run systemf
```

#### Typing in the REPLs
- λ is typed as `λ` or `\` or `lambda`
- Λ is typed as `/\`
- type arguments in the System F are wrapped in the brackets like: `[Nat]`
- to introduce either a type or a term of certain type to the system you use `assume` like: `assume Bool :: *`

### Simply Typed Lambda Calculus (λ->)

#### Declare Types of variables

To declare that some identifier has a type T you simply `assume ident :: T`.

You then need to declare that type `T` is of kind `*` as `assume T :: *`.

Example with identity function:

```
λ-> >> assume (id :: T -> T) (T :: *) (a :: T) (b :: T)
λ-> >> id a
       (id a) :: T
λ-> >> id b
       (id b) :: T
```

### System F
```
REPL for λ2

λ2 >> (/\ T . (\ (t :: T) -> t))
       <type lambda> :: (forall T . (T -> T))
```
