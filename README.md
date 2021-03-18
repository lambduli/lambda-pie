# lambda-pie

Implementation of the simply typed lambda calculus λ-> and dependently typed λ-calculus λΠ.

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