Inductive foo : Type :=
| Foo : bar -> foo
with bar : Type :=
| Bar : foo -> bar.
