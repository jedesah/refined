# Dependent refinement

```tut:silent
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.string.StartsWith
import shapeless.tag.@@
import shapeless.Witness
```

Scala's path dependent types makes it possible to express refinements
that depend other statically known values:

```tut
def foo[S <: String](a: S)(b: String @@ StartsWith[a.type]) = a + b
```

```tut
foo("ab")("abcd")
```

```tut:fail
foo("cd")("abcd")
```

Unfortunately Scala does not allow to use singleton types of `AnyVal`s,
see [section 3.2.1][spec-3.2.1] of the Scala Language Specification:

```tut:fail
def bar[I <: Int](i: I)(j: Int @@ Greater[i.type]) = j - i
```

### shapeless to the rescue

We can however use `shapeless.Witness` to workaround this limitation in
the specification. `Witness` captures the singleton type of an `AnyVal`
and makes it available via the type member `T`:

```tut
def baz[I <: Int](i: Witness.Aux[I])(j: Int @@ Greater[i.T]) = j - i.value
```

```tut:nofail
baz(Witness(2))(4)
```

```tut:fail
baz(Witness(6))(4)
```

[spec-3.2.1]: http://www.scala-lang.org/files/archive/spec/2.11/03-types.html#singleton-types
