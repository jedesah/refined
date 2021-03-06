# refined: simple refinement types for Scala
[![Download](https://img.shields.io/maven-central/v/eu.timepit/refined_2.11.svg)][search.maven]
[![Build Status](https://img.shields.io/travis/fthomas/refined/master.svg)](https://travis-ci.org/fthomas/refined)
[![Gitter](https://img.shields.io/badge/Gitter-join%20chat-brightgreen.svg)](https://gitter.im/fthomas/refined?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![codecov.io](https://img.shields.io/codecov/c/github/fthomas/refined.svg)](http://codecov.io/github/fthomas/refined)
[![Codacy Badge](https://img.shields.io/codacy/e4f25ef2656e463e8fed3f4f9314abdb.svg)](https://www.codacy.com/app/fthomas/refined)

*refined* is a Scala library for refining types with type-level predicates
which constrain the set of values described by the refined type. It started
as a port of the [refined][refined.hs] Haskell library (which also provides
an excellent motivation why this kind of library is useful).

A quick example:

```scala
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import shapeless.tag.@@

// This refines Int with the Positive predicate and checks via an
// implicit macro that the assigned value satisfies it:
val i1: Int @@ Positive = 5
i1: Int @@ Positive = 5

// If the value does not satisfy the predicate, we get a meaningful
// compile error:
val i2: Int @@ Positive = -5
<console>:21: error: Predicate failed: (-5 > 0).
       val i2: Int @@ Positive = -5
                                  ^

// There is also the explicit refineMT macro that can infer the base
// type from its parameter:
scala> refineMT[Positive](5)
res0: Int @@ Positive = 5

// Macros can only validate literals because their values are known at
// compile-time. To validate arbitrary (runtime) values we can use the
// refineT function:
scala> refineT[Positive](5)
res1: Either[String, Int @@ Positive] = Right(5)

scala> refineT[Positive](-5)
res2: Either[String, Int @@ Positive] = Left(Predicate failed: (-5 > 0).)
```

`@@` is [shapeless'][shapeless] type for tagging types which has the nice
property of being a subtype of its first type parameter (i.e. `(T @@ P) <: T`).

*refined* also contains inference rules for converting between different
refined types. For example, `Int @@ Greater[_10]` can be safely converted
to `Int @@ Positive` because all integers greater than ten are also positive.
The type conversion of refined types is a compile-time operation that is
provided by the library:

```scala
import shapeless.nat._

scala> val a: Int @@ Greater[_5] = 10
a: Int @@ Greater[_5] = 10

// Since every value greater than 5 is also greater than 4, a can be ascribed
// the type Int @@ Greater[_4]:
scala> val b: Int @@ Greater[_4] = a
b: Int @@ Greater[_4] = 10

// An unsound ascription leads to a compile error:
scala> val c: Int @@ Greater[_6] = a
<console>:34: error: invalid inference: Greater[_5] ==> Greater[_6]
       val b: Int @@ Greater[_6] = a
                                   ^
```

This mechanism allows to pass values of more specific types (e.g. `Int @@ Greater[_10]`)
to functions that take a more general type (e.g. `Int @@ Positive`) without manual
intervention.

## Table of contents

1. [More examples](#more-examples)
2. [Using refined](#using-refined)
3. [Documentation](#documentation)
4. [Provided predicates](#provided-predicates)
5. [Contributors and participation](#contributors-and-participation)
6. [Projects using refined](#projects-using-refined)
7. [Performance concerns](#performance-concerns)
8. [Related projects](#related-projects)
9. [License](#license)

## More examples

```scala
import shapeless.{ ::, HNil }
import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.generic._
import eu.timepit.refined.string._

scala> refineMT[NonEmpty]("Hello")
res2: String @@ NonEmpty = Hello

scala> refineMT[NonEmpty]("")
<console>:27: error: Predicate isEmpty() did not fail.
            refineMT[NonEmpty]("")
                              ^

scala> type ZeroToOne = Not[Less[_0]] And Not[Greater[_1]]
defined type alias ZeroToOne

scala> refineMT[ZeroToOne](1.8)
<console>:27: error: Right predicate of (!(1.8 < 0) && !(1.8 > 1)) failed: Predicate (1.8 > 1) did not fail.
              refineMT[ZeroToOne](1.8)
                                 ^

scala> refineMT[AnyOf[Digit :: Letter :: Whitespace :: HNil]]('F')
res3: Char @@ AnyOf[Digit :: Letter :: Whitespace :: HNil] = F

scala> refineMT[MatchesRegex[W.`"[0-9]+"`.T]]("123.")
<console>:34: error: Predicate failed: "123.".matches("[0-9]+").
              refineMT[MatchesRegex[W.`"[0-9]+"`.T]]("123.")
                                                    ^

scala> val d1: Char @@ Equal[W.`'3'`.T] = '3'
d1: Char @@ Equal[Char('3')] = 3

scala> val d2: Char @@ Digit = d1
d2: Char @@ Digit = 3

scala> val d3: Char @@ Letter = d1
<console>:34: error: invalid inference: Equal[Char('3')] ==> Letter
       val d3: Char @@ Letter = d1
                                ^

scala> val r1: String @@ Regex = "(a|b)"
r1: String @@ Regex = (a|b)

scala> val r2: String @@ Regex = "(a|b"
<console>:40: error: Regex predicate failed: Unclosed group near index 4
(a|b
    ^
       val r2: String @@ Regex = "(a|b"
                                 ^

scala> val u1: String @@ Url = "htp://example.com"
<console>:40: error: Url predicate failed: unknown protocol: htp
       val u1: String @@ Url = "htp://example.com"
                               ^
```

Note that `W` is a shortcut for [`shapeless.Witness`][singleton-types] which
provides syntax for singleton types.

## Using refined

The latest version of the library is 0.3.2, which is available for Scala and
[Scala.js][scala.js] version 2.10 and 2.11.

If you're using sbt, add the following to your build:

    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined"            % "0.3.2",
      "eu.timepit" %% "refined-scalaz"     % "0.3.2",         // optional, JVM only
      "eu.timepit" %% "refined-scodec"     % "0.3.2",         // optional
      "eu.timepit" %% "refined-scalacheck" % "0.3.2" % "test" // optional
    )

For Scala.js just replace `%%` with `%%%` above.

Instructions for Maven and other build tools are available at [search.maven.org][search.maven].

Release notes for the latest version are available in
[0.3.2.markdown](https://github.com/fthomas/refined/blob/master/notes/0.3.2.markdown).

## Documentation

API documentation of the latest release is available at:
[http://fthomas.github.io/refined/latest/api/](http://fthomas.github.io/refined/latest/api/#eu.timepit.refined.package)

There are further (type-checked) examples in the [`docs`][docs]
directory including ones for defining [custom predicates][custom-pred]
and working with [type aliases][type-aliases]. It also contains a
[description][design-description] of *refined's* design and internals.

[custom-pred]: https://github.com/fthomas/refined/blob/master/docs/custom_predicates.md
[design-description]: https://github.com/fthomas/refined/blob/master/docs/design_description.md
[docs]: https://github.com/fthomas/refined/tree/master/docs
[type-aliases]: https://github.com/fthomas/refined/blob/master/docs/type_aliases.md

## Provided predicates

The library comes with these predefined predicates:

[`boolean`](https://github.com/fthomas/refined/blob/master/core/shared/src/main/scala/eu/timepit/refined/boolean.scala)

* `True`: constant predicate that is always `true`
* `False`: constant predicate that is always `false`
* `Not[P]`: negation of the predicate `P`
* `And[A, B]`: conjunction of the predicates `A` and `B`
* `Or[A, B]`: disjunction of the predicates `A` and `B`
* `Xor[A, B]`: exclusive disjunction of the predicates `A` and `B`
* `AllOf[PS]`: conjunction of all predicates in `PS`
* `AnyOf[PS]`: disjunction of all predicates in `PS`
* `OneOf[PS]`: exclusive disjunction of all predicates in `PS`

[`char`](https://github.com/fthomas/refined/blob/master/core/shared/src/main/scala/eu/timepit/refined/char.scala)

* `Digit`: checks if a `Char` is a digit
* `Letter`: checks if a `Char` is a letter
* `LetterOrDigit`: checks if a `Char` is a letter or digit
* `LowerCase`: checks if a `Char` is a lower case character
* `UpperCase`: checks if a `Char` is an upper case character
* `Whitespace`: checks if a `Char` is white space

[`collection`](https://github.com/fthomas/refined/blob/master/core/shared/src/main/scala/eu/timepit/refined/collection.scala)

* `Contains[U]`: checks if a `TraversableOnce` contains a value equal to `U`
* `Count[PA, PC]`: counts the number of elements in a `TraversableOnce` which
  satisfy the predicate `PA` and passes the result to the predicate `PC`
* `Empty`: checks if a `TraversableOnce` is empty
* `NonEmpty`: checks if a `TraversableOnce` is not empty
* `Forall[P]`: checks if the predicate `P` holds for all elements of a
  `TraversableOnce`
* `Exists[P]`: checks if the predicate `P` holds for some elements of a
  `TraversableOnce`
* `Head[P]`: checks if the predicate `P` holds for the first element of
  a `Traversable`
* `Index[N, P]`: checks if the predicate `P` holds for the element at
  index `N` of a sequence
* `Last[P]`: checks if the predicate `P` holds for the last element of
  a `Traversable`
* `Size[P]`: checks if the size of a `TraversableOnce` satisfies the predicate `P`
* `MinSize[N]`: checks if the size of a `TraversableOnce` is greater than
  or equal to `N`
* `MaxSize[N]`: checks if the size of a `TraversableOnce` is less than
  or equal to `N`

[`generic`](https://github.com/fthomas/refined/blob/master/core/shared/src/main/scala/eu/timepit/refined/generic.scala)

* `Equal[U]`: checks if a value is equal to `U`
* `Eval[S]`: checks if a value applied to the predicate `S` yields `true`
* `ConstructorNames[P]`: checks if the constructor names of a sum type satisfy `P`
* `FieldNames[P]`: checks if the field names of a product type satisfy `P`
* `Subtype[U]`: witnesses that the type of a value is a subtype of `U`
* `Supertype[U]`: witnesses that the type of a value is a supertype of `U`

[`numeric`](https://github.com/fthomas/refined/blob/master/core/shared/src/main/scala/eu/timepit/refined/numeric.scala)

* `Less[N]`: checks if a numeric value is less than `N`
* `LessEqual[N]`: checks if a numeric value is less than or equal to `N`
* `Greater[N]`: checks if a numeric value is greater than `N`
* `GreaterEqual[N]`: checks if a numeric value is greater than or equal to `N`
* `Positive`: checks if a numeric value is greater than zero
* `NonPositive`: checks if a numeric value is zero or negative
* `Negative`: checks if a numeric value is less than zero
* `NonNegative`: checks if a numeric value is zero or positive
* `Interval.Open[L, H]`: checks if a numeric value is in the interval (`L`, `H`)
* `Interval.OpenClosed[L, H]`: checks if a numeric value is in the interval (`L`, `H`]
* `Interval.ClosedOpen[L, H]`: checks if a numeric value is in the interval [`L`, `H`)
* `Interval.Closed[L, H]`: checks if a numeric value is in the interval [`L`, `H`]

[`string`](https://github.com/fthomas/refined/blob/master/core/shared/src/main/scala/eu/timepit/refined/string.scala)

* `EndsWith[S]`: checks if a `String` ends with the suffix `S`
* `MatchesRegex[S]`: checks if a `String` matches the regular expression `S`
* `Regex`: checks if a `String` is a valid regular expression
* `StartsWith[S]`: checks if a `String` starts with the prefix `S`
* `Uri`: checks if a `String` is a valid URI
* `Url`: checks if a `String` is a valid URL
* `Uuid`: checks if a `String` is a valid UUID
* `Xml`: checks if a `String` is valid XML
* `XPath`: checks if a `String` is a valid XPath expression

## Contributors and participation

* [Alexandre Archambault](https://github.com/alexarchambault) ([@alxarchambault](https://twitter.com/alxarchambault))
* [Frank S. Thomas](https://github.com/fthomas) ([@fst9000](https://twitter.com/fst9000))
* [Vladimir Koshelev](https://github.com/koshelev) ([@vlad_koshelev](https://twitter.com/vlad_koshelev))
* Your name here :-)

The *refined* project supports the [Typelevel][typelevel] [code of conduct][code-of-conduct]
and wants all of its channels (Gitter, GitHub, etc.) to be welcoming environments for everyone.

## Projects using refined

If you have a project that uses the library to enforce more static guarantees
and you'd like to include in this list, please open a pull request or mention
it in the Gitter channel and we'll add a link to it here.

* [argonaut-shapeless](https://github.com/alexarchambault/argonaut-shapeless) -
  provides the argonaut-refined subproject for (de)serialization of refined
  types from and to JSON
* [circe](https://github.com/travisbrown/circe) - provides the circe-refined
  subproject for (de)serialization of refined types from and to JSON
* [Monocle](https://github.com/julien-truffaut/Monocle) - provides the
  monocle-refined subproject which contains lenses for safe bit indexing
  into primitive types
* Your project here :-)

## Performance concerns

Using *refined's* macros for compile-time refinement bears zero runtime
overhead for reference types and only causes boxing for value types.
[PostErasureAnyRef][PostErasureAnyRef] and [PostErasureAnyVal][PostErasureAnyVal]
show the differences of unrefined and refined types during the posterasure
compiler phase.

[PostErasureAnyRef]: https://github.com/fthomas/refined/blob/master/contrib/scalaz/src/test/scala/eu/timepit/refined/scalaz/examples/PostErasureAnyRef.scala
[PostErasureAnyVal]: https://github.com/fthomas/refined/blob/master/contrib/scalaz/src/test/scala/eu/timepit/refined/scalaz/examples/PostErasureAnyVal.scala

## Related projects

* [bond](https://github.com/fwbrasil/bond): Type-level validation for Scala
* [F7](http://research.microsoft.com/en-us/projects/f7/): Refinement Types for F#
* [LiquidHaskell](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/about/):
  Refinement Types via SMT and Predicate Abstraction
* [refined][refined.hs]: Refinement types with static and runtime checking for
  Haskell. *refined* was inspired this library and even stole its name!

## License

*refined* is licensed under the MIT license, available at http://opensource.org/licenses/MIT
and also in the [LICENSE](https://github.com/fthomas/refined/blob/master/LICENSE) file.

[code-of-conduct]: http://typelevel.org/conduct.html
[refined.hs]: http://nikita-volkov.github.io/refined
[scala.js]: http://www.scala-js.org
[search.maven]: http://search.maven.org/#search|ga|1|eu.timepit.refined
[shapeless]: https://github.com/milessabin/shapeless
[singleton-types]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#singleton-typed-literals
[typelevel]: http://typelevel.org
