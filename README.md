## Motivation

This library aims at facilitating programming with immutable
objects. Scala provides case classes for this purpose, and they are a
good solution for most situations. However, they sometimes involve
writing a lot of boilerplate. This problem is specially exacerbated
when programming inheritance hierarchies, since abstract setters need
to be implemented as well. You can find a more elaborate motivation to
the updatable package in our blog: http://blog.hablapps.com.

## Automatic generation of implementation classes

The updatable package allows you to generate implementation classes automatically:

```scala
scala> import org.hablapps.updatable._
import org.hablapps.updatable._

scala> import scala.language.reflectiveCalls
import scala.language.reflectiveCalls

scala> trait Person{
     |   val name: String
     |   val age: Int
     |   val friends: Traversable[String]
     | }
defined trait Person

scala> implicit val Person = builder[Person]
Person: org.hablapps.updatable.Builder[Person]{ ... }
```

The `builder` construct is actually a macro that instantiate
the `Builder` type class for the `Person` type. The `Person` builder
includes a factory method `apply` which enables the instantiation of
person objects. Instantiation is carried out through an anonymous
class which overrides the `toString` and `==` operators conveniently.

```scala
scala> val socrates = Person("Socrates",20,Set()) 
socrates: Person = Person(name=Socrates,age=20)

scala> val plato = Person("Plato",10,Set("Socrates"))
plato: Person = Person(name=Plato,age=10,friends=Set("Socrates"))

scala> val anotherSocrates = Person("Socrates",20,Set())
anotherSocrates: Person = Person(name=Socrates,age=20)

scala> anotherSocrates == socrates
res1: Boolean = true
```

The factory method is equipped with default parameters, obtained
through the `Default` type class. The companion object of this type
class already provides instantiations for common Scala types. Of
course, you can define default values for your own types.

```scala
scala> val defaultPerson = Person()
defaultPerson: Person = Person(name=,age=0)

scala> val wittgenstein = Person("Ludwig")
wittgenstein: Person = Person(name=Ludwig,age=0)
```

Currently, if no value is provided for some attribute and no default
is available, a runtime exception is thrown when the factory is
invoked.

## Var-like updates 

The generated builder also allows us to update immutable objects of
the corresponding type through the `updated` method. This method,
however, is not type-safe, so it's better to use the `:=` operator
which mimics the syntax of `var` updates and won't let you assign
values of the wrong type. This operator is enabled through an implicit
macro conversion.

```scala
scala> socrates.age := 65
res9: org.hablapps.updatable.Updatable[Person] = Person(name=Socrates,age=65)

scala> socrates.age := "65"
<console>:20: error: type mismatch;
 found   : String("65")
 required: Int
              socrates.age := "65"
                              ^

scala> socrates.friends := Set("Plato")
res11: org.hablapps.updatable.Updatable[Person] = Person(name=Socrates,age=50,friends=Set(Plato))

scala> socrates.friends := Set(Plato)
<console>:21: error: type mismatch;
 found   : Person
 required: String
              socrates.friends := Set(Plato)
                                      ^
```

Note that the results of these updates are not plain values of type
`Person`, but objects of type `Updatable[Person]`. Updatable objects are
just like plain objects, with the difference that they also wrap the
builder used in its instantiation. Most of the time, however, you can
safely omit this feature since there is an implicit conversion from
updatable values to plain ones.

For multi-valued attributes, besides the `:=` operator we can also
employ the `+=` and `-=` operators -- provided there is evidence for a
`Modifiable` instance of the corresponding type constructor:

```scala
scala> socrates.friends += "Plato"
res13: org.hablapps.updatable.Updatable[Person] = Person(name=Socrates,age=50,friends=Set(Plato))

scala> (socrates.friends := Set("Plato")).friends -= "Plato"
res15: org.hablapps.updatable.Updatable[Person] = Person(name=Socrates,age=50)
```

Currently, modifiable instances for `Option` and any kind of `Traversable`
are defined. Note that although existence of `Modifiable` instances are
statically checked, a runtime exception will be thrown if no
modifiable evidence was found. This is to facilitate the use of
modifiables at generic contexts.

## Updates in generic contexts

Let's extend the `Person` trait with new types: 

```scala
scala> trait ComputerScientist extends Person{ 
     |   val designerOf: Option[String] 
     | }
defined trait ComputerScientist

scala> implicit val ComputerScientist = builder[ComputerScientist]
ComputerScientist: org.hablapps.updatable.Builder[ComputerScientist]{...}

scala> trait Philosopher extends Person{ 
     |   val skeptic: Option[Boolean] 
     | }
defined trait Philosopher

scala> implicit val Philosopher = builder[Philosopher]
Philosopher: org.hablapps.updatable.Builder[Philosopher]{ ... }

scala> Philosopher()
res16: Philosopher = Philosopher(name=,age=0)

scala> Philosopher(_idealist=Some(true))
res17: Philosopher = Philosopher(name=,age=0,idealist=true)

scala> ComputerScientist(_name="McCarthy",_designerOf=Some("lisp")) 
res18: ComputerScientist = ComputerScientist(name=McCarthy,age=0,designerOf=lisp)
```
Now, let's define a generic function to increment the age of any person: 

```scala
scala> def incAge[P <: Person : Builder](p: P): P = 
     |   p.age := p.age + 1
incAge: [P <: Person](p: P)(implicit evidence$1: org.hablapps.updatable.Builder[P])P

scala> incAge(Person())
res20: Person = Person(name=,age=1)

scala> incAge(Philosopher())
res21: Philosopher = Philosopher(name=,age=1)

scala> incAge(ComputerScientist())
res22: ComputerScientist = ComputerScientist(name=,age=1)
```

Note that the `:=` operator can also be used within generic contexts -
if we pass evidence of the right builder. This can be achieved in two
ways: either explicitly, as in the example above, or encapsulated in
an updatable object, as in the following example:

```scala
scala> def incAge[P <: Person](p: Updatable[P]): P = 
     |   p.age := p.age + 1
incAge: [P <: Person](p: org.hablapps.updatable.Updatable[P])P

scala> incAge(Philosopher())
res23: Philosopher = Philosopher(name=,age=1)
```

### What about if we want to override some attribute?

The computer scientist and philosopher types shown above were simply defined
by extending the `Person` type with <em>additional</em>
fields. But we may also want to <em>refine</em> some existing field,
such as the `friends` attribute. For instance, we may want to declare
that philosophers store their friends in `Set`s and computer
scientists in `List`s. Let's try it and see what happens: 

```scala
scala> trait ComputerScientist extends Person{ 
     |   val friends: List[String] 
     | }
defined trait ComputerScientist

scala> implicit val ComputerScientist = builder[ComputerScientist]
ERROR: attribute `friends` of type `Person` is overridden; can't generate builder
```

Ooops! As soon as we attempt to generate a builder for the
`ComputerScientist` type, we are told that we can't do it because that
type overrides an attribute. Why did we ban overriding? Because
generic updates of overridable attributes are not type-safe.

But then, how do we <em>refine</em> some attribute? Well, we can
follow the same strategy that we would if we didn't have the updatable
package, and did have to implement the setters ourselves: we can
employ auxiliary type members. For instance, we can abstract over the
type constructor of the `friends` attribute using a `FriendsCol[_]`
abstract type, as follows: 

```scala
scala> :paste
trait Person {
  val name: String
  val age: Int

  type FriendsCol[_]
  val friends: FriendsCol[String]
}

implicit val Person = weakBuilder[Person]

trait Philosopher extends Person {
  type FriendsCol[x] = Set[x]
}

implicit val Philosopher = builder[Philosopher]

trait ComputerScientist extends Person {
  type FriendsCol[x] = List[x]
}

implicit val ComputerScientist = builder[ComputerScientist]
```

Note that the `Person` trait defined above can't be instantiated now
without first defining the `FriendsCol[_]` type member, so we can't
generate a builder for this type. Instead, we must generate a
`WeakBuilder` which simply provides attribute reifications. If we
tried to generate a builder, a warning would be issued:

```scala
<console>:20: warning: 
	Person is an abstract type. Are you sure you are not willing to use a 'weakBuilder[Person]' instead?.
	- Abstract attributes: friends
	- Abstract types: FriendsCol
	
         implicit val Person = builder[Person]
```

Now, if we want to define a new generic method over `Person` objects,
we can refer to the actual declaration of the `friends` attribute
using dependent types. For instance: 

```scala
def setFriends[P <: Person : Builder](p: P)(persons: p.FriendsCol[String]): P =
  p.friends := persons

scala> setFriends(Philosopher())(List("Socrates"))
<console>:20: error: type mismatch;
 found   : List[String]
 required: scala.collection.immutable.Set[String]
              setFriends(Philosopher())(List("Socrates"))
                                            ^
```

## Current & future work

Current work mostly focuses on fixing problems. But we would like to add more
functionality in the near future. Here are some possible topics.

### Lense-like composition

The library is certainly related to lenses, although our primary use
case is not lense composition. It should be easy, though, to extend
the library to support nested updates,

```scala
scala> trait T1{ val a1: T2 }; trait T2{ val a2: T3 }; trait T3{ val a3: Int }
scala> implicit val T1 = builder[T1]; ... 
scala> val t : T1 = T1(T2(T3(1))
...
scala> (t.a1.a2.a3 := v): T1
...
```

### Parameterized factories

The idea is to be able to instantiate objects of traits that have type
members undefined.

```scala
scala> trait T{ 
     |   type A1
     |   type A2
     |   val a1: A1
     |   val a2: A2 
     | }
scala>  val t: T = T[Int,String](1,"")
```

### Type macros

Type macros fit our problem quite naturally. Eventually, we will
create a branch to test the updatable package with the macro-paradise
branch.

## Using Updatable

Please, note that the library is currently in experimental status.

Updatable is published to the Hablapps Repository. Release and snapshot builds are published relative to Scala 2.10.2.

To build with Scala 2.10.2 add the following to your SBT (0.12.0 or later) configuration,

```scala
scalaVersion := "2.10.2"

scalaBinaryVersion <<= scalaVersion { sv => sv }

resolvers ++= Seq(
  "Hablapps - releases" at "http://repo.hablapps.org/releases",
  "Hablapps - snapshots" at "http://repo.hablapps.org/snapshots"
)

libraryDependencies ++= Seq(
  "org.hbalapps" % "updatable" %% "0.7.1"
)
```

(*) We have detected some problems with 2.10.1-RCX versions. We aim
to fix this issue in the next weeks.

## License

Updatable is released under Apache License, Version 2.0.
