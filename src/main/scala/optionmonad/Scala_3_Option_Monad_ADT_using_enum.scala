package optionmonad

// the following import purports to hide scala.Option, but in practice it is not needed
// it is only here for sceptics and/or paranoids
import scala.{Option => _, Some => _, None => _}

enum Option[+A]:
  case Some(a: A)
  case None
  
  def map[B](f: A => B): Option[B] =
    this match
      case Some(a) => Some(f(a))
      case None => None
  
  def flatMap[B](f: A => Option[B]): Option[B] =
    this match
      case Some(a) => f(a)
      case None => None
      
  def fold[B](ifEmpty: => B)(f: A => B) =
    this match
      case Some(a) => f(a)
      case None => ifEmpty
      
  def filter(p: A => Boolean): Option[A] =
    this match
      case Some(a) if p(a) => Some(a)
      case _ => None
      
  def withFilter(p: A => Boolean): Option[A] =
    filter(p)
      
object Option: 
  def pure[A](a: A):Option[A] = Some(a)
  def none: Option[Nothing] = None

extension[A](a: A):
  def some: Option[A] = Some(a)

enum Language(val toPreposition: String):
  case English extends Language("to")
  case German  extends Language("nach")
  case French  extends Language("à")
  case Spanish extends Language("a") 
  case Italian extends Language("a")

import Language._
enum Greeting(val language: Language):
  override def toString: String = s"${enumLabel} ${language.toPreposition}"
  case Welcome extends Greeting(English)
  case Willkommen extends Greeting(German)
  case Bienvenue extends Greeting(French)
  case Bienvenido extends Greeting(Spanish)
  case Benvenuto extends Greeting(Italian)  

enum Planet:
  case Mercury, Venus, Earth, Mars, Jupiter, Saturn, Neptune, Uranus, Pluto, Scala3

case class Earthling(name: String, surname: String, languages: Language*) 

import optionmonad.Option.{Some,None}, optionmonad.Option._, optionmonad.Planet._, optionmonad.Greeting._

def greet(maybeGreeting: Option[Greeting], maybeEarthling: Option[Earthling], maybePlanet: Option[Planet]): Option[String] = 
  for
    greeting   <- maybeGreeting 
    earthling  <- maybeEarthling
    planet     <- maybePlanet
    if earthling.languages contains greeting.language
  yield s"$greeting $planet ${earthling.name}!"

import scala.util.Try

@main def main =

  val maybeGreeting = 
    greet(  maybeGreeting = Some(Welcome),
           maybeEarthling = Some(Earthling("Fred", "Smith", English, Italian)),
              maybePlanet = Some(Scala3))
  
  println(maybeGreeting.fold("Error: no greeting message available")(message => s"*** $message ***"))
  
  //     Greeting               Earthling                                           Planet
  assert(greet(Some(Welcome),   Some(Earthling("Fred", "Smith", English, Italian)), Some(Scala3)) == Some("Welcome to Scala3 Fred!"))
  assert(greet(Some(Benvenuto), Some(Earthling("Fred", "Smith", English, Italian)), Some(Scala3)) == Some("Benvenuto a Scala3 Fred!"))
  assert(greet(Some(Bienvenue), Some(Earthling("Fred", "Smith", English, Italian)), Some(Scala3)) == None)
  assert(greet(None,            Some(Earthling("Fred", "Smith", English, Italian)), Some(Scala3)) == None)
  assert(greet(Some(Welcome),   None,                                               Some(Scala3)) == None)
  assert(greet(Some(Welcome),   Some(Earthling("Fred", "Smith", English,Italian)),  None)         == None)

  assert(greet(Welcome.some,    Earthling("Fred", "Smith", English, Italian).some, Scala3.some) == ("Welcome to Scala3 Fred!").some)
  assert(greet(Benvenuto.some,  Earthling("Fred", "Smith", English, Italian).some, Scala3.some) == ("Benvenuto a Scala3 Fred!").some)
  assert(greet(Bienvenue.some,  Earthling("Fred", "Smith", English, Italian).some, Scala3.some) == none)
  assert(greet(none,            Earthling("Fred", "Smith", English, Italian).some, Scala3.some) == none)
  assert(greet(Welcome.some,    none,                                              Scala3.some) == none)
  assert(greet(Welcome.some,    Earthling("Fred", "Smith", English, Italian).some, none)        == none)

  val stringToInt: String => Option[Int] =
    s => Try { s.toInt }.fold(_ => None, Some(_))
  assert( stringToInt("123") == Some(123) )
  assert( stringToInt("1x3") == None )
  
  val intToChars: Int => Option[List[Char]] =
    n => if n < 0 then None else Some(n.toString.toArray.toList)
    
  assert(intToChars(123) == Some(List('1', '2', '3')))  
  assert(intToChars(0) == Some(List('0')))  
  assert(intToChars(-10) == None)

  import scala.util.{Failure, Success, Try}
  val charsToInt: Seq[Char] => Option[Int] =
    chars => Try { chars.foldLeft(0){(n,char) => 10 * n + char.toString.toInt} }.fold(_ => None, Some(_))

  assert(charsToInt(List('1', '2', '3')) == Some(123) )
  assert(charsToInt(List('1', 'x', '3')) == None )  
  
  def doublePalindrome(s: String): Option[String] =
    for
      n <- stringToInt(s)
      chars <- intToChars(2 * n)
      palindrome <- charsToInt(chars ++ chars.reverse) 
    yield palindrome.toString
  assert( doublePalindrome("123") == Some("246642") )
  assert( doublePalindrome("1x3") == None )
  
  // plain function composition
  extension[A,B,C](f: B => C)
    def ∘ (g: A => B): A => C =
      a => f(g(a))

  //////////////////
  // FUNCTOR LAWS //
  //////////////////
  {
    val double: Int => Int = n => 2 * n
    val square: Int => Int = n => n * n
    val f = stringToInt
    val g = double
    val h = square
    val a = "123"
    
    // identity law: ma map id = ma
    assert( (f(a) map identity) == f(a) )
    assert( (a.some map identity) == identity(a.some) )
    assert( (none map identity) == identity(none) )    
    // composition law: ma map (g ∘ h) == ma map h map g
    assert( (f(a) map (g ∘ h)) == (f(a) map h map g) )
    assert( (3.some map (g ∘ h)) == (3.some map h map g) )
    assert( (none map (g ∘ h)) == (none map h map g) )
  }  
  
  // Haskell-style >>= (bind) alias for flatMap
  extension[A,B](oa: Option[A]):
    def >>= (f: A => Option[B]): Option[B] = 
      oa flatMap f
      
  // Kleisli arrow composition operator, AKA the fish operator
  extension[A,B,C](f: A => Option[B]):
    def >=> (g: B => Option[C]): A => Option[C] =
      a => f(a) >>= g

  ////////////////
  // MONAD LAWS //
  ////////////////
  {
    val f = stringToInt
    val g = intToChars
    val h = charsToInt
    val a = "123"

    /********************************************
     * Monad laws expressed in terms of flatMap *
     ********************************************/
    
    // left identity law: pure(a) flatMap f == f(a)
    assert((pure(a) flatMap f) == f(a))
    // right identity law: ma flatMap pure == ma
    assert((f(a) flatMap pure) == f(a))
    assert((a.some flatMap pure) == a.some)
    assert((none flatMap pure) == none)
    // associativity law: ma flatMap f flatMap g = ma flatMap (a => f(a) flatMap g)
    assert(((f(a) flatMap g) flatMap h) == (f(a) flatMap (x => g(x) flatMap h)))
    assert(((3.some flatMap g) flatMap h) == (3.some flatMap (x => g(x) flatMap h)))
    assert(((none flatMap g) flatMap h) == (none flatMap (x => g(x) flatMap h)))

    /**************************************************************************
     * Monad laws expressed in terms of bind operator >>= (alias for flatMap) *
     **************************************************************************/
    
    // left identity law: pure(a) flatMap f == f(a)
    assert((pure(a) >>= f) == f(a))
    // right identity law: ma flatMap pure == ma
    assert((f(a) >>= pure) == f(a))
    assert((3.some >>= pure) == 3.some)
    assert((none >>= pure) == none)
    // associativity law: ma flatMap f flatMap g = ma flatMap (a => f(a) flatMap g)
    assert(((f(a) >>= g) >>= h) == (f(a) >>= (x => g(x) >>= h)))
    assert(((3.some >>= g) >>= h) == (3.some >>= (x => g(x) >>= h)))
    assert(((none >>= g) >>= h) == (none >>= ((x:Int) => g(x) >>= h)))

    /****************************************************************************
     * Monad laws expressed in terms of fish operator >=> (Kleisli composition) *
     ****************************************************************************/
    
    // left identity law: pure >=> f == f
    assert( (pure[String] >=> f)(a) == f(a) )
    // right identity law: f >=> pure == f
    assert( (f >=> pure)(a) == f(a))
    // associativity law : f >=> (g >=> h) == (f >=> g) >=> h
    assert( (f >=> (g >=> h))(a) == ((f >=> g) >=> h)(a) )
  }
