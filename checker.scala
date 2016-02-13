import scala.io._
import cs162.assign4.syntax._
import Aliases._
import scala.io.Source.fromFile

//——————————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          println(Pretty.prettySyntax(program))
          getType( program.e, new TypeEnv())
          println("This program is well-typed")
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  // Gets a listing of the constructor names associated with a given type definition.
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructors`, along with return values:
  //
  // constructors("Either") = Set("Left", "Right")
  // constructors("Foo") = a thrown Illtyped exception
  //
  def constructors(name: Label): Set[Label] =
    typeDefs.find(_.name == name).map(_.constructors.keySet).getOrElse(throw Illtyped)

  // Takes the following parameters:
  // -The name of a user-defined type
  // -The name of a user-defined constructor in that user-defined type
  // -The types which we wish to apply to the constructor
  // Returns the type that is held within the constructor.
  //
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructorType`, along with return values:
  //
  // constructorType("Either", "Left", Seq(NumT, BoolT)) = NumT
  // constructorType("Either", "Right", Seq(NumT, BoolT)) = BoolT
  // constructorType("Either", "Left", Seq(NumT)) = a thrown Illtyped exception
  // constructorType("Either", "Right", Seq(BoolT)) = a thrown Illtyped exception
  // constructorType("Either", "Foo", Seq(UnitT)) = a thrown Illtyped exception
  // constructorType("Bar", "Left", Seq(UnitT)) = a thrown Illtyped exception
  //
  def constructorType(name: Label, constructor: Label, types: Seq[Type]): Type = 
    (for {
      td <- typeDefs
      rawType <- td.constructors.get(constructor)
      if (types.size == td.tvars.size)
    } yield replace(rawType, td.tvars.zip(types).toMap)).headOption.getOrElse(throw Illtyped)

  // Gets the type of the constructor.
  // For example, considering the typedefs given in the `constructors` comment above,
  // `typename` will return the following with the given arguments:
  //
  // typename(Label("Left")) = Label("Either")
  // typename(Label("Right")) = Label("Either")
  // typename(Label("Some")) = Label("Maybe")
  // typename(Label("None")) = Label("Maybe")
  //
  def typename(constructor: Label): Label =
    typeDefs.find(_.constructors.contains(constructor)).getOrElse(throw Illtyped).name

  // Given a type and a mapping of type variables to other types, it
  // will recursively replace the type variables in `t` with the
  // types in `tv2t`, if possible.  If a type variable isn't
  // in `tv2t`, it should simply return the original type.  If a
  // `TFunT` is encountered, then whatever type variables it defines
  // (the first parameter in the `TFunT`) should overwrite whatever is in
  // `tv2t` right before a recursive `replace` call.  In other words,
  // type variables can shadow other type variables.
  //
  def replace( t:Type, tv2t:Map[TVar, Type] ): Type =
    t match {
      case NumT | BoolT | UnitT => ??? // FILL ME IN

      case FunT(params, ret) => ??? // FILL ME IN

      case RcdT(fields) => ??? // FILL ME IN

      case TypT(name, typs) => ??? // FILL ME IN

      case tv:TVar => ??? // FILL ME IN

      case TFunT(tvars, funt) => ??? // FILL ME IN
    }

  // HINT - the bulk of this remains unchanged from the previous assignment.
  // Feel free to copy and paste code from your last submission into here.
  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      case x:Var => (env.get(x)).getOrElse(throw Illtyped) // FILL ME IN

      case _:Num => NumT // FILL ME IN

      case _:Bool => BoolT // FILL ME IN

      case _:Unit => UnitT // FILL ME IN

      case Plus | Minus | Times | Divide => FunT(List(NumT, NumT), NumT) // FILL ME IN

      case LT | EQ => FunT(List(NumT, NumT), BoolT) // FILL ME IN

      case And | Or => FunT(List(BoolT, BoolT), BoolT) // FILL ME IN

      case Not => FunT(List(BoolT), BoolT) // FILL ME IN

      case Fun(params, body) => getType(body, env ++ params.map(i => i._1 -> i._2).toMap) // FILL ME IN

      case Call(fun, args) => {
        val x = getType(fun, env)
        x match {
          case y: FunT => {
            if (y.params.length == args.length) {
              for (i <- 0 to y.params.length) {
                if (y.params.apply(i) != getType(args.apply(i), env)) {
                  throw Illtyped
                }
              }
              y.ret
            } else {
              throw Illtyped
            }
          }
          case _ => throw Illtyped
        }
      } // FILL ME IN

      case If(e1, e2, e3) => {
        if (getType(e1, env) == BoolT) {
          if (getType(e2, env) == getType(e3, env)) {
            getType(e2, env)
          } else {
            throw Illtyped
          }
        } else {
          throw Illtyped
        }
      } // FILL ME IN

      case Let(x, e1, e2) => getType(e2, env + (x -> getType(e1, env))) // FILL ME IN

      case Rec(x, t1, e1, e2) => {
        if (getType(e1, env + (x -> t1)) == t1) {
          getType(e2, env + (x -> getType(e2, env + (x -> t1))))
        } else {
          throw Illtyped
        }
      } // FILL ME IN

      case Record(fields) => RcdT(fields.map { case (k, v) => (k, getType(v, env)) }) // FILL ME IN

      case Access(e, field) => {
        e match {
          case Record(fields) => {
            if (fields.contains(field)) {
              getType(fields.getOrElse(field, throw Illtyped), env)
            } else { throw Illtyped }
          }
          case _ => throw Illtyped
        }
      } // FILL ME IN

      case c @ Construct(constructor, typs, e) => ??? // FILL ME IN

      case Match(e, cases) => ??? // FILL ME IN

      case TAbs(tvars, fun) => ??? // FILL ME IN

      case TApp(e, typs) => ??? // FILL ME IN
    }
}
