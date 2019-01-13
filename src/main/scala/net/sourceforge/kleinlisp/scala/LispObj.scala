package net.sourceforge.kleinlisp.scala

import net.sourceforge.kleinlisp.Function
import net.sourceforge.kleinlisp.LispObject
import net.sourceforge.kleinlisp.objects._

object LispObj{

  implicit def toObj( obj: LispObject ): LispObj = obj match {
    case d: DoubleObject   => new DoubleObj( d.value() )
    case i: IntObject      => new IntObj( i.value() )
    case s: StringObject   => new StringObj( s.value() )
    case a: AtomObject     => new AtomObj( a.value() )
    case l: ListObject     => new ListObj(toList( l.asList().get() ))
    case f: FunctionObject => new FunctionObj( toFunction(f.function()) )
  }

  implicit def toList( l: ListObject ): List[LispObj] = {
    if( l == ListObject.NIL ) Nil
    else toObj( l.car() ) :: toList( l.cdr() )
  }

  implicit def fromList( l: List[LispObj]) : ListObject = l match {
    case x :: rest => new ListObject( x.toLispObject(), fromList(rest))
    case Nil => ListObject.NIL
  }

  implicit def toFunction(function: Function): List[LispObj] => LispObj = {
    def aux( l: List[LispObj] ): LispObj = {
      toObj( function.evaluate( fromList(l) ) )
    }

    aux
  }

  implicit def fromFunction( function:  List[LispObj] => LispObj ): Function = {
    class FunctionAux extends Function{
      override def evaluate(listObject: ListObject): LispObject = {
        function( toList( listObject ) ).toLispObject()
      }
    }

    new FunctionAux()
  }

  implicit def toFunctionObject( function:  List[LispObj] => LispObj ): LispObject =
    new FunctionObject(fromFunction(function))

}

sealed trait LispObj {
  def toLispObject(): LispObject
}

case class DoubleObj( value: Double ) extends LispObj {
  override def toLispObject(): LispObject = new DoubleObject( value )
}

case class IntObj( value: Int ) extends LispObj {
  override def toLispObject(): LispObject = new IntObject( value )
}

case class AtomObj( value: String ) extends LispObj {
  override def toLispObject(): LispObject = new AtomObject( value )
}

case class StringObj( value: String ) extends LispObj{
  override def toLispObject(): LispObject = new StringObject( value )
}

case class ListObj( list: List[LispObj]) extends LispObj{
  override def toLispObject(): LispObject = LispObj.fromList(list)
}

case class BooleanObj( value: Boolean ) extends LispObj{
  override def toLispObject(): LispObject = new BooleanObject(value)
}

case class FunctionObj( function: List[LispObj] => LispObj ) extends LispObj {
  override def toLispObject(): LispObject = new FunctionObject( LispObj.fromFunction(function) )
}

case class ErrorObj() extends LispObj {
  override def toLispObject(): LispObject = new ErrorObject("")
}

case class VoidObj() extends LispObj{
  override def toLispObject(): LispObject = new VoidObject
}