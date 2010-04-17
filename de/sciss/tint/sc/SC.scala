/*
 *  SC.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.tint.sc

import de.sciss.scalaosc.{ OSCMessage }
import math._

/**
 * 	@version	0.14, 17-Apr-10
 */
object SC {
  // GEs
  implicit def float2GE( x: Float ) = Constant( x )
  implicit def int2GE( x: Int ) = Constant( x.toFloat )
  implicit def double2GE( x: Double ) = Constant( x.toFloat )
  implicit def seqOfGE2GESeq( x: Seq[ GE ]) = new GESeq( (x flatMap (_.toUGenIns)): _* )
  implicit def doneAction2GE( x: DoneAction ) = Constant( x.id )

  // why these are necessary now??
  implicit def seqOfFloat2GESeq( x: Seq[ Float ]) = new GESeq( (x map (Constant( _ ))): _* )
  implicit def seqOfInt2GESeq( x: Seq[ Int ]) = new GESeq( (x map (i => Constant( i.toFloat ))): _* )
  implicit def seqOfDouble2GESeq( x: Seq[ Double ]) = new GESeq( (x map (d => Constant( d.toFloat ))): _* )
 
  implicit def string2ControlName( name: String ) = ControlName( name )

   // do we really need this?
//  implicit def string2GE( name: String ) : ControlDesc =
//    SynthDef.graphBuilder.map( _.getControlDesc( name )) orNull

  // mixed number / GE binops
  // these conflict with scala.math, so we commented them out
//  def max( a: GE, b: GE ) = a.max( b )
//  def min( a: GE, b: GE ) = a.min( b )

  // Misc
  implicit def string2Option( x: String ) = Some( x )
  def dup[T]( x: T, num: Int ) : Seq[T] = {
//  var res = new Array[T]( num );
    (1 to num) map (y => x)
  }

   // Buffer convenience
   implicit def message2Option( msg: OSCMessage ) = Some( msg )

   // Nodes
   implicit def int2Node( id: Int ) : Node = new Group( Server.default, id )
   implicit def server2Group( s: Server ) : Group = s.defaultGroup

   // Maths conversions
   def ampdb( amp: Float ) = (log10( amp ) * 20).toFloat
   def dbamp( db: Float ) = (exp( db / 20 * log( 10 ))).toFloat
   def midicps( midi: Float ) = (440 * pow( 2, (midi - 69) * 0.083333333333 )).toFloat
   def cpsmidi( freq: Float ) = (log( freq * 0.0022727272727 ) / log( 2 ) * 12 + 69).toFloat
  
//  implicit def stringToStringOrInt( x: String ) = new StringOrInt( x )
//  implicit def intToStringOrInt( x: Int ) = new StringOrInt( x )
  
   // String
   def warn( s: String ) : String = {
      println( "WARNING:\n" + s )
      s
   }
  
  // Function
  // XXX
//  def fork( f: () => Any ) : Thread = { val t = new Thread( new Runnable() { def run() { f }}); t.start; t }
//  def fork( f: => Unit ) : Routine = { Routine.run( f )}
  
//	asSynthDef { arg rates, prependArgs, outClass='Out, fadeTime, name;}
//  def toSynthDef( f: () => Any, rates: Seq[Any], prependArgs: Seq[Any], outClass: Symbol, fadeTime: AnyVal, name: String ) : SynthDef = {
//   		^GraphBuilder.wrapOut(name ?? { this.identityHash.abs.asString },
//			this, rates, prependArgs, outClass, fadeTime
//		);	
//  }

// CCC
//  def fork[A,C]( ctx: =>(A @cps[A,C]) ) = { val r = new Routine( Clock.default, ctx ); r.start; r }
//  def sleep( delta: Long ): Unit @suspendable = self.asInstanceOf[Routine[_,_]].sleep( delta )
//
// def loopWhile(cond: =>Boolean)(body: =>(Any @suspendable)): Unit @suspendable = {
//   if (cond) {
//     body; loopWhile(cond)(body)
//   } else ()
// }
  
   def play( thunk: => GE ) : Synth = {
      val func = () => thunk
      playFunc( func, Server.default.defaultGroup, 0, Some(0.02f), addToHead )
//	   play( f, target, 0, Some(0.02f), 'addToHead )
   }

  // XXX should place the thunk always in second argument list
  /*
  def play( f: => GE, target: => Node ) : Synth = {
	def func() = f
    playFunc( func, target, 0, Some(0.02f), 'addToHead )
  }

  def play( f: => GE, target: => Node, outBus: Int ) : Synth = {
	def func() = f
    playFunc( func, target, outBus, Some(0.02f), 'addToHead )
  }

  def play( f: => GE, target: => Node, outBus: Int, fadeTime: Option[Float]) : Synth = {
	def func() = f
    playFunc( func, target, outBus, fadeTime, 'addToHead )
  }
  */
   def play( target: Node = Server.default.defaultGroup, outBus: Int,
             fadeTime: Option[Float] = Some( 0.02f ),
             addAction: AddAction = addToHead )( thunk: => GE ) : Synth = {
	   val func = () => thunk
	   playFunc( func, target, outBus, fadeTime, addAction )
   }

   private var uniqueIDCnt = 0
   private def uniqueID = {
      val result = uniqueIDCnt
      uniqueIDCnt += 1
      uniqueIDCnt
   }

   private def playFunc( func: () => GE, target: Node, outBus: Int, fadeTime: Option[Float], addAction: AddAction ) : Synth = {
    // arg target, outbus = 0, fadeTime=0.02, addAction='addToHead;

//		target = target.asTarget;
//        val target = new Group( Server.default, 1 ) // XXX
		val server = target.server
//		if( server.condition != 'running ) { 
//			throw new IllegalStateException( "server '" + server.name + "' not running." )
//		}
//		val defName = "temp__" + abs( func.hashCode )
		val defName    = "temp_" + uniqueID // why risk a hashcode clash?
		val synthDef   = GraphBuilder.wrapOut( defName, func, fadeTime )
		val synth      = new Synth( synthDef.name, server )
		val bytes      = synthDef.toBytes
		val synthMsg   = synth.newMsg( target, List( "i_out" -> outBus.toFloat, "out" -> outBus.toFloat ), addAction )
		if( bytes.remaining > (65535 / 4) ) { // preliminary fix until full size works
			if( server.isLocal ) {
				synthDef.load( server, synthMsg )
			} else {
				warn( "synthdef may have been too large to send to remote server" )
				server.sendMsg( "/d_recv", bytes, synthMsg )
			}
		} else {
			server.sendMsg( "/d_recv", bytes, synthMsg )
		}
		synth
	}
}