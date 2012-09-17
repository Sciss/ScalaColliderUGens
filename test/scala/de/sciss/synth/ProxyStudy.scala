package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}

object ProxyStudy2 extends App {
   trait ControlBuilder[ Proxy ] {
      def +=( p: Proxy ) : Unit
      def result : Seq[ Proxy ]
   }

   trait ControlType {
      type Proxy
      def newBuilder : ControlBuilder[ Proxy ]
   }

   object AudioControl extends ControlType {
      final case class Proxy( name: String )

      def newBuilder : ControlBuilder[ Proxy ] = new Factory

      private final class Factory extends ControlBuilder[ Proxy ] {
         var result = List.empty[ Proxy ]
         def +=( p: Proxy ) { result ::= p }
      }
   }

   object Def {
      val current = new ThreadLocal[ DefBuilder ]

      def apply( body: => Any ) : Def = {
         val b = new DefBuilder
         current.set( b )
         try {
            body
         } finally {
            current.remove()
         }
         b.build
      }
   }
   class DefBuilder {
      def getBuilder[ A ]( tpe: ControlType { type Proxy = A }) : ControlBuilder[ A ] = {

         sys.error( "How to do that???" )
      }

      def build : Def = {
         Def( IIdxSeq.empty )
      }
   }

   trait Controls {
      ctl =>
      type Proxy
      private type Tpe = ControlType { type Proxy = ctl.Proxy }
      def tpe: Tpe
      def proxies: Seq[ Proxy ]
   }

   case class Def( proxies: IIdxSeq[ Controls ])

   class RichString( s: String ) {
      def audio : AudioControl.Proxy = {
         val res = AudioControl.Proxy( s )
         Def.current.get().getBuilder( AudioControl ) += res
         res
      }
   }

   implicit def enrichString( s: String ) : RichString = new RichString( s )

   val df = Def { "freq".audio }
   println( df )
}

object ProxyStudy {
   trait ControlFactory[ Proxy ] {
      def make() : Proxy
      def result : Seq[ Proxy ]
   }

   trait ControlType {
      type Proxy
      def newFactory : ControlFactory[ Proxy ]
   }

   object AudioControl extends ControlType {
      final case class Proxy( name: String )

      def newFactory : ControlFactory[ Proxy ] = new Factory

      private final class Factory extends ControlFactory[ Proxy ] {
         var result = List.empty[ Proxy ]
         def make() = { val res = Proxy( util.Random.shuffle( "hallo_welt".toSeq ).mkString( "" )); result ::= res; res }
      }
   }

   object DefBuilder {
      def controlFactory[ A ]( tpe: ControlType { type Proxy = A }) : ControlFactory[ A ] = {



         sys.error( "How to do that???" )
      }
   }
}
