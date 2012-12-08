/*
 *  Buffer.scala
 *  (ScalaCollider)
 *
 *  Copyright (c) 2008-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

import de.sciss.synth.{ Completion => Comp }
import de.sciss.osc.{Bundle, Packet}

object Buffer {
//   sealed abstract class Completion {
//      private[Buffer] val message: Option[ Buffer => OSCMessage ]
//      private[Buffer] val action:  Option[ Buffer => Unit ]
//   }
   type Completion = Comp[ Buffer ]
   val NoCompletion = Comp[ Buffer ]( None, None )

   def alloc( server: Server = Server.default, numFrames: Int, numChannels: Int = 1,
              completion: Completion = NoCompletion ) : Buffer = {
      val b = apply( server )
      import Ops._
      b.alloc( numFrames, numChannels, completion )
      b
   }

   def read( server: Server = Server.default, path: String, startFrame: Int = 0, numFrames: Int = -1,
             completion: Completion = NoCompletion ) : Buffer = {
      val b = apply( server )
      import Ops._
      b.allocRead( path, startFrame, numFrames, completion )
      b
   }

   def cue( server: Server = Server.default, path: String, startFrame: Int = 0, numChannels: Int = 1,
            bufFrames: Int = 32768, completion: Completion = NoCompletion ) : Buffer = {
      val b = apply( server )
      import Ops._
      b.alloc( bufFrames, numChannels, b.cueMsg( path, startFrame, completion ))
      b
   }

   def readChannel( server: Server = Server.default, path: String, startFrame: Int = 0, numFrames: Int = -1,
                    channels: Seq[ Int ], completion: Completion = NoCompletion ) : Buffer = {
      val b = apply( server )
      import Ops._
      b.allocReadChannel( path, startFrame, numFrames, channels, completion )
      b
   }

   def apply( server: Server = Server.default ) : Buffer = {
      apply( server, allocID( server ))
   }

   private def allocID( server: Server ) : Int = {
      val id = server.allocBuffer( 1 )
      if( id == -1 ) {
         throw AllocatorExhaustedException( "Buffer: failed to get a buffer allocated (on " + server.name + ")" )
      }
      id
   }
}

final case class Buffer( server: Server, id: Int ) extends Model {
   b =>

   def this( server: Server = Server.default ) = this( server, Buffer.allocID( server ))

   import Buffer._

   private var released       = false
   private var numFramesVar   = -1
   private var numChannelsVar = -1
   private var sampleRateVar  = 0f

   override def toString = "Buffer(" + server + "," + id +
      (if( numFramesVar >= 0 ) ") : <" + numFramesVar + "," + numChannelsVar + "," + sampleRateVar + ">" else ")")

   def numFrames   = numFramesVar
   def numChannels = numChannelsVar
   def sampleRate  = sampleRateVar

   def register() {
       server.bufManager.register( this )
   }

   private[synth] def updated( change: BufferManager.BufferInfo ) {
      val info       = change.info
      numFramesVar   = info.numFrames
      numChannelsVar = info.numChannels
      sampleRateVar  = info.sampleRate
      dispatch( change )
   }

   def queryMsg = osc.BufferQueryMessage( id )

//   def free { server ! freeMsg }

   def freeMsg: osc.BufferFreeMessage = freeMsg( None, release = true )

   /**
    *    @param   release  whether the buffer id should be immediately returned to the id-allocator or not.
    *                      if you build a system that monitors when bundles are really sent to the server,
    *                      and you need to deal with transaction abortion, you might want to pass in
    *                      <code>false</code> here, and manually release the id, using the <code>release</code>
    *                      method
    */
	def freeMsg( completion: Optional[ Packet ] = None, release: Boolean = true ) = {
      if( release ) this.release()
      osc.BufferFreeMessage( id, completion )
	}

   /**
    *    Releases the buffer id to the id-allocator pool, without sending any
    *    OSCMessage. Use with great care.
    */
   def release() {
      if( released ) sys.error( this.toString + " : has already been freed" )
      server.freeBuffer( id )
      released = true
   }

	def closeMsg: osc.BufferCloseMessage = closeMsg( None )

	def closeMsg( completion: Option[ Packet ] = None ) =
      osc.BufferCloseMessage( id, completion )

	def allocMsg( numFrames: Int, numChannels: Int = 1, completion: Option[ Packet ] = None ) = {
      numFramesVar   = numFrames
      numChannelsVar = numChannels
      sampleRateVar  = server.sampleRate.toFloat
      osc.BufferAllocMessage( id, numFrames, numChannels, completion )
   }

   def allocReadMsg( path: String, startFrame: Int = 0, numFrames: Int = -1,
                     completion: Option[ Packet ] = None ) = {
//      this.cache;
//      path = argpath;
      osc.BufferAllocReadMessage( id, path, startFrame, numFrames, completion )
   }

   def allocReadChannelMsg( path: String, startFrame: Int = 0, numFrames: Int = -1, channels: Seq[ Int ],
                            completion: Option[ Packet ] = None ) = {
//      this.cache;
//      path = argpath;
      osc.BufferAllocReadChannelMessage( id, path, startFrame, numFrames, channels.toList, completion )
   }

   def cueMsg( path: String, startFrame: Int = 0, completion: Completion = NoCompletion ) =
      osc.BufferReadMessage( id, path, startFrame, numFrames, 0, leaveOpen = true, completion = makePacket( completion ))

   def readMsg( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
                leaveOpen: Boolean = false, completion: Option[ Packet ] = None ) =
      osc.BufferReadMessage( id, path, fileStartFrame, numFrames, bufStartFrame, leaveOpen, completion )

   def readChannelMsg( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
                leaveOpen: Boolean = false, channels: Seq[ Int ],
                completion: Option[ Packet ] = None ) =
      osc.BufferReadChannelMessage( id, path, fileStartFrame, numFrames, bufStartFrame, leaveOpen, channels.toList,
         completion )

   def setMsg( pairs: (Int, Float)* ) = {
      val numSmp = numChannels * numFrames
      require( pairs.forall( tup => (tup._1 >= 0 && tup._1 < numSmp) ))
      osc.BufferSetMessage( id, pairs: _* )
   }

   def setnMsg( v: IndexedSeq[ Float ]) = {
      val numSmp = numChannels * numFrames
      require( v.size == numSmp )
      osc.BufferSetnMessage( id, (0, v.toIndexedSeq) )
   }

   def setnMsg( pairs: (Int, IndexedSeq[ Float ])* ) = {
      val numSmp = numChannels * numFrames
      require( pairs.forall( tup => (tup._1 >= 0 && (tup._1 + tup._2.size) <= numSmp) ))
      val ipairs = pairs.map( tup => (tup._1, tup._2.toIndexedSeq ))
      osc.BufferSetnMessage( id, ipairs: _* )
   }
   
	def zeroMsg: osc.BufferZeroMessage = zeroMsg( None )

	def zeroMsg( completion: Option[ Packet ]) =
      osc.BufferZeroMessage( id, completion )

   def writeMsg( path: String, fileType: io.AudioFileType = io.AudioFileType.AIFF,
                 sampleFormat: io.SampleFormat = io.SampleFormat.Float, numFrames: Int = -1, startFrame: Int = 0,
                 leaveOpen: Boolean = false, completion: Option[ Packet] = None ) = {
//      require( isPowerOfTwo( this.numFrames ))
      osc.BufferWriteMessage( id, path, fileType, sampleFormat, numFrames, startFrame, leaveOpen, completion )
   }

   def makePacket( completion: Completion, forceQuery: Boolean = false ) : Option[ Packet ] = {
      val a = completion.action
      if( forceQuery || a.isDefined ) {
         register()
         a.foreach { action =>
            lazy val l: Model.Listener = {
               case BufferManager.BufferInfo( _, _ ) =>
                  removeListener( l )
                  action( b )
            }
            addListener( l )
         }
      }
      (completion.message, a) match {
         case (None, None)                => if( forceQuery ) Some( queryMsg ) else None
         case (Some( msg ), None)         => Some( if( forceQuery ) Bundle.now( msg.apply( b ), queryMsg ) else msg.apply( b ))
         case (None, Some( act ))         => Some( queryMsg )
         case (Some( msg ), Some( act ))  => Some( Bundle.now( msg.apply( b ), queryMsg ))
      }
   }
}