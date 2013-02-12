/*
 *  UGenSource.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2013 Hanns Holger Rutz. All rights reserved.
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

import collection.immutable.{IndexedSeq => IIdxSeq}

object UGenSource {
  trait ZeroOut extends UGenSource[Unit] {
    final protected def rewrap(args: IIdxSeq[UGenInLike], exp: Int) {
      var i = 0;
      while (i < exp) {
        unwrap(args.map(_.unwrap(i)))
        i += 1
      }
    }
  }

  @SerialVersionUID(5726777539636001639L) trait SingleOut extends SomeOut

  trait MultiOut extends SomeOut

  protected sealed trait SomeOut extends UGenSource[UGenInLike] with GE.Lazy {
    final protected def rewrap(args: IIdxSeq[UGenInLike], exp: Int): UGenInLike =
      UGenInGroup(IIdxSeq.tabulate(exp)(i => unwrap(args.map(_.unwrap(i)))))
  }

}

sealed trait UGenSource[U] extends Lazy.Expander[U] with Product {
  protected def makeUGen(args: IIdxSeq[UGenIn]): U

  final def name: String = productPrefix

  final protected def unwrap(args: IIdxSeq[UGenInLike]): U = {
    var uins = IIdxSeq.empty[UGenIn]
    var uinsOk = true
    var exp = 0
    args.foreach(_.unbubble match {
      case u: UGenIn => if (uinsOk) uins :+= u
      case g: UGenInGroup =>
        exp = math.max(exp, g.numOutputs)
        uinsOk = false // don't bother adding further UGenIns to uins
    })
    if (uinsOk) {
      // aka uins.size == args.size
      makeUGen(uins)
    } else {
      rewrap(args, exp)
    }
  }

  protected def rewrap(args: IIdxSeq[UGenInLike], exp: Int): U
}