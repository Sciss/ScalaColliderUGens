/*
 *  DoneAction.scala
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

import ugen.Constant

object DoneAction {
  import language.implicitConversions
  /** Done actions can be used as UGen inputs, in which case their identifier constant is used. */
  implicit def toGE(action: DoneAction): Constant = new Constant(action.id)
}

/** An enumeration of special codes used by some UGens to issue a related action when they they are "done".
  * For example, a `PlayBuf` can be instructed to free its enclosing synth when the buffer has been played
  * to the end, using the [[de.sciss.synth.freeSelf freeSelf]] done-action.
  *
  * The following actions are known:
  *
  *  - `0` - [[de.sciss.synth.doNothing doNothing]]
  *  - `1` - [[de.sciss.synth.pauseSelf pauseSelf]]
  *  - `2` - [[de.sciss.synth.freeSelf freeSelf]]
  *  - `3` - [[de.sciss.synth.freeSelfPred freeSelfPred]]
  *  - `4` - [[de.sciss.synth.freeSelfSucc freeSelfSucc]]
  *  - `5` - [[de.sciss.synth.freeSelfPredAll freeSelfPredAll]]
  *  - `6` - [[de.sciss.synth.freeSelfSuccAll freeSelfSuccAll]]
  *  - `7` - [[de.sciss.synth.freeSelfToHead freeSelfToHead]]
  *  - `8` - [[de.sciss.synth.freeSelfToTail freeSelfToTail]]
  *  - `9` - [[de.sciss.synth.freeSelfPausePred freeSelfPausePred]]
  *  - `10` - [[de.sciss.synth.freeSelfPauseSucc freeSelfPauseSucc]]
  *  - `11` - [[de.sciss.synth.freeSelfPredDeep freeSelfPredDeep]]
  *  - `12` - [[de.sciss.synth.freeSelfSuccDeep freeSelfSuccDeep]]
  *  - `13` - [[de.sciss.synth.freeAllInGroup freeAllInGroup]]
  *  - `14` - [[de.sciss.synth.freeGroup freeGroup]]
  */
sealed trait DoneAction extends Product {
  /** The identifier which is recognized by UGens such as `Done`. */
  def id: Int
  final val name: String = productPrefix
}

/** A `DoneAction` with id `0`, signifying that nothing should be done in particular when the UGen is finished. */
case object doNothing extends DoneAction { final val id = 0 }
/** A `DoneAction` with id `1`, pausing the enclosing synth when the UGen is finished. */
case object pauseSelf extends DoneAction { final val id = 1 }
/** A `DoneAction` with id `2`, freeing the enclosing synth when the UGen is finished. */
case object freeSelf extends DoneAction { final val id = 2 }
/** A `DoneAction` with id `3`, freeing the enclosing synth as well as the predecessor node
  * when the UGen is finished. */
case object freeSelfPred extends DoneAction { final val id = 3 }
/** A `DoneAction` with id `4`, freeing the enclosing synth as well as the successor node
  * when the UGen is finished. */
case object freeSelfSucc extends DoneAction { final val id = 4 }
/** A `DoneAction` with id `5`, freeing the enclosing synth when the UGen is finished.
  * If the predecessor node is a group, calls freeAll on
  * that group. If the predecssor node is a synth,
  * frees that synth.
  */
case object freeSelfPredAll extends DoneAction { final val id = 5 }
/** A `DoneAction` with id `6`, freeing the enclosing synth when the UGen is finished.
  * If the successor node is a group, calls freeAll on
  * that group. If the successor node is a synth,
  * frees that synth.
  */
case object freeSelfSuccAll extends DoneAction { final val id = 6 }
/** A `DoneAction` with id `7`, freeing the enclosing synth and all preceding nodes
  * in its group when the UGen is finished.
  */
case object freeSelfToHead extends DoneAction { final val id = 7 }
/** A `DoneAction` with id `8`, freeing the enclosing synth and all succeeding nodes
  * in its group when the UGen is finished.
  */
case object freeSelfToTail extends DoneAction { final val id = 8 }
/** A `DoneAction` with id `9`, freeing the enclosing synth and pauses the predecessor node
  * when the UGen is finished.
  */
case object freeSelfPausePred extends DoneAction { final val id = 9 }
/** A `DoneAction` with id `10`, freeing the enclosing synth and pauses the successor node
  * when the UGen is finished.
  */
case object freeSelfPauseSucc extends DoneAction { final val id = 10 }
/** A `DoneAction` with id `11`, freeing the enclosing synth when the UGen is finished.
  * If the predecessor node is a group, calls deepFree on
  * that group. If the predecessor node is a synth,
  * frees that synth.
  */
case object freeSelfPredDeep extends DoneAction { final val id = 11 }
/** A `DoneAction` with id `12`, freeing the enclosing synth when the UGen is finished.
  * If the successor node is a group, calls deepFree on
  * that group. If the successor node is a synth,
  * frees that synth.
  */
case object freeSelfSuccDeep extends DoneAction { final val id = 12 }
/** A `DoneAction` with id `13`, freeing the enclosing synth along with all other nodes
  * in the group when the UGen is finished (i.e. does
  * a freeAll on the group)
  */
case object freeAllInGroup extends DoneAction { final val id = 13 }
/** A `DoneAction` with id `14`, freeing the enclosing group when the UGen is finished,
  * and hence also frees this synth along with all other
  * nodes in the group.
  */
case object freeGroup extends DoneAction { final val id = 14 }
