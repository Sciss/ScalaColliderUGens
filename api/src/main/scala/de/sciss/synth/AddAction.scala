/*
 *  AddAction.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

/** Add-actions are used by the server to determine where to place a node with
  * respect to other nodes. They form an enumeration of integers which are
  * represented by case objects being subclasses of this abstract class.
  *
  * The following actions are known:
  *
  *  - `0` - [[de.sciss.synth.addToHead addToHead]]
  *  - `1` - [[de.sciss.synth.addToTail addToTail]]
  *  - `2` - [[de.sciss.synth.addBefore addBefore]]
  *  - `3` - [[de.sciss.synth.addAfter addAfter]]
  *  - `4` - [[de.sciss.synth.addReplace addReplace]]
  */
sealed trait AddAction { def id: Int }

/** An `AddAction` with id `0`, indicating that a node should be add to the head of
  * of a target group.
  */
case object addToHead extends AddAction { final val id = 0 }
/** An `AddAction` with id `1`, indicating that a node should be add to the tail of
  * of a target group.
  */
case object addToTail extends AddAction { final val id = 1 }
/** An `AddAction` with id `2`, indicating that a node should be added to the same
  * group as the target node, right before it.
  */
case object addBefore extends AddAction { final val id = 2 }
/** An `AddAction` with id `3`, indicating that a node should be added to the same
  * group as the target node, right after it.
  */
case object addAfter extends AddAction { final val id = 3 }
/** An `AddAction` with id `4`, indicating that a node should replace an existing
  * node, that is take the target node's exact position in the tree.
  */
case object addReplace extends AddAction { final val id = 4 }
