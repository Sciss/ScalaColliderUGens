///*
// *  ProductWithType.scala
// *  (ScalaColliderUGens)
// *
// *  Copyright (c) 2008-2021 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Lesser General Public License v2.1+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth
//
///** Allows implementations of `GE` or `UGenSource` to have a more efficient
//  * serialization, based on a registered type identifier.
//  * The special value `-1` is reserved for types that are not yet registered,
//  * causing a fallback to name based serialization.
//  */
//trait ProductWithId extends Product {
//  def typeId: Int
//}
