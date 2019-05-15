/*
 *  VBAPSetup.scala
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

/*
  Translated from SuperCollider to Scala by H.H.Rutz in 2016.
  Original copyright notice:

  VBAP created by Ville Pukki
  This version ported from ver 0.99 PD code by Scott Wilson
  Development funded in part by the AHRC http://www.ahrc.ac.uk

  Copyright

  This software is being provided to you, the licensee, by Ville Pulkki,
  under the following license. By obtaining, using and/or copying this
  software, you agree that you have read, understood, and will comply
  with these terms and conditions: Permission to use, copy, modify and
  distribute, including the right to grant others rights to distribute
  at any tier, this software and its documentation for any purpose and
  without fee or royalty is hereby granted, provided that you agree to
  comply with the following copyright notice and statements, including
  the disclaimer, and that the same appear on ALL copies of the software
  and documentation, including modifications that you make for internal
  use or for distribution:


  Written by Ville Pulkki 1999
  Helsinki University of Technology
  and
  University of California at Berkeley

*/

package de.sciss.synth.ugen

import de.sciss.numbers.Implicits._

import scala.collection.{Seq => SSeq}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions
import scala.math.{abs, acos, cos, sin, sqrt}

object VBAPSetup {
  object Polar {
    implicit def fromAzi  (azi:  Double         ): Polar = Polar(azi = azi   , ele = 0.0   )
    implicit def fromTuple(tup: (Double, Double)): Polar = Polar(azi = tup._1, ele = tup._2)
  }
  case class Polar(azi: Double, ele: Double = 0.0) {
    def toCartesian: Cartesian = {
      val aziRad  = azi.toRadians
      val eleRad  = ele.toRadians
      val x       = cos(aziRad) * cos(eleRad)
      val y       = sin(aziRad) * cos(eleRad)
      val z       = sin(eleRad)
      Cartesian(x = x, y = y, z = z)
    }
  }

  case class Cartesian(x: Double, y: Double, z: Double) {
    /* length of a vector */
    def length: Double = sqrt(x.squared + y.squared + z.squared)

    /* vector dot product */
    def dotProduct(that: Cartesian): Double = (this.x * that.x) + (this.y * that.y) + (this.z * that.z)

    def angle(that: Cartesian): Double = {
      /* angle between two loudspeakers */
      val inner0  = (this dotProduct that) / (this.length * that.length)
      val inner   = inner0.clip(-1.0, 1.0)
      abs(acos(inner))
    }

    def normalize: Cartesian = {
      val l = length
      copy(x = x / l, y = y / l, z = z / l)
    }
  }

  /** Creates a new setup for a given dimensionality and speaker positions.
    * Usually one would use the `bufferData` value of the returned object
    * to fill the `Buffer` passed to the `VBAP` UGen.
    * See the `VBAP` documentation for an example.
    *
    * @param dim            either `2` or `3` dimensions.
    * @param directions     the speaker directions (azimuth for 2D, azimuth and elevation for 3D)
    * @return A setup with calculated buffer data
    */
  def apply(dim: Int, directions: SSeq[Polar],
            minSideLength: Double  = 0.01): VBAPSetup = {
    require(dim == 2 || dim == 3, s"Dimension must be 2 or 3")
    new Impl(dim = dim, directions = directions.toIndexedSeq,
      maxNumSpeakers = directions.size, minSideLength = minSideLength)
  }

  private class Impl(val dim: Int, val directions: Vec[Polar], maxNumSpeakers: Int, minSideLength: Double)
    extends VBAPSetup {

    override def toString: String = s"VBAPSetup(dim = $dim, directions = $directions)"

    val speakers    : Vec[Cartesian] = directions.map(_.toCartesian)
    val numSpeakers : Int           = speakers.size

    /* calculate and print out chosen loudspeaker sets and corresponding matrices */
    lazy val bufferData: Vec[Float] = {
      val arr = if (dim == 3) {
        val sets = choose_ls_triplets()
        calculate_3x3_matrixes(sets)
      } else /* if (dim == 2) */ {
        assert(dim == 2)
        choose_ls_tuplets()
      }
      arr.toIndexedSeq
    }

    type SpeakerSet = Array[Int]

    /* Selects the loudspeaker triplets, and
       calculates the inversion matrices for each selected triplet.
       A line (connection) is drawn between each loudspeaker. The lines
       denote the sides of the triangles. The triangles should not be
       intersecting. All crossing connections are searched and the
       longer connection is erased. This yields non-intesecting triangles,
       which can be used in panning.
       See theory in paper Pulkki, V. Lokki, T. "Creating Auditory Displays
       with Multiple Loudspeakers Using VBAP: A Case Study with
       DIVA Project" in International Conference on
       Auditory Displays -98.*/

    def choose_ls_triplets(): Vec[SpeakerSet] = {
      val connections       = Array.fill(maxNumSpeakers)(new Array[Int](maxNumSpeakers))
      // val angles            = new Array(maxNumSpeakers)
      // val sorted_angles     = new Array(maxNumSpeakers)
      val tableSize         = (maxNumSpeakers * (maxNumSpeakers - 1)) / 2
      val distance_table    = new Array[Double](tableSize)
      val distance_table_i  = new Array[Int   ](tableSize)
      val distance_table_j  = new Array[Int   ](tableSize)

      var sets = Vector.empty[SpeakerSet]
      for(i <- 0 until numSpeakers) {
        for(j <- i + 1 until numSpeakers) {
          for(k <- j + 1 until numSpeakers) {
            if(vol_p_side_lgth(i,j,k) > minSideLength) {
              connections(i)(j)=1
              connections(j)(i)=1
              connections(i)(k)=1
              connections(k)(i)=1
              connections(j)(k)=1
              connections(k)(j)=1
              //"i: % j: %, k: %\n".postf(i, j, k);
              sets :+= Array[Int](i,j,k)
            }
          }
        }
      }

      /*calculate distances between all lss and sorting them*/
      var table_size = ((numSpeakers - 1) * numSpeakers) / 2
      for(i <- 0 until table_size) {
        distance_table(i) = 100000.0
      }
      for(i <- 0 until numSpeakers) {
        for(j <- i+1 until numSpeakers) {
          if(connections(i)(j) == 1) {
            val distance = abs(vec_angle(speakers(i),speakers(j)))

            var k = 0
            while(distance_table(k) < distance) k += 1

            for(l <- table_size - 1 to k + 1 by -1) {
              distance_table  (l) = distance_table(l-1)
              distance_table_i(l) = distance_table_i(l-1)
              distance_table_j(l) = distance_table_j(l-1)
            }
            distance_table  (k) = distance
            distance_table_i(k) = i
            distance_table_j(k) = j
          } else {
            table_size = table_size - 1
          }
        }
      }

      /* disconnecting connections which are crossing shorter ones,
         starting from shortest one and removing all that cross it,
         and proceeding to next shortest */
      for(i <- 0 until table_size) {
        val fst_ls = distance_table_i(i)
        val sec_ls = distance_table_j(i)
        if(connections(fst_ls)(sec_ls) == 1) {
          for(j <- 0 until numSpeakers) {
            for(k <- j + 1 until numSpeakers) {
              if( (j!=fst_ls) && (k != sec_ls) && (k!=fst_ls) && (j != sec_ls)) {
                if(lines_intersect(fst_ls, sec_ls, j,k)) {
                  connections(j)(k) = 0
                  connections(k)(j) = 0
                }
              }
            }
          }
        }
      }

      /* remove triangles which had crossing sides
         with smaller triangles or include loudspeakers*/

//      println(s"triplet_amount before stripping: ${sets.size}")

      sets = sets.filterNot { set =>
        val i1 = set(0) // .chanOffsets(0)
        val j1 = set(1) // .chanOffsets(1)
        val k1 = set(2) // .chanOffsets(2)

        (connections(i1)(j1) == 0) || (connections(i1)(k1) == 0) || (connections(j1)(k1) == 0) ||
          any_ls_inside_triplet(i1,j1,k1)
      }
//      println(s"triplet_amount after stripping: ${sets.size}")
      
      sets
    }

    @inline def vec_angle(v1: Cartesian, v2: Cartesian): Double = v1 angle      v2
    @inline def vec_prod (v1: Cartesian, v2: Cartesian): Double = v1 dotProduct v2

    /* checks if two lines intersect on 3D sphere */
    def lines_intersect(i: Int, j: Int, k: Int, l: Int): Boolean = {
      val v1        = unq_cross_prod(speakers(i), speakers(j))
      val v2        = unq_cross_prod(speakers(k), speakers(l))
      val v3        = unq_cross_prod(v1, v2)

      val neg_v3    = Cartesian(x = -v3.x, y = -v3.y, z = -v3.z)

      val dist_ij   = vec_angle(speakers(i), speakers(j))
      val dist_kl   = vec_angle(speakers(k), speakers(l))
      val dist_iv3  = vec_angle(speakers(i), v3)
      val dist_jv3  = vec_angle(v3, speakers(j))
      val dist_inv3 = vec_angle(speakers(i), neg_v3)
      val dist_jnv3 = vec_angle(neg_v3, speakers(j))
      val dist_kv3  = vec_angle(speakers(k), v3)
      val dist_lv3  = vec_angle(v3, speakers(l))
      val dist_knv3 = vec_angle(speakers(k), neg_v3)
      val dist_lnv3 = vec_angle(neg_v3, speakers(l))

      /* if one of loudspeakers is close to crossing point, don't do anything*/
      val isClose = (abs(dist_iv3) <= 0.01) || (abs(dist_jv3) <= 0.01) ||
        (abs(dist_kv3) <= 0.01) || (abs(dist_lv3) <= 0.01) ||
        (abs(dist_inv3) <= 0.01) || (abs(dist_jnv3) <= 0.01) ||
        (abs(dist_knv3) <= 0.01) || (abs(dist_lnv3) <= 0.01)

      /* if crossing point is on line between both loudspeakers return 1 */
      !isClose && (((abs(dist_ij - (dist_iv3 + dist_jv3)) <= 0.01 ) &&
        (abs(dist_kl - (dist_kv3 + dist_lv3))  <= 0.01)) ||
        ((abs(dist_ij - (dist_inv3 + dist_jnv3)) <= 0.01)  &&
          (abs(dist_kl - (dist_knv3 + dist_lnv3)) <= 0.01 )))
    }

    /* calculate volume of the parallelepiped defined by the loudspeaker
       direction vectors and divide it with total length of the triangle sides.
       This is used when removing too narrow triangles. */
    def vol_p_side_lgth(i: Int, j: Int, k: Int): Double = {
      val xprod   = unq_cross_prod(speakers(i), speakers(j))
      val volper  = abs(vec_prod(xprod, speakers(k)))
      val lgth    = (abs(vec_angle(speakers(i), speakers(j)))
        + abs(vec_angle(speakers(i), speakers(k)))
        + abs(vec_angle(speakers(j), speakers(k))))
      if(lgth > 0.00001) volper / lgth else 0.0
    }

    //unq_cross_prod(t_ls v1,t_ls v2, t_ls *res)
    /* vector cross product */
    def unq_cross_prod(v1: Cartesian, v2: Cartesian): Cartesian = {
      val x0      = (v1.y * v2.z ) - (v1.z * v2.y)
      val y0      = (v1.z * v2.x ) - (v1.x * v2.z)
      val z0      = (v1.x * v2.y ) - (v1.y * v2.x)
      val pos0    = Cartesian(x = x0, y = y0, z = z0)
      pos0.normalize
    }

    /* returns true if there is loudspeaker(s) inside given ls triplet */
    def any_ls_inside_triplet(a: Int, b: Int, c: Int): Boolean = { // speakers, numSpeakers
      val lp1 =  speakers(a)
      val lp2 =  speakers(b)
      val lp3 =  speakers(c)

      val invmx = new Array[Double](9)

      /* matrix inversion */
      val invdet = 1.0 / (  lp1.x * ((lp2.y * lp3.z) - (lp2.z * lp3.y))
        - (lp1.y * ((lp2.x * lp3.z) - (lp2.z * lp3.x)))
        + (lp1.z * ((lp2.x * lp3.y) - (lp2.y * lp3.x))))

      invmx(0) = ((lp2.y * lp3.z) - (lp2.z * lp3.y)) * invdet
      invmx(3) = ((lp1.y * lp3.z) - (lp1.z * lp3.y)) * -invdet
      invmx(6) = ((lp1.y * lp2.z) - (lp1.z * lp2.y)) * invdet
      invmx(1) = ((lp2.x * lp3.z) - (lp2.z * lp3.x)) * -invdet
      invmx(4) = ((lp1.x * lp3.z) - (lp1.z * lp3.x)) * invdet
      invmx(7) = ((lp1.x * lp2.z) - (lp1.z * lp2.x)) * -invdet
      invmx(2) = ((lp2.x * lp3.y) - (lp2.y * lp3.x)) * invdet
      invmx(5) = ((lp1.x * lp3.y) - (lp1.y * lp3.x)) * -invdet
      invmx(8) = ((lp1.x * lp2.y) - (lp1.y * lp2.x)) * invdet

      var any_ls_inside = false
      for(i <- 0 until numSpeakers) {
        if((i != a) && (i != b) && (i != c)) {
          var this_inside = true
          for(j <- 0 to 2) {
            var tmp = speakers(i).x * invmx(0 + (j*3))
            tmp    += speakers(i).y * invmx(1 + (j*3))
            tmp    += speakers(i).z * invmx(2 + (j*3))
            if(tmp < -0.001) {
              this_inside = false
            }
          }
          if (this_inside) {
            any_ls_inside = true
          }
        }
      }
      any_ls_inside
    }

    /* Calculates the inverse matrices for 3D */
    def calculate_3x3_matrixes(sets: Vec[SpeakerSet]): Array[Float] = {
      val triplet_amount = sets.size
      require(triplet_amount > 0, "define-loudspeakers: Not valid 3-D configuration")

      //"triplet_amount: %\n".postf(triplet_amount);
      val list_length = triplet_amount * 21 + 2
      val result = new Array[Float](list_length)

      result(0) = dim
      result(1) = numSpeakers
      var pointer = 2

      sets.foreach { set =>
        val lp1 = speakers(set(0)) // [set.chanOffsets[0]];
        val lp2 = speakers(set(1)) // [set.chanOffsets[1]];
        val lp3 = speakers(set(2)) // [set.chanOffsets[2]];

        val invmx = new Array[Double](9)

        //"lp1x: % lp1y: % lp1z: %\n".postf(lp1.x, lp1.y, lp1.z);
        //"lp2x: % lp2y: % lp2z: %\n".postf(lp2.x, lp2.y, lp2.z);
        //"lp3x: % lp3y: % lp3z: %\n".postf(lp3.x, lp3.y, lp3.z);
        val invdet = 1.0 / (  (lp1.x * ((lp2.y * lp3.z) - (lp2.z * lp3.y)))
          - (lp1.y * ((lp2.x * lp3.z) - (lp2.z * lp3.x)))
          + (lp1.z * ((lp2.x * lp3.y) - (lp2.y * lp3.x))))

        //"invdet: %\n".postf(invdet);

        invmx(0) = ((lp2.y * lp3.z) - (lp2.z * lp3.y)) * invdet
        invmx(3) = ((lp1.y * lp3.z) - (lp1.z * lp3.y)) * -invdet
        invmx(6) = ((lp1.y * lp2.z) - (lp1.z * lp2.y)) * invdet
        invmx(1) = ((lp2.x * lp3.z) - (lp2.z * lp3.x)) * -invdet
        invmx(4) = ((lp1.x * lp3.z) - (lp1.z * lp3.x)) * invdet
        invmx(7) = ((lp1.x * lp2.z) - (lp1.z * lp2.x)) * -invdet
        invmx(2) = ((lp2.x * lp3.y) - (lp2.y * lp3.x)) * invdet
        invmx(5) = ((lp1.x * lp3.y) - (lp1.y * lp3.x)) * -invdet
        invmx(8) = ((lp1.x * lp2.y) - (lp1.y * lp2.x)) * invdet
        // set.inv_mx = invmx;
        for (i <- 0 until 3) {
          result(pointer) = set(i) + 1
          pointer += 1
        }
        for (i <- 0 until 9) {
          result(pointer) = invmx(i).toFloat
          pointer += 1
        }
        result(pointer) = lp1.x.toFloat; pointer += 1
        result(pointer) = lp2.x.toFloat; pointer += 1
        result(pointer) = lp3.x.toFloat; pointer += 1
        result(pointer) = lp1.y.toFloat; pointer += 1
        result(pointer) = lp2.y.toFloat; pointer += 1
        result(pointer) = lp3.y.toFloat; pointer += 1
        result(pointer) = lp1.z.toFloat; pointer += 1
        result(pointer) = lp2.z.toFloat; pointer += 1
        result(pointer) = lp3.z.toFloat; pointer += 1

      }
      result
    }

    /* selects the loudspeaker pairs, calculates the inversion
       matrices and stores the data to a global array*/
    def choose_ls_tuplets(): Array[Float] = {
      var amount = 0

      val exist   = new Array[Boolean](maxNumSpeakers)
      val inv_mat = Array.fill(maxNumSpeakers)(new Array[Double](4))

      /* sort loudspeakers according their azimuth angle */
      val (sorted_lss, aziOut) = sort_2D_lss()

      /* adjacent loudspeakers are the loudspeaker pairs to be used.*/
      for(i <- 0 to numSpeakers -2) {
        if((aziOut(sorted_lss(i+1)) - aziOut(sorted_lss(i))) <= (180 - 10)) {
          if(calc_2D_inv_tmatrix(aziOut(sorted_lss(i)), aziOut(sorted_lss(i+1)), inv_mat(i))) {
            exist(i)=true
            amount = amount + 1
          }
        }
      }

      if(((6.283 - aziOut(sorted_lss(numSpeakers-1))) + aziOut(sorted_lss(0))) <= (180 -  10)) {
        if(calc_2D_inv_tmatrix(aziOut(sorted_lss(numSpeakers-1)), aziOut(sorted_lss(0)), inv_mat(numSpeakers-1))) {
          exist(numSpeakers-1)=true
          amount += 1
        }
      }

      /* Output */
      val list_length= amount * 6 + 2
      val result = new Array[Float](list_length)

      result(0) = dim
      result(1) = numSpeakers
      var pointer=2

      for(i <- 0 to numSpeakers - 2) {
        if(exist(i)) {
          result(pointer) = sorted_lss(i)+1
          pointer = pointer + 1
          result(pointer) = sorted_lss(i+1)+1
          pointer = pointer + 1
          for(j <- 0 to 3) {
            result(pointer) = inv_mat(i)(j).toFloat
            pointer = pointer + 1
          }
        }
      }
      if(exist(numSpeakers-1)) {
        result(pointer) = sorted_lss(numSpeakers-1)+1
        pointer = pointer + 1
        result(pointer) = sorted_lss(0)+1
        pointer = pointer + 1
        for(j <- 0 to 3) {
          result(pointer) = inv_mat(numSpeakers-1)(j).toFloat
          pointer = pointer + 1
        }
      }
      result
    }

    /* sort loudspeakers according to azimuth angle */
    def sort_2D_lss(): (Array[Int], Array[Double]) = {

      val sorted_lss  = new Array[Int   ](maxNumSpeakers)
      val aziOut      = new Array[Double](numSpeakers)

      /* Transforming angles between -180 and 180 */
      for (i <- 0 until numSpeakers) {
        aziOut(i) = acos( speakers(i).x).toDegrees
        val tmp = if (abs(speakers(i).y) <= 0.001) {
          1.0
        } else {
          speakers(i).y / abs(speakers(i).y)
        }
        aziOut(i) *= tmp
      }
      for (i <- 0 until numSpeakers) {
        var tmp = 2000.0
        var index = 0
        for (j <- 0 until numSpeakers) {
          if (aziOut(j) <= tmp) {
            tmp   = aziOut(j)
            index = j
          }
        }
        sorted_lss(i) = index
        aziOut(index) += 4000.0
      }
      for (i <- 0 until numSpeakers) {
        aziOut(i) -= 4000.0
      }
      
      (sorted_lss, aziOut)
    }

    /* calculate inverse 2x2 matrix. Replaces contents of `inv_mat`! */
    def calc_2D_inv_tmatrix(azi1: Double, azi2: Double, inv_mat: Array[Double]): Boolean = {
      val x1 = cos(azi1.toRadians)
      val x2 = sin(azi1.toRadians)
      val x3 = cos(azi2.toRadians)
      val x4 = sin(azi2.toRadians)
      val det = (x1 * x4) - ( x3 * x2 )
      if(abs(det) <= 0.001) {
        inv_mat(0) = 0.0
        inv_mat(1) = 0.0
        inv_mat(2) = 0.0
        inv_mat(3) = 0.0
        false
      } else {
        inv_mat(0) =  x4 / det
        inv_mat(1) = -x3 / det
        inv_mat(2) = -x2 / det
        inv_mat(3) =  x1 / det
        true
      }
    }
  }
}
trait VBAPSetup {
  import VBAPSetup.Polar

  /** Either `2` or `3`. */
  def dim: Int

  /** The speaker directions as passed to `VBAPSetup.apply`. */
  def directions: Vec[Polar]

  /** Data that can be loaded into a `Buffer`, correctly formatted for the VBAP UGen. */
  def bufferData: Vec[Float]
}