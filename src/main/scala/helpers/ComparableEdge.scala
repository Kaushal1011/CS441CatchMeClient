package helpers

/**
 * This class is used to compare edges in the graph
 * @param srcId source node id
 * @param dstId destination node id
 * @param weight weight of the edge
 * @param propertiesSrc properties of the source node
 * @param propertiesDst properties of the destination node
 * @param children_prop_hash_source children properties of the source node
 * @param children_prop_hash_destination children properties of the destination node
 */
class ComparableEdge(val srcId: Int, val dstId: Int, val weight: Double,
                     val propertiesSrc: List[Int], val propertiesDst: List[Int],
                     val children_prop_hash_source: List[Int],
                     val children_prop_hash_destination: List[Int],
                     val valuableSrc: Boolean,
                     val valuableDst: Boolean)  {

  def SimRankJaccardSimilarity(other: ComparableEdge): Double = {
    val intersectionSrcProp = this.propertiesSrc.intersect(other.propertiesSrc).size
    val unionSrcProp = this.propertiesSrc ++ other.propertiesSrc.distinct

    val intersectionDstProp = this.propertiesDst.intersect(other.propertiesDst).size
    val unionDstProp = this.propertiesDst ++ other.propertiesDst.distinct

    val intersectionSrcChildren = this.children_prop_hash_source.intersect(other.children_prop_hash_source).size
    val unionSrcChildren = this.children_prop_hash_source ++ other.children_prop_hash_source.distinct

    val intersectionDstChildren = this.children_prop_hash_destination.intersect(other.children_prop_hash_destination).size
    val unionDstChildren = this.children_prop_hash_destination ++ other.children_prop_hash_destination.distinct

    val jaccardSrc: Double = if (unionSrcProp == 0) 0 else intersectionSrcProp.toDouble / unionSrcProp.size.toDouble
    val jaccardDst: Double = if (unionDstProp == 0) 0 else intersectionDstProp.toDouble / unionDstProp.size.toDouble
    val jaccardSrcChildren: Double = if (unionSrcChildren == 0) 0 else intersectionSrcChildren.toDouble / unionSrcChildren.size.toDouble
    val jaccardDstChildren: Double = if (unionDstChildren == 0) 0 else intersectionDstChildren.toDouble / unionDstChildren.size.toDouble

    val denominator: Int = Seq(unionSrcProp.size, unionDstProp.size, unionSrcChildren.size, unionDstChildren.size).count(_ > 0)

    val result = if (denominator == 0) {
      0.0
    } else {
      (jaccardSrc + jaccardDst + jaccardSrcChildren + jaccardDstChildren) / denominator
    }

    result
  }

  def getSrcId(): Int = srcId
  def getIdentifier(): (Int, Int) = if (srcId < dstId) (srcId, dstId) else (dstId, srcId)
  def getDstId(): Int = dstId
  def getWeight(): Double = weight
  def getPropertiesSrc(): List[Int] = propertiesSrc
  def getPropertiesDst(): List[Int] = propertiesDst
  def getChildrenPropHashSource(): List[Int] = children_prop_hash_source
  def getChildrenPropHashDestination(): List[Int] = children_prop_hash_destination

  def getValuableSrc(): Boolean = valuableSrc

  def getValuableDst(): Boolean = valuableDst

  override def toString: String = s"($srcId, $dstId, $weight, $propertiesSrc, $propertiesDst, $children_prop_hash_source, $children_prop_hash_destination, $valuableSrc, $valuableDst)"
}
