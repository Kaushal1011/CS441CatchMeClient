package helpers

/**
 * This class is used to compare nodes in the graph
 * @param id - id of the node
 * @param incoming_nodes_len - number of incoming nodes
 * @param outgoing_node_len - number of outgoing nodes
 * @param children_props_hash - hash of children properties
 * @param properties - properties of the node
 * @param graphType - type of the graph (original or reversed)
 */
class ComparableNode(val id: Int, val incoming_nodes_len: Int, val outgoing_node_len: Int,
                     val children_props_hash: List[Int], val properties: List[Int],
                     val valuableData: Boolean, val graphType: String = "original")  extends Serializable {

  /**
   * This function is used to calculate the similarity between two nodes
   * @param other - the other node to compare with
   * @return
   */
  def SimRankFromJaccardSimilarity(other: ComparableNode): Double = {

    // similarity of children properties
    val intersectionChildProp = children_props_hash.intersect(other.children_props_hash)
    val unionChildProp = children_props_hash ++ other.children_props_hash

    // similarity of properties
    val intersectionProps = properties.intersect(other.properties)
    val unionProps = properties ++ other.properties

    // Jaccard similarity for child properties
    val jaccardSimilarityChildProp =
      if (unionChildProp.isEmpty) 0.0
      else intersectionChildProp.size.toDouble / unionChildProp.distinct.size.toDouble

    // Jaccard similarity for properties
    val jaccardSimilarityProps =
      if (unionProps.isEmpty) 0.0
      else intersectionProps.size.toDouble / unionProps.distinct.size.toDouble

    // denominator logic: Number of non-zero unions
    val denominator: Int = Seq(unionChildProp.distinct.size, unionProps.distinct.size).count(_ > 0)

    val simScore =
      if (denominator == 0) 0.0
      else (jaccardSimilarityChildProp + jaccardSimilarityProps) / denominator

    simScore
  }

  def get_id(): Int = id

  def get_number_of_incoming_nodes(): Int = incoming_nodes_len

  def get_number_of_outgoing_nodes(): Int = outgoing_node_len

  def get_children_props_hash(): List[Int] = children_props_hash

  def get_properties(): List[Int] = properties

  def get_valuable_data(): Boolean = valuableData

  /**
   * This function string encodes the node
   * @return - string encoding of the node
   */
  override def toString: String = s"($id, $incoming_nodes_len, $outgoing_node_len, $children_props_hash, $properties, $valuableData)"
}
