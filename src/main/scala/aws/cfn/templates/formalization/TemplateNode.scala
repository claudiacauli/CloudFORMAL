package aws.cfn.templates.formalization


sealed trait Node
sealed trait StackSetNode extends Node

  sealed trait GenericValueNode extends StackSetNode
    case object NoValue extends GenericValueNode with Entity
    sealed class ValueNode[T](value: T) extends GenericValueNode {
      override def toString: String = value.toString
    }
      final case class StringNode(value:String)     extends ValueNode[String](value)
      final case class BooleanNode(value: Boolean)  extends ValueNode[Boolean](value)
      final case class IntNode(value: Int)          extends ValueNode[Int](value)
      final case class FloatNode(value: Float)      extends ValueNode[Float](value)
      final case class DoubleNode(value : Double)   extends  ValueNode[Double](value)
      final case class LongNode(value: Long)        extends ValueNode[Long](value)
      final case class JsonNode(value: String)      extends ValueNode[String](value)
      final case class CommaDelimitedListNode(value: String) extends ValueNode[String](value)
      final case class TimeStampNode(value: String)   extends ValueNode[String](value)
      final case class ListNode[T<:Node](value: Vector[T])  extends GenericValueNode{
        override def toString: String = value.toString()
      }
      final case class MapNode[T<:Node](value: Map[String,T]) extends GenericValueNode{
        override def toString: String = value.toString()
      }


  sealed trait ObjectNode extends StackSetNode

    sealed trait Entity extends ObjectNode

      sealed trait Principal extends Entity

      case object Public extends Principal {
        override def toString : String = "Public"
      }

      final case class ServicePrincipal(name:String) extends Principal {
        override def toString: String = "ServicePrincipal(" + name + ")"
      }

      final case class AccountPrincipal(accountId:String) extends Principal{
        override def toString: String = "AccountPrincipal("+accountId+")"
      }

      final case class FederatedAccountPrincipal(federation:String) extends Principal{
        override def toString: String = "FederatedAccountPrincipal("+federation+")"
      }

      final case class CanonicalUserPrincipal(canonicalUserId:String) extends Principal{
        override def toString: String = "CanonicalUserPrincipal("+canonicalUserId+")"
      }

      sealed trait Resource extends Principal with Entity

      final case class ExternalResource(name:String, infrastructure:Infrastructure=null) extends Resource {
        override def toString: String = "ExternalEntity(" + name + ")"
      }

      final case class StackSetResource(resourceLogicalId : String,
                                        serviceType : String,
                                        resourceType : String,
                                        stackset : StackSet,
                                        attributes : Map[String, GenericValueNode]
                                   ) extends Resource
      {
        val value: String = resourceLogicalId
        var resourceName : String = resourceLogicalId
        var givenProperties: Map[String,Node] = Map()
        var absentProperties: Set[String] = Set()
        def apply(): StackSetResource = this

        override def toString: String = {
         serviceType + "::" + resourceType + "(" + resourceLogicalId + ")"
        }
      }

    final case class ListOfEntities(nodes:Vector[Resource]) extends Resource with Entity

    final case class Subproperty(givenProperties  : Map[String,Node],
                                 absentProperties : Set[String] = Set()) extends ObjectNode
    {
      def apply(): Subproperty = this
    }

    final case class PolicyDocument(statements: Set[Statement]) extends ObjectNode
    {
      def apply(): PolicyDocument = this
    }



    sealed abstract class Statement ( val principals: (Boolean,Set[Principal]),
                                      val actions: (Boolean,Vector[String]),
                                      val resources: (Boolean, Vector[Resource]),
                                      val hasCondition: Boolean,
                                      val isAssumeRoleStatement: Boolean )  extends ObjectNode {

      def list(l: Vector[Any]): String = {
        l.foldLeft("")((a,b)=>a+b+" ")
      }

      def pretty(a: (Boolean,Vector[Any])) : String = {
        if (!a._1) " NOT { " + list(a._2) + " }" else " { " + list(a._2) + " }"
      }

      def listS(l: Set[Principal]): String = {
        l.foldLeft("")((a,b)=>a+b+" ")
      }

      def prettyS(a: (Boolean,Set[Principal])) : String = {
        if (!a._1) " NOT { " + listS(a._2) + " }" else " { " + listS(a._2) + " }"
      }

      def prettyB(hasC: Boolean): String = {
        if (hasC) "\n under a condition."
        else "\n with NO condition."
      }

    }

      final case class AllowStatement(p: (Boolean,Set[Principal]),
                                      a: (Boolean,Vector[String]),
                                      r: (Boolean, Vector[Resource]),
                                      hasC: Boolean ,
                                      isARS: Boolean) extends Statement(p,a,r,hasC,isARS){
        override def toString: String = {
          "Allows \n principals " + prettyS(p) +
            "\n to perform actions " + pretty(a) + "\n on resources " + pretty(r) + prettyB(hasC)
        }
      }

      final case class DenyStatement(p: (Boolean,Set[Principal]),
                                     a: (Boolean,Vector[String]),
                                     r: (Boolean, Vector[Resource]),
                                     hasC: Boolean ,
                                     isARS: Boolean) extends Statement(p,a,r,hasC,isARS){
        override def toString: String = {
          "Denies \n principals" + prettyS(p) +
            "\n to perform actions " + pretty(a) + "\n on resources " + pretty(r) + prettyB(hasC)
        }
      }