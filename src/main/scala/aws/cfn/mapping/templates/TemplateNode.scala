package aws.cfn.mapping.templates

private[templates]
sealed trait Node


  private[templates]
  sealed trait GenericValueNode
    extends Node


    private[templates]
    case object NoValue
      extends GenericValueNode
        with Entity


    private[templates]
    sealed class ValueNode[T](value: T)
      extends GenericValueNode
    {
      override def toString: String = value.toString
    }


      private[templates]
      final case class StringNode(value:String)
        extends ValueNode[String](value)

      private[templates]
      final case class BooleanNode(value: Boolean)
        extends ValueNode[Boolean](value)

      private[templates]
      final case class IntNode(value: Int)
        extends ValueNode[Int](value)

      private[templates]
      final case class FloatNode(value: Float)
        extends ValueNode[Float](value)

      private[templates]
      final case class DoubleNode(value : Double)
        extends ValueNode[Double](value)

      private[templates]
      final case class LongNode(value: Long)
        extends ValueNode[Long](value)

      private[templates]
      final case class JsonNode(value: String)
        extends ValueNode[String](value)

      private[templates]
      final case class CommaDelimitedListNode(value: String)
        extends ValueNode[String](value)

      private[templates]
      final case class TimeStampNode(value: String)
        extends ValueNode[String](value)

      private[templates]
      final case class ListNode[T<:Node](value: Vector[T])
        extends GenericValueNode
      {
        override def toString: String = value.toString()
      }

      private[templates]
      final case class MapNode[T<:Node](value: Map[String,T])
        extends GenericValueNode
      {
        override def toString: String = value.toString()
      }




  private[templates]
  sealed trait ObjectNode
    extends Node


    private[templates]
    sealed trait Entity
      extends ObjectNode


      private[templates]
      sealed trait Principal
        extends Entity


      private[templates]
      case object Public
        extends Principal
      {
        override def toString : String = "Public"
      }


      private[templates]
      final case class ServicePrincipal(name:String)
        extends Principal
      {
        override def toString: String = "ServicePrincipal(" + name + ")"
      }


      private[templates]
      final case class AccountPrincipal(accountId:String)
        extends Principal
      {
        override def toString: String = "AccountPrincipal("+accountId+")"
      }


      private[templates]
      final case class FederatedAccountPrincipal(federation:String)
        extends Principal
      {
        override def toString: String = "FederatedAccountPrincipal("+federation+")"
      }


      private[templates]
      final case class CanonicalUserPrincipal(canonicalUserId:String)
        extends Principal
      {
        override def toString: String = "CanonicalUserPrincipal("+canonicalUserId+")"
      }


      private[templates]
      sealed trait Resource
        extends Principal


      private[templates]
      final case class ExternalResource(name:String, infrastructure:Infrastructure=null)
        extends Resource
      {
        override def toString: String = "ExternalEntity(" + name + ")"
      }


      private[templates]
      final case class StackSetResource
      (resourceLogicalId : String, serviceType : String,
       resourceType : String, stackset: StackSet,
       attributes : Map[String, GenericValueNode])
        extends Resource
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


    private[templates]
    final case class ListOfResources(nodes:Vector[Resource])
      extends Resource


    private[templates]
    final case class Subproperty
    (givenProperties  : Map[String,Node],
     absentProperties : Set[String] = Set())
      extends ObjectNode
    {
      def apply(): Subproperty = this
    }


    private[templates]
    final case class PolicyDocument(statements: Set[Statement])
      extends ObjectNode
    {
      def apply(): PolicyDocument = this
    }



    private[templates]
    sealed abstract class Statement ( val principals: (Boolean,Set[Principal]),
                                      val actions: (Boolean,Vector[String]),
                                      val resources: (Boolean, Vector[Resource]),
                                      val hasCondition: Boolean,
                                      val isAssumeRoleStatement: Boolean )
      //extends ObjectNode
    {

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

      private[templates]
      final case class AllowStatement(p: (Boolean,Set[Principal]),
                                      a: (Boolean,Vector[String]),
                                      r: (Boolean, Vector[Resource]),
                                      hasC: Boolean ,
                                      isARS: Boolean)
        extends Statement(p,a,r,hasC,isARS)
      {
        override def toString: String = {
          "Allows \n principals " + prettyS(p) +
            "\n to perform actions " + pretty(a) + "\n on resources " + pretty(r) + prettyB(hasC)
        }
      }



      private[templates]
      final case class DenyStatement(p: (Boolean,Set[Principal]),
                                     a: (Boolean,Vector[String]),
                                     r: (Boolean, Vector[Resource]),
                                     hasC: Boolean ,
                                     isARS: Boolean)
        extends Statement(p,a,r,hasC,isARS)
      {
        override def toString: String = {
          "Denies \n principals" + prettyS(p) +
            "\n to perform actions " + pretty(a) + "\n on resources " + pretty(r) + prettyB(hasC)
        }
      }
