package aws.cfn.templates.encoding

import argonaut.Json
import aws.cfn.templates.formalization._

class Json2PolicyDocumentEncoder(ssE:Json2StackSetEncoder, tE:Json2TemplateEncoder, rE:Json2ResourceEncoder, nE:Json2NodeEncoder) {

  def encode(jsonPolicy:Json, resourcesIsAttachedTo: Set[StackSetResource]): PolicyDocument = {
      // TODO

    // Nodes might be arrays or single values/objects
    // Nodes might contain arns or intrinsic functions and therefore need to be encoded.
    // Nodes in resources and principals might / should evaluate to resources or set of them

    println( "\n\n\nPolicy attached to: " + resourcesIsAttachedTo )
    val hasArrayOfStatements = jsonPolicy.field("Statement").get.isArray
    val statements : Vector[Json] =
      if (hasArrayOfStatements)
        jsonPolicy.field("Statement").get.array.get.toVector
      else
        Vector(jsonPolicy.field("Statement").get)


    println("Policy has statements: ")
    statements foreach println
    statements foreach encodeStatement

    PolicyDocument(Vector())
  }


  private def encodeStatement(statementJsonNode : Json) : Statement = {

    def principals : Option[(Boolean,Json)] = {
      statementJsonNode.field("Principal") match {
        case Some(princBlock) => Some(true,princBlock)
        case None => statementJsonNode.field("NotPrincipal") match {
          case Some (notPrincBlock) => Some(false,notPrincBlock)
          case None => None
        }
      }
    }

    def actions : Option[(Boolean,Json)] = {
      statementJsonNode.field("Action") match {
        case Some(aBlock) => Some(true,aBlock)
        case None => statementJsonNode.field("NotAction") match {
          case Some(notABlock) => Some(false,notABlock)
          case None => None
        }
      }
    }

    def resources : Option[(Boolean,Json)] = {
      statementJsonNode.field("Resource") match {
        case Some(resBlock) => Some(true,resBlock)
        case None => statementJsonNode.field("NotResouce") match {
          case Some (notResBlock) => Some(false,notResBlock)
          case None => None
        }
      }
    }

    resources match {
      case Some((b,j)) => println("Resource block: " + (b,nE.encode(j)))
      case None => println("Resource block omitted")
    }

    actions match {
      case Some(a) => println("actions block " + a)
      case None => println("No actions")
    }

    principals match {
      case Some(p) => println("Principals " + p )
      case None => println("Principal is omitted")
    }

//    def listOfResourcesFromBlock(block: Json) : Vector[Node] = {
//      val vecJsons = if (block.isArray) block.array.get.toVector else Vector(block)
//      val evalResources = vecJsons flatMap  ( j => nE.encode(j) match {
//        case s:StackSetResource => Vector(s)
//        case f:ForeignResource => Vector(f)
//        case _ => Vector()
//      } )
//      evalResources foreach println
//      evalResources
//    }
//
//    resources match {
//      case Some((b,list)) => println("Resource block points to " + list)
//      case None => println("Resources are omitted")
//    }

    statementJsonNode.field("Effect").get.string.get.toLowerCase match {
      case "allow"  => encodeAllowStatement(statementJsonNode)
      case "deny"   => encodeDenyStatement(statementJsonNode)
    }

  }


  private def encodeAllowStatement(statementJson : Json): AllowStatement = {
    null
  }


  private def encodeDenyStatement(statementJson : Json) : DenyStatement = {
    null
  }


}
