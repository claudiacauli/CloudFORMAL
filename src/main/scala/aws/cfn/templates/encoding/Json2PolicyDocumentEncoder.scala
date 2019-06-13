package aws.cfn.templates.encoding

import java.util.regex.Pattern

import argonaut.Json
import aws.cfn.maps.ActionsMap
import aws.cfn.templates.formalization._



class Json2PolicyDocumentEncoder( iE:Json2InfrastructureEncoder, ssE:Json2StackSetEncoder,
                                  tE:Json2TemplateEncoder, rE:Json2ResourceEncoder, nE:Json2NodeEncoder,
                                  jsonPolicy:Json, parentResource: StackSetResource ) {

  var principals    : (Boolean,Set[Principal]) = _
  var actions       : (Boolean,Vector[String]) = _
  var resources     : (Boolean,Vector[Resource]) = _
  var hasCondition  : Boolean = _
  var isAssumeRoleStatement : Boolean = _
  var resourcesIsAttachedTo : Set[Principal] = _
  var policyDocument : PolicyDocument = _

  def createNode() : PolicyDocument = policyDocument

  def encode(): Unit = {

    resourcesIsAttachedTo  =
      if (iE.resourcesPointingToPolicy.get(parentResource).isDefined)
        iE.resourcesPointingToPolicy(parentResource).toSet
      else Set(parentResource)

      // TODO

    // Nodes might be arrays or single values/objects
    // Nodes might contain arns or intrinsic functions and therefore need to be encoded.
    // Nodes in resources and principals might / should evaluate to resources or set of them

    val hasArrayOfStatements = jsonPolicy.field("Statement").get.isArray
    val statements : Set[Json] =
      if (hasArrayOfStatements)
        jsonPolicy.field("Statement").get.array.get.toSet
      else
        Set(jsonPolicy.field("Statement").get)

    val stmtsVector = statements map (j => encodeStatement(j))
//    println("\n POLICY (" + ssE.stackSet.name + " - " + tE.template.name + ")" + "\n "+jsonPolicy)
//    stmtsVector foreach println

    iE.policyStatements = iE.policyStatements ++ stmtsVector

    iE.infrastructure.policies = iE.infrastructure.policies ++
      Set((ssE.stackSet.name,tE.template.name,jsonPolicy,stmtsVector))

    policyDocument = PolicyDocument(stmtsVector)
  }


  private def encodeStatement(statementJsonNode : Json) : Statement = {

    isAssumeRoleStatement =
      isResourceOmitted(statementJsonNode) && isAttachedToRole(statementJsonNode)
    hasCondition  =
      statementJsonNode.hasField("Condition")

    if (isStarActionBlock(statementJsonNode)){
      resources     = computeResources(statementJsonNode)
      actions       = computeActions(statementJsonNode)
    } else {
      actions       = computeActions(statementJsonNode)
      resources     = computeResources(statementJsonNode)
    }

    principals    = computePrincipals(statementJsonNode)

    val allFieldsDefined = true
    //val allFieldsDefined = resources._2.nonEmpty && actions._2.nonEmpty && principals._2.nonEmpty

    if (allFieldsDefined && statementJsonNode.field("Effect").get.string.get.toLowerCase == "allow")
      AllowStatement(principals,actions,resources,hasCondition,isAssumeRoleStatement)
    else
      DenyStatement(principals,actions,resources,hasCondition,isAssumeRoleStatement)

  }



  private def computePrincipals(json: Json) : (Boolean,Set[Principal]) = {

    val isPrincipalBlock    = json.hasField("Principal")
    val isNotPrincipalBlock = json.hasField("NotPrincipal")

    def isStarPrincipalBlock(j:Json) : Boolean = {
      ( j.hasField("Principal") && j.field("Principal").get.isString && j.field("Principal").get.string.get == "*" ) ||
        ( j.hasField("NotPrincipal") && j.field("NotPrincipal").get.isString && j.field("NotPrincipal").get.string.get == "*" )
    }

    def computePrincipalSet : Set[Principal] = {
      isStarPrincipalBlock(json) match {
        case false  => computeNormalSet
        case true   => computeStarSet
      }
    }


    def computeStarSet : Set[Principal] = {
      Set(Public)
    }


    def computeNormalSet : Set[Principal] = {

//      def nodesFromCanonicalPrincipal(j:Json) : Set[Principal]  = Set()  // TODO
//      def nodesFromFederatedPrincipal(j: Json) : Set[Principal] = Set() // TODO
//      def nodesFromServicePrincipal(j: Json) : Set[Principal]  = {
//
//        def singleServicePrincipal(j:Json) : Set[Principal] = {
//          nE.encode(j) match {
//            case e:Entity       => Set(e)
//            case StringNode(s)  => Set(ServicePrincipal(s,iE.infrastructure))
//            case x => println("The function encode to encode a json node did not return an entity but: " + x); Set()
//          }
//        }
//        if (j.isArray)
//          j.array.get.toSet flatMap singleServicePrincipal
//        else
//          singleServicePrincipal(j)
//      }

//      def nodesFromAwsPrincipal(j : Json): Set[Principal] = {
//
//        def singleAwsPrincipal(j:Json): Set[Principal] = {
//          val encodedJson = nE.encode(j)
//          encodedJson match {
//            case r:StackSetResource               => Set(r)
//            case f:ExternalResource      => Set(f)
//            case l:ListNode[Principal]   => l.value.toSet
//            case l:ListOfEntities      => l.nodes.toSet
//            case StringNode(account)   =>
//              (for {ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
//                   if tE.parameters("aws::accountid").asInstanceOf[StringNode].value == account} yield {
//                r._2
//              }).toSet
//          }
//        }
//
//        if (j.isArray)
//          j.array.get.toSet flatMap singleAwsPrincipal
//        else
//          singleAwsPrincipal(j)
//
//      }

      def principalsFromSingleNode(j:Json) : Set[Principal] = {

        if ( isServicePrincipal(j) ){
          nE.encode(j.field("Service").get) match {
            case StringNode(v) => Set(ServicePrincipal(v))
            case ListNode(v) if v.isInstanceOf[Vector[StringNode]]
              => v.toSet.asInstanceOf[Set[StringNode]] flatMap (sn => Set(ServicePrincipal(sn.value)))
            case r:Resource => Set(r)
            case _ => println("Weird. The evaluation of a node that should resolve to a service principal id wasn't a string node or list thereof.")
              Set()
          }
        }

        else if (isAwsPrincipal(j)){
          nE.encode(j.field("AWS").get) match {
            case StringNode(v) => Set(AccountPrincipal(v))
            case ListNode(v) if v.isInstanceOf[Vector[StringNode]]
              => v.toSet.asInstanceOf[Set[StringNode]] flatMap (sn => Set(AccountPrincipal(sn.value)))
            case r:Resource => Set(r)
            case _ =>
              println("Weird. The evaluation of a node that should resolve to an account id wasn't a string node or list thereof.")
              Set()
          }
        }

        else if (isFederatedPrincipal(j)) {
          nE.encode(j.field("Federated").get) match {
            case StringNode(v) => Set(FederatedAccountPrincipal(v))
            case ListNode(v) if v.isInstanceOf[Vector[StringNode]]
            => v.toSet.asInstanceOf[Set[StringNode]] flatMap (sn => Set(FederatedAccountPrincipal(sn.value)))
            case r:Resource => Set(r)
            case _ => println("Weird. The evaluation of a node that should resolve to a federation string id wasn't a string node or list thereof.")
              Set()
          }
        }

        else if (isCanonicalUserPrincipal(j)) {
          nE.encode(j.field("CanonicalUser").get) match {
            case StringNode(v) => Set(CanonicalUserPrincipal(v))
            case ListNode(v) if v.isInstanceOf[Vector[StringNode]]
            => v.toSet.asInstanceOf[Set[StringNode]] flatMap (sn => Set(CanonicalUserPrincipal(sn.value)))
            case r:Resource => Set(r)
            case _ => println("Weird. The evaluation of a node that should resolve to a canonical user id wasn't a string node or list thereof.")
              Set()
          }
        }

        else                                    {
          nE.encode(j) match {
            case r:Resource => Set(r)
            case StringNode(s) => println("The function returned a stringnode! with value " + s); Set()
            case x => println("The function to encode the node did not return an entity but " + x); Set()
          }
        }
      }


      if (isPrincipalBlock && !json.field("Principal").get.isArray){
        principalsFromSingleNode(json.field("Principal").get)
      }
      else if (isPrincipalBlock)
        json.field("Principal").get.array.get.toSet flatMap principalsFromSingleNode
      else if (isNotPrincipalBlock && !json.field("NotPrincipal").get.isArray)
        principalsFromSingleNode(json.field("NotPrincipal").get)
      else
        json.field("NotPrincipal").get.array.get.toSet flatMap principalsFromSingleNode

    }

    isPrincipalBlock || isNotPrincipalBlock match {
      case false  => (true,resourcesIsAttachedTo)
      case true   => (isPrincipalBlock,computePrincipalSet)
    }
  }



  private def computeActions(json: Json) : (Boolean,Vector[String]) = {

    val isActionBlock = json.hasField("Action")
    val isNotActionBlock = json.hasField("NotAction")

    def computeActionSet : Set[String] = {
      isStarActionBlock(json) match {
        case false  => computeNormalSet
        case true   => computeStarSet
      }
    }

    def computeNormalSet : Set[String] = {

      def actionsFromJsonString ( s : String ) : Set[String] = {
        if (s.toLowerCase == "sts:assumerole")
          Set("sts:AssumeRole")
        else {
          var service = s.split(":").head
          var actionNamePattern = s.split(":",-1).last
          actionNamePattern = actionNamePattern.replaceAll("\\*",  "[a-zA-Z]*")
          val actPattern = Pattern.compile( actionNamePattern ,
            Pattern.CASE_INSENSITIVE)

          ActionsMap.lookUpActionPrefix(service) filter
            ( actStr => actPattern.matcher(actStr.split(":").last).matches())
        }
      }

      if (isActionBlock && !json.field("Action").get.isArray){
        actionsFromJsonString( json.field("Action").get.string.get )
      }
      else if (isActionBlock)
        (json.field("Action").get.array.get.toVector flatMap ( j => actionsFromJsonString(j.string.get) )).toSet
      else if (isNotActionBlock && !json.field("NotAction").get.isArray)
        actionsFromJsonString( json.field("NotAction").get.string.get )
      else
        (json.field("NotAction").get.array.get.toVector flatMap ( j => actionsFromJsonString(j.string.get) )).toSet

    }

    def computeStarSet : Set[String] = {
      val servType  = resources._2.head.asInstanceOf[StackSetResource].serviceType
      ActionsMap.lookUpActionPrefix(servType)
    }

    isActionBlock || isNotActionBlock match {
      case false  => (true,Vector())
      case true   => (isActionBlock,computeActionSet.toVector)
    }

  }



  private def computeResources(json: Json) : (Boolean,Vector[Resource]) = {


    def computeResourceSet: Set[Resource] = {
      isStarResourceBlock(json) match {
        case false => computeNormalSet
        case true  => computeStarSet
      }
    }

    def ifNotEntityEmptySet(encodedNone : Node) : Set[Resource] = {
      encodedNone match {
        case ListOfEntities(v) => v.toSet
        case r:Resource => Set(r)
        case StringNode(s) => println("The function returned a stringnode! with value " + s); Set()
        case x => println("The function to encode the node did not return entity but " + x) ; Set()
      }
    }

    def computeNormalSet : Set[Resource] = {

        if (isResourceBlock(json) && !json.field("Resource").get.isArray)
          ifNotEntityEmptySet(nE.encode(json.field("Resource").get))
        else if (isResourceBlock(json) )
          (json.field("Resource").get.array.get.toVector flatMap (j => ifNotEntityEmptySet(nE.encode(j)) )).toSet
        else if (isNotResourceBlock(json) && !json.field("NotResource").get.isArray)
          ifNotEntityEmptySet(nE.encode(json.field("NotResource").get))
        else
          (json.field("NotResource").get.array.get.toVector flatMap (j=> ifNotEntityEmptySet(nE.encode(j)) )).toSet

    }


    def computeStarSet : Set[Resource] = {

      if (actions._2.nonEmpty) {

        var service = actions._2.head.split(":").head
        var resType :String = null
        if (actions._2.head.toLowerCase == "sts:assumerole") {
          service = "iam"
          resType = "role"
        } else if (actions._2.head.toLowerCase == "sts:getcalleridentity"){
          service = "iam"
        } else if (actions._2.head.toLowerCase.startsWith("acm:")){
          service = "certificatemanager"
          resType = "certificate"
        }

        (for { ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
              if tE.parameters("aws::accountid") == this.tE.parameters("aws::accountid") &&
                r._2.serviceType.toLowerCase == service && ((resType!=null && resType==r._2.resourceType.toLowerCase) || resType == null) }
          yield r._2
        ).toSet

      } else Set()

    }


      isResourceBlock(json) || isNotResourceBlock(json)  match {
        case false => (true,resourcesIsAttachedTo.toVector.asInstanceOf[Vector[Resource]])
        case true  => (isResourceBlock(json), computeResourceSet.toVector)
        }

  }




  private def isServicePrincipal(j:Json) : Boolean = {
    j.hasField("Service")
  }




  private def isAwsPrincipal(j:Json) : Boolean = {
    j.hasField("AWS")
  }




  private def isFederatedPrincipal(j:Json): Boolean = {
    j.hasField("Federated")
  }






  private def isCanonicalUserPrincipal(j:Json) : Boolean = {
    j.hasField("CanonicalUser")
  }




  private def isNormalPrincipal (j:Json) : Boolean =
    !isServicePrincipal(j) && !isAwsPrincipal(j) && !isFederatedPrincipal(j) && !isCanonicalUserPrincipal(j)




  private def isStarActionBlock(j: Json) : Boolean =
    ( j.hasField("Action") && j.field("Action").get.isString && j.field("Action").get.string.get == "*" ) ||
    ( j.hasField("NotAction") && j.field("NotAction").get.isString && j.field("NotAction").get.string.get == "*" )





  private def isStarResourceBlock(j: Json) : Boolean =
    ( j.hasField("Resource") && j.field("Resource").get.isString && j.field("Resource").get.string.get == "*" ) ||
      ( j.hasField("NotResource") && j.field("NotResourcen").get.isString && j.field("NotResource").get.string.get == "*" )


  private def isResourceBlock(j:Json)    = j.hasField("Resource")
  private def isNotResourceBlock(j:Json)  = j.hasField("NotResource")
  private def isResourceOmitted(j:Json) : Boolean = !(isResourceBlock(j)||isNotResourceBlock(j))

  def isAttachedToRole(j:Json):Boolean =
    resourcesIsAttachedTo.size==1 && resourcesIsAttachedTo.head.isInstanceOf[StackSetResource] &&
      resourcesIsAttachedTo.head.asInstanceOf[StackSetResource].serviceType.toLowerCase =="iam" &&
      resourcesIsAttachedTo.head.asInstanceOf[StackSetResource].resourceType.toLowerCase == "role"


}