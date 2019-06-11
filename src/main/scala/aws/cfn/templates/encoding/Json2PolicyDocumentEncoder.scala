package aws.cfn.templates.encoding

import java.util.regex.Pattern

import argonaut.Json
import aws.cfn.maps.ActionsMap
import aws.cfn.templates.formalization._



class Json2PolicyDocumentEncoder( iE:Json2InfrastructureEncoder, ssE:Json2StackSetEncoder,
                                  tE:Json2TemplateEncoder, rE:Json2ResourceEncoder, nE:Json2NodeEncoder,
                                  jsonPolicy:Json, parentResource: Resource ) {

  var principals    : (Boolean,Vector[Node]) = _
  var actions       : (Boolean,Vector[String]) = _
  var resources     : (Boolean,Vector[Node]) = _
  var hasCondition  : Boolean = _
  var resourcesIsAttachedTo : Set[Resource] = _
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
    val statements : Vector[Json] =
      if (hasArrayOfStatements)
        jsonPolicy.field("Statement").get.array.get.toVector
      else
        Vector(jsonPolicy.field("Statement").get)

    val stmtsVector = statements map (j => encodeStatement(j,resourcesIsAttachedTo))
    println("\n POLICY (" + ssE.stackSet.name + " - " + tE.template.name + ")" + "\n "+jsonPolicy)
    stmtsVector foreach println

    iE.allPoliciesStatement = iE.allPoliciesStatement ++ stmtsVector

    policyDocument = PolicyDocument(stmtsVector)
  }


  private def encodeStatement(statementJsonNode : Json,
                              resourcesIsAttachedTo: Set[Resource]) : Statement = {



    hasCondition  = statementJsonNode.hasField("Condition")

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
      AllowStatement(principals,actions,resources,hasCondition)
    else
      DenyStatement(principals,actions,resources,hasCondition)

  }



  private def computePrincipals(json: Json) : (Boolean,Vector[Node]) = {

    val isPrincipalBlock    = json.hasField("Principal")
    val isNotPrincipalBlock = json.hasField("NotPrincipal")

    def isStarPrincipalBlock(j:Json) : Boolean = {
      ( j.hasField("Principal") && j.field("Principal").get.isString && j.field("Principal").get.string.get == "*" ) ||
        ( j.hasField("NotPrincipal") && j.field("NotPrincipal").get.isString && j.field("NotPrincipal").get.string.get == "*" )
    }

    def computePrincipalSet :Vector[Node] = {
      isStarPrincipalBlock(json) match {
        case false  => computeNormalSet
        case true   => computeStarSet
      }
    }


    def computeStarSet : Vector[Node] = {
      Vector(Public)
    }


    def computeNormalSet : Vector[Node] = {

      def nodesFromCanonicalPrincipal(j:Json) = Vector()  // TODO
      def nodesFromFederatedPrincipal(j: Json) = Vector() // TODO
      def nodesFromServicePrincipal(j: Json) : Vector[Node]  = {

        def singleServicePrincipal(j:Json) : Vector[Node] = {
          Vector(nE.encode(j))
        }
        if (j.isArray)
          j.array.get.toVector flatMap singleServicePrincipal
        else
          singleServicePrincipal(j)
      }

      def nodesFromAwsPrincipal(j : Json): Vector[Node] = {

        def singleAwsPrincipal(j:Json): Vector[Node] = {
          val encodedJson = nE.encode(j)
          encodedJson match {
            case r:Resource       => Vector(r)
            case f:ExternalEntity        => Vector(f)
            case l:ListNode[Node]         => l.value
            case l:ListOfObjectNodes      => l.nodes
            case StringNode(account)      =>
              for {ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
                   if tE.parameters("aws::accountid").asInstanceOf[StringNode].value == account} yield r._2
          }
        }

        if (j.isArray)
          j.array.get.toVector flatMap singleAwsPrincipal
        else
          singleAwsPrincipal(j)

      }

      def principalsFromSingleNode(j:Json) : Vector[Node] = {
        if ( isServicePrincipal(j) )            nodesFromServicePrincipal(j.field("Service").get)
        else if (isAwsPrincipal(j))             nodesFromAwsPrincipal (j.field("AWS").get)
        else if (isFederatedPrincipal(j))       nodesFromFederatedPrincipal (j.field("Federated").get)
        else if (isCanonicalUserPrincipal(j))   nodesFromCanonicalPrincipal (j.field("CanonicalUser").get)
        else                                    Vector(nE.encode(j))
      }


      if (isPrincipalBlock && !json.field("Principal").get.isArray){
        principalsFromSingleNode(json.field("Principal").get)
      }
      else if (isPrincipalBlock)
        json.field("Principal").get.array.get.toVector flatMap principalsFromSingleNode
      else if (isNotPrincipalBlock && !json.field("NotPrincipal").get.isArray)
        principalsFromSingleNode(json.field("NotPrincipal").get)
      else
        json.field("NotPrincipal").get.array.get.toVector flatMap principalsFromSingleNode

    }

    isPrincipalBlock || isNotPrincipalBlock match {
      case false  => (true,resourcesIsAttachedTo.toVector)
      case true   => (isPrincipalBlock,computePrincipalSet)
    }
  }



  private def computeActions(json: Json) : (Boolean,Vector[String]) = {

    val isActionBlock = json.hasField("Action")
    val isNotActionBlock = json.hasField("NotAction")

    def computeActionSet : Vector[String] = {
      isStarActionBlock(json) match {
        case false  => computeNormalSet
        case true   => computeStarSet
      }
    }

    def computeNormalSet : Vector[String] = {

      def actionsFromJsonString ( s : String ) : Vector[String] = {
        if (s.toLowerCase == "sts:assumerole")
          Vector("sts:AssumeRole")
        else {
          var service = s.split(":").head
          var pt = s.split(":",-1).last
          //if (pt=="*") pt = "[a-zA-Z]*"
          pt = pt.replaceAll("\\*",  "[a-zA-Z]*")
          val actPattern = Pattern.compile( pt ,
            Pattern.CASE_INSENSITIVE)

            ActionsMap.lookUp(service) flatMap
              ( p => p._2.values flatMap
                ( actVector => actVector filter ( actStr => actPattern.matcher(actStr).matches()) map (a => p._1+":"+a)))

        }
      }

      if (isActionBlock && !json.field("Action").get.isArray){
        actionsFromJsonString( json.field("Action").get.string.get )
      }
      else if (isActionBlock)
        json.field("Action").get.array.get.toVector flatMap ( j => actionsFromJsonString(j.string.get) )
      else if (isNotActionBlock && !json.field("NotAction").get.isArray)
        actionsFromJsonString( json.field("NotAction").get.string.get )
      else
        json.field("NotAction").get.array.get.toVector flatMap ( j => actionsFromJsonString(j.string.get) )

    }

    def computeStarSet : Vector[String] = {
      val servType  = resources._2.head.asInstanceOf[Resource].serviceType
      val resType   = resources._2.head.asInstanceOf[Resource].resourceType
      ActionsMap.lookUp(servType,resType)
    }

    isActionBlock || isNotActionBlock match {
      case false  => (true,Vector())
      case true   => (isActionBlock,computeActionSet)
    }

  }



  private def computeResources(json: Json) : (Boolean,Vector[Node]) = {

    val isResourceBlock     = json.hasField("Resource")
    val isNotResourceBlock  = json.hasField("NotResource")


    def computeResourceSet: Vector[Node] = {
      isStarResourceBlock(json) match {
        case false => computeNormalSet
        case true  => computeStarSet
      }
    }


    def computeNormalSet : Vector[Node] = {
        if (isResourceBlock && !json.field("Resource").get.isArray)
          Vector(nE.encode(json.field("Resource").get))
        else if (isResourceBlock )
          json.field("Resource").get.array.get.toVector map (j=>nE.encode(j))
        else if (isNotResourceBlock && !json.field("NotResource").get.isArray)
          Vector(nE.encode(json.field("NotResource").get))
        else
          json.field("NotResource").get.array.get.toVector map (j=>nE.encode(j))
    }


    def computeStarSet : Vector[Node] = {

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

        for { ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
              if tE.parameters("aws::accountid") == this.tE.parameters("aws::accountid") &&
                r._2.serviceType.toLowerCase == service && ((resType!=null && resType==r._2.resourceType.toLowerCase) || resType == null) }
          yield r._2

      } else Vector()
    }


      isResourceBlock || isNotResourceBlock  match {
        case false => (true,resourcesIsAttachedTo.toVector)
        case true  => (isResourceBlock, computeResourceSet)
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





}