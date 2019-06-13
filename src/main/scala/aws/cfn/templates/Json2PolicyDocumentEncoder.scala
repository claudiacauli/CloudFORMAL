package aws.cfn.templates

import java.util.regex.Pattern

import argonaut.Json
import aws.cfn.actions.ActionsMap


protected class Json2PolicyDocumentEncoder( nE:Json2NodeEncoder,
                                  jsonPolicy:Json, parentResource: StackSetResource ) {

  require(nE.optRE.isDefined)

  val rE: Json2ResourceEncoder = nE.optRE.get
  val tE: Json2TemplateEncoder = rE.tE
  val ssE: Json2StackSetEncoder = tE.ssE
  val iE: Json2InfrastructureEncoder = ssE.iE
  

  val IAMService = "iam"
  val IAMRole = "role"
  val STSAssumeRole = "sts:assumerole"

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

    val hasArrayOfStatements = jsonPolicy.field(Policy.StatementTag).get.isArray
    val statements : Set[Json] =
      if (hasArrayOfStatements)
        jsonPolicy.field(Policy.StatementTag).get.array.get.toSet
      else
        Set(jsonPolicy.field(Policy.StatementTag).get)

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
      statementJsonNode.hasField(Policy.ConditionTag)

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

    if (allFieldsDefined && statementJsonNode.field(Policy.EffectTag).get.string.get.toLowerCase == Policy.AllowValue)
      AllowStatement(principals,actions,resources,hasCondition,isAssumeRoleStatement)
    else
      DenyStatement(principals,actions,resources,hasCondition,isAssumeRoleStatement)

  }



  private def computePrincipals(json: Json) : (Boolean,Set[Principal]) = {

    val isPrincipalBlock    = json.hasField(Policy.PrincipalTag)
    val isNotPrincipalBlock = json.hasField(Policy.NotPrincipalTag)

    def isStarPrincipalBlock(j:Json) : Boolean = {
      ( j.hasField(Policy.PrincipalTag) && j.field(Policy.PrincipalTag).get.isString && j.field(Policy.PrincipalTag).get.string.get == Policy.StarValue ) ||
        ( j.hasField(Policy.NotPrincipalTag) && j.field(Policy.NotPrincipalTag).get.isString && j.field(Policy.NotPrincipalTag).get.string.get == Policy.StarValue )
    }

    def computePrincipalSet : Set[Principal] = {
      if (isStarPrincipalBlock(json))
        computeStarSet
      else computeNormalSet
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
          nE.encode(j.field(Policy.ServiceTag).get) match {
            case StringNode(v) => Set(ServicePrincipal(v))
            case ListNode(v) if v.head.isInstanceOf[StringNode]
              => v.toSet.asInstanceOf[Set[StringNode]] flatMap (sn => Set(ServicePrincipal(sn.value)))
            case r:Resource => Set(r)
            case _ => println("Weird. The evaluation of a node that should resolve to a service principal id wasn't a string node or list thereof.")
              Set()
          }
        }

        else if (isAwsPrincipal(j)){
          nE.encode(j.field(Policy.AWSTag).get) match {
            case StringNode(v) => Set(AccountPrincipal(v))
            case ListNode(v) if v.head.isInstanceOf[StringNode]
              => v.toSet.asInstanceOf[Set[StringNode]] flatMap (sn => Set(AccountPrincipal(sn.value)))
            case r:Resource => Set(r)
            case _ =>
              println("Weird. The evaluation of a node that should resolve to an account id wasn't a string node or list thereof.")
              Set()
          }
        }

        else if (isFederatedPrincipal(j)) {
          nE.encode(j.field(Policy.FederatedTag).get) match {
            case StringNode(v) => Set(FederatedAccountPrincipal(v))
            case ListNode(v) if v.head.isInstanceOf[StringNode]
            => v.toSet.asInstanceOf[Set[StringNode]] flatMap (sn => Set(FederatedAccountPrincipal(sn.value)))
            case r:Resource => Set(r)
            case _ => println("Weird. The evaluation of a node that should resolve to a federation string id wasn't a string node or list thereof.")
              Set()
          }
        }

        else if (isCanonicalUserPrincipal(j)) {
          nE.encode(j.field(Policy.CanonicalUserTag).get) match {
            case StringNode(v) => Set(CanonicalUserPrincipal(v))
            case ListNode(v) if v.head.isInstanceOf[StringNode]
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


      if (isPrincipalBlock && !json.field(Policy.PrincipalTag).get.isArray){
        principalsFromSingleNode(json.field(Policy.PrincipalTag).get)
      }
      else if (isPrincipalBlock)
        json.field(Policy.PrincipalTag).get.array.get.toSet flatMap principalsFromSingleNode
      else if (isNotPrincipalBlock && !json.field(Policy.NotPrincipalTag).get.isArray)
        principalsFromSingleNode(json.field(Policy.NotPrincipalTag).get)
      else
        json.field(Policy.NotPrincipalTag).get.array.get.toSet flatMap principalsFromSingleNode

    }

    if (isPrincipalBlock || isNotPrincipalBlock)
      (isPrincipalBlock, computePrincipalSet)
    else (true, resourcesIsAttachedTo)

  }



  private def computeActions(json: Json) : (Boolean,Vector[String]) = {

    val isActionBlock = json.hasField(Policy.ActionTag)
    val isNotActionBlock = json.hasField(Policy.NotActionTag)

    def computeActionSet : Set[String] =
      if (isStarActionBlock(json))
        computeStarSet
      else computeNormalSet


    def computeNormalSet : Set[String] = {

      def actionsFromJsonString ( s : String ) : Set[String] = {
        if (s.toLowerCase == STSAssumeRole)
          Set(STSAssumeRole)
        else {
          val service = s.split(":").head
          var actionNamePattern = s.split(":",-1).last
          actionNamePattern = actionNamePattern.replaceAll("\\*",  ".*")
          val actPattern = Pattern.compile( actionNamePattern ,
            Pattern.CASE_INSENSITIVE)
          ActionsMap.lookupServiceName(service) filter
            ( aName => actPattern.matcher(aName).matches() )
        }
      }

      if (isActionBlock && !json.field(Policy.ActionTag).get.isArray){
        actionsFromJsonString( json.field(Policy.ActionTag).get.string.get )
      }
      else if (isActionBlock)
        (json.field(Policy.ActionTag).get.array.get.toVector flatMap (j => actionsFromJsonString(j.string.get) )).toSet
      else if (isNotActionBlock && !json.field(Policy.NotActionTag).get.isArray)
        actionsFromJsonString( json.field(Policy.NotActionTag).get.string.get )
      else
        (json.field(Policy.NotActionTag).get.array.get.toVector flatMap (j => actionsFromJsonString(j.string.get) )).toSet

    }

    def computeStarSet : Set[String] = {
      val servType  = resources._2.head.asInstanceOf[StackSetResource].serviceType
      ActionsMap.lookupServiceName(servType)
    }

    if (isActionBlock || isNotActionBlock)
      (isActionBlock, computeActionSet.toVector)
    else (true, Vector())

  }



  private def computeResources(json: Json) : (Boolean,Vector[Resource]) = {


    def computeResourceSet: Set[Resource] =
      if (isStarResourceBlock(json))
        computeStarSet
      else computeNormalSet


    def ifNotEntityEmptySet(encodedNone : Node) : Set[Resource] = {
      encodedNone match {
        case ListOfResources(v) => v.toSet
        case r:Resource => Set(r)
        case StringNode(s) => println("The function returned a stringnode! with value " + s); Set()
        case x => println("The function to encode the node did not return entity but " + x) ; Set()
      }
    }

    def computeNormalSet : Set[Resource] = {

        if (isResourceBlock(json) && !json.field(Policy.ResourceTag).get.isArray)
          ifNotEntityEmptySet(nE.encode(json.field(Policy.ResourceTag).get))
        else if (isResourceBlock(json) )
          (json.field(Policy.ResourceTag).get.array.get.toVector flatMap (j => ifNotEntityEmptySet(nE.encode(j)) )).toSet
        else if (isNotResourceBlock(json) && !json.field(Policy.NotResourceTag).get.isArray)
          ifNotEntityEmptySet(nE.encode(json.field(Policy.NotResourceTag).get))
        else
          (json.field(Policy.NotResourceTag).get.array.get.toVector flatMap (j=> ifNotEntityEmptySet(nE.encode(j)) )).toSet

    }


    def computeStarSet : Set[Resource] = {

      if (actions._2.nonEmpty) {

        val service = ActionsMap.getServiceFromActionPrefix(actions._2.head.split(":").head)
        var resType :String = null
        if (actions._2.head.toLowerCase == STSAssumeRole)
          resType = IAMRole

        (for { ssE <- iE.stackSetEncoders; tE <- ssE.templatesEncoders; r <- tE.resources.toVector
              if tE.parameters(PseudoParameter.AccountId) == this.tE.parameters(PseudoParameter.AccountId) &&
                r._2.serviceType.toLowerCase == service && ((resType!=null && resType==r._2.resourceType.toLowerCase) || resType == null) }
          yield r._2
        ).toSet

      } else Set()

    }


      if (isResourceBlock(json) || isNotResourceBlock(json))
        (isResourceBlock(json), computeResourceSet.toVector)
      else (true, resourcesIsAttachedTo.toVector.asInstanceOf[Vector[Resource]])

  }




  private def isServicePrincipal(j:Json) : Boolean = {
    j.hasField(Policy.ServiceTag)
  }




  private def isAwsPrincipal(j:Json) : Boolean = {
    j.hasField(Policy.AWSTag)
  }




  private def isFederatedPrincipal(j:Json): Boolean = {
    j.hasField(Policy.FederatedTag)
  }






  private def isCanonicalUserPrincipal(j:Json) : Boolean = {
    j.hasField(Policy.CanonicalUserTag)
  }




//  private def isNormalPrincipal (j:Json) : Boolean =
//    !isServicePrincipal(j) && !isAwsPrincipal(j) && !isFederatedPrincipal(j) && !isCanonicalUserPrincipal(j)




  private def isStarActionBlock(j: Json) : Boolean =
    ( j.hasField(Policy.ActionTag) && j.field(Policy.ActionTag).get.isString && j.field(Policy.ActionTag).get.string.get == Policy.StarValue ) ||
    ( j.hasField(Policy.NotActionTag) && j.field(Policy.NotActionTag).get.isString && j.field(Policy.NotActionTag).get.string.get == Policy.StarValue )





  private def isStarResourceBlock(j: Json) : Boolean =
    ( j.hasField(Policy.ResourceTag) && j.field(Policy.ResourceTag).get.isString && j.field(Policy.ResourceTag).get.string.get == Policy.StarValue ) ||
      ( j.hasField(Policy.NotResourceTag) && j.field(Policy.NotResourceTag).get.isString && j.field(Policy.NotResourceTag).get.string.get == Policy.StarValue )


  private def isResourceBlock(j:Json)    = j.hasField(Policy.ResourceTag)
  private def isNotResourceBlock(j:Json)  = j.hasField(Policy.NotResourceTag)
  private def isResourceOmitted(j:Json) : Boolean = !(isResourceBlock(j)||isNotResourceBlock(j))

  def isAttachedToRole(j:Json):Boolean =
    resourcesIsAttachedTo.size==1 && resourcesIsAttachedTo.head.isInstanceOf[StackSetResource] &&
      resourcesIsAttachedTo.head.asInstanceOf[StackSetResource].serviceType.toLowerCase == IAMService &&
      resourcesIsAttachedTo.head.asInstanceOf[StackSetResource].resourceType.toLowerCase == IAMRole


}