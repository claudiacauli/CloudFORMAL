package com.cloud.formal.mapping.templates

import java.util.regex.Pattern

import argonaut.Json
import com.cloud.formal.mapping.actions.ActionsMap
import com.typesafe.scalalogging.LazyLogging


private class Json2PolicyDocumentEncoder( nE:Json2NodeEncoder,
                                  jsonPolicy:Json, parentResource: StackSetResource )
extends LazyLogging {

  require(nE.optRE.isDefined)

  val rE: Json2ResourceEncoder = nE.optRE.get
  val tE: Json2TemplateEncoder = rE.tE
  val ssE: Json2StackSetEncoder = tE.ssE
  val iE: Json2InfrastructureEncoder = ssE.iE


  val IAMService = "iam"
  val IAMRole = "role"
  val STSAssumeRole = "sts:assumerole"

  var principals: (Boolean, Set[Principal]) = _
  var actions: (Boolean, Vector[String]) = _
  var resources: (Boolean, Vector[Resource]) = _
  var hasCondition: Boolean = _
  var isAssumeRoleStatement: Boolean = _
  var resourcesIsAttachedTo: Set[Principal] = _
  var policyDocument: PolicyDocument = _

  //def createNode(): PolicyDocument = policyDocument

  def encode(): Unit = {

    resourcesIsAttachedTo =
      if (iE.resourcesPointingToPolicy.get(parentResource).isDefined)
        iE.resourcesPointingToPolicy(parentResource).toSet
      else Set(parentResource)


    val statementsNodes =
      jsonPolicy.field(Policy.StatementTag).get match {
        case a if a.isArray => a.array.get.toSet
        case o => Set(o)
      }

    val statementsObjs : Set[Statement] =
      (statementsNodes map encodeStatement)
          .filter(_.isDefined)
          .map(_.get)

    iE.policyStatements ++= statementsObjs
    iE.infrastructure.policies ++=
      Set((ssE.stackSet.name, tE.template.name, jsonPolicy, statementsObjs))
    policyDocument = PolicyDocument(statementsObjs)

  }


  private def encodeStatement(statementNode: Json): Option[Statement] = {

    // check if it is a statement
    // if not, call the Node Encoder.

    val optStatementNode =
    if (!statementNode.hasField(Policy.EffectTag)) {
      val encodedNode = nE.encode(statementNode)
      encodedNode match {
        case NoValue => None
        case StatementJsonWrapper(j) => Some(j)
      }
    } else Some(statementNode)

    optStatementNode match {
      case None => None
      case Some(encodedStatementNode) => {

        isAssumeRoleStatement =
          resourceIsOmitted(encodedStatementNode) && isAttachedToRole(encodedStatementNode)
        hasCondition =
          encodedStatementNode.hasField(Policy.ConditionTag)

        if (isStarActionBlock(encodedStatementNode)) {
          resources = computeResources(encodedStatementNode)
          actions = computeActions(encodedStatementNode)
        } else {
          actions = computeActions(encodedStatementNode)
          resources = computeResources(encodedStatementNode)
        }
        principals = computePrincipals(encodedStatementNode)


        if (isAllowStatement(encodedStatementNode))
          Some(AllowStatement(principals, actions,
            resources, hasCondition, isAssumeRoleStatement,
            tE.parameters(PseudoParameter.AccountId).asInstanceOf[StringNode].value))
        else
          Some(DenyStatement(principals, actions,
            resources, hasCondition, isAssumeRoleStatement,
            tE.parameters(PseudoParameter.AccountId).asInstanceOf[StringNode].value))

      }
    }

  }


  private def computePrincipals(j: Json): (Boolean, Set[Principal]) = {

    def computePrincipalSet: Set[Principal] =
      if (isStarPrincipalBlock(j))
        getStarSet
      else computeNormalSet

    def getStarSet: Set[Principal] =
      Set(Public)

    def computeNormalSet: Set[Principal] = {

      if (hasPrincipalBlock(j) &&
        !j.field(Policy.PrincipalTag).get.isArray) {
        principalsFromSinglePrincipalNode(
          j.field(Policy.PrincipalTag).get)
      }
      else if (hasPrincipalBlock(j)) {
        j.field(Policy.PrincipalTag)
          .get.array.get.toSet
          .flatMap(principalsFromSinglePrincipalNode)
      }
      else if (hasNotPrincipalBlock(j) &&
        !j.field(Policy.NotPrincipalTag).get.isArray) {
        principalsFromSinglePrincipalNode(
          j.field(Policy.NotPrincipalTag).get)
      }
      else {
        j.field(Policy.NotPrincipalTag)
          .get.array.get.toSet
          .flatMap(principalsFromSinglePrincipalNode)
      }
    }


    if (!principalIsOmitted(j))
      (hasPrincipalBlock(j), computePrincipalSet)
    else
      (true, resourcesIsAttachedTo)
  }


  private def computeActions(json: Json): (Boolean, Vector[String]) = {

    def computeActionSet: Set[String] =
      if (isStarActionBlock(json))
        getStarSet
      else computeNormalSet

    def getStarSet: Set[String] = {
      val servType = resources._2
        .head.asInstanceOf[StackSetResource]
        .serviceType
      ActionsMap.lookupServiceName(servType)
    }

    def computeNormalSet: Set[String] = {
      if (hasActionBlock(json) &&
        !json.field(Policy.ActionTag).get.isArray) {
        actionsFromJsonString(
          json.field(Policy.ActionTag).get.string.get)
      }
      else if (hasActionBlock(json)) {
        json.field(Policy.ActionTag).get.array.get.toVector
          .flatMap(j =>
            actionsFromJsonString(j.string.get)).toSet
      }
      else if (hasNotActionBlock(json) &&
        !json.field(Policy.NotActionTag).get.isArray) {
        actionsFromJsonString(
          json.field(Policy.NotActionTag).get.string.get)
      }
      else
        json.field(Policy.NotActionTag).get.array.get.toVector
          .flatMap(j =>
            actionsFromJsonString(j.string.get)).toSet
    }


    if (!actionIsOmitted(json))
      (hasActionBlock(json), computeActionSet.toVector)
    else (true, Vector())
  }


  private def computeResources(json: Json): (Boolean, Vector[Resource]) = {

    def computeResourceSet: Set[Resource] =
      if (isStarResourceBlock(json))
        getStarSet
      else computeNormalSet

    def getStarSet: Set[Resource] = {
      if (actions._2.nonEmpty) {
        (
          for {ssE <- iE.stackSetEncoders
               tE <- ssE.templatesEncoders
               r <- tE.resources.toVector
               if sameAccountAsThis(tE) && sameService(r._2) &&
                 sameResourceType(r._2)}
            yield r._2
          ).toSet
      } else
        Set()
    }

    def computeNormalSet =
      if (hasResourceBlock(json) &&
        !json.field(Policy.ResourceTag).get.isArray) {
        getResourcesFromSingleNode(
          nE.encode(json.field(Policy.ResourceTag).get))
      }
      else if (hasResourceBlock(json)) {
        json.field(Policy.ResourceTag).get.array.get.toVector
          .flatMap(j =>
            getResourcesFromSingleNode(nE.encode(j))).toSet
      }
      else if (hasNotResourceBlock(json) &&
        !json.field(Policy.NotResourceTag).get.isArray) {
        getResourcesFromSingleNode(
          nE.encode(json.field(Policy.NotResourceTag).get))
      }
      else {
        json.field(Policy.NotResourceTag).get.array.get.toVector
          .flatMap(j =>
            getResourcesFromSingleNode(nE.encode(j))).toSet
      }


    if (!resourceIsOmitted(json))
      (hasResourceBlock(json), computeResourceSet.toVector)
    else (true, resourcesIsAttachedTo.toVector.asInstanceOf[Vector[Resource]])
  }


  private def isServicePrincipal(j: Json): Boolean = {
    j.hasField(Policy.ServiceTag)
  }


  private def isAwsPrincipal(j: Json): Boolean = {
    j.hasField(Policy.AWSTag)
  }


  private def isFederatedPrincipal(j: Json): Boolean = {
    j.hasField(Policy.FederatedTag)
  }


  private def isCanonicalUserPrincipal(j: Json): Boolean = {
    j.hasField(Policy.CanonicalUserTag)
  }


  private def fieldIsStar(j: Json, fieldTag: String) =
    j.field(fieldTag).get match {
      case s if s.isString
      => s.string.get == Policy.StarValue
      case a if a.isArray
      => a.array.get contains Policy.StarValue
      case _ => false
    }


  private def isStarPrincipalBlock(j: Json): Boolean =
    if (hasPrincipalBlock(j))
      fieldIsStar(j, Policy.PrincipalTag)
    else if (hasNotPrincipalBlock(j))
      fieldIsStar(j, Policy.NotPrincipalTag)
    else false


  private def isStarActionBlock(j: Json): Boolean =
    if (hasActionBlock(j))
      fieldIsStar(j, Policy.ActionTag)
    else if (hasNotActionBlock(j))
      fieldIsStar(j, Policy.NotActionTag)
    else false


  private def isStarResourceBlock(j: Json) =
    if (hasResourceBlock(j))
      fieldIsStar(j, Policy.ResourceTag)
    else if (hasNotResourceBlock(j))
      fieldIsStar(j, Policy.NotResourceTag)
    else false


  private def hasPrincipalBlock(j: Json) =
    j.hasField(Policy.PrincipalTag)


  private def hasNotPrincipalBlock(j: Json) =
    j.hasField(Policy.NotPrincipalTag)


  private def hasResourceBlock(j: Json) =
    j.hasField(Policy.ResourceTag)


  private def hasNotResourceBlock(j: Json) =
    j.hasField(Policy.NotResourceTag)


  private def hasActionBlock(j: Json) =
    j.hasField(Policy.ActionTag)


  private def hasNotActionBlock(j: Json) =
    j.hasField(Policy.NotActionTag)


  def isAttachedToRole(j: Json): Boolean =
    if (resourcesIsAttachedTo.size == 1)
      resourcesIsAttachedTo.head match {
        case r: StackSetResource =>
          r.serviceType.toLowerCase == IAMService &&
            r.resourceType.toLowerCase == IAMRole
        case _ => false
      }
    else false


  private def isAllowStatement(j: Json) = {
    j.field(Policy.EffectTag)
      .get.string.get
      .toLowerCase == Policy.AllowValue
  }


  private def resourceIsOmitted(j: Json) =
    !hasResourceBlock(j) && !hasNotResourceBlock(j)


  private def principalIsOmitted(j: Json) =
    !hasPrincipalBlock(j) && !hasNotPrincipalBlock(j)


  private def actionIsOmitted(j: Json) =
    !hasActionBlock(j) && !hasNotActionBlock(j)


  private def getServicePrincipals(j: Json): Set[Principal] = {

    def getSinglePrincipal(j: Json): Set[Principal] =
      nE.encode(j) match {
        case StringNode(v)
        => Set(ServicePrincipal(v))
        case ListNode(v) if v.head.isInstanceOf[StringNode]
        => v.toSet.asInstanceOf[Set[StringNode]]
          .flatMap(sn =>
            Set(ServicePrincipal(sn.value)))
        case _ =>
          logger.error(s"Policy Service Principal field " +
            s"does not evaluate to a StringNode. " +
            s"Returning Dummy Principal")
          Set(ServicePrincipal("DummyServicePrincipal"))
      }

    if (j.isArray)
      j.array.get.flatMap(getSinglePrincipal).toSet
    else
      getSinglePrincipal(j)

  }







  private def getAwsPrincipals(j: Json): Set[Principal] = {

    def getSinglePrincipal(j: Json): Set[Principal] =
      nE.encode(j) match {
        case StringNode(v)
        => Set(AccountPrincipal(v))
        case ListNode(v) if v.head.isInstanceOf[StringNode]
        => v.toSet.asInstanceOf[Set[StringNode]]
          .flatMap (sn =>
            Set(AccountPrincipal(sn.value)))
        case r: Resource => Set(r)
        //case v: Vector[Node] =>
        //case v: => v.toSet.asInstanceOf[Set[Principal]]
        case x =>
          logger.error(s"Policy Aws Principal field " +
            s"evaluates to unexpected type $x for node $j" +
            s"Returning Dummy Account")
          Set(AccountPrincipal("DummyAccount"))
      }


    if (j.isArray)
      j.array.get.flatMap(getSinglePrincipal).toSet
    else
      getSinglePrincipal(j)

    }





  private def getFederatedPrincipals(j: Json): Set[Principal] = {

    def getSinglePrincipal(j: Json): Set[Principal] =
      nE.encode(j) match {
        case StringNode (v)
        => Set (FederatedAccountPrincipal (v) )
        case ListNode (v) if v.head.isInstanceOf[StringNode]
        => v.toSet.asInstanceOf[Set[StringNode]]
          .flatMap (sn =>
            Set(FederatedAccountPrincipal(sn.value)))
        case _ =>
          logger.error(s"Policy Federated Principal field " +
            s"does not evaluate to a StringNode. " +
            s"Returning Dummy Federation")
          Set(FederatedAccountPrincipal("DummyFederation"))
      }

    if (j.isArray)
      j.array.get.flatMap(getSinglePrincipal).toSet
    else
      getSinglePrincipal(j)

  }




  private def getCanonicalUserPrincipals(j: Json): Set[Principal] = {

    def getSinglePrincipal(j: Json): Set[Principal] =
      nE.encode(j) match {
        case StringNode(v)
        => Set(CanonicalUserPrincipal(v))
        case ListNode(v) if v.head.isInstanceOf[StringNode]
        => v.toSet.asInstanceOf[Set[StringNode]]
          .flatMap (sn =>
            Set(CanonicalUserPrincipal(sn.value)))
        case _ =>
          logger.error(s"Policy Canonical User Principal field " +
            s"does not evaluate to a StringNode. " +
            s"Returning Dummy CanonicalUser")
          Set(CanonicalUserPrincipal("DummyCanonicalUser"))
      }

    if (j.isArray)
      j.array.get
        .flatMap(getSinglePrincipal).toSet
    else
      getSinglePrincipal(j)

  }





  private def principalsFromSinglePrincipalNode(j:Json): Set[Principal] =
    if (isServicePrincipal(j))
      getServicePrincipals(j.field(Policy.ServiceTag).get)
    else if (isAwsPrincipal(j))
      getAwsPrincipals(j.field(Policy.AWSTag).get)
    else if (isFederatedPrincipal(j))
      getFederatedPrincipals(j.field(Policy.FederatedTag).get)
    else if (isCanonicalUserPrincipal(j))
      getCanonicalUserPrincipals(j.field(Policy.CanonicalUserTag).get)
    else
      nE.encode(j) match {
        case r: Resource => Set(r)
        case ListOfResources(v) => v.toSet
        case x
        => logger.warn(s"Policy Principal evaluated to " +
          s"unexpected node $x. Returning Empty Set.")
          Set()
      }



  private def actionsFromJsonString(s: String): Set[String] = {
    val service = s.split(":").head
    val actioRegex = s.replaceAll("\\*", ".*")
    val actPattern =
      Pattern.compile(actioRegex,
      Pattern.CASE_INSENSITIVE)

    ActionsMap
      .lookupServiceName(service)
      .filter(aName =>
        actPattern.matcher(aName).matches())
  }



  private def sameAccountAsThis(tE: Json2TemplateEncoder) =
    tE.parameters(PseudoParameter.AccountId) ==
      this.tE.parameters(PseudoParameter.AccountId)



  private def sameService(r: StackSetResource) = {
    val service = ActionsMap
      .getServiceFromActionPrefix(
        actions._2.head.split(":").head)
    r.serviceType.toLowerCase == service
  }



  private def sameResourceType(r: StackSetResource) = {
    var resType: String = null
    if (actions._2.head.toLowerCase == STSAssumeRole)
      resType = IAMRole
    (resType!=null && resType == r.resourceType.toLowerCase) ||
      resType == null
  }



  private def getResourcesFromSingleNode(encodedNode: Node, isReattempt: Boolean=false): Set[Resource] =
    encodedNode match {
      case ListOfResources(v)
      => v.toSet
      case r:Resource
      => Set(r)
      case StringNode(s)
      => if (!isReattempt)
        getResourcesFromSingleNode(ArnFunction(Some(rE),tE)(s), isReattempt = true)
      else Set()
      case _ =>
        logger.error(s"Resource Node evaluates to " +
          s"$encodedNode which is not a Resource. Returning" +
          s" empty resources.")
        Set()
    }



}

