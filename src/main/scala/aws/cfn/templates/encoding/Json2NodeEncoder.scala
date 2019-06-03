package aws.cfn.templates.encoding

import java.io.File

import argonaut._
import aws.cfn.dlmodel.DLModelIRI
import aws.cfn.shared.EncodeUtils
import aws.cfn.shared.EncodeUtils.subFieldNames
import aws.cfn.templates.encoding.Json2ResourceEncoder.rangeNameOf
import aws.cfn.templates.formalization._
import org.semanticweb.owlapi.model.OWLOntology

import scala.language.postfixOps

protected class Json2NodeEncoder(ssE: Json2StackSetEncoder, tE: Json2TemplateEncoder, rE: Json2ResourceEncoder) {


  def encode(json: Json, subpropType: Option[(String,String)] = None): Node = {
    json match {
      case j if isArn(j)            => ArnFunction(tE.resources, tE.parameters, j.string.get, ssE.resourceByArn) ()
      case j if j.isNull || isNoValue(j)                    => NoValue
      case j if j.isObject && isIntrinsicFunction(j)        => evalIntrinsicFunction(j)
      case j if j.isObject && isMapProperty(j,subpropType)  => encodeMapProperty(j,mapProperty(j,subpropType).get)
      case j if j.isObject && isPolicyDoc(j)                   => rE.PolicyEncoder.encode(j)
      case j if j.isObject && subpropType.isDefined && subpropType.get._2.equals("string") => StringNode(j.toString())
      case j if j.isObject && subpropType.isDefined && subpropType.get._2.equals("policy") => encodeEmbeddedPolicy(j,subpropType.get)
      case j if j.isObject        => encodeSubproperty(j,subpropType)
      case j if j.isArray         => encodeArrayNode(j,subpropType)
      case j                      => encodeValueNode(j,subpropType)
    }
  }





  private def encodeValueNode(j: Json, subpropType: Option[(String,String)])=
    subpropType match {
      case Some((_,spt))  => matchNodeBySubpropType(j,spt)
      case None           => matchNodeByJsonType(j)
    }




  private def encodeArrayNode(j: Json, subpropType:Option[(String,String)])
    = ListNode((j.array.get map (ji => encode(ji,subpropType))).toVector)






  private def encodeMapProperty(json: Json, subpropertyType: String) = NoValue // TODO






  private def encodeSubproperty(j: Json, subpropType:Option[(String,String)]): Node = {

    def givenProperties =
      (j.objectFieldsOrEmpty map (f => subpropType.get._2+"_"+f.toString)).toSet

    def nodeObjectForProperty(j: Json, propFullName: String)  = {
      val propTemplateName = propFullName.split(subpropType.get._2+ "_").last
      encode( j.field(propTemplateName).get, Json2ResourceEncoder.rangeNameOf(propFullName,rE.resourceOntology,rE.serviceType,rE.resourceType,ssE))
    }

    subpropType match {
      case None               => {
        val presentProperties = (givenProperties flatMap ( propName => Map(propName -> nodeObjectForProperty(j,propName)))).toMap
        Subproperty(presentProperties)
      }
      case Some((_,spt)) => {
        val absentProperties  = Json2ResourceEncoder.subPropertiesNamesOfClassName(spt, ssE,rE.serviceType,rE.resourceType, rE.resourceOntology) -- givenProperties.map(s => s.toLowerCase())
        val presentProperties = (givenProperties flatMap ( propName => Map(propName -> nodeObjectForProperty(j,propName)))).toMap
        Subproperty(presentProperties,absentProperties)
      }
    }
  }



  def encodeEmbeddedPolicy(json: Json, policyType: (String,String)) : StackSetResource = {

    val resourceType = policyType._2
    val serviceType = policyType._1.split(resourceType).head
    val embeddedPolicyIRI = DLModelIRI.embeddedPolicyIRI(ssE.stackSet.name)
    val embeddedPolicyName = embeddedPolicyIRI.toString.split("#").last
    val embeddedPolicyResource = StackSetResource(embeddedPolicyName, serviceType, resourceType, Map())

    val ontology : OWLOntology = ssE.manager.loadOntologyFromOntologyDocument(
      new File("src/main/resources/terminology/resourcespecificationsOwl/" + serviceType+resourceType + ".owl"))

    def givenProperties(json : Json, resourceType:String): Set[String] =
      (json.objectFieldsOrEmpty map (f => resourceType+"_"+f.toString)).toSet

    def nodeObjectForProperty(propFullName: String) : Node = {
      val propTemplateName = propFullName.split(resourceType+ "_").last
      val subPropertyType = Json2ResourceEncoder.rangeNameOf(propFullName.toLowerCase(), ontology, serviceType, resourceType, ssE)
      val returnNode = encode(json.field(propTemplateName).get ,subPropertyType)
      returnNode
    }

    embeddedPolicyResource.absentProperties = Json2ResourceEncoder.subPropertiesNamesOfClassName(resourceType.toLowerCase(),ssE,serviceType,resourceType,ontology) --
      givenProperties(json,resourceType).map(s=>s.toLowerCase())
    embeddedPolicyResource.givenProperties = (givenProperties(json,resourceType) flatMap (propName => Map(propName ->
      nodeObjectForProperty(propName)))).toMap

    tE.embeddedResources = tE.embeddedResources ++ Map(embeddedPolicyName -> embeddedPolicyResource)
    embeddedPolicyResource
  }


  /*
  TODO
  Clean up this function to validate inputs and evaluate functions!
   */


  private def evalIntrinsicFunction(j:Json) : Node = {

    EncodeUtils.subFieldNames(j)(0) match {
      case "fn::base64"     => validateParamsAndEvalBase64(j)
      case "fn::cidr"       => validateParamsAndEvalCidr(j)
      case "fn::if"         => validateParamsAndEvalIf(j)
      case "fn::not"        => validateParamsAndEvalNot(j)
      case "fn::and"        => validateParamsAndEvalAnd(j)
      case "fn::equals"     => validateParamsAndEvalEquals(j)
      case "fn::or"         => validateParamsAndEvalOr(j)
      case "fn::findinmap"  => validateParamsAndEvalFindInMap(j)
      case "fn::getatt"     => validateParamsAndEvalGetAtt(j)
      case "fn::getazs"       => validateParamsAndEvalGetAZs(j)
      case "fn::importvalue"  => validateParamsAndEvalImportValue(j)
      case "fn::join"         => validateParamsAndEvalJoin(j)
      case "fn::select"       => validateParamsAndEvalSelect(j)
      case "fn::split"        => validateParamsAndEvalSplit(j)
      case "fn::sub"          => validateParamsAndEvalSub(j)
      case "fn::transform"    => validateParamsAndEvalTransform(j)
      case "ref"              => validateParamsAndEvalRef(j)
      case unknownFun => {
        println("We should NOT get here. Is this a not implemented function? " + unknownFun )
        NoValue
      }
    }

  }


  private def arrayAt(j: Json, funName:String, index:Int) = {
    if (j.field(funName).get.isArray)
      j.field(funName).get.array.get(index)
    else
      j.field(funName).get
  }


  private def validateParamsAndEvalBase64(j: Json) =
    Base64Function (encode (j.field ("Fn::Base64").get).asInstanceOf[StringNode] ) ()



  private def validateParamsAndEvalCidr(j: Json) =
    CidrFunction (
      encode (arrayAt(j, "Fn::Cidr",0)).asInstanceOf[StringNode],
      encode (arrayAt(j, "Fn::Cidr",1) ).asInstanceOf[IntNode],
      encode (arrayAt(j, "Fn::Cidr",2)).asInstanceOf[IntNode] ) ()


  private def validateParamsAndEvalIf(j: Json) = {

    val encodedCondition  = encode(arrayAt(j, "Fn::If",0))
    val encodedTrueExp    = encode(arrayAt(j, "Fn::If", 1))
    val encodedFalseExp   = encode(arrayAt(j, "Fn::If", 2))

    encodedCondition match {
      case sn: StringNode   => tE.conditions.get(sn.value) match {
        case None     => NoValue
        case Some(b)  => IfFunction(BooleanNode(b), encodedTrueExp, encodedFalseExp)()
      }
      case bn : BooleanNode => IfFunction(bn, encodedTrueExp, encodedFalseExp)()
      case _                => {
        println("\nWe should NOT get here: the encoded condition inside an If function doesn't evaluate to a boolean node.")
        println("The original json node of the condition is: " + arrayAt(j, "Fn::If",0))
        println("The evaluated json node of the condition is: " + encodedCondition)
        NoValue
      }
    }

  }


  private def validateParamsAndEvalNot(j: Json) = {

    val encodedJson = encode(j.field("Fn::Not").get)

    encodedJson match {
      case n : BooleanNode            => NotFunction(n) ()
      case n : ListNode[BooleanNode]  => NotFunction(n.value.head) ()
      case _ => {
        println("\nWe should NOT get here. The result of the evaluation of the node contained in a Not function is not a boolean node.")
        println("The original json node of the FnNot is: " + j.field("Fn::Not").get )
        println("The evaluated json node of the condition is: " + encodedJson)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalAnd(j: Json) = {

    val encodedLeftExp = encode(arrayAt(j, "Fn::And",0))
    val encodedRightExp = encode(arrayAt(j,"Fn::And",1))

    (encodedLeftExp, encodedRightExp) match {
      case (l:BooleanNode,r:BooleanNode) => AndFunction(l,r) ()
      case _ => {
        println("\nWe should NOT get here. The result of the evaluation of one or both the And expressions is not a boolean node.")
        println("The original jsons are: " + arrayAt(j,"Fn::And",0) + " and " + arrayAt(j, "Fn::And",1) )
        println("The evaluated json nodes are: " + encodedLeftExp + " and " + encodedRightExp)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalOr(j: Json) = {

    val encodedLeftExp  = encode(arrayAt(j, "Fn::Or",0))
    val encodedRightExp = encode(arrayAt(j,"Fn::Or",1))

    (encodedLeftExp, encodedRightExp) match {
      case (l:BooleanNode, r:BooleanNode) => OrFunction(l,r) ()
      case _ => {
        println("\nWe should NOT get here. The result of the evaluation of one or both the Or expressions is not a boolean node.")
        println("The original jsons are: " + arrayAt(j,"Fn::Or",0) + " and " + arrayAt(j, "Fn::Or",1) )
        println("The evaluated json nodes are: " + encodedLeftExp + " and " + encodedRightExp)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalEquals(j: Json) = {

    val encodedLeftExp = encode (arrayAt(j,"Fn::Equals",0))
    val encodedRightExp = encode (arrayAt(j,"Fn::Equals",1))

    EqualsFunction(encodedLeftExp, encodedRightExp) ()
  }


  private def validateParamsAndEvalFindInMap(j: Json) = {

    val hasSecondLevelKey   = j.field("Fn::FindInMap").get.array.get.size == 3
    val encodedMapName      = encode(arrayAt(j,"Fn::FindInMap",0))
    val encodedTopLevelKey  = encode(arrayAt(j,"Fn::FindInMap",1))
    val encodedSecondLevelKey = if (hasSecondLevelKey) encode(arrayAt(j,"Fn::FindInMap",2)) else None

    (encodedMapName,encodedTopLevelKey,encodedSecondLevelKey) match {
      case (m:StringNode,k1:StringNode,None)                => FindInMapFunction(tE.mappings, m,k1) ()
      case (m:StringNode,k1:StringNode,k2:StringNode)       => FindInMapFunction(tE.mappings, m,k1,k2) ()
      case _ => {
        println("We should NOT get here. It was not possible to evaluate the parameters of a FindInMap function with the correct types or values missing in map!")
        println("Template: " + tE.template.name + " and Original node is : " + j)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalGetAtt(j: Json) = {

    val encodedResourceName   = encode(arrayAt(j,"Fn::GetAtt",0))
    val encodedAttributeName  = encode(arrayAt(j,"Fn::GetAtt",1))

    (encodedResourceName, encodedAttributeName) match {
      case (r:StringNode,a:StringNode)  => GetAttFunction(r,a,tE.resources) ()
      case _ => {
        println("\nWe should NOT get here. It was not possible to evaluate the parameters of a GetAtt function with correct types.")
        println("Original node is: " + j)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalGetAZs(j: Json) = {

    val encodedRegion = encode (j.field ("Fn::GetAZs").get)

    encodedRegion match {
      case r:StringNode => GetAZsFunction(r) ()
      case _ => {
        println("\nWe should NOT get here. It was not possible to valuate GetAZs param as StringNode. Node is " + j)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalImportValue(j: Json) = {

    val encodedImportName = encode (j.field ("Fn::ImportValue").get)

    encodedImportName match {
      case i:StringNode => ImportValueFunction(i, ssE.outputsByExportName, tE.outputByLogicalId) ()
      case _ => {
        println("\nWe should NOT get here. It was not possible to evaluate ImportValue params as a String. Node is: " + j)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalJoin(j: Json) = {

    val encodedDelimiter = encode (arrayAt(j,"Fn::Join",0))
    val encodedList = encode (arrayAt(j,"Fn::Join",1))

    (encodedDelimiter, encodedList) match {
      case (d:StringNode,l:ListNode[StringNode]) => JoinFunction(d,l) ()
      case _ => {
        println("\nWe should NOT get here. Unable to resolve params of Join to correct types. Json : " + j)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalSelect(j: Json) = {

    val encodedIndex = encode(arrayAt(j,"Fn::Select",0))
    val encodedList  = encode(arrayAt(j,"Fn::Select",1))

    (encodedIndex, encodedList) match {
      case (i:IntNode, l:ListNode[Node]) => SelectFunction(i,l) ()
      case _ => {
        println("\nWe should NOT get here. Unable to evaluate params of Select to the right types. Json node " + j)
        NoValue
      }
    }
  }


  private def validateParamsAndEvalSplit(j: Json) = {

    val encodedDelimiter = encode (arrayAt(j,"Fn::Split",0))
    val encodedString = encode (arrayAt(j,"Fn::Split",1))

    (encodedDelimiter, encodedString) match {
      case (d:StringNode, s:StringNode) => SplitFunction(d,s) ()
      case _ => {
        println("\nWe should NOT get here. Unable to evaluate params for SplitFunction. Node " + j )
        NoValue
      }
    }
  }


  private def validateParamsAndEvalSub(j: Json) = {

    val stringToMatch = arrayAt(j, "Fn::Sub",0)
    val isArray = j.field("Fn::Sub").get.isArray
    val isArrayWithTwoElements = isArray && j.field("Fn::Sub").get.array.get.size == 2
    val substitutionMap = if (isArrayWithTwoElements) Some(getNodesAsMapOfEvalStrings (arrayAt(j,"Fn::Sub",1))) else None

    stringToMatch match {
      case n if isArn(n) => {
        val subArn = SubFunction(tE.resources, tE.parameters, StringNode(n.string.get), substitutionMap) ()
        ArnFunction(tE.resources, tE.parameters, subArn.value, ssE.resourceByArn) ()
      }
      case _ => {
        val encodedStringToMatch = encode(stringToMatch)
        (substitutionMap,encodedStringToMatch) match {
          case (None,s:StringNode)    => SubFunction(tE.resources, tE.parameters, s) ()
          case (_,s:StringNode)       => SubFunction(tE.resources,tE.parameters,s,substitutionMap) ()
          case _ => {
            println("\nWe should NOT get here. Unable to resolve SubFunction params to right types. Node " + j)
            NoValue
          }
        }
      }
    }
  }


  private def validateParamsAndEvalTransform(j: Json) = {
    // TODO
    NoValue
  }


  private def validateParamsAndEvalRef(j: Json) = {
    val encodedReferredEntity = encode (j.field("Ref").get)
    RefFunction(encodedReferredEntity, tE.resources, tE.parameters, ssE) ()
  }





  private def getNodesAsMapOfEvalStrings(j:Json)=
    subFieldNames(j) zip subFieldValueEvalContents(j) toMap

  private def subFieldValueEvalContents (j: Json) =
    j.objectFieldsOrEmpty map ( f => getLowerCaseEvalStringField(j, f) )

  private def getLowerCaseEvalStringField(j: Json, field:String) = {
    val encodedField = encode( j.field(field).get )
    encodedField match {
      case NoValue => "" // TODO
      case StringNode(s) => s
      case StackSetResource(name,_,_,_) => name
    }
  }






  private def matchNodeByJsonType(j: Json) =
    j match {
      case n if n.isNumber && n.number.get.toInt.isDefined    => IntNode(n.number.get.toInt.get)
      case n if n.isNumber && n.number.get.toLong.isDefined   => LongNode(n.number.get.toLong.get)
      case n if n.isNumber && n.number.get.toDouble.isDefined => DoubleNode(n.number.get.toDouble.get)
      case n if n.isNumber && n.number.get.toFloat.isDefined  => FloatNode(n.number.get.toFloat.get)
      case n if n.isBool                                      => BooleanNode(n.bool.get)
      case n if n.isString                                    => StringNode(n.string.get.toLowerCase())
      case n => {
        println("We couldn't match the json node to any of the primitive possible types. Json is " + n)
        NoValue
      }
    }




  private def matchNodeBySubpropType (j:Json, subpropType:String) = {
    subpropType match {
      case "string"   if j.isString           => StringNode(j.string.get.toLowerCase())
      case "boolean"  if j.isBool             => BooleanNode(j.bool.get)
      case "integer"  if j.isNumber && j.number.get.toInt.isDefined     => IntNode(j.number.get.toInt.get)
      case "long"   if j.isNumber && j.number.get.toLong.isDefined      => LongNode(j.number.get.toLong.get)
      case "double" if j.isNumber && j.number.get.toDouble.isDefined    => DoubleNode(j.number.get.toDouble.get)
      case "float"  if j.isNumber && j.number.get.toFloat.isDefined     => FloatNode(j.number.get.toFloat.get)
      case _ => forceSubpropertyType(j,subpropType)
    }
  }




  private def forceSubpropertyType(j:Json, subpropType:String)  = {
    subpropType match {
      case "string" if j.isNumber     => StringNode(j.number.get.toString)
      case "string"                   => StringNode(j.bool.get.toString)
      case "boolean" if j.isString    => BooleanNode(j.string.get.toBoolean)
      case "boolean"                  => BooleanNode(j.number.get.truncateToInt>0)
      case "integer" if j.isString    => IntNode(j.string.get.toInt)
      case "integer" if j.isNumber    => IntNode(j.number.get.truncateToInt)
      case "integer"                  => IntNode(j.bool.get.toString.toInt)
      case "long" if j.isString       => LongNode(j.string.get.toLong)
      case "long" if j.isNumber       => LongNode(j.number.get.toLong.get)
      case "long"                     => LongNode(j.bool.get.toString.toLong)
      case "double" if j.isString     => DoubleNode(j.string.get.toDouble)
      case "double" if j.isNumber     => DoubleNode(j.number.get.toDouble.get)
      case "double"                   => DoubleNode(j.bool.get.toString.toDouble)
      case "float" if j.isString      => FloatNode(j.string.get.toFloat)
      case "float" if j.isNumber      => FloatNode(j.number.get.toFloat.get)
      case "float"                    => FloatNode(j.bool.get.toString.toFloat)
      case _ => {
        println("It was not possible to match the current json field with the expected subproperty type. Returning foreign node." )
        println("Json is " + j + " and subproperty is " + subpropType + " template is " + tE.template.name)
        ForeignResource(j.toString())  // TODO!
      }
    }
  }





  private def isNoValue(j: Json) = {
  (j.isString && j.string.get.equals("AWS::NoValue")) ||
    (j.isArray && j.array.get.size==1 && j.array.get(0).isString &&
      j.array.get(0).string.get.equals("No::Value"))
  }




  private def isArn(j:Json) =
    j.isString && j.string.get.startsWith("arn:") && j.string.get.contains(":")





  private def isIntrinsicFunction(j:Json)  = {
    EncodeUtils.subFieldNames(j)(0).startsWith("fn::") ||
    EncodeUtils.subFieldNames(j)(0).equals("ref")
  }



  private def isPolicyDoc(j: Json) = {
    j.hasField("Statement") && j.field("Statement").get.isArray &&
    j.field("Statement").get.array.get(0).hasField("Effect") &&
    j.field("Statement").get.array.get(0).hasField("Action")
  }



  private def isMapProperty(j:Json, subpropType:Option[(String,String)]) =
    mapProperty(j, subpropType).isDefined



  private def mapProperty(j: Json, subpropType:Option[(String,String)]): Option[String] =
    subpropType match {
    case Some((_,s)) if s.startsWith("mapentry_")   => Some(s.split("mapentry_").last)
    case None         => None
    case _            => None
  }



}