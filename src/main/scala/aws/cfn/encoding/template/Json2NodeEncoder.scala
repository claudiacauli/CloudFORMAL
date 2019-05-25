package aws.cfn.encoding.template

import argonaut.{DecodeJson, Json}
import aws.cfn.encoding.EncodeUtils
import aws.cfn.encoding.EncodeUtils.subFieldNames
import aws.cfn.formalization
import aws.cfn.formalization._

import scala.language.postfixOps

protected class Json2NodeEncoder(ssE: Json2StackSetEncoder, tE: Json2TemplateEncoder, rE: Json2ResourceEncoder) {


  def encode(json: Json, subpropertyType: Option[String] = None): Node = {
    println("Working with node " + json)
    println("Parameters " + tE.parameters)
    if ( isArn(json) ) ArnFunction(tE.resources, tE.parameters, json.string.get, ssE.resourceByArn)()
    else if (json.isNull)
      NoValue
    else if (json.isObject && isIntrinsicFunction(json))
      evalIntrinsicFunction(json)
    else if (json.isObject && isMapProperty(json,subpropertyType).isDefined)
      encodeMapProperty(json,isMapProperty(json,subpropertyType).get)
    else if (json.isObject && isPolicy(json))
      rE.PolicyEncoder.encode(json)
    else if (json.isObject && subpropertyType.isDefined && subpropertyType.get.equals("string"))
      StringNode(json.toString())
    else if (json.isObject)
      encodeSubproperty(json,subpropertyType)
    else if (json.isArray)
      encodeArrayNode(json,subpropertyType)
    else
      encodeValueNode(json,subpropertyType)
  }




  def encodeArrayNode(json: Json, subpropertyType:Option[String]): ListNode[Node]
    = ListNode((json.array.get map ( j => encode(j,subpropertyType))).toVector)



  def encodeValueNode(json: Json, subpropertyType: Option[String]): Node = {


    def matchJsonByItsType : Node = {
      if (json.isString ) StringNode(DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.toLowerCase())
      else if (json.isBool ) BooleanNode(DecodeJson.BooleanDecodeJson.decodeJson(json).toOption.get)
      else if (json.isNumber && json.number.get.toInt.isDefined) IntNode(json.number.get.toInt.get)
      else if (json.isNumber && json.number.get.toLong.isDefined) LongNode(json.number.get.toLong.get)
      else if (json.isNumber && json.number.get.toDouble.isDefined) DoubleNode(json.number.get.toDouble.get)
      else if (json.isNumber && json.number.get.toLong.isDefined) FloatNode(json.number.get.toLong.get)
      else StringNode(DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.toLowerCase())
    }

    def matchJsonBySubpropertyType : Node = {
      if (json.isString && subpropertyType.get.equals("string")) StringNode(DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.toLowerCase())
      else if (json.isBool && subpropertyType.get.equals("boolean")) BooleanNode(DecodeJson.BooleanDecodeJson.decodeJson(json).toOption.get)
      else if (json.isNumber && subpropertyType.get.equals("integer")) IntNode(json.number.get.toInt.get)
      else if (json.isNumber && subpropertyType.get.equals("long")) LongNode(json.number.get.toLong.get)
      else if (json.isNumber && subpropertyType.get.equals("double")) DoubleNode(json.number.get.toDouble.get)
      else if (json.isNumber && subpropertyType.get.equals("float")) FloatNode(json.number.get.toLong.get)
      else forceSubpropertyType   // THIS IS A FALLBACK SOLUTION... FOR THOSE FIELD THAT DO NOT RESPECT THEIR OWN SPECIFICATION!
    }

    def forceSubpropertyType : Node = {
      subpropertyType.get match {
        case "string" => {
          if (json.isNumber) StringNode(json.number.get.toString)
          else StringNode(json.bool.get.toString)
        }
        case "boolean" => {
          if (json.isString ) BooleanNode(json.string.get.toBoolean)
          else BooleanNode(json.number.get.truncateToInt > 0)
        }
        case "integer" =>
          if (json.isString) IntNode(json.string.get.toInt)
          else if (json.isNumber) IntNode(json.number.get.truncateToInt)
          else IntNode(json.bool.get.toString.toInt)
        case "long" =>
          if (json.isString) LongNode(json.string.get.toLong)
          else if (json.isNumber) LongNode(json.number.get.toLong.get)
          else LongNode(json.bool.get.toString.toLong)
        case "double" =>
          if (json.isString) DoubleNode(json.string.get.toDouble)
          else if (json.isNumber) DoubleNode(json.number.get.toDouble.get)
          else DoubleNode(json.bool.get.toString.toDouble)
        case "float" =>
          if (json.isString) FloatNode(json.string.get.toFloat)
          else if (json.isNumber) FloatNode(json.number.get.toFloat.get)
          else FloatNode(json.bool.get.toString.toFloat)
        case _ => ForeignNode(json.toString())
      }
    }



    subpropertyType match {
      case Some(spt) => matchJsonBySubpropertyType
      case None => matchJsonByItsType
    }

  }

  def isArn(json:Json) : Boolean =
    json.isString && json.string.get.startsWith("arn:") && json.string.get.contains(":")


  def isIntrinsicFunction(json:Json) : Boolean = {
    EncodeUtils.subFieldNames(json)(0).startsWith("fn::") ||
    EncodeUtils.subFieldNames(json)(0).equals("ref")
  }

  def evalIntrinsicFunction(json:Json) : Node = {

    def arrayAt(funName:String, index:Int) = {
      if (json.field(funName).get.isArray)
        json.field(funName).get.array.get(index)
      else
        json.field(funName).get
    }

    println("Working on node " + json)

    EncodeUtils.subFieldNames(json)(0) match {
      case "fn::base64"
        => Base64Function (encode (json.field ("Fn::Base64").get).asInstanceOf[StringNode] ) ()
      case "fn::cidr"
        => CidrFunction (encode (arrayAt("Fn::Cidr",0)).asInstanceOf[StringNode],
              encode (arrayAt("Fn::Cidr",1) ).asInstanceOf[IntNode],
              encode (arrayAt("Fn::Cidr",2)).asInstanceOf[IntNode] ) ()
      case "fn::if" => {
        val encodedCondition = encode (arrayAt("Fn::If",0))
        if (encodedCondition.isInstanceOf[StringNode]) {
          tE.conditions.get(encodedCondition.asInstanceOf[StringNode].value) match {
            case None => NoValue
            case Some(b) => IfFunction ( BooleanNode(b) , encode (arrayAt("Fn::If",1)), encode (arrayAt("Fn::If",2))) ()
          }
        } else {
          IfFunction ( encodedCondition.asInstanceOf[BooleanNode] ,
          encode (arrayAt("Fn::If",1)),
          encode (arrayAt("Fn::If",2))) ()
        }
      }
      case "fn::not" => {
        var encodedJson = encode(json.field("Fn::Not").get)
        encodedJson match {
          case n : ListNode[BooleanNode] => NotFunction (n.value.head) ()
          case n : BooleanNode => NotFunction(n)()
        }
      }
      case "fn::and"
        => AndFunction (encode (arrayAt("Fn::And",0) ).asInstanceOf[BooleanNode],
        encode (arrayAt("Fn::And",1)).asInstanceOf[BooleanNode]) ()
      case "fn::equals"
        => EqualsFunction (encode (arrayAt("Fn::Equals",0)),
        encode (arrayAt("Fn::Equals",1))) ()
      case "fn::or"
        => OrFunction (encode (arrayAt("Fn::Or",0)).asInstanceOf[BooleanNode],
        encode (arrayAt("Fn::Or",1)).asInstanceOf[BooleanNode] ) ()
      case "fn::findinmap" =>
        if (json.field ("Fn::FindInMap").get.array.get.size == 2) {
          FindInMapFunction (tE.mappings,
            encode(arrayAt("Fn::FindInMap",0)).asInstanceOf[StringNode],
            encode(arrayAt("Fn::FindInMap",1)).asInstanceOf[StringNode]) ()
        } else {
          FindInMapFunction (tE.mappings,
            encode(arrayAt("Fn::FindInMap",0)).asInstanceOf[StringNode],
            encode(arrayAt("Fn::FindInMap",1)).asInstanceOf[StringNode],
            encode(arrayAt("Fn::FindInMap",2)).asInstanceOf[StringNode] ) ()
        }
      case "fn::getatt" => GetAttFunction (
        encode (arrayAt("Fn::GetAtt",0)).asInstanceOf[StringNode],
        encode (arrayAt("Fn::GetAtt",1)).asInstanceOf[StringNode], tE.resources) ()
      case "fn::getazs" => GetAZsFunction (encode (json.field ("Fn::GetAZs").get).asInstanceOf[StringNode] ) ()
      case "fn::importvalue" =>
        ImportValueFunction (encode (json.field ("Fn::ImportValue").get).asInstanceOf[StringNode],
          ssE.outputsByExportName, tE.outputByLogicalId) ()
      case "fn::join" => JoinFunction (
        encode (arrayAt("Fn::Join",0)).asInstanceOf[StringNode],
        encode (arrayAt("Fn::Join",1)).asInstanceOf[ListNode[Node]] ) ()
      case "fn::select" => {
        var p2 = encode (arrayAt("Fn::Select",1))
        if (!p2.isInstanceOf[ListNode[Node]])
          p2 = ListNode(Vector(p2))
        else if (p2.equals(NoValue))
          NoValue
        SelectFunction (
          encode (arrayAt("Fn::Select",0)).asInstanceOf[IntNode],
          p2.asInstanceOf[ListNode[Node]] ) ()
      }
      case "fn::split" => SplitFunction (
        encode (arrayAt("Fn::Split",0)).asInstanceOf[StringNode],
        encode (arrayAt("Fn::Split",1)).asInstanceOf[StringNode] ) ()
      case "fn::sub" =>
        if (  isArn(arrayAt("Fn::Sub",0)) ){
          SubFunction (tE.resources, tE.parameters, StringNode(arrayAt("Fn::Sub",0).string.get ) ) ()
        }
        else if ( json.field("Fn::Sub").get.isArray && json.field("Fn::Sub").get.array.get.size == 2)
        {
          SubFunction (tE.resources, tE.parameters, encode (arrayAt("Fn::Sub",0) ).asInstanceOf[StringNode],
          Some (getNodesAsMapOfEvalStrings (arrayAt("Fn::Sub",1)) ) ) ()
        }
        else if ( json.field("Fn::Sub").get.isArray && json.field("Fn::Sub").get.array.get.size == 1)
        {
          SubFunction (tE.resources, tE.parameters, encode (arrayAt("Fn::Sub",0) ).asInstanceOf[StringNode]) ()
        }
        else {
          SubFunction (tE.resources, tE.parameters, encode(json.field("Fn::Sub").get).asInstanceOf[StringNode] ) ()
        }
      case "fn::transform" => NoValue // TODO!
      case "ref" => RefFunction (
        encode (json.field("Ref").get),
        tE.resources, tE.parameters,ssE) ()
    }
  }


  def isMapProperty(json: Json, subpropertyType:Option[String]) : Option[String] = subpropertyType match {
    case None => None
    case Some(s) if s.startsWith("mapentry_") => Some(s.split("mapentry_").last)
    case _ => None
  }


  def encodeMapProperty(json: Json, subpropertyType: String) = NoValue // TODO



  def encodeSubproperty(json: Json, subpropertyType:Option[String]): Node = {

    def givenProperties = (json.objectFieldsOrEmpty map (f => subpropertyType.get+"_"+f.toString)).toSet

    def nodeObjectForProperty(json: Json, propFullName: String) : Node = {
      val propTemplateName = propFullName.split(subpropertyType.get+ "_").last
      encode( json.field(propTemplateName).get, rE.rangeNameOf(propFullName))
    }

    subpropertyType match {
      case None => StringNode( DecodeJson.StringDecodeJson.decodeJson(json).toOption.get )
      case Some(spt) => {
        val absentProperties : Set[String] = rE.subPropertiesNamesOfClassName(spt) -- givenProperties.map(s => s.toLowerCase())
        val presentProperties : Map[String,Node] = (givenProperties flatMap ( propName => Map(propName -> nodeObjectForProperty(json,propName)))).toMap
        SubpropertyNode(presentProperties,absentProperties)
      }
    }


  }


  def isPolicy(json: Json): Boolean = {
    json.hasField("Statement") && json.field("Statement").get.isArray &&
      json.field("Statement").get.array.get(0).hasField("Effect") &&
      json.field("Statement").get.array.get(0).hasField("Action")
  }


  def getNodesAsMapOfEvalStrings(j:Json) : Map[String, String] = subFieldNames(j) zip subFieldValueEvalContents(j) toMap

  def subFieldValueEvalContents (json: Json) : List[String] =
    json.objectFieldsOrEmpty map ( f => getLowerCaseEvalStringField(json, f) )

  def getLowerCaseEvalStringField(json: Json, field:String): String = {
    encode(json.field(field).get) match {
      case NoValue => "" // TODO
      case StringNode(s) => s
    }
  }




}