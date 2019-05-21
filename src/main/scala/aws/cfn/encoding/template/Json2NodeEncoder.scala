package aws.cfn.encoding.template

import argonaut.{DecodeJson, Json}
import aws.cfn.encoding.EncodeUtils
import aws.cfn.formalization._


protected class Json2NodeEncoder(ssE: Json2StackSetEncoder, tE: Json2TemplateEncoder, rE: Json2ResourceEncoder) {


  def encode(json: Json, subpropertyType: Option[String] = None): Node = {
    if (json.isNull) NoValue
    else if (json.isObject && isIntrinsicFunction(json)) evalIntrinsicFunction(json)
    else if (json.isObject && isMapProperty(json,subpropertyType).isDefined) encodeMapProperty(json,isMapProperty(json,subpropertyType).get)
    else if (json.isObject && isPolicy(json)) rE.PolicyEncoder.encode(json)
    else if (json.isObject) encodeSubproperty(json,subpropertyType)
    else if (json.isArray) encodeArrayNode(json,subpropertyType)
    else encodeValueNode(json)
  }




  def encodeArrayNode(json: Json, subpropertyType:Option[String]): ListNode[Node]
    = ListNode((json.array.get map ( j => encode(j,subpropertyType))).toVector)



  def encodeValueNode(json: Json): Node = {
    val stringValue = DecodeJson.StringDecodeJson.decodeJson(json).toOption.get.toLowerCase()
    if (json.isBool) BooleanNode(DecodeJson.BooleanDecodeJson.decodeJson(json).toOption.get)
    else if (json.isNumber) LongNode(DecodeJson.LongDecodeJson.decodeJson(json).toOption.get)
    else StringNode(stringValue)
  }

  def isIntrinsicFunction(json:Json) : Boolean = {
    EncodeUtils.subFieldNames(json)(0).startsWith("fn::") ||
    EncodeUtils.subFieldNames(json)(0).equals("ref")
  }

  def evalIntrinsicFunction(json:Json) : Node = {

    def arrayAt(funName:String, index:Int) = json.field(funName).get.array.get(index)

    EncodeUtils.subFieldNames(json)(0) match {
      case "fn::base64"
        => Base64Function (encode (json.field ("Fn::Base64").get).asInstanceOf[StringNode] ) ()
      case "fn::cidr"
        => CidrFunction (encode (arrayAt("Fn::Cidr",0)).asInstanceOf[StringNode],
              encode (arrayAt("Fn::Cidr",1) ).asInstanceOf[IntNode],
              encode (arrayAt("Fn::Cidr",2)).asInstanceOf[IntNode] ) ()
      case "fn::if"
        => IfFunction (encode (arrayAt("Fn::If",0)).asInstanceOf[BooleanNode],
            encode (arrayAt("Fn::If",1)),
            encode (arrayAt("Fn::If",2))) ()
      case "fn::not"
        => NotFunction (encode (json.field ("Fn::Not").get).asInstanceOf[BooleanNode]) ()
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
      case "fn::getazs" => GetAZsFunction (encode (json.field ("fn::GetAZs").get).asInstanceOf[StringNode] ) ()
      case "fn::importvalue" =>
        ImportValueFunction (encode (json.field ("fn::ImportValue").get).asInstanceOf[StringNode],
          ssE.outputsByExportName, tE.outputByLogicalId) ()
      case "fn::join" => JoinFunction (
        encode (arrayAt("Fn::Join",0)).asInstanceOf[StringNode],
        encode (arrayAt("Fn::Join",1)).asInstanceOf[ListNode[Node]] ) ()
      case "fn::select" => SelectFunction (
        encode (arrayAt("Fn::Select",0)).asInstanceOf[IntNode],
        encode (arrayAt("Fn::Select",1)).asInstanceOf[ListNode[Node]] ) ()
      case "fn::split" => SplitFunction (
        encode (arrayAt("Fn::Split",0)).asInstanceOf[StringNode],
        encode (arrayAt("Fn::Split",1)).asInstanceOf[StringNode] ) ()
      case "fn::sub" =>
        if ( json.field("Fn::Sub").get.isArray && json.field("Fn::Sub").get.array.get.size == 2)
        {
          SubFunction (tE.resources, tE.parameters, encode (arrayAt("Fn::Sub",0) ).asInstanceOf[StringNode],
          Some (EncodeUtils.getNodesAsMapOfStrings (arrayAt("Fn::Sub",1)) ) ) ()
        }
        else if ( json.field("Fn::Sub").get.isArray && json.field("Fn::Sub").get.array.get.size == 1)
        {
          SubFunction (tE.resources, tE.parameters, encode (arrayAt("Fn::Sub",0) ).asInstanceOf[StringNode]) ()
        }
        else {
          SubFunction (tE.resources, tE.parameters, encode (json.field("Fn::Sub").get).asInstanceOf[StringNode] ) ()
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


  def encodeMapProperty(json: Json, subpropertyType: String) = ??? // TODO



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

}