package com.cloud.formal.specification

import org.junit.runner.RunWith
import org.junit.runners.BlockJUnit4ClassRunner
import org.scalatest.FunSuite


@RunWith(classOf[BlockJUnit4ClassRunner])
class DecodingTests extends FunSuite {

//    val mapFile = new File ("/Users/claudia/IdeaProjects/cfontology/src/main/resources/customProperties.txt")
//    val it = Source.fromFile(mapFile).getLines()
//
//    var newString : String = ""
//
//    while(it.hasNext){
//
//      val line = it.next()
//      val lineSegments = line.split(" ")
//      val resSpecName = lineSegments(0)
//      val propName = lineSegments(1)
//      val targetResSpecName = if (!lineSegments(2).equals("null")) lineSegments(2).split("#")(0).toLowerCase else "null"
//        val targetResName = if (!lineSegments(2).equals("null")) lineSegments(2).split("#")(1).toLowerCase() else "null"
//      val isRequired = lineSegments(4)
//      val isFunctional = lineSegments(3)
//
//      newString = newString.concat( "\"" + resSpecName.toLowerCase()  + "_" + propName.toLowerCase() + "\" -> " + "(\""  + targetResSpecName.toLowerCase()
//      + "\", " + "\"" + targetResName.toLowerCase + "\", "+ isRequired +", "+ isFunctional +" ),\n"
//      )
//    }
//
//    val newFile = new File ("map.txt")
//    val fw = new FileWriter(newFile)
//    fw.write(newString)
//    fw.close()

//  def fileFromString (s : String) : File = {
//    val f1 : File = new File( "TestFile" + new Random(1000) + ".txt" )
//    val fw1 : FileWriter = new FileWriter(f1)
//    fw1.write(s)
//    fw1.close()
//    f1
//  }
//
//  val f1 = fileFromString("{")
//  val f2 = fileFromString("")
//  val f3 = fileFromString("{}")
//  val f4 = fileFromString("{ \"ResourceType\" : ")
//
//  test("Log error and None parsing file with no valid JSON"){
//    assert( Decode.parse( f1 ) === None )
//  }
//
//  test("Log error and None parsing file empty") {
//    assert ( Decode.parse( f2 ) === Parse.parseOption("") )
//  }
//
//  test("EmptyJson parsing file \"{}\"") {
//    assert ( Decode.parse( f3 ) === Parse.parseOption("{}") )
//  }
//
//  test ( "Again not valid JSON" ){
//    assert( Decode.parse( f4) === None )
//  }
//
//    val s3bucketSpecJson = Parser.jsonFromFile(new File("/Users/claudia/Downloads/CloudFormationResourceSpecification/S3BucketSpecification.json")).get
//    println(ResourceSpecificationDecoder.decodeWithName(s3bucketSpecJson, "S3Bucket")  )

  //  val attrNode = Decode.parse(new File("/Users/claudia/attributeNode.json")).get
//  val resourceTypeNode = Decode.parse(new File("/Users/claudia/resourceType.json")).get
//
//  val propN = ( resourceTypeNode.field(resourceTypeNode.objectFields.get(0)).get.field("Properties").get )
//  val attrN = resourceTypeNode.field(resourceTypeNode.objectFields.get(0)).get.field("Attributes").get
//  val rs = new ResourceTypeDecoder(resourceTypeNode.objectFields.get(0), resourceTypeNode.field(resourceTypeNode.objectFields.get(0)).get )
//
//  println (propN)
//  println ( rs.resource.props )
//
//  println("+++")
//
//  println(attrN)
//  println( rs.resource.attrs )
//
//  //
////  println("===")
////  rs.attributeList foreach (a => println(a))
////  println("===")
////  rs.propertyList foreach println
//
//
//
//  //  println( Decoder.getPropertyNodes(propNode) )
////
////  test ( "isProperty" ){
////    assert( Decoder.isPropertiesNode(propNode) === true )
////  }
////
////  test ( "getPropertyNode" ) {
////    assert ( Decoder.getPropertyNodes(propNode)("BasePath") === null )
////  }
////
////  test ( "isAttribute" ){
////    assert( Decoder.isAttributesNode(attrNode) == false )
////  }


//    OntologyWriter.write(DLEncoder.encode(JsonDecoder.decodeWithName(   Parser.jsonFromFilePath(
//        "/Users/claudia/Downloads/CloudFormationResourceSpecification/S3BucketPolicySpecification.json"
//    ).get, "S3BucketPolicy" )))





}
