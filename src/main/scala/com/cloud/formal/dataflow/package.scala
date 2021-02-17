package com.cloud.formal

package object dataflow {


  private[dataflow]
  object Ontologies {

    val LISTDFDS: List[String] =
                  List(   "dfd.owl",
                          "dfdapigateway.owl",
                          "dfdcloudwatch.owl",
                          "dfdcloudtrail.owl",
                          "dfddynamodb.owl",
                          "dfdlambda.owl",
                          "dfds3.owl",
                          "dfdsns.owl",
                          "dfdsqs.owl" )

    val serviceTypeToDFD: Map[String, String] =
                  Map( "apigateway" -> "dfdapigateway.owl",
                      "cloudwatch" -> "dfdcloudwatch.owl",
                      "cloudtrail" -> "dfdcloudtrail.owl",
                      "dynamodb" -> "dfddynamodb.owl",
                      "lambda" -> "dfdlambda.owl",
                      "s3" -> "dfds3.owl",
                      "sns" -> "dfdsns.owl",
                      "sqs" -> "dfdsqs.owl"
    )


  }


  private[dataflow]
  object DFDNaming {

    val DFDTAG = "DFD"

  }

}
