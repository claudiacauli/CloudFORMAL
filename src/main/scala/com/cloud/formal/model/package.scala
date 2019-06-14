package com.claudiacauli.www.cloudformal

package object model{


  private[model]
  object Format extends  Enumeration {
    val Rdf         = "rdf"
    val Xml         = "xml"
    val Turtle      = "ttl"
    val Functional  = "fun"
    val DefaultFormat: String = Rdf
  }


  private[model]
  object ModelSuffix extends Enumeration {
    val StackSet        = "_StackSetModel"
    val Infrastructure  = "_InfrastructureModel"
    val Permissions     = "_PermissionsModel"
  }


  private[model]
  object ModelFileSuffix extends Enumeration {
    val StackSet: String        = ModelSuffix.StackSet + Extension.Owl
    val Infrastructure: String  = ModelSuffix.Infrastructure + Extension.Owl
    val Permissions: String     = ModelSuffix.Permissions + Extension.Owl
  }


  private[model]
  object ProtegeCatalogue {
    val FileName: String = "catalog-v001.xml"
  }


}

