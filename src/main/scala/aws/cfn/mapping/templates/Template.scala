package aws.cfn.mapping.templates

private class Template(val name: String) {

  private[templates] var parameters:   Map[String,GenericValueNode] = _
  private[templates] var mappings:     Map[String,Map[String,Either[String,Map[String,Any]]]] = _
  private[templates] var conditions:   Map[String,Boolean] = _
  private[templates] var outputByLogicalId:  Map[String,Node] = _
  private[templates] var outputByExportName: Map[String,Node] = _
  private[templates] var resources:          Map[String,StackSetResource]  = _
  private[templates] var policyStatements:   Set[Statement] = _

  override def toString: String = {
    "\t\tTemplate: " + name + "\n" +
      prettyString(parameters,"Parameters") +
      prettyString(mappings, "Mappings") +
      prettyString(conditions, "Conditions") +
      prettyString(outputByLogicalId, "Outputs By ID") +
      prettyString(outputByExportName, "Outputs By Export Name")
  }

  private def prettyString(map : Map[String,Any], s : String) = {
    if (map.nonEmpty)
      "\t\t "+s+": \n"   + map.foldLeft("")((a,b)=> a + "\t\t\t("+b._1+" -> "+b._2+")\n")
    else ""
  }

}
