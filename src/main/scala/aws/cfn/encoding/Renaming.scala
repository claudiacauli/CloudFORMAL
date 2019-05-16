package aws.cfn.encoding

object Renaming{

  val resourceSpecificationName : String => String = resSpecFileName => resSpecFileName.split("Specification")(0).toLowerCase

  val resourceTypeName : String => String = resTypeName => resTypeName.split("::")(2).toLowerCase()

  val subpropertyTypeName : String => String = subPropTypeName =>
    if (subPropTypeName.contains('.')) subPropTypeName.split('.')(1).toLowerCase
    else subPropTypeName.toLowerCase

  val propertyName: (String,String) => String = (domainTypeName, propName) => domainTypeName.toLowerCase() + "_" + propName.toLowerCase

  val attributeName: (String,String) => String = (domainTypeName, attrName) => "attribute_" + propertyName(domainTypeName,attrName)

  val unambiguousPropertyName: (String,String,String) => String = (resTypeName, domainName, propName) =>
    (resTypeName + "_" + domainName + "_" + propName).toLowerCase()

}
