# CloudFORMAL
Is a prototype tool to encode **AWS CloudFormation** Templates into **Description Logic** knowledge bases. 

CloudFORMAL translates AWS CloudFormation Templates into Description Logic knowledge bases. It maps 
Resource Specification files (https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cfn-resource-specification.html) into a _Terminological Box_ (aka TBox) and the actual Resources Instances of a given template into an _Assertional Box_ (aka ABox). TBox and ABox are in _ALCOIQ_ and serialized in OWL 2, Web Ontology Language.

The output OWL files can be opened and navigated in Protege. The produced model is a representation of the infrastructure to be deployed and can be reasoned upon and used as a basis for studies about the formal analysis and verification of Cloud infrastructure before deployment.
