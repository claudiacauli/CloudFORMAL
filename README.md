# CloudFORMAL
Is a prototype tool to encode **AWS CloudFormation** Templates into **Description Logic** knowledge bases. 

CloudFORMAL translates AWS CloudFormation Templates into Description Logic knowledge bases. It maps 
Resource Specification files (https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cfn-resource-specification.html), service Actions, and common AWS terms (i.e., concepts such as Stack, Account, User, Public, etc.) into a _Terminological Box_ (aka TBox) and the actual 
Resources Instances of a given template into an _Assertional Box_ (aka ABox). TBox and ABox are serialized in OWL 2, Web 
Ontology Language, allowing for all the features of the very expressive Description Logic _SROIQ_. 

The output OWL files can be opened and navigated in Protege. The produced model is a representation of the infrastructure to be deployed and can be reasoned upon and used as a basis for studies about the formal analysis and verification of Cloud infrastructure before deployment.
