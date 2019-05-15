# CFnDL
Encodes **AWS CloudFormation** Templates into **Description Logic** knowledge bases. 

CFnDL translates AWS CloudFormation Templates into Description Logic knowledge bases. It maps 
Resource Specification files, service Actions, and IAM Policies syntax into a _Terminological Box_ (aka TBox) and the actual 
Resources Instances of a given template into an _Assertional Box_ (aka ABox). TBox and ABox are serialized in OWL, Web 
Ontology Language, allowing for all the features of the very expressive Description Logic _SROIQ_. 

The produced model is an approximation of the Cloud Infrastructure to be deployed and can be reasoned upon and used as
 a basis for formal analysis and verification tools.
