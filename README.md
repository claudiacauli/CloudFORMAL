# CloudFORMAL
CloudFORMAL is a prototype Scala tool to encode **AWS CloudFormation**
Templates into **Description Logic** models. It maps the CFn Resource
Specification files (from
[here](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cfn-resource-specification.html))
into _Terminological Boxes_, and the actual CFn Templates into
_Assertional Boxes_, building a comprehensive model that is aware of
the inter-connections among resources. Models are serialized in OWL2,
using the OwlApi, and reasoned upon with the JFact inference engine.
Expressivity of the models is that of the description logic ALCOIQ.

Samples of input infrastructures (actual templates found through
GitHub advanced search) are in the _Benchmarks_ folder. Their
corresponding output models are in the _BenchmarksOut_ folder. The
_*InfrastructureModel.owl_ files can be opened, navigated, and queried
in Protégé. Models are automatically checked against common security
best-practices, such as those recommended by tools like ScoutSuite and
CloudConformity (re-implemented
[here](https://github.com/claudiacauli/CloudFORMAL/tree/master/src/main/scala/com/cloud/formal/reasoning/properties)).
Reports of such checks are found in the _*Report.csv files.

## How to Run
To run the tool, Java and Maven are needed. To check if these
are installed, run `java -version` and `mvn -v` on a terminal. If
either one is not installed, try following the instructions
[here](https://docs.oracle.com/javase/8/docs/technotes/guides/install/install_overview.html)
and/or [here](http://maven.apache.org/install.html).

Once Java and Maven are working, run the install script with
```
source ./install.sh
```
The script will initialize the Maven project from pom.xml,
compile it, and build an executable .jar. It will also create the
volatile alias `CloudFORMAL` for the full command `java -Xmx1G -jar
target/ExecCloudFORMAL.jar`. The alias is deleted when the terminal is
closed (and won't work at all if the script is run without `source`).

## Try it out!
Three main features are currently implemented:

###### 1. Encoding cfn Resource Specification
A folder with all cfn specifications is available at
_src/main/resources/CloudFormationResourceSpecification_ (the latest version
for N. Virginia region can be downloaded from
[here](https://d1uauaxba7bl26.cloudfront.net/latest/CloudFormationResourceSpecification.zip)).
To use this feature type one of the commands below.
```
CloudFORMAL -s src/main/resources/CloudFormationResourceSpecification/
CloudFORMAL -s pathToYourSpecFolder pathToYourSpecOutDir
```
The first argument is the input directory (the directory containing
all the specification jsons), the second argument is the output
directory. If not output directory is given, the default will be
_src/main/resources/terminology/resourcespecificationsOwl/_.

###### 2. Encoding cfn Templates
The folder containing the sample inputs is _Benchmarks_. Each sample
input folder **must** have a specific structure: parent folder **>**
infrastructure folder **>** stackset folders **>** (template json files +
descriptor files). Descriptor files are used to feed potential input
parameters and pseudo-parameters to the template. See the _Benchmarks_
folder for guidance. To run this feature type one of the following
commands.
```
CloudFORMAL -m  Benchmarks/01_sqilup/
CloudFORMAL -m  Benchmarks/16_retailmenot/
CloudFORMAL -m  Benchmarks/04_stationeering/ pathToYourTemplateOutDir
CloudFORMAL -ma Benchmarks/
CloudFORMAL -ma Benchmarks/ pathToYourTemplateOutDir
```
The first three commands model one infrastructure at the time, the
last two model all infrastructures under a given directory. If the
output folder path is omitted, default will be _BenchmarksOut_.
During this encoding, some terminology .owl models will be imported
and saved in the final model directory.

###### 3. Run Security Checks
Once the models have been created (e.g., in the _BenchmarksOut_
directory), we can perform the reasoning. This step needs an
_*InfrastructureModel.owl_ file as input. To test, run one of the commands:
```
CloudFORMAL -r  BenchmarksOut/01_sqilup/01_sqilup_InfrastructureModel.owl
CloudFORMAL -ra BenchmarksOut/
CloudFORMAL -r  pathToYourTemplateOutDir/dirName/
CloudFORMAL -ra pathToYourTemplateOutDir/
```
The `-r` option reasons over one infrastructure, `-ra` reasons against
all models in the given directory. Running this command will save a
_*Report.csv_ file in the corresponding output folder. This command
doesn't need a second argument.
