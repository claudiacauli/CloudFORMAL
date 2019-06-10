package aws.cfn.maps

object ResourcesNameFieldsMap {

  def lookUp(s: String, r: String) :Option[(String)]= map.get(s+"::"+r)

  private val map : Map[String,String] = Map(
    // Resource -> Name of top level property containing resource's name
    "s3::bucket" -> "bucketname",
    "sqs::queue" -> "queuename",
    "sns::topic" -> "topicname",
    "route53::hostedzone" -> "name",
    "route53::recordset" -> "name",
    "logs::logstream" -> "logstreamname",
    "logs::loggroup" -> "loggroupname",
    "lambda::function" -> "functioname",
    "kinesis::stream" -> "name",
    "iam::role" -> "rolename",
    "iam::group" -> "groupname",
    "events::rule" -> "name",
    "dynamodb::table" -> "tablename"

  )

}
