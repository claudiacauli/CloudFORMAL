/*
 *    Copyright 2019 Claudia Cauli
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.cloud.formal.mapping

package object templates{


  private[templates]
  object TemplateTag extends Enumeration
  {
    val Condition    = "Condition"
    val Conditions   = "Conditions"
    val Parameters   = "Parameters"
    val Transform    = "Transform"
    val Resources    = "Resources"
    val Mappings     = "Mappings"
    val Outputs      = "Outputs"
    val Default      = "Default"
    val Type         = "Type"
    val ValueTag     = "Value"
    val Name         = "Name"
    val Export       = "Export"
    val CloudFormation = "CloudFormation"
    val CustomResource = "CustomResource"
  }


  private[templates]
  object ParameterType extends Enumeration
  {
    val String = "string"
    val Number = "number"
    val ListOfNumber = "list<number>"
    val CommaDelimitedList = "commadelimitedlist"
  }


  private[templates]
  object PseudoParameter extends Enumeration
  {
    val AccountId         : String = "aws::accountid"
    val StackName         : String = "aws::stackname"
    val StackId           : String = "aws::stackid"
    val Partition         : String = "aws::partition"
    val Region            : String = "aws::region"
    val URLSuffix         : String = "aws::urlsuffix"
    val NotificationARNs  : String = "aws::notificationarns"
    val NoValue           : String = "aws::novalue"
  }


  private[templates]
  object CFnFunTag extends Enumeration
  {

    val FunTagRegex     = "^(Fn::|Ref).*"

    val Base64UC        = "Fn::Base64"
    val CidrUC          = "Fn::Cidr"
    val IfUC            = "Fn::If"
    val NotUC           = "Fn::Not"
    val AndUC           = "Fn::And"
    val EqualsUC        = "Fn::Equals"
    val OrUC            = "Fn::Or"
    val FindInMapUC     = "Fn::FindInMap"
    val GetAttUC        = "Fn::GetAtt"
    val GetAZsUC        = "Fn::GetAZs"
    val ImportValueUC   = "Fn::ImportValue"
    val JoinUC          = "Fn::Join"
    val SelectUC        = "Fn::Select"
    val SplitUC         = "Fn::Split"
    val SubUC           = "Fn::Sub"
    val TransformUC     = "Fn::Transform"
    val RefUC           = "Ref"

    val Base64: String      = Base64UC.toLowerCase
    val Cidr: String        = CidrUC.toLowerCase
    val If: String          = IfUC.toLowerCase
    val Not: String         = NotUC.toLowerCase
    val And: String         = AndUC.toLowerCase
    val Equals: String      = EqualsUC.toLowerCase
    val Or: String          = OrUC.toLowerCase
    val FindInMap: String   = FindInMapUC.toLowerCase
    val GetAtt: String      = GetAttUC.toLowerCase
    val GetAZs: String      = GetAZsUC.toLowerCase
    val ImportValue: String = ImportValueUC.toLowerCase
    val Join: String        = JoinUC.toLowerCase
    val Select: String      = SelectUC.toLowerCase
    val Split: String       = SplitUC.toLowerCase
    val Sub: String         = SubUC.toLowerCase
    val Transform: String   = TransformUC.toLowerCase
    val Ref: String         = RefUC.toLowerCase
  }


  private[templates]
  object Policy extends Enumeration
  {
    val StatementTag      = "Statement"
    val EffectTag         = "Effect"
    val AllowValue        = "allow"
    val PrincipalTag      = "Principal"
    val NotPrincipalTag   = "NotPrincipal"
    val ActionTag         = "Action"
    val NotActionTag      = "NotAction"
    val ResourceTag       = "Resource"
    val NotResourceTag    = "NotResource"
    val ConditionTag      = "Condition"
    val StarValue         = "*"
    val ServiceTag        = "Service"
    val AWSTag            = "AWS"
    val FederatedTag      = "Federated"
    val CanonicalUserTag  = "CanonicalUser"
  }


  private[templates]
  object SummaryFileName extends Enumeration
  {
    val Infrastructure   = "InfrastructureSummary.txt"
    val Permissions      = "PermissionsSummary.txt"
  }


}

