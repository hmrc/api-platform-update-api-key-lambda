package uk.gov.hmrc.apiplatform.updateapikey

import com.amazonaws.services.lambda.runtime.events.SQSEvent
import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.apigateway.model.{CreateApiKeyRequest, CreateUsagePlanKeyRequest}
import software.amazon.awssdk.services.sqs.SqsClient
import uk.gov.hmrc.api_platform_manage_api.AwsApiGatewayClient.awsApiGatewayClient
import uk.gov.hmrc.api_platform_manage_api.AwsIdRetriever
import uk.gov.hmrc.aws_gateway_proxied_request_lambda.SqsHandler

class UpsertApiKeyHandler(override val apiGatewayClient: ApiGatewayClient,
                          sqsClient: SqsClient,
                          environment: Map[String, String]) extends SqsHandler with AwsIdRetriever {

  val UsagePlansEnvironmentVariable: String = "usage_plans"

  def this() {
    this(awsApiGatewayClient, SqsClient.create(), sys.env)
  }

  override def handleInput(input: SQSEvent, context: Context): Unit = {
    implicit val logger: LambdaLogger = context.getLogger

    if (input.getRecords.size != 1) throw new IllegalArgumentException(s"Invalid number of records: ${input.getRecords.size}")

    val usagePlans: Map[String, String] = usagePlanIdsFromEnvironment()
    val updateRequest: UpdateAPIKeyRequest = fromJson[UpdateAPIKeyRequest](input.getRecords.get(0).getBody)

    val requestedUsagePlanId: String  = usagePlans.get(updateRequest.usagePlan) match {
      case Some(usagePlanId) => usagePlanId
      case None => throw new IllegalArgumentException(s"Requested Usage Plan [${updateRequest.usagePlan}] does not exist")
    }

    val findAPIKeyIdResult: (String, Boolean) = findApiKeyId(updateRequest)
    if (isNewAPIKey(findAPIKeyIdResult)) {
      addAPIKeyToUsagePlan(findAPIKeyIdResult._1, requestedUsagePlanId)
    } else {
      moveAPIKeyToUsagePlan(findAPIKeyIdResult._1, requestedUsagePlanId)
    }
  }

  def isNewAPIKey(findAPIKeyIdResult: (String, Boolean)): Boolean = findAPIKeyIdResult._2

  def usagePlanIdsFromEnvironment(): Map[String, String] = {
    environment.get(UsagePlansEnvironmentVariable) match {
      case Some(usagePlansJson) => fromJson[Map[String, String]](usagePlansJson)
      case None => throw new RuntimeException(s"No Usage Plans found under Environment Variable [$UsagePlansEnvironmentVariable]")
    }
  }

  def findApiKeyId(updateRequest: UpdateAPIKeyRequest)(implicit logger: LambdaLogger): (String, Boolean) =
    getAwsApiKeyIdByApplicationName(updateRequest.apiKeyName) match {
      case Some(apiKeyId) =>
        logger.log(s"Found existing API Key Id [$apiKeyId] for Application [${updateRequest.apiKeyName}]")
        (apiKeyId, false)
      case None =>
        val newAPIKeyId = createNewApiKey(updateRequest.apiKeyName, updateRequest.apiKeyValue)
        logger.log(s"Created API Key with Id [$newAPIKeyId] for Application [${updateRequest.apiKeyName}]")
        (newAPIKeyId, true)
    }

  def createNewApiKey(apiKeyName: String, apiKeyValue: String): String =
    apiGatewayClient.createApiKey(
      CreateApiKeyRequest.builder()
        .name(apiKeyName)
        .value(apiKeyValue)
        .build())
      .id()

  def addAPIKeyToUsagePlan(apiKeyId: String, usagePlanId: String) =
    apiGatewayClient.createUsagePlanKey(
      CreateUsagePlanKeyRequest.builder()
        .keyId(apiKeyId)
        .usagePlanId(usagePlanId)
        .keyType("API_KEY")
        .build())

  def moveAPIKeyToUsagePlan(apiKeyId: String, usagePlanId: String): Unit = {

  }
}

case class UpdateAPIKeyRequest(usagePlan: String, apiKeyName: String, apiKeyValue: String)