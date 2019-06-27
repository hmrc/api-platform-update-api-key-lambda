package uk.gov.hmrc.apiplatform.upsertapikey

import com.amazonaws.services.lambda.runtime.events.SQSEvent
import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.apigateway.model.{CreateApiKeyRequest, CreateUsagePlanKeyRequest, DeleteUsagePlanKeyRequest, GetUsagePlanKeysRequest}
import software.amazon.awssdk.services.sqs.SqsClient
import uk.gov.hmrc.api_platform_manage_api.AwsApiGatewayClient.awsApiGatewayClient
import uk.gov.hmrc.api_platform_manage_api.AwsIdRetriever
import uk.gov.hmrc.aws_gateway_proxied_request_lambda.SqsHandler

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class UpsertApiKeyHandler(override val apiGatewayClient: ApiGatewayClient,
                          sqsClient: SqsClient,
                          environment: Map[String, String]) extends SqsHandler with AwsIdRetriever {

  val UsagePlansEnvironmentVariable: String = "usage_plans"

  def this() {
    this(awsApiGatewayClient, SqsClient.create(), sys.env)
  }

  override def handleInput(input: SQSEvent, context: Context): Unit = {
    if (input.getRecords.size != 1) throw new IllegalArgumentException(s"Invalid number of records: ${input.getRecords.size}")
    implicit val usagePlans: Map[String, String] = fromJson[Map[String, String]](environment(UsagePlansEnvironmentVariable))
    implicit val logger: LambdaLogger = context.getLogger

    val updateRequest: UpdateAPIKeyRequest = fromJson[UpdateAPIKeyRequest](input.getRecords.get(0).getBody)
    updateUsagePlan(updateRequest)
  }

  private def updateUsagePlan(updateRequest: UpdateAPIKeyRequest)(implicit logger: LambdaLogger, usagePlans: Map[String, String]): Unit = {
    val requestedUsagePlanId: String  = usagePlans(updateRequest.usagePlan.toUpperCase)

    def createNewApiKey(apiKeyName: String, apiKeyValue: String): String =
      apiGatewayClient.createApiKey(
        CreateApiKeyRequest.builder()
          .name(apiKeyName)
          .value(apiKeyValue)
          .build())
        .id()

    getAwsApiKeyIdByApplicationName(updateRequest.apiKeyName) match {
      case Some(apiKeyId) =>
        logger.log(s"Found existing API Key Id [$apiKeyId] for Application [${updateRequest.apiKeyName}]")
        moveAPIKeyToUsagePlan(apiKeyId, requestedUsagePlanId)
      case None =>
        val newAPIKeyId = createNewApiKey(updateRequest.apiKeyName, updateRequest.apiKeyValue)
        logger.log(s"Created API Key with Id [$newAPIKeyId] for Application [${updateRequest.apiKeyName}]")
        addAPIKeyToUsagePlan(newAPIKeyId, requestedUsagePlanId)
    }
  }

  private def addAPIKeyToUsagePlan(apiKeyId: String, usagePlanId: String) =
    apiGatewayClient.createUsagePlanKey(
      CreateUsagePlanKeyRequest.builder()
        .keyId(apiKeyId)
        .usagePlanId(usagePlanId)
        .keyType("API_KEY")
        .build())


  private def moveAPIKeyToUsagePlan(apiKeyId: String, requestedUsagePlanId: String)
                                   (implicit logger: LambdaLogger, usagePlans: Map[String, String]): Unit = {
    def removeAPIKeyFromUsagePlan(apiKeyId: String, usagePlanId: String) =
      apiGatewayClient.deleteUsagePlanKey(
        DeleteUsagePlanKeyRequest.builder()
          .usagePlanId(usagePlanId)
          .keyId(apiKeyId)
          .build())

    // Remove API Key from Usage Plans other than the one being requested
    usagePlans.values
      .filterNot(_ == requestedUsagePlanId)
      .filter(otherUsagePlanId => apiKeyAssociatedWithUsagePlan(apiKeyId, otherUsagePlanId))
      .foreach(usagePlanToRemoveKeyFrom => {
        logger.log(s"Removing API Key [$apiKeyId] from Usage Plan [$usagePlanToRemoveKeyFrom]")
        removeAPIKeyFromUsagePlan(apiKeyId, usagePlanToRemoveKeyFrom)
      })

    // Add API Key to requested Usage Plan if it is not already associated with it
    if(!apiKeyAssociatedWithUsagePlan(apiKeyId, requestedUsagePlanId)) {
      logger.log(s"Adding API Key [$apiKeyId] to Usage Plan [$requestedUsagePlanId]")
      addAPIKeyToUsagePlan(apiKeyId, requestedUsagePlanId)
    } else
      logger.log(s"API Key [$apiKeyId] is already associated with requested Usage Plan [$requestedUsagePlanId]")
  }

  @tailrec
  private def apiKeyAssociatedWithUsagePlan(apiKeyId: String, usagePlanId: String, position: Option[String] = None): Boolean = {
    def buildGetUsagePlanKeysRequest(position: Option[String]): GetUsagePlanKeysRequest = {
      position match {
        case Some(p) => GetUsagePlanKeysRequest.builder().usagePlanId(usagePlanId).limit(Limit).position(p).build()
        case None => GetUsagePlanKeysRequest.builder().usagePlanId(usagePlanId).limit(Limit).build()
      }
    }

    val response = apiGatewayClient.getUsagePlanKeys(buildGetUsagePlanKeysRequest(position))

    response.items().asScala.find(usagePlanKey => usagePlanKey.value() == apiKeyId) match {
      case Some(_) => true
      case _ => if (response.position == null) false else apiKeyAssociatedWithUsagePlan(apiKeyId, usagePlanId, Some(response.position))
    }
  }
}

case class UpdateAPIKeyRequest(usagePlan: String, apiKeyName: String, apiKeyValue: String)