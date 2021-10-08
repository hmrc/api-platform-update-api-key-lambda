package uk.gov.hmrc.apiplatform.upsertapikey

import com.amazonaws.services.lambda.runtime.events.SQSEvent
import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.apigateway.model.{CreateApiKeyRequest, CreateUsagePlanKeyRequest, CreateUsagePlanKeyResponse, DeleteUsagePlanKeyRequest, GetUsagePlanKeysRequest}
import uk.gov.hmrc.api_platform_manage_api.AwsApiGatewayClient.awsApiGatewayClient
import uk.gov.hmrc.api_platform_manage_api.AwsIdRetriever
import uk.gov.hmrc.aws_gateway_proxied_request_lambda.SqsHandler

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class UpsertApiKeyHandler(override val apiGatewayClient: ApiGatewayClient,
                          environment: Map[String, String]) extends SqsHandler with AwsIdRetriever {

  val UsagePlansEnvironmentVariable: String = "usage_plans"

  def this() {
    this(awsApiGatewayClient, sys.env)
  }

  override def handleInput(input: SQSEvent, context: Context): Unit = {
    if (input.getRecords.size != 1) throw new IllegalArgumentException(s"Invalid number of records: ${input.getRecords.size}")
    implicit val usagePlans: Map[String, Seq[String]] = fromJson[Map[String, Seq[String]]](environment(UsagePlansEnvironmentVariable))
    implicit val logger: LambdaLogger = context.getLogger

    val updateRequest: UpdateAPIKeyRequest = fromJson[UpdateAPIKeyRequest](input.getRecords.get(0).getBody)
    updateUsagePlan(updateRequest)
  }

  private def updateUsagePlan(updateRequest: UpdateAPIKeyRequest)(implicit logger: LambdaLogger, usagePlans: Map[String, Seq[String]]): Unit = {
    val requestedUsagePlanIds: Seq[String]  = usagePlans(updateRequest.usagePlan.toUpperCase)

    def createNewApiKey(apiKeyName: String, apiKeyValue: String): String =
      apiGatewayClient.createApiKey(
        CreateApiKeyRequest.builder()
          .name(apiKeyName)
          .value(apiKeyValue)
          .enabled(true)
          .build())
        .id()

    logger.log(s"Looking for API key for ${updateRequest.apiKeyName}")
    getAwsApiKeyByKeyName(updateRequest.apiKeyName) match {
      case Some(apiKey) =>
        logger.log(s"Found existing API Key Id [${apiKey.id}] for Application [${updateRequest.apiKeyName}]")
        moveAPIKeyToUsagePlans(apiKey.id, requestedUsagePlanIds)
      case None =>
        val newAPIKeyId = createNewApiKey(updateRequest.apiKeyName, updateRequest.apiKeyValue)
        logger.log(s"Created API Key with Id [$newAPIKeyId] for Application [${updateRequest.apiKeyName}]")
        addAPIKeyToUsagePlans(newAPIKeyId, requestedUsagePlanIds)
    }
  }

  private def addAPIKeyToUsagePlans(apiKeyId: String, requestedUsagePlanIds: Seq[String]): Unit = {
    requestedUsagePlanIds foreach { requestedUsagePlanId =>
      addAPIKeyToUsagePlan(apiKeyId, requestedUsagePlanId)
    }
  }

  private def addAPIKeyToUsagePlan(apiKeyId: String, requestedUsagePlanId: String): CreateUsagePlanKeyResponse = {
    apiGatewayClient.createUsagePlanKey(
      CreateUsagePlanKeyRequest.builder()
        .keyId(apiKeyId)
        .usagePlanId(requestedUsagePlanId)
        .keyType("API_KEY")
        .build())
  }

  private def moveAPIKeyToUsagePlans(apiKeyId: String, requestedUsagePlanIds: Seq[String])
                                   (implicit logger: LambdaLogger, usagePlans: Map[String, Seq[String]]): Unit = {
    def removeAPIKeyFromUsagePlan(apiKeyId: String, usagePlanId: String) = {
      apiGatewayClient.deleteUsagePlanKey(
        DeleteUsagePlanKeyRequest.builder()
          .usagePlanId(usagePlanId)
          .keyId(apiKeyId)
          .build())
    }

    // Remove API Key from Usage Plans other than the one being requested
    usagePlans.values.flatten
      .filterNot(requestedUsagePlanIds.contains(_))
      .filter(otherUsagePlanId => apiKeyAssociatedWithUsagePlan(apiKeyId, otherUsagePlanId))
      .foreach(usagePlanToRemoveKeyFrom => {
        logger.log(s"Removing API Key [$apiKeyId] from Usage Plan [$usagePlanToRemoveKeyFrom]")
        removeAPIKeyFromUsagePlan(apiKeyId, usagePlanToRemoveKeyFrom)
      })

    requestedUsagePlanIds foreach { requestedUsagePlanId =>
      // Add API Key to requested Usage Plan if it is not already associated with it
      if(!apiKeyAssociatedWithUsagePlan(apiKeyId, requestedUsagePlanId)) {
        logger.log(s"Adding API Key [$apiKeyId] to Usage Plan [$requestedUsagePlanId]")
        addAPIKeyToUsagePlan(apiKeyId, requestedUsagePlanId)
      } else {
        logger.log(s"API Key [$apiKeyId] is already associated with requested Usage Plan [$requestedUsagePlanId]")
      }
    }
  }

  private def buildGetUsagePlanKeysRequest(position: Option[String], usagePlanId: String): GetUsagePlanKeysRequest = {
    position match {
      case Some(p) => GetUsagePlanKeysRequest.builder().usagePlanId(usagePlanId).limit(Limit).position(p).build()
      case None => GetUsagePlanKeysRequest.builder().usagePlanId(usagePlanId).limit(Limit).build()
    }
  }

  @tailrec
  private def apiKeyAssociatedWithUsagePlan(apiKeyId: String, usagePlanId: String, position: Option[String] = None)
                                           (implicit logger: LambdaLogger): Boolean = {


    logger.log(s"Looking for usage plan keys for usage plan ${usagePlanId}")
    val getUsagePlan = buildGetUsagePlanKeysRequest(position, usagePlanId)
    val response = apiGatewayClient.getUsagePlanKeys(getUsagePlan)

    response.items().asScala.find(usagePlanKey => usagePlanKey.id == apiKeyId) match {
      case Some(_) => true
      case _ => if (response.position == null) false else apiKeyAssociatedWithUsagePlan(apiKeyId, usagePlanId, Some(response.position))
    }
  }
}

case class UpdateAPIKeyRequest(usagePlan: String, apiKeyName: String, apiKeyValue: String)
