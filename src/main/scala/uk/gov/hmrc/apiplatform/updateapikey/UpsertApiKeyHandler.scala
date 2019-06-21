package uk.gov.hmrc.apiplatform.updateapikey

import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import com.amazonaws.services.lambda.runtime.events.SQSEvent
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.sqs.SqsClient
import uk.gov.hmrc.api_platform_manage_api.AwsApiGatewayClient.awsApiGatewayClient
import uk.gov.hmrc.api_platform_manage_api.AwsIdRetriever
import uk.gov.hmrc.aws_gateway_proxied_request_lambda.SqsHandler

class UpsertApiKeyHandler(override val apiGatewayClient: ApiGatewayClient,
                          sqsClient: SqsClient,
                          environment: Map[String, String]) extends SqsHandler with AwsIdRetriever {

  def this() {
    this(awsApiGatewayClient, SqsClient.create(), sys.env)
  }

  override def handleInput(input: SQSEvent, context: Context): Unit = {
    implicit val logger: LambdaLogger = context.getLogger
    if (input.getRecords.size != 1) {
      throw new IllegalArgumentException(s"Invalid number of records: ${input.getRecords.size}")
    }


  }
}