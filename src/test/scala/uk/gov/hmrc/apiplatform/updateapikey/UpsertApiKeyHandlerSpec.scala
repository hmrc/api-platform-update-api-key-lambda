package uk.gov.hmrc.apiplatform.updateapikey

import com.amazonaws.services.lambda.runtime.events.SQSEvent
import com.amazonaws.services.lambda.runtime.events.SQSEvent.SQSMessage
import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{Matchers, WordSpecLike}
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.apigateway.model.EndpointType.REGIONAL
import software.amazon.awssdk.services.apigateway.model._
import software.amazon.awssdk.services.sqs.SqsClient

import scala.collection.JavaConversions._

class UpsertApiKeyHandlerSpec extends WordSpecLike with Matchers with MockitoSugar {

  trait Setup {
    val mockAPIGatewayClient: ApiGatewayClient = mock[ApiGatewayClient]
    val mockSqsClient: SqsClient = mock[SqsClient]


    val mockContext: Context = mock[Context]
    when(mockContext.getLogger).thenReturn(mock[LambdaLogger])
    when(mockAPIGatewayClient.getRestApi(any[GetRestApiRequest]))
      .thenReturn(GetRestApiResponse.builder().endpointConfiguration(EndpointConfiguration.builder().types(REGIONAL).build()).build())
//    when(mockAPIGatewayClient.getRestApis(any[GetRestApisRequest])).thenReturn(buildMatchingRestApisResponse(apiId, apiName))
    when(mockAPIGatewayClient.getUsagePlan(any[GetUsagePlanRequest])).thenReturn(GetUsagePlanResponse.builder().build())

    val environment: Map[String, String] = Map()

    val upsertApiKeyHandler = new UpsertApiKeyHandler(mockAPIGatewayClient, mockSqsClient, environment)
  }

  "Update API Key Handler" should {
    def buildSQSEvent(messages: Seq[SQSMessage]): SQSEvent = {
      val sqsEvent = new SQSEvent()
      sqsEvent.setRecords(messages)

      sqsEvent
    }

    def updateMessage(): SQSMessage = new SQSMessage()

    "throw exception if the event has no messages" in new Setup {
      val sqsEvent: SQSEvent = buildSQSEvent(List())

      val exception: IllegalArgumentException = intercept[IllegalArgumentException](upsertApiKeyHandler.handleInput(sqsEvent, mockContext))
      exception.getMessage shouldEqual "Invalid number of records: 0"
    }

    "throw exception if the event has multiple messages" in new Setup {
      val sqsEvent: SQSEvent = buildSQSEvent(List(updateMessage(), updateMessage()))

      val exception: IllegalArgumentException = intercept[IllegalArgumentException](upsertApiKeyHandler.handleInput(sqsEvent, mockContext))
      exception.getMessage shouldEqual "Invalid number of records: 2"
    }
  }
}
