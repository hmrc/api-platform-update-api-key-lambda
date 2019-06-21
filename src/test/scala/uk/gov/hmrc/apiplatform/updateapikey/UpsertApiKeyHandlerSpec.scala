package uk.gov.hmrc.apiplatform.updateapikey

import java.util.UUID

import com.amazonaws.services.lambda.runtime.events.SQSEvent
import com.amazonaws.services.lambda.runtime.events.SQSEvent.SQSMessage
import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.mockito.stubbing.OngoingStubbing
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{Matchers, WordSpecLike}
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.apigateway.model._
import software.amazon.awssdk.services.sqs.SqsClient

import scala.collection.JavaConversions._

class UpsertApiKeyHandlerSpec extends WordSpecLike with Matchers with MockitoSugar {

  trait Setup {
    def getApiKeysCallReturns(existingAPIKeys: List[(String, String, String)]): OngoingStubbing[GetApiKeysResponse] = {
      when(mockAPIGatewayClient.getApiKeys(any[GetApiKeysRequest]))
        .thenReturn(
          GetApiKeysResponse.builder()
            .items(
              existingAPIKeys.map((apiKey: (String, String, String)) => ApiKey.builder().id(apiKey._1).name(apiKey._2).value(apiKey._3).build()))
            .build())
    }

    val mockAPIGatewayClient: ApiGatewayClient = mock[ApiGatewayClient]
    val mockSqsClient: SqsClient = mock[SqsClient]

    val usagePlans: Map[String, String] =
      Map(
        "BRONZE" -> UUID.randomUUID().toString,
        "SILVER" -> UUID.randomUUID().toString,
        "GOLD" -> UUID.randomUUID().toString,
        "PLATINUM" -> UUID.randomUUID().toString)

    val environment: Map[String, String] =
      Map("usage_plans" -> usagePlans.map(plan => s""" "${plan._1}" : "${plan._2}" """).mkString("{", ",", "}"))

    val upsertApiKeyHandler = new UpsertApiKeyHandler(mockAPIGatewayClient, mockSqsClient, environment)
  }

  "Update API Key Handler" should {
    val mockContext: Context = mock[Context]
    when(mockContext.getLogger).thenReturn(mock[LambdaLogger])

    def buildSQSEvent(messages: Seq[SQSMessage]): SQSEvent = {
      val sqsEvent = new SQSEvent()
      sqsEvent.setRecords(messages)

      sqsEvent
    }

    def updateMessage(usagePlan: String, apiKeyName: String, apiKeyValue: String): SQSMessage = {
      val messageBody = s"""{"usagePlan": "$usagePlan", "apiKeyName": "$apiKeyName", "apiKeyValue": "$apiKeyValue" }"""
      val message = new SQSMessage()
      message.setBody(messageBody)

      message
    }

    def validSQSEvent(usagePlan: String, apiKeyName: String, apiKeyValue: String): SQSEvent =
      buildSQSEvent(List(updateMessage(usagePlan, apiKeyName, apiKeyValue)))

    "create API Key if it does not exist and adds it to Usage Plan" in new Setup {
      val usagePlan: String = "BRONZE"
      val apiKeyName: String = UUID.randomUUID().toString
      val apiKeyValue: String = UUID.randomUUID().toString
      val generatedApiKeyId: String = UUID.randomUUID().toString

      getApiKeysCallReturns(List.empty)

      val createApiKeyRequestCaptor: ArgumentCaptor[CreateApiKeyRequest] = ArgumentCaptor.forClass(classOf[CreateApiKeyRequest])
      when(mockAPIGatewayClient.createApiKey(createApiKeyRequestCaptor.capture())).thenReturn(CreateApiKeyResponse.builder().id(generatedApiKeyId).build())

      val createUsagePlanKeyRequestCaptor: ArgumentCaptor[CreateUsagePlanKeyRequest] = ArgumentCaptor.forClass(classOf[CreateUsagePlanKeyRequest])
      when(mockAPIGatewayClient.createUsagePlanKey(createUsagePlanKeyRequestCaptor.capture())).thenReturn(CreateUsagePlanKeyResponse.builder().build())

      upsertApiKeyHandler.handleInput(buildSQSEvent(Seq(updateMessage(usagePlan, apiKeyName, apiKeyValue))), mockContext)

      val capturedCreateApiKeyRequest: CreateApiKeyRequest = createApiKeyRequestCaptor.getValue
      capturedCreateApiKeyRequest.name() shouldBe apiKeyName
      capturedCreateApiKeyRequest.value() shouldBe apiKeyValue

      val capturedUsagePlanKeyRequest: CreateUsagePlanKeyRequest = createUsagePlanKeyRequestCaptor.getValue
      capturedUsagePlanKeyRequest.keyId() shouldBe generatedApiKeyId
      capturedUsagePlanKeyRequest.usagePlanId() shouldBe usagePlans(usagePlan)
      capturedUsagePlanKeyRequest.keyType() shouldBe "API_KEY"
    }

    "does not update Usage Plan if API Key is already associated with it" in new Setup {
      val usagePlan: String = "BRONZE"
      val apiKeyId: String = UUID.randomUUID().toString
      val apiKeyName: String = UUID.randomUUID().toString
      val apiKeyValue: String = UUID.randomUUID().toString

      getApiKeysCallReturns(List((apiKeyId, apiKeyName, apiKeyValue)))

      upsertApiKeyHandler.handleInput(validSQSEvent(usagePlan, apiKeyName, apiKeyValue), mockContext)


    }

    "moves API Key from one Usage Plan to another if requested" in new Setup {

    }

    "throw exception if the event has no messages" in new Setup {
      val sqsEvent: SQSEvent = buildSQSEvent(List())

      val exception: IllegalArgumentException = intercept[IllegalArgumentException](upsertApiKeyHandler.handleInput(sqsEvent, mockContext))
      exception.getMessage shouldEqual "Invalid number of records: 0"
    }

    "throw exception if the event has multiple messages" in new Setup {
      val sqsEvent: SQSEvent =
        buildSQSEvent(
          List(
            updateMessage("BRONZE", "abc", "123"),
            updateMessage("SILVER", "def", "456")))

      val exception: IllegalArgumentException = intercept[IllegalArgumentException](upsertApiKeyHandler.handleInput(sqsEvent, mockContext))
      exception.getMessage shouldEqual "Invalid number of records: 2"
    }

    "throw exception if Usage Plans are not provided in an Environment Variable" in {
      val upsertApiKeyHandler = new UpsertApiKeyHandler(mock[ApiGatewayClient], mock[SqsClient], Map())

      val exception =
        intercept[RuntimeException](upsertApiKeyHandler.handleInput(buildSQSEvent(List(updateMessage("BRONZE", "abc", "123"))), mockContext))

      exception.getMessage shouldEqual "No Usage Plans found under Environment Variable [usage_plans]"
    }

    "throw exception if requested Usage Plan does not exist" in new Setup {
      val invalidUsagePlan = "foobar"
      val exception: IllegalArgumentException =
        intercept[IllegalArgumentException](upsertApiKeyHandler.handleInput(validSQSEvent(invalidUsagePlan, UUID.randomUUID().toString, UUID.randomUUID().toString), mockContext))

      exception.getMessage shouldEqual s"Requested Usage Plan [$invalidUsagePlan] does not exist"
    }
  }
}
