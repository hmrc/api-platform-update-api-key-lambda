package uk.gov.hmrc.apiplatform.updateapikey

import java.util.UUID

import com.amazonaws.services.lambda.runtime.events.SQSEvent
import com.amazonaws.services.lambda.runtime.events.SQSEvent.SQSMessage
import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{times, verify, when}
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.{Answer, OngoingStubbing}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{Matchers, WordSpecLike}
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.apigateway.model._
import software.amazon.awssdk.services.sqs.SqsClient

import scala.collection.JavaConversions._

class UpsertApiKeyHandlerSpec extends WordSpecLike with Matchers with MockitoSugar {

  trait Setup {
    def mockedGetApiKeysCallReturns(existingAPIKeys: List[(String, String, String)]): OngoingStubbing[GetApiKeysResponse] =
      when(mockAPIGatewayClient.getApiKeys(any[GetApiKeysRequest]))
        .thenReturn(
          GetApiKeysResponse.builder()
            .items(
              existingAPIKeys.map((apiKey: (String, String, String)) => ApiKey.builder().id(apiKey._1).name(apiKey._2).value(apiKey._3).build()))
            .build())

    def mockedGetUsagePlanKeysReturns(bronzeUsagePlanKeys: Seq[String] = Seq.empty,
                                      silverUsagePlanKeys: Seq[String] = Seq.empty,
                                      goldUsagePlanKeys: Seq[String] = Seq.empty,
                                      platinumUsagePlanKeys: Seq[String] = Seq.empty): OngoingStubbing[GetUsagePlanKeysResponse] =
      when(mockAPIGatewayClient.getUsagePlanKeys(any[GetUsagePlanKeysRequest]))
        .thenAnswer(new GetUsagePlanKeysAnswer(bronzeUsagePlanKeys, silverUsagePlanKeys, goldUsagePlanKeys, platinumUsagePlanKeys))

    def captureCreateAPIKeyRequests(returnedAPIKeyId: String): ArgumentCaptor[CreateApiKeyRequest] = {
      val captor: ArgumentCaptor[CreateApiKeyRequest] = ArgumentCaptor.forClass(classOf[CreateApiKeyRequest])
      when(mockAPIGatewayClient.createApiKey(captor.capture())).thenReturn(CreateApiKeyResponse.builder().id(returnedAPIKeyId).build())

      captor
    }

    def captureCreateUsagePlanKeyRequests(): ArgumentCaptor[CreateUsagePlanKeyRequest] = {
      val captor: ArgumentCaptor[CreateUsagePlanKeyRequest] = ArgumentCaptor.forClass(classOf[CreateUsagePlanKeyRequest])
      when(mockAPIGatewayClient.createUsagePlanKey(captor.capture())).thenReturn(CreateUsagePlanKeyResponse.builder().build())

      captor
    }

    def captureDeleteUsagePlanKeyRequests(): ArgumentCaptor[DeleteUsagePlanKeyRequest] = {
      val captor: ArgumentCaptor[DeleteUsagePlanKeyRequest] = ArgumentCaptor.forClass(classOf[DeleteUsagePlanKeyRequest])
      when(mockAPIGatewayClient.deleteUsagePlanKey(captor.capture())).thenReturn(DeleteUsagePlanKeyResponse.builder().build())

      captor
    }

    class GetUsagePlanKeysAnswer(bronzeUsagePlanKeys: Seq[String],
                                 silverUsagePlanKeys: Seq[String],
                                 goldUsagePlanKeys: Seq[String],
                                 platinumUsagePlanKeys: Seq[String]) extends Answer[GetUsagePlanKeysResponse] {

      override def answer(invocationOnMock: InvocationOnMock): GetUsagePlanKeysResponse = {
        val request: GetUsagePlanKeysRequest = invocationOnMock.getArgument(0)

        def usagePlanKeys: Seq[UsagePlanKey] =
          if (request.usagePlanId() == usagePlans("BRONZE")) {
            bronzeUsagePlanKeys.map(key => UsagePlanKey.builder().value(key).build())
          } else if (request.usagePlanId() == usagePlans("SILVER")) {
            silverUsagePlanKeys.map(key => UsagePlanKey.builder().value(key).build())
          } else if (request.usagePlanId() == usagePlans("GOLD")) {
            goldUsagePlanKeys.map(key => UsagePlanKey.builder().value(key).build())
          } else if (request.usagePlanId() == usagePlans("PLATINUM")) {
            platinumUsagePlanKeys.map(key => UsagePlanKey.builder().value(key).build())
          } else {
            Seq.empty
          }

        GetUsagePlanKeysResponse.builder().items(usagePlanKeys).build()
      }
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

    def verifyCapturedCreateApiKeyRequest(captor: ArgumentCaptor[CreateApiKeyRequest], apiKeyName: String, apiKeyValue: String): Unit = {
      val capturedCreateApiKeyRequest: CreateApiKeyRequest = captor.getValue
      capturedCreateApiKeyRequest.name() shouldBe apiKeyName
      capturedCreateApiKeyRequest.value() shouldBe apiKeyValue
    }

    def verifyCapturedCreateUsagePlanKeyRequest(captor: ArgumentCaptor[CreateUsagePlanKeyRequest], apiKeyId: String, usagePlanId: String): Unit = {
      val capturedCreateUsagePlanKeyRequest: CreateUsagePlanKeyRequest = captor.getValue
      capturedCreateUsagePlanKeyRequest.keyId() shouldBe apiKeyId
      capturedCreateUsagePlanKeyRequest.usagePlanId() shouldBe usagePlanId
      capturedCreateUsagePlanKeyRequest.keyType() shouldBe "API_KEY"
    }

    def verifyCapturedDeleteUsagePlanKeyRequest(captor: ArgumentCaptor[DeleteUsagePlanKeyRequest], apiKeyId: String, usagePlanId: String): Unit = {
      val capturedDeleteUsagePlanKeyRequest = captor.getValue
      capturedDeleteUsagePlanKeyRequest.keyId() shouldBe apiKeyId
      capturedDeleteUsagePlanKeyRequest.usagePlanId() shouldBe usagePlanId
    }

    def validSQSEvent(usagePlan: String, apiKeyName: String, apiKeyValue: String): SQSEvent =
      buildSQSEvent(List(updateMessage(usagePlan, apiKeyName, apiKeyValue)))

    "create API Key if it does not exist and adds it to Usage Plan" in new Setup {
      val usagePlan: String = "BRONZE"
      val apiKeyName: String = UUID.randomUUID().toString
      val apiKeyValue: String = UUID.randomUUID().toString
      val generatedApiKeyId: String = UUID.randomUUID().toString

      mockedGetApiKeysCallReturns(List.empty)
      private val createApiKeyRequestCaptor: ArgumentCaptor[CreateApiKeyRequest] = captureCreateAPIKeyRequests(generatedApiKeyId)
      private val createUsagePlanKeyRequestCaptor: ArgumentCaptor[CreateUsagePlanKeyRequest] = captureCreateUsagePlanKeyRequests()

      upsertApiKeyHandler.handleInput(buildSQSEvent(Seq(updateMessage(usagePlan, apiKeyName, apiKeyValue))), mockContext)

      verifyCapturedCreateApiKeyRequest(createApiKeyRequestCaptor, apiKeyName, apiKeyValue)
      verifyCapturedCreateUsagePlanKeyRequest(createUsagePlanKeyRequestCaptor, generatedApiKeyId, usagePlans(usagePlan))
    }

    "does not update Usage Plan if API Key is already associated with it" in new Setup {
      val usagePlan: String = "BRONZE"
      val apiKeyId: String = UUID.randomUUID().toString
      val apiKeyName: String = UUID.randomUUID().toString
      val apiKeyValue: String = UUID.randomUUID().toString

      mockedGetApiKeysCallReturns(List((apiKeyId, apiKeyName, apiKeyValue)))
      mockedGetUsagePlanKeysReturns(bronzeUsagePlanKeys = Seq(apiKeyId))

      upsertApiKeyHandler.handleInput(validSQSEvent(usagePlan, apiKeyName, apiKeyValue), mockContext)

      verify(mockAPIGatewayClient, times(0)).createUsagePlanKey(any[CreateUsagePlanKeyRequest])
    }

    "moves API Key from one Usage Plan to another if requested" in new Setup {
      val usagePlan: String = "SILVER"
      val apiKeyId: String = UUID.randomUUID().toString
      val apiKeyName: String = UUID.randomUUID().toString
      val apiKeyValue: String = UUID.randomUUID().toString

      mockedGetApiKeysCallReturns(List((apiKeyId, apiKeyName, apiKeyValue)))
      mockedGetUsagePlanKeysReturns(bronzeUsagePlanKeys = Seq(apiKeyId))
      private val deleteUsagePlanKeyRequestCaptor: ArgumentCaptor[DeleteUsagePlanKeyRequest] = captureDeleteUsagePlanKeyRequests()
      private val createUsagePlanKeyRequestCaptor: ArgumentCaptor[CreateUsagePlanKeyRequest] = captureCreateUsagePlanKeyRequests()

      upsertApiKeyHandler.handleInput(validSQSEvent(usagePlan, apiKeyName, apiKeyValue), mockContext)

      verifyCapturedDeleteUsagePlanKeyRequest(deleteUsagePlanKeyRequestCaptor, apiKeyId, usagePlans("BRONZE"))
      verifyCapturedCreateUsagePlanKeyRequest(createUsagePlanKeyRequestCaptor, apiKeyId, usagePlans(usagePlan))
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
