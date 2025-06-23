package uk.gov.hmrc.apiplatform.upsertapikey

import java.util.UUID.randomUUID

import com.amazonaws.services.lambda.runtime.events.SQSEvent
import com.amazonaws.services.lambda.runtime.events.SQSEvent.SQSMessage
import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{times, verify, when}
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.{Answer, OngoingStubbing}
import software.amazon.awssdk.services.apigateway.ApiGatewayClient
import software.amazon.awssdk.services.apigateway.model._

import scala.jdk.CollectionConverters._
import uk.gov.hmrc.api_platform_manage_api.utils.JsonMapper
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class UpsertApiKeyHandlerSpec extends AnyWordSpec with Matchers with MockitoSugar with JsonMapper {

  trait Setup {
    case class TestApiKey(id: String, name: String, value: String)

    def toApiKey(testApiKey: TestApiKey): ApiKey = ApiKey.builder().id(testApiKey.id).name(testApiKey.name).value(testApiKey.value).build()
    def toUsagePlanKey(testApiKey: TestApiKey): UsagePlanKey = UsagePlanKey.builder().id(testApiKey.id).build()

    def mockedGetApiKeysCallReturns(existingAPIKeys: Seq[TestApiKey]): OngoingStubbing[GetApiKeysResponse] =
      when(mockAPIGatewayClient.getApiKeys(any[GetApiKeysRequest]))
        .thenReturn(
          GetApiKeysResponse.builder()
            .items(existingAPIKeys.map(toApiKey).asJava)
            .build())

    def mockedGetUsagePlanKeysReturns(bronzeUsagePlanKeys: Seq[TestApiKey] = Seq.empty,
                                      silverUsagePlanKeys: Seq[TestApiKey] = Seq.empty,
                                      goldUsagePlanKeys: Seq[TestApiKey] = Seq.empty,
                                      platinumUsagePlanKeys: Seq[TestApiKey] = Seq.empty): OngoingStubbing[GetUsagePlanKeysResponse] =
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

    class GetUsagePlanKeysAnswer(var bronzeUsagePlanKeys: Seq[TestApiKey],
                                 var silverUsagePlanKeys: Seq[TestApiKey],
                                 var goldUsagePlanKeys: Seq[TestApiKey],
                                 var platinumUsagePlanKeys: Seq[TestApiKey]) extends Answer[GetUsagePlanKeysResponse] {
      override def answer(invocationOnMock: InvocationOnMock): GetUsagePlanKeysResponse = {
        def usagePlanKeys(usagePlanId: String): Seq[UsagePlanKey] =
          if (usagePlans("BRONZE").contains(usagePlanId)) {
            bronzeUsagePlanKeys.map(toUsagePlanKey)
          } else if (usagePlans("SILVER").contains(usagePlanId)) {
            silverUsagePlanKeys.map(toUsagePlanKey)
          } else if (usagePlans("GOLD").contains(usagePlanId)) {
            goldUsagePlanKeys.map(toUsagePlanKey)
          } else if (usagePlans("PLATINUM").contains(usagePlanId)) {
            platinumUsagePlanKeys.map(toUsagePlanKey)
          } else {
            Seq.empty
          }

        val request: GetUsagePlanKeysRequest = invocationOnMock.getArgument(0)
        GetUsagePlanKeysResponse.builder().items(usagePlanKeys(request.usagePlanId()).asJava).build()
      }
    }

    class PagedGetUsagePlanKeysAnswer(matchingAPIKey: TestApiKey, usagePlanIds: Seq[String], numberOfPages: Int) extends Answer[GetUsagePlanKeysResponse] {
      override def answer(invocationOnMock: InvocationOnMock): GetUsagePlanKeysResponse = {
        def randomUsagePlanKey: UsagePlanKey = toUsagePlanKey(TestApiKey(randomUUID().toString, randomUUID().toString, randomUUID().toString))

        def nextPosition(position: String): String = {
          position match {
            case null => "2"
            case pos => (pos.toInt + 1).toString
          }
        }

        val request: GetUsagePlanKeysRequest = invocationOnMock.getArgument(0)
        if (!usagePlanIds.contains(request.usagePlanId))
          GetUsagePlanKeysResponse.builder().build()
        else {
          if (request.position() == numberOfPages.toString)
            GetUsagePlanKeysResponse.builder().items(toUsagePlanKey(matchingAPIKey)).build()
          else
            GetUsagePlanKeysResponse.builder().position(nextPosition(request.position())).items(randomUsagePlanKey).build()
        }

      }
    }

    val mockAPIGatewayClient: ApiGatewayClient = mock[ApiGatewayClient]

    val usagePlans: Map[String, Seq[String]] =
      Map(
        "BRONZE" -> Seq(randomUUID().toString, randomUUID().toString),
        "SILVER" -> Seq(randomUUID().toString, randomUUID().toString),
        "GOLD" -> Seq(randomUUID().toString, randomUUID().toString),
        "PLATINUM" -> Seq(randomUUID().toString, randomUUID().toString)
      )

    val usagePlansJson: String = toJson(usagePlans)
    val environment: Map[String, String] = Map("usage_plans" -> usagePlansJson)

    val upsertApiKeyHandler = new UpsertApiKeyHandler(mockAPIGatewayClient, environment)
  }

  "Update API Key Handler" should {
    val mockContext: Context = mock[Context]
    when(mockContext.getLogger).thenReturn(mock[LambdaLogger])

    def buildSQSEvent(messages: Seq[SQSMessage]): SQSEvent = {
      val sqsEvent = new SQSEvent()
      sqsEvent.setRecords(messages.asJava)

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
      capturedCreateApiKeyRequest.enabled() shouldBe true
    }

    def verifyCapturedCreateUsagePlanKeyRequest(captor: ArgumentCaptor[CreateUsagePlanKeyRequest], apiKeyId: String, usagePlanIds: Seq[String]): Unit = {
      val capturedCreateUsagePlanKeyRequests: Seq[CreateUsagePlanKeyRequest] = captor.getAllValues.asScala.toSeq
      capturedCreateUsagePlanKeyRequests.map(_.keyId) should contain only apiKeyId
      capturedCreateUsagePlanKeyRequests.map(_.usagePlanId) should contain theSameElementsAs usagePlanIds
      capturedCreateUsagePlanKeyRequests.map(_.keyType) should contain only "API_KEY"
    }

    def verifyCapturedDeleteUsagePlanKeyRequest(captor: ArgumentCaptor[DeleteUsagePlanKeyRequest], apiKeyId: String, usagePlanIds: Seq[String]): Unit = {
      val capturedDeleteUsagePlanKeyRequests = captor.getAllValues.asScala.toSeq
      capturedDeleteUsagePlanKeyRequests.map(_.keyId) should contain only apiKeyId
      capturedDeleteUsagePlanKeyRequests.map(_.usagePlanId) should contain theSameElementsAs usagePlanIds
    }

    def validSQSEvent(usagePlan: String, apiKeyName: String, apiKeyValue: String): SQSEvent =
      buildSQSEvent(List(updateMessage(usagePlan, apiKeyName, apiKeyValue)))

    "create API Key if it does not exist and adds it to Usage Plans" in new Setup {
      val usagePlan: String = "BRONZE"
      val apiKeyName: String = randomUUID().toString
      val apiKeyValue: String = randomUUID().toString
      val generatedApiKeyId: String = randomUUID().toString

      mockedGetApiKeysCallReturns(List.empty)
      private val createApiKeyRequestCaptor: ArgumentCaptor[CreateApiKeyRequest] = captureCreateAPIKeyRequests(generatedApiKeyId)
      private val createUsagePlanKeyRequestCaptor: ArgumentCaptor[CreateUsagePlanKeyRequest] = captureCreateUsagePlanKeyRequests()

      upsertApiKeyHandler.handleInput(buildSQSEvent(Seq(updateMessage(usagePlan, apiKeyName, apiKeyValue))), mockContext)

      verifyCapturedCreateApiKeyRequest(createApiKeyRequestCaptor, apiKeyName, apiKeyValue)
      verifyCapturedCreateUsagePlanKeyRequest(createUsagePlanKeyRequestCaptor, generatedApiKeyId, usagePlans(usagePlan))
    }

    "does not update Usage Plans if API Key is already associated with them" in new Setup {
      val usagePlan: String = "BRONZE"
      val apiKeyId: String = randomUUID().toString
      val apiKeyName: String = randomUUID().toString
      val apiKeyValue: String = randomUUID().toString
      val matchingApiKey: TestApiKey = TestApiKey(apiKeyId, apiKeyName, apiKeyValue)

      mockedGetApiKeysCallReturns(List(matchingApiKey))
      mockedGetUsagePlanKeysReturns(bronzeUsagePlanKeys = Seq(matchingApiKey))

      upsertApiKeyHandler.handleInput(validSQSEvent(usagePlan, apiKeyName, apiKeyValue), mockContext)

      verify(mockAPIGatewayClient, times(0)).createUsagePlanKey(any[CreateUsagePlanKeyRequest])
    }

    "moves API Key from one Usage Plan to another if requested" in new Setup {
      val usagePlan: String = "SILVER"
      val apiKeyId: String = randomUUID().toString
      val apiKeyName: String = randomUUID().toString
      val apiKeyValue: String = randomUUID().toString
      val matchingApiKey: TestApiKey = TestApiKey(apiKeyId, apiKeyName, apiKeyValue)

      mockedGetApiKeysCallReturns(List(matchingApiKey))
      mockedGetUsagePlanKeysReturns(bronzeUsagePlanKeys = Seq(matchingApiKey))
      private val deleteUsagePlanKeyRequestCaptor: ArgumentCaptor[DeleteUsagePlanKeyRequest] = captureDeleteUsagePlanKeyRequests()
      private val createUsagePlanKeyRequestCaptor: ArgumentCaptor[CreateUsagePlanKeyRequest] = captureCreateUsagePlanKeyRequests()

      upsertApiKeyHandler.handleInput(validSQSEvent(usagePlan, apiKeyName, apiKeyValue), mockContext)

      verifyCapturedDeleteUsagePlanKeyRequest(deleteUsagePlanKeyRequestCaptor, apiKeyId, usagePlans("BRONZE"))
      verifyCapturedCreateUsagePlanKeyRequest(createUsagePlanKeyRequestCaptor, apiKeyId, usagePlans(usagePlan))
    }

    "handle multiple pages of Usage Plan Keys returned" in new Setup {
      val usagePlan: String = "BRONZE"
      val apiKeyId: String = randomUUID().toString
      val apiKeyName: String = randomUUID().toString
      val apiKeyValue: String = randomUUID().toString
      val matchingApiKey: TestApiKey = TestApiKey(apiKeyId, apiKeyName, apiKeyValue)

      mockedGetApiKeysCallReturns(List(matchingApiKey))
      when(mockAPIGatewayClient.getUsagePlanKeys(any[GetUsagePlanKeysRequest]))
        .thenAnswer(new PagedGetUsagePlanKeysAnswer(matchingApiKey, usagePlans(usagePlan), 5))

      upsertApiKeyHandler.handleInput(validSQSEvent(usagePlan, apiKeyName, apiKeyValue), mockContext)

      verify(mockAPIGatewayClient, times(0)).createUsagePlanKey(any[CreateUsagePlanKeyRequest])
    }

    "handle lower case Usage Plan name" in new Setup {
      val usagePlan: String = "bronze"
      val apiKeyId: String = randomUUID().toString
      val apiKeyName: String = randomUUID().toString
      val apiKeyValue: String = randomUUID().toString
      val matchingApiKey: TestApiKey = TestApiKey(apiKeyId, apiKeyName, apiKeyValue)

      mockedGetApiKeysCallReturns(List(matchingApiKey))
      mockedGetUsagePlanKeysReturns(bronzeUsagePlanKeys = Seq(matchingApiKey))

      upsertApiKeyHandler.handleInput(validSQSEvent(usagePlan, apiKeyName, apiKeyValue), mockContext)

      verify(mockAPIGatewayClient, times(0)).createUsagePlanKey(any[CreateUsagePlanKeyRequest])
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
      val upsertApiKeyHandler = new UpsertApiKeyHandler(mock[ApiGatewayClient], Map())

      val exception =
        intercept[NoSuchElementException](upsertApiKeyHandler.handleInput(buildSQSEvent(List(updateMessage("BRONZE", "abc", "123"))), mockContext))

      exception.getMessage shouldEqual "key not found: usage_plans"
    }

    "throw exception if requested Usage Plan does not exist" in new Setup {
      val invalidUsagePlan = "foobar"
      val exception: NoSuchElementException =
        intercept[NoSuchElementException](upsertApiKeyHandler.handleInput(validSQSEvent(invalidUsagePlan, randomUUID().toString, randomUUID().toString), mockContext))

      exception.getMessage shouldEqual s"key not found: ${invalidUsagePlan.toUpperCase}"
    }
  }
}
