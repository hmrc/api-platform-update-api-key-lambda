
# api-platform-update-api-key-lambda

Lambda function which:
* creates an AWS API Gateway key (if it doesn't exist)
* puts the AWS API Gateway key in the appropriate AWS API Gateway Usage Plan

The `event` for the Lambda function is an SQS message. The body of the SQS message is JSON. For example:
```
{
  "apiKeyName":"abcdef1234",
  "apiKeyValue":"acbdef1234567890abcdef1234567890",
  "usagePlan":"BRONZE"
}
```

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
