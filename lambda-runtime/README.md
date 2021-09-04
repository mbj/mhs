# lambda-runtime

An opinionated runtime environment for haskell applications running on [AWS Lambda].

Key Features:

- Only exposes a single function that constructs a lambda
- Strong emphasis on error catching and handling, exposing the ability to catch `InternalLambdaClientErrors` in user applications for post processing and reporting.
- Aim for small dependency footprint
- Expose a CLI `run` function which can be used to test lambda locally.

## Local Testing

See example executable on how to do local testing using a CLI.
