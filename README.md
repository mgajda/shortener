# URL shortener

To start server at 8080: `stack run`.

API endpoints:

* `GET /` -- gives HTML form for testing /shorten endpoint using browser
* `POST /shorten` -- shortens an url given as form field `url` **or** request body with `plain/text` content type with default encoding (ASCII) or `charset=utf-8`
* `GET /...` -- shortened URLs redirection

# Deployment

Using latest AWS CLI and AWS AppRunner.

1. Need following variables:

    * AWS `USERID` -- this number is attached to each AWS account
    * AWS `REGION` -- very few regions currently support AppRunner, including `eu-west-1` and `us-east-1`
    * AWS ECR repository name -- you may pick your own, yet unused in your account

2. Create AWS ECR repository within ${REGION}:

```
aws ecr create-repository --name ${ECR_REPOSITORY_NAME} --region ${REGION}
```

3. Create AppRunner service within ${REGION} with private ECR ImageRepository source.
   (This is the best documented, running from AWS CLI still seems to want JSON template.)
   I also suggest to automatically redeploy service on push.
   All other options may be left at their default values.

4. Logging to AWS ECR repo with AWS access key:

   ```sh
   aws ecr get-login-password --region ${REGION} | docker login --username AWS --password-stdin ${USERID}.dkr.ecr.${REGION}.amazonaws.com/apprunner
   ```

5. Build and push latest image (which should automatically deploy it as indicated in 3):

   ```sh
   docker build . -f Dockerfile -t my-demo
   docker tag my-demo:latest ${USERID}.dkr.ecr.${REGION}.amazonaws.com/apprunner
   docker push ${USERID}.dkr.ecr.${REGION}.amazonaws.com/apprunner
   ```
