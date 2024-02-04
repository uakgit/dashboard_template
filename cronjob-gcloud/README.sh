# Install docker hub / install Google Cloud CLI
# Configure docker in docker hub / configure gcloud in SDK shell


# Build your Docker image locally in windows terminal opend in cronjob directory
docker build -t cronjob-ProjectABC .

# Run container (just for checking)
docker run -it --rm cronjob-ProjectABC

# Export the GOOGLE_APPLICATION_CREDENTIALS environment variable
# export GOOGLE_APPLICATION_CREDENTIALS="ProjectABC-a8fb07c9fac0.json"

# Authenticate Docker to GCP account (Must have "editor" rights)
# gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS

# Configure docker with Google Cloud in SDK shell
gcloud auth configure-docker


# Tag your Docker image in windows terminal
docker tag cronjob-ProjectABC gcr.io/ProjectABC/cronjob-ProjectABC

# Push your Docker image to GCR in windows terminal
docker push gcr.io/ProjectABC/cronjob-ProjectABC

# A cronjob can be set/edited at 
https://console.cloud.google.com/run/jobs/
