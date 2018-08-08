
GITCOMMIT := $(shell git rev-parse HEAD 2>/dev/null)
SERVICE_NAME := hearts

.PHONY: run docker-run build-image release-image build-with-stack-docker
run:
	stack exec -- yesod devel

docker-run:
	docker-compose run hearts

build-image:
	echo $(GITCOMMIT) > REVISION
	docker build -t docker.hearts.io/${SERVICE_NAME}:$(GITCOMMIT) .
	#docker build --pull -t docker.hearts.io/${SERVICE_NAME}:$(GITCOMMIT) .

release-image: build-with-stack-docker
	docker push rbutler10/hearts
	docker push docker.hearts.io/${SERVICE_NAME}:$(GITCOMMIT)

build-with-stack-docker:
	stack image container
