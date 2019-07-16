REPO = owend/vanilla
TAG ?= 0.1.0
IMAGE = $(REPO):$(TAG)


.PHONY: curl
curl:
	@curl -s -XPOST localhost:8080/equivalence -H 'Content-Type: application/json' \
	-d "$$(cat ./curlable/reqFields.json)" | jq '.'

.PHONY: curl-dps
curl-dps:
	@curl -s -XPOST localhost:8080/dps -H 'Content-Type: application/json' \
	-d "$$(cat ./curlable/reqFields.json)" | jq '.'

.PHONY: swagger
swagger:
	stack build api:api-swagger && stack exec -- api-swagger

.PHONY: build-docker
build-docker:
	docker build -t $(IMAGE) .

.PHONY: deploy-docker
deploy-docker: build-docker
	docker push $(IMAGE)
