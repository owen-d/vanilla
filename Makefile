
.PHONY: curl
curl:
	@curl -s -XPOST localhost:8080/equivalence -H 'Content-Type: application/json' \
	-d "$$(cat ./curlable/reqFields.json)" | jq '.'

.PHONY: curl-dps
curl-dps:
	@curl -s -XPOST localhost:8080/dps -H 'Content-Type: application/json' \
	-d "$$(cat ./curlable/reqFields.json)" | jq '.'
