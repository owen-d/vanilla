
.PHONY: curl
curl:
	@curl -s -XPOST localhost:8080/equivalence -H 'Content-Type: application/json' \
	-d "$$(cat ./curlable/reqFields.json)" | jq '.'
