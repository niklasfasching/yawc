.PHONY: test
test:
	lein test

.PHONY: integration-test
integration-test:
	docker-compose up --exit-code-from headless
