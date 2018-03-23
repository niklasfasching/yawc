.PHONY: autobahn
autobahn:
	docker run -it --rm \
		--volume ${PWD}/autobahn-server.json:/config/fuzzingserver.json \
		--volume ${PWD}/autobahn-reports:/reports \
		--publish 9001:9001 \
		--name fuzzingserver \
		--entrypoint="/usr/local/bin/wstest" \
		crossbario/autobahn-testsuite \
		--mode fuzzingserver \
		--spec /config/fuzzingserver.json
