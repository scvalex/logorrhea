all: init dev

.PHONY: statics_fetch statics_fetch_force statics_clean clean init dev

staticfetcher.py:
	wget https://raw.github.com/nh2/staticfetcher/master/staticfetcher.py

statics_fetch: staticfetcher.py
	python statics.py fetch

statics_fetch_force: staticfetcher.py
	python statics.py fetch --force

statics_clean: staticfetcher.py
	python statics.py clean

clean: statics_clean
	rm -f staticfetcher.py

init:
	[ -f .statics_fetched ] && make statics_fetch_force || true
	touch .statics_fetched

dev: statics_fetch
	./dev.sh
