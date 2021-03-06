# Top-level Makefile for Treacherous Talks

# Path to rebar
REBAR=./rebar

# Path to system release folder where releases are put
SYSREL=system-release

# URL to Riak tar.gz download
RIAK_URL=http://downloads.basho.com/riak/riak-1.0.2/riak-1.0.2.tar.gz

# Internal variables used for naming release tar
DATE=`date +%Y%m%d-%H%M`
COMMIT=$(shell git diff-index --quiet HEAD; \
               if [ $$? -ne 0 ]; then echo dirty; \
               else git rev-list --max-count=1 HEAD; fi)


standard: small_clean get_deps compile docs
	@echo ok

complete: standard test release tar_release
	@echo ok

plt:
	@if [ ! -f ~/.dialyzer_plt ]; then                            \
	  echo "################################################";    \
	  echo "WARNING: ~/.dialyzer_plt was not found";              \
	  echo "I am building it for you."                            \
	  echo "This will take a while (this time).";                 \
	  sleep 3;                                                    \
	  echo "You might as well make some coffee..";                \
	  sleep 2;                                                    \
	  time dialyzer --build_plt --apps erts kernel stdlib mnesia; \
	fi
	 # delete the .eunit dirs, they confuse dialyzer:
	find apps -name .eunit -type d -exec rm -rf '{}' \;
	$(REBAR) compile
	@echo "######################"
	@echo "building apps/apps.plt"
	dialyzer -r apps --build_plt --output_plt apps/apps.plt || echo ""

dia:
	dialyzer --plts apps/apps.plt ~/.dialyzer_plt -- -r apps

### Build rules

get_deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
	$(REBAR) apps=cluster_manager,fault_tolerance escriptize

docs:
	$(REBAR) doc skip_deps=true

small_clean:
	$(REBAR) clean skip_deps=true

clean:
	$(REBAR) clean


### Test rules

test: release
	cd ext_test; bash test -i -u

unittest: release
	cd ext_test; bash test -u

inttest: release
	cd ext_test; bash test -i


### Release rules

# Do a release and also copy the cluster_manager escript
release: clean_release copy_docs
	$(REBAR) generate

# Remove all things in system-release except for Riak (if it's there) since we
# don't build it with our tools. To make rmdir work even when $SYSREL doesn't
# exist, we create it first... And then we try to remove it if it is empty.
clean_release:
	mkdir -p $(SYSREL)
	find $(SYSREL)/* -depth -maxdepth 0 ! -iname riak -print0 | xargs -0 rm -rf
	rmdir --ignore-fail-on-non-empty $(SYSREL)

# This rule copies docs to SYSREL/docs so that they are easier to collect for
# Buildbot
copy_docs:
	rm -rf $(SYSREL)/docs
	mkdir -p $(SYSREL)/docs
	for dir in apps/*/doc/edoc-info; \
	do path=$$(dirname $$dir); name=$$(basename $$(dirname $$path)); \
	cp -r $$path $(SYSREL)/docs/$$name; done;

# Create a tar.gz file of all releases in SYSREL
tar_release:
	rm -f $(SYSREL)/release-*.tar.gz
	tar -czf $(SYSREL)/release-$(DATE)-$(COMMIT).tar.gz system-release/*

# Create a stupid deb package of all releases in SYSREL using the tool fpm. It
# can be installed via a gem, run "gem install fpm". Beware that you might need
# to add the gem bin directory to your $PATH, such as /var/lib/gems/1.8/bin.
deb_release:
	rm -f $(SYSREL)/*.deb
	cd $(SYSREL); fpm -s dir -t deb -n treacherous-talks --prefix /opt/tt \
	-v $(DATE)-$(COMMIT) *


### Helper rules for internal development

create_deps_file: clean clean_release get_deps riak_release
	tar -czf dependencies.tar.gz deps/ $(SYSREL)/

fetch_deps_file:
	wget -nv 'http://buildbot.pcs/mirror/dependencies.tar.gz' 2>&1
	tar -xf dependencies.tar.gz
	rm dependencies.tar.gz

# This is an ugly way to get Riak into our debian packages and development
# environment. It simply fetches Riak, builds it and creates a release. That
# release is then moved into $SYSREL.
riak_release:
	rm -rf riak-build
	mkdir -p riak-build
	cd riak-build; wget -nv '$(RIAK_URL)' 2>&1
	cd riak-build; tar -xf riak-*.tar.gz
	cd riak-build; rm riak-*.tar.gz
	cd riak-build/riak-*; make rel
	rm -rf $(SYSREL)/riak
	mkdir -p $(SYSREL)/riak
	mv riak-build/riak-*/rel/riak $(SYSREL)
	rm -rf riak-build

# Start the full stack
start:
	bash ext_test/common_functions.sh start_system "configs/fullstack.config"

# Stop the full stack
stop:
	bash ext_test/common_functions.sh stop_system "configs/fullstack.config"


.PHONY: standard complete get_deps compile docs small_clean clean test \
	unittest inttest release clean_release copy_docs tar_release \
	deb_release create_deps_file fetch_deps_file plt dia riak_release \
	start stop
