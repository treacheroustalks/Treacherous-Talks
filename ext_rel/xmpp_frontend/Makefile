reldir = rel/xmpp_frontend
configs = config/ejabberd.cfg config/ejabberdctl.cfg config/inetrc
beams = components/echo_bot.beam components/tt_register_bot.beam
erls = components/echo_bot.erl components/tt_register_bot.erl
deps-ext = ../../apps/controller_app ../../apps/command_parser ../../apps/service ../../apps/datatypes
deps = components/controller_app components/command_parser components/service components/datatypes

all: release

release: ejabberd $(beams) $(configs)
	mkdir -p $(reldir)
	cp -r ejabberd $(reldir)
	cp -r config $(reldir)
	mkdir -p $(reldir)/components
	cp components/*/ebin/*.beam ebin
	rm -rf $(reldir)/ebin
	cp -r ebin $(reldir)/ebin
	cp install_Makefile $(reldir)/Makefile
	cp install_README $(reldir)/README
	touch $(reldir)

compile-beam: $(beams)
$(beams): $(deps) ejabberd-beams components/*.erl
	mkdir -p ebin
	erlc -I components -I ejabberd/src -pa ejabberd/src -o ebin $(erls)

ejabberd/src/Makefile: ejabberd
	cd ejabberd/src && ./configure

deps: $(deps)
$(deps):
	rebar get-deps
	rebar compile
	cp -r $(deps-ext) components

ejabberd-beams: ejabberd/src/Makefile
	cd ejabberd/src && make compile-beam

ejabberd/:
	# wget -nv -O ejabberd-2.1.9.tar.gz 'https://github.com/processone/ejabberd/tarball/v2.1.9'
	wget -nv 'http://buildbot.pcs/mirror/ejabberd-2.1.9.tar.gz'
	tar -xf ejabberd-2.1.9.tar.gz
	mv processone-ejabberd-4ca3c33 ejabberd

clean: clean-release
	rm -rf ejabberd ejabberd-2.1.9.tar.gz
	rm -rf ebin
	rm -rf components/datatypes
	rm -rf $(deps)


clean-release:
	rm -rf $(reldir)
