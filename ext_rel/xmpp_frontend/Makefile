reldir = rel/xmpp_frontend
configs = config/ejabberd.cfg config/ejabberdctl.cfg config/inetrc
beams = components/echo_bot.beam components/tt_register_bot.beam components/user_command.beam components/bin_utils.beam
deps = components/controller.beam components/user_command.hrl components/service_worker.beam

all: release

release: ejabberd $(beams) $(configs)
	mkdir -p $(reldir)
	cp -r ejabberd $(reldir)
	cp -r config $(reldir)
	mkdir -p $(reldir)/components
	cp $(beams) $(deps) $(reldir)/components
	cp install_Makefile $(reldir)/Makefile
	cp install_README $(reldir)/README
	touch $(reldir)

compile-beam: $(beams)
$(beams): $(deps) ejabberd-beams components/echo_bot.erl components/tt_register_bot.erl components/user_command.erl components/bin_utils.erl components/datatypes
	cd components && erlc user_command.erl bin_utils.erl
	cd components && erlc -I ../ejabberd/src -pa ../ejabberd/src echo_bot.erl tt_register_bot.erl

ejabberd/src/Makefile: ejabberd
	cd ejabberd/src && ./configure

#### deps
#
components/controller.beam: ../../apps/controller_app/ebin/controller.beam
	cp ../../apps/controller_app/ebin/controller.beam components
components/service_worker.beam: ../../apps/service/ebin/service_worker.beam
	cp ../../apps/service/ebin/service_worker.beam components

../../apps/controller_app/ebin/controller.beam:
	cd ../../apps/controller_app/ && ./build
../../apps/service/ebin/service_worker.beam:
	cd ../../apps/service/  && ./build

components/datatypes: ../../apps/datatypes
	cp -r ../../apps/datatypes components
#
### end deps

ejabberd-beams: ejabberd/src/Makefile
	cd ejabberd/src && make compile-beam

ejabberd/:
	# wget -nv -O ejabberd-2.1.9.tar.gz 'https://github.com/processone/ejabberd/tarball/v2.1.9'
	wget -nv 'http://buildbot.pcs/mirror/ejabberd-2.1.9.tar.gz'
	tar -xf ejabberd-2.1.9.tar.gz
	mv processone-ejabberd-4ca3c33 ejabberd

clean: clean-release
	rm -rf ejabberd ejabberd-2.1.9.tar.gz
	rm components/*.beam
	rm -rf components/datatypes

clean-release:
	rm -rf $(reldir)
