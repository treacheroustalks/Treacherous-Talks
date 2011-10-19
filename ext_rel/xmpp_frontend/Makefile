reldir = rel/xmpp_frontend

all: release

release: ejabberd components
	mkdir -p $(reldir)
	cp -r ejabberd $(reldir)
	cp -r config $(reldir)
	cp -r components $(reldir)
	cp install_Makefile $(reldir)/Makefile
	cp install_README $(reldir)/README
	touch $(reldir)

ejabberd/:
	wget -O ejabberd-2.1.9.tar.gz 'https://github.com/processone/ejabberd/tarball/v2.1.9'
	tar -xf ejabberd-2.1.9.tar.gz
	mv processone-ejabberd-4ca3c33 ejabberd

clean: clean-release
	rm -rf ejabberd ejabberd-2.1.9.tar.gz

clean-release:
	rm -rf $(reldir)
