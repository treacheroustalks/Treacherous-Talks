expat = expat-1.95.8
reldir = rel/xmpp_frontend

all: release

release: $(expat) ejabberd
	mkdir -p $(reldir)
	cp -r ejabberd $(expat) config $(reldir)
	cp install_Makefile $(reldir)/Makefile
	touch $(reldir)

ejabberd/:
	wget -O ejabberd-2.1.9.tar.gz 'https://github.com/processone/ejabberd/tarball/v2.1.9'
	tar -xf ejabberd-2.1.9.tar.gz
	mv processone-ejabberd-4ca3c33 ejabberd

$(expat):
	wget "http://downloads.sourceforge.net/project/expat/expat/1.95.8/expat-1.95.8.tar.gz"
	tar -xf $(expat).tar.gz

clean:
	rm -rf ejabberd ejabberd-2.1.9.tar.gz
	rm -rf $(expat) $(expat).tar.gz 
	if [ -e $(reldir) ]; then cd $(reldir) && rm -rf ejabberd $(expat) config; fi