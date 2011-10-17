expat = expat-1.95.8
reldir = rel/xmpp_frontend

all: release

release: $(reldir)

$(reldir): find-erts update-installation ejabberd-installation
	mkdir -p $(reldir)
	cp -r find-erts update-installation ejabberd-installation build_erts installation_prefix $(reldir)

ejabberd-installation/: ejabberd expat-installation
	./find-erts > build_erts
	echo "$(CURDIR)/ejabberd-installation" > installation_prefix
	bash -x -c 'cd ejabberd/src && ./configure \
                                           --prefix=`cat ../../installation_prefix` \
                                           --with-expat=$(CURDIR)/expat-installation \
                                           --with-erlang=`cat ../../build_erts`'
	cd ejabberd/src && make install

ejabberd/:
	git clone git://git.process-one.net/ejabberd/mainline.git ejabberd
	cd ejabberd && git checkout "v2.1.9"

expat-installation/:
	wget "http://downloads.sourceforge.net/project/expat/expat/1.95.8/expat-1.95.8.tar.gz"
	tar -xf $(expat).tar.gz
	cd $(expat) && ./configure --prefix=$(CURDIR)/expat-installation
	cd $(expat) && make install

clean:
	rm -rf ejabberd ejabberd-installation installation_prefix build_erts
	rm -rf $(expat) $(expat).tar.gz expat-installation
	cd rel && rm -rf ejabberd-installation update-installation xmpp_frontend