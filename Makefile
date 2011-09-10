all: dist/build/iptadmin/iptadmin

dist/build/iptadmin/iptadmin: src/Template.hs src/IptAdmin/AddPage.hs src/IptAdmin/EditForm/Utils.hs src/IptAdmin/EditForm/Render.hs src/IptAdmin/EditForm/Class.hs src/IptAdmin/EditForm/Types.hs src/IptAdmin/Utils.hs src/IptAdmin/Render.hs src/IptAdmin/EditPage.hs src/IptAdmin/InsertPage.hs src/IptAdmin/EditChainForm/Render.hs src/IptAdmin/EditChainForm/Parse.hs src/IptAdmin/DelPage.hs src/IptAdmin/EditForm.hs src/IptAdmin/DelPage/Render.hs src/IptAdmin/ShowPage/Render.hs src/IptAdmin/EditPolicyPage.hs src/IptAdmin/EditPolicyForm/Render.hs src/IptAdmin/EditChainPage.hs src/IptAdmin/AccessControl.hs src/IptAdmin/DelChainPage.hs src/IptAdmin/LoginPage.hs src/IptAdmin/Types.hs src/IptAdmin/System.hs src/IptAdmin/DelChainPage/Render.hs src/IptAdmin/Config.hs src/IptAdmin/AddChainPage.hs src/IptAdmin/ShowPage.hs src/IptAdmin/Static.hs src/Main.hs
	cabal install

src/Template.hs: html/htmlwrapper.html
	ptmpl src/Template.hs html/htmlwrapper.html

install: /usr/bin/iptadmin /etc/iptadmin/iptadmin.conf /etc/pam.d/iptadmin /etc/init.d/iptadmin
	groupadd iptadmin || true
	/etc/init.d/iptadmin start || true
	echo "installed"
	echo "connect to http://localhost:8000 (the port depends on configuration)"

/usr/bin/iptadmin: dist/build/iptadmin/iptadmin
	cp dist/build/iptadmin/iptadmin /usr/bin/

/etc/iptadmin/iptadmin.conf: doc/examples/config/iptadmin.conf
	mkdir /etc/iptadmin || true
	cp doc/examples/config/iptadmin.conf /etc/iptadmin/ || true

/etc/pam.d/iptadmin: doc/examples/pam/iptadmin
	cp doc/examples/pam/iptadmin /etc/pam.d/

/etc/init.d/iptadmin: doc/examples/init/iptadmin
	cp doc/examples/init/iptadmin /etc/init.d/ || true
	chmod +x /etc/init.d/iptadmin || true

uninstall:
	/etc/init.d/iptadmin stop || true
	rm /usr/bin/iptadmin || true
	rm /etc/iptadmin/iptadmin.conf || true
	rmdir /etc/iptadmin || true
	rm /etc/pam.d/iptadmin || true
	rm /etc/init.d/iptadmin
	echo "uninstalled"
