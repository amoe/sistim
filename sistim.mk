prefix = /usr/local
libdir = $(prefix)/share/scheme/r6rs
libname = sistim

live:
	rsync -rtv --exclude-from exclude.rsf ./ $(libdir)/$(libname)
