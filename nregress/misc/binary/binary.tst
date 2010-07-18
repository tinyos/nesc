nescc -fnesc-separator=__ -I def -c -o Foo.o def/BuildBinaryFoo.nc && \
nescc -fnesc-separator=__ -I use -o FooUse.exe use/UseBinaryFoo.nc Foo.o
