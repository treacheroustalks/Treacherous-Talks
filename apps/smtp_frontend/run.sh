rebar compile
cd ebin

sudo erl -name lin@lin.pcs -eval "
application:start(smtp_frontend),
application:stop(smtp_frontend),
init:stop().
"

cd ..
