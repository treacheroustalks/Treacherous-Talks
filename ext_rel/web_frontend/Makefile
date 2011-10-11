
help:
	@echo 
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make release"
	@echo
	@echo

all: get-deps compile

get-deps:
	rebar get-deps

compile: get-deps
	rebar compile

clean:
	rebar clean

# YAWS

release: compile
	@rm -rf rel/web_frontend
	@(make rel_inner)
	@echo Generated a self-contained project
	@echo in 'rel/web_frontend', configured to run on Yaws.


# SHARED

rel_inner:
	@(cd rel; rebar generate)
	@(cd rel/web_frontend; make)
	@echo "Built On (uname -v):" > rel/web_frontend/BuildInfo.txt
	@uname -v >> rel/web_frontend/BuildInfo.txt

rellink:  
	$(foreach app,$(wildcard deps/*), rm -rf rel/web_frontend/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/web_frontend/lib;)


