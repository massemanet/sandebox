REBAR = ./rebar

.PHONY: all clean deps

all:
	@$(REBAR) compile

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

eunit:
        @$(REBAR) eunit skip_deps=true

deps:
	if [ -d priv/static/CodeMirror ]; then \
	  ( cd priv/static/CodeMirror ]; git pull ) \
	else \
	  ( cd priv/static ; git clone git@github.com:marijnh/CodeMirror.git ) \
	fi
	@$(REBAR) get-deps
	@$(REBAR) update-deps
	@$(MAKE) all
