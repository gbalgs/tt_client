DEPS_DIR ?= $(CURDIR)/deps
DEPS =
ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DEPS))


PROJECT ?= $(notdir $(CURDIR))

V ?= 0
appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))
erlc_verbose_0 = @echo " ERLC  " $(filter %.erl %.core,$(?F));
erlc_verbose = $(erlc_verbose_$(V))
xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose = $(xyrl_verbose_$(V))
dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))
gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))


all: deps compile_all

clean-all: clean clean-deps
	$(gen_verbose) rm -rf .$(PROJECT).plt logs
	
clean:
	$(gen_verbose) rm -rf ebin/*.beam erl_crash.dump
	
clean-deps:
	@for dep in $(ALL_DEPS_DIRS) ; do $(MAKE) -C $$dep clean-all; done

app: compile_all
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed 's/ebin\///;s/\.beam/,/' | sed '$$s/.$$//'))
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

deps: $(ALL_DEPS_DIRS) app
	@for dep in $(ALL_DEPS_DIRS) ; do \
			$(MAKE) deps -C $$dep ; \
	done
	
define compile_erl
	$(erlc_verbose) erlc -v $(ERLC_OPTS) -o ebin/ \
		-pa ebin/ -I include/ $(COMPILE_FIRST_PATHS) $(1)
endef

define compile_xyrl
	$(xyrl_verbose) erlc -v -o ebin/ $(1)
	$(xyrl_verbose) erlc $(ERLC_OPTS) -o ebin/ ebin/*.erl
	@rm ebin/*.erl
endef

define compile_dtl
	$(dtl_verbose) erl -noshell -pa ebin/ $(DEPS_DIR)/erlydtl/ebin/ -eval ' \
		Compile = fun(F) -> \
			Module = list_to_atom( \
				string:to_lower(filename:basename(F, ".dtl")) ++ "_dtl"), \
			erlydtl_compiler:compile(F, Module, [{out_dir, "ebin/"}]) \
		end, \
		_ = [Compile(F) || F <- string:tokens("$(1)", " ")], \
		init:stop()'
endef

compile_all: $(shell find src -type f -name \*.erl) \
		$(shell find src -type f -name \*.core) \
		$(shell find src -type f -name \*.xrl) \
		$(shell find src -type f -name \*.yrl) \
		$(shell find templates -type f -name \*.dtl 2>/dev/null)
	@mkdir -p ebin/
	$(if $(strip $(filter %.erl %.core,$?)), \
		$(call compile_erl,$(filter %.erl %.core,$?)))
	$(if $(strip $(filter %.xrl %.yrl,$?)), \
		$(call compile_xyrl,$(filter %.xrl %.yrl,$?)))
	$(if $(strip $(filter %.dtl,$?)), \
		$(call compile_dtl,$(filter %.dtl,$?)))