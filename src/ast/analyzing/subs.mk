#

UTIL_OBJS = xthread.cmo xset.cmo xstring.cmo xlist.cmo xarray.cmo \
		xqueue.cmo xoption.cmo \
		xprint.cmo xfile.cmo xhash.cmo compression.cmo \
		xchannel.cmo XML.cmo \
		LCS.cmo weight.cmo HCS.cmo HIS.cmo SMP.cmo LLL.cmo

UTIL_OBJS_DEP = $(foreach f,$(UTIL_OBJS),$(UTIL_DIR)/$(f))

OTREEDIFF_LIB = Otreediff.cmo
OTREEDIFF_OBJS_DEP = $(OTREEDIFF_DIR)/$(OTREEDIFF_LIB)

MLDIFF_OBJS = mldiff.cmo

MLDIFF_OBJS_DEP = $(foreach f,$(MLDIFF_OBJS),$(MLDIFF_DIR)/$(f))

COMMON_OBJS_P = key.cmo loc.cmo binding.cmo moveid.cmo adiff.cmo \
		astml.cmo entity.cmo storage.cmo cache.cmo \
		hash_options.cmo fact_options.cmo fs_options.cmo base_options.cmo \
		basic_options.cmo const.cmo misc.cmo parser_options.cmo \
		origin.cmo region.cmo range.cmo Lrange.cmo LCrange.cmo \
		fragment.cmo GIDfragment.cmo LCfragment.cmo Lfragment.cmo \
		spec_base.cmo spec.cmo \
		triple.cmo info.cmo fs.cmo delta_base.cmo \
		sourcecode.cmo stat.cmo fact_base.cmo \
		charpool.cmo lang_base.cmo

COMMON_OBJS_DEP_P = $(foreach f,$(COMMON_OBJS_P),$(COMMON_DIR)/$(f))

ENGINE_OBJS_P = dirtree_base.cmo

ENGINE_OBJS_DEP_P = $(foreach f,$(ENGINE_OBJS_P),$(ENGINE_DIR)/$(f))

LANGS_COMMON_OBJS = fname.cmo astloc.cmo layeredloc.cmo position.cmo ranges.cmo regions.cmo \
	compat.cmo ast_base.cmo source_base.cmo env_base.cmo parserlib_base.cmo \
	unparsing_base.cmo macro_base.cmo

LANGS_COMMON_OBJS_DEP = $(foreach f,$(LANGS_COMMON_OBJS),$(LANGS_COMMON_DIR)/$(f))
