(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>
   Copyright 2020 Chiba Institute of Technology

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

(* Author: Masatomo Hashimoto <m.hashimoto@stair.center> *)

module Aux = Parser_aux
module PB  = Parserlib_base
module T   = Tokens_
module C   = Context

open Compat

type token = T.token PB.token

let pr_ctx () s = Printf.sprintf "               >>>>>>>>>>>>>>>>>> %s\n" s

let mes fmt = Common._mes "Scanner" fmt

let list_memqn xs = List.exists (fun y -> List.exists ((==) y) xs)

exception Found
exception Abort


let is_semicolon = function
  | T.SEMICOLON _ -> true
  | _ -> false

let is_capital_ident =
  let pat = Str.regexp "^[A-Z0-9_]+$" in
  fun x -> Str.string_match pat x 0

let is_type_name = (* mostly behave like type_name *)
  let l = [
    "int8_t";
    "int16_t";
    "int32_t";
    "int64_t";
    "uint8_t";
    "uint16_t";
    "uint32_t";
    "uint64_t";
    "uintptr_t";
    "ptrdiff_t";
    "size_t";
    (*"string";*)
    "u8string";
    "u16string";
    "u32string";
    "wstring";
    "DWORD";
    (*"BOOL";*)
    "HANDLE";
    "PRBool";
    (*"UInt32";
    "UInt64";*)
    "nsresult";
    "z_stream";
    "evutil_socket_t";
    "ev_intptr_t";
    "FT_UInt32";
    "FT_Module_Constructor";
    "FT_Module_Destructor";
    "FT_Module_Requester";
    "GLenum";
    "GLsizei";
    "GLfloat";
    "GLboolean";
    "GLshort";
    "GLsync";
    "GLubyte";
    "GLbitfield";
    "GLint";
    "GLintptr";
    "GLuint";
    "GLfixed";
    (*"EGLDisplay";*)
    "EGLint";
    "PyObject";
    "opus_int";
    "opus_int8";
    "opus_int16";
    "opus_int32";
    "opus_int64";
    "opus_uint";
    "opus_uint8";
    "opus_uint16";
    "opus_uint32";
    "opus_uint64";
    "opus_val16";
    "opus_val32";
    "CK_ATTRIBUTE";
    "CK_C_GetFunctionList";
    "YY_BUFFER_STATE";
    "OPJ_BOOL";
    "OPJ_CHAR";
    "OPJ_FLOAT32";
    "OPJ_FLOAT64";
    "OPJ_BYTE";
    "OPJ_INT8";
    "OPJ_UINT8";
    "OPJ_INT16";
    "OPJ_UINT16";
    "OPJ_INT32";
    "OPJ_UINT32";
    "OPJ_INT64";
    "OPJ_UINT64";
    "OPJ_OFF_T";
    "OPJ_SIZE_T";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_templ_name =
  let l = [
    "conditional_t";
    (*"type_identity_t";
    "aligned_storage_t";
    "aligned_union_t";
    "remove_cvref_t";
    "decay_t";
    "enable_if_t";
    "common_type_t";
    "common_reference_t";
    "underlying_type_t";
    "invoke_result_t";
    "unwrap_reference_t";
    "unwrap_ref_decay_t";
    "void_t";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_attr_macro =
  let l = [
    "__devinitdata";
    "__in";
    "__init";
    "__initdata";
    "__ref";
    "__refdata";
    "__user";
    "COM_Outptr_";
    "COM_Outptr_result_maybenull_";
    "COM_Outptr_opt_";
    "COM_Outptr_opt_result_maybenull_";
    "CV_EXPORTS";
    "CV_EXPORTS_W_SIMPLE";
    "GAPI_EXPORTS";
    "LIBPROTOBUF_EXPORT";
    "_In_";
    "_In_z_";
    "_In_opt_";
    "_Inout_";
    "_Inout_z_";
    "_Inout_opt_";
    "_Out_";
    "_Out_opt_";
    "_Outptr_";
    "_Outptr_opt_";
    "_Outptr_opt_result_maybenull_";
    "_Outptr_opt_result_maybenull_z_";
    "_Outptr_opt_result_nullonfailure_";
    "_Outptr_opt_result_z_";
    "_Outptr_result_maybenull_";
    "_Outptr_result_maybenull_z_";
    "_Outptr_result_nullonfailure_";
    "_Outptr_result_z_";
    "_Outref_";
    "_Outref_result_maybenull_";
    "_Outref_result_nullonfailure_";
    "_Result_nullonfailure_";
    "_Result_zeroonfailure_";
    "_Nullable";
    "_Nonnull";
    "GTEST_API_";
    "MOZ_STACK_CLASS";
    "MOZ_ASAN_BLACKLIST";
    "MOZ_CAN_RUN_SCRIPT";
    "MOZ_CAN_RUN_SCRIPT_BOUNDARY";
    "MOZ_RAII";
    "MOZ_TRIVIAL_CTOR_DTOR";
    "MOZ_NON_PARAM";
    "MOZ_NON_TEMPORARY_CLASS";
    "MOZ_ALLOW_TEMPORARY";
    "MOZ_STATIC_ASSERT_UNUSED_ATTRIBUTE";
    "JSONCPP_TEMPLATE_DELETE";
    "CDM_CLASS_API";
    "HB_UNUSED";
    "SK_RESTRICT";
    "WINBASEAPI";
    "COMPILE_ASSERT_UNUSED_ATTRIBUTE";
    "_Check_return_";
    "_Post_equals_last_error_";
    "NS_SWIFT_NOTHROW";
    "NS_REFINED_FOR_SWIFT";
    (*"__vector";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_attr_macro_ident =
  let l = [
    "_In_range_";
    "_Out_range_";
    "_Ret_range_";
    "_Deref_in_range_";
    "_Deref_out_range_";
    "_Deref_inout_range_";
    "_Field_range_";
    "_Pre_equal_to_";
    "_Post_equal_to_";
    "_Struct_size_bytes_";
    "_In_reads_";
    "_In_reads_bytes_";
    "_In_reads_or_z_";
    "_In_reads_to_ptr_";
    "_In_reads_to_ptr_z_";
    "_In_reads_z_";
    "_Inout_updates_";
    "_Inout_updates_to_";
    "_Inout_updates_bytes_";
    "_Inout_updates_bytes_all_";
    "_Inout_updates_bytes_to_";
    "_Inout_updates_all_";
    "_Inout_updates_z_";
    "_Out_writes_";
    "_Out_writes_all_";
    "_Out_writes_bytes_";
    "_Out_writes_bytes_all_";
    "_Out_writes_bytes_to_";
    "_Out_writes_to_";
    "_Out_writes_to_ptr_";
    "_Out_writes_to_ptr_z_";
    "_Out_writes_z_";
    "_Outptr_result_buffer_";
    "_Outptr_result_buffer_to_";
    "_Outptr_result_bytebuffer_";
    "_Outptr_result_bytebuffer_to_";
    "_Outptr_opt_result_buffer_";
    "_Outptr_opt_result_buffer_to_";
    "_Outptr_opt_result_bytebuffer_";
    "_Outptr_opt_result_bytebuffer_to_";
    "_Outref_result_buffer_";
    "_Outref_result_buffer_all_";
    "_Outref_result_buffer_all_maybenull_";
    "_Outref_result_buffer_maybenull_";
    "_Outref_result_buffer_to_";
    "_Outref_result_buffer_to_maybenull_";
    "_Outref_result_bytebuffer_";
    "_Outref_result_bytebuffer_all_";
    "_Outref_result_bytebuffer_all_maybenull_";
    "_Outref_result_bytebuffer_maybenull_";
    "_Outref_result_bytebuffer_to_";
    "_Outref_result_bytebuffer_to_maybenull_";
    "MOZ_FORMAT_PRINTF";
    "MOZ_UNSAFE_REF";
    "NS_AVAILABLE_MAC";
    "RTC_GUARDED_BY";
    "RTC_ACQUIRED_AFTER";
    "RTC_ACQUIRED_BEFORE";
    "RTC_PT_GUARDED_BY";
    "RTC_ACCESS_ON";
    "RTC_NO_SANITIZE";
    "OSVR_CPP_ONLY";
    "XXH_ALIGN";
    "GTEST_LOCK_EXCLUDED_";
    "HB_PRINTF_FUNC";
    "_Releases_exclusive_lock_";
    "DWRITE_DECLARE_INTERFACE";
    "MSGPACK_DEPRECATED";
    "PROTOBUF_SECTION_VARIABLE";
    "NS_SWIFT_NAME";
    "NS_SWIFT_UNAVAILABLE";
    (*"VR_OUT_STRING";*)
    (*"_Pragma";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let suffix_pat = Str.regexp "^CL_\\(API\\|EXT\\)_SUFFIX_.*$"
let is_suffix s = Str.string_match suffix_pat s 0

let is_cv_spec_macro =
  let l = [
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s || is_suffix s

let is_cv_spec_macro_ident =
  let l = [
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_decl_spec_macro =
  let l = [
    "__inline";
    "JS_FRIEND_API";
    "JS_PUBLIC_API";
    "HB_EXTERN";
    "LSS_INLINE";
    (*"INLINE";*)
    "__host__";
    "__device__";
    "__forceinline__";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_decl_spec_macro_ident =
  let l = [
    "__declspec";
    "MOZ_THREAD_LOCAL";
    "SK_STDMETHODIMP_";
    "STDMETHODIMP_";
    "HB_DEPRECATED_FOR";
    "CV_DECL_ALIGNED";
    "CVAPI";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_param_decl_macro =
  let l = [
    "MOZ_GUARD_OBJECT_NOTIFIER_PARAM";
    "sqlite3ParserCTX_PDECL";
    "sqlite3Fts5ParserCTX_PDECL";
    "STATE_PARAM";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_param_decl_macro_ident =
  let l = [
    "STROKER_DEBUG_PARAMS";
    (*"DEBUG_COIN_DECLARE_PARAMS";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_params_macro =
  let l = [
    (*"SYSCTL_HANDLER_ARGS";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_params_macro_ident =
  let l = [
    "PNGARG";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_arg_macro =
  let l = [
    "MSGPACK_PP_SEQ_TO_LIST_A_ID";
    (*"MOZ_GUARD_OBJECT_NOTIFIER_PARAM_TO_PARENT";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_arg_macro_ident =
  let l = [
    "SkDEBUGPARAMS";
    "PATH_OPS_DEBUG_T_SECT_PARAMS";
    "T1_FIELD_CALLBACK";
    "MSGPACK_PP_LPAREN";
    "MSGPACK_PP_COMMA";
    "NR_ADD_STUN_ATTRIBUTE";
    "PyObject_HEAD_INIT";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_args_macro =
  let l = [
    (*"MSGPACK_PP_SEQ_BINARY_TRANSFORM_A";
    "MSGPACK_PP_SEQ_BINARY_TRANSFORM_B";*)
    (*"__VA_ARGS__";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_args_macro_ident =
  let l = [
    "MSGPACK_DEFINE_MAP_IMPL";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s


let is_cc_macro =
  let l = [
    "WINAPI";
    "APIENTRY";
    "GLAPIENTRY";
    "NS_STDCALL";
    "CV_STDCALL";
    "CL_API_CALL";
    "CL_API_ENTRY";
    "CL_CALLBACK";
    "PTRFASTCALL";
    "ITTAPI";
    "GR_GL_FUNCTION_TYPE";
    "U_CALLCONV_FPTR";
    "HOOK_CALL";
    "_Optlink";
    "__stdcall";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_ptr_macro =
  let l = [
    "GL_APIENTRYP";
    "EGLAPIENTRYP";
    "PNGCBAPI";
    "APIENTRYP";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_virt_spec_macro =
  let l = [
    "Q_DECL_OVERRIDE";
    "MOZ_MUST_OVERRIDE";
    "CV_FINAL";
    "CV_OVERRIDE";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_virt_spec_macro_ident =
  let l = [
    "RTC_EXCLUSIVE_LOCKS_REQUIRED";
    "SK_REQUIRES";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_type_macro_ident =
  let l = [
    "__typeof__";
    "__typeof";
    "BOOST_TYPEOF";
    (*"LOCAL";*)
    (*"GLOBAL";*)
    (*"EXTERN";*)(* also used as obj macro *)
    "FT_LOCAL";
    "FT_LOCAL_ARRAY";
    "FT_EXPORT";
    "FT_EXPORT_VAR";
    "FT_BASE_DEF";
    "HT_HEAD";
    "NSPR_API";
    "EXPORT_XPCOM_API";
    "ElfW";
    "NP_EXPORT";
    "EXPORT_CDECL";
    "TAILQ_HEAD";
    "GMOCK_RESULT_";
    "GMOCK_ARG_";
    "GMOCK_MATCHER_";
    "SK_WHEN";
    "msgpack_unpack_struct";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_expr_macro_ident =
  let l = [
    "offsetof";
    "va_arg";
    "evutil_offsetof";
    "evutil_timercmp";
    "MOZ_FOR_EACH_SEPARATED"; (* also used as param_decl *)
    (*"MOZ_LOG";*)
    (*"PKIX_ERRORENTRY";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_class_head_macro_ident =
  let l = [
    "DECL_INTERFACE";
    "CODER_INTERFACE";
    "STREAM_INTERFACE";
    "STREAM_INTERFACE_SUB";
    "ARCHIVE_INTERFACE";
    "PASSWORD_INTERFACE";
    "MIDL_INTERFACE";
    "PARAM_TEST_CASE";
    "GAPI_FLUID_KERNEL";
    "GAPI_GPU_KERNEL";
    "GAPI_OCV_KERNEL";
    "GAPI_OCV_KERNEL_ST";
    "GAPI_PLAIDML_KERNEL";
    "GAPI_RENDER_OCV_KERNEL";
    "G_API_OP";
    "G_TYPED_KERNEL";
    "G_TYPED_KERNEL_M";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_decl_stmt_macro_ident =
  let l = [
    "MOZ_ALIGNED_DECL";
    "MOZ_FOR_EACH"; (* also used as expr *)
    "JS_FOR_EACH_TRACEKIND"; (* also used as expr *)
    "DECLARE_ALIGNED";
    "PNG_UNUSED";
    "PNG_FUNCTION";
    "SIMD_ALIGNED";
    "LSS_BODY";
    "GTEST_DISABLE_MSC_WARNINGS_PUSH_";
    "GTEST_DISABLE_MSC_WARNINGS_POP_";
    "IMPL_IUNKNOWN_QUERY_IFACE_AMBIGIOUS";
    "IMPL_IUNKNOWN_QUERY_IFACE";
    "COPY_REQUEST_COMMON";
    "COPY_REQUEST_STATE";
    "RINOK_THREAD";
    "__pragma";
    "_Pragma";
    "G_API_NET";
    "PERF_TEST_P";
    "OCL_PERF_TEST_P";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_body_macro =
  let l = [
    "GTEST_CXX11_EQUALS_DELETE_";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_body_macro_ident =
  let l = [
    "HB_AUTO_RETURN";
    (*"HB_RETURN";*)
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_ident_macro_ident =
  let l = [
    "STDMETHOD";
    "STDMETHOD_";
    "GTEST_TEST_CLASS_NAME_";
    "GTEST_CASE_NAMESPACE_";
    "GTEST_FLAG";
    "GMOCK_ACTION_CLASS_";
    "IFACEMETHOD";
    "LSS_NAME";
    (*"ITT_JOIN";*)
    "ITT_VERSIONIZE";
    "ITTNOTIFY_NAME";
    "BIND_NAME_CONCAT";
    "CK_PKCS11_FUNCTION_INFO";
    "NS_NPAPIPLUGIN_CALLBACK";
    "NS_CYCLE_COLLECTION_CLASSNAME";
    "ZYAN_MACRO_CONCAT_EXPAND";
    "msgpack_unpack_func";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_str_macro =
  let l = [
    "__DATE__";
    "__FILE__";
    "__TIME__";
    "GTEST_PATH_SEP_";
    "XPCOM_FILE_PATH_SEPARATOR";
    "CV_VERSION_STATUS";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_stmt_macro =
  let l = [
    "YY_BREAK"; "YY_RULE_SETUP";
    "MOZILLA_PKIX_MAP_LIST";
    "MOZILLA_PKIX_UNREACHABLE_DEFAULT_ENUM";
    "FT_BEGIN_STMNT";
    "FT_END_STMNT";
    "PROFILE_CALL";
    "BEFORE_CALL";
    "AFTER_CALL";
    "COPY_REQUEST_IDENTITY";
    "FLOATING_POINT_SET_PRECISION";
    "FLOATING_POINT_SET_EXCEPTIONS";
    "FLOATING_POINT_RESTORE_EXCEPTIONS";
    "FLOATING_POINT_RESTORE_PRECISION";
    "sqlite3ParserARG_STORE";
    "sqlite3ParserCTX_STORE";
    "sqlite3Fts5ParserARG_FETCH";
    "sqlite3Fts5ParserCTX_FETCH";
    "sqlite3Fts5ParserARG_STORE";
    "sqlite3Fts5ParserCTX_STORE";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_decl_macro =
  let l = [
    "U_NAMESPACE_BEGIN"; "U_NAMESPACE_END";
    "U_NAMESPACE_USE";
    "U_CDECL_BEGIN"; "U_CDECL_END";
    "CAIRO_BEGIN_DECLS"; "CAIRO_END_DECLS";
    "HB_BEGIN_DECLS"; "HB_END_DECLS";
    "PR_BEGIN_EXTERN_C"; "PR_END_EXTERN_C";
    "MOZ_BEGIN_EXTERN_C"; "MOZ_END_EXTERN_C";
    "MOZ_BEGIN_STD_NAMESPACE"; "MOZ_END_STD_NAMESPACE";
    "MOZ_DECL_USE_GUARD_OBJECT_NOTIFIER";
    "NS_ELEMENT_INTERFACE_TABLE_TO_MAP_SEGUE";
    "NS_ASSUME_NONNULL_BEGIN";
    "FT_BEGIN_HEADER"; "FT_END_HEADER";
    "YY_DECL"; "YY_MALLOC_DECL";
    "QUERY_ENTRY_ISetCompressCodecsInfo";
    "MY_QUERYINTERFACE_BEGIN"; "MY_QUERYINTERFACE_END";
    "MY_ADDREF_RELEASE";
    "MY_UNKNOWN_IMP";
    "DECL_IUNKNOWN_INHERITED";
    "NUMERIC_CONVERSION_METHOD_END";
    "REGISTER_CODECS_VAR";
    "REGISTER_CODEC_VAR";
    "OSVR_EXTERN_C_BEGIN"; "OSVR_EXTERN_C_END";
    "OT_TABLE_START"; "OT_TABLE_END";
    "DLL_BLOCKLIST_DEFINITIONS_BEGIN"; "DLL_BLOCKLIST_DEFINITIONS_END";
    "KEEP_CLASSES_FRIENDLY";
    "KEEP_CAST_FRIENDLY";
    "sqlite3ParserARG_SDECL";
    "sqlite3ParserCTX_SDECL";
    "sqlite3Fts5ParserARG_SDECL";
    "sqlite3Fts5ParserCTX_SDECL";
    "SQLITE_EXTENSION_INIT1";
    "STATE_PARAM_DECL";
    "TRIVIAL_NEW_WRAPPERS";
    "TRIVIAL_NEW_WRAPPERS_WITH_ALLOC";
    "TEST_INIT";
    "EXPECT_DEATH_INIT";
    "REQUEST_HEADER_FIELDS";
    "REQUEST_IDENTITY_FIELDS";
    "REQUEST_STATE_FIELDS";
    "REQUEST_TRAILER_FIELDS";
    "FT_MODERR_START_LIST";
    "CV_CPU_OPTIMIZATION_HAL_NAMESPACE_BEGIN"; "CV_CPU_OPTIMIZATION_HAL_NAMESPACE_END";
    "CV__DNN_INLINE_NS_BEGIN"; "CV__DNN_INLINE_NS_END";
    "IMATH_INTERNAL_NAMESPACE_HEADER_ENTER"; "IMATH_INTERNAL_NAMESPACE_HEADER_EXIT";
    "IEX_INTERNAL_NAMESPACE_HEADER_ENTER"; "IEX_INTERNAL_NAMESPACE_HEADER_EXIT";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_decl_macro_ident =
  let l = [
    "JS_DECLARE_NEW_METHODS";
    "MOZ_DEFINE_MALLOC_SIZE_OF";
    "MOZ_COUNTED_DTOR_VIRTUAL";
    "MOZ_COUNTED_DTOR";
    "NS_IMETHODIMP_TO_ERRORRESULT";
    "NS_FORWARD_SAFE_NSIURISETTERS_RET";
    "PS_GET";
    "PS_GET_AND_SET";
    "PS_GET_LOCKLESS";
    "NUMERIC_CONVERSION_METHOD_BEGIN";
    "MY_UNKNOWN_IMP2";
    "MY_QUERYINTERFACE_ENTRY_UNKNOWN";
    "FORBID_STRING_OPS_AString";
    "FORBID_STRING_OPS_UString";
    "FORBID_STRING_OPS_2";
    "CV_CONTOUR_FIELDS";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_block_head_macro =
  let l = [
    "IMPL_IUNKNOWN_QUERY_HEAD";
    "STARTOP";
    "STRONG_GLENUM_BEGIN";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_block_end_macro =
  let l = [
    "IMPL_IUNKNOWN_QUERY_TAIL";
    "IMPL_IUNKNOWN_QUERY_TAIL_AGGREGATED";
    "IMPL_IUNKNOWN_QUERY_TAIL_INHERITED";
    "ENDOP";
    "STRONG_GLENUM_END";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_asm_macro_ident =
  let l = [
    "OC_M2STR";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_suffix_macro_ident =
  let l = [
    "CV_DEFAULT";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_delim_macro =
  let l = [
    "MOZ_NSTARRAY_COMMA";
    "SK_CALLABLE_TRAITS__COMMA";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_shader_kw =
  let l = [
    "cbuffer";
    "Texture2D";
    "BlendState";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_asm_kw =
  let l = [
    "push";
    "pop";
    "sub";
    "subs";
    "";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_doxygen_cmd =
  let l = [
    "\\sa";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_gas_directive =
  let l = [
    "abort";
    "ascii";
    "asciz";
    "att_syntax";
    "bss";
    "byte";
    "comm";
    "data";
    "def";
    "dim";
    "double";
    "eject";
    "endef";
    "endif";
    "endm";
    "error";
    "equ";
    "file";
    "global";
    "if";
    "intel_syntax";
    "macro";
    "section";
    "set";
    "skip";
    "string";
    "text";
    "word";
  ] in
  let names = Xset.create 0 in
  List.iter (Xset.add names) l;
  fun s -> Xset.mem names s

let is_qt_decl_macro = function
  | "Q_OBJECT" -> true
  | _ -> false

let is_qt_decl_macro_func = function
  | "Q_PROPERTY" -> true
  | _ -> false

let levels_to_str e =
  let l = [
    "_pp_if_section_lv",    e#_pp_if_section_level;
    "brace_lv",             e#brace_level;
    "brace_lv_marker",      e#pstat#brace_level_marker;
    "braced_init_lv",       e#braced_init_level;
    "bracket_lv",           e#bracket_level;
    "in_body_brace_lv",     e#in_body_brace_level;
    "macro_arg_lv",         e#macro_arg_level;
    "objc_message_expr_lv", e#objc_message_expr_level;
    "odd_brace_lv",         e#odd_brace_level;
    "paren_lv",             e#paren_level;
    "rel_paren_lv",         e#rel_paren_level;
    "pp_if_section_lv",     e#pp_if_section_level;
    "pp_paren_lv",          e#pp_paren_level;
    "templ_head_lv",        e#templ_head_level;
    "templ_param_arg_lv",   e#templ_param_arg_level;
    "typename_lv",          e#typename_level;
  ]
  in
  String.concat " "
    (List.map
       (fun (k, v) ->
         Printf.sprintf "%s:%d" k v
       ) (List.filter (fun (k, v) -> v <> 0) l))

let flags_to_str e =
  let l =
    ["pp_line"        , e#pp_line_flag;
     "pp_if"          , e#pp_if_flag;
     "pp_ifdef"       , e#pp_ifdef_flag;
     "pp_ifx_d"       , e#pp_ifx_d_flag;
     "pp_define"      , e#pp_define_flag;
     "pp_define_body" , e#pp_define_body_flag;
     "pp_func_body_odd", e#pp_func_body_odd_flag;
     "pp_if_section"  , e#pp_if_section_flag;
     "pp_odd"         , e#pp_odd_flag;
     "pp_elif"        , e#pp_elif_flag;
     "pp_else"        , e#pp_else_flag;
     "pp_params"      , e#pp_params_flag;
     "alias"          , e#alias_flag;
     "alignas"        , e#alignas_flag;
     "_arg_paren"     , e#_arg_paren_flag;
     "arg_paren"      , e#arg_paren_flag;
     "asm"            , e#asm_flag;
     "asm_block"      , e#asm_block_flag;
     "base_clause"    , e#base_clause_flag;
     "body_head"      , e#body_head_flag;
     "braced_asm"     , e#braced_asm_flag;
     "braced_init"    , e#braced_init_flag;
     "brace_lv_marker", e#pstat#brace_level_marker_flag;
     "cast_key"       , e#cast_key_flag;
     "cast_head"      , e#cast_head_flag;
     "class_name"     , e#class_name_flag;
     "const_flag"     , e#const_flag;
     "conv_func_id"   , e#conv_func_id_flag;
     "ctor_init_flag" , e#ctor_init_flag;
     "decltype"       , e#decltype_flag;
     "dtor"           , e#dtor_flag;
     "dtor_if_section", e#dtor_if_section_flag;
     "decl_stmt_block", e#decl_stmt_block_flag;
     "end_of_attr_macro_call", e#end_of_attr_macro_call_flag;
     "end_of_cast_type", e#end_of_cast_type_flag;
     "end_of_class_spec", e#end_of_class_spec_flag;
     "end_of_decltype", e#end_of_decltype_flag;
     "end_of_noptr_dtor_paren", e#end_of_noptr_dtor_paren_flag;
     "end_of_enum_spec", e#end_of_enum_spec_flag;
     "end_of_handler_head", e#end_of_handler_head_flag;
     "end_of_id_macro_call", e#end_of_id_macro_call_flag;
     "end_of_if_head" , e#end_of_if_head_flag;
     "end_of_objc_meth_sel", e#end_of_objc_meth_sel_flag;
     "end_of_params"  , e#end_of_params_flag;
     "end_of_templ_head", e#end_of_templ_head_flag;
     "enum_head"      , e#enum_head_flag;
     "exec_config"    , e#exec_config_flag;
     "expr"           , e#expr_flag;
     "fold_paren"     , e#fold_paren_flag;
     "for"            , e#for_flag;
     "for_range_init" , e#for_range_init_flag;
     "in_body_brace"  , e#in_body_brace_flag;
     "init"           , e#init_flag;
     "lambda_dtor"    , e#lambda_dtor_flag;
     "last_ty_templ_id", e#last_ty_templ_id_flag;
     "macro_arg"      , e#macro_arg_flag;
     "ns_alias"       , e#ns_alias_flag;
     "in_objc_message_expr", e#in_objc_message_expr;
     "noexcept"       , e#noexcept_flag;
     "objc_block"     , e#objc_block_flag;
     "objc_class_interface", e#objc_class_interface_flag;
     "old_param_decl" , e#old_param_decl_flag;
     "param_head"     , e#param_head_flag;
     "sizeof_ty"      , e#sizeof_ty_flag;
     "start_of_func_body", e#start_of_func_body_flag;
     "stmts"          , e#stmts_flag;
     "str"            , e#str_flag;
     "templ_arg"      , e#templ_arg_flag;
     "templ_head"     , e#templ_head_flag;
     "top_stmts"      , e#top_stmts_flag;
     "trailing_retty" , e#trailing_retty_flag;
     "ty_param"       , e#ty_param_flag;
     "ty_param_key"   , e#ty_param_key_flag;
     "ty_param_rhs"   , e#ty_param_rhs_flag;
     "ty_templ_id"    , e#ty_templ_id_flag;
     "type_paren"     , e#type_paren_flag;
     "typedef"        , e#typedef_flag;
     "typename"       , e#typename_flag;
     "using"          , e#using_flag;
     "using_ns"       , e#using_ns_flag;
     "value"          , e#value_flag;
     "virtual_func"   , e#virtual_func_flag;
   ] in
  String.concat "" (List.map (fun (k, v) -> "["^k^"]") (List.filter (fun (k, v) -> v) l))

let paren_balanced ?(level=0) l =
  try
    0 =
    List.fold_left
      (fun lv -> function
        | T.TY_LPAREN | LPAREN -> lv + 1
        | RPAREN when lv = 0 -> raise Exit
        | RPAREN -> lv - 1
        | _ -> lv
      ) level l
  with
    Exit -> false

let templ_param_arg_balanced
    ?(stack=Stack.create())
    ?(paren_level=0)
    ?(exact=false)
    ?(rev=false)
    ?(weak=false)
    l =
  let ok = ref false in
  let ini_lv = Stack.length stack in
  let push x = Stack.push x stack in
  let pop () = try ignore (Stack.pop stack) with _ -> () in
  let get_top () = try Stack.top stack with _ -> 0 in
  (*let ini_top = get_top() in*)
  let _, lv, plv, blv, _, _ =
    DEBUG_MSG "(maxlv,lv,plv,blv)top";
    List.fold_left
      (fun (max_lv, lv, plv, blv, prev2, prev1) rt ->
        DEBUG_MSG "(%d,%d,%d,%d)%d %s %s %s" max_lv lv plv blv (get_top())
          (Token.rawtoken_to_string prev2) (Token.rawtoken_to_string prev1) (Token.rawtoken_to_string rt);
        let templ_lt_possible =
          match prev2, prev1 with
          | (T.TY_LPAREN|LPAREN), T.RPAREN -> false
          | _ -> true
        in
        DEBUG_MSG "templ_lt_possible=%B" templ_lt_possible;
        match rt with
        | T.TEMPL_LT | LT when templ_lt_possible && blv = 0 -> begin
            let lv1 = lv + 1 in
            let max_lv1 = if lv1 > max_lv then lv1 else max_lv in
            push plv;
            (max_lv1, lv1, plv, blv, prev1, rt)
        end
        | TY_TEMPL_GT | TEMPL_GT | GT when blv = 0 && plv = get_top() -> begin
            let lv1 = lv - 1 in
            if (lv1 = 0 || (not exact && lv1 = ini_lv)) && max_lv > ini_lv then
              ok := true;
            pop();
            (max_lv, lv1, plv, blv, prev1, rt)
        end
        | GT_GT when not weak && blv = 0 && plv = get_top() -> begin
            let lv2 = lv - 2 in
            if (lv2 = 0 || (not exact && (lv2 = ini_lv || lv2 + 1 = ini_lv))) && max_lv > ini_lv then
              ok := true;
            pop();
            pop();
            (max_lv, lv2, plv, blv, prev1, rt)
        end
        | TY_LPAREN | LPAREN when blv = 0 -> (max_lv, lv, plv + 1, blv, prev1, rt)
        | RPAREN when blv = 0 -> (max_lv, lv, plv - 1, blv, prev1, rt)
        | LBRACE -> (max_lv, lv, plv, blv + 1, prev1, rt)
        | RBRACE -> (max_lv, lv, plv, blv - 1, prev1, rt)
        | _ -> (max_lv, lv, plv, blv, prev1, rt)
      ) (ini_lv, ini_lv, paren_level, 0, T.EOF, T.EOF) (if rev then List.rev l else l)
  in
  DEBUG_MSG "level=%d inner_paren_level=%d brace_level=%d" lv plv blv;
  let b =
    lv = 0 || !ok
  in
  DEBUG_MSG "b=%B" b;
  b

let templ_param_arg_balanced_ll ?(level=0) ll =
  0 =
  List.fold_left
    (fun lv l ->
      List.fold_left
        (fun lv0 -> function
          | T.TEMPL_LT | LT -> lv0 + 1
          | TY_TEMPL_GT | TEMPL_GT | GT -> lv0 - 1
          | GT_GT -> lv0 - 2
          | _ -> lv0
        ) lv l
    ) level ll


let filt_at_level0 ?(rev=true) l f =
  try
    let _ =
      List.fold_left
        (fun (i, lv, prev) x ->
          DEBUG_MSG "%d: %s (%d)" i (Token.rawtoken_to_string x) lv;
          match x with
          | T.TEMPL_LT | TY_LPAREN | LBRACKET -> i + 1, lv + 1, x
          | TY_TEMPL_GT | RPAREN | RBRACKET -> i + 1, lv - 1, x
          | _ when f(x) && lv = 0 && begin
              match prev with
              | EQ -> false
              | _ -> true
          end -> raise Exit
          | _ -> i + 1, lv, x
        ) (0, 0, T.EOF) (if rev then List.rev l else l)
    in
    false
  with
  | Exit -> true
  | Abort -> false

let mem_at_level0 ?(rev=true) l rt = filt_at_level0 ~rev l ((==) rt)

let split_at_comma l =
  let ll, _ =
    List.fold_left
      (fun (a, plv) -> function
        | T.COMMA as x -> begin
            if plv > 0 then
              match a with
              | [] -> ([x]::a, plv)
              | h::t -> ((x::h)::t, plv)
            else
              ([]::a, plv)
        end
        | TY_LPAREN | LPAREN | TEMPL_LT as x -> begin
            match a with
            | [] -> ([x]::a, plv+1)
            | h::t -> ((x::h)::t, plv+1)
        end
        | RPAREN | TY_TEMPL_GT | TEMPL_GT as x -> begin
            match a with
            | [] -> ([x]::a, plv-1)
            | h::t -> ((x::h)::t, plv-1)
        end
        | x -> begin
            match a with
            | [] -> ([x]::a, plv)
            | h::t -> ((x::h)::t, plv)
        end
      ) ([], 0) l
  in
  ll

let split_at_semicolon l =
  let ll, _ =
    List.fold_left
      (fun (a, blv) -> function
        | T.SEMICOLON _ as x -> begin
            if blv > 0 then
              match a with
              | [] -> ([x]::a, blv)
              | h::t -> ((x::h)::t, blv)
            else
              ([]::a, blv)
        end
        | LBRACE as x -> begin
            match a with
            | [] -> ([x]::a, blv+1)
            | h::t -> ((x::h)::t, blv+1)
        end
        | RBRACE as x -> begin
            match a with
            | [] -> ([x]::a, blv-1)
            | h::t -> ((x::h)::t, blv-1)
        end
        | x -> begin
            match a with
            | [] -> ([x]::a, blv)
            | h::t -> ((x::h)::t, blv)
        end
      ) ([], 0) l
  in
  ll

let contained_in_list pat lst =
  if pat = [] then
    failwith "Scanner.contained_in_list";
  let rec scan ?(head=false) = function
    | (ph::pt as p) -> begin
        function
          | h::t when h = ph -> if pt == [] then true else scan ~head:true pt t
          | _::t when head -> scan pat t
          | _::t -> scan p t
          | [] -> false
    end
    | [] -> fun _ -> true
  in
  scan pat lst

let contained_in_list_f filt lst =
  let rec scan = function
    | [] -> false
    | h::t as l -> if filt l then true else (scan t)
  in
  scan lst

let count_parens =
  List.fold_left
    (fun (oc, cc) x ->
      match (x : T.token) with
      | TY_LPAREN | LPAREN -> oc+1, cc
      | RPAREN -> oc, cc+1
      | _ -> oc, cc
    ) (0, 0)


class type c_t = object
  method keep_flag : bool
  method clear_keep_flag : unit -> unit

  method get_token : unit -> token

  method prev_endofs : int
  method prev_endln : int
  method prev_edp : Lexing.position
  method prev_rawtoken : T.token
  method prev_rawtoken2 : T.token
  method prev_rawtoken3 : T.token
  method prev_rawtoken4 : T.token

  method current_token : token
  method current_loc : Ast.Loc.t

  method enter_block : unit -> unit

  method peek : unit -> token
  method peek_nth : int -> token
  method peek_rawtoken : unit -> T.token
  method peek_nth_rawtoken : int -> T.token
  method peek_rawtoken_up_to : ?from:int -> ?skip_pp_control_line:bool -> ?is_target:(T.token -> bool) -> T.token list -> int * T.token list
  method peek_rawtoken_up_to_rparen :
      ?from:int -> ?level:int -> ?filt:(T.token -> bool) -> T.token option -> bool * int * T.token list
  method peek_rawtoken_up_to_rparen_none : unit -> int * T.token list
  method peek_rawtoken_up_to_group_end :
      ?limit:int -> ?from:int -> ?filt:(T.token -> bool) -> unit -> int * T.token list
  method peek_rawtoken_up_to_section_end : ?from:int -> unit -> int
  method peek_rawtoken_up_to_end_of_qualified_id : ?from:int -> ?ini_tlv:int -> unit -> int
  method peek_rawtoken_up_to_rparen_split_at_comma :
      ?from:int -> ?ignore_pp:bool -> ?ignore_templ_lv:bool -> ?lv_ofs:int -> ?filt:(T.token list -> bool) ->
        unit -> int * T.token list list
  method peek_rawtoken_up_to_rbrace : ?noexcept:bool ->
      ?from:int -> ?lv_ofs:int -> ?filt:(T.token -> bool) -> unit -> int * T.token list
  method peek_rawtoken_up_to_rbracket :
      ?from:int -> ?lv_ofs:int -> ?filt:(T.token -> bool) -> unit -> int * T.token list

  method reg_ident_conv : string -> T.token -> unit
  method find_ident_conv : string -> T.token

  method lookup_name : string -> Pinfo.Name.Spec.c
  method is_type : ?weak:bool -> string -> bool
  method is_templ : string -> bool
  method is_val : string -> bool
  method _is_val : string -> bool
  method is_macro_fun : string -> bool
  method is_macro_obj : string -> bool

  method reg_macro_fun : string -> unit

  method is_ty : ?weak:bool -> T.token -> bool
  method check_if_param : ?weak:bool -> T.token list -> bool
  method check_if_params : ?weak:bool -> T.token list list -> bool
  method check_if_macro_arg : T.token list -> bool
  method check_if_macro_args : T.token list list -> bool
  method check_if_noptr_dtor : ?weak:bool -> T.token list -> bool
  method check_if_noptr_dtor_ : ?weak:bool -> T.token list -> bool

  method is_lparen : ?from:int -> ?ignore_pp:bool -> unit -> bool

  method skip_pp : ?limit:int -> int -> int

  method prepend_token : token -> unit
  method discard_token : unit -> token
  method queue_token : token -> unit
  method reset : unit -> unit

  method check_top_stmts_flag : bool
  method set_check_top_stmts_flag : unit -> unit
  method clear_check_top_stmts_flag : unit -> unit

  method macro_body_parsing_flag : bool
  method set_macro_body_parsing_flag : unit -> unit
  method clear_macro_body_parsing_flag : unit -> unit

  method is_opening_stmt_macro : string -> bool

  method context : C.t
  method sub_context : C.sub

  method ctx_reset : unit -> unit
  method ctx_ty : unit -> unit
  method ctx_expr : ?force:bool -> unit -> unit
  method ctx_stmt : unit -> unit
  method ctx_enum : unit -> unit
  method ctx_class : unit -> unit
  method ctx_mem : unit -> unit
  method ctx_new : unit -> unit
  method ctx_top : unit -> unit
  method ctx_ini : unit -> unit
  method ctx_mem_init : unit -> unit
  method ctx_in_case_label : unit -> unit
  method ctx_in_expr : unit -> unit
  method ctx_end_of_ty_spec : unit -> unit
  method ctx_start_of_stmt : int -> unit
  method ctx_end_of_lam_intro : unit -> unit
  method ctx_end_of_dtor : unit -> unit
  method ctx_end_of_id_expr : unit -> unit
  method ctx_end_of_stmt : unit -> unit
  method ctx_in_simple_templ_id : unit -> unit

  method top_context : C.t
  method top_sub_context : C.sub
  method push_context : unit -> unit
  method push_sub_context : unit -> unit
  method pop_context : unit -> unit
  method pop_sub_context : unit -> unit

  method replay_queue_length : int
  method init_replay_queue : int -> Aux.pstat -> token list -> unit
  method setup_replay : unit -> unit
  method stop_replay_queue : unit -> unit
  method register_replay_success_callback : (unit -> unit) -> unit
  method has_alternative_tokens : bool
  method clear_alternative_tokens : unit -> unit

  method restore_context : unit -> unit
  method restore_state : unit -> unit
  method state_number : int

  method show_token_hist : unit -> unit
  method set_token_hist_flag : unit -> unit

  method pp_restore_context : unit -> unit

end


let ty_pat0 = function
  | T.BOOL::MINUS_GT::IDENT _::_ -> false
  | (T.CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL
  | SHORT | INT | LONG | FLOAT | DOUBLE | VOID | CONST)::_ -> true
  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::_ -> true
  | IDENT x::[] when is_type_name x -> true
  | _ -> false

let ty_pat1 = function
  | (T.CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL
  | SHORT | INT | LONG | FLOAT | DOUBLE | VOID | CONST)::_ -> true
  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[] -> true
  | IDENT x::[] when is_type_name x -> true
  | _ -> false

let ty_pat2 = function
  | [] -> false
  | (T.CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL
  | SHORT | INT | LONG | FLOAT | DOUBLE | VOID | CONST)::_ -> true
  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[] -> true
  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::[] -> true
  | (IDENT _)::(IDENT _)::[] -> true
  | IDENT x::[] when is_type_name x -> true
  | l -> begin
      match Xlist.last l with
      | CONST | CLASS | STRUCT | UNION -> true
      | _ -> false
  end

let is_basic_ty = function
  | T.CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
  | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST -> true
  | _ -> false

let is_decl_spec = function
  | T.CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
  | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO
  | STATIC | THREAD_LOCAL | EXTERN | MUTABLE | REGISTER | FRIEND | VIRTUAL
  | CONST | CONSTEVAL | CONSTINIT | CONSTEXPR | INLINE -> true
  | TYPE_MACRO _ | IDENT_TM _ | DECL_SPEC_MACRO _ | IDENT_DM _ -> true
  | IDENT x when begin
      is_type_name x || is_type_macro_ident x ||
      is_decl_spec_macro x || is_decl_spec_macro_ident x
  end -> true
  | _ -> false

let ty_pat3 = function
  | [] -> false
  | T.BOOL::MINUS_GT::IDENT _::_ -> false
  | (T.CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL
  | SHORT | INT | LONG | FLOAT | DOUBLE | VOID | CONST)::_ -> true
  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[] -> true
  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::x::[] -> is_basic_ty x
  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::[] -> true
  | IDENT x::[] when is_type_name x -> true
  | IDENT _::IDENT x::[] when is_type_name x -> true
  | l -> begin
      match Xlist.last l with
      | CONST | CLASS | STRUCT | UNION | DECLTYPE -> true
      | _ -> false
  end

let is_numeral = function
  | T.INT_LITERAL _ | FLOAT_LITERAL _ | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> true
  | _ -> false

let is_literal = function
  | T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
  | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
  | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
    -> true
  | _ -> false

let is_literal_or_ident = function
  | T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
  | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
  | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
  | IDENT _
    -> true
  | _ -> false

let is_literal_abort_at_eq = function
  | T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
  | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
  | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
    -> true
  | EQ -> raise Abort
  | _ -> false

let is_fold_op = function
  | T.PTR_AMP_AMP | BAR_BAR _ | PLUS | MINUS | PTR_STAR | SLASH | PERC
  | HAT _ | PTR_AMP | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
  | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
  | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
  | DOT_STAR | MINUS_GT_STAR | COMMA -> true
  | _ -> false

let is_op = function
  | T.AMP_AMP _ | PTR_AMP_AMP | BAR_BAR _
  | PLUS | MINUS | STAR | PTR_STAR | SLASH | PERC
  | HAT _ | PTR_AMP | AMP _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
  | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
  | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
  | LT | GT | TEMPL_LT | TY_TEMPL_GT(* | PLUS_PLUS | MINUS_MINUS*)
  | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> true
  | _ -> false

let is_uop = function
  | T.EXCLAM _ | TILDE _ | PLUS_PLUS | MINUS_MINUS -> true
  | _ -> false

let is_arith = function
  | T.PLUS | MINUS | SLASH | PERC -> true
        (*| PTR_STAR -> begin
          true
          end*)
  | _ -> false

let is_ident = function
  | T.IDENT _ -> true
  | T.IDENT_V _ -> true
  | _ -> false

let is_pp_control_line = function
  | T.PP_INCLUDE | PP_IMPORT | PP_DEFINE | PP_UNDEF | PP_LINE | PP_ERROR
  | PP_PRAGMA | PP_ | PP_UNKNOWN _ -> true
  | _ -> false

let is_stmt_head = function
  | T.RETURN | GOTO | BREAK | CONTINUE
  | IF | ELSE | SWITCH | CASE | DEFAULT
  | FOR | WHILE | DO
  | TRY -> true
  | _ -> false


let each_pat = Str.regexp "^.*FOR_EACH.*$"
let contains_foreach s = Str.string_match each_pat s 0

let comma_pat = Str.regexp "^.*COMMA.*$"
let contains_comma s = Str.string_match comma_pat s 0

let ns_pat = Str.regexp "^.*NAMESPACE.*$"
let contains_ns s = Str.string_match ns_pat s 0

let arg_pat = Str.regexp "^.*arg.*$"
let contains_arg s = Str.string_match arg_pat (String.lowercase_ascii s) 0

let param_pat = Str.regexp "^.*param.*$"
let contains_param s = Str.string_match param_pat (String.lowercase_ascii s) 0

let ns_block_head_pat = Str.regexp "^\\(NS_\\(IMPL\\|.*INTERFACE_MAP\\)_.*\\|FT_\\)BEGIN.*\\|NS_.*HEAD\\(_.*\\)?$"
let ns_block_end_pat = Str.regexp "^\\(NS_\\(IMPL\\|.*INTERFACE_MAP\\)_.*\\|FT_\\)END.*\\|NS_\\(.*\\([^D].\\|.[^E]\\)\\|.\\)?TAIL.*\\|NS_.*_END_.*$"
let ns_decl_func_pat = Str.regexp "^NS_\\(\\|INLINE_DECL\\|DECL\\|DECLARE\\|DEFINE\\|IMPL\\|.*INTERFACE_MAP\\)_.*\\|NS_INTERFACE_TABLE.*\\|NS_.*ENTRY$"
let ns_ty_func_pat = Str.regexp "^NS_\\(IMETHOD\\).*$"

let callback_or_tailed_pat = Str.regexp "^.*\\(CALLBACK\\|TAILED\\)$"

let is_ns_block_head_macro s = Str.string_match ns_block_head_pat s 0

let is_ns_block_end_macro s =
  Str.string_match ns_block_end_pat s 0 && not (Xstring.endswith s "CALLBACK") && not (Xstring.endswith s "TAILED")
  (*Str.string_match ns_block_end_pat s 0 && not (Str.string_match callback_or_tailed_pat s 0)*)

let is_ns_decl_macro_func s =
  let b =
    match s with
    | "NS_ERROR_SIGNED_JAR_MODIFIED_ENTRY" -> false
    | "NS_ERROR_SIGNED_JAR_UNSIGNED_ENTRY" -> false
    | _ ->
        Str.string_match ns_decl_func_pat s 0 &&
        not (is_ns_block_end_macro s)
  in
  DEBUG_MSG "s=%s b=%B" s b;
  b
let is_ns_ty_macro_func s = Str.string_match ns_ty_func_pat s 0

let mock_decl_func_pat = Str.regexp "^MOCK_\\(CONST_\\)?METHOD[0-9]$"
let moz_decl_func_pat = Str.regexp "^MOZ_DECLARE_.+$"

let google_attr_pat = Str.regexp "^\\(GOOGLE\\|GTEST\\)\\(_[A-Z]+\\)?_ATTRIBUTE_.+$"

(* pixel shader asm (target of MS HLSL Shader Compiler) *)
let ps_ver_pat = Str.regexp "^\\(vs\\|ps\\|gs\\)_[0-9]_[0-9x]$"


let dummy_lexpos = Lexing.dummy_pos

exception To_be_recovered
exception Continue

let get_common_prefix_rev ?(lbrace_only=false) ?(rparen_only=false) l1 l2 =
  let prefix = ref [] in
  let eq =
    if lbrace_only then
      fun x y -> x == T.LBRACE && x == y
    else if rparen_only then
      fun x y -> (x == T.LBRACE || x == T.RPAREN || is_semicolon x) && x == y
    else
      (=)
  in
  begin
    try
      List.iter2
        (fun x1 x2 ->
          if eq x1 x2 then
            prefix := x1 :: !prefix
          else
            raise Exit
        ) l1 l2
    with
      Exit -> ()
  end;
  !prefix

let insert_after_nth_token self ?(to_be_discarded=[]) n tl =
  DEBUG_MSG "inserting %s after %d-th token"
    (String.concat ";" (List.map (fun t -> let rt,_,_ = t in Token.rawtoken_to_string rt) tl))
    n;
  let n =
    let rn = self#replay_queue_length in
    if rn > 0 then begin
      let n' = n - rn in
      DEBUG_MSG "n=%d->%d" n n';
      n'
    end
    else
      n
  in
  let l = ref [] in
  for i = 1 to n do
    let t = self#discard_token() in
    if not (List.mem i to_be_discarded) then
      l := t::!l
  done;
  List.iter self#prepend_token (List.rev tl);
  List.iter self#prepend_token !l

let remove_pp_head =
  let is_pp rt =
    match (rt : T.token) with
    | PP_IF | PP_IFDEF | PP_IFNDEF -> true
    | x -> is_pp_control_line x
  in
  let rec skip = function
    | [] -> []
    | T.NEWLINE::x::tl when is_pp x -> skip tl
    | T.NEWLINE::tl -> tl
    | _::tl -> skip tl
  in
  function
    | [] -> []
    | (x::tl as l) -> if is_pp x then skip tl else l

let skip_pp_l =
  let is_pp rt =
    match (rt : T.token) with
    | PP_IF | PP_IFDEF | PP_IFNDEF -> true
    | x -> is_pp_control_line x
  in
  let rec skip = function
    | [] -> None
    | T.NEWLINE::x::tl when is_pp x -> skip tl
    | T.NEWLINE::x::tl -> DEBUG_MSG "x=%s" (Token.rawtoken_to_string x); Some x
    | _::tl -> skip tl
  in
  skip

let rev_skip_pp_l =
  let is_pp rt =
    match (rt : T.token) with
    | PP_ENDIF -> true
    | x -> is_pp_control_line x
  in
  let rec skip = function
    | [] -> None
    | h::tl when is_pp h -> begin
        match tl with
        | [] -> None
        | NEWLINE::tl1 -> skip tl1
        | x::_ ->
            DEBUG_MSG "x=%s" (Token.rawtoken_to_string x);
            Some x
    end
    | _::tl -> skip tl
  in
  skip


let parse_warning env stp edp (fmt : ('a, out_channel, unit, 'b) format4) : 'a =
  let path = Common.relpath env#current_filename in
  let head = Printf.sprintf "[%s]" path in
  PB.parse_warning ~out:stderr ~head stp edp fmt

let conv_token (env : Aux.env) scanner (token : token) =
  DEBUG_MSG "@";
  let self = (scanner :> c_t) in
  let _rawtok, stp, edp = token in
  let mk x = x, stp, edp in
  (*let mk1 x = (x, stp, stp) in*)
  let _mk x = (x, stp, Astloc.decr_lexpos edp) in
  let mk_ x = (x, Astloc.incr_lexpos stp, edp) in
  let prev_endofs = self#prev_endofs in
  let prev_endln = self#prev_endln in
  let prev_rawtoken = self#prev_rawtoken in
  let prev_rawtoken2 = self#prev_rawtoken2 in
  let prev_rawtoken3 = self#prev_rawtoken3 in
  let prev_rawtoken4 = self#prev_rawtoken4 in
  let context = self#context in
  let sub_context = self#sub_context in
  let keep_flag = self#keep_flag in
  let check_top_stmts_flag = self#check_top_stmts_flag in
  let macro_body_parsing_flag = self#macro_body_parsing_flag in
  let is_type = self#is_type in
  let is_templ = self#is_templ in
  let is_val = self#is_val in
  let _is_val = self#_is_val in
  let is_macro_fun = self#is_macro_fun in
  let check_if_macro_args = self#check_if_macro_args in
  let check_if_noptr_dtor = self#check_if_noptr_dtor in
  let check_if_noptr_dtor_ = self#check_if_noptr_dtor_ in
  (*let is_macro_obj = self#is_macro_obj in*)

  let get_common_suffix ?(lbrace_only=false) ?(rparen_only=false) nth_ l =
    let common_suffix = ref l in
    let nth_list = ref [nth_-1] in
    let sect_end_nth = ref 0 in
    let __mk = ref (fun rt -> rt, dummy_lexpos, dummy_lexpos) in
    let rec scan l nth_ =
      match self#peek_nth_rawtoken nth_ with
      | PP_ELIF | PP_ELSE as x -> begin
          let nth', _ = self#peek_rawtoken_up_to ~from:(nth_+1) [T.NEWLINE] in
          let nth_', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) () ~from:(nth'+1) in
          nth_list := (nth_'-1) :: !nth_list;
          DEBUG_MSG "rev_l'=%s" (String.concat ";" (List.map Token.rawtoken_to_string (List.rev l')));
          let suffix = get_common_prefix_rev ~lbrace_only ~rparen_only l l' in
          DEBUG_MSG "suffix=%s" (String.concat ";" (List.map Token.rawtoken_to_string suffix));
          if List.length !common_suffix > List.length suffix then
            common_suffix := suffix;
          match x with
          | PP_ELIF -> scan l' nth_'
          | PP_ELSE -> begin
              let n, _ = self#peek_rawtoken_up_to ~from:(nth_+1) [T.NEWLINE] in
              let n, _ = self#peek_rawtoken_up_to_group_end ~limit:(-1) () ~from:(n+1) in
              match self#peek_nth n with
              | PP_ENDIF, sp, ep -> begin
                  let n, _ = self#peek_rawtoken_up_to ~from:n [T.NEWLINE] in
                  DEBUG_MSG "sect_end_nth=%d" n;
                  sect_end_nth := n;
                  __mk := (fun rt -> rt, sp, ep)
              end
              | _ -> ()
          end
          | _ -> ()
      end
      | _ -> ()
    in
    scan l nth_;
    !common_suffix, !nth_list, !sect_end_nth, !__mk
  in

  let is_bound x =
    try
      let _ = env#lookup_obj (Pinfo.encode_ident x) in
      true
    with
      Not_found -> false
  in
  let conv_next_n_tokens conv n =
    DEBUG_MSG "converting next %d tokens" n;
    let l = ref [] in
    for i = 1 to n do
      let _rt,_,_ as _t = self#discard_token() in
      let rt,_,_ as t = conv _t in
      DEBUG_MSG "%d: %s -> %s" i (Token.rawtoken_to_string _rt) (Token.rawtoken_to_string rt);
      l := t::!l
    done;
    List.iter self#prepend_token !l
  in
  let conv_nth_token conv n =
    DEBUG_MSG "converting %d-th token" n;
    let l = ref [] in
    for i = 1 to n - 1 do
      let t = self#discard_token() in
      l := t::!l
    done;
    let _rt,_,_ as _t = self#discard_token() in
    let rt,_,_ as t = conv _t in
    DEBUG_MSG "%s -> %s" (Token.rawtoken_to_string _rt) (Token.rawtoken_to_string rt);
    l := t::!l;
    List.iter self#prepend_token !l
  in
  let insert_after_nth_token = insert_after_nth_token self in
  let check_double_paren from =
    match self#peek_nth_rawtoken from with
    | TY_LPAREN -> begin
        match self#peek_nth_rawtoken (from+1) with
        | TY_LPAREN -> begin
            let _, _, l = self#peek_rawtoken_up_to_rparen ~from ~level:0 None in
            DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
            match l with
            | RPAREN::l0 -> begin
                match (List.rev l0) with
                | TY_LPAREN::TY_LPAREN::l1 -> paren_balanced l1
                | _ -> false
            end
            | _ -> false
        end
        | _ -> false
    end
    | _ -> false
  in

  let skip_pp_control_line ?(from=1) () =
    let nth = ref from in
    try
      while true do
        match self#peek_nth_rawtoken !nth with
        | PP_DEFINE | PP_UNDEF | PP_LINE | PP_ERROR | PP_UNKNOWN _ | PP_
        | PP_INCLUDE | PP_IMPORT | PP_PRAGMA -> begin
            let n, _ = self#peek_rawtoken_up_to ~from:(!nth) [T.NEWLINE] in
            nth := n + 1
        end
        | _ -> raise Exit
      done;
      0
    with
      Exit ->
        if !nth > from then
          !nth - 1
        else
          !nth
  in

  let check_if_arg_macro () =
    (self#peek_rawtoken() != TY_LPAREN ||
    match self#peek_nth_rawtoken 2 with
    | IDENT _ -> begin
        match self#peek_nth_rawtoken 3 with
        | RPAREN -> begin
            match self#peek_nth_rawtoken 4 with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 5 with
                | COMMA | RPAREN -> true
                | _ -> false
            end
            | _ -> false
        end
        | _ -> false
    end
    | DOT | MINUS_GT -> true
    | _ -> false) &&
    env#paren_level > 0 &&
    match prev_rawtoken with
    | ARG_MACRO _ -> begin
        match self#peek_nth_rawtoken 2 with
        | RPAREN -> begin
            conv_nth_token (function T.IDENT x,s,e -> T.ARG_MACRO x,s,e | x -> x) 1;
            false
        end
        | _ -> false
    end
    | COMMA when begin
        match self#peek_nth_rawtoken 2 with
        | DOT | MINUS_GT -> true
        | _ -> false
    end -> true
    | COMMA -> begin
        begin
          match self#peek_nth_rawtoken 2 with
          | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
              let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in
              match self#peek_nth_rawtoken (nth+1) with
              | COMMA -> begin
                  conv_nth_token (function T.IDENT x,s,e -> T.ARG_MACRO x,s,e | x -> x) 1
              end
              | _ -> ()
          end
          | _ -> ()
        end;
        false
    end
    | LPAREN -> begin
        match prev_rawtoken2 with
        | EOF | COMMA -> false
        | FOR | WHILE | IF -> false
        | IDENT _ -> false
        | _ ->
            match self#peek_nth_rawtoken 2 with
            | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ -> false
            | EQ -> false
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 3 with
                | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ -> false
                | _ -> true
            end
            | _ -> true
    end
    | _ -> false
  in

  (* e.g. @ ( * ) ( param_decl, ..., param_decl) *)
  let check_if_abst_dtor_ident ?(nth=2) () =
    (*match self#peek_rawtoken() with
    | TY_LPAREN when begin*)
        match self#peek_nth_rawtoken nth with
        | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
            match self#peek_nth_rawtoken (nth+1) with
            | RPAREN -> true
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | RPAREN -> self#peek_nth_rawtoken (nth+3) == TY_LPAREN
                | _ -> false
            end
            | _ -> false
        end
        | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ -> begin
            match self#peek_nth_rawtoken (nth+1) with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | RPAREN -> true
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | RPAREN -> self#peek_nth_rawtoken (nth+4) == TY_LPAREN
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end
        | _ -> false
    (*end
    | _ -> false*)
  in

  let is_ty = self#is_ty in
  (*let check_if_param = self#check_if_param in*)
  let check_if_params = self#check_if_params in

  let conv_ident s =
    DEBUG_MSG "@";
    match context, sub_context with
    | EXPR, _ | _, IN_EXPR -> begin
        DEBUG_MSG "@";
        (*DEBUG_MSG "next: %s" (Token.rawtoken_to_string (self#peek_rawtoken()));*)
        match prev_rawtoken with
        | STR_LITERAL _ | USER_STR_LITERAL _ when begin
            self#peek_rawtoken() != TY_LPAREN
        end -> DEBUG_MSG "(STR_LITERAL|USER_STR_LITERAL) @"; mk (T.STR_MACRO s)
        | _ ->
        match self#peek_rawtoken() with
        | COLON_COLON -> DEBUG_MSG "@ COLON_COLON"; token

        | LBRACE when prev_rawtoken == LAM_LBRACKET -> DEBUG_MSG "@ LBRACE"; mk (T.IDENT_V s)

        | LBRACE when env#lambda_dtor_flag && prev_rawtoken == RPAREN
          -> DEBUG_MSG "@ LBRACE"; mk (T.ATTR_MACRO s)

        | LBRACE when begin
            not env#dtor_flag &&
            not env#lambda_intro_flag &&
            not env#end_of_id_macro_call_flag &&
            match prev_rawtoken with
            | IDENT _ -> false
            | x when is_ty x -> false
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                match prev_rawtoken2 with
                | IDENT _ -> true
                | x when is_ty x -> true
                | _ -> false
            end -> false
            | _ -> true
        end -> begin
            DEBUG_MSG "@ LBRACE";
            if not env#trailing_retty_flag then
              conv_nth_token (function T.LBRACE,s,e -> T.INI_LBRACE,s,e | x -> x) 1;
            token
          end

        | EQ_EQ | EXCLAM_EQ _ -> DEBUG_MSG "@ (EQ_EQ|EXCLAM_EQ)"; mk (T.IDENT_V s)

        | PLUS | MINUS | PTR_STAR | SLASH | PERC | COMMA | TEMPL_LT | TY_TEMPL_GT when begin
            match prev_rawtoken with
            | INT_LITERAL _ | FLOAT_LITERAL _ -> true
            | _ -> false
        end -> DEBUG_MSG "(INT_LITERAL|FLOAT_LITERAL) @ (PLUS|...)"; mk (T.SUFFIX_MACRO s)

        | IDENT _ | TY_LPAREN when begin
            env#expr_flag && not env#in_objc_message_expr
        end -> begin
          DEBUG_MSG "@ (IDENT|TY_LPAREN)";
          (*match self#peek_rawtoken() with
          | TY_LPAREN -> mk (T.IDENT_OM s)
          | _ -> *)mk (T.OP_MACRO s)
        end

        | IDENT _ when begin
            match prev_rawtoken with
            | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA | LBRACKET -> true
                | _ -> false
            end
            | COMMA when env#arg_paren_flag -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA -> true
                | RPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | SEMICOLON _ -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.OP_MACRO s)

        | IDENT _ when env#decltype_flag && not env#in_objc_message_expr -> begin
            DEBUG_MSG "@ IDENT";
            match prev_rawtoken with
            | COLON_COLON -> begin
                DEBUG_MSG "@"; 
                conv_nth_token (function T.IDENT x,s,e -> T.ARGS_MACRO x,s,e | x -> x) 1;
                mk (T.IDENT_IM s)
            end
            | TEMPL_LT when env#cast_key_flag -> DEBUG_MSG "@ IDENT"; mk (T.CV_MACRO s)

            | _ -> DEBUG_MSG "@ IDENT"; mk (T.OP_MACRO s)
        end

        | IDENT x when begin
            context == EXPR &&
            match prev_rawtoken with
            | LPAREN when begin
                is_op (self#peek_nth_rawtoken 2) &&
                match self#peek_nth_rawtoken 3 with
                | IDENT y -> y = x
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.OP_MACRO s)

        | IDENT _ when begin
            match prev_rawtoken with
            | MINUS_GT | DOT | AMP _ | BAR _ | EXCLAM _ | STAR | SLASH | PERC | PLUS | MINUS
            | GT_GT | LT_LT -> true
            | COLON when env#in_objc_message_expr -> true
            | RETURN -> true
            | LPAREN -> begin
                match prev_rawtoken2 with
                | IF | SWITCH | WHILE | IDENT_V _ | IDENT _ -> false
                | _ ->
                    match self#peek_rawtoken() with
                    | COLON_COLON | TEMPL_LT -> false
                    | _ -> true
            end
            | COMMA when env#paren_level = 1 && not env#type_paren_flag && begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 ()
                    in
                    match self#peek_nth_rawtoken (nth+1) with
                    | RPAREN | COMMA -> begin
                        conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AM x,s,e | x -> x) 1;
                        true
                    end
                    | _ -> false
                end
                | _ -> false
            end -> true
            | RPAREN when env#end_of_cast_type_flag -> begin
                match self#peek_rawtoken() with
                | COLON_COLON | TEMPL_LT -> false
                | _ -> true
            end
            | RPAREN when env#in_objc_message_expr -> true
            | OBJC_LBRACKET when env#in_objc_message_expr -> true
            | ARG_MACRO _ -> begin
                env#paren_level > 0 && env#arg_paren_flag &&
                match self#peek_nth_rawtoken 2 with
                | RPAREN -> begin
                    conv_nth_token (function T.IDENT x,s,e -> T.ARG_MACRO x,s,e | x -> x) 1;
                    (*self#prepend_token (mk T.MARKER);*)
                    true
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT _ | IDENT_V _
        | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _ | NULLPTR
        | STR_LITERAL _ | PP_STRINGIZED _ | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
        | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
        | TY_LPAREN when check_if_arg_macro() -> DEBUG_MSG "@ ..."; mk (T.ARG_MACRO s)

        | RPAREN when env#arg_paren_flag && begin
            match prev_rawtoken with
            | IDENT_V _ | THIS -> true
            | _ -> false
        end -> DEBUG_MSG "@ RPAREN"; mk (T.ARG_MACRO s)

        | SEMICOLON _ when begin
            match prev_rawtoken with
            | IDENT_V _ -> true
            | _ -> false
        end -> DEBUG_MSG "@ SEMICOLON"; mk (T.ARGS_MACRO s)

        | IDENT _ when begin
            match prev_rawtoken with
            | LPAREN -> begin
                match prev_rawtoken2 with
                | IF | WHILE | SWITCH -> begin
                    match self#peek_nth_rawtoken 2 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> begin
                            conv_nth_token (function T.IDENT x,s,e -> T.OP_MACRO x,s,e | x -> x) 1;
                            conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 2;
                            true
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT _ | SEMICOLON _ | COMMA as rt when begin
            match prev_rawtoken with
            | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ -> true
            | EQ when is_ident rt -> begin
                match prev_rawtoken2 with
                | RBRACKET -> begin
                    match prev_rawtoken3 with
                    | LBRACKET -> begin
                        match prev_rawtoken4 with
                        | IDENT_V x -> true
                        | RPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | LPAREN when begin
                match prev_rawtoken2 with
                | IF | WHILE | SWITCH -> true
                | _ -> false
            end -> false
            | COMMA | LPAREN when is_ident rt -> begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> true
                | RPAREN -> is_semicolon (self#peek_nth_rawtoken 3)
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ (IDENT|SEMICOLON|COMMA)"; mk (T.STR_MACRO s)

        | COMMA when begin
            env#templ_arg_flag && prev_rawtoken == COLON_COLON && prev_rawtoken2 == TEMPL_GT &&
            not env#last_ty_templ_id_flag
        end -> DEBUG_MSG "@ COMMA"; mk (T.IDENT_V s)

        | IDENT _ when begin
            not env#in_objc_message_expr &&
            match prev_rawtoken with
            | EQ when begin
                match self#peek_rawtoken() with
                | IDENT x when begin
                    match self#peek_nth_rawtoken 2 with
                    | IDENT y when is_capital_ident x && not (is_capital_ident y) -> true
                    | _ -> false
                end -> true
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT IDENT IDENT SEMICOLON"; mk (T.IDENT_V s)

        | IDENT _ when begin
            not env#in_objc_message_expr &&
            match prev_rawtoken with
            | LBRACKET -> begin
                match self#peek_nth_rawtoken 2 with
                | RBRACKET -> true
                | _ -> false
            end
            | RBRACKET -> true
            | LPAREN -> begin
                match prev_rawtoken2 with
                | IDENT_V _ -> begin
                    match prev_rawtoken3 with
                    | MINUS_GT | EQ -> true
                    | _ -> false
                end
                | IDENT _ when self#peek_nth_rawtoken 2 == MINUS_GT -> true
                | _ -> false
            end
            | IDENT_V _ -> true
            | EQ when begin
                match self#peek_rawtoken() with
                | IDENT x when is_param_decl_macro x || is_param_decl_macro_ident x -> false
                | IDENT x when begin
                    match self#peek_nth_rawtoken 2 with
                    | IDENT y when is_capital_ident x && not (is_capital_ident y) -> true
                    | _ -> false
                end -> false
                | IDENT _ when begin
                    match prev_rawtoken2 with
                    | IDENT_V x -> x = s
                    | _ -> false
                end -> false
                | _ -> true
            end -> true
            | COMMA when not env#type_paren_flag && self#peek_nth_rawtoken 2 == COMMA -> true
            | SLASH | PERC -> true
            | PLUS | MINUS | STAR -> begin
                match prev_rawtoken2 with
                | LPAREN -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.OP_MACRO s)

        | IDENT _ when prev_rawtoken == TY_LPAREN && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            List.length ll = 1 &&
            match self#peek_nth_rawtoken (nth+1) with
            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
              -> true
            | TY_LPAREN -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
                  -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.ATTR_MACRO s)

        | IDENT _ when env#paren_level > 0 && begin
            match prev_rawtoken with
            | COMMA(* | LPAREN*) when not env#stack#in_params -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT _ | COMMA when env#paren_level > 0 && begin
            env#type_paren_flag && env#init_flag &&
            match prev_rawtoken with
            | EQ | COLON_COLON -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT x when begin
            x = s &&
            not env#macro_arg_flag &&
            not keep_flag &&
            (*is_capital_ident x &&*)
            env#arg_paren_flag &&
            match prev_rawtoken with
            | COMMA | LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.STR_MACRO s)

        | IDENT _ when begin
            DEBUG_MSG "s=%s" s;
            try
              let nd = env#find_resolved_macro s in
              match nd#label with
              | ObjectLikeMacro -> begin
                  match (nd#nth_child 0)#label with
                  | StringLiteral _ -> true
                  | _ -> false
              end
              | _ -> false
            with
              _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.STR_MACRO s)

        | IDENT x when begin
            not env#macro_arg_flag &&
            not keep_flag &&
            (*is_capital_ident x &&*)
            env#arg_paren_flag &&
            match prev_rawtoken with
            | COMMA | LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT x when begin
            not env#macro_arg_flag &&
            not env#type_paren_flag &&
            not keep_flag &&
            (*is_capital_ident x &&*)
            match prev_rawtoken with
            | COLON_COLON | INI_LBRACE -> begin
                match self#peek_nth_rawtoken 2 with
                | RBRACKET | COMMA ->
                    conv_nth_token (function T.IDENT x,s,e -> T.ARGS_MACRO x,s,e | x -> x) 1;
                    true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT x when contains_arg x && env#in_body_brace_flag && begin
            match self#peek_nth_rawtoken 2 with
            | SEMICOLON _ -> begin
              conv_nth_token (function T.IDENT x,s,e -> T.ARGS_MACRO x,s,e | x -> x) 1;
              true
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT x when begin
            env#arg_paren_flag &&
            (is_arg_macro x || is_arg_macro_ident x)
        end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

        | IDENT x when begin
            env#templ_arg_flag &&
            match self#peek_nth_rawtoken 2 with
            | COMMA | TY_TEMPL_GT -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.CV_MACRO s)

        | IDENT _ when prev_rawtoken == LAM_LBRACKET -> DEBUG_MSG "LAM_LBRACKET @ IDENT"; mk (T.IDENT_V s)

        | IDENT _ when begin
            env#arg_paren_flag &&
            prev_rawtoken == COLON_COLON &&
            self#peek_nth_rawtoken 2 == TY_LPAREN &&
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
            match self#peek_nth_rawtoken (nth+1) with
            | COMMA | RPAREN | IDENT _ -> begin
                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AGM x,s,e | x -> x) 1;
                true
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @ IDENT"; mk (T.IDENT_V s)

        | IDENT _ when begin
            prev_rawtoken == EQ &&
            match prev_rawtoken2 with
            | IDENT_V x when x = s -> true
            | _ -> false
        end -> DEBUG_MSG "EQ @ IDENT"; mk (T.IDENT_V s)

        | IDENT _ when begin
            context == EXPR &&
            not env#type_paren_flag &&
            (match prev_rawtoken with
            | AMP_AMP _ | BAR_BAR _ | PLUS | MINUS | STAR | SLASH | PERC
            | COMMA -> true
            | _ -> false) &&
            match self#peek_nth_rawtoken 2 with
            | EOF | COMMA | RPAREN -> begin
                conv_nth_token (function T.IDENT x,s,e -> T.ARGS_MACRO x,s,e | x -> x) 1;
                true
            end
            | _ -> false
        end -> DEBUG_MSG "COMMA @ IDENT"; mk (T.IDENT_V s)

        | IDENT _ when is_literal (self#peek_nth_rawtoken 2) -> DEBUG_MSG "@ IDENT literal"; mk (T.IDENT_V s)

        | IDENT _ | IDENT_V _ when begin
            match prev_rawtoken with
            | OP_MACRO _ -> false
            | _ -> true
        end -> DEBUG_MSG "@ (IDENT|IDENT_V)"; token

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
        | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST
        | TYPE_MACRO _ when begin
            env#templ_arg_flag &&
            match prev_rawtoken with
            | TEMPL_LT | COMMA -> true
            | _ -> false
        end -> DEBUG_MSG "@ (CHAR|...)"; token

        | ELLIPSIS when begin
            match self#peek_nth_rawtoken 2 with
            | IDENT _ | IDENT_V _ -> true
            | _ -> false
        end -> DEBUG_MSG "@ ELLIPSIS (IDENT|IDENT_V)"; token

        | STR_LITERAL _ | PP_STRINGIZED _ -> DEBUG_MSG "@ (STR_LITERAL|PP_STRINGIZED)"; mk (T.STR_MACRO s)

        | PP_IF | PP_IFDEF | PP_IFNDEF when begin
            env#arg_paren_flag &&
            (match prev_rawtoken with
            | IDENT_V _ -> true
            | _ -> false) &&
            let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | COMMA -> true
            | _ -> false
        end -> DEBUG_MSG "@ (PP_IF|PP_IFDEF|PP_IFNDEF)"; mk (T.ARG_MACRO s)

        | PP_IF | PP_IFDEF | PP_IFNDEF when begin
            let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | STR_LITERAL _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | PP_ENDIF -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ (PP_IF|PP_IFDEF|PP_IFNDEF)"; mk (T.STR_MACRO s)

        | NOEXCEPT when begin
            env#macro_arg_flag &&
            prev_rawtoken == LPAREN
        end -> DEBUG_MSG "LPAREN @ NOEXCEPT"; mk (T.ARG_MACRO s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | TY_TILDE | TY_HAT -> true
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; token

        | TY_LPAREN when begin
            match prev_rawtoken with
            | CONST -> true
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_TM s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | DOT | MINUS_GT -> true
            | LPAREN -> begin
                match prev_rawtoken2 with
                | DECLTYPE -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_V s)

        | TEMPL_LT when begin
            match self#peek_nth_rawtoken 2 with
            | DECLTYPE | TY_TEMPL_GT -> true
            | GT_GT when env#templ_param_arg_level > 0 -> true
            | _ when env#typename_flag -> true
            | x when is_basic_ty x && self#peek_nth_rawtoken 3 == TY_TEMPL_GT -> begin
                begin
                  match prev_rawtoken with
                  | CONST -> conv_nth_token (function T.TY_TEMPL_GT,s,e -> T.TY_TEMPL_GT_,s,e | x -> x) 3;
                  | _ -> ()
                end;
                true
            end
            | _ -> false
        end -> DEBUG_MSG "@ TEMPL_LT"; token

        | TEMPL_LT -> begin
            DEBUG_MSG "@ TEMPL_LT";
            match prev_rawtoken with
            | rt when begin
                match rt with
                | MINUS_GT | DOT -> false
                | _ -> is_templ s
            end -> DEBUG_MSG "* @ TEMPL_LT"; token

            | TEMPLATE | TYPENAME | TY_LPAREN -> DEBUG_MSG "(TEMPLATE|TYPENAME|TY_LPAREN) @ TEMPL_LT"; token

            | TEMPL_LT when env#cast_key_flag -> DEBUG_MSG "TEMPL_LT @ TEMPL_LT"; token

            | CONST when begin
                match prev_rawtoken2 with
                | COMMA | TEMPL_LT -> true
                | _ -> false
            end -> begin
              DEBUG_MSG "CONST @ TEMPL_LT";
              if not env#cast_key_flag then
                self#ctx_ty();
              token
            end

            | EQ | RETURN when env#templ_param_arg_level = 0 && not (is_val s) && begin
                let _, _l = self#peek_rawtoken_up_to ~is_target:is_semicolon [T.RBRACE] in
                let l = List.rev _l in
                DEBUG_MSG "l=%s"
                  (String.concat ";" (List.map Token.rawtoken_to_string l));
                match l with
                | TEMPL_LT::
                  (IDENT _|INT_LITERAL _|FLOAT_LITERAL _)::
                  (PTR_AMP_AMP|BAR_BAR _)::
                  (IDENT _|INT_LITERAL _|FLOAT_LITERAL _)::
                  TY_TEMPL_GT::(IDENT _|INT_LITERAL _|FLOAT_LITERAL _)::_ when context == EXPR -> false
                | _ when begin
                    match l with
                    | TEMPL_LT::(INT_LITERAL _)::x::_ when x != COMMA -> true
                    | _ -> false
                end -> false
                | _ when begin
                    context == EXPR &&
                    contained_in_list_f
                      (function
                        | T.TY_TEMPL_GT::IDENT _::_ -> true
                        | _ -> false) l
                end -> false
                (*| _ when contained_in_list [T.TY_TEMPL_GT;TY_LPAREN;RPAREN] l -> true*)
                | _ when templ_param_arg_balanced l -> true
                | _ -> false
            end -> DEBUG_MSG "(EQ|RETURN) @ TEMPL_LT"; token

            | MINUS_GT | DOT | LPAREN | COMMA when begin
                (match prev_rawtoken with
                | LPAREN | COMMA -> not env#decltype_flag
                | _ -> true) &&
                match self#peek_nth_rawtoken 2 with
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST
                | TYPE_MACRO _ -> false
                | TYPENAME | DECLTYPE -> false
                | IDENT x when is_type x || is_type_name x -> false
                | IDENT _ | INT_LITERAL _ | FLOAT_LITERAL _ when self#peek_nth_rawtoken 3 == TY_TEMPL_GT -> false
                | _ ->
                    if env#paren_level > 0 then begin
                      let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                      match List.rev ll with
                      | [x::TY_TEMPL_GT::_] when (env#arg_paren_flag || not env#type_paren_flag) && begin
                          match x with
                          | COLON_COLON -> false
                          | _ -> true
                      end -> true
                      | _l::_ -> begin
                          let l = List.rev _l in
                          DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                          let paren_level = env#paren_level in
                          let get_stack =
                            if paren_level > (try env#templ_param_arg_stack_top with _ -> 0) then
                              fun () -> Stack.create()
                            else
                              env#get_templ_param_arg_stack
                          in
                          not (templ_param_arg_balanced ~stack:(get_stack()) ~paren_level l) &&
                          not (templ_param_arg_balanced ~stack:(get_stack()) ~paren_level ~weak:true l)
                      end
                      | _ -> false
                    end
                    else
                      false
            end -> DEBUG_MSG "(MINUS_GT|DOT|LPAREN|COMMA) @ TEMPL_LT"; mk (T.IDENT_V s)

            | LPAREN when begin
                match self#peek_rawtoken_up_to_rparen (Some T.TY_TEMPL_GT) with
                | true, nth, _ -> begin
                    match self#peek_nth_rawtoken (nth+1) with
                    | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                    | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID
                    | TYPE_MACRO _ -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "LPAREN @ TEMPL_LT"; mk (T.IDENT_V s)

            | PLUS | MINUS | STAR | SLASH | PERC | AMP_AMP _ | BAR_BAR _ when begin
                env#paren_level > 0 && env#templ_param_arg_level = 0 &&
                let chk () =
                  let _, l = self#peek_rawtoken_up_to_rparen_none() in
                  not (templ_param_arg_balanced l)
                in
                let found, nth, l =
                  self#peek_rawtoken_up_to_rparen ~from:2 (Some T.TY_TEMPL_GT)
                in
                if found then begin
                  if List.memq T.TEMPL_LT l then
                    chk()
                  else
                    match self#peek_nth_rawtoken (nth+1) with
                    | TY_LPAREN -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | RPAREN -> false
                        | _ -> chk()
                    end
                    | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                    | INT | LONG | FLOAT | DOUBLE | TYPE_MACRO _ when not env#macro_arg_flag && begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | TY_LPAREN -> true
                        | _ -> false
                    end -> true
                    | _ -> chk()
                end
                else begin
                  not (templ_param_arg_balanced (T.TEMPL_LT::(List.rev l)))
                end
            end -> DEBUG_MSG "... @ TEMPL_LT"; mk (T.IDENT_V s)

            | SEMICOLON _ when env#for_flag && begin
                self#peek_nth_rawtoken 3 == TY_TEMPL_GT &&
                match self#peek_nth_rawtoken 2 with
                | IDENT x when is_type_name x || is_type x -> true
                | x when is_basic_ty x -> true
                | _ -> false
            end -> DEBUG_MSG "SEMICOLON @ TEMPL_LT"; token

            | SEMICOLON _ when env#for_flag -> DEBUG_MSG "SEMICOLON @ TMEPL_LT"; mk (T.IDENT_V s)

            | COMMA | COLON_COLON when begin
                env#paren_level > 0 && env#templ_param_arg_level = 0 &&
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match List.rev ll with
                | l::_ -> begin
                    DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                    match l with
                    | TY_TEMPL_GT::_ -> true
                    | RPAREN::TY_LPAREN::TY_TEMPL_GT::_ -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "(COMMA|COLON_COLON) @ TEMPL_LT"; token

            | AMP _ when begin
                match prev_rawtoken2 with
                | IDENT_V _ | LPAREN -> false
                | x when is_literal x -> false
                | _ when env#in_body_brace_flag && env#paren_level = 0 && begin
                    let filt = function
                      | T.SEMICOLON _ -> true
                      | _ -> false
                    in
                    let l =
                      let _, l = self#peek_rawtoken_up_to_rbrace ~noexcept:true ~filt () in
                      List.rev l
                    in
                    DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                    let paren_level = env#paren_level in
                    not (templ_param_arg_balanced ~stack:(env#get_templ_param_arg_stack()) ~paren_level l)
                end -> false
                | _ -> true
            end -> DEBUG_MSG "AMP @ TEMPL_LT"; token

            (*NG!| PLUS | MINUS | STAR | SLASH | PERC -> token*)
            | _ -> begin
                DEBUG_MSG "* @ TEMPL_LT";
                let templ_unbalanced = ref None in
                let templ_end_nth = ref 0 in
                match self#peek_nth_rawtoken 2 with
                | BOOL | SHORT | INT | LONG | DOUBLE | FLOAT when begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> begin
                        let l =
                          if
                            not env#type_paren_flag &&
                            env#paren_level > 0 &&
                            try
                              env#templ_param_arg_stack_top = env#paren_level
                            with
                              _ -> false
                          then
                            let _, l = self#peek_rawtoken_up_to_rparen_none() in
                            List.rev l
                          else if env#in_body_brace_flag then
                            let filt = function
                              | T.SEMICOLON _ -> true
                              | _ -> false
                            in
                            let _, l = self#peek_rawtoken_up_to_rbrace ~noexcept:true ~filt () in
                            List.rev l
                          else
                            []
                        in
                        DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                        let paren_level = env#paren_level in
                        not (templ_param_arg_balanced ~stack:(env#get_templ_param_arg_stack()) ~paren_level l)
                    end
                    | _ -> false
                end -> DEBUG_MSG "@ TEMPL_LT ..."; mk (T.IDENT_V s)

                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
                | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST | TY_TEMPL_GT
                | VOLATILE | TYPENAME | TYPE_MACRO _ -> DEBUG_MSG "@ TEMPL_LT ..."; token

                | IDENT _ when begin
                    match self#peek_nth_rawtoken 3 with
                    | HAT _ -> true
                    | _ -> false
                end -> DEBUG_MSG "@ TEMPL_LT IDENT HAT"; token

                | INT_LITERAL _ | FLOAT_LITERAL _ | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                  when begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_TEMPL_GT -> true
                    | _ -> false
                  end -> DEBUG_MSG "@ TEMPL_LT ..."; token

                | _ when begin
                    let n, l =
                      let stack = Stack.create()(*env#get_templ_param_arg_stack*) in
                      let push x = Stack.push x stack in
                      let pop () = try ignore (Stack.pop stack) with _ -> () in
                      let get_top () = try Stack.top stack with _ -> 0 in
                      let get_lv () = Stack.length stack in
                      let plv = ref env#paren_level in
                      let filt rt =
                        match (rt : T.token) with
                        | TY_LPAREN -> incr plv; false
                        | RPAREN -> decr plv; false
                        | TEMPL_LT -> push !plv; false
                        | TY_TEMPL_GT when !plv = get_top() -> pop(); get_lv() = 0
                        | GT_GT when !plv = get_top() -> pop(); pop(); get_lv() = 0
                        | SEMICOLON _ -> true
                        | _ -> false
                      in
                      if env#paren_level = 0 then
                        self#peek_rawtoken_up_to_rbrace ~noexcept:true ~filt ()
                      else
                        let _, n, l =
                          self#peek_rawtoken_up_to_rparen ~filt None
                        in
                        n, l
                    in
                    let rt_n = self#peek_nth_rawtoken n in
                    DEBUG_MSG "rt_n=%s" (Token.rawtoken_to_string rt_n);
                    DEBUG_MSG "%s" (Token.rawtoken_to_string (self#peek_nth_rawtoken (n+1)));
                    let b =
                    match rt_n with
                    | TY_TEMPL_GT | GT_GT when begin
                        match self#peek_nth_rawtoken (n+1) with
                        | COMMA | TY_TEMPL_GT | GT_GT | SEMICOLON _ -> true
                        | _ -> false
                    end -> true

                    | TY_TEMPL_GT | GT_GT | SEMICOLON _ | COMMA | TY_LPAREN | RPAREN -> begin
                        let l = rt_n::l in
                        let l, n =
                          let n1 = n + 1 in
                          let rt_n1 = self#peek_nth_rawtoken n1 in
                          match rt_n1 with
                          | TY_TEMPL_GT | GT_GT -> rt_n1::l, n1
                          | _ -> l, n
                        in
                        templ_end_nth := n;
                        let l = List.rev l in
                        DEBUG_MSG "n=%d l=%s" n (String.concat ";" (List.map Token.rawtoken_to_string l));
                        let paren_level = env#paren_level in
                        let templ_balanced =
                          templ_param_arg_balanced ~stack:(env#get_templ_param_arg_stack()) ~paren_level l
                        in
                        templ_unbalanced := Some (not templ_balanced);
                        templ_balanced &&
                        match self#peek_nth_rawtoken (n+1) with
                        | TY_LPAREN -> begin
                            let nth, ll =
                              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(n+2) ()
                            in
                            match ll with
                            (*| [_] -> false*)
                            | [_] when begin
                                match self#peek_nth_rawtoken (nth+1) with
                                | SIZEOF | IDENT _ | TY_LPAREN -> true
                                | _ -> false
                            end -> false
                            | _ -> true
                        end
                        | IDENT _ when env#type_paren_flag -> begin
                            match self#peek_nth_rawtoken (n+2) with
                            | RPAREN | COMMA -> true
                            | _ -> false
                        end
                        | LBRACE | COLON_COLON -> true
                        | _ when begin
                            match self#peek_nth_rawtoken (n-1) with
                            | x when is_ty x -> true
                            | _ -> false
                        end -> true
                        | _ -> false
                    end
                    | _ -> false
                    in
                    b(* && env#in_body_brace_flag*)
                end -> DEBUG_MSG "@ TEMPL_LT *"; token

                | TY_LPAREN when begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | RPAREN -> begin
                            match self#peek_nth_rawtoken 5 with
                            | PLUS | MINUS | PTR_STAR | SLASH | PERC -> true
                            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
                            | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _
                            | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "@ TEMPL_LT TY_LPAREN"; token

                | IDENT _ | IDENT_V _ -> begin
                    DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V)";
                    let chk_basic_ty x i =
                      is_basic_ty x && begin
                        match self#peek_nth_rawtoken i with
                        | GT_GT -> true
                        | TY_TEMPL_GT -> begin
                            match self#peek_nth_rawtoken (i+1) with
                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                            | TY_TEMPL_GT -> true
                            | _ -> false
                        end
                        | TY_LPAREN -> begin
                            let _, nth, l =
                              self#peek_rawtoken_up_to_rparen ~from:(i+1) None
                            in
                            match self#peek_nth_rawtoken (nth+1) with
                            | GT_GT -> true
                            | _ -> false
                        end
                        | _ -> false
                      end
                    in
                    match self#peek_nth_rawtoken 3 with
                    | COMMA when !templ_unbalanced = Some true
                      -> DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V)"; mk (T.IDENT_V s)

                    | TY_TEMPL_GT | TEMPL_GT | COMMA -> DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V)"; token

                    | GT_GT when env#templ_param_arg_level > 0
                      -> DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) GT_GT"; token

                    | GT_GT -> begin
                        DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) GT_GT";
                        match self#peek_nth_rawtoken 4 with
                        | COMMA | RPAREN | COLON_COLON -> DEBUG_MSG "@"; token
                        | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                        | TY_LPAREN -> begin
                            DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) GT_GT TY_LPAREN";
                            match self#peek_nth_rawtoken 5 with
                            | RPAREN -> DEBUG_MSG "@"; token
                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                        end
                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end

                    | IDENT _ -> begin
                        DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) IDENT";
                        match self#peek_nth_rawtoken 4 with
                        | COLON_COLON -> begin
                            DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) IDENT COLON_COLON";
                            match self#peek_nth_rawtoken 5 with
                            | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> begin
                                match self#peek_nth_rawtoken 6 with
                                | TY_TEMPL_GT | GT_GT | COMMA -> token
                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end
                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                        end
                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end

                    | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> begin
                        DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) (PTR_AMP|PTR_AMP_AMP|PTR_STAR)";
                        match self#peek_nth_rawtoken 4 with
                        | TY_TEMPL_GT | GT_GT | COMMA | LBRACKET -> DEBUG_MSG "@"; token
                        | TY_LPAREN -> begin
                            let _, nth, l =
                              self#peek_rawtoken_up_to_rparen ~from:5 None
                            in
                            match self#peek_nth_rawtoken (nth+1) with
                            | TY_TEMPL_GT -> begin
                                match self#peek_nth_rawtoken (nth+2) with
                                | TY_LPAREN -> begin
                                    match self#peek_nth_rawtoken (nth+3) with
                                    | RPAREN -> token
                                    | PTR_STAR -> begin
                                        match self#peek_nth_rawtoken (nth+4) with
                                        | THIS -> token
                                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                    end
                                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                end
                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end
                            | _ -> mk (T.IDENT_V s)
                        end
                        | x when x != TEMPL_LT && begin
                            match self#peek_nth_rawtoken 5 with
                            | TY_TEMPL_GT -> begin
                                match self#peek_nth_rawtoken 6 with
                                | IDENT _ -> false
                                | MINUS when begin
                                    match self#peek_nth_rawtoken 7 with
                                    | IDENT _ -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | PTR_AMP_AMP | BAR_BAR _ -> true
                                        | _ -> false
                                    end
                                    | _ -> false
                                end -> false
                                | x when is_literal x -> false
                                | _ -> true
                            end
                            | _ -> false
                        end -> DEBUG_MSG "@"; token

                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end

                    | ELLIPSIS -> DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) ELLIPSIS"; token

                    | TEMPL_LT -> begin
                        DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) TEMPL_LT";
                        match self#peek_nth_rawtoken 4 with
                        | CONST | TYPENAME -> token
                        | IDENT _ | IDENT_V _ | BOOL | CHAR | CHAR8_T | CHAR16_T | CHAR32_T
                        | WCHAR_T | FLOAT | DOUBLE | SHORT | INT | LONG | VOID | TYPE_MACRO _ -> begin
                            DEBUG_MSG "@";
                            match self#peek_nth_rawtoken 5 with
                            | ELLIPSIS when not env#fold_paren_flag -> begin
                                match self#peek_nth_rawtoken 6 with
                                | GT_GT -> token
                                | _ -> mk (T.IDENT_V s)
                            end
                            | COMMA when !templ_unbalanced = Some false -> token
                            | TY_TEMPL_GT | TEMPL_GT -> begin
                                DEBUG_MSG "@";
                                let _, l =
                                  self#peek_rawtoken_up_to ~from:6 ~is_target:is_semicolon [T.RBRACE]
                                in
                                if
                                  filt_at_level0 l
                                    (function
                                      | T.TY_TEMPL_GT | T.TEMPL_GT | T.GT_GT -> true
                                      | _ -> false
                                    )
                                then
                                  token
                                else
                                  mk (T.IDENT_V s)
                            end
                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                DEBUG_MSG "@";
                                match self#peek_nth_rawtoken 6 with
                                | GT_GT -> begin
                                    DEBUG_MSG "@";
                                    match self#peek_nth_rawtoken 7 with
                                    | COMMA | RPAREN | COLON_COLON -> token
                                    | TY_LPAREN -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | RPAREN -> token
                                        | _ -> begin
                                            let nth, ll =
                                              self#peek_rawtoken_up_to_rparen_split_at_comma
                                                ~from:8 ()
                                            in
                                            match ll with
                                            | [_] -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                            | _ -> DEBUG_MSG "@"; token
                                        end
                                    end
                                    | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                end
                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end
                            | GT_GT -> begin
                                DEBUG_MSG "@";
                                match self#peek_nth_rawtoken 6 with
                                | COMMA | RPAREN | COLON_COLON
                                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> DEBUG_MSG "@"; token
                                | TY_LPAREN -> begin
                                    match self#peek_nth_rawtoken 7 with
                                    | RPAREN ->  DEBUG_MSG "@"; token
                                    | _ -> begin
                                        let nth, ll =
                                          self#peek_rawtoken_up_to_rparen_split_at_comma
                                            ~from:7 ()
                                        in
                                        match ll with
                                        | [_] ->  DEBUG_MSG "@"; mk (T.IDENT_V s)
                                        | _ -> DEBUG_MSG "@"; token
                                    end
                                end
                                | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end
                            | COLON_COLON -> begin
                                DEBUG_MSG "@";
                                match self#peek_nth_rawtoken 6 with
                                | IDENT _ -> begin
                                    match self#peek_nth_rawtoken 7 with
                                    | TY_TEMPL_GT -> token
                                    | GT_GT -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | TY_TEMPL_GT -> token
                                        | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                            match self#peek_nth_rawtoken 9 with
                                            | IDENT _ -> begin
                                                match self#peek_nth_rawtoken 10 with
                                                | RPAREN -> token
                                                | _ -> mk (T.IDENT_V s)
                                            end
                                            | _ -> mk (T.IDENT_V s)
                                        end
                                        | _ -> mk (T.IDENT_V s)
                                    end
                                    | TEMPL_LT -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | IDENT _ -> begin
                                            match self#peek_nth_rawtoken 9 with
                                            | TY_TEMPL_GT | COMMA -> token
                                            | GT_GT -> begin
                                                match self#peek_nth_rawtoken 10 with
                                                | COMMA | RPAREN | COLON_COLON
                                                | TY_TEMPL_GT -> token
                                                | TY_LPAREN -> begin
                                                    match self#peek_nth_rawtoken 11 with
                                                    | RPAREN -> token
                                                    | _ -> mk (T.IDENT_V s)
                                                end
                                                | _ -> mk (T.IDENT_V s)
                                            end
                                            | _ -> mk (T.IDENT_V s)
                                        end
                                        | x when chk_basic_ty x 9 -> token
                                        | _ -> mk (T.IDENT_V s)
                                    end
                                    | _ -> mk (T.IDENT_V s)
                                end
                                | _ -> mk (T.IDENT_V s)
                            end
                            | TEMPL_LT -> begin
                                DEBUG_MSG "@ TEMPL_LT (IDENT|IDENT_V) TEMPL_LT . TEMPL_LT";
                                match self#peek_nth_rawtoken 6 with
                                | IDENT _ -> begin
                                    match self#peek_nth_rawtoken 7 with
                                    | ELLIPSIS -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | GT_GT -> token
                                        | TY_TEMPL_GT -> token
                                        | _ -> mk (T.IDENT_V s)
                                    end
                                    | TY_TEMPL_GT | COMMA -> token
                                    | GT_GT -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | COMMA | RPAREN | COLON_COLON | TY_TEMPL_GT -> token
                                        | TY_LPAREN -> begin
                                            match self#peek_nth_rawtoken 9 with
                                            | RPAREN -> token
                                            | _ -> mk (T.IDENT_V s)
                                        end
                                        | _ -> mk (T.IDENT_V s)
                                    end
                                    | COLON_COLON -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | IDENT _ -> begin
                                            match self#peek_nth_rawtoken 9 with
                                            | GT_GT -> begin
                                                match self#peek_nth_rawtoken 10 with
                                                | TY_TEMPL_GT -> token
                                                | _ -> mk (T.IDENT_V s)
                                            end
                                            | _ -> mk (T.IDENT_V s)
                                        end
                                        | _ -> mk (T.IDENT_V s)
                                    end
                                    | TEMPL_LT -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | IDENT _ -> begin
                                            match self#peek_nth_rawtoken 9 with
                                            | TY_TEMPL_GT -> begin
                                                match self#peek_nth_rawtoken 10 with
                                                | GT_GT -> token
                                                | _ -> mk (T.IDENT_V s)
                                            end
                                            | TEMPL_LT -> begin
                                                match self#peek_nth_rawtoken 10 with
                                                | IDENT _ -> begin
                                                    match self#peek_nth_rawtoken 11 with
                                                    | TY_TEMPL_GT -> begin
                                                        match self#peek_nth_rawtoken 12 with
                                                        | GT_GT -> token
                                                        | _ -> mk (T.IDENT_V s)
                                                    end
                                                    | _ -> mk (T.IDENT_V s)
                                                end
                                                | _ -> mk (T.IDENT_V s)
                                            end
                                            | _ -> mk (T.IDENT_V s)
                                        end
                                        | x when chk_basic_ty x 9 -> token
                                        | _ -> mk (T.IDENT_V s)
                                    end
                                    | _ -> mk (T.IDENT_V s)
                                end
                                | x when chk_basic_ty x 7 -> token
                                | _ -> mk (T.IDENT_V s)
                            end
                            | _ -> mk (T.IDENT_V s)
                        end
                        | _ -> begin
                            DEBUG_MSG "@";
                            match self#peek_nth_rawtoken 5 with
                            | TY_TEMPL_GT -> begin
                                match self#peek_nth_rawtoken 6 with
                                | COMMA -> token
                                | _ -> mk (T.IDENT_V s)
                            end
                            | _ -> mk (T.IDENT_V s)
                        end
                        (*| _ -> mk (T.IDENT_V s)*)
                    end

                    | COLON_COLON -> begin
                        DEBUG_MSG "* @ TEMPL_LT IDENT COLON_COLON";
                        match self#peek_nth_rawtoken 4 with
                        | IDENT _ -> begin
                            DEBUG_MSG "@";
                            match self#peek_nth_rawtoken 5 with
                            | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                            | LBRACKET when self#peek_nth_rawtoken 6 == RBRACKET -> DEBUG_MSG "@"; token
                            | GT_GT when begin
                                match self#peek_nth_rawtoken 6 with
                                | TY_TEMPL_GT | COMMA -> true
                                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                    match self#peek_nth_rawtoken 7 with
                                    | IDENT _ -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | RPAREN -> true
                                        | _ -> false
                                    end
                                    | _ -> false
                                end
                                | _ -> false
                            end -> DEBUG_MSG "@"; token
                            | TEMPL_LT -> begin
                                DEBUG_MSG "@";
                                match self#peek_nth_rawtoken 6 with
                                | TYPENAME -> begin
                                    let nth, ll =
                                      self#peek_rawtoken_up_to_rparen_split_at_comma
                                        ~from:7 ()
                                    in
                                    match List.rev ll with
                                    | (RPAREN::TY_LPAREN::_)::_ -> token
                                    | _ -> mk (T.IDENT_V s)
                                end
                                | DECLTYPE -> begin
                                    DEBUG_MSG "@";
                                    let _, nth, l =
                                      self#peek_rawtoken_up_to_rparen ~from:8 None
                                    in
                                    match self#peek_nth_rawtoken (nth+1) with
                                    | GT_GT -> token
                                    | TY_TEMPL_GT when self#peek_nth_rawtoken (nth+2) == COMMA -> token
                                    | _ -> mk (T.IDENT_V s)
                                end
                                | IDENT x -> begin
                                    DEBUG_MSG "@";
                                    match self#peek_nth_rawtoken 7 with
                                    | ELLIPSIS -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | GT_GT -> token
                                        | TY_TEMPL_GT -> token
                                        | _ -> mk (T.IDENT_V s)
                                    end

                                    | TEMPL_LT -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | IDENT _ -> begin
                                            match self#peek_nth_rawtoken 9 with
                                            | TY_TEMPL_GT | COMMA -> token
                                            | GT_GT -> begin
                                                match self#peek_nth_rawtoken 10 with
                                                | COMMA | RPAREN | COLON_COLON -> token
                                                | TY_LPAREN -> begin
                                                    match self#peek_nth_rawtoken 11 with
                                                    | RPAREN -> DEBUG_MSG "@"; token
                                                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                                end
                                                | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                            end
                                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                        end
                                        | x when chk_basic_ty x 9 -> DEBUG_MSG "@"; token
                                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                    end

                                    | TY_TEMPL_GT | COMMA when begin
                                        match !templ_unbalanced with
                                        | Some b -> b
                                        | _ -> false
                                    end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

                                    | TY_TEMPL_GT | COMMA when not (is_type_name x || is_type x)
                                      -> DEBUG_MSG "@"; token

                                    | TY_TEMPL_GT when begin
                                        self#peek_nth_rawtoken 8 == COMMA &&
                                        !templ_unbalanced = Some false
                                    end -> DEBUG_MSG "@"; token

                                    | COMMA when begin
                                        is_type x &&
                                        !templ_unbalanced = Some false &&
                                        !templ_end_nth > 0 &&
                                        match self#peek_nth_rawtoken (!templ_end_nth+1) with
                                        | COMMA | RPAREN -> true
                                        | TY_LPAREN when env#decltype_flag -> true
                                        | _ when begin
                                            match self#peek_nth_rawtoken (!templ_end_nth-1) with
                                            | ELLIPSIS | ELLIPSIS_ -> true
                                            | _ -> false
                                        end -> true
                                        | _ -> false
                                    end -> DEBUG_MSG "@"; token

                                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                        match self#peek_nth_rawtoken 8 with
                                        | GT_GT -> begin
                                            DEBUG_MSG "@";
                                            match self#peek_nth_rawtoken 9 with
                                            | GT_GT -> token
                                            | COMMA | RPAREN | COLON_COLON | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                                            | ELLIPSIS when not env#fold_paren_flag -> DEBUG_MSG "@"; token
                                            | TY_LPAREN -> begin
                                                DEBUG_MSG "@";
                                                match self#peek_nth_rawtoken 10 with
                                                | RPAREN -> DEBUG_MSG "@"; token
                                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                            end
                                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                        end
                                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                    end

                                    | GT_GT -> begin
                                        DEBUG_MSG "@";
                                        match self#peek_nth_rawtoken 8 with
                                        | GT_GT -> token
                                        | COMMA | RPAREN | COLON_COLON | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                                        | ELLIPSIS when not env#fold_paren_flag -> DEBUG_MSG "@"; token
                                        | TY_LPAREN -> begin
                                            DEBUG_MSG "@";
                                            match self#peek_nth_rawtoken 9 with
                                            | RPAREN -> token
                                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                        end
                                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                    end

                                    | COLON_COLON -> begin
                                        DEBUG_MSG "@";
                                        match self#peek_nth_rawtoken 8 with
                                        | IDENT _ -> begin
                                            DEBUG_MSG "@";
                                            match self#peek_nth_rawtoken 9 with
                                            | TY_TEMPL_GT | COMMA -> DEBUG_MSG "@"; token
                                            | GT_GT -> begin
                                                DEBUG_MSG "@";
                                                match self#peek_nth_rawtoken 10 with
                                                | COMMA | RPAREN | COLON_COLON -> DEBUG_MSG "@"; token
                                                | TY_LPAREN -> begin
                                                    DEBUG_MSG "@";
                                                    match self#peek_nth_rawtoken 11 with
                                                    | RPAREN -> DEBUG_MSG "@"; token
                                                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                                end
                                                | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                            end
                                            | TEMPL_LT -> begin
                                                DEBUG_MSG "@";
                                                match self#peek_nth_rawtoken 10 with
                                                | IDENT _ -> begin
                                                    DEBUG_MSG "@";
                                                    match self#peek_nth_rawtoken 11 with
                                                    | TY_TEMPL_GT | COMMA -> DEBUG_MSG "@"; token
                                                    | GT_GT -> begin
                                                        DEBUG_MSG "@";
                                                        match self#peek_nth_rawtoken 12 with
                                                        | COMMA | RPAREN | COLON_COLON -> DEBUG_MSG "@"; token

                                                        | TY_LPAREN -> begin
                                                            DEBUG_MSG "@";
                                                            match self#peek_nth_rawtoken 13 with
                                                            | RPAREN -> DEBUG_MSG "@"; token

                                                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                                        end
                                                        | TY_TEMPL_GT -> DEBUG_MSG "@"; token

                                                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                                    end
                                                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                                end
                                                | x when chk_basic_ty x 11 -> DEBUG_MSG "@"; token

                                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                            end
                                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                        end
                                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                    end
                                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                end
                                | x when chk_basic_ty x 7 -> DEBUG_MSG "@"; token

                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end

                            | COMMA when begin
                                DEBUG_MSG "@";
                                env#templ_param_arg_level = 0 &&
                                env#paren_level > 0 && begin
                                  let found, _, l =
                                    self#peek_rawtoken_up_to_rparen (Some T.TY_TEMPL_GT)
                                  in
                                  not found
                                end
                            end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

                            | COMMA when begin
                                env#templ_param_arg_level = 0 &&
                                env#paren_level > 0 && begin
                                  let found, _, l =
                                    self#peek_rawtoken_up_to_rparen (Some T.TY_TEMPL_GT)
                                  in
                                  found && templ_param_arg_balanced (T.TY_TEMPL_GT::l)
                                end
                            end -> DEBUG_MSG "@"; token

                            | COMMA when env#paren_level = 0 -> DEBUG_MSG "@"; token

                            | COMMA when env#paren_level > 0 && begin
                                let _, l = self#peek_rawtoken_up_to_rparen_none() in
                                match l with
                                | TY_TEMPL_GT::_ -> true
                                | _ -> false
                            end -> DEBUG_MSG "@"; token

                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                DEBUG_MSG "@";
                                match self#peek_nth_rawtoken 6 with
                                | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end

                            | COLON_COLON -> begin
                                DEBUG_MSG "@";
                                match self#peek_nth_rawtoken 6 with
                                | IDENT _ -> begin
                                    DEBUG_MSG "@";
                                    match self#peek_nth_rawtoken 7 with
                                    | TY_TEMPL_GT -> token
                                    | TEMPL_LT -> begin
                                        DEBUG_MSG "@";
                                        match self#peek_nth_rawtoken 8 with
                                        | TYPENAME -> DEBUG_MSG "@"; token
                                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                    end
                                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                                end
                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end
                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                        end
                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end

                    | TY_LPAREN -> begin
                        DEBUG_MSG "* @ TEMPL_LT (IDENT|IDENT_V) TY_LPAREN";
                        let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:4 None in
                        match self#peek_nth_rawtoken (nth+1) with
                        | TY_TEMPL_GT -> begin
                            match self#peek_nth_rawtoken (nth+2) with
                            | TY_TEMPL_GT -> DEBUG_MSG "@"; token
                            | TY_LPAREN -> begin
                                match self#peek_nth_rawtoken (nth+3) with
                                | RPAREN -> DEBUG_MSG "@"; token
                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end
                            | COMMA -> DEBUG_MSG "@"; token
                            | COLON_COLON -> DEBUG_MSG "@"; token
                            | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                        end
                        | TY_LPAREN -> DEBUG_MSG "@"; token
                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end

                    (*| _ when !templ_unbalanced = Some false -> DEBUG_MSG "@"; token*)

                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                end

                | COLON_COLON -> begin
                    DEBUG_MSG "@ TEMPL_LT COLON_COLON";
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | COLON_COLON -> begin
                            match self#peek_nth_rawtoken 5 with
                            | IDENT _ -> begin
                                match self#peek_nth_rawtoken 6 with
                                | TY_TEMPL_GT | COMMA -> token
                                | TEMPL_LT -> token
                                | _ -> mk (T.IDENT_V s)
                            end
                            | _ -> mk (T.IDENT_V s)
                        end
                        | _ -> mk (T.IDENT_V s)
                    end
                    | _ -> mk (T.IDENT_V s)
                end
                | SIZEOF -> begin
                    DEBUG_MSG "@ TEMPL_LT SIZEOF";
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken 5 with
                            | RPAREN -> begin
                                match self#peek_nth_rawtoken 6 with
                                | TY_TEMPL_GT -> token
                                | _ -> mk (T.IDENT_V s)
                            end
                            | _ -> mk (T.IDENT_V s)
                        end
                        | _ -> mk (T.IDENT_V s)
                    end
                    | ELLIPSIS -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_LPAREN -> begin
                            match self#peek_nth_rawtoken 5 with
                            | IDENT _ -> begin
                                match self#peek_nth_rawtoken 6 with
                                | RPAREN -> begin
                                    match self#peek_nth_rawtoken 7 with
                                    | TY_TEMPL_GT -> token
                                    | PLUS | MINUS | PTR_STAR | SLASH | PERC -> begin
                                        match self#peek_nth_rawtoken 9 with
                                        | TY_TEMPL_GT -> token
                                        | _ -> mk (T.IDENT_V s)
                                    end
                                    | _ -> mk (T.IDENT_V s)
                                end
                                | _ -> mk (T.IDENT_V s)
                            end
                            | _ -> mk (T.IDENT_V s)
                        end
                        | _ -> mk (T.IDENT_V s)
                    end
                    | _ -> mk (T.IDENT_V s)
                end

                | BOOL_LITERAL _ | STR_LITERAL _ | USER_STR_LITERAL _ -> DEBUG_MSG "@"; token

                | x when is_literal x && begin
                    match self#peek_nth_rawtoken 3 with
                    | COMMA -> begin
                        match self#peek_nth_rawtoken 4 with
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken 5 with
                            | ELLIPSIS -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | TY_TEMPL_GT -> true
                    | _ -> false
                end -> DEBUG_MSG "@"; token

                | _ when env#braced_init_flag && begin
                    let nth, _l = self#peek_rawtoken_up_to [T.RBRACE] in
                    let l = List.rev _l in
                    templ_param_arg_balanced l
                end -> DEBUG_MSG "@"; token

                | TY_LPAREN when begin (* @ TEMPL_LT TY_LPAREN *)
                    let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:3 None in
                    DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                    match self#peek_nth_rawtoken (nth+1) with
                    | TY_TEMPL_GT -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | TY_LPAREN -> begin
                            match self#peek_nth_rawtoken (nth+3) with
                            | RPAREN -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "@"; token

                | _ when env#paren_level > 0 && prev_rawtoken != LPAREN && begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma()
                    in
                    match List.rev ll with
                    | l::_ -> begin
                        DEBUG_MSG "%s"
                          (String.concat ";" (List.map Token.rawtoken_to_string l));
                        match l with
                        | RPAREN::TY_LPAREN::TY_TEMPL_GT::_ -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "@"; token

                | _ when prev_rawtoken == LPAREN && begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma()
                    in
                    match List.rev ll with
                    | l::_ -> begin
                        DEBUG_MSG "%s"
                          (String.concat ";" (List.map Token.rawtoken_to_string l));
                        templ_param_arg_balanced l &&
                        match l with
                        | RPAREN::TY_LPAREN::TY_TEMPL_GT::_ -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "@"; token

                | _ when begin
                  let nth, _l =
                    self#peek_rawtoken_up_to ~is_target:is_semicolon [T.LBRACE;RBRACE;NEWLINE]
                  in
                  match (_l : T.token list) with
                  | RPAREN::TY_LPAREN::TY_TEMPL_GT::_ -> begin
                      let l = List.rev _l in
                      DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                      templ_param_arg_balanced l
                  end
                  | _ -> false
                end -> DEBUG_MSG "@"; token

                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
            end
        end
        | TY_TEMPL_GT | TEMPL_GT -> begin
            DEBUG_MSG "@ (TY_TEMPL_GT|TEMPL_GT)";
            match prev_rawtoken with
            | LPAREN | RETURN | COLON | TEMPLATE -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | COLON_COLON when env#typename_flag -> DEBUG_MSG "@"; token

            | COLON_COLON when env#cast_key_flag -> DEBUG_MSG "@"; token

            | COLON_COLON when prev_rawtoken2 == TEMPL_GT && not env#last_ty_templ_id_flag
              -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | COLON_COLON when begin
                match prev_rawtoken2 with
                | IDENT _ -> begin
                    match prev_rawtoken3 with
                    | COLON -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | PLUS | MINUS | STAR | SLASH | PERC  | TILDE _ | EXCLAM _ | GT | LT | AMP _ | BAR _
            | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ | AMP_AMP _ | BAR_BAR _ when begin
                (*env#templ_param_arg_level = 0*)
                match self#peek_rawtoken() with
                | COLON_COLON -> false
                | TEMPL_LT when env#templ_param_arg_level > 0 -> false
                | _ -> true
            end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | MINUS_GT | DOT when not env#type_paren_flag -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | CONST when env#templ_arg_flag && begin
                match self#peek_rawtoken() with
                | TY_TEMPL_GT -> true
                | _ -> false
            end -> DEBUG_MSG "@"; token

            | RPAREN when env#end_of_cast_type_flag -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | _ -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN when not (is_semicolon prev_rawtoken) && env#templ_param_arg_level > 0
                  -> DEBUG_MSG "@"; token
                | GT_GT when not (is_semicolon prev_rawtoken) -> DEBUG_MSG "@"; token
                | RPAREN when env#sizeof_ty_flag -> DEBUG_MSG "@"; token
                | _ when is_type s -> DEBUG_MSG "@"; token
                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
            end
        end
        | GT_GT -> begin
            DEBUG_MSG "@";
            match prev_rawtoken with
            | COLON_COLON -> begin
                match prev_rawtoken2 with
                | IDENT _ when begin
                    match prev_rawtoken3 with
                    | TYPENAME | CONST -> true
                    | _ -> false
                end -> DEBUG_MSG "TYPENAME COLON_COLON @ GT_GT"; token
                | _ -> DEBUG_MSG "* COLON_COLON @ GT_GT"; mk (T.IDENT_V s)
            end
            | CONST -> DEBUG_MSG "CONST @ GT_GT"; token
            | _ -> DEBUG_MSG "* @ GT_GT"; mk (T.IDENT_V s)
        end

        | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> begin
            DEBUG_MSG "@ (PTR_AMP|PTR_AMP_AMP|PTR_STAR)";
            match self#peek_nth_rawtoken 2 with
            | PTR_STAR when self#peek_rawtoken() == PTR_STAR && begin
                match self#peek_nth_rawtoken 3 with
                | PTR_STAR -> true
                | TY_TEMPL_GT -> true
                | _ -> false
            end -> DEBUG_MSG "@ PTR_STAR PTR_STAR"; token

            | TY_TEMPL_GT | GT_GT | COMMA | RPAREN | LBRACE | LBRACKET | CONST
              -> DEBUG_MSG "@ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) ..."; token

            | ELLIPSIS when not env#fold_paren_flag
              -> DEBUG_MSG "@ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) ELLIPSIS"; token

            | TY_LPAREN when check_if_abst_dtor_ident ~nth:3 ()
              -> DEBUG_MSG "@ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) TY_LPAREN"; token

            | TY_LPAREN when begin
                match prev_rawtoken with
                | LT | GT | LT_EQ | GT_EQ | EQ_EQ | EXCLAM_EQ _ -> false
                | LPAREN when prev_rawtoken2 == WHILE -> false
                | _ ->
                let nth, ll =
                  self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 ()
                in
                match List.rev ll with
                | [] -> true
                | l0::l1::_ when begin
                    match prev_rawtoken with
                    | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ -> false
                    | _ -> true
                end -> true
                | _ -> false
            end -> DEBUG_MSG "@ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) TY_LPAREN"; token

            | PTR_AMP | PTR_AMP_AMP | PTR_STAR when begin
                match self#peek_nth_rawtoken 3 with
                | TY_TEMPL_GT | COMMA | RPAREN | LBRACE -> true
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 4 with
                    | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) (PTR_AMP|PTR_AMP_AMP|PTR_STAR)"; token

            | IDENT _ when begin
                match self#peek_nth_rawtoken 3 with
                | RPAREN when env#type_paren_flag -> true
                | _ -> false
            end -> DEBUG_MSG "@ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) IDENT"; token

            | _ -> begin
                match prev_rawtoken with
                | STR_LITERAL _ | USER_STR_LITERAL _ -> DEBUG_MSG "(STR_LITERAL|USER_STR_LITERAL) @"; mk (T.STR_MACRO s)

                | STRUCT | CONST | TY_LPAREN -> DEBUG_MSG "... @ (PTR_AMP|PTR_AMP_AMP|PTR_STAR)"; token

                | COLON_COLON when
                    env#const_flag ||
                    env#type_paren_flag &&
                    (not env#templ_arg_flag || env#templ_param_arg_stack_top < env#paren_level)
                  -> DEBUG_MSG "COLON_COLON @ (PTR_AMP|PTR_AMP_AMP|PTR_STAR)"; token

                | COLON_COLON when begin
                    match prev_rawtoken2 with
                    | IDENT _ -> begin
                        match prev_rawtoken3 with
                        | CONST | TY_LPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "(CONST|TY_LPAREN) IDENT COLON_COLON @ (PTR_AMP|PTR_AMP_AMP|PTR_STAR)"; token

                | TEMPL_LT when env#cast_key_flag
                  -> DEBUG_MSG "TEMPL_LT @ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) *"; token

                | _ -> DEBUG_MSG "* @ (PTR_AMP|PTR_AMP_AMP|PTR_STAR) *"; mk (T.IDENT_V s)
            end
        end

        | TY_LPAREN when env#typename_flag -> DEBUG_MSG "@ TY_LPAREN"; token

        | TY_LPAREN when prev_rawtoken == TY_LPAREN && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match ll with
            | [l] -> begin
                list_memqn [T.PTR_STAR;PTR_AMP;PTR_AMP_AMP] l &&
                self#peek_nth_rawtoken (nth+1) == TY_LPAREN
            end
            | _ -> false
        end -> DEBUG_MSG "TY_LPAREN @ TY_LPAREN"; token

        | TY_LPAREN when check_if_abst_dtor_ident() -> DEBUG_MSG "@ TY_LPAREN"; token

        | TY_LPAREN when prev_rawtoken == TY_LPAREN -> DEBUG_MSG "TY_LPAREN @ TY_LPAREN"; mk (T.IDENT_TM s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | STR_LITERAL _ | PP_STRINGIZED _ | STR_MACRO _ -> true
            | _ when begin
                match prev_rawtoken with
                | COMMA | LPAREN | QUEST | COLON | EOF(* | NEWLINE*) -> true
                | RPAREN when env#end_of_literal_macro_call_flag -> true
                | _ -> false
            end -> begin
              let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
              match self#peek_nth_rawtoken (nth+1) with
              | IDENT s' when s' = s -> begin
                  match self#peek_nth_rawtoken (nth+2) with
                  | TY_LPAREN -> begin
                      let conv ((rt, sp, ep) as t) =
                        match rt with
                        | T.IDENT x when x = s' -> T.IDENT_LM x, sp, ep
                        | _ -> t
                      in
                      conv_next_n_tokens conv (nth+1);
                      true
                  end
                  | _ -> false
              end
              | STR_LITERAL _ | PP_STRINGIZED _ | STR_MACRO _ -> true
              | PP_ENDIF -> begin
                  match self#peek_nth_rawtoken (nth+3) with
                  | STR_LITERAL _ | PP_STRINGIZED _ | STR_MACRO _ -> true
                  | _ -> false
              end
              | EOF when env#end_of_literal_macro_call_flag -> true
              | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_LM s)

        | TY_LPAREN when begin
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
            match self#peek_nth_rawtoken (nth+1) with
            | STR_LITERAL _ | PP_STRINGIZED _ | STR_MACRO _ -> true
            | PP_ENDIF -> begin
                match self#peek_nth_rawtoken (nth+3) with
                | STR_LITERAL _ | PP_STRINGIZED _ | STR_MACRO _ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_LM s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | COMMA | LPAREN | QUEST -> true
            | _ -> false
        end && begin
          let nth, ll =
            self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
          in
          DEBUG_MSG "\n%s"
            (String.concat "\n"
               (List.map
                  (fun x -> String.concat ";"
                      (List.map Token.rawtoken_to_string x)) ll));
          match ll with
          | [[IDENT _;IDENT _]] -> true
          | [[IDENT _;IDENT _;DOT;IDENT _]] -> true
          | [[IDENT _;DOT;IDENT _;IDENT _]] -> true
          | _ when List.exists ty_pat0 ll -> true
          | _ -> false
        end -> DEBUG_MSG "(COMMA|LPAREN|QUEST) @ TY_LPAREN"; mk (T.IDENT_EM s)

        | TY_LPAREN when env#arg_paren_flag && begin
            match self#peek_nth_rawtoken 2 with
            | IDENT x -> begin
                match self#peek_nth_rawtoken 3 with
                | RPAREN -> begin
                    let b = is_val x in
                    if b then
                      conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 2;
                    b
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

        | TY_LPAREN when env#in_body_brace_flag && begin
            let nth, ll =
              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
            in
            (match (ll : T.token list list) with
            | [[IDENT x]] -> not (is_val x)
            | _ -> true
            ) &&
            List.exists ty_pat3 ll
        end -> DEBUG_MSG "@"; mk (T.IDENT_EM s)

        | TY_LPAREN when begin
            match self#peek_nth_rawtoken 2 with
            | COMMA | DOT -> true
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.IDENT_EM s)

        | TY_LPAREN when begin
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
            match self#peek_nth_rawtoken (nth+1) with
            | COLON_COLON -> true
            | HEAD_COLON_COLON -> begin
                conv_nth_token (function T.HEAD_COLON_COLON,s,e -> T.COLON_COLON,s,e | x -> x) (nth+1);
                true
            end
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.IDENT_IM s)

        | COMMA when
            prev_rawtoken == COLON_COLON && sub_context == IN_SIMPLE_TEMPL_ID &&
            env#typename_flag -> DEBUG_MSG "@"; token

        | COMMA | RPAREN when begin
            prev_rawtoken == COLON_COLON &&
            not env#init_flag &&
            env#type_paren_flag
        end -> DEBUG_MSG "@"; token

        | RPAREN when begin
            match prev_rawtoken with
            | STR_MACRO _ -> is_semicolon (self#peek_nth_rawtoken 2)
            | _ -> false
        end -> mk (T.STR_MACRO s)

        | RPAREN | COMMA when begin
            match prev_rawtoken with
            | IDENT _ | CONST -> not env#lambda_dtor_flag && self#peek_nth_rawtoken 2 == LBRACE (* compound literal *)
            | CLASS | STRUCT | UNION | ELAB_ENUM -> true
            | UNSIGNED | SIGNED when begin
                self#peek_rawtoken() == RPAREN &&
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> true
                | IDENT _ when env#type_paren_flag -> true
                | _ when env#sizeof_ty_flag -> true
                | _ -> false
            end -> true
            | BASE_COLON when begin
                match prev_rawtoken2 with
                | COMMA | LPAREN -> true
                | _ -> false
            end -> true
            | LPAREN when prev_rawtoken2 == ELLIPSIS && prev_rawtoken3 == SIZEOF -> true
            | TY_LPAREN -> true
            | HEAD_COLON_COLON when prev_rawtoken2 == TY_LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "@ RPAREN"; token

        | EQ when begin
            match prev_rawtoken with
            | CONST -> true
            | _ -> false
        end -> DEBUG_MSG "@ EQ"; mk (T.IDENT_V s)

        | EQ when env#stack#at_enum && env#paren_level = 0 -> DEBUG_MSG "@ EQ"; mk (T.IDENT_E s)

        | TY_LPAREN when (context == ENUM || env#stack#at_enum) && begin
            match prev_rawtoken with
            | LBRACE | COMMA | NEWLINE | INT_LITERAL _ -> begin
                let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
                match self#peek_nth_rawtoken (nth+1) with
                | RBRACE | IDENT _ | PP_UNDEF -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_E s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | IDENT_V _ when begin
                match prev_rawtoken2 with
                | DOT | MINUS_GT -> false
                | _ -> true
            end -> true
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.EMPTY_MACRO s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | IDENT_V _ when begin
                match prev_rawtoken2 with
                | DOT | MINUS_GT -> true
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "(DOT|MINUS_GT) IDENT_V @ TY_LPAREN"; mk (T.OP_MACRO s)

        | TY_LPAREN when env#paren_level > 0 && begin
            match prev_rawtoken with
            | MARKER when begin
                match prev_rawtoken2 with
                | IDENT_V _ | RPAREN -> true
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_AM s)

        | TY_LPAREN when begin
            env#end_of_params_flag && env#lambda_dtor_flag &&
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | LBRACE -> true
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_AM s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | SEMICOLON _ | RBRACE | RPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                DEBUG_MSG "\n%s"
                  (String.concat "\n"
                     (List.map
                        (fun x -> String.concat ";"
                            (List.map Token.rawtoken_to_string x)) ll));
                List.exists
                  (fun (l : T.token list) ->
                    match l with
                    | (SEMICOLON _|RBRACE)::_ -> true
                    | _ -> false
                  ) ll
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_DSM s)

        | TY_LPAREN when (env#braced_init_flag || env#arg_paren_flag) && begin
            prev_rawtoken != COLON_COLON &&
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            ll <> [] &&
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
            | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _
            | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> true
            | LBRACE when not env#lambda_dtor_flag -> true
            | RBRACE -> begin
                match prev_rawtoken with
                | INI_LBRACE
                | COMMA | COLON_COLON | HEAD_COLON_COLON | EQ | QUEST | COLON
                | PLUS | MINUS | STAR | SLASH | PERC | AMP _ | BAR _ | HAT _
                | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
                | EXCLAM _ | AMP_AMP _ | BAR_BAR _ -> false
                | RPAREN when env#end_of_cast_type_flag -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_AGM s)

        | TY_LPAREN when not env#type_paren_flag && begin
            match prev_rawtoken with
            | LPAREN | COMMA -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                ll <> [] &&
                match self#peek_nth_rawtoken (nth+1) with
                | RPAREN | COMMA | BAR_BAR _ | PTR_AMP_AMP ->
                    List.exists
                      (function
                        | [T.TEMPL_LT|TY_TEMPL_GT|GT_EQ|LT_EQ|EQ_EQ|EXCLAM_EQ _] -> true
                        | l ->
                            match List.rev l with
                            | IDENT x::_ when is_type_macro_ident x -> true
                            | _ -> false
                      ) ll
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_EM s)

        | TY_LPAREN when not env#type_paren_flag && begin
            match prev_rawtoken with
            | NEWLINE -> begin
                match prev_rawtoken2 with
                | BAR_BAR _ -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    ll <> [] &&
                    match self#peek_nth_rawtoken (nth+1) with
                    | PP_IF | PP_IFDEF | PP_IFNDEF -> true
                    | IDENT x when x = s -> begin
                        conv_nth_token (function T.IDENT x,s,e -> T.IDENT_LOM x,s,e | x -> x) (nth+1);
                        true
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_LOM s)

        | LBRACKET when begin
            match self#peek_nth_rawtoken 2 with
            | RBRACKET -> begin
                match self#peek_nth_rawtoken 3 with
                | EQ | LBRACE | TY_LPAREN -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "@ LBRACKET"; token

        | DOT when begin
            match self#peek_nth_rawtoken 2 with
            | RPAREN -> true
              | PTR_STAR | SLASH | PERC | PLUS | MINUS -> true
            | _ -> false
        end -> DEBUG_MSG "@ DOT"; mk (T.INT_MACRO s)

        | _ -> begin
            DEBUG_MSG "@ *";
            match prev_rawtoken with
            | TY_LPAREN when begin
                match self#peek_rawtoken() with
                | TY_LPAREN -> begin
                    let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | PTR_STAR -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | RPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> mk (T.IDENT_TM s)

            | TY_LPAREN | GOTO | TYPENAME | CLASS | STRUCT | UNION | ELAB_CLASS
              -> DEBUG_MSG "(TY_LPAREN|GOTO|TYPENAME|CLASS|STRUCT|UNION) @ *"; token

            | CONST when not env#lambda_dtor_flag && env#type_paren_flag && begin
                match prev_rawtoken2 with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    match self#peek_rawtoken() with
                    | COMMA | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP) CONST @ *"; mk (T.IDENT_V s)

            | CONST | VOLATILE | CV_MACRO _ when not env#lambda_dtor_flag
              -> DEBUG_MSG "(CONST|VOLATILE|CV_MACRO) @ *"; token

            | COLON_COLON when env#typename_flag && context != EXPR && begin
                match self#peek_rawtoken() with
                | EQ | TY_TEMPL_GT | COMMA | RPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "COLON_COLON @ *"; token

            | COLON_COLON when begin
                env#templ_arg_flag && prev_rawtoken2 == TEMPL_GT && begin
                  match self#peek_rawtoken() with
                  | COMMA | ELLIPSIS | CONST -> true
                  | _ -> false
                end && env#last_ty_templ_id_flag
            end -> DEBUG_MSG "COLON_COLON @ *"; token

            | COLON_COLON when env#sizeof_ty_flag && env#bracket_level = 0
              -> DEBUG_MSG "COLON_COLON @ *"; token

            | COLON_COLON when begin
                match prev_rawtoken2 with
                | IDENT _ -> begin
                    match prev_rawtoken3 with
                    | TYPENAME -> true
                    | TY_LPAREN -> true
                    | TEMPL_LT when env#cast_key_flag -> true
                    | COMMA when env#templ_arg_flag && begin
                        match self#peek_rawtoken() with
                        | CONST -> begin
                            match self#peek_nth_rawtoken 2 with
                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                match self#peek_nth_rawtoken 3 with
                                | TY_TEMPL_GT -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end -> true
                    (*| COLON when env#init_flag && self#peek_rawtoken() == TEMPL_LT -> true*)
                    | LPAREN when begin
                        match self#peek_rawtoken() with
                        | CONST -> true
                        | _ -> false
                    end -> begin
                        match prev_rawtoken4 with
                        | IF -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "COLON_COLON @ *"; token

            | _ when env#conv_func_id_flag && begin
                match self#peek_rawtoken() with
                | TY_LPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "* @ *"; token

            | MINUS_GT when env#type_paren_flag -> DEBUG_MSG " MINUS_GT @ *"; token

            | STR_LITERAL _ | PP_STRINGIZED _ | STR_MACRO _ | STR_MARKER -> DEBUG_MSG "STR @ *"; mk (T.STR_MACRO s)

            | COMMA when env#str_flag -> DEBUG_MSG "COMMA @ *"; mk (T.STR_MACRO s)

            | COMMA when begin
                match self#peek_rawtoken() with
                | CONST -> true
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 2 with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> true
                        | _ -> false
                    end
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 3 with
                        | COLON_COLON -> begin
                            match self#peek_nth_rawtoken 4 with
                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                match self#peek_nth_rawtoken 5 with
                                | RPAREN -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "COMMA @ *"; token

            | TEMPL_LT when begin
                env#cast_key_flag &&
                self#peek_rawtoken() == TY_LPAREN
            end -> DEBUG_MSG "TEMPL_LT @ TY_LPAREN"; mk (T.IDENT_TM s)

            | TEMPL_LT when begin
                match self#peek_rawtoken() with
                | CONST | VOLATILE -> true
                | LBRACKET -> begin
                    match self#peek_nth_rawtoken 2 with
                    | RBRACKET -> true
                    | _ -> false
                end
                | TY_LPAREN -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    let filt = function
                      | T.CONST | TYPENAME -> true
                      | _ -> false
                    in
                    match ll with
                    | [] -> true
                    | [l] -> begin
                        match l with
                        | [PTR_STAR|PTR_AMP|HAT _] -> true
                        | [x] -> is_basic_ty x
                        | ELLIPSIS::IDENT _::[] -> true
                        | IDENT _::PTR_STAR::IDENT _::[] -> begin
                            self#peek_nth_rawtoken (nth+1) == TY_TEMPL_GT
                        end
                        | PTR_STAR::COLON_COLON::_ -> true
                        | _ -> filt_at_level0 l filt
                    end
                    | [ELLIPSIS]::_ -> true
                    | _ -> List.exists (fun l -> filt_at_level0 l filt) ll
                end
                | _ -> false
            end -> DEBUG_MSG "TEMPL_LT @ *"; token

            | ATTR_MACRO _ when env#type_paren_flag && begin
                match self#peek_rawtoken() with
                | RPAREN -> begin
                    match self#peek_nth_rawtoken 2 with
                    | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                    | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                    | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
                      -> true
                    | TY_LPAREN -> begin
                        match self#peek_nth_rawtoken 3 with
                        | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                        | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                        | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
                          -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "ATTR_MACRO @ *"; token

            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | NULLPTR | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
            | USER_CHAR_LITERAL _ when begin
                env#type_paren_flag &&
                match self#peek_rawtoken() with
                | RPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "(INT_LITERAL|...) @ *"; mk (T.PARAM_DECL_MACRO s)

            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | NULLPTR | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
            | USER_CHAR_LITERAL _ when begin
                match self#peek_rawtoken() with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 2 with
                    | PLUS | MINUS | PTR_STAR | SLASH | PERC | HAT _ | AMP _ | BAR _ -> true
                    | IDENT _ -> begin
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                        match self#peek_nth_rawtoken (nth+1) with
                        | SEMICOLON _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "(INT_LITERAL|...) @ *"; mk (T.IDENT_OM s)

            | COMMA | LPAREN when begin
                match self#peek_rawtoken() with
                | COLON -> begin
                    match self#peek_nth_rawtoken 2 with
                    | COMMA | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@"; token

            | COMMA | LPAREN when begin
                (env#arg_paren_flag || not env#type_paren_flag && env#paren_level > 0) &&
                self#peek_rawtoken() == TY_LPAREN && 
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                (check_if_macro_args ll || is_macro_fun s ||
                match ll with
                | [[IDENT x]] when is_macro_fun x -> true
                | _ -> false) &&
                match self#peek_nth_rawtoken (nth+1) with
                | IDENT _ -> true
                | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                | CHAR_LITERAL _ | NULLPTR | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                | USER_CHAR_LITERAL _ -> true
                | _ -> false
            end -> DEBUG_MSG "(COMMA|LPAREN) @ TY_LPAREN"; mk (T.IDENT_OM s)

            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | NULLPTR | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
            | USER_CHAR_LITERAL _ | IDENT_V _ | RBRACKET when begin
                match self#peek_rawtoken() with
                | RPAREN | COMMA | RBRACE | SEMICOLON _ | COLON | EOF -> true
                | _ -> false
            end -> DEBUG_MSG "(INT_LITERAL|...) @ *"; mk (T.SUFFIX_MACRO s)

            | RPAREN when env#end_of_noptr_dtor_paren_flag && begin
                match self#peek_rawtoken() with
                | RPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "RPAREN @ *"; mk (T.PARAMS_MACRO s)

            | RPAREN when env#end_of_id_macro_call_flag && begin
                match self#peek_rawtoken() with
                | SEMICOLON _ | LBRACE -> true
                | _ -> false
            end -> begin
              DEBUG_MSG "RPAREN @ *";
              if env#in_body_brace_flag then
                mk (T.ARGS_MACRO s)
              else
                mk (T.PARAMS_MACRO s)
            end

            | RPAREN when env#expr_flag && begin
                match self#peek_rawtoken() with
                | COLON | COMMA -> true
                | _ -> false
            end -> DEBUG_MSG "RPAREN @ (COLON|COMMA)"; mk (T.ARGS_MACRO s)

            | IDENT_V _ when begin
                match prev_rawtoken2 with
                | EQ | STAR | LPAREN -> is_literal (self#peek_rawtoken())
                | _ -> false
            end -> DEBUG_MSG "(EQ|STAR) IDENT_V @ literal"; mk (T.OP_MACRO s)

            | IDENT_V _ when begin
                context == EXPR &&
                match prev_rawtoken2 with
                | DOT -> true
                | _ -> false
            end -> DEBUG_MSG "IDENT_V @"; mk (T.ARGS_MACRO s)

            | _ when begin
                match self#peek_rawtoken() with
                | TY_LPAREN -> begin
                    let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | SHARP_SHARP -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ *"; mk (T.IDENT_IM s)

            | _ when context == EXPR && begin
                match self#peek_rawtoken() with
                | TY_LPAREN -> begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~ignore_templ_lv:true ~from:2 ()
                    in
                    let nth, ll =
                      if
                        List.exists
                          (fun l ->
                            match (l : T.token list) with
                            | IDENT _::TEMPL_LT::IDENT _::_ -> true
                            | _ ->
                            match (List.rev l : T.token list) with
                            | [IDENT _;T.TEMPL_LT;x] when is_basic_ty x -> true
                            | IDENT _::TEMPL_LT::IDENT _::_ -> true
                            | _ -> false
                          ) ll
                      then
                        self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                      else
                        nth, ll
                    in
                    DEBUG_MSG "\n%s"
                      (String.concat "\n"
                         (List.map
                            (fun x -> String.concat ";"
                                (List.map Token.rawtoken_to_string x)) ll));
                    List.exists
                      (function
                        | [] -> true
                        | T.SEMICOLON _::_ -> true
                        | [PLUS_PLUS|MINUS_MINUS] -> true
                        | [x] when is_op x -> true
                        | [x] when is_basic_ty x && (prev_rawtoken != RPAREN || env#end_of_cast_type_flag) -> true
                        | [IDENT x] when (is_type_name x || is_type x) && prev_rawtoken != RPAREN -> true
                        | (T.PTR_STAR|PTR_AMP|PTR_AMP_AMP)::r -> not (list_memqn [T.TEMPL_LT;TY_TEMPL_GT] r)
                        | [T.TEMPL_LT|TY_TEMPL_GT|GT_EQ|LT_EQ|EQ_EQ|EXCLAM_EQ _] -> true
                        | l when begin
                            List.memq (Xlist.last l) [T.STRUCT;CLASS;UNION;ENUM;RETURN;DEFAULT;GOTO;CASE;THROW]
                        end -> true
                        | l -> self#peek_nth_rawtoken 2 == LBRACE && List.exists is_semicolon l
                      ) ll
                end
                | _ -> false                
            end -> DEBUG_MSG "* @ *"; mk (T.IDENT_EM s)

            | LPAREN when begin
                self#peek_rawtoken() == COLON &&
                match self#peek_nth_rawtoken 2 with
                | RPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "LPAREN @ COLON RPAREN"; token

            | _ when is_type s -> DEBUG_MSG "* @ *"; token

            | LPAREN when begin
                match self#peek_rawtoken() with
                | CONST -> true
                | _ -> false
            end -> DEBUG_MSG "LPAREN @ CONST"; token

            | RBRACKET when begin
                (env#arg_paren_flag || env#macro_arg_flag) &&
                self#peek_rawtoken() == COMMA
            end -> DEBUG_MSG "RBRACKET @ COMMA"; mk (T.SUFFIX_MACRO s)

            | RPAREN when env#expr_flag && self#peek_rawtoken() == EOF
              -> DEBUG_MSG "RPAREN @ EOF"; mk (T.SUFFIX_MACRO s)

            | EQ when context == EXPR && begin
                match self#peek_rawtoken() with
                | LBRACE -> true
                | _ -> false
            end -> DEBUG_MSG "EQ @ LBRACE"; token

            | MINUS_GT | CONST when env#trailing_retty_flag && begin
                match self#peek_rawtoken() with
                | LBRACE -> true
                | _ -> false
            end -> DEBUG_MSG "(MINUS_GT|CONST) @ LBRACE"; token

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                env#cast_head_flag &&
                not env#lambda_dtor_flag &&
                is_ty prev_rawtoken2 &&
                self#peek_rawtoken() == RPAREN
            end -> DEBUG_MSG "* @ *"; mk (T.CV_MACRO s)

            | _ when begin
                match self#peek_rawtoken() with
                | HAT _ -> begin
                    match self#peek_nth_rawtoken 2 with
                    | TY_TEMPL_GT | COMMA -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ HAT TY_TEMPL_GT"; token

            | _ -> DEBUG_MSG "* @ *"; mk (T.IDENT_V s)
        end
    end
    | MEM_INIT, END_OF_TY_SPEC when
        prev_rawtoken == COMMA && env#paren_level = 0 -> token
    | _, END_OF_TY_SPEC when prev_rawtoken == ELAB_ENUM -> token
    | c, END_OF_TY_SPEC when c != ENUM -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | IDENT_V _ when begin
            match self#peek_rawtoken() with
            | SEMICOLON _ -> true
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; mk (T.ATTR_MACRO s)

        | IDENT_V _ when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | SEMICOLON _ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @ TY_LPAREN"; mk (T.IDENT_AM s)

        | RPAREN when begin
            env#end_of_params_flag &&
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | SEMICOLON _ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @ TY_LPAREN"; mk (T.IDENT_AM s)

        | OPERATOR when begin
            match self#peek_rawtoken() with
            | TEMPL_LT | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> false
            | _ -> true
        end -> begin
          DEBUG_MSG "OPERATOR @";
          env#clear_conv_func_id_flag();
          mk (T.OP_MACRO s)
        end

        | TY_LPAREN when begin
            match prev_rawtoken2 with
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
            | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
            | PTR_STAR | IDENT _ -> begin
                (
                 (match prev_rawtoken2 with IDENT _ -> false | _ -> true) ||
                 (match prev_rawtoken3 with SEMICOLON _ | NEWLINE | TYPEDEF | EXTERN -> true | _ -> false)
                ) &&
                match self#peek_rawtoken() with
                | COLON_COLON | RPAREN | MINUS_GT | DOT | COMMA | ELLIPSIS
                | PLUS | MINUS | SLASH | PERC | BAR _ | BAR_BAR _ | LBRACKET -> false
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                    match self#peek_nth_rawtoken 2 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken (nth+1) with
                        | TY_LPAREN -> true
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth+2) with
                            | TY_LPAREN -> begin
                                match self#peek_nth_rawtoken (nth+3) with
                                | TY_LPAREN -> true
                                | _ -> false
                            end
                            | SEMICOLON _ -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | TEMPL_LT -> false
                | IDENT _ -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "TY_LPAREN @"; mk (T.CC_MACRO s)

        | TY_LPAREN when begin
            match prev_rawtoken2 with
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
            | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
            | PTR_STAR | IDENT _ -> begin
                (
                 (match prev_rawtoken2 with IDENT _ -> false | _ -> true) ||
                 (match prev_rawtoken3 with SEMICOLON _ | NEWLINE | TYPEDEF | EXTERN -> true | _ -> false)
                ) &&
                match self#peek_rawtoken() with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 2 with
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 3 with
                        | TY_LPAREN -> begin
                            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:4 () in
                            match self#peek_nth_rawtoken (nth+1) with
                            | SEMICOLON _ | CONST | OVERRIDE | VIRTUAL -> begin
                                List.exists ty_pat3 ll
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "TY_LPAREN @"; mk (T.PTR_MACRO s)

        | TY_LPAREN when begin
            match prev_rawtoken2 with
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
            | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
            | PTR_STAR | PTR_AMP -> begin
                match self#peek_rawtoken() with
                | RPAREN when begin
                    match self#peek_nth_rawtoken 2 with
                    | TY_LPAREN -> true
                    | _ -> false
                end -> true
                | _ -> false
            end
            | IDENT _ when begin
                match prev_rawtoken3 with
                | TYPEDEF | STATIC -> true
                | _ -> false
            end -> true
            | RPAREN when self#peek_rawtoken() == RPAREN -> begin
                match prev_rawtoken3 with
                | PTR_STAR -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_V s)

        (*| TY_LPAREN when begin
            self#peek_rawtoken() == RPAREN &&
            self#peek_nth_rawtoken 2 == TY_LPAREN &&
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
            match self#peek_nth_rawtoken (nth+1) with
            | LBRACE | COLON | CONST -> true
            | _ -> check_if_params ll
        end -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_V s)*)

        (*| TY_LPAREN *)| CLASS | ENUM | STRUCT | UNION -> DEBUG_MSG "(TY_LPAREN|CLASS|ENUM|STRUCT|UNION) @"; token

        | LPAREN when prev_rawtoken2 == ELLIPSIS && prev_rawtoken3 == SIZEOF -> DEBUG_MSG "LPAREN @"; token

        | STR_LITERAL _ when prev_rawtoken2 == OPERATOR -> DEBUG_MSG "STR_LITERAL @"; token

        | USING when self#peek_rawtoken() == EQ -> DEBUG_MSG "USING @"; token

        | EQ when begin
            env#using_flag && env#alias_flag && begin
              match self#peek_rawtoken() with
              | SEMICOLON _ | TY_LPAREN -> true
              | _ -> false
            end
        end -> DEBUG_MSG "EQ @"; token

        (*NG!| COLON_COLON when env#typename_flag -> token*)

        | COLON_COLON when
            env#using_flag && env#alias_flag && is_semicolon (self#peek_rawtoken())
          -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when env#templ_head_flag && env#ty_param_key_flag -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when env#const_flag && self#peek_rawtoken() == TY_TEMPL_GT -> begin
            DEBUG_MSG "COLON_COLON @";
            token
        end
        | COLON_COLON when begin
            match prev_rawtoken2 with
            | IDENT _ -> begin
                match prev_rawtoken3 with
                | CONST -> begin
                    match prev_rawtoken4 with
                    | COMMA | TEMPL_LT -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when begin
            env#templ_arg_flag &&
            match self#peek_rawtoken() with
            | PLUS | MINUS | PTR_STAR | SLASH | PERC -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_TEMPL_GT | GT_GT | COMMA | RPAREN -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | CONST when env#using_flag && env#alias_flag && is_semicolon (self#peek_rawtoken())
          -> DEBUG_MSG "CONST @"; token

        | CONST | VOLATILE when begin
            match prev_rawtoken2 with
            | COMMA | TEMPL_LT -> true
            | CONST | VOLATILE -> begin
                match prev_rawtoken3 with
                | COMMA | TEMPL_LT -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(CONST|VOLATILE) @"; token

        | STATIC when begin
            match self#peek_rawtoken() with
            | TYPENAME -> true
            | _ -> false
        end -> DEBUG_MSG "STATIC @"; token

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
            (match prev_rawtoken2 with
            | TY_LPAREN | CC_MACRO _ -> true
            | _ -> false) &&
            self#peek_rawtoken() == TY_LPAREN && begin
              let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
              match self#peek_nth_rawtoken (nth+1) with
              | RPAREN -> begin
                  match self#peek_nth_rawtoken (nth+2) with
                  | TY_LPAREN -> true
                  | IDENT _ when begin
                      match self#peek_nth_rawtoken (nth+3) with
                      | SEMICOLON _ -> begin
                          conv_nth_token (function T.IDENT x,s,e -> T.PARAMS_MACRO x,s,e | x -> x) (nth+2);
                          true
                      end
                      | _ -> false
                  end -> true
                  | _ -> false
              end
              | _ -> false
            end
        end -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP) @"; mk (T.IDENT_IM s)

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
            self#peek_rawtoken() != COLON_COLON &&
            match prev_rawtoken2 with
            | TY_LPAREN | COMMA -> true
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
            | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
            | IDENT _ -> begin
                match prev_rawtoken3 with
                | SEMICOLON _ | RBRACE -> begin
                    match self#peek_rawtoken() with
                    | TY_LPAREN when check_if_abst_dtor_ident() -> false
                    | TY_LPAREN -> begin
                        let nth, ll =
                          self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                        in
                        match self#peek_nth_rawtoken (nth+1) with
                        | SEMICOLON _ | LBRACE -> true
                        | CONST | OVERRIDE | FINAL -> true
                        | IDENT x when is_capital_ident x -> true
                        | _ -> false
                    end
                    | IDENT x when not (is_capital_ident s) && is_capital_ident x -> begin
                        begin
                          match self#peek_nth_rawtoken 2 with
                          | TY_LPAREN ->
                              conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AM x,s,e | x -> x) 1
                          | _ -> ()
                        end;
                        true
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP) @"; mk (T.IDENT_V s)

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
            match self#peek_rawtoken() with
            | IDENT x when is_attr_macro x || is_attr_macro_ident x -> false
            | IDENT x when not env#type_paren_flag -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> not (check_double_paren 2)
                    | _ -> begin
                        let nth, ll =
                          self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 ()
                        in
                        match self#peek_nth_rawtoken (nth+1) with
                        | IDENT y when y = x -> false
                        | _ -> true
                    end
                end
                | _ -> true
            end
            | PTR_STAR -> true
            | _ -> false
        end -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP) @"; mk (T.ATTR_MACRO s)

        | LBRACKET when self#peek_rawtoken() != COLON_COLON -> DEBUG_MSG "LBRACKET @"; mk (T.IDENT_V s)

        | LPAREN | COMMA when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> true
            | _ -> false
        end && begin
          let _, _, l =
            self#peek_rawtoken_up_to_rparen ~level:env#paren_level None
          in
          match List.rev l with
          | TY_LPAREN::IDENT _::TY_LPAREN::rest -> begin
              match List.rev rest with
              | RPAREN::rest -> begin
                  let ll = split_at_comma (List.rev rest) in
                  DEBUG_MSG "\n%s"
                    (String.concat "\n"
                       (List.map
                          (fun x -> String.concat ";"
                              (List.map Token.rawtoken_to_string x)) ll));
                  List.exists ty_pat1 ll
              end
              | _ -> false
          end
          | _ -> false
        end -> DEBUG_MSG "(LPAREN|COMMA) @"; mk (T.IDENT_EM s)

        | TEMPL_LT when begin
            match prev_rawtoken2 with
            | STATIC_CAST | DYNAMIC_CAST | REINTERPRET_CAST | CONST_CAST -> true
            | _ -> false
        end -> DEBUG_MSG "TEMPL_LT @"; token

        | COMMA | TEMPL_LT when begin
            env#templ_arg_flag &&
            match self#peek_rawtoken() with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA | TY_TEMPL_GT | TEMPL_GT | GT_GT -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(COMMA|TEMPL_LT) @"; token

        | COMMA | TEMPL_LT when begin
            env#typedef_flag && env#templ_param_arg_level > 0 &&
            match self#peek_rawtoken() with
            | PLUS | MINUS | SLASH | PERC -> true
            | PTR_STAR -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_TEMPL_GT | COMMA -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "(COMMA|TEMPL_LT) @"; mk (T.IDENT_V s)

        | RPAREN | CONST when begin
            env#end_of_params_flag &&
            (context == TOP || context == MEM) &&
            not env#objc_class_interface_flag &&
            match self#peek_rawtoken() with
            | COLON | LBRACE -> true
            | EQ when env#end_of_params_flag -> true
            | _ -> false
        end -> DEBUG_MSG "(RPAREN|CONST) @"; mk (T.NOEXCEPT_MACRO s)

        | RPAREN when begin
            env#end_of_params_flag &&
            not env#objc_class_interface_flag &&
            match self#peek_rawtoken() with
            | SEMICOLON _ -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; mk (T.CV_MACRO s)

        | RPAREN when begin
            context == CLASS &&
            match self#peek_rawtoken() with
            | LBRACE -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; mk (T.IDENT s)

        | RPAREN when begin
            env#alignas_flag &&
            match self#peek_rawtoken() with
            | x when is_basic_ty x -> true
            | IDENT x when is_type_name x || is_type x -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; mk (T.ATTR_MACRO s)

        | RPAREN when self#peek_rawtoken() != COLON_COLON && begin
            match prev_rawtoken2 with
            | IDENT _ | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
            | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ -> begin
                match prev_rawtoken3 with
                | TY_LPAREN -> true
                | LPAREN -> begin
                    match prev_rawtoken4 with
                    | IDENT_TM _ -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; mk (T.IDENT_V s)

        | COMMA when begin
            match self#peek_rawtoken() with
            | STAR | SLASH | PERC | AMP _ | BAR _ | PLUS | MINUS | EXCLAM _ | TILDE _
            | LT_LT | GT_GT | AMP_AMP _ | BAR_BAR _ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
              -> true
            | PTR_STAR | PTR_AMP_AMP when env#templ_arg_flag || env#arg_paren_flag -> true
            | TEMPL_LT when begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end -> true
            | TY_TEMPL_GT when env#templ_param_arg_level = 0 -> true
            | _ -> false
        end -> DEBUG_MSG "COMMA @"; mk (T.IDENT_V s)

        | TY_TILDE when context == TOP || context == MEM -> DEBUG_MSG "TY_TILDE @"; token

        | IDENT_V _ | RPAREN when
            not env#end_of_params_flag &&
            self#peek_rawtoken() == TY_LPAREN &&
            self#peek_nth_rawtoken 2 == TY_LPAREN
          -> DEBUG_MSG "(IDENT_V|RPAREN) @"; mk (T.IDENT_PM s)

        | IDENT_V _ when begin
            env#type_paren_flag &&
            self#peek_rawtoken() == TY_LPAREN &&
            let nth, ll =
              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
            in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT x when x = s -> begin
                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_PM x,s,e | x -> x) (nth+1);
                true
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; mk (T.IDENT_PM s)

        | IDENT _ when self#peek_rawtoken() == TY_LPAREN && env#templ_arg_flag && begin
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
            match self#peek_nth_rawtoken (nth+1) with
            | TY_TEMPL_GT -> true
            | _ -> false
        end -> DEBUG_MSG "IDENT @"; mk (T.IDENT_TM s)

        | RBRACE when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | EQ -> true
                | LBRACKET when
                    self#peek_nth_rawtoken (nth+2) == RBRACKET &&
                    self#peek_nth_rawtoken (nth+3) == EQ
                  -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "* @"; mk (T.IDENT_IM s)

        | _ when self#peek_rawtoken() == TY_LPAREN && begin
            match prev_rawtoken with
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
            | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
            | CONST | EXTERN | PTR_STAR | CC_MACRO _
            | IDENT _ -> begin
                match prev_rawtoken2 with
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
                | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
                | ELAB_ENUM | ELAB_CLASS | CLASS | STRUCT | UNION
                | CONST | EXTERN | PTR_STAR | IDENT _ -> begin
                    let _, nth, l = self#peek_rawtoken_up_to_rparen ~level:0 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | TY_LPAREN -> true
                    | LBRACKET -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | RBRACKET -> begin
                            match self#peek_nth_rawtoken (nth+3) with
                            | EQ -> true
                            | _ -> false
                        end
                        | INT_LITERAL _ | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth+3) with
                            | RBRACKET -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | EQ when begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | DELETE | DEFAULT | INT_LITERAL _ when context == TOP || context == MEM -> false
                        | _ -> true
                    end -> true
                    | IDENT _ when begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | EQ -> true
                        | _ -> false
                    end -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "* @"; mk (T.IDENT_IM s)

        | EQ when env#ty_param_key_flag -> DEBUG_MSG "EQ @"; token

        | MINUS_GT when env#type_paren_flag -> DEBUG_MSG "MINUS_GT @"; token

        | MINUS_GT when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll =
                  self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                in
                match self#peek_nth_rawtoken (nth+1) with
                | LBRACE -> true
                | TY_LPAREN -> true
                | _ -> false
            end
            | LBRACE -> true
            | _ -> false
        end -> DEBUG_MSG "MINUS_GT @"; token

        | COLON_COLON when begin
            match prev_rawtoken2 with
            | IDENT _ -> begin
                match prev_rawtoken3 with
                | TYPENAME -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when env#type_paren_flag && begin
            match self#peek_rawtoken() with
            | RPAREN -> is_semicolon (self#peek_nth_rawtoken 2)
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; token

        | QUEST when self#peek_rawtoken() == COLON -> DEBUG_MSG "QUEST @"; mk (T.IDENT_V s)

        | ELLIPSIS when prev_rawtoken2 == TYPENAME -> DEBUG_MSG "ELLIPSIS @"; token

        | _ -> begin
            DEBUG_MSG "* @";
            match self#peek_rawtoken() with
            | CONST when env#templ_arg_flag -> token
            | PTR_AMP | PTR_STAR when env#templ_arg_flag -> begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ when begin
                    match self#peek_nth_rawtoken 3 with
                    | PLUS | MINUS | SLASH | PTR_STAR | PTR_AMP | HAT _ | BAR _ -> true
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | EQ_EQ | EXCLAM_EQ _ | QUEST -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> mk (T.IDENT_V s)
                | INT_LITERAL _ | FLOAT_LITERAL _
                | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> mk (T.IDENT_V s)
                | _ ->
                    let nth, l = self#peek_rawtoken_up_to [T.COMMA;TY_TEMPL_GT] in
                    if filt_at_level0 l ((==) T.QUEST) then
                      mk (T.IDENT_V s)
                    else
                      token
            end
            | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> begin
                DEBUG_MSG "* @ (PTR_AMP|PTR_AMP_AMP|PTR_STAR)";
                if
                  prev_rawtoken == COLON_COLON && prev_rawtoken2 == TEMPL_GT &&
                  not env#last_ty_templ_id_flag
                then
                  match self#peek_nth_rawtoken 2 with
                  | IDENT _ -> begin
                      match self#peek_nth_rawtoken 3 with
                      | EQ -> DEBUG_MSG "@"; token
                      | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                  end
                  | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                else if prev_rawtoken == COMMA && env#dtor_flag then begin
                  DEBUG_MSG "@"; mk (T.CC_MACRO s)
                end
                else
                  match self#peek_nth_rawtoken 2 with
                  | TY_LPAREN -> begin
                      match self#peek_nth_rawtoken 3 with
                      | TY_LPAREN when check_double_paren 2 -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                      | _ -> DEBUG_MSG "@"; token
                  end
                  | _ -> DEBUG_MSG "@"; token
            end
            | IDENT _ when env#end_of_params_flag -> DEBUG_MSG "@ IDENT"; mk (T.ATTR_MACRO s)
            | EOF | SEMICOLON _ when env#end_of_params_flag && begin
                match prev_rawtoken with
                | ATTR_MACRO _ | CONST -> true
                | _ -> false
            end -> DEBUG_MSG "@ (EOF|SEMICOLON)"; mk (T.ATTR_MACRO s)
            | IDENT _ when begin
                env#type_paren_flag &&
                match prev_rawtoken with
                | CONST -> begin
                    match self#peek_nth_rawtoken 2 with
                    | COMMA | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "CONST @ IDENT (COMMA|RPAREN)"; mk (T.CV_MACRO s)

            | IDENT x when (contains_arg x || contains_param x) && begin
                match self#peek_nth_rawtoken 2 with
                | LBRACE when begin
                    match context with
                    | TOP | MEM -> sub_context == END_OF_TY_SPEC
                    | _ -> false
                end -> begin
                  conv_nth_token (function T.IDENT x,s,e -> T.PARAMS_MACRO x,s,e | x -> x) 1;
                  true
                end
                | _ -> false
            end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

            | IDENT x when begin
                is_attr_macro x &&
                env#type_paren_flag &&
                match self#peek_nth_rawtoken 2 with
                | COMMA | RPAREN | EQ -> begin
                    match prev_rawtoken with
                    | IDENT _ | TYPE_MACRO _ | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

            | TEMPL_LT | COLON_COLON | IDENT _ -> begin
                DEBUG_MSG "* @ (TEMPL_LT|COLON_COLON|IDENT)";
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN when check_double_paren 2 -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    | _  -> begin
                        match self#peek_rawtoken() with
                        | IDENT x -> begin
                            let nth, ll =
                              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 ()
                            in
                            match self#peek_nth_rawtoken (nth+1) with
                            | IDENT y when y = x -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
                            | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
                            | CONST -> DEBUG_MSG "@"; token
                            | IDENT y when is_type_name y || is_type y -> DEBUG_MSG "@"; token
                            | _ when is_attr_macro x || is_attr_macro_ident x -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            | _ when env#type_paren_flag && begin
                                match prev_rawtoken with
                                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
                                | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ -> true
                                | _ -> false
                            end -> begin
                              DEBUG_MSG "@";
                              conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AM x,s,e | x -> x) 1;
                              mk (T.IDENT_V s)
                            end
                            | _ -> DEBUG_MSG "@"; token
                        end
                        | _ -> DEBUG_MSG "@"; token
                    end
                end

                | RPAREN(* | COMMA*) | PP_IF | PP_IFDEF | PP_IFNDEF when begin
                    env#type_paren_flag && env#paren_level = 1 && not env#typename_flag &&
                    match self#peek_rawtoken() with
                    | IDENT _ when begin
                        match prev_rawtoken with
                        | TY_LPAREN | COMMA -> begin
                            match self#peek_nth_rawtoken 2 with
                            | RPAREN | COMMA -> true
                            | _ -> false
                        end
                        | _ -> false
                    end -> false
                    | IDENT x -> begin
                        conv_nth_token (function T.IDENT x,s,e -> T.PARAM_DECL_MACRO x,s,e | x -> x) 1;
                        true
                    end
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

                | SEMICOLON _ when begin
                    match self#peek_rawtoken() with
                    | IDENT x when is_capital_ident x -> begin
                        conv_nth_token (function T.IDENT x,s,e -> T.ATTR_MACRO x,s,e | x -> x) 1;
                        true
                    end
                    | _ -> false
                end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

                | IDENT _ when env#paren_level = 1 && self#peek_nth_rawtoken 3 == RPAREN
                  -> DEBUG_MSG "@"; mk (T.IDENT_V s)

                | _ when begin
                    match self#peek_rawtoken() with
                    | IDENT x when not (is_type_name x || is_type x) -> begin
                        match prev_rawtoken with
                        | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> true
                        | CONST | VOLATILE -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "@"; mk (T.CV_MACRO s)

                | _ -> DEBUG_MSG "@"; token
            end

            | ELLIPSIS when self#peek_nth_rawtoken 2 == TY_TEMPL_GT && begin
                prev_rawtoken == COLON_COLON && prev_rawtoken2 == TEMPL_GT &&
                env#last_ty_templ_id_flag
            end -> DEBUG_MSG "@"; token

            | TY_TEMPL_GT | TEMPL_GT | COMMA when (*env#templ_arg_flag &&*) begin
                prev_rawtoken == COLON_COLON &&
                (prev_rawtoken2 == TEMPL_GT ||
                (match prev_rawtoken2 with IDENT _ -> true | _ -> false) &&
                prev_rawtoken3 == COLON_COLON && prev_rawtoken4 == TEMPL_GT) &&
                env#last_ty_templ_id_flag
            end -> DEBUG_MSG "@"; token

            | GT_GT when env#templ_param_arg_level > 1 && begin
                prev_rawtoken == COLON_COLON && prev_rawtoken2 == TEMPL_GT &&
                env#last_ty_templ_id_flag
            end -> DEBUG_MSG "@"; token

            | GT_GT -> begin
                DEBUG_MSG "@";
                if is_type_name s || is_type s then
                  token
                else
                  mk (T.IDENT_V s)
            end

            | SEMICOLON _ when context == MEM(* && sub_context == END_OF_TY_SPEC*) && begin
                env#end_of_params_flag && not env#typedef_flag &&
                match prev_rawtoken with
                | RPAREN when not env#end_of_decl_spec_macro_call_flag && env#end_of_params_flag -> true
                | _ -> false
            end -> DEBUG_MSG "* @ SEMICOLON"; mk (T.VIRT_SPEC_MACRO s)
            | SEMICOLON _ when context == MEM && sub_context == END_OF_TY_SPEC && begin
                not env#typedef_flag &&
                match prev_rawtoken with
                | MINUS_GT -> true
                | COLON_COLON -> env#typename_flag
                | _ -> false
            end -> DEBUG_MSG "* @ SEMICOLON"; token
            | COLON when not env#for_flag && not env#objc_class_interface_flag -> mk (T.IDENT_B s)
            | OPERATOR when begin
                match prev_rawtoken with
                | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> true
                | _ -> false
            end -> mk (T.ATTR_MACRO s)
            | OPERATOR -> DEBUG_MSG "@ OPERATOR"; token

            | TY_LPAREN when begin
                match self#peek_nth_rawtoken 2 with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_LPAREN -> true
                        | LBRACKET when env#type_paren_flag -> true
                        | _ -> false
                    end
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | RPAREN -> begin
                            match self#peek_nth_rawtoken 5 with
                            | TY_LPAREN -> true
                            | LBRACKET when env#type_paren_flag -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | RPAREN when env#conv_func_id_flag -> true
                | _ -> false
            end -> DEBUG_MSG "@ TY_LPAREN"; token

            | TY_LPAREN when begin
                is_ty _rawtok
            end -> DEBUG_MSG "@ TY_LPAREN"; token

            | TY_LPAREN when begin
                env#objc_class_interface_flag &&
                match prev_rawtoken with
                | IDENT_V _ -> true
                | _ -> false
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_AM s)

            | TY_LPAREN when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                DEBUG_MSG "\n%s"
                  (String.concat "\n"
                     (List.map
                        (fun x -> String.concat ";"
                            (List.map Token.rawtoken_to_string x)) ll));
                ll <> [] &&
                match self#peek_nth_rawtoken (nth+1) with
                | RPAREN when begin
                    match prev_rawtoken with
                    | CONST -> true
                    | _ -> false
                end -> true
                | TY_LPAREN -> begin
                    let nth0, ll0 =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) ()
                    in
                    match ll0 with
                    | [l] -> begin
                        match self#peek_nth_rawtoken (nth0+1) with
                        | TY_LPAREN -> begin
                            let nth1, ll1 =
                              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth0+2) ()
                            in
                            match self#peek_nth_rawtoken (nth1+1) with
                            | LBRACE | CONST -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | IDENT x when x = s -> begin
                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AM x,s,e | x -> x) (nth+1);
                    true
                end
                | IDENT x when is_macro_fun x -> false
                | IDENT x when is_virt_spec_macro_ident x || is_virt_spec_macro x -> false
                | IDENT x when is_cv_spec_macro_ident x || is_cv_spec_macro x -> false

                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | TY_LPAREN -> begin
                        match self#peek_nth_rawtoken (nth+3) with
                        | TY_LPAREN when begin
                            let nth1, ll1 =
                              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+4) ()
                            in
                            match self#peek_nth_rawtoken (nth1+1) with
                            | RPAREN -> true
                            | _ -> false
                        end -> false

                        | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                        | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                        | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                        | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> false
                        | _ ->
                        not
                          (List.exists
                             (fun l ->
                               filt_at_level0 l
                                 (function
                                   | T.CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                                   | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST
                                   | TYPE_MACRO _ -> true
                                   | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                                   | IDENT x when is_type_name x -> true
                                   | _ -> false
                                 )
                          ) ll)
                    end
                    | SEMICOLON _ when begin
                        List.exists
                          (function
                              [T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                            | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                            | USER_STR_LITERAL _ | USER_CHAR_LITERAL _] -> true
                            | _ -> false
                          ) ll
                    end -> true
                    | CONST -> true
                    | _ -> false
                end
                | OPERATOR -> true
                | TY_TEMPL_GT -> prev_rawtoken == CONST && env#templ_arg_flag
                | _ -> false
            end -> begin
              DEBUG_MSG "@ TY_LPAREN";
              match prev_rawtoken with
              | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> mk (T.IDENT_AM s)
              | x when is_ty x -> mk (T.IDENT_AM s)
              | _ -> mk (T.IDENT_TM s)
            end

            | TY_LPAREN when env#end_of_params_flag && begin
                match prev_rawtoken with
                | COMMA -> false
                | _ ->
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                DEBUG_MSG "\n%s"
                  (String.concat "\n"
                     (List.map
                        (fun x -> String.concat ";"
                            (List.map Token.rawtoken_to_string x)) ll));
                match self#peek_nth_rawtoken (nth+1) with
                | LBRACE | SEMICOLON _ -> true
                | CONST when begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | LBRACE | SEMICOLON _ -> true
                    | _ -> false
                end -> true
                | _ -> false
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_CM s)

            | TY_LPAREN when begin
                match prev_rawtoken with
                | IDENT _ -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    DEBUG_MSG "\n%s"
                      (String.concat "\n"
                         (List.map
                            (fun x -> String.concat ";"
                                (List.map Token.rawtoken_to_string x)) ll));
                    match self#peek_nth_rawtoken (nth+1) with
                    | STRUCT | UNION | CLASS | ENUM -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_DM s)

            | TY_LPAREN when begin
                match prev_rawtoken with
                | AMP_AMP _ | BAR_BAR _ | AMP _ | BAR _ | STAR | PLUS | MINUS | SLASH | PERC
                | EXCLAM _ | TILDE _ | HAT _
                | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
                | LPAREN | COMMA -> begin
                    (match prev_rawtoken with
                    | COMMA -> env#macro_arg_flag || env#arg_paren_flag || env#templ_arg_flag
                    | LPAREN -> begin
                        match prev_rawtoken2 with
                        | AMP_AMP _ | BAR_BAR _ | AMP _ | BAR _ | STAR | PLUS | MINUS | SLASH | PERC
                        | EXCLAM _ | TILDE _ | HAT _
                        | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ -> true
                        | _ when env#arg_paren_flag -> true
                        | _ -> false
                    end
                    | _ -> true) &&
                    let b = is_macro_fun s in
                    b ||
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    DEBUG_MSG "\n%s"
                      (String.concat "\n"
                         (List.map
                            (fun x -> String.concat ";"
                                (List.map Token.rawtoken_to_string x)) ll));
                    check_if_macro_args ll
                    (*List.exists
                      (fun l ->
                        match (List.rev l : T.token list) with
                        | TYPENAME::_ -> true
                        | [IDENT x] when is_type_name x || is_type x -> true
                        | x::_ when is_basic_ty x -> true
                        | _ -> false
                      ) ll*)
                end
                | _ -> false
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_EM s)

            | TY_LPAREN when begin
                env#type_paren_flag &&
                match prev_rawtoken with
                | IDENT_V _ -> begin
                    match prev_rawtoken2 with
                    | IDENT _ -> begin
                        match prev_rawtoken3 with
                        | COMMA | TY_LPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_PM s)

            | TY_LPAREN when begin
                not env#macro_arg_flag &&
                env#end_of_params_flag &&
                match prev_rawtoken with
                | CONST | RPAREN when begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                    in
                    match self#peek_nth_rawtoken (nth+1) with
                    | SEMICOLON _ | LBRACE | VOLATILE | RESTRICT _ | MS_STDCALL _ | MS_CDECL _
                    | CC_MACRO _ | CV_MACRO _ | EQ | COLON -> false
                    | TEMPLATE -> true
                    | IDENT _ when begin
                        try
                          let _, macro_kind, tok_list_obj = env#find_pending_macro s in
                          match macro_kind with
                          | FunctionLike _ -> begin
                              let tok_list = (Obj.obj tok_list_obj : token list) in
                              match tok_list with
                              | (T.LBRACE,_,_)::rest when begin
                                  match Xlist.last rest with
                                  | RBRACE,_,_ -> true
                                  | _ -> false
                              end -> true
                              | _ -> false
                          end
                          | _ -> false
                        with
                          _ -> false
                    end -> true
                    | IDENT _ -> false
                    | _ -> true
                end -> true
                | _ -> false
            end -> DEBUG_MSG "(CONST|RPAREN) @ TY_LPAREN"; mk (T.IDENT_BM s)

            | TY_LPAREN when begin
                env#end_of_params_flag &&
                match prev_rawtoken with
                | RPAREN | CONST -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | COLON | SEMICOLON _ | LBRACE -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "(CONST|RPAREN) @ TY_LPAREN"; mk (T.IDENT_AM s)

            | x when is_basic_ty x -> DEBUG_MSG "@ *"; token

            | RPAREN when begin
                env#type_paren_flag && begin
                  match prev_rawtoken with
                  | COLON_COLON | HEAD_COLON_COLON -> true
                  | IDENT _ when begin
                      env#objc_class_interface_flag &&
                      match self#peek_nth_rawtoken 2 with
                      | IDENT _ -> true
                      | _ -> false
                  end -> true
                  | _ -> false
                end &&
                self#peek_nth_rawtoken 2 != TY_LPAREN
            end -> DEBUG_MSG "@ RPAREN"; token

            | SEMICOLON _ | EOF | GNU_ASM _ when begin
                match prev_rawtoken with
                | IDENT_V _ -> begin
                    match prev_rawtoken2 with
                    | IDENT _ -> begin
                        match prev_rawtoken3 with
                        | SEMICOLON _ | RBRACE -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ (SEMICOLON|EOF|GNU_ASM)"; mk (T.PARAMS_MACRO s)

            | LBRACE when context == TOP && sub_context == END_OF_TY_SPEC && begin
                match prev_rawtoken with
                | IDENT_V _ -> true
                | _ -> false
            end -> DEBUG_MSG "IDENT_V @ LBRACE"; mk (T.PARAMS_MACRO s)

            | SEMICOLON _ when env#trailing_retty_flag && env#end_of_params_flag
              -> DEBUG_MSG "@ SEMICOLON"; token

            | COMMA when begin
                prev_rawtoken == COLON_COLON &&
                (env#type_paren_flag && env#alias_flag ||
                (*env#base_clause_flag ||*)
                env#templ_arg_flag && env#const_flag)
            end -> DEBUG_MSG "@ COMMA"; token

            | GNU_ATTR _ when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    let _, nth, l = self#peek_rawtoken_up_to_rparen ~level:0 ~from:2 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | IDENT _ -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ GNU_ATTR"; token

            | TY_TEMPL_GT when env#typename_flag && env#templ_arg_flag -> DEBUG_MSG "@ TY_TEMPL_GT"; token

            | HAT _ when begin
                env#templ_arg_flag && self#peek_nth_rawtoken 2 == TY_TEMPL_GT
            end -> DEBUG_MSG "@ HAT TY_TEMPL_GT"; token

            | HAT _ when begin
                match self#peek_nth_rawtoken 2 with
                | IDENT x -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN when x = "get" || x = "set" -> true
                    | TY_LPAREN -> begin
                        let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:4 None in
                        match self#peek_nth_rawtoken (nth+1) with
                        | LBRACE | SEMICOLON _ -> true
                        | _ -> false
                    end
                    | LBRACE | SEMICOLON _ -> true
                    | COLON_COLON -> begin
                        try
                          let nth = self#peek_rawtoken_up_to_end_of_qualified_id ~from:3 () in
                          match self#peek_nth_rawtoken (nth+1) with
                          | EQ -> true
                          | _ -> false
                        with _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ HAT IDENT TY_LPAREN"; token

            | _ when prev_rawtoken == TY_LPAREN && is_type ~weak:true s -> DEBUG_MSG "TY_LPAREN @ *"; token

            | _ when prev_rawtoken == TY_LPAREN && env#objc_class_interface_flag -> DEBUG_MSG "TY_LPAREN @ *"; token

            | _ ->
                DEBUG_MSG "@ *";
                (*if context == TOP && sub_context == END_OF_TY_SPEC then begin
                  match self#peek_rawtoken() with
                  | EQ -> begin
                      match self#peek_nth_rawtoken 2 with
                      | PP_INCLUDE -> begin
                          let nth, _ = self#peek_rawtoken_up_to ~from:3 [T.NEWLINE] in
                          match self#peek_nth_rawtoken (nth+1) with
                          | SEMICOLON _ -> begin
                              let _, s, _ = self#peek_nth (nth+1) in
                              insert_after_nth_token nth [T.DUMMY_EXPR, s, s]
                          end
                          | PP_INCLUDE -> begin
                              let nth', _ = self#peek_rawtoken_up_to ~from:(nth+2) [T.NEWLINE] in
                              match self#peek_nth_rawtoken (nth'+1) with
                              | SEMICOLON _ -> begin
                                  let _, s, _ = self#peek_nth (nth'+1) in
                                  insert_after_nth_token nth' [T.DUMMY_EXPR, s, s]
                              end
                              | _ -> ()
                          end
                          | _ -> ()
                      end
                      | _ -> ()
                  end
                  | _ -> ()
                end;*)
                mk (T.IDENT_V s)
        end
    end
    | _, START_OF_STMT sn -> begin
        DEBUG_MSG "@";
        match self#peek_rawtoken() with
        | COLON | COLON_COLON -> DEBUG_MSG "@ (COLON|COLON_COLON)"; token

        | PP_DEFINE | PP_UNDEF | PP_LINE | PP_ERROR | PP_UNKNOWN _ | PP_
        | PP_INCLUDE | PP_IMPORT | PP_PRAGMA -> DEBUG_MSG "@ (PP_DEFINE|...)"; mk (T.STMT_MACRO s)

        | HAT _ when begin
            match self#peek_nth_rawtoken 2 with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 3 with
                | EQ | SEMICOLON _ -> true
                | _ -> false
            end
            | _ -> false
        end -> begin
          DEBUG_MSG "@ HAT";
          conv_nth_token (function T.HAT x,s,e -> T.PTR_HAT,s,e | x -> x) 1;
          token
        end

        | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | LT_LT_EQ | GT_GT_EQ
        | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | PLUS | MINUS | SLASH | PERC | HAT _ | BAR _
        | DOT | MINUS_GT | LBRACKET | LT_LT | PLUS_PLUS | MINUS_MINUS | EQ_EQ | EXCLAM_EQ _
          -> begin
            DEBUG_MSG "@ (EQ|...)";
            begin
              match self#peek_nth_rawtoken 2 with
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken 3 with
                  | IDENT _ -> begin
                      match self#peek_nth_rawtoken 4 with
                      | IDENT _ -> begin
                          match self#peek_nth_rawtoken 5 with
                          | SEMICOLON _ -> begin
                              conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 2;
                              conv_nth_token (function T.IDENT x,s,e -> T.OP_MACRO x,s,e | x -> x) 3;
                              conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 4;
                          end
                          | _ -> ()
                      end
                      | _ -> ()
                  end
                  | _ -> ()
              end
              | _ -> ()
            end;
            mk (T.IDENT_V s)
          end

        | LBRACE when begin
            match self#peek_nth_rawtoken 2 with
            | RBRACE -> begin
                match self#peek_nth_rawtoken 3 with
                | DOT -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ LBRACE RBRACE DOT"; token

        | RBRACE(* | LBRACE*) | EOF | RETURN | BREAK | CONTINUE | GOTO | CASE | DEFAULT
        | IF | ELSE | SWITCH | FOR | WHILE | DO | TRY
        | THIS | MS_ASM _ -> DEBUG_MSG "@"; mk (T.STMT_MACRO s)

        | LBRACE when begin
            let nth, l = self#peek_rawtoken_up_to_rbrace ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | DOT -> false
            | _ -> true
        end -> DEBUG_MSG "@"; mk (T.STMT_MACRO s)

        | IDENT x when x = s && begin
            match self#peek_nth_rawtoken 2 with
            | EQ | COMMA -> false
            | _ -> true
        end -> begin
          conv_nth_token (function T.IDENT x,s,e -> T.STMT_MACRO x,s,e | x -> x) 1;
          DEBUG_MSG "@ IDENT"; mk (T.STMT_MACRO s)
        end

        | IDENT _ when begin
            match prev_rawtoken with
            | END_ASM | STMT_MACRO _ -> true
            | _ -> false
        end && begin
          match self#peek_nth_rawtoken 2 with
          | MS_ASM _ -> true
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 3 with
              | MS_ASM _ -> true
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken 4 with
                  | MS_ASM _ -> true
                  | _ -> false
              end
              | _ -> false
          end
          | TY_LPAREN -> begin
              let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
              ll <> [] &&
              List.exists
                (fun (l : T.token list) ->
                  DEBUG_MSG "l=%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                  match l with
                  | (RETURN | DEFAULT | GOTO | CASE | SEMICOLON _)::_ -> true
                  | _ -> false
                ) ll
          end
          | _ -> false
        end -> DEBUG_MSG "@"; mk (T.STMT_MACRO s)

        | IDENT x when prev_rawtoken != LPAREN && begin
            let x_ = Pinfo.encode_ident x in
            let is_bound () =
              let b =
              try
                let spec = env#lookup_obj x_ in
                DEBUG_MSG "%s -> %s" x_ spec#to_string;
                begin
                  try
                    match spec#section_info_opt with
                    | Some i -> i <> env#pp_if_section_top_info
                    | _ -> true
                  with
                    _ -> true
                end
              with
                Not_found -> false
              in
              DEBUG_MSG "%s -> %B" x_ b;
              b
            in
            match self#peek_nth_rawtoken 2 with
            | EQ -> is_bound()
            | LBRACKET -> begin
                let nth, l = self#peek_rawtoken_up_to_rbracket ~from:3 () in
                match self#peek_nth_rawtoken (nth+1) with
                | EQ | DOT -> is_bound()
                | _ -> false
            end
            | TY_LPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                match self#peek_nth_rawtoken (nth+1) with
                | RBRACE -> true
                | IDENT y when y = x -> true
                | IDENT _
                | IF | SWITCH | FOR | WHILE | DO | RETURN | GOTO | CONTINUE | BREAK | TRY -> begin
                    let b =
                      List.exists
                        (fun l ->
                          match (List.rev l : T.token list) with
                          | [x] when is_literal x -> true
                          | IDENT _::(DOT|MINUS_GT)::_ -> true
                          | _ -> false
                        ) ll
                    in
                    if b then begin
                      conv_nth_token (function T.IDENT x,s,e -> T.IDENT_SM x,s,e | x -> x) 1;
                      conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 2;
                    end;
                    b
                end
                | SEMICOLON _ -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | RBRACE -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.STMT_MACRO s)

        | IDENT x when begin
            match self#peek_nth_rawtoken 2 with
            | EQ when not (is_bound x) -> true
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; token

        | IDENT x when begin
            prev_rawtoken == LBRACE &&
            (match prev_rawtoken2 with
            | SEMICOLON _ | RPAREN -> true
            | _ -> false) &&
            match self#peek_nth_rawtoken 2 with
            | RBRACE -> true
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 3 with
                | RBRACE -> true
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 4 with
                    | RBRACE -> true
                    | _ -> false
                end
                | TY_LPAREN -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:4 () in
                    check_if_macro_args ll
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ IDENT"; mk (T.STMT_MACRO s)

        | TY_LPAREN when begin
            match self#peek_nth_rawtoken 2 with
            | LBRACKET -> begin
                match self#peek_nth_rawtoken 3 with
                | RBRACKET | PTR_AMP | PTR_STAR | EQ | ELLIPSIS | THIS -> begin
                    conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 1;
                    conv_nth_token (function T.LBRACKET,s,e -> T.LAM_LBRACKET,s,e | x -> x) 2;
                    true
                end
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 4 with
                    | COMMA | RBRACKET -> begin
                        conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 1;
                        conv_nth_token (function T.LBRACKET,s,e -> T.LAM_LBRACKET,s,e | x -> x) 2;
                        true
                    end
                    | _ -> false
                end
                | _ -> false
            end
            (*| x when is_literal x -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | IDENT x when x = s -> false
                | x when is_stmt_head x -> false
                | _ -> true
            end*)
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_V s)

        | TY_LPAREN when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | COMMA -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_DM s)

        | TY_LPAREN when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | LT_LT | GT_GT | SEMICOLON _ -> check_if_macro_args ll
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_EM s)

        | TY_LPAREN when is_macro_fun s && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | BREAK | CONTINUE | DELETE | FOR | WHILE | IF | SWITCH | THIS | CASE | DEFAULT | EOF -> true
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_SM s)

        | TY_LPAREN when begin
            match prev_rawtoken with
            | LBRACE | RBRACE | SEMICOLON _ | NEWLINE -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | EQ -> true
                | LBRACKET when
                    self#peek_nth_rawtoken (nth+2) == RBRACKET &&
                    self#peek_nth_rawtoken (nth+3) == EQ
                  -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "* @"; mk (T.IDENT_IM s)

        | TY_LPAREN when begin
            match self#peek_nth_rawtoken 2 with
            | GT_GT | LT_LT | PTR_STAR | SLASH | PLUS | MINUS | BAR _ | PTR_AMP
            | TEMPL_LT | TY_TEMPL_GT | LT_EQ | GT_EQ | EXCLAM_EQ _ | EQ_EQ | BAR_BAR _
            | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 3 with
                | RPAREN -> true
                | _ -> false
            end
            (*| RPAREN when begin
                match self#peek_nth_rawtoken 3 with
                | SEMICOLON _ | DOT -> false
                | _ -> true
            end -> true*)(*NG*)
            | _ -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                List.exists
                  (fun (l : T.token list) ->
                    match l with
                    | (RETURN | DEFAULT | GOTO | CASE | SEMICOLON _)::_ -> true
                    | [IDENT x] when is_type_name x || is_type x -> true
                    | [IDENT x] when begin
                        try
                          let _, macro_kind, tok_list_obj = env#find_pending_macro x in
                          match macro_kind with
                          | FunctionLike _ -> begin
                              let tok_list = (Obj.obj tok_list_obj : token list) in
                              match tok_list with
                              | (T.RETURN,_,_|DEFAULT,_,_|GOTO,_,_|CASE,_,_|SEMICOLON _,_,_)::_ -> true
                              | _ -> false
                          end
                          | _ -> false
                        with
                          _ -> false
                    end -> true
                    | [x] when is_basic_ty x || is_op x -> true
                    (*| RBRACE::_ -> list_memqn [T.SEMICOLON _] l*)(*NG?*)
                    | l ->
                        match List.rev l with
                        | (STRUCT|UNION|CLASS|ENUM)::_ -> true
                        | TY_LPAREN::STR_LITERAL _::RPAREN::STR_LITERAL _::_ -> true
                        | _ -> false
                  ) ll ||
                match self#peek_nth_rawtoken (nth+1) with
                | BREAK | CONTINUE | DELETE | FOR | WHILE | IF | SWITCH | THIS | CASE | DEFAULT | EOF -> true

                | IDENT x when x = s -> begin
                    DEBUG_MSG "@";
                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_SM x,s,e | x -> x) (nth+1);
                    true
                end

                | IDENT x when begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | TY_LPAREN -> begin
                        let _, nth1, l1 = self#peek_rawtoken_up_to_rparen ~from:(nth+3) None in
                        match self#peek_nth_rawtoken (nth1+1) with
                        | BREAK | CONTINUE | DELETE | FOR | WHILE | IF | SWITCH | THIS | CASE | DEFAULT -> true
                        | SEMICOLON _ -> true
                        | IDENT y when y = x || y = s -> begin
                            conv_nth_token (function T.IDENT x,s,e -> T.IDENT_SM x,s,e | x -> x) (nth+1);
                            conv_nth_token (function T.IDENT x,s,e -> T.IDENT_SM x,s,e | x -> x) (nth1+1);
                            true
                        end
                        | IDENT y when begin
                            is_bound y ||
                            match self#peek_nth_rawtoken (nth1+2) with
                            | TY_LPAREN -> begin
                                let _, nth2, l2 =
                                  self#peek_rawtoken_up_to_rparen ~from:(nth1+3) None
                                in
                                match self#peek_nth_rawtoken (nth2+1) with
                                | BREAK | CONTINUE | DELETE | FOR | WHILE | IF | SWITCH | THIS | CASE | DEFAULT -> true
                                | SEMICOLON _ -> true
                                | IDENT z -> z = y || z = x || z = s
                                | _ -> false
                            end
                            | _ -> false
                        end -> true
                        | _ -> false
                    end
                    | EQ -> true
                    | _ -> false
                end -> true
                | PTR_STAR -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken (nth+3) with
                        | MINUS_GT | DOT -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | SEMICOLON _ -> false
                | PP_UNDEF | PP_DEFINE | PP_INCLUDE | PP_IMPORT
                | PP_LINE | PP_ERROR | PP_PRAGMA | PP_UNKNOWN _ -> true
                | _ -> false
            end
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_SM s)

        | TY_LPAREN when begin
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
            match self#peek_nth_rawtoken (nth+1) with
            | PTR_STAR -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | THIS -> false
                | TY_LPAREN -> begin
                    let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+3) () in
                    match self#peek_nth_rawtoken (nth'+1) with
                    | EQ -> false
                    | _ -> true
                end
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | LT_LT -> false
                    | GT_GT when env#templ_param_arg_level < 2 -> false
                    | _ -> true
                end
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_TM s)

        | TY_LPAREN when env#stack#at_block && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            (match self#peek_nth_rawtoken (nth+1) with
            | SEMICOLON _ -> true
            | AMP_AMP _ | BAR_BAR _
            | PLUS | MINUS | STAR | SLASH | PERC
            | HAT _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
            | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
            | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
            | LT | GT | TY_TEMPL_GT
            | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> true
            | PLUS_PLUS | MINUS_MINUS when begin
                match self#peek_nth_rawtoken (nth+2) with
                | IDENT _ -> false
                | _ -> true
            end -> true
            | _ -> false) &&
            match ll with
            | [l] when begin
                match (List.rev l : T.token list) with
                | IDENT _::(DOT|MINUS_GT)::IDENT _::TY_LPAREN::r -> begin
                    let nth', _ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:6 () in
                    match self#peek_nth_rawtoken (nth'+1) with
                    | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end -> true
            | _ ->
            List.for_all
              (function
                | [x] -> begin
                    match x with
                    | T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                    | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                    | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> true
                    | IDENT x -> not (is_type_name x || is_type x)
                    | _ -> false
                end
                | _ -> false
              ) ll(* ||
            not (check_if_macro_args ll)*)
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_V s)

        | TY_LPAREN when begin
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
            match self#peek_nth_rawtoken (nth+1) with
            | COLON -> true
            | TY_LPAREN -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
                | PTR_STAR | PTR_AMP -> begin
                    conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth+1);
                    true
                end
                | _ when self#is_lparen ~from:(nth+2) () -> begin
                    conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth+1);
                    true
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_IM s)

        | TY_LPAREN when begin
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
            match self#peek_nth_rawtoken (nth+1) with
            | SEMICOLON _ -> begin
                match l with
                | RPAREN::rest -> begin
                    match List.rev rest with
                    | TY_LPAREN::rest2 -> begin
                        DEBUG_MSG "rest2=%s"(String.concat ";" (List.map Token.rawtoken_to_string rest2));
                        let count =
                          try
                            List.fold_left
                              (fun c x ->
                                match (x : T.token) with
                                | TY_LPAREN | LPAREN -> c + 1
                                | RPAREN when c = 0 -> raise Exit
                                | RPAREN -> c - 1
                                | _ -> c
                              ) 0 rest2
                          with
                            Exit -> -1
                        in
                        DEBUG_MSG "count=%d" count;
                        count = 0
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_SM s)

        | PP_IF | PP_IFDEF | PP_IFNDEF when begin
            let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | RETURN | GOTO | IF | SWITCH | FOR | WHILE | DO -> true
            | _ -> false
        end -> mk (T.STMT_MACRO s)

        (*| TEMPL_LT when begin
            match self#peek_nth_rawtoken 2 with
            | x when is_ty x -> true
            | _ -> false
        end -> DEBUG_MSG "@ TEMPL_LT"; token*)(* this prevents correct conversions of following tokens *)

        | _ when is_type_name s || is_type s -> DEBUG_MSG "@"; token

        | x when is_literal x && is_semicolon (self#peek_nth_rawtoken 2) -> DEBUG_MSG "@"; mk (T.OP_MACRO s)

        | _ -> begin
            try
              if
                match self#peek_rawtoken() with
                | TY_LPAREN -> false
                | _ -> true
              then
                mk (self#find_ident_conv s)
              else
                raise Not_found
            with
              Not_found ->
            let cands =
              match self#peek_rawtoken() with
              | TY_LPAREN | LPAREN -> [mk (T.IDENT_V s); mk (T.IDENT_SM s);]
              | IDENT _ -> [mk (T.IDENT_V s); mk (T.STMT_MACRO s)]
              | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                  let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in
                  match self#peek_nth_rawtoken (nth+1) with
                  | TY_LPAREN | LPAREN -> [mk (T.IDENT_V s); mk (T.IDENT_SM s);]
                  | IDENT _ -> [mk (T.IDENT_V s); mk (T.STMT_MACRO s)]
                  | _ -> [mk (T.IDENT_V s)]
              end
              | PP_ELIF | PP_ELSE | PP_ENDIF -> begin
                  [mk (T.IDENT_V s); mk (T.STMT_MACRO s)]
              end
              | _ -> [mk (T.IDENT_V s)]
            in
            self#init_replay_queue sn (env#get_pstat()) cands;
            token
        end
    end
    | STMT, sc -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | CONST when begin
            match prev_rawtoken2 with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
            | _ -> false
        end -> DEBUG_MSG "CONST @"; mk (T.IDENT_V s)

        | CONST when begin
            match self#peek_rawtoken() with
            | EQ -> true
            | _ -> false
        end -> DEBUG_MSG "CONST @ EQ"; mk (T.IDENT_V s)

        | TY_LPAREN | CONST | NAMESPACE -> DEBUG_MSG "(TY_LPAREN|CONST|NAMESPACE) @"; token

        | STRUCT | UNION | ELAB_ENUM -> DEBUG_MSG "(STRUCT|UNION|ELAB_ENUM) @"; token

        | GOTO when begin
            match self#peek_rawtoken() with
            | SEMICOLON _ | COMMA | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "GOTO @"; token

        | CASE when begin
            match self#peek_rawtoken() with
            | COLON_COLON | TEMPL_LT -> true
            | _ -> false
        end -> DEBUG_MSG "CASE @"; token

        | AUTO | DOT | QUEST | RETURN when begin
            match self#peek_rawtoken() with
            | COLON_COLON | TEMPL_LT -> false
            | _ -> true
        end -> DEBUG_MSG "(AUTO|DOT|QUEST|RETURN) @"; mk (T.IDENT_V s)

        | MINUS_GT when begin
            match self#peek_rawtoken() with
            | COLON_COLON(* | TEMPL_LT*) | LBRACE -> true
            | _ when env#trailing_retty_flag -> true
            | _ -> false
        end -> DEBUG_MSG "MINUS_GT @"; token

        (*| MINUS_GT -> mk (T.IDENT_V s)*)

        | USING when self#peek_rawtoken() == EQ -> DEBUG_MSG "USING @"; token

        | EQ when env#using_flag && env#alias_flag && begin
            match self#peek_rawtoken() with
            | TEMPL_LT -> true
            | TY_LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "EQ @"; token

        | LBRACKET when self#peek_rawtoken() == TY_LPAREN && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            check_if_macro_args ll
        end -> DEBUG_MSG "LBRACKET @"; mk (T.IDENT_EM s)

        | LBRACKET when begin
            match self#peek_rawtoken() with
            | COLON_COLON | TEMPL_LT -> false
            | _ -> true
        end -> DEBUG_MSG "LBRACKET @"; mk (T.IDENT_V s)

        | OBJC_LBRACKET when self#peek_rawtoken() != COLON_COLON -> DEBUG_MSG "OBJ_LBRACKET @"; mk (T.IDENT_V s)

        | TY_TEMPL_GT when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
                match self#peek_nth_rawtoken (nth+1) with
                | TY_LPAREN -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "TY_TEMPL_GT @"; mk (T.IDENT_IM s)

        | TY_TEMPL_GT when begin
            match self#peek_rawtoken() with
            | IDENT _ -> true
            | _ -> false
        end -> DEBUG_MSG "TY_TEMPL_GT @"; token

        | STAR | AMP _ when prev_rawtoken2 == DELETE && begin
            match self#peek_rawtoken() with
            | TEMPL_LT -> true
            | _ -> false
        end -> DEBUG_MSG "(STAR|AMP) @"; token

        | TY_TEMPL_GT | GT_GT | LT_LT | GT | LT | GT_EQ | LT_EQ
        | BAR _ | AMP _ | HAT _ | EXCLAM _ | PERC | PLUS | MINUS | STAR | SLASH
        | AMP_AMP _ | BAR_BAR _ | EQ_EQ | EXCLAM_EQ _ | DOT | MINUS_GT when begin
            match self#peek_rawtoken() with
            | COLON_COLON | LBRACE -> false
            | TEMPL_LT when env#paren_level > 0 && begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match List.rev ll with
                | _l::_ -> begin
                    let l = List.rev _l in
                    DEBUG_MSG "%s"
                      (String.concat ";" (List.map Token.rawtoken_to_string l));
                    templ_param_arg_balanced l
                end
                | _ -> false
            end -> false
            | TEMPL_LT when begin
                match self#peek_nth_rawtoken 2 with
                | x when is_ty x -> true
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_TEMPL_GT -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> false
            | _ -> true
        end -> DEBUG_MSG "... @"; mk (T.IDENT_V s)

        | IDENT _ when begin
            match self#peek_rawtoken() with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | LBRACKET -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT @"; token

        | IDENT _ when begin
            match self#peek_rawtoken() with
            | COLON_COLON -> false
            | RPAREN -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                    | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                    | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                    | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> false
                    | _ -> true
                end
                | _ -> true
            end
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | EQ -> false
                | _ -> true
            end
            | _ -> true
        end -> DEBUG_MSG "IDENT @"; mk (T.IDENT_V s)

        | RPAREN when begin
            match prev_rawtoken2 with
            | AUTO -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; mk (T.IDENT_V s)

        | COMMA when not env#macro_arg_flag && begin
            match prev_rawtoken2 with
            | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> true
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
            | INT | LONG | TYPE_MACRO _ -> env#type_paren_flag
            | IDENT _ when env#type_paren_flag -> true
            | _ -> false
        end -> DEBUG_MSG "COMMA @"; token

        | TEMPL_LT | COMMA when begin
            match self#peek_rawtoken() with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA | TY_TEMPL_GT -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(TEMPL_LT|COMMA) @"; token

        | LPAREN when prev_rawtoken2 == ELLIPSIS && prev_rawtoken3 == SIZEOF -> DEBUG_MSG "LPAREN @"; token

        | LPAREN | COMMA when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> true
            | _ -> false
        end && begin
          let _, _, l =
            self#peek_rawtoken_up_to_rparen ~level:env#paren_level None
          in
          match List.rev l with
          | TY_LPAREN::IDENT _::TY_LPAREN::rest -> begin
              match List.rev rest with
              | RPAREN::rest -> begin
                  let ll = split_at_comma (List.rev rest) in
                  DEBUG_MSG "\n%s"
                    (String.concat "\n"
                       (List.map
                          (fun x -> String.concat ";"
                              (List.map Token.rawtoken_to_string x)) ll));
                  List.exists ty_pat1 ll
              end
              | _ -> false
          end
          | _ -> false
        end -> DEBUG_MSG "(LPAREN|COMMA) @"; mk (T.IDENT_EM s)

        | LPAREN -> begin
            match self#peek_rawtoken() with
            | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _
            | PP_STRINGIZED _ -> DEBUG_MSG "LPAREN @ ..."; mk (T.STR_MACRO s)

            | IDENT _ when begin
                self#peek_nth_rawtoken 2 == RPAREN &&
                match self#peek_nth_rawtoken 3 with
                | EOF | SEMICOLON _ -> true
                | _ -> false
            end -> begin
              DEBUG_MSG "LPAREN @ IDENT RPAREN";
              conv_nth_token (function T.IDENT x,s,e -> T.ARGS_MACRO x,s,e | x -> x) 1;
              mk (T.IDENT_V s)
            end

            | COLON_COLON | IDENT _ | IDENT_V _ -> DEBUG_MSG "LPAREN @ (COLON_COLON|IDENT|IDENT_V)"; token

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                match self#peek_nth_rawtoken 2 with
                | RPAREN | COMMA -> true
                | _ -> false
            end -> DEBUG_MSG "LPAREN @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; token

            | TEMPL_LT when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match List.rev ll with
                | _l::_ -> begin
                    let l = List.rev _l in
                    DEBUG_MSG "%s"
                      (String.concat ";" (List.map Token.rawtoken_to_string l));
                    templ_param_arg_balanced l
                end
                | _ -> false
            end -> DEBUG_MSG "LPAREN @ TEMPL_LT"; token

            | COLON when env#macro_arg_flag && self#peek_nth_rawtoken 2 == RPAREN
              -> DEBUG_MSG "LPAREN @ COLON RPAREN"; token

            | TY_LPAREN when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                check_if_macro_args ll
            end -> DEBUG_MSG "LPAREN @ TY_LPAREN"; mk (T.IDENT_EM s)

            | _ -> begin
                begin
                  match prev_rawtoken2 with
                  | WHILE -> self#ctx_expr(); self#ctx_ini()
                  | _ -> ()
                end;
                DEBUG_MSG "LPAREN @ *";
                mk (T.IDENT_V s)
            end
        end

        | COLON_COLON when sc == IN_CASE_LABEL && self#peek_rawtoken() == RPAREN ->
            DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when env#type_paren_flag && self#peek_rawtoken() == RPAREN ->
            DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when begin
            match self#peek_rawtoken() with
            | GT_GT -> true
            | _ -> false
        end && begin
          match prev_rawtoken2 with
          | IDENT _ -> begin
              match prev_rawtoken3 with
              | SEMICOLON _ | LBRACE | RBRACE -> true
              | _ -> false
          end
          | _ -> false
        end -> DEBUG_MSG "IDENT COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when env#using_ns_flag && is_semicolon (self#peek_rawtoken()) -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when self#peek_rawtoken() == RBRACKET -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when env#templ_arg_flag && begin
            match prev_rawtoken2 with
            | IDENT _ -> begin
                match prev_rawtoken3 with
                | COLON -> is_literal prev_rawtoken4
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when env#typename_flag && begin
            match self#peek_rawtoken() with
            | EQ | TY_TEMPL_GT | COMMA | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when env#paren_level > 0 && not env#type_paren_flag && begin
            match self#peek_rawtoken() with
            | PLUS | MINUS | SLASH | PERC | BAR_BAR _ -> true
            | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ when self#peek_nth_rawtoken 3 == COLON -> false
                | _ -> true
            end
            | TY_TEMPL_GT -> is_literal (self#peek_nth_rawtoken 2)
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when begin
            match prev_rawtoken2 with
            | IDENT _ -> begin
                match prev_rawtoken3 with
                | LPAREN -> prev_rawtoken4 == TEMPL_LT
                | LT when self#peek_rawtoken() != TEMPL_LT -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | SEMICOLON _ | COLON | NEWLINE when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                match self#peek_nth_rawtoken 2 with
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
                | CLASS | STRUCT | UNION -> true
                | _ -> begin
                    let nth, _ =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                    in
                    match self#peek_nth_rawtoken (nth+1) with
                    | RETURN | GOTO | BREAK | CONTINUE
                    | IF | SWITCH | CASE | DEFAULT | FOR | WHILE | DO | TRY -> true
                    | ELSE -> true
                    | PP_UNDEF -> true
                    | IDENT _ -> true(*self#peek_nth_rawtoken (nth+2) == TY_LPAREN*)
                    | _ -> false
                end
            end
            | _ -> false
        end -> DEBUG_MSG "(SEMICOLON|COLON|NEWLINE) @"; mk (T.IDENT_SM s)

        | SEMICOLON _ | COLON | NEWLINE | ODD_RBRACE | RBRACE when begin
            match self#peek_rawtoken() with
            | RETURN | GOTO | BREAK | CONTINUE
            | IF | SWITCH | CASE | DEFAULT | FOR | WHILE | DO | TRY -> true
            | ELSE -> true
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | DOT | MINUS_GT -> true
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "(SEMICOLON|COLON|NEWLINE|ODD_RBRACE|RBRACE) @"; mk (T.STMT_MACRO s)

        | SEMICOLON _ | COLON | NEWLINE when begin
            match self#peek_rawtoken() with
            | GT_GT | LT_LT | RPAREN -> true
            | TY_LPAREN -> begin
                let nth, ll =
                  self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                in
                List.exists
                  (function
                    | h::_ -> DEBUG_MSG "%s" (Token.rawtoken_to_string h); is_literal h
                    | _ -> false
                  ) ll &&
                match self#peek_nth_rawtoken (nth+1) with
                | RBRACE | ELSE -> false
                | RETURN | GOTO | BREAK | CONTINUE
                | IF | SWITCH | FOR | WHILE | DO | TRY | CASE | DEFAULT -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "(SEMICOLON|COLON|NEWLINE) @"; mk (T.IDENT_V s)

        | COLON when begin
            match self#peek_rawtoken() with
            | COLON_COLON -> false
            | TEMPL_LT when env#templ_param_arg_level > 0 -> false
            | _ -> true
        end -> DEBUG_MSG "COLON @"; mk (T.IDENT_V s)

        | MINUS_GT_STAR | DOT_STAR | CASE when
            match self#peek_rawtoken() with
            | COLON_COLON | TEMPL_LT -> false
            | _ -> true
          -> DEBUG_MSG "(MINUS_GT_STAR|DOT_STAR|CASE) @"; mk (T.IDENT_V s)

        | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ | EOF when begin
            self#peek_rawtoken() == TY_LPAREN &&
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~level:env#paren_level None in
            match self#peek_nth_rawtoken (nth+1) with
            | STR_LITERAL _ | USER_STR_LITERAL _ -> true
            | PP_ENDIF -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | STR_LITERAL _ | USER_STR_LITERAL _ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.IDENT_LM s)

        | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _
        | PP_STRINGIZED _ -> DEBUG_MSG "* @"; mk (T.STR_MACRO s)

        | COMMA when begin
            match self#peek_rawtoken() with
            | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ | PP_STRINGIZED _ -> true
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.STR_MACRO s)

        | IF -> DEBUG_MSG "IF @"; mk (T.IDENT_EM s)

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ when env#for_flag && begin
            match self#peek_rawtoken() with
            | COLON -> true
            | _ -> false
        end -> DEBUG_MSG "* @"; mk (T.IDENT_V s)

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | IDENT _ | TYPE_MACRO _ when begin
            match self#peek_rawtoken() with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true (* e.g. far pointer *)
            | _ -> false
        end -> token

        | IDENT_V _ when not env#in_objc_message_expr && begin
            match prev_rawtoken2 with
            | DOT | MINUS_GT -> begin
                match self#peek_rawtoken() with
                | SEMICOLON _ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; mk (T.ARGS_MACRO s)

        | IDENT_V _ | RBRACKET when not env#in_objc_message_expr && begin
            sub_context == END_OF_ID_EXPR &&
            match self#peek_rawtoken() with
            | SEMICOLON _ | LBRACE | EQ -> true
            | _ -> false
        end -> DEBUG_MSG "(IDENT_V|RBRACKET) @"; mk (T.ATTR_MACRO s)

        | IDENT_V _ | RBRACKET when env#in_objc_message_expr && begin
            match self#peek_rawtoken() with
            | COLON -> true
            | _ -> false
        end -> DEBUG_MSG "(IDENT_V|RBRACKET) @"; mk (T.IDENT_V s)

        | STATIC(* | IDENT _*) when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll =
                  self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                in
                match self#peek_nth_rawtoken (nth+1) with
                | SEMICOLON _ -> true
                    (*List.exists
                      (function
                        | [x] -> DEBUG_MSG "%s" (Token.rawtoken_to_string x); is_literal x
                        | _ -> false
                      ) ll*)
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "STATIC @"; mk (T.IDENT_DM s)

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when prev_rawtoken2 == TY_LPAREN && begin
            match self#peek_rawtoken() with
            | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP) @"; mk (T.IDENT_V s)

        | _ -> begin
            DEBUG_MSG "* @";
            match self#peek_rawtoken() with

            | HAT _ when (env#type_paren_flag || env#templ_arg_flag) && begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COMMA | RPAREN -> true
                    | _ -> false
                end
                | TY_TEMPL_GT -> true
                | _ -> false
            end -> DEBUG_MSG "* @ HAT"; token

            | PLUS_PLUS | MINUS_MINUS | MINUS_GT | DOT
            | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | LT_LT_EQ | GT_GT_EQ
            | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | EQ_EQ | EXCLAM_EQ _ | GT_EQ | LT_EQ | LT_LT
            | MINUS | PLUS | SLASH | PERC | HAT _ | BAR _ | BAR_BAR _ | QUEST -> begin
                DEBUG_MSG "@";
                self#ctx_expr();
                mk (T.IDENT_V s)
            end

            | GT_GT when env#templ_param_arg_level < 2 -> begin
                DEBUG_MSG "@";
                self#ctx_expr();
                mk (T.IDENT_V s)
            end

            | LBRACKET when begin
                match self#peek_nth_rawtoken 2 with
                | RBRACKET when env#templ_arg_flag -> true
                | _ -> false
            end -> DEBUG_MSG "@"; token

            | LBRACKET -> begin
                DEBUG_MSG "@";
                self#ctx_expr();
                mk (T.IDENT_V s)
            end

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when env#templ_arg_flag && begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> true
                | _ -> false
            end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | TEMPL_LT when begin
                match self#peek_nth_rawtoken 2 with
                | INT_LITERAL _ | USER_INT_LITERAL _ | FLOAT_LITERAL _ | USER_FLOAT_LITERAL _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | PTR_AMP_AMP | BAR_BAR _ -> true
                    | _ -> false
                end
                | IDENT _ when env#paren_level > 0 && env#templ_param_arg_level = 0 && begin
                    let _, nth, _l = self#peek_rawtoken_up_to_rparen ~filt:is_semicolon None in
                    let l = List.rev _l in
                    not (templ_param_arg_balanced l)
                end -> true
                | _ -> false
            end -> begin
              DEBUG_MSG "@";
              self#ctx_expr();
              mk (T.IDENT_V s)
            end

            | TY_TEMPL_GT when begin
                match self#peek_nth_rawtoken 2 with
                | INT_LITERAL _ | USER_INT_LITERAL _ | FLOAT_LITERAL _ | USER_FLOAT_LITERAL _ -> true
                | _ -> false
            end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

            | COLON -> begin
                match sc with
                | IN_CASE_LABEL -> begin
                    DEBUG_MSG "@ COLON";
                    self#ctx_ini();
                    mk (T.IDENT_V s)
                end

                | _ when begin
                    env#paren_level > 0 &&
                    not env#type_paren_flag &&
                    match prev_rawtoken with
                    | ENUM | CLASS | STRUCT | UNION -> false
                    | COMMA | LPAREN when env#macro_arg_flag && begin
                        match self#peek_nth_rawtoken 2 with
                        | COMMA | RPAREN -> true
                        | _ -> false
                    end -> false
                    | _ -> true
                end -> DEBUG_MSG "@ COLON"; mk (T.IDENT_V s)

                | _ when begin
                    match prev_rawtoken with
                    | RBRACE | SEMICOLON _ -> true
                    | _ -> false
                end -> DEBUG_MSG "@ COLON"; token

                | _ -> begin
                    match self#peek_nth_rawtoken 2 with
                    | LBRACE when begin
                        match prev_rawtoken with
                        | RBRACE | ODD_RBRACE -> true
                        | _ -> false
                    end -> DEBUG_MSG "@ COLON LBRACE"; token

                    | LBRACE | EQ -> DEBUG_MSG "@ COLON (LBRACE|EQ)"; mk (T.IDENT_V s)

                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> DEBUG_MSG "@ COLON IDENT RPAREN"; mk (T.IDENT_V s)
                        | _ -> DEBUG_MSG "@ COLON IDENT"; token
                    end
                    | _ -> DEBUG_MSG "@"; token
                end
            end

            | SEMICOLON _ when begin
                DEBUG_MSG "RPAREN @ SEMICOLON";
                prev_rawtoken == RPAREN &&
                not env#typename_flag &&
                env#end_of_id_macro_call_flag &&
                env#in_body_brace_flag
            end -> mk (T.ARGS_MACRO s)

            | SEMICOLON _ -> begin
                DEBUG_MSG "* @ SEMICOLON";
                match prev_rawtoken with
                | GOTO -> token
                | COLON_COLON | EQ when env#using_flag && env#alias_flag || env#ns_alias_flag -> token
                | _ -> mk (T.IDENT_V s)
            end

            | TY_LPAREN when begin
                not env#macro_arg_flag && not env#arg_paren_flag &&
                match self#peek_nth_rawtoken 2 with
                | GT_GT | LT_LT | PTR_STAR | SLASH | PLUS | MINUS | BAR _ | PTR_AMP
                | TEMPL_LT | TY_TEMPL_GT | LT_EQ | GT_EQ | EXCLAM_EQ _ | EQ_EQ | BAR_BAR _
                | PTR_AMP_AMP -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> true
                    | _ -> false
                end
                | _ -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | RETURN | GOTO | BREAK | CONTINUE | IF | SWITCH | FOR | WHILE | DO -> true
                    | _ -> false
                end
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_SM s)

            | TY_LPAREN when check_if_abst_dtor_ident() -> DEBUG_MSG "@ TY_LPAREN"; token

            | TY_LPAREN when begin
                match prev_rawtoken with
                | COLON_COLON | HEAD_COLON_COLON -> false
                | _ -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    DEBUG_MSG "\n%s"
                      (String.concat "\n"
                         (List.map
                            (fun x -> String.concat ";"
                                (List.map Token.rawtoken_to_string x)) ll));
                    match self#peek_nth_rawtoken (nth+1) with
                    | IDENT x when x = s -> begin
                        conv_nth_token (function T.IDENT x,s,e -> T.IDENT_DSM x,s,e | x -> x) (nth+1);

                        if self#peek_nth_rawtoken (nth+2) == TY_LPAREN then begin
                          let rec conv from =
                            let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from () in
                            match self#peek_nth_rawtoken (nth'+1) with
                            | IDENT x' when x' = s && self#peek_nth_rawtoken (nth'+2) == TY_LPAREN -> begin
                                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_DSM x,s,e | x -> x) (nth'+1);
                                conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth'+2);
                                conv (nth'+3)
                            end
                            | _ -> ()
                          in
                          conv (nth+3)
                        end;

                        true
                    end
                    | _ ->
                    List.exists
                      (fun (l : T.token list) ->
                        match l with
                        | [RBRACE; LBRACE] -> false
                        | (SEMICOLON _|RBRACE)::_ -> true
                        | _ -> false
                      ) ll
                end
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_DSM s)

            | TY_LPAREN when env#templ_arg_flag && begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | TEMPL_LT -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | TYPENAME | CONST -> true
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken (nth+3) with
                        | COMMA -> begin
                            match self#peek_nth_rawtoken (nth+4) with
                            | TYPENAME | CONST -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_IM s)

            | IDENT _ | TY_LPAREN when env#expr_flag -> DEBUG_MSG "@ (IDENT|TY_LPAREN)"; mk (T.OP_MACRO s)

            | ELLIPSIS when env#param_head_flag -> DEBUG_MSG "@ ELLIPSIS"; token

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                prev_rawtoken == COMMA && env#param_head_flag
            end -> DEBUG_MSG "@ (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; token

            | _ -> begin
                DEBUG_MSG "* @ *";
                if keep_flag then begin
                  match sc with
                  | IN_SIMPLE_TEMPL_ID -> begin
                      match self#peek_rawtoken() with
                      | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                          match self#peek_nth_rawtoken 2 with
                          | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
                          | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> true
                          | _ -> false
                      end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP) ..."; mk (T.IDENT_V s)

                      | _ -> DEBUG_MSG "* @ *"; token
                  end
                  | _ -> begin
                      match self#peek_rawtoken() with
                      | COLON_COLON | IDENT _ | TEMPL_LT -> DEBUG_MSG "* @ (COLON_COLON|IDENT|TEMPL_LT)"; token

                      | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                          match self#peek_nth_rawtoken 2 with 
                          | COMMA | RPAREN -> true
                          | IDENT _ when env#type_paren_flag && begin
                              match self#peek_nth_rawtoken 3 with 
                              | COMMA | RPAREN -> true
                              | _ -> false
                          end -> true
                          | _ -> false
                      end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP) (COMMA|RPAREN)"; token

                      | _ -> DEBUG_MSG "* @ *"; mk (T.IDENT_V s)
                  end
                end
                else begin
                  DEBUG_MSG "* @ *";
                  match self#peek_rawtoken() with
                  | IDENT _ | IDENT_V _
                  | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _ | NULLPTR
                  | STR_LITERAL _ | PP_STRINGIZED _ | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                  | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ | TY_LPAREN when check_if_arg_macro()
                    -> DEBUG_MSG "@ *"; mk (T.ARG_MACRO s)

                  | COLON_COLON -> DEBUG_MSG "@ COLON_COLON"; token

                  | IDENT x when is_args_macro x || is_args_macro_ident x -> DEBUG_MSG "@ IDENT"; mk (T.IDENT_V s)

                  | IDENT _ -> DEBUG_MSG "@ IDENT"; token

                  | RPAREN when not env#type_paren_flag && begin
                      match self#peek_nth_rawtoken 2 with
                      | PTR_AMP_AMP | BAR_BAR _ -> true
                      | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
                      | AMP _ | BAR _ | EXCLAM _ | MINUS_GT | DOT -> true
                      | _ -> false
                  end -> DEBUG_MSG "@ RPAREN"; mk (T.IDENT_V s)

                  | PTR_STAR | PTR_AMP when prev_rawtoken != TYPEDEF && begin
                      match self#peek_nth_rawtoken 2 with
                      | INT_LITERAL _ | FLOAT_LITERAL _
                      | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                      | TILDE _ | SIZEOF -> true
                      | TY_LPAREN -> not (check_if_abst_dtor_ident ~nth:3 ())
                      | IDENT _ when begin
                          match self#peek_nth_rawtoken 3 with
                          | PLUS_PLUS | MINUS_MINUS | MINUS_GT | DOT
                          | EQ_EQ | EXCLAM_EQ _ | GT_EQ | LT_EQ | LT_LT
                          | MINUS | PLUS | SLASH | PERC | HAT _ | BAR _ | BAR_BAR _ | QUEST -> true
                          | GT_GT when env#templ_param_arg_level < 2 -> true
                          | _ -> false
                      end -> true
                      | _ -> false
                  end -> DEBUG_MSG "@ (PTR_STAR|PTR_AMP)"; mk (T.IDENT_V s)

                  | PTR_STAR | PTR_AMP when begin
                      match self#peek_nth_rawtoken 2 with
                      | IDENT _ -> begin
                          match prev_rawtoken with
                          | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT
                          | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
                          | CONST -> true
                          | _ -> false
                      end
                      | TY_LPAREN when check_if_abst_dtor_ident ~nth:3 () -> true
                      | _ -> false
                  end -> DEBUG_MSG "type @ (PTR_STAR|PTR_AMP) IDENT"; token

                  | TY_LPAREN when begin
                      match prev_rawtoken with
                      | TYPEDEF -> begin
                          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                          match self#peek_nth_rawtoken (nth+1) with
                          | TY_LPAREN when List.length ll = 1 -> false
                          | _ -> true
                      end
                      | _ -> false
                  end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_TM s)

                  | TY_LPAREN when begin
                      match prev_rawtoken with
                      | THROW -> true
                      | _ -> false
                  end -> DEBUG_MSG "@ TY_LPAREN"; token

                  | TY_LPAREN when begin
                      env#templ_param_arg_level = 0 && not env#type_paren_flag && not env#arg_paren_flag &&
                      let nth, ll =
                        self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                      in
                      DEBUG_MSG "\n%s"
                        (String.concat "\n"
                           (List.map
                              (fun x -> String.concat ";"
                                  (List.map Token.rawtoken_to_string x)) ll));
                      List.exists
                        (fun l ->
                          match List.rev l with
                          | [T.IDENT x] -> is_type_name x || is_type x
                          | T.TY_LPAREN::COMMA::_ -> true
                          | _ -> false
                        ) ll ||
                      match self#peek_nth_rawtoken (nth+1) with
                      | IF | SWITCH | FOR | WHILE | CASE | RBRACE -> true
                      | _ -> false
                  end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_SM s)

                  | TY_LPAREN when begin
                      let nth, ll =
                        self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                      in
                      match self#peek_nth_rawtoken (nth+1) with
                      | IDENT _ -> is_semicolon (self#peek_nth_rawtoken (nth+2))
                      | _ -> false
                  end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_TM s)

                  | TY_LPAREN when begin
                      (env#macro_arg_flag || env#arg_paren_flag || env#templ_arg_flag) &&
                      match prev_rawtoken with
                      | LPAREN | COMMA -> begin
                          let nth, ll =
                            self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                          in
                          check_if_macro_args ll &&
                          match self#peek_nth_rawtoken (nth+1) with
                          | COMMA | RPAREN | TY_TEMPL_GT -> true
                          | _ -> false
                      end
                      | _ -> false
                  end -> DEBUG_MSG "(LPAREN|COMMA) @ TY_LPAREN"; mk (T.IDENT_EM s)

                  | TY_LPAREN when begin
                      match prev_rawtoken with
                      | RBRACE | SEMICOLON _ | LBRACE | COLON | BLOCK_HEAD_MACRO _ -> begin
                          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                          match self#peek_nth_rawtoken (nth+1) with
                          | DOT | MINUS_GT | SEMICOLON _ -> check_if_macro_args ll
                          | _ -> false
                      end
                      | _ -> false
                  end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_EM s)

                  | TY_LPAREN when begin
                      let nth, ll =
                        self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                      in
                      match self#peek_nth_rawtoken (nth+1) with
                      | DOT | MINUS_GT | SEMICOLON _ -> true
                      | _ -> false
                  end -> DEBUG_MSG "@ TY_LPAREN"; mk (T.IDENT_V s)

                  | RBRACKET when env#in_objc_message_expr -> DEBUG_MSG "@ RBRACKET"; mk (T.IDENT_V s)

                  | (*COMMA*)x when begin
                      x != COLON_COLON && x != TEMPL_LT &&
                      match prev_rawtoken with
                      | COLON_COLON -> begin
                          match prev_rawtoken2 with
                          | TEMPL_GT -> not env#last_ty_templ_id_flag
                          | IDENT _ -> begin
                              match prev_rawtoken3 with
                              | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
                              | AMP _ | BAR _ | EXCLAM _ -> true
                              | LPAREN when not env#for_flag && not env#macro_arg_flag -> true
                              | _ -> false
                          end
                          | _ -> false
                      end
                      | COMMA -> begin
                          match self#peek_rawtoken() with
                          | RPAREN when not env#type_paren_flag -> true
                          | _ -> false
                      end
                      | _ -> false
                  end -> DEBUG_MSG "(COLON_COLON|COMMA) @ *"; mk (T.IDENT_V s)

                  | RPAREN | COMMA when env#macro_arg_flag && not env#type_paren_flag
                    -> DEBUG_MSG "@ (RPAREN|COMMA)"; mk (T.IDENT_V s)

                  | RPAREN | COMMA when env#arg_paren_flag && prev_rawtoken == COMMA
                    -> DEBUG_MSG "@ (RPAREN|COMMA)"; mk (T.IDENT_V s)

                  | _ -> begin
                      DEBUG_MSG "* @ *";
                      match prev_rawtoken with
                      | TYPEDEF -> DEBUG_MSG "TYPEDEF @ *"; token

                      | EQ | LT_LT | PLUS_PLUS | MINUS_MINUS
                      | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                      | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
                      | TY_TEMPL_GT | DELETE -> DEBUG_MSG "(EQ|...) @ *"; mk (T.IDENT_V s)

                      | MINUS_GT | DOT when self#peek_rawtoken() != TEMPL_LT
                        -> DEBUG_MSG "(MINUS_GT|DOT) @ *"; mk (T.IDENT_V s)

                      | _ when prev_rawtoken != COLON_COLON && is_val s -> DEBUG_MSG "* @ *"; mk (T.IDENT_V s)

                      | RPAREN when env#lambda_dtor_flag && begin
                          match self#peek_rawtoken() with
                          | LBRACE -> true
                          | _ -> false
                      end -> DEBUG_MSG "RPAREN @ LBRACE"; mk (T.ATTR_MACRO s)

                      | SWITCH when self#peek_rawtoken() == TY_LPAREN -> DEBUG_MSG "SWITCH @"; mk (T.IDENT_EM s)

                      | _ -> DEBUG_MSG "* @ *"; token
                  end
                end
            end
        end
    end

    | MEM_INIT, _ when begin
        (prev_rawtoken2 == LBRACKET || prev_rawtoken2 == EOF)  &&
        prev_rawtoken == LBRACKET &&
        self#peek_rawtoken() == RBRACKET
    end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

    | MEM_INIT, _ when begin
        match prev_rawtoken with
        | COLON when env#end_of_params_flag && self#peek_rawtoken() == TY_LPAREN && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | TY_LPAREN -> begin
                    let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+3) () in
                    let b = not (check_if_macro_args ll') in
                    if b then
                      conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth+1);
                    b
                end
                | _ -> false
            end
            | _ -> false
        end -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.IDENT_EM s)

    | MEM_INIT, _ when begin
        match prev_rawtoken with
        | PLUS | MINUS | STAR | SLASH | PERC when env#templ_arg_flag -> true
        | COLON | COMMA when self#peek_rawtoken() == TY_LPAREN -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

    | MEM_INIT, INI when begin
        match prev_rawtoken with
        | COLON | COMMA | NEWLINE -> begin
            match self#peek_rawtoken() with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AM x,s,e | x -> x) 1;
                    true
                end
                | _ -> false
            end
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

    | MEM_INIT, _ when begin
        match prev_rawtoken with
        | RPAREN when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA -> true
                | _ -> false
            end
            | _ -> false
        end -> true
        | COMMA -> begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll =
                  self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                in
                match self#peek_nth_rawtoken (nth+1) with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | TY_LPAREN -> begin
                        match self#peek_nth_rawtoken (nth+3) with
                        | COMMA -> false
                        | _ -> true
                    end 
                    | _ -> true
                end
                | DECLTYPE -> true
                | _ -> false
            end
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.IDENT_EM s)

    | MEM_INIT, _ when prev_rawtoken == EOF && self#peek_rawtoken() == COMMA -> DEBUG_MSG "@"; mk (T.IDENT_V s)

    | MEM_INIT, INI when prev_rawtoken == RPAREN && self#peek_rawtoken() == TY_LPAREN && begin
        let nth, ll =
          self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
        in
        match self#peek_nth_rawtoken (nth+1) with
        | IDENT x when x = s -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.IDENT_EM s)

    | MEM_INIT, _ when not env#macro_arg_flag && not env#arg_paren_flag -> DEBUG_MSG "@"; token

    | ENUM, _ when begin
        match prev_rawtoken with
        | EQ | COLON_COLON -> true
        | _ -> false
    end && begin
        match self#peek_rawtoken() with
        | COMMA | RBRACE | PLUS | MINUS | PTR_STAR | SLASH | PERC -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

    | ENUM, _ when not env#macro_arg_flag -> DEBUG_MSG "@"; mk (T.IDENT_E s)

    | (TOP | MEM), END_OF_ID_EXPR when begin
        not env#objc_class_interface_flag &&
        match self#peek_rawtoken() with
        | SEMICOLON _ -> begin
            match prev_rawtoken with
            | RPAREN | CONST -> s = "Q_DECL_OVERRIDE"
            | _ -> false
        end
        | _ -> false
    end -> begin
      match context with
      | MEM -> mk (T.VIRT_SPEC_MACRO s)
      | _ -> mk (T.NOEXCEPT_MACRO s)
    end
    | TOP, INI when begin
        prev_rawtoken == EOF &&
        self#peek_rawtoken() == COMMA &&
        match self#peek_nth_rawtoken 2 with
        | IDENT _ -> self#peek_nth_rawtoken 3 == COMMA
        | _ -> false
    end -> begin
      DEBUG_MSG "@";
      self#ctx_expr();
      self#prepend_token (mk (T.IDENT_V s));
      mk T.MARKER
    end
    | CLASS, _ when begin
        match prev_rawtoken with
        | CLASS | STRUCT | UNION | ENUM -> begin
            match self#peek_rawtoken() with
            | IDENT "sealed" -> false
            | IDENT x when is_virt_spec_macro x -> false
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | LBRACE -> begin
                    match self#peek_nth_rawtoken 3 with
                    | PRIVATE | PROTECTED | PUBLIC | VIRTUAL -> true
                    | _ -> begin
                        let filt = function
                          | T.PRIVATE | PROTECTED | PUBLIC | VIRTUAL | SEMICOLON _ -> true
                          | _ -> false
                        in
                        try
                          let _ = self#peek_rawtoken_up_to_rbrace ~lv_ofs:0 ~filt () in
                          false
                        with
                          Found -> true
                    end
                end
                | FINAL -> true
                | COLON -> true
                | _ -> false
            end
            | TY_LPAREN when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | IDENT _ -> true
                | _ -> false
            end -> true
            | _ -> false
        end
        | _ -> false
    end -> begin
      DEBUG_MSG "@";
      match self#peek_rawtoken() with
      | TY_LPAREN -> mk (T.IDENT_AM s)
      | _ -> mk (T.ATTR_MACRO s)
    end
    | _, (END_OF_DTOR | END_OF_ID_EXPR) when prev_rawtoken == RPAREN && begin
        not env#objc_class_interface_flag &&
        match self#peek_rawtoken() with
        | SEMICOLON _ -> not env#typedef_flag
        | LBRACE when not env#alignas_flag && context != CLASS -> true
        | EQ -> begin
            match self#peek_nth_rawtoken 2 with
            | INT_LITERAL "0" -> true
            | _ -> false
        end
        | _ -> false
    end -> begin
      match context with
      | MEM when env#stack#in_template -> DEBUG_MSG "@"; mk (T.ATTR_MACRO s)
      | MEM -> DEBUG_MSG "@"; mk (T.VIRT_SPEC_MACRO s)
      | _ when env#end_of_attr_macro_call_flag -> DEBUG_MSG "@"; mk (T.ATTR_MACRO s)
      | _ -> DEBUG_MSG "@"; mk (T.NOEXCEPT_MACRO s)
    end
    | _, END_OF_DTOR when begin
        match prev_rawtoken with
        | CONST | RBRACKET -> true
        | _ -> false
    end && begin
      match self#peek_rawtoken() with
      | SEMICOLON _ | LBRACE -> true
      | _ -> false
    end -> DEBUG_MSG "@"; mk (T.ATTR_MACRO s)
    | _, END_OF_ID_EXPR when env#end_of_params_flag && prev_rawtoken == CONST && begin
        match self#peek_rawtoken() with
        | SEMICOLON _ | LBRACE -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.ATTR_MACRO s)
    | _, END_OF_ID_EXPR when prev_rawtoken == RBRACKET && begin
        match self#peek_rawtoken() with
        | SEMICOLON _ | LBRACE | EQ -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk (T.ATTR_MACRO s)

    | _, END_OF_ID_EXPR when begin
        env#templ_arg_flag &&
        match prev_rawtoken with
        | COMMA | TEMPL_LT -> true
        | _ -> false
    end && begin
      match self#peek_rawtoken() with
      | TYPENAME -> true
      | _ -> false
    end -> DEBUG_MSG "@"; token

    | _ when
        prev_rawtoken == TYPEDEF && self#peek_rawtoken() == TY_LPAREN &&
        not (is_type_name s || is_type s) && begin
          match self#peek_nth_rawtoken 2 with
          | PTR_STAR | PTR_AMP | PTR_AMP_AMP | HAT _ -> false
          | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ -> begin
              match self#peek_nth_rawtoken 3 with
              | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> false
              | _ -> true
          end
          | IDENT _ when begin
              match self#peek_nth_rawtoken 3 with
              | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
              | _ -> false
          end -> false
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 3 with
              | COLON_COLON -> begin
                  match self#peek_nth_rawtoken 4 with
                  | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> false
                  | _ -> true
              end
              | RPAREN -> begin
                  match self#peek_nth_rawtoken 4 with
                  | TY_LPAREN -> false
                  | _ -> true
              end
              | _ -> begin
                  let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                  match ll with
                  | [] -> false
                  | [_] -> begin
                      match self#peek_nth_rawtoken (nth+1) with
                      | TY_LPAREN -> false
                      | _ -> true
                  end
                  | _ when begin
                      match self#peek_nth_rawtoken (nth+1) with
                      | TY_LPAREN -> begin
                          let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                          match self#peek_nth_rawtoken (nth'+1) with
                          | SEMICOLON _ -> true
                          | _ -> false
                      end
                      | _ -> false
                  end -> false
                  | _ -> true
              end
          end
          | _ -> true
        end -> DEBUG_MSG "@"; mk (T.IDENT_TM s)

    | NEW, INI -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | LPAREN when is_templ s -> DEBUG_MSG "@"; token
        | LPAREN | AMP _ when begin
            match self#peek_rawtoken() with
            | TEMPL_LT | COLON_COLON -> false
            | _ -> true
        end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

        | COLON_COLON when begin
           match prev_rawtoken2 with
           | IDENT _ when prev_rawtoken3 == LPAREN -> true
           | _ -> false
        end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

        | MINUS_MINUS | PLUS_PLUS -> DEBUG_MSG "@"; mk (T.IDENT_V s)
        | x when is_op x -> DEBUG_MSG "@"; mk (T.IDENT_V s)

        | _ -> DEBUG_MSG "@"; token
    end
    | _ -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | STATIC | IDENT _ when begin
            ((match prev_rawtoken with IDENT _ -> false | _ -> true) || context == MEM) &&
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll =
                  self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                in
                match self#peek_nth_rawtoken (nth+1) with
                | SEMICOLON _ ->
                    List.exists
                      (function
                        | [x] -> DEBUG_MSG "%s" (Token.rawtoken_to_string x); is_literal x
                        | _ -> false
                      ) ll
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(STATIC|IDENT) @"; mk (T.IDENT_DM s)

        | NAMESPACE when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "NAMESPACE @"; mk (T.IDENT_IM s)

        | PP_DEFINE | OPERATOR | NEW(* | TYPEDEF*) | NAMESPACE | ELAB_ENUM -> token

        | CLASS | STRUCT | UNION | ENUM when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | COLON | LBRACE | SEMICOLON _ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(CLASS|STRUCT|UNION|ENUM) @"; mk (T.IDENT_IM s)

        | CLASS | STRUCT | UNION | ENUM when begin
            match self#peek_rawtoken() with
            | ALIGNAS  -> true
            | _ -> false
        end -> DEBUG_MSG "(CLASS|STRUCT|UNION|ENUM) @"; mk (T.ATTR_MACRO s)

        | CLASS | STRUCT | UNION | ENUM when begin
            match self#peek_rawtoken() with
            | IDENT _  -> false
            | _ -> true
        end -> DEBUG_MSG "(CLASS|STRUCT|UNION|ENUM) @"; token

        | CLASS | STRUCT | UNION | ENUM when begin
            match self#peek_rawtoken() with
            | IDENT x when is_virt_spec_macro x -> false
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | SEMICOLON _ -> false
                | COLON_COLON when begin
                    match prev_rawtoken2 with
                    | SEMICOLON _ | RBRACE -> true
                    | _ -> false
                end -> true
                | LBRACE | EQ | COMMA | PTR_STAR | PTR_AMP | LBRACKET -> false
                | COLON_COLON | TEMPL_LT when is_capital_ident s -> true
                | COLON_COLON | TEMPL_LT -> false
                | TY_LPAREN -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | SEMICOLON _ | LBRACE | EQ | CONST -> false
                    | _ -> true
                end
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "(CLASS|STRUCT|UNION|ENUM) @"; mk (T.ATTR_MACRO s)

        | IDENT _ when env#enum_head_flag && begin
            match self#peek_rawtoken() with
            | LBRACE -> true
            | _ -> false
        end -> DEBUG_MSG "IDENT @"; mk (T.BASE_MACRO s)

        | TY_LPAREN when env#dtor_if_section_flag -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_V s)

        | TY_LPAREN when self#peek_rawtoken() != TY_LPAREN -> DEBUG_MSG "TY_LPAREN @"; token

        | AMP _ | AMP_AMP _ when env#end_of_params_flag && begin
            match self#peek_rawtoken() with
            | LBRACE | IDENT _ -> true
            | _ -> false
        end -> DEBUG_MSG "(AMP|AMP_AMP) @"; mk (T.ATTR_MACRO s)

        | STAR | SLASH | PERC | AMP _ | BAR _ | HAT _ | PLUS | MINUS | EXCLAM _ | TILDE _
        | LT_LT | GT_GT | AMP_AMP _ | BAR_BAR _ | EQ_EQ | EXCLAM_EQ _ | LT | GT
        | LT_EQ | GT_EQ | QUEST | DOT | MINUS_GT when begin
            (match prev_rawtoken with
            | MINUS_GT -> not env#end_of_params_flag && not env#trailing_retty_flag
            | _ -> true) &&
            match self#peek_rawtoken() with
            | TEMPL_LT when is_bound s -> true
            | TEMPL_LT | COLON_COLON -> false
            | _ -> true
        end -> DEBUG_MSG "* @"; mk (T.IDENT_V s)

        | COMMA | TEMPL_LT when env#templ_arg_flag && not env#param_head_flag && begin
            match self#peek_rawtoken() with
            | PTR_STAR -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_TEMPL_GT | GT_GT | COMMA | TY_LPAREN
                | PTR_STAR | PTR_AMP | CONST | VOLATILE | LBRACKET | ELLIPSIS -> false
                | _ -> true
            end
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

        | COMMA | TEMPL_LT when env#templ_arg_flag && not env#param_head_flag && begin
            match self#peek_rawtoken() with
            | PTR_STAR -> begin
                match self#peek_nth_rawtoken 2 with
                | PTR_AMP | LBRACKET -> true
                | TY_LPAREN -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                    match ll with
                    | [_] -> false
                    | _ -> true
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(COMMA|TEMPL_LT) @"; token

        | COMMA when begin
            env#type_paren_flag && env#param_head_flag && self#peek_rawtoken() != TY_LPAREN
        end -> begin
          DEBUG_MSG "COMMA @";
          if self#peek_rawtoken() == DOT then begin
            parse_warning env stp edp "occurrence of odd '.' is replaced by '::'";
            conv_nth_token (function T.DOT,s,e -> T.COLON_COLON,s,e | x -> x) 1;
          end;
          token
        end

        | USING when self#peek_rawtoken() == EQ -> DEBUG_MSG "USING @"; token

        | LPAREN when env#macro_arg_flag && begin
            match self#peek_rawtoken() with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA | RPAREN -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; token

        | LPAREN when (env#macro_arg_flag || env#arg_paren_flag) && begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                let b = check_if_macro_args ll in
                if b then begin
                  match self#peek_nth_rawtoken (nth+1) with
                  | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                      match self#peek_nth_rawtoken (nth+2) with
                      | IDENT x when x = s ->
                          conv_nth_token
                            (function
                              | T.PTR_STAR,s,e -> T.STAR,s,e
                              | T.PTR_AMP,s,e -> T.AMP "&",s,e
                              | T.PTR_AMP_AMP,s,e -> T.AMP_AMP "&&",s,e
                              | x -> x
                            ) (nth+1);
                      | _ -> ()
                  end
                  | _ -> ()
                end;
                b
                (*List.exists
                  (function
                    | [T.RPAREN;TY_LPAREN]
                    | [T.IDENT _; IDENT _]
                    | (T.PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> true
                    | x::_ when is_basic_ty x -> true
                    | T.IDENT x::_ when is_type_name x || is_type x -> true
                    | _ -> false
                  ) ll*)
            end
            | _ -> false
        end -> DEBUG_MSG "LPAREN @"; mk (T.IDENT_EM s)

        | LPAREN when begin
           match self#peek_rawtoken() with
           | STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ | PP_STRINGIZED _ -> true
           | _ -> false
        end -> DEBUG_MSG "LPAREN @"; mk (T.STR_MACRO s)

        | LPAREN | COMMA | DECL_MACRO _ when begin
            DEBUG_MSG "(LPAREN|COMMA|DECL_MACRO) @";
            env#macro_arg_flag &&
            let rec chk n =
              match self#peek_nth_rawtoken n with
              | TY_LPAREN -> begin
                  let nth, _ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(n+1) () in
                  match self#peek_nth_rawtoken (nth+1) with
                  | IDENT _(*x when x = s*) -> begin
                      begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | TY_LPAREN ->
                            conv_nth_token (function T.IDENT x,s,e -> T.IDENT_DSM x,s,e | x -> x) (nth+1);
                        | _ ->
                            conv_nth_token (function T.IDENT x,s,e -> T.DECL_MACRO x,s,e | x -> x) (nth+1);
                      end;
                      let _ = chk (nth+2) in
                      true
                  end
                  | RPAREN when match prev_rawtoken with DECL_MACRO _ -> true | _ -> false -> true
                  | _ -> false
              end
              | _ -> false
            in
            chk 1
        end -> DEBUG_MSG "LPAREN @"; mk (T.IDENT_DSM s)

        | LPAREN when begin
           match self#peek_rawtoken() with
           | COLON_COLON -> false
           | TEMPL_LT when env#paren_level > 0 && begin
               let nth, _l = self#peek_rawtoken_up_to ~is_target:is_semicolon [T.LBRACE;RBRACE;NEWLINE] in
               let l = List.rev _l in
               DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
               let paren_level = env#paren_level in
               not (templ_param_arg_balanced ~stack:(env#get_templ_param_arg_stack()) ~paren_level l) &&
               not (templ_param_arg_balanced ~stack:(env#get_templ_param_arg_stack()) ~paren_level ~weak:true l)
           end -> true
           | TEMPL_LT -> false
           | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
           | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST | IDENT _ | TYPE_MACRO _ -> false
           | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
               match self#peek_nth_rawtoken 2 with
               | RPAREN -> true
               | IDENT _ when begin
                   match prev_rawtoken2 with
                   | IDENT_DSM _ -> begin
                       let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                       match List.rev ll with
                       | ((T.SEMICOLON _|RBRACE|EQ)::_)::_ -> true
                       | _ -> false
                   end
                   | _ -> false
               end -> true
               | _ -> false
           end -> false
           | _ when prev_rawtoken2 == ELLIPSIS && prev_rawtoken3 == SIZEOF -> false
           | _ -> true
        end -> DEBUG_MSG "LPAREN @"; mk (T.IDENT_V s)

        | LPAREN when prev_rawtoken2 == ELLIPSIS && prev_rawtoken3 == SIZEOF
          -> DEBUG_MSG "LPAREN @"; token

        | LBRACKET when self#peek_rawtoken() != COLON_COLON -> mk (T.IDENT_V s)

        | IDENT _ when prev_rawtoken2 == EOF && begin
            match self#peek_rawtoken() with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | LBRACE -> begin
                    let filt = function
                      | T.SEMICOLON _ -> true
                      | _ -> false
                    in
                    try
                      let _ = self#peek_rawtoken_up_to_rbrace ~filt ~from:3 () in
                      false
                    with
                      _ -> begin
                        conv_nth_token (function T.IDENT x,s,e -> T.PARAMS_MACRO x,s,e | x -> x) 1;
                        true
                      end
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "EOF IDENT @"; mk (T.IDENT_V s)

        | _ when env#macro_arg_flag && begin
            contains_comma s ||
            is_literal prev_rawtoken && is_literal (self#peek_rawtoken())
        end -> mk (T.DELIM_MACRO s)

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | TY_TEMPL_GT | STATIC
        | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO | IDENT _ | TYPE_MACRO _ when begin
            match self#peek_rawtoken() with
            | IDENT x when begin
                is_attr_macro x || is_attr_macro_ident x || is_suffix_macro_ident x ||
                Str.string_match google_attr_pat x 0 ||
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    self#is_lparen ~from:3 () &&
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | SEMICOLON _ -> true
                    | _ -> false
                end
                | _ -> false
            end -> false
            | IDENT x when is_params_macro x || is_params_macro_ident x -> false
            | IDENT x when is_capital_ident x && begin
                match self#peek_nth_rawtoken 2 with
                | LBRACE -> true
                | _ -> false
            end -> false
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | EQ -> true
                    | _ -> false
                end
                | _ -> false
            end -> false
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true (* e.g. far pointer *)
            | OPERATOR | EXPLICIT | VIRTUAL | TYPEDEF -> true
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
            | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | STATIC -> true
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> false
                    | PTR_AMP_AMP | PTR_AMP | PTR_STAR -> begin
                        match self#peek_nth_rawtoken 4 with
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken 5 with
                            | RPAREN -> begin
                                match self#peek_nth_rawtoken 6 with
                                | SEMICOLON _ -> false
                                | _ -> true
                            end
                            | _ -> true
                        end
                        | _ -> true
                    end
                    | COMMA -> false
                    | _ -> true
                end
                | _ -> true
            end
            | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
                let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
                match (List.rev l' : T.token list) with
                | IDENT _::TY_LPAREN::_ -> true
                | _ -> false
            end
            | PP_ELIF | PP_ELSE -> begin
                let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:3 () in
                match l with
                | [IDENT _] -> begin
                    DEBUG_MSG "nth=%d" nth;
                    conv_nth_token
                      (function T.IDENT x,s,e -> T.DECL_SPEC_MACRO x,s,e | x -> x) (nth-1);
                    true
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; token

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | TY_TEMPL_GT
        | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO(* | IDENT _*) | TYPE_MACRO _ when begin
            match self#peek_rawtoken() with
            | LBRACE when begin
                match self#peek_nth_rawtoken 2 with
                | x when is_stmt_head x -> true
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.DTOR_MACRO s)

        | IDENT _ when s = "sealed" && begin
            prev_rawtoken2 == CLASS &&(* prev_rawtoken3 == MS_REF &&*)
            match self#peek_rawtoken() with
            | LBRACE -> true
            | _ -> false
        end -> DEBUG_MSG "@"; mk T.MS_SEALED

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | TY_TEMPL_GT
        | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO | IDENT _ | TYPE_MACRO _ when begin
            match self#peek_rawtoken() with
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | PTR_AMP_AMP | PTR_AMP | PTR_STAR -> begin
                        match self#peek_nth_rawtoken 4 with
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken 5 with
                            | RPAREN -> begin
                                match self#peek_nth_rawtoken 6 with
                                | SEMICOLON _ -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> true
            | COLON_COLON | TEMPL_LT | COLON | TY_LPAREN | TYPENAME | CONSTEXPR | CONSTEVAL | CONSTINIT
            | TILDE _ -> false
            | GNU_ATTR _ when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    let _, nth, l = self#peek_rawtoken_up_to_rparen ~level:0 ~from:2 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    (*| EQ -> false*)
                    | IDENT _ -> false
                    | _ -> true
                end
                | _ -> false
            end -> true
            | GNU_ATTR _ -> false
            | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ -> false
            | PP_IF | PP_IFDEF | PP_IFNDEF when env#type_paren_flag && env#stack#in_params -> begin
                conv_nth_token
                  (function
                    | T.PP_IF,s,e -> T.PP_IF_E,s,e
                    | T.PP_IFDEF,s,e -> T.PP_IFDEF_E,s,e
                    | T.PP_IFNDEF,s,e -> T.PP_IFNDEF_E,s,e
                    | x -> x
                  ) 1;
                self#prepend_token (mk T.MARKER);
                true
            end
            | LBRACE when begin
                match self#peek_nth_rawtoken 2 with
                | x when is_stmt_head x -> true
                | _ -> false
            end -> false
            | SEMICOLON _ when env#alias_flag -> false
            | _ -> true
        end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO | IDENT _ | TYPE_MACRO _ when begin
            not env#objc_class_interface_flag &&
            match self#peek_rawtoken() with
            | COLON -> true
            | _ -> false
        end -> DEBUG_MSG "@"; mk (T.IDENT_B s)

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | AUTO | TYPEDEF
        | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | EXTERN | PTR_STAR
        | IDENT _ when self#peek_rawtoken() == TY_LPAREN && not env#templ_arg_flag && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | TY_LPAREN when begin
                match ll with
                | [l] when begin
                    match List.rev l with
                    | IDENT _::[]
                    | (PTR_STAR|PTR_AMP|PTR_AMP_AMP|HAT _)::_
                    | (IDENT _|MS_CDECL _|MS_STDCALL _|CC_MACRO _)::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[]
                    | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> true
                    | IDENT x::_ when is_ptr_macro x -> true
                    | _ -> false
                end -> false
                | _ -> true
            end -> true
            | _ -> false
        end -> DEBUG_MSG "(CHAR|...|TYPE_MACRO|CONST|EXTERN|PTR_STAR|IDENT) @"; mk (T.IDENT_IM s)

        | TYPEDEF -> DEBUG_MSG "TYPEDEF @"; token

        | COLON_COLON when env#templ_arg_flag && begin
            begin
              match self#peek_rawtoken() with
              | TY_TEMPL_GT -> true
              | GT_GT -> env#templ_param_arg_level > 1
              | _ -> false
            end && begin
              match prev_rawtoken2 with
              | IDENT _ -> begin
                  match prev_rawtoken3 with
                  | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ | LT | GT | COLON -> true
                  | _ -> false
              end
              | _ -> false
            end
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when env#ty_param_flag && begin
            match self#peek_rawtoken() with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when
            prev_rawtoken2 == TEMPL_GT &&
            (self#peek_rawtoken() == TY_TEMPL_GT ||
            env#templ_param_arg_level > 1 && self#peek_rawtoken() == GT_GT) &&
            env#templ_arg_flag && not env#last_ty_templ_id_flag
          -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when
            prev_rawtoken2 == TEMPL_GT &&
            (self#peek_rawtoken() == COMMA || self#peek_rawtoken() == RPAREN) &&
            env#templ_head_flag && env#macro_arg_flag && not env#last_ty_templ_id_flag
          -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when
            env#using_flag && not env#alias_flag && is_semicolon (self#peek_rawtoken())
          -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when begin
            match self#peek_rawtoken() with
            | LBRACKET -> begin
                match self#peek_nth_rawtoken 2 with
                | RBRACKET -> begin
                    match self#peek_nth_rawtoken 3 with
                    | EQ -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when begin
            env#macro_arg_flag && not env#type_paren_flag && not env#typename_flag &&
            match self#peek_rawtoken() with
            | PTR_STAR when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | CONST -> true
                    | IDENT x when is_type_name x || is_type x -> true
                    | x when is_basic_ty x -> true
                    | _ -> false
                end
                | _ -> false
            end -> false
            | PTR_STAR | PLUS | MINUS | SLASH | PERC when begin
                match self#peek_nth_rawtoken 2 with
                | COMMA | RPAREN -> false
                | _ -> true
            end -> true
            | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when begin
            match self#peek_rawtoken() with
            | COLON when context != CLASS && not env#enum_head_flag -> true
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

        | COLON_COLON when env#typename_flag && begin
            match self#peek_rawtoken() with
            | EQ | TY_TEMPL_GT -> true
            | _ -> false
        end -> DEBUG_MSG "COLON_COLON @"; token

        | COLON_COLON when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
                match self#peek_nth_rawtoken (nth+1) with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | SEMICOLON _ -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> begin
          DEBUG_MSG "COLON_COLON @";
          let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
          conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth+1);
          mk (T.IDENT_TM s)
        end

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when self#peek_rawtoken() != COLON_COLON && (begin
            match self#peek_rawtoken() with
            | SEMICOLON _ | COMMA -> true
            | _ -> false
        end || begin
          match prev_rawtoken2 with
          | TY_LPAREN | COMMA -> true
          | _ -> false
        end) -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP) @"; mk (T.IDENT_V s)

        | COMMA when begin
            match self#peek_rawtoken() with
            | TY_LPAREN when env#templ_arg_flag && is_type_name s -> true
            | EQ -> true
            | _ -> false
        end -> DEBUG_MSG "COMMA @"; token

        | COMMA when begin
            match prev_rawtoken2 with
            | IDENT_V _ | RBRACKET -> begin
                match self#peek_rawtoken() with
                | COMMA | LBRACKET | SEMICOLON _ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(IDENT|RBRACKET) COMMA @ (COMMA|LBRACKET|SEMICOLON)"; mk (T.IDENT_V s)

        | COMMA when begin
            match self#peek_rawtoken() with
            | STAR | SLASH | PERC | AMP _ | BAR _ | PLUS | MINUS | EXCLAM _ | TILDE _
            | LT_LT | GT_GT | AMP_AMP _ | BAR_BAR _ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ ->
                true
            | TEMPL_LT when begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> true
                    | _ -> false
                end
                | INT_LITERAL _ | FLOAT_LITERAL _ | CHAR_LITERAL _ | USER_INT_LITERAL _
                | USER_FLOAT_LITERAL _ | USER_CHAR_LITERAL _ when env#paren_level > 0 -> begin
                    let found, nth, l =
                      self#peek_rawtoken_up_to_rparen ~from:3 (Some T.TY_TEMPL_GT)
                    in
                    found &&
                    match self#peek_nth_rawtoken (nth+1) with
                    | INT_LITERAL _ | FLOAT_LITERAL _ | CHAR_LITERAL _ | USER_INT_LITERAL _
                    | USER_FLOAT_LITERAL _ | USER_CHAR_LITERAL _ -> true
                    | _ -> false
                end
                | _ -> false
            end -> true
            | TY_TEMPL_GT when env#templ_param_arg_level = 0 -> true
            | _ -> false
        end -> DEBUG_MSG "COMMA @"; mk (T.IDENT_V s)

        | COMMA | CONST when env#type_paren_flag && self#peek_rawtoken() == TY_LPAREN && begin
            let _, nth, l = self#peek_rawtoken_up_to_rparen ~level:env#paren_level None in
            match self#peek_nth_rawtoken (nth+1) with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
            | _ -> false
        end -> DEBUG_MSG "(COMMA|CONST) @"; mk (T.IDENT_TM s)

        | TY_TEMPL_GT when begin
            match self#peek_rawtoken() with
            | COMMA | RPAREN | SEMICOLON _ -> true
            | LBRACE when context != CLASS -> true
            | _ -> false
        end -> DEBUG_MSG "TY_TEMPL_GT @"; mk (T.IDENT_V s)

        | _ when env#conv_func_id_flag && begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "* @"; token

        | CLASS_LBRACE | SEMICOLON _ | COLON when begin
            (context == MEM || env#stack#in_class) &&
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
                match self#peek_nth_rawtoken (nth+1) with
                | PRIVATE | PUBLIC | PROTECTED | INLINE -> true
                | _ -> false
            end
            | IDENT _ -> begin
                not (is_qt_decl_macro s) &&
                match self#peek_nth_rawtoken 2 with
                | PRIVATE | PUBLIC | PROTECTED | INLINE -> true
                | _ -> false
            end
            | _ -> false
        end -> begin
          DEBUG_MSG "(CLASS_LBRACE|SEMICOLON) @";
          match self#peek_rawtoken() with
          | TY_LPAREN -> begin
              conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 1;
              mk (T.IDENT_V s)
          end
          | _ -> mk (T.DECL_MACRO s)
        end

        | CLASS_LBRACE | SEMICOLON _ when begin
            (context == MEM || env#stack#in_class) &&
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
                self#peek_nth_rawtoken (nth+1) == TY_LPAREN
            end
            | IDENT _ -> not (is_qt_decl_macro s)
            | _ -> false
        end -> DEBUG_MSG "(CLASS_LBRACE|SEMICOLON) @"; token

        | CLASS_LBRACE | SEMICOLON _ | DECL_MACRO _ | RBRACE | COLON when begin
            (context == MEM || env#stack#in_class) &&
            (prev_rawtoken != COLON ||
            match prev_rawtoken2 with
            | PRIVATE | PROTECTED | PUBLIC -> true
            | _ -> false) &&
            match self#peek_rawtoken() with
            | PRIVATE | PROTECTED | PUBLIC | RBRACE -> true
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:3 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | VIRTUAL | TYPEDEF | EXPLICIT | STATIC -> true
                    | _ -> false
                end
                | RBRACE -> true
                | _ -> false
            end -> true
            | TY_LPAREN -> false
            | _ when is_qt_decl_macro s -> true
            | _ when is_ns_decl_macro_func s && self#peek_rawtoken() != TY_LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "(CLASS_LBRACE|SEMICOLON|DECL_MACRO|RBRACE|COLON) @"; mk (T.DECL_MACRO s)

        | CLASS_LBRACE | SEMICOLON _ | RBRACE | NEWLINE when begin
            context == MEM && sub_context == INI &&
            match self#peek_rawtoken() with
            | COLON -> true
            | _ -> false
        end -> DEBUG_MSG "(CLASS_LBRACE|SEMICOLON|RBRACE|NEWLINE) @"; mk (T.ACC_SPEC_MACRO s)

        | CLASS_LBRACE | SEMICOLON _ | RBRACE | NEWLINE | COLON when s = "property" && begin
            context == MEM && sub_context == INI &&
            match self#peek_rawtoken() with
            | x when is_basic_ty x -> true
            | IDENT x when is_type_name x || is_type x -> true
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | COLON_COLON | HAT _ -> true
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "(CLASS_LBRACE|SEMICOLON|RBRACE|NEWLINE|COLON) @"; mk T.MS_PROPERTY

        | CLASS_LBRACE | SEMICOLON _ | RBRACE | NEWLINE when
            (context == TOP || context == MEM) && sub_context == INI && begin
              match self#peek_rawtoken() with
              | FRIEND | TYPEDEF | CONSTEXPR | CONSTEVAL | CONSTINIT | INLINE
              | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
              | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ -> true
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken 2 with
                  | FRIEND | TYPEDEF | CONSTEXPR | CONSTEVAL | CONSTINIT | INLINE
                  | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
                  | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ -> true
                  | _ -> false
              end
              | _ -> false
            end -> DEBUG_MSG "(CLASS_LBRACE|SEMICOLON|RBRACE|NEWLINE) @"; mk (T.DECL_SPEC_MACRO s)

        | CLASS_LBRACE | SEMICOLON _ | RBRACE | NEWLINE when begin
            (context == TOP || context == MEM) && sub_context == INI &&
            match self#peek_rawtoken() with
            | TY_LPAREN -> begin
                match self#peek_nth_rawtoken 2 with
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
                | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | IDENT "boolean" | TYPE_MACRO _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | IDENT x -> x <> s
                        | _ -> false
                    end
                    | _ -> false
                end
                | STRUCT | UNION | CLASS | ENUM -> begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | RPAREN -> begin
                            match self#peek_nth_rawtoken 5 with
                            | IDENT _ -> true
                            | _ -> false
                        end
                        | PTR_STAR -> begin
                            match self#peek_nth_rawtoken 5 with
                            | RPAREN -> begin
                                match self#peek_nth_rawtoken 6 with
                                | IDENT _ -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> begin
          DEBUG_MSG "(CLASS_LBRACE|SEMICOLON|RBRACE|NEWLINE) @";
          let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
          begin
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | TY_LPAREN -> begin
                    let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+3) () in
                    match self#peek_nth_rawtoken (nth'+1) with
                    | TY_LPAREN ->
                        conv_nth_token (function T.IDENT x,s,e -> T.IDENT_IM x,s,e | x -> x) (nth+1);
                    | _ ->
                        conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth+1);
                end
                | _ -> ()
            end
            | _ -> ()
          end;
          mk (T.IDENT_TM s)
        end

        | RBRACE | SEMICOLON _ when begin
            context == TOP && sub_context == INI && s = "interface" &&
            match self#peek_rawtoken() with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | SEMICOLON _ -> false
                | _ -> true
            end
            | _ -> true
        end -> DEBUG_MSG "(RBRACE|SEMICOLON) @"; mk (T.CLASS_HEAD_MACRO s)

        | IDENT_V _ | RPAREN when
            (prev_rawtoken != RPAREN || env#end_of_id_macro_call_flag || env#end_of_noptr_dtor_paren_flag) &&
            not env#end_of_params_flag &&
            self#peek_rawtoken() == TY_LPAREN && self#peek_nth_rawtoken 2 == TY_LPAREN
          -> DEBUG_MSG "(IDENT_V|RPAREN) @"; mk (T.IDENT_PM s)

        | IDENT_V _ | RPAREN when env#type_paren_flag && self#peek_rawtoken() == TY_LPAREN && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | COMMA | RPAREN -> check_if_macro_args ll
            | _ -> false
        end -> DEBUG_MSG "(IDENT_V|RPAREN) @"; mk (T.IDENT_PDM s)

        | TY_LPAREN | COMMA when begin
            self#peek_rawtoken() == TY_LPAREN && (prev_rawtoken != COMMA || env#type_paren_flag)
        end -> begin
            let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
            match self#peek_nth_rawtoken (nth+1) with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP | RPAREN -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_TM s)

            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
            | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST | TYPE_MACRO _
              -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_AM s)

            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | COMMA | RPAREN when env#type_paren_flag -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_TM s)
                | COMMA | RPAREN | COLON_COLON
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP | STAR | CONST -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_AM s)
                | IDENT _ when begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | COMMA | RPAREN -> true
                    | _ -> false
                end -> DEBUG_MSG "TY_LPAREN @"; mk (T.IDENT_AM s)
                | _ -> DEBUG_MSG "TY_LPAREN @"; token
            end
            | _ -> DEBUG_MSG "TY_LPAREN @"; token
        end

        (*| LPAREN when self#peek_rawtoken() == COMMA -> mk (T.IDENT_V s)*)

        | STR_LITERAL _ | USER_STR_LITERAL _
        | STR_MACRO _ when prev_rawtoken2 != EXTERN -> begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> mk (T.IDENT_LM s)
            | _ -> mk (T.STR_MACRO s)
        end

        | COLON when env#macro_arg_flag && self#peek_rawtoken() != COLON_COLON
          -> DEBUG_MSG "COLON @"; mk (T.IDENT_V s)

        | COLON when env#templ_arg_flag -> DEBUG_MSG "COLON @"; mk (T.IDENT_V s)

        | COLON when begin
            match prev_rawtoken2 with
            | PUBLIC | PRIVATE | PROTECTED -> begin
                match self#peek_rawtoken() with
                | IDENT x when is_decl_macro x -> true
                | IDENT x when is_qt_decl_macro x -> true
                | IDENT x when is_ns_decl_macro_func x -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON @"; mk (T.DECL_MACRO s)

        | EXPLICIT when begin
            match self#peek_rawtoken() with
            | TEMPL_LT -> true
            | _ -> false
        end -> DEBUG_MSG "EXPLICIT @ TEMPL_LT"; token

        | EXPLICIT when begin
            match self#peek_rawtoken() with
            | IDENT _ when self#peek_nth_rawtoken 2 == LBRACE -> true
            | _ -> true
        end -> begin
          DEBUG_MSG "EXPLICIT @";
          conv_nth_token (function T.IDENT x,s,e -> T.PARAMS_MACRO x,s,e | x -> x) 1;
          mk (T.IDENT_V s)
        end

        | EXPLICIT when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> false
            | _ -> true
        end -> DEBUG_MSG "EXPLICIT @"; mk (T.DECL_SPEC_MACRO s)

        | LBRACE when begin
            match self#peek_rawtoken() with
            | IDENT _ -> contains_ns s
            | _ -> false
        end -> mk (T.NS_MACRO s)

        | LBRACE when begin
            match self#peek_rawtoken() with
            | TY_LPAREN -> contains_ns s
            | _ -> false
        end -> mk (T.IDENT_NSM s)

        | RPAREN | CONST when(* env#stack#in_class &&*) env#end_of_params_flag && begin
            not env#objc_class_interface_flag &&
            match self#peek_rawtoken() with
            | LBRACE | COLON | EQ | OVERRIDE -> true
            | TY_TEMPL_GT when env#templ_arg_flag -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; mk (T.NOEXCEPT_MACRO s)

        | RPAREN when env#end_of_params_flag && begin
            not env#objc_class_interface_flag &&
            match self#peek_rawtoken() with
            | SEMICOLON _ -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @ SEMICOLON "; mk (T.ATTR_MACRO s)

        | RPAREN when env#end_of_noptr_dtor_paren_flag && not env#in_body_brace_flag && begin
            match self#peek_rawtoken() with
            | SEMICOLON _ -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @ SEMICOLON"; mk (T.PARAMS_MACRO s)

        | EQ when env#templ_head_flag && env#ty_param_flag -> DEBUG_MSG "EQ @"; token

        | ATTR_MACRO _ when env#end_of_params_flag && is_semicolon (self#peek_rawtoken())
          -> DEBUG_MSG "ATTR_MACRO @ SEMICOLON "; mk (T.ATTR_MACRO s)

        | TEMPL_LT | COMMA when env#templ_arg_flag && begin
            match self#peek_rawtoken() with
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN | COLON_COLON | TEMPL_LT | IDENT _ -> false
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COMMA | TY_TEMPL_GT -> false
                    | _ -> true
                end
                | _ -> true
            end -> begin
              conv_nth_token (function T.IDENT x,s,e -> T.ARG_MACRO x,s,e | x -> x) 1;
              true
            end
            | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "(TEMPL_LT|COMMA) @"; mk (T.IDENT_V s)

        | _ -> begin
            DEBUG_MSG "* @";
            let templ_unbalanced_flag = ref false in
            match self#peek_rawtoken() with
            | COLON_COLON -> DEBUG_MSG "* @ COLON_COLON"; token

            | IDENT x -> begin
                DEBUG_MSG "* @ IDENT";
                match self#peek_nth_rawtoken 2 with
                | GT_GT -> DEBUG_MSG "* @ IDENT GT_GT"; mk (T.IDENT_V s)
                | TY_LPAREN -> begin
                    DEBUG_MSG "* @ IDENT TY_LPAREN";
                    if
                      not env#end_of_params_flag &&
                      (match prev_rawtoken with
                      | COLON_COLON | CONST | CONSTEXPR | CONSTEVAL | CONSTINIT | VOLATILE
                      | STATIC | INLINE | EOF | SEMICOLON _ | RBRACE -> false
                      | NEWLINE when begin
                          match prev_rawtoken2 with
                          | STR_LITERAL _ -> begin
                              match prev_rawtoken3 with
                              | PP_INCLUDE -> true
                              | _ -> false
                          end
                          | _ -> false
                      end -> false
                      | _ -> true) &&
                      self#is_lparen ~from:3 ()
                    then begin
                      DEBUG_MSG "* @ IDENT TY_LPAREN";
                      conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 1;
                      conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 2;
                      mk (T.DECL_MACRO s)
                    end
                    else
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> begin
                        DEBUG_MSG "* @ IDENT TY_LPAREN TY_LPAREN";
                        try
                          let _, _ =
                            self#peek_rawtoken_up_to_rparen_split_at_comma
                              ~from:4 ~filt:ty_pat2 ()
                          in
                          token
                        with
                          Found -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end
                    | _ when begin
                        not env#typename_flag && env#end_of_params_flag &&
                        let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:3 None in
                        match self#peek_nth_rawtoken (nth+1) with
                        | SEMICOLON _ | LBRACE -> true
                        | _ -> false
                    end -> DEBUG_MSG "* @ IDENT TY_LPAREN *"; mk (T.ATTR_MACRO s)

                    | _ when begin
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                        match self#peek_nth_rawtoken (nth+1) with
                        | IDENT y when y = s && begin
                            match self#peek_nth_rawtoken (nth+2) with
                            | IDENT z -> z = x && self#peek_nth_rawtoken (nth+3) == TY_LPAREN
                            | _ -> false
                        end -> begin
                          conv_nth_token (function T.IDENT x,s,e -> T.IDENT_DSM x,s,e | x -> x) 1;
                          let rec conv from =
                            match self#peek_nth_rawtoken from with
                            | IDENT y when y = s -> begin
                                conv_nth_token (function T.IDENT x,s,e -> T.DECL_SPEC_MACRO x,s,e | x -> x) from;
                                match self#peek_nth_rawtoken (from+1) with
                                | IDENT z when z = x && self#peek_nth_rawtoken (from+2) == TY_LPAREN -> begin
                                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_DSM x,s,e | x -> x) (from+1);
                                    let nth', _ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(from+3) () in
                                    conv (nth'+1)
                                end
                                | _ -> ()
                            end
                            | _ -> ()
                          in
                          conv (nth+1);
                          true
                        end
                        | _ -> false
                    end -> DEBUG_MSG "* @ IDENT TY_LPAREN *"; mk (T.DECL_SPEC_MACRO s)

                    | _ -> DEBUG_MSG "* @ IDENT TY_LPAREN *"; token
                end

                | TEMPLATE | CLASS | STRUCT | UNION | ENUM when begin
                    env#stack#at_top
                end -> DEBUG_MSG "* @ IDENT (TEMPLATE|CLASS|STRUCT|UNION|ENUM)"; mk (T.DECL_MACRO s)

                | COLON | LBRACE when x <> "sealed" && not (is_virt_spec_macro x) && begin
                    prev_rawtoken <> COLON_COLON &&
                    not env#in_body_brace_flag &&
                    context == CLASS && sub_context == INI
                end -> DEBUG_MSG "* @ IDENT (COLON|LBRACE)"; mk (T.ATTR_MACRO s)

                | SEMICOLON _ when env#end_of_params_flag -> DEBUG_MSG "* @ IDENT SEMICOLON"; mk (T.ATTR_MACRO s)

                | _ when begin
                    context == TOP &&
                    try
                      let _, macro_kind, tok_list_obj = env#find_pending_macro s in
                      match macro_kind with
                      | ObjectLike -> begin
                          let tok_list = (Obj.obj tok_list_obj : token list) in
                          match tok_list with
                          | _ when begin
                              match Xlist.last tok_list with
                              | T.SEMICOLON _,_,_ -> true
                              | _ -> false
                          end -> true
                          | _ -> false
                      end
                      | _ -> false
                    with
                      _ -> false
                end -> DEBUG_MSG "* @ IDENT *"; mk (T.DECL_MACRO s)

                | _ -> DEBUG_MSG "* @ IDENT *"; token (*???*)
            end
            | LBRACKET when
                self#peek_nth_rawtoken 2 == RBRACKET && self#peek_nth_rawtoken 3 != EQ
              -> DEBUG_MSG "* @ LBRACKET"; token

            | LBRACKET when env#templ_arg_flag && begin
                match prev_rawtoken with
                | CONST -> true
                | _ -> false
            end -> DEBUG_MSG "* @ LBRACKET"; token

            | LBRACKET when env#alias_flag && begin
                let nth, l = self#peek_rawtoken_up_to_rbracket ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | SEMICOLON _ -> true
                | _ -> false
            end -> DEBUG_MSG "@ LBRACKET"; token

            | EQ when begin
                not env#type_paren_flag && not env#templ_head_flag &&
                match self#peek_nth_rawtoken 2 with
                | INT_LITERAL "0" when env#end_of_params_flag -> false
                | _ -> true
            end -> DEBUG_MSG "@ EQ"; mk (T.IDENT_V s)

            | HAT _ when (context == TOP || context == MEM || context == NEW) && begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COMMA | RPAREN when env#type_paren_flag -> true
                    | LBRACE | SEMICOLON _ | EQ -> true
                    | TY_LPAREN when begin
                        let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:4 None in
                        match self#peek_nth_rawtoken (nth+1) with
                        | LBRACE | SEMICOLON _ -> true
                        | _ -> false
                    end -> true
                    | COLON_COLON when begin
                        try
                          let nth = self#peek_rawtoken_up_to_end_of_qualified_id ~from:3 () in
                          match self#peek_nth_rawtoken (nth+1) with
                          | COMMA | RPAREN when env#type_paren_flag -> true
                          | LBRACE | SEMICOLON _ -> true
                          | TY_LPAREN when begin
                              match self#peek_nth_rawtoken (nth+2) with
                              | RPAREN -> begin
                                  match self#peek_nth_rawtoken (nth+3) with
                                  | LBRACE | SEMICOLON _ -> true
                                  | _ -> false
                              end
                              | _ -> false
                          end -> true
                          | TY_LPAREN when begin
                              let _, nth', l = self#peek_rawtoken_up_to_rparen ~from:(nth+2) None in
                              match self#peek_nth_rawtoken (nth'+1) with
                              | LBRACE | SEMICOLON _ -> true
                              | _ -> false
                          end -> true
                          | _ -> false
                        with _ -> false
                    end -> true
                    | _ -> false
                end
                | COMMA | RPAREN when env#type_paren_flag || env#templ_arg_flag -> true
                | TY_TEMPL_GT when env#templ_arg_flag -> true
                | _ -> false
            end -> DEBUG_MSG "@ HAT"; token

            | DOT | MINUS_GT | LBRACKET | RBRACKET | LT_LT | GT_EQ | LT_EQ | GT_GT_EQ
            | LT_LT_EQ | EQ_EQ | EXCLAM_EQ _ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ
            | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | PLUS | MINUS | SLASH | PERC
            | BAR _ | BAR_BAR _ | HAT _ | QUEST when begin
                (self#peek_rawtoken() != LBRACKET || begin
                  let nth, _ = self#peek_rawtoken_up_to_rbracket ~from:2 () in
                  match self#peek_nth_rawtoken (nth+1) with
                  | EQ | SEMICOLON _ | DOT -> true
                  | _ -> false
                end) &&
                not env#objc_class_interface_flag &&
                match context, sub_context with
                | CLASS, INI when begin
                    match prev_rawtoken with
                    | TEMPLATE | CLASS | STRUCT | UNION | ENUM -> true
                    | _ -> false
                end -> false
                | _ -> true
            end -> begin
                DEBUG_MSG "* @ *";
                let wrong_context =
                  not env#macro_arg_flag &&
                  match context, sub_context with
                  | (TOP | MEM), _ when begin
                      match prev_rawtoken with
                      | SEMICOLON _ | NEWLINE | EOF -> true
                      | _ -> false
                  end -> true
                  | _ when env#templ_arg_flag -> begin
                      match self#peek_nth_rawtoken 3 with
                      | COMMA -> true
                      | _ -> false
                  end
                  | _ -> false
                in
                DEBUG_MSG "wrong_context=%B" wrong_context;
                if not wrong_context then begin
                  self#ctx_expr();
                  self#ctx_ini();
                end;
                mk (T.IDENT_V s)
            end

            | GT_GT when prev_rawtoken == LPAREN -> DEBUG_MSG "* @ GT_GT"; mk (T.IDENT_V s)

            | GT_GT when begin
                match self#peek_nth_rawtoken 2 with
                | INT_LITERAL _ | USER_INT_LITERAL _ -> true
                | _ -> false
            end -> DEBUG_MSG "* @ GT_GT"; mk (T.IDENT_V s)

            | PUBLIC | PRIVATE | PROTECTED | VIRTUAL when env#base_clause_flag && begin
                match prev_rawtoken with
                | NEWLINE -> true
                | _ -> false
            end -> DEBUG_MSG "NEWLINE @ (PUBLIC|PRIVATE|PROTECTED|VIRTUAL)"; mk (T.BASE_SPEC_MACRO s)

            | TY_LPAREN -> begin
                DEBUG_MSG "* @ TY_LPAREN";
                let fptr_flag = ref false in
                let ty_flag = ref false in
                match prev_rawtoken with
                | TY_TILDE | NEW | ELAB_ENUM -> DEBUG_MSG "(TY_TILDE|NEW|ELAB_ENUM) @ TY_LPAREN"; token

                | PUBLIC | PRIVATE | PROTECTED | VIRTUAL when env#base_clause_flag
                  -> DEBUG_MSG "(PUBLIC|PRIVATE|PROTECTED|VIRTUAL) @ TY_LPAREN"; mk (T.IDENT_IM s)

                | COLON_COLON when context == NEW -> DEBUG_MSG "COLON_COLON @ TY_LPAREN"; token

                | COLON_COLON when env#alias_flag && begin
                    match self#peek_nth_rawtoken 2 with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "COLON_COLON @ TY_LPAREN"; token

                | COLON_COLON when begin
                    prev_rawtoken2 == TEMPL_GT && env#last_ty_templ_id_flag
                end -> DEBUG_MSG "COLON_COLON @ TY_LPAREN"; token

                | COLON_COLON when begin
                    match prev_rawtoken2 with
                    | IDENT _ -> begin
                        match prev_rawtoken3 with
                        | HEAD_COLON_COLON -> begin
                            match prev_rawtoken4 with
                            | OPERATOR -> true
                            | _ -> false
                        end
                        | OPERATOR | TYPEDEF | TYPENAME -> true
                        | _ -> false
                    end
                    | OPERATOR -> true
                    | _ -> false
                end -> DEBUG_MSG "COLON_COLON @ TY_LPAREN"; token

                | HEAD_COLON_COLON when begin
                    match prev_rawtoken2 with
                    | OPERATOR -> true
                    | _ -> false
                end -> DEBUG_MSG "HEAD_COLON_COLON @ TY_LPAREN"; token

                | COLON_COLON when (env#type_paren_flag || env#alias_flag || env#templ_head_flag) && begin
                    DEBUG_MSG "@";
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match ll with
                    | [l] -> begin
                        match self#peek_nth_rawtoken (nth+1) with
                        | TY_LPAREN | TY_TEMPL_GT -> begin
                            match (l : T.token list) with
                            | (PTR_STAR | PTR_AMP | PTR_AMP_AMP)::_ -> true
                            | IDENT _::(PTR_STAR | PTR_AMP | PTR_AMP_AMP)::_ -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> begin
                        match self#peek_nth_rawtoken (nth+1) with
                        | TY_LPAREN | TY_TEMPL_GT -> begin
                            List.exists
                              (fun (l : T.token list) ->
                                match l with
                                | (PTR_STAR | PTR_AMP | PTR_AMP_AMP)::_ -> true
                                | IDENT _::(PTR_STAR | PTR_AMP | PTR_AMP_AMP)::_ -> true
                                | _ -> false
                              ) ll
                        end
                        | _ -> false
                    end
                end -> DEBUG_MSG "COLON_COLON @ TY_LPAREN"; token

                | COMMA when env#param_head_flag && begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    DEBUG_MSG "\n%s"
                      (String.concat "\n"
                         (List.map
                            (fun x -> String.concat ";"
                                (List.map Token.rawtoken_to_string x)) ll));
                    match self#peek_nth_rawtoken (nth+1) with
                    | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
                    | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST -> true
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | COMMA | RPAREN -> true
                        | COLON_COLON -> true
                        | PTR_STAR | PTR_AMP | PTR_AMP_AMP | STAR -> true
                        | IDENT _ when begin
                            match self#peek_nth_rawtoken (nth+3) with
                            | COMMA | RPAREN -> true
                            | _ -> false
                        end -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> begin
                  DEBUG_MSG "COMMA @ TY_LPAREN";
                  mk (T.IDENT_AM s)
                end

                | COMMA when env#param_head_flag -> begin
                  DEBUG_MSG "COMMA @ TY_LPAREN";
                  self#ctx_end_of_ty_spec();
                  token
                end

                | COMMA when env#templ_arg_flag && begin
                    match self#peek_nth_rawtoken 2 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 3 with
                        | COLON_COLON -> begin
                            match self#peek_nth_rawtoken 4 with
                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                match self#peek_nth_rawtoken 5 with
                                | RPAREN -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "COMMA @ TY_LPAREN"; token

                | COMMA when env#templ_head_flag && begin
                    match self#peek_nth_rawtoken 2 with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP
                    | MS_CDECL _ | MS_STDCALL _ -> true
                    | IDENT x when is_cc_macro x -> true
                    | IDENT _ when begin
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> begin
                            match self#peek_nth_rawtoken 4 with
                            | TY_LPAREN -> true
                            | _ -> false
                        end
                        | _ -> false
                    end -> true
                    | _ -> false
                end -> DEBUG_MSG "COMMA @ TY_LPAREN"; token

                | TY_TEMPL_GT -> begin
                    DEBUG_MSG "TY_TEMPL_GT @ TY_LPAREN";
                    match self#peek_nth_rawtoken 2 with
                    | IDENT _ -> begin
                        DEBUG_MSG "TY_TEMPL_GT @ TY_LPAREN IDENT";
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> begin
                            DEBUG_MSG "TY_TEMPL_GT @ TY_LPAREN IDENT RPAREN";
                            match self#peek_nth_rawtoken 4 with
                            | TY_LPAREN -> DEBUG_MSG "TY_TEMPL_GT @ TY_LPAREN IDENT RPAREN TY_LPAREN"; mk (T.IDENT_IM s)
                            | _ -> mk (T.IDENT_V s)
                        end
                        | _ -> mk (T.IDENT_V s)
                    end
                    | _ when begin
                        let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
                        match self#peek_nth_rawtoken (nth+1) with
                        | IDENT x when is_virt_spec_macro_ident x || is_virt_spec_macro x -> false
                        | IDENT x when is_cv_spec_macro_ident x || is_cv_spec_macro x -> false
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth+2) with
                            | TY_LPAREN -> begin
                                let _, nth', _ =
                                  self#peek_rawtoken_up_to_rparen ~from:(nth+3) None
                                in
                                match self#peek_nth_rawtoken (nth'+1) with
                                | LBRACE | SEMICOLON _ -> true
                                | CONST when begin
                                    match self#peek_nth_rawtoken (nth'+2) with
                                    | LBRACE | SEMICOLON _ -> true
                                    | _ -> false
                                end -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | TY_TEMPL_GT when env#templ_arg_flag -> true
                        | _ -> false
                    end -> DEBUG_MSG "TY_TEMPL_GT @ TY_LPAREN *"; mk (T.IDENT_AM s)
                    | _ -> DEBUG_MSG "TY_TEMPL_GT @ TY_LPAREN *"; mk (T.IDENT_V s)
                end

                | EOF | NEWLINE | SEMICOLON _ | RBRACE
                | EXTERN | STR_LITERAL _ | STATIC | DECL_SPEC_MACRO _ when begin
                    (match prev_rawtoken with STR_LITERAL _ -> false | _ -> true) ||
                    (match prev_rawtoken2 with EXTERN -> true | _ -> false)
                end -> begin
                    DEBUG_MSG "(EOF|NEWLINE|SEMICOLON|EXTERN|STATIC|STR_LITERAL|DECL_SPEC_MACRO) @ TY_LPAREN";
                    match self#peek_nth_rawtoken 2 with
                    | PP_ELIF | PP_ELSE | PP_ENDIF when env#pp_ifx_d_flag
                      -> DEBUG_MSG "... @ TY_LPAREN (PP_ELIF|PP_ELSE|PP_ENDIF)"; mk (T.IDENT_V s)
                    | _ ->
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    DEBUG_MSG "\n%s"
                      (String.concat "\n"
                         (List.map
                            (fun x -> String.concat ";"
                                (List.map Token.rawtoken_to_string x)) ll));
                    let nth1 = nth + 1 in
                    let nth2 = nth + 2 in
                    match self#peek_nth_rawtoken nth1 with

                    (*| COMMA when prev_rawtoken == EOF -> begin
                        DEBUG_MSG "EOF @ TY_LPAREN...RPAREN ";
                        self#prepend_token (mk (T.IDENT_EM s));
                        mk T.BEGIN_STMTS
                    end*)

                    | IDENT x when begin
                        match prev_rawtoken with
                        | DECL_SPEC_MACRO y -> y = x
                        | _ -> false
                    end -> begin
                      DEBUG_MSG "... @ TY_LPAREN...RPAREN IDENT";
                      mk (T.IDENT_DSM s)
                    end

                    | PP_IF | PP_IFDEF | PP_IFNDEF when begin
                        (*env#pp_ifx_d_flag &&*)
                        let nth', l' = self#peek_rawtoken_up_to ~from:nth2 [T.NEWLINE] in
                        match self#peek_nth_rawtoken (nth'+1) with
                        | IDENT x -> begin
                            DEBUG_MSG "x=%s" x;
                            match prev_rawtoken with
                            | DECL_SPEC_MACRO y -> y = x
                            | _ -> false
                        end
                        | _ -> false
                    end -> begin
                      DEBUG_MSG "... @ TY_LPAREN...RPAREN (PP_IF|PP_IFDEF|PP_IFNDEF)";
                      mk (T.IDENT_DSM s)
                    end

                    | PP_ENDIF | PP_ELSE | PP_ELIF as rt when begin
                        DEBUG_MSG "... @ TY_LPAREN...RPAREN (PP_ENDIF|PP_ELIF|PP_ELSE)";
                        let nth' =
                          match rt with
                          | PP_ENDIF -> let n, _ = self#peek_rawtoken_up_to ~from:nth2 [T.NEWLINE] in n
                          | PP_ELSE | PP_ELIF -> self#peek_rawtoken_up_to_section_end ~from:nth2 ()
                          | _ -> assert false
                        in
                        match self#peek_nth_rawtoken (nth'+1) with
                        | IDENT x when is_macro_fun x -> false
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth'+2) with
                            | TY_LPAREN -> begin
                                let nth'', _ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth'+3) () in
                                match self#peek_nth_rawtoken (nth''+1) with
                                | LBRACE | EQ -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end -> DEBUG_MSG "... @ TY_LPAREN...RPAREN (PP_ENDIF|PP_ELIF|PP_ELSE)"; mk (T.IDENT_DM s)

                    | PP_ENDIF | PP_ELSE | PP_ELIF as rt when begin
                        DEBUG_MSG "... @ TY_LPAREN...RPAREN (PP_ENDIF|PP_ELIF|PP_ELSE)";
                        let nth' =
                          match rt with
                          | PP_ENDIF -> let n, _ = self#peek_rawtoken_up_to ~from:nth2 [T.NEWLINE] in n
                          | PP_ELSE | PP_ELIF -> self#peek_rawtoken_up_to_section_end ~from:nth2 ()
                          | _ -> assert false
                        in
                        match self#peek_nth_rawtoken (nth'+1) with
                        | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                            let nth'', l'' =
                              self#peek_rawtoken_up_to ~from:(nth'+2) ~skip_pp_control_line:true [T.NEWLINE]
                            in
                            match self#peek_nth_rawtoken (nth''+1) with
                            | IDENT x -> begin
                                match prev_rawtoken with
                                | DECL_SPEC_MACRO y -> y = x
                                | _ -> false
                            end
                            | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                                let nth''', l''' = self#peek_rawtoken_up_to ~from:(nth''+2) [T.NEWLINE] in
                                match self#peek_nth_rawtoken (nth'''+1) with
                                | IDENT x -> begin
                                    match prev_rawtoken with
                                    | DECL_SPEC_MACRO y -> y = x
                                    | _ -> false
                                end
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | PP_ENDIF | PP_ELSE | PP_ELIF as rt -> begin
                            let nth'' =
                              match rt with
                              | PP_ENDIF -> let n, _ = self#peek_rawtoken_up_to ~from:(nth'+2) [T.NEWLINE] in n
                              | PP_ELSE | PP_ELIF -> self#peek_rawtoken_up_to_section_end ~from:(nth'+2) ()
                              | _ -> assert false
                            in
                            match self#peek_nth_rawtoken (nth''+1) with
                            | IDENT x -> begin
                                match prev_rawtoken with
                                | DECL_SPEC_MACRO y -> y = x
                                | _ -> false
                            end
                            | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                                let nth''', l''' = self#peek_rawtoken_up_to ~from:(nth''+2) [T.NEWLINE] in
                                match self#peek_nth_rawtoken (nth'''+1) with
                                | IDENT x -> begin
                                    match prev_rawtoken with
                                    | DECL_SPEC_MACRO y -> y = x
                                    | _ -> false
                                end
                                | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                                    let nth'''', l'''' = self#peek_rawtoken_up_to ~from:(nth'''+2) [T.NEWLINE] in
                                    match self#peek_nth_rawtoken (nth''''+1) with
                                    | IDENT x -> begin
                                        match prev_rawtoken with
                                        | DECL_SPEC_MACRO y -> y = x
                                        | _ -> false
                                    end
                                    | _ -> false
                                end
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | IDENT x -> begin
                            match prev_rawtoken with
                            | DECL_SPEC_MACRO y -> y = x
                            | _ -> false
                        end
                        | _ -> false
                    end -> begin
                      DEBUG_MSG "@";
                      mk (T.IDENT_DSM s)
                    end

                    | _ when begin
                        List.for_all
                          (fun l ->
                            List.for_all (fun x -> x != T.NEWLINE) l
                          ) ll &&
                        List.exists
                          (fun (l : T.token list) ->
                            match l with
                            | (SEMICOLON _|RBRACE)::_ -> true
                            | _ -> false
                          ) ll
                    end -> begin
                      DEBUG_MSG "@";
                      mk (T.IDENT_DSM s)
                    end

                    | PARAMS_MACRO _ -> DEBUG_MSG "@"; token

                    | IDENT _ when begin
                        match self#peek_nth_rawtoken nth2 with
                        | EQ -> true
                        | _ -> false
                    end -> DEBUG_MSG "@"; mk (T.IDENT_TM s)

                    | IDENT _ when begin
                        match ll with
                        | [[MUTABLE]] -> true
                        | _ -> false
                    end -> DEBUG_MSG "@"; mk (T.IDENT_DM s)

                    | IDENT x -> begin
                        let rec chk from =
                          let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from () in
                          match self#peek_nth_rawtoken (nth'+1) with
                          | LBRACE | SEMICOLON _ | EQ -> check_if_macro_args ll'
                          | IDENT _ -> chk_sub (nth'+2)
                          | _ -> false
                        and chk_sub from =
                          match self#peek_nth_rawtoken from with
                          | TY_LPAREN -> chk (from+1)
                          | IDENT _ -> chk_sub (from+1)
                          | _ -> false
                        in
                        DEBUG_MSG "x=%s" x;
                        match self#peek_nth_rawtoken nth2 with
                        | TY_LPAREN when is_ident_macro_ident x -> DEBUG_MSG "@"; mk (T.IDENT_TM s)
                        | TY_LPAREN -> begin
                            match self#peek_nth_rawtoken (nth+3) with
                            | TY_LPAREN when check_double_paren nth2 -> DEBUG_MSG "@"; mk (T.IDENT_IM s)
                            | _ when x <> s && chk (nth+3) -> DEBUG_MSG "@"; mk (T.IDENT_DSM s)
                            | _ -> begin
                                let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth2+1) () in
                                match self#peek_nth_rawtoken (nth'+1) with
                                | LBRACE | SEMICOLON _ -> DEBUG_MSG "@"; mk (T.IDENT_DSM s)

                                | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                            end
                        end
                        | LBRACKET -> DEBUG_MSG "@"; mk (T.IDENT_TM s)

                        | IDENT y when begin
                            DEBUG_MSG "y=%s" y;
                            match self#peek_nth_rawtoken (nth2+1) with
                            | TY_LPAREN -> chk (nth2+2)
                            | IDENT _ -> chk_sub (nth2+2)
                            | _ -> false
                        end -> DEBUG_MSG "@"; mk (T.IDENT_TM s)

                        | SEMICOLON _ when not (is_capital_ident x) -> DEBUG_MSG "@"; mk (T.IDENT_TM s)

                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end

                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                        match self#peek_nth_rawtoken nth2 with
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth+3) with
                            | SEMICOLON _ | EOF -> true
                            | TY_LPAREN -> begin
                                match self#peek_nth_rawtoken (nth+4) with
                                | x when is_ty x -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end -> DEBUG_MSG "@"; mk (T.IDENT_TM s)

                    | EOF when begin
                        match ll with
                        | [l] -> begin
                            match List.rev l with
                            | (MS_STDCALL _|MS_CDECL _|CC_MACRO _)::_
                            | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_
                            | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[]
                            | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> true
                            | _ -> false
                        end
                        | _ -> false
                    end -> DEBUG_MSG "@"; token

                    | TY_LPAREN | LBRACKET -> begin
                        match ll with
                        | [l] when begin
                            (self#peek_nth_rawtoken nth1 != LBRACKET ||
                            self#peek_nth_rawtoken nth2 != LBRACKET) &&
                            match List.rev l with
                            | IDENT _::[]
                            | (MS_STDCALL _|MS_CDECL _|CC_MACRO _)::_
                            | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_
                            | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[]
                            | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> true
                            | _ -> false
                        end -> DEBUG_MSG "@"; token
                        | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    end

                    | PP_ELIF | PP_ELSE | PP_ENDIF when
                        env#pp_if_section_flag && env#end_of_params_flag
                      -> DEBUG_MSG "@"; mk (T.IDENT_AM s)

                    | EXPLICIT | VIRTUAL(* | TYPEDEF*) -> DEBUG_MSG "@"; mk (T.IDENT_TM s)

                    | _ -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                end

                | MINUS_GT when begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                    in
                    match self#peek_nth_rawtoken (nth+1) with
                    | LBRACE -> true
                    | TY_LPAREN -> true
                    | _ -> false
                end -> DEBUG_MSG "MINUS_GT @ TY_LPAREN"; token

                | CONST when prev_rawtoken2 == OPERATOR
                  -> DEBUG_MSG "OPERATOR CONST @ TY_LPAREN"; token

                | CONST when begin
                    env#type_paren_flag &&
                    match prev_rawtoken2 with
                    | TY_LPAREN | COMMA -> true
                    | _ -> false
                end -> DEBUG_MSG "CONST @ TY_LPAREN"; token

                | CONST when env#templ_arg_flag && begin
                    match prev_rawtoken2 with
                    | TEMPL_LT | COMMA -> true
                    | _ -> false
                end -> DEBUG_MSG "CONST @ TY_LPAREN"; mk (T.IDENT_TM s)

                | CONST | RPAREN when env#end_of_params_flag && begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                    in
                    match self#peek_nth_rawtoken (nth+1) with
                    | SEMICOLON _ | LBRACE | VOLATILE | RESTRICT _ | MS_STDCALL _ | MS_CDECL _
                    | CC_MACRO _ | CV_MACRO _ | EQ | COLON -> false
                    | TEMPLATE -> true
                    | IDENT _ when begin
                        try
                          let _, macro_kind, tok_list_obj = env#find_pending_macro s in
                          match macro_kind with
                          | FunctionLike _ -> begin
                              let tok_list = (Obj.obj tok_list_obj : token list) in
                              match tok_list with
                              | (T.LBRACE,_,_)::rest when begin
                                  match Xlist.last rest with
                                  | RBRACE,_,_ -> true
                                  | _ -> false
                              end -> true
                              | _ -> false
                          end
                          | _ -> false
                        with
                          _ -> false
                    end -> true
                    | IDENT _ -> false
                    | _ -> true
                end -> DEBUG_MSG "(CONST|RPAREN) @ TY_LPAREN"; mk (T.IDENT_BM s)

                | CONST when env#end_of_params_flag -> DEBUG_MSG "CONST @"; mk (T.IDENT_CM s)

                | IDENT _ when env#ty_param_flag && env#ty_param_key_flag
                  -> DEBUG_MSG "IDENT @ TY_LPAREN"; mk (T.IDENT_TPM s)

                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO | TYPE_MACRO _
                | IDENT _ when begin
                    let nth, ll =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 ()
                    in
                    List.exists
                      (fun l ->
                        match l with
                        | _::T.EQ::_ -> true
                        | _ -> begin
                            match List.rev l with
                            | x::_ when is_basic_ty x -> true
                            | T.IDENT x::_ when is_type_name x || is_type x -> true
                            | [T.IDENT _;(PTR_STAR|PTR_AMP|PTR_AMP_AMP)] -> true
                            | [T.IDENT _;(PTR_STAR|PTR_AMP|PTR_AMP_AMP);IDENT _] -> true
                            | [T.IDENT _;IDENT _] -> true
                            | _ -> false
                        end
                      ) ll &&
                    match self#peek_nth_rawtoken (nth+1) with
                    | IDENT x when is_capital_ident x -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | TY_LPAREN -> begin
                            let nth', ll' =
                              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+3) ()
                            in
                            match self#peek_nth_rawtoken (nth'+1) with
                            | LBRACE | SEMICOLON _ -> true
                            | CONST when begin
                                match self#peek_nth_rawtoken (nth'+2) with
                                | LBRACE | SEMICOLON _ -> true
                                | _ -> false
                            end -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "... @ TY_LPAREN"; mk (T.IDENT_V s)

                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO | TYPE_MACRO _ when begin
                    let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | IDENT x when not (is_capital_ident x) -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | TY_LPAREN -> begin
                            let _, nth', _ =
                              self#peek_rawtoken_up_to_rparen ~from:(nth+3) None
                            in
                            match self#peek_nth_rawtoken (nth'+1) with
                            | LBRACE | SEMICOLON _ -> true
                            | CONST when begin
                                match self#peek_nth_rawtoken (nth'+2) with
                                | LBRACE | SEMICOLON _ -> true
                                | _ -> false
                            end -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | TY_TEMPL_GT when env#templ_arg_flag -> true
                    | _ -> false
                end -> DEBUG_MSG "... @ TY_LPAREN"; mk (T.IDENT_AM s)

                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
                | AUTO | INLINE when begin
                    let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | TEMPL_LT | COLON_COLON -> true
                    | HEAD_COLON_COLON -> begin
                        conv_nth_token (function T.HEAD_COLON_COLON,s,e -> T.COLON_COLON,s,e | x -> x) (nth+1);
                        true
                    end
                    | TY_LPAREN -> begin
                        let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                        match self#peek_nth_rawtoken (nth'+1) with
                        | SEMICOLON _ | LBRACE -> true
                        | IDENT x -> is_attr_macro x || is_attr_macro_ident x || is_capital_ident x
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "... @ TY_LPAREN"; mk (T.IDENT_IM s)

                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
                | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | AUTO
                | CONST | CONSTEXPR | CONSTEVAL | CONSTINIT | INLINE
                | TYPE_MACRO _ when begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | TY_LPAREN | LBRACKET -> begin
                        let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                        match self#peek_nth_rawtoken (nth'+1) with
                        (*| SEMICOLON _ | LBRACE -> false*)
                        | _ -> begin
                            match ll with
                            | [l] when begin
                                match List.rev l with
                                | IDENT _::[]
                                | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_
                                | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[]
                                | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> true
                                | _ -> false
                            end -> true
                            | _ -> false
                        end
                    end
                    | _ -> false
                end -> DEBUG_MSG "... @ TY_LPAREN"; token

                | CONST | INLINE | DECL_SPEC_MACRO _ | IDENT _
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID when begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with

                    | IDENT x when is_virt_spec_macro_ident x || is_virt_spec_macro x -> false
                    | IDENT x when is_cv_spec_macro_ident x || is_cv_spec_macro x -> false
                    | IDENT x when is_attr_macro_ident x || is_attr_macro x -> false

                    | IDENT x -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | TY_LPAREN when not (is_basic_ty prev_rawtoken) || prev_rawtoken == CONST -> begin
                            let _, nth', _ =
                              self#peek_rawtoken_up_to_rparen ~from:(nth+3) None
                            in
                            match self#peek_nth_rawtoken (nth'+1) with
                            | LBRACE | SEMICOLON _ -> true
                            | CONST when begin
                                match self#peek_nth_rawtoken (nth'+2) with
                                | LBRACE | SEMICOLON _ -> true
                                | _ -> false
                            end -> true
                            | _ -> false
                        end
                        | LBRACKET -> true
                        | SEMICOLON _ when not (check_if_params ~weak:true ll) -> true
                        | _ -> false
                    end
                    | TY_TEMPL_GT when env#templ_arg_flag -> true
                    | TY_LPAREN -> begin
                        match ll with
                        | [l] when begin
                            match List.rev l with
                            | IDENT _::[]
                            | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_
                            | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::[]
                            | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> fptr_flag := true; true
                            | _ -> false
                        end -> true
                        | _ -> false
                    end
                    | PTR_STAR(* | PTR_AMP | PTR_AMP_AMP*) -> ty_flag := true; true
                    | _ -> false
                end -> begin
                  match prev_rawtoken with
                  | _ when !fptr_flag -> DEBUG_MSG "@"; mk (T.TYPE_MACRO s)
                  | IDENT _ when !ty_flag -> DEBUG_MSG "@"; mk (T.IDENT_TM s)
                  | IDENT _ -> DEBUG_MSG "IDENT @ TY_LPAREN"; mk (T.IDENT_AM s)
                  | _ -> DEBUG_MSG "* @ TY_LPAREN"; mk (T.IDENT_TM s)
                end

                | TEMPL_GT when begin
                    let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:2 None in
                    match self#peek_nth_rawtoken (nth+1) with
                    | OPERATOR -> true
                    | _ -> false
                end -> DEBUG_MSG "TEMP_GT @ TY_LPAREN"; mk (T.IDENT_TM s)

                | TEMPL_GT when begin
                    match self#peek_nth_rawtoken 2 with
                    | EXCLAM _ | PLUS | MINUS | TILDE _ -> env#end_of_templ_head_flag
                    | _ -> false
                end -> DEBUG_MSG "TEMPL_GT @ TY_LPAREN"; mk (T.IDENT_DM s)

                | TEMPL_LT when env#templ_arg_flag && begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | BAR_BAR _ | PLUS | MINUS | SLASH | PERC | BAR _ -> begin
                        check_if_macro_args ll
                        (*List.exists
                          (fun l ->
                            match (List.rev l : T.token list) with
                            | TYPENAME::_ -> true
                            | [IDENT x] when is_type_name x || is_type x -> true
                            | x::_ when is_basic_ty x -> true
                            | _ -> false
                          ) ll*)
                    end
                    | _ -> false
                end -> DEBUG_MSG "TEMP_LT @ TY_LPAREN"; mk (T.IDENT_EM s)

                | TEMPL_LT when env#templ_head_flag && begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    List.exists
                      (function
                        | [T.TYPENAME] -> true
                        | [T.IDENT _] -> true
                        | _ -> false
                      ) ll
                end -> DEBUG_MSG "TEMP_LT @ TY_LPAREN"; mk (T.IDENT_TM s)

                | TEMPL_LT when env#templ_head_flag && begin
                    match self#peek_nth_rawtoken 2 with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 3 with
                        | COLON_COLON -> begin
                            match self#peek_nth_rawtoken 4 with
                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "TEMPL_LT @ TY_LPAREN"; token

                | TEMPL_LT when begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    let filt = function
                      | T.CONST | TYPENAME -> true
                      | _ -> false
                    in
                    match ll with
                    | [] -> true
                    | [l] -> begin
                        match l with
                        | [PTR_STAR|PTR_AMP|HAT _] -> true
                        | [x] -> is_basic_ty x
                        | ELLIPSIS::IDENT _::[] -> true
                        | IDENT _::PTR_STAR::IDENT _::[] -> begin
                            self#peek_nth_rawtoken (nth+1) == TY_TEMPL_GT
                        end
                        | PTR_STAR::COLON_COLON::_ -> true
                        | _ -> filt_at_level0 l filt
                    end
                    | [ELLIPSIS]::_ -> true
                    | _ -> List.exists (fun l -> filt_at_level0 l filt) ll
                end -> DEBUG_MSG "TEMPL_LT @ TY_LPAREN"; token

                | TEMPL_LT when env#templ_arg_flag && begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | TEMPL_LT -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | TYPENAME | CONST -> true
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth+3) with
                            | COMMA -> begin
                                match self#peek_nth_rawtoken (nth+4) with
                                | TYPENAME | CONST -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | TY_TEMPL_GT -> true
                    | _ -> false
                end -> DEBUG_MSG "TEMPL_LT @ TY_LPAREN"; mk (T.IDENT_IM s)

                | RPAREN when begin
                    (context == TOP || context == MEM) && env#end_of_params_flag &&
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    (match ll with
                    | [l] -> List.exists is_stmt_head l
                    | _ -> false) &&
                    match self#peek_nth_rawtoken (nth+1) with
                    | x when is_ty x -> true
                    | IDENT _ when is_ty (self#peek_nth_rawtoken (nth+2)) -> true
                    | _ -> false
                end -> DEBUG_MSG "RPAREN @ TY_LPAREN"; mk (T.IDENT_BM s)

                | RPAREN when begin
                    sub_context == END_OF_DTOR || env#end_of_params_flag ||
                    match prev_rawtoken2 with
                    | TY_LPAREN -> true
                    | _ -> false
                end -> DEBUG_MSG "RPAREN @ TY_LPAREN"; mk (T.IDENT_AM s)

                | RPAREN when begin
                    match context, sub_context with
                    | MEM, INI ->
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                        DEBUG_MSG "\n%s"
                          (String.concat "\n"
                             (List.map
                                (fun x -> String.concat ";"
                                    (List.map Token.rawtoken_to_string x)) ll));
                        List.exists
                          (fun (l : T.token list) ->
                            match l with
                            | (SEMICOLON _|RBRACE)::_ -> true
                            | _ -> false
                          ) ll
                    | _ -> false
                end -> begin
                  DEBUG_MSG "RPAREN @ TY_LPAREN";
                  mk (T.IDENT_DSM s)
                end

                | RPAREN when begin
                    match context, sub_context with
                    | (MEM|TOP), INI -> begin
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                        List.length ll = 1 &&
                        match self#peek_nth_rawtoken (nth+1) with
                        | TY_LPAREN -> begin
                            let nth', ll' =  self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                            match self#peek_nth_rawtoken (nth'+1) with
                            | SEMICOLON _ | EQ | LBRACE -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "RPAREN @ TY_LPAREN"; token

                | EQ when env#using_flag && env#alias_flag -> begin
                    DEBUG_MSG "@";
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match ll with
                    | (PTR_STAR::_)::_ -> DEBUG_MSG "EQ @ TY_LPAREN"; token
                    | (ELLIPSIS::_)::_ -> DEBUG_MSG "EQ @ TY_LPAREN"; token
                    | _ -> DEBUG_MSG "EQ @ TY_LPAREN"; mk (T.IDENT_TM s)
                end

                | EQ when env#templ_head_flag && env#ty_param_flag
                  -> DEBUG_MSG "EQ @ TY_LPAREN"; mk (T.IDENT_TM s)

                | COMMA when begin
                    env#templ_head_flag &&
                    match self#peek_nth_rawtoken 2 with
                    | EXCLAM _ -> true
                    | IDENT _ -> true
                    | COLON_COLON -> begin
                        match self#peek_nth_rawtoken 3 with
                        | IDENT _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "COMMA @ TY_LPAREN"; mk (T.IDENT_TPM s)

                | COMMA | COLON_COLON when (env#templ_arg_flag || env#type_paren_flag) && begin
                    match self#peek_nth_rawtoken 2 with
                    | CONST -> true
                    | PTR_STAR | PTR_AMP -> begin
                        match self#peek_nth_rawtoken 3 with
                        | IDENT _ -> self#peek_nth_rawtoken 4 == RPAREN
                        | RPAREN -> true
                        | _ -> false
                    end
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 3 with
                        | ELLIPSIS -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "(COMMA|COLON_COLON) @ TY_LPAREN"; token

                | COMMA | LPAREN | TEMPL_LT when (env#macro_arg_flag || env#templ_arg_flag) && begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    check_if_macro_args ll
                    (*List.exists
                      (fun l ->
                        DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                        match List.rev l with
                        | TYPENAME::_ -> true
                        | T.TY_LPAREN::_ -> true
                        | [IDENT x] when is_type_name x || is_type x -> true
                        | [IDENT _; IDENT _] -> true
                        | _ -> false
                      ) ll*)
                end -> DEBUG_MSG "(COMMA|LPAREN|TEMPL_LT) @ TY_LPAREN"; mk (T.IDENT_EM s)

                | ATTR_MACRO _ when begin
                    not (context == MEM && sub_context == INI) &&
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | SEMICOLON _ | LBRACE -> true
                    | _ -> false
                end -> DEBUG_MSG "ATTR_MACRO @ TY_LPAREN"; mk (T.IDENT_AM s)

                | CONST | OVERRIDE | FINAL | VIRT_SPEC_MACRO _ when begin
                    env#end_of_params_flag &&
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | SEMICOLON _ | LBRACE -> true
                    | IDENT x when x = s -> begin
                        conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AM x,s,e | x -> x) (nth+1);
                        true
                    end
                    | _ -> false
                end -> DEBUG_MSG "(CONST|OVERRIDE|FINAL|VIRT_SPEC_MACRO) @ TY_LPAREN"; mk (T.IDENT_AM s)

                (*| _ when env#typename_flag -> DEBUG_MSG "* @ TY_LPAREN"; token*)

                | _ -> DEBUG_MSG "* @ TY_LPAREN"; mk (T.IDENT_V s)
            end

            | COMMA | TY_TEMPL_GT when
                env#templ_head_flag && not env#ty_param_key_flag && env#init_flag
              -> DEBUG_MSG "* @ (COMMA|TY_TEMPL_GT)"; mk (T.IDENT_V s)

            | COMMA | TY_TEMPL_GT when begin
                env#templ_head_flag && not env#ty_param_key_flag && not env#templ_arg_flag &&
                not env#ty_param_rhs_flag && not env#ty_param_flag && not env#type_paren_flag &&

                (prev_rawtoken != COLON_COLON || prev_rawtoken2 != TEMPL_GT ||
                not env#last_ty_templ_id_flag) &&
                
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> false
                | _ when prev_rawtoken == ELLIPSIS -> false
                | _ -> true
            end -> DEBUG_MSG "* @ (COMMA|TY_TEMPL_GT)"; mk (T.IDENT_C s)

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                env#templ_arg_flag &&
                match self#peek_nth_rawtoken 2 with
                | TY_TEMPL_GT | GT_GT | COMMA -> true
                | _ -> false
            end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; token

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                env#templ_arg_flag && begin
                  match self#peek_nth_rawtoken 2 with
                  | TY_TEMPL_GT | GT_GT | COMMA -> false
                  | IDENT _ -> begin
                      match self#peek_nth_rawtoken 3 with
                      | TY_TEMPL_GT | GT_GT -> true
                      | PLUS | MINUS | SLASH | PTR_STAR | PTR_AMP | HAT _ | BAR _
                      | QUEST -> true
                      | _ -> env#paren_level > 0 && not env#type_paren_flag
                  end
                  | _ -> env#paren_level > 0 && not env#type_paren_flag
                end
            end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; mk (T.IDENT_V s)

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TILDE _ -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; mk (T.IDENT_V s)

            | PTR_AMP when begin
                match self#peek_nth_rawtoken 2 with
                | TILDE _ -> true
                | _ -> false
            end -> begin
              self#ctx_expr();
              self#ctx_ini();
              DEBUG_MSG "* @ PTR_AMP";
              mk (T.IDENT_V s)
            end

            | PTR_AMP_AMP when begin
                match self#peek_nth_rawtoken 2 with
                | EXCLAM _ -> true
                | _ -> false
            end -> begin
              self#ctx_expr();
              self#ctx_ini();
              DEBUG_MSG "* @ PTR_AMP_AMP";
              mk (T.IDENT_V s)
            end

            | RPAREN | PTR_AMP_AMP when env#noexcept_flag
              -> DEBUG_MSG "* @ (RPAREN|PTR_AMP_AMP)"; mk (T.IDENT_V s)

            | RPAREN | COMMA when begin
                match prev_rawtoken with
                | TEMPL_GT -> begin
                    match prev_rawtoken2 with
                    | TEMPL_LT -> begin
                        match prev_rawtoken3 with
                        | TEMPLATE -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ (RPAREN|COMMA)"; mk (T.DECL_MACRO s)

            | RPAREN | COMMA when begin
                match prev_rawtoken with
                | MINUS_GT | DOT -> true
                | _ when begin
                    env#paren_level > 0 &&
                    not env#type_paren_flag &&
                    not env#typename_flag
                end -> true
                | _ -> false
            end -> DEBUG_MSG "* @ (RPAREN|COMMA)"; mk (T.IDENT_V s)

            | TEMPL_LT when begin
                match self#peek_nth_rawtoken 2 with
                | TYPENAME -> true
                | IDENT _ when begin
                    match self#peek_nth_rawtoken 3 with
                    | ELLIPSIS | TY_TEMPL_GT -> true
                    | _ -> false
                end -> true
                | _ when begin
                    match context with
                    | TOP | CLASS | MEM -> true
                    | _ -> false
                end && env#templ_param_arg_level > 0 && begin
                  let nth, _l =
                    self#peek_rawtoken_up_to ~is_target:is_semicolon [T.LBRACE;RBRACE(*;NEWLINE*)]
                  in
                  let l = List.rev _l in
                  DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                  let paren_level = env#paren_level in
                  let stack =
                    if env#pp_if_section_flag then
                      try
                        let info = env#pp_if_section_top_info in
                        DEBUG_MSG "info=%s" (Pinfo.pp_if_section_info_to_string info);
                        let lv = info.Pinfo.i_templ_param_arg_level in
                        if lv = env#templ_param_arg_level then
                          Stack.create()
                        else
                          env#get_templ_param_arg_stack()
                      with
                        _ -> env#get_templ_param_arg_stack()
                    else
                      env#get_templ_param_arg_stack()
                  in
                  let b = templ_param_arg_balanced ~stack ~paren_level ~exact:true l in
                  if not b then
                    templ_unbalanced_flag := true;
                  b
                end -> true
                | _ -> false
            end -> DEBUG_MSG "* @ TEMPL_LT"; token

            | TEMPL_LT when !templ_unbalanced_flag -> DEBUG_MSG "* @ TEMPL_LT"; mk (T.IDENT_V s)

            | _ -> begin
                DEBUG_MSG "* @ *";
                match context, sub_context with
                | (TOP|MEM), INI -> begin
                    DEBUG_MSG "@";
                    match prev_rawtoken with
                    | HEAD_COLON_COLON -> begin
                        match self#peek_rawtoken() with
                        | EOF -> DEBUG_MSG "HEAD_COLON_COLON @ EOF"; mk (T.IDENT_V s)

                        | SEMICOLON _ when prev_rawtoken2 == USING
                          -> DEBUG_MSG "USING HEAD_COLON_COLON @ SEMICOLON"; mk (T.IDENT_V s)

                        | _ -> DEBUG_MSG "HEAD_COLON_COLON @ *"; token
                    end

                    | TY_TEMPL_GT when begin
                        match self#peek_rawtoken() with
                        | TEMPL_LT | COLON_COLON -> false
                        | _ -> true
                    end -> DEBUG_MSG "TY_TEMPL_GT @ (TEMPL_LT|COLON_COLON)"; mk (T.IDENT_V s)

                    | RPAREN when begin
                        match self#peek_rawtoken() with
                        | SEMICOLON _ -> true
                        | _ -> false
                    end -> DEBUG_MSG "RPAREN @ SEMICOLON"; mk (T.IDENT_V s)

                    | RPAREN when env#end_of_params_flag && begin
                        match self#peek_rawtoken() with
                        | TY_LPAREN -> false
                        | _ -> true
                    end -> DEBUG_MSG "RPAREN @ *"; mk (T.ATTR_MACRO s)

                    | RPAREN when context == MEM && sub_context == INI && begin
                        env#stack#at_class &&
                        match self#peek_rawtoken() with
                        | RBRACE -> true
                        | _ -> false
                    end -> DEBUG_MSG "RPAREN @ RBRACE"; mk (T.DECL_MACRO s)

                    | CONST when prev_rawtoken2 == RBRACE && begin
                        match self#peek_rawtoken() with
                        | SEMICOLON _ | LBRACE -> true
                        | _ -> false
                    end -> DEBUG_MSG "RBRACE CONST @ (SEMICOLON|LBRACE)"; mk (T.IDENT_V s)

                    | COLON_COLON when env#using_flag && begin
                        match self#peek_rawtoken() with
                        | EOF -> true
                        | _ -> false
                    end -> DEBUG_MSG "COLON_COLON @ EOF"; mk (T.IDENT_V s)

                    | COLON_COLON when env#macro_arg_flag && env#paren_level > 1 && begin
                        match self#peek_rawtoken() with
                        | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> begin
                            match self#peek_nth_rawtoken 2 with
                            | IDENT _ -> begin
                                match self#peek_nth_rawtoken 3 with
                                | RPAREN -> begin
                                    match self#peek_nth_rawtoken 4 with
                                    | AMP_AMP _ | BAR_BAR _
                                    | PLUS | MINUS | STAR | PTR_STAR | SLASH | PERC
                                    | HAT _ | AMP _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
                                    | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
                                    | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
                                    | LT | GT
                                    | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> true
                                    | _ -> false
                                end
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end -> DEBUG_MSG "COLON_COLON @ IDENT"; mk (T.IDENT_V s)

                    | COMMA when begin
                        not env#ty_param_flag &&
                        not env#type_paren_flag && self#peek_rawtoken() == COMMA &&
                        not env#objc_class_interface_flag
                    end -> DEBUG_MSG "COMMA @ COMMA"; mk (T.IDENT_V s)

                    | COMMA | LPAREN when env#arg_paren_flag && begin
                        match self#peek_rawtoken() with
                        | PTR_AMP_AMP | PTR_AMP | PTR_STAR -> begin
                            match self#peek_nth_rawtoken 2 with
                            | COMMA | RPAREN -> false
                            | _ -> true
                        end
                        | AMP_AMP _ | BAR_BAR _
                        | PLUS | MINUS | STAR | SLASH | PERC
                        | HAT _ | AMP _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
                        | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
                        | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
                        | LT | GT
                        | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> true
                        | _ -> false
                    end -> DEBUG_MSG "(COMMA|LPAREN) @ ..."; mk (T.IDENT_V s)

                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP | PTR_HAT when env#type_paren_flag
                      -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP|PTR_HAT) @"; mk (T.IDENT_V s)

                    | CONST when env#type_paren_flag && begin
                        match prev_rawtoken2 with
                        | PTR_STAR | PTR_AMP | PTR_AMP_AMP | PTR_HAT -> true
                        | _ -> false
                    end -> DEBUG_MSG "(PTR_STAR|PTR_AMP|PTR_AMP_AMP|PTR_HAT) CONST @"; mk (T.IDENT_V s)

                    | _ -> begin
                        DEBUG_MSG "* @";
                        match self#peek_rawtoken() with
                        | NAMESPACE | EXTERN | TEMPLATE when begin
                            match self#peek_nth_rawtoken 2 with
                            | LBRACKET | COMMA -> false
                            | _ -> true
                        end -> DEBUG_MSG "@ (NAMESPACE|EXTERN|TEMPLATE)"; mk (T.DECL_MACRO s)

                        | USING when context == STMT -> DEBUG_MSG "@ USING"; mk (T.STMT_MACRO s)

                        | USING -> DEBUG_MSG "@ USING"; mk (T.DECL_MACRO s)

                        | ELLIPSIS when begin
                            env#templ_head_flag && env#templ_param_arg_level > 0 &&
                            not env#typename_flag
                        end -> DEBUG_MSG "@ ELLIPSIS"; mk (T.IDENT_C s)

                        | PP_DEFINE | PP_UNDEF | PP_INCLUDE | PP_IMPORT
                        | PP_IF | PP_IFDEF | PP_IFNDEF | PP_ENDIF | EOF when begin
                            not env#objc_class_interface_flag && not env#ty_param_flag && not env#pp_ifx_d_flag &&
                            match prev_rawtoken with
                            | COLON_COLON -> false
                            | _ -> begin
                                match self#peek_nth_rawtoken 2 with
                                | NEWLINE -> begin
                                    match self#peek_nth_rawtoken 3 with
                                    | IDENT _ -> begin
                                        match self#peek_nth_rawtoken 4 with
                                        | NAMESPACE -> true
                                        | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                                            let nth, _ =
                                              self#peek_rawtoken_up_to ~from:5 [T.NEWLINE]
                                            in
                                            match self#peek_nth_rawtoken (nth+1) with
                                            | STATIC -> true
                                            | _ -> false
                                        end
                                        | _ -> false
                                    end
                                    | _ -> true
                                end
                                | _ -> true
                            end
                        end -> DEBUG_MSG "@ ..."; mk (T.DECL_MACRO s)

                        | COLON | LBRACE when begin
                            not env#objc_class_interface_flag &&
                            match prev_rawtoken with
                            | RPAREN when prev_rawtoken2 != RPAREN -> true
                            | CONST -> true
                            | _ -> false
                        end -> DEBUG_MSG "@ (COLON|LBRACE)"; mk (T.VIRT_SPEC_MACRO s)

                        | SEMICOLON _ when begin
                            not env#objc_class_interface_flag &&
                            not env#alias_flag &&
                            match prev_rawtoken with
                            | RPAREN when prev_rawtoken2 != RPAREN -> true
                            | CONST -> true
                            | _ -> false
                        end -> DEBUG_MSG "@ SEMICOLON"; mk (T.VIRT_SPEC_MACRO s)

                        | COLON when context == MEM && sub_context == INI && prev_rawtoken == EOF 
                          -> DEBUG_MSG "@ COLON"; mk (T.ACC_SPEC_MACRO s)

                        | _ ->
                            match prev_rawtoken with
                            | PTR_STAR | PTR_AMP | PTR_AMP_AMP | RESTRICT _ | PTR_MACRO _ when
                                env#type_paren_flag
                              -> DEBUG_MSG "@"; mk (T.IDENT_V s)

                            | NEWLINE | LBRACE -> begin
                                match self#peek_rawtoken() with
                                | RBRACE -> DEBUG_MSG "@"; mk (T.DECL_MACRO s)
                                | STR_LITERAL _ | TEMPL_LT when s = "import" -> DEBUG_MSG "@"; mk T.IMPORT
                                | _ -> DEBUG_MSG "@"; token
                            end

                            | NOEXCEPT | CONST when self#peek_rawtoken() == LBRACE
                              -> DEBUG_MSG "(NOEXCEPT|CONST) @"; mk (T.ATTR_MACRO s)

                            | SEMICOLON _ | DECL_MACRO _ | OBJC_END when begin
                                match self#peek_rawtoken() with
                                | OBJC_INTERFACE | OBJC_PROTOCOL -> true
                                | _ -> false
                            end -> begin
                              DEBUG_MSG "(SEMICOLON|DECL_MACRO) @ (OBJC_INTERFACE|OBJC_PROTOCOL)";
                              mk (T.ATTR_MACRO s)
                            end

                            | SEMICOLON _ | DECL_MACRO _ | PUBLIC | PRIVATE | PROTECTED when begin
                                s = "ref" &&
                                match self#peek_rawtoken() with
                                | CLASS -> true
                                | _ -> false
                            end -> DEBUG_MSG "@"; mk T.MS_REF

                            | _ -> DEBUG_MSG "@"; token
                    end
                end
                | _ when env#templ_arg_flag && begin
                    match self#peek_rawtoken() with
                    | PTR_AMP_AMP | BAR_BAR _ -> begin
                        match self#peek_nth_rawtoken 2 with
                        | IDENT _ -> true
                        | _ -> false
                    end
                    | RPAREN when not env#type_paren_flag -> true
                    | COMMA when prev_rawtoken == COLON_COLON ->
                        prev_rawtoken2 == TEMPL_GT && not env#last_ty_templ_id_flag || begin
                          match prev_rawtoken2 with
                          | IDENT _ -> prev_rawtoken3 == COLON && is_literal prev_rawtoken4
                          | _ -> false
                        end
                    | _ -> false
                end -> begin
                  DEBUG_MSG "@";
                  (*self#ctx_expr();
                  self#ctx_ini();*)
                  mk (T.IDENT_V s)
                end

                | _ when begin
                    match prev_rawtoken with
                    | DOT (*| MINUS_GT*) -> begin
                        match self#peek_rawtoken() with
                        | COLON_COLON | TEMPL_LT -> false
                        | _ -> true
                    end
                    | COMMA -> begin
                        match prev_rawtoken2 with
                        | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                        | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                        | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                        | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

                | _ when not env#objc_class_interface_flag && begin
                    begin
                      match prev_rawtoken with
                      | RPAREN when prev_rawtoken2 != RPAREN -> true
                      | CONST -> true
                      | _ -> false
                    end && begin
                      match self#peek_rawtoken() with
                      | COLON | LBRACE when not env#alignas_flag && context != CLASS -> true
                      | _ -> false
                    end
                end -> DEBUG_MSG "@"; mk (T.VIRT_SPEC_MACRO s)

                | _ when env#macro_arg_flag && begin
                    contains_comma s
                end -> mk (T.DELIM_MACRO s)

                (*| MEM, INI -> begin
                    DEBUG_MSG "@";
                    match prev_rawtoken with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP when env#type_paren_flag -> DEBUG_MSG "@"; mk (T.IDENT_V s)
                    | NEWLINE -> begin
                        match self#peek_rawtoken() with
                        | RBRACE -> DEBUG_MSG "@"; mk (T.DECL_MACRO s)
                        | _ -> DEBUG_MSG "@"; token
                    end
                    | _ -> DEBUG_MSG "@"; token
                end*)

                | _ when begin
                    match prev_rawtoken with
                    | OVERRIDE | FINAL | CONST -> begin
                        env#end_of_params_flag &&
                        match self#peek_rawtoken() with
                        | EOF -> self#prepend_token (mk_ (T.SEMICOLON false)); true
                        | LBRACE | COLON | SEMICOLON _ -> true
                        | EQ when begin
                            match self#peek_nth_rawtoken 2 with
                            | INT_LITERAL "0" -> true
                            | _ -> false
                        end -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "(OVERRIDE|FINAL) @ (LBRACE|COLON|EOF)"; mk (T.VIRT_SPEC_MACRO s)

                | _ when begin
                    match self#peek_rawtoken() with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken 2 with
                        | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
                        | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> true
                        | SIZEOF -> true
                        | TY_LPAREN -> begin
                            match self#peek_nth_rawtoken 3 with
                            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
                            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> true
                            | SIZEOF -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP) ..."; mk (T.IDENT_V s)

                | _ when begin
                    match prev_rawtoken with
                    | IDENT _ | RBRACKET when begin
                        env#type_paren_flag &&
                        match self#peek_rawtoken() with
                        | RPAREN -> true
                        | _ -> false
                    end -> true
                    | _ -> false
                end -> DEBUG_MSG "(IDENT|RBRACKET) @ RPAREN"; mk (T.PARAM_DECL_MACRO s)

                | _ when is_val s -> DEBUG_MSG "@"; mk (T.IDENT_V s)

                | _ when begin
                    match prev_rawtoken with
                    | COMMA | LPAREN -> begin
                        match self#peek_rawtoken() with
                        | PLUS_PLUS | MINUS_MINUS -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "(COMMA|LPAREN) @ (PLUS_PLUS|MINUS_MINUS)"; mk (T.IDENT_V s)

                | _ when begin
                    match prev_rawtoken with
                    | COLON_COLON -> begin
                        match self#peek_rawtoken() with
                        | PP_ENDIF -> begin
                            match self#peek_nth_rawtoken 2 with
                            | NEWLINE -> begin
                                match self#peek_nth_rawtoken 3 with
                                | RPAREN -> begin
                                    match self#peek_nth_rawtoken 4 with
                                    | PTR_AMP_AMP | BAR_BAR _ -> true
                                    | _ -> false
                                end
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "COLON_COLON @"; mk (T.IDENT_V s)

                | _ -> DEBUG_MSG "@"; token
            end
        end
    end
  in

  let conv_ty_lparen () =
    DEBUG_MSG "@";
    let followed_by_type =
      let cache = ref None in
      fun () ->
        match !cache with
        | Some b -> DEBUG_MSG "b=%B" b; b
        | None -> begin
            DEBUG_MSG "checking...";
            let b =
              match self#peek_rawtoken() with
              | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
              | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | STRUCT
              | MS_CDECL _ | MS_STDCALL _ | CC_MACRO _ | TYPENAME | DECLTYPE
              | UNSIGNED when begin
                  match self#peek_nth_rawtoken 2 with
                  | TY_LPAREN -> begin
                      let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:3 None in
                      (match self#peek_nth_rawtoken (nth+1) with
                      | TY_LPAREN | LBRACKET -> true
                      | _ -> false) &&
                      match l with
                      | PTR_STAR::_ -> true
                      | _ -> begin
                          match List.rev l with
                          | (PTR_STAR|HAT _)::_ -> true
                          | _ -> false
                      end
                  end
                  | EQ_EQ | EXCLAM_EQ _ -> false
                  | _ -> true
              end -> true
              | IDENT x when (is_type_name x || is_type x) && begin
                  match self#peek_nth_rawtoken 2 with
                  | TY_LPAREN | COLON_COLON -> false
                  | _ -> true
              end -> true
              | IDENT x when is_type_macro_ident x -> true
              (*| x when is_ty x -> true*)
              | _ -> false
            in
            DEBUG_MSG "followed_by_type=%B" b;
            cache := Some b;
            b
        end
    in
    match context, sub_context with
    | STMT, END_OF_TY_SPEC when begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | IDENT_V _ -> begin
            match self#peek_rawtoken() with
            | RPAREN -> true
            | _ -> false
        end
        | LPAREN -> false
        | _ -> begin
            match self#peek_rawtoken() with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when not env#cast_key_flag -> true
            | _ -> false
        end
    end -> DEBUG_MSG "@"; token

    | STMT, START_OF_STMT _ when begin
        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
        (match self#peek_nth_rawtoken (nth+1) with
        | SEMICOLON _ -> true
        | _ -> false) &&
        match ll with
        | [[ELLIPSIS];_] -> true
        | [_;[ELLIPSIS]] -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.FOLD_LPAREN

    | EXPR, _ | NEW, _ | MEM_INIT, _ | STMT, _  -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | IF | FOR | WHILE | SWITCH
        | IDENT_SM _ | IDENT_EM _ | IDENT_TM _ | IDENT_LM _ -> DEBUG_MSG "... @"; mk T.LPAREN

        | RETURN | EOF | LBRACE | EXCLAM _ | COMMA | LPAREN when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            match ll with
            | [l] -> begin
                (match l with
                | ELLIPSIS::x::_ -> is_fold_op x
                | _ -> false
                ) ||
                contained_in_list_f
                  (function
                    | x::T.ELLIPSIS::y::_ -> is_fold_op x && x == y
                    | _ -> false
                  ) l
            end
            | [ELLIPSIS]::_ -> true
            | _ -> false
        end -> DEBUG_MSG "... @"; mk T.FOLD_LPAREN

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
            not env#conv_func_id_flag &&
            prev_rawtoken3 != OPERATOR &&
            match context, sub_context with
            | NEW, END_OF_TY_SPEC -> false
            | _ -> true
        end -> DEBUG_MSG "@"; token

        | CATCH | TY_HAT -> DEBUG_MSG "(CATCH|TY_HAT) @"; token

        | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | FLOAT | DOUBLE when begin
            match prev_rawtoken2 with
            | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
            | PLUS | MINUS | STAR | SLASH | PERC
            | EXCLAM _ | AMP _ | BAR _ | AMP_AMP _ | BAR_BAR _
            | RETURN | LBRACKET -> true
            | EQ when not env#alias_flag -> true
            | LPAREN when begin
                match prev_rawtoken3 with
                | IDENT_V _ -> true
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "(CHAR|...) @"; mk T.LPAREN

        | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | AUTO | IDENT _ when begin
            (match prev_rawtoken with IDENT x -> is_type_name x || is_type x | _ -> true) &&
            match prev_rawtoken2 with
            | RETURN -> false
            | SEMICOLON _ | RBRACE | NEWLINE | LBRACE -> begin
                match self#peek_rawtoken() with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                | _ -> false
            end
            | COMMA when env#arg_paren_flag -> false
            | EQ when prev_rawtoken3 == RBRACKET -> false
            | EQ when env#alias_flag -> true
            | LPAREN when begin
                match prev_rawtoken3 with
                | IF | SWITCH | WHILE
                | AMP_AMP _ | BAR_BAR _ -> true
                | PLUS | MINUS | STAR | SLASH | PERC | AMP _ | BAR _ -> true
                | IDENT_V _ when context == EXPR -> true
                | _ -> false
            end -> false
            | _ -> begin
                let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                begin
                  match self#peek_nth_rawtoken (nth+1) with
                  | QUEST | PTR_AMP_AMP | BAR_BAR _ -> false
                  | _ -> true
                end &&
                match l with
                | IDENT _::PTR_STAR::IDENT _::[] -> true
                | IDENT _::IDENT _::_ -> true
                | PTR_STAR::_ -> true
                | _ -> filt_at_level0 l is_basic_ty
            end
        end -> DEBUG_MSG "(CHAR|...) @"; token

        | VOID when self#peek_rawtoken() != RPAREN && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            match ll with
            | [l] when list_memqn [T.LBRACE] l-> false
            | _ -> true
        end -> DEBUG_MSG "VOID @"; token

        | RBRACKET when begin
            prev_rawtoken2 == LBRACKET &&
            not env#in_body_brace_flag && context != EXPR &&
            match prev_rawtoken3 with
            | OPERATOR -> true
            | _ -> false
        end -> DEBUG_MSG "OPERATOR LBRACKET RBRACKET @"; token

        | RBRACKET when prev_rawtoken2 != LBRACKET || prev_rawtoken3 != DELETE
          -> DEBUG_MSG "RBRACKET @"; mk T.LPAREN

        | COLON when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            match ll with
            | [l] -> begin
                match self#peek_nth_rawtoken (nth+1) with
                | TY_LPAREN -> begin
                    let nth', ll' =
                      self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) ()
                    in
                    match ll' with
                    | [] -> false
                    | [[x]] -> is_literal x
                    | [_] -> false
                    | _ -> true
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COLON @"; mk T.LPAREN

        | RPAREN when begin
            not env#stack#at_block &&
            prev_rawtoken2 == LPAREN && prev_rawtoken3 == OPERATOR &&
            not env#in_body_brace_flag && context != EXPR
        end -> DEBUG_MSG "OPERATOR LPAREN RPAREN @"; token

        | IDENT _ when begin
            match prev_rawtoken2 with
            | SEMICOLON _ | RBRACE | NEWLINE -> begin
                match self#peek_rawtoken() with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    match self#peek_nth_rawtoken 2 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 3 with
                        | RPAREN -> begin
                            match self#peek_nth_rawtoken 4 with
                            | TY_LPAREN | PARAMS_MACRO _ -> true
                            | _ -> false
                        end
                        | LBRACKET -> begin
                            match self#peek_nth_rawtoken 4 with
                            | RBRACKET -> begin
                                match self#peek_nth_rawtoken 5 with
                                | RPAREN -> begin
                                    match self#peek_nth_rawtoken 6 with
                                    | TY_LPAREN | PARAMS_MACRO _ -> true
                                    | _ -> false
                                end
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | TY_HAT -> true
            | EQ when env#using_flag && env#alias_flag -> true
            | _ -> begin
                let _, l = self#peek_rawtoken_up_to_rparen_none() in
                match l with
                (*| IDENT _::PTR_STAR::IDENT _::[] -> true*)
                | PTR_STAR::_ -> true
                (*| _ -> filt_at_level0 l is_basic_ty*)
                | _ -> false
            end
        end -> DEBUG_MSG "IDENT @"; token

        | RPAREN when begin
            DEBUG_MSG "@";
            match prev_rawtoken4, prev_rawtoken3, prev_rawtoken2 with
            | _, TY_LPAREN, (PTR_STAR|PTR_AMP|PTR_HAT)
            | TY_LPAREN, (CC_MACRO _|MS_STDCALL _|MS_CDECL _), (PTR_STAR|PTR_AMP|PTR_HAT)
            | IDENT _, COLON_COLON, (PTR_STAR|PTR_AMP|PTR_HAT)
            | TY_LPAREN, HEAD_COLON_COLON, (PTR_STAR|PTR_AMP|PTR_HAT)
            | _, TY_LPAREN, IDENT_V _
            | TY_LPAREN, (PTR_STAR|PTR_AMP|PTR_HAT), IDENT_V _
            | (CC_MACRO _|MS_STDCALL _|MS_CDECL _), (PTR_STAR|PTR_AMP|PTR_HAT), IDENT_V _
            | (PTR_STAR|PTR_AMP|PTR_HAT), CONST, IDENT_V _
              -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                check_if_params ~weak:true ll ||
                match self#peek_nth_rawtoken (nth+1) with
                | RPAREN -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | IDENT _ -> true
                    | _ -> false
                end
                | _ -> false
              end
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; token

        | RPAREN when begin
            match prev_rawtoken2 with
            | IDENT_V _ when prev_rawtoken3 == PTR_STAR -> begin
                match prev_rawtoken4 with
                | PTR_STAR -> true
                | TY_LPAREN | COLON_COLON | RPAREN -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; token

        | SIZEOF -> begin
            DEBUG_MSG "SIZEOF @";
            match self#peek_rawtoken() with
            | DECLTYPE when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                match self#peek_nth_rawtoken (nth+1) with
                | RPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "@ DECLTYPE"; token

            | IDENT x when is_str_macro x -> DEBUG_MSG "@ IDENT"; mk T.LPAREN

            | IDENT x when is_type_macro_ident x && begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                match self#peek_nth_rawtoken (nth+1) with
                | RPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "@ IDENT"; token

            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | DOT | MINUS_GT -> true
                | LBRACKET when self#peek_nth_rawtoken 3 != RBRACKET -> true
                | IDENT _ when begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | RPAREN -> begin
                            conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 1;
                            conv_nth_token (function T.IDENT x,s,e -> T.OP_MACRO x,s,e | x -> x) 2;
                            conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 3;
                            true
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> true
                | RPAREN when self#peek_nth_rawtoken 3 == LBRACKET -> true
                | _ -> begin
                    let _, l = self#peek_rawtoken_up_to_rparen_none() in
                    match l with
                    | IDENT _::(DOT|MINUS_GT)::_ -> true
                    | RPAREN::TY_LPAREN::_ -> true
                    | RPAREN::x::TY_LPAREN::_ when is_literal x -> true
                    | RPAREN::x::_ when is_literal x -> true
                    | _ -> false
                end
            end -> DEBUG_MSG "@ IDENT"; mk T.LPAREN

            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _ 
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
            | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
            | PTR_STAR | PTR_AMP | THIS | NEW -> DEBUG_MSG "@ (INT_LITERAL|...)"; mk T.LPAREN

            | TY_LPAREN | LPAREN -> DEBUG_MSG "@ TY_LPAREN"; mk T.LPAREN

            | EXCLAM _ -> DEBUG_MSG "@ EXCLAM"; mk T.LPAREN

            | FINAL when self#peek_nth_rawtoken 2 == RPAREN -> DEBUG_MSG "@ FINAL"; mk T.LPAREN

            | _ when begin
                let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                (match self#peek_nth_rawtoken (nth+1) with
                | MINUS_GT -> true
                | _ -> false) ||
                match l with
                | [IDENT x] when _is_val x -> true
                | RPAREN::TY_LPAREN::_::OPERATOR::_ -> true
                | RPAREN::_::TY_LPAREN::IDENT _::_ -> true
                | _ -> false
            end -> DEBUG_MSG "@"; mk T.LPAREN

            | _ -> begin
                DEBUG_MSG "@";
                if not env#pp_line_flag then 
                  env#enter_sizeof_ty();
                token
            end
        end

        | EQ | SEMICOLON _ when begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match l with
            | [T.IDENT _;IDENT _] -> begin
                match self#peek_nth_rawtoken (nth+1) with
                | SEMICOLON _ | EOF -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "EQ @"; mk T.LPAREN

        | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ | QUEST
        | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
        | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ when begin
            let nth, _ = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ when begin
                match self#peek_nth_rawtoken 1 with
                | TY_LPAREN -> true
                | _ -> false
            end -> false
            | IDENT _ -> true
            | _ ->
            match self#peek_rawtoken() with
            | VOLATILE | CONST -> true
            | x when begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN | LPAREN -> false
                | _ ->
                    match prev_rawtoken with
                    | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ -> true
                    | _ -> false
            end && is_basic_ty x -> true
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | RPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ | NEW
                    | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                    | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                    | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
                    | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> true
                    | TY_LPAREN -> is_basic_ty (self#peek_nth_rawtoken 4)
                    | _ -> false
                end
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> true
                    | _ -> false
                end
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        let _, nth, _ = self#peek_rawtoken_up_to_rparen ~from:4 None in
                        match self#peek_nth_rawtoken (nth-1) with
                        | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(EQ_EQ|...) @"; token

        | IDENT_V _ when begin
           match prev_rawtoken2 with
           | RPAREN -> prev_rawtoken3 == AUTO
           | _ -> false
        end -> DEBUG_MSG "@"; token

        | IDENT_V _ when begin
           match prev_rawtoken2 with
           | MINUS_GT | DOT | EQ(* | LBRACE*)
           | AMP_AMP _ | BAR_BAR _ | EXCLAM _ | LPAREN -> true
           | COLON when context != STMT || sub_context != INI -> true
           | _ -> false
        end -> DEBUG_MSG "@"; mk T.LPAREN

        | NEW when begin
            match self#peek_rawtoken() with
            | TYPENAME -> true
            | x when is_basic_ty x -> true
            | IDENT x when is_type_name x || is_type x -> true
            | _ -> false
        end -> DEBUG_MSG "NEW @"; token

        | NEW when begin
            let nth, _ = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> true
            | _ -> false
        end -> DEBUG_MSG "NEW @"; mk T.LPAREN

        | LPAREN when prev_rawtoken2 == WHILE && begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | PTR_AMP_AMP | BAR_BAR _ -> true
            | _ -> false
        end -> DEBUG_MSG "WHILE LPAREN @"; mk T.LPAREN

        | COMMA when begin
            env#macro_arg_flag &&
            match self#peek_rawtoken() with
            | IDENT x when not (is_type_name x || is_type x) -> begin
                match self#peek_nth_rawtoken 2 with
                | RPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COMMA | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "COMMA @"; mk T.LPAREN

        | COMMA | LPAREN when begin
            (not env#in_body_brace_flag && followed_by_type()) ||
            begin
              match self#peek_rawtoken() with
              | EXCLAM _ -> false
              | _ -> true
            end &&
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | LBRACE -> true
            | THIS -> true
            | IDENT _ | IDENT_V _ -> begin
                match l with
                | [IDENT _] -> true
                | _ -> false
            end
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                (match self#peek_nth_rawtoken (nth+2) with
                | SIZEOF -> false
                | _ -> true) &&
                match l with
                | RPAREN::TY_LPAREN::RPAREN::_ -> true
                | RPAREN::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> true
                | RPAREN::x::_ when is_basic_ty x -> true
                | _ -> false
            end
            | COMMA when env#macro_arg_flag -> begin
                let ll = split_at_comma l in
                not (List.exists (function [x] -> is_literal x | _ -> false) ll) &&
                match l with
                | [] -> true
                | [IDENT _] -> false
                | IDENT x::COMMA::_ when is_val x -> false
                | IDENT _::_ when not (List.exists is_op l) -> true
                | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(COMMA|LPAREN) @"; token

        | ELLIPSIS when prev_rawtoken2 == SIZEOF -> DEBUG_MSG "SIZEOF ELLIPSIS @"; mk T.LPAREN

        | TYPEID when begin
            match self#peek_rawtoken() with
            | TYPENAME -> true
            | _ -> false
        end -> DEBUG_MSG "TYPEID @"; token

        | IDENT_V _ when
            not env#in_body_brace_flag &&
            not env#end_of_params_flag &&
            followed_by_type()
          -> DEBUG_MSG "IDENT_V @"; token

        | IDENT_V _ when
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            List.exists
              (fun l ->
                match (l : T.token list) with
                | [x] when is_literal x -> true
                | _ -> false
              ) ll
          -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

        | DOT_STAR | MINUS_GT_STAR when begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "(DOT_STAR|MINUS_GT_STAR) @"; mk T.LPAREN

        | _ when begin
            match self#peek_rawtoken() with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | RPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | INT_LITERAL _ | FLOAT_LITERAL _
                    | CHAR_LITERAL _ | STR_LITERAL _ | TILDE _ -> true
                    | MINUS_MINUS | PLUS_PLUS -> begin
                        match self#peek_nth_rawtoken 4 with
                        | IDENT _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "* @"; token

        | _ when begin
            let chk n = (* (...)(...) *)
              match self#peek_nth_rawtoken n with
              | TY_LPAREN -> begin
                  let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(n+1) () in
                  List.length ll = 1 &&
                  match self#peek_nth_rawtoken (nth+1) with
                  | TY_LPAREN -> begin
                      let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                      match self#peek_nth_rawtoken (nth'+1) with
                      | RPAREN -> true
                      | _ -> false
                  end
                  | _ -> false
              end
              | _ -> false
            in
            match self#peek_rawtoken() with
            | x when is_ty x -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> chk 2
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> chk 3
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_LPAREN -> chk 4
                        | _ -> false
                    end
                    | _ -> false
                end
                | y when is_ty y -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> chk 3
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_LPAREN -> chk 4
                        | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                            match self#peek_nth_rawtoken 5 with
                            | TY_LPAREN -> chk 5
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "* @"; token

        | _ when begin
            match self#peek_rawtoken() with
            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
            | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> true
            | NEW -> true
            | IDENT x when is_val x -> true
            | IDENT x -> begin
                match self#peek_nth_rawtoken 2 with
                | IDENT y when y = x -> true
                | DOT | MINUS_GT -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "* @"; mk T.LPAREN

        | _ when not env#cast_key_flag -> begin
            try
              let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~filt:ty_pat2 () in
              DEBUG_MSG "|ll|=%d\n%s" (List.length ll)
                (String.concat "\n"
                   (List.map
                      (fun x -> String.concat ";"
                          (List.map Token.rawtoken_to_string x)) ll));

              let param_flag = ref false in

              match (ll : T.token list list) with
              | [_] when begin
                  begin
                    match prev_rawtoken with
                    | COMMA | TY_LPAREN | LBRACKET | OBJC_LBRACKET -> true
                    | _ -> false
                  end && begin
                    match self#peek_nth_rawtoken (nth+1) with
                    | IDENT _ | NULLPTR -> true
                    | _ -> false
                  end &&
                  match self#peek_nth_rawtoken (nth+2) with
                  | COMMA | RPAREN -> true
                  | IDENT _ when env#in_objc_message_expr -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; token

              | [[IDENT _]] when context == EXPR && begin
                  match self#peek_nth_rawtoken (nth+1) with
                  | INI_LBRACE | LBRACE -> true
                  | TY_LPAREN when prev_rawtoken == LPAREN && begin
                      let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                      match ll' with
                      | [_] -> true
                      | _ -> false
                  end -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; token

              | [(PTR_STAR|PTR_AMP|PTR_AMP_AMP|HAT _)::_] -> DEBUG_MSG "@"; token

              | [l] when List.for_all is_ty l -> DEBUG_MSG "@"; token

              | [RBRACKET::LBRACKET::l] when List.for_all is_ty l -> DEBUG_MSG "@"; token

              | [[x]|[IDENT _;x]|(*[(PTR_STAR|PTR_AMP|PTR_AMP_AMP);x]|*)
                [RBRACKET;LBRACKET;x]|[RBRACKET;_;LBRACKET;x]|[ELLIPSIS;x]] when is_ty x
                -> DEBUG_MSG "@"; token

              | [[IDENT _;ENUM]] -> DEBUG_MSG "@"; token

              (*| [(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::COLON_COLON::_] -> DEBUG_MSG "@"; token*)

              | [[IDENT _;(PTR_STAR|PTR_AMP|PTR_AMP_AMP)]|
                [IDENT _;(PTR_STAR|PTR_AMP|PTR_AMP_AMP);(MS_STDCALL _|MS_CDECL _|CC_MACRO _)]] when begin
                  (*not env#in_body_brace_flag && *)begin
                    match context, sub_context with
                    | MEM_INIT, _ when prev_rawtoken == EOF -> true
                    | (EXPR(*|STMT*)), _ | NEW, _ | MEM_INIT, _ | _, START_OF_STMT _ -> false
                    | _ -> true
                  end &&
                  match prev_rawtoken with
                  | DECLTYPE | SEMICOLON _ | RBRACE -> false
                  | IDENT_V _ when context == STMT -> false
                  | _ -> begin
                      match self#peek_nth_rawtoken (nth+1) with
                      | PLUS_PLUS | MINUS_MINUS | PLUS | MINUS | SLASH | PERC
                      | MINUS_GT_STAR | DOT_STAR | MINUS_GT | DOT(* | EOF*) -> false
                      | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
                      | SEMICOLON _ | RPAREN | LBRACKET | COMMA -> false
                      | _ -> true
                  end
              end -> DEBUG_MSG "@"; token

              | [IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::COLON_COLON::_] -> DEBUG_MSG "@"; token

              | [VOLATILE::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_] -> DEBUG_MSG "@"; token

              | (PTR_STAR::_)::_ -> DEBUG_MSG "@"; token

              | (TY_TEMPL_GT::TEMPL_LT::_)::_ -> DEBUG_MSG "@"; token

              | [[IDENT _;RPAREN;(PTR_STAR|PTR_AMP|PTR_AMP_AMP);TY_LPAREN;IDENT _]] -> DEBUG_MSG "@"; token

              | [l] when begin
                  DEBUG_MSG "@";
                  let chk n =
                    let nth0, ll0 = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:n () in
                    DEBUG_MSG "|ll0|=%d\n%s" (List.length ll0)
                      (String.concat "\n"
                         (List.map
                            (fun x -> String.concat ";"
                                (List.map Token.rawtoken_to_string x)) ll0));
                    match self#peek_nth_rawtoken (nth0+1) with
                    | TY_LPAREN -> begin
                        match ll0 with
                        | [l0] when check_if_noptr_dtor l0 -> begin
                            let nth1, ll1 =
                              self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth0+2) ()
                            in
                            DEBUG_MSG "|ll1|=%d\n%s" (List.length ll1)
                              (String.concat "\n"
                                 (List.map
                                    (fun x -> String.concat ";"
                                        (List.map Token.rawtoken_to_string x)) ll1));
                            check_if_params ~weak:true ll1
                        end
                        | _ -> false
                    end
                    | _ -> false
                  in
                  let is_id = function
                    | T.IDENT _ -> true
                    | _ -> false
                  in
                  DEBUG_MSG "@";
                  match self#peek_rawtoken() with
                  | x when is_ty x || is_id x -> begin
                      match self#peek_nth_rawtoken 2 with
                      | TY_LPAREN -> chk 3
                      | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                          match self#peek_nth_rawtoken 3 with
                          | TY_LPAREN -> chk 4
                          | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                              match self#peek_nth_rawtoken 4 with
                              | TY_LPAREN -> chk 5
                              | _ -> false
                          end
                          | _ -> false
                      end
                      | y when is_ty y || is_id y -> begin
                          match self#peek_nth_rawtoken 3 with
                          | TY_LPAREN -> chk 4
                          | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                              match self#peek_nth_rawtoken 4 with
                              | TY_LPAREN -> chk 5
                              | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                                  match self#peek_nth_rawtoken 5 with
                                  | TY_LPAREN -> chk 6
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | _ -> false
                      end
                      | _ -> false
                  end
                  | _ -> false
              end -> DEBUG_MSG "@"; token

              | [l] when begin
                  let rev_l = List.rev l in
                  DEBUG_MSG "rev_l=%s" (String.concat ";" (List.map Token.rawtoken_to_string rev_l));
                  (match self#peek_nth_rawtoken (nth+1) with
                  | TY_LPAREN | LBRACKET -> true
                  | PTR_AMP -> true
                  | _ -> false) &&
                  match rev_l with
                  | x::r when is_ty x && check_if_noptr_dtor_ r -> true
                  | x::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::r when is_ty x && check_if_noptr_dtor_ r -> true

                  | x::y::r when is_ty x && is_ty y && check_if_noptr_dtor_ r -> true
                  | x::y::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::r when is_ty x && is_ty y && check_if_noptr_dtor_ r -> true

                  | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ when begin
                      match self#peek_nth_rawtoken (nth+1) with
                      | TY_LPAREN -> begin
                          match prev_rawtoken with
                          | x when is_ty x -> true
                          | _ -> false
                      end
                      | _ -> false
                  end -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; token

              | _ -> begin
                  DEBUG_MSG "@";
                  match self#peek_nth_rawtoken (nth+1) with

                  | EQ when begin
                      self#peek_nth_rawtoken (nth+2) == LBRACE &&
                      prev_rawtoken == RPAREN &&
                      prev_rawtoken2 == RBRACKET &&
                      (ll = [] ||
                      List.for_all
                        (fun (l : T.token list) ->
                          match List.rev l with
                          | [IDENT _] -> true
                          | [IDENT _;IDENT _] -> true
                          | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _] -> true
                          | [x] -> is_ty x
                          | [x;IDENT _] -> is_ty x
                          | x::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_ -> is_ty x
                          | _ -> false
                        ) ll)
                  end -> DEBUG_MSG "@"; token

                  | SEMICOLON _ when env#end_of_noptr_dtor_paren_flag -> DEBUG_MSG "@"; token

                  | IDENT x when begin
                      env#init_flag && is_param_decl_macro x ||
                      match self#peek_nth_rawtoken (nth+2) with
                      | IDENT _ -> true
                      | TY_LPAREN when not (is_ident_macro_ident x) -> true
                      | _ -> false
                  end -> DEBUG_MSG "@"; mk T.LPAREN

                  | IDENT x when begin
                      is_arg_macro x ||
                      env#arg_paren_flag &&
                      (match prev_rawtoken with
                      | IDENT_V _ -> true
                      | _ -> false) &&
                      match self#peek_nth_rawtoken (nth+2) with
                      | RPAREN -> begin
                          conv_nth_token (function T.IDENT x,s,e -> T.ARG_MACRO x,s,e | x -> x) (nth+1);
                          true
                      end
                      | _ -> false
                  end -> DEBUG_MSG "@"; mk T.LPAREN

                  | IDENT _ | INT_LITERAL _ | NULLPTR | SIZEOF | COLON_COLON when begin
                      match prev_rawtoken with
                      | IDENT _ -> false
                      | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ | QUEST
                      | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
                      | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ when begin
                          match self#peek_rawtoken() with
                          | TY_LPAREN -> true
                          | _ -> false
                      end -> false
                      | RPAREN when env#expr_flag -> false
                      | _ when List.exists (function x::_ -> is_literal x | _ -> false) ll -> false
                      | _ when begin
                          match ll with
                          | [l] -> list_memqn [T.PLUS;MINUS;SLASH] l
                          | _ -> false
                      end -> false
                      | _ -> true
                  end -> DEBUG_MSG "@"; token

                  | TY_LPAREN when begin
                      (not env#arg_paren_flag || prev_rawtoken2 == TY_LPAREN) &&
                      (match prev_rawtoken with
                      | x when is_basic_ty x -> true
                      | IDENT _ -> true
                      | _ -> false) &&
                      match ll with
                      | [l] -> begin
                          is_ty (self#peek_nth_rawtoken (nth+2)) ||
                          match (l : T.token list) with
                          | [PTR_STAR|PTR_AMP|PTR_AMP_AMP]
                          | [IDENT _;(PTR_STAR|PTR_AMP|PTR_AMP_AMP)]
                          | [IDENT _;CONST;(PTR_STAR|PTR_AMP|PTR_AMP_AMP)]
                          | [(PTR_STAR|PTR_AMP|PTR_AMP_AMP);(MS_STDCALL _|MS_CDECL _|CC_MACRO _)]
                          | [IDENT _;(PTR_STAR|PTR_AMP|PTR_AMP_AMP);(MS_STDCALL _|MS_CDECL _|CC_MACRO _)] -> true
                          | _ -> false
                      end
                      | _ -> false
                  end -> begin
                    DEBUG_MSG "@";
                    conv_nth_token (function T.TY_LPAREN,s,e -> T.TY_LPAREN_,s,e | x -> x) (nth+1);
                    token
                  end

                  | TY_LPAREN when begin
                      match ll with
                      | [l] -> begin
                          DEBUG_MSG "l=%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                          is_ty (self#peek_nth_rawtoken (nth+2)) && self#peek_nth_rawtoken (nth+3) != TY_LPAREN ||
                          match (l : T.token list) with
                          | x::_ when is_ty x -> true
                          | [IDENT _;x] when is_ty x -> true
                          | [(PTR_STAR|PTR_AMP|PTR_AMP_AMP);x] when is_ty x -> true
                          | [IDENT i;(PTR_STAR|PTR_AMP|PTR_AMP_AMP);x] when is_ty x && is_attr_macro i -> true
                          | [RBRACKET;LBRACKET;x] when is_ty x -> true
                          | [RBRACKET;_;LBRACKET;x] when is_ty x -> true

                          | IDENT _::_ -> begin
                              let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                              (match ll' with
                              | [l'] -> begin
                                  DEBUG_MSG "l'=%s" (String.concat ";" (List.map Token.rawtoken_to_string l'));
                                  match (l' : T.token list) with
                                  | ELLIPSIS::_ -> false
                                  | _ -> true
                              end
                              | _ -> false) &&
                              match self#peek_nth_rawtoken (nth'+1) with
                              | IDENT _ -> true
                              | PTR_STAR | PTR_AMP -> begin
                                  match self#peek_nth_rawtoken (nth'+2) with
                                  | IDENT _ -> true
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | _::_ when Xlist.last l == DECLTYPE -> true
                          | _ -> false
                      end
                      | _ -> false
                  end -> DEBUG_MSG "@"; token

                  | TY_LPAREN when not (is_ident prev_rawtoken) && begin
                      match ll with
                      | [] -> false
                      | [[_;PTR_STAR]] -> begin
                          let b = is_basic_ty (self#peek_nth_rawtoken (nth+2)) in
                          if b then
                            param_flag := true;
                          b
                      end
                      | [_::(MINUS_GT|DOT|DOT_STAR|MINUS_GT_STAR)::_] -> false
                      | [RBRACKET::_::LBRACKET::_] -> false
                      | _ -> true
                  end && begin
                    let _, nth', ll' = self#peek_rawtoken_up_to_rparen ~from:(nth+2) None in
                    ll' != [] &&
                    match self#peek_nth_rawtoken (nth'+1) with
                    | PTR_STAR | SLASH | PLUS | MINUS | PERC
                    | DOT | MINUS_GT when List.length ll = 1 -> true
                    | SEMICOLON _ when !param_flag -> true
                    | _ -> false
                  end -> DEBUG_MSG "@"; token

                  | LBRACKET when begin
                      (match prev_rawtoken with
                      | SEMICOLON _ | RBRACE | LBRACE | RPAREN | COLON
                      | PLUS_PLUS | MINUS_MINUS | DELETE -> false
                      | _ -> true
                      ) &&
                      (match context, sub_context with
                      | STMT, INI -> true
                      | _ -> false) &&
                      let nth', l' = self#peek_rawtoken_up_to_rbracket ~from:(nth+2) () in
                      match (l' : T.token list) with
                      | [INT_LITERAL _] | [IDENT _] -> begin
                          match self#peek_nth_rawtoken (nth'+1) with
                          | SEMICOLON _ | EQ -> begin
                              match ll with
                              | [l] -> begin
                                  match (l : T.token list) with
                                  | [IDENT _;CONST;PTR_STAR]
                                  | [IDENT _;PTR_STAR] -> true
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | LBRACKET -> begin
                              let nth'', l'' = self#peek_rawtoken_up_to_rbracket ~from:(nth'+2) () in
                              match self#peek_nth_rawtoken (nth''+1) with
                              | SEMICOLON _ | EQ -> begin
                                  match ll with
                                  | [l] -> begin
                                      match (l : T.token list) with
                                      | [IDENT _;CONST;PTR_STAR]
                                      | [IDENT _;PTR_STAR] -> true
                                      | _ -> false
                                  end
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | _ -> false
                      end
                      | _ -> false
                  end -> DEBUG_MSG "@"; token

                  | _ when prev_rawtoken == LAM_MARKER -> DEBUG_MSG "@"; token

                  | RPAREN when
                      env#end_of_params_flag &&
                      env#type_paren_flag &&
                      not env#init_flag
                    -> DEBUG_MSG "@"; token

                  | EXCLAM _ | THIS -> DEBUG_MSG "@"; token

                  | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                      match (ll : T.token list list) with
                      | [(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::_] -> true
                      | _ -> false
                  end -> DEBUG_MSG "@"; token

                  | LBRACE when begin
                      env#pp_ifx_d_flag &&
                      match prev_rawtoken with
                      | IDENT_V _ -> begin
                          match prev_rawtoken2 with
                          | VOID -> true
                          | _ -> false
                      end
                      | _ -> false
                  end -> DEBUG_MSG "@"; token

                  | rt -> DEBUG_MSG "@ ... RPAREN %s" (Token.rawtoken_to_string rt); mk T.LPAREN
              end
            with
              Found ->
                DEBUG_MSG "@";
                match prev_rawtoken with
                | EOF | IDENT_V _ | RPAREN when context == EXPR && begin
                    (prev_rawtoken != RPAREN || env#end_of_id_macro_call_flag) &&
                    let nth, _ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                    let count =
                      ref ((List.fold_left (fun a l -> a + List.length l) 0 _ll) + (List.length _ll) - 1)
                    in
                    DEBUG_MSG "count=%d" !count;
                    incr count;
                    let ll =
                      List.map (fun l -> decr count; List.rev_map (fun x -> decr count; (!count, x)) l) _ll
                    in
                    let nll = List.length ll in
                    DEBUG_MSG "|ll|=%d\n%s" nll
                      (String.concat "\n"
                         (List.map
                            (fun l ->
                              String.concat ";"
                                (List.map
                                   (fun (i, x) ->
                                     Printf.sprintf "%d:%s" i (Token.rawtoken_to_string x)
                                   ) l
                                )) ll));
                    match self#peek_nth_rawtoken (nth+1) with
                    | EOF | TY_LPAREN | SEMICOLON _ -> begin
                        let found = ref false in
                        List.iter
                          (function
                            | [i0,T.IDENT _; i1,IDENT _] -> begin
                                DEBUG_MSG "i0=%d i1=%d" i0 i1;
                                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (i0+1);
                                conv_nth_token (function T.IDENT x,s,e -> T.ARGS_MACRO x,s,e | x -> x) (i1+1);
                                found := true
                            end
                            | _ -> ()) ll;
                        !found
                    end
                    | RPAREN when nll > 1 && context == EXPR -> true
                    | IDENT _ when begin
                        nll > 1 && context == EXPR &&
                        match self#peek_nth_rawtoken (nth+2) with
                        | EOF -> true
                        | _ -> false
                    end -> begin
                      conv_nth_token (function T.IDENT x,s,e -> T.SUFFIX_MACRO x,s,e | x -> x) (nth+1);
                      true
                    end
                    | _ -> false
                end -> DEBUG_MSG "(EOF|IDENT_V|RPAREN) @"; mk T.LPAREN

                | IDENT_V _ when env#in_body_brace_flag && begin
                    match env#stack#top#scope with
                    | Top | Class _ | Enum _ -> true
                    | _ -> false
                end -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

                | IDENT_V _ when begin
                    context == STMT &&
                    match prev_rawtoken2 with
                    | COLON_COLON | HEAD_COLON_COLON -> true
                    | _ ->
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                        match self#peek_nth_rawtoken (nth+1) with
                        | SEMICOLON _ -> false
                        | _ -> true
                end -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

                | IDENT_V _ when env#braced_init_flag -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

                | IDENT _ when context == MEM_INIT && prev_rawtoken2 == COLON_COLON -> DEBUG_MSG "IDENT @"; mk T.LPAREN

                | _ when begin
                    match self#peek_rawtoken() with
                    | IDENT _ | CLASS -> begin
                        match self#peek_nth_rawtoken 2 with
                        | EQ_EQ | EXCLAM_EQ _ -> true
                        | PLUS | MINUS | SLASH | PERC | BAR _ | BAR_BAR _ | LT_EQ | GT_EQ -> true
                        | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> is_literal (self#peek_nth_rawtoken 3)
                        | RPAREN when env#braced_init_flag && begin
                            match self#peek_nth_rawtoken 3 with
                            | COMMA | RPAREN -> begin
                                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) 1;
                                true
                            end
                            | _ -> false
                        end -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "* @"; mk T.LPAREN

                | _ -> DEBUG_MSG "* @"; token
        end

        | _ when followed_by_type() && self#peek_nth_rawtoken 2 != LBRACE -> DEBUG_MSG "@"; token

        | x when is_ty x && begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            match (ll : T.token list list) with
            | [l] -> begin
                (match self#peek_nth_rawtoken (nth+1) with
                | TY_LPAREN | LBRACKET -> true
                | _ -> false) &&
                match List.rev l with
                | x::r when is_ty x && check_if_noptr_dtor_ r -> true
                | x::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::r when is_ty x && check_if_noptr_dtor_ r -> true
                | x::y::r when is_ty x && is_ty y && check_if_noptr_dtor_ r -> true
                | x::y::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::r when is_ty x && is_ty y && check_if_noptr_dtor_ r -> true
                | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::[] -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; token

        | _ -> DEBUG_MSG "@"; mk T.LPAREN
    end

    | (MEM | TOP), END_OF_ID_EXPR when begin
        match prev_rawtoken with
        | IDENT_LM _ | IDENT_EM _ -> true
        | COLON -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    | (MEM | TOP), END_OF_ID_EXPR when begin
        match prev_rawtoken with
        | GNU_ASM _ -> false
        | SIZEOF | IDENT_V _ when env#bracket_level > 0 -> false
        | IDENT_V _ when env#macro_arg_flag || env#templ_arg_flag -> false
        | IDENT_V _ -> begin
            DEBUG_MSG "IDENT_V @";
            begin
              match prev_rawtoken2 with
              | DOT | MINUS_GT | LBRACKET | LPAREN | AMP_AMP _ | BAR_BAR _ -> false
              | PLUS | MINUS | STAR | SLASH | PERC -> false
              | _ -> true
            end && begin
              match self#peek_rawtoken() with
              | EXCLAM _
              | PTR_STAR | PTR_AMP | PTR_AMP_AMP
              | STATIC_CAST | DYNAMIC_CAST | REINTERPRET_CAST | CONST_CAST
              | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
              | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
              | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
              | USER_CHAR_LITERAL _ -> false
              | _ -> true
            end && begin
              let nth, l = self#peek_rawtoken_up_to_rparen_none() in
              match self#peek_nth_rawtoken (nth+1) with
              | PTR_STAR | SLASH | PLUS | MINUS | PERC -> false
              | IDENT s when is_ns_decl_macro_func s || is_ns_block_end_macro s -> false
              | _ -> true
            end
        end
        | IDENT _ when begin
            prev_rawtoken2 == TY_TILDE &&
            match prev_rawtoken3 with
            | DOT | MINUS_GT -> true
            | _ -> false
        end -> false
        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
        | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _
        | CONST | AUTO | IDENT _ -> begin
            match prev_rawtoken2 with
            | QUEST | COLON | LBRACKET | LPAREN -> false
            | _ when begin
                match self#peek_rawtoken() with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 2 with
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 3 with
                        | EQ_EQ | EXCLAM_EQ _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> false
            | _ -> true
        end
        | IDENT_PM _ | IDENT_DSM _ -> false
        | LBRACKET -> begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            (match self#peek_nth_rawtoken (nth+1) with
            | RBRACKET | QUEST | PLUS | MINUS | SLASH | PERC | PTR_AMP_AMP | BAR_BAR _ -> false
            | _ -> true
            ) &&
            match l with
            | IDENT _::x::_ when is_op x -> false
            | (IDENT _ | PTR_STAR | PTR_AMP | PTR_AMP_AMP)::_ -> true
            | _ -> false
        end
        | COMMA when self#peek_rawtoken() == RPAREN -> false
        | COMMA -> begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | PTR_STAR | SLASH | PLUS | MINUS | PERC | QUEST -> false
            | TY_TEMPL_GT -> false
            | _ when begin
                match self#peek_rawtoken() with
                | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
                | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
                | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
                | USER_CHAR_LITERAL _ -> true
                | _ -> false
            end -> false
            | _ -> true
        end
        (*NG!| COMMA -> begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | RPAREN -> false
            | _ -> true
        end*)
        | TEMPL_GT when env#templ_arg_flag && self#peek_rawtoken() == RPAREN -> false
        | TEMPL_GT when not env#last_ty_templ_id_flag && self#peek_rawtoken() == RPAREN -> false
        | AMP _ | BAR _ | EXCLAM _ | AMP_AMP _ | BAR_BAR _ | LT | GT | LT_EQ | GT_EQ
        | EQ_EQ | EXCLAM_EQ _ | PLUS | MINUS | STAR | SLASH | PERC when begin
            match prev_rawtoken2 with
            | IDENT_V _ | COMMA | RPAREN | TEMPL_GT -> true
            | _ -> false
        end -> false
        | RPAREN when env#macro_arg_flag && prev_rawtoken2 == LPAREN -> false
        | RPAREN when
            env#macro_arg_flag &&
            (match prev_rawtoken2 with IDENT_V _ -> true | _ -> false) &&
            prev_rawtoken3 == LPAREN
          -> false
        | NEWLINE when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            List.for_all (function [T.IDENT _] -> true | _ -> false) ll &&
            match self#peek_nth_rawtoken (nth+1) with
            | x when is_ty x -> true
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | x when is_ty x -> true
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | COMMA | SEMICOLON _ -> true
                    | _ -> false
                end
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken (nth+4) with
                        | COMMA | SEMICOLON _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | REGISTER -> true
            | _ -> false
        end -> false
        | _ -> begin
            DEBUG_MSG "@";
            match self#peek_rawtoken() with
            | EXCLAM _
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP
            | STATIC_CAST | DYNAMIC_CAST | REINTERPRET_CAST | CONST_CAST
            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
            | USER_CHAR_LITERAL _ -> false
            | IDENT x when is_str_macro x -> false
            | IDENT _ when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match (ll : T.token list list) with
                | [ELLIPSIS::RPAREN::_] -> true
                | _ -> begin
                    match self#peek_nth_rawtoken (nth+1) with
                    | ELLIPSIS -> true
                    | IDENT x when is_decl_macro x -> true
                    | _ -> false
                end
            end -> false
            | TY_LPAREN when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match (ll : T.token list list) with
                | [x::_] -> is_literal x
                | _ -> false
            end -> false
            | _ -> true
        end
    end -> DEBUG_MSG "@"; token

    | TOP, _ when begin
        match prev_rawtoken with
        | IDENT_V _ | NEWLINE -> begin
            DEBUG_MSG "(IDENT_V|NEWLINE) @";
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
            let len = nth - 1 in
            DEBUG_MSG "len=%d" len;
            if
              let b =
                match self#peek_nth_rawtoken (nth+1) with
                | LBRACE -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | LBRACE -> begin
                        match self#peek_nth_rawtoken (nth+3) with
                        | x when is_literal x -> true
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth+4) with
                            | COMMA | RBRACE -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
              in
              if b then begin
                conv_nth_token (function T.LBRACE,s,e -> T.INI_LBRACE,s,e | x -> x) (nth+1);
                conv_nth_token (function T.LBRACE,s,e -> T.INI_LBRACE,s,e | x -> x) (nth+2);
              end;
              b
            then
              false
            else if len > 0 then begin
              if
                List.for_all
                  (function
                    | [T.IDENT _] -> true
                    | [T.IDENT x; IDENT y] when is_param_decl_macro x || is_param_decl_macro y -> true
                    | _ -> false
                  ) ll
              then begin
                let ll =
                  List.flatten
                    (List.map
                       (function
                         | [x; y]  -> [[y]; [x]]
                         | x -> [x]
                       ) ll)
                in
                let ids =
                  List.map
                    (function
                      | [T.IDENT i] -> i
                      | _ -> assert false
                    ) ll
                in
                (not (List.exists (fun x -> is_type_name x || self#is_type x ) ids) &&
                 self#peek_nth_rawtoken (nth+1) == LBRACE &&
                 match prev_rawtoken2 with
                 | SEMICOLON _ | RBRACE -> false
                 | _ -> true) ||

                (prev_rawtoken == NEWLINE && env#pp_if_section_flag &&
                 match self#peek_nth_rawtoken (nth+1) with
                | IDENT _ when begin
                    let nth', _ = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
                    let nth'' =
                      if self#peek_nth_rawtoken nth' == PP_ENDIF then
                        let n, _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                        n
                      else
                        self#peek_rawtoken_up_to_section_end ~from:(nth'+1) ()
                    in
                    match self#peek_nth_rawtoken (nth''+1) with
                    | LBRACE -> true
                    | _ -> false
                end -> true
                | _ -> false) ||

                let chk_id x = List.mem x ids in
                let conv_ids () =
                  let conv ((rt, sp, ep) as tok) =
                    match rt with
                    | T.IDENT s when is_param_decl_macro s -> tok
                    | T.IDENT s -> T.IDENT_V s, sp, ep
                    | _ -> tok
                  in
                  conv_next_n_tokens conv len;
                  true
                in
                let chk_dtor nth_j rest =
                  match self#peek_nth_rawtoken (nth_j) with
                  | SEMICOLON _ | LBRACE -> false
                  | PTR_STAR -> begin
                      match self#peek_nth_rawtoken (nth_j+1) with
                      | IDENT x when
                          self#peek_nth_rawtoken (nth_j+2) != COLON_COLON && chk_id x
                        -> conv_ids()
                      | PTR_STAR -> begin
                          match self#peek_nth_rawtoken (nth_j+2) with
                          | IDENT x when
                              self#peek_nth_rawtoken (nth_j+3) != COLON_COLON && chk_id x
                            -> conv_ids()
                          | _ -> false
                      end
                      | TY_LPAREN -> begin
                          match self#peek_nth_rawtoken (nth_j+2) with
                          | PTR_STAR -> begin
                              match self#peek_nth_rawtoken (nth_j+3) with
                              | IDENT _ -> begin
                                  match self#peek_nth_rawtoken (nth_j+4) with
                                  | RPAREN -> begin
                                      match self#peek_nth_rawtoken (nth_j+5) with
                                      | TY_LPAREN -> true
                                      | _ -> false
                                  end
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ -> begin
                              match self#peek_nth_rawtoken (nth_j+3) with
                              | PTR_STAR -> begin
                                  match self#peek_nth_rawtoken (nth_j+4) with
                                  | IDENT _ -> true
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | IDENT _ -> begin
                              match self#peek_nth_rawtoken (nth_j+3) with
                              | RPAREN -> begin
                                  match self#peek_nth_rawtoken (nth_j+4) with
                                  | TY_LPAREN -> true
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | _ -> false
                      end
                      | _ -> false
                  end
                  | IDENT x when begin
                      let x_j_1 = self#peek_nth_rawtoken (nth_j+1) in
                      x_j_1 != COLON_COLON && begin
                        match self#peek_nth_rawtoken (nth_j+2) with
                        | EOF when (match x_j_1 with IDENT _ -> true | _ -> false) -> false
                        | _ -> chk_id x
                      end
                  end -> conv_ids()
                  | x -> rest x
                in
                match self#peek_nth_rawtoken (nth+1) with
                | IDENT x when is_type_name x || is_type x -> begin
                    chk_dtor (nth+2)
                      (function
                        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                        | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | AUTO
                        | IDENT _ ->
                            chk_dtor (nth+3)
                              (function
                                | IDENT _ -> chk_dtor (nth+4) (fun _ -> false)
                                | _ -> false
                              )
                        | _ -> false
                      )
                end
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | AUTO
                | IDENT _ | STRUCT | REGISTER -> begin
                    chk_dtor (nth+2)
                      (function
                        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                        | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | AUTO
                        | IDENT _ ->
                            chk_dtor (nth+3)
                              (function
                                | IDENT _ -> chk_dtor (nth+4) (fun _ -> false)
                                | _ -> false
                              )
                        | _ -> false
                      )
                end
                | PP_ELSE | PP_ELIF -> begin
                    let nth' = self#peek_rawtoken_up_to_section_end() in
                    match self#peek_nth_rawtoken (nth'+1) with
                    | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                    | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | AUTO
                    | STRUCT | REGISTER -> begin
                        chk_dtor (nth'+2)
                          (function
                            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                            | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | AUTO
                            | IDENT _ ->
                                chk_dtor (nth'+3)
                                  (function
                                    | IDENT _ -> chk_dtor (nth'+4) (fun _ -> false)
                                    | _ -> false
                                  )
                            | _ -> false
                          )
                    end
                    | IDENT x when is_type_name x || is_type x -> begin
                        chk_dtor (nth'+2)
                          (function
                            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                            | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | AUTO
                            | IDENT _ ->
                                chk_dtor (nth'+3)
                                  (function
                                    | IDENT _ -> chk_dtor (nth'+4) (fun _ -> false)
                                    | _ -> false
                                  )
                            | _ -> false
                          )
                    end
                    | IDENT _ -> chk_dtor (nth'+2) (fun _ -> false)
                    | _ -> false
                end
                | _ -> false
              end
              else
                false
            end
            else
              false
        end
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.PS_LPAREN

    | c, END_OF_ID_EXPR when followed_by_type() -> DEBUG_MSG "@"; token

    | c, END_OF_ID_EXPR -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | IDENT _ -> begin
            match self#peek_rawtoken() with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> DEBUG_MSG "@"; token
            | _ -> DEBUG_MSG "@"; mk T.LPAREN
        end
        | SIZEOF -> begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match l with
            | [x] when is_basic_ty x -> DEBUG_MSG "@"; token
            | [T.IDENT x] when is_type_name x || is_type x -> DEBUG_MSG "@"; token
            | _ -> DEBUG_MSG "@"; mk T.LPAREN
        end
        | _ when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            match ll with
            | (PTR_STAR::_)::_ -> true
            | _ -> false
        end -> DEBUG_MSG "@"; token
        | _ -> DEBUG_MSG "@"; mk T.LPAREN
    end

    | MEM, END_OF_TY_SPEC when begin
        match prev_rawtoken with
        | IDENT_V _ -> begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
            match self#peek_nth_rawtoken (nth+1) with
            | RBRACE -> true
            | _ ->
                List.for_all (fun l -> List.for_all ((!=) T.EQ) l) ll &&
                List.exists
                  (fun l ->
                    filt_at_level0 l
                      (function
                        | T.PLUS | MINUS | SLASH | PERC -> true
                        | _ -> false
                      )
                  ) ll
        end
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    | MEM, END_OF_TY_SPEC when begin
        match self#peek_rawtoken() with
        | IDENT _ -> begin
            match self#peek_nth_rawtoken 2 with
            | RPAREN when begin
                match self#peek_nth_rawtoken 3 with
                | CONST -> true
                | _ -> false
            end -> true
            | RPAREN when begin
                match self#peek_nth_rawtoken 3 with
                | VIRTUAL | TYPEDEF | EXPLICIT | STATIC -> true
                | _ -> false
            end -> false
            | _ -> begin
                match prev_rawtoken with
                | IDENT_V _ when env#templ_param_arg_level = 0 -> true
                | _ -> false
            end
        end
        | _ ->
            match prev_rawtoken with
            | RBRACKET when prev_rawtoken2 == LBRACKET && prev_rawtoken3 == OPERATOR -> true
            | _ -> false
    end -> DEBUG_MSG "@"; token

    | (TOP|MEM), END_OF_DTOR when begin
        match prev_rawtoken with
        | THROW -> true
        | IDENT_V _ -> begin
            match self#peek_rawtoken() with
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
            | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST
            | CLASS | STRUCT | UNION -> true
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "@"; token

    | _, START_OF_STMT _ when begin
        match self#peek_rawtoken() with
        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
        | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ -> true
        | _ -> false
    end -> DEBUG_MSG "@";token

    | _, START_OF_STMT _ -> DEBUG_MSG "@"; mk T.LPAREN

    | TOP, _ when match prev_rawtoken with IDENT_EM _ -> true | _ -> false -> DEBUG_MSG "@"; mk T.LPAREN

    | (TOP|MEM), INI when begin
        match self#peek_rawtoken() with
        | EQ | SEMICOLON _ -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    | TOP, _ when begin
        match prev_rawtoken with
        | IDENT_V _ -> begin
            DEBUG_MSG "IDENT_V @";
            match self#peek_rawtoken() with
            (*| CONST -> false*)
            | x when is_decl_spec x -> false
            | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
            | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
            | USER_CHAR_LITERAL _ -> true
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | DOT | MINUS_GT -> true
                | TY_LPAREN -> begin
                    let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma () in
                    let chk () =
                      not env#end_of_params_flag && self#peek_nth_rawtoken (nth'+1) == LBRACE ||
                      List.exists
                        (fun (l : T.token list) ->
                          DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                          match List.rev l with
                          | x::_ when is_basic_ty x -> true
                          | IDENT x::_ when is_type_name x || is_type x -> true
                          | (CONST|TYPENAME)::_ -> true
                          | [IDENT _; IDENT _] -> true
                          | _ -> false
                        ) ll'
                    in
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | TY_LPAREN | LBRACKET -> false
                    | COMMA | RPAREN when begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | CONST | TYPENAME -> true
                        | _ when chk() -> true
                        | _ -> false
                    end -> false
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | COMMA | RPAREN -> false
                        | _ when chk() -> false
                        | _ -> true
                    end
                    | x when is_ty x -> false
                    | _ when begin
                        match self#peek_nth_rawtoken (nth'+1) with
                        | CONST -> true
                        | _ -> false
                    end -> false
                    | _ -> true
                end
                | _ -> false
            end -> true
            | IDENT _ when begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> true
                | TEMPL_LT when begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | MINUS_GT -> true
                    | _ -> false
                end -> true
                | _ -> false
            end -> false
            | PP_ELIF | PP_ELSE | PP_ENDIF when env#pp_ifx_d_flag -> false
            | x when is_ty x -> false
            | _ -> begin
                DEBUG_MSG "@";
                let is_literal_ = function
                  | [x] -> is_literal x
                  | _ -> false
                in
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
                List.exists (function [] -> true | _ -> false) ll ||
                match prev_rawtoken2 with
                | LBRACE | RBRACE | SEMICOLON _ | NEWLINE | RPAREN -> begin
                    match self#peek_nth_rawtoken (nth+1) with
                    | SEMICOLON _ | LBRACE | COLON | CONST -> List.exists is_literal_ ll
                    | PP_ELSE | PP_ELIF | PP_ENDIF when List.exists is_literal_ ll -> true
                    | PP_ELSE | PP_ELIF | PP_ENDIF -> context == TOP && not env#pp_ifx_d_flag && begin
                        let nth, l = self#peek_rawtoken_up_to ~from:(nth+1) [T.NEWLINE] in
                        match self#peek_nth_rawtoken (nth+1) with
                        | LBRACE -> false
                        | _ -> true
                    end
                    | _ when prev_rawtoken2 == RPAREN && sub_context == END_OF_TY_SPEC -> false
                    | _ -> true
                end
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | AUTO -> false
                | _ -> begin
                    match self#peek_nth_rawtoken (nth+1) with
                    | SLASH | PERC | PLUS | MINUS -> true
                    | SEMICOLON _ -> begin
                        List.exists (function [x] -> is_literal_abort_at_eq x || is_arith x | _ -> false) ll
                    end
                    | _ -> false
                end
            end
        end
        | COMMA when begin
            match self#peek_rawtoken() with
            | x when is_ty x -> true
            | _ -> false
        end -> false
        | COMMA -> begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | SLASH | PERC | PLUS | MINUS -> true
            | _ -> false
        end
        | _ -> false
    end -> begin
      DEBUG_MSG "@";
      begin
        match prev_rawtoken with
        | IDENT_V s -> begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | SEMICOLON _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | IDENT x when x = s && self#peek_nth_rawtoken (nth+3) == TY_LPAREN -> begin
                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth+2);
                    conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth+3)
                end
                | _ -> ()
            end
            | IDENT x when x = s && self#peek_nth_rawtoken (nth+2) == TY_LPAREN -> begin
                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth+1);
                conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth+2)
            end
            | _ -> ()
        end
        | _ -> ()
      end;
      mk T.LPAREN
    end
(*
    | TOP, _ when begin
        match prev_rawtoken with
        | IDENT_V _ -> begin
            begin
              match prev_rawtoken2 with
              | DOT | MINUS_GT | LBRACKET -> false
              | _ -> true
            end && begin
              match self#peek_rawtoken() with
              | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
              | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
              | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
              | USER_CHAR_LITERAL _ -> false
              | _ -> true
            end
        end
        | _ -> false
    end -> token
*)
    | _, END_OF_TY_SPEC when prev_rawtoken == ELLIPSIS && prev_rawtoken2 == SIZEOF -> DEBUG_MSG "@"; mk T.LPAREN

    (*| _, END_OF_TY_SPEC when begin
        followed_by_type() &&
        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
        List.for_all
          (function
            | [x] when is_ty x -> true
            | [T.IDENT x] when is_type_name x || is_type x -> true
            | [T.PTR_STAR;_] -> true
            | [T.PTR_STAR;IDENT _;_] -> true
            | _ -> false
          ) ll
    end -> DEBUG_MSG "@"; token*)

    | _, END_OF_TY_SPEC when begin
        prev_rawtoken != OPERATOR &&
        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
        List.length ll > 0 &&
        check_if_params ll
    end -> DEBUG_MSG "@"; token

    | _, END_OF_TY_SPEC when begin
        match self#peek_rawtoken() with
        | IDENT _ -> begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
            match self#peek_nth_rawtoken (nth+1) with
            | SEMICOLON _ | LBRACE ->
                List.for_all
                  (function
                    | [x] when is_basic_ty x -> true
                    | [T.IDENT _] -> true
                    | [T.PTR_STAR;_] -> true
                    | [T.PTR_STAR;IDENT _;_] -> true
                    | _ -> false
                  ) ll
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "@"; token

    | _ when env#typedef_flag && begin
        match prev_rawtoken with
        | IDENT_V _ -> begin
            let nth, _ = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | PLUS | MINUS | PTR_STAR | SLASH | PERC -> true
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    | _ when begin
        match self#peek_rawtoken() with
        | HAT _ when self#peek_nth_rawtoken 2 != RPAREN -> true
        | _ -> false
    end -> DEBUG_MSG "@"; token

    | _ when begin
        match self#peek_rawtoken() with
        | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
        | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
        | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
        | LBRACE | THIS -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    | _ when begin
        match self#peek_rawtoken() with
        | DYNAMIC_CAST | STATIC_CAST | REINTERPRET_CAST | CONST_CAST -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    | _ when begin
        match self#peek_rawtoken_up_to [T.RPAREN] with
        | nth, [IDENT _] when begin
            match prev_rawtoken with
            | IDENT_V x -> begin
                match prev_rawtoken2 with
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
                | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST
                | CONSTEXPR | CONSTEVAL | CONSTINIT | EXPLICIT | INLINE | AUTO | STATIC -> false
                | CC_MACRO _ -> false
                | IDENT _ -> false
                | COLON_COLON -> false
                | CLASS_LBRACE -> false
                | SEMICOLON _ when context == MEM || context == TOP -> false
                | _ when begin
                    match self#peek_nth_rawtoken (nth+1) with
                    | COLON -> self#reg_macro_fun x; true
                    | _ -> false
                end -> false
                | _ -> self#reg_macro_fun x; true
            end
            | _ -> false
        end -> true
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    (*| _ when begin
        match prev_rawtoken with
        | IDENT_V _ -> begin
            match prev_rawtoken2 with
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
            | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST
            | CONSTEXPR | CONSTEVAL | EXPLICIT | AUTO -> true
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "@"; token*)

    | _ when begin
        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
        ((match prev_rawtoken with IDENT_V _ -> false | _ -> true) || prev_rawtoken2 != COLON ||
         match prev_rawtoken3 with
         | PRIVATE | PUBLIC | PROTECTED -> begin
             match self#peek_nth_rawtoken (nth+1) with
             | COLON -> false
             | _ -> true
         end
         | _ -> true) &&
        match ll with
        | l::_ -> begin
            DEBUG_MSG "@";
            match l with
            | TY_TEMPL_GT::_::TEMPL_LT::IDENT _::[] when begin
                match prev_rawtoken2 with
                | SEMICOLON _ | NEWLINE | LBRACE | RBRACE -> true
                | _ -> false
            end -> true
            | IDENT _::TY_TEMPL_GT::_ -> false
            | IDENT _::IDENT _::_ -> false
            | RPAREN::TY_LPAREN::IDENT _::x::_ when is_basic_ty x -> false
            | RPAREN::IDENT _::IDENT _::_ -> false
            | RPAREN::_::TY_LPAREN::IDENT x::_ when is_suffix_macro_ident x -> false
            | RPAREN::ELLIPSIS::_ -> false
            | RPAREN::_ when begin
                match (List.rev l : T.token list) with
                | IDENT x::TY_LPAREN::_ when is_type_macro_ident x || is_param_decl_macro_ident x -> true
                | IDENT _::IDENT _::IDENT x::_ when is_type_macro_ident x || is_param_decl_macro_ident x -> true
                | _ -> false
            end -> false
            | _ when begin
                match self#peek_nth_rawtoken (nth+1) with
                | LBRACE | COLON -> true
                | _ -> false
            end -> false
            | _ when check_if_params ll -> false
            | _ ->
                (List.memq T.TY_LPAREN l) &&
                not
                  (List.memq T.PTR_STAR l ||
                  List.memq T.PTR_AMP l ||
                  List.memq T.PTR_AMP_AMP l ||
                  List.memq T.DECLTYPE l ||
                  List.memq T.EQ l)
        end
        | _ -> false
    end -> DEBUG_MSG "@"; mk T.LPAREN

    | _ when begin
        let _, l = self#peek_rawtoken_up_to_rparen_none() in
        match l with
        | (BOOL_LITERAL _ | INT_LITERAL _ | FLOAT_LITERAL _ | CHAR_LITERAL _
        | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _
        | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _)::COMMA::_ -> true
        | _ -> false
    end -> begin
      DEBUG_MSG "@";
      self#ctx_expr();
      mk T.LPAREN
    end

    | _ when followed_by_type() && begin
        let _, l = self#peek_rawtoken_up_to_rparen_none() in
        contained_in_list [T.TY_LPAREN;T.COMMA] l
    end -> mk T.LPAREN

    | _, sc -> begin
        DEBUG_MSG "@";
        let is_lparen = ref false in
        match prev_rawtoken with
        | OPERATOR (*| LPAREN*) | IF | SWITCH | WHILE | FOR | COLON
        | IDENT_SM _ | IDENT_EM _ | IDENT_TM _ | IDENT_LM _ -> DEBUG_MSG "* @"; mk T.LPAREN

        | LPAREN when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            (match ll with
            | [[(PTR_STAR|PTR_AMP|PTR_AMP_AMP);IDENT _]] -> true
            | _ -> false) ||
            (List.exists
               (function
                 | [x] -> is_ty x
                 | [T.IDENT _;x] -> is_ty x
                 | _ -> false
               ) ll ||
               List.length ll > 0 &&
               List.for_all
                 (function
                   | [T.IDENT _;IDENT _] -> true
                   | _ -> false
                 ) ll) &&
            match self#peek_nth_rawtoken (nth+1) with
            | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "LPAREN @"; token

        | LPAREN when begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> false
            | _ -> true
        end -> DEBUG_MSG "LPAREN @"; mk T.LPAREN

        | NEW | DELETE | MINUS_GT | MINUS_GT_STAR | TILDE _ | EXCLAM _ | PLUS | MINUS
        | STAR | SLASH | PERC | HAT _ | AMP _ | BAR _ | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ
        | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | EQ_EQ | EXCLAM_EQ _
        | LT | GT | LT_EQ | GT_EQ | LT_EQ_GT | AMP_AMP _ | BAR_BAR _ | LT_LT | GT_GT
        | LT_LT_EQ | GT_GT_EQ | PLUS_PLUS | MINUS_MINUS
        | CO_AWAIT | OP_MACRO _ when begin
            prev_rawtoken2 == OPERATOR ||
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            List.length ll = 1 &&
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> true
            | _ -> false
        end -> DEBUG_MSG "... @"; token

        | NEW | DELETE | MINUS_GT | MINUS_GT_STAR | TILDE _ | EXCLAM _ | PLUS | MINUS
        | STAR | SLASH | PERC | HAT _ | AMP _ | BAR _ | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ
        | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | EQ_EQ | EXCLAM_EQ _
        | LT | GT | LT_EQ | GT_EQ | LT_EQ_GT | AMP_AMP _ | BAR_BAR _ | LT_LT | GT_GT
        | LT_LT_EQ | GT_GT_EQ | PLUS_PLUS | MINUS_MINUS
        | CO_AWAIT | OP_MACRO _  when begin
            prev_rawtoken2 != OPERATOR &&
            not (followed_by_type())
        end -> DEBUG_MSG "... @"; mk T.LPAREN

        | IDENT_V s when begin
            (context == TOP || context == MEM) &&
            sub_context == END_OF_TY_SPEC &&
            match self#peek_nth_rawtoken 1 with
            | PTR_STAR | PTR_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | SEMICOLON _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | SEMICOLON _ -> true
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

        | IDENT_V s when begin
            match prev_rawtoken2 with
            | COLON_COLON when begin
                not env#arg_paren_flag && not env#macro_arg_flag && not env#templ_arg_flag &&
                sub_context != END_OF_TY_SPEC
            end -> begin
              let b = self#is_lparen() in
              if b then
                is_lparen := true;
              not b
            end
            | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL
            | SHORT | INT | LONG | FLOAT | DOUBLE | VOID | TYPE_MACRO _
            | EXPLICIT | CONSTEXPR | CONSTEVAL | CONSTINIT | AUTO
            | IDENT _ -> begin
                let b = self#is_lparen ~ignore_pp:true () in
                if b then
                  is_lparen := true;
                not b
            end
            | TEMPL_GT -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match self#peek_nth_rawtoken (nth+1) with
                | TEMPLATE -> false
                | _ -> true
            end
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match prev_rawtoken3 with
                | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL
                | SHORT | INT | LONG | FLOAT | DOUBLE | VOID | TYPE_MACRO _
                | EXPLICIT | CONSTEXPR | CONSTEVAL | CONSTINIT | AUTO | COLON_COLON -> true
                | IDENT _ -> begin
                    let b = self#is_lparen() in
                    if b then
                      is_lparen := true;
                    not b
                end
                | _ -> false
            end
            | RBRACE(* | SEMICOLON*) when begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                begin
                  match self#peek_nth_rawtoken (nth+1) with
                  | IDENT x when x = s -> true
                  | RBRACE(* | SEMICOLON*) -> is_lparen := true; true
                  | SEMICOLON _ when env#in_body_brace_flag -> is_lparen := true; true
                  | PUBLIC | PRIVATE | PROTECTED -> true
                  | x when is_ty x -> true
                  | _ -> false
                end ||
                List.exists
                  (function
                    | T.RPAREN::_ | [PTR_AMP|PTR_STAR|PTR_AMP_AMP] -> true
                    | _ -> false
                  ) ll
            end -> false
            | RBRACE(* | SEMICOLON*) when context == MEM -> true

            | COLON -> begin
                match self#peek_rawtoken() with
                | EXCLAM _ -> false
                | _ ->
                    match prev_rawtoken3 with
                    | PUBLIC | PROTECTED | PRIVATE -> begin
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                        match self#peek_nth_rawtoken (nth+1) with
                        | RBRACE | TYPEDEF | EXPLICIT | ENUM | CLASS | STRUCT | UNION
                        | PUBLIC | PROTECTED | PRIVATE -> false
                        | IDENT x when is_type_name x -> false
                        | IDENT x when self#peek_nth_rawtoken (nth+2) != TY_LPAREN && is_type ~weak:true x -> false
                        | IDENT _ when self#peek_nth_rawtoken (nth+2) == TY_LPAREN -> false
                        | _ when is_macro_fun s -> false
                        | _ when List.exists (contained_in_list [T.SIZEOF]) ll -> false
                        | _ -> not (check_if_macro_args ll)
                    end
                    | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; token

        | IDENT_V _ when !is_lparen -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

        | IDENT_V s when begin
            is_qt_decl_macro_func s ||
            match prev_rawtoken2 with
            | DECL_MACRO _ when s = "Q_INTERFACES" -> true
            | COMMA when env#templ_arg_flag -> true
            | COLON_COLON when env#templ_arg_flag && begin
                match self#peek_rawtoken() with
                | RPAREN -> begin
                    match self#peek_nth_rawtoken 2 with
                    | COMMA | TY_TEMPL_GT -> true
                    | _ -> false
                end
                | _ -> false
            end -> true
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

        | IDENT_V _ | TEMPL_GT when env#macro_arg_flag && begin
            match self#peek_rawtoken() with
            | IDENT x when is_type_name x -> false
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 3 with
                | RPAREN -> true
                | _ -> false
            end
            | COLON_COLON -> begin
                let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                List.for_all
                  (function
                    | T.COLON_COLON | IDENT _ | COMMA -> true
                    | _ -> false
                  ) l
            end
            | RPAREN when self#peek_nth_rawtoken 2 == LBRACE -> false
            | RPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "(IDENT_V|TEMPL_GT) @"; mk T.LPAREN

        | IDENT_V s when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            match self#peek_nth_rawtoken (nth+1) with
            | EOF when begin
                (context == TOP || context == MEM) && not (check_if_params ~weak:true ll) ||
                context == TOP &&
                match prev_rawtoken2 with
                | RBRACE | SEMICOLON _ -> true
                | _ -> false
            end -> true
            | RBRACE | TYPEDEF | EXPLICIT | STATIC | ENUM | CLASS | STRUCT | UNION
            | PUBLIC | PRIVATE | PROTECTED -> true
            | TILDE _ -> true
            | IDENT x when is_type_name x -> true
            | IDENT x when self#peek_nth_rawtoken (nth+2) != TY_LPAREN && is_type ~weak:true x -> true
            | SEMICOLON _ when begin
                context == TOP &&
                match prev_rawtoken2 with
                | SEMICOLON _ | RBRACE -> true
                | NEWLINE when begin
                    prev_rawtoken3 != PP_ENDIF &&
                    not env#pp_ifx_d_flag &&
                    self#is_lparen()
                end -> true
                | _ -> false
            end -> true
            | COMMA when begin
                match prev_rawtoken2 with
                | TEMPL_LT when env#templ_arg_flag -> true
                | _ -> false
            end -> true
            | PP_ENDIF when begin
                self#peek_nth_rawtoken (nth+2) == NEWLINE &&
                self#peek_nth_rawtoken (nth+3) == EOF &&
                context == TOP &&
                match prev_rawtoken2 with
                | SEMICOLON _ | RBRACE -> true
                | _ -> false
            end -> true
            | PP_DEFINE | PP_UNDEF | PP_LINE | PP_ERROR | PP_UNKNOWN _ | PP_
            | PP_INCLUDE | PP_IMPORT | PP_PRAGMA when begin
                let nth' = skip_pp_control_line ~from:(nth+1) () in
                match self#peek_nth_rawtoken (nth'+1) with
                | EQ | LBRACE -> false
                | _ -> true
            end -> true
            (*| SEMICOLON -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | IDENT x when x = s && self#peek_nth_rawtoken (nth+3) == TY_LPAREN -> begin
                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth+2);
                    conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth+3);
                    true
                end
                | _ -> false
            end*)
            | IDENT x when x = s && self#peek_nth_rawtoken (nth+2) == TY_LPAREN -> begin
                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth+1);
                conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth+2);
                self#reg_macro_fun x;

                let rec conv from =
                  let nth', ll' = self#peek_rawtoken_up_to_rparen_split_at_comma ~from () in
                  match self#peek_nth_rawtoken (nth'+1) with
                  | IDENT x' when x' = s && self#peek_nth_rawtoken (nth'+2) == TY_LPAREN -> begin
                      conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) (nth'+1);
                      conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) (nth'+2);
                      self#reg_macro_fun x';
                      conv (nth'+3)
                  end
                  | _ -> ()
                in
                conv (nth+3);

                true
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

        | RPAREN when env#macro_arg_flag && prev_rawtoken2 == LPAREN -> DEBUG_MSG "RPAREN @"; mk T.LPAREN

        | IDENT _ when begin
            (context == MEM  || context == TOP) &&
            match prev_rawtoken2 with
            | TY_TILDE -> true
            | _ -> false
        end -> DEBUG_MSG "TY_TILDE IDENT @"; token

        | IDENT _ when begin
            match prev_rawtoken2 with
            | MINUS_GT when env#trailing_retty_flag -> false
            | DOT | MINUS_GT | TY_TILDE -> true
            (*| COMMA -> true*)
            | _ when not env#objc_class_interface_flag -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match self#peek_nth_rawtoken (nth+1) with
                | PLUS | MINUS | PTR_STAR | SLASH | PERC -> true
                | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "IDENT @"; mk T.LPAREN

        | IDENT _ when sc == END_OF_TY_SPEC -> DEBUG_MSG "IDENT @"; token

        | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | SIGNED | UNSIGNED | CONST | AUTO(* | TY_TEMPL_GT*) -> begin
            DEBUG_MSG "@";
            match prev_rawtoken2 with
            | EOF | COMMA | LPAREN | TEMPL_LT when begin
                let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                match self#peek_nth_rawtoken (nth+1) with
                | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ | TEMPL_LT
                | BAR _ | PTR_AMP | HAT _ | LT_LT -> false
                | GT_GT when env#templ_param_arg_level < 2 -> false
                | TY_TEMPL_GT when env#templ_param_arg_level = 0 -> false
                | RPAREN when env#decltype_flag || env#templ_arg_flag -> false
                | _ -> true
            end -> DEBUG_MSG "@"; token

            | OPERATOR | UNSIGNED | INLINE | TY_LPAREN -> DEBUG_MSG "@"; token

            | LONG when begin
                match prev_rawtoken3 with
                | OPERATOR | UNSIGNED | INLINE -> true
                | _ -> false
            end -> DEBUG_MSG "@"; token

            | EQ when env#alias_flag -> DEBUG_MSG "@"; token

            | _ when env#decltype_flag -> DEBUG_MSG "@"; mk T.LPAREN

            | _ when sc == END_OF_TY_SPEC && env#conv_func_id_flag -> DEBUG_MSG "@"; token

            | _ ->
                DEBUG_MSG "@";
                let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                match l with
                | [x] when is_basic_ty x -> DEBUG_MSG "@"; token

                | [IDENT _] when begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | RPAREN -> true
                        | _ -> begin
                            let _, nth', l' = self#peek_rawtoken_up_to_rparen ~from:4 None in
                            match self#peek_nth_rawtoken (nth'+1) with
                            | EOF | SEMICOLON _ | LBRACE | CONST -> true
                            | _ -> false
                        end
                    end
                    | _ -> false
                end -> DEBUG_MSG "* @"; token

                | IDENT _::COLON_COLON::rest when begin
                    let rec chk = function
                      | [] -> true
                      | T.IDENT _::COLON_COLON::r -> chk r
                      | _ -> false
                    in
                    chk rest &&
                    match self#peek_nth_rawtoken (nth+1) with
                    | TY_LPAREN -> begin
                        match self#peek_nth_rawtoken (nth+2) with
                        | RPAREN -> true
                        | _ -> begin
                            let _, nth', l' = self#peek_rawtoken_up_to_rparen ~from:(nth+2) None in
                            match self#peek_nth_rawtoken (nth'+1) with
                            | EOF | SEMICOLON _ | LBRACE | CONST -> true
                            | _ -> false
                        end
                    end
                    | _ -> false
                end -> DEBUG_MSG "* @"; token

                | [IDENT _; IDENT _] -> DEBUG_MSG "* @"; token

                | IDENT _::x::_ when is_basic_ty x -> DEBUG_MSG "* @"; token

                | IDENT _::IDENT x::_ when is_type_name x || is_type x -> DEBUG_MSG "* @"; token

                | IDENT _::IDENT _::COLON_COLON::_ -> DEBUG_MSG "* @"; token

                | x::COMMA::_ when is_basic_ty x -> DEBUG_MSG "* @"; token

                | IDENT x::COMMA::_ when is_type_name x || is_type x -> DEBUG_MSG "* @"; token

                | _ -> begin
                    if list_memqn [T.PTR_STAR;PTR_AMP] l then begin
                      DEBUG_MSG "* @";
                      token
                    end
                    else begin
                      DEBUG_MSG "* @";
                      mk T.LPAREN
                    end
                end
        end

        | COMMA | TEMPL_LT when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            match ll with
            | [l] -> begin
                match l with
                | ELLIPSIS::x::_ -> is_fold_op x
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(COMMA|TEMPL_LT) @"; mk T.FOLD_LPAREN

        | COMMA | TEMPL_LT when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            env#templ_arg_flag &&
            match ll with
            | [[x]] when is_basic_ty x -> true
            | [[IDENT x]] when is_type_name x || is_type x -> true
            | [l] -> begin
                match self#peek_nth_rawtoken (nth+1) with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+2) with
                    | TY_TEMPL_GT -> true
                    | GT_GT when env#templ_param_arg_level > 1 -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(COMMA|TEMPL_LT) @"; token

        | COMMA | TEMPL_LT when begin
            env#templ_arg_flag ||
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            List.exists
              (fun l ->
                match (List.rev l) with
                | x::rest -> begin
                    is_basic_ty x &&
                    match rest with
                    | TY_LPAREN::rest2 when
                        List.exists (mem_at_level0 rest2) [T.PTR_STAR;PTR_AMP] -> true
                    | TY_LPAREN::rest2 when list_memqn [T.PTR_STAR;PTR_AMP] rest2 -> false
                    | TY_LPAREN::_ -> true
                    | _ -> false
                end
                | _ -> false
              ) ll
        end -> DEBUG_MSG "(COMMA|TEMPL_LT) @"; mk T.LPAREN

        | ELLIPSIS when prev_rawtoken2 == SIZEOF -> DEBUG_MSG "SIZEOF ELLIPSIS @"; mk T.LPAREN

        | PTR_STAR when self#peek_rawtoken() == RPAREN -> DEBUG_MSG "PTR_STAR @ RPAREN"; token

        | IDENT_V s when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
            DEBUG_MSG "%s" (Token.rawtoken_to_string (self#peek_nth_rawtoken (nth+1)));
            match self#peek_nth_rawtoken (nth+1) with
            | LBRACE | SEMICOLON _ | COLON when check_if_params ~weak:true ll -> false
            | _ ->
                is_macro_fun s ||
                match context, sub_context with
                | TOP, INI when prev_rawtoken2 == RPAREN ->
                    check_if_macro_args ll(* && not (check_if_params ~weak:true ll)*)
                | _ -> false
        end -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

        | RPAREN when prev_rawtoken2 == LPAREN && prev_rawtoken3 == OPERATOR
          -> DEBUG_MSG "OPERATOR LPAREN RPAREN @"; token

        | _ when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
            DEBUG_MSG "|ll|=%d\n%s" (List.length ll)
              (String.concat "\n"
                 (List.map
                    (fun x -> String.concat ";"
                        (List.map Token.rawtoken_to_string x)) ll));
            match (ll : T.token list list) with
            | [[PTR_STAR]] -> true
            | _ when begin
                List.exists
                  (function
                    | [] -> true
                    | x::_ when is_literal x -> true
                    | _ -> false
                  ) ll
            end -> false
            | _ ->
                List.exists
                  (function
                    | [x] when is_ty x -> true
                    | [T.IDENT _;x] when is_ty x -> true
                    | l ->
                        match (List.rev l : T.token list) with
                        | x::y::_ when is_ty x && y != COLON_COLON -> true
                        | _ -> false
                  ) ll
        end -> DEBUG_MSG "* @"; token

        | _ when begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
            DEBUG_MSG "|ll|=%d\n%s" (List.length ll)
              (String.concat "\n"
                 (List.map
                    (fun x -> String.concat ";"
                        (List.map Token.rawtoken_to_string x)) ll));
            List.exists
              (fun l ->
                match (l : T.token list) with
                | x::[] when is_literal x -> true
                | x::(MINUS|PLUS)::EQ::_ when is_literal x -> false
                | x::y::rest when is_literal x && y != EQ && not (list_memqn [T.EQ] rest) -> true
                | [T.IDENT x] when is_val x -> true
                | _ -> false
              ) ll
        end -> DEBUG_MSG "* @"; mk T.LPAREN

        | TEMPL_GT when env#cast_key_flag -> DEBUG_MSG "TEMPL_GT @"; mk T.LPAREN

        | TEMPL_GT when env#templ_arg_flag && begin
            let nth, l = self#peek_rawtoken_up_to_rparen_none() in
            match self#peek_nth_rawtoken (nth+1) with
            | TY_TEMPL_GT | COMMA -> true
            | _ -> false
        end -> DEBUG_MSG "TEMPL_GT @"; mk T.LPAREN

        | TEMPL_GT when env#macro_arg_flag -> DEBUG_MSG "TEMPL_GT @"; mk T.LPAREN

        | RPAREN when prev_rawtoken2 == PTR_STAR && prev_rawtoken3 == TY_LPAREN
          -> DEBUG_MSG "TY_LPAREN PTR_STAR RPAREN @"; token

        | _ -> begin
            DEBUG_MSG "* @";
            match self#peek_rawtoken() with
            | STR_LITERAL _ | PP_STRINGIZED _ | BOOL_LITERAL _ | INT_LITERAL _ | FLOAT_LITERAL _
            | CHAR_LITERAL _ | NULLPTR
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
            | PLUS | MINUS | EXCLAM _ -> DEBUG_MSG "* @ *"; mk T.LPAREN

            | RPAREN when begin
                match self#peek_nth_rawtoken 2 with
                | RPAREN when env#type_paren_flag || env#macro_arg_flag -> false
                | RPAREN | DOT
                | EQ_EQ | EXCLAM_EQ _ | TEMPL_LT | LT_EQ | GT_EQ | LT_LT | PLUS | MINUS | SLASH | PERC -> true
                | TY_TEMPL_GT when env#templ_param_arg_level < 1 -> true
                | GT_GT when env#templ_param_arg_level < 2 -> true
                | MINUS_GT when begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_LPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> true
                | MINUS_GT -> sub_context != END_OF_TY_SPEC
                | TY_LPAREN -> true
                | _ -> false
            end -> DEBUG_MSG "@ RPAREN"; mk T.LPAREN

            | STRUCT when begin
                env#end_of_params_flag &&
                match self#peek_nth_rawtoken 2 with
                | RPAREN -> begin
                    conv_nth_token (function T.STRUCT,s,e -> T.IDENT_V "struct",s,e | x -> x) 1;
                    true
                end
                | _ -> false
            end -> DEBUG_MSG "* @ STRUCT"; mk T.LPAREN

            | IDENT x -> begin
                DEBUG_MSG "* @ IDENT";
                match self#peek_nth_rawtoken 2 with
                | LBRACKET when begin
                    match prev_rawtoken with
                    | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT LBRACKET"; token

                | GT_GT | LT_LT | LBRACKET -> DEBUG_MSG "@ IDENT (GT_GT|LBRACKET)"; mk T.LPAREN

                | PTR_AMP | PTR_STAR when begin
                    match self#peek_nth_rawtoken 3 with
                    | INT_LITERAL _ | FLOAT_LITERAL _ | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> true
                    | SIZEOF -> true
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT (PTR_AMP|PTR_STAR)"; mk T.LPAREN

                | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> DEBUG_MSG "@ IDENT (PTR_AMP|PTR_AMP_AMP|PTR_STAR)"; token

                | HAT _ when begin
                    (context == MEM || context == TOP) && sc == END_OF_TY_SPEC &&
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ | COMMA | RPAREN -> true
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT HAT"; token

                | COMMA -> begin
                    match sc with
                    | END_OF_TY_SPEC -> DEBUG_MSG "@ IDENT COMMA"; token

                    (*| _ when is_type_name x || is_type x || env#type_paren_flag -> DEBUG_MSG "@ IDENT COMMA"; token*)
                    | _ when (is_type_name x || is_type ~weak:true x || env#type_paren_flag) && begin
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma () in
                        (not (check_if_macro_args ll) || check_if_params ~weak:true ll) &&
                        match self#peek_nth_rawtoken (nth+1) with
                        | COLON | SEMICOLON _ | LBRACE -> true
                        | COMMA | RPAREN when env#type_paren_flag -> true
                        | _ -> false
                    end -> DEBUG_MSG "@ IDENT COMMA"; token

                    | _ when env#alias_flag -> DEBUG_MSG "@ IDENT COMMA"; token

                    | _ -> begin
                        DEBUG_MSG "@ IDENT COMMA";
                        self#ctx_expr();
                        mk T.LPAREN
                    end
                end

                | PLUS | MINUS | SLASH | PERC | TY_TEMPL_GT | LT_EQ | GT_EQ
                | DOT | MINUS_GT | BAR _ | HAT _ -> begin
                    DEBUG_MSG "@ IDENT ...";
                    self#ctx_expr();
                    mk T.LPAREN
                end

                | TY_LPAREN when begin
                    match self#peek_nth_rawtoken 3 with
                    | INT_LITERAL _ | USER_INT_LITERAL _ -> true
                    (*| IDENT x when
                        self#peek_nth_rawtoken 4 == RPAREN &&
                        self#peek_nth_rawtoken 5 == COMMA
                      -> true*)
                    | RPAREN -> self#peek_nth_rawtoken 4 == LBRACE
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT TY_LPAREN"; mk T.LPAREN

                | RPAREN when begin
                    match self#peek_nth_rawtoken 3 with
                    | CONST | VOLATILE when begin
                        match self#peek_nth_rawtoken 4 with
                        | SEMICOLON _ | LBRACE -> true
                        | _ -> false
                    end -> false
                    | IDENT x when is_cv_spec_macro x -> false
                    | TY_TEMPL_GT when begin
                        match prev_rawtoken with
                        | CC_MACRO _ -> false
                        | _ -> true
                    end -> true
                    | RBRACE | IDENT _ | INLINE | STATIC | BAR _ | QUEST -> true
                    | PUBLIC | PRIVATE | PROTECTED | USING -> true
                    | ENUM | TYPEDEF | TEMPLATE | CLASS | STRUCT | UNION -> true
                    | PP_IF | PP_IFDEF | PP_IFNDEF when context == TOP || context == MEM -> true
                    | COMMA when begin
                        match prev_rawtoken with
                        | IDENT_V _ -> env#macro_arg_flag
                        | _ -> false
                    end -> true
                    | CONST when begin
                        match self#peek_nth_rawtoken 4 with
                        | LBRACE | THROW | EOF -> true
                        | _ -> false
                    end -> false
                    | x -> is_basic_ty x
                end -> DEBUG_MSG "@ IDENT RPAREN"; mk T.LPAREN

                | RPAREN when begin
                    match prev_rawtoken with
                    | IDENT_V _ -> self#peek_nth_rawtoken 3 == LBRACKET
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT RPAREN"; token

                | TEMPL_LT when begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_TEMPL_GT -> begin
                            match self#peek_nth_rawtoken 5 with
                            | COMMA | RPAREN -> begin
                                match sc with
                                | END_OF_TY_SPEC -> false
                                | _ -> begin
                                    self#ctx_expr();
                                    true
                                end
                            end
                            | TY_LPAREN -> true
                            | _ -> false
                        end
                        | RPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT TEMPL_LT"; mk T.LPAREN

                | COLON_COLON -> begin
                    match self#peek_nth_rawtoken 3 with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken 4 with
                        | PTR_AMP | PTR_AMP_AMP | PTR_STAR when begin
                            match self#peek_nth_rawtoken 5 with
                            | T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
                            | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _
                            | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> false
                            | _ -> true
                        end -> DEBUG_MSG "@"; token
                        | DOT | MINUS_GT | BAR _ | BAR_BAR _ | PLUS | MINUS | SLASH | PERC
                        | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ -> begin
                            DEBUG_MSG "@";
                            self#ctx_expr();
                            mk T.LPAREN
                        end
                        | COMMA -> begin
                            match sc with
                            | END_OF_TY_SPEC when begin
                                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:5 () in
                                not
                                  (List.exists
                                     (fun l ->
                                       match (l : T.token list) with
                                       | [] -> true
                                       | x::_ when is_literal x -> true
                                       | _ ->
                                           match List.rev l with
                                           | (PLUS|MINUS)::_ -> true
                                           | _ -> false
                                     ) ll)
                            end -> DEBUG_MSG "@"; token
                            | _ -> begin
                                DEBUG_MSG "@";
                                self#ctx_expr();
                                mk T.LPAREN
                            end
                        end
                        | COLON_COLON -> begin
                            match self#peek_nth_rawtoken 5 with
                            | IDENT _ -> begin
                                match self#peek_nth_rawtoken 6 with
                                | PTR_AMP | PTR_AMP_AMP | PTR_STAR -> DEBUG_MSG "@"; token
                                | COMMA | DOT | MINUS_GT | BAR _ -> begin
                                    DEBUG_MSG "@";
                                    self#ctx_expr();
                                    mk T.LPAREN
                                end
                                | RPAREN when begin
                                    match self#peek_nth_rawtoken 7 with
                                    | RBRACE -> true
                                    | _ -> false
                                end -> DEBUG_MSG "@"; mk T.LPAREN
                                | COLON_COLON when begin
                                    match self#peek_nth_rawtoken 7 with
                                    | RPAREN -> true
                                    | _ -> false
                                end -> DEBUG_MSG "@"; mk T.LPAREN
                                | _ -> DEBUG_MSG "@"; token
                            end
                            | _ -> DEBUG_MSG "@"; token
                        end
                        | TEMPL_LT when env#templ_arg_flag -> begin
                            let _, l = self#peek_rawtoken_up_to_rparen_none() in
                            match l with
                            | RPAREN::TY_LPAREN::_ -> DEBUG_MSG "@"; mk T.LPAREN
                            | _ -> DEBUG_MSG "@"; token
                        end
                        | RPAREN when begin
                            match self#peek_nth_rawtoken 5 with
                            | PTR_AMP | PTR_AMP_AMP | PTR_STAR when begin
                                match self#peek_nth_rawtoken 6 with
                                | T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
                                | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _
                                | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> false
                                | _ -> true
                            end -> true
                            | DOT | MINUS_GT | BAR _ | BAR_BAR _ | PLUS | MINUS | SLASH | PERC
                            | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ -> true
                            | _ -> false                        
                        end -> DEBUG_MSG "@"; mk T.LPAREN

                        | _ -> begin
                            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                            match self#peek_nth_rawtoken (nth+1) with
                            | PTR_AMP | PTR_AMP_AMP | PTR_STAR when begin
                                match self#peek_nth_rawtoken (nth+2) with
                                | T.INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _
                                | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _
                                | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> false
                                | _ -> true
                            end -> DEBUG_MSG "@"; mk T.LPAREN

                            | MINUS_GT when sub_context == END_OF_TY_SPEC -> DEBUG_MSG "@"; token

                            | DOT | MINUS_GT | BAR _ | BAR_BAR _ | PLUS | MINUS | SLASH | PERC
                            | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ -> DEBUG_MSG "@"; mk T.LPAREN

                            | RPAREN when begin
                                match prev_rawtoken with
                                | IDENT_V _ -> true
                                | _ -> false
                            end -> DEBUG_MSG "IDENT_V @"; mk T.LPAREN

                            | TY_TEMPL_GT when begin
                                match prev_rawtoken with
                                | IDENT_V s -> begin
                                    match self#peek_nth_rawtoken (nth+2) with
                                    | COMMA | RPAREN -> begin
                                        match ll with
                                        | (IDENT x::_)::_ when x = s -> true
                                        | _ -> false
                                    end
                                    | _ -> false
                                end
                                | _ -> false
                            end -> DEBUG_MSG "IDENT_V @ TY_LPAREN ... RPAREN TY_TEMPL_GT"; mk T.LPAREN

                            | _ -> DEBUG_MSG "@"; token
                        end
                    end
                    | RPAREN -> DEBUG_MSG "@ IDENT COLON_COLON RPAREN"; mk T.LPAREN

                    | _ -> DEBUG_MSG "@ IDENT COLON_COLON"; token
                end
                | EQ_EQ | EXCLAM_EQ _ -> DEBUG_MSG "@ IDENT (EQ_EQ|EXCLAM_EQ)"; mk T.LPAREN

                (*| RPAREN when begin
                    (env#arg_paren_flag || env#macro_arg_flag) &&
                    prev_rawtoken == TEMPL_GT &&
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> begin
                        let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:4 () in
                        match self#peek_nth_rawtoken (nth+1) with
                        | RPAREN -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> DEBUG_MSG "TEMPL_GT @ IDENT RPAREN TY_LPAREN ... RPAREN"; mk T.LPAREN*)

                | RPAREN when (env#end_of_cast_type_flag || env#end_of_params_flag) && begin
                    not env#type_paren_flag &&
                    match self#peek_nth_rawtoken 3 with
                    | COMMA | RPAREN -> true
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT RPAREN (COMMA|RPAREN)"; mk T.LPAREN

                | TY_LPAREN when env#end_of_params_flag -> DEBUG_MSG "@ IDENT TY_LPAREN"; mk T.LPAREN

                | TY_LPAREN when begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                    match self#peek_nth_rawtoken (nth+1) with
                    | PLUS | MINUS | PTR_STAR | SLASH | PERC | QUEST
                    | PTR_AMP | BAR _ | PTR_AMP_AMP | BAR_BAR _ -> true
                    | _ -> false
                end -> DEBUG_MSG "@ IDENT TY_LPAREN"; mk T.LPAREN

                | COLON -> DEBUG_MSG "@ IDENT COLON"; mk T.LPAREN

                | _ -> DEBUG_MSG "@"; token
            end

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_LPAREN -> begin
                            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:5 () in
                            check_if_params ll
                        end
                        | LBRACKET when env#type_paren_flag -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; token

            | TY_LPAREN | PTR_STAR | PTR_AMP | PTR_AMP_AMP when not env#typedef_flag && begin
                match prev_rawtoken with
                | RPAREN when env#expr_flag -> true
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP | IDENT _ | TY_TEMPL_GT | RPAREN -> false
                | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ -> false
                | _ -> true
            end -> DEBUG_MSG "@"; mk T.LPAREN

            | LBRACKET | OPERATOR | TILDE _ | NAMESPACE -> DEBUG_MSG "@"; mk T.LPAREN

            | INLINE when begin
                match self#peek_nth_rawtoken 2 with
                | NAMESPACE -> true
                | _ -> false
            end -> DEBUG_MSG "@"; mk T.LPAREN

            | _ -> DEBUG_MSG "* @ *"; token
        end
    end
  in
  let conv_ptr_op () =
    DEBUG_MSG "@";
    let get () =
      match _rawtok with
      | PTR_STAR    -> mk T.STAR
      | PTR_AMP     -> mk (T.AMP "&")
      | PTR_AMP_AMP -> begin
          match self#peek_rawtoken() with
          | PP_IF | PP_IFDEF | PP_IFNDEF when begin
              let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
              let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
              let open_count, close_count = count_parens l in
              DEBUG_MSG "open_count=%d, close_count=%d" open_count close_count;
              close_count - open_count = 1
          end -> mk (T.AMP_AMP_BROKEN "&&")
          | PP_ELIF | PP_ELSE | PP_ENDIF -> mk (T.AMP_AMP_BROKEN "&&")
          | _ -> mk (T.AMP_AMP "&&")
      end
      | _ -> assert false
    in
    match prev_rawtoken with
    | TY_LPAREN | PTR_STAR -> DEBUG_MSG "@"; token
    | OPERATOR | IDENT_V _ | GT_GT | LBRACE | SEMICOLON _ | NEWLINE | LPAREN | COLON
    | INT_LITERAL _ | FLOAT_LITERAL _ | CHAR_LITERAL _ | STR_LITERAL _ | BOOL_LITERAL _
    | NULLPTR | PP_STRINGIZED _ | DELETE | ELSE | BLOCK_HEAD_MACRO _ | LAM_LBRACKET | LBRACKET
    | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
    | EQ_EQ | EXCLAM_EQ _ | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
    | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | AMP_AMP _ | BAR_BAR _ | PLUS_PLUS | MINUS_MINUS
    | GOTO | RETURN | TEMPL_LT | TEMPL_GT
    | QUEST | AMP _ | BAR _ | HAT _ | EXCLAM _ | PLUS | MINUS | STAR | SLASH | PERC
      -> begin
        DEBUG_MSG "... @";
        begin
          match self#peek_rawtoken() with
          | IF | FOR | WHILE | DO | SWITCH | CASE
          | RETURN | GOTO | BREAK | CONTINUE -> begin
              self#prepend_token (mk_ (T.SEMICOLON false));
              self#prepend_token (mk_ T.DUMMY_EXPR)
          end
          | _ -> ()
        end;
        get()
      end

    | RBRACE when begin
        match context, sub_context with
        | STMT, INI -> true
        | _ -> false
    end -> DEBUG_MSG "RBRACE @"; get()

    | RBRACE when begin
        match context, sub_context with
        | EXPR, _
        | _, START_OF_STMT _ -> false
        | _ -> true
    end -> DEBUG_MSG "RBRACE @"; token

    | RBRACKET when prev_rawtoken2 == LBRACKET && prev_rawtoken3 == DELETE -> DEBUG_MSG "RBRCKET @"; get()

    | CONST | VOLATILE when begin
        match prev_rawtoken2 with
        | CONST | VOLATILE -> true
        | _ -> false
    end && prev_rawtoken3 == RPAREN && begin
        match self#peek_rawtoken() with
        | COMMA | TY_TEMPL_GT when env#templ_arg_flag -> true
        | SEMICOLON _ when env#typedef_flag -> true
        | _ -> false
    end -> DEBUG_MSG "(CONST|VOLATILE) @"; get()

    | CONST | VOLATILE when prev_rawtoken2 == RPAREN && begin
        match self#peek_rawtoken() with
        | COMMA | TY_TEMPL_GT when env#templ_arg_flag -> true
        | SEMICOLON _ when env#typedef_flag -> true
        | LBRACE -> true
        | EQ -> begin
            match self#peek_nth_rawtoken 2 with
            | DELETE | DEFAULT -> true
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "(CONST|VOLATILE) @"; get()

    | RPAREN | CONST when begin
        match self#peek_rawtoken() with
        | SEMICOLON _ | LBRACE -> true
        | _ -> false
    end -> DEBUG_MSG "(RPAREN|CONST) @"; get()

    | CONST when begin
        env#end_of_params_flag &&
        (not env#type_paren_flag || prev_rawtoken2 == RPAREN) &&
        not env#templ_arg_flag
    end -> DEBUG_MSG "CONST @"; get()

    | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | FLOAT | SIGNED | UNSIGNED
    | DOUBLE | VOID | TYPE_MACRO _ | AUTO | CONST | VOLATILE | IDENT _ | TY_TEMPL_GT when begin
        self#peek_rawtoken() == LBRACKET &&
        match self#peek_nth_rawtoken 2 with
        | IDENT _ -> begin
            match self#peek_nth_rawtoken 3 with
            | COMMA -> true
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "(CHAR|...) @"; get()

    | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | FLOAT | SIGNED | UNSIGNED
    | DOUBLE | VOID | TYPE_MACRO _ | AUTO | CONST | VOLATILE | IDENT _ | TY_TEMPL_GT -> begin
        DEBUG_MSG "(CHAR|...) @";
        begin
          match context, sub_context with
          | MEM, END_OF_TY_SPEC -> begin
              match self#peek_rawtoken() with
              | TY_LPAREN -> begin
                  match self#peek_nth_rawtoken 2 with
                  | RPAREN -> begin
                      match self#peek_nth_rawtoken 3 with
                      | SEMICOLON _ -> self#prepend_token (mk T.DUMMY_DTOR)
                      | _ -> ()
                  end
                  | _ -> ()
              end
              | _ -> ()
          end
          | _ -> ()
        end;
        token
    end

    | COMMA when self#peek_rawtoken() == COMMA -> DEBUG_MSG "COMMA @"; get()

    | COMMA when begin
        match prev_rawtoken2 with
        | IDENT_V _ -> begin
            match prev_rawtoken3 with
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "COMMA @"; token

    | COMMA when begin
        not env#old_param_decl_flag &&
        match self#peek_rawtoken() with
        | IDENT _ -> begin
            match self#peek_nth_rawtoken 2 with
            | COMMA when sub_context == END_OF_TY_SPEC -> false
            | COMMA | TY_TEMPL_GT -> true
            | COLON_COLON -> begin
                match self#peek_nth_rawtoken 3 with
                | OPERATOR -> true
                | IDENT _ -> true
                | _ -> false
            end
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "COMMA @"; get()

    | COMMA when self#peek_rawtoken() = THIS -> DEBUG_MSG "COMMA @"; get()

    | RPAREN when begin
        match sub_context with
        | START_OF_STMT _ -> true
        | _ when match self#peek_rawtoken() with TILDE _ -> true | _ -> false -> true
        | _ -> false
    end -> DEBUG_MSG "RPAREN @"; get()

    | RPAREN when env#type_paren_flag && self#peek_rawtoken() == COMMA -> DEBUG_MSG "RPAREN @"; token

    | RPAREN when begin
        match self#peek_rawtoken() with
        | RPAREN | COMMA when not env#end_of_params_flag -> true
        | IDENT _ -> begin
            match self#peek_nth_rawtoken 2 with
            | EQ -> begin
                match sub_context with
                | START_OF_STMT _ -> false
                | INI when env#decl_stmt_block_flag -> false
                | _ when prev_rawtoken2 == PTR_STAR -> false
                | _ -> true
            end
            | COMMA when env#type_paren_flag -> true
            | SEMICOLON _ when env#end_of_decltype_flag -> true
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "RPAREN @"; token

    | RPAREN when begin
        match prev_rawtoken2 with
        | INT_LITERAL _ | FLOAT_LITERAL _ | STR_LITERAL _ | CHAR_LITERAL _ | BOOL_LITERAL _
        | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _
        | USER_STR_LITERAL _ | USER_CHAR_LITERAL _(* | IDENT_V _*) | ELLIPSIS -> true
        | IDENT_V _ when env#expr_flag -> true
        | IDENT_V _ when env#templ_arg_flag && begin
            match self#peek_rawtoken() with
            | IDENT _ -> true
            | _ -> false
        end -> true
        | _ -> false
    end -> DEBUG_MSG "RPAREN @"; get()

    | RPAREN when begin
        env#end_of_sizeof_flag ||
        match self#peek_rawtoken() with
        | TY_LPAREN | LBRACE | SIZEOF -> true
        | DYNAMIC_CAST | STATIC_CAST | REINTERPRET_CAST | CONST_CAST -> true
        | COMMA when env#templ_arg_flag -> true
        | SEMICOLON _ when env#typedef_flag -> true
        | IDENT _ -> begin
            match self#peek_nth_rawtoken 2 with
            | RPAREN -> begin
                match self#peek_nth_rawtoken 3 with
                | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
                | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
                | PTR_AMP_AMP | AMP_AMP _ | BAR_BAR _ | PLUS_PLUS | MINUS_MINUS
                | PLUS | MINUS | PTR_STAR | SLASH | PERC -> true
                | _ -> false
            end
            | TY_LPAREN when env#templ_arg_flag || env#arg_paren_flag -> true
            | EQ -> begin
                match sub_context with
                | START_OF_STMT _ -> true
                | INI when env#decl_stmt_block_flag -> true
                | _ -> false
            end
            | _ -> false
        end
        | _ -> false
    end -> DEBUG_MSG "RPAREN @"; get()

    | ATTR_MACRO _ when begin
        match prev_rawtoken2 with
        | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | FLOAT | DOUBLE | VOID | AUTO | CONST | VOLATILE | IDENT _ | TYPE_MACRO _ | TY_TEMPL_GT -> true
        | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
        | _ -> false
    end -> DEBUG_MSG "ATTR_MACRO @"; token

    | DECL_MACRO _ -> DEBUG_MSG "DECL_MACRO @"; get()

    | _ when match sub_context with START_OF_STMT _ -> true | _ -> false -> DEBUG_MSG "* @"; get()

    | _ when begin
        match self#peek_rawtoken() with
        | BOOL_LITERAL _
        | INT_LITERAL _ | FLOAT_LITERAL _ | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> true
        | _ -> false
    end -> DEBUG_MSG "* @ *"; get()

    | _ when env#macro_arg_flag && env#end_of_decltype_flag -> DEBUG_MSG "* @"; token

    | _ when env#macro_arg_flag && match prev_rawtoken with CC_MACRO _ -> false | _ -> true -> DEBUG_MSG "* @"; get()

    | _ -> begin
        DEBUG_MSG "* @";
        match context, sub_context with
        | EXPR, _ | NEW, _ | _, END_OF_DTOR when not env#type_paren_flag -> begin
            match self#peek_rawtoken() with
            | TY_TEMPL_GT | COMMA -> DEBUG_MSG "* @ TY_LPAREN"; token
            | _ when begin
                not env#braced_init_flag &&
                prev_rawtoken == COMMA &&
                sub_context == END_OF_TY_SPEC &&
                env#paren_level = 0
            end -> DEBUG_MSG "* @ * "; token
            | _ -> DEBUG_MSG "* @ *"; get()
        end
        | _ when begin
            not env#dtor_flag &&
            not env#old_param_decl_flag &&
            prev_rawtoken == COMMA &&
            match self#peek_rawtoken() with
            | IDENT _ -> true
            | _ -> false
        end -> DEBUG_MSG "* @ *"; get()

        | _ when env#expr_flag -> DEBUG_MSG "* @ *"; get()

        | _ -> DEBUG_MSG "* @ *"; token
    end
  in
  let conv_lbrace () =
    DEBUG_MSG "@";
    match context with
    | CLASS -> begin
        match self#peek_rawtoken() with
        | RBRACE -> begin
            DEBUG_MSG "@";
            match self#peek_nth_rawtoken 2 with
            | PLUS | MINUS | SLASH -> mk T.INI_LBRACE
            | PTR_STAR when not env#base_clause_flag -> mk T.INI_LBRACE
            | _ -> begin
                DEBUG_MSG "@";
                self#ctx_top();
                env#clear_class_name_flag();
                mk T.CLASS_LBRACE
            end
        end
        | _ -> begin
            DEBUG_MSG "@";
            self#ctx_top();
            env#clear_class_name_flag();
            mk T.CLASS_LBRACE
        end
    end
    | MEM_INIT when begin
        match prev_rawtoken with
        | NEWLINE -> begin
            match prev_rawtoken2 with
            | PP_ENDIF -> begin
                match prev_rawtoken3 with
                | RPAREN | RBRACE -> false
                | NEWLINE -> begin
                    match prev_rawtoken4 with
                    | PP_ENDIF | PP_ELSE -> false
                    | _ -> true
                end
                | _ -> true
            end
            | _ -> true
        end
        | _ -> true
    end -> DEBUG_MSG "@"; mk T.INI_LBRACE

    | STMT | EXPR -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | CLASS | STRUCT | UNION -> DEBUG_MSG "@"; mk T.CLASS_LBRACE

        | TY_TEMPL_GT when env#trailing_retty_flag && env#paren_level = 0 -> DEBUG_MSG "@"; token

        | COMMA when not env#macro_arg_flag -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | INI_LBRACE | EQ | IDENT_V _ | RETURN -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | NEWLINE when env#braced_init_flag -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | RBRACKET when sub_context != END_OF_LAM_INTRO -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | RPAREN when env#end_of_cast_type_flag -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | TY_TEMPL_GT when begin
            not env#lambda_dtor_flag &&
            match self#peek_rawtoken() with
            | RETURN -> false
            | _ -> true
        end -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | LPAREN when begin
            match prev_rawtoken2 with
            | IDENT_V _ | IDENT _ | TEMPL_GT | TY_TEMPL_GT -> true
            | _ -> false
        end -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | COLON when env#for_range_init_flag -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | IDENT _ when begin
            match prev_rawtoken2 with
            | LPAREN | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
            | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | RETURN | COMMA | INI_LBRACE -> true
            | COLON_COLON when not env#trailing_retty_flag -> true
            | _ -> false
        end -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | IDENT _ when begin
            match prev_rawtoken2 with
            | MINUS_GT -> true
            | _ when env#trailing_retty_flag -> true
            | _ -> false
        end -> DEBUG_MSG "@"; token

        | x when begin
            not env#lambda_dtor_flag && is_ty x &&
            prev_rawtoken2 != COLON &&
            match x with
            | CONST | VOLATILE -> false
            | _ -> true
        end -> DEBUG_MSG "@"; mk T.INI_LBRACE

        | _ -> begin
            DEBUG_MSG "* @";
            match self#peek_rawtoken() with
            | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
            | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
            | USER_CHAR_LITERAL _ | MINUS | DOT | LBRACKET when begin
                match sub_context with
                | START_OF_STMT _ -> begin
                    let filt = function
                      | T.SEMICOLON _ -> true
                      | _ -> false
                    in
                    try
                      let _ = self#peek_rawtoken_up_to_rbrace ~filt () in
                      true
                    with
                      _ -> false
                end
                | _ -> true
            end -> DEBUG_MSG "@ (STR_LITERAL|...)"; mk T.INI_LBRACE

            | RBRACE when self#peek_nth_rawtoken 2 == DOT -> DEBUG_MSG "@ RBRACE"; mk T.INI_LBRACE

            | IDENT _ | RBRACE -> begin
                match prev_rawtoken with
                | IDENT(*_V*) _ when not env#enum_head_flag -> DEBUG_MSG "IDENT @ (IDENT|RBRACE)"; mk T.INI_LBRACE

                | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
                  -> DEBUG_MSG "(PLUS_EQ|...) @"; mk T.INI_LBRACE

                | RPAREN when env#end_of_decltype_flag && env#in_body_brace_flag
                  -> DEBUG_MSG "RPAREN @"; mk T.INI_LBRACE

                | COMMA when env#braced_init_flag -> DEBUG_MSG "RPAREN @"; mk T.INI_LBRACE

                | _ -> begin
                    match self#peek_nth_rawtoken 2 with
                    | COMMA when prev_rawtoken == EOF -> DEBUG_MSG "* @ (IDENT|RBRACE) COMMA"; mk T.INI_LBRACE
                    | COMMA -> begin
                        match self#peek_nth_rawtoken 3 with
                        | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
                        | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
                        | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
                        | USER_CHAR_LITERAL _ when not env#lambda_dtor_flag
                          -> DEBUG_MSG "* @ (IDENT|RBRACE) COMMA ..."; mk T.INI_LBRACE

                        | _ when env#braced_init_flag && not env#lambda_dtor_flag || prev_rawtoken == LBRACKET
                          -> DEBUG_MSG "* @ (IDENT|RBRACE) COMMA *"; mk T.INI_LBRACE

                        | _ -> DEBUG_MSG "* @ (IDENT|RBRACE) COMMA *"; token
                    end
                    | _ when begin
                        env#pp_if_section_flag && env#expr_flag && env#in_body_brace_flag &&
                        not env#lambda_dtor_flag &&
                        match prev_rawtoken with
                        | RPAREN when env#paren_level = 1 -> true
                        | _ -> false
                    end -> begin
                      DEBUG_MSG "* @ (IDENT|RBRACE)";
                      DEBUG_MSG "lack of closing parentheses";
                      self#prepend_token token;
                      mk T.RPAREN
                    end

                    | _ -> DEBUG_MSG "* @ (IDENT|RBRACE)"; token
                end
            end

            | LBRACE -> begin
                match self#peek_nth_rawtoken 2 with
                | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
                | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
                | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
                | USER_CHAR_LITERAL _ -> DEBUG_MSG "@"; mk T.INI_LBRACE
                | _ -> DEBUG_MSG "@ LBRACE"; token
            end

            | NEW when env#braced_init_flag -> DEBUG_MSG "@ NEW"; mk T.INI_LBRACE

            | NEW when env#in_body_brace_flag && begin
                let filt = function
                  | T.SEMICOLON _ -> true
                  | _ -> false
                in
                try
                  let nth, _ = self#peek_rawtoken_up_to_rbrace ~filt () in
                  match self#peek_nth_rawtoken (nth+1) with
                  | DOT | MINUS_GT -> true
                  | _ -> false
                with
                  _ -> false
            end -> DEBUG_MSG "@ NEW"; mk T.INI_LBRACE

            | _ when prev_rawtoken == NEWLINE && begin
                match prev_rawtoken2 with
                | INT_LITERAL "0" -> prev_rawtoken3 == PP_IF && self#peek_rawtoken() == PP_ENDIF
                | _ -> false
            end -> begin
              DEBUG_MSG "@ *";
              mk T.ODD_LBRACE
            end

            | _ when env#expr_flag && env#paren_level = 1 && prev_rawtoken == RPAREN
              -> begin
                DEBUG_MSG "RPAREN @";
                DEBUG_MSG "lack of closing parenthesis";
                self#prepend_token token;
                mk T.RPAREN
              end

            | _ -> DEBUG_MSG "@ *"; token
        end
    end
    | _ -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | IDENT_V _ | COMMA | LPAREN when not env#macro_arg_flag
          -> DEBUG_MSG "(IDENT_V|COMMA|LPAREN) @"; mk T.INI_LBRACE

        | EQ when prev_rawtoken2 != MARKER -> DEBUG_MSG "EQ @"; mk T.INI_LBRACE

        | TY_TEMPL_GT when context == NEW -> DEBUG_MSG "TY_TEMPL_GT @"; mk T.INI_LBRACE

        | TY_TEMPL_GT when begin
            (env#arg_paren_flag || env#macro_arg_flag) &&
            match self#peek_nth_rawtoken 1 with
            | RBRACE -> begin
                match self#peek_nth_rawtoken 2 with
                | COMMA | RPAREN -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "TEMPL_GT @"; mk T.INI_LBRACE

        | RBRACKET when begin
            match prev_rawtoken2 with
            | LBRACKET -> begin
                match prev_rawtoken3 with
                | IDENT_V _ -> true
                | _ -> false
            end
            | INT_LITERAL _ | IDENT_V _ -> begin
                match prev_rawtoken3 with
                | LBRACKET -> begin
                    match prev_rawtoken4 with
                    | IDENT_V _ -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "RBRACKET @"; mk T.INI_LBRACE

        | ENUM -> DEBUG_MSG "ENUM @"; token

        | IDENT _ when begin
            match prev_rawtoken2 with
            | NEW -> true
            | _ when context == NEW -> true
            | _ -> false
        end -> DEBUG_MSG "IDENT @"; mk T.INI_LBRACE

        | x when is_ty x && begin
            match prev_rawtoken2 with
            | NEW -> true
            | _ when context == NEW -> true
            | _ -> false
        end -> DEBUG_MSG "IDENT @"; mk T.INI_LBRACE

        | DECL_MACRO _ when begin
            match self#peek_rawtoken() with
            | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
            | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
            | USER_CHAR_LITERAL _ | MINUS | DOT | LBRACKET -> begin
                let filt = function
                  | T.SEMICOLON _ -> true
                  | _ -> false
                in
                try
                  let _ = self#peek_rawtoken_up_to_rbrace ~filt () in
                  true
                with
                  _ -> false
            end
            | IDENT _ -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_LPAREN -> begin
                    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | COMMA -> true
                    | _ -> false
                end
                | _ ->
                    let _, l = self#peek_rawtoken_up_to_rbrace() in
                    match (l : T.token list) with
                    | (STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
                    | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
                    | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
                    | USER_CHAR_LITERAL _)::COMMA::_ -> true
                    | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "DECL_MACRO @"; mk T.INI_LBRACE

        | _ -> begin
            DEBUG_MSG "* @";
            match self#peek_rawtoken() with
            | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
            | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
            | USER_CHAR_LITERAL _ | MINUS | DOT | LBRACKET when begin
                let filt = function
                  | T.SEMICOLON _ -> true
                  | _ -> false
                in
                try
                  let _ = self#peek_rawtoken_up_to_rbrace ~filt () in
                  true
                with
                  _ -> false
            end -> DEBUG_MSG "@"; mk T.INI_LBRACE

            | _ -> DEBUG_MSG "@"; token
        end
    end
  in
  let conv_ty_templ_gt () =
    DEBUG_MSG "@";

    match self#peek_rawtoken() with
    | TY_LPAREN_ -> DEBUG_MSG "@ TY_LPAREN_"; token
    | TY_TEMPL_GT -> begin
      if
        not env#init_flag &&
        ((env#templ_head_flag && not env#templ_arg_flag) ||
        (env#templ_arg_flag && env#ty_templ_id_flag))
      then begin
        DEBUG_MSG "@";
        token
      end
      else if env#expr_flag then begin
        DEBUG_MSG "@";
        mk T.TEMPL_GT
      end
      else if
        context == TY(* || env#const_flag*) ||
        (env#templ_param_arg_level = 2 && env#cast_key_flag)
      then begin
        DEBUG_MSG "@";
        token
      end
      else begin
        DEBUG_MSG "@";
        mk T.TEMPL_GT
      end
    end
    | _ ->

    match context, sub_context with
    | EXPR, sc -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | IDENT_V _ | RBRACKET | RPAREN when env#templ_param_arg_level = 0
          -> DEBUG_MSG "(IDENT_V|RBRACKET|RPAREN) @"; mk T.GT

        | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
        | FLOAT | DOUBLE | VOID | AUTO | CONST | VOLATILE | TYPE_MACRO _
        | TY_TEMPL_GT when env#templ_param_arg_level > 0 && begin
            match self#peek_rawtoken() with
            | LBRACKET -> begin
                match self#peek_nth_rawtoken 2 with
                | RBRACKET -> true
                | _ -> false
            end
            | _ -> false              
        end -> DEBUG_MSG "... @"; token

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP | IDENT _ when begin
            env#templ_arg_flag && env#lambda_dtor_flag && env#type_paren_flag
        end -> DEBUG_MSG "... @"; token

        | BOOL_LITERAL _ when env#templ_head_flag && env#templ_head_lv = env#templ_param_arg_level
          -> begin
            DEBUG_MSG "BOOL_LITERAL @";
            env#exit_templ_head();
            mk T.TEMPL_GT
          end

        | _ when begin
            env#templ_param_arg_level > 0 &&
            env#cast_key_flag &&
            match prev_rawtoken2 with
            | TEMPL_LT -> begin
                match prev_rawtoken3 with
                | DYNAMIC_CAST | STATIC_CAST | REINTERPRET_CAST | CONST_CAST -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "* @"; mk T.TEMPL_GT

        | _ when begin
            env#templ_arg_flag && env#templ_param_arg_level = 1 && env#conv_func_id_flag
        end -> DEBUG_MSG "* @"; token

        | _ -> begin
            DEBUG_MSG "* @";
            match self#peek_rawtoken() with
            | RBRACE when env#templ_param_arg_level > 0 -> DEBUG_MSG "* @ RBRACE"; mk T.TEMPL_GT

            | LBRACE when env#templ_param_arg_level > 0 -> DEBUG_MSG "* @ LBRACE"; token

            | COLON -> DEBUG_MSG "* @ COLON"; mk T.TEMPL_GT

            | SEMICOLON _ when env#using_flag && env#alias_flag -> DEBUG_MSG "* @ SEMICOLON"; token

            | SEMICOLON _ when
                env#templ_arg_flag && not env#last_ty_templ_id_flag -> DEBUG_MSG "* @ SEMICOLON"; mk T.TEMPL_GT

            | CLASS | STRUCT | UNION | DECLTYPE | INLINE | OPERATOR | AUTO when begin
                env#templ_head_flag && env#templ_param_arg_level > 0
            end -> begin
              DEBUG_MSG "@";
              env#exit_templ_head();
              mk T.TEMPL_GT
            end

            | COMMA when env#typename_flag && env#templ_param_arg_level > 0 && begin
                match self#peek_nth_rawtoken 2 with
                | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
                | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _ | USER_INT_LITERAL _
                | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _ | USER_CHAR_LITERAL _ -> true
                | _ -> false
            end -> DEBUG_MSG "* @ COMMA"; mk T.TEMPL_GT

            | COMMA when env#typename_flag && env#templ_param_arg_level > 0 -> DEBUG_MSG "* @ COMMA"; token

            | COMMA when env#type_paren_flag -> DEBUG_MSG "* @ COMMA"; token

            (*| COMMA when
                env#templ_param_arg_level > 1 &&
                env#templ_arg_flag
              -> DEBUG_MSG "* @ COMMA"; token*)

            | TY_LPAREN when begin
                env#templ_arg_flag && env#ty_templ_id_flag && not env#cast_key_flag
            end -> DEBUG_MSG "* @ TY_LPAREN"; token

            | TY_LPAREN when begin
                env#templ_arg_flag &&
                (env#cast_key_flag || env#alias_flag) &&
                (not env#cast_key_flag || env#templ_param_arg_level > 1) &&
                match self#peek_nth_rawtoken 2 with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                    conv_nth_token (function T.TY_LPAREN,s,e -> T.TY_LPAREN_,s,e | x -> x) 1;
                    true
                end
                | _ -> false
            end -> DEBUG_MSG "* @ TY_LPAREN"; token

            | TY_LPAREN when self#peek_nth_rawtoken 2 == RPAREN -> DEBUG_MSG "* @ TY_LPAREN"; mk T.TEMPL_GT

            | TY_LPAREN | COLON_COLON | COMMA when
                env#templ_param_arg_level > 0 -> DEBUG_MSG "* @ TY_LPAREN"; mk T.TEMPL_GT

            | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
            | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | UNSIGNED | SIGNED | STATIC
            | CONSTEXPR | CONSTEVAL | CONSTINIT -> begin
                if
                  env#templ_head_flag && env#templ_head_lv = env#templ_param_arg_level
                then begin
                  DEBUG_MSG "@";
                  env#exit_templ_head();
                  mk T.TEMPL_GT
                end
                else begin
                  DEBUG_MSG "@"; 
                  token
                end
            end

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | TY_TEMPL_GT | COMMA | ELLIPSIS | CONST -> DEBUG_MSG "@"; token

                | IDENT _ when begin
                    match self#peek_nth_rawtoken 3 with
                    | EQ -> true
                    | COMMA when env#templ_param_arg_level > 0 -> true
                    | _ -> false
                end -> DEBUG_MSG "@"; token

                | _ when env#templ_param_arg_level > 0 && env#type_paren_flag -> DEBUG_MSG "@"; token

                | _ when env#templ_param_arg_level > 0 -> DEBUG_MSG "@"; mk T.TEMPL_GT

                | _ -> DEBUG_MSG "@"; mk T.GT
            end

            | IDENT _ when sc == IN_SIMPLE_TEMPL_ID && env#templ_param_arg_level > 0 -> DEBUG_MSG "@ IDENT"; token

            | IDENT _ when env#typedef_flag && env#templ_param_arg_level = 1 -> DEBUG_MSG "@ IDENT"; token

            | IDENT _ when begin
                env#templ_head_flag && env#templ_param_arg_level = 1 &&
                (not env#init_flag || env#paren_level = 0)
            end -> begin
              DEBUG_MSG "@ IDENT";
              self#ctx_ini();
              env#exit_templ_head();
              mk T.TEMPL_GT
            end

            | IDENT _ when env#templ_arg_flag && begin(* * @ IDENT *)
                env#templ_param_arg_stack_top = env#paren_level &&
                match self#peek_nth_rawtoken 2 with
                | IDENT _ | TY_LPAREN | EQ | COMMA | SEMICOLON _ | LBRACKET -> true
                | COLON_COLON -> true
                | RPAREN when env#type_paren_flag && (env#paren_level = 1 || env#lambda_dtor_flag) -> true
                | _ -> false
            end -> DEBUG_MSG "@ IDENT"; token

            | RPAREN -> begin
                if env#templ_param_arg_level = 1 && env#init_flag then begin
                  DEBUG_MSG "@ RPAREN"; 
                  mk T.TEMPL_GT
                end
                else if env#sizeof_ty_flag || env#type_paren_flag then begin
                  DEBUG_MSG "@ RPAREN";
                  token
                end
                else if prev_rawtoken == COMMA then begin
                  DEBUG_MSG "@ RPAREN"; 
                  mk T.GT
                end
                else begin
                  DEBUG_MSG "@ RPAREN"; 
                  mk T.TEMPL_GT
                end
            end

            | TY_TEMPL_GT when
                env#cast_key_flag && env#templ_param_arg_level = 2 &&
                self#peek_nth_rawtoken 2 == TY_LPAREN
              -> DEBUG_MSG "@ TY_TEMPL_GT"; token

            | TY_TEMPL_GT when env#templ_arg_flag && env#ty_templ_id_flag -> DEBUG_MSG "@ TY_TEMPL_GT"; token

            | TY_TEMPL_GT -> DEBUG_MSG "@ TY_TEMPL_GT"; mk T.TEMPL_GT

            | GT_GT -> DEBUG_MSG "@ GT_GT"; mk T.TEMPL_GT

            | ELLIPSIS when begin
                match self#peek_nth_rawtoken 2 with
                | RBRACE -> true
                | RPAREN when not env#type_paren_flag -> true
                | _ -> false
            end -> DEBUG_MSG "@ ELLIPSIS"; mk T.TEMPL_GT

            | ELLIPSIS when begin
                env#templ_arg_flag &&
                match self#peek_nth_rawtoken 2 with
                | TY_TEMPL_GT -> true
                | _ -> false
            end -> DEBUG_MSG "@ ELLIPSIS"; token

            | EOF -> DEBUG_MSG "@ EOF"; token

            | OPERATOR when env#templ_arg_flag -> DEBUG_MSG "* @ OPERATOR"; token

            | EQ_EQ | EXCLAM_EQ _ | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | MINUS_GT | DOT
            | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | AMP_AMP _ | BAR_BAR _ | PLUS_PLUS | MINUS_MINUS
            | PLUS | MINUS | STAR | SLASH | PERC when env#templ_arg_flag -> DEBUG_MSG "* @ ..."; mk T.TEMPL_GT

            | QUEST | SEMICOLON _ when env#templ_arg_flag -> DEBUG_MSG "* @ (QUEST|SEMICOLON)"; mk T.TEMPL_GT

            | EXPLICIT | USING when env#templ_head_flag && env#templ_head_lv = env#templ_param_arg_level -> begin
                DEBUG_MSG "* @ (EXPLICIT|USING)";
                env#exit_templ_head();
                mk T.TEMPL_GT
            end

            | LPAREN when env#end_of_decltype_flag -> DEBUG_MSG "* @ LPAREN"; mk T.TEMPL_GT

            | HAT _ -> DEBUG_MSG "* @ HAT"; token

            | rt -> DEBUG_MSG "* @ %s" (Token.rawtoken_to_string rt); mk T.GT
        end
    end

    | TY, _ when self#peek_rawtoken() == COLON_COLON -> DEBUG_MSG "@ COLON_COLON"; mk T.TEMPL_GT

    | TY, _ when
        env#paren_level = 1 && env#arg_paren_flag && self#peek_rawtoken() == RPAREN
      -> DEBUG_MSG "@"; mk T.TEMPL_GT

    | TY, _ -> DEBUG_MSG "@"; token

    | NEW, _ -> begin
        DEBUG_MSG "@";
        match self#peek_rawtoken() with
        | COLON_COLON when env#templ_param_arg_level > 0 -> DEBUG_MSG "@"; mk T.TEMPL_GT
        | TY_LPAREN | RPAREN | SEMICOLON _ | LBRACKET | LBRACE when env#templ_param_arg_level > 0 -> DEBUG_MSG "@"; token
        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
            match self#peek_nth_rawtoken 2 with
            | TY_TEMPL_GT | LBRACKET -> true
            | TY_LPAREN | RPAREN | SEMICOLON _ | LBRACE when env#templ_param_arg_level > 0 -> true
            | _ -> false
        end -> DEBUG_MSG "@"; token
        | _ when env#templ_arg_flag && env#ty_templ_id_flag -> DEBUG_MSG "@"; token
        | _ when env#templ_param_arg_level > 0 -> DEBUG_MSG "@"; mk T.TEMPL_GT
        | _ -> DEBUG_MSG "@"; mk T.GT
    end

    | STMT, _ when env#templ_param_arg_level = 0 -> DEBUG_MSG "@"; mk T.GT

    | _ when env#templ_head_flag && env#templ_head_lv = env#templ_param_arg_level -> begin
        DEBUG_MSG "@";
        env#exit_templ_head();
        mk T.TEMPL_GT
    end

    | c, _ -> begin
        DEBUG_MSG "@";
        match prev_rawtoken with
        | OPERATOR -> mk T.GT

        | _ when env#templ_arg_flag && env#templ_param_arg_stack_top < env#paren_level -> mk T.GT

        | _ when env#templ_arg_flag && begin
            match self#peek_rawtoken() with
            | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ when env#expr_flag -> begin
                    conv_nth_token (function T.PTR_AMP_AMP,s,e -> T.AMP_AMP "&&",s,e | x -> x) 1;
                    true
                end
                | EXCLAM _ -> begin
                    conv_nth_token (function T.PTR_AMP_AMP,s,e -> T.AMP_AMP "&&",s,e | x -> x) 1;
                    true
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; mk T.TEMPL_GT

        | _ when env#templ_arg_flag (*&& not env#typename_flag && not env#alias_flag *)&& begin
            match self#peek_rawtoken() with
            | IDENT _ | SEMICOLON _ -> true
            | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COLON_COLON -> false
                    | _ -> true
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; token(*mk T.TEMPL_GT*)

        | _ when env#conv_func_id_flag && self#peek_rawtoken() == TY_LPAREN -> token

        | _ when env#templ_param_arg_level = 0 -> mk T.GT

        | _ -> begin
            DEBUG_MSG "* @";
            match self#peek_rawtoken() with
            | SEMICOLON _ when env#using_flag && env#alias_flag && env#typename_flag -> DEBUG_MSG "* @ SEMICOLON"; token

            | SEMICOLON _ | RBRACKET -> DEBUG_MSG "* @ (SEMICOLON|RBRACKET)"; mk T.TEMPL_GT

            | RPAREN when env#decltype_flag && env#paren_level = 1 -> DEBUG_MSG "* @ RPAREN"; mk T.TEMPL_GT

            | COMMA | RPAREN when not env#expr_flag && begin
                env#type_paren_flag || env#ty_param_key_flag || env#const_flag ||
                (env#templ_arg_flag && env#ty_templ_id_flag)
            end -> DEBUG_MSG "* @ (COMMA|RPAREN)"; token

            | COMMA | RPAREN when begin
                env#templ_arg_flag && not env#ty_templ_id_flag
            end -> DEBUG_MSG "* @ (COMMA|RPAREN)"; mk T.TEMPL_GT

            | COLON_COLON | CLASS | COMMA | COLON -> DEBUG_MSG "* @ (COLON_COLON|CLASS|COMMA|COLON)"; mk T.TEMPL_GT

            | FINAL when begin
                match self#peek_nth_rawtoken 2 with
                | SEMICOLON _(* | LBRACE*) -> false
                | _ -> true
            end -> DEBUG_MSG "* @ FINAL"; mk T.TEMPL_GT

            | TY_LPAREN when env#templ_arg_flag && env#ty_templ_id_flag -> DEBUG_MSG "* @ TY_LPAREN"; token

            | TY_LPAREN when c != MEM_INIT && begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                let nll = List.length ll in
                match self#peek_nth_rawtoken 2 with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP when not env#cast_key_flag && nll = 1 -> true
                | IDENT _ when env#templ_arg_flag && env#templ_param_arg_level > 1 && not env#cast_key_flag -> true
                | x when begin
                    is_basic_ty x &&
                    env#templ_arg_flag &&
                    env#templ_param_arg_level > 1 &&
                    not env#cast_key_flag
                end -> true
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COLON_COLON -> begin
                        match self#peek_nth_rawtoken 4 with
                        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when not env#cast_key_flag && nll = 1 -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ TY_LPAREN (IDENT COLON_COLON)? (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; token

            | TY_LPAREN when not env#macro_arg_flag && not env#type_paren_flag && begin
                match self#peek_nth_rawtoken 2 with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP
                | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
                | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | UNSIGNED | SIGNED | STATIC
                | CONSTEXPR | CONSTEVAL | CONSTINIT when env#using_flag && env#alias_flag -> false
                | IDENT x when (is_type_name x || is_type x) && env#using_flag && env#alias_flag -> false
                | IDENT x when self#peek_nth_rawtoken 3 == COLON_COLON && env#using_flag && env#alias_flag -> false
                | _ -> true
            end -> DEBUG_MSG "* @ TY_LPAREN"; mk T.TEMPL_GT

            | TY_LPAREN when begin
                match self#peek_nth_rawtoken 2 with
                | RPAREN -> begin
                    match self#peek_nth_rawtoken 3 with
                    | TY_LPAREN -> true
                    | RPAREN | MINUS_GT | DOT -> true
                    | _ -> false
                end
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | RPAREN -> begin
                        match self#peek_nth_rawtoken 4 with
                        | TY_LPAREN -> begin
                            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:5 () in
                            match (ll : T.token list list) with
                            | [ELLIPSIS::RPAREN::_] -> true
                            | [RPAREN::IDENT _::TY_LPAREN::TY_TEMPL_GT::_] -> true
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ TY_LPAREN"; mk T.TEMPL_GT

            | LBRACE -> begin
                DEBUG_MSG "* @ LBRACE";
                match self#peek_nth_rawtoken 2 with
                | RBRACE -> begin
                    DEBUG_MSG "* @ LBRACE RBRACE";
                    match self#peek_nth_rawtoken 3 with
                    | PLUS | MINUS | PTR_STAR | SLASH | DOT -> DEBUG_MSG "* @ LBRACE RBRACE ..."; token

                    | COMMA | RPAREN when env#arg_paren_flag -> DEBUG_MSG "* @ LBRACE RBRACE (COMMA|RPAREN)"; token

                    | _ -> DEBUG_MSG "* @ LBRACE RBRACE"; mk T.TEMPL_GT
                end

                | IDENT _ when begin
                    env#arg_paren_flag && not env#lambda_dtor_flag &&
                    let nth, l = self#peek_rawtoken_up_to_rbrace ~from:2 () in
                    match self#peek_nth_rawtoken (nth+1) with
                    | COMMA | RPAREN -> begin
                        conv_nth_token (function T.LBRACE,s,e -> T.INI_LBRACE,s,e | x -> x) 1;
                        true
                    end
                    | _ -> false
                end -> DEBUG_MSG "* @ LBRACE ... RBRACE (COMMA|RPAREN)"; token

                | _ when begin
                    env#templ_arg_flag && not env#base_clause_flag &&
                    context != CLASS &&
                    (env#ty_templ_id_flag || env#end_of_decltype_flag || env#trailing_retty_flag)
                end -> DEBUG_MSG "* @ LBRACE"; token

                | _ when begin
                    env#templ_arg_flag &&
                    (env#in_body_brace_flag && context != CLASS)
                end -> DEBUG_MSG "* @ LBRACE"; token

                | _ when env#macro_arg_flag -> DEBUG_MSG "* @ LBRACE"; token

                | _ -> DEBUG_MSG "* @ LBRACE"; mk T.TEMPL_GT
            end

            | NEWLINE | INT_LITERAL _ | FLOAT_LITERAL _ | STR_LITERAL _
            | CHAR_LITERAL _ | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
            | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
            | USER_CHAR_LITERAL _ -> DEBUG_MSG "* @ ..."; mk T.GT

            | PP_ENDIF when self#peek_nth_rawtoken 3 == LBRACE -> DEBUG_MSG "* @ PP_ENDIF"; mk T.TEMPL_GT

            (*| _ when not env#ty_templ_id_flag -> mk T.TEMPL_GT*)

            | _ when begin
                env#base_clause_flag && env#templ_param_arg_level = 1 &&
                match self#peek_rawtoken() with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> false
                | _ -> true
            end -> DEBUG_MSG "* @ *"; mk T.TEMPL_GT

            | PP_ELIF | PP_ELSE when begin
                env#templ_param_arg_level = 1 && env#paren_level = 0 &&
                env#templ_arg_flag && not env#ty_templ_id_flag &&
                let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
                match self#peek_nth_rawtoken (nth+1) with
                | COLON -> true
                | _ -> false
            end -> DEBUG_MSG "* @ (PP_ELIF|PP_ELSE)"; mk T.TEMPL_GT

            | HAT _ when begin
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COMMA | RPAREN | SEMICOLON _ | EQ -> true
                    | COLON_COLON | TY_LPAREN when sub_context == END_OF_TY_SPEC -> true
                    | _ -> false
                end
                | COMMA | RPAREN -> true
                | SEMICOLON _ -> true
                | _ -> false
            end -> DEBUG_MSG "* @ HAT"; token

            | PLUS_PLUS | MINUS_MINUS | MINUS_GT | DOT
            | EQ_EQ | EXCLAM_EQ _ | GT_EQ | LT_EQ | LT_LT
            | MINUS | PLUS | SLASH | PERC | HAT _ | BAR _ | BAR_BAR _
            | QUEST -> DEBUG_MSG "* @ ..."; mk T.TEMPL_GT

            | EQ when not env#type_paren_flag && not env#templ_head_flag
              -> DEBUG_MSG "* @ EQ"; mk T.TEMPL_GT

            | ELLIPSIS when env#type_paren_flag -> DEBUG_MSG "* @ ELLIPSIS"; token

            | TY_LPAREN when env#arg_paren_flag && env#templ_arg_flag -> begin
                DEBUG_MSG "* @ TY_LPAREN";
                conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 1;                    
                mk T.TEMPL_GT
            end

            | TY_LPAREN when env#macro_arg_flag && env#templ_arg_flag && begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | RPAREN when begin
                    match self#peek_nth_rawtoken 2 with
                    | CONST -> true
                    | x when is_ty x -> true
                    | _ -> false
                end -> false
                | RPAREN -> true
                | QUEST -> true
                | x when is_op x -> true
                | _ -> false
            end -> begin
              DEBUG_MSG "* @ TY_LPAREN";
              conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 1;
              mk T.TEMPL_GT
            end

            | _ when env#cast_key_flag -> DEBUG_MSG "* @ *"; mk T.TEMPL_GT

            | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                env#templ_arg_flag && env#templ_param_arg_level > 1 &&
                match self#peek_nth_rawtoken 2 with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken 3 with
                    | COLON_COLON -> true
                    | _ -> false
                end
                | _ -> false
            end -> DEBUG_MSG "* @ (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; mk T.TEMPL_GT

            | _ when env#expr_flag -> DEBUG_MSG "* @ *"; mk T.TEMPL_GT

            | _ -> DEBUG_MSG "* @ *"; token
        end
    end
  in
  let conv_gt_gt () =
    DEBUG_MSG "@";
    let split() =
      self#prepend_token (mk_ T.TY_TEMPL_GT);
      self#prepend_token (mk_ T.TY_TEMPL_GT);
      raise To_be_recovered
      (*if
        not env#init_flag &&
        ((env#templ_head_flag && not env#templ_arg_flag) ||
        (env#templ_arg_flag && env#ty_templ_id_flag))
      then begin
        DEBUG_MSG "@";
        _mk T.TY_TEMPL_GT
      end
      else if env#expr_flag then begin
        DEBUG_MSG "@";
        _mk T.TEMPL_GT
      end
      else if
        context == TY(* || env#const_flag*) ||
        (env#templ_param_arg_level = 2 && env#cast_key_flag)
      then begin
        DEBUG_MSG "@";
        _mk T.TY_TEMPL_GT
      end
      else begin
        DEBUG_MSG "@";
        _mk T.TEMPL_GT
      end*)
    in
    if prev_rawtoken != RPAREN && prev_rawtoken2 == LPAREN then
      token
    else if env#typedef_flag || env#templ_param_arg_level > 1 then
      split()
    else if env#templ_param_arg_level = 0 then
      token
    else begin
      match sub_context with
      | IN_SIMPLE_TEMPL_ID -> split()
      | _ -> begin
          match self#peek_rawtoken() with
          | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> split()
          | IDENT _ -> begin
              match prev_rawtoken with
              | IDENT _ when prev_rawtoken2 == COMMA -> split()
              | _ -> begin
                  match self#peek_nth_rawtoken 2 with
                  | LBRACE -> split()
                  | _ -> token
              end
          end
          | TY_LPAREN when (self#peek_nth_rawtoken 2) == RPAREN -> split()
          | _ -> begin
              match prev_rawtoken with
              | IDENT _ -> begin
                  match prev_rawtoken2 with
                  | CONST -> split()
                  | _ -> token
              end
              | _ -> token
          end
      end
    end
  in
  DEBUG_MSG "@";
  match _rawtok with
  | IDENT s when env#pp_if_flag && begin
      match self#peek_rawtoken() with
      | COLON_COLON -> true
      | _ -> false
  end -> DEBUG_MSG "@"; token

  | IDENT s when env#pp_if_flag -> DEBUG_MSG "@"; mk (T.IDENT_V s)

  | IDENT s when begin
      match prev_rawtoken with
      | PP_IFDEF_A | PP_IFNDEF_A | PP_IFDEF_ATTR | PP_IFNDEF_ATTR
      | PP_IFDEF_B | PP_IFNDEF_B | PP_IFDEF_C | PP_IFNDEF_C
      | PP_IFDEF_CB | PP_IFNDEF_CB
      | PP_IFDEF_D | PP_IFNDEF_D | PP_IFDEF_E | PP_IFNDEF_E
      | PP_IFDEF_H | PP_IFNDEF_H | PP_IFDEF_I | PP_IFNDEF_I
      | PP_IFDEF_O | PP_IFNDEF_O | PP_IFDEF_P | PP_IFNDEF_P
      | PP_IFDEF_S | PP_IFNDEF_S
      | PP_IFDEF_CLOSING | PP_IFNDEF_CLOSING
      | PP_IFDEF_COND | PP_IFNDEF_COND | PP_IFDEF_COND_ | PP_IFNDEF_COND_
      | PP_IFDEF_SHIFT | PP_IFNDEF_SHIFT -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

  | IDENT s when begin
      match prev_rawtoken with
      | TYPENAME when self#peek_rawtoken() == TY_LPAREN -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.IDENT_IM s)

  | IDENT s when begin
      match prev_rawtoken with
      | PP_IFDEF | PP_IFNDEF | PP_UNDEF | PP_DEFINE -> true
      | TYPENAME -> true
      | OBJC_INTERFACE | OBJC_PROTOCOL -> true
      | _ -> false
  end -> DEBUG_MSG "@"; token

  | IDENT s when begin
      match self#peek_rawtoken() with
      | USER_FLOAT_LITERAL _ -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    let rt, _, ep = self#discard_token() in
    match rt with
    | USER_FLOAT_LITERAL f -> T.STR_LITERAL (s^f), stp, ep
    | _ -> assert false
  end
  | IDENT s when env#pp_define_flag || env#pp_line_flag -> DEBUG_MSG "@"; token

  | IDENT "ASM_GLOBAL_DIRECTIVE" when prev_rawtoken == EOF -> begin
      DEBUG_MSG "@";
      self#prepend_token token;
      mk T.BEGIN_ASM
  end

  | IDENT s when env#asm_flag && not env#braced_asm_flag && not env#asm_block_flag -> begin
      DEBUG_MSG "@";
      begin
        match self#peek_rawtoken() with
        | MS_ASM _ when not env#braced_asm_flag -> DEBUG_MSG "inserting END_ASM"; self#prepend_token (mk_ T.END_ASM)

        | RBRACE when begin
            env#brace_level = 1 &&
            match self#peek_nth_rawtoken 2 with
            | DOT -> false
            | IDENT x when is_asm_kw x -> false
            | x -> DEBUG_MSG "%s" (Token.rawtoken_to_string x); true
        end -> DEBUG_MSG "inserting END_ASM"; self#prepend_token (mk_ T.END_ASM)

        | EOF -> DEBUG_MSG "inserting END_ASM"; self#prepend_token (mk_ T.END_ASM)

        | _ -> ()
      end;
      token
  end
  | IDENT s when env#asm_flag -> DEBUG_MSG "@"; token

  | IDENT s when begin
      match self#peek_rawtoken() with
      | PARAMS_MACRO _ -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

  | IDENT s when begin
      match self#peek_rawtoken() with
      | COLON_COLON -> begin
          begin
            match prev_rawtoken with
            | EQ_EQ | EXCLAM_EQ _ -> begin
                try
                  let nth = self#peek_rawtoken_up_to_end_of_qualified_id() in
                  DEBUG_MSG "nth=%d" nth;
                  match self#peek_nth_rawtoken (nth+1) with
                  | LBRACE -> ()
                  | _ -> conv_nth_token (function T.IDENT x,s,e -> T.IDENT_V x,s,e | x -> x) nth
                with
                  _ -> ()
            end
            | _ -> ()
          end;
          true
      end
      | _ -> false
  end -> DEBUG_MSG "@"; token

  | IDENT s when s = "FT_TYPEOF" -> DEBUG_MSG "@"; mk (T.IDENT_CHM s)

  | IDENT s when s = "EXTERN" && not env#stack#at_enum && not env#arg_paren_flag -> begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> DEBUG_MSG "@"; mk (T.IDENT_TM s)
      | _ -> DEBUG_MSG "@"; token
  end

  | IDENT s when is_str_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.STR_MACRO s)
  | IDENT s when is_stmt_macro s(* && self#peek_rawtoken() != TY_LPAREN*) -> mk (T.STMT_MACRO s)
  | IDENT s when is_decl_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.DECL_MACRO s)
  | IDENT s when is_body_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.BODY_MACRO s)
  | IDENT s when is_cc_macro s(* && self#peek_rawtoken() != TY_LPAREN*) -> mk (T.CC_MACRO s)
  | IDENT s when is_ptr_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.PTR_MACRO s)
  | IDENT s when is_attr_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.ATTR_MACRO s)
  | IDENT s when is_param_decl_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.PARAM_DECL_MACRO s)
  | IDENT s when is_arg_macro s(* && self#peek_rawtoken() != TY_LPAREN*) -> mk (T.ARG_MACRO s)
  | IDENT s when is_args_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.ARGS_MACRO s)
  | IDENT s when is_params_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.PARAMS_MACRO s)
  | IDENT s when is_delim_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.DELIM_MACRO s)
  | IDENT s when is_cv_spec_macro s && self#peek_rawtoken() != TY_LPAREN -> mk (T.CV_MACRO s)

  | IDENT s when is_type_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_TM s)
  | IDENT s when is_param_decl_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_PDM s)
  | IDENT s when is_params_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_PM s)
  | IDENT s when is_arg_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_AGM s)
  | IDENT s when is_args_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_AM s)
  | IDENT s when is_suffix_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_SXM s)

  | IDENT s when
      is_expr_macro_ident s &&
      env#expr_flag &&
      not env#in_objc_message_expr &&
      self#peek_rawtoken() == TY_LPAREN
    -> mk (T.IDENT_OM s)

  | IDENT s when
      is_expr_macro_ident s &&
      env#type_paren_flag &&
      not env#in_objc_message_expr &&
      self#peek_rawtoken() == TY_LPAREN
    -> mk (T.IDENT_PDM s)

  | IDENT s when is_expr_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_EM s)

  | IDENT s when is_cv_spec_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_CM s)
  | IDENT s when is_virt_spec_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_VM s)
  | IDENT s when is_attr_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_AM s)
  | IDENT s when is_ident_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> DEBUG_MSG "@"; mk (T.IDENT_IM s)
  | IDENT s when is_body_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> mk (T.IDENT_BM s)

  | IDENT s when Str.string_match google_attr_pat s 0 -> begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> DEBUG_MSG "@"; mk (T.IDENT_AM s)
      | _ -> DEBUG_MSG "@"; mk (T.ATTR_MACRO s)
  end

  | IDENT "NS_ENUM" -> DEBUG_MSG "@"; mk (T.IDENT_E "NS_ENUM")
  | IDENT "NS_OPTIONS" -> DEBUG_MSG "@"; mk (T.IDENT_E "NS_OPTIONS")

  | IDENT s when begin
      self#peek_rawtoken() == TY_LPAREN &&
      not (is_type_name s || is_type s) &&
      not (env#using_flag || env#alias_flag) &&
      match prev_rawtoken with
      | AMP_AMP _ | BAR_BAR _ | PLUS | MINUS | STAR | SLASH | PERC
      | EXCLAM _ | HAT _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
      | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
      | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
      | DOT_STAR | MINUS_GT_STAR -> true
      | DOT | MINUS_GT -> true
      | LPAREN -> begin
          match prev_rawtoken2 with
          | DECLTYPE(* | IDENT _ | IDENT_V _*) -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    match prev_rawtoken with
    | DOT | MINUS_GT -> DEBUG_MSG "@"; mk (T.IDENT_V s)
    | _ ->
        if is_macro_fun s then begin
          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
          if env#ty_param_rhs_flag then begin
            DEBUG_MSG "@";
            mk (T.IDENT_TM s)
          end
          else if
            match self#peek_nth_rawtoken (nth+1) with
            | TY_LPAREN -> true
            | _ -> false
          then begin
            DEBUG_MSG "@";
            mk (T.IDENT_IM s)
          end
          else if is_literal (self#peek_nth_rawtoken (nth+1)) then begin
            DEBUG_MSG "@";
            mk (T.IDENT_OM s)
          end
          else begin
            DEBUG_MSG "@";
            mk (T.IDENT_EM s)
          end
        end
        else
          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
          match self#peek_nth_rawtoken (nth+1) with
          | COLON_COLON -> DEBUG_MSG "@"; mk (T.IDENT_IM s)
          | TY_LPAREN when env#decltype_flag && check_if_macro_args ll -> DEBUG_MSG "@"; mk (T.IDENT_EM s)
          | _ ->
              if
                check_if_macro_args ll ||
                (context == EXPR && List.exists self#check_if_param ll)
              then begin
                if env#ty_param_rhs_flag then begin
                  DEBUG_MSG "@";
                  mk (T.IDENT_TM s)
                end
                else begin
                  DEBUG_MSG "@";
                  mk (T.IDENT_EM s)
                end
              end
              else begin
                DEBUG_MSG "@";
                mk (T.IDENT_V s)
              end
  end

  | IDENT s when (is_type_name s || is_type(* ~weak:true*) s) && begin
      not env#typedef_flag &&
      (not env#using_flag || env#alias_flag) &&
      match prev_rawtoken with
      | SHORT | INT | LONG
      | PTR_STAR | PTR_AMP | PTR_AMP_AMP
      | AMP_AMP _ | BAR_BAR _ | PLUS | MINUS | STAR | SLASH | PERC
      | EXCLAM _ | HAT _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
      | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
      | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
      | DOT_STAR | MINUS_GT_STAR -> false
      | DOT | MINUS_GT when begin
          match self#peek_rawtoken() with
          | COLON_COLON | TEMPL_LT -> false
          | _ -> true
      end -> false
      | COLON_COLON when self#peek_rawtoken() == COLON -> false
      | CONST when begin
          match prev_rawtoken2 with
          | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
          | _ -> false
      end -> false
      | INI_LBRACE -> false
      | COMMA when env#braced_init_flag -> false
      | NEWLINE | SEMICOLON _ when begin
          match self#peek_rawtoken() with
          | TY_LPAREN -> begin
              let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
              check_if_macro_args ll
          end
          | _ -> false
      end -> false
      | _ when begin
          match self#peek_rawtoken() with
          | LT_EQ | GT_EQ | EQ_EQ | EXCLAM_EQ _ -> true
          | TEMPL_LT when env#paren_level > 0 && begin
              let nth, l = self#peek_rawtoken_up_to_rparen_none() in
              not (templ_param_arg_balanced ~paren_level:env#paren_level l)
          end -> true
          | TY_TEMPL_GT when env#templ_param_arg_level = 0 -> true
          | _ -> false
      end -> false
      | _ -> true
  end -> DEBUG_MSG "@"; token

  | IDENT s when is_decl_spec_macro s -> DEBUG_MSG "@"; mk (T.DECL_SPEC_MACRO s)
  | IDENT s when is_virt_spec_macro s -> DEBUG_MSG "@"; mk (T.VIRT_SPEC_MACRO s)
  | IDENT s when is_decl_spec_macro_ident s -> DEBUG_MSG "@"; mk (T.IDENT_DM s)
  | IDENT s when is_class_head_macro_ident s && self#peek_rawtoken() == TY_LPAREN -> begin
      DEBUG_MSG "@";
      mk (T.IDENT_CHM s)
  end
  | IDENT s when is_qt_decl_macro_func s -> DEBUG_MSG "@"; mk (T.IDENT_V s)

  | IDENT s when is_decl_macro_ident s && begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> begin
          conv_nth_token (function T.TY_LPAREN,s,e -> T.LPAREN,s,e | x -> x) 1;
          true
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

  | IDENT s when Str.string_match mock_decl_func_pat s 0 -> DEBUG_MSG "@"; mk (T.IDENT_DSM s)
  | IDENT s when Str.string_match moz_decl_func_pat s 0 -> DEBUG_MSG "@"; mk (T.IDENT_DSM s)

  | IDENT s when begin
      self#peek_rawtoken() == TY_LPAREN &&
      is_decl_stmt_macro_ident s &&
      not env#macro_arg_flag &&
      not env#arg_paren_flag &&
      not env#braced_init_flag
  end -> begin
    DEBUG_MSG "@";
    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
    begin
      match self#peek_nth_rawtoken (nth+1) with
      | LBRACE -> begin
          if
            env#pp_if_section_flag &&
            (env#pp_elif_flag || env#pp_else_flag) &&
            not (env#get_broken_info()) && not (env#get_paren_closing_info()) &&
            let rbraces = env#pp_if_section_top_info.Pinfo.i_rbraces in
            rbraces = 1
          then begin
            conv_nth_token (function T.LBRACE,s,e -> T.ODD_LBRACE,s,e | x -> x) (nth+1);
            env#decr_rbrace_info()
          end
          else
            insert_after_nth_token nth [mk T.MARKER]
      end
      | _ -> ()
    end;
    mk (T.IDENT_DSM s)
  end

  | IDENT s when begin
      self#peek_rawtoken() == TY_LPAREN &&
      match s with
      | "MOZ_FOR_EACH"
      | "JS_FOR_EACH_TRACEKIND"
        -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
    match self#peek_nth_rawtoken (nth+1) with
    | RBRACE | SEMICOLON _ -> mk (T.IDENT_EM s)
    | _ -> mk (T.IDENT_OM s)
  end

  | IDENT s when begin
      match s with
      | "PR_BEGIN_MACRO" -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.BLOCK_HEAD_MACRO s)

  | IDENT s when (is_ns_block_head_macro s || is_block_head_macro s) && begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> begin
          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
          self#peek_nth_rawtoken (nth+1) != EOF
      end
      | _ -> self#peek_nth_rawtoken 2 != EOF
  end -> begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> DEBUG_MSG "@"; mk (T.IDENT_BHM s)
      | _ -> DEBUG_MSG "@"; mk (T.BLOCK_HEAD_MACRO s)
  end

  | IDENT s when begin
      match s with
      | "PR_END_MACRO" -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.BLOCK_END_MACRO s)

  | IDENT s when env#decl_stmt_block_flag && (is_ns_block_end_macro s || is_block_end_macro s) -> begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> begin
          DEBUG_MSG "@";
          let token = mk (T.IDENT_BEM s) in
          match prev_rawtoken with
          | COMMA -> begin
              self#prepend_token token;
              self#prepend_token (mk (T.SEMICOLON false));
              DEBUG_MSG "complementing expression with dummy expression";
              mk T.DUMMY_EXPR
          end
          | RPAREN -> begin
              self#prepend_token token;
              mk (T.SEMICOLON false)
          end
          | _ -> token
      end
      | _ -> DEBUG_MSG "@"; mk (T.BLOCK_END_MACRO s)
  end

  | IDENT s when is_ns_decl_macro_func s -> begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> begin
          DEBUG_MSG "@";
          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
          begin
            match self#peek_nth_rawtoken (nth+1) with
            | LBRACE -> insert_after_nth_token nth [mk T.MARKER]
            | _ -> ()
          end;
          mk (T.IDENT_DSM s)
      end
      | _ when env#macro_arg_flag || env#braced_init_flag || env#arg_paren_flag -> DEBUG_MSG "@"; mk (T.IDENT_V s)
      | _ -> DEBUG_MSG "@"; mk (T.DECL_MACRO s)
  end

  | IDENT s when is_ns_ty_macro_func s -> begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> DEBUG_MSG "@"; mk (T.IDENT_TM s)
      | _ -> DEBUG_MSG "@"; mk (T.TYPE_MACRO s)
  end

  | IDENT s when s = "BAD_CAST" && begin
      match self#peek_rawtoken() with
      | STR_LITERAL _ | IDENT _ -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.OP_MACRO s)

  | IDENT s when env#value_flag && begin
      match self#peek_rawtoken() with
      | TEMPL_LT | COLON_COLON | LBRACE -> false
      | PTR_STAR | PTR_AMP | PTR_AMP_AMP when self#peek_nth_rawtoken 2 == RPAREN -> false
      | _ -> true
  end -> DEBUG_MSG "@"; mk (T.IDENT_V s)

  | IDENT s when begin
      match s with
      | "unique_ptr" -> begin
          match self#peek_rawtoken() with
          | TEMPL_LT -> true
          | _ -> false
      end
      | _ -> false
  end -> DEBUG_MSG "@"; token

  | IDENT s when env#end_of_objc_meth_sel_flag || env#objc_sel_flag && begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.IDENT_AM s)

  | IDENT s when env#end_of_objc_meth_sel_flag || env#objc_sel_flag && begin
      match self#peek_rawtoken() with
      | SEMICOLON _ -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.ATTR_MACRO s)

  | IDENT "in" when begin
      match prev_rawtoken with
      | IDENT _ | IDENT_V _ -> begin
          prev_rawtoken2 == LPAREN && prev_rawtoken3 == FOR &&
          match self#peek_rawtoken() with
          | COLON -> false
          | _ -> true
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.IN

  | IDENT s when begin
      match prev_rawtoken with
      | SUFFIX_MACRO _ -> true
      | IDENT_V _ when begin
          match self#peek_rawtoken() with
          | TY_LPAREN -> false
          | _ ->
              match prev_rawtoken2 with
              | RETURN -> true
              | _ -> false
      end -> true
      | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _
      | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ -> begin
          match self#peek_nth_rawtoken 1 with
          | EOF | SEMICOLON _ -> true
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 2 with
              | EOF | SEMICOLON _ -> true
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken 3 with
                  | EOF | SEMICOLON _ -> true
                  | _ -> false
              end
              | _ -> false
          end
          | _ -> false
      end
      | RPAREN when begin
          env#expr_flag && env#arg_paren_flag &&
          match self#peek_rawtoken() with
          | TY_LPAREN -> false
          | _ -> true
      end -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.SUFFIX_MACRO s)

  | IDENT s when prev_rawtoken == SHARP_SHARP && self#peek_rawtoken() == TY_LPAREN -> DEBUG_MSG "@"; mk (T.IDENT_IM s)

  | IDENT s when env#macro_arg_flag && self#peek_rawtoken() == TY_LPAREN && begin
      match s with
      | "warning" -> begin
          match self#peek_nth_rawtoken 2 with
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 3 with
              | COLON -> true
              | _ -> false
          end
          | _ -> false
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.MS_PRAGMA s)

  | IDENT s when begin
      DEBUG_MSG "@";
      match context, sub_context with
      | STMT, START_OF_STMT _ -> begin
          try
            let _, macro_kind, tok_list_obj = env#find_pending_macro s in
            match macro_kind with
            | ObjectLike -> begin
                let tok_list = (Obj.obj tok_list_obj : token list) in
                let plv, blv, last_rt =
                  List.fold_left
                    (fun (plv, blv, _) ((rt : T.token), _, _) ->
                      DEBUG_MSG "%s" (Token.rawtoken_to_string rt);
                      match rt with
                      | TY_LPAREN -> (plv + 1, blv, rt)
                      | RPAREN -> (plv - 1, blv, rt)
                      | LBRACE -> (plv, blv + 1, rt)
                      | RBRACE -> (plv, blv - 1, rt)
                      | _ -> (plv, blv, rt)
                    ) (0, 0, T.EOF) tok_list
                in
                DEBUG_MSG "plv=%d blv=%d last_rt=%s" plv blv (Token.rawtoken_to_string last_rt);
                let b0 = blv > 0 in
                let b1 = blv < 0 in
                if b0 then begin
                  let t = mk T.LBRACE in
                  for i = 1 to blv do
                    self#prepend_token t
                  done
                end
                else if b1 then begin
                  let t = mk T.RBRACE in
                  for i = 1 to -blv do
                    self#prepend_token t
                  done
                end;
                b0 || b1 || last_rt == COLON
            end
            | FunctionLike _ -> begin
                let tok_list = (Obj.obj tok_list_obj : token list) in
                let plv, blv, last_rt =
                  List.fold_left
                    (fun (plv, blv, _) ((rt : T.token), _, _) ->
                      DEBUG_MSG "%s" (Token.rawtoken_to_string rt);
                      match rt with
                      | TY_LPAREN -> (plv + 1, blv, rt)
                      | RPAREN -> (plv - 1, blv, rt)
                      | LBRACE -> (plv, blv + 1, rt)
                      | RBRACE -> (plv, blv - 1, rt)
                      | _ -> (plv, blv, rt)
                    ) (0, 0, T.EOF) tok_list
                in
                DEBUG_MSG "plv=%d blv=%d last_rt=%s" plv blv (Token.rawtoken_to_string last_rt);
                let b0 = blv > 0 in
                let b1 = blv < 0 in
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                let tl = ref [] in
                if b0 then begin
                  let t = mk T.LBRACE in
                  for i = 1 to blv do
                    tl := t :: !tl
                  done;
                  insert_after_nth_token nth !tl
                end
                else if b1 then begin
                  let t = mk T.RBRACE in
                  for i = 1 to -blv do
                    tl := t :: !tl
                  done;
                  insert_after_nth_token nth !tl
                end;
                b0 || b1 || last_rt == COLON
            end
          with
            _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    match self#peek_rawtoken() with
    | TY_LPAREN -> mk (T.IDENT_SM s)
    | _ -> mk (T.STMT_MACRO s)
  end

  | IDENT s when begin
      try
        let _, macro_kind, tok_list_obj = env#find_pending_macro s in
        match macro_kind with
        | ObjectLike -> begin
            let tok_list = (Obj.obj tok_list_obj : token list) in
            let plv, blv, last_rt =
              List.fold_left
                (fun (plv, blv, _) ((rt : T.token), _, _) ->
                  DEBUG_MSG "%s" (Token.rawtoken_to_string rt);
                  match rt with
                  | TY_LPAREN -> (plv + 1, blv, rt)
                  | RPAREN -> (plv - 1, blv, rt)
                  | LBRACE -> (plv, blv + 1, rt)
                  | RBRACE -> (plv, blv - 1, rt)
                  | _ -> (plv, blv, rt)
                ) (0, 0, T.EOF) tok_list
            in
            DEBUG_MSG "plv=%d blv=%d last_rt=%s" plv blv (Token.rawtoken_to_string last_rt);
            plv = 1 && last_rt == COMMA
        end
        | _ -> false
      with
        _ -> false
  end -> begin
    DEBUG_MSG "@";
    env#open_paren PK_NORMAL;
    mk (T.IDENT_LPAREN s)
  end

  | IDENT "ref" when begin
      match self#peek_rawtoken() with
      | NEW | CLASS -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.MS_REF

  | IDENT s when is_templ_name s && self#peek_rawtoken() == TEMPL_LT -> DEBUG_MSG "@"; token

  | IDENT s when _is_val s -> DEBUG_MSG "@"; mk (T.IDENT_V s)

  | IDENT s when prev_rawtoken == EOF && begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> begin
          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
          match self#peek_nth_rawtoken (nth+1) with
          | COMMA -> begin
              match self#peek_nth_rawtoken (nth+2) with
              | LBRACKET -> true
              | _ -> false
          end
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token token;
    mk T.BEGIN_ASM
  end

  | IDENT s -> conv_ident s

  | IDENT_ s -> mk (T.IDENT s)

  | IDENT_LOM s -> begin
      begin
        match self#peek_rawtoken() with
        | TY_LPAREN -> begin
            let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT x when x = s ->
                conv_nth_token (function T.IDENT x,s,e -> T.IDENT_LOM x,s,e | x -> x) (nth+1);
            | _ -> ()
        end
        | _ -> ()
      end;
      token
  end

  | CC_MACRO s when prev_rawtoken == LPAREN -> mk (T.IDENT s)

  | PTR_AMP_AMP when env#pp_if_flag -> mk (T.AMP_AMP "&&")
  | PTR_AMP when env#pp_if_flag -> mk (T.AMP "&")
  | PTR_STAR when env#pp_if_flag -> mk T.STAR

  | TY_LPAREN_ -> mk T.TY_LPAREN

  | TY_LPAREN when env#pp_define_flag && begin
      match prev_rawtoken with
      | IDENT _ -> begin
          match prev_rawtoken2 with
          | PP_DEFINE -> begin
              let b =
                let so = stp.Lexing.pos_cnum in
                prev_endofs + 1 = so ||
                prev_endln + 1 = stp.Lexing.pos_lnum && prev_endofs + 3 = so
              in
              if not b then
                env#exit_pp_define();
              b
          end
          | _ -> false
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.PP_LPAREN
  | TY_LPAREN when env#pp_if_flag -> DEBUG_MSG "@"; mk T.LPAREN
  | TY_LPAREN when env#pp_line_flag && context != EXPR -> DEBUG_MSG "@"; token

  | TY_LPAREN | LPAREN when begin
      match prev_rawtoken with
      | IDENT_SM _ | IDENT_EM _ | IDENT_DSM _ -> begin
          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
          DEBUG_MSG "\n%s"
            (String.concat "\n"
               (List.map
                  (fun x -> String.concat ";"
                      (List.map Token.rawtoken_to_string x)) ll));
          let f = function
            | (T.SEMICOLON _|RBRACE)::_ -> true
            | l ->
                match (List.rev l : T.token list) with
                | (IF|SWITCH|FOR|WHILE|DO)::_ -> true
                | _ -> false
          in
          match ll with
          | [] -> false
          | [(SEMICOLON _|RBRACE)::[]] -> true
          | [(SEMICOLON _|RBRACE)::r] when Xlist.last r == LBRACE -> true
          | [l] when begin
              match prev_rawtoken with
              | IDENT_DSM _ -> true
              | _ -> false
          end -> begin
            contained_in_list_f
              (function
                | T.COLON::INT_LITERAL _::SEMICOLON _::[] -> true
                | _ -> false
              ) (List.rev l)
          end
          | [l] -> begin
              let b = List.exists is_semicolon l || f l in
              if b then begin
                let rt, _, e = self#peek_nth (nth-1) in
                DEBUG_MSG "rt=%s" (Token.rawtoken_to_string rt);
                insert_after_nth_token (nth-1) [T.SEMICOLON false, e, e]
              end;
              b
          end
          | _ -> List.for_all f ll
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.SS_LPAREN

  | TY_LPAREN when begin
      match prev_rawtoken with
      | IDENT_E _
      | IDENT_AGM _ | IDENT_AM _ | IDENT_BEM _ | IDENT_BHM _ | IDENT_BM _ | IDENT_CHM _ | IDENT_CM _
      | IDENT_DM _ | IDENT_DSM _ | IDENT_EM _ | IDENT_IM _ | IDENT_LM _ | IDENT_LOM _ | IDENT_NSM _
      | IDENT_OM _ | IDENT_PDM _ | IDENT_PM _ | IDENT_SM _ | IDENT_SXM _ | IDENT_TM _ | IDENT_TPM _ | IDENT_VM _
      | GNU_ATTR _ | NOEXCEPT | IF | SWITCH | WHILE | FOR | ODD_FOR | MS_PRAGMA _  | DECLTYPE -> true
      | LPAREN when match prev_rawtoken2 with GNU_ATTR _ -> true | _ -> false -> true
      | ELLIPSIS when prev_rawtoken2 == SIZEOF -> true
      | TEMPL_GT when env#cast_key_flag -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.LPAREN

  | TY_LPAREN when sub_context == END_OF_LAM_INTRO -> begin
      self#ctx_ini();
      DEBUG_MSG "@";
      token
  end
  | TY_LPAREN when begin
      match self#peek_rawtoken() with
      | SIZEOF -> true
      | PTR_STAR | PTR_AMP | PTR_AMP_AMP | HAT _ | MS_CDECL _ | MS_STDCALL _ | CC_MACRO _ -> false
      | rt1 -> begin
          match prev_rawtoken with
          | STATIC_ASSERT | IDENT_TM _ | DECLTYPE -> true
          | IDENT _ -> begin
              match prev_rawtoken2 with
              | TYPEDEF -> begin
                  match rt1 with
                  | IDENT _ -> begin
                      match self#peek_nth_rawtoken 2 with
                      | COLON_COLON -> begin
                          match self#peek_nth_rawtoken 3 with
                          | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> false
                          | _ -> true
                      end
                      | RPAREN -> begin
                          match self#peek_nth_rawtoken 3 with
                          | TY_LPAREN -> false
                          | _ -> true
                      end
                      | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> false
                      | IDENT _ -> begin
                          match self#peek_nth_rawtoken 3 with
                          | RPAREN -> begin
                              match self#peek_nth_rawtoken 4 with
                              | TY_LPAREN -> false
                              | _ -> true
                          end
                          | _ -> true
                      end
                      | _ -> true
                  end
                  | _ -> true
              end
              | _ -> false
          end
          | IDENT_V _ -> begin
              match prev_rawtoken2 with
              | DOT -> true
              | _ -> false
          end
          | _ -> false
      end
  end -> DEBUG_MSG "@"; mk T.LPAREN

  | TY_LPAREN when env#typedef_flag && env#templ_param_arg_level = 0 && begin
      not env#decltype_flag && env#bracket_level = 0 &&
      match prev_rawtoken with
      | IDENT_PM _ | LBRACKET -> false
      | IDENT_V _ when prev_rawtoken2 == LPAREN && prev_rawtoken3 == DECLTYPE -> false
      | IDENT_V _ when prev_rawtoken2 == COLON -> false
      | IDENT_V _ when begin
          prev_rawtoken2 == EQ &&
          match prev_rawtoken3 with
          | IDENT_E _ -> true
          | _ -> false
      end -> false
      | _ when context == MEM -> false
      | _ when is_literal(self#peek_rawtoken()) -> false
      | EQ when begin
          match prev_rawtoken2 with
          | IDENT_E _ -> begin
              match self#peek_rawtoken() with
              | MINUS -> true
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken 2 with
                  | PLUS | MINUS | SLASH | PERC | BAR _ -> true
                  | PTR_STAR | PTR_AMP when begin
                      match self#peek_nth_rawtoken 3 with
                      | RPAREN -> false
                      | _ -> true
                  end -> true
                  | _ -> false
              end
              | _ -> false
          end
          | _ -> false
      end -> false
      | LPAREN when begin
          let nth, l = self#peek_rawtoken_up_to_rparen_none() in
          match self#peek_nth_rawtoken (nth+1) with
          | QUEST | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> true
          | _ -> false
      end -> false
      | RPAREN when env#end_of_cast_type_flag && begin
          match self#peek_rawtoken() with
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 2 with
              | PLUS | MINUS | SLASH | PERC | BAR _ -> true
              | PTR_STAR | PTR_AMP when begin
                  match self#peek_nth_rawtoken 3 with
                  | RPAREN -> false
                  | _ -> true
              end -> true
              | _ -> false
          end
          | _ -> false
      end -> false
      | _ -> true
  end -> DEBUG_MSG "@"; token

  | TY_LPAREN when begin
      match prev_rawtoken with
      | ALIGNOF -> true
      | OBJC_AVAILABLE | OBJC_PROPERTY | OBJC_PLUS | OBJC_MINUS | OBJC_SELECTOR | OBJC_CATCH | OBJC_ENCODE -> true
      | COLON when env#objc_class_interface_flag -> true
      | _ -> false
  end -> DEBUG_MSG "@"; token

  | TY_LPAREN -> conv_ty_lparen()

  | RPAREN when begin
      match prev_rawtoken with
      | PLUS | MINUS | STAR | SLASH | PERC
      | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ | AMP_AMP _ | BAR_BAR _ -> begin
          match prev_rawtoken2 with
          | LPAREN | TY_LPAREN | COMMA -> false
          | _ -> begin
              match self#peek_rawtoken() with
              | COMMA | RPAREN -> false
              | _ -> true
          end
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "complementing expression with dummy expression";
    self#prepend_token token;
    mk T.DUMMY_EXPR
  end

  | RBRACE when begin
      prev_rawtoken == NEWLINE &&
      match self#peek_rawtoken() with
      | PP_ENDIF -> begin
          match prev_rawtoken2 with
          | INT_LITERAL "0" -> begin
              match prev_rawtoken3 with
              | PP_IF | PP_ODD_IF -> not (env#get_odd_info()) || env#pp_if_section_level > env#_pp_if_section_level
              | _ -> false
          end
          | _ -> false
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.ODD_RBRACE

  | RBRACE when begin
      env#asm_flag && not env#braced_asm_flag && not env#asm_block_flag && prev_rawtoken != END_ASM &&
      match self#peek_rawtoken() with
      | DOT -> false
      | IDENT x when is_asm_kw x -> false
      | _ -> true
  end -> begin
    DEBUG_MSG "inserting END_ASM";
    self#prepend_token token;
    mk T.END_ASM
  end

  | DOT when begin
      not env#asm_flag &&
      (match context, sub_context with
      | TOP, INI -> true
      | _ -> false) &&
      (match prev_rawtoken with
      | EOF | NEWLINE -> true
      | _ -> false) &&
      match self#peek_rawtoken() with
      | IDENT x when is_gas_directive x -> true
      | IF -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token token;
    mk T.BEGIN_ASM
  end

  | EQ when begin
      prev_rawtoken != OPERATOR &&
      match self#peek_rawtoken() with
      | SEMICOLON _ -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "complementing expression with dummy expression";
    self#prepend_token (mk T.DUMMY_EXPR);
    token
  end

  | RBRACE when begin
      env#braced_init_flag &&
      match self#peek_rawtoken() with
      | LBRACE -> begin
          match self#peek_nth_rawtoken 2 with
          | NEW -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token (mk T.COMMA);
    token
  end

  | RBRACE when begin
      env#paren_level > 0 && env#brace_level > 0 && env#rel_paren_level > 0
  end -> begin
    parse_warning env stp edp "lack of closing parentheses";
    DEBUG_MSG "lack of closing parentheses";
    self#prepend_token token;
    mk T.RPAREN
  end

  | LT_LT when context == EXPR && prev_rawtoken == EOF -> begin
      self#prepend_token token;
      mk T.DUMMY_EXPR
  end

  | RPAREN when context == TOP && env#type_paren_flag && env#paren_level = 1 && begin
      not env#objc_class_interface_flag &&
      match self#peek_rawtoken() with
      | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
      | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID
      | STATIC -> true
      | PP_IF | PP_IFDEF | PP_IFNDEF(* | PP_ELIF | PP_ELSE*) | PP_ENDIF -> begin
          let nth = self#skip_pp 2 in
          match self#peek_nth_rawtoken (nth+1) with
          | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
          | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID
          | STATIC -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    parse_warning env stp edp "semicolon expected at end of declaration";
    self#prepend_token (mk (T.SEMICOLON false));
    token
  end

  | EQ when begin
      match self#peek_rawtoken() with
      | PP_INCLUDE -> true
      | _ -> false
  end -> begin
    begin
      let rec skip n =
        let nth, _ = self#peek_rawtoken_up_to ~from:n [T.NEWLINE] in
        match self#peek_nth_rawtoken (nth+1) with
        | PP_INCLUDE -> skip (nth+2)
        | _ -> nth
      in
      let nth = skip 2 in
      insert_after_nth_token nth [mk T.MARKER]
    end;
    token
  end

  | RBRACE when not env#stack#at_enum && env#in_body_brace_flag && not env#braced_init_flag && begin
      match prev_rawtoken with
      | IDENT_V _ -> begin
          match prev_rawtoken2 with
          | EQ | EXCLAM_EQ _ -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    parse_warning env stp edp "semicolon expected at end of declaration";
    self#prepend_token token;
    self#prepend_token (mk_ (T.SEMICOLON false));
    raise To_be_recovered
  end

  | RBRACE when begin
      env#stack#at_namespace && prev_rawtoken == NEWLINE && prev_rawtoken2 == PP_ELSE &&
      match self#peek_rawtoken() with
      | PP_INCLUDE -> begin
          let nth, _ = self#peek_rawtoken_up_to ~from:2 [T.NEWLINE] in
          match self#peek_nth_rawtoken (nth+1) with
          | NAMESPACE -> begin
              match self#peek_nth_rawtoken (nth+2) with
              | IDENT x -> begin
                  match self#peek_nth_rawtoken (nth+3) with
                  | LBRACE -> begin
                      conv_nth_token (function T.LBRACE,s,e -> T.ODD_LBRACE,s,e | x -> x) (nth+3);
                      true
                  end
                  | _ -> false
              end
              | _ -> false
          end
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    mk T.ODD_RBRACE
  end

  | RPAREN | SEMICOLON _ | DOT | MINUS_GT | LT_LT | RBRACE | EQ
  | STR_LITERAL _ -> DEBUG_MSG "@"; token

  | QUEST when env#macro_arg_flag -> DEBUG_MSG "@"; self#ctx_expr(); token

  (*| COMMA when not env#macro_arg_flag -> token*)

  | INT_LITERAL s when s = "0" && begin
      match self#peek_rawtoken() with
      | INT_LITERAL _ -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    let rt, _, ep = self#discard_token() in
    match rt with
    | INT_LITERAL s' -> T.INT_LITERAL (s^s'), stp, ep
    | _ -> assert false
  end
  | INT_LITERAL s when s <> "0" -> begin
      if env#asm_flag && not env#braced_asm_flag then begin
        match self#peek_rawtoken() with
        | MS_ASM _  when not env#braced_asm_flag -> DEBUG_MSG "inserting END_ASM"; self#prepend_token (mk_ T.END_ASM)
        | RBRACE when env#brace_level = 1 -> DEBUG_MSG "inserting END_ASM"; self#prepend_token (mk_ T.END_ASM)
        | _ -> ()
      end;
      token
  end

  | PTR_STAR | PTR_AMP | PTR_AMP_AMP when env#pp_line_flag -> token
  | PTR_STAR when prev_rawtoken == COLON_COLON -> token
  | PTR_AMP when env#fold_paren_flag -> mk (T.AMP "&")
  | PTR_AMP_AMP when env#fold_paren_flag -> mk (T.AMP_AMP "&&")
  | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> conv_ptr_op()

  | LBRACKET when env#asm_flag -> DEBUG_MSG "@"; token

  | LBRACKET -> begin
      DEBUG_MSG "@";
      match prev_rawtoken with
      | COMMA | LPAREN | EQ | EOF | NEWLINE | SEMICOLON _ | RETURN(* | RPAREN*) when begin
          not env#asm_flag && not env#typedef_flag &&
          let nth, l = self#peek_rawtoken_up_to_rbracket() in
          DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
          match List.rev l with
          | [INT_LITERAL _] -> false
          | LBRACKET::_ -> false
          | IDENT _::IDENT _::TY_LPAREN::_ -> begin
              match self#peek_nth_rawtoken (nth+1) with
              | LBRACE | TY_LPAREN | TEMPL_LT -> true
              | _ -> false
          end
          | IDENT _::(IDENT _|CLASS)::_ -> false
          | IDENT_V _::IDENT_V _::_ -> false
          | _ -> begin
              match self#peek_nth_rawtoken (nth+1) with
              | LBRACE | TY_LPAREN | TEMPL_LT -> true
              | _ -> false
          end
      end -> DEBUG_MSG "(COMMA|...) @"; mk T.LAM_LBRACKET

      | COMMA | LPAREN | EQ | EOF | NEWLINE | RETURN | QUEST | COLON
      | EXCLAM _ | AMP_AMP _ | BAR_BAR _ | PLUS | MINUS | STAR | SLASH | PERC
      | HAT _ | AMP _ | BAR _ | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
      | DOT | MINUS_GT | MINUS_GT_STAR
      | OBJC_LBRACKET | LBRACE | RBRACE | RPAREN | SEMICOLON _ when begin
          not env#for_flag &&
          not env#asm_flag && not env#typedef_flag &&
          let nth, _l = self#peek_rawtoken_up_to_rbracket() in
          (match prev_rawtoken with
          | RPAREN -> env#end_of_cast_type_flag
          | _ -> true
          ) &&
          let l = List.rev _l in
          DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
          match self#peek_nth_rawtoken (nth+1) with
          | SEMICOLON _ -> begin
              match l with
              | LBRACKET::_ -> self#peek_nth_rawtoken (nth-1) != RBRACKET
              | _ -> true
          end
          (*| RPAREN | COMMA *)| QUEST | COLON -> true
          | AMP_AMP _ | BAR_BAR _ | PLUS | MINUS | STAR | SLASH | PERC
          | HAT _ | AMP _ | BAR _ | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
          | DOT | MINUS_GT | MINUS_GT_STAR -> true
          | _ -> begin
              match l with
              | LBRACKET::_ -> begin
                  let nth', l' = self#peek_rawtoken_up_to_rbracket ~from:2 () in
                  match self#peek_nth_rawtoken (nth'+1) with
                  | RBRACKET -> false
                  | _ -> true
              end
              | IDENT _::(IDENT _|CLASS)::_ -> true
              | TY_LPAREN::IDENT _::RPAREN::IDENT _::IDENT _::_ -> true
              | _ -> false
          end
      end -> DEBUG_MSG "(COMMA|...) @"; mk T.OBJC_LBRACKET

      | COMMA_BROKEN -> DEBUG_MSG "COMMA @"; mk T.LAM_LBRACKET

      | EQ -> DEBUG_MSG "EQ @"; token

      | LBRACE | SEMICOLON _ when context == TOP && begin
          self#peek_rawtoken() != LBRACKET &&
          let nth, l = self#peek_rawtoken_up_to_rbracket() in
          match self#peek_nth_rawtoken (nth+1) with
          | PUBLIC | PRIVATE | PROTECTED | IDENT "ref" | CLASS -> true
          | _ -> false
      end -> DEBUG_MSG "(LBRACE|SEMICOLON) @"; mk T.MS_ATTR_LBRACKET

      | _ -> begin
          DEBUG_MSG "@";
          match self#peek_rawtoken() with
          | LBRACKET when begin
              not env#in_objc_message_expr && not env#objc_block_flag &&
              let nth, l = self#peek_rawtoken_up_to_rbracket ~from:2 () in
              match self#peek_nth_rawtoken (nth+1) with
              | RBRACKET -> true
              | _ -> false
          end -> DEBUG_MSG "@"; mk T.ATTR_LBRACKET
          | EQ when begin
              match self#peek_nth_rawtoken 2 with
              | RBRACKET | COMMA -> true
              | _ -> false
          end -> DEBUG_MSG "@"; mk T.LAM_LBRACKET
          | PTR_STAR when begin
              match self#peek_nth_rawtoken 2 with
              | THIS -> begin
                  match self#peek_nth_rawtoken 3 with
                  | RBRACKET -> begin
                      match self#peek_nth_rawtoken 4 with
                      | TY_LPAREN | LBRACE -> true
                      | _ -> false
                  end
                  | COMMA -> true
                  | _ -> false
              end
              | _ -> false
          end -> DEBUG_MSG "@"; mk T.LAM_LBRACKET

          | PTR_AMP when begin
              match prev_rawtoken with
              | OPERATOR | NEW | DELETE | IDENT_V _ | IDENT _ | RPAREN -> false
              | _ ->
                  match self#peek_nth_rawtoken 2 with
                  | RBRACKET -> begin
                      match self#peek_nth_rawtoken 3 with
                      | TY_LPAREN | LBRACE | TEMPL_LT -> true
                      | _ -> false
                  end
                  | IDENT _ | COMMA -> true
                  | _ -> false
          end -> DEBUG_MSG "@"; mk T.LAM_LBRACKET

          | RBRACKET when begin
              match prev_rawtoken with
              | OPERATOR | NEW | DELETE | IDENT_V _ | IDENT _ | RBRACKET -> false
              | RPAREN -> env#end_of_cast_type_flag
              | _ ->
                  match self#peek_nth_rawtoken 2 with
                  | TY_LPAREN | LBRACE | TEMPL_LT -> true
                  | _ -> false
          end -> DEBUG_MSG "@"; mk T.LAM_LBRACKET

          | IDENT _ | THIS when begin
              context != NEW &&
              match prev_rawtoken with
              | OPERATOR | NEW | DELETE | IDENT_V _ | IDENT _ | RBRACKET -> false
              | RPAREN -> env#end_of_cast_type_flag
              | _ ->
                  match self#peek_nth_rawtoken 2 with
                  | LBRACE | ELLIPSIS -> true
                  | RBRACKET -> begin
                      match self#peek_nth_rawtoken 3 with
                      | TY_LPAREN | LBRACE -> true
                      | _ -> false
                  end
                  | TY_LPAREN | COMMA -> begin
                      let nth, l = self#peek_rawtoken_up_to_rbracket ~from:3 () in
                      match self#peek_nth_rawtoken (nth+1) with
                      | TY_LPAREN | LBRACE -> true
                      | _ -> false
                  end
                  | _ -> false
          end -> DEBUG_MSG "@"; mk T.LAM_LBRACKET

          | _ -> begin
              DEBUG_MSG "@";
              begin
                match self#peek_rawtoken() with
                | EOF -> self#prepend_token (mk T.RBRACKET)
                | _ -> ()
              end;
              token
          end
      end
  end
  | LAM_LBRACKET when env#asm_flag -> DEBUG_MSG "@"; mk T.LBRACKET

  | LBRACE when env#pp_line_flag || env#asm_flag -> DEBUG_MSG "@"; token

  | LBRACE -> conv_lbrace()

  | TEMPL_LT when env#pp_define_body_flag -> DEBUG_MSG "@"; token

  | TEMPL_LT -> begin
      DEBUG_MSG "@";
      match prev_rawtoken with
      | TEMPLATE -> DEBUG_MSG "TEMPLATE @"; token

      | RPAREN when env#end_of_id_macro_call_flag && begin
          match self#peek_nth_rawtoken 1 with
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 2 with
              | TY_LPAREN -> begin
                  let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                  match self#peek_nth_rawtoken (nth+1) with
                  | SEMICOLON _ -> true
                  | _ -> false
              end
              | _ -> false
          end
          | _ -> false
      end -> DEBUG_MSG "RPAREN @"; mk T.LT

      | RPAREN when env#end_of_id_macro_call_flag -> DEBUG_MSG "RPAREN @"; token

      | RPAREN when env#templ_arg_flag && begin
          match self#peek_nth_rawtoken 1 with
          | TYPENAME | CONST -> true
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 2 with
              | COMMA -> begin
                  match self#peek_nth_rawtoken 3 with
                  | TYPENAME | CONST -> true
                  | _ -> false
              end
              | _ -> false
          end
          | _ -> false
      end -> DEBUG_MSG "RPAREN @"; token

      | RPAREN when prev_rawtoken2 == LPAREN && prev_rawtoken3 == OPERATOR -> DEBUG_MSG "RPAREN @"; token

      | RPAREN when env#objc_cat_flag && begin
          match prev_rawtoken2 with
          | IDENT _ -> true
          | _ -> false
      end -> DEBUG_MSG "RPAREN @"; token

      | COMMA when env#macro_arg_flag && begin
          match self#peek_rawtoken() with
          | IDENT _ -> begin
              match self#peek_nth_rawtoken 2 with
              | TY_LPAREN -> begin
                  let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                  match self#peek_nth_rawtoken (nth+1) with
                  | TY_TEMPL_GT -> begin
                      match self#peek_nth_rawtoken (nth+2) with
                      | COMMA -> begin
                          if not (List.exists (List.exists is_literal) ll) then begin
                            conv_nth_token (function T.IDENT x,s,e -> T.IDENT_ x,s,e | x -> x) 1;
                            conv_nth_token (function T.TY_LPAREN,s,e -> T.TY_LPAREN_,s,e | x -> x) 2;
                          end;
                          true
                      end
                      | _ -> false
                  end
                  | _ -> false
              end
              | TEMPL_LT -> begin
                  match self#peek_nth_rawtoken 3 with
                  | x when is_ty x || (match x with IDENT _ -> true | _ -> false) -> begin
                      match self#peek_nth_rawtoken 4 with
                      | TY_TEMPL_GT -> begin
                          match self#peek_nth_rawtoken 5 with
                          | TY_LPAREN -> begin
                              let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:6 () in
                              match self#peek_nth_rawtoken (nth+1) with
                              | TY_TEMPL_GT -> begin
                                  match self#peek_nth_rawtoken (nth+2) with
                                  | COMMA -> begin
                                      if not (List.exists (List.exists is_literal) ll) then begin
                                        conv_nth_token (function T.IDENT x,s,e -> T.IDENT_ x,s,e | x -> x) 1;
                                        conv_nth_token (function T.TY_LPAREN,s,e -> T.TY_LPAREN_,s,e | x -> x) 5;
                                      end;
                                      true
                                  end
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | _ -> false
                      end
                      | COLON_COLON -> begin
                          try
                            let nth = self#peek_rawtoken_up_to_end_of_qualified_id ~from:4 ~ini_tlv:1 () in
                            DEBUG_MSG "nth=%d" nth;
                            match self#peek_nth_rawtoken (nth+1) with
                            | TY_LPAREN -> begin
                                let nth', ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                                DEBUG_MSG "nth'=%d" nth';
                                match self#peek_nth_rawtoken (nth'+1) with
                                | TY_TEMPL_GT -> begin
                                    match self#peek_nth_rawtoken (nth'+2) with
                                    | COMMA -> begin
                                        if not (List.exists (List.exists is_literal) ll) then begin
                                          conv_nth_token (function T.IDENT x,s,e -> T.IDENT_ x,s,e | x -> x) 1;
                                          conv_nth_token (function T.IDENT x,s,e -> T.IDENT_ x,s,e | x -> x) 5;
                                          conv_nth_token (function T.TY_LPAREN,s,e -> T.TY_LPAREN_,s,e | x -> x) (nth+1);
                                        end;
                                        true
                                    end
                                    | _ -> false
                                end
                                | _ -> false
                            end
                            | _ -> false
                          with _ -> false
                      end
                      | _ -> false
                  end
                  | _ -> false
              end
              | COLON_COLON -> begin
                  try
                    let nth = self#peek_rawtoken_up_to_end_of_qualified_id ~from:2 () in
                    DEBUG_MSG "nth=%d" nth;
                    match self#peek_nth_rawtoken (nth+1) with
                    | TY_LPAREN -> begin
                        let nth', ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) () in
                        DEBUG_MSG "nth'=%d" nth';
                        match self#peek_nth_rawtoken (nth'+1) with
                        | TY_TEMPL_GT -> begin
                            match self#peek_nth_rawtoken (nth'+2) with
                            | COMMA -> begin
                                if not (List.exists (List.exists is_literal) ll) then begin
                                  conv_nth_token (function T.IDENT x,s,e -> T.IDENT_ x,s,e | x -> x) 3;
                                  conv_nth_token (function T.IDENT x,s,e -> T.IDENT_ x,s,e | x -> x) nth;
                                  conv_nth_token (function T.TY_LPAREN,s,e -> T.TY_LPAREN_,s,e | x -> x) (nth+1);
                                end;
                                true
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | _ -> false
                  with _ -> false
              end
              | _ -> false
          end
          | _ -> false
      end -> DEBUG_MSG "COMMA @"; token

      | PP_INCLUDE | PP_IMPORT | OPERATOR | IDENT_V _ | INT_LITERAL _ | FLOAT_LITERAL _
      | BOOL_LITERAL _ | CHAR_LITERAL _ | STR_LITERAL _ | NULLPTR | PP_STRINGIZED _
      | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | RPAREN | RBRACKET | COMMA -> DEBUG_MSG "... @"; mk T.LT

      | LPAREN when self#peek_rawtoken() == RPAREN -> DEBUG_MSG "LPAREN @"; mk T.LT

      | LPAREN when env#pp_line_flag && env#pp_if_flag && begin
          match self#peek_rawtoken() with
          | IDENT _ | ASM -> begin
              let nth, l = self#peek_rawtoken_up_to_rparen_none() in
              DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
              match self#peek_nth_rawtoken (nth-1) with
              | TY_TEMPL_GT -> true
              | _ -> false
          end
          | _ -> false
      end -> begin
        DEBUG_MSG "LPAREN @";
        let nth, l = self#peek_rawtoken_up_to_rparen_none() in
        let s = String.concat "" ("<"::(List.rev_map Token.rawtoken_to_repr l)) in
        for i = 2 to nth do
          ignore (self#discard_token())
        done;
        mk (T.STR_LITERAL s)
      end

      | IDENT _ | DYNAMIC_CAST | STATIC_CAST | REINTERPRET_CAST | CONST_CAST -> DEBUG_MSG "... @"; token

      | _ -> begin
          DEBUG_MSG "* @";
          match context, sub_context with
          | EXPR, _ -> begin
              match self#peek_rawtoken() with
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken 2 with
                  | TY_TEMPL_GT | COMMA -> DEBUG_MSG "* @ IDENT"; token
                  | _ -> DEBUG_MSG "* @ IDENT"; mk T.LT
              end

              | CONST | TY_TEMPL_GT -> DEBUG_MSG "* @ (CONST|TY_TEMPL_GT)"; token

              | QUEST when env#paren_level > 0 && begin
                  match self#peek_nth_rawtoken 2 with
                  | IDENT "xml" -> true
                  | _ -> false
              end -> begin
                DEBUG_MSG "* @ QUEST";
                let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                let sl = ref ["<"] in
                let prev_rt = ref T.EOF in
                let last_ep = ref edp in
                for i = 1 to nth - 1 do
                  let rt, sp, ep = self#discard_token() in
                  last_ep := ep;
                  let s =
                    (if is_ident rt then
                      match !prev_rt with
                      | IDENT _ | IDENT_V _
                      | CHAR_LITERAL _ | STR_LITERAL _ -> " "
                      | _ -> ""
                    else "")^(Token.rawtoken_to_repr rt)
                  in
                  sl := s :: !sl;
                  prev_rt := rt
                done;
                let s = String.concat "" (List.rev !sl) in
                (T.STR_LITERAL s, stp, !last_ep)
              end

              | _ -> DEBUG_MSG "* @"; mk T.LT
          end

          | _ when env#paren_level > 0 && begin
              match self#peek_nth_rawtoken 1 with
              | RPAREN -> true
              | _ -> false
          end -> DEBUG_MSG "* @"; mk T.LT

          | _ -> DEBUG_MSG "* @"; token
      end
  end

  | TEMPL_LT_ -> DEBUG_MSG "@"; mk T.TEMPL_LT

  | TY_TEMPL_GT_ -> mk T.TY_TEMPL_GT
  | TY_TEMPL_GT when env#pp_line_flag && not env#pp_if_flag -> token
  | TY_TEMPL_GT -> conv_ty_templ_gt()

  | EQ_EQ when begin
      match self#peek_rawtoken() with
      | EQ -> true
      | _ -> false
  end -> begin
    let _, st, ed = self#discard_token() in
    T.EQ_EQ_EQ, stp, ed 
  end

  | GT_GT when begin
      match self#peek_rawtoken() with
      | INT_LITERAL _ | USER_INT_LITERAL _ -> true
      | _ -> false
  end -> token
  | GT_GT when self#peek_rawtoken() == TY_TEMPL_GT && env#exec_config_flag -> begin
      let _, st, ed = self#discard_token() in
      env#exit_exec_config();
      T.CUDA_GT_GT_GT, stp, ed
  end
  | GT_GT when self#peek_rawtoken() == TY_TEMPL_GT && begin
      match self#peek_nth_rawtoken 2 with
      | INT_LITERAL _ -> true
      | _ -> false
  end -> begin
    let _, st, ed = self#discard_token() in
    T.GT_GT_GT, stp, ed 
  end
  | GT_GT -> conv_gt_gt()

  | COLON_COLON when self#peek_rawtoken() != NEW && not env#pp_define_body_flag && begin
      match prev_rawtoken with
      | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
      | EQ_EQ | EXCLAM_EQ _ | TY_LPAREN | LPAREN | PLUS | MINUS | STAR | SLASH | PERC | CLASS_LBRACE
      | HAT _ | AMP _ | BAR _ | EXCLAM _ | LT_LT | LBRACE | PUBLIC | PRIVATE | PROTECTED | EXTERN
      | OPERATOR | INLINE | STATIC | CONSTEXPR | CONSTEVAL | CONSTINIT | VIRTUAL | INI_LBRACE
      | RETURN | SEMICOLON _ | RBRACE | COMMA | LT | GT | LT_EQ | GT_EQ | COLON | TEMPL_LT | CONST
      | CLASS | ENUM | STRUCT | UNION | PLUS_PLUS | MINUS_MINUS | THREAD_LOCAL | LBRACKET | DELETE
      | AMP_AMP _ | BAR_BAR _ | PTR_STAR | PTR_AMP | PTR_AMP_AMP | TYPENAME | TYPEDEF | MUTABLE
      | USING | NEW | CASE | EOF | QUEST | ELSE | BASE_COLON | TILDE _ | MINUS_GT | DOT | THROW
      | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
      | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | UNSIGNED | SIGNED | NAMESPACE | FRIEND -> true
      | RPAREN when begin
          not env#end_of_id_macro_call_flag &&
          not env#end_of_decltype_flag &&
          not env#pp_define_body_flag
      end -> true
      | NEWLINE when begin
          match sub_context with
          | START_OF_STMT _ | INI -> true
          | _ when env#pp_if_section_top_info.Pinfo.i_sub_context == INI -> true
          | _ -> false
      end -> true
      | TEMPL_GT when env#end_of_templ_head_flag -> true
      | TEMPL_GT when prev_rawtoken2 == TEMPL_LT && prev_rawtoken3 == TEMPLATE -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.HEAD_COLON_COLON

  | COLON_COLON -> DEBUG_MSG "@"; token

  | COLON when env#asm_flag -> DEBUG_MSG "@"; token

  | COLON -> begin
      begin
        match prev_rawtoken with
        | DEFAULT -> begin
            match self#peek_rawtoken() with
            | RBRACE -> begin
                DEBUG_MSG "complementing label with dummy statement";
                self#prepend_token (mk T.DUMMY_STMT)
            end
            | _ -> ()
        end
        | _ -> ()
      end;
      match context with
      | CLASS -> begin
          match prev_rawtoken with
          | PUBLIC | PRIVATE | PROTECTED -> DEBUG_MSG "@"; token
          | _ when env#templ_arg_flag -> DEBUG_MSG "@"; token
          | _ -> DEBUG_MSG "@"; mk T.BASE_COLON
      end

      | _ when env#macro_arg_flag && not env#asm_flag && begin
          match prev_rawtoken with
          | BEGIN_ASM -> false
          | COMMA | LPAREN -> begin
              match self#peek_rawtoken() with
              | STR_LITERAL _ -> true
              | _ -> false
          end
          | _ -> false
      end -> begin
        DEBUG_MSG "inserting END_ASM";
        self#prepend_token token;
        let nth, l = self#peek_rawtoken_up_to_rparen_none() in
        insert_after_nth_token (nth-1) [mk T.END_ASM];
        mk T.BEGIN_ASM
      end

      | _ when prev_rawtoken == COMMA && not env#pp_define_body_flag -> DEBUG_MSG "@"; mk T.BASE_COLON

      | _ when prev_rawtoken == EOF && begin
          match self#peek_rawtoken() with
          | LBRACKET -> true
          | _ -> false
      end -> begin
        DEBUG_MSG "@";
        self#prepend_token token;
        mk T.BEGIN_ASM
      end

      | STMT when env#in_body_brace_flag && begin
          match prev_rawtoken with
          | IDENT _ -> begin
              match self#peek_rawtoken() with
              | RBRACE -> true
              | _ -> false
          end
          | _ -> false
      end -> begin
        DEBUG_MSG "complementing with dummy-statement";
        self#prepend_token (mk T.DUMMY_STMT);
        token
      end

      | _ -> DEBUG_MSG "@"; token
  end

  | TILDE _ when not env#pp_line_flag && begin
      match prev_rawtoken with
      | QUEST -> false
      | COLON when context == EXPR -> false
      | VIRTUAL | COLON_COLON | SEMICOLON _ | RBRACE | INLINE | COLON | MINUS_GT | DOT
      | CLASS_LBRACE | ATTR_MACRO _ -> true
      | NEWLINE | RPAREN when context == MEM -> true
      | IDENT _ when context == MEM -> true (* maybe a macro *)
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.TY_TILDE

  | TILDE _ when begin
      match prev_rawtoken with
      | LPAREN -> self#peek_rawtoken() == RPAREN
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token (mk T.DUMMY_EXPR);
    token
  end

  | TILDE _ -> token

  | CLASS when env#macro_arg_flag -> begin
      DEBUG_MSG "@";
      mk T.ELAB_CLASS
  end

  | ENUM -> begin
      DEBUG_MSG "@";
      match prev_rawtoken with

      | CONST when begin
          match self#peek_rawtoken() with
          | LBRACE -> true
          | _ -> false
      end -> DEBUG_MSG "CONST @"; token

      | CONST | INLINE | TEMPL_LT | COMMA when self#peek_rawtoken() != LBRACE
        -> DEBUG_MSG "(CONST|INLINE|TEMPL_LT|COMMA) @"; mk T.ELAB_ENUM

      | _ -> begin
          DEBUG_MSG "* @";
          match self#peek_rawtoken() with
          | IDENT _ -> begin
              DEBUG_MSG "* @ IDENT";
              match self#peek_nth_rawtoken 2 with
              | RPAREN | COMMA | EQ
                -> DEBUG_MSG "* @ IDENT (RPAREN|COMMA|EQ)"; mk T.ELAB_ENUM
              | IDENT _ when begin
                  match self#peek_nth_rawtoken 3 with
                  | IDENT _ -> begin
                      match self#peek_nth_rawtoken 4 with
                      | LBRACE -> false
                      | _ -> true
                  end
                  | LBRACE -> false
                  | _ -> true
              end -> DEBUG_MSG "* @ IDENT IDENT"; mk T.ELAB_ENUM
              | TY_LPAREN when begin
                  match self#peek_nth_rawtoken 3 with
                  | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                  | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ -> true
                  | IDENT _ -> begin
                      let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:3 () in
                      match self#peek_nth_rawtoken (nth+1) with
                      | TY_LPAREN -> begin
                          conv_nth_token (function T.IDENT x,s,e -> T.CC_MACRO x,s,e | x -> x) 3;
                          true
                      end
                      | _ -> false
                  end
                  | _ -> false
              end -> DEBUG_MSG "* @ IDENT TY_LPAREN"; mk T.ELAB_ENUM
              | PTR_STAR | PTR_AMP | PTR_AMP_AMP
                -> DEBUG_MSG "* @ IDENT (PTR_STAR|PTR_AMP|PTR_AMP_AMP)"; mk T.ELAB_ENUM
              | COLON_COLON -> begin
                  DEBUG_MSG "* @ IDENT COLON_COLON";
                  match self#peek_nth_rawtoken 3 with
                  | IDENT _ -> begin
                      DEBUG_MSG "* @ IDENT COLON_COLON IDENT";
                      match self#peek_nth_rawtoken 4 with
                      | IDENT _ | RPAREN | COMMA | EQ -> mk T.ELAB_ENUM
                      | COLON_COLON -> begin
                          match self#peek_nth_rawtoken 5 with
                          | IDENT _ -> DEBUG_MSG "@"; mk T.ELAB_ENUM
                          | _ -> DEBUG_MSG "@"; token
                      end
                      | _ -> DEBUG_MSG "@"; token
                  end
                  | _ -> DEBUG_MSG "@"; token
              end
              | _ -> DEBUG_MSG "@"; token
          end
          | _ -> DEBUG_MSG "@"; token
      end
  end

  | ELLIPSIS when not env#pp_line_flag && begin
      match prev_rawtoken with
      | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
      | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | UNSIGNED | SIGNED | IDENT _
      | PTR_STAR | PTR_AMP | PTR_AMP_AMP | TY_TEMPL_GT -> true
      | _ -> false
  end && begin
    match self#peek_rawtoken() with
    | RPAREN -> false
    (*| IDENT _ -> false*)
    | _ -> true
  end -> DEBUG_MSG "@"; mk T.ELLIPSIS_

  | INT_LITERAL "0" when
      prev_rawtoken == EQ && (env#virtual_func_flag ||
      (match prev_rawtoken2 with
      | OVERRIDE | FINAL | VIRT_SPEC_MACRO _ -> true
      | _ -> false)) -> begin
        match self#peek_rawtoken() with
      | SEMICOLON _ -> DEBUG_MSG "@"; mk T.PURE_ZERO
      | _ -> DEBUG_MSG "@"; token
  end

  | INT_LITERAL i when begin
      match self#peek_rawtoken() with
      | USER_INT_LITERAL _ -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    let rt, st, ed = self#discard_token() in
    match rt with
    | USER_INT_LITERAL j -> (INT_LITERAL (i^j)), stp, ed
    | _ -> assert false
  end

  | INT_LITERAL _ when begin
      match prev_rawtoken with
      | LBRACKET -> begin
          match self#peek_rawtoken() with
          | PP_DEFINE | PP_UNDEF -> begin
              let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
              match self#peek_nth_rawtoken (nth+1) with
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken (nth+2) with
                  | TY_LPAREN -> false
                  | _ ->
                      conv_nth_token (function T.IDENT x,s,e -> T.SUFFIX_MACRO x,s,e | x -> x) (nth+1);
                      true
              end
              | _ -> false
          end
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token (mk T.SUFFIX_MARKER);
    token
  end

  | USER_INT_LITERAL s when prev_rawtoken == TYPEDEF -> begin (* e.g. typedef 64BIT_INT_CUSTOM_TYPE ZPOS64_T; *)
      DEBUG_MSG "@";
      mk (T.IDENT s)
  end

  | ELSE -> begin
      match self#peek_rawtoken() with
      | PP_ENDIF | EOF -> mk T.ODD_ELSE
      | IDENT x when is_ns_block_end_macro x -> mk T.ODD_ELSE
      | _ -> token
  end

  | BAR_BAR s when begin
      match self#peek_rawtoken() with
      | PP_IF | PP_IFDEF | PP_IFNDEF when macro_body_parsing_flag -> true
      | PP_ELIF | PP_ELSE | PP_ENDIF -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk (T.BAR_BAR_BROKEN s)

  | PLUS | MINUS | STAR | SLASH | PERC
  | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ
  | AMP_AMP _ | BAR_BAR _ when self#peek_rawtoken() == EOF -> begin
      DEBUG_MSG "complementing expression with dummy expression";
      self#prepend_token (mk T.DUMMY_EXPR);
      token
  end

  | ASM when begin
      env#end_of_params_flag ||
      match self#peek_rawtoken() with
      | VOLATILE -> true
      | _ -> false
  end -> mk (T.GNU_ASM "asm")

  | ASM when begin
      match prev_rawtoken with
      | IDENT_V _ -> begin
          match self#peek_rawtoken() with
          | TY_LPAREN -> true
          | _ -> false
      end
      | _ -> false
  end -> mk (T.IDENT_AM "asm")

  | HAT "^" when not env#pp_line_flag && prev_rawtoken == TY_LPAREN && self#peek_rawtoken() != COMMA
    -> DEBUG_MSG "@"; mk T.PTR_HAT

  | HAT "^" when not env#pp_line_flag && begin
      (env#type_paren_flag || env#templ_arg_flag || context == MEM || context == TOP) &&
      match prev_rawtoken with
      | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | FLOAT | SIGNED | UNSIGNED
      | DOUBLE | VOID | TYPE_MACRO _ | AUTO | CONST | VOLATILE | IDENT _ | TY_TEMPL_GT -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.PTR_HAT

  | HAT "^" when begin
      match prev_rawtoken with
      | LPAREN -> begin
          match self#peek_rawtoken() with
          | COMMA | RPAREN -> false
          | _ -> true
      end
      | COMMA -> begin
          match self#peek_rawtoken() with
          | COMMA | RPAREN -> false
          | _ -> true
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.TY_HAT

  | BAR_BAR s when begin
      match self#peek_rawtoken() with
      | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
          (*let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in*)
          let nth = self#skip_pp 1 in
          let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
          let open_count, close_count = count_parens l in
          DEBUG_MSG "open_count=%d, close_count=%d" open_count close_count;
          close_count - open_count = 1
      end
      | _ -> false
  end -> mk (T.BAR_BAR_BROKEN s)

  | BAR_BAR _ when prev_rawtoken == EOF -> begin
      self#prepend_token token;
      _mk T.DUMMY_EXPR
  end

  | TYPEID when begin
      match self#peek_rawtoken() with
      | TY_LPAREN | LPAREN -> false
      | _ -> true
  end -> begin
    DEBUG_MSG "@";
    if env#arg_paren_flag then
      mk (T.IDENT_V "typeid")
    else
      mk (T.IDENT "typeid")
  end

  | PP_UNKNOWN x when env#pp_line_flag -> mk (T.PP_STRINGIZED x)

  | PP_IF | PP_IFDEF | PP_IFNDEF when begin
      match prev_rawtoken with
      | NEWLINE -> begin
          not env#asm_flag && context == TOP && sub_context == INI &&
          let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
          match self#peek_nth_rawtoken (nth+1) with
          | DOT when begin
              match self#peek_nth_rawtoken (nth+2) with
              | IDENT x -> is_gas_directive x
              | _ -> false
          end -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    let _gete () =
      match _rawtok with
      | PP_IF     -> mk T.PP_IF_E
      | PP_IFDEF  -> mk T.PP_IFDEF_E
      | PP_IFNDEF -> mk T.PP_IFNDEF_E
      | _ -> assert false
    in
    self#prepend_token (_gete());
    mk T.BEGIN_ASM
  end

  | PP_IF | PP_IFDEF | PP_IFNDEF
  | PP_IF_E | PP_IFDEF_E | PP_IFNDEF_E
  | PP_IF_S | PP_IFDEF_S | PP_IFNDEF_S
  | PP_IF_CLOSING | PP_IFDEF_CLOSING | PP_IFNDEF_CLOSING -> begin
      DEBUG_MSG "@";

      let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in

      if
        let plv = ref 0 in
        let filt = function
          | T.TY_LPAREN | LPAREN -> incr plv; false
          | T.RPAREN -> decr plv; false
          | _ -> false
        in
        match self#peek_nth_rawtoken (nth+1) with
        | RBRACE -> begin
            DEBUG_MSG "@";
            match self#peek_nth_rawtoken (nth+2) with
            | ELSE -> begin
                DEBUG_MSG "@";
                let nth0, l0 = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+3) ~filt () in
                !plv > 0 &&
                match self#peek_nth_rawtoken nth0 with
                | PP_ELSE -> begin
                    DEBUG_MSG "@";
                    let nth1, l1 = self#peek_rawtoken_up_to ~from:nth0 [T.NEWLINE] in
                    match self#peek_nth_rawtoken (nth1+1) with
                    | RBRACE -> begin
                        DEBUG_MSG "@";
                        match self#peek_nth_rawtoken (nth1+2) with
                        | ELSE -> begin
                            DEBUG_MSG "@";
                            conv_nth_token (function T.RBRACE,s,e -> T.ODD_RBRACE,s,e | x -> x) (nth+1);
                            conv_nth_token (function T.ELSE,s,e -> T.ODD_ELSE,s,e | x -> x) (nth+2);
                            conv_nth_token (function T.RBRACE,s,e -> T.ODD_RBRACE,s,e | x -> x) (nth1+1);
                            conv_nth_token (function T.ELSE,s,e -> T.ODD_ELSE,s,e | x -> x) (nth1+2);
                            true
                        end
                        | _ -> false
                    end
                    | _ -> false
                end
                | PP_ENDIF -> begin
                    conv_nth_token (function T.RBRACE,s,e -> T.ODD_RBRACE,s,e | x -> x) (nth+1);
                    conv_nth_token (function T.ELSE,s,e -> T.ODD_ELSE,s,e | x -> x) (nth+2);
                    true
                end
                | x -> DEBUG_MSG "%s" (Token.rawtoken_to_string x); false
            end
            | _ -> false
        end
        | _ -> false
      then begin
        self#prepend_token token;
        self#prepend_token (mk T.ELSE);
        mk T.RBRACE
      end
      else

      let cond =
        let s = String.concat " " (List.map Token.rawtoken_to_repr (List.rev l)) in
        match _rawtok with
        | PP_IF | PP_IF_E | PP_IF_S | PP_IF_CLOSING -> Pinfo.PP_IF s
        | PP_IFDEF | PP_IFDEF_E | PP_IFDEF_S | PP_IFDEF_CLOSING -> Pinfo.PP_IFDEF s
        | PP_IFNDEF | PP_IFNDEF_E | PP_IFNDEF_S | PP_IFNDEF_CLOSING -> Pinfo.PP_IFNDEF s
        | _ -> assert false
      in
      env#enter_pp_if_section stp.Lexing.pos_lnum context sub_context cond;
      if env#broken_flag then begin
        env#set_broken_info()
      end;
      if env#asm_flag then begin
        env#set_asm_info()
      end;

      Stack.iter
        (fun info ->
          DEBUG_MSG "%s" (Pinfo.pp_if_section_info_to_string info)
        ) env#pstat#pp_if_section_stack;

      if
        (env#braced_init_flag && begin
          match prev_rawtoken with
          | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
          | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
          | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
          | USER_CHAR_LITERAL _ -> true
          | COMMA -> begin
              match prev_rawtoken2 with
              | STR_LITERAL _ | CHAR_LITERAL _ | FLOAT_LITERAL _ | INT_LITERAL _
              | BOOL_LITERAL _ | NULLPTR | PP_STRINGIZED _
              | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ | USER_STR_LITERAL _
              | USER_CHAR_LITERAL _ -> true
              | _ -> false
          end
          | _ -> false
        end) ||
        begin
          DEBUG_MSG "@";
          env#brace_level > 0 &&
          ((env#in_body_brace_level = 0 || cond = env#pstat#last_pp_if_section_info.Pinfo.i_cond) &&
           env#in_body_brace_flag ||
           env#stack#in_class) &&
          begin
            match prev_rawtoken with
            | LBRACE | RBRACE | SEMICOLON _ | NEWLINE -> true
            | _ -> false
          end &&
          try
            let lv = ref 0 in
            let min_lv = ref 0 in
            let filt = function
              | T.LBRACE -> incr lv; false
              | T.RBRACE ->
                  decr lv;
                  if !lv < !min_lv then
                    min_lv := !lv;
                  false
              | _ -> false
            in
            let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~filt () in
            DEBUG_MSG "lv=%d, min_lv=%d" !lv !min_lv;
            match self#peek_nth_rawtoken nth with
            | PP_ENDIF when !min_lv < 0 -> env#set_odd_info(); true
            | _ -> false
          with
          | Abort -> false
        end
      then
        env#set_pp_odd_flag()
      else
        env#clear_pp_odd_flag();

      let conv_flag =
        match _rawtok with
        | PP_IF | PP_IFDEF | PP_IFNDEF -> true
        | _ -> false
      in
      DEBUG_MSG "conv_flag=%B" conv_flag;

      let _getodd () =
        match _rawtok with
        | PP_IF     -> mk T.PP_ODD_IF
        | PP_IFDEF  -> mk T.PP_ODD_IFDEF
        | PP_IFNDEF -> mk T.PP_ODD_IFNDEF
        | _ -> assert false
      in
      let getodd () =
        env#set_odd_info();
        _getodd()
      in
      if env#pp_odd_flag then begin
        DEBUG_MSG "@";
        getodd()
      end
      else if conv_flag then begin
        let geta () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_A
          | PP_IFDEF  -> mk T.PP_IFDEF_A
          | PP_IFNDEF -> mk T.PP_IFNDEF_A
          | _ -> assert false
        in
        let geto () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_O
          | PP_IFDEF  -> mk T.PP_IFDEF_O
          | PP_IFNDEF -> mk T.PP_IFNDEF_O
          | _ -> assert false
        in
        let geti () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_I
          | PP_IFDEF  -> mk T.PP_IFDEF_I
          | PP_IFNDEF -> mk T.PP_IFNDEF_I
          | _ -> assert false
        in
        let gets () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_S
          | PP_IFDEF  -> mk T.PP_IFDEF_S
          | PP_IFNDEF -> mk T.PP_IFNDEF_S
          | _ -> assert false
        in
        let getp () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_P
          | PP_IFDEF  -> mk T.PP_IFDEF_P
          | PP_IFNDEF -> mk T.PP_IFNDEF_P
          | _ -> assert false
        in
        let getb () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_B
          | PP_IFDEF  -> mk T.PP_IFDEF_B
          | PP_IFNDEF -> mk T.PP_IFNDEF_B
          | _ -> assert false
        in
        let _gete () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_E
          | PP_IFDEF  -> mk T.PP_IFDEF_E
          | PP_IFNDEF -> mk T.PP_IFNDEF_E
          | _ -> assert false
        in
        let gete ?(closing=false) () =
          DEBUG_MSG "@ closing=%B" closing;
          if env#asm_flag then
            _gete()
          else if closing then begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            begin
              match self#peek_nth_rawtoken (nth+1) with
              | PTR_AMP_AMP -> begin
                  let stat = ref 0 in
                  let filt = function
                    | T.RPAREN when !stat = 0 -> stat := 1; false
                    | LBRACE when !stat = 1 -> stat := 2; false
                    | TY_LPAREN when !stat = 1 -> stat := 0; false
                    | RBRACE when !stat = 2 -> stat := 1; false
                    | _ -> false
                  in
                  let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+2) ~filt () in
                  match l with
                  | COMMA::_ when !stat = 2 -> self#prepend_token (geta())
                  | _ -> self#prepend_token (_gete())
              end
              | _ -> self#prepend_token (_gete())
            end;
            self#prepend_token (mk T.MARKER);
            raise To_be_recovered
          end
          else
          let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
          match self#peek_nth_rawtoken (nth+1) with
          | LT_LT | GT_GT -> begin
              match _rawtok with
              | PP_IF     -> mk T.PP_IF_SHIFT
              | PP_IFDEF  -> mk T.PP_IFDEF_SHIFT
              | PP_IFNDEF -> mk T.PP_IFNDEF_SHIFT
              | _ -> assert false
          end
          | PTR_AMP_AMP -> geta()
          | BAR_BAR _ -> geto()
          | BAR _ -> getb()
          | STR_LITERAL _ when env#braced_init_flag -> geti()
          | STR_LITERAL _ when not env#asm_flag -> gets()
          | QUEST -> begin
              env#set_cond_expr_info();
              match _rawtok with
              | PP_IF     -> mk T.PP_IF_COND
              | PP_IFDEF  -> mk T.PP_IFDEF_COND
              | PP_IFNDEF -> mk T.PP_IFNDEF_COND
              | _ -> assert false
          end
          | COLON -> begin
              env#set_cond_expr_info();
              match _rawtok with
              | PP_IF     -> mk T.PP_IF_COND_
              | PP_IFDEF  -> mk T.PP_IFDEF_COND_
              | PP_IFNDEF -> mk T.PP_IFNDEF_COND_
              | _ -> assert false
          end
          | DOT | MINUS_GT when begin
              match self#peek_nth_rawtoken (nth+2) with
              | IDENT _ -> begin
                  match self#peek_nth_rawtoken (nth+3) with
                  | TY_LPAREN -> true
                  | _ -> false
              end
              | _ -> false
          end -> getp()
          | EQ -> geti()
          | x when is_pp_control_line x -> begin
              let nth', _ = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
              let nth'' =
                if self#peek_nth_rawtoken nth' == PP_ENDIF then
                  let n, _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                  n
                else
                  self#peek_rawtoken_up_to_section_end ~from:(nth'+1) ()
              in
              match self#peek_nth_rawtoken (nth''+1) with
              | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                  let nth''', _ = self#peek_rawtoken_up_to ~from:(nth''+2) [T.NEWLINE] in
                  match self#peek_nth_rawtoken (nth'''+1) with
                  | BAR _ -> getb()
                  | _ -> _gete()
              end
              | _ -> _gete()
          end
          | _ ->
              let ini_plv = env#paren_level in
              let plv = ref ini_plv in
              let min_plv = ref ini_plv in
              let filt (x : T.token) =
                begin
                  match x with
                  | TY_LPAREN -> incr plv
                  | RPAREN -> begin
                      decr plv;
                      min_plv := min !plv !min_plv
                  end
                  | _ -> ()
                end;
                false
              in
              let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) ~filt () in
              match self#peek_nth_rawtoken (nth'-1) with
              | BAR_BAR _ -> geto()
              | PTR_AMP_AMP | AMP_AMP _ when !min_plv = ini_plv -> geta()
              | IDENT _ -> begin
                  let nth'' =
                    if self#peek_nth_rawtoken nth' == PP_ENDIF then
                      let n, _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                      n
                    else
                      self#peek_rawtoken_up_to_section_end ~from:(nth'+1) ()
                  in
                  match self#peek_nth_rawtoken (nth''+1) with
                  | TY_LPAREN -> getp()
                  | _ -> _gete()
              end
              | _ -> _gete()
        in
        let getd () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_D
          | PP_IFDEF  -> mk T.PP_IFDEF_D
          | PP_IFNDEF -> mk T.PP_IFNDEF_D
          | _ -> assert false
        in
        let geth () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_H
          | PP_IFDEF  -> mk T.PP_IFDEF_H
          | PP_IFNDEF -> mk T.PP_IFNDEF_H
          | _ -> assert false
        in
        let geteh () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_EH
          | PP_IFDEF  -> mk T.PP_IFDEF_EH
          | PP_IFNDEF -> mk T.PP_IFNDEF_EH
          | _ -> assert false
        in
        let getc () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_C
          | PP_IFDEF  -> mk T.PP_IFDEF_C
          | PP_IFNDEF -> mk T.PP_IFNDEF_C
          | _ -> assert false
        in
        let getcb () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_CB
          | PP_IFDEF  -> mk T.PP_IFDEF_CB
          | PP_IFNDEF -> mk T.PP_IFNDEF_CB
          | _ -> assert false
        in
        let getcl () =
          env#set_cond_sub_info Pinfo.PP_CLOSING;
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_CLOSING
          | PP_IFDEF  -> mk T.PP_IFDEF_CLOSING
          | PP_IFNDEF -> mk T.PP_IFNDEF_CLOSING
          | _ -> assert false
        in
        let getattr () =
          match _rawtok with
          | PP_IF     -> mk T.PP_IF_ATTR
          | PP_IFDEF  -> mk T.PP_IFDEF_ATTR
          | PP_IFNDEF -> mk T.PP_IFNDEF_ATTR
          | _ -> assert false
        in
        DEBUG_MSG "@";
        match prev_rawtoken with
        | TY_LPAREN (*| LPAREN *) when not env#type_paren_flag -> DEBUG_MSG "@"; gete()

        | INI_LBRACE -> DEBUG_MSG "@"; geti()

        | CLASS | STRUCT | UNION when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT x when is_decl_spec_macro x || is_decl_spec_macro_ident x -> true
            | _ -> false
        end -> DEBUG_MSG "@"; getd()

        | QUEST | BAR _ | PLUS | MINUS | SLASH | PERC -> DEBUG_MSG "@"; gete()

        | COLON when begin
            match prev_rawtoken2 with
            | PUBLIC | PROTECTED | PRIVATE -> true
            | _ -> false
        end -> DEBUG_MSG "@"; token

        | COLON when env#in_body_brace_flag && begin
            match context, sub_context with
            | STMT, START_OF_STMT _ -> false
            | _ -> true
        end -> DEBUG_MSG "@"; gete()

        | RETURN -> begin
            DEBUG_MSG "@";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            let last_rt = self#peek_nth_rawtoken (nth'-1) in
            match (l' : T.token list) with
            | PTR_AMP_AMP::_ -> DEBUG_MSG "@"; geta()
            | BAR_BAR _::_ -> DEBUG_MSG "@"; geto()
            | _ when begin
                let nth'' =
                  if self#peek_nth_rawtoken nth' == PP_ENDIF then
                    let n, _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                    n
                  else
                    self#peek_rawtoken_up_to_section_end ~from:(nth'+1) ()
                in
                match self#peek_nth_rawtoken (nth''+1) with
                | TY_LPAREN when last_rt != EQ -> true
                | _ -> false
            end -> DEBUG_MSG "@"; getp()
            | _ when last_rt == EQ -> geta()
            | _ -> DEBUG_MSG "@"; gete()
        end

        | TY_TEMPL_GT when context != TOP || not env#objc_class_interface_flag -> begin
            DEBUG_MSG "@";
            env#set_dtor_if_section_flag();
            _gete()
        end

        | TEMPL_GT when (env#end_of_templ_head_flag || prev_rawtoken2 == TEMPL_LT) && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | USING -> false
            | _ -> true
        end -> DEBUG_MSG "@"; getd()

        | STR_MACRO _ | STR_MARKER -> DEBUG_MSG "@"; gets()

        | RBRACE when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | CATCH -> true
            | _ -> false
        end -> DEBUG_MSG "@"; geth()

        | NEWLINE | SEMICOLON _ | LBRACE when begin
            not env#type_paren_flag &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | CLASS | UNION | STRUCT -> begin
                let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
                (*not (list_memqn [(*T.LBRACE;*)T.SEMICOLON _] l)*)
                not (List.exists is_semicolon l)
            end
            | _ -> false
        end -> DEBUG_MSG "@"; getc()

        | NEWLINE when begin
            try
              let info = env#pp_if_section_top_info in
              DEBUG_MSG "info=%s" (Pinfo.pp_if_section_info_to_string info);
              info.Pinfo.i_context == C.EXPR
            with
              _ -> false
        end -> begin
          DEBUG_MSG "@";
          if env#braced_init_flag && env#paren_level = 0 then
            geti()
          else if
            prev_rawtoken2 == PP_ENDIF &&
            match prev_rawtoken3 with
            | BAR _ | AMP _ | BAR_BAR _ | AMP_AMP _ -> true
            | _ -> false
          then
            gete()
          else if env#arg_paren_flag then
            geti()
          else
            gete()
        end

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ -> true
            | _ -> false
        end -> DEBUG_MSG "@"; getc()

        | EQ when begin
            match prev_rawtoken2 with
            | IDENT _ -> prev_rawtoken3 == TYPENAME
            | _ -> false
        end -> DEBUG_MSG "@"; getd()

        | EQ when (context == STMT || context == EXPR) && begin
            match prev_rawtoken2 with
            | IDENT_V _ -> true
            | _ -> false
        end -> begin
          DEBUG_MSG "@";
          let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
          let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
          match (l' : T.token list) with
          | PTR_AMP_AMP::_ -> DEBUG_MSG "@"; geta()
          | BAR _::_ -> DEBUG_MSG "@"; gete()
          | _ -> begin
              let nth'' =
                if self#peek_nth_rawtoken nth' == PP_ENDIF then
                  let n, _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                  n
                else
                  self#peek_rawtoken_up_to_section_end ~from:(nth'+1) ()
              in
              match self#peek_nth_rawtoken (nth''+1) with
              | TY_LPAREN -> DEBUG_MSG "@"; getp()
              | DOT | MINUS_GT -> DEBUG_MSG "@"; geta()
              | _ -> DEBUG_MSG "@"; gete()
          end
        end

        | CLASS_LBRACE | LBRACE | SEMICOLON _ | RBRACE | NEWLINE | EOF | TEMPL_GT | STATIC when begin
            DEBUG_MSG "@";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            (*let nth = self#skip_pp ~limit:1 1 in*)
            let nth1 = nth + 1 in
            let rt_nth1 = self#peek_nth_rawtoken nth1 in

            let nth, nth1, rt_nth1 =
              match rt_nth1 with
              | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                  let nth' = self#peek_rawtoken_up_to_section_end ~from:(nth1+1) () in
                  match self#peek_nth_rawtoken (nth'+1) with
                  | PP_ENDIF -> begin
                      let nth'', _ = self#peek_rawtoken_up_to ~from:(nth1+1) [T.NEWLINE] in
                      nth'', nth''+1, self#peek_nth_rawtoken (nth''+1)
                  end
                  | _ -> nth, nth1, rt_nth1
              end
              | _ -> nth, nth1, rt_nth1
            in

            match rt_nth1 with
            | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | FLOAT | DOUBLE
            | VOID | UNSIGNED | SIGNED | CONST | EXTERN | TYPE_MACRO _ | IDENT _ when begin
                ((match rt_nth1 with IDENT _ -> false | _ -> true) ||
                (match rt_nth1 with
                | IDENT x ->
                    is_type_name x || is_type x || is_type_macro_ident x ||
                    is_decl_spec_macro x || is_decl_spec_macro_ident x
                | _ -> false)) &&
                context == TOP &&
                let filt = function
                  | T.SEMICOLON _ -> true
                  | _ -> false
                in
                try
                  let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+2) ~filt () in
                  DEBUG_MSG "l'=%s" (String.concat ";" (List.map Token.rawtoken_to_string l'));
                  match l' with
                  | [STR_LITERAL _] when rt_nth1 == EXTERN -> true
                  | _ ->
                  match self#peek_nth_rawtoken (nth'-1) with
                  | RPAREN when begin
                      match self#peek_nth_rawtoken nth' with
                      | PP_ENDIF -> begin
                          let n, _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                          match self#peek_nth_rawtoken (n+1) with
                          | IDENT y -> begin
                              match self#peek_nth_rawtoken nth1 with
                              | IDENT x -> x = y
                              | _ -> false
                          end
                          | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                              let n', _ = self#peek_rawtoken_up_to ~from:(n+2) ~skip_pp_control_line:true [T.NEWLINE] in
                              match self#peek_nth_rawtoken (n'+1) with
                              | IDENT y -> begin
                                  match self#peek_nth_rawtoken nth1 with
                                  | IDENT x -> x = y
                                  | _ -> false
                              end
                              | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                                  let n'', _ = self#peek_rawtoken_up_to ~from:(n'+2) [T.NEWLINE] in
                                  match self#peek_nth_rawtoken (n''+1) with
                                  | IDENT y -> begin
                                      match self#peek_nth_rawtoken nth1 with
                                      | IDENT x -> x = y
                                      | _ -> false
                                  end
                                  | _ -> false
                              end
                              | _ -> false
                          end
                          | _ -> false
                      end
                      | _ -> false
                  end -> false
                  | RPAREN -> true
                  | _ -> false
                with
                  _ -> false
            end -> true

            | STATIC | VOID | INT | INLINE when context == TOP -> begin
                let nth', l' = self#peek_rawtoken_up_to ~from:(nth+2) ~is_target:is_semicolon
                    [T.TY_LPAREN;LBRACE;PP_ELIF;PP_ELSE;PP_ENDIF]
                in
                not (list_memqn [T.PP_IF;PP_IFDEF;PP_IFNDEF] l') &&
                match self#peek_nth_rawtoken nth' with
                | PP_ELIF | PP_ELSE | PP_ENDIF -> begin
                    begin
                      match l' with
                      | [IDENT _] ->
                          conv_nth_token
                            (function T.IDENT x,s,e -> T.DECL_SPEC_MACRO x,s,e | x -> x)
                            (nth+2)
                      | _ -> ()
                    end;
                    true
                end
                | _ -> false
            end

            | CONSTEXPR -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | PP_ELIF | PP_ELSE | PP_ENDIF -> true
                | _ -> false
            end

            | IDENT x when context == TOP && sub_context == INI && begin
                match self#peek_nth_rawtoken (nth+2) with
                | IDENT _ -> begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | PP_ELIF | PP_ELSE | PP_ENDIF -> true
                    | _ -> false
                end
                | PP_ELIF | PP_ELSE | PP_ENDIF when is_type_name x || is_type x -> true
                | TY_LPAREN when (is_decl_spec_macro_ident x || is_type_macro_ident x) && begin
                    let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+3) () in
                    match self#peek_nth_rawtoken (nth'-1) with
                    | SEMICOLON _ | RBRACE | NEWLINE -> false
                    | _ -> true
                end -> true
                | _ -> false
            end -> true

            | IDENT x when (context == TOP || context == MEM) && begin
                DEBUG_MSG "x=%s" x;
                match self#peek_nth_rawtoken (nth+2) with
                | TY_LPAREN -> begin
                    let nth_, ll_ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+3) () in
                    match self#peek_nth_rawtoken (nth_+1) with
                    | IDENT _ -> begin
                        match self#peek_nth_rawtoken (nth_+2) with
                        | TY_LPAREN -> begin
                            let nth__, ll__ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth_+3) () in
                            match self#peek_nth_rawtoken (nth__+1) with
                            | PP_ELIF | PP_ELSE -> begin
                                let nth'' = self#peek_rawtoken_up_to_section_end ~from:(nth__+1) () in
                                  match self#peek_nth_rawtoken (nth''+1) with
                                  | LBRACE | EQ -> true
                                  | _ -> false
                            end
                            | PP_ENDIF -> begin
                                let nth'', _ = self#peek_rawtoken_up_to ~from:(nth__+1) [T.NEWLINE] in
                                match self#peek_nth_rawtoken (nth''+1) with
                                | LBRACE | EQ -> true
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end

                    | PP_ELIF | PP_ELSE -> begin
                        let nth'' = self#peek_rawtoken_up_to_section_end ~from:(nth_+1) () in
                        match self#peek_nth_rawtoken (nth''+1) with
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth''+2) with
                            | TY_LPAREN -> begin
                                let nth''', _ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth''+3) () in
                                match self#peek_nth_rawtoken (nth'''+1) with
                                | LBRACE | EQ -> begin
                                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_DM x,s,e | x -> x) nth1;
                                    true
                                end
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end
                    | PP_ENDIF -> begin
                        let nth'', _ = self#peek_rawtoken_up_to ~from:(nth_+1) [T.NEWLINE] in
                        match self#peek_nth_rawtoken (nth''+1) with
                        | IDENT _ -> begin
                            match self#peek_nth_rawtoken (nth''+2) with
                            | TY_LPAREN -> begin
                                let nth''', _ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth''+3) () in
                                match self#peek_nth_rawtoken (nth'''+1) with
                                | LBRACE | EQ -> begin
                                    conv_nth_token (function T.IDENT x,s,e -> T.IDENT_DM x,s,e | x -> x) nth1;
                                    true
                                end
                                | _ -> false
                            end
                            | _ -> false
                        end
                        | _ -> false
                    end

                    | _ -> false
                end
                | _ -> false
            end -> true

            | GNU_ATTR _ when begin
                let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+3) () in
                match self#peek_nth_rawtoken (nth'-1) with
                | SEMICOLON _ | RBRACE | NEWLINE -> false
                | _ -> true
            end -> true

            | LBRACKET when begin
                self#peek_nth_rawtoken (nth+2) == LBRACKET &&
                let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+3) () in
                match self#peek_nth_rawtoken (nth'-1) with
                | RBRACKET when self#peek_nth_rawtoken (nth'-2) == RBRACKET -> true
                | _ -> false
            end -> true

            | PP_IF | PP_IFDEF | PP_IFNDEF when begin
                let nth', _ = self#peek_rawtoken_up_to ~from:nth1 [T.NEWLINE] in
                match self#peek_nth_rawtoken (nth'+1) with
                | GNU_ATTR _ when begin
                    let nth'', l'' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth'+3) () in
                    match self#peek_nth_rawtoken (nth''-1) with
                    | SEMICOLON _ | RBRACE | NEWLINE -> false
                    | _ when begin
                        let nth' = self#peek_rawtoken_up_to_section_end ~from:(nth+2) () in
                        match self#peek_nth_rawtoken (nth'+1) with
                        | PP_ENDIF -> false
                        | _ -> true
                    end -> false
                    | _ -> true
                end -> true
                | _ -> false
            end -> true

            | _ -> false
        end -> DEBUG_MSG "@"; getd()

        | LBRACE when context == STMT && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IF -> false
            | _ ->
            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            match self#peek_nth_rawtoken (nth'-1) with
            | PTR_AMP_AMP -> true
            | _ -> false
        end -> DEBUG_MSG "LBRACE @"; geta()

        | LBRACE when context == STMT && begin
            (match self#peek_rawtoken() with
            | INT_LITERAL "0" -> true
            | _ -> false) &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | STATIC -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | VOID -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "LBRACE @"; getd()

        | LBRACE when context == STMT -> begin
            DEBUG_MSG "LBRACE @";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            begin
              match self#peek_nth (nth'-1) with
              | EQ, _, e -> insert_after_nth_token (nth'-1) [T.DUMMY_EXPR,e,e; T.SEMICOLON false,e,e];
              | _ -> ()
            end;
            token
        end

        | SEMICOLON _ when context == STMT && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | x when is_stmt_head x -> begin
                let ini_blv = env#brace_level in
                let open_count = ref 0 in
                let close_count = ref 0 in
                let blv = ref ini_blv in
                let min_blv = ref ini_blv in
                let filt (x : T.token) =
                  begin
                    match x with
                    | LBRACE | INI_LBRACE | CLASS_LBRACE -> begin
                        DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                        incr open_count;
                        incr blv
                    end
                    | RBRACE -> begin
                        DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                        incr close_count;
                        decr blv;
                        min_blv := min !blv !min_blv
                    end
                    | ELSE when !min_blv < ini_blv -> begin
                        DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                        incr min_blv
                    end
                    | _ -> ()
                  end;
                  false
                in
                let _(*nth', l'*) = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) ~filt () in
                DEBUG_MSG "brace_level: %d -> %d (min:%d)" ini_blv !blv !min_blv;
                DEBUG_MSG "open_count=%d close_count=%d" !open_count !close_count;
                ini_blv = !blv && ini_blv = !min_blv
            end
            | _ -> false
        end -> DEBUG_MSG "SEMICOLON @ CASE"; token

        | RPAREN | CONST when env#end_of_params_flag && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | LBRACE | SEMICOLON _ -> true
            | _ -> false
        end -> DEBUG_MSG "RPAREN @"; token

        | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | TYPEDEF
        | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | UNSIGNED | SIGNED when begin
            env#pp_ifx_d_flag &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> self#peek_nth_rawtoken (nth+2) == TY_LPAREN
            | _ -> false
        end -> DEBUG_MSG "(CHAR|...) @"; _gete()

        | PTR_STAR | PTR_AMP | PTR_AMP_AMP
        | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG | TYPEDEF
        | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | UNSIGNED | SIGNED(* | IDENT _*) -> begin
            DEBUG_MSG "(PTR_STAR|...) @";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in

            let ini_blv = env#brace_level in
            let open_count = ref 0 in
            let close_count = ref 0 in
            let blv = ref ini_blv in
            let min_blv = ref ini_blv in
            let filt (x : T.token) =
              begin
                match x with
                | LBRACE | INI_LBRACE | CLASS_LBRACE -> begin
                    DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                    incr open_count;
                    incr blv
                end
                | RBRACE -> begin
                    DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                    incr close_count;
                    decr blv;
                    min_blv := min !blv !min_blv
                end
                | ELSE when !min_blv < ini_blv -> begin
                    DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                    incr min_blv
                end
                | _ -> ()
              end;
              false
            in

            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) ~filt () in
            DEBUG_MSG "brace_level: %d -> %d (min:%d)" ini_blv !blv !min_blv;
            DEBUG_MSG "open_count=%d close_count=%d" !open_count !close_count;

            match self#peek_nth_rawtoken (nth'-1) with
            | LBRACE -> begin
                DEBUG_MSG "@";
                env#set_broken_flag();
                self#prepend_token token;
                env#exit_pp_if_section();
                mk T.SECTION_MARKER
            end
            | _ when !open_count - !close_count > 0 -> begin
                DEBUG_MSG "@";
                env#set_broken_flag();
                self#prepend_token token;
                env#exit_pp_if_section();
                mk T.SECTION_MARKER
            end
            | SEMICOLON _ when list_memqn [T.EQ] l' -> DEBUG_MSG "@"; geti()
            | SEMICOLON _ -> DEBUG_MSG "@"; _gete()
            | IDENT _ when begin
                let nth'', _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                match self#peek_nth_rawtoken (nth''+1) with
                | COMMA | SEMICOLON _ | RPAREN -> true
                | _ -> false
            end -> begin
              if list_memqn [T.EQ] l' then begin
                DEBUG_MSG "@";
                geti()
              end
              else begin
                DEBUG_MSG "@";
                _gete()
              end
            end
            (*| IDENT _ when list_memqn [T.EQ] l' -> DEBUG_MSG "@"; geti()
            | IDENT _ -> DEBUG_MSG "@"; _gete()*)
            | NEWLINE when begin
                list_memqn [T.EQ] l' &&
                let rt_opt = rev_skip_pp_l l' in
                match rt_opt with
                | Some SEMICOLON _ -> true
                | _ -> false
            end -> DEBUG_MSG "@"; geti()

            | RPAREN when begin
                (context == TOP || context == MEM) &&
                sub_context == END_OF_TY_SPEC &&
                match prev_rawtoken with
                | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> true
                | _ -> false
            end -> DEBUG_MSG "@"; _gete()

            | _ -> DEBUG_MSG "@"; getd()
        end

        | NEWLINE when begin
            env#pp_ifx_d_flag &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            match self#peek_nth_rawtoken (nth'-1) with
            | SEMICOLON _ | LBRACE -> false
            | _ -> true
        end -> DEBUG_MSG "NEWLINE @"; getd()

        | NEWLINE when begin
            prev_rawtoken2 == PP_ENDIF &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ when self#peek_nth_rawtoken (nth+2) == TY_LPAREN -> begin
                let nth_, ll_ = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+3) () in
                let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+3) () in
                nth_ = nth' - 1 &&
                match self#peek_nth_rawtoken (nth'-1) with
                | RPAREN -> begin
                    DEBUG_MSG "l'=%s" (String.concat ";" (List.map Token.rawtoken_to_string l'));
                    match (l' : T.token list) with
                    | RPAREN::RPAREN::_
                    | RPAREN::IDENT _::[]
                    | RPAREN::IDENT _::COMMA::_ -> false
                    | _ ->
                        let nth'' =
                          if self#peek_nth_rawtoken nth' == PP_ENDIF then
                            let n, _ = self#peek_rawtoken_up_to ~from:nth' [T.NEWLINE] in
                            n
                          else
                            self#peek_rawtoken_up_to_section_end ~from:(nth'+1) ()
                        in
                        let nth'', _ = self#peek_rawtoken_up_to ~from:nth'' [T.NEWLINE] in
                        match self#peek_nth_rawtoken (nth''+1) with
                        | LBRACE | EQ -> true
                        | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "NEWLINE @"; getd()

        | NEWLINE when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | x when is_pp_control_line x -> begin
                let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+2) () in
                match self#peek_nth_rawtoken (nth'-1) with
                | LBRACE -> begin
                    match self#peek_nth_rawtoken (nth'-2) with
                    | RPAREN -> true
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "NEWLINE @"; getd()

        | COMMA | NEWLINE when begin
            context == MEM_INIT && sub_context == INI &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> begin
                let ini_plv = 0 in
                let plv = ref ini_plv in
                let min_plv = ref ini_plv in
                let filt (x : T.token) =
                  begin
                    match x with
                    | TY_LPAREN -> incr plv
                    | RPAREN -> begin
                        decr plv;
                        min_plv := min !plv !min_plv
                    end
                    | _ -> ()
                  end;
                  false
                in
                let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+2) ~filt () in
                !plv > 0 &&
                match self#peek_nth_rawtoken (nth'-1) with
                | COMMA -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "(COMMA|NEWLINE) @"; gete()

        | IDENT _ | DECL_MACRO _ when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            (match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | TY_LPAREN -> true
                | _ -> false
            end
            | _ -> false) &&
            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+2) () in
            match self#peek_nth_rawtoken (nth'-1) with
            | RPAREN | SEMICOLON _ -> begin
                let nth'' = self#peek_rawtoken_up_to_section_end ~from:(nth'+1) () in
                match self#peek_nth_rawtoken (nth''+1) with
                | LBRACE -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; _gete()

        | IDENT _ | DECL_MACRO _ when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            (match self#peek_nth_rawtoken (nth+1) with
            | INLINE -> true
            | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
            | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | UNSIGNED | SIGNED when begin
                match prev_rawtoken with
                | IDENT _ -> true
                | _ -> false
            end -> true
            | _ -> false) &&
            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+2) () in
            match self#peek_nth_rawtoken (nth'-1) with
            | SEMICOLON _ | RBRACE | NEWLINE -> false
            | _ -> true
        end -> DEBUG_MSG "@"; getd()

        | RPAREN when begin
            (context == TOP || context == MEM) && env#end_of_id_macro_call_flag &&
            let nth, _ = self#peek_rawtoken_up_to ~skip_pp_control_line:true [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | TY_LPAREN -> true
            | _ -> false
        end -> DEBUG_MSG "@"; _gete()

        | RPAREN when begin
            (context == TOP || context == MEM) && (sub_context == END_OF_ID_EXPR || env#end_of_params_flag) &&
            let nth, _ = self#peek_rawtoken_up_to ~skip_pp_control_line:true [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | COLON | SEMICOLON _ -> false
            | _ -> true
        end -> DEBUG_MSG "@"; getd()

        | RPAREN when begin
            env#end_of_params_flag &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | GNU_ATTR _ -> true
            | _ -> false
        end -> DEBUG_MSG "@"; getd()

        | RPAREN when begin
            (*env#expr_flag ||*)
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | MINUS_GT | DOT when context == EXPR -> true
            | _ -> false
        end -> DEBUG_MSG "@"; gete()

        (*| PTR_AMP_AMP when env#paren_level > 0 -> geta()*)
        (*| BAR_BAR _ when env#paren_level > 0 -> DEBUG_MSG "@"; geto()*)
        | COMMA | STR_LITERAL _ when env#paren_level > 0 && begin
            let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
            let open_count, close_count = count_parens l in
            DEBUG_MSG "open_count=%d, close_count=%d" open_count close_count;
            close_count - open_count = 1
        end -> DEBUG_MSG "@"; getcl()

        | COMMA when env#dtor_flag -> begin
            DEBUG_MSG "@";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            if list_memqn [T.EQ] l || not (list_memqn [T.TY_LPAREN;LPAREN;RPAREN] l) then begin
              DEBUG_MSG "@";
              geti()
            end
            else begin
              DEBUG_MSG "@";
              getd()
            end
        end

        | STR_LITERAL _ when not env#asm_flag -> DEBUG_MSG "@"; gets()

        | COMMA | NEWLINE when begin
            not env#asm_flag && (env#braced_init_flag || env#arg_paren_flag) &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let plv = ref 0 in
            let filt (x : T.token) =
              begin
                match x with
                | TY_LPAREN | LPAREN -> incr plv
                | RPAREN -> decr plv
                | _ -> ()
              end;
              false
            in
            let _ = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) ~filt () in
            !plv = 0
        end -> begin
          DEBUG_MSG "@";
          let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
          let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
          match l with
          | (T.BAR _|COLON)::_ -> DEBUG_MSG "@"; gete()
          | T.SEMICOLON _::_ -> DEBUG_MSG "@"; token
          | _ -> DEBUG_MSG "@"; geti()
        end

        | IDENT_V _ when prev_rawtoken2 == QUEST -> DEBUG_MSG "@"; gete()

        | IDENT_V _ when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            (match self#peek_nth_rawtoken (nth+1) with
            | GNU_ATTR _ -> true
            | _ -> false) &&
            let nth', l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            let nth'', _ = self#peek_rawtoken_up_to ~from:(nth'+1) [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth''+1) with
            | EQ -> true
            | _ -> false
        end -> DEBUG_MSG "@"; getattr()

        (*| _ when begin
            prev_rawtoken != NEWLINE &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | PP_ENDIF -> begin
                conv_nth_token (function T.PP_ENDIF,s,e -> T.PP_ODD_ENDIF,s,e | x -> x) (nth+1);
                true
            end
            | _ -> false
        end -> DEBUG_MSG "@"; _getodd()*)

        | _ when begin
            env#class_name_flag &&
            match prev_rawtoken with
            | SEMICOLON _ | RBRACE | COMMA | BASE_COLON | BAR_BAR _ | AMP_AMP _ -> false
            | NEWLINE when begin
                prev_rawtoken2 == PP_ENDIF && prev_rawtoken3 == COMMA
            end -> false
            | MARKER when env#type_paren_flag -> false
            | _ -> begin
                let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
                match self#peek_nth_rawtoken (nth+1) with
                | COLON | COMMA -> false
                | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                    let nth', _ = self#peek_rawtoken_up_to ~from:(nth+1) [T.NEWLINE] in
                    match self#peek_nth_rawtoken (nth'+1) with
                    | COLON | COMMA -> false
                    | _ -> true
                end
                | _ -> true
            end
        end -> begin
          DEBUG_MSG "@";
          env#clear_class_name_flag();
          getcb()
        end

        | _ when not env#asm_flag && prev_rawtoken == EXTERN && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            match l with
            | [] -> false
            | _ ->
                List.for_all
                  (function
                    | T.STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ -> true
                    | _ -> false
                  ) l
        end -> DEBUG_MSG "@"; getd()

        | _ when not env#asm_flag && prev_rawtoken != EXTERN && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
            match l with
            | [] -> false
            | _ ->
                List.for_all
                  (function
                    | T.STR_LITERAL _ | USER_STR_LITERAL _ | STR_MACRO _ -> true
                    | _ -> false
                  ) l
        end -> DEBUG_MSG "@"; gets()

        | _ when not env#asm_flag && env#paren_level > 0 -> begin
            DEBUG_MSG "@";
            let plv = ref 0 in
            let min_plv = ref 0 in
            let blv = ref 0 in
            let min_blv = ref 0 in
            let filt = function
              | T.LPAREN | TY_LPAREN -> incr plv; false
              | T.RPAREN -> begin
                  decr plv;
                  if !plv < !min_plv then
                    min_plv := !plv;
                  false
              end
              | T.LBRACE -> incr blv; false
              | T.RBRACE -> begin
                  decr blv;
                  if !blv < !min_blv then
                    min_blv := !blv;
                  false
              end
              | _ -> false
            in
            (*let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in*)
            let nth = self#skip_pp 1 in
            DEBUG_MSG "nth=%d" nth;

            DEBUG_MSG "(%d+1)-th token -> %s"
              nth (Token.rawtoken_to_string (self#peek_nth_rawtoken (nth+1)));

            let nth_, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~filt () ~from:(nth+1) in
            DEBUG_MSG "nth_=%d" nth_;

            let rev_l = List.rev l in
            DEBUG_MSG "rev_l=%s" (String.concat ";" (List.map Token.rawtoken_to_string rev_l));

            DEBUG_MSG "min_plv=%d min_blv=%d" !min_plv !min_blv;
            DEBUG_MSG "plv=%d blv=%d" !plv !blv;

            if !min_plv < -1 then begin

              let common_suffix, nth_list, sect_end_nth, __mk = get_common_suffix ~rparen_only:true nth_ l in

              if sect_end_nth > 0 then begin
                let len = List.length common_suffix in

                DEBUG_MSG "common_suffix=%s (%d)"
                  (String.concat ";" (List.map Token.rawtoken_to_string common_suffix)) len;

                let to_be_discarded = ref [] in
                List.iter
                  (fun nth ->
                    to_be_discarded := nth :: !to_be_discarded;
                    for i = 1 to len - 1 do
                      to_be_discarded := (nth - i) :: !to_be_discarded
                    done
                  ) nth_list;
                let to_be_discarded = !to_be_discarded in
                DEBUG_MSG "to_be_discarded=[%s]" (String.concat ";" (List.map string_of_int to_be_discarded));
                insert_after_nth_token ~to_be_discarded sect_end_nth (List.map __mk common_suffix);
                List.iter
                  (function
                    | T.RPAREN -> incr min_plv
                    | _ -> ()
                  ) common_suffix;
                DEBUG_MSG "min_plv -> %d" !min_plv
              end
            end;

            if !min_plv < 0 then begin
              DEBUG_MSG "@";
              let closing =
                match prev_rawtoken with
                | AMP_AMP_BROKEN _ | BAR_BAR_BROKEN _ | LPAREN -> false
                | _ -> true
              in
              gete ~closing ()
            end
            else if
              match context, sub_context with
              | STMT, START_OF_STMT _ -> true
              | _ -> false
            then begin
              DEBUG_MSG "@";
              token
            end
            else
            match (l : T.token list) with
            | RBRACKET::_ when not env#type_paren_flag -> DEBUG_MSG "@"; token
            | (PTR_AMP|BAR _|COLON|LT_LT|GT_GT)::_ -> DEBUG_MSG "@"; gete()
            | PTR_AMP_AMP::_ -> DEBUG_MSG "@"; geta()
            | BAR_BAR _::_ -> DEBUG_MSG "@"; geto()
            | _ -> begin
                DEBUG_MSG "@";
                let head_opt = ref None in
                match (rev_l : T.token list) with
                | (AMP _|BAR _|LT_LT|GT_GT)::_ -> DEBUG_MSG "@"; gete()
                | PTR_AMP_AMP::_ -> DEBUG_MSG "@"; geta()
                | BAR_BAR _::_ -> DEBUG_MSG "@"; geto()
                | (PP_IF|PP_IFDEF|PP_IFNDEF)::_ when begin
                    match skip_pp_l rev_l with
                    | Some (PTR_AMP_AMP|BAR_BAR _) as h_opt -> head_opt := h_opt; true
                    | _ -> false
                end -> begin
                  DEBUG_MSG "@";
                  match !head_opt with
                  | Some PTR_AMP_AMP -> geta()
                  | Some (BAR_BAR _) -> geto()
                  | _ -> assert false
                end
                | _ ->
                    DEBUG_MSG "@";
                    if env#arg_paren_flag && context == EXPR then begin
                      DEBUG_MSG "@";
                      geti()
                    end
                    else if
                      env#_arg_paren_flag &&
                      context == EXPR &&
                      let nth, _ = self#peek_rawtoken_up_to ~from:2 [T.NEWLINE] in
                      match self#peek_nth_rawtoken (nth+1) with
                      | COMMA -> true
                      | _ -> false
                    then begin
                      DEBUG_MSG "@";
                      geti()
                    end
                    else if
                      env#stack#in_params &&
                      match prev_rawtoken with
                      | MARKER when prev_rawtoken2 == NEWLINE && prev_rawtoken3 == PP_ENDIF -> true
                      | TY_LPAREN | COMMA
                      | IDENT_V _ | PTR_STAR | PTR_AMP | PTR_AMP_AMP | PARAM_DECL_MACRO _ -> true
                      | NEWLINE -> false
                      | _ when begin
                          let nth, _ = self#peek_rawtoken_up_to ~from:2 [T.NEWLINE] in
                          match self#peek_nth_rawtoken (nth+1) with
                          | COMMA -> true
                          | _ -> false
                      end -> true
                      | _ -> false
                    then begin
                      DEBUG_MSG "@";
                      match remove_pp_head rev_l with
                      | COMMA::_ -> begin
                          DEBUG_MSG "@";
                          self#prepend_token (gete());
                          env#exit_pp_if_section();
                          mk T.MARKER
                      end
                      | _::_ when List.hd l == COMMA -> begin
                          DEBUG_MSG "@";
                          let nth' =
                            if self#peek_nth_rawtoken nth_ == PP_ENDIF then
                              let n, _ = self#peek_rawtoken_up_to ~from:nth_ [T.NEWLINE] in
                              n
                            else
                              self#peek_rawtoken_up_to_section_end ~from:(nth_+1) ()
                          in
                          let rt, _, e = self#peek_nth (nth'-1) in
                          DEBUG_MSG "rt=%s" (Token.rawtoken_to_string rt);
                          insert_after_nth_token nth' [T.MARKER, e, e];
                          gete()
                      end
                      | [IDENT _] when begin
                          match prev_rawtoken with
                          | PTR_STAR | PTR_AMP | PTR_AMP_AMP -> false
                          | _ ->
                          let nth' =
                            if self#peek_nth_rawtoken nth_ == PP_ENDIF then
                              let n, _ = self#peek_rawtoken_up_to ~from:nth_ [T.NEWLINE] in
                              n
                            else
                              self#peek_rawtoken_up_to_section_end ~from:(nth_+1) ()
                          in
                          match self#peek_nth_rawtoken (nth'+1) with
                          | IDENT _ -> begin
                              match self#peek_nth_rawtoken (nth'+2) with
                              | COMMA | RPAREN -> true
                              | _ -> false
                          end
                          | COMMA when begin
                              match self#peek_nth_rawtoken (nth'+2) with
                              | IDENT x when is_type_name x || is_type x -> true
                              | x when is_basic_ty x -> true
                              | _ -> false
                          end -> true
                          | _ -> false
                      end -> DEBUG_MSG "@"; getd()
                      | _ -> begin
                          DEBUG_MSG "@";
                          gete()
                      end
                    end
                    else if
                      let nth' =
                        if self#peek_nth_rawtoken nth_ == PP_ENDIF then
                          let n, _ = self#peek_rawtoken_up_to ~from:nth_ [T.NEWLINE] in
                          n
                        else
                          self#peek_rawtoken_up_to_section_end ~from:(nth_+1) ()
                      in
                      match try self#peek_nth_rawtoken (nth'+1) with _ -> EOF with
                      | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                          let nth'', _ = self#peek_rawtoken_up_to ~from:(nth'+2) [T.NEWLINE] in
                          match self#peek_nth_rawtoken (nth''+1) with
                          | BAR_BAR _ -> true
                          | _ -> false
                      end
                      | _ -> false
                    then begin
                      DEBUG_MSG "@";
                      geto()
                    end
                    else begin
                      DEBUG_MSG "@";
                      gete()
                    end
            end
        end

        | EQ when begin
            begin
              match prev_rawtoken2 with
              | IDENT_E _ -> true
              | _ -> false
            end ||
            try
              let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
              let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
              match l with
              | COLON::_ -> true
              | SEMICOLON _::x::_ when is_literal x -> true
              | RPAREN::_ when context == EXPR -> true
              | _ -> begin
                  match List.rev l with
                  | STR_LITERAL _::_ -> true
                  | _ when not env#alias_flag && not env#ty_param_flag -> true
                  | _ -> false
              end
            with
              Abort -> false
        end -> begin
          DEBUG_MSG "@";
          let nth = self#peek_rawtoken_up_to_section_end() in
          match self#peek_nth_rawtoken (nth+1) with
          | DOT | MINUS_GT -> geta()
          | _ ->
              let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
              let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
              match (l : T.token list) with
              | PTR_AMP_AMP::_ -> geta()
              | _ -> gete()
        end

        | EQ when env#ty_param_flag -> DEBUG_MSG "@"; getd()

        | COMMA when env#brace_level > 0 && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | COLON -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; gete()

        | COMMA when env#braced_init_flag && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ when begin
                match self#peek_nth_rawtoken (nth+2) with
                | TY_LPAREN -> begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | DOT -> true
                    | _ -> false
                end
                | _ -> false
            end -> true
            | x -> is_literal x
        end -> DEBUG_MSG "@"; _gete()

        | COMMA when env#top_stmts_flag && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | COMMA -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; _gete()

        | NEWLINE when begin
            prev_rawtoken2 == PP_ENDIF && prev_rawtoken3 == COMMA && env#braced_init_flag &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | x -> is_literal x
        end -> begin
          DEBUG_MSG "@";
          self#prepend_token (_gete());
          mk T.MARKER
        end

        | NEWLINE when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | IDENT _ -> begin
                match self#peek_nth_rawtoken (nth+2) with
                | LBRACKET -> begin
                    match self#peek_nth_rawtoken (nth+3) with
                    | RBRACKET -> begin
                        match self#peek_nth_rawtoken (nth+4) with
                        | EQ -> true
                        | _ -> false
                    end
                    | _ -> false
                end
                | _ -> false
            end
            | _ -> false
        end -> begin
          DEBUG_MSG "@";
          geti()
        end

        | NEWLINE when begin
            prev_rawtoken2 == PP_ENDIF &&
            match prev_rawtoken3 with
            | BAR _ -> true
            | _ -> false
        end -> DEBUG_MSG "@"; _gete()

        | IDENT_V _ when context == TOP || context == MEM && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | TY_LPAREN -> begin
                let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
                match l with
                | RBRACE::_ -> true
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; gete()

        | LT_LT | GT_GT -> DEBUG_MSG "@"; gete()

        | _ when begin
            env#braced_init_flag &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | DOT when prev_rawtoken == COMMA -> true
            | LBRACE -> context == EXPR
            | _ -> false
        end -> DEBUG_MSG "@"; geti()

        | _ when begin
            env#brace_level > 0 &&
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | LBRACKET when self#peek_nth_rawtoken (nth+2) == LBRACKET -> false
            | DOT | LBRACKET | BAR _ -> true
            | COLON -> env#in_body_brace_flag
            | LBRACE -> context == EXPR
            | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                let nth', _ = self#peek_rawtoken_up_to ~from:(nth+2) [T.NEWLINE] in
                match self#peek_nth_rawtoken (nth'+1) with
                | DOT | BAR _ -> true
                | LBRACKET when self#peek_nth_rawtoken (nth'+2) != LBRACKET -> true
                | LBRACE -> context == EXPR
                | _ -> false
            end
            | _ -> false
        end -> DEBUG_MSG "@"; gete()

        | _ when env#expr_flag && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | COLON -> true
            | _ -> false
        end -> gete()

        | _ when env#top_stmts_flag && begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | RBRACE -> true
            | _ -> false
        end -> begin
            DEBUG_MSG "@";
            self#prepend_token token;
            mk T.END_STMTS
        end

        | _ when env#brace_level > 0 -> begin
            let ini_plv = env#paren_level in
            let ini_blv = env#brace_level in
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let open_count = ref 0 in
            let close_count = ref 0 in
            let plv = ref ini_plv in
            let min_plv = ref ini_plv in
            let blv = ref ini_blv in
            let min_blv = ref ini_blv in
            let filt (x : T.token) =
              begin
                match x with
                | TY_LPAREN -> incr plv
                | RPAREN -> begin
                    decr plv;
                    min_plv := min !plv !min_plv
                end
                | LBRACE | INI_LBRACE | CLASS_LBRACE -> begin
                    DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                    incr open_count;
                    incr blv
                end
                | RBRACE -> begin
                    DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                    incr close_count;
                    decr blv;
                    min_blv := min !blv !min_blv
                end
                | ELSE when !min_blv < ini_blv -> begin
                    DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                    incr min_blv
                end
                | _ -> ()
              end;
              false
            in
            let nth_, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) ~filt () in
            DEBUG_MSG "paren_level: %d -> %d (min:%d)" ini_plv !plv !min_plv;
            DEBUG_MSG "brace_level: %d -> %d (min:%d)" ini_blv !blv !min_blv;
            DEBUG_MSG "open_count=%d close_count=%d" !open_count !close_count;

            if !plv > ini_plv && is_semicolon (self#peek_nth_rawtoken (nth_-1)) then begin
              let nth', _ = self#peek_rawtoken_up_to ~from:nth_ [T.NEWLINE] in
              let plv' = ref ini_plv in
              let min_plv' = ref ini_plv in
              let filt (x : T.token) =
                begin
                  match x with
                  | TY_LPAREN -> incr plv'
                  | RPAREN -> begin
                      decr plv';
                      min_plv' := min !plv' !min_plv'
                  end
                  | _ -> ()
                end;
                false
              in
              let nth'_, l' = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth'+1) ~filt () in
              DEBUG_MSG "paren_level': %d -> %d (min':%d)" ini_plv !plv' !min_plv';
              if !plv' = ini_plv && is_semicolon (self#peek_nth_rawtoken (nth'_-1)) then begin
                let rt, _, e = self#peek_nth (nth_-2) in
                DEBUG_MSG "rt=%s" (Token.rawtoken_to_string rt);
                let n = !plv - ini_plv in
                DEBUG_MSG "lack of %d closing paren(s)" n;
                let rl = ref [] in
                for i = 1 to n do
                  rl := (T.RPAREN, e, e)::!rl
                done;
                insert_after_nth_token (nth_-2) !rl;
              end
            end;

            let odd_class_head_flag =
              !open_count - !close_count > 0 &&
              match (l : T.token list) with
              | LBRACE::IDENT _::(CLASS|STRUCT|UNION)::_ -> true
              | _ -> false
            in
            DEBUG_MSG "odd_class_head_flag=%B" odd_class_head_flag;

            let odd_enum_head_flag =
              !open_count - !close_count > 0 &&
              match (l : T.token list) with
              | LBRACE::IDENT _::ENUM::_ -> true
              | _ -> false
            in
            DEBUG_MSG "odd_enum_flag=%B" odd_enum_head_flag;

            if odd_enum_head_flag || odd_class_head_flag then begin
              let common_suffix, nth_list, sect_end_nth, __mk = get_common_suffix ~lbrace_only:true nth_ l in
              let len = List.length common_suffix in
              DEBUG_MSG "common_suffix=%s (%d)"
                (String.concat ";" (List.map Token.rawtoken_to_string common_suffix)) len;

              let to_be_discarded = ref [] in
              List.iter
                (fun nth ->
                  to_be_discarded := nth :: !to_be_discarded;
                  for i = 1 to len - 1 do
                    to_be_discarded := (nth - i) :: !to_be_discarded
                  done
                ) nth_list;
              let to_be_discarded = !to_be_discarded in
              DEBUG_MSG "to_be_discarded=[%s]" (String.concat ";" (List.map string_of_int to_be_discarded));
              insert_after_nth_token ~to_be_discarded sect_end_nth (List.map __mk common_suffix);
              List.iter
                (function
                  | T.LBRACE -> decr open_count
                  | _ -> ()
                ) common_suffix;
              DEBUG_MSG "open_count -> %d" !open_count
            end;

            if !close_count - !open_count > 0 then begin
              DEBUG_MSG "@";
              if
                env#odd_brace_level > 0 &&
                match self#peek_nth_rawtoken nth_ with
                | PP_ENDIF -> true
                | PP_ELIF | PP_ELSE -> begin
                    let open_count2 = ref 0 in
                    let close_count2 = ref 0 in
                    let blv2 = ref ini_blv in
                    let min_blv2 = ref ini_blv in
                    let filt (x : T.token) =
                      begin
                        match x with
                        | LBRACE | INI_LBRACE | CLASS_LBRACE -> begin
                            DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                            incr open_count2;
                            incr blv2
                        end
                        | RBRACE -> begin
                            DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                            incr close_count2;
                            decr blv2;
                            min_blv2 := min !blv2 !min_blv2
                        end
                        | ELSE when !min_blv2 < ini_blv -> begin
                            DEBUG_MSG "%s" (Token.rawtoken_to_string x);
                            incr min_blv2
                        end
                        | _ -> ()
                      end;
                      false
                    in
                    let _ = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth_+1) ~filt () in
                    !close_count2 - !open_count2 <= 0
                end
                | _ -> true
              then begin
                DEBUG_MSG "@";
                getodd()
              end
              else begin
                DEBUG_MSG "@";
                getcl()
              end
            end
            else if env#odd_brace_level = 0 && !blv = ini_blv && !min_blv < ini_blv then begin
              DEBUG_MSG "@";
              getodd()
            end
            else if
              match prev_rawtoken with
              | AMP_AMP _ -> true
              | _ -> false
            then begin
              DEBUG_MSG "@"; geta()
            end
            else if odd_class_head_flag then begin
              DEBUG_MSG "@"; getc()
            end
            else if odd_enum_head_flag then begin
              DEBUG_MSG "@"; geteh()
            end
            else begin
              DEBUG_MSG "@";
              (*DEBUG_MSG "l=%s" (String.concat ";" (List.map Token.rawtoken_to_string l));*)
              match l with
              | [] -> DEBUG_MSG "@"; token

              | [SEMICOLON _;RPAREN] -> DEBUG_MSG "@"; getcl()

              | [x] when is_literal x && not env#templ_arg_flag -> DEBUG_MSG "@"; _gete()

              | COLON::_ when prev_rawtoken == COLON && begin
                  match prev_rawtoken2 with
                  | CASE | IDENT _ | PUBLIC | PRIVATE | PROTECTED -> false
                  | _ ->
                      match self#peek_nth_rawtoken (nth+1) with
                      | CASE -> false
                      | _ -> true
              end -> DEBUG_MSG "@"; _gete()

              | [COMMA;IDENT _] when env#braced_init_flag -> DEBUG_MSG "@"; gete()

              | [COMMA;IDENT _;(PTR_STAR|PTR_AMP|PTR_AMP_AMP)] when env#braced_init_flag -> DEBUG_MSG "@"; gete()

              | (BAR _|AMP _)::_ -> DEBUG_MSG "@"; gete()

              | EQ::IDENT _::[] when begin
                  match context, sub_context with
                  | STMT, START_OF_STMT _ -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; geta()

              | EQ::_::_ when begin
                  match context, sub_context with
                  | STMT, START_OF_STMT _ -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; token

              | (LT_LT|EQ|PLUS_EQ|MINUS_EQ|STAR_EQ|SLASH_EQ|PERC_EQ|
                LT_LT_EQ|GT_GT_EQ|AMP_EQ _|HAT_EQ _|BAR_EQ _)::_ -> DEBUG_MSG "@"; geta()

              | _ when env#asm_flag -> DEBUG_MSG "@"; gete()

              | _ when begin
                  match prev_rawtoken with
                  | BAR_BAR _ -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; geto()

              | BAR_BAR _::_ when Xlist.last l != IF -> DEBUG_MSG "@"; geto()

              | _ when begin
                  prev_rawtoken == NEWLINE &&
                  prev_rawtoken2 == PP_ELSE &&
                  try
                    (env#pp_if_section_nth_info 2).Pinfo.i_label == InitDeclarator
                  with
                    _ -> false
              end -> DEBUG_MSG "@"; geti()

              | RPAREN::_ when begin
                  env#braced_init_flag &&
                  match Xlist.last l with
                  | IDENT _ -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; geti()

              | _ -> begin
                  DEBUG_MSG "l=%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
                  match Xlist.last l with
                  | EQ when sub_context == END_OF_TY_SPEC -> DEBUG_MSG "@"; geti()

                  | LT_LT | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
                  | LT_LT_EQ | GT_GT_EQ | AMP_EQ _ | HAT_EQ _ | BAR_EQ _
                  | PLUS | MINUS(* | PTR_STAR*) | SLASH | PERC -> DEBUG_MSG "@"; gete()

                  | PTR_AMP_AMP -> DEBUG_MSG "@"; geta()

                  | BAR_BAR _ -> DEBUG_MSG "@"; geto()

                  | COMMA when env#braced_init_flag || env#arg_paren_flag -> DEBUG_MSG "@"; geti()

                  | _ when env#braced_init_flag || env#end_of_cast_type_flag -> DEBUG_MSG "@"; gete()

                  | PP_DEFINE | PP_UNDEF when begin
                      let n = self#skip_pp 1 in
                      DEBUG_MSG "n=%d" n;
                      match self#peek_nth_rawtoken (n+1) with
                      | PP_ENDIF -> begin
                          let n', _ = self#peek_rawtoken_up_to ~from:(n+1) [T.NEWLINE] in
                          match self#peek_nth_rawtoken (n'+1) with
                          | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
                              let n'', _ = self#peek_rawtoken_up_to ~from:(n'+2) [T.NEWLINE] in
                              match self#peek_nth_rawtoken (n''+1) with
                              | BAR _ -> true
                              | rt -> DEBUG_MSG "rt=%s" (Token.rawtoken_to_string rt); false
                          end
                          | rt -> DEBUG_MSG "rt=%s" (Token.rawtoken_to_string rt); false
                      end
                      | rt -> DEBUG_MSG "rt=%s" (Token.rawtoken_to_string rt); false
                  end -> DEBUG_MSG "@"; getb()

                  | _ -> begin
                      DEBUG_MSG "@";
                      begin
                        let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
                        match self#peek_nth_rawtoken (nth+1) with
                        | COLON_COLON -> begin
                            match prev_rawtoken with
                            | COMMA -> begin
                                conv_nth_token
                                  (function T.COLON_COLON,s,e -> T.HEAD_COLON_COLON,s,e | x -> x) (nth+1);
                            end
                            | _ -> ()
                        end
                        | _ -> ()
                      end;
                      token
                  end
              end
            end
        end

        | _ when env#asm_flag -> DEBUG_MSG "@"; gete()

        | _ when env#top_stmts_flag && prev_rawtoken == COMMA -> DEBUG_MSG "@"; gete()

        | _ when begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | EQ when sub_context == END_OF_ID_EXPR -> true
            | _ -> false
        end -> DEBUG_MSG "@"; geti()

        | _ -> DEBUG_MSG "@"; token
      end
      else begin
        DEBUG_MSG "@";
        token
      end
  end

  | PP_ELIF when env#pp_odd_flag -> DEBUG_MSG "@"; mk T.PP_ODD_ELIF
  | PP_ELSE when env#pp_odd_flag -> DEBUG_MSG "@"; mk T.PP_ODD_ELSE

  | PP_ELSE when begin
      let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
      match self#peek_nth_rawtoken (nth+1) with
      | PP_ENDIF -> true
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.PP_ODD_ELSE

  | PP_ELSE | PP_ELIF when not macro_body_parsing_flag && env#end_of_if_head_flag -> begin
      DEBUG_MSG "complementing if-statement with dummy then-part";
      self#prepend_token token;
      mk T.DUMMY_STMT
  end

  | PP_ELSE | PP_ELIF when begin
      DEBUG_MSG "@";
      (env#braced_init_level > 0 && (prev_rawtoken != LBRACE || prev_rawtoken2 != EQ || prev_rawtoken3 != MARKER) ||
      env#stack#block_level > 1 ||
      env#body_head_flag ||
      env#stack#at_class && not (env#get_broken_info()) ||
      env#stack#at_namespace) &&
      env#pp_if_section_rel_brace_level > 0
  end -> begin
    if env#get_broken_info() then begin
      DEBUG_MSG "@";
      token
    end
    else begin
      DEBUG_MSG "@";
      let nb = env#pp_if_section_rel_brace_level in
      DEBUG_MSG "nb=%d" nb;
      if not env#pp_else_flag && not env#pp_elif_flag then begin
        DEBUG_MSG "@";
        let c = env#get_pp_if_compl_info() in
        c.Pinfo.c_brace <- c.Pinfo.c_brace + nb
      end;
      self#prepend_token token;
      if env#stack#block_level = 0 then
        self#prepend_token (mk (T.SEMICOLON false));
      DEBUG_MSG "complementing with %d closing brace(s)" nb;
      for i = 2 to nb do
        self#prepend_token (mk T.RBRACE)
      done;
      mk T.RBRACE
    end
  end

  | PP_ELIF | PP_ELSE | PP_ENDIF when begin
      env#lambda_dtor_flag && env#trailing_retty_flag &&
      is_ty prev_rawtoken && prev_rawtoken2 == MINUS_GT
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token token;
    mk T.DUMMY_STMT
  end

  | PP_ELSE | PP_ELIF -> begin
      match prev_rawtoken with
      | SEMICOLON _ -> token

      (*| NEWLINE when env#pp_top_label == InitDeclarator -> token*)

      | _ when env#get_semicolon_info() -> begin
          DEBUG_MSG "complementing with a semicolon";
          self#prepend_token token;
          _mk (T.SEMICOLON false)
      end

      | _ -> begin
          if context == TOP && sub_context == INI then begin
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | DEFAULT -> begin
                DEBUG_MSG "@";
                insert_after_nth_token nth [mk T.BEGIN_STMTS];
            end
            | _ -> ()
          end;
          token
      end
  end

  | PP_ENDIF when not macro_body_parsing_flag && env#end_of_if_head_flag -> begin
      DEBUG_MSG "complementing if-statement with dummy then-part";
      self#prepend_token token;
      mk T.DUMMY_STMT
  end

  | PP_ENDIF when begin
      not macro_body_parsing_flag &&
      context == STMT &&
      match prev_rawtoken with
      | COLON -> begin
          match prev_rawtoken2 with
          | IDENT _ -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "complementing with dummy-statement";
    self#prepend_token token;
    mk T.DUMMY_STMT
  end

  | PP_ENDIF when begin
      DEBUG_MSG "stack#block_level=%d pp_if_section_rel_brace_level=%d alt_pp_branch_flag=%B"
        env#stack#block_level env#pp_if_section_rel_brace_level env#alt_pp_branch_flag;

      (env#stack#block_level > 1 ||
      env#body_head_flag ||
      env#stack#at_class && not (env#get_broken_info()) ||
      env#stack#at_namespace) &&

      ((try env#pp_if_section_rel_brace_level > 0 with _ -> env#brace_level > 0) ||
      (env#get_pp_if_compl_info()).Pinfo.c_brace > 0) &&

      env#alt_pp_branch_flag
  end -> begin
    DEBUG_MSG "@";
    match context with
    | TOP when prev_rawtoken == LBRACE -> begin
        insert_after_nth_token 1 [mk T.MARKER];
        DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
        env#exit_pp_if_section();
        DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
        token
    end
    | _ ->
        if env#get_broken_info() then begin
          DEBUG_MSG "@";
          let info = env#pp_if_section_top_info in
          DEBUG_MSG "info=%s" (Pinfo.pp_if_section_info_to_string info);
          if self#peek_nth_rawtoken 2 != PP_ENDIF && not info.Pinfo.i_func_head then begin
            if env#paren_level > info.Pinfo.i_paren_level then
              insert_after_nth_token 1 [mk T.MARKER; mk T.MARKER]
            else
              insert_after_nth_token 1 [mk T.MARKER];
          end;
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#exit_pp_if_section();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          token
        end
        else begin
          let compl_flag = env#pp_if_section_rel_brace_level = 0 in
          DEBUG_MSG "compl_flag=%B" compl_flag;
          let lv =
            if compl_flag then begin
              let c = env#get_pp_if_compl_info() in
              c.Pinfo.c_brace
            end
            else
              env#pp_if_section_rel_brace_level - env#pp_if_section_top_info.Pinfo.i_rbraces
          in
          DEBUG_MSG "lv=%d" lv;
          env#reset_pp_if_compl_info();

          let stmt_flag =
            not env#body_head_flag &&
            env#pp_else_flag && env#in_body_brace_flag && env#stack#block_level = lv
          in
          DEBUG_MSG "stmt_flag=%B" stmt_flag;

          if
            match self#peek_nth_rawtoken 2 with
            | PP_ELIF | PP_ELSE | PP_ENDIF -> false
            | _ -> true
          then begin
            let _ = self#discard_token() in

            if stmt_flag then
              self#prepend_token (mk T.BEGIN_STMTS)
            else
            for i = 1 to lv do
              self#prepend_token (mk T.LBRACE)
            done;

            if lv = 1 && env#stack#at_namespace then
              self#prepend_token (mk T.NAMESPACE);

            if lv > 0 && not stmt_flag then
              self#prepend_token (mk (T.BRACE_LEVEL lv));

            self#prepend_token (mk T.NEWLINE);

            if compl_flag then begin
              self#prepend_token token;
              for i = 1 to lv do
                self#prepend_token (mk T.RBRACE)
              done;
              for i = 2 to lv do
                self#prepend_token (mk T.LBRACE)
              done;
              mk T.LBRACE
            end
            else begin
              DEBUG_MSG "complementing with %d closing brace(s)" lv;
              self#prepend_token token;

              if stmt_flag then begin
                env#clear_semicolon_info();
              end;

              for i = 2 to lv do
                env#incr_lbrace_info();
                self#prepend_token (mk T.RBRACE)
              done;
              env#incr_lbrace_info();
              mk T.RBRACE
            end
          end
          else begin
            DEBUG_MSG "complementing with closing brace(s)";
            self#prepend_token token;
            for i = 2 to lv do
              env#incr_lbrace_info();
              self#prepend_token (mk T.RBRACE)
            done;
            env#incr_lbrace_info();
            mk T.RBRACE
          end
        end
  end

  | COMMA when begin
      check_top_stmts_flag &&
      env#top_stmts_flag &&
      env#top_stmts_top = env#pp_if_section_level &&
      self#peek_rawtoken() == PP_ENDIF
  end -> DEBUG_MSG "@"; mk (T.SEMICOLON false)

  | COMMA when begin
      not env#type_paren_flag &&
      not env#templ_head_flag &&
      not env#templ_arg_flag &&
      not env#macro_arg_flag &&
      context == MEM && sub_context == END_OF_TY_SPEC &&
      match prev_rawtoken with
      | IDENT_V _ -> begin
          match self#peek_rawtoken() with
          | CHAR | CHAR8_T | CHAR16_T | CHAR32_T | WCHAR_T | BOOL | SHORT
          | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID | CONST -> true
          | RBRACE -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "IDENT_V @ (CHAR|...)";
    parse_warning env stp edp "semicolon expected at end of declaration";
    mk (T.SEMICOLON false)
  end

  | PP_ENDIF when begin
      env#pp_func_body_odd_flag
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token token;
    mk T.SECTION_MARKER
  end

  | PP_ENDIF when begin
      check_top_stmts_flag &&
      prev_rawtoken != END_STMTS &&
      env#top_stmts_flag &&
      env#top_stmts_top = env#pp_if_section_level
  end -> begin
    DEBUG_MSG "@";
    parse_warning env stp edp "closing statement context";
    self#prepend_token token;
    mk T.END_STMTS
  end

  | PP_ENDIF when self#peek_rawtoken() == EOF && env#brace_level > 0 && env#paren_level = 0 -> begin
      parse_warning env stp edp "lack of closing braces";
      self#prepend_token token;
      for i = 1 to env#brace_level do
        self#prepend_token (mk T.RBRACE)
      done;
      raise To_be_recovered
  end

  | PP_ENDIF -> begin
      DEBUG_MSG "@";
      if sub_context == END_OF_LAM_INTRO && prev_rawtoken == RBRACKET then begin
        DEBUG_MSG "@";
        let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
        insert_after_nth_token nth [mk T.LAM_MARKER]
      end
      else if
        env#get_broken_info() &&
        prev_rawtoken == LBRACE &&
        self#peek_nth_rawtoken 2 != PP_ENDIF &&
        try
          let info = env#pp_if_section_top_info in
          not info.Pinfo.i_func_head
        with
          _ -> true
      then begin
        DEBUG_MSG "@";
        let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
        insert_after_nth_token nth [mk T.MARKER]
      end;

      if
        env#asm_flag &&
        env#pp_if_section_level = 1 &&
        let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
        self#peek_nth_rawtoken (nth+1) == EOF
      then begin
        DEBUG_MSG "inserting END_ASM";
        self#prepend_token token;
        self#prepend_token (mk T.END_ASM);
        raise To_be_recovered
      end;

      if prev_rawtoken == RBRACE && env#end_of_class_spec_flag then begin
        self#prepend_token token;
        self#prepend_token (mk (T.SEMICOLON false));
        raise To_be_recovered
      end;

      try
        let info = env#pp_if_section_top_info in
        DEBUG_MSG "info=%s" (Pinfo.pp_if_section_info_to_string info);
        let brace_lv = env#pp_if_section_rel_brace_level in
        DEBUG_MSG "brace_lv=%d" brace_lv;
        let is_quasi_pp_else =
          brace_lv > 0 &&
          match self#peek_nth_rawtoken 2 with
          | PP_IFNDEF -> begin
              match self#peek_nth_rawtoken 3 with
              | IDENT x -> begin
                  env#pstat#check_pp_branch_cond
                    (function Pinfo.PP_IFDEF y -> y = x | _ -> false)
              end
              | _ -> false
          end
          | _ -> false
        in
        DEBUG_MSG "is_quasi_pp_else=%B" is_quasi_pp_else;
        if is_quasi_pp_else then begin
          let _ = self#discard_token() in
          let _ = self#discard_token() in
          let _ = self#discard_token() in
          self#prepend_token (mk T.PP_ELSE);
          raise To_be_recovered
        end
        else if brace_lv > 0 && (not env#alt_pp_branch_flag) && not env#stack#in_params then begin
          env#open_odd_brace();
          (*env#set_pp_odd_flag();*)
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#exit_pp_if_section ~odd:true ();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          mk T.PP_ODD_ENDIF
        end
        else if
          info.Pinfo.i_odd &&
          env#brace_level > env#odd_brace_level &&
          info.Pinfo.i_brace_level - env#brace_level = env#brace_level - env#odd_brace_level
        then begin
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#exit_pp_if_section();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          mk T.PP_ODD_ENDIF
        end
        else if info.Pinfo.i_odd && env#odd_brace_level = 0 then begin
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#exit_pp_if_section();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#clear_pp_odd_flag();
          mk T.PP_ODD_ENDIF
        end
        else if env#pp_odd_flag && info.Pinfo.i_odd && brace_lv = 0 then begin
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          (*if info.Pinfo.i_odd then begin
            env#close_odd_brace();
            env#clear_pp_odd_flag()
          end;*)
          env#exit_pp_if_section();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#clear_pp_odd_flag();
          mk T.PP_ODD_ENDIF
        end
        else if not env#pp_odd_flag && info.Pinfo.i_odd_canceled && brace_lv < 0 then begin
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#exit_pp_if_section();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          mk T.PP_ODD_ENDIF
        end
        else if
          match prev_rawtoken with
          | PLUS | MINUS when context == EXPR -> true
          | _ -> false
        then begin
          DEBUG_MSG "complementing expression with dummy expression";
          let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
          insert_after_nth_token nth [mk prev_rawtoken];
          self#prepend_token token;
          mk T.DUMMY_EXPR
        end
        else begin
          DEBUG_MSG "@";
          let lbraces = env#pp_if_section_top_info.Pinfo.i_lbraces in
          let rbraces = env#pp_if_section_top_info.Pinfo.i_rbraces in
          DEBUG_MSG "lbraces=%d rbraces=%d" lbraces rbraces;
          if rbraces > 0 then begin
            if rbraces = lbraces then begin
              env#clear_lbrace_info();
              env#clear_rbrace_info();
            end
            else begin
              let t = self#discard_token() in
              for i = 1 to rbraces do
                self#prepend_token (mk T.RBRACE)
              done;
              self#prepend_token t
            end
          end;

          env#clear_pp_odd_flag();

          begin
            match env#pp_top_label with
            | StringLiteral s -> begin
                let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in
                DEBUG_MSG "nth=%d" nth;
                let t =
                  match prev_rawtoken with
                  | SEMICOLON _ -> mk (T.SEMICOLON false)
                  | _ when s = ";" -> mk (T.SEMICOLON false)
                  | _ -> mk T.STR_MARKER
                in
                insert_after_nth_token nth [t];
            end
            | LogicalOrExpression s -> begin
                let nth, l = self#peek_rawtoken_up_to [T.NEWLINE] in
                DEBUG_MSG "nth=%d" nth;
                match prev_rawtoken with
                | SEMICOLON _ -> begin
                    insert_after_nth_token nth [mk_ T.MARKER; mk_ (T.SEMICOLON false)];
                end
                | _ -> ()
            end
            | _ -> ()
          end;

          if env#get_cond_expr_info() then begin
            DEBUG_MSG "@";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            match self#peek_nth_rawtoken (nth+1) with
            | COLON -> begin
                insert_after_nth_token nth [mk T.COND_MARKER]
            end
            | _ -> ()
          end;

          if env#get_comma_info() then begin
            DEBUG_MSG "@";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let _, _, e = self#peek_nth nth in
            match self#peek_nth_rawtoken (nth+1) with
            | PP_ENDIF -> ()
            | _ -> insert_after_nth_token nth [T.COMMA, e, e]
          end
          else if env#get_semicolon_info() then begin
            DEBUG_MSG "@";
            let nth, _ = self#peek_rawtoken_up_to [T.NEWLINE] in
            let _, _, e = self#peek_nth nth in
            (*let b = not (is_semicolon prev_rawtoken) in*)
            insert_after_nth_token nth [T.SEMICOLON false, e, e]
          end;

          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#exit_pp_if_section();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          Stack.iter
            (fun info ->
              DEBUG_MSG "%s" (Pinfo.pp_if_section_info_to_string info)
            ) env#pstat#pp_if_section_stack;

          if info.Pinfo.i_odd then begin
            env#close_odd_brace();
            env#clear_pp_odd_flag()
          end;

          token
        end
      with
        Stack.Empty -> token
  end

  | PP_ENDIF_ -> begin
      begin
        try
          let info = env#pp_if_section_top_info in
          DEBUG_MSG "top info=%s" (Pinfo.pp_if_section_info_to_string info);
          (*DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          env#exit_pp_if_section();
          DEBUG_MSG "pp_if_section_level:%d" env#pp_if_section_level;
          Stack.iter
            (fun i ->
              DEBUG_MSG "%s" (Pinfo.pp_if_section_info_to_string i)
            ) env#pstat#pp_if_section_stack;*)

          if info.Pinfo.i_odd then begin
            env#close_odd_brace();
            env#clear_pp_odd_flag()
          end;
        with _ -> ()
      end;
      mk T.PP_ENDIF
  end

  | SHARP when self#peek_rawtoken() == NEWLINE -> mk T.PP_

  | NEWLINE when begin
      let if0_flag =
        match prev_rawtoken with
        | INT_LITERAL "0" -> prev_rawtoken2 == PP_IF
        | _ -> false
      in
      DEBUG_MSG "if0_flag=%B" if0_flag;
      not env#asm_flag &&
      (env#pp_if_flag || env#pp_ifdef_flag || env#pp_elif_flag || env#pp_else_flag) &&
      match prev_rawtoken with
      | PP_ENDIF -> false
      | _ when env#pp_odd_flag -> false
      | _ when begin
          not env#objc_class_interface_flag &&
          (context == TOP || env#pp_if_section_top_info.Pinfo.i_context == TOP)
      end -> begin
        match self#peek_rawtoken() with
        | IDENT x when is_type_name x || is_type x -> false
        | IDENT x -> begin
            match self#peek_nth_rawtoken 2 with
            | COMMA when env#type_paren_flag -> false
            | COMMA when not env#templ_arg_flag -> true
            | MINUS -> true
            | DOT | MINUS_GT(* | COMMA*) -> true
            | _ -> begin
                try
                  let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
                  match l with
                  | COMMA::_ -> if0_flag
                  | (DOT|QUEST)::_ -> true
                  | SEMICOLON _::DOT::_ -> true
                  | IDENT _::IDENT _::IDENT _::x::_ when is_literal x -> true
                  | IDENT _::IDENT _::x::_ when is_literal x -> true
                  | IDENT _::x::_ when is_literal x -> true
                  | _ -> false
                with
                  Abort -> false
            end
        end
        | STR_LITERAL _ when prev_rawtoken3 == EXTERN -> false
        | INT_LITERAL _ | FLOAT_LITERAL _ | BOOL_LITERAL _ | CHAR_LITERAL _ | STR_LITERAL _
        | NULLPTR | PP_STRINGIZED _  | USER_STR_LITERAL _ | USER_CHAR_LITERAL _
        | USER_INT_LITERAL _ | USER_FLOAT_LITERAL _ when not env#templ_arg_flag -> true
        | OBJC_UNKNOWN _ -> begin
            match self#peek_nth_rawtoken 2 with
            | EQ -> true
            | _ -> false
        end
        | _ -> begin
            DEBUG_MSG "@";
            let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
            match l with
            | x::y::_ when is_numeral x && is_numeral y -> true
            | _ -> false
        end
      end
      | _ when begin
          context == STMT ||
          match env#pp_if_section_top_info.Pinfo.i_sub_context with
          | START_OF_STMT _ -> true
          | _ -> false
      end -> begin

        match self#peek_rawtoken() with
        | TY_LPAREN -> begin
            match self#peek_nth_rawtoken 2 with
            | COLON -> true (* lisp? *)
            | _ -> false
        end
        | IDENT _ -> begin
            try
              let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
              match l with
              | IDENT _::IDENT _::IDENT _::x::_ when is_literal x -> true
              | IDENT _::IDENT _::x::_ when is_literal x -> true
              | IDENT _::x::_ when is_literal x -> true
              | INT_LITERAL _::_ -> true
              | _ -> false
            with
              Abort -> false
        end
        | _ ->
            let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
            if0_flag &&
              contained_in_list_f
                (function
                  | T.IDENT _::IDENT _::IDENT _::_ -> true
                  | T.MINUS_GT::COMMA::_ -> true
                  | _ -> false
                ) l
      end
      | _ when begin
          DEBUG_MSG "@";
          let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
          if0_flag &&
            contained_in_list_f
              (function
                | T.COMMA::IDENT _::MINUS_MINUS::_ -> true
                | _ -> false
              ) (List.rev l)
      end -> true

      | _ -> false
  end -> begin
    begin
      try
        let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
        parse_warning env stp edp "generating #error for tokens: %s"
          (String.concat " " (List.rev_map Token.rawtoken_to_repr l));
        let tl = ref [mk T.PP_ERROR] in
        let marker_flag = ref false in
        for i = 1 to (nth-1) do
          let t = self#discard_token() in
          if i = 1 && PB.token_to_rawtoken t == COMMA then
            marker_flag := true;
          tl := t::!tl
        done;
        tl := (mk T.NEWLINE)::!tl;
        if !marker_flag then
          tl := (mk T.MARKER)::!tl;
        List.iter self#prepend_token !tl;
      with
        Abort -> ()
    end;
    token
  end

  | NEWLINE when begin
      match prev_rawtoken with
      | INT_LITERAL "0" when begin
          match prev_rawtoken2 with
          | PP_IF -> begin
              match self#peek_rawtoken() with
              | IDENT s when Str.string_match ps_ver_pat s 0 -> true
              | IDENT s when is_shader_kw s -> true
              | _ -> false
          end
          | _ -> false
      end -> true
      | _ -> false
  end -> begin
    begin
      try
        let nth, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) () in
        let tl = ref [mk T.ASM_SHADER] in
        for i = 1 to (nth-1) do
          let t = self#discard_token() in
          tl := t::!tl
        done;
        tl := (mk T.NEWLINE)::!tl;
        List.iter self#prepend_token !tl;
      with
        Abort -> ()
    end;
    env#set_asm_shader_flag();
    token
  end

  | NEWLINE when begin
      context == TOP && sub_context == INI &&
      env#pp_define_body_flag &&
      not env#asm_flag &&
      env#pp_if_section_flag
  end -> begin
    DEBUG_MSG "@";
    match self#peek_rawtoken() with
    | RBRACE -> begin
        match self#peek_nth_rawtoken 2 with
        | WHILE -> begin
            match self#peek_nth_rawtoken 3 with
            | TY_LPAREN -> begin
                let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:4 None in
                match self#peek_nth_rawtoken (nth+1) with
                | PP_ELIF | PP_ELSE -> begin
                    DEBUG_MSG "@";
                    insert_after_nth_token nth [token];
                    self#discard_token()
                end
                | _ -> token
            end
            | _ -> token
        end
        | _ -> token
    end
    | _ -> token
  end

  | RBRACKET -> begin
      begin
        match self#peek_rawtoken() with
        | EQ -> begin
            match self#peek_nth_rawtoken 2 with
            | LBRACE -> begin
                match self#peek_nth_rawtoken 3 with
                | PP_ELIF | PP_ELSE | PP_ENDIF -> begin
                    DEBUG_MSG "@";
                    self#prepend_token (mk T.MARKER)
                end
                | _ -> ()
            end
            | _ -> ()
        end
        | IDENT _ -> begin
            match self#peek_nth_rawtoken 2 with
            | EQ -> begin
                match self#peek_nth_rawtoken 3 with
                | LBRACE -> begin
                    match self#peek_nth_rawtoken 4 with
                    | PP_ELIF | PP_ELSE | PP_ENDIF -> begin
                        DEBUG_MSG "@";
                        let t = self#discard_token() in
                        self#prepend_token (mk T.MARKER);
                        self#prepend_token t
                    end
                    | _ -> ()
                end
                | _ -> ()
            end
            | _ -> ()
        end
        | _ -> ()
      end;
      token
  end

  | COMMA when env#paren_level > 0 && self#peek_rawtoken() == LBRACKET && begin
      let nth, l = self#peek_rawtoken_up_to_rbracket ~from:2 () in
      match self#peek_nth_rawtoken (nth+1) with
      | PP_ELIF | PP_ELSE | PP_ENDIF -> true
      | _ -> false
  end -> mk T.COMMA_BROKEN

  | COMMA when begin
      env#paren_level > 0 && env#arg_paren_flag && not env#asm_flag &&
      match self#peek_rawtoken() with
      | PP_ELIF | PP_ELSE | PP_ENDIF -> begin
          let info = env#pp_if_section_top_info in
          info.Pinfo.i_context == STMT &&
          match info.Pinfo.i_sub_context with
          | INI | START_OF_STMT _ -> true
          | _ -> false
      end
      | _ -> false
  end -> mk T.COMMA_BROKEN

  | COMMA when begin
      match self#peek_rawtoken() with
      | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | EQ_EQ | EXCLAM_EQ _ -> begin
          match self#peek_nth_rawtoken 2 with
          | COMMA | RPAREN -> false
          | _ -> true
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "complementing expression with dummy expression";
    self#prepend_token (mk_ T.DUMMY_EXPR);
    token
  end

  | COMMA when env#macro_arg_flag -> begin
      DEBUG_MSG "@";
      let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
      let l =
        match ll with
        | [] -> []
        | _ -> Xlist.last ll
      in
      if
        contained_in_list_f
          (function
            | T.INT_LITERAL _::(IDENT _|BAR_BAR _|PTR_AMP_AMP)::_ -> true
            | IDENT _::IDENT _::IDENT _::_ -> true
            | IDENT _::IDENT _::MINUS::_ -> true
            | _ -> false
          ) l
      then begin
        let line = String.concat " " (List.rev_map Token.rawtoken_to_repr l) in
        let ep = ref dummy_lexpos in
        for i = 1 to nth - 1 do
          let _, _, e = self#discard_token() in
          ep := e
        done;
        self#prepend_token ((T.DOXYGEN_LINE line), stp, !ep)
      end
      else if
        not env#braced_init_flag &&
        self#peek_rawtoken() != LBRACKET &&
        env#bracket_level = 0 &&
        List.length ll = 1 &&
        let f' l =
          match (l : T.token list) with
          | (IF|SWITCH|FOR|WHILE|DO)::_ -> true
          | _ -> false
        in
        List.exists
          (fun l ->
            let ll' = split_at_semicolon l in
            DEBUG_MSG "\n%s"
              (String.concat "\n"
                 (List.map
                    (fun x -> String.concat ";"
                        (List.map Token.rawtoken_to_string x)) ll'));
            List.length ll' > 1 &&
            List.exists f' ll'
          ) ll
      then begin
        DEBUG_MSG "@";
        self#prepend_token (mk T.MARKER)
      end;
      token
  end

  | COMMA when env#type_paren_flag -> begin
      DEBUG_MSG "@";
      if prev_rawtoken != TY_LPAREN && self#peek_rawtoken() == RPAREN then begin
        self#prepend_token (mk T.DUMMY_TYPE)
      end;
      token
  end

  | COMMA when env#ctor_init_flag && env#paren_level = 0 && begin
      match self#peek_rawtoken() with
      | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
          let nth, _ = self#peek_rawtoken_up_to ~from:2 [T.NEWLINE] in
          let _, l = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(nth+1) () in
          match l with
          | RBRACE::_ -> true
          | _ -> false
      end
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token token;
    mk T.MARKER
  end

  | DOXYGEN_CMD s | BS_IDENT s when env#macro_arg_flag -> begin
      DEBUG_MSG "@";
      let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
      let l =
        match ll with
        | [] -> []
        | _ -> Xlist.last ll
      in
      let line = String.concat " " (s::(List.rev_map Token.rawtoken_to_repr l)) in
      let ep = ref dummy_lexpos in
      for i = 1 to nth - 1 do
        let _, _, e = self#discard_token() in
        ep := e
      done;
      (T.DOXYGEN_LINE line), stp, !ep
  end

  | BS_IDENT s when is_doxygen_cmd s -> mk (T.DOXYGEN_CMD s)

  | MS_ASM s when begin
      not env#pp_line_flag && not env#braced_asm_flag &&
      match self#peek_rawtoken() with
      | TY_LPAREN -> true
      | IDENT _ -> begin
          match self#peek_nth_rawtoken 2 with
          | TY_LPAREN -> true
          | _ -> false
      end
      | _ -> false
  end -> env#exit_lex_ms_asm_line(); DEBUG_MSG "@"; mk (T.GNU_ASM s)

  | PLUS when env#objc_class_interface_flag -> mk T.OBJC_PLUS
  | MINUS when env#objc_class_interface_flag -> mk T.OBJC_MINUS

  | THROW when begin
      match self#peek_rawtoken() with
      | SEMICOLON _ | RPAREN | RBRACKET | EOF -> true
      | _ -> false
  end -> mk T.THROW_

  | GNU_ASM s when begin
      env#end_of_params_flag &&
      match self#peek_rawtoken() with
      | TY_LPAREN -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    begin
      let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
      match self#peek_nth_rawtoken (nth+1) with
      | IDENT _ -> begin
          match self#peek_nth_rawtoken (nth+2) with
          | SEMICOLON _ | LBRACE | COLON -> begin
              conv_nth_token (function T.IDENT x,s,e -> T.ATTR_MACRO x,s,e | x -> x) (nth+1)
          end
          | TY_LPAREN -> begin
              conv_nth_token (function T.IDENT x,s,e -> T.IDENT_AM x,s,e | x -> x) (nth+1)
          end
          | _ -> ()
      end
      | _ -> ()
    end;
    mk (T.IDENT_AM s)
  end

  | FOR when begin
      match self#peek_rawtoken() with
      | TY_LPAREN -> begin
          let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
          match self#peek_nth_rawtoken (nth+1) with
          | PP_IF | PP_IFDEF | PP_IFNDEF | PP_ELIF | PP_ELSE | PP_ENDIF -> true
          | _ -> false
      end
      | _ -> false
  end -> DEBUG_MSG "@"; mk T.ODD_FOR

  | TEMPLATE when context == EXPR && prev_rawtoken == EOF && begin
      match self#peek_rawtoken() with
      | IDENT _ -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "complementing expression with dummy expression";
    self#prepend_token token;
    _mk T.DUMMY_EXPR
  end

  | EOF when env#asm_block_flag -> begin
      DEBUG_MSG "inserting END_ASM";
      self#prepend_token token;
      mk T.END_ASM
  end

  | EOF when env#end_of_handler_head_flag && begin
      match self#peek_rawtoken() with
      | EOF -> true
      | _ -> false
  end -> begin
    DEBUG_MSG "@";
    self#prepend_token token;
    mk T.DUMMY_STMT
  end

  | EOF when env#decl_stmt_block_flag -> begin
      DEBUG_MSG "lack of block end";
      self#prepend_token token;
      mk (T.BLOCK_END_MACRO "")
  end

  | _ -> token
(* end of conv_token *)


module F (Stat : Aux.STATE_T) = struct

  module U = Ulexer.F (Stat)

  open Stat

  let peek_rawtoken_cache_len = 4

  class sstat = object (self)
    val mutable prev_rawtoken = T.EOF
    val mutable prev_rawtoken2 = T.EOF
    val mutable prev_rawtoken3 = T.EOF
    val mutable prev_rawtoken4 = T.EOF

    method set_prev_rawtoken r = prev_rawtoken <- r
    method set_prev_rawtoken2 r = prev_rawtoken2 <- r
    method set_prev_rawtoken3 r = prev_rawtoken3 <- r
    method set_prev_rawtoken4 r = prev_rawtoken4 <- r

    method prev_rawtoken = prev_rawtoken
    method prev_rawtoken2 = prev_rawtoken2
    method prev_rawtoken3 = prev_rawtoken3
    method prev_rawtoken4 = prev_rawtoken4

    method reset () =
      prev_rawtoken <- T.EOF;
      prev_rawtoken2 <- T.EOF;
      prev_rawtoken3 <- T.EOF;
      prev_rawtoken4 <- T.EOF
  end

  class c (env : Aux.env) = object (self)
    inherit [T.token] PB.scanner

    val mutable _ulexbuf = Ulexing.from_utf8_string ""

    val mutable token_hist_flag = false

    val mutable context = C.TOP
    val mutable sub_context = C.INI

    val mutable prev_endofs = 0
    val mutable prev_endln = 0
    val mutable prev_stp = dummy_lexpos
    val mutable prev_edp = dummy_lexpos
    val mutable prev_rawtoken = T.EOF
    val mutable prev_rawtoken2 = T.EOF
    val mutable prev_rawtoken3 = T.EOF
    val mutable prev_rawtoken4 = T.EOF

    val mutable sstat = new sstat

    val mutable current_token = T.EOF, dummy_lexpos, dummy_lexpos

    val context_stack = Stack.create()
    val sub_context_stack = Stack.create()

    val mutable saved_pstat = new Aux.pstat

    val queue = (new Xqueue.c : token Xqueue.c)

    val mutable replay_queue = (new Xqueue.c : token Xqueue.c)
    val replay_queue_backup = (new Xqueue.c : token Xqueue.c)
    val mutable state_number = -1
    val mutable keep_flag = false
    val mutable replay_flag = false
    val mutable alternative_tokens = []
    val mutable replay_success_callback = fun () -> ()

    val mutable check_top_stmts_flag = true
    val mutable macro_body_parsing_flag = false
    val mutable q_prop_flag = false

    val ident_conv_tbl = Hashtbl.create 0

    val macro_fun_set = Xset.create 0

    method reg_ident_conv s rt =
      let tbl =
        try
          Hashtbl.find ident_conv_tbl s
        with
          Not_found -> begin
            let tbl = Hashtbl.create 0 in
            Hashtbl.add ident_conv_tbl s tbl;
            tbl
          end
      in
      try
        let c = Hashtbl.find tbl rt in
        Hashtbl.replace tbl rt (c+1)
      with
        Not_found -> Hashtbl.add tbl rt 1

    method find_ident_conv s =
      let tbl = Hashtbl.find ident_conv_tbl s in
      let cands = ref [] in
      Hashtbl.iter
        (fun rt c ->
          DEBUG_MSG "%s: %d" (Token.rawtoken_to_string rt) c;
          if c > 2 then
            cands := rt :: !cands;
        ) tbl;
      match !cands with
      | [x] -> x
      | _ -> raise Not_found

    method lookup_name s =
      try
        let spec = env#lookup_name s in
        DEBUG_MSG "%s -> %s" s spec#to_string;
        spec
      with
        Not_found ->
          let s_ = Pinfo.encode_ident s in
          let spec = env#lookup_name s_ in
          DEBUG_MSG "%s -> %s" s_ spec#to_string;
          spec

    method is_type ?(weak=false) s =
      let b =
        try
          let spec = self#lookup_name s in
          match (spec#kind : Pinfo.Name.Spec.kind) with
          | Class _ | Struct _ | Union _
          | Enum _ | EnumClass _ | EnumStruct _ when weak -> true
          | Variable ty when weak -> Pinfo.Type.is_typedef ty
          | Member mspec when weak -> Pinfo.Type.is_typedef mspec#ty || Pinfo.Type.is_type_type mspec#ty

          | TypeParam pspec -> not (Pinfo.Name.Spec.param_spec_is_concept pspec)
          | Type -> true

          | _ -> false
        with
          Not_found -> false
      in
      DEBUG_MSG "%s -> %B" s b;
      b

    method is_templ s =
      let b =
        try
          let spec = self#lookup_name s in
          match (spec#kind : Pinfo.Name.Spec.kind) with
          | Template _ -> true
          | _ -> false
        with
          Not_found -> false
      in
      DEBUG_MSG "%s -> %B" s b;
      b

    method _is_val s =
      try
        let spec = env#stack#top#find (Pinfo.encode_ident s) in
        match (spec#kind : Pinfo.Name.Spec.kind) with
        | Variable ty when not (Pinfo.Type.is_typedef ty) -> begin
            match Pinfo.Type.unwrap ty with
            | SimpleTy sty when Pinfo.Type.simple_ty_has_basic_ty sty -> begin
                match prev_rawtoken with
                | ENUM -> false
                | RBRACE | SEMICOLON _ when self#peek_rawtoken() == T.COLON -> false
                | _ -> true
            end
            | _ -> false
        end
        | _ -> false
      with
        Not_found -> false

    method is_val s =
      let b =
        try
          let spec = self#lookup_name s in
          match (spec#kind : Pinfo.Name.Spec.kind) with
          | Variable ty when Pinfo.Type.is_typedef ty -> false
          (*| Function _*)(* may be a ctor *)
          | Variable ty when begin
              match Pinfo.Type.unwrap ty with
              | SimpleTy sty -> begin
                  let s = Pinfo.Type.simple_ty_to_string sty in
                  DEBUG_MSG "s=%s" s;
                  s = "9interface"(* MIDL? *)
              end
              | _ -> false
          end -> false
          | Variable _ | Enumerator _ | FParam _ -> true
          | Param ty -> begin
              match Pinfo.Type.unwrap ty with
              | SimpleTy sty -> Pinfo.Type.simple_ty_has_basic_ty sty
              | _ -> false
          end
          | _ -> false
        with
          Not_found -> false
      in
      DEBUG_MSG "%s -> %B" s b;
      b

    method reg_macro_fun s =
      DEBUG_MSG "%s" s;
      Xset.add macro_fun_set s

    method is_macro_fun s =
      let b =
        try
          let _ = self#lookup_name (Ast.mk_macro_call_id s) in
          true
          (*match (spec#kind : Pinfo.Name.Spec.kind) with
          | MacroFun -> true
          | _ -> false*)
        with
          Not_found -> Xset.mem macro_fun_set s
      in
      DEBUG_MSG "%s -> %B" s b;
      b

    method is_macro_obj s =
      let b =
        try
          let spec = self#lookup_name (Ast.mk_macro_id s) in
          match (spec#kind : Pinfo.Name.Spec.kind) with
          | MacroFun -> true
          | _ -> false
        with
          Not_found -> false
      in
      DEBUG_MSG "%s -> %B" s b;
      b

    method reset () =
      check_top_stmts_flag <- true;
      macro_body_parsing_flag <- false;
      q_prop_flag <- false;
      env#reset_pstat();
      sstat#reset();
      prev_rawtoken <- T.EOF;
      prev_rawtoken2 <- T.EOF;
      prev_rawtoken3 <- T.EOF;
      prev_rawtoken4 <- T.EOF;
      queue#clear;
      replay_queue#clear;
      replay_queue_backup#clear;
      Hashtbl.clear ident_conv_tbl;
      Xset.clear macro_fun_set

    method is_opening_stmt_macro s =
      contains_foreach s && begin
        match self#peek_rawtoken() with
        | T.TY_LPAREN | LBRACE -> false
        | _ -> true
      end

    method prev_endofs = prev_endofs
    method prev_endln = prev_endln
    method prev_edp = prev_edp
    method prev_rawtoken = prev_rawtoken
    method prev_rawtoken2 = prev_rawtoken2
    method prev_rawtoken3 = prev_rawtoken3
    method prev_rawtoken4 = prev_rawtoken4
    (*method queue = queue*)

    method check_top_stmts_flag = check_top_stmts_flag
    method set_check_top_stmts_flag () = check_top_stmts_flag <- true
    method clear_check_top_stmts_flag () = check_top_stmts_flag <- false

    method macro_body_parsing_flag = macro_body_parsing_flag
    method set_macro_body_parsing_flag () = macro_body_parsing_flag <- true
    method clear_macro_body_parsing_flag () = macro_body_parsing_flag <- false

    method private q_prop_flag = q_prop_flag
    method private set_q_prop_flag () = q_prop_flag <- true
    method private clear_q_prop_flag () = q_prop_flag <- false

    method current_token = current_token

    method current_loc =
      let _, st, ed = current_token in
      env#current_pos_mgr#lexposs_to_loc ~get_position:false st ed

    method enter_block () =
      env#stack#enter_block self#current_loc.Ast.Loc.start_line

    method set_token_hist_flag () = token_hist_flag <- true

    method pp_restore_context () =
      try
        let info = env#pp_if_section_top_info in
        let c = info.Pinfo.i_context in
        DEBUG_MSG "%s" (C.to_string c);
        context <- c;
        let sc = info.Pinfo.i_sub_context in
        DEBUG_MSG "%s" (C.sub_to_string sc);
        sub_context <- sc
      with
        _ -> ()

    method private enter_source (src : Source.c) =
      DEBUG_MSG "source=\"%s\"" src#filename;
      let ulexbuf =
        if src#filename = "<stdin>" then begin
          src#get_ulexbuf_from_stdin
        end
        else begin
          src#get_ulexbuf
        end
      in
      _ulexbuf <- ulexbuf;
      ulexbuf

    method queue_token t =
      DEBUG_MSG "%s" (Token.to_string env#current_pos_mgr t);
      queue#add t

    method context = context
    method sub_context = sub_context

    method top_context = Stack.top context_stack

    method push_context () =
      let c = self#context in
      DEBUG_MSG "push_context: %s" (C.to_string c);
      Stack.push c context_stack

    method pop_context () =
      let c = Stack.pop context_stack in
      DEBUG_MSG "pop_context: %s" (C.to_string c);
      context <- c

    method top_sub_context = Stack.top sub_context_stack

    method push_sub_context () =
      let c = self#sub_context in
      DEBUG_MSG "push_sub_context: %s" (C.sub_to_string c);
      Stack.push c sub_context_stack

    method pop_sub_context () =
      let c = Stack.pop sub_context_stack in
      DEBUG_MSG "pop_sub_context: %s" (C.sub_to_string c);
      sub_context <- c

    method private enter_pp_line () =
      env#enter_pp_line()

    method private exit_pp_line () =
      env#exit_pp_line()

    method restore_context () =
      context <- C.STMT;
      sub_context <- C.INI

    method restore_state () =
      let pstat_copy = saved_pstat#copy in
      self#load_prev_rawtokens();
      env#set_pstat pstat_copy;
      env#restore_stack()

    method ctx_end_of_ty_spec () =
      match sub_context with
      | END_OF_TY_SPEC -> ()
      | _ ->
          sub_context <- C.END_OF_TY_SPEC;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_end_of_id_expr () =
      match sub_context with
      | END_OF_ID_EXPR | IN_EXPR -> ()
      | _ ->
          sub_context <- C.END_OF_ID_EXPR;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_end_of_dtor () =
      match sub_context with
      | END_OF_DTOR -> ()
      | _ ->
          sub_context <- C.END_OF_DTOR;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_end_of_stmt () =
      match sub_context with
      | END_OF_STMT -> ()
      | _ ->
          sub_context <- C.END_OF_STMT;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_end_of_lam_intro () =
      match sub_context with
      | END_OF_LAM_INTRO -> ()
      | _ ->
          sub_context <- C.END_OF_LAM_INTRO;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_in_case_label () =
      match sub_context with
      | IN_CASE_LABEL -> ()
      | _ ->
          sub_context <- C.IN_CASE_LABEL;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_in_expr () =
      match sub_context with
      | IN_EXPR -> ()
      | _ ->
          sub_context <- C.IN_EXPR;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)


    method ctx_stmt () =
      match context with
      | STMT -> ()
      | _ ->
          context <- C.STMT;
          DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_expr ?(force=false) () =
      match context with
      | CLASS | EXPR -> ()
      | _ ->
          if force || not env#templ_head_flag then begin
            context <- C.EXPR;
            DEBUG_MSG "%a" pr_ctx (C.to_string context)
          end

    method ctx_in_simple_templ_id () =
      match sub_context with
      | IN_SIMPLE_TEMPL_ID -> ()
      | _ ->
          sub_context <- C.IN_SIMPLE_TEMPL_ID;
          DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_ty () =
      match context with
      | TY -> ()
      | _ ->
          context <- C.TY;
          DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_new () =
      match context with
      | NEW -> ()
      | _ ->
          context <- C.NEW;
          DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_mem_init () =
      match context with
      | MEM_INIT -> ()
      | _ ->
          context <- C.MEM_INIT;
          DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_class () =
      match context with
      | CLASS -> ()
      | _ ->
          context <- C.CLASS;
          DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_mem () =
      match context with
      | MEM -> ()
      | _ ->
          context <- C.MEM;
          DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_enum () =
      match context with
      | ENUM -> ()
      | _ ->
          context <- C.ENUM;
          DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_start_of_stmt sn =
      context <- C.STMT;
      sub_context <- C.START_OF_STMT sn;
      DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_ini () =
      sub_context <- C.INI;
      DEBUG_MSG "%a" pr_ctx (C.sub_to_string sub_context)

    method ctx_top () =
      context <- C.TOP;
      DEBUG_MSG "%a" pr_ctx (C.to_string context)

    method ctx_reset () =
      self#ctx_ini();
      if env#stack#in_class then
        self#ctx_mem()
      else
        self#ctx_top()

    method private save_prev_rawtokens () =
      sstat#set_prev_rawtoken prev_rawtoken;
      sstat#set_prev_rawtoken2 prev_rawtoken2;
      sstat#set_prev_rawtoken3 prev_rawtoken3;
      sstat#set_prev_rawtoken4 prev_rawtoken4

    method private load_prev_rawtokens () =
      prev_rawtoken <- sstat#prev_rawtoken;
      prev_rawtoken2 <- sstat#prev_rawtoken2;
      prev_rawtoken3 <- sstat#prev_rawtoken3;
      prev_rawtoken4 <- sstat#prev_rawtoken4

    method replay_queue_length = replay_queue#length

    method init_replay_queue sn pstat tokens =
      DEBUG_MSG "token queue initialized: state=%d" sn;
      DEBUG_MSG "pstat=%s" pstat#to_string;
      DEBUG_MSG "tokens=[%s]"
        (String.concat ";"
           (List.map Token.rawtoken_to_string (List.map (fun (x, _, _) -> x) tokens)));
      alternative_tokens <- tokens;
      replay_queue_backup#clear;
      state_number <- sn;
      self#save_prev_rawtokens();
      saved_pstat <- pstat#copy;
      env#save_stack();
      env#set_scanner_keep_flag();
      keep_flag <- true

    method stop_replay_queue () =
      DEBUG_MSG "token queue stopped";
      ignore replay_queue_backup#take;
      replay_queue <- replay_queue_backup#copy;
      begin
        match alternative_tokens with
        | t::rest -> begin
            replay_queue#prepend t;
            alternative_tokens <- rest;
        end
        | [] -> failwith "Scanner.F.c#stop_replay_queue"
      end;
      env#clear_scanner_keep_flag();
      keep_flag <- false;
      replay_flag <- true

    method clear_keep_flag () =
      DEBUG_MSG "keep_flag cleared";
      env#clear_scanner_keep_flag();
      keep_flag <- false

    method keep_flag = keep_flag
    (*method replay_flag = replay_flag*)
    method state_number = state_number

    method clear_alternative_tokens () =
      DEBUG_MSG "alternative_tokens cleared";
      alternative_tokens <- []

    method prepend_token t = queue#prepend t
    method discard_token () =
      let _ = self#peek_rawtoken() in
      let t = queue#take in
      DEBUG_MSG "discarded %s" (Token.to_string env#current_pos_mgr t);
      t

    method private replay ?(mix=false) () =
      DEBUG_MSG "mix=%B" mix;
      try
        replay_queue#take
      with
        Queue.Empty ->
          replay_flag <- false;
          if not mix then begin
            self#clear_alternative_tokens();
            replay_success_callback();
          end;
          raise Queue.Empty

    method register_replay_success_callback f =
      replay_success_callback <- f

    method setup_replay () =
      replay_queue <- replay_queue_backup#copy;
      begin
        match alternative_tokens with
        | t::rest -> begin
            replay_queue#prepend t;
            alternative_tokens <- rest;
        end
        | [] -> failwith "Scanner.F.c#setup_replay"
      end;
      replay_flag <- true

    method private keep x = replay_queue_backup#add x

    method has_alternative_tokens = alternative_tokens <> []

    method private __get_token () =
      try
        queue#take
      with
        Queue.Empty ->
          U._token _ulexbuf

    method private _peek () =
      try
        queue#peek
      with
        Queue.Empty ->
          let t = U._token _ulexbuf in
          queue#add t;
          t

    method peek () =
      if replay_flag then
        try
          replay_queue#peek
        with
          Queue.Empty -> self#_peek()
      else
        self#_peek()

    method peek_rawtoken () =
      let rt, _, _ = self#peek() in
      rt

    method private _peek_nth n =
      try
        queue#peek_nth n
      with
        Failure _ ->
          let tr = ref None in
          for i = 1 to n - queue#length do
            let t = U._token _ulexbuf in
            queue#add t;
            tr := Some t
          done;
          match !tr with
          | Some t -> t
          | None -> failwith "Scanner.F.c#_peek_nth"

    method peek_nth n =
      if replay_flag then
        try
          replay_queue#peek_nth n
        with
          Failure _ -> self#_peek_nth (n - replay_queue#length)
      else
        self#_peek_nth n

    method peek_nth_rawtoken n =
      let rt, _, _ = self#peek_nth n in
      rt

    method peek_rawtoken_up_to ?(from=1) ?(skip_pp_control_line=false) ?(is_target=fun _ -> false) target_list =
      let peek_rawtoken_up_to () =
      let l = ref [] in
      let nth = ref from in
      try
        while true do
          let rt = self#peek_nth_rawtoken !nth in
          if
            (is_target rt || List.exists ((==) rt) target_list) &&
            (not skip_pp_control_line ||
            try not (is_pp_control_line (self#peek_nth_rawtoken (!nth + 1))) with _ -> true)
          then
            raise Exit
          else if rt == EOF then
            raise Exit
          else begin
            DEBUG_MSG "  %d: %s" !nth (Token.rawtoken_to_string rt);
            l := rt :: !l
          end;
          incr nth
        done;
        assert false
      with
      | Exit -> !nth, !l
      in
      peek_rawtoken_up_to()

    method peek_rawtoken_up_to_section_end ?(from=1) () =
      let nth = ref from in
      try
        while true do
          let n0, _ = self#peek_rawtoken_up_to ~from:(!nth) [T.NEWLINE] in
          let n1, _ = self#peek_rawtoken_up_to_group_end ~limit:(-1) ~from:(n0+1) () in
          nth := n1 + 1;
          match self#peek_nth_rawtoken n1 with
          | PP_ELIF | PP_ELSE -> ()
          | PP_ENDIF -> raise Exit
          | _ -> raise Abort
        done;
        assert false
      with
      | Exit -> begin
          let n, _ = self#peek_rawtoken_up_to ~from:(!nth) [T.NEWLINE] in
          n
      end
      | Abort -> -1

    method peek_rawtoken_up_to_group_end ?(limit=16) ?(from=1) ?(filt=fun _ -> false) () =
      let peek_rawtoken_up_to () =
      let l = ref [] in
      let nth = ref from in
      let lv = ref 0 in
      let chk =
        if limit < 0 then
          fun () -> ()
        else
          fun () -> if !nth > from + limit then raise Abort
      in
      try
        while true do
          chk();
          let rt = self#peek_nth_rawtoken !nth in
          begin
            match rt with
            | PP_IF | PP_IFDEF | PP_IFNDEF -> incr lv; l := rt :: !l
            | PP_ELIF | PP_ELSE | PP_ENDIF when !lv = 0 -> raise Exit
            | PP_ENDIF -> decr lv; l := rt :: !l
            | EOF -> raise Exit
            | _ -> begin
                DEBUG_MSG "  %d: %s" !nth (Token.rawtoken_to_string rt);
                if filt rt then
                  raise Found;
                l := rt :: !l
            end
          end;
          incr nth
        done;
        assert false
      with
      | Exit -> !nth, !l
      in
      peek_rawtoken_up_to()

    method peek_rawtoken_up_to_rparen ?(from=1) ?(level=(env#paren_level-1)) ?(filt=fun _ -> false) rawtok_opt =
      DEBUG_MSG "target level: %d" level;
      let peek_rawtoken_up_to_rparen () =
        let l = ref [] in
        let nth = ref from in
        let lv = ref env#paren_level in
        let chk =
          match rawtok_opt with
          | Some x -> fun rt -> if rt == x || filt rt then raise Found
          | None -> fun x -> if filt x then raise Found
        in
        let skip_up_to_pp_endif () =
          DEBUG_MSG "skipping...";
          try
            while true do
              let rt = self#peek_nth_rawtoken !nth in
              begin
                match rt with
                | PP_ENDIF -> begin
                    DEBUG_MSG "  %d: %s" !nth (Token.rawtoken_to_string rt);
                    l := rt :: !l;
                    incr nth;
                    begin
                      try
                        while true do
                          let rt0 = self#peek_nth_rawtoken !nth in
                          match rt0 with
                          | NEWLINE -> begin
                              DEBUG_MSG "  %d: %s" !nth (Token.rawtoken_to_string rt);
                              l := rt :: !l;
                              incr nth;
                              raise Exit
                          end
                          | EOF -> raise Not_found
                          | _ -> begin
                              l := rt :: !l;
                              incr nth;
                          end
                        done;
                        assert false
                      with
                        Exit -> ()
                    end;
                    raise Exit
                end
                | EOF -> raise Not_found
                | _ -> ()
              end;
              DEBUG_MSG "  %d: %s" !nth (Token.rawtoken_to_string rt);
              l := rt :: !l;
              incr nth
            done;
            assert false
          with
            Exit -> ()
        in
        try
          while true do
            try
              let rt = self#peek_nth_rawtoken !nth in
              chk rt;
              begin
                match rt with
                | PP_ELIF | PP_ELSE -> begin
                    l := rt :: !l;
                    skip_up_to_pp_endif();
                    raise Continue
                end

                | TY_LPAREN | LPAREN -> incr lv
                | RPAREN -> begin
                    decr lv;
                    if !lv = level then
                      raise Exit
                end
                | EOF -> raise Not_found
                | _ -> ()
              end;
              DEBUG_MSG "  %d: %s (lv=%d)" !nth (Token.rawtoken_to_string rt) !lv;
              l := rt :: !l;
              incr nth
            with
              Continue -> ()
          done;
          assert false
        with
        | Exit -> false, !nth, !l
        | Found -> true, !nth, !l
        | Not_found -> false, !nth, !l
      in
      peek_rawtoken_up_to_rparen()

    val mutable peek_rawtoken_up_to_rparen_none_cache = None
    method peek_rawtoken_up_to_rparen_none () =
      match peek_rawtoken_up_to_rparen_none_cache with
      | Some x -> x
      | _ ->
          let _, nth, l = self#peek_rawtoken_up_to_rparen None in
          let x = nth, l in
          peek_rawtoken_up_to_rparen_none_cache <- Some x;
          x

    method peek_rawtoken_up_to_rparen_split_at_comma
        ?(from=1) ?(ignore_pp=false) ?(ignore_templ_lv=false) ?(lv_ofs=1) ?(filt=fun _ -> false) ()
        =
      let paren_lv = env#paren_level in
      let templ_lv = env#templ_param_arg_level in
      let brace_lv = env#brace_level in
      let level = paren_lv - lv_ofs in
      let ll = ref [] in
      let l = ref [] in
      let nth = ref from in
      let lv = ref paren_lv in
      let tlv = ref templ_lv in
      let blv = ref brace_lv in
      let prev = ref T.EOF in
      let in_pp_if_section = env#pp_if_section_flag in
      try
        while true do
          let rt = self#peek_nth_rawtoken !nth in
          let flag = ref true in
          begin
            match rt with
            | TY_LPAREN | LPAREN -> incr lv
            | RPAREN -> begin
                decr lv;
                if !lv = level then
                  raise Exit
            end
            | LBRACE -> incr blv
            | RBRACE -> decr blv
            | TY_TEMPL_GT | TEMPL_GT when !prev != COMMA && !tlv > 0 -> decr tlv
            | TEMPL_LT when (match !prev with IDENT _ | EOF -> true | _ -> false) -> incr tlv
            | COMMA -> begin
                if
                  !lv = paren_lv &&
                  (ignore_templ_lv || !tlv = templ_lv) &&
                  !blv = brace_lv
                then begin
                  if filt !l then begin
                    DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string !l));
                    raise Found
                  end;
                  ll := !l :: !ll;
                  l := [];
                  flag := false;
                end
            end
            | PP_IF | PP_IFDEF | PP_IFNDEF | PP_ELSE | PP_ELIF | PP_ENDIF
            | PP_DEFINE | PP_UNDEF | PP_INCLUDE when ignore_pp -> begin
                let n, _ = self#peek_rawtoken_up_to ~from:(!nth+1) [T.NEWLINE] in
                nth := n
            end
            | PP_ELSE | PP_ELIF when in_pp_if_section -> begin
                let n, _ = self#peek_rawtoken_up_to ~from:(!nth+1) [T.PP_ENDIF] in
                let n, _ = self#peek_rawtoken_up_to ~from:n [T.NEWLINE] in
                nth := n
            end
            | EOF -> raise Exit
            | _ -> ()
          end;
          DEBUG_MSG "  %d: %s (lv=%d)" !nth (Token.rawtoken_to_string rt) !lv;
          if !flag then
            l := rt :: !l;
          prev := rt;
          incr nth
        done;
        assert false
      with
      | Exit ->
          match !prev with
          | EOF -> !nth, !ll
          | _ ->
              if filt !l then begin
                DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string !l));
                raise Found
              end;
              !nth, (!l :: !ll)

    method peek_rawtoken_up_to_rbrace
        ?(noexcept=false) ?(from=1) ?(lv_ofs=1) ?(filt=fun _ -> false) () =
      let brace_lv = env#brace_level in
      let level = brace_lv - lv_ofs in
      let l = ref [] in
      let nth = ref from in
      let lv = ref brace_lv in
      try
        while true do
          let rt = self#peek_nth_rawtoken !nth in
          if filt rt then
            raise (if noexcept then Exit else Found);

          let flag = ref true in
          begin
            match rt with
            | LBRACE -> incr lv
            | RBRACE -> begin
                decr lv;
                if !lv = level then
                  raise Exit
            end
            | EOF -> raise Exit
            | _ -> ()
          end;
          DEBUG_MSG "  %d: %s (lv=%d)" !nth (Token.rawtoken_to_string rt) !lv;
          if !flag then
            l := rt :: !l;
          incr nth
        done;
        assert false
      with
      | Exit -> !nth, !l

    method peek_rawtoken_up_to_rbracket ?(from=1) ?(lv_ofs=1) ?(filt=fun _ -> false) () =
      let bracket_lv = env#bracket_level in
      let level = bracket_lv - lv_ofs in
      let l = ref [] in
      let nth = ref from in
      let lv = ref bracket_lv in
      try
        while true do
          let rt = self#peek_nth_rawtoken !nth in
          if filt rt then
            raise Found;
          let flag = ref true in
          begin
            match rt with
            | LBRACKET | LAM_LBRACKET -> incr lv
            | RBRACKET -> begin
                decr lv;
                if !lv = level then
                  raise Exit
            end
            | EOF -> raise Exit
            | _ -> ()
          end;
          DEBUG_MSG "  %d: %s (lv=%d)" !nth (Token.rawtoken_to_string rt) !lv;
          if !flag then
            l := rt :: !l;
          incr nth
        done;
        assert false
      with
      | Exit -> !nth, !l

    method peek_rawtoken_up_to_end_of_qualified_id ?(from=1) ?(ini_tlv=0) () =
      let nth = ref from in
      let tlv = ref ini_tlv in
      let ident = T.IDENT "" in
      let stat = ref T.COLON_COLON in
      let chk rt =
        match !stat with
        | COLON_COLON -> begin
            stat := ident;
            T.COLON_COLON == rt
        end
        | IDENT _ -> begin
            stat := T.COLON_COLON;
            match rt with
            | IDENT _ -> true
            | _ -> false
        end
        | _ -> false
      in
      try
        while true do
          let rt = self#peek_nth_rawtoken !nth in
          begin
            match rt with
            | TEMPL_LT -> incr tlv;
            | TY_TEMPL_GT -> decr tlv
            | GT_GT when !tlv > 1 -> decr tlv; decr tlv
            | _ when !tlv = 0 ->
                if not (chk rt) then
                  raise Exit
            | SEMICOLON _ | RBRACE | EOF
            | PP_IF | PP_IFDEF | PP_IFNDEF | PP_ELIF | PP_ELSE | PP_ENDIF
              -> failwith "Scanner.F.c#peek_rawtoken_up_to_end_of_qualified_id"
            | _ -> ()
          end;
          DEBUG_MSG "%d: %s" !nth (Token.rawtoken_to_string rt);
          incr nth
        done;
        assert false
      with
      | Exit -> !nth - 1

    method is_lparen ?(from=1) ?(ignore_pp=false) () =
      let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from ~ignore_pp () in
      let followed_by_lbrace = ref false in
      let b =
        match self#peek_nth_rawtoken (nth+1) with
        | VIRTUAL | TYPEDEF | EXPLICIT | STATIC | RBRACE
        | STRUCT | UNION | CLASS | ENUM -> DEBUG_MSG "@"; true
        | LBRACE -> followed_by_lbrace := true; false
        (*| IDENT _ -> begin
            match self#peek_nth_rawtoken (nth+2) with
            | TY_LPAREN -> self#is_lparen ~from:(nth+3) ()
            | _ -> false
        end*)
        | _ -> false
      in
      let b =
        b ||
        List.exists
          (fun l ->
            DEBUG_MSG "l=%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
            match (l : T.token list) with
            (*| RPAREN::IDENT _::COMMA::_ -> true*)
            | [IDENT _;IDENT _;DOT;IDENT _] -> true
            | l when ty_pat3 l -> false
            | IDENT _::rest when ty_pat3 rest -> false
            | RPAREN::IDENT _::(PTR_STAR)::RPAREN::RPAREN::TY_LPAREN::_ -> DEBUG_MSG "@"; true
            | RPAREN::TY_LPAREN::IDENT _::x::_ when is_basic_ty x || !followed_by_lbrace -> false
            | RPAREN::TY_LPAREN::IDENT _::IDENT _::x::_ when is_basic_ty x -> false
            | RPAREN::TY_LPAREN::IDENT _::IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP|HAT _)::x::_ when is_basic_ty x -> false
            | RPAREN::TY_LPAREN::RPAREN::(PTR_STAR|PTR_AMP|PTR_AMP_AMP|HAT _)::_ -> false
            | RPAREN::TY_LPAREN::RPAREN::IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP|HAT _)::_ -> false
            | RPAREN::TY_LPAREN::_ when not (List.memq T.EQ l) -> DEBUG_MSG "@"; true
            | x::rest when is_literal x && List.memq T.EQ rest -> false
            | x::_ when is_literal x -> DEBUG_MSG "@"; true
            | [RPAREN;x;TY_LPAREN;IDENT _] when is_literal x -> DEBUG_MSG "@"; true
            | _ ->
                match List.rev l with
                | x::_ when is_decl_spec x -> false
                | x::_ when is_literal x -> DEBUG_MSG "@"; true
                | DECLTYPE::_ -> false
                | IDENT _::IDENT _::_ -> false
                | (PLUS|MINUS)::_
                | _::(PLUS|MINUS|SLASH|PERC|EQ_EQ|EXCLAM_EQ _|LT_EQ|GT_EQ)::_::_ -> DEBUG_MSG "@"; true
                | (PP_IF|PP_IFDEF|PP_IFNDEF)::_ -> false
                | _ ->
                    not (list_memqn [T.EQ;DECLTYPE;TEMPL_LT;TY_TEMPL_GT;TEMPL_GT;LBRACKET;RBRACKET] l) &&
                    (list_memqn [T.EQ_EQ;LT_EQ;GT_EQ;PLUS;MINUS;SLASH;PERC;LT_LT] l ||
                    List.exists
                      (function
                        | T.EXCLAM_EQ _ -> true
                        | _ -> false
                      ) l)
          ) ll
      in
      DEBUG_MSG "b=%B" b;
      b

    method is_ty ?(weak=false) x =
      match (x : T.token) with
      | x when is_basic_ty x -> true
      | AUTO -> true
      | IDENT x -> is_type_name x || self#is_type ~weak x
      | _ -> false

    method check_if_noptr_dtor ?(weak=false) l = (* assumes rev *)
      match (l : T.token list) with
      | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::[]
      | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::COLON_COLON::_
      | CONST::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::[]
      | CONST::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::COLON_COLON::_
      | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::[]
      | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::COLON_COLON::_
      | IDENT _::CONST::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::[]
      | IDENT _::CONST::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::COLON_COLON::_
      | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::(MS_CDECL _|MS_STDCALL _|CC_MACRO _)::[]
      | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::(MS_CDECL _|MS_STDCALL _|CC_MACRO _)::[]
        -> true
      | _ -> false

    method check_if_noptr_dtor_ ?(weak=false) l = (* assumes fwd *)
      match (l : T.token list) with
      | TY_LPAREN::r -> begin
          match (r : T.token list) with
          | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::RPAREN::_
          | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::RPAREN::_
          | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::CONST::RPAREN::_
          | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::CONST::RPAREN::_
          | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::RPAREN::_
          | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::RPAREN::_
          | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::CONST::IDENT _::RPAREN::_
          | IDENT _::COLON_COLON::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::CONST::IDENT _::RPAREN::_
          | (MS_CDECL _|MS_STDCALL _|CC_MACRO _)::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::RPAREN::_
          | (MS_CDECL _|MS_STDCALL _|CC_MACRO _)::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::RPAREN::_
            -> true
          | _ -> false
      end
      | _ -> false

    method check_if_param ?(weak=false) l = (* assumes rev *)
      let is_ty = self#is_ty ~weak in
      DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
      let b =
        match (l : T.token list) with
        | [IDENT _] when weak -> true
        | [ELLIPSIS] -> true
        | TY_TEMPL_GT::RPAREN::IDENT _::IDENT _::_ -> true
        | x::EQ::_ when is_literal x -> true
        | x::_ when is_ty x -> true
        | IDENT _::x::_ when is_ty x -> true
        | [IDENT _;x] when is_ty x -> true
        | [IDENT _;IDENT _] when weak -> true
        | [IDENT _;IDENT _;COLON_COLON;IDENT _] when weak -> true
        | IDENT _::IDENT _::COLON_COLON::IDENT _::rest when begin
            weak && List.for_all (function T.IDENT _ | COLON_COLON -> true | _ -> false) rest
        end -> true
        | [IDENT _;STRUCT|UNION|CLASS|ENUM] -> true
        | [IDENT _;IDENT _;STRUCT|UNION|CLASS|ENUM] -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;x] when is_ty x -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;x] when is_ty x -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;x;CONST] when is_ty x -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;x;CONST] when is_ty x -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _] -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _] -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _;CONST] -> true
        | [PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _;CONST] -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;x] when is_ty x -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;x] when is_ty x -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;x;CONST] when is_ty x -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;x;CONST] when is_ty x -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _] when weak -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _;COLON_COLON;IDENT _] when weak -> true
        | IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::IDENT _::COLON_COLON::IDENT _::rest when begin
            weak && List.for_all (function T.IDENT _ | COLON_COLON -> true | _ -> false) rest
        end -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _;STRUCT|UNION|CLASS|ENUM] when weak -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _] when weak -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _;CONST] when weak -> true
        | [IDENT _;PTR_STAR|PTR_AMP|PTR_AMP_AMP;PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _;CONST] when weak -> true
        | [RBRACKET;LBRACKET;x] when is_ty x -> true
        | RBRACKET::IDENT _::LBRACKET::RPAREN::IDENT _::(PTR_STAR|PTR_AMP|PTR_AMP_AMP)::TY_LPAREN::IDENT x::_ when
            is_type_name x || weak -> true
        | [IDENT _;RBRACKET;LBRACKET;x] when is_ty x -> true
        | [IDENT _;RBRACKET;LBRACKET;IDENT _] -> true
        | [RBRACKET;_;LBRACKET;x] when is_ty x -> true
        | [IDENT _;RBRACKET;_;LBRACKET;x] when is_ty x -> true
        | [IDENT _;RBRACKET;_;LBRACKET;IDENT _] -> true
        | _::_ when begin
            match Xlist.last l with
            | CONST -> true
            | IDENT x when is_type_macro_ident x -> true
            | _ -> false
        end -> true
        | _ -> false
      in
      DEBUG_MSG "b=%B" b;
      b

    method check_if_params ?(weak=false) = List.for_all (self#check_if_param ~weak)

    method check_if_macro_arg l = (* assumes rev *)
      DEBUG_MSG "%s" (String.concat ";" (List.map Token.rawtoken_to_string l));
      let b =
        match (l : T.token list) with
        | [] -> true
        | [x] when is_basic_ty x -> true
        | [x] when is_op x || is_uop x -> true
        | [IDENT x] when is_type_name x || self#is_type x -> true
        | [IDENT _; IDENT x] when is_type_name x || self#is_type x -> true
        | [TY_LPAREN] -> true
        | [RPAREN] -> true
        | [RPAREN;TY_LPAREN] -> true
        | [RPAREN;IDENT _;TY_LPAREN] -> false
        | RPAREN::rest when begin
            match List.rev rest with
            | TY_LPAREN::x when not (list_memqn [T.TY_LPAREN;RPAREN] x) -> begin
                let ll = split_at_comma x in
                DEBUG_MSG "\n%s"
                  (String.concat "\n"
                     (List.map
                        (fun x -> String.concat ";"
                            (List.map Token.rawtoken_to_string x)) ll));
                self#check_if_params ~weak:true ll
            end
            | _ -> false
        end -> true
        | [(DOT|MINUS_GT|DOT_STAR|MINUS_GT_STAR);IDENT _] -> true
        | SEMICOLON _::_ -> true
        | RETURN::_ -> true
        | LT_LT::_ -> true
        | RBRACE::SEMICOLON _::r when Xlist.last r == LBRACE -> true
        (*| [PTR_STAR|PTR_AMP|PTR_AMP_AMP;IDENT _] -> false*)
        | (PTR_STAR|PTR_AMP|PTR_AMP_AMP)::r -> not (list_memqn [T.TEMPL_LT;TY_TEMPL_GT] r)
              (*| _ when list_memqn [T.SEMICOLON] l -> true*)
        | _ -> begin
            match List.rev l with
            | DOT::_ -> true
            | x::y::r when
                is_basic_ty x && y != TY_LPAREN && not (list_memqn [T.PTR_STAR;PTR_AMP] r)
              -> true
            | IDENT x::y::r when
                (is_type_name x || self#is_type x) &&
                y != TY_LPAREN && y != TEMPL_LT &&
                not (list_memqn [T.PTR_STAR;PTR_AMP] r)
              -> true
            | (TYPENAME|STRUCT|CLASS|UNION|ENUM|RETURN|DEFAULT|GOTO|CASE|THROW|IF)::_ -> true
                  (*| TY_LPAREN::_ -> true*)
            | LBRACE::r when List.exists is_semicolon r -> true
            | _ -> false
        end
      in
      DEBUG_MSG "b=%B" b;
      b

    method check_if_macro_args = List.exists self#check_if_macro_arg

    method skip_pp ?(limit=(-1)) from =
      let n, _ = self#peek_rawtoken_up_to ~from [T.NEWLINE] in
      match self#peek_nth_rawtoken (n+1) with
      | PP_IF | PP_IFDEF | PP_IFNDEF
      | PP_ELIF | PP_ELSE(* | PP_ENDIF*)
      | PP_DEFINE | PP_UNDEF when limit > 0 || limit < 0 -> begin
          let limit =
            if limit > 0 then
              limit - 1
            else
              limit
          in
          self#skip_pp ~limit (n+2)
      end
      | _ -> n


    method get_token () =
      try
        self#_get_token()
      with
        To_be_recovered -> self#get_token()

    val token_hist_tbl = Hashtbl.create 0

    method show_token_hist () =
      let total = ref 0 in
      let l =
        Hashtbl.fold
          (fun rt c a ->
            total := !total + c;
            ((Token.rawtoken_to_string rt), c)::a
          ) token_hist_tbl []
      in
      List.iter
        (fun (k, v) ->
          Printf.printf "%s: %d (%.2f%%)\n" k v ((float v)*.100.0/.(float !total))
        ) (List.fast_sort (fun (_, v0) (_, v1) -> compare v0 v1) l)

    method private _get_token () =
      let token =
        if replay_flag then
          try
            self#replay ~mix:(keep_flag && replay_flag) ()
          with
            Queue.Empty -> self#__get_token()
        else
          self#__get_token()
      in

      let token =
        let (_rawtok, stp, edp) = token in
        let mk x = (x, stp, edp) in

        if token_hist_flag then begin
          let rt =
            match _rawtok with
            | IDENT _ -> T.IDENT ""
            | STR_LITERAL _ -> T.STR_LITERAL ""
            | CHAR_LITERAL _ -> T.CHAR_LITERAL ""
            | INT_LITERAL _ -> T.INT_LITERAL ""
            | FLOAT_LITERAL _ -> T.FLOAT_LITERAL ""
            | BOOL_LITERAL _ -> T.BOOL_LITERAL ""
            | _ -> _rawtok
          in
          try
            let c = Hashtbl.find token_hist_tbl rt in
            Hashtbl.replace token_hist_tbl rt (c+1)
          with
            Not_found ->
              Hashtbl.add token_hist_tbl rt 1
        end;

        DEBUG_MSG "[%s][%s]" (C.to_string context) (C.sub_to_string sub_context);
        DEBUG_MSG "%s" (levels_to_str env);

        BEGIN_DEBUG
          if env#pp_if_section_level > 0 then begin
            DEBUG_MSG "pp_if_section_rel_brace_level:%d" env#pp_if_section_rel_brace_level;
            let info = env#pp_if_section_top_info in
            DEBUG_MSG "%s" (Pinfo.pp_if_section_info_to_string info);
          end;
          if env#pp_odd_if_section_level > 0 then begin
            DEBUG_MSG "pp_odd_if_section_rel_brace_level:%d" env#pp_odd_if_section_rel_brace_level;
          end;
          if self#check_top_stmts_flag then
            DEBUG_MSG "[check_top_stmts]";
          DEBUG_MSG "odd_brace_lv_stack=%s" env#pstat#odd_brace_lv_stack_to_string;
        END_DEBUG;

        DEBUG_MSG "%s" (flags_to_str env);
        DEBUG_MSG "scope: %s" env#stack#scopes_to_string;
        DEBUG_MSG "block level: %d" env#stack#block_level;

        DEBUG_MSG "prev4=%s" (Token.rawtoken_to_string prev_rawtoken4);
        DEBUG_MSG "prev3=%s" (Token.rawtoken_to_string prev_rawtoken3);
        DEBUG_MSG "prev2=%s" (Token.rawtoken_to_string prev_rawtoken2);
        DEBUG_MSG "prev1=%s" (Token.rawtoken_to_string prev_rawtoken);
        DEBUG_MSG "%s" (Token.rawtoken_to_string _rawtok);
        let str = ref "" in
        let _rawtok_orig = _rawtok in
        let _rawtok, token =
          let get () =
            let rt = T.IDENT (Token.rawtoken_to_repr _rawtok) in
            rt, mk rt
          in
          match _rawtok with
          | DEFINED | HAS_INCLUDE | HAS_CPP_ATTRIBUTE when not env#pp_line_flag || begin
              match prev_rawtoken with
              | PP_IFDEF | PP_IFNDEF -> true
              | _ -> false
          end -> get()
          | ASSERT | ENSURES | EXPECTS when
              prev_rawtoken == LBRACKET && prev_rawtoken2 == ATTR_LBRACKET -> _rawtok, token
          | ASSERT | ENSURES | EXPECTS -> get()
          | AUDIT | AXIOM when begin
              match prev_rawtoken with
              | ASSERT | ENSURES | EXPECTS -> false
              | _ -> true
          end -> get()
          | FINAL | OVERRIDE -> begin
              match prev_rawtoken with
              | PP_DEFINE | PP_UNDEF | PP_IFDEF | PP_IFNDEF | EXCLAM _ | BOOL | RETURN
              | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | LT_LT_EQ | GT_GT_EQ
              | AMP_EQ _ | HAT_EQ _ | BAR_EQ _ | AMP _ | BAR _ | BAR_BAR _ | AMP_AMP _
              | LT | GT | LT_EQ | GT_EQ | EQ_EQ | EXCLAM_EQ _ | QUEST -> DEBUG_MSG "@"; get()
              | TY_TEMPL_GT when begin
                  let next = self#peek_rawtoken() in
                  env#class_name_flag && next == LBRACE ||
                  not env#trailing_retty_flag && is_semicolon next
              end -> DEBUG_MSG "@"; get()
              | LPAREN when begin
                  match self#peek_rawtoken() with
                  | RPAREN | PTR_AMP_AMP | BAR_BAR _ | PTR_AMP | BAR _ | COMMA
                  | PLUS | MINUS | SLASH | PERC | QUEST -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; get()
              | COMMA when begin
                  match self#peek_rawtoken() with
                  | COMMA | RPAREN | SEMICOLON _ -> true
                  | PLUS | MINUS | SLASH | PERC -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; get()
              | RPAREN when env#end_of_cast_type_flag -> DEBUG_MSG "@"; get()
              | RPAREN -> _rawtok, token
              | IDENT _ when begin
                  match prev_rawtoken2 with
                  | SEMICOLON _ | LBRACE | RBRACE -> true
                  | COMMA when env#type_paren_flag && begin
                      match self#peek_rawtoken() with
                      | COMMA | RPAREN -> true
                      | _ -> false
                  end -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; get()
              | x when x != CONST && is_basic_ty x && begin
                  match self#peek_rawtoken() with
                  | SEMICOLON _ -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; get()

              | PTR_STAR | PTR_AMP | PTR_AMP_AMP when
                  not env#trailing_retty_flag && not env#end_of_params_flag
                -> DEBUG_MSG "@"; get()

              | IDENT x when (is_type_name x || self#is_type x) && begin
                  match self#peek_rawtoken() with
                  | SEMICOLON _ -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; get()
              | _ -> begin
                  match self#peek_rawtoken() with
                  | TY_LPAREN | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
                  | LT_LT_EQ | GT_GT_EQ | AMP_EQ _ | HAT_EQ _ | BAR_EQ _
                  | EQ_EQ | EXCLAM_EQ _ | LBRACKET | MINUS_GT | DOT | COMMA -> DEBUG_MSG "@"; get()
                  | EQ when context != MEM || begin
                      match self#peek_nth_rawtoken 2 with
                      | INT_LITERAL "0" -> false
                      | _ -> true
                  end -> DEBUG_MSG "@"; get()
                  | _ -> _rawtok, token
              end
          end
          | CLASS | ENUM | STRUCT | UNION | PRIVATE | PUBLIC | PROTECTED | THIS when begin
              match prev_rawtoken with
              | MINUS_GT | DOT | AMP _ | STAR | EQ_EQ | EXCLAM_EQ _ -> true
              | EQ when begin
                  match self#peek_rawtoken() with
                  | SEMICOLON _ | COMMA -> true
                  | _ -> false
              end -> true
              | TY_LPAREN | LPAREN when self#peek_rawtoken() == RPAREN -> true
              | COMMA when begin
                  match self#peek_rawtoken() with
                  | RPAREN | COMMA | SEMICOLON _ -> true
                  | _ -> false
              end -> true
              | PTR_STAR | PTR_AMP | PTR_AMP_AMP when begin
                  match prev_rawtoken2 with
                  | TY_LPAREN | IDENT _ | COMMA -> begin
                      match self#peek_rawtoken() with
                      | RPAREN | COMMA | SEMICOLON _ -> true
                      | _ -> false
                  end
                  | _ when self#peek_rawtoken() == COMMA -> true
                  | _ -> false
              end -> true
              | x when is_basic_ty x && begin
                  match self#peek_rawtoken() with
                  | SEMICOLON _ | EQ | COMMA -> true
                  | _ -> false
              end -> true
              | IDENT x when(* is_type_name x &&*) begin
                  match self#peek_rawtoken() with
                  | SEMICOLON _ | EQ | COMMA | RPAREN -> true
                  | _ -> false
              end -> true
              | _ when begin
                  match self#peek_rawtoken() with
                  | DOT | MINUS_GT | EQ_EQ | EXCLAM_EQ _
                  | TEMPL_LT (*| TY_TEMPL_GT *)| LT_EQ | GT_EQ
                  | BAR_BAR _ | PTR_AMP_AMP | PTR_AMP | BAR _
                  | PTR_STAR | SLASH | PERC | PLUS | MINUS
                  | RBRACKET when _rawtok != THIS -> true
                  | COMMA -> true
                  | EQ when not env#templ_head_flag -> true
                  | _ -> false
              end -> true
              | _ -> false
          end -> DEBUG_MSG "@"; get()
          | BREAK | CASE | CATCH | CONST | CONTINUE | DEFAULT | DELETE | DO | ELSE | FOR | IF | NEW | RETURN
          | SWITCH | THIS | THROW | TRY | VOID | WHILE | CLASS | ENUM | PRIVATE | PROTECTED | PUBLIC when begin
              env#macro_arg_flag &&
              match prev_rawtoken with
              | LPAREN | COMMA -> begin
                  match self#peek_rawtoken() with
                  | COMMA | RPAREN -> true
                  | _ -> false
              end
              | _ -> false
          end -> DEBUG_MSG "@"; get()

          | RETURN when begin
              match self#peek_rawtoken() with
              | GT_EQ | LT_EQ | TEMPL_LT | TY_TEMPL_GT | EQ_EQ | EXCLAM_EQ _ -> true
              | _ -> false
          end -> DEBUG_MSG "@"; get()

          | CLASS when env#in_objc_message_expr -> DEBUG_MSG "@"; get()
          | IF -> begin
              match prev_rawtoken with
              | COMMA when begin
                  match self#peek_rawtoken() with
                  | COMMA | RPAREN -> true
                  | _ -> false
              end -> DEBUG_MSG "@"; get()
              | _ -> DEBUG_MSG "@"; _rawtok, token
          end

          | NEW when env#objc_class_interface_flag && prev_rawtoken == RPAREN -> DEBUG_MSG "@"; get()

          | NEW | NAMESPACE | THIS when begin
              match prev_rawtoken with
              | PTR_STAR | STRUCT -> true
              | COMMA when env#dtor_flag -> true
              | _ -> false
          end -> DEBUG_MSG "@"; get()
          | TEMPLATE | BOOL_LITERAL _ | DEFAULT when begin
              match self#peek_rawtoken() with
              | EQ | EQ_EQ | EXCLAM_EQ _ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
              | LT_LT_EQ | GT_GT_EQ | AMP_EQ _ | HAT_EQ _ | BAR_EQ _
              | LT_EQ | GT_EQ | BAR_BAR _ | PTR_AMP_AMP | PTR_AMP | BAR _
              | SLASH | PERC | PLUS | MINUS
              | DOT | MINUS_GT | LBRACKET -> true
              | PTR_STAR -> begin
                  match self#peek_nth_rawtoken 2 with
                  | INT_LITERAL _ | USER_INT_LITERAL _ -> true
                  | _ -> false
              end
              | _ -> false
          end -> DEBUG_MSG "@"; get()
          | BOOL when begin
              match self#peek_rawtoken() with
              | QUEST | EQ | EQ_EQ | EXCLAM_EQ _ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
              | LT_LT_EQ | GT_GT_EQ | AMP_EQ _ | HAT_EQ _ | BAR_EQ _ | PLUS | MINUS | SLASH | PERC
              | DOT | MINUS_GT -> true
              | RPAREN when prev_rawtoken == LPAREN && prev_rawtoken2 == IF -> true
              | RPAREN when begin
                  match prev_rawtoken with
                  | EQ_EQ | EXCLAM_EQ _ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
                  | LT_LT_EQ | GT_GT_EQ | AMP_EQ _ | HAT_EQ _ | BAR_EQ _ | PLUS | MINUS | SLASH | PERC
                  | DOT | MINUS_GT -> true
                  | _ -> false
              end -> true
              | _ -> false
          end -> DEBUG_MSG "@"; get()
          | NEW | DELETE when begin
              match self#peek_rawtoken() with
              | QUEST | EQ | EQ_EQ | EXCLAM_EQ _ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ
              | LT_LT_EQ | GT_GT_EQ | AMP_EQ _ | HAT_EQ _ | BAR_EQ _ | PLUS | MINUS | SLASH | PERC
              | DOT | MINUS_GT -> true
              | PTR_STAR -> begin
                  match self#peek_nth_rawtoken 2 with
                  | INT_LITERAL _ | USER_INT_LITERAL _ -> true
                  | IDENT _ -> true
                  | _ -> false
              end
              | _ -> false
          end -> DEBUG_MSG "@"; get()
          | NEW when prev_rawtoken != OPERATOR && begin
              match self#peek_rawtoken() with
              | SEMICOLON _ | RPAREN | COMMA -> true
              | _ -> false
          end -> DEBUG_MSG "@ (SEMICOLON|RPAREN|COMMA)"; get()
          | DELETE when begin
              prev_rawtoken != OPERATOR && prev_rawtoken != EQ &&
              match self#peek_rawtoken() with
              | SEMICOLON _ | RPAREN | COMMA -> true
              | _ -> false
          end -> DEBUG_MSG "@ (SEMICOLON|RPAREN|COMMA)"; get()
          | DEFAULT when begin
              (prev_rawtoken != EQ || prev_rawtoken2 != NEWLINE) &&
              match self#peek_rawtoken() with
              | RPAREN | COMMA -> true
              | SEMICOLON _ when not env#pp_define_body_flag && not env#end_of_params_flag -> true
              | _ -> false
          end -> DEBUG_MSG "@ (SEMICOLON|RPAREN|COMMA)"; get()
          | TYPENAME when env#templ_head_flag -> DEBUG_MSG "@"; _rawtok, token
          | TEMPLATE | EXPORT | TYPENAME when begin
              not env#pp_line_flag &&
              match self#peek_rawtoken() with
              | RPAREN | COMMA -> true
              | EQ when env#templ_param_arg_level = 0 -> true
              | SEMICOLON _ when not env#end_of_params_flag -> true
              | _ -> false
          end -> DEBUG_MSG "@ (SEMICOLON|RPAREN|COMMA)"; get()
          | TYPENAME when begin
              env#pp_params_flag &&
              match self#peek_rawtoken() with
              | RPAREN | COMMA -> true
              | _ -> false
          end -> DEBUG_MSG "@ (RPAREN|COMMA)"; get()
          | BOOL when begin
              match prev_rawtoken with
              | PP_DEFINE | PP_UNDEF | PP_IFDEF | PP_IFNDEF -> true
              | LPAREN when prev_rawtoken2 == DEFINED -> true
              | STAR | AMP _ -> true
              | MINUS_GT when begin
                  match self#peek_rawtoken() with
                  | COMMA -> true
                  | _ -> false
              end -> true
              | _ when env#pp_params_flag -> true
              | _ -> false
          end -> DEBUG_MSG "@ (SEMICOLON|RPAREN|COMMA)"; get()
          | NEW | DELETE | FOR | CONST | VOLATILE | BOOL_LITERAL _ | THIS | DEFAULT
          | INLINE | INT | DOUBLE | FLOAT | SHORT | LONG | VOID | STATIC | REGISTER | DECLTYPE
          | STATIC_ASSERT | STRUCT | UNION | CATCH | NULLPTR | CLASS | ENUM | RETURN
          | HAS_CPP_ATTRIBUTE | HAS_INCLUDE
          | MS_STDCALL _ | MS_CDECL _ | CC_MACRO _ | GNU_ATTR _ when begin
              match prev_rawtoken with
              | PP_DEFINE | PP_UNDEF | PP_IFDEF | PP_IFNDEF -> true
              | LPAREN when prev_rawtoken2 == DEFINED -> true
              | EQ when env#arg_paren_flag -> true
              | _ when env#pp_params_flag -> true
              | _ -> false
          end -> DEBUG_MSG "@ (SEMICOLON|RPAREN|COMMA)"; get()
          | NEW when prev_rawtoken != OPERATOR && begin
              match self#peek_rawtoken() with
              | TY_LPAREN -> begin
                  let _, nth, l = self#peek_rawtoken_up_to_rparen ~from:2 None in
                  match self#peek_nth_rawtoken (nth+1) with
                  | MINUS_GT | DOT | SEMICOLON _ -> true
                  | _ -> false
              end
              | LBRACKET -> begin
                  let nth, l = self#peek_rawtoken_up_to_rbracket ~from:2 () in
                  match self#peek_nth_rawtoken (nth+1) with
                  | EQ -> true
                  | _ -> false
              end
              | TY_TEMPL_GT when prev_rawtoken == LPAREN -> true
              | TEMPL_LT when prev_rawtoken == LPAREN && begin
                  let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                  not (list_memqn [T.TY_TEMPL_GT] l)
              end -> true
              | _ -> false
          end -> DEBUG_MSG "@ (TY_LPAREN|LBRACKET|TY_TEMPL_GT|TEMPL_LT)"; get()
          | OPERATOR | PUBLIC | PRIVATE | PROTECTED when begin
              match self#peek_rawtoken() with
              | DOT | SEMICOLON _ | RPAREN -> true
              | EQ when begin
                  match prev_rawtoken with
                  | MINUS_GT | DOT when begin
                      match self#peek_nth_rawtoken 2 with
                      | TEMPL_LT -> false
                      | _ -> true
                  end -> true
                  | COLON -> true
                  | _ -> false
              end -> true
              | EQ -> begin
                  match self#peek_nth_rawtoken 2 with
                  | IDENT _ -> true
                  | _ -> false
              end
              | COMMA when env#type_paren_flag || env#arg_paren_flag -> true
              | x when begin
                  match prev_rawtoken with
                  | MINUS_GT | DOT when begin
                      match x with
                      | IDENT _ | TYPENAME -> false

                      | LBRACKET when begin
                          self#peek_nth_rawtoken 2 == RBRACKET &&
                          match self#peek_nth_rawtoken 3 with
                          | TEMPL_LT | TY_LPAREN -> true
                          | _ -> false
                      end -> false

                      | LPAREN | TY_LPAREN when begin
                          self#peek_nth_rawtoken 2 == RPAREN &&
                          match self#peek_nth_rawtoken 3 with
                          | TEMPL_LT | TY_LPAREN -> true
                          | _ -> false
                      end -> false

                      | TEMPL_LT | TY_TEMPL_GT | PTR_STAR | PTR_AMP | PTR_AMP_AMP
                      | NEW | DELETE | MINUS_GT | MINUS_GT_STAR | TILDE _ | EXCLAM _
                      | PLUS | MINUS | STAR | SLASH | PERC | HAT _ | AMP _ | BAR _ | CO_AWAIT | OP_MACRO _
                      | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
                      | EQ_EQ | EXCLAM_EQ _ | LT | GT | LT_EQ | GT_EQ | LT_EQ_GT | AMP_AMP _ | BAR_BAR _
                      | LT_LT | GT_GT | LT_LT_EQ | GT_GT_EQ | PLUS_PLUS | MINUS_MINUS when begin
                          match self#peek_nth_rawtoken 2 with
                          | TEMPL_LT | TY_LPAREN -> true
                          | _ -> false
                      end -> false

                      | _ when is_basic_ty x -> false
                      | _ -> true
                  end -> true
                  | LPAREN -> begin
                      match prev_rawtoken2 with
                      | SWITCH -> true
                      | _ -> false
                  end
                  | _ -> false
              end -> true
              | _ -> false
          end -> get()
          | EXPLICIT | THREAD_LOCAL | CATCH when begin
              match prev_rawtoken with
              | LPAREN | COMMA -> true
              | AMP_AMP _ | BAR_BAR _ | PLUS | MINUS | STAR | SLASH | PERC
              | HAT _ | AMP _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
              | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
              | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
              | LT | GT | TEMPL_LT | TY_TEMPL_GT | EXCLAM _
              | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> true
              | _ ->
              match self#peek_rawtoken() with
              | COMMA | RPAREN -> true
              | AMP_AMP _ | BAR_BAR _ | PLUS | MINUS | STAR | SLASH | PERC
              | HAT _ | AMP _ | BAR _ | LT_LT | GT_GT | PLUS_EQ | MINUS_EQ
              | STAR_EQ | SLASH_EQ | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _
              | LT_LT_EQ | GT_GT_EQ | EQ | EQ_EQ | EXCLAM_EQ _ | LT_EQ | GT_EQ
              | LT | GT | TEMPL_LT | TY_TEMPL_GT
              | DOT | MINUS_GT | DOT_STAR | MINUS_GT_STAR -> true
              | _ -> false
          end -> get()
          | TRY when begin
              match self#peek_rawtoken() with
              | LBRACE | COLON -> false
              | PP_ENDIF -> false
              | _ -> true
          end -> get()
          | HAT s | AMP_AMP s | BAR_BAR s | EXCLAM s | AMP s | BAR s | TILDE s
          | AMP_EQ s | BAR_EQ s | HAT_EQ s | EXCLAM_EQ s when begin
              let c = Char.code s.[0] in 
              97 <= c && c <= 123 &&
              match prev_rawtoken with
              | CHAR | CHAR8_T| CHAR16_T| CHAR32_T | WCHAR_T | BOOL | SHORT | INT | LONG
              | FLOAT | DOUBLE | VOID | TYPE_MACRO _ | CONST | UNSIGNED -> true
              | EQ -> true
              | _ -> begin
                  match self#peek_rawtoken() with
                  | EQ | LBRACKET -> true
                  | _ -> false
              end
          end -> begin
            let rt = T.IDENT s in
            rt, mk rt
          end
          | CHAR8_T when begin
              match prev_rawtoken with
              | ENUM -> true
              | _ -> false
          end -> get()
          | QUEST when begin
              match prev_rawtoken with
              | TY_LPAREN | LPAREN | COMMA when begin
                  match self#peek_rawtoken() with
                  | COMMA | RPAREN -> str := "?"; true
                  | QUEST -> begin
                      match self#peek_nth_rawtoken 2 with
                      | COMMA | RPAREN -> begin
                          let _ = self#discard_token() in
                          str := "??";
                          true
                      end
                      | _ -> false
                  end
                  | _ -> false
              end -> true
              | INI_LBRACE | COMMA when begin
                  match self#peek_rawtoken() with
                  | COMMA | RBRACE -> str := "?"; true
                  | QUEST -> begin
                      match self#peek_nth_rawtoken 2 with
                      | COMMA | RBRACE -> begin
                          let _ = self#discard_token() in
                          str := "??";
                          true
                      end
                      | _ -> false
                  end
                  | _ -> false
              end -> true
              | _ -> false
          end -> begin
            let rt = T.IDENT (!str) in
            rt, mk rt
          end

          | CATCH when begin
              context == TOP && sub_context == END_OF_TY_SPEC
          end -> get()

          | ALIGNAS | ALIGNOF | CHAR16_T | CHAR32_T | CONSTEXPR | DECLTYPE | NOEXCEPT | NULLPTR(* | OVERRIDE*)
          | STATIC_ASSERT | THREAD_LOCAL when begin
              context == ENUM && sub_context == INI && env#brace_level = 1 &&
              (match prev_rawtoken with
              | LBRACE | COMMA -> true
              | _ -> false) &&
              match self#peek_rawtoken() with
              | COMMA | RBRACE -> true
              | _ -> false
          end -> get()

          | _ -> _rawtok, token
        in
        BEGIN_DEBUG
          if _rawtok <> _rawtok_orig then
            DEBUG_MSG "-> %s" (Token.rawtoken_to_string _rawtok)
        END_DEBUG;

        begin
          match _rawtok with
          | IDENT _ | RBRACE -> ()
          | _ -> begin
              match sub_context with
              | START_OF_STMT _ -> begin
                  self#clear_keep_flag();
                  self#clear_alternative_tokens()
              end
              | _ -> ()
          end
        end;

        let _rawtok, token =
          match sub_context with
          | START_OF_STMT _ -> begin
              match _rawtok with
              | IDENT_V s -> let rt = T.IDENT s in rt, mk rt
              | _ -> _rawtok, token
          end
          | _ when env#pp_line_flag -> begin
              match _rawtok with
              | AT -> begin
                  match self#peek_rawtoken() with
                  | PP_UNKNOWN s -> begin
                      let _, _, ep = self#discard_token() in
                      let rt = T.PP_STRINGIZED ("@"^s) in
                      rt, (rt, stp, ep)
                  end
                  | _ -> _rawtok, token
              end
              | PP_UNKNOWN s -> let rt = T.PP_STRINGIZED s in rt, mk rt
              | _ -> _rawtok, token
          end
          | _ -> _rawtok, token
        in

        peek_rawtoken_up_to_rparen_none_cache <- None;

        if env#asm_shader_flag then
          token
        else
          conv_token env self token

      in (* let token *)

      let begin_stmts mk stp edp =
        parse_warning env stp edp "a statement is about to begin in a wrong context";
        self#prepend_token token;
        self#prepend_token (mk T.BEGIN_STMTS);
        raise To_be_recovered
      in

      if
        check_top_stmts_flag && not env#top_stmts_flag &&
        not env#pp_line_flag && not env#pp_define_body_flag
      then begin
        DEBUG_MSG "@";
        let rawtok, stp, edp = token in
        let mk x = (x, stp, edp) in
        match rawtok with
        | _ when begin
            match context with
            | TOP | MEM -> true
            | _ -> false
        end -> begin
          DEBUG_MSG "@";
          match rawtok with
          | LBRACE when begin
              not env#end_of_params_flag &&
              not env#start_of_func_body_flag && not env#end_of_old_param_decl_flag &&
              match prev_rawtoken with
              | NEWLINE | SEMICOLON _ | EOF -> true
              | _ -> false
          end -> begin
            DEBUG_MSG "@";
            begin_stmts mk stp edp
          end
          | _ -> ()
        end
        | _ -> ()
      end;

      begin
        let rawtok, stp, edp = token in
        begin
          match rawtok with
          | IDENT _ | IDENT_V _ -> ()
          | IDENT_B s | IDENT_C s | IDENT_E s | IDENT_LPAREN s | IDENT_AGM s
          | IDENT_LOM s | IDENT_EM s | IDENT_SM s | IDENT_SXM s | IDENT_TM s | IDENT_IM s | IDENT_PM s
          | IDENT_CM s | IDENT_LM s | IDENT_AM s | IDENT_TPM s | IDENT_NSM s | IDENT_DSM s
          | IDENT_BHM s | IDENT_BEM s | IDENT_CHM s | IDENT_OM s | IDENT_DM s | IDENT_PDM s
          | IDENT_VM s
          | STR_MACRO s | DECL_MACRO s | STMT_MACRO s | VIRT_SPEC_MACRO s | OP_MACRO s
          | PARAMS_MACRO s | ARGS_MACRO s | ARG_MACRO s | NEW_INIT_MACRO s | ATTR_MACRO s
          | ACC_SPEC_MACRO s | DECL_SPEC_MACRO s | CV_MACRO s | NOEXCEPT_MACRO s | NS_MACRO s
          | EMPTY_MACRO s | DELIM_MACRO s | BASE_MACRO s | SUFFIX_MACRO s | BODY_MACRO s
          | BLOCK_HEAD_MACRO s | BLOCK_END_MACRO s | TYPE_MACRO s | CC_MACRO s
          | PARAM_DECL_MACRO s | PTR_MACRO s when is_capital_ident s -> begin
              self#reg_ident_conv s rawtok
          end
          | _ -> ()
        end;
        DEBUG_MSG " -> %s" (Token.rawtoken_to_string rawtok);
        let mk x = (x, stp, edp) in
        match rawtok with

        | AT when prev_rawtoken == EOF || env#asm_flag -> begin
          DEBUG_MSG "@";
          raise To_be_recovered
        end

        | INT_LITERAL i when begin
            env#braced_init_flag &&
            match self#peek_rawtoken() with
            | INT_LITERAL _ -> true
            | _ -> false
        end -> begin
          DEBUG_MSG "@";
          self#prepend_token (T.COMMA, edp, edp)
        end

        | LBRACKET | LAM_LBRACKET | ATTR_LBRACKET -> env#open_bracket()
        | RBRACKET -> env#close_bracket()

        | INI_LBRACE when context == TOP && prev_rawtoken == EOF -> begin
            DEBUG_MSG "@";
            self#prepend_token token;
            self#prepend_token (mk T.MARKER);
            raise To_be_recovered
        end
        | LBRACE | INI_LBRACE | CLASS_LBRACE when not env#pp_line_flag -> begin
            env#open_in_body_brace();
            env#open_brace();
        end

        | RBRACE when not env#pp_line_flag -> begin
            DEBUG_MSG "@";
            if env#pp_if_section_level > 0 then begin
              let ok =
                match self#peek_rawtoken() with
                | ELSE -> begin
                    match self#peek_nth_rawtoken 2 with
                    | LBRACE | IF -> false
                    | _ -> true
                end
                | _ -> true
              in
              if ok then begin
                let info = env#pp_if_section_top_info in
                DEBUG_MSG "info=%s" (Pinfo.pp_if_section_info_to_string info);
                let unmatched =
                  (info.Pinfo.i_context != TOP && not info.Pinfo.i_broken) &&
                  match info.Pinfo.i_pp_else with
                  | Some x -> begin
                      if env#brace_level = x then begin
                        match info.Pinfo.i_pp_elif with
                        | Some y -> y <= x
                        | None -> info.Pinfo.i_brace_level <= x
                      end
                      else
                        false
                  end
                  | None -> begin
                      match info.Pinfo.i_pp_elif with
                      | Some y -> begin
                          if env#brace_level = y then
                            info.Pinfo.i_brace_level <= y
                          else
                            false
                      end
                      | None -> false
                  end
                in
                DEBUG_MSG "unmatched=%B" unmatched;
                if unmatched then begin
                  (*parse_warning env stp edp "unmatched brace";*)
                  env#incr_rbrace_info();
                  self#prepend_token (mk T.ODD_RBRACE);
                  raise To_be_recovered
                end
              end
            end;

            if
              env#pp_odd_if_section_level > 0 &&
              env#odd_brace_level > 0 &&
              env#pp_odd_if_section_rel_brace_level = 0
            then begin
              parse_warning env stp edp "closing with #endif";
              env#close_odd_brace();
              self#prepend_token token;
              self#prepend_token (T.NEWLINE, edp, edp);
              self#prepend_token (T.PP_ENDIF_, edp, edp);
              raise To_be_recovered
            end;

            if env#brace_level > 0 then begin
              if env#in_body_brace_level > 0 then
                env#close_in_body_brace()
              else if env#end_of_class_spec_flag && not (is_semicolon prev_rawtoken) then begin
                parse_warning env stp edp "lack of semicolon";
                self#prepend_token token;
                self#prepend_token (mk (T.SEMICOLON false));
                raise To_be_recovered
              end;
              env#close_brace();
            end
            else if
              match self#peek_rawtoken() with
              | OBJC_CATCH -> true
              | _ -> false
            then begin
              parse_warning env stp edp "unmatched brace, complementing with \"@try {\"";
              self#prepend_token token;
              self#prepend_token (mk T.LBRACE);
              self#prepend_token (mk T.OBJC_TRY);
              raise To_be_recovered
            end
            else begin
              parse_warning env stp edp "unmatched brace";
              self#prepend_token (mk T.ODD_RBRACE);
              raise To_be_recovered
            end
        end
        | TY_LPAREN when prev_rawtoken == EOF && begin
            match context with
            | TOP | MEM -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma() in
                match self#peek_nth_rawtoken (nth+1) with
                | LBRACE | COLON -> true
                | _ -> false
            end
            | _ -> false
        end ->begin
          self#prepend_token token;
          self#prepend_token (mk T.DUMMY_DTOR);
          raise To_be_recovered
        end
        | TY_LPAREN -> begin
            let sub =
              match prev_rawtoken with
              | SIZEOF -> Aux.PKS_SIZEOF
              | _ -> Aux.PKS_NONE
            in
            env#open_paren (PK_TYPE sub)
        end
        | PP_LPAREN -> env#open_paren PK_PP
        | SS_LPAREN -> env#open_paren PK_SS
        | PS_LPAREN -> env#open_paren PK_PS
        | FOLD_LPAREN -> env#open_paren PK_F

        | LPAREN -> begin
            if not env#asm_flag then begin
              match prev_rawtoken with
              | IDENT_SM _ | IDENT_EM _ | IDENT_V _ -> begin
                  let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                  match (l : T.token list) with
                  | RBRACKET::IDENT _::LBRACKET::PERC::_
                  | RBRACKET::IDENT _::LBRACKET::IDENT _::PERC::_ -> begin
                      DEBUG_MSG "inserting END_ASM";
                      insert_after_nth_token self (nth-1) [mk T.END_ASM];
                      self#prepend_token (mk T.BEGIN_ASM)
                  end
                  | _ -> ()
              end
              | _ -> ()
            end;
            match prev_rawtoken with
            | IDENT_V s when s = "Q_PROPERTY" -> begin
                env#open_paren PK_NORMAL;
                self#set_q_prop_flag();
                self#prepend_token (mk T.BEGIN_QPROP)
            end
            | IDENT _ | IDENT_V _ | TEMPL_GT | STATIC_ASSERT -> env#open_paren PK_ARG
            | RPAREN when not env#end_of_cast_type_flag -> env#open_paren PK_ARG
            | _ -> env#open_paren PK_NORMAL
        end

        | RPAREN -> begin
            DEBUG_MSG "@";
            if self#q_prop_flag then begin
              self#clear_q_prop_flag();
              self#prepend_token token;
              self#prepend_token (mk T.END_QPROP);
              raise To_be_recovered
            end;
            env#exit_const();
            begin
              try
                match env#paren_stack_top with
                | PK_TYPE PKS_SIZEOF -> env#exit_sizeof_ty();
                | _ -> ()
              with
                _ -> ()
            end;
            try
              env#close_paren()
            with
              Stack.Empty -> begin
                let b =
                  env#pp_if_section_level > 0 &&
                  (env#pp_else_flag || env#pp_elif_flag) &&
                  env#get_paren_closing_info()
                in
                if b then
                  ()
                else begin
                  parse_warning env stp edp "unmatched parenthesis";
                  raise To_be_recovered
                end
              end
        end
        | EOF -> begin
            DEBUG_MSG "@";
            self#clear_keep_flag();
            self#clear_alternative_tokens();
            let close_brace ?(semicolon=false) () =
              self#prepend_token token;
              for i = 1 to env#brace_level do
                if semicolon then
                  self#prepend_token (mk (T.SEMICOLON false));
                self#prepend_token (mk T.RBRACE)
              done
            in
            let close_paren () =
              self#prepend_token token;
              for i = 1 to env#paren_level do
                self#prepend_token (mk T.RPAREN)
              done
            in
            let close_section () =
              self#prepend_token token;
              for i = 1 to env#_pp_if_section_level do
                self#prepend_token (T.NEWLINE, edp, edp);
                self#prepend_token (T.PP_ENDIF, edp, edp)
              done
            in
            if env#brace_level > 0 && env#paren_level = 0 then begin
              parse_warning env prev_stp prev_edp "lack of closing braces";
              let semicolon =
                env#stack#at_enum
              in
              close_brace ~semicolon ();
              begin
                match prev_rawtoken with
                | OVERRIDE | FINAL when env#end_of_params_flag -> self#prepend_token (mk (T.SEMICOLON false))
                | RPAREN -> self#prepend_token (mk (T.SEMICOLON false))
                | COLON -> begin
                    match sub_context with
                    | START_OF_STMT _ -> self#prepend_token (mk (T.SEMICOLON false))
                    | _ -> ()
                end
                | _ when begin
                    match prev_rawtoken2 with
                    | EQ -> begin
                        match prev_rawtoken3 with
                        | IDENT_V _ -> begin
                            match prev_rawtoken4 with
                            | IDENT _ -> true
                            | x -> is_basic_ty x
                        end
                        | _ -> false
                    end
                    | _ -> false
                end -> self#prepend_token (mk (T.SEMICOLON false))
                | _ -> ()
              end;
              raise To_be_recovered
            end
            else if env#brace_level = 0 && env#paren_level > 0 then begin
              parse_warning env prev_stp prev_edp "lack of closing parentheses";
              close_paren();
              raise To_be_recovered
            end
            else if env#brace_level > 0 && env#paren_level > 0 then begin
              match prev_rawtoken with
              | LBRACE -> begin
                  parse_warning env prev_stp prev_edp "lack of closing braces and parentheses";
                  close_paren();
                  close_brace();
                  raise To_be_recovered
              end
              | LPAREN -> begin
                  parse_warning env prev_stp prev_edp "lack of closing parentheses and braces";
                  close_brace();
                  close_paren();
                  raise To_be_recovered
              end
              | _ -> ()
            end;
            if env#top_stmts_flag then begin
              self#prepend_token token;
              self#prepend_token (mk T.END_STMTS);
              if prev_rawtoken == RPAREN then begin
                self#prepend_token (mk (T.SEMICOLON false));
              end;
              raise To_be_recovered
            end;
            if env#_pp_if_section_level > 0 then begin
              parse_warning env prev_stp prev_edp "closing with #endif";
              close_section();
              raise To_be_recovered
            end;
            if env#asm_flag && not macro_body_parsing_flag && prev_rawtoken != END_ASM then begin
              DEBUG_MSG "inserting END_ASM";
              self#prepend_token token;
              self#prepend_token (mk T.END_ASM);
              raise To_be_recovered
            end;
            if
              env#base_clause_flag &&
              match prev_rawtoken with
              | IDENT _ | TEMPL_GT | RPAREN -> true
              | _ -> false
            then begin
              DEBUG_MSG "@";
              self#prepend_token token;
              self#prepend_token (mk (T.SEMICOLON false));
              self#prepend_token (mk T.RBRACE);
              self#prepend_token (mk T.CLASS_LBRACE);
              raise To_be_recovered
            end;
            match prev_rawtoken with
            | PLUS | MINUS | STAR | SLASH | PERC
            | MINUS_GT | AMP _ | BAR _ | HAT _ | EXCLAM _ | TILDE _
            | AMP_AMP _ | BAR_BAR _ when context == EXPR -> begin
                self#prepend_token token;
                self#prepend_token (mk T.DUMMY_EXPR);
                raise To_be_recovered
            end
            | _ -> ()
        end

        | SHARP_SHARP when prev_rawtoken == COMMA && begin
            match self#peek_rawtoken() with
            | IDENT _ -> true
            | _ -> false
        end -> begin
          let _rt, _, ep = self#discard_token() in
          let rt =
            match _rt with
            | IDENT s -> T.IDENT ("## "^s)
            | _ -> assert false
          in
          self#prepend_token (rt, stp, ep);
          raise To_be_recovered
        end

        | IDENT_V _ when begin
            check_top_stmts_flag && not env#pp_line_flag && not env#pp_define_body_flag &&
            context == CLASS
        end -> begin
          match self#peek_rawtoken() with
          | EQ -> begin
              match self#peek_nth_rawtoken 2 with
              | LBRACE -> begin
                  match self#peek_nth_rawtoken 3 with
                  | PP_ELIF | PP_ELSE | PP_ENDIF -> begin
                      DEBUG_MSG "@";
                      self#prepend_token (mk T.MARKER)
                  end
                  | _ -> ()
              end
              | _ -> ()
          end
          | _ -> ()
        end

        | USING when env#macro_arg_flag && prev_rawtoken == COMMA -> begin
            DEBUG_MSG "@";
            self#prepend_token token;
            self#prepend_token (mk T.BEGIN_STMTS);
            raise To_be_recovered
        end
        | SEMICOLON _ when begin
            env#stmts_flag &&
            env#macro_arg_flag && env#paren_level = 1 && self#peek_rawtoken() == RPAREN
        end -> begin
          DEBUG_MSG "@";
          self#prepend_token (mk T.END_STMTS)
        end

        | END_ASM when env#asm_flag && prev_rawtoken == LPAREN && begin
            match prev_rawtoken2 with
            | GNU_ASM _ -> true
            | _ -> false
        end -> begin
          DEBUG_MSG "@";
          raise To_be_recovered
        end

        | _ when begin
            check_top_stmts_flag && not env#top_stmts_flag &&
            not env#pp_line_flag && not env#pp_define_body_flag &&
            match context with
            | TOP | MEM -> true
            | EXPR when begin
                try
                  let info = env#pp_if_section_top_info in
                  DEBUG_MSG "info=%s" (Pinfo.pp_if_section_info_to_string info);
                  let c = info.Pinfo.i_context in
                  c == C.EXPR
                with
                  _ -> false
            end -> false
            | EXPR -> env#stack#at_top
            | _ -> false
        end -> begin
          DEBUG_MSG "@";
          match rawtok with
          | IDENT_V _ when begin
              not env#braced_init_flag &&
              not env#decl_stmt_block_flag &&
              sub_context != END_OF_TY_SPEC &&
              match prev_rawtoken with
              | SEMICOLON _ | NEWLINE | EOF -> true
              | RBRACE when begin
                  not env#end_of_class_spec_flag && not env#end_of_enum_spec_flag
              end -> true
              | _ -> false
          end -> begin
            DEBUG_MSG "@";
            match self#peek_rawtoken() with
            | EQ when self#peek_nth_rawtoken 2 == LBRACE -> begin
                DEBUG_MSG "@";
                self#prepend_token token;
                self#prepend_token (mk T.DUMMY_TYPE);
                raise To_be_recovered
            end

            | MINUS_GT | DOT | GT_GT | LT_LT | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ
            | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _ | LBRACKET -> begin
                DEBUG_MSG "@";
                begin_stmts mk stp edp
            end

            | TY_LPAREN -> begin
                let nth, ll = self#peek_rawtoken_up_to_rparen_split_at_comma ~from:2 () in
                match self#peek_nth_rawtoken (nth+1) with
                | COMMA -> begin
                    let b =
                      List.exists
                        (function
                          | [x] when is_literal x -> true
                          | _ -> false
                        ) ll
                    in
                    if b then begin
                      DEBUG_MSG "@";
                      begin_stmts mk stp edp
                    end
                end
                | _ -> ()
            end

            | _ -> ()
          end

          | IDENT _ when begin
              match prev_rawtoken with
              | SEMICOLON _ | NEWLINE | EOF -> true
              | _ -> false
          end -> begin
            DEBUG_MSG "@";
            match self#peek_rawtoken() with
            | COLON_COLON -> begin
                try
                  let nth = self#peek_rawtoken_up_to_end_of_qualified_id() in
                  match self#peek_nth_rawtoken (nth+1) with
                  | MINUS_GT | DOT | GT_GT | LT_LT | EQ | PLUS_EQ | MINUS_EQ | STAR_EQ | SLASH_EQ
                  | PERC_EQ | HAT_EQ _ | AMP_EQ _ | BAR_EQ _-> begin
                      DEBUG_MSG "@";
                      begin_stmts mk stp edp
                  end
                  | TY_LPAREN -> begin
                      let nth', ll =
                        self#peek_rawtoken_up_to_rparen_split_at_comma ~from:(nth+2) ()
                      in
                      let b =
                        List.exists
                          (function
                            | [x] when is_literal x -> true
                            | _ -> false
                          ) ll
                      in
                      if b then begin
                        DEBUG_MSG "@";
                        begin_stmts mk stp edp
                      end
                  end
                  | _ -> ()
                with
                  Failure _ -> ()
            end
            | _ -> ()
          end

          | DELETE | IF | FOR | ODD_FOR | WHILE | DO | SWITCH when begin
              not env#stack#in_block && not env#decl_stmt_block_flag &&
              match prev_rawtoken with
              | SEMICOLON _ | NEWLINE | RPAREN | EOF | RBRACE -> true
              | _ -> false
          end -> begin
            DEBUG_MSG "@";
            begin_stmts mk stp edp
          end

          | IDENT s when prev_rawtoken == RPAREN && self#peek_rawtoken() == COMMA -> begin
              let nth = ref 2 in
              let trailing_comma_flag = ref false in
              let b =
                try
                  while true do
                    begin
                      match self#peek_nth_rawtoken !nth with
                      | IDENT _ -> begin
                          match self#peek_nth_rawtoken (!nth+1) with
                          | COMMA -> begin
                              match self#peek_nth_rawtoken (!nth+2) with
                              | IDENT _ -> begin
                                  match self#peek_nth_rawtoken (!nth+3) with
                                  | TY_LPAREN -> begin
                                      match self#peek_nth_rawtoken (!nth+4) with
                                      | RPAREN -> begin
                                          trailing_comma_flag := true;
                                          raise Found
                                      end
                                      | _ -> raise Exit
                                  end
                                  | _ -> ()
                              end
                              | _ -> raise Exit
                          end
                          | IDENT _ -> begin
                              match self#peek_nth_rawtoken (!nth+2) with
                              | TY_LPAREN -> begin
                                  match self#peek_nth_rawtoken (!nth+3) with
                                  | RPAREN -> raise Found
                                  | _ -> raise Exit
                              end
                              | _ -> raise Exit
                          end
                          | _ -> raise Exit
                      end
                      | _ -> raise Exit
                    end;
                    nth := !nth + 2
                  done;
                  assert false
                with
                | Exit -> false
                | Found -> true
              in
              if b then begin
                parse_warning env stp edp "enumerators in a wrong context";
                let l = ref [] in
                for i = 1 to !nth do
                  let _rt, sp, ep = self#discard_token() in
                  let rt =
                    match _rt with
                    | IDENT s -> T.IDENT_E s
                    | _ -> _rt
                  in
                  DEBUG_MSG "%d: %s%s" i (Token.rawtoken_to_string _rt)
                    (if rt != _rt then
                      Printf.sprintf " -> %s" (Token.rawtoken_to_string rt)
                    else
                      "");
                  l := (rt, sp, ep)::!l
                done;
                if !trailing_comma_flag then begin
                  let rt,_,_ = self#discard_token() in
                  assert (rt == COMMA)
                end;

                let l2 = ref [] in
                let _rt, sp, ep = self#discard_token() in
                let rt =
                  match _rt with
                  | IDENT s -> T.IDENT_V s
                  | _ -> assert false
                in
                l2 := (rt, sp, ep)::!l2;
                let _rt, sp, ep = self#discard_token() in
                let rt =
                  match _rt with
                  | TY_LPAREN -> T.LPAREN
                  | _ -> assert false
                in
                l2 := (rt, sp, ep)::!l2;
                l2 := (self#discard_token())::!l2;
                self#prepend_token (mk (T.SEMICOLON false));
                List.iter self#prepend_token !l2;

                self#prepend_token (mk T.END_ETORS);
                List.iter self#prepend_token !l;
                self#prepend_token (mk (T.IDENT_E s));
                self#prepend_token (mk T.BEGIN_ETORS);
                raise To_be_recovered
              end
          end

          | _ -> ()
        end

        | EXTERN when
            check_top_stmts_flag && not env#pp_line_flag && not env#pp_define_body_flag &&
            prev_rawtoken != END_STMTS &&
            env#top_stmts_flag &&
            begin
              match self#peek_rawtoken() with
              | STR_LITERAL _ -> true
              | _ -> false
            end -> begin
              parse_warning env stp edp "a declaration is about to begin in a wrong context";
              self#prepend_token token;
              self#prepend_token (mk T.END_STMTS);
              raise To_be_recovered
            end

        | STMT_MACRO s when self#is_opening_stmt_macro s -> begin
            self#prepend_token (mk T.LBRACE);
            self#prepend_token token;
            raise To_be_recovered
        end

        | MINUS_GT when not env#pp_define_body_flag -> begin
            match self#peek_rawtoken() with
            | RPAREN -> begin
                DEBUG_MSG "@";
                self#prepend_token (mk T.MARKER)
            end
            | _ -> ()
        end

        | END_ASM when env#asm_flag && env#braced_asm_flag -> begin
          DEBUG_MSG "@";
          raise To_be_recovered
        end

        | END_ASM when begin
            not env#asm_flag ||
            env#paren_level - env#asm_paren_level > 0 ||
            self#peek_rawtoken() == LBRACE
        end -> begin
          DEBUG_MSG "@";
          raise To_be_recovered
        end

        | NEWLINE when begin
            env#pp_paren_level = 1 &&
            (match prev_rawtoken with
            | STR_LITERAL _ -> true
            | _ -> false) &&
            match self#peek_rawtoken() with
            | STR_LITERAL _ -> begin
                let nth, l = self#peek_rawtoken_up_to_rparen_none() in
                let _, _, e = self#peek_nth nth in
                insert_after_nth_token self nth [T.NEWLINE, e, e];
                true
            end
            | _ -> false
        end -> begin
          DEBUG_MSG "@";
          raise To_be_recovered
        end

        | _ -> ()
      end;

      begin
        let rawtok, stp, edp = token in
        match rawtok with
        | PP_DEFINE -> begin
            self#push_context();
            self#push_sub_context()
        end
        | _ -> ()
      end;

      begin
        match sub_context with
        | START_OF_STMT _ -> begin
            let rawtok, _, _ = token in
            match rawtok with
            | RBRACE when env#body_head_flag -> begin
                self#ctx_top();
                self#ctx_ini()
            end
            | _  -> begin
                if env#in_body_brace_level >= 0 then
                  self#ctx_stmt()
                else begin
                  self#ctx_top();
                  env#clear_in_body_brace_flag()
                end;
                self#ctx_ini();
            end
        end
        | _ -> ()
      end;

      env#clear_body_head_flag();
      env#clear_for_range_init_flag();
      env#clear_param_head_flag();

      begin
        let rawtok, _, _ = token in
        match rawtok with
        | CONST when begin
            match prev_rawtoken with
            | TEMPL_LT | TY_LPAREN | COMMA -> true
            | LPAREN when prev_rawtoken2 == IF -> true
            | _ -> false
        end -> env#enter_const()
        | USING -> begin
            if self#peek_rawtoken() != NAMESPACE then
              env#set_using_flag()
        end
        | EQ when env#using_flag -> env#set_alias_flag()
        (*| EQ when env#typename_flag -> env#exit_typename()*)
        | BAR _ when context != EXPR -> self#ctx_in_expr()
        | TEMPL_LT when not env#pp_line_flag -> begin
            env#enter_templ_param_arg();
        end
        | TY_TEMPL_GT | TEMPL_GT when not env#pp_line_flag -> begin
            env#exit_const();
            begin
              try
                env#exit_templ_param_arg()
              with
                _ -> ()
            end;
            env#clear_end_of_params_flag();
            env#clear_ty_param_key_flag();
        end
        | LT_LT -> self#ctx_ini()

        | CUDA_LT_LT_LT -> env#enter_exec_config()

        | TYPEDEF -> env#set_typedef_flag()
        | COMMA -> begin
            env#exit_const();
            env#clear_ty_param_key_flag();
        end
        | SEMICOLON _ -> begin
            env#clear_typedef_flag();
            env#clear_using_flag();
            env#clear_alias_flag();
            env#clear_virtual_func_flag();
            env#clear_end_of_params_flag();
            env#clear_class_name_flag();
            env#clear_ns_alias_flag();
            (*env#clear_old_param_decl_flag();*)
            env#exit_enum_head();
            self#ctx_ini();
        end
        | LBRACE -> begin
            env#clear_end_of_params_flag();
            env#clear_class_name_flag();
            if env#pstat#brace_level_marker_flag then
              env#pstat#incr_brace_level_marker()
        end
        | RBRACE when env#pstat#brace_level_marker_flag -> begin
            env#pstat#decr_brace_level_marker();
            if env#pstat#brace_level_marker = 0 then begin
              env#pstat#clear_brace_level_marker_flag();
              if env#get_cond_sub_info() != Pinfo.PP_CLOSING then
                try
                  env#pstat#set_odd_canceled_info()
                with
                  _ -> ()
            end
        end

        | RPAREN when env#pp_define_flag -> begin
            env#exit_pp_define();
            env#enter_pp_define_body();
            env#clear_ty_param_key_flag();
        end

        | RPAREN -> env#clear_class_name_flag()

        | DYNAMIC_CAST | STATIC_CAST | REINTERPRET_CAST | CONST_CAST -> env#set_cast_key_flag()

        | STATIC_ASSERT when context != MEM && not env#pp_line_flag -> begin
            self#ctx_stmt();
            self#ctx_ini();
        end

        | PP_IF
        | PP_IF_A | PP_IF_ATTR | PP_IF_B | PP_IF_C | PP_IF_CB | PP_IF_CLOSING
        | PP_IF_COND | PP_IF_COND_
        | PP_IF_D | PP_IF_E | PP_IF_EH | PP_IF_H | PP_IF_I | PP_IF_O | PP_IF_P | PP_IF_S
        | PP_IF_SHIFT -> begin
            self#enter_pp_line();
            env#enter_pp_if();
            env#enter__pp_if_section()
        end
        | PP_ODD_IF -> begin
            self#enter_pp_line();
            env#enter_pp_if();
        end
        | PP_ODD_ELIF -> begin
            self#enter_pp_line();
            env#enter_pp_if();
        end
        | PP_ELIF -> begin
            env#add_pp_elif();
            self#enter_pp_line();
            env#enter_pp_if();
        end

        | PP_DEFINE -> begin
            self#enter_pp_line();
            match self#peek_nth_rawtoken 2 with
            | TY_LPAREN -> env#enter_pp_define()
            | _ -> ()
        end

        | PP_IFDEF | PP_IFNDEF
        | PP_IFDEF_A | PP_IFNDEF_A | PP_IFDEF_ATTR | PP_IFNDEF_ATTR | PP_IFDEF_B | PP_IFNDEF_B
        | PP_IFDEF_C | PP_IFNDEF_C | PP_IFDEF_CB | PP_IFNDEF_CB | PP_IFDEF_CLOSING | PP_IFNDEF_CLOSING
        | PP_IFDEF_COND | PP_IFNDEF_COND | PP_IFDEF_COND_ | PP_IFNDEF_COND_
        | PP_IFDEF_D | PP_IFNDEF_D | PP_IFDEF_E | PP_IFNDEF_E | PP_IFDEF_EH | PP_IFNDEF_EH
        | PP_IFDEF_H | PP_IFNDEF_H | PP_IFDEF_I | PP_IFNDEF_I | PP_IFDEF_O | PP_IFNDEF_O
        | PP_IFDEF_P | PP_IFNDEF_P | PP_IFDEF_S | PP_IFNDEF_S
        | PP_IFDEF_SHIFT | PP_IFNDEF_SHIFT -> begin
            self#enter_pp_line();
            env#enter_pp_ifdef();
            env#enter__pp_if_section()
        end
        | PP_ODD_IFDEF | PP_ODD_IFNDEF -> begin
            self#enter_pp_line();
            env#enter_pp_ifdef()
        end

        | PP_INCLUDE | PP_IMPORT | PP_UNDEF | PP_LINE | PP_ERROR | PP_PRAGMA | PP_ | PP_UNKNOWN _
        | PP_ODD_ELSE | PP_ODD_ENDIF -> self#enter_pp_line()

        | PP_ENDIF -> begin
            env#clear_dtor_if_section_flag();
            self#enter_pp_line();
            env#exit__pp_if_section()
        end

        | PP_ELSE -> begin
            env#add_pp_else();
            self#enter_pp_line();
        end
        | NEWLINE -> begin
            self#exit_pp_line();
            env#exit_pp_if();
            env#exit_pp_ifdef();
            env#exit_pp_define();
            env#exit_pp_define_body();
            env#clear_broken_flag();
            env#clear_typedef_flag();
        end
        | END_STMTS -> begin
            self#ctx_top();
            self#ctx_ini();
        end
        | BRACE_LEVEL _ -> env#pstat#set_brace_level_marker_flag()
        | IDENT_BM _ -> env#clear_end_of_params_flag()
        | _ -> ()
      end;

      env#clear_expr_flag();
      env#clear_dtor_flag();
      env#clear_end_of_if_head_flag();
      env#clear_end_of_id_macro_call_flag();
      env#clear_end_of_literal_macro_call_flag();
      env#clear_end_of_decltype_flag();
      env#clear_end_of_noptr_dtor_paren_flag();
      env#clear_start_of_func_body_flag();
      env#clear_end_of_old_param_decl_flag();
      env#clear_end_of_class_spec_flag();
      env#clear_end_of_enum_spec_flag();
      env#clear_end_of_cast_type_flag();
      env#clear_end_of_templ_head_flag();
      env#clear_end_of_sizeof_flag();
      env#clear_end_of_handler_head_flag();
      env#clear_end_of_attr_macro_call_flag();

      begin
        let rawtok, _, _ = token in
        match rawtok with
        | COLON_COLON -> ()
        | _ -> env#clear_value_flag()
      end;

      DEBUG_MSG "                              ------%s%s------ %s\n"
        (if replay_flag then "R" else "-") (if keep_flag then "K" else "-")
        (Token.to_string env#current_pos_mgr token);

      if keep_flag then begin
        let rawtok, st, ed = token in
        let mk x = x, st, ed in
        let token =
          match rawtok with
          | INI_LBRACE -> mk T.LBRACE
          | IDENT_TM x -> mk (T.IDENT x)
          (*| IDENT_V x -> mk (T.IDENT x)*)
          | _ -> token
        in
        self#keep token
      end;

      current_token <- token;

      let rt, sp, ep = token in
      prev_rawtoken4 <- prev_rawtoken3;
      prev_rawtoken3 <- prev_rawtoken2;
      prev_rawtoken2 <- prev_rawtoken;
      prev_rawtoken <- rt;
      prev_endofs <- ep.Lexing.pos_cnum;
      prev_endln <- ep.Lexing.pos_lnum;
      prev_stp <- sp;
      prev_edp <- ep;

      token

    initializer
      env#set_enter_source_callback self#enter_source


    end (* Scanner.F.c *)

end (* Scanner.F *)
