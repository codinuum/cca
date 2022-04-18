#!/usr/bin/env python3

import os.path

from .materialize_fact import main, Materializer, VIRTUOSO_PORT, VIRTUOSO_PW
from .siteconf import CCA_HOME

QUERY_DIR = os.path.join(CCA_HOME, 'queries', 'materialize')

JAVA_ITER_QUERIES = [
    'resolved_name.rq',
    'resolved_facc.rq',

    'reftype_of_this.rq',
    'reftype_of_ivk.rq',
    'reftype_of_new.rq',
    'reftype_of_expr0.rq',
    'reftype_of_expr1.rq',
    'reftype_of_expr2.rq',
    'reftype_of_field_access.rq',

    'type_of_this.rq',
    'type_of_uop.rq',
    'type_of_bop.rq',
    'type_of_array_access.rq',
    'type_of_ivk.rq',
    'type_of_new.rq',
    'type_of_field_access.rq',

    # 'simple_ivk0.rq',
    'simple_ivkx.rq',

    # 'primary_ivk.rq',
    'primary_ivk0.rq',
    'primary_ivkx.rq',

    'type_ivk0.rq',
    'type_ivkx.rq',

    'new_ivkx0.rq',
    'new_ivkx1.rq',
    'new_ivkx2.rq',

    'super_ivkx.rq',
    'this_ivkx.rq',
]

QUERIES = {
    'java': [
        'tdecl_in_srctree.rq',
        'file_mapping.rq',
        'stmt_level0.rq',
        'stmt_level.rq',
        'pruned_tdecl.rq',
        'pruned_super_type.rq',
        'pruned_method.rq',
        'pruned_field.rq',
        'pruned_field_access.rq',
        'pruned_name.rq',
        'pruned_enum_const.rq',
        'pruned_invocation.rq',
        'pruned_import.rq',
        'pruned_param.rq',
        'grafted_tdecl.rq',
        'grafted_super_type.rq',
        'grafted_method.rq',
        'grafted_field.rq',
        'grafted_field_access.rq',
        'grafted_name.rq',
        'grafted_enum_const.rq',
        'grafted_invocation.rq',
        'grafted_import.rq',
        'grafted_param.rq',
        'resolved_reftype.rq',
        'resolved_tyvar.rq',
        'resolved_type_ivk_pe.rq',
        'resolved_type_ivk_ps.rq',
        'resolved_type_ivk.rq',
        'resolved_type_ivk_static.rq',
        'class_hierarchy.rq',
        'interface_hierarchy.rq',
        'class_name_hierarchy.rq',
        'resolved_enum_const.rq',
        'resolved_facc0.rq',
        'refers_to_decl.rq',
        'tdecl_mapped_eq.rq',
        'tdecl_modified.rq',
        'stable_mapping.rq',
        'return_reftype.rq',
        'return_type.rq',
        'reftype_of_new0.rq',
        'declared_by_field0-0-0.rq',
        'declared_by_field0-0-1.rq',
        'declared_by_field0-0-2.rq',
        'declared_by_field0-1.rq',
        'declared_by_catch_param.rq',
        'declared_by_for_header.rq',
        'reftype_of_enum_const.rq',
        'reftype_of_cast.rq',
        'reftype_of_declared_var.rq',
        'reftype_of_var_declared_by_param.rq',
        'reftype_of_local_field_access.rq',
        'type_of_enum_const.rq',
        'type_of_cast.rq',
        'type_of_literal.rq',
        'type_of_declared_var.rq',
        'type_of_var_declared_by_param.rq',
        'type_of_local_field_access.rq',
        'param_ty.rq',
        'param_ty_name.rq',
        'simple_ivk0.rq',
        'super_ivk0.rq',
        'this_ivk0.rq',
        # 'type_ivk.rq',
        'new_ivk0.rq',
    ] + JAVA_ITER_QUERIES + JAVA_ITER_QUERIES + JAVA_ITER_QUERIES + [
        # 'declared_by_field1.rq',
        'declared_by_field2.rq',
    ],
    # 'c' :
    # [ 'pruned_functions.rq',
    #   'pruned_declarations.rq',
    #   'grafted_functions.rq',
    #   'grafted_declarations.rq',
    #   'functions_in_srctree.rq',
    #   'declarations_in_srctree.rq',
    # ],
}


def materialize(proj_id, pw=VIRTUOSO_PW, port=VIRTUOSO_PORT, conf=None):
    m = Materializer(QUERY_DIR, QUERIES, proj_id, pw=pw, port=port, conf=conf)
    rc = m.materialize()
    return rc


def main_():
    main(QUERY_DIR, QUERIES, 'materialize facts for refactoring')


if __name__ == '__main__':
    main_()
