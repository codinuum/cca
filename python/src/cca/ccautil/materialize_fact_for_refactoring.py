#!/usr/bin/env python3

import os.path

from .materialize_fact import main, Materializer, VIRTUOSO_PORT, VIRTUOSO_PW
from .siteconf import CCA_HOME

QUERY_DIR = os.path.join(CCA_HOME, 'queries', 'refactoring')

JAVA_ITER_QUERIES = [
    'materialize_resolved_name.rq',
    'materialize_resolved_facc.rq',

    'materialize_reftype_of_this.rq',
    'materialize_reftype_of_ivk.rq',
    'materialize_reftype_of_new.rq',
    'materialize_reftype_of_expr0.rq',
    'materialize_reftype_of_expr1.rq',
    'materialize_reftype_of_expr2.rq',
    'materialize_reftype_of_field_access.rq',

    'materialize_type_of_this.rq',
    'materialize_type_of_uop.rq',
    'materialize_type_of_bop.rq',
    'materialize_type_of_array_access.rq',
    'materialize_type_of_ivk.rq',
    'materialize_type_of_new.rq',
    'materialize_type_of_field_access.rq',

    # 'materialize_simple_ivk0.rq',
    'materialize_simple_ivkx.rq',

    # 'materialize_primary_ivk.rq',
    'materialize_primary_ivk0.rq',
    'materialize_primary_ivkx.rq',

    'materialize_type_ivk0.rq',
    'materialize_type_ivkx.rq',

    'materialize_new_ivkx0.rq',
    'materialize_new_ivkx1.rq',
    'materialize_new_ivkx2.rq',

    'materialize_super_ivkx.rq',
    'materialize_this_ivkx.rq',
]

QUERIES = {
    'java': [
        'materialize_tdecl_in_srctree.rq',
        'materialize_file_mapping.rq',
        'materialize_stmt_level0.rq',
        'materialize_stmt_level.rq',
        'materialize_pruned_tdecl.rq',
        'materialize_pruned_super_type.rq',
        'materialize_pruned_method.rq',
        'materialize_pruned_field.rq',
        'materialize_pruned_field_access.rq',
        'materialize_pruned_name.rq',
        'materialize_pruned_enum_const.rq',
        'materialize_pruned_invocation.rq',
        'materialize_pruned_import.rq',
        'materialize_pruned_param.rq',
        'materialize_grafted_tdecl.rq',
        'materialize_grafted_super_type.rq',
        'materialize_grafted_method.rq',
        'materialize_grafted_field.rq',
        'materialize_grafted_field_access.rq',
        'materialize_grafted_name.rq',
        'materialize_grafted_enum_const.rq',
        'materialize_grafted_invocation.rq',
        'materialize_grafted_import.rq',
        'materialize_grafted_param.rq',
        'materialize_resolved_reftype.rq',
        'materialize_resolved_tyvar.rq',
        'materialize_resolved_type_ivk_pe.rq',
        'materialize_resolved_type_ivk_ps.rq',
        'materialize_resolved_type_ivk.rq',
        'materialize_resolved_type_ivk_static.rq',
        'materialize_class_hierarchy.rq',
        'materialize_interface_hierarchy.rq',
        'materialize_class_name_hierarchy.rq',
        'materialize_resolved_enum_const.rq',
        'materialize_resolved_facc0.rq',
        'materialize_refers_to_decl.rq',
        'materialize_tdecl_mapped_eq.rq',
        'materialize_tdecl_modified.rq',
        'materialize_stable_mapping.rq',
        'materialize_return_reftype.rq',
        'materialize_return_type.rq',
        'materialize_reftype_of_new0.rq',
        'materialize_declared_by_field0-0-0.rq',
        'materialize_declared_by_field0-0-1.rq',
        'materialize_declared_by_field0-0-2.rq',
        'materialize_declared_by_field0-1.rq',
        'materialize_declared_by_catch_param.rq',
        'materialize_reftype_of_enum_const.rq',
        'materialize_reftype_of_cast.rq',
        'materialize_reftype_of_declared_var.rq',
        'materialize_reftype_of_var_declared_by_param.rq',
        'materialize_reftype_of_local_field_access.rq',
        'materialize_type_of_enum_const.rq',
        'materialize_type_of_cast.rq',
        'materialize_type_of_literal.rq',
        'materialize_type_of_declared_var.rq',
        'materialize_type_of_var_declared_by_param.rq',
        'materialize_type_of_local_field_access.rq',
        'materialize_param_ty.rq',
        'materialize_simple_ivk0.rq',
        'materialize_super_ivk0.rq',
        'materialize_this_ivk0.rq',
        # 'materialize_type_ivk.rq',
        'materialize_new_ivk0.rq',
    ] + JAVA_ITER_QUERIES + JAVA_ITER_QUERIES + JAVA_ITER_QUERIES + [
        # 'materialize_declared_by_field1.rq',
        'materialize_declared_by_field2.rq',
    ],
    # 'c' :
    # [ 'materialize_pruned_functions.rq',
    #   'materialize_pruned_declarations.rq',
    #   'materialize_grafted_functions.rq',
    #   'materialize_grafted_declarations.rq',
    #   'materialize_functions_in_srctree.rq',
    #   'materialize_declarations_in_srctree.rq',
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
