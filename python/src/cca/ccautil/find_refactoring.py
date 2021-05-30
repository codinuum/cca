#!/usr/bin/env python3


'''
  find_refactoring.py

  Copyright 2018-2021 Chiba Institute of Technology

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
'''

__author__ = 'Masatomo Hashimoto <m.hashimoto@stair.center>'

import os.path
#import logging

from .siteconf import CCA_HOME
from . import find_change_patterns, sparql
from .find_change_patterns import Predicates
from .ns import REF_NS, JREF_NS, CREF_NS

from cca.factutil.rdf import Resource, Predicate

#logger = logging.getLogger()

QUERY_DIR = os.path.join(CCA_HOME, 'queries', 'refactoring')


class FactExtractor(find_change_patterns.FactExtractor):
        
    def get_other_info(self, ver0, ver1):
        return self.get_git_commit_info(ver0, ver1)

JAVA_PREDICATES = Predicates()
JAVA_PREDICATES.chgpat_ns  = JREF_NS
JAVA_PREDICATES.p_filepair = Predicate(REF_NS, 'filePair')
JAVA_PREDICATES.p_chgpat   = Predicate(REF_NS, 'refactoring')

C_PREDICATES = Predicates()
C_PREDICATES.chgpat_ns  = CREF_NS
C_PREDICATES.p_filepair = Predicate(REF_NS, 'filePair')
C_PREDICATES.p_chgpat   = Predicate(REF_NS, 'refactoring')


PREDICATE_TBL = {'java':JAVA_PREDICATES,'c':C_PREDICATES}



def get_queries(weak=False):
    # push_down_method = 'push_down_method.rq'
    # rename_method = 'rename_method.rq'
    # move_method = 'move_method.rq'

    # if weak:
    #     push_down_method = 'weak_'+push_down_method
    #     rename_method    = 'weak_'+rename_method
    #     move_method      = 'weak_'+move_method

    queries = { 'java' : # (FILE, ENT_VAR0, ENT_VAR1, EXTRA_ENT_VAR_PAIRS, EXTRA_VAR_LIST, ESSENTIAL_VARS(RM,AD,MP), INST_KEY, INST_KEY_IS_ONE_TO_ONE, PER_VER, MIN_EXTRA_PAIRS)
                [ 
                    ('local_variable_rename.rq', 'originalDtor', 'modifiedDtor',
                     [('originalVariable','modifiedVariable')], ['originalVariableName','modifiedVariableName'],
                     ([],[],[('dtor','dtor_'),('v','v_')]), None, None, False, 1),

                    ('add_parameter.rq', 'originalMethod', 'addedParameter', [],
                     #[('originalInvocation','modifiedInvocation')],
                     ['parameterName','methodName','modifiedMethod','className','class','class_'],
                     ([], ['param_'], []), None, None, False, 0),

                    ('add_parameter_and_add_method.rq', 'originalMethod', 'addedParameter', [],
                     ['parameterName','methodName','modifiedMethod','className','class','class_','originalContext','addedMethod'],
                     ([], ['param_'], []), None, None, False, 0),

                    ('remove_parameter.rq', 'removedParameter', 'modifiedMethod', [],
                     #[('originalInvocation','modifiedInvocation')],
                     ['parameterName','methodName','originalMethod','className','class','class_'],
                     (['param'], [], []), None, None, False, 0),

                    ('remove_parameter_and_remove_method.rq', 'removedParameter', 'modifiedMethod', [],
                     ['parameterName','methodName','originalMethod','className','class','class_'],
                     (['param'], [], []), None, None, False, 0),

                    ('rename_parameter.rq', 'originalParameter', 'modifiedParameter',
                     [('originalVariable','modifiedVariable')],
                     ['originalParameterName','modifiedParameterName','methodName','className','originalMethod','modifiedMethod','originalClass'],
                     ([],[],[('param','param_'),('v','v_')]), None, None, False, 1),

                    ('change_bidirectional_association_to_unidirectional.rq', 'removedField', 'modifiedContext',
                     [('field0','field0_')], ['otherClassName','className','originalClass','modifiedClass'],
                     (['field1'], [], []), None, None, False, 1),

                    ('change_unidirectional_association_to_bidirectional.rq', 'originalContext', 'addedField',
                     [('field0','field0_')], ['otherClassName','className','originalClass','modifiedClass'],
                     ([], ['field1_'], []), None, None, False, 1),

                    ('consolidate_conditional_expression.rq', 'originalIf', 'modifiedIf', [],
                     ['methodName','originalMethod','modifiedMethod'], ([], [], [('if0','if_')]), None, None, False, 0),

                    ('extract_class.rq', 'originalClass', 'extractedClass', [],
                     ['className', 'extractedClassName'], ([], ['class1_'], []), None, None, False, 0),

                    ('inline_class.rq', 'inlinedClass', 'modifiedClass', [],
                     ['inlinedClassName','className'], (['class1'], [], []), None, None, False, 0),

                    ('extract_interface.rq', 'originalContext', 'extractedInterface', [('originalClass','modifiedClass')],
                     ['interfaceName'], ([], ['interf_'], [('class','class_')]), None, None, False, 2),

                    ('quasi_extract_interface.rq', 'originalContext', 'extractedInterface', [('originalClass','modifiedClass')],
                     ['interfaceName'], ([], ['interf_'], [('class','class_')]), None, None, False, 1),

                    ('extract_method.rq', 'originalMethod', 'extractedMethod', [('originalContext', 'addedInvocation')],
                     ['originalMethodFQN','modifiedMethodFQN','extractedMethodFQN'],
                     ([], ['meth_'], [('ctx','ivk_')]), None, None, False, 1),

                    ('inline_method.rq', 'inlinedMethod', 'modifiedMethod', [('removedInvocation','modifiedContext')],
                     ['originalMethodFQN','modifiedMethodFQN','inlinedMethodFQN'],
                     (['meth'], [], [('ivk','ctx_')]), None, None, False, 1),

                    ('extract_superclass.rq', 'originalContext', 'extractedSuperclass', [('originalClass','modifiedClass')],
                     ['className','superclassName'], ([], ['SuperC_'], [('C0x','C0x_')]), None, None, False, 2),

                    ('quasi_extract_superclass.rq', 'originalContext', 'extractedSuperclass', [('originalClass','modifiedClass')],
                     ['className','superclassName'], ([], ['SuperC_'], [('C0x','C0x_')]), None, None, False, 1),

                    ('extract_superclass_and_move_field.rq', 'originalField', 'movedField',
                     [('originalField','context_'),('context','movedField')],
                     ['fieldTypeName','fieldName','fromClassName','toClassName','fromModifiers','toModifiers','fromClass','toClass'],
                     (['vdtor'], ['vdtor_'], []), ('vdtor','vdtor_'), (False, True), False, 2),

                    ('extract_superclass_and_move_method.rq', 'originalMethod', 'movedMethod',
                     [('originalMethod','context_'),('context','movedMethod')],
                     ['signature','methodName','fromClassName','toClassName','fromClass','toClass'],
                     (['meth'], ['meth_'], []), ('meth','meth_'), (False, True), False, 2),

                    ('form_template_method.rq', 'originalParentClass', 'modifiedParentClass', [],
                     ['templateMethodName','templateMethod','subMethod'],
                     ([], ['templM_','subM_'], []), None, None, False, 0),

                    ('hide_delegate.rq', 'originalInvocation', 'modifiedInvocation',
                     [('originalContext', 'addedServerMethod')], 
                     ['ClientClassName','ServerClassName','DelegateClassName','delegateMethodName','serverMethodName',
                      'addedServerMethod','originalServerClass','modifiedServerClass',
                      'originalClientClass','modifiedClientClass'], 
                     ([], ['serverM_'], []), None, None, False, 1), 

                    ('remove_middle_man.rq', 'originalInvocation', 'modifiedInvocation',
                     [('removedServerMethod', 'modifiedContext')],
                     ['ClientClassName','ServerClassName','DelegateClassName','serverMethodName','delegateMethodName',
                      'removedServerMethod','originalServerClass','modifiedServerClass',
                      'originalClientClass','modifiedClientClass'], 
                     (['serverM'], [], []), None, None, False, 1),

                    ('hide_method.rq', 'originalMethod', 'modifiedMethod', [],
                     ['methodName'], ([], [], [('meth','meth_')]), None, None, False, 0),

                    ('extract_variable.rq', 'originalExpr', 'movedExpr', 
                     [('originalContext','modifiedContext'),('originalExpr','extractedVariable')],
                     ['extractedVariableName','originalMethodName','modifiedMethodName','originalMethod','modifiedMethod'],
                     ([], ['decl_','v_'], [('f','f_'),('a','rhs_')]), None, None, False, 0),

                    ('inline_temp.rq', 'originalExpr', 'movedExpr', 
                     [('originalContext','modifiedContext'),('eliminatedVariable','movedExpr')],
                     ['eliminatedVariableName','originalMethodName','modifiedMethodName','originalMethod','modifiedMethod'],
                     (['decl','v'], [], [('f','f_'),('rhs','a_')]), None, None, False, 0),

                    ('introduce_assertion.rq', 'originalMethod', 'introducedAssertion', [],
                     ['methodName','modifiedMethod'], ([], ['assert_'], []), None, None, False, 0),

                    ('introduce_local_extension.rq', 'originalClientMethod', 'modifiedClientMethod', [],
                     ['introducedClassName','introducedClass'], ([], ['ExtC_'], []), None, None, False, 0),

                    ('introduce_null_object.rq', 'equation', 'modifiedContext', [],
                     ['className','introducedNullClass'], (['if'], ['null_class_'], []), None, None, False, 0),

                    ('introduce_parameter_object.rq', 'originalMethod', 'modifiedMethod', [],
                     ['parameterNames','parameterClassName'], ([], ['param_'], []), None, None, False, 0),

                    ('pull_up_constructor_body.rq', 'originalCtor', 'modifiedCtor', [],
                     ['className','originalClass','modifiedClass'], ([], [], [('ctor','ctor_')]), None, None, False, 0),

                    ('pull_up_field.rq', 'originalContext', 'movedField',
                     [('originalField','modifiedContext'),('originalField','movedField')],
                     ['fieldName','className','superclassName','originalClass','modifiedClass','superclass'],
                     (['vdtor0x'], ['vdtor0_'], []), ('vdtor0x','vdtor0_'), (False, True), False, 1),

                    ('quasi_pull_up_field.rq', 'originalContext', 'movedField',
                     [('originalField','modifiedContext'),('originalField','movedField')],
                     ['fieldName','className','superclassName','originalClass','modifiedClass','superclass'],
                     (['vdtor0x'], ['vdtor0_'], []), ('vdtor0x','vdtor0_'), (True, True), False, 1),

                    ('push_down_field.rq', 'originalField', 'modifiedContext',
                     [('originalContext','movedField'),('originalField','movedField')],
                     ['fieldName','className','subclassName','originalClass','modifiedClass','subclass'],
                     (['vdtor0'], ['vdtor0_'], []), ('vdtor0','vdtor0_'), (True, False), False, 1),

                    ('pull_up_method.rq', 'originalContext', 'movedMethod',
                     [('originalMethod','modifiedContext'),('originalMethod','movedMethod')],
                     ['methodName','signature','className','superclassName','originalClass','modifiedClass','superclass'],
                     (['methx'], ['meth_'], []), ('methx','meth_'), (False, True), False, 1),

                    ('quasi_pull_up_method.rq', 'originalContext', 'movedMethod',
                     [('originalMethod','modifiedContext'),('originalMethod','movedMethod')],
                     ['methodName','signature','className','superclassName','originalClass','modifiedClass','superclass'],
                     (['methx'], ['meth_'], []), ('methx','meth_'), (True, True), False, 1),

                    ('rename_and_pull_up_method.rq', 'originalContext', 'movedMethod',
                     [('originalMethod','modifiedContext'),('originalMethod','movedMethod')],
                     ['methodName','modifiedMethodName','signature','className','superclassName','originalClass','modifiedClass','superclass'],
                     (['meth'], ['meth_'], []), ('meth','meth_'), (False, True), False, 1),

                    ('change_signature_and_pull_up_method.rq', 'originalContext', 'movedMethod',
                     [('originalMethod','modifiedContext'),('originalMethod','movedMethod')],
                     ['methodName','signature','changedSignature','className','superclassName','originalClass','modifiedClass','superclass'],
                     (['meth'], ['meth_'], []), ('meth','meth_'), (False, True), False, 1),

                    ('push_down_method.rq', 'originalMethod', 'modifiedContext',
                     [('originalContext','movedMethod'),('originalMethod','movedMethod')],
                     ['methodName','signature','className','subclassName','originalClass','modifiedClass','subclass'],
                     (['meth0'], ['meth0_'], []), ('meth0','meth0_'), (True, False), False, 1),

                    ('quasi_push_down_method.rq', 'originalMethod', 'modifiedContext',
                     [('originalContext','movedMethod'),('originalMethod','movedMethod')],
                     ['methodName','signature','className','subclassName','originalClass','modifiedClass','subclass'],
                     (['meth0'], ['meth0_'], []), ('meth0','meth0_'), (True, True), False, 1),

                    ('remove_assignments_to_parameters.rq', 'removedAssignment', 'addedDeclarator', [], 
                     ['methodName','parameterName','originalMethod','modifiedMethod'],
                     (['assign'], ['dtor_'], []), None, None, False, 0),

                    ('remove_control_flag.rq', 'assign', 'introducedControl', [], 
                     ['methodName','flagName','originalMethod','modifiedMethod'],
                     (['dtor'], [], [('assign','break_or_continue_')]), None, None, False, 0),

                    ('method_visibility_increased.rq', 'originalMethod', 'modifiedMethod', [],
                     ['originalMethodName','modifiedMethodName'], ([], [], [('meth','meth_')]), None, None, False, 0),

                    ('method_visibility_decreased.rq', 'originalMethod', 'modifiedMethod', [],
                     ['originalMethodName','modifiedMethodName'], ([], [], [('meth','meth_')]), None, None, False, 0),

                    ('replace_constructor_with_factory_method.rq', 'removedCtor', 'factoryMethod', [],
                     ['className','methodName','originalClass','modifiedClass'], (['ctor'], [], []), None, None, False, 0),

                    ('replace_exception_with_test.rq', 'try', 'addedIf', [],
                     ['methodName','originalMethod','modifiedMethod'], (['try'], ['if_'], []), None, None, False, 0),

                    ('replace_magic_number_with_symbolic_constant.rq', 'const', 'ident_', [], 
                     ['constantName','methodName','originalMethod','modifiedMethod'],
                     ([], [], [('const','ident_')]), None, None, False, 0),

                    ('replace_nested_conditional_with_guard_clauses.rq', 'removedIf', 'addedIf', [], 
                     ['methodName','originalMethod','modifiedMethod'], (['if0'], ['if_'], []), None, None, False, 0),

                    ('replace_parameter_with_method.rq', 'originalInvocation', 'modifiedInvocation', [],
                     ['methodName','originalMethod','modifiedMethod'], (['arg'], [], []), None, None, False, 0),

                    ('replace_temp_with_query.rq', 'originalDeclarator', 'addedMethod', [], 
                     ['variableName','addedMethodName','methodName','originalMethod','modifiedMethod'],
                     (['decl'], ['qmeth_'], [('v','invoke_')]), None, None, False, 0),

                    ('separate_query_from_modifier.rq', 'originalMethod', 'addedMethod1', [], 
                     ['methodName','addedMethodName1','addedMethodName2','originalMethod','addedMethod1','addedMethod2'],
                     (['meth0'], ['meth1_','meth2_'], []), None, None, False, 0),

                    ('rename_package.rq', 'originalPackage', 'modifiedPackage', [],
                     ['originalPackageName','modifiedPackageName'], ([], [], [('pdecl','pdecl_')]), None, None, False, 0),

                    ('rename_class.rq', 'originalClass', 'modifiedClass', [],
                     ['originalClassName','modifiedClassName'], ([], [], [('class','class_')]), None, None, False, 0),

                    ('rename_field.rq', 'originalField', 'modifiedField', [],
                     ['originalFieldName','modifiedFieldName'], ([], [], [('vdtor','vdtor_')]), None, None, False, 0),

                    ('rename_method.rq', 'originalMethod', 'modifiedMethod', [],
                     ['originalMethodName','modifiedMethodName'], ([], [], [('meth','meth_')]), None, None, False, 0),

                    ('move_class.rq', 'originalClass', 'modifiedClass', [],
                     ['originalClassName','modifiedClassName'], ([], [], [('class','class_')]), None, None, False, 0),

                    ('move_field.rq', 'originalField', 'movedField',
                     [('originalField','modifiedContext'),('originalContext','movedField')],
                     ['fieldTypeName','fieldName','fromClassName','toClassName','fromModifiers','toModifiers','fromClass','toClass'],
                     (['vdtor'], ['vdtor_'], []), ('vdtor','vdtor_'), (False, False), False, 1),

                    ('local_move_field.rq', 'originalField', 'movedField',
                     [('originalField','modifiedContext'),('originalContext','movedField')],
                     ['fieldTypeName','fieldName','fromClassName','toClassName','fromModifiers','toModifiers','fromClass','toClass'],
                     ([], [], [('vdtor','vdtor_')]), ('vdtor','vdtor_'), (True, True), False, 1),

                    ('move_method.rq', 'originalMethod', 'movedMethod',
                     [('originalMethod','context_'),('context','movedMethod')],
                     ['signature','methodName','fromClassName','toClassName','fromClass','toClass'],
                     (['meth'], ['meth_'], []), ('meth','meth_'), (False, False), False, 1),

                    ('local_move_method.rq', 'originalMethod', 'movedMethod', [],
                     ['signature','methodName','fromClassName','toClassName','fromClass','toClass'],
                     ([], [], [('meth','meth_')]), ('meth','meth_'), (True, True), False, 1),
                ],
                'c' :
                    [ # ('add_macro_parameter.rq', 'e', 'param_', [], ['name_'], ([], [], []), None, None, False, 0),
                      # ('remove_macro_parameter.rq', 'param', 'e_', [], ['name'], ([], [], []), None, None, False, 0),
                      # ('add_parameter.rq', 'e', 'param_', [], ['name_'], ([], [], []), None, None, False, 0),
                      # ('remove_parameter.rq', 'param', 'e_', [], ['name'], ([], [], []), None, None, False, 0),
                      # ('change_bidirectional_association_to_unidirectional.rq', 'mem0', 'struct0_', [] , ['sname0_'], ([], [], []), None, None, False, 0),
                      # ('change_unidirectional_association_to_bidirectional.rq', 'struct0', 'mem0_', [] , ['sname0'], ([], [], []), None, None, False, 0),
                      # ('consolidate_conditional_expression.rq', 'if0', 'if0_', [] , ['fname1_'], ([], [], []), None, None, False, 0),
                      # ('decompose_conditional.rq', 'if', 'func_', [], ['fname_'], ([], [], []), None, None, False, 0),
                      # ('extract_function.rq', 'ent0', 'func_', [], ['fname_'], ([], [], []), None, None, False, 0),
                      # ('extract_structure.rq', 'spec0', 'spec1_', [], ['name0', 'name1_'], ([], [], []), None, None, False, 0),
                      # ('inline_function.rq', 'func', 'ent0', [], ['fname'], ([], [], []), None, None, False, 0),
                      # ('inline_structure.rq', 'spec1', 'spec0_', [], ['name1', 'name0_'], ([], [], []), None, None, False, 0),

                        # ('extract_variable.rq', 'originalExpr', 'movedExpr', 
                        #  [('originalContext', 'modifiedContext'),('originalExpr','extractedVariable')], 
                        #  ['extractedVariableName','functionName'], ([], ['decl_','v_'], [('f','f_'),('a','rhs_')]), 
                        #  None, None, False, 1),

                        # ('inline_temp.rq', 'originalExpr', 'movedExpr', 
                        #  [('originalContext','modifiedContext'),('eliminatedVariable','movedExpr')], 
                        #  ['eliminatedVariableName', 'functionName'], (['decl', 'v'], [], [('f', 'f_'),('rhs','a_')]), 
                        #  None, None, False, 1),

                      # ('introduce_parameter_structure.rq', 'func', 'func_', [], ['name_ty0', 'name_ty1', 'name_struct_'], ([], [], []), None, None, False, 0),
                      # ('move_member.rq', 'mem0', 'mem0_', [], ['name0', 'name0_'], ([], [], []), None, None, False, 0),
                      # ('parameterize_function.rq', 'func0', 'func_', [], [], ([], [], []), None, None, False, 0),
                      # ('preserve_whole_structure.rq', 'call', 'call_', [], [], ([], [], []), None, None, False, 0),
                      # ('remove_assignments_to_parameters.rq', 'lhs0', 'lhs1_', [], [], ([], [], []), None, None, False, 0),
                      # ('rename_function.rq', 'func', 'func_', [], ['name', 'name_'], ([], [], []), None, None, False, 0),
                      # ('rename_macro.rq', 'macro', 'macro_', [], ['name', 'name_'], ([], [], []), None, None, False, 0),
                      # ('replace_data_value_with_structure.rq', 'decl', 'decl_', [], ['sname'], ([], [], []), None, None, False, 0),
                      # ('replace_magic_number_with_symbolic_constant.rq', 'const', 'ident_', [], ['name_'], ([], [], []), None, None, False, 0),
                      # ('replace_nested_conditional_with_guard_clauses.rq', 'if0', 'if0_', [], [], ([], [], []), None, None, False, 0),
                      # ('replace_parameter_with_explicit_functions.rq', 'func', 'func0_', [], [], ([], [], []), None, None, False, 0),
                      # ('replace_temp_with_query.rq', 'decl', 'func_', [], [], ([], [], []), None, None, False, 0),
                      # ('split_temporary_variable.rq', 'decl0', 'decl1_', [], [], ([], [], []), None, None, False, 0),
                      ]
                }
    return queries

QUERIES = get_queries(weak=False)

def find(base_dir, proj_id, foutdir, outdir, pw, port,
         limit=None, lang=None, method='odbc', change_enumeration=False, per_ver=False,
         query_prec=False, conf=None):

    find_change_patterns.find(QUERY_DIR, QUERIES, PREDICATE_TBL, FactExtractor,
                              base_dir, proj_id, foutdir, outdir, pw, port,
                              limit, lang, method, change_enumeration, per_ver, query_prec, conf=conf)

def main():
    find_change_patterns.main(QUERY_DIR, 
                              QUERIES, 
                              'find refactorings',
                              predicate_tbl=PREDICATE_TBL,
                              extra_fact_extractor=FactExtractor)

if __name__ == '__main__':
    main()
