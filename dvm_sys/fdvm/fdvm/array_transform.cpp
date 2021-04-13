#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <set>
#include <string>
#include <typeinfo>
#include <utility>
#define IN_ANAL_
#include "../Sage/lib/include/sage++user.h"
#undef IN_ANAL_
#include <cmath>
#include <limits>
#include <list>
#include <map>
#include <vector>
#define MY_DECL_ATTRIBUTE 1805
#define EXTRA_ATTRIBUTE 1806
#define MY_TARGET_ATTRIBUTE 1807
#define _NAN std::numeric_limits<double>::quiet_NaN();

using namespace std;

SgFile *f, *current_file;
int current_file_id;
unsigned fun_count = 0, stat_count = 0;
SgStatement *main_fun = nullptr, *bad_fun = nullptr;
SgStatement *wh(SgStatement *where);
SgStatement *to_ins = nullptr;
map<string, vector<string>> commons;
set<string> hashes;
set<string> sym_identifiers;
map<string, vector<pair<string, string>>> parsed_data;
set<SgStatement *> modules;
map<string, set<string>> not_transformable_ars_dyn_to_stat;
map<string, set<string>> not_transformable_ars_stat_to_dyn;
set<string> visited_spaces;
map<string, vector<SgStatement *>> raw_func_calls;
map<string, set<int>> not_transformable_fun_params_dyn_to_stat;
map<string, set<int>> not_transformable_fun_params_stat_to_dyn;
map<string, set<string>> ars_as_params;
map<string, vector<SgStatement *>> funs_interfaces;
map<string, set<string>> stat_to_dyn_ars;
map<string, set<string>> dyn_to_stat_ars;
map<string, vector<pair<SgStatement *, pair<string, SgExprListExp *>>>> funs_funs_calls;
map<string, set<string>> interfaces_decls;
map<string, map<string, vector<int>>> fun_array_sizes;
map<string, map<string, SgExpression *>> transformable_array_sizes;
map<string, map<string, SgExpression *>> transformable_array_sizes_to_interfaces;
map<string, map<string, map<string, string>>> mods_uses;
map<string, set<string>> mod_publics;
map<string, vector<SgStatement *>> procedure_calls;
set<SgStatement *> uses;
set<string> remain_files;
map<SgStatement *, SgStatement *>
    last_declarations; //потом работаем с этим вместо вычисления каждый раз myLastDeclaration
map<string, map<string, map<int, int>>> dynamic_array_shapes;
set<string> cycle_funs;
map<string, vector<string>> not_visible_files;
map<string, set<string>> susp_ars_from_inc;
map<string, pair<string, string>> modules_functions;
map<string, string> dont_touch_modules_functions;
map<string, vector<string>> funs_calls;
int dec_count = 0;
map<string, string> mod_dec;
map<string, string> com_mod_names;
int is_changed = 0;

SgStatement *myLastDeclaration(SgStatement *st) {
    SgStatement *res = st;
    if (st->variant() == MODULE_STMT) {
        return st->lastDeclaration();
    }
    if (st->variant() == INTERFACE_STMT) {
        return st;
    }
    if (st->lexNext() && st->lexNext()->variant() == USE_STMT || st->lexNext()->variant() == IMPL_DECL) {
        while (st->lexNext() && st->variant() == USE_STMT || st->variant() == IMPL_DECL) {
            st = st->lexNext();
        }
    }
    for (auto cur_st = st; cur_st && cur_st != st->lastNodeOfStmt(); cur_st = cur_st->lexNext()) {
        if (cur_st->variant() == INTERFACE_STMT) {
            cur_st = cur_st->lastNodeOfStmt()->lastNodeOfStmt();
            res = cur_st;
        }
        int cur_var = cur_st->variant();
        if (cur_var == VAR_DECL || cur_var == PARAM_DECL || cur_var == DATA_DECL ||
            (cur_st->expr(0) && cur_st->expr(0)->variant() == COMM_LIST) || cur_var == EXTERN_STAT ||
            cur_var == OMP_THREADPRIVATE_DIR || cur_var == SAVE_DECL) {
            res = cur_st;
        }
    }
    return res;
}

void parse_last_declarations(SgProject &project) {
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->variant() == MODULE_STMT || st->variant() == PROG_HEDR || st->variant() == FUNC_HEDR ||
                st->variant() == PROC_HEDR || st->variant() == BLOCK_DATA) {
                last_declarations[st] = myLastDeclaration(st);
            }
        }
    }
}

void set_file_name(SgStatement *st, const string &name) {
    string *new_string = new string(name);
    char *char_name = const_cast<char *>(new_string->c_str());
    int size = sizeof(char_name);
    st->addAttribute(MY_DECL_ATTRIBUTE, char_name, size);
}

void update_last_declaration(SgStatement *space, SgStatement *to_insert, int fl) {
    if (last_declarations[space]->variant() == INTERFACE_STMT) {
        last_declarations[space]->lastNodeOfStmt()->lastNodeOfStmt()->insertStmtAfter(*to_insert, *space);
    } else {
        last_declarations[space]->insertStmtAfter(*to_insert, *space);
    }
    if (fl) {
        if (last_declarations[space]->variant() == INTERFACE_STMT) {
            last_declarations[space] = last_declarations[space]->lastNodeOfStmt()->lastNodeOfStmt()->lexNext();
        } else {
            last_declarations[space] = last_declarations[space]->lexNext();
        }
    }
}

string create_hash(SgStatement *st) {
    if (st->variant() == PROG_HEDR) {
        return "program";
    }
    string res = "";
    while (st && st->variant() != INTERFACE_STMT) {
        if (st->symbol()) {
            if (st->variant() == PROG_HEDR) {
                res = res + " " + "program";
            } else {
                if (res == "") {
                    res = st->symbol()->identifier();
                } else {
                    res = res + " " + st->symbol()->identifier();
                }
            }
        }
        st = st->controlParent();
    }
    return res;
}

string get_real_file_name(SgStatement *st) {
    char *name = nullptr;
    if (st->numberOfAttributes(MY_DECL_ATTRIBUTE)) {
        name = static_cast<char *>(st->attributeValue(0, MY_DECL_ATTRIBUTE));
    } else {
        name = st->fileName();
    }
    return string(name);
}

bool if_parameter(SgStatement *st) {
    if (st->variant() == VAR_DECL && st->expr(2)) {
        SgExprListExp *lis = static_cast<SgExprListExp *>(st->expr(2));
        for (int j = 0; j < lis->length(); ++j) {
            if (lis->elem(j)->variant() == PARAMETER_OP) {
                st->setVariant(PARAM_DECL);
                return true;
            }
        }
    }
    return false;
}

SgStatement *createif(SgVarRefExp *sym) {
    return new SgStatement(LOGIF_NODE, nullptr, nullptr, &(*sym != *new SgValueExp(0)), nullptr, nullptr);
}
SgStatement *wh(SgStatement *where) {
    if (where->variant() == GLOBAL || where->variant() == MODULE_STMT || where->variant() == PROG_HEDR ||
        where->variant() == FUNC_HEDR || where->variant() == PROC_HEDR || where->variant() == BLOCK_DATA) {
        return where;
    }
    return where->controlParent() ? wh(where->controlParent()) : nullptr;
}

string trim(const string &s) {
    string::const_iterator it = s.begin();
    while (it != s.end() && isspace(*it)) {
        it++;
    }
    string::const_reverse_iterator rit = s.rbegin();
    while (rit.base() != it && isspace(*rit)) {
        rit++;
    }
    return string(it, rit.base());
}

void my_delete(SgStatement *st) {
    SgStatement *space = wh(st);
    if (last_declarations[space] == st) {
        last_declarations[space] = last_declarations[space]->lexPrev();
    }
    st->deleteStmt();
}

void init_data(string name, SgStatement *call_st, SgExpression *current_variable) {
    for (long unsigned int i = 0; i < parsed_data[name].size(); ++i) {
        long unsigned pos = parsed_data[name][i].first.find('(');
        if (pos == string::npos) {
            pos = parsed_data[name][i].first.length();
        }
        if (current_variable->symbol()->identifier() == parsed_data[name][i].first.substr(0, pos)) {
            int has_string_initialization = 0, array_index = 1; // исправить - срез может не с единицы начинаться
            for (long unsigned t = 0; t < parsed_data[name][i].second.length(); ++t) {
                if (parsed_data[name][i].second[t] == '\'') {
                    has_string_initialization = 1;
                    int st_t = t;
                    t++;
                    while (parsed_data[name][i].second[t] != '\'') {
                        t++;
                    }
                    SgStatement *tmp =
                        new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
                    string *buf_str = new string(parsed_data[name][i].first + "(" + to_string(array_index++) + ")" +
                                                 " = " + parsed_data[name][i].second.substr(st_t, t - st_t + 1));
                    NODE_STR(tmp->expr(0)->thellnd) = (char *)buf_str->c_str();
                    call_st->insertStmtAfter(*tmp, *call_st->controlParent()->controlParent());
                }
            }
            if (!has_string_initialization) {
                SgStatement *tmp =
                    new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
                string *buf_str = new string(parsed_data[name][i].first + " = " + parsed_data[name][i].second);
                NODE_STR(tmp->expr(0)->thellnd) = (char *)buf_str->c_str();
                call_st->insertStmtAfter(*tmp, *call_st->controlParent()->controlParent());
            }
        }
    }
}

map<SgStatement *, SgStatement *> module_allocations, module_deallocations;

void check_module(SgStatement *dyn_array_st, char *name, bool is_allocate, SgStatement *this_module, SgVarRefExp *sym,
                  SgStatement *ass_st, SgExpression *current_variable, SgStatement **bad_fun_arr) {
    SgStatement *ptfunc = nullptr;
    if ((is_allocate && module_allocations[this_module] == nullptr) ||
        (!is_allocate && module_deallocations[this_module] == nullptr)) {
        string temp_name("added_func_number_"), fun_name = temp_name;
        fun_name = temp_name + to_string(fun_count++);
        while (hashes.find(fun_name) != hashes.end()) {
            fun_name = temp_name + to_string(fun_count++);
        }
        hashes.insert(fun_name);
        ptfunc = new SgProcHedrStmt(const_cast<char *>(fun_name.c_str()));
        set_file_name(ptfunc, get_real_file_name(this_module));
        last_declarations[ptfunc] = ptfunc;
        SgModuleSymb *cur_mod = new SgModuleSymb(name);
        SgStatement *use_stmt = new SgStatement(
            USE_STMT, nullptr, cur_mod, new SgUseOnlyExp(*new SgExprListExp(*new SgVarRefExp(*(ptfunc->symbol())))),
            nullptr, nullptr);
        SgStatement *last_ex = this_module->lastExecutable();
        SgStatement *current_statement = this_module;
        //проверка, содержит ли данный модуль contains
        for (; current_statement != this_module->lastNodeOfStmt()->lexNext();
             current_statement = current_statement->lexNext()) {
            if (current_statement->variant() == CONTAINS_STMT) {
                break;
            }
        }
        last_ex->insertStmtAfter(*ptfunc);
        if (current_statement == this_module->lastNodeOfStmt()->lexNext()) {
            ptfunc->insertStmtBefore(*new SgDeclarationStatement(CONTAINS_STMT), *this_module);
        }
        SgCallStmt *call = new SgCallStmt(*(ptfunc->symbol()));
        set_file_name(call, get_real_file_name(main_fun));
        current_statement = main_fun;
        //проверка на то, есть ли в main USE для данного statement
        for (; current_statement != main_fun->lastExecutable() &&
               !(current_statement->variant() == USE_STMT &&
                 string(current_statement->symbol()->identifier()) == string(name));
             current_statement = current_statement->lexNext()) {
        };
        if (current_statement == main_fun->lastExecutable()) {
            main_fun->insertStmtAfter(*use_stmt, *main_fun);
            if (last_declarations[main_fun] == main_fun) {
                last_declarations[main_fun] = main_fun->lexNext();
            }
        } else {
            if (current_statement->expr(0) && current_statement->expr(0)->variant() == ONLY_NODE) {
                SgExprListExp *only_list =
                    static_cast<SgExprListExp *>(static_cast<SgUseOnlyExp *>(current_statement->expr(0))->onlyList());
                only_list->append(*new SgVarRefExp(*(ptfunc->symbol())));
            }
        }
        if (is_allocate) {
            update_last_declaration(main_fun, call, 0);
        } else {
            for (SgStatement *now = main_fun; now != main_fun->lastNodeOfStmt()->lexNext(); now = now->lexNext()) {
                if ((now->variant() == EXIT_STMT || now->variant() == RETURN_NODE || now->variant() == RETURN_STAT) &&
                    now->controlParent()->variant() != LOGIF_NODE &&
                    (now->lexPrev() && now->lexPrev()->variant() != EXIT_STMT &&
                     now->lexPrev()->variant() != RETURN_NODE && now->lexPrev()->variant() != RETURN_STAT)) {
                    now->insertStmtBefore(call->copy(), *now->controlParent());
                    string cur_file_name(wh(now)->fileName());
                    set_file_name(now, cur_file_name);
                }
            }
            main_fun->lastNodeOfStmt()->insertStmtBefore(*call, *main_fun);
        }
        if (is_allocate) {
            module_allocations[this_module] = ptfunc;
        } else {
            module_deallocations[this_module] = ptfunc;
        }
    } else {
        if (is_allocate) {
            ptfunc = module_allocations[this_module];
        } else {
            ptfunc = module_deallocations[this_module];
        }
    }
    SgStatement *check_result = createif(sym);
    ptfunc->insertStmtAfter(*dyn_array_st, *ptfunc);
    dyn_array_st->insertStmtAfter(*check_result, *ptfunc);
    SgCallStmt *call_st = new SgCallStmt(*(bad_fun_arr[current_file_id]->symbol()));
    set_file_name(call_st, get_real_file_name(this_module));
    set_file_name(check_result, get_real_file_name(this_module));
    string *first_str = new string(dyn_array_st->expr(0)->unparse()), *second_str = new string(create_hash(wh(ptfunc)));
    call_st->addArg(*new SgValueExp((char *)first_str->c_str()));
    call_st->addArg(*new SgValueExp((char *)second_str->c_str()));
    check_result->insertStmtAfter(*new SgCallStmt(*call_st), *check_result);
    if (is_allocate && ass_st) {
        init_data(name, call_st, current_variable);
        if (ass_st) {
            call_st->insertStmtAfter(*ass_st, *ptfunc);
        }
    }
}

SgStatement *is_into_interface(SgStatement *st) {
    while (st != nullptr && st->variant() != INTERFACE_STMT) {
        st = st->controlParent();
    }
    return st;
}
map<string, set<string>> checked_vars;
map<SgStatement *, SgVariableSymb *> space_stat;

void update_func_arg(SgExpression *exp, SgSymbol *new_symb, string &old_sym_id) {
    if (!exp) {
        return;
    }
    if (exp->variant() == FUNC_CALL) {
        auto fun_exp = static_cast<SgFunctionCallExp *>(exp);
        int arg_num = fun_exp->numberOfArgs();
        for (int i = 0; i < arg_num; ++i) {
            SgExpression *cur_arg = fun_exp->arg(i);
            if (cur_arg->symbol() && string(cur_arg->symbol()->identifier()) == old_sym_id) {
                cur_arg->setSymbol(new_symb);
            }
        }
    }
    update_func_arg(exp->lhs(), new_symb, old_sym_id);
    update_func_arg(exp->rhs(), new_symb, old_sym_id);
}

void main_parse(SgExpression *node_s, bool con_fl, SgStatement *st, SgStatement *ifmain, int i,
                vector<SgStatement *> &array_of_deallocations, vector<SgVariableSymb *> &stats,
                map<int, set<string>> &bad_ars, SgStatement **bad_fun_arr) {
    SgStatement *check_interface = is_into_interface(st);
    if (check_interface != nullptr) {
        return;
    }
    SgDeclarationStatement *current_statement = static_cast<SgDeclarationStatement *>(st), *new_dec = nullptr;
    SgStatement *current_space = wh(current_statement);
    int if_added = 0;
    SgExprListExp *gl_spec_list = new SgExprListExp(*new SgAttributeExp(ALLOCATABLE_OP));
    if (current_statement->expr(2)) {
        gl_spec_list->linkToEnd(current_statement->expr(2)->copy());
    }
    for (SgStatement *st = current_space; !if_added && st != last_declarations[current_space]; st = st->lexNext()) {
        if (st->variant() == INTERFACE_STMT) {
            st = st->lastNodeOfStmt();
            continue;
        }
        if (st->variant() == VAR_DECL) {
            SgVarDeclStmt *decl_st = static_cast<SgVarDeclStmt *>(st);
            if (decl_st->expr(1)->type()->variant() == current_statement->expr(1)->type()->variant() &&
                decl_st->expr(2)) {
                SgExprListExp *attr_list = static_cast<SgExprListExp *>(decl_st->expr(2));
                if (attr_list != nullptr && attr_list->length() == gl_spec_list->length()) {
                    int len = attr_list->length(), k = 0;
                    for (; k < len; ++k) {
                        int found = 0;
                        for (int t = 0; t < len; ++t) {
                            if (gl_spec_list->elem(k)->variant() == attr_list->elem(t)->variant()) {
                                found = 1;
                                break;
                            }
                        }
                        if (!found) {
                            break;
                        }
                    }

                    if (k == len) {
                        new_dec = static_cast<SgDeclarationStatement *>(st);
                        break;
                    }
                }
            }
        }
    }
    for (int k = 0; k < current_statement->numberOfVars(); ++k) {
        SgExpression *current_variable = current_statement->var(k);
        SgStatement *assign_stmt = nullptr;
        if (current_variable->variant() == ASSGN_OP) {
            SgArrayRefExp *ar_ref = new SgArrayRefExp(*current_variable->operand(1)->symbol());
            assign_stmt = new SgStatement(ASSIGN_STAT, nullptr, nullptr, ar_ref,
                                          &(current_variable->operand(2)->copy()), nullptr);
            current_variable = &current_variable->operand(1)->copy(); // если у нас есть присваивание массива
        }
        string space_name = create_hash(current_space), var_name = string(current_variable->symbol()->identifier());
        if (stat_to_dyn_ars[space_name].find(var_name) == stat_to_dyn_ars[space_name].end()) {
            continue;
        }
        if (stat_to_dyn_ars[space_name].find(var_name) != stat_to_dyn_ars[space_name].end() &&
            not_transformable_ars_stat_to_dyn[space_name].find(var_name) !=
                not_transformable_ars_stat_to_dyn[space_name].end()) {
            cout << "SORRY! CAN'T TRANSFORM ARRAY " << var_name << "IN FUNCTION " << space_name << endl;
            string st_file_name(get_real_file_name(st)), control_file_name(get_real_file_name(current_space));
            if (st_file_name != control_file_name) {
                if (susp_ars_from_inc[st_file_name].find(var_name) != susp_ars_from_inc[st_file_name].end()) {
                    SgExprListExp *new_list = new SgExprListExp(current_statement->var(k)->copy());
                    SgVarDeclStmt *new_dec = new SgVarDeclStmt(*new_list, current_statement->expr(2)->copy(),
                                                               *current_statement->expr(1)->type());
                    update_last_declaration(current_space, new_dec, 1);
                    set_file_name(new_dec, control_file_name);
                }
            }
        }
        if (current_variable->variant() == ARRAY_REF &&
            bad_ars[current_space->id()].find(var_name) == bad_ars[current_space->id()].end() &&
            not_transformable_ars_stat_to_dyn[space_name].find(var_name) ==
                not_transformable_ars_stat_to_dyn[space_name].end() &&
            checked_vars[space_name].find(var_name) == checked_vars[space_name].end()) {
            SgType *type = current_variable->symbol()->type();
            SgArrayType *ar_type = isSgArrayType(type);
            int n_dim = ar_type->dimension();
            int if_arg = 0;
            int par_ind = 0;
            if (ar_type->sizeInDim(0)->variant() != DDOT ||
                (ar_type->sizeInDim(0)->variant() == DDOT && strlen(ar_type->sizeInDim(0)->unparse()) > 1)) {
                if (current_space->variant() == FUNC_HEDR || current_space->variant() == PROC_HEDR) {
                    SgFunctionSymb *current_fun_symbol = static_cast<SgFunctionSymb *>(current_space->symbol());
                    for (; par_ind < current_fun_symbol->numberOfParameters(); ++par_ind) {
                        if (string(current_fun_symbol->parameter(par_ind)->identifier()) ==
                            current_variable->symbol()->identifier()) {
                            break;
                        }
                    }
                    if (par_ind != current_fun_symbol->numberOfParameters()) {
                        if_arg = 1; //если массив оказался параметром функции
                    }
                }
                int is_implicit = 0;
                SgExpression *cur_dim = ar_type->sizeInDim(0);
                SgArrayRefExp *arr = new SgArrayRefExp(*current_variable->symbol()),
                              *dearr = new SgArrayRefExp(*current_variable->symbol()),
                              *newarr = new SgArrayRefExp(*current_variable->symbol());
                SgExprListExp *allocate_list = new SgExprListExp(*arr), *deallocate_list = new SgExprListExp(*dearr);
                SgExprListExp *dims = new SgExprListExp(*cur_dim),
                              *newdims = new SgExprListExp(*new SgExpression(DDOT));
                SgType *basetype = new SgType(T_INT);
                SgVariableSymb *thisstat;
                string st_file_name(get_real_file_name(st)), space_file_name(get_real_file_name(current_space));
                if (cur_dim->variant() == STAR_RANGE) {
                    is_implicit = 1;
                }
                if (space_stat[current_space] == nullptr) {
                    string pat_name("this_stat"), cand_name = pat_name;
                    cand_name = pat_name + to_string(stat_count++);
                    while (sym_identifiers.find(cand_name) != sym_identifiers.end()) {
                        cand_name = pat_name + to_string(stat_count++);
                    }
                    sym_identifiers.insert(cand_name);
                    thisstat = new SgVariableSymb(cand_name.c_str(), basetype, current_space);
                    space_stat[current_space] = thisstat;
                    SgVarDeclStmt *stat_decl =
                        new SgVarDeclStmt(*new SgExprListExp(*new SgVarRefExp(*thisstat)), *basetype);
                    current_statement->insertStmtAfter(*stat_decl, *current_space);
                    set_file_name(stat_decl, space_file_name);
                    if (last_declarations[current_space] == current_statement) {
                        last_declarations[current_space] = stat_decl;
                    }
                } else {
                    thisstat = space_stat[current_space];
                }
                SgVarRefExp *stat_ref = new SgVarRefExp(thisstat);
                for (int i = 1; i < n_dim; i++) {
                    dims->append(*ar_type->sizeInDim(i));
                    newdims->append(*new SgExpression(DDOT));
                    if (ar_type->sizeInDim(i)->variant() == STAR_RANGE) {
                        is_implicit = 1;
                    }
                }
                arr->setLhs(dims);
                SgStatement *allocate_stmt = nullptr;
                SgStatement *deallocate_stmt = nullptr;
                if (!if_arg) {
                    allocate_stmt =
                        new SgStatement(ALLOCATE_STMT, nullptr, nullptr, allocate_list,
                                        new SgExpression(ASSGN_OP, new SgVarRefExp(*new SgVariableSymb("STAT")),
                                                         new SgVarRefExp(*thisstat), nullptr, nullptr),
                                        nullptr);
                    ;
                    deallocate_stmt =
                        new SgStatement(DEALLOCATE_STMT, nullptr, nullptr, deallocate_list,
                                        new SgExpression(ASSGN_OP, new SgVarRefExp(*new SgVariableSymb("STAT")),
                                                         new SgVarRefExp(*thisstat), nullptr, nullptr),
                                        nullptr);
                    set_file_name(allocate_stmt, space_file_name);
                    set_file_name(deallocate_stmt, space_file_name);
                    array_of_deallocations.push_back(deallocate_stmt);
                }
                stats.push_back(thisstat);
                checked_vars[space_name].insert(var_name);
                newarr->setLhs(*newdims);
                SgExprListExp *spec_list = static_cast<SgExprListExp *>(&gl_spec_list->copy());
                int is_target = 0;
                if ((current_variable->symbol()->numberOfAttributes(MY_TARGET_ATTRIBUTE) > 0 || if_arg) &&
                    !is_implicit) {
                    spec_list = new SgExprListExp(*new SgAttributeExp(TARGET_OP));
                    spec_list->linkToEnd(gl_spec_list->copy());
                    is_target = 1;
                }
                SgVarDeclStmt *new_var_decl = nullptr;
                if (is_target == 0 && new_dec != nullptr) {
                    new_dec->addVar(*newarr);
                } else {
                    new_var_decl = new SgVarDeclStmt(*new SgExprListExp(*newarr), *spec_list, *arr->type());
                    set_file_name(new_var_decl, st_file_name);
                    current_statement->insertStmtAfter(*new_var_decl);
                    if (last_declarations[current_space] == current_statement) {
                        last_declarations[current_space] = new_var_decl;
                    }
                }
                if (current_space->variant() == MODULE_STMT) {
                    check_module(allocate_stmt, current_space->symbol()->identifier(), true, current_space, stat_ref,
                                 assign_stmt, current_variable, bad_fun_arr);
                    check_module(deallocate_stmt, current_space->symbol()->identifier(), false, current_space, stat_ref,
                                 assign_stmt, current_variable, bad_fun_arr);
                    current_statement->deleteVar(k ? k + 1 : k);
                    k--;
                } else {
                    SgCallStmt *call_bad = nullptr;
                    auto new_arr_ref =
                        *new SgVarRefExp(*new SgVariableSymb(newarr->symbol()->identifier(), *ar_type, *current_space));
                    if (!if_arg) {
                        SgVarRefExp *now_stat_ref = new SgVarRefExp(*thisstat);
                        call_bad = new SgCallStmt(*(bad_fun_arr[i]->symbol()));
                        string *first_str = new string(allocate_stmt->expr(0)->unparse()),
                               *second_str = new string(create_hash(current_space));
                        call_bad->addArg(*new SgValueExp((char *)first_str->c_str()));
                        call_bad->addArg(*new SgValueExp((char *)second_str->c_str()));
                        SgStatement *bad_if = new SgStatement(LOGIF_NODE, nullptr, nullptr,
                                                              &(*now_stat_ref != *new SgValueExp(0)), nullptr, nullptr);
                        update_last_declaration(current_space, allocate_stmt, 0);
                        allocate_stmt->insertStmtAfter(*bad_if);
                        bad_if->insertStmtAfter(*call_bad, *bad_if);
                        set_file_name(bad_if, space_file_name);
                        set_file_name(call_bad, space_file_name);
                        init_data(space_name, call_bad, current_variable);
                    } else {
                        if (!is_implicit) {
                            SgExpression *cur_ref = current_statement->var(k);
                            SgExprListExp *current_dims = dims;
                            string pat_name = string("ar_pointer"), cand_name = pat_name;
                            int temp_count = 0;
                            SgFunctionSymb *fun_sym = static_cast<SgFunctionSymb *>(current_space->symbol());
                            while (sym_identifiers.find(cand_name) != sym_identifiers.end()) {
                                cand_name = pat_name + to_string(temp_count++);
                            }
                            sym_identifiers.insert(cand_name);
                            SgVariableSymb *new_sym =
                                new SgVariableSymb((char *)cand_name.c_str(), *cur_ref->symbol()->type());
                            string *new_str = new string(fun_sym->identifier());
                            SgFunctionSymb *new_fun_sym = new SgFunctionSymb(
                                fun_sym->variant(), new_str->c_str(), *SgTypeInt(), *current_file->firstStatement());
                            new_fun_sym->setType(fun_sym->type());
                            for (int j = 0; j < fun_sym->numberOfParameters(); ++j) {
                                if (j == par_ind) {
                                    new_fun_sym->insertParameter(j, *new_sym);
                                } else {
                                    new_fun_sym->insertParameter(j, fun_sym->parameter(j)->copy());
                                }
                            }
                            SgFunctionSymb *space_symb = static_cast<SgFunctionSymb *>(current_space->symbol());
                            current_space->setSymbol(*new_fun_sym);
                            static_cast<SgExprListExp *>(new_var_decl->expr(0))->elem(0)->setSymbol(*new_sym);
                            SgExprListExp *new_spec_list =
                                new SgExprListExp(*new SgExpression(POINTER_OP, nullptr, nullptr, nullptr, nullptr));
                            SgAttributeExp *dim_exp = new SgAttributeExp(DIMENSION_OP);
                            dim_exp->setLhs(newdims->copy());
                            new_spec_list->append(*dim_exp);
                            SgVarRefExp *sym_ref = new SgVarRefExp(*cur_ref->symbol());
                            SgVarDeclStmt *new_st =
                                new SgVarDeclStmt(*new SgExprListExp(*sym_ref), *new_spec_list, *newarr->type());
                            update_last_declaration(current_space, new_st, 1);
                            set_file_name(new_st, space_file_name);
                            SgVarRefExp *new_sym_ref = new SgVarRefExp(*new_sym);
                            SgExprListExp &list_exp = static_cast<SgExprListExp &>(current_dims->copy());
                            for (int y = 0; y < list_exp.length(); ++y) {
                                auto cur_exp = list_exp.elem(y);
                                if (cur_exp->variant() != DDOT) {
                                    SgExpression &tmp = cur_exp->copy();
                                    cur_exp->setVariant(DDOT);
                                    cur_exp->setRhs(tmp);
                                    cur_exp->setLhs(*new SgValueExp(1));
                                }
                            }
                            SgArrayRefExp *copy_ref = new SgArrayRefExp(*cur_ref->symbol(), list_exp);
                            string old_sym_id(copy_ref->symbol()->identifier());
                            SgPointerAssignStmt *new_ass = new SgPointerAssignStmt(*copy_ref, *new_sym_ref);
                            set_file_name(new_ass, space_file_name);
                            update_last_declaration(current_space, new_ass, 0);
                            for (SgStatement *st : raw_func_calls[space_name]) {
                                if (st->variant() == PROC_STAT) {
                                    SgCallStmt *call_st = static_cast<SgCallStmt *>(st);
                                    int arg_num = call_st->numberOfArgs();
                                    for (int i = 0; i < arg_num; ++i) {
                                        SgExpression *cur_arg = call_st->arg(i);
                                        if (cur_arg->symbol() &&
                                            string(cur_arg->symbol()->identifier()) == old_sym_id) {
                                            cur_arg->setSymbol(new_sym);
                                        }
                                    }
                                } else {
                                    for (int t = 0; t < 3; t++) {
                                        update_func_arg(st->expr(i), new_sym, old_sym_id);
                                    }
                                }
                            }
                        }
                        for (SgStatement *inter_ref : funs_interfaces[space_name]) {
                            SgFunctionSymb *fun_sym = isSgFunctionSymb(inter_ref->symbol());
                            string arr_name = string(fun_sym->parameter(par_ind)->identifier());
                            int not_found = 1;
                            for (SgStatement *cur_st = inter_ref; not_found && cur_st != inter_ref->lastNodeOfStmt();
                                 cur_st = cur_st->lexNext()) {
                                if (cur_st->variant() == VAR_DECL) {
                                    auto decl_lst = static_cast<SgExprListExp *>(cur_st->expr(0));
                                    for (int j = 0; j < decl_lst->length(); ++j) {
                                        string var_name = string(decl_lst->elem(j)->symbol()->identifier());
                                        if (var_name == arr_name) {
                                            SgVarDeclStmt *new_var_decl =
                                                new SgVarDeclStmt(*new SgExprListExp(*new SgArrayRefExp(
                                                                      *decl_lst->elem(j)->symbol(), newdims->copy())),
                                                                  spec_list->copy(), *newarr->type());

                                            static_cast<SgVarDeclStmt *>(cur_st)->deleteVar(j ? j + 1 : j);
                                            inter_ref->insertStmtAfter(*new_var_decl, *inter_ref);
                                            string inter_file_name(inter_ref->fileName());
                                            set_file_name(new_var_decl, inter_file_name);
                                            if (!cur_st->expr(0) ||
                                                static_cast<SgExprListExp *>(cur_st->expr(0))->length() == 0) {
                                                my_delete(cur_st);
                                            }
                                            not_found = 0;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if (assign_stmt) {
                        call_bad->insertStmtAfter(*assign_stmt, *call_bad->controlParent()->controlParent());
                        set_file_name(assign_stmt, space_file_name);
                    }
                    current_statement->deleteVar(k ? k + 1 : k);
                    k--;
                }
            }
        }
    }
}

//эта функция парсит начальные значения у переменных и размерностей массивов в модуле
void findinit(set<SgStatement *> &cur_modules, map<string, map<string, string>> &modules_names, SgExpression *exp,
              map<string, int> &dims, map<string, int> &par_dims, SgStatement *mod, SgStatement *to_ins,
              SgStatement *place_in_fun_to_ins) {
    if (!exp) {
        return;
    }
    if (exp->variant() == VAR_REF || exp->variant() == CONST_REF || exp->variant() == ARRAY_REF) {
        int if_found = 0;
        SgSymbol *sym = exp->symbol();
        for (auto iter = cur_modules.begin(); !if_found && iter != cur_modules.end(); ++iter) {
            SgStatement *now = *iter;
            char *current_symbol_name = (*iter)->symbol()->identifier();
            if ((*iter)->variant() == MODULE_STMT &&
                modules_names[current_symbol_name].find(sym->identifier()) ==
                    modules_names[current_symbol_name].end() &&
                modules_names[current_symbol_name].find(string(";all;")) == modules_names[current_symbol_name].end()) {
                continue;
            }
            string new_name =
                modules_names[current_symbol_name].find(string(";all;")) == modules_names[current_symbol_name].end()
                    ? modules_names[current_symbol_name][sym->identifier()]
                    : string(sym->identifier());
            for (SgStatement *k = last_declarations[now]; k != now; k = k->lexPrev()) {
                bool fl = if_parameter(k); //проверяем, не является ли данный стейтмент var decl с parameter условием
                if (k->variant() == PARAM_DECL) {
                    SgParameterStmt *lis = static_cast<SgParameterStmt *>(k);
                    for (int j = 0; j < lis->numberOfConstants(); ++j) {
                        if (lis->constant(j)->identifier() == new_name) {
                            if_found = 1;
                            SgSymbol *current_constant = lis->constant(j);
                            if (par_dims[current_constant->identifier()] >= 1) {
                                return;
                            } else {
                                par_dims[current_constant->identifier()]++;
                                SgVarRefExp *expr = new SgVarRefExp(current_constant);
                                SgExpression &ass_op = *new SgExpression(ASSGN_OP, expr, lis->value(j));
                                SgVarDeclStmt *new_st = new SgVarDeclStmt(*new SgExprListExp(ass_op));
                                if (k->expr(1)) {
                                    new_st->setExpression(1, *k->expr(1));
                                } else {
                                    int type_fl = 1;
                                    for (SgStatement *st = k; type_fl && st != now; st = st->lexPrev()) {
                                        if (st->variant() == VAR_DECL) {
                                            SgVarDeclStmt *dec_st = static_cast<SgVarDeclStmt *>(st);
                                            for (int i = 0; i < dec_st->numberOfSymbols(); ++i) {
                                                if (string(dec_st->symbol(i)->identifier()) == sym->identifier()) {
                                                    new_st->setExpression(1, *st->expr(1));
                                                    type_fl = 0;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    if (type_fl) {
                                        new_st->setExpression(1, *new SgTypeExp(*new SgType(T_FLOAT)));
                                    }
                                }
                                if (k->expr(2)) {
                                    new_st->setExpression(2, k->expr(2)->copy());
                                    SgExprListExp *lst = static_cast<SgExprListExp *>(new_st->expr(2));
                                    lst->append(*new SgAttributeExp(PRIVATE_OP));
                                } else {
                                    SgExprListExp *scope_l = new SgExprListExp(*new SgAttributeExp(PRIVATE_OP));
                                    scope_l->append(*new SgAttributeExp(PARAMETER_OP));
                                    new_st->setExpression(2, *scope_l);
                                }
                                place_in_fun_to_ins->insertStmtBefore(*new_st);
                                if (last_declarations[mod]->lexNext() == new_st) {
                                    last_declarations[mod] = new_st;
                                }
                                if (now->variant() != MODULE_STMT) {
                                    findinit(cur_modules, modules_names, lis->value(j), dims, par_dims, mod, to_ins,
                                             place_in_fun_to_ins->lexPrev());
                                } else {
                                    set<SgStatement *> new_mod;
                                    new_mod.insert(now);
                                    map<string, map<string, string>> new_names;
                                    new_names[current_symbol_name][string(";all;")] = string(";all;");
                                    findinit(new_mod, new_names, lis->value(j), dims, par_dims, mod, to_ins,
                                             place_in_fun_to_ins->lexPrev());
                                }
                                place_in_fun_to_ins = place_in_fun_to_ins->lexPrev();
                            }
                        }
                    }
                }
                if (fl) {
                    k->setVariant(VAR_DECL);
                }
            }
        }
        if (!if_found) { //может быть плохо, лучше эту логику в анпарсе делать
            string *new_inc = new string("include 'mpif.h'");
            SgStatement *tmp1 =
                new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
            NODE_STR(tmp1->expr(0)->thellnd) = const_cast<char *>(new_inc->c_str());
            mod->insertStmtAfter(*tmp1);
            string mod_name(mod->fileName());
            set_file_name(tmp1, mod_name);
        }
    } else {
        findinit(cur_modules, modules_names, exp->lhs(), dims, par_dims, mod, to_ins, place_in_fun_to_ins);
        findinit(cur_modules, modules_names, exp->rhs(), dims, par_dims, mod, to_ins, place_in_fun_to_ins);
    }
}

void superfun(SgExprListExp *exp, bool mode, bool fl, SgStatement *st, SgStatement *mod, map<string, int> &dims,
              map<string, int> &par_dims, SgStatement *func) {
    set<SgStatement *> cur_modules;
    SgStatement *now = wh(st);
    cur_modules.insert(now);
    set<string> cur_names;
    map<string, map<string, string>> modules_names;
    modules_names[string(now->symbol()->identifier())][string(";all;")] = string(";all;");
    SgStatement *fin_st = last_declarations[now]->lexNext();
    for (SgStatement *st = now; st != fin_st; st = st->lexNext()) {
        if (st->variant() == USE_STMT) {
            cur_names.insert(st->symbol()->identifier());
            int vis_fl = 0;
            if (st->expr(0) && st->expr(0)->variant() == ONLY_NODE) {
                SgExprListExp *tmp = static_cast<SgExprListExp *>(static_cast<SgUseOnlyExp *>(st->expr(0))->onlyList());
                for (int k = 0; k < tmp->length(); ++k) {
                    if (tmp->elem(k)->symbol()) {
                        modules_names[st->symbol()->identifier()][tmp->elem(k)->symbol()->identifier()] =
                            tmp->elem(k)->symbol()->identifier();
                        vis_fl = 1;
                    } else {
                        modules_names[st->symbol()->identifier()][tmp->elem(k)->operand(2)->symbol()->identifier()] =
                            tmp->elem(k)->operand(1)->symbol()->identifier();
                        vis_fl = 1;
                    }
                }
            }

            if (!vis_fl) {
                modules_names[st->symbol()->identifier()][string(";all;")] = string(";all;");
            }
        }
    }
    for (auto iter = modules.begin(); iter != modules.end(); ++iter) {
        if (cur_names.find((*(iter))->symbol()->identifier()) != cur_names.end()) {
            cur_modules.insert(*iter);
        }
    }
    for (int i = 0; i < exp->length(); ++i) {
        SgStatement *current_st = now;
        do {
            current_st = current_st->lexNext();
            bool fl = if_parameter(current_st);
            if (current_st->expr(0) && current_st->expr(0)->variant() == EXPR_LIST &&
                (current_st->variant() == VAR_DECL || current_st->variant() == PARAM_DECL)) {
                SgDeclarationStatement *decl_st = static_cast<SgDeclarationStatement *>(current_st);
                set<string> identifiers;
                for (int j = 0; j < decl_st->numberOfVars(); ++j) {
                    int var = decl_st->var(j)->variant() == ASSGN_OP &&
                                      string(decl_st->var(j)->operand(1)->symbol()->identifier()) ==
                                          exp->elem(i)->symbol()->identifier()
                                  ? 2
                                  : decl_st->var(j)->symbol() ? string(decl_st->var(j)->symbol()->identifier()) ==
                                                                        exp->elem(i)->symbol()->identifier()
                                                                    ? 1
                                                                    : 0
                                                              : 0;
                    if (var) {
                        ;
                        SgExpression *current_variable = var == 1 ? decl_st->var(j) : decl_st->var(j)->operand(1);
                        if (exp->elem(i)->symbol()->type()->variant() == T_ARRAY) {
                            SgArrayType *arr = static_cast<SgArrayType *>(current_variable->symbol()->type());
                            if (mode) {
                                for (int k = 0; k < arr->dimension(); k++) {
                                    SgExpression *dem_expr = arr->sizeInDim(k);
                                    findinit(cur_modules, modules_names, dem_expr, dims, par_dims, mod,
                                             last_declarations[func]->lexNext(), last_declarations[mod]->lexNext());
                                }
                                SgArrayRefExp *arr_ref = new SgArrayRefExp(*current_variable->symbol());
                                SgArrayType *arr_type = static_cast<SgArrayType *>(current_variable->symbol()->type());
                                SgExprListExp *ar_ref_list = new SgExprListExp(*arr_ref);
                                SgExprListExp *this_dims = new SgExprListExp(*arr_type->sizeInDim(0));
                                for (int t = 1; t < arr_type->dimension(); t++) {
                                    this_dims->append(*arr_type->sizeInDim(t));
                                }
                                arr_ref->setLhs(this_dims);
                                SgStatement *new_decl_st = nullptr;
                                SgExpression *init_value = nullptr;
                                if (current_st->variant() == VAR_DECL) {
                                    SgVarDeclStmt *var_decl_st = static_cast<SgVarDeclStmt *>(current_st);
                                    new_decl_st = new SgVarDeclStmt(*ar_ref_list, *var_decl_st->type());
                                    init_value = var_decl_st->initialValue((j));
                                } else {
                                    SgParameterStmt *par_st = static_cast<SgParameterStmt *>(current_st);
                                    new_decl_st = new SgParameterStmt(*ar_ref_list = *par_st->value(j));
                                    init_value = par_st->value((j));
                                }
                                update_last_declaration(mod, new_decl_st, 1);
                                if (init_value) {
                                    if (current_st->variant() == VAR_DECL) {
                                        update_last_declaration(
                                            func,
                                            new SgAssignStmt(
                                                *new SgVarRefExp(*static_cast<SgVarDeclStmt *>(current_st)->symbol(j)),
                                                *init_value),
                                            0);
                                    }
                                    auto list_find = init_value;
                                    while (list_find && list_find->variant() != CONSTRUCTOR_REF)
                                        list_find = list_find->lhs();
                                    if (list_find) {
                                        auto list = static_cast<SgExprListExp *>(list_find);
                                        for (int u = 0; u < list->length(); ++u) {
                                            findinit(cur_modules, modules_names, list->elem(u), dims, par_dims, mod,
                                                     last_declarations[func]->lexNext(),
                                                     last_declarations[mod]->lexNext());
                                        }
                                    } else {
                                        cout << "Sorry, such kind of initialization not implemnted" << endl;
                                    }
                                }
                            }
                            if (decl_st->var(j)->symbol()) {
                                identifiers.insert(decl_st->var(j)->symbol()->identifier());
                            } else {
                                identifiers.insert(decl_st->var(j)->operand(1)->symbol()->identifier());
                            }
                        } else {
                            if (current_st->variant() == VAR_DECL) {
                                SgVarDeclStmt *decl_st = static_cast<SgVarDeclStmt *>(current_st);
                                if (mode) {
                                    if (dims[decl_st->symbol(j)->identifier()] < 1) {
                                        dims[decl_st->symbol(j)->identifier()]++;
                                        if (exp->elem(i)->lhs()) {
                                            findinit(cur_modules, modules_names, exp->elem(i)->lhs(), dims, par_dims,
                                                     mod, last_declarations[func]->lexNext(),
                                                     last_declarations[mod]->lexNext());
                                        }
                                        SgStatement *new_decl_st = nullptr;
                                        SgExpression *decl_core = nullptr;
                                        if (decl_st->initialValue((j))) {
                                            decl_core = new SgExpression(ASSGN_OP, new SgVarRefExp(*decl_st->symbol(j)),
                                                                         decl_st->initialValue((j)), nullptr, nullptr);
                                            new_decl_st =
                                                new SgVarDeclStmt(*new SgExprListExp(*decl_core), *decl_st->type());
                                            if (!current_st->expr(2)) {
                                                SgExprListExp *scope_l =
                                                    new SgExprListExp(*new SgAttributeExp(PUBLIC_OP));
                                                new_decl_st->setExpression(2, *scope_l);
                                            }
                                        } else {
                                            decl_core = new SgVarRefExp(*decl_st->symbol(j));
                                            new_decl_st = new SgVarDeclStmt(
                                                *new SgExprListExp(static_cast<SgExpression &>(*decl_core)),
                                                *decl_st->type());
                                        }
                                        if (current_st->expr(2)) {
                                            new_decl_st->setExpression(2, *current_st->expr(2));
                                        }
                                        int insert_fl = 0;
                                        SgStatement *fin_st = last_declarations[mod]->lexNext();
                                        for (SgStatement *st = mod; !insert_fl && st != fin_st; st = st->lexNext()) {
                                            if (st->variant() == VAR_DECL) {
                                                if (st->expr(1)->type()->variant() ==
                                                    new_decl_st->expr(1)->type()->variant()) {
                                                    if (new_decl_st->expr(2) && st->expr(2)) {
                                                        SgExprListExp *list = static_cast<SgExprListExp *>(st->expr(2));
                                                        SgExprListExp *new_list =
                                                            static_cast<SgExprListExp *>(new_decl_st->expr(2));
                                                        if (list->length() != new_list->length()) {
                                                            break;
                                                        }
                                                        int check_fl = 0;
                                                        for (int j = 0; j < list->length(); ++j) {
                                                            for (int k = 0; k < new_list->length(); ++k) {
                                                                if (new_list->elem(k)->variant() ==
                                                                    list->elem(j)->variant()) {
                                                                    if (new_list->elem(k)->variant() == DIMENSION_OP) {
                                                                        SgExprListExp *new_dim =
                                                                            static_cast<SgExprListExp *>(
                                                                                new_list->elem(k)->lhs());
                                                                        SgExprListExp *dim =
                                                                            static_cast<SgExprListExp *>(
                                                                                list->elem(j)->lhs());
                                                                        if (new_dim->length() == dim->length()) {
                                                                            check_fl += 1;
                                                                            break;
                                                                        }
                                                                    } else {
                                                                        check_fl += 1;
                                                                        break;
                                                                    }
                                                                }
                                                            }
                                                        }

                                                        if (check_fl == list->length()) {
                                                            SgExprListExp *list =
                                                                static_cast<SgExprListExp *>(st->expr(0));
                                                            list->append(*decl_core);
                                                            insert_fl = 1;
                                                            break;
                                                        }

                                                    } else {
                                                        if (!new_decl_st->expr(2) && !st->expr(2)) {
                                                            SgExprListExp *list =
                                                                static_cast<SgExprListExp *>(st->expr(0));
                                                            list->append(*decl_core);
                                                            insert_fl = 1;
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        if (!insert_fl) {
                                            update_last_declaration(mod, new_decl_st, 1);
                                        }
                                        if (var != 1) {
                                            if (decl_st->initialValue((j))) {
                                                findinit(cur_modules, modules_names, decl_st->initialValue(j), dims,
                                                         par_dims, mod, last_declarations[func]->lexNext(),
                                                         last_declarations[mod]->lexNext());
                                            }
                                        }
                                    }
                                }
                                if (decl_st->var(j)->symbol()) {
                                    identifiers.insert(decl_st->var(j)->symbol()->identifier());
                                } else {
                                    identifiers.insert(decl_st->var(j)->operand(1)->symbol()->identifier());
                                }
                            } else if (current_st->variant() == PARAM_DECL) {
                                SgParameterStmt *param_st = static_cast<SgParameterStmt *>(current_st);
                                if (mode) {
                                    if (par_dims[param_st->constant(j)->identifier()] < 1) {
                                        par_dims[param_st->constant(j)->identifier()]++;
                                        SgExprListExp *expr = new SgExprListExp(
                                            *static_cast<SgExpression *>(new SgVarRefExp(param_st->constant(j))));
                                        update_last_declaration(
                                            mod, new SgParameterStmt(*new SgExprListExp(*expr = *param_st->value(j))),
                                            1);
                                        findinit(cur_modules, modules_names, param_st->value(j), dims, par_dims, mod,
                                                 last_declarations[func]->lexNext(), last_declarations[mod]->lexNext());
                                    }
                                } //мб тут тоже инициализацию без mode
                                identifiers.insert(decl_st->var(j)->symbol()->identifier());
                            }
                        }
                    }
                }
                for (int i = decl_st->numberOfVars() - 1; i >= 0; --i) {
                    if (decl_st->var(i)->symbol()) {
                        if (identifiers.find(decl_st->var(i)->symbol()->identifier()) != identifiers.end()) {
                            decl_st->deleteVar(i ? i + 1 : i);
                        }
                    } else {
                        if (identifiers.find(decl_st->var(i)->operand(1)->symbol()->identifier()) !=
                            identifiers.end()) {
                            decl_st->deleteVar(i ? i + 1 : i);
                        }
                    }
                }
            }
            if (fl) {
                current_st->setVariant(VAR_DECL);
            }
        } while (current_st != last_declarations[now]);
    }
    //работаем с data стейтментами
    if (!mode) {
        return;
    }
    string func_name = create_hash(wh(st));
    for (int i = 0; i < exp->length(); ++i) {
        for (long unsigned int m = 0; m < parsed_data[func_name].size(); ++m) {
            long unsigned int pos = parsed_data[func_name][m].first.find('(');
            if (pos == string::npos) {
                pos = parsed_data[func_name][m].first.length();
            }
            if (exp->elem(i)->symbol()->identifier() == parsed_data[func_name][m].first.substr(0, pos)) {
                SgStatement *tmp =
                    new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
                string &str = parsed_data[func_name][m].second;
                int first = 0;
                for (long unsigned int i = 0; i < str.length();) {
                    while (i < str.length() && str[i] != '+' && str[i] != '*' && str[i] != '-' && str[i] != '/' &&
                           str[i] != ',') {
                        ++i;
                    }
                    string name = str.substr(first, i - first);
                    int find_fl = 0;
                    for (auto iter = cur_modules.begin(); iter != cur_modules.end(); ++iter) {
                        SgStatement *cur_sp = *iter;
                        char *symbol_name = (*iter)->symbol()->identifier();
                        if ((*iter)->variant() == MODULE_STMT &&
                            modules_names[symbol_name].find(string(";all;")) == modules_names[symbol_name].end() &&
                            modules_names[symbol_name].find(name) == modules_names[symbol_name].end()) {
                            continue;
                        }
                        name = modules_names[symbol_name].find(string(";all;")) == modules_names[symbol_name].end()
                                   ? modules_names[symbol_name][name]
                                   : name;
                        if (find_fl) {
                            break;
                        }
                        SgStatement *fin_st = last_declarations[cur_sp]->lexNext();
                        for (auto st = cur_sp; st != fin_st; st = st->lexNext()) {
                            bool fl = if_parameter(st);
                            if (st->variant() == PARAM_DECL) {
                                SgParameterStmt *param_st = static_cast<SgParameterStmt *>(st);
                                for (int j = 0; j < param_st->numberOfConstants(); ++j) {
                                    if (param_st->constant(j)->identifier() == name) {
                                        if (par_dims[param_st->constant(j)->identifier()] < 1) {
                                            auto fin_st = last_declarations[cur_sp]->lexNext();
                                            for (auto tmp = cur_sp; tmp != fin_st; tmp = tmp->lexNext()) {
                                                if (tmp->variant() == VAR_DECL) {
                                                    SgVarDeclStmt *var_decl_st = static_cast<SgVarDeclStmt *>(tmp);
                                                    for (int k = 0; k < var_decl_st->numberOfSymbols(); ++k) {
                                                        if (var_decl_st->symbol(k)->identifier() == name) {
                                                            mod->insertStmtAfter(
                                                                *new SgVarDeclStmt(*new SgExprListExp(*new SgVarRefExp(
                                                                                       *var_decl_st->symbol(k))),
                                                                                   *var_decl_st->type()));
                                                            if (last_declarations[mod] == mod) {
                                                                last_declarations[mod] = mod->lexNext();
                                                            }
                                                            find_fl = 1;
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                            par_dims[param_st->constant(j)->identifier()]++;
                                            if (now->variant() != MODULE_STMT) {
                                                findinit(cur_modules, modules_names, param_st->value(j), dims, par_dims,
                                                         mod, last_declarations[func]->lexNext(),
                                                         last_declarations[mod]->lexNext());
                                            } else {
                                                set<SgStatement *> new_mod;
                                                new_mod.insert(now);
                                                map<string, map<string, string>> new_names;
                                                new_names[symbol_name][string(";all;")] = string(";all;");
                                                findinit(new_mod, new_names, param_st->value(j), dims, par_dims, mod,
                                                         last_declarations[func]->lexNext(),
                                                         last_declarations[mod]->lexNext());
                                            }
                                            SgVarDeclStmt *new_st = new SgVarDeclStmt(*new SgExprListExp(
                                                *new SgExpression(ASSGN_OP, new SgVarRefExp(param_st->constant(j)),
                                                                  param_st->value(j))));
                                            if (param_st->expr(1)) {
                                                new_st->setExpression(1, param_st->expr(1)->copy());
                                            } else {
                                                new_st->setExpression(1, *new SgTypeExp(*new SgType(T_FLOAT)));
                                            }
                                            if (param_st->expr(2)) {
                                                new_st->setExpression(2, param_st->expr(2)->copy());
                                                int found1 = 0, found2 = 0;
                                                SgExprListExp *lst = static_cast<SgExprListExp *>(new_st->expr(2));
                                                for (int l = 0; l < lst->length(); ++l) {
                                                    if (lst->elem(l)->variant() == PARAMETER_OP) {
                                                        found1 = 1;
                                                    }
                                                    if (lst->elem(l)->variant() == PRIVATE_OP) {
                                                        found2 = 1;
                                                    }
                                                }
                                                if (!found1) {
                                                    lst->append(*new SgAttributeExp(PARAMETER_OP));
                                                }
                                                if (!found2) {
                                                    lst->append(*new SgAttributeExp(PRIVATE_OP));
                                                }
                                            } else {
                                                auto new_lst = new SgExprListExp(*new SgAttributeExp(PARAMETER_OP));
                                                new_lst->append(*new SgAttributeExp(PRIVATE_OP));
                                                new_st->setExpression(2, *new_lst);
                                            }
                                            update_last_declaration(mod, new_st, 1);
                                        }
                                    }
                                }
                            }
                            if (fl) {
                                st->setVariant(VAR_DECL);
                            }
                        }
                    }
                    i = i + 1;
                    first = i;
                }
                int has_string_intializers = 0, gl_c = 0;
                for (long unsigned int t = 0; t < parsed_data[func_name][m].second.length(); ++t) {
                    if (parsed_data[func_name][m].second[t] == '\'') {
                        has_string_intializers = 1;
                        int start_ind = t;
                        t++;
                        while (parsed_data[func_name][m].second[t] != '\'') {
                            t++;
                        }
                        string *buf_str =
                            new string(parsed_data[func_name][m].first + "(" + to_string(gl_c++) + ")" + " = " +
                                       parsed_data[func_name][m].second.substr(start_ind, t - start_ind + 1));
                        NODE_STR(tmp->expr(0)->thellnd) = (char *)buf_str->c_str();
                        func->lastExecutable()->insertStmtAfter(*tmp);
                    }
                }
                if (!has_string_intializers) {
                    string *buf_str =
                        new string(parsed_data[func_name][m].first + " = " + parsed_data[func_name][m].second);
                    NODE_STR(tmp->expr(0)->thellnd) = (char *)buf_str->c_str();
                    func->lastExecutable()->insertStmtAfter(*tmp, *func);
                }
            }
        }
    }
}

void BlocktoMod(SgExpression *expr, SgStatement *st, SgFile &f, int mode) {
    SgStatement *mod = nullptr, *ptfunc = nullptr;
    SgNestedVarListDeclStmt *decl_st = static_cast<SgNestedVarListDeclStmt *>(st);
    SgExprListExp *exp_list = static_cast<SgExprListExp *>(decl_st->list(0));
    char *prev_mod_name = expr->symbol()->identifier();
    if (mode) {
        string mod_string_name(prev_mod_name), new_name = mod_string_name;
        int tries = 0;
        while (sym_identifiers.find(new_name) != sym_identifiers.end()) {
            new_name = mod_string_name + to_string(tries++);
        }
        com_mod_names[mod_string_name] = new_name;
        sym_identifiers.insert(new_name);
        string *dyn_new_name = new string(new_name);
        char *char_mod_name = const_cast<char *>(dyn_new_name->c_str());
        mod = new SgStatement(PROC_HEDR, nullptr, static_cast<SgSymbol *>(new SgModuleSymb(char_mod_name)), nullptr,
                              nullptr, nullptr);
        string pat = "module_declaration_function_", name = pat + to_string(dec_count);
        while (hashes.find(name) != hashes.end()) {
            name = pat + to_string(dec_count++);
        }
        hashes.insert(name);
        mod_dec[mod->symbol()->identifier()] = name;
        char *cur_hed = const_cast<char *>(name.c_str());
        ptfunc = new SgProcHedrStmt(cur_hed);
        last_declarations[ptfunc] = ptfunc;
        f.firstStatement()->insertStmtAfter(*mod);
        mod = f.firstStatement()->lexNext();
        last_declarations[mod] = mod;
    } else {
        string mod_name = com_mod_names[prev_mod_name];
        for (SgStatement *cur_mod : modules) {
            if (string(cur_mod->symbol()->identifier()) == mod_name) {
                mod = cur_mod;
                break;
            }
        }
    }
    map<string, int> dims, par_dims;
    superfun(exp_list, mode, true, st, mod, dims, par_dims, ptfunc);
    string module_name = string(mod->symbol()->identifier());
    if (mode) {
        update_last_declaration(mod, ptfunc, 0);
        ptfunc->insertStmtBefore(*new SgDeclarationStatement(CONTAINS_STMT), *mod);
        mod->setVariant(MODULE_STMT);
        for (int j = 0; j < exp_list->length(); ++j) {
            commons[module_name].push_back(exp_list->elem(j)->symbol()->identifier());
        }
    }
    vector<string> cur;
    for (long unsigned int i = 0; i < commons[module_name].size(); ++i) {
        cur.push_back(string(exp_list->elem(i)->symbol()->identifier()));
    }
    if (mode && ptfunc->lastNodeOfStmt() == ptfunc->lexNext()) {
        ptfunc->lexPrev()->deleteStmt();
        ptfunc->deleteStmt();
        ptfunc = nullptr;
    }
    decl_st->deleteList(0);
    SgStatement *use_st = new SgStatement(USE_STMT, nullptr, mod->symbol(), nullptr, nullptr, nullptr);
    int only_fl = 0;
    for (long unsigned int i = 0; i < commons[module_name].size(); ++i) {
        if (cur[i] != commons[module_name][i]) {
            only_fl = 1;
            break;
        }
    }
    if (only_fl) {
        SgExprListExp *only_lst = nullptr;
        if (cur[0] != commons[module_name][0]) {
            only_lst = new SgExprListExp(
                *new SgExpression(RENAME_NODE, new SgVarRefExp(*new SgVariableSymb(cur[0].c_str())),
                                  new SgVarRefExp(*new SgVariableSymb(commons[module_name][0].c_str())), nullptr));
        } else {
            only_lst = new SgExprListExp(*new SgVarRefExp(*new SgVariableSymb((char *)cur[0].c_str())));
        }
        for (long unsigned int i = 1; i < commons[module_name].size(); ++i) {
            if (cur[i] != commons[module_name][i]) {
                only_lst->append(
                    *new SgExpression(RENAME_NODE, new SgVarRefExp(*new SgVariableSymb(cur[i].c_str())),
                                      new SgVarRefExp(*new SgVariableSymb(commons[module_name][i].c_str())), nullptr));
            } else {
                only_lst->append(*new SgVarRefExp(*new SgVariableSymb(cur[i].c_str())));
            }
        }
        if (mode && ptfunc != nullptr) {
            only_lst->append(*new SgVarRefExp(*new SgVariableSymb(mod_dec[mod->symbol()->identifier()].c_str())));
        }
        use_st->setExpression(0, *new SgUseOnlyExp(*only_lst));
    }
    SgStatement *space = nullptr;
    if (wh(st)->variant() == BLOCK_DATA) {
        if (mode && ptfunc != nullptr) {
            space = main_fun;
            bool fl = false;
            for (auto tmp = space; tmp != space->lastNodeOfStmt(); tmp = tmp->lexNext()) {
                if ((tmp->variant() == USE_STMT && string(tmp->symbol()->identifier()) == module_name) ||
                    (tmp->expr(0) && tmp->expr(0)->variant() == COMM_LIST &&
                     string(tmp->expr(0)->symbol()->identifier()) == module_name)) {
                    if (tmp->variant() == USE_STMT && tmp->expr(0)) {
                        auto only_list =
                            static_cast<SgExprListExp *>(static_cast<SgUseOnlyExp *>(tmp->expr(0))->onlyList());
                        only_list->append(*new SgVarRefExp(*(ptfunc->symbol())));
                    }
                    fl = true;
                    break;
                }
            }
            SgCallStmt *call = new SgCallStmt(*(ptfunc->symbol()));
            string space_name(space->fileName());
            set_file_name(call, space_name);
            if (fl) {
                update_last_declaration(space, call, 0);
            } else {
                SgExprListExp only_list =
                    *new SgExprListExp(*new SgVarRefExp(*new SgVariableSymb(mod_dec[module_name].c_str())));
                use_st->setExpression(0, *new SgUseOnlyExp(only_list));
                space->insertStmtAfter(*use_st);
                if (last_declarations[space] == space) {
                    last_declarations[space] = space->lexNext();
                }
                set_file_name(use_st, space_name);
                update_last_declaration(space, call, 0);
            }
        }
    } else {
        space = wh(st);
        space->insertStmtAfter(*use_st);
        if (last_declarations[space] == space) {
            last_declarations[space] = space->lexNext();
        }
        string space_name(space->fileName());
        set_file_name(use_st, space_name);
        if (ptfunc != nullptr && mode) {
            auto call = new SgCallStmt(*(ptfunc->symbol()));
            set_file_name(call, space_name);
            update_last_declaration(space, call, 0);
        }
    }
    if (mode) {
        stat_to_dyn_ars[create_hash(mod)] = stat_to_dyn_ars[create_hash(wh(st))];
        modules.insert(mod);
    }
    my_delete(st);
}

double calculate_size(SgExpression *, set<SgStatement *> &, map<string, map<string, string>> &);

bool sortinrev(const pair<int, int> &a, const pair<int, int> &b) { return a.first > b.first; }

string tolower(string s) {
    string ans = s;
    for_each(ans.begin(), ans.end(), [](char &c) { c = tolower(c); });
    return ans;
}

void init_module_stat(SgStatement *scope, set<SgStatement *> &cur_modules,
                      map<string, map<string, string>> &modules_names) {
    set<string> cur_names;
    SgStatement *tmp = scope;
    while (tmp) {
        if (tmp->symbol()) {
            cur_modules.insert(tmp);
            modules_names[tmp->symbol()->identifier()][string(";all;")] = string(";all;");
        }
        tmp = tmp->controlParent();
    }
    for (SgStatement *st = scope; st != last_declarations[scope]; st = st->lexNext()) {
        if (st->variant() == USE_STMT) {
            cur_names.insert(st->symbol()->identifier());
            int vis_fl = 0;
            if (st->expr(0) && st->expr(0)->variant() == ONLY_NODE) {
                SgExprListExp *tmp = static_cast<SgExprListExp *>(static_cast<SgUseOnlyExp *>(st->expr(0))->onlyList());
                for (int k = 0; k < tmp->length(); ++k) {
                    if (tmp->elem(k)->variant() == RENAME_NODE && tmp->elem(k)->operand(1) &&
                        tmp->elem(k)->operand(2)) {
                        modules_names[st->symbol()->identifier()][tmp->elem(k)->operand(2)->symbol()->identifier()] =
                            string(tmp->elem(k)->operand(1)->symbol()->identifier());
                        vis_fl = 1;
                    } else {
                        modules_names[st->symbol()->identifier()][tmp->elem(k)->symbol()->identifier()] =
                            string(tmp->elem(k)->symbol()->identifier());
                    }
                }
            }
            if (!vis_fl) {
                modules_names[(char *)st->symbol()->identifier()][string(";all;")] = string(";all;");
            }
        }
    }
    for (auto iter = modules.begin(); iter != modules.end(); ++iter) {
        if (cur_names.find((*(iter))->symbol()->identifier()) != cur_names.end()) {
            cur_modules.insert(*iter);
        }
    }
}

void data_parse(SgProject &proj) {
    map<string, int> current_num;
    for (int k = 0; k < proj.numberOfFiles(); ++k) {
        SgFile &cur_file = proj.file(k);
        auto cur_st = cur_file.firstStatement();
        for (; cur_st;) {
            SgStatement *tmp = cur_st->lexNext();
            if (cur_st->variant() == DATA_DECL) {
                set<string> names;
                int is_bad = 0;
                SgStatement *scope = wh(cur_st);
                string name = create_hash(scope);
                int fl = 0;
                vector<pair<int, int>> var_pos, val_pos;
                set<int> to_del_vars;
                string data_str = string(cur_st->expr(0)->unparse());
                int flag = 0;
                for (auto it = data_str.begin(); it != data_str.end(); ++it) {
                    if (*it == '\'') {
                        flag = ~flag;
                    }
                    if (isspace(*it)) {
                        if (!flag) {
                            data_str.erase(it);
                        }
                    }
                }
                data_str.erase(0, 4);
                int first = 0, second = 0, current_count = 0;
                pair<string, string> name_value;
                int ar_elem_fl = 0;
                map<string, map<string, string>> modules_names;
                set<SgStatement *> cur_modules;
                init_module_stat(scope, cur_modules, modules_names);
                for (; first < data_str.length();) {
                    if (data_str[first] == '\'') {
                        first++;
                        while (data_str[first] != '\'') {
                            first++;
                        }
                    }
                    if (!fl) {
                        while ((ar_elem_fl || data_str[first] != ',') && data_str[first] != '/') {
                            if (data_str[first] == '(') {
                                ar_elem_fl = 1;
                            } else if (data_str[first] == ')') {
                                ar_elem_fl = 0;
                            }
                            first++;
                        }
                        name_value.first = tolower(data_str.substr(second, first - second));
                        var_pos.push_back(pair<int, int>(second, first - (data_str[first] == '/') - second + 1));
                        name_value.first.erase(remove_if(name_value.first.begin(), name_value.first.end(),
                                                         [](char c) { return isspace(c); }),
                                               name_value.first.end());
                        parsed_data[name].push_back(name_value);
                        names.insert(name_value.first);
                        if (data_str[first] == '/') {
                            fl = 1;
                        }
                        first++;
                        second = first;
                        current_count++;
                    } else {
                        SgStatement *tmp = scope;
                        int var_size = 0;
                        for (; tmp && wh(tmp) == scope; tmp = tmp->lexNext()) {
                            if (tmp->variant() == VAR_DECL) {
                                int pos = parsed_data[name][current_num[name]].first.find('('), ar_fl = 0;
                                if (pos == string::npos) {
                                    pos = parsed_data[name][current_num[name]].first.length();
                                    ar_fl = 1;
                                }
                                SgDeclarationStatement *cur_decl = static_cast<SgDeclarationStatement *>(tmp);
                                for (int i = 0; i < cur_decl->numberOfVars(); ++i) {
                                    if (cur_decl->var(i)->symbol() &&
                                        cur_decl->var(i)->symbol()->identifier() ==
                                            parsed_data[name][current_num[name]].first.substr(0, pos) &&
                                        cur_decl->var(i)->variant() == ARRAY_REF) {
                                        if (stat_to_dyn_ars[name].find(cur_decl->var(i)->symbol()->identifier()) !=
                                            stat_to_dyn_ars[name].end()) {
                                            to_del_vars.insert(current_num[name]);
                                        }
                                        SgType *type = cur_decl->var(i)->symbol()->type();
                                        SgArrayType *ar_type = isSgArrayType(type);
                                        if (ar_fl) {
                                            var_size = 1;
                                            for (int j = 0; j < ar_type->dimension(); j++) {
                                                var_size *=
                                                    calculate_size(ar_type->sizeInDim(j), cur_modules, modules_names);
                                            }
                                        }
                                        break;
                                    }
                                }
                            }
                            if (tmp->expr(0) && tmp->expr(0)->variant() == COMM_LIST) {
                                SgNestedVarListDeclStmt *current_list = static_cast<SgNestedVarListDeclStmt *>(tmp);
                                SgExprListExp *exp_list = static_cast<SgExprListExp *>(current_list->list(0));
                                for (int j = 0; j < exp_list->length(); j++) {
                                    int pos = parsed_data[name][current_num[name]].first.find('(');
                                    if (pos == string::npos) {
                                        pos = parsed_data[name][current_num[name]].first.length();
                                    }
                                    if (string(exp_list->elem(j)->symbol()->identifier()) ==
                                        string(parsed_data[name][current_num[name]].first.substr(0, pos))) {
                                        to_del_vars.insert(current_num[name]);
                                        if (exp_list->elem(j)->symbol()->type()->variant() == T_ARRAY) {
                                            SgVarDeclStmt *cur_decl = static_cast<SgVarDeclStmt *>(
                                                exp_list->elem(j)->symbol()->declaredInStmt());
                                            SgExprListExp *exp_list = static_cast<SgExprListExp *>(cur_decl->expr(0));
                                            for (int k = 0; k < exp_list->length(); ++k) {
                                                if (cur_decl->var(k)->symbol() &&
                                                    cur_decl->var(k)->symbol()->identifier() ==
                                                        parsed_data[name][current_num[name]].first.substr(0, pos) &&
                                                    cur_decl->var(k)->variant() == ARRAY_REF) {
                                                    auto type = cur_decl->var(k)->symbol()->type();
                                                    auto ar_type = isSgArrayType(type);
                                                    var_size =
                                                        1; //добавить обработку того случая, когда размер невычислим!
                                                    for (int j = 0; j < ar_type->dimension(); j++) {
                                                        var_size *= calculate_size(ar_type->sizeInDim(j), cur_modules,
                                                                                   modules_names);
                                                    }
                                                }
                                            }
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        int is_array = var_size;
                        if (!var_size) {
                            var_size = 1;
                        }
                        if (isnanf(var_size)) {
                            for (int l = parsed_data[name].size(); l >= 0; --l) {
                                if (names.find(parsed_data[name][l].first) != names.end()) {
                                    parsed_data[name].erase(parsed_data[name].begin() + l);
                                }
                            }
                            is_bad = 1;
                        }
                        int cur_count = 0, inner_fl = 0;
                        while (cur_count < var_size) {
                            first++;
                            if (data_str[first] == '\'') {
                                inner_fl = ~inner_fl;
                            }
                            if (data_str[first] == ',' && !inner_fl) {
                                cur_count++;
                            }
                            if (data_str[first] == '/' && !inner_fl) {
                                fl = 0;
                                break;
                            }
                        }
                        string str = data_str.substr(second, first - second);
                        if (!is_array) {
                            parsed_data[name][current_num[name]++].second = str;
                        } else {
                            parsed_data[name][current_num[name]++].second =
                                "reshape((/" + str + "/), shape(" + parsed_data[name][current_num[name]].first + "))";
                        }
                        val_pos.push_back(pair<int, int>(second, first - (data_str[first] == '/') - second + 1));
                        first++;
                        first += !fl;
                        second = first;
                    }
                }
                if (is_bad) {
                    cur_st = tmp;
                    continue;
                }
                //здесь удаляем переменные из множества to_del используя позиции var_pos и val_pos
                vector<pair<int, int>> final_del;
                for (int j = parsed_data[name].size(); j >= (int)parsed_data[name].size() - current_count; j--) {
                    if (to_del_vars.find(j) != to_del_vars.end()) {
                        final_del.push_back(val_pos[j - ((int)parsed_data[name].size() - current_count)]);
                        final_del.push_back(var_pos[j - ((int)parsed_data[name].size() - current_count)]);
                    }
                }
                sort(final_del.begin(), final_del.end(), sortinrev);
                for (int j = 0; j < final_del.size(); ++j) {
                    data_str.erase(final_del[j].first, final_del[j].second);
                }
                int inner_fl = 0;
                for (int j = data_str.length() - 2; j >= 0; --j) {
                    if (data_str[j] == '\'') {
                        inner_fl = ~inner_fl;
                    }
                    if (data_str[j] == ',' && data_str[j + 1] == '/' && !inner_fl) {
                        data_str.erase(j, 1);
                    } else if (data_str[j] == '/' && data_str[j + 1] == '/' && !inner_fl) {
                        if (j + 2 < data_str.length() && data_str[j + 2] == ',' && !inner_fl) {
                            data_str.erase(j + 2, 1);
                        }
                        data_str.erase(j, 2);
                    }
                }
                if (data_str[data_str.length() - 1] == ',')
                    data_str.erase(data_str.length() - 1, 1);
                if (!data_str.length()) {
                    my_delete(cur_st);
                }
                string *tmp = new string(("data " + data_str).c_str());
                NODE_STR(cur_st->expr(0)->thellnd) = (char *)tmp->c_str();
            }
            cur_st = tmp;
        }
    }
}

double find_val(SgExpression *exp, set<SgStatement *> &current_modules,
                map<string, map<string, string>> &modules_names) { //добавить обработку модулей, как в суперфун
    for (auto iter = current_modules.begin(); iter != current_modules.end(); ++iter) {
        SgStatement *now = *iter, *start = now;
        char *name = now->symbol()->identifier();
        if ((modules_names[name].find(";all;") == modules_names[name].end() &&
             modules_names[name].find(string(exp->symbol()->identifier())) != modules_names[name].end())) {
            continue;
        }
        string new_name = modules_names[name].find(string(";all;")) == modules_names[name].end()
                              ? modules_names[name][string(exp->symbol()->identifier())]
                              : string(exp->symbol()->identifier());
        do {
            if (now->expr(2) && now->expr(2)->variant() == EXPR_LIST) {
                SgExprListExp *exp_list = static_cast<SgExprListExp *>(now->expr(2));
                int len = exp_list->length();
                int is_private = 0;
                for (int y = 0; y < len; ++y) {
                    if (exp_list->elem(y)->variant() == PRIVATE_OP) {
                        is_private = 1;
                        break;
                    }
                }
                if (is_private) {
                    now = now->lexNext();

                    continue;
                }
            }
            bool fl = if_parameter(now);
            if (now->variant() == VAR_DECL) {
                SgVarDeclStmt *tmp = static_cast<SgVarDeclStmt *>(now);
                for (int i = 0; i < tmp->numberOfSymbols(); ++i) {
                    if (tmp->symbol(i)->identifier() == new_name) {
                        if (start->variant() != MODULE_STMT) {
                            if (tmp->initialValue(i) == nullptr) {
                                continue;
                            }
                            return calculate_size(tmp->initialValue(i), modules, modules_names);
                        } else {
                            set<SgStatement *> new_modules;
                            new_modules.insert(start);
                            map<string, map<string, string>> new_names;
                            new_names[name][string(";all;")] = string(";all;");
                            if (tmp->initialValue(i) == nullptr) {
                                continue;
                            }
                            return calculate_size(tmp->initialValue(i), new_modules, new_names);
                        }
                    }
                }
            } else if (now->variant() == PARAM_DECL) {
                SgParameterStmt *tmp = static_cast<SgParameterStmt *>(now);
                for (int i = 0; i < tmp->numberOfConstants(); ++i) {
                    if (tmp->constant(i)->identifier() == new_name) {
                        if (fl) {
                            now->setVariant(VAR_DECL);
                        }
                        if (start->variant() != MODULE_STMT) {
                            return calculate_size(tmp->value(i), modules, modules_names);
                        } else {
                            set<SgStatement *> new_modules;
                            new_modules.insert(start);
                            map<string, map<string, string>> new_names;
                            new_names[name][string(";all;")] = string(";all;");
                            if (tmp->value(i) == nullptr) {
                                now = now->lexNext();
                                continue;
                            }
                            return calculate_size(tmp->value(i), new_modules, new_names);
                        }
                    }
                }
            }
            if (fl) {
                now->setVariant(VAR_DECL);
            }
            now = now->lexNext();
        } while (now != last_declarations[start]->lexNext());
    }
    return _NAN;
}

double calculate_size(SgExpression *exp, set<SgStatement *> &current_modules,
                      map<string, map<string, string>> &modules_names) {
    switch (exp->variant()) {
    case INT_VAL:
    case CHAR_VAL:
        return static_cast<SgValueExp *>(exp)->intValue();
    case FLOAT_VAL:
    case DOUBLE_VAL:
        return atof(static_cast<SgValueExp *>(exp)->doubleValue());
    case ADD_OP:
        return calculate_size(exp->operand(1), current_modules, modules_names) +
               calculate_size(exp->operand(2), current_modules, modules_names);
    case SUB_OP:
        return calculate_size(exp->operand(1), current_modules, modules_names) -
               calculate_size(exp->operand(2), current_modules, modules_names);
    case DIV_OP:
        return calculate_size(exp->operand(1), current_modules, modules_names) /
               calculate_size(exp->operand(2), current_modules, modules_names);
    case MULT_OP:
        return calculate_size(exp->operand(1), current_modules, modules_names) *
               calculate_size(exp->operand(2), current_modules, modules_names);
    case VAR_REF:
    case CONST_REF:
        return find_val(exp, current_modules, modules_names);
    case DDOT:
        return calculate_size(exp->operand(2), current_modules, modules_names) -
               calculate_size(exp->operand(1), current_modules, modules_names) + 1;
    default:
        return _NAN;
    }
}

void exctract_structure(map<string, list<string>> &include_stacks, string &file_name) {
    ifstream stream(file_name.c_str());
    string del("delimiter");
    string cur_str, pattern("include'");
    int is_in = 0;
    while (getline(stream, cur_str, '\n')) {
        if (cur_str.length() >= 2 && cur_str[0] == 'c' && (cur_str[1] == '-' || isspace(cur_str[1]))) {
            continue;
        }
        cur_str.erase(remove_if(cur_str.begin(), cur_str.end(), [](char c) { return isspace(c); }), cur_str.end());
        if (cur_str.length() == 0 || cur_str[0] == '!') {
            continue;
        }
        if (cur_str.compare(0, pattern.size(), pattern) == 0) {
            auto name = cur_str.substr(pattern.size(), cur_str.size() - 1 - pattern.size());
            if (remain_files.find(name) != remain_files.end()) {
                include_stacks[file_name].push_back(name);
                is_in = 1;
            }
        } else if (is_in == 1) {
            include_stacks[file_name].push_back(del);
            is_in = 0;
        }
    }
}

void check_common(SgStatement *st, set<SgStatement *> &cur_modules, map<string, map<string, string>> &modules_names,
                  string &cur_fun) {
    string com_name = string(st->expr(0)->symbol()->identifier());
    if (modules_functions.find(com_name) == modules_functions.end()) {
        modules_functions[com_name].first = cur_fun;
        SgNestedVarListDeclStmt *decl_st = static_cast<SgNestedVarListDeclStmt *>(st);
        SgExprListExp *cur_list = static_cast<SgExprListExp *>(decl_st->list(0));
        for (int j = 0; j < cur_list->length(); ++j) { //для проверки, все ли типы совпадают
            modules_functions[com_name].second += ' ' + to_string(cur_list->elem(j)->symbol()->type()->variant());
            if (cur_list->elem(j)->symbol()->type()->variant() == T_ARRAY) {
                SgArrayType *ar_type = isSgArrayType(cur_list->elem(j)->symbol()->type());
                int var_size = 1;
                for (int j = 0; j < ar_type->dimension(); j++) {
                    var_size *= calculate_size(ar_type->sizeInDim(j), cur_modules, modules_names);
                }
                modules_functions[com_name].second += ' ' + to_string(var_size);
            }
        }
    } else {
        string cur_str;
        SgNestedVarListDeclStmt *decl_st = static_cast<SgNestedVarListDeclStmt *>(st);
        SgExprListExp *cur_list = static_cast<SgExprListExp *>(decl_st->list(0));
        int is_bad = 0;
        for (int j = 0; !is_bad && j < cur_list->length(); ++j) {
            cur_str += ' ' + to_string(cur_list->elem(j)->symbol()->type()->variant());
            if (cur_list->elem(j)->symbol()->type()->variant() == T_ARRAY) {
                SgArrayType *ar_type = isSgArrayType(cur_list->elem(j)->symbol()->type());
                int var_size = 1;
                for (int j = 0; j < ar_type->dimension(); j++) {
                    var_size *= calculate_size(ar_type->sizeInDim(j), cur_modules, modules_names);
                }
                cur_str += ' ' + to_string(var_size);
            }
            SgStatement *dec_st = cur_list->elem(j)->symbol()->declaredInStmt();
            SgExprListExp *dec_lst = static_cast<SgExprListExp *>(dec_st->expr(0));
            for (int k = 0; k < dec_lst->length(); ++k) {
                if (dec_lst->elem(k)->variant() == ASSGN_OP &&
                    string(dec_lst->elem(k)->operand(1)->symbol()->identifier()) ==
                        cur_list->elem(j)->symbol()->identifier()) {
                    dont_touch_modules_functions[com_name] = "ouch!";
                    is_bad = 1;
                    break;
                }
            }
        }
        if (modules_functions[com_name].second != cur_str && !is_bad) {
            dont_touch_modules_functions[com_name] = cur_str;
        }
    }
}

void parse_data_blocks(SgProject &proj) {
    for (int i = 0; i < proj.numberOfFiles(); ++i) {
        for (SgStatement *cur_st = proj.file(i).firstStatement(); cur_st; cur_st = cur_st->lexNext()) {
            SgStatement *last_st = cur_st->lastNodeOfStmt();
            if (cur_st->variant() != BLOCK_DATA) {
                cur_st = last_st;
            } else {
                SgStatement *st = cur_st;
                map<string, map<string, string>> modules_names;
                set<SgStatement *> cur_modules;
                string cur_name = create_hash(cur_st);
                init_module_stat(cur_st, cur_modules, modules_names);
                while (last_st != cur_st) {
                    if (st->expr(0) && st->expr(0)->variant() == COMM_LIST) {
                        check_common(st, cur_modules, modules_names, cur_name);
                    }
                    st = st->lexNext();
                }
                cur_st = last_st;
            }
        }
    }
}

void parse_modules(string cur_fun, SgProject &proj, set<string> &visited) {
    for (int i = 0; i < proj.numberOfFiles(); ++i) {
        for (int j = 0; j < proj.file(i).numberOfFunctions(); ++j) {
            SgStatement *now = proj.file(i).functions(j);
            if (create_hash(now) == cur_fun) {
                map<string, map<string, string>> modules_names;
                set<SgStatement *> cur_modules;
                init_module_stat(now, cur_modules, modules_names);
                for (SgStatement *st = proj.file(i).functions(j); st && wh(st) == now; st = st->lexNext()) {
                    if (st->expr(0) && st->expr(0)->variant() == COMM_LIST) {
                        check_common(st, cur_modules, modules_names, cur_fun);
                    }
                }
            }
        }
    }
    for (string it : funs_calls[cur_fun]) {
        if (visited.find(it) == visited.end()) {
            visited.insert(it);
            parse_modules(it, proj, visited);
        }
    }
}

void insert_include(set<string> &found_files, set<string> &visited_files, SgStatement *st, SgStatement *last_st,
                    SgStatement *st_beg, string current_file, map<string, list<string>> &include_structure,
                    string prev_true_name) {
    if (prev_true_name == current_file) {
        found_files.clear();
        found_files.insert(current_file);
        SgStatement *cur_st = st, *prev_st = st->lexPrev(), *tmp = nullptr;
        while (cur_st && cur_st != last_st && get_real_file_name(cur_st) != current_file) {
            if ((get_real_file_name(cur_st) != get_real_file_name(prev_st)) &&
                found_files.find(get_real_file_name(cur_st)) == found_files.end()) {
                found_files.insert(get_real_file_name(cur_st));
            }
            tmp = cur_st->lexNext();
            if (cur_st->lastNodeOfStmt() && cur_st->lastNodeOfStmt() != cur_st) {
                tmp = cur_st->lastNodeOfStmt()->lexNext();
            }
            prev_st = cur_st;
            cur_st = tmp;
        }
    }
    string cur_true_name = string(get_real_file_name(st));
    for (auto fil = found_files.begin(); fil != found_files.end(); fil++) {
        if (*include_structure[*fil].begin() == cur_true_name) {
            if (*fil == current_file) {
                SgStatement *tmp1 =
                    new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
                string *new_inc = new string("include '" + ("1" + cur_true_name) + "'");
                NODE_STR(tmp1->expr(0)->thellnd) = (char *)new_inc->c_str();
                set_file_name(tmp1, *fil);
                st->insertStmtBefore(*tmp1, *st_beg);
            } else if (visited_files.find(*fil) == visited_files.end()) {
                auto out = std::ofstream(("1" + *fil).c_str(), std::ios_base::app);
                SgStatement copy = st->copy();
                copy.setComments("");
                string cur_st_string = string(copy.unparse()), spaces("");
                for (auto it = cur_st_string.begin(); it != cur_st_string.end() && isspace(*it); ++it) {
                    spaces += ' ';
                }
                out << (spaces + "include '") << ("1" + cur_true_name) << "'" << endl;
                out.close();
            }
            include_structure[*fil].pop_front();
            break;
        }
    }
    if (include_structure[cur_true_name].size() && *include_structure[cur_true_name].begin() == "delimiter") {
        include_structure[cur_true_name].pop_front();
    }
}

void unparse_fun(SgStatement *st_beg, SgStatement *last_st, string &current_file, string &first_file,
                 set<string> &visited_files, map<string, list<string>> &include_structure,
                 map<string, list<string>> &full_include_structure) { //доделать!!!
    std::ofstream out;
    set<string> pre_visited_files;
    SgStatement *st = st_beg->lexNext();
    for (int i = 0; i < 2; ++i) {
        if (st_beg->childList2(0) && (st_beg->childList2(0)->variant() == ELSEIF_NODE)) {
            break;
        }
        if (st_beg->numberOfChildrenList2() && st_beg->numberOfChildrenList1()) {
            if (i == 0) {
                last_st = st_beg->childList1(st_beg->numberOfChildrenList1() - 1)->lexNext();
                st = st_beg->childList1(0);
            } else {
                if (st_beg->numberOfChildrenList2() && !(st_beg->childList2(0)->variant() == ELSEIF_NODE)) {
                    last_st = st_beg->childList2(st_beg->numberOfChildrenList2() - 1)->lexNext();
                    st = st_beg->childList2(0);
                } else {
                    continue;
                }
            }
        } else if (i) {
            break;
        }
        set<string> well_done_files;
        set<string> found_files;
        if (get_real_file_name(st) != current_file) {
            insert_include(found_files, visited_files, st, last_st, st_beg, current_file, include_structure,
                           current_file);
        }
        while (st && st != last_st) {
            SgStatement *beg_st = st, *tmp = st;
            string prev_name(("1" + get_real_file_name(st)).c_str()), prev_true_name(get_real_file_name(st));
            out = std::ofstream(("1" + get_real_file_name(st)).c_str(), std::ios_base::app);
            while (st && st != last_st && get_real_file_name(st) == get_real_file_name(beg_st)) {
                tmp = st->lexNext();
                if (st->lastNodeOfStmt() && st->lastNodeOfStmt() != st &&
                    visited_files.find(get_real_file_name(st)) == visited_files.end()) {
                    SgStatement *last_st = nullptr;
                    tmp = st->lastNodeOfStmt()->lexNext();
                    if (st->variant() == INTERFACE_STMT) { //из-за специфики интерфейсов
                        last_st = st->lastNodeOfStmt()->lastNodeOfStmt()->lexNext();
                        tmp = st->lastNodeOfStmt()->lastNodeOfStmt()->lexNext();
                    } else {
                        last_st = st->lastNodeOfStmt();
                    }
                    string new_cur_file = get_real_file_name(st);
                    auto pr = full_include_structure;
                    pr[current_file] = include_structure[current_file];
                    if (include_structure[new_cur_file].size() &&
                        *include_structure[new_cur_file].begin() == "delimiter") {
                        include_structure[new_cur_file].pop_front();
                    }
                    unparse_fun(st, last_st, new_cur_file, current_file, visited_files, pr, full_include_structure);
                    include_structure[current_file] = pr[current_file];
                } else if (get_real_file_name(st) != current_file) {
                    if (visited_files.find(get_real_file_name(st)) == visited_files.end()) {
                        out << st->unparse();
                    }
                    st->deleteStmt();
                }
                st = tmp;
            }
            out.close();
            if (!st || st == last_st) {
                continue;
            }
            insert_include(found_files, visited_files, st, last_st, st_beg, current_file, include_structure,
                           prev_true_name);
            if (prev_true_name != current_file) {
                pre_visited_files.insert(prev_true_name);
            }
        }
    }
    visited_files.insert(pre_visited_files.begin(), pre_visited_files.end());
    if (first_file != current_file) {
        out = std::ofstream(("1" + current_file).c_str(), std::ios_base::app);
        out << st_beg->unparse();
        st_beg->deleteStmt();
    }
}

set<string> private_blocks;

void find_private_blocks(SgProject &project, map<string, string> &old_new_block_name) {
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->variant() == OMP_THREADPRIVATE_DIR) {
                SgExprListExp *private_list = static_cast<SgExprListExp *>(st->expr(0));
                for (int j = 0; j < private_list->length(); ++j) {
                    string new_name = old_new_block_name[private_list->elem(j)->lhs()->symbol()->identifier()];
                    private_blocks.insert(new_name);
                    private_list->elem(j)->lhs()->setSymbol(
                        *new SgConstructSymb(const_cast<char *>(new_name.c_str()), *wh(st))); //мб исправить
                }
            }
        }
    }
}

bool if_transformable(SgExpression *exp) {
    if (exp == nullptr) {
        return true;
    }
    return ((!exp->symbol() || (exp->symbol()->variant() == CONST_NAME)) && if_transformable(exp->lhs()) &&
            if_transformable(exp->rhs()));
}

SgStatement *dyn_to_stat(SgStatement *st) {
    SgExprListExp *arr_lst = static_cast<SgExprListExp *>(st->expr(0));
    SgStatement *space = wh(st);
    int len = arr_lst->length();
    string space_name = create_hash(space);
    SgExprListExp *spec_list = static_cast<SgExprListExp *>(st->expr(2)), *new_spec_list = nullptr;
    for (int i = 0; i < spec_list->length(); ++i) {
        if (spec_list->elem(i)->variant() != ALLOCATABLE_OP && spec_list->elem(i)->variant() != DIMENSION_OP) {
            if (new_spec_list == nullptr) {
                new_spec_list = new SgExprListExp(*spec_list->elem(i));
            } else {
                new_spec_list->append(*spec_list->elem(i));
            }
        }
    }
    for (int i = 0; i < len; ++i) {
        SgExpression *cur_var = arr_lst->elem(i);
        string cur_name = string(cur_var->symbol()->identifier());
        if (not_transformable_ars_dyn_to_stat[space_name].find(cur_name) !=
                not_transformable_ars_dyn_to_stat[space_name].end() ||
            dyn_to_stat_ars[space_name].find(cur_name) == dyn_to_stat_ars[space_name].end()) {
            if (dyn_to_stat_ars[space_name].find(cur_name) != dyn_to_stat_ars[space_name].end()) {
                cout << "SORRY! CAN'T TRANSFORM ARRAY " << cur_name << "IN FUNCTION " << space_name << endl;
            }
            string st_file_name(get_real_file_name(st)), control_file_name(get_real_file_name(space));
            if (st_file_name != control_file_name) {
                if (susp_ars_from_inc[st_file_name].find(cur_name) != susp_ars_from_inc[st_file_name].end()) {
                    SgExprListExp *new_list = new SgExprListExp(cur_var->copy());
                    SgVarDeclStmt *new_dec = new SgVarDeclStmt(*new_list, st->expr(2)->copy(), *st->expr(1)->type());
                    update_last_declaration(space, new_dec, 1);
                    set_file_name(new_dec, control_file_name);
                }
            }
            continue;
        }
        SgVarDeclStmt *new_decl_st =
            new SgVarDeclStmt(*new SgExprListExp(*new SgArrayRefExp(*cur_var->symbol(),
                                                                    *transformable_array_sizes[space_name][cur_name])),
                              *st->expr(1)->type());
        if (new_spec_list) {
            new_decl_st->setExpression(2, *new_spec_list);
        }
        update_last_declaration(space, new_decl_st, 1);
        string file_name(space->fileName());
        set_file_name(new_decl_st, file_name);
        SgFunctionSymb *fun_sym = isSgFunctionSymb(space->symbol());
        if (fun_sym && space->variant() != PROG_HEDR) {
            int par_ind = -1;
            for (int k = 0; k < fun_sym->numberOfParameters(); ++k) {
                if (fun_sym->parameter(k)->identifier() == cur_name) {
                    par_ind = k;
                    break;
                }
            }
            if (par_ind != -1) {
                for (auto inter_ref : funs_interfaces[space_name]) {
                    SgFunctionSymb *fun_sym = isSgFunctionSymb(inter_ref->symbol());
                    string arr_name = string(fun_sym->parameter(par_ind)->identifier());
                    int not_found = 1;
                    for (SgStatement *cur_st = inter_ref; not_found && cur_st != inter_ref->lastNodeOfStmt();
                         cur_st = cur_st->lexNext()) {
                        if (cur_st->variant() == VAR_DECL) {
                            SgDeclarationStatement *dec_st = static_cast<SgDeclarationStatement *>(cur_st);
                            int list_len = dec_st->numberOfVars();
                            for (int m = 0; m < list_len; ++m) {
                                if (dec_st->var(m)->symbol()->identifier() == arr_name) {
                                    dec_st->deleteVar(m ? m + 1 : m);
                                    SgVarDeclStmt *new_decl_st =
                                        new SgVarDeclStmt(*new SgExprListExp(*new SgArrayRefExp(
                                                              cur_var->symbol()->copy(),
                                                              *transformable_array_sizes[space_name][cur_name])),
                                                          *st->expr(1)->type());
                                    update_last_declaration(inter_ref, new_decl_st, 1);
                                    string inter_file_name(inter_ref->fileName());
                                    set_file_name(new_decl_st, inter_file_name);
                                    if (dec_st->numberOfVars() == 0) {
                                        my_delete(dec_st);
                                    }
                                    not_found = 0;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    SgStatement *ans = st->lexNext();
    SgExprListExp *new_list = nullptr;
    for (int i = 0; i < len; ++i) {
        SgExpression *cur_var = arr_lst->elem(i);
        string cur_name = string(cur_var->symbol()->identifier());
        if (not_transformable_ars_dyn_to_stat[space_name].find(cur_name) !=
                not_transformable_ars_dyn_to_stat[space_name].end() ||
            arr_lst->elem(i)->symbol()->type()->variant() != T_ARRAY ||
            dyn_to_stat_ars[space_name].find(cur_name) == dyn_to_stat_ars[space_name].end()) {
            if (new_list == nullptr) {
                new_list = new SgExprListExp(*arr_lst->elem(i));
            } else {
                new_list->append(*arr_lst->elem(i));
            }
        }
    }
    if (new_list == nullptr) {
        my_delete(st);
    } else {
        st->setExpression(0, *new_list);
    }
    return ans;
}

void clear_space(SgProject &proj) {
    for (int i = 0; i < proj.numberOfFiles(); ++i) {
        for (SgStatement *cur_st = proj.file(i).firstStatement(); cur_st;) {
            SgStatement *tmp = cur_st->lexNext();
            if (cur_st->variant() == ALLOCATE_STMT || cur_st->variant() == DEALLOCATE_STMT) {
                SgExprListExp *allocate_lst = static_cast<SgExprListExp *>(cur_st->expr(0));
                int al_len = allocate_lst->length();
                SgStatement *space = cur_st;
                while (space) {
                    space = space->controlParent();
                    if (space &&
                        (space->variant() == MODULE_STMT || (space->symbol() && isSgFunctionSymb(space->symbol())))) {
                        SgExprListExp *new_list = nullptr;
                        string space_name = create_hash(space);
                        for (int j = 0; j < al_len; ++j) {
                            if (not_transformable_ars_dyn_to_stat[space_name].find(
                                    allocate_lst->elem(j)->symbol()->identifier()) !=
                                    not_transformable_ars_dyn_to_stat[space_name].end() ||
                                dyn_to_stat_ars[space_name].find(allocate_lst->elem(j)->symbol()->identifier()) ==
                                    dyn_to_stat_ars[space_name].end()) {
                                if (new_list == nullptr) {
                                    new_list = new SgExprListExp(*allocate_lst->elem(j));
                                } else {
                                    new_list->append(*allocate_lst->elem(j));
                                }
                            }
                        }
                        if (new_list == nullptr) {
                            if (cur_st->lexNext()->variant() == LOGIF_NODE &&
                                static_cast<SgHeapStmt *>(cur_st)->statVariable() && cur_st->lexNext()->expr(0) &&
                                (cur_st->lexNext()->expr(0)->variant() == EQ_OP ||
                                 cur_st->lexNext()->expr(0)->variant() == NE_OP ||
                                 cur_st->lexNext()->expr(0)->variant() == NOTEQL_OP) &&
                                ((cur_st->lexNext()->expr(0)->operand(1)->symbol() &&
                                  string(cur_st->lexNext()->expr(0)->operand(1)->symbol()->identifier()) ==
                                      static_cast<SgHeapStmt *>(cur_st)->statVariable()->symbol()->identifier()))) {
                                cur_st->lexNext()->deleteStmt();
                                tmp = cur_st->lexNext();
                            }
                            SgStatement *cur_space = wh(cur_st);
                            cur_st->deleteStmt();
                            int fl = (cur_space->lastNodeOfStmt() == cur_space->lexNext());
                            if (fl && cur_space->variant() == PROC_HEDR) {
                                tmp = cur_space->lastNodeOfStmt()->lexNext();
                                string proc_name = create_hash(cur_space);
                                for (long unsigned int j = 0; j < procedure_calls[proc_name].size(); ++j) {
                                    procedure_calls[proc_name][j]->deleteStmt();
                                }
                                SgStatement *module_stmt = cur_space;
                                while (module_stmt->controlParent() && module_stmt->variant() != MODULE_STMT) {
                                    module_stmt = module_stmt->controlParent();
                                }
                                if (module_stmt->variant() == MODULE_STMT) {
                                    string mod_name = create_hash(module_stmt);
                                    for (auto use_st : uses) {
                                        if (use_st->symbol()->identifier() == mod_name) {
                                            SgExprListExp *only_lst = static_cast<SgExprListExp *>(
                                                static_cast<SgUseOnlyExp *>(use_st->expr(0))->onlyList());
                                            SgExprListExp *new_lst = nullptr;
                                            for (int m = only_lst->length() - 1; m >= 0; m--) {
                                                if (only_lst->elem(m)->variant() == RENAME_NODE) {
                                                    if (only_lst->elem(m)->operand(1)->symbol()->identifier() !=
                                                        proc_name.substr(0, proc_name.find(' ', 0))) {
                                                        if (new_lst == nullptr) {
                                                            new_lst = new SgExprListExp(only_lst->elem(m)->copy());
                                                        } else {
                                                            new_lst->append(only_lst->elem(m)->copy());
                                                        }
                                                    }
                                                } else if (only_lst->elem(m)->symbol()->identifier() != proc_name) {
                                                    if (new_list == nullptr) {
                                                        new_list = new SgExprListExp(only_lst->elem(m)->copy());
                                                    } else {
                                                        new_list->append(only_lst->elem(m)->copy());
                                                    }
                                                }
                                            }
                                            if (new_lst != nullptr) {
                                                use_st->setExpression(0, *new SgUseOnlyExp(*new_lst));
                                            } else {
                                                use_st->setExpression(0, nullptr);
                                                uses.erase(use_st);
                                            }
                                        }
                                    }
                                }
                                cur_space->deleteStmt();
                            }
                            break;
                        } else {
                            cur_st->setExpression(0, *new_list);
                        }
                    }
                }
            }
            cur_st = tmp;
        }
    }
}

string find_name(SgFunctionSymb *&cur_sym, string &cur_fun, SgProject &proj, SgStatement *&res, int &is_found) {
    for (auto mod : modules) {
        string mod_name = create_hash(mod);
        map<string, string>::iterator it;
        if ((it = find_if(mods_uses[mod_name][cur_fun].begin(), mods_uses[mod_name][cur_fun].end(),
                          [cur_sym](const auto &mo) { return mo.second == string(cur_sym->identifier()); })) !=
            mods_uses[mod_name][cur_fun].end()) {
            for (SgStatement *st = mod; st != mod->lastNodeOfStmt(); st = st->lexNext()) {
                if (st->variant() == PROC_HEDR && string(st->symbol()->identifier()) == it->first) {
                    res = st;
                    break;
                }
            }
        }
    }
    string cur_name = res ? create_hash(res) : string(cur_sym->identifier());
    if (!res) {
        int is_done = 0;
        for (int i = 0; !is_done && i < proj.numberOfFiles(); ++i) {
            for (int j = 0; j < proj.file(i).numberOfFunctions(); ++j) {
                SgStatement *now = proj.file(i).functions(j);
                if (now->symbol()->identifier() == string(cur_sym->identifier()) &&
                    now->controlParent()->variant() == GLOBAL) {
                    res = now;
                    cur_sym = isSgFunctionSymb(now->symbol());
                    is_done = 1;
                    break;
                }
            }
        }
    } else {
        cur_sym = isSgFunctionSymb(res->symbol());
    }
    is_found = 0;
    return cur_name;
}

bool find_array_dim(SgSymbol *cur_sym, string &cur_fun, string &his_fun, int arg_num, SgProject &proj,
                    SgFunctionSymb *fun_sym) {
    SgStatement *fun_space = fun_sym->body(), *las_fun_dec = last_declarations[fun_space]->lexNext();
    SgSymbol *cur_arg = fun_sym->parameter(arg_num);
    int true_dims = 0, is_found = 0;
    for (SgStatement *st = fun_space; !is_found && st != las_fun_dec; st = st->lexNext()) {
        if (st->variant() == VAR_DECL) {
            SgVarDeclStmt *dec_st = static_cast<SgVarDeclStmt *>(st);
            int sym_num = dec_st->numberOfSymbols();
            for (int k = 0; k < sym_num; ++k) {
                SgSymbol *var =
                    dec_st->var(k)->symbol() ? dec_st->var(k)->symbol() : dec_st->var(k)->operand(1)->symbol();
                if (string(var->identifier()) == string(cur_arg->identifier())) {
                    true_dims = static_cast<SgArrayType *>(dec_st->var(k)->symbol()->type())->dimension();
                    is_found = 1;
                    break;
                }
            }
        }
    }
    for (SgStatement *mod : modules) {
        string mod_name = create_hash(mod);
        map<string, string>::iterator it;
        if ((it = find_if(mods_uses[mod_name][cur_fun].begin(), mods_uses[mod_name][cur_fun].end(),
                          [cur_sym](const auto &mo) { return mo.second == string(cur_sym->identifier()); })) !=
            mods_uses[mod_name][cur_fun].end()) {
            SgStatement *fin_st = last_declarations[mod]->lexNext();
            for (SgStatement *st = mod; st != fin_st; st = st->lexNext()) {
                if (st->variant() == VAR_DECL) {
                    SgVarDeclStmt *dec_st = static_cast<SgVarDeclStmt *>(st);
                    int sym_num = dec_st->numberOfVars();
                    for (int k = 0; k < sym_num; ++k) {
                        SgSymbol *var =
                            dec_st->var(k)->symbol() ? dec_st->var(k)->symbol() : dec_st->var(k)->operand(1)->symbol();
                        if (string(var->identifier()) == it->first) {
                            int his_dims = static_cast<SgArrayType *>(dec_st->var(k)->symbol()->type())->dimension();
                            if ((dynamic_array_shapes[cur_fun][his_fun].find(arg_num) !=
                                     dynamic_array_shapes[cur_fun][his_fun].end() &&
                                 dynamic_array_shapes[cur_fun][his_fun][arg_num] != his_dims) ||
                                his_dims != true_dims) {
                                return false;
                            } else {
                                dynamic_array_shapes[cur_fun][his_fun][arg_num] = his_dims;
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    SgStatement *cur_space = cur_sym->scope();
    SgStatement *fin_st = last_declarations[cur_space]->lexNext();
    for (SgStatement *st = cur_space; st != fin_st; st = st->lexNext()) {
        if (st->variant() == VAR_DECL) {
            SgVarDeclStmt *dec_st = static_cast<SgVarDeclStmt *>(st);
            int sym_num = dec_st->numberOfSymbols();
            for (int k = 0; k < sym_num; ++k) {
                SgSymbol *var =
                    dec_st->var(k)->symbol() ? dec_st->var(k)->symbol() : dec_st->var(k)->operand(1)->symbol();
                if (string(var->identifier()) == string(cur_sym->identifier())) {
                    int his_dims = static_cast<SgArrayType *>(dec_st->var(k)->symbol()->type())->dimension();
                    if ((dynamic_array_shapes[cur_fun][his_fun].find(arg_num) !=
                             dynamic_array_shapes[cur_fun][his_fun].end() &&
                         dynamic_array_shapes[cur_fun][his_fun][arg_num] != his_dims) ||
                        his_dims != true_dims) {
                        return false;
                    } else {
                        dynamic_array_shapes[cur_fun][his_fun][arg_num] = his_dims;
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

void parse_exp(string cur_fun, SgExpression *exp, SgProject &proj, int fl) {
    if (!exp) {
        return;
    }
    if (exp->variant() == FUNC_CALL) {
        auto fun_exp = static_cast<SgFunctionCallExp *>(exp);
        auto cur_sym = isSgFunctionSymb(fun_exp->funName());
        SgStatement *res = nullptr;
        int is_found = 1;
        string cur_name = find_name(cur_sym, cur_fun, proj, res, is_found);
        if (fl == 1) {
            for (auto it = not_transformable_fun_params_dyn_to_stat[cur_name].begin();
                 it != not_transformable_fun_params_dyn_to_stat[cur_name].end(); ++it) {
                auto ars_ref = static_cast<SgExprListExp *>(fun_exp->arg(*it)->symbRefs());
                for (int j = 0; j < ars_ref->length(); ++j) {
                    not_transformable_ars_dyn_to_stat[cur_fun].insert(ars_ref->elem(j)->symbol()->identifier());
                }
            }
        }
        if (fl == 2) {
            for (int i = 0; i < cur_sym->numberOfParameters(); ++i) {
                auto ars_ref = static_cast<SgExprListExp *>(fun_exp->arg(i)->symbRefs());
                if (!ars_ref) {
                    continue;
                }
                int ar_fl = (cur_sym->parameter(i)->type()->variant() == T_ARRAY);
                if (!ar_fl) {
                    continue;
                }
                for (int j = 0; j < ars_ref->length(); ++j) {
                    if (stat_to_dyn_ars[cur_name].find(cur_sym->parameter(i)->identifier()) !=
                            stat_to_dyn_ars[cur_name].end() &&
                        not_transformable_ars_stat_to_dyn[cur_name].find(cur_sym->parameter(i)->identifier()) ==
                            not_transformable_ars_stat_to_dyn[cur_name].end()) {
                        if (ars_ref->elem(j)->symbol()->type()->variant() == T_ARRAY) {
                            stat_to_dyn_ars[cur_fun].insert(ars_ref->elem(j)->symbol()->identifier());
                        }
                    }
                }
            }
        }
    }
    parse_exp(cur_fun, exp->lhs(), proj, fl);
    parse_exp(cur_fun, exp->rhs(), proj, fl);
}

void parse_exp_forward(string cur_fun, SgExpression *exp, SgProject &proj, set<string> &susp_ars, int fl) {
    if (!exp) {
        return;
    }
    if (exp->variant() == FUNC_CALL) {
        SgFunctionCallExp *fun_exp = static_cast<SgFunctionCallExp *>(exp);
        SgFunctionSymb *cur_sym = isSgFunctionSymb(exp->symbol());
        SgStatement *res = nullptr;
        int is_found = 1;
        string cur_name = find_name(cur_sym, cur_fun, proj, res, is_found);
        pair<string, SgExprListExp *> my_first_pair(cur_name, static_cast<SgExprListExp *>(fun_exp->args()));
        pair<SgStatement *, pair<string, SgExprListExp *>> my_pair(res, my_first_pair);
        if (!fl) {
            funs_funs_calls[cur_fun].push_back(my_pair);
            if (cur_sym->result()) {
                not_transformable_ars_dyn_to_stat[cur_name].insert(cur_sym->result()->identifier());
                not_transformable_ars_stat_to_dyn[cur_name].insert(cur_sym->result()->identifier());
            }
        }
        for (int i = 0; i < cur_sym->numberOfParameters(); ++i) {
            string cur_param_name(cur_sym->parameter(i)->identifier());
            SgExpression *cur_arg = fun_exp->arg(i);
            SgExprListExp *ars_ref = static_cast<SgExprListExp *>(cur_arg->symbRefs());
            if (!ars_ref) {
                continue;
            }
            if (cur_arg->variant() == ARRAY_REF && (cur_arg->lhs() || !is_found) && fl == 0) {
                if (cur_arg->lhs() != nullptr) {
                    not_transformable_ars_stat_to_dyn[cur_name].insert(cur_param_name);
                    not_transformable_fun_params_stat_to_dyn[cur_name].insert(i);
                }
                if (!is_found && cur_arg->lhs() == nullptr) {
                    not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_arg->symbol()->identifier());
                }
            }
            int ar_fl = (cur_sym->parameter(i)->type()->variant() == T_ARRAY);
            if (!ar_fl) {
                continue;
            }
            for (int j = 0; j < ars_ref->length(); ++j) {
                string cur_sym_name(ars_ref->elem(j)->symbol()->identifier());
                if (fl == 0) {
                    bool if_good = find_array_dim(ars_ref->elem(j)->symbol(), cur_fun, cur_name, i, proj, cur_sym);
                    if (!if_good) {
                        not_transformable_ars_stat_to_dyn[cur_name].insert(cur_param_name);
                    }
                }
                ars_as_params[cur_name].insert(cur_sym_name);
                if (fl == 2 && (dyn_to_stat_ars[cur_fun].find(cur_sym_name) != dyn_to_stat_ars[cur_fun].end() &&
                                not_transformable_ars_dyn_to_stat[cur_fun].find(cur_sym_name) ==
                                    not_transformable_ars_dyn_to_stat[cur_fun].end())) {
                    dyn_to_stat_ars[cur_name].insert(cur_param_name);
                }
                if (transformable_array_sizes[cur_fun][cur_sym_name] == nullptr ||
                    susp_ars.find(cur_sym_name) != susp_ars.end() ||
                    transformable_array_sizes[cur_name][cur_param_name] != nullptr) {
                    if (fl == 1 && (transformable_array_sizes[cur_name][cur_param_name] != nullptr)) {
                        if (transformable_array_sizes[cur_fun][cur_sym_name] == nullptr ||
                            trim(string(transformable_array_sizes[cur_fun][cur_sym_name]->unparse())) !=
                                trim(string(transformable_array_sizes[cur_name][cur_param_name]->unparse()))) {
                            not_transformable_ars_dyn_to_stat[cur_name].insert(cur_param_name);
                            not_transformable_fun_params_dyn_to_stat[cur_name].insert(i);
                        }
                    }
                    if (fl == 1 && transformable_array_sizes[cur_fun][cur_sym_name] == nullptr) {
                        not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_sym_name);
                    }
                } else if (fl == 0) {
                    transformable_array_sizes[cur_name][cur_param_name] =
                        transformable_array_sizes[cur_fun][cur_sym_name];
                }

                if ((fl == 1) && not_transformable_ars_stat_to_dyn[cur_fun].find(cur_sym_name) !=
                                     not_transformable_ars_stat_to_dyn[cur_fun].end()) {
                    not_transformable_ars_stat_to_dyn[cur_name].insert(cur_param_name);
                }
                if (fl == 2 && (dyn_to_stat_ars[cur_fun].find(cur_sym_name) != dyn_to_stat_ars[cur_fun].end())) {
                    not_transformable_ars_stat_to_dyn[cur_name].insert(cur_param_name);
                }
            }
        }
    }
    parse_exp_forward(cur_fun, exp->lhs(), proj, susp_ars, fl);
    parse_exp_forward(cur_fun, exp->rhs(), proj, susp_ars, fl);
}

void fill_use(map<string, set<string>> &map_to_fill, SgStatement *cur_mod, string &fun_name, string &mod_name) {
    for (auto name_it = map_to_fill[fun_name].begin(); name_it != map_to_fill[fun_name].end(); name_it++) {
        SgStatement *fin_st = last_declarations[cur_mod]->lexNext();
        for (SgStatement *cur_st = cur_mod; cur_st != fin_st; cur_st = cur_st->lexNext()) {
            if (cur_st->expr(0) && cur_st->expr(0)->variant() == EXPR_LIST && cur_st->variant() == VAR_DECL) {
                SgVarDeclStmt *decl_st = static_cast<SgVarDeclStmt *>(cur_st);
                for (int k = 0; k < decl_st->numberOfSymbols(); ++k) {
                    if (decl_st->symbol(k)->identifier() == *name_it) {
                        if (cur_st->expr(2)) {
                            int priv_flag = 0;
                            SgExprListExp *spec_list = static_cast<SgExprListExp *>(cur_st->expr(2));
                            for (int m = 0; m < spec_list->length(); ++m) {
                                if (spec_list->elem(m)->variant() == PRIVATE_OP) {
                                    priv_flag = 1;
                                    break;
                                }
                            }
                            if (!priv_flag) {
                                map_to_fill[mod_name].insert(string(*name_it));
                            }
                        } else {
                            map_to_fill[mod_name].insert(string(*name_it));
                        }
                    }
                }
            }
        }
    }
}

void extract_uses(SgProject &proj, int fl) {
    for (int i = 0; i < proj.numberOfFiles(); ++i) {
        for (int j = 0; j < proj.file(i).numberOfFunctions(); ++j) {
            SgStatement *now = proj.file(i).functions(j);
            string fun_name(create_hash(now));
            if (now->controlParent()->variant() != GLOBAL) {
                auto module_st = now;
                while (module_st->controlParent() && module_st->variant() != MODULE_STMT) {
                    module_st = module_st->controlParent();
                }
                if (module_st) {
                    string mod_name(create_hash(module_st));
                    for (auto &name : stat_to_dyn_ars[fun_name]) {
                        int found = 0;
                        SgStatement *fin_st = last_declarations[now]->lexNext();
                        for (SgStatement *cur_st = now; !found && cur_st != fin_st; cur_st = cur_st->lexNext()) {
                            if (cur_st->variant() == VAR_DECL) {
                                SgExprListExp *lst = static_cast<SgExprListExp *>(cur_st->expr(0));
                                for (int m = 0; m < lst->length(); ++m) {
                                    char *cur_n = lst->elem(m)->symbol()
                                                      ? lst->elem(m)->symbol()->identifier()
                                                      : lst->elem(m)->operand(1)->symbol()->identifier();
                                    if (name == cur_n) {
                                        found = 1;
                                        break;
                                    }
                                }
                            }
                        }
                        if (!found) {
                            stat_to_dyn_ars[mod_name].insert(name);
                        }
                    }
                }
            }
            for (SgStatement *cur_st = now; cur_st != now->lastNodeOfStmt(); cur_st = cur_st->lexNext()) {
                if (cur_st->variant() == USE_STMT) {
                    string mod_name(cur_st->symbol()->identifier());
                    if (cur_st->expr(0)) {
                        SgExprListExp *only_lst =
                            static_cast<SgExprListExp *>(static_cast<SgUseOnlyExp *>(cur_st->expr(0))->onlyList());
                        for (int k = 0; k < only_lst->length(); ++k) {
                            SgExpression *cur_exp = only_lst->elem(k);
                            if (cur_exp->variant() == RENAME_NODE && cur_exp->operand(1) && cur_exp->operand(2)) {
                                string first_op_name(cur_exp->operand(1)->symbol()->identifier()),
                                    second_op_name(cur_exp->operand(2)->symbol()->identifier());
                                if (fl == 1) {
                                    if (not_transformable_ars_dyn_to_stat[fun_name].find(second_op_name) !=
                                        not_transformable_ars_dyn_to_stat[fun_name].end()) {
                                        not_transformable_ars_dyn_to_stat[mod_name].insert(first_op_name);
                                    }
                                    if (not_transformable_ars_stat_to_dyn[fun_name].find(second_op_name) !=
                                        not_transformable_ars_stat_to_dyn[fun_name].end()) {
                                        not_transformable_ars_stat_to_dyn[mod_name].insert(first_op_name);
                                    }
                                }
                                if (fl == 2) {
                                    if (dyn_to_stat_ars[fun_name].find(second_op_name) !=
                                        dyn_to_stat_ars[fun_name].end()) {
                                        dyn_to_stat_ars[mod_name].insert(first_op_name);
                                    }
                                    if (stat_to_dyn_ars[fun_name].find(second_op_name) !=
                                        stat_to_dyn_ars[fun_name].end()) {
                                        stat_to_dyn_ars[mod_name].insert(first_op_name);
                                    }
                                }
                            } else {
                                string op_name(cur_exp->symbol()->identifier());
                                if (fl == 1) {
                                    not_transformable_ars_stat_to_dyn[mod_name].insert(op_name);
                                    not_transformable_ars_dyn_to_stat[mod_name].insert(op_name);
                                }
                                if (fl == 2) {
                                    stat_to_dyn_ars[mod_name].insert(op_name);
                                    dyn_to_stat_ars[mod_name].insert(op_name);
                                }
                            }
                        }
                    } else {
                        SgStatement *cur_mod = nullptr;
                        for (auto mod_it = modules.begin(); mod_it != modules.end(); mod_it++) {
                            if ((*mod_it)->symbol()->identifier() == trim(mod_name)) {
                                cur_mod = *mod_it;
                                break;
                            }
                        }
                        if (fl == 2) {
                            fill_use(dyn_to_stat_ars, cur_mod, fun_name, mod_name);
                            fill_use(stat_to_dyn_ars, cur_mod, fun_name, mod_name);
                        }
                        if (fl == 1) {
                            fill_use(not_transformable_ars_dyn_to_stat, cur_mod, fun_name, mod_name);
                            fill_use(not_transformable_ars_stat_to_dyn, cur_mod, fun_name, mod_name);
                        }
                    }
                }
            }
        }
    }
}

void find_transformable_ars(string cur_fun, SgProject &proj, int repeat_fl, set<string> visited_spaces,
                            SgStatement *module = nullptr) {
    visited_spaces.insert(cur_fun);
    set<string> susp_ars;
    int is_done = 0;
    for (int i = 0; !is_done && i < proj.numberOfFiles(); ++i) {
        for (int j = 0; j < proj.file(i).numberOfFunctions(); ++j) {
            SgStatement *now = proj.file(i).functions(j);
            if (module != nullptr) {
                now = module;
            }
            if (create_hash(now) == cur_fun || module != nullptr) {
                if (!repeat_fl) {
                    set<SgStatement *> cur_modules;
                    map<string, map<string, string>> modules_names;
                    init_module_stat(now, cur_modules, modules_names);
                    set<string> visited_ars;
                    for (SgStatement *cur_st = now; cur_st != now->lastNodeOfStmt()->lexNext();
                         cur_st = cur_st->lexNext()) {
                        if (cur_st->variant() == ALLOCATE_STMT) {
                            SgExprListExp *allocate_list = static_cast<SgExprListExp *>(cur_st->expr(0));
                            int al_len = allocate_list->length();
                            for (int j = 0; j < al_len; ++j) {
                                SgExpression *cur_al = allocate_list->elem(j);
                                string cur_name = string(cur_al->symbol()->identifier());
                                if (module != nullptr) {
                                    int found = 0;
                                    for (SgStatement *tmp = now; !found && (tmp != now->lastDeclaration()->lexNext());
                                         tmp = tmp->lexNext()) {
                                        if (tmp->variant() == VAR_DECL) {
                                            SgExprListExp *lst = static_cast<SgExprListExp *>(tmp->expr(0));
                                            for (int k = 0; k < lst->length(); ++k) {
                                                char *name = lst->elem(k)->symbol()
                                                                 ? lst->elem(k)->symbol()->identifier()
                                                                 : lst->elem(k)->operand(1)->symbol()->identifier();
                                                if (name == cur_name) {
                                                    found = 1;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    if (!found) {
                                        continue;
                                    }
                                }
                                if (cur_st->controlParent() != wh(cur_st)) {
                                    not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_name);
                                    continue;
                                }
                                if (visited_ars.find(cur_name) != visited_ars.end()) {
                                    not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_name);
                                } else {
                                    visited_ars.insert(cur_name);
                                    if (now->variant() != PROG_HEDR && module == nullptr) {
                                        SgFunctionSymb *current_fun_symbol =
                                            static_cast<SgFunctionSymb *>(now->symbol());
                                        int i = 0;
                                        for (; i < current_fun_symbol->numberOfParameters(); ++i) {
                                            if (current_fun_symbol->parameter(i)->identifier() == cur_name) {
                                                break;
                                            }
                                        }
                                        if (i != current_fun_symbol->numberOfParameters()) {
                                            not_transformable_fun_params_dyn_to_stat[cur_fun].insert(i);
                                            not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_name);
                                            continue;
                                        }
                                    }
                                    if (module == nullptr) {
                                        for (SgStatement *st : raw_func_calls[cur_fun]) {
                                            int found = 0;
                                            for (int m = 0; m < 3; ++m) {
                                                if (st->expr(m) && st->expr(m)->variant() == EXPR_LIST) {
                                                    SgExprListExp *refs = static_cast<SgExprListExp *>(st->expr(m));
                                                    for (int k = 0; k < refs->length(); ++k) {
                                                        if (refs->elem(k)->symbol() &&
                                                            refs->elem(k)->symbol()->identifier() == cur_name) {
                                                            if (st->lineNumber() < cur_st->lineNumber()) {
                                                                found = 1;
                                                                not_transformable_ars_dyn_to_stat[cur_fun].insert(
                                                                    cur_name);
                                                                break;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            if (found) {
                                                break;
                                            }
                                        }
                                    }
                                    SgArrayRefExp *ar_ref = static_cast<SgArrayRefExp *>(cur_al);
                                    SgExprListExp *new_exp_list = nullptr;
                                    int is_bad = 0;
                                    for (int k = 0; !is_bad && k < ar_ref->numberOfSubscripts(); ++k) {
                                        SgExpression *cur_exp = ar_ref->subscript(k);
                                        SgExprListExp *sym_list = static_cast<SgExprListExp *>(cur_exp->symbRefs());
                                        if (sym_list) {
                                            for (int y = 0; y < sym_list->length(); ++y) {
                                                string cur_ident = string(sym_list->elem(y)->symbol()->identifier());
                                                for (SgStatement *tmp = cur_st; tmp != now; tmp = tmp->lexPrev()) {
                                                    if (tmp->variant() == ASSIGN_STAT) {
                                                        SgAssignStmt *ass_st = static_cast<SgAssignStmt *>(tmp);
                                                        if (ass_st->lhs()->symbol() &&
                                                            string(ass_st->lhs()->symbol()->identifier()) ==
                                                                cur_ident) {
                                                            not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_name);
                                                            is_bad = 1;
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        SgExpression *new_exp = nullptr;
                                        int is_param = 0;
                                        SgFunctionSymb *fun_sym = isSgFunctionSymb(now->symbol());
                                        if (fun_sym && now->variant() != PROG_HEDR) {
                                            int arg_count = fun_sym->numberOfParameters();
                                            for (int t = 0; t < arg_count; ++t) {
                                                if (fun_sym->parameter(t)->identifier() == cur_name) {
                                                    is_param = 1;
                                                    break;
                                                }
                                            }
                                        }
                                        if (cur_exp->variant() == DDOT) {
                                            SgSubscriptExp *sub_exp = static_cast<SgSubscriptExp *>(cur_exp);
                                            double low_bound =
                                                calculate_size(sub_exp->lbound(), cur_modules, modules_names);
                                            double high_bound =
                                                calculate_size(sub_exp->ubound(), cur_modules, modules_names);
                                            if (isnanf(low_bound) || isnanf(high_bound)) {
                                                if (!is_param) {
                                                    transformable_array_sizes[cur_fun][cur_name] = ar_ref->lhs();
                                                    susp_ars.insert(cur_name);
                                                } else {
                                                    not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_name);
                                                }
                                                break;
                                            }
                                            SgValueExp *first_arg = new SgValueExp((int)low_bound);
                                            SgValueExp *second_arg = new SgValueExp((int)high_bound);
                                            new_exp = new SgSubscriptExp(*first_arg, *second_arg);
                                        } else {
                                            double new_val = calculate_size(cur_exp, cur_modules, modules_names);
                                            if (isnanf(new_val)) {
                                                if (!is_param) {
                                                    transformable_array_sizes[cur_fun][cur_name] =
                                                        &ar_ref->lhs()->copy();
                                                    susp_ars.insert(cur_name);
                                                } else {
                                                    not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_name);
                                                }
                                                break;
                                            }
                                            new_exp = new SgValueExp((int)new_val);
                                        }
                                        if (new_exp_list == nullptr) {
                                            new_exp_list = new SgExprListExp(*new_exp);
                                        } else {
                                            new_exp_list->append(*new_exp);
                                        }
                                    }
                                    if (susp_ars.find(cur_name) == susp_ars.end()) {
                                        transformable_array_sizes[cur_fun][cur_name] = new_exp_list;
                                    }
                                }
                            }
                        }
                    }
                    for (SgStatement *current_st = now; current_st != now->lastNodeOfStmt()->lexNext();
                         current_st = current_st->lexNext()) {
                        if (current_st->expr(0) && current_st->expr(0)->variant() == EXPR_LIST &&
                            (current_st->variant() == VAR_DECL)) {
                            if (current_st->expr(2)) {
                                SgExprListExp *cur_t = static_cast<SgExprListExp *>(current_st->expr(2));
                                int j = 0;
                                for (; j < cur_t->length(); ++j) {
                                    if (cur_t->elem(j)->variant() == ALLOCATABLE_OP) {
                                        break;
                                    }
                                }
                                if (j == cur_t->length()) {
                                    continue;
                                }
                            } else {
                                continue;
                            }
                            SgExprListExp *arr_lst = static_cast<SgExprListExp *>(current_st->expr(0));
                            int len = arr_lst->length();
                            for (int k = 0; k < len; ++k) {
                                auto cur_var = arr_lst->elem(k);
                                string cur_name = string(cur_var->symbol()->identifier());
                                int is_param = 0;
                                if (now->variant() != PROG_HEDR) {
                                    SgFunctionSymb *current_fun_symbol = static_cast<SgFunctionSymb *>(now->symbol());
                                    int par_ind = 0;
                                    for (; par_ind < current_fun_symbol->numberOfParameters(); ++par_ind) {
                                        if (current_fun_symbol->parameter(par_ind)->identifier() == cur_name) {
                                            break;
                                        }
                                    }
                                    if (par_ind != current_fun_symbol->numberOfParameters()) {
                                        is_param = 1;
                                    }
                                }
                                if (visited_ars.find(cur_name) == visited_ars.end() && !is_param && module == nullptr) {
                                    not_transformable_ars_dyn_to_stat[cur_fun].insert(cur_name);
                                    continue;
                                }
                            }
                        }
                    }
                }
                if (module == nullptr) {
                    int sum_decs = not_transformable_ars_dyn_to_stat[cur_fun].size() +
                                   not_transformable_ars_stat_to_dyn[cur_fun].size() + dyn_to_stat_ars[cur_fun].size() +
                                   stat_to_dyn_ars[cur_fun].size() + transformable_array_sizes[cur_fun].size();
                    for (SgStatement *st : raw_func_calls[cur_fun]) {
                        if (st->variant() == PROC_STAT) {
                            SgCallStmt *call_st = static_cast<SgCallStmt *>(st);
                            SgFunctionCallExp *call_expr = new SgFunctionCallExp(st->symbol()->copy());
                            int args_number = call_st->numberOfArgs();
                            for (int m = 0; m < args_number; ++m) {
                                call_expr->addArg(call_st->arg(m)->copy());
                            }
                            SgFunctionSymb *cur_sym = isSgFunctionSymb(st->symbol());
                            SgStatement *res = nullptr;
                            int is_found = 1;
                            string cur_name = find_name(cur_sym, cur_fun, proj, res, is_found);
                            if (!repeat_fl) {
                                procedure_calls[cur_name].push_back(st);
                            }
                            parse_exp_forward(cur_fun, call_expr, proj, susp_ars, repeat_fl);
                        } else {
                            for (int j = 0; j < 3; ++j) {
                                if (st->expr(j) && st->expr(j)->symbRefs()) {
                                    parse_exp_forward(cur_fun, st->expr(j), proj, susp_ars, repeat_fl);
                                }
                            }
                        }
                    }
                    for (string &new_name : funs_calls[cur_fun]) {
                        if (visited_spaces.find(new_name) == visited_spaces.end()) {
                            find_transformable_ars(new_name, proj, repeat_fl, visited_spaces);
                        } else {
                            cycle_funs.insert(new_name);
                        }
                    }
                    for (SgStatement *st : raw_func_calls[cur_fun]) {
                        if (st->variant() == PROC_STAT) {
                            SgFunctionSymb *cur_sym = isSgFunctionSymb(st->symbol());
                            SgStatement *res = nullptr;
                            int is_found = 1;
                            string cur_name = find_name(cur_sym, cur_fun, proj, res, is_found);
                            SgCallStmt *call_st = static_cast<SgCallStmt *>(st);
                            SgFunctionCallExp *call_expr = new SgFunctionCallExp(st->symbol()->copy());
                            int args_number = call_st->numberOfArgs();
                            for (int m = 0; m < args_number; ++m) {
                                call_expr->addArg(call_st->arg(m)->copy());
                            }
                            parse_exp(cur_fun, call_expr, proj, repeat_fl);
                        } else {
                            for (int j = 0; j < 3; ++j) {
                                if (st->expr(j) && st->expr(j)->symbRefs()) {
                                    parse_exp(cur_fun, st->expr(j), proj, repeat_fl);
                                }
                            }
                        }
                    }
                    int new_sum = not_transformable_ars_dyn_to_stat[cur_fun].size() +
                                  not_transformable_ars_stat_to_dyn[cur_fun].size() + dyn_to_stat_ars[cur_fun].size() +
                                  stat_to_dyn_ars[cur_fun].size() + transformable_array_sizes[cur_fun].size();
                    if (new_sum != sum_decs) {
                        is_changed = 1;
                        if (cycle_funs.find(cur_fun) != cycle_funs.end()) {
                            find_transformable_ars(cur_fun, proj, repeat_fl, visited_spaces);
                        }
                    }
                } else {
                    map<string, set<string>> checked_mod_vars;
                    for (auto mod = mods_uses[cur_fun].begin(); mod != mods_uses[cur_fun].end(); ++mod) {
                        string fun_name = mod->first;
                        auto var_names = mod->second;
                        for (auto var_name = var_names.begin(); var_name != var_names.end(); var_name++) {
                            string prev_name = var_name->first;
                            string new_name = var_name->second;
                            if (transformable_array_sizes[fun_name][new_name] == nullptr) {
                                continue;
                            }
                            if (transformable_array_sizes[cur_fun][prev_name] != nullptr &&
                                transformable_array_sizes[cur_fun][prev_name] !=
                                    transformable_array_sizes[fun_name][new_name]) {
                                not_transformable_ars_dyn_to_stat[cur_fun].insert(prev_name);
                                continue;
                            } else {
                                transformable_array_sizes[cur_fun][prev_name] =
                                    transformable_array_sizes[fun_name][new_name];
                                checked_mod_vars[fun_name].insert(prev_name);
                            }
                        }
                    }
                    for (auto it = checked_mod_vars.begin(); it != checked_mod_vars.end(); ++it) {
                        string fun_name = it->first;
                        auto set_vars = it->second;
                        for (auto var : set_vars) {
                            dyn_to_stat_ars[fun_name].insert(mods_uses[cur_fun][fun_name][var]);
                        }
                    }
                }
                is_done = 1;
                break;
            }
        }
    }
}

void find_interfaces(SgProject &project) {
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (auto st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->symbol() && isSgFunctionSymb(st->symbol())) {
                string cur_sym_ident = create_hash(st);
                SgStatement *interface_stmt = is_into_interface(st);
                if (!interface_stmt) {
                    continue;
                }
                funs_interfaces[cur_sym_ident].push_back(st);
                interfaces_decls[create_hash(wh(interface_stmt))].insert(st->symbol()->identifier());
            }
        }
    }
}

void parse_usr(string &pat, map<string, set<string>> &my_map, string &comms, string &fun_name) {
    int pos;
    if ((pos = comms.find(pat)) != -1) {
        int last_pos = comms.find("\n", pos + 1);
        string sub = comms.substr(pos + pat.size(), last_pos - (pos + pat.size()));
        sub.erase(remove_if(sub.begin(), sub.end(), [](char c) { return isspace(c); }), sub.end());
        int sep_pos = 0, new_sep_pos = sub.find(",", pos);
        while (new_sep_pos != -1) {
            string new_name = sub.substr(sep_pos, new_sep_pos - sep_pos);
            my_map[fun_name].insert(new_name);
            sep_pos = new_sep_pos + 1;
            new_sep_pos = sub.find(",", sep_pos);
        }
        string new_name = sub.substr(sep_pos, sub.length() - sep_pos);
        my_map[fun_name].insert(new_name);
    }
}

void create_interface(SgProject &project, SgStatement *space, string &fun_name,
                      string &ins_fun_file_name) { //добавить имена файлов
    int inserted = 0;
    vector<string> param_names;
    string file_name = get_real_file_name(space);
    for (int i = 0; !inserted && i < project.numberOfFiles(); ++i) {
        for (int j = 0; j < project.file(i).numberOfFunctions(); j++) {
            SgStatement *fun_st = project.file(i).functions(j);
            if (fun_st->controlParent()->variant() == INTERFACE_STMT) {
                continue;
            }
            string cur_fun_name = string(create_hash(fun_st));
            SgFunctionSymb *fun_symb = nullptr;
            if (cur_fun_name == fun_name) {
                set<SgStatement *> cur_modules;
                map<string, map<string, string>> modules_names;
                init_module_stat(fun_st, cur_modules, modules_names);
                SgStatement *fin_st = last_declarations[fun_st]->lexNext();
                SgStatement *res_st = nullptr;
                if (fun_st->variant() == PROC_HEDR) {
                    SgProcHedrStmt *proc_st = static_cast<SgProcHedrStmt *>(fun_st);
                    res_st = new SgProcHedrStmt(proc_st->symbol()->identifier());
                    set_file_name(res_st, file_name);
                    fun_symb = static_cast<SgFunctionSymb *>(proc_st->symbol());
                } else if (fun_st->variant() == FUNC_HEDR) {
                    SgFuncHedrStmt *func_st = static_cast<SgFuncHedrStmt *>(fun_st);
                    res_st = new SgFuncHedrStmt(func_st->symbol()->identifier());
                    fun_symb = static_cast<SgFunctionSymb *>(func_st->symbol());
                    res_st->setExpression(0, func_st->expr(0)->copy());
                    set_file_name(res_st, file_name);
                    string cur_ident = string(func_st->expr(0)->symbol()->identifier());
                    int found = 0;
                    for (SgStatement *st = fun_st; !found && st != fin_st; st = st->lexNext()) {
                        if (st->variant() == VAR_DECL) {
                            SgExprListExp *var_list = static_cast<SgExprListExp *>(st->expr(0));
                            for (int t = 0; t < var_list->length(); ++t) {
                                SgExprListExp *new_list = new SgExprListExp(*var_list->elem(t));
                                SgVarDeclStmt *new_st = new SgVarDeclStmt(*new_list, *st->expr(1)->type());
                                if (st->expr(2)) {
                                    new_st->setExpression(2, *st->expr(2));
                                }
                                res_st->insertStmtAfter(*new_st, *res_st);
                                set_file_name(new_st, file_name);
                                break;
                            }
                        }
                    }
                }
                SgFunctionSymb *new_fun_ref = static_cast<SgFunctionSymb *>(res_st->symbol());
                for (int k = 0; k < fun_symb->numberOfParameters(); ++k) {
                    string cur_ident = string(fun_symb->parameter(k)->identifier());
                    new_fun_ref->insertParameter(k, fun_symb->parameter(k)->copy());
                    int found = 0;
                    SgStatement *fin_st = last_declarations[fun_st]->lexNext();
                    for (SgStatement *st = fun_st; !found && st != fin_st; st = st->lexNext()) {
                        if (st->variant() == VAR_DECL) {
                            SgExprListExp *var_list = static_cast<SgExprListExp *>(st->expr(0));
                            for (int t = 0; t < var_list->length(); ++t) {
                                if (var_list->elem(t)->symbol() &&
                                    var_list->elem(t)->symbol()->identifier() == cur_ident) {
                                    if (var_list->elem(t)->symbol()->identifier() == cur_ident) {
                                        found = 1;
                                        SgExpression cur_el(var_list->elem(t)->copy());
                                        if (var_list->elem(t)->lhs()) {
                                            SgExprListExp *dims =
                                                static_cast<SgExprListExp *>(var_list->elem(t)->lhs());
                                            int is_dynamic = 0, len = dims->length();
                                            for (int j = 0; j < len; ++j) {
                                                if (dims->elem(j)->variant() == DDOT && !dims->elem(j)->lhs() &&
                                                    !dims->elem(j)->rhs()) {
                                                    is_dynamic = 1;
                                                    break;
                                                }
                                            }
                                            if (is_dynamic) {
                                                int num_dim = dynamic_array_shapes[ins_fun_file_name][fun_name][k];
                                                SgExprListExp *new_dims = new SgExprListExp(*new SgExpression(DDOT));
                                                for (int h = 1; h < num_dim; ++h) {
                                                    new_dims->append(*new SgExpression(DDOT));
                                                }
                                                cur_el.setLhs(*new_dims);
                                            } else {
                                                if (transformable_array_sizes_to_interfaces[fun_name].find(cur_ident) ==
                                                    transformable_array_sizes_to_interfaces[fun_name].end()) {
                                                    if (transformable_array_sizes[fun_name].find(cur_ident) !=
                                                            transformable_array_sizes[fun_name].end() &&
                                                        transformable_array_sizes[fun_name][cur_ident] != nullptr) {
                                                        transformable_array_sizes_to_interfaces[fun_name][cur_ident] =
                                                            &transformable_array_sizes[fun_name][cur_ident]->copy();
                                                    } else {
                                                        SgExprListExp *new_dims_list = nullptr;
                                                        for (int j = 0; j < len; ++j) {
                                                            SgExpression *cur_exp = dims->elem(j), *new_exp = nullptr;
                                                            if (cur_exp->variant() == DDOT) {
                                                                SgSubscriptExp *sub_exp =
                                                                    static_cast<SgSubscriptExp *>(cur_exp);
                                                                double low_bound = calculate_size(
                                                                    sub_exp->lbound(), cur_modules, modules_names);
                                                                double high_bound = calculate_size(
                                                                    sub_exp->ubound(), cur_modules, modules_names);
                                                                if (isnanf(low_bound) || isnanf(high_bound)) {
                                                                    new_exp = &cur_exp->copy();
                                                                } else {
                                                                    SgValueExp *first_arg =
                                                                        new SgValueExp((int)low_bound);
                                                                    SgValueExp *second_arg =
                                                                        new SgValueExp((int)high_bound);
                                                                    new_exp =
                                                                        new SgSubscriptExp(*first_arg, *second_arg);
                                                                }
                                                            } else {
                                                                double new_val =
                                                                    calculate_size(cur_exp, cur_modules, modules_names);
                                                                if (isnanf(new_val)) {
                                                                    new_exp = &cur_exp->copy();
                                                                } else {
                                                                    new_exp = new SgValueExp((int)new_val);
                                                                }
                                                            }
                                                            if (new_dims_list == nullptr) {
                                                                new_dims_list = new SgExprListExp(*new_exp);
                                                            } else {
                                                                new_dims_list->append(*new_exp);
                                                            }
                                                        }
                                                        transformable_array_sizes_to_interfaces[fun_name][cur_ident] =
                                                            new_dims_list;
                                                    }
                                                }
                                                cur_el.setLhs(
                                                    transformable_array_sizes_to_interfaces[fun_name][cur_ident]
                                                        ->copy());
                                            }
                                        }
                                        SgExprListExp *new_list = new SgExprListExp(cur_el);
                                        SgVarDeclStmt *new_st = new SgVarDeclStmt(*new_list, *st->expr(1)->type());
                                        if (st->expr(2)) {
                                            new_st->setExpression(2, *st->expr(2));
                                        }
                                        res_st->insertStmtAfter(*new_st, *res_st);
                                        set_file_name(new_st, file_name);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                string *name = new string("added_interface_for_func " + fun_name);
                SgInterfaceSymb *new_inter_sym = new SgInterfaceSymb(const_cast<char *>(name->c_str()), *space);
                SgStatement *inter_st =
                    new SgStatement(INTERFACE_STMT, nullptr, new_inter_sym, nullptr, nullptr, nullptr);
                update_last_declaration(space, inter_st, 1);
                set_file_name(inter_st, file_name);
                inter_st->insertStmtAfter(*res_st, *inter_st);
                last_declarations[inter_st->lexNext()] = myLastDeclaration(inter_st->lexNext());
                inserted = 1;
                SgStatement *last_st = last_declarations[space]->lexNext();
                string true_fun_name(fun_st->symbol()->identifier());
                for (SgStatement *st = space; st != last_st;) {
                    SgStatement *tmp = st->lexNext();
                    if (tmp->variant() == EXTERN_STAT) {
                        SgVarListDeclStmt *ext_st = static_cast<SgVarListDeclStmt *>(tmp);
                        int n = ext_st->numberOfSymbols();
                        for (int y = 0; y < n; ++y) {
                            if (ext_st->symbol(y)->identifier() == true_fun_name) {
                                ext_st->deleteSymbol(y ? y + 1 : y);
                                break;
                            }
                        }
                    }
                    st = tmp;
                }
                return;
            }
        }
    }
}

void insert_interfaces(SgProject &project) {
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (int j = 0; j < project.file(i).numberOfFunctions(); j++) {
            SgStatement *fun_st = project.file(i).functions(j);
            string fun_name = string(create_hash(fun_st));
            if (fun_st->controlParent()->variant() == INTERFACE_STMT) {
                continue;
            }
            for (auto &cur_pair : funs_funs_calls[fun_name]) {
                string call_name = cur_pair.second.first;

                SgStatement *cur_fun_st = cur_pair.first;
                if (!cur_fun_st) {
                    continue;
                }
                SgFunctionSymb *cur_sym = static_cast<SgFunctionSymb *>(cur_fun_st->symbol());
                SgStatement *fin_st = last_declarations[cur_fun_st]->lexNext();
                if (interfaces_decls[fun_name].find(call_name) != interfaces_decls[fun_name].end()) {
                    continue;
                }
                SgExpression *call_expr = cur_pair.second.second;
                if (!call_expr) {
                    continue;
                }
                int inserted = 0;
                for (int k = 0; !inserted && k < cur_sym->numberOfParameters(); ++k) {
                    SgSymbol *cur_arg_sym = cur_sym->parameter(k);
                    string cur_arg_sym_name = string(cur_arg_sym->identifier());
                    for (SgStatement *st = cur_fun_st; st != fin_st; st = st->lexNext()) {
                        if (st->variant() == VAR_DECL) {
                            SgVarDeclStmt *dec_st = static_cast<SgVarDeclStmt *>(st);
                            int sym_num = dec_st->numberOfSymbols();
                            int o = 0;
                            for (; o < sym_num; ++o) {
                                if (string(dec_st->symbol(o)->identifier()) == cur_arg_sym_name) {
                                    break;
                                }
                            }
                            if (o == sym_num) {
                                continue;
                            }
                            SgExprListExp *st_spec = static_cast<SgExprListExp *>(st->expr(2));
                            if (!st->expr(2)) {
                                break;
                            }
                            int t = 0;
                            for (; t < st_spec->length(); t++) {
                                if (st_spec->elem(t)->variant() == ALLOCATABLE_OP) {
                                    break;
                                }
                            }
                            if (t == st_spec->length()) {
                                break;
                            }
                            create_interface(project, fun_st, call_name, fun_name);
                            interfaces_decls[fun_name].insert(call_name);
                            inserted = 1;
                            break;
                        }
                    }
                }
            }
        }
    }
}

void fill_mod_publics() {
    for (SgStatement *cur_mod : modules) {
        string mod_name = string(cur_mod->symbol()->identifier());
        for (SgStatement *st = cur_mod; st != cur_mod->lastNodeOfStmt()->lexNext(); st = st->lexNext()) {
            if (st->variant() == VAR_DECL) {
                int if_private = 0;
                if (st->expr(2)) {
                    SgExprListExp *exp_list = static_cast<SgExprListExp *>(st->expr(2));
                    for (int i = 0; i < exp_list->length(); ++i) {
                        if (exp_list->elem(i)->variant() == PRIVATE_OP) {
                            if_private = 1;
                            break;
                        }
                    }
                }
                if (if_private == 1) {
                    continue;
                }
                SgExprListExp *exp_list = static_cast<SgExprListExp *>(st->expr(0));
                for (int i = 0; i < exp_list->length(); ++i) {
                    char *ident = exp_list->elem(i)->symbol() ? exp_list->elem(i)->symbol()->identifier()
                                                              : exp_list->elem(i)->operand(1)->symbol()->identifier();
                    mod_publics[mod_name].insert(ident);
                }
            }
            if (st->variant() == PROC_HEDR || st->variant() == FUNC_HEDR) {
                mod_publics[mod_name].insert(st->symbol()->identifier());
            }
        }
    }
}

void fill_mod_use(SgProject &proj) {
    fill_mod_publics();
    for (int i = 0; i < proj.numberOfFiles(); ++i) {
        for (int j = 0; j < proj.file(i).numberOfFunctions(); ++j) {
            SgStatement *now = proj.file(i).functions(j);
            string fun_name(create_hash(now));
            for (SgStatement *cur_st = now; cur_st != now->lastNodeOfStmt(); cur_st = cur_st->lexNext()) {
                if (cur_st->variant() == USE_STMT) {
                    string mod_name(cur_st->symbol()->identifier());
                    if (cur_st->expr(0)) {
                        uses.insert(cur_st);
                        SgExprListExp *only_lst =
                            static_cast<SgExprListExp *>(static_cast<SgUseOnlyExp *>(cur_st->expr(0))->onlyList());
                        for (int k = 0; k < only_lst->length(); ++k) {
                            SgExpression *cur_exp = only_lst->elem(k);
                            if (cur_exp->variant() == RENAME_NODE && cur_exp->operand(1) && cur_exp->operand(2)) {
                                mods_uses[mod_name][fun_name][string(cur_exp->operand(1)->symbol()->identifier())] =
                                    string(cur_exp->operand(2)->symbol()->identifier());
                            } else {
                                mods_uses[mod_name][fun_name][string(cur_exp->symbol()->identifier())] =
                                    string(cur_exp->symbol()->identifier());
                            }
                        }
                    } else {
                        for (string sym : mod_publics[mod_name]) {
                            mods_uses[mod_name][fun_name][sym] = sym;
                        }
                    }
                }
            }
        }
    }
}

void module_spread(int fl) {
    for (auto &it1 : mods_uses) {
        string mod_name = it1.first;
        auto &map_funs = it1.second;
        for (auto &it2 : map_funs) {
            string fun_name = it2.first;
            auto &sym_names = it2.second;
            for (auto &it3 : sym_names) {
                string prev_name = it3.first, new_name = it3.second;
                if (fl == 1) {
                    if (not_transformable_ars_dyn_to_stat[mod_name].find(prev_name) !=
                        not_transformable_ars_dyn_to_stat[mod_name].end()) {
                        not_transformable_ars_dyn_to_stat[fun_name].insert(new_name);
                    }
                    if (not_transformable_ars_stat_to_dyn[mod_name].find(prev_name) !=
                        not_transformable_ars_stat_to_dyn[mod_name].end()) {
                        not_transformable_ars_stat_to_dyn[fun_name].insert(new_name);
                    }
                }
                if (fl == 2) {
                    if (dyn_to_stat_ars[mod_name].find(prev_name) != dyn_to_stat_ars[mod_name].end() &&
                        not_transformable_ars_dyn_to_stat[mod_name].find(prev_name) ==
                            not_transformable_ars_dyn_to_stat[mod_name].end()) {
                        dyn_to_stat_ars[fun_name].insert(new_name);
                    }
                    if (stat_to_dyn_ars[mod_name].find(prev_name) != stat_to_dyn_ars[mod_name].end() &&
                        not_transformable_ars_stat_to_dyn[mod_name].find(prev_name) ==
                            not_transformable_ars_stat_to_dyn[mod_name].end()) {
                        stat_to_dyn_ars[fun_name].insert(new_name);
                    }
                }
            }
        }
    }
}

void add_deallocate(SgStatement *now, vector<SgStatement *> array_of_deallocations, vector<SgVariableSymb *> &stats,
                    int i, SgStatement **bad_fun_arr) {
    string filename(wh(now)->fileName());
    for (long unsigned int m = 0; m < array_of_deallocations.size(); m++) {
        auto deal = array_of_deallocations[m]->copy();
        set_file_name(&deal, filename);
        now->insertStmtBefore(deal, *now->controlParent());
        SgStatement *bad_if = createif(new SgVarRefExp(*stats[m]));
        now->insertStmtBefore(*bad_if, *now->controlParent());
        SgCallStmt *call_bad = new SgCallStmt(*(bad_fun_arr[i]->symbol()));
        string *first_str = new string(deal.expr(0)->unparse()), *second_str = new string(create_hash(wh(now)));
        call_bad->addArg(*new SgValueExp((char *)first_str->c_str()));
        call_bad->addArg(*new SgValueExp((char *)second_str->c_str()));
        bad_if->insertStmtAfter(*call_bad, *bad_if);
        set_file_name(bad_if, filename);
        set_file_name(call_bad, filename);
    }
}

void transform_all(SgProject &project, map<string, set<string>> &map_to_fill, int if_auto) {
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->variant() == MODULE_STMT || st->variant() == MODULE_STMT || st->variant() == PROG_HEDR ||
                st->variant() == FUNC_HEDR || st->variant() == PROC_HEDR || st->variant() == BLOCK_DATA) {
                SgStatement *last_st = last_declarations[st]->lexNext();
                string fun_name(create_hash(st));
                set<string> param_names;
                SgFunctionSymb *space_sym = st->symbol() ? isSgFunctionSymb(st->symbol()) : nullptr;
                if (if_auto) {
                    if (space_sym == nullptr || st->variant() == PROG_HEDR) {
                        continue;
                    } else {
                        int n_params = space_sym->numberOfParameters();
                        for (int k = 0; k < n_params; ++k) {
                            param_names.insert(space_sym->parameter(k)->identifier());
                        }
                    }
                }
                for (SgStatement *cur_st = st; cur_st != last_st; cur_st = cur_st->lexNext()) {
                    if (cur_st->variant() == VAR_DECL) {
                        SgDeclarationStatement *dec_st = static_cast<SgDeclarationStatement *>(cur_st);
                        int st_len = dec_st->numberOfVars();
                        for (int k = 0; k < st_len; ++k) {
                            SgExpression *cur_var = nullptr;
                            if (dec_st->var(k)->symbol()) {
                                cur_var = dec_st->var(k);
                            } else {
                                cur_var = dec_st->var(k)->operand(1);
                            }
                            if (cur_var->symbol()->type()->variant() == T_ARRAY) {
                                if (if_auto) {
                                    SgArrayType *ar_type = static_cast<SgArrayType *>(cur_var->symbol()->type());
                                    int n_dim = ar_type->dimension();
                                    int is_auto = 0;
                                    for (int m = 0; !is_auto && m < n_dim; ++m) {
                                        SgExprListExp *symb_refs =
                                            static_cast<SgExprListExp *>(ar_type->sizeInDim(m)->symbRefs());
                                        if (!symb_refs) {
                                            continue;
                                        }
                                        int refs_len = symb_refs->length();
                                        for (int y = 0; y < refs_len; ++y) {
                                            if (param_names.find(symb_refs->elem(y)->symbol()->identifier()) !=
                                                param_names.end()) {
                                                is_auto = 1;
                                                break;
                                            }
                                        }
                                    }
                                    if (is_auto) {
                                        map_to_fill[fun_name].insert(cur_var->symbol()->identifier());
                                    }
                                } else {
                                    map_to_fill[fun_name].insert(cur_var->symbol()->identifier());
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

int main(int argc, char **argv) {
    SgProject project("dvm.proj");
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->variant() == MODULE_STMT) {
                modules.insert(st);
            }
        }
    }
    parse_last_declarations(project);
    map<string, set<int>> susp_decl;
    map<SgStatement *, set<int>> local_susp;
    for (int j = 0; j < project.numberOfFiles(); ++j) {
        for (SgStatement *st = project.file(j).firstStatement(); st;) {
            SgStatement *next = st->lexNext();
            if (st->variant() == DIM_STAT) {
                SgExprListExp *tmp = static_cast<SgExprListExp *>(st->expr(0));
                for (int i = 0; i < tmp->length(); ++i) {
                    SgExpression *cur_exp = tmp->elem(i);
                    string ident;
                    SgExpression *dims;
                    if (cur_exp->variant() == ASSGN_OP) {
                        ident = cur_exp->operand(1)->symbol()->identifier();
                        dims = cur_exp->operand(1)->lhs();
                        cur_exp->operand(1)->setLhs(nullptr);
                    } else {
                        ident = cur_exp->symbol()->identifier();
                        dims = cur_exp->lhs();
                        cur_exp->setLhs(nullptr);
                    }
                    SgStatement *sp = wh(st);
                    int found_fl = 0;
                    string first_name = get_real_file_name(sp);
                    for (SgStatement *stm = sp; !found_fl && stm != last_declarations[sp]->lexNext();
                         stm = stm->lexNext()) {
                        if (get_real_file_name(stm) != first_name) {
                            first_name = get_real_file_name(stm);
                        }
                        if (stm->variant() == VAR_DECL) {
                            SgVarDeclStmt *dec_st = static_cast<SgVarDeclStmt *>(stm);
                            for (int k = 0; !found_fl && k < dec_st->numberOfSymbols(); ++k) {
                                char *sym_iden = dec_st->symbol(k)->identifier();
                                if (sym_iden == ident) {
                                    string name = string(dec_st->unparse());
                                    name.erase(remove_if(name.begin(), name.end(), [](char c) { return isspace(c); }),
                                               name.end());
                                    susp_decl[name].insert(k);
                                    local_susp[dec_st].insert(k);
                                    SgExpression *lis = new SgExprListExp(*cur_exp);
                                    SgAttributeExp *at_exp = new SgAttributeExp(DIMENSION_OP);
                                    at_exp->setLhs(dims);
                                    SgVarDeclStmt *new_dec_st =
                                        new SgVarDeclStmt(*lis, *at_exp, *dec_st->expr(1)->type());
                                    update_last_declaration(sp, new_dec_st, 1);
                                    dec_st->deleteSymbol(k ? k + 1 : k);
                                    if (last_declarations[sp] == dec_st) {
                                        last_declarations[sp] = new_dec_st;
                                    }
                                    string filename(wh(st)->fileName());
                                    set_file_name(new_dec_st, filename);
                                    found_fl = 1;
                                    break;
                                }
                            }
                        }
                    }
                    if (!found_fl) {
                        SgExpression *lis = new SgExprListExp(*cur_exp);
                        SgAttributeExp *at_exp = new SgAttributeExp(DIMENSION_OP);
                        at_exp->setLhs(dims);
                        SgVarDeclStmt *new_dec_st = new SgVarDeclStmt(*lis, *at_exp, *new SgType(T_FLOAT));
                        update_last_declaration(sp, new_dec_st, 1);
                        string st_filename(st->fileName());
                        set_file_name(new_dec_st, st_filename);
                    }
                }
                my_delete(st);
            }
            st = next;
        }
    }
    int ignore = 0;
    if (argc > 1) {
        ignore = 1;
        if (string(argv[1]) == "all_dynamic") {
            transform_all(project, stat_to_dyn_ars, 0);
        } else if (string(argv[1]) == "all_static") {
            transform_all(project, dyn_to_stat_ars, 0);
        } else if (string(argv[1]) == "all_automatic") {
            transform_all(project, dyn_to_stat_ars, 1);
        }
    }
    string pat_one("dyn_to_st"), pat_two("st_to_dyn");
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->variant() == MODULE_STMT || st->variant() == MODULE_STMT || st->variant() == PROG_HEDR ||
                st->variant() == FUNC_HEDR || st->variant() == PROC_HEDR || st->variant() == BLOCK_DATA) {
                SgStatement *fun_st = st;
                string fun_name(create_hash(fun_st));
                if (fun_st->comments() && !ignore) {
                    string comms(fun_st->comments());
                    parse_usr(pat_one, dyn_to_stat_ars, comms, fun_name);
                    parse_usr(pat_two, stat_to_dyn_ars, comms, fun_name);
                }
                SgStatement *fin_st = last_declarations[st]->lexNext();
                for (SgStatement *cur_st = st; cur_st != fin_st; cur_st = cur_st->lexNext()) {
                    if (cur_st->comments() && !ignore) {
                        string comms(cur_st->comments());
                        parse_usr(pat_one, dyn_to_stat_ars, comms, fun_name);
                        parse_usr(pat_two, stat_to_dyn_ars, comms, fun_name);
                    }
                }
                if (isSgFunctionSymb(st->symbol())) {
                    hashes.insert(create_hash(st));
                }
            }
        }
    }
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgSymbol *sym = project.file(i).firstSymbol(); sym; sym = sym->next()) {
            sym_identifiers.insert(sym->identifier());
        }
    }
    fill_mod_use(project);
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st;) {
            if (st->variant() == PROC_HEDR || st->variant() == FUNC_HEDR || st->variant() == PROG_HEDR) {
                string hash = create_hash(st);
                SgStatement *now = st;
                st = st->lexNext();
                set<string> cur_funs;
                while (st && st != now->lastNodeOfStmt()) {
                    if (st->variant() == PROC_STAT) {
                        SgFunctionSymb *cur_sym = isSgFunctionSymb(st->symbol());
                        SgStatement *res = nullptr;
                        for (SgStatement *mod : modules) {
                            string mod_name = create_hash(mod);
                            if (mods_uses[mod_name][hash].find(string(cur_sym->identifier())) !=
                                mods_uses[mod_name][hash].end()) {
                                for (SgStatement *st = mod; st != mod->lastNodeOfStmt(); st = st->lexNext()) {
                                    if (st->variant() == PROC_HEDR &&
                                        string(st->symbol()->identifier()) == cur_sym->identifier()) {
                                        res = st;
                                        break;
                                    }
                                }
                            }
                        }
                        string tm_hash = res ? create_hash(res) : string(cur_sym->identifier());
                        if (cur_funs.find(tm_hash) == cur_funs.end()) {
                            funs_calls[hash].push_back(tm_hash);
                            cur_funs.insert(tm_hash);
                        }
                        raw_func_calls[hash].push_back(st);
                        st = st->lexNext();
                    } else {
                        for (int j = 0; j < 3; ++j) {
                            if (st->variant() != USE_STMT && st->expr(j) && st->expr(j)->symbRefs()) {
                                SgExprListExp *sym_list = static_cast<SgExprListExp *>(st->expr(j)->symbRefs());
                                for (int k = 0; k < sym_list->length(); k++) {
                                    SgFunctionSymb *cur_sym = isSgFunctionSymb(sym_list->elem(k)->symbol());
                                    if (cur_sym) {
                                        SgStatement *res = nullptr;
                                        for (SgStatement *mod : modules) {
                                            string mod_name = create_hash(mod);
                                            if (mods_uses[mod_name][hash].find(string(cur_sym->identifier())) !=
                                                mods_uses[mod_name][hash].end()) {
                                                for (SgStatement *st = mod; st != mod->lastNodeOfStmt();
                                                     st = st->lexNext()) {
                                                    if (st->variant() == PROC_HEDR &&
                                                        string(st->symbol()->identifier()) == cur_sym->identifier()) {
                                                        res = st;
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        string tm_hash = res ? create_hash(res) : string(cur_sym->identifier());
                                        if (cur_funs.find(tm_hash) == cur_funs.end()) {
                                            funs_calls[hash].push_back(tm_hash);
                                            raw_func_calls[hash].push_back(st);
                                            cur_funs.insert(tm_hash);
                                        }
                                    }
                                }
                            }
                        }
                        st = st->lexNext();
                    }
                }
            } else {
                st = st->lexNext();
            }
        }
    }
    find_interfaces(project);
    map<string, string> old_new_block_name;
    set<string> visited_comms;
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->expr(0) && st->expr(0)->variant() == COMM_LIST) {
                SgStatement *scope = wh(st);
                if (!st->expr(0)->symbol()) {
                    string cand_name("added_mod_name");
                    while (hashes.find(cand_name) != hashes.end()) {
                        cand_name += '1';
                    }
                    st->expr(0)->setSymbol(*new SgConstructSymb(const_cast<char *>(cand_name.c_str()), *scope));
                }
                string block_name = st->expr(0)->symbol()->identifier();
                if (visited_comms.find(block_name) != visited_comms.end()) {
                    continue;
                } else {
                    visited_comms.insert(block_name);
                }
                block_name.erase(remove_if(block_name.begin(), block_name.end(), [](char c) { return isspace(c); }),
                                 block_name.end());
                string first_block_name = block_name;
                while (hashes.find(block_name) != hashes.end()) {
                    block_name += '1';
                }
                hashes.insert(block_name);
                old_new_block_name[first_block_name] = block_name;
                st->expr(0)->setSymbol(*new SgConstructSymb((char *)block_name.c_str(), *scope));
            }
        }
    }
    find_private_blocks(project, old_new_block_name);
    data_parse(project);
    set<string> visited;
    visited.insert("program");
    parse_data_blocks(project);
    parse_modules("program", project, visited);
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->expr(0) && st->expr(0)->variant() == COMM_LIST) {
                if (modules_functions.find(st->expr(0)->symbol()->identifier()) == modules_functions.end()) {
                    modules_functions[st->expr(0)->symbol()->identifier()].first =
                        wh(st)->symbol() ? wh(st)->symbol()->identifier() : " ";
                    SgNestedVarListDeclStmt *decl_st = static_cast<SgNestedVarListDeclStmt *>(st);
                    SgExprListExp *cur_list = static_cast<SgExprListExp *>(decl_st->list(0));
                    for (int j = 0; j < cur_list->length(); ++j) { //для проверки, все ли типы совпадают
                        modules_functions[st->expr(0)->symbol()->identifier()].second +=
                            ' ' + to_string(cur_list->elem(j)->symbol()->type()->variant());
                    }
                }
            }
        }
    }
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        if (project.file(i).mainProgram()) {
            if (main_fun && project.file(i).mainProgram() != main_fun) {
                cout << "ERROR: TWO MAIN FUNCTIONS";
                return 1;
            }
            main_fun = project.file(i).mainProgram();
            to_ins = last_declarations[main_fun];
            SgStatement *current_st = main_fun;
            for (; current_st;) {
                SgStatement *tmp = current_st->lexNext();
                if (current_st->expr(0) && current_st->expr(0)->variant() == COMM_LIST) {
                    if (wh(current_st)->variant() == PROG_HEDR &&
                        modules_functions[current_st->expr(0)->symbol()->identifier()].first == string("program") &&
                        dont_touch_modules_functions[current_st->expr(0)->symbol()->identifier()] == "") {
                        if (private_blocks.find(current_st->expr(0)->symbol()->identifier()) == private_blocks.end()) {
                            BlocktoMod(static_cast<SgObjectListExp *>(current_st->expr(0)), current_st, project.file(i),
                                       1);
                        }
                    }
                }
                current_st = tmp;
            }
        }
    }
    if (!main_fun) {
        cout << "ERROR: NO MAIN FUNCTIONS" << endl;
        return 1;
    }
    SgStatement *bad_fun_arr[project.numberOfFiles()] = {0};
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        string name("memory_errors_handler");
        name += to_string(i);
        while (hashes.find(name) != hashes.end()) {
            name += to_string(i);
        }
        hashes.insert(name);
        SgFunctionSymb *bad_fun_sym =
            new SgFunctionSymb(FUNCTION_NAME, (char *)name.c_str(), *SgTypeInt(), *(project.file(i).firstStatement()));
        SgSymbol first_par = *new SgVariableSymb("ar_name"), second_par = *new SgVariableSymb("fun_name");
        bad_fun_sym->insertParameter(0, first_par);
        bad_fun_sym->insertParameter(1, second_par);
        bad_fun = new SgProcHedrStmt(*bad_fun_sym);
        bad_fun_arr[i] = bad_fun;
        SgExprListExp *out_str =
            new SgExprListExp(*new SgValueExp("MEMORY ALLOCATION ERROR. PROGRAM IS BEING TERMINATED."));
        out_str->append(*new SgVarRefExp(first_par));
        out_str->append(*new SgVarRefExp(second_par));
        SgInputOutputStmt *io_st =
            new SgInputOutputStmt(PRINT_STAT, *new SgVarRefExp(*new SgVariableSymb("*", *new SgType(T_INT))), *out_str);
        project.file(i).firstStatement()->insertStmtAfter(*bad_fun_arr[i]);
        last_declarations[bad_fun_arr[i]] = bad_fun_arr[i];
        SgStatement *tmp1 = new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
        string str(" character (len = *):: ar_name, fun_name");
        string *new_inc = new string(str);
        NODE_STR(tmp1->expr(0)->thellnd) = (char *)new_inc->c_str();
        bad_fun_arr[i]->insertStmtAfter(*tmp1);
        tmp1->insertStmtAfter(*io_st);
        io_st->insertStmtAfter(*new SgCallStmt(
            *new SgFunctionSymb(FUNCTION_NAME, "EXIT", *new SgType(T_FUNCTION), *bad_fun_arr[i]), *new SgValueExp(1)));
        for (SgStatement *current_st = project.file(i).firstStatement(); current_st;) {
            SgStatement *tmp = current_st->lexNext();
            if (current_st->expr(0) && current_st->expr(0)->variant() == COMM_LIST) {
                if (((wh(current_st)->symbol() &&
                      modules_functions[current_st->expr(0)->symbol()->identifier()].first ==
                          string(wh(current_st)->symbol()->identifier())) ||
                     (!(wh(current_st)->symbol()) &&
                      modules_functions[current_st->expr(0)->symbol()->identifier()].first == " ")) &&
                    dont_touch_modules_functions[current_st->expr(0)->symbol()->identifier()] == "") {
                    if (private_blocks.find(current_st->expr(0)->symbol()->identifier()) == private_blocks.end()) {
                        BlocktoMod(static_cast<SgObjectListExp *>(current_st->expr(0)), current_st, project.file(i), 1);
                    }
                }
            }
            current_st = tmp;
        }
    }
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *current_st = project.file(i).firstStatement(); current_st;) {
            SgStatement *tmp = current_st->lexNext();
            if (current_st->expr(0) && current_st->expr(0)->variant() == COMM_LIST) {
                if (dont_touch_modules_functions[current_st->expr(0)->symbol()->identifier()] == "" &&
                    modules_functions[current_st->expr(0)->symbol()->identifier()].first !=
                        string(wh(current_st)->symbol()->identifier())) {
                    if (private_blocks.find(current_st->expr(0)->symbol()->identifier()) == private_blocks.end()) {
                        BlocktoMod(static_cast<SgObjectListExp *>(current_st->expr(0)), current_st, project.file(i), 0);
                    }
                }
            }
            current_st = tmp;
        }
    }
    map<int, set<string>> ars_from_com;
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        f = &(project.file(i));
        for (SgStatement *current_st = f->firstStatement(); current_st; current_st = current_st->lexNext()) {
            if (current_st->expr(0) && current_st->expr(0)->variant() == COMM_LIST) {
                SgNestedVarListDeclStmt *decl_st = static_cast<SgNestedVarListDeclStmt *>(current_st);
                SgExprListExp *cur_list = static_cast<SgExprListExp *>(decl_st->list(0));
                for (int j = 0; j < cur_list->length(); ++j) {
                    SgSymbol *cur_sym = cur_list->elem(j)->symbol() ? cur_list->elem(j)->symbol()
                                                                    : cur_list->elem(j)->operand(1)->symbol();
                    if (cur_sym->type()->variant() == T_ARRAY) {
                        ars_from_com[wh(current_st)->id()].insert(string(cur_sym->identifier()));
                        not_transformable_ars_stat_to_dyn[create_hash(wh(current_st))].insert(
                            string(cur_sym->identifier()));
                    }
                }
            }
            if (current_st->variant() == SAVE_DECL) {
                auto save_st = static_cast<SgVarListDeclStmt *>(current_st);
                for (int j = 0; j < save_st->numberOfSymbols(); ++j) {
                    if (save_st->symbol(j)->type()->variant() == T_ARRAY) {
                        ars_from_com[wh(current_st)->id()].insert(string(save_st->symbol(j)->identifier()));
                        not_transformable_ars_stat_to_dyn[create_hash(wh(current_st))].insert(
                            string(save_st->symbol(j)->identifier()));
                    }
                }
            }
        }
    }
    modules.clear();
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            if (st->variant() == MODULE_STMT) {
                modules.insert(st);
            }
        }
    }
    fill_mod_use(project);
    string pr("program");
    set<string> visited_spaces;
    find_transformable_ars(pr, project, 0, visited_spaces);
    visited_spaces.clear();
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *current_st = f->firstStatement(); current_st; current_st = current_st->lexNext()) {
            if (current_st->variant() == MODULE_STMT || current_st->variant() == BLOCK_DATA) {
                string name = create_hash(current_st); //мб не надо
                find_transformable_ars(name, project, 0, visited_spaces, current_st);
            }
        }
    }
    visited_spaces.clear();
    do {
        is_changed = 0;
        find_transformable_ars(pr, project, 1, visited_spaces);
        visited_spaces.clear();
        extract_uses(project, 1);
        module_spread(1);
    } while (is_changed);
    visited_spaces.clear();
    do {
        is_changed = 0;
        find_transformable_ars(pr, project, 2, visited_spaces);
        visited_spaces.clear();
        extract_uses(project, 2);
        module_spread(2);
    } while (is_changed);
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *current_st = f->firstStatement(); current_st; current_st = current_st->lexNext()) {
            if (current_st->variant() == VAR_DECL) {
                string st_file_name(get_real_file_name(current_st)), control_file(get_real_file_name(wh(current_st)));
                if (st_file_name != control_file) {
                    SgVarDeclStmt *dec_st = static_cast<SgVarDeclStmt *>(current_st);
                    int sim_num = dec_st->numberOfSymbols();
                    for (int j = 0; j < sim_num; ++j) {
                        string cur_iden(dec_st->symbol(j)->identifier());
                        if ((dyn_to_stat_ars[control_file].find(cur_iden) != dyn_to_stat_ars[control_file].end() &&
                             not_transformable_ars_dyn_to_stat[control_file].find(cur_iden) !=
                                 not_transformable_ars_dyn_to_stat[control_file].end()) ||
                            (stat_to_dyn_ars[control_file].find(cur_iden) != stat_to_dyn_ars[control_file].end() &&
                             not_transformable_ars_stat_to_dyn[control_file].find(cur_iden) !=
                                 not_transformable_ars_stat_to_dyn[control_file].end())) {
                            susp_ars_from_inc[st_file_name].insert(cur_iden);
                        }
                    }
                }
            }
        }
    }
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *current_st = f->firstStatement(); current_st;) {
            SgStatement *tmp = current_st->lexNext();
            if (current_st->variant() == INTERFACE_STMT) {
                current_st = current_st->lastNodeOfStmt()->lexNext();
                continue;
            }
            if (current_st->expr(0) && current_st->expr(0)->variant() == EXPR_LIST &&
                (current_st->variant() == VAR_DECL)) {
                if (current_st->expr(2)) {
                    SgExprListExp *cur_t = static_cast<SgExprListExp *>(current_st->expr(2));
                    int j = 0;
                    for (; j < cur_t->length(); ++j) {
                        if (cur_t->elem(j)->variant() == ALLOCATABLE_OP) {
                            break;
                        }
                    }
                    if (j == cur_t->length()) {
                        current_st = tmp;
                        continue;
                    }
                    current_st = dyn_to_stat(current_st);
                    continue;
                }
            }
            current_st = tmp;
        }
    }
    clear_space(project);
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        SgStatement *main = nullptr;
        current_file = &(project.file(i));
        current_file_id = i;
        for (SgStatement *current_st = f->firstStatement(); current_st;) {
            SgStatement *tmp = current_st->lexNext();
            if (current_st->expr(0) && current_st->expr(0)->variant() == EXPR_LIST &&
                (current_st->variant() == VAR_DECL)) {
                if (current_st->expr(2)) {
                    SgExprListExp *cur_t = static_cast<SgExprListExp *>(current_st->expr(2));
                    int j = 0;
                    for (; j < cur_t->length(); ++j) {
                        if (cur_t->elem(j)->variant() == PARAMETER_OP || cur_t->elem(j)->variant() == POINTER_OP ||
                            cur_t->elem(j)->variant() == ALLOCATABLE_OP) {
                            break;
                        }
                    }
                    if (j != cur_t->length()) {
                        current_st = tmp;
                        continue;
                    }
                }
                vector<SgStatement *> array_of_deallocations;
                vector<SgVariableSymb *> stats;
                main_parse(current_st->expr(0), current_st->variant() == VAR_DECL, current_st, main, i,
                           array_of_deallocations, stats, ars_from_com, bad_fun_arr);
                SgStatement *now = wh(current_st), *fix_now = now;
                if (now->variant() != MODULE_STMT) {
                    SgStatement *last_node = fix_now->lastNodeOfStmt();
                    for (; now && now != last_node; now = now->lexNext()) {
                        if ((now->variant() == EXIT_STMT || now->variant() == RETURN_NODE ||
                             now->variant() == RETURN_STAT) &&
                            now->controlParent()->variant() != LOGIF_NODE &&
                            (now->lexPrev() && now->lexPrev()->variant() != EXIT_STMT &&
                             now->lexPrev()->variant() != RETURN_NODE && now->lexPrev()->variant() != RETURN_STAT)) {
                            add_deallocate(now, array_of_deallocations, stats, i, bad_fun_arr);
                        }
                    }
                    if (last_node->lexPrev()->variant() != RETURN_STAT &&
                        last_node->lexPrev()->variant() != RETURN_NODE) {
                        add_deallocate(last_node, array_of_deallocations, stats, i, bad_fun_arr);
                    }
                }
                if (!current_st->expr(0) || static_cast<SgExprListExp *>(current_st->expr(0))->length() == 0) {
                    my_delete(current_st);
                }
            }
            current_st = tmp;
        }
        for (SgStatement *current_st = f->firstStatement(); current_st;) {
            SgStatement *tmp = current_st->lexNext();
            if ((current_st->variant() == VAR_DECL || current_st->variant() == PARAM_DECL) &&
                (!static_cast<SgDeclarationStatement *>((current_st))->numberOfVars())) {
                my_delete(current_st);
            }
            current_st = tmp;
        }
    }
    insert_interfaces(project);
    set<string> visited_files;
    map<string, list<string>> include_structure, copy_struct;
    set<string> visited_names;
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            string name = get_real_file_name(st);
            if (remain_files.find(name) == remain_files.end()) {
                remain_files.insert(name);
            }
        }
    }
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st; st = st->lexNext()) {
            string cur_name = get_real_file_name(st);
            if (visited_names.find(cur_name) == visited_names.end()) {
                visited_names.insert(cur_name);
                exctract_structure(include_structure, cur_name);
            }
        }
    }
    copy_struct = include_structure;
    string mp_temp = "call mpi_"; //мб странно
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (int k = 0; k < project.file(i).numberOfFunctions(); ++k) {
            for (SgStatement *st = project.file(i).functions(k); st != project.file(i).functions(k)->lastNodeOfStmt();
                 st = st->lexNext()) {
                if (!(st->variant() == GLOBAL || st->variant() == MODULE_STMT || st->variant() == PROG_HEDR ||
                      st->variant() == FUNC_HEDR || st->variant() == PROC_HEDR || st->variant() == BLOCK_DATA)) {
                    string unp = string(st->unparse());
                    if (unp.find(mp_temp) != string::npos) {
                        string *new_inc = new string("include 'mpif.h'");
                        SgStatement *tmp1 =
                            new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
                        NODE_STR(tmp1->expr(0)->thellnd) = (char *)new_inc->c_str();
                        SgStatement *scope = wh(st), *last_dec = last_declarations[scope], *tmp = scope;
                        string file_name(get_real_file_name(scope));
                        while ((tmp->variant() != VAR_DECL && tmp->variant() != PARAM_DECL && tmp != last_dec) ||
                               get_real_file_name(tmp) != file_name) {
                            tmp = tmp->lexNext();
                        }
                        if (tmp == scope) {
                            scope->insertStmtAfter(*tmp1, *scope);
                        } else {
                            tmp->insertStmtBefore(*tmp1, *scope);
                        }
                        string filename(get_real_file_name(wh(st)));
                        set_file_name(tmp1, filename);
                        break;
                    }
                }
            }
        }
    }
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        for (SgStatement *st = project.file(i).firstStatement(); st;) {
            SgStatement *tmp = st->lexNext();
            if (st->variant() == VAR_DECL) {
                if (st->expr(2)) {
                    st = tmp;
                    continue;
                }
                string *comms = nullptr;
                if (st->comments()) {
                    comms = new string(st->comments());
                    st->setComments("");
                }
                string str = trim(string(st->unparse()));
                int com_pos = 0, len = str.length();
                for (; com_pos < len && str[com_pos] != ',' && str[com_pos] != '(' && str[com_pos] != '='; com_pos++) {
                }
                com_pos--;
                while (isspace(str[com_pos])) {
                    com_pos--;
                }
                string min_str = str.substr(0, com_pos);
                long unsigned int del_pos = min_str.find_last_of(" ");
                if (del_pos == string::npos) {
                    st = tmp;
                    continue;
                }
                if (str[del_pos - 1] != ':') {
                    str.insert(del_pos, string("::"));
                    SgStatement *tmp1 =
                        new SgStatement(DATA_DECL, nullptr, nullptr, new SgExpression(STMT_STR), nullptr, nullptr);
                    string *new_inc = new string(str);
                    NODE_STR(tmp1->expr(0)->thellnd) = (char *)new_inc->c_str();
                    string his_name(get_real_file_name(st));
                    set_file_name(tmp1, his_name);
                    st->replaceWithStmt(*tmp1);
                }
                if (comms != nullptr) {
                    st->setComments(comms->c_str());
                }
            }
            if (st->variant() == EXTERN_STAT) {
                if (static_cast<SgVarListDeclStmt *>(st)->numberOfSymbols() == 0) {
                    my_delete(st);
                }
            }
            st = tmp;
        }
    }
    map<string, set<string>> well_done_files;
    for (int i = 0; i < project.numberOfFiles(); ++i) {
        SgStatement *st = project.file(i).firstStatement();
        string name = get_real_file_name(st), dum_name("");
        while (st->lexNext()) {
            st = st->lexNext();
        }
        unparse_fun(project.file(i).firstStatement(), st, name, dum_name, visited_files, include_structure,
                    copy_struct);
    }
    return 0;
}
