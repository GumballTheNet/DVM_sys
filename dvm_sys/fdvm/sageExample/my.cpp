#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <utility>
#include <cstdlib>
#include <algorithm>
#include <typeinfo>
#include <set>
#include <string>
#define IN_ANAL_
#include "../Sage/lib/include/sage++user.h"

#undef IN_ANAL_
//#include "aks_structs.h"
#include <map>
#include <vector>
using namespace std;

SgFile *f, *curent_file;
int curent_file_id;
unsigned fun_count = 0, stat_count = 0;
SgStatement *main_fun = nullptr, *bad_fun = nullptr;

SgStatement * wh(SgStatement *where);
SgStatement *to_ins = nullptr;
map<string, vector<string>> commons; 
set <string> checked;
SgLogIfStmt *createif(SgVarRefExp *sym) {
	return new SgLogIfStmt(*sym != *new SgValueExp (0), *new SgCallStmt(*(bad_fun->symbol())));
}

void createsubal(SgStatement *st, char *name, bool fl, SgStatement *now, SgVarRefExp *sym, int mod) {
	string names("func");
	names += to_string(fun_count++);
	auto ptfunc = new SgProcHedrStmt((char*)names.c_str());
	names = name;
	SgModuleSymb *cur_mod = new SgModuleSymb((char*)names.c_str());
	SgStatement *use_stmt = new SgStatement(USE_STMT, nullptr,cur_mod,nullptr,nullptr,nullptr);
	auto bad = createif(sym);
	auto k = now->lastExecutable();
	//	if (mod)k = k->lexPrev();
	//auto areturn = new SgReturnStmt();
	k->insertStmtAfter(*ptfunc);
//	ptfunc->insertStmtAfter(*areturn);
	ptfunc->insertStmtAfter(*st);
	
	st->insertStmtAfter(*bad);
	//bad->insertStmtAfter(*areturn);
	SgCallStmt *call = new SgCallStmt(*(ptfunc->symbol()));
	if (fl) {	
		auto tmp = main_fun;
		for (; tmp != main_fun->lastExecutable() && !(tmp->variant() == USE_STMT && string(tmp->symbol()->identifier()) == string(names)); tmp = tmp->lexNext()){};
		if (tmp == main_fun->lastExecutable()) {		
			main_fun->lastDeclaration()->insertStmtAfter(*use_stmt);
			use_stmt->insertStmtAfter(*call);
		} else {
			tmp->insertStmtAfter(*call);
		}
	}	else {
		main_fun->lastExecutable()->insertStmtAfter(*call);
	}
}

//auto lab = new SgGotoStmt(*(new SgLabel(10)));
void del(SgStatement *tmp, SgHeapStmt *freeh, SgVarRefExp *sym) {
	auto beg = wh(tmp)->symbol()->identifier();
	while (tmp &&  (wh(tmp)->symbol()->identifier() == beg || tmp->variant() == CONTROL_END || tmp->variant() == RETURN_NODE) ) {
		if (tmp->unparse()) {
			string un(tmp->unparse());
			un.erase(remove_if(un.begin(), un.end(),  ::isspace), un.end());
			if ((tmp->variant() == CONTROL_END || tmp->variant() == RETURN_NODE || tmp->variant() == RETURN_STAT) && (strcmp(un.substr(0,6).c_str(), "return") == 0 || un == "end" )) {
						auto bad = createif(sym);
						tmp->insertStmtBefore(*freeh);
						tmp->insertStmtBefore(*bad);
						freeh = new SgHeapStmt(DEALLOCATE_STMT, *freeh->allocationList(), *freeh->statVariable());	

			}
		}
		tmp = tmp->lexNext();

	}	
}

void Print(SgExpression *node_s, bool con_fl, SgStatement *cur, SgStatement *ifmain)
{
	auto p =  static_cast<SgExprListExp*>(node_s);
	int mod = 0;
	for (int k = 0; k < p->length(); ++k) {
	auto node = p->elem(k);
	if (node->variant() == ARRAY_REF) {
		auto c = node->symbol()->type();
		auto m = isSgArrayType(c);
		int	n = m->dimension();
		if (m->sizeInDim(0)->variant() != DDOT || (m->sizeInDim(0)->variant() == DDOT && strlen(m->sizeInDim(0)->unparse()) > 1)) {
			SgStatement *now =   wh(cur);
			if (now->variant() == FUNC_HEDR) {
				auto sym = static_cast<SgFunctionSymb*>(now->symbol());
				int i = 0;
				for (; i < sym->numberOfParameters(); ++i) {
					if (sym->parameter(i)->identifier() == node->symbol()->identifier()) {
						break;
					}
				}
				if (i != sym->numberOfParameters()) {
					return;
				}
			}
			SgExpression *cur_dim = m->sizeInDim(0);
			switch (cur_dim->variant()) {
				case INT_VAL:
				cur_dim = new SgValueExp(static_cast<SgValueExp*>(cur_dim)->intValue());
				break;
				default:
				cur_dim = new SgExpression(cur_dim->variant(),  cur_dim->lhs(), cur_dim->rhs(),cur_dim->symbol(), cur_dim->type());
				break;
			}
			SgArrayRefExp *arr = new SgArrayRefExp(*node->symbol()), *dearr = new SgArrayRefExp(*node->symbol());
			SgExprListExp *lis = new SgExprListExp(*arr), *delis = new SgExprListExp(*dearr);
			SgExprListExp *dims = new SgExprListExp(*cur_dim);
			SgType *basetype = new SgType(T_INT);
			SgVariableSymb *thisstat = new SgVariableSymb(("this_stat" + to_string(stat_count++)).c_str(), *basetype, *now);
			thisstat->declareTheSymbol(*now);
			SgVarRefExp *symexp = new SgVarRefExp(thisstat);
			for (int i = 0; i < n; i++){
				if (i){
					SgExpression *cur_dim = m->sizeInDim(i);
					switch (cur_dim->variant()) {
						case INT_VAL:
						cur_dim = new SgValueExp(static_cast<SgValueExp*>(cur_dim)->intValue());
						break;
						default:
						cur_dim = new SgExpression(cur_dim->variant(),  cur_dim->lhs(), cur_dim->rhs(),cur_dim->symbol(), cur_dim->type());
						break;
					}
					dims->append(*cur_dim);
				}
				

			}
			arr->setLhs(dims);
			SgHeapStmt *h = new SgHeapStmt( ALLOCATE_STMT, *lis, *symexp), *freeh = new SgHeapStmt(DEALLOCATE_STMT, *delis, *symexp);
			if (now->variant() == MODULE_STMT) {
				createsubal(h, now->symbol()->identifier(), true, now, symexp,mod);
				mod = 1;
				createsubal(freeh, now->symbol()->identifier(), false, now, symexp,mod);
			} else { 
				cout<<symexp->unparse()<<endl;
			auto bad =createif(symexp);

			now->lastDeclaration()->insertStmtAfter(*h);
			h->insertStmtAfter(*bad);
				del(cur, freeh, symexp);
			}
		}
	}}
	for (int k = 0; k < p->length(); ++k) {
	auto node = p->elem(k);
	if (node->variant() == ARRAY_REF) {
		auto c = node->symbol()->type();
		auto m = isSgArrayType(c);
		int	n = m->dimension();
		for (int i = 0; i < n; ++i) {
			m->sizeInDim(i)->setType(nullptr);
				m->sizeInDim(i)->setSymbol(nullptr);
				m->sizeInDim(i)->setVariant(DDOT);
				m->sizeInDim(i)->setLhs(nullptr);
				m->sizeInDim(i)->setRhs(nullptr);
			}
			node->setLhs(m->getDimList());
		}
	}
}

SgStatement * wh(SgStatement *where)
{
	SgFuncHedrStmt *fh;
	if (where->variant() == GLOBAL || where->variant() == MODULE_STMT || where->variant() == PROG_HEDR || where->variant() == PROC_HEDR) {
		return where;}
	if ((fh = isSgFuncHedrStmt(where)))
    {
		return fh;
    }
	return wh(where->controlParent());
}

void BlocktoMod(SgObjectListExp *ee, SgStatement *st, SgFile& f, int mode) {
	SgStatement *mod = new SgStatement(PROC_HEDR,nullptr,static_cast<SgSymbol*>(new SgModuleSymb(static_cast<SgExpression*>(ee)->symbol()->identifier())),nullptr,nullptr,nullptr);
	SgNestedVarListDeclStmt *tt = static_cast<SgNestedVarListDeclStmt*>(st);
	set<string> dims;
	SgNestedVarListDeclStmt* del = static_cast<SgNestedVarListDeclStmt*>(st);
	auto exp = static_cast<SgExprListExp*>(tt->list(0));
	int fl = 1;
	if(mode)
	f.firstStatement()->insertStmtAfter(*mod);
	for (int i = 0; i < exp->length(); ++i) {
		auto now = wh(st), s = now;
			do {
				if (fl)
				s = s->lexNext();
				else
				fl = 1;
				if (s->expr(0) && s->expr(0)->variant() == EXPR_LIST && (s->variant() == VAR_DECL || s->variant() == PARAM_DECL)) {
					SgExprListExp *lis = static_cast<SgExprListExp*>(s->expr(0));
					for (int j = 0; j < lis->length();++j) {
						
						int var = lis->elem(j)->variant() == ASSGN_OP && lis->elem(j)->operand(1)->symbol()->identifier() ==   exp->elem(i)->symbol()->identifier() ? 2 :
						lis->elem(j)->symbol()?lis->elem(j)->symbol()->identifier() == exp->elem(i)->symbol()->identifier() ?
						1 : 0 :0;
						if (var) {
							auto el = var == 1 ? lis->elem(j) : lis->elem(j)->operand(1) ;
							if (el->variant() == ARRAY_REF) {
								auto arr = static_cast< SgArrayType*>(el->symbol()->type());
								if (mode) {

									for (int k = 0; k < arr->dimension(); k++) {

										if(arr->sizeInDim(k)->variant() == VAR_REF ||arr->sizeInDim(k)->variant() ==CONST_REF ) {
											auto tmp = now;
											auto expre = arr->sizeInDim(k);
											if (dims.find(string(expre->symbol()->identifier())) != dims.end()) {
												continue;
											} else {
												dims.insert(string(expre->symbol()->identifier()));
											}
											int fl = 1;
											do {//для массивов декларации
												tmp = tmp->lexNext();
												if ((tmp->variant() == VAR_DECL)) {
													SgVarDeclStmt *st = static_cast<SgVarDeclStmt*>(tmp);
													for (int m = 0; m < st->numberOfSymbols() && fl;++m) {
														if (st->symbol(m)->identifier() == expre->symbol()->identifier()) {
															auto expr = new SgExprListExp(static_cast<SgExpression&>(*new SgVarRefExp(*st->symbol(m))) = *st->initialValue(m));
															mod->insertStmtAfter(*new SgVarDeclStmt(*expr, *st->type()));
															mod = mod->lexNext();
															fl = 0;
														}
													} 
												}//потом еще delete прописать
												if (tmp->variant() == PARAM_DECL) {
													SgParameterStmt *st =  static_cast<SgParameterStmt*>(tmp);
													for (int m = 0; m < st->numberOfConstants() && fl;++m) {
														if (st->constant(m)->identifier() == expre->symbol()->identifier()) {
															auto expr = new SgExprListExp(*new SgExpression(ASSGN_OP,new SgVarRefExp(*st->constant(m)),st->value(m),nullptr,nullptr));
															mod->insertStmtAfter(*new SgVarDeclStmt(*expr, *new SgType(T_FLOAT)));
															fl = 0;
														}
													} 
												}
											} while (tmp != now->lastDeclaration());
										}
									}
									SgVarDeclStmt *st = static_cast<SgVarDeclStmt*>(s);
									SgArrayRefExp *arrr = new SgArrayRefExp(*lis->elem(j)->symbol());
									SgArrayType *m = static_cast<SgArrayType*>(lis->elem(j)->symbol()->type());
									SgExprListExp *lis = new SgExprListExp(*arrr);
									SgExprListExp *dims = new SgExprListExp(*m->sizeInDim(0));
									SgExpression *cur_dim;
									for (int t = 1; t < m->dimension(); t++){
										if (t){
											SgExpression *cur_dim = m->sizeInDim(t);
											switch (cur_dim->variant()) {
												case INT_VAL:
												cur_dim = new SgValueExp(static_cast<SgValueExp*>(cur_dim)->intValue());
												break;
												default:
												cur_dim = new SgExpression(cur_dim->variant(),  cur_dim->lhs(), cur_dim->rhs(),cur_dim->symbol(), cur_dim->type());
												break;
											}
											dims->append(*cur_dim);
										}
									}
									arrr->setLhs(dims);
									cout<<dims->length()<<endl;
									auto p = var == 1 ? new SgVarDeclStmt(*lis, *st->type()) : new SgVarDeclStmt(*lis = *st->completeInitialValue((j)), *st->type());
									mod->lastDeclaration()->insertStmtAfter(*p);
								} 
								SgVarDeclStmt *st = static_cast<SgVarDeclStmt*>(s);
								st->deleteSymbol(j ? j + 1 : j );
								if (!st->numberOfSymbols()) {
										s = s->lexNext();
										st->extractStmt();
										fl = 0;break;
								}
							
							} else {
								if (s->variant() == VAR_DECL) {
									SgVarDeclStmt *st = static_cast<SgVarDeclStmt*>(s);
									if (mode) {
										auto expr = var == 1 ? new SgExprListExp((*new SgVarRefExp(*st->symbol(j)))) :new SgExprListExp((*st->completeInitialValue((j))))  ;
										SgStatement* p = new SgVarDeclStmt(*expr, *st->type());
										mod->getScopeForDeclare()->insertStmtAfter(*p);
									}
									st->deleteSymbol(j ? j + 1 : j);
									if (!st->numberOfSymbols()) {
										s = s->lexNext();
										st->extractStmt();
										fl = 0;break;
									}
								}
								if (s->variant() == PARAM_DECL) {
									SgParameterStmt *st = static_cast<SgParameterStmt*>(s);
									if (mode){
										auto expr = new SgExprListExp(static_cast<SgExpression&>(*new SgVarRefExp(*st->constant(j))) = *st->value(j));
										mod->lastDeclaration()->insertStmtAfter(*new SgVarDeclStmt(*expr, *new SgType(T_FLOAT)));
									}
									st->deleteConstant(j);
									if (!st->numberOfConstants()) {
										s = s->lexNext();
										st->extractStmt();
										fl = 0;break;
									}
								}
								dims.insert(string(exp->elem(i)->symbol()->identifier()));
							}
							
						}	
						}
					}
						
			} while(s != now->lastDeclaration());
		}
		if (mode){
			mod->setVariant(MODULE_STMT);
			auto k = new  SgStatement(USE_STMT, nullptr, mod->symbol(), nullptr,nullptr,nullptr);
			for (int i = 0; i < 1; ++i) {
				auto y = static_cast<SgExprListExp*>( del->list(i));
				for (int j = 0; j < y->length();++j) {
					commons[string(mod->symbol()->identifier())].push_back(y->elem(j)->symbol()->identifier());
				}
				del->deleteList(i);
			}
			auto m = static_cast<SgStatement*>(del);
		//	m->setVariant(USE_STMT);
		//	m->setSymbol(*new SgModuleSymb(mod->symbol()->identifier()));
			wh(st)->lastDeclaration()->insertStmtAfter(*k);
			m->deleteStmt();

		} 
		else {
			string module_name = string(static_cast<SgExpression*>(ee)->symbol()->identifier());
			vector<string> cur;
			for (int i = 0; i < commons[module_name].size();++i) {
				cur.push_back(string(static_cast<SgExprListExp*>(tt->list(0))->elem(i)->symbol()->identifier()));
			}
			tt->deleteList(0);
			auto m = static_cast<SgStatement*>(st);
			//m->setVariant(USE_STMT);
			//m->setSymbol(*new SgModuleSymb((char*)module_name.c_str()));
			SgExprListExp y(*new SgExpression(RENAME_NODE,new SgVarRefExp(*new SgVariableSymb( (char*)commons[module_name][0].c_str())),new SgVarRefExp(*new SgVariableSymb((char*)cur[0].c_str())),nullptr));
			for (int i = 1; i < commons[module_name].size();++i) {
				y.append(*new SgExpression(RENAME_NODE,new SgVarRefExp(*new SgVariableSymb( (char*)commons[module_name][i].c_str())),new SgVarRefExp(*new SgVariableSymb((char*)cur[i].c_str())),nullptr));
			}
			auto k = new  SgStatement(USE_STMT, nullptr, mod->symbol(), nullptr,nullptr,nullptr);
			k->setExpression(0,y);
			wh(st)->lastDeclaration()->insertStmtAfter(*k);

			m->deleteStmt();
	}

}


				
int main(int argc, char **argv)
{
 	SgProject project("dvm.proj");
 	int main_file = 0;
 	for (int i = 0; i < project.numberOfFiles(); ++i) {
		if (project.file(i).mainProgram()) {
			if (main_fun && project.file(i).mainProgram() != main_fun) {
				cout<<"ERROR";
				return 1;
			}
			main_file = i;
			
			main_fun = project.file(i).mainProgram();
			to_ins = main_fun->lastDeclaration();
			auto s = main_fun;
			int fl = 1;
			do{
				if (fl)
				s = s->lexNext();
				fl = 1;
				if(s->expr(0) && s->expr(0)->variant() == COMM_LIST ) {
					if (checked.find(string(s->expr(0)->symbol()->identifier())) == checked.end()) {
						auto tmp = s->lexNext();
						checked.insert(string(s->expr(0)->symbol()->identifier()));
						BlocktoMod(static_cast<SgObjectListExp*>(s->expr(0)), s,project.file(i),1);
						s = tmp;
						fl = 0;
					}
				}
			}while (s != main_fun->lastNodeOfStmt());
		}	
	}
	if (!main_fun) {
		cout<<"ERROR"<<endl;
		return 1;
	}
	for (int i = 0; i < project.numberOfFiles(); ++i) {
		for (auto s = project.file(i).firstStatement();s;){
			auto tmp = s->lexNext();
			if(s->expr(0) && s->expr(0)->variant() == COMM_LIST ) {
				if (checked.find(string(s->expr(0)->symbol()->identifier())) == checked.end()) {
					checked.insert(string(s->expr(0)->symbol()->identifier()));
					BlocktoMod(static_cast<SgObjectListExp*>(s->expr(0)), s,project.file(i),1);

				} else {
					BlocktoMod(static_cast<SgObjectListExp*>(s->expr(0)), s,project.file(i),0);
				}
			}
			s = tmp;

		}
	}

		string name("bad_fun");
			bad_fun = new SgProcHedrStmt((char*)name.c_str());
			auto m = new SgInputOutputStmt(PRINT_STAT,*new SgVarRefExp(*new SgVariableSymb("*",*new SgType(T_INT))),*new SgValueExp("MOYA OBORONA"));
			project.file(main_file).firstStatement()->insertStmtAfter(*bad_fun);
			bad_fun->insertStmtAfter(*m);
		for (int i = 0; i < project.numberOfFiles(); ++i) {
			SgStatement *main = nullptr;
			f = &(project.file(i));			
			SgStatement * cur1 = nullptr, *cur2 = nullptr;
			for (auto s = f->firstStatement(); s; s = s->lexNext()) {
				
					if (s->expr(0) && s->expr(0)->variant() == EXPR_LIST && (s->variant() == VAR_DECL || s->variant() == PARAM_DECL)) {
						Print(s->expr(0),s->variant() == VAR_DECL,s, main);
					} 
			}
		}	

		for (int i = 0; i < project.numberOfFiles(); ++i) {
			FILE *fp = fopen(("temp" + to_string(i) + ".f").c_str(),"w+");
			f = &(project.file(i));			
			f->unparse(fp);
		}
	return 0;
}

