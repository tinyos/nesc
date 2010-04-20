/* This file is part of the nesC compiler.

This file is derived from the RC and the GNU C Compiler. It is thus
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
   Copyright (C) 2002 Intel Corporation

The attached "nesC" software is provided to you under the terms and
conditions of the GNU General Public License Version 2 as published by the
Free Software Foundation.

nesC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with nesC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

/* This is a version of c-parse.y with two conflicts and supporting the gcc3
   attribute syntax. It is a partial merge of the gcc 3 grammar */

/* This file defines the grammar of C */
/* To whomever it may concern: I have heard that such a thing was once
   written by AT&T, but I have never seen it.  */

%pure_parser
%expect 12

%{
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#include "parser.h"
#include "c-parse.h"
#include "c-lex.h"
#include "c-lex-int.h"
#include "semantics.h"
#include "expr.h"
#include "stmt.h"
#include "init.h"
#include "nesc-semantics.h"
#include "nesc-interface.h"
#include "nesc-component.h"
#include "nesc-configuration.h"
#include "nesc-module.h"
#include "nesc-env.h"
#include "nesc-c.h"
#include "nesc-attributes.h"
#include "nesc-task.h"
#include "nesc-cpp.h"
#include "attributes.h"
#include "machine.h"

int yyparse(void) deletes;

void yyerror();

/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1
%}

%start dispatch

/* All identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER

/* All identifiers that are declared typedefs in the current block.
   In some contexts, they are treated just like IDENTIFIER,
   but they can also serve as typespecs in declarations.  */
%token TYPENAME

/* An identifier that is declared as a component reference in the
   current block, and which is going to be used to refer to a typedef
   from the component via the component-ref '.' identifier syntax
   (detected in the lexer) */
%token COMPONENTREF

/* Reserved words that specify storage class.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token <u.itoken> SCSPEC

/* Reserved words that specify type.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token <u.itoken> TYPESPEC

/* Reserved words that qualify types/functions: "const" or "volatile", 
   "deletes".
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token <u.itoken> TYPE_QUAL FN_QUAL

/* Character or numeric constants.
   yylval is the node for the constant.  */
%token CONSTANT

/* String constants in raw form. */
%token STRING MAGIC_STRING

/* "...", used for functions with variable arglists.  */
%token <u.itoken> ELLIPSIS

/* the reserved words */
/* SCO include files test "ASM", so use something else. */
%token <u.itoken> SIZEOF ENUM STRUCT UNION IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token <u.itoken> BREAK CONTINUE RETURN GOTO ASM_KEYWORD TYPEOF ALIGNOF
%token <u.itoken> ATTRIBUTE EXTENSION LABEL
%token <u.itoken> REALPART IMAGPART VA_ARG OFFSETOF

/* Add precedence rules to solve dangling else s/r conflict */
%nonassoc IF
%nonassoc ELSE

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */

%right <u.itoken> ASSIGN '='
%right <u.itoken> '?' ':'
%left <u.itoken> OROR
%left <u.itoken> ANDAND
%left <u.itoken> '|'
%left <u.itoken> '^'
%left <u.itoken> '&'
%left <u.itoken> EQCOMPARE
%left <u.itoken> ARITHCOMPARE '<' '>'
%left <u.itoken> LSHIFT RSHIFT
%left <u.itoken> '+' '-'
%left <u.itoken> '*' '/' '%'
%right <u.itoken> PLUSPLUS MINUSMINUS
%left <u.itoken> POINTSAT '.' '(' '['

%type <u.asm_operand> asm_operand asm_operands nonnull_asm_operands
%type <u.asm_stmt> maybeasm
%type <u.attribute> maybe_attribute attributes attribute attribute_list
%type <u.attribute> nesc_attributes nattrib 
%type <u.gcc_attribute> attrib target_attribute
%type <u.nesc_attribute> nastart
%type <u.constant> CONSTANT
%type <u.decl> datadecl datadecls datadef decl decls extdef extdefs fndef
%type <u.decl> initdecls initdecls_ notype_initdecls notype_initdecls_ fndef2
%type <u.decl> nested_function notype_nested_function old_style_parm_decls
%type <u.decl> initdcl component_decl_list component_decl_list2 component_decl
%type <u.decl> components component_declarator enumerator enumlist
%type <u.decl> components_notype component_notype_declarator 
%type <u.decl> parmlist parmlist_1 parmlist_2 parms parm
%type <u.decl> parmlist_or_identifiers identifiers notype_initdcl
%type <u.decl> parmlist_or_identifiers_1 old_parameter just_datadef
%type <u.declarator> declarator after_type_declarator notype_declarator
%type <u.declarator> absdcl absdcl1 absdcl1_noea absdcl1_ea direct_absdcl1
%type <u.declarator> parm_declarator 
%type <u.nested> array_declarator fn_declarator array_or_fn_declarator
%type <u.nested> absfn_declarator array_or_absfn_declarator
%type <u.expr> cast_expr expr expr_no_commas exprlist init initlist_maybe_comma
%type <u.expr> initlist1 initelt nonnull_exprlist primary string
%type <u.expr> nonnull_exprlist_ initval restricted_expr
%type <u.designator> designator_list designator
%type <u.expr> unary_expr xexpr function_call
%type <u.expr> generic_type typelist
%type <u.id_label> id_label maybe_label_decls label_decls label_decl
%type <u.id_label> identifiers_or_typenames
%type <idtoken> identifier type_parm
%type <idtoken> IDENTIFIER TYPENAME MAGIC_STRING COMPONENTREF
%type <u.iexpr> if_prefix
%type <u.istmt> stmt_or_labels simple_if stmt_or_label
%type <u.itoken> unop extension '~' '!' compstmt_start '{' ';'
%type <u.itoken> sizeof alignof
%type <u.label> label
%type <u.stmt> stmts xstmts compstmt_or_error compstmt
%type <u.stmt> labeled_stmt stmt stmt_or_error atomic_stmt
%type <u.cstmt> do_stmt_start
%type <u.string> asm_clobbers STRING
%type <u.telement> declspecs_nosc_nots_nosa_noea
%type <u.telement> declspecs_nosc_nots_nosa_ea
%type <u.telement> declspecs_nosc_nots_sa_noea
%type <u.telement> declspecs_nosc_nots_sa_ea
%type <u.telement> declspecs_nosc_ts_nosa_noea
%type <u.telement> declspecs_nosc_ts_nosa_ea
%type <u.telement> declspecs_nosc_ts_sa_noea
%type <u.telement> declspecs_nosc_ts_sa_ea
%type <u.telement> declspecs_sc_nots_nosa_noea
%type <u.telement> declspecs_sc_nots_nosa_ea
%type <u.telement> declspecs_sc_nots_sa_noea
%type <u.telement> declspecs_sc_nots_sa_ea
%type <u.telement> declspecs_sc_ts_nosa_noea
%type <u.telement> declspecs_sc_ts_nosa_ea
%type <u.telement> declspecs_sc_ts_sa_noea
%type <u.telement> declspecs_sc_ts_sa_ea
%type <u.telement> declspecs_ts
%type <u.telement> declspecs_nots
%type <u.telement> declspecs_ts_nosa
%type <u.telement> declspecs_nots_nosa
%type <u.telement> declspecs_nosc_ts
%type <u.telement> declspecs_nosc_nots
%type <u.telement> declspecs_nosc
%type <u.telement> declspecs
%type <u.telement> scspec type_qual type_spec eattributes
%type <u.telement> type_spec_attr type_spec_nonattr
%type <u.telement> type_spec_nonreserved_nonattr type_spec_reserved_attr
%type <u.telement> type_spec_reserved_nonattr
%type <u.telement> structdef structuse
%type <u.telement> maybe_type_qual maybe_type_quals_attrs fn_qual fn_quals
%type <u.type> typename
%type <u.word> idword any_word tag
%type <u.fields> fieldlist
%type <u.itoken> structkind

/* the dispatching (fake) tokens */
%token <u.itoken> DISPATCH_C DISPATCH_NESC DISPATCH_PARM DISPATCH_TYPE

/* nesC reserved words */
%token <u.itoken> ATOMIC USES INTERFACE COMPONENTS PROVIDES MODULE 
%token <u.itoken> INCLUDES CONFIGURATION AS TASTNIOP IMPLEMENTATION CALL 
%token <u.itoken> SIGNAL POST GENERIC NEW NX_STRUCT NX_UNION
/* words reserved for nesC's future. Some may never be used... */
%token <u.itoken> ABSTRACT COMPONENT EXTENDS
%token <idtoken> TARGET_ATTRIBUTE0 TARGET_ATTRIBUTE1 TARGET_DEF

%type <u.itoken> callkind
%type <u.decl> datadef_list 
%type <u.decl> parameters parameters1
%type <u.decl> requires provides requires_or_provides requires_or_provides_list
%type <u.decl> requires_or_provides_list_
%type <u.decl> parameterised_interface_list parameterised_interface
%type <u.decl> parameterised_interfaces 
%type <u.decl> interface_parms interface_parm_list interface_parm
%type <u.decl> component_parms 
%type <u.decl> template_parms template_parmlist template_parm
%type <u.decl> target_def
%type <u.iref> interface_ref interface_type
%type <u.cref> component_ref component_ref2 component_list cuses
%type <u.conn> connection
%type <u.decl> configuration_decl configuration_decls
%type <u.ep> endpoint
%type <u.pid> parameterised_identifier
%type <u.impl> iconfiguration imodule
%type <abstract> generic
%type <u.expr> generic_arglist generic_arg generic_args


%{
/* Region in which to allocate parse structures. Idea: the AST user can set
   this to different regions at appropriate junctures depending on what's
   being done with the AST */
region parse_region;
/* We'll see this a LOT below */
#define pr parse_region

/* Number of statements (loosely speaking) and compound statements 
   seen so far.  */
static int stmt_count;
static int compstmt_count;
  
#ifdef RC_ADJUST
static size_t rc_adjust_yystype(void *x, int by) 
{
  struct yystype *p = x;
  RC_ADJUST_PREAMBLE;

  RC_ADJUST(p->u.ptr, by);
  RC_ADJUST(p->idtoken.location.filename, by);
  RC_ADJUST(p->idtoken.id.data, by);
  RC_ADJUST(p->idtoken.decl, by);

  return sizeof *p;
}

static void rc_update_yystype(struct yystype *old, struct yystype *new)
{
  regionid base = regionidof(old);

  RC_UPDATE(base, old->u.ptr, new->u.ptr);
  RC_UPDATE(base, old->idtoken.location.filename, new->idtoken.location.filename);
  RC_UPDATE(base, old->idtoken.id.data, new->idtoken.id.data);
  RC_UPDATE(base, old->idtoken.decl, new->idtoken.decl);
}
#endif

/* A stack of declspecs and attributes for use during parsing */
typedef struct spec_stack *spec_stack;
struct spec_stack { 
  type_element parentptr declspecs;
  attribute parentptr attributes;
  spec_stack sameregion next;
};

struct parse_state 
{
  /* Stack of saved values of current_declspecs and prefix_attributes.  */
  /* In an ideal world, we would be able to eliminate most rc ops for
     declspec_stack and ds_region assignments. Seems tricky though. */
  spec_stack declspec_stack;
  region ds_region;

  /* List of types and structure classes of the current declaration.  */
  type_element declspecs;
  attribute attributes;

  /* >0 if currently parsing an expression that will not be evaluated (argument
     to alignof, sizeof. Currently not typeof though that could be considered
     a bug) */
  int unevaluated_expression;
} pstate;

bool unevaluated_expression(void)
{
  return pstate.unevaluated_expression != 0;
}

/* Pop top entry of declspec_stack back into current_declspecs,
   prefix_attributes */
static void pop_declspec_stack(void) deletes
{
  pstate.declspecs = pstate.declspec_stack->declspecs;
  pstate.attributes = pstate.declspec_stack->attributes;
  pstate.declspec_stack = pstate.declspec_stack->next;
}

static void push_declspec_stack(void)
{
  spec_stack news;

  news = ralloc(pstate.ds_region, struct spec_stack);
  news->declspecs = pstate.declspecs;
  news->attributes = pstate.attributes;
  news->next = pstate.declspec_stack;
  pstate.declspec_stack = news;
}

static node parse_tree;

node parse(void) deletes
{
  int result, old_errorcount = errorcount;
  struct parse_state old_pstate = pstate;

  pstate.declspecs = NULL;
  pstate.attributes = NULL;
  pstate.unevaluated_expression = 0;
  pstate.declspec_stack = NULL;
  pstate.ds_region = newsubregion(parse_region);
  parse_tree = NULL;
  result = yyparse();
  if (result)
    parse_tree = NULL;
  deleteregion_ptr(&pstate.ds_region);

  if (result != 0 && errorcount == old_errorcount)
    fprintf(stderr, "Errors detected in input file (your bison.simple is out of date)");

  pstate = old_pstate;

  return parse_tree;
}

static void set_nesc_ast(void *tree)
{
  nesc_declaration cdecl = current.container;
  nesc_decl nd = CAST(nesc_decl, tree);

  nd->cdecl = cdecl;
  cdecl->ast = nd;
}

static void set_nesc_parse_tree(void *tree)
{
  set_nesc_ast(tree);
  parse_tree = CAST(node, tree);
}

static void set_nesc_impl(implementation impl)
{
  nesc_declaration cdecl = current.container;

  CAST(component, cdecl->ast)->implementation = impl;
  parse_tree = CAST(node, cdecl->ast);
}

void refuse_asm(asm_stmt s)
{
  if (s)
    error_with_location(s->location, "unexpected asm statement");
}

/* Merge the attributes in front of a declaration (but which aren't part
   of the declspecs) with the attributes after the declaration.
   We're pretending they all came after */
attribute prefix_attr(attribute post_attr)
{
  return attribute_chain(pstate.attributes, post_attr);
}

/* Simple build functions */
declaration make_data_decl(type_element modifiers, declaration decls)
{
  location l = modifiers ? modifiers->location : decls->location;

  data_decl dd = new_data_decl(parse_region, l, modifiers, decls);

  pop_declspec_stack();

  if (decls == NULL && current.spec_section != spec_normal)
    error("provides/uses must be followed by a command, event or interface");

  return CAST(declaration, dd);
}

declaration make_error_decl(void)
{
  return new_error_decl(pr, dummy_location);
}

declaration make_extension_decl(int old_pedantic, location l, declaration d)
{ 
  pedantic = old_pedantic; 
  return CAST(declaration, new_extension_decl(pr, l, d));
}

word make_cword(location l, const char *s)
{
  return new_word(pr, l, str2cstring(pr, s));
}

declarator make_qualified_declarator(location l, declarator d, type_element quals)
{
  if (quals)
    return CAST(declarator, new_qualified_declarator(pr, l, d, quals));
  else
    return d;
}

declarator make_pointer_declarator(location l, declarator d, type_element quals)
{
  d = make_qualified_declarator(l, d, quals);

  return CAST(declarator, new_pointer_declarator(pr, l, d));
}

declarator make_identifier_declarator(location l, cstring id)
{
  return CAST(declarator, new_identifier_declarator(pr, l, id));
}

statement make_error_stmt(void)
{
  return new_error_stmt(pr, dummy_location);
}

/* Tell yyparse how to print a token's value, if yydebug is set.  */

#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)
void yyprint();
%}

%%

dispatch:
	  DISPATCH_NESC interface { }
	| DISPATCH_NESC component { }
	| DISPATCH_C extdefs {
	    declaration cdecls = declaration_reverse($2); 
	    parse_tree = CAST(node, cdecls); }
	| DISPATCH_C { parse_tree = NULL; }
	| DISPATCH_PARM parm { parse_tree = CAST(node, $2); }
	| DISPATCH_PARM error { parse_tree = CAST(node, make_error_decl()); }
	| DISPATCH_TYPE typename { parse_tree = CAST(node, $2); }
	| DISPATCH_TYPE error { parse_tree = NULL; }
	;

ncheader:
	  { end_macro_saving(); } includes_list
	| extdefs 
		  { 
		    end_macro_saving(); 
		    add_cdecls(declaration_reverse($1));
		  }
	;

includes_list: 
	  includes_list includes 
	| /* empty */
	;

includes:
	  INCLUDES include_list ';' { }
	;

include_list:
	  identifier 
		{ require_c($1.location, $1.id.data); }
	| include_list ',' identifier 
		{ require_c($3.location, $3.id.data); }
	;

interface: 
          ncheader
	  INTERFACE idword
		{ 
		  start_nesc_entity(l_interface, $3);
		} 
	  interface_parms nesc_attributes 
		{
		  handle_nescdecl_attributes($6, current.container);
		}
	  '{' datadef_list '}' 
		{
		  interface intf = new_interface(pr, $2.location, $3, $6, declaration_reverse($9));

		  set_nesc_parse_tree(intf);

		  if (intf->cdecl->abstract)
		    poplevel();
		}
	;

interface_parms:
	  /* empty */ { $$ = NULL; }
	| '<' interface_parm_list '>' 
		{
		  nesc_declaration intf = current.container;

		  intf->parameters = $2;
		  intf->parameter_env = current.env;
		  $$ = $2;

		  /* Template intfs need a new level for the actual intf */
		  pushlevel(FALSE);
		  /* The interface env counts as global */
		  current.env->global_level = TRUE;
		  intf->env = current.env;
		  intf->abstract = TRUE;
		}
	;

interface_parm_list:
	  interface_parm
	| interface_parm_list ',' interface_parm
		{ $$ = declaration_chain($1, $3); }
	;

interface_parm:
	  type_parm nesc_attributes 
		{ $$ = declare_type_parameter($1.location, $1.id, $2, NULL); }
	;

type_parm:
	  IDENTIFIER { $$ = $1; }
	;

datadef_list: 
	  datadef_list just_datadef { $$ = declaration_chain($2, $1); }
	| just_datadef ;

parameters: 
	  '[' { pushlevel(TRUE); } parameters1 
		{ /* poplevel done in users of parameters */ $$ = $3; } ;

parameters1: 
	  parms ']'
	  	{
		  $$ = declaration_reverse($1);
		  check_interface_parameter_types($$); 
		}
	| error ']' { $$ = make_error_decl(); }
	;

component: 
	  ncheader module
	| ncheader configuration
	| ncheader binary_component
	;

module: 
	  generic MODULE idword 
	        { 
		  start_nesc_entity(l_component, $3);
		  current.container->abstract = $1; 
		} 
	  component_parms nesc_attributes 
		{
		  handle_nescdecl_attributes($6, current.container);
		}
	  '{' requires_or_provides_list '}'
	  imodule
		{
		  declaration intfs = 
		    declaration_chain(declaration_reverse($9), all_tasks);
		  set_nesc_parse_tree(new_component(pr, $2.location, $3, $6, $1, $5, intfs, $11));
	        }
	;

configuration:
	  generic CONFIGURATION idword
		{ 
		  start_nesc_entity(l_component, $3);
		  current.container->abstract = $1; 
		  current.container->configuration = TRUE;
		} 
	  component_parms nesc_attributes 
		{
		  handle_nescdecl_attributes($6, current.container);
		}
	  '{' requires_or_provides_list '}'
		{
		  set_nesc_ast(new_component(pr, $2.location, $3, $6, $1, $5, declaration_reverse($9), NULL));
		}
	  iconfiguration
		{
		  set_nesc_impl($12);
	        }
        ;

binary_component: 
	  COMPONENT idword
	        { 
		  start_nesc_entity(l_component, $2);
		} 
	  nesc_attributes 
		{
		  handle_nescdecl_attributes($4, current.container);
		}
	  '{' requires_or_provides_list '}'
		{ 
		  binary_component dummy = new_binary_component(pr, $1.location, start_implementation());
		  component c = new_component(pr, $1.location, $2, $4, FALSE, NULL, declaration_reverse($7), CAST(implementation, dummy));
		  set_nesc_parse_tree(c);
	        }
	;

generic: GENERIC { $$ = TRUE; }
	| /* empty */ { $$ = FALSE; }
	;

component_parms:
	  /* empty */
		{
		  if (current.container->abstract)
		    error("generic components require a parameter list");
		    /* We don't create the extra environment level for this
		       generic component as nothing actually requires its 
		       existence */
		  $$ = NULL;
		}
	| '(' template_parms ')'
		{
		  nesc_declaration comp = current.container;

		  if (!comp->abstract)
		    error("only generic components can have a parameter list");
		  comp->parameters = $2;
		  comp->parameter_env = current.env;
		  $$ = $2;

		  /* generic components need a new level for the 
		     specification */
		  pushlevel(FALSE);
		  current.env->global_level = TRUE;
		  comp->env = current.env;
		}
	;

template_parms:
	/* empty */ { $$ = NULL; }
      | template_parmlist
      ;

template_parmlist:
	  template_parm
	| template_parmlist ',' template_parm
		{ $$ = declaration_chain($1, $3); }
	;

/* A declaration of a template parameter, i.e., a regular
   parameter-like declaration (name required).
   The 'typedef t' syntax for declaring a type argument is detected
   inside declare_template_parameter */
template_parm:
	  declspecs_ts xreferror after_type_declarator maybe_attribute
		{ $$ = declare_template_parameter($3, $1, $4); }
	| declspecs_ts xreferror notype_declarator maybe_attribute
		{ $$ = declare_template_parameter($3, $1, $4); }
	| declspecs_nots xreferror notype_declarator maybe_attribute
		{ $$ = declare_template_parameter($3, $1, $4); }
	| declspecs_ts xreferror
		{ $$ = declare_template_parameter(NULL, $1, NULL); }
	;

requires_or_provides_list: 
	  requires_or_provides_list_
		{ current.spec_section = spec_normal; }
	;

requires_or_provides_list_: 
	  requires_or_provides_list_ requires_or_provides
		{ $$ = declaration_chain($2, $1); }
	| /* empty */ { $$ = NULL; }
	;

requires_or_provides: 
	  requires 
	| provides 
	| { current.spec_section = spec_normal; } just_datadef { $$ = $2; }
	;

requires: 
	  USES { current.spec_section = spec_uses; } 
	  parameterised_interface_list 
		{ $$ = CAST(declaration, new_rp_interface(pr, $1.location, TRUE, declaration_reverse($3))); } ;

provides: 
	  PROVIDES { current.spec_section = spec_provides; } 
	  parameterised_interface_list 
		{ $$ = CAST(declaration, new_rp_interface(pr, $1.location, FALSE, declaration_reverse($3))); } ;

parameterised_interface_list:
	  parameterised_interface
	| '{' parameterised_interfaces '}' { $$ = $2; }
	;

parameterised_interfaces: 
	  parameterised_interfaces parameterised_interface 
		{ $$ = declaration_chain($2, $1); }
	| parameterised_interface
	;

parameterised_interface:
	  just_datadef
	| interface_ref nesc_attributes ';' 
		{
		  declare_interface_ref($1, NULL, current.env, $2);
		  $$ = CAST(declaration, $1);
		}
	| interface_ref parameters nesc_attributes ';'
		{ 
		  $1->gparms = $2;
		  poplevel();
		  declare_interface_ref($1, $2, current.env, $3);
		  $$ = CAST(declaration, $1);
		}
	;

interface_ref: 
	  interface_type
	| interface_type AS idword { $$ = $1; $$->word2 = $3; }
	;

interface_type:
	  INTERFACE idword
		{ 
		  preload(l_interface, $1.location, $2->cstring.data);
		  $$ = new_interface_ref(pr, $1.location, $2, NULL, NULL, NULL, NULL, NULL); 
		}
	| INTERFACE idword 
		{ 
		  preload(l_interface, $1.location, $2->cstring.data);
		}
	  '<' typelist '>'
		{ $$ = new_interface_ref(pr, $1.location, $2, $5, NULL, NULL, NULL, NULL); }
	;

typelist:
	  generic_type
	| typelist ',' generic_type { $$ = expression_chain($1, $3); }
	;

iconfiguration:
	  IMPLEMENTATION { $<u.env>$ = start_implementation(); } 
	  '{'
	  configuration_decls
	  '}'
		{ $$ = CAST(implementation, new_configuration(pr, $1.location, $<u.env>2, declaration_reverse($4)));
		}
	;

cuses:    COMPONENTS component_list ';' { $$ = $2; }
	;

component_list: 
	  component_list ',' component_ref { $$ = component_ref_chain($3, $1); }
	| component_ref
	;

component_ref: 
	  component_ref2 { $$ = require_component($1, NULL); }
	| component_ref2 AS idword { $$ = require_component($1, $3); }
	;

component_ref2: 
	  idword { $$ = new_component_ref(pr, $1->location, $1, NULL,
					  FALSE, NULL); }
	| NEW idword '(' generic_args ')'
		 { $$ = new_component_ref(pr, $1.location, $2, NULL,
					  TRUE, $4); }
	;

generic_args:
	  /* empty */ { $$ = NULL; }
	| generic_arglist
	;

generic_arglist:
	  generic_arg
	| generic_arglist ',' generic_arg { $$ = expression_chain($1, $3); }
	;

generic_arg:
	  expr_no_commas 
		{ $$ = $1; $$->type = default_conversion_for_assignment($$); }
	| generic_type
	;

generic_type:
	  typename { $$ = make_type_argument($1); }
	;

configuration_decls: 
	  configuration_decls configuration_decl { $$ = declaration_chain($2, $1); }
	| /* empty */ { $$ = NULL; }
	;

configuration_decl:
	  connection { $$ = CAST(declaration, $1); }
	| just_datadef
	| cuses { $$ = CAST(declaration, $1); }
	;

connection:
	  endpoint '=' endpoint ';' 
		{ $$ = CAST(connection, new_eq_connection(pr, $2.location, $1, $3)); }
	| endpoint POINTSAT endpoint ';' 
		{ $$ = CAST(connection, new_rp_connection(pr, $2.location, $3, $1)); }
	| endpoint TASTNIOP endpoint ';'
		{ $$ = CAST(connection, new_rp_connection(pr, $2.location, $1, $3)); }
	;

endpoint: 
	  endpoint '.' parameterised_identifier
		{ $$ = $1;
		  $$->ids = parameterised_identifier_chain($$->ids, $3);
		}
	| parameterised_identifier 
		{ $$ = new_endpoint(parse_region, $1->location, $1); }
	;

parameterised_identifier:
	  idword 
	  { $$ = new_parameterised_identifier(pr, $1->location, $1, NULL); }
	| idword '[' nonnull_exprlist ']'
	  { $$ = new_parameterised_identifier(pr, $1->location, $1, $3); }
	;

imodule:  IMPLEMENTATION { $<u.env>$ = start_implementation(); all_tasks = NULL; } '{' extdefs '}' 
		{ 
		  $$ = CAST(implementation, new_module(pr, $1.location, $<u.env>2, declaration_reverse($4))); 
		} ;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0. */

extdefs:
	  { $<u.telement>$ = NULL; } extdef { $$ = $2; }
	| extdefs { $<u.telement>$ = NULL; } extdef
		{ $$ = declaration_chain($3, $1); }	  
	;

extdef:
	  fndef
	| datadef
	| ASM_KEYWORD '(' expr ')' ';'
		{ 
		  $$ = CAST(declaration, new_asm_decl
		    (pr, $1.location,
		     new_asm_stmt(pr, $1.location, $3, NULL, NULL, NULL, NULL))); }
	| extension extdef
		{ $$ = make_extension_decl($1.i, $1.location, $2); }
	;

datadef:
	  setspecs notype_initdecls ';'
		{ if (pedantic)
		    error("ANSI C forbids data definition with no type or storage class");
		  else if (!flag_traditional)
		    warning("data definition has no type or storage class"); 

		  $$ = make_data_decl(NULL, $2); }
	| just_datadef
	;

just_datadef:
          declspecs_nots setspecs notype_initdecls ';'
		{ $$ = make_data_decl($1, $3); }
	| declspecs_ts setspecs initdecls ';'
		{ $$ = make_data_decl($1, $3); }
	| declspecs setspecs ';'
	  	{ shadow_tag($1); 
	    	  $$ = make_data_decl($1, NULL); }
	| error ';' { $$ = make_error_decl(); }
	| error '}' { $$ = make_error_decl(); }
	| ';'
		{ if (pedantic)
		    pedwarn("ANSI C does not allow extra `;' outside of a function");
		  $$ = NULL; }
	| target_def
	;

target_def:
	  TARGET_DEF identifier '=' expr ';'
		  { $$ = target->keilc_definition($1.location, $1.id, $2.id, $4); }
	;

fndef:
	  declspecs_ts setspecs declarator fndef2 { $$ = $4; }
	| declspecs_nots setspecs notype_declarator fndef2 { $$ = $4; }
	| setspecs notype_declarator fndef2 { $$ = $3; }
	;

fndef2:	   maybeasm maybe_attribute
		{ 
		  /* maybeasm is only here to avoid a s/r conflict */
		  refuse_asm($1);

		  /* $0 refers to the declarator that precedes fndef2
		     in fndef (we can't just save it in an action, as that
		     causes s/r and r/r conflicts) */
		  if (!start_function(pstate.declspecs, $<u.declarator>0, $2, 0))
		    YYERROR1; 
		}
	  old_style_parm_decls
		{ store_parm_decls(declaration_reverse($4)); }
	  compstmt_or_error
		{ $$ = finish_function($6);
		  pop_declspec_stack(); }
	;

identifier:
	  IDENTIFIER
	| TYPENAME
	;

id_label:
	  identifier { $$ = new_id_label(pr, $1.location, $1.id); }
	;

idword:
	  identifier { $$ = new_word(pr, $1.location, $1.id); }
        ;

unop:     '&'
		{ $$ = $1; $$.i = kind_address_of; }
	| '-'
		{ $$ = $1; $$.i = kind_unary_minus; }
	| '+'
		{ $$ = $1; $$.i = kind_unary_plus; }
	| PLUSPLUS
		{ $$ = $1; $$.i = kind_preincrement; }
	| MINUSMINUS
		{ $$ = $1; $$.i = kind_predecrement; }
	| '~'
		{ $$ = $1; $$.i = kind_bitnot; }
	| '!'
		{ $$ = $1; $$.i = kind_not; }
	| REALPART
		{ $$ = $1; $$.i = kind_realpart; }
	| IMAGPART
		{ $$ = $1; $$.i = kind_imagpart; }
	;

expr:	nonnull_exprlist
		{ if ($1->next)
		    $$ = make_comma($1->location, $1);
		  else
		    $$ = $1; }
	;

exprlist:
	  /* empty */
		{ $$ = NULL; }
	| nonnull_exprlist
	;

nonnull_exprlist:
	  nonnull_exprlist_
		{ $$ = expression_reverse($1); }
	;

nonnull_exprlist_:
	  expr_no_commas
		{ $$ = $1; }
	| nonnull_exprlist_ ',' expr_no_commas
		{ $$ = expression_chain($3, $1); }
	;

callkind:
	  CALL { $$.i = command_call; }
	| SIGNAL { $$.i = event_signal; }
	| POST { $$.i = post_task; }
	;

unary_expr:
	  primary
	| callkind function_call 
		{
		  function_call fc = CAST(function_call, $2);
		  type calltype = fc->arg1->type;
		  bool noerror = fc->type != error_type;
		  
		  $$ = $2;
		  fc->call_kind = $1.i;
		  switch ($1.i)
		    {
		    case command_call:
		      if (noerror && !type_command(calltype))
			error("only commands can be called");
		      break;
		    case event_signal:
		      if (noerror && !type_event(calltype))
			error("only events can be signaled");
		      break;
		    case post_task:
		      fc->type = unsigned_char_type;
		      if (noerror)
		        {
			  if (!type_task(calltype))
			    error("only tasks can be posted");
			  else if (flag_use_scheduler)
			    /* If requested, replace post/task by references to
			       an interface */
			    handle_post(fc);
			}
		      break;
		    }
		}
	| '*' cast_expr
		{ $$ = make_dereference($1.location, $2); }
	/* __extension__ turns off -pedantic for following primary.  */
	| extension cast_expr	
		{ $$ = make_extension_expr($1.location, $2);
		  pedantic = $1.i; }
	| unop cast_expr
		{ $$ = make_unary($1.location, $1.i, $2);
#if 0
		  overflow_warning($$); 
#endif
		}
	/* Refer to the address of a label as a pointer.  */
	| ANDAND id_label
		{
		  $$ = CAST(expression, make_label_address($1.location, $2));
		  use_label($2);
		}
	| sizeof unary_expr
		{ 
#if 0
		  if (TREE_CODE ($2) == COMPONENT_REF
		      && DECL_C_BIT_FIELD (TREE_OPERAND ($2, 1)))
		    error("`sizeof' applied to a bit-field");
		  $$ = c_sizeof (TREE_TYPE ($2)); 
#endif
		  $$ = make_sizeof_expr($1.location, $2);
		  pstate.unevaluated_expression--; }
	| sizeof '(' typename ')'
		{ $$ = make_sizeof_type($1.location, $3);
		  pstate.unevaluated_expression--; }
	| alignof unary_expr
		{ $$ = make_alignof_expr($1.location, $2);
		  pstate.unevaluated_expression--; }
	| alignof '(' typename ')'
		{ $$ = make_alignof_type($1.location, $3); 
		  pstate.unevaluated_expression--; }
	;

sizeof:
	  SIZEOF { pstate.unevaluated_expression++; $$ = $1; }
	;

alignof:
	  ALIGNOF { pstate.unevaluated_expression++; $$ = $1; }
	;

cast_expr:
	  unary_expr
	| '(' typename ')' cast_expr
	  	{ $$ = make_cast($1.location, $2, $4); }
	| '(' typename ')' '{' 
		{ 
		  start_init(NULL, NULL);
		  really_start_incremental_init($2->type); 
		}
	  initlist_maybe_comma '}'
		{ 
		  expression constructor = make_init_list($4.location, $6);

		  finish_init();

		  if (pedantic)
		    pedwarn("ANSI C forbids constructor expressions");

		  $$ = make_cast_list($1.location, $2, constructor);
		}
	;

expr_no_commas:
	  cast_expr
	| expr_no_commas '+' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_plus, $1, $3); }
	| expr_no_commas '-' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_minus, $1, $3); }
	| expr_no_commas '*' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_times, $1, $3); }
	| expr_no_commas '/' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_divide, $1, $3); }
	| expr_no_commas '%' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_modulo, $1, $3); }
	| expr_no_commas LSHIFT expr_no_commas
	    	{ $$ = make_binary($2.location, kind_lshift, $1, $3); }
	| expr_no_commas RSHIFT expr_no_commas
	    	{ $$ = make_binary($2.location, kind_rshift, $1, $3); }
	| expr_no_commas ARITHCOMPARE expr_no_commas
	    	{ $$ = make_binary($2.location, $2.i, $1, $3); }
	| expr_no_commas '<' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_lt, $1, $3); }
	| expr_no_commas '>' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_gt, $1, $3); }
	| expr_no_commas EQCOMPARE expr_no_commas
	    	{ $$ = make_binary($2.location, $2.i, $1, $3); }
	| expr_no_commas '&' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_bitand, $1, $3); }
	| expr_no_commas '|' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_bitor, $1, $3); }
	| expr_no_commas '^' expr_no_commas
	    	{ $$ = make_binary($2.location, kind_bitxor, $1, $3); }
	| expr_no_commas ANDAND expr_no_commas
	    	{ $$ = make_binary($2.location, kind_andand, $1, $3); }
	| expr_no_commas OROR expr_no_commas
	    	{ $$ = make_binary($2.location, kind_oror, $1, $3); }
	| expr_no_commas '?' expr ':' expr_no_commas
	  	{ $$ = make_conditional($2.location, $1, $3, $5); }
	| expr_no_commas '?'
		{ if (pedantic)
		    pedwarn("ANSI C forbids omitting the middle term of a ?: expression"); 
		}
	  ':' expr_no_commas
	  	{ $$ = make_conditional($2.location, $1, NULL, $5); }
	| expr_no_commas '=' expr_no_commas
	    	{ $$ = make_assign($2.location, kind_assign, $1, $3); }
	| expr_no_commas ASSIGN expr_no_commas
	    	{ $$ = make_assign($2.location, $2.i, $1, $3); }
	;

primary:
	  IDENTIFIER
		{ 
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  $$ = make_identifier($1.location, $1.id, yychar == '('); 
		}
	| CONSTANT { $$ = CAST(expression, $1); }
	| string { $$ = $1; }
	| '(' expr ')'
		{ $$ = $2; $$->parens = TRUE; }
	| '(' error ')'
		{ $$ = make_error_expr(); }
	| '('
		{ if (current.function_decl == 0)
		    {
		      error("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		    push_label_level();
		}
	  compstmt ')'
		{ 
		  pop_label_level();
		  if (pedantic)
		    pedwarn("ANSI C forbids braced-groups within expressions");
		  $$ = make_compound_expr($1.location, $3);
		}
	| function_call
		{
		  /* Magic functions may rewrite this to something else */
		  if (is_function_call($1))
		    {
		      function_call fc = CAST(function_call, $1);
		      type calltype = fc->arg1->type;

		      if (type_command(calltype))
			error("commands must be called with call");
		      else if (type_event(calltype))
			error("events must be signaled with signal");
		      else if (type_task(calltype))
			error("tasks must be posted with post");
		    }

		  $$ = $1;
		}
	| VA_ARG '(' expr_no_commas ',' typename ')'
		{ $$ = make_va_arg($1.location, $3, $5); }
	| OFFSETOF '(' typename ',' fieldlist ')'
		{ $$ = make_offsetof($1.location, $3, $5); }
	| primary '[' nonnull_exprlist ']' 
		{ $$ = make_array_ref($2.location, $1, $3); }
	| primary '.' identifier
		{ $$ = make_field_ref($2.location, $1, $3.id); }
	| primary POINTSAT identifier
		{ $$ = make_field_ref($2.location, make_dereference($2.location, $1),
				      $3.id); }
	| primary PLUSPLUS
		{ $$ = make_postincrement($2.location, $1); }
	| primary MINUSMINUS
		{ $$ = make_postdecrement($2.location, $1); }
	;

fieldlist:
	identifier { $$ = dd_new_list(pr); dd_add_last(pr, $$, $1.id.data); }
	| fieldlist '.' identifier { $$ = $1; dd_add_last(pr, $$, $3.id.data); }
	;

function_call: 
	  primary '(' exprlist ')'
	  	{ $$ = make_function_call($2.location, $1, $3); }
	;

string:   STRING { $$ = CAST(expression, $1); }
	| MAGIC_STRING { $$ = make_identifier($1.location, $1.id, FALSE); }
        ;

old_style_parm_decls:
	  /* empty */ { $$ = NULL; }
	| datadecls
	| datadecls ELLIPSIS
		/* ... is used here to indicate a varargs function.  */
		{ if (pedantic)
		    pedwarn("ANSI C does not permit use of `varargs.h'"); 
		  $$ = declaration_chain(CAST(declaration, new_ellipsis_decl(pr, $2.location)), $1);
		}
	;

/* The following are analogous to decls and decl
   except that they do not allow nested functions.
   They are used for old-style parm decls.  */
datadecls:
	  datadecl
	| datadecls datadecl { $$ = declaration_chain($2, $1); }
	;

/* We don't allow prefix attributes here because they cause reduce/reduce
   conflicts: we can't know whether we're parsing a function decl with
   attribute suffix, or function defn with attribute prefix on first old
   style parm.  */
datadecl:
	  declspecs_ts_nosa setspecs initdecls ';'
		{ $$ = make_data_decl($1, $3); }
	| declspecs_nots_nosa setspecs notype_initdecls ';'
		{ $$ = make_data_decl($1, $3); }
	| declspecs_ts_nosa setspecs ';'
		{ shadow_tag_warned($1, 1);
		  $$ = make_data_decl($1, NULL);
		  pedwarn("empty declaration"); }
	| declspecs_nots_nosa ';'
		{ pedwarn("empty declaration"); 
		  $$ = NULL; }
	;

/* This combination which saves a lineno before a decl
   is the normal thing to use, rather than decl itself.
   This is to avoid shift/reduce conflicts in contexts
   where statement labels are allowed.  */
decls:
	  decl
	| errstmt { $$ = make_error_decl(); }
	| decls decl { $$ = declaration_chain($2, $1); }
	| decl errstmt { $$ = make_error_decl(); }
	;

/* records the type and storage class specs to use for processing
   the declarators that follow.
   Maintains a stack of outer-level values of pstate.declspecs,
   for the sake of parm declarations nested in function declarators.  */
setspecs: /* empty */
		{ 
		  push_declspec_stack();
		  pending_xref_error();
		  pstate.declspecs = $<u.telement>0;
		  pstate.attributes = NULL;
		}
	;

/* Possibly attributes after a comma, which should be saved in
   pstate.attributes */
maybe_resetattrs:
	  maybe_attribute
		{ pstate.attributes = $1; }
	;

decl:
	  declspecs_ts setspecs initdecls ';'
		{ $$ = make_data_decl($1, $3); }
	| declspecs_nots setspecs notype_initdecls ';'
		{ $$ = make_data_decl($1, $3); }
	| declspecs_ts setspecs nested_function
		{ $$ = $3;
		  pop_declspec_stack(); }
	| declspecs_nots setspecs notype_nested_function
		{ $$ = $3;
		  pop_declspec_stack(); }
	| declspecs setspecs ';'
		{ shadow_tag($1);
		  $$ = make_data_decl($1, NULL); }
	| extension decl
		{ $$ = make_extension_decl($1.i, $1.location, $2); }
	;

/* declspecs borrowed from gcc 3. I think it's really ugly, but I guess
   they (and therefore I) am stuck with this brokenness.
   The only redeeming feature is that it's cleaner than gcc 2
*/
/* A list of declaration specifiers.  These are:

   - Storage class specifiers (SCSPEC), which for GCC currently include
   function specifiers ("inline").

   - Type specifiers (type_spec_*).

   - Type qualifiers (TYPE_QUAL).

   - Attribute specifier lists (attributes).

   These are stored as a TREE_LIST; the head of the list is the last
   item in the specifier list.  Each entry in the list has either a
   TREE_PURPOSE that is an attribute specifier list, or a TREE_VALUE that
   is a single other specifier or qualifier; and a TREE_CHAIN that is the
   rest of the list.  TREE_STATIC is set on the list if something other
   than a storage class specifier or attribute has been seen; this is used
   to warn for the obsolescent usage of storage class specifiers other than
   at the start of the list.  (Doing this properly would require function
   specifiers to be handled separately from storage class specifiers.)

   The various cases below are classified according to:

   (a) Whether a storage class specifier is included or not; some
   places in the grammar disallow storage class specifiers (_sc or _nosc).

   (b) Whether a type specifier has been seen; after a type specifier,
   a typedef name is an identifier to redeclare (_ts or _nots).

   (c) Whether the list starts with an attribute; in certain places,
   the grammar requires specifiers that don't start with an attribute
   (_sa or _nosa).

   (d) Whether the list ends with an attribute (or a specifier such that
   any following attribute would have been parsed as part of that specifier);
   this avoids shift-reduce conflicts in the parsing of attributes
   (_ea or _noea).

   TODO:

   (i) Distinguish between function specifiers and storage class specifiers,
   at least for the purpose of warnings about obsolescent usage.

   (ii) Halve the number of productions here by eliminating the _sc/_nosc
   distinction and instead checking where required that storage class
   specifiers aren't present.  */

declspecs_nosc_nots_nosa_noea:
	  type_qual
	| declspecs_nosc_nots_nosa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_nosa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_nosc_nots_nosa_ea:
	  declspecs_nosc_nots_nosa_noea eattributes
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_nosc_nots_sa_noea:
	  declspecs_nosc_nots_sa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_sa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_nosc_nots_sa_ea:
	  eattributes
	| declspecs_nosc_nots_sa_noea eattributes
	;

declspecs_nosc_ts_nosa_noea:
	  type_spec_nonattr
	| declspecs_nosc_ts_nosa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_nosa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_nosa_noea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_nosa_ea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_nosa_noea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_nosa_ea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_nosc_ts_nosa_ea:
	  type_spec_attr
	| declspecs_nosc_ts_nosa_noea eattributes
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_nosa_noea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_nosa_ea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_nosa_noea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_nosa_ea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_nosc_ts_sa_noea:
	  declspecs_nosc_ts_sa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_sa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_sa_noea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_sa_ea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_sa_noea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_sa_ea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_nosc_ts_sa_ea:
	  declspecs_nosc_ts_sa_noea eattributes
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_sa_noea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_sa_ea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_sa_noea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_sa_ea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_nots_nosa_noea:
	  scspec
	| declspecs_sc_nots_nosa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_nosa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_nosa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_nosa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_nosa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_nosa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_nots_nosa_ea:
	  declspecs_sc_nots_nosa_noea eattributes
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_nots_sa_noea:
	  declspecs_sc_nots_sa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_sa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_sa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_nots_sa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_sa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_sa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_nots_sa_ea:
	  declspecs_sc_nots_sa_noea eattributes
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_ts_nosa_noea:
	  declspecs_sc_ts_nosa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_nosa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_nosa_noea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_nosa_ea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_nosa_noea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_nosa_ea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_nosa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_nosa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_nosa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_nosa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_ts_nosa_ea:
	  declspecs_sc_ts_nosa_noea eattributes
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_nosa_noea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_nosa_ea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_nosa_noea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_nosa_ea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_ts_sa_noea:
	  declspecs_sc_ts_sa_noea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_sa_ea type_qual
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_sa_noea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_sa_ea type_spec_reserved_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_sa_noea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_sa_ea type_spec_nonattr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_sa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_nosc_ts_sa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_sa_noea scspec
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_sa_ea scspec
		{ $$ = type_element_chain($1, $2); }
	;

declspecs_sc_ts_sa_ea:
	  declspecs_sc_ts_sa_noea eattributes
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_sa_noea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_ts_sa_ea type_spec_reserved_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_sa_noea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	| declspecs_sc_nots_sa_ea type_spec_attr
		{ $$ = type_element_chain($1, $2); }
	;

/* Particular useful classes of declspecs.  */
declspecs_ts:
	  declspecs_nosc_ts_nosa_noea
	| declspecs_nosc_ts_nosa_ea
	| declspecs_nosc_ts_sa_noea
	| declspecs_nosc_ts_sa_ea
	| declspecs_sc_ts_nosa_noea
	| declspecs_sc_ts_nosa_ea
	| declspecs_sc_ts_sa_noea
	| declspecs_sc_ts_sa_ea
	;

declspecs_nots:
	  declspecs_nosc_nots_nosa_noea
	| declspecs_nosc_nots_nosa_ea
	| declspecs_nosc_nots_sa_noea
	| declspecs_nosc_nots_sa_ea
	| declspecs_sc_nots_nosa_noea
	| declspecs_sc_nots_nosa_ea
	| declspecs_sc_nots_sa_noea
	| declspecs_sc_nots_sa_ea
	;

declspecs_ts_nosa:
	  declspecs_nosc_ts_nosa_noea
	| declspecs_nosc_ts_nosa_ea
	| declspecs_sc_ts_nosa_noea
	| declspecs_sc_ts_nosa_ea
	;

declspecs_nots_nosa:
	  declspecs_nosc_nots_nosa_noea
	| declspecs_nosc_nots_nosa_ea
	| declspecs_sc_nots_nosa_noea
	| declspecs_sc_nots_nosa_ea
	;

declspecs_nosc_ts:
	  declspecs_nosc_ts_nosa_noea
	| declspecs_nosc_ts_nosa_ea
	| declspecs_nosc_ts_sa_noea
	| declspecs_nosc_ts_sa_ea
	;

declspecs_nosc_nots:
	  declspecs_nosc_nots_nosa_noea
	| declspecs_nosc_nots_nosa_ea
	| declspecs_nosc_nots_sa_noea
	| declspecs_nosc_nots_sa_ea
	;

declspecs_nosc:
	  declspecs_nosc_ts
	| declspecs_nosc_nots
	;

declspecs:
	  declspecs_ts
	| declspecs_nots
	;

/* A (possibly empty) sequence of type qualifiers and attributes.  */
maybe_type_quals_attrs:
	  /* empty */
		{ $$ = NULL; }
	| declspecs_nosc_nots
	;

/* A type specifier (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.

   The _reserved versions start with a reserved word and may appear anywhere
   in the declaration specifiers; the _nonreserved versions may only
   appear before any other type specifiers, and after that are (if names)
   being redeclared.

   FIXME: should the _nonreserved version be restricted to names being
   redeclared only?  The other entries there relate only the GNU extensions
   and Objective C, and are historically parsed thus, and don't make sense
   after other type specifiers, but it might be cleaner to count them as
   _reserved.

   _attr means: specifiers that either end with attributes,
   or are such that any following attributes would
   be parsed as part of the specifier.

   _nonattr: specifiers.  */

type_spec_nonattr:
	  type_spec_reserved_nonattr
	| type_spec_nonreserved_nonattr
	;

type_spec_attr:
	  type_spec_reserved_attr
	;

type_spec_reserved_nonattr:
	  type_spec
	| structuse
	;

type_spec_reserved_attr:
	  structdef
	;

type_spec_nonreserved_nonattr:
	  TYPENAME
		{ /* For a typedef name, record the meaning, not the name.
		     In case of `foo foo, bar;'.  */
		  $$ = CAST(type_element, new_typename(pr, $1.location, $1.decl)); }
	| COMPONENTREF '.' identifier
		{
		  /* reference to a typedef from a component. */
		  $$ = CAST(type_element, new_component_typeref(pr, $1.location, $3.decl, $1.id)); 
		}
	| TYPEOF '(' expr ')'
		{ $$ = CAST(type_element, new_typeof_expr(pr, $1.location, $3)); }
	| TYPEOF '(' typename ')'
		{ $$ = CAST(type_element, new_typeof_type(pr, $1.location, $3)); }
	;
/* type_spec_nonreserved_attr does not exist.  */

initdecls:
	  initdecls_ { $$ = declaration_reverse($1); }
	;

notype_initdecls:
	  notype_initdecls_ { $$ = declaration_reverse($1); }
	;

initdecls_:
	  initdcl
	| initdecls_ ',' maybe_resetattrs initdcl 
		{ $$ = declaration_chain($4, $1); }
	;

notype_initdecls_:
	  notype_initdcl { $$ = $1; }
	| notype_initdecls_ ',' maybe_resetattrs initdcl 
		{ $$ = declaration_chain($4, $1); }
	;

maybeasm:
	  /* empty */
		{ $$ = NULL; }
	| ASM_KEYWORD '(' STRING ')'
		{ $$ = new_asm_stmt(pr, $1.location, CAST(expression, $3),
				    NULL, NULL, NULL, NULL); }
	;

initdcl:
	  declarator maybeasm maybe_attribute '='
		{ $<u.decl>$ = start_decl($1, $2, pstate.declspecs, 1,
                                          prefix_attr($3));
		  start_init($<u.decl>$, NULL); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_init();
		  $$ = finish_decl($<u.decl>5, $6); }
	| declarator maybeasm maybe_attribute
		{ declaration d = start_decl($1, $2, pstate.declspecs, 0,
					     prefix_attr($3));
		  $$ = finish_decl(d, NULL); }
	;

notype_initdcl:
	  notype_declarator maybeasm maybe_attribute '='
		{ $<u.decl>$ = start_decl($1, $2, pstate.declspecs, 1,
					 prefix_attr($3));
		  start_init($<u.decl>$, NULL); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_init();
		  $$ = finish_decl($<u.decl>5, $6); }
	| notype_declarator maybeasm maybe_attribute
		{ declaration d = start_decl($1, $2, pstate.declspecs, 0,
					     prefix_attr($3));
		  $$ = finish_decl(d, NULL); }
	;

maybe_attribute:
	  /* empty */
  		{ $$ = NULL; }
	| attributes
		{ $$ = attribute_reverse($1); }
	;
 
eattributes:
	  attributes { $$ = CAST(type_element, $1); }
	;

nesc_attributes:
	  /* empty */ { $$ = NULL; }
	| nesc_attributes nattrib
		{ $$ = attribute_chain($2, $1); }
	;

attributes:
	  attribute
		{ $$ = $1; }
	| attributes attribute
		{ $$ = attribute_chain($2, $1); }
	;

attribute:
	  ATTRIBUTE '(' '(' attribute_list ')' ')'
		{ $$ = $4; }
	| target_attribute { $$ = CAST(attribute, $1); }
	| nattrib
	;

target_attribute:
	  TARGET_ATTRIBUTE0
		{ word w = new_word(pr, $1.location, $1.id);
		  $$ = new_target_attribute(pr, $1.location, w, NULL); }
	| TARGET_ATTRIBUTE1 restricted_expr
		{ word w = new_word(pr, $1.location, $1.id);
		  $$ = new_target_attribute(pr, $1.location, w, $2); }
	| '@' restricted_expr
		{ word w = new_word(pr, $2->location, str2cstring(pr, "iar_at"));
		  $$ = new_target_attribute(pr, $2->location, w, $2); }
	;

restricted_expr:
	  CONSTANT { $$ = CAST(expression, $1); }
	| string { $$ = $1; }
	| '(' expr ')' { $$ = $2; }
	;

attribute_list:
	  attrib
		{ $$ = CAST(attribute, $1); }
	| attribute_list ',' attrib
		{ $$ = attribute_chain($1, CAST(attribute, $3)); }
	;

attrib:
	  /* empty */
		{ $$ = NULL; }
	| any_word
		{ $$ = new_gcc_attribute(pr, $1->location, $1, NULL); }
	| any_word '(' IDENTIFIER ')'
		{ $$ = new_gcc_attribute
		    (pr, $1->location, $1, make_attr_args($3.location, $3.id, NULL)); }
	| any_word '(' IDENTIFIER ',' nonnull_exprlist ')'
		{ $$ = new_gcc_attribute
		    (pr, $2.location, $1, make_attr_args($3.location, $3.id, $5));
		}
	| any_word '(' exprlist ')'
		{ $$ = new_gcc_attribute(pr, $2.location, $1, $3);
		}
	;

nattrib:
	  '@' nastart '(' initlist_maybe_comma ')'
		{ $$ = finish_attribute_use($2, $4); }
	| '@' nastart error ')'
		{ $$ = finish_attribute_use($2, make_error_expr()); }
	;

nastart:
	  idword
	  	{ $$ = start_attribute_use($1); }
	;

/* This still leaves out most reserved keywords,
   shouldn't we include them?  */

any_word:
	  idword
	| scspec
		{ $$ = make_cword($1->location, rid_name(CAST(rid, $1))); }
	| type_spec 
		{ $$ = make_cword($1->location, rid_name(CAST(rid, $1))); }
	| type_qual 
		{ $$ = make_cword($1->location, qualifier_name(CAST(qualifier, $1)->id)); }
	| SIGNAL
		{ $$ = make_cword($1.location, "signal"); }
	;

/* Initializers.  `init' is the entry point.  */

init:
	  expr_no_commas { $$ = $1; simple_init($$); }
	| '{'
		{ really_start_incremental_init(NULL); }
	  initlist_maybe_comma '}'
		{ $$ = make_init_list($1.location, $3); }
	| error
		{ $$ = make_error_expr(); }
	;

/* `initlist_maybe_comma' is the guts of an initializer in braces.  */
initlist_maybe_comma:
	  /* empty */
		{ if (pedantic)
		    pedwarn("ANSI C forbids empty initializer braces"); 
		  $$ = NULL; }
	| initlist1 maybecomma { $$ = expression_reverse($1); }
	;

initlist1:
	  initelt
	| initlist1 ',' initelt { $$ = expression_chain($3, $1); }
	;

/* `initelt' is a single element of an initializer.
   It may use braces.  */
initelt:
	  designator_list '=' initval
		{ if (pedantic)
		    pedwarn("ANSI C forbids specifying subobject to initialize"); 
		  $$ = make_init_specific($1, $3); }
	| designator initval
		{ if (pedantic)
		    pedwarn("obsolete use of designated initializer without `='");
		  $$ = make_init_specific($1, $2); }
	| identifier ':'
		{ $<u.designator>$ = set_init_label($1.location, $1.id);
		  if (pedantic)
		    pedwarn("obsolete use of designated initializer with `:'"); }
	  initval
		{ $$ = make_init_specific($<u.designator>3, $4); }
	| initval
	;

initval:
	  '{'
		{ push_init_level (0); }
	  initlist_maybe_comma '}'
		{ $$ = make_init_list($1.location, $3); 
		  process_init_element(NULL); }
	| expr_no_commas
		{ process_init_element($1); $$ = $1; }
	| error { $$ = make_error_expr(); }
	;

designator_list:
	  designator 
	| designator_list designator { $$ = designator_chain($1, $2); }
	;

designator:
	  '.' identifier
		{ $$ = set_init_label($2.location, $2.id); }
	/* These are for labeled elements.  The syntax for an array element
	   initializer conflicts with the syntax for an Objective-C message,
	   so don't include these productions in the Objective-C grammar.  */
	| '[' expr_no_commas ELLIPSIS expr_no_commas ']'
		{ $$ = set_init_index($1.location, $2, $4);
		  if (pedantic)
		    pedwarn ("ISO C forbids specifying range of elements to initialize"); }
	| '[' expr_no_commas ']'
		{ $$ = set_init_index($1.location, $2, NULL); }
	;

nested_function:
	  declarator maybeasm maybe_attribute
		{ 
		  /* maybeasm is only here to avoid a s/r conflict */
		  refuse_asm($2);

		  if (!start_function(pstate.declspecs, $1, $3, 1))
		    {
		      YYERROR1;
		    }
		  }
	   old_style_parm_decls
		{ store_parm_decls(declaration_reverse($3)); }
/* This used to use compstmt_or_error.
   That caused a bug with input `f(g) int g {}',
   where the use of YYERROR1 above caused an error
   which then was handled by compstmt_or_error.
   There followed a repeated execution of that same rule,
   which called YYERROR1 again, and so on.  */
	  compstmt
		{ $$ = finish_function($7); }
	;

notype_nested_function:
	  notype_declarator maybeasm maybe_attribute
		{ 
		  /* maybeasm is only here to avoid a s/r conflict */
		  refuse_asm($2);

		  if (!start_function(pstate.declspecs, $1, $3, 1))
		    {
		      YYERROR1;
		    }
		}
	  old_style_parm_decls
		{ store_parm_decls(declaration_reverse($3)); }
/* This used to use compstmt_or_error.
   That caused a bug with input `f(g) int g {}',
   where the use of YYERROR1 above caused an error
   which then was handled by compstmt_or_error.
   There followed a repeated execution of that same rule,
   which called YYERROR1 again, and so on.  */
	  compstmt
		{ $$ = finish_function($7); }
	;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit type_spec).  */

declarator:
	  after_type_declarator
	| notype_declarator
	;

/* A declarator that is allowed only after an explicit type_spec.  */

after_type_declarator:
	  after_type_declarator array_or_fn_declarator 
		{ $$ = finish_array_or_fn_declarator($1, $2); }
        | '*' maybe_type_quals_attrs after_type_declarator
		{ $$ = make_pointer_declarator($1.location, $3, $2); }
	| '(' maybe_attribute after_type_declarator ')'
		{ $$ = make_qualified_declarator($1.location, $3, CAST(type_element, $2)); }
	| TYPENAME { $$ = make_identifier_declarator($1.location, $1.id); }
	| TYPENAME '.' identifier 
		{
		  $$ = make_interface_ref_declarator($1.location, $1.id, $3.id);
		}
	;

/* Kinds of declarator that can appear in a parameter list
   in addition to notype_declarator.  This is like after_type_declarator
   but does not allow a typedef name in parentheses as an identifier
   (because it would conflict with a function with that typedef as arg).  */
parm_declarator:
	  parm_declarator array_or_fn_declarator
		{ $$ = finish_array_or_fn_declarator($1, $2); }
	| '*' maybe_type_quals_attrs parm_declarator
		{ $$ = make_pointer_declarator($1.location, $3, $2); }
	| TYPENAME 
		{ $$ = make_identifier_declarator($1.location, $1.id); }
	;


/* A declarator allowed whether or not there has been
   an explicit type_spec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  notype_declarator array_or_fn_declarator
		{ $$ = finish_array_or_fn_declarator($1, $2); }
	| '*' maybe_type_quals_attrs notype_declarator
		{ $$ = make_pointer_declarator($1.location, $3, $2); }
	| '(' maybe_attribute notype_declarator ')'
		{ $$ = make_qualified_declarator($1.location, $3, CAST(type_element, $2)); }
	| IDENTIFIER 
		{ $$ = make_identifier_declarator($1.location, $1.id); }
	| IDENTIFIER '.' identifier
		{
		  $$ = make_interface_ref_declarator($1.location, $1.id, $3.id);
		}
	;

tag:
	  identifier { $$ = new_word(pr, $1.location, $1.id); }
	;

structuse:
	  structkind tag nesc_attributes
		{ $$ = xref_tag($1.location, $1.i, $2); 
		  if ($3) warning("attributes ignored"); }
	| ENUM tag nesc_attributes
		{ $$ = xref_tag($1.location, kind_enum_ref, $2);
		  if ($3) warning("attributes ignored"); }
	;

structdef:
	  structkind tag nesc_attributes '{'
		{ $<u.telement>$ = start_struct($1.location, $1.i, $2);
		  /* Start scope of tag before parsing components.  */
		}
	  component_decl_list '}' maybe_attribute 
		{ $$ = finish_struct($<u.telement>5, $6, attribute_chain($3, $8)); }
	| STRUCT '@' tag nesc_attributes '{'
		{ $<u.telement>$ = start_struct($1.location, kind_attribute_ref, $3);
		  /* Start scope of tag before parsing components.  */
		}
	  component_decl_list '}' maybe_attribute 
		{ $$ = finish_struct($<u.telement>6, $7, attribute_chain($4, $9)); }
	| structkind '{' component_decl_list '}' maybe_attribute
		{ $$ = finish_struct(start_struct($1.location, $1.i,
						  NULL), $3, $5);
		}
	| ENUM tag nesc_attributes '{'
		{ $<u.telement>$ = start_enum($1.location, $2); }
	  enumlist maybecomma_warn '}' maybe_attribute
		{ $$ = finish_enum($<u.telement>5, declaration_reverse($6), attribute_chain($3, $9)); }
	| ENUM '{'
		{ $<u.telement>$ = start_enum($1.location, NULL); }
	  enumlist maybecomma_warn '}' maybe_attribute
		{ $$ = finish_enum($<u.telement>3, declaration_reverse($4), $7); }
	;

structkind:
	  STRUCT { $$ = $1; $$.i = kind_struct_ref; }
	| UNION { $$ = $1; $$.i = kind_union_ref; }
	| NX_STRUCT { $$ = $1; $$.i = kind_nx_struct_ref; }
	| NX_UNION { $$ = $1; $$.i = kind_nx_union_ref; }
	;

maybecomma:
	  /* empty */
	| ','
	;

maybecomma_warn:
	  /* empty */
	| ','
		{ if (pedantic) pedwarn("comma at end of enumerator list"); }
	;

component_decl_list:
	  component_decl_list2
		{ $$ = declaration_reverse($1); }
	| component_decl_list2 component_decl
		{ $$ = declaration_reverse(declaration_chain($2, $1));
		  pedwarn("no semicolon at end of struct or union"); }
	;

component_decl_list2:	
	  /* empty */
		{ $$ = NULL; }
	| component_decl_list2 component_decl ';'
		{ $$ = declaration_chain($2, $1); }
	| component_decl_list2 ';'
		{ if (pedantic)
		    pedwarn("extra semicolon in struct or union specified"); 
		   $$ = $1; }
	;

/* There is a shift-reduce conflict here, because `components' may
   start with a `typename'.  It happens that shifting (the default resolution)
   does the right thing, because it treats the `typename' as part of
   a `typed_type_specs'.

   It is possible that this same technique would allow the distinction
   between `notype_initdecls' and `initdecls' to be eliminated.
   But I am being cautious and not trying it.  */

component_decl:
	  declspecs_nosc_ts setspecs components
		{ $$ = make_data_decl($1, declaration_reverse($3)); }
	| declspecs_nosc_ts setspecs
		{ if (pedantic)
		    pedwarn("ISO C doesn't support unnamed structs/unions");

		  $$ = make_data_decl($1, NULL); }
	| declspecs_nosc_nots setspecs components_notype
		{ $$ = make_data_decl($1, declaration_reverse($3)); }
	| declspecs_nosc_nots setspecs
		{ if (pedantic)
		    pedwarn("ANSI C forbids member declarations with no members");
		  shadow_tag($1);
		  $$ = make_data_decl($1, NULL); }
	| error
		{ $$ = make_error_decl(); }
	| extension component_decl
		{ $$ = make_extension_decl($1.i, $1.location, $2); }
	;

components:
	  component_declarator
	| components ',' maybe_resetattrs component_declarator
		{ $$ = declaration_chain($4, $1); }
	;

/* It should be possible to use components after the ',', but gcc 3
   isn't doing this */
components_notype:
	  component_notype_declarator
	| components_notype ',' maybe_resetattrs component_notype_declarator
		{ $$ = declaration_chain($4, $1); }
	;

component_declarator:
	  declarator maybe_attribute
		{ $$ = make_field($1, NULL, pstate.declspecs,
				  prefix_attr($2)); }
	| declarator ':' expr_no_commas maybe_attribute
		{ $$ = make_field($1, $3, pstate.declspecs,
				  prefix_attr($4)); }
	| ':' expr_no_commas maybe_attribute
		{ $$ = make_field(NULL, $2, pstate.declspecs,
				  prefix_attr($3)); }
	;

component_notype_declarator:
	  notype_declarator maybe_attribute
		{ $$ = make_field($1, NULL, pstate.declspecs,
				  prefix_attr($2)); }
	| notype_declarator ':' expr_no_commas maybe_attribute
		{ $$ = make_field($1, $3, pstate.declspecs,
				  prefix_attr($4)); }
	| ':' expr_no_commas maybe_attribute
		{ $$ = make_field(NULL, $2, pstate.declspecs,
				  prefix_attr($3)); }
	;

enumlist:
	  enumerator
	| enumlist ',' enumerator
		{ $$ = declaration_chain($3, $1); }
	| error
		{ $$ = NULL; }
	;


enumerator:
	  identifier
	  	{ $$ = make_enumerator($1.location, $1.id, NULL); }
	| identifier '=' expr_no_commas
	  	{ $$ = make_enumerator($1.location, $1.id, $3); }
	;

typename:
	  declspecs_nosc 
	  	{ pending_xref_error(); }
	  absdcl
		{ $$ = make_type($1, $3); }
	;

absdcl:   /* an abstract declarator */
	/* empty */
		{ $$ = NULL; }
	| absdcl1
	;

absdcl1:  /* a nonempty absolute declarator */
	  absdcl1_ea
	| absdcl1_noea
	;

absdcl1_noea:
	  direct_absdcl1
	| '*' maybe_type_quals_attrs absdcl1_noea
		{ $$ = make_pointer_declarator($1.location, $3, $2); }
	;

absdcl1_ea:
	  '*' maybe_type_quals_attrs
		{ $$ = make_pointer_declarator($1.location, NULL, $2); }
	| '*' maybe_type_quals_attrs absdcl1_ea
		{ $$ = make_pointer_declarator($1.location, $3, $2); }
	;

direct_absdcl1:
	  '(' maybe_attribute absdcl1 ')'
		{ $$ = make_qualified_declarator($1.location, $3, CAST(type_element, $2)); }
	| direct_absdcl1 array_or_absfn_declarator
		{ $$ = finish_array_or_fn_declarator($1, $2); }
	| array_or_absfn_declarator 
		{ $$ = finish_array_or_fn_declarator(NULL, $1); }
	;

array_or_fn_declarator:
	  fn_declarator 
	| array_declarator
	;

array_or_absfn_declarator:
	  absfn_declarator 
	| array_declarator
	;

fn_declarator:
	  parameters '(' parmlist_or_identifiers_1 fn_quals
		{ $$ = CAST(nested_declarator,
		    new_function_declarator(pr, $2.location, NULL, $3, $1, $4, NULL)); }
	|  '(' parmlist_or_identifiers fn_quals
		{ $$ = CAST(nested_declarator,
		    new_function_declarator(pr, $1.location, NULL, $2, NULL, $3, NULL)); }
	;

absfn_declarator:
	  '(' parmlist fn_quals
		{ $$ = CAST(nested_declarator,
		    new_function_declarator(pr, $1.location, NULL, $2, NULL, $3, NULL)); }
	;

array_declarator:
	  '[' expr ']'
		{ $$ = CAST(nested_declarator, new_array_declarator(pr, $1.location, NULL, $2)); }
	| '[' ']' 
		{ $$ = CAST(nested_declarator, new_array_declarator(pr, $1.location, NULL, NULL)); }
	;

/* at least one statement, the first of which parses without error.  */
/* stmts is used only after decls, so an invalid first statement
   is actually regarded as an invalid decl and part of the decls.  */

stmts:
	stmt_or_labels
		{
		  if (pedantic && $1.i)
		    pedwarn("ANSI C forbids label at end of compound statement");
		  /* Add an empty statement to last label if stand-alone */
		  if ($1.i)
		    {
		      statement last_label = last_statement($1.stmt);

		      chain_with_labels(last_label, CAST(statement, new_empty_stmt(pr, last_label->location)));
		    }
		  $$ = $1.stmt;
		}
	;

stmt_or_labels:
	  stmt_or_label
	| stmt_or_labels stmt_or_label
		{ $$.i = $2.i; $$.stmt = chain_with_labels($1.stmt, $2.stmt); }
	| stmt_or_labels errstmt
		{ $$.i = 0; $$.stmt = make_error_stmt(); }
	;

xstmts:
	/* empty */ { $$ = NULL; }
	| stmts
	;

errstmt:  error ';'
	;

pushlevel:
	  /* empty */
		{ pushlevel(FALSE); }
	;

/* Read zero or more forward-declarations for labels
   that nested functions can jump to.  */
maybe_label_decls:
	  /* empty */ { $$ = NULL; }
	| label_decls
		{ if (pedantic)
		    pedwarn("ANSI C forbids label declarations"); 
		  $$ = id_label_reverse($1); }
	;

label_decls:
	  label_decl
	| label_decls label_decl { $$ = id_label_chain($2, $1); }
	;

label_decl:
	  LABEL identifiers_or_typenames ';'
		{ $$ = $2; }
	;

/* This is the body of a function definition.
   It causes syntax errors to ignore to the next openbrace.  */
compstmt_or_error:
	  compstmt
	| error compstmt { $$ = $2; }
	;

compstmt_start: 
	  '{' { $$ = $1; compstmt_count++; }
        ;

compstmt: 
	  compstmt_start pushlevel '}'
		{ $$ = CAST(statement, new_compound_stmt(pr, $1.location, NULL, NULL, NULL, poplevel())); }
	| compstmt_start pushlevel maybe_label_decls decls xstmts '}'
		{ $$ = CAST(statement, new_compound_stmt(pr, $1.location, $3,
		    declaration_reverse($4), $5, poplevel())); }
	| compstmt_start pushlevel maybe_label_decls error '}'
		{ poplevel();
		  $$ = make_error_stmt(); }
	| compstmt_start pushlevel maybe_label_decls stmts '}'
		{ $$ = CAST(statement, new_compound_stmt(pr, $1.location, $3, NULL, $4, poplevel())); }
	;

/* Value is number of statements counted as of the closeparen.  */
simple_if:
	  if_prefix labeled_stmt
		{ $$.stmt = CAST(statement, new_if_stmt(pr, $1.expr->location, $1.expr, $2, NULL));
		  $$.i = $1.i; }
	| if_prefix error { $$.i = $1.i; $$.stmt = make_error_stmt(); }
	;

if_prefix:
	  IF '(' expr ')'
		{ $$.i = stmt_count;
		  $$.expr = $3;
		  check_condition("if", $3); }
	;

/* This is a subroutine of stmt.
   It is used twice, once for valid DO statements
   and once for catching errors in parsing the end test.  */
do_stmt_start:
	  DO
		{ stmt_count++;
		  compstmt_count++; 
		  $<u.cstmt>$ = CAST(conditional_stmt,
				   new_dowhile_stmt(pr, $1.location, NULL, NULL));
		 push_loop(CAST(statement, $<u.cstmt>$)); }
	  labeled_stmt WHILE
		{ $$ = $<u.cstmt>2; 
		  $$->stmt = $3; }
	;

labeled_stmt:
	  stmt
		{ $$ = $1; }
	| label labeled_stmt
		{ $$ = CAST(statement, new_labeled_stmt(pr, $1->location, $1, $2)); }
	;

stmt_or_label:
	  stmt
		{ $$.i = 0; $$.stmt = $1; }
	| label
		{ $$.i = 1; $$.stmt = CAST(statement, new_labeled_stmt(pr, $1->location, $1, NULL)); }
	;

atomic_stmt:
	  ATOMIC { 
		   atomic_stmt last_atomic = current.in_atomic;

		   current.in_atomic = new_atomic_stmt(pr, $1.location, NULL);
		   current.in_atomic->containing_atomic = last_atomic;
		 }
	  stmt_or_error
	  	{
		  atomic_stmt this_atomic = current.in_atomic;

		  this_atomic->stmt = $3;
	  	  current.in_atomic = this_atomic->containing_atomic;
		  if (current.in_atomic) /* Ignore nested atomics */
		    $$ = $3;
		  else
		    $$ = CAST(statement, this_atomic);
		}
	;

stmt_or_error:
	  stmt
	| error { $$ = make_error_stmt(); }
	;

/* Parse a single real statement, not including any labels.  */
stmt:
	  compstmt
		{ stmt_count++; $$ = $1; }
	| expr ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_expression_stmt(pr, $1->location, $1)); }
	| simple_if ELSE
		{ $1.i = stmt_count; }
	  labeled_stmt
		{ if (extra_warnings && stmt_count == $1.i)
		    warning("empty body in an else-statement");
		  $$ = $1.stmt;
		  if (is_if_stmt($$)) /* could be an error_stmt */
		    CAST(if_stmt, $$)->stmt2 = $4;
		}
	| simple_if %prec IF
		{ /* This warning is here instead of in simple_if, because we
		     do not want a warning if an empty if is followed by an
		     else statement.  Increment stmt_count so we don't
		     give a second error if this is a nested `if'.  */
		  if (extra_warnings && stmt_count++ == $1.i)
		    warning_with_location ($1.stmt->location,
					   "empty body in an if-statement");
		  $$ = $1.stmt; }
	| simple_if ELSE error
		{ $$ = make_error_stmt(); }
	| WHILE
		{ stmt_count++; }
	  '(' expr ')' 
	        { check_condition("while", $4); 
		  $<u.cstmt>$ = CAST(conditional_stmt,
			           new_while_stmt(pr, $1.location, $4, NULL));
		  /* The condition is not "in the loop" for break or continue */
		  push_loop(CAST(statement, $<u.cstmt>$)); }
	  labeled_stmt
		{ $$ = CAST(statement, $<u.cstmt>6);
		  $<u.cstmt>6->stmt = $7; 
		  pop_loop(); }
	| do_stmt_start '(' expr ')' ';'
		{ $$ = CAST(statement, $1);
		  $1->condition = $3;
		  check_condition("do-while", $3); 
		  /* Note that pop_loop should be before the expr to be consistent
		     with while, but GCC is inconsistent. See loop1.c */
		  pop_loop(); }
	| do_stmt_start error
		{ $$ = make_error_stmt(); 
		  pop_loop(); }
	| FOR '(' xexpr ';' { stmt_count++; }
		xexpr ';' { if ($6) check_condition("for", $6); }
		xexpr ')' 
		{ $<u.for_stmt>$ = new_for_stmt(pr, $1.location, $3, $6, $9, NULL);
		  push_loop(CAST(statement, $<u.for_stmt>$)); }
		labeled_stmt
		{ $$ = CAST(statement, $<u.for_stmt>11);
		  $<u.for_stmt>11->stmt = $12; 
		  pop_loop(); }
	| SWITCH '(' expr ')'
	        { stmt_count++; check_switch($3); 
		  $<u.cstmt>$ = CAST(conditional_stmt,
			           new_switch_stmt(pr, $1.location, $3, NULL)); 
		  push_loop(CAST(statement, $<u.cstmt>$)); } 
	  labeled_stmt
		{ $$ = CAST(statement, $<u.cstmt>5); 
		  $<u.cstmt>5->stmt = $6;
		  pop_loop(); }
	| BREAK ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_break_stmt(pr, $1.location));
		  check_break($$);
		}
	| CONTINUE ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_continue_stmt(pr, $1.location));
		  check_continue($$);
		}
	| RETURN ';'
		{ stmt_count++;
		  $$ = make_void_return($1.location); }
	| RETURN expr ';'
		{ stmt_count++;
		  $$ = make_return($1.location, $2); }
	| ASM_KEYWORD maybe_type_qual '(' expr ')' ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_asm_stmt(pr, $1.location, $4, NULL,
					       NULL, NULL, $2)); }
	/* This is the case with just output operands.  */
	| ASM_KEYWORD maybe_type_qual '(' expr ':' asm_operands ')' ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_asm_stmt(pr, $1.location, $4, $6, NULL,
					       NULL, $2)); }
	/* This is the case with input operands as well.  */
	| ASM_KEYWORD maybe_type_qual '(' expr ':' asm_operands ':' asm_operands ')' ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_asm_stmt(pr, $1.location, $4, $6, $8, NULL, $2)); }
	/* This is the case with clobbered registers as well.  */
	| ASM_KEYWORD maybe_type_qual '(' expr ':' asm_operands ':'
  	  asm_operands ':' asm_clobbers ')' ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_asm_stmt(pr, $1.location, $4, $6, $8, $10, $2)); }
	| GOTO id_label ';'
		{ stmt_count++;
		  $$ = CAST(statement, new_goto_stmt(pr, $1.location, $2));
		  use_label($2);
		}
	| GOTO '*' expr ';'
		{ if (pedantic)
		    pedwarn("ANSI C forbids `goto *expr;'");
		  fail_in_atomic("goto *");
		  stmt_count++;
		  $$ = CAST(statement, new_computed_goto_stmt(pr, $1.location, $3)); 
		  check_computed_goto($3); }
	| atomic_stmt
	| ';' { $$ = CAST(statement, new_empty_stmt(pr, $1.location)); }
	;

/* Any kind of label, including jump labels and case labels.
   ANSI C accepts labels only before statements, but we allow them
   also at the end of a compound statement.  */

label:	  CASE expr_no_commas ':'
		{ $$ = CAST(label, new_case_label(pr, $1.location, $2, NULL)); 
		  check_case($$); }
	| CASE expr_no_commas ELLIPSIS expr_no_commas ':'
		{ $$ = CAST(label, new_case_label(pr, $1.location, $2, $4)); 
		  check_case($$); }
	| DEFAULT ':'
		{ $$ = CAST(label, new_default_label(pr, $1.location)); 
		  check_default($$); }
	| id_label ':'
		{ $$ = CAST(label, $1); 
		  define_label($1); }
	;

/* Either a type-qualifier or nothing.  First thing in an `asm' statement.  */

maybe_type_qual:
	/* empty */
		{ $$ = NULL; }
	| type_qual
	;

xexpr:
	/* empty */
		{ $$ = NULL; }
	| expr
	;

/* These are the operands other than the first string and colon
   in  asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x))  */
asm_operands:
	  /* empty */
		{ $$ = NULL; }
	| nonnull_asm_operands
	;

nonnull_asm_operands:
	  asm_operand
	| nonnull_asm_operands ',' asm_operand
		{ $$ = asm_operand_chain($1, $3); }
	;

asm_operand:
	  STRING '(' expr ')'
		{ $$ = new_asm_operand(pr, $1->location, NULL, $1, $3);  }
	| '[' idword ']' STRING '(' expr ')'
		{ $$ = new_asm_operand(pr, $1.location, $2, $4, $6);  }
	;

asm_clobbers:
	  STRING
		{ $$ = $1; }
	| asm_clobbers ',' STRING
		{ $$ = string_chain($1, $3); }
	;

/* This is what appears inside the parens in a function declarator.
   Its value is a list of ..._TYPE nodes.  */
parmlist:
		{ pushlevel(TRUE); }
	  parmlist_1
		{ $$ = $2;
		  /* poplevel() is done when building the declarator */
		}
	;

parmlist_1:
	  parmlist_2 ')' { $$ = $1; }
	| parms ';'
		{ if (pedantic)
		    pedwarn("ANSI C forbids forward parameter declarations");
		  allow_parameter_redeclaration($1, TRUE);
		}
	  parmlist_1
		{ $$ = declaration_chain($1, $4); }
	| error ')'
		{ $$ = make_error_decl(); }
	;

/* This is what appears inside the parens in a function declarator.
   Is value is represented in the format that grokdeclarator expects.  */
parmlist_2:  /* empty */
		{ $$ = NULL; }
	| ELLIPSIS
		{ $$ = make_error_decl();
		  /* Gcc used to allow this as an extension.  However, it does
		     not work for all targets, and thus has been disabled.
		     Also, since func (...) and func () are indistinguishable,
		     it caused problems with the code in expand_builtin which
		     tries to verify that BUILT_IN_NEXT_ARG is being used
		     correctly.  */
		  error("ANSI C requires a named argument before `...'");
		}
	| parms
		{ $$ = $1; }
	| parms ',' ELLIPSIS
		{ $$ = declaration_chain($1, CAST(declaration, new_ellipsis_decl(pr, $3.location))); }
	;

parms:
	  parm
	| parms ',' parm
		{ $$ = declaration_chain($1, $3); }
	;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  */
parm:
	  declspecs_ts xreferror parm_declarator maybe_attribute
		{ $$ = declare_parameter($3, $1, $4); }
	| declspecs_ts xreferror notype_declarator maybe_attribute
		{ $$ = declare_parameter($3, $1, $4); }
	| declspecs_ts xreferror absdcl
		{ $$ = declare_parameter($3, $1, NULL); }
	| declspecs_ts xreferror absdcl1_noea attributes
		{ $$ = declare_parameter($3, $1, $4); }
	| declspecs_nots xreferror notype_declarator maybe_attribute
		{ $$ = declare_parameter($3, $1, $4); }
	| declspecs_nots xreferror absdcl
		{ $$ = declare_parameter($3, $1, NULL); }
	| declspecs_nots xreferror absdcl1_noea attributes 
		{ $$ = declare_parameter($3, $1, $4); }
	;

xreferror: { pending_xref_error(); } ;

/* This is used in a function definition
   where either a parmlist or an identifier list is ok.
   Its value is a list of ..._TYPE nodes or a list of identifiers.  */
parmlist_or_identifiers:
		{ pushlevel(TRUE); }
	  parmlist_or_identifiers_1
		{ $$ = $2;
		  /* poplevel is done when building the declarator */ }
	;

parmlist_or_identifiers_1:
	  parmlist_1
	| identifiers ')' { $$ = $1; }
	;

/* A nonempty list of identifiers.  */
identifiers:
	  old_parameter
		{ $$ = $1; }
	| identifiers ',' old_parameter
		{ $$ = declaration_chain($1, $3); }
	;

old_parameter:
	  IDENTIFIER { $$ = declare_old_parameter($1.location, $1.id); }
	;

/* A nonempty list of identifiers, including typenames.  */
identifiers_or_typenames:
	  id_label { $$ = $1; declare_label($1); }
	| identifiers_or_typenames ',' id_label
		{ $$ = id_label_chain($3, $1);
		  declare_label($3); }
	;

/* A possibly empty list of function qualifiers (only one exists so far) */
fn_quals:
	  /* empty */ { $$ = NULL; }
	| fn_qual { $$ = $1; }
	;

extension:
	  EXTENSION
		{ $$.location = $1.location;
		  $$.i = pedantic;
		  pedantic = 0; }
	;

scspec:
	  SCSPEC
		{ $$ = CAST(type_element, new_rid(pr, $1.location, $1.i)); }
	| DEFAULT
		{ $$ = CAST(type_element, new_rid(pr, $1.location, RID_DEFAULT)); }
	;

type_qual:
         TYPE_QUAL 
		{ $$ = CAST(type_element, new_qualifier(pr, $1.location, $1.i)); }
	;

fn_qual:
	  FN_QUAL 
		{ $$ = CAST(type_element, new_qualifier(pr, $1.location, $1.i)); }
	;

type_spec:
	  TYPESPEC
		{ $$ = CAST(type_element, new_rid(pr, $1.location, $1.i)); }
	;


%%
