
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND COMMA FROM IDENTIFIER LPAREN NUMBER OPERATOR OR RPAREN SELECT SEMICOLON STAR STRING WHEREquery : select_statementselect_statement : SELECT select_list FROM table_references where_clause SEMICOLONselect_list : STARselect_list : IDENTIFIERselect_list : select_list COMMA IDENTIFIERtable_references : IDENTIFIERtable_references : table_references COMMA IDENTIFIERwhere_clause : WHERE condition\n| emptycondition : IDENTIFIER OPERATOR valuecondition : LPAREN condition RPARENcondition : condition AND conditioncondition : condition OR conditionvalue : NUMBER\n| STRING\n| IDENTIFIERempty :'
    
_lr_action_items = {'SELECT':([0,],[3,]),'$end':([1,2,16,],[0,-1,-2,]),'STAR':([3,],[5,]),'IDENTIFIER':([3,7,8,13,14,20,21,22,23,],[6,10,11,17,19,19,19,19,27,]),'FROM':([4,5,6,11,],[7,-3,-4,-5,]),'COMMA':([4,5,6,9,10,11,17,],[8,-3,-4,13,-6,-5,-7,]),'WHERE':([9,10,17,],[14,-6,-7,]),'SEMICOLON':([9,10,12,15,17,18,25,26,27,28,29,30,31,],[-17,-6,16,-9,-7,-8,-12,-13,-16,-10,-14,-15,-11,]),'LPAREN':([14,20,21,22,],[20,20,20,20,]),'AND':([18,24,25,26,27,28,29,30,31,],[21,21,21,21,-16,-10,-14,-15,-11,]),'OR':([18,24,25,26,27,28,29,30,31,],[22,22,22,22,-16,-10,-14,-15,-11,]),'OPERATOR':([19,],[23,]),'NUMBER':([23,],[29,]),'STRING':([23,],[30,]),'RPAREN':([24,25,26,27,28,29,30,31,],[31,-12,-13,-16,-10,-14,-15,-11,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'query':([0,],[1,]),'select_statement':([0,],[2,]),'select_list':([3,],[4,]),'table_references':([7,],[9,]),'where_clause':([9,],[12,]),'empty':([9,],[15,]),'condition':([14,20,21,22,],[18,24,25,26,]),'value':([23,],[28,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> query","S'",1,None,None,None),
  ('query -> select_statement','query',1,'p_query','sql_parser.py',84),
  ('select_statement -> SELECT select_list FROM table_references where_clause SEMICOLON','select_statement',6,'p_select_statement','sql_parser.py',89),
  ('select_list -> STAR','select_list',1,'p_select_list_star','sql_parser.py',93),
  ('select_list -> IDENTIFIER','select_list',1,'p_select_list_single','sql_parser.py',97),
  ('select_list -> select_list COMMA IDENTIFIER','select_list',3,'p_select_list_multiple','sql_parser.py',101),
  ('table_references -> IDENTIFIER','table_references',1,'p_table_references_single','sql_parser.py',105),
  ('table_references -> table_references COMMA IDENTIFIER','table_references',3,'p_table_references_multiple','sql_parser.py',109),
  ('where_clause -> WHERE condition','where_clause',2,'p_where_clause','sql_parser.py',113),
  ('where_clause -> empty','where_clause',1,'p_where_clause','sql_parser.py',114),
  ('condition -> IDENTIFIER OPERATOR value','condition',3,'p_condition_comparison','sql_parser.py',121),
  ('condition -> LPAREN condition RPAREN','condition',3,'p_condition_grouped','sql_parser.py',125),
  ('condition -> condition AND condition','condition',3,'p_condition_and','sql_parser.py',129),
  ('condition -> condition OR condition','condition',3,'p_condition_or','sql_parser.py',133),
  ('value -> NUMBER','value',1,'p_value','sql_parser.py',137),
  ('value -> STRING','value',1,'p_value','sql_parser.py',138),
  ('value -> IDENTIFIER','value',1,'p_value','sql_parser.py',139),
  ('empty -> <empty>','empty',0,'p_empty','sql_parser.py',143),
]
