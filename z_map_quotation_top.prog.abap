*&---------------------------------------------------------------------*
*& Include          Z_MAPACOMPARATIVO_TOP
*&---------------------------------------------------------------------*
report z_rel_default no standard page heading message-id s1 line-size 600.
tables : ekko,lfa1.
data: gt_saida type table of lfa1.

data :fldname(30), fldval(50).



  selection-screen : begin of block b1 with frame title text-001.
  select-options: s_submi for ekko-submi no-EXTENSION no INTERVALS OBLIGATORY DEFAULT '100'.

  selection-screen: end of block b1.
