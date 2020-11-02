*&---------------------------------------------------------------------*
*& Report Z_MAP_QUOTATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*



include z_map_quotation_top.

include z_map_quotation_cls.


start-of-selection.

lcl_controller=>run( ).

at line-selection.
  get cursor field fldname value fldval.
  r_controller->handle_click_in_lst_abap( ).
