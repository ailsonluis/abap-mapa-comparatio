"Relatorio padrão OO
"Lista materiais sem descrição na linguagem de logon do usuario.

report z_avl_oo.

class lcl_app definition.
  public section.

  class-data: r_mara type ref to mara.

  types:
      begin of ty_alv,
        icon  type ICON_D,
        matnr type mara-matnr,
        mtart type mara-mtart,
        matkl type mara-matkl,
        matkx type makt-maktx,
        vpsta type mara-vpsta,
      end of ty_alv.

  data: rg_mtart type range of mara-mtart,
         rg_matnr type range of mara-matnr.

  data: gt_mara type table of mara,
        gt_makt type table of makt,
        gt_alv type table of ty_alv,
        r_alv   type ref to cl_salv_table,
        lv_teste type char0010.


  methods constructor.

  methods start.

  methods get_data.

  methods process_data.

  methods show_alv.

  methods on_user_command for event added_function of cl_salv_events importing e_salv_function.

endclass.

class lcl_app implementation.

  method constructor.
    "Para quando a classe é iniciada!
    lv_teste  =  'Construiu'.
  endmethod.
  method start.
    get_data( ).
    process_data( ).
    show_alv( ).
  endmethod.

  method get_data.
    data i type i.
    "isso é um exemplo nao usar select *!
    select * from mara into table gt_mara
             where mtart in rg_mtart
             and   matnr in rg_matnr.
    "isso é um exemplo neste caso seria melhor um innerjoin

    if gt_mara is not initial. "ou line_exists( gt_mara[ 1 ] ).- se existir um registro na tabela

        select matnr maktx from makt
         into corresponding fields of table gt_makt
         for all entries in gt_mara
         where matnr = gt_mara-matnr
           and spras = sy-langu.


    endif.
  endmethod.

  method process_data.
   "move os campos correspondentes com parametro de filtro
   gt_alv = corresponding #( gt_mara ).


   loop at gt_alv assigning field-symbol(<fs_alv>).
      try.
          <fs_alv>-matkx = gt_makt[ matnr = <fs_alv>-matnr ]-maktx.
          <fs_alv>-icon = icon_checked.
        catch cx_sy_itab_line_not_found.
          <fs_alv>-matkx = 'description not found!'.
          <fs_alv>-icon = icon_incomplete.

      endtry.


   endloop.

  endmethod.


  method show_alv.
    data: r_events     type ref to cl_salv_events_table,
          r_selections type ref to cl_salv_selections.

    try.
*       Monta lista ALV de acordo com a tabela GT_CTE:
        cl_salv_table=>factory(
          exporting
            list_display   = if_salv_c_bool_sap=>false
            "r_container    =
            "container_name = 'Name'
          importing
            r_salv_table   = r_alv
          changing
            t_table        = gt_alv ).

*       STATUS_GUI:
        r_alv->set_screen_status(
          pfstatus      =  'STATUS_1'
          report        =  sy-repid
          set_functions = r_alv->c_functions_all ).

      catch cx_salv_msg.

    endtry.

*   Seleção das linhas:
    r_selections = r_alv->get_selections( ).
    r_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*   Seta eventos
    r_events = r_alv->get_event( ).
    set handler on_user_command for r_events.


*   Exibe:
    r_alv->display( ).
  endmethod.

   method on_user_command.

*   Eventos:
    case e_salv_function.
      when '&TESTE'.
        break-point.
      when '&CANCEL'.
        leave program.

    endcase.

  endmethod.
endclass.


selection-screen begin of block b1 with frame title text-t01.

"PARAMETERS: p_path TYPE ibipparms-path.

select-options: s_matnr for lcl_app=>r_mara->matnr,
                s_mtart for lcl_app=>r_mara->mtart.

selection-screen end of block b1.

start-of-selection.

data r_app type ref to lcl_app.

"create OBJECT r_app.
r_app = new lcl_app( ).
r_app->rg_matnr = s_matnr[].
r_app->rg_mtart = s_mtart[].
r_app->start( ).
