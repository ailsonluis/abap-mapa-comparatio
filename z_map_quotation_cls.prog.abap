*&---------------------------------------------------------------------*
*& Include          Z_MAP_QUOTATION_CLS
*&---------------------------------------------------------------------*

class lcl_controller definition deferred.
class lcl_model definition deferred.
class lcl_view definition deferred.

data: r_controller type ref to lcl_controller,
      r_model type ref to lcl_model,
      r_view type ref to lcl_view.

class lcl_controller definition.
public section.
  class-methods:

    run.
  methods:
  handle_click_in_lst_abap.

endclass.


class lcl_model definition.
 public section.
 data: gt_cotacao type table of zv_rfp,
       gt_head type table of zv_rfp,
       gt_item type table of zv_rfp,
       gt_bestbuy type table of zv_rfp.


    methods:
      get_data,
      process_data,
      get_reference importing i_ekpo type zv_rfp
                    exporting e_vlrreferencia type ekpo-brtwr,

      best_buy importing i_ekpo type zv_rfp
               exporting e_bestbuy type zv_rfp,

      get_icon_perc importing i_perc  type epm_perc
                    returning value(i_icon) type icon_d,

      create_po importing i_anfnr type ekpo-anfnr,

      change_cotation importing i_ebeln type ekpo-ebeln
                                i_ebelp type ekpo-ebelp.




endclass.


class lcl_view definition.
   public section.

    methods:
      lst_1,
      lst_2.





endclass.

class lcl_controller implementation.

  method run.
    create object : r_controller, r_model, r_view.
    r_model->get_data( ).
    r_model->process_data( ).

    r_view->lst_1( ). "INDICAR O METHODO QUE VAI UTILIZAR LST_1 OU LST_2
  endmethod.


  method handle_click_in_lst_abap.
    data: pos_x type p decimals 3,
          pos_y type p decimals 3,
          pos_freeze type p decimals 3,
          pos_cot type p decimals 3,
          tabix type syst-tabix,
          wa_head type  zv_rfp.

    pos_freeze = 115. "posicão de congelamento da coluna
    pos_cot = 50. " quantidade de posições dentro da coluna com os dados de cotacao (Vlrnitario, VlrTotal, Perce. Icone)

    pos_x = sy-cucol + sy-staco - 2. "posicao inicial de clique

    if pos_x <= pos_freeze.
      exit.
    endif.

    "posicao de clique com o freeze
    pos_x = ( sy-cucol + sy-staco - pos_freeze ) mod pos_cot.

    if pos_x = 0.
      exit.
    endif.

    pos_y = ( sy-cucol + sy-staco - pos_freeze ) div pos_cot.

    tabix = 1 + pos_y.
    try.
      wa_head = corresponding #( r_model->gt_head[ tabix ]  ).
    catch cx_sy_itab_line_not_found.

    endtry.


    if sy-lisel cs 'Cotações'.

       "chama ME47 FLDVAL
       break-point.
    endif.



    if sy-lisel cs 'Criar doc. Compras'.

       message 'Pedido de compras criado!' &&  wa_head-ebeln type 'I'.
       r_model->create_po( EXPORTING i_anfnr = wa_head-ebeln ).
       break-point.
    endif.

    if sy-lisel cs 'Melhor escolha'.

       "chama ME47 FLDVAL
       break-point.
    endif.


    if sy-lisel cs 'Total'.
       break-point.
    endif.

    if sy-lisel cs '@0J@'.
       break-point.
    endif.
    if sy-lisel cs '@P7@'.
      "chama ME47 FLDVAL para o material
      set parameter id 'ANF' field wa_head-ebeln.
      set parameter id 'BSP' field wa_head-ebelp.
      call transaction 'ME47' and skip first screen.

    endif.



  endmethod.
endclass.

class lcl_model implementation.
  method get_data.

    data: wa_saida    type lfa1,

          wa_material type ent1027,
          vl_files    type i.

     select * from zv_rfp
      into table @gt_cotacao
      where submi in @s_submi
        and spras eq @sy-langu.

  endmethod.
  method process_data.
   data wa_head type zv_rfp.

    gt_item[] = gt_cotacao[].
    sort gt_cotacao by ebeln ebelp ascending.
    delete adjacent duplicates from gt_cotacao comparing ebeln ebelp.

    sort gt_item by matnr ascending.
    delete adjacent duplicates from gt_item comparing matnr.

    sort gt_item by ebeln ebelp ascending.

    loop at gt_cotacao assigning field-symbol(<fs_cotacao>).
      wa_head-ebeln =  <fs_cotacao>-ebeln.
      wa_head-lifnr =  <fs_cotacao>-lifnr.
      wa_head-name1 =  <fs_cotacao>-name1.
      wa_head-brtwr = <fs_cotacao>-brtwr.
      wa_head-inco1 = <fs_cotacao>-inco1.
      wa_head-inco2 = <fs_cotacao>-inco2.
      wa_head-zterm = <fs_cotacao>-zterm.
      wa_head-text1 = <fs_cotacao>-text1.
      wa_head-telf1 = <fs_cotacao>-telf1.
      wa_head-verkf = <fs_cotacao>-verkf.
      collect wa_head into gt_head.
      clear wa_head.
    "  cl_progress_indicator=>progress_indicate( exporting i_text = | Localizando... { sy-tabix } | i_processed = sy-tabix i_output_immediately = abap_true ).
    endloop.


  endmethod.

  method get_reference.
    "implementar methodo para buscar o ultimo preço para
    "i_ekpo-matnr.
    "i_ekpo-werks.

    e_vlrreferencia = 9.

  endmethod.

  method best_buy.

    data: gl_bestbuy type table of zv_rfp,
          wl_bestbuy type zv_rfp.

    gl_bestbuy = gt_cotacao.

    sort  gl_bestbuy ascending by matnr .

    delete adjacent duplicates from gl_bestbuy.

    delete gl_bestbuy where matnr ne i_ekpo-matnr.


    delete adjacent duplicates from gl_bestbuy comparing ebeln ebelp.

    sort  gl_bestbuy ascending by netpr . "ordena pelo menor preço

    try.
      wl_bestbuy  = gl_bestbuy[ 1 ] .

      append  wl_bestbuy to gt_bestbuy.
      e_bestbuy =  wl_bestbuy ."CORRESPONDING #( gl_bestbuy[ 1 ] ).
   catch cx_sy_itab_line_not_found.
   endtry.


  endmethod.
  method get_icon_perc.
    constants: vl_up(4) type c value '@LS@' ,
               vl_down(4) type c value '@LR@',
               vl_equal(4) type c value '@20@',
               vl_best(4)  type c value '@DF@'.

    i_icon = cond #(
                      when i_perc = 0 then vl_equal
                      when i_perc > 0 then vl_up
                      when i_perc < 0 then vl_down    ).

  endmethod.

  method create_po.
    " importing i_anfnr type ekpo-anfnr,
    "chama metodo para encerrar/ recusar os itens das cotações rejeitadas
    me->change_cotation( exporting i_ebeln = '123131313' i_ebelp = '10' ).

    "cria o pedido de compras da cotação ganhadora.


  endmethod.
  method change_cotation .
    "importing i_ebeln type ekpo-ebeln
     "                           i_ebelp type ekpo-ebelp.

    BREAK-POINT.
    MESSAGE 'Marca os itens das cotações rejeitadas' type 'I'.
  endmethod.
endclass.

class lcl_view implementation.

    method lst_1.
    data: boundary like sy-cucol.
    data: laenge type i ,

          lv_cot type ekko-ebeln.

*    data: gt_head type table of zekko_ekpo,
*          gt_item type table of zekko_ekpo.

    data: ablng type i value 57.

    constants: vl_up(4) type c value '@LS@' ,
               vl_down(4) type c value '@LR@',
               vl_equal(4) type c value '@20@',
               vl_best(4)  type c value '@DF@'.



    data(vl_lines) = lines( r_model->gt_head ).
    data(vl_colcotacao) = 49.
    data(vl_col_melhor) = 90.
    data(qtdcotacao) = vl_lines.

    laenge = 131 + ( qtdcotacao * 49 ) + 90."69.

*** Cabeçalho
     "new-line  no-scrolling.
     format color col_normal intensified.
      "new-line  no-scrolling.

     uline at /(115).
     write:  / sy-vline, 'RFP:', r_model->gt_head[ 1 ]-submi ,sy-vline.

     write: 'Nº Cotações:', '599', sy-vline..
     write: 'Data criação:', sy-datum,  sy-vline..
     write: 'Criado por:',(35) sy-uname,  sy-vline.
     uline at /(115).

    "skip 1.
***

  data: wa_coth type zv_rfp,
        wa_coti type zv_rfp,
        vl_head(50) type c.

* 1 linha com os numeros de cotações
  format color col_heading intensified off.
  uline at /(LAENGE).
  write: / sy-vline, (130) 'Cotações' centered, sy-vline.
  do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (49) wa_coth-ebeln , sy-vline.
  enddo.
  format color col_positive intensified on.
  write: (68) 'Melhor Compra' , sy-vline.
  format color col_heading intensified off.

* 2 Linha de nome dos fornecedores
  uline at /(LAENGE).
  write: / sy-vline, (130) 'Fornecedores' centered , sy-vline.
  do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (49) wa_coth-name1 , sy-vline.
  enddo.
  format color col_positive intensified on.
  write: (68) 'sugerida com base no preço total' ,  sy-vline.
  format color col_heading intensified off.
****

  uline at /(LAENGE).

  write:   / sy-vline, (23) 'Material' .

  write:   sy-vline, (40) 'Descrição'.
  write:   sy-vline, (3)  'UMB'.
  write:   sy-vline, (17) 'QtdCotação'.
  write:   sy-vline, (16) 'Vlr.referencia'.
  write:   sy-vline, (16) 'Vlr.Total Ref'.
  write:   sy-vline.
 " set left scroll-boundary.
 " boundary = sy-colno.

  do vl_lines times.
      write:    (14) 'Vlr.Unit'.
      write:   sy-vline, (16) 'Vlr.Total'.
      write:   sy-vline, (6) '%'.
      write:   sy-vline, (4) 'Ind', sy-vline.
  enddo.

  write:    (16) 'Cotação/Item'.
  write:   sy-vline, (14) 'Vlr.Unit'.
  write:   sy-vline, (16) 'Vlr.Total'.
  write:   sy-vline, (6) '%'.
  write:   sy-vline, (4) 'Ind', sy-vline.


uline at /(laenge).

******

*Detalhe dos itens
 data: gt_ekpo type table of ekpo.

  "select  * from ekpo into table gt_ekpo  up to 30 rows.
  data: vl_referencia type  ekpo-brtwr,
        vl_totalref type ekpo-brtwr,
        vl_totgerref type ekpo-brtwr,
        vl_totbestbuy type ekpo-brtwr,

        wl_bestby type zv_rfp,
        vl_per type epm_perc,
        vl_pertxt(6) type c.
        vl_referencia = vl_totalref = vl_totgerref = 0.
        vl_totbestbuy = 0.
  sort r_model->gt_item ascending by ebeln matnr.

  loop at r_model->gt_item into data(wa_ekpo).

    format color = 2 .
     " write: / sy-vline, '@P7@'.
     write: /  sy-vline, '@P7@',  wa_ekpo-matnr(18) .
     write:   sy-vline,  wa_ekpo-txz01.
     write:   sy-vline,  wa_ekpo-meins.
     write:   sy-vline,  wa_ekpo-ktmng.
     r_model->get_reference( exporting i_ekpo = wa_ekpo importing e_vlrreferencia = vl_referencia ).
     vl_totalref = vl_referencia *  wa_ekpo-ktmng. "vlr. total do item (qty x unit)
     vl_totgerref = vl_totgerref + vl_totalref. "vlr total dos itens (somatorio do vlr.total dos itens)
     write:   sy-vline,  vl_referencia CURRENCY 'BRL' .
     write:   sy-vline,   vl_totalref .
     do vl_lines times.
       wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .

       wa_coti = corresponding #( r_model->gt_cotacao[  ebeln = wa_coth-ebeln matnr = wa_ekpo-matnr ] ) .


        vl_pertxt = vl_per =  ( ( wa_coti-netpr  - vl_referencia   ) /  vl_referencia )  * 100. " ( ( vl_referencia - wa_coti-netpr  ) / ( wa_coti-netpr ) ) * -100 .
        write:   sy-vline,  wa_coti-netpr.
        write:   sy-vline,  wa_coti-brtwr.
        write:   sy-vline,  vl_pertxt.
        write:   sy-vline,  r_model->get_icon_perc( exporting i_perc = vl_per ).
     enddo.


     "Metodo para mostrar o menor preço do item
     r_model->best_buy( exporting i_ekpo = wa_ekpo importing e_bestbuy = wl_bestby  ).
     vl_pertxt = vl_per =  ( ( wl_bestby-netpr - vl_referencia    ) /  vl_referencia )   * 100 .
     vl_totbestbuy = vl_totbestbuy  + wl_bestby-brtwr.
     write:   sy-vline, wl_bestby-ebeln, wl_bestby-ebelp.
     write:   sy-vline, (14)  wl_bestby-netpr.
     write:   sy-vline, (16) wl_bestby-brtwr.
     write:   sy-vline,  vl_pertxt.
     write:   sy-vline,  r_model->get_icon_perc( exporting i_perc = vl_per ).
     "write:   sy-vline,  '@12@'.
     write:   sy-vline.
  endloop.

  uline at /(LAENGE).

****TOTAL

  format color col_total intensified on.
  uline at /(LAENGE).
  write: / sy-vline, (111) 'Total' centered.
  write:  sy-vline, (16)  vl_totgerref,  sy-vline.
*  set left scroll-boundary.
*  boundary = sy-colno.

  do vl_lines times.
    data(vlr_totalcotacao) = r_model->gt_head[ sy-index ]-brtwr.
    vl_pertxt = vl_per =   ( ( vlr_totalcotacao -  vl_totgerref    ) / ( vl_totgerref ) ) * 100 .
    write:    (14) '.'.
    write:   sy-vline, (16) vlr_totalcotacao.
    write:   sy-vline, (6) vl_pertxt.
    write:   sy-vline, (4) r_model->get_icon_perc( exporting i_perc = vl_per ).
    write:   sy-vline.
  enddo.

   "Metodo para mostrar o menor preço total

     vl_pertxt = vl_per =  ( ( vl_totbestbuy - vl_totgerref      ) /  vl_totgerref   )   * 100 .
     "write:   sy-vline, wl_bestby-ebeln, wl_bestby-ebelp.
     write:    (16) ''.
     write:   (16) ''.
     write:   sy-vline, (16) vl_totbestbuy.
     write:   sy-vline,  vl_pertxt.
     write:   sy-vline,  r_model->get_icon_perc( exporting i_perc = vl_per ).
     "write:   sy-vline,  '@12@'.
     write:   sy-vline.

uline at /(laenge).

**** Condição de pagamento
  format color col_heading intensified off.
  uline at /(LAENGE).
  write: / sy-vline, (130) 'Condição de Pagamento' centered, sy-vline.
*  set left scroll-boundary.
*  boundary = sy-colno.

  do vl_lines times.
    wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
    "
    write:    (4) wa_coth-zterm, (44) wa_coth-text1, sy-vline.
  enddo.

  uline at /(laenge).
****


**** Condição frete
  format color col_heading intensified off.
  uline at /(LAENGE).
  write: / sy-vline, (130) 'Frete' centered, sy-vline.
*  set left scroll-boundary.
*  boundary = sy-colno.

  do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
   " CONCATENATE wa_coth-inco1 ' ' wa_coth-inco2 into vl_head.
    vl_head =  wa_coth-inco1   && | | &&    wa_coth-inco2 .
     write:    (49) vl_head , sy-vline.
  enddo.

  uline at /(laenge).
*****
*
***** Escolher melhor opção de compra
* format color col_heading intensified off.
*  uline at /(LAENGE).
*  write: / sy-vline, (130) 'Escolha' centered, sy-vline.
**  set left scroll-boundary.
**  boundary = sy-colno.
*
*  do vl_lines times.
*     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
*     "Chamar metodo para verificar se ja criou o pedido e indicar texto que deve aparecer
*     "r_model->get_status_coth(exporting......)
*     data(vl_texto) = 'Criar doc. Compras'.
*     write:    (49) vl_texto , sy-vline.
*  enddo.
*     vl_texto = 'Melhor escolha'.
*     write:    (60) vl_texto , sy-vline.
*  uline at /(laenge).
*
*****
*
*
***** Observações
*  format color col_heading intensified off.
*  uline at /(LAENGE).
*  write: / sy-vline, (118) '@0J@'  , ' Observação' centered, sy-vline.
* " set left scroll-boundary.
*
*
*  uline at /(laenge).
*  format color col_heading intensified off.
*****



    endmethod.



 method lst_2.
    data: boundary like sy-cucol.
    data: laenge type i ,

          lv_cot type ekko-ebeln.

*    data: gt_head type table of zekko_ekpo,
*          gt_item type table of zekko_ekpo.

    data: ablng type i value 57.

    constants: vl_up(4) type c value '@LS@' ,
               vl_down(4) type c value '@LR@',
               vl_equal(4) type c value '@20@',
               vl_best(4)  type c value '@DF@'.



    data(vl_lines) = lines( r_model->gt_head ).
    data(vl_colcotacao) = 48.
    data(vl_col_melhor) = 90.
    data(qtdcotacao) = vl_lines.

    laenge = 71 + ( qtdcotacao * 37 ) + 48."69.

*** Cabeçalho
     "new-line  no-scrolling.
     format color col_normal intensified.
      "new-line  no-scrolling.

     uline at /(115).
     write:  / sy-vline, 'RFP:', r_model->gt_head[ 1 ]-submi ,sy-vline.

     write: 'Nº Cotações:', '599', sy-vline..
     write: 'Data criação:', sy-datum,  sy-vline..
     write: 'Criado por:',(35) sy-uname,  sy-vline.
     uline at /(115).

    "skip 1.
***

  data: wa_coth type zv_rfp,
        wa_coti type zv_rfp,
        vl_head(50) type c.



*Detalhe dos itens
 data: gt_ekpo type table of ekpo.

  "select  * from ekpo into table gt_ekpo  up to 30 rows.
  data: vl_referencia type  ekpo-brtwr,
        vl_totalref type ekpo-brtwr,
        vl_totgerref type ekpo-brtwr,
        vl_totbestbuy type ekpo-brtwr,

        wl_bestby type zv_rfp,
        vl_per type epm_perc,
        vl_pertxt(6) type c.
        vl_referencia = vl_totalref = vl_totgerref = 0.
        vl_totbestbuy = 0.

  sort r_model->gt_item ascending by ebeln matnr.



*** Formato reduzido**


  uline at /(LAENGE).
  write: / sy-vline,(69) 'Cotações'.
  write:  sy-vline.

  do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (35) wa_coth-ebeln , sy-vline.
  enddo.
  format color col_positive intensified on.
  write: (41) 'Melhor Compra' , sy-vline.
  format color col_heading intensified off.



* LINHA 2 - DADOS DE FORNECEDORES
  uline at /(LAENGE).
  write:  / sy-vline,(69) 'Fornecedores', sy-vline.
   do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (35) wa_coth-name1 , sy-vline.

   enddo.
   write: (41) space , sy-vline.

   write: / sy-vline,(69) 'Contato', sy-vline.
    do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (35) wa_coth-verkf , sy-vline.

   enddo.

  write: (41) space , sy-vline.
  write: / sy-vline,(69) 'Telefone', sy-vline.
    do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (35) wa_coth-telf1 , sy-vline.

   enddo.

  format color col_positive intensified on.
  write: (41) 'sugerida com base no preço total' ,  sy-vline.
  format color col_heading intensified off.




  uline at /(LAENGE).
  write: / sy-vline,(5) 'Item', (45) 'Material' .
  write:   sy-vline, (15) space.

  do vl_lines times.
     write:   sy-vline, (15) space.
     write:   sy-vline, (10) space.
     write:   sy-vline, (4) space.
  ENDDO.
  write: sy-vline,(16) space. "cotacao /item"
  write:   sy-vline, (15) space. "Vlr.unitario"
  write:   sy-vline, (4) space. "Vlr.Total"
  write:  sy-vline.


  new-line.
  write: / sy-vline,(5) space, (45) 'Descrição'.
  write:   sy-vline,(15) 'Vlr.Referencia'.

  do vl_lines times.
     write:   sy-vline,(15) 'Vlr.Unitário'.
     write:   sy-vline,(10) 'Apr/Rec'.
     write:   sy-vline,(4) '%'.
  enddo.

  write: sy-vline,(10) 'Cotação',(5) 'Item'.
  write:   sy-vline, (15) 'Vlr.Unitário'.
  write:   sy-vline, (4) '%'.
  write:  sy-vline.

  new-line.
  write: / sy-vline,(5) space, (30) 'Quantidade', (14) 'Unidade Básica'.
  write:   sy-vline,(15) 'Vlr.Total Ref'.

  do vl_lines times.
    write:   sy-vline,(15) 'Vlr.Total'.
    write:   sy-vline,(10) 'Desconto'.
    write:   sy-vline,(4)  'Icone'.
  enddo.

  write: sy-vline,(16) space.
  write:   sy-vline, (15) 'Vlr.Total'.
  write:   sy-vline, (4) 'Icone'.
  write:  sy-vline.






"***************
  format color = 2 .
  uline at /(LAENGE).
 loop at r_model->gt_item into data(wa_item).

  write: / sy-vline,(5) wa_item-ebelp , (45) wa_item-matnr . "L1/C1 - Item / Material"
  write:   sy-vline, (15) space. "L1/c2 - vazio"

  do vl_lines times. " L1/C3-5 - Vazio"
     write:   sy-vline, (15) space.
     write:   sy-vline, (10) space.
     write:   sy-vline, (4) space.
  ENDDO.

  write: sy-vline,(16) space. "L1/C?+3 "
  write:   sy-vline, (15) space. "L1"
  write:   sy-vline, (4) space. "L1"
  write:  sy-vline.


  new-line.
  r_model->get_reference( exporting i_ekpo = wa_item importing e_vlrreferencia = vl_referencia ).
  vl_totalref = vl_referencia *  wa_item-ktmng. "vlr. total do item (qty x unit)
  vl_totgerref = vl_totgerref + vl_totalref. "vlr total dos itens (somatorio do vlr.total dos itens)

  write: / sy-vline,(5) space, (45) wa_item-txz01. "L2 - Decrição do material
  write:   sy-vline,(15) vl_referencia." L2 - Valor de Referencia"

  do vl_lines times.
    wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
    wa_coti = corresponding #( r_model->gt_cotacao[  ebeln = wa_coth-ebeln matnr = wa_item-matnr ] ) .
    vl_pertxt = vl_per =  ( ( wa_coti-netpr  - vl_referencia   ) /  vl_referencia )  * 100. " ( ( vl_referencia - wa_coti-netpr  ) / ( wa_coti-netpr ) ) * -100 .
     write:   sy-vline,(15) wa_coti-netpr. "L2 - Valor Unitario de cada cotação/item"
     write:   sy-vline,(10) wa_coti-agmem. "L2 - "
     write:   sy-vline,(4) vl_pertxt. "L2 - Percentual de diferença"
  enddo.
    "****
     r_model->best_buy( exporting i_ekpo = wa_item importing e_bestbuy = wl_bestby  ).
     vl_pertxt = vl_per =  ( ( wl_bestby-netpr - vl_referencia    ) /  vl_referencia )   * 100 .
     vl_totbestbuy = vl_totbestbuy  + wl_bestby-brtwr.

  "****
   "Melhor Cotação L1
    write: sy-vline,(10) wl_bestby-ebeln ,(5) wl_bestby-ebelp. "L2 - Cotação /Item"
    write:   sy-vline, (15) wl_bestby-netpr. "L2 - Unitário melhor cotação/item"
    write:   sy-vline, (4) vl_pertxt . "L2 - Percentual Melhor cotação/item"
    write:  sy-vline.

    new-line.
    write: / sy-vline,(5) space, (30) wa_item-brtwr LEFT-JUSTIFIED ,(14) wa_item-meins left-justified. "L3 - Quantidade /unidade"
    write:   sy-vline,(15) vl_totalref. "L3 - Vlr.Referencia total"
  "endif.

  do vl_lines times.
    wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
    wa_coti = corresponding #( r_model->gt_cotacao[  ebeln = wa_coth-ebeln matnr = wa_item-matnr ] ) .
    vl_pertxt = vl_per =  ( ( wa_coti-netpr  - vl_referencia   ) /  vl_referencia )  * 100. " ( ( vl_referencia - wa_coti-netpr  ) / ( wa_coti-netpr ) ) * -100 .
    write:   sy-vline,(15) wa_coti-brtwr. "L3 - Valor total de cada cotação/item"
    write:   sy-vline,(10)  '10,30'.
    write:   sy-vline,(4)    r_model->get_icon_perc( exporting i_perc = vl_per )..
  enddo.

  write: sy-vline,(16) space.
  write:   sy-vline, (15) wl_bestby-brtwr.
  write:   sy-vline, (4) r_model->get_icon_perc( exporting i_perc = vl_per ).
  write:  sy-vline.

  uline at /(laenge).


ENDLOOP.

"***************
"Total


* DADOS RODAPE
  uline at /(LAENGE).
  write:  / sy-vline,(69) 'Total', sy-vline.
   do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     "Criar methodo que passe o parametro wa_coth-ebeln e retorne o somatorio do preço bruto
     write: (35) '99999999,99' , sy-vline.

  enddo.
   "criar method para retornar a somatoria dos melhores item/cotação
   write: (41) '99999999,99' , sy-vline.

   write: / sy-vline,(69) 'Condições de Pagamento', sy-vline.
    do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (35) wa_coth-zterm && | | &&  wa_coth-text1 , sy-vline.

   enddo.

  write: (41) space , sy-vline.
  write: / sy-vline,(69) 'Frete', sy-vline.
    do vl_lines times.
     wa_coth = corresponding #( r_model->gt_head[ sy-index ] ) .
     write: (35)  wa_coth-inco1   && | | &&    wa_coth-inco2  , sy-vline.

   enddo.

 uline at /(laenge).
    endmethod.

endclass.
