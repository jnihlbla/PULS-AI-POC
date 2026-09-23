000100* GENERATION OF COBOL HOST STRUCTURE FROM WACDWP-TAB                      
000200  01 WACDWP.                                                              
000300*              ALL REPORTED COMPANYS WARRANTY                             
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDDEALER                          PIC X(6).                         
000700*              DEALER KUNDNUMMER                                          
000800   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000900*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
001000   03 DAAARP                            PIC S9(7) COMP-3.                 
001100*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
001200*              12 PER ÅR                                                  
001300   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001400*              PRODUKTSLAG                                                
001500   03 KDPRODSL-IMP                      PIC S9(3) COMP-3.                 
001600*              PRODUKTSLAG LOKALT HOS IMPORTÖR                            
001700   03 IDLEVNR-IMP                       PIC S9(5) COMP-3.                 
001800*              LEVERANTÖR ENLIGT AVTAL                                    
001900   03 IDFKNGRP                          PIC S9(5) COMP-3.                 
002000*              FUNKTIONSGRUPP                                             
002100   03 KVREPAIR                          PIC S9(9) COMP-3.                 
002200*              REPAIR QUANTITY                                            
002300   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
002400*              VÄRDE TILL SUGGESTED RETAIL                                
002500   03 SURET                             PIC S9(13)V9(2) COMP-3.           
002600*              VÄRDE TILL SUGGESTED RETAIL                                
002700   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
002800*              VÄRDE TILL STANDARD LANDING COST                           
002900   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
003000*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
003100   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
003200*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
003300   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
003400*              VÄRDE TILL DEALER NET                                      
003500   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
003600*              VÄRDE TILL DEALER NET                                      
003700   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
003800*              VÄRDE TILL LANDED COST BERÄKNAT                            
003900   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
004000*              VÄRDE TILL LANDED COST BERÄKNAT                            
004100   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
004200*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004300   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
004400*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004500   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
004600*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004700   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
004800*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
004900   03 FLLOKINK                          PIC X(1).                         
005000*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
005100*              AV IMPORTÖREN                                              
005200   03 KDVALISO                          PIC X(3).                         
005300*              VALUTAKOD ENLIGT ISO-STANDARD.                             
005400*                                                                         
005500*** END OF VILMAII-COPY LENGTH= 139 OLD LENGTH=                           
