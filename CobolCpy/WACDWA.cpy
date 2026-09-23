000100* GENERATION OF COBOL HOST STRUCTURE FROM WACDWA-TAB                      
000200  01 WACDWA.                                                              
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
001300   03 IDARTNR20                         PIC X(20).                        
001400*              20-STÄLLIGT ARTIKELNUMMER FÖR AS400 (VIPS)                 
001500*              FORMATET ÄR HÖGERJUSTERAT MED INLEDANDE                    
001600*              BLANKTECKEN, OCH UTAN INLEDANDE NOLLOR.                    
001700   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001800*              PRODUKTSLAG                                                
001900   03 KDPRODSL-IMP                      PIC S9(3) COMP-3.                 
002000*              PRODUKTSLAG LOKALT HOS IMPORTÖR                            
002100   03 IDLEVNR-IMP                       PIC S9(5) COMP-3.                 
002200*              LEVERANTÖR ENLIGT AVTAL                                    
002300   03 IDFKNGRP                          PIC S9(5) COMP-3.                 
002400*              FUNKTIONSGRUPP                                             
002500   03 KVREPAIR                          PIC S9(9) COMP-3.                 
002600*              REPAIR QUANTITY                                            
002700   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
002800*              VÄRDE TILL SUGGESTED RETAIL                                
002900   03 SURET                             PIC S9(13)V9(2) COMP-3.           
003000*              VÄRDE TILL SUGGESTED RETAIL                                
003100   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
003200*              VÄRDE TILL STANDARD LANDING COST                           
003300   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
003400*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
003500   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
003600*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
003700   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
003800*              VÄRDE TILL DEALER NET                                      
003900   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
004000*              VÄRDE TILL DEALER NET                                      
004100   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
004200*              VÄRDE TILL LANDED COST BERÄKNAT                            
004300   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
004400*              VÄRDE TILL LANDED COST BERÄKNAT                            
004500   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
004600*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004700   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
004800*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004900   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
005000*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
005100   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
005200*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
005300   03 FLLOKINK                          PIC X(1).                         
005400*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
005500*              AV IMPORTÖREN                                              
005600   03 KDVALISO                          PIC X(3).                         
005700*              VALUTAKOD ENLIGT ISO-STANDARD.                             
005800   03 DADATTID                          PIC X(14).                        
005900*                                                                         
006000*** END OF VILMAII-COPY LENGTH= 173 OLD LENGTH=                           
