000100* GENERATION OF COBOL HOST STRUCTURE FROM WACWA-TAB                       
000200  01 WACWA.                                                               
000300*              ALL REPORTED COMPANYS WARRANTY                             
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000700*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
000800   03 DAAARP                            PIC S9(7) COMP-3.                 
000900*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
001000*              12 PER ÅR                                                  
001100   03 IDARTNR20                         PIC X(20).                        
001200*              20-STÄLLIGT ARTIKELNUMMER FÖR AS400 (VIPS)                 
001300*              FORMATET ÄR HÖGERJUSTERAT MED INLEDANDE                    
001400*              BLANKTECKEN, OCH UTAN INLEDANDE NOLLOR.                    
001500   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001600*              PRODUKTSLAG                                                
001700   03 KDPRODSL-IMP                      PIC S9(3) COMP-3.                 
001800*              PRODUKTSLAG LOKALT HOS IMPORTÖR                            
001900   03 IDLEVNR-IMP                       PIC S9(5) COMP-3.                 
002000*              LEVERANTÖR ENLIGT AVTAL                                    
002100   03 IDFKNGRP                          PIC S9(5) COMP-3.                 
002200*              FUNKTIONSGRUPP                                             
002300   03 KVREPAIR                          PIC S9(9) COMP-3.                 
002400*              REPAIR QUANTITY                                            
002500   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
002600*              VÄRDE TILL SUGGESTED RETAIL                                
002700   03 SURET                             PIC S9(13)V9(2) COMP-3.           
002800*              VÄRDE TILL SUGGESTED RETAIL                                
002900   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
003000*              VÄRDE TILL STANDARD LANDING COST                           
003100   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
003200*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
003300   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
003400*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
003500   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
003600*              VÄRDE TILL DEALER NET                                      
003700   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
003800*              VÄRDE TILL DEALER NET                                      
003900   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
004000*              VÄRDE TILL LANDED COST BERÄKNAT                            
004100   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
004200*              VÄRDE TILL LANDED COST BERÄKNAT                            
004300   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
004400*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004500   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
004600*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004700   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
004800*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004900   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
005000*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
005100   03 FLLOKINK                          PIC X(1).                         
005200*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
005300*              AV IMPORTÖREN                                              
005400   03 KDVALISO                          PIC X(3).                         
005500*              VALUTAKOD ENLIGT ISO-STANDARD.                             
005600*                                                                         
005700*** END OF VILMAII-COPY LENGTH= 153 OLD LENGTH=                           
