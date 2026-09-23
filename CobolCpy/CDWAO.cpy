000100* GENERATION OF COBOL HOST STRUCTURE FROM CDWAO-TAB                       
000200  01 CDWAO.                                                               
000300*              ALL REPORTED COMPANYS SALES VALUES                         
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
001700   03 KDORDTYP-DEAL                     PIC X(1).                         
001800*              ORDERTYP HOS DEALER                                        
001900   03 KDINVCR                           PIC X(2).                         
002000*              TYP AV FAKTURERING/KREDITERING                             
002100*              OD = DEBITERING                                            
002200*              OC = KREDITERING                                           
002300*              TYPE OF INVOICE LINE                                       
002400*              OD = DEBIT                                                 
002500*              OC = CREDIT                                                
002600   03 KDFAKRAD                          PIC X(1).                         
002700*              TYP AV FAKTURARAD                                          
002800*              1 = NORMAL FAKTURARAD                                      
002900*              2 = BYTES                                                  
003000*              3 = LEVERANSANMÄRKNING                                     
003100*              4 = BUY-BACK                                               
003200*              5 = PROD JUSTERING                                         
003300*              TYPE OF INVOICE LINE                                       
003400*              1 = NORMAL                                                 
003500*              2 = EXCHANGE                                               
003600*              3 = DISCREPANCY                                            
003700*              4 = BUY-BACK                                               
003800*              5 = PROCE ADJUSTMENT                                       
003900   03 KDPRODSL                          PIC S9(3) COMP-3.                 
004000*              PRODUKTSLAG                                                
004100   03 KDPRODSL-IMP                      PIC S9(3) COMP-3.                 
004200*              PRODUKTSLAG LOKALT HOS IMPORTÖR                            
004300   03 IDLEVNR-IMP                       PIC S9(5) COMP-3.                 
004400*              LEVERANTÖR ENLIGT AVTAL                                    
004500   03 IDFKNGRP                          PIC S9(5) COMP-3.                 
004600*              FUNKTIONSGRUPP                                             
004700   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
004800*              VÄRDE TILL SUGGESTED RETAIL                                
004900   03 SURET                             PIC S9(13)V9(2) COMP-3.           
005000*              VÄRDE TILL SUGGESTED RETAIL                                
005100   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
005200*              VÄRDE TILL DEALER NET                                      
005300   03 SULANDCO                          PIC S9(13)V9(2) COMP-3.           
005400*              VÄRDE TILL LANDED COST                                     
005500   03 SUPNET                            PIC S9(13)V9(2) COMP-3.           
005600*              VÄRDE TILL PURCHASE NET                                    
005700   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
005800*              VÄRDE TILL STANDARD LANDING COST                           
005900   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
006000*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
006100   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
006200*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
006300   03 SULEVANT                          PIC S9(9) COMP-3.                 
006400*              SUMMA LEVERERAT ANTAL                                      
006500*              AV 1 ARTIKEL                                               
006600   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
006700*              VÄRDE TILL DEALER NET                                      
006800   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
006900*              VÄRDE TILL DEALER NET                                      
007000   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
007100*              VÄRDE TILL LANDED COST BERÄKNAT                            
007200   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
007300*              VÄRDE TILL LANDED COST BERÄKNAT                            
007400   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
007500*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
007600   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
007700*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
007800   03 SUPNET-LOC                        PIC S9(13)V9(2) COMP-3.           
007900*              VÄRDE TILL PURCHASE NET                                    
008000   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
008100*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
008200   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
008300*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
008400   03 FLLOKINK                          PIC X(1).                         
008500*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
008600*              AV IMPORTÖREN                                              
008700   03 KDVALISO                          PIC X(3).                         
008800*              VALUTAKOD ENLIGT ISO-STANDARD.                             
008900   03 DADATTID                          PIC X(14).                        
009000*                                                                         
009100*** END OF VILMAII-COPY LENGTH= 209 OLD LENGTH=                           
