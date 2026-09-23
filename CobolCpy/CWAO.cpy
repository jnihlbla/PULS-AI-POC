000100* GENERATION OF COBOL HOST STRUCTURE FROM CWAO-TAB                        
000200  01 CWAO.                                                                
000300*              ALL REPORTED COMPANYS SALES VALUES                         
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000700*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
000800   03 IDARTNR20                         PIC X(20).                        
000900*              20-STÄLLIGT ARTIKELNUMMER FÖR AS400 (VIPS)                 
001000*              FORMATET ÄR HÖGERJUSTERAT MED INLEDANDE                    
001100*              BLANKTECKEN, OCH UTAN INLEDANDE NOLLOR.                    
001200   03 KDORDTYP-DEAL                     PIC X(1).                         
001300*              ORDERTYP HOS DEALER                                        
001400   03 KDINVCR                           PIC X(2).                         
001500*              TYP AV FAKTURERING/KREDITERING                             
001600*              OD = DEBITERING                                            
001700*              OC = KREDITERING                                           
001800*              TYPE OF INVOICE LINE                                       
001900*              OD = DEBIT                                                 
002000*              OC = CREDIT                                                
002100   03 KDFAKRAD                          PIC X(1).                         
002200*              TYP AV FAKTURARAD                                          
002300*              1 = NORMAL FAKTURARAD                                      
002400*              2 = BYTES                                                  
002500*              3 = LEVERANSANMÄRKNING                                     
002600*              4 = BUY-BACK                                               
002700*              5 = PROD JUSTERING                                         
002800*              TYPE OF INVOICE LINE                                       
002900*              1 = NORMAL                                                 
003000*              2 = EXCHANGE                                               
003100*              3 = DISCREPANCY                                            
003200*              4 = BUY-BACK                                               
003300*              5 = PROCE ADJUSTMENT                                       
003400   03 DAAARP                            PIC S9(7) COMP-3.                 
003500*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
003600*              12 PER ÅR                                                  
003700   03 KDPRODSL                          PIC S9(3) COMP-3.                 
003800*              PRODUKTSLAG                                                
003900   03 KDPRODSL-IMP                      PIC S9(3) COMP-3.                 
004000*              PRODUKTSLAG LOKALT HOS IMPORTÖR                            
004100   03 IDLEVNR-IMP                       PIC S9(5) COMP-3.                 
004200*              LEVERANTÖR ENLIGT AVTAL                                    
004300   03 IDFKNGRP                          PIC S9(5) COMP-3.                 
004400*              FUNKTIONSGRUPP                                             
004500   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
004600*              VÄRDE TILL SUGGESTED RETAIL                                
004700   03 SURET                             PIC S9(13)V9(2) COMP-3.           
004800*              VÄRDE TILL SUGGESTED RETAIL                                
004900   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
005000*              VÄRDE TILL DEALER NET                                      
005100   03 SULANDCO                          PIC S9(13)V9(2) COMP-3.           
005200*              VÄRDE TILL LANDED COST                                     
005300   03 SUPNET                            PIC S9(13)V9(2) COMP-3.           
005400*              VÄRDE TILL PURCHASE NET                                    
005500   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
005600*              VÄRDE TILL STANDARD LANDING COST                           
005700   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
005800*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
005900   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
006000*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
006100   03 SULEVANT                          PIC S9(9) COMP-3.                 
006200*              SUMMA LEVERERAT ANTAL                                      
006300*              AV 1 ARTIKEL                                               
006400   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
006500*              VÄRDE TILL DEALER NET                                      
006600   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
006700*              VÄRDE TILL DEALER NET                                      
006800   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
006900*              VÄRDE TILL LANDED COST BERÄKNAT                            
007000   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
007100*              VÄRDE TILL LANDED COST BERÄKNAT                            
007200   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
007300*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
007400   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
007500*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
007600   03 SUPNET-LOC                        PIC S9(13)V9(2) COMP-3.           
007700*              VÄRDE TILL PURCHASE NET                                    
007800   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
007900*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
008000   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
008100*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
008200   03 FLLOKINK                          PIC X(1).                         
008300*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
008400*              AV IMPORTÖREN                                              
008500   03 KDVALISO                          PIC X(3).                         
008600*              VALUTAKOD ENLIGT ISO-STANDARD.                             
008700*                                                                         
008800*** END OF VILMAII-COPY LENGTH= 189 OLD LENGTH=                           
