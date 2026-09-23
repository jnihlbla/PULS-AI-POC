000100* GENERATION OF COBOL HOST STRUCTURE FROM CDWPO-TAB                       
000200  01 CDWPO.                                                               
000300*              AGGREGATED FROM SOURCETABLE CDWAOXX                        
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDDEALER                          PIC X(6).                         
000700*              DEALER KUNDNUMMER                                          
000800   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000900*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
001000   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001100*              PRODUKTSLAG                                                
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
003400   03 FLLOKINK                          PIC X(1).                         
003500*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
003600*              AV IMPORTÖREN                                              
003700   03 DAAARP                            PIC S9(7) COMP-3.                 
003800*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
003900*              12 PER ÅR                                                  
004000   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
004100*              VÄRDE TILL SUGGESTED RETAIL                                
004200   03 SURET                             PIC S9(13)V9(2) COMP-3.           
004300*              VÄRDE TILL SUGGESTED RETAIL                                
004400   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
004500*              VÄRDE TILL DEALER NET                                      
004600   03 SULANDCO                          PIC S9(13)V9(2) COMP-3.           
004700*              VÄRDE TILL LANDED COST                                     
004800   03 SUPNET                            PIC S9(13)V9(2) COMP-3.           
004900*              VÄRDE TILL PURCHASE NET                                    
005000   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
005100*              VÄRDE TILL STANDARD LANDING COST                           
005200   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
005300*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
005400   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
005500*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
005600   03 SULEVANT                          PIC S9(9) COMP-3.                 
005700*              SUMMA LEVERERAT ANTAL                                      
005800*              AV 1 ARTIKEL                                               
005900   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
006000*              VÄRDE TILL DEALER NET                                      
006100   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
006200*              VÄRDE TILL DEALER NET                                      
006300   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
006400*              VÄRDE TILL LANDED COST BERÄKNAT                            
006500   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
006600*              VÄRDE TILL LANDED COST BERÄKNAT                            
006700   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
006800*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
006900   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
007000*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
007100   03 SUPNET-LOC                        PIC S9(13)V9(2) COMP-3.           
007200*              VÄRDE TILL PURCHASE NET                                    
007300   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
007400*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
007500   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
007600*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
007700   03 KDVALISO                          PIC X(3).                         
007800*              VALUTAKOD ENLIGT ISO-STANDARD.                             
007900*                                                                         
008000*** END OF VILMAII-COPY LENGTH= 167 OLD LENGTH=                           
