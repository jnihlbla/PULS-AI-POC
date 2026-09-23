000100* GENERATION OF COBOL HOST STRUCTURE FROM CWPO-TAB                        
000200  01 CWPO.                                                                
000300*              AGGREGATED FROM SOURCETABLE CDWAOXX                        
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000700*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
000800   03 KDPRODSL                          PIC S9(3) COMP-3.                 
000900*              PRODUKTSLAG                                                
001000   03 KDORDTYP-DEAL                     PIC X(1).                         
001100*              ORDERTYP HOS DEALER                                        
001200   03 KDINVCR                           PIC X(2).                         
001300*              TYP AV FAKTURERING/KREDITERING                             
001400*              OD = DEBITERING                                            
001500*              OC = KREDITERING                                           
001600*              TYPE OF INVOICE LINE                                       
001700*              OD = DEBIT                                                 
001800*              OC = CREDIT                                                
001900   03 KDFAKRAD                          PIC X(1).                         
002000*              TYP AV FAKTURARAD                                          
002100*              1 = NORMAL FAKTURARAD                                      
002200*              2 = BYTES                                                  
002300*              3 = LEVERANSANMÄRKNING                                     
002400*              4 = BUY-BACK                                               
002500*              5 = PROD JUSTERING                                         
002600*              TYPE OF INVOICE LINE                                       
002700*              1 = NORMAL                                                 
002800*              2 = EXCHANGE                                               
002900*              3 = DISCREPANCY                                            
003000*              4 = BUY-BACK                                               
003100*              5 = PROCE ADJUSTMENT                                       
003200   03 FLLOKINK                          PIC X(1).                         
003300*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
003400*              AV IMPORTÖREN                                              
003500   03 DAAARP                            PIC S9(7) COMP-3.                 
003600*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
003700*              12 PER ÅR                                                  
003800   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
003900*              VÄRDE TILL SUGGESTED RETAIL                                
004000   03 SURET                             PIC S9(13)V9(2) COMP-3.           
004100*              VÄRDE TILL SUGGESTED RETAIL                                
004200   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
004300*              VÄRDE TILL DEALER NET                                      
004400   03 SULANDCO                          PIC S9(13)V9(2) COMP-3.           
004500*              VÄRDE TILL LANDED COST                                     
004600   03 SUPNET                            PIC S9(13)V9(2) COMP-3.           
004700*              VÄRDE TILL PURCHASE NET                                    
004800   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
004900*              VÄRDE TILL STANDARD LANDING COST                           
005000   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
005100*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
005200   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
005300*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
005400   03 SULEVANT                          PIC S9(9) COMP-3.                 
005500*              SUMMA LEVERERAT ANTAL                                      
005600*              AV 1 ARTIKEL                                               
005700   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
005800*              VÄRDE TILL DEALER NET                                      
005900   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
006000*              VÄRDE TILL DEALER NET                                      
006100   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
006200*              VÄRDE TILL LANDED COST BERÄKNAT                            
006300   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
006400*              VÄRDE TILL LANDED COST BERÄKNAT                            
006500   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
006600*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
006700   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
006800*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
006900   03 SUPNET-LOC                        PIC S9(13)V9(2) COMP-3.           
007000*              VÄRDE TILL PURCHASE NET                                    
007100   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
007200*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
007300   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
007400*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
007500   03 KDVALISO                          PIC X(3).                         
007600*              VALUTAKOD ENLIGT ISO-STANDARD.                             
007700*                                                                         
007800*** END OF VILMAII-COPY LENGTH= 161 OLD LENGTH=                           
