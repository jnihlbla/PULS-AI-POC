000100* GENERATION OF COBOL HOST STRUCTURE FROM REFCDWP-TAB                     
000200  01 REFCDWP.                                                             
000300*                                                                         
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDDEALER                          PIC X(6).                         
000700*              DEALER KUNDNUMMER                                          
000800   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000900*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
001000   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001100*              PRODUKTSLAG                                                
001200   03 DAAARP                            PIC S9(7) COMP-3.                 
001300*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
001400*              12 PER ÅR                                                  
001500   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
001600*              VÄRDE TILL SUGGESTED RETAIL                                
001700   03 SURET                             PIC S9(13)V9(2) COMP-3.           
001800*              VÄRDE TILL SUGGESTED RETAIL                                
001900   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
002000*              VÄRDE TILL DEALER NET                                      
002100   03 SULANDCO                          PIC S9(13)V9(2) COMP-3.           
002200*              VÄRDE TILL LANDED COST                                     
002300   03 SUPNET                            PIC S9(13)V9(2) COMP-3.           
002400*              VÄRDE TILL PURCHASE NET                                    
002500   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
002600*              VÄRDE TILL STANDARD LANDING COST                           
002700   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
002800*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
002900   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
003000*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
003100   03 SULEVANT                          PIC S9(9) COMP-3.                 
003200*              SUMMA LEVERERAT ANTAL                                      
003300*              AV 1 ARTIKEL                                               
003400   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
003500*              VÄRDE TILL DEALER NET                                      
003600   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
003700*              VÄRDE TILL DEALER NET                                      
003800   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
003900*              VÄRDE TILL LANDED COST BERÄKNAT                            
004000   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
004100*              VÄRDE TILL LANDED COST BERÄKNAT                            
004200   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
004300*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004400   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
004500*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004600   03 SUPNET-LOC                        PIC S9(13)V9(2) COMP-3.           
004700*              VÄRDE TILL PURCHASE NET                                    
004800   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
004900*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
005000   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
005100*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
005200   03 SUDLRNET-BUDGET                   PIC S9(13)V9(2) COMP-3.           
005300*              VÄRDE TILL DEALER NET                                      
005400*                                                                         
005500*** END OF VILMAII-COPY LENGTH= 167 OLD LENGTH=                           
