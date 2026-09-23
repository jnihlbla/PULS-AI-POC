000100* GENERATION OF COBOL HOST STRUCTURE FROM CWDPDB-TAB                      
000200  01 CWDPDB.                                                              
000300*              DEALER BUDGET WEEKLY PER PRODSL                            
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000700*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
000800   03 IDDEALER                          PIC X(6).                         
000900*              DEALER KUNDNUMMER                                          
001000   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001100*              PRODUKTSLAG                                                
001200   03 KDTARGTY                          PIC X(1).                         
001300*              TARGET TYPE                                                
001400   03 FLLOKINK                          PIC X(1).                         
001500*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
001600*              AV IMPORTÖREN                                              
001700   03 TIAAAA                            PIC S9(5) COMP-3.                 
001800*              ÅRTAL (ÅÅÅÅ)                                               
001900   03 DAAARP                            PIC S9(7) COMP-3.                 
002000*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
002100*              12 PER ÅR                                                  
002200   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
002300*              VÄRDE TILL DEALER NET                                      
002400*                                                                         
002500*** END OF VILMAII-COPY LENGTH= 31 OLD LENGTH=                            
