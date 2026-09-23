000100 01  3136-WDGX3136.                                                       
000200*                                 ARTIKELSTATISTIK                        
000300*                                 DELBUDGET                               
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (KDMARK-BUDG + KDPRODSL                 
000600*                                  + IDFKNGRP)                            
000700     03 3136-KDMARK-BUDG     PIC S9(3)           COMP-3.                  
000800*                                 MARKNADSKOD BUDGET 96 MARKNADER         
000900*                                 MARKET CODE BUDGET (96 MARKETS)         
001000     03 3136-KDPRODSL        PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200*                                 TYPE OF ASSORTMENT                      
001300     03 3136-IDFKNGRP        PIC S9(5)           COMP-3.                  
001400*                                 FUNKTIONSGRUPP                          
001500*                                 FUNCTION GROUP                          
001600     03 3136-SUTOTFSG-BUDG   PIC S9(11)V9(2)     COMP-3.                  
001700*                                 BUDGETERAT FÖRSÄLJNINGSVÄRDE            
001800*                                 BUDGETED SALES AMOUNT                   
001900     03 3136-IDSKURVA        PIC S9(3)           COMP-3.                  
002000*                                 SÄSONGSKURVA                            
002100*                                 SEASON INDEX NUMBER                     
002200     03 3136-TIUPPDAT        PIC S9(7)           COMP-3.                  
002300*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002400*                                 UPDATING DATE     (YYMMDD)              
002500     03 FILLER               PIC X.                                       
002600*** END COPY WDGX3136C0  LENGTH=21                                        
