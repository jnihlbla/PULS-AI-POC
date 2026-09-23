000100 01  PGAD-WDB621.                                                         
000200*                                 DC PARAMETERS                           
000300*                                 ADDITIONAL COSTS/PRODUCT GROUP          
000400*                                 SEGMENT KEY: KDPRODSL                   
000500     03 PGAD-KDPRODSL        PIC S9(3)           COMP-3.                  
000600*                                 PRODUKTSLAG                             
000700*                                 PRODUCT GROUP                           
000800     03 PGAD-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 PGAD-RELANDCO-PG-FROM                                             
001200                             PIC S9(3)V9(3)      COMP-3.                  
001300*                                 LANDING COST/PG EFTER TITULF            
001400*                                 LANDING COST/PG AFTER TITULF            
001500     03 PGAD-RELANDCO-PG-TO  PIC S9(3)V9(3)      COMP-3.                  
001600*                                 LANDING COST/PG TILL TITULF             
001700*                                 LANDING COST/PG UNTIL TITULF            
001800     03 PGAD-TILANDCO        PIC S9(7)           COMP-3.                  
001900*                                 STARTDATUM LANDING COST FAKTOR          
002000*                                 START DATE LANDING COST FACTOR          
002100     03 PGAD-TIUPPDAT        PIC S9(7)           COMP-3.                  
002200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002300*                                 UPDATING DATE     (YYMMDD)              
002400*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
