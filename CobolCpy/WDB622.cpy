000100 01  FGAD-WDB622.                                                         
000200*                                 DC PARAMETERS                           
000300*                                 ADDITIONAL COSTS/FUNCTION GROUP         
000400*                                 SEGMENT KEY: IDFKNGRP                   
000500     03 FGAD-IDFKNGRP        PIC S9(5)           COMP-3.                  
000600*                                 FUNKTIONSGRUPP                          
000700*                                 FUNCTION GROUP                          
000800     03 FGAD-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 FGAD-RELANDCO-FG-FROM                                             
001200                             PIC S9(3)V9(3)      COMP-3.                  
001300*                                 LANDING COST/FG EFTER TITULF            
001400*                                 LANDING COST/FG AFTER TITULF            
001500     03 FGAD-RELANDCO-FG-TO  PIC S9(3)V9(3)      COMP-3.                  
001600*                                 LANDING COST/FG TILL TITULF             
001700*                                 LANDING COST/FG UNTIL TITULF            
001800     03 FGAD-RELANDCO-EITX-FROM                                           
001900                             PIC S9(3)V9(3)      COMP-3.                  
002000*                                 LANDING COST/FG EI EFTER TITULF         
002100*                                 LANDING COST/FG EI AFTER TITULF         
002200     03 FGAD-RELANDCO-EITX-TO                                             
002300                             PIC S9(3)V9(3)      COMP-3.                  
002400*                                 LANDING COST/FG EI TILL TITULF          
002500*                                 LANDING COST/FG EI UNTIL TITULF         
002600     03 FGAD-RELANDCO-EGTX-FROM                                           
002700                             PIC S9(3)V9(3)      COMP-3.                  
002800*                                 LANDING COST/FG EG EFTER TITULF         
002900*                                 LANDING COST/FG EG AFTER TITULF         
003000     03 FGAD-RELANDCO-EGTX-TO                                             
003100                             PIC S9(3)V9(3)      COMP-3.                  
003200*                                 LANDING COST/FG EG TILL TITULF          
003300*                                 LANDING COST/FG EG UNTIL TITULF         
003400     03 FGAD-RELANDCO-EOCF-FROM                                           
003500                             PIC S9(3)V9(3)      COMP-3.                  
003600*                                 LANDING COST/FG EO EFTER TITULF         
003700*                                 LANDING COST/FG EO AFTER TITULF         
003800     03 FGAD-RELANDCO-EOCF-TO                                             
003900                             PIC S9(3)V9(3)      COMP-3.                  
004000*                                 LANDING COST/FG EO TILL TITULF          
004100*                                 LANDING COST/FG EO UNTIL TITULF         
004200     03 FGAD-TILANDCO        PIC S9(7)           COMP-3.                  
004300*                                 STARTDATUM LANDING COST FAKTOR          
004400*                                 START DATE LANDING COST FACTOR          
004500     03 FGAD-TIUPPDAT        PIC S9(7)           COMP-3.                  
004600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004700*                                 UPDATING DATE     (YYMMDD)              
004800*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
