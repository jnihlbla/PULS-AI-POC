000100 01  W236OO3.                                                             
000200*                                 LISTPOST MELLAN W23502 OCH              
000300*                                 W23504.                                 
000400*                                                                         
000500     03 SORTBGRP.                                                         
000600*                                                                         
000700        05 KDPRODSL          PIC S9(3)           COMP-3.                  
000800*                                 PRODUKTSLAG                             
000900        05 IDLEVNR           PIC S9(5)           COMP-3.                  
001000*                                 LEVERANT÷RNUMMER                        
001100        05 TIAARP            PIC S9(5)           COMP-3.                  
001200*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
001300*                                 12 PER ≈R                               
001400     03 DATA.                                                             
001500        05 TYP               PIC S9              COMP-3.                  
001600*                                 LEVERANSSTATUS                          
001700*                                 1 = F÷RSENAD LEVERANS                   
001800*                                 2 = F÷RTIDIG LEVERANS                   
001900*                                 3 = PLANERAD LEVERANS                   
002000        05 ANTAL             PIC S9(7)           COMP-3.                  
002100*                                 ANTAL LEVERANSER      KVLEV-002         
002200        05 VARDE             PIC S9(9)V9(2)      COMP-3.                  
002300*                                 VƒRDE PLANERADE LEVERANSER              
002400        05 IDARTNR           PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600        05 TIAVROP-INL       PIC S9(5)           COMP-3.                  
002700*                                 INLEVERANSDATUM (PLANERAD)              
002800*                                 (≈≈VV)                                  
002900        05 IDLOPNRM-PL       PIC S9(9)           COMP-3.                  
003000*                                 AVBOKNINGSID, (≈≈VVDLLLL)               
003100*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
