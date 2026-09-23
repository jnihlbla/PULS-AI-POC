000100 01  W231212.                                                             
000200*                                 NEDLÄSNING ORDERINGÅNG 12               
000300*                                 PERIODER                                
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 OI-DEL               OCCURS 12 TIMES.                             
000900        05 KVOI-PROG         PIC S9(7)           COMP-3.                  
001000*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
001100        05 KVOI-DIV          PIC S9(7)           COMP-3.                  
001200*                                 ORDERINGÅNG DIVERSE OCH TPO             
001300        05 KVOI-SATS         PIC S9(7)           COMP-3.                  
001400*                                 ORDERINGÅNG SATSFÖRBRUKNING             
001500        05 KVOI-NDC          PIC S9(7)           COMP-3.                  
001600*                                 ORDERINGÅNG LEV FRÅN NDC                
001700        05 KVOI-SDC          PIC S9(7)           COMP-3.                  
001800*                                 ORDERINGÅNG LEV FRÅN SDC                
001900        05 KVOI-REFILL       PIC S9(7)           COMP-3.                  
002000*                                 ORDERINGÅNG LEV FRÅN REFILL             
002100*** END OF VILMAII-COPY LENGTH= 296 BYTES                                 
