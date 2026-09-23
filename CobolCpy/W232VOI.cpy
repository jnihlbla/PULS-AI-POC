000100 01  W232VOI.                                                             
000200*                                 WDL8-EXTRACT FÖR EPLUS                  
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 DAGLIG               OCCURS 21 TIMES.                             
000600        05 TIVVD             PIC 9(3).                                    
000700*                                 VECKA + DAG (VVD)                       
000800        05 KVOI-DIV          PIC S9(7)           COMP-3.                  
000900*                                 ORDERINGÅNG DIVERSE OCH TPO             
001000        05 KVOI-NDC          PIC S9(7)           COMP-3.                  
001100*                                 ORDERINGÅNG LEV FRÅN NDC                
001200        05 KVOI-PROG         PIC S9(7)           COMP-3.                  
001300*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
001400        05 KVOI-REFILL       PIC S9(7)           COMP-3.                  
001500*                                 ORDERINGÅNG LEV FRÅN REFILL             
001600        05 KVOI-SATS         PIC S9(7)           COMP-3.                  
001700*                                 ORDERINGÅNG SATSFÖRBRUKNING             
001800        05 KVOI-SDC          PIC S9(7)           COMP-3.                  
001900*                                 ORDERINGÅNG LEV FRÅN SDC                
002000        05 KVOI-LEDTID       PIC S9(7)           COMP-3.                  
002100*                                 ORDERINGÅNG FÖRSKUTEN                   
002200*** END OF VILMAII-COPY LENGTH= 656 BYTES                                 
