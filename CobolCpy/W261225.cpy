000100 01  W261225.                                                             
000200*                                 NEDLÄSNING AV ORDERINGÅNGSREG           
000300*                                 FÖR LTK-UPPFÖLJN OCH                    
000400*                                 LAGERBALANSER 16 RESP 8 SENASTE         
000500*                                 PERIODERNA                              
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 ORDERTRAFFDEL        OCCURS 16 TIMES.                             
001100*                                 ANTAL ORDERTRÄFF                        
001200        05 OTRA-PROGNOSPAV-C1                                             
001300                             PIC S9(7)           COMP-3.                  
001400*                                 ORDERTRÄFF, PROGNOSPÅVERKANDE           
001500        05 OTRA-DIVERSE-C1   PIC S9(7)           COMP-3.                  
001600*                                 ORDERTRÄFF, DIVERSE                     
001700     03 ORDERINGANGSDEL      OCCURS 8 TIMES.                              
001800*                                 ORDERINGÅNG   STYCK                     
001900        05 OING-PROGNOSPAV-C1                                             
002000                             PIC S9(7)           COMP-3.                  
002100*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
002200        05 OING-DIVERSE-C1   PIC S9(7)           COMP-3.                  
002300*                                 ORDERINGÅNG DIVERSE OCH TPO             
002400        05 OING-SATS-C1      PIC S9(7)           COMP-3.                  
002500*                                 ORDERINGÅNG SATSFÖRBRUKNING             
002600        05 OING-SDC-C2       PIC S9(7)           COMP-3.                  
002700*                                 ORDERINGÅNG LEV FRÅN SDC                
002800        05 OING-NDC-C2       PIC S9(7)           COMP-3.                  
002900*                                 ORDERINGÅNG LEV FRÅN NDC                
003000        05 OING-REFILL       PIC S9(7)           COMP-3.                  
003100*                                 ORDERINGÅNG LEV FRÅN REFILL             
003200*** END OF VILMAII-COPY LENGTH= 328 BYTES                                 
