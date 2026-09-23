000100 01  2250-WDGX2250.                                                       
000200*                                 ÅTERSTARTS REGISTER                     
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 2250-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 2250-TIUPPDAT        PIC S9(7)           COMP-3.                  
000900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001000*                                 UPDATING DATE     (YYMMDD)              
001100     03 2250-TIUPPTID        PIC S9(9)           COMP-3.                  
001200*                                 UPPDATERINGSTID  (TTMMSSTH)             
001300*                                 UPDATING TIME    (HHMMSSTH)             
001400     03 2250-KVPOST          PIC S9(7)           COMP-3.                  
001500*                                 RÄKNARE, ANTAL POSTER                   
001600*                                 RECORD COUNTER                          
001700     03 2250-IDDC            PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 2250-IDARTNR         PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300     03 2250-IDLEVNR         PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002600     03 FILLER               PIC X(4).                                    
002700*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
