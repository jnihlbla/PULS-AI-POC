000100 01  4004-WDGX4004.                                                       
000200*                                 PLOCKSATSER UNDER UTSKRIFT              
000300*                                 ETIKETTER                               
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500     03 4004-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4004-KDPRT-PLE       PIC X(3).                                    
000900*                                 PRINTERKOD PLOCKETIKETTER               
001000*                                 PRINTERCODE PICKING LABLES              
001100     03 4004-NYCKEL-GRP.                                                  
001200        05 4004-KDPRT        PIC X(3).                                    
001300*                                 PRINTERKOD                              
001400*                                 PRINTERCODE                             
001500        05 4004-KDSS         PIC X.                                       
001600*                                 SIDOSKIPSKOD                            
001700*                                 CODE FOR PAGESKIP                       
001800        05 4004-ADLAGOMR     PIC S9(3)           COMP-3.                  
001900*                                 LAGEROMRÅDE                             
002000*                                 AREA                                    
002100        05 4004-ADGANG       PIC S9(3)           COMP-3.                  
002200*                                 GÅNG                                    
002300*                                 AISLE                                   
002400        05 4004-ADPLATS      PIC S9(5)           COMP-3.                  
002500*                                 LAGERPLATSNUMMER                        
002600*                                 LOCATION                                
002700        05 4004-IDARTNR      PIC S9(9)           COMP-3.                  
002800*                                 ARTIKELNUMMER                           
002900*                                 PART NUMBER                             
003000        05 4004-IDLOPNR      PIC S9(3)           COMP-3.                  
003100*                                 LÖPNUMMER                               
003200*                                 SEQUENCE NUMBER                         
003300     03 4004-KVRADER         OCCURS 101 TIMES                             
003400                             PIC S9(5)           COMP-3.                  
003500*                                 ANTAL RADER                             
003600*                                 NUMBER OF LINES                         
003700     03 4004-IDMSG3IV        PIC X(30).                                   
003800*                                 3IV MEDDELANDE-ID                       
003900*                                 3IV MESSAGE ID                          
004000     03 4004-IDSNO3IV        PIC X(25).                                   
004100*                                 SERIENR PÅ TERMINAL I 3IV               
004200*                                 SERIAL NO OF TERMINAL IN 3IV            
004300     03 4004-ADDISPXTRA      PIC X(20).                                   
004400*                                 ADDRESSTILLÄGG                          
004500*                                 ADDRESS EXTENTION                       
004600     03 4004-FILLER          PIC X(25).                                   
004700*** END OF VILMAII-COPY LENGTH= 425 BYTES                                 
