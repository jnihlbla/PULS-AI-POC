000100 01  4580-WDGX4580-CTX.                                                   
000200*                                 ÅTERSTARTS REGISTER                     
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4580-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4580-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 POST ELLER RADRÄKNARE                   
001000*                                 RECORD OR LINE COUNTER                  
001100     03 4580-TIUPPDAT        PIC S9(7)           COMP-3.                  
001200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001300*                                 UPDATING DATE     (YYMMDD)              
001400     03 4580-TIUPPTID        PIC S9(9)           COMP-3.                  
001500*                                 UPPDATERINGSTID  (TTMMSSTH)             
001600*                                 UPDATING TIME    (HHMMSSTH)             
001700     03 4580-IDSEGKEY        PIC X(66).                                   
001800*                                 DATABAS SEGMENT NYCKLAR                 
001900*                                 DATABASE SEGMENT KEY                    
002000     03 4580-IDWDK7A1KY-FILLER REDEFINES 4580-IDSEGKEY.                   
002100        05 4580-IDWDK7A1KY   PIC X(14).                                   
002200*                                 NYCKEL TILL WDK7A1KY                    
002300*                                 KEY TO WDK7A1KY                         
002400        05 FILLER            PIC X(52).                                   
002500     03 4580-IDWDGX2213-FILLER REDEFINES 4580-IDSEGKEY.                   
002600        05 4580-IDWDGX2213   PIC X(30).                                   
002700*                                 NYCKEL TILL WDGX2213                    
002800*                                 KEY TO WDGX2213                         
002900        05 FILLER            PIC X(36).                                   
003000     03 4580-IDWDGX2254-FILLER REDEFINES 4580-IDSEGKEY.                   
003100        05 4580-IDWDGX2254   PIC X(7).                                    
003200*                                 NYCKEL TILL WDGX2254                    
003300*                                 KEY TO WDGX2254                         
003400        05 FILLER            PIC X(59).                                   
003500     03 4580-IDWDGX2256-FILLER REDEFINES 4580-IDSEGKEY.                   
003600        05 4580-IDWDGX2256   PIC X(7).                                    
003700*                                 NYCKEL TILL WDGX2256                    
003800*                                 KEY TO WDGX2256                         
003900        05 FILLER            PIC X(59).                                   
004000     03 4580-IDGSAMKEY-W54020-FILLER REDEFINES 4580-IDSEGKEY.             
004100        05 4580-IDGSAMKEY-W54020                                          
004200                             PIC X(11).                                   
004300*                                 NYCKEL TILL GSAMKEY-W54020              
004400*                                 KEY TO GSAMKEY-W54020                   
004500        05 FILLER            PIC X(55).                                   
004600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
