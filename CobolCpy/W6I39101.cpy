000100 01  MID-W6I39101.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I39101                                
000400     03 MID-IDEVENT          PIC X(30).                                   
000500*                                 EVENT NAMN                              
000600     03 MID-SUBSCR.                                                       
000700*                                                                         
000800        05 MID-IDSUBSCR-ARCH PIC 9(11).                                   
000900*                                 SUBSCRIPTION ID FROM PROJECT44          
001000        05 MID-KDSTASUB      PIC 9.                                       
001100*                                 STATUS OF SUBSCRIPTION FROM P44         
001200     03 MID-CONTAINER.                                                    
001300*                                                                         
001400        05 MID-IDLBBET       PIC X(12).                                   
001500*                                 LASTBÄRARBETECKNING                     
001600        05 MID-IDCONTNR      PIC 9(11).                                   
001700*                                 UNIQUE ID OF CONTAINER FROM             
001800*                                 PROJECT44                               
001900        05 MID-IDBOKN        PIC X(15).                                   
002000*                                 BOKNINGSNUMMER                          
002100        05 MID-BETRPFIR      PIC X(15).                                   
002200*                                 TRANSPORTFIRMANS NAMN                   
002300        05 MID-IDSUBSCR-CONT PIC 9(11).                                   
002400*                                 SUBSCRIPTION ID FROM PROJECT44          
002500        05 MID-KDEVENT       PIC 9(3).                                    
002600*                                 EVENT KOD                               
002700        05 MID-DABERANK-LIFDEPPL                                          
002800                             PIC X(21).                                   
002900*                                 PLANNED ETA DATE FROM PROJECT44         
003000        05 MID-DABERANK-LIFDEPAC                                          
003100                             PIC X(21).                                   
003200*                                 ACTUAL ETA DATE FROM PROJECT44          
003300        05 MID-DABERANK-PODDEPPL                                          
003400                             PIC X(21).                                   
003500*                                 PLANNED ETA DATE FROM PROJECT44         
003600        05 MID-DABERANK-PODDEPAC                                          
003700                             PIC X(21).                                   
003800*                                 ACTUAL ETA DATE FROM PROJECT4           
003900        05 MID-DABERANK-DLVDELPL                                          
004000                             PIC X(21).                                   
004100*                                 PLANNED ETA DATE FROM PROJECT44         
004200        05 MID-DABERANK-DLVDELAC                                          
004300                             PIC X(21).                                   
004400*                                 ACTUAL ETA DATE FROM PROJECT4           
004500        05 MID-DABERANK-PODDISPL                                          
004600                             PIC X(21).                                   
004700*                                 PLANNED ETA DATE FROM PROJECT44         
004800        05 MID-DABERANK-PODDISAC                                          
004900                             PIC X(21).                                   
005000*                                 ACTUAL ETA DATE FROM PROJECT4           
005100        05 MID-DABERANK-PODARRPL                                          
005200                             PIC X(21).                                   
005300*                                 PLANNED ETA DATE FROM PROJECT44         
005400        05 MID-DABERANK-LIFARRAC                                          
005500                             PIC X(21).                                   
005600*                                 ACTUAL ETA DATE FROM PROJECT4           
005700*** END OF VILMAII-COPY LENGTH= 319 BYTES                                 
