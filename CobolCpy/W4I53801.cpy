000100 01  MID-W4I53801.                                                        
000200*                                 COPYTEXT FÖR MID W4I53801               
000300*                                                                         
000400     03 MID-IDDISTR-IN       PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 MID-IDDISTR-UT       PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDFAKT-IN        PIC X(7).                                    
000900*                                 FAKTURANUMMER                           
001000     03 MID-IDFAKT-UT        PIC X(7).                                    
001100*                                 FAKTURANUMMER                           
001200     03 MID-IDKUNDNR-ENTER   PIC 9(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MID-KDFRAKT-ENTER    PIC 9(2).                                    
001500*                                 FRAKTSÄTT DC TILL KUND                  
001600     03 MID-IDSKEPPN-ENTER   PIC 9(7).                                    
001700*                                 SKEPPNINGSNUMMER                        
001800     03 MID-IDFAKT-ENTER     PIC 9(7).                                    
001900*                                 FAKTURANUMMER                           
002000     03 MID-IDORDNR5-ENTER   PIC 9(5).                                    
002100*                                 ORDERNUMMER                             
002200     03 MID-IDKOLLI-ENTER    PIC 9(5).                                    
002300*                                 KOLLINUMMER                             
002400     03 MID-IDPRODNR-ENTER   PIC 9(7).                                    
002500*                                 PRODUKTIONSNUMMER                       
002600     03 MID-IDKUNDNR-NEXT    PIC 9(6).                                    
002700*                                 KUNDNUMMER                              
002800     03 MID-KDFRAKT-NEXT     PIC 9(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000     03 MID-IDSKEPPN-NEXT    PIC 9(7).                                    
003100*                                 SKEPPNINGSNUMMER                        
003200     03 MID-IDFAKT-NEXT      PIC 9(7).                                    
003300*                                 FAKTURANUMMER                           
003400     03 MID-IDORDNR5-NEXT    PIC 9(5).                                    
003500*                                 ORDERNUMMER                             
003600     03 MID-IDKOLLI-NEXT     PIC 9(5).                                    
003700*                                 KOLLINUMMER                             
003800     03 MID-IDPRODNR-NEXT    PIC 9(7).                                    
003900*                                 PRODUKTIONSNUMMER                       
004000     03 MID-KDTRPTYP-UT      PIC 9.                                       
004100*                                 TRANSPORTMEDEL FÖR GODS                 
004200     03 MID-FLCONTAIN-UT     PIC X.                                       
004300*                                 CONTAINER FULL ELLER INTE J/N           
004400     03 MID-IDLBBET-UT       PIC X(12).                                   
004500*                                 LASTBÄRARBETECKNING                     
004600     03 MID-IDBOKN-UT        PIC X(15).                                   
004700*                                 BOKNINGSNUMMER                          
004800     03 MID-INPUT.                                                        
004900        05 MID-FLSLUT        PIC X.                                       
005000*                                 AVSLUTNINGSFLAGGA                       
005100        05 MID-FLBORT        PIC X.                                       
005200*                                 ALLMÄN FLAGGA                           
005300        05 MID-KDTRPTYP      PIC X.                                       
005400*                                 TRANSPORTMEDEL FÖR GODS                 
005500        05 MID-FLCONTAIN     PIC X.                                       
005600*                                 CONTAINER FULL ELLER INTE J/N           
005700        05 MID-IDLBBET       PIC X(12).                                   
005800*                                 LASTBÄRARBETECKNING                     
005900        05 MID-IDBOKN        PIC X(15).                                   
006000*                                 BOKNINGSNUMMER                          
006100        05 MID-IDFORDREG     PIC X(30).                                   
006200*                                 FORDON REG. NUMMER                      
006300     03 MID-RADER            OCCURS 9 TIMES.                              
006400        05 MID-IDFAKT        PIC X(7).                                    
006500*                                 FAKTURANUMMER                           
006600        05 MID-IDORDNR5      PIC X(5).                                    
006700*                                 ORDERNUMMER                             
006800        05 MID-IDKOLLI       PIC X(5).                                    
006900*                                 KOLLINUMMER                             
007000        05 MID-IDPRODNR      PIC X(7).                                    
007100*                                 PRODUKTIONSNUMMER                       
007200*** END OF VILMAII-COPY LENGTH= 406 BYTES                                 
