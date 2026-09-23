000100 01  MID-W4I90401-CTX.                                                    
000200*                                 COPYTEXT FÖR MID W4090401               
000300*                                                                         
000400     03 MID-IDDISTR-IN       PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 MID-IDDISTR-UT       PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MID-KDFRAKT-IN       PIC X(2).                                    
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400     03 MID-KDFRAKT-UT       PIC X(2).                                    
001500*                                 FRAKTSÄTT DC TILL KUND                  
001600     03 MID-KDORDKL-IN       PIC X.                                       
001700*                                 ORDERKLASS                              
001800     03 MID-KDORDKL-UT       PIC X.                                       
001900*                                 ORDERKLASS                              
002000     03 MID-FLAENDR-IN       PIC X.                                       
002100*                                 ÄNDRINGSFLAGGA                          
002200     03 MID-FLAENDR-UT       PIC X.                                       
002300*                                 ÄNDRINGSFLAGGA                          
002400     03 MID-BEKUNDRF-001     PIC X(10).                                   
002500*                                 KUNDENS REFERENS                        
002600     03 MID-IDSKYLT          PIC X(3).                                    
002700*                                 NATIONALITETSTECKEN                     
002800*                                 SPRÅKIDENTIFIKATION                     
002900     03 MID-BEVARREF         PIC X(10).                                   
003000*                                 VÅR REFERENS                            
003100     03 MID-KDROPACK         PIC X.                                       
003200*                                 FRISLÄPPNINGSKOD RO/DO                  
003300     03 MID-KDFAKTYP         PIC X.                                       
003400*                                 FAKTURATYP                              
003500     03 MID-IDFTG-IN         PIC X(2).                                    
003600*                                 FÖRETAGSID EKONOM REDOVISNING           
003700     03 MID-IDFTG-UT         PIC X(2).                                    
003800*                                 FÖRETAGSID EKONOM REDOVISNING           
003900     03 MID-IDKONTO-IN       PIC X(10).                                   
004000*                                 KONTO                                   
004100     03 MID-IDKONTO-UT       PIC X(10).                                   
004200*                                 KONTO                                   
004300     03 MID-IDKST-IN         PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500     03 MID-IDKST-UT         PIC X(10).                                   
004600*                                 KOSTNADSSTÄLLE                          
004700     03 MID-IDANALYS-IN      PIC X(12).                                   
004800*                                 ANALYSNUMMER                            
004900     03 MID-IDANALYS-UT      PIC X(12).                                   
005000*                                 ANALYSNUMMER                            
005100     03 MID-KDNOTES          PIC X(2).                                    
005200*                                 NOTERINGSKOD                            
005300     03 MID-TID              OCCURS 10 TIMES                              
005400                             PIC X.                                       
005500*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
005600     03 MID-TISTADAT         PIC X(6).                                    
005700*                                 GENERELLT STARTDATUM                    
005800*** END OF VILMAII-COPY LENGTH= 139 BYTES                                 
