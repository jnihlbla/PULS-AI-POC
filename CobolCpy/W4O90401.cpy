000100 01  MOD-W4O90401-CTX.                                                    
000200*                                 COPYTEXT FÖR MOD W4090401               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-KDFRAKT-IN       PIC X(2).                                    
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 MOD-KDFRAKT-UT       PIC X(2).                                    
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-FLAENDR-IN       PIC X.                                       
002500*                                 ÄNDRINGSFLAGGA                          
002600     03 MOD-FLAENDR-UT       PIC X.                                       
002700*                                 ÄNDRINGSFLAGGA                          
002800     03 MOD-BEKUNDRF-ATTR    PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-BEKUNDRF-001     PIC X(10).                                   
003100*                                 KUNDENS REFERENS                        
003200     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-IDSKYLT          PIC X(3).                                    
003500*                                 NATIONALITETSTECKEN                     
003600*                                 SPRÅKIDENTIFIKATION                     
003700     03 MOD-KXSPRAK-ATTR     PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-KXSPRAK          PIC X(20).                                   
004000     03 MOD-BEVARREF-ATTR    PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-BEVARREF         PIC X(10).                                   
004300*                                 VÅR REFERENS                            
004400     03 MOD-KDROPACK-ATTR    PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-KDROPACK         PIC X.                                       
004700*                                 FRISLÄPPNINGSKOD RO/DO                  
004800     03 MOD-KXROPACK-ATTR    PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-KXROPACK         PIC X(20).                                   
005100     03 MOD-KDFAKTYP-ATTR    PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KDFAKTYP         PIC X.                                       
005400*                                 FAKTURATYP                              
005500     03 MOD-KXFAKTYP-ATTR    PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-KXFAKTYP         PIC X(20).                                   
005800     03 MOD-IDFTG-IN-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-IDFTG-IN         PIC X(2).                                    
006100*                                 FÖRETAGSID EKONOM REDOVISNING           
006200     03 MOD-IDFTG-UT         PIC X(2).                                    
006300*                                 FÖRETAGSID EKONOM REDOVISNING           
006400     03 MOD-IDKONTO-IN-ATTR  PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-IDKONTO-IN       PIC X(10).                                   
006700*                                 KONTO                                   
006800     03 MOD-IDKONTO-UT       PIC X(10).                                   
006900*                                 KONTO                                   
007000     03 MOD-IDKST-IN-ATTR    PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-IDKST-IN         PIC X(10).                                   
007300*                                 KOSTNADSSTÄLLE                          
007400     03 MOD-IDKST-UT         PIC X(10).                                   
007500*                                 KOSTNADSSTÄLLE                          
007600     03 MOD-IDANALYS-IN-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-IDANALYS-IN      PIC X(12).                                   
007900*                                 ANALYSNUMMER                            
008000     03 MOD-IDANALYS-UT      PIC X(12).                                   
008100*                                 ANALYSNUMMER                            
008200     03 MOD-KDNOTES-ATTR     PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-KDNOTES          PIC X(2).                                    
008500*                                 NOTERINGSKOD                            
008600     03 MOD-W4O90401-001-GRP OCCURS 10 TIMES.                             
008700*                                 VECKODAGAR                              
008800        05 MOD-TID-ATTR      PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-TID           PIC X.                                       
009100*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
009200     03 MOD-TISTADAT-ATTR    PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-TISTADAT         PIC X(6).                                    
009500*                                 GENERELLT STARTDATUM                    
009600     03 MOD-TEMFSINF         PIC X(55).                                   
009700*                                 INFORMATIONSMEDDELANDE                  
009800*** END OF VILMAII-COPY LENGTH= 346 BYTES                                 
