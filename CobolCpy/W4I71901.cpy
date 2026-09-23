000100 01  MID-W4I71901.                                                        
000200*                                                                         
000300     03 MID-IDPARTNR-IN      PIC X(9).                                    
000400*                                 PARTNERNUMMER                           
000500     03 MID-IDFTG-IN         PIC X(2).                                    
000600*                                 FÖRETAGSID EKONOM REDOVISNING           
000700     03 MID-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-RAD-INFO         OCCURS 8 TIMES.                              
001200        05 MID-KDBEHX        PIC X.                                       
001300*                                 BEHANDLINGSKOD-X                        
001400        05 MID-KDANMORS      PIC X(2).                                    
001500*                                 ORSAK TILL LEVERANSANMÄRKNING           
001600        05 MID-IDDISTR       PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800        05 MID-IDKUNDNR      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000        05 MID-IDREF         PIC X(15).                                   
002100*                                 REFERENS ID                             
002200        05 MID-DAREGDAT      PIC X(8).                                    
002300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002400     03 MID-INPUT.                                                        
002500        05 MID-KDBEHX-UPD    PIC X.                                       
002600*                                 BEHANDLINGSKOD-X                        
002700        05 MID-SUARTBTO-UPD  PIC X(10).                                   
002800*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
002900        05 MID-KDVALISO-UPD  PIC X(3).                                    
003000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003100*** END OF VILMAII-COPY LENGTH= 323 BYTES                                 
