000100 01  W4067410.                                                            
000200*                                 COPYTEXT FÖR SUBRUTIN W4067410          
000300*                                                                         
000400     03 IDSHIPM              PIC 9(7).                                    
000500*                                 SKEPPNINGSNUMMER                        
000600     03 IDTRPTNR             PIC 9(3).                                    
000700*                                 TRANSPORTIDENTITET                      
000800     03 IDLBBET-HUV          PIC X(12).                                   
000900*                                 LASTBÄRARBETECKNING                     
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 FRAN-IDTRANS         PIC X(4).                                    
001300*                                 BILDNUMMER                              
001400     03 BEROUTE              PIC X(25).                                   
001500*                                 FÄRDVÄG, DESTINATION                    
001600     03 IDBOKN               PIC X(15).                                   
001700*                                 BOKNINGSNUMMER                          
001800     03 IDTRANSP-NAMN        PIC X(15).                                   
001900*                                 TRANSPORTMEDEL NAMN                     
002000     03 TIAVGANG             PIC X(6).                                    
002100*                                 AVGÅNGSDATUM                            
002200     03 BETEXT-NEDK          PIC X(15).                                   
002300     03 TINEDK               PIC X(6).                                    
002400*                                 NEDKÖRES TILL DATUM                     
002500     03 BETEXT-HAEMT         PIC X(15).                                   
002600     03 TIHAEMT              PIC X(6).                                    
002700*                                 DATUM HÄMTNING GODS                     
002800     03 LB-GRP.                                                           
002900        05 FILLER            OCCURS 6 TIMES.                              
003000           07 KDLBTYP        PIC X(3).                                    
003100*                                 LASTBÄRARTYP                            
003200           07 IDLBBET        PIC X(12).                                   
003300*                                 LASTBÄRARBETECKNING                     
003400     03 OEVR-GRP.                                                         
003500        05 FILLER            OCCURS 3 TIMES.                              
003600           07 BETEXT-OEVR    PIC X(72).                                   
003700     03 FILLER               OCCURS 3 TIMES.                              
003800        05 FLTRDOK           PIC X(3).                                    
003900     03 KDTRDOK              PIC 9.                                       
004000*                                 TRANSPORTDOKUMENTKOD                    
004100     03 KVLDISTR             PIC 9(3).                                    
004200*                                 ANTAL DISTRIKT                          
004300*** END OF VILMAII-COPY LENGTH= 450 BYTES                                 
