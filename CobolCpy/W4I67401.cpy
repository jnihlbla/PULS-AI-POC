000100 01  MID-W4I67401.                                                        
000200*                                 COPYTEXT FÖR MID W4I67401               
000300*                                                                         
000400     03 MID-IDTRPTNR         PIC X(3).                                    
000500*                                 TRANSPORTIDENTITET                      
000600*                                 TRANSPORT IDENTITY                      
000700     03 MID-IDLBBET-HUV      PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900*                                 TRAILER NUMBER                          
001000     03 MID-IDDC             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 MID-IDSHIPM          PIC X(7).                                    
001400*                                 SKEPPNINGSNUMMER                        
001500*                                 SHIPMENT NO                             
001600     03 MID-FRAN-IDTRANS     PIC X(4).                                    
001700*                                 BILDNUMMER                              
001800*                                 SCREEN NUMBER                           
001900     03 MID-BEROUTE          PIC X(25).                                   
002000*                                 FÄRDVÄG, DESTINATION                    
002100*                                 ROUTE, DESTINATION                      
002200     03 MID-IDBOKN           PIC X(15).                                   
002300*                                 BOKNINGSNUMMER                          
002400*                                 BOOKING NUMBER                          
002500     03 MID-IDTRANSP-NAMN    PIC X(15).                                   
002600*                                 TRANSPORTMEDEL NAMN                     
002700*                                 NAME OF TRANSPORT                       
002800     03 MID-TIAVGANG         PIC X(6).                                    
002900*                                 AVGÅNGSDATUM                            
003000     03 MID-BETEXT-NEDK      PIC X(15).                                   
003100     03 MID-TINEDK           PIC X(6).                                    
003200*                                 NEDKÖRES TILL DATUM                     
003300*                                 DATE, DRIVE DOWN TO                     
003400     03 MID-BETEXT-HAEMT     PIC X(15).                                   
003500     03 MID-TIHAEMT          PIC X(6).                                    
003600*                                 DATUM HÄMTNING GODS                     
003700*                                 PICK-UP DATE GOODS                      
003800     03 MID-LB-GRP.                                                       
003900        05 FILLER            OCCURS 6 TIMES.                              
004000           07 MID-KDLBTYP    PIC X(3).                                    
004100*                                 LASTBÄRARTYP                            
004200*                                 CARRIER TYPE                            
004300           07 MID-IDLBBET    PIC X(12).                                   
004400*                                 LASTBÄRARBETECKNING                     
004500*                                 TRAILER NUMBER                          
004600     03 MID-OEVR-GRP.                                                     
004700        05 FILLER            OCCURS 3 TIMES.                              
004800           07 MID-BETEXT-OEVR                                             
004900                             PIC X(72).                                   
005000     03 FILLER               OCCURS 3 TIMES.                              
005100        05 MID-FLTRDOK       PIC X(3).                                    
005200*** END OF VILMAII-COPY LENGTH= 446 BYTES                                 
