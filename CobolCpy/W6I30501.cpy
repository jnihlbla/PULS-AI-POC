000100 01  MID-W6I30501.                                                        
000200*                                 MID TILL FRÅGA/UPPDATERING AV           
000300*                                 BUFFERSALDOBASEN                        
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-KVBUFF-IN        PIC X(7).                                    
000700*                                 FÖRÄDLAT BUFFERSALDO                    
000800     03 MID-ADBUFFOMR-IN     PIC X(2).                                    
000900*                                 BUFFERTOMRÅDE                           
001000     03 MID-ADBUFFGANG-IN    PIC X(2).                                    
001100*                                 BUFFERT GÅNG                            
001200     03 MID-ADBUFFPL-IN      PIC X(5).                                    
001300*                                 BUFFERPLATSNUMMER                       
001400     03 MID-IDARTNR-UT       PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 MID-KVBUFF-UT        PIC X(7).                                    
001700*                                 FÖRÄDLAT BUFFERSALDO                    
001800     03 MID-ADBUFFOMR-UT     PIC X(2).                                    
001900*                                 BUFFERTOMRÅDE                           
002000     03 MID-ADBUFFGANG-UT    PIC X(2).                                    
002100*                                 BUFFERT GÅNG                            
002200     03 MID-ADBUFFPL-UT      PIC X(5).                                    
002300*                                 BUFFERPLATSNUMMER                       
002400     03 MID-IDDC             PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MID-RADER            OCCURS 7 TIMES.                              
002700        05 MID-INPUT-CMD.                                                 
002800           07 MID-KDCMDVAL   PIC X(3).                                    
002900*                                 GENERELL KOMMANDOKOD                    
003000        05 MID-ADBUFFOMR     PIC X(2).                                    
003100*                                 BUFFERTOMRÅDE                           
003200        05 MID-ADBUFFGANG    PIC X(2).                                    
003300*                                 BUFFERT GÅNG                            
003400        05 MID-ADBUFFPL      PIC X(5).                                    
003500*                                 BUFFERPLATSNUMMER                       
003600        05 MID-DABUFPAF      PIC X(8).                                    
003700*                                 BUFFERT PÅFYLLNINGS DATUM               
003800        05 MID-INPUT-SALDO.                                               
003900           07 MID-KVBUFF-F-IN                                             
004000                             PIC X(7).                                    
004100*                                 FÖRÄDLAT BUFFERSALDO                    
004200           07 MID-KVKOLLI-F-IN                                            
004300                             PIC X(4).                                    
004400*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
004500     03 MID-INPUT-RAD.                                                    
004600        05 MID-ADBUFFOMR-UPD PIC X(2).                                    
004700*                                 BUFFERTOMRÅDE                           
004800        05 MID-ADBUFFGANG-UPD                                             
004900                             PIC X(2).                                    
005000*                                 BUFFERT GÅNG                            
005100        05 MID-ADBUFFPL-UPD  PIC X(5).                                    
005200*                                 BUFFERPLATSNUMMER                       
005300        05 MID-KVBUFF-F-UPD  PIC X(7).                                    
005400*                                 FÖRÄDLAT BUFFERSALDO                    
005500        05 MID-KVKOLLI-F-UPD PIC X(4).                                    
005600*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
005700*** END OF VILMAII-COPY LENGTH= 289 BYTES                                 
