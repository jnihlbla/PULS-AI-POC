000100 01  MID-W6I16201.                                                        
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
002400     03 MID-KDBRIST-IN       PIC X.                                       
002500*                                 BRIST KOD                               
002600     03 MID-KDBRIST-UT       PIC X.                                       
002700*                                 BRIST KOD                               
002800     03 MID-KDPAF-IN         PIC X.                                       
002900*                                 PÅFYLLNADSKOD                           
003000     03 MID-KDPAF-UT         PIC X.                                       
003100*                                 PÅFYLLNADSKOD                           
003200     03 MID-RADER            OCCURS 7 TIMES.                              
003300        05 MID-INPUT-CMD.                                                 
003400           07 MID-KDCMDVAL   PIC X(3).                                    
003500*                                 GENERELL KOMMANDOKOD                    
003600        05 MID-ADBUFFOMR     PIC X(2).                                    
003700*                                 BUFFERTOMRÅDE                           
003800        05 MID-ADBUFFGANG    PIC X(2).                                    
003900*                                 BUFFERT GÅNG                            
004000        05 MID-ADBUFFPL      PIC X(5).                                    
004100*                                 BUFFERPLATSNUMMER                       
004200        05 MID-DABUFPAF      PIC X(8).                                    
004300*                                 BUFFERT PÅFYLLNINGS DATUM               
004400        05 MID-INPUT-SALDO.                                               
004500           07 MID-KVBUFF-F-IN                                             
004600                             PIC X(7).                                    
004700*                                 FÖRÄDLAT BUFFERSALDO                    
004800           07 MID-KVBUFF-OF-IN                                            
004900                             PIC X(7).                                    
005000*                                 BUFFERSALDO OFÖRÄDLAT GODS              
005100           07 MID-KVKOLLI-F-IN                                            
005200                             PIC X(4).                                    
005300*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
005400           07 MID-KVKOLLI-OF-IN                                           
005500                             PIC X(4).                                    
005600*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
005700     03 MID-INPUT-RAD.                                                    
005800        05 MID-ADBUFFOMR-UPD PIC X(2).                                    
005900*                                 BUFFERTOMRÅDE                           
006000        05 MID-ADBUFFGANG-UPD                                             
006100                             PIC X(2).                                    
006200*                                 BUFFERT GÅNG                            
006300        05 MID-ADBUFFPL-UPD  PIC X(5).                                    
006400*                                 BUFFERPLATSNUMMER                       
006500        05 MID-KVBUFF-F-UPD  PIC X(7).                                    
006600*                                 FÖRÄDLAT BUFFERSALDO                    
006700        05 MID-KVBUFF-OF-UPD PIC X(7).                                    
006800*                                 BUFFERSALDO OFÖRÄDLAT GODS              
006900        05 MID-KVKOLLI-F-UPD PIC X(4).                                    
007000*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
007100        05 MID-KVKOLLI-OF-UPD                                             
007200                             PIC X(4).                                    
007300*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
007400     03 MID-KVANTAL-UPD      PIC X(6).                                    
007500*                                 ANTAL                                   
007600     03 MID-TREATED-UPD      PIC X.                                       
007700     03 MID-LOCATION-UPD     PIC X(10).                                   
007800*** END OF VILMAII-COPY LENGTH= 396 BYTES                                 
