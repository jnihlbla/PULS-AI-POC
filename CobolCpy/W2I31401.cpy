000100 01  MID-W2I31401.                                                        
000200*                                 MID-COPYTEXT FÖR W2031400               
000300     03 MID-IDKAMP-IN        PIC X(7).                                    
000400*                                 SERVICEKAMPANJ                          
000500     03 MID-IDKAMP-UT        PIC X(7).                                    
000600*                                 SERVICEKAMPANJ                          
000700     03 MID-IDKAMP-GRP-IN    PIC X(7).                                    
000800*                                 ID FÖR KAMPANJGRUPPER                   
000900     03 MID-IDKAMP-GRP-UT    PIC X(7).                                    
001000*                                 ID FÖR KAMPANJGRUPPER                   
001100     03 MID-VISA-UNIK-IN     PIC X.                                       
001200     03 MID-VISA-UNIK-UT     PIC X.                                       
001300     03 MID-VISA-DEF-ERS-IN  PIC X.                                       
001400     03 MID-VISA-DEF-ERS-UT  PIC X.                                       
001500     03 MID-INPUT.                                                        
001600        05 MID-GRP           OCCURS 12 TIMES.                             
001700           07 MID-CMD        PIC X.                                       
001800           07 MID-IDARTNR    PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000           07 MID-KVKAMP-LAUNCH                                           
002100                             PIC X(7).                                    
002200*                                                                         
002300           07 MID-KVKAMP-FIRST                                            
002400                             PIC X(7).                                    
002500*                                                                         
002600           07 MID-KVKAMP-TOTAL                                            
002700                             PIC X(7).                                    
002800*                                 ANTAL I KAMPANJ                         
002900           07 MID-FLKVKAMP-TOTAL                                          
003000                             PIC X.                                       
003100*                                 MANUELLT ELLER MASKINELLT BERÄK         
003200*                                 NAD KVANTITET                           
003300           07 MID-RERESPRT   PIC X(3).                                    
003400*                                                                         
003500        05 MID-REG-NY-RAD.                                                
003600           07 MID-NY-IDARTNR PIC X(9).                                    
003700*                                 ARTIKELNUMMER                           
003800           07 MID-NY-KVKAMP-LAUNCH                                        
003900                             PIC X(7).                                    
004000*                                                                         
004100           07 MID-NY-KVKAMP-FIRST                                         
004200                             PIC X(7).                                    
004300*                                                                         
004400           07 MID-NY-KVKAMP-TOTAL                                         
004500                             PIC X(7).                                    
004600*                                 ANTAL I KAMPANJ                         
004700*** END OF VILMAII-COPY LENGTH= 482 BYTES                                 
