000100 01  MID-W2I34401.                                                        
000200*                                 MID-COPYTEXT FÖR W2034400               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT.                                                        
001200        05 MID-RAD           OCCURS 11 TIMES.                             
001300           07 MID-CMD        PIC X.                                       
001400           07 MID-ADLAGOMR-CD                                             
001500                             PIC X(2).                                    
001600*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
001700           07 MID-KVDAGAR-CDBEH                                           
001800                             PIC X(2).                                    
001900*                                 NO. OF DAYS TO BE USED WHEN             
002000*                                 CALCULATING CD REFILLORDERS             
002100           07 MID-FLCDREL    PIC X.                                       
002200*                                 OMGÅENDE RELEASE AV CD-REFILL           
002300           07 MID-TIDATUM-CROSS                                           
002400                             PIC 9(6).                                    
002500*                                 STARTDAG CROSS DOCKING FÖRDRÖJN         
002600        05 MID-NY-IDDC       PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800        05 MID-NY-ADLAGOMR-CD                                             
002900                             PIC X(2).                                    
003000*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
003100        05 MID-NY-KVDAGAR-CDBEH                                           
003200                             PIC X(2).                                    
003300*                                 NO. OF DAYS TO BE USED WHEN             
003400*                                 CALCULATING CD REFILLORDERS             
003500        05 MID-NY-FLCDREL    PIC X.                                       
003600*                                 OMGÅENDE RELEASE AV CD-REFILL           
003700*** END OF VILMAII-COPY LENGTH= 161 BYTES                                 
