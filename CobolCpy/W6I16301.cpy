000100 01  MID-W6I16301.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-ADLAGOMR-CDC     PIC X(2).                                    
000800*                                 LAGEROMRÅDE                             
000900     03 MID-ADGANG-CDC       PIC X(2).                                    
001000*                                 GÅNG                                    
001100     03 MID-ADPLATS-CDC      PIC X(5).                                    
001200*                                 LAGERPLATSNUMMER                        
001300     03 MID-KVMAXPL          PIC 9(6).                                    
001400*                                 MAX ANTAL (STYCK) PÅ PLOCKPLATS         
001500     03 MID-CDPLATS          OCCURS 4 TIMES.                              
001600*                                 CROSSDOCKING GÅNG OCH PLATS             
001700*                                                                         
001800        05 MID-ADGANG-CD     PIC 9(2).                                    
001900*                                 GÅNG                                    
002000        05 MID-ADPLATS-CD    PIC 9(5).                                    
002100*                                 LAGERPLATSNUMMER                        
002200     03 MID-KDSPEEMB         PIC X.                                       
002300*                                 SPECIALEMBALLAGEKOD                     
002400     03 MID-IDARTNR-EMBQ3    PIC X(9).                                    
002500*                                 EMBALLAGEARTIKELNR FÖR Q3               
002600     03 MID-IDARTNR-EMBQ4    PIC X(9).                                    
002700*                                 EMBALLAGEARTIKELNR FÖR Q4               
002800     03 MID-FLEJBUFF         PIC X.                                       
002900*                                 EJ BUFFERTSTYRNING                      
003000     03 MID-ADINLOMR-BOA     PIC X(4).                                    
003100*                                 BUFFERTOMRÅDE-ALTERNATIVT               
003200     03 MID-ADINLOMR-PRT     PIC X(4).                                    
003300*                                 PRINTERPLACERING                        
003400*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
