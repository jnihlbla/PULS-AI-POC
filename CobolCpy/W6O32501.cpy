000100 01  MOD-W6O32501.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDARBTYP-IN      PIC X(8).                                    
000800*                                 TYP AV ARBETE                           
000900     03 MOD-KDARBTYP-UT      PIC X(8).                                    
001000*                                 TYP AV ARBETE                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDARTNR-IN       PIC Z(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC Z(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-RAD              OCCURS 11 TIMES.                             
002000*                                 GRUPP MED RADER                         
002100        05 MOD-TEMEMO-ATTR   PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-TEMEMO        PIC X(66).                                   
002400*                                 TEXTRAD MAIL                            
002500     03 MOD-KVTILLG-CDC      PIC -(7)9.                                   
002600*                                 LAGERTILLGÅNG-CDC                       
002700     03 MOD-KVAKS-CDC        PIC -(7)9.                                   
002800*                                 DEL AV AK SOM LIGGER I CDC              
002900     03 MOD-SUTPO-TOT        PIC -(7)9.                                   
003000*                                 TPO-KVANTITET, TOTAL                    
003100     03 MOD-IDDC             PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MOD-KVTILLG-SDC      PIC -(7)9.                                   
003400*                                 LAGERTILLGÅNG-SDC                       
003500     03 MOD-KVAKS-SDC        PIC -(7)9.                                   
003600*                                 DEL AV AK SOM LIGGER I SDC              
003700     03 MOD-KVSKROT          PIC -(7)9.                                   
003800*                                 ANTAL SENASTE SKROTORDER                
003900     03 MOD-KVSKROT-KVAR     PIC -(7)9.                                   
004000*                                 KVARLIGGANDE ANTAL                      
004100     03 MOD-KDERS-UTG        PIC Z9.                                      
004200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
004300     03 MOD-BEEMBLEM         OCCURS 20 TIMES                              
004400                             PIC X(5).                                    
004500*                                 EMBLEM                                  
004600     03 MOD-TEMFSINF         PIC X(55).                                   
004700*                                 INFORMATIONSMEDDELANDE                  
004800*** END OF VILMAII-COPY LENGTH= 1045 BYTES                                
