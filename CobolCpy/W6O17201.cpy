000100 01  MOD-W6O17201.                                                        
000200*                                 MOD-COPYTEXT FÖR W6017200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC Z(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-TABINDX-ENTER    PIC 9(9).                                    
001200     03 MOD-TABINDX-NEXT     PIC 9(9).                                    
001300     03 MOD-BEART            PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500     03 MOD-ADLAGOMR         PIC Z9.                                      
001600*                                 LAGEROMRÅDE                             
001700     03 MOD-ADGANG           PIC Z9.                                      
001800*                                 GÅNG                                    
001900     03 MOD-ADPLATS          PIC Z(4)9.                                   
002000*                                 LAGERPLATSNUMMER                        
002100     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
002200*                                 GRUPP MED TABELLRADER                   
002300        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-KDCMDVAL      PIC X(3).                                    
002600*                                 GENERELL KOMMANDOKOD                    
002700        05 MOD-KVBUFF-UT     PIC Z(6)9.                                   
002800*                                 FÖRÄDLAT BUFFERSALDO                    
002900        05 MOD-ADBUFFOMR     PIC Z9.                                      
003000*                                 BUFFERTOMRÅDE                           
003100        05 MOD-ADBUFFGANG    PIC Z9.                                      
003200*                                 BUFFERT GÅNG                            
003300        05 MOD-ADBUFFPL      PIC Z(4)9.                                   
003400*                                 BUFFERPLATSNUMMER                       
003500        05 MOD-ADBUFPPL      PIC 9(2).                                    
003600*                                 PALLPLATSNUMMER I BUFFERT               
003700        05 MOD-TIPAF         PIC 9(6).                                    
003800*                                 PÅFYLLNADSDATUM   (ÅÅMMDD)              
003900        05 MOD-KVBUFF-IN-ATTR                                             
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-KVBUFF-IN     PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400     03 MOD-TEMFSINF         PIC X(61).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END COPY W6O17201    LENGTH=571                                       
