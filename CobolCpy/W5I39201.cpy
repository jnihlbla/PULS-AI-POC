000100 01  MID-W5I39201.                                                        
000200*                                 MID-COPYTEXT FÖR BILD 5392              
000300*                                 INVENTERING                             
000400     03 MID-KVINVSKR         PIC X(2).                                    
000500*                                 BEGÄRDA INVENTERINGSUNDERLAG            
000600     03 MID-ADLAGOMR         PIC X(2).                                    
000700*                                 LAGEROMRÅDE                             
000800     03 MID-IDDC             PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MID-ART-PRINT-GRP.                                                
001100*                                 GRUPPNIVÅ ARTIKEL-PRINT                 
001200        05 MID-ART-PRINT     OCCURS 10 TIMES.                             
001300*                                 ARTIKEL-PRINT GRUPP                     
001400           07 MID-IDARTNR-PRINT                                           
001500                             PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700           07 MID-KDINVPRIO-PRINT                                         
001800                             PIC X.                                       
001900*                                 INVENTERING PRIORITET                   
002000           07 MID-KDINVKAT-PRINT                                          
002100                             PIC X(2).                                    
002200*                                 INVENTERINGSKATEGORI                    
002300     03 MID-KVINVSKR-PRINT   PIC X(2).                                    
002400*                                 BEGÄRDA INVENTERINGSUNDERLAG            
002500     03 MID-IDPRTLST         PIC X(8).                                    
002600*                                 LOGISK PRINTER+LISTA IDENTITET          
002700*** END OF VILMAII-COPY LENGTH= 136 BYTES                                 
