000100 01  MOD-W90436O1.                                                        
000200*                                 MOD FÖR PROGRAM W9043601-SPIE2/         
000300*                                 W60181                                  
000400*                                 PROGRAMMET VISAR OCH UPPDATERAR         
000500*                                 FÖRPACKNINGSTYP                         
000600     03 MOD-IDTRANS          PIC X(4).                                    
000700*                                 BILDNUMMER                              
000800     03 MOD-TEMFSFEL         PIC X(40).                                   
000900*                                 MFS FELMEDDELANDE                       
001000     03 MOD-IDARTNR-IN       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDARTNR-UT       PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-FILLER           PIC X(25).                                   
001500     03 MOD-BEFT-UT          PIC Z9.                                      
001600*                                 FÖRPACKNINGSTYP                         
001700     03 MOD-FILLER           PIC X(4).                                    
001800     03 MOD-FILLER           PIC X(8).                                    
001900     03 MOD-FILLER           PIC X(6).                                    
002000     03 MOD-TEBEFT-UT        PIC X(40).                                   
002100*                                 TEXT FÖRPACKNINGSINSTRUKTION            
002200     03 MOD-TEBEFT-79-UT     PIC X(79).                                   
002300     03 MOD-BEFT-IN-ATTR     PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-BEFT-IN          PIC Z9.                                      
002600*                                 FÖRPACKNINGSTYP                         
002700     03 MOD-FILLER           PIC X(2).                                    
002800     03 MOD-FILLER           PIC X(4).                                    
002900     03 MOD-TEBEFT-IN-ATTR   PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-TEBEFT-IN        PIC X(40).                                   
003200*                                 TEXT FÖRPACKNINGSINSTRUKTION            
003300     03 MOD-TEBEFT-79-IN-ATTR                                             
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-TEBEFT-79-IN     PIC X(79).                                   
003700     03 MOD-W90436O1-001-GRP OCCURS 4 TIMES.                              
003800*                                 RADER SOM VISAR HISTORIK PÅ             
003900*                                 FÖRPACKNINGSINSTRUKTIONER               
004000        05 MOD-FILLER        PIC X(2).                                    
004100        05 MOD-FILLER        PIC X(4).                                    
004200        05 MOD-FILLER        PIC X(8).                                    
004300        05 MOD-FILLER        PIC X(6).                                    
004400        05 MOD-FILLER        PIC X(40).                                   
004500        05 MOD-FILLER        PIC X(79).                                   
004600     03 MOD-TEMFSINF         PIC X(55).                                   
004700*                                 INFORMATIONSMEDDELANDE                  
004800*** END OF VILMAII-COPY LENGTH= 970 BYTES                                 
