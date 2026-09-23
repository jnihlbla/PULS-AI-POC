000100 01  MOD-W90414O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9041400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-FILLER           PIC X(3).                                    
001200     03 MOD-FILLER           PIC X(3).                                    
001300     03 MOD-VECKA-FOM-IN     PIC X(4).                                    
001400*                                 ÅR - VECKA  (ÅÅVV)                      
001500     03 MOD-VECKA-FOM-UT     PIC X(4).                                    
001600*                                 ÅR - VECKA  (ÅÅVV)                      
001700     03 MOD-VECKA-TOM-IN     PIC X(4).                                    
001800*                                 ÅR - VECKA  (ÅÅVV)                      
001900     03 MOD-VECKA-TOM-UT     PIC X(4).                                    
002000*                                 ÅR - VECKA  (ÅÅVV)                      
002100     03 MOD-IDRADNR-DOLT     PIC 9(4).                                    
002200*                                 RADNUMMER                               
002300     03 MOD-IDRADNR-DOLT2    PIC 9(4).                                    
002400*                                 RADNUMMER                               
002500     03 MOD-FILLER           PIC X(9).                                    
002600     03 MOD-OUTPUT           OCCURS 13 TIMES.                             
002700*                                                                         
002800        05 MOD-FILLER        PIC X(2).                                    
002900        05 MOD-FILLER        PIC X.                                       
003000        05 MOD-REANTPSA      PIC Z9.9(3).                                 
003100*                                 ANTAL PER SATS                          
003200        05 MOD-IDLEVNR       PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400        05 MOD-ART-LEV       PIC X(30).                                   
003500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003600        05 MOD-TILLK-IDAO    PIC X(10).                                   
003700*                                 ÄNDRINGSORDERNUMMER                     
003800        05 MOD-START-VECKA   PIC X(4).                                    
003900*                                 ÅR - VECKA  (ÅÅVV)                      
004000        05 MOD-UTGR-IDAO     PIC X(10).                                   
004100*                                 ÄNDRINGSORDERNUMMER                     
004200        05 MOD-STOPP-VECKA   PIC X(4).                                    
004300*                                 ÅR - VECKA  (ÅÅVV)                      
004400        05 MOD-STRTYP        PIC X.                                       
004500*                                 STRUKTURTYP                             
004600     03 MOD-FILLER           PIC X(2).                                    
004700     03 MOD-FILLER           PIC X.                                       
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 1108 BYTES                                
