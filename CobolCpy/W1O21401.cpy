000100 01  MOD-W1O21401.                                                        
000200*                                 MOD-COPYTEXT FÖR W1021400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600*                                 SPRÅKIDENTIFIKATION                     
001700     03 MOD-VECKA-FOM-IN     PIC X(4).                                    
001800*                                 ÅR - VECKA  (ÅÅVV)                      
001900     03 MOD-VECKA-FOM-UT     PIC X(4).                                    
002000*                                 ÅR - VECKA  (ÅÅVV)                      
002100     03 MOD-VECKA-TOM-IN     PIC X(4).                                    
002200*                                 ÅR - VECKA  (ÅÅVV)                      
002300     03 MOD-VECKA-TOM-UT     PIC X(4).                                    
002400*                                 ÅR - VECKA  (ÅÅVV)                      
002500     03 MOD-IDRADNR-DOLT     PIC 9(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-IDRADNR-DOLT2    PIC 9(4).                                    
002800*                                 RADNUMMER                               
002900     03 MOD-IDSATSNR-DOLT    PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-OUTPUT           OCCURS 13 TIMES.                             
003200*                                                                         
003300        05 MOD-SELECT-ATTR   PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-SELECT        PIC X.                                       
003600        05 MOD-REANTPSA      PIC Z9.9(3).                                 
003700*                                 ANTAL PER SATS                          
003800        05 MOD-IDLEVNR       PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER                        
004000        05 MOD-ART-LEV       PIC X(30).                                   
004100*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004200        05 MOD-TILLK-IDAO    PIC X(10).                                   
004300*                                 ÄNDRINGSORDERNUMMER                     
004400        05 MOD-START-VECKA   PIC X(4).                                    
004500*                                 ÅR - VECKA  (ÅÅVV)                      
004600        05 MOD-UTGR-IDAO     PIC X(10).                                   
004700*                                 ÄNDRINGSORDERNUMMER                     
004800        05 MOD-STOPP-VECKA   PIC X(4).                                    
004900*                                 ÅR - VECKA  (ÅÅVV)                      
005000        05 MOD-STRTYP        PIC X.                                       
005100*                                 STRUKTURTYP                             
005200     03 MOD-KDPRTVAL-ATTR    PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-KDPRTVAL         PIC X.                                       
005500*                                 PRINTER-VAL KOD                         
005600     03 MOD-TEMFSINF         PIC X(55).                                   
005700*                                 INFORMATIONSMEDDELANDE                  
005800*** END OF VILMAII-COPY LENGTH= 1108 BYTES                                
