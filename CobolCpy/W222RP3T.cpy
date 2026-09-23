000100 01  W222RP3T-CTX.                                                        
000200*                                 UPPDATERING AV SÄSONGINDEX              
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC 9(8).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 KDCLAGER             PIC 9.                                       
000800*                                 CENTRALLAGERKOD                         
000900     03 W222RP3T-001-GRP     OCCURS 12 TIMES.                             
001000        05 RESEASON          PIC 9V9(2).                                  
001100*                                 SÄSONGSINDEX                            
001200     03 FLABORT-SEASON       PIC X.                                       
001300*                                 BORTTAG AV BEFINTLIGA                   
001400*                                 SÄSONGSINDEX?                           
001500*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
