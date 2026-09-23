000100 01  MID-W3I18201.                                                        
000200*                                 MID-COPYTEXT FÖR W3018200               
000300     03 MID-IDKOLLI-IN       PIC X(5).                                    
000400*                                 KOLLINUMMER                             
000500     03 MID-SKAPA-NY-KOLLI   PIC X.                                       
000600     03 MID-VIKT-KOLLI-IN    PIC X(8).                                    
000700     03 MID-VOL-KOLLI-IN     PIC X(8).                                    
000800     03 MID-INDEX-10.                                                     
000900*                                 MID-COPYTEXT FÖR W3018200               
001000        05 MID-NYRAD         OCCURS 10 TIMES.                             
001100*                                 RADINFORMATION                          
001200           07 MID-IDARTNR-OBJ                                             
001300                             PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500           07 MID-KVCLEAR    PIC X(7).                                    
001600*                                 KVANTITET ATT CLEARA                    
001700     03 MID-VIKT-TOT-IN      PIC X(8).                                    
001800     03 MID-VOL-TOT-IN       PIC X(8).                                    
001900     03 MID-SKAPA-PROFORMA   PIC X.                                       
002000     03 MID-DEL-IDARTNO      PIC X(9).                                    
002100*                                 OBJEKTNUMMER                            
002200     03 MID-DEL-KVANTAL      PIC 9(6).                                    
002300*                                 ANTAL ATT DEBITERA                      
002400     03 MID-DEL-IDKOLLI      PIC X(5).                                    
002500*                                 KOLLINUMMER                             
002600*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 
