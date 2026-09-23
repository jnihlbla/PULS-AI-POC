000100 01  W488L223.                                                            
000200*                                 LÄNKAREA NR 3 FÖR KOMMUNIKATION         
000300*                                 MELLAN W4882200 OCH DESS SUBPGM         
000400*                                 ANVÄNDS VID ANROPEN:                    
000500*                                         UPPDATERA-HTR-FEL               
000600     03 IDLOPNRF             PIC S9(5)           COMP-3.                  
000700*                                 LÖPNUMMER FELTRANS                      
000800     03 IDARTNR              PIC X(8).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 IDFELKOD             PIC X(3).                                    
001100*                                 FELKOD                                  
001200     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400*** END COPY W488L223C0  LENGTH=18                                        
