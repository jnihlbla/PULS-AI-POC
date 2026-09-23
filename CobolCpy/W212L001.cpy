000100 01  W212L001.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.  KONTROLL OM ARTIKELN         
000400*                                 FINNS PÅ ARTIKELREG.                    
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 HAMTA-ARTIKEL       VALUE +1.                                    
000800     03 FLJANEJ-ARTIKEL      PIC X.                                       
000900      88 ARTIKEL-FANNS       VALUE 'J'.                                   
001000      88 ARTIKEL-SAKNAS      VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 FLAGGA-AVTAL         PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400     03 NYCKLAR.                                                          
001500        05 IDARTNR           PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700*** END OF VILMAII-COPY LENGTH= 9 BYTES                                   
