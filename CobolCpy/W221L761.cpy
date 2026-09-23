000100 01  W221L761.                                                            
000200*                                 LÄNKAREA FÖR PGM W22176 FÖR             
000300*                                 LÄSNING AV ARTIKELREG   GAMLA           
000400*                                                                         
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAES-BENAEMNING     VALUE +761.                                  
000800*                                 ANROPSTYP       KDCALL-W221-002         
000900     03 NYCKLAR.                                                          
001000        05 IDARTNR           PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 IO-AREA.                                                          
001300        05 BEART-SVE         PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500*** END COPY W221L761C0  LENGTH=32                                        
