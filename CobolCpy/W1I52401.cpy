000100 01  MID-W1I52401.                                                        
000200*                                 MID-COPYTEXT FÖR BILD 1524.             
000300*                                 BESTÄLLNING AV KONTROLL-                
000400*                                 PROGRAM INOM KATALOGSYSTEMET            
000500     03 MID-KDBEH-IDJOB      PIC 9.                                       
000600*                                 BEHANDLINGSKOD                          
000700     03 MID-IDCATNR          PIC Z(4)9.                                   
000800*                                 KATALOG-ID                              
000900     03 MID-SPRAK-KOD.                                                    
001000        05 MID-IDSKYLT       OCCURS 6 TIMES                               
001100                             PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MID-KDPRTVAL         PIC X.                                       
001500*                                 PRINTER-VAL KOD                         
001600     03 MID-TIERSDAT         PIC X(5).                                    
001700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
001800*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
