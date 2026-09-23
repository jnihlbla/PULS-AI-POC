000100 01  MID-W1I53301.                                                        
000200*                                 MID-COPYTEXT FÖR BILD 1533              
000300*                                 GENERERINGSTABELL FÖR VADIS             
000400     03 MID-IDCATNR-FROM-IN  PIC X(5).                                    
000500*                                 KATALOG-ID                              
000600     03 MID-IDCATNR-FROM-UT  PIC X(5).                                    
000700*                                 KATALOG-ID                              
000800     03 MID-IDCATNR-TO-IN    PIC X(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MID-IDCATNR-TO-UT    PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200     03 MID-AARSKOLUMN       OCCURS 2 TIMES.                              
001300        05 MID-FLVADGEN      OCCURS 12 TIMES                              
001400                             PIC X.                                       
001500*                                 AKTIVERA VADIS-GENERERING               
001600*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
