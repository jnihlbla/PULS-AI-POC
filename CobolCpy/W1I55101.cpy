000100 01  MID-W1I55101.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 OMNUMRERING AV KATALOGAVSNITT           
000400     03 MID-IDCATNR-IN       PIC X(5).                                    
000500*                                 KATALOG-ID                              
000600     03 MID-IDCATNR-UT       PIC X(5).                                    
000700*                                 KATALOG-ID                              
000800     03 MID-IDCATGRP-IN      PIC X(2).                                    
000900*                                 KATALOG-GRUPP                           
001000     03 MID-IDCATGRP-UT      PIC X(2).                                    
001100*                                 KATALOG-GRUPP                           
001200     03 MID-IDCATAVS-IN      PIC X(4).                                    
001300*                                 KATALOG-AVSNITT                         
001400     03 MID-IDCATAVS-UT      PIC X(4).                                    
001500*                                 KATALOG-AVSNITT                         
001600     03 MID-RADRAKNARE.                                                   
001700*                                 DOLT FÄLT                               
001800        05 MID-NYTT-NR-IDCATRAD                                           
001900                             PIC 9(4).                                    
002000*                                 RADNUMMER                               
002100        05 MID-8000-RADER    PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300        05 MID-OMNUMRERA-8000-RADER                                       
002400                             PIC X.                                       
002500*                                 ALLMÄN FLAGGA                           
002600        05 MID-IXHEL         PIC 9(9).                                    
002700*                                 INDEX HELORD                            
002800*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
