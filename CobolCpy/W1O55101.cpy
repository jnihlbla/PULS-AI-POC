000100 01  MOD-W1O55101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 OMNUMRERING AV KATALOGAVSNITT           
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 MOD-IDCATNR-IN       PIC X(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MOD-IDCATNR-UT       PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200     03 MOD-IDCATGRP-IN      PIC X(2).                                    
001300*                                 KATALOG-GRUPP                           
001400     03 MOD-IDCATGRP-UT      PIC X(2).                                    
001500*                                 KATALOG-GRUPP                           
001600     03 MOD-IDCATAVS-IN      PIC X(4).                                    
001700*                                 KATALOG-AVSNITT                         
001800     03 MOD-IDCATAVS-UT      PIC X(4).                                    
001900*                                 KATALOG-AVSNITT                         
002000     03 MOD-RADRAKNARE.                                                   
002100*                                 DOLT FÄLT                               
002200        05 MOD-NYTT-NR-IDCATRAD                                           
002300                             PIC X(4).                                    
002400*                                 RADNUMMER                               
002500        05 MOD-8000-RADER    PIC X.                                       
002600*                                 ALLMÄN FLAGGA                           
002700        05 MOD-OMNUMRERA-8000-RADER                                       
002800                             PIC X.                                       
002900*                                 ALLMÄN FLAGGA                           
003000        05 MOD-IXHEL         PIC X(9).                                    
003100*                                 INDEX HELORD                            
003200     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
003300*                                 MEDDELANDEFÄLT PÅ RAD 23                
003400*** END OF VILMAII-COPY LENGTH= 142 BYTES                                 
