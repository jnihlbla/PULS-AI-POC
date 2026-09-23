000100 01  MID-W6I18301.                                                        
000200*                                 MID-COPYTEXT FÖR W60183                 
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-UT       PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDFPINST-IN      PIC X(7).                                    
001200*                                 FÖRPACKNINGSINSTRUKTION NR              
001300     03 MID-IDFPINST-UT      PIC X(7).                                    
001400*                                 FÖRPACKNINGSINSTRUKTION NR              
001500     03 MID-IDFPINST         PIC X(7).                                    
001600*                                 FÖRPACKNINGSINSTRUKTION NR              
001700     03 MID-INDEL.                                                        
001800        05 MID-IDARTNR-KOP   PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000        05 MID-IDLEVNR-KOP   PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200        05 MID-FLNY          PIC X.                                       
002300*                                 ALLMÄN SVARSFLAGGA                      
002400        05 MID-IDPERSON      PIC 9(3).                                    
002500*                                 PERSONKOD                               
002600        05 MID-FLBORT        PIC X.                                       
002700*                                 ALLMÄN SVARSFLAGGA                      
002800        05 MID-TEFPINST      OCCURS 10 TIMES                              
002900                             PIC X(60).                                   
003000        05 MID-IDLTERM       PIC X(8).                                    
003100*                                 LOGISKT TERMINALNAMN                    
003200*** END OF VILMAII-COPY LENGTH= 676 BYTES                                 
