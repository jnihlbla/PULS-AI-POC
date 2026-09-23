000100 01  MID-W6I18501.                                                        
000200*                                 MID-COPYTEXT FÖR W60185                 
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-UT       PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-TYP-IN           PIC X.                                       
001200*                                 ALLMÄN SVARSFLAGGA                      
001300     03 MID-TYP-UT           PIC X.                                       
001400*                                 ALLMÄN SVARSFLAGGA                      
001500     03 MID-SUM-IN           PIC X.                                       
001600*                                 ALLMÄN SVARSFLAGGA                      
001700     03 MID-SUM-UT           PIC X.                                       
001800*                                 ALLMÄN SVARSFLAGGA                      
001900     03 MID-INDEL.                                                        
002000        05 MID-IDLTERM       PIC X(8).                                    
002100*                                 LOGISKT TERMINALNAMN                    
002200        05 MID-RAD           OCCURS 13 TIMES.                             
002300           07 MID-CMD        PIC X.                                       
002400           07 MID-IDFPINST   PIC 9(7).                                    
002500*                                 FÖRPACKNINGSINSTRUKTION NR              
002600*** END OF VILMAII-COPY LENGTH= 144 BYTES                                 
