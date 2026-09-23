000100 01  MOD-W6O18401.                                                        
000200*                                 MOD COPYTEXT FÖR W60184                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-IDARTNR-IN       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDFPINST-IN      PIC X(7).                                    
001600*                                 FÖRPACKNINGSINSTRUKTION NR              
001700     03 MOD-IDFPINST-UT      PIC X(7).                                    
001800*                                 FÖRPACKNINGSINSTRUKTION NR              
001900     03 MOD-RAD              OCCURS 10 TIMES.                             
002000        05 MOD-IDLEVNR       PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200        05 MOD-IDFPINST      PIC Z(7).                                    
002300*                                 FÖRPACKNINGSINSTRUKTION NR              
002400        05 MOD-IDARTNR       PIC Z(8)9.                                   
002500*                                 ARTIKELNUMMER                           
002600     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-IDARTNR-UPD      PIC 9(9).                                    
002900*                                 ARTIKELNUMMER                           
003000     03 MOD-IDFPINST-ATTR    PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-IDFPINST-UPD     PIC 9(7).                                    
003300*                                 FÖRPACKNINGSINSTRUKTION NR              
003400     03 MOD-FLBORT-ATTR      PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-FLBORT           PIC X.                                       
003700*                                 ALLMÄN SVARSFLAGGA                      
003800     03 MOD-TEMFSINF         PIC X(55).                                   
003900*                                 INFORMATIONSMEDDELANDE                  
004000*** END OF VILMAII-COPY LENGTH= 374 BYTES                                 
