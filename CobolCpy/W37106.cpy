000100 01  W37106.                                                              
000200*                                 FAKTURANUMMERSERIE TILL                 
000300*                                 BYTES-SYSTEMET                          
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 FILLER               PIC X.                                       
000800     03 IDLOPNR              PIC 9(3).                                    
000900*                                 LÖPNUMMER                               
001000     03 FILLER               PIC X.                                       
001100     03 IDFAKT-MIN           PIC 9(7).                                    
001200*                                 MIN GRÄNS FAKTURANUMMER                 
001300     03 FILLER               PIC X.                                       
001400     03 IDFAKT-MAX           PIC 9(7).                                    
001500*                                 MAX GRÄNS FAKTURANUMMER                 
001600     03 FILLER               PIC X.                                       
001700     03 IDFAKT-AKT           PIC 9(7).                                    
001800*                                 AKTUELLT  FAKTURANUMMER                 
001900     03 FILLER               PIC X.                                       
002000*** END COPY W37106CCC0  LENGTH=32                                        
