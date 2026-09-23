000100 01  MID-W4I22701.                                                        
000200*                                                                         
000300     03 MID-IDANSK-IN        PIC X(3).                                    
000400*                                 ANSKAFFARNUMMER                         
000500     03 MID-IDANSK-UT        PIC X(3).                                    
000600*                                 ANSKAFFARNUMMER                         
000700     03 MID-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MID-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MID-IDARTNR-IN       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MID-INPUT.                                                        
001600*                                 INRAPPORTERINGSDEL 4227                 
001700        05 MID-KDCMDVAL      OCCURS 14 TIMES                              
001800                             PIC X(3).                                    
001900*                                 GENERELL KOMMANDOKOD                    
002000*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
