000100 01  W213L325.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21332 MOT HÄNDELSEREGISTRET            
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 INSERT-WDGX2204     VALUE +402.                                  
000700      88 INSERT-WDGX2214     VALUE +403.                                  
000800      88 INSERT-WDGX2302     VALUE +404.                                  
000900*                                 ANROPSTYP FÖR SYSTEM R2XX               
001000     03 IOAREA.                                                           
001100*                                                                         
001200        05 IDARTNR           PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400        05 KDLPORS           PIC S9(3)           COMP-3.                  
001500*                                 LEVERANSPLANEORSAK                      
001600        05 IDLEVNR           PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800        05 IDDC              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
