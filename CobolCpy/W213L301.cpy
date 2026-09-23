000100 01  W213L301.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21330 MOT HÄNDELSEREGISTER             
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ROT            VALUE +101.                                  
000700      88 LAES-DELETE-SEGM3   VALUE +102.                                  
000800*                                 ANROPSTYP FÖR SYSTEM R2XX               
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDHTYP               PIC X(4).                                    
001400*                                 HÄNDELSETYP                             
001500     03 IO-AREA.                                                          
001600        05 IDARTNR           PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800        05 IDLEVNR           PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000        05 IDSYSTEM          PIC X(4).                                    
002100*                                 VOLVO VCCS SYSTEMNUMMER                 
002200*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
