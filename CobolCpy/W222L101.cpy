000100 01  W222L101.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22210 MOT HÄNDELSEREGISTER             
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ROT            VALUE +101.                                  
000700      88 LAES-DELETE-DATA-03 VALUE +102.                                  
000800      88 LAES-DELETE-DATA-04 VALUE +103.                                  
000900*                                                                         
001000     03 FLAGGA-ANROP         PIC X.                                       
001100      88 POST-FINNS          VALUE 'J'.                                   
001200      88 POST-SAKNAS         VALUE 'N'.                                   
001300*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001400     03 IDHTYP               PIC X(4).                                    
001500*                                 HÄNDELSETYP                             
001600     03 IO-AREA              PIC X(26).                                   
001700*** END COPY W222L101C0  LENGTH=33                                        
