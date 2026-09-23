000010 01  W221L162.                                                            
000020*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000030*                                 W22116 MOT HÄNDELSEREGISTER             
000040*                                                                         
000050     03 KDCALL               PIC S9(3)           COMP-3.                  
000060      88 LAES-DLET-WDG3      VALUE +2.                                    
000070      88 ISRT-2204           VALUE +4.                                    
000080*                                 ANROPSTYP     KDCALL-W221               
000090     03 FLJANEJ-ANROP        PIC X.                                       
000100      88 POST-FINNS          VALUE 'J'.                                   
000110      88 POST-SAKNAS         VALUE 'N'.                                   
000120*                                 JA/NEJ-FLAGGA FÖR R2XX                  
000130     03 IDHTYP               PIC X(4).                                    
000140*                                 HÄNDELSETYP                             
000150     03 IOAREA.                                                           
000160        05 IDARTNR           PIC S9(9)           COMP-3.                  
000170*                                 ARTIKELNUMMER                           
000180        05 KDLPORS           PIC S9(3)           COMP-3.                  
000190*                                 LEVERANSPLANEORSAK                      
      *** END COPY W221L162    LENGTH=14                                        
