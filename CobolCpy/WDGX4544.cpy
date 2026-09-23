000100 01  4544-WDGX4544.                                                       
000200*                                 DHLS POSTNR OCH ORTSBETECKNING          
000300*                                 FYSISK NYCKEL: KY4544                   
000400*                                 (IDLANDX2 + ADPOSTNR-FOM +              
000500*                                  ADPOSTNR-TOM)                          
000600     03 4544-IDLANDX2        PIC X(2).                                    
000700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000800*                                 2-LETTER CODE FOR COUNTRY               
000900     03 4544-ADPOSTNR-FOM    PIC X(10).                                   
001000*                                 LÄGSTA POSTNUMMER I INTERVALL           
001100*                                 LOWEST POSTAL NUMBER                    
001200     03 4544-ADPOSTNR-TOM    PIC X(10).                                   
001300*                                 HÖGSTA POSTNUMMER I INTERVALL           
001400*                                 HIGHEST POSTAL NUMBER                   
001500     03 4544-IDCITY          PIC X(3).                                    
001600*                                 3-STÄLLIG ORTSBETECKNINGSKOD            
001700*                                 3-LETTER CODE FOR CITY                  
001800*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
