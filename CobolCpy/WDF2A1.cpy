000100 01  SEQA-WDF2A1.                                                         
000200*                                 DIREKTLEV. STYRNING                     
000300*                                 SEKUNDÄRT INDEX TILL WDF201,            
000400*                                 ARTIKEL INGÅNG VIA WDF212               
000500*                                 FYSISK NYCKEL: WDF2A1KY                 
000600*                                 (IDARTNR + IDLEVNR)                     
000700*                                 SECONDARY NYCKEL: WDF2ASEQ              
000800*                                 (IDARTNR)                               
000900     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 SEQA-IDLEVNR         PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 SEQA-DASTADAT        PIC 9(8).                                    
001400*                                 GENERELLT STARTDATUM                    
001500     03 SEQA-KVLS-DLEV       PIC S9(7)           COMP-3.                  
001600*                                 LAGERSALDO HOS DIREKTLEVENATÖR          
001700     03 SEQA-TIINLMOT        PIC S9(7)           COMP-3.                  
001800*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
001900     03 SEQA-TIREGDAT        PIC S9(7)           COMP-3.                  
002000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002100     03 SEQA-IDDIRGRP        PIC X(10).                                   
002200*                                 DIREKTLEVERANSGRUPP                     
002300*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
