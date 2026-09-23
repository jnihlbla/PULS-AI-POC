000100 01  SEQA-WDF1A1.                                                         
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 SEKUNDÄRT INDEX TILL WDF122,            
000400*                                 MAILTYP INGÅNG                          
000500*                                 FYSISK NYCKEL: WDF1A1KY                 
000600*                                 (IDLEVNR + IDDC-KLEV +                  
000700*                                  KDMAIL + IDATTENT)                     
000800*                                 SECONDARY NYCKEL: WDF1ASEQ              
000900*                                 (IDLEVNR + IDDCKLEV)                    
001000*                                                                         
001100     03 SEQA-IDLEVNR         PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001400     03 SEQA-IDDC-KLEV       PIC X(2).                                    
001500*                                 DC FÖR KONTAKT LEVERANTÖR               
001600*                                 DC FOR CONTACT PERS AT SUPPLIER         
001700     03 SEQA-KDMAIL          PIC X(4).                                    
001800*                                 TYP AV MAIL UTSKICK                     
001900*                                 TYPE OF MAIL SENDNINGS                  
002000     03 SEQA-IDATTENT        PIC S9(3)           COMP-3.                  
002100*                                 ATTENTION NUMMER                        
002200*                                 ATTENTION NUMBER                        
002300*** END OF VILMAII-COPY LENGTH= 13 BYTES                                  
