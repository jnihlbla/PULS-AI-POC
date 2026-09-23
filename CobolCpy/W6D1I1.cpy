000100 01  SEQI-W6D1I1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 ARTIKEL INGÅNG                          
000500*                                 FYSISK NYCKEL: W6D1I1KY                 
000600*                                 (IDARTNR, IDDC, IDLEVNR, IDFS,          
000700*                                  TIAVIDAT, IDRADNR-INL)                 
000800*                                 SECONDARY NYCKEL: W6D1ISEQ              
000900*                                 (IDARTNR)                               
001000     03 SEQI-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 SEQI-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQI-IDLEVNR         PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 SEQI-IDFS            PIC X(8).                                    
002000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002100*                                 ADVICE NOTE NUMBER ODETTE               
002200     03 SEQI-TIAVIDAT        PIC S9(7)           COMP-3.                  
002300*                                 AVISERINGSDATUM (YYMMDD)                
002400*                                 ADVICE NOTE DATE                        
002500     03 SEQI-IDRADNR-INL     PIC S9(5)           COMP-3.                  
002600*                                 RADNUMMER INLEVERANS                    
002700*                                 LINE NUMBER GOODS RECEIVING             
002800*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
