000100 01  SEQH-W6D1H1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 ARTIKEL INGÅNG                          
000500*                                 FYSISK NYCKEL: W6D1H1KY                 
000600*                                 (IDARTNR, IDDC, IDLEVNR, IDFS,          
000700*                                  TIAVIDAT, IDRADNR-INL)                 
000800*                                 SECONDARY NYCKEL: W6D1HSEQ              
000900*                                 (IDARTNR)                               
001000     03 SEQH-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 SEQH-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQH-IDLEVNR         PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 SEQH-IDFS            PIC X(8).                                    
002000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002100*                                 ADVICE NOTE NUMBER ODETTE               
002200     03 SEQH-TIAVIDAT        PIC S9(7)           COMP-3.                  
002300*                                 AVISERINGSDATUM (YYMMDD)                
002400*                                 ADVICE NOTE DATE                        
002500     03 SEQH-IDRADNR-INL     PIC S9(5)           COMP-3.                  
002600*                                 RADNUMMER INLEVERANS                    
002700*                                 LINE NUMBER GOODS RECEIVING             
002800*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
