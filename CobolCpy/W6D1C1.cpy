000100 01  SEQC-W6D1C1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 LEVERANTÖR - KOLLI INGÅNG               
000500*                                 TAS BORT OM KOLLI SAKNAS                
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 IDOKOLLI  > ZERO                        
000800*                                 FYSISK NYCKEL: W6D1C1KY                 
000900*                                 (IDLEVNRK, IDOKOLLI                     
001000*                                  IDRADNR-INL,IDDC, IDLEVNR              
001100*                                  IDFS,     TIAVIDAT, IDRADNR)           
001200*                                 SECONDARY NYCKEL: W6D1CSEQ              
001300*                                 (IDLEVNRK, IDOKOLLI)                    
001400     03 SEQC-IDLEVNR-KOLLI   PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER KOLLI                  
001600*                                 SUPPLIER NUMBER CASE                    
001700     03 SEQC-IDOKOLLI        PIC 9(9).                                    
001800*                                 ODETTE KOLLINUMMER                      
001900*                                 ODETTE CASE NUMBER                      
002000     03 SEQC-IDRADNR-INL     PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER INLEVERANS                    
002200*                                 LINE NUMBER GOODS RECEIVING             
002300     03 SEQC-IDDC            PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 SEQC-IDLEVNR         PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 SEQC-IDFS            PIC X(8).                                    
003000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003100*                                 ADVICE NOTE NUMBER ODETTE               
003200     03 SEQC-TIAVIDAT        PIC S9(7)           COMP-3.                  
003300*                                 AVISERINGSDATUM (YYMMDD)                
003400*                                 ADVICE NOTE DATE                        
003500     03 SEQC-IDRADNR         PIC S9(5)           COMP-3.                  
003600*                                 RADNUMMER                               
003700*                                 LINE NO                                 
003800*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
