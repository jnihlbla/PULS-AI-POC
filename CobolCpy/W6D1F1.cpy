000100 01  SEQF-W6D1F1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 VAGNS INGÅNG                            
000500*                                 TAS BORT NÄR VAGN SAKNAS                
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 IDINLVGN  > ZERO                        
000800*                                 FYSISK NYCKEL: W6D1F1KY                 
000900*                                 (IDINLVGN, IDRADNR-INL, IDDC            
001000*                                  IDLEVNR,  IDFS,     TIAVIDAT,          
001100*                                  IDRADNR)                               
001200*                                 SECONDARY NYCKEL: W6D1FSEQ              
001300*                                 (IDINLVGN)                              
001400     03 SEQF-IDINLVGN        PIC 9(3).                                    
001500*                                 VAGNSIDENTITET                          
001600*                                 INTERNAL CARRIER ID                     
001700     03 SEQF-IDRADNR-INL     PIC S9(5)           COMP-3.                  
001800*                                 RADNUMMER INLEVERANS                    
001900*                                 LINE NUMBER GOODS RECEIVING             
002000     03 SEQF-IDDC            PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200*                                 WAREHOUSE IDENTIFIER                    
002300     03 SEQF-IDLEVNR         PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002600     03 SEQF-IDFS            PIC X(8).                                    
002700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002800*                                 ADVICE NOTE NUMBER ODETTE               
002900     03 SEQF-TIAVIDAT        PIC S9(7)           COMP-3.                  
003000*                                 AVISERINGSDATUM (YYMMDD)                
003100*                                 ADVICE NOTE DATE                        
003200     03 SEQF-IDRADNR         PIC S9(5)           COMP-3.                  
003300*                                 RADNUMMER                               
003400*                                 LINE NO                                 
003500*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
