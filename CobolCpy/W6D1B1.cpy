000100 01  SEQB-W6D1B1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 PARTI (IDLOPNRM) INGÅNG                 
000500*                                 EXIT: INDEX FINNS NÄR                   
000600*                                 IDLOPNRM  > ZERO                        
000700*                                 FYSISK NYCKEL: W6D1B1KY                 
000800*                                 (IDLOPNRM, IDRADNR-INL,                 
000900*                                 IDDC,IDLEVNR, IDFS, TIAVIDAT)           
001000*                                 SECONDARY NYCKEL: W6D1BSEQ              
001100*                                 (IDLOPNRM)                              
001200     03 SEQB-IDLOPNRM        PIC S9(9)           COMP-3.                  
001300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001400*                                 (0VVDLLLLK)                             
001500*                                 SERIAL NO RECEIVING REPORT              
001600*                                 (0WWDLLLLC)                             
001700     03 SEQB-IDRADNR-INL     PIC S9(5)           COMP-3.                  
001800*                                 RADNUMMER INLEVERANS                    
001900*                                 LINE NUMBER GOODS RECEIVING             
002000     03 SEQB-IDDC            PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200*                                 WAREHOUSE IDENTIFIER                    
002300     03 SEQB-IDLEVNR         PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002600     03 SEQB-IDFS            PIC X(8).                                    
002700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002800*                                 ADVICE NOTE NUMBER ODETTE               
002900     03 SEQB-TIAVIDAT        PIC S9(7)           COMP-3.                  
003000*                                 AVISERINGSDATUM (YYMMDD)                
003100*                                 ADVICE NOTE DATE                        
003200*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
