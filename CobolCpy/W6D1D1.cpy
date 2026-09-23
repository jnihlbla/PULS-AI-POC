000100 01  SEQD-W6D1D1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 INLÄGGNINGSLIST INGÅNG                  
000500*                                 TAS BORT NÄR LISTAN INLAGD              
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 IDILIST   > ZERO                        
000800*                                 FYSISK NYCKEL: W6D1D1KY                 
000900*                                 (IDILIST,  IDILIRAD,                    
001000*                                  IDRADNR.INL,  IDDC, IDLEVNR            
001100*                                  IDFS, TIAVIDAT, IDRADNR)               
001200*                                 SECONDARY NYCKEL: W6D1DSEQ              
001300*                                 (IDILIST,  IDILIRAD)                    
001400     03 SEQD-IDILIST         PIC 9(5).                                    
001500*                                 INLÄGGNINGSLISTEIDENTITET               
001600*                                 REPORTINGLIST-IDENTITY                  
001700     03 SEQD-IDILIRAD        PIC S9(5)           COMP-3.                  
001800*                                 INLÄGGNINGSLISTERADNUMMER               
001900*                                 REPORTINGLISTLINENUMBER                 
002000     03 SEQD-IDRADNR-INL     PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER INLEVERANS                    
002200*                                 LINE NUMBER GOODS RECEIVING             
002300     03 SEQD-IDDC            PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 SEQD-IDLEVNR         PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 SEQD-IDFS            PIC X(8).                                    
003000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003100*                                 ADVICE NOTE NUMBER ODETTE               
003200     03 SEQD-TIAVIDAT        PIC S9(7)           COMP-3.                  
003300*                                 AVISERINGSDATUM (YYMMDD)                
003400*                                 ADVICE NOTE DATE                        
003500     03 SEQD-IDRADNR         PIC S9(5)           COMP-3.                  
003600*                                 RADNUMMER                               
003700*                                 LINE NO                                 
003800*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
