000100 01  SEQB-WDJ2B1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDJ201             
000300*                                 SATSORDERREGISTER                       
000400*                                 EXIT: INDEX FINNS NÄR                   
000500*                                 KDSATSTA  = R                           
000600*                                 FYSISK NYCKEL: WDJ2B1KY                 
000700*                                 (IDANSK,   FLBYGGB,  IDARTNR,           
000800*                                  DAREGDAT, IDORDNST)                    
000900*                                 SEKUNDÄR NYCKEL: WDJ2BSEQ               
001000*                                 (IDANSK,   FLBYGGB,  IDARTNR,           
001100*                                  DAREGDAT)                              
001200     03 SEQB-IDANSK          PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400*                                 PROCURER NO.                            
001500     03 SEQB-FLBYGGB         PIC X.                                       
001600*                                 FLAGGA BYGGBAR SATSORDER                
001700*                                 FLAG POSSIBLE TO BUILD KIT-ORDE         
001800*                                 R                                       
001900     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 SEQB-DAREGDAT        PIC 9(8).                                    
002300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002400*                                 REGISTRATION DATE (YYYYMMDD)            
002500     03 SEQB-IDORDNST.                                                    
002600*                                 SATSORDERNUMMER-TOTALT                  
002700*                                 KIT-ORDER-NUMBER-TOTAL                  
002800        05 SEQB-IDORDNSB     PIC S9(5)           COMP-3.                  
002900*                                 SATSORDERNUMMER-BAS                     
003000*                                 KIT-ORDER-NUMBER-BASIC                  
003100        05 SEQB-IDORDNSS     PIC S9              COMP-3.                  
003200*                                 SATSORDERNUMMER-SUFFIX                  
003300*                                 KIT-ORDER-NUMBER-SUFFIX                 
003400*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
