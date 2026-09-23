000100 01  SEQC-WDJ2C1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDJ201             
000300*                                 SATSORDERREGISTER                       
000400*                                 FYSISK NYCKEL: WDJ2C1KY                 
000500*                                 (IDARTNR,  DAREGDAT, IDORDNST)          
000600*                                 SEKUNDÄR NYCKEL: WDJ2CSEQ               
000700*                                 (IDARTNR,  DAREGDAT)                    
000800     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 SEQC-DAREGDAT        PIC 9(8).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001300*                                 REGISTRATION DATE (YYYYMMDD)            
001400     03 SEQC-IDORDNST.                                                    
001500*                                 SATSORDERNUMMER-TOTALT                  
001600*                                 KIT-ORDER-NUMBER-TOTAL                  
001700        05 SEQC-IDORDNSB     PIC S9(5)           COMP-3.                  
001800*                                 SATSORDERNUMMER-BAS                     
001900*                                 KIT-ORDER-NUMBER-BASIC                  
002000        05 SEQC-IDORDNSS     PIC S9              COMP-3.                  
002100*                                 SATSORDERNUMMER-SUFFIX                  
002200*                                 KIT-ORDER-NUMBER-SUFFIX                 
002300*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
