000100 01  SEQE-WDJ2E1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDJ211             
000300*                                 EXIT: INDEX FINNS NÄR                   
000400*                                 FLSATUTS  = N                           
000500*                                 SATSORDERREGISTER                       
000600*                                 FYSISK NYCKEL: WDJ2E1KY                 
000700*                                 (KDCLAGER, IDARTNR, IDORDNST)           
000800*                                 SEKUNDÄR NYCKEL: WDJ2ESEQ               
000900*                                 (KDCLAGER, IDARTNR)                     
001000     03 SEQE-KDCLAGER        PIC S9              COMP-3.                  
001100*                                 CENTRALLAGERKOD                         
001200*                                 CENTRAL WAREHOUSE CODE                  
001300     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 SEQE-IDORDNST.                                                    
001700*                                 SATSORDERNUMMER-TOTALT                  
001800*                                 KIT-ORDER-NUMBER-TOTAL                  
001900        05 SEQE-IDORDNSB     PIC S9(5)           COMP-3.                  
002000*                                 SATSORDERNUMMER-BAS                     
002100*                                 KIT-ORDER-NUMBER-BASIC                  
002200        05 SEQE-IDORDNSS     PIC S9              COMP-3.                  
002300*                                 SATSORDERNUMMER-SUFFIX                  
002400*                                 KIT-ORDER-NUMBER-SUFFIX                 
002500*** END COPY WDJ2E1CCC0  LENGTH=10                                        
