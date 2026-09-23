000100 01  SEQH-WDQ3H1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 PULIST-INGÅNG                           
000500*                                 INDEX FINNS NÄR KDODELSTA = U           
000600*                                 FYSISK NYCKEL: WDQ3H1KY                 
000700*                                 (IDDC, IDPRCPLK, IDLOTNR-PLK,           
000800*                                  IDPRODNR, IDPLKLST)                    
000900*                                 SECONDARY NYCKEL: WDQ3HSEQ              
001000*                                 (IDDC, IDPRCPLK, IDLOTNR-PLK)           
001100     03 SEQH-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQH-IDPRCPLK        PIC X(4).                                    
001500*                                 ID FÖR EN PLOCKRUNDA                    
001600*                                 ID FOR A PICKING UNIT                   
001700     03 SEQH-IDLOTNR-PLK     PIC S9(3)           COMP-3.                  
001800*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
001900*                                 ORDERLOT NUMBER FOR A PICKLIST          
002000     03 SEQH-IDPRODNR        PIC S9(7)           COMP-3.                  
002100*                                 PRODUKTIONSNUMMER                       
002200*                                 PRODUCTION NUMBER                       
002300     03 SEQH-IDPLKLST        PIC S9(3)           COMP-3.                  
002400*                                 PLOCKLISTNUMMER                         
002500*                                 PICKING LIST NUMBER                     
002600     03 SEQH-IDWDQ301        PIC X(12).                                   
002700*                                 NYCKEL TILL WDQ301                      
002800*                                 KEY TO WDQ301                           
002900*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
