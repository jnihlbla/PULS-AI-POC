000100 01  SEQI-WDQ3I1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 PRCPLK-INGÅNG STATUS U/P                
000500*                                 INDEX FINNS NÄR KDODELSTA = U/P         
000600*                                         OCH IDPRCPLK EJ SPACE           
000700*                                         OCH IDLOTNR-PLK > 0             
000800*                                 FYSISK NYCKEL: WDQ3I1KY                 
000900*                                 (IDDC. IDPRCPLK, IDLOTNR-PLK)           
001000*                                  IDPRODNR, IDPLKLST)                    
001100*                                 SECONDARY NYCKEL: WDQ3ISEQ              
001200*                                 (IDDC. IDPRCPLK, IDLOTNR-PLK)           
001300     03 SEQI-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQI-IDPRCPLK        PIC X(4).                                    
001700*                                 ID FÖR EN PLOCKRUNDA                    
001800*                                 ID FOR A PICKING UNIT                   
001900     03 SEQI-IDLOTNR-PLK     PIC S9(3)           COMP-3.                  
002000*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
002100*                                 ORDERLOT NUMBER FOR A PICKLIST          
002200     03 SEQI-IDPRODNR        PIC S9(7)           COMP-3.                  
002300*                                 PRODUKTIONSNUMMER                       
002400*                                 PRODUCTION NUMBER                       
002500     03 SEQI-IDPLKLST        PIC S9(3)           COMP-3.                  
002600*                                 PLOCKLISTNUMMER                         
002700*                                 PICKING LIST NUMBER                     
002800     03 SEQI-IDWDQ301        PIC X(12).                                   
002900*                                 NYCKEL TILL WDQ301                      
003000*                                 KEY TO WDQ301                           
003100*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
