000100 01  SEQJ-WDQ3J1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 PICK-ROUND PER PRINT DATE               
000500*                                 INDEX FINNS NÄR KDODELSTA = U           
000600*                                 FYSISK NYCKEL: WDQ3J1KY                 
000700*                                 (IDDC, DAUTSKR, TIUTSTID,               
000800*                                  IDPRCPLK, IDLOTNR-PLK)                 
000900*                                  IDPRODNR, IDPLKLST)                    
001000*                                 SECONDARY NYCKEL: WDQ3JSEQ              
001100*                                 (IDDC, DAUTSKR, TIUTSTID)               
001200     03 SEQJ-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 SEQJ-DAUTSKR         PIC 9(8).                                    
001600*                                 UTSKRIFTDATUM  (ÅÅÅÅMMDD)               
001700*                                 PRINTING DATE  (CCYYMMDD)               
001800     03 SEQJ-TIUTSTID        PIC S9(7)           COMP-3.                  
001900*                                 UTSKRIFTSTID (TTMMSS)                   
002000*                                 TIME OF PRINTING (HHMMSS)               
002100     03 SEQJ-IDPRCPLK        PIC X(4).                                    
002200*                                 ID FÖR EN PLOCKRUNDA                    
002300*                                 ID FOR A PICKING UNIT                   
002400     03 SEQJ-IDLOTNR-PLK     PIC S9(3)           COMP-3.                  
002500*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
002600*                                 ORDERLOT NUMBER FOR A PICKLIST          
002700     03 SEQJ-IDPRODNR        PIC S9(7)           COMP-3.                  
002800*                                 PRODUKTIONSNUMMER                       
002900*                                 PRODUCTION NUMBER                       
003000     03 SEQJ-IDPLKLST        PIC S9(3)           COMP-3.                  
003100*                                 PLOCKLISTNUMMER                         
003200*                                 PICKING LIST NUMBER                     
003300     03 SEQJ-IDUSER          PIC X(8).                                    
003400*                                 ANVÄNDARENS SÄKERHETS ID                
003500*                                 USER SECURITY-IDENTITY                  
003600     03 SEQJ-IDWDQ301        PIC X(12).                                   
003700*                                 NYCKEL TILL WDQ301                      
003800*                                 KEY TO WDQ301                           
003900*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
