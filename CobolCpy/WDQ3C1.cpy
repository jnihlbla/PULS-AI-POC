000100 01  SEQC-WDQ3C1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 PRC-UTSKRIFT                            
000500*                                 TAS BORT NÄR ORDDELEN PACKATS           
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 KDODELSTA = R ELLER U                   
000800*                                 FYSISK NYCKEL: WDQ3C1KY                 
000900*                                 (IDDC,     IDPRC, DARFS,                
001000*                                  DALSTORD, IDORDER,                     
001100*                                  IDPRODNR, IDPLKLST)                    
001200*                                 SECONDARY NYCKEL: WDQ3CSEQ              
001300*                                 (IDDC,     IDPRC, DARFS,                
001400*                                  DALSTORD, IDORDER)                     
001500     03 SEQC-IDDC            PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 SEQC-IDPRC.                                                       
001900*                                 PRODUKTIONSKANAL                        
002000*                                 PRODUCTION CHANNEL                      
002100        05 SEQC-IDPRCBAS     PIC X(3).                                    
002200*                                 PRC-BAS                                 
002300*                                 PRC-BASIC                               
002400        05 SEQC-IDPRCVAR     PIC X.                                       
002500*                                 PRC-VARIANT                             
002600*                                 PRC-VARIANT                             
002700     03 SEQC-DARFS           PIC 9(12).                                   
002800*                                 KLART FÖR TRANSPORT                     
002900*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
003000     03 SEQC-DALSTORD        PIC 9(12).                                   
003100*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
003200*                                 LATEST START-TIME ORDER                 
003300     03 SEQC-IDORDER         PIC S9(7)           COMP-3.                  
003400*                                 VOLVO PARTS ORDERNUMMER                 
003500*                                 VOLVO PARTS ORDER NUMBER                
003600     03 SEQC-IDPRODNR        PIC S9(7)           COMP-3.                  
003700*                                 PRODUKTIONSNUMMER                       
003800*                                 PRODUCTION NUMBER                       
003900     03 SEQC-IDPLKLST        PIC S9(3)           COMP-3.                  
004000*                                 PLOCKLISTNUMMER                         
004100*                                 PICKING LIST NUMBER                     
004200     03 SEQC-KVRADER         PIC S9(5)           COMP-3.                  
004300*                                 ANTAL RADER                             
004400*                                 NUMBER OF LINES                         
004500     03 SEQC-SUPTID          PIC S9(3)V9(2)      COMP-3.                  
004600*                                 TOTAL PRODUKTIONSTID TIM+MIN            
004700*                                 TOTAL PRODUCTIONTIME HOUR MIN.          
004800     03 SEQC-KDODELSTA       PIC X.                                       
004900*                                 ORDERDELSTATUS                          
005000*                                 ORDER PART STATUS                       
005100     03 SEQC-IDWDQ301        PIC X(12).                                   
005200*                                 NYCKEL TILL WDQ301                      
005300*                                 KEY TO WDQ301                           
005400*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
