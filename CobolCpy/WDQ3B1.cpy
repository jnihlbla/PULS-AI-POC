000100 01  SEQB-WDQ3B1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 PRC-UTSKRIFT                            
000500*                                 TAS BORT NÄR ORDDELEN SKRIVS UT         
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 KDODELSTA = R                           
000800*                                 FYSISK NYCKEL: WDQ3B1KY                 
000900*                                 (IDDC,     IDPRCBAS, DAUTSKR,           
001000*                                  TIUTSTID, DARFS,   DALSTORD,           
001100*                                  IDPRCVAR, IDORDER,                     
001200*                                  IDPRODNR, IDPLKLST)                    
001300*                                 SECONDARY NYCKEL: WDQ3BSEQ              
001400*                                 (IDDC,     IDPRCBAS, DAUTSKR,           
001500*                                  TIUTSTID, DARFS,   DALSTORD,           
001600*                                  IDPRCVAR)                              
001700     03 SEQB-IDDC            PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 SEQB-IDPRCBAS        PIC X(3).                                    
002100*                                 PRC-BAS                                 
002200*                                 PRC-BASIC                               
002300     03 SEQB-DAUTSKR         PIC 9(8).                                    
002400*                                 UTSKRIFTDATUM  (ÅÅÅÅMMDD)               
002500*                                 PRINTING DATE  (CCYYMMDD)               
002600     03 SEQB-TIUTSTID        PIC S9(7)           COMP-3.                  
002700*                                 UTSKRIFTSTID (TTMMSS)                   
002800*                                 TIME OF PRINTING (HHMMSS)               
002900     03 SEQB-DARFS           PIC 9(12).                                   
003000*                                 KLART FÖR TRANSPORT                     
003100*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
003200     03 SEQB-DALSTORD        PIC 9(12).                                   
003300*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
003400*                                 LATEST START-TIME ORDER                 
003500     03 SEQB-IDPRCVAR        PIC X.                                       
003600*                                 PRC-VARIANT                             
003700*                                 PRC-VARIANT                             
003800     03 SEQB-IDORDER         PIC S9(7)           COMP-3.                  
003900*                                 VOLVO PARTS ORDERNUMMER                 
004000*                                 VOLVO PARTS ORDER NUMBER                
004100     03 SEQB-IDPRODNR        PIC S9(7)           COMP-3.                  
004200*                                 PRODUKTIONSNUMMER                       
004300*                                 PRODUCTION NUMBER                       
004400     03 SEQB-IDPLKLST        PIC S9(3)           COMP-3.                  
004500*                                 PLOCKLISTNUMMER                         
004600*                                 PICKING LIST NUMBER                     
004700     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
004800*                                 DISTRIKTNUMMER                          
004900*                                 DISTRICT NUMBER                         
005000     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
005100*                                 KUNDNUMMER                              
005200*                                 CUSTOMER NO                             
005300     03 SEQB-IDWDQ301        PIC X(12).                                   
005400*                                 NYCKEL TILL WDQ301                      
005500*                                 KEY TO WDQ301                           
005600*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
