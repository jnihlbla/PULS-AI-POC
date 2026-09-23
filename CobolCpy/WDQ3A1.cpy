000100 01  SEQA-WDQ3A1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 TRANSPORT-AVGÅNG                        
000500*                                 TAS BORT NÄR ORDDELEN SKRIVS UT         
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 KDODELSTA = R                           
000800*                                 FYSISK NYCKEL: WDQ3A1KY                 
000900*                                 (IDDC,     IDTRP,    DATRPAVT,          
001000*                                  DARFS,    DALSTORD, IDPRC,             
001100*                                  IDORDER,  IDPRODNR, IDPLKLST)          
001200*                                 SECONDARY NYCKEL: WDQ3ASEQ              
001300*                                 (IDDC,     IDTRP,    DATRPAVT,          
001400*                                  DARFS,    DALSTORD, IDPRC)             
001500     03 SEQA-IDDC            PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 SEQA-IDTRP.                                                       
001900*                                 TRANSPORTIDENTITET                      
002000*                                 TRANSPORTIDENTITY                       
002100        05 SEQA-IDTRPLOS     PIC X(3).                                    
002200*                                 TRANSPORTLÖSNING                        
002300*                                 TRANSPORTSOLUTION                       
002400        05 SEQA-IDTRPVAR     PIC X(2).                                    
002500*                                 TRANSPORTLÖSNINGSGRUPP                  
002600*                                 TRANSPORTSOLUTIONGROUP                  
002700     03 SEQA-DATRPAVT.                                                    
002800*                                 TRANSPORTAVGÅNGSTIDPUNKT                
002900*                                 TRANSPORT DEPARTURE                     
003000*                                 YYYYMMDD+HHMM                           
003100        05 SEQA-DATRPAVD     PIC 9(8).                                    
003200*                                 TRANSPORTAVGÅNGSDATUM                   
003300*                                 TRANSPORT DEPARTURE DATE                
003400        05 SEQA-TIHHMM       PIC S9(5)           COMP-3.                  
003500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003600*                                 TIME IN HOUR AND MINUTE                 
003700     03 SEQA-DARFS           PIC 9(12).                                   
003800*                                 KLART FÖR TRANSPORT                     
003900*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
004000     03 SEQA-DALSTORD        PIC 9(12).                                   
004100*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
004200*                                 LATEST START-TIME ORDER                 
004300     03 SEQA-IDPRC.                                                       
004400*                                 PRODUKTIONSKANAL                        
004500*                                 PRODUCTION CHANNEL                      
004600        05 SEQA-IDPRCBAS     PIC X(3).                                    
004700*                                 PRC-BAS                                 
004800*                                 PRC-BASIC                               
004900        05 SEQA-IDPRCVAR     PIC X.                                       
005000*                                 PRC-VARIANT                             
005100*                                 PRC-VARIANT                             
005200     03 SEQA-IDORDER         PIC S9(7)           COMP-3.                  
005300*                                 VOLVO PARTS ORDERNUMMER                 
005400*                                 VOLVO PARTS ORDER NUMBER                
005500     03 SEQA-IDPRODNR        PIC S9(7)           COMP-3.                  
005600*                                 PRODUKTIONSNUMMER                       
005700*                                 PRODUCTION NUMBER                       
005800     03 SEQA-IDPLKLST        PIC S9(3)           COMP-3.                  
005900*                                 PLOCKLISTNUMMER                         
006000*                                 PICKING LIST NUMBER                     
006100     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
006200*                                 DISTRIKTNUMMER                          
006300*                                 DISTRICT NUMBER                         
006400     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
006500*                                 KUNDNUMMER                              
006600*                                 CUSTOMER NO                             
006700     03 SEQA-IDWDQ301        PIC X(12).                                   
006800*                                 NYCKEL TILL WDQ301                      
006900*                                 KEY TO WDQ301                           
007000*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
