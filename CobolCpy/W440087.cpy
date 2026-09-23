000100 01  W440083.                                                             
000200*                                 USED FOR 3 BACKORDER LISTS - W4         
000300*                                 40V2                                    
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 BEART                PIC X(25).                                   
000800*                                 ARTIKELBENÄMNING                        
000900*                                 PART DESCRIPTION                        
001000     03 IDLEVNR              PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001400*                                 FUNKTIONSGRUPP                          
001500*                                 FUNCTION GROUP                          
001600     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001700*                                 PRODUKTSLAG                             
001800*                                 PRODUCT GROUP                           
001900     03 KVRADER              PIC S9(5)           COMP-3.                  
002000*                                 ANTAL RADER                             
002100*                                 NUMBER OF LINES                         
002200     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
002300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002400*                                 ORDERED QUANTITY ADAPTED                
002500*                                  ITEMS                                  
002600     03 TEASTRIX             PIC X.                                       
002700*                                 ASTERISK                                
002800*                                 ASTERISK                                
002900     03 FLSPARR              PIC X.                                       
003000*                                 SPÄRRAD ORDER ?                         
003100*                                 BLOCKED ORDER ?                         
003200     03 FLSALDCH             PIC X.                                       
003300*                                 SALDOKONTROLL ?                         
003400     03 IDLANDX3             PIC X(3).                                    
003500*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
003600*                                 3-LETTER CODE FOR COUNTRY.              
003700     03 DARODAT              PIC S9(5)           COMP-3.                  
003800*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
003900*                                 YEAR - WEEK - DAY  (YYWWD)              
004000     03 SUORDV               PIC S9(9)V9(2)      COMP-3.                  
004100*                                 SUMMA ORDERVÄRDE                        
004200*                                 TOTAL ORDER VALUE                       
004300     03 LEVBSK               OCCURS 3 TIMES.                              
004400        05 TIAAVVD           PIC S9(5)           COMP-3.                  
004500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
004600*                                 YEAR - WEEK - DAY  (YYWWD)              
004700     03 IDDISTR              PIC S9(5)           COMP-3.                  
004800*                                 DISTRIKTNUMMER                          
004900*                                 DISTRICT NUMBER                         
005000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
005100*                                 KUNDNUMMER                              
005200*                                 CUSTOMER NO                             
005300     03 IDORDNR5             PIC 9(5).                                    
005400*                                 ORDERNUMMER                             
005500*                                 ORDER NUMBER                            
005600     03 KVRO                 PIC S9(7)           COMP-3.                  
005700*                                 ANTAL RESTNOTERADE ARTIKLAR             
005800*                                 BACKORDERED QTY                         
005900     03 TIREGDAT             PIC S9(7)           COMP-3.                  
006000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006100*                                 REGISTRATION DATE (YYMMDD)              
006200*** END OF VILMAII-COPY LENGTH= 91 BYTES                                  
