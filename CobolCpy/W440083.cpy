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
002600     03 PRARTNTO-LOCPREL     PIC S9(7)V9(2)      COMP-3.                  
002700*                                 PREL NETTO SLUTKUNDSPRIS I              
002800*                                 LOKAL VALUTA                            
002900*                                 PREL NET PRICE - LOCAL CURRENCY         
003000     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
003100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003200*                                 NET PRICE EACH LOCAL CURRENCY           
003300     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 ARTIKELPRIS NETTO                       
003500*                                 NET PRICE EACH   (FOB NET)              
003600     03 TEASTRIX             PIC X.                                       
003700*                                 ASTERISK                                
003800*                                 ASTERISK                                
003900     03 FLSPARR              PIC X.                                       
004000*                                 SPÄRRAD ORDER ?                         
004100*                                 BLOCKED ORDER ?                         
004200     03 FLSALDCH             PIC X.                                       
004300*                                 SALDOKONTROLL ?                         
004400     03 IDLANDX3             PIC X(3).                                    
004500*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
004600*                                 3-LETTER CODE FOR COUNTRY.              
004700     03 DARODAT              PIC 9(8).                                    
004800*                                 RESTORDERDATUM       (ÅÅÅÅMMDD)         
004900*                                 BACK ORDER DATE      (YYYYMMDD)         
005000     03 SUORDV               PIC S9(9)V9(2)      COMP-3.                  
005100*                                 SUMMA ORDERVÄRDE                        
005200*                                 TOTAL ORDER VALUE                       
005300     03 IDDC-RO              PIC X(2).                                    
005400*                                 LAGER DÄR RESTORDER FÅR SKE             
005500*                                 WAREHOUSE FOR BACKORDERS                
005600     03 LEVBSK               OCCURS 3 TIMES.                              
005700        05 TIAAVVD           PIC S9(5)           COMP-3.                  
005800*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005900*                                 YEAR - WEEK - DAY  (YYWWD)              
006000     03 IDDISTR              PIC S9(5)           COMP-3.                  
006100*                                 DISTRIKTNUMMER                          
006200*                                 DISTRICT NUMBER                         
006300     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
006400*                                 KUNDNUMMER                              
006500*                                 CUSTOMER NO                             
006600     03 IDORDNR5             PIC 9(5).                                    
006700*                                 ORDERNUMMER                             
006800*                                 ORDER NUMBER                            
006900     03 KVRO                 PIC S9(7)           COMP-3.                  
007000*                                 ANTAL RESTNOTERADE ARTIKLAR             
007100*                                 BACKORDERED QTY                         
007200     03 TIREGDAT             PIC S9(7)           COMP-3.                  
007300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007400*                                 REGISTRATION DATE (YYMMDD)              
007500*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
