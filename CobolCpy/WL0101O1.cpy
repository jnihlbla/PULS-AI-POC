000100 01  RESP-WL0101O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0101         
000300*                                 LDC GOODS RECEIVING                     
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-TIFAKT-KEY      PIC X(6).                                    
000800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
000900*                                 INVOICING DATE   (YYMMDD)               
001000     03 RESP-IDLBBET-KEY     PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200*                                 TRAILER NUMBER                          
001300     03 RESP-IDDC-SEND-KEY   PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 RESP-FLTRACK         PIC X.                                       
001700*                                 FLAG FOR TRACKING-ID FOR A DC           
001800*                                                                         
001900*                                 FLAG FOR TRACKING-ID FOR A DC           
002000*                                                                         
002100     03 RESP-KVRADER         PIC Z(4)9.                                   
002200*                                 ANTAL RADER                             
002300*                                 NUMBER OF LINES                         
002400     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002500*                                 GRUPP MED TABELLRADER                   
002600        05 RESP-CMD-IN       PIC X(3).                                    
002700        05 RESP-IDDC-SEND    PIC X(2).                                    
002800*                                 SÄNDANDE LAGER                          
002900*                                 SENDING WAREHOUSE                       
003000        05 RESP-IDDC-LEV     PIC X(2).                                    
003100*                                 LEVERERANDE DC I EXPORTFLÖDET           
003200*                                 DELIVERY DC IN EXPORT FLOW              
003300        05 RESP-IDFAKT       PIC Z(6)9.                                   
003400*                                 FAKTURANUMMER                           
003500*                                 INVOICE NO.                             
003600        05 RESP-IDSHIPM      PIC Z(7).                                    
003700*                                 SKEPPNINGSNUMMER                        
003800*                                 SHIPMENT NO                             
003900        05 RESP-TIFAKT       PIC 9(6).                                    
004000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004100*                                 INVOICING DATE   (YYMMDD)               
004200        05 RESP-TIBERANK     PIC 9(6).                                    
004300*                                 BERÄKNAD ANKOMSTDATUM                   
004400*                                 ESTIMATED RECEIVING DATE                
004500        05 RESP-DABERANK-PROP                                             
004600                             PIC 9(6).                                    
004700*                                 PROPOSED ETA FROM P44 AND PULS          
004800        05 RESP-DABERANK-DISCH                                            
004900                             PIC 9(6).                                    
005000*                                 DISCHARGED ETA DATUM                    
005100        05 RESP-FLMANETA     PIC X.                                       
005200*                                 MANUALLY UPDATE ETA FLAG (Y/N)          
005300        05 RESP-IDLBBET      PIC X(12).                                   
005400*                                 LASTBÄRARBETECKNING                     
005500*                                 TRAILER NUMBER                          
005600        05 RESP-KDTRPSTA     PIC X.                                       
005700*                                 TRANSPORTSTATUS                         
005800*                                 TRANSPORT STATUS                        
005900        05 RESP-ADINLOMR     PIC X(4).                                    
006000*                                 INLEVERANSOMRÅDE                        
006100*                                 RECEIVING AREA                          
006200        05 RESP-KVKOLLI-FAKT PIC Z(3)9.                                   
006300*                                 ANTAL KOLLI                             
006400*                                 NBR OF CASES                            
006500        05 RESP-KVKOLLI-MOT  PIC Z(3)9.                                   
006600*                                 ANTAL KOLLI                             
006700*                                 NBR OF CASES                            
006800        05 RESP-KVRADER-FAKT PIC Z(4)9.                                   
006900*                                 ANTAL RADER PER FAKTURA                 
007000*                                 NUMBER OF LINES PER INVOICE             
007100        05 RESP-KVRADER-MOT  PIC Z(4)9.                                   
007200*                                 ANTAL MOTTAGNA  RADER                   
007300*                                 NUMBER OF LINES RECEIVED                
007400        05 RESP-KVRADER-PRIO PIC Z(4)9.                                   
007500*                                 ANTAL PRIORITERADE RADER                
007600*                                 NUMBER OF PRIORITY LINES                
007700        05 RESP-IDMSG-ERROR-LINE                                          
007800                             PIC X(3).                                    
007900*                                 FELMEDDELANDE ID                        
008000*                                 ERROR MESSAGE ID                        
008100        05 RESP-IDTRACK      PIC X(25).                                   
008200*                                 TRACKING ID FROM CUSTOMS                
008300*                                 CUSTOMS TRACKING ID                     
008400        05 RESP-FLOLD-IDTRACK                                             
008500                             PIC X.                                       
008600*                                 ALLMÄN FLAGGA                           
008700*                                 GENERAL FLAG                            
008800*** END OF VILMAII-COPY LENGTH= 57528 BYTES                               
