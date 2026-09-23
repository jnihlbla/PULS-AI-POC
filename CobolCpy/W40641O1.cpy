000100 01  RESP-W40641O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM W4W641         
000300*                                 OUTBOUND CHANGE IDLBBET                 
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-TIFAKT-KEY      PIC X(6).                                    
000800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
000900*                                 INVOICING DATE   (YYMMDD)               
001000     03 RESP-IDLBBET-KEY     PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200*                                 TRAILER NUMBER                          
001300     03 RESP-IDDC-REC-KEY    PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 RESP-KVRADER         PIC Z(4)9.                                   
001700*                                 ANTAL RADER                             
001800*                                 NUMBER OF LINES                         
001900     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002000*                                 GRUPP MED TABELLRADER                   
002100        05 RESP-IDFAKT       PIC Z(6)9.                                   
002200*                                 FAKTURANUMMER                           
002300*                                 INVOICE NO.                             
002400        05 RESP-IDSHIPM      PIC Z(7).                                    
002500*                                 SKEPPNINGSNUMMER                        
002600*                                 SHIPMENT NO                             
002700        05 RESP-TIFAKT       PIC 9(6).                                    
002800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002900*                                 INVOICING DATE   (YYMMDD)               
003000        05 RESP-TIBERANK     PIC 9(6).                                    
003100*                                 BERÄKNAD ANKOMSTDATUM                   
003200*                                 ESTIMATED RECEIVING DATE                
003300        05 RESP-IDLBBET      PIC X(12).                                   
003400*                                 LASTBÄRARBETECKNING                     
003500*                                 TRAILER NUMBER                          
003600        05 RESP-KVKOLLI-FAKT PIC Z(3)9.                                   
003700*                                 ANTAL KOLLI                             
003800*                                 NBR OF CASES                            
003900        05 RESP-KVRADER-FAKT PIC Z(4)9.                                   
004000*                                 ANTAL RADER PER FAKTURA                 
004100*                                 NUMBER OF LINES PER INVOICE             
004200        05 RESP-IDMSG-ERROR-LINE                                          
004300                             PIC X(3).                                    
004400*                                 FELMEDDELANDE ID                        
004500*                                 ERROR MESSAGE ID                        
004600*** END OF VILMAII-COPY LENGTH= 25027 BYTES                               
