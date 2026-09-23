000100 01  RESP-WF0255O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0255         
000300*                                 PERIOD CALENDER LOCATE                  
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-BELEGRAD-1      PIC X(35).                                   
000800*                                 DEL AV LEGAL SELLER NAMN                
000900*                                 PART OF LEGAL SELLER NAME               
001000     03 RESP-KVRADER         PIC Z(4)9.                                   
001100*                                 ANTAL RADER                             
001200*                                 NUMBER OF LINES                         
001300     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001400*                                 GRUPP MED TABELLRADER                   
001500        05 RESP-TIRP-LINE    PIC 9(2).                                    
001600*                                 REDOVISNINGSPERIOD                      
001700*                                 12 PER ≈R                               
001800*                                 ACCOUNTING PERIOD                       
001900*                                 12 PER YEAR                             
002000        05 RESP-DASTADAT-LINE                                             
002100                             PIC Z(8).                                    
002200*                                 GENERELLT STARTDATUM                    
002300*                                 GENERAL START DATE                      
002400        05 RESP-DAFINDOC-LINE                                             
002500                             PIC Z(8).                                    
002600*                                 DOKUMENT DATUM (≈≈≈≈MMDD)               
002700*                                 INVOICING DATE   (YYYYMMDD)             
002800        05 RESP-FLPERIOD-LINE                                             
002900                             PIC X.                                       
003000*                                 ANGER OM PERIODK÷RNING K÷RD F÷R         
003100*                                  AKTUELL PERIOD                         
003200*                                 STATES THAT PERIOD-EXECUTION IS         
003300*                                  PERFORMED                              
003400        05 RESP-DAREGDAT-LINE                                             
003500                             PIC Z(8).                                    
003600*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003700*                                 REGISTRATION DATE (YYYYMMDD)            
003800        05 RESP-DAUPPDAT-LINE                                             
003900                             PIC Z(8).                                    
004000*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
004100*                                                                         
004200*                                 UPDATING DATE     (YYYYMMDD)            
004300*                                                                         
004400        05 RESP-IDUSER-LINE  PIC X(8).                                    
004500*                                 ANVƒNDARENS SƒKERHETS ID                
004600*                                 USER SECURITY-IDENTITY                  
004700*** END OF VILMAII-COPY LENGTH= 21544 BYTES                               
