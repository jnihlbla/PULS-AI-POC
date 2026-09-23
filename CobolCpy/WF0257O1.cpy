000100 01  RESP-WF0257O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0257         
000300*                                 PAYMENT TERM LOCATE                     
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDSPRAK-KEY     PIC X(2).                                    
000800*                                 2-STÄLLIG ISO SPRÅKKOD                  
000900*                                 2-LETTER ISO LANGUAGE CODE              
001000     03 RESP-KDBETALV-KEY    PIC X(4).                                    
001100*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
001200*                                 TERMS OF PAYMENT                        
001300     03 RESP-FLPREL-KEY      PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500*                                 GENERAL FLAG                            
001600     03 RESP-BELEGRAD-1      PIC X(35).                                   
001700*                                 DEL AV LEGAL SELLER NAMN                
001800*                                 PART OF LEGAL SELLER NAME               
001900     03 RESP-KVRADER         PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100*                                 NUMBER OF LINES                         
002200     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002300*                                 GRUPP MED TABELLRADER                   
002400        05 RESP-IDSPRAK-LINE PIC X(2).                                    
002500*                                 2-STÄLLIG ISO SPRÅKKOD                  
002600*                                 2-LETTER ISO LANGUAGE CODE              
002700        05 RESP-KDBETALV-LINE                                             
002800                             PIC X(4).                                    
002900*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
003000*                                 TERMS OF PAYMENT                        
003100        05 RESP-BEBETVIL-LINE                                             
003200                             PIC X(30).                                   
003300*                                 BETALNINGSVILLKORSTEXT                  
003400*                                 TERMS OF PAYMENT TEXT                   
003500        05 RESP-DAREGDAT-LINE                                             
003600                             PIC Z(8).                                    
003700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003800*                                 REGISTRATION DATE (YYYYMMDD)            
003900        05 RESP-DAUPPDAT-LINE                                             
004000                             PIC Z(8).                                    
004100*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
004200*                                                                         
004300*                                 UPDATING DATE     (YYYYMMDD)            
004400*                                                                         
004500        05 RESP-IDUSER-LINE  PIC X(8).                                    
004600*                                 ANVÄNDARENS SÄKERHETS ID                
004700*                                 USER SECURITY-IDENTITY                  
004800*** END OF VILMAII-COPY LENGTH= 30051 BYTES                               
