000100 01  RESP-WF0271O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0271         
000300*                                 INTERSTATE RULES LOCATE                 
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLANDX3-SEND-KEY                                            
000800                             PIC X(3).                                    
000900*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001000*                                 3-LETTER CODE FOR COUNTRY.              
001100     03 RESP-IDLANDX3-REC-KEY                                             
001200                             PIC X(3).                                    
001300*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001400*                                 3-LETTER CODE FOR COUNTRY.              
001500     03 RESP-BELEGRAD-1      PIC X(35).                                   
001600*                                 DEL AV LEGAL SELLER NAMN                
001700*                                 PART OF LEGAL SELLER NAME               
001800     03 RESP-KVRADER         PIC Z(4)9.                                   
001900*                                 ANTAL RADER                             
002000*                                 NUMBER OF LINES                         
002100     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002200*                                 GRUPP MED TABELLRADER                   
002300        05 RESP-IDLANDX3-SEND-LINE                                        
002400                             PIC X(3).                                    
002500*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
002600*                                 3-LETTER CODE FOR COUNTRY.              
002700        05 RESP-BELAND-SEND-LINE                                          
002800                             PIC X(35).                                   
002900*                                 LANDSBETECKNING                         
003000*                                 NAME OF COUNTRY                         
003100        05 RESP-IDLANDX3-REC-LINE                                         
003200                             PIC X(3).                                    
003300*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
003400*                                 3-LETTER CODE FOR COUNTRY.              
003500        05 RESP-BELAND-REC-LINE                                           
003600                             PIC X(35).                                   
003700*                                 LANDSBETECKNING                         
003800*                                 NAME OF COUNTRY                         
003900        05 RESP-DAREGDAT-LINE                                             
004000                             PIC Z(8).                                    
004100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004200*                                 REGISTRATION DATE (YYYYMMDD)            
004300        05 RESP-DAUPPDAT-LINE                                             
004400                             PIC Z(8).                                    
004500*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
004600*                                                                         
004700*                                 UPDATING DATE     (YYYYMMDD)            
004800*                                                                         
004900        05 RESP-IDUSER-LINE  PIC X(8).                                    
005000*                                 ANVÄNDARENS SÄKERHETS ID                
005100*                                 USER SECURITY-IDENTITY                  
005200        05 RESP-FLCOMING-LINE                                             
005300                             PIC X.                                       
005400*                                 ALLMÄN FLAGGA                           
005500*                                 GENERAL FLAG                            
005600*** END OF VILMAII-COPY LENGTH= 50550 BYTES                               
