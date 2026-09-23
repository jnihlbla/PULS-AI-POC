000100 01  RESP-WF0265O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0265         
000300*                                 SENDING COUNTRY LOCATE                  
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLANDX3-KEY    PIC X(3).                                    
000800*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 3-LETTER CODE FOR COUNTRY.              
001000     03 RESP-BELEGRAD-1      PIC X(35).                                   
001100*                                 DEL AV LEGAL SELLER NAMN                
001200*                                 PART OF LEGAL SELLER NAME               
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500*                                 NUMBER OF LINES                         
001600     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001700*                                 GRUPP MED TABELLRADER                   
001800        05 RESP-IDLANDX3-LINE                                             
001900                             PIC X(3).                                    
002000*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
002100*                                 3-LETTER CODE FOR COUNTRY.              
002200        05 RESP-BELAND-LINE  PIC X(35).                                   
002300*                                 LANDSBETECKNING                         
002400*                                 NAME OF COUNTRY                         
002500        05 RESP-DAREGDAT-LINE                                             
002600                             PIC Z(8).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002800*                                 REGISTRATION DATE (YYYYMMDD)            
002900        05 RESP-DAUPPDAT-LINE                                             
003000                             PIC Z(8).                                    
003100*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
003200*                                                                         
003300*                                 UPDATING DATE     (YYYYMMDD)            
003400*                                                                         
003500        05 RESP-IDUSER-LINE  PIC X(8).                                    
003600*                                 ANVÄNDARENS SÄKERHETS ID                
003700*                                 USER SECURITY-IDENTITY                  
003800        05 RESP-FLCOMING-LINE                                             
003900                             PIC X.                                       
004000*                                 ALLMÄN FLAGGA                           
004100*                                 GENERAL FLAG                            
004200*** END OF VILMAII-COPY LENGTH= 31547 BYTES                               
