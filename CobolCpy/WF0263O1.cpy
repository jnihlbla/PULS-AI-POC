000100 01  RESP-WF0263O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0263         
000300*                                 DOCUMENT TYPE LOCATE                    
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 RESP-BELEGRAD-1      PIC X(35).                                   
001100*                                 DEL AV LEGAL SELLER NAMN                
001200*                                 PART OF LEGAL SELLER NAME               
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500*                                 NUMBER OF LINES                         
001600     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001700*                                 GRUPP MED TABELLRADER                   
001800        05 RESP-KDFINDOC-LINE                                             
001900                             PIC X(4).                                    
002000*                                 TYP FINANSIELLT DOKUMENT                
002100*                                 FINANCIAL DOCUMENT TYPE                 
002200        05 RESP-BEFINDOC-LINE                                             
002300                             PIC X(35).                                   
002400*                                 FINANSIELLT DOKUMENT                    
002500*                                 FINANCIAL DOCUMENT                      
002600        05 RESP-DAREGDAT-LINE                                             
002700                             PIC Z(8).                                    
002800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002900*                                 REGISTRATION DATE (YYYYMMDD)            
003000        05 RESP-DAUPPDAT-LINE                                             
003100                             PIC Z(8).                                    
003200*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
003300*                                                                         
003400*                                 UPDATING DATE     (YYYYMMDD)            
003500*                                                                         
003600        05 RESP-IDUSER-LINE  PIC X(8).                                    
003700*                                 ANVÄNDARENS SÄKERHETS ID                
003800*                                 USER SECURITY-IDENTITY                  
003900        05 RESP-FLCOMING-LINE                                             
004000                             PIC X.                                       
004100*                                 ALLMÄN FLAGGA                           
004200*                                 GENERAL FLAG                            
004300*** END OF VILMAII-COPY LENGTH= 32048 BYTES                               
