000100 01  RESP-WF0273O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0273         
000300*                                 BUSINESS RELATIONS LOCATE               
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
001100*                                 TYP AV BETALARE                         
001200*                                 TYPE OF FIN.CUSTOMER                    
001300     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001400*                                 GRUPP AV BETALARE                       
001500*                                 FIN.CUSTOMER GROUP                      
001600     03 RESP-BELEGRAD-1      PIC X(35).                                   
001700*                                 DEL AV LEGAL SELLER NAMN                
001800*                                 PART OF LEGAL SELLER NAME               
001900     03 RESP-KVRADER         PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100*                                 NUMBER OF LINES                         
002200     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002300*                                 GRUPP MED TABELLRADER                   
002400        05 RESP-KDFINDOC-LINE                                             
002500                             PIC X(4).                                    
002600*                                 TYP FINANSIELLT DOKUMENT                
002700*                                 FINANCIAL DOCUMENT TYPE                 
002800        05 RESP-KDPARTTY-LINE                                             
002900                             PIC X(3).                                    
003000*                                 TYP AV BETALARE                         
003100*                                 TYPE OF FIN.CUSTOMER                    
003200        05 RESP-KDPARTGR-LINE                                             
003300                             PIC X(15).                                   
003400*                                 GRUPP AV BETALARE                       
003500*                                 FIN.CUSTOMER GROUP                      
003600        05 RESP-DAREGDAT-LINE                                             
003700                             PIC Z(8).                                    
003800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003900*                                 REGISTRATION DATE (YYYYMMDD)            
004000        05 RESP-DAUPPDAT-LINE                                             
004100                             PIC Z(8).                                    
004200*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
004300*                                                                         
004400*                                 UPDATING DATE     (YYYYMMDD)            
004500*                                                                         
004600        05 RESP-IDUSER-LINE  PIC X(8).                                    
004700*                                 ANVÄNDARENS SÄKERHETS ID                
004800*                                 USER SECURITY-IDENTITY                  
004900        05 RESP-FLCOMING-LINE                                             
005000                             PIC X.                                       
005100*                                 ALLMÄN FLAGGA                           
005200*                                 GENERAL FLAG                            
005300*** END OF VILMAII-COPY LENGTH= 23566 BYTES                               
