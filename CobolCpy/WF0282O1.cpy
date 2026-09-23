000100 01  RESP-WF0272O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0272         
000300*                                 PAYMENT INSTRUCTIONS LOCATE             
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 RESP-KDVALISO-KEY    PIC X(3).                                    
001100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001200*                                 CURRENCY CODE BY ISO-STANDARD.          
001300     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
001400*                                 TYP AV BETALARE                         
001500*                                 TYPE OF FIN.CUSTOMER                    
001600     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001700*                                 GRUPP AV BETALARE                       
001800*                                 FIN.CUSTOMER GROUP                      
001900     03 RESP-BELEGRAD-1      PIC X(35).                                   
002000*                                 DEL AV LEGAL SELLER NAMN                
002100*                                 PART OF LEGAL SELLER NAME               
002200     03 RESP-KVRADER         PIC Z(4)9.                                   
002300*                                 ANTAL RADER                             
002400*                                 NUMBER OF LINES                         
002500     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002600*                                 GRUPP MED TABELLRADER                   
002700        05 RESP-KDFINDOC-LINE                                             
002800                             PIC X(4).                                    
002900*                                 TYP FINANSIELLT DOKUMENT                
003000*                                 FINANCIAL DOCUMENT TYPE                 
003100        05 RESP-KDVALISO-LINE                                             
003200                             PIC X(3).                                    
003300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003400*                                 CURRENCY CODE BY ISO-STANDARD.          
003500        05 RESP-KDPARTTY-LINE                                             
003600                             PIC X(3).                                    
003700*                                 TYP AV BETALARE                         
003800*                                 TYPE OF FIN.CUSTOMER                    
003900        05 RESP-KDPARTGR-LINE                                             
004000                             PIC X(15).                                   
004100*                                 GRUPP AV BETALARE                       
004200*                                 FIN.CUSTOMER GROUP                      
004300        05 RESP-DAREGDAT-LINE                                             
004400                             PIC Z(8).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004600*                                 REGISTRATION DATE (YYYYMMDD)            
004700        05 RESP-DAUPPDAT-LINE                                             
004800                             PIC Z(8).                                    
004900*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
005000*                                                                         
005100*                                 UPDATING DATE     (YYYYMMDD)            
005200*                                                                         
005300        05 RESP-IDUSER-LINE  PIC X(8).                                    
005400*                                 ANVÄNDARENS SÄKERHETS ID                
005500*                                 USER SECURITY-IDENTITY                  
005600        05 RESP-FLCOMING-LINE                                             
005700                             PIC X.                                       
005800*                                 ALLMÄN FLAGGA                           
005900*                                 GENERAL FLAG                            
006000*** END OF VILMAII-COPY LENGTH= 25069 BYTES                               
