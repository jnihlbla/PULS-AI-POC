000100 01  RESP-WF0261O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0261         
000300*                                 CUSTOMER GROUP LOCATE                   
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
000800*                                 TYP AV BETALARE                         
000900*                                 TYPE OF FIN.CUSTOMER                    
001000     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001100*                                 GRUPP AV BETALARE                       
001200*                                 FIN.CUSTOMER GROUP                      
001300     03 RESP-BELEGRAD-1      PIC X(35).                                   
001400*                                 DEL AV LEGAL SELLER NAMN                
001500*                                 PART OF LEGAL SELLER NAME               
001600     03 RESP-KVRADER         PIC Z(4)9.                                   
001700*                                 ANTAL RADER                             
001800*                                 NUMBER OF LINES                         
001900     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002000*                                 GRUPP MED TABELLRADER                   
002100        05 RESP-KDPARTTY-LINE                                             
002200                             PIC X(3).                                    
002300*                                 TYP AV BETALARE                         
002400*                                 TYPE OF FIN.CUSTOMER                    
002500        05 RESP-KDPARTGR-LINE                                             
002600                             PIC X(15).                                   
002700*                                 GRUPP AV BETALARE                       
002800*                                 FIN.CUSTOMER GROUP                      
002900        05 RESP-DAREGDAT-LINE                                             
003000                             PIC Z(8).                                    
003100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003200*                                 REGISTRATION DATE (YYYYMMDD)            
003300        05 RESP-DAUPPDAT-LINE                                             
003400                             PIC Z(8).                                    
003500*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003600*                                                                         
003700*                                 UPDATING DATE     (YYYYMMDD)            
003800*                                                                         
003900        05 RESP-DADELDAT-LINE                                             
004000                             PIC Z(8).                                    
004100*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
004200*                                 DELETION DATE     (YYYYMMDD)            
004300        05 RESP-IDUSER-LINE  PIC X(8).                                    
004400*                                 ANVƒNDARENS SƒKERHETS ID                
004500*                                 USER SECURITY-IDENTITY                  
004600        05 RESP-FLCOMING-LINE                                             
004700                             PIC X.                                       
004800*                                 ALLMƒN FLAGGA                           
004900*                                 GENERAL FLAG                            
005000*** END OF VILMAII-COPY LENGTH= 25562 BYTES                               
