000100 01  RESP-WF0277O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0277         
000300*                                 ERRONEUS BUNDLES LOCATE                 
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDSYSTEM-SEND   PIC X(4).                                    
000800*                                 VOLVO SƒNDANDE SYSTEM                   
000900*                                 VOLVO SENDING SYSTEM                    
001000     03 RESP-FLASC           PIC X.                                       
001100*                                 ALLMƒN FLAGGA                           
001200*                                 GENERAL FLAG                            
001300     03 RESP-DADATUM         PIC X(8).                                    
001400*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001500*                                 REGISTRATION DATE (YYYYMMDD)            
001600     03 RESP-TIHHMMSS        PIC 9(6).                                    
001700*                                 TIM - MIN - SEK   (HHMMSS)              
001800*                                 HOUR - MINUTE - SEC (HHMMSS)            
001900     03 RESP-BELEGRAD-1      PIC X(35).                                   
002000*                                 DEL AV LEGAL SELLER NAMN                
002100*                                 PART OF LEGAL SELLER NAME               
002200     03 RESP-KVRADER         PIC Z(4)9.                                   
002300*                                 ANTAL RADER                             
002400*                                 NUMBER OF LINES                         
002500     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002600*                                 GRUPP MED TABELLRADER                   
002700        05 RESP-IDBUNDLE-LINE                                             
002800                             PIC X(15).                                   
002900*                                 BUNDLE ID                               
003000*                                 BUNDLE ID                               
003100        05 RESP-DAREGDAT-LINE                                             
003200                             PIC X(8).                                    
003300*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003400*                                 REGISTRATION DATE (YYYYMMDD)            
003500        05 RESP-TIREGTID-LINE                                             
003600                             PIC 9(10).                                   
003700*                                 REGISTRERINGSTID                        
003800*                                 GENERAL REGISTRATION TIME               
003900        05 RESP-IDREF-LINE   PIC X(15).                                   
004000*                                 REFERENS ID                             
004100*                                 REFERENCE ID                            
004200        05 RESP-DAREFDAT-LINE                                             
004300                             PIC X(8).                                    
004400*                                 REFERENSDATUM (≈≈≈≈MMDD)                
004500*                                 REFERENCE DATE(YYYYMMDD)                
004600        05 RESP-IDREFRAD-LINE                                             
004700                             PIC 9(5).                                    
004800*                                 REFERENSRADSNR                          
004900*                                 REFERENCE LINE NUMBER                   
005000        05 RESP-ANTIBUNT-LINE                                             
005100                             PIC Z(4)9.                                   
005200*                                 ANTAL RADER                             
005300*                                 NUMBER OF LINES                         
005400        05 RESP-FELIBUNT-LINE                                             
005500                             PIC Z(4)9.                                   
005600*                                 ANTAL RADER                             
005700*                                 NUMBER OF LINES                         
005800        05 RESP-IDSYSTEM-SEND-LINE                                        
005900                             PIC X(4).                                    
006000*                                 VOLVO SƒNDANDE SYSTEM                   
006100*                                 VOLVO SENDING SYSTEM                    
006200*** END OF VILMAII-COPY LENGTH= 37563 BYTES                               
