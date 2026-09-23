000100 01  RESP-WF0286O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0286         
000300*                                 DOCUMENT NUMBER SERIE ASSIGNMEN         
000400*                                 T, LOCATE                               
000500     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 RESP-KDFINDOC-KEY    PIC X(4).                                    
000900*                                 TYP FINANSIELLT DOKUMENT                
001000*                                 FINANCIAL DOCUMENT TYPE                 
001100     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
001200*                                 TYP AV BETALARE                         
001300*                                 TYPE OF FIN.CUSTOMER                    
001400     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001500*                                 GRUPP AV BETALARE                       
001600*                                 FIN.CUSTOMER GROUP                      
001700     03 RESP-BELEGRAD-1      PIC X(35).                                   
001800*                                 DEL AV LEGAL SELLER NAMN                
001900*                                 PART OF LEGAL SELLER NAME               
002000     03 RESP-KVRADER         PIC Z(4)9.                                   
002100*                                 ANTAL RADER                             
002200*                                 NUMBER OF LINES                         
002300     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002400*                                 GRUPP MED TABELLRADER                   
002500        05 RESP-KDFINDOC-LINE                                             
002600                             PIC X(4).                                    
002700*                                 TYP FINANSIELLT DOKUMENT                
002800*                                 FINANCIAL DOCUMENT TYPE                 
002900        05 RESP-KDPARTTY-LINE                                             
003000                             PIC X(3).                                    
003100*                                 TYP AV BETALARE                         
003200*                                 TYPE OF FIN.CUSTOMER                    
003300        05 RESP-KDPARTGR-LINE                                             
003400                             PIC X(15).                                   
003500*                                 GRUPP AV BETALARE                       
003600*                                 FIN.CUSTOMER GROUP                      
003700        05 RESP-IDLANDX3-LINE                                             
003800                             PIC X(3).                                    
003900*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
004000*                                 3-LETTER CODE FOR COUNTRY.              
004100        05 RESP-IDLOPNR-LINE PIC 9(3).                                    
004200*                                 LÖPNUMMER                               
004300*                                 SEQUENCE NUMBER                         
004400        05 RESP-DAREGDAT-LINE                                             
004500                             PIC Z(8).                                    
004600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004700*                                 REGISTRATION DATE (YYYYMMDD)            
004800        05 RESP-IDUSER-LINE  PIC X(8).                                    
004900*                                 ANVÄNDARENS SÄKERHETS ID                
005000*                                 USER SECURITY-IDENTITY                  
005100*** END OF VILMAII-COPY LENGTH= 22066 BYTES                               
