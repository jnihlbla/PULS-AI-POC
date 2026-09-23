000100 01  RESP-WF0284O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0284         
000300*                                 DOCUMENT NUMBER SERIE LOCATE            
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLOPNR-KEY     PIC 9(3).                                    
000800*                                 LÖPNUMMER                               
000900*                                 SEQUENCE NUMBER                         
001000     03 RESP-BETEXT-KEY      PIC X(10).                                   
001100     03 RESP-BELEGRAD-1      PIC X(35).                                   
001200*                                 DEL AV LEGAL SELLER NAMN                
001300*                                 PART OF LEGAL SELLER NAME               
001400     03 RESP-KVRADER         PIC Z(4)9.                                   
001500*                                 ANTAL RADER                             
001600*                                 NUMBER OF LINES                         
001700     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001800*                                 GROUP MED TABELLRADER                   
001900        05 RESP-IDLOPNR-LINE PIC 9(3).                                    
002000*                                 LÖPNUMMER                               
002100*                                 SEQUENCE NUMBER                         
002200        05 RESP-BETEXT-LINE  PIC X(55).                                   
002300        05 RESP-IDFINDOC-START-LINE                                       
002400                             PIC Z(8)9.                                   
002500*                                 FINANSIELLT DOKUMENT ID                 
002600*                                 FINANCIAL DOCUMENT ID                   
002700        05 RESP-IDFINDOC-NEXT-LINE                                        
002800                             PIC Z(8)9.                                   
002900*                                 FINANSIELLT DOKUMENT ID                 
003000*                                 FINANCIAL DOCUMENT ID                   
003100        05 RESP-IDFINDOC-STOP-LINE                                        
003200                             PIC Z(8)9.                                   
003300*                                 FINANSIELLT DOKUMENT ID                 
003400*                                 FINANCIAL DOCUMENT ID                   
003500        05 RESP-IDUSER-LINE  PIC X(8).                                    
003600*                                 ANVÄNDARENS SÄKERHETS ID                
003700*                                 USER SECURITY-IDENTITY                  
003800*** END OF VILMAII-COPY LENGTH= 46557 BYTES                               
