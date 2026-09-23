000100 01  RESP-WF0287O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0287         
000300*                                 DOCUMENT NUMBER SERIE ASSIGNMEN         
000400*                                 T                                       
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
001700     03 RESP-IDLANDX3-KEY    PIC X(3).                                    
001800*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001900*                                 3-LETTER CODE FOR COUNTRY.              
002000     03 RESP-IDLOPNR-KEY     PIC 9(3).                                    
002100*                                 LÖPNUMMER                               
002200*                                 SEQUENCE NUMBER                         
002300     03 RESP-BELEGRAD-1      PIC X(35).                                   
002400*                                 DEL AV LEGAL SELLER NAMN                
002500*                                 PART OF LEGAL SELLER NAME               
002600     03 RESP-DAREGDAT        PIC Z(8).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002800*                                 REGISTRATION DATE (YYYYMMDD)            
002900     03 RESP-DADELDAT        PIC Z(8).                                    
003000*                                 BORTTAGSDATUM      (ÅÅÅÅMMDD)           
003100*                                 DELETION DATE     (YYYYMMDD)            
003200     03 RESP-IDUSER          PIC X(8).                                    
003300*                                 ANVÄNDARENS SÄKERHETS ID                
003400*                                 USER SECURITY-IDENTITY                  
003500*** END OF VILMAII-COPY LENGTH= 91 BYTES                                  
