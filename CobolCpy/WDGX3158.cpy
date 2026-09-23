000100 01  3158-WDGX3158.                                                       
000200*                                 STYRREGISTER BYTES KUNDER               
000300*                                 FYSISK NYCKEL:                          
000400*                                 IDDISTR + KDEXCHA                       
000500     03 3158-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700*                                 DISTRICT NUMBER                         
000800     03 3158-KDEXCHA         PIC S9(3)           COMP-3.                  
000900*                                 EXCHANGE ACCOUNT CODE                   
001000     03 3158-DAREGDAT        PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001200*                                 REGISTRATION DATE (YYYYMMDD)            
001300     03 3158-EXCLUDES        OCCURS 16 TIMES.                             
001400        05 3158-IDFKNGRP-FOM PIC S9(5)           COMP-3.                  
001500*                                 FUNKTIONSGRUPP-FROM                     
001600*                                 FUNCTION-GROUP FROM                     
001700        05 3158-IDFKNGRP-TOM PIC S9(5)           COMP-3.                  
001800*                                 FUNKTIONSGRUPP-TOM                      
001900*                                 FUNCTION-GROUP UP TO                    
002000     03 3158-FLEXCDET        PIC X.                                       
002100*                                 EXCHANGE REPORT FLAG DET                
002200     03 3158-FLEXCREP        PIC X.                                       
002300*                                 EXCHANGE REPORT FLAG                    
002400     03 3158-FLEXCRET        PIC X.                                       
002500*                                 EXCHANGE RETURN FLAG                    
002600     03 3158-FLFAKT          PIC X.                                       
002700*                                 FAKTURERINGSFLAGGA                      
002800     03 3158-FLRETREM        PIC X.                                       
002900*                                 EXCHANGE REMIND FLAG                    
003000     03 3158-IDDISTR-BET     PIC S9(5)           COMP-3.                  
003100*                                 BATALANDE DISTRIKT                      
003200*                                 DISTRICT TO CHARGE                      
003300     03 3158-IDMAIL          PIC X(60).                                   
003400*                                 MAIL ADRESS                             
003500*                                 MAIL ADRESS                             
003600     03 3158-IDUSER          PIC X(8).                                    
003700*                                 ANVÄNDARENS SÄKERHETS ID                
003800*                                 USER SECURITY-IDENTITY                  
003900     03 3158-SUPOINT-BAL     PIC S9(9)           COMP-3.                  
004000*                                 POINT VALUE                             
004100     03 3158-SUPOINT-RIT     PIC S9(9)           COMP-3.                  
004200*                                 POINT VALUE                             
004300     03 3158-SUPOINT-PP      PIC S9(9)           COMP-3.                  
004400*                                 PENDING VALUE                           
004500     03 3158-KVVECKOR-BYFA   PIC S9(3)           COMP-3.                  
004600*                                 ANTAL VECKOR FÖR BYTESFAKTURA           
004700*                                 NUMBER OF WEEKS ECH INVOICE             
004800     03 3158-KVVECKOR-BYRE   PIC S9(3)           COMP-3.                  
004900*                                 ANTAL VECKOR INNAN RENSING              
005000*                                 NUMBER OF WEEKS BEFORE DELETE           
005100     03 3158-TIKLOCK         PIC S9(9)           COMP-3.                  
005200*                                 KLOCKSLAG (TTMMSSTH)                    
005300*                                 TIME OF DAY (HHMMSSTH)                  
005400     03 3158-FILLER          PIC X(24).                                   
005500*** END OF VILMAII-COPY LENGTH= 233 BYTES                                 
