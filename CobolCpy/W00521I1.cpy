000100 01  REQU-W00521I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W0052100              
000300*                                                                         
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-TIREGDAT-KEY    PIC X(6).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000900*                                 REGISTRATION DATE (YYMMDD)              
001000     03 REQU-TIREGTID-KEY    PIC X(6).                                    
001100*                                 REGISTRERINGSTID                        
001200*                                 GENERAL REGISTRATION TIME               
001300     03 REQU-IDUSER-KEY      PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500*                                 USER SECURITY-IDENTITY                  
001600     03 REQU-FLSORT-KEY      PIC X.                                       
001700*                                 SORTERINGSFLAGGA (J/N)                  
001800     03 REQU-BEWEBSCR-KEY    PIC X(50).                                   
001900*                                 VALD BILD I PULS WEBBEN                 
002000*                                 CHOSEN SCREEN ON PULS WEB               
002100     03 REQU-IDDC-START      PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300*                                 WAREHOUSE IDENTIFIER                    
002400     03 REQU-TIREGDAT-START  PIC 9(6).                                    
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600*                                 REGISTRATION DATE (YYMMDD)              
002700     03 REQU-TIREGTID-START  PIC 9(6).                                    
002800*                                 REGISTRERINGSTID                        
002900*                                 GENERAL REGISTRATION TIME               
003000     03 REQU-IDUSER-START    PIC X(8).                                    
003100*                                 ANVÄNDARENS SÄKERHETS ID                
003200*                                 USER SECURITY-IDENTITY                  
003300     03 REQU-KVMILSEC-START  PIC 9(6).                                    
003400*                                 ANTAL MILLISEK FÖR WEBSVAR              
003500*                                 NO OF MILLISEC FOR WEB RESPONDE         
003600     03 REQU-KVRADER         PIC 9(5).                                    
003700*                                 ANTAL RADER                             
003800*                                 NUMBER OF LINES                         
003900     03 REQU-PF4FLAG         PIC X.                                       
004000*                                 ALLMÄN FLAGGA                           
004100*                                 GENERAL FLAG                            
004200*** END OF VILMAII-COPY LENGTH= 107 BYTES                                 
