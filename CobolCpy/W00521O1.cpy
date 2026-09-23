000100 01  RESP-W00521O1.                                                       
000200*                                 RESP WEB RESPONSE TIME LOG              
000300     03 RESP-IDDC-START      PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDDC-NEXT       PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 RESP-TIREGDAT-START  PIC 9(6).                                    
001000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001100*                                 REGISTRATION DATE (YYMMDD)              
001200     03 RESP-TIREGDAT-NEXT   PIC 9(6).                                    
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400*                                 REGISTRATION DATE (YYMMDD)              
001500     03 RESP-TIREGTID-START  PIC 9(6).                                    
001600*                                 REGISTRERINGSTID                        
001700*                                 GENERAL REGISTRATION TIME               
001800     03 RESP-TIREGTID-NEXT   PIC 9(6).                                    
001900*                                 REGISTRERINGSTID                        
002000*                                 GENERAL REGISTRATION TIME               
002100     03 RESP-IDUSER-START    PIC X(8).                                    
002200*                                 ANVÄNDARENS SÄKERHETS ID                
002300*                                 USER SECURITY-IDENTITY                  
002400     03 RESP-IDUSER-NEXT     PIC X(8).                                    
002500*                                 ANVÄNDARENS SÄKERHETS ID                
002600*                                 USER SECURITY-IDENTITY                  
002700     03 RESP-KVMILSEC-START  PIC 9(6).                                    
002800*                                 ANTAL MILLISEK FÖR WEBSVAR              
002900*                                 NO OF MILLISEC FOR WEB RESPONDE         
003000     03 RESP-KVMILSEC-NEXT   PIC 9(6).                                    
003100*                                 ANTAL MILLISEK FÖR WEBSVAR              
003200*                                 NO OF MILLISEC FOR WEB RESPONDE         
003300     03 RESP-KVRADER         PIC 9(5).                                    
003400*                                 ANTAL RADER                             
003500*                                 NUMBER OF LINES                         
003600     03 RESP-KVMILSEC-AVG    PIC Z(5)9.                                   
003700*                                 ANTAL MILLISEK FÖR WEBSVAR              
003800*                                 NO OF MILLISEC FOR WEB RESPONDE         
003900     03 RESP-WEBRESPTIME     OCCURS 500 TIMES.                            
004000        05 RESP-IDDC-LINE    PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200*                                 WAREHOUSE IDENTIFIER                    
004300        05 RESP-TIREGDAT-LINE                                             
004400                             PIC 9(6).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600*                                 REGISTRATION DATE (YYMMDD)              
004700        05 RESP-TIREGTID-LINE                                             
004800                             PIC 9(6).                                    
004900*                                 REGISTRERINGSTID                        
005000*                                 GENERAL REGISTRATION TIME               
005100        05 RESP-IDUSER-LINE  PIC X(8).                                    
005200*                                 ANVÄNDARENS SÄKERHETS ID                
005300*                                 USER SECURITY-IDENTITY                  
005400        05 RESP-KVMILSEC-LINE                                             
005500                             PIC Z(5)9.                                   
005600*                                 ANTAL MILLISEK FÖR WEBSVAR              
005700*                                 NO OF MILLISEC FOR WEB RESPONDE         
005800        05 RESP-BEWEBSCR-LINE                                             
005900                             PIC X(50).                                   
006000*                                 VALD BILD I PULS WEBBEN                 
006100*                                 CHOSEN SCREEN ON PULS WEB               
006200        05 RESP-BEWEBURL-LINE                                             
006300                             PIC X(50).                                   
006400*                                 LÄNK PULS WEB SIDA                      
006500*                                 LINK PULS WEBSITE                       
006600        05 RESP-IDLOPNR-LINE PIC Z9.                                      
006700*                                 LÖPNUMMER                               
006800*                                 SEQUENCE NUMBER                         
006900*** END OF VILMAII-COPY LENGTH= 65067 BYTES                               
