000100 01  MOD-W0O52101.                                                        
000200*                                 MOD WEB RESPONSE TIME LOG               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MOD-TIREGDAT-IN      PIC X(6).                                    
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400*                                 REGISTRATION DATE (YYMMDD)              
001500     03 MOD-TIREGTID-IN      PIC X(6).                                    
001600*                                 REGISTRERINGSTID                        
001700*                                 GENERAL REGISTRATION TIME               
001800     03 MOD-IDUSER-IN        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000*                                 USER SECURITY-IDENTITY                  
002100     03 MOD-FLSORT-IN        PIC X.                                       
002200*                                 SORTERINGSFLAGGA (J/N)                  
002300     03 MOD-BEWEBSCR-IN      PIC X(50).                                   
002400*                                 VALD BILD I PULS WEBBEN                 
002500*                                 CHOSEN SCREEN ON PULS WEB               
002600     03 MOD-IDDC-UT          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800*                                 WAREHOUSE IDENTIFIER                    
002900     03 MOD-TIREGDAT-UT      PIC X(6).                                    
003000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003100*                                 REGISTRATION DATE (YYMMDD)              
003200     03 MOD-TIREGTID-UT      PIC X(6).                                    
003300*                                 REGISTRERINGSTID                        
003400*                                 GENERAL REGISTRATION TIME               
003500     03 MOD-IDUSER-UT        PIC X(8).                                    
003600*                                 ANVÄNDARENS SÄKERHETS ID                
003700*                                 USER SECURITY-IDENTITY                  
003800     03 MOD-FLSORT-UT        PIC X.                                       
003900*                                 SORTERINGSFLAGGA (J/N)                  
004000     03 MOD-BEWEBSCR-UT      PIC X(50).                                   
004100*                                 VALD BILD I PULS WEBBEN                 
004200*                                 CHOSEN SCREEN ON PULS WEB               
004300     03 MOD-WEBRESPTIME      OCCURS 14 TIMES.                             
004400        05 MOD-IDDC          PIC X(2).                                    
004500*                                 IDENTIFIERARE LAGER                     
004600*                                 WAREHOUSE IDENTIFIER                    
004700        05 MOD-TIREGDAT      PIC 9(6).                                    
004800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004900*                                 REGISTRATION DATE (YYMMDD)              
005000        05 MOD-TIREGTID      PIC 9(6).                                    
005100*                                 REGISTRERINGSTID                        
005200*                                 GENERAL REGISTRATION TIME               
005300        05 MOD-IDUSER        PIC X(8).                                    
005400*                                 ANVÄNDARENS SÄKERHETS ID                
005500*                                 USER SECURITY-IDENTITY                  
005600        05 MOD-KVMILSEC      PIC Z(5)9.                                   
005700*                                 ANTAL MILLISEK FÖR WEBSVAR              
005800*                                 NO OF MILLISEC FOR WEB RESPONDE         
005900        05 MOD-BEWEBSCR      PIC X(50).                                   
006000*                                 VALD BILD I PULS WEBBEN                 
006100*                                 CHOSEN SCREEN ON PULS WEB               
006200        05 MOD-BEWEBURL      PIC X(50).                                   
006300*                                 LÄNK PULS WEB SIDA                      
006400*                                 LINK PULS WEBSITE                       
006500     03 MOD-KVMILSEC-AVG     PIC Z(5)9.                                   
006600*                                 ANTAL MILLISEK FÖR WEBSVAR              
006700*                                 NO OF MILLISEC FOR WEB RESPONDE         
006800     03 MOD-TEMFSINF         PIC X(55).                                   
006900*                                 INFORMATIONSMEDDELANDE                  
007000*                                 INFORMATION MESSAGE                     
007100*** END OF VILMAII-COPY LENGTH= 2043 BYTES                                
