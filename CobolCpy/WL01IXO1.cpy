000100 01  RESP-DCS-WL01IXO1.                                                   
000200*                                 RESPONSE COPYTXT PGM WL01IX             
000300*                                 DC DATA TO WEB                          
000400     03 RESP-DCS-IDLANDX2    PIC X(2).                                    
000500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000600*                                 2-LETTER CODE FOR COUNTRY               
000700     03 RESP-DCS-IDTIDZON    PIC 9(2).                                    
000800*                                 TIDZONER PÅ JORDEN.                     
000900*                                 TIME ZONE ON EARTH                      
001000     03 RESP-DCS-KDDC        PIC X(2).                                    
001100*                                 TYP AV DISTR. LAGER                     
001200*                                 TYPE OF DELIV. CENTER                   
001300     03 RESP-DCS-IDFTG       PIC 9(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500*                                 COMPANY IDENTITY ACCOUNTING             
001600     03 RESP-DCS-FLINVACS    PIC X.                                       
001700*                                 ACS-INVENTERING?                        
001800*                                 ACS INVENTORY?                          
001900*** END OF VILMAII-COPY LENGTH= 9 BYTES                                   
