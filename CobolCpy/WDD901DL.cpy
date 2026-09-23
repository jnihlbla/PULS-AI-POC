000100 01  WDD901DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 PART INFORMATION WITH DC                
000400*                                                                         
000500     03 IDSEGM               PIC X(6).                                    
000600*                                 SEGMENT                                 
000700     03 FILLERX2             PIC X(2).                                    
000800     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC-KEY             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 FILLERX31            PIC X(31).                                   
001300     03 WDD901-CTX.                                                       
001400*                                 LEVERANSPLANEREGISTER                   
001500*                                 ARTIKELINFORMATION                      
001600*                                 FYSISK NYCKEL: WDD901KY                 
001700*                                 (IDARTNR + IDDC)                        
001800        05 IDARTNR           PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000        05 IDDC              PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  
