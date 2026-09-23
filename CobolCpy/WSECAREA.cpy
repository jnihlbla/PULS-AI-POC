000100 01  SEC-WSECAREA.                                                        
000200*                                 PARAMETRAR TILL WSECURIT FÖR            
000300*                                 TEST OM USER HAR RÄTT ATT SE            
000400*                                 DENNA BILD MED DENNA NYCKEL.            
000500*                                 EXEMPEL PÅ ANROP:                       
000600*                                 MOVE MSG-SIGNON-USERID TO               
000700*                                                 SEC-IDUSER              
000800*                                 MOVE "4211" TO  SEC-IDTRANS             
000900*                                 MOVE MID-IDDISTR TO SEC-IDKEY           
001000*                                 CALL WSECURIT USING                     
001100*                                      SEC-IDUSER                         
001200*                                      SEC-IDTRANS                        
001300*                                      SEC-IDKEY                          
001400*                                      SEC-KDSVAR                         
001500*                                                                         
001600*                                 RESULTAT ERHÅLLS I SEC-KDSVAR           
001700*                                     = OK VISA ALLT                      
001800*                                  F  = FEL (OBEHÖRIG)                    
001900*                                  1  = IMPORTÖR SOM FÅR KÖRA VOR         
002000*                                  2  = GMT SOM EJ FÅR KÖRA VOR           
002100*                                  3  = PV/LV (INTERNDISTRIKT)            
002200*                                  4  = VCBV OCH USA FÖR 1101             
002300*                                  5  = IMPORTÖR FÅR EJ KÖRA VOR          
002400*                                  6  = GMT SOM FÅR KÖRA VOR              
002500*                                 -------------------------------         
002600*                                 PARAMETERS TO WSECURIT FOR              
002700*                                 TEST IF USER HAS AUTHORITY TO           
002800*                                 USE THIS KEY                            
002900*                                 FORMAT.                                 
003000*                                 EXAMPEL:                                
003100*                                 MOVE MSG-SIGNON-USERID TO               
003200*                                                 SEC-IDUSER              
003300*                                 MOVE "4211" TO  SEC-IDTRANS             
003400*                                 MOVE MID-IDDISTR TO SEC-IDKEY           
003500*                                 CALL WSECURIT USING                     
003600*                                      SEC-IDUSER                         
003700*                                      SEC-IDTRANS                        
003800*                                      SEC-IDKEY                          
003900*                                      SEC-KDSVAR                         
004000*                                                                         
004100*                                 THE RESULTS IN SEC-KDSVAR IS            
004200*                                 SPACE = OK                              
004300*                                  F    = ERROR                           
004400*                                  1    = IMPORTER VOR                    
004500*                                  2    = DEALER NO VOR                   
004600*                                  3    = VOLVO CAR                       
004700*                                  4    = VCBV + USA FOR 1101             
004800*                                  5    = IMPORTER NO VOR                 
004900*                                  6    = DEALER VOR                      
005000*                                 -------------------------------         
005100     03 SEC-IDUSER           PIC X(8).                                    
005200*                                 ANVÄNDARENS SÄKERHETS ID                
005300*                                 USER SECURITY-IDENTITY                  
005400     03 SEC-IDTRANS          PIC X(4).                                    
005500*                                 BILDNUMMER                              
005600*                                 SCREEN NUMBER                           
005700     03 SEC-IDKEY            PIC X(20).                                   
005800*                                 SECURITY-NYCKEL                         
005900*                                 SECURITY-KEY                            
006000     03 SEC-KDSVAR           PIC X.                                       
006100*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
006200*                                 RETURN CODE FROM PROGRAM                
006300*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
