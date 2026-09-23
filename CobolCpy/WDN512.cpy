000100 01  RAD-WDN512.                                                          
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 RADSEGMENT                              
000400*                                 FYSISK NYCKEL: WDN512KY                 
000500*                                 (IDCATRAD + KDCATPUB-FOM)               
000600     03 RAD-IDCATRAD         PIC 9(4).                                    
000700*                                 RADNUMMER                               
000800*                                 ROW NUMBER IN TEXT BLOCK                
000900     03 RAD-KDCATPUB-FOM     PIC X(6).                                    
001000*                                 PUBLICERINGS TIDKOD, F.O.M.             
001100*                                 RELEASE TIME CODE, FROM                 
001200     03 RAD-KDCATPUB-TOM     PIC X(6).                                    
001300*                                 PUBLICERINGS TIDKOD, T.O.M.             
001400*                                 RELEASE TIME CODE, TO                   
001500     03 RAD-IDUSER           PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 RAD-TIUPPDAT         PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002000*                                 UPDATING DATE     (YYMMDD)              
002100     03 RAD-KDRADST          PIC X.                                       
002200*                                 RAD-     L = LÅNAD.                     
002300*                                 STATUS   Ä = ÄNDRAD.                    
002400*                                          N = NYREGISTRERAD.             
002500*                                          C = L,Ä,N EFTER OMBRYT         
002600*                                              FRAM TILL VADGEN.          
002700*                                      SPACE = OFÖRÄNDRAD.                
002800*                                 LINE-    L = LOAN LINE.                 
002900*                                 STATUS   Ä = AMENDED LINE.              
003000*                                          N = NEW LINE.                  
003100*                                          C = CHANGED LINE - EG,         
003200*                                              SUBSCR.OF ABOVE 3.         
003300*                                      SPACE = UNCHANGED LINE.            
003400*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
