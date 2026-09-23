000100 01  W601-W6012111.                                                       
000200*                                 COPYTEXT FÖR SUBPGM W6012111            
000300     03 W601-IDPRTLST        PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500*                                 LOGICAL PRINTER+LIST IDENTITY           
000600     03 W601-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 W601-IDLBBET         PIC X(12).                                   
001000*                                 LASTBÄRARBETECKNING                     
001100*                                 TRAILER NUMBER                          
001200     03 W601-IDUSER          PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400*                                 USER SECURITY-IDENTITY                  
001500     03 W601-TELOSSN         PIC X(105).                                  
001600*                                 LOSSNINGSTEXT                           
001700*                                 UNLOADING TEXT                          
001800*** END OF VILMAII-COPY LENGTH= 135 BYTES                                 
