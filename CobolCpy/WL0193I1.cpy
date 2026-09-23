000100 01  REQU-WL0193I1.                                                       
000200*                                 REQU-COPYTEXT FÖR WL019300              
000300*                                                                         
000400     03 REQU-TIDATUM         PIC 9(6).                                    
000500*                                 DATUM ENLIGT KDDATFORM                  
000600     03 REQU-KDSVAR          PIC X.                                       
000700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000800     03 REQU-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 REQU-IDTRPTNR        PIC 9(3).                                    
001100*                                 TRANSPORTIDENTITET                      
001200     03 REQU-IDLBBET         PIC X(12).                                   
001300*                                 LASTBÄRARBETECKNING                     
001400     03 REQU-IDMSGVER        PIC 9(3).                                    
001500*                                 VERSIONSNUMMER PÅ MEDDELANDE            
001600     03 REQU-KDPGMACT        PIC X.                                       
001700*                                 TYP AV PROGRAMBEARBETNING               
001800     03 REQU-IDUSER          PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
