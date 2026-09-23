000100 01  W231L003.                                                            
000200*                                 LÄNKAREA FÖR LÄSNING AV                 
000300*                                 GEMENSAM- INFO                          
000400     03 KDCALL               PIC S9(3)           COMP-3.                  
000500      88 LAES-GEMENSAMINFO   VALUE +3.                                    
000600     03 FLJANEJ-C2           PIC X.                                       
000700*                                 JA/NEJ-FLAGGA                           
000800     03 IO-AREA.                                                          
000900        05 KDUART            PIC X.                                       
001000*                                 UNDANTAGSARTIKEL                        
001100        05 KDLTK             PIC S9              COMP-3.                  
001200*                                 LAGERTILLHÖRIGHETSKOD                   
001300        05 KDERS             PIC S9(3)           COMP-3.                  
001400*                                 ERSÄTTNINGSKOD                          
001500        05 CLAGERDEL         OCCURS 2 TIMES.                              
001600           07 KVAKS          PIC S9(7)           COMP-3.                  
001700*                                 ANKOMSTSALDO                            
001800           07 KVAKS-E        PIC S9(7)           COMP-3.                  
001900*                                 DEL AV EFR TILL ANDRA CLAGRET           
002000           07 KVAKS-F        PIC S9(7)           COMP-3.                  
002100*                                 DEL AV AKS TILL ANDRA CLAGRET           
002200           07 KVLS           PIC S9(7)           COMP-3.                  
002300*                                 LAGERSALDO                              
002400           07 KVRESS         PIC S9(7)           COMP-3.                  
002500*                                 RESERVERAT ANTAL ARTIKLAR               
002600           07 KVROS          PIC S9(7)           COMP-3.                  
002700*                                 RESTORDERSALDO                          
002800           07 KVSLAGER       PIC S9(7)           COMP-3.                  
002900*                                 SÄKERHETSLAGER                          
003000*** END COPY W231L003C0  LENGTH=63                                        
