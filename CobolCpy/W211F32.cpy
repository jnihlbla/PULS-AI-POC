000100 01  W211F32.                                                             
000200*                                                                         
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 KDCLAGER             PIC 9.                                       
000600*                                 CENTRALLAGERKOD                         
000700     03 IDARTNR              PIC 9(8).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDLOPNRM             PIC 9(8).                                    
001000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001100*                                 (0VVDLLLLK)                             
001200     03 KDAVVANT             PIC 9.                                       
001300*                                 AVVIKELSEANTAL KOD                      
001400*                                 0=INGEN ANM.   1=AVVIKELSE              
001500*                                 2=MAKULERING AV MOTT.RAPPORT            
001600     03 KVANTMOT             PIC 9(6).                                    
001700*                                 ANTAL MOTTAGET                          
001800     03 KVFORDEL             PIC 9(6).                                    
001900*                                 ANTAL FÖRDELAT                          
002000     03 IDKOLLI              PIC 9(6).                                    
002100*                                 KOLLINUMMER         IDKOLLI-002         
002200     03 KDAVVKV              PIC 9.                                       
002300*                                 KVALITETSAVVIKELSEKOD                   
002400*                                 0=INGEN ANM.  1=AVVIKELSE               
002500*                                 2=AVVIKELSE, RETURNERAS                 
002600     03 KVRETUR              PIC 9(6).                                    
002700*                                 RETURNERAT ANTAL    KVRETUR-002         
002800*** END COPY W211F32CC0  LENGTH=46                                        
