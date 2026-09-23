000100 01  102-W00742.                                                          
000200*                                 COPYTEXT FÖR UTDATA FRÅN W00742         
000300     03 102-IDPTYP           PIC X(3)                                     
000400                             VALUE SPACES.                                
000500*                                 POSTTYP                                 
000600     03 102-IDLBBET-C2       PIC X(6)                                     
000700                             VALUE SPACES.                                
000800*                                 LASTBÄRARBETECKNING C2                  
000900     03 102-IDLOWREG         PIC X(6)                                     
001000                             VALUE SPACES.                                
001100*                                 BILREGISTERNUMMER                       
001200     03 102-KDTDOC           PIC X(2)                                     
001300                             VALUE SPACES.                                
001400*                                 TRANSPORT DOKUMENT TYP                  
001500     03 102-IDTDOC           PIC 9(7)                                     
001600                             VALUE ZEROS.                                 
001700*                                 TRANSPORT DOKUMENT NUMMER               
001800     03 102-IDDISTRPREF      PIC 9(2)                                     
001900                             VALUE ZEROS.                                 
002000*                                 DISTRIKTNUMMER LANDKOD                  
002100     03 102-IDFAKT           PIC 9(7)                                     
002200                             VALUE ZEROS.                                 
002300*                                 FAKTURANUMMER                           
002400     03 FILLER               PIC X(47)                                    
002500                             VALUE SPACES.                                
002600*** END COPY W00742CCC0  LENGTH=80                                        
