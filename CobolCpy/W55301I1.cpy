000100 01  MID-W55301I1.                                                        
000200*                                 TRANSACTION TO PROGRAM W55301           
000300*                                 VIA W00606                              
001600     03 MID-IDTRANS          PIC X(4)                                     
001700                             VALUE SPACES.                                
001800*                                 BILDNUMMER                              
002000     03 MID-KDMFSFOR         PIC X                                        
002100                             VALUE SPACE.                                 
002200*                                 TYP AV MFS-FORMAT                       
002300*                                 1 = R-FORMAT  2 = N-FORMAT              
002500     03 MID-IDPROCESS        PIC X(10)                                    
002600                             VALUE SPACES.                                
002700*                                 PROCESSNAMN                             
002900     03 MID-KDSOPFUNK        PIC X                                        
003000                             VALUE SPACE.                                 
003100*                                 FUNKTIONSTYP TILL SOP PROGRAM           
003300     03 MID-TESYMBV          PIC X(500)                                   
003400                             VALUE SPACES.                                
