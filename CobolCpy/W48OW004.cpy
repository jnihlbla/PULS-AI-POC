000100 01  W480W004.                                                            
000200     03 IX                   PIC S9(4)           COMP SYNC                
000300                             VALUE ZEROS.                                 
000400     03 MAX-IX               PIC S9(4)           COMP SYNC                
000500                             VALUE +11.                                   
000600     03 IDARTNR              PIC S9(9)           COMP-3                   
000700                             VALUE ZEROS.                                 
000800     03 BEART-SVE            PIC X(25)                                    
000900                             VALUE SPACES.                                
001000     03 PRISTILL             PIC X                                        
001100                             VALUE SPACE.                                 
001200     03 PRARTNTO             PIC 9(9)                                     
001300                             VALUE ZEROS.                                 
001400     03 FILLER               OCCURS 11 TIMES.                             
001500        05 FORDELAS          PIC X.                                       
001600        05 KVBEART           PIC S9(7)           COMP-3.                  
001700        05 ORDERID.                                                       
001800           07 IDDISTR        PIC 9(4).                                    
001900           07 IDKUNDNR       PIC 9(6).                                    
002000           07 KDCLAGER       PIC 9.                                       
002100           07 KDFRAKT        PIC 9(2).                                    
002200           07 IDORDNR        PIC 9(5).                                    
002300           07 KDORDKL        PIC 9.                                       
002400           07 KDMASK         PIC X.                                       
002500*** END COPY W480W004C0  LENGTH=319                                       
