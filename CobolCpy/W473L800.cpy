000100 01  0-W473L800.                                                          
000200*                                 CALL0 COPYTEXT TILL SUBPROG.            
000300*                                 W4738310                                
000400*                                                                         
000500     03 0-KDCALL             PIC S9(3)           COMP-3                   
000600                             VALUE ZEROS.                                 
000700*                                 ANROPSTYP                               
000800     03 0-LAS-SEGM           PIC S9(3)           COMP-3                   
000900                             VALUE +1.                                    
001000     03 0-TA-BORT-ORDER      PIC S9(3)           COMP-3                   
001100                             VALUE +2.                                    
001200     03 0-KDSVAR             PIC X                                        
001300                             VALUE SPACE.                                 
001400*                                 SVARSKOD FRÅN SUBPROGRAM                
001500     03 0-KDSVAR-OK          PIC X                                        
001600                             VALUE ' '.                                   
001700*                                 SVARSKOD : OK                           
001800     03 0-KDSVAR-FEL         PIC X                                        
001900                             VALUE 'F'.                                   
002000*                                 SVARSKOD: FEL                           
002100*                                                                         
002200*** END COPY W473L800C0  LENGTH=9                                         
