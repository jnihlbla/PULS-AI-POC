000100 01  4474-WDGX4474.                                                       
000200*                                 LISTBESTÄLLNINGAR                       
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDLISTTYP + IDLISTA +                  
000500*                                  LOW-VALUE)                             
000600     03 4474-IDLISTTYP       PIC X(6).                                    
000700*                                 LISTTYP                                 
000800*                                 LISTTYP                                 
000900     03 4474-IDLISTA         PIC S9(3)           COMP-3.                  
001000*                                 LISTNUMMER                              
001100     03 4474-LOW-VALUE       PIC X(2).                                    
001200     03 4474-BELISTA         PIC X(25).                                   
001300*                                 TYP AV LISTNING                         
001400     03 4474-FLBEST          PIC X.                                       
001500*                                 LISTA BESTÄLLD                          
001600*                                 LIST ORDERED                            
001700     03 4474-FILLER          PIC X(4).                                    
001800*** END COPY WDGX4474C0  LENGTH=40                                        
