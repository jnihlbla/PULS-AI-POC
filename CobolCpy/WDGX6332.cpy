000100 01  6332-WDGX6332.                                                       
000200*                                 DC TREE LEVEL-2                         
000300*                                 FYSISK NYCKEL: IDDC                     
000400     03 6332-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 6332-IDUSER          OCCURS 4 TIMES                               
000800                             PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 6332-KDDC            PIC X(2).                                    
001200*                                 TYP AV DISTR. LAGER                     
001300*                                 TYPE OF DELIV. CENTER                   
001400*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
