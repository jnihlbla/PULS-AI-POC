000100 01  4442-WDGX4442.                                                       
000200*                                 BESKRIVNING AV HLOTABELL                
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDDC + IDHLOTAB +                      
000500*                                  LOW-VALUE)                             
000600     03 4442-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 4442-IDHLOTAB        PIC 9(2).                                    
001000*                                 HLOTABELLSIDENTITET                     
001100*                                 MAINAREA TABLE IDENTITY                 
001200     03 4442-LOW-VALUE       PIC X(6).                                    
001300     03 4442-IDHLO           OCCURS 99 TIMES                              
001400                             PIC 9(2).                                    
001500*                                 HUVUDLAGEROMRÅDE                        
001600*                                 MAIN AREA                               
001700     03 4442-FILLER          PIC X(62).                                   
