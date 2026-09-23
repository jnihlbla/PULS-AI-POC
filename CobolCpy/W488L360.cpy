000100 01  AREA.                                                                
000200     03 KDCALL               PIC S9(3)           COMP-3                   
000300                             VALUE ZEROS.                                 
000400*                                 ANROPSTYP                               
000500     03 UPPDATERA-ARTD       PIC S9(3)           COMP-3                   
000600                             VALUE +1.                                    
000700     03 KDSVAR               PIC X                                        
000800                             VALUE SPACE.                                 
000900*                                 SVARSKOD FRÅN SUBPROGRAM                
001000     03 KDSVAR-OK            PIC X                                        
001100                             VALUE ' '.                                   
001200*                                 SVARSKOD : OK                           
001300     03 KDSVAR-FEL           PIC X                                        
001400                             VALUE 'F'.                                   
001500*                                 SVARSKOD: FEL                           
001600*                                                                         
001700*** END COPY W488L360C0  LENGTH=7                                         
