000100 01  AREA.                                                                
000200     03 KDCALL               PIC S9(3)           COMP-3                   
000300                             VALUE ZEROS.                                 
000400*                                 ANROPSTYP                               
000500     03 LAES-ARTIKELBAS      PIC S9(3)           COMP-3                   
000600                             VALUE +1.                                    
000700     03 LAES-ARTIKELBEN      PIC S9(3)           COMP-3                   
000800                             VALUE +2.                                    
000900     03 KDSVAR               PIC X                                        
001000                             VALUE SPACE.                                 
001100*                                 SVARSKOD FRÅN SUBPROGRAM                
001200     03 KDSVAR-OK            PIC X                                        
001300                             VALUE ' '.                                   
001400*                                 SVARSKOD : OK                           
001500     03 KDSVAR-FEL           PIC X                                        
001600                             VALUE 'F'.                                   
001700*                                 SVARSKOD: FEL                           
001800*                                                                         
001900*** END COPY W488L420C0  LENGTH=9                                         
