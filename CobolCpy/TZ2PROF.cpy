000100* GENERATION OF COBOL HOST STRUCTURE FROM TZ2PROF-TAB                     
000200  01 TZ2PROF.                                                             
000300*              TZ2PROF                                                    
000400   03 IDUSER                            PIC X(8).                         
000500*              ANVÄNDARENS SÄKERHETS ID                                   
000600   03 IDPROFGRP                         PIC X(8).                         
000700*              ANVÄNDARPROFIL-DATAGRUPP                                   
000800   03 IDLOPNR                           PIC S9(3) COMP-3.                 
000900*              LÖPNUMMER                                                  
001000   03 TEPROFDATA.                                                         
001100*              ANVÄNDARPROFIL-DATA                                        
001200     49 TEPROFDATA-L                    PIC S9(4) COMP.                   
001300*              ANVÄNDARPROFIL-DATA                                        
001400     49 TEPROFDATA-D                    PIC X(10000).                     
001500*              ANVÄNDARPROFIL-DATA                                        
001600*                                                                         
001700*** END OF VILMAII-COPY LENGTH= 10020 OLD LENGTH=                         
