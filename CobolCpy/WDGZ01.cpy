000100 01  WDGZ01.                                                              
000200*                                 LOGG-RECORD ROTSEG                      
000300*                                 RECORD TILL KONTROLLLISTAN              
000400*                                 OCH ANDRA SEKV. FILER                   
000500     03 WDGZPOST.                                                         
000600        05 WDGZKEY.                                                       
000700           07 TIAAMMDD       PIC 9(6).                                    
000800*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
000900           07 TIKLOCK        PIC 9(8).                                    
001000*                                 KLOCKSLAG (HHMMSSTH)                    
001100           07 IDLOGLOP       PIC 9.                                       
001200*                                 L÷PNUMMER I LOGGPOST                    
001300*                                 (÷KAS MED 1 VARJE G≈NG                  
001400*                                 F÷R ATT F≈ UNIK NYCKEL)                 
001500           07 IDPTYP         PIC X(3).                                    
001600*                                 POSTTYP                                 
001700        05 FILLER            PIC X(87).                                   
001800     03 FILLER REDEFINES WDGZPOST.                                        
001900        05 FILLER            PIC X(15).                                   
002000        05 LOGGPOST          PIC X(90).                                   
002100     03 SORTPOST             PIC X(36).                                   
002200*** END COPY WDGZ01CCC0  LENGTH=141                                       
