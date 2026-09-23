000100 01  WDG601.                                                              
000200*                                 LOGG-RECORD ROTSEG                      
000300*                                 RECORD TILL KONTROLLLISTAN              
000400*                                 OCH ANDRA SEKV. FILER                   
000500     03 WDG6POST.                                                         
000600        05 WDG6KEY.                                                       
000700*                                 FYSISK NYCKEL                           
000800           07 TIAAMMDD       PIC 9(6).                                    
000900*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001000           07 TIKLOCK        PIC 9(8).                                    
001100*                                 KLOCKSLAG (HHMMSSXX)                    
001200           07 IDLOGLOP       PIC 9.                                       
001300*                                 L÷PNUMMER I LOGGPOST                    
001400*                                 (÷KAS MED 1 VARJE G≈NG                  
001500*                                 F÷R ATT F≈ UNIK NYCKEL)                 
001600           07 IDPTYP         PIC X(3).                                    
001700*                                 POSTTYP                                 
001800        05 FILLER            PIC X(87).                                   
001900     03 FILLER REDEFINES WDG6POST.                                        
002000        05 FILLER            PIC X(15).                                   
002100        05 LOGGPOST          PIC X(90).                                   
002200     03 SORTPOST             PIC X(36).                                   
002300*** END COPY WDG601CCC0  LENGTH=141                                       
