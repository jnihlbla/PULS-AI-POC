000100 01  KRED-W460006.                                                        
000200*                                 KREDITERINGS-TRANSAKTIONER NOAC         
000300*                                 POSTTYP = RHD                           
000400     03 KRED-SORT-IDDISTR    PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 KRED-SORT-TIFILDAT   PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 KRED-SORT-TIHHMMSS   PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 KRED-IDPTYP          PIC X(3).                                    
001100*                                 POSTTYP                                 
001200     03 KRED-IDDISTR         PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 KRED-IDKUNDNR        PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 KRED-IDORDNR         PIC 9(7).                                    
001700*                                 ORDERNR             IDORDNR-002         
001800     03 KRED-IDTRANSLOP      PIC 9(5).                                    
001900*                                 TRANSAKTIONS-LÖPNUMMER                  
002000*** END COPY W460006CC0  LENGTH=41                                        
