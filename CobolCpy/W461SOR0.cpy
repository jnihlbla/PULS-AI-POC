000100 01  SOR0-W461SOR0.                                                       
000200*                                 SORTDEL F÷R TRANSAKTIONER TILL          
000300*                                 NOAC                                    
000400     03 SOR0-IDDISTR         PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 SOR0-IDKUNDNR        PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 SOR0-IDRONR          PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 SOR0-TIRODAT         PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (≈≈MMDD)         
001200     03 SOR0-IDPTYP          PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 SOR0-IDLOPNR         PIC S9(5)           COMP-3.                  
001500*                                 L÷PNUMMER          IDLOPNR-002          
001600*** END COPY W461SOR0C0  LENGTH=21                                        
