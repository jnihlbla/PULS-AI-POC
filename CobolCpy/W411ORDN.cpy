000100 01  ORDN-W411ORDN.                                                       
000200*                                 LÄNKAREA TILL W411ORDN - KONTRO         
000300*                                 LL OCH UTTAG AV AUT. ORDERNR            
000400     03 ORDN-IDSYSTEM        PIC X(4).                                    
000500*                                 SKAPANDE SYSTEMNUMMER                   
000600     03 ORDN-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 ORDN-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 ORDN-IDORDNR-IN      PIC 9(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 ORDN-IDORDNR-UT      PIC 9(7).                                    
001300*                                 ORDERNUMMER                             
001400     03 ORDN-IDPRODNR-UT     PIC S9(7)           COMP-3.                  
001500*                                 PRODUKTIONSNUMMER                       
001600     03 ORDN-IDORDER-UT      PIC S9(7)           COMP-3.                  
001700*                                 VOLVO PARTS ORDERNUMMER                 
001800     03 ORDN-IDORDNSB-UT     PIC S9(5)           COMP-3.                  
001900*                                 SATSORDERNUMMER-BAS                     
002000*** END COPY W411ORDN    LENGTH=36                                        
