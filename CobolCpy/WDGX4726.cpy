000100 01  AUTFAKT-WDGX4726.                                                    
000200*                                 4726 HTR FÖR AUTOMATFAKTURERING         
000300*                                 NYCKEL: IDDISTR, IDKUNDNR               
000400*                                         IDDC, KDFAKTYP                  
000500*                                                                         
000600*                                 NYCKEL I ROTEN SAKNAS                   
000700     03 AUTFAKT-IDDISTR      PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 AUTFAKT-IDKUNDNR     PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 AUTFAKT-IDDC         PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 AUTFAKT-KDFAKTYP     PIC X.                                       
001400*                                 FAKTURATYP                              
001500*** END COPY WDGX4726    LENGTH=10                                        
