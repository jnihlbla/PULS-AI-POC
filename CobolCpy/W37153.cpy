000010 01  W37153.                                                              
000020*                                 LEV LEV.ANM. TILL BYTES                 
000030*                                 FRÅN CSI-SYSTEM.                        
000040*                                 SUPPORTLAGER                            
000050     03 IDPTYP               PIC X(3).                                    
000060*                                 POSTTYP                                 
000070     03 IDGMTREF.                                                         
000080*                                 GODSMOTTAGAREREFERENS                   
000090        05 IDDISTR           PIC S9(5)           COMP-3.                  
000100*                                 DISTRIKTNUMMER                          
000110        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
000120*                                 KUNDNUMMER                              
000130        05 IDKUNDRF          PIC X(10).                                   
000140*                                 KUNDENS REFERENS (ORDERID)              
000150        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
000160           07 IDORDNR5       PIC 9(5).                                    
000170*                                 ORDERNUMMER                             
000180           07 FILLER         PIC X(5).                                    
000190        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
000200           07 IDORDNR7       PIC 9(7).                                    
000210*                                 ORDERNUMMER                             
000220           07 FILLER         PIC X(3).                                    
000230     03 IDARTNR              PIC S9(9)           COMP-3.                  
000240*                                 ARTIKELNUMMER                           
000250     03 IDLAGER              PIC X(2).                                    
000260*                                 IDENTIFIERARE LAGER                     
000270     03 KVLEVANM             PIC S9(7)           COMP-3.                  
000280*                                 LEVERANSANMÄRKNINGSANTAL                
000290     03 KDANMORS             PIC S9(3)           COMP-3.                  
000300*                                 ORSAK TILL LEVERANSANMÄRKNING           
      *** END COPY W37153      LENGTH=33                                        
