000010 01  W37152.                                                              
000020*                                 GODKÄNDA BYTESOBJEKT                    
000030*                                 FRÅN CSI-SYSTEM TILL                    
000040*                                 BYTES (W371)                            
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
000230     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
000240*                                 OBJEKTNUMMER                            
000250     03 KDBYTREF             PIC X(3).                                    
000260*                                 CENTRAL REFERENS                        
000270     03 IDLAGER              PIC X(2).                                    
000280*                                 IDENTIFIERARE LAGER                     
000290     03 KVRETUR-GODK         PIC S9(5)           COMP-3.                  
000300*                                 ANTAL I RETUR                           
      *** END COPY W37152      LENGTH=33                                        
