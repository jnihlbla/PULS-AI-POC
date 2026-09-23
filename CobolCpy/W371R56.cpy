000100 01  W371R56.                                                             
000200*                                 RETUR AV BYTESOBJEKT                    
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
001100*                                 OBJEKTNUMMER                            
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDORDNR              PIC S9(5)           COMP-3.                  
001500*                                 ORDERNUMMER                             
001600     03 KVRETUR              PIC S9(5)           COMP-3.                  
001700*                                 ANTAL I RETUR                           
001800     03 IDKUNDRF             PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
