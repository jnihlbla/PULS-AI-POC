000100 01  4101-WDGX4101.                                                       
000200*                                 RETUR AV RETURER                        
000300*                                 FYSISK NYCKEL: WDGXKEY                  
000400*                                  (IDHTYP, IDDC, IDDISTR,)               
000500*                                  ( LOW-VALUE)                           
000600     03 4101-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4101-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4101-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 4101-LOWVALUE        PIC X(21).                                   
001500*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
