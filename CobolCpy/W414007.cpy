000100 01  BASL-W414007.                                                        
000200*                                 SKAPAS VID REGISTRERING                 
000300*                                 TPO3 - BASLAGER I ORDER-                
000400*                                 ENTRY OM RAD SPÄRRAS.                   
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 BASL-IDARTNR         PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 BASL-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 BASL-IDKUNDNR        PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 BASL-IDKUNDRF        PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500     03 BASL-KDORDBEK        PIC 9(2).                                    
001600*                                 ORDERBEKRÄFTELSEKOD                     
001700     03 BASL-KVBEART-Q       PIC S9(7)           COMP-3.                  
001800*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001900     03 BASL-TITPO           PIC S9(7)           COMP-3.                  
002000*                                 PLANERAD ORDERDATUM                     
