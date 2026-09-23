000100 01  W371FAK-CTX.                                                         
000200*                                 FAKTURERADE RADER TILL BYTES            
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDORDNR              PIC S9(5)           COMP-3.                  
001500*                                 ORDERNUMMER                             
001600     03 KVLEVART             PIC S9(7)           COMP-3.                  
001700*                                 LEVERERAT ANTAL STYCK                   
001800     03 FLINVEST             PIC X.                                       
001900*                                 BYTES INVENTERINGSFLAGGA                
002000*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
