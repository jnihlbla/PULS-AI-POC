000100 01  RY9S-WDGZRY9S.                                                       
000200*                                 RY9, DEL 2 LÄGGES I SORTAREAN.          
000300*                                 SKAPAS VID ANNULLATION/                 
000400*                                 ÄNDRING/BEDÖMNING AV RO/TPO.            
000500*                                 ANVÄNDS VID SKAPANDE AV TRAN-           
000600*                                 SAKTIONER TILL ÖVRIGA SYSTEM.           
000700     03 RY9S-IDDISTR         PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 RY9S-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 RY9S-IDORDER         PIC S9(7)           COMP-3.                  
001200*                                 VOLVO PARTS ORDERNUMMER                 
001300     03 RY9S-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 RY9S-KDFRAKT         PIC S9(3)           COMP-3.                  
001600*                                 FRAKTSÄTT C1-C2 TILL KUND               
001700     03 RY9S-KDORDBEK        PIC 9(2).                                    
001800*                                 ORDERBEKRÄFTELSEKOD                     
001900     03 RY9S-KDORDKL         PIC S9              COMP-3.                  
002000*                                 ORDERKLASS                              
002100     03 FILLER               PIC X(18).                                   
002200*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
