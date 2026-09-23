000100 01  RYK-WDGZRYK.                                                         
000200*                                 RYK,                                    
000300*                                 SKAPAS DÅ ORDER GÅR I REST              
000400*                                 FÖR FÖRSTA GÅNGEN ELLER VID             
000500*                                 ANNULLATION AV RESTORDER.               
000600*                                 ANVÄNDS VID SKAPANDE AV TRANS-          
000700*                                 AKTIONER TILL ÖVRIGA SYSTEM.            
000800     03 RYK-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 RYK-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 RYK-IDKUNDNR         PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 RYK-IDORDER          PIC S9(7)           COMP-3.                  
001500*                                 VOLVO PARTS ORDERNUMMER                 
001600     03 RYK-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 RYK-TIRODAT          PIC S9(7)           COMP-3.                  
001900*                                 RESTORDERDATUM         (ÅÅMMDD)         
002000     03 RYK-KVLEVART         PIC S9(7)           COMP-3.                  
002100*                                 LEVERERAT ANTAL ARTIKLAR                
002200     03 RYK-KVBEART-Q        PIC S9(7)           COMP-3.                  
002300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002400     03 RYK-KDORDKL          PIC S9              COMP-3.                  
002500*                                 ORDERKLASS                              
002600     03 RYK-KDPRODSL         PIC S9(3)           COMP-3.                  
002700*                                 PRODUKTSLAG                             
002800     03 RYK-KDORDBEK         PIC 9(2).                                    
002900*                                 ORDERBEKRÄFTELSEKOD                     
003000     03 FILLER               PIC X(54).                                   
003100*** END COPY WDGZRYK     LENGTH=90                                        
