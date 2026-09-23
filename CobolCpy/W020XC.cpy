000100 01  W020XC.                                                              
000200*                                 W020XC:                                 
000300*                                 ANVÄNDS FÖRSTA GÅNGEN I W09208          
000400*                                 OCH INNEH. VECKANS RO-TRANS-            
000500*                                 AKTIONER SOM LÄSTS NED FRÅN             
000600*                                 WDG6. INGÅR I UPPFÖLJNING AV            
000700*                                 RO, BORS.                               
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 IDDISTR              PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 IDORDER              PIC S9(7)           COMP-3.                  
001500*                                 VOLVO PARTS ORDERNUMMER                 
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 TIRODAT              PIC S9(7)           COMP-3.                  
001900*                                 RESTORDERDATUM         (ÅÅMMDD)         
002000     03 KVLEVART             PIC S9(7)           COMP-3.                  
002100*                                 LEVERERAT ANTAL ARTIKLAR                
002200     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
002300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002400     03 KDORDKL              PIC S9              COMP-3.                  
002500*                                 ORDERKLASS                              
002600     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002700*                                 PRODUKTSLAG                             
002800     03 KDORDBEK             PIC 9(2).                                    
002900*                                 ORDERBEKRÄFTELSEKOD                     
003000*** END COPY W020XC      LENGTH=36                                        
