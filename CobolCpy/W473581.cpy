000100 01  W473581.                                                             
000200*                                 BESKRIVNING AV KVANTITETS               
000300*                                 FÖRÄNDRINGSPOST.                        
000400     03 IDDEL.                                                            
000500        05 W412580.                                                       
000600*                                 ID-DEL FÖR PT 58X                       
000700*                                                                         
000800           07 IDPTYP         PIC X(3).                                    
000900*                                 POSTTYP                                 
001000           07 IDDISTR        PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200           07 IDKUNDNR       PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400           07 KDCLAGER       PIC S9              COMP-3.                  
001500*                                 CENTRALLAGERKOD                         
001600           07 KDFRAKT        PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT C1-C2 TILL KUND               
001800           07 IDORDNR        PIC S9(5)           COMP-3.                  
001900*                                 ORDERNUMMER                             
002000           07 KDRESTR        PIC S9(3)           COMP-3.                  
002100*                                 RESTRIKTIONSKOD                         
002200           07 IDARTNR        PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400           07 IDKORTNR       PIC S9(3)           COMP-3.                  
002500*                                 KORTNUMMER                              
002600*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002700           07 IDLOPNRE       PIC S9(3)           COMP-3.                  
002800*                                 LÖPNUMMER ERSÄTTNING                    
002900           07 IDRONR         PIC S9(5)           COMP-3.                  
003000*                                 RESTORDERNUMMER                         
003100           07 IDDIVORD       PIC S9(3)           COMP-3.                  
003200*                                 DIVERSEORDERNUMMER                      
003300           07 KDORDKL        PIC S9              COMP-3.                  
003400*                                 ORDERKLASS                              
003500           07 KDFAKTYP       PIC X.                                       
003600*                                 FAKTURATYP                              
003700           07 IDPTYP-URS     PIC X(3).                                    
003800*                                 POSTTYP                                 
003900           07 KDKVBRYT       PIC S9              COMP-3.                  
004000*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004100           07 TIAAMMDD       PIC S9(7)           COMP-3.                  
004200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004300           07 TIKLOCK        PIC S9(9)           COMP-3.                  
004400*                                 KLOCKSLAG (HHMMSSTH)                    
004500     03 BEART                PIC X(15).                                   
004600*                                 ARTIKELBENÄMNING      BEART-002         
004700     03 KVBEART              PIC S9(7)           COMP-3.                  
004800*                                 BESTÄLLT ANTAL ARTIKLAR                 
004900     03 KVLEVART             PIC S9(7)           COMP-3.                  
005000*                                 LEVERERAT ANTAL ARTIKLAR                
005100     03 FLSLULEV             PIC S9              COMP-3.                  
005200*                                 SLUTLEVERANSMÄRKNING                    
005300     03 KDARTERS             PIC S9              COMP-3.                  
005400*                                 ERSÄTTNINGSKOD W415                     
005500     03 FILLER               PIC X.                                       
005600*** END COPY W473581CC0  LENGTH=73                                        
