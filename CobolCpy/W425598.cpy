000100 01  W425598.                                                             
000200*                                 SPLIT AV ORDER.  ORDERRAD STYRS         
000300*                                 FRÅN C2 TILL C1.                        
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 FLVRTP               PIC S9              COMP-3.                  
000800*                                 TRANSMISSION MED TELEPROCESSING         
000900     03 KDVR                 PIC S9              COMP-3.                  
001000*                                 ANSLUTNINGSFORM TILL VR-SYST            
001100     03 IDDISTR              PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500     03 KDCLAGER             PIC S9              COMP-3.                  
001600*                                 CENTRALLAGERKOD                         
001700     03 IDORDNR              PIC S9(5)           COMP-3.                  
001800*                                 ORDERNUMMER                             
001900     03 IDRONR               PIC S9(5)           COMP-3.                  
002000*                                 RESTORDERNUMMER                         
002100     03 IDDIVORD             PIC S9(3)           COMP-3.                  
002200*                                 DIVERSEORDERNUMMER                      
002300     03 IDARTNR              PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500     03 KDRESTR              PIC S9(3)           COMP-3.                  
002600*                                 RESTRIKTIONSKOD                         
002700     03 KDORDKL              PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900     03 IDPTYP-URS           PIC X(3).                                    
003000*                                 POSTTYP                                 
003100     03 KDKVBRYT             PIC S9              COMP-3.                  
003200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003300     03 KVBEART              PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLT ANTAL ARTIKLAR                 
003500     03 KDFAKTYP             PIC X.                                       
003600*                                 FAKTURATYP                              
003700     03 KDCLAGER-NYTT        PIC S9              COMP-3.                  
003800*                                 CENTRALLAGERKOD                         
003900     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
004000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004100     03 TIKLOCK              PIC S9(9)           COMP-3.                  
004200*                                 KLOCKSLAG (HHMMSSTH)                    
004300     03 KDRO                 PIC S9              COMP-3.                  
004400*                                 RESTORDERKOD PÅ INFORMATION             
004500*                                 TILL VR                                 
004600*** END COPY W425598CC0  LENGTH=49                                        
