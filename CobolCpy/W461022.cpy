000100 01  KREDRAD-W461022.                                                     
000200*                                 KREDITERINGSTRANSAR  FÖR ORDER          
000300*                                 FRÅN DISTRIKT 1283                      
000400*                                 FRÅN VR SYSTEMET PT 022                 
000500     03 KREDRAD-IDPTYP       PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 KREDRAD-IDDISTR      PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 KREDRAD-IDKUNDNR     PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 KREDRAD-IDRONR       PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERNUMMER      IDRONR-002         
001300     03 KREDRAD-TIRODAT      PIC S9(7)           COMP-3.                  
001400*                                 RESTORDERDATUM         (ÅÅMMDD)         
001500     03 KREDRAD-IDLOPNR      PIC S9(3)           COMP-3.                  
001600*                                 LÖPNUMMER                               
001700     03 KREDRAD-IDFAKT       PIC S9(7)           COMP-3.                  
001800*                                 FAKTURANUMMER                           
001900     03 KREDRAD-IDORDNR      PIC S9(7)           COMP-3.                  
002000*                                 ORDERNR             IDORDNR-002         
002100     03 KREDRAD-IDARTNR      PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 KREDRAD-KDANMORS     PIC S9(3)           COMP-3.                  
002400*                                 ORSAK TILL LEVERANSANMÄRKNING           
002500     03 KREDRAD-KVKREANT     PIC S9(7)           COMP-3.                  
002600*                                 KREDITERAT ANTAL                        
002700     03 KREDRAD-PRARTBTO     PIC S9(7)V9(2)      COMP-3.                  
002800*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
002900     03 FILLER               PIC X(5).                                    
003000*** END COPY W461022CC0  LENGTH=49                                        
