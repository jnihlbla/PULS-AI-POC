000100 01  CROSS-WDE621.                                                        
000200*                                 KOLLIREGISTER                           
000300*                                 CROSS DC INFO                           
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500     03 CROSS-KDSEGKEY       PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700     03 CROSS-IDDC-CROSS     PIC X(2).                                    
000800*                                 DC FÖR CROSS DOCKING                    
000900     03 CROSS-TIRFSDAT       PIC 9(6).                                    
001000*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001100     03 CROSS-IDDISTR        PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 CROSS-IDKUNDNR       PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500     03 CROSS-IDPRODNR       PIC S9(7)           COMP-3.                  
001600*                                 PRODUKTIONSNUMMER                       
001700     03 CROSS-IDKOLLI        PIC S9(5)           COMP-3.                  
001800*                                 KOLLINUMMER                             
001900     03 CROSS-IDLEVNR        PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100     03 CROSS-IDSUPREF       PIC X(10).                                   
002200*                                 LEVERANTöRSREF.                         
002300     03 CROSS-IDTRPTNR-CROSS PIC S9(3)           COMP-3.                  
002400*                                 TRANSPORTIDENTITET DCCROSS              
002500     03 CROSS-TIRECXDAT      PIC 9(6).                                    
002600*                                 DATUM FÖR MOTTAG KLI I KROSS-DC         
002700     03 CROSS-TIRECXTID      PIC 9(4).                                    
002800*                                 TID FÖR MOTTAG KOLLI I KROSS-DC         
002900     03 CROSS-IDDC-SEND      PIC X(2).                                    
003000*                                 SÄNDANDE LAGER                          
003100     03 CROSS-KDKOLSTA-CROSS PIC S9              COMP-3.                  
003200*                                 KOLLISTATUS CROSS-DOCK KOLLI            
003300     03 CROSS-TISKEPPN       PIC S9(7)           COMP-3.                  
003400*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
003500     03 CROSS-IDSHIPM-CROSS  PIC 9(7).                                    
003600*                                 SKEPPNINGSNUMMER FRÅN KROSS DC          
003700     03 CROSS-IDLBBET-CROSS  PIC X(12).                                   
003800*                                 LASTBÄRARBETECKNING DCCROSS             
003900*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
