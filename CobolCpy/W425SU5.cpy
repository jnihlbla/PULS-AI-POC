000100 01  W425SU5.                                                             
000200*                                 POSTTYP SU5                             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDSUPPL              PIC 9(5).                                    
001000*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001100     03 IDRONR               PIC 9(7).                                    
001200*                                 RESTORDERNUMMER      IDRONR-002         
001300     03 IDARTNR              PIC 9(8).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 REKSIFFR             PIC 9.                                       
001600*                                 KONTROLLSIFFRA                          
001700     03 KVRO                 PIC 9(5).                                    
001800*                                 RESTORDERKVANTITET     KVRO-002         
001900     03 KDORDER              PIC 9.                                       
002000*                                 ORDERKOD                                
002100     03 KDFAKTYP             PIC X.                                       
002200*                                 FAKTURATYP                              
002300     03 KDORDKL              PIC 9.                                       
002400*                                 ORDERKLASS                              
002500     03 KDVRINFO             PIC 9.                                       
002600*                                 PÅVERKAN I VR/DSP SYSTEM                
002700     03 TIAAMMDD-REG         PIC 9(6).                                    
002800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002900     03 TIKLOCK-REG          PIC 9(8).                                    
003000*                                 KLOCKSLAG (TTMMSSTH)                    
003100     03 FILLER               PIC X(57).                                   
003200*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
