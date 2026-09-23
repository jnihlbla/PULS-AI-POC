000100 01  W425SU4-CTX.                                                         
000200*                                 POSTTYP SU4                             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDSUPPL              PIC 9(5).                                    
001000*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001100     03 IDORDNR-002          PIC 9(7).                                    
001200*                                 ORDERNR             IDORDNR-002         
001300     03 IDARTNR              PIC 9(8).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 REKSIFFR             PIC 9.                                       
001600*                                 KONTROLLSIFFRA                          
001700     03 KVRO-002             PIC 9(5).                                    
001800*                                 RESTORDERKVANTITET     KVRO-002         
001900     03 KDRESTR              PIC 9(2).                                    
002000*                                 RESTRIKTIONSKOD                         
002100     03 KDRO                 PIC 9.                                       
002200*                                 RESTORDERKOD PÅ INFORMATION             
002300*                                 TILL VR                                 
002400     03 KDORDER              PIC 9.                                       
002500*                                 ORDERKOD                                
002600     03 KDFAKTYP             PIC X.                                       
002700*                                 FAKTURATYP                              
002800     03 IDORDNR7-LEV         PIC 9(7).                                    
002900*                                 LEVERANSORDERNUMMER                     
003000     03 TID-ERS              PIC X.                                       
003100*                                 TIDIGARE ERSATT?                        
003200     03 TIDISPIN             PIC 9(6).                                    
003300*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
003400     03 KDVRINFO             PIC 9.                                       
003500*                                 PÅVERKAN I VR/DSP SYSTEM                
003600     03 TIAAMMDD-REG         PIC 9(6).                                    
003700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003800     03 TIKLOCK-REG          PIC 9(8).                                    
003900*                                 KLOCKSLAG (TTMMSSTH)                    
004000     03 FILLERX41            PIC X(41).                                   
004100*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
