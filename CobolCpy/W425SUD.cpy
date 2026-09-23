000100 01  W425SUD-CTX.                                                         
000200*                                 INFO OM EJ VR-REGISTRERAD TPO           
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDSUPPL              PIC 9(5).                                    
001100*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001200     03 IDORDNR-002          PIC 9(7).                                    
001300*                                 ORDERNR             IDORDNR-002         
001400     03 IDARTNR              PIC 9(8).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 REKSIFFR             PIC 9.                                       
001700*                                 KONTROLLSIFFRA                          
001800     03 KVBEART-002          PIC 9(5).                                    
001900*                                 LAGERAVBOKAT,       KVBEART-002         
002000*                                 PACKAT ELLER RESTNOTERAT ANTAL          
002100     03 TITPO                PIC 9(6).                                    
002200*                                 PLANERAD ORDERDATUM                     
002300     03 KDTPOTYP             PIC 9.                                       
002400*                                 TYP AV TIDPLANERAD ORDER                
002500     03 KDVRTPO              PIC 9.                                       
002600*                                 KOD FÖR TPO:ER FRÅN VR                  
002700     03 KDVRINFO             PIC 9.                                       
002800*                                 PÅVERKAN I VR/DSP SYSTEM                
002900     03 TIAAMMDD             PIC 9(6).                                    
003000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003100     03 TIKLOCK              PIC 9(8).                                    
003200*                                 KLOCKSLAG (TTMMSSTH)                    
003300     03 FILLERX52            PIC X(52).                                   
003400*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
