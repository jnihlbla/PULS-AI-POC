000100 01  W425SU1-CTX.                                                         
000200*                                 POSTTYP SU1                             
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
001700     03 KVBEART-002          PIC 9(5).                                    
001800*                                 LAGERAVBOKAT,       KVBEART-002         
001900*                                 PACKAT ELLER RESTNOTERAT ANTAL          
002000     03 KDORDER              PIC 9.                                       
002100*                                 ORDERKOD                                
002200     03 KDTPOTYP             PIC 9.                                       
002300*                                 TYP AV TIDPLANERAD ORDER                
002400     03 KDFAKTYP             PIC X.                                       
002500*                                 FAKTURATYP                              
002600     03 KDMANPR              PIC X.                                       
002700*                                 MANUELLT PRIS ELLER PRISTILLÄGG         
002800     03 KDVRTPO              PIC 9.                                       
002900*                                 KOD FÖR TPO:ER FRÅN VR                  
003000     03 KDVRINFO             PIC 9.                                       
003100*                                 PÅVERKAN I VR/DSP SYSTEM                
003200     03 TIAAMMDD-REG         PIC 9(6).                                    
003300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003400     03 TIKLOCK-REG          PIC 9(8).                                    
003500*                                 KLOCKSLAG (TTMMSSTH)                    
003600     03 FILLERX55            PIC X(55).                                   
003700*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
