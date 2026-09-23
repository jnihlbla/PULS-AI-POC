000100 01  W425SU2-CTX.                                                         
000200*                                 POSTTYP SU2                             
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
001300     03 IDARTNR-ERS          PIC 9(8).                                    
001400*                                 ERSATT ARTIKELNUMMER                    
001500     03 REKSIFFR-ERS         PIC 9.                                       
001600*                                 KONTROLLSIFFRA                          
001700     03 IDARTNR-TILLK        PIC 9(8).                                    
001800*                                 TILLKOMMANDE ARTIKELNUMMER              
001900     03 REKSIFFR-TILLK       PIC 9.                                       
002000*                                 KONTROLLSIFFRA                          
002100     03 KVBEART-002          PIC 9(5).                                    
002200*                                 LAGERAVBOKAT,       KVBEART-002         
002300*                                 PACKAT ELLER RESTNOTERAT ANTAL          
002400     03 DIERS                PIC 9(3)V9(3).                               
002500*                                 KVANTITET I ERSÄTTN.                    
002600     03 KVLEVART-002         PIC 9(5).                                    
002700*                                 LEVERERAT ANTAL    KVLEVART-002         
002800     03 KDRESTR              PIC 9(2).                                    
002900*                                 RESTRIKTIONSKOD                         
003000     03 KDRO                 PIC 9.                                       
003100*                                 RESTORDERKOD PÅ INFORMATION             
003200*                                 TILL VR                                 
003300     03 KDORDER              PIC 9.                                       
003400*                                 ORDERKOD                                
003500     03 FLVRERS              PIC 9.                                       
003600*                                 0 = ERSÄTTNING UPPDATERAS I VR          
003700*                                 1 = ERSÄTTNING EJ UPPDAT I VR           
003800     03 KDERS                PIC 9(3).                                    
003900*                                 ERSÄTTNINGSKOD                          
004000     03 KDORDKL              PIC 9.                                       
004100*                                 ORDERKLASS                              
004200     03 KDFAKTYP             PIC X.                                       
004300*                                 FAKTURATYP                              
004400     03 IDORDNR7-LEV         PIC 9(7).                                    
004500*                                 LEVERANSORDERNUMMER                     
004600     03 KDTPOTYP             PIC 9.                                       
004700*                                 TYP AV TIDPLANERAD ORDER                
004800     03 KDVRTPO              PIC 9.                                       
004900*                                 KOD FÖR TPO:ER FRÅN VR                  
005000     03 KDVRINFO             PIC 9.                                       
005100*                                 PÅVERKAN I VR/DSP SYSTEM                
005200     03 TIAAMMDD             PIC 9(6).                                    
005300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005400     03 TIKLOCK              PIC 9(8).                                    
005500*                                 KLOCKSLAG (TTMMSSTH)                    
005600     03 FILLERX21            PIC X(21).                                   
005700*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
