000100 01  W425SU3-CTX.                                                         
000200*                                 POSTTYP SU3                             
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
002000     03 KDKVFOR              PIC 9.                                       
002100*                                 KVANTANPASSNINGSKOD                     
002200*                                 1 = ÖKNING.  2 = MINSKNING              
002300     03 KDRESTR              PIC 9(2).                                    
002400*                                 RESTRIKTIONSKOD                         
002500     03 KDRO                 PIC 9.                                       
002600*                                 RESTORDERKOD PÅ INFORMATION             
002700*                                 TILL VR                                 
002800     03 KDORDER              PIC 9.                                       
002900*                                 ORDERKOD                                
003000     03 IDORDNR7-LEV         PIC 9(7).                                    
003100*                                 LEVERANSORDERNUMMER                     
003200     03 KVQPACK-1            PIC 9(5).                                    
003300*                                 ANTAL I Q1 FÖRPACKNING                  
003400     03 TID-ERS              PIC X.                                       
003500*                                 TIDIGARE ERSATT?                        
003600     03 KDORDKL              PIC 9.                                       
003700*                                 ORDERKLASS                              
003800     03 KDFAKTYP             PIC X.                                       
003900*                                 FAKTURATYP                              
004000     03 FIKTIV-KVANT         PIC 9(4).                                    
004100*                                 FIKTIV KVANT                            
004200     03 KDVRTPO              PIC 9.                                       
004300*                                 KOD FÖR TPO:ER FRÅN VR                  
004400     03 KDVRINFO             PIC 9.                                       
004500*                                 PÅVERKAN I VR/DSP SYSTEM                
004600     03 TIAAMMDD             PIC 9(6).                                    
004700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004800     03 TIKLOCK              PIC 9(8).                                    
004900*                                 KLOCKSLAG (TTMMSSTH)                    
005000     03 FILLERX35            PIC X(35).                                   
005100*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
