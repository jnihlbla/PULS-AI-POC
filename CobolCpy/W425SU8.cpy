000100 01  W425SU8-CTX.                                                         
000200*                                 FAKTURARAD1: IDARTNR, KVLEVART          
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDSUPPL              PIC S9(5)           COMP-3.                  
001000*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001100     03 IDFAKT               PIC S9(7)           COMP-3.                  
001200*                                 FAKTURANUMMER                           
001300     03 IDORDNR-002          PIC S9(7)           COMP-3.                  
001400*                                 ORDERNR             IDORDNR-002         
001500     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001600*                                 KOLLINUMMER                             
001700     03 IDARTNR              PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900     03 IDRONR-002           PIC S9(7)           COMP-3.                  
002000*                                 RESTORDERNUMMER      IDRONR-002         
002100     03 KVLEVART-002         PIC S9(5)           COMP-3.                  
002200*                                 LEVERERAT ANTAL    KVLEVART-002         
002300     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
002400*                                 ARTIKELPRIS NETTO                       
002500     03 KDSRA-002            PIC S9              COMP-3.                  
002600*                                 EG/EFTA-KOD           KDSRA-002         
002700     03 VKART                PIC S9(7)           COMP-3.                  
002800*                                 ARTIKELVIKT (G)                         
002900     03 KDARTURS-NUM         PIC S9(3)           COMP-3.                  
003000*                                 ARTIKELURSPRUNGSKOD NUMERISK            
003100     03 KDRO                 PIC S9              COMP-3.                  
003200*                                 RESTORDERKOD PÅ INFORMATION             
003300*                                 TILL VR                                 
003400     03 KDORDER              PIC S9              COMP-3.                  
003500*                                 ORDERKOD                                
003600     03 FLQPRIS              PIC S9              COMP-3.                  
003700*                                 KVANTITETSPRISMÄRKE                     
003800     03 IDFKNGRP-002         PIC S9(7)           COMP-3.                  
003900*                                 FUNKTIONSGRUPP     IDFKNGRP-002         
004000     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
004100*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004200     03 PRARTULL             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 TULLPRIS PER ARTIKEL                    
004400     03 TIPRIS               PIC S9(7)           COMP-3.                  
004500*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
004600     03 KDVIP                PIC X.                                       
004700*                                 VIP-KOD                                 
004800     03 BERADREF             PIC X(10).                                   
004900*                                 KUNDENS RADREFERENS                     
005000     03 KDORDKL              PIC X.                                       
005100*                                 ORDERKLASS                              
005200     03 KDORDKL-LEV          PIC X.                                       
005300*                                 ORDERKLASS                              
005400     03 KDFAKTYP             PIC X.                                       
005500*                                 FAKTURATYP                              
005600     03 FLPRTILL             PIC X.                                       
005700*                                 PRISTILLÄGGS FLAGGA                     
005800     03 KDVRINFO             PIC S9              COMP-3.                  
005900*                                 PÅVERKAN I VR/DSP SYSTEM                
006000     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
006100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
006200     03 TIKLOCK              PIC S9(9)           COMP-3.                  
006300*                                 KLOCKSLAG (TTMMSSTH)                    
006400     03 KDARTRAB             PIC 9(2).                                    
006500*                                 RABATTKOD (ARTIKELPRIS)                 
006600     03 FILLERX15            PIC X(15).                                   
006700*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
