000100 01  W425PSU9-CTX.                                                        
000200*                                 FAKTURARAD2: KVQPACK, KDPRODSL          
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDSUPPL              PIC 9(5).                                    
001000*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001100     03 IDARTNR              PIC 9(8).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 REKSIFFR             PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 BEART-004            PIC X(12).                                   
001600*                                                       BEART-004         
001700*                                 BENÄMNING ENLIGT SPRÅKKOD               
001800     03 KDSORT               PIC X(2).                                    
001900*                                 SORT-KOD                                
002000     03 IDFKNGRP             PIC 9(5).                                    
002100*                                 FUNKTIONSGRUPP                          
002200     03 KDPRODSL             PIC 9(2).                                    
002300*                                 PRODUKTSLAG                             
002400     03 KDVVKL               PIC 9.                                       
002500*                                 VOLYMVÄRDESKLASS                        
002600     03 PRARTBTO             PIC 9(7)V9(2).                               
002700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
002800     03 KDRABATT             PIC 9(2).                                    
002900     03 KVQPACK              PIC 9(5).                                    
003000*                                 ANTAL KVANTITETFÖRPACKNINGAR            
003100     03 KDORDER              PIC 9.                                       
003200*                                 ORDERKOD                                
003300     03 KDRO                 PIC 9.                                       
003400*                                 RESTORDERKOD PÅ INFORMATION             
003500*                                 TILL VR                                 
003600     03 FLQPRIS              PIC 9.                                       
003700*                                 KVANTITETSPRISMÄRKE                     
003800     03 KDVRINFO             PIC 9.                                       
003900*                                 PÅVERKAN I VR/DSP SYSTEM                
004000     03 TIAAMMDD             PIC 9(6).                                    
004100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004200     03 TIKLOCK              PIC 9(8).                                    
004300*                                 KLOCKSLAG (TTMMSSTH)                    
004400     03 REBPRIS              PIC 9(4)V9(1).                               
004500*                                 BASPRISNIVÅ                             
004600     03 FILLERX26            PIC X(26).                                   
004700*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
