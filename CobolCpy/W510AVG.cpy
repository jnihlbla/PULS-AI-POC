000100 01  AVG-W510AVG.                                                         
000200*                                 LÄNKAREA BERÄKNING AVERAGE COST         
000300*                                                                         
000400*                                 FYLL I AVG-INDATAS  ALLA FÄLT           
000500*                                                                         
000600*                                 MÖJLIGA VÄRDEN PÅ AVG-KDSVAR:           
000700*                                                                         
000800*                                   -> BERÄKNINGEN GICK BRA               
000900*                                                                         
001000*                                 1 -> FEL PRODUKTKOD                     
001100*                                                                         
001200*                                 2 -> FEL ANROPSTYP                      
001300*                                                                         
001400*                                 3 -> VALUTA-KURS SAKNAS                 
001500*                                                                         
001600*                                 ANROPSTYPER:                            
001700*                                                                         
001800*                                  -010  REFILL CDC TO NDC                
001900*                                                                         
002000*                                        JAPANESE AUDIO                   
002100*                                                                         
002200*                                  -020  TRANSFER WITHIN THE US           
002300*                                                                         
002400*                                  -030  TRANSFER BETWEEN US CAN          
002500*                                                                         
002600*                                  -040  ST-TERMINAL                      
002700*                                                                         
002800*                                  -060  VOR                              
002900*                                                                         
003000*                                                                         
003100*                                                                         
003200     03 AVG-KDCALL           PIC S9(3)           COMP-3.                  
003300*                                 ANROPSTYP                               
003400     03 AVG-PRAVCOST-OLD     PIC S9(7)V9(2)      COMP-3.                  
003500*                                 FÖREGÅENDE MEDELVÄRDESKOSTNAD I         
003600*                                  UTL.VALUTA                             
003700     03 AVG-KVLS-OLD         PIC S9(7)           COMP-3.                  
003800*                                 LAGERSALDO FÖRE ÄNDRING                 
003900     03 AVG-KVANTMOT         PIC S9(7)           COMP-3.                  
004000*                                 ANTAL MOTTAGET                          
004100     03 AVG-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
004200*                                 ARTIKELPRIS NETTO                       
004300     03 AVG-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
004400*                                 VALUTAKURS                              
004500     03 AVG-KDVALISO         PIC X(3).                                    
004600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004700     03 AVG-KVLEVART         PIC S9(7)           COMP-3.                  
004800*                                 LEVERERAT ANTAL STYCK                   
004900     03 AVG-PRARTBEL         PIC S9(8)V9(5)      COMP-3.                  
005000*                                 BESTPRIS LEVERANTÖRENS VALUTA           
005100     03 AVG-KDPSLLOC         PIC 9(2).                                    
005200*                                 PRODUKTSLAG LOKALT                      
005300     03 AVG-IDDC             PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500     03 AVG-PRAVCOST-NEW     PIC S9(7)V9(2)      COMP-3.                  
005600*                                 NY MEDELVÄRDESKOSTNAD I UTL.VAL         
005700*                                 UTA                                     
005800     03 AVG-REMARKUP         PIC S9V9(2)         COMP-3.                  
005900*                                 KOST UPPRÄKNINGSFAKTOR                  
006000     03 AVG-TIAA             PIC 9(2).                                    
006100*                                 ÅR    (ÅÅ)                              
006200     03 AVG-TIMM             PIC 9(2).                                    
006300*                                 MÅNAD (MM)                              
006400     03 AVG-KDPRODSL         PIC 9(2).                                    
006500*                                 PRODUKTSLAG                             
006600     03 AVG-IDFKNGRP         PIC 9(4).                                    
006700*                                 FUNKTIONSGRUPP                          
006800     03 AVG-KDSVAR           PIC X.                                       
006900*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
007000*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
