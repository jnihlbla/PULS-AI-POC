000100 01  W5137A.                                                              
000200*                                 URVALSREGISTER FÖR DC 11 INV            
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
000700*                                 LAGEROMRÅDE                             
000800     03 ADGANG               PIC S9(3)           COMP-3.                  
000900*                                 GÅNG                                    
001000     03 ADPLATS              PIC S9(5)           COMP-3.                  
001100*                                 LAGERPLATSNUMMER                        
001200     03 KDVVKL               PIC S9              COMP-3.                  
001300*                                 VOLYMVÄRDESKLASS                        
001400     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001500*                                 PRODUKTSLAG                             
001600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001700*                                 FUNKTIONSGRUPP                          
001800     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
001900*                                 TOTALT PERIODBEHOV                      
002000     03 SUARTSTD             PIC S9(9)V9(2)      COMP-3.                  
002100*                                 SUMMA STANDARDPRIS RADVÄRDE             
002200     03 DAINVDAT             PIC S9(7)           COMP-3.                  
002300*                                 INVENTERINGSDATUM                       
002400     03 TIFINLV              PIC S9(5)           COMP-3.                  
002500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002600     03 IDURVAL-INV          PIC X.                                       
002700*                                 TYP AV URVAL FÖR INVENTERING            
002800*                                 1  = OMRÅDE 91, ALLA /ÅR                
002900*                                 2  = 10% AV DE BILLIGASTE /5 ÅR         
003000*                                 3  = 30% AV DE MED HÖGST PB /ÅR         
003100*                                 4  = 33% AV RESTEN /ÅR                  
003200*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
