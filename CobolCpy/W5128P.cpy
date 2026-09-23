000100 01  W5128P.                                                              
000200*                                 WDK6 AND WDK7 DATA                      
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 KDERS                PIC S9(3)           COMP-3.                  
001000*                                 ERSÄTTNINGSKOD                          
001100*                                 SUPERSESSION CODE                       
001200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001300*                                 PRODUKTSLAG                             
001400*                                 PRODUCT GROUP                           
001500     03 KDPSLLOC             PIC 9(2).                                    
001600*                                 PRODUKTSLAG LOKALT                      
001700*                                 PRODUCT GROUP LOCAL                     
001800     03 TIFINLV              PIC S9(5)           COMP-3.                  
001900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002000*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
002100     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
002200*                                 PERIODBEHOV REFILLING                   
002300*                                 FORECAST REFILLING                      
002400     03 KVEFRS               PIC S9(7)           COMP-3.                  
002500*                                 EJ FAKTURERAT ANTAL STYCK               
002600*                                 ORDERED NOT INVOICED QTY                
002700     03 KVLS                 PIC S9(7)           COMP-3.                  
002800*                                 LAGERSALDO                              
002900*                                 STOCK BALANCE                           
003000     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
003100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003200*                                 AVERAGE COST FOREIGN CURRENCY           
003300*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
