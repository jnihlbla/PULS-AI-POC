000100 01  W56082.                                                              
000200*                                 INFO LAGERSTATUS-LISTA FÖR USA          
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEART-ENG            PIC X(25).                                   
000700*                                 ENGELSK ARTIKELBENÄMNING                
000800     03 KDPSLLOC             PIC 9(2).                                    
000900*                                 PRODUKTSLAG LOKALT                      
001000     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
001100*                                 ARTIKELPRIS NETTO                       
001200     03 KVLS                 OCCURS 7 TIMES                               
001300                             PIC S9(7)           COMP-3.                  
001400*                                 LAGERSALDO                              
001500     03 KVEFRS               OCCURS 7 TIMES                               
001600                             PIC S9(7)           COMP-3.                  
001700*                                 EJ FAKTURERAT ANTAL STYCK               
001800     03 PRAVCOST             OCCURS 7 TIMES                               
001900                             PIC S9(7)V9(2)      COMP-3.                  
002000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002100*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
