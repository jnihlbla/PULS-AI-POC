000100 01  W56081.                                                              
000200*                                 AKT. ART. FÖR LAGER-LISTA USA           
000300*                                 LEVERANTÖR 1441                         
000400*                                                                         
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 KDPSLLOC             PIC 9(2).                                    
000800*                                 PRODUKTSLAG LOKALT                      
000900     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
001000*                                 ARTIKELPRIS NETTO                       
001100     03 IDDC                 OCCURS 7 TIMES                               
001200                             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 PRAVCOST             OCCURS 7 TIMES                               
001500                             PIC S9(7)V9(2)      COMP-3.                  
001600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
001700*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
