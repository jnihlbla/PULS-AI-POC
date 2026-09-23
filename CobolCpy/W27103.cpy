000100 01  W27103.                                                              
000200*                                 INFO OM ARTIKLAR MED HÖG ORDER-         
000300*                                 INGÅNG, SDC, DAG. FIL TILL MEMO         
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
000900*                                 PERIODBEHOV REFILLING                   
001000     03 KVOI                 PIC S9(7)           COMP-3.                  
001100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
001200     03 KVLS                 PIC S9(7)           COMP-3.                  
001300*                                 LAGERSALDO                              
001400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001500*                                 ARTIKELSTANDARDPRIS                     
001600     03 LARMGRANS            PIC S9(7)           COMP-3.                  
001700     03 LARMORSAK            PIC X(2).                                    
001800     03 PRMATRL              PIC S9(7)V9(2)      COMP-3.                  
001900*                                 FAST PRIS UNDER LÖPANDE ÅR              
002000*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
