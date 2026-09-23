000100 01  W27104.                                                              
000200*                                 INFO OM ARTIKLAR MED HÖG ORDER-         
000300*                                 INGÅNG, NDC, DAG. FIL TILL MEMO         
000400     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
000500*                                 PERSONKOD REFILLANSVARIG                
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
001100*                                 PERIODBEHOV REFILLING                   
001200     03 KVOI                 PIC S9(7)           COMP-3.                  
001300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
001400     03 KVLS                 PIC S9(7)           COMP-3.                  
001500*                                 LAGERSALDO                              
001600     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001700*                                 ARTIKELSTANDARDPRIS                     
001800     03 LARMGRANS            PIC S9(7)           COMP-3.                  
001900     03 LARMORSAK            PIC X(2).                                    
002000     03 KDREFTYP             PIC X.                                       
002100*                                 TYP AV REFILLORDER                      
002200     03 LARMTYP              PIC X(5).                                    
002300     03 PRMATRL              PIC S9(7)V9(2)      COMP-3.                  
002400*                                 FAST PRIS UNDER LÖPANDE ÅR              
002500*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
