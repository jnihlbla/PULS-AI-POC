000100 01  ETD-W911ETD.                                                         
000200*                                 LÄNKAREA TILL W911ETD                   
000300     03 ETD-IDARTNR-IN       PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 ETD-IDDISTR-IN       PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 ETD-IDKUNDNR-IN      PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 ETD-KDORDKL-IN       PIC S9              COMP-3.                  
001000*                                 ORDERKLASS                              
001100     03 ETD-KVBEART-IN       PIC S9(7)           COMP-3.                  
001200*                                 BESTÄLLT ANTAL STYCKEN                  
001300     03 ETD-KVAVBART         PIC S9(7)           COMP-3.                  
001400*                                 AVBOKAT ANTAL ARTIKLAR                  
001500     03 ETD-TIREGDAT         PIC 9(6).                                    
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700     03 ETD-TIDISPIN         PIC S9(7)           COMP-3.                  
001800*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
001900     03 ETD-TIKLAR           PIC S9(7)           COMP-3.                  
002000*                                 KLARDATUM          (ÅÅMMDD)             
002100     03 ETD-IDDC             PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 ETD-KDORDBEK         PIC 9(2).                                    
002400*                                 ORDERBEKRÄFTELSEKOD                     
002500     03 ETD-TEORDBEK         PIC X(70).                                   
002600*                                 ORDERBEKRÄFTELSETEXT                    
002700     03 ETD-TEORDBEK-ENG     PIC X(70).                                   
002800*                                 ORDERBEKRÄFTELSETEXT                    
002900     03 ETD-FLTPO1           PIC X.                                       
003000*                                 ARTIKELN GODKÄND FÖR TPO1               
003100     03 ETD-KVFRYSTI         PIC S9(3)           COMP-3.                  
003200*                                 FRYSTID FÖR TPO-ORDER                   
003300     03 ETD-BEART            PIC X(25).                                   
003400*                                 ARTIKELBENÄMNING                        
003500     03 ETD-BEART-ENG        PIC X(25).                                   
003600*                                 ARTIKELBENÄMNING                        
003700     03 ETD-KDSORT           PIC X(2).                                    
003800*                                 SORT-KOD                                
003900     03 ETD-IDARTNR-TILLK    PIC S9(9)           COMP-3.                  
004000*                                 TILLKOMMANDE ARTIKELNUMMER              
004100     03 ETD-IDMFSMED         PIC X(3).                                    
004200*                                 MFS MEDDELANDE NUMMER                   
004300*** END OF VILMAII-COPY LENGTH= 242 BYTES                                 
