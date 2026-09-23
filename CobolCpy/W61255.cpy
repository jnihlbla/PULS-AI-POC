000100 01  SHIST-W61255.                                                        
000200*                                 DAGLIG FIL FÖR ARTRIKLAR                
000300*                                 SOM ÄR FAKTURERADE UNDER                
000400*                                 DET GÅGNA DYGNET.                       
000500     03 SHIST-IDDC           PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 SHIST-TIBERANK       PIC 9(6).                                    
000800*                                 BERÄKNAD ANKOMSTDATUM                   
000900     03 SHIST-KDFRAKT        PIC S9(3)           COMP-3.                  
001000*                                 FRAKTSÄTT DC TILL KUND                  
001100     03 SHIST-IDARTNR        PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 SHIST-BEART          PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500     03 SHIST-KVPB-REF       PIC S9(6)V9(1)      COMP-3.                  
001600*                                 PERIODBEHOV REFILLING                   
001700     03 SHIST-KVAVIS         PIC S9(7)           COMP-3.                  
001800*                                 AVISERAT ANTAL                          
001900     03 SHIST-KDVSOP         PIC S9(3)           COMP-3.                  
002000*                                 VSOP-KOD                                
002100     03 SHIST-VKART          PIC S9(7)           COMP-3.                  
002200*                                 ARTIKELVIKT (G)                         
002300     03 SHIST-VLARTNTO       PIC S9(8)V9(1)      COMP-3.                  
002400*                                 ARTIKELVOLYM NETTO (CM3)                
002500     03 SHIST-ADLAGOMR       PIC S9(3)           COMP-3.                  
002600*                                 LAGEROMRÅDE                             
002700     03 SHIST-IDORDNR5       PIC 9(5).                                    
002800*                                 ORDERNUMMER                             
002900     03 SHIST-IDFAKT         PIC S9(7)           COMP-3.                  
003000*                                 FAKTURANUMMER                           
003100     03 SHIST-IDKOLLI        PIC S9(5)           COMP-3.                  
003200*                                 KOLLINUMMER                             
003300*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
