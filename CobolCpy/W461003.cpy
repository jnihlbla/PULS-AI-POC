000100 01  OBEN-W461003.                                                        
000200*                                 ORDERBEKRÄFTELSE  ENTYDIG ERS           
000300*                                 TILL NOAC PT-003                        
000400     03 OBEN-IDPTYP          PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 OBEN-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 OBEN-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 OBEN-KDFRAKT         PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 OBEN-IDORDNR         PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 OBEN-KDLIDEL         PIC S9              COMP-3.                  
001500*                                 DEL AV LISTAN                           
001600     03 OBEN-IDDC            PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 OBEN-IDARTNR         PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 OBEN-REKSIFFR        PIC S9              COMP-3.                  
002100*                                 KONTROLLSIFFRA                          
002200     03 OBEN-BEART           PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 OBEN-IDLOPNRE        PIC S9(3)           COMP-3.                  
002500*                                 LÖPNUMMER ERSÄTTNING                    
002600     03 OBEN-IDKORTNR        PIC S9(3)           COMP-3.                  
002700*                                 KORTNUMMER                              
002800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002900     03 OBEN-BERADREF        PIC X(10).                                   
003000*                                 KUNDENS RADREFERENS                     
003100     03 OBEN-IDRONR          PIC S9(7)           COMP-3.                  
003200*                                 RESTORDERNUMMER      IDRONR-002         
003300     03 OBEN-TIRODAT         PIC S9(7)           COMP-3.                  
003400*                                 RESTORDERDATUM         (ÅÅMMDD)         
003500     03 OBEN-BEVOLREF        PIC X(10).                                   
003600*                                 VOLVO REFERENS                          
003700     03 OBEN-KDRESTR         PIC S9(3)           COMP-3.                  
003800*                                 RESTRIKTIONSKOD                         
003900     03 OBEN-KDERS           PIC S9(3)           COMP-3.                  
004000*                                 ERSÄTTNINGSKOD                          
004100     03 OBEN-KVBEART         PIC S9(7)           COMP-3.                  
004200*                                 BESTÄLLT ANTAL STYCKEN                  
004300     03 OBEN-IDARTNR-TILLK   PIC S9(9)           COMP-3.                  
004400*                                 TILLKOMMANDE ARTIKELNUMMER              
004500     03 OBEN-REKSIFFR-TILLK  PIC S9              COMP-3.                  
004600*                                 TILLKOMMANDE KONTROLLSIFFRA             
004700     03 OBEN-BEART-TILLK     PIC X(15).                                   
004800*                                 BENÄMNING TILLKOMMANDE ARTIKEL          
004900     03 OBEN-KVBEART-TILLK   PIC S9(7)           COMP-3.                  
005000*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
005100     03 OBEN-DIERS-KVOT      PIC S9(4)V9(3)      COMP-3.                  
005200*                                 KVOT MELLAN                             
005300*                                 DIERS-TILLK OCH DIERS-ERS               
005400     03 OBEN-KDERSUP         PIC S9              COMP-3.                  
005500*                                 UPPDATERING AV IMPORTÖRS ARTREG         
005600*                                 VID ERSÄTTNING                          
005700     03 OBEN-KDKVBRYT        PIC S9              COMP-3.                  
005800*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005900     03 OBEN-KDDSP           PIC S9              COMP-3.                  
006000*                                 PÅVERKAN PÅ DSP                         
006100     03 OBEN-KDFAKTYP        PIC X.                                       
006200*                                 FAKTURATYP                              
006300     03 OBEN-KDRO            PIC S9              COMP-3.                  
006400*                                 RESTORDERKOD PÅ INFORMATION             
006500*                                 TILL VR                                 
006600     03 FILLER               PIC X(4).                                    
006700*** END COPY W461003     LENGTH=128                                       
