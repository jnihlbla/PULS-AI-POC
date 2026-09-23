000100 01  OBEN-W461S003.                                                       
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE ENTYDIG ERS.           
000400*                                 TILL NOAC                               
000500     03 OBEN-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 OBEN-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 OBEN-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 OBEN-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 OBEN-SOR0-IDPTYP     PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 OBEN-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 OBEN-W461003.                                                     
001800*                                 ORDERBEKRÄFTELSE  ENTYDIG ERS           
001900*                                 TILL NOAC PT-003                        
002000        05 OBEN-IDPTYP       PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 OBEN-IDDISTR      PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 OBEN-IDKUNDNR     PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 OBEN-KDFRAKT      PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800        05 OBEN-IDORDNR      PIC S9(7)           COMP-3.                  
002900*                                 ORDERNR             IDORDNR-002         
003000        05 OBEN-KDLIDEL      PIC S9              COMP-3.                  
003100*                                 DEL AV LISTAN                           
003200        05 OBEN-IDDC         PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400        05 OBEN-IDARTNR      PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 OBEN-REKSIFFR     PIC S9              COMP-3.                  
003700*                                 KONTROLLSIFFRA                          
003800        05 OBEN-BEART        PIC X(25).                                   
003900*                                 ARTIKELBENÄMNING                        
004000        05 OBEN-IDLOPNRE     PIC S9(3)           COMP-3.                  
004100*                                 LÖPNUMMER ERSÄTTNING                    
004200        05 OBEN-IDKORTNR     PIC S9(3)           COMP-3.                  
004300*                                 KORTNUMMER                              
004400*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
004500        05 OBEN-BERADREF     PIC X(10).                                   
004600*                                 KUNDENS RADREFERENS                     
004700        05 OBEN-IDRONR       PIC S9(7)           COMP-3.                  
004800*                                 RESTORDERNUMMER      IDRONR-002         
004900        05 OBEN-TIRODAT      PIC S9(7)           COMP-3.                  
005000*                                 RESTORDERDATUM         (ÅÅMMDD)         
005100        05 OBEN-BEVOLREF     PIC X(10).                                   
005200*                                 VOLVO REFERENS                          
005300        05 OBEN-KDRESTR      PIC S9(3)           COMP-3.                  
005400*                                 RESTRIKTIONSKOD                         
005500        05 OBEN-KDERS        PIC S9(3)           COMP-3.                  
005600*                                 ERSÄTTNINGSKOD                          
005700        05 OBEN-KVBEART      PIC S9(7)           COMP-3.                  
005800*                                 BESTÄLLT ANTAL STYCKEN                  
005900        05 OBEN-IDARTNR-TILLK                                             
006000                             PIC S9(9)           COMP-3.                  
006100*                                 TILLKOMMANDE ARTIKELNUMMER              
006200        05 OBEN-REKSIFFR-TILLK                                            
006300                             PIC S9              COMP-3.                  
006400*                                 TILLKOMMANDE KONTROLLSIFFRA             
006500        05 OBEN-BEART-TILLK  PIC X(15).                                   
006600*                                 BENÄMNING TILLKOMMANDE ARTIKEL          
006700        05 OBEN-KVBEART-TILLK                                             
006800                             PIC S9(7)           COMP-3.                  
006900*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
007000        05 OBEN-DIERS-KVOT   PIC S9(4)V9(3)      COMP-3.                  
007100*                                 KVOT MELLAN                             
007200*                                 DIERS-TILLK OCH DIERS-ERS               
007300        05 OBEN-KDERSUP      PIC S9              COMP-3.                  
007400*                                 UPPDATERING AV IMPORTÖRS ARTREG         
007500*                                 VID ERSÄTTNING                          
007600        05 OBEN-KDKVBRYT     PIC S9              COMP-3.                  
007700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
007800        05 OBEN-KDDSP        PIC S9              COMP-3.                  
007900*                                 PÅVERKAN PÅ DSP                         
008000        05 OBEN-KDFAKTYP     PIC X.                                       
008100*                                 FAKTURATYP                              
008200        05 OBEN-KDRO         PIC S9              COMP-3.                  
008300*                                 RESTORDERKOD PÅ INFORMATION             
008400*                                 TILL VR                                 
008500        05 FILLER            PIC X(4).                                    
008600*** END COPY W461S003    LENGTH=149                                       
