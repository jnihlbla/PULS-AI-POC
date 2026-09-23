000100 01  OBEEN-W461004.                                                       
000200*                                 ORDERBEKRÄFTELSE EJ ENTYDIG ERS         
000300*                                 TILL NOAC PT-004                        
000400     03 OBEEN-IDPTYP         PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 OBEEN-IDDISTR        PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 OBEEN-IDKUNDNR       PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 OBEEN-KDFRAKT        PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 OBEEN-IDORDNR        PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 OBEEN-KDLIDEL        PIC S9              COMP-3.                  
001500*                                 DEL AV LISTAN                           
001600     03 OBEEN-IDDC           PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 OBEEN-IDARTNR        PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 OBEEN-REKSIFFR       PIC S9              COMP-3.                  
002100*                                 KONTROLLSIFFRA                          
002200     03 OBEEN-BEART          PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 OBEEN-IDLOPNRE       PIC S9(3)           COMP-3.                  
002500*                                 LÖPNUMMER ERSÄTTNING                    
002600     03 OBEEN-IDKORTNR       PIC S9(3)           COMP-3.                  
002700*                                 KORTNUMMER                              
002800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002900     03 OBEEN-BERADREF       PIC X(10).                                   
003000*                                 KUNDENS RADREFERENS                     
003100     03 OBEEN-IDRONR         PIC S9(7)           COMP-3.                  
003200*                                 RESTORDERNUMMER      IDRONR-002         
003300     03 OBEEN-TIRODAT        PIC S9(7)           COMP-3.                  
003400*                                 RESTORDERDATUM         (ÅÅMMDD)         
003500     03 OBEEN-BEVOLREF       PIC X(10).                                   
003600*                                 VOLVO REFERENS                          
003700     03 OBEEN-KDRESTR        PIC S9(3)           COMP-3.                  
003800*                                 RESTRIKTIONSKOD                         
003900     03 OBEEN-KDERS          PIC S9(3)           COMP-3.                  
004000*                                 ERSÄTTNINGSKOD                          
004100     03 OBEEN-KVBEART        PIC S9(7)           COMP-3.                  
004200*                                 BESTÄLLT ANTAL STYCKEN                  
004300     03 OBEEN-IDARTNR-TILLK  PIC S9(9)           COMP-3.                  
004400*                                 TILLKOMMANDE ARTIKELNUMMER              
004500     03 OBEEN-REKSIFFR-TILLK PIC S9              COMP-3.                  
004600*                                 TILLKOMMANDE KONTROLLSIFFRA             
004700     03 OBEEN-KVBEART-TILLK  PIC S9(7)           COMP-3.                  
004800*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
004900     03 OBEEN-DIERS-KVOT     PIC S9(4)V9(3)      COMP-3.                  
005000*                                 KVOT MELLAN                             
005100*                                 DIERS-TILLK OCH DIERS-ERS               
005200     03 OBEEN-KDERSUP        PIC S9              COMP-3.                  
005300*                                 UPPDATERING AV IMPORTÖRS ARTREG         
005400*                                 VID ERSÄTTNING                          
005500     03 OBEEN-KDKVBRYT       PIC S9              COMP-3.                  
005600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005700     03 OBEEN-KDDSP          PIC S9              COMP-3.                  
005800*                                 PÅVERKAN PÅ DSP                         
005900     03 OBEEN-KDFAKTYP       PIC X.                                       
006000*                                 FAKTURATYP                              
006100     03 OBEEN-KDRO           PIC S9              COMP-3.                  
006200*                                 RESTORDERKOD PÅ INFORMATION             
006300*                                 TILL VR                                 
006400     03 FILLER               PIC X(4).                                    
006500*** END COPY W461004     LENGTH=113                                       
