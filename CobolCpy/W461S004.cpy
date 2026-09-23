000100 01  OBEEN-W461S004.                                                      
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE EJ ENTYDIG ERS         
000400*                                 TILL NOAC                               
000500     03 OBEEN-SOR0-IDDISTR   PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 OBEEN-SOR0-IDKUNDNR  PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 OBEEN-SOR0-IDRONR    PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 OBEEN-SOR0-TIRODAT   PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 OBEEN-SOR0-IDPTYP    PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 OBEEN-SOR0-IDLOPNR   PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 OBEEN-W461004.                                                    
001800*                                 ORDERBEKRÄFTELSE EJ ENTYDIG ERS         
001900*                                 TILL NOAC PT-004                        
002000        05 OBEEN-IDPTYP      PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 OBEEN-IDDISTR     PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 OBEEN-IDKUNDNR    PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 OBEEN-KDFRAKT     PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800        05 OBEEN-IDORDNR     PIC S9(7)           COMP-3.                  
002900*                                 ORDERNR             IDORDNR-002         
003000        05 OBEEN-KDLIDEL     PIC S9              COMP-3.                  
003100*                                 DEL AV LISTAN                           
003200        05 OBEEN-IDDC        PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400        05 OBEEN-IDARTNR     PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 OBEEN-REKSIFFR    PIC S9              COMP-3.                  
003700*                                 KONTROLLSIFFRA                          
003800        05 OBEEN-BEART       PIC X(25).                                   
003900*                                 ARTIKELBENÄMNING                        
004000        05 OBEEN-IDLOPNRE    PIC S9(3)           COMP-3.                  
004100*                                 LÖPNUMMER ERSÄTTNING                    
004200        05 OBEEN-IDKORTNR    PIC S9(3)           COMP-3.                  
004300*                                 KORTNUMMER                              
004400*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
004500        05 OBEEN-BERADREF    PIC X(10).                                   
004600*                                 KUNDENS RADREFERENS                     
004700        05 OBEEN-IDRONR      PIC S9(7)           COMP-3.                  
004800*                                 RESTORDERNUMMER      IDRONR-002         
004900        05 OBEEN-TIRODAT     PIC S9(7)           COMP-3.                  
005000*                                 RESTORDERDATUM         (ÅÅMMDD)         
005100        05 OBEEN-BEVOLREF    PIC X(10).                                   
005200*                                 VOLVO REFERENS                          
005300        05 OBEEN-KDRESTR     PIC S9(3)           COMP-3.                  
005400*                                 RESTRIKTIONSKOD                         
005500        05 OBEEN-KDERS       PIC S9(3)           COMP-3.                  
005600*                                 ERSÄTTNINGSKOD                          
005700        05 OBEEN-KVBEART     PIC S9(7)           COMP-3.                  
005800*                                 BESTÄLLT ANTAL STYCKEN                  
005900        05 OBEEN-IDARTNR-TILLK                                            
006000                             PIC S9(9)           COMP-3.                  
006100*                                 TILLKOMMANDE ARTIKELNUMMER              
006200        05 OBEEN-REKSIFFR-TILLK                                           
006300                             PIC S9              COMP-3.                  
006400*                                 TILLKOMMANDE KONTROLLSIFFRA             
006500        05 OBEEN-KVBEART-TILLK                                            
006600                             PIC S9(7)           COMP-3.                  
006700*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
006800        05 OBEEN-DIERS-KVOT  PIC S9(4)V9(3)      COMP-3.                  
006900*                                 KVOT MELLAN                             
007000*                                 DIERS-TILLK OCH DIERS-ERS               
007100        05 OBEEN-KDERSUP     PIC S9              COMP-3.                  
007200*                                 UPPDATERING AV IMPORTÖRS ARTREG         
007300*                                 VID ERSÄTTNING                          
007400        05 OBEEN-KDKVBRYT    PIC S9              COMP-3.                  
007500*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
007600        05 OBEEN-KDDSP       PIC S9              COMP-3.                  
007700*                                 PÅVERKAN PÅ DSP                         
007800        05 OBEEN-KDFAKTYP    PIC X.                                       
007900*                                 FAKTURATYP                              
008000        05 OBEEN-KDRO        PIC S9              COMP-3.                  
008100*                                 RESTORDERKOD PÅ INFORMATION             
008200*                                 TILL VR                                 
008300        05 FILLER            PIC X(4).                                    
008400*** END COPY W461S004    LENGTH=134                                       
