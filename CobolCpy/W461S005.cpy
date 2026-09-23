000100 01  OBTEXT-W461S005.                                                     
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE TEXT TILL NOAC         
000400     03 OBTEXT-SOR0-IDDISTR  PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 OBTEXT-SOR0-IDKUNDNR PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 OBTEXT-SOR0-IDRONR   PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 OBTEXT-SOR0-TIRODAT  PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 OBTEXT-SOR0-IDPTYP   PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 OBTEXT-SOR0-IDLOPNR  PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 OBTEXT-W461005.                                                   
001700*                                 ORDERBEKRÄFTELSE TEXT                   
001800*                                 TILL NOAC PT-005                        
001900        05 OBTEXT-IDPTYP     PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 OBTEXT-IDDISTR    PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 OBTEXT-IDKUNDNR   PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 OBTEXT-KDFRAKT    PIC S9(3)           COMP-3.                  
002600*                                 FRAKTSÄTT C1-C2 TILL KUND               
002700        05 OBTEXT-IDORDNR    PIC S9(7)           COMP-3.                  
002800*                                 ORDERNR             IDORDNR-002         
002900        05 OBTEXT-KDLIDEL    PIC S9              COMP-3.                  
003000*                                 DEL AV LISTAN                           
003100        05 OBTEXT-IDDC       PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300        05 OBTEXT-IDARTNR    PIC S9(9)           COMP-3.                  
003400*                                 ARTIKELNUMMER                           
003500        05 OBTEXT-REKSIFFR   PIC S9              COMP-3.                  
003600*                                 KONTROLLSIFFRA                          
003700        05 OBTEXT-BEART      PIC X(25).                                   
003800*                                 ARTIKELBENÄMNING                        
003900        05 OBTEXT-IDLOPNRE   PIC S9(3)           COMP-3.                  
004000*                                 LÖPNUMMER ERSÄTTNING                    
004100        05 OBTEXT-IDKORTNR   PIC S9(3)           COMP-3.                  
004200*                                 KORTNUMMER                              
004300*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
004400        05 OBTEXT-BERADREF   PIC X(10).                                   
004500*                                 KUNDENS RADREFERENS                     
004600        05 OBTEXT-IDRONR     PIC S9(7)           COMP-3.                  
004700*                                 RESTORDERNUMMER      IDRONR-002         
004800        05 OBTEXT-TIRODAT    PIC S9(7)           COMP-3.                  
004900*                                 RESTORDERDATUM         (ÅÅMMDD)         
005000        05 OBTEXT-BEVOLREF   PIC X(10).                                   
005100*                                 VOLVO REFERENS                          
005200        05 OBTEXT-KDRESTR    PIC S9(3)           COMP-3.                  
005300*                                 RESTRIKTIONSKOD                         
005400        05 OBTEXT-KDERS      PIC S9(3)           COMP-3.                  
005500*                                 ERSÄTTNINGSKOD                          
005600        05 OBTEXT-KVBEART    PIC S9(7)           COMP-3.                  
005700*                                 BESTÄLLT ANTAL STYCKEN                  
005800        05 OBTEXT-BEERS      PIC X(20).                                   
005900*                                 ERSÄTTNINGSTEXT                         
006000        05 OBTEXT-KDERSUP    PIC S9              COMP-3.                  
006100*                                 UPPDATERING AV IMPORTÖRS ARTREG         
006200*                                 VID ERSÄTTNING                          
006300        05 OBTEXT-KDKVBRYT   PIC S9              COMP-3.                  
006400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006500        05 OBTEXT-KDDSP      PIC S9              COMP-3.                  
006600*                                 PÅVERKAN PÅ DSP                         
006700        05 OBTEXT-KDFAKTYP   PIC X.                                       
006800*                                 FAKTURATYP                              
006900        05 OBTEXT-KDRO       PIC S9              COMP-3.                  
007000*                                 RESTORDERKOD PÅ INFORMATION             
007100*                                 TILL VR                                 
007200        05 FILLER            PIC X(4).                                    
007300*** END COPY W461S005    LENGTH=140                                       
