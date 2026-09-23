000100 01  OBTEXT-W461005.                                                      
000200*                                 ORDERBEKRÄFTELSE TEXT                   
000300*                                 TILL NOAC PT-005                        
000400     03 OBTEXT-IDPTYP        PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 OBTEXT-IDDISTR       PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 OBTEXT-IDKUNDNR      PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 OBTEXT-KDFRAKT       PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 OBTEXT-IDORDNR       PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 OBTEXT-KDLIDEL       PIC S9              COMP-3.                  
001500*                                 DEL AV LISTAN                           
001600     03 OBTEXT-IDDC          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 OBTEXT-IDARTNR       PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 OBTEXT-REKSIFFR      PIC S9              COMP-3.                  
002100*                                 KONTROLLSIFFRA                          
002200     03 OBTEXT-BEART         PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 OBTEXT-IDLOPNRE      PIC S9(3)           COMP-3.                  
002500*                                 LÖPNUMMER ERSÄTTNING                    
002600     03 OBTEXT-IDKORTNR      PIC S9(3)           COMP-3.                  
002700*                                 KORTNUMMER                              
002800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002900     03 OBTEXT-BERADREF      PIC X(10).                                   
003000*                                 KUNDENS RADREFERENS                     
003100     03 OBTEXT-IDRONR        PIC S9(7)           COMP-3.                  
003200*                                 RESTORDERNUMMER      IDRONR-002         
003300     03 OBTEXT-TIRODAT       PIC S9(7)           COMP-3.                  
003400*                                 RESTORDERDATUM         (ÅÅMMDD)         
003500     03 OBTEXT-BEVOLREF      PIC X(10).                                   
003600*                                 VOLVO REFERENS                          
003700     03 OBTEXT-KDRESTR       PIC S9(3)           COMP-3.                  
003800*                                 RESTRIKTIONSKOD                         
003900     03 OBTEXT-KDERS         PIC S9(3)           COMP-3.                  
004000*                                 ERSÄTTNINGSKOD                          
004100     03 OBTEXT-KVBEART       PIC S9(7)           COMP-3.                  
004200*                                 BESTÄLLT ANTAL STYCKEN                  
004300     03 OBTEXT-BEERS         PIC X(20).                                   
004400*                                 ERSÄTTNINGSTEXT                         
004500     03 OBTEXT-KDERSUP       PIC S9              COMP-3.                  
004600*                                 UPPDATERING AV IMPORTÖRS ARTREG         
004700*                                 VID ERSÄTTNING                          
004800     03 OBTEXT-KDKVBRYT      PIC S9              COMP-3.                  
004900*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005000     03 OBTEXT-KDDSP         PIC S9              COMP-3.                  
005100*                                 PÅVERKAN PÅ DSP                         
005200     03 OBTEXT-KDFAKTYP      PIC X.                                       
005300*                                 FAKTURATYP                              
005400     03 OBTEXT-KDRO          PIC S9              COMP-3.                  
005500*                                 RESTORDERKOD PÅ INFORMATION             
005600*                                 TILL VR                                 
005700     03 FILLER               PIC X(4).                                    
005800*** END COPY W461005     LENGTH=119                                       
