000100 01  ERSF-W461S042-CTX.                                                   
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 RIF      POST TILL NOAC                 
000400     03 ERSF-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 ERSF-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 ERSF-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 ERSF-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 ERSF-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 ERSF-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 ERSF-W461RIFN-CTX.                                                
001700*                                 ORDERBEKRÄFTELSE TEXT                   
001800*                                 TILL IMPORTÖR PT-RIF                    
001900        05 ERSF-IDPTYP       PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 ERSF-IDDC         PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300        05 ERSF-IDARTNR      PIC 9(9).                                    
002400*                                 ARTIKELNUMMER                           
002500        05 ERSF-REKSIFFR     PIC 9.                                       
002600*                                 KONTROLLSIFFRA                          
002700        05 ERSF-IDLOPNRE     PIC 9(3).                                    
002800*                                 LÖPNUMMER ERSÄTTNING                    
002900        05 ERSF-IDKORTNR     PIC 9(2).                                    
003000*                                 KORTNUMMER                              
003100*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
003200        05 ERSF-BERADREF     PIC X(10).                                   
003300*                                 KUNDENS RADREFERENS                     
003400        05 ERSF-IDRONR       PIC 9(7).                                    
003500*                                 RESTORDERNUMMER      IDRONR-002         
003600        05 ERSF-BEVOLREF     PIC X(10).                                   
003700*                                 VOLVO REFERENS                          
003800        05 ERSF-KDRESTR      PIC 9(2).                                    
003900*                                 RESTRIKTIONSKOD                         
004000        05 ERSF-KDERS        PIC 9(2).                                    
004100*                                 ERSÄTTNINGSKOD                          
004200        05 ERSF-KVBEART      PIC 9(6).                                    
004300*                                 BESTÄLLT ANTAL STYCKEN                  
004400        05 ERSF-BEERS        PIC X(20).                                   
004500*                                 ERSÄTTNINGSTEXT                         
004600        05 ERSF-KDERSUP      PIC 9.                                       
004700*                                 UPPDATERING AV IMPORTÖRS ARTREG         
004800*                                 VID ERSÄTTNING                          
004900        05 ERSF-KDDSP        PIC 9.                                       
005000*                                 PÅVERKAN PÅ DSP                         
005100        05 ERSF-FILLERX1     PIC X.                                       
005200*** END OF VILMAII-COPY LENGTH= 101 BYTES                                 
