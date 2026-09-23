000100 01  ERSE-W461S041-CTX.                                                   
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 RIE      POST TILL NOAC                 
000400     03 ERSE-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 ERSE-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 ERSE-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 ERSE-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 ERSE-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 ERSE-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 ERSE-W461RIEN-CTX.                                                
001700*                                 ORDERBEKRÄFTELSE EJ ENTYDIG ERS         
001800*                                 TILL IMPORTÖR PT-RIE                    
001900        05 ERSE-IDPTYP       PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 ERSE-IDDC         PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300        05 ERSE-IDARTNR      PIC 9(9).                                    
002400*                                 ARTIKELNUMMER                           
002500        05 ERSE-REKSIFFR     PIC 9.                                       
002600*                                 KONTROLLSIFFRA                          
002700        05 ERSE-IDLOPNRE     PIC 9(3).                                    
002800*                                 LÖPNUMMER ERSÄTTNING                    
002900        05 ERSE-IDKORTNR     PIC 9(2).                                    
003000*                                 KORTNUMMER                              
003100*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
003200        05 ERSE-BERADREF     PIC X(10).                                   
003300*                                 KUNDENS RADREFERENS                     
003400        05 ERSE-IDRONR       PIC 9(7).                                    
003500*                                 RESTORDERNUMMER      IDRONR-002         
003600        05 ERSE-BEVOLREF     PIC X(10).                                   
003700*                                 VOLVO REFERENS                          
003800        05 ERSE-KDRESTR      PIC 9(2).                                    
003900*                                 RESTRIKTIONSKOD                         
004000        05 ERSE-KDERS        PIC 9(2).                                    
004100*                                 ERSÄTTNINGSKOD                          
004200        05 ERSE-KVBEART      PIC 9(6).                                    
004300*                                 BESTÄLLT ANTAL STYCKEN                  
004400        05 ERSE-IDARTNR-TILLK                                             
004500                             PIC 9(9).                                    
004600*                                 TILLKOMMANDE ARTIKELNUMMER              
004700        05 ERSE-REKSIFFR-TILLK                                            
004800                             PIC 9.                                       
004900*                                 TILLKOMMANDE KONTROLLSIFFRA             
005000        05 ERSE-KVBEART-TILLK                                             
005100                             PIC 9(6).                                    
005200*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
005300        05 ERSE-DIERS-KVOT   PIC 9(3)V9(3).                               
005400*                                 KVOT MELLAN                             
005500*                                 DIERS-TILLK OCH DIERS-ERS               
005600        05 ERSE-KDERSUP      PIC 9.                                       
005700*                                 UPPDATERING AV IMPORTÖRS ARTREG         
005800*                                 VID ERSÄTTNING                          
005900        05 ERSE-KDDSP        PIC 9.                                       
006000*                                 PÅVERKAN PÅ DSP                         
006100*** END OF VILMAII-COPY LENGTH= 102 BYTES                                 
