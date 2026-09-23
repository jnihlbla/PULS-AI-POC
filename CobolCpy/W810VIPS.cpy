000100 01  W810VIPS.                                                            
000200*                                 POST FÖR VIPS                           
000300*                                                                         
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDLEVNR              PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 IDORDNR              PIC S9(5)           COMP-3.                  
000900*                                 ORDERNUMMER UTGÅR PD90                  
001000     03 IDRONR               PIC S9(5)           COMP-3.                  
001100*                                 RESTORDERNUMMER                         
001200     03 TIORDREG             PIC S9(7)           COMP-3.                  
001300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001400     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001500*                                 FRAKTSÄTT DC TILL KUND                  
001600     03 IDARTNR              PIC S9(11)          COMP-3.                  
001700*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
001800     03 REKSIFFR             PIC S9              COMP-3.                  
001900*                                 KONTROLLSIFFRA                          
002000     03 KVBEART              PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT ANTAL STYCKEN                  
002200     03 TIRODAT              PIC S9(7)           COMP-3.                  
002300*                                 RESTORDERDATUM         (ÅÅMMDD)         
002400     03 KDORDKL              PIC X.                                       
002500*                                 ORDERKLASS                              
002600     03 KDVIP                PIC X.                                       
002700*                                 VIP-KOD                                 
002800     03 KDSTATUS             PIC X.                                       
002900*                                 STATUS ERSÄTTNING                       
003000     03 KDFAKTYP             PIC X.                                       
003100*                                 FAKTURATYP                              
003200     03 BERADREF             PIC X(10).                                   
003300*                                 KUNDENS RADREFERENS                     
003400*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
