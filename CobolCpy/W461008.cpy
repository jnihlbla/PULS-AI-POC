000100 01  OBSTOP-W461008.                                                      
000200*                                 ORDERBEKR.  STOPPADE RADER              
000300*                                 TILL NOAC PT-008                        
000400     03 OBSTOP-IDPTYP        PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 OBSTOP-IDDISTR       PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 OBSTOP-IDKUNDNR      PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 OBSTOP-KDFRAKT       PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 OBSTOP-IDORDNR       PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 OBSTOP-KDLIDEL       PIC S9              COMP-3.                  
001500*                                 DEL AV LISTAN                           
001600     03 OBSTOP-IDDC          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 OBSTOP-IDARTNR       PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 OBSTOP-REKSIFFR      PIC S9              COMP-3.                  
002100*                                 KONTROLLSIFFRA                          
002200     03 OBSTOP-BEART         PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 OBSTOP-BERADREF      PIC X(10).                                   
002500*                                 KUNDENS RADREFERENS                     
002600     03 OBSTOP-IDRONR        PIC S9(7)           COMP-3.                  
002700*                                 RESTORDERNUMMER      IDRONR-002         
002800     03 OBSTOP-TIRODAT       PIC S9(7)           COMP-3.                  
002900*                                 RESTORDERDATUM         (ÅÅMMDD)         
003000     03 OBSTOP-BEVOLREF      PIC X(10).                                   
003100*                                 VOLVO REFERENS                          
003200     03 OBSTOP-KDRESTR       PIC S9(3)           COMP-3.                  
003300*                                 RESTRIKTIONSKOD                         
003400     03 OBSTOP-KVBEART       PIC S9(7)           COMP-3.                  
003500*                                 BESTÄLLT ANTAL STYCKEN                  
003600     03 OBSTOP-TIORDREG      PIC S9(7)           COMP-3.                  
003700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003800     03 OBSTOP-KDKVBRYT      PIC S9              COMP-3.                  
003900*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004000     03 OBSTOP-KDDSP         PIC S9              COMP-3.                  
004100*                                 PÅVERKAN PÅ DSP                         
004200     03 OBSTOP-KDFAKTYP      PIC X.                                       
004300*                                 FAKTURATYP                              
004400     03 OBSTOP-TIAAMMDD      PIC S9(7)           COMP-3.                  
004500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004600     03 OBSTOP-TIKLOCK       PIC S9(9)           COMP-3.                  
004700*                                 KLOCKSLAG (TTMMSSTH)                    
004800*** END COPY W461008     LENGTH=100                                       
