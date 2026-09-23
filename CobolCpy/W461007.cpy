000100 01  OBLAG-W461007.                                                       
000200*                                 ORDERBEKR.  LAGERAVBOKNING              
000300*                                 TILL NOAC PT-007                        
000400     03 OBLAG-IDPTYP         PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 OBLAG-IDDISTR        PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 OBLAG-IDKUNDNR       PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 OBLAG-KDFRAKT        PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 OBLAG-IDORDNR        PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 OBLAG-KDLIDEL        PIC S9              COMP-3.                  
001500*                                 DEL AV LISTAN                           
001600     03 OBLAG-IDDC           PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 OBLAG-IDARTNR        PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 OBLAG-REKSIFFR       PIC S9              COMP-3.                  
002100*                                 KONTROLLSIFFRA                          
002200     03 OBLAG-BEART          PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 OBLAG-BERADREF       PIC X(10).                                   
002500*                                 KUNDENS RADREFERENS                     
002600     03 OBLAG-IDRONR         PIC S9(7)           COMP-3.                  
002700*                                 RESTORDERNUMMER      IDRONR-002         
002800     03 OBLAG-TIRODAT        PIC S9(7)           COMP-3.                  
002900*                                 RESTORDERDATUM         (ÅÅMMDD)         
003000     03 OBLAG-BEVOLREF       PIC X(10).                                   
003100*                                 VOLVO REFERENS                          
003200     03 OBLAG-KDRESTR        PIC S9(3)           COMP-3.                  
003300*                                 RESTRIKTIONSKOD                         
003400     03 OBLAG-KVBEART        PIC S9(7)           COMP-3.                  
003500*                                 BESTÄLLT ANTAL STYCKEN                  
003600     03 OBLAG-KVAVBART       PIC S9(7)           COMP-3.                  
003700*                                 AVBOKAT ANTAL ARTIKLAR                  
003800     03 OBLAG-KVRO           PIC S9(7)           COMP-3.                  
003900*                                 ANTAL RESTNOTERADE ARTIKLAR             
004000     03 OBLAG-TIORDREG       PIC S9(7)           COMP-3.                  
004100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004200     03 OBLAG-KDKVBRYT       PIC S9              COMP-3.                  
004300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004400     03 OBLAG-KDDSP          PIC S9              COMP-3.                  
004500*                                 PÅVERKAN PÅ DSP                         
004600     03 OBLAG-KDFAKTYP       PIC X.                                       
004700*                                 FAKTURATYP                              
004800     03 OBLAG-TIDISPIN       PIC S9(7)           COMP-3.                  
004900*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
005000     03 OBLAG-TIAAMMDD       PIC S9(7)           COMP-3.                  
005100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005200     03 OBLAG-TIKLOCK        PIC S9(9)           COMP-3.                  
005300*                                 KLOCKSLAG (TTMMSSTH)                    
005400*** END COPY W461007     LENGTH=112                                       
