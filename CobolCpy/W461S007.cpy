000100 01  OBLAG-W461S007.                                                      
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE LAGERAVBOKN            
000400*                                 TILL NOAC                               
000500     03 OBLAG-SOR0-IDDISTR   PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 OBLAG-SOR0-IDKUNDNR  PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 OBLAG-SOR0-IDRONR    PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 OBLAG-SOR0-TIRODAT   PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 OBLAG-SOR0-IDPTYP    PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 OBLAG-SOR0-IDLOPNR   PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 OBLAG-W461007.                                                    
001800*                                 ORDERBEKR.  LAGERAVBOKNING              
001900*                                 TILL NOAC PT-007                        
002000        05 OBLAG-IDPTYP      PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 OBLAG-IDDISTR     PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 OBLAG-IDKUNDNR    PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 OBLAG-KDFRAKT     PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800        05 OBLAG-IDORDNR     PIC S9(7)           COMP-3.                  
002900*                                 ORDERNR             IDORDNR-002         
003000        05 OBLAG-KDLIDEL     PIC S9              COMP-3.                  
003100*                                 DEL AV LISTAN                           
003200        05 OBLAG-IDDC        PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400        05 OBLAG-IDARTNR     PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 OBLAG-REKSIFFR    PIC S9              COMP-3.                  
003700*                                 KONTROLLSIFFRA                          
003800        05 OBLAG-BEART       PIC X(25).                                   
003900*                                 ARTIKELBENÄMNING                        
004000        05 OBLAG-BERADREF    PIC X(10).                                   
004100*                                 KUNDENS RADREFERENS                     
004200        05 OBLAG-IDRONR      PIC S9(7)           COMP-3.                  
004300*                                 RESTORDERNUMMER      IDRONR-002         
004400        05 OBLAG-TIRODAT     PIC S9(7)           COMP-3.                  
004500*                                 RESTORDERDATUM         (ÅÅMMDD)         
004600        05 OBLAG-BEVOLREF    PIC X(10).                                   
004700*                                 VOLVO REFERENS                          
004800        05 OBLAG-KDRESTR     PIC S9(3)           COMP-3.                  
004900*                                 RESTRIKTIONSKOD                         
005000        05 OBLAG-KVBEART     PIC S9(7)           COMP-3.                  
005100*                                 BESTÄLLT ANTAL STYCKEN                  
005200        05 OBLAG-KVAVBART    PIC S9(7)           COMP-3.                  
005300*                                 AVBOKAT ANTAL ARTIKLAR                  
005400        05 OBLAG-KVRO        PIC S9(7)           COMP-3.                  
005500*                                 ANTAL RESTNOTERADE ARTIKLAR             
005600        05 OBLAG-TIORDREG    PIC S9(7)           COMP-3.                  
005700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005800        05 OBLAG-KDKVBRYT    PIC S9              COMP-3.                  
005900*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006000        05 OBLAG-KDDSP       PIC S9              COMP-3.                  
006100*                                 PÅVERKAN PÅ DSP                         
006200        05 OBLAG-KDFAKTYP    PIC X.                                       
006300*                                 FAKTURATYP                              
006400        05 OBLAG-TIDISPIN    PIC S9(7)           COMP-3.                  
006500*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
006600        05 OBLAG-TIAAMMDD    PIC S9(7)           COMP-3.                  
006700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
006800        05 OBLAG-TIKLOCK     PIC S9(9)           COMP-3.                  
006900*                                 KLOCKSLAG (TTMMSSTH)                    
007000*** END COPY W461S007    LENGTH=133                                       
