000100 01  OBSTOP-W461S008.                                                     
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE STOPPADE RADER         
000400*                                 TILL NOAC                               
000500     03 OBSTOP-SOR0-IDDISTR  PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 OBSTOP-SOR0-IDKUNDNR PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 OBSTOP-SOR0-IDRONR   PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 OBSTOP-SOR0-TIRODAT  PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 OBSTOP-SOR0-IDPTYP   PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 OBSTOP-SOR0-IDLOPNR  PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 OBSTOP-W461008.                                                   
001800*                                 ORDERBEKR.  STOPPADE RADER              
001900*                                 TILL NOAC PT-008                        
002000        05 OBSTOP-IDPTYP     PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 OBSTOP-IDDISTR    PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 OBSTOP-IDKUNDNR   PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 OBSTOP-KDFRAKT    PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800        05 OBSTOP-IDORDNR    PIC S9(7)           COMP-3.                  
002900*                                 ORDERNR             IDORDNR-002         
003000        05 OBSTOP-KDLIDEL    PIC S9              COMP-3.                  
003100*                                 DEL AV LISTAN                           
003200        05 OBSTOP-IDDC       PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400        05 OBSTOP-IDARTNR    PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 OBSTOP-REKSIFFR   PIC S9              COMP-3.                  
003700*                                 KONTROLLSIFFRA                          
003800        05 OBSTOP-BEART      PIC X(25).                                   
003900*                                 ARTIKELBENÄMNING                        
004000        05 OBSTOP-BERADREF   PIC X(10).                                   
004100*                                 KUNDENS RADREFERENS                     
004200        05 OBSTOP-IDRONR     PIC S9(7)           COMP-3.                  
004300*                                 RESTORDERNUMMER      IDRONR-002         
004400        05 OBSTOP-TIRODAT    PIC S9(7)           COMP-3.                  
004500*                                 RESTORDERDATUM         (ÅÅMMDD)         
004600        05 OBSTOP-BEVOLREF   PIC X(10).                                   
004700*                                 VOLVO REFERENS                          
004800        05 OBSTOP-KDRESTR    PIC S9(3)           COMP-3.                  
004900*                                 RESTRIKTIONSKOD                         
005000        05 OBSTOP-KVBEART    PIC S9(7)           COMP-3.                  
005100*                                 BESTÄLLT ANTAL STYCKEN                  
005200        05 OBSTOP-TIORDREG   PIC S9(7)           COMP-3.                  
005300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005400        05 OBSTOP-KDKVBRYT   PIC S9              COMP-3.                  
005500*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005600        05 OBSTOP-KDDSP      PIC S9              COMP-3.                  
005700*                                 PÅVERKAN PÅ DSP                         
005800        05 OBSTOP-KDFAKTYP   PIC X.                                       
005900*                                 FAKTURATYP                              
006000        05 OBSTOP-TIAAMMDD   PIC S9(7)           COMP-3.                  
006100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
006200        05 OBSTOP-TIKLOCK    PIC S9(9)           COMP-3.                  
006300*                                 KLOCKSLAG (TTMMSSTH)                    
006400*** END COPY W461S008    LENGTH=121                                       
