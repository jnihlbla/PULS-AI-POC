000100 01  OBKVAN-W461S006.                                                     
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE KVANTANPASSN.          
000400*                                 TILL NOAC                               
000500     03 OBKVAN-SOR0-IDDISTR  PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 OBKVAN-SOR0-IDKUNDNR PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 OBKVAN-SOR0-IDRONR   PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 OBKVAN-SOR0-TIRODAT  PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 OBKVAN-SOR0-IDPTYP   PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 OBKVAN-SOR0-IDLOPNR  PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 OBKVAN-W461006.                                                   
001800*                                 ORDERBEKR.  KVANTANPASSNING             
001900*                                 TILL NOAC PT-006                        
002000        05 OBKVAN-IDPTYP     PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 OBKVAN-IDDISTR    PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 OBKVAN-IDKUNDNR   PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 OBKVAN-KDFRAKT    PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800        05 OBKVAN-IDORDNR    PIC S9(7)           COMP-3.                  
002900*                                 ORDERNR             IDORDNR-002         
003000        05 OBKVAN-KDLIDEL    PIC S9              COMP-3.                  
003100*                                 DEL AV LISTAN                           
003200        05 OBKVAN-IDDC       PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400        05 OBKVAN-IDARTNR    PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 OBKVAN-REKSIFFR   PIC S9              COMP-3.                  
003700*                                 KONTROLLSIFFRA                          
003800        05 OBKVAN-BEART      PIC X(25).                                   
003900*                                 ARTIKELBENÄMNING                        
004000        05 OBKVAN-BERADREF   PIC X(10).                                   
004100*                                 KUNDENS RADREFERENS                     
004200        05 OBKVAN-IDRONR     PIC S9(7)           COMP-3.                  
004300*                                 RESTORDERNUMMER      IDRONR-002         
004400        05 OBKVAN-TIRODAT    PIC S9(7)           COMP-3.                  
004500*                                 RESTORDERDATUM         (ÅÅMMDD)         
004600        05 OBKVAN-BEVOLREF   PIC X(10).                                   
004700*                                 VOLVO REFERENS                          
004800        05 OBKVAN-KDRESTR    PIC S9(3)           COMP-3.                  
004900*                                 RESTRIKTIONSKOD                         
005000        05 OBKVAN-KVBEART    PIC S9(7)           COMP-3.                  
005100*                                 BESTÄLLT ANTAL STYCKEN                  
005200        05 OBKVAN-KVBEART-Q  PIC S9(7)           COMP-3.                  
005300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
005400        05 OBKVAN-KVQPACK-1  PIC S9(5)           COMP-3.                  
005500*                                 ANTAL I Q1 FÖRPACKNING                  
005600        05 OBKVAN-KDKVBRYT   PIC S9              COMP-3.                  
005700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005800        05 OBKVAN-KDDSP      PIC S9              COMP-3.                  
005900*                                 PÅVERKAN PÅ DSP                         
006000        05 OBKVAN-KDFAKTYP   PIC X.                                       
006100*                                 FAKTURATYP                              
006200        05 OBKVAN-TIAAMMDD   PIC S9(7)           COMP-3.                  
006300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
006400        05 OBKVAN-TIKLOCK    PIC S9(9)           COMP-3.                  
006500*                                 KLOCKSLAG (TTMMSSTH)                    
006600*** END COPY W461S006    LENGTH=124                                       
