000100 01  W225LI03.                                                            
000200*                                 LISTRECORD FÖR RANKING FÖR              
000300*                                 TOPP200                                 
000400*                                                                         
000500     03 GEMENSAMT.                                                        
000600        05 IDARTNR           PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800        05 KDCLPOST          PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERPOST                        
001000        05 IDLEVNR           PIC S9(5)           COMP-3.                  
001100*                                 LEVERANTÖRNUMMER                        
001200        05 SUROBEL           PIC S9(7)V9(2)      COMP-3.                  
001300*                                 RESTORDERVÄRDE STANDARDPRIS             
001400        05 KVRORAD           PIC S9(7)V9(2)      COMP-3.                  
001500*                                 RESTNOTERADE RADER  KVRORAD-003         
001600        05 TIRODAT-ORDER     PIC S9(5)           COMP-3.                  
001700*                                               TIRODAT-ORDER-002         
001800*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
001900        05 BEART-SVE         PIC X(25).                                   
002000*                                 SVENSK ARTIKELBENÄMNING                 
002100        05 IDANSK            PIC S9(3)           COMP-3.                  
002200*                                 ANSKAFFARNUMMER                         
002300        05 KVAVIS-LEVBESK-1  PIC S9(7)           COMP-3.                  
002400*                                 AVISERAT ANTAL                          
002500        05 TIAVIDAT-LEVBESK-1                                             
002600                             PIC S9(5)           COMP-3.                  
002700*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
002800     03 C1-INFO.                                                          
002900        05 KDPRIO-C1         PIC S9              COMP-3.                  
003000*                                 PRIORITETSKOD                           
003100        05 SUROBEL-C1        PIC S9(7)V9(2)      COMP-3.                  
003200*                                 RESTORDERVÄRDE STANDARDPRIS             
003300        05 KVRORAD-C1        PIC S9(7)V9(2)      COMP-3.                  
003400*                                 RESTNOTERADE RADER  KVRORAD-003         
003500        05 KVROS-C1          PIC S9(7)           COMP-3.                  
003600*                                 RESTORDERSALDO                          
003700        05 TIRODAT-ORDER-C1  PIC S9(5)           COMP-3.                  
003800*                                               TIRODAT-ORDER-002         
003900*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
004000        05 KVAKS-C1          PIC S9(7)           COMP-3.                  
004100*                                 ANKOMSTSALDO                            
004200        05 KVAKS-F-C1        PIC S9(7)           COMP-3.                  
004300*                                 DEL AV AKS TILL ANDRA CLAGRET           
004400        05 KVAKS-E-C1        PIC S9(7)           COMP-3.                  
004500*                                 DEL AV EFR TILL ANDRA CLAGRET           
004600     03 C2-INFO.                                                          
004700        05 KDPRIO-C2         PIC S9              COMP-3.                  
004800*                                 PRIORITETSKOD                           
004900        05 SUROBEL-C2        PIC S9(7)V9(2)      COMP-3.                  
005000*                                 RESTORDERVÄRDE STANDARDPRIS             
005100        05 KVRORAD-C2        PIC S9(7)V9(2)      COMP-3.                  
005200*                                 RESTNOTERADE RADER  KVRORAD-003         
005300        05 KVROS-C2          PIC S9(7)           COMP-3.                  
005400*                                 RESTORDERSALDO                          
005500        05 TIRODAT-ORDER-C2  PIC S9(5)           COMP-3.                  
005600*                                               TIRODAT-ORDER-002         
005700*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
005800        05 KVAKS-C2          PIC S9(7)           COMP-3.                  
005900*                                 ANKOMSTSALDO                            
006000        05 KVAKS-F-C2        PIC S9(7)           COMP-3.                  
006100*                                 DEL AV AKS TILL ANDRA CLAGRET           
006200        05 KVAKS-E-C2        PIC S9(7)           COMP-3.                  
006300*                                 DEL AV EFR TILL ANDRA CLAGRET           
006400*** END COPY W225LI03C0  LENGTH=116                                       
