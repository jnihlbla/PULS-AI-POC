000100 01  1154-WDGX1154.                                                       
000200*                                 TILLKOMMANDE ARTIKLAR                   
000300*                                 I SATSSTRUKTUR                          
000400*                                 NYCKEL = KDSEGKEY ="1"                  
000500     03 1154-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 1154-IDLEVNR         PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001100     03 1154-BELEVART        PIC X(30).                                   
001200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001300*                                 SUPPLIERS PART DESCRIPTION              
001400     03 1154-IDARTNR         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 1154-BEART-SVE       PIC X(25).                                   
001800*                                 SVENSK ARTIKELBENÄMNING                 
001900     03 1154-IDSTRTYP        PIC X.                                       
002000*                                 STRUKTURTYP                             
002100*                                 TYPE OF STRUCTURE                       
002200     03 1154-KDHOM           PIC S9              COMP-3.                  
002300*                                 HOMONYMKOD                              
002400*                                 HOMONYMOUS CODE                         
002500     03 1154-KDSORT          PIC X(2).                                    
002600*                                 SORT-KOD                                
002700*                                 UNIT OF MEASURE                         
002800     03 1154-REANTPSA        PIC S9(2)V9(3)      COMP-3.                  
002900*                                 ANTAL PER SATS                          
003000     03 1154-TESTRNOT        OCCURS 2 TIMES                               
003100                             PIC X(70).                                   
003200*                                 STRUKTURNOTERING                        
003300*                                 STRUCTURE INFORMATION                   
003400     03 FILLER               PIC X(29).                                   
003500*** END OF VILMAII-COPY LENGTH= 242 BYTES                                 
