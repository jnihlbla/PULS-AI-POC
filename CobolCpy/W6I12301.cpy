000100 01  MID-W6I12301.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I12301                                
000400     03 MID-GROUP.                                                        
000500*                                 LINES                                   
000600        05 MID-IDARTNR-IN    PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900        05 MID-IDARTNR-UT    PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200        05 MID-IDLOPNRM-IN   PIC X(8).                                    
001300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001400*                                 (0VVDLLLLK)                             
001500*                                 SERIAL NO RECEIVING REPORT              
001600*                                 (0WWDLLLLC)                             
001700        05 MID-IDLOPNRM-UT   PIC X(8).                                    
001800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001900*                                 (0VVDLLLLK)                             
002000*                                 SERIAL NO RECEIVING REPORT              
002100*                                 (0WWDLLLLC)                             
002200        05 MID-IDLEVNR-IN    PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER                         
002500        05 MID-IDLEVNR-UT    PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700*                                 SUPPLIER NUMBER                         
002800        05 MID-IDFS-IN       PIC X(8).                                    
002900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003000*                                 ADVICE NOTE NUMBER ODETTE               
003100        05 MID-IDFS-UT       PIC X(8).                                    
003200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003300*                                 ADVICE NOTE NUMBER ODETTE               
003400        05 MID-IDLBBET-IN    PIC X(12).                                   
003500*                                 LASTBÄRARBETECKNING                     
003600*                                 TRAILER NUMBER                          
003700        05 MID-IDLBBET-UT    PIC X(12).                                   
003800*                                 LASTBÄRARBETECKNING                     
003900*                                 TRAILER NUMBER                          
004000        05 MID-IDDC-IN       PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200*                                 WAREHOUSE IDENTIFIER                    
004300        05 MID-IDDC-UT       PIC X(2).                                    
004400*                                 IDENTIFIERARE LAGER                     
004500*                                 WAREHOUSE IDENTIFIER                    
004600        05 MID-ADINLOMR-PRT  PIC X(4).                                    
004700*                                 PRINTERPLACERING                        
004800*                                 PLACE OF A PRINTER                      
004900     03 MID-BEART            PIC X(25).                                   
005000*                                 ARTIKELBENÄMNING                        
005100*                                 PART DESCRIPTION                        
005200     03 MID-KDLAGEMB         PIC X(4).                                    
005300*                                 EMBALLAGEBETECKNING                     
005400*                                 PACKINGNOTATION                         
005500     03 MID-ADLAGOMR         PIC X(2).                                    
005600*                                 LAGEROMRÅDE                             
005700*                                 AREA                                    
005800     03 MID-ADGANG           PIC X(2).                                    
005900*                                 GÅNG                                    
006000*                                 AISLE                                   
006100     03 MID-ADPLATS          PIC X(5).                                    
006200*                                 LAGERPLATSNUMMER                        
006300*                                 LOCATION                                
006400     03 MID-KDSORT           PIC X(2).                                    
006500*                                 SORT-KOD                                
006600*                                 UNIT OF MEASURE                         
006700     03 MID-KDFARLIG-TXT     PIC X(10).                                   
006800     03 MID-RAD              OCCURS 11 TIMES.                             
006900*                                 LINES                                   
007000        05 MID-KDCMDVAL-INPUT                                             
007100                             PIC X(3).                                    
007200*                                 GENERELL KOMMANDOKOD                    
007300*                                 GENERAL COMMAND-CODE                    
007400        05 MID-IDLOPNRM      PIC X(9).                                    
007500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007600*                                 (0VVDLLLLK)                             
007700*                                 SERIAL NO RECEIVING REPORT              
007800*                                 (0WWDLLLLC)                             
007900        05 MID-IDRADNR       PIC X(3).                                    
008000*                                 RADNUMMER                               
008100*                                 LINE NO                                 
008200        05 MID-IDLEVNR       PIC X(5).                                    
008300*                                 LEVERANTÖRNUMMER                        
008400*                                 SUPPLIER NUMBER                         
008500        05 MID-IDOKOLLI      PIC X(9).                                    
008600*                                 ODETTE KOLLINUMMER                      
008700*                                 ODETTE CASE NUMBER                      
008800        05 MID-KDINLSTA      PIC X(3).                                    
008900*                                 SYSTEMSTATUS INLEVERANS                 
009000*                                 SYSTEM STATUS RECEIVING                 
009100*** END OF VILMAII-COPY LENGTH= 494 BYTES                                 
