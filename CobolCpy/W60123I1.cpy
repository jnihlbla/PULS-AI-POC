000100 01  REQU-W6I12301.                                                       
000200*                                 COPYTEXT FÖR REQU                       
000300*                                 W6I12301                                
000400     03 REQU-GROUP.                                                       
000500*                                 LINES                                   
000600        05 REQU-IDARTNR-KEY  PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900        05 REQU-IDLOPNRM-KEY PIC X(8).                                    
001000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001100*                                 (0VVDLLLLK)                             
001200*                                 SERIAL NO RECEIVING REPORT              
001300*                                 (0WWDLLLLC)                             
001400        05 REQU-IDLEVNR-KEY  PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001700        05 REQU-IDFS-KEY     PIC X(8).                                    
001800*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001900*                                 ADVICE NOTE NUMBER ODETTE               
002000        05 REQU-IDLBBET-KEY  PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200*                                 TRAILER NUMBER                          
002300        05 REQU-IDDC-KEY     PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600        05 REQU-ADINLOMR-PRT PIC X(4).                                    
002700*                                 PRINTERPLACERING                        
002800*                                 PLACE OF A PRINTER                      
002900     03 REQU-BEART           PIC X(25).                                   
003000*                                 ARTIKELBENÄMNING                        
003100*                                 PART DESCRIPTION                        
003200     03 REQU-KDLAGEMB        PIC X(4).                                    
003300*                                 EMBALLAGEBETECKNING                     
003400*                                 PACKINGNOTATION                         
003500     03 REQU-ADLAGOMR        PIC X(2).                                    
003600*                                 LAGEROMRÅDE                             
003700*                                 AREA                                    
003800     03 REQU-ADGANG          PIC X(2).                                    
003900*                                 GÅNG                                    
004000*                                 AISLE                                   
004100     03 REQU-ADPLATS         PIC X(5).                                    
004200*                                 LAGERPLATSNUMMER                        
004300*                                 LOCATION                                
004400     03 REQU-KDSORT          PIC X(2).                                    
004500*                                 SORT-KOD                                
004600*                                 UNIT OF MEASURE                         
004700     03 REQU-KDFARLIG-TXT    PIC X(10).                                   
004800     03 REQU-IDSPRAK         PIC X(2).                                    
004900*                                 2-STÄLLIG ISO SPRÅKKOD                  
005000*                                 2-LETTER ISO LANGUAGE CODE              
005100     03 REQU-KVRADER         PIC 9(5).                                    
005200*                                 ANTAL RADER                             
005300*                                 NUMBER OF LINES                         
005400     03 REQU-RAD             OCCURS 500 TIMES.                            
005500*                                 LINES                                   
005600        05 REQU-KDCMDVAL-INPUT                                            
005700                             PIC X(3).                                    
005800*                                 GENERELL KOMMANDOKOD                    
005900*                                 GENERAL COMMAND-CODE                    
006000        05 REQU-IDLOPNRM     PIC X(9).                                    
006100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
006200*                                 (0VVDLLLLK)                             
006300*                                 SERIAL NO RECEIVING REPORT              
006400*                                 (0WWDLLLLC)                             
006500        05 REQU-IDRADNR      PIC X(3).                                    
006600*                                 RADNUMMER                               
006700*                                 LINE NO                                 
006800        05 REQU-IDLEVNR      PIC X(5).                                    
006900*                                 LEVERANTÖRNUMMER                        
007000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007100        05 REQU-IDOKOLLI     PIC X(9).                                    
007200*                                 ODETTE KOLLINUMMER                      
007300*                                 ODETTE CASE NUMBER                      
007400        05 REQU-KDINLSTA     PIC X(3).                                    
007500*                                 SYSTEMSTATUS INLEVERANS                 
007600*                                 SYSTEM STATUS RECEIVING                 
007700*** END OF VILMAII-COPY LENGTH= 16105 BYTES                               
