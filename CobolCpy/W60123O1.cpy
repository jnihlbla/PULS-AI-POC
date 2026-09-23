000100 01  RESP-W60123O1.                                                       
000200*                                 COPYTEXT FOR RESP W60123O1              
000300*                                                                         
000400     03 RESP-GROUP.                                                       
000500*                                 LINES                                   
000600        05 RESP-IDARTNR-KEY  PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800        05 RESP-IDLOPNRM-KEY PIC X(8).                                    
000900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001000*                                 (0VVDLLLLK)                             
001100        05 RESP-IDLEVNR-KEY  PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300        05 RESP-IDFS-KEY     PIC X(8).                                    
001400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001500        05 RESP-IDLBBET-KEY  PIC X(12).                                   
001600*                                 LASTBÄRARBETECKNING                     
001700        05 RESP-IDDC-KEY     PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900        05 RESP-ADINLOMR-PRT-ATTR                                         
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200        05 RESP-ADINLOMR-PRT PIC X(4).                                    
002300*                                 PRINTERPLACERING                        
002400     03 RESP-BEART           PIC X(25).                                   
002500*                                 ARTIKELBENÄMNING                        
002600     03 RESP-KDLAGEMB        PIC X(4).                                    
002700*                                 EMBALLAGEBETECKNING                     
002800     03 RESP-ADLAGOMR        PIC X(2).                                    
002900*                                 LAGEROMRÅDE                             
003000     03 RESP-ADGANG          PIC X(2).                                    
003100*                                 GÅNG                                    
003200     03 RESP-ADPLATS         PIC X(5).                                    
003300*                                 LAGERPLATSNUMMER                        
003400     03 RESP-KDSORT          PIC X(2).                                    
003500*                                 SORT-KOD                                
003600     03 RESP-KDFARLIG-TXT    PIC X(10).                                   
003700     03 RESP-KVRADER         PIC 9(5).                                    
003800*                                 ANTAL RADER                             
003900     03 RESP-BEPRTLST        PIC X(25).                                   
004000*                                 LOGISK LISTA+PRINTER BENÄMNING          
004100     03 RESP-RAD             OCCURS 500 TIMES.                            
004200*                                 LINES                                   
004300        05 RESP-KDCMDVAL-INPUT-LINE-ATTR                                  
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 RESP-KDCMDVAL-INPUT-LINE                                       
004700                             PIC X(3).                                    
004800*                                 GENERELL KOMMANDOKOD                    
004900        05 RESP-IDLOPNRM-LINE                                             
005000                             PIC X(9).                                    
005100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005200*                                 (0VVDLLLLK)                             
005300        05 RESP-IDRADNR-LINE PIC X(3).                                    
005400*                                 RADNUMMER                               
005500        05 RESP-IDLEVNR-LINE PIC X(5).                                    
005600*                                 LEVERANTÖRNUMMER                        
005700        05 RESP-IDOKOLLI-LINE                                             
005800                             PIC X(9).                                    
005900*                                 ODETTE KOLLINUMMER                      
006000        05 RESP-KVINLART-LINE                                             
006100                             PIC Z(5)9.                                   
006200*                                 ANTAL I PARTIRAD                        
006300        05 RESP-ADINLOMR-LINE                                             
006400                             PIC X(4).                                    
006500*                                 INLEVERANSOMRÅDE                        
006600        05 RESP-IDINLVGN-LINE                                             
006700                             PIC X(3).                                    
006800*                                 VAGNSIDENTITET                          
006900        05 RESP-ADINLOMR-NXT-LINE                                         
007000                             PIC X(4).                                    
007100*                                 INLEVERANSOMRÅDE NÄSTA                  
007200        05 RESP-KDKLIPRI-LINE-ATTR                                        
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 RESP-KDKLIPRI-LINE                                             
007600                             PIC X.                                       
007700*                                 PRIORITETSKOD KOLLI                     
007800        05 RESP-FLSATS-LINE-ATTR                                          
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 RESP-FLSATS-LINE  PIC X.                                       
008200*                                 SATSARTIKEL                             
008300        05 RESP-KDINLSTA-LINE-ATTR                                        
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 RESP-KDINLSTA-LINE                                             
008700                             PIC X(3).                                    
008800*                                 SYSTEMSTATUS INLEVERANS                 
008900*** END OF VILMAII-COPY LENGTH= 29630 BYTES                               
