000100 01  RESP-W60121O1.                                                       
000200*                                 COPYTEXT FOR MOD W60121O1               
000300     03 RESP-GROUP.                                                       
000400*                                 LINES                                   
000500        05 RESP-IDARTNR-KEY  PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700        05 RESP-IDLOPNRM-KEY PIC X(8).                                    
000800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000900*                                 (0VVDLLLLK)                             
001000        05 RESP-IDLEVNR-KEY  PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200        05 RESP-IDFS-KEY     PIC X(8).                                    
001300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001400        05 RESP-IDLBBET-KEY  PIC X(12).                                   
001500*                                 LASTBÄRARBETECKNING                     
001600        05 RESP-IDDC-KEY     PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800        05 RESP-ADINLOMR-PRT-ATTR                                         
001900                             PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100        05 RESP-ADINLOMR-PRT PIC X(4).                                    
002200*                                 PRINTERPLACERING                        
002300     03 RESP-KVRADER         PIC 9(5).                                    
002400*                                 ANTAL RADER                             
002500     03 RESP-FLKLAR-TOT-KEY  PIC X.                                       
002600*                                 AVSLUTNINGSMARKERING                    
002700     03 RESP-IDARTNR-START   PIC 9(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 RESP-IDLEVNR-START   PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100     03 RESP-IDFS-START      PIC X(8).                                    
003200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003300     03 RESP-IDARTNR-NEXT    PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500     03 RESP-IDLEVNR-NEXT    PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700     03 RESP-IDFS-NEXT       PIC X(8).                                    
003800*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003900     03 RESP-NEXT            PIC X.                                       
004000     03 RESP-FLKLAR-BIL-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 RESP-FLKLAR-BIL      PIC X.                                       
004300*                                 AVSLUTNINGSMARKERING                    
004400     03 RESP-ADINLOMR-ATTR   PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 RESP-ADINLOMR        PIC X(4).                                    
004700*                                 INLEVERANSOMRÅDE                        
004800     03 RESP-TELOSSN1        PIC X(39).                                   
004900     03 RESP-TELOSSN2        PIC X(66).                                   
005000     03 RESP-BEPRTLST        PIC X(25).                                   
005100*                                 LOGISK LISTA+PRINTER BENÄMNING          
005200     03 RESP-FLKVROS         PIC X.                                       
005300*                                 RESTORDERSALDO                          
005400     03 RESP-FLKVAKAR        PIC X.                                       
005500*                                 FLAGGA KARANTÄN                         
005600     03 RESP-KDFARLIG        PIC X.                                       
005700*                                 KOD FÖR FARLIGT GODS                    
005800     03 RESP-LINE            OCCURS 500 TIMES.                            
005900*                                 LINES                                   
006000        05 RESP-KDCMDVAL-INPUT-LINE-ATTR                                  
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 RESP-KDCMDVAL-INPUT-LINE                                       
006400                             PIC X(3).                                    
006500*                                 GENERELL KOMMANDOKOD                    
006600        05 RESP-IDLEVNR-LINE-ATTR                                         
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 RESP-IDLEVNR-LINE PIC X(5).                                    
007000*                                 LEVERANTÖRNUMMER                        
007100        05 RESP-IDFS-LINE-ATTR                                            
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 RESP-IDFS-LINE    PIC X(8).                                    
007500*                                 FÖLJESEDELSNUMMER ENL ODETTE            
007600        05 RESP-IDARTNR-LINE-ATTR                                         
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 RESP-IDARTNR-LINE PIC X(8).                                    
008000*                                 ARTIKELNUMMER                           
008100        05 RESP-KVAVIS-TOT-LINE-ATTR                                      
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 RESP-KVAVIS-TOT-LINE                                           
008500                             PIC X(6).                                    
008600*                                 AVISERAT ANTAL                          
008700        05 RESP-KVKOLLI-LINE-ATTR                                         
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 RESP-KVKOLLI-LINE PIC X(4).                                    
009100*                                 ANTAL KOLLI                             
009200        05 RESP-KDLAGEMB-LINE-ATTR                                        
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500        05 RESP-KDLAGEMB-LINE                                             
009600                             PIC X(4).                                    
009700*                                 EMBALLAGEBETECKNING                     
009800        05 RESP-BEFT-LINE-ATTR                                            
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 RESP-BEFT-LINE    PIC X(2).                                    
010200*                                 FÖRPACKNINGSTYP                         
010300        05 RESP-ADINLOMR-NXT-LINE-ATTR                                    
010400                             PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600        05 RESP-ADINLOMR-NXT-LINE                                         
010700                             PIC X(4).                                    
010800*                                 INLEVERANSOMRÅDE NÄSTA                  
010900        05 RESP-KVAVIS-LINE-ATTR                                          
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200        05 RESP-KVAVIS-LINE  PIC X(6).                                    
011300*                                 AVISERAT ANTAL                          
011400        05 RESP-KVAVIS-PRIO-LINE-ATTR                                     
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700        05 RESP-KVAVIS-PRIO-LINE                                          
011800                             PIC X(6).                                    
011900*                                 AVISERAT ANTAL                          
012000        05 RESP-KVAVIS-KIT-LINE-ATTR                                      
012100                             PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300        05 RESP-KVAVIS-KIT-LINE                                           
012400                             PIC X(6).                                    
012500*                                 AVISERAT ANTAL                          
012600        05 RESP-ADTRDEST-KIT-LINE-ATTR                                    
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900        05 RESP-ADTRDEST-KIT-LINE                                         
013000                             PIC X(3).                                    
013100*                                 TRANSPORTDESTINATION SATSER             
013200        05 RESP-FLKVROS-LINE PIC X.                                       
013300*                                 RESTORDERSALDO                          
013400        05 RESP-FLKVAKAR-LINE                                             
013500                             PIC X.                                       
013600*                                 FLAGGA KARANTÄN                         
013700        05 RESP-KDFARLIG-LINE                                             
013800                             PIC X.                                       
013900*                                 KOD FÖR FARLIGT GODS                    
014000*** END OF VILMAII-COPY LENGTH= 47243 BYTES                               
