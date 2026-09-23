000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL013430.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000400 DATE-WRITTEN.   MAJ  2006.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        REDIGERING OCH UTSKRIFT AV PACKUNDERLA                           
000900*        UTSKRIFT MED HJÄLP AV D&P                                        
001000*                                                                         
001100*                                                                         
001200*    LÄNKAREA :       WL013430                                            
001300*                                                                         
001400*    CHANGE LOG                                                           
001500*                                                                         
001600*                                                                         
001700 DATA DIVISION.                                                           
001800                                                                          
001900 WORKING-STORAGE SECTION.                                                 
002000                                                                          
002100 77  IDPGM                       PIC X(8)    VALUE 'WL013430'.            
002200 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
002300 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
002400 77  KDRC-DISPLAY                PIC Z(5).                                
002500                                                                          
002600 77  WS-CURRENT-SECTION          PIC X(16)   VALUE 'MAIN'.                
002700 77  WS-CURRENT-IMS-SECTION      PIC X(16)   VALUE SPACE.                 
002800                                                                          
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  YES                         PIC X       VALUE 'Y'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300 77  WS-OHUV-IDKUNDNR            PIC 9(7)    VALUE ZERO.                  
003400 77  WS-OHUV-IDDISTR             PIC 9(5)    VALUE ZERO.                  
003500 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
003600 77  WS-OHUV-KDORDKL             PIC 9(2)    VALUE ZERO.                  
003700 77  SPAR-IDORDER                PIC S9(9)   COMP-3.                      
003800 77  WS-DATUM-TID                PIC 9(11).                               
003900 77  WS-DATUM                    PIC 9(6).                                
004000 77  WS-TID                      PIC 9(6).                                
004100 77  WS-4010-IDPLKLST            PIC 9(3).                                
004200 77  WS-TAKF-TIHHMM              PIC 9(4).                                
004300 77  WS-KDCROSS                  PIC X(02)   VALUE SPACE.                 
004400 77  WS-PRC-KDKOLLI              PIC X(08)   VALUE SPACE.                 
004500 77  WS-VKTARA                   PIC S9(6)V9(1) VALUE ZERO.               
004600 77  WS-HEAD-VKORDNTO            PIC S9(6)V9(1) VALUE ZERO.               
004700 77  WS-HEAD-VKORDBTO            PIC S9(6)V9(1) VALUE ZERO.               
004800 77  ADD-1-HEKTO                 PIC S9(6)V9(1) VALUE 00000.1.            
004900 77  WS-TOTAL-VKORDNTO           PIC S9(6)V9(1) VALUE ZERO.               
005000 77   WS-TOTAL-VKORDNTO-TOT      PIC S9(6)V9(1) VALUE ZERO.               
005100                                                                          
005200 77  IX1                         PIC S9(9)   VALUE +0.                    
005300 77  IX2                         PIC S9(9)   VALUE +0.                    
005400 77  ORDER-IX                    PIC S9(9)   VALUE +0.                    
005500                                                                          
005600 01  WS-TID-TACD.                                                         
005700     03  WS-HOUR-TACD            PIC  9(2).                               
005800     03  WS-COLON-TACD           PIC  X(1) VALUE ':'.                     
005900     03  WS-MINUTE-TACD          PIC  9(2).                               
006000                                                                          
006100 01  WS-BARCODE.                                                          
006200     03  WS-BARCODE-DISTR        PIC  9(4).                               
006300     03  WS-BARCODE-KUNDNR       PIC  9(6).                               
006400     03  WS-BARCODE-ORDNR        PIC  9(7).                               
006500     03  WS-BARCODE-KOLLI        PIC  9(5).                               
006600*                                                                         
006700 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
006800*01  FILLER REDEFINES TEST-IDDISTR.                                       
006900***  03   NOCOPY WWDIST03.                                                
007000 01  FILLER REDEFINES TEST-IDDISTR.                                       
007100*    03   -COPY WWDIST05.                                                 
007200 01  FILLER REDEFINES TEST-IDDISTR.                                       
007300*    03   -COPY WWDIST08.                                                 
007400 01  FILLER REDEFINES TEST-IDDISTR.                                       
007500*    03   -COPY WWDIST13.                                                 
007600 01  FILLER REDEFINES TEST-IDDISTR.                                       
007700*    03   -COPY WWDIST34.                                                 
007800 01  FILLER REDEFINES TEST-IDDISTR.                                       
007900*  03     -COPY WWDIST85.                                                 
008000     SKIP2                                                                
008100*                                                                         
008200                                                                          
008300*01  -COPY WWDC99                                                         
008400                                                                          
008500                                                                          
008600******************************************************************        
008700*TABELL FÖR ATT LAGRA LDC VIP-ORDER INNAN UTSKRIFT PÅ SISTA SIDA *        
008800******************************************************************        
008900*                                                                         
009000 01    FILLER                    PIC X(8)   VALUE 'LDC-TAB '.             
009100 01    LDC-TAB-IX                PIC S9(3)  COMP-3.                       
009200 01    LDC-TAB-IX-MAX            PIC S9(3)  COMP-3 VALUE +100.            
009300 01    LDC-TOT-ANT-ART           PIC S9(3) COMP-3.                        
009400                                                                          
009500*LDC-GB                                                                   
009600 01 LDC-TABELL.                                                           
009700    03 LDC-TAB-VIP-ORDER         OCCURS 100.                              
009800       05   LDC-TAB-VIPID        PIC  X(10) VALUE SPACE.                  
009900       05   LDC-TAB-ANTAL-ART    PIC  9(3)  VALUE ZERO.                   
010000                                                                          
010100*************************************                                     
010200*  TABELL FÖR ATT LAGRA PICKUP-PRC  *                                     
010300*************************************                                     
010400                                                                          
010500 01  TAB-PICKUP-PRC.                                                      
010600     03     FILLER             OCCURS 10.                                 
010700       05   TAB-VLKOLGR        PIC S9(1)V9(3) COMP-3.                     
010800       05   TAB-SUVOLYM        PIC S9(2)V9(3) COMP-3.                     
010900       05   FILLER             OCCURS 99.                                 
011000         07 TAB-IDPRC          PIC  X(4).                                 
011100         07 TAB-IDPRODNR       PIC S9(7)      COMP-3.                     
011200         07 TAB-IDPLKLST       PIC S9(7)      COMP-3.                     
011300         07 TAB-VLORDNTO       PIC S9(4)V9(3) COMP-3.                     
011400                                                                          
011500***********************                                                   
011600*  ARBETSAREA FÖR PRC *                                                   
011700***********************                                                   
011800 01  W-SPAR-IDPRC.                                                        
011900*    03  -COPY WDGX4448   -PRE W-SPAR-                                    
012000                                                                          
012100****************************                                              
012200*  SPAR-AREA PLOCKSATS PU  *                                              
012300****************************                                              
012400 01  W-PU-PLOCKSATS.                                                      
012500*    03  -COPY WDGX4008   -PRE W-PU-                                      
012600                                                                          
012700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
012800 01  GENERAL-SUBPROGRAMS.                                                 
012900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
013300     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
013400                                                                          
013500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
013800                                                                          
013900*01  -COPY WZ01SEND                                                       
014000                                                                          
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'WTRAUTF8    '.        
014300                                                                          
014400*01  -COPY WTRAUTF8                                                       
014500                                                                          
014600     EJECT                                                                
014700 01  UT-AREA-START               PIC X(24)   VALUE                        
014800                                 'UT-AREA-START '.                        
014900 01  HDR-AREA.                                                            
015000*    03  -COPY WZ01REQU -PRE HDR-                                         
015100*    03  -COPY WZ04HDR                                                    
015200                                                                          
015300 01  DOC-HEAD-AREA.                                                       
015400*    03  -COPY WL01331                                                    
015500     EJECT                                                                
015600 01  DOC-LINE-AREA.                                                       
015700*    03  -COPY WL01332                                                    
015800     EJECT                                                                
015900 01  DOC-TOT-AREA.                                                        
016000*    03  -COPY WL01333                                                    
016100                                                                          
016200 01  NYCKLAR-TILL-DLI.                                                    
016300                                                                          
016400*                                                                         
016500     03  W-IDGMTREF-X.                                                    
016600         05  W-IDGMTREF          PIC X(17)        VALUE SPACE.            
016700*                                                                         
016800     03  W-4001-IDHTYP-X.                                                 
016900         05  W-4001-IDHTYP       PIC  X(4)  VALUE '4007'.                 
017000         05  W-4001-IDPRODNR     PIC  9(7).                               
017100         05  W-4001-IDPLKLST     PIC  9(3).                               
017200         05  W-4001-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
017300                                                                          
017400     03  W-4007-IDHTYP-X.                                                 
017500         05  W-4007-IDHTYP       PIC  X(4)  VALUE '4007'.                 
017600         05  W-4007-IDPRODNR     PIC  9(7).                               
017700         05  W-4007-IDPLKLST     PIC  9(3).                               
017800         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
017900                                                                          
018000     03  W-4010-IDHTYP-X.                                                 
018100         05  W-4010-IDORDER      PIC S9(7) COMP-3.                        
018200         05  W-4010-KDPRT        PIC  X(3).                               
018300         05  W-4010-KDSS-PU      PIC  X(1).                               
018400         05  W-4010-ADLAGOMR     PIC S9(3) COMP-3.                        
018500         05  W-4010-ADGANG       PIC S9(3) COMP-3.                        
018600         05  W-4010-ADPLATS      PIC S9(5) COMP-3.                        
018700         05  W-4010-IDARTNR      PIC S9(9) COMP-3.                        
018800         05  W-4010-IDLOPNR      PIC S9(3) COMP-3.                        
018900                                                                          
019000     03  W-4010-KDPRT-MAX-X.                                              
019100         05  W-4010-KDPRT-MAX    PIC  X(3) VALUE '999'.                   
019200                                                                          
019300     03  W-4732-IDHTYP-X.                                                 
019400         05  W-4732-IDHTYP       PIC  X(4)  VALUE '4732'.                 
019500         05  W-4732-KDFRAKT      PIC S9(3)  COMP-3.                       
019600         05  W-4732-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
019700                                                                          
019800     03  W-4447-IDHTYP-X.                                                 
019900         05  W-4447-IDHTYP       PIC  X(4)  VALUE '4447'.                 
020000         05  W-4447-IDDC         PIC  X(2).                               
020100         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
020200                                                                          
020300     03  W-4448-IDPRC-X.                                                  
020400         05  W-4448-IDPRC-KEY    PIC  X(4).                               
020500         05  W-4448-LOW-VALUE    PIC  X(1)  VALUE LOW-VALUE.              
020600                                                                          
020700     03  W-4535-IDHTYP-X.                                                 
020800         05  W-4535-IDHTYP       PIC  X(4)  VALUE '4535'.                 
020900         05  W-4535-KDFDKRAV     PIC S9(3)  COMP-3.                       
021000         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
021100                                                                          
021200     03  W-4536-IDSKYLT-X.                                                
021300         05  W-4536-IDSKYLT      PIC  X(3).                               
021400         05  W-4536-LOW-VALUE    PIC  X(2)  VALUE LOW-VALUE.              
021500                                                                          
021600     03  W-IDORDER-X.                                                     
021700         05  W-IDORDER           PIC S9(7) COMP-3.                        
021800                                                                          
021900     03  W-IDDC-X.                                                        
022000         05  W-IDDC              PIC X(2).                                
022100                                                                          
022200     03  W-IDGMT-X.                                                       
022300         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
022400         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
022500                                                                          
022600     03  W-WDQ301KY-MIN-X.                                                
022700         05  W-Q301-MIN-IDORDER  PIC S9(7) COMP-3.                        
022800         05  W-Q301-MIN-IDDC     PIC X(2).                                
022900         05  W-Q301-MIN-IDPRODNR PIC S9(7) COMP-3.                        
023000         05  W-Q301-MIN-IDPLKLST PIC S9(3) COMP-3.                        
023100                                                                          
023200     03  W-WDQ301KY-MAX-X.                                                
023300         05  W-Q301-MAX-IDORDER  PIC S9(7) COMP-3.                        
023400         05  W-Q301-MAX-IDDC     PIC X(2).                                
023500         05  W-Q301-MAX-IDPRODNR PIC S9(7) COMP-3.                        
023600         05  W-Q301-MAX-IDPLKLST PIC S9(3) COMP-3.                        
023700                                                                          
023800     03  W-IDDC-B6-X.                                                     
023900         05 W-IDDC-B6            PIC X(2).                                
024000                                                                          
024100     03  W-IDPRC-B6-X.                                                    
024200         05  W-IDPRC-B6          PIC X(4)    VALUE SPACE.                 
024300                                                                          
024400     03  W-WDE601-IDPRODNR-X.                                             
024500         05  W-IDPRODNR-WDE6     PIC S9(7)   VALUE ZERO COMP-3.           
024600                                                                          
024700     03  W-WDE611-IDKOLLI-X.                                              
024800         05  W-IDKOLLI-WDE6      PIC S9(5)   VALUE ZERO COMP-3.           
024900                                                                          
025000     03  W-IDARTNR-X.                                                     
025100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
025200                                                                          
025300     03  W-WDK501KY-X.                                                    
025400         05  W-KDKOLLI-K5        PIC X(8)    VALUE SPACE.                 
025500                                                                          
025600 01  FILLER               PIC X(16)   VALUE 'IMS-WS STATUS-WS'.           
025700                                                                          
025800 01  STATUS-WS                   PIC XX.                                  
025900     88  SEGMENT-FINNS           VALUE '  '.                              
026000     88  SEGMENT-SAKNAS          VALUE 'GE'.                              
026100 01  WDK5-STATUS-WS              PIC XX.                                  
026200     88  WDK5-SEGMENT-FOUND      VALUE '  '.                              
026300     88  WDK5-SEGMENT-MISSING    VALUE 'GE'.                              
026400     88  WDK5-SEGMENT-END        VALUE 'GB'.                              
026500                                                                          
026600                                                                          
026700 01  GODK-STATUSKODER.                                                    
026800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026900                                                                          
027000 01  SSA1                        PIC X(192).                              
027100 01  SSA2                        PIC X(96).                               
027200                                                                          
027300*    --- IMS FUNKTIONSKODER                                               
027400*01  -COPY W0003                                                          
027500                                                                          
027600 01  FILLER               PIC X(16)   VALUE 'WDGX4007'.                   
027700 01  DLI-IO-WDGX4007.                                                     
027800*    03  -COPY WDGX4007                                                   
027900                                                                          
028000 01  FILLER               PIC X(16)   VALUE 'WDGX4008'.                   
028100 01  DLI-IO-WDGX4008.                                                     
028200*    03  -COPY WDGX4008                                                   
028300                                                                          
028400 01  FILLER               PIC X(16)   VALUE 'WDGX4448'.                   
028500 01  DLI-IO-WDGX4448.                                                     
028600*    03  -COPY WDGX4448                                                   
028700                                                                          
028800 01  FILLER               PIC X(16)   VALUE 'WDGX4536'.                   
028900 01  DLI-IO-WDGX4536.                                                     
029000*    03  -COPY WDGX4536                                                   
029100                                                                          
029200 01  FILLER               PIC X(16)   VALUE 'WDGX4732'.                   
029300 01  DLI-IO-WDGX4732.                                                     
029400*    03  -COPY WDGX4732                                                   
029500                                                                          
029600 01  FILLER               PIC X(16)   VALUE 'WDGX4010'.                   
029700 01  DLI-IO-WDGX4010.                                                     
029800*    03  -COPY WDGX4010                                                   
029900                                                                          
030000 01  FILLER               PIC X(16)   VALUE 'WDB201  '.                   
030100 01  DLI-IO-WDB201.                                                       
030200*    03  -COPY WDB201                                                     
030300                                                                          
030400 01  FILLER               PIC X(16)   VALUE 'WDQ201-12'.                  
030500 01  DLI-IO-WDQ201-12.                                                    
030600*    03  -COPY WDQ201                                                     
030700*    03  -COPY WDQ212                                                     
030800                                                                          
030900 01  FILLER               PIC X(16)   VALUE 'WDQ301  '.                   
031000 01  DLI-IO-WDQ301.                                                       
031100*    03  -COPY WDQ301                                                     
031200                                                                          
031300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
031400 01  DLI-IO-WDB601.                                                       
031500*    03  -COPY WDB601                                                     
031600                                                                          
031700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB612'.                      
031800 01  DLI-IO-WDB612.                                                       
031900*    03  -COPY WDB612                                                     
032000     EJECT                                                                
032100                                                                          
032200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
032300 01  DLI-IO-WDE601.                                                       
032400*    03  -COPY WDE601                                                     
032500                                                                          
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
032700 01  DLI-IO-WDE611.                                                       
032800*    03  -COPY WDE611                                                     
032900                                                                          
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDI201'.                      
033100 01  DLI-IO-WDI201.                                                       
033200*    03  -COPY WDI201                                                     
033300                                                                          
033400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF502'.                      
033500 01  DLI-IO-WDF502.                                                       
033600*    03  -COPY WDF502                                                     
033700                                                                          
033800 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDK501'.                       
033900 01  DLI-IO-WDK501.                                                       
034000*  03    EMBB01   -COPY WDK501                                            
034100     EJECT                                                                
034200                                                                          
034300 LINKAGE SECTION.                                                         
034400*                                                                         
034500*01  -COPY WL013430                                                       
034600                                                                          
034700 01  DISTRDOC-PCB                PIC X.                                   
034800                                                                          
034900*01  -COPY W0008  -PRE 4007-                                              
035000     05  FILLER                  PIC X.                                   
035100                                                                          
035200*01  -COPY W0008  -PRE 4447-                                              
035300     05  FILLER                  PIC X.                                   
035400                                                                          
035500*01  -COPY W0008  -PRE 4535-                                              
035600     05  FILLER                  PIC X.                                   
035700                                                                          
035800*01  -COPY W0008  -PRE 4732-                                              
035900     05  FILLER                  PIC X.                                   
036000                                                                          
036100*01  -COPY W0008  -PRE WDB2-                                              
036200     05  FILLER                  PIC X.                                   
036300                                                                          
036400*01  -COPY W0008  -PRE WDQ2-                                              
036500     05  FILLER                  PIC X.                                   
036600                                                                          
036700*01  -COPY W0008  -PRE WDQ3-                                              
036800     05  FILLER                  PIC X.                                   
036900                                                                          
037000*01  -COPY W0008  -PRE WDB6-                                              
037100     05  FILLER                  PIC X.                                   
037200                                                                          
037300*01  -COPY W0008  -PRE WDE6-                                              
037400     05  FILLER                  PIC X.                                   
037500                                                                          
037600*01  -COPY W0008  -PRE WDI2-                                              
037700     05  FILLER                  PIC X.                                   
037800                                                                          
037900*01  -COPY W0008  -PRE WDF5-                                              
038000     05  FILLER                  PIC X.                                   
038100                                                                          
038200*01  -COPY W0008  -PRE WDK5-                                              
038300     05  FILLER                  PIC X.                                   
038400                                                                          
038500                                                                          
038600  PROCEDURE DIVISION USING 3430-WL013430 DISTRDOC-PCB                     
038700                           4007-PCB 4447-PCB 4535-PCB 4732-PCB            
038800                           WDB2-PCB                                       
038900                           WDQ2-PCB WDQ3-PCB WDB6-PCB WDE6-PCB            
039000                           WDI2-PCB WDF5-PCB WDK5-PCB.                    
039100                                                                          
039200  MAIN SECTION.                                                           
039300     PERFORM A-INIT                                                       
039400     PERFORM B-LAES-PLOCKSATS                                             
039500                                                                          
039600     IF SEGMENT-FINNS                                                     
039700        PERFORM S90-OPEN-DAP-SEND                                         
039800        PERFORM S91-PUT-DAP-HEADER                                        
039900                                                                          
040000        PERFORM C-BEHANDLA-PURADER                                        
040100        PERFORM D-UPPDAT-PLOCKSATS                                        
040200                                                                          
040300        PERFORM S95-CLOSE-DAP-SEND                                        
040400     END-IF                                                               
040500                                                                          
040600     MOVE ZERO TO RETURN-CODE                                             
040700     GOBACK.                                                              
040800                                                                          
040900                                                                          
041000 A-INIT SECTION.                                                          
041100     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
041200                                                                          
041300     MOVE ZERO                TO SPAR-IDORDER                             
041400     MOVE ZERO                TO LDC-TOT-ANT-ART                          
041500                                                                          
041600     MOVE '999'      TO W-4010-KDPRT-MAX                                  
041700     MOVE +0.1       TO ADD-1-HEKTO                                       
041800                                                                          
041900*    INITIERA LDC-TABELL                                                  
042000     MOVE +1 TO LDC-TAB-IX                                                
042100     PERFORM UNTIL LDC-TAB-IX = LDC-TAB-IX-MAX                            
042200        MOVE ZERO       TO LDC-TAB-ANTAL-ART(LDC-TAB-IX)                  
042300        MOVE HIGH-VALUE TO LDC-TAB-VIPID (LDC-TAB-IX)                     
042400        ADD +1 TO LDC-TAB-IX                                              
042500     END-PERFORM                                                          
042600                                                                          
042700*       --INITIALIZE D&P HEADER RECORD (WZ01REQU+WZ04HDR)                 
042800     MOVE 001                 TO HDR-REQU-IDMSGVER                        
042900     MOVE SPACE               TO HDR-REQU-KDPGMACT                        
043000     MOVE 3430-IDUSER         TO HDR-REQU-IDUSER                          
043100                                                                          
043200     IF 3430-IDTRANS = 'L138' OR 'A138'                                   
043300       MOVE 'CASE-PACK-SPEC'  TO HDR-IDOUTTYPE                            
043400     ELSE                                                                 
043500       MOVE 'PACKING-SPEC2'   TO HDR-IDOUTTYPE                            
043600     END-IF                                                               
043700     MOVE SPACE               TO HDR-IDOUTREC                             
043800     MOVE 3430-IDDC           TO HDR-IDOUTREC(1:2)                        
043900     MOVE 3430-IDUSER         TO HDR-IDOUTREC(3:8)                        
044000     MOVE 3430-IDLIST         TO HDR-IDLIST                               
044100     .                                                                    
044200                                                                          
044300 B-LAES-PLOCKSATS SECTION.                                                
044400     MOVE 'B-LAES-PLOCKSATS'  TO WS-CURRENT-SECTION                       
044500                                                                          
044600     MOVE 3430-IDPRODNR-KEY   TO W-4001-IDPRODNR                          
044700     MOVE 3430-IDPLKLST-KEY   TO W-4001-IDPLKLST                          
044800                                                                          
044900     PERFORM IMS-01-GHU-WDGX4008                                          
045000                                                                          
045100     IF SEGMENT-FINNS                                                     
045200        IF 4008-NYCKEL-GRP NOT = LOW-VALUE                                
045300           MOVE 4008-NYCKEL-GRP  TO W-4010-IDHTYP-X                       
045400           PERFORM IMS-02-GNP-WDGX4010-KVAL                               
045500                                                                          
045600           PERFORM S01-LAES-ORDERHUVUD                                    
045700        END-IF                                                            
045800     END-IF                                                               
045900     .                                                                    
046000                                                                          
046100 C-BEHANDLA-PURADER SECTION.                                              
046200     MOVE 'C-BEHANDLA-PURADER  '   TO WS-CURRENT-SECTION                  
046300                                                                          
046400     PERFORM IMS-03-GNP-WDGX4010-OKVAL                                    
046500     MOVE +1         TO ORDER-IX                                          
046600                                                                          
046700     PERFORM UNTIL SEGMENT-SAKNAS                                         
046800                                                                          
046900       IF SPAR-IDORDER NOT = 4010-IDORDER                                 
047000                                                                          
047100*         -- NO TOTAL THE FIRST TIME                                      
047200          IF SPAR-IDORDER > ZERO                                          
047300            PERFORM CC-TOTAL-IDORDER                                      
047400            PERFORM S94-PUT-DOC-TOT                                       
047500                                                                          
047600          END-IF                                                          
047700                                                                          
047800          PERFORM S01-LAES-ORDERHUVUD                                     
047900          PERFORM S02-LAES-KUND                                           
048000                                                                          
048100*         -- NEW LDC PRINTING                                             
048200          IF 3430-IDTRANS = 'L138' OR 'A138'                              
048300            PERFORM CF-SKAPA-KOLLIFLAGGA                                  
048400          END-IF                                                          
048500                                                                          
048600          PERFORM CA-SKAPA-PACKHUVUD                                      
048700          PERFORM S92-PUT-DOC-HEAD                                        
048800                                                                          
048900          MOVE 4010-IDORDER TO SPAR-IDORDER                               
049000          ADD +1            TO ORDER-IX                                   
049100       END-IF                                                             
049200                                                                          
049300       PERFORM CB-REDIGERA-DETALJRAD                                      
049400       PERFORM S93-PUT-DOC-LINE                                           
049500                                                                          
049600       PERFORM IMS-03-GNP-WDGX4010-OKVAL                                  
049700     END-PERFORM                                                          
049800                                                                          
049900     PERFORM CC-TOTAL-IDORDER                                             
050000     PERFORM S94-PUT-DOC-TOT                                              
050100     PERFORM CE-SKRIV-LDC-GB-INFO                                         
050200     .                                                                    
050300                                                                          
050400                                                                          
050500 CA-SKAPA-PACKHUVUD SECTION.                                              
050600     MOVE 'CA-SKAPA-PACKHUVUD '   TO WS-CURRENT-SECTION                   
050700                                                                          
050800     MOVE '1'   TO HEAD-IDAFPRCD                                          
050900                                                                          
051000*    PERFORM S01-LAES-ORDERHUVUD                                          
051100*    -- FLYTTAD TILL C-BEHANDLA-PURADER                                   
051200     PERFORM CAA-LAES-ORDERDEL                                            
051300     PERFORM CAB-LAES-FRAKTTEXT                                           
051400     PERFORM CAC-LAES-PRC                                                 
051500     PERFORM CAD-REDIGERA-PACKHUVUD                                       
051600     PERFORM CAE-READ-TACDIS-CASE-LABEL                                   
051700     .                                                                    
051800                                                                          
051900 CAA-LAES-ORDERDEL SECTION.                                               
052000     MOVE 'CAA-LAES-ORDERDEL'     TO WS-CURRENT-SECTION                   
052100                                                                          
052200                                                                          
052300     MOVE 4010-IDORDER      TO W-Q301-MIN-IDORDER                         
052400                               W-Q301-MAX-IDORDER                         
052500     MOVE '11'              TO W-Q301-MIN-IDDC                            
052600                               W-Q301-MAX-IDDC                            
052700     MOVE ZERO              TO W-Q301-MIN-IDPRODNR                        
052800                               W-Q301-MIN-IDPLKLST                        
052900     MOVE 9999999           TO W-Q301-MAX-IDPRODNR                        
053000     MOVE 999               TO W-Q301-MAX-IDPLKLST                        
053100                                                                          
053200     PERFORM IMS-12-GU-WDQ301-DC11                                        
053300                                                                          
053400     IF SEGMENT-FINNS                                                     
053500       MOVE 'CD' TO  WS-KDCROSS                                           
053600     ELSE                                                                 
053700       MOVE SPACE TO WS-KDCROSS                                           
053800     END-IF                                                               
053900                                                                          
054000                                                                          
054100                                                                          
054200     MOVE 4010-IDORDER  TO W-Q301-MIN-IDORDER                             
054300                           W-Q301-MAX-IDORDER                             
054400     MOVE 4010-IDDC     TO W-Q301-MIN-IDDC                                
054500                           W-Q301-MAX-IDDC                                
054600     MOVE 4010-IDPRODNR TO W-Q301-MIN-IDPRODNR                            
054700                           W-Q301-MAX-IDPRODNR                            
054800     MOVE 4010-IDPLKLST TO W-Q301-MIN-IDPLKLST                            
054900                           W-Q301-MAX-IDPLKLST                            
055000                                                                          
055100     PERFORM IMS-06-GU-WDQ301                                             
055200     .                                                                    
055300                                                                          
055400 CAB-LAES-FRAKTTEXT SECTION.                                              
055500     MOVE 'CAB-LAES-FRAKTTEXT'    TO WS-CURRENT-SECTION                   
055600                                                                          
055700     MOVE ARB-KDFRAKT                TO W-4732-KDFRAKT                    
055800     PERFORM IMS-07-GU-WDGX4732                                           
055900     IF SEGMENT-FINNS                                                     
056000        MOVE FRAKT-BEFRAKT (1)       TO HEAD-BEFRAKT                      
056100     ELSE                                                                 
056200        MOVE 'FREIGHT TEXT MISSING'  TO HEAD-BEFRAKT                      
056300     END-IF                                                               
056400     .                                                                    
056500                                                                          
056600 CAC-LAES-PRC SECTION.                                                    
056700     MOVE 'CAC-LAES-PRC      '    TO WS-CURRENT-SECTION                   
056800                                                                          
056900     MOVE 1        TO IX1                                                 
057000     PERFORM UNTIL IX1 > 10                                               
057100        MOVE 0     TO TAB-VLKOLGR  (IX1)                                  
057200                      TAB-SUVOLYM  (IX1)                                  
057300        MOVE 1     TO IX2                                                 
057400        PERFORM UNTIL IX2 > 99                                            
057500           MOVE SPACE    TO TAB-IDPRC    (IX1 IX2)                        
057600           MOVE 0        TO TAB-IDPRODNR (IX1 IX2)                        
057700                            TAB-IDPLKLST (IX1 IX2)                        
057800                            TAB-VLORDNTO (IX1 IX2)                        
057900           ADD 1      TO IX2                                              
058000        END-PERFORM                                                       
058100        ADD 1      TO IX1                                                 
058200     END-PERFORM                                                          
058300                                                                          
058400     MOVE 4010-IDDC     TO W-4447-IDDC                                    
058500     MOVE 4010-IDPRC    TO W-4448-IDPRC-KEY                               
058600     PERFORM IMS-08-GU-WDGX4448                                           
058700     IF SEGMENT-FINNS                                                     
058800        MOVE DLI-IO-WDGX4448 TO W-SPAR-IDPRC                              
058900        IF W-SPAR-4448-KDPRCTYP = 2                                       
059000           MOVE 1 TO IX1                                                  
059100           PERFORM UNTIL IX1 > 10                                         
059200              IF W-SPAR-4448-IDPRC-SUB (IX1) NOT = SPACE                  
059300                 MOVE W-SPAR-4448-IDPRC-SUB (IX1)                         
059400                                         TO W-4448-IDPRC-KEY              
059500                 PERFORM IMS-08-GU-WDGX4448                               
059600                 IF SEGMENT-FINNS                                         
059700                    MOVE 4448-VLKOLGR TO TAB-VLKOLGR (IX1)                
059800                 END-IF                                                   
059900              END-IF                                                      
060000              ADD 1 TO IX1                                                
060100           END-PERFORM                                                    
060200        END-IF                                                            
060300     END-IF                                                               
060400     .                                                                    
060500                                                                          
060600 CAD-REDIGERA-PACKHUVUD SECTION.                                          
060700     MOVE 'CAD-REDIGERA-PACKHUVUD'    TO WS-CURRENT-SECTION               
060800                                                                          
060900     PERFORM CADA-REDIGERA-HRAD0-HRAD3                                    
061000     PERFORM CADB-REDIGERA-HRAD4                                          
061100                                                                          
061200     MOVE OHUV-IDDISTR     TO TEST-IDDISTR                                
061300                                                                          
061400     PERFORM CADE-ADDRESS-DATA                                            
061500                                                                          
061600     PERFORM CADC-REDIGERA-HRAD7                                          
061700     PERFORM CADE-REDIGERA-HRAD8                                          
061800     PERFORM CADD-REDIGERA-HRAD9                                          
061900                                                                          
062000*GOODS MARKING                                                            
062100     IF  ARB-BEGMRK = SPACE                                               
062200       MOVE SPACE                     TO HEAD-BEGMRK-RAD1                 
062300       MOVE SPACE                     TO HEAD-BEGMRK-RAD2                 
062400     ELSE                                                                 
062500        MOVE ARB-BEGMRK-RAD1          TO HEAD-BEGMRK-RAD1                 
062600        MOVE ARB-BEGMRK-RAD2          TO HEAD-BEGMRK-RAD2                 
062700     END-IF                                                               
062800                                                                          
062900*WAREHOUSE INSTRUCTION                                                    
063000     IF  OHUV-BELAGINS-GRP = SPACE                                        
063100       MOVE SPACE                     TO HEAD-BELAGINS-DEL1               
063200       MOVE SPACE                     TO HEAD-BELAGINS-DEL2               
063300     ELSE                                                                 
063400        IF OHUV-BELAGINS-DEL1 = SPACE                                     
063500           MOVE OHUV-BELAGINS-DEL2    TO HEAD-BELAGINS-DEL1               
063600        ELSE                                                              
063700           MOVE OHUV-BELAGINS-DEL1    TO HEAD-BELAGINS-DEL1               
063800           IF OHUV-BELAGINS-DEL2 NOT = SPACE                              
063900                                                                          
064000             MOVE OHUV-BELAGINS-DEL2  TO HEAD-BELAGINS-DEL2               
064100           END-IF                                                         
064200        END-IF                                                            
064300     END-IF                                                               
064400     .                                                                    
064500                                                                          
064600 CADA-REDIGERA-HRAD0-HRAD3 SECTION.                                       
064700     MOVE 'CADA-REDIGERA-HRAD0-HRAD3' TO WS-CURRENT-SECTION               
064800                                                                          
064900     MOVE OHUV-IDDISTR         TO HEAD-IDDISTR                            
065000     MOVE OHUV-IDKUNDNR        TO HEAD-IDKUNDNR                           
065100     MOVE 4010-IDKUNDRF        TO HEAD-IDKUNDRF                           
065200     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
065300                                                                          
065400     INSPECT HEAD-IDKUNDRF REPLACING LEADING ZERO BY SPACE                
065500     IF HEAD-IDKUNDRF = SPACE                                             
065600        MOVE '      0' TO HEAD-IDKUNDRF                                   
065700     END-IF                                                               
065800     MOVE OHUV-KDORDKL      TO HEAD-KDORDKL                               
065900     MOVE 4010-IDBORD       TO HEAD-IDBORD                                
066000     MOVE 4010-IDUSER       TO HEAD-IDUSER                                
066100     INSPECT HEAD-IDUSER REPLACING LEADING ZERO BY SPACE                  
066200     MOVE 4010-IDPRODNR     TO HEAD-IDPRODNR                              
066300     MOVE ODEL-IDPLKLST     TO HEAD-IDPLKLST                              
066400                                                                          
066500*    -- MOVED TO PARENT SECTION                                           
066600*    MOVE OHUV-BEGMT-RAD1   TO HEAD-BEGMT-RAD1                            
066700                                                                          
066800*    -- DUPLICATED CODE. DONE I CADB SECTION                              
066900*    MOVE 4010-TIREGDAT     TO WS-DATUM                                   
067000*    MOVE 4010-TIREGTID     TO WS-TID                                     
067100*    MOVE WS-DATUM          TO HEAD-TIREGDAT                              
067200*    MOVE WS-TID (1:2)      TO HEAD-HOUR                                  
067300*    MOVE WS-TID (3:2)      TO HEAD-MINUTE                                
067400     .                                                                    
067500                                                                          
067600 CADB-REDIGERA-HRAD4 SECTION.                                             
067700     MOVE 'CADB-REDIGERA-HRAD4' TO WS-CURRENT-SECTION                     
067800                                                                          
067900*    -- MOVED TO CADE- SECTION                                            
068000*    MOVE OHUV-BEGMT-RAD2           TO HEAD-BEGMT-RAD2                    
068100                                                                          
068200     MOVE 4010-IDPRC         TO HEAD-IDPRC                                
068300     MOVE 4010-IDLOPNR-PL    TO HEAD-IDLOPNR-PL                           
068400     MOVE 4010-IDLOPNR-ORD   TO HEAD-IDLOPNR-ORD                          
068500     MOVE 4010-IDZON         TO HEAD-IDZON                                
068600                                                                          
068700     MOVE 4010-TIREGDAT      TO WS-DATUM                                  
068800     MOVE 4010-TIREGTID      TO WS-TID                                    
068900     MOVE WS-DATUM           TO HEAD-TIREGDAT                             
069000     MOVE WS-TID (1:2)       TO HEAD-HOUR                                 
069100     MOVE WS-TID (3:2)       TO HEAD-MINUTE                               
069200     .                                                                    
069300                                                                          
069400 CADC-REDIGERA-HRAD7 SECTION.                                             
069500     MOVE 'CADC-REDIGERA-HRAD7' TO WS-CURRENT-SECTION                     
069600                                                                          
069700*    -- MOVED TO PARENT SECTION                                           
069800*    MOVE 4010-BERADREF      TO HEAD-ADGMT-LAND                           
069900                                                                          
070000     MOVE 4010-BERADREF           TO HEAD-BERADREF                        
070100                                                                          
070200     IF DIST85-ITALIEN                                                    
070300       IF OHUV-BEKUNDRF > SPACE                                           
070400         MOVE OHUV-BEKUNDRF         TO HEAD-BERADREF                      
070500       ELSE                                                               
070600         IF 4010-BERADREF > SPACE                                         
070700           MOVE 4010-BERADREF       TO HEAD-BERADREF                      
070800         ELSE                                                             
070900           IF 4010-BEVOLREF > SPACE                                       
071000             MOVE 4010-BEVOLREF     TO HEAD-BERADREF                      
071100           END-IF                                                         
071200         END-IF                                                           
071300       END-IF                                                             
071400     END-IF                                                               
071500                                                                          
071600* FÖR TACDIS ORDER MÖRKAS JOBB NR. I BERADREF                             
071700     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
071800     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
071900       MOVE SPACE            TO HEAD-BERADREF(9:2)                        
072000     END-IF                                                               
072100     MOVE ARB-KDFRAKT        TO HEAD-KDFRAKT                              
072200                                                                          
072300     IF WS-KDCROSS = 'CD'                                                 
072400        MOVE 'CROSS DOCK LINES FROM CDC' TO HEAD-BETEXT                   
072500     ELSE                                                                 
072600        MOVE SPACE           TO HEAD-BETEXT                               
072700     END-IF                                                               
072800                                                                          
072900     MOVE ODEL-DAUTSKR (3:6) TO WS-DATUM                                  
073000     MOVE ODEL-TIUTSTID      TO WS-TID                                    
073100     MOVE WS-DATUM           TO HEAD-TIUTSKR                              
073200                                                                          
073300     MOVE WS-TID (1:2)       TO HEAD-HOUR-PRINT                           
073400     MOVE WS-TID (3:2)       TO HEAD-MINUTE-PRINT                         
073500     .                                                                    
073600     EJECT                                                                
073700 CADE-REDIGERA-HRAD8    SECTION.                                          
073800                                                                          
073900     MOVE OHUV-BEKUNDRF           TO HEAD-BEKUNDRF                        
074000     MOVE OHUV-BEBETRAD-1         TO HEAD-BEBETRAD-1                      
074100     MOVE OHUV-BETELNR            TO HEAD-BETELNR                         
074200     MOVE OHUV-IDMAIL             TO HEAD-IDMAIL                          
074300                                                                          
074400     .                                                                    
074500     EJECT                                                                
074600 CADD-REDIGERA-HRAD9 SECTION.                                             
074700     MOVE 'CADD-REDIGERA-HRAD9'   TO WS-CURRENT-SECTION                   
074800                                                                          
074900     MOVE ODEL-KDFDKRAV           TO W-4535-KDFDKRAV                      
075000     MOVE 4535                    TO W-4535-IDHTYP                        
075100     MOVE OHUV-IDSKYLT            TO W-4536-IDSKYLT                       
075200     PERFORM IMS-09-GU-WDGX4536                                           
075300     IF SEGMENT-FINNS                                                     
075400       MOVE 4536-BEFDKRAV         TO HEAD-BEFDKRAV                        
075500     END-IF                                                               
075600     MOVE 4010-TIRFS              TO WS-DATUM-TID                         
075700     MOVE WS-DATUM-TID (2:6)      TO HEAD-TIRFSDAT                        
075800     MOVE WS-DATUM-TID (8:2)      TO HEAD-TIRFSTID (1:2)                  
075900     MOVE WS-DATUM-TID (10:2)     TO HEAD-TIRFSTID (3:2)                  
076000     .                                                                    
076100     EJECT                                                                
076200                                                                          
076300 CADE-ADDRESS-DATA   SECTION.                                             
076400     MOVE 'ADDRESS-DATA'          TO WS-CURRENT-SECTION                   
076500                                                                          
076600*    -- FIX BAD DATA IN GMT-OVR FIELDS                                    
076700     IF GMT-BEGMT-OVR-RAD1 = LOW-VALUE                                    
076800        MOVE SPACE TO GMT-BEGMT-OVR-RAD1                                  
076900     END-IF                                                               
077000     IF GMT-BEGMT-OVR-RAD2 = LOW-VALUE                                    
077100        MOVE SPACE TO GMT-BEGMT-OVR-RAD2                                  
077200     END-IF                                                               
077300     IF GMT-ADGMT-OVR-GATA = LOW-VALUE                                    
077400        MOVE SPACE TO GMT-ADGMT-OVR-GATA                                  
077500     END-IF                                                               
077600     IF GMT-ADGMT-OVR-PADR = LOW-VALUE                                    
077700        MOVE SPACE TO GMT-ADGMT-OVR-PADR                                  
077800     END-IF                                                               
077900     MOVE 3430-IDDC TO WS-IDDC                                            
078000     IF (NDC-CN OR LDC-CN OR NDC-JP)                                      
078100     AND (                                                                
078200*      -- IF OVR FIELDS CONTAIN ANY SIGNIFICANT VALUES --                 
078300             GMT-BEGMT-OVR-RAD1 NOT = SPACE                               
078400          OR GMT-BEGMT-OVR-RAD2 NOT = SPACE                               
078500          OR GMT-ADGMT-OVR-GATA NOT = SPACE                               
078600          OR GMT-ADGMT-OVR-PADR NOT = SPACE                               
078700         )                                                                
078800*      -- ADDRESS IN DOUBLE-BYTE CHINESE CODE FETCHED FROM                
078900*      -- CUSTOMER DATABASE "OVR" FIELDS INSTEAD OF ORDER HEAD            
079000*      -- (EXCEPT BERADREF/LAND)                                          
079100       IF NDC-JP                                                          
079200         MOVE '930'          TO TRAUTF8-KDCP                              
079300       ELSE                                                               
079400         MOVE '935'          TO TRAUTF8-KDCP                              
079500       END-IF                                                             
079600                                                                          
079700       MOVE GMT-BEGMT-OVR-RAD1    TO HEAD-BEGMT-RAD1                      
079800       MOVE GMT-BEGMT-OVR-RAD2    TO HEAD-BEGMT-RAD2                      
079900       MOVE GMT-ADGMT-OVR-GATA    TO HEAD-ADGMT-GATA                      
080000       MOVE GMT-ADGMT-OVR-PADR    TO HEAD-ADGMT-PADR                      
080100       MOVE 4010-BERADREF         TO HEAD-ADGMT-LAND                      
080200                                                                          
080300     ELSE                                                                 
080400*      -- ADDRESS IN NORMAL EBCDIC CODE (OHUV = GMT)                      
080500       MOVE '278' TO TRAUTF8-KDCP                                         
080600                                                                          
080700       MOVE OHUV-BEGMT-RAD1          TO HEAD-BEGMT-RAD1                   
080800       MOVE OHUV-BEGMT-RAD2          TO HEAD-BEGMT-RAD2                   
080900       MOVE OHUV-ADGMT-GATA          TO HEAD-ADGMT-GATA                   
081000       MOVE OHUV-ADGMT-PADR          TO HEAD-ADGMT-PADR                   
081100       MOVE OHUV-ADGMT-LAND          TO HEAD-ADGMT-LAND                   
081200     END-IF                                                               
081300*TACDIS                                                                   
081400     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
081500     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
081600     AND GMT-FLLDCKND = JA                                                
081700                                                                          
081800       MOVE '278' TO TRAUTF8-KDCP                                         
081900                                                                          
082000*IDGROSS                                                                  
082100       IF  OHUV-IDGROSS NUMERIC                                           
082200       AND OHUV-IDGROSS > ZERO                                            
082300*GROSSIST ADRESS RAD1                                                     
082400         IF OHUV-BEGMT-RAD1 = SPACE                                       
082500           MOVE OHUV-BEGMT-RAD1      TO HEAD-BEGMT-RAD1                   
082600         ELSE                                                             
082700           MOVE GMT-BEGMT-RAD1    TO HEAD-BEGMT-RAD1                      
082800         END-IF                                                           
082900*RAD2                                                                     
083000         IF OHUV-BEGMT-RAD2 = SPACE                                       
083100           MOVE OHUV-BEGMT-RAD2      TO HEAD-BEGMT-RAD2                   
083200         ELSE                                                             
083300           MOVE GMT-BEGMT-RAD2    TO HEAD-BEGMT-RAD2                      
083400         END-IF                                                           
083500*GATA                                                                     
083600         IF OHUV-ADGMT-GATA = SPACE                                       
083700           MOVE OHUV-ADGMT-GATA      TO HEAD-ADGMT-GATA                   
083800         ELSE                                                             
083900           MOVE GMT-ADGMT-GATA    TO HEAD-ADGMT-GATA                      
084000         END-IF                                                           
084100*PADR                                                                     
084200         IF OHUV-ADGMT-PADR = SPACE                                       
084300           MOVE OHUV-ADGMT-PADR      TO HEAD-ADGMT-PADR                   
084400         ELSE                                                             
084500           MOVE GMT-ADGMT-PADR    TO HEAD-ADGMT-PADR                      
084600         END-IF                                                           
084700*LAND                                                                     
084800         IF OHUV-ADGMT-LAND = SPACE                                       
084900           MOVE OHUV-ADGMT-LAND      TO HEAD-ADGMT-LAND                   
085000         ELSE                                                             
085100           MOVE GMT-ADGMT-LAND    TO HEAD-ADGMT-LAND                      
085200         END-IF                                                           
085300       ELSE                                                               
085400*KUND REG ADRESS                                                          
085500         MOVE GMT-BEGMT-RAD1      TO HEAD-BEGMT-RAD1                      
085600         MOVE GMT-BEGMT-RAD2      TO HEAD-BEGMT-RAD2                      
085700         MOVE GMT-ADGMT-GATA      TO HEAD-ADGMT-GATA                      
085800         MOVE GMT-ADGMT-PADR      TO HEAD-ADGMT-PADR                      
085900         MOVE GMT-ADGMT-LAND      TO HEAD-ADGMT-LAND                      
086000       END-IF                                                             
086100                                                                          
086200*TO THE LOWER PART OF THE TACDIS LABEL                                    
086300*KUNDINFO-RAD1                                                            
086400                                                                          
086500         IF OHUV-BEGMT-RAD1 = SPACE                                       
086600           MOVE GMT-BEGMT-RAD1    TO HEAD-KUNDINFO-RAD1                   
086700         ELSE                                                             
086800           MOVE OHUV-BEGMT-RAD1   TO HEAD-KUNDINFO-RAD1                   
086900         END-IF                                                           
087000                                                                          
087100*KUNDINFO-RAD2                                                            
087200         IF OHUV-BEGMT-RAD2 = SPACE                                       
087300           MOVE GMT-BEGMT-RAD2    TO HEAD-KUNDINFO-RAD2                   
087400         ELSE                                                             
087500           MOVE OHUV-BEGMT-RAD2   TO HEAD-KUNDINFO-RAD2                   
087600         END-IF                                                           
087700                                                                          
087800     END-IF                                                               
087900                                                                          
088000     MOVE 35 TO TRAUTF8-KVMAXTL                                           
088100                                                                          
088200     MOVE HEAD-BEGMT-RAD1     TO TRAUTF8-TECONV-FROM                      
088300     CALL WTRAUTF8    USING TRAUTF8-AREA                                  
088400     MOVE TRAUTF8-TECONV-TO   TO HEAD-BEGMT-RAD1                          
088500                                                                          
088600     MOVE HEAD-BEGMT-RAD2     TO TRAUTF8-TECONV-FROM                      
088700     CALL WTRAUTF8    USING TRAUTF8-AREA                                  
088800     MOVE TRAUTF8-TECONV-TO   TO HEAD-BEGMT-RAD2                          
088900                                                                          
089000     MOVE HEAD-ADGMT-GATA     TO TRAUTF8-TECONV-FROM                      
089100     CALL WTRAUTF8    USING TRAUTF8-AREA                                  
089200     MOVE TRAUTF8-TECONV-TO   TO HEAD-ADGMT-GATA                          
089300                                                                          
089400     MOVE HEAD-ADGMT-PADR     TO TRAUTF8-TECONV-FROM                      
089500     CALL WTRAUTF8    USING TRAUTF8-AREA                                  
089600     MOVE TRAUTF8-TECONV-TO   TO HEAD-ADGMT-PADR                          
089700                                                                          
089800*    -- COUNTRY/LAND IS ALWAYS IN NORMAL EBCDIC                           
089900     MOVE '278' TO TRAUTF8-KDCP                                           
090000     MOVE HEAD-ADGMT-LAND     TO TRAUTF8-TECONV-FROM                      
090100     CALL WTRAUTF8    USING TRAUTF8-AREA                                  
090200     MOVE TRAUTF8-TECONV-TO   TO HEAD-ADGMT-LAND                          
090300                                                                          
090400*                                                                         
090500     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
090600     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
090700       IF GMT-FLLDCKND = JA                                               
090800                                                                          
090900         MOVE '278' TO TRAUTF8-KDCP                                       
091000         MOVE HEAD-KUNDINFO-RAD1 TO TRAUTF8-TECONV-FROM                   
091100         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
091200         MOVE TRAUTF8-TECONV-TO TO HEAD-KUNDINFO-RAD1                     
091300                                                                          
091400         MOVE HEAD-KUNDINFO-RAD2 TO TRAUTF8-TECONV-FROM                   
091500         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
091600         MOVE TRAUTF8-TECONV-TO TO HEAD-KUNDINFO-RAD2                     
091700       END-IF                                                             
091800     END-IF                                                               
091900     .                                                                    
092000                                                                          
092100     EJECT                                                                
092200                                                                          
092300 CAE-READ-TACDIS-CASE-LABEL     SECTION.                                  
092400     MOVE 'CAE-READ-TACDIS-CASE-LABEL' TO WS-CURRENT-SECTION              
092500                                                                          
092600*TACDIS CASE LABEL                                                        
092700                                                                          
092800     MOVE NEJ                   TO HEAD-FLTACDISKND                       
092900                                                                          
093000     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
093100     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
093200     AND GMT-FLLDCKND = JA                                                
093300                                                                          
093400       MOVE OHUV-IDGMTREF       TO W-IDGMTREF                             
093500                                                                          
093600       PERFORM IMS-GU-WDI201                                              
093700                                                                          
093800       IF SEGMENT-FINNS                                                   
093900                                                                          
094000         MOVE JA                TO HEAD-FLTACDISKND                       
094100         MOVE TAKF-IDBILREG     TO HEAD-IDBILREG                          
094200         MOVE TAKF-TETACDBO     TO HEAD-TETACDBO                          
094300         MOVE TAKF-BETELNR-TACD TO HEAD-BETELNR-TACD                      
094400         MOVE TAKF-BEMEKAN      TO HEAD-BEMEKAN                           
094500         MOVE TAKF-FLFPLOCK     TO HEAD-FLFPLOCK                          
094600                                                                          
094700         IF TAKF-BEGMT-RAD1 > SPACE                                       
094800           MOVE TAKF-BEGMT-RAD1 TO HEAD-KUNDINFO-RAD1                     
094900         ELSE                                                             
095000           IF OHUV-BEGMT-RAD1 = SPACE                                     
095100             MOVE GMT-BEGMT-RAD1  TO HEAD-KUNDINFO-RAD1                   
095200           ELSE                                                           
095300             MOVE OHUV-BEGMT-RAD1 TO HEAD-KUNDINFO-RAD1                   
095400           END-IF                                                         
095500         END-IF                                                           
095600                                                                          
095700         IF TAKF-BEGMT-RAD2 > SPACE                                       
095800           MOVE TAKF-BEGMT-RAD2 TO HEAD-KUNDINFO-RAD2                     
095900         ELSE                                                             
096000           IF OHUV-BEGMT-RAD2 = SPACE                                     
096100             MOVE GMT-BEGMT-RAD2  TO HEAD-KUNDINFO-RAD2                   
096200           ELSE                                                           
096300             MOVE OHUV-BEGMT-RAD2 TO HEAD-KUNDINFO-RAD2                   
096400           END-IF                                                         
096500         END-IF                                                           
096600                                                                          
096700         IF TAKF-BEGMT-RAD1 > SPACE                                       
096800           MOVE TAKF-BEGMT-RAD1 TO HEAD-BEGMT-RAD1                        
096900         ELSE                                                             
097000           IF OHUV-BEGMT-RAD1 > SPACE                                     
097100             MOVE OHUV-BEGMT-RAD1    TO HEAD-BEGMT-RAD1                   
097200           ELSE                                                           
097300             MOVE GMT-BEGMT-RAD1  TO HEAD-BEGMT-RAD1                      
097400           END-IF                                                         
097500         END-IF                                                           
097600                                                                          
097700         IF TAKF-BEGMT-RAD2 > SPACE                                       
097800           MOVE TAKF-BEGMT-RAD2 TO HEAD-BEGMT-RAD2                        
097900         ELSE                                                             
098000           IF OHUV-BEGMT-RAD2 > SPACE                                     
098100             MOVE OHUV-BEGMT-RAD2    TO HEAD-BEGMT-RAD2                   
098200           ELSE                                                           
098300             MOVE GMT-BEGMT-RAD2  TO HEAD-BEGMT-RAD2                      
098400           END-IF                                                         
098500         END-IF                                                           
098600                                                                          
098700         IF TAKF-ADGMT-GATA > SPACE                                       
098800           MOVE TAKF-ADGMT-GATA TO HEAD-ADGMT-GATA                        
098900         ELSE                                                             
099000           IF OHUV-ADGMT-GATA > SPACE                                     
099100             MOVE OHUV-ADGMT-GATA    TO HEAD-ADGMT-GATA                   
099200           ELSE                                                           
099300             MOVE GMT-ADGMT-GATA  TO HEAD-ADGMT-GATA                      
099400           END-IF                                                         
099500         END-IF                                                           
099600                                                                          
099700         IF TAKF-ADGMT-PADR > SPACE                                       
099800           MOVE TAKF-ADGMT-PADR TO HEAD-ADGMT-PADR                        
099900         ELSE                                                             
100000           IF OHUV-ADGMT-PADR > SPACE                                     
100100             MOVE OHUV-ADGMT-PADR    TO HEAD-ADGMT-PADR                   
100200           ELSE                                                           
100300             MOVE GMT-ADGMT-PADR  TO HEAD-ADGMT-PADR                      
100400           END-IF                                                         
100500         END-IF                                                           
100600                                                                          
100700         IF  TAKF-TIHHMM = ZERO                                           
100800           MOVE SPACE               TO HEAD-HOUR-MINUTE-TACDIS            
100900         ELSE                                                             
101000           MOVE TAKF-TIHHMM         TO WS-TAKF-TIHHMM                     
101100           MOVE WS-TAKF-TIHHMM(1:2) TO WS-HOUR-TACD                       
101200           MOVE WS-TAKF-TIHHMM(3:2) TO WS-MINUTE-TACD                     
101300           MOVE ':'                 TO WS-COLON-TACD                      
101400           MOVE WS-TID-TACD         TO HEAD-HOUR-MINUTE-TACDIS            
101500         END-IF                                                           
101600                                                                          
101700         MOVE 35 TO TRAUTF8-KVMAXTL                                       
101800                                                                          
101900         MOVE '278' TO TRAUTF8-KDCP                                       
102000         MOVE HEAD-KUNDINFO-RAD1 TO TRAUTF8-TECONV-FROM                   
102100         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
102200         MOVE TRAUTF8-TECONV-TO TO HEAD-KUNDINFO-RAD1                     
102300                                                                          
102400         MOVE HEAD-KUNDINFO-RAD2 TO TRAUTF8-TECONV-FROM                   
102500         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
102600         MOVE TRAUTF8-TECONV-TO TO HEAD-KUNDINFO-RAD2                     
102700                                                                          
102800         MOVE HEAD-BEGMT-RAD1 TO TRAUTF8-TECONV-FROM                      
102900         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
103000         MOVE TRAUTF8-TECONV-TO TO HEAD-BEGMT-RAD1                        
103100                                                                          
103200         MOVE HEAD-BEGMT-RAD2 TO TRAUTF8-TECONV-FROM                      
103300         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
103400         MOVE TRAUTF8-TECONV-TO TO HEAD-BEGMT-RAD2                        
103500                                                                          
103600         MOVE HEAD-ADGMT-GATA TO TRAUTF8-TECONV-FROM                      
103700         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
103800         MOVE TRAUTF8-TECONV-TO TO HEAD-ADGMT-GATA                        
103900                                                                          
104000         MOVE HEAD-ADGMT-PADR TO TRAUTF8-TECONV-FROM                      
104100         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
104200         MOVE TRAUTF8-TECONV-TO TO HEAD-ADGMT-PADR                        
104300                                                                          
104400*        -- COUNTRY/LAND IS ALWAYS IN NORMAL EBCDIC                       
104500         MOVE '278' TO TRAUTF8-KDCP                                       
104600         MOVE HEAD-ADGMT-LAND TO TRAUTF8-TECONV-FROM                      
104700         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
104800         MOVE TRAUTF8-TECONV-TO TO HEAD-ADGMT-LAND                        
104900                                                                          
105000*                                                                         
105100       ELSE                                                               
105200         MOVE NEJ               TO HEAD-FLTACDISKND                       
105300         MOVE ALL X'20'         TO HEAD-IDBILREG                          
105400         MOVE ALL X'20'         TO HEAD-TETACDBO                          
105500         MOVE ALL X'20'         TO HEAD-BETELNR-TACD                      
105600         MOVE ALL X'20'         TO HEAD-BEMEKAN                           
105700         MOVE ALL X'20'         TO HEAD-FLFPLOCK                          
105800         MOVE ALL X'20'         TO HEAD-HOUR-MINUTE-TACDIS                
105900       END-IF                                                             
106000     END-IF                                                               
106100                                                                          
106200     MOVE OHUV-TIREPDAT         TO HEAD-TIREPDAT                          
106300                                                                          
106400     MOVE OHUV-IDBILREG         TO HEAD-IDBILREG                          
106500                                                                          
106600     IF ((OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                
106700     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
106800     AND OHUV-IDGROSS NUMERIC                                             
106900        AND OHUV-IDGROSS > ZERO )                                         
107000        MOVE OHUV-IDKUNDNR         TO WS-IDKUNDNR                         
107100        MOVE WS-IDKUNDNR(4:3)      TO HEAD-IDKUNDNR (1:3)                 
107200        MOVE OHUV-IDGROSS          TO HEAD-IDKUNDNR (4:3)                 
107300     END-IF                                                               
107400     .                                                                    
107500                                                                          
107600 CB-REDIGERA-DETALJRAD SECTION.                                           
107700     MOVE 'CB-REDIGERA-DETALJRAD'  TO WS-CURRENT-SECTION                  
107800                                                                          
107900*    -- FLYTTAT TILL C-BEHANDLA-PURAD (S02-LAES-KUND)                     
108000*    MOVE TEST-IDDISTR                   TO W-IDDISTR-WDB2                
108100*    MOVE WS-OHUV-IDKUNDNR               TO W-IDKUNDNR-WDB2               
108200*    PERFORM IMS-04-GU-WDB201                                             
108300*    IF SEGMENT-SAKNAS                                                    
108400*      MOVE NEJ                          TO GMT-FLLDCKND                  
108500*    END-IF                                                               
108600                                                                          
108700     MOVE '2'                            TO LINE-IDAFPRCD                 
108800                                                                          
108900     IF GMT-FLLDCKND = JA  AND                                            
109000       (WS-OHUV-KDORDKL = 0 OR WS-OHUV-KDORDKL = 1 OR                     
109100        WS-OHUV-KDORDKL = 3 OR WS-OHUV-KDORDKL = 4)                       
109200                                                                          
109300        MOVE 4010-BERADREF  TO LINE-WIPID                                 
109400     END-IF                                                               
109500                                                                          
109600     IF 4010-IDKUNDRF-RO > '0000000   '                                   
109700        MOVE 4010-IDKUNDRF-RO (3:5) TO LINE-IDKUNDRF-RO-URS               
109800     ELSE                                                                 
109900        MOVE SPACE                  TO LINE-IDKUNDRF-RO-URS               
110000     END-IF                                                               
110100                                                                          
110200     MOVE 4010-ADLAGOMR-ORD    TO LINE-ADLAGOMR                           
110300     MOVE 4010-ADGANG          TO LINE-ADGANG                             
110400     MOVE 4010-ADPLATS         TO LINE-ADPLATS                            
110500                                                                          
110600     IF DIST34-ENGLAND-SDC                                                
110700        IF GMT-FLLDCKND = JA                                              
110800           IF (4010-KDORDKL = 0 OR 1 OR 3 OR 4)                           
110900                                                                          
111000             PERFORM CBA-SPARA-I-LDC-TAB                                  
111100           END-IF                                                         
111200        ELSE                                                              
111300           MOVE 4010-BERADREF  TO LINE-BERADREF                           
111400        END-IF                                                            
111500     END-IF                                                               
111600                                                                          
111700     IF 4010-IDSYSTEM (1:3) = 'LYN'                                       
111800        MOVE 4010-IDARTNR        TO W-IDARTNR                             
111900        PERFORM IMS-GU-WDF502                                             
112000         IF SEGMENT-FINNS                                                 
112100           MOVE XLEV-IDLEVART       TO LINE-IDLEVART                      
112200         END-IF                                                           
112300     END-IF                                                               
112400                                                                          
112500     MOVE 4010-IDSYSTEM        TO LINE-IDSYSTEM                           
112600     MOVE 4010-IDARTNR         TO LINE-IDARTNR                            
112700     MOVE 4010-REKSIFFR        TO LINE-REKSIFFR                           
112800     MOVE 4010-IDPURAD         TO LINE-IDPURAD                            
112900                                                                          
113000     IF DIST08-URSP-RAPP OR DIST08-URSP-SPX                               
113100        MOVE 4010-KDARTURS     TO LINE-KDARTURS                           
113200     ELSE                                                                 
113300        MOVE SPACE             TO LINE-KDARTURS                           
113400     END-IF                                                               
113500                                                                          
113600     IF 4010-FLTILLK = JA                                                 
113700        MOVE '*'               TO LINE-FLTILLK                            
113800     ELSE                                                                 
113900        MOVE SPACE             TO LINE-FLTILLK                            
114000     END-IF                                                               
114100                                                                          
114200     MOVE 4010-BEART           TO LINE-BEART                              
114300     MOVE 4010-KVBEART-Q       TO LINE-KVBEART-Q                          
114400     MOVE 4010-KVAVBART        TO LINE-KVAVBART                           
114500                                                                          
114600     IF 4010-KDFARLIG = 4 OR 6                                            
114700       MOVE 'D'                TO LINE-KDFARLIG                           
114800*                      D = DANGEROUS GOODS.                               
114900     ELSE                                                                 
115000       MOVE SPACE              TO LINE-KDFARLIG                           
115100     END-IF                                                               
115200                                                                          
115300     MOVE 4010-BERADREF(1:8)   TO LINE-BERADREF                           
115400     MOVE 4010-IDSPECEMB       TO LINE-IDSPECEMB                          
115500                                                                          
115600     ADD 1                     TO 4008-KVRADER-GRP (100)                  
115700     ADD 1                     TO 4008-KVRADER     (1)                    
115800                                                                          
115900*** 4008-KVRADER (3) ANVÄNDS FÖR ATT KUNNA LAGRA ANTAL                    
116000*** AVBOKADE ARTKLAR PER RAD.                                             
116100*** VID LÄMPLIGT TILLFÄLLE BÖR SEGMENT 4464 KOMPLETTERAS                  
116200*** MED DATAELEMENT KVAVBART.                                             
116300                                                                          
116400     ADD 4010-KVAVBART         TO 4008-KVRADER     (3)                    
116500                                                                          
116600     ADD 4010-VKORDNTO         TO 4008-VKORDNTO    (1)                    
116700     ADD 4010-VLORDNTO         TO 4008-VLORDNTO    (1)                    
116800                                                                          
116900     IF 4010-KDPRT NOT = 4008-KDPRT-PU                                    
117000        IF 4010-ADLAGOMR-ORD > 0 AND < 100                                
117100           ADD 1 TO 4008-KVRADER-GRP (4010-ADLAGOMR-ORD)                  
117200        END-IF                                                            
117300     END-IF                                                               
117400     .                                                                    
117500                                                                          
117600 CBA-SPARA-I-LDC-TAB          SECTION.                                    
117700     MOVE 'CBA-SPARA-I-LDC-TAB  '  TO WS-CURRENT-SECTION                  
117800                                                                          
117900                                                                          
118000     MOVE +1      TO LDC-TAB-IX                                           
118100     MOVE +100    TO LDC-TAB-IX-MAX                                       
118200                                                                          
118300     PERFORM UNTIL 4010-BERADREF =                                        
118400                   LDC-TAB-VIPID(LDC-TAB-IX)                              
118500                OR LDC-TAB-VIPID (LDC-TAB-IX) = HIGH-VALUE                
118600                OR LDC-TAB-IX = LDC-TAB-IX-MAX                            
118700        ADD 1    TO LDC-TAB-IX                                            
118800     END-PERFORM                                                          
118900                                                                          
119000     IF LDC-TAB-VIPID (LDC-TAB-IX) = HIGH-VALUE                           
119100                                                                          
119200        MOVE 4010-BERADREF TO LDC-TAB-VIPID  (LDC-TAB-IX)                 
119300        MOVE +1            TO LDC-TAB-ANTAL-ART (LDC-TAB-IX)              
119400        IF 4010-BERADREF(4:7) > SPACE                                     
119500          ADD +1           TO LDC-TOT-ANT-ART                             
119600        END-IF                                                            
119700     ELSE                                                                 
119800        ADD +1             TO LDC-TAB-ANTAL-ART (LDC-TAB-IX)              
119900     END-IF                                                               
120000     .                                                                    
120100                                                                          
120200 CC-TOTAL-IDORDER SECTION.                                                
120300     MOVE 'CC-TOTAL-IDORDER    '   TO WS-CURRENT-SECTION                  
120400                                                                          
120500     MOVE '3'                 TO TOTAL-IDAFPRCD                           
120600                                                                          
120700*    ORDER-ACKAR                                                          
120800     MOVE 4008-VKORDNTO (1)   TO WS-TOTAL-VKORDNTO                        
120900     ADD ADD-1-HEKTO          TO WS-TOTAL-VKORDNTO                        
121000     MOVE WS-TOTAL-VKORDNTO   TO TOTAL-VKORDNTO                           
121100     MOVE 4008-VLORDNTO (1)   TO TOTAL-VLORDNTO                           
121200     MOVE 4008-KVRADER  (1)   TO TOTAL-KVRADER                            
121300                                                                          
121400*    ADDERA TILL TOTAL-ACKAR                                              
121500     ADD 4008-KVRADER   (1)   TO 4008-KVRADER     (2)                     
121600     ADD 4008-VKORDNTO  (1)   TO 4008-VKORDNTO    (2)                     
121700     ADD 4008-VLORDNTO  (1)   TO 4008-VLORDNTO    (2)                     
121800                                                                          
121900*    NOLLA ORDER-ACKAR                                                    
122000     MOVE ZERO                TO 4008-KVRADER     (1)                     
122100                                 4008-VKORDNTO    (1)                     
122200                                 4008-VLORDNTO    (1)                     
122300*    TOTAL-ACKAR                                                          
122400     MOVE 4008-VKORDNTO (2)   TO TOTAL-VKORDNTO-TOT                       
122500                                                                          
122600     MOVE 4008-VLORDNTO (2)   TO TOTAL-VLORDNTO-TOT                       
122700     MOVE 4008-KVRADER  (2)   TO TOTAL-KVRADER-TOT                        
122800     .                                                                    
122900     EJECT                                                                
123000                                                                          
123100 CE-SKRIV-LDC-GB-INFO   SECTION.                                          
123200     MOVE 'CE-SKRIV-LDC-GB-INFO '  TO WS-CURRENT-SECTION                  
123300                                                                          
123400     IF LDC-TOT-ANT-ART > 0                                               
123500     IF DIST34-ENGLAND-SDC AND                                            
123600        GMT-FLLDCKND = JA                                                 
123700        IF (WS-OHUV-KDORDKL = 0 OR WS-OHUV-KDORDKL = 1                    
123800        OR  WS-OHUV-KDORDKL = 3 OR WS-OHUV-KDORDKL = 4)                   
123900                                                                          
124000           MOVE '2A'                    TO LINE-IDAFPRCD                  
124100           PERFORM S92-PUT-DOC-HEAD                                       
124200                                                                          
124300           MOVE 1   TO  LDC-TAB-IX                                        
124400           PERFORM UNTIL LDC-TAB-VIPID(LDC-TAB-IX) = HIGH-VALUE           
124500                      OR LDC-TAB-IX = LDC-TAB-IX-MAX                      
124600                                                                          
124700              MOVE LDC-TAB-VIPID (LDC-TAB-IX) TO LINE-WIPID               
124800              INSPECT LINE-BERADREF                                       
124900              REPLACING LEADING ZERO BY SPACE                             
125000              MOVE LDC-TAB-ANTAL-ART (LDC-TAB-IX)                         
125100                TO LINE-KVAVBART                                          
125200                                                                          
125300              PERFORM S93-PUT-DOC-LINE                                    
125400              MOVE SPACE             TO DOC-LINE-AREA                     
125500              MOVE '2A'              TO LINE-IDAFPRCD                     
125600              ADD 1 TO LDC-TAB-IX                                         
125700                                                                          
125800           END-PERFORM                                                    
125900                                                                          
126000        END-IF                                                            
126100     END-IF                                                               
126200     MOVE ZERO              TO LDC-TOT-ANT-ART                            
126300     END-IF                                                               
126400     .                                                                    
126500                                                                          
126600 CF-SKAPA-KOLLIFLAGGA     SECTION.                                        
126700     MOVE 'CF-SKAPA-KOLLIFLAGGA'  TO WS-CURRENT-SECTION                   
126800*                                                                         
126900     MOVE OHUV-IDDISTR       TO WS-BARCODE-DISTR                          
127000     MOVE OHUV-IDKUNDNR      TO WS-BARCODE-KUNDNR                         
127100     MOVE OHUV-IDORDNR7      TO WS-BARCODE-ORDNR                          
127200                                                                          
127300     MOVE 4010-IDPLKLST      TO WS-4010-IDPLKLST                          
127400                                                                          
127500     MOVE 4010-IDDC                         TO W-IDDC-B6-X                
127600     MOVE 4010-IDPRC                        TO W-IDPRC-B6-X               
127700                                                                          
127800     PERFORM IMS-33-GU-WDB612                                             
127900     IF SEGMENT-FINNS                                                     
128000                                                                          
128100       MOVE PRC-IDKOLLI-PRCSTA              TO HEAD-IDKOLLI               
128200       MOVE PRC-IDKOLLI-PRCSTA              TO WS-BARCODE-KOLLI           
128300       MOVE PRC-KDKOLLI                     TO W-KDKOLLI-K5               
128400     ELSE                                                                 
128500                                                                          
128600       MOVE '9999'                          TO W-IDPRC-B6-X               
128700       PERFORM IMS-33-GU-WDB612                                           
128800                                                                          
128900       IF SEGMENT-FINNS                                                   
129000         MOVE PRC-IDKOLLI-PRCSTA            TO HEAD-IDKOLLI               
129100         MOVE PRC-IDKOLLI-PRCSTA            TO WS-BARCODE-KOLLI           
129200         MOVE PRC-KDKOLLI                   TO W-KDKOLLI-K5               
129300       ELSE                                                               
129400         MOVE +00010                        TO HEAD-IDKOLLI               
129500         MOVE +00010                        TO WS-BARCODE-KOLLI           
129600         MOVE 'NC'                          TO W-KDKOLLI-K5               
129700       END-IF                                                             
129800     END-IF                                                               
129900                                                                          
130000*TARA WEIGHT                                                              
130100*    MOVE WS-PRC-KDKOLLI                TO W-KDKOLLI-K5                   
130200     PERFORM IMS-GU-WDK5                                                  
130300     IF WDK5-SEGMENT-FOUND                                                
130400        MOVE EMB-VKTARA                 TO WS-VKTARA                      
130500     ELSE                                                                 
130600        MOVE ZERO                       TO WS-VKTARA                      
130700     END-IF                                                               
130800                                                                          
130900     PERFORM CFA-KOLLA-OM-KOLLI-FINNS                                     
131000                                                                          
131100     MOVE 4010-IDPRODNR                   TO HEAD-IDPRODNR                
131200     MOVE 3430-ADFLGEO(4010-IDLOPNR-ORD)  TO HEAD-ADFLGEO                 
131300     MOVE 3430-ADFLOMR(4010-IDLOPNR-ORD)  TO HEAD-ADFLOMR                 
131400     MOVE 3430-ADRUTNIV(4010-IDLOPNR-ORD) TO HEAD-ADRUTNIV                
131500     MOVE OHUV-IDDEPT                     TO HEAD-IDDEPT                  
131600     MOVE 3430-VKORDNTO(4010-IDLOPNR-ORD) TO HEAD-VKORDBTO                
131700     MOVE 3430-VKORDNTO(4010-IDLOPNR-ORD) TO WS-HEAD-VKORDNTO             
131800                                                                          
131900     IF WDK5-SEGMENT-FOUND                                                
132000       COMPUTE HEAD-VKORDBTO = WS-HEAD-VKORDNTO + WS-VKTARA               
132100       END-COMPUTE                                                        
132200                                                                          
132300       MOVE HEAD-VKORDBTO      TO WS-HEAD-VKORDBTO                        
132400       COMPUTE HEAD-VKORDBTO = WS-HEAD-VKORDBTO + ADD-1-HEKTO             
132500       END-COMPUTE                                                        
132600     END-IF                                                               
132700                                                                          
132800     MOVE WS-BARCODE                      TO HEAD-BARCODE                 
132900     MOVE 3430-IDTRPTNR(4010-IDLOPNR-ORD) TO HEAD-IDTRPTNR                
133000     .                                                                    
133100                                                                          
133200 CFA-KOLLA-OM-KOLLI-FINNS  SECTION.                                       
133300     MOVE 'CFA-KOLLA-OM-KOLLI-FINNS'  TO WS-CURRENT-SECTION               
133400                                                                          
133500     MOVE 4010-IDPRODNR               TO W-IDPRODNR-WDE6                  
133600     MOVE WS-BARCODE-KOLLI            TO W-IDKOLLI-WDE6                   
133700                                                                          
133800     PERFORM IMS-34-GU-WDE601                                             
133900     IF SEGMENT-FINNS                                                     
134000                                                                          
134100       PERFORM IMS-35-GNP-WDE611                                          
134200       IF SEGMENT-FINNS                                                   
134300*LK      IF KOLLI-KDKOLSTA > +0                                           
134400           PERFORM UNTIL SEGMENT-SAKNAS                                   
134500                                                                          
134600             ADD +1                   TO W-IDKOLLI-WDE6                   
134700             ADD +1                   TO WS-BARCODE-KOLLI                 
134800             PERFORM IMS-35-GNP-WDE611                                    
134900           END-PERFORM                                                    
135000                                                                          
135100           MOVE WS-BARCODE-KOLLI      TO HEAD-IDKOLLI                     
135200*LK      END-IF                                                           
135300       END-IF                                                             
135400     END-IF                                                               
135500     .                                                                    
135600                                                                          
135700 D-UPPDAT-PLOCKSATS SECTION.                                              
135800     MOVE 'D-UPPDAT-PLOCKSATS' TO WS-CURRENT-SECTION                      
135900                                                                          
136000     MOVE 4010-IDORDER      TO 4008-IDORDER                               
136100     MOVE 4010-KDPRT        TO 4008-KDPRT                                 
136200     MOVE 4010-KDSS-PU      TO 4008-KDSS                                  
136300     MOVE 4010-ADLAGOMR     TO 4008-ADLAGOMR                              
136400     MOVE 4010-ADGANG       TO 4008-ADGANG                                
136500     MOVE 4010-ADPLATS      TO 4008-ADPLATS                               
136600     MOVE 4010-IDARTNR      TO 4008-IDARTNR                               
136700     MOVE 4010-IDLOPNR      TO 4008-IDLOPNR                               
136800     MOVE DLI-IO-WDGX4008   TO W-PU-PLOCKSATS                             
136900     PERFORM IMS-10-GHU-WDGX4008                                          
137000     MOVE W-PU-PLOCKSATS    TO DLI-IO-WDGX4008                            
137100     PERFORM IMS-11-REPL-WDGX4008                                         
137200     .                                                                    
137300                                                                          
137400                                                                          
137500 S01-LAES-ORDERHUVUD SECTION.                                             
137600     MOVE 'S01-LAES-ORDERHUVUD'   TO WS-CURRENT-SECTION                   
137700                                                                          
137800     MOVE 4010-IDORDER     TO W-IDORDER                                   
137900     MOVE 4010-IDDC        TO W-IDDC                                      
138000                                                                          
138100     PERFORM IMS-05-GHU-WDQ201-12                                         
138200                                                                          
138300     MOVE OHUV-IDDISTR     TO TEST-IDDISTR                                
138400     MOVE OHUV-IDKUNDNR    TO WS-OHUV-IDKUNDNR                            
138500     MOVE OHUV-KDORDKL     TO WS-OHUV-KDORDKL                             
138600     .                                                                    
138700                                                                          
138800                                                                          
138900 S02-LAES-KUND  SECTION.                                                  
139000     MOVE 'S02-LAES-KUND'     TO WS-CURRENT-SECTION                       
139100                                                                          
139200     MOVE TEST-IDDISTR               TO W-IDDISTR-WDB2                    
139300     MOVE WS-OHUV-IDKUNDNR           TO W-IDKUNDNR-WDB2                   
139400                                                                          
139500     PERFORM IMS-04-GU-WDB201                                             
139600                                                                          
139700     IF SEGMENT-SAKNAS                                                    
139800       MOVE SPACE                    TO DLI-IO-WDB201                     
139900       MOVE NEJ                      TO GMT-FLLDCKND                      
140000     END-IF                                                               
140100     .                                                                    
140200                                                                          
140300                                                                          
140400 S90-OPEN-DAP-SEND SECTION.                                               
140500     MOVE 'S90-OPEN-DAP-SEND' TO WS-CURRENT-SECTION                       
140600                                                                          
140700     MOVE 'OPEN'                     TO SEND-KDFUNC                       
140800     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
140900     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
141000                                                                          
141100     IF SEND-KDRC > 0                                                     
141200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
141300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
141400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
141500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
141600     END-IF                                                               
141700     .                                                                    
141800                                                                          
141900 S91-PUT-DAP-HEADER SECTION.                                              
142000     MOVE 'S91-PUT-DAP-HEADER' TO WS-CURRENT-SECTION                      
142100                                                                          
142200     MOVE 'PUT'                      TO SEND-KDFUNC                       
142300     MOVE LENGTH OF HDR-AREA         TO SEND-KVDLEN                       
142400                                                                          
142500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
142600                         SEND-KVDLEN                                      
142700                         HDR-AREA                                         
142800     IF SEND-KDRC > ZERO                                                  
142900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
143000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
143100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
143200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
143300     END-IF                                                               
143400     .                                                                    
143500                                                                          
143600 S92-PUT-DOC-HEAD SECTION.                                                
143700     MOVE 'S92-PUT-DOC-HEAD  '   TO WS-CURRENT-SECTION                    
143800                                                                          
143900     MOVE 'PUT'                           TO SEND-KDFUNC                  
144000     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
144100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
144200                         SEND-KVDLEN                                      
144300                         DOC-HEAD-AREA                                    
144400     IF SEND-KDRC > ZERO                                                  
144500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
144600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
144700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
144800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
144900     END-IF                                                               
145000     .                                                                    
145100                                                                          
145200 S93-PUT-DOC-LINE SECTION.                                                
145300     MOVE 'S93-PUT-DOC-LINE  '   TO WS-CURRENT-SECTION                    
145400                                                                          
145500     MOVE 'PUT'                           TO SEND-KDFUNC                  
145600     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
145700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
145800                         SEND-KVDLEN                                      
145900                         DOC-LINE-AREA                                    
146000     IF SEND-KDRC > ZERO                                                  
146100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
146200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
146300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
146400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
146500     END-IF                                                               
146600     .                                                                    
146700                                                                          
146800 S94-PUT-DOC-TOT SECTION.                                                 
146900     MOVE 'S94-PUT-DOC-TOT  '   TO WS-CURRENT-SECTION                     
147000                                                                          
147100     MOVE 'PUT'                           TO SEND-KDFUNC                  
147200     MOVE LENGTH OF DOC-TOT-AREA          TO SEND-KVDLEN                  
147300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
147400                         SEND-KVDLEN                                      
147500                         DOC-TOT-AREA                                     
147600     IF SEND-KDRC > ZERO                                                  
147700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
147800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
147900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
148000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
148100     END-IF                                                               
148200     .                                                                    
148300                                                                          
148400                                                                          
148500 S95-CLOSE-DAP-SEND SECTION.                                              
148600     MOVE 'S95-CLOSE-DAP-SEND' TO WS-CURRENT-SECTION                      
148700                                                                          
148800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
148900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
149000                                                                          
149100     IF SEND-KDRC > 0                                                     
149200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
149300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
149400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
149500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
149600     END-IF                                                               
149700     .                                                                    
149800                                                                          
149900     SKIP3                                                                
150000* IMS SEKTIONER                                                           
150100* IMS SEKTIONER                                                           
150200* IMS SEKTIONER                                                           
150300     EJECT                                                                
150400                                                                          
150500 IMS-GU-WDI201      SECTION.                                              
150600     MOVE 'IMS-GU-WDI201'     TO WS-CURRENT-IMS-SECTION                   
150700                                                                          
150800     STRING 'WDI201  (IDGMTREF =' W-IDGMTREF-X ')'                        
150900            DELIMITED BY SIZE INTO SSA1                                   
151000     MOVE '  GE' TO GODK-STATUSKODER                                      
151100     CALL CBLTDLI USING GU WDI2-PCB DLI-IO-WDI201 SSA1                    
151200     MOVE WDI2-STATUS-CODE TO STATUS-WS                                   
151300     PERFORM IMS-STATUSKONTROLL                                           
151400     SKIP3                                                                
151500     .                                                                    
151600                                                                          
151700 IMS-01-GHU-WDGX4008   SECTION.                                           
151800     MOVE 'IMS-01'    TO WS-CURRENT-IMS-SECTION                           
151900                                                                          
152000     STRING 'WL400701(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
152100          DELIMITED BY SIZE INTO SSA1                                     
152200     MOVE 'WL400711 '         TO SSA2                                     
152300     MOVE '  GE'              TO GODK-STATUSKODER                         
152400     CALL CBLTDLI USING GHU 4007-PCB DLI-IO-WDGX4008 SSA1 SSA2            
152500     MOVE 4007-STATUS-CODE    TO STATUS-WS                                
152600     PERFORM IMS-STATUSKONTROLL                                           
152700     .                                                                    
152800                                                                          
152900 IMS-02-GNP-WDGX4010-KVAL SECTION.                                        
153000     MOVE 'IMS-02'    TO WS-CURRENT-IMS-SECTION                           
153100                                                                          
153200     STRING 'WL400721(KY4010   =' W-4010-IDHTYP-X ')'                     
153300          DELIMITED BY SIZE INTO SSA1                                     
153400     MOVE '    '              TO GODK-STATUSKODER                         
153500     CALL CBLTDLI USING GNP 4007-PCB DLI-IO-WDGX4010 SSA1                 
153600     MOVE 4007-STATUS-CODE    TO STATUS-WS                                
153700     PERFORM IMS-STATUSKONTROLL                                           
153800     .                                                                    
153900                                                                          
154000 IMS-03-GNP-WDGX4010-OKVAL SECTION.                                       
154100     MOVE 'IMS-03'    TO WS-CURRENT-IMS-SECTION                           
154200                                                                          
154300     STRING 'WL400721(KDPRT    <' W-4010-KDPRT-MAX-X ')'                  
154400          DELIMITED BY SIZE INTO SSA1                                     
154500     MOVE '  GE'              TO GODK-STATUSKODER                         
154600     CALL CBLTDLI USING GNP 4007-PCB DLI-IO-WDGX4010 SSA1                 
154700     MOVE 4007-STATUS-CODE    TO STATUS-WS                                
154800     PERFORM IMS-STATUSKONTROLL                                           
154900     .                                                                    
155000                                                                          
155100 IMS-04-GU-WDB201 SECTION.                                                
155200     MOVE 'IMS-04'    TO WS-CURRENT-IMS-SECTION                           
155300                                                                          
155400     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
155500          DELIMITED BY SIZE INTO SSA1                                     
155600     MOVE '  GE' TO GODK-STATUSKODER                                      
155700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
155800     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
155900     PERFORM IMS-STATUSKONTROLL                                           
156000     .                                                                    
156100                                                                          
156200 IMS-05-GHU-WDQ201-12 SECTION.                                            
156300     MOVE 'IMS-05' TO WS-CURRENT-IMS-SECTION                              
156400                                                                          
156500     STRING 'WDQ201  *D(IDORDER  =' W-IDORDER-X ')'                       
156600          DELIMITED BY SIZE INTO SSA1                                     
156700     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
156800          DELIMITED BY SIZE INTO SSA2                                     
156900     MOVE '  GE' TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-WDQ201-12 SSA1 SSA2           
157100     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     .                                                                    
157400                                                                          
157500 IMS-06-GU-WDQ301   SECTION.                                              
157600     MOVE 'IMS-06' TO WS-CURRENT-IMS-SECTION                              
157700                                                                          
157800     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
157900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
158000          DELIMITED BY SIZE INTO SSA1                                     
158100     MOVE '  '                TO GODK-STATUSKODER                         
158200     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
158300     MOVE WDQ3-STATUS-CODE    TO STATUS-WS                                
158400     PERFORM IMS-STATUSKONTROLL                                           
158500     .                                                                    
158600                                                                          
158700 IMS-07-GU-WDGX4732  SECTION.                                             
158800     MOVE 'IMS-07' TO WS-CURRENT-IMS-SECTION                              
158900                                                                          
159000     STRING 'WL473201(WDGXKEY  =' W-4732-IDHTYP-X ')'                     
159100          DELIMITED BY SIZE INTO SSA1                                     
159200     MOVE   'WL473211 '       TO SSA2                                     
159300     MOVE '  GE'              TO GODK-STATUSKODER                         
159400     CALL CBLTDLI USING GU 4732-PCB DLI-IO-WDGX4732 SSA1 SSA2             
159500     MOVE 4732-STATUS-CODE    TO STATUS-WS                                
159600     PERFORM IMS-STATUSKONTROLL                                           
159700     .                                                                    
159800                                                                          
159900 IMS-08-GU-WDGX4448  SECTION.                                             
160000     MOVE 'IMS-08' TO WS-CURRENT-IMS-SECTION                              
160100                                                                          
160200     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
160300          DELIMITED BY SIZE INTO SSA1                                     
160400     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
160500          DELIMITED BY SIZE INTO SSA2                                     
160600     MOVE '  GE' TO GODK-STATUSKODER                                      
160700     CALL CBLTDLI USING GU 4447-PCB DLI-IO-WDGX4448 SSA1 SSA2             
160800     MOVE 4447-STATUS-CODE    TO STATUS-WS                                
160900     PERFORM IMS-STATUSKONTROLL                                           
161000     .                                                                    
161100                                                                          
161200 IMS-09-GU-WDGX4536  SECTION.                                             
161300     MOVE 'IMS-09' TO WS-CURRENT-IMS-SECTION                              
161400                                                                          
161500     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
161600          DELIMITED BY SIZE INTO SSA1                                     
161700     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
161800          DELIMITED BY SIZE INTO SSA2                                     
161900     MOVE '  GE'              TO GODK-STATUSKODER                         
162000     CALL CBLTDLI USING GU 4535-PCB DLI-IO-WDGX4536 SSA1 SSA2             
162100     MOVE 4535-STATUS-CODE    TO STATUS-WS                                
162200     PERFORM IMS-STATUSKONTROLL                                           
162300     .                                                                    
162400                                                                          
162500 IMS-10-GHU-WDGX4008  SECTION.                                            
162600     MOVE 'IMS-10' TO WS-CURRENT-IMS-SECTION                              
162700                                                                          
162800     STRING 'WL400701(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
162900          DELIMITED BY SIZE INTO SSA1                                     
163000     MOVE 'WL400711 '         TO SSA2                                     
163100     MOVE '    '              TO GODK-STATUSKODER                         
163200     CALL CBLTDLI USING GHU 4007-PCB DLI-IO-WDGX4008 SSA1 SSA2            
163300     MOVE 4007-STATUS-CODE    TO STATUS-WS                                
163400     PERFORM IMS-STATUSKONTROLL                                           
163500     .                                                                    
163600                                                                          
163700 IMS-11-REPL-WDGX4008  SECTION.                                           
163800     MOVE 'IMS-11' TO WS-CURRENT-IMS-SECTION                              
163900                                                                          
164000     MOVE '    '           TO GODK-STATUSKODER                            
164100     CALL CBLTDLI USING REPL 4007-PCB DLI-IO-WDGX4008                     
164200     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     .                                                                    
164500                                                                          
164600 IMS-12-GU-WDQ301-DC11   SECTION.                                         
164700     MOVE 'IMS-12' TO WS-CURRENT-IMS-SECTION                              
164800                                                                          
164900     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
165000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
165100          DELIMITED BY SIZE INTO SSA1                                     
165200     MOVE '  GE'              TO GODK-STATUSKODER                         
165300     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
165400     MOVE WDQ3-STATUS-CODE    TO STATUS-WS                                
165500     PERFORM IMS-STATUSKONTROLL                                           
165600     .                                                                    
165700                                                                          
165800*IMS-32-GU-WDB601    SECTION.                                             
165900*    MOVE 'IMS-08'     TO WS-CURRENT-IMS-SECTION                          
166000*                                                                         
166100*    STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
166200*         DELIMITED BY SIZE INTO SSA1                                     
166300*    MOVE '  ' TO GODK-STATUSKODER                                        
166400*    CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
166500*    MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
166600*    PERFORM IMS-STATUSKONTROLL                                           
166700*    .                                                                    
166800                                                                          
166900 IMS-33-GU-WDB612 SECTION.                                                
167000     MOVE 'IMS-GU-WDB612'    TO WS-CURRENT-IMS-SECTION                    
167100                                                                          
167200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X  ')'                        
167300          DELIMITED BY SIZE INTO SSA1                                     
167400     STRING 'WDB612  (IDPRC    =' W-IDPRC-B6-X ')'                        
167500          DELIMITED BY SIZE INTO SSA2                                     
167600     MOVE '  GE'             TO GODK-STATUSKODER                          
167700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB612 SSA1 SSA2               
167800     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
167900     PERFORM IMS-STATUSKONTROLL                                           
168000     .                                                                    
168100     EJECT                                                                
168200 IMS-34-GU-WDE601 SECTION.                                                
168300     MOVE 'IMS-34'   TO WS-CURRENT-IMS-SECTION                            
168400                                                                          
168500     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
168600          DELIMITED BY SIZE INTO SSA1                                     
168700     MOVE '  GE'              TO GODK-STATUSKODER                         
168800     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
168900     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
169000     PERFORM IMS-STATUSKONTROLL                                           
169100     .                                                                    
169200                                                                          
169300 IMS-35-GNP-WDE611     SECTION.                                           
169400     MOVE 'IMS-35'   TO WS-CURRENT-IMS-SECTION                            
169500                                                                          
169600     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
169700          DELIMITED BY SIZE INTO SSA1                                     
169800     MOVE '  GE'              TO GODK-STATUSKODER                         
169900     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
170000     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
170100     PERFORM IMS-STATUSKONTROLL                                           
170200     .                                                                    
170300                                                                          
170400 IMS-GU-WDF502 SECTION.                                                   
170500     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
170600            DELIMITED BY SIZE INTO SSA1                                   
170700     MOVE 'WDF502  '       TO SSA2                                        
170800     MOVE '  GE'           TO GODK-STATUSKODER                            
170900     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
171000     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
171100     PERFORM IMS-STATUSKONTROLL                                           
171200     .                                                                    
171300                                                                          
171400 IMS-GU-WDK5     SECTION.                                                 
171500     MOVE 'IMS-GU-WDK5'   TO WS-CURRENT-IMS-SECTION                       
171600                                                                          
171700     STRING 'WDK501  (KDKOLLI  =' W-WDK501KY-X ')'                        
171800            DELIMITED BY SIZE INTO SSA1                                   
171900     MOVE '  GE'              TO GODK-STATUSKODER                         
172000     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
172100     MOVE WDK5-STATUS-CODE TO WDK5-STATUS-WS                              
172200     PERFORM IMS-STATUSKONTROLL                                           
172300     .                                                                    
172400     SKIP2                                                                
172500                                                                          
172600 IMS-STATUSKONTROLL SECTION.                                              
172700                                                                          
172800     SET STATUS-IX TO 1                                                   
172900     SEARCH GODK-STATUS                                                   
173000       AT END CALL FELLOG                                                 
173100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
173200     END-SEARCH                                                           
173300     .                                                                    
