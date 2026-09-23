000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2120210.                                            
000300 AUTHOR.             B SWAHNBERG, DATA LOGIC AB.                          
000400 DATE-WRITTEN.       OKTOBER 1978.                                        
000500*    REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2120200 OCH                   
000800*        SKÖTER SAMTLIGA IMS-LÄS-ANROP MOT ARTIKELBASEN (ARTC)            
000900*        INLEVERANSBASEN (INLB) OCH LEVERANTÖRSREGISTRET (LEVA)           
001000*        UPPDATERINGAR GÖRS I SEPARAT BMP-PROGRAM                         
001100*                                                                         
001200*    ÄNDRING 041014: SEKTION U TILLAGD SER TILL ATT KDAVT                 
001300                     INTE BLIR = FÖRRÄN ALLA AVTAL ÄR BORTA.              
001400                     //JOHAN NIHLBLAD                                     
001500*    SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP3                                                                
001800 DATA DIVISION.                                                           
001900     EJECT                                                                
002000 WORKING-STORAGE SECTION.                                                 
002100     SKIP2                                                                
002200*    -- CHECKED BY WY2000                                                 
002300 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
002400*                                                                         
002500 01  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                     
002600 01  WS-IDAVTAL              PIC S9(13)  VALUE ZERO  COMP-3.              
002700 01  WS-ANTAL-AVTAL-RAKN     PIC 9(2)    VALUE ZERO.                      
002800                                                                          
002900*01  -COPY WWDCKONS                                                       
003000                                                                          
003100*                                                                         
003200 01  KONSTANTER.                                                          
003300     03  JA                  PIC X       VALUE 'J'.                       
003400     03  NEJ                 PIC X       VALUE 'N'.                       
003500*    -COPY W212CALL                                                       
003600     EJECT                                                                
003700 01  NYCKEL-BEGREPP.                                                      
003800     03  W-IDARTNR-X.                                                     
003900         05  W-IDARTNR       PIC S9(9)               COMP-3.              
004000     03  W-WDD901KY-X.                                                    
004100         05  W-IDARTNR-D9    PIC S9(9)               COMP-3.              
004200         05  W-IDDC-D9       PIC X(2).                                    
004300     03  W-IDLEVNR-X.                                                     
004400         05  W-IDLEVNR       PIC X(5)    VALUE SPACE.                     
004500     03  W-IDBEST-X.                                                      
004600         05  W-IDBEST        PIC S9(13)              COMP-3.              
004700     03  W-IDAVTAL-X.                                                     
004800         05  W-IDAVTAL       PIC S9(13)              COMP-3.              
004900***  03  W-FLAVRART          PIC X.                                       
005000     03  W-KDERS-0-X.                                                     
005100         05    FILLER        PIC S9(3)   VALUE ZERO  COMP-3.              
005200*                                                                         
005300 01  SUB-PROGRAM.                                                         
005400     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
005500     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
005600     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
005700     SKIP1                                                                
005800 01  TEST-IDAVTAL            PIC 9(13).                                   
005900 01  FILLER REDEFINES TEST-IDAVTAL.                                       
006000     03  FILLER              PIC 9.                                       
006100     03  TEST-PREFIX         PIC 9(3).                                    
006200     03  TEST-ORDERNR        PIC 9(6).                                    
006300     03  FILLER REDEFINES TEST-ORDERNR.                                   
006400         05  ORDERNR-POS1    PIC 9.                                       
006500         05  FILLER          PIC 9(5).                                    
006600     03  TEST-SUFFIX         PIC 9(3).                                    
006700     SKIP1                                                                
006800 01  INDEX-AREA.                                                          
006900     03  IX-BEST             PIC S9(1)               COMP-3.              
007000     SKIP1                                                                
007100 01  FLAGGOR.                                                             
007200     03  RAETT-SEGM          PIC X       VALUE SPACE.                     
007300     SKIP2                                                                
007400 01  AVTAL-FINNS             PIC X(1)    VALUE 'N'.                       
007500*                                                                         
007600*    ARBETS-AREOR TILL IMS-SEKTIONERNA                                    
007700*                                                                         
007800 01  IMS-WS.                                                              
007900     03    FILLER            PIC X(16)   VALUE '     IMS-VS     '.        
008000     SKIP2                                                                
008100*                                STATUSKOD FRÅN IMS                       
008200     03  STATUS-WS           PIC X(2).                                    
008300       88  SEGMENT-FINNS                 VALUE '  '.                      
008400       88  SEGMENT-SAKNAS                VALUE 'GE'.                      
008500     EJECT                                                                
008600     03  GODK-STATUSKODER.                                                
008700         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
008800     SKIP3                                                                
008900     03  SSA1                PIC X(64).                                   
009000     03  SSA2                PIC X(64).                                   
009100     03  SSA3                PIC X(64).                                   
009200     03  SSA4                PIC X(64).                                   
009300     03  SSA5                PIC X(64).                                   
009400     EJECT                                                                
009500*    -COPY W0003.                                                         
009600                                                                          
009700     EJECT                                                                
009800 01  FILLER                  PIC X(16)   VALUE 'IO-AREA-01      '.        
009900 01  DLI-IO-AREA-01.                                                      
010000     SKIP3                                                                
010100*03  WLARTC01 -COPY WDK601                                                
010200                                                                          
010300     EJECT                                                                
010400 01  FILLER                  PIC X(16)   VALUE 'IO-AREA-11      '.        
010500 01  DLI-IO-AREA-11.                                                      
010600     SKIP3                                                                
010700*03  WLARTC11 -COPY WDK611                                                
010800                                                                          
010900 01  FILLER                  PIC X(16)   VALUE 'IO-AREA-601     '.        
011000 01  DLI-IO-AREA-601.                                                     
011100     SKIP3                                                                
011200*03  WDK601 -COPY WDK601 -PRE 2-                                          
011300                                                                          
011400 01  FILLER                  PIC X(16)   VALUE 'IO-AREA-611     '.        
011500 01  DLI-IO-AREA-611.                                                     
011600     SKIP3                                                                
011700*03  WDK611 -COPY WDK611 -PRE 2-                                          
011800                                                                          
011900 01  FILLER                  PIC X(16)   VALUE 'IO-AREA-623     '.        
012000 01  DLI-IO-AREA-623.                                                     
012100     SKIP3                                                                
012200*03  WDK623 -COPY WDK623 -PRE 2-                                          
012300                                                                          
012400     EJECT                                                                
012500 01  FILLER                  PIC X(16)   VALUE 'IO-AREA         '.        
012600 01  FILLER.                                                              
012700 03  DLI-IO-AREA             PIC X(200)  VALUE SPACE.                     
012800     SKIP3                                                                
012900*03  WLARTC22 -COPY WDK622     -RED DLI-IO-AREA.                          
013000     EJECT                                                                
013100*03  WLARTC23 -COPY WDK623     -RED DLI-IO-AREA.                          
013200     EJECT                                                                
013300*03  WLINLB01 -COPY WDD901     -PRE INLB01- -RED DLI-IO-AREA.             
013400     EJECT                                                                
013500*03  WLINLB11 -COPY WDD902     -PRE LEVPL- -RED DLI-IO-AREA.              
013600     EJECT                                                                
013700*03  WLLEVA01 -COPY WDF101     -PRE LEVNR- -RED DLI-IO-AREA.              
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000     SKIP3                                                                
014100 01  LINK-AREA.                                                           
014200     03  LINK-KDCALL         PIC S9(3)           COMP-3.                  
014300     03    FILLER            PIC X(111).                                  
014400     SKIP3                                                                
014500*01  LINK-001 -COPY W212L001   -PRE L001- -RED LINK-AREA.                 
014600     EJECT                                                                
014700*01  LINK-002 -COPY W212L002   -PRE L002- -RED LINK-AREA.                 
014800     EJECT                                                                
014900*01  LINK-003 -COPY W212L003   -PRE L003- -RED LINK-AREA.                 
015000     EJECT                                                                
015100*01  LINK-004 -COPY W212L004   -PRE L004- -RED LINK-AREA.                 
015200     EJECT                                                                
015300*01  LINK-005 -COPY W212L005   -PRE L005- -RED LINK-AREA.                 
015400     EJECT                                                                
015500*01  LINK-006 -COPY W212L006   -PRE L006- -RED LINK-AREA.                 
015600     EJECT                                                                
015700*01  LINK-008 -COPY W212L008   -PRE L008- -RED LINK-AREA.                 
015800     EJECT                                                                
015900*    PCB (-ER)                                                            
016000*                                                                         
016100*01  -COPY W0008       -PRE ARTC-.                                        
016200     05    FILLER            PIC X(14).                                   
016300     EJECT                                                                
016400*01  -COPY W0008       -PRE INLB-.                                        
016500     05    FILLER            PIC X(14).                                   
016600     EJECT                                                                
016700*01  -COPY W0008        -PRE LEVA-.                                       
016800     05    FILLER            PIC X(3).                                    
016900     EJECT                                                                
017000*01  -COPY W0008       -PRE WDK6-.                                        
017100     05    FILLER            PIC X(14).                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION USING LINK-AREA ARTC-PCB INLB-PCB LEVA-PCB            
017400                          WDK6-PCB.                                       
017500 SUBRTN SECTION.                                                          
017600     SKIP3                                                                
017700     EVALUATE LINK-KDCALL                                                 
017800        WHEN HAMTA-ARTIKEL                                                
017900                PERFORM A-HAMTA-ARTIKEL                                   
018000        WHEN HAMTA-MTRLF-INFO                                             
018100                PERFORM B-HAMTA-MTRLF-INFO                                
018200        WHEN HAMTA-BEST-INFO-KEY                                          
018300                PERFORM C-HAMTA-BEST-INFO-KEY                             
018400        WHEN HAMTA-AVT-INFO-KEY                                           
018500                PERFORM D-HAMTA-AVT-INFO-KEY                              
018600        WHEN HAMTA-BEST-INFO                                              
018700                PERFORM H-HAMTA-BEST-INFO                                 
018800        WHEN HAMTA-NEXT-BEST-INFO                                         
018900                PERFORM T-HAMTA-NEXT-BESTINFO                             
019000        WHEN HAMTA-ANTAL-AVTAL                                            
019100                PERFORM U-HAMTA-ANTAL-AVTAL                               
019200        WHEN OTHER                                                        
019300             DISPLAY 'W2120210 FELAKTIGT VÄRDE PÅ KDCALL: '               
019400             LINK-KDCALL                                                  
019500             MOVE 16 TO RKOD                                              
019600             CALL ABEND USING RKOD                                        
019700     END-EVALUATE                                                         
019800     SKIP3                                                                
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-HAMTA-ARTIKEL SECTION.                                                 
020400     SKIP3                                                                
020500     MOVE L001-IDARTNR TO W-IDARTNR                                       
020600     MOVE NEJ TO L001-FLJANEJ-ARTIKEL                                     
020700     SKIP1                                                                
020800     PERFORM IMS-GET-ARTIKEL-SEGM                                         
020900     IF SEGMENT-FINNS                                                     
021000             MOVE JA TO L001-FLJANEJ-ARTIKEL                              
021100     END-IF                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 B-HAMTA-MTRLF-INFO SECTION.                                              
021500     SKIP3                                                                
021600     MOVE L002-IDARTNR TO W-IDARTNR                                       
021700     MOVE L002-IDLEVNR TO W-IDLEVNR                                       
021800     MOVE NEJ TO L002-FLJANEJ-LEVNR                                       
021900     MOVE NEJ TO L002-FLJANEJ-LEVPLAN                                     
022000     MOVE LOW-VALUE TO L002-IO-AREA                                       
022100     MOVE NEJ       TO L002-PARMA-OCH-KONV                                
022200     MOVE SPACE     TO L002-IDLEVNR-MOTSV                                 
022300*                                                                         
022400     PERFORM IMS-GET-ARTIKEL-SEGM                                         
022500     MOVE ART-IDLEVNR  TO L002-IDLEVNR-REG                                
022600     MOVE ART-KDPRODSL TO L002-KDPRODSL                                   
022700     PERFORM IMS-GET-MTRLF-SEGM                                           
022800*                                                                         
022900     MOVE CLAG-KDAVT     TO L002-KDAVT                                    
023000     MOVE CLAG-KDKSP     TO L002-KDKSP                                    
023100     MOVE CLAG-IDANSK    TO L002-IDANSK                                   
023200     MOVE CLAG-IDINK     TO L002-IDINK                                    
023300     MOVE CLAG-FLAVRART  TO L002-FLAVRART                                 
023400     MOVE CLAG-IDLEVNR-SHIP TO L002-IDLEVNR-SHIP                          
023500                                                                          
023600     MOVE NEJ TO AVTAL-FINNS                                              
023700     PERFORM IMS-GET-AVT-SEGM-OKVAL                                       
023800     PERFORM UNTIL SEGMENT-SAKNAS OR AVTAL-FINNS = JA                     
023900         IF L002-IDLEVNR-REG = AVT-IDLEVNR-AVT                            
024000            MOVE AVT-IDLEVNR-AVT TO L002-AVTAL-IDLEVNR-AVT                
024100            MOVE AVT-IDAVTAL TO L002-AVTAL-IDAVTAL                        
024200            MOVE JA TO AVTAL-FINNS                                        
024300         ELSE                                                             
024400            PERFORM IMS-GET-AVT-SEGM-OKVAL                                
024500         END-IF                                                           
024600     END-PERFORM                                                          
024700     IF AVTAL-FINNS = NEJ                                                 
024800        MOVE ZERO TO L002-AVTAL-IDAVTAL                                   
024900     END-IF                                                               
025000*                                                                         
025100     PERFORM IMS-GET-LEVNR-SEGM                                           
025200*                                                                         
025300     IF SEGMENT-FINNS                                                     
025400         MOVE JA TO L002-FLJANEJ-LEVNR                                    
025500         PERFORM BA-KOLL-GSDB-IDLEVNR                                     
025600*                                                                         
025700         MOVE W-IDARTNR      TO W-IDARTNR-D9                              
025710         MOVE WC-CDC-SE      TO W-IDDC-D9                                 
025800         PERFORM IMS-GET-LEVPLAN-SEGM                                     
025900         IF SEGMENT-FINNS                                                 
026000             MOVE JA TO L002-FLJANEJ-LEVPLAN                              
026100             MOVE LEVPL-KVBR TO L002-KVBR                                 
026200         ELSE                                                             
026300             MOVE NEJ TO L002-FLJANEJ-LEVPLAN                             
026400         END-IF                                                           
026500     ELSE                                                                 
026600         MOVE NEJ TO L002-FLJANEJ-LEVNR                                   
026700         MOVE NEJ TO L002-FLJANEJ-LEVPLAN                                 
026800     END-IF                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 BA-KOLL-GSDB-IDLEVNR  SECTION.                                           
027200     SKIP3                                                                
027300*    OM LEVNR ÄR ETT PARMA-IDLEVNR OCH                                    
027400*    IDLEVNR-MOTSV ÄR ETT GSDB-IDLEVNR                                    
027500*    SÅ SKALL INTE LEVNR GODKÄNNAS                                        
027600                                                                          
027700     MOVE L002-IDLEVNR TO WS-IDLEVNR                                      
027800     INSPECT WS-IDLEVNR REPLACING ALL SPACE BY ZERO                       
027900     IF WS-IDLEVNR NUMERIC AND                                            
028000        LEVNR-LEV-IDLEVNR-MOTSV > SPACE                                   
028100        MOVE NEJ TO L002-FLJANEJ-LEVNR                                    
028200        MOVE JA  TO L002-PARMA-OCH-KONV                                   
028300        MOVE SPACE  TO L002-IDLEVNR-MOTSV                                 
028400***        DVS ATT DETTA ÄR ETT PARMA-ID SOM REDAN ÄR KONV.               
028500     ELSE                                                                 
028600*       GSDB-LEV MED MOTSV=NUM => FYLL I MOTSV                            
028700        MOVE LEVNR-LEV-IDLEVNR-MOTSV TO WS-IDLEVNR                        
028800        INSPECT WS-IDLEVNR REPLACING ALL SPACE BY ZERO                    
028900        IF LEVNR-LEV-IDLEVNR-MOTSV > SPACE AND                            
029000           WS-IDLEVNR NUMERIC                                             
029100           MOVE LEVNR-LEV-IDLEVNR-MOTSV TO L002-IDLEVNR-MOTSV             
029200        ELSE                                                              
029300           MOVE SPACE                   TO L002-IDLEVNR-MOTSV             
029400        END-IF                                                            
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 C-HAMTA-BEST-INFO-KEY SECTION.                                           
029900     SKIP3                                                                
030000     MOVE L003-IDARTNR TO W-IDARTNR                                       
030100*                                                                         
030200     PERFORM IMS-GET-MTRLF-SEGM                                           
030300*                                                                         
030400     MOVE 1 TO IX-BEST                                                    
030500     PERFORM IMS-GET-BEST-SEGM-OKVAL                                      
030600*                                                                         
030700     PERFORM UNTIL SEGMENT-SAKNAS OR IX-BEST > 7                          
030800         MOVE BEST-IDBEST TO L003-TAB-IDBEST (IX-BEST)                    
030900         MOVE BEST-IDLEVNR-BEST TO L003-TAB-IDLEVNR (IX-BEST)             
031000         MOVE BEST-TIBEST TO L003-TAB-TIBEST (IX-BEST)                    
031100         MOVE BEST-KDBEH-BEST TO L003-TAB-KDBEH-BEST (IX-BEST)            
031200         ADD 1 TO IX-BEST                                                 
031300         PERFORM IMS-GET-BEST-SEGM-OKVAL                                  
031400     END-PERFORM                                                          
031500*                                                                         
031600     MOVE IX-BEST  TO L003-ANTAL-BEST                                     
031700     SUBTRACT 1  FROM L003-ANTAL-BEST                                     
031800     .                                                                    
031900     EJECT                                                                
032000 D-HAMTA-AVT-INFO-KEY SECTION.                                            
032100     SKIP3                                                                
032200     MOVE L003-IDARTNR TO W-IDARTNR                                       
032300*                                                                         
032400     PERFORM IMS-GET-MTRLF-SEGM                                           
032500*                                                                         
032600     MOVE 1 TO IX-BEST                                                    
032700     PERFORM IMS-GET-AVT-SEGM-OKVAL                                       
032800*                                                                         
032900     PERFORM UNTIL SEGMENT-SAKNAS OR IX-BEST > 5                          
033000         MOVE AVT-IDAVTAL       TO L003-TAB-IDBEST (IX-BEST)              
033100         MOVE AVT-IDLEVNR-AVT   TO L003-TAB-IDLEVNR(IX-BEST)              
033200         ADD 1 TO IX-BEST                                                 
033300         PERFORM IMS-GET-AVT-SEGM-OKVAL                                   
033400     END-PERFORM                                                          
033500*                                                                         
033600     MOVE IX-BEST  TO L003-ANTAL-BEST                                     
033700     SUBTRACT 1  FROM L003-ANTAL-BEST                                     
033800     .                                                                    
033900     EJECT                                                                
034000 H-HAMTA-BEST-INFO SECTION.                                               
034100     SKIP3                                                                
034200     MOVE L004-IDARTNR  TO W-IDARTNR                                      
034300     MOVE L004-IDBEST   TO W-IDBEST                                       
034400***  MOVE L004-FLAVRART TO W-FLAVRART                                     
034500*                                                                         
034600     PERFORM IMS-GET-BEST-SEGM                                            
034700*                                                                         
034800     MOVE BEST-IDBEST       TO L004-IDBEST                                
034900     MOVE BEST-IDLEVNR-BEST TO L004-IDLEVNR-BEST                          
035000     MOVE BEST-KDBEH-BEST   TO L004-KDBEH-BEST                            
035100     MOVE BEST-KVBEST       TO L004-KVBEST                                
035200     MOVE BEST-TIBEST       TO L004-TIBEST                                
035300     MOVE JA TO L004-FLJANEJ-BESTINFO                                     
035400     .                                                                    
035500     EJECT                                                                
035600 T-HAMTA-NEXT-BESTINFO SECTION.                                           
035700     SKIP3                                                                
035800     MOVE L004-IDARTNR  TO W-IDARTNR                                      
035900     MOVE L004-IDBEST   TO W-IDBEST                                       
036000***  MOVE L004-FLAVRART TO W-FLAVRART                                     
036100*                                                                         
036200     PERFORM IMS-GET-NEXT-BEST-SEGM                                       
036300*                                                                         
036400     IF SEGMENT-FINNS                                                     
036500       MOVE JA TO L004-FLJANEJ-BESTINFO                                   
036600       MOVE BEST-IDBEST       TO L004-IDBEST                              
036700       MOVE BEST-IDLEVNR-BEST TO L004-IDLEVNR-BEST                        
036800       MOVE BEST-KDBEH-BEST   TO L004-KDBEH-BEST                          
036900       MOVE BEST-KVBEST       TO L004-KVBEST                              
037000       MOVE BEST-TIBEST       TO L004-TIBEST                              
037100     ELSE                                                                 
037200       MOVE NEJ TO L004-FLJANEJ-BESTINFO                                  
037300     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600****** ÄT ETRACKER 1382465                                                
037700 U-HAMTA-ANTAL-AVTAL SECTION.                                             
037800     MOVE ZERO TO WS-ANTAL-AVTAL-RAKN                                     
037900     MOVE NEJ TO L001-FLAGGA-AVTAL                                        
038000     MOVE ZERO TO WS-IDAVTAL                                              
038100     MOVE L001-IDARTNR    TO W-IDARTNR                                    
038200     PERFORM IMS-GU-WDK601                                                
038300     PERFORM IMS-GNP-WDK623                                               
038400     PERFORM UNTIL SEGMENT-SAKNAS                                         
038500       IF 2-AVT-IDAVTAL = WS-IDAVTAL                                      
038600          CONTINUE                                                        
038700       ELSE                                                               
038800          ADD +1          TO WS-ANTAL-AVTAL-RAKN                          
038900       END-IF                                                             
039000       MOVE 2-AVT-IDAVTAL TO WS-IDAVTAL                                   
039100       PERFORM IMS-GNP-WDK623                                             
039200     END-PERFORM                                                          
039300     IF WS-ANTAL-AVTAL-RAKN > 1                                           
039400        MOVE JA TO L001-FLAGGA-AVTAL                                      
039500     END-IF                                                               
039600                                                                          
039700     .                                                                    
039800     EJECT                                                                
039900****** SLUT ÄT                                                            
040000******************************************************************        
040100*                        IMS-SEKTIONER                           *        
040200*    SEKTIONERNA ÄR SKRIVNA I ORDNING EFTER DATABAS-STRUKTUREN:  *        
040300*    ARTIKEL                                                     *        
040400*    MATERIALFÖRSÖRJNING                                         *        
040500*    BESTÄLLNING                                                 *        
040600*    BEST-NOTERING                                               *        
040700*    AVTAL                                                       *        
040800*    AVTALS-NOTERING                                             *        
040900*    LEVERANSPLAN                                                *        
041000*    LEVERANTÖR                                                  *        
041100*    DELETE  I WLARTC                                            *        
041200*    REPLACE I WLARTC                                            *        
041300*                                                                *        
041400******************************************************************        
041500     SKIP3                                                                
041600 IMS-GET-ARTIKEL-SEGM SECTION.                                            
041700     SKIP2                                                                
041800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X '&KDERS    ='               
041900                                                W-KDERS-0-X ')'           
042000             DELIMITED BY SIZE INTO SSA5                                  
042100     MOVE '  GE' TO GODK-STATUSKODER                                      
042200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA5                   
042300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
042400     PERFORM IMS-STATUSKONTROLL                                           
042500     .                                                                    
042600     EJECT                                                                
042700 IMS-GET-MTRLF-SEGM SECTION.                                              
042800     SKIP2                                                                
042900     STRING 'WLARTC01*P(IDARTNR  =' W-IDARTNR-X ')'                       
043000             DELIMITED BY SIZE INTO SSA1                                  
043100     MOVE 'WLARTC11 ' TO SSA2                                             
043200     MOVE '  ' TO GODK-STATUSKODER                                        
043300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2             
043400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     SKIP3                                                                
043700     .                                                                    
043800 IMS-GET-BEST-SEGM-OKVAL SECTION.                                         
043900     SKIP2                                                                
044000     STRING 'WLARTC11(KDSEGKEY =1)'                                       
044100            DELIMITED BY SIZE INTO SSA1                                   
044200     MOVE 'WLARTC22 ' TO SSA2                                             
044300     MOVE '  GE' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
044500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     SKIP3                                                                
044800     .                                                                    
044900 IMS-GET-BEST-SEGM SECTION.                                               
045000     SKIP2                                                                
045100     MOVE 'WLARTC11*F  ' TO SSA1                                          
045200     STRING 'WLARTC22(IDBEST   =' W-IDBEST-X ')'                          
045300             DELIMITED BY SIZE INTO SSA2                                  
045400     MOVE '  ' TO GODK-STATUSKODER                                        
045500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1 SSA2               
045600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
045700     PERFORM IMS-STATUSKONTROLL                                           
045800     SKIP3                                                                
045900     .                                                                    
046000 IMS-GET-NEXT-BEST-SEGM SECTION.                                          
046100     SKIP2                                                                
046200     STRING 'WLARTC11(KDSEGKEY =1) '                                      
046300            DELIMITED BY SIZE INTO SSA1                                   
046400     STRING 'WLARTC22(IDBEST   =' W-IDBEST-X ')'                          
046500             DELIMITED BY SIZE INTO SSA2                                  
046600     MOVE '  GE' TO GODK-STATUSKODER                                      
046700     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1 SSA2               
046800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046900     PERFORM IMS-STATUSKONTROLL                                           
047000     .                                                                    
047100     EJECT                                                                
047200 IMS-GET-AVT-SEGM-OKVAL SECTION.                                          
047300     SKIP2                                                                
047400     STRING 'WLARTC11(KDSEGKEY =1)'                                       
047500            DELIMITED BY SIZE INTO SSA1                                   
047600     MOVE 'WLARTC23 ' TO SSA2                                             
047700     MOVE '  GE' TO GODK-STATUSKODER                                      
047800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
047900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     SKIP3                                                                
048200     .                                                                    
048300 IMS-GET-AVTAL-SEGM SECTION.                                              
048400     SKIP2                                                                
048500     MOVE   'WLARTC11  '  TO  SSA1                                        
048600     STRING 'WLARTC23*F(IDAVTAL  =' W-IDAVTAL-X ')'                       
048700             DELIMITED BY SIZE INTO SSA2                                  
048800     MOVE '  GE' TO GODK-STATUSKODER                                      
048900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1 SSA2               
049000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     EJECT                                                                
049400 IMS-GU-WDK601 SECTION.                                                   
049500     SKIP2                                                                
049600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
049700             DELIMITED BY SIZE INTO SSA1                                  
049800     MOVE '  GE' TO GODK-STATUSKODER                                      
049900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-601 SSA1                  
050000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050300     EJECT                                                                
050400 IMS-GNP-WDK623 SECTION.                                                  
050500     SKIP2                                                                
050600     STRING 'WDK611  (KDSEGKEY =1)'                                       
050700            DELIMITED BY SIZE INTO SSA1                                   
050800     MOVE 'WDK623   ' TO SSA2                                             
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-623 SSA1 SSA2            
051100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     SKIP3                                                                
051400     .                                                                    
051500 IMS-GET-LEVPLAN-SEGM SECTION.                                            
051600     SKIP2                                                                
051700     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
051800             DELIMITED BY SIZE INTO SSA1                                  
051900     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
052000             DELIMITED BY SIZE INTO SSA2                                  
052100     MOVE '  GE' TO GODK-STATUSKODER                                      
052200     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2                
052300     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
052400     PERFORM IMS-STATUSKONTROLL                                           
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-GET-LEVNR-SEGM SECTION.                                              
052800     SKIP2                                                                
052900     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
053000             DELIMITED BY SIZE INTO SSA4                                  
053100     MOVE '  GE' TO GODK-STATUSKODER                                      
053200     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA SSA4                      
053300     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
053400     PERFORM IMS-STATUSKONTROLL                                           
053500     .                                                                    
053600     EJECT                                                                
053700 IMS-STATUSKONTROLL SECTION.                                              
053800*                                                                         
053900     SET STATUS-IX TO 1                                                   
054000     SEARCH GODK-STATUS                                                   
054100        AT END CALL FELLOG                                                
054200        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
054300        CONTINUE                                                          
054400     END-SEARCH                                                           
054500     .                                                                    
