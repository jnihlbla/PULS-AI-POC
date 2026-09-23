000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W9103700.                                        
000400 AUTHOR.                 GUNNAR LARSSON, IDK.                             
000500 DATE-WRITTEN.           JUNI 1991.                                       
000600                                                                          
000700******************************************************************        
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      SKAPAR VR-TRANSAR FRÅN ORDERRADSKÖN (WDQ4).                        
001100*      - VILLKOREN FÖR ATT SKAPA POST PÅ VR-FIL ÄR I                      
001200*        PGM-KODEN MARKERADE MED TEXTEN 'VR-VILLKOR UPPFYLLT'.            
001300*                                                                         
001700*      INDATA:                                                            
001800*      - WDQ4         CBLTDLI                                             
002100*                                                                         
002200*      UTDATA:                                                            
002300*      - VR-TRANS-FIL W91037                                              
002400*                                                                         
002500******************************************************************        
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*    ---- UTFIL: VR-TRANSAR                                               
003400     SELECT  W91037        ASSIGN  W91037D1.                              
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W91037                                                               
004100     LABEL RECORD STANDARD                                                
004200     RECORDING  F                                                         
004300     BLOCK CONTAINS 0.                                                    
004400                                                                          
004500 01  W91037-POST -COPY W910B02   -L.                                      
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004701                                                                          
004710*    -- CHECKED BY WY2000                                                 
004800 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W9103700'.                
004900 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
005000     SKIP3                                                                
005100 01      FILLER              PIC X(16)   VALUE                            
005200                                         'W***************'.              
005300 01      W.                                                               
005400*                                                                         
005500  02     W-AAMMDD              PIC 9(6).                                  
005600  02     FILLER                REDEFINES W-AAMMDD.                        
005700   03    W-AAMMDD-AA           PIC 9(2).                                  
005800   03    W-AAMMDD-MM           PIC 9(2).                                  
005900   03    W-AAMMDD-DD           PIC 9(2).                                  
006000                                                                          
006100  02     W-AAVVD               PIC 9(5).                                  
006200  02     FILLER                REDEFINES W-AAVVD.                         
006300   03    W-AAVVD-AA            PIC 9(2).                                  
006400   03    W-AAVVD-VV            PIC 9(2).                                  
006500   03    W-AAVVD-D             PIC 9(1).                                  
006600                                                                          
007200  02     W-IDKUNDRF-RO         PIC X(10).                                 
007300  02     FILLER                REDEFINES W-IDKUNDRF-RO.                   
007400   03    W-IDKUNDRF-RO-1-7     PIC 9(7).                                  
007500   03    FILLER                PIC X(3).                                  
007600                                                                          
007700  02     W-ANT-B02             PIC S9(9) VALUE ZERO  COMP-3.              
007800     EJECT                                                                
007900 01      FILLER              PIC X(16)   VALUE                            
008000                                         'K-KONSTANTER****'.              
008100 01      K-KONSTANTER.                                                    
008200*                                                                         
008300  02     JA                  PIC X(1)    VALUE 'J'.                       
008400  02     NEJ                 PIC X(1)    VALUE 'N'.                       
008410  02     FTG-LV              PIC 9(2)    VALUE 03.                        
008420 01      WS-IDORDER          PIC 9(7)    VALUE ZERO.                      
008500     SKIP2                                                                
008510*      --- VALID IDDC CODES                                               
008520*                                                                         
008530*01    -COPY WWDC99                                                       
008540       EJECT                                                              
008600 01      FILLER              PIC X(16)   VALUE                            
008700                                         'DYNAM-SUBPGM****'.              
008800 01      DYNAM-SUBPGM.                                                    
008900*                                                                         
009000  02     ABEND               PIC X(8)    VALUE 'ABEND   '.                
009100  02     FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
009200  02     POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
009300  02     CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
009500  02     DATKORT             PIC X(8)    VALUE 'DATKORT '.                
009600  02     W009KSIF            PIC X(8)    VALUE 'W009KSIF'.                
009710  02     W460DIS1            PIC X(8)    VALUE 'W460DIS1'.                
009800     SKIP2                                                                
009900 01      FILLER              PIC X(16)   VALUE                            
010000                                         'KSIF************'.              
010100 01      KSIF.                                                            
010200*                                                                         
010300  02     KSIF-IDARTNR        PIC 9(9)    VALUE ZERO.                      
010400  02     KSIF-IDDISTR        PIC 9(4)    VALUE ZERO.                      
010500  02     KSIF-LNG            PIC 9(1)    VALUE ZERO.                      
010600  02     KSIF-REKSIFFR       PIC 9(1)    VALUE ZERO.                      
010700     SKIP2                                                                
010800*    ----  PARAMETRAR TILL ABEND                                          
010900     SKIP1                                                                
011000 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16 COMP SYNC.               
011100     EJECT                                                                
011700     EJECT                                                                
011800 01      FILLER              PIC X(16)   VALUE                            
011900                                         'B02*************'.              
012000 01  AREA   -COPY W910B02      -PRE B02-                                  
012500     EJECT                                                                
012600*    ----  PARAMETRAR TILL POSTSUM                                        
012700                                                                          
012800 01  -COPY W0005       -PRE POSTSUM-.                                     
012900     EJECT                                                                
013000*    ----  PARAMETRAR TILL DATUMKORT                                      
013100                                                                          
013200 01  DATUMKORT-ID            PIC X(6)   VALUE 'WDATUM'.                   
013300     SKIP3                                                                
013400 01  -COPY WDATKORT                                                       
013500     EJECT                                                                
013510*    ----  PARAMETRAR TILL W460DIS1                                       
013520                                                                          
013550*01  -COPY W460DIS1                                                       
013560     EJECT                                                                
013570*01  -COPY W460LISO                                                       
013580     EJECT                                                                
013600*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
013700                                                                          
013800 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
013900     SKIP3                                                                
014000*    ---- STATUSKOD FRÅN IMS                                              
014100                                                                          
014200 01  STATUS-WS               PIC XX.                                      
014300     88  SEGMENT-FINNS                    VALUE '  '.                     
014400     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
014500     88  SEGMENT-SLUT                     VALUE 'GB'.                     
014600     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
014700     88  IMS-EJ-OK                        VALUE 'XD'.                     
014800     SKIP3                                                                
014900 01  GODK-STATUSKODER.                                                    
015000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
015100     SKIP3                                                                
015200 01  SSA1                    PIC X(64).                                   
015300 01  SSA2                    PIC X(32).                                   
015400     EJECT                                                                
015500*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
015600                                                                          
015700 01      FILLER              PIC X(16)   VALUE                            
015800                                         'NYCKAR-TILL-DLI*'.              
015900 01      NYCKLAR-TILL-DLI.                                                
016000                                                                          
016100  02     W-IDARTNR-X.                                                     
016200   03    W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
016300                                                                          
016400  02     W-IDORDER-X.                                                     
016500   03    W-IDORDER           PIC S9(7)   VALUE ZERO  COMP-3.              
016600     EJECT                                                                
016700 01  -COPY W0003                                                          
016800     EJECT                                                                
016900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
017000     SKIP1                                                                
017100 01  DLI-IO-AREA.                                                         
017400 03  -COPY WDQ401      -PRE WDQ4-                                         
017500     EJECT                                                                
017600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA2'.              
017700     SKIP1                                                                
017800 01  DLI-IO-AREA2.                                                        
018100 03  WLORQI01 -COPY WDQ201      -PRE ORQI-                                
018200     EJECT                                                                
018500 01  FILLER                  PIC X(16) VALUE 'DW**************'.          
018600                                                                          
018700 01      DW.                                                              
018800  02     DW-IDORDER          PIC 9(7)-.                                   
018900     EJECT                                                                
019000 LINKAGE SECTION.                                                         
019100     SKIP2                                                                
019200 01  -COPY W0008      -PRE  WDQ4-                                         
019300       05  FILLER                PIC X.                                   
019400     EJECT                                                                
019500 01  -COPY W0008      -PRE  ORQI-                                         
019600       05  FILLER                PIC X.                                   
019700     SKIP2                                                                
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING  WDQ4-PCB ORQI-PCB.                            
020200     ENTRY 'DLITCBL' USING  WDQ4-PCB ORQI-PCB.                            
020300     SKIP2                                                                
020400     PERFORM A-INIT                                                       
020500                                                                          
020600     PERFORM IMS-GN-WDQ4-ORAD                                             
020700                                                                          
020800     PERFORM UNTIL (SEGMENT-SLUT)                                         
020900                                                                          
021000       MOVE WDQ4-ORAD-IDDISTR    TO DIS1-IDDISTR                          
021100                                                                          
021110       CALL W460DIS1 USING DIS1-W460DIS1                                  
021120                                                                          
021462       IF DIS1-IDLANDX2 = ISO-SPANIEN                                     
022300         CONTINUE                                                         
022400       ELSE                                                               
023000         MOVE WDQ4-ORAD-IDDISTR  TO B02-IDDISTR                           
023100         MOVE WDQ4-ORAD-IDKUNDNR TO B02-IDKUNDNR                          
023200                                                                          
023400         MOVE WDQ4-ORAD-IDKUNDRF-RO TO W-IDKUNDRF-RO                      
023500                                                                          
023600         EVALUATE TRUE                                                    
023900           WHEN W-IDKUNDRF-RO-1-7 > ZERO                                  
024000             MOVE W-IDKUNDRF-RO-1-7  TO B02-IDORDNR7                      
024100           WHEN OTHER                                                     
024200             MOVE WDQ4-ORAD-IDORDNR7 TO B02-IDORDNR7                      
024300         END-EVALUATE                                                     
024400                                                                          
024900         MOVE WDQ4-ORAD-IDORDER TO W-IDORDER                              
025000         PERFORM IMS-GU-ORQI-OHUV                                         
025010*FIX FÖR ATT LÄSA FÖRBI RADER DÅ ORDERHUVUDET Q2 SAKNAS.                  
025011*DÅ DENNA FIX ÖPPNAS GLÖM EJ ATT GODKÄNNA GE I LÄSNINGEN.                 
025020         IF SEGMENT-SAKNAS                                                
025021            MOVE W-IDORDER TO WS-IDORDER                                  
025030            DISPLAY 'IDORDER SAKNAS PÅ Q2 ' WS-IDORDER                    
025110         ELSE                                                             
025200         IF  WDQ4-ORAD-KDORDKL < 5                                        
025300         AND (ORQI-OHUV-FLOVRLEV = NEJ)                                   
025400         AND (ORQI-OHUV-KDFAKTYP = 'R' OR 'N' OR 'K')                     
025500*----------------------------------------- VR-VILLKOR UPPFYLLT            
025600           PERFORM B-SKAPA-B02                                            
025700         END-IF                                                           
025800         END-IF                                                           
025810*FIX                                                                      
025900       END-IF                                                             
026000                                                                          
026100       PERFORM IMS-GN-WDQ4-ORAD                                           
026200     END-PERFORM                                                          
026300                                                                          
026400     PERFORM Z-FINIT                                                      
026500                                                                          
026600     MOVE ZERO TO RETURN-CODE                                             
026700     GOBACK                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 A-INIT SECTION.                                                          
027100     SKIP2                                                                
027200     OPEN OUTPUT W91037                                                   
027300                                                                          
027400     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
027500                                                                          
027600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
027700     MOVE D-AAR    TO W-AAMMDD-AA                                         
027800     MOVE D-MAANAD TO W-AAMMDD-MM                                         
027900     MOVE D-DAG    TO W-AAMMDD-DD                                         
028000     MOVE D-AAR    TO W-AAVVD-AA                                          
028100     MOVE D-VECKA  TO W-AAVVD-VV                                          
028200     MOVE D-DAGNR  TO W-AAVVD-D                                           
028300                                                                          
028400     MOVE ZERO     TO W-ANT-B02                                           
028500     .                                                                    
028600     EJECT                                                                
028700 B-SKAPA-B02 SECTION.                                                     
028800     SKIP2                                                                
028900     MOVE 'B02'                  TO B02-IDPTYP                            
029000                                                                          
030000     MOVE WDQ4-ORAD-IDDC       TO WS-IDDC                                 
030100     IF  CDC-SE                                                           
030200         MOVE 71                 TO B02-IDSUPPL                           
030300         MOVE 1                  TO B02-REKSUPPL                          
030400     ELSE                                                                 
030500         MOVE 72                 TO B02-IDSUPPL                           
030600         MOVE 9                  TO B02-REKSUPPL                          
030700     END-IF                                                               
031700                                                                          
031800     MOVE WDQ4-ORAD-IDARTNR      TO B02-IDARTNR                           
031900     MOVE WDQ4-ORAD-IDARTNR      TO KSIF-IDARTNR                          
032000     MOVE 9                      TO KSIF-LNG                              
032100     CALL  W009KSIF   USING         KSIF-IDARTNR                          
032200                                    KSIF-LNG                              
032300                                    KSIF-REKSIFFR                         
032400     MOVE KSIF-REKSIFFR          TO B02-REKSIFFR                          
032500                                                                          
032600     MOVE WDQ4-ORAD-KVBEART-Q    TO B02-KVBEART                           
032700                                                                          
032800     EVALUATE TRUE                                                        
032900       WHEN WDQ4-ORAD-TIRODAT  > ZERO                                     
033000         MOVE WDQ4-ORAD-TIRODAT  TO B02-TIAAMMDD                          
033100       WHEN WDQ4-ORAD-TITPO    > ZERO                                     
033200         MOVE WDQ4-ORAD-TITPO    TO B02-TIAAMMDD                          
033300       WHEN OTHER                                                         
033400         MOVE WDQ4-ORAD-TIREGDAT TO B02-TIAAMMDD                          
033500     END-EVALUATE                                                         
033600                                                                          
034100     MOVE WDQ4-ORAD-KDORDKL      TO B02-KDORDER                           
034200                                                                          
034700     IF  W-IDKUNDRF-RO-1-7  > ZERO                                        
034710       MOVE 1                   TO B02-KDRO                               
034720     ELSE                                                                 
034721       MOVE ZERO                TO B02-KDRO                               
034730     END-IF                                                               
036000                                                                          
036001     MOVE WDQ4-ORAD-KDVRINFO     TO B02-KDVRINFO                          
036010                                                                          
036100     PERFORM S01-SKRIV-B02                                                
036200     .                                                                    
036300     EJECT                                                                
036400 Z-FINIT SECTION.                                                         
036500     SKIP2                                                                
036600     CLOSE  W91037                                                        
036700                                                                          
036800     MOVE 'S' TO POSTSUM-OPKOD                                            
036900     CALL POSTSUM USING POSTSUM-PARM                                      
037000     .                                                                    
037100     EJECT                                                                
037200 S01-SKRIV-B02 SECTION.                                                   
037300     SKIP2                                                                
037400     WRITE W91037-POST FROM B02-AREA                                      
037500                                                                          
037600     MOVE 'W91037'           TO POSTSUM-FDNAMN                            
037700     MOVE 'W91037D1'         TO POSTSUM-DDNAMN2                           
037800     MOVE 'B02'              TO POSTSUM-TRANSTYP                          
037900     CALL  POSTSUM   USING      POSTSUM-PARM                              
038000                                                                          
038100     ADD +1                  TO W-ANT-B02                                 
038200     .                                                                    
038300     EJECT                                                                
038400*    ---- IMS SEKTIONER                                                   
038500     SKIP2                                                                
038600 IMS-GN-WDQ4-ORAD SECTION.                                                
038700                                                                          
038800     MOVE 'WDQ401   ' TO SSA1                                             
038900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
039000     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-AREA SSA1                      
039100     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039400     SKIP2                                                                
039500 IMS-GU-ORQI-OHUV SECTION.                                                
039600                                                                          
039700     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
039800            DELIMITED BY SIZE INTO SSA1                                   
039900     MOVE '  GE' TO GODK-STATUSKODER                                      
040000*    MOVE '  ' TO GODK-STATUSKODER                                        
040100     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA2 SSA1                     
040200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
040300     PERFORM IMS-STATUSKONTROLL                                           
041200     .                                                                    
041300     EJECT                                                                
041500                                                                          
042500 IMS-STATUSKONTROLL SECTION.                                              
042600                                                                          
042700     SET STATUS-IX TO 1                                                   
042800     SEARCH GODK-STATUS                                                   
042900       AT  END                                                            
043000         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
043100           DELIMITED BY SIZE INTO FELTEXT                                 
043200         CALL FELLOG                                                      
043300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
043400     END-SEARCH.                                                          
