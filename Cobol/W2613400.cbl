000300     SKIP2                                                                
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.                 W2613400.                                    
000600*              PROGRAM CONVERTED BY                                       
000700*              COBOL CONVERSION AID PO 5785-ABJ                           
000800*              CONVERSION DATE 05/25/91 10:51:03.                         
000900*AUTHOR.                     IDK INGVAR CARLSSON.                         
001000*DATE-WRITTEN.               MARS 1979.                                   
001100*    SKIP2                                                                
001200*    REMARKS.                                                             
001300*                                                                         
002200*    FUNKTION.                                                            
002300*                                                                         
002400*        PROGRAMMET SKAPAR LISTPOSTER LAGERBALANSERING                    
002500*                                                                         
002600*                                                                         
002700*    SUBPROGRAM.                                                          
002800*        W2613410    SKÖTER ALL IMS-HANTERING.                            
002900*                                                                         
003000*    RETURKODER.                                                          
003100*    25              FUNKTIONGRUPPTABELL FÖR LITEN.                       
003200*    1000            WDD1-FIL OCH WDD6-FIL ÖVERENSSTÄMMER EJ              
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 INPUT-OUTPUT SECTION.                                                    
003700 FILE-CONTROL.                                                            
004400                                                                          
004500*--------------------------------------- FIL INNEHÅLLANDE ARTIKEL-        
004600*                                        INFORMATION FÖR LTK-UPP-         
004700*                                        FÖLJNING OCH LAGERBALANS-        
004800*                                        ERING                            
004900*                                        INPUT                            
005000                                                                          
005100     SELECT W26131 ASSIGN UT-S-W26134D1.                                  
005200                                                                          
005300*--------------------------------------- FIL INNEHÅLLANDE ORDER-          
005400*                                        INGÅNGS INFORMATION FÖR          
005500*                                        LTK-BALANSERING OCH              
005600*                                        LAGERBALANSERING                 
005700*                                        INPUT                            
005800                                                                          
005900     SELECT W26133 ASSIGN UT-S-W26134D2.                                  
006000     EJECT                                                                
006100                                                                          
006200*--------------------------------------- LISTPOSTER                       
006300*                                        LAGERBALANSERING                 
006400*                                        W26150-001                       
006500                                                                          
006600     SELECT W26137 ASSIGN UT-S-W26134D4.                                  
006610                                                                          
006611*                                        KARANTÄN                         
006620     SELECT W26138 ASSIGN UT-S-W26134D8.                                  
006700                                                                          
007300*--------------------------------------- LISTPOSTER                       
007400*                                        LAGERBALANSERING                 
007500*                                        W26136-001                       
007600                                                                          
007700     SELECT W26139 ASSIGN UT-S-W26134D6.                                  
007800                                                                          
007801*                                        KARANTÄN                         
007810     SELECT W26140 ASSIGN UT-S-W26134D9.                                  
007820                                                                          
007900*--------------------------------------- LISTPOSTER                       
008000*                                        LAGERBALANSERING                 
008100*                                        W26138-001                       
008200                                                                          
008300     SELECT W26141 ASSIGN UT-S-W26134D7.                                  
008310                                                                          
008400*                                        KARANTÄN                         
008410     SELECT W26142 ASSIGN UT-S-W26134DA.                                  
008420                                                                          
008500                                                                          
008600*---------------------------------------- FIL INNEHÅLLANDE                
008700*                                         LTK-UPPFÖLJNING                 
008800*                                                                         
009800                                                                          
009900     EJECT                                                                
010000 DATA DIVISION.                                                           
010100 FILE SECTION.                                                            
010200                                                                          
011100 FD  W26131                                                               
011200     RECORDING F                                                          
011300     BLOCK 0                                                              
011400                          .                                               
011500                                                                          
011600*01  POST -COPY W261224    -PRE ART- -L                                   
011800     SKIP3                                                                
011900 FD  W26133                                                               
012000     RECORDING F                                                          
012100     BLOCK 0                                                              
012200                          .                                               
012300                                                                          
012400*01  POST -COPY W261225    -PRE OI- -L                                    
012600     EJECT                                                                
012700     SKIP3                                                                
012800 FD  W26137                                                               
012900     RECORDING F                                                          
013000     BLOCK 0                                                              
013100                          .                                               
013200                                                                          
013300*01  POST -COPY W261LI02   -PRE LB- -L                                    
013400     SKIP3                                                                
013500 FD  W26138                                                               
013600     RECORDING F                                                          
013700     BLOCK 0                                                              
013800                          .                                               
013900                                                                          
014000*01  POST -COPY W261LI02   -PRE LBK- -L                                   
014300     EJECT                                                                
014400 FD  W26139                                                               
014500     RECORDING F                                                          
014600     BLOCK 0                                                              
014700                          .                                               
014800                                                                          
014900*01  POST -COPY W2613601   -PRE L1- -L                                    
015100     SKIP3                                                                
015110 FD  W26140                                                               
015120     RECORDING F                                                          
015130     BLOCK 0                                                              
015140                          .                                               
015150                                                                          
015160*01  POST -COPY W2613601   -PRE L1K- -L                                   
015170     SKIP3                                                                
015200 FD  W26141                                                               
015300     RECORDING F                                                          
015400     BLOCK 0                                                              
015500                          .                                               
015600                                                                          
015700*01  POST -COPY W2613801   -PRE L2- -L                                    
015800     SKIP3                                                                
015810 FD  W26142                                                               
015820     RECORDING F                                                          
015830     BLOCK 0                                                              
015840                          .                                               
015850                                                                          
015860*01  POST -COPY W2613801   -PRE L2K- -L                                   
015900     EJECT                                                                
017500 WORKING-STORAGE SECTION.                                                 
017501*    -COPY WY2000W2                                                       
017502     SKIP3                                                                
017700 77  INDENT-I PIC X(40) VALUE                                             
017800     'W2613400 91/05/25 TIME 09.40 VILMAII'.                              
017900***  STATEMENT ABOVE GENERATED BY VILMAII CONVERTER                       
018000*                                                                         
018100     SKIP2                                                                
018200 01  DAGENS-DATUM-AAVV       PIC 9(4).                                    
018300 01  ART-TILTK-AA            PIC 9(5)   VALUE ZERO.                       
018400 01  ART-TILTK-VV            PIC 9(5)   VALUE ZERO.                       
018500 01  DAGENS-DATUM-DEAAB      PIC S9(5)               COMP-3.              
018600 01  KDLTK-2-TEST-DATUM      PIC S9(5)               COMP-3.              
018700 01  W-OI-OTRA               PIC S9(9)               COMP-3.              
018800 01  KVPB-SEP-TOTAL          PIC S9(6)V9(1)          COMP-3.              
018900 01  RKOD                    PIC S9(4)               COMP SYNC.           
019000                                                                          
019100*--------------------------------------- ALLMÄNNA ARBETSAREOR             
019200 01  W.                                                                   
019300     05  W-TEST-DATUM        PIC S9(5).                                   
019400     05  FILLER              REDEFINES W-TEST-DATUM.                      
019500         10  W-TEST-AAR      PIC 9(2).                                    
019600         10  W-TEST-VECKA    PIC 9(2).                                    
019700         10  W-TEST-DAG      PIC S9.                                      
019800     05  W-DATUM-OLD.                                                     
019900         10  W-AAR-OLD       PIC 9(2).                                    
020000         10  W-VECKA-OLD     PIC S9(2).                                   
020100         10  FILLER          PIC 9.                                       
020200     05  W-DDATUM-AAAAVVD    PIC 9(7).                                    
020201     05  FILLER REDEFINES W-DDATUM-AAAAVVD.                               
020202     07  W-DDATUM-SEKEL      PIC 9(2).                                    
020210     07  W-DDATUM-AAVVD      PIC 9(5).                                    
020300     07  FILLER              REDEFINES W-DDATUM-AAVVD.                    
020400         10  W-DDATUM-AA     PIC 9(2).                                    
020500         10  W-DDATUM-VV     PIC 9(2).                                    
020600         10  W-DDATUM-D      PIC 9.                                       
020700     07  FILLER              REDEFINES W-DDATUM-AAVVD.                    
020800         10  W-DDATUM-AAVV   PIC 9(4).                                    
020900         10  FILLER          PIC X.                                       
020910     05  W-DAFINLV           PIC 9(7).                                    
020920     05  FILLER  REDEFINES   W-DAFINLV.                                   
020930         07  W-DAFINLV-SEKEL PIC 9(2).                                    
020940         07  W-DAFINLV-AAVVD PIC 9(5).                                    
020950         07  FILLER REDEFINES W-DAFINLV-AAVVD.                            
020960             10  W-DAFINLV-AA  PIC 9(2).                                  
020970             10  FILLER        PIC 9(3).                                  
021000     05  W-ORSAK             PIC S9                  COMP SYNC.           
021100     05  W-KVDISP-TOT        PIC S9(7)               COMP-3.              
021200     05  W-KVPB-TOT          PIC S9(7)V9             COMP-3.              
021300     05  W-KVAKS-TOT         PIC S9(7)               COMP-3.              
021400     05  W-KVOI-TOT          PIC S9(7)               COMP-3.              
021500     05  W-KVOTR-TOT         PIC S9(9)               COMP-3.              
021600     05  W-KVSLAGER          PIC S9(7)               COMP-3.              
021700     05  W-BER-MAX           PIC S9(7)               COMP-3.              
021800     05  W-VARDE-MAX         PIC S9(7)               COMP-3.              
022300     05  W-KVOT-8            PIC S9(7)               COMP-3.              
022400     05  W-KVOT-16           PIC S9(7)               COMP-3.              
022500     05  W-KVOI              PIC S9(7)               COMP-3.              
022600     05  W-KVOI-PP           PIC S9(7)               COMP-3.              
023100     SKIP3                                                                
023200*--------------------------------------- SWITCHAR                         
023300 01  SWITCHAR.                                                            
023500     05  SW-LTK2-REGLER-UPPF PIC X.                                       
023600     05  SW-TRAFF-FKN-TAB    PIC X.                                       
023800     SKIP2                                                                
023900*--------------------------------------- INDEXFÄLT                        
024000 01  INDEXFALT.                                                           
024100     05  IX                  PIC S9(9)               COMP SYNC.           
024200     SKIP3                                                                
024300*--------------------------------------- KONSTANTER                       
024400 01  KONSTANTER.                                                          
024500     05  JA                  PIC X       VALUE 'J'.                       
024600     05  NEJ                 PIC X       VALUE 'N'.                       
024700     05  W-26-VECKOR         PIC S9(3)   VALUE +26.                       
024800     05  W-52-VECKOR         PIC S9(3)   VALUE +52.                       
024900     05  LAS-ART-INFO        PIC S9(3)   VALUE +5    COMP-3.              
025000     SKIP3                                                                
025100*--------------------------------------- IDBEGREPP FÖR ART-POST           
025200 01  ARTID.                                                               
025300     05  ARTID-IDARTNR       PIC S9(9)               COMP-3.              
025400     SKIP3                                                                
025500*--------------------------------------- IDBEGREPP FÖR OI-POST            
025600 01  OIID.                                                                
025700     05  OIID-IDARTNR        PIC S9(9)               COMP-3.              
025800     SKIP3                                                                
025900*--------------------------------------- DYNAMISKA SUBPROGRAM             
026000 01  DYNAMISKA-SUBPROGRAM.                                                
026100     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
026200     05  W2613410            PIC X(8)    VALUE 'W2613410'.                
026300     05  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
026400     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
026500     05  ABEND               PIC X(8)    VALUE 'ABEND   '.                
026600     EJECT                                                                
026700*01  -COPY W0005 -PRE POSTSUM-                                            
026900     EJECT                                                                
027000*--------------------------------------- PARAMETRAR TILL DATKORT          
027100                                                                          
027200 01  PROGRAM-NAMN            PIC X(8)    VALUE 'W26134'.                  
027300                                                                          
027400 01  DATUMKORT-ID            PIC X(8)    VALUE 'WDATUM'.                  
027500                                                                          
027600*01  -COPY WDATKORT                                                       
027800     EJECT                                                                
027900*--------------------------------------- PARAMETRAR TILL W009VADD         
028000                                                                          
028100 01  W009VADD-DATUM          PIC S9(5)               COMP-3.              
028200                                                                          
028300 01  W009VADD-ANTAL          PIC S9(3)               COMP-3.              
029600     EJECT                                                                
029700*--------------------------------------- AREA FÖR W26131                  
029800                                                                          
029900*01  AREA -COPY W261224    -PRE ART-                                      
030100     EJECT                                                                
030700*--------------------------------------- AREA FÖR W26133                  
030800                                                                          
030900*01  AREA -COPY W261225    -PRE OI-                                       
031100     EJECT                                                                
031200*--------------------------------------- AREA FÖR W26137                  
031300                                                                          
031400*01  AREA -COPY W261LI02   -PRE LB-                                       
031600     EJECT                                                                
031700*01  AREA -COPY W261L005   -PRE IMS1-                                     
031900     EJECT                                                                
032000*--------------------------------------- AREA FÖR W26139                  
032100                                                                          
032200*01       -COPY W2613601     -PRE L1-                                     
032400     EJECT                                                                
032500*--------------------------------------- AREA FÖR W26141                  
032600                                                                          
032700*01       -COPY W2613801     -PRE L2-                                     
032900     EJECT                                                                
033900                                                                          
034000 LINKAGE SECTION.                                                         
034100     SKIP2                                                                
034200*01  -COPY W0008 -PRE WLINLB-                                             
034400     05  FILLER              PIC X.                                       
034500     EJECT                                                                
034600*01  -COPY W0008 -PRE WLBENA-                                             
034800     05  FILLER              PIC X.                                       
034900     EJECT                                                                
034910*01  -COPY W0008 -PRE WDD8-                                               
034920     05  FILLER              PIC X.                                       
034930     EJECT                                                                
035000 PROCEDURE DIVISION USING   WLINLB-PCB WLBENA-PCB WDD8-PCB.               
035100     ENTRY 'DLITCBL' USING  WLINLB-PCB WLBENA-PCB WDD8-PCB.               
035200     SKIP2                                                                
035300     PERFORM A-INITIERING                                                 
035400     PERFORM B-LAS-ART                                                    
035500     PERFORM C-LAS-OI                                                     
035600                                                                          
035700     PERFORM UNTIL                                                        
035800      NOT ( ARTID NOT = HIGH-VALUE OR OIID NOT = HIGH-VALUE )             
035900       IF ARTID < OIID                                                    
036000         PERFORM D-BEHANDLA-ARTIKEL                                       
036100         PERFORM B-LAS-ART                                                
036200       ELSE                                                               
036300         IF ARTID = OIID                                                  
036400           PERFORM D-BEHANDLA-ARTIKEL                                     
036500           PERFORM B-LAS-ART                                              
036600           PERFORM C-LAS-OI                                               
036700         ELSE                                                             
036800           PERFORM C-LAS-OI                                               
036900         END-IF                                                           
037000       END-IF                                                             
037100     END-PERFORM                                                          
037200     PERFORM Z-AVSLUTNING                                                 
037300     MOVE ZERO TO RETURN-CODE                                             
037400     GOBACK                                                               
037500     CONTINUE.                                                            
037600     EJECT                                                                
037700******************************************************************        
037800*                                                                *        
037900*    INITIERING                                                  *        
038000*    ÖPPNA FUNKTIONSGRUPPFILEN                                   *        
038100*                                                                *        
038200******************************************************************        
038300                                                                          
038400 A-INITIERING SECTION.                                                    
038500                                                                          
038600     OPEN INPUT                                                           
038700                W26131                                                    
038800                W26133                                                    
039000         OUTPUT W26137                                                    
039100                W26138                                                    
039110                W26139                                                    
039120                W26140                                                    
039200                W26141                                                    
039300                W26142                                                    
039500                                                                          
039600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
039700                                                                          
039800     MOVE D-AAR TO W-DDATUM-AA                                            
039900     MOVE D-VECKA TO W-DDATUM-VV                                          
040000     MOVE D-DAG TO W-DDATUM-D                                             
040100     COMPUTE DAGENS-DATUM-AAVV = D-AAR * 100 + D-VECKA                    
040110     IF W-DDATUM-AA > 50                                                  
040120        MOVE 19     TO W-DDATUM-SEKEL                                     
040130     ELSE                                                                 
040131        MOVE 20     TO W-DDATUM-SEKEL                                     
040140     END-IF                                                               
040200                                                                          
040300                                                                          
042300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
042400     CONTINUE.                                                            
044600     EJECT                                                                
044700******************************************************************        
044800*                                                                *        
044900*    LAS ART                                                     *        
045000*    LÄSER W26131                                                *        
045100*                                                                *        
045200******************************************************************        
045300                                                                          
045400 B-LAS-ART SECTION.                                                       
045500                                                                          
045600     READ W26131 INTO ART-AREA                                            
045700          AT END MOVE HIGH-VALUE TO ARTID                                 
045800     END-READ                                                             
045900                                                                          
046000     IF ARTID NOT = HIGH-VALUE                                            
046100       MOVE ART-IDARTNR TO ARTID-IDARTNR                                  
046200       MOVE 'W26131' TO POSTSUM-FDNAMN                                    
046300       MOVE 'W26134D1' TO POSTSUM-DDNAMN2                                 
046400       MOVE ART-IDPTYP TO POSTSUM-TRANSTYP                                
046500       CALL POSTSUM USING POSTSUM-PARM                                    
046600       IF ART-TILTK = ZERO                                                
046700         MOVE ZERO     TO ART-TILTK-AA                                    
046800                          ART-TILTK-VV                                    
046900       ELSE                                                               
047000         COMPUTE ART-TILTK-AA = ART-TILTK / 100                           
047100         COMPUTE ART-TILTK-VV = ART-TILTK - ART-TILTK-AA * 100            
047200         CONTINUE                                                         
047300       END-IF                                                             
047400     END-IF                                                               
047500                                                                          
049100     CONTINUE.                                                            
049200     EJECT                                                                
049300******************************************************************        
049400*                                                                *        
049500*    LAS OI                                                      *        
049600*    LÄSER W26133                                                *        
049700*                                                                *        
049800******************************************************************        
049900                                                                          
050000 C-LAS-OI SECTION.                                                        
050100                                                                          
050200     READ W26133 INTO OI-AREA                                             
050300          AT END MOVE HIGH-VALUE TO OIID                                  
050400     END-READ                                                             
050500                                                                          
050600     IF  OIID NOT = HIGH-VALUE                                            
050700       MOVE OI-IDARTNR TO OIID-IDARTNR                                    
050800       MOVE 'W26133' TO POSTSUM-FDNAMN                                    
050900       MOVE 'W26134D2' TO POSTSUM-DDNAMN2                                 
051000       MOVE OI-IDPTYP TO POSTSUM-TRANSTYP                                 
051100       CALL POSTSUM USING POSTSUM-PARM                                    
051200     END-IF                                                               
051300     CONTINUE.                                                            
051400     EJECT                                                                
051500******************************************************************        
051600*                                                                *        
051700*    BEHANDLA ARTIKEL                                            *        
051800*                                                                *        
051900******************************************************************        
052000                                                                          
052100 D-BEHANDLA-ARTIKEL SECTION.                                              
052200                                                                          
052300*                                                                         
052400     COMPUTE W-KVDISP-TOT =( ART-KVLS (1) + ART-KVLS (2))                 
052500                 - ( ART-KVRESS (1) + ART-KVRESS (2)                      
052600                 + ART-KVOKS-BULK(1) + ART-KVOKS-BULK(2)                  
052700                 + ART-KVOKS-DAG (1) + ART-KVOKS-DAG (2)                  
052800                 + ART-KVOKS-VOR (1) + ART-KVOKS-VOR (2) )                
052900     COMPUTE W-KVPB-TOT = ART-KVPB-SEP (1) + ART-KVPB-SEP (2)             
053000                        + ART-KVPB-SATS (1) + ART-KVPB-SATS (2)           
053100     COMPUTE W-KVSLAGER = ART-KVSLAGER (1) + ART-KVSLAGER (2)             
053200*                                                                         
053300     MOVE ZERO TO W-KVOI-TOT                                              
053400     IF ARTID = OIID                                                      
053500       MOVE +1 TO IX                                                      
053600       PERFORM UNTIL                                                      
053700        NOT ( IX < +9 )                                                   
053800         COMPUTE W-KVOI-TOT = OI-OING-PROGNOSPAV-C1 (IX)                  
053900             + OI-OING-DIVERSE-C1 (IX) + OI-OING-SATS-C1 (IX)             
054000             + OI-OING-SDC-C2     (IX) + OI-OING-NDC-C2  (IX)             
054100                                          + W-KVOI-TOT                    
054200         ADD +1 TO IX                                                     
054300       END-PERFORM                                                        
054400     END-IF                                                               
054500     PERFORM DB-TEST-FOR-LAGERBALANSERING                                 
054600     PERFORM DC-TEST-FOR-W26136-001                                       
054700     PERFORM DD-TEST-FOR-W26138-001                                       
054900     CONTINUE.                                                            
055000     EJECT                                                                
055200******************************************************************        
055300*                                                                *        
055400*    TEST FÖR LAGERBALANSERING                                   *        
055500*                                                                *        
055600******************************************************************        
055700                                                                          
055800 DB-TEST-FOR-LAGERBALANSERING SECTION.                                    
055900                                                                          
056200     PERFORM DBC-SKAPA-LARM-LB                                            
056300     CONTINUE.                                                            
056400     EJECT                                                                
064100******************************************************************        
064200*                                                                *        
064300*    SKAPA LARM LB                                               *        
064400*    SKAPA LISTPOST LAGERBALANSERING                             *        
064500*                                                                *        
064600******************************************************************        
064700                                                                          
064800 DBC-SKAPA-LARM-LB SECTION.                                               
064900                                                                          
065000     MOVE ART-IDARTNR TO IMS1-IDARTNR                                     
065100     MOVE LAS-ART-INFO TO IMS1-KDCALL                                     
065200     CALL W2613410 USING IMS1-AREA WLINLB-PCB WLBENA-PCB                  
065210                                   WDD8-PCB                               
065300*                                                                         
065400     IF ART-KDVVKL < +4                                                   
065500                                                                          
065600       IF (ART-FLSKROT-BEORD (1) NOT = JA) AND                            
065700          (ART-FLSKROT-BEORD (2) NOT = JA)                                
065800                                                                          
065900         IF W-KVPB-TOT > +0                                               
066000                                                                          
066100           IF ART-KDERS (1) = 00 OR 09 OR 19 OR 29                        
066200             IF ART-KDVVKL = +1                                           
066300               COMPUTE W-BER-MAX = (3 * W-KVOI-TOT)                       
066400               + W-KVSLAGER                                               
066500             ELSE                                                         
066600               EVALUATE TRUE                                              
066700               WHEN ART-KDVVKL = +2                                       
066800                 COMPUTE W-BER-MAX = (3 * W-KVOI-TOT / 2)                 
066900                 + W-KVSLAGER                                             
067000               WHEN ART-KDVVKL = +3                                       
067100                 COMPUTE W-BER-MAX = (3 * W-KVOI-TOT / 5)                 
067200                 + W-KVSLAGER                                             
067300               END-EVALUATE                                               
067400             END-IF                                                       
067500             COMPUTE W-VARDE-MAX =                                        
067600                     (W-KVDISP-TOT - W-BER-MAX) *                         
067700                     ART-PRARTSTD                                         
067800             IF (W-VARDE-MAX > 500 OR NOT > +0)                           
067900               CONTINUE                                                   
068000             ELSE                                                         
068100               IF ART-KDERS (1) = ZERO                                    
068101                 MOVE ART-TIFINLV   TO W-DAFINLV-AAVVD                    
068102                 IF W-DAFINLV-AA > 50                                     
068103                    MOVE 19     TO W-DAFINLV-SEKEL                        
068104                 ELSE                                                     
068105                    MOVE 20     TO W-DAFINLV-SEKEL                        
068106                 END-IF                                                   
068200                 IF W-DDATUM-AAAAVVD - W-DAFINLV > 3000                   
068300                   MOVE ZERO TO W-KVOTR-TOT                               
068400                   MOVE +1 TO IX                                          
068500                   IF ARTID = OIID                                        
068600                     PERFORM UNTIL                                        
068700                      NOT ( IX < 17 )                                     
068800                       COMPUTE W-KVOTR-TOT =                              
069000                                 OI-OTRA-PROGNOSPAV-C1 (IX)               
069100                               + OI-OTRA-DIVERSE-C1 (IX)                  
069500                               + W-KVOTR-TOT                              
069600                       ADD +1 TO IX                                       
069700                     END-PERFORM                                          
069800                   END-IF                                                 
069900                   IF W-KVOTR-TOT < 4                                     
070000                     PERFORM DBCA-FLYTTA-W26137                           
070500                   END-IF                                                 
070600                 END-IF                                                   
070700               ELSE                                                       
070800                 PERFORM DBCA-FLYTTA-W26137                               
070900               END-IF                                                     
071000             END-IF                                                       
071100           END-IF                                                         
071200         END-IF                                                           
071300       END-IF                                                             
071400     END-IF                                                               
071500     CONTINUE.                                                            
071600     EJECT                                                                
071700 DBCA-FLYTTA-W26137 SECTION.                                              
071800*                                                                         
071900     MOVE 1 TO LB-IDLISTA                                                 
072000     MOVE ART-IDANSK TO LB-IDANSK                                         
072100     MOVE ART-IDLEVNR TO LB-IDLEVNR                                       
072200     MOVE ART-IDARTNR TO LB-IDARTNR                                       
072300     MOVE ART-TIFINLV TO LB-TIFINLV                                       
072400     MOVE ART-PRARTSTD TO LB-PRARTSTD                                     
072500     MOVE ZERO       TO LB-KVSKKNST                                       
072600     MOVE IMS1-TEXT-BEART  TO LB-BEART-SVE                                
072700                                                                          
072800     MOVE ART-TIURPROD TO LB-TIURPROD                                     
072900     MOVE ART-KDERS (1) TO LB-KDERS (1)                                   
073000     MOVE ART-KDERS (2) TO LB-KDERS (2)                                   
073100                                                                          
073200     MOVE IMS1-KVBR TO LB-KVBR                                            
073300     MOVE ART-FLJANEJ-C2 TO LB-FLJANEJ-C2                                 
073400                                                                          
073500     MOVE 1 TO IX                                                         
073600     PERFORM UNTIL                                                        
073700      NOT ( IX < 3 )                                                      
073800       MOVE ART-KVLS (IX) TO LB-KVLS (IX)                                 
073900       MOVE ART-KVRESS (IX) TO LB-KVRESS (IX)                             
074000       MOVE ART-KVAKS (IX) TO LB-KVAKS (IX)                               
074100       MOVE ART-KVOKS-BULK (IX) TO LB-KVOKS-BULK (IX)                     
074200       MOVE ART-KVOKS-DAG (IX) TO LB-KVOKS-DAG (IX)                       
074300       MOVE ART-KVOKS-VOR (IX) TO LB-KVOKS-VOR (IX)                       
074400       MOVE ART-SUTPO-TOT (IX) TO LB-SUTPO-TOT (IX)                       
074500       MOVE ART-KVROS (IX) TO LB-KVROS (IX)                               
074600       MOVE ART-KVPB-SEP (IX) TO LB-KVPB-SEP (IX)                         
074700       MOVE ART-KVPB-SATS (IX) TO LB-KVPB-SATS (IX)                       
074800       ADD +1 TO IX                                                       
074900     END-PERFORM                                                          
075000     IF ARTID = OIID                                                      
075100       MOVE 1 TO IX                                                       
075200       PERFORM UNTIL                                                      
075300        NOT ( IX < 9 )                                                    
075400         MOVE OI-OING-PROGNOSPAV-C1  (IX) TO                              
075410              LB-KVOI-C1KUND-PROGNOS (IX)                                 
075420         MOVE OI-OING-DIVERSE-C1     (IX) TO                              
075421              LB-KVOI-C1KUND-DIVERSE (IX)                                 
075422         MOVE OI-OING-SATS-C1        (IX) TO                              
075423              LB-KVOI-C1KUND-SATS    (IX)                                 
075424         MOVE OI-OING-SDC-C2         (IX) TO                              
075425              LB-KVOI-C2KUND-PROGNOS (IX)                                 
075426         ADD  OI-OING-NDC-C2         (IX) TO                              
075427              LB-KVOI-C2KUND-PROGNOS (IX)                                 
075428         MOVE ZERO TO LB-KVOI-C2KUND-DIVERSE (IX)                         
075429         MOVE ZERO TO LB-KVOI-C2KUND-SATS    (IX)                         
075430                                                                          
075500         ADD +1 TO IX                                                     
075600       END-PERFORM                                                        
075700     ELSE                                                                 
075800       MOVE 1 TO IX                                                       
075900       PERFORM UNTIL                                                      
076000        NOT ( IX < 9 )                                                    
076100         MOVE ZERO TO LB-KVOI-C1KUND-PROGNOS (IX)                         
076200         MOVE ZERO TO LB-KVOI-C1KUND-DIVERSE (IX)                         
076300         MOVE ZERO TO LB-KVOI-C1KUND-SATS (IX)                            
076400         MOVE ZERO TO LB-KVOI-C2KUND-PROGNOS (IX)                         
076500         MOVE ZERO TO LB-KVOI-C2KUND-DIVERSE (IX)                         
076600         MOVE ZERO TO LB-KVOI-C2KUND-SATS (IX)                            
076700         ADD +1 TO IX                                                     
076800       END-PERFORM                                                        
076900     END-IF                                                               
076901                                                                          
076902*    KARANTÄNARTIKEL ?                                                    
076903     MOVE ZERO              TO LB-KVBUFF                                  
076910     IF ART-ADLAGOMR = 71 AND ART-ADPLATS = 7000                          
076920        MOVE ART-KVLS (1)   TO LB-KVBUFF                                  
076921        ADD  ART-KVLS (2)   TO LB-KVBUFF                                  
076922        PERFORM DBCAB-SKRIV-W26138                                        
076930     ELSE                                                                 
076940        IF IMS1-KVBUFF > ZERO                                             
076950           MOVE IMS1-KVBUFF TO LB-KVBUFF                                  
076951           PERFORM DBCAB-SKRIV-W26138                                     
076960        ELSE                                                              
076970*          EJ KARANTÄN !                                                  
077000           PERFORM DBCAA-SKRIV-W26137                                     
077010        END-IF                                                            
077020     END-IF                                                               
077100     .                                                                    
077200     EJECT                                                                
077300******************************************************************        
077400*                                                                *        
077500*    SKRIV W26137              KARANTÄNARTIKLAR:W26138           *        
077600*    SKRIV POST PÅ W26137                                        *        
077700*                                                                *        
077800******************************************************************        
077900                                                                          
078000 DBCAA-SKRIV-W26137 SECTION.                                              
078100                                                                          
078200     WRITE LB-POST FROM LB-AREA                                           
078300                                                                          
078400     MOVE 'W26137' TO POSTSUM-FDNAMN                                      
078500     MOVE 'W26134D4' TO POSTSUM-DDNAMN2                                   
078600     MOVE LB-IDLISTA TO POSTSUM-TRANSTYP                                  
078700     CALL POSTSUM USING POSTSUM-PARM                                      
078800     .                                                                    
078900     EJECT                                                                
078910 DBCAB-SKRIV-W26138 SECTION.                                              
078920                                                                          
078930     WRITE LBK-POST FROM LB-AREA                                          
078940                                                                          
078950     MOVE 'W26138'   TO POSTSUM-FDNAMN                                    
078960     MOVE 'W26134D8' TO POSTSUM-DDNAMN2                                   
078970     MOVE LB-IDLISTA TO POSTSUM-TRANSTYP                                  
078980     CALL POSTSUM USING POSTSUM-PARM                                      
078990     .                                                                    
078991     EJECT                                                                
079000******************************************************************        
079100*                                                                *        
079200*    POSTER TILL LISTA W26136-001                                *        
079300*                                                                *        
079400******************************************************************        
079500                                                                          
079600 DC-TEST-FOR-W26136-001 SECTION.                                          
079700                                                                          
079800     IF ART-KDERS (1) = 9 OR 19 OR 29                                     
079900       CONTINUE                                                           
080000     ELSE                                                                 
080100       EVALUATE TRUE                                                      
080200        WHEN ART-FLSKROT-BEORD (1) = JA OR                                
080300             ART-FLSKROT-BEORD (2) = JA                                   
080310          CONTINUE                                                        
080400        WHEN OTHER                                                        
080500         COMPUTE W-KVAKS-TOT = ART-KVAKS (1) + ART-KVAKS (2)              
080600         IF W-KVDISP-TOT > ZERO AND W-KVPB-TOT = ZERO                     
080700           PERFORM S01-SKRIV-W26136-001                                   
080800         ELSE                                                             
080900           EVALUATE TRUE                                                  
081000           WHEN ART-KDERS (1) > ZERO AND < 10                             
081100             PERFORM S01-SKRIV-W26136-001                                 
081200           WHEN ART-KDERS (1) > 10                                        
081300             IF W-KVDISP-TOT > ZERO                                       
081400               PERFORM S01-SKRIV-W26136-001                               
081500             ELSE                                                         
081600               EVALUATE TRUE                                              
081700               WHEN W-KVAKS-TOT > ZERO                                    
081800                 PERFORM S01-SKRIV-W26136-001                             
081900               END-EVALUATE                                               
082000             END-IF                                                       
082100           END-EVALUATE                                                   
082200         END-IF                                                           
082300       END-EVALUATE                                                       
082400     END-IF                                                               
082500     CONTINUE.                                                            
082600     EJECT                                                                
082700******************************************************************        
082800*                                                                *        
082900*    POSTER TILL LISTA W26138-001                                *        
083000*                                                                *        
083100******************************************************************        
083200                                                                          
083300 DD-TEST-FOR-W26138-001 SECTION.                                          
083400*                                                                         
083500     IF W-KVDISP-TOT > ZERO AND W-KVPB-TOT = ZERO                         
083600       CONTINUE                                                           
083700     ELSE                                                                 
083800       EVALUATE TRUE                                                      
083900        WHEN (ART-KDERS (1) > +0 AND < +9) OR (ART-KDERS                  
084000             (1) > +10 AND < +19) OR (ART-KDERS (1) > +20                 
084100             AND < +29)                                                   
084200        WHEN ART-FLSKROT-BEORD (1) = JA OR                                
084210             ART-FLSKROT-BEORD (2) = JA                                   
084220             CONTINUE                                                     
084400        WHEN OTHER                                                        
084500         MOVE W-DDATUM-AA TO W-AAR-OLD                                    
084600         COMPUTE W-VECKA-OLD = W-DDATUM-VV - W-26-VECKOR                  
084700         IF W-VECKA-OLD < ZERO                                            
084800           COMPUTE W-VECKA-OLD = W-VECKA-OLD + W-52-VECKOR                
084810           IF W-AAR-OLD = 00                                              
084820              MOVE 99     TO W-AAR-OLD                                    
084830           ELSE                                                           
084900              COMPUTE W-AAR-OLD = W-AAR-OLD - 1                           
084910           END-IF                                                         
085000           CONTINUE                                                       
085100         ELSE                                                             
085200           EVALUATE TRUE                                                  
085300           WHEN W-VECKA-OLD = ZERO                                        
085400             MOVE W-52-VECKOR TO W-VECKA-OLD                              
085420             IF W-AAR-OLD = 00                                            
085430                MOVE 99     TO W-AAR-OLD                                  
085440             ELSE                                                         
085450                COMPUTE W-AAR-OLD = W-AAR-OLD - 1                         
085460             END-IF                                                       
085600           END-EVALUATE                                                   
085700         END-IF                                                           
085720         IF W-AAR-OLD = 00                                                
085730            MOVE 99     TO W-AAR-OLD                                      
085740         ELSE                                                             
085750            COMPUTE W-AAR-OLD = W-AAR-OLD - 1                             
085760         END-IF                                                           
085900         MOVE W-AAR-OLD TO W-TEST-AAR                                     
086000         MOVE W-VECKA-OLD TO W-TEST-VECKA                                 
086100         MOVE W-DDATUM-D TO W-TEST-DAG                                    
086200*                                                                         
086201         MOVE W-TEST-DATUM   TO TMP1-YYWWD                                
086202         MOVE ART-TIFINLV    TO TMP2-YYWWD                                
086210         PERFORM WY2000P2                                                 
086300         IF TMP1-YYWWD > TMP2-YYWWD                                       
086400           IF ART-KDVVKL = +1                                             
086500             COMPUTE W-BER-MAX = (3 * W-KVOI-TOT) + W-KVSLAGER            
086600             IF W-KVDISP-TOT < W-BER-MAX                                  
086700               CONTINUE                                                   
086800             ELSE                                                         
086900               COMPUTE W-VARDE-MAX = (W-KVDISP-TOT - W-BER-MAX)           
087000                                   * ART-PRARTSTD                         
087100               IF W-VARDE-MAX > +499                                      
087200                 PERFORM S02-SKRIV-W26138-001                             
087300               END-IF                                                     
087400             END-IF                                                       
087500           ELSE                                                           
087600             EVALUATE TRUE                                                
087700             WHEN ART-KDVVKL = +2                                         
087800               COMPUTE W-BER-MAX = (3 * W-KVOI-TOT / 2) +                 
087900               W-KVSLAGER                                                 
088000               IF W-KVDISP-TOT < W-BER-MAX                                
088100                 CONTINUE                                                 
088200               ELSE                                                       
088300                 COMPUTE W-VARDE-MAX = (W-KVDISP-TOT - W-BER-MAX)         
088400                                         * ART-PRARTSTD                   
088500                 IF W-VARDE-MAX > +499                                    
088600                   PERFORM S02-SKRIV-W26138-001                           
088700                 END-IF                                                   
088800               END-IF                                                     
088900             WHEN ART-KDVVKL = +3                                         
089000               COMPUTE W-BER-MAX = (3 * W-KVOI-TOT / 5) +                 
089100               W-KVSLAGER                                                 
089200               IF W-KVDISP-TOT < W-BER-MAX                                
089300                 CONTINUE                                                 
089400               ELSE                                                       
089500                 COMPUTE W-VARDE-MAX = (W-KVDISP-TOT - W-BER-MAX)         
089600                                         * ART-PRARTSTD                   
089700                 IF W-VARDE-MAX > +499                                    
089800                   PERFORM S02-SKRIV-W26138-001                           
089900                 END-IF                                                   
090000               END-IF                                                     
090100             WHEN ART-KDVVKL = +4                                         
090200               COMPUTE W-BER-MAX = (3 * W-KVOI-TOT / 10) +                
090300               W-KVSLAGER                                                 
090400               IF W-KVDISP-TOT < W-BER-MAX                                
090500                 CONTINUE                                                 
090600               ELSE                                                       
090700                 COMPUTE W-VARDE-MAX = (W-KVDISP-TOT - W-BER-MAX)         
090800                                         * ART-PRARTSTD                   
090900                 IF W-VARDE-MAX > +9999                                   
091000                   PERFORM S02-SKRIV-W26138-001                           
091100                 END-IF                                                   
091200               END-IF                                                     
091300             WHEN ART-KDVVKL = +5                                         
091400               COMPUTE W-BER-MAX = (W-KVOI-TOT / 5) + W-KVSLAGER          
091500               IF W-KVDISP-TOT < W-BER-MAX                                
091600                 CONTINUE                                                 
091700               ELSE                                                       
091800                 COMPUTE W-VARDE-MAX = (W-KVDISP-TOT - W-BER-MAX)         
091900                                         * ART-PRARTSTD                   
092000                 IF W-VARDE-MAX > +29999                                  
092100                   PERFORM S02-SKRIV-W26138-001                           
092200                 END-IF                                                   
092300               END-IF                                                     
092400             END-EVALUATE                                                 
092500           END-IF                                                         
092600         END-IF                                                           
092700       END-EVALUATE                                                       
092800     END-IF                                                               
092900     CONTINUE.                                                            
093000     EJECT                                                                
104000 S01-SKRIV-W26136-001 SECTION.                                            
104010******************************************************************        
104020*                                                                *        
104030*    UTSKRIFT AV POSTER TILL W2613601 LISTAN, DVS W26139         *        
104040*                                                 W26140,KARANT  *        
104050******************************************************************        
104060                                                                          
104100*                                                                         
104200     MOVE 001              TO L1-IDLISTA                                  
104300     MOVE ART-IDARTNR      TO L1-IDARTNR                                  
104400     MOVE ART-IDANSK       TO L1-IDANSK                                   
104500     MOVE ART-KDLTK        TO L1-KDLTK                                    
104600     MOVE ART-PRARTSTD     TO L1-PRARTSTD                                 
104700     MOVE ART-IDLEVNR      TO L1-IDLEVNR                                  
104800     MOVE IMS1-KVBR        TO L1-KVBR                                     
104900     MOVE IMS1-TEXT-BEART  TO L1-BEART-SVE                                
105000*                                                                         
105100     MOVE 1 TO IX                                                         
105200     PERFORM UNTIL                                                        
105300      NOT ( IX < 3 )                                                      
105400       MOVE ART-KDERS (IX)     TO L1-KDERS (IX)                           
105500       MOVE ART-KVLS (IX)      TO L1-KVLS (IX)                            
105600       MOVE ART-KVRESS (IX)    TO L1-KVRESS (IX)                          
105700       MOVE ART-KVAKS (IX)     TO L1-KVAKS (IX)                           
105800       MOVE ART-KVOKS-BULK(IX) TO L1-KVOKS-BULK (IX)                      
105900       MOVE ART-KVOKS-DAG (IX) TO L1-KVOKS-DAG (IX)                       
106000       MOVE ART-KVOKS-VOR (IX) TO L1-KVOKS-VOR (IX)                       
106100       MOVE ART-SUTPO-TOT (IX) TO L1-SUTPO-TOT (IX)                       
106200       MOVE ART-KVPB-SEP (IX)  TO L1-KVPB-SEP (IX)                        
106300       MOVE ART-KVPB-SATS (IX) TO L1-KVPB-SATS (IX)                       
106400       ADD +1 TO IX                                                       
106500     END-PERFORM                                                          
106510                                                                          
106520     MOVE ZERO              TO L1-KVBUFF                                  
106530     IF ART-ADLAGOMR = 71 AND ART-ADPLATS = 7000                          
106540        MOVE ART-KVLS (1)   TO L1-KVBUFF                                  
106550        ADD  ART-KVLS (2)   TO L1-KVBUFF                                  
106561        WRITE L1K-POST    FROM L1-W2613601                                
106562        MOVE 'W26140'       TO POSTSUM-FDNAMN                             
106563        MOVE 'W26134D9'     TO POSTSUM-DDNAMN2                            
106564        MOVE L1-IDLISTA     TO POSTSUM-TRANSTYP                           
106565        CALL POSTSUM USING     POSTSUM-PARM                               
106570     ELSE                                                                 
106580        IF IMS1-KVBUFF > ZERO                                             
106590           MOVE IMS1-KVBUFF TO L1-KVBUFF                                  
106592           WRITE L1K-POST FROM L1-W2613601                                
106593           MOVE 'W26140'    TO POSTSUM-FDNAMN                             
106594           MOVE 'W26134D9'  TO POSTSUM-DDNAMN2                            
106595           MOVE L1-IDLISTA  TO POSTSUM-TRANSTYP                           
106596           CALL POSTSUM USING  POSTSUM-PARM                               
106597        ELSE                                                              
106598           WRITE L1-POST FROM L1-W2613601                                 
106599           MOVE 'W26139'   TO POSTSUM-FDNAMN                              
106600           MOVE 'W26134D6' TO POSTSUM-DDNAMN2                             
106601           MOVE L1-IDLISTA TO POSTSUM-TRANSTYP                            
106602           CALL POSTSUM USING POSTSUM-PARM                                
106603        END-IF                                                            
106604     END-IF                                                               
107200     .                                                                    
107300     EJECT                                                                
107400******************************************************************        
107500*                                                                *        
107600*    UTSKRIFT AV POSTER TILL W2613801 LISTAN, DVS W26141         *        
107700*                                                 W26142,KARANT  *        
107800******************************************************************        
107900                                                                          
108000 S02-SKRIV-W26138-001 SECTION.                                            
108100*                                                                         
108200     MOVE 001              TO L2-IDLISTA                                  
108300     MOVE ART-IDARTNR      TO L2-IDARTNR                                  
108400     MOVE ART-IDANSK       TO L2-IDANSK                                   
108500     MOVE ART-PRARTSTD     TO L2-PRARTSTD                                 
108600     MOVE ART-IDLEVNR      TO L2-IDLEVNR                                  
108700     MOVE IMS1-KVBR        TO L2-KVBR                                     
108800     MOVE IMS1-TEXT-BEART  TO L2-BEART-SVE                                
108900     MOVE ART-KDVVKL       TO L2-KDVVKL                                   
109000     MOVE W-KVOI-TOT       TO L2-KVOI-TOT                                 
109100*                                                                         
109200     MOVE 1 TO IX                                                         
109300     PERFORM UNTIL                                                        
109400      NOT ( IX < 3 )                                                      
109500       MOVE ART-KDERS (IX)     TO L2-KDERS (IX)                           
109600       MOVE ART-KVLS (IX)      TO L2-KVLS (IX)                            
109700       MOVE ART-KVRESS (IX)    TO L2-KVRESS (IX)                          
109800       MOVE ART-KVOKS-BULK(IX) TO L2-KVOKS-BULK (IX)                      
109900       MOVE ART-KVOKS-DAG (IX) TO L2-KVOKS-DAG (IX)                       
110000       MOVE ART-KVOKS-VOR (IX) TO L2-KVOKS-VOR (IX)                       
110100       MOVE ART-SUTPO-TOT (IX) TO L2-SUTPO-TOT (IX)                       
110200       MOVE ART-KVPB-SEP (IX)  TO L2-KVPB-SEP (IX)                        
110300       MOVE ART-KVPB-SATS (IX) TO L2-KVPB-SATS (IX)                       
110400       MOVE ART-KVSLAGER (IX)  TO L2-KVSLAGER (IX)                        
110500       MOVE ART-KVAKS (IX)     TO L2-KVAKS (IX)                           
110600       ADD +1 TO IX                                                       
110700     END-PERFORM                                                          
110710                                                                          
110720     MOVE ZERO              TO L2-KVBUFF                                  
110730     IF ART-ADLAGOMR = 71 AND ART-ADPLATS = 7000                          
110740        MOVE ART-KVLS (1)   TO L2-KVBUFF                                  
110750        ADD  ART-KVLS (2)   TO L2-KVBUFF                                  
110760        WRITE L2K-POST    FROM L2-W2613801                                
110770        MOVE 'W26142'       TO POSTSUM-FDNAMN                             
110780        MOVE 'W26134DA'     TO POSTSUM-DDNAMN2                            
110790        MOVE L2-IDLISTA     TO POSTSUM-TRANSTYP                           
110791        CALL POSTSUM USING     POSTSUM-PARM                               
110792     ELSE                                                                 
110793        IF IMS1-KVBUFF > ZERO                                             
110794           MOVE IMS1-KVBUFF TO L2-KVBUFF                                  
110795           WRITE L2K-POST FROM L2-W2613801                                
110796           MOVE 'W26142'    TO POSTSUM-FDNAMN                             
110797           MOVE 'W26134DA'  TO POSTSUM-DDNAMN2                            
110798           MOVE L2-IDLISTA  TO POSTSUM-TRANSTYP                           
110799           CALL POSTSUM USING  POSTSUM-PARM                               
110800        ELSE                                                              
110801           WRITE L2-POST FROM L2-W2613801                                 
110802           MOVE 'W26141'   TO POSTSUM-FDNAMN                              
110803           MOVE 'W26134D7' TO POSTSUM-DDNAMN2                             
110804           MOVE L2-IDLISTA TO POSTSUM-TRANSTYP                            
110805           CALL POSTSUM USING POSTSUM-PARM                                
110806        END-IF                                                            
110807     END-IF                                                               
111400     .                                                                    
111500     EJECT                                                                
120200                                                                          
120300******************************************************************        
120400*                                                                *        
120500*    AVSLUTNING                                                  *        
120600*    STÄNG FILER                                                 *        
120700*    SKRIV POSTSUMS RÄKNEVERK                                    *        
120800*                                                                *        
120900******************************************************************        
121000                                                                          
121100 Z-AVSLUTNING SECTION.                                                    
121200                                                                          
121300     CLOSE                                                                
121400           W26131                                                         
121500           W26133                                                         
121600           W26137                                                         
121700           W26138                                                         
121800           W26139                                                         
121810           W26140                                                         
121900           W26141                                                         
122000           W26142                                                         
122200                                                                          
122300     MOVE 'S' TO POSTSUM-OPKOD                                            
122400     CALL POSTSUM USING POSTSUM-PARM                                      
122500     CONTINUE.                                                            
122610     EJECT                                                                
122700*    -COPY WY2000P2                                                       
