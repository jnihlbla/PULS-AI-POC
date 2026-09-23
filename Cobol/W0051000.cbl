000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0051000.                                                
000300 AUTHOR.         LARS THELL.                                              
000400 DATE-WRITTEN.   95/02/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR SALDOINFORMATION FRÅN ARTIKELBASER                         
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001100*                              WLARTS (WDK7)                              
001200*                              WLARTM (WDK9)                              
001210*                              WDQ4C                                      
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T510                                              
001600*        MID:         W0I51001                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W0O51001                                            
002000*                                                                         
002100*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
002200*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W0051000'.            
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 01  FILLER                      PIC X(8)    VALUE 'ARB-FÄLT'.            
003700 01  ARBETSFAELT.                                                         
003800     03  SDC-IX                  PIC  9(2)   VALUE ZERO.                  
003900     03  WS-KVOKS-TOT            PIC S9(7)   VALUE ZERO COMP-3.           
004000     03  WS-KVDISP-CLAG          PIC S9(7)   VALUE ZERO COMP-3.           
004100     03  WS-KVDISP-SLAG          PIC S9(7)   VALUE ZERO COMP-3.           
004110     03  WS-KVOKS-PREL           PIC S9(7)   VALUE ZERO COMP-3.           
004120     03  WS-KVPB-TOT             PIC S9(7)V9 VALUE ZERO.                  
004121     03  WS-KVPB-TOT-INT         PIC S9(7)   VALUE ZERO.                  
004130     03  WS-KVPB-TOT-DISP        PIC Z(6)9.9 VALUE ZERO.                  
004200                                                                          
004300 01  WX-IDDC                     PIC X(2)    VALUE '  '.                  
004400                                                                          
004500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004600     88  NYCKLAR-OK                          VALUE 'J'.                   
004700     88  NYCKLAR-FEL                         VALUE 'N'.                   
004800                                                                          
004900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005000     88  EGEN-MID                            VALUE '0510'.                
005100     88  HELP-MID                            VALUE '0551'.                
005200     EJECT                                                                
005300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005400 01  GENERELLA-SUBPROGRAM.                                                
005500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     EJECT                                                                
006000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006100*01 -COPY WMSGINIT                                                        
006200     EJECT                                                                
006300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006400*01 -COPY WMEDAREA                                                        
006500     SKIP3                                                                
006600 01  MESSAGE-CODES.                                                       
006700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006800     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
006900     EJECT                                                                
007000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007300                                                                          
007400*01  MID -COPY W0I51001                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
007700                                                                          
007800*01  -COPY WMSGAREA                                                       
007900     EJECT                                                                
008000     03  MOD REDEFINES MSG-AREA.                                          
008100*      05  -COPY W0O51001    -PRE MOD-                                    
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008400                                                                          
008500*01  -COPY WMFSAREA                                                       
008600     EJECT                                                                
008700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*                                                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100                                                                          
009200 01  NYCKLAR-TILL-DLI.                                                    
009300     03  W-IDARTNR-X.                                                     
009400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009410                                                                          
009420     03  W-WDQ4CKY-FOM.                                                   
009430         05  W-Q4-IDARTNR-F      PIC  S9(9)    COMP-3.                    
009440         05  W-Q4-IDDC-F         PIC  X(2).                               
009460         05  FILLER              PIC  X(13)    VALUE LOW-VALUE.           
009461                                                                          
009462     03  W-WDQ4CKY-TOM.                                                   
009463         05  W-Q4-IDARTNR-T      PIC  S9(9)    COMP-3.                    
009464         05  W-Q4-IDDC-T         PIC  X(2).                               
009465         05  FILLER              PIC  X(13)    VALUE HIGH-VALUE.          
009500     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010010     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(128).                              
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTC11'.           
011200                                                                          
011300 01  DLI-IO-ARTC11.                                                       
011400*  03  -COPY WDK611                                                       
011500     EJECT                                                                
011600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTS11'.           
011700 01  DLI-IO-ARTS11.                                                       
011800*  03  -COPY WDK711                                                       
011900     EJECT                                                                
012000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTM01'.           
012100 01  DLI-IO-ARTM01.                                                       
012200*  03  -COPY WDK901   -PRE ARTM-                                          
012300     EJECT                                                                
012310 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDQ4C1'.           
012320 01  DLI-IO-WDQ4C1.                                                       
012330*  03  -COPY WDQ4C1                                                       
012340     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500*01  -COPY W0009   -PRE MSG-                                              
012600     EJECT                                                                
012700*01  -COPY W0008  -PRE USEA-                                              
012800     05  FILLER                  PIC X.                                   
012900                                                                          
013000*01  -COPY W0008  -PRE ARTC-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013300*01  -COPY W0008  -PRE ARTS-                                              
013400     05  FILLER                  PIC X.                                   
013500                                                                          
013600*01  -COPY W0008  -PRE ARTM-                                              
013700     05  FILLER                  PIC X.                                   
013710                                                                          
013720*01  -COPY W0008  -PRE WDQ4C-                                             
013730     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
014000                           ARTC-PCB ARTS-PCB ARTM-PCB WDQ4C-PCB.          
014100 MAIN SECTION.                                                            
014200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
014300                           ARTC-PCB ARTS-PCB ARTM-PCB WDQ4C-PCB.          
014400                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014800       PERFORM B-KOLLA-NYCKLAR                                            
014900       IF NYCKLAR-OK                                                      
015000         PERFORM F-LAES-VISA-INFO                                         
015100       END-IF                                                             
015200       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O51001 + 4                      
015300       PERFORM IMS-INSERT-MSG                                             
015400     END-IF                                                               
015500                                                                          
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK                                                               
015800     .                                                                    
015900     EJECT                                                                
016000 A-INIT SECTION.                                                          
016100                                                                          
016200     IF MSG-DUBBLA-TRANSKODER                                             
016300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I51001                 
016400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
016500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016600     ELSE                                                                 
016700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I51001                  
016800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
016900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017000     END-IF                                                               
017100                                                                          
017200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017500                                                                          
017600     MOVE LOW-VALUE TO MSG-AREA                                           
017700     MOVE 'W0O51001' TO MFS-IDMOD                                         
017800     MOVE '0510' TO MOD-IDTRANS                                           
017900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018000                                                                          
018100                                                                          
018200     IF EGEN-MID OR HELP-MID                                              
018300       CONTINUE                                                           
018400     ELSE                                                                 
018500       MOVE SPACE TO MFS-KDTRTYP                                          
018600       MOVE '7' TO MFS-IDPFK                                              
018700     END-IF                                                               
018800                                                                          
018900     IF MFS-SPLIT                                                         
018901        MOVE 'FCtot'       TO MOD-RUBKOL04                                
018910        MOVE 'P-Oks'       TO MOD-RUBKOL05                                
018920     ELSE                                                                 
018922        MOVE 'Oks-d'       TO MOD-RUBKOL04                                
018923        MOVE 'Oks-b'       TO MOD-RUBKOL05                                
018940     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 B-KOLLA-NYCKLAR SECTION.                                                 
019300                                                                          
019400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019500     MOVE '001'             TO MSGI-KDCALL                                
019600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
019700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
019800     MOVE '0510'            TO MSGI-IDTRANS                               
019900     IF EGEN-MID OR HELP-MID                                              
020000       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
020100     ELSE                                                                 
020200       IF MID-IDARTNR-IN NUMERIC                                          
020300         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
020400       END-IF                                                             
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
020800                                                                          
020900     MOVE JA TO NYCKLAR-SW                                                
021000                                                                          
021100*    -- KONTROLL AV IDARTNR                                               
021200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021300                                                                          
021400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021500       MOVE '7'         TO MFS-IDPFK                                      
021600       MOVE SPACE       TO MFS-KDTRTYP                                    
021610                           MSGI-SPAR-AREA                                 
021700     END-IF                                                               
021800     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
021900       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
022000     ELSE                                                                 
022100       MOVE NEJ TO NYCKLAR-SW                                             
022200     END-IF                                                               
022300                                                                          
022400     IF MID-IDDC-IN NOT = ALL '+'                                         
022500       MOVE MID-IDDC-IN     TO WX-IDDC                                    
022600     ELSE                                                                 
022700       IF MFS-NEXT                                                        
022800         MOVE MID-IDDC-NEXT   TO WX-IDDC                                  
022900       ELSE                                                               
022910         IF MFS-FIRST                                                     
022911           MOVE SPACE           TO WX-IDDC                                
022912         ELSE                                                             
022920           MOVE MID-IDDC-ENTER  TO WX-IDDC                                
023110         END-IF                                                           
023120       END-IF                                                             
023200     END-IF                                                               
023300                                                                          
023400     IF NYCKLAR-OK                                                        
023500       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
023600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
023700       MOVE WX-IDDC         TO MOD-IDDC-UT                                
023710                               MOD-IDDC-ENTER                             
023800     ELSE                                                                 
023900       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
024000       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
024100     END-IF                                                               
024200                                                                          
024300     IF NYCKLAR-FEL                                                       
024400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
024500       CALL WMEDKONV USING MED-WMEDAREA                                   
024600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024700       PERFORM MFS-RENSA-FAELT-UT                                         
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 F-LAES-VISA-INFO SECTION.                                                
025200                                                                          
025300     PERFORM IMS-GU-ARTC01                                                
025400                                                                          
025500     IF SEGMENT-SAKNAS                                                    
025600        MOVE ARTIKEL-SAKNAS        TO MED-IDMFSFEL                        
025700        CALL WMEDKONV USING MED-WMEDAREA                                  
025800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
025900        PERFORM MFS-RENSA-FAELT-UT                                        
026000     ELSE                                                                 
026100        PERFORM FC-BEHANDLA-WDK9                                          
026200        PERFORM FA-BEHANDLA-WDK6                                          
026300        PERFORM FB-BEHANDLA-WDK7                                          
026400     END-IF                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 FA-BEHANDLA-WDK6  SECTION.                                               
026800                                                                          
026900     PERFORM IMS-GNP-ARTC11                                               
027000     IF SEGMENT-FINNS                                                     
027100                                                                          
027200        COMPUTE WS-KVDISP-CLAG =  CLAG-KVLS  -                            
027300                                  WS-KVOKS-TOT -                          
027400                                  CLAG-KVRESS                             
027500                                                                          
027600                                                                          
027700        MOVE CLAG-KVLS         TO MOD-KVLS-CDC                            
027800        MOVE WS-KVDISP-CLAG    TO MOD-KVDISP-CDC                          
027900        MOVE CLAG-KVUTRS       TO MOD-KVUTRS-CDC                          
028000        MOVE CLAG-KVRESS       TO MOD-KVRESS-CDC                          
028100        MOVE CLAG-KVEFRS       TO MOD-KVEFRS-CDC                          
028200        MOVE CLAG-KVAKS-CDC    TO MOD-KVAKS-CDC                           
028300        MOVE CLAG-KVAKS-PAV    TO MOD-KVAKS-PAV-CDC                       
028400        MOVE CLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL-CDC                    
028500        MOVE CLAG-KVROS        TO MOD-KVROS-CDC                           
028600        MOVE CLAG-KVSPANT      TO MOD-KVSPANT                             
028700     ELSE                                                                 
028800        PERFORM MFS-RENSA-WDK6-FAELT                                      
028900     END-IF                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 FB-BEHANDLA-WDK7  SECTION.                                               
029300                                                                          
029400     PERFORM IMS-GU-ARTS01                                                
029500     IF SEGMENT-SAKNAS                                                    
029600       PERFORM MFS-RENSA-WDK7-FAELT                                       
029700     ELSE                                                                 
029800       MOVE +1              TO SDC-IX                                     
029900       PERFORM IMS-GNP-ARTS11                                             
030000       PERFORM UNTIL SEGMENT-SAKNAS OR SDC-IX > 12                        
030100         IF SDC-IX          < 13                                          
030200           IF SLAG-IDDC NOT = '22'  AND                                   
030300             SLAG-IDDC NOT < WX-IDDC                                      
030400             PERFORM FBA-SDCINFO-TILL-MOD                                 
030500             ADD +1           TO SDC-IX                                   
030600           END-IF                                                         
030700           MOVE SLAG-IDDC     TO MOD-IDDC-NEXT                            
030900         END-IF                                                           
031000         PERFORM IMS-GNP-ARTS11                                           
031100       END-PERFORM                                                        
031200       IF SDC-IX = 13                                                     
031300         MOVE SLAG-IDDC       TO MOD-IDDC-NEXT                            
031500       END-IF                                                             
031600       PERFORM UNTIL SDC-IX > 12                                          
031700         PERFORM MFS-RENSA-FAELT-SDC-KOL                                  
031800         ADD +1             TO SDC-IX                                     
031900       END-PERFORM                                                        
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 FBA-SDCINFO-TILL-MOD SECTION.                                            
032400                                                                          
032500     MOVE ZERO              TO WS-KVDISP-SLAG                             
032600     COMPUTE WS-KVDISP-SLAG =  SLAG-KVLS  -                               
032700                               SLAG-KVOKS-DAG -                           
032800                               SLAG-KVOKS-BULK                            
032900                                                                          
033000     MOVE SLAG-IDDC         TO MOD-IDDC-SDC(SDC-IX)                       
033100     MOVE SLAG-KVLS         TO MOD-KVLS-SDC(SDC-IX)                       
033200     MOVE WS-KVDISP-SLAG    TO MOD-KVDISP-SDC(SDC-IX)                     
033310     IF MFS-SPLIT                                                         
033321        COMPUTE WS-KVPB-TOT  = SLAG-KVPB-REF  + SLAG-KVPBREOI             
033335        IF WS-KVPB-TOT > 9999.9                                           
033336           COMPUTE WS-KVPB-TOT-INT  ROUNDED = WS-KVPB-TOT                 
033337           MOVE WS-KVPB-TOT-INT  TO WS-KVPB-TOT-DISP                      
033338           MOVE WS-KVPB-TOT-DISP(1:7)                                     
033339                             TO MOD-KVOKS-DAG-SDC(SDC-IX)(1:7)            
033340        ELSE                                                              
033341           MOVE WS-KVPB-TOT  TO WS-KVPB-TOT-DISP                          
033342           MOVE WS-KVPB-TOT-DISP(3:7)                                     
033343                             TO MOD-KVOKS-DAG-SDC(SDC-IX)(1:7)            
033350        END-IF                                                            
033360        PERFORM FBAA-GET-KVOKS-PREL                                       
033410     ELSE                                                                 
033411        MOVE SLAG-KVOKS-DAG  TO MOD-KVOKS-DAG-SDC(SDC-IX)                 
033412        MOVE SLAG-KVOKS-BULK TO MOD-KVOKS-BULK-SDC(SDC-IX)                
033420     END-IF                                                               
033500     MOVE SLAG-KVROS-DAG    TO MOD-KVROS-DAG(SDC-IX)                      
033600     MOVE SLAG-KVROS-BULK   TO MOD-KVROS-BULK(SDC-IX)                     
033700     MOVE SLAG-KVRESS       TO MOD-KVRESS-SDC(SDC-IX)                     
033800     MOVE SLAG-KVEFRS       TO MOD-KVEFRS-SDC(SDC-IX)                     
033900     MOVE SLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL-SDC(SDC-IX)               
034000     MOVE SLAG-KVUTRS       TO MOD-KVUTRS-SDC(SDC-IX)                     
034100     MOVE SLAG-KVAKS-SDC    TO MOD-KVAKS-SDC(SDC-IX)                      
034200     .                                                                    
034300     EJECT                                                                
034400 FBAA-GET-KVOKS-PREL SECTION.                                             
034500                                                                          
034501     MOVE MSGI-IDARTNR      TO W-Q4-IDARTNR-F                             
034502                               W-Q4-IDARTNR-T                             
034503     MOVE SLAG-IDDC         TO W-Q4-IDDC-F                                
034504                               W-Q4-IDDC-T                                
034505     MOVE ZERO              TO WS-KVOKS-PREL                              
034506     PERFORM IMS-GN-WDQ4C1                                                
034507     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
034508        ADD SEQC-KVOKS-PREL TO WS-KVOKS-PREL                              
034509        PERFORM IMS-GN-WDQ4C1                                             
034510     END-PERFORM                                                          
034511     MOVE WS-KVOKS-PREL     TO MOD-KVOKS-BULK-SDC(SDC-IX)                 
034512     .                                                                    
034520     EJECT                                                                
034530 FC-BEHANDLA-WDK9  SECTION.                                               
034540                                                                          
034600     PERFORM IMS-GU-ARTM01                                                
034700     IF SEGMENT-FINNS                                                     
034800        COMPUTE WS-KVOKS-TOT            =  ARTM-ART-KVOKS-BULK +          
034900                                           ARTM-ART-KVOKS-DAG  +          
035000                                           ARTM-ART-KVOKS-VOR             
035100        MOVE ARTM-ART-KVOKS-VOR         TO MOD-KVOKS-VOR                  
035200        MOVE ARTM-ART-KVOKS-DAG         TO MOD-KVOKS-DAG                  
035300        MOVE ARTM-ART-KVOKS-BULK        TO MOD-KVOKS-BULK                 
035400        MOVE ARTM-ART-KVPREAVB-VOR      TO MOD-KVPREAVB-VOR               
035500        MOVE ARTM-ART-KVPREAVB-DAG      TO MOD-KVPREAVB-DAG               
035600        MOVE ARTM-ART-KVPREAVB-BULK     TO MOD-KVPREAVB-BULK              
035700        MOVE ARTM-ART-KVPRERO-DAG       TO MOD-KVPRERO-DAG                
035800        MOVE ARTM-ART-KVPRERO-BULK      TO MOD-KVPRERO-BULK               
035900        MOVE ARTM-ART-RERF-ART          TO MOD-RERF-ART                   
036000        MOVE ARTM-ART-SUTPO-TOT         TO MOD-SUTPO-TOT                  
036100        MOVE ARTM-ART-KVOFFERT          TO MOD-KVOFFERT                   
036200        MOVE WS-KVOKS-TOT               TO MOD-KVOKS-CDC                  
036300     ELSE                                                                 
036400        PERFORM MFS-RENSA-WDK9-FAELT                                      
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800 MFS-RENSA-FAELT-UT SECTION.                                              
036900                                                                          
037000*    --- ALLA UTDATA-FÄLT                                                 
037100     PERFORM MFS-RENSA-WDK6-FAELT                                         
037200     PERFORM MFS-RENSA-WDK7-FAELT                                         
037300     PERFORM MFS-RENSA-WDK9-FAELT                                         
037400     .                                                                    
037500     SKIP3                                                                
037600 MFS-RENSA-WDK6-FAELT SECTION.                                            
037700                                                                          
037800     MOVE MFS-RENSA-FAELT     TO MOD-KVLS-CDC                             
037900                                 MOD-KVDISP-CDC                           
038000                                 MOD-KVUTRS-CDC                           
038100                                 MOD-KVOKS-CDC                            
038200                                 MOD-KVRESS-CDC                           
038300                                 MOD-KVEFRS-CDC                           
038400                                 MOD-KVAKS-CDC                            
038500                                 MOD-KVAKS-PAV-CDC                        
038600                                 MOD-KVSPARR-KVAL-CDC                     
038700                                 MOD-KVROS-CDC                            
038800                                 MOD-KVSPANT                              
038900     .                                                                    
039000     EJECT                                                                
039100 MFS-RENSA-WDK7-FAELT   SECTION.                                          
039200                                                                          
039300     MOVE +1                  TO SDC-IX                                   
039400     PERFORM UNTIL SDC-IX     >  12                                       
039500       PERFORM MFS-RENSA-FAELT-SDC-KOL                                    
039600       ADD +1                 TO SDC-IX                                   
039700     END-PERFORM                                                          
039800     .                                                                    
039900     SKIP2                                                                
040000 MFS-RENSA-FAELT-SDC-KOL SECTION.                                         
040100                                                                          
040200     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-SDC(SDC-IX)                       
040300                               MOD-KVLS-SDC(SDC-IX)                       
040400                               MOD-KVDISP-SDC(SDC-IX)                     
040500                               MOD-KVOKS-DAG-SDC(SDC-IX)                  
040600                               MOD-KVOKS-BULK-SDC(SDC-IX)                 
040700                               MOD-KVROS-DAG(SDC-IX)                      
040800                               MOD-KVROS-BULK(SDC-IX)                     
040900                               MOD-KVRESS-SDC(SDC-IX)                     
041000                               MOD-KVEFRS-SDC(SDC-IX)                     
041100                               MOD-KVSPARR-KVAL-SDC(SDC-IX)               
041200                               MOD-KVUTRS-SDC(SDC-IX)                     
041300                               MOD-KVAKS-SDC(SDC-IX)                      
041400     .                                                                    
041500     EJECT                                                                
041600 MFS-RENSA-WDK9-FAELT SECTION.                                            
041700                                                                          
041800     MOVE MFS-RENSA-FAELT TO MOD-KVOFFERT                                 
041900                             MOD-KVOKS-BULK                               
042000                             MOD-KVOKS-DAG                                
042100                             MOD-KVOKS-VOR                                
042200                             MOD-KVPREAVB-BULK                            
042300                             MOD-KVPREAVB-DAG                             
042400                             MOD-KVPREAVB-VOR                             
042500                             MOD-KVPRERO-BULK                             
042600                             MOD-KVPRERO-DAG                              
042700                             MOD-RERF-ART                                 
042800                             MOD-SUTPO-TOT                                
042900     .                                                                    
043000                                                                          
043100     EJECT                                                                
043200* --- IMS SEKTIONER ---                                                   
043300                                                                          
043400 IMS-GET-MSG SECTION.                                                     
043500                                                                          
043600     MOVE '  QC' TO GODK-STATUSKODER                                      
043700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
043800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043900     PERFORM IMS-STATUSKONTROLL                                           
044000     .                                                                    
044100     SKIP3                                                                
044200 IMS-INSERT-MSG SECTION.                                                  
044300                                                                          
044400     IF MSGI-IDLAND-SPR = 'GB'                                            
044500*      MOVE 'N' TO MFS-KDHUVOMR                                           
044600       MOVE '0' TO MFS-KDHUVOMR                                           
044700     END-IF                                                               
044800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
044900     MOVE SPACE TO GODK-STATUSKODER                                       
045000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
045100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
045200     PERFORM IMS-STATUSKONTROLL                                           
045300     .                                                                    
045400     EJECT                                                                
045500 IMS-GU-ARTC01    SECTION.                                                
045600                                                                          
045700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
045800          DELIMITED BY SIZE INTO SSA1                                     
045900     MOVE '  GE' TO GODK-STATUSKODER                                      
046000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1                    
046100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400                                                                          
046500                                                                          
046600 IMS-GNP-ARTC11    SECTION.                                               
046700                                                                          
046800     MOVE 'WLARTC11 '            TO SSA1                                  
046900     MOVE '  GE' TO GODK-STATUSKODER                                      
047000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1                    
047100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047200     PERFORM IMS-STATUSKONTROLL                                           
047300     .                                                                    
047400     EJECT                                                                
047500 IMS-GU-ARTS01    SECTION.                                                
047600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
047700          DELIMITED BY SIZE INTO SSA1                                     
047800     MOVE '  GE' TO GODK-STATUSKODER                                      
047900     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11 SSA1                    
048000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
048100     PERFORM IMS-STATUSKONTROLL                                           
048200     .                                                                    
048300     EJECT                                                                
048400 IMS-GNP-ARTS11 SECTION.                                                  
048500     MOVE 'WLARTS11 ' TO SSA1                                             
048600     MOVE '  GE' TO GODK-STATUSKODER                                      
048700     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
048800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
048900     PERFORM IMS-STATUSKONTROLL                                           
049000     .                                                                    
049100     EJECT                                                                
049200 IMS-GU-ARTM01    SECTION.                                                
049300                                                                          
049400     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
049500          DELIMITED BY SIZE INTO SSA1                                     
049600     MOVE '  GE' TO GODK-STATUSKODER                                      
049700     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-ARTM01 SSA1                    
049800     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
049900     PERFORM IMS-STATUSKONTROLL                                           
050000     .                                                                    
050010                                                                          
050020 IMS-GN-WDQ4C1 SECTION.                                                   
050030                                                                          
050040     STRING 'WDQ4C1  (WDQ4C1KY>=' W-WDQ4CKY-FOM                           
050050                    '&WDQ4C1KY<=' W-WDQ4CKY-TOM ')'                       
050070            DELIMITED BY SIZE INTO SSA1                                   
050080     MOVE '  GEGB'            TO GODK-STATUSKODER                         
050090     CALL CBLTDLI USING GN WDQ4C-PCB DLI-IO-WDQ4C1 SSA1                   
050091     MOVE WDQ4C-STATUS-CODE   TO STATUS-WS                                
050092     PERFORM IMS-STATUSKONTROLL                                           
050093     .                                                                    
050100     EJECT                                                                
050200 IMS-STATUSKONTROLL SECTION.                                              
050300                                                                          
050400     SET STATUS-IX TO 1                                                   
050500     SEARCH GODK-STATUS                                                   
050600       AT END                                                             
050700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
050800         DELIMITED BY SIZE INTO FELTEXT                                   
050900         CALL FELLOG                                                      
051000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
051100         CONTINUE                                                         
051200     END-SEARCH                                                           
051300     .                                                                    
