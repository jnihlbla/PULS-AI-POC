000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000400 PROGRAM-ID.     W440RROT.                                                
000500 AUTHOR.         GUNNAR LARSSON, IDK.                                     
000600 DATE-WRITTEN.   NOV 1990.                                                
000700                                                                          
000800*****************************************************************         
000900*                                                                         
000910*    SKALL SAMMA ÄNDRING IN I W411RANS ?????                              
000920*                                                                         
001000*    RANSONERING VID RESTORDERTÄCKNING                                    
001100*    R               R   O    T                                           
001200*                                                                         
001300*    DETTA ÄR EN SUBMODUL SOM ANROPAS I RESTORDER-SYSTEMET FÖR            
001400*    ATT BERÄKNA RANSONERAD DISPONIBEL KVANT PER CLAGER.                  
001500*                                                                         
001600*    REGISTER :    WLARTM (WDK9) ARTIKELREGISTER                          
001610*             :    WLARTS (WDK7) ARTIKELREGISTER                          
001700*                                                                         
001800*    LÄNKAREA :    W440RROTC0                                             
001900*                                                                         
002000*****************************************************************         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002603*    -COPY WY2000W1                                                       
002604     SKIP3                                                                
002700 01    IDPGM                     PIC X(08)   VALUE 'W440RROT'.            
002800 01    FELTEXT                   PIC X(80)   VALUE SPACE.                 
002900 01    RKOD-ABEND                PIC S9(4)   VALUE +33 COMP SYNC.         
003000*                                                                         
003100 01      FILLER                  PIC X(16)   VALUE                        
003200                                 'K-KONSTANTER****'.                      
003300 01      K-KONSTANTER.                                                    
003400  02     K-ANT-RF                PIC S9(1)   VALUE +3  COMP-3.            
003500  02     K-TABING-RF10           PIC S9(9)   VALUE +1  COMP SYNC.         
003600  02     K-TABING-RF07           PIC S9(9)   VALUE +2  COMP SYNC.         
003700  02     K-TABING-RF05           PIC S9(9)   VALUE +3  COMP SYNC.         
003800  02     K-RECLPROC-DEFAULT      PIC S9(3)   VALUE +12 COMP-3.            
003900  02     K-ANTDGR-BESTTID        PIC S9(3)   VALUE +30 COMP-3.            
004000  02     K-MAXANT-CLAGER         PIC S9(1)   VALUE +2  COMP-3.            
004100  02     C1                      PIC S9(1)   VALUE +1  COMP-3.            
004200  02     C2                      PIC S9(1)   VALUE +2  COMP-3.            
004300  02     JA                      PIC X(1)    VALUE 'J'.                   
004400  02     NEJ                     PIC X(1)    VALUE 'N'.                   
004500*                                                                         
004600*                                                                         
004610 01  WS-KVPB-REF                 PIC S9(8)V9(1)        COMP-3.            
004700*                                                                         
004800 01      FILLER                  PIC X(16)   VALUE                        
004900                                 'W***************'.                      
005000 01      W.                                                               
005100*                                                                         
005200*--------------------------------* C-LAGER BEROENDE FÄLT                  
005300  02     FILLER                  OCCURS 2.                                
005400*                                * LGE: FÅR EJ CLEARAS                    
005500   03    WS-LGE                  PIC S9(9)           COMP-3.              
005600*                                * GL: GEMENSAMT LAGER (ÖVER LGE)         
005700   03    WS-GL                   PIC S9(9)           COMP-3.              
005800*                                * DISP: UHT RANSONERING                  
005900   03    WS-DISP                 PIC S9(9)           COMP-3.              
006000*                                * SUTPO: T.O.M VECKAN FÖRE               
006100*                                *        TIDISPIN                        
006200   03    WS-SUTPO                PIC S9(9)           COMP-3.              
006300*                                * BEHOV: SAMLAT BEHOV                    
006400   03    WS-BEHOV                PIC S9(9)           COMP-3.              
006500*                                * BGL : BEHOV INOM GL (ÖVER LGE)         
006600   03    WS-BGL                  PIC S9(9)           COMP-3.              
006700*                                * ANDEL AV RANSON. KVANT                 
006800   03    WS-RANSKV-ANDEL         PIC S9(9)           COMP-3.              
006900*                                * BIDRAG MELLAN C-LAGREN                 
007000   03    WS-BIDRAG-FR            PIC S9(9)           COMP-3.              
007100*                                * ARB.FÄLT FÖR BER RANSON. KVANT         
007200   03    WS-BEHOV-BGL-KVOT       PIC S9(9)V9(3)      COMP-3.              
007300*                                                                         
007400*                                * TOT DISP VID RF 1.0                    
007500  02     WS-DISP-RF10            PIC S9(9)           COMP-3.              
007600*                                * TOT DISP VID RF 0.5                    
007700  02     WS-DISP-RF05            PIC S9(9)           COMP-3.              
007800*                                * TOT KVROS                              
007900  02     WS-TOT-KVROS            PIC S9(9)           COMP-3.              
008000*                                * BRIST C1                               
008100  02     WS-BRIST-C1             PIC S9(9)           COMP-3.              
008200*                                * RANSONERAD TOTALKVANT ("X")            
008300  02     WS-RANSKV               PIC S9(9)           COMP-3.              
008400*                                * GL ALLA C-LAGER                        
008500  02     WS-TOT-GL               PIC S9(9)           COMP-3.              
008600*                                * BGL ALLA C-LAGER                       
008700  02     WS-TOT-BGL              PIC S9(9)           COMP-3.              
008800*                                                                         
008900*                                * ANTAL C-LAGER SOM BEHANDLAS            
009000  02     WS-ANT-CLAGER           PIC S9(1)           COMP-3.              
009100*                                * ANDEL EJ CLEARING                      
009200  02     WS-RECLPROC             PIC S9(3)           COMP-3.              
009300*                                * HÖGSTA VÄRDE, GER LÄGSTA "X"           
009400  02     WS-BEHOV-BGL-KVOT-HIGH  PIC S9(9)V9(3)      COMP-3.              
009500*                                * ANT. DAGAR TILL NÄSTA INLEV.           
009600  02     WS-ANTDGR-INLEV         PIC S9(5)           COMP-3.              
009700*                                                                         
009800*                                * DAGENS AAMMDD                          
009900  02     WS-AKT-AAMMDD           PIC 9(6).                                
010000*                                * DAGENS AAVV                            
010100  02     WS-AKT-AAAAVV.                                                   
010110   03    WS-AKT-SEKEL            PIC 9(2).                                
010120   03    WS-AKT-AAVV             PIC 9(4).                                
010200*                                                                         
010300*                                * FÖR BERÄKN. AAVV                       
010400  02     WS-AAVV                 PIC 9(4).                                
010410                                                                          
010500  02     FILLER                  REDEFINES WS-AAVV.                       
010600   03    WS-AAVV-AA              PIC 9(2).                                
010700   03    WS-AAVV-VV              PIC 9(2).                                
010800*                                                                         
010900*                                * FLAGGA ARTM FINNS/SAKNAS               
011000  02     WS-ARTM-FINNS           PIC X(1).                                
011100*                                                                         
011200*------------------------------- * RANS.FAKTOR WORK                       
011300 01      RFW.                                                             
011400  02     RFW-TABVARDE.                                                    
011500   03    FILLER                  PIC S9(1)V9(2) VALUE 1.0 COMP-3.         
011600   03    FILLER                  PIC S9(1)V9(2) VALUE 0.7 COMP-3.         
011700   03    FILLER                  PIC S9(1)V9(2) VALUE 0.5 COMP-3.         
011800  02     FILLER                  REDEFINES RFW-TABVARDE.                  
011900   03    RFW-ING                 OCCURS 3.                                
012000    04   RFW-RERF                PIC S9(1)V9(2)           COMP-3.         
012100*                                                                         
012200*------------------------------- * RANSONERAD UTTAGSKVANT WORK            
012300 01      RUKW.                                                            
012400  02     RUKW-RF-ING             OCCURS 3.                                
012500   03    RUKW-CL-TAB.                                                     
012600    04   RUKW-CL-ING             OCCURS 2.                                
012700*                                * DISP OVAN LGE                          
012800     05  RUKW-OVAN-LGE           PIC S9(9)                COMP-3.         
012900*                                * DISP INOM LGE                          
013000     05  RUKW-INOM-LGE           PIC S9(9)                COMP-3.         
013100*                                * DISP INKL LGE                          
013200     05  RUKW-DISP               PIC S9(9)                COMP-3.         
013300*                                                                         
013400*------------------------------- * INDEX-VARIABLER                        
013500 01      IX-INDEX.                                                        
013600  02     CLIX                    PIC S9(9)             COMP SYNC.         
013700  02     RFIX                    PIC S9(9)             COMP SYNC.         
013800     EJECT                                                                
013810*      --- VALID IDDC CODES                                               
013820*                                                                         
013830*01    -COPY WWDCKONS                                                     
013840       EJECT                                                              
013900*                                                                         
014000 01  GENERELLA-SUBPROGRAM.                                                
014100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
014500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014600     EJECT                                                                
014700                                                                          
014800*    --- ARBETS-AREOR TILL GENERELLA SUBPROGRAM                           
014900     SKIP3                                                                
015000*01  -COPY WDATAREA                                                       
015100     EJECT                                                                
015200                                                                          
015300*01  -COPY WORKAREA                                                       
015400     EJECT                                                                
015500                                                                          
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP2                                                                
016000*    --- STATUS-KOD FRÅN IMS                                              
016100 01  STATUS-WS                   PIC XX.                                  
016200     88  SEGMENT-FINNS                       VALUE '  '.                  
016300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016400     SKIP2                                                                
016500 01  GODK-STATUSKODER.                                                    
016600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700     SKIP2                                                                
016800 01  SSA1                        PIC X(64).                               
016900 01  SSA2                        PIC X(64).                               
017000     EJECT                                                                
017100                                                                          
017200*    --- IMS FUNKTIONSKODER                                               
017300*01  -COPY W0003                                                          
017400     EJECT                                                                
017500                                                                          
017600 01  NYCKLAR-TILL-DLI.                                                    
017700                                                                          
017800     03  W-IDARTNR-X.                                                     
017900         05  W-IDARTNR           PIC  S9(9)  COMP-3.                      
018000                                                                          
018100     03  W-DABEHOV-MIN-X.                                                 
018200         05  W-DABEHOV-MIN       PIC   9(6).                              
018300                                                                          
018400     03  W-DABEHOV-MAX-X.                                                 
018500         05  W-DABEHOV-MAX       PIC   9(6).                              
018600                                                                          
018700     EJECT                                                                
018800                                                                          
018900*    ---  DLI INPUT-OUTPUT AREOR                                          
019000*                                                                         
019100*01  WLARTM01 -COPY WDK901                                                
019200     EJECT                                                                
019300                                                                          
019400*01  WLARTM11 -COPY WDK911                                                
019500     EJECT                                                                
019501*                                                                         
019510*01  WLARTS01 -COPY WDK701                                                
019520     EJECT                                                                
019530                                                                          
019540*01  WLARTS11 -COPY WDK711                                                
019550     EJECT                                                                
019600 LINKAGE SECTION.                                                         
019700                                                                          
019800                                                                          
019900*   -COPY W440RROT                                                        
020000     EJECT                                                                
020100                                                                          
020200*01  -COPY W0008      -PRE ARTM-                                          
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020410*01  -COPY W0008      -PRE ARTS-                                          
020420     05  FILLER                  PIC X.                                   
020430     EJECT                                                                
020500 PROCEDURE DIVISION  USING RROT-W440RROT ARTM-PCB ARTS-PCB.               
020600                                                                          
020700 STYRDEL SECTION.                                                         
020800****************************************************************          
020900*                                                                         
021000*    KDLTK STYR VILKA C-LAGER SOM SKALL BEHANDLAS.                        
021100*    BEHOV OCH TILLGÅNGAR SUMMERAS.                                       
021200*    RANSONERAT DISPONIBELT BERÄKNAS.                                     
021300*                                                                         
021400****************************************************************          
021500                                                                          
021600     PERFORM A-INIT                                                       
021700                                                                          
021800     MOVE RROT-IDARTNR         TO W-IDARTNR                               
021900     PERFORM IMS-GU-ARTM01                                                
022000                                                                          
022100     IF  SEGMENT-FINNS                                                    
022200       MOVE JA                 TO WS-ARTM-FINNS                           
022300                                                                          
022400     ELSE                                                                 
022500       MOVE NEJ                TO WS-ARTM-FINNS                           
022600                                                                          
022700*------- ALLA FÄLT I ARTM-ART SOM PROGRAMMET REFERERAS TILL               
022800*        SKALL NOLLSTÄLLAS.                                               
022900       MOVE ZERO               TO ART-KVPREAVB-VOR                        
023000       MOVE ZERO               TO ART-KVPREAVB-DAG                        
023100       MOVE ZERO               TO ART-KVOKS-BULK                          
023200       MOVE ZERO               TO ART-KVOKS-DAG                           
023300     END-IF                                                               
023400                                                                          
023500     IF  RROT-KDLTK = 1                                                   
023600       MOVE 1                  TO WS-ANT-CLAGER                           
023700     ELSE                                                                 
023800       MOVE K-MAXANT-CLAGER    TO WS-ANT-CLAGER                           
023900     END-IF                                                               
024000                                                                          
024100     MOVE 1                    TO CLIX                                    
024200                                                                          
024300     PERFORM UNTIL (CLIX > WS-ANT-CLAGER)                                 
024400                                                                          
024500       PERFORM B-SUM-BEHOV-ETC                                            
024600       ADD 1                   TO CLIX                                    
024700     END-PERFORM                                                          
024800                                                                          
024900     PERFORM C-KORR-BGL                                                   
025000                                                                          
025100     PERFORM D-RANSONERING                                                
025200                                                                          
025300     GOBACK                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 A-INIT SECTION.                                                          
025700****************************************************************          
025800*                                                                         
025900*    NOLLSTÄLLNING, DATUMINITIERNG MM                                     
026000*                                                                         
026100****************************************************************          
026200                                                                          
026300     MOVE 1                    TO CLIX                                    
026400     PERFORM UNTIL (CLIX > K-MAXANT-CLAGER)                               
026500       MOVE ZERO               TO RROT-KVDISP  (CLIX)                     
026600                                                                          
026700       MOVE ZERO               TO WS-LGE       (CLIX)                     
026800       MOVE ZERO               TO WS-GL        (CLIX)                     
026900       MOVE ZERO               TO WS-DISP      (CLIX)                     
027000       MOVE ZERO               TO WS-SUTPO     (CLIX)                     
027100       MOVE ZERO               TO WS-BEHOV     (CLIX)                     
027200       MOVE ZERO               TO WS-BGL       (CLIX)                     
027300       MOVE ZERO               TO WS-BIDRAG-FR (CLIX)                     
027400       MOVE ZERO               TO WS-BEHOV-BGL-KVOT (CLIX)                
027500       MOVE ZERO               TO WS-RANSKV-ANDEL   (CLIX)                
027600       ADD 1                   TO CLIX                                    
027700     END-PERFORM                                                          
027800                                                                          
027900     MOVE ZERO                 TO WS-TOT-GL                               
028000     MOVE ZERO                 TO WS-TOT-BGL                              
028100                                                                          
028200     MOVE 1                    TO RFIX                                    
028300     PERFORM UNTIL (RFIX > K-ANT-RF)                                      
028400       MOVE 1                  TO CLIX                                    
028500       PERFORM UNTIL (CLIX > K-MAXANT-CLAGER)                             
028600         MOVE ZERO             TO RUKW-OVAN-LGE (RFIX, CLIX)              
028700         MOVE ZERO             TO RUKW-INOM-LGE (RFIX, CLIX)              
028800         MOVE ZERO             TO RUKW-DISP     (RFIX, CLIX)              
028900         ADD 1                 TO CLIX                                    
029000       END-PERFORM                                                        
029100       ADD 1                   TO RFIX                                    
029200     END-PERFORM                                                          
029300                                                                          
029400     ACCEPT WS-AKT-AAMMDD      FROM DATE                                  
029500                                                                          
029600     PERFORM AA-BER-AKT-AAVV                                              
029700     .                                                                    
029800     EJECT                                                                
029900 AA-BER-AKT-AAVV SECTION.                                                 
030000****************************************************************          
030100*                                                                         
030200*    BERÄKNAR DAGENS DATUM I "AAVV"-FORM                                  
030300*                                                                         
030400****************************************************************          
030500                                                                          
030600     MOVE 'IDAG'               TO DAT-KDDATFORM                           
030700     CALL WDATKONV USING DAT-KDDATFORM                                    
030800                         DAT-I-TIDATUM                                    
030900                         DAT-O-TIDATUM                                    
031000                         DAT-KDSVAR                                       
031100                                                                          
031200     IF DAT-KDSVAR-OK                                                     
031300       MOVE DAT-TIAA-VECKA     TO WS-AAVV-AA                              
031400       MOVE DAT-TIVV           TO WS-AAVV-VV                              
031500       MOVE WS-AAVV            TO WS-AKT-AAVV                             
031510       MOVE DAT-TISEKEL        TO WS-AKT-SEKEL                            
031600                                                                          
031700     ELSE                                                                 
031800       MOVE ' FEL FRÅN SUB-PROGRAM WDATKONV I SECTION AA-'                
031900                               TO FELTEXT                                 
032000       DISPLAY '*****************************************'                
032100       DISPLAY IDPGM FELTEXT                                              
032200       DISPLAY '*****************************************'                
032300       CALL ABEND USING RKOD-ABEND                                        
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 B-SUM-BEHOV-ETC SECTION.                                                 
032800****************************************************************          
032900*                                                                         
033000*    SUMMERAR BEHOV, LAGERTILLGÅNGAR ETC.                                 
033100*                                                                         
033200****************************************************************          
033300                                                                          
033400     PERFORM BA-BER-TPO                                                   
033500                                                                          
033600     PERFORM BB-BER-DISP                                                  
033700                                                                          
033800     PERFORM BC-BER-LGE                                                   
033900                                                                          
034000     PERFORM BD-BER-BEHOV                                                 
034100                                                                          
034200     PERFORM BE-BER-ACK-BGL-GL                                            
034300                                                                          
034400     PERFORM BF-KORR-BEHOV                                                
034500     .                                                                    
034600     EJECT                                                                
034700 BA-BER-TPO SECTION.                                                      
034800****************************************************************          
034900*                                                                         
035000*    SUMMERAR TPO-BEHOV FRAM TILL NÄSTA INLEVERANS.                       
035100*                                                                         
035200****************************************************************          
035300                                                                          
035400     PERFORM BAA-BYGG-WDK911-NKL                                          
035500                                                                          
035600     IF  W-DABEHOV-MIN <= W-DABEHOV-MAX                                   
035700                                                                          
035800       MOVE RROT-IDARTNR         TO W-IDARTNR                             
035900                                                                          
036000       IF  WS-ARTM-FINNS         = JA                                     
036100         PERFORM IMS-GNP-ARTM11                                           
036200                                                                          
036300         PERFORM UNTIL (NOT SEGMENT-FINNS)                                
036400           ADD ANT-SUTPO-PB TO WS-SUTPO (CLIX)                            
036500           ADD ANT-SUTPO-EJPB TO WS-SUTPO (CLIX)                          
036600           PERFORM IMS-GNP-ARTM11                                         
036700         END-PERFORM                                                      
036800                                                                          
036900       ELSE                                                               
037000         CONTINUE                                                         
037100       END-IF                                                             
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 BAA-BYGG-WDK911-NKL SECTION.                                             
037600****************************************************************          
037700*                                                                         
037800*    BYGGER NYCKEL WDK911.                                                
037900*    SKAPAR FÖR AKT. C-LAGER ETT DATUM-INTERVALL                          
038000*    FOM DAGENS DATUM, TILL NÄSTA INLEV.DATUM                             
038100*                                                                         
038200****************************************************************          
038300                                                                          
038400     MOVE WS-AKT-AAAAVV        TO W-DABEHOV-MIN                           
038500                                                                          
038600     IF  RROT-TIDISPIN (CLIX)  = ZERO                                     
038700       MOVE W-DABEHOV-MIN      TO W-DABEHOV-MAX                           
038800                                                                          
038900     ELSE                                                                 
039000       MOVE RROT-TIDISPIN (CLIX) TO DAT-I-TIDATUM                         
039100       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
039200       CALL WDATKONV USING DAT-KDDATFORM                                  
039300                           DAT-I-TIDATUM                                  
039400                           DAT-O-TIDATUM                                  
039500                           DAT-KDSVAR                                     
039600                                                                          
039700       IF DAT-KDSVAR-OK                                                   
039800         MOVE DAT-TIAA-VECKA     TO WS-AAVV-AA                            
039900         MOVE DAT-TIVV           TO WS-AAVV-VV                            
040000         MOVE WS-AAVV            TO W-DABEHOV-MAX                         
040010         MOVE DAT-TISEKEL        TO W-DABEHOV-MAX (1:2)                   
040100                                                                          
040200       ELSE                                                               
040300         MOVE ' FEL FRÅN SUB-PROGRAM WDATKONV I SECTION BAA-'             
040400                               TO FELTEXT                                 
040500         DISPLAY '*****************************************'              
040600         DISPLAY IDPGM FELTEXT                                            
040700         DISPLAY '*****************************************'              
040800         CALL ABEND USING RKOD-ABEND                                      
040900       END-IF                                                             
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 BB-BER-DISP SECTION.                                                     
041400****************************************************************          
041500*                                                                         
041600*    BERÄKNAR DISPONIBEL KVANT UHT RANSONERING (MIN. NOLL)                
041700*                                                                         
041800****************************************************************          
041900                                                                          
042000     COMPUTE WS-DISP (CLIX)                                               
042100            = RROT-KVLS          (CLIX)                                   
042200            - RROT-KVSPANT       (CLIX)                                   
042300            - RROT-KVUTRS        (CLIX)                                   
042400            - RROT-KVRESS        (CLIX)                                   
042500            - ART-KVPREAVB-VOR                                            
042600            - ART-KVPREAVB-DAG                                            
042700     END-COMPUTE                                                          
042800                                                                          
042900     IF WS-DISP (CLIX) < ZERO                                             
043000       MOVE ZERO               TO WS-DISP (CLIX)                          
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 BC-BER-LGE                    SECTION.                                   
043500****************************************************************          
043600*                                                                         
043700*    BERÄKNAR LGE - LAGERTILLGÅNG SOM EJ FÅR CLEARAS.                     
043800*    BESTÅR AV %-SATS AV PERIOD-BEHOV.                                    
043900*    (MAX. DISPONIBELT)                                                   
044000*                                                                         
044100****************************************************************          
044200                                                                          
044300* *  IF RROT-RECLPROC (CLIX) > ZERO                                       
044400* *    MOVE RROT-RECLPROC (CLIX) TO WS-RECLPROC                           
044500* *  ELSE                                                                 
044600* *    MOVE K-RECLPROC-DEFAULT   TO WS-RECLPROC                           
044700* *  END-IF                                                               
044800                                                                          
044900* *  COMPUTE WS-LGE (CLIX) ROUNDED =                                      
045000* *            (RROT-KVPB-SEP  (CLIX)                                     
045100* *           + RROT-KVPB-SATS (CLIX)                                     
045200* *           + RROT-KVPB-TPO  (CLIX) )                                   
045300* *         * WS-RECLPROC                                                 
045400* *         / 100                                                         
045500* *  END-COMPUTE                                                          
045600                                                                          
045700* *  IF WS-LGE (CLIX) > WS-DISP (CLIX)                                    
045800* *    MOVE WS-DISP (CLIX)     TO WS-LGE (CLIX)                           
045900* *  END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 BD-BER-BEHOV SECTION.                                                    
046300****************************************************************          
046400*                                                                         
046500*    BERÄKNAR SUMMAN AV ALLA BEHOV.                                       
046600*                                                                         
046700****************************************************************          
046800                                                                          
046900     PERFORM BDA-ANTDGR-TILL-INLEV                                        
046910     PERFORM BDB-BER-DAGSBEHOV-REFILL                                     
047000                                                                          
047100     COMPUTE WS-BEHOV (CLIX) =                                            
047200               (WS-ANTDGR-INLEV                                           
047300              * (RROT-KVPB-SEP         (CLIX)                             
047400               + RROT-KVPB-SATS        (CLIX))                            
047500              * 12 / 260 )                                                
047510             + (WS-ANTDGR-INLEV                                           
047520             * (WS-KVPB-REF * 12 / 260))                                  
047600             + WS-SUTPO              (CLIX)                               
047700             + ART-KVOKS-DAG                                              
047800             + ART-KVOKS-BULK                                             
047900             - ART-KVPREAVB-DAG                                           
048000     END-COMPUTE                                                          
048100     .                                                                    
048200     EJECT                                                                
048300 BDA-ANTDGR-TILL-INLEV SECTION.                                           
048400****************************************************************          
048500*                                                                         
048600*    BERÄKNAR ANTAL DAGAR FRAM TILL NÄSTA INLEVERANS.                     
048700*    RESULTAT REDUCERAS MED 2 FÖR ATT                                     
048800*    IDAG SAMT INLEVERANSDAGEN SKALL RÄKNAS BORT.                         
048900*                                                                         
049000****************************************************************          
049100                                                                          
049101     MOVE RROT-TIDISPIN (CLIX)  TO TMP1-YYMMDD                            
049102     MOVE WS-AKT-AAMMDD         TO TMP2-YYMMDD                            
049110     PERFORM WY2000P1                                                     
049200     IF  RROT-TIDISPIN (CLIX) > 0                                         
049300     AND TMP1-YYMMDD          > TMP2-YYMMDD                               
049400*      *-- INLEV.DATUM I FRAMTIDEN                                        
049500                                                                          
049600       MOVE WS-AKT-AAMMDD          TO WORK-TIAAMMDD-FOM                   
049700       MOVE RROT-TIDISPIN (CLIX)   TO WORK-TIAAMMDD-TOM                   
049800       MOVE WC-CDC-SE              TO WORK-IDDC                           
049810       MOVE 001                    TO WORK-KDCALL                         
049900       CALL WORKDAY  USING WORK-KDCALL                                    
050000                           WORK-DATE-AREA                                 
050100                           WORK-KDSVAR                                    
050200                                                                          
050300       IF WORK-KDSVAR-OK                                                  
050400         COMPUTE WS-ANTDGR-INLEV = WORK-KVWORKD - 2                       
050500         END-COMPUTE                                                      
050600                                                                          
050700         IF WS-ANTDGR-INLEV < ZERO                                        
050800           MOVE ZERO           TO WS-ANTDGR-INLEV                         
050900         END-IF                                                           
051000                                                                          
051100       ELSE                                                               
051200         MOVE ' FEL FRÅN SUB-PROGRAM WORKDAY I SECTION BDA-'              
051300                               TO FELTEXT                                 
051400         DISPLAY '*****************************************'              
051500         DISPLAY IDPGM FELTEXT                                            
051600         DISPLAY '*****************************************'              
051700         CALL ABEND USING RKOD-ABEND                                      
051800       END-IF                                                             
051900                                                                          
052000       IF WS-ANTDGR-INLEV > K-ANTDGR-BESTTID                              
052100         MOVE K-ANTDGR-BESTTID     TO WS-ANTDGR-INLEV                     
052200       END-IF                                                             
052300                                                                          
052400     ELSE                                                                 
052500       MOVE ZERO               TO WS-ANTDGR-INLEV                         
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052810 BDB-BER-DAGSBEHOV-REFILL                SECTION.                         
052820     MOVE ZERO                      TO WS-KVPB-REF                        
052830                                                                          
052840     PERFORM IMS-GU-ARTS01                                                
052850                                                                          
052860     IF SEGMENT-FINNS                                                     
052870                                                                          
052880       PERFORM IMS-GNP-ARTS11                                             
052890                                                                          
052891       PERFORM UNTIL SEGMENT-SAKNAS                                       
052892                                                                          
052893         IF  SLAG-IDLEVNR = '1441 ' OR 'BP2TW'                            
052894           COMPUTE WS-KVPB-REF      =                                     
052895             WS-KVPB-REF + SLAG-KVPB-REF                                  
052896           END-COMPUTE                                                    
052897         END-IF                                                           
052898                                                                          
052899         PERFORM IMS-GNP-ARTS11                                           
052900       END-PERFORM                                                        
052901     END-IF                                                               
052902     .                                                                    
052903     EJECT                                                                
052910 BE-BER-ACK-BGL-GL SECTION.                                               
053000****************************************************************          
053100*                                                                         
053200*    BERÄKNAR BGL (BEHOV INOM GL) INOM AKT. C-LAGER.                      
053300*    ACKUMULERAR TOTAL BGL.                                               
053400*    KORRIGERAR BGL INOM AKT. C-LAGER FÖR KOMMANDE BERÄKNINGAR.           
053500*    BERÄKNAR GL  (LAGER ÖVER LGE) INOM AKT. C-LAGER.                     
053600*    ACKUMULERAR TOTAL GL.                                                
053700*                                                                         
053800****************************************************************          
053900                                                                          
054000*------------------------------ BERÄKNA BGL                               
054100                                                                          
054200     COMPUTE WS-BGL (CLIX)                                                
054300            = WS-BEHOV (CLIX)                                             
054400            - WS-LGE   (CLIX)                                             
054500     END-COMPUTE                                                          
054600                                                                          
054700     IF WS-BGL (CLIX) < ZERO                                              
054800       MOVE ZERO               TO WS-BGL (CLIX)                           
054900     END-IF                                                               
055000                                                                          
055100*------------------------------ ACKUMULERA TOT BGL                        
055200                                                                          
055300     ADD WS-BGL (CLIX)         TO WS-TOT-BGL                              
055400                                                                          
055500*------------------------------ KORRIGERA BGL                             
055600                                                                          
055700     IF WS-BGL (CLIX) < 1                                                 
055800       MOVE 1                  TO WS-BGL (CLIX)                           
055900     END-IF                                                               
056000                                                                          
056100*------------------------------ BERÄKNA GL                                
056200                                                                          
056300     COMPUTE WS-GL  (CLIX)                                                
056400            = WS-DISP  (CLIX)                                             
056500            - WS-LGE   (CLIX)                                             
056600     END-COMPUTE                                                          
056700                                                                          
056800     IF WS-GL  (CLIX) < ZERO                                              
056900       MOVE ZERO               TO WS-GL  (CLIX)                           
057000     END-IF                                                               
057100                                                                          
057200*------------------------------ ACKUMULERA TOT GL                         
057300                                                                          
057400     ADD WS-GL  (CLIX)         TO WS-TOT-GL                               
057500     .                                                                    
057600     EJECT                                                                
057700 BF-KORR-BEHOV SECTION.                                                   
057800****************************************************************          
057900*                                                                         
058000*    KORRIGERAR BEHOV, FÖR KOMMANDE BERÄKNINGAR.                          
058100*                                                                         
058200****************************************************************          
058300                                                                          
058400     IF WS-BEHOV (CLIX) < 1                                               
058500       MOVE 1                  TO WS-BEHOV (CLIX)                         
058600     END-IF                                                               
058700     .                                                                    
058800     EJECT                                                                
058900 C-KORR-BGL SECTION.                                                      
059000****************************************************************          
059100*                                                                         
059200*    KORRIGERAR BGL, FÖR KOMMANDE BERÄKNINGAR.                            
059300*                                                                         
059400****************************************************************          
059500                                                                          
059600     IF WS-TOT-BGL < 1                                                    
059700       MOVE 1                  TO WS-TOT-BGL                              
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 D-RANSONERING SECTION.                                                   
060200****************************************************************          
060300*                                                                         
060400*    BERÄKNAR RANSONERAD DISPONIBEL KVANT                                 
060500*    FÖR C1 ELLER FÖR BÅDA C-LAGREN BEROENDE PÅ LTK.                      
060600*                                                                         
060700*    ARTIKLAR SOM ÄR ERSATTA, KDERS > 0, OCH ARTIKLAR                     
060800*    MED DIREKTLEVERANSANDEL, REDIRLEV > 0, SKALL EJ                      
060900*    RANSONERAS. DETTA ÅSTADKOMMES GENOM ATT NOLLSTÄLLA                   
061000*    RANSONERINGSFAKTORTABELLEN FÖR DESSA.                                
061100*                                                                         
061200****************************************************************          
061300                                                                          
061400     PERFORM DA-BER-BEHOV-BGL-KVOT                                        
061500*      *-- ERSATTA OCH DIREKTLEVERANSARTIKLAR RANSONERAS EJ               
061600     IF RROT-REDIRLEV(1) > +0 OR RROT-KDERS(1) > +0 OR                    
061700        RROT-REDIRLEV(2) > +0 OR RROT-KDERS(2) > +0                       
061800         MOVE 1                    TO RFIX                                
061900         PERFORM UNTIL (RFIX > K-ANT-RF)                                  
062000             MOVE +0           TO RFW-RERF(RFIX)                          
062100             ADD 1             TO RFIX                                    
062200         END-PERFORM                                                      
062300     END-IF                                                               
062400                                                                          
062500     IF  RROT-KDLTK = 1                                                   
062600*      *-- LTK = 1, BEHANDLA ENDAST C1                                    
062700       PERFORM DB-RANS-LTK-1                                              
062800                                                                          
062900     ELSE                                                                 
063000*      *-- LTK <> 1, BEHANDLA BÅDA C-LAGREN                               
063100       PERFORM DC-RANS-LTK-OVRIGA                                         
063200                                                                          
063300     END-IF                                                               
063400     .                                                                    
063500     EJECT                                                                
063600 DA-BER-BEHOV-BGL-KVOT SECTION.                                           
063700****************************************************************          
063800*                                                                         
063900*    TAR FRAM HÖGSTA "BEHOV-BGL-KVOTEN",                                  
064000*    VILKEN GER LÄGSTA RANSON. KVANT (=X)                                 
064100*    EFTERSOM X = TOT-GL - (RF * BEHOV-BGL-KVOT-HIGH)                     
064200*                                                                         
064300****************************************************************          
064400                                                                          
064500     MOVE ZERO                 TO WS-BEHOV-BGL-KVOT-HIGH                  
064600     MOVE 1                    TO CLIX                                    
064700                                                                          
064800     PERFORM UNTIL (CLIX > WS-ANT-CLAGER)                                 
064900                                                                          
065000       COMPUTE WS-BEHOV-BGL-KVOT (CLIX) ROUNDED                           
065100             =  WS-BEHOV (CLIX)                                           
065200             *  WS-TOT-BGL                                                
065300             /  WS-BGL   (CLIX)                                           
065400       END-COMPUTE                                                        
065500                                                                          
065600       IF  WS-BEHOV-BGL-KVOT (CLIX) > WS-BEHOV-BGL-KVOT-HIGH              
065700         MOVE WS-BEHOV-BGL-KVOT (CLIX)                                    
065800                               TO WS-BEHOV-BGL-KVOT-HIGH                  
065900       END-IF                                                             
066000       ADD 1                   TO CLIX                                    
066100     END-PERFORM                                                          
066200     .                                                                    
066300     EJECT                                                                
066400 DB-RANS-LTK-1 SECTION.                                                   
066500****************************************************************          
066600*                                                                         
066700*    RANSONERING FÖR LTK = 1                                              
066800*    ENDAST C1 BEHANDLAS.                                                 
066900*                                                                         
067000****************************************************************          
067100                                                                          
067200     MOVE 1                    TO RFIX                                    
067300                                                                          
067400*    *-- DISP BERÄKNAS FÖR VARJE RF.                                      
067500     PERFORM UNTIL (RFIX > K-ANT-RF)                                      
067600                                                                          
067700*      *-- RANSONERAD KVANT RÄKNAS UT. DECIMALER KAPAS BORT.              
067800       IF WS-BEHOV-BGL-KVOT-HIGH = 1                                      
067900           COMPUTE WS-RANSKV                                              
068000                 =  WS-TOT-GL                                             
068100           END-COMPUTE                                                    
068200       ELSE                                                               
068300           COMPUTE WS-RANSKV                                              
068400                 =  WS-TOT-GL                                             
068500                 -  (RFW-RERF (RFIX)                                      
068600                 *   WS-BEHOV-BGL-KVOT-HIGH)                              
068700           END-COMPUTE                                                    
068800       END-IF                                                             
068900                                                                          
069000       IF  WS-RANSKV > ZERO                                               
069100*        *-- RANSONERAD KVANT FINNS                                       
069200                                                                          
069300         IF  WS-RANSKV >= RROT-KVROS (C1)                                 
069400*          *-- RANSONERAD KVANT TÄCKER ALLA RESTORDER                     
069500           MOVE RROT-KVROS (C1) TO RUKW-DISP (RFIX, C1)                   
069600                                                                          
069700         ELSE                                                             
069800*          *-- RANSONERAD KVANT TÄCKER EJ ALLA RESTORDER                  
069900           MOVE WS-RANSKV      TO RUKW-DISP (RFIX, C1)                    
070000                                                                          
070100         END-IF                                                           
070200                                                                          
070300       ELSE                                                               
070400*        *-- RANSONERAD KVANT SAKNAS                                      
070500         MOVE ZERO             TO RUKW-DISP (RFIX, C1)                    
070600       END-IF                                                             
070700                                                                          
070800       ADD 1                   TO RFIX                                    
070900     END-PERFORM                                                          
071000                                                                          
071100     PERFORM DBC-BER-RANSON-DISP                                          
071200     .                                                                    
071300     EJECT                                                                
071400 DBC-BER-RANSON-DISP SECTION.                                             
071500****************************************************************          
071600*                                                                         
071700*    PRÖVAR HUR SÄNKNING AV RANSONERINGSFAKTORN (RF)                      
071800*    PÅVERKAR DISPONIBLA KVANTITETER.                                     
071900*    ENDAST C1 BEHANDLAS (LTK = 1).                                       
072000*                                                                         
072100*    OM  DISP VID RF 1.0 > NOLL                                           
072200*      OM  DISP VID RF 0.5 >= TOTALT RO                                   
072300*      OCH C1 HAR RO OCH ALLA DESSA TÄCKS                                 
072400*        VÄLJS DISP VID RF 0.5                                            
072500*      ANNARS                                                             
072600*        VÄLJS DISP VID RF 0.7                                            
072700*      SLUT-OM                                                            
072800*    ANNARS                                                               
072900*      SÄTTS DISP NOLL                                                    
073000*    SLUT-OM                                                              
073100*                                                                         
073200****************************************************************          
073300                                                                          
073400     MOVE RUKW-DISP (K-TABING-RF10, C1)                                   
073500                               TO WS-DISP-RF10                            
073600                                                                          
073700     IF  WS-DISP-RF10 > ZERO                                              
073800*      *-- DISP RF 1.0 > NOLL;  UTTAG FÅR GÖRAS.                          
073900                                                                          
074000       MOVE RUKW-DISP (K-TABING-RF05, C1)                                 
074100                               TO WS-DISP-RF05                            
074200                                                                          
074300       MOVE RROT-KVROS (C1)    TO WS-TOT-KVROS                            
074400                                                                          
074500       IF (WS-DISP-RF05 >= WS-TOT-KVROS)                                  
074600       AND  (RROT-KVROS (C1) > ZERO                                       
074700         AND RUKW-DISP (K-TABING-RF05, C1) >= RROT-KVROS (C1))            
074800                                                                          
074900*        *-- DISP MÅSTE VID RF 0.5 TÄCKA ALLA RESTORDER                   
075000*        *-- SAMT C1 MÅSTE HA RESTORDER                                   
075100*        *-- OCH DESSA SKALL TÄCKAS HELT,                                 
075200*        *-- FÖR ATT DISP FRÅN  RF 0.5 SKALL VÄLJAS.                      
075300                                                                          
075400         MOVE RUKW-DISP (K-TABING-RF05, C1)                               
075500                               TO RROT-KVDISP (C1)                        
075600                                                                          
075700       ELSE                                                               
075800*        *-- DISP FÖR RF 0.5 OTILLRÄCKLIGT,                               
075900*        *-- DISP FRÅN  RF 0.7 VÄLJES.                                    
076000                                                                          
076100         MOVE RUKW-DISP (K-TABING-RF07, C1)                               
076200                               TO RROT-KVDISP (C1)                        
076300       END-IF                                                             
076400                                                                          
076500     ELSE                                                                 
076600*      *-- DISP FÖR RF 1.0 SAKNAS, DISP SÄTTS NOLL                        
076700                                                                          
076800       MOVE ZERO               TO RROT-KVDISP (C1)                        
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 DC-RANS-LTK-OVRIGA SECTION.                                              
077300****************************************************************          
077400*                                                                         
077500*    RANSONERING FÖR LTK <> 1.                                            
077600*    BÅDA C-LAGREN BEHANDLAS.                                             
077700*                                                                         
077800****************************************************************          
077900                                                                          
078000     MOVE 1                    TO RFIX                                    
078100                                                                          
078200*    *-- DIVERSE DISPONIBLA KVANTITETER BERÄKNAS FÖR VARJE RF.            
078300     PERFORM UNTIL (RFIX > K-ANT-RF)                                      
078400                                                                          
078500*      *-- RANSONERAD KVANT RÄKNAS UT. MINIMERAD MHT C-LAGER.             
078600       IF WS-BEHOV-BGL-KVOT-HIGH = 1                                      
078700           COMPUTE WS-RANSKV                                              
078800                 =  WS-TOT-GL                                             
078900           END-COMPUTE                                                    
079000       ELSE                                                               
079100           COMPUTE WS-RANSKV                                              
079200                 =  WS-TOT-GL                                             
079300                 -  (RFW-RERF (RFIX)                                      
079400                 *   WS-BEHOV-BGL-KVOT-HIGH)                              
079500           END-COMPUTE                                                    
079600       END-IF                                                             
079700                                                                          
079800       IF  WS-RANSKV > ZERO                                               
079900*        *-- RANSONERAD KVANT FINNS                                       
080000                                                                          
080100         IF  WS-RANSKV >= RROT-KVROS (C1) + RROT-KVROS (C2)               
080200                                                                          
080300           PERFORM DCA-RANSKV-TILLRACKL                                   
080400                                                                          
080500         ELSE                                                             
080600           PERFORM DCB-RANSKV-BRIST                                       
080700                                                                          
080800         END-IF                                                           
080900                                                                          
081000         COMPUTE RUKW-DISP (RFIX, C1)                                     
081100               = RUKW-OVAN-LGE (RFIX, C1)                                 
081200               + RUKW-INOM-LGE (RFIX, C1)                                 
081300         END-COMPUTE                                                      
081400                                                                          
081500         COMPUTE RUKW-DISP (RFIX, C2)                                     
081600               = RUKW-OVAN-LGE (RFIX, C2)                                 
081700               + RUKW-INOM-LGE (RFIX, C2)                                 
081800         END-COMPUTE                                                      
081900                                                                          
082000       ELSE                                                               
082100*        *-- RANSONERAD KVANT SAKNAS                                      
082200         CONTINUE                                                         
082300       END-IF                                                             
082400                                                                          
082500       ADD 1                   TO RFIX                                    
082600     END-PERFORM                                                          
082700                                                                          
082800     PERFORM DCC-BER-RANSON-DISP                                          
082900     .                                                                    
083000     EJECT                                                                
083100 DCA-RANSKV-TILLRACKL SECTION.                                            
083200****************************************************************          
083300*                                                                         
083400*    RANSONERAD KVANT TÄCKER ALLA RESTORDER.                              
083500*    BÅDA C-LAGREN BEHANDLAS (LTK <> 1).                                  
083600*                                                                         
083700****************************************************************          
083800                                                                          
083900*    *-- RANSONERAD KVANT, MAX. TOTAL KVROS                               
084000     COMPUTE WS-RANSKV                                                    
084100           = RROT-KVROS (C1)                                              
084200           + RROT-KVROS (C2)                                              
084300     END-COMPUTE                                                          
084400                                                                          
084500     COMPUTE WS-BRIST-C1                                                  
084600           = RROT-KVROS (C1)                                              
084700           - WS-GL (C1)                                                   
084800     END-COMPUTE                                                          
084900                                                                          
085000     IF  WS-BRIST-C1 < ZERO                                               
085100       MOVE ZERO               TO WS-BRIST-C1                             
085200     END-IF                                                               
085300                                                                          
085400     COMPUTE RUKW-OVAN-LGE (RFIX, C2)                                     
085500           =  RROT-KVROS (C2)                                             
085600           +  WS-BRIST-C1                                                 
085700     END-COMPUTE                                                          
085800                                                                          
085900     IF  RUKW-OVAN-LGE (RFIX, C2) > WS-GL (C2)                            
086000*      *-- MAX GL                                                         
086100       MOVE WS-GL (C2)         TO RUKW-OVAN-LGE (RFIX, C2)                
086200     END-IF                                                               
086300                                                                          
086400     COMPUTE RUKW-OVAN-LGE (RFIX, C1)                                     
086500           =  WS-RANSKV                                                   
086600           -  RUKW-OVAN-LGE (RFIX, C2)                                    
086700     END-COMPUTE                                                          
086800                                                                          
086900*    *-- EFTERSOM DISP OVAN LGE TILLRÄCKLIG,                              
087000*    *-- SÅ GÅR MAN EJ NER I LGE.                                         
087100     MOVE ZERO                 TO RUKW-INOM-LGE (RFIX, C1)                
087200     MOVE ZERO                 TO RUKW-INOM-LGE (RFIX, C2)                
087300     .                                                                    
087400     EJECT                                                                
087500 DCB-RANSKV-BRIST SECTION.                                                
087600****************************************************************          
087700*                                                                         
087800*    RANSONERAD KVANT TÄCKER EJ ALLA RESTORDER.                           
087900*    BÅDA C-LAGREN BEHANDLAS (LTK <> 1).                                  
088000*                                                                         
088100****************************************************************          
088200                                                                          
088300*    *-- RANSONERAD KVANT FÖRDELAS PROPORTIONELLT                         
088400*    *-- ANTAL RESTORDER PER C-LAGER.                                     
088500     COMPUTE WS-RANSKV-ANDEL (C1) ROUNDED                                 
088600           =  WS-RANSKV                                                   
088700           *  RROT-KVROS (C1)                                             
088800            / (RROT-KVROS (C1) + RROT-KVROS (C2))                         
088900     END-COMPUTE                                                          
089000     COMPUTE WS-RANSKV-ANDEL (C2)                                         
089100           = WS-RANSKV                                                    
089200           - WS-RANSKV-ANDEL (C1)                                         
089300     END-COMPUTE                                                          
089400                                                                          
089500     COMPUTE WS-BRIST-C1                                                  
089600           = WS-RANSKV-ANDEL (C1)                                         
089700           - WS-GL           (C1)                                         
089800     END-COMPUTE                                                          
089900                                                                          
090000     IF  WS-BRIST-C1 < ZERO                                               
090100       MOVE ZERO               TO WS-BRIST-C1                             
090200     END-IF                                                               
090300                                                                          
090400     COMPUTE RUKW-OVAN-LGE (RFIX, C2)                                     
090500           =  WS-RANSKV-ANDEL (C2)                                        
090600           +  WS-BRIST-C1                                                 
090700     END-COMPUTE                                                          
090800                                                                          
090900     IF  RUKW-OVAN-LGE (RFIX, C2) > WS-GL (C2)                            
091000*      *-- MAX GL                                                         
091100       MOVE WS-GL (C2)         TO RUKW-OVAN-LGE (RFIX, C2)                
091200     END-IF                                                               
091300                                                                          
091400     COMPUTE RUKW-OVAN-LGE (RFIX, C1)                                     
091500           =  WS-RANSKV                                                   
091600           -  RUKW-OVAN-LGE (RFIX, C2)                                    
091700     END-COMPUTE                                                          
091800                                                                          
091900     IF  RUKW-OVAN-LGE (RFIX, C1) > WS-GL (C1)                            
092000*      *-- MAX GL                                                         
092100       MOVE WS-GL (C1)         TO RUKW-OVAN-LGE (RFIX, C1)                
092200     END-IF                                                               
092300                                                                          
092400     COMPUTE WS-BIDRAG-FR (C2)                                            
092500           =  RUKW-OVAN-LGE (RFIX, C2)                                    
092600           -  RROT-KVROS (C2)                                             
092700     END-COMPUTE                                                          
092800                                                                          
092900     IF  WS-BIDRAG-FR (C2) < ZERO                                         
093000       MOVE ZERO               TO WS-BIDRAG-FR (C2)                       
093100     END-IF                                                               
093200                                                                          
093300     COMPUTE RUKW-INOM-LGE (RFIX, C1)                                     
093400           =  RROT-KVROS (C1)                                             
093500           -  RUKW-OVAN-LGE (RFIX, C1)                                    
093600           -  WS-BIDRAG-FR (C2)                                           
093700     END-COMPUTE                                                          
093800                                                                          
093900*    *--- MIN. NOLL, MAX. LGE.                                            
094000     EVALUATE TRUE                                                        
094100       WHEN  RUKW-INOM-LGE (RFIX, C1) < ZERO                              
094200         MOVE ZERO             TO RUKW-INOM-LGE (RFIX, C1)                
094300       WHEN  RUKW-INOM-LGE (RFIX, C1) > WS-LGE (C1)                       
094400         MOVE WS-LGE (C1)      TO RUKW-INOM-LGE (RFIX, C1)                
094500       WHEN  OTHER                                                        
094600         CONTINUE                                                         
094700     END-EVALUATE                                                         
094800                                                                          
094900     COMPUTE WS-BIDRAG-FR (C1)                                            
095000           =  RUKW-OVAN-LGE (RFIX, C1)                                    
095100           -  RROT-KVROS (C1)                                             
095200     END-COMPUTE                                                          
095300                                                                          
095400     IF WS-BIDRAG-FR (C1) < ZERO                                          
095500       MOVE ZERO               TO WS-BIDRAG-FR (C1)                       
095600     END-IF                                                               
095700                                                                          
095800     COMPUTE RUKW-INOM-LGE (RFIX, C2)                                     
095900           =  RROT-KVROS (C2)                                             
096000           -  RUKW-OVAN-LGE (RFIX, C2)                                    
096100           -  WS-BIDRAG-FR (C1)                                           
096200     END-COMPUTE                                                          
096300                                                                          
096400*    *--- MIN. NOLL, MAX. LGE.                                            
096500     EVALUATE TRUE                                                        
096600       WHEN  RUKW-INOM-LGE (RFIX, C2) < ZERO                              
096700         MOVE ZERO             TO RUKW-INOM-LGE (RFIX, C2)                
096800       WHEN  RUKW-INOM-LGE (RFIX, C2) > WS-LGE (C2)                       
096900         MOVE WS-LGE (C2)      TO RUKW-INOM-LGE (RFIX, C2)                
097000       WHEN  OTHER                                                        
097100         CONTINUE                                                         
097200     END-EVALUATE                                                         
097300     .                                                                    
097400     EJECT                                                                
097500 DCC-BER-RANSON-DISP SECTION.                                             
097600****************************************************************          
097700*                                                                         
097800*    PRÖVAR HUR SÄNKNING AV RANSONERINGSFAKTORN (RF)                      
097900*    PÅVERKAR DISPONIBLA KVANTITETER.                                     
098000*    BÅDA C-LAGREN BEHANDLAS (LTK <> 1).                                  
098100*                                                                         
098200*    OM  DISP VID RF 1.0 > NOLL                                           
098300*      OM  DISP VID RF 0.5 >= TOTALT RO                                   
098400*      OCH ÅTMINSTONE ETT C-LAGER HAR RO OCH ALLA DESSA TÄCKS             
098500*        VÄLJS DISP VID RF 0.5                                            
098600*      ANNARS                                                             
098700*        VÄLJS DISP VID RF 0.7                                            
098800*      SLUT-OM                                                            
098900*    ANNARS                                                               
099000*      SÄTTS DISP NOLL                                                    
099100*    SLUT-OM                                                              
099200*                                                                         
099300****************************************************************          
099400                                                                          
099500     COMPUTE WS-DISP-RF10                                                 
099600             =  RUKW-DISP (K-TABING-RF10, C1)                             
099700             +  RUKW-DISP (K-TABING-RF10, C2)                             
099800     END-COMPUTE                                                          
099900                                                                          
100000     IF  WS-DISP-RF10 > ZERO                                              
100100*      *-- DISP RF 1.0 > NOLL;  UTTAG FÅR GÖRAS.                          
100200                                                                          
100300       COMPUTE WS-DISP-RF05                                               
100400               = RUKW-DISP (K-TABING-RF05, C1)                            
100500               + RUKW-DISP (K-TABING-RF05, C2)                            
100600       END-COMPUTE                                                        
100700                                                                          
100800       COMPUTE WS-TOT-KVROS                                               
100900             =   RROT-KVROS (C1)                                          
101000             +   RROT-KVROS (C2)                                          
101100       END-COMPUTE                                                        
101200                                                                          
101300       IF (WS-DISP-RF05 >= WS-TOT-KVROS)                                  
101400       AND ((RROT-KVROS (C1) > ZERO                                       
101500         AND RUKW-DISP (K-TABING-RF05, C1) >= RROT-KVROS (C1))            
101600        OR (RROT-KVROS (C2) > ZERO                                        
101700          AND RUKW-DISP (K-TABING-RF05, C2) >= RROT-KVROS (C2)))          
101800                                                                          
101900*        *-- DISP MÅSTE VID RF 0.5 TÄCKA ALLA RESTORDER                   
102000*        *-- SAMT ÅTMINSTONE DET ENA C-LAGRET MÅSTE HA RESTORDER          
102100*        *-- OCH DESSA SKALL TÄCKAS HELT,                                 
102200*        *-- FÖR ATT DISP FRÅN  RF 0.5 SKALL VÄLJAS.                      
102300                                                                          
102400         MOVE RUKW-DISP (K-TABING-RF05, C1)                               
102500                               TO RROT-KVDISP (C1)                        
102600         MOVE RUKW-DISP (K-TABING-RF05, C2)                               
102700                               TO RROT-KVDISP (C2)                        
102800                                                                          
102900       ELSE                                                               
103000*        *-- DISP FÖR RF 0.5 OTILLRÄCKLIGT,                               
103100*        *-- DISP FRÅN  RF 0.7 VÄLJES.                                    
103200                                                                          
103300         MOVE RUKW-DISP (K-TABING-RF07, C1)                               
103400                               TO RROT-KVDISP (C1)                        
103500         MOVE RUKW-DISP (K-TABING-RF07, C2)                               
103600                               TO RROT-KVDISP (C2)                        
103700       END-IF                                                             
103800                                                                          
103900     ELSE                                                                 
104000*      *-- DISP FÖR RF 1.0 SAKNAS, DISP SÄTTS NOLL                        
104100                                                                          
104200       MOVE ZERO               TO RROT-KVDISP (C1)                        
104300       MOVE ZERO               TO RROT-KVDISP (C2)                        
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 IMS-GU-ARTM01                 SECTION.                                   
104800     SKIP2                                                                
104900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
105000            DELIMITED BY SIZE INTO SSA1                                   
105100     MOVE '  GE'               TO GODK-STATUSKODER                        
105200     CALL CBLTDLI USING GU  ARTM-PCB ART-WDK901 SSA1                      
105300     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
105400     PERFORM IMS-STATUSKONTROLL                                           
105500     .                                                                    
105600     SKIP2                                                                
105700 IMS-GNP-ARTM11                SECTION.                                   
105800     SKIP2                                                                
105900     STRING 'WLARTM11(DABEHOV >=' W-DABEHOV-MIN-X                         
106000                    '&DABEHOV  <' W-DABEHOV-MAX-X ')'                     
106100            DELIMITED BY SIZE INTO SSA1                                   
106200     MOVE '  GE'               TO GODK-STATUSKODER                        
106300     CALL CBLTDLI USING GNP ARTM-PCB ANT-WDK911 SSA1                      
106400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     SKIP2                                                                
106710 IMS-GU-ARTS01                 SECTION.                                   
106720                                                                          
106730     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
106740            DELIMITED BY SIZE INTO SSA1                                   
106750     MOVE '  GE'               TO GODK-STATUSKODER                        
106760     CALL CBLTDLI USING GU  ARTS-PCB SART-WDK701 SSA1                     
106770     MOVE ARTS-STATUS-CODE     TO STATUS-WS                               
106780     PERFORM IMS-STATUSKONTROLL                                           
106790     .                                                                    
106791     SKIP2                                                                
106792 IMS-GNP-ARTS11                SECTION.                                   
106793                                                                          
106794     MOVE 'WLARTS11 ' TO SSA1                                             
106795     MOVE '  GE'               TO GODK-STATUSKODER                        
106796     CALL CBLTDLI USING GNP ARTS-PCB SLAG-WDK711 SSA1                     
106797     MOVE ARTS-STATUS-CODE     TO STATUS-WS                               
106798     PERFORM IMS-STATUSKONTROLL                                           
106799     .                                                                    
106800     EJECT                                                                
106810 IMS-STATUSKONTROLL            SECTION.                                   
106900     SKIP2                                                                
107000     SET STATUS-IX             TO 1                                       
107100     SEARCH GODK-STATUS AT END                                            
107200         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
107300              DELIMITED BY SIZE INTO FELTEXT                              
107400         CALL FELLOG                                                      
107500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
107600     END-SEARCH                                                           
107700     .                                                                    
107710     EJECT                                                                
107900*    -COPY WY2000P1                                                       
