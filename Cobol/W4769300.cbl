000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4769300.                                        
000400 AUTHOR.                 STINA MOGREN.                                    
000500 DATE-WRITTEN.           APRIL 2007.                                      
000600                                                                          
000700     REMARKS.                                                             
000800*      FUNKTION:                                                          
000900*        PROGRAMMET FÅR IDENTITER PÅ FÄRDIGPACKADE KOLLIN                 
001000*        FRÅN FAKTURERINGEN.                                              
001100*        PÅ DE ORDER SOM ÄR DIREKTLEVERANSER LÄSES RADERNA                
001200*        FRÅN ORDERRADSEGMENTET.                                          
001300*                                                                         
001400*        EN FIL SKAPAS:    - R33-TRANSAR TILL INLEVERANS                  
001500*      (PROGRAMMET ÄR EN KOPIA AV W47592)                                 
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 - OM RETURKOD FRÅN SORT (EX.VIS FÖR LITE SORTWK)           
001900*                                                                         
002000*    ÄNDRINGAR:                                                           
002100*        FEB-2008: SÄTT IDLEVNR PÅ RAD SOM KOMMER FRÅN SPEC-ORDER         
002200*                  4231 DÄR FLLSBOK=N OCH INMATAT IDLEVNR FINNS.          
002300*                  OM IDLEVNR-EJLS SAKNAS PÅ WDQ201 TAS HUV-LEV.          
002400*                  ETRACKER 5174148                                       
002500                                                                          
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*    ---- INFIL:                                                          
003400*                            - FÄRDIGPACKADE KOLLIN                       
003500     SELECT  W4768U        ASSIGN  W47693D1.                              
003600*    ---- UTFILER:                                                        
003700*                            - POSTER TILL INLEVERANS                     
003800     SELECT  W47693-R33    ASSIGN  W47693D2.                              
003900     SKIP2                                                                
004000*    ---- SORTFIL PÅ RANDOMKEY:                                           
004100*                                                                         
004200     SELECT  SORTFIL       ASSIGN  W47693DS.                              
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500                                                                          
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W4768U                                                               
004900     LABEL RECORD STANDARD                                                
005000     RECORDING  F                                                         
005100     BLOCK CONTAINS 0.                                                    
005200                                                                          
005300*    -COPY W4758U01      -L.                                              
005400     EJECT                                                                
005500 FD  W47693-R33                                                           
005600     LABEL RECORD STANDARD                                                
005700     RECORDING  V                                                         
005800     BLOCK CONTAINS 0.                                                    
005900                                                                          
006000*01  LPOST  -COPY W211R31            -L.                                  
006100                                                                          
006200*01  UTPOST -COPY W211R33            -L.                                  
006300     EJECT                                                                
006400 SD  SORTFIL                                                              
006500     RECORDING F.                                                         
006600                                                                          
006700 01  SD-AREA.                                                             
006800     03  SD-RANDOMKEY        PIC X(4).                                    
006900     03  SD-POST.                                                         
007000*        05  -COPY W4758U01       -PRE SD-.                               
007100     EJECT                                                                
007200*                                                                         
007300 WORKING-STORAGE SECTION.                                                 
007400     SKIP2                                                                
007500                                                                          
007600*    -- CHECKED BY WY2000                                                 
007700 77   IDPGM                  PIC X(8)    VALUE 'W4769300'.                
007800*    ---- KONSTANTER                                                      
007900                                                                          
008000 77  JA                      PIC X       VALUE 'J'.                       
008100 77  NEJ                     PIC X       VALUE 'N'.                       
008200 77  DATABAS                 PIC X(4)    VALUE 'WDE6'.                    
008300 77  SOFTWARE                PIC X(10)   VALUE 'SOFTWARE  '.              
008400     SKIP2                                                                
008500*    ---- END-OF-FILE SWITCHAR                                            
008600                                                                          
008700 77  INFIL-EOF               PIC X       VALUE 'N'.                       
008800 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
008900     SKIP2                                                                
009000*    ---- ÖVRIGA SWITCHAR                                                 
009100                                                                          
009200 77  LEV-FINNS               PIC X       VALUE 'N'.                       
009300                                                                          
009400 77  WDF2-TRAEFF-SW              PIC X       VALUE 'J'.                   
009500     88  WDF2-TRAEFF                         VALUE 'J'.                   
009600     88  EJ-WDF2-TRAEFF                      VALUE 'N'.                   
009700                                                                          
009800*    VALID DDGS                                                           
009900*01    -COPY WWLEV06                                                      
010000                                                                          
010100     SKIP3                                                                
010200*    ---- SPARFÄLT                                                        
010300                                                                          
010400 01  SPAR-IDDISTR            PIC S9(5)  COMP-3 VALUE ZERO.                
010500 01  SPAR-IDKUNDNR           PIC S9(7)  COMP-3 VALUE ZERO.                
010600 01  SPAR-IDFAKT             PIC S9(7)  COMP-3 VALUE ZERO.                
010700 01  SPAR-TIPACKN            PIC S9(7)  COMP-3 VALUE ZERO.                
010800 01  SPAR-TIFAKT             PIC S9(7)  COMP-3 VALUE ZERO.                
010900 01  SPAR-IDORDNR            PIC S9(5)  COMP-3 VALUE ZERO.                
011000 01  SPAR-IDLEVNR            PIC  X(5)         VALUE SPACE.               
011100 01  SPAR-BELEVNAMN          PIC X(4)          VALUE SPACE.               
011200 01  SPAR-KDFAKTYP           PIC X             VALUE SPACE.               
011300 01  SPAR-IDDC               PIC X(2)          VALUE SPACE.               
011400     EJECT                                                                
011500*    ---- DYNAMISKA SUBPROGRAM                                            
011600                                                                          
011700 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
011800 01  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.                
011900 01  W015RAND                PIC X(8)    VALUE 'W015RAND'.                
012000 01  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.                
012100 01  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.                
012200     SKIP3                                                                
012300     EJECT                                                                
012400*    ---- PARAMETRAR TILL ABEND                                           
012500                                                                          
012600 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
012700     EJECT                                                                
012800*    ----  PARAMETRAR TILL POSTSUM                                        
012900                                                                          
013000*01  -COPY W0005       -PRE POSTSUM-.                                     
013100     EJECT                                                                
013200*    ----  AREA FÖR SORTERADE TRANSAKTIONER                               
013300 01  FILLER          PIC X(24)   VALUE 'WSORT-AREA-START'.                
013400     SKIP2                                                                
013500 01  WSORT-AREA.                                                          
013600     03  FILLER         PIC X(4).                                         
013700*    03  POST  -COPY W4758U01    -PRE  WSORT-.                            
013800     EJECT                                                                
013900*    ----  AREA FÖR R33-OR                                                
014000 01  FILLER          PIC X(24)   VALUE 'UT-R33-START'.                    
014100     SKIP2                                                                
014200*01  AREA   -COPY W211R33     -PRE UTR33-.                                
014300     EJECT                                                                
014400*    ----  TABELLAREA LEVERANTÖR                                          
014500 01  FILLER          PIC X(24)   VALUE 'DIRLEV-TAB-START'.                
014600     SKIP2                                                                
014700 01  W476WDIR.                                                            
014800     03  DIRLEV-TAB.                                                      
014900         05  FILLER         PIC X(9)  VALUE 'NOKIAD0FR'.                  
015000         05  FILLER         PIC X(9)  VALUE 'GISLAY0CA'.                  
015100         05  FILLER         PIC X(9)  VALUE 'GOODAY0MA'.                  
015200         05  FILLER         PIC X(9)  VALUE 'UNICBK6SA'.                  
015300         05  FILLER         PIC X(9)  VALUE 'SAMHBQ8YA'.                  
015400         05  FILLER         PIC X(9)  VALUE 'BASFBYV5A'.                  
015500         05  FILLER         PIC X(9)  VALUE 'TORSC6T3A'.                  
015600         05  FILLER         PIC X(9)  VALUE 'HERBC96UD'.                  
015700         05  FILLER         PIC X(9)  VALUE 'HALDMSNFA'.                  
015800         05  FILLER         PIC X(9)  VALUE 'TUDON82JA'.                  
015900         05  FILLER         PIC X(9)  VALUE 'ARTEP1NQA'.                  
016000         05  FILLER         PIC X(9)  VALUE 'VABOS5PQA'.                  
016100         05  FILLER         PIC X(9)  VALUE 'VASES5PQB'.                  
016200         05  FILLER         PIC X(9)  VALUE 'MICHS6Q1A'.                  
016300         05  FILLER         PIC X(9)  VALUE 'VADEV022A'.                  
016400         05  FILLER         PIC X(9)  VALUE 'CLAS10987'.                  
016500         05  FILLER         PIC X(9)  VALUE 'VASE1345 '.                  
016600         05  FILLER         PIC X(9)  VALUE 'PIRE1429 '.                  
016700         05  FILLER         PIC X(9)  VALUE 'GISL1720 '.                  
016800         05  FILLER         PIC X(9)  VALUE 'CONT1727 '.                  
016900         05  FILLER         PIC X(9)  VALUE 'HERB1800 '.                  
017000         05  FILLER         PIC X(9)  VALUE 'TORS2351 '.                  
017100         05  FILLER         PIC X(9)  VALUE 'TUDO2370 '.                  
017200         05  FILLER         PIC X(9)  VALUE 'PUB 2394 '.                  
017300         05  FILLER         PIC X(9)  VALUE 'VABO254  '.                  
017400         05  FILLER         PIC X(9)  VALUE 'DUNL3061 '.                  
017500         05  FILLER         PIC X(9)  VALUE 'MICH3157 '.                  
017600         05  FILLER         PIC X(9)  VALUE 'UNIC3247 '.                  
017700         05  FILLER         PIC X(9)  VALUE 'BASF3259 '.                  
017800         05  FILLER         PIC X(9)  VALUE 'VADE6175 '.                  
017900         05  FILLER         PIC X(9)  VALUE 'VART645  '.                  
018000         05  FILLER         PIC X(9)  VALUE 'KONR6492 '.                  
018100         05  FILLER         PIC X(9)  VALUE 'HALD7302 '.                  
018200         05  FILLER         PIC X(9)  VALUE 'GOOD745  '.                  
018300         05  FILLER         PIC X(9)  VALUE 'SAMH80   '.                  
018400         05  FILLER         PIC X(9)  VALUE 'ARTE8204 '.                  
018500     03  REDIRLEV  REDEFINES DIRLEV-TAB.                                  
018600         05  DIRLEV OCCURS 36                                             
018700             ASCENDING KEY LEVNR                                          
018800             INDEXED BY DIR-IX.                                           
018900             07  NAMN            PIC X(4).                                
019000             07  LEVNR           PIC X(5).                                
019100     EJECT                                                                
019200*    ----   ARBETSAREOR TILL IMS-SEKTIONERNA                              
019300 01  FILLER          PIC X(24)   VALUE 'IMS-WS-START'.                    
019400                                                                          
019500*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
019600 01  W-IDPRODNR-X.                                                        
019700     03  W-IDPRODNR          PIC S9(7)    COMP-3.                         
019800 01  W-IDKOLLI-X.                                                         
019900     03  W-IDKOLLI           PIC S9(5)    COMP-3.                         
020000     SKIP3                                                                
020100 01  W-WDE4FSEQ-X.                                                        
020200     03  W-E4F-IDPRODNR      PIC S9(7)    COMP-3.                         
020300     03  W-E4F-IDKOLLI       PIC S9(5)    COMP-3.                         
020400 01  W-IDARTNR-X.                                                         
020500     03  W-IDARTNR           PIC S9(9)    COMP-3.                         
020600 01  W-IDLEVNSH-X.                                                        
020700     03  W-IDLEVNSH          PIC  X(5).                                   
020800 01  W-WDF211KEY-X.                                                       
020900     03  W-IDDISTR-X.                                                     
021000       05  W-IDDISTR         PIC S9(5)    COMP-3.                         
021100                                                                          
021200     03  W-IDKUNDNR-FOM-X.                                                
021300       05  W-IDKUNDNR-FOM    PIC S9(7)    COMP-3.                         
021400     03  W-IDKUNDNR-TOM-X.                                                
021500       05  W-IDKUNDNR-TOM    PIC S9(7)    COMP-3.                         
021600                                                                          
021700 01  W-WDQ2CSEQ-X.                                                        
021800     03    W-IDDISTR-Q2        PIC S9(5) COMP-3.                          
021900     03    W-IDKUNDNR-Q2       PIC S9(7) COMP-3.                          
022000     03    W-IDORDN7-Q2        PIC  9(7).                                 
022100     03    FILLER              PIC  X(3)        VALUE SPACE.              
022200                                                                          
022300 01  W-IDDC-X.                                                            
022400     03  W-IDDC                PIC X(2)    VALUE SPACE.                   
022500     SKIP3                                                                
022600*    --- STATUSKOD FRÅN IMS                                               
022700 01  STATUS-WS                   PIC XX.                                  
022800     88  SEGMENT-FINNS                       VALUE '  '.                  
022900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023000     88  BASEN-SLUT                          VALUE 'GB'.                  
023100     SKIP3                                                                
023200*      --- VALID IDDC CODES                                               
023300*                                                                         
023400*01    -COPY WWDC99                                                       
023500*01    -COPY WWDCKONS                                                     
023600*                                                                         
023700 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
023800*01  FILLER  -COPY WWDIST35   -RED TEST-IDDISTR.                          
023900     EJECT                                                                
024000                                                                          
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400 01  ALL-SSA.                                                             
024500     03 SSA1                     PIC X(96).                               
024600     03 SSA2                     PIC X(96).                               
024700     03 SSA3                     PIC X(96).                               
024800     EJECT                                                                
024900*01  -COPY W0003                                                          
025000     EJECT                                                                
025100 01  FILLER         PIC X(24)    VALUE 'DLI-IO-E601'.                     
025200 01  DLI-IO-E601.                                                         
025300*  03  -COPY WDE601                                                       
025400     EJECT                                                                
025500 01  FILLER         PIC X(24)    VALUE 'DLI-IO-E611'.                     
025600 01  DLI-IO-E611.                                                         
025700*  03  -COPY WDE611                                                       
025800     EJECT                                                                
025900 01  FILLER         PIC X(24)    VALUE 'DLI-IO-E401'.                     
026000 01  DLI-IO-E401.                                                         
026100*  03  -COPY WDE401                                                       
026200     EJECT                                                                
026300 01  FILLER         PIC X(24)    VALUE 'DLI-IO-E411'.                     
026400 01  DLI-IO-E411.                                                         
026500*  03  -COPY WDE411                                                       
026600     EJECT                                                                
026700 01  FILLER         PIC X(24)    VALUE 'DLI-IO-E421'.                     
026800 01  DLI-IO-E421.                                                         
026900*  03  -COPY WDE421                                                       
027000     EJECT                                                                
027100 01  FILLER         PIC X(24)    VALUE 'DLI-IO-K601'.                     
027200 01  DLI-IO-K601.                                                         
027300*  03  -COPY WDK601                                                       
027400     EJECT                                                                
027500 01  FILLER         PIC X(24)    VALUE 'DLI-IO-K623'.                     
027600 01  DLI-IO-K623.                                                         
027700*  03  -COPY WDK623                                                       
027800     EJECT                                                                
027900 01  FILLER         PIC X(24)    VALUE 'DLI-IO-F201'.                     
028000 01  DLI-IO-F201.                                                         
028100*  03  -COPY WDF201                                                       
028200     EJECT                                                                
028300 01  FILLER         PIC X(24)    VALUE 'DLI-IO-F211'.                     
028400 01  DLI-IO-F211.                                                         
028500*  03  -COPY WDF211                                                       
028600     EJECT                                                                
028700 01  FILLER         PIC X(24)    VALUE 'DLI-IO-Q201'.                     
028800 01  DLI-IO-Q201.                                                         
028900*  03  -COPY WDQ201                                                       
029000     EJECT                                                                
029100 01  FILLER         PIC X(24)    VALUE 'DLI-IO-B601'.                     
029200 01  DLI-IO-B601.                                                         
029300*  03  -COPY WDB601                                                       
029400                                                                          
029500     EJECT                                                                
029600 LINKAGE SECTION.                                                         
029700     SKIP3                                                                
029800*01  -COPY W0009      -PRE  MSG-                                          
029900     SKIP3                                                                
030000*01  -COPY W0008      -PRE  WDE6-                                         
030100       05  FILLER                PIC X.                                   
030200     SKIP3                                                                
030300*01  -COPY W0008      -PRE  WDE4-                                         
030400       05  FILLER                PIC X.                                   
030500     SKIP3                                                                
030600*01  -COPY W0008      -PRE  WDK6-                                         
030700       05  FILLER                PIC X.                                   
030800     SKIP3                                                                
030900*01  -COPY W0008      -PRE  WDF2-                                         
031000       05  FILLER                PIC X.                                   
031100     SKIP3                                                                
031200*01  -COPY W0008      -PRE  WDQ2-                                         
031300       05  FILLER                PIC X.                                   
031400     SKIP3                                                                
031500*01  -COPY W0008      -PRE  WDB6-                                         
031600       05  FILLER                PIC X.                                   
031700     EJECT                                                                
031800 PROCEDURE DIVISION  USING  MSG-PCB  WDE6-PCB                             
031900                            WDE4-PCB WDK6-PCB                             
032000                            WDF2-PCB WDQ2-PCB WDB6-PCB.                   
032100     ENTRY 'DLITCBL' USING  MSG-PCB  WDE6-PCB                             
032200                            WDE4-PCB WDK6-PCB                             
032300                            WDF2-PCB WDQ2-PCB WDB6-PCB.                   
032400     SKIP2                                                                
032500     PERFORM A-INIT                                                       
032600                                                                          
032700     SORT SORTFIL                                                         
032800       ASCENDING SD-RANDOMKEY SD-IDPRODNR                                 
032900       INPUT PROCEDURE B-LAGG-IN-RANDOMKEY                                
033000       OUTPUT PROCEDURE C-LAS-BAS-SKAPA-FIL.                              
033100                                                                          
033200     IF  SORT-RETURN > ZERO                                               
033300       DISPLAY '*** W4769300 - FEL VID SORTERING'                         
033400       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
033500     ELSE                                                                 
033600       PERFORM Z-FINIT                                                    
033700       MOVE ZERO TO RETURN-CODE                                           
033800       GOBACK                                                             
033900     END-IF                                                               
034000                                                                          
034100     .                                                                    
034200     EJECT                                                                
034300 A-INIT       SECTION.                                                    
034400     SKIP2                                                                
034500     OPEN INPUT  W4768U                                                   
034600          OUTPUT W47693-R33                                               
034700                                                                          
034800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
034900     .                                                                    
035000     EJECT                                                                
035100 B-LAGG-IN-RANDOMKEY SECTION.                                             
035200     SKIP2                                                                
035300     PERFORM S01-LAS-INFIL                                                
035400                                                                          
035500     PERFORM UNTIL INFIL-EOF = JA                                         
035600         CALL W015RAND USING SD-IDPRODNR SD-RANDOMKEY DATABAS             
035700         PERFORM S02-SKRIV-SORTFIL                                        
035800         PERFORM S01-LAS-INFIL                                            
035900     END-PERFORM                                                          
036000     .                                                                    
036100     EJECT                                                                
036200 C-LAS-BAS-SKAPA-FIL SECTION.                                             
036300     SKIP2                                                                
036400     PERFORM S03-LAS-SORTERAD-FIL                                         
036500                                                                          
036600     PERFORM UNTIL SORTFIL-EOF = JA                                       
036700                                                                          
036800       MOVE WSORT-IDPRODNR TO W-IDPRODNR                                  
036900       PERFORM IMS-GU-WDE601                                              
037000                                                                          
037100       MOVE VORD-IDDC      TO WS-IDDC                                     
037200       IF VORD-FLDIRLEV = JA                                              
037300                                                                          
037400         MOVE WSORT-IDKOLLI TO W-IDKOLLI                                  
037500         PERFORM IMS-GNP-WDE611                                           
037600                                                                          
037700         IF SEGMENT-FINNS                                                 
037800           MOVE KOLLI-TIPACKN      TO SPAR-TIPACKN                        
037900           MOVE KOLLI-IDDISTR      TO SPAR-IDDISTR                        
038000           MOVE KOLLI-IDKUNDNR     TO SPAR-IDKUNDNR                       
038100           MOVE KOLLI-IDFAKT       TO SPAR-IDFAKT                         
038200                                                                          
038300           MOVE VORD-IDPRODNR TO W-E4F-IDPRODNR                           
038400           MOVE KOLLI-IDKOLLI TO W-E4F-IDKOLLI                            
038500           PERFORM IMS-GU-WDE411-FSEQ                                     
038600           IF SEGMENT-FINNS                                               
038700             PERFORM IMS-GNP-WDE401                                       
038800             IF SEGMENT-FINNS AND KORD-FLORDSPE = NEJ                     
038900                                                                          
039000               SEARCH ALL DIRLEV AT END                                   
039100                MOVE NEJ TO LEV-FINNS                                     
039200                WHEN LEVNR(DIR-IX) = ORAD-IDLEVNR                         
039300                MOVE JA            TO LEV-FINNS                           
039400                MOVE LEVNR(DIR-IX) TO SPAR-IDLEVNR                        
039500                MOVE NAMN (DIR-IX) TO SPAR-BELEVNAMN                      
039600               END-SEARCH                                                 
039700                                                                          
039800               MOVE ORAD-IDLEVNR   TO LEV06-IDLEVNR                       
039900                                      SPAR-IDLEVNR                        
040000                                                                          
040100               IF  LEV-FINNS = JA OR LEV06-DDGS                           
040200                 PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT               
040300                   IF ORAD-IDBIL > SPACE AND                              
040400                      ORAD-IDLEVNR = '1441 ' OR 'BP2TW'                   
040500                     CONTINUE                                             
040600                   ELSE                                                   
040700                     PERFORM IMS-GNP-WDE421                               
040800                     PERFORM CA-SKAPA-R33                                 
040900                   END-IF                                                 
041000                   PERFORM IMS-GN-WDE411-FSEQ                             
041100                 END-PERFORM                                              
041200               END-IF                                                     
041300             END-IF                                                       
041400           END-IF                                                         
041500         END-IF                                                           
041600       ELSE                                                               
041700         IF CDC-SE OR NDC-JP                                              
041800           MOVE VORD-KDFAKTYP TO SPAR-KDFAKTYP                            
041900           MOVE WSORT-IDKOLLI TO W-IDKOLLI                                
042000           PERFORM IMS-GNP-WDE611                                         
042100                                                                          
042200           IF SEGMENT-FINNS                                               
042300             MOVE KOLLI-TIFAKT      TO SPAR-TIFAKT                        
042400             MOVE KOLLI-IDDISTR     TO SPAR-IDDISTR                       
042500             MOVE KOLLI-IDKUNDNR    TO SPAR-IDKUNDNR                      
042600             MOVE KOLLI-IDFAKT      TO SPAR-IDFAKT                        
042700                                                                          
042800             MOVE VORD-IDPRODNR TO W-E4F-IDPRODNR                         
042900             MOVE KOLLI-IDKOLLI TO W-E4F-IDKOLLI                          
043000             PERFORM IMS-GU-WDE411-FSEQ                                   
043100             IF SEGMENT-FINNS AND ORAD-FLDIRLEV = JA                      
043200                              AND ORAD-IDSYSTEM NOT = 'SOFT'              
043300               PERFORM IMS-GNP-WDE401                                     
043400               MOVE KORD-IDORDNR5 TO SPAR-IDORDNR                         
043500               IF SEGMENT-FINNS AND KORD-FLORDSPE = JA                    
043600                                AND KORD-FLLSBOK = NEJ                    
043700                                AND SPAR-KDFAKTYP NOT = 'G'               
043800                                AND WSORT-BEVOLREF NOT = SOFTWARE         
043900                 PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT               
044000                   PERFORM CC-HAMTA-LEVNR                                 
044100                   PERFORM IMS-GNP-WDE421                                 
044200                   PERFORM CD-SKAPA-R33-EJ-LAGERAVBOK                     
044300                   PERFORM IMS-GN-WDE411-FSEQ                             
044400                 END-PERFORM                                              
044500               END-IF                                                     
044600             ELSE                                                         
044700*                                                                         
044800*---           VOR DC11 --> CN/IN - FÖRSTA FAKT.                          
044900*                                                                         
045000               IF CDC-SE AND SEGMENT-FINNS AND                            
045100                  VORD-IDDC-EXP > SPACE                                   
045200                 IF VORD-IDDC-EXP NOT = 11                                
045300                   IF KOLLI-TIFAKT-EXP = 0                                
045400                     IF DCS-IDDC NOT = VORD-IDDC                          
045500                        MOVE VORD-IDDC TO W-IDDC                          
045600                        PERFORM IMS-GU-WDB601                             
045700                     END-IF                                               
045800                     MOVE VORD-KDFAKTYP TO SPAR-KDFAKTYP                  
045900                     MOVE WSORT-IDKOLLI TO W-IDKOLLI                      
046000                     MOVE KOLLI-TIFAKT   TO SPAR-TIFAKT                   
046100                     MOVE KOLLI-IDDISTR  TO SPAR-IDDISTR                  
046200                     MOVE KOLLI-IDKUNDNR TO SPAR-IDKUNDNR                 
046300                     MOVE KOLLI-IDFAKT   TO SPAR-IDFAKT                   
046400                     PERFORM IMS-GNP-WDE401                               
046500                     MOVE KORD-IDORDNR5 TO SPAR-IDORDNR                   
046600                     IF SEGMENT-FINNS                                     
046700                       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT         
046800                         PERFORM IMS-GNP-WDE421                           
046900                         PERFORM CE-SKAPA-R33-EXP                         
047000                         PERFORM IMS-GN-WDE411-FSEQ                       
047100                       END-PERFORM                                        
047200                     END-IF                                               
047300                   END-IF                                                 
047400                 END-IF                                                   
047500               END-IF                                                     
047600             END-IF                                                       
047700           END-IF                                                         
047800         ELSE                                                             
047900           MOVE VORD-IDDISTR        TO TEST-IDDISTR                       
048000           IF (DIST35-NONVCC-NONVCC-REFILL  OR                            
048010               DIST35-NONVCC-NONVCC-TRANSFER)                             
048100           AND (NDC-CN OR NDC-US OR                                       
048200                NDC-KR OR NDC-IN OR                                       
048300                NDC-MY OR NDC-TH OR                                       
048400                NDC-TW OR NDC-MX OR                                       
048500                NDC-BR)                                                   
048600             IF DCS-IDDC NOT = VORD-IDDC                                  
048700                MOVE VORD-IDDC TO W-IDDC                                  
048800                PERFORM IMS-GU-WDB601                                     
048900             END-IF                                                       
049000             MOVE VORD-KDFAKTYP TO SPAR-KDFAKTYP                          
049100             MOVE WSORT-IDKOLLI TO W-IDKOLLI                              
049200             PERFORM IMS-GNP-WDE611                                       
049300                                                                          
049400             IF SEGMENT-FINNS                                             
049500               MOVE KOLLI-TIFAKT-EXP TO SPAR-TIFAKT                       
049600               MOVE KOLLI-IDDISTR    TO SPAR-IDDISTR                      
049700               MOVE KOLLI-IDKUNDNR   TO SPAR-IDKUNDNR                     
049800               MOVE KOLLI-IDFAKT-EXP TO SPAR-IDFAKT                       
049900                                                                          
050000               MOVE VORD-IDPRODNR TO W-E4F-IDPRODNR                       
050100               MOVE KOLLI-IDKOLLI TO W-E4F-IDKOLLI                        
050200               PERFORM IMS-GU-WDE411-FSEQ                                 
050300               IF SEGMENT-FINNS                                           
050400                 PERFORM IMS-GNP-WDE401                                   
050500                 MOVE KORD-IDORDNR5 TO SPAR-IDORDNR                       
050600                 IF SEGMENT-FINNS                                         
050700                   PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT             
050800                     PERFORM IMS-GNP-WDE421                               
050900                     PERFORM CE-SKAPA-R33-EXP                             
051000                     PERFORM IMS-GN-WDE411-FSEQ                           
051100                   END-PERFORM                                            
051200                 END-IF                                                   
051300               END-IF                                                     
051400             END-IF                                                       
051500           END-IF                                                         
051600         END-IF                                                           
051700       END-IF                                                             
051800       PERFORM S03-LAS-SORTERAD-FIL                                       
051900     END-PERFORM                                                          
052000     .                                                                    
052100     EJECT                                                                
052200 CA-SKAPA-R33 SECTION.                                                    
052300     SKIP3                                                                
052400     MOVE WSORT-IDPTYP          TO UTR33-IDPTYP                           
052500                                                                          
052600     MOVE ORAD-IDARTNR          TO W-IDARTNR                              
052700     MOVE SPAR-IDLEVNR          TO W-IDLEVNSH                             
052800     PERFORM IMS-GU-WDK623                                                
052900     IF SEGMENT-FINNS                                                     
053000        MOVE AVT-IDLEVNR-AVT    TO UTR33-IDLEVNR-INL                      
053100     ELSE                                                                 
053200        MOVE SPAR-IDLEVNR       TO UTR33-IDLEVNR-INL                      
053300     END-IF                                                               
053400     MOVE WSORT-IDPRODNR        TO UTR33-IDAVINR                          
053500     MOVE +6                    TO UTR33-KDSORT2                          
053600     IF SPAR-BELEVNAMN = 'PUB '                                           
053700       MOVE +6                  TO UTR33-KDRT                             
053800     ELSE                                                                 
053900       MOVE ZERO                TO UTR33-KDRT                             
054000     END-IF                                                               
054100     MOVE ZERO                  TO UTR33-IDKONTO                          
054200*SAP                                                                      
054300     MOVE SPACE                 TO UTR33-IDANALYS                         
054400                                   UTR33-IDKST                            
054500     MOVE SPAR-TIPACKN          TO UTR33-TIAVIDAT                         
054600     MOVE ORAD-IDARTNR          TO UTR33-IDARTNR                          
054700     MOVE KKOLLI-KVLEVART       TO UTR33-KVAVIS                           
054800     MOVE WS-IDDC               TO UTR33-IDDC                             
054900     MOVE SPAR-IDDISTR          TO UTR33-IDDISTR                          
055000     MOVE SPAR-IDKUNDNR         TO UTR33-IDKUNDNR                         
055100     MOVE SPAR-IDFAKT           TO UTR33-IDFAKT                           
055200     MOVE KOLLI-IDSUPREF        TO UTR33-IDSUPREF                         
055300     MOVE KORD-IDPRODNR         TO UTR33-IDPRODNR                         
055400     MOVE KORD-IDKUNDRF         TO UTR33-IDKUNDRF                         
055500                                                                          
055600     PERFORM S04-SKRIV-R33                                                
055700     .                                                                    
055800     EJECT                                                                
055900 CC-HAMTA-LEVNR SECTION.                                                  
056000     SKIP3                                                                
056100     MOVE ORAD-IDARTNR     TO W-IDARTNR                                   
056200     MOVE KORD-IDDISTR     TO W-IDDISTR                                   
056300     MOVE +0               TO W-IDKUNDNR-FOM                              
056400     MOVE +9999999         TO W-IDKUNDNR-TOM                              
056500     MOVE NEJ              TO WDF2-TRAEFF-SW                              
056600                                                                          
056700     PERFORM IMS-GU-WDF201-ASEQ                                           
056800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR WDF2-TRAEFF            
056900       PERFORM IMS-GNP-WDF211-DISTRIKT                                    
057000       IF SEGMENT-FINNS                                                   
057100         IF DIR-IDDISTR-FOM = DIR-IDDISTR-TOM                             
057200           IF DIR-IDKUNDNR-FOM NOT > KORD-IDKUNDNR AND                    
057300                DIR-IDKUNDNR-TOM NOT < KORD-IDKUNDNR                      
057400                                                                          
057500             MOVE LEV-IDLEVNR TO SPAR-IDLEVNR                             
057600             MOVE JA          TO WDF2-TRAEFF-SW                           
057700           ELSE                                                           
057800             MOVE KORD-IDKUNDNR     TO W-IDKUNDNR-FOM                     
057900                                       W-IDKUNDNR-TOM                     
058000             PERFORM IMS-GNP-WDF211-KUND                                  
058100             IF SEGMENT-FINNS                                             
058200                                                                          
058300               MOVE LEV-IDLEVNR TO SPAR-IDLEVNR                           
058400               MOVE JA          TO WDF2-TRAEFF-SW                         
058500             END-IF                                                       
058600           END-IF                                                         
058700         ELSE                                                             
058800           MOVE LEV-IDLEVNR TO SPAR-IDLEVNR                               
058900           MOVE JA          TO WDF2-TRAEFF-SW                             
059000         END-IF                                                           
059100       END-IF                                                             
059200       IF EJ-WDF2-TRAEFF                                                  
059300         PERFORM IMS-GN-WDF201-ASEQ                                       
059400       END-IF                                                             
059500     END-PERFORM                                                          
059600                                                                          
059700     IF EJ-WDF2-TRAEFF                                                    
059800***    --KOLLA SPECIALORDER, IFALL HUV-LEV SKA OVERRIDAS                  
059900       MOVE KORD-IDDISTR  TO W-IDDISTR-Q2                                 
060000       MOVE KORD-IDKUNDNR TO W-IDKUNDNR-Q2                                
060100       MOVE KORD-IDORDNR5 TO W-IDORDN7-Q2                                 
060200       PERFORM IMS-GU-WDQ201-CSEQ                                         
060300       IF OHUV-IDLEVNR-EJLS  NOT = SPACE AND LOW-VALUE                    
060400         MOVE OHUV-IDLEVNR-EJLS TO SPAR-IDLEVNR                           
060500       ELSE                                                               
060600         PERFORM IMS-GU-WDK601                                            
060700         IF SEGMENT-FINNS                                                 
060800           MOVE ART-IDLEVNR TO SPAR-IDLEVNR                               
060900         ELSE                                                             
061000           MOVE SPACE TO SPAR-IDLEVNR                                     
061100         END-IF                                                           
061200         MOVE SPACE             TO SPAR-IDLEVNR                           
061300       END-IF                                                             
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 CD-SKAPA-R33-EJ-LAGERAVBOK SECTION.                                      
061800     SKIP3                                                                
061900     MOVE WSORT-IDPTYP          TO UTR33-IDPTYP                           
062000     MOVE +6                    TO UTR33-KDSORT2                          
062100     MOVE ORAD-IDARTNR          TO UTR33-IDARTNR                          
062200     MOVE SPAR-IDLEVNR          TO UTR33-IDLEVNR-INL                      
062300     MOVE ZERO                  TO UTR33-KDRT                             
062400     MOVE SPAR-TIFAKT           TO UTR33-TIAVIDAT                         
062500     MOVE ZERO                  TO UTR33-IDKONTO                          
062600*SAP                                                                      
062700     MOVE SPACE                 TO UTR33-IDANALYS                         
062800                                   UTR33-IDKST                            
062900     MOVE SPAR-IDORDNR          TO UTR33-IDAVINR                          
063000     MOVE KKOLLI-KVLEVART       TO UTR33-KVAVIS                           
063100     MOVE WC-CDC-SE             TO UTR33-IDDC                             
063200     MOVE SPAR-IDDISTR          TO UTR33-IDDISTR                          
063300     MOVE SPAR-IDKUNDNR         TO UTR33-IDKUNDNR                         
063400     MOVE SPAR-IDFAKT           TO UTR33-IDFAKT                           
063500     MOVE SPACE                 TO UTR33-IDSUPREF                         
063600     MOVE KORD-IDPRODNR         TO UTR33-IDPRODNR                         
063700     MOVE KORD-IDKUNDRF         TO UTR33-IDKUNDRF                         
063800                                                                          
063900     PERFORM S04-SKRIV-R33                                                
064000     .                                                                    
064100     EJECT                                                                
064200 CE-SKAPA-R33-EXP  SECTION.                                               
064300     SKIP3                                                                
064400     MOVE WSORT-IDPTYP          TO UTR33-IDPTYP                           
064500     MOVE +6                    TO UTR33-KDSORT2                          
064600     MOVE ORAD-IDARTNR          TO UTR33-IDARTNR                          
064700     MOVE DCS-IDLEVNR-DC        TO UTR33-IDLEVNR-INL                      
064800     MOVE ZERO                  TO UTR33-KDRT                             
064900     MOVE SPAR-TIFAKT           TO UTR33-TIAVIDAT                         
065000     MOVE ZERO                  TO UTR33-IDKONTO                          
065100*SAP                                                                      
065200     MOVE SPACE                 TO UTR33-IDANALYS                         
065300                                   UTR33-IDKST                            
065400     MOVE SPAR-IDORDNR          TO UTR33-IDAVINR                          
065500     MOVE KKOLLI-KVLEVART       TO UTR33-KVAVIS                           
065600     MOVE VORD-IDDC             TO UTR33-IDDC                             
065700     MOVE SPAR-IDDISTR          TO UTR33-IDDISTR                          
065800     MOVE SPAR-IDKUNDNR         TO UTR33-IDKUNDNR                         
065900     MOVE SPAR-IDFAKT           TO UTR33-IDFAKT                           
066000     MOVE SPACE                 TO UTR33-IDSUPREF                         
066100     MOVE KORD-IDPRODNR         TO UTR33-IDPRODNR                         
066200     MOVE KORD-IDKUNDRF         TO UTR33-IDKUNDRF                         
066300                                                                          
066400     PERFORM S04-SKRIV-R33                                                
066500     .                                                                    
066600     EJECT                                                                
066700 S01-LAS-INFIL SECTION.                                                   
066800     SKIP3                                                                
066900     READ W4768U INTO SD-POST                                             
067000     AT END                                                               
067100        MOVE JA TO INFIL-EOF                                              
067200     END-READ                                                             
067300                                                                          
067400     IF INFIL-EOF = NEJ                                                   
067500       MOVE 'W4768U'         TO POSTSUM-FDNAMN                            
067600       MOVE 'W47693D1'       TO POSTSUM-DDNAMN2                           
067700       MOVE 'R33'        TO POSTSUM-TRANSTYP                              
067800       CALL POSTSUM USING POSTSUM-PARM                                    
067900     END-IF                                                               
068000     .                                                                    
068100     SKIP3                                                                
068200 S02-SKRIV-SORTFIL SECTION.                                               
068300     SKIP3                                                                
068400     RELEASE SD-AREA                                                      
068500     .                                                                    
068600     EJECT                                                                
068700 S03-LAS-SORTERAD-FIL SECTION.                                            
068800     SKIP3                                                                
068900     RETURN SORTFIL INTO WSORT-AREA                                       
069000     AT END                                                               
069100        MOVE JA TO SORTFIL-EOF                                            
069200     END-RETURN                                                           
069300     .                                                                    
069400     SKIP3                                                                
069500 S04-SKRIV-R33    SECTION.                                                
069600                                                                          
069700     WRITE UTPOST FROM UTR33-AREA                                         
069800                                                                          
069900     MOVE 'W47693'               TO POSTSUM-FDNAMN                        
070000     MOVE 'W47693D2'             TO POSTSUM-DDNAMN2                       
070100     MOVE 'R33'                  TO POSTSUM-TRANSTYP                      
070200     CALL POSTSUM USING POSTSUM-PARM                                      
070300     .                                                                    
070400     EJECT                                                                
070500 Z-FINIT   SECTION.                                                       
070600     SKIP3                                                                
070700     CLOSE  W47693-R33 W4768U                                             
070800                                                                          
070900*    ----  SKRIV UT ANTAL LÄSTA OCH SKRIVNA POSTER                        
071000     MOVE 'S' TO POSTSUM-OPKOD                                            
071100     CALL POSTSUM USING POSTSUM-PARM                                      
071200     .                                                                    
071300     EJECT                                                                
071400*    ---- IMS SEKTIONER ----                                              
071500                                                                          
071600                                                                          
071700 IMS-GU-WDE601 SECTION.                                                   
071800                                                                          
071900     MOVE SPACE                 TO ALL-SSA                                
072000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
072100            DELIMITED BY SIZE INTO SSA1                                   
072200     MOVE '  '                  TO GODK-STATUSKODER                       
072300     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-E601 SSA1                     
072400     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
072500     PERFORM IMS-STATUSKONTROLL                                           
072600     .                                                                    
072700     SKIP3                                                                
072800 IMS-GNP-WDE611 SECTION.                                                  
072900                                                                          
073000     MOVE SPACE                 TO ALL-SSA                                
073100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
073200            DELIMITED BY SIZE INTO SSA1                                   
073300     MOVE '  GE'                TO GODK-STATUSKODER                       
073400     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
073500     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
073600     PERFORM IMS-STATUSKONTROLL                                           
073700     .                                                                    
073800     EJECT                                                                
073900 IMS-GU-WDE411-FSEQ SECTION.                                              
074000                                                                          
074100     MOVE SPACE                 TO ALL-SSA                                
074200     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
074300            DELIMITED BY SIZE INTO SSA1                                   
074400     MOVE '  GE'                TO GODK-STATUSKODER                       
074500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E411 SSA1                      
074600     MOVE WDE4-STATUS-CODE        TO STATUS-WS                            
074700     PERFORM IMS-STATUSKONTROLL                                           
074800     .                                                                    
074900     SKIP3                                                                
075000 IMS-GN-WDE411-FSEQ SECTION.                                              
075100                                                                          
075200     MOVE SPACE                 TO ALL-SSA                                
075300     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
075400            DELIMITED BY SIZE INTO SSA1                                   
075500     MOVE '  GEGB'              TO GODK-STATUSKODER                       
075600     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E411 SSA1                      
075700     MOVE WDE4-STATUS-CODE        TO STATUS-WS                            
075800     PERFORM IMS-STATUSKONTROLL                                           
075900     .                                                                    
076000     SKIP3                                                                
076100 IMS-GNP-WDE401 SECTION.                                                  
076200                                                                          
076300     MOVE SPACE                 TO ALL-SSA                                
076400     MOVE 'WDE401' TO SSA1                                                
076500     MOVE '    '                TO GODK-STATUSKODER                       
076600     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E401 SSA1                     
076700     MOVE WDE4-STATUS-CODE        TO STATUS-WS                            
076800     PERFORM IMS-STATUSKONTROLL                                           
076900     .                                                                    
077000     SKIP2                                                                
077100 IMS-GNP-WDE421 SECTION.                                                  
077200                                                                          
077300     MOVE SPACE                 TO ALL-SSA                                
077400     STRING 'WDE421  (WDE421KY =' W-WDE4FSEQ-X ')'                        
077500            DELIMITED BY SIZE INTO SSA1                                   
077600     MOVE '    '                TO GODK-STATUSKODER                       
077700     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E421 SSA1                     
077800     MOVE WDE4-STATUS-CODE        TO STATUS-WS                            
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100     EJECT                                                                
078200 IMS-GU-WDF201-ASEQ SECTION.                                              
078300                                                                          
078400     MOVE SPACE                 TO ALL-SSA                                
078500     STRING 'WDF201  (WDF2ASEQ =' W-IDARTNR-X ')'                         
078600            DELIMITED BY SIZE INTO SSA1                                   
078700     MOVE '  GE' TO GODK-STATUSKODER                                      
078800     CALL CBLTDLI USING GU  WDF2-PCB DLI-IO-F201 SSA1                     
078900     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
079000     PERFORM IMS-STATUSKONTROLL                                           
079100     .                                                                    
079200     SKIP3                                                                
079300 IMS-GN-WDF201-ASEQ SECTION.                                              
079400                                                                          
079500     MOVE SPACE                 TO ALL-SSA                                
079600     STRING 'WDF201  (WDF2ASEQ =' W-IDARTNR-X ')'                         
079700            DELIMITED BY SIZE INTO SSA1                                   
079800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
079900     CALL CBLTDLI USING GN  WDF2-PCB DLI-IO-F201 SSA1                     
080000     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
080100     PERFORM IMS-STATUSKONTROLL                                           
080200     .                                                                    
080300     SKIP3                                                                
080400 IMS-GNP-WDF211-DISTRIKT SECTION.                                         
080500                                                                          
080600     MOVE SPACE                 TO ALL-SSA                                
080700     STRING 'WDF211  (IDDISTRF<=' W-IDDISTR-X                             
080800                    '&IDDISTRT>=' W-IDDISTR-X                             
080900                    '&IDKUNDNF>=' W-IDKUNDNR-FOM-X                        
081000                    '&IDKUNDNT<=' W-IDKUNDNR-TOM-X ')'                    
081100            DELIMITED BY SIZE INTO SSA1                                   
081200     MOVE '  GE' TO GODK-STATUSKODER                                      
081300     CALL CBLTDLI USING GNP WDF2-PCB DLI-IO-F211 SSA1                     
081400     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
081500     PERFORM IMS-STATUSKONTROLL                                           
081600     .                                                                    
081700     SKIP3                                                                
081800 IMS-GNP-WDF211-KUND SECTION.                                             
081900                                                                          
082000     MOVE SPACE                 TO ALL-SSA                                
082100     STRING 'WDF211  (IDDISTRF =' W-IDDISTR-X                             
082200                    '&IDDISTRT =' W-IDDISTR-X                             
082300                    '&IDKUNDNF<=' W-IDKUNDNR-FOM-X                        
082400                    '&IDKUNDNT>=' W-IDKUNDNR-TOM-X ')'                    
082500            DELIMITED BY SIZE INTO SSA1                                   
082600     MOVE '  GE' TO GODK-STATUSKODER                                      
082700     CALL CBLTDLI USING GNP WDF2-PCB DLI-IO-F211 SSA1                     
082800     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
082900     PERFORM IMS-STATUSKONTROLL                                           
083000     .                                                                    
083100     EJECT                                                                
083200 IMS-GU-WDK601 SECTION.                                                   
083300                                                                          
083400     MOVE SPACE                 TO ALL-SSA                                
083500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
083600            DELIMITED BY SIZE INTO SSA1                                   
083700     MOVE '  '                TO GODK-STATUSKODER                         
083800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-K601 SSA1                      
083900     MOVE WDK6-STATUS-CODE        TO STATUS-WS                            
084000     PERFORM IMS-STATUSKONTROLL                                           
084100     .                                                                    
084200     SKIP3                                                                
084300 IMS-GU-WDK623 SECTION.                                                   
084400                                                                          
084500     MOVE SPACE                 TO ALL-SSA                                
084600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
084700            DELIMITED BY SIZE INTO SSA1                                   
084800     MOVE 'WDK611 '             TO SSA2                                   
084900     STRING 'WDK623  (IDLEVNSH =' W-IDLEVNSH-X ')'                        
085000            DELIMITED BY SIZE INTO SSA3                                   
085100     MOVE '  GE'                TO GODK-STATUSKODER                       
085200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-K623 SSA1 SSA2 SSA3            
085300     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
085400     PERFORM IMS-STATUSKONTROLL                                           
085500     .                                                                    
085600     SKIP3                                                                
085700 IMS-GU-WDQ201-CSEQ SECTION.                                              
085800                                                                          
085900     MOVE SPACE                 TO ALL-SSA                                
086000     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X  ')'                       
086100            DELIMITED BY SIZE INTO SSA1                                   
086200     MOVE '    ' TO GODK-STATUSKODER                                      
086300     CALL CBLTDLI USING GU  WDQ2-PCB DLI-IO-Q201 SSA1                     
086400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     EJECT                                                                
086800 IMS-GU-WDB601    SECTION.                                                
086900                                                                          
087000     MOVE SPACE                 TO ALL-SSA                                
087100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
087200          DELIMITED BY SIZE INTO SSA1                                     
087300     MOVE '    ' TO GODK-STATUSKODER                                      
087400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-B601 SSA1                      
087500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
087600     PERFORM IMS-STATUSKONTROLL                                           
087700     IF SEGMENT-SAKNAS                                                    
087800        MOVE SPACE TO DCS-KDDC                                            
087900     END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 IMS-STATUSKONTROLL SECTION.                                              
088300                                                                          
088400     SET STATUS-IX TO 1                                                   
088500     SEARCH GODK-STATUS                                                   
088600       AT END CALL FELLOG                                                 
088700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
088800     END-SEARCH                                                           
088900     .                                                                    
