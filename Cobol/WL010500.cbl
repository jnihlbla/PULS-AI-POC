000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010500.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/11/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:CARPARTS.LDC.WL0105                                             
000800*    WEB-LDC: WL010500 PROGRAM IS A REPLICA OF W6030500 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        UPPDATERAR BUFFERTSALDO LDC                                      
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*                              WLARTS (WDK7)                              
001600*                              WLBENA (WDD3)                              
001700*        PROGRAMMET UPPDATERAR WLARTD (WDD8)                              
001800*                              WLLOCB (WDJ9)                              
001900*                                                                         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: WL0105U                                             
002300*        REQUEST:     WZ01REQU                                            
002400*                     WL0105I1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        RESPONSE:    WZ01RESP                                            
002800*                     WL0105O1                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400     SKIP2                                                                
003500 FILE-CONTROL.                                                            
003600     SKIP2                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'WL010500'.            
004400 77  WS-ADRESS                   PIC X(50)   VALUE                        
004500          'CARPARTS.LDC.BUFFERINFORMATION'.                               
004600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004700 77  KDRC-DISPLAY                PIC Z(5).                                
004800 77  JA                          PIC X      VALUE 'J'.                    
004900 77  NEJ                         PIC X      VALUE 'N'.                    
005000 77  INDX-DISPLAY                PIC 999    VALUE ZERO.                   
005100 77  WS-RESP-AREA                PIC S9(5) VALUE ZERO COMP-3.             
005200 77  WS-KVANT                    PIC S9(5)  VALUE ZERO.                   
005300 77  RAD-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
005400 77  MAX-IX                      PIC S9(9)  VALUE +500 COMP SYNC.         
005500 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
005600 77  LOGG-DATUM                  PIC S9(8)  VALUE ZERO.                   
005700 77  LOGG-TID                    PIC S9(7)  VALUE ZERO.                   
005800 77  BUFFER-LOCATION             PIC X      VALUE 'B'.                    
005900                                                                          
006000 77  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
006100 77  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
006200 77  WS-CP-UTF8                  PIC X(4)   VALUE 'UTF8'.                 
006300 77  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
006400                                                                          
006500 77  INDATA-SW                   PIC X      VALUE 'J'.                    
006600     88  INDATA-OK                          VALUE 'J'.                    
006700     88  INDATA-FEL                         VALUE 'N'.                    
006800 77  NYCKLAR-SW                  PIC X      VALUE 'J'.                    
006900     88  NYCKLAR-OK                         VALUE 'J'.                    
007000     88  NYCKLAR-FEL                        VALUE 'N'.                    
007100 77  UPD-FRAN-NYCKELRAD-SW       PIC X      VALUE 'N'.                    
007200     88  UPD-FRAN-NYCKELRAD                 VALUE 'J'.                    
007300 77  INDATA-FINNS-SW             PIC X      VALUE 'N'.                    
007400     88  INDATA-FINNS                       VALUE 'J'.                    
007500     88  INDATA-SAKNAS                      VALUE 'N'.                    
007600 01  WS-KVBUFF                   PIC X(7).                                
007700 01  WS-ADBUFFOMR                PIC X(2).                                
007800 01  WS-ADBUFFGANG               PIC X(2).                                
007900 01  WS-ADBUFFPL                 PIC X(5).                                
008000 01  WS-ANTAL                    PIC 9(7).                                
008100 01  SW-REQU-INPUT-RAD           PIC X.                                   
008200                                                                          
008300 01  WS-SUBUFF-F                 PIC S9(7)   VALUE +0.                    
008400 01  WS-SUKOLLI-F                PIC S9(5)   VALUE +0.                    
008500                                                                          
008600 77  WS-RESP-ADBUFFOMR           PIC 9(2)    VALUE ZERO.                  
008700 77  WS-RESP-ADBUFFGANG          PIC 9(2)    VALUE ZERO.                  
008800 77  WS-RESP-ADBUFFPL            PIC 9(5)    VALUE ZERO.                  
008900 77  WS-RESP-KVBUFF-F            PIC 9(7)    VALUE ZERO.                  
009000 77  WS-RESP-KVBUFF-F-IN         PIC 9(7)    VALUE ZERO.                  
009100 77  WS-RESP-KVKOLLI-F           PIC 9(4)    VALUE ZERO.                  
009200 77  WS-RESP-KVKOLLI-F-IN        PIC 9(4)    VALUE ZERO.                  
009300 77  WS-RESP-SUKOLLI-F           PIC 9(4)    VALUE ZERO.                  
009400                                                                          
009500                                                                          
009600 77  AKTUELLT-LAND               PIC X       VALUE 'N'.                   
009700     88  AKTUELLT-LAND-KINA                  VALUE 'J'.                   
009800     88  AKTUELLT-LAND-EJ-KINA               VALUE 'N'.                   
009900     EJECT                                                                
010000                                                                          
010100 01  W-MINKEY-WDD811KY.                                                   
010200     03  W-MINKEY-IDTRANS          PIC X(4)    VALUE 'L105'.              
010300     03  W-MINKEY-ADBUFFOMR-ENTER  PIC S9(3)   VALUE +0 COMP-3.           
010400     03  W-MINKEY-DABUFPAF-ENTER   PIC  9(8)   VALUE ZERO.                
010500     03  W-MINKEY-ADBUFFGANG-ENTER PIC S9(3)   VALUE +0 COMP-3.           
010600     03  W-MINKEY-ADBUFFPL-ENTER   PIC S9(5)   VALUE +0 COMP-3.           
010700     03  W-MINKEY-ADBUFFOMR-NEXT   PIC S9(3)   VALUE +0 COMP-3.           
010800     03  W-MINKEY-DABUFPAF-NEXT    PIC  9(8)   VALUE ZERO.                
010900     03  W-MINKEY-ADBUFFGANG-NEXT  PIC S9(3)   VALUE +0 COMP-3.           
011000     03  W-MINKEY-ADBUFFPL-NEXT    PIC S9(5)   VALUE +0 COMP-3.           
011100                                                                          
011200 01  WS-INFO.                                                             
011300     03  WS-INFO1                PIC X(8)      VALUE 'PART NO '.          
011400     03  WS-INFOART              PIC Z(8)9     VALUE SPACE.               
011500     03  WS-INFO2                PIC X(20)     VALUE                      
011600                 ' IN SAME BUFFER LOC.'.                                  
011700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012000       EJECT                                                              
012100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012200 01  GENERELLA-SUBPROGRAM.                                                
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
012800     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
012900     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
013000     EJECT                                                                
013100 01  MESSAGE-CODES.                                                       
013200     03  UPDATE-DONE             PIC X(3)  VALUE '001'.                   
013300     03  NO-DATA-ENTERED         PIC X(3)  VALUE '014'.                   
013400     03  INVALID-KEY-FIELDS      PIC X(3)  VALUE '022'.                   
013500     03  IS-INVALID              PIC X(3)  VALUE '023'.                   
013600     03  TOO-MANY-LINES          PIC X(3)  VALUE '028'.                   
013700     03  SYSTEM-ERROR            PIC X(3)  VALUE '099'.                   
013800     03  CONFLICT-FIELDS         PIC X(3)  VALUE '046'.                   
013900     03  KEYS-ARE-MISSING        PIC X(30) VALUE '041'.                   
014000     03  ALREADY-EXIST           PIC X(30) VALUE '047'.                   
014100     03  PART-NO-IN-SAME-BUFFER-LOC PIC X(30) VALUE '107'.                
014200     03  ERR-UNAUTHORIZED        PIC X(30) VALUE '00A'.                   
014300     03  ERR-LOC-MISSING         PIC X(30) VALUE '706'.                   
014400     EJECT                                                                
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
014700*01  -COPY WTRAUTF8                                                       
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015000     SKIP3                                                                
015100*01  -COPY WZ01SUB                                                        
015200     EJECT                                                                
015300                                                                          
015400 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
015500*01  -COPY WMSGCONV                                                       
015600                                                                          
015700 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
015800*01  -COPY WZ01AUTH                                                       
      *01  -COPY WWDC99                                                         
           EJECT                                                                
015900                                                                          
016000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
016100     SKIP3                                                                
016200 01  REQU-AREA.                                                           
016300*    03  -COPY WZ01REQ2                                                   
016400*    03  -COPY WL0105I1                                                   
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
016700     SKIP3                                                                
016800 01  RESP-AREA.                                                           
016900*    03  -COPY WZ01RES2                                                   
017000*    03  -COPY WL0105O1                                                   
017100     EJECT                                                                
017200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-IDARTNR-X.                                                     
017600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017700     03  W-KDSEGKEY-X.                                                    
017800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
017900     03  W-ADBUFFGANG-X.                                                  
018000         05  W-ADBUFFGANG-SRCH   PIC S9(3)   VALUE ZERO COMP-3.           
018100     03  W-ADBUFFPL-X.                                                    
018200         05  W-ADBUFFPL-SRCH     PIC S9(5)   VALUE ZERO COMP-3.           
018300     03  W-WDD811KY-X.                                                    
018400         05  W-IDDC-X.                                                    
018500           07  W-IDDC            PIC X(2)    VALUE SPACE.                 
018600         05  W-ADBUFFOMR         PIC S9(3)   VALUE ZERO COMP-3.           
018700         05  W-DABUFPAF          PIC  9(8)   VALUE ZERO.                  
018800         05  W-ADBUFFGANG        PIC S9(3)   VALUE ZERO COMP-3.           
018900         05  W-ADBUFFPL          PIC S9(5)   VALUE ZERO COMP-3.           
019000     03  W-WDD811KY-MIN-X.                                                
019100         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
019200         05  W-ADBUFFOMR-MIN     PIC S9(3)   VALUE ZERO COMP-3.           
019300         05  W-DABUFPAF-MIN      PIC  9(8)   VALUE ZERO.                  
019400         05  W-ADBUFFGANG-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
019500         05  W-ADBUFFPL-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
019600     03  W-WDD811KY-MAX-X.                                                
019700         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
019800         05  W-ADBUFFOMR-MAX     PIC S9(3)   VALUE +999 COMP-3.           
019900         05  W-ADBUFPAF-MAX      PIC  9(8)   VALUE  99999999.             
020000         05  W-ADBUFFGANG-MAX    PIC S9(3)   VALUE +999 COMP-3.           
020100         05  W-ADBUFFPL-MAX      PIC S9(5)   VALUE +99999 COMP-3.         
020200     03  W-WDJ911KY-X.                                                    
020300         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
020400         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
020500         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
020600         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO COMP-3.           
020700         05  W-ADGANG            PIC 9(2)    VALUE ZERO COMP-3.           
020800         05  W-ADPLATS           PIC 9(5)    VALUE ZERO COMP-3.           
020900     03  W-WDD8A1KY-MIN-X.                                                
021000         05  W-IDDC-A1KY-MIN      PIC X(2)  VALUE SPACE.                  
021100         05  W-ADBUFFOM-A1KY-MIN  PIC S9(3) VALUE +000 COMP-3.            
021200         05  W-ADBUFGAN-A1KY-MIN  PIC S9(3) VALUE +000 COMP-3.            
021300         05  W-ADBUFPL-A1KY-MIN   PIC S9(5) VALUE +00000 COMP-3.          
021400         05  FILLER               PIC X(13) VALUE LOW-VALUE.              
021500     03  W-WDD8A1KY-MAX-X.                                                
021600         05  W-IDDC-A1KY-MAX      PIC X(2)  VALUE SPACE.                  
021700         05  W-ADBUFFOM-A1KY-MAX  PIC S9(3) VALUE +000 COMP-3.            
021800         05  W-ADBUFGAN-A1KY-MAX  PIC S9(3) VALUE +000 COMP-3.            
021900         05  W-ADBUFPL-A1KY-MAX   PIC S9(5) VALUE +00000 COMP-3.          
022000         05  FILLER               PIC X(13) VALUE HIGH-VALUE.             
022100                                                                          
022200     EJECT                                                                
022300                                                                          
022400     03  W-IDSKYLT-X.                                                     
022500         05  W-IDSKYLT           PIC X(3)  VALUE SPACE.                   
022600                                                                          
022700     03  W-IDDC-B6-X.                                                     
022800         05 W-IDDC-B6                  PIC X(2).                          
022900                                                                          
023000     SKIP2                                                                
023100*    --- STATUS-KOD FRÅN IMS                                              
023200 01  STATUS-WS                   PIC XX.                                  
023300     88  SEGMENT-FINNS                       VALUE '  '.                  
023400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023600     SKIP2                                                                
023700 01  GODK-STATUSKODER.                                                    
023800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023900     SKIP3                                                                
024000 01  SSA1                        PIC X(128).                              
024100 01  SSA2                        PIC X(96).                               
024200     EJECT                                                                
024300*    --- IMS FUNKTIONSKODER                                               
024400*01  -COPY W0003                                                          
024500     EJECT                                                                
024600*    ---  DLI INPUT-OUTPUT AREA                                           
024700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
024800     SKIP3                                                                
024900 01  DLI-IO-AREA.                                                         
025000     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
025100     SKIP3                                                                
025200     03  WLARTC01 REDEFINES IO-AREA.                                      
025300*        05  -COPY WDK601 -PRE ARTC-                                      
025400     EJECT                                                                
025500     03  WLARTC11 REDEFINES IO-AREA.                                      
025600*        05  -COPY WDK611 -PRE ARTC-                                      
025700     EJECT                                                                
025800     03  WLARTD01 REDEFINES IO-AREA.                                      
025900*        05  -COPY WDD801 -PRE ARTD-                                      
026000     EJECT                                                                
026100     03  WLARTD11 REDEFINES IO-AREA.                                      
026200*        05  -COPY WDD811 -PRE ARTD-                                      
026300     EJECT                                                                
026400     03  WLBENA11 REDEFINES IO-AREA.                                      
026500*        05  -COPY WDD311 -PRE BENA-                                      
026600     EJECT                                                                
026700     03  WLARTS11 REDEFINES IO-AREA.                                      
026800*        05  -COPY WDK711 -PRE ARTS-                                      
026900     EJECT                                                                
027000     03  WLLOCB01 REDEFINES IO-AREA.                                      
027100*        05  -COPY WDJ901 -PRE LOCB-                                      
027200     EJECT                                                                
027300     03  WLLOCB11 REDEFINES IO-AREA.                                      
027400*        05  -COPY WDJ911 -PRE LOCB-                                      
027500     EJECT                                                                
027600*    ---  DLI INPUT-OUTPUT AREA3                                          
027700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
027800 01  DLI-IO-AREA3.                                                        
027900     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
028000     SKIP3                                                                
028100     03  WDD8A1-X REDEFINES IO-AREA3.                                     
028200*        05  -COPY WDD8A1                                                 
028300                                                                          
028400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
028500 01   DLI-IO-AREA-B601.                                                   
028600*     03  -COPY WDB601                                                    
028700                                                                          
028800     EJECT                                                                
028900 LINKAGE SECTION.                                                         
029000*01  -COPY W0009   -PRE MSG-                                              
029100     EJECT                                                                
029200 01  ATAB-PCB                    PIC X.                                   
029300                                                                          
029400*01  -COPY W0008  -PRE ARTC-                                              
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008  -PRE ARTD-                                              
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008  -PRE BENA-                                              
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008  -PRE ASEQ-                                              
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008  -PRE ARTS-                                              
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008  -PRE LOCB-                                              
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008  -PRE WDD8A-                                             
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE WDB6-                                              
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB ARTC-PCB ARTD-PCB             
031900     BENA-PCB ARTS-PCB LOCB-PCB WDD8A-PCB WDB6-PCB.                       
032000                                                                          
032100 MAIN SECTION.                                                            
032200     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB ARTC-PCB ARTD-PCB             
032300     BENA-PCB ARTS-PCB LOCB-PCB WDD8A-PCB WDB6-PCB.                       
032400                                                                          
032500     PERFORM S01-HAEMTA-ANROPSDATA                                        
032600     IF SUB-KDRC = 0                                                      
032700       PERFORM A-INIT                                                     
032800       IF NYCKLAR-OK                                                      
032900         PERFORM B-KOLLA-NYCKLAR                                          
033000         IF NYCKLAR-OK                                                    
033100           IF REQU-UPDATE                                                 
033200             PERFORM G-KOLLA-INPUT                                        
033300             IF INDATA-OK                                                 
033400               PERFORM H-UPPDATERA                                        
033500             END-IF                                                       
033600           END-IF                                                         
033700           IF SUB-KDTRANS = 'WLA105' AND REQU-UPDATE                      
033800*            When called from API, there is no need to read               
033900*            again to check for latest info.                              
034000             CONTINUE                                                     
034100           ELSE                                                           
034200             PERFORM F-LAES-VISA-INFO                                     
034300           END-IF                                                         
034400         END-IF                                                           
034500       END-IF                                                             
034600       IF SUB-KDTRANS = 'WLA105'                                          
034700         PERFORM S11-MSG-CONV                                             
034800       END-IF                                                             
034900       PERFORM S02-RETURNERA-SVAR                                         
035000     END-IF                                                               
035100                                                                          
035200     MOVE ZERO TO RETURN-CODE                                             
035300     GOBACK                                                               
035400     .                                                                    
035500     EJECT                                                                
035600 A-INIT SECTION.                                                          
035700                                                                          
035800     MOVE JA                     TO NYCKLAR-SW                            
035900                                                                          
036000     MOVE ALL '+' TO RESP-AREA                                            
036100     MOVE SPACE   TO RESP-AREA                                            
036200     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
036300                     RESP-IDMSG-INFO                                      
036400                     RESP-IDELMT-ERROR                                    
036500     MOVE ZERO    TO RESP-KVRADER                                         
036600     MOVE '001'   TO RESP-IDRESVER                                        
036700     MOVE ZERO    TO W-ADBUFFOMR-MIN                                      
036800                     W-DABUFPAF-MIN                                       
036900                     W-ADBUFFGANG-MIN                                     
037000                     W-ADBUFFPL-MIN                                       
037100     MOVE 500     TO MAX-IX                                               
037200     IF SUB-KDTRANS(1:7) = 'WL0105T' OR 'WL0105U'                         
037300       CONTINUE                                                           
037400     ELSE                                                                 
037500                                                                          
037600       MOVE 001                  TO AUTH-KDCALL                           
037700       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
037800                                    REQU-WZ01REQ2                         
037900                                                                          
038000       IF AUTH-KDRC > 0                                                   
038100         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
038200         MOVE NEJ                TO NYCKLAR-SW                            
038300       END-IF                                                             
038400                                                                          
038500       MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)     TO                    
038600                                 REQU-KDPGMACT                            
038700       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)     TO                    
038800                                 REQU-IDDC-KEY                            
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 B-KOLLA-NYCKLAR SECTION.                                                 
039300                                                                          
039400***  KONTROLL AV REQU-UPDATE OR REQU-QUERY                                
039500     IF REQU-UPDATE OR REQU-QUERY                                         
039600        CONTINUE                                                          
039700     ELSE                                                                 
039800        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
039900        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
040000        MOVE NEJ TO NYCKLAR-SW                                            
040100     END-IF                                                               
040200                                                                          
040300***  KONTROLL IDDC                                                        
040400     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
                                 WS-IDDC                                        
040500     PERFORM IMS-GU-WDB601                                                
040600     IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                              
040700        MOVE REQU-IDDC-KEY TO W-IDDC                                      
040800                              W-IDDC-MIN                                  
040900                              W-IDDC-MAX                                  
041000                              W-IDDC-A1KY-MAX                             
041100                              W-IDDC-A1KY-MIN                             
041200                                                                          
041300        IF DCS-CHINA                                                      
041400           SET AKTUELLT-LAND-KINA     TO TRUE                             
041500        ELSE                                                              
041600           SET AKTUELLT-LAND-EJ-KINA  TO TRUE                             
041700        END-IF                                                            
041800     ELSE                                                                 
041900        MOVE 'IDDC' TO RESP-IDELMT-ERROR                                  
042000        MOVE NEJ TO NYCKLAR-SW                                            
042100     END-IF                                                               
042200                                                                          
042300***  KONTROLL AV IDARTNR                                                  
042400     IF REQU-IDARTNR-KEY = ALL '+'                                        
042500        MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                               
042600        MOVE NEJ TO NYCKLAR-SW                                            
042700     ELSE                                                                 
042800        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
042900        IF REQU-IDARTNR-KEY NUMERIC                                       
043000           MOVE REQU-IDARTNR-KEY TO W-IDARTNR                             
043100        ELSE                                                              
043200           MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                            
043300           MOVE NEJ TO NYCKLAR-SW                                         
043400        END-IF                                                            
043500     END-IF                                                               
043600                                                                          
043700***  KONTROLL AV KVBUFF                                                   
043800     IF REQU-KVBUFF-KEY NOT = ALL '+'                                     
043900        MOVE REQU-KVBUFF-KEY TO WS-KVBUFF                                 
044000     ELSE                                                                 
044100        MOVE ZERO TO WS-KVBUFF                                            
044200     END-IF                                                               
044300     INSPECT WS-KVBUFF REPLACING LEADING SPACE BY ZERO                    
044400     IF WS-KVBUFF NUMERIC                                                 
044500       CONTINUE                                                           
044600     ELSE                                                                 
044700        MOVE 'KVBUFF' TO RESP-IDELMT-ERROR                                
044800        MOVE NEJ TO NYCKLAR-SW                                            
044900     END-IF                                                               
045000                                                                          
045100***  KONTROLL AV ADBUFFOMR                                                
045200     IF REQU-ADBUFFOMR-KEY NOT = ALL '+'                                  
045300        MOVE REQU-ADBUFFOMR-KEY TO WS-ADBUFFOMR                           
045400     ELSE                                                                 
045500        MOVE ZERO TO WS-ADBUFFOMR                                         
045600     END-IF                                                               
045700     INSPECT WS-ADBUFFOMR REPLACING LEADING SPACE BY ZERO                 
045800     IF WS-ADBUFFOMR NUMERIC                                              
045900        MOVE WS-ADBUFFOMR TO W-ADBUFFOMR                                  
046000                            W-ADBUFFOMR-MIN                               
046100     ELSE                                                                 
046200        MOVE NEJ TO NYCKLAR-SW                                            
046300        MOVE 'ADBUFFOMR'  TO RESP-IDELMT-ERROR                            
046400     END-IF                                                               
046500                                                                          
046600***  KONTROLL AV ADBUFFGANG                                               
046700     IF REQU-ADBUFFGANG-KEY NOT = ALL '+'                                 
046800        MOVE REQU-ADBUFFGANG-KEY TO WS-ADBUFFGANG                         
046900     ELSE                                                                 
047000        MOVE ZERO TO WS-ADBUFFGANG                                        
047100     END-IF                                                               
047200     INSPECT WS-ADBUFFGANG REPLACING LEADING SPACE BY ZERO                
047300     IF WS-ADBUFFGANG NUMERIC                                             
047400       MOVE WS-ADBUFFGANG TO W-ADBUFFGANG                                 
047500                             W-ADBUFFGANG-MIN                             
047600     ELSE                                                                 
047700        MOVE NEJ TO NYCKLAR-SW                                            
047800        MOVE 'ADBUFFGANG' TO RESP-IDELMT-ERROR                            
047900     END-IF                                                               
048000                                                                          
048100***  KONTROLL AV ADBUFFPL                                                 
048200     IF REQU-ADBUFFPL-KEY NOT = ALL '+'                                   
048300        MOVE REQU-ADBUFFPL-KEY TO WS-ADBUFFPL                             
048400     ELSE                                                                 
048500        MOVE ZERO TO WS-ADBUFFPL                                          
048600     END-IF                                                               
048700     INSPECT WS-ADBUFFPL REPLACING LEADING SPACE BY ZERO                  
048800     IF WS-ADBUFFPL NUMERIC                                               
048900        MOVE WS-ADBUFFPL TO W-ADBUFFPL                                    
049000                            W-ADBUFFPL-MIN                                
049100     ELSE                                                                 
049200        MOVE NEJ TO NYCKLAR-SW                                            
049300        MOVE 'ADBUFFPL' TO RESP-IDELMT-ERROR                              
049400     END-IF                                                               
049500                                                                          
049600     MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                            
049700     INSPECT RESP-IDARTNR-KEY  REPLACING LEADING ZERO BY SPACE            
049800                                                                          
049900     MOVE WS-KVBUFF    TO RESP-KVBUFF-KEY                                 
050000     INSPECT RESP-KVBUFF-KEY REPLACING LEADING ZERO BY SPACE              
050100                                                                          
050200     MOVE WS-ADBUFFOMR TO RESP-ADBUFFOMR-KEY                              
050300     INSPECT RESP-ADBUFFOMR-KEY REPLACING LEADING ZERO BY SPACE           
050400                                                                          
050500     MOVE WS-ADBUFFGANG TO RESP-ADBUFFGANG-KEY                            
050600     INSPECT RESP-ADBUFFGANG-KEY REPLACING LEADING ZERO BY SPACE          
050700                                                                          
050800     MOVE WS-ADBUFFPL   TO RESP-ADBUFFPL-KEY                              
050900     INSPECT RESP-ADBUFFPL-KEY REPLACING LEADING ZERO BY SPACE            
051000     MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                  
051100                                                                          
051200     IF NYCKLAR-FEL                                                       
051300        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
051400           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
051500        ELSE                                                              
051600           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
051700        END-IF                                                            
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 F-LAES-VISA-INFO SECTION.                                                
052200                                                                          
052300     MOVE ZERO TO WS-SUBUFF-F                                             
052400                  WS-SUKOLLI-F                                            
052500     MOVE 500  TO MAX-IX                                                  
052600        MOVE ZERO          TO WS-KVBUFF                                   
052700        MOVE ZERO          TO WS-ADBUFFOMR                                
052800        MOVE WS-ADBUFFOMR  TO W-ADBUFFOMR                                 
052900                              W-ADBUFFOMR-MIN                             
053000                                                                          
053100        MOVE ZERO          TO WS-ADBUFFGANG                               
053200        MOVE WS-ADBUFFGANG TO W-ADBUFFGANG                                
053300                              W-ADBUFFGANG-MIN                            
053400                                                                          
053500        MOVE ZERO          TO WS-ADBUFFPL                                 
053600        MOVE WS-ADBUFFPL   TO W-ADBUFFPL                                  
053700                              W-ADBUFFPL-MIN                              
053800                                                                          
053900     PERFORM FA-LAES-GRUNDDATA                                            
054000                                                                          
054100     IF SEGMENT-SAKNAS                                                    
054200        MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                        
054300        MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERROR                         
054400     ELSE                                                                 
054500        PERFORM IMS-GU-ARTD01                                             
054600        IF SEGMENT-SAKNAS                                                 
054700           CONTINUE                                                       
054800****       MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                     
054900****       MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERROR                      
055000        ELSE                                                              
055100           MOVE ZERO TO WS-KVANT                                          
055200           MOVE +1 TO RAD-IX                                              
055300           PERFORM IMS-GNP-ARTD11                                         
055400           PERFORM UNTIL SEGMENT-SAKNAS                                   
055500              IF RAD-IX <= MAX-IX                                         
055600                 PERFORM FB-RADINFO                                       
055700                 PERFORM FC-SUMMA-RAD                                     
055800                 ADD +1 TO RAD-IX                                         
055900              END-IF                                                      
056000              ADD +1 TO WS-KVANT                                          
056100              PERFORM IMS-GNP-ARTD11                                      
056200           END-PERFORM                                                    
056300     IF REQU-ADBUFFOMR-UPD = ALL '+'                                      
056400        MOVE SPACE TO RESP-ADBUFFOMR-UPD                                  
056500     ELSE                                                                 
056600        MOVE REQU-ADBUFFOMR-UPD TO RESP-ADBUFFOMR-UPD                     
056700     END-IF                                                               
056800     IF REQU-ADBUFFGANG-UPD = ALL '+'                                     
056900        MOVE SPACE TO RESP-ADBUFFGANG-UPD                                 
057000     ELSE                                                                 
057100        MOVE REQU-ADBUFFGANG-UPD TO RESP-ADBUFFGANG-UPD                   
057200     END-IF                                                               
057300     IF REQU-ADBUFFPL-UPD = ALL '+'                                       
057400        MOVE SPACE TO RESP-ADBUFFPL-UPD                                   
057500     ELSE                                                                 
057600        MOVE REQU-ADBUFFPL-UPD TO RESP-ADBUFFPL-UPD                       
057700     END-IF                                                               
057800     IF REQU-KVBUFF-F-UPD = ALL '+'                                       
057900        MOVE SPACE            TO RESP-KVBUFF-F-UPD                        
058000     ELSE                                                                 
058100        MOVE REQU-KVBUFF-F-UPD TO RESP-KVBUFF-F-UPD                       
058200        INSPECT RESP-KVBUFF-F-UPD REPLACING                               
058300                LEADING ZERO BY SPACE                                     
058400     END-IF                                                               
058500     IF REQU-KVKOLLI-F-UPD = ALL '+'                                      
058600        MOVE SPACE             TO RESP-KVKOLLI-F-UPD                      
058700     ELSE                                                                 
058800      MOVE REQU-KVKOLLI-F-UPD TO RESP-KVKOLLI-F-UPD                       
058900      INSPECT RESP-KVKOLLI-F-UPD REPLACING                                
059000              LEADING ZERO BY SPACE                                       
059100     END-IF                                                               
059200                                                                          
059300           IF REQU-QUERY                                                  
059400           OR (REQU-UPDATE AND INDATA-OK)                                 
059500              MOVE SPACE TO  RESP-ADBUFFOMR-UPD                           
059600                             RESP-ADBUFFGANG-UPD                          
059700                             RESP-ADBUFFPL-UPD                            
059800                             RESP-KVBUFF-F-UPD                            
059900                             RESP-KVKOLLI-F-UPD                           
060000                             RESP-KVBUFF-KEY                              
060100                             RESP-ADBUFFOMR-KEY                           
060200                             RESP-ADBUFFGANG-KEY                          
060300                             RESP-ADBUFFPL-KEY                            
060400           END-IF                                                         
060500           MOVE WS-SUBUFF-F  TO RESP-SUBUFF-F                             
060600           MOVE WS-SUKOLLI-F TO WS-RESP-SUKOLLI-F                         
060700           MOVE WS-RESP-SUKOLLI-F TO RESP-SUKOLLI-F                       
060800           MOVE WS-KVANT     TO RESP-KVRADER                              
060900           IF WS-KVANT > MAX-IX                                           
061000              MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                     
061100           END-IF                                                         
061200        END-IF                                                            
061300     END-IF                                                               
061400     .                                                                    
061500     EJECT                                                                
061600 FA-LAES-GRUNDDATA SECTION.                                               
061700                                                                          
           IF DCS-CDC                                                           
            PERFORM IMS-GU-ARTC11                                               
           ELSE                                                                 
            PERFORM IMS-GU-ARTS11                                               
           END-IF                                                               
061900     IF SEGMENT-FINNS                                                     
            IF DCS-CDC                                                          
              MOVE ARTC-CLAG-ADLAGOMR TO RESP-ADLAGOMR                          
              MOVE ARTC-CLAG-ADGANG   TO RESP-ADGANG                            
              MOVE ARTC-CLAG-ADPLATS  TO RESP-ADPLATS                           
              MOVE ARTC-CLAG-KVLS     TO RESP-KVLS                              
            ELSE                                                                
062000        MOVE ARTS-SLAG-ADLAGOMR TO RESP-ADLAGOMR                          
062100        MOVE ARTS-SLAG-ADGANG   TO RESP-ADGANG                            
062200        MOVE ARTS-SLAG-ADPLATS  TO RESP-ADPLATS                           
062300        MOVE ARTS-SLAG-KVLS     TO RESP-KVLS                              
062400      END-IF                                                              
              MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                           
              IF DCS-UNICODE-IDSKYLT                                            
                 MOVE 'UTF8'             TO TRAUTF8-KDCP                        
              ELSE                                                              
                 MOVE '278 '             TO TRAUTF8-KDCP                        
              END-IF                                                            
063200        PERFORM IMS-GU-BENA11                                             
063300        IF SEGMENT-FINNS                                                  
063400          MOVE BENA-TEXT-BEART  TO TRAUTF8-TECONV-FROM                    
063500        ELSE                                                              
063600          MOVE WS-CP-EBCDIC     TO TRAUTF8-KDCP                           
063700          MOVE SPACE            TO TRAUTF8-TECONV-FROM                    
063800        END-IF                                                            
              IF TRAUTF8-TECONV-FROM = SPACES                                   
               MOVE 'GB'  TO W-IDSKYLT                                          
               MOVE '278' TO TRAUTF8-KDCP                                       
               PERFORM IMS-GU-BENA11                                            
               MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                   
              END-IF                                                            
063900                                                                          
064000*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
064100        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
064200                                                                          
064300*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
064400        MOVE TRAUTF8-TECONV-TO  TO RESP-BEART                             
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 FB-RADINFO SECTION.                                                      
064900                                                                          
065000     IF REQU-KDCMDVAL(RAD-IX) = ALL '+'                                   
065100        MOVE SPACE                 TO RESP-KDCMDVAL(RAD-IX)               
065200     ELSE                                                                 
065300        MOVE REQU-KDCMDVAL(RAD-IX) TO RESP-KDCMDVAL(RAD-IX)               
065400     END-IF                                                               
065500     IF REQU-KVBUFF-F-IN(RAD-IX) = ALL '+'                                
065600        MOVE ZERO                  TO RESP-KVBUFF-F-IN(RAD-IX)            
065700     ELSE                                                                 
065800        MOVE REQU-KVBUFF-F-IN(RAD-IX) TO RESP-KVBUFF-F-IN(RAD-IX)         
065900     END-IF                                                               
066000     IF REQU-KVKOLLI-F-IN(RAD-IX) = ALL '+'                               
066100        MOVE ZERO                  TO RESP-KVKOLLI-F-IN(RAD-IX)           
066200     ELSE                                                                 
066300      MOVE REQU-KVKOLLI-F-IN(RAD-IX) TO RESP-KVKOLLI-F-IN(RAD-IX)         
066400     END-IF                                                               
066500     IF REQU-QUERY                                                        
066600        MOVE ZERO                  TO RESP-KVBUFF-F-IN(RAD-IX)            
066700        MOVE SPACE                 TO RESP-KDCMDVAL(RAD-IX)               
066800        MOVE ZERO                  TO RESP-KVKOLLI-F-IN(RAD-IX)           
066900        MOVE SPACE                TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
067000     END-IF                                                               
067100     IF REQU-UPDATE AND INDATA-OK                                         
067200        MOVE ZERO                  TO RESP-KVBUFF-F-IN(RAD-IX)            
067300        MOVE SPACE                 TO RESP-KDCMDVAL(RAD-IX)               
067400        MOVE ZERO                  TO RESP-KVKOLLI-F-IN(RAD-IX)           
067500        MOVE SPACE                TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
067600     END-IF                                                               
067700                                                                          
067800     MOVE ARTD-SALDO-ADBUFFOMR  TO WS-RESP-ADBUFFOMR                      
067900     MOVE WS-RESP-ADBUFFOMR     TO RESP-ADBUFFOMR(RAD-IX)                 
068000     MOVE ARTD-SALDO-ADBUFFGANG TO WS-RESP-ADBUFFGANG                     
068100     MOVE WS-RESP-ADBUFFGANG    TO RESP-ADBUFFGANG(RAD-IX)                
068200     MOVE ARTD-SALDO-ADBUFFPL   TO WS-RESP-ADBUFFPL                       
068300     MOVE WS-RESP-ADBUFFPL      TO RESP-ADBUFFPL(RAD-IX)                  
068400     IF REQU-IDDC-KEY NOT =  92 OR 71                                     
068500        MOVE ARTD-SALDO-DABUFPAF   TO RESP-DABUFPAF(RAD-IX)               
068600     ELSE                                                                 
068700       IF ARTD-SALDO-ADBUFFOMR = 99 AND                                   
068800         (REQU-IDDC-KEY = 92 OR 71)                                       
068900          MOVE ARTD-SALDO-DABUFPAF   TO RESP-DABUFPAF(RAD-IX)             
069000       ELSE                                                               
069100          MOVE ZERO                  TO RESP-DABUFPAF(RAD-IX)             
069200       END-IF                                                             
069300     END-IF                                                               
069400     MOVE ARTD-SALDO-KVBUFF-F   TO WS-RESP-KVBUFF-F                       
069500     MOVE WS-RESP-KVBUFF-F      TO RESP-KVBUFF-F(RAD-IX)                  
069600     MOVE ARTD-SALDO-KVKOLLI-F  TO WS-RESP-KVKOLLI-F                      
069700     MOVE WS-RESP-KVKOLLI-F     TO RESP-KVKOLLI-F(RAD-IX)                 
069800     .                                                                    
069900     EJECT                                                                
070000 FC-SUMMA-RAD SECTION.                                                    
070100                                                                          
070200     ADD  ARTD-SALDO-KVBUFF-F  TO WS-SUBUFF-F                             
070300     ADD  ARTD-SALDO-KVKOLLI-F TO WS-SUKOLLI-F                            
070400     .                                                                    
070500     EJECT                                                                
070600 G-KOLLA-INPUT SECTION.                                                   
070700                                                                          
070800     IF REQU-KVRADER NUMERIC                                              
070900        MOVE REQU-KVRADER TO MAX-IX                                       
071000     END-IF                                                               
071100                                                                          
071200     MOVE JA TO INDATA-SW                                                 
071300     MOVE NEJ TO SW-REQU-INPUT-RAD                                        
071400     MOVE NEJ TO INDATA-FINNS-SW                                          
071500                                                                          
071600     IF REQU-ADBUFFOMR-UPD = ALL '+'                                      
071700     AND REQU-ADBUFFGANG-UPD = ALL '+'                                    
071800     AND REQU-ADBUFFPL-UPD = ALL '+'                                      
071900     AND REQU-KVBUFF-F-UPD = ALL '+'                                      
072000     AND REQU-KVKOLLI-F-UPD = ALL '+'                                     
072100         CONTINUE                                                         
072200     ELSE                                                                 
072300         MOVE JA TO SW-REQU-INPUT-RAD                                     
072400         MOVE JA TO INDATA-FINNS-SW                                       
072500     END-IF                                                               
072600                                                                          
072700     IF    WS-KVBUFF           > ZERO    OR                               
072800           WS-ADBUFFOMR        > ZERO    OR                               
072900           WS-ADBUFFPL         > ZERO                                     
073000       MOVE JA TO INDATA-FINNS-SW                                         
073100     END-IF                                                               
073200                                                                          
073300     MOVE +1 TO RAD-IX                                                    
073400     PERFORM UNTIL RAD-IX > MAX-IX OR INDATA-FINNS                        
073500        IF REQU-KDCMDVAL(RAD-IX) = ALL '+'   AND                          
073600           (REQU-KVBUFF-F-IN(RAD-IX) = ALL '+')  AND                      
073700           (REQU-KVKOLLI-F-IN(RAD-IX) = ALL '+')                          
073800          CONTINUE                                                        
073900        ELSE                                                              
074000          MOVE JA TO INDATA-FINNS-SW                                      
074100        END-IF                                                            
074200        ADD +1 TO RAD-IX                                                  
074300     END-PERFORM                                                          
074400                                                                          
074500     IF INDATA-SAKNAS                                                     
074600       MOVE NO-DATA-ENTERED TO RESP-IDMSG-ERROR                           
074700       MOVE NEJ TO INDATA-SW                                              
074800     ELSE                                                                 
074900       IF SUB-KDTRANS = 'WLA105'                                          
075000         PERFORM GA-KOLLA-KEYS                                            
075100       END-IF                                                             
075200       IF INDATA-OK                                                       
075300         PERFORM GB-KOLLA-RADER                                           
075400         IF SW-REQU-INPUT-RAD = JA                                        
075500           PERFORM GC-KOLLA-INPUT-RAD                                     
075600           IF INDATA-OK                                                   
075700             PERFORM GD-KOLLA-KONFLIKT                                    
075800           END-IF                                                         
075900         END-IF                                                           
076000         IF INDATA-FEL                                                    
076100           MOVE IS-INVALID       TO RESP-IDMSG-ERROR                      
076200         ELSE                                                             
076300           PERFORM GE-KOLL-MOT-DB                                         
076400           IF INDATA-FEL                                                  
076500             CONTINUE                                                     
076600           END-IF                                                         
076700         END-IF                                                           
076800       END-IF                                                             
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 GA-KOLLA-KEYS SECTION.                                                   
077300                                                                          
077400     PERFORM IMS-GU-ARTD01                                                
077500     IF SEGMENT-SAKNAS                                                    
077600       IF SW-REQU-INPUT-RAD = NEJ                                         
077700         MOVE ERR-LOC-MISSING    TO RESP-IDMSG-ERROR                      
077800         MOVE NEJ                TO INDATA-SW                             
077900       END-IF                                                             
078000     ELSE                                                                 
078100       IF SW-REQU-INPUT-RAD = NEJ                                         
078200         MOVE REQU-ADBUFFOMR-ONE TO W-ADBUFFOMR-MIN                       
078300         MOVE REQU-ADBUFFGANG-ONE                                         
078400                                 TO W-ADBUFFGANG-SRCH                     
078500         MOVE REQU-ADBUFFPL-ONE  TO W-ADBUFFPL-SRCH                       
078600                                                                          
078700         PERFORM IMS-GNP-02-ARTD11                                        
078800         IF SEGMENT-FINNS                                                 
078900           MOVE ARTD-SALDO-DABUFPAF                                       
079000                                 TO REQU-DABUFPAF-ONE                     
079100         ELSE                                                             
079200           MOVE ERR-LOC-MISSING  TO RESP-IDMSG-ERROR                      
079300           MOVE NEJ              TO INDATA-SW                             
079400         END-IF                                                           
079500       END-IF                                                             
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900 GB-KOLLA-RADER SECTION.                                                  
080000                                                                          
080100     MOVE +1 TO RAD-IX                                                    
080200     PERFORM UNTIL RAD-IX > MAX-IX                                        
080300        IF REQU-KDCMDVAL(RAD-IX) = ALL '+'                                
080400           CONTINUE                                                       
080500        ELSE                                                              
080600           MOVE REQU-KDCMDVAL(RAD-IX) TO RESP-KDCMDVAL(RAD-IX)            
080700           IF REQU-KDCMDVAL(RAD-IX) = 'OUT' OR 'IN ' OR 'D  '             
080800                                  OR 'O  ' OR 'I  ' OR 'ACT'              
080900              IF (REQU-KVBUFF-F-IN(RAD-IX) = ALL '+') AND                 
081000                 (REQU-KVKOLLI-F-IN(RAD-IX) = ALL '+')                    
081100                 IF REQU-KDCMDVAL(RAD-IX) = 'D  '                         
081200                    CONTINUE                                              
081300                 ELSE                                                     
081400                  MOVE RAD-IX TO INDX-DISPLAY                             
081500                  MOVE SPACE TO RESP-IDELMT-ERROR                         
081600                  STRING 'KDCMDVAL*' INDX-DISPLAY                         
081700                    DELIMITED BY SIZE                                     
081800                    INTO RESP-IDELMT-ERROR                                
081900                  MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(RAD-IX)             
082000                  MOVE NEJ TO INDATA-SW                                   
082100                 END-IF                                                   
082200              END-IF                                                      
082300           ELSE                                                           
082400              MOVE RAD-IX TO INDX-DISPLAY                                 
082500              MOVE SPACE TO RESP-IDELMT-ERROR                             
082600              STRING 'KDCMDVAL*' INDX-DISPLAY                             
082700                  DELIMITED BY SIZE                                       
082800                  INTO RESP-IDELMT-ERROR                                  
082900              MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                 
083000              MOVE NEJ TO INDATA-SW                                       
083100           END-IF                                                         
083200        END-IF                                                            
083300        PERFORM GBA-KOLLA-ANTAL                                           
083400        ADD +1 TO RAD-IX                                                  
083500     END-PERFORM                                                          
083600     .                                                                    
083700     EJECT                                                                
083800 GBA-KOLLA-ANTAL SECTION.                                                 
083900                                                                          
084000     IF (REQU-KVBUFF-F-IN(RAD-IX) NOT = ALL '+') OR                       
084100        (REQU-KVKOLLI-F-IN(RAD-IX) NOT = ALL '+')                         
084200        IF (REQU-KDCMDVAL (RAD-IX) = ALL '+' OR                           
084300           REQU-KDCMDVAL (RAD-IX) = 'D  ')                                
084400              MOVE RAD-IX TO INDX-DISPLAY                                 
084500              MOVE SPACE TO RESP-IDELMT-ERROR                             
084600              STRING 'KDCMDVAL*' INDX-DISPLAY                             
084700                  DELIMITED BY SIZE                                       
084800                  INTO RESP-IDELMT-ERROR                                  
084900              MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                 
085000              MOVE NEJ TO INDATA-SW                                       
085100        END-IF                                                            
085200                                                                          
085300        IF REQU-KVBUFF-F-IN(RAD-IX) NOT = ALL '+'                         
085400           MOVE REQU-KVBUFF-F-IN(RAD-IX)                                  
085500                         TO RESP-KVBUFF-F-IN(RAD-IX)                      
085600           IF REQU-KVBUFF-F-IN (RAD-IX) NUMERIC                           
085700              CONTINUE                                                    
085800           ELSE                                                           
085900              MOVE RAD-IX TO INDX-DISPLAY                                 
086000              MOVE SPACE TO RESP-IDELMT-ERROR                             
086100              STRING 'KVBUFF*' INDX-DISPLAY                               
086200                  DELIMITED BY SIZE                                       
086300                  INTO RESP-IDELMT-ERROR                                  
086400              MOVE 'QV' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                  
086500              MOVE NEJ TO INDATA-SW                                       
086600           END-IF                                                         
086700        END-IF                                                            
086800                                                                          
086900        IF REQU-KVKOLLI-F-IN(RAD-IX) NOT = ALL '+'                        
087000           MOVE REQU-KVKOLLI-F-IN(RAD-IX)                                 
087100                         TO RESP-KVKOLLI-F-IN(RAD-IX)                     
087200           IF REQU-KVKOLLI-F-IN(RAD-IX) NUMERIC                           
087300              CONTINUE                                                    
087400           ELSE                                                           
087500              MOVE RAD-IX TO INDX-DISPLAY                                 
087600              MOVE SPACE TO RESP-IDELMT-ERROR                             
087700              STRING 'KVKOLLI*' INDX-DISPLAY                              
087800                  DELIMITED BY SIZE                                       
087900                  INTO RESP-IDELMT-ERROR                                  
088000              MOVE 'QV' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                  
088100              MOVE NEJ TO INDATA-SW                                       
088200           END-IF                                                         
088300        END-IF                                                            
088400                                                                          
088500        IF REQU-KDCMDVAL(RAD-IX) = 'D  ' OR '+++'                         
088600           MOVE NEJ TO INDATA-SW                                          
088700           MOVE CONFLICT-FIELDS TO RESP-IDMSG-INFO                        
088800           MOVE 'CON' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                    
088900        END-IF                                                            
089000                                                                          
089100     ELSE                                                                 
089200                                                                          
089300        IF REQU-KDCMDVAL(RAD-IX) = 'IN ' OR 'OUT'                         
089400                               OR 'I  ' OR 'O  ' OR 'ACT'                 
089500           IF WS-KVBUFF NUMERIC                                           
089600              CONTINUE                                                    
089700           ELSE                                                           
089800              MOVE RAD-IX TO INDX-DISPLAY                                 
089900              MOVE SPACE TO RESP-IDELMT-ERROR                             
090000              STRING 'KDCMDVAL*' INDX-DISPLAY                             
090100                  DELIMITED BY SIZE                                       
090200                  INTO RESP-IDELMT-ERROR                                  
090300              MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                 
090400              MOVE NEJ TO INDATA-SW                                       
090500           END-IF                                                         
090600        END-IF                                                            
090700     END-IF                                                               
090800     .                                                                    
090900     EJECT                                                                
091000 GC-KOLLA-INPUT-RAD  SECTION.                                             
091100                                                                          
091200     IF REQU-ADBUFFOMR-UPD = ALL '+'                                      
091300        IF  REQU-ADBUFFGANG-UPD = ALL '+'                                 
091400        AND REQU-ADBUFFPL-UPD = ALL '+'                                   
091500           CONTINUE                                                       
091600        ELSE                                                              
091700           MOVE 'ADBUFFOMR'  TO RESP-IDELMT-ERROR                         
091800           MOVE 'OMR' TO RESP-IDMSG-ERROR-UPD                             
091900           MOVE NEJ TO INDATA-SW                                          
092000        END-IF                                                            
092100     ELSE                                                                 
092200        MOVE REQU-ADBUFFOMR-UPD TO RESP-ADBUFFOMR-UPD                     
092300        IF REQU-ADBUFFOMR-UPD NUMERIC AND                                 
092400           REQU-ADBUFFOMR-UPD > ZERO                                      
092500           CONTINUE                                                       
092600        ELSE                                                              
092700           MOVE 'ADBUFFOMR'  TO RESP-IDELMT-ERROR                         
092800           MOVE 'OMR' TO RESP-IDMSG-ERROR-UPD                             
092900           MOVE NEJ TO INDATA-SW                                          
093000        END-IF                                                            
093100     END-IF                                                               
093200                                                                          
093300     IF REQU-ADBUFFGANG-UPD = ALL '+'                                     
093400        CONTINUE                                                          
093500     ELSE                                                                 
093600        MOVE REQU-ADBUFFGANG-UPD TO RESP-ADBUFFGANG-UPD                   
093700        IF REQU-ADBUFFGANG-UPD NUMERIC                                    
093800           CONTINUE                                                       
093900        ELSE                                                              
094000           MOVE 'ADBUFFGANG' TO RESP-IDELMT-ERROR                         
094100           MOVE 'PL' TO RESP-IDMSG-ERROR-UPD                              
094200           MOVE NEJ TO INDATA-SW                                          
094300        END-IF                                                            
094400     END-IF                                                               
094500                                                                          
094600     IF REQU-ADBUFFPL-UPD = ALL '+'                                       
094700        CONTINUE                                                          
094800     ELSE                                                                 
094900        MOVE REQU-ADBUFFPL-UPD TO RESP-ADBUFFPL-UPD                       
095000        IF REQU-ADBUFFPL-UPD NUMERIC                                      
095100           CONTINUE                                                       
095200        ELSE                                                              
095300           MOVE 'ADBUFFGPL' TO RESP-IDELMT-ERROR                          
095400           MOVE 'PL' TO RESP-IDMSG-ERROR-UPD                              
095500           MOVE NEJ TO INDATA-SW                                          
095600        END-IF                                                            
095700     END-IF                                                               
095800                                                                          
095900     IF REQU-KVBUFF-F-UPD = ALL '+'                                       
096000        CONTINUE                                                          
096100     ELSE                                                                 
096200        MOVE REQU-KVBUFF-F-UPD TO RESP-KVBUFF-F-UPD                       
096300        IF REQU-KVBUFF-F-UPD NUMERIC                                      
096400           CONTINUE                                                       
096500        ELSE                                                              
096600           MOVE 'KVBUFF' TO RESP-IDELMT-ERROR                             
096700           MOVE 'QV' TO RESP-IDMSG-ERROR-UPD                              
096800           MOVE NEJ TO INDATA-SW                                          
096900        END-IF                                                            
097000     END-IF                                                               
097100                                                                          
097200     IF REQU-KVKOLLI-F-UPD = ALL '+'                                      
097300        CONTINUE                                                          
097400     ELSE                                                                 
097500       MOVE REQU-KVKOLLI-F-UPD TO RESP-KVKOLLI-F-UPD                      
097600       IF REQU-KVKOLLI-F-UPD NUMERIC                                      
097700          CONTINUE                                                        
097800       ELSE                                                               
097900          MOVE 'KVKOLLI' TO RESP-IDELMT-ERROR                             
098000          MOVE 'QV' TO RESP-IDMSG-ERROR-UPD                               
098100          MOVE NEJ TO INDATA-SW                                           
098200       END-IF                                                             
098300     END-IF                                                               
098400                                                                          
098500     IF REQU-ADBUFFOMR-UPD = ALL   '+'                                    
098600        IF  REQU-KVBUFF-F-UPD = ALL  '+'                                  
098700        AND REQU-KVKOLLI-F-UPD = ALL '+'                                  
098800           CONTINUE                                                       
098900        ELSE                                                              
099000           MOVE NEJ TO INDATA-SW                                          
099100           MOVE 'ADBUFFOMR' TO RESP-IDELMT-ERROR                          
099200           MOVE 'OMR' TO RESP-IDMSG-ERROR-UPD                             
099300        END-IF                                                            
099400     END-IF                                                               
099500     .                                                                    
099600     EJECT                                                                
099700 GD-KOLLA-KONFLIKT SECTION.                                               
099800                                                                          
099900     MOVE +1 TO RAD-IX                                                    
100000     PERFORM UNTIL RAD-IX > MAX-IX                                        
100100        IF REQU-KDCMDVAL (RAD-IX) =                                       
100200           (ALL '+' OR SPACE OR LOW-VALUE)                                
100300          CONTINUE                                                        
100400        ELSE                                                              
100500          MOVE CONFLICT-FIELDS  TO RESP-IDMSG-INFO                        
100600          MOVE NEJ TO INDATA-SW                                           
100700        END-IF                                                            
100800        ADD +1 TO RAD-IX                                                  
100900     END-PERFORM                                                          
101000     .                                                                    
101100     EJECT                                                                
101200 GE-KOLL-MOT-DB SECTION.                                                  
101300                                                                          
101400     PERFORM IMS-GU-ARTC11                                                
101500     IF SEGMENT-SAKNAS                                                    
101600        MOVE NEJ TO INDATA-SW                                             
101700        MOVE KEYS-ARE-MISSING TO RESP-IDMSG-INFO                          
101800     ELSE                                                                 
101900        PERFORM IMS-GU-ARTS11                                             
102000        IF SEGMENT-SAKNAS                                                 
102100           MOVE NEJ TO INDATA-SW                                          
102200           MOVE KEYS-ARE-MISSING TO RESP-IDMSG-INFO                       
102300        END-IF                                                            
102400     END-IF                                                               
102500                                                                          
102600     PERFORM GDA-UPD-FRAN-NYCKELRAD                                       
102700                                                                          
102800     MOVE +1 TO RAD-IX                                                    
102900     PERFORM UNTIL RAD-IX > MAX-IX                                        
103000        IF REQU-KDCMDVAL(RAD-IX) = ALL '+'                                
103100          CONTINUE                                                        
103200        ELSE                                                              
103300                                                                          
103400          MOVE REQU-ADBUFFOMR(RAD-IX) TO RESP-ADBUFFOMR(RAD-IX)           
103500          MOVE REQU-ADBUFFGANG(RAD-IX)                                    
103600                           TO RESP-ADBUFFGANG(RAD-IX)                     
103700          MOVE REQU-ADBUFFPL(RAD-IX)                                      
103800                           TO RESP-ADBUFFPL(RAD-IX)                       
103900          INSPECT REQU-DABUFPAF (RAD-IX)                                  
104000                                REPLACING LEADING SPACE BY ZERO           
104100          IF REQU-DABUFPAF(RAD-IX) NOT NUMERIC                            
104200             MOVE ZERO TO REQU-DABUFPAF(RAD-IX)                           
104300          END-IF                                                          
104400          MOVE REQU-DABUFPAF(RAD-IX) TO RESP-DABUFPAF(RAD-IX)             
104500          MOVE REQU-DABUFPAF (RAD-IX)    TO W-DABUFPAF                    
104600                                                                          
104700          INSPECT REQU-ADBUFFOMR (RAD-IX)                                 
104800                                   REPLACING LEADING SPACE BY ZERO        
104900          IF REQU-ADBUFFOMR (RAD-IX) NOT NUMERIC                          
105000            MOVE ZERO TO REQU-ADBUFFOMR (RAD-IX)                          
105100          END-IF                                                          
105200          MOVE REQU-ADBUFFOMR (RAD-IX) TO W-ADBUFFOMR                     
105300          INSPECT REQU-ADBUFFGANG (RAD-IX)                                
105400                                 REPLACING LEADING SPACE BY ZERO          
105500          IF REQU-ADBUFFGANG (RAD-IX) NOT NUMERIC                         
105600            MOVE ZERO TO REQU-ADBUFFGANG (RAD-IX)                         
105700          END-IF                                                          
105800          MOVE REQU-ADBUFFGANG (RAD-IX) TO W-ADBUFFGANG                   
105900          INSPECT REQU-ADBUFFPL (RAD-IX)                                  
106000                                REPLACING LEADING SPACE BY ZERO           
106100          IF REQU-ADBUFFPL(RAD-IX) NOT NUMERIC                            
106200            MOVE ZERO TO REQU-ADBUFFPL (RAD-IX)                           
106300          END-IF                                                          
106400          MOVE REQU-ADBUFFPL (RAD-IX) TO W-ADBUFFPL                       
106500                                                                          
106600          PERFORM IMS-GU-ARTD11                                           
106700          IF SEGMENT-SAKNAS                                               
106800            MOVE 'KDCMDVAL' TO RESP-IDELMT-ERROR                          
106900            MOVE 'CMD'      TO RESP-IDMSG-ERROR-LINE(RAD-IX)              
107000            MOVE NEJ TO INDATA-SW                                         
107100          ELSE                                                            
107200            IF REQU-KDCMDVAL (RAD-IX) = 'OUT' OR 'O  '                    
107300               IF (REQU-KVBUFF-F-IN(RAD-IX) NOT = ALL '+') OR             
107400                  (REQU-KVKOLLI-F-IN(RAD-IX) NOT = ALL '+')               
107500                IF REQU-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                
107600                   MOVE REQU-KVBUFF-F-IN (RAD-IX) TO WS-ANTAL             
107700                   IF WS-ANTAL > ARTD-SALDO-KVBUFF-F                      
107800                     MOVE NEJ TO INDATA-SW                                
107900                     MOVE 'CON' TO RESP-IDMSG-ERROR-LINE(RAD-IX)          
108000                     MOVE CONFLICT-FIELDS  TO RESP-IDMSG-INFO             
108100                   END-IF                                                 
108200                END-IF                                                    
108300                IF REQU-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'               
108400                   MOVE REQU-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL            
108500                   IF WS-ANTAL > ARTD-SALDO-KVKOLLI-F                     
108600                     MOVE NEJ TO INDATA-SW                                
108700                     MOVE 'CON' TO RESP-IDMSG-ERROR-LINE(RAD-IX)          
108800                     MOVE CONFLICT-FIELDS  TO RESP-IDMSG-INFO             
108900                   END-IF                                                 
109000                END-IF                                                    
109100              ELSE                                                        
109200                MOVE WS-KVBUFF TO WS-ANTAL                                
109300                IF WS-ANTAL  > ARTD-SALDO-KVBUFF-F                        
109400                  MOVE NEJ TO INDATA-SW                                   
109500                  MOVE CONFLICT-FIELDS  TO RESP-IDMSG-INFO                
109600                  MOVE 'CON' TO RESP-IDMSG-ERROR-LINE(RAD-IX)             
109700                END-IF                                                    
109800              END-IF                                                      
109900            END-IF                                                        
110000          END-IF                                                          
110100        END-IF                                                            
110200                                                                          
110300        ADD +1 TO RAD-IX                                                  
110400     END-PERFORM                                                          
110500                                                                          
110600     IF SW-REQU-INPUT-RAD = JA                                            
110700       MOVE REQU-ADBUFFOMR-UPD TO W-ADBUFFOMR                             
110800       IF REQU-ADBUFFGANG-UPD NOT = ALL '+'                               
110900         MOVE REQU-ADBUFFGANG-UPD TO W-ADBUFFGANG                         
111000       ELSE                                                               
111100         MOVE ZERO                TO W-ADBUFFGANG                         
111200       END-IF                                                             
111300       IF REQU-ADBUFFPL-UPD NOT = ALL '+'                                 
111400         MOVE REQU-ADBUFFPL-UPD   TO W-ADBUFFPL                           
111500       ELSE                                                               
111600         MOVE ZERO                TO W-ADBUFFPL                           
111700       END-IF                                                             
111800                                                                          
111900       PERFORM IMS-GU-ARTD11                                              
112000       IF SEGMENT-FINNS                                                   
112100         MOVE NEJ TO INDATA-SW                                            
112200         MOVE ALREADY-EXIST TO RESP-IDMSG-INFO                            
112300       ELSE                                                               
112400         IF ((W-ADBUFFOMR > 19 AND W-ADBUFFOMR < 30 AND                   
112500             AKTUELLT-LAND-EJ-KINA AND                                    
                   REQU-IDDC-KEY NOT = 53)  OR                                  
112600             (W-ADBUFFOMR = 99  AND                                       
112700             (REQU-IDDC-KEY = 92 OR 71)))                                 
112800            MOVE W-ADBUFFOMR     TO W-ADBUFFOM-A1KY-MIN                   
112900                                    W-ADBUFFOM-A1KY-MAX                   
113000            MOVE W-ADBUFFGANG    TO W-ADBUFGAN-A1KY-MIN                   
113100                                    W-ADBUFGAN-A1KY-MAX                   
113200            MOVE W-ADBUFFPL      TO W-ADBUFPL-A1KY-MIN                    
113300                                    W-ADBUFPL-A1KY-MAX                    
113400            MOVE SPACE TO RESP-MOD-LEDTEXT                                
113500            PERFORM IMS-GU-WDD8A1                                         
113600            IF SEGMENT-FINNS                                              
113700              MOVE SEQA-IDARTNR TO WS-INFOART                             
113800              MOVE WS-INFO TO RESP-MOD-LEDTEXT                            
113900                                                                          
114000              MOVE NEJ TO INDATA-SW                                       
114100*             MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                         
                    STRING 'PARTNO*' WS-INFOART delimited by size               
                            INTO RESP-IDELMT-ERROR                              
114200*             MOVE ALREADY-EXIST TO RESP-IDMSG-INFO                       
114200              MOVE PART-NO-IN-SAME-BUFFER-LOC TO RESP-IDMSG-ERROR         
114300            END-IF                                                        
114400          END-IF                                                          
114500        END-IF                                                            
114600     END-IF                                                               
114700                                                                          
114800     IF UPD-FRAN-NYCKELRAD                                                
114900       MOVE SPACE             TO RESP-MOD-LEDTEXT                         
115000       MOVE WS-ADBUFFOMR      TO W-ADBUFFOMR                              
115100       MOVE WS-ADBUFFGANG     TO W-ADBUFFGANG                             
115200       MOVE WS-ADBUFFPL       TO W-ADBUFFPL                               
115300       PERFORM IMS-GU-ARTD11                                              
115400       IF SEGMENT-FINNS                                                   
115500         MOVE NEJ TO INDATA-SW                                            
115600         MOVE ALREADY-EXIST TO RESP-IDMSG-INFO                            
115700       ELSE                                                               
115800         IF ((W-ADBUFFOMR > 19 AND W-ADBUFFOMR < 30 AND                   
115900             AKTUELLT-LAND-EJ-KINA AND                                    
                   REQU-IDDC-KEY NOT = 53)  OR                                  
116000             (W-ADBUFFOMR = 99  AND                                       
116100             (REQU-IDDC-KEY = 92 OR 71)))                                 
116200            MOVE W-ADBUFFOMR     TO W-ADBUFFOM-A1KY-MIN                   
116300                                    W-ADBUFFOM-A1KY-MAX                   
116400            MOVE W-ADBUFFGANG    TO W-ADBUFGAN-A1KY-MIN                   
116500                                    W-ADBUFGAN-A1KY-MAX                   
116600            MOVE W-ADBUFFPL      TO W-ADBUFPL-A1KY-MIN                    
116700                                    W-ADBUFPL-A1KY-MAX                    
116800            PERFORM IMS-GU-WDD8A1                                         
116900            IF SEGMENT-FINNS                                              
117000              MOVE SEQA-IDARTNR TO WS-INFOART                             
117100              MOVE WS-INFO TO RESP-MOD-LEDTEXT                            
117200                                                                          
117300              MOVE NEJ TO INDATA-SW                                       
117400              MOVE ALREADY-EXIST TO RESP-IDMSG-INFO                       
117500              MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                         
117600           END-IF                                                         
117700        END-IF                                                            
117800       END-IF                                                             
117900     END-IF                                                               
118000                                                                          
118100     .                                                                    
118200     EJECT                                                                
118300 GDA-UPD-FRAN-NYCKELRAD SECTION.                                          
118400                                                                          
118500     IF SW-REQU-INPUT-RAD = NEJ                                           
118600       MOVE +1 TO RAD-IX                                                  
118700       MOVE NEJ TO INDATA-FINNS-SW                                        
118800       PERFORM UNTIL RAD-IX > MAX-IX OR INDATA-FINNS                      
118900          IF REQU-KDCMDVAL(RAD-IX) = ALL '+' AND                          
119000          (REQU-KVBUFF-F-IN(RAD-IX) = ALL '+') AND                        
119100          (REQU-KVKOLLI-F-IN(RAD-IX) = ALL '+')                           
119200             CONTINUE                                                     
119300          ELSE                                                            
119400             MOVE JA TO INDATA-FINNS-SW                                   
119500          END-IF                                                          
119600          ADD +1 TO RAD-IX                                                
119700       END-PERFORM                                                        
119800       IF INDATA-SAKNAS                                                   
119900          MOVE JA TO UPD-FRAN-NYCKELRAD-SW                                
120000       END-IF                                                             
120100     END-IF                                                               
120200     .                                                                    
120300     EJECT                                                                
120400 H-UPPDATERA SECTION.                                                     
120500                                                                          
120600     PERFORM IMS-GU-ARTD01                                                
120700     IF SEGMENT-SAKNAS                                                    
120800       MOVE W-IDARTNR      TO ARTD-ART-IDARTNR                            
120900       PERFORM IMS-ISRT-ARTD01                                            
121000     END-IF                                                               
121100     IF SW-REQU-INPUT-RAD = NEJ                                           
121200       MOVE +1 TO RAD-IX                                                  
121300       PERFORM UNTIL RAD-IX > MAX-IX                                      
121400         IF REQU-KDCMDVAL (RAD-IX) = ALL '+'                              
121500           CONTINUE                                                       
121600         ELSE                                                             
121700           INSPECT REQU-ADBUFFOMR (RAD-IX)                                
121800                                 REPLACING LEADING SPACE BY ZERO          
121900           MOVE REQU-ADBUFFOMR (RAD-IX) TO W-ADBUFFOMR                    
122000                                          W-ADBUFFOMR-MIN                 
122100           INSPECT REQU-ADBUFFGANG (RAD-IX)                               
122200                                  REPLACING LEADING SPACE BY ZERO         
122300           MOVE REQU-ADBUFFGANG (RAD-IX) TO W-ADBUFFGANG                  
122400                                           W-ADBUFFGANG-MIN               
122500           INSPECT REQU-ADBUFFPL  (RAD-IX)                                
122600                                  REPLACING LEADING SPACE BY ZERO         
122700           MOVE REQU-ADBUFFPL  (RAD-IX) TO W-ADBUFFPL                     
122800                                           W-ADBUFFPL-MIN                 
122900           INSPECT REQU-DABUFPAF  (RAD-IX)                                
123000                                  REPLACING LEADING SPACE BY ZERO         
123100           MOVE REQU-DABUFPAF  (RAD-IX) TO W-DABUFPAF                     
123200                                           W-DABUFPAF-MIN                 
123300                                                                          
123400           PERFORM IMS-GHU-ARTD11                                         
123500           IF REQU-KDCMDVAL (RAD-IX) = 'D  '                              
123600             PERFORM IMS-DLET-ARTD                                        
123700******       SLÄCKNING AV BUFFERTPLATS (WDJ9)                             
123800             PERFORM HB-STANG-BUFFERTPLATS-WDJ9                           
123900           ELSE                                                           
124000             IF REQU-KDCMDVAL (RAD-IX) = 'IN ' OR 'I  '                   
124100                IF (REQU-KVBUFF-F-IN(RAD-IX) NOT = ALL '+') OR            
124200                 (REQU-KVKOLLI-F-IN(RAD-IX) NOT = ALL '+')                
124300                 IF REQU-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'               
124400                   MOVE REQU-KVBUFF-F-IN (RAD-IX) TO WS-ANTAL             
124500                   ADD WS-ANTAL TO ARTD-SALDO-KVBUFF-F                    
124600                 END-IF                                                   
124700                 IF REQU-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'              
124800                   MOVE REQU-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL            
124900                   ADD WS-ANTAL TO ARTD-SALDO-KVKOLLI-F                   
125000                 END-IF                                                   
125100               ELSE                                                       
125200                 MOVE WS-KVBUFF TO WS-ANTAL                               
125300                 ADD WS-ANTAL   TO ARTD-SALDO-KVBUFF-F                    
125400               END-IF                                                     
125500               PERFORM IMS-REPL-ARTD                                      
125600             ELSE                                                         
125700              IF REQU-KDCMDVAL (RAD-IX) = 'OUT' OR 'O  '                  
125800               IF (REQU-KVBUFF-F-IN(RAD-IX) NOT = ALL '+') OR             
125900                 (REQU-KVKOLLI-F-IN(RAD-IX) NOT = ALL '+')                
126000                 IF REQU-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'               
126100                   MOVE REQU-KVBUFF-F-IN (RAD-IX) TO WS-ANTAL             
126200                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVBUFF-F             
126300                 END-IF                                                   
126400                 IF REQU-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'              
126500                   MOVE REQU-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL            
126600                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVKOLLI-F            
126700                 END-IF                                                   
126800               ELSE                                                       
126900                 MOVE WS-KVBUFF    TO WS-ANTAL                            
127000                 SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVBUFF-F               
127100               END-IF                                                     
127200                                                                          
127300               IF ARTD-SALDO-DABUFPAF  > ZERO  AND                        
127400                  (AKTUELLT-LAND-EJ-KINA)      AND                        
127500                  ARTD-SALDO-KVBUFF-F  = ZERO  AND                        
127600                  ARTD-SALDO-KVBUFF-OF = ZERO  AND                        
127700                  ARTD-SALDO-KVKOLLI-F = ZERO  AND                        
127800                  ARTD-SALDO-KVKOLLI-OF = ZERO                            
127900                  PERFORM IMS-DLET-ARTD                                   
128000                  PERFORM HB-STANG-BUFFERTPLATS-WDJ9                      
128100               ELSE                                                       
128200                 PERFORM IMS-REPL-ARTD                                    
128300               END-IF                                                     
128400              END-IF                                                      
128500** VALIDATE IF FROM API - CMD = 'ACT'                                     
128600             IF REQU-KDCMDVAL (RAD-IX) = 'ACT'                            
128700                IF (REQU-KVBUFF-F-IN(RAD-IX) NOT = ALL '+') OR            
128800                 (REQU-KVKOLLI-F-IN(RAD-IX) NOT = ALL '+')                
128900                 IF REQU-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'               
129000                   MOVE REQU-KVBUFF-F-IN (RAD-IX) TO                      
129100                        ARTD-SALDO-KVBUFF-F                               
129200                 END-IF                                                   
129300                 IF REQU-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'              
129400                   MOVE REQU-KVKOLLI-F-IN (RAD-IX) TO                     
129500                        ARTD-SALDO-KVKOLLI-F                              
129600                 END-IF                                                   
129700               END-IF                                                     
129800               IF ARTD-SALDO-KVBUFF-F  = 0 AND                            
129900                  ARTD-SALDO-KVKOLLI-F = 0                                
130000                 PERFORM IMS-DLET-ARTD                                    
                       PERFORM HB-STANG-BUFFERTPLATS-WDJ9                       
130100               ELSE                                                       
130200                 PERFORM IMS-REPL-ARTD                                    
130300               END-IF                                                     
130400             END-IF                                                       
130500** VALIDATE IF FROM API - CMD = 'ACT'  -- END!!                           
130600                                                                          
130700             END-IF                                                       
130800           END-IF                                                         
130900         END-IF                                                           
131000         ADD +1 TO RAD-IX                                                 
131100       END-PERFORM                                                        
131200                                                                          
131300     ELSE                                                                 
131400       MOVE REQU-ADBUFFOMR-UPD TO ARTD-SALDO-ADBUFFOMR                    
131500       IF REQU-ADBUFFGANG-UPD NOT = ALL '+'                               
131600         MOVE REQU-ADBUFFGANG-UPD TO ARTD-SALDO-ADBUFFGANG                
131700       ELSE                                                               
131800         MOVE ZERO               TO ARTD-SALDO-ADBUFFGANG                 
131900       END-IF                                                             
132000       IF REQU-ADBUFFPL-UPD NOT = ALL '+'                                 
132100         MOVE REQU-ADBUFFPL-UPD TO ARTD-SALDO-ADBUFFPL                    
132200       ELSE                                                               
132300         MOVE ZERO             TO ARTD-SALDO-ADBUFFPL                     
132400       END-IF                                                             
132500       IF REQU-KVBUFF-F-UPD NOT = ALL '+'                                 
132600         MOVE REQU-KVBUFF-F-UPD TO ARTD-SALDO-KVBUFF-F                    
132700       ELSE                                                               
132800         MOVE ZERO             TO ARTD-SALDO-KVBUFF-F                     
132900       END-IF                                                             
133000                                                                          
133100                                                                          
133200       IF REQU-KVKOLLI-F-UPD NOT = ALL '+'                                
133300         MOVE REQU-KVKOLLI-F-UPD TO ARTD-SALDO-KVKOLLI-F                  
133400       ELSE                                                               
133500         MOVE ZERO              TO ARTD-SALDO-KVKOLLI-F                   
133600       END-IF                                                             
133700                                                                          
133800       MOVE ZERO                TO ARTD-SALDO-KVKOLLI-OF                  
133900                                   ARTD-SALDO-KDPAF                       
134000                                   ARTD-SALDO-KDBRIST                     
134100                                   ARTD-SALDO-KVBUFF-OF                   
134200                                                                          
134300       MOVE REQU-IDDC-KEY TO ARTD-SALDO-IDDC                              
134400       IF ARTD-SALDO-ADBUFFOMR > 19 AND                                   
134500          ARTD-SALDO-ADBUFFOMR < 30 AND                                   
134600          (AKTUELLT-LAND-EJ-KINA AND REQU-IDDC-KEY NOT = 92)              
134700                                                                          
134800           MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF        
134900       ELSE                                                               
135000         IF ARTD-SALDO-ADBUFFOMR = 99 AND                                 
135100            (REQU-IDDC-KEY = 71 OR 92)                                    
135200           MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF        
135300         ELSE                                                             
135400           MOVE ZERO                        TO ARTD-SALDO-DABUFPAF        
135500         END-IF                                                           
135600       END-IF                                                             
135700       PERFORM IMS-ISRT-ARTD11                                            
135800       PERFORM HA-UPPDATERA-WDJ9                                          
135900     END-IF                                                               
136000                                                                          
136100     IF UPD-FRAN-NYCKELRAD                                                
136200       MOVE WS-ADBUFFOMR    TO ARTD-SALDO-ADBUFFOMR                       
136300       MOVE WS-ADBUFFGANG   TO ARTD-SALDO-ADBUFFGANG                      
136400       MOVE WS-ADBUFFPL     TO ARTD-SALDO-ADBUFFPL                        
136500       MOVE WS-KVBUFF       TO ARTD-SALDO-KVBUFF-F                        
136600       IF WS-ADBUFFOMR > 19 AND WS-ADBUFFOMR < 30 AND                     
136700          (AKTUELLT-LAND-EJ-KINA AND REQU-IDDC-KEY NOT = 92)              
136800        MOVE +1         TO ARTD-SALDO-KVKOLLI-F                           
136900        MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF           
137000       ELSE                                                               
137100         IF ARTD-SALDO-ADBUFFOMR = 99 AND                                 
137200            (REQU-IDDC-KEY = 71 OR 92)                                    
137300           MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF        
137400         ELSE                                                             
137500           MOVE ZERO                        TO ARTD-SALDO-DABUFPAF        
137600                                             ARTD-SALDO-KVKOLLI-F         
137700         END-IF                                                           
137800       END-IF                                                             
137900                                                                          
138000       MOVE ZERO            TO ARTD-SALDO-KVBUFF-OF                       
138100                               ARTD-SALDO-KVKOLLI-OF                      
138200                               ARTD-SALDO-KDPAF                           
138300                               ARTD-SALDO-KDBRIST                         
138400       MOVE REQU-IDDC-KEY   TO ARTD-SALDO-IDDC                            
138500       PERFORM IMS-ISRT-ARTD11                                            
138600       PERFORM HA-UPPDATERA-WDJ9                                          
138700     END-IF                                                               
138800                                                                          
138900     MOVE UPDATE-DONE TO RESP-IDMSG-INFO                                  
139000     .                                                                    
139100     EJECT                                                                
139200                                                                          
139300 HA-UPPDATERA-WDJ9 SECTION.                                               
139400                                                                          
139500     PERFORM IMS-GU-LOCB01                                                
139600     IF SEGMENT-SAKNAS                                                    
139700       MOVE W-IDARTNR TO LOCB-ART-IDARTNR                                 
139800       PERFORM IMS-ISRT-LOCB01                                            
139900       PERFORM IMS-GU-LOCB01                                              
140000     END-IF                                                               
140100     IF SEGMENT-FINNS                                                     
140200       MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM                     
140300       MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                       
140400       COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                     
140500                                                     LOGG-DATUM           
140600       COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID              
140700       MOVE REQU-IDDC-KEY        TO LOCB-HIST-IDDC                        
140800       IF UPD-FRAN-NYCKELRAD                                              
140900         MOVE WS-ADBUFFOMR       TO LOCB-HIST-ADLAGOMR                    
141000         IF WS-ADBUFFGANG NOT = ALL '+'                                   
141100           MOVE WS-ADBUFFGANG      TO LOCB-HIST-ADGANG                    
141200         ELSE                                                             
141300           MOVE ZERO               TO LOCB-HIST-ADGANG                    
141400         END-IF                                                           
141500         IF WS-ADBUFFPL NOT = ALL '+'                                     
141600           MOVE WS-ADBUFFPL        TO LOCB-HIST-ADPLATS                   
141700         ELSE                                                             
141800           MOVE ZERO               TO LOCB-HIST-ADPLATS                   
141900         END-IF                                                           
142000       ELSE                                                               
142100         MOVE REQU-ADBUFFOMR-UPD TO LOCB-HIST-ADLAGOMR                    
142200         IF REQU-ADBUFFGANG-UPD NOT = ALL '+'                             
142300           MOVE REQU-ADBUFFGANG-UPD TO LOCB-HIST-ADGANG                   
142400         ELSE                                                             
142500           MOVE ZERO               TO LOCB-HIST-ADGANG                    
142600         END-IF                                                           
142700         IF REQU-ADBUFFPL-UPD NOT = ALL '+'                               
142800           MOVE REQU-ADBUFFPL-UPD  TO LOCB-HIST-ADPLATS                   
142900         ELSE                                                             
143000           MOVE ZERO               TO LOCB-HIST-ADPLATS                   
143100         END-IF                                                           
143200       END-IF                                                             
143300       MOVE BUFFER-LOCATION      TO LOCB-HIST-KDLOC                       
143400       MOVE REQU-IDUSER          TO LOCB-HIST-IDUSER                      
143500       MOVE SPACE                TO LOCB-HIST-IDUSER-STO                  
143600       MOVE ZERO                 TO LOCB-HIST-DASTODAT                    
143700                                                                          
143800       PERFORM IMS-ISRT-LOCB11                                            
143900     END-IF                                                               
144000     .                                                                    
144100     EJECT                                                                
144200 HB-STANG-BUFFERTPLATS-WDJ9 SECTION.                                      
144300                                                                          
144400     PERFORM IMS-GU-LOCB01                                                
144500     IF SEGMENT-SAKNAS                                                    
144600       CONTINUE                                                           
144700     ELSE                                                                 
144800       PERFORM IMS-GHNP-LOCB11                                            
144900       IF SEGMENT-FINNS                                                   
145000         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
145100           (REQU-ADBUFFOMR (RAD-IX) = LOCB-HIST-ADLAGOMR)                 
145200                                                     AND                  
145300           (REQU-ADBUFFGANG (RAD-IX) = LOCB-HIST-ADGANG)                  
145400                                                     AND                  
145500           (REQU-ADBUFFPL (RAD-IX)  = LOCB-HIST-ADPLATS)                  
145600                                                     AND                  
145700           (LOCB-HIST-KDLOC = 'B')                                        
145800           PERFORM IMS-GHNP-LOCB11                                        
145900         END-PERFORM                                                      
146000         IF SEGMENT-FINNS AND                                             
146100           (REQU-ADBUFFOMR (RAD-IX) = LOCB-HIST-ADLAGOMR)                 
146200                                                     AND                  
146300           (REQU-ADBUFFGANG (RAD-IX) = LOCB-HIST-ADGANG)                  
146400                                                     AND                  
146500           (REQU-ADBUFFPL (RAD-IX)  = LOCB-HIST-ADPLATS)                  
146600                                                     AND                  
146700           (LOCB-HIST-KDLOC = 'B')                                        
146800           MOVE FUNCTION CURRENT-DATE(1:8) TO                             
146900                                    LOCB-HIST-DASTODAT                    
147000           MOVE REQU-IDUSER TO LOCB-HIST-IDUSER-STO                       
147100           PERFORM IMS-REPL-LOCB11                                        
147200         END-IF                                                           
147300       END-IF                                                             
147400     END-IF                                                               
147500     .                                                                    
147600     EJECT                                                                
147700 S01-HAEMTA-ANROPSDATA SECTION.                                           
147800                                                                          
147900     MOVE 'GETARG'               TO SUB-KDFUNC                            
148000     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
148100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
148200                                                                          
148300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
148400                                                                          
148500     IF SUB-KDRC > 0                                                      
148600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
148700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
148800       DELIMITED BY SIZE INTO FELTEXT                                     
148900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
149000     END-IF                                                               
149100     .                                                                    
149200     SKIP3                                                                
149300 S02-RETURNERA-SVAR SECTION.                                              
149400                                                                          
149500     COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA                           
149600       - LENGTH OF RESP-RAD * (500 - WS-KVANT)                            
149700                                                                          
149800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
149900     MOVE WS-RESP-AREA               TO SUB-KVDLEN                        
150000                                                                          
150100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
150200                                                                          
150300     IF SUB-KDRC > 0                                                      
150400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
150500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
150600       DELIMITED BY SIZE INTO FELTEXT                                     
150700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
150800     END-IF                                                               
150900     .                                                                    
151000     EJECT                                                                
151100 S11-MSG-CONV SECTION.                                                    
151200     MOVE SPACES                  TO RESP-MESSAGES (1)                    
151300                                     RESP-MESSAGES (2)                    
151400     MOVE 1                       TO MSG-IX                               
151500*    REQUEST OK                                                           
151600     MOVE 200                     TO RESP-KDSTATUS-API                    
151700     IF RESP-IDMSG-INFO > SPACE                                           
151800       MOVE SPACES                TO MSG-CONV-AREA                        
151900       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
152000       CALL WMSGCONV           USING MSG-CONV-AREA                        
152100       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
152200       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
152300       ADD 1                      TO MSG-IX                               
152400     END-IF                                                               
152500     IF RESP-IDMSG-ERROR > SPACE                                          
152600*      BAD REQUEST                                                        
152700       MOVE 400                   TO RESP-KDSTATUS-API                    
152800       MOVE SPACES                TO MSG-CONV-AREA                        
152900       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
153000       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
153100       CALL WMSGCONV           USING MSG-CONV-AREA                        
153200       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
153300       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
153400     END-IF                                                               
153500     .                                                                    
153600     EJECT                                                                
153700                                                                          
153800 IMS-GU-ARTC11   SECTION.                                                 
153900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
154000          DELIMITED BY SIZE INTO SSA1                                     
154100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
154200          DELIMITED BY SIZE INTO SSA2                                     
154300     MOVE '  GE' TO GODK-STATUSKODER                                      
154400     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA SSA1 SSA2                
154500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     .                                                                    
154800     SKIP3                                                                
154900 IMS-GU-ARTS11   SECTION.                                                 
155000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
155100          DELIMITED BY SIZE INTO SSA1                                     
155200     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
155300          DELIMITED BY SIZE INTO SSA2                                     
155400     MOVE '  GE' TO GODK-STATUSKODER                                      
155500     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-AREA SSA1 SSA2                
155600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
155700     PERFORM IMS-STATUSKONTROLL                                           
155800     .                                                                    
155900     SKIP3                                                                
156000 IMS-GU-ARTD01    SECTION.                                                
156100     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
156200          DELIMITED BY SIZE INTO SSA1                                     
156300     MOVE '  GE' TO GODK-STATUSKODER                                      
156400     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1                     
156500     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
156600     PERFORM IMS-STATUSKONTROLL                                           
156700     .                                                                    
156800     SKIP3                                                                
156900 IMS-GNP-ARTD11     SECTION.                                              
157000     STRING 'WLARTD11(WDD811KY=>' W-WDD811KY-MIN-X                        
157100                    '&WDD811KY=<' W-WDD811KY-MAX-X ')'                    
157200          DELIMITED BY SIZE INTO SSA1                                     
157300     MOVE '  GE' TO GODK-STATUSKODER                                      
157400     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
157500     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
157600     PERFORM IMS-STATUSKONTROLL                                           
157700     .                                                                    
157800     SKIP3                                                                
157900 IMS-GNP-02-ARTD11     SECTION.                                           
158000     STRING 'WLARTD11(WDD811KY=>' W-WDD811KY-MIN-X                        
158100                    '&WDD811KY=<' W-WDD811KY-MAX-X                        
158200                    '&ADBUFGAN= ' W-ADBUFFGANG-X                          
158300                    '&ADBUFPL = ' W-ADBUFFPL-X     ')'                    
158400          DELIMITED BY SIZE INTO SSA1                                     
158500     MOVE '  GE' TO GODK-STATUSKODER                                      
158600     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
158700     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
158800     PERFORM IMS-STATUSKONTROLL                                           
158900     .                                                                    
159000     SKIP3                                                                
159100 IMS-GU-ARTD11     SECTION.                                               
159200     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
159300          DELIMITED BY SIZE INTO SSA1                                     
159400     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
159500          DELIMITED BY SIZE INTO SSA2                                     
159600     MOVE '  GE' TO GODK-STATUSKODER                                      
159700     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA SSA1 SSA2                 
159800     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
159900     PERFORM IMS-STATUSKONTROLL                                           
160000     .                                                                    
160100     EJECT                                                                
160200 IMS-GHU-ARTD11     SECTION.                                              
160300     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
160400          DELIMITED BY SIZE INTO SSA1                                     
160500     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
160600          DELIMITED BY SIZE INTO SSA2                                     
160700     MOVE '  ' TO GODK-STATUSKODER                                        
160800     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1 SSA2                
160900     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200     SKIP3                                                                
161300 IMS-ISRT-ARTD01 SECTION.                                                 
161400     MOVE 'WLARTD01 ' TO SSA1                                             
161500     MOVE '  II' TO GODK-STATUSKODER                                      
161600     CALL CBLTDLI USING ISRT ARTD-PCB DLI-IO-AREA SSA1                    
161700     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
161800     PERFORM IMS-STATUSKONTROLL                                           
161900     .                                                                    
162000     EJECT                                                                
162100 IMS-ISRT-ARTD11 SECTION.                                                 
162200     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
162300          DELIMITED BY SIZE INTO SSA1                                     
162400     MOVE 'WLARTD11 ' TO SSA2                                             
162500     MOVE '  II' TO GODK-STATUSKODER                                      
162600     CALL CBLTDLI USING ISRT ARTD-PCB DLI-IO-AREA SSA1 SSA2               
162700     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
162800     PERFORM IMS-STATUSKONTROLL                                           
162900     .                                                                    
163000     SKIP3                                                                
163100 IMS-REPL-ARTD SECTION.                                                   
163200     MOVE '  ' TO GODK-STATUSKODER                                        
163300     CALL CBLTDLI USING REPL ARTD-PCB DLI-IO-AREA                         
163400     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
163500     PERFORM IMS-STATUSKONTROLL                                           
163600     .                                                                    
163700     SKIP3                                                                
163800 IMS-DLET-ARTD SECTION.                                                   
163900     MOVE '  ' TO GODK-STATUSKODER                                        
164000     CALL CBLTDLI USING DLET ARTD-PCB DLI-IO-AREA                         
164100     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
164200     PERFORM IMS-STATUSKONTROLL                                           
164300     .                                                                    
164400     EJECT                                                                
164500 IMS-GU-WDD8A1 SECTION.                                                   
164600     STRING 'WDD8A1  (WDD8A1KY >' W-WDD8A1KY-MIN-X                        
164700                    '&WDD8A1KY <' W-WDD8A1KY-MAX-X ')'                    
164800            DELIMITED BY SIZE INTO SSA1                                   
164900     MOVE '  GE' TO GODK-STATUSKODER                                      
165000     CALL CBLTDLI USING GU WDD8A-PCB DLI-IO-AREA3 SSA1                    
165100     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
165200     PERFORM IMS-STATUSKONTROLL                                           
165300     .                                                                    
165400     EJECT                                                                
165500 IMS-GU-BENA11    SECTION.                                                
165600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
165700          DELIMITED BY SIZE INTO SSA1                                     
165800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
165900          DELIMITED BY SIZE INTO SSA2                                     
166000     MOVE '  GE' TO GODK-STATUSKODER                                      
166100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
166200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
166300     PERFORM IMS-STATUSKONTROLL                                           
166400     .                                                                    
166500     SKIP3                                                                
166600 IMS-GU-LOCB01 SECTION.                                                   
166700     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
166800          DELIMITED BY SIZE INTO SSA1                                     
166900     MOVE '  GE' TO GODK-STATUSKODER                                      
167000     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-AREA SSA1                      
167100     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
167200     PERFORM IMS-STATUSKONTROLL                                           
167300     .                                                                    
167400     SKIP3                                                                
167500 IMS-ISRT-LOCB01 SECTION.                                                 
167600     MOVE 'WLLOCB01 ' TO SSA1                                             
167700     MOVE '  ' TO GODK-STATUSKODER                                        
167800     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA SSA1                    
167900     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
168000     PERFORM IMS-STATUSKONTROLL                                           
168100     .                                                                    
168200     EJECT                                                                
168300 IMS-GHNP-LOCB11 SECTION.                                                 
168400     STRING 'WLLOCB11(IDDC     =' W-IDDC-X ')'                            
168500             DELIMITED BY SIZE INTO SSA1                                  
168600     MOVE '  GE' TO GODK-STATUSKODER                                      
168700     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-AREA SSA1                    
168800     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
168900     PERFORM IMS-STATUSKONTROLL                                           
169000     .                                                                    
169100     SKIP3                                                                
169200 IMS-REPL-LOCB11 SECTION.                                                 
169300     MOVE '  ' TO GODK-STATUSKODER                                        
169400     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-AREA                         
169500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
169600     PERFORM IMS-STATUSKONTROLL                                           
169700     .                                                                    
169800     SKIP3                                                                
169900 IMS-ISRT-LOCB11 SECTION.                                                 
170000     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
170100          DELIMITED BY SIZE INTO SSA1                                     
170200     MOVE 'WLLOCB11 ' TO SSA2                                             
170300     MOVE '  II' TO GODK-STATUSKODER                                      
170400     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA SSA1 SSA2               
170500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
170600     PERFORM IMS-STATUSKONTROLL                                           
170700     .                                                                    
170800 IMS-GU-WDB601    SECTION.                                                
170900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
171000          DELIMITED BY SIZE INTO SSA1                                     
171100     MOVE '  GE' TO GODK-STATUSKODER                                      
171200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
171300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
171400     PERFORM IMS-STATUSKONTROLL                                           
171500     IF SEGMENT-SAKNAS                                                    
171600         MOVE SPACE TO DCS-KDDC                                           
171700                       DCS-IDLANDX2                                       
171800     END-IF                                                               
171900     .                                                                    
172000     EJECT                                                                
172100 IMS-STATUSKONTROLL SECTION.                                              
172200                                                                          
172300     SET STATUS-IX TO 1                                                   
172400     SEARCH GODK-STATUS                                                   
172500       AT END                                                             
172600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
172700         DELIMITED BY SIZE INTO FELTEXT                                   
172800         CALL FELLOG                                                      
172900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
173000         CONTINUE                                                         
173100     END-SEARCH                                                           
173200     .                                                                    
