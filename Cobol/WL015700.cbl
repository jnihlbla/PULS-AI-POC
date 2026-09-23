000100 ID DIVISION.                                                             
000110                                                                          
000200 PROGRAM-ID.     WL015700.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/09/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.CONFIRMCASES'                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SKAPAR OCH FYLLER PÅ KOLLIN MED RETURTILLSTÅND HOS               
001100*        RETUR-TERMINALER.                                                
001200*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001300*        PROGRAMMET LÄSER      WLRETC (WDA3)                              
001400*        PROGRAMMET UPPDATERAR WL4111 (WDR1)                              
001500*        OBS SKULLE PROGRAMMET BLI DYRT ELLER SEGT. SKAPA NYTT            
001600*        INDEX MED IDKOLLI SOM INGÅNG OCH LÄS DETTA INDEX ISTÄLLET        
001700*        FÖR WLRETC01.                                                    
001800*                                                                         
001900*        WL015700 PROGRAM IS A REPLICA OF W4074200 PROGRAM                
002000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002100*                                                                         
002200*    E'TRACKER: 4230251  2007-02                                          
002300*    2012-02-21 E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1              
002310*                                                                         
002320*                                                                         
002400*    INDATA.                                                              
002500*        TRANSACTION: WL0157U                                             
002600*        REQUEST:     WL0157I1                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        RESPONSE:    WL0157O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'WL015700'.            
004400 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
004500 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004600                                                                          
004700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000                                                                          
005100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  YES                         PIC X       VALUE 'Y'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  W-DELETE                    PIC X(3)    VALUE 'DEL'.                 
005800 77  W-PACKA                     PIC X(3)    VALUE 'PAC'.                 
005900                                                                          
006000 01  W-ANTAL-KOLLI               PIC S9(3)   VALUE +0 COMP-3.             
006100 01  W-ANTAL-RADER-RET1          PIC S9(3)   VALUE +0 COMP-3.             
006200 01  W-ANTAL-RADER-RET2          PIC S9(3)   VALUE +0 COMP-3.             
006300                                                                          
006400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800                                                                          
006900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007000     88  INDATA-OK                           VALUE 'J'.                   
007100     88  INDATA-FEL                          VALUE 'N'.                   
007200                                                                          
007300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007400     88  NYCKLAR-OK                          VALUE 'J'.                   
007500     88  NYCKLAR-FEL                         VALUE 'N'.                   
007600                                                                          
007700 77  WS-IDELMT-ERROR             PIC X(16).                               
007800 77  WS-IDMSG-ERROR              PIC X(03).                               
007900 77  WS-IDMSG-INFO               PIC X(03).                               
008000 77  WS-COUNT                    PIC 9(3)   VALUE ZERO.                   
008100 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
008200     88  REC-LIMIT                           VALUE 'J'.                   
008300                                                                          
008400 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
008500 77   W-UPDATE-SW                PIC X       VALUE 'N'.                   
008600     88  W-UPDATE-OK                         VALUE 'J'.                   
008700     EJECT                                                                
008800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008900 01  GENERAL-SUBPROGRAMS.                                                 
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     SKIP3                                                                
009500*    --- PARAMETERS TO ABEND                                              
009600                                                                          
009700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010000     EJECT                                                                
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010300     SKIP3                                                                
010400*01  -COPY WZ01SUB                                                        
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010700     SKIP3                                                                
010800 01  REQU-AREA.                                                           
010900*    03  -COPY WZ01REQU                                                   
011000*    03  -COPY WL0157I1                                                   
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011300     SKIP3                                                                
011400 01  RESP-AREA.                                                           
011500*    03  -COPY WZ01RESP                                                   
011600*    03  -COPY WL0157O1                                                   
011700     EJECT                                                                
011800 01  MESSAGE-CODES.                                                       
011900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012000     03  ERR-OTILL-UPD           PIC X(3)    VALUE '007'.                 
012100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
012200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
012300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
012400     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '169'.                 
012500     03  ERR-INFO-SAKNAS         PIC X(3)    VALUE '185'.                 
012600     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
012700     03  TOO-MANY-LINES          PIC X(3)    VALUE '028'.                 
012800     EJECT                                                                
012900 77  SW-VISA-KOLLI               PIC X       VALUE 'N'.                   
013000     88  VISA-KOLLI                          VALUE 'J'.                   
013100                                                                          
013200 77  SW-SKAPA-NYTT-KOLLI         PIC X       VALUE 'N'.                   
013300     88  SKAPA-NYTT-KOLLI                    VALUE 'J'.                   
013400                                                                          
013500 77  SW-SKAPA-FLERA-KOLLI        PIC X       VALUE 'N'.                   
013600     88  SKAPA-FLERA-KOLLI                   VALUE 'J'.                   
013700                                                                          
013800 77  SW-PACKA-I-FLERA-KOLLI      PIC X       VALUE 'N'.                   
013900     88  PACKA-I-FLERA-KOLLI                 VALUE 'J'.                   
014000                                                                          
014100 77  SW-RAD-CMD                  PIC X       VALUE 'N'.                   
014200     88  RAD-CMD                             VALUE 'J'.                   
014300                                                                          
014400 77  SW-INM-RAD                  PIC X       VALUE 'N'.                   
014500     88  INM-RAD                             VALUE 'J'.                   
014600                                                                          
014700 77  SW-KOLLI                    PIC X       VALUE 'N'.                   
014800     88  KOLLI-FINNS                         VALUE 'J'.                   
014900                                                                          
015000 77  SW-FORSTA-VALDA-RAD         PIC X       VALUE 'N'.                   
015100     88  FORSTA-VALDA-RAD                    VALUE 'J'.                   
015200                                                                          
015300 77  W-FLVISA                    PIC X       VALUE 'N'.                   
015400 77  W-IDPERSON                  PIC S9(3)   VALUE ZERO COMP-3.           
015500 77  W-KDARBTYP                  PIC X(8)    VALUE SPACE.                 
015600 77  W-KLI-PACKAT                PIC S9(1)   VALUE +1   COMP-3.           
015700 77  W-IDKOLLI-NUM               PIC  9(5)   VALUE ZERO.                  
015800 77  W-IDKOLLI-FOM               PIC  9(5)   VALUE ZERO.                  
015900 77  W-IDKOLLI-TOM               PIC  9(5)   VALUE ZERO.                  
016000 77  W-KVKOLLI                   PIC  9(4)   VALUE ZERO.                  
016100 77  W-SPAR-FLFARLIG             PIC X       VALUE SPACE.                 
016200 77  W-SPAR-IDDC-RET             PIC X(2)    VALUE SPACE.                 
016300     EJECT                                                                
016400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016500*                                                                         
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800                                                                          
016900 01  NYCKLAR-TILL-DLI.                                                    
017000     03  W-WDA301KY-X.                                                    
017100         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
017200         05  W-DAREGDAT          PIC  9(8)          VALUE ZERO.           
017300         05  W-TIKLOCK           PIC S9(9)   COMP-3 VALUE ZERO.           
017400                                                                          
017500     03  W-WDA3ASEQ-MIN-X.                                                
017600         05  W-IDRT-ASEQ-MIN     PIC  X(3)   VALUE SPACE.                 
017700         05  W-IDDC-ASEQ-MIN     PIC  X(2)   VALUE SPACE.                 
017800         05  W-IDDISTR-ASEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
017900         05  W-IDKUNDNR-ASEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
018000         05  W-IDRAPPNR-ASEQ-MIN PIC  9(7)   VALUE ZERO.                  
018100                                                                          
018200     03  W-WDA3ASEQ-MAX-X.                                                
018300         05  W-IDRT-ASEQ-MAX     PIC  X(3)   VALUE SPACE.                 
018400         05  W-IDDC-ASEQ-MAX     PIC  X(2)   VALUE SPACE.                 
018500         05  W-IDDISTR-ASEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
018600         05  W-IDKUNDNR-ASEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
018700         05  W-IDRAPPNR-ASEQ-MAX PIC  9(7)   VALUE ZERO.                  
018800                                                                          
018900     03  W-WDA3FSEQ-MIN-X.                                                
019000         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
019100         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
019200         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
019300         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
019400                                                                          
019500     03  W-WDA3FSEQ-MAX-X.                                                
019600         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
019700         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
019800         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
019900         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
020000                                                                          
020100     03  W-WDA3BSEQ-MIN-X.                                                
020200         05  W-IDRT-BSEQ-MIN     PIC  X(3)          VALUE SPACE.          
020300         05  W-IDDC-BSEQ-MIN     PIC  X(2)          VALUE SPACE.          
020400         05  W-IDRTLOP-BSEQ-MIN  PIC  9(3)          VALUE ZERO.           
020500         05  W-IDKOLLI-BSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
020600                                                                          
020700     03  W-WDA3BSEQ-MAX-X.                                                
020800         05  W-IDRT-BSEQ-MAX     PIC  X(3)          VALUE SPACE.          
020900         05  W-IDDC-BSEQ-MAX     PIC  X(2)          VALUE SPACE.          
021000         05  W-IDRTLOP-BSEQ-MAX  PIC  9(3)          VALUE ZERO.           
021100         05  W-IDKOLLI-BSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
021200                                                                          
021300     03  W-WDA3B1KY-MIN-X.                                                
021400         05  W-IDRT-B1-MIN       PIC  X(3)          VALUE SPACE.          
021500         05  W-IDDC-B1-MIN       PIC  X(2)          VALUE SPACE.          
021600         05  W-IDRTLOP-B1-MIN    PIC  9(3)          VALUE ZERO.           
021700         05  W-IDKOLLI-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
021800         05  W-DAREGDAT-B1-MIN   PIC  9(8)          VALUE ZERO.           
021900         05  W-TIKLOCK-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
022000                                                                          
022100     03  W-WDA3B1KY-MAX-X.                                                
022200         05  W-IDRT-B1-MAX       PIC  X(3)          VALUE SPACE.          
022300         05  W-IDDC-B1-MAX       PIC  X(2)          VALUE SPACE.          
022400         05  W-IDRTLOP-B1-MAX    PIC  9(3)          VALUE ZERO.           
022500         05  W-IDKOLLI-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
022600         05  W-DAREGDAT-B1-MAX   PIC  9(8)          VALUE ZERO.           
022700         05  W-TIKLOCK-B1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
022800                                                                          
022900     03  W-WDGXKEY-X.                                                     
023000         05  W-IDHTYP            PIC  X(4)   VALUE '4111'.                
023100         05  W-IDRT-4111         PIC  X(3)   VALUE SPACE.                 
023200         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
023300                                                                          
023400     03  W-IDKOLLI-X.                                                     
023500         05  W-IDKOLLI           PIC S9(5)   COMP-3 VALUE ZERO.           
023600     SKIP2                                                                
023700*    --- STATUS-KOD FRÅN IMS                                              
023800 01  STATUS-WS                   PIC XX.                                  
023900     88  SEGMENT-FINNS                       VALUE '  '.                  
024000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024300     SKIP2                                                                
024400 01  GODK-STATUSKODER.                                                    
024500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024600     SKIP3                                                                
024700 01  SSA1                        PIC X(128).                              
024800 01  SSA2                        PIC X(64).                               
024900     EJECT                                                                
025000*    --- IMS FUNKTIONSKODER                                               
025100*01  -COPY W0003                                                          
025200     EJECT                                                                
025300*    ---  DLI INPUT-OUTPUT AREA                                           
025400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025500     SKIP3                                                                
025600 01  DLI-IO-AREA.                                                         
025700     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
025800     SKIP3                                                                
025900     03  WLRETA01 REDEFINES IO-AREA.                                      
026000*        05  -COPY WDA301                                                 
026100     EJECT                                                                
026200     03  WLRETA01 REDEFINES IO-AREA.                                      
026300*        05  -COPY WDA3B1                                                 
026400     EJECT                                                                
026701 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDGX4111'.          
026702 01  DLI-IO-WDGX4111.                                                     
026703*    03  -COPY WDGX4111                                                   
026704     EJECT                                                                
026705 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDGX4112'.          
026706 01  DLI-IO-WDGX4112.                                                     
026707*    03  -COPY WDGX4112                                                   
026709                                                                          
026710     EJECT                                                                
026800 LINKAGE SECTION.                                                         
026900 01  MSG-PCB                     PIC X.                                   
027000     EJECT                                                                
027100*01  -COPY W0008  -PRE RETA1-                                             
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE RETA2-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008  -PRE RETA3-                                             
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01  -COPY W0008  -PRE RETA4-                                             
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300*01  -COPY W0008  -PRE RETC-                                              
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600*01  -COPY W0008  -PRE 4111-                                              
028700     05  FILLER                  PIC X.                                   
028800     EJECT                                                                
028900                                                                          
029000 PROCEDURE DIVISION  USING MSG-PCB                                        
029100                           RETA1-PCB RETA2-PCB RETA3-PCB                  
029200                           RETA4-PCB RETC-PCB 4111-PCB.                   
029300 MAIN SECTION.                                                            
029400     ENTRY 'DLITCBL' USING MSG-PCB                                        
029500                           RETA1-PCB RETA2-PCB RETA3-PCB                  
029600                           RETA4-PCB RETC-PCB 4111-PCB.                   
029700                                                                          
029800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
029900     IF SUB-KDRC = 0                                                      
030000       IF REQU-KDPGMACT = 'C' OR 'E' OR 'S' OR 'N'                        
030100         PERFORM A-INIT                                                   
030200         PERFORM B-KOLLA-NYCKLAR                                          
030300         IF NYCKLAR-OK                                                    
030400           IF REQU-KDPGMACT = 'C'                                         
030500              PERFORM C-RAKNA-IDDC-RADER                                  
030600           ELSE                                                           
030700              IF REQU-KDPGMACT = 'E' OR 'N'                               
030800                PERFORM G-KOLLA-INPUT                                     
030900              END-IF                                                      
031000              IF REQU-KDPGMACT = 'E' OR 'N'                               
031100                IF INDATA-OK                                              
031200                  PERFORM H-UPPDATERA                                     
031300                END-IF                                                    
031400              END-IF                                                      
031500              PERFORM F-LAES-VISA-INFO                                    
031600           END-IF                                                         
031700         END-IF                                                           
031800       ELSE                                                               
031900         MOVE SYSTEM-ERROR     TO RESP-IDMSG-ERROR                        
032000       END-IF                                                             
032100                                                                          
032200       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
032300       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
032400       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
032500       IF WS-IDMSG-INFO NOT = SPACE                                       
032600         MOVE SPACE            TO RESP-IDELMT-ERROR                       
032700         MOVE SPACE            TO RESP-IDMSG-ERROR                        
032800       ELSE                                                               
032900         IF WS-IDMSG-ERROR NOT = SPACE                                    
033000           MOVE ALL '+'          TO RESP-AREA                             
033100           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
033200           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
033300           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
033400           MOVE 001              TO RESP-IDMSGVER                         
033500           IF REQU-KVRADER NUMERIC                                        
033600             MOVE REQU-KVRADER   TO RESP-KVRADER                          
033700           ELSE                                                           
033800             MOVE ZERO           TO RESP-KVRADER                          
033900           END-IF                                                         
034000         END-IF                                                           
034100       END-IF                                                             
034200       PERFORM S02-RETURN-RESPONSE                                        
034300     END-IF                                                               
034400                                                                          
034500                                                                          
034600     MOVE ZERO TO RETURN-CODE                                             
034700     GOBACK                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 A-INIT SECTION.                                                          
035100     MOVE 'A-INIT'       TO CURRENT-SECTION                               
035200                                                                          
035300     MOVE LOW-VALUE      TO W-WDA3ASEQ-MIN-X                              
035400                            W-WDA3BSEQ-MIN-X                              
035500                            W-WDA3FSEQ-MIN-X                              
035600                            W-WDA3B1KY-MIN-X                              
035700                                                                          
035800     MOVE HIGH-VALUE     TO W-WDA3ASEQ-MAX-X                              
035900                            W-WDA3BSEQ-MAX-X                              
036000                            W-WDA3FSEQ-MAX-X                              
036100                            W-WDA3B1KY-MAX-X                              
036200                                                                          
036300     MOVE ALL '+'        TO RESP-AREA                                     
036400     MOVE SPACE          TO RESP-IDMSG-ERROR                              
036500                            RESP-IDMSG-INFO                               
036600                            RESP-IDELMT-ERROR                             
036700     MOVE 001            TO RESP-IDMSGVER                                 
036800     MOVE ZERO           TO RESP-KVRADER                                  
036900     MOVE ZERO           TO WS-COUNT                                      
037000     .                                                                    
037100     EJECT                                                                
037200 B-KOLLA-NYCKLAR SECTION.                                                 
037300     MOVE 'B-KOLLA-NYCKLAR' TO CURRENT-SECTION                            
037400                                                                          
037500     MOVE JA TO NYCKLAR-SW                                                
037600                                                                          
037700     MOVE REQU-IDDC-RET  TO W-IDDC-ASEQ-MIN                               
037710                            W-IDDC-ASEQ-MAX                               
037800                            W-IDDC-FSEQ-MIN                               
038100                            W-IDDC-FSEQ-MAX                               
038110                            W-IDDC                                        
038200                                                                          
038300     MOVE REQU-IDRT-KEY    TO W-IDRT-ASEQ-MIN                             
038400                              W-IDRT-ASEQ-MAX                             
038500                              W-IDRT-BSEQ-MIN                             
038600                              W-IDRT-BSEQ-MAX                             
038700                              W-IDRT-B1-MIN                               
038800                              W-IDRT-B1-MAX                               
038900                              W-IDRT-4111                                 
039000                                                                          
039100*    -- KONTROLL AV IDKOLLI                                               
039200                                                                          
039300     IF REQU-IDKOLLI-KEY  NUMERIC AND                                     
039400        REQU-IDKOLLI-KEY  > ZERO                                          
039500        MOVE REQU-IDKOLLI-KEY TO W-IDKOLLI-BSEQ-MIN                       
039600                                 W-IDKOLLI-BSEQ-MAX                       
039700                                 W-IDKOLLI                                
039800     END-IF                                                               
039900                                                                          
040000*    -- KONTROLL AV FLAGGA VISA KOLLIINNEHÅLL                             
040100                                                                          
040200     MOVE REQU-FLVISA-KEY TO W-FLVISA                                     
040300                                                                          
040400     IF W-FLVISA          = JA OR YES                                     
040500        MOVE JA           TO SW-VISA-KOLLI                                
040600     ELSE                                                                 
040700        MOVE NEJ          TO SW-VISA-KOLLI                                
040800                             W-FLVISA                                     
040900     END-IF                                                               
041000                                                                          
041100     IF VISA-KOLLI AND REQU-IDKOLLI-KEY NOT NUMERIC                       
041200        MOVE NEJ          TO NYCKLAR-SW                                   
041300        MOVE '026'              TO RESP-IDMSG-ERROR                       
041400        MOVE 'IDKOLLI'          TO RESP-IDELMT-ERROR                      
041500     END-IF                                                               
041600                                                                          
041700     MOVE REQU-IDDC-KEY   TO RESP-IDDC-KEY                                
041800                                                                          
041900     IF NYCKLAR-OK                                                        
042000       IF REQU-IDKOLLI-KEY NUMERIC                                        
042100          MOVE REQU-IDKOLLI-KEY  TO RESP-IDKOLLI-KEY                      
042200          INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE        
042300       END-IF                                                             
042400       MOVE W-FLVISA               TO RESP-FLVISA-KEY                     
042500                                                                          
042600     END-IF                                                               
042700                                                                          
042800     IF REQU-IDRT-KEY = SPACE OR                                          
042900        REQU-IDRT-KEY = 'CDC' OR                                          
043000        REQU-IDRT-KEY = 'US1' OR                                          
043100        REQU-IDRT-KEY = 'US2' OR                                          
043200        REQU-IDRT-KEY = 'US3' OR                                          
043210        REQU-IDRT-KEY = 'US4' OR                                          
043220        REQU-IDRT-KEY = 'US5' OR                                          
043230        REQU-IDRT-KEY = 'US6' OR                                          
043240        REQU-IDRT-KEY = 'ET2' OR                                          
043300        REQU-IDRT-KEY = 'CA1'                                             
043400        MOVE 'IDRT'         TO RESP-IDELMT-ERROR                          
043500        MOVE '023'          TO RESP-IDMSG-ERROR                           
043600        MOVE NEJ            TO NYCKLAR-SW                                 
043700     END-IF                                                               
043800                                                                          
043900     IF NYCKLAR-FEL                                                       
044000       IF REQU-IDRT-KEY = SPACE OR                                        
044100          REQU-IDRT-KEY = 'CDC' OR                                        
044200          REQU-IDRT-KEY = 'US1' OR                                        
044300          REQU-IDRT-KEY = 'US2' OR                                        
044400          REQU-IDRT-KEY = 'US3' OR                                        
044410          REQU-IDRT-KEY = 'US4' OR                                        
044420          REQU-IDRT-KEY = 'US5' OR                                        
044430          REQU-IDRT-KEY = 'US6' OR                                        
044440          REQU-IDRT-KEY = 'ET2' OR                                        
044500          REQU-IDRT-KEY = 'CA1'                                           
044600         MOVE ERR-OTILL-UPD   TO RESP-IDMSG-ERROR                         
044700       END-IF                                                             
044800     END-IF                                                               
044900     .                                                                    
045000     EJECT                                                                
045100 C-RAKNA-IDDC-RADER SECTION.                                              
045200     MOVE 'C-RAKNA-IDDC-RADER' TO CURRENT-SECTION                         
045300                                                                          
045400     MOVE LOW-VALUE   TO W-IDDC-ASEQ-MIN                                  
045500     MOVE HIGH-VALUE  TO W-IDDC-ASEQ-MAX                                  
045600                                                                          
045700     MOVE SPACE       TO RESP-IDDC-RET1                                   
045800                         RESP-IDDC-RET2                                   
045900     MOVE ZERO        TO W-ANTAL-RADER-RET1                               
046000                         W-ANTAL-RADER-RET2                               
046100                                                                          
046200     PERFORM IMS-GU-SEQA-WLRETA01                                         
046300     IF SEGMENT-FINNS                                                     
046400                                                                          
046500        MOVE RET-IDDC TO RESP-IDDC-RET1                                   
046600        PERFORM UNTIL SEGMENT-SAKNAS  OR                                  
046700                      RET-IDDC NOT = RESP-IDDC-RET1                       
046800                                                                          
046900           ADD +1 TO W-ANTAL-RADER-RET1                                   
047000           PERFORM IMS-GN-SEQA-WLRETA01                                   
047100                                                                          
047200        END-PERFORM                                                       
047300                                                                          
047400        IF SEGMENT-FINNS                                                  
047500           MOVE RET-IDDC TO RESP-IDDC-RET2                                
047600           PERFORM UNTIL SEGMENT-SAKNAS                                   
047700                                                                          
047800              ADD +1 TO W-ANTAL-RADER-RET2                                
047900              PERFORM IMS-GN-SEQA-WLRETA01                                
048000                                                                          
048100           END-PERFORM                                                    
048200        END-IF                                                            
048300        MOVE W-ANTAL-RADER-RET1 TO RESP-KVRADER-RET1                      
048400        MOVE W-ANTAL-RADER-RET2 TO RESP-KVRADER-RET2                      
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 F-LAES-VISA-INFO SECTION.                                                
048900     MOVE 'F-LAES-VISA-INFO  ' TO CURRENT-SECTION                         
049000                                                                          
049100     IF VISA-KOLLI                                                        
049200        PERFORM FA-LAES-PACKAT-KOLLI                                      
049300     ELSE                                                                 
049400        PERFORM FB-LAES-OPACKADE-RAPPORTER                                
049500     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800 FA-LAES-PACKAT-KOLLI SECTION.                                            
049900     MOVE 'FA-LAES-PACKAT-KOLLI' TO CURRENT-SECTION                       
050000                                                                          
050100     MOVE ZERO                  TO W-IDRTLOP-BSEQ-MIN                     
050200     MOVE 999                   TO W-IDRTLOP-BSEQ-MAX                     
050300     PERFORM IMS-GU-SEQB-WLRETA01                                         
050400                                                                          
050500     IF SEGMENT-SAKNAS                                                    
050600        MOVE '025'              TO RESP-IDMSG-ERROR                       
050700        MOVE 'IDKOLLI'          TO RESP-IDELMT-ERROR                      
050800     ELSE                                                                 
050900       MOVE +1                  TO INDX                                   
051000                                                                          
051100       PERFORM UNTIL INDX       > MAX-INDX OR SEGMENT-SAKNAS              
051200         IF SEGMENT-FINNS                                                 
051300           IF RET-KDRETSTA = '1' OR '2' OR '3'                            
051400             IF INDX = +1                                                 
051500               MOVE RET-FLFARLIG   TO RESP-FLFARLIG-KOLLI                 
051600             END-IF                                                       
051700                                                                          
051800             PERFORM S01-REDIGERA-MOD                                     
051900             PERFORM IMS-GN-SEQB-WLRETA01                                 
052000           END-IF                                                         
052100         END-IF                                                           
052200         ADD 1 TO INDX                                                    
052300       END-PERFORM                                                        
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 FB-LAES-OPACKADE-RAPPORTER SECTION.                                      
052800     MOVE 'FB-LAES-OPACKADE-RAP' TO CURRENT-SECTION                       
052900                                                                          
053000     PERFORM FBA-LAES-KOLLI                                               
053100                                                                          
053200     PERFORM IMS-GU-SEQA-WLRETA01                                         
053300     IF SEGMENT-SAKNAS                                                    
053400        MOVE 'RETURN'           TO RESP-IDELMT-ERROR                      
053500        MOVE '025'              TO RESP-IDMSG-ERROR                       
053600     ELSE                                                                 
053700        MOVE +1                  TO INDX                                  
053800                                                                          
053900        PERFORM UNTIL INDX       > MAX-INDX OR SEGMENT-SAKNAS             
054000          IF SEGMENT-FINNS                                                
054100                                                                          
054200            PERFORM S01-REDIGERA-MOD                                      
054300            PERFORM IMS-GN-SEQA-WLRETA01                                  
054400          END-IF                                                          
054500          ADD 1 TO INDX                                                   
054600        END-PERFORM                                                       
054700                                                                          
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100                                                                          
055200 FBA-LAES-KOLLI           SECTION.                                        
055300     MOVE 'FBA-LAES-KOLLI      ' TO CURRENT-SECTION                       
055400                                                                          
055500     MOVE REQU-IDDC-RET            TO W-IDDC-BSEQ-MIN                     
055600                                      W-IDDC-BSEQ-MAX                     
055700     MOVE ZERO                     TO W-IDRTLOP-BSEQ-MIN                  
055800                                      W-IDRTLOP-BSEQ-MAX                  
055900     PERFORM IMS-GU-SEQB-WLRETA01                                         
056000     IF SEGMENT-FINNS                                                     
056100        MOVE RET-FLFARLIG          TO RESP-FLFARLIG-KOLLI                 
056200     END-IF                                                               
056300                                                                          
056400     .                                                                    
056500     EJECT                                                                
056600 G-KOLLA-INPUT SECTION.                                                   
056700     MOVE 'G-KOLLA-INPUT       ' TO CURRENT-SECTION                       
056800                                                                          
056900     MOVE JA               TO INDATA-SW                                   
057000     MOVE NEJ              TO SW-KOLLI                                    
057100                                                                          
057200     PERFORM GA-FORMELL-KONTROLL                                          
057300     IF INDATA-OK                                                         
057400        PERFORM GB-LOGISK-KONTROLL                                        
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 GA-FORMELL-KONTROLL SECTION.                                             
058000     MOVE 'GA-FORMELL-KONTROLL ' TO CURRENT-SECTION                       
058100                                                                          
058200     IF REQU-INPUT        = ALL '+' AND                                   
058300        REQU-KDPGMACT NOT = 'N'                                           
058400       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
058500       MOVE NEJ                  TO INDATA-SW                             
058600     ELSE                                                                 
058700                                                                          
058800       MOVE NEJ                   TO SW-PACKA-I-FLERA-KOLLI               
058900       INSPECT REQU-IDKOLLI-FOM REPLACING LEADING SPACE BY ZERO           
059000       INSPECT REQU-IDKOLLI-TOM REPLACING LEADING SPACE BY ZERO           
059100       IF REQU-IDKOLLI-FOM        NUMERIC AND                             
059200          REQU-IDKOLLI-FOM        > ZERO                                  
059300           MOVE JA                TO SW-PACKA-I-FLERA-KOLLI               
059400           MOVE REQU-IDKOLLI-FOM  TO RESP-IDKOLLI-FOM                     
059500           MOVE REQU-IDKOLLI-TOM  TO RESP-IDKOLLI-TOM                     
059600       END-IF                                                             
059700                                                                          
059800       PERFORM GAA-KOLLA-NYTT-KOLLI                                       
059900                                                                          
060000       IF REQU-KDPGMACT NOT = 'N'                                         
060100         PERFORM GAB-KOLLA-KDCMD                                          
060200         PERFORM GAC-KOLLA-INM-RAD                                        
060300         PERFORM GAD-KOLLA-ANT-FUNKTIONER                                 
060400       END-IF                                                             
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800                                                                          
060900 GAA-KOLLA-NYTT-KOLLI SECTION.                                            
061000     MOVE 'GAA-KOLLA-NYTT-KOLLI' TO CURRENT-SECTION                       
061100                                                                          
061200     MOVE NEJ                      TO SW-SKAPA-NYTT-KOLLI                 
061300                                      SW-SKAPA-FLERA-KOLLI                
061400                                                                          
061500     IF REQU-KDPGMACT = 'N'                                               
061600       MOVE JA                     TO SW-SKAPA-NYTT-KOLLI                 
061700     END-IF                                                               
061800                                                                          
061900     IF REQU-KVKOLLI              NOT = ALL '+'                           
062000       IF REQU-KVKOLLI            NOT NUMERIC                             
062100         MOVE NEJ                 TO INDATA-SW                            
062200         MOVE '024'               TO RESP-IDMSG-ERROR                     
062300         MOVE 'KVKOLLI'           TO RESP-IDELMT-ERROR                    
062400       ELSE                                                               
062500         IF SKAPA-NYTT-KOLLI                                              
062600         OR                                                               
062700          ((REQU-IDKOLLI-KEY NOT = ALL '+' OR                             
062800            REQU-IDKOLLI-FOM NOT = ALL '+')                               
062900            AND REQU-KDPGMACT = 'E')                                      
063000                                                                          
063100           MOVE JA                TO SW-SKAPA-FLERA-KOLLI                 
063200           MOVE REQU-KVKOLLI      TO W-KVKOLLI                            
063300         ELSE                                                             
063400           MOVE NEJ               TO INDATA-SW                            
063500           MOVE '023'             TO RESP-IDMSG-ERROR                     
063600           MOVE 'KVKOLLI'         TO RESP-IDELMT-ERROR                    
063700         END-IF                                                           
063800       END-IF                                                             
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 GAB-KOLLA-KDCMD      SECTION.                                            
064300     MOVE 'GAB-KOLLA-KDCMD     ' TO CURRENT-SECTION                       
064400                                                                          
064500                                                                          
064600     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
064700        MOVE REQU-KVRADER      TO WS-INDX-REC                             
064800        MOVE NEJ               TO WS-REC-LIMIT                            
064900        MOVE NEJ               TO  SW-RAD-CMD                             
065000        MOVE +1                TO INDX                                    
065100                                                                          
065200     PERFORM UNTIL INDX     >  MAX-INDX OR REC-LIMIT                      
065300        IF REQU-KDCMD(INDX) = ALL '+' OR SPACE                            
065400           CONTINUE                                                       
065500        ELSE                                                              
065600           MOVE JA          TO  SW-RAD-CMD                                
065700                                                                          
065800           IF VISA-KOLLI                                                  
065900              IF REQU-KDCMD(INDX)     = W-DELETE                          
066000                 CONTINUE                                                 
066100              ELSE                                                        
066200                 MOVE NEJ                  TO INDATA-SW                   
066300                 MOVE '023'                TO RESP-IDMSG-ERROR            
066400                                    RESP-IDMSG-ERROR-LINE (INDX)          
066500                 MOVE 'CMD'                TO RESP-IDELMT-ERROR           
066600              END-IF                                                      
066700           ELSE                                                           
066800              IF REQU-KDCMD(INDX)          =  W-PACKA                     
066900                 CONTINUE                                                 
067000              ELSE                                                        
067100                 MOVE NEJ                  TO INDATA-SW                   
067200                 MOVE '023'                TO RESP-IDMSG-ERROR            
067300                                    RESP-IDMSG-ERROR-LINE (INDX)          
067400                 MOVE 'CMD'                TO RESP-IDELMT-ERROR           
067500              END-IF                                                      
067600           END-IF                                                         
067700        END-IF                                                            
067800                                                                          
067900          IF INDX = WS-INDX-REC                                           
068000             MOVE JA TO WS-REC-LIMIT                                      
068100          ELSE                                                            
068200             ADD +1                       TO INDX                         
068300          END-IF                                                          
068400                                                                          
068500     END-PERFORM                                                          
068600     ELSE                                                                 
068700        MOVE NEJ           TO INDATA-SW                                   
068800        IF REQU-KVRADER = 0                                               
068900           MOVE 'KVRADER' TO RESP-IDELMT-ERROR                            
069000           MOVE '126'     TO RESP-IDMSG-ERROR                             
069100        ELSE                                                              
069200           MOVE 'KVRADER' TO RESP-IDELMT-ERROR                            
069300           MOVE '024'     TO RESP-IDMSG-ERROR                             
069400        END-IF                                                            
069500     END-IF                                                               
069600                                                                          
069700     .                                                                    
069800     EJECT                                                                
069900 GAC-KOLLA-INM-RAD    SECTION.                                            
070000     MOVE 'GAC-KOLLA-INKM-RAD  ' TO CURRENT-SECTION                       
070100                                                                          
070200     MOVE NEJ                         TO  SW-INM-RAD                      
070300     IF REQU-IDDISTR-IN               NOT = ALL '+' OR                    
070400        REQU-IDKUNDNR-IN              NOT = ALL '+' OR                    
070500        REQU-IDRAPPNR-IN              NOT = ALL '+'                       
070600        MOVE JA                       TO  SW-INM-RAD                      
070700                                                                          
070800        IF REQU-IDDISTR-IN             NUMERIC                            
070900           CONTINUE                                                       
071000        ELSE                                                              
071100          MOVE NEJ                     TO INDATA-SW                       
071200          MOVE '024'                   TO RESP-IDMSG-ERROR                
071300          MOVE 'IDDISTR'               TO RESP-IDELMT-ERROR               
071400        END-IF                                                            
071500                                                                          
071600        IF REQU-IDKUNDNR-IN            NUMERIC                            
071700           CONTINUE                                                       
071800        ELSE                                                              
071900          MOVE NEJ                     TO INDATA-SW                       
072000          MOVE '024'                   TO RESP-IDMSG-ERROR                
072100          MOVE 'IDKUNDNR'              TO RESP-IDELMT-ERROR               
072200        END-IF                                                            
072300                                                                          
072400        IF REQU-IDRAPPNR-IN            NUMERIC                            
072500           CONTINUE                                                       
072600        ELSE                                                              
072700          MOVE NEJ                     TO INDATA-SW                       
072800          MOVE '024'                   TO RESP-IDMSG-ERROR                
072900          MOVE 'IDRAPPNR'              TO RESP-IDELMT-ERROR               
073000        END-IF                                                            
073100                                                                          
073200     END-IF                                                               
073300                                                                          
073400     .                                                                    
073500     EJECT                                                                
073600 GAD-KOLLA-ANT-FUNKTIONER SECTION.                                        
073700     MOVE 'GAD-KOLLA-ANT-FUNK  ' TO CURRENT-SECTION                       
073800                                                                          
073900     IF SKAPA-NYTT-KOLLI                                                  
074000        IF RAD-CMD OR INM-RAD                                             
074100           MOVE NEJ                TO INDATA-SW                           
074200           MOVE '247'              TO RESP-IDMSG-ERROR                    
074300        END-IF                                                            
074400     END-IF                                                               
074500                                                                          
074600     IF RAD-CMD                                                           
074700        IF SKAPA-NYTT-KOLLI                                               
074800           MOVE NEJ                TO INDATA-SW                           
074900           MOVE '247'              TO RESP-IDMSG-ERROR                    
075000        END-IF                                                            
075100                                                                          
075200        IF INM-RAD                                                        
075300           MOVE NEJ                TO INDATA-SW                           
075400           MOVE '990'              TO RESP-IDMSG-ERROR                    
075500        END-IF                                                            
075600     END-IF                                                               
075700                                                                          
075800     IF INM-RAD                                                           
075900        IF SKAPA-NYTT-KOLLI                                               
076000           MOVE NEJ                TO INDATA-SW                           
076100           MOVE '247'              TO RESP-IDMSG-ERROR                    
076200        END-IF                                                            
076300                                                                          
076400        IF RAD-CMD                                                        
076500           MOVE NEJ                TO INDATA-SW                           
076600           MOVE '991'              TO RESP-IDMSG-ERROR                    
076700        END-IF                                                            
076800     END-IF                                                               
076900                                                                          
077000     .                                                                    
077100     EJECT                                                                
077200 GB-LOGISK-KONTROLL SECTION.                                              
077300     MOVE 'GB-LOGISK-KONTROLL  ' TO CURRENT-SECTION                       
077400                                                                          
077500     IF RAD-CMD                                                           
077600       PERFORM GBA-KOLLA-VALDA-RADER                                      
077700     END-IF                                                               
077800                                                                          
077900     IF INM-RAD                                                           
078000       PERFORM GBB-KOLLA-INMATAT-TILLSTAND                                
078100     END-IF                                                               
078200                                                                          
078300     .                                                                    
078400     EJECT                                                                
078500 GBA-KOLLA-VALDA-RADER    SECTION.                                        
078600     MOVE 'GBA-KOLLA-VALDA-RADER' TO CURRENT-SECTION                      
078700                                                                          
078800     IF PACKA-I-FLERA-KOLLI                                               
078900        CONTINUE                                                          
079000     ELSE                                                                 
079100        PERFORM S02-KOLLA-KOLLISTATUS                                     
079200     END-IF                                                               
079300                                                                          
079400     IF INDATA-OK                                                         
079500       MOVE JA                 TO SW-FORSTA-VALDA-RAD                     
079600       MOVE +1                 TO INDX                                    
079700       MOVE NEJ                TO WS-REC-LIMIT                            
079800       PERFORM UNTIL INDX      >  MAX-INDX OR REC-LIMIT                   
079900          IF REQU-KDCMD(INDX)  =  W-PACKA OR W-DELETE                     
080000             PERFORM GBAB-KOLLA-VALT-RETURTILLSTAND                       
080100          END-IF                                                          
080200                                                                          
080300         IF INDX = WS-INDX-REC                                            
080400            MOVE JA TO WS-REC-LIMIT                                       
080500         ELSE                                                             
080600            ADD +1                       TO INDX                          
080700         END-IF                                                           
080800       END-PERFORM                                                        
080900     END-IF                                                               
081000                                                                          
081100     .                                                                    
081200     EJECT                                                                
081300 GBAB-KOLLA-VALT-RETURTILLSTAND    SECTION.                               
081400     MOVE 'GBAB-KOLLA-VALT-RT   ' TO CURRENT-SECTION                      
081500                                                                          
081510     IF REQU-IDDISTR(INDX) NOT NUMERIC                                    
081520       MOVE ZERO TO REQU-IDDISTR(INDX)                                    
081530     END-IF                                                               
081510     IF REQU-IDKUNDNR(INDX) NOT NUMERIC                                   
081520       MOVE ZERO TO REQU-IDKUNDNR(INDX)                                   
081530     END-IF                                                               
081510     IF REQU-IDRAPPNR(INDX) NOT NUMERIC                                   
081520       MOVE ZERO TO REQU-IDRAPPNR(INDX)                                   
081530     END-IF                                                               
                                                                                
081600     MOVE REQU-IDDISTR(INDX)       TO W-IDDISTR-FSEQ-MIN                  
081700                                      W-IDDISTR-FSEQ-MAX                  
081800     MOVE REQU-IDKUNDNR(INDX)      TO W-IDKUNDNR-FSEQ-MIN                 
081900                                      W-IDKUNDNR-FSEQ-MAX                 
082000     MOVE REQU-IDRAPPNR(INDX)      TO W-IDRAPPNR-FSEQ-MIN                 
082100                                      W-IDRAPPNR-FSEQ-MAX                 
082200     PERFORM IMS-GHU-SEQF-WLRETA01                                        
082300     IF SEGMENT-FINNS                                                     
082400        IF REQU-KDCMD(INDX)        = W-PACKA                              
082500           PERFORM GBABA-KOLLA-PACKNING                                   
082600        END-IF                                                            
082700        IF REQU-KDCMD(INDX)        = W-DELETE                             
082800           PERFORM GBABB-KOLLA-BORTTAG                                    
082900        END-IF                                                            
083000     ELSE                                                                 
083100        MOVE NEJ                    TO INDATA-SW                          
083200        MOVE '023'                  TO RESP-IDMSG-ERROR                   
083300                              RESP-IDMSG-ERROR-LINE (INDX)                
083400        MOVE 'CMD'                  TO RESP-IDELMT-ERROR                  
083500     END-IF                                                               
083600     .                                                                    
083700     EJECT                                                                
083800                                                                          
083900 GBABA-KOLLA-PACKNING   SECTION.                                          
084000     MOVE 'GBABA-KOLLA-PACKNING ' TO CURRENT-SECTION                      
084100                                                                          
084200     IF RET-IDDC                   NOT = W-SPAR-IDDC-RET AND              
084300        KOLLI-FINNS                                                       
084400        MOVE NEJ                   TO INDATA-SW                           
084500        MOVE '007'                  TO RESP-IDMSG-ERROR                   
084600                               RESP-IDMSG-ERROR-LINE (INDX)               
084700     END-IF                                                               
084800                                                                          
084900     IF RET-IDKOLLI                NOT = ZERO                             
085000        MOVE NEJ                   TO INDATA-SW                           
085100        MOVE '023'                  TO RESP-IDMSG-ERROR                   
085200                               RESP-IDMSG-ERROR-LINE (INDX)               
085300        MOVE 'CMD'                  TO RESP-IDELMT-ERROR                  
085400     END-IF                                                               
085500                                                                          
085600     IF RET-FLFARLIG               NOT = W-SPAR-FLFARLIG AND              
085700        KOLLI-FINNS                                                       
085800        MOVE NEJ                   TO INDATA-SW                           
085900        MOVE '023'                 TO RESP-IDMSG-ERROR                    
086000                           RESP-IDMSG-ERROR-LINE (INDX)                   
086100        MOVE 'CMD'                 TO RESP-IDELMT-ERROR                   
086200     END-IF                                                               
086300                                                                          
086400     IF KOLLI-FINNS                                                       
086500        IF RET-IDPERSON             = W-IDPERSON AND                      
086600           RET-KDARBTYP             = W-KDARBTYP                          
086700            CONTINUE                                                      
086800        ELSE                                                              
086900            MOVE NEJ                   TO INDATA-SW                       
087000            MOVE '023'                 TO RESP-IDMSG-ERROR                
087100                              RESP-IDMSG-ERROR-LINE (INDX)                
087200            MOVE 'CMD'                 TO RESP-IDELMT-ERROR               
087300        END-IF                                                            
087400     ELSE                                                                 
087500        IF FORSTA-VALDA-RAD                                               
087600           MOVE RET-IDPERSON        TO W-IDPERSON                         
087700           MOVE RET-KDARBTYP        TO W-KDARBTYP                         
087800           MOVE NEJ                 TO SW-FORSTA-VALDA-RAD                
087900        ELSE                                                              
088000           IF RET-IDPERSON          = W-IDPERSON AND                      
088100              RET-KDARBTYP          = W-KDARBTYP                          
088200               CONTINUE                                                   
088300           ELSE                                                           
088400               MOVE NEJ                   TO INDATA-SW                    
088500               MOVE '023'                 TO RESP-IDMSG-ERROR             
088600                                  RESP-IDMSG-ERROR-LINE (INDX)            
088700               MOVE 'CMD'                 TO RESP-IDELMT-ERROR            
088800           END-IF                                                         
088900        END-IF                                                            
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300                                                                          
089400 GBABB-KOLLA-BORTTAG    SECTION.                                          
089500     MOVE 'GBABB-KOLLA-BORTTAG  ' TO CURRENT-SECTION                      
089600                                                                          
089700     IF RET-KDKOLSTA               NOT = W-KLI-PACKAT                     
089800        MOVE NEJ                   TO INDATA-SW                           
089900        MOVE '023'                 TO RESP-IDMSG-ERROR                    
090000                              RESP-IDMSG-ERROR-LINE (INDX)                
090100        MOVE 'CMD'                 TO RESP-IDELMT-ERROR                   
090200     END-IF                                                               
090300                                                                          
090400     .                                                                    
090500     EJECT                                                                
090600                                                                          
090700 GBB-KOLLA-INMATAT-TILLSTAND  SECTION.                                    
090800     MOVE 'GBB-KOLLA-INMATAT-TIL' TO CURRENT-SECTION                      
090900                                                                          
091000     PERFORM S02-KOLLA-KOLLISTATUS                                        
091100                                                                          
091200     MOVE REQU-IDDISTR-IN          TO W-IDDISTR-FSEQ-MIN                  
091300                                      W-IDDISTR-FSEQ-MAX                  
091400     MOVE REQU-IDKUNDNR-IN         TO W-IDKUNDNR-FSEQ-MIN                 
091500                                      W-IDKUNDNR-FSEQ-MAX                 
091600     MOVE REQU-IDRAPPNR-IN         TO W-IDRAPPNR-FSEQ-MIN                 
091700                                      W-IDRAPPNR-FSEQ-MAX                 
091800     PERFORM IMS-GHU-SEQF-WLRETA01                                        
091900     IF SEGMENT-FINNS                                                     
092000        IF RET-IDKOLLI             NOT = ZERO                             
092100           IF RET-KDKOLSTA NOT = W-KLI-PACKAT                             
092200              MOVE NEJ                TO INDATA-SW                        
092300              MOVE '992'              TO RESP-IDMSG-ERROR                 
092400           END-IF                                                         
092500        END-IF                                                            
092600                                                                          
092700        IF RET-FLFARLIG            NOT = W-SPAR-FLFARLIG AND              
092800           KOLLI-FINNS                                                    
092900           MOVE NEJ                TO INDATA-SW                           
093000           MOVE '993'              TO RESP-IDMSG-ERROR                    
093100        END-IF                                                            
093200                                                                          
093300        IF KOLLI-FINNS                                                    
093400           IF RET-IDPERSON          = W-IDPERSON AND                      
093500              RET-KDARBTYP          = W-KDARBTYP                          
093600               CONTINUE                                                   
093700           ELSE                                                           
093800               MOVE NEJ                TO INDATA-SW                       
093900               MOVE '994'              TO RESP-IDMSG-ERROR                
094000           END-IF                                                         
094100        END-IF                                                            
094200                                                                          
094300        IF RET-IDDC             NOT = W-SPAR-IDDC-RET AND                 
094400           KOLLI-FINNS                                                    
094500           MOVE NEJ                TO INDATA-SW                           
094600           MOVE '007'              TO RESP-IDMSG-ERROR                    
094700        END-IF                                                            
094800                                                                          
094900     ELSE                                                                 
095000        MOVE NEJ                TO INDATA-SW                              
095100        MOVE '025'              TO RESP-IDMSG-ERROR                       
095200        MOVE 'REPORT'           TO RESP-IDELMT-ERROR                      
095300     END-IF                                                               
095400     .                                                                    
095500     EJECT                                                                
095600 H-UPPDATERA SECTION.                                                     
095700     MOVE 'H-UPPDATERA          ' TO CURRENT-SECTION                      
095800                                                                          
095900     MOVE NEJ           TO  W-UPDATE-SW                                   
096000                                                                          
096100     IF SKAPA-NYTT-KOLLI                                                  
096200        PERFORM HA-TA-UT-KOLLINR                                          
096300        MOVE JA           TO  W-UPDATE-SW                                 
096400     END-IF                                                               
096500                                                                          
096600     IF RAD-CMD                                                           
096700        PERFORM HB-UPPDATERA-VALDA-TILLSTAND                              
096800     END-IF                                                               
096900                                                                          
097000     IF INM-RAD                                                           
097100       PERFORM HC-UPPDATERA-INMATAT-TILLSTAND                             
097200       MOVE JA           TO  W-UPDATE-SW                                  
097300     END-IF                                                               
097400                                                                          
097500     IF  W-UPDATE-OK                                                      
097600       IF REQU-KDPGMACT = 'N'                                             
097700         MOVE '294'               TO RESP-IDMSG-INFO                      
097800       ELSE                                                               
097900         MOVE '001'               TO RESP-IDMSG-INFO                      
098000       END-IF                                                             
098100     ELSE                                                                 
098200        MOVE '004'               TO RESP-IDMSG-INFO                       
098300     END-IF                                                               
098400     .                                                                    
098500     EJECT                                                                
098600                                                                          
098700 HA-TA-UT-KOLLINR SECTION.                                                
098800     MOVE 'HA-TA-UT-KOLLINR     ' TO CURRENT-SECTION                      
098900                                                                          
099000     PERFORM IMS-GHU-WL411111                                             
099100     IF SKAPA-FLERA-KOLLI                                                 
099200        COMPUTE W-IDKOLLI-NUM  =  4112-IDKOLLI + 1                        
099300        MOVE W-IDKOLLI-NUM     TO RESP-IDKOLLI-FOM                        
099400        COMPUTE 4112-IDKOLLI   =  4112-IDKOLLI + W-KVKOLLI                
099500        MOVE 4112-IDKOLLI      TO RESP-IDKOLLI-TOM                        
099600        MOVE ZERO              TO RESP-IDKOLLI-KEY                        
099700        INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE          
099710        INSPECT RESP-IDKOLLI-FOM REPLACING LEADING ZERO BY SPACE          
099800        INSPECT RESP-IDKOLLI-TOM REPLACING LEADING ZERO BY SPACE          
099900     ELSE                                                                 
100000        COMPUTE 4112-IDKOLLI   =  4112-IDKOLLI + 1                        
100100        MOVE 4112-IDKOLLI      TO W-IDKOLLI-NUM                           
100200        MOVE W-IDKOLLI-NUM     TO RESP-IDKOLLI-KEY                        
100300                                  REQU-IDKOLLI-KEY                        
100400        INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE          
100500                                                                          
100600     END-IF                                                               
100700     PERFORM IMS-REPL-WL411111                                            
100800                                                                          
100900                                                                          
101000     .                                                                    
101100     EJECT                                                                
101200                                                                          
101300 HB-UPPDATERA-VALDA-TILLSTAND SECTION.                                    
101400     MOVE 'HB-UPPDATERA-VALDA-TI' TO CURRENT-SECTION                      
101500                                                                          
101600     MOVE +1                 TO INDX                                      
101700     MOVE NEJ                TO WS-REC-LIMIT                              
101800     MOVE NEJ                TO  W-UPDATE-SW                              
101900     PERFORM UNTIL INDX      >  MAX-INDX  OR REC-LIMIT                    
102000        IF REQU-KDCMD(INDX)  =  W-PACKA                                   
102100           PERFORM HBA-PACKA-VALT-RT-I-KOLLI                              
102200        END-IF                                                            
102300                                                                          
102400        IF REQU-KDCMD(INDX)  =  W-DELETE                                  
102500           PERFORM HBB-TA-BORT-VALT-RT-UR-KOLLI                           
102600        END-IF                                                            
102700                                                                          
102800       IF INDX = WS-INDX-REC                                              
102900          MOVE JA TO WS-REC-LIMIT                                         
103000       ELSE                                                               
103100          ADD +1                       TO INDX                            
103200       END-IF                                                             
103300     END-PERFORM                                                          
103400                                                                          
103500     .                                                                    
103600     EJECT                                                                
103700                                                                          
103800 HBA-PACKA-VALT-RT-I-KOLLI   SECTION.                                     
103900     MOVE 'HBA-PACKA-VALT-RT-I-K' TO CURRENT-SECTION                      
104000                                                                          
104100     MOVE REQU-IDDISTR(INDX)       TO W-IDDISTR-FSEQ-MIN                  
104200                                      W-IDDISTR-FSEQ-MAX                  
104300     MOVE REQU-IDKUNDNR(INDX)      TO W-IDKUNDNR-FSEQ-MIN                 
104400                                      W-IDKUNDNR-FSEQ-MAX                 
104500     MOVE REQU-IDRAPPNR(INDX)      TO W-IDRAPPNR-FSEQ-MIN                 
104600                                      W-IDRAPPNR-FSEQ-MAX                 
104700     PERFORM IMS-GHU-SEQF-WLRETA01                                        
104800                                                                          
104900     IF SEGMENT-FINNS                                                     
105000        IF PACKA-I-FLERA-KOLLI                                            
105100           PERFORM HBAA-PACKA-I-FLERA-KOLLI                               
105200        ELSE                                                              
105300          MOVE REQU-IDKOLLI-KEY    TO RET-IDKOLLI                         
105400          MOVE W-KLI-PACKAT        TO RET-KDKOLSTA                        
105500          PERFORM IMS-REPL-SEQF-WLRETA01                                  
105600          MOVE JA           TO  W-UPDATE-SW                               
105700        END-IF                                                            
105800     ELSE                                                                 
105900        CALL FELLOG                                                       
106000        MOVE NEJ          TO  W-UPDATE-SW                                 
106100     END-IF                                                               
106200     .                                                                    
106300     EJECT                                                                
106400                                                                          
106500 HBAA-PACKA-I-FLERA-KOLLI   SECTION.                                      
106600     MOVE 'HBAA-PACKA-I-FLERA-KO' TO CURRENT-SECTION                      
106700                                                                          
106800     MOVE REQU-IDKOLLI-FOM         TO W-IDKOLLI-FOM                       
106900     MOVE REQU-IDKOLLI-TOM         TO W-IDKOLLI-TOM                       
107000                                                                          
107100     MOVE REQU-IDKOLLI-FOM         TO RET-IDKOLLI                         
107200     MOVE W-KLI-PACKAT             TO RET-KDKOLSTA                        
107300                                                                          
107400     PERFORM IMS-REPL-SEQF-WLRETA01                                       
107500     COMPUTE W-IDKOLLI-NUM         =  W-IDKOLLI-FOM + 1                   
107600     PERFORM UNTIL W-IDKOLLI-NUM   >  W-IDKOLLI-TOM                       
107700        MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                  
107800        ACCEPT RET-TIKLOCK         FROM TIME                              
107900        MOVE W-IDKOLLI-NUM         TO RET-IDKOLLI                         
108000        PERFORM IMS-ISRT-WLRETA01                                         
108100        IF SEGMENT-FINNS-REDAN                                            
108200           PERFORM UNTIL SEGMENT-FINNS                                    
108300              ADD +1               TO RET-TIKLOCK                         
108400              PERFORM IMS-ISRT-WLRETA01                                   
108500           END-PERFORM                                                    
108600        END-IF                                                            
108700        ADD +1                     TO W-IDKOLLI-NUM                       
108800     END-PERFORM                                                          
108900     MOVE JA                 TO  W-UPDATE-SW                              
109000                                                                          
109100     .                                                                    
109200     EJECT                                                                
109300 HBB-TA-BORT-VALT-RT-UR-KOLLI SECTION.                                    
109400     MOVE 'HBB-TA-BORT-VALT-RT-U' TO CURRENT-SECTION                      
109500                                                                          
109600     MOVE REQU-IDDISTR(INDX)       TO W-IDDISTR-FSEQ-MIN                  
109700                                      W-IDDISTR-FSEQ-MAX                  
109800     MOVE REQU-IDKUNDNR(INDX)      TO W-IDKUNDNR-FSEQ-MIN                 
109900                                      W-IDKUNDNR-FSEQ-MAX                 
110000     MOVE REQU-IDRAPPNR(INDX)      TO W-IDRAPPNR-FSEQ-MIN                 
110100                                      W-IDRAPPNR-FSEQ-MAX                 
110200     MOVE +0    TO W-ANTAL-KOLLI                                          
110300     PERFORM IMS-GHU-SEQF-WLRETA01                                        
110400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
110500        ADD +1          TO W-ANTAL-KOLLI                                  
110600        IF RET-IDKOLLI = W-IDKOLLI                                        
110700          MOVE RET-DAREGDAT          TO W-DAREGDAT                        
110800          MOVE RET-TIKLOCK           TO W-TIKLOCK                         
110900        END-IF                                                            
111000        PERFORM IMS-GHN-SEQF-WLRETA01                                     
111100     END-PERFORM                                                          
111200                                                                          
111300     PERFORM IMS-GHU-WLRETA01                                             
111400     IF SEGMENT-FINNS                                                     
111500       IF W-ANTAL-KOLLI > +1                                              
111600          PERFORM IMS-DLET-WLRETA01                                       
111700       ELSE                                                               
111800          MOVE ZERO         TO RET-IDKOLLI                                
111900                               RET-KDKOLSTA                               
112000          PERFORM IMS-REPL-WLRETA01                                       
112100       END-IF                                                             
112200     END-IF                                                               
112300     MOVE JA                 TO  W-UPDATE-SW                              
112400                                                                          
112500     .                                                                    
112600     EJECT                                                                
112700                                                                          
112800 HC-UPPDATERA-INMATAT-TILLSTAND  SECTION.                                 
112900     MOVE 'HC-UPPDATERA-INMATAT-' TO CURRENT-SECTION                      
113000                                                                          
113100     MOVE REQU-IDDISTR-IN          TO W-IDDISTR-FSEQ-MIN                  
113200                                      W-IDDISTR-FSEQ-MAX                  
113300     MOVE REQU-IDKUNDNR-IN         TO W-IDKUNDNR-FSEQ-MIN                 
113400                                      W-IDKUNDNR-FSEQ-MAX                 
113500     MOVE REQU-IDRAPPNR-IN         TO W-IDRAPPNR-FSEQ-MIN                 
113600                                      W-IDRAPPNR-FSEQ-MAX                 
113700     PERFORM IMS-GHU-SEQF-WLRETA01                                        
113800                                                                          
113900     IF SEGMENT-FINNS                                                     
114000        IF RET-IDKOLLI = ZERO                                             
114100          MOVE REQU-IDKOLLI-KEY      TO RET-IDKOLLI                       
114200          MOVE W-KLI-PACKAT          TO RET-KDKOLSTA                      
114300          PERFORM IMS-REPL-SEQF-WLRETA01                                  
114400        ELSE                                                              
114500          MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                
114600          ACCEPT RET-TIKLOCK  FROM TIME                                   
114700          MOVE REQU-IDKOLLI-KEY      TO RET-IDKOLLI                       
114800          PERFORM IMS-ISRT-WLRETA01                                       
114900          IF SEGMENT-FINNS-REDAN                                          
115000             PERFORM UNTIL SEGMENT-FINNS                                  
115100                ADD +1             TO RET-TIKLOCK                         
115200                PERFORM IMS-ISRT-WLRETA01                                 
115300             END-PERFORM                                                  
115400          END-IF                                                          
115500        END-IF                                                            
115600     ELSE                                                                 
115700        CALL FELLOG                                                       
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100                                                                          
116200 S01-REDIGERA-MOD          SECTION.                                       
116300     MOVE 'S01-REDIGERA-MOD     ' TO CURRENT-SECTION                      
116400                                                                          
116500     MOVE RET-IDDC                 TO RESP-IDDC-RETUR                     
116600     MOVE RET-IDDISTR              TO RESP-IDDISTR    (INDX)              
116700     MOVE RET-IDKUNDNR             TO RESP-IDKUNDNR    (INDX)             
116800     MOVE RET-IDRAPPNR             TO RESP-IDRAPPNR    (INDX)             
116900     MOVE RET-KVKOLLI-AAF          TO RESP-KVKOLLI-AAF (INDX)             
117000     MOVE RET-FLFARLIG             TO RESP-FLFARLIG    (INDX)             
117100     ADD  +1                       TO WS-COUNT                            
117200     MOVE WS-COUNT                 TO RESP-KVRADER                        
117300                                                                          
117400     IF WS-COUNT < 501                                                    
117500        CONTINUE                                                          
117600     ELSE                                                                 
117700        MOVE TOO-MANY-LINES  TO RESP-IDMSG-ERROR                          
117800     END-IF                                                               
117900     .                                                                    
118000     EJECT                                                                
118100 S02-KOLLA-KOLLISTATUS           SECTION.                                 
118200     MOVE 'S02-KOLLA-KOLLISTATUS' TO CURRENT-SECTION                      
118300                                                                          
118400     PERFORM S02A-KOLLA-IDKOLLI                                           
118500                                                                          
118600     PERFORM IMS-GU-WLRETC01                                              
118700                                                                          
118800     IF SEGMENT-FINNS                                                     
118900        MOVE SEQB-IDDC                 TO W-IDDC                          
118910        MOVE SEQB-DAREGDAT             TO W-DAREGDAT                      
119000        MOVE SEQB-TIKLOCK              TO W-TIKLOCK                       
119100        PERFORM IMS-GU-WLRETA01                                           
119200        MOVE JA                        TO SW-KOLLI                        
119300        MOVE RET-FLFARLIG              TO W-SPAR-FLFARLIG                 
119400        MOVE RET-IDDC                  TO W-SPAR-IDDC-RET                 
119500        IF RET-KDKOLSTA                NOT = W-KLI-PACKAT                 
119600            IF INM-RAD                                                    
119700               MOVE '995'              TO RESP-IDMSG-ERROR                
119800            ELSE                                                          
119900               MOVE +1                 TO INDX                            
120000               MOVE NEJ                TO WS-REC-LIMIT                    
120100               PERFORM UNTIL INDX      >  MAX-INDX  OR REC-LIMIT          
120200                  IF REQU-KDCMD(INDX)  =  W-PACKA                         
120300                   CONTINUE                                               
120400                  END-IF                                                  
120500                                                                          
120600                  IF INDX = WS-INDX-REC                                   
120700                     MOVE JA           TO WS-REC-LIMIT                    
120800                  ELSE                                                    
120900                     ADD +1            TO INDX                            
121000                  END-IF                                                  
121100               END-PERFORM                                                
121200            END-IF                                                        
121300            MOVE NEJ                   TO INDATA-SW                       
121400            MOVE '023'                 TO RESP-IDMSG-ERROR                
121500                              RESP-IDMSG-ERROR-LINE (INDX)                
121600            MOVE 'CMD'                 TO RESP-IDELMT-ERROR               
121700                                                                          
121800        END-IF                                                            
121900                                                                          
122000        MOVE RET-IDPERSON          TO W-IDPERSON                          
122100        MOVE RET-KDARBTYP          TO W-KDARBTYP                          
122110        MOVE REQU-IDDC-RET         TO W-IDDC                              
122200     END-IF                                                               
122300                                                                          
122400     .                                                                    
122500     EJECT                                                                
122600 S02A-KOLLA-IDKOLLI               SECTION.                                
122700     MOVE 'S02A-KOLLA-IDKOLLI   ' TO CURRENT-SECTION                      
122800                                                                          
122900     PERFORM IMS-GHU-WL411111                                             
123000     IF W-IDKOLLI                   >  4112-IDKOLLI                       
123100         IF INM-RAD                                                       
123200            MOVE NEJ                TO INDATA-SW                          
123300            MOVE '996'              TO RESP-IDMSG-ERROR                   
123400         ELSE                                                             
123500            MOVE +1                       TO INDX                         
123600            MOVE NEJ                      TO WS-REC-LIMIT                 
123700            PERFORM UNTIL INDX            >  MAX-INDX OR REC-LIMIT        
123800               IF REQU-KDCMD(INDX)        =  W-PACKA                      
123900                  MOVE NEJ                TO INDATA-SW                    
124000                  MOVE '023'              TO RESP-IDMSG-ERROR             
124100                                 RESP-IDMSG-ERROR-LINE (INDX)             
124200                  MOVE 'CMD'              TO RESP-IDELMT-ERROR            
124300               END-IF                                                     
124400                                                                          
124500                IF INDX = WS-INDX-REC                                     
124600                   MOVE JA           TO WS-REC-LIMIT                      
124700                ELSE                                                      
124800                   ADD +1            TO INDX                              
124900                END-IF                                                    
125000                                                                          
125100            END-PERFORM                                                   
125200         END-IF                                                           
125300     END-IF                                                               
125400                                                                          
125500     .                                                                    
125600     EJECT                                                                
125700*    --- DISPATCHER SECTIONS                                              
125800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
125900                                                                          
126000     MOVE 'GETARG'               TO SUB-KDFUNC                            
126100     MOVE 'CARPARTS.LDC.CONFIRMCASES'        TO SUB-ADDISPABS             
126200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
126300                                                                          
126400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
126500                                                                          
126600     IF SUB-KDRC > 0                                                      
126700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
126800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
126900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
127000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
127100     END-IF                                                               
127200     .                                                                    
127300     SKIP3                                                                
127400 S02-RETURN-RESPONSE SECTION.                                             
127500                                                                          
127600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
127700     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
127800                                                                          
127900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
128000                                                                          
128100     IF SUB-KDRC > 0                                                      
128200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
128300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
128400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
128500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
128600     END-IF                                                               
128700     .                                                                    
128800     EJECT                                                                
128900* --- IMS SEKTIONER ---                                                   
129000     SKIP3                                                                
129100 IMS-GU-SEQA-WLRETA01       SECTION.                                      
129200     MOVE 'IMS-GU-SEQA-WLRETA01 ' TO CURRENT-IMS-SECTION                  
129300                                                                          
129400     STRING 'WLRETA01(WDA3ASEQ>=' W-WDA3ASEQ-MIN-X                        
129500                    '&WDA3ASEQ<=' W-WDA3ASEQ-MAX-X ')'                    
129600          DELIMITED BY SIZE INTO SSA1                                     
129700     MOVE '  GE'           TO GODK-STATUSKODER                            
129800     CALL CBLTDLI USING GU RETA1-PCB DLI-IO-AREA SSA1                     
129900     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200                                                                          
130300 IMS-GN-SEQA-WLRETA01 SECTION.                                            
130400     MOVE 'IMS-GN-SEQA-WLRETA01 ' TO CURRENT-IMS-SECTION                  
130500                                                                          
130600     STRING 'WLRETA01(WDA3ASEQ>=' W-WDA3ASEQ-MIN-X                        
130700                    '&WDA3ASEQ<=' W-WDA3ASEQ-MAX-X ')'                    
130800          DELIMITED BY SIZE INTO SSA1                                     
130900     MOVE '  GE' TO GODK-STATUSKODER                                      
131000     CALL CBLTDLI USING GN RETA1-PCB DLI-IO-AREA SSA1                     
131100     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
131200     PERFORM IMS-STATUSKONTROLL                                           
131300     .                                                                    
131400     EJECT                                                                
131500 IMS-GU-SEQB-WLRETA01       SECTION.                                      
131600     MOVE 'IMS-GU-SEQB-WLRETA01 ' TO CURRENT-IMS-SECTION                  
131700                                                                          
131800     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
131900                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X                        
132000                    '&IDKOLLI  =' W-IDKOLLI-X      ')'                    
132100          DELIMITED BY SIZE INTO SSA1                                     
132200     MOVE '  GE'           TO GODK-STATUSKODER                            
132300     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-AREA SSA1                     
132400     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
132500     PERFORM IMS-STATUSKONTROLL                                           
132600     .                                                                    
132700                                                                          
132800 IMS-GN-SEQB-WLRETA01 SECTION.                                            
132900     MOVE 'IMS-GN-SEQB-WLRETA01 ' TO CURRENT-IMS-SECTION                  
133000                                                                          
133100     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
133200                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X                        
133300                    '&IDKOLLI  =' W-IDKOLLI-X      ')'                    
133400          DELIMITED BY SIZE INTO SSA1                                     
133500     MOVE '  GE' TO GODK-STATUSKODER                                      
133600     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-AREA SSA1                     
133700     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
133800     PERFORM IMS-STATUSKONTROLL                                           
133900     .                                                                    
134000     EJECT                                                                
134100                                                                          
134200 IMS-GHU-SEQF-WLRETA01       SECTION.                                     
134300     MOVE 'IMS-GHU-SEQF-WLRETA01' TO CURRENT-IMS-SECTION                  
134400                                                                          
134500     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
134600                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
134700          DELIMITED BY SIZE INTO SSA1                                     
134800     MOVE '  GE'           TO GODK-STATUSKODER                            
134900     CALL CBLTDLI USING GHU RETA3-PCB DLI-IO-AREA SSA1                    
135000     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
135100     PERFORM IMS-STATUSKONTROLL                                           
135200     .                                                                    
135300                                                                          
135400                                                                          
135500 IMS-GHN-SEQF-WLRETA01       SECTION.                                     
135600     MOVE 'IMS-GHN-SEQF-WLRETA01' TO CURRENT-IMS-SECTION                  
135700                                                                          
135800     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
135900                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
136000          DELIMITED BY SIZE INTO SSA1                                     
136100     MOVE '  GEGB'         TO GODK-STATUSKODER                            
136200     CALL CBLTDLI USING GHN RETA3-PCB DLI-IO-AREA SSA1                    
136300     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
136400     PERFORM IMS-STATUSKONTROLL                                           
136500     .                                                                    
136600                                                                          
136700                                                                          
136800 IMS-REPL-SEQF-WLRETA01      SECTION.                                     
136900     MOVE 'IMS-REPL-SEQF-WLRETA01' TO CURRENT-IMS-SECTION                 
137000                                                                          
137100     MOVE '    '           TO GODK-STATUSKODER                            
137200     CALL CBLTDLI USING REPL RETA3-PCB DLI-IO-AREA                        
137300     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
137400     PERFORM IMS-STATUSKONTROLL                                           
137500     .                                                                    
137600     EJECT                                                                
137700                                                                          
137800 IMS-GU-WLRETA01       SECTION.                                           
137900     MOVE 'IMS-GU-WLRETA01       ' TO CURRENT-IMS-SECTION                 
138000                                                                          
138100     STRING 'WLRETA01(WDA301KY>=' W-WDA301KY-X ')'                        
138200          DELIMITED BY SIZE INTO SSA1                                     
138300     MOVE '  '           TO GODK-STATUSKODER                              
138400     CALL CBLTDLI USING GU RETA4-PCB DLI-IO-AREA SSA1                     
138500     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
138600     PERFORM IMS-STATUSKONTROLL                                           
138700     .                                                                    
138800                                                                          
138900 IMS-GHU-WLRETA01      SECTION.                                           
139000     MOVE 'IMS-GHU-WLRETA01      ' TO CURRENT-IMS-SECTION                 
139100                                                                          
139200     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
139300          DELIMITED BY SIZE INTO SSA1                                     
139400     MOVE '  GE'         TO GODK-STATUSKODER                              
139500     CALL CBLTDLI USING GHU RETA4-PCB DLI-IO-AREA SSA1                    
139600     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900                                                                          
140000 IMS-ISRT-WLRETA01    SECTION.                                            
140100     MOVE 'IMS-ISRT-WLRETA01     ' TO CURRENT-IMS-SECTION                 
140200                                                                          
140300     MOVE 'WLRETA01 ' TO SSA1                                             
140400     MOVE '  II' TO GODK-STATUSKODER                                      
140500     CALL CBLTDLI USING ISRT RETA4-PCB DLI-IO-AREA SSA1                   
140600     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900     SKIP2                                                                
141000 IMS-REPL-WLRETA01    SECTION.                                            
141100     MOVE 'IMS-REPL-WLRETA01     ' TO CURRENT-IMS-SECTION                 
141200                                                                          
141300     MOVE '  ' TO GODK-STATUSKODER                                        
141400     CALL CBLTDLI USING REPL RETA4-PCB DLI-IO-AREA                        
141500     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
141600     PERFORM IMS-STATUSKONTROLL                                           
141700     .                                                                    
141800     SKIP2                                                                
141900 IMS-DLET-WLRETA01    SECTION.                                            
142000     MOVE 'IMS-DLET-WLRETA01     ' TO CURRENT-IMS-SECTION                 
142100                                                                          
142200     MOVE '  ' TO GODK-STATUSKODER                                        
142300     CALL CBLTDLI USING DLET RETA4-PCB DLI-IO-AREA                        
142400     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
142500     PERFORM IMS-STATUSKONTROLL                                           
142600     .                                                                    
142700     EJECT                                                                
142800 IMS-GHU-WL411111  SECTION.                                               
142900     MOVE 'IMS-GHU-WL411111      ' TO CURRENT-IMS-SECTION                 
143000                                                                          
143100     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-X ')'                         
143200                      DELIMITED BY SIZE INTO SSA1                         
143300     MOVE  'WL411111 ' TO SSA2                                            
143400     MOVE '  ' TO GODK-STATUSKODER                                        
143500     CALL CBLTDLI USING GHU 4111-PCB DLI-IO-WDGX4112 SSA1 SSA2            
143600     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
143700     PERFORM IMS-STATUSKONTROLL                                           
143800     .                                                                    
143900     SKIP2                                                                
144000 IMS-REPL-WL411111  SECTION.                                              
144100     MOVE 'IMS-REPL-WL411111     ' TO CURRENT-IMS-SECTION                 
144200                                                                          
144300     MOVE '  ' TO GODK-STATUSKODER                                        
144400     CALL CBLTDLI USING REPL 4111-PCB DLI-IO-WDGX4112                     
144500     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
144600     PERFORM IMS-STATUSKONTROLL                                           
144700     .                                                                    
144800     SKIP2                                                                
144900 IMS-GU-WLRETC01       SECTION.                                           
145000     MOVE 'IMS-GU-WLRETC01       ' TO CURRENT-IMS-SECTION                 
145100                                                                          
145200     STRING 'WLRETC01(WDA3B1KY>=' W-WDA3B1KY-MIN-X                        
145300                    '&WDA3B1KY<=' W-WDA3B1KY-MAX-X                        
145400                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
145500          DELIMITED BY SIZE INTO SSA1                                     
145600     MOVE '  GE'           TO GODK-STATUSKODER                            
145700     CALL CBLTDLI USING GU RETC-PCB DLI-IO-AREA SSA1                      
145800     MOVE RETC-STATUS-CODE TO STATUS-WS                                   
145900     PERFORM IMS-STATUSKONTROLL                                           
146000     .                                                                    
146100                                                                          
146200 IMS-STATUSKONTROLL SECTION.                                              
146300                                                                          
146400     SET STATUS-IX TO 1                                                   
146500     SEARCH GODK-STATUS                                                   
146600       AT END                                                             
146700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
146800         DELIMITED BY SIZE INTO FELTEXT                                   
146900         CALL FELLOG                                                      
147000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
147100         CONTINUE                                                         
147200     END-SEARCH                                                           
147300     .                                                                    
