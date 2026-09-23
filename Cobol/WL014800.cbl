000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL014800.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/07/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.RETURNREG'                                 
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        FUNKTION:                                                        
001100*        REGISTRERING AV MOTTAGNA RETURER HOS CDC, LDC OCH                
001200*        NDC.                                                             
001300*        MESSGE REQUIRED FOR HILIGHTED FIELDS ERROR                       
001400*        NDC.                                                             
001500*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001600*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001700*        PROGRAMMET UPPDATERAR WL4111 (WDR1)                              
001800*        PROGRAMMET LÄSER      WLRETG (WDA3)                              
001900*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
002000*        PROGRAMMET LÄSER      WL4103 (WDR1)                              
002100*                                                                         
002200*        WL014800 PROGRAM IS A REPLICA OF W4073300 PROGRAM                
002300*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: WL0148U                                             
002700*        REQUEST:     WL0148I1                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        RESPONSE:    WL0148O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'WL014800'.            
004500                                                                          
004600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004800 77   FELTEXT                    PIC X(80) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  W-CDC                       PIC X(3)    VALUE 'CDC'.                 
005400 77  W-US1                       PIC X(3)    VALUE 'US1'.                 
005500 77  W-US2                       PIC X(3)    VALUE 'US2'.                 
005600 77  W-US3                       PIC X(3)    VALUE 'US3'.                 
005700 77  W-CA1                       PIC X(3)    VALUE 'CA1'.                 
005800 77  W-JP1                       PIC X(3)    VALUE 'JP1'.                 
005900 77  W-AU1                       PIC X(3)    VALUE 'AU1'.                 
006000 77  W-SE1                       PIC X(3)    VALUE 'SE1'.                 
006100 77  W-GB1                       PIC X(3)    VALUE 'GB1'.                 
006200 77  W-SE2                       PIC X(3)    VALUE 'SE2'.                 
006300 77  W-GB2                       PIC X(3)    VALUE 'GB2'.                 
006400 77  W-GB3                       PIC X(3)    VALUE 'GB3'.                 
006500 77  W-NL1                       PIC X(3)    VALUE 'NL1'.                 
006600 77  W-IT1                       PIC X(3)    VALUE 'IT1'.                 
006700 77  W-PLUS                      PIC X       VALUE '+'.                   
006800                                                                          
006900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
007000 77  W-IX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007200 77  INDX2                       PIC S9(4)  VALUE +0    COMP SYNC.        
007300 77  MAX-INDX                    PIC S9(4)  VALUE +8    COMP SYNC.        
007400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007500                                                                          
007600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007700     88  INDATA-OK                           VALUE 'J'.                   
007800     88  INDATA-FEL                          VALUE 'N'.                   
007900                                                                          
008000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008100     88  NYCKLAR-OK                          VALUE 'J'.                   
008200     88  NYCKLAR-FEL                         VALUE 'N'.                   
008300                                                                          
008400 77  W-FLFARLIG                  PIC X(1).                                
008410 77  W-FLBUYBAC                  PIC X(1)    VALUE 'N'.                   
008500 77  W-IDPERSON                  PIC S9(3)   COMP-3.                      
008600 77  W-KDARBTYP                  PIC X(8).                                
008700 77  W-IDRTLOP-NUM               PIC  9(3)          VALUE ZERO.           
008800 77  W-KVRADER                   PIC S9(5)   COMP-3 VALUE ZERO.           
008900 77  W-SND-SAENT                 PIC X(1)           VALUE '2'.            
009000 77  W-SND-LOSS                  PIC X(1)           VALUE '3'.            
009100 77  W-KLI-SAENT                 PIC S9(1)   COMP-3 VALUE +4.             
009200 77  W-KLI-SAK                   PIC S9(1)   COMP-3 VALUE +6.             
009300 77  W-KLI-AVV                   PIC S9(1)   COMP-3 VALUE +7.             
009400 77  W-KVKOLLI-LOSS              PIC S9(5)   COMP-3 VALUE ZERO.           
009500 77  W-KVKOLLI-MOT               PIC S9(5)   COMP-3 VALUE ZERO.           
009600 77  SPAR-IDKOLLI                PIC S9(5)   COMP-3 VALUE ZERO.           
009700 77  WS-IDKOLLI-FOM              PIC S9(5)   COMP-3 VALUE ZERO.           
009800 77  WS-IDKOLLI-TOM              PIC S9(5)   COMP-3 VALUE ZERO.           
009900 77  SPAR-KDRETSTA               PIC  X(1)          VALUE SPACE.          
010000 77  SPAR-KDKOLSTA               PIC S9(1)   COMP-3 VALUE +0.             
010100                                                                          
010200 77  SW-FOERSTA-RT               PIC X       VALUE 'N'.                   
010300     88  FOERSTA-RT                          VALUE 'J'.                   
010400                                                                          
010500 77  SW-GAMMAL-SND               PIC X       VALUE 'N'.                   
010600     88  GAMMAL-SND                          VALUE 'J'.                   
010700                                                                          
010800 77  SW-SKAPA-NY-SND             PIC X       VALUE 'N'.                   
010900     88  SKAPA-NY-SND                        VALUE 'J'.                   
011000                                                                          
011100 77  SW-LOSS-INFO                PIC X       VALUE 'N'.                   
011200     88  LOSS-INFO                           VALUE 'J'.                   
011300                                                                          
011400 77  WWW                        PIC X(4)    VALUE 'HERE'.                 
011500 77  SW-KOLLI-INFO               PIC X       VALUE 'N'.                   
011600     88  KOLLI-INFO                          VALUE 'J'.                   
011700                                                                          
011800 77  SW-RAD-INFO                 PIC X       VALUE 'N'.                   
011900     88  RAD-INFO                            VALUE 'J'.                   
012000                                                                          
012100 77  WS-IDELMT-ERROR             PIC X(16)  VALUE SPACE.                  
012200 77  WS-IDMSG-ERROR              PIC X(03)  VALUE SPACE.                  
012300 77  WS-IDMSG-INFO               PIC X(03)  VALUE SPACE.                  
012400 77  W-UPDATE-SW                PIC X       VALUE 'N'.                    
012500     88   W-UPDATE-OK                          VALUE 'J'.                 
012510                                                                          
012520 01  W-KDANMORS                  PIC X(2).                                
012530     88  KDANMORS-BUYBAC-98                  VALUE '98'.                  
012540                                                                          
012600                                                                          
012700 01  -COPY WDA301    -PRE SPAR-                                           
012800                                                                          
012900     EJECT                                                                
013000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
013100 01  GENERAL-SUBPROGRAMS.                                                 
013200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
013500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013600     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
013700     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
013800     SKIP3                                                                
013900*    --- PARAMETERS TO ABEND                                              
014000                                                                          
014100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014400     EJECT                                                                
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014700     SKIP3                                                                
014800*01  -COPY WZ01SUB                                                        
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015100     SKIP3                                                                
015200 01  REQU-AREA.                                                           
015300*    03  -COPY WZ01REQU                                                   
015400*    03  -COPY WL0148I1                                                   
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015700     SKIP3                                                                
015800 01  RESP-AREA.                                                           
015900*    03  -COPY WZ01RESP                                                   
016000*    03  -COPY WL0148O1                                                   
016100     EJECT                                                                
016200*    --- PARAMETRAR TILL SUBPROGRAM W418ANSV                              
016300*01 -COPY W418ANSV                                                        
016400     EJECT                                                                
016500*    --- PARAMETRAR TILL SUBPROGRAM W418OKOD                              
016600*01 -COPY W418OKOD -PRE  OKOD-                                            
016700     EJECT                                                                
016800     SKIP3                                                                
016900 01  MESSAGE-CODES.                                                       
017000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
017100     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '187'.                 
017200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
017300     03  SYS-ERR                 PIC X(3)    VALUE '099'.                 
017400     EJECT                                                                
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600*                                                                         
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017900     SKIP3                                                                
018000 01  NYCKLAR-TILL-DLI.                                                    
018100     03  W-IDKOLLI-X.                                                     
018200         05  W-IDKOLLI           PIC S9(5)    VALUE ZERO  COMP-3.         
018300                                                                          
018400     03  W-WDA301KY-X.                                                    
018500         05  W-IDDC              PIC  X(2)    VALUE SPACE.                
018600         05  W-DAREGDAT          PIC  9(8)    VALUE ZERO.                 
018700         05  W-TIKLOCK           PIC S9(9)    VALUE ZERO  COMP-3.         
018800                                                                          
018900     03  W-WDA3F1KY-MIN-X.                                                
019000         05  W-IDDC-MIN          PIC  X(2)    VALUE SPACE.                
019100         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
019200         05  W-IDKUNDNR-MIN      PIC S9(7)    VALUE ZERO  COMP-3.         
019300         05  W-IDRAPPNR-MIN      PIC  9(7)    VALUE ZERO.                 
019400         05  W-IDRT-MIN          PIC  X(3)    VALUE SPACE.                
019500         05  W-IDRTLOP-MIN       PIC  9(3)    VALUE ZERO.                 
019600         05  W-IDKOLLI-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
019700         05  W-DAREGDAT-MIN      PIC  9(8)    VALUE ZERO.                 
019800         05  W-TIKLOCK-MIN       PIC S9(9)    VALUE ZERO  COMP-3.         
019900                                                                          
020000     03  W-WDA3F1KY-MAX-X.                                                
020100         05  W-IDDC-MAX          PIC  X(2)    VALUE SPACE.                
020200         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
020300         05  W-IDKUNDNR-MAX      PIC S9(7)    VALUE ZERO  COMP-3.         
020400         05  W-IDRAPPNR-MAX      PIC  9(7)    VALUE ZERO.                 
020500         05  W-IDRT-MAX          PIC  X(3)    VALUE SPACE.                
020600         05  W-IDRTLOP-MAX       PIC  9(3)    VALUE ZERO.                 
020700         05  W-IDKOLLI-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
020800         05  W-DAREGDAT-MAX      PIC  9(8)    VALUE ZERO.                 
020900         05  W-TIKLOCK-MAX       PIC S9(9)    VALUE ZERO  COMP-3.         
021000                                                                          
021100     03  W-WDA3BSEQ-MIN-X.                                                
021200         05  W-IDRT-BSEQ-MIN      PIC  X(3)          VALUE SPACE.         
021300         05  W-IDDC-BSEQ-MIN      PIC  X(2)          VALUE SPACE.         
021400         05  W-IDRTLOP-BSEQ-MIN   PIC  9(3)          VALUE ZERO.          
021500         05  W-IDKOLLI-BSEQ-MIN   PIC S9(5)   COMP-3 VALUE ZERO.          
021600                                                                          
021700     03  W-WDA3BSEQ-MAX-X.                                                
021800         05  W-IDRT-BSEQ-MAX      PIC  X(3)          VALUE SPACE.         
021900         05  W-IDDC-BSEQ-MAX      PIC  X(2)          VALUE SPACE.         
022000         05  W-IDRTLOP-BSEQ-MAX   PIC  9(3)          VALUE ZERO.          
022100         05  W-IDKOLLI-BSEQ-MAX   PIC S9(5)   COMP-3 VALUE ZERO.          
022200                                                                          
022300     03  W-IDLEVANM-X.                                                    
022400         05  W-IDDISTR-ANM       PIC S9(5)    VALUE ZERO  COMP-3.         
022500         05  W-IDKUNDNR-ANM      PIC S9(7)    VALUE ZERO  COMP-3.         
022600         05  W-IDRAPPNR-ANM      PIC  9(7)    VALUE ZERO.                 
022700                                                                          
022800     03  W-WDGXKEY-X.                                                     
022900         05  W-IDHTYP            PIC  X(4)   VALUE '4111'.                
023000         05  W-IDRT-4111         PIC  X(3)   VALUE SPACE.                 
023100         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
023200                                                                          
023300     SKIP2                                                                
023400*    --- STATUS-KOD FRÅN IMS                                              
023500 01  STATUS-WS                   PIC XX.                                  
023600     88  SEGMENT-FINNS                       VALUE '  '.                  
023700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024000     SKIP2                                                                
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400 01  SSA1                        PIC X(128).                              
024500 01  SSA2                        PIC X(192).                              
024600     EJECT                                                                
024700*    --- IMS FUNKTIONSKODER                                               
024800*01  -COPY W0003                                                          
024900     EJECT                                                                
025000*    ---  DLI INPUT-OUTPUT AREA                                           
025100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025200     SKIP3                                                                
025300 01  DLI-IO-AREA.                                                         
025400     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
025500     SKIP3                                                                
025600     03  WLRETA01 REDEFINES IO-AREA.                                      
025700*        05  -COPY WDA301                                                 
025800     EJECT                                                                
025900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
026000     SKIP3                                                                
026100 01  DLI-IO-AREA2.                                                        
026200     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
026300     SKIP3                                                                
026400     03  WLRETG01 REDEFINES IO-AREA2.                                     
026500*        05  -COPY WDA3F1                                                 
026600     EJECT                                                                
026700     03  WLKREE01 REDEFINES IO-AREA2.                                     
026800*        05  -COPY WDA201                                                 
026900     EJECT                                                                
027000     03  WLKREE11 REDEFINES IO-AREA2.                                     
027100*        05  -COPY WDA211                                                 
027200     EJECT                                                                
027300     SKIP3                                                                
027400 01  FILLER                    PIC X(16) VALUE 'WDGX4111-AREA'.           
027500 01  WL411101  -COPY WDGX4111                                             
027600     EJECT                                                                
027700 01  FILLER                    PIC X(16) VALUE 'WDGX4112-AREA'.           
027800 01  WL411111  -COPY WDGX4112                                             
027900                                                                          
028000     EJECT                                                                
028100 LINKAGE SECTION.                                                         
028200 01  MSG-PCB                     PIC X.                                   
028300     EJECT                                                                
028400*01  -COPY W0008  -PRE RETA1-                                             
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700*01  -COPY W0008  -PRE RETA2-                                             
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000*01  -COPY W0008  -PRE RETG-                                              
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01  -COPY W0008  -PRE KREE-                                              
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE 4111-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE 4113-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE 4115-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE 4117-                                              
030600     05  FILLER                  PIC X.                                   
030700 PROCEDURE DIVISION  USING MSG-PCB  RETA1-PCB RETA2-PCB                   
030800                           RETG-PCB KREE-PCB 4111-PCB                     
030900                           4113-PCB 4115-PCB 4117-PCB.                    
031000 MAIN SECTION.                                                            
031100     ENTRY 'DLITCBL' USING MSG-PCB  RETA1-PCB RETA2-PCB                   
031200                           RETG-PCB KREE-PCB 4111-PCB                     
031300                           4113-PCB 4115-PCB 4117-PCB.                    
031400                                                                          
031500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
031600     IF SUB-KDRC = 0                                                      
031700       IF REQU-KDPGMACT = 'S' OR 'E'  OR 'N'                              
031800         PERFORM A-INIT                                                   
031900         PERFORM B-KOLLA-NYCKLAR                                          
032000         IF NYCKLAR-OK                                                    
032100           IF REQU-KDPGMACT = 'E' OR 'N'                                  
032200             PERFORM G-KOLLA-INPUT                                        
032300             IF INDATA-OK                                                 
032400               PERFORM H-UPPDATERA                                        
032500             END-IF                                                       
032600           END-IF                                                         
032700           IF INDATA-OK                                                   
032800             PERFORM F-LAES-VISA-INFO                                     
032900           END-IF                                                         
033000         END-IF                                                           
033100       ELSE                                                               
033200         MOVE SYS-ERR      TO RESP-IDMSG-ERROR                            
033300       END-IF                                                             
033400       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
033500       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
033600       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
033700       IF WS-IDMSG-ERROR NOT = SPACE                                      
033800         MOVE ALL '+' TO RESP-IDDC-KEY                                    
033900         MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                        
034000         MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                       
034100         MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                         
034200         MOVE 001              TO RESP-IDMSGVER                           
034300       END-IF                                                             
034400       PERFORM S02-RETURN-RESPONSE                                        
034500     END-IF                                                               
034600                                                                          
034700     MOVE ZERO TO RETURN-CODE                                             
034800     GOBACK                                                               
034900     .                                                                    
035000                                                                          
035100 A-INIT SECTION.                                                          
035200                                                                          
035300     MOVE ALL '+'       TO RESP-AREA                                      
035400     MOVE 001           TO RESP-IDMSGVER                                  
035500     MOVE SPACE         TO RESP-IDMSG-ERROR                               
035600                           RESP-IDMSG-INFO                                
035700                           RESP-IDELMT-ERROR                              
035800                                                                          
035900     MOVE  1            TO W-IX                                           
036000     PERFORM UNTIL W-IX > MAX-INDX                                        
036100       MOVE ALL '+'     TO RESP-RADER (W-IX)                              
036200       MOVE SPACE       TO RESP-IDMSG-ERROR-LINE (W-IX)                   
036300       ADD 1            TO W-IX                                           
036400     END-PERFORM                                                          
036500                                                                          
036600     IF REQU-KDPGMACT = 'S'                                               
036700       MOVE SPACE       TO RESP-WL0148O1                                  
036800     END-IF                                                               
036900     MOVE LOW-VALUE     TO W-WDA3F1KY-MIN-X                               
037000                           W-WDA3BSEQ-MIN-X                               
037100     MOVE HIGH-VALUE    TO W-WDA3F1KY-MAX-X                               
037200                           W-WDA3BSEQ-MAX-X                               
037300     .                                                                    
037400     EJECT                                                                
037500 B-KOLLA-NYCKLAR SECTION.                                                 
037600                                                                          
037700     MOVE JA TO NYCKLAR-SW                                                
037800                                                                          
037900     IF REQU-KDPGMACT NOT = 'N'                                           
038000       PERFORM BA-KOLLA-IDRETSND                                          
038100     END-IF                                                               
038200                                                                          
038300     MOVE REQU-IDRT-KEY    TO W-IDRT-4111                                 
038400     PERFORM IMS-GU-WL411101                                              
038500     IF SEGMENT-FINNS                                                     
038600       MOVE REQU-IDRT-KEY  TO W-IDRT-BSEQ-MIN                             
038700                              W-IDRT-BSEQ-MAX                             
038800     ELSE                                                                 
038900       MOVE NEJ            TO NYCKLAR-SW                                  
039000       MOVE 'IDRT'         TO RESP-IDELMT-ERROR                           
039100       MOVE '023'          TO RESP-IDMSG-ERROR                            
039200     END-IF                                                               
039300     MOVE REQU-IDRT-KEY    TO RESP-IDRT-KEY                               
039400                                                                          
039500     MOVE REQU-IDDC-KEY      TO W-IDDC                                    
039600                                W-IDDC-MIN                                
039700                                W-IDDC-MAX                                
039800                                W-IDDC-BSEQ-MIN                           
039900                                W-IDDC-BSEQ-MAX                           
040000                                RESP-IDDC-KEY                             
040100     .                                                                    
040200                                                                          
040300 BA-KOLLA-IDRETSND   SECTION.                                             
040400                                                                          
040500     IF REQU-IDRTLOP-KEY     NUMERIC AND                                  
040600        REQU-IDRTLOP-KEY  > ZERO                                          
040700       MOVE REQU-IDRTLOP-KEY TO W-IDRTLOP-BSEQ-MIN                        
040800                                W-IDRTLOP-BSEQ-MAX                        
040900     ELSE                                                                 
041000       MOVE NEJ            TO NYCKLAR-SW                                  
041100       MOVE 'IDRT-IDRTLOP' TO RESP-IDELMT-ERROR                           
041200       MOVE '023'          TO RESP-IDMSG-ERROR                            
041300     END-IF                                                               
041400     MOVE REQU-IDRTLOP-KEY TO RESP-IDRTLOP-KEY                            
041500     .                                                                    
041600     EJECT                                                                
041700 F-LAES-VISA-INFO SECTION.                                                
041800                                                                          
041900     PERFORM IMS-GHU-SEQB-WLRETA01                                        
042000                                                                          
042100     IF SEGMENT-SAKNAS AND REQU-KDPGMACT NOT = 'N'                        
042200       MOVE '025'              TO RESP-IDMSG-ERROR                        
042300       MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                       
042400       IF SKAPA-NY-SND                                                    
042500         CONTINUE                                                         
042600       END-IF                                                             
042700     ELSE                                                                 
042800       IF REQU-KDPGMACT NOT = 'N'                                         
042900       IF RET-IDRT = 'CDC' OR 'US1' OR 'US2' OR 'US3' OR 'CA1' OR         
043000                     'JP1' OR 'AU1' OR 'SE1' OR 'GB1' OR                  
043100                     'SE2' OR 'GB2' OR 'NL1' OR 'IT1' OR 'GB3'            
043200         MOVE RET-KVKOLLI-MOT   TO RESP-KVKOLLI-MOT                       
043300       ELSE                                                               
043400         PERFORM S03-RAEKNA-KOLLI                                         
043500         IF W-KVKOLLI-MOT  > ZERO                                         
043600           MOVE W-KVKOLLI-MOT   TO RESP-KVKOLLI-MOT                       
043700         END-IF                                                           
043800       END-IF                                                             
043900                                                                          
044000       MOVE RET-IDANSTNR-MOT    TO RESP-IDANSTNR-MOT                      
044100       MOVE RET-ADINLOMR-MOT    TO RESP-ADINLOMR-MOT                      
044200       MOVE SPACE               TO RESP-IDFRASED-MOT                      
044300                                                                          
044400       MOVE ZERO                TO RESP-KVKOLLI-LOSS-UPD                  
044500                                   RESP-IDANSTNR-LOSS-UPD                 
044600       MOVE SPACE               TO RESP-ADINLOMR-LOSS-UPD                 
044700                                   RESP-IDFRASED-LOSS-UPD                 
044800     END-IF                                                               
044900     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200 G-KOLLA-INPUT SECTION.                                                   
045300                                                                          
045400     MOVE JA                     TO INDATA-SW                             
045500     IF REQU-INPUT = ALL '+'  AND  REQU-KDPGMACT NOT = 'N'                
045600       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
045700       MOVE NEJ                  TO INDATA-SW                             
045800     ELSE                                                                 
045900       PERFORM GA-FORMELL-KONTROLL                                        
046000       IF INDATA-OK                                                       
046100         PERFORM GB-LOGISK-KONTROLL                                       
046200       END-IF                                                             
046300       IF INDATA-OK AND (REQU-KDPGMACT = 'E' OR 'N')                      
046400         IF REQU-KVKOLLI-LOSS-UPD NOT  = ALL '+'                          
046500           MOVE ZERO                 TO RESP-KVKOLLI-LOSS-UPD             
046600         ELSE                                                             
046700           MOVE REQU-KVKOLLI-LOSS-UPD(1:) TO                              
046800                                        RESP-KVKOLLI-LOSS-UPD(1:)         
046900           INSPECT RESP-KVKOLLI-LOSS-UPD                                  
047000                                 REPLACING LEADING ZERO BY SPACE          
047100         END-IF                                                           
047200                                                                          
047300         IF REQU-IDANSTNR-LOSS-UPD  = ALL '+'                             
047400           MOVE ZERO                   TO RESP-IDANSTNR-LOSS-UPD          
047500         ELSE                                                             
047600           MOVE REQU-IDANSTNR-LOSS-UPD TO RESP-IDANSTNR-LOSS-UPD          
047700         END-IF                                                           
047800                                                                          
047900         MOVE REQU-ADINLOMR-LOSS-UPD TO RESP-ADINLOMR-LOSS-UPD            
048000         MOVE REQU-IDFRASED-LOSS-UPD TO RESP-IDFRASED-LOSS-UPD            
048100                                                                          
048200         IF REQU-IDKOLLI  NOT = ALL '+'                                   
048300           MOVE REQU-IDKOLLI(1:)       TO RESP-IDKOLLI(1:)                
048400           INSPECT  RESP-IDKOLLI REPLACING LEADING ZERO BY SPACE          
048500         END-IF                                                           
048600         IF REQU-IDDISTR  NOT = ALL '+'                                   
048700           MOVE REQU-IDDISTR(1:)       TO RESP-IDDISTR(1:)                
048800           INSPECT RESP-IDDISTR  REPLACING LEADING ZERO BY SPACE          
048900         END-IF                                                           
049000                                                                          
049100         IF REQU-IDKOLLI-FOM  NOT = ALL '+'                               
049200           MOVE REQU-IDKOLLI-FOM(1:)   TO RESP-IDKOLLI-FOM(1:)            
049300          INSPECT RESP-IDKOLLI-FOM REPLACING LEADING ZERO BY SPACE        
049400         END-IF                                                           
049500                                                                          
049600         IF REQU-IDKOLLI-TOM  NOT = ALL '+'                               
049700           MOVE REQU-IDKOLLI-TOM(1:)   TO RESP-IDKOLLI-TOM(1:)            
049800          INSPECT RESP-IDKOLLI-TOM REPLACING LEADING ZERO BY SPACE        
049900         END-IF                                                           
050000       END-IF                                                             
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400 GA-FORMELL-KONTROLL   SECTION.                                           
050500                                                                          
050600     PERFORM GAA-KOLLA-NY-SND                                             
050700     PERFORM GAB-KOLLA-LOSS-INFO                                          
050800     PERFORM GAD-KOLLA-KOLLI-INFO                                         
050900     PERFORM GAE-KOLLA-RAD-INFO                                           
051000     PERFORM GAF-RELATIONS-KONTROLL                                       
051100     .                                                                    
051200                                                                          
051300                                                                          
051400 GAA-KOLLA-NY-SND      SECTION.                                           
051500                                                                          
051600     MOVE NEJ                     TO SW-SKAPA-NY-SND                      
051700                                                                          
051800     IF REQU-KDPGMACT              NOT = 'E'                              
051900       IF REQU-KDPGMACT            =  'N'                                 
052000         MOVE JA                   TO SW-SKAPA-NY-SND                     
052100       ELSE                                                               
052200         MOVE NEJ                  TO INDATA-SW                           
052300         MOVE '099'                TO RESP-IDMSG-ERROR                    
052400         MOVE 'KDPGMACT'           TO RESP-IDELMT-ERROR                   
052500       END-IF                                                             
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000 GAB-KOLLA-LOSS-INFO   SECTION.                                           
053100                                                                          
053200     MOVE NEJ                          TO SW-LOSS-INFO                    
053300                                                                          
053400     IF REQU-LOSS                      NOT = ALL '+'                      
053500       MOVE JA                        TO SW-LOSS-INFO                     
053600                                                                          
053700       PERFORM GABA-KOLLA-KVKOLLI-LOSS                                    
053800       PERFORM GABB-KOLLA-IDANSTNR-LOSS                                   
053900       PERFORM GABC-KOLLA-ADINLOMR-LOSS                                   
054000       PERFORM GABD-KOLLA-IDFRASED-LOSS                                   
054100     END-IF                                                               
054200     .                                                                    
054300                                                                          
054400 GABA-KOLLA-KVKOLLI-LOSS SECTION.                                         
054500                                                                          
054600     IF REQU-KVKOLLI-LOSS-UPD          NOT = ALL '+'                      
054700       IF REQU-KVKOLLI-LOSS-UPD       NUMERIC                             
054800         CONTINUE                                                         
054900       ELSE                                                               
055000         MOVE NEJ                    TO INDATA-SW                         
055100         MOVE '024'                  TO RESP-IDMSG-ERROR                  
055200         MOVE 'KVKOLLI-LOSS'         TO RESP-IDELMT-ERROR                 
055300       END-IF                                                             
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700                                                                          
055800 GABB-KOLLA-IDANSTNR-LOSS SECTION.                                        
055900                                                                          
056000     IF REQU-IDANSTNR-LOSS-UPD  NOT = ALL '+'                             
056100       IF REQU-IDANSTNR-LOSS-UPD   NUMERIC                                
056200         CONTINUE                                                         
056300       ELSE                                                               
056400         MOVE NEJ                    TO INDATA-SW                         
056500         MOVE '024'                  TO RESP-IDMSG-ERROR                  
056600         MOVE 'IDANSTNR-LOSS'        TO RESP-IDELMT-ERROR                 
056700      END-IF                                                              
056800     END-IF                                                               
056900     .                                                                    
057000                                                                          
057100 GABC-KOLLA-ADINLOMR-LOSS SECTION.                                        
057200                                                                          
057300     IF REQU-ADINLOMR-LOSS-UPD         NOT = ALL '+'                      
057400       CONTINUE                                                           
057500     END-IF                                                               
057600     .                                                                    
057700                                                                          
057800 GABD-KOLLA-IDFRASED-LOSS SECTION.                                        
057900                                                                          
058000     IF REQU-IDFRASED-LOSS-UPD         NOT = ALL '+'                      
058100       CONTINUE                                                           
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500                                                                          
058600 GAD-KOLLA-KOLLI-INFO   SECTION.                                          
058700                                                                          
058800     MOVE NEJ TO SW-KOLLI-INFO                                            
058900                                                                          
059000     IF (REQU-IDDISTR NOT = ALL '+' AND REQU-IDDISTR NOT = ZERO)          
059100     OR (REQU-IDKOLLI NOT = ALL '+' AND REQU-IDKOLLI NOT = ZERO)          
059200     OR (REQU-IDKOLLI-FOM NOT = ALL '+'                                   
059300                                  AND REQU-IDKOLLI-FOM NOT = ZERO)        
059400     OR (REQU-IDKOLLI-TOM NOT = ALL '+'                                   
059500                                  AND REQU-IDKOLLI-TOM NOT = ZERO)        
059600       MOVE JA TO SW-KOLLI-INFO                                           
059700                                                                          
059800       PERFORM GADA-KOLLA-IDKOLLI                                         
059900       PERFORM GADB-KOLLA-IDDISTR                                         
060000       PERFORM GADC-KOLLA-IDKOLLI-FOM-TOM                                 
060100       IF REQU-IDKOLLI-FOM = ALL '+' AND                                  
060200                                     REQU-IDKOLLI-TOM = ALL '+'           
060300         IF REQU-IDKOLLI = ALL '+'                                        
060400           MOVE '026'               TO RESP-IDMSG-ERROR                   
060500           MOVE 'IDKOLLI'           TO RESP-IDELMT-ERROR                  
060600           MOVE NEJ                 TO INDATA-SW                          
060700         END-IF                                                           
060800       ELSE                                                               
060900         IF REQU-IDKOLLI = ALL '+'                                        
061000         OR  (REQU-IDKOLLI NUMERIC   AND REQU-IDKOLLI = ZERO)             
061100         OR  (WS-IDKOLLI-FOM = ZERO  AND WS-IDKOLLI-TOM = ZERO)           
061200            CONTINUE                                                      
061300         ELSE                                                             
061400            MOVE '209'               TO RESP-IDMSG-ERROR                  
061500            MOVE NEJ                 TO INDATA-SW                         
061600         END-IF                                                           
061700       END-IF                                                             
061800       IF WS-IDKOLLI-FOM = ZERO                                           
061900       AND WS-IDKOLLI-TOM = ZERO                                          
062000       AND W-IDKOLLI      = ZERO                                          
062100         MOVE '126'               TO RESP-IDMSG-ERROR                     
062200         MOVE 'IDKOLLI'           TO RESP-IDELMT-ERROR                    
062300         MOVE NEJ                 TO INDATA-SW                            
062400       END-IF                                                             
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800                                                                          
062900 GADA-KOLLA-IDKOLLI      SECTION.                                         
063000                                                                          
063100     IF REQU-IDKOLLI                NOT = ALL '+'                         
063200       IF REQU-IDKOLLI             NOT NUMERIC                            
063300         MOVE '024'               TO RESP-IDMSG-ERROR                     
063400         MOVE 'IDKOLLI'           TO RESP-IDELMT-ERROR                    
063500         MOVE NEJ                 TO INDATA-SW                            
063600       ELSE                                                               
063700         IF REQU-IDKOLLI > ZERO                                           
063800           MOVE REQU-IDKOLLI      TO W-IDKOLLI                            
063900         ELSE                                                             
064000           MOVE '126'             TO RESP-IDMSG-ERROR                     
064100           MOVE 'IDKOLLI'         TO RESP-IDELMT-ERROR                    
064200           MOVE NEJ               TO INDATA-SW                            
064300         END-IF                                                           
064400       END-IF                                                             
064500     END-IF                                                               
064600     .                                                                    
064700 GADB-KOLLA-IDDISTR      SECTION.                                         
064800                                                                          
064900     IF REQU-IDDISTR                NOT = ALL '+'                         
065000       IF REQU-IDDISTR             NOT NUMERIC                            
065100         MOVE '024'               TO RESP-IDMSG-ERROR                     
065200         MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                    
065300         MOVE NEJ                 TO INDATA-SW                            
065400       END-IF                                                             
065500     ELSE                                                                 
065600       MOVE '026'                  TO RESP-IDMSG-ERROR                    
065700       MOVE 'IDDISTR'              TO RESP-IDELMT-ERROR                   
065800       MOVE NEJ                    TO INDATA-SW                           
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200                                                                          
066300 GADC-KOLLA-IDKOLLI-FOM-TOM SECTION.                                      
066400                                                                          
066500     IF REQU-IDKOLLI-FOM  NOT = ALL '+'                                   
066600       IF REQU-IDKOLLI-FOM  NOT NUMERIC                                   
066700         MOVE '024'               TO RESP-IDMSG-ERROR                     
066800         MOVE 'IDKOLLI-FOM'       TO RESP-IDELMT-ERROR                    
066900         MOVE NEJ                 TO INDATA-SW                            
067000       ELSE                                                               
067100         MOVE REQU-IDKOLLI-FOM    TO WS-IDKOLLI-FOM                       
067200         MOVE REQU-IDKOLLI-FOM(1:) TO RESP-IDKOLLI-FOM(1:)                
067300         INSPECT RESP-IDKOLLI-FOM REPLACING LEADING ZERO BY SPACE         
067400       END-IF                                                             
067500     ELSE                                                                 
067600       MOVE ZERO                   TO WS-IDKOLLI-FOM                      
067700     END-IF                                                               
067800                                                                          
067900     IF REQU-IDKOLLI-TOM  NOT = ALL '+'                                   
068000       IF REQU-IDKOLLI-TOM  NOT NUMERIC                                   
068100         MOVE '024'               TO RESP-IDMSG-ERROR                     
068200         MOVE 'IDKOLLI-TOM'       TO RESP-IDELMT-ERROR                    
068300         MOVE NEJ                 TO INDATA-SW                            
068400       ELSE                                                               
068500         MOVE REQU-IDKOLLI-TOM    TO WS-IDKOLLI-TOM                       
068600         MOVE REQU-IDKOLLI-TOM    TO RESP-IDKOLLI-TOM(1:)                 
068700         INSPECT RESP-IDKOLLI-TOM REPLACING LEADING ZERO BY SPACE         
068800       END-IF                                                             
068900     ELSE                                                                 
069000       MOVE ZERO                   TO WS-IDKOLLI-TOM                      
069100     END-IF                                                               
069200                                                                          
069300     IF  WS-IDKOLLI-FOM = ZERO                                            
069400     AND WS-IDKOLLI-TOM = ZERO                                            
069500       CONTINUE                                                           
069600     ELSE                                                                 
069700       IF WS-IDKOLLI-TOM > WS-IDKOLLI-FOM                                 
069800       AND WS-IDKOLLI-FOM > ZERO                                          
069900       AND WS-IDKOLLI-FOM + 20 > WS-IDKOLLI-TOM                           
070000         CONTINUE                                                         
070100       ELSE                                                               
070200         MOVE '023'               TO RESP-IDMSG-ERROR                     
070300         MOVE 'IDKOLLI-TOM'       TO RESP-IDELMT-ERROR                    
070400         MOVE NEJ                 TO INDATA-SW                            
070500       END-IF                                                             
070600     END-IF                                                               
070700     .                                                                    
070800     EJECT                                                                
070900 GAE-KOLLA-RAD-INFO   SECTION.                                            
071000                                                                          
071100     MOVE NEJ      TO SW-RAD-INFO                                         
071200     MOVE +1       TO INDX                                                
071300     PERFORM UNTIL INDX > MAX-INDX                                        
071400                                                                          
071500       IF (REQU-IDKUNDNR(INDX) NOT = ALL '+'                              
071600                               AND REQU-IDKUNDNR(INDX) NOT = ZERO)        
071700       OR (REQU-IDRAPPNR(INDX) NOT = ALL '+'                              
071800                               AND REQU-IDRAPPNR(INDX) NOT = ZERO)        
071900       OR (REQU-KVKOLLI (INDX) NOT = ALL '+'                              
072000                               AND REQU-KVKOLLI (INDX) NOT = ZERO)        
072100       OR (REQU-IDFRASED(INDX) NOT = ALL '+'                              
072200                              AND REQU-IDFRASED(INDX) NOT = SPACE)        
072300       OR (REQU-TERETNOT(INDX) NOT = ALL '+'                              
072400                              AND REQU-TERETNOT(INDX) NOT = SPACE)        
072500         MOVE JA TO SW-RAD-INFO                                           
072600                                                                          
072700         PERFORM GAED-KOLLA-IDKUNDNR                                      
072800         PERFORM GAEA-KOLLA-IDRAPPNR                                      
072900         PERFORM GAEB-KOLLA-KVKOLLI                                       
073000         PERFORM GAEC-KOLLA-RT-PAA-SIDA                                   
073100                                                                          
073200         IF REQU-IDFRASED(INDX)      NOT = ALL '+'                        
073300           CONTINUE                                                       
073400         END-IF                                                           
073500                                                                          
073600         IF REQU-TERETNOT(INDX)      NOT = ALL '+'                        
073700           CONTINUE                                                       
073800         END-IF                                                           
073900       END-IF                                                             
074000                                                                          
074100       ADD +1                        TO INDX                              
074200     END-PERFORM                                                          
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 GAEA-KOLLA-IDRAPPNR     SECTION.                                         
074700                                                                          
074800     IF REQU-IDRAPPNR(INDX)         NOT = ALL '+'                         
074900       IF REQU-IDRAPPNR(INDX)      NOT NUMERIC                            
075000         MOVE NEJ                 TO INDATA-SW                            
075100         MOVE '024'               TO RESP-IDMSG-ERROR                     
075200                                  RESP-IDMSG-ERROR-LINE (INDX)            
075300         MOVE 'IDRAPPNR'          TO RESP-IDELMT-ERROR                    
075400       END-IF                                                             
075500     ELSE                                                                 
075600       MOVE '026'                  TO RESP-IDMSG-ERROR                    
075700                                    RESP-IDMSG-ERROR-LINE (INDX)          
075800       MOVE 'IDRAPPNR'             TO RESP-IDELMT-ERROR                   
075900       MOVE NEJ                    TO INDATA-SW                           
076000     END-IF                                                               
076100     .                                                                    
076200 GAEB-KOLLA-KVKOLLI      SECTION.                                         
076300                                                                          
076400     IF REQU-KVKOLLI(INDX)          NOT = ALL '+'                         
076500       IF REQU-KVKOLLI(INDX)       NOT NUMERIC                            
076600         MOVE NEJ                 TO INDATA-SW                            
076700         MOVE '024'               TO RESP-IDMSG-ERROR                     
076800                                    RESP-IDMSG-ERROR-LINE (INDX)          
076900         MOVE 'KVKOLLI'           TO RESP-IDELMT-ERROR                    
077000       END-IF                                                             
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400 GAEC-KOLLA-RT-PAA-SIDA    SECTION.                                       
077500                                                                          
077600     COMPUTE INDX2                 =  INDX + 1                            
077700     PERFORM UNTIL INDX2           >  MAX-INDX OR                         
077800       (REQU-IDKUNDNR(INDX)        =  REQU-IDKUNDNR(INDX2) AND            
077900        REQU-IDRAPPNR(INDX)        =  REQU-IDRAPPNR(INDX2))               
078000       ADD +1                    TO INDX2                                 
078100     END-PERFORM                                                          
078200                                                                          
078300     IF INDX2                      >  MAX-INDX                            
078400       CONTINUE                                                           
078500     ELSE                                                                 
078600       MOVE '206'                TO RESP-IDMSG-ERROR                      
078700                                    RESP-IDMSG-ERROR-LINE (INDX2)         
078800       MOVE NEJ                  TO INDATA-SW                             
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200                                                                          
079300 GAED-KOLLA-IDKUNDNR     SECTION.                                         
079400                                                                          
079500     IF REQU-IDKUNDNR(INDX)         NOT = ALL '+'                         
079600       IF REQU-IDKUNDNR(INDX)      NOT NUMERIC                            
079700         MOVE '024'               TO RESP-IDMSG-ERROR                     
079800                                     RESP-IDMSG-ERROR-LINE (INDX)         
079900         MOVE 'IDKUNDNR'          TO RESP-IDELMT-ERROR                    
080000         MOVE NEJ                 TO INDATA-SW                            
080100       END-IF                                                             
080200     ELSE                                                                 
080300       MOVE '026'                  TO RESP-IDMSG-ERROR                    
080400                                      RESP-IDMSG-ERROR-LINE (INDX)        
080500       MOVE 'IDKUNDNR'             TO RESP-IDELMT-ERROR                   
080600       MOVE NEJ                    TO INDATA-SW                           
080700     END-IF                                                               
080800     .                                                                    
080900 GAF-RELATIONS-KONTROLL SECTION.                                          
081000                                                                          
081100     IF SKAPA-NY-SND                                                      
081200       IF LOSS-INFO OR RAD-INFO                                           
081300         MOVE NEJ                  TO INDATA-SW                           
081400         MOVE ERR-FLERA-FUNKTIONER TO RESP-IDMSG-ERROR                    
081500       ELSE                                                               
081600         IF KOLLI-INFO                                                    
081700           MOVE NEJ                  TO SW-KOLLI-INFO                     
081800         END-IF                                                           
081900       END-IF                                                             
082000     END-IF                                                               
082100                                                                          
082200     IF KOLLI-INFO                                                        
082300       IF RAD-INFO                                                        
082400         CONTINUE                                                         
082500       ELSE                                                               
082600         PERFORM S01C-FELMARKERA-KOLLI-INFO                               
082700       END-IF                                                             
082800     END-IF                                                               
082900                                                                          
083000     IF RAD-INFO                                                          
083100       IF KOLLI-INFO                                                      
083200         CONTINUE                                                         
083300       ELSE                                                               
083400         PERFORM S01C-FELMARKERA-KOLLI-INFO                               
083500       END-IF                                                             
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900                                                                          
084000 GB-LOGISK-KONTROLL   SECTION.                                            
084100                                                                          
084200     IF SKAPA-NY-SND                                                      
084300       CONTINUE                                                           
084400     ELSE                                                                 
084500       PERFORM GBA-KOLLA-SNDSTATUS                                        
084600       PERFORM GBB-KOLLA-RELATIONER                                       
084700     END-IF                                                               
084800                                                                          
084900     IF RAD-INFO                                                          
085000       PERFORM GBC-KOLLA-REG-RAPPORTER                                    
085100     END-IF                                                               
085200     .                                                                    
085300 GBA-KOLLA-SNDSTATUS           SECTION.                                   
085400                                                                          
085500     MOVE REQU-IDRTLOP-KEY          TO W-IDRTLOP-BSEQ-MIN                 
085600                                       W-IDRTLOP-BSEQ-MAX                 
085700                                       W-IDRTLOP-NUM                      
085800     PERFORM GBAA-KOLLA-IDRTLOP                                           
085900                                                                          
086000     PERFORM IMS-GHU-SEQB-WLRETA01                                        
086100                                                                          
086200     IF SEGMENT-FINNS                                                     
086300       MOVE JA  TO SW-GAMMAL-SND                                          
086400       IF RET-KDRETSTA = W-SND-SAENT OR  W-SND-LOSS                       
086500         CONTINUE                                                         
086600       ELSE                                                               
086700         PERFORM S01-FELMARKERA                                           
086800         MOVE 'KDRETSTA'       TO RESP-IDELMT-ERROR                       
086900         MOVE '023'            TO RESP-IDMSG-ERROR                        
087000       END-IF                                                             
087100     END-IF                                                               
087200     .                                                                    
087300     EJECT                                                                
087400                                                                          
087500 GBAA-KOLLA-IDRTLOP               SECTION.                                
087600                                                                          
087700     PERFORM IMS-GHNP-WL411111                                            
087800     IF W-IDRTLOP-NUM                  >  4112-IDRTLOP                    
087900       PERFORM S01-FELMARKERA                                             
088000     END-IF                                                               
088100     .                                                                    
088200                                                                          
088300 GBB-KOLLA-RELATIONER             SECTION.                                
088400                                                                          
088500     IF LOSS-INFO                                                         
088600       PERFORM GBBA-KOLLA-LOSS-INFO                                       
088700     END-IF                                                               
088800                                                                          
088900     IF RAD-INFO                                                          
089000       IF GAMMAL-SND                                                      
089100       OR LOSS-INFO                                                       
089200         CONTINUE                                                         
089300       ELSE                                                               
089400         PERFORM S01C-FELMARKERA-KOLLI-INFO                               
089500       END-IF                                                             
089600     END-IF                                                               
089700     .                                                                    
089800     EJECT                                                                
089900                                                                          
090000 GBBA-KOLLA-LOSS-INFO               SECTION.                              
090100                                                                          
090200     IF GAMMAL-SND                                                        
090300       IF RET-IDRT = W-CDC OR W-US1 OR W-US2 OR W-US3 OR W-CA1 OR         
090400                     W-JP1 OR W-AU1 OR W-SE1 OR W-GB1 OR W-GB3 OR         
090500                     W-SE2 OR W-GB2 OR W-NL1 OR W-IT1                     
090600         CONTINUE                                                         
090700       ELSE                                                               
090800         IF REQU-KVKOLLI-LOSS-UPD NOT = ALL '+'                           
090900           MOVE '033'                TO RESP-IDMSG-ERROR                  
091000           MOVE 'KVKOLLI-LOSS'       TO RESP-IDELMT-ERROR                 
091100           MOVE NEJ                  TO INDATA-SW                         
091200         END-IF                                                           
091300         IF REQU-IDFRASED-LOSS-UPD NOT = ALL '+'                          
091400           MOVE '033'                TO RESP-IDMSG-ERROR                  
091500           MOVE 'IDFRASED-LOSS'      TO RESP-IDELMT-ERROR                 
091600           MOVE NEJ                  TO INDATA-SW                         
091700         END-IF                                                           
091800       END-IF                                                             
091900     END-IF                                                               
092000     .                                                                    
092100     EJECT                                                                
092200                                                                          
092300 GBC-KOLLA-REG-RAPPORTER   SECTION.                                       
092400                                                                          
092500     MOVE +1                         TO INDX                              
092600     PERFORM UNTIL INDX              > MAX-INDX                           
092700                                                                          
092800       IF REQU-IDRAPPNR(INDX)       NOT = ALL '+'                         
092900         MOVE REQU-IDDISTR        TO W-IDDISTR-ANM                        
093000                                     W-IDDISTR-MIN                        
093100                                     W-IDDISTR-MAX                        
093200         MOVE REQU-IDDISTR(1:)    TO RESP-IDDISTR(1:)                     
093300         INSPECT RESP-IDDISTR  REPLACING LEADING ZERO BY SPACE            
093400         MOVE REQU-IDKUNDNR(INDX) TO W-IDKUNDNR-ANM                       
093500                                     W-IDKUNDNR-MIN                       
093600                                     W-IDKUNDNR-MAX                       
093700         MOVE REQU-IDRAPPNR(INDX) TO W-IDRAPPNR-ANM                       
093800                                     W-IDRAPPNR-MIN                       
093900                                      W-IDRAPPNR-MAX                      
094000         PERFORM IMS-GU-WLKREE01                                          
094100         IF (SEGMENT-SAKNAS AND REQU-KDPGMACT NOT = 'N')                  
094200         OR (SEGMENT-FINNS AND ANM-KDLEVANM NOT = '4')                    
094300           MOVE NEJ             TO INDATA-SW                              
094400           IF SEGMENT-SAKNAS                                              
094500             MOVE 'IDLEVANM'   TO RESP-IDELMT-ERROR                       
094600             MOVE '025'        TO RESP-IDMSG-ERROR                        
094700                                  RESP-IDMSG-ERROR-LINE (INDX)            
094800           ELSE                                                           
094900             MOVE 'KDLEVANM'    TO RESP-IDELMT-ERROR                      
095000             MOVE '023'         TO RESP-IDMSG-ERROR                       
095100                                   RESP-IDMSG-ERROR-LINE (INDX)           
095200           END-IF                                                         
095300         ELSE                                                             
095400           PERFORM GBCA-LAES-WLKREE11                                     
095500           IF SEGMENT-FINNS                                               
095600           AND LEV-IDDC-RET = REQU-IDDC-KEY                               
095700             PERFORM IMS-GU-WLRETG01                                      
095800             PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                 
095900                                          OR INDATA-FEL                   
096000               IF SEQF-IDRT    = W-IDRT-BSEQ-MIN                          
096100               AND SEQF-IDRTLOP = W-IDRTLOP-BSEQ-MIN                      
096200                 IF SEQF-IDKOLLI = W-IDKOLLI                              
096300                 OR (     (SEQF-IDKOLLI > WS-IDKOLLI-FOM - 1)             
096400                      AND (SEQF-IDKOLLI < WS-IDKOLLI-TOM + 1)  )          
096500                   MOVE '206'  TO RESP-IDMSG-ERROR                        
096600                                  RESP-IDMSG-ERROR-LINE (INDX)            
096700                   MOVE NEJ    TO INDATA-SW                               
096800                 END-IF                                                   
096900               ELSE                                                       
097000                 MOVE '206'    TO RESP-IDMSG-ERROR                        
097100                                  RESP-IDMSG-ERROR-LINE (INDX)            
097200                 MOVE NEJ      TO INDATA-SW                               
097300               END-IF                                                     
097400               PERFORM IMS-GN-WLRETG01                                    
097500             END-PERFORM                                                  
097600           ELSE                                                           
097700             IF REQU-KDPGMACT NOT = 'N'                                   
097800               MOVE NEJ               TO INDATA-SW                        
097900               IF SEGMENT-SAKNAS                                          
098000                 MOVE 'IDRT-IDRTLOP'  TO RESP-IDELMT-ERROR                
098100                 MOVE '025'           TO RESP-IDMSG-ERROR                 
098200                                      RESP-IDMSG-ERROR-LINE (INDX)        
098300               ELSE                                                       
098400                 MOVE 'KDLEVANM'      TO RESP-IDELMT-ERROR                
098500                 MOVE '023'           TO RESP-IDMSG-ERROR                 
098600                                      RESP-IDMSG-ERROR-LINE (INDX)        
098700               END-IF                                                     
098800             END-IF                                                       
098900           END-IF                                                         
099000         END-IF                                                           
099100       END-IF                                                             
099200       ADD +1        TO INDX                                              
099300     END-PERFORM                                                          
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700 GBCA-LAES-WLKREE11          SECTION.                                     
099800                                                                          
099900     MOVE NEJ                    TO OKOD-FL-RETILL                        
100000                                    OKOD-FL-INTERNUPPACKNING              
100100     PERFORM IMS-GNP-WLKREE11                                             
100200     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
100300                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
100400       IF LEV-KDKREBEH(1:1) = 'Y'                                         
100500       OR LEV-KDKREBEH(1:1) = 'J'                                         
100600       OR LEV-KDKREBEH(1:1) = 'C'                                         
100700       OR LEV-KDKREBEH = 'D01'                                            
100800       OR LEV-KDKREBEH = 'D02'                                            
100900       OR LEV-KDKREBEH = 'D03'                                            
101000*--ANROPA KONTROLL AV ORSAKSKODER                                         
101100         MOVE LEV-KDANMORS   TO OKOD-KDANMORS                             
101200         CALL W418OKOD USING OKOD-W418OKOD                                
101300       END-IF                                                             
101400       IF OKOD-FL-RETILL = 'J'                                            
101500       OR OKOD-FL-INTERNUPPACKNING = 'J'                                  
101600         CONTINUE                                                         
101700       ELSE                                                               
101800         PERFORM IMS-GNP-WLKREE11                                         
101900       END-IF                                                             
102000     END-PERFORM                                                          
102100     .                                                                    
102200     EJECT                                                                
102300 H-UPPDATERA SECTION.                                                     
102400                                                                          
102500     MOVE NEJ           TO  W-UPDATE-SW                                   
102600     IF SKAPA-NY-SND                                                      
102700       PERFORM HA-TA-UT-IDRTLOP                                           
102800       MOVE JA           TO  W-UPDATE-SW                                  
102900     END-IF                                                               
103000                                                                          
103100     IF LOSS-INFO                                                         
103200       PERFORM HC-UPPDATERA-MOT                                           
103300       MOVE JA           TO  W-UPDATE-SW                                  
103400     END-IF                                                               
103500                                                                          
103600     IF RAD-INFO                                                          
103700       PERFORM HD-UPPDATERA-RT                                            
103800       MOVE JA           TO  W-UPDATE-SW                                  
103900     END-IF                                                               
104000                                                                          
104100     IF REQU-KDPGMACT = 'N'                                               
104200       MOVE '288'               TO RESP-IDMSG-INFO                        
104300     ELSE                                                                 
104400       MOVE '001'               TO RESP-IDMSG-INFO                        
104500     END-IF                                                               
104600     .                                                                    
104700     EJECT                                                                
104800                                                                          
104900 HA-TA-UT-IDRTLOP SECTION.                                                
105000                                                                          
105100     PERFORM IMS-GHNP-WL411111                                            
105200     COMPUTE 4112-IDRTLOP = 4112-IDRTLOP + 001                            
105300     IF 4112-IDRTLOP = ZERO                                               
105400       ADD +001 TO 4112-IDRTLOP                                           
105500     END-IF                                                               
105600                                                                          
105700     PERFORM IMS-REPL-WL411111                                            
105800                                                                          
105900     MOVE 4112-IDRTLOP    TO W-IDRTLOP-NUM                                
106000     MOVE W-IDRTLOP-NUM   TO RESP-IDRTLOP-KEY                             
106100     MOVE W-IDRTLOP-NUM   TO W-IDRTLOP-BSEQ-MIN                           
106200                             W-IDRTLOP-BSEQ-MIN                           
106300                             W-IDRTLOP-BSEQ-MAX                           
106400                                                                          
106500     MOVE REQU-IDRT-KEY   TO RESP-IDRT-KEY                                
106600                             W-IDRT-BSEQ-MIN                              
106700                             W-IDRT-BSEQ-MAX                              
106800     .                                                                    
106900                                                                          
107000 HC-UPPDATERA-MOT   SECTION.                                              
107100                                                                          
107200     IF GAMMAL-SND                                                        
107300       PERFORM HCA-UPPDATERA-RT-MOT                                       
107400     ELSE                                                                 
107500       PERFORM HCB-SKAPA-RT-MOT                                           
107600       MOVE JA           TO  W-UPDATE-SW                                  
107700     END-IF                                                               
107800     .                                                                    
107900                                                                          
108000 HCA-UPPDATERA-RT-MOT  SECTION.                                           
108100                                                                          
108200     PERFORM IMS-GHU-SEQB-WLRETA01                                        
108300                                                                          
108400     IF SEGMENT-FINNS                                                     
108500       PERFORM UNTIL SEGMENT-SAKNAS                                       
108600         IF REQU-KVKOLLI-LOSS-UPD     NOT = ALL '+'                       
108700           MOVE REQU-KVKOLLI-LOSS-UPD  TO RET-KVKOLLI-MOT                 
108800         END-IF                                                           
108900                                                                          
109000         IF REQU-ADINLOMR-LOSS-UPD    NOT = ALL '+'                       
109100           MOVE REQU-ADINLOMR-LOSS-UPD TO RET-ADINLOMR-MOT                
109200                                          RET-ADINLOMR                    
109300         END-IF                                                           
109400                                                                          
109500         IF REQU-IDANSTNR-LOSS-UPD    NOT = ALL '+'                       
109600           MOVE REQU-IDANSTNR-LOSS-UPD TO RET-IDANSTNR-MOT                
109700         END-IF                                                           
109800                                                                          
109900         IF REQU-IDFRASED-LOSS-UPD    NOT = ALL '+'                       
110000           MOVE REQU-IDFRASED-LOSS-UPD TO RET-IDFRASED-CDC                
110100         END-IF                                                           
110200                                                                          
110300         PERFORM IMS-REPL-SEQB-WLRETA01                                   
110400         PERFORM IMS-GHN-SEQB-WLRETA01                                    
110500       END-PERFORM                                                        
110600     ELSE                                                                 
110700       CALL FELLOG                                                        
110800     END-IF                                                               
110900     .                                                                    
111000                                                                          
111100 HCB-SKAPA-RT-MOT  SECTION.                                               
111200                                                                          
111300     MOVE REQU-IDDC-KEY        TO RET-IDDC                                
111400     MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                     
111500                                         RET-DASNDDAT                     
111600     ACCEPT RET-TIKLOCK        FROM TIME                                  
111700                                                                          
111800     PERFORM S02-INIT-WLRETA01                                            
111900                                                                          
112000     IF REQU-ADINLOMR-LOSS-UPD NOT = ALL '+'                              
112100       MOVE REQU-ADINLOMR-LOSS-UPD  TO RET-ADINLOMR                       
112200                                      RET-ADINLOMR-MOT                    
112300     END-IF                                                               
112400     MOVE SPACE                     TO RET-ADINLOMR-LOSS                  
112500     IF REQU-IDANSTNR-LOSS-UPD NOT = ALL '+'                              
112600       MOVE REQU-IDANSTNR-LOSS-UPD  TO RET-IDANSTNR-MOT                   
112700     ELSE                                                                 
112800       MOVE ZERO                    TO RET-IDANSTNR-MOT                   
112900     END-IF                                                               
113000     MOVE ZERO                      TO RET-IDANSTNR-LOSS                  
113100     IF REQU-IDFRASED-LOSS-UPD  NOT = ALL '+'                             
113200       MOVE REQU-IDFRASED-LOSS-UPD TO RET-IDFRASED-CDC                    
113300     ELSE                                                                 
113400       MOVE SPACE                  TO RET-IDFRASED-CDC                    
113500     END-IF                                                               
113600     IF REQU-KVKOLLI-LOSS-UPD NOT = ALL '+'                               
113700       MOVE REQU-KVKOLLI-LOSS-UPD   TO RET-KVKOLLI-MOT                    
113800     ELSE                                                                 
113900       MOVE ZERO                    TO RET-KVKOLLI-MOT                    
114000     END-IF                                                               
114100     MOVE ZERO                      TO RET-KVKOLLI-LOSS                   
114200                                       RET-KVKOLLI-AAF                    
114300                                                                          
114400     PERFORM IMS-ISRT-WLRETA01                                            
114500     PERFORM UNTIL SEGMENT-FINNS                                          
114600       ADD +1 TO RET-TIKLOCK                                              
114700       PERFORM IMS-ISRT-WLRETA01                                          
114800     END-PERFORM                                                          
114900     .                                                                    
115000     EJECT                                                                
115100                                                                          
115200 HD-UPPDATERA-RT SECTION.                                                 
115300                                                                          
115400     MOVE +1                   TO INDX                                    
115500     MOVE JA                   TO SW-FOERSTA-RT                           
115600     PERFORM UNTIL INDX        > MAX-INDX                                 
115700                                                                          
115800       IF REQU-IDKUNDNR(INDX)  NOT = ALL '+'                              
115900         PERFORM HDA-BEHANDLA-RADER                                       
116000         MOVE SPACE TO   RESP-RADER(INDX)                                 
116100       END-IF                                                             
116200       ADD +1                  TO INDX                                    
116300                                                                          
116400     END-PERFORM                                                          
116500     .                                                                    
116600     EJECT                                                                
116700                                                                          
116800 HDA-BEHANDLA-RADER     SECTION.                                          
116900                                                                          
117000     IF FOERSTA-RT                                                        
117100       MOVE NEJ               TO SW-FOERSTA-RT                            
117200       PERFORM IMS-GHU-SEQB-WLRETA01                                      
117300       IF SEGMENT-FINNS AND RET-IDDISTR  = ZERO                           
117400         MOVE RET-WDA301        TO SPAR-RET-WDA301                        
117500         PERFORM HDAA-REDIGERA-WLRETA01                                   
117600         PERFORM IMS-REPL-SEQB-WLRETA01                                   
117700         MOVE RET-KDRETSTA      TO SPAR-KDRETSTA                          
117800         MOVE RET-KDKOLSTA      TO SPAR-KDKOLSTA                          
117900         IF WS-IDKOLLI-FOM > ZERO                                         
118000           ADD +1        TO RET-IDKOLLI                                   
118100           PERFORM UNTIL RET-IDKOLLI > WS-IDKOLLI-TOM                     
118200             MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT             
118300             ACCEPT RET-TIKLOCK  FROM TIME                                
118400             PERFORM IMS-ISRT-WLRETA01                                    
118500             IF SEGMENT-FINNS                                             
118600               ADD +1        TO RET-IDKOLLI                               
118700             END-IF                                                       
118800           END-PERFORM                                                    
118900         END-IF                                                           
119000       ELSE                                                               
119100         IF SEGMENT-FINNS                                                 
119200           MOVE RET-KDRETSTA      TO SPAR-KDRETSTA                        
119300           MOVE RET-KDKOLSTA      TO SPAR-KDKOLSTA                        
119400         END-IF                                                           
119500         PERFORM HDAA-REDIGERA-WLRETA01                                   
119600         IF SPAR-KDRETSTA NOT = SPACE                                     
119700           MOVE SPAR-KDRETSTA        TO RET-KDRETSTA                      
119800           MOVE SPAR-KDKOLSTA        TO RET-KDKOLSTA                      
119900         ELSE                                                             
120000           MOVE W-SND-SAENT          TO RET-KDRETSTA                      
120100           MOVE W-KLI-SAENT          TO RET-KDKOLSTA                      
120200         END-IF                                                           
120300         MOVE REQU-IDDC-KEY  TO RET-IDDC                                  
120400         MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                 
120500         ACCEPT RET-TIKLOCK  FROM TIME                                    
120600         MOVE RET-WDA301     TO SPAR-RET-WDA301                           
120700         PERFORM IMS-ISRT-WLRETA01                                        
120800         PERFORM UNTIL SEGMENT-FINNS                                      
120900           ADD +1    TO RET-TIKLOCK                                       
121000           PERFORM IMS-ISRT-WLRETA01                                      
121100         END-PERFORM                                                      
121200         IF WS-IDKOLLI-FOM > ZERO                                         
121300           ADD +1        TO RET-IDKOLLI                                   
121400           PERFORM UNTIL RET-IDKOLLI > WS-IDKOLLI-TOM                     
121500             MOVE REQU-IDDC-KEY  TO RET-IDDC                              
121600             MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT             
121700             ACCEPT RET-TIKLOCK  FROM TIME                                
121800             PERFORM IMS-ISRT-WLRETA01                                    
121900             IF SEGMENT-FINNS                                             
122000               ADD +1        TO RET-IDKOLLI                               
122100             END-IF                                                       
122200           END-PERFORM                                                    
122300         END-IF                                                           
122400       END-IF                                                             
122500     ELSE                                                                 
122600       MOVE SPAR-RET-WDA301   TO RET-WDA301                               
122700       PERFORM HDAA-REDIGERA-WLRETA01                                     
122800       MOVE REQU-IDDC-KEY     TO RET-IDDC                                 
122900       MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                   
123000       ACCEPT RET-TIKLOCK     FROM TIME                                   
123100       IF SPAR-KDRETSTA NOT = SPACE                                       
123200         MOVE SPAR-KDRETSTA        TO RET-KDRETSTA                        
123300         MOVE SPAR-KDKOLSTA        TO RET-KDKOLSTA                        
123400       ELSE                                                               
123500         MOVE W-SND-SAENT          TO RET-KDRETSTA                        
123600         MOVE W-KLI-SAENT          TO RET-KDKOLSTA                        
123700       END-IF                                                             
123800       PERFORM IMS-ISRT-WLRETA01                                          
123900       PERFORM UNTIL SEGMENT-FINNS                                        
124000         ADD +1 TO RET-TIKLOCK                                            
124100         PERFORM IMS-ISRT-WLRETA01                                        
124200       END-PERFORM                                                        
124300       IF WS-IDKOLLI-FOM > ZERO                                           
124400         ADD +1        TO RET-IDKOLLI                                     
124500         PERFORM UNTIL RET-IDKOLLI > WS-IDKOLLI-TOM                       
124600           MOVE REQU-IDDC-KEY     TO RET-IDDC                             
124700           MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT               
124800           ACCEPT RET-TIKLOCK  FROM TIME                                  
124900           PERFORM IMS-ISRT-WLRETA01                                      
125000           IF SEGMENT-FINNS                                               
125100             ADD +1        TO RET-IDKOLLI                                 
125200           END-IF                                                         
125300         END-PERFORM                                                      
125400       END-IF                                                             
125500     END-IF                                                               
125600     .                                                                    
125700     EJECT                                                                
125800 HDAA-REDIGERA-WLRETA01  SECTION.                                         
125900                                                                          
126000     PERFORM HDAAB-HAEMTA-ANSVARIG                                        
126100     PERFORM HDAAA-HAEMTA-LEV-ANM-UPPG                                    
126200     PERFORM HDAAC-INIT-RT-INFO                                           
126300     .                                                                    
126400                                                                          
126500 HDAAA-HAEMTA-LEV-ANM-UPPG SECTION.                                       
126600                                                                          
126700     MOVE REQU-IDKUNDNR(INDX)  TO W-IDKUNDNR-ANM                          
126800     MOVE REQU-IDRAPPNR(INDX)  TO W-IDRAPPNR-ANM                          
126900                                                                          
127000     PERFORM IMS-GU-WLKREE01                                              
127100     IF SEGMENT-FINNS                                                     
127200       MOVE ANM-FLFARLIG      TO W-FLFARLIG                               
127300       MOVE ANM-KVRADER-RT    TO W-KVRADER                                
127400     END-IF                                                               
127500     .                                                                    
127600                                                                          
127700 HDAAB-HAEMTA-ANSVARIG SECTION.                                           
127800                                                                          
127810     MOVE NEJ                  TO W-FLBUYBAC                              
127900     MOVE REQU-IDKUNDNR(INDX)  TO W-IDKUNDNR-ANM                          
128000     MOVE REQU-IDRAPPNR(INDX)  TO W-IDRAPPNR-ANM                          
128100     PERFORM IMS-GU-WLKREE01                                              
128200     IF SEGMENT-FINNS                                                     
128300       MOVE ANM-IDFTG            TO ANSV-IDFTG                            
128400                                                                          
128500       PERFORM GBCA-LAES-WLKREE11                                         
128600       MOVE 3                    TO ANSV-KDCALL                           
128700       MOVE REQU-IDDISTR         TO ANSV-IDDISTR                          
128800       MOVE REQU-IDDISTR(1:)     TO RESP-IDDISTR(1:)                      
128900       INSPECT RESP-IDDISTR  REPLACING LEADING ZERO BY SPACE              
129000       MOVE REQU-IDDC-KEY        TO ANSV-IDDC                             
129100       MOVE REQU-IDKUNDNR(INDX)  TO ANSV-IDKUNDNR                         
129300       MOVE LEV-KDANMORS         TO ANSV-KDANMORS                         
129301                                                                          
129302       MOVE LEV-KDANMORS         TO W-KDANMORS                            
129303       IF KDANMORS-BUYBAC-98                                              
129304         MOVE JA                 TO W-FLBUYBAC                            
129305       END-IF                                                             
129306                                                                          
129310       MOVE ZERO                 TO ANSV-KDORDKL                          
129320                                    ANSV-ADLAGOMR                         
129400                                                                          
129500       CALL W418ANSV USING ANSV-W418ANSV 4113-PCB 4115-PCB                
129600                                         4117-PCB                         
129700                                                                          
129800       IF ANSV-OK                                                         
129900         MOVE ANSV-KDARBTYP     TO W-KDARBTYP                             
130000         MOVE ANSV-IDPERSON     TO W-IDPERSON                             
130100       ELSE                                                               
130200         MOVE 'RET'             TO W-KDARBTYP                             
130300         MOVE 9                 TO W-IDPERSON                             
130400       END-IF                                                             
130500     END-IF                                                               
130600     .                                                                    
130700     EJECT                                                                
130800                                                                          
130900 HDAAC-INIT-RT-INFO   SECTION.                                            
131000                                                                          
131100     MOVE REQU-IDDISTR         TO RET-IDDISTR                             
131200     MOVE REQU-IDDISTR(1:)     TO RESP-IDDISTR(1:)                        
131300     INSPECT RESP-IDDISTR  REPLACING LEADING ZERO BY SPACE                
131400     MOVE REQU-IDKUNDNR(INDX)  TO RET-IDKUNDNR                            
131500     MOVE REQU-IDRAPPNR(INDX)  TO RET-IDRAPPNR                            
131600     MOVE W-FLFARLIG           TO RET-FLFARLIG                            
131610     MOVE W-FLBUYBAC           TO RET-FLBUYBAC                            
131700     IF REQU-IDKOLLI NUMERIC AND REQU-IDKOLLI > ZERO                      
131800       MOVE REQU-IDKOLLI         TO RET-IDKOLLI                           
131900       MOVE REQU-IDKOLLI(1:)     TO RESP-IDKOLLI(1:)                      
132000       INSPECT  RESP-IDKOLLI REPLACING LEADING ZERO BY SPACE              
132100     ELSE                                                                 
132200       MOVE REQU-IDKOLLI-FOM     TO RET-IDKOLLI                           
132300       MOVE REQU-IDKOLLI-FOM(1:) TO RESP-IDKOLLI-FOM(1:)                  
132400       INSPECT RESP-IDKOLLI-FOM REPLACING LEADING ZERO BY SPACE           
132500     END-IF                                                               
132600     MOVE W-IDPERSON           TO RET-IDPERSON                            
132700     MOVE W-KDARBTYP           TO RET-KDARBTYP                            
132800     MOVE W-KVRADER            TO RET-KVRADER                             
132900     IF REQU-KVKOLLI(INDX)     NOT = ALL '+'                              
133000       MOVE REQU-KVKOLLI(INDX)   TO RET-KVKOLLI-AAF                       
133100     ELSE                                                                 
133200       MOVE ZERO                 TO RET-KVKOLLI-AAF                       
133300     END-IF                                                               
133400                                                                          
133500     IF REQU-IDFRASED(INDX)      NOT = ALL '+'                            
133600       MOVE REQU-IDFRASED(INDX) TO RET-IDFRASED-AAF                       
133700     ELSE                                                                 
133800       MOVE SPACE               TO RET-IDFRASED-AAF                       
133900     END-IF                                                               
134000                                                                          
134100     IF REQU-TERETNOT(INDX)    NOT = ALL '+'                              
134200       MOVE REQU-TERETNOT(INDX) TO RET-TERETNOT                           
134300     ELSE                                                                 
134400       MOVE SPACE             TO RET-TERETNOT                             
134500     END-IF                                                               
134600                                                                          
134700     MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DASNDDAT                     
134800     .                                                                    
134900     EJECT                                                                
135000 S01-FELMARKERA SECTION.                                                  
135100                                                                          
135200     IF LOSS-INFO                                                         
135300       PERFORM S01A-FELMARKERA-LOSS-INFO                                  
135400     END-IF                                                               
135500                                                                          
135600     IF KOLLI-INFO                                                        
135700       PERFORM S01C-FELMARKERA-KOLLI-INFO                                 
135800     END-IF                                                               
135900     .                                                                    
136000                                                                          
136100 S01A-FELMARKERA-LOSS-INFO SECTION.                                       
136200                                                                          
136300     MOVE '207'               TO RESP-IDMSG-ERROR                         
136400     MOVE NEJ                 TO INDATA-SW                                
136500     .                                                                    
136600                                                                          
136700 S01C-FELMARKERA-KOLLI-INFO SECTION.                                      
136800                                                                          
136900     MOVE '208'               TO RESP-IDMSG-ERROR                         
137000     MOVE NEJ                 TO INDATA-SW                                
137100     .                                                                    
137200     EJECT                                                                
137300 S02-INIT-WLRETA01  SECTION.                                              
137400                                                                          
137500     MOVE ZERO                 TO RET-IDDISTR                             
137600                                  RET-IDKUNDNR                            
137700                                  RET-IDRAPPNR                            
137800     MOVE NEJ                  TO RET-FLFARLIG                            
137810     MOVE NEJ                  TO RET-FLBUYBAC                            
137900     MOVE SPACE                TO RET-IDFRASED-AAF                        
138000     MOVE SPACE                TO RET-IDFRASED-CDC                        
138100     MOVE ZERO                 TO RET-IDKOLLI                             
138200     MOVE ZERO                 TO RET-IDPERSON                            
138300     MOVE REQU-IDRT-KEY        TO RET-IDRT                                
138400     MOVE REQU-IDRTLOP-KEY     TO RET-IDRTLOP                             
138500     MOVE ZERO                 TO RET-KDARBTYP                            
138600                                  RET-KDKOLSTA                            
138700     MOVE W-SND-SAENT          TO RET-KDRETSTA                            
138800     MOVE ZERO                 TO RET-KVRADER                             
138900     MOVE SPACE                TO RET-TERETNOT                            
139000     MOVE ZERO                 TO RET-TIINLMOT                            
139100                                  RET-TIKLAR                              
139200                                  RET-TILOSSN                             
139300                                  RET-DARETANK                            
139400                                  RET-TIREGDAT-TRRT                       
139410                                  RET-TISNDDAT-TRRT                       
139420     MOVE SPACE                TO RET-IDRT-TRANSIT                        
139500     .                                                                    
139600     EJECT                                                                
139700 S03-RAEKNA-KOLLI         SECTION.                                        
139800                                                                          
139900     MOVE ZERO                    TO W-KVKOLLI-LOSS                       
140000                                     W-KVKOLLI-MOT                        
140100     PERFORM UNTIL SEGMENT-SAKNAS                                         
140200       IF RET-IDKOLLI = SPAR-IDKOLLI                                      
140300         CONTINUE                                                         
140400       ELSE                                                               
140500         IF RET-TILOSSN > ZERO                                            
140600           IF RET-KDKOLSTA = W-KLI-SAK                                    
140700             CONTINUE                                                     
140800           ELSE                                                           
140900             ADD +1         TO W-KVKOLLI-LOSS                             
141000           END-IF                                                         
141100         END-IF                                                           
141200         IF RET-TIINLMOT > ZERO                                           
141300           IF RET-KDKOLSTA = W-KLI-AVV                                    
141400             CONTINUE                                                     
141500           ELSE                                                           
141600             ADD +1         TO W-KVKOLLI-MOT                              
141700           END-IF                                                         
141800         END-IF                                                           
141900       END-IF                                                             
142000                                                                          
142100       MOVE RET-IDKOLLI TO SPAR-IDKOLLI                                   
142200                                                                          
142300       PERFORM IMS-GHN-SEQB-WLRETA01                                      
142400     END-PERFORM                                                          
142500     .                                                                    
142600     EJECT                                                                
142700*    --- DISPATCHER SECTIONS                                              
142800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
142900                                                                          
143000     MOVE 'GETARG'               TO SUB-KDFUNC                            
143100     MOVE 'CARPARTS.LDC.RETURNREG'         TO SUB-ADDISPABS               
143200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
143300                                                                          
143400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
143500                                                                          
143600     IF SUB-KDRC > 0                                                      
143700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
143800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
143900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
144000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
144100     END-IF                                                               
144200     .                                                                    
144300                                                                          
144400 S02-RETURN-RESPONSE SECTION.                                             
144500                                                                          
144600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
144700     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
144800                                                                          
144900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
145000                                                                          
145100     IF SUB-KDRC > 0                                                      
145200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
145300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
145400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
145500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
145600     END-IF                                                               
145700     .                                                                    
145800     EJECT                                                                
145900 IMS-GU-WLRETG01 SECTION.                                                 
146000                                                                          
146100     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
146200                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
146300          DELIMITED BY SIZE INTO SSA1                                     
146400     MOVE '  GE' TO GODK-STATUSKODER                                      
146500     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA2 SSA1                     
146600     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900                                                                          
147000 IMS-GN-WLRETG01 SECTION.                                                 
147100                                                                          
147200     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
147300                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
147400          DELIMITED BY SIZE INTO SSA1                                     
147500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
147600     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA2 SSA1                     
147700     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
147800     PERFORM IMS-STATUSKONTROLL                                           
147900     .                                                                    
148000                                                                          
148100 IMS-ISRT-WLRETA01    SECTION.                                            
148200                                                                          
148300     MOVE 'WLRETA01 ' TO SSA1                                             
148400     MOVE '  II' TO GODK-STATUSKODER                                      
148500     CALL CBLTDLI USING ISRT RETA1-PCB DLI-IO-AREA SSA1                   
148600     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
148700     PERFORM IMS-STATUSKONTROLL                                           
148800     .                                                                    
148900     EJECT                                                                
149000 IMS-GHU-SEQB-WLRETA01       SECTION.                                     
149100                                                                          
149200     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
149300                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
149400          DELIMITED BY SIZE INTO SSA1                                     
149500     MOVE '  GE'           TO GODK-STATUSKODER                            
149600     CALL CBLTDLI USING GHU RETA2-PCB DLI-IO-AREA SSA1                    
149700     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000                                                                          
150100 IMS-GHN-SEQB-WLRETA01 SECTION.                                           
150200                                                                          
150300     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
150400                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     MOVE '  GE' TO GODK-STATUSKODER                                      
150700     CALL CBLTDLI USING GHN RETA2-PCB DLI-IO-AREA SSA1                    
150800     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
150900     PERFORM IMS-STATUSKONTROLL                                           
151000     .                                                                    
151100                                                                          
151200 IMS-REPL-SEQB-WLRETA01      SECTION.                                     
151300                                                                          
151400     MOVE '    '           TO GODK-STATUSKODER                            
151500     CALL CBLTDLI USING REPL RETA2-PCB DLI-IO-AREA                        
151600     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
151700     PERFORM IMS-STATUSKONTROLL                                           
151800     .                                                                    
151900     EJECT                                                                
152000 IMS-GU-WLKREE01    SECTION.                                              
152100                                                                          
152200     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
152300          DELIMITED BY SIZE INTO SSA1                                     
152400     MOVE '  GE' TO GODK-STATUSKODER                                      
152500     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA2 SSA1                     
152600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
152700     PERFORM IMS-STATUSKONTROLL                                           
152800     .                                                                    
152900                                                                          
153000 IMS-GNP-WLKREE11    SECTION.                                             
153100                                                                          
153200     MOVE 'WLKREE11 ' TO SSA1                                             
153300     MOVE '  GE' TO GODK-STATUSKODER                                      
153400     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1                    
153500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
153600     PERFORM IMS-STATUSKONTROLL                                           
153700     .                                                                    
153800                                                                          
153900 IMS-GU-WL411101  SECTION.                                                
154000                                                                          
154100     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-X ')'                         
154200                      DELIMITED BY SIZE INTO SSA1                         
154300     MOVE '  GE' TO GODK-STATUSKODER                                      
154400     CALL CBLTDLI USING GU 4111-PCB WL411101 SSA1                         
154500     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     .                                                                    
154800                                                                          
154900 IMS-GHNP-WL411111  SECTION.                                              
155000                                                                          
155100     MOVE  'WL411111*F' TO SSA1                                           
155200     MOVE '    ' TO GODK-STATUSKODER                                      
155300     CALL CBLTDLI USING GHNP 4111-PCB WL411111 SSA1                       
155400     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
155500     PERFORM IMS-STATUSKONTROLL                                           
155600     .                                                                    
155700     EJECT                                                                
155800 IMS-REPL-WL411111  SECTION.                                              
155900                                                                          
156000     MOVE '  ' TO GODK-STATUSKODER                                        
156100     CALL CBLTDLI USING REPL 4111-PCB WL411111                            
156200     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
156300     PERFORM IMS-STATUSKONTROLL                                           
156400     .                                                                    
156500 IMS-STATUSKONTROLL SECTION.                                              
156600                                                                          
156700     SET STATUS-IX TO 1                                                   
156800     SEARCH GODK-STATUS                                                   
156900       AT END                                                             
157000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
157100         DELIMITED BY SIZE INTO FELTEXT                                   
157200         CALL FELLOG                                                      
157300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
157400         CONTINUE                                                         
157500     END-SEARCH                                                           
157600     .                                                                    
157700     EJECT                                                                
