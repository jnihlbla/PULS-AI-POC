000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL015100.                                                
000400 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500 DATE-WRITTEN.   2004/08/23.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR RETURTILLSTÅNDSKÖ.                                         
001000*        ANVÄNDS FÖR UTSKRIFT AV RETURTILLSTÅND.                          
001100*                                                                         
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLRETA (WDA3)                              
001400*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
001500*        PROGRAMMET LÄSER              WDB6                               
001600*                                                                         
001700*        WL015100 PROGRAM IS A REPLICA OF W4073600 PROGRAM                
001800*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001900*                                                                         
002000*                                                                         
002100*    ADDRESS: 'CARPARTS.LDC.SHOWRETPERMITQUEUE                            
002200*                                                                         
002300*    E-TRACKER: 4230251 2007-02   SUSANNE OLSSON                          
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: WL0151U                                             
002700*        REQUEST:     WZ01REQU                                            
002800*                     WL0151I1                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        RESPONSE:    WZ01RESP                                            
003200*                     WL0151O1                                            
003300*                     WL0154I1                                            
003400*                                                                         
003500*    ETRACKER 10296404 RETURNS FROM CA TO US                              
003600*    ETRACKER 10302968 GENERIC SOLUTION IDFTG                             
003700                                                                          
003800     SKIP3                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'WL015100'.            
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004800 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  W-IDDISTR-SEC               PIC 9(4).                                
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
005600                                                                          
005700 77  WS-COUNT-PR                 PIC 9(05) VALUE ZERO.                    
005800                                                                          
005900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006000 77  WS-COUNT                    PIC S9(4)  VALUE +0    COMP SYNC.        
006100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006200 77  4794-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
006300 77  MAX-INDX                    PIC S9(4)  VALUE +2000 COMP SYNC.        
006400 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
006500                                                                          
006600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006700 77  WS-IDDISTR                  PIC  X(4)  VALUE SPACE.                  
006800 77  WS-IDKUNDNR                 PIC  X(6)  VALUE SPACE.                  
006900 77  WS-KDLEVANM                 PIC  X(1)  VALUE SPACE.                  
007000 77  WS-FLSUM                    PIC  X(1)  VALUE SPACE.                  
007100                                                                          
007200 77  W-KVRT                      PIC S9(5)  VALUE ZERO COMP-3.            
007300 77  W-KVRADER                   PIC S9(5)  VALUE ZERO COMP-3.            
007400 77  W-ANM-AVIS                  PIC X(1)   VALUE '4'.                    
007500 77  W-ANM-MOT                   PIC X(1)   VALUE '5'.                    
007600 77  W-ANM-PAAB                  PIC X(1)   VALUE '6'.                    
007700                                                                          
007800 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
007900     88  REC-LIMIT                           VALUE 'J'.                   
008000                                                                          
008100 77  SW-FLCMD                    PIC X       VALUE 'N'.                   
008200     88  FLCMD-IFYLLT                        VALUE 'J'.                   
008300                                                                          
008400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008500     88  INDATA-OK                           VALUE 'J'.                   
008600     88  INDATA-FEL                          VALUE 'N'.                   
008700                                                                          
008800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008900     88  NYCKLAR-OK                          VALUE 'J'.                   
009000     88  NYCKLAR-FEL                         VALUE 'N'.                   
009100                                                                          
009200*    --- DATUM                                                            
009300                                                                          
009400 01  W-TIAADDD.                                                           
009500     03  FILLER                  PIC 9(1)   VALUE ZERO.                   
009600     03  W-TIAA                  PIC 9(2)   VALUE ZERO.                   
009700     03  W-TIDDD                 PIC 9(3)   VALUE ZERO.                   
009800                                                                          
009900 01  W-TIAADDD-IDAG.                                                      
010000     03  W-TIAA-IDAG             PIC 9(2).                                
010100     03  W-TIDDD-IDAG            PIC 9(3).                                
010200                                                                          
010300 77  WS-IDELMT-ERROR             PIC X(16).                               
010400 77  WS-IDMSG-ERROR              PIC X(03) VALUE SPACE.                   
010500 77  WS-IDMSG-INFO               PIC X(03) VALUE SPACE.                   
010600                                                                          
010700*    --- PARAMETERS TO ABEND                                              
010800                                                                          
010900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011200     SKIP3                                                                
011300                                                                          
011400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011500 01  GENERELLA-SUBPROGRAM.                                                
011600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012200     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
012300                                                                          
012400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012500*01 -COPY WMEDAREA                                                        
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012800*01 -COPY WDATAREA                                                        
012900     EJECT                                                                
013000                                                                          
013100*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
013200*   -COPY WSECAREA                                                        
013300     EJECT                                                                
013400 01  MESSAGE-CODES.                                                       
013500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013600     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
013700     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
013800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014200     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
014300     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
014400     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '231'.                 
014500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014600     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
014700     03  ERR-USER-NOT-AUTHORIZED PIC X(3)    VALUE '405'.                 
014800                                                                          
014900*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
015000   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
015100     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
015200                                                                          
015300     EJECT                                                                
015400*                                                                         
015500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015800     SKIP3                                                                
015900*01  -COPY WZ01SUB                                                        
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
016200     SKIP3                                                                
016300 01  REQU-AREA.                                                           
016400*    03  -COPY WZ01REQU                                                   
016500*    03  -COPY WL0151I1                                                   
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
016800     SKIP3                                                                
016900 01  RESP-AREA.                                                           
017000*    03  -COPY WZ01RESP                                                   
017100*    03  -COPY WL0151O1                                                   
017200     EJECT                                                                
017300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017400*                                                                         
017500                                                                          
017600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017700     SKIP3                                                                
017800 01  W-MINKEY-X.                                                          
017900     03  W-MINKEY-IDTRANS          PIC  X(4)   VALUE '4736'.              
018000     SKIP3                                                                
018100                                                                          
018200 01  NYCKLAR-TILL-DLI.                                                    
018300                                                                          
018400     03  W-IDLEVANM-X.                                                    
018500         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
018600         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
018700         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
018800                                                                          
018900     03  W-WDA3BSEQ-X.                                                    
019000         05  W-IDRT-BSEQ         PIC  X(3)          VALUE SPACE.          
019100         05  W-IDDC-BSEQ         PIC  X(2)          VALUE SPACE.          
019200         05  W-IDRTLOP-BSEQ      PIC  9(3)          VALUE ZERO.           
019300         05  W-IDKOLLI-BSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
019400                                                                          
019500     03  W-WDA2C1KY-MIN-X.                                                
019600         05  W-IDFTG-C1-MIN      PIC  9(2)          VALUE ZERO.           
019700         05  W-KDLEVANM-C1-MIN   PIC  X(1)          VALUE ZERO.           
019800         05  W-KDARBTYP-C1-MIN   PIC  X(8)          VALUE SPACE.          
019900         05  W-IDPERSON-C1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
020000         05  W-DARETANK-C1-MIN   PIC  9(8)          VALUE ZERO.           
020100         05  W-DARETILL-C1-MIN   PIC  9(8)          VALUE ZERO.           
020200         05  W-IDDISTR-C1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
020300         05  W-IDKUNDNR-C1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
020400         05  W-IDRAPPNR-C1-MIN   PIC  9(7)          VALUE ZERO.           
020500                                                                          
020600     03  W-WDA2C1KY-MAX-X.                                                
020700         05  W-IDFTG-C1-MAX      PIC  9(2)          VALUE ZERO.           
020800         05  W-KDLEVANM-C1-MAX   PIC  X(1)          VALUE ZERO.           
020900         05  W-KDARBTYP-C1-MAX   PIC  X(8)          VALUE SPACE.          
021000         05  W-IDPERSON-C1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
021100         05  W-DARETANK-C1-MAX   PIC  9(8)          VALUE ZERO.           
021200         05  W-DARETILL-C1-MAX   PIC  9(8)          VALUE ZERO.           
021300         05  W-IDDISTR-C1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
021400         05  W-IDKUNDNR-C1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
021500         05  W-IDRAPPNR-C1-MAX   PIC  9(7)          VALUE ZERO.           
021600                                                                          
021700     03  W-WDA3FSEQ-MIN-X.                                                
021800         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
021900         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
022000         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
022100         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
022200                                                                          
022300     03  W-WDA3FSEQ-MAX-X.                                                
022400         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
022500         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
022600         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
022700         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
022800                                                                          
022900     03  W-IDDISTR-MIN-X.                                                 
023000         05  W-IDDISTR-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
023100                                                                          
023200     03  W-IDDISTR-MAX-X.                                                 
023300         05  W-IDDISTR-MAX       PIC S9(5)   COMP-3 VALUE ZERO.           
023400                                                                          
023500     03  W-IDKUNDNR-MIN-X.                                                
023600         05  W-IDKUNDNR-MIN      PIC S9(7)   COMP-3 VALUE ZERO.           
023700                                                                          
023800     03  W-IDKUNDNR-MAX-X.                                                
023900         05  W-IDKUNDNR-MAX      PIC S9(7)   COMP-3 VALUE ZERO.           
024000                                                                          
024100     03  W-WDGX4107-X.                                                    
024200         05  W-IDHTYP-4107       PIC X(4)        VALUE '4107'.            
024300         05  FILLER              PIC X(26)       VALUE LOW-VALUE.         
024400                                                                          
024500     03  W-KDSEGKEY-X.                                                    
024600         05  W-KDSEGKEY          PIC X(1)        VALUE '1'.               
024700                                                                          
024800     03  W-IDDC-B6-X.                                                     
024900         05 W-IDDC-B6            PIC X(2).                                
025000                                                                          
025100                                                                          
025200*    --- STATUS-KOD FRÅN IMS                                              
025300 01  STATUS-WS                   PIC XX.                                  
025400     88  STATUS-OK                           VALUE '  '.                  
025500     88  SEGMENT-FINNS                       VALUE '  '.                  
025600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
025900     88  TRANSKOD-FEL                        VALUE 'A1'.                  
026000     88  SECURITY-FEL                        VALUE 'A4'.                  
026100                                                                          
026200 01  GODK-STATUSKODER.                                                    
026300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026400                                                                          
026500 01  SSA1                        PIC X(192).                              
026600 01  SSA2                        PIC X(32).                               
026700                                                                          
026800*    --- IMS FUNKTIONSKODER                                               
026900*01  -COPY W0003                                                          
027000     EJECT                                                                
027100*    ---  DLI INPUT-OUTPUT AREA                                           
027200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027300     SKIP3                                                                
027400 01  DLI-IO-AREA.                                                         
027500     03  IO-AREA                 PIC X(192)  VALUE SPACE.                 
027600     SKIP3                                                                
027700     03  WLRETA01 REDEFINES IO-AREA.                                      
027800*        05  -COPY WDA301                                                 
027900     EJECT                                                                
028000     03  WLKREE01 REDEFINES IO-AREA.                                      
028100*        05  -COPY WDA201                                                 
028200     EJECT                                                                
028300     03  WLKREH01 REDEFINES IO-AREA.                                      
028400*        05  -COPY WDA2C1                                                 
028500     EJECT                                                                
028600     03  WL410711 REDEFINES IO-AREA.                                      
028700*        05  -COPY WDGX4108                                               
028800                                                                          
028900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
029000 01   DLI-IO-AREA-B601.                                                   
029100*     03  -COPY WDB601                                                    
029200     EJECT                                                                
029300 LINKAGE SECTION.                                                         
029400                                                                          
029500 01  MSG-PCB                     PIC X.                                   
029600                                                                          
029700*01  -COPY W0008   -PRE KREH-                                             
029800     05  FILLER                  PIC X.                                   
029900                                                                          
030000*01  -COPY W0008  -PRE KREE-                                              
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008  -PRE RETA-                                              
030400     05  FILLER                  PIC X.                                   
030500                                                                          
030600*01  -COPY W0008  -PRE 4107-                                              
030700     05  FILLER                  PIC X.                                   
030800                                                                          
030900*01  -COPY W0008  -PRE WDB6-                                              
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200 PROCEDURE DIVISION  USING MSG-PCB                                        
031300                           KREH-PCB KREE-PCB  RETA-PCB 4107-PCB           
031400                           WDB6-PCB.                                      
031500                                                                          
031600     ENTRY 'DLITCBL' USING MSG-PCB                                        
031700                           KREH-PCB KREE-PCB  RETA-PCB 4107-PCB           
031800                           WDB6-PCB.                                      
031900                                                                          
032000     PERFORM S06-FETCH-REQUEST-ARGUMENT                                   
032100     IF SUB-KDRC = 0                                                      
032200       PERFORM A-INIT                                                     
032300       PERFORM B-KOLLA-NYCKLAR                                            
032400       IF NYCKLAR-OK                                                      
032500         IF REQU-KDPGMACT = 'E'                                           
032600           PERFORM G-KOLLA-INPUT                                          
032700           IF INDATA-OK                                                   
032800             PERFORM H-UPPDATERA-SKRIV-UT                                 
032900           END-IF                                                         
033000         END-IF                                                           
033100         IF INDATA-OK                                                     
033200           PERFORM F-LAES-VISA-INFO                                       
033300         END-IF                                                           
033400       END-IF                                                             
033500                                                                          
033600       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
033700       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
033800       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
033900       IF WS-IDMSG-INFO NOT = SPACE                                       
034000         MOVE SPACE            TO RESP-IDMSG-ERROR                        
034100         MOVE SPACE            TO RESP-IDELMT-ERROR                       
034200       ELSE                                                               
034300         IF WS-IDMSG-ERROR NOT = SPACE                                    
034400           MOVE ALL '+' TO RESP-AREA                                      
034500           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
034600           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
034700           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
034800           MOVE  001             TO RESP-IDMSGVER                         
034900           IF REQU-KDPGMACT = 'S'                                         
035000             MOVE ZERO             TO RESP-KVRADER                        
035100           ELSE                                                           
035200             IF REQU-KVRADER NUMERIC                                      
035300               MOVE REQU-KVRADER     TO RESP-KVRADER                      
035400             ELSE                                                         
035500               MOVE ZERO             TO RESP-KVRADER                      
035600             END-IF                                                       
035700           END-IF                                                         
035800         END-IF                                                           
035900       END-IF                                                             
036000       PERFORM S07-RETURN-RESPONSE                                        
036100     END-IF                                                               
036200                                                                          
036300     MOVE ZERO TO RETURN-CODE                                             
036400     GOBACK                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 A-INIT SECTION.                                                          
036800                                                                          
036900     MOVE ALL '+' TO RESP-AREA                                            
037000     MOVE SPACE   TO RESP-IDMSG-INFO                                      
037100                     RESP-IDMSG-ERROR                                     
037200                     RESP-IDELMT-ERROR                                    
037300     MOVE 001     TO RESP-IDMSGVER                                        
037400     MOVE ZERO    TO RESP-KVRADER                                         
037500     MOVE ZERO    TO WS-COUNT-PR                                          
037600                                                                          
037700                                                                          
037800     MOVE LOW-VALUE         TO W-WDA2C1KY-MIN-X                           
037900                               W-IDDISTR-MIN-X                            
038000                               W-IDKUNDNR-MIN-X                           
038100                                                                          
038200     MOVE HIGH-VALUE        TO W-WDA2C1KY-MAX-X                           
038300                               W-IDDISTR-MAX-X                            
038400                               W-IDKUNDNR-MAX-X                           
038500                                                                          
038600*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
038700*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
038800*    IF REQU-IDUSER = 'PHCA4G1'                                           
038900*       AND REQU-IDDC-KEY = '44'                                          
039000*      MOVE '54'           TO REQU-IDFTG-KEY                              
039100*    END-IF                                                               
039200*    END FIX                                                              
039300                                                                          
039400*    MOVE REQU-IDFTG-KEY    TO W-IDFTG-C1-MIN                             
039500*                              W-IDFTG-C1-MAX                             
039600     IF REQU-IDFTG-KEY NOT NUMERIC                                        
039700       MOVE NEJ             TO NYCKLAR-SW                                 
039800       MOVE 'IDFTG'         TO RESP-IDELMT-ERROR                          
039900       MOVE '023'           TO RESP-IDMSG-ERROR                           
040000     ELSE                                                                 
040100       MOVE REQU-IDFTG-KEY  TO W-IDFTG-C1-MIN                             
040200                               W-IDFTG-C1-MAX                             
040300     END-IF                                                               
040400     .                                                                    
040500                                                                          
040600 B-KOLLA-NYCKLAR SECTION.                                                 
040700                                                                          
040800                                                                          
040900     MOVE JA TO NYCKLAR-SW                                                
041000                                                                          
041100     PERFORM BA-KOLLA-IDANSV                                              
041200     PERFORM BB-KOLLA-IDDISTR                                             
041300     PERFORM BC-KOLLA-IDKUNDNR                                            
041400     PERFORM BD-KOLLA-KDLEVANM                                            
041500     PERFORM BE-KOLLA-FLSUM                                               
041600     PERFORM BF-KOLLA-IDDC                                                
041700                                                                          
041800     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
041900       MOVE REQU-IDUSER          TO SEC-IDUSER                            
042000       MOVE 'L151'               TO SEC-IDTRANS                           
042100       MOVE REQU-IDDISTR-KEY     TO WS-IDDISTR                            
042200       MOVE WS-IDDISTR           TO SEC-IDKEY                             
042300                                                                          
042400       CALL WSECURIT USING SEC-IDUSER                                     
042500                           SEC-IDTRANS                                    
042600                           SEC-IDKEY                                      
042700                           SEC-KDSVAR                                     
042800                                                                          
042900       IF SEC-KDSVAR = 'F'                                                
043000         MOVE '00A' TO RESP-IDMSG-ERROR                                   
043100         MOVE NEJ   TO NYCKLAR-SW                                         
043200       END-IF                                                             
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600                                                                          
043700 BA-KOLLA-IDANSV    SECTION.                                              
043800                                                                          
043900*    -- IDANSV HÄMTAS FRÅN WDB6                                           
044000                                                                          
044100     MOVE 'RET'                TO  W-KDARBTYP-C1-MIN                      
044200                                   W-KDARBTYP-C1-MAX                      
044300                                                                          
044400     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
044500     PERFORM IMS-GU-WDB601                                                
044600                                                                          
044700     MOVE DCS-IDPERSON-REM     TO  W-IDPERSON-C1-MIN                      
044800                                   W-IDPERSON-C1-MAX                      
044900     .                                                                    
045000                                                                          
045100 BB-KOLLA-IDDISTR  SECTION.                                               
045200                                                                          
045300     IF REQU-IDDISTR-KEY  NUMERIC AND REQU-IDDISTR-KEY > ZERO             
045400       MOVE REQU-IDDISTR-KEY    TO W-IDDISTR-MIN                          
045500                                   W-IDDISTR-MAX                          
045600                                   RESP-IDDISTR-KEY                       
045700     ELSE                                                                 
045800       IF REQU-IDDISTR-KEY = ALL '+' OR SPACE                             
045900         CONTINUE                                                         
046000       ELSE                                                               
046100         MOVE NEJ                TO NYCKLAR-SW                            
046200         MOVE 'IDDISTR'          TO RESP-IDELMT-ERROR                     
046300         MOVE '023'              TO RESP-IDMSG-ERROR                      
046400       END-IF                                                             
046500     END-IF                                                               
046600     .                                                                    
046700     EJECT                                                                
046800 BC-KOLLA-IDKUNDNR   SECTION.                                             
046900                                                                          
047000     IF REQU-IDKUNDNR-KEY   NUMERIC AND REQU-IDKUNDNR-KEY > ZERO          
047100       MOVE REQU-IDKUNDNR-KEY   TO W-IDKUNDNR-MIN                         
047200                                   W-IDKUNDNR-MAX                         
047300                                   RESP-IDKUNDNR-KEY                      
047400     ELSE                                                                 
047500       IF REQU-IDKUNDNR-KEY = ALL '+' OR SPACE                            
047600         CONTINUE                                                         
047700       ELSE                                                               
047800         MOVE NEJ                TO NYCKLAR-SW                            
047900         MOVE 'IDKUNDNR'         TO RESP-IDELMT-ERROR                     
048000         MOVE '023'              TO RESP-IDMSG-ERROR                      
048100       END-IF                                                             
048200     END-IF                                                               
048300     .                                                                    
048400                                                                          
048500 BD-KOLLA-KDLEVANM   SECTION.                                             
048600                                                                          
048700     IF REQU-KDLEVANM-KEY NUMERIC                                         
048800                                                                          
048900       IF  REQU-KDLEVANM-KEY > '3'                                        
049000       AND REQU-KDLEVANM-KEY < '7'                                        
049100                                                                          
049200         MOVE REQU-KDLEVANM-KEY TO W-KDLEVANM-C1-MIN                      
049300                                   W-KDLEVANM-C1-MAX                      
049400                                   RESP-KDLEVANM-KEY                      
049500       ELSE                                                               
049600         MOVE NEJ            TO NYCKLAR-SW                                
049700         MOVE 'KDLEVANM'     TO RESP-IDELMT-ERROR                         
049800         MOVE '235'          TO RESP-IDMSG-ERROR                          
049900       END-IF                                                             
050000     ELSE                                                                 
050100       IF REQU-KDLEVANM-KEY = ALL '+' OR SPACE                            
050200         MOVE W-ANM-MOT      TO W-KDLEVANM-C1-MIN                         
050300                                W-KDLEVANM-C1-MAX                         
050400                                REQU-KDLEVANM-KEY                         
050500                                RESP-KDLEVANM-KEY                         
050600       ELSE                                                               
050700         MOVE NEJ            TO NYCKLAR-SW                                
050800         MOVE 'KDLEVANM'     TO RESP-IDELMT-ERROR                         
050900         MOVE '023'          TO RESP-IDMSG-ERROR                          
051000       END-IF                                                             
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 BE-KOLLA-FLSUM      SECTION.                                             
051500                                                                          
051600     IF REQU-FLSUM-KEY          = ALL '+'                                 
051700       CONTINUE                                                           
051800     ELSE                                                                 
051900       MOVE REQU-FLSUM-KEY      TO WS-FLSUM                               
052000                                   RESP-FLSUM-KEY                         
052100     END-IF                                                               
052200     IF WS-FLSUM = JA OR YES                                              
052300       CONTINUE                                                           
052400     ELSE                                                                 
052500       MOVE NEJ TO WS-FLSUM                                               
052600                   RESP-FLSUM-KEY                                         
052700     END-IF                                                               
052800                                                                          
052900     .                                                                    
053000     EJECT                                                                
053100 BF-KOLLA-IDDC       SECTION.                                             
053200                                                                          
053300     IF REQU-IDDC-KEY          = ALL '+'                                  
053400       MOVE NEJ            TO NYCKLAR-SW                                  
053500       MOVE 'IDDC'         TO RESP-IDELMT-ERROR                           
053600       MOVE '026'          TO RESP-IDMSG-ERROR                            
053700                                                                          
053800     ELSE                                                                 
053900       MOVE REQU-IDDC-KEY  TO RESP-IDDC-KEY                               
054000                              W-IDDC-BSEQ                                 
054100                              W-IDDC-FSEQ-MIN                             
054200                              W-IDDC-FSEQ-MAX                             
054300     END-IF                                                               
054400     .                                                                    
054500                                                                          
054600 F-LAES-VISA-INFO SECTION.                                                
054700                                                                          
054800     PERFORM S05-BERAKNA-DATUM                                            
054900                                                                          
055000     MOVE ZERO         TO W-KVRT                                          
055100                          W-KVRADER                                       
055200                                                                          
055300     PERFORM IMS-GU-WLKREH01                                              
055400     IF SEGMENT-FINNS                                                     
055500       PERFORM S02-KOLLA-BEHORIGHET                                       
055600     END-IF                                                               
055700                                                                          
055800     IF SEGMENT-SAKNAS                                                    
055900       MOVE 'IDRT-IDRTLOP'   TO RESP-IDELMT-ERROR                         
056000       MOVE '025'            TO RESP-IDMSG-ERROR                          
056100     ELSE                                                                 
056200       MOVE +1    TO INDX                                                 
056300       MOVE +0    TO WS-COUNT                                             
056400                                                                          
056500       PERFORM UNTIL INDX > MAX-INDX                                      
056600         IF SEGMENT-FINNS                                                 
056700           ADD +1               TO W-KVRT                                 
056800           COMPUTE W-KVRADER    =  W-KVRADER                              
056900                                +  SEQC-KVRADER-OBEH                      
057000                                                                          
057100           PERFORM FB-REDIGERA-MOD                                        
057200                                                                          
057300           PERFORM IMS-GN-WLKREH01                                        
057400           IF SEGMENT-FINNS                                               
057500             PERFORM S02-KOLLA-BEHORIGHET                                 
057600           END-IF                                                         
057700           ADD +1 TO WS-COUNT                                             
057800         END-IF                                                           
057900         ADD +1  TO INDX                                                  
058000       END-PERFORM                                                        
058100                                                                          
058200       MOVE WS-COUNT  TO RESP-KVRADER                                     
058300                                                                          
058400       IF WS-COUNT = 2000                                                 
058500         MOVE '028'  TO RESP-IDMSG-ERROR                                  
058600       END-IF                                                             
058700                                                                          
058800       IF SEGMENT-FINNS AND (WS-FLSUM = JA OR YES)                        
058900         PERFORM FE-ADDERA-RT                                             
059000       END-IF                                                             
059100                                                                          
059200       IF WS-FLSUM             =  JA OR YES                               
059300         MOVE W-KVRADER       TO RESP-KVRADER-RT                          
059400         MOVE W-KVRT          TO RESP-KVANT-RT                            
059500       END-IF                                                             
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900                                                                          
060000                                                                          
060100 FB-REDIGERA-MOD         SECTION.                                         
060200                                                                          
060300     MOVE SEQC-DARETILL(3:6) TO RESP-TIRETILL (INDX)                      
060400     MOVE SEQC-DARETANK(3:6) TO RESP-TIRETANK (INDX)                      
060500                                                                          
060600     MOVE SEQC-IDDISTR      TO RESP-IDDISTR (INDX)                        
060700     MOVE SEQC-IDKUNDNR     TO RESP-IDKUNDNR (INDX)                       
060800     MOVE SEQC-IDRAPPNR     TO RESP-IDRAPPNR (INDX)                       
060900     MOVE SEQC-KVRADER-RT   TO RESP-KVRADER-TOT (INDX)                    
061000     MOVE SEQC-KVRADER-OBEH TO RESP-KVRADER-OBEH (INDX)                   
061100     MOVE SEQC-KDLEVANM     TO WS-KDLEVANM                                
061200                                                                          
061300     PERFORM FBA-LAES-ANTAL-KOLLI                                         
061400     .                                                                    
061500     EJECT                                                                
061600                                                                          
061700 FBA-LAES-ANTAL-KOLLI     SECTION.                                        
061800                                                                          
061900     MOVE SEQC-IDDISTR    TO W-IDDISTR-FSEQ-MIN                           
062000                             W-IDDISTR-FSEQ-MAX                           
062100     MOVE SEQC-IDKUNDNR   TO W-IDKUNDNR-FSEQ-MIN                          
062200                             W-IDKUNDNR-FSEQ-MAX                          
062300     MOVE SEQC-IDRAPPNR   TO W-IDRAPPNR-FSEQ-MIN                          
062400                             W-IDRAPPNR-FSEQ-MAX                          
062500                                                                          
062600     PERFORM IMS-GU-WLRETA01                                              
062700                                                                          
062800     IF SEGMENT-FINNS                                                     
062900       IF WS-KDLEVANM       =  W-ANM-MOT                                  
063000          MOVE RET-KVKOLLI-AAF TO RESP-KVKOLLI (INDX)                     
063100       ELSE                                                               
063200          MOVE ZERO         TO RESP-KVKOLLI (INDX)                        
063300       END-IF                                                             
063400       MOVE RET-IDRETSND    TO RESP-IDRETSND(INDX)                        
063500     ELSE                                                                 
063600       MOVE ZERO            TO RESP-KVKOLLI (INDX)                        
063700       MOVE SPACE           TO RESP-IDRETSND(INDX)                        
063800     END-IF                                                               
063900     .                                                                    
064000     EJECT                                                                
064100 FE-ADDERA-RT  SECTION.                                                   
064200                                                                          
064300     PERFORM UNTIL SEGMENT-SAKNAS                                         
064400       ADD +1               TO W-KVRT                                     
064500       COMPUTE W-KVRADER    =  W-KVRADER + SEQC-KVRADER-OBEH              
064600                                                                          
064700       PERFORM IMS-GN-WLKREH01                                            
064800     END-PERFORM                                                          
064900     .                                                                    
065000                                                                          
065100 G-KOLLA-INPUT SECTION.                                                   
065200                                                                          
065300     MOVE JA TO INDATA-SW                                                 
065400                                                                          
065500     PERFORM GA-FORMELL-KONTROLL                                          
065600     .                                                                    
065700                                                                          
065800 GA-FORMELL-KONTROLL SECTION.                                             
065900                                                                          
066000     PERFORM GAA-KOLLA-FLCMD                                              
066100                                                                          
066200     .                                                                    
066300                                                                          
066400 GAA-KOLLA-FLCMD      SECTION.                                            
066500                                                                          
066600     MOVE NEJ  TO SW-FLCMD                                                
066700                                                                          
066800     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
066900       MOVE +1           TO INDX                                          
067000       MOVE REQU-KVRADER TO WS-INDX-REC                                   
067100       MOVE NEJ          TO WS-REC-LIMIT                                  
067200                                                                          
067300       PERFORM UNTIL INDX       >  MAX-INDX OR REC-LIMIT                  
067400         IF REQU-FLCMD(INDX)   NOT = '+' AND ' ' AND 'N'                  
067500           MOVE REQU-FLCMD(INDX)  TO RESP-FLCMD (INDX)                    
067600                                                                          
067700           IF REQU-FLCMD(INDX)          = 'Y' OR 'J'                      
067800             MOVE JA  TO SW-FLCMD                                         
067900           ELSE                                                           
068000             MOVE NEJ TO INDATA-SW                                        
068100           END-IF                                                         
068200         END-IF                                                           
068300         IF INDX = WS-INDX-REC                                            
068400           MOVE JA TO WS-REC-LIMIT                                        
068500         ELSE                                                             
068600           ADD +1  TO INDX                                                
068700         END-IF                                                           
068800       END-PERFORM                                                        
068900                                                                          
069000       IF FLCMD-IFYLLT                                                    
069100         CONTINUE                                                         
069200       ELSE                                                               
069300         MOVE NEJ   TO INDATA-SW                                          
069400         MOVE 'CMD' TO RESP-IDELMT-ERROR                                  
069500         MOVE '026' TO RESP-IDMSG-ERROR                                   
069600       END-IF                                                             
069700     ELSE                                                                 
069800       MOVE NEJ     TO INDATA-SW                                          
069900       IF REQU-KVRADER = 0                                                
070000         MOVE 'KVRADER' TO RESP-IDELMT-ERROR                              
070100         MOVE '126'     TO RESP-IDMSG-ERROR                               
070200       ELSE                                                               
070300         MOVE 'KVRADER' TO RESP-IDELMT-ERROR                              
070400         MOVE '024'     TO RESP-IDMSG-ERROR                               
070500       END-IF                                                             
070600     END-IF                                                               
070700     .                                                                    
070800                                                                          
070900 H-UPPDATERA-SKRIV-UT SECTION.                                            
071000                                                                          
071100     PERFORM HA-BEHANDLA-VALDA-TILLSTAND                                  
071200     .                                                                    
071300                                                                          
071400 HA-BEHANDLA-VALDA-TILLSTAND SECTION.                                     
071500                                                                          
071600     MOVE +1                       TO INDX                                
071700                                      4794-IX                             
071800     MOVE NEJ TO WS-REC-LIMIT                                             
071900                                                                          
072000     PERFORM UNTIL INDX > MAX-INDX                                        
072100                OR REC-LIMIT                                              
072200                                                                          
072300       IF REQU-FLCMD(INDX) = 'J' OR 'Y'                                   
072310         IF REQU-IDDISTR(INDX) NOT NUMERIC                                
072320            MOVE ZERO TO REQU-IDDISTR(INDX)                               
072330         END-IF                                                           
072331         IF REQU-IDKUNDNR(INDX) NOT NUMERIC                               
072332            MOVE ZERO TO REQU-IDKUNDNR(INDX)                              
072333         END-IF                                                           
072334         IF REQU-IDRAPPNR(INDX) NOT NUMERIC                               
072335            MOVE ZERO TO REQU-IDRAPPNR(INDX)                              
072336         END-IF                                                           
072340                                                                          
072400         MOVE REQU-IDDISTR(INDX)  TO W-IDDISTR                            
072500                                    W-IDDISTR-FSEQ-MIN                    
072700         MOVE REQU-IDKUNDNR(INDX) TO W-IDKUNDNR                           
072800                                    W-IDKUNDNR-FSEQ-MIN                   
073000         MOVE REQU-IDRAPPNR(INDX) TO W-IDRAPPNR                           
073100                                    W-IDRAPPNR-FSEQ-MIN                   
073200         IF 4794-IX < 11                                                  
073300           MOVE REQU-IDDISTR(INDX)  TO RESP-IDDISTR-REP  (4794-IX)        
073330           MOVE REQU-IDKUNDNR(INDX) TO RESP-IDKUNDNR-REP (4794-IX)        
073360           MOVE REQU-IDRAPPNR(INDX) TO RESP-IDRAPPNR-REP (4794-IX)        
073361           ADD +1                   TO 4794-IX                            
073370         END-IF                                                           
073380                                                                          
073500         ADD +1                 TO WS-COUNT-PR                            
073600                                                                          
073700         IF WS-COUNT-PR < 11                                              
073800           CONTINUE                                                       
073900         ELSE                                                             
074000           MOVE '274'       TO RESP-IDMSG-ERROR                           
074100           MOVE NEJ         TO INDATA-SW                                  
074200         END-IF                                                           
074300                                                                          
074400         PERFORM IMS-GHU-WLKREE01                                         
074500         IF SEGMENT-FINNS AND ANM-KDLEVANM = W-ANM-MOT                    
074600           MOVE W-ANM-PAAB      TO ANM-KDLEVANM                           
074700           PERFORM IMS-REPL-WLKREE01                                      
074800           IF STATUS-OK                                                   
074900             MOVE '001'  TO RESP-IDMSG-INFO                               
075000           ELSE                                                           
075100             MOVE '004'  TO RESP-IDMSG-ERROR                              
075200           END-IF                                                         
075300** UPPDATERINGEN BORTTAGEN DÅ DEN STÖR SORTORDNINGEN I KOLLIKÖN           
075400         END-IF                                                           
075500       END-IF                                                             
075600       IF INDX = WS-INDX-REC                                              
075700         MOVE JA TO WS-REC-LIMIT                                          
075800       ELSE                                                               
075900         ADD +1  TO INDX                                                  
076000       END-IF                                                             
076100     END-PERFORM                                                          
076200                                                                          
076300     IF 4794-IX > +1                                                      
076400       PERFORM HAB-STARTA-4794                                            
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800                                                                          
076900 HAB-STARTA-4794  SECTION.                                                
077000                                                                          
077100                                                                          
077200     MOVE 'WL0151'              TO RESP-IDPGM-REP                         
077300                                                                          
077400     .                                                                    
077500                                                                          
077600 S02-KOLLA-BEHORIGHET SECTION.                                            
077700                                                                          
077800     MOVE 'F' TO SEC-KDSVAR                                               
077900     PERFORM UNTIL SEC-KDSVAR NOT = 'F'                                   
078000             OR    SEGMENT-SAKNAS                                         
078100             OR    SEGMENT-SLUT                                           
078200       MOVE REQU-IDUSER          TO SEC-IDUSER                            
078300       MOVE 'L151'               TO SEC-IDTRANS                           
078400       MOVE SEQC-IDDISTR         TO W-IDDISTR-SEC                         
078500       MOVE W-IDDISTR-SEC        TO SEC-IDKEY                             
078600                                                                          
078700       CALL WSECURIT USING SEC-IDUSER                                     
078800                           SEC-IDTRANS                                    
078900                           SEC-IDKEY                                      
079000                           SEC-KDSVAR                                     
079100                                                                          
079200       IF SEC-KDSVAR = 'F'                                                
079300          PERFORM IMS-GN-WLKREH01                                         
079400       END-IF                                                             
079500     END-PERFORM                                                          
079600     .                                                                    
079700     EJECT                                                                
079800 S05-BERAKNA-DATUM        SECTION.                                        
079900                                                                          
080000     PERFORM IMS-GU-WL410711                                              
080100                                                                          
080200     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
080300                                                                          
080400     CALL WDATKONV USING DAT-KDDATFORM                                    
080500                         DAT-I-TIDATUM                                    
080600                         DAT-O-TIDATUM                                    
080700                         DAT-KDSVAR                                       
080800     IF DAT-KDSVAR-FEL                                                    
080900       MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                               
081000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
081100     END-IF                                                               
081200                                                                          
081300     MOVE DAT-TIAADDD     TO W-TIAADDD-IDAG                               
081400                                                                          
081500     IF 4108-KVDAGAR-RTAOSEA >= W-TIDDD-IDAG                              
081600       IF W-TIAA-IDAG          = 00                                       
081700         MOVE 99 TO W-TIAA                                                
081800       ELSE                                                               
081900         COMPUTE W-TIAA = W-TIAA-IDAG  - 1                                
082000       END-IF                                                             
082100       COMPUTE W-TIDDD  = 365 - 4108-KVDAGAR-RTAOSEA                      
082200                        + W-TIDDD-IDAG                                    
082300       MOVE W-TIAADDD          TO DAT-I-TIDATUM                           
082400       MOVE 'AADDD'            TO DAT-KDDATFORM                           
082500                                                                          
082600       CALL WDATKONV USING DAT-KDDATFORM                                  
082700                           DAT-I-TIDATUM                                  
082800                           DAT-O-TIDATUM                                  
082900                           DAT-KDSVAR                                     
083000       IF DAT-KDSVAR-FEL                                                  
083100         MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                             
083200         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
083300       END-IF                                                             
083400     ELSE                                                                 
083500       MOVE W-TIAA-IDAG TO W-TIAA                                         
083600       COMPUTE W-TIDDD  =  W-TIDDD-IDAG -                                 
083700                           4108-KVDAGAR-RTAOSEA                           
083800       MOVE W-TIAADDD   TO DAT-I-TIDATUM                                  
083900       MOVE 'AADDD'     TO DAT-KDDATFORM                                  
084000                                                                          
084100       CALL WDATKONV USING DAT-KDDATFORM                                  
084200                           DAT-I-TIDATUM                                  
084300                           DAT-O-TIDATUM                                  
084400                           DAT-KDSVAR                                     
084500       IF DAT-KDSVAR-FEL                                                  
084600         MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                             
084700         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
084800       END-IF                                                             
084900     END-IF                                                               
085000                                                                          
085100     IF 4108-KVDAGAR-RTAOVR >= W-TIDDD-IDAG                               
085200       IF W-TIAA-IDAG = 00                                                
085300         MOVE 99 TO W-TIAA                                                
085400       ELSE                                                               
085500         COMPUTE W-TIAA = W-TIAA-IDAG  - 1                                
085600       END-IF                                                             
085700       COMPUTE W-TIDDD = 365 - 4108-KVDAGAR-RTAOVR + W-TIDDD-IDAG         
085800                                                                          
085900       MOVE W-TIAADDD          TO DAT-I-TIDATUM                           
086000       MOVE 'AADDD'            TO DAT-KDDATFORM                           
086100                                                                          
086200       CALL WDATKONV USING DAT-KDDATFORM                                  
086300                           DAT-I-TIDATUM                                  
086400                           DAT-O-TIDATUM                                  
086500                           DAT-KDSVAR                                     
086600       IF DAT-KDSVAR-FEL                                                  
086700         MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                             
086800         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
086900       END-IF                                                             
087000     ELSE                                                                 
087100       MOVE W-TIAA-IDAG        TO W-TIAA                                  
087200       COMPUTE W-TIDDD = W-TIDDD-IDAG - 4108-KVDAGAR-RTAOVR               
087300                                                                          
087400       MOVE W-TIAADDD          TO DAT-I-TIDATUM                           
087500       MOVE 'AADDD'            TO DAT-KDDATFORM                           
087600                                                                          
087700       CALL WDATKONV USING DAT-KDDATFORM                                  
087800                           DAT-I-TIDATUM                                  
087900                           DAT-O-TIDATUM                                  
088000                           DAT-KDSVAR                                     
088100       IF DAT-KDSVAR-FEL                                                  
088200         MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                             
088300         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
088400       END-IF                                                             
088500     END-IF                                                               
088600                                                                          
088700     IF 4108-KVDAGAR-RTM >= W-TIDDD-IDAG                                  
088800       IF W-TIAA-IDAG = 00                                                
088900         MOVE 99  TO W-TIAA                                               
089000       ELSE                                                               
089100         COMPUTE W-TIAA = W-TIAA-IDAG  - 1                                
089200       END-IF                                                             
089300       COMPUTE W-TIDDD = 365 - 4108-KVDAGAR-RTM + W-TIDDD-IDAG            
089400                                                                          
089500       MOVE W-TIAADDD          TO DAT-I-TIDATUM                           
089600       MOVE 'AADDD'            TO DAT-KDDATFORM                           
089700                                                                          
089800       CALL WDATKONV USING DAT-KDDATFORM                                  
089900                           DAT-I-TIDATUM                                  
090000                           DAT-O-TIDATUM                                  
090100                           DAT-KDSVAR                                     
090200       IF DAT-KDSVAR-FEL                                                  
090300         MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                             
090400         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
090500       END-IF                                                             
090600                                                                          
090700     ELSE                                                                 
090800       MOVE W-TIAA-IDAG        TO W-TIAA                                  
090900       COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                          
091000                                  4108-KVDAGAR-RTM                        
091100       MOVE W-TIAADDD          TO DAT-I-TIDATUM                           
091200       MOVE 'AADDD'            TO DAT-KDDATFORM                           
091300                                                                          
091400       CALL WDATKONV USING DAT-KDDATFORM                                  
091500                           DAT-I-TIDATUM                                  
091600                           DAT-O-TIDATUM                                  
091700                           DAT-KDSVAR                                     
091800       IF DAT-KDSVAR-FEL                                                  
091900         MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                             
092000         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
092100       END-IF                                                             
092200                                                                          
092300     END-IF                                                               
092400                                                                          
092500     IF 4108-KVDAGAR-RTP >= W-TIDDD-IDAG                                  
092600       IF W-TIAA-IDAG = 00                                                
092700         MOVE 99 TO W-TIAA                                                
092800       ELSE                                                               
092900         COMPUTE W-TIAA = W-TIAA-IDAG  - 1                                
093000       END-IF                                                             
093100       COMPUTE W-TIDDD = 365 - 4108-KVDAGAR-RTP + W-TIDDD-IDAG            
093200                                                                          
093300       MOVE W-TIAADDD          TO DAT-I-TIDATUM                           
093400       MOVE 'AADDD'            TO DAT-KDDATFORM                           
093500                                                                          
093600       CALL WDATKONV USING DAT-KDDATFORM                                  
093700                           DAT-I-TIDATUM                                  
093800                           DAT-O-TIDATUM                                  
093900                           DAT-KDSVAR                                     
094000       IF DAT-KDSVAR-FEL                                                  
094100          MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                            
094200          CALL ABEND USING RKOD-ABEND-MED-DUMP                            
094300       END-IF                                                             
094400     ELSE                                                                 
094500       MOVE W-TIAA-IDAG TO W-TIAA                                         
094600       COMPUTE W-TIDDD = W-TIDDD-IDAG - 4108-KVDAGAR-RTP                  
094700                                                                          
094800       MOVE W-TIAADDD          TO DAT-I-TIDATUM                           
094900       MOVE 'AADDD'            TO DAT-KDDATFORM                           
095000                                                                          
095100       CALL WDATKONV USING DAT-KDDATFORM                                  
095200                           DAT-I-TIDATUM                                  
095300                           DAT-O-TIDATUM                                  
095400                           DAT-KDSVAR                                     
095500       IF DAT-KDSVAR-FEL                                                  
095600         MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                             
095700         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
095800       END-IF                                                             
095900     END-IF                                                               
096000     .                                                                    
096100     EJECT                                                                
096200*    --- DISPATCHER SECTIONS                                              
096300 S06-FETCH-REQUEST-ARGUMENT SECTION.                                      
096400                                                                          
096500     MOVE 'GETARG'               TO SUB-KDFUNC                            
096600     MOVE 'CARPARTS.LDC.SHOWRETPERMITQUEUE' TO SUB-ADDISPABS              
096700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
096800                                                                          
096900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
097000                                                                          
097100     IF SUB-KDRC > 0                                                      
097200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
097300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
097400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
097500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
097600     END-IF                                                               
097700     .                                                                    
097800     SKIP3                                                                
097900 S07-RETURN-RESPONSE SECTION.                                             
098000                                                                          
098100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
098200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
098300                                                                          
098400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
098500                                                                          
098600     IF SUB-KDRC > 0                                                      
098700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
098800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
098900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
099000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400     SKIP3                                                                
099500* --- IMS SEKTIONER ---                                                   
099600     SKIP3                                                                
099700 IMS-GU-WL410711            SECTION.                                      
099800                                                                          
099900     STRING 'WL410701(WDGXKEY  =' W-WDGX4107-X ')'                        
100000          DELIMITED BY SIZE INTO SSA1                                     
100100     STRING 'WL410711(KDSEGKEY =' W-KDSEGKEY-X ')'                        
100200          DELIMITED BY SIZE INTO SSA2                                     
100300     MOVE '  '           TO GODK-STATUSKODER                              
100400     CALL CBLTDLI USING GU 4107-PCB DLI-IO-AREA SSA1 SSA2                 
100500     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
100600     PERFORM IMS-STATUSKONTROLL                                           
100700     .                                                                    
100800 IMS-GHU-WLKREE01       SECTION.                                          
100900                                                                          
101000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
101100          DELIMITED BY SIZE INTO SSA1                                     
101200     MOVE '  GE'           TO GODK-STATUSKODER                            
101300     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1                     
101400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700                                                                          
101800 IMS-REPL-WLKREE01      SECTION.                                          
101900                                                                          
102000     MOVE '    '           TO GODK-STATUSKODER                            
102100     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
102200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
102300     PERFORM IMS-STATUSKONTROLL                                           
102400     .                                                                    
102500                                                                          
102600 IMS-GU-WLKREH01       SECTION.                                           
102700                                                                          
102800     STRING 'WLKREH01(WDA2C1KY>=' W-WDA2C1KY-MIN-X                        
102900                    '&WDA2C1KY<=' W-WDA2C1KY-MAX-X                        
103000                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
103100                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
103200                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
103300                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
103400          DELIMITED BY SIZE INTO SSA1                                     
103500     MOVE '  GE'           TO GODK-STATUSKODER                            
103600     CALL CBLTDLI USING GU KREH-PCB DLI-IO-AREA SSA1                      
103700     MOVE KREH-STATUS-CODE TO STATUS-WS                                   
103800     PERFORM IMS-STATUSKONTROLL                                           
103900     .                                                                    
104000                                                                          
104100 IMS-GN-WLKREH01       SECTION.                                           
104200                                                                          
104300     STRING 'WLKREH01(WDA2C1KY>=' W-WDA2C1KY-MIN-X                        
104400                    '&WDA2C1KY<=' W-WDA2C1KY-MAX-X                        
104500                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
104600                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
104700                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
104800                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
104900          DELIMITED BY SIZE INTO SSA1                                     
105000     MOVE '  GEGB'           TO GODK-STATUSKODER                          
105100     CALL CBLTDLI USING GN KREH-PCB DLI-IO-AREA SSA1                      
105200     MOVE KREH-STATUS-CODE TO STATUS-WS                                   
105300     PERFORM IMS-STATUSKONTROLL                                           
105400     .                                                                    
105500                                                                          
105600 IMS-GU-WLRETA01       SECTION.                                           
105700                                                                          
105800     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
105900                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
106000          DELIMITED BY SIZE INTO SSA1                                     
106100     MOVE '  GE'           TO GODK-STATUSKODER                            
106200     CALL CBLTDLI USING GU RETA-PCB DLI-IO-AREA SSA1                      
106300     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
106400     PERFORM IMS-STATUSKONTROLL                                           
106500     .                                                                    
106600                                                                          
106700 IMS-GU-WDB601    SECTION.                                                
106800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
106900          DELIMITED BY SIZE INTO SSA1                                     
107000     MOVE '  GE' TO GODK-STATUSKODER                                      
107100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
107200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     IF SEGMENT-SAKNAS                                                    
107500         MOVE ZERO  TO DCS-IDPERSON-REM                                   
107600     END-IF                                                               
107700     .                                                                    
107800                                                                          
107900                                                                          
108000 IMS-STATUSKONTROLL SECTION.                                              
108100                                                                          
108200     SET STATUS-IX TO 1                                                   
108300     SEARCH GODK-STATUS                                                   
108400       AT END                                                             
108500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
108600         DELIMITED BY SIZE INTO FELTEXT                                   
108700         CALL FELLOG                                                      
108800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
108900         CONTINUE                                                         
109000     END-SEARCH                                                           
109100     .                                                                    
