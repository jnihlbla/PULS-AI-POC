000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL014600.                                                
000400 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500 DATE-WRITTEN.   2004/07/01.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR SÄNDNINGSKÖ FÖR RAPPORTERING FÖR LOSSADE OCH MOT-          
001000*        TAGNA SÄNDNINGAR.                                                
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001300*        PROGRAMMET LÄSER      WLRETB (WDA3)                              
001400*    SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                     
001500*                                                                         
001501*        WL014600 PROGRAM IS A REPLICA OF W4073100 PROGRAM                
001502*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001503*                                                                         
001504*                                                                         
001510* ADDRESS: 'CARPARTS.LDC.RETURNQUEUE'                                     
001520*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: WL0146U                                             
001800*        REQUEST:     WZ01REQU                                            
001810*                     WL0146I1                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        RESPONSE:    WZ01RESP                                            
002110*                     WL0146O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'WL014600'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003120 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003130 77  KDRC-DISPLAY                PIC Z(5).                                
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  W-CDC                       PIC X(3)    VALUE 'CDC'.                 
003610 77  W-US1                       PIC X(3)    VALUE 'US1'.                 
003620 77  W-US2                       PIC X(3)    VALUE 'US2'.                 
003630 77  W-US3                       PIC X(3)    VALUE 'US3'.                 
003640 77  W-CA1                       PIC X(3)    VALUE 'CA1'.                 
003650 77  W-JP1                       PIC X(3)    VALUE 'JP1'.                 
003660 77  W-AU1                       PIC X(3)    VALUE 'AU1'.                 
003670 77  W-SE1                       PIC X(3)    VALUE 'SE1'.                 
003680 77  W-GB1                       PIC X(3)    VALUE 'GB1'.                 
003690 77  W-SE2                       PIC X(3)    VALUE 'SE2'.                 
003691 77  W-GB2                       PIC X(3)    VALUE 'GB2'.                 
003692 77  W-GB3                       PIC X(3)    VALUE 'GB3'.                 
003693 77  W-NL1                       PIC X(3)    VALUE 'NL1'.                 
003694 77  W-IT1                       PIC X(3)    VALUE 'IT1'.                 
003700                                                                          
003800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003900 77  WS-COUNT                    PIC S9(4)  VALUE +0    COMP SYNC.        
003910 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004010 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
004020 77  WS-DEL-COUNT                PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  4792-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  4792-MAX-INDX               PIC S9(4)  VALUE +24   COMP SYNC.        
004300 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
004600                                                                          
004700 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
004800     88  REC-LIMIT                           VALUE 'J'.                   
004801                                                                          
004810 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004820     88  INDATA-OK                           VALUE 'J'.                   
004900     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005410 77  W-UPDATE-SW                 PIC X       VALUE 'N'.                   
005420     88  W-UPDATE-OK                         VALUE 'J'.                   
005430                                                                          
006000*    --- KONSTANTER                                                       
006120 77  W-RECEIVING                 PIC  X(3)  VALUE 'REC'.                  
006201 77  W-LOSSNING                  PIC  X(3)  VALUE 'LOA'.                  
006230 77  W-DELETE                    PIC  X(3)  VALUE 'DEL'.                  
006310 77  W-DEVIATION                 PIC  X(3)  VALUE 'DEV'.                  
006400 77  W-SND-REG                   PIC  X(1)  VALUE '1'.                    
006500 77  W-SND-SAENT                 PIC  X(1)  VALUE '2'.                    
006600 77  W-SND-LOSS                  PIC  X(1)  VALUE '3'.                    
006700 77  W-SND-MOT                   PIC  X(1)  VALUE '4'.                    
006800 77  W-KLI-LOSS                  PIC S9(1)  VALUE +4 COMP-3.              
006900 77  W-KLI-MOT                   PIC S9(1)  VALUE +5 COMP-3.              
007000 77  W-KLI-SAK                   PIC S9(1)  VALUE +6 COMP-3.              
007100 77  W-KLI-AVV                   PIC S9(1)  VALUE +7 COMP-3.              
007300                                                                          
007500 77  SPAR-IDKOLLI                PIC S9(5)  VALUE ZERO COMP-3.            
007510 77  W-IDANSTNR                  PIC S9(5)  VALUE ZERO COMP-3.            
007600 77  W-KDRETSTA-NUM              PIC  X(1)  VALUE ZERO.                   
007900 77  W-KVKOLLI-SND               PIC S9(5)  VALUE ZERO COMP-3.            
007902 77  W-KVKOLLI-MOT               PIC S9(5)  VALUE ZERO COMP-3.            
007910 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007920 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008000 77  W-KDRETSTA                  PIC X       VALUE 'T'.                   
008100     88  W-REG-KEY                           VALUE 'T'.                   
008200     88  W-SAENT-KEY                         VALUE 'S'.                   
008210     88  W-SEND-KEY                          VALUE 'I'.                   
008300     88  W-LOSS-KEY                          VALUE 'L'.                   
008400     88  W-MOT-KEY                           VALUE 'M'.                   
008410     88  W-REC-KEY                           VALUE 'R'.                   
008500     88  W-PAAB-KEY                          VALUE 'P'.                   
008510     88  W-INPR-KEY                          VALUE 'P'.                   
008600                                                                          
008700 77  SW-IDANSTNR                 PIC X       VALUE 'N'.                   
008800     88  IDANSTNR-IFYLLT                     VALUE 'J'.                   
008900                                                                          
009000 77  SW-IDRETSND                 PIC X       VALUE 'N'.                   
009100     88  IDRETSND-IFYLLT                     VALUE 'J'.                   
009200                                                                          
009210 77  SW-KDRETSTA                 PIC X       VALUE 'N'.                   
009220     88  KDRETSTA-IFYLLT                     VALUE 'J'.                   
009230                                                                          
009300 77  SW-ADINLOMR                 PIC X       VALUE 'N'.                   
009400     88  ADINLOMR-IFYLLT                     VALUE 'J'.                   
009800                                                                          
009842 77  WS-IDELMT-ERROR             PIC X(16).                               
009843 77  WS-IDMSG-ERROR              PIC X(03).                               
009844 77  WS-IDMSG-INFO               PIC X(03).                               
009845                                                                          
009850*    --- PARAMETERS TO ABEND                                              
009860                                                                          
009870 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009880 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009890 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009891     SKIP3                                                                
009900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010000 01  GENERELLA-SUBPROGRAM.                                                
010100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010510     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010520     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010800*01 -COPY WMEDAREA                                                        
010900     SKIP3                                                                
011900     EJECT                                                                
012500     SKIP3                                                                
012600*                                                                         
012700*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
012800   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
012900     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
013000                                                                          
013100 01  BILD-HOPP-AREOR.                                                     
014100                                                                          
014200   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
014300   03      P-TO-P-SW.                                                     
014400                                                                          
014500     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
014600     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
014700     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
014800     05  P-TO-P-KDTRANS          PIC X(8).                                
014900     05  P-TO-P-IDTRANS          PIC X(4).                                
015000     05  P-TO-P-KDMFSFOR         PIC X(1).                                
015100     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
015200                                                                          
015300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015310 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015320     SKIP3                                                                
015330*01  -COPY WZ01SUB                                                        
015340     EJECT                                                                
015350 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015360     SKIP3                                                                
015370 01  REQU-AREA.                                                           
015380*    03  -COPY WZ01REQU                                                   
015390*    03  -COPY WL0146I1                                                   
015391     EJECT                                                                
015392 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015393     SKIP3                                                                
015394 01  RESP-AREA.                                                           
015395*    03  -COPY WZ01RESP                                                   
015396*    03  -COPY WL0146O1                                                   
015397     EJECT                                                                
017500                                                                          
017600 77  W-KVKOLLI                   PIC  9(4)   VALUE ZERO.                  
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
017900     SKIP3                                                                
018000 01  KOM-MSG-IO-AREA.                                                     
018100*03  -COPY WMSGKOM                                                        
018200     EJECT                                                                
018300 01  FILLER                   PIC X(16)   VALUE 'MSG/KOM-AREA'.           
018400     SKIP2                                                                
018500*01  -COPY WMSGSNUF           -PRE P-TO-P-                                
018600                                                                          
018700     EJECT                                                                
018800 01      FILLER                  PIC X(24)   VALUE                        
018900                                 'MOD4792-MID-W4I79201'.                  
019000     SKIP2                                                                
019100     -COPY W4I79201 -PRE MOD4792-                                         
019200     EJECT                                                                
019300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019400*                                                                         
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019700     SKIP3                                                                
019800 01  SPAR-AREA.                                                           
019810     03  SPAR-IDTRANS            PIC  X(4)          VALUE 'L146'.         
020000     03  SPAR-WDA3CSEQ-ENTER.                                             
020001         05  SPAR-IDDC-ENTER         PIC  X(2)        VALUE SPACE.        
020010         05  SPAR-KDRETSTA-ENTER     PIC  X(1)        VALUE ZERO.         
020100         05  SPAR-DARETANK-ENTER     PIC  9(8)        VALUE ZERO.         
020200         05  SPAR-IDRT-ENTER         PIC  X(3)        VALUE SPACE.        
020300         05  SPAR-IDRTLOP-ENTER      PIC  9(3)        VALUE ZERO.         
020310     03  SPAR-WDA3CSEQ-NEXT.                                              
020320         05  SPAR-IDDC-NEXT          PIC  X(2)        VALUE SPACE.        
020330         05  SPAR-KDRETSTA-NEXT      PIC  X(1)        VALUE ZERO.         
020340         05  SPAR-DARETANK-NEXT      PIC  9(8)        VALUE ZERO.         
020350         05  SPAR-IDRT-NEXT          PIC  X(3)        VALUE SPACE.        
020360         05  SPAR-IDRTLOP-NEXT       PIC  9(3)        VALUE ZERO.         
020400                                                                          
020500     SKIP3                                                                
020600 01  NYCKLAR-TILL-DLI.                                                    
020700     03  W-WDA301KY-X.                                                    
020800         05  W-IDDC-301            PIC  X(2)          VALUE SPACE.        
020810         05  W-DAREGDAT            PIC  9(8)          VALUE ZERO.         
020900         05  W-TIKLOCK             PIC S9(9)   COMP-3 VALUE ZERO.         
021000                                                                          
021100     03  W-WDA3CSEQ-MIN-X.                                                
021110         05  W-IDDC-CSEQ-MIN       PIC  X(2)          VALUE SPACE.        
021200         05  W-KDRETSTA-CSEQ-MIN   PIC  X(1)          VALUE ZERO.         
021300         05  W-DARETANK-CSEQ-MIN   PIC  9(8)          VALUE ZERO.         
021400         05  W-IDRT-CSEQ-MIN       PIC  X(3)          VALUE SPACE.        
021500         05  W-IDRTLOP-CSEQ-MIN    PIC  9(3)          VALUE ZERO.         
021600                                                                          
021700     03  W-WDA3CSEQ-MAX-X.                                                
021710         05  W-IDDC-CSEQ-MAX       PIC  X(2)          VALUE SPACE.        
021800         05  W-KDRETSTA-CSEQ-MAX   PIC  X(1)          VALUE ZERO.         
021900         05  W-DARETANK-CSEQ-MAX   PIC  9(8)          VALUE ZERO.         
022000         05  W-IDRT-CSEQ-MAX       PIC  X(3)          VALUE SPACE.        
022100         05  W-IDRTLOP-CSEQ-MAX    PIC  9(3)          VALUE ZERO.         
022200                                                                          
022300     03  W-WDA3BSEQ-MIN-X.                                                
022400         05  W-IDRT-BSEQ-MIN       PIC  X(3)          VALUE SPACE.        
022410         05  W-IDDC-BSEQ-MIN       PIC  X(2)          VALUE SPACE.        
022500         05  W-IDRTLOP-BSEQ-MIN    PIC  9(3)          VALUE ZERO.         
022600         05  W-IDKOLLI-BSEQ-MIN    PIC S9(5)   COMP-3 VALUE ZERO.         
022700                                                                          
022800     03  W-WDA3BSEQ-MAX-X.                                                
022900         05  W-IDRT-BSEQ-MAX       PIC  X(3)          VALUE SPACE.        
022910         05  W-IDDC-BSEQ-MAX       PIC  X(2)          VALUE SPACE.        
023000         05  W-IDRTLOP-BSEQ-MAX    PIC  9(3)          VALUE ZERO.         
023100         05  W-IDKOLLI-BSEQ-MAX    PIC S9(5)   COMP-3 VALUE ZERO.         
023200                                                                          
023300     SKIP2                                                                
023400*    --- STATUS-KOD FRÅN IMS                                              
023500 01  STATUS-WS                   PIC XX.                                  
023600     88  STATUS-OK                           VALUE '  '.                  
023700     88  SEGMENT-FINNS                       VALUE '  '.                  
023800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024000     88  TRANSKOD-FEL                        VALUE 'A1'.                  
024100     88  SECURITY-FEL                        VALUE 'A4'.                  
024200     SKIP2                                                                
024300 01  GODK-STATUSKODER.                                                    
024400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024500     SKIP3                                                                
024600 01  SSA1                        PIC X(128).                              
024700     EJECT                                                                
024800*    --- IMS FUNKTIONSKODER                                               
024900*01  -COPY W0003                                                          
025000     EJECT                                                                
025100*    ---  DLI INPUT-OUTPUT AREA                                           
025200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025300     SKIP3                                                                
025400 01  DLI-IO-AREA.                                                         
025700     03  WLRETA01.                                                        
025800*        05  -COPY WDA301                                                 
025900     EJECT                                                                
026000 LINKAGE SECTION.                                                         
026100                                                                          
026110 01  MSG-PCB                     PIC X.                                   
026120     EJECT                                                                
026500*01  -COPY W0009   -PRE DISP-                                             
026600     EJECT                                                                
027000*01  -COPY W0008  -PRE RETA-                                              
027100     05  FILLER                  PIC X.                                   
027200     EJECT                                                                
027210*01  -COPY W0008  -PRE SEQB-                                              
027220     05  FILLER                  PIC X.                                   
027230     EJECT                                                                
027300*01  -COPY W0008  -PRE SEQC-                                              
027400     05  FILLER                  PIC X.                                   
027500                                                                          
027600 01  KOM-KOMA-PCB                PIC X.                                   
027700     EJECT                                                                
027800 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB                               
027900                           RETA-PCB SEQB-PCB SEQC-PCB                     
028000                           KOM-KOMA-PCB.                                  
028010 MAIN SECTION.                                                            
028100     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB                               
028200                           RETA-PCB SEQB-PCB SEQC-PCB                     
028300                           KOM-KOMA-PCB.                                  
028400                                                                          
028510     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
028600     IF SUB-KDRC = 0                                                      
028700       PERFORM A-INIT                                                     
028800       PERFORM B-KOLLA-NYCKLAR                                            
028900       IF NYCKLAR-OK                                                      
029010         IF REQU-KDPGMACT = 'E'                                           
029100           PERFORM G-KOLLA-INPUT                                          
029200           IF INDATA-OK                                                   
029300             PERFORM H-UPPDATERA                                          
029400           END-IF                                                         
030500         END-IF                                                           
030900            IF INDATA-OK                                                  
031000               PERFORM F-LAES-VISA-INFO                                   
031100            END-IF                                                        
031300       END-IF                                                             
031801                                                                          
031802          MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                        
031803          MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                       
031804          MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                      
031805          IF WS-IDMSG-INFO NOT = SPACE                                    
031806             MOVE SPACE            TO RESP-IDMSG-ERROR                    
031807             MOVE SPACE            TO RESP-IDELMT-ERROR                   
031808          ELSE                                                            
031809            IF WS-IDMSG-ERROR NOT = SPACE                                 
031810                MOVE ALL '+' TO RESP-AREA                                 
031811                MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                 
031812                MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                
031813                MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                  
031814                MOVE  001             TO RESP-IDMSGVER                    
031815                IF  REQU-KDPGMACT = 'S'                                   
031816                   MOVE ZERO             TO RESP-KVRADER                  
031817                ELSE                                                      
031818                  IF REQU-KVRADER NUMERIC                                 
031819                    MOVE REQU-KVRADER     TO RESP-KVRADER                 
031820                  ELSE                                                    
031821                    MOVE ZERO             TO RESP-KVRADER                 
031822                  END-IF                                                  
031823                END-IF                                                    
031824            END-IF                                                        
031825          END-IF                                                          
031830          PERFORM S02-RETURN-RESPONSE                                     
032000     END-IF                                                               
032100                                                                          
032200     MOVE ZERO TO RETURN-CODE                                             
032300     GOBACK                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 A-INIT SECTION.                                                          
032700                                                                          
032710     MOVE ALL '+'                         TO RESP-AREA                    
032720     MOVE SPACE                           TO RESP-IDMSG-ERROR             
032730                                             RESP-IDMSG-INFO              
032740                                             RESP-IDELMT-ERROR            
032750     MOVE 001                             TO RESP-IDMSGVER                
032760     MOVE ZERO                            TO RESP-KVRADER                 
035300                                                                          
035400     MOVE LOW-VALUE                       TO W-WDA3CSEQ-MIN-X             
035500                                             W-WDA3BSEQ-MIN-X             
035600                                                                          
035700     MOVE HIGH-VALUE                      TO W-WDA3CSEQ-MAX-X             
035800                                             W-WDA3BSEQ-MAX-X             
036100     .                                                                    
036200     EJECT                                                                
036300 B-KOLLA-NYCKLAR SECTION.                                                 
036400                                                                          
037470                                                                          
037500     MOVE JA                              TO NYCKLAR-SW                   
037600                                                                          
037700     PERFORM BA-KOLLA-KDRETSTA                                            
037800     PERFORM BB-KOLLA-IDRETSND                                            
037810                                                                          
037811     MOVE REQU-IDDC-KEY                   TO W-IDDC-301                   
037812                                             W-IDDC-CSEQ-MIN              
037813                                             W-IDDC-CSEQ-MAX              
037814                                             W-IDDC-BSEQ-MIN              
037815                                             W-IDDC-BSEQ-MAX              
037816                                             RESP-IDDC-KEY                
037818                                                                          
037820     IF IDRETSND-IFYLLT AND KDRETSTA-IFYLLT                               
037823         MOVE NEJ                         TO NYCKLAR-SW                   
037860     END-IF                                                               
037870     IF NOT IDRETSND-IFYLLT AND NOT KDRETSTA-IFYLLT                       
037880         MOVE NEJ                         TO NYCKLAR-SW                   
037890     END-IF                                                               
037891     IF NYCKLAR-FEL                                                       
037892       MOVE '043'    TO RESP-IDMSG-ERROR                                  
037893     ELSE                                                                 
037900       IF KDRETSTA-IFYLLT                                                 
037901         IF W-KDRETSTA-NUM = ZERO                                         
037910           MOVE '023'         TO RESP-IDMSG-ERROR                         
037920           MOVE 'KDRETSTA'    TO RESP-IDELMT-ERROR                        
038100         END-IF                                                           
038110       END-IF                                                             
038200     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 BA-KOLLA-KDRETSTA   SECTION.                                             
040200                                                                          
040400                                                                          
041310     MOVE REQU-KDRETSTA-KEY               TO W-KDRETSTA                   
041320                                             RESP-KDRETSTA-KEY            
041401     IF REQU-KDRETSTA-KEY = SPACE OR ALL '+'                              
041402        MOVE 'N'                          TO SW-KDRETSTA                  
041403     ELSE                                                                 
041404        MOVE 'J'                          TO SW-KDRETSTA                  
041405     END-IF                                                               
041406                                                                          
041407     IF W-REG-KEY    OR                                                   
041410        W-SAENT-KEY  OR                                                   
041411        W-SEND-KEY   OR                                                   
041420        W-LOSS-KEY   OR                                                   
041430        W-MOT-KEY    OR                                                   
041440        W-REC-KEY    OR                                                   
041450        W-INPR-KEY   OR                                                   
041500        W-PAAB-KEY                                                        
041510                                                                          
041600       EVALUATE TRUE                                                      
041700       WHEN W-REG-KEY                                                     
041800          MOVE '1'                        TO W-KDRETSTA-NUM               
041900       WHEN W-SAENT-KEY                                                   
042000          MOVE '2'                        TO W-KDRETSTA-NUM               
042010       WHEN W-SEND-KEY                                                    
042020          MOVE '2'                        TO W-KDRETSTA-NUM               
042100       WHEN W-LOSS-KEY                                                    
042200          MOVE '3'                        TO W-KDRETSTA-NUM               
042300       WHEN W-MOT-KEY                                                     
042400          MOVE '4 '                       TO W-KDRETSTA-NUM               
042410       WHEN W-REC-KEY                                                     
042420          MOVE '4 '                       TO W-KDRETSTA-NUM               
042500       WHEN W-PAAB-KEY                                                    
042600          MOVE '5'                        TO W-KDRETSTA-NUM               
042610       WHEN W-INPR-KEY                                                    
042620          MOVE '5'                        TO W-KDRETSTA-NUM               
042700       END-EVALUATE                                                       
042800                                                                          
042900       MOVE W-KDRETSTA-NUM                TO W-KDRETSTA-CSEQ-MIN          
043000                                             W-KDRETSTA-CSEQ-MAX          
043010     ELSE                                                                 
043020       MOVE ZERO                          TO W-KDRETSTA-NUM               
043100     END-IF                                                               
043200                                                                          
043300     .                                                                    
043400     EJECT                                                                
043500 BB-KOLLA-IDRETSND   SECTION.                                             
044800                                                                          
044821     IF (REQU-KDRETSTA-KEY NOT = ALL '+') AND                             
044823         REQU-IDRT-KEY         = ALL '+'                                  
044824       CONTINUE                                                           
044830     ELSE                                                                 
044900       IF REQU-IDRTLOP-KEY NUMERIC AND                                    
045000          REQU-IDRTLOP-KEY > ZERO                                         
045100*         MOVE JA                         TO SW-IDRETSND                  
045200          MOVE REQU-IDRTLOP-KEY           TO W-IDRTLOP-BSEQ-MIN           
045300                                             W-IDRTLOP-BSEQ-MAX           
045400                                             RESP-IDRTLOP-KEY             
045500       END-IF                                                             
045600                                                                          
045800       IF REQU-IDRT-KEY NOT = ALL '+' AND SPACE                           
045900          MOVE REQU-IDRT-KEY              TO W-IDRT-CSEQ-MIN              
046200                                             W-IDRT-CSEQ-MAX              
046300                                             W-IDRT-BSEQ-MIN              
046400                                             W-IDRT-BSEQ-MAX              
046500                                             RESP-IDRT-KEY                
046510          MOVE JA                         TO SW-IDRETSND                  
046600       ELSE                                                               
046601          MOVE NEJ                        TO SW-IDRETSND                  
046602       END-IF                                                             
046610     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
054900 F-LAES-VISA-INFO SECTION.                                                
055000                                                                          
055100     IF IDRETSND-IFYLLT                                                   
055200        PERFORM IMS-GHU-SEQB-WLRETA01                                     
055300     ELSE                                                                 
055400        PERFORM IMS-GU-SEQC-WLRETA01                                      
055401        IF SEGMENT-FINNS                                                  
055410          MOVE RET-KDRETSTA               TO W-KDRETSTA-CSEQ-MIN          
055420          MOVE RET-DARETANK               TO W-DARETANK-CSEQ-MIN          
055430          MOVE RET-IDRT                   TO W-IDRT-CSEQ-MIN              
055440          MOVE RET-IDRTLOP                TO W-IDRTLOP-CSEQ-MIN           
055460          PERFORM FA-FIXA-ENTER-KEY                                       
055500        END-IF                                                            
055510     END-IF                                                               
055600                                                                          
055700     IF SEGMENT-SAKNAS                                                    
056131       IF REQU-KDPGMACT = 'E'                                             
056140          CONTINUE                                                        
056150       ELSE                                                               
056151          MOVE 'IDRT-IDRTLOP' TO RESP-IDELMT-ERROR                        
056152          MOVE '041'          TO RESP-IDMSG-ERROR                         
056160       END-IF                                                             
056200     ELSE                                                                 
056210       PERFORM FA-FIXA-ENTER-KEY                                          
056220                                                                          
056300       MOVE +1                            TO INDX                         
056400       MOVE +0                            TO WS-COUNT                     
056600       PERFORM UNTIL INDX > MAX-INDX                                      
056700         IF SEGMENT-FINNS                                                 
056900           PERFORM FB-REDIGERA-MOD                                        
057000*   FÖR ATT INTE LÄSA VIDARE OM SÄNDNINGSNUMMER ÄR IFYLLT                 
057100           IF IDRETSND-IFYLLT                                             
057200              MOVE 'GE'                   TO STATUS-WS                    
057300           ELSE                                                           
057800              PERFORM IMS-GN-SEQC-WLRETA01                                
057810              MOVE RET-KDRETSTA           TO W-KDRETSTA-CSEQ-MIN          
057820              MOVE RET-DARETANK           TO W-DARETANK-CSEQ-MIN          
057830              MOVE RET-IDRT               TO W-IDRT-CSEQ-MIN              
057840              MOVE RET-IDRTLOP            TO W-IDRTLOP-CSEQ-MIN           
057900           END-IF                                                         
058000           ADD 1                          TO WS-COUNT                     
059400         END-IF                                                           
059500         ADD 1                          TO INDX                           
059600       END-PERFORM                                                        
059601                                                                          
059602       MOVE WS-COUNT TO RESP-KVRADER                                      
059603                                                                          
059604       IF WS-COUNT > MAX-INDX                                             
059605                                                                          
059606          MOVE '028'  TO RESP-IDMSG-ERROR                                 
059607       END-IF                                                             
059610                                                                          
059620* OM DET FINNS ETT 12:E SEGMENT.                                          
059630                                                                          
059700       IF SEGMENT-FINNS                                                   
059802         IF INDX = MAX-INDX OR                                            
059803            INDX > MAX-INDX                                               
059804           MOVE '028'    TO RESP-IDMSG-ERROR                              
059805         END-IF                                                           
059840       END-IF                                                             
059900                                                                          
060300                                                                          
060400     END-IF                                                               
060500     .                                                                    
060600     EJECT                                                                
060700 FA-FIXA-ENTER-KEY        SECTION.                                        
060800                                                                          
060900     IF SEGMENT-FINNS                                                     
061000        MOVE RET-IDDC                   TO SPAR-IDDC-ENTER                
061010        MOVE RET-KDRETSTA               TO SPAR-KDRETSTA-ENTER            
061100        MOVE RET-DARETANK               TO SPAR-DARETANK-ENTER            
061200        MOVE RET-IDRT                   TO SPAR-IDRT-ENTER                
061300        MOVE RET-IDRTLOP                TO SPAR-IDRTLOP-ENTER             
061400     ELSE                                                                 
061410        MOVE W-WDA3CSEQ-MIN-X           TO SPAR-WDA3CSEQ-ENTER            
061900     END-IF                                                               
062200     .                                                                    
062300     EJECT                                                                
062400                                                                          
062500 FB-REDIGERA-MOD          SECTION.                                        
062600                                                                          
062610     IF REQU-KDPGMACT = 'S'                                               
062700        MOVE SPACE                      TO RESP-KDCMD       (INDX)        
062710     END-IF                                                               
062800     MOVE RET-IDRT                      TO RESP-IDRT        (INDX)        
062900     MOVE RET-IDRTLOP                   TO RESP-IDRTLOP     (INDX)        
063000     MOVE RET-DASNDDAT (3:6)            TO RESP-TISNDDAT    (INDX)        
063001                                                                          
063010     PERFORM FBA-RAEKNA-KOLLI                                             
063020                                                                          
063030     IF W-KVKOLLI-SND         >  ZERO                                     
063040        MOVE W-KVKOLLI-SND              TO RESP-KVKOLLI-SND (INDX)        
063050     ELSE                                                                 
063051        IF RET-TILOSSN      > ZERO AND                                    
063052           RET-KVKOLLI-LOSS > ZERO                                        
063053          MOVE RET-KVKOLLI-LOSS         TO RESP-KVKOLLI-SND (INDX)        
063054        ELSE                                                              
063055          IF RET-TIINLMOT    > ZERO AND                                   
063056             RET-KVKOLLI-MOT > ZERO                                       
063057            MOVE RET-KVKOLLI-MOT        TO RESP-KVKOLLI-SND (INDX)        
063060          END-IF                                                          
063061        END-IF                                                            
063070     END-IF                                                               
063100     IF RET-KDRETSTA   >  W-SND-REG                                       
064200        IF RET-TIINLMOT           >  ZERO                                 
064300           MOVE RET-TIINLMOT          TO RESP-TIINLMOT     (INDX)         
064400           MOVE RET-ADINLOMR-MOT      TO RESP-ADINLOMR-MOT (INDX)         
064500           MOVE RET-IDANSTNR-MOT      TO RESP-IDANSTNR-MOT (INDX)         
064506           IF RET-KVKOLLI-MOT     > ZERO                                  
064507             MOVE RET-KVKOLLI-MOT     TO RESP-KVKOLLI-MOT  (INDX)         
064508           ELSE                                                           
064509             IF RET-KVKOLLI-LOSS  > ZERO                                  
064510                MOVE RET-KVKOLLI-LOSS TO RESP-KVKOLLI-MOT  (INDX)         
064511             ELSE                                                         
064512               IF W-KVKOLLI-MOT   > ZERO                                  
064513                 MOVE W-KVKOLLI-MOT   TO RESP-KVKOLLI-MOT  (INDX)         
064520               ELSE                                                       
064530                 IF W-KVKOLLI-SND > ZERO                                  
064540                   MOVE ZERO          TO RESP-KVKOLLI-MOT  (INDX)         
064591                 END-IF                                                   
064592               END-IF                                                     
064593             END-IF                                                       
064594           END-IF                                                         
065100        END-IF                                                            
065200                                                                          
066000                                                                          
068200     END-IF                                                               
068300     .                                                                    
068400     EJECT                                                                
068500 FBA-RAEKNA-KOLLI         SECTION.                                        
068600                                                                          
068700     MOVE ZERO                          TO W-KVKOLLI-SND                  
068900                                           W-KVKOLLI-MOT                  
069000     IF IDRETSND-IFYLLT                                                   
069100         CONTINUE                                                         
069200     ELSE                                                                 
069300         MOVE RET-IDRT                  TO W-IDRT-BSEQ-MIN                
069400                                           W-IDRT-BSEQ-MAX                
069500         MOVE RET-IDRTLOP               TO W-IDRTLOP-BSEQ-MIN             
069600                                           W-IDRTLOP-BSEQ-MAX             
069700         PERFORM IMS-GHU-SEQB-WLRETA01                                    
069800     END-IF                                                               
069900                                                                          
070000     PERFORM UNTIL SEGMENT-SAKNAS                                         
070100       IF RET-IDKOLLI = SPAR-IDKOLLI                                      
070110         CONTINUE                                                         
070120       ELSE                                                               
070130         IF RET-DASNDDAT > ZERO                                           
070600            ADD +1                      TO W-KVKOLLI-SND                  
070601         END-IF                                                           
070680         IF RET-TIINLMOT > ZERO                                           
070690            IF RET-KDKOLSTA = W-KLI-AVV                                   
070691              CONTINUE                                                    
070692            ELSE                                                          
070693              ADD +1                    TO W-KVKOLLI-MOT                  
070694            END-IF                                                        
070695         END-IF                                                           
071610       END-IF                                                             
071611                                                                          
071620       MOVE RET-IDKOLLI                 TO SPAR-IDKOLLI                   
071700                                                                          
071800       PERFORM IMS-GHN-SEQB-WLRETA01                                      
071900     END-PERFORM                                                          
071910     MOVE ZERO                          TO SPAR-IDKOLLI                   
072000     .                                                                    
072100     EJECT                                                                
074300                                                                          
074400 G-KOLLA-INPUT SECTION.                                                   
074500                                                                          
074600     MOVE JA                            TO INDATA-SW                      
074700                                                                          
074800     PERFORM GA-FORMELL-KONTROLL                                          
074900     IF INDATA-OK                                                         
075000        PERFORM GB-LOGISK-KONTROLL                                        
075100     END-IF                                                               
075200                                                                          
076000                                                                          
076100     .                                                                    
076200     EJECT                                                                
076300                                                                          
076400 GA-FORMELL-KONTROLL SECTION.                                             
076500                                                                          
076600     IF REQU-INPUT                = ALL '+'                               
076601       IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                       
076602         MOVE +1                            TO INDX                       
076603         MOVE REQU-KVRADER                  TO WS-INDX-REC                
076604         MOVE NEJ                           TO WS-REC-LIMIT               
076605         MOVE NEJ                           TO INDATA-SW                  
076606                                                                          
076607         PERFORM UNTIL INDX >  MAX-INDX OR REC-LIMIT                      
076608            IF  REQU-KDCMD(INDX)    = W-DELETE                            
076611               MOVE JA       TO INDATA-SW                                 
076612            END-IF                                                        
076620            IF INDX = WS-INDX-REC                                         
076630               MOVE JA TO WS-REC-LIMIT                                    
076640            ELSE                                                          
076650               ADD +1        TO INDX                                      
076660            END-IF                                                        
076670         END-PERFORM                                                      
076671         IF INDATA-OK                                                     
076673            PERFORM GAA-KOLLA-IDANSTNR                                    
076674            PERFORM GAB-KOLLA-ADINLOMR                                    
076675            PERFORM GAC-KOLLA-KDCMD                                       
076676         ELSE                                                             
076677            MOVE '014'    TO RESP-IDMSG-ERROR                             
076678         END-IF                                                           
076680       END-IF                                                             
077300     ELSE                                                                 
077400                                                                          
077500       PERFORM GAA-KOLLA-IDANSTNR                                         
077600       PERFORM GAB-KOLLA-ADINLOMR                                         
077700       PERFORM GAC-KOLLA-KDCMD                                            
077800     END-IF                                                               
077900                                                                          
078000     .                                                                    
078100     EJECT                                                                
078200                                                                          
078300 GAA-KOLLA-IDANSTNR   SECTION.                                            
078400                                                                          
078500     MOVE NEJ                           TO  SW-IDANSTNR                   
078600                                                                          
078700     IF REQU-IDANSTNR NOT = ALL '+'                                       
078800        IF REQU-IDANSTNR NUMERIC                                          
079000           MOVE JA                      TO SW-IDANSTNR                    
079100           MOVE REQU-IDANSTNR           TO W-IDANSTNR                     
079110                                           RESP-IDANSTNR                  
079200        ELSE                                                              
079400           MOVE NEJ                     TO INDATA-SW                      
079420           MOVE 'IDANSTNR' TO RESP-IDELMT-ERROR                           
079430           MOVE '024'      TO RESP-IDMSG-ERROR                            
079500        END-IF                                                            
079600     END-IF                                                               
079700                                                                          
079800     .                                                                    
079900     EJECT                                                                
080000 GAB-KOLLA-ADINLOMR   SECTION.                                            
080100                                                                          
080200     MOVE NEJ                           TO  SW-ADINLOMR                   
080300                                                                          
080400     IF REQU-ADINLOMR NOT = ALL '+'                                       
080600        MOVE JA                         TO  SW-ADINLOMR                   
080610        MOVE REQU-ADINLOMR              TO  RESP-ADINLOMR                 
080700     END-IF                                                               
080800                                                                          
080900     .                                                                    
081000     EJECT                                                                
081100 GAC-KOLLA-KDCMD      SECTION.                                            
081200                                                                          
081210     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
081300       MOVE +1                            TO INDX                         
081310       MOVE REQU-KVRADER                  TO WS-INDX-REC                  
081320       MOVE NEJ                           TO WS-REC-LIMIT                 
081400                                                                          
081500       PERFORM UNTIL INDX >  MAX-INDX OR REC-LIMIT                        
081510          IF  REQU-KDCMD(INDX)        NOT = ALL '+' AND SPACE             
081610             MOVE REQU-KDCMD (INDX)  TO RESP-KDCMD (INDX)                 
081810             IF REQU-KDCMD(INDX) = W-LOSSNING   OR                        
081911                                   W-RECEIVING  OR                        
081912                                   W-DEVIATION  OR                        
081913                                   W-DELETE                               
082000                                                                          
082410               IF REQU-KDCMD(INDX) =  W-LOSSNING   OR                     
082420                                      W-RECEIVING                         
082500                  IF REQU-IDRT(INDX) = W-CDC OR W-US1 OR W-US2  OR        
082510                                       W-US3 OR W-CA1 OR W-JP1 OR         
082530                                       W-AU1 OR W-SE1 OR W-GB1 OR         
082540                                       W-SE2 OR W-GB2 OR W-GB3 OR         
082550                                       W-NL1 OR W-IT1                     
082600                     CONTINUE                                             
082700*             DESSA OBL FÄLT FINNS REDAN FRÅN 4733 OM CDC-S               
082800                  ELSE                                                    
082900                     PERFORM GACA-KOLLA-OBL-FAELT                         
083000                  END-IF                                                  
083100               END-IF                                                     
083200                                                                          
083211               IF REQU-KDCMD(INDX)   =  W-DELETE                          
083220                  IF REQU-IDRT(INDX) = W-CDC OR W-US1 OR W-US2  OR        
083221                                       W-US3 OR W-CA1 OR W-JP1 OR         
083223                                       W-AU1 OR W-SE1 OR W-GB1 OR         
083224                                       W-SE2 OR W-GB2 OR W-GB3 OR         
083225                                       W-NL1 OR W-IT1                     
083230                     CONTINUE                                             
083240                  ELSE                                                    
083242                    MOVE NEJ     TO INDATA-SW                             
083243                    MOVE '004'   TO RESP-IDMSG-ERROR                      
083244                                    RESP-IDMSG-ERROR-LINE (INDX)          
083260                  END-IF                                                  
083280               END-IF                                                     
084200             ELSE                                                         
084400               MOVE NEJ                   TO INDATA-SW                    
084410               MOVE 'CMD'        TO RESP-IDELMT-ERROR                     
084411               MOVE '023'        TO RESP-IDMSG-ERROR                      
084420                                    RESP-IDMSG-ERROR-LINE (INDX)          
084500             END-IF                                                       
084600          END-IF                                                          
084610          IF INDX = WS-INDX-REC                                           
084620             MOVE JA TO WS-REC-LIMIT                                      
084630          ELSE                                                            
084700             ADD +1                          TO INDX                      
084710          END-IF                                                          
084800       END-PERFORM                                                        
084901     ELSE                                                                 
084902       MOVE NEJ           TO INDATA-SW                                    
084903       IF REQU-KVRADER = 0                                                
084904          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
084905          MOVE '126'     TO RESP-IDMSG-ERROR                              
084906       ELSE                                                               
084907          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
084908          MOVE '024'     TO RESP-IDMSG-ERROR                              
084909       END-IF                                                             
084910     END-IF                                                               
085000     .                                                                    
085100     EJECT                                                                
085200                                                                          
085300 GACA-KOLLA-OBL-FAELT SECTION.                                            
085400                                                                          
085500     IF IDANSTNR-IFYLLT                                                   
085600         CONTINUE                                                         
085700     ELSE                                                                 
085900        MOVE NEJ              TO INDATA-SW                                
085910        MOVE 'IDANSTNR'       TO RESP-IDELMT-ERROR                        
085920        MOVE '026'            TO RESP-IDMSG-ERROR                         
085930                                 RESP-IDMSG-ERROR-LINE (INDX)             
086000     END-IF                                                               
086100                                                                          
086200     IF ADINLOMR-IFYLLT                                                   
086300         CONTINUE                                                         
086400     ELSE                                                                 
086600         MOVE NEJ                       TO INDATA-SW                      
086610        MOVE 'ADINLOMR'       TO RESP-IDELMT-ERROR                        
086620        MOVE '026'            TO RESP-IDMSG-ERROR                         
086630                                 RESP-IDMSG-ERROR-LINE (INDX)             
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000 GB-LOGISK-KONTROLL SECTION.                                              
087100                                                                          
087200     MOVE +1                            TO INDX                           
087220     MOVE NEJ                           TO WS-REC-LIMIT                   
087300     PERFORM UNTIL INDX >  MAX-INDX OR REC-LIMIT                          
087360                                                                          
087401        IF REQU-KDCMD(INDX)   =  W-LOSSNING   OR                          
087420                                 W-RECEIVING  OR                          
087430                                 W-DELETE                                 
087500           PERFORM GBA-KOLLA-VALD-SANDNING                                
087600        END-IF                                                            
087610        IF INDX = WS-INDX-REC                                             
087620           MOVE JA TO WS-REC-LIMIT                                        
087630        ELSE                                                              
087700           ADD +1                          TO INDX                        
087710        END-IF                                                            
087800     END-PERFORM                                                          
087900                                                                          
088000     .                                                                    
088100     EJECT                                                                
088200 GBA-KOLLA-VALD-SANDNING          SECTION.                                
088300                                                                          
088400     MOVE REQU-IDRT(INDX)                TO W-IDRT-BSEQ-MIN               
088500                                            W-IDRT-BSEQ-MAX               
088600     MOVE REQU-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN            
088700                                            W-IDRTLOP-BSEQ-MAX            
088800     PERFORM IMS-GHU-SEQB-WLRETA01                                        
088900     IF SEGMENT-FINNS                                                     
089000                                                                          
089100        IF REQU-KDCMD(INDX) =  W-LOSSNING                                 
089200           IF RET-KDRETSTA  =  W-SND-SAENT                                
089210              IF REQU-IDANSTNR = ALL '+'                                  
089220                IF RET-IDANSTNR-LOSS = ZERO                               
089222                  MOVE NEJ      TO INDATA-SW                              
089223                  MOVE '227'    TO RESP-IDMSG-ERROR                       
089224                                  RESP-IDMSG-ERROR-LINE (INDX)            
089230                END-IF                                                    
089240              END-IF                                                      
089400           ELSE                                                           
089600              MOVE NEJ          TO INDATA-SW                              
089610              MOVE '228'        TO RESP-IDMSG-ERROR                       
089620                                   RESP-IDMSG-ERROR-LINE (INDX)           
089700           END-IF                                                         
089800        END-IF                                                            
089900                                                                          
089911        IF REQU-KDCMD(INDX)         =  W-DELETE                           
089920           IF RET-KDRETSTA          =  W-SND-SAENT OR                     
089921             (RET-KDRETSTA          =  W-SND-LOSS  AND                    
089922              RET-IDDISTR           = ZERO)                               
089930              CONTINUE                                                    
089940           ELSE                                                           
089960              MOVE NEJ        TO INDATA-SW                                
089962              MOVE '234'      TO RESP-IDMSG-ERROR                         
089963                                 RESP-IDMSG-ERROR-LINE (INDX)             
089970           END-IF                                                         
089980        END-IF                                                            
089990                                                                          
090010        IF REQU-KDCMD(INDX)        =  W-RECEIVING                         
090100           IF (RET-KDRETSTA        =  W-SND-SAENT OR                      
090200              RET-KDRETSTA         =  W-SND-LOSS) AND                     
090210              RET-IDDISTR          >  ZERO                                
090310              IF REQU-IDANSTNR = ALL '+'                                  
090320                IF RET-IDANSTNR-MOT  = ZERO                               
090340                  MOVE NEJ     TO INDATA-SW                               
090342                  MOVE '227'   TO RESP-IDMSG-ERROR                        
090343                                  RESP-IDMSG-ERROR-LINE (INDX)            
090350                END-IF                                                    
090360              END-IF                                                      
090400           ELSE                                                           
090600              MOVE NEJ         TO INDATA-SW                               
090610              MOVE '233'       TO RESP-IDMSG-ERROR                        
090620                                  RESP-IDMSG-ERROR-LINE (INDX)            
090700           END-IF                                                         
090800        END-IF                                                            
090900     ELSE                                                                 
091100        MOVE NEJ          TO INDATA-SW                                    
091110        MOVE '229'        TO RESP-IDMSG-ERROR                             
091120                             RESP-IDMSG-ERROR-LINE (INDX)                 
091200     END-IF                                                               
091300     .                                                                    
091400     EJECT                                                                
091500 H-UPPDATERA SECTION.                                                     
091600                                                                          
091700     MOVE +1                            TO 4792-INDX                      
091800     ACCEPT DAGENS-DATUM FROM DATE                                        
092200        PERFORM HB-UPPDATERA-VALDA-SAENDNINGAR                            
092400                                                                          
092500     IF 4792-INDX >  +1                                                   
092600        PERFORM S03-STARTA-R31-RAPPORTERING                               
092700     END-IF                                                               
092800                                                                          
092821     IF W-UPDATE-OK                                                       
092822        MOVE '001'               TO RESP-IDMSG-INFO                       
092823     ELSE                                                                 
092824        MOVE '004'               TO RESP-IDMSG-INFO                       
092825     END-IF                                                               
092830                                                                          
093400     .                                                                    
093500     EJECT                                                                
093600                                                                          
095200 HB-UPPDATERA-VALDA-SAENDNINGAR SECTION.                                  
095300                                                                          
095400     MOVE +1                            TO INDX                           
095420     MOVE NEJ                           TO WS-REC-LIMIT                   
095430     MOVE NEJ                           TO W-UPDATE-SW                    
095500     PERFORM UNTIL INDX >  MAX-INDX OR REC-LIMIT                          
095560                                                                          
095600        IF REQU-KDCMD(INDX) =  W-LOSSNING                                 
095700           PERFORM HBA-RAPPORTERA-LOSSAT                                  
095710           MOVE SPACE  TO RESP-KDCMD(INDX)                                
095800        END-IF                                                            
095900                                                                          
096010        IF REQU-KDCMD(INDX) =  W-RECEIVING                                
096100           PERFORM HBB-RAPPORTERA-MOTTAGET                                
096110           MOVE SPACE  TO RESP-KDCMD(INDX)                                
096200        END-IF                                                            
096211        IF REQU-KDCMD(INDX) =  W-DELETE                                   
096220           PERFORM HBC-BORTTAG                                            
096221           MOVE SPACE  TO RESP-KDCMD(INDX)                                
096230        END-IF                                                            
096240        IF INDX = WS-INDX-REC                                             
096250           MOVE JA TO WS-REC-LIMIT                                        
096260        ELSE                                                              
096300           ADD +1                          TO INDX                        
096310        END-IF                                                            
096400     END-PERFORM                                                          
096600     .                                                                    
096700     EJECT                                                                
096800                                                                          
096900 HBA-RAPPORTERA-LOSSAT      SECTION.                                      
097000                                                                          
097100     MOVE REQU-IDRT(INDX)                TO W-IDRT-BSEQ-MIN               
097200                                            W-IDRT-BSEQ-MAX               
097300     MOVE REQU-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN            
097400                                            W-IDRTLOP-BSEQ-MAX            
097500     PERFORM IMS-GHU-SEQB-WLRETA01                                        
097600                                                                          
097700     IF SEGMENT-FINNS                                                     
097800        PERFORM UNTIL SEGMENT-SAKNAS                                      
097900           IF RET-KDKOLSTA =  W-KLI-SAK OR W-KLI-AVV                      
098000              CONTINUE                                                    
098100           ELSE                                                           
098200              IF REQU-ADINLOMR =  ALL '+'                                 
098300                 CONTINUE                                                 
098400              ELSE                                                        
098500                 MOVE REQU-ADINLOMR      TO RET-ADINLOMR-LOSS             
098510                                            RET-ADINLOMR                  
098600              END-IF                                                      
098700              IF REQU-IDANSTNR =  ALL '+'                                 
098800                 CONTINUE                                                 
098900              ELSE                                                        
099000                 MOVE W-IDANSTNR        TO RET-IDANSTNR-LOSS              
099100              END-IF                                                      
099200              MOVE W-SND-LOSS           TO RET-KDRETSTA                   
099300              MOVE W-KLI-LOSS           TO RET-KDKOLSTA                   
099400              MOVE DAGENS-DATUM         TO RET-TILOSSN                    
099500              IF RET-DARETANK =  ZERO                                     
099600                 MOVE FUNCTION CURRENT-DATE (1:8) TO  RET-DARETANK        
099700              END-IF                                                      
099710              IF RET-IDDISTR > ZERO                                       
099711                PERFORM S02-FYLL-R31-MID                                  
099720              END-IF                                                      
099900                                                                          
100000              PERFORM IMS-REPL-SEQB-WLRETA01                              
100010              MOVE JA              TO W-UPDATE-SW                         
100100           END-IF                                                         
100200           PERFORM IMS-GHN-SEQB-WLRETA01                                  
100300        END-PERFORM                                                       
100400     ELSE                                                                 
100500        CALL FELLOG                                                       
100600     END-IF                                                               
100700     .                                                                    
100800     EJECT                                                                
100900                                                                          
101000 HBB-RAPPORTERA-MOTTAGET    SECTION.                                      
101100                                                                          
101200     MOVE REQU-IDRT(INDX)                TO W-IDRT-BSEQ-MIN               
101300                                            W-IDRT-BSEQ-MAX               
101400     MOVE REQU-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN            
101500                                            W-IDRTLOP-BSEQ-MAX            
101600     PERFORM IMS-GHU-SEQB-WLRETA01                                        
101700                                                                          
101800     IF SEGMENT-FINNS                                                     
101900        PERFORM UNTIL SEGMENT-SAKNAS                                      
102000           IF RET-KDKOLSTA = W-KLI-SAK OR                                 
102010                             W-KLI-AVV                                    
102100              CONTINUE                                                    
102200           ELSE                                                           
102300              IF RET-KDRETSTA =  W-SND-SAENT  OR                          
102310                (RET-KDRETSTA =  W-SND-LOSS   AND                         
102320                 RET-IDRT     =  W-CDC)                                   
102400                 PERFORM S02-FYLL-R31-MID                                 
102410                 CONTINUE                                                 
102500              END-IF                                                      
102600              IF REQU-ADINLOMR =  ALL '+'                                 
102700                 CONTINUE                                                 
102800              ELSE                                                        
102900                 MOVE REQU-ADINLOMR      TO RET-ADINLOMR-MOT              
102910                                            RET-ADINLOMR                  
103000              END-IF                                                      
103100              IF REQU-IDANSTNR =  ALL '+'                                 
103200                 CONTINUE                                                 
103300              ELSE                                                        
103400                 MOVE W-IDANSTNR        TO RET-IDANSTNR-MOT               
103500              END-IF                                                      
103600              MOVE W-SND-MOT            TO RET-KDRETSTA                   
103700              MOVE W-KLI-MOT            TO RET-KDKOLSTA                   
103800              MOVE DAGENS-DATUM         TO RET-TIINLMOT                   
103900              IF RET-DARETANK =  ZERO                                     
104000                 MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DARETANK         
104100              END-IF                                                      
104110              IF RET-TILOSSN =  ZERO                                      
104120                 MOVE DAGENS-DATUM      TO RET-TILOSSN                    
104130              END-IF                                                      
104200                                                                          
104300              PERFORM IMS-REPL-SEQB-WLRETA01                              
104310              MOVE JA              TO W-UPDATE-SW                         
104400           END-IF                                                         
104500           PERFORM IMS-GHN-SEQB-WLRETA01                                  
104600        END-PERFORM                                                       
104700     ELSE                                                                 
104800        CALL FELLOG                                                       
104900     END-IF                                                               
105000     .                                                                    
105100     EJECT                                                                
105200                                                                          
105210 HBC-BORTTAG                SECTION.                                      
105220                                                                          
105230     MOVE REQU-IDRT(INDX)                TO W-IDRT-BSEQ-MIN               
105240                                            W-IDRT-BSEQ-MAX               
105250     MOVE REQU-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN            
105260                                            W-IDRTLOP-BSEQ-MAX            
105270     PERFORM IMS-GU-SEQB-WLRETA01                                         
105280                                                                          
105291     PERFORM UNTIL SEGMENT-SAKNAS                                         
105292        IF RET-KDRETSTA =  W-SND-SAENT  OR                                
105293          (RET-KDRETSTA =  W-SND-LOSS   AND                               
105294           RET-IDDISTR  = ZERO)                                           
105295          MOVE RET-DAREGDAT             TO W-DAREGDAT                     
105296          MOVE RET-TIKLOCK              TO W-TIKLOCK                      
105297          PERFORM IMS-GHU-WLRETA01                                        
105298          PERFORM IMS-DLET-WLRETA01                                       
105299          MOVE JA              TO W-UPDATE-SW                             
105300          ADD +1     TO WS-DEL-COUNT                                      
105301          IF WS-DEL-COUNT  = REQU-KVRADER                                 
105310            MOVE NEJ TO INDATA-SW                                         
105320          END-IF                                                          
105321          PERFORM IMS-GN-SEQB-WLRETA01                                    
105322        END-IF                                                            
105323     END-PERFORM                                                          
105326     .                                                                    
105327     EJECT                                                                
105328                                                                          
105330                                                                          
108300 S02-FYLL-R31-MID                SECTION.                                 
108400                                                                          
108410     MOVE RET-IDDC                      TO                                
108420                                   MOD4792-MID-IDDC                       
108500     MOVE RET-DAREGDAT (3:6)            TO                                
108510                                   MOD4792-MID-TIREGDAT(4792-INDX)        
108600     MOVE RET-TIKLOCK                   TO                                
108610                                   MOD4792-MID-TIKLOCK (4792-INDX)        
108700     ADD +1                             TO 4792-INDX                      
108800                                                                          
108900     IF 4792-INDX >  4792-MAX-INDX                                        
109000        PERFORM S03-STARTA-R31-RAPPORTERING                               
109100        MOVE +1                         TO 4792-INDX                      
109200     END-IF                                                               
109300     .                                                                    
109400     EJECT                                                                
109500                                                                          
109600 S03-STARTA-R31-RAPPORTERING     SECTION.                                 
109700                                                                          
109800     ACCEPT DAGENS-DATUM FROM DATE                                        
109900     ACCEPT DAGENS-TID   FROM TIME                                        
110000                                                                          
110100     MOVE SPACE                         TO MSG-KOM-WMSGKOM                
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
110300     MOVE LOW-VALUE                     TO MSG-KOM-KDZ1                   
110400     MOVE LOW-VALUE                     TO MSG-KOM-KDZ2                   
110500     MOVE SPACE                         TO MSG-KOM-KDTRANS                
110600     MOVE 'W4I79201'                    TO MSG-KOM-IDCPYTXT               
110700     MOVE 'INLEVRET'                    TO MSG-KOM-IDSNDNOD               
110810     MOVE 'WL014600'                    TO MSG-KOM-IDSNDJOB               
110900     MOVE DAGENS-DATUM                  TO MSG-KOM-TIREGDAT               
111000     MOVE DAGENS-TID                    TO MSG-KOM-TIKLOCK                
111100     MOVE SPACE                         TO MSG-KOM-IDMFSMED               
111200                                                                          
111300     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
111400                                  LENGTH OF MOD4792-MID-W4I79201          
111500                                                                          
111600     MOVE 'W4T792X '                    TO P-TO-P-MSG-KDTRANS             
111710     MOVE 'L146'                        TO P-TO-P-MSG-IDTRANS             
111810     MOVE '2'                           TO P-TO-P-MSG-KDMFSFOR            
111900                                                                          
112000     COMPUTE MOD4792-MID-KVPOST  = 4792-INDX - 1                          
112100                                                                          
112200     MOVE MOD4792-MID-W4I79201          TO P-TO-P-MSG-INDATA              
112300                                                                          
112400     CALL W006KOM USING MSG-PCB                                           
112500                        DISP-PCB                                          
112600                        KOM-KOMA-PCB                                      
112700                        MSG-KOM-WMSGKOM                                   
112800                        P-TO-P-MSG-IO-AREA-SNUF                           
112900                                                                          
113000     .                                                                    
113100     EJECT                                                                
121810*    --- DISPATCHER SECTIONS                                              
121820 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
121830                                                                          
121840     MOVE 'GETARG'               TO SUB-KDFUNC                            
121850     MOVE 'CARPARTS.LDC.RETURNQUEUE'  TO SUB-ADDISPABS                    
121860     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
121870                                                                          
121880     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
121890                                                                          
121891     IF SUB-KDRC > 0                                                      
121892       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
121893       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
121894       DELIMITED BY SIZE INTO ERROR-TEXT                                  
121895       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
121896     END-IF                                                               
121897     .                                                                    
121898     SKIP3                                                                
121899 S02-RETURN-RESPONSE SECTION.                                             
121900                                                                          
121901     MOVE 'RETURN'                   TO SUB-KDFUNC                        
121902     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
121903                                                                          
121904     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
121905                                                                          
121906     IF SUB-KDRC > 0                                                      
121907       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
121908       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
121909       DELIMITED BY SIZE INTO ERROR-TEXT                                  
121910       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
121911     END-IF                                                               
121912     .                                                                    
121913     EJECT                                                                
121914     SKIP2                                                                
121920* --- IMS SEKTIONER ---                                                   
122000     SKIP3                                                                
125700 IMS-GHU-WLRETA01       SECTION.                                          
125800                                                                          
125900     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
126100          DELIMITED BY SIZE INTO SSA1                                     
126200     MOVE '  '           TO GODK-STATUSKODER                              
126300     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-AREA SSA1                     
126400     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
126500     PERFORM IMS-STATUSKONTROLL                                           
126600     .                                                                    
126700                                                                          
126710 IMS-DLET-WLRETA01      SECTION.                                          
126711                                                                          
126712     MOVE '    '           TO GODK-STATUSKODER                            
126713     CALL CBLTDLI USING DLET RETA-PCB DLI-IO-AREA                         
126714     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
126715     PERFORM IMS-STATUSKONTROLL                                           
126716     .                                                                    
126717     EJECT                                                                
126718 IMS-GU-SEQB-WLRETA01       SECTION.                                      
126720                                                                          
126730     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
126740                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
126750          DELIMITED BY SIZE INTO SSA1                                     
126760     MOVE '  GE'           TO GODK-STATUSKODER                            
126770     CALL CBLTDLI USING GU SEQB-PCB DLI-IO-AREA SSA1                      
126780     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
126790     PERFORM IMS-STATUSKONTROLL                                           
126791     .                                                                    
126792                                                                          
126800 IMS-GN-SEQB-WLRETA01 SECTION.                                            
126900                                                                          
127000     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
127100                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
127200          DELIMITED BY SIZE INTO SSA1                                     
127300     MOVE '  GE' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING GN SEQB-PCB DLI-IO-AREA SSA1                      
127500     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     EJECT                                                                
127900                                                                          
127910 IMS-GHU-SEQB-WLRETA01       SECTION.                                     
127920                                                                          
127930     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
127940                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
127950          DELIMITED BY SIZE INTO SSA1                                     
127960     MOVE '  GE'           TO GODK-STATUSKODER                            
127970     CALL CBLTDLI USING GHU SEQB-PCB DLI-IO-AREA SSA1                     
127980     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
127990     PERFORM IMS-STATUSKONTROLL                                           
127991     .                                                                    
127992                                                                          
127993 IMS-GHN-SEQB-WLRETA01 SECTION.                                           
127994                                                                          
127995     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
127996                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
127997          DELIMITED BY SIZE INTO SSA1                                     
127998     MOVE '  GE' TO GODK-STATUSKODER                                      
127999     CALL CBLTDLI USING GHN SEQB-PCB DLI-IO-AREA SSA1                     
128000     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
128001     PERFORM IMS-STATUSKONTROLL                                           
128002     .                                                                    
128003     EJECT                                                                
128004                                                                          
128010 IMS-REPL-SEQB-WLRETA01      SECTION.                                     
128100                                                                          
128200     MOVE '    '           TO GODK-STATUSKODER                            
128300     CALL CBLTDLI USING REPL SEQB-PCB DLI-IO-AREA                         
128400     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
128500     PERFORM IMS-STATUSKONTROLL                                           
128600     .                                                                    
128700     EJECT                                                                
128800 IMS-GU-SEQC-WLRETA01       SECTION.                                      
128900                                                                          
129000     STRING 'WLRETA01(WDA3CSEQ>=' W-WDA3CSEQ-MIN-X                        
129100                    '&WDA3CSEQ<=' W-WDA3CSEQ-MAX-X ')'                    
129200          DELIMITED BY SIZE INTO SSA1                                     
129300     MOVE '  GE'           TO GODK-STATUSKODER                            
129400     CALL CBLTDLI USING GHU SEQC-PCB DLI-IO-AREA SSA1                     
129500     MOVE SEQC-STATUS-CODE TO STATUS-WS                                   
129600     PERFORM IMS-STATUSKONTROLL                                           
129700     .                                                                    
129800                                                                          
129900 IMS-GN-SEQC-WLRETA01 SECTION.                                            
130000                                                                          
130100     STRING 'WLRETA01(WDA3CSEQ> ' W-WDA3CSEQ-MIN-X                        
130200                    '&WDA3CSEQ<=' W-WDA3CSEQ-MAX-X ')'                    
130300          DELIMITED BY SIZE INTO SSA1                                     
130400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
130500     CALL CBLTDLI USING GN SEQC-PCB DLI-IO-AREA SSA1                      
130600     MOVE SEQC-STATUS-CODE TO STATUS-WS                                   
130700     PERFORM IMS-STATUSKONTROLL                                           
130800     .                                                                    
130900     EJECT                                                                
131000                                                                          
131100 IMS-STATUSKONTROLL SECTION.                                              
131200                                                                          
131300     SET STATUS-IX TO 1                                                   
131400     SEARCH GODK-STATUS                                                   
131500       AT END                                                             
131600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
131700         DELIMITED BY SIZE INTO FELTEXT                                   
131800         CALL FELLOG                                                      
131900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
132000         CONTINUE                                                         
132100     END-SEARCH                                                           
132200     .                                                                    
