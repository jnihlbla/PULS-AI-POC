000010*                                                                         
000020******************************************************************        
000030*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0146      *        
000040******************************************************************        
000050*                                                                         
000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4073100.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   95/06/09.                                                
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
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T731                                              
001800*        MID:         W4I73101                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O73101                                            
002110*                                                                         
002120*    E'TRACKER: 4230251 LDC-3  SUSANNE OLSSON                             
002130*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4073100'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
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
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004100 77  4792-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  4792-MAX-INDX               PIC S9(4)  VALUE +24   COMP SYNC.        
004300 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
004400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004500 77  WS-KDRETSTA                 PIC  X(1)  VALUE SPACE.                  
004600                                                                          
004700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004800     88  INDATA-OK                           VALUE 'J'.                   
004900     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '4731'.                
005700     88  GODK-MID                            VALUE '4731'.                
005800     88  HELP-MID                            VALUE '0551'.                
005900     EJECT                                                                
006000*    --- KONSTANTER                                                       
006100 77  W-MOTTAGNING                PIC  X(1)  VALUE 'M'.                    
006110 77  W-RECEIVING                 PIC  X(1)  VALUE 'R'.                    
006200 77  W-LOSSNING                  PIC  X(1)  VALUE 'L'.                    
006210 77  W-BORTTAG                   PIC  X(1)  VALUE 'B'.                    
006220 77  W-DELETE                    PIC  X(1)  VALUE 'D'.                    
006300 77  W-AVVIKELSE                 PIC  X(3)  VALUE 'AVV'.                  
006310 77  W-DEVIATION                 PIC  X(3)  VALUE 'DEV'.                  
006400 77  W-SND-REG                   PIC  X(1)  VALUE '1'.                    
006500 77  W-SND-SAENT                 PIC  X(1)  VALUE '2'.                    
006600 77  W-SND-LOSS                  PIC  X(1)  VALUE '3'.                    
006700 77  W-SND-MOT                   PIC  X(1)  VALUE '4'.                    
006800 77  W-KLI-LOSS                  PIC S9(1)  VALUE +4 COMP-3.              
006900 77  W-KLI-MOT                   PIC S9(1)  VALUE +5 COMP-3.              
007000 77  W-KLI-SAK                   PIC S9(1)  VALUE +6 COMP-3.              
007100 77  W-KLI-AVV                   PIC S9(1)  VALUE +7 COMP-3.              
007200 77  W-IDRT                      PIC  X(3)  VALUE SPACE.                  
007300                                                                          
007500 77  SPAR-IDKOLLI                PIC S9(5)  VALUE ZERO COMP-3.            
007510 77  W-IDANSTNR                  PIC S9(5)  VALUE ZERO COMP-3.            
007600 77  W-KDRETSTA-NUM              PIC  X(1)  VALUE ZERO.                   
007900 77  W-KVKOLLI-SND               PIC S9(5)  VALUE ZERO COMP-3.            
007901 77  W-KVKOLLI-LOSS              PIC S9(5)  VALUE ZERO COMP-3.            
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
009300 77  SW-ADINLOMR                 PIC X       VALUE 'N'.                   
009400     88  ADINLOMR-IFYLLT                     VALUE 'J'.                   
009500                                                                          
009600 77  SW-4732                     PIC X       VALUE 'N'.                   
009700     88  STARTA-4732                         VALUE 'J'.                   
009800                                                                          
009810*      --- VALID IDDC CODES                                               
009820*                                                                         
009830*01    -COPY WWDC99                                                       
009840       EJECT                                                              
009900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010000 01  GENERELLA-SUBPROGRAM.                                                
010100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
010300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010800*01 -COPY WMEDAREA                                                        
010900     SKIP3                                                                
011000 01  MESSAGE-CODES.                                                       
011100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011200     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
011300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011710     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
011720     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
011800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011900     EJECT                                                                
012000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012300     SKIP3                                                                
012400*01 -COPY WMSGINIT                                                        
012500     SKIP3                                                                
012600*                                                                         
012700*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
012800   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
012900     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
013000                                                                          
013100 01  BILD-HOPP-AREOR.                                                     
013200                                                                          
013300   03    W-BILD               PIC X(4)    VALUE SPACE.                    
013400   03    W-HOPP-IDTRANS.                                                  
013500     05  FILLER               PIC X(1)    VALUE 'W'.                      
013600     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
013700     05  FILLER               PIC X(1)    VALUE 'T'.                      
013800     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
013900     05  FILLER               PIC X(2)    VALUE SPACE.                    
014000                                                                          
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
015400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015500     SKIP3                                                                
015600*01  MID -COPY W4I73101                                                   
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015900     SKIP3                                                                
016000*01  -COPY WMSGAREA                                                       
016100     EJECT                                                                
016200     03  MOD REDEFINES MSG-AREA.                                          
016300*      05  -COPY W4O73101   -PRE MOD-                                     
016400     EJECT                                                                
016500   03    MOD-MENY            REDEFINES MSG-AREA.                          
016600     05  FILLER              PIC X(4).                                    
016700     05  MOD-KDMFSFOR        PIC X(1).                                    
016800     05  MOD-TEMFSINF2       PIC X(55).                                   
016900     05  FILLER              PIC X(1873).                                 
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017200     SKIP3                                                                
017300*01  -COPY WMFSAREA                                                       
017400     EJECT                                                                
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
019810     03  SPAR-IDTRANS            PIC  X(4)          VALUE '4731'.         
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
025500     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
025600     SKIP3                                                                
025700     03  WLRETA01 REDEFINES IO-AREA.                                      
025800*        05  -COPY WDA301                                                 
025900     EJECT                                                                
026000 LINKAGE SECTION.                                                         
026100                                                                          
026200*01  -COPY W0009   -PRE MSG-                                              
026300*01  -COPY W0009   -PRE ALT-                                              
026400     EJECT                                                                
026500*01  -COPY W0009   -PRE DISP-                                             
026600     EJECT                                                                
026700*01  -COPY W0008   -PRE USEA-                                             
026800     05  FILLER                  PIC X.                                   
026900     EJECT                                                                
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
027800 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB   DISP-PCB                    
027900                           USEA-PCB RETA-PCB SEQB-PCB SEQC-PCB            
028000                           KOM-KOMA-PCB.                                  
028100     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB   DISP-PCB                    
028200                           USEA-PCB RETA-PCB SEQB-PCB SEQC-PCB            
028300                           KOM-KOMA-PCB.                                  
028400                                                                          
028500     PERFORM IMS-GET-MSG                                                  
028600     IF SEGMENT-FINNS                                                     
028700       PERFORM A-INIT                                                     
028800       PERFORM B-KOLLA-NYCKLAR                                            
028900       IF NYCKLAR-OK                                                      
029000         IF MFS-UPDATE                                                    
029100           PERFORM G-KOLLA-INPUT                                          
029200           IF INDATA-OK                                                   
029300             PERFORM H-UPPDATERA                                          
029400           END-IF                                                         
029500         ELSE                                                             
029600           IF MFS-FIRST                                                   
029700             PERFORM C-FOERSTA-SIDA                                       
029800           ELSE                                                           
029900             IF MFS-NEXT                                                  
030000               PERFORM D-NAESTA-SIDA                                      
030100             ELSE                                                         
030200               PERFORM E-SAMMA-SIDA                                       
030300             END-IF                                                       
030400           END-IF                                                         
030500         END-IF                                                           
030600         IF STARTA-4732 OR STARTA-ANNAN-BILD                              
030700            CONTINUE                                                      
030800         ELSE                                                             
030900            IF INDATA-OK                                                  
031000               PERFORM F-LAES-VISA-INFO                                   
031100            END-IF                                                        
031200         END-IF                                                           
031300       END-IF                                                             
031400       IF STARTA-4732 OR STARTA-ANNAN-BILD                                
031500          CONTINUE                                                        
031600       ELSE                                                               
031700          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73101 + 4                   
031800          PERFORM IMS-INSERT-MSG                                          
031900       END-IF                                                             
032000     END-IF                                                               
032100                                                                          
032200     MOVE ZERO TO RETURN-CODE                                             
032300     GOBACK                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 A-INIT SECTION.                                                          
032700                                                                          
032800     IF MSG-DUBBLA-TRANSKODER                                             
032900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73101                 
033000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
033100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
033200     ELSE                                                                 
033300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I73101                 
033400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
033500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
033600     END-IF                                                               
033700                                                                          
033800     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
033900     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
034000     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
034100                                                                          
034200     MOVE LOW-VALUE                       TO MSG-AREA                     
034300     MOVE 'W4O73101'                      TO MFS-IDMOD                    
034400     MOVE '4731'                          TO MOD-IDTRANS                  
034500     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
034510                                             MOD-TEMFSINF                 
034520                                                                          
034530     MOVE SPACE                           TO MED-IDMFSINF                 
034540     MOVE SPACE                           TO MED-IDMFSFEL                 
034550                                                                          
034600                                                                          
034700     IF EGEN-MID OR HELP-MID                                              
034800       CONTINUE                                                           
034900     ELSE                                                                 
035000       MOVE SPACE                         TO MFS-KDTRTYP                  
035100       MOVE '7'                           TO MFS-IDPFK                    
035200     END-IF                                                               
035300                                                                          
035400     MOVE LOW-VALUE                       TO W-WDA3CSEQ-MIN-X             
035500                                             W-WDA3BSEQ-MIN-X             
035600                                                                          
035700     MOVE HIGH-VALUE                      TO W-WDA3CSEQ-MAX-X             
035800                                             W-WDA3BSEQ-MAX-X             
035900                                                                          
036000     MOVE NEJ                             TO SW-IDRETSND                  
036100     .                                                                    
036200     EJECT                                                                
036300 B-KOLLA-NYCKLAR SECTION.                                                 
036400                                                                          
036500     MOVE ALL '+'                         TO MSGI-WMSGINIT                
036600     MOVE '001'                           TO MSGI-KDCALL                  
036700     MOVE MSG-SIGNON-USERID               TO MSGI-IDUSER                  
036710     MOVE '4731'                          TO MSGI-IDTRANS                 
036720     MOVE MSG-LTERM-NAME                  TO MSGI-IDLTERM-USER            
036800     IF GODK-MID                                                          
037101        IF MID-IDRT-IN = ALL '+'                                          
037102          IF MID-KDRETSTA-IN NOT = ALL '+'                                
037103            MOVE SPACE                    TO MSGI-IDRT                    
037104            MOVE ZERO                     TO MSGI-IDRTLOP                 
037105            MOVE MID-KDRETSTA-IN          TO MSGI-KDRETSTA                
037106          END-IF                                                          
037107        ELSE                                                              
037108          MOVE MID-IDRT-IN                TO MSGI-IDRT                    
037109          MOVE MID-IDRTLOP-IN             TO MSGI-IDRTLOP                 
037110          MOVE SPACE                      TO MSGI-KDRETSTA                
037120        END-IF                                                            
037200     END-IF                                                               
037300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037310                                                                          
037320     MOVE MSGI-SPAR-AREA   TO SPAR-AREA                                   
037400                                                                          
037420     IF MSGI-IDLAND-SPR = 'GB'                                            
037430       MOVE 'GB'                          TO MED-IDSKYLT                  
037440     ELSE                                                                 
037450       MOVE 'S '                          TO MED-IDSKYLT                  
037460     END-IF                                                               
037470                                                                          
037500     MOVE JA                              TO NYCKLAR-SW                   
037600                                                                          
037700     PERFORM BA-KOLLA-KDRETSTA                                            
037800     PERFORM BB-KOLLA-IDRETSND                                            
037810                                                                          
037811     MOVE MSGI-IDDC                       TO WS-IDDC                      
037812                                             W-IDDC-301                   
037813                                             W-IDDC-CSEQ-MIN              
037814                                             W-IDDC-CSEQ-MAX              
037815                                             W-IDDC-BSEQ-MIN              
037816                                             W-IDDC-BSEQ-MAX              
037817                                                                          
037820     IF IDRETSND-IFYLLT OR W-KDRETSTA-NUM > ZERO                          
037830         CONTINUE                                                         
037840     ELSE                                                                 
037850         MOVE NEJ                         TO NYCKLAR-SW                   
037860     END-IF                                                               
037900                                                                          
038000     IF GODK-MID OR NYCKLAR-OK                                            
038100        MOVE MSGI-KDRETSTA                TO MOD-KDRETSTA-UT              
038300        IF IDRETSND-IFYLLT                                                
038400           MOVE MSGI-IDRT                 TO MOD-IDRT-UT                  
038410           MOVE MSGI-IDRTLOP              TO MOD-IDRTLOP-UT               
038500        ELSE                                                              
038600           MOVE MFS-RENSA-FAELT           TO MOD-IDRT-UT                  
038610                                             MOD-IDRTLOP-UT               
038700        END-IF                                                            
038800     ELSE                                                                 
038900        MOVE MFS-RENSA-FAELT              TO MOD-KDRETSTA-UT              
038910                                             MOD-IDRT-UT                  
038920                                             MOD-IDRTLOP-UT               
039000     END-IF                                                               
039100                                                                          
039200     IF NYCKLAR-FEL                                                       
039300       MOVE ERR-WRONG-KEY                 TO MED-IDMFSFEL                 
039400       CALL WMEDKONV USING MED-WMEDAREA                                   
039500       MOVE MED-MFSFEL                    TO MOD-TEMFSFEL                 
039600       PERFORM MFS-RENSA-FAELT-IN                                         
039700       PERFORM MFS-RENSA-FAELT-UT                                         
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 BA-KOLLA-KDRETSTA   SECTION.                                             
040200                                                                          
040300     MOVE MFS-RENSA-FAELT                 TO MOD-KDRETSTA-IN              
040400                                                                          
040500     IF MID-KDRETSTA-IN         NOT = ALL '+'                             
040900       MOVE '7'                           TO MFS-IDPFK                    
041000       MOVE SPACE                         TO MFS-KDTRTYP                  
041100     END-IF                                                               
041200                                                                          
041300     MOVE MSGI-KDRETSTA                   TO W-KDRETSTA                   
041400     IF W-REG-KEY    OR                                                   
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
043600                                                                          
043700     MOVE MFS-RENSA-FAELT                 TO MOD-IDRT-IN                  
043800                                             MOD-IDRTLOP-IN               
043900                                                                          
044000     IF MID-IDRT-IN NOT = ALL '+'                                         
044100       MOVE '7'                           TO MFS-IDPFK                    
044200       MOVE SPACE                         TO MFS-KDTRTYP                  
044300     END-IF                                                               
044800                                                                          
044821     IF (MID-KDRETSTA-IN NOT = ALL '+') AND                               
044823         MID-IDRT-IN         = ALL '+'                                    
044824       CONTINUE                                                           
044830     ELSE                                                                 
044900       IF MSGI-IDRTLOP NUMERIC AND                                        
045000          MSGI-IDRTLOP > ZERO                                             
045100          MOVE JA                         TO SW-IDRETSND                  
045200          MOVE MSGI-IDRTLOP               TO W-IDRTLOP-BSEQ-MIN           
045300                                             W-IDRTLOP-BSEQ-MAX           
045400                                             MOD-IDRTLOP-UT               
045500       END-IF                                                             
045600                                                                          
045800       IF MSGI-IDRT NOT = SPACE                                           
045900          MOVE MSGI-IDRT                  TO W-IDRT-CSEQ-MIN              
046200                                             W-IDRT-CSEQ-MAX              
046300                                             W-IDRT-BSEQ-MIN              
046400                                             W-IDRT-BSEQ-MAX              
046500                                             MOD-IDRT-UT                  
046600       END-IF                                                             
046610     END-IF                                                               
046700                                                                          
046800     .                                                                    
046900     EJECT                                                                
047020 C-FOERSTA-SIDA SECTION.                                                  
047100                                                                          
047200     MOVE INF-FIRST-PAGE                  TO MED-IDMFSINF                 
047300     CALL WMEDKONV USING MED-WMEDAREA                                     
047400     MOVE MED-MFSINF                      TO MOD-TEMFSFEL                 
047500                                                                          
047600*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
047700     PERFORM MFS-RENSA-FAELT-IN                                           
047800     .                                                                    
047900     EJECT                                                                
048000 D-NAESTA-SIDA SECTION.                                                   
048100                                                                          
048310     IF SPAR-IDTRANS = '4731'                                             
048400       MOVE SPAR-WDA3CSEQ-NEXT            TO W-WDA3CSEQ-MIN-X             
048500       MOVE MSGI-IDDC                     TO W-IDDC-CSEQ-MIN              
048800     ELSE                                                                 
048900       MOVE LOW-VALUE                     TO W-WDA3CSEQ-MIN-X             
048910       MOVE MSGI-IDDC                     TO W-IDDC-CSEQ-MIN              
049000       PERFORM MFS-RENSA-FAELT-IN                                         
049010       PERFORM MFS-RENSA-FAELT-UT                                         
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 E-SAMMA-SIDA SECTION.                                                    
049500                                                                          
049710     IF SPAR-IDTRANS = '4731'                                             
049800       MOVE SPAR-WDA3CSEQ-ENTER           TO W-WDA3CSEQ-MIN-X             
049900       MOVE MSGI-IDDC                     TO W-IDDC-CSEQ-MIN              
050200       IF MID-INPUT = ALL '+'                                             
050300          PERFORM MFS-RENSA-FAELT-IN                                      
050400       ELSE                                                               
050500          MOVE +1                         TO INDX                         
050600          PERFORM UNTIL INDX    >  MAX-INDX                               
050700             IF MID-KDCMD(INDX) NUMERIC                                   
050800                PERFORM EA-STARTA-ANNAN-BILD                              
050900                MOVE JA                   TO SW-STARTA-ANNAN-BILD         
051000                MOVE MAX-INDX             TO INDX                         
051010             ELSE                                                         
051020                IF MID-KDCMD(INDX) = ALL '+'                              
051021                  MOVE MFS-RENSA-FAELT    TO MOD-KDCMD(INDX)              
051022                  MOVE MFS-ALFA-FAELT-RAETT TO                            
051023                                      MOD-KDCMD-ATTR(INDX)                
051030                ELSE                                                      
051031                  MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD(INDX)              
051032                  MOVE MFS-ADD-LAES-IN-FAELT TO                           
051033                                      MOD-KDCMD-ATTR(INDX)                
051040                END-IF                                                    
051100             END-IF                                                       
051200             ADD +1                       TO INDX                         
051300          END-PERFORM                                                     
051400          IF STARTA-ANNAN-BILD                                            
051500             CONTINUE                                                     
051600          ELSE                                                            
051700             MOVE INF-PRESS-PF11          TO MED-IDMFSINF                 
051800             CALL WMEDKONV USING MED-WMEDAREA                             
051900             MOVE MED-MFSINF              TO MOD-TEMFSFEL                 
052000          END-IF                                                          
052100       END-IF                                                             
052200     ELSE                                                                 
052300       MOVE LOW-VALUE                     TO W-WDA3CSEQ-MIN-X             
052310       MOVE MSGI-IDDC                     TO W-IDDC-CSEQ-MIN              
052400       PERFORM MFS-RENSA-FAELT-IN                                         
052401     END-IF                                                               
052600     .                                                                    
052700     EJECT                                                                
052800 EA-STARTA-ANNAN-BILD  SECTION.                                           
052900                                                                          
053100     MOVE MID-IDRT(INDX)                  TO MSGI-IDRT                    
053200     MOVE MID-IDRTLOP(INDX)               TO MSGI-IDRTLOP                 
053300     MOVE '001'                           TO MSGI-KDCALL                  
053400     MOVE MSG-SIGNON-USERID               TO MSGI-IDUSER                  
053500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
053600                                                                          
053800     MOVE LOW-VALUE                       TO P-TO-P-KDZ1                  
053900     MOVE LOW-VALUE                       TO P-TO-P-KDZ2                  
054000     MOVE MID-KDCMD(INDX) (1:1)           TO W-HOPP-IDTRANS-2             
054100     MOVE MID-KDCMD(INDX) (2:3)           TO W-HOPP-IDTRANS-4-6           
054200     MOVE W-HOPP-IDTRANS                  TO P-TO-P-KDTRANS               
054300     MOVE '4731'                          TO P-TO-P-IDTRANS               
054400     MOVE MFS-KDMFSFOR                    TO P-TO-P-KDMFSFOR              
054500                                                                          
054600     PERFORM S01-INSERT-ALTMSG                                            
054700     .                                                                    
054800     EJECT                                                                
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
055710       IF MFS-UPDATE                                                      
055711         PERFORM MFS-RENSA-FAELT-UT                                       
055720       ELSE                                                               
055730         IF MFS-NEXT                                                      
055740           MOVE INF-LAST-PAGE-SHOWN        TO MED-IDMFSFEL                
055741           PERFORM MFS-ROER-EJ-FAELT-UT                                   
055750         ELSE                                                             
055800           MOVE ERR-INFO-MISSING           TO MED-IDMFSFEL                
055801           PERFORM MFS-RENSA-FAELT-IN                                     
055802           PERFORM MFS-RENSA-FAELT-UT                                     
055810         END-IF                                                           
055900         CALL WMEDKONV USING MED-WMEDAREA                                 
056000         MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                
056102         MOVE W-WDA3CSEQ-MIN-X             TO SPAR-WDA3CSEQ-ENTER         
056110       END-IF                                                             
056200     ELSE                                                                 
056210       PERFORM FA-FIXA-ENTER-KEY                                          
056220                                                                          
056300       MOVE +1                            TO INDX                         
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
058000         ELSE                                                             
058100           MOVE MFS-STAENG-FAELT          TO MOD-KDCMD-ATTR (INDX)        
058200           MOVE MFS-RENSA-FAELT           TO MOD-KDCMD      (INDX)        
058210                                             MOD-IDRT       (INDX)        
058300                                             MOD-IDRTLOP    (INDX)        
058400                                             MOD-TISNDDAT   (INDX)        
058500                                             MOD-KVKOLLI-SND(INDX)        
058600                                             MOD-TILOSSN    (INDX)        
058700                                           MOD-ADINLOMR-LOSS(INDX)        
058800                                           MOD-IDANSTNR-LOSS(INDX)        
058900                                           MOD-KVKOLLI-LOSS (INDX)        
059000                                           MOD-TIINLMOT     (INDX)        
059100                                           MOD-ADINLOMR-MOT (INDX)        
059200                                           MOD-IDANSTNR-MOT (INDX)        
059300                                           MOD-KVKOLLI-MOT  (INDX)        
059400         END-IF                                                           
059500         ADD 1                          TO INDX                           
059600       END-PERFORM                                                        
059610                                                                          
059620* OM DET FINNS ETT 12:E SEGMENT.                                          
059630                                                                          
059700       IF SEGMENT-FINNS                                                   
059800         PERFORM FC-FIXA-NEXT-KEY                                         
059801       ELSE                                                               
059802         IF MED-IDMFSINF = SPACE                                          
059811           MOVE INF-LAST-PAGE           TO MED-IDMFSINF                   
059812           CALL WMEDKONV USING MED-WMEDAREA                               
059813           MOVE MED-MFSINF              TO MOD-TEMFSINF                   
059820         END-IF                                                           
059840       END-IF                                                             
059900                                                                          
060000       MOVE '002'                       TO MSGI-KDCALL                    
060100       MOVE '4731'                      TO SPAR-IDTRANS                   
060110       MOVE SPAR-AREA                   TO MSGI-SPAR-AREA                 
060200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
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
062700                                                                          
062800     MOVE RET-IDRT                      TO MOD-IDRT         (INDX)        
062900     MOVE RET-IDRTLOP                   TO MOD-IDRTLOP      (INDX)        
063000     MOVE RET-DASNDDAT (3:6)            TO MOD-TISNDDAT     (INDX)        
063001                                                                          
063010     PERFORM FBA-RAEKNA-KOLLI                                             
063020                                                                          
063030     IF W-KVKOLLI-SND         >  ZERO                                     
063040        MOVE W-KVKOLLI-SND              TO MOD-KVKOLLI-SND  (INDX)        
063050     ELSE                                                                 
063051        IF RET-TILOSSN      > ZERO AND                                    
063052           RET-KVKOLLI-LOSS > ZERO                                        
063053          MOVE RET-KVKOLLI-LOSS         TO MOD-KVKOLLI-SND  (INDX)        
063054        ELSE                                                              
063055          IF RET-TIINLMOT    > ZERO AND                                   
063056             RET-KVKOLLI-MOT > ZERO                                       
063057            MOVE RET-KVKOLLI-MOT        TO MOD-KVKOLLI-SND  (INDX)        
063058          ELSE                                                            
063059            MOVE MFS-RENSA-FAELT        TO MOD-KVKOLLI-SND  (INDX)        
063060          END-IF                                                          
063061        END-IF                                                            
063070     END-IF                                                               
063100     IF RET-KDRETSTA   >  W-SND-REG                                       
063210        IF RET-TILOSSN >  ZERO  AND CDC                                   
063300           MOVE RET-TILOSSN             TO MOD-TILOSSN      (INDX)        
063400           MOVE RET-ADINLOMR-LOSS       TO MOD-ADINLOMR-LOSS(INDX)        
063500           MOVE RET-IDANSTNR-LOSS       TO MOD-IDANSTNR-LOSS(INDX)        
063510           IF RET-KVKOLLI-LOSS > ZERO                                     
063511             MOVE RET-KVKOLLI-LOSS      TO MOD-KVKOLLI-LOSS (INDX)        
063512           ELSE                                                           
063513             IF RET-KVKOLLI-MOT > ZERO                                    
063514                MOVE RET-KVKOLLI-MOT    TO MOD-KVKOLLI-LOSS (INDX)        
063515             ELSE                                                         
063516               IF W-KVKOLLI-LOSS > ZERO                                   
063517                 MOVE W-KVKOLLI-LOSS    TO MOD-KVKOLLI-LOSS (INDX)        
063518               ELSE                                                       
063519                 IF W-KVKOLLI-SND > ZERO                                  
063520                   MOVE ZERO            TO MOD-KVKOLLI-LOSS (INDX)        
063521                 ELSE                                                     
063523                   MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI-LOSS (INDX)        
063524                 END-IF                                                   
063525               END-IF                                                     
063526             END-IF                                                       
063527           END-IF                                                         
063530                                                                          
063600        ELSE                                                              
063700           MOVE MFS-RENSA-FAELT         TO MOD-TILOSSN      (INDX)        
063800                                           MOD-ADINLOMR-LOSS(INDX)        
063900                                           MOD-IDANSTNR-LOSS(INDX)        
064000                                           MOD-KVKOLLI-LOSS (INDX)        
064100        END-IF                                                            
064200        IF RET-TIINLMOT           >  ZERO                                 
064300           MOVE RET-TIINLMOT            TO MOD-TIINLMOT     (INDX)        
064400           MOVE RET-ADINLOMR-MOT        TO MOD-ADINLOMR-MOT (INDX)        
064500           MOVE RET-IDANSTNR-MOT        TO MOD-IDANSTNR-MOT (INDX)        
064506           IF RET-KVKOLLI-MOT     > ZERO                                  
064507             MOVE RET-KVKOLLI-MOT       TO MOD-KVKOLLI-MOT  (INDX)        
064508           ELSE                                                           
064509             IF RET-KVKOLLI-LOSS  > ZERO                                  
064510                MOVE RET-KVKOLLI-LOSS   TO MOD-KVKOLLI-MOT  (INDX)        
064511             ELSE                                                         
064512               IF W-KVKOLLI-MOT   > ZERO                                  
064513                 MOVE W-KVKOLLI-MOT     TO MOD-KVKOLLI-MOT  (INDX)        
064520               ELSE                                                       
064530                 IF W-KVKOLLI-SND > ZERO                                  
064540                   MOVE ZERO            TO MOD-KVKOLLI-MOT  (INDX)        
064550                 ELSE                                                     
064590                   MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI-MOT  (INDX)        
064591                 END-IF                                                   
064592               END-IF                                                     
064593             END-IF                                                       
064594           END-IF                                                         
064600        ELSE                                                              
064700           MOVE MFS-RENSA-FAELT         TO MOD-TIINLMOT     (INDX)        
064800                                           MOD-ADINLOMR-MOT (INDX)        
064900                                           MOD-IDANSTNR-MOT (INDX)        
065000                                           MOD-KVKOLLI-MOT  (INDX)        
065100        END-IF                                                            
065200                                                                          
066000                                                                          
067300     ELSE                                                                 
067400        MOVE MFS-RENSA-FAELT            TO MOD-TILOSSN      (INDX)        
067500                                           MOD-ADINLOMR-LOSS(INDX)        
067600                                           MOD-IDANSTNR-LOSS(INDX)        
067700                                           MOD-KVKOLLI-LOSS (INDX)        
067800                                           MOD-TIINLMOT     (INDX)        
067900                                           MOD-ADINLOMR-MOT (INDX)        
068000                                           MOD-IDANSTNR-MOT (INDX)        
068100                                           MOD-KVKOLLI-MOT  (INDX)        
068200     END-IF                                                               
068300     .                                                                    
068400     EJECT                                                                
068500 FBA-RAEKNA-KOLLI         SECTION.                                        
068600                                                                          
068700     MOVE ZERO                          TO W-KVKOLLI-SND                  
068800                                           W-KVKOLLI-LOSS                 
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
070610         IF RET-TILOSSN > ZERO                                            
070620            IF RET-KDKOLSTA = W-KLI-SAK                                   
070640              CONTINUE                                                    
070641            ELSE                                                          
070650              ADD +1                    TO W-KVKOLLI-LOSS                 
070660            END-IF                                                        
070670         END-IF                                                           
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
072200 FC-FIXA-NEXT-KEY        SECTION.                                         
072300                                                                          
072410     IF MED-IDMFSINF = SPACE OR '006'                                     
072500       MOVE INF-MORE-INFO-EXISTS          TO MED-IDMFSINF                 
072600       CALL WMEDKONV USING MED-WMEDAREA                                   
072700       MOVE MED-MFSINF                    TO MOD-TEMFSINF                 
072710     END-IF                                                               
072800                                                                          
072900     MOVE RET-IDDC                      TO SPAR-IDDC-NEXT                 
072910     MOVE RET-KDRETSTA                  TO SPAR-KDRETSTA-NEXT             
073000     MOVE RET-DARETANK                  TO SPAR-DARETANK-NEXT             
073100     MOVE RET-IDRT                      TO SPAR-IDRT-NEXT                 
073200     MOVE RET-IDRTLOP                   TO SPAR-IDRTLOP-NEXT              
074000                                                                          
074100     .                                                                    
074200     EJECT                                                                
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
075300     IF INDATA-FEL                                                        
075310        IF MED-IDMFSFEL = SPACE                                           
075320          MOVE ERR-CORR-HILITE-FLDS     TO MED-IDMFSFEL                   
075500          CALL WMEDKONV USING MED-WMEDAREA                                
075600          MOVE MED-MFSFEL               TO MOD-TEMFSFEL                   
075700          PERFORM MFS-ROER-EJ-FAELT-UT                                    
075800          PERFORM MFS-ROER-EJ-FAELT-IN                                    
075810        END-IF                                                            
075900     END-IF                                                               
076000                                                                          
076100     .                                                                    
076200     EJECT                                                                
076300                                                                          
076400 GA-FORMELL-KONTROLL SECTION.                                             
076500                                                                          
076600     IF MID-INPUT                = ALL '+'                                
076700       MOVE ERR-PF11-AND-NO-DATA        TO MED-IDMFSFEL                   
076800       CALL WMEDKONV USING MED-WMEDAREA                                   
076900       MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                   
077000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
077100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
077200       MOVE NEJ                         TO INDATA-SW                      
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
078700     IF MID-IDANSTNR NOT = ALL '+'                                        
078800        IF MID-IDANSTNR NUMERIC                                           
078900           MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDANSTNR-ATTR              
079000           MOVE JA                      TO SW-IDANSTNR                    
079100           MOVE MID-IDANSTNR            TO W-IDANSTNR                     
079200        ELSE                                                              
079300           MOVE MFS-NUM-FAELT-FEL       TO MOD-IDANSTNR-ATTR              
079400           MOVE NEJ                     TO INDATA-SW                      
079500        END-IF                                                            
079600     END-IF                                                               
079700                                                                          
079800     .                                                                    
079900     EJECT                                                                
080000 GAB-KOLLA-ADINLOMR   SECTION.                                            
080100                                                                          
080200     MOVE NEJ                           TO  SW-ADINLOMR                   
080300                                                                          
080400     IF MID-ADINLOMR NOT = ALL '+'                                        
080500        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-ADINLOMR-ATTR              
080600        MOVE JA                         TO  SW-ADINLOMR                   
080700     END-IF                                                               
080800                                                                          
080900     .                                                                    
081000     EJECT                                                                
081100 GAC-KOLLA-KDCMD      SECTION.                                            
081200                                                                          
081300     MOVE +1                            TO INDX                           
081400                                                                          
081500     PERFORM UNTIL INDX >  MAX-INDX                                       
081600        IF MID-KDCMD(INDX) NOT = ALL '+'                                  
081700                                                                          
081800           IF MID-KDCMD(INDX) = W-MOTTAGNING OR                           
081810                                W-LOSSNING   OR                           
081900                                W-AVVIKELSE  OR                           
081910                                W-BORTTAG    OR                           
081911                                W-RECEIVING  OR                           
081912                                W-DEVIATION  OR                           
081913                                W-DELETE                                  
081920             IF MID-KDCMD(INDX) = W-LOSSNING AND NDC                      
081931                MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)           
081932                MOVE NEJ              TO INDATA-SW                        
081940             END-IF                                                       
082000                                                                          
082100             MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDCMD-ATTR(INDX)           
082300                                                                          
082400             IF MID-KDCMD(INDX) =  W-MOTTAGNING OR                        
082410                                   W-LOSSNING   OR                        
082420                                   W-RECEIVING                            
082500                IF MID-IDRT(INDX) = W-CDC OR W-US1 OR W-US2 OR            
082510                                    W-US3 OR W-CA1 OR W-JP1 OR            
082530                                    W-AU1 OR W-SE1 OR W-GB1 OR            
082540                                    W-SE2 OR W-GB2 OR W-GB3 OR            
082550                                    W-NL1 OR W-IT1                        
082600                   CONTINUE                                               
082700*                  DESSA OBL FÄLT FINNS REDAN FRÅN 4733 OM CDC-SND        
082800                ELSE                                                      
082900                   PERFORM GACA-KOLLA-OBL-FAELT                           
083000                END-IF                                                    
083100             END-IF                                                       
083200                                                                          
083210             IF MID-KDCMD(INDX)   =  W-BORTTAG OR W-DELETE                
083220                IF MID-IDRT(INDX) = W-CDC OR W-US1 OR W-US2 OR            
083221                                    W-US3 OR W-CA1 OR W-JP1 OR            
083223                                    W-AU1 OR W-SE1 OR W-GB1 OR            
083224                                    W-SE2 OR W-GB2 OR W-GB3 OR            
083225                                    W-NL1 OR W-IT1                        
083230                   CONTINUE                                               
083240                ELSE                                                      
083241                  MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)         
083242                  MOVE NEJ              TO INDATA-SW                      
083260                END-IF                                                    
083280             END-IF                                                       
083290                                                                          
083300             IF MID-KDCMD(INDX)    = W-AVVIKELSE OR W-DEVIATION           
083400                 MOVE JA                TO SW-4732                        
083600                 MOVE MID-IDRT(INDX)    TO MSGI-IDRT                      
083800                 MOVE MID-IDRTLOP(INDX) TO MSGI-IDRTLOP                   
084000             END-IF                                                       
084100                                                                          
084200           ELSE                                                           
084300             MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDCMD-ATTR(INDX)           
084400             MOVE NEJ                   TO INDATA-SW                      
084500           END-IF                                                         
084600        END-IF                                                            
084700        ADD +1                          TO INDX                           
084800     END-PERFORM                                                          
084900                                                                          
085000     .                                                                    
085100     EJECT                                                                
085200                                                                          
085300 GACA-KOLLA-OBL-FAELT SECTION.                                            
085400                                                                          
085500     IF IDANSTNR-IFYLLT                                                   
085600         CONTINUE                                                         
085700     ELSE                                                                 
085800         MOVE MFS-NUM-FAELT-FEL         TO MOD-IDANSTNR-ATTR              
085900         MOVE NEJ                       TO INDATA-SW                      
086000     END-IF                                                               
086100                                                                          
086200     IF ADINLOMR-IFYLLT                                                   
086300         CONTINUE                                                         
086400     ELSE                                                                 
086500         MOVE MFS-ALFA-FAELT-FEL        TO MOD-ADINLOMR-ATTR              
086600         MOVE NEJ                       TO INDATA-SW                      
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000 GB-LOGISK-KONTROLL SECTION.                                              
087100                                                                          
087200     MOVE +1                            TO INDX                           
087300     PERFORM UNTIL INDX >  MAX-INDX                                       
087400        IF MID-KDCMD(INDX)   =  W-MOTTAGNING OR                           
087401                                W-LOSSNING   OR                           
087410                                W-BORTTAG    OR                           
087420                                W-RECEIVING  OR                           
087430                                W-DELETE                                  
087500           PERFORM GBA-KOLLA-VALD-SANDNING                                
087600        END-IF                                                            
087700        ADD +1                          TO INDX                           
087800     END-PERFORM                                                          
087900                                                                          
088000     .                                                                    
088100     EJECT                                                                
088200 GBA-KOLLA-VALD-SANDNING          SECTION.                                
088300                                                                          
088400     MOVE MID-IDRT(INDX)                TO W-IDRT-BSEQ-MIN                
088500                                           W-IDRT-BSEQ-MAX                
088600     MOVE MID-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN             
088700                                           W-IDRTLOP-BSEQ-MAX             
088800     PERFORM IMS-GHU-SEQB-WLRETA01                                        
088900     IF SEGMENT-FINNS                                                     
089000                                                                          
089100        IF MID-KDCMD(INDX) =  W-LOSSNING                                  
089200           IF RET-KDRETSTA =  W-SND-SAENT                                 
089210              IF MID-IDANSTNR = ALL '+'                                   
089220                IF RET-IDANSTNR-LOSS = ZERO                               
089221                  MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANSTNR-ATTR            
089222                  MOVE NEJ              TO INDATA-SW                      
089230                END-IF                                                    
089240              END-IF                                                      
089400           ELSE                                                           
089500              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)           
089600              MOVE NEJ                  TO INDATA-SW                      
089700           END-IF                                                         
089800        END-IF                                                            
089900                                                                          
089910        IF MID-KDCMD(INDX)         =  W-BORTTAG OR W-DELETE               
089920           IF RET-KDRETSTA         =  W-SND-SAENT OR                      
089921             (RET-KDRETSTA         =  W-SND-LOSS  AND                     
089922              RET-IDDISTR          = ZERO)                                
089930              CONTINUE                                                    
089940           ELSE                                                           
089950              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)           
089960              MOVE NEJ                  TO INDATA-SW                      
089970           END-IF                                                         
089980        END-IF                                                            
089990                                                                          
090000        IF MID-KDCMD(INDX)         =  W-MOTTAGNING OR W-RECEIVING         
090100           IF (RET-KDRETSTA        =  W-SND-SAENT OR                      
090200              RET-KDRETSTA         =  W-SND-LOSS) AND                     
090210              RET-IDDISTR          >  ZERO                                
090310              IF MID-IDANSTNR = ALL '+'                                   
090320                IF RET-IDANSTNR-MOT  = ZERO                               
090330                  MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANSTNR-ATTR            
090340                  MOVE NEJ              TO INDATA-SW                      
090350                END-IF                                                    
090360              END-IF                                                      
090400           ELSE                                                           
090500              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)           
090600              MOVE NEJ                  TO INDATA-SW                      
090700           END-IF                                                         
090800        END-IF                                                            
090900     ELSE                                                                 
091000        MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDCMD-ATTR(INDX)           
091100        MOVE NEJ                        TO INDATA-SW                      
091200     END-IF                                                               
091300     .                                                                    
091400     EJECT                                                                
091500 H-UPPDATERA SECTION.                                                     
091600                                                                          
091700     MOVE +1                            TO 4792-INDX                      
091800     ACCEPT DAGENS-DATUM FROM DATE                                        
091900     IF STARTA-4732                                                       
092000        PERFORM HA-STARTA-4732                                            
092100     ELSE                                                                 
092200        PERFORM HB-UPPDATERA-VALDA-SAENDNINGAR                            
092300     END-IF                                                               
092400                                                                          
092500     IF 4792-INDX >  +1                                                   
092600        PERFORM S03-STARTA-R31-RAPPORTERING                               
092700     END-IF                                                               
092800                                                                          
092900     MOVE INF-UPDATE-DONE               TO MED-IDMFSINF                   
093000     CALL WMEDKONV USING MED-WMEDAREA                                     
093100     MOVE MED-MFSINF                    TO MOD-TEMFSINF                   
093200     PERFORM MFS-FORM-ATTR                                                
093300     PERFORM MFS-RENSA-FAELT-IN                                           
093400     .                                                                    
093500     EJECT                                                                
093600                                                                          
093700 HA-STARTA-4732  SECTION.                                                 
093800                                                                          
093810     MOVE MID-IDRT(INDX)                TO MSGI-IDRT                      
093820     MOVE MID-IDRTLOP(INDX)             TO MSGI-IDRTLOP                   
093900     MOVE '001'                         TO MSGI-KDCALL                    
094000     MOVE MSG-SIGNON-USERID             TO MSGI-IDUSER                    
094100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
094200                                                                          
094300     MOVE LOW-VALUE                     TO P-TO-P-KDZ1                    
094400     MOVE LOW-VALUE                     TO P-TO-P-KDZ2                    
094500     MOVE 'W4T732  '                    TO P-TO-P-KDTRANS                 
094600     MOVE '4731'                        TO P-TO-P-IDTRANS                 
094700     MOVE MFS-KDMFSFOR                  TO P-TO-P-KDMFSFOR                
094800                                                                          
094900     PERFORM S01-INSERT-ALTMSG                                            
095000     .                                                                    
095100     EJECT                                                                
095200 HB-UPPDATERA-VALDA-SAENDNINGAR SECTION.                                  
095300                                                                          
095400     MOVE +1                            TO INDX                           
095500     PERFORM UNTIL INDX >  MAX-INDX                                       
095600        IF MID-KDCMD(INDX) =  W-LOSSNING                                  
095700           PERFORM HBA-RAPPORTERA-LOSSAT                                  
095800        END-IF                                                            
095900                                                                          
096000        IF MID-KDCMD(INDX) =  W-MOTTAGNING OR W-RECEIVING                 
096100           PERFORM HBB-RAPPORTERA-MOTTAGET                                
096200        END-IF                                                            
096210        IF MID-KDCMD(INDX) =  W-BORTTAG OR W-DELETE                       
096220           PERFORM HBC-BORTTAG                                            
096230        END-IF                                                            
096300        ADD +1                          TO INDX                           
096400     END-PERFORM                                                          
096500                                                                          
096600     .                                                                    
096700     EJECT                                                                
096800                                                                          
096900 HBA-RAPPORTERA-LOSSAT      SECTION.                                      
097000                                                                          
097100     MOVE MID-IDRT(INDX)                TO W-IDRT-BSEQ-MIN                
097200                                           W-IDRT-BSEQ-MAX                
097300     MOVE MID-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN             
097400                                           W-IDRTLOP-BSEQ-MAX             
097500     PERFORM IMS-GHU-SEQB-WLRETA01                                        
097600                                                                          
097700     IF SEGMENT-FINNS                                                     
097800        PERFORM UNTIL SEGMENT-SAKNAS                                      
097900           IF RET-KDKOLSTA =  W-KLI-SAK OR W-KLI-AVV                      
098000              CONTINUE                                                    
098100           ELSE                                                           
098200              IF MID-ADINLOMR =  ALL '+'                                  
098300                 CONTINUE                                                 
098400              ELSE                                                        
098500                 MOVE MID-ADINLOMR      TO RET-ADINLOMR-LOSS              
098510                                           RET-ADINLOMR                   
098600              END-IF                                                      
098700              IF MID-IDANSTNR =  ALL '+'                                  
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
101200     MOVE MID-IDRT(INDX)                TO W-IDRT-BSEQ-MIN                
101300                                           W-IDRT-BSEQ-MAX                
101400     MOVE MID-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN             
101500                                           W-IDRTLOP-BSEQ-MAX             
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
102500              END-IF                                                      
102600              IF MID-ADINLOMR =  ALL '+'                                  
102700                 CONTINUE                                                 
102800              ELSE                                                        
102900                 MOVE MID-ADINLOMR      TO RET-ADINLOMR-MOT               
102910                                           RET-ADINLOMR                   
103000              END-IF                                                      
103100              IF MID-IDANSTNR =  ALL '+'                                  
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
105230     MOVE MID-IDRT(INDX)                TO W-IDRT-BSEQ-MIN                
105240                                           W-IDRT-BSEQ-MAX                
105250     MOVE MID-IDRTLOP(INDX)             TO W-IDRTLOP-BSEQ-MIN             
105260                                           W-IDRTLOP-BSEQ-MAX             
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
105321          PERFORM IMS-GN-SEQB-WLRETA01                                    
105322        END-IF                                                            
105323     END-PERFORM                                                          
105326     .                                                                    
105327     EJECT                                                                
105328                                                                          
105330                                                                          
105400 S01-INSERT-ALTMSG SECTION.                                               
105500                                                                          
105600     MOVE P-TO-P-SW                     TO MSG-IO-AREA                    
105700     PERFORM IMS-CHANGE-ALTMSG                                            
105800     IF STATUS-OK                                                         
105900       PERFORM IMS-INSERT-ALTMSG                                          
106000     ELSE                                                                 
106100       MOVE LOW-VALUE                   TO MSG-AREA                       
106200       MOVE 'W4O73101'                  TO MFS-IDMOD                      
106300       MOVE '4731'                      TO MOD-IDTRANS                    
106400       MOVE P-TO-P-KDTRANS (2:1)        TO W-BILD (1:1)                   
106500       MOVE P-TO-P-KDTRANS (4:3)        TO W-BILD (2:3)                   
106600       IF SECURITY-FEL                                                    
106700         STRING 'NOT AUTHORIZED TO USE '                                  
106800                W-BILD                                                    
106900                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
107000       ELSE                                                               
107100         STRING 'WRONG PICTURE '                                          
107200                 W-BILD                                                   
107300                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
107400       END-IF                                                             
107500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73101 + 4                      
107600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
107700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
107800       PERFORM IMS-INSERT-MSG                                             
107900     END-IF                                                               
108000     .                                                                    
108100     EJECT                                                                
108200                                                                          
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
110800     MOVE 'W4073100'                    TO MSG-KOM-IDSNDJOB               
110900     MOVE DAGENS-DATUM                  TO MSG-KOM-TIREGDAT               
111000     MOVE DAGENS-TID                    TO MSG-KOM-TIKLOCK                
111100     MOVE SPACE                         TO MSG-KOM-IDMFSMED               
111200                                                                          
111300     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
111400                                  LENGTH OF MOD4792-MID-W4I79201          
111500                                                                          
111600     MOVE 'W4T792X '                    TO P-TO-P-MSG-KDTRANS             
111700     MOVE '4731'                        TO P-TO-P-MSG-IDTRANS             
111800     MOVE MFS-KDMFSFOR                  TO P-TO-P-MSG-KDMFSFOR            
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
113200 MFS-RENSA-FAELT-UT SECTION.                                              
113300                                                                          
113400     MOVE +1                            TO INDX                           
113500     PERFORM UNTIL INDX >  MAX-INDX                                       
113600        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
113700        ADD +1                          TO INDX                           
113800     END-PERFORM                                                          
113900     .                                                                    
114000     SKIP3                                                                
114100 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
114200                                                                          
114300     MOVE MFS-RENSA-FAELT        TO MOD-IDRT          (INDX)              
114400                                    MOD-IDRTLOP       (INDX)              
114500                                    MOD-TISNDDAT      (INDX)              
114600                                    MOD-KVKOLLI-SND   (INDX)              
114700                                    MOD-TILOSSN       (INDX)              
114800                                    MOD-ADINLOMR-LOSS (INDX)              
114900                                    MOD-IDANSTNR-LOSS (INDX)              
115000                                    MOD-KVKOLLI-LOSS  (INDX)              
115100                                    MOD-TIINLMOT      (INDX)              
115200                                    MOD-ADINLOMR-MOT (INDX)               
115300                                    MOD-IDANSTNR-MOT (INDX)               
115400                                    MOD-KVKOLLI-MOT  (INDX)               
115500     .                                                                    
115600     SKIP3                                                                
115700 MFS-RENSA-FAELT-IN SECTION.                                              
115800                                                                          
115900*    --- ALLA INDATA-FÄLT                                                 
116000     MOVE MFS-RENSA-FAELT       TO MOD-IDANSTNR-UPD                       
116100                                   MOD-ADINLOMR-UPD                       
116200                                                                          
116300     MOVE +1 TO INDX                                                      
116400     PERFORM UNTIL INDX         >  MAX-INDX                               
116500       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
116600       ADD +1                   TO INDX                                   
116700     END-PERFORM                                                          
116800     .                                                                    
116900     EJECT                                                                
117000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
117100                                                                          
117200     MOVE +1                  TO INDX                                     
117300     PERFORM UNTIL INDX       >  MAX-INDX                                 
117400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
117500       ADD +1                 TO INDX                                     
117600     END-PERFORM                                                          
117700     .                                                                    
117800                                                                          
117900                                                                          
118000 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
118100                                                                          
118200     MOVE MFS-ROER-EJ-FAELT      TO MOD-IDRT          (INDX)              
118300                                    MOD-IDRTLOP       (INDX)              
118400                                    MOD-TISNDDAT      (INDX)              
118500                                    MOD-KVKOLLI-SND   (INDX)              
118600                                    MOD-TILOSSN       (INDX)              
118700                                    MOD-ADINLOMR-LOSS (INDX)              
118800                                    MOD-IDANSTNR-LOSS (INDX)              
118900                                    MOD-KVKOLLI-LOSS  (INDX)              
119000                                    MOD-TIINLMOT      (INDX)              
119100                                    MOD-ADINLOMR-MOT (INDX)               
119200                                    MOD-IDANSTNR-MOT (INDX)               
119300                                    MOD-KVKOLLI-MOT  (INDX)               
119400     .                                                                    
119500                                                                          
119600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
119700                                                                          
119800*    --- ALLA INDATA-FÄLT                                                 
119900     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDANSTNR-UPD                       
120000                                   MOD-ADINLOMR-UPD                       
120100                                                                          
120200     MOVE +1 TO INDX                                                      
120300     PERFORM UNTIL INDX         >  MAX-INDX                               
120400       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD(INDX)                        
120500       ADD +1                   TO INDX                                   
120600     END-PERFORM                                                          
120700     .                                                                    
120800     EJECT                                                                
120900 MFS-FORM-ATTR SECTION.                                                   
121000                                                                          
121100*    --- ALLA INDATA-FÄLT                                                 
121200     MOVE +1 TO INDX                                                      
121300     PERFORM UNTIL INDX         >  MAX-INDX                               
121400       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMD-ATTR(INDX)                   
121500       ADD +1                   TO INDX                                   
121600     END-PERFORM                                                          
121700     .                                                                    
121800     EJECT                                                                
121900* --- IMS SEKTIONER ---                                                   
122000     SKIP3                                                                
122100 IMS-GET-MSG SECTION.                                                     
122200                                                                          
122300     MOVE '  QC' TO GODK-STATUSKODER                                      
122400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
122500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
122600     PERFORM IMS-STATUSKONTROLL                                           
122700     .                                                                    
122800     SKIP3                                                                
122900 IMS-INSERT-MSG SECTION.                                                  
123000                                                                          
123110     IF MSGI-IDLAND-SPR = 'GB'                                            
123200       MOVE 'N' TO MFS-KDHUVOMR                                           
123300     END-IF                                                               
123400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
123500     MOVE SPACE TO GODK-STATUSKODER                                       
123600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
123700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123800     PERFORM IMS-STATUSKONTROLL                                           
123900     .                                                                    
124000     EJECT                                                                
124100 IMS-CHANGE-ALTMSG SECTION.                                               
124200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
124300     MOVE '  A1A4' TO GODK-STATUSKODER                                    
124400     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
124500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
124600     PERFORM IMS-STATUSKONTROLL                                           
124700     .                                                                    
124800     SKIP3                                                                
124900 IMS-INSERT-ALTMSG SECTION.                                               
125000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
125100     MOVE SPACE TO GODK-STATUSKODER                                       
125200     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
125300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
125400     PERFORM IMS-STATUSKONTROLL                                           
125500     .                                                                    
125600     EJECT                                                                
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
