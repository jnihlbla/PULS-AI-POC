000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013510.                                                
000400*AUTHOR.         BERT ANDERSSON > RAHUL JAIN.                             
000500*DATE-WRITTEN.   92/06/12 > MAR 2012.                                     
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN MPP SOM ANVÄNDS FÖR ATT TA BORT                 
001100*        KOLLIN, DVS. KOLLINUMMER FRÅN INLEVERANSREGISTRET.               
001200*        ARTIKLARNA FRÅN DET BORTTAGNA KOLLIT LÄGGS I EN                  
001300*        SK. "POOL" PÅ W6D121 DÄR ENDAST ETT RADNR FINNS.                 
001400*        NYCKEL PÅ 21-SEGMENTET (W6D121) BLIR IDRADNR = 1                 
001500*        EFTER UPPDATERING.                                               
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T135                                              
002100*        MID:         W60135I1                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W60135O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6013510'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300 77  WS-IDINLVGN                 PIC  X(3)     VALUE SPACE.               
004400 77  WS-ADINLOMR                 PIC  X(4)     VALUE SPACE.               
004500 77  SPAR-ADINLOMR               PIC  X(4)     VALUE SPACE.               
004600 77  WS-ADINLOMR-NXT             PIC  X(4)     VALUE SPACE.               
004700 77  WS-KDINLQ                   PIC  X        VALUE SPACE.               
004800 77  WS-BEFT-FOM                 PIC  X(2)     VALUE SPACE.               
004900 77  WS-BEFT-TOM                 PIC  X(2)     VALUE SPACE.               
005000 77  WS-FLINLFB                  PIC  X        VALUE SPACE.               
005100 77  SPAR-FLINLFB                PIC  X        VALUE SPACE.               
005200 77  WS-IDLEVNR-KOLLI            PIC  X(5).                               
005300 77  SPAR-IDLEVNR-KOLLI          PIC  X(5)     VALUE SPACE.               
005400 77  WS-IDOKOLLI                 PIC  X(9)     VALUE SPACE.               
005500 77  SPAR-IDOKOLLI               PIC  9(9)     VALUE ZERO COMP-3.         
005600 77  WS-IDLOPNRM                 PIC  X(9)     VALUE SPACE.               
005700 77  WS-ART-IDLOPNRM             PIC S9(9)     VALUE ZERO COMP-3.         
005800 77  WS-ART-KVAVIS-PRIO          PIC S9(6)     VALUE ZERO COMP-3.         
005900 77  WS-ART-KDINLPRIO            PIC  S9(3)    VALUE ZERO COMP-3.         
006000 77  WS-RAD-KDINLPRIO            PIC  S9(3)    VALUE ZERO COMP-3.         
006100 77  WS-KDINLPRIO                PIC  S9(3)    VALUE ZERO COMP-3.         
006200 77  WS-FLPRIO                   PIC  X(1)     VALUE SPACE.               
006300 77  ANTAL-KOLLI-PA-BILD         PIC  9(2)     VALUE ZERO COMP-3.         
006400 77  ANTAL-KOLLI-I-6191MID       PIC  9(2)     VALUE ZERO COMP-3.         
006500 77  WS-KVINLART-NEW             PIC  S9(7)    VALUE ZERO COMP-3.         
006600 77  WS-KVINLART-OLD             PIC  S9(7)    VALUE ZERO COMP-3.         
006700 77  ACC-KVINLART                PIC  S9(7)    VALUE ZERO COMP-3.         
006800 77  WS-PRARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
006900 77  WS-IDRADNR                  PIC  9(5)     VALUE ZERO.                
007000 77  WS-ADINLOMR-OLD             PIC  X(4)     VALUE SPACE.               
007100 77  WS-ADINLOMR-NEW             PIC  X(4)     VALUE SPACE.               
007200 77  WS-ADINLOMR-NXT-OLD         PIC  X(4)     VALUE SPACE.               
007300 77  WS-ADINLOMR-NXT-NEW         PIC  X(4)     VALUE SPACE.               
007400 77  WS-KDINLSTA-OLD             PIC  X(3)     VALUE SPACE.               
007500 77  WS-KDINLSTA-NEW             PIC  X(3)     VALUE SPACE.               
007600 77  WS-FLGODK                   PIC  X        VALUE SPACE.               
007700 77  WS-FLDIVKLI                 PIC  X        VALUE SPACE.               
007800*                                                                         
007900 77  LNG-P-TO-P-PREFIX           PIC S9(4) VALUE +17  COMP SYNC.          
008000 77  MAX-I-6191-TRANS            PIC S9(4) VALUE +24  COMP SYNC.          
008100 77  MAX-KOLUMN                  PIC S9(2) VALUE +02  COMP SYNC.          
008200 77  KOL-INDX                    PIC S9(2) VALUE ZERO COMP SYNC.          
008300 77  RAD-INDX                    PIC S9(2) VALUE ZERO COMP SYNC.          
008400 77  6191-INDX                   PIC  9(2) VALUE ZERO COMP SYNC.          
008500*                                                                         
008600 01  WS-IDLEVNR-INTERVALL.                                                
008700   03  WS-IDLEVNR-KOLLI-RAD      PIC  X(5).                               
008800       88  GODK-IDLEVNR          VALUE '99500' THRU '99599'.              
008900                                                                          
009000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009100     88  INDATA-OK                           VALUE 'J'.                   
009200     88  INDATA-FEL                          VALUE 'N'.                   
009300                                                                          
009400 77  REQU-INDATA-SW              PIC X       VALUE 'N'.                   
009500     88  REQU-INDATA-FINNS                   VALUE 'J'.                   
009600                                                                          
009700 77  SPEC-FELMED-SW              PIC X       VALUE 'N'.                   
009800     88  SPECIELLT-FELMED-FINNS              VALUE 'J'.                   
009900                                                                          
010000 77  PARTI-BEHANDLAD-SW          PIC X       VALUE 'N'.                   
010100     88  PARTI-BEHANDLAD                     VALUE 'J'.                   
010200                                                                          
010300 77  KOLLI-RAD-BORTTAGEN-SW      PIC X       VALUE 'N'.                   
010400     88  KOLLI-RAD-BORTTAGEN                 VALUE 'J'.                   
010500                                                                          
010600 77  UPPDATE-DONE-SW             PIC X       VALUE 'N'.                   
010700     88  UPPDATE-DONE                        VALUE 'J'.                   
010800                                                                          
010900 77  NYTT-PARTI-SW               PIC X       VALUE 'N'.                   
011000     88  NYTT-PARTI                          VALUE 'J'.                   
011100                                                                          
011200 77  PMRK-SW                     PIC X       VALUE 'N'.                   
011300     88  PMRK                                VALUE 'J'.                   
011400                                                                          
011500 77  STATUS-SW                   PIC X       VALUE 'N'.                   
011600     88  STATUS-GODK                         VALUE 'J'.                   
011700                                                                          
011800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011900     88  NYCKLAR-OK                          VALUE 'J'.                   
012000     88  NYCKLAR-FEL                         VALUE 'N'.                   
012100                                                                          
012200 77  FOERSTA-6191TRANS-SW        PIC X       VALUE 'J'.                   
012300     88  FOERSTA-6191TRANS                   VALUE 'J'.                   
012400                                                                          
012500 01  ALL-SPACE.                                                           
012600     03 FILLER                   PIC X(80)   VALUE SPACE.                 
012700 01  ALL-PLUS.                                                            
012800     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
012900     EJECT                                                                
013000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013100 01  GENERELLA-SUBPROGRAM.                                                
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013400     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
013500     EJECT                                                                
013600 01  MESSAGE-CODES.                                                       
013700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
013800     03  ERR-OTILLATEN-UPDAT     PIC X(3)    VALUE '007'.                 
013900     03  ERR-FINNS-EJ-PA-REG     PIC X(3)    VALUE '025'.                 
014000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
014100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
014200     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
014300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
014400     03  DOKUMENT-NEEDED         PIC X(3)    VALUE '365'.                 
014500     03  ERR-DIVERSE             PIC X(3)    VALUE '366'.                 
014600     03  INF-NO-UPDATE           PIC X(3)    VALUE '004'.                 
014700     EJECT                                                                
014800*01  -COPY W611PMRK                                                       
014900     EJECT                                                                
015000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015300     SKIP3                                                                
015400*01  -COPY WMFSAREA                                                       
015500     EJECT                                                                
015600*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
015700     EJECT                                                                
015800     SKIP2                                                                
015900 01      FILLER                  PIC X(24)   VALUE                        
016000                                 'MOD6191-MID-W6I19101'.                  
016100     SKIP2                                                                
016200     -COPY W6I19101 -PRE MOD6191-                                         
016300     EJECT                                                                
016400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016500*                                                                         
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800     SKIP3                                                                
016900 01  NYCKLAR-TILL-DLI.                                                    
017000*                                                                         
017100     03  W-W6D101KY-X.                                                    
017200         05  W-IDDC-101KY        PIC  X(2)    VALUE SPACE.                
017300         05  W-IDLEVNR           PIC  X(5)    VALUE SPACE.                
017400         05  W-IDFS              PIC  X(8)    VALUE SPACE.                
017500         05  W-TIAVIDAT          PIC S9(7)    VALUE ZERO COMP-3.          
017600*                                                                         
017700     03  W-W6D1C1KY-MIN-X.                                                
017800         05  W-IDLEVNR-KOLLI-MIN  PIC  X(5)   VALUE SPACE.                
017900         05  W-IDOKOLLI-MIN       PIC 9(9)    VALUE ZERO.                 
018000         05  W-IDRADNR-INL-MIN    PIC S9(5)   VALUE ZERO COMP-3.          
018100         05  W-IDDC-C1KY-MIN      PIC  X(2)   VALUE LOW-VALUE.            
018200         05  W-IDLEVNR-MIN        PIC  X(5)   VALUE SPACE.                
018300         05  W-IDFS-MIN           PIC X(8)    VALUE LOW-VALUE.            
018400         05  W-TIAVIDAT-MIN       PIC S9(7)   VALUE ZERO COMP-3.          
018500         05  W-IDRADNR-MIN        PIC S9(5)   VALUE ZERO COMP-3.          
018600*                                                                         
018700     03  W-W6D1C1KY-MAX-X.                                                
018800         05  W-IDLEVNR-KOLLI-MAX  PIC  X(5)   VALUE SPACE.                
018900         05  W-IDOKOLLI-MAX       PIC 9(9)    VALUE 999999999.            
019000         05  W-IDRADNR-INL-MAX    PIC S9(5) COMP-3 VALUE 99999.           
019100         05  W-IDDC-C1KY-MAX      PIC  X(2)    VALUE HIGH-VALUE.          
019200         05  W-IDLEVNR-MAX        PIC  X(5)   VALUE SPACE.                
019300         05  W-IDFS-MAX           PIC X(8)    VALUE '99999999'.           
019400         05  W-TIAVIDAT-MAX       PIC S9(7) COMP-3 VALUE 9999999.         
019500         05  W-IDRADNR-MAX        PIC S9(5)   COMP-3 VALUE 99999.         
019600*                                                                         
019700     03  W-IDRADNR-INL-X.                                                 
019800         05  W-IDRADNR-INL        PIC S9(5)   VALUE ZERO COMP-3.          
019900*                                                                         
020000     03  W-IDRADNR-X.                                                     
020100         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
020200                                                                          
020300     03  W-IDLEVNRK-X.                                                    
020400         05  W-IDLEVNRK          PIC  X(5)   VALUE SPACE.                 
020500                                                                          
020600     03  W-IDOKOLLI-X.                                                    
020700         05  W-IDOKOLLINR        PIC  9(9).                               
020800                                                                          
020900     03  W-IDDC-X.                                                        
021000         05  W-IDDC              PIC  X(2).                               
021100                                                                          
021200     03  W-IDDC-B6-X.                                                     
021300         05 W-IDDC-B6            PIC X(2).                                
021400     SKIP2                                                                
021500*    --- STATUS-KOD FRÅN IMS                                              
021600 01  STATUS-WS                   PIC XX.                                  
021700     88  SEGMENT-FINNS                       VALUE '  '.                  
021800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022000     88  END-OF-DATA                         VALUE 'GB'.                  
022100     SKIP2                                                                
022200 01  GODK-STATUSKODER.                                                    
022300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022400     SKIP3                                                                
022500 01  SSA1                        PIC X(128).                              
022600 01  SSA2                        PIC X(128).                              
022700     EJECT                                                                
022800*    --- IMS FUNKTIONSKODER                                               
022900*01  -COPY W0003                                                          
023000     EJECT                                                                
023100*    ---  DLI INPUT-OUTPUT AREA                                           
023200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023300     SKIP3                                                                
023400 01  DLI-IO-AREA.                                                         
023500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
023600     SKIP3                                                                
023700     03  W6D1C1 REDEFINES IO-AREA.                                        
023800*        05  -COPY W6D1C1                                                 
023900     SKIP3                                                                
024000     03  W6INLA01 REDEFINES IO-AREA.                                      
024100*        05  -COPY W6D101                                                 
024200     SKIP3                                                                
024300     03  W6INLA11 REDEFINES IO-AREA.                                      
024400*        05  -COPY W6D111                                                 
024500     SKIP3                                                                
024600     03  W6INLA21 REDEFINES IO-AREA.                                      
024700*        05  -COPY W6D121                                                 
024800                                                                          
024900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
025000 01   DLI-IO-AREA-B601.                                                   
025100*     03  -COPY WDB601                                                    
025200                                                                          
025300     EJECT                                                                
025400 LINKAGE SECTION.                                                         
025500                                                                          
025600 01  REQU-AREA.                                                           
025700*    03 -COPY WZ01REQU                                                    
025800*    03 -COPY W60135I1                                                    
025900     EJECT                                                                
026000 01  RESP-AREA.                                                           
026100*    03 -COPY WZ01RESP                                                    
026200*    03 -COPY W60135O1                                                    
026300     EJECT                                                                
026400 01  MAX-KVRADER                 PIC S9(4) COMP.                          
026500*                                                                         
026600*01  -COPY W0009   -PRE 6191-                                             
026700     SKIP2                                                                
026800*01  -COPY W0008   -PRE INLA-                                             
026900     05  FILLER                  PIC X.                                   
027000     SKIP2                                                                
027100*01  -COPY W0008   -PRE INLA-CSEQ-                                        
027200     05  FILLER                  PIC X.                                   
027300     SKIP2                                                                
027400*01  -COPY W0008   -PRE INLD-                                             
027500     05  FILLER                  PIC X.                                   
027600     SKIP2                                                                
027700*01  -COPY W0008   -PRE WDB6-                                             
027800     05  FILLER                  PIC X.                                   
027900     SKIP2                                                                
028000**  PCB'ER FÖR SUBPGM                                                     
028100 01  PMRK-INLB-PCB               PIC X.                                   
028200                                                                          
028300 01  PMRK-INLC-PCB               PIC X.                                   
028400                                                                          
028500 01  PMRK-PLAA-PCB               PIC X.                                   
028600     EJECT                                                                
028700*                                                                         
028800 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
028900                           6191-PCB INLA-PCB                              
029000                           INLA-CSEQ-PCB INLD-PCB WDB6-PCB                
029100                           PMRK-INLB-PCB PMRK-INLC-PCB                    
029200                           PMRK-PLAA-PCB.                                 
029300 MAIN SECTION.                                                            
029400     PERFORM A-INIT                                                       
029500     PERFORM B-KOLLA-NYCKLAR                                              
029600     IF NYCKLAR-OK                                                        
029700       IF REQU-UPDATE                                                     
029800         PERFORM G-KOLLA-INPUT                                            
029900         IF INDATA-OK                                                     
030000           PERFORM H-UPPDATERA                                            
030100         END-IF                                                           
030200       ELSE                                                               
030300         PERFORM E-SAMMA-SIDA                                             
030400       END-IF                                                             
030500     END-IF                                                               
030600                                                                          
030700     GOBACK                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 A-INIT SECTION.                                                          
031100                                                                          
031200     MOVE ALL '+'                   TO RESP-W60135O1                      
031300                                                                          
031400     MOVE 001                       TO RESP-IDMSGVER                      
031500     MOVE SPACE                     TO RESP-IDMSG-ERROR                   
031600                                       RESP-IDMSG-INFO                    
031700                                       RESP-IDELMT-ERROR                  
031800     .                                                                    
031900     EJECT                                                                
032000 B-KOLLA-NYCKLAR SECTION.                                                 
032100                                                                          
032200     MOVE JA                        TO NYCKLAR-SW                         
032300                                       FOERSTA-6191TRANS-SW               
032400     MOVE NEJ                       TO WS-FLDIVKLI                        
032500                                       PARTI-BEHANDLAD-SW                 
032600                                       NYTT-PARTI-SW                      
032700                                       KOLLI-RAD-BORTTAGEN-SW             
032800                                                                          
032900     MOVE ALL-SPACE                 TO RESP-IDDC                          
033000     IF REQU-IDDC-KEY = ALL '+'                                           
033100        MOVE NEJ TO NYCKLAR-SW                                            
033200     ELSE                                                                 
033300        MOVE REQU-IDDC-KEY TO W-IDDC-B6                                   
033400     END-IF                                                               
033500     PERFORM IMS-GU-WDB601                                                
033600                                                                          
033700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
033800        MOVE NEJ TO NYCKLAR-SW                                            
033900     ELSE                                                                 
034000        MOVE DCS-IDDC       TO W-IDDC                                     
034100                               RESP-IDDC                                  
034200     END-IF                                                               
034300                                                                          
034400     IF NYCKLAR-FEL                                                       
034500       MOVE ERR-WRONG-KEY           TO RESP-IDMSG-ERROR                   
034600       PERFORM MFS-RENSA-FAELT-IN                                         
034700       PERFORM MFS-RENSA-FAELT-UT                                         
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 E-SAMMA-SIDA SECTION.                                                    
035200                                                                          
035300     PERFORM S01-REQU-INFAELT-KONTROLL                                    
035400                                                                          
035500     IF REQU-INDATA-FINNS                                                 
035600       MOVE INF-PRESS-PF11          TO RESP-IDMSG-INFO                    
035700       PERFORM EA-REQU-INDATA-TILL-RESP                                   
035800     ELSE                                                                 
035900       PERFORM MFS-RENSA-FAELT-IN                                         
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 EA-REQU-INDATA-TILL-RESP SECTION.                                        
036400                                                                          
036500* * * * * FÖR VARJE MID-FÄLT                                              
036600* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
036700* * * * * FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR               
036800* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
036900                                                                          
037000     MOVE +1                        TO RAD-INDX  KOL-INDX                 
037100     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
037200       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
037300         IF REQU-IDLEVNR-KOLLI (RAD-INDX, KOL-INDX) = ALL '+'             
037400           MOVE ALL-SPACE           TO                                    
037500                RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                    
037600         ELSE                                                             
037700           MOVE REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) TO                 
037800                RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                    
037900           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
038000                RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)               
038100         END-IF                                                           
038200         IF REQU-IDOKOLLI(RAD-INDX, KOL-INDX) = ALL '+'                   
038300           MOVE ALL-SPACE TO                                              
038400                RESP-IDOKOLLI(RAD-INDX, KOL-INDX)                         
038500         ELSE                                                             
038600           MOVE REQU-IDOKOLLI(RAD-INDX, KOL-INDX) TO                      
038700                RESP-IDOKOLLI(RAD-INDX, KOL-INDX)                         
038800           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
038900                RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)                    
039000         END-IF                                                           
039100         ADD +1                     TO KOL-INDX                           
039200       END-PERFORM                                                        
039300       ADD +1                       TO RAD-INDX                           
039400       MOVE +1                      TO KOL-INDX                           
039500     END-PERFORM                                                          
039600                                                                          
039700     IF REQU-FLGODK =  ALL '+'                                            
039800       MOVE ALL-SPACE               TO RESP-FLGODK                        
039900     ELSE                                                                 
040000       MOVE REQU-FLGODK             TO RESP-FLGODK                        
040100       MOVE MFS-ADD-LAES-IN-FAELT   TO RESP-FLGODK-ATTR                   
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 G-KOLLA-INPUT SECTION.                                                   
040600                                                                          
040700     MOVE JA  TO INDATA-SW                                                
040800     PERFORM S01-REQU-INFAELT-KONTROLL                                    
040900                                                                          
041000     IF REQU-INDATA-FINNS                                                 
041100                                                                          
041200       IF REQU-FLGODK NOT = '+' AND  NOT = SPACE                          
041300         IF REQU-FLGODK = 'N' OR 'J' OR 'Y'                               
041400           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLGODK-ATTR                  
041500           MOVE REQU-FLGODK          TO WS-FLGODK                         
041600         ELSE                                                             
041700           MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLGODK-ATTR                  
041800           MOVE NEJ TO INDATA-SW                                          
041900         END-IF                                                           
042000       END-IF                                                             
042100                                                                          
042200       IF ANTAL-KOLLI-PA-BILD = 1                                         
042300         PERFORM S05-HAMTA-INDATA                                         
042400       END-IF                                                             
042500                                                                          
042600       MOVE +1                      TO RAD-INDX  KOL-INDX                 
042700       PERFORM UNTIL RAD-INDX > MAX-KVRADER                               
042800         PERFORM UNTIL KOL-INDX > MAX-KOLUMN                              
042900           IF REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) = ALL '+'            
043000             IF REQU-IDOKOLLI(RAD-INDX, KOL-INDX) = ALL '+'               
043100               MOVE MFS-ALFA-FAELT-RAETT TO                               
043200                    RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)           
043300               MOVE MFS-NUM-FAELT-RAETT TO                                
043400                    RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)                
043500             ELSE                                                         
043600               MOVE MFS-NUM-FAELT-RAETT TO                                
043700                    RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)                
043800               MOVE MFS-ALFA-FAELT-FEL TO                                 
043900                    RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)           
044000               MOVE NEJ               TO INDATA-SW                        
044100             END-IF                                                       
044200           ELSE                                                           
044300             IF REQU-IDOKOLLI(RAD-INDX, KOL-INDX) = ALL '+'               
044400               MOVE MFS-NUM-FAELT-FEL TO                                  
044500                    RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)                
044600               MOVE MFS-ALFA-FAELT-RAETT TO                               
044700                    RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)           
044800               MOVE NEJ               TO INDATA-SW                        
044900             ELSE                                                         
045000               PERFORM GA-INDATA-KONTROLL                                 
045100             END-IF                                                       
045200           END-IF                                                         
045300           ADD +1                   TO KOL-INDX                           
045400         END-PERFORM                                                      
045500         ADD +1                     TO RAD-INDX                           
045600         MOVE +1                    TO KOL-INDX                           
045700       END-PERFORM                                                        
045800                                                                          
045900       IF WS-FLDIVKLI = JA                                                
046000         IF ANTAL-KOLLI-PA-BILD > 1                                       
046100           MOVE ERR-DIVERSE         TO RESP-IDMSG-ERROR                   
046200           MOVE JA                  TO SPEC-FELMED-SW                     
046300         ELSE                                                             
046400           IF GODK-IDLEVNR OR                                             
046500              WS-FLGODK = JA OR YES                                       
046600             CONTINUE                                                     
046700           ELSE                                                           
046800             MOVE MFS-OEPPNA-ALFA-FAELT-HI TO RESP-FLGODK-ATTR            
046900             MOVE DOKUMENT-NEEDED   TO RESP-IDMSG-ERROR                   
047000             MOVE JA                TO SPEC-FELMED-SW                     
047100           END-IF                                                         
047200         END-IF                                                           
047300       END-IF                                                             
047400                                                                          
047500       IF INDATA-FEL                                                      
047600         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
047700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
047800       END-IF                                                             
047900       IF SPECIELLT-FELMED-FINNS                                          
048000         PERFORM MFS-ROER-EJ-FAELT-IN                                     
048100         MOVE NEJ TO INDATA-SW                                            
048200       END-IF                                                             
048300     ELSE                                                                 
048400       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
048500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
048600       MOVE NEJ TO INDATA-SW                                              
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000 GA-INDATA-KONTROLL   SECTION.                                            
049100                                                                          
049200     IF REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) NOT = ALL '+'              
049300       IF REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) NOT = SPACE              
049400         MOVE MFS-ALFA-FAELT-RAETT TO                                     
049500              RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)                 
049600         MOVE REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) TO                   
049700              SPAR-IDLEVNR-KOLLI W-IDLEVNR-KOLLI-MIN                      
049800                                 W-IDLEVNR-KOLLI-MAX                      
049900                                 W-IDLEVNRK                               
050000       ELSE                                                               
050100         MOVE MFS-ALFA-FAELT-FEL TO                                       
050200              RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)                 
050300         MOVE NEJ TO INDATA-SW                                            
050400       END-IF                                                             
050500     END-IF                                                               
050600                                                                          
050700     IF REQU-IDOKOLLI(RAD-INDX, KOL-INDX) NOT = ALL '+'                   
050800       INSPECT REQU-IDOKOLLI(RAD-INDX, KOL-INDX) REPLACING                
050900       LEADING SPACE BY ZERO                                              
051000       IF REQU-IDOKOLLI(RAD-INDX, KOL-INDX) NUMERIC AND                   
051100          REQU-IDOKOLLI(RAD-INDX, KOL-INDX) > ZERO                        
051200         MOVE MFS-NUM-FAELT-RAETT TO                                      
051300              RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)                      
051400         MOVE REQU-IDOKOLLI(RAD-INDX, KOL-INDX) TO                        
051500              SPAR-IDOKOLLI W-IDOKOLLI-MIN W-IDOKOLLI-MAX                 
051600              W-IDOKOLLINR                                                
051700       ELSE                                                               
051800         MOVE MFS-NUM-FAELT-FEL TO                                        
051900              RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)                      
052000         MOVE NEJ TO INDATA-SW                                            
052100       END-IF                                                             
052200     END-IF                                                               
052300                                                                          
052400     IF INDATA-OK                                                         
052500       PERFORM GAA-KONTROLL-AV-KOLLI                                      
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 GAA-KONTROLL-AV-KOLLI SECTION.                                           
053000                                                                          
053100     PERFORM IMS-GU-INLD01                                                
053200     IF SEGMENT-FINNS                                                     
053300       MOVE SEQC-IDLEVNR            TO W-IDLEVNR                          
053400       MOVE SEQC-IDFS               TO W-IDFS                             
053500       MOVE W-IDDC                  TO W-IDDC-101KY                       
053600       MOVE SEQC-TIAVIDAT           TO W-TIAVIDAT                         
053700       MOVE SEQC-IDRADNR-INL        TO W-IDRADNR-INL                      
053800       MOVE SEQC-IDRADNR            TO W-IDRADNR                          
053900                                                                          
054000       PERFORM IMS-GU-INLA11                                              
054100       IF ART-IDDC        = W-IDDC                                        
054200         PERFORM GAAA-SPARA-IDLOPNRM                                      
054300         IF WS-ART-IDLOPNRM = ART-IDLOPNRM                                
054400                                                                          
054500           PERFORM IMS-GNP-INLA21                                         
054600           IF SEGMENT-FINNS                                               
054700                                                                          
054800             IF RAD-FLDIVKLI = JA                                         
054900               MOVE JA              TO  WS-FLDIVKLI                       
055000             ELSE                                                         
055100                                                                          
055200               IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE                  
055300                 PERFORM IMS-GNP-INLA21-F-OKVAL                           
055400                 IF RAD-IDRADNR = +1 AND RAD-IDILIST NOT = ZERO           
055500                   MOVE ERR-OTILLATEN-UPDAT TO RESP-IDMSG-ERROR           
055600                   MOVE MFS-ALFA-FAELT-FEL TO                             
055700                     RESP-IDLEVNR-KOLLI-ATTR (RAD-INDX, KOL-INDX)         
055800                   MOVE MFS-NUM-FAELT-FEL TO                              
055900                     RESP-IDOKOLLI-ATTR (RAD-INDX, KOL-INDX)              
056000                   MOVE JA                TO SPEC-FELMED-SW               
056100                 END-IF                                                   
056200               ELSE                                                       
056300                 PERFORM IMS-GU-INLA11                                    
056400                 PERFORM IMS-GNP-INLA21-F-OKVAL                           
056500                 IF RAD-IDRADNR = +1 AND RAD-IDILIST NOT = ZERO           
056600                   MOVE ERR-OTILLATEN-UPDAT TO RESP-IDMSG-ERROR           
056700                   MOVE MFS-ALFA-FAELT-FEL TO                             
056800                     RESP-IDLEVNR-KOLLI-ATTR (RAD-INDX, KOL-INDX)         
056900                   MOVE MFS-NUM-FAELT-FEL TO                              
057000                        RESP-IDOKOLLI-ATTR (RAD-INDX, KOL-INDX)           
057100                   MOVE JA                TO SPEC-FELMED-SW               
057200                 ELSE                                                     
057300                   PERFORM IMS-GNP-INLA21-F-KVAL                          
057400                   PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA            
057500                     IF SEGMENT-FINNS                                     
057600                       IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE          
057700                         MOVE JA TO STATUS-SW                             
057800                       END-IF                                             
057900                       PERFORM IMS-GNP-INLA21-KVAL                        
058000                     END-IF                                               
058100                   END-PERFORM                                            
058200                   IF NOT STATUS-GODK                                     
058300                     MOVE ERR-OTILLATEN-UPDAT TO RESP-IDMSG-ERROR         
058400                     MOVE JA      TO SPEC-FELMED-SW                       
058500                   END-IF                                                 
058600                 END-IF                                                   
058700               END-IF                                                     
058800             END-IF                                                       
058900           ELSE                                                           
059000             MOVE ERR-FINNS-EJ-PA-REG TO RESP-IDMSG-ERROR                 
059100             MOVE 'IDLEVNR' TO RESP-IDELMT-ERROR                          
059200             MOVE MFS-ALFA-FAELT-FEL TO                                   
059300                  RESP-IDLEVNR-KOLLI-ATTR (RAD-INDX, KOL-INDX)            
059400             MOVE MFS-NUM-FAELT-FEL TO                                    
059500                  RESP-IDOKOLLI-ATTR (RAD-INDX, KOL-INDX)                 
059600             MOVE JA                TO SPEC-FELMED-SW                     
059700           END-IF                                                         
059800         ELSE                                                             
059900             MOVE ERR-OTILLATEN-UPDAT TO RESP-IDMSG-ERROR                 
060000             MOVE MFS-ALFA-FAELT-FEL TO                                   
060100                  RESP-IDLEVNR-KOLLI-ATTR (RAD-INDX, KOL-INDX)            
060200             MOVE MFS-NUM-FAELT-FEL TO                                    
060300                  RESP-IDOKOLLI-ATTR (RAD-INDX, KOL-INDX)                 
060400             MOVE JA                TO SPEC-FELMED-SW                     
060500         END-IF                                                           
060600       ELSE                                                               
060700         MOVE ERR-FINNS-EJ-PA-REG   TO RESP-IDMSG-ERROR                   
060800         MOVE MFS-ALFA-FAELT-FEL TO                                       
060900              RESP-IDLEVNR-KOLLI-ATTR (RAD-INDX, KOL-INDX)                
061000             MOVE 'IDLEVNR' TO RESP-IDELMT-ERROR                          
061100         MOVE MFS-NUM-FAELT-FEL TO                                        
061200              RESP-IDOKOLLI-ATTR (RAD-INDX, KOL-INDX)                     
061300         MOVE JA                    TO SPEC-FELMED-SW                     
061400       END-IF                                                             
061500     ELSE                                                                 
061600       MOVE ERR-FINNS-EJ-PA-REG     TO RESP-IDMSG-ERROR                   
061700       MOVE 'IDLEVNR' TO RESP-IDELMT-ERROR                                
061800       MOVE MFS-ALFA-FAELT-FEL TO                                         
061900            RESP-IDLEVNR-KOLLI-ATTR (RAD-INDX, KOL-INDX)                  
062000       MOVE MFS-NUM-FAELT-FEL TO                                          
062100            RESP-IDOKOLLI-ATTR (RAD-INDX, KOL-INDX)                       
062200       MOVE JA                      TO SPEC-FELMED-SW                     
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 GAAA-SPARA-IDLOPNRM SECTION.                                             
062700                                                                          
062800     IF WS-ART-IDLOPNRM = ZERO                                            
062900       MOVE ART-IDLOPNRM            TO WS-ART-IDLOPNRM                    
063000     END-IF                                                               
063100     .                                                                    
063200     EJECT                                                                
063300 H-UPPDATERA SECTION.                                                     
063400     MOVE ZERO                      TO ACC-KVINLART                       
063500                                                                          
063600     IF WS-FLDIVKLI = NEJ                                                 
063700       PERFORM HA-BEHANDLA-KOLLI-I-ETT-PARTI                              
063800       MOVE INF-UPDATE-DONE         TO RESP-IDMSG-INFO                    
063900       PERFORM HC-RENSA-FAELT                                             
064000     ELSE                                                                 
064100       PERFORM HB-BEHANDLA-DIVERSE-KOLLI                                  
064200       IF UPPDATE-DONE                                                    
064300         MOVE INF-UPDATE-DONE       TO RESP-IDMSG-INFO                    
064400         PERFORM HC-RENSA-FAELT                                           
064500       ELSE                                                               
064600         MOVE INF-NO-UPDATE         TO RESP-IDMSG-INFO                    
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000                                                                          
065100     IF ANTAL-KOLLI-I-6191MID < 24                                        
065200       PERFORM S03A-STARTA-W60191                                         
065300       MOVE ZERO                    TO 6191-INDX                          
065400     END-IF                                                               
065500     .                                                                    
065600     EJECT                                                                
065700 HA-BEHANDLA-KOLLI-I-ETT-PARTI  SECTION.                                  
065800                                                                          
065900     MOVE +1                        TO RAD-INDX  KOL-INDX                 
066000     MOVE ZERO                      TO W-IDRADNR                          
066100                                       ANTAL-KOLLI-I-6191MID              
066200     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
066300       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
066400         IF REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) = ALL '+' AND          
066500            REQU-IDOKOLLI(RAD-INDX, KOL-INDX) = ALL '+'                   
066600           CONTINUE                                                       
066700         ELSE                                                             
066800           PERFORM HAA-BEHANDLA-KOLLI                                     
066900         END-IF                                                           
067000         ADD +1                     TO KOL-INDX                           
067100       END-PERFORM                                                        
067200       ADD +1                       TO RAD-INDX                           
067300       MOVE +1                      TO KOL-INDX                           
067400     END-PERFORM                                                          
067500                                                                          
067600     MOVE +1                        TO W-IDRADNR                          
067700     PERFORM IMS-GU-INLA11                                                
067800     MOVE ART-KVAVIS-PRIO           TO WS-ART-KVAVIS-PRIO                 
067900     PERFORM IMS-GHNP-INLA21                                              
068000     MOVE ACC-KVINLART              TO WS-KVINLART-NEW                    
068100     IF SEGMENT-FINNS                                                     
068200       IF RAD-KDINLSTA = 'SAK'                                            
068300         MOVE RAD-KDINLSTA          TO WS-KDINLSTA-OLD                    
068400         MOVE SPACE                 TO RAD-KDINLSTA                       
068500       ELSE                                                               
068600         MOVE RAD-KDINLSTA          TO WS-KDINLSTA-OLD                    
068700       END-IF                                                             
068800       IF PMRK                                                            
068900         MOVE WS-FLPRIO           TO RAD-FLPRIO                           
069000         MOVE WS-KDINLPRIO        TO RAD-KDINLPRIO                        
069100       END-IF                                                             
069200       MOVE RAD-KVINLART          TO WS-KVINLART-OLD                      
069300       MOVE RAD-ADINLOMR          TO WS-ADINLOMR-OLD                      
069400       MOVE SPAR-FLINLFB          TO RAD-FLINLFB                          
069500       IF RAD-ADINLOMR = SPACE                                            
069600         MOVE SPAR-ADINLOMR TO RAD-ADINLOMR                               
069700       END-IF                                                             
069800       MOVE RAD-ADINLOMR          TO WS-ADINLOMR-NEW                      
069900       MOVE RAD-ADINLOMR-NXT      TO WS-ADINLOMR-NXT-OLD                  
070000                                     WS-ADINLOMR-NXT-NEW                  
070100       MOVE SPACE                 TO WS-KDINLSTA-NEW                      
070200       MOVE JA                      TO RAD-FLINLFP                        
070300       ADD  ACC-KVINLART            TO RAD-KVINLART                       
070400       MOVE RAD-KVINLART            TO WS-KVINLART-NEW                    
070500       PERFORM IMS-REPL-INLA                                              
070600       PERFORM S03-FLYTTA-TILL-6191REQU                                   
070700     ELSE                                                                 
070800       PERFORM S02-SKAPA-RADNR1                                           
070900       PERFORM IMS-ISRT-INLA21                                            
071000       MOVE SPACE                   TO WS-ADINLOMR-OLD                    
071100                                       WS-ADINLOMR-NXT-OLD                
071200                                       WS-KDINLSTA-OLD                    
071300       MOVE ZERO                    TO WS-KVINLART-OLD                    
071400                                                                          
071500       MOVE RAD-ADINLOMR            TO WS-ADINLOMR-NEW                    
071600       MOVE RAD-ADINLOMR-NXT        TO WS-ADINLOMR-NXT-NEW                
071700       MOVE RAD-KDINLSTA            TO WS-KDINLSTA-NEW                    
071800       MOVE RAD-KVINLART            TO WS-KVINLART-NEW                    
071900       PERFORM S03-FLYTTA-TILL-6191REQU                                   
072000     END-IF                                                               
072100     IF PMRK                                                              
072200       PERFORM S04-CALL-W611PMRK                                          
072300     END-IF                                                               
072400     MOVE WS-ART-IDLOPNRM             TO RESP-IDLOPNRM                    
072500     .                                                                    
072600     EJECT                                                                
072700 HAA-BEHANDLA-KOLLI SECTION.                                              
072800                                                                          
072900     MOVE REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) TO                       
073000          SPAR-IDLEVNR-KOLLI                                              
073100     MOVE REQU-IDOKOLLI(RAD-INDX, KOL-INDX) TO SPAR-IDOKOLLI              
073200                                                                          
073300     PERFORM IMS-GU-INLA11                                                
073400                                                                          
073500     MOVE ART-IDLOPNRM              TO WS-ART-IDLOPNRM                    
073600     MOVE ART-PRARTSTD              TO WS-PRARTSTD                        
073700     PERFORM IMS-GHNP-INLA21-GT                                           
073800     PERFORM UNTIL SEGMENT-SAKNAS                                         
073900       IF SEGMENT-FINNS                                                   
074000         IF SPAR-IDLEVNR-KOLLI = RAD-IDLEVNR-KOLLI AND                    
074100            SPAR-IDOKOLLI    = RAD-IDOKOLLI                               
074200           PERFORM HAAA-BEHANDLA-KOLLI                                    
074300         END-IF                                                           
074400         PERFORM IMS-GHNP-INLA21-GT                                       
074500       END-IF                                                             
074600     END-PERFORM                                                          
074700     .                                                                    
074800     EJECT                                                                
074900 HAAA-BEHANDLA-KOLLI SECTION.                                             
075000                                                                          
075100     IF RAD-KDINLSTA = 'SAK' OR 'FPK' OR SPACE                            
075200       MOVE RAD-IDRADNR               TO WS-IDRADNR                       
075300       IF RAD-ADINLOMR NOT = SPACE                                        
075400         MOVE RAD-ADINLOMR TO SPAR-ADINLOMR                               
075500       END-IF                                                             
075600       MOVE RAD-ADINLOMR              TO WS-ADINLOMR-OLD                  
075700       MOVE RAD-ADINLOMR-NXT          TO WS-ADINLOMR-NXT-OLD              
075800       MOVE RAD-KDINLSTA              TO WS-KDINLSTA-OLD                  
075900       MOVE SPACE                     TO WS-KDINLSTA-NEW                  
076000                                         WS-ADINLOMR-NEW                  
076100                                         WS-ADINLOMR-NXT-NEW              
076200       MOVE ZERO                      TO WS-KVINLART-NEW                  
076300       MOVE RAD-KVINLART              TO WS-KVINLART-OLD                  
076400       MOVE RAD-FLINLFB               TO SPAR-FLINLFB                     
076500       ADD  RAD-KVINLART              TO ACC-KVINLART                     
076600       IF RAD-KDINLSTA = 'SAK'                                            
076700         MOVE SPACE                   TO RAD-KDINLSTA                     
076800         PERFORM IMS-REPL-INLA                                            
076900         MOVE RAD-KDINLSTA            TO WS-KDINLSTA-OLD                  
077000         PERFORM S03-FLYTTA-TILL-6191REQU                                 
077100       END-IF                                                             
077200       IF RAD-FLPRIO = JA                                                 
077300         MOVE RAD-FLPRIO    TO WS-FLPRIO                                  
077400         MOVE RAD-KDINLPRIO TO WS-KDINLPRIO                               
077500         MOVE JA            TO PMRK-SW                                    
077600       END-IF                                                             
077700       PERFORM IMS-DLET-INLA                                              
077800       PERFORM S03-FLYTTA-TILL-6191REQU                                   
077900     END-IF                                                               
078000     .                                                                    
078100     EJECT                                                                
078200 HB-BEHANDLA-DIVERSE-KOLLI   SECTION.                                     
078300                                                                          
078400     PERFORM IMS-GU-INLA11                                                
078500     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                          
078600       MOVE ART-PRARTSTD            TO WS-PRARTSTD                        
078700       MOVE ART-KVAVIS-PRIO         TO WS-ART-KVAVIS-PRIO                 
078800       MOVE ART-IDLOPNRM            TO WS-ART-IDLOPNRM                    
078900                                                                          
079000       PERFORM HBC-LAES-INLA21                                            
079100                                                                          
079200       MOVE NEJ                  TO KOLLI-RAD-BORTTAGEN-SW                
079300                                                                          
079400       PERFORM UNTIL PARTI-BEHANDLAD                                      
079500         IF SEGMENT-FINNS                                                 
079600           IF SPAR-IDLEVNR-KOLLI = RAD-IDLEVNR-KOLLI AND                  
079700              SPAR-IDOKOLLI    = RAD-IDOKOLLI                             
079800             IF RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'                    
079900               PERFORM HBA-BEHANDLA-KOLLI                                 
080000             END-IF                                                       
080100           END-IF                                                         
080200           PERFORM IMS-GHNP-INLA21-GT                                     
080300         ELSE                                                             
080400           IF KOLLI-RAD-BORTTAGEN                                         
080500             PERFORM HBB-BEHANDLA-RAD1                                    
080600             IF PMRK                                                      
080700               PERFORM S04-CALL-W611PMRK                                  
080800             END-IF                                                       
080900           END-IF                                                         
081000           MOVE JA                  TO PARTI-BEHANDLAD-SW                 
081100         END-IF                                                           
081200       END-PERFORM                                                        
081300                                                                          
081400       MOVE NEJ TO NYTT-PARTI-SW                                          
081500       PERFORM IMS-GN-INLD01                                              
081600       IF SEGMENT-FINNS                                                   
081700         PERFORM HBD-KONTROLL-C-INDX-NYCKLAR                              
081800       END-IF                                                             
081900     END-PERFORM                                                          
082000     MOVE ZERO                      TO RESP-IDLOPNRM                      
082100     .                                                                    
082200     EJECT                                                                
082300 HBA-BEHANDLA-KOLLI SECTION.                                              
082400                                                                          
082500                                                                          
082600     IF RAD-KDINLSTA = 'FPK' OR SPACE                                     
082700       IF NYTT-PARTI                                                      
082800         MOVE NEJ TO NYTT-PARTI-SW                                        
082900         MOVE RAD-KVINLART            TO WS-KVINLART-OLD                  
083000         MOVE ZERO                    TO ACC-KVINLART                     
083100       END-IF                                                             
083200       MOVE RAD-IDRADNR               TO WS-IDRADNR                       
083300                                         W-IDRADNR                        
083400       ADD  RAD-KVINLART              TO ACC-KVINLART                     
083500       MOVE RAD-FLINLFB               TO SPAR-FLINLFB                     
083600       IF RAD-FLPRIO = 'J'                                                
083700         MOVE RAD-FLPRIO    TO WS-FLPRIO                                  
083800         MOVE RAD-KDINLPRIO TO WS-KDINLPRIO                               
083900         MOVE JA            TO PMRK-SW                                    
084000       END-IF                                                             
084100       IF RAD-ADINLOMR NOT = SPACE                                        
084200         MOVE RAD-ADINLOMR TO SPAR-ADINLOMR                               
084300       END-IF                                                             
084400       MOVE RAD-ADINLOMR            TO WS-ADINLOMR-OLD                    
084500       MOVE RAD-ADINLOMR-NXT        TO WS-ADINLOMR-NXT-OLD                
084600       MOVE RAD-KDINLSTA            TO WS-KDINLSTA-OLD                    
084700       MOVE RAD-KVINLART            TO WS-KVINLART-OLD                    
084800       IF RAD-IDRADNR = +1                                                
084900         MOVE RAD-ADINLOMR            TO WS-ADINLOMR-NEW                  
085000         MOVE RAD-ADINLOMR-NXT        TO WS-ADINLOMR-NXT-NEW              
085100         MOVE RAD-KDINLSTA            TO WS-KDINLSTA-NEW                  
085200         MOVE RAD-KVINLART            TO WS-KVINLART-NEW                  
085300       ELSE                                                               
085400         MOVE SPACE                   TO WS-ADINLOMR-NEW                  
085500                                         WS-ADINLOMR-NXT-NEW              
085600                                         WS-KDINLSTA-NEW                  
085700         MOVE ZERO                    TO WS-KVINLART-NEW                  
085800       END-IF                                                             
085900       PERFORM S03-FLYTTA-TILL-6191REQU                                   
086000       IF RAD-IDRADNR NOT = +1                                            
086100         PERFORM IMS-DLET-INLA                                            
086200       END-IF                                                             
086300       MOVE JA                      TO KOLLI-RAD-BORTTAGEN-SW             
086400                                       UPPDATE-DONE-SW                    
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800 HBB-BEHANDLA-RAD1 SECTION.                                               
086900                                                                          
087000     MOVE +1                        TO W-IDRADNR                          
087100                                       WS-IDRADNR                         
087200     PERFORM IMS-GHNP-INLA21-FIRST                                        
087300     MOVE WS-KDINLSTA-OLD           TO WS-KDINLSTA-NEW                    
087400     IF SEGMENT-FINNS                                                     
087500       MOVE RAD-ADINLOMR            TO WS-ADINLOMR-OLD                    
087600       IF RAD-ADINLOMR = SPACE                                            
087700         MOVE SPAR-ADINLOMR         TO RAD-ADINLOMR                       
087800       END-IF                                                             
087900       IF PMRK                                                            
088000         MOVE WS-FLPRIO             TO RAD-FLPRIO                         
088100         MOVE WS-KDINLPRIO          TO RAD-KDINLPRIO                      
088200       END-IF                                                             
088300       MOVE SPAR-FLINLFB            TO RAD-FLINLFB                        
088400       MOVE RAD-ADINLOMR            TO WS-ADINLOMR-NEW                    
088500       MOVE RAD-ADINLOMR-NXT        TO WS-ADINLOMR-NXT-OLD                
088600                                       WS-ADINLOMR-NXT-NEW                
088700       MOVE RAD-KDINLSTA            TO WS-KDINLSTA-OLD                    
088800                                       WS-KDINLSTA-NEW                    
088900       MOVE RAD-KVINLART            TO WS-KVINLART-OLD                    
089000       IF SPAR-IDLEVNR-KOLLI = RAD-IDLEVNR-KOLLI AND                      
089100          SPAR-IDOKOLLI    = RAD-IDOKOLLI                                 
089200         MOVE RAD-KVINLART          TO WS-KVINLART-NEW                    
089300         MOVE SPACE                 TO RAD-IDLEVNR-KOLLI                  
089400         MOVE ZERO                  TO RAD-IDOKOLLI                       
089500         MOVE NEJ                   TO RAD-FLDIVKLI                       
089600         PERFORM IMS-REPL-INLA                                            
089700         PERFORM S03-FLYTTA-TILL-6191REQU                                 
089800       ELSE                                                               
089900         ADD  ACC-KVINLART          TO RAD-KVINLART                       
090000         PERFORM IMS-REPL-INLA                                            
090100         MOVE RAD-KVINLART          TO WS-KVINLART-NEW                    
090200         PERFORM S03-FLYTTA-TILL-6191REQU                                 
090300       END-IF                                                             
090400     ELSE                                                                 
090500       PERFORM S02-SKAPA-RADNR1                                           
090600       PERFORM IMS-ISRT-INLA21                                            
090700       MOVE SPACE                   TO WS-KDINLSTA-OLD                    
090800                                       WS-KDINLSTA-NEW                    
090900                                       WS-ADINLOMR-OLD                    
091000                                       WS-ADINLOMR-NEW                    
091100                                       WS-ADINLOMR-NXT-OLD                
091200                                       WS-ADINLOMR-NXT-NEW                
091300       MOVE ZERO                    TO WS-KVINLART-OLD                    
091400       MOVE ACC-KVINLART            TO WS-KVINLART-NEW                    
091500       PERFORM S03-FLYTTA-TILL-6191REQU                                   
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 HBC-LAES-INLA21  SECTION.                                                
092000                                                                          
092100     IF PARTI-BEHANDLAD                                                   
092200       MOVE ZERO                    TO W-IDRADNR                          
092300       MOVE NEJ                     TO PARTI-BEHANDLAD-SW                 
092400                                       PMRK-SW                            
092500       MOVE JA                      TO NYTT-PARTI-SW                      
092600       PERFORM IMS-GHNP-INLA21-GT                                         
092700     ELSE                                                                 
092800       PERFORM IMS-GHNP-INLA21                                            
092900     END-IF                                                               
093000     .                                                                    
093100     EJECT                                                                
093200 HBD-KONTROLL-C-INDX-NYCKLAR  SECTION.                                    
093300                                                                          
093400     PERFORM UNTIL SEGMENT-SAKNAS OR NYTT-PARTI                           
093500       IF SEQC-IDLEVNR  = W-IDLEVNR  AND                                  
093600          SEQC-IDFS     = W-IDFS     AND                                  
093700          SEQC-TIAVIDAT = W-TIAVIDAT AND                                  
093800          SEQC-IDRADNR-INL  = W-IDRADNR-INL                               
093900          PERFORM IMS-GN-INLD01                                           
094000       ELSE                                                               
094100         MOVE SEQC-IDLEVNR            TO W-IDLEVNR                        
094200         MOVE SEQC-IDFS               TO W-IDFS                           
094300         MOVE SEQC-TIAVIDAT           TO W-TIAVIDAT                       
094400         MOVE SEQC-IDRADNR-INL        TO W-IDRADNR-INL                    
094500         MOVE SEQC-IDRADNR            TO W-IDRADNR                        
094600         PERFORM IMS-GU-INLA11                                            
094700         MOVE JA TO NYTT-PARTI-SW                                         
094800       END-IF                                                             
094900     END-PERFORM                                                          
095000     .                                                                    
095100     EJECT                                                                
095200 HC-RENSA-FAELT SECTION.                                                  
095300                                                                          
095400*    --- ALLA INDATA-FÄLT                                                 
095500     MOVE ALL-SPACE                 TO RESP-FLGODK                        
095600                                                                          
095700     MOVE +1                        TO RAD-INDX  KOL-INDX                 
095800     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
095900       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
096000         MOVE ALL-SPACE             TO                                    
096100              RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                      
096200              RESP-IDOKOLLI(RAD-INDX, KOL-INDX)                           
096300         ADD +1                     TO KOL-INDX                           
096400       END-PERFORM                                                        
096500       ADD +1                       TO RAD-INDX                           
096600       MOVE +1                      TO KOL-INDX                           
096700     END-PERFORM                                                          
096800     .                                                                    
096900     EJECT                                                                
097000 S01-REQU-INFAELT-KONTROLL SECTION.                                       
097100                                                                          
097200     MOVE +1                        TO RAD-INDX  KOL-INDX                 
097300     MOVE ZERO                      TO ANTAL-KOLLI-PA-BILD                
097400     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
097500       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
097600         IF REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) = ALL '+' AND          
097700            REQU-IDOKOLLI(RAD-INDX, KOL-INDX) = ALL '+'                   
097800           CONTINUE                                                       
097900         ELSE                                                             
098000           ADD +1                   TO ANTAL-KOLLI-PA-BILD                
098100           MOVE JA                  TO REQU-INDATA-SW                     
098200         END-IF                                                           
098300         ADD +1                     TO KOL-INDX                           
098400       END-PERFORM                                                        
098500       ADD +1                       TO RAD-INDX                           
098600       MOVE +1                      TO KOL-INDX                           
098700     END-PERFORM                                                          
098800                                                                          
098910     IF REQU-FLGODK NOT = ALL '+' AND SPACE                               
099000       MOVE JA                      TO REQU-INDATA-SW                     
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400 S02-SKAPA-RADNR1 SECTION.                                                
099500                                                                          
099600     MOVE +1                        TO RAD-IDRADNR                        
099700     MOVE ACC-KVINLART              TO RAD-KVINLART                       
099800     IF WS-ART-KVAVIS-PRIO > ZERO                                         
099900       MOVE +31                     TO RAD-KDINLPRIO                      
100000     ELSE                                                                 
100100       MOVE +39                     TO RAD-KDINLPRIO                      
100200     END-IF                                                               
100300     MOVE ZERO                      TO RAD-IDOKOLLI                       
100400                                       RAD-IDILIST                        
100500                                       RAD-TIUPPDAT                       
100600                                       RAD-IDANSTNR                       
100700                                       RAD-IDILIRAD                       
100800                                       RAD-IDINLVGN                       
100900     MOVE SPACE                     TO RAD-KDINLSTA                       
101000                                       RAD-IDLEVNR-KOLLI                  
101100     MOVE SPAR-ADINLOMR             TO RAD-ADINLOMR                       
101200     MOVE SPACE                     TO RAD-ADINLOMR-NXT                   
101300     MOVE NEJ                       TO RAD-FLPRIO                         
101400                                       RAD-FLKVAANT                       
101500                                       RAD-FLSVSLS                        
101600     MOVE SPAR-FLINLFB              TO RAD-FLINLFB                        
101700     MOVE JA                        TO RAD-FLINLFP                        
101800     MOVE NEJ                       TO RAD-FLSATS                         
101900                                       RAD-FLDIVKLI                       
102000     IF PMRK                                                              
102100       MOVE WS-FLPRIO              TO RAD-FLPRIO                          
102200       MOVE WS-KDINLPRIO           TO RAD-KDINLPRIO                       
102300     END-IF                                                               
102400     .                                                                    
102500     EJECT                                                                
102600 S03-FLYTTA-TILL-6191REQU SECTION.                                        
102700                                                                          
102800     ADD +1                         TO 6191-INDX                          
102900                                                                          
103000     MOVE WS-ART-IDLOPNRM  TO MOD6191-MID-IDLOPNRM (6191-INDX)            
103100     MOVE RAD-IDRADNR      TO MOD6191-MID-IDRADNR  (6191-INDX)            
103200     MOVE RAD-KDINLPRIO    TO MOD6191-MID-KDINLPRIO(6191-INDX)            
103300     MOVE WS-PRARTSTD      TO MOD6191-MID-PRARTSTD (6191-INDX)            
103400     MOVE +0               TO MOD6191-MID-KVKOLLI  (6191-INDX)            
103500     MOVE 'N'              TO MOD6191-MID-FLINLI   (6191-INDX)            
103600     MOVE WS-ADINLOMR-OLD  TO MOD6191-MID-ADINLOMR-OLD                    
103700                                           (6191-INDX)                    
103800     MOVE WS-ADINLOMR-NXT-OLD TO MOD6191-MID-ADINLOMR-NXT-OLD             
103900                                               (6191-INDX)                
104000     MOVE WS-KDINLSTA-OLD  TO MOD6191-MID-KDINLSTA-OLD(6191-INDX)         
104100     MOVE WS-KVINLART-OLD  TO MOD6191-MID-KVINLART-OLD(6191-INDX)         
104200     MOVE WS-ADINLOMR-NEW  TO MOD6191-MID-ADINLOMR-NEW(6191-INDX)         
104300     MOVE WS-ADINLOMR-NXT-NEW TO MOD6191-MID-ADINLOMR-NXT-NEW             
104400                                               (6191-INDX)                
104500     MOVE WS-KDINLSTA-NEW  TO MOD6191-MID-KDINLSTA-NEW(6191-INDX)         
104600     MOVE WS-KVINLART-NEW  TO MOD6191-MID-KVINLART-NEW(6191-INDX)         
104700     IF 6191-INDX             = MAX-I-6191-TRANS                          
104800       PERFORM S03A-STARTA-W60191                                         
104900       MOVE ZERO           TO 6191-INDX                                   
105000     END-IF                                                               
105100     .                                                                    
105200     EJECT                                                                
105300 S03A-STARTA-W60191      SECTION.                                         
105400                                                                          
105500     MOVE 6191-INDX                 TO MOD6191-MID-KVPOST                 
105600     MOVE 'W6013500'                TO MOD6191-MID-IDPGM                  
105700     MOVE DCS-IDDC                  TO MOD6191-MID-IDDC                   
105800     COMPUTE P-TO-P-MSG-KVLL = LNG-P-TO-P-PREFIX +                        
105900                           17 + (MOD6191-MID-KVPOST * 64)                 
106000     MOVE 'W6T191X '                TO P-TO-P-MSG-KDTRANS                 
106100     MOVE '6135'                    TO P-TO-P-MSG-IDTRANS                 
106200     IF REQU-IDSPRAK = 'SV'                                               
106300        MOVE '1'                    TO P-TO-P-MSG-KDMFSFOR                
106400     ELSE                                                                 
106500        MOVE '2'                    TO P-TO-P-MSG-KDMFSFOR                
106600     END-IF                                                               
106700                                                                          
106800     MOVE MOD6191-MID-W6I19101      TO P-TO-P-MSG-INDATA                  
106900                                                                          
107000     IF FOERSTA-6191TRANS                                                 
107100       PERFORM IMS-ISRT-ALT-MSG-6191                                      
107200       MOVE NEJ                     TO FOERSTA-6191TRANS-SW               
107300     ELSE                                                                 
107400       PERFORM IMS-PURG-ALT-MSG-6191                                      
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 S04-CALL-W611PMRK      SECTION.                                          
107900                                                                          
108000     MOVE SPACE                     TO PMRK-IDLEVNR                       
108100     MOVE ZERO                      TO PMRK-IDOKOLLI                      
108200     MOVE WS-ART-IDLOPNRM           TO PMRK-IDLOPNRM                      
108300     MOVE +1                        TO PMRK-IDRADNR                       
108400     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
108500                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
108600     .                                                                    
108700     EJECT                                                                
108800 S05-HAMTA-INDATA      SECTION.                                           
108900                                                                          
109000     MOVE +1                        TO RAD-INDX  KOL-INDX                 
109100     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
109200       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
109300         IF REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) = ALL '+' AND          
109400            REQU-IDOKOLLI(RAD-INDX, KOL-INDX) = ALL '+'                   
109500           CONTINUE                                                       
109600         ELSE                                                             
109700           MOVE REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) TO                 
109800                SPAR-IDLEVNR-KOLLI                                        
109900                WS-IDLEVNR-KOLLI-RAD                                      
110000           MOVE REQU-IDOKOLLI(RAD-INDX, KOL-INDX) TO SPAR-IDOKOLLI        
110100         END-IF                                                           
110200         ADD +1                     TO KOL-INDX                           
110300       END-PERFORM                                                        
110400       ADD +1                       TO RAD-INDX                           
110500       MOVE +1                      TO KOL-INDX                           
110600     END-PERFORM                                                          
110700     .                                                                    
110800     EJECT                                                                
110900 MFS-RENSA-FAELT-UT SECTION.                                              
111000                                                                          
111100*    --- ALLA UTDATA-FÄLT                                                 
111200     MOVE ALL-SPACE                 TO RESP-FLGODK                        
111300                                                                          
111400     MOVE +1                        TO RAD-INDX  KOL-INDX                 
111500     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
111600       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
111700         MOVE ALL-SPACE             TO                                    
111800              RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                      
111900              RESP-IDOKOLLI(RAD-INDX, KOL-INDX)                           
112000         ADD +1                     TO KOL-INDX                           
112100       END-PERFORM                                                        
112200       ADD +1                       TO RAD-INDX                           
112300       MOVE +1                      TO KOL-INDX                           
112400     END-PERFORM                                                          
112500     .                                                                    
112600     EJECT                                                                
112700 MFS-RENSA-FAELT-IN SECTION.                                              
112800                                                                          
112900*    --- ALLA INDATA-FÄLT                                                 
113000     MOVE ALL-SPACE                 TO RESP-FLGODK                        
113100                                                                          
113200     MOVE +1                        TO RAD-INDX  KOL-INDX                 
113300     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
113400       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
113500         MOVE ALL-SPACE             TO                                    
113600              RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                      
113700              RESP-IDOKOLLI(RAD-INDX, KOL-INDX)                           
113800         ADD +1                     TO KOL-INDX                           
113900       END-PERFORM                                                        
114000       ADD +1                       TO RAD-INDX                           
114100       MOVE +1                      TO KOL-INDX                           
114200     END-PERFORM                                                          
114300     .                                                                    
114400     EJECT                                                                
114500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
114600                                                                          
114700*    --- ALLA INDATA-FÄLT                                                 
114800     MOVE ALL-PLUS                  TO RESP-FLGODK                        
114900                                                                          
115000     MOVE +1                        TO RAD-INDX  KOL-INDX                 
115100     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
115200       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
115300         MOVE ALL-PLUS              TO                                    
115400              RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                      
115500              RESP-IDOKOLLI(RAD-INDX, KOL-INDX)                           
115600         ADD +1                     TO KOL-INDX                           
115700       END-PERFORM                                                        
115800       ADD +1                       TO RAD-INDX                           
115900       MOVE +1                      TO KOL-INDX                           
116000     END-PERFORM                                                          
116100     .                                                                    
116200     EJECT                                                                
116300* --- IMS SEKTIONER ---                                                   
116400     SKIP3                                                                
116500 IMS-ISRT-ALT-MSG-6191  SECTION.                                          
116600     MOVE SPACE TO GODK-STATUSKODER                                       
116700     CALL  CBLTDLI  USING ISRT 6191-PCB P-TO-P-MSG-IO-AREA-SNUF           
116800     MOVE 6191-STATUS-CODE TO STATUS-WS                                   
116900     PERFORM IMS-STATUSKONTROLL                                           
117000     .                                                                    
117100     SKIP3                                                                
117200 IMS-PURG-ALT-MSG-6191  SECTION.                                          
117300     MOVE SPACE TO GODK-STATUSKODER                                       
117400     CALL  CBLTDLI  USING PURG 6191-PCB P-TO-P-MSG-IO-AREA-SNUF           
117500     MOVE 6191-STATUS-CODE TO STATUS-WS                                   
117600     PERFORM IMS-STATUSKONTROLL                                           
117700     .                                                                    
117800     EJECT                                                                
117900 IMS-GU-INLD01 SECTION.                                                   
118000     STRING 'W6INLD01(W6D1C1KY=>' W-W6D1C1KY-MIN-X                        
118100                    '&W6D1C1KY=<' W-W6D1C1KY-MAX-X                        
118200                    '&IDDC    = ' W-IDDC ')'                              
118300             DELIMITED BY SIZE INTO SSA1                                  
118400     MOVE '  GE' TO GODK-STATUSKODER                                      
118500     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
118600     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
118700     PERFORM IMS-STATUSKONTROLL                                           
118800     .                                                                    
118900     SKIP3                                                                
119000 IMS-GN-INLD01 SECTION.                                                   
119100     STRING 'W6INLD01(W6D1C1KY=>' W-W6D1C1KY-MIN-X                        
119200                    '&W6D1C1KY=<' W-W6D1C1KY-MAX-X                        
119300                    '&IDDC    = ' W-IDDC ')'                              
119400             DELIMITED BY SIZE INTO SSA1                                  
119500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
119600     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA SSA1                      
119700     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
119800     PERFORM IMS-STATUSKONTROLL                                           
119900     .                                                                    
120000     SKIP3                                                                
120100 IMS-GU-INLA11 SECTION.                                                   
120200     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
120300          DELIMITED BY SIZE INTO SSA1                                     
120400     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
120500          DELIMITED BY SIZE INTO SSA2                                     
120600     MOVE '  GE' TO GODK-STATUSKODER                                      
120700     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1 SSA2                 
120800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
120900     PERFORM IMS-STATUSKONTROLL                                           
121000     .                                                                    
121100     SKIP3                                                                
121200 IMS-GNP-INLA21 SECTION.                                                  
121300     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
121400          DELIMITED BY SIZE INTO SSA1                                     
121500     MOVE '  GE' TO GODK-STATUSKODER                                      
121600     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1                     
121700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
121800     PERFORM IMS-STATUSKONTROLL                                           
121900     .                                                                    
122000     SKIP3                                                                
122100 IMS-GHNP-INLA21 SECTION.                                                 
122200     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
122300          DELIMITED BY SIZE INTO SSA1                                     
122400     MOVE '  GE' TO GODK-STATUSKODER                                      
122500     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA SSA1                    
122600     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     .                                                                    
122900     SKIP3                                                                
123000 IMS-GHNP-INLA21-GT SECTION.                                              
123100     STRING 'W6INLA21(IDRADNR  >' W-IDRADNR-X ')'                         
123200          DELIMITED BY SIZE INTO SSA1                                     
123300     MOVE '  GE' TO GODK-STATUSKODER                                      
123400     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA SSA1                    
123500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
123600     PERFORM IMS-STATUSKONTROLL                                           
123700     .                                                                    
123800     SKIP3                                                                
123900 IMS-GHNP-INLA21-FIRST SECTION.                                           
124000     STRING 'W6INLA21*F(IDRADNR  =' W-IDRADNR-X ')'                       
124100             DELIMITED BY SIZE INTO SSA1                                  
124200     MOVE '  GE' TO GODK-STATUSKODER                                      
124300     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA SSA1                    
124400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
124500     PERFORM IMS-STATUSKONTROLL                                           
124600     .                                                                    
124700     SKIP2                                                                
124800 IMS-GNP-INLA21-F-OKVAL SECTION.                                          
124900     MOVE  'W6INLA21*F'     TO SSA1                                       
125000     MOVE '  GE' TO GODK-STATUSKODER                                      
125100     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1                     
125200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
125300     PERFORM IMS-STATUSKONTROLL                                           
125400     .                                                                    
125500     EJECT                                                                
125600 IMS-GNP-INLA21-KVAL SECTION.                                             
125700                                                                          
125800     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-X                            
125900                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
126000          DELIMITED BY SIZE INTO SSA1                                     
126100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
126200     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1                     
126300     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
126400     PERFORM IMS-STATUSKONTROLL                                           
126500     .                                                                    
126600     SKIP3                                                                
126700 IMS-GNP-INLA21-F-KVAL SECTION.                                           
126800                                                                          
126900     STRING 'W6INLA21*F(IDLEVNRK =' W-IDLEVNRK-X                          
127000                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
127100          DELIMITED BY SIZE INTO SSA1                                     
127200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
127300     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1                     
127400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
127500     PERFORM IMS-STATUSKONTROLL                                           
127600     .                                                                    
127700     SKIP3                                                                
127800 IMS-ISRT-INLA21 SECTION.                                                 
127900                                                                          
128000     MOVE 'W6INLA21 ' TO SSA1                                             
128100     MOVE '  II' TO GODK-STATUSKODER                                      
128200     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1                    
128300     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
128400     PERFORM IMS-STATUSKONTROLL                                           
128500     .                                                                    
128600     SKIP3                                                                
128700 IMS-REPL-INLA SECTION.                                                   
128800                                                                          
128900     MOVE '  ' TO GODK-STATUSKODER                                        
129000     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA                         
129100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400     SKIP3                                                                
129500 IMS-DLET-INLA SECTION.                                                   
129600                                                                          
129700     MOVE '  ' TO GODK-STATUSKODER                                        
129800     CALL CBLTDLI USING DLET INLA-PCB DLI-IO-AREA                         
129900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200     EJECT                                                                
130300 IMS-GU-WDB601    SECTION.                                                
130400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
130500          DELIMITED BY SIZE INTO SSA1                                     
130600     MOVE '  GE' TO GODK-STATUSKODER                                      
130700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
130800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
130900     PERFORM IMS-STATUSKONTROLL                                           
131000     IF SEGMENT-SAKNAS                                                    
131100         MOVE SPACE TO DCS-KDDC                                           
131200     END-IF                                                               
131300     .                                                                    
131400 IMS-STATUSKONTROLL SECTION.                                              
131500                                                                          
131600     SET STATUS-IX TO 1                                                   
131700     SEARCH GODK-STATUS                                                   
131800       AT END                                                             
131900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
132000         DELIMITED BY SIZE INTO FELTEXT                                   
132100         CALL FELLOG                                                      
132200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
132300         CONTINUE                                                         
132400     END-SEARCH                                                           
132500     .                                                                    
