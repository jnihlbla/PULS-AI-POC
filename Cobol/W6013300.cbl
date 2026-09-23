000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013300.                                                
000400*AUTHOR.         KATARINA KYMMER /ARCHANA BHAT.                           
000500*DATE-WRITTEN.   92/08/14 / JUNE 2012.                                    
000600                                                                          
000700**   REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN MPP SOM VISAR VILKA ÅTGÄRDER SOM                
001100*        FINNS PÅ ETT VISST PARTI,VAD SOM HITTILLS ÄR GJORT               
001200*        PÅ ETT PARTI,SAMT TILLÅTER REGISTRERING AV UTFÖRDA               
001300*        ÅTGÄRDER.BILDEN ANVÄNDS VID FÖRBEHANDLING AV IN-                 
001400*        LEVERANSER SOM HUVUDSAKLIG ARBETSBILD.                           
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001700*                   UPPDATERAR W6LOPA (W6G1)                              
001800*                   UPPDATERAR W6KVAE (W6H7)                              
001900*        W006KOM    UPPDATERAR WLKOMA (WDP8)                              
002000*                                                                         
002100*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
002200*                              W6UPFA (W6L1)                              
002300*                              WDK6                                       
002400*                              WDB6                                       
002500*        W611ADR    LÄSER      W6HANA (W6G1)                              
002600*                              W6PLAA (W6G1)                              
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W6T133                                              
003000*        MID:         W6I13301                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W6O13301                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)  VALUE 'W6013300'.             
004300 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004400                                                                          
004500 77  WS-IDLEVNR-KOLLI            PIC X(5).                                
004600 77  WS-IDOKOLLI                 PIC X(9).                                
004700 77  WS-IDLOPNRM-ALFA            PIC X(9).                                
004800 77  WS-IDLOPNRM-NUM             PIC 9(9).                                
004810 77  WS-IDINLVGN                 PIC 9(3).                                
004820 77  WS-ADINLOMR                 PIC X(4).                                
004830 77  WS-ADINLOMR-NXT             PIC X(4).                                
004840 77  WS-KDINLQ                   PIC X.                                   
004850 77  WS-BEFT-FOM                 PIC X(2).                                
004860 77  WS-BEFT-TOM                 PIC X(2).                                
004870 77  WS-FLINLFB                  PIC X.                                   
004900 77  INDX                        PIC S9(4)  VALUE +0 COMP SYNC.           
005000 77  MAX-INDX                    PIC S9(4)  VALUE +7 COMP SYNC.           
005100                                                                          
005200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  YES                         PIC X       VALUE 'Y'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '6133'.                
006200     88  GODK-MID                            VALUE '6131' '6132'          
006300                                                   '6133' '6134'          
006400                                                   '6135' '6136'          
006500                                                   '6137' '6138'          
006600                                                   '6139'.                
006700     88  HELP-MID                            VALUE '0551'.                
006800     EJECT                                                                
006900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500     03  W6013310                PIC X(8)    VALUE 'W6013310'.            
007600     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007700                                                                          
007800 77  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
007900     EJECT                                                                
008000*01 -COPY WMSGINIT                                                        
008100 01  W-IDMSG-ERROR               PIC X(3).                                
008200     88   WRONG-KEY                         VALUE '022'.                  
008300*                                                                         
008400 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
008500 01  REQU-AREA.                                                           
008600*    03 -COPY WZ01REQU                                                    
008700*    03 -COPY W60133I1                                                    
008800 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +7.                
008900     EJECT                                                                
009000*                                                                         
009100 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
009200 01  RESP-AREA.                                                           
009300*    03 -COPY WZ01RESP                                                    
009400*    03 -COPY W60133O1                                                    
009500*                                                                         
009600     SKIP3                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009800*01 -COPY WMEDAREA                                                        
009900     SKIP3                                                                
010000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010300     SKIP3                                                                
010400*01  MID -COPY W6I13301                                                   
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010700     SKIP3                                                                
010800*01  -COPY WMSGAREA                                                       
010900     EJECT                                                                
011000     03  MOD REDEFINES MSG-AREA.                                          
011100*      05  -COPY W6O13301                                                 
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011400     SKIP3                                                                
011500*01  -COPY WMFSAREA                                                       
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
011800     SKIP3                                                                
011900*01  -COPY WL01MCNV                                                       
012000     EJECT                                                                
012100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012200                                                                          
012300     SKIP2                                                                
012400*    --- STATUS-KOD FRÅN IMS                                              
012500 01  STATUS-WS                   PIC XX.                                  
012600     88  SEGMENT-FINNS                       VALUE '  '.                  
012700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013000     SKIP2                                                                
013100 01  GODK-STATUSKODER.                                                    
013200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(128).                              
013500 01  SSA2                        PIC X(64).                               
013600     EJECT                                                                
013700*    --- IMS FUNKTIONSKODER                                               
013800*01  -COPY W0003                                                          
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0009   -PRE MSG-                                              
014300     EJECT                                                                
014400*01  -COPY W0009   -PRE ALT1-                                             
014500     EJECT                                                                
014600*01  -COPY W0009   -PRE ALT2-                                             
014700     EJECT                                                                
014800*01  -COPY W0009   -PRE ALT3-                                             
014900     EJECT                                                                
015000*01  -COPY W0009   -PRE ALT4-                                             
015100     EJECT                                                                
015200*01  -COPY W0009   -PRE 6197-                                             
015300     EJECT                                                                
015400*01  -COPY W0009   -PRE DISP-                                             
015500     EJECT                                                                
015600*01  -COPY W0008  -PRE USEA-                                              
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008  -PRE PLAA-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200*01  -COPY W0008  -PRE LOPA-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE INLA-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE INLD-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE INLC-                                              
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400*01  -COPY W0008  -PRE ALT-INLC-                                          
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017700*01  -COPY W0008  -PRE INLB1-                                             
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000*01  -COPY W0008  -PRE INLG-                                              
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008  -PRE KVAE-                                              
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE KVABSEQ-                                           
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE WDK6-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE WDB6-                                              
019300     05  FILLER                  PIC X.                                   
019400*01  -COPY W0008  -PRE WDD3-                                              
019500     05  FILLER                  PIC X.                                   
019600     EJECT                                                                
019700**  PCB'ER FÖR SUBPGM                                                     
019800 01  ADR-INLA-PCB                PIC X.                                   
019900                                                                          
020000 01  ADR-INLC-PCB                PIC X.                                   
020100                                                                          
020200 01  ADR-PLAA-PCB                PIC X.                                   
020300                                                                          
020400 01  ADR-WDK6-PCB                PIC X.                                   
020500                                                                          
020600 01  ADR-STYR-HANA-PCB           PIC X.                                   
020700                                                                          
020800 01  ADR-STYR-PLAA-PCB           PIC X.                                   
020900                                                                          
021000 01  KOM-KOMA-PCB                PIC X.                                   
021100                                                                          
021200 01  PMRK-INLB-PCB               PIC X.                                   
021300                                                                          
021400 01  PMRK-INLC-PCB               PIC X.                                   
021500                                                                          
021600 01  PMRK-PLAA-PCB               PIC X.                                   
021700                                                                          
021800 01  STYR-HANA-PCB               PIC X.                                   
021900                                                                          
022000 01  STYR-PLAA-PCB               PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008  -PRE UPFA-                                              
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
022600                           ALT4-PCB 6197-PCB DISP-PCB USEA-PCB            
022700                           PLAA-PCB LOPA-PCB INLA-PCB                     
022800                           INLD-PCB                                       
022900                           INLC-PCB                                       
023000                           ALT-INLC-PCB                                   
023100                           INLB1-PCB                                      
023200                           INLG-PCB KVAE-PCB                              
023300                           KVABSEQ-PCB                                    
023400                           WDK6-PCB WDB6-PCB WDD3-PCB                     
023500                           ADR-INLA-PCB                                   
023600                           ADR-INLC-PCB                                   
023700                           ADR-PLAA-PCB                                   
023800                           ADR-WDK6-PCB                                   
023900                           ADR-STYR-HANA-PCB                              
024000                           ADR-STYR-PLAA-PCB                              
024100                           KOM-KOMA-PCB                                   
024200                           PMRK-INLB-PCB PMRK-INLC-PCB                    
024300                           PMRK-PLAA-PCB                                  
024400                           STYR-HANA-PCB                                  
024500                           STYR-PLAA-PCB                                  
024600                           UPFA-PCB.                                      
024700     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
024800                           ALT4-PCB 6197-PCB DISP-PCB USEA-PCB            
024900                           PLAA-PCB LOPA-PCB INLA-PCB                     
025000                           INLD-PCB                                       
025100                           INLC-PCB                                       
025200                           ALT-INLC-PCB                                   
025300                           INLB1-PCB                                      
025400                           INLG-PCB KVAE-PCB                              
025500                           KVABSEQ-PCB                                    
025600                           WDK6-PCB WDB6-PCB WDD3-PCB                     
025700                           ADR-INLA-PCB                                   
025800                           ADR-INLC-PCB                                   
025900                           ADR-PLAA-PCB                                   
026000                           ADR-WDK6-PCB                                   
026100                           ADR-STYR-HANA-PCB                              
026200                           ADR-STYR-PLAA-PCB                              
026300                           KOM-KOMA-PCB                                   
026400                           PMRK-INLB-PCB PMRK-INLC-PCB                    
026500                           PMRK-PLAA-PCB                                  
026600                           STYR-HANA-PCB                                  
026700                           STYR-PLAA-PCB                                  
026800                           UPFA-PCB.                                      
026900                                                                          
027000     PERFORM IMS-GET-MSG                                                  
027100     IF SEGMENT-FINNS                                                     
027200       PERFORM A-INIT                                                     
027300       PERFORM B-INIT-KEYS                                                
027400       IF MFS-UPDATE OR MFS-UPD-V                                         
027500         SET REQU-UPDATE TO TRUE                                          
027600         MOVE MID-IDLOPNRM-ENTER TO REQU-IDLOPNRM-START                   
027700       ELSE                                                               
027800         IF MFS-FIRST                                                     
027900          SET REQU-FIRST TO TRUE                                          
028000          PERFORM MFS-RENSA-FAELT-IN                                      
028100         ELSE                                                             
028200          IF MFS-NEXT                                                     
028300           SET REQU-NEXT TO TRUE                                          
028400           PERFORM D-NAESTA-SIDA                                          
028500          ELSE                                                            
028600           SET REQU-QUERY TO TRUE                                         
028700           PERFORM E-SAMMA-SIDA                                           
028800          END-IF                                                          
028900         END-IF                                                           
029000       END-IF                                                             
029100       PERFORM F-CALL-BIZ-LOGIC-W6013310                                  
029200                                                                          
029300       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O13301 + 4                      
029400       PERFORM IMS-INSERT-MSG                                             
029500     END-IF                                                               
029600                                                                          
029700     MOVE ZERO TO RETURN-CODE                                             
029800     GOBACK                                                               
029900     .                                                                    
030000     EJECT                                                                
030100 A-INIT SECTION.                                                          
030200                                                                          
030300     IF MSG-DUBBLA-TRANSKODER                                             
030400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13301                 
030500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
030600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
030700     ELSE                                                                 
030800       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I13301                 
030900       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
031000       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
031100     END-IF                                                               
031200                                                                          
031300     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
031400     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
031500     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
031600                                                                          
031700     MOVE LOW-VALUE                       TO MSG-AREA                     
031800     MOVE 'W6O133N1'                      TO MFS-IDMOD                    
031900     MOVE '6133'                          TO MOD-IDTRANS                  
032000     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
032100                                             MOD-TEMFSINF                 
032200                                                                          
032300     IF EGEN-MID OR HELP-MID                                              
032400       CONTINUE                                                           
032500     ELSE                                                                 
032600       MOVE SPACE                         TO MFS-KDTRTYP                  
032700       MOVE '7'                           TO MFS-IDPFK                    
032800     END-IF                                                               
032900                                                                          
033000     PERFORM AA-INIT-NYCKLAR                                              
033100     .                                                                    
033200     EJECT                                                                
033300 AA-INIT-NYCKLAR SECTION.                                                 
033400                                                                          
033500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
033600     MOVE '001'                  TO MSGI-KDCALL                           
033700     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
033800     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
033900     MOVE '6133'                 TO MSGI-IDTRANS                          
034000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034100     .                                                                    
034200     EJECT                                                                
034300 B-INIT-KEYS SECTION.                                                     
034400                                                                          
034500     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-IN                         
034600                             MOD-IDOKOLLI-IN                              
034700                             MOD-IDLOPNRM-IN                              
034800                             MOD-IDDC-IN                                  
034900                                                                          
035000     IF MID-IDLEVNR-KOLLI-IN = ALL '+'                                    
035100        MOVE MID-IDLEVNR-KOLLI-UT TO WS-IDLEVNR-KOLLI                     
035200                                     REQU-IDLEVNR-KOLLI-KEY               
035300     ELSE                                                                 
035400        MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                     
035500                                     REQU-IDLEVNR-KOLLI-KEY               
035600        MOVE '7'                  TO MFS-IDPFK                            
035700        MOVE SPACE                TO MFS-KDTRTYP                          
035800     END-IF                                                               
035900                                                                          
036000     IF MID-IDOKOLLI-IN = ALL '+'                                         
036100        MOVE MID-IDOKOLLI-UT TO WS-IDOKOLLI                               
036200        INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO               
036300     ELSE                                                                 
036400        MOVE MID-IDOKOLLI-IN TO WS-IDOKOLLI                               
036500        MOVE '7'             TO MFS-IDPFK                                 
036600        MOVE SPACE           TO MFS-KDTRTYP                               
036700     END-IF                                                               
036800     MOVE WS-IDOKOLLI        TO REQU-IDOKOLLI-KEY                         
036900                                                                          
037000     IF MID-IDLOPNRM-IN = ALL '+'                                         
037100        MOVE MID-IDLOPNRM-UT TO WS-IDLOPNRM-ALFA                          
037200        INSPECT WS-IDLOPNRM-ALFA REPLACING LEADING SPACE BY ZERO          
037300     ELSE                                                                 
037400        MOVE MID-IDLOPNRM-IN TO WS-IDLOPNRM-ALFA                          
037500        MOVE '7'             TO MFS-IDPFK                                 
037600        MOVE SPACE           TO MFS-KDTRTYP                               
037700     END-IF                                                               
037800     MOVE WS-IDLOPNRM-ALFA   TO REQU-IDLOPNRM-KEY                         
037900                                                                          
038000     IF (MID-IDLEVNR-KOLLI-IN  NOT = ALL '+' AND                          
038010         MID-IDLEVNR-KOLLI-IN  NOT = SPACES)                              
038100     OR (MID-IDOKOLLI-IN NOT = ALL '+' AND                                
038110         MID-IDOKOLLI-IN NOT = SPACES)                                    
038200        MOVE ZERO            TO WS-IDLOPNRM-ALFA                          
038300                                REQU-IDLOPNRM-KEY                         
038310        INSPECT WS-IDLOPNRM-ALFA REPLACING LEADING ZERO BY SPACE          
038400     ELSE                                                                 
038500        IF MID-IDLOPNRM-IN  NOT = ALL '+'                                 
038510        AND MID-IDLOPNRM-IN NOT = SPACES                                  
038600           MOVE SPACE        TO WS-IDLEVNR-KOLLI                          
038700                                REQU-IDLEVNR-KOLLI-KEY                    
038800           MOVE ZERO         TO WS-IDOKOLLI                               
038900                                REQU-IDOKOLLI-KEY                         
039000        END-IF                                                            
039100     END-IF                                                               
039200                                                                          
039300     IF MID-IDDC-IN = ALL '+'                                             
039400        MOVE MSGI-IDDC      TO REQU-IDDC-KEY                              
039500     ELSE                                                                 
039600        MOVE MID-IDDC-IN    TO REQU-IDDC-KEY                              
039700        MOVE '7'            TO MFS-IDPFK                                  
039800        MOVE SPACE          TO MFS-KDTRTYP                                
039900     END-IF                                                               
040000                                                                          
040100     IF GODK-MID                                                          
040110        PERFORM BA-KOLLA-OEVRIGA-NYCKLAR                                  
040200        MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                     
040300        MOVE WS-IDOKOLLI      TO MOD-IDOKOLLI-UT                          
040400        MOVE WS-IDLOPNRM-ALFA TO MOD-IDLOPNRM-UT                          
040500        MOVE REQU-IDDC-KEY    TO MOD-IDDC-UT                              
040600        INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE           
040700        INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE           
040800        INSPECT MOD-IDDC-UT     REPLACING LEADING ZERO BY SPACE           
040900     ELSE                                                                 
041000        MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-KOLLI-UT                     
041100                                 MOD-IDOKOLLI-UT                          
041200                                 MOD-IDLOPNRM-UT                          
041300                                 MOD-IDDC-UT                              
041400     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 BA-KOLLA-OEVRIGA-NYCKLAR SECTION.                                        
041801                                                                          
041802     IF MID-ADINLOMR-PRT         =  ALL '+'                               
041803         MOVE MFS-RENSA-FAELT    TO MOD-ADINLOMR-PRT                      
041805     ELSE                                                                 
041806         MOVE MID-ADINLOMR-PRT   TO MOD-ADINLOMR-PRT                      
041807     END-IF                                                               
041808                                                                          
041809     MOVE MFS-RENSA-FAELT        TO  MOD-IDINLVGN-IN                      
041810     IF MID-IDINLVGN-IN          = ALL '+'                                
041811       MOVE MID-IDINLVGN-UT      TO WS-IDINLVGN                           
041812     ELSE                                                                 
041813       MOVE MID-IDINLVGN-IN      TO WS-IDINLVGN                           
041814     END-IF                                                               
041815     MOVE WS-IDINLVGN            TO MOD-IDINLVGN-UT                       
041816                                                                          
041817     MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-IN                      
041818     IF MID-ADINLOMR-IN          = ALL '+'                                
041819       MOVE MID-ADINLOMR-UT      TO WS-ADINLOMR                           
041820     ELSE                                                                 
041821       MOVE MID-ADINLOMR-IN      TO WS-ADINLOMR                           
041822     END-IF                                                               
041823     MOVE WS-ADINLOMR            TO MOD-ADINLOMR-UT                       
041824                                                                          
041825     MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-NXT-IN                  
041826     IF MID-ADINLOMR-NXT-IN      = ALL '+'                                
041827       MOVE MID-ADINLOMR-NXT-UT  TO WS-ADINLOMR-NXT                       
041828     ELSE                                                                 
041829       MOVE MID-ADINLOMR-NXT-IN  TO WS-ADINLOMR-NXT                       
041830     END-IF                                                               
041831     MOVE WS-ADINLOMR-NXT        TO MOD-ADINLOMR-NXT-UT                   
041832                                                                          
041833     MOVE MFS-RENSA-FAELT        TO  MOD-KDINLQ-IN                        
041834     IF MID-KDINLQ-IN            = ALL '+'                                
041835       MOVE MID-KDINLQ-UT        TO WS-KDINLQ                             
041836     ELSE                                                                 
041837       MOVE MID-KDINLQ-IN        TO WS-KDINLQ                             
041838     END-IF                                                               
041839     MOVE WS-KDINLQ              TO MOD-KDINLQ-UT                         
041840                                                                          
041841     MOVE MFS-RENSA-FAELT        TO  MOD-BEFT-FOM-IN                      
041842     IF MID-BEFT-FOM-IN          = ALL '+'                                
041843       MOVE MID-BEFT-FOM-UT      TO WS-BEFT-FOM                           
041844     ELSE                                                                 
041845       MOVE MID-BEFT-FOM-IN      TO WS-BEFT-FOM                           
041846     END-IF                                                               
041847     MOVE WS-BEFT-FOM            TO MOD-BEFT-FOM-UT                       
041848                                                                          
041849     MOVE MFS-RENSA-FAELT        TO  MOD-BEFT-TOM-IN                      
041850     IF MID-BEFT-TOM-IN          = ALL '+'                                
041851       MOVE MID-BEFT-TOM-UT      TO WS-BEFT-TOM                           
041852     ELSE                                                                 
041853       MOVE MID-BEFT-TOM-IN      TO WS-BEFT-TOM                           
041854     END-IF                                                               
041855     MOVE WS-BEFT-TOM            TO MOD-BEFT-TOM-UT                       
041856                                                                          
041857     MOVE MFS-RENSA-FAELT        TO  MOD-FLINLFB-IN                       
041858     IF MID-FLINLFB-IN           = ALL '+'                                
041859       MOVE MID-FLINLFB-UT       TO WS-FLINLFB                            
041860     ELSE                                                                 
041861       MOVE MID-FLINLFB-IN       TO WS-FLINLFB                            
041862     END-IF                                                               
041863     MOVE WS-FLINLFB             TO MOD-FLINLFB-UT                        
041864     .                                                                    
041865     EJECT                                                                
041870 D-NAESTA-SIDA SECTION.                                                   
041900                                                                          
042000     MOVE MID-IDLOPNRM-NEXT    TO REQU-IDLOPNRM-START                     
042100     PERFORM MFS-RENSA-FAELT-IN                                           
042200     .                                                                    
042300     EJECT                                                                
042400 E-SAMMA-SIDA SECTION.                                                    
042500                                                                          
042600     IF EGEN-MID OR HELP-MID                                              
042700       MOVE MID-IDLOPNRM-ENTER TO REQU-IDLOPNRM-START                     
042800     ELSE                                                                 
042900       PERFORM MFS-RENSA-FAELT-IN                                         
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
043400 F-CALL-BIZ-LOGIC-W6013310 SECTION.                                       
043500                                                                          
043600     PERFORM FA-INIT-REQU                                                 
043700     CALL W6013310 USING REQU-AREA RESP-AREA MAX-KVRADER                  
043800                         MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB               
043900                         ALT4-PCB 6197-PCB DISP-PCB USEA-PCB              
044000                         PLAA-PCB LOPA-PCB INLA-PCB                       
044100                         INLD-PCB                                         
044200                         INLC-PCB                                         
044300                         ALT-INLC-PCB                                     
044400                         INLB1-PCB                                        
044500                         INLG-PCB KVAE-PCB                                
044600                         KVABSEQ-PCB                                      
044700                         WDK6-PCB WDB6-PCB WDD3-PCB                       
044800                         ADR-INLA-PCB                                     
044900                         ADR-INLC-PCB                                     
045000                         ADR-PLAA-PCB                                     
045100                         ADR-WDK6-PCB                                     
045200                         ADR-STYR-HANA-PCB                                
045300                         ADR-STYR-PLAA-PCB                                
045400                         KOM-KOMA-PCB                                     
045500                         PMRK-INLB-PCB PMRK-INLC-PCB                      
045600                         PMRK-PLAA-PCB                                    
045700                         STYR-HANA-PCB                                    
045800                         STYR-PLAA-PCB                                    
045900                         UPFA-PCB                                         
046000                                                                          
046100     PERFORM FB-SET-MSG-AND-HILIGHT                                       
046200     IF NOT WRONG-KEY                                                     
046300        PERFORM FC-MOVE-RESP-TO-MOD                                       
046400     END-IF                                                               
046500     .                                                                    
046600 FA-INIT-REQU SECTION.                                                    
046700                                                                          
046800     MOVE MAX-KVRADER               TO REQU-KVRADER                       
046900     MOVE '101'                     TO REQU-IDMSGVER                      
047000     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
047100                                                                          
047200     MOVE MID-KDCMDVAL              TO REQU-KDCMDVAL                      
047300     MOVE MID-KVINLART              TO REQU-KVINLART                      
047400     MOVE MID-ADINLOMR              TO REQU-ADINLOMR                      
047500     MOVE MID-ADINLOMR-PRT          TO REQU-ADINLOMR-PRT                  
047600     MOVE MID-IDINLVGN              TO REQU-IDINLVGN                      
047700     MOVE MID-ADINLOMR-NXT          TO REQU-ADINLOMR-NXT                  
047800     MOVE MID-KDKLIPRI              TO REQU-KDKLIPRI                      
047900     MOVE MID-FLSATS                TO REQU-FLSATS                        
048000     MOVE MID-KDPRTVAL              TO REQU-KDPRTVAL                      
048100     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
048200     .                                                                    
048300 FB-SET-MSG-AND-HILIGHT SECTION.                                          
048400                                                                          
048500     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
048600                                       W-IDMSG-ERROR                      
048700     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
048800     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
048900     MOVE MSGI-IDSPRAK              TO MCNV-IDSPRAK                       
049000                                                                          
049100     CALL WL01MCNV USING MCNV-AREA                                        
049200                                                                          
049300     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
049400     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
049500     IF WRONG-KEY                                                         
049600        PERFORM MFS-RENSA-FAELT-UT                                        
049700        PERFORM MFS-RENSA-FAELT-IN                                        
049800     END-IF                                                               
049900     IF RESP-BEPRTLST NOT = ALL '+' AND                                   
050000        RESP-BEPRTLST NOT = SPACES                                        
050100        MOVE RESP-BEPRTLST          TO MOD-TEMFSFEL                       
050200     END-IF                                                               
050300                                                                          
050400     .                                                                    
050500     EJECT                                                                
050600 FC-MOVE-RESP-TO-MOD SECTION.                                             
050700                                                                          
050800     IF RESP-IDLEVNR-KOLLI = SPACE                                        
050900        MOVE MFS-ERASE-FIELD              TO MOD-IDLEVNR-KOLLI-UT         
051000     ELSE                                                                 
051100       IF RESP-IDLEVNR-KOLLI = ALL '+'                                    
051200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDLEVNR-KOLLI-UT         
051300       ELSE                                                               
051400          MOVE RESP-IDLEVNR-KOLLI         TO MOD-IDLEVNR-KOLLI-UT         
051500       END-IF                                                             
051600     END-IF                                                               
051700                                                                          
051800     IF RESP-IDOKOLLI = SPACE                                             
051900        MOVE MFS-ERASE-FIELD              TO MOD-IDOKOLLI-UT              
052000     ELSE                                                                 
052100       IF RESP-IDOKOLLI = ALL '+'                                         
052200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDOKOLLI-UT              
052300       ELSE                                                               
052400          MOVE RESP-IDOKOLLI              TO MOD-IDOKOLLI-UT              
052500       END-IF                                                             
052600     END-IF                                                               
052700                                                                          
052800     IF RESP-IDLOPNRM = SPACE                                             
052900        MOVE MFS-ERASE-FIELD              TO MOD-IDLOPNRM-UT              
053000     ELSE                                                                 
053100       IF RESP-IDLOPNRM  = ALL '+'                                        
053200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDLOPNRM-UT              
053300       ELSE                                                               
053400          MOVE RESP-IDLOPNRM              TO MOD-IDLOPNRM-UT              
053410          INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE         
053500       END-IF                                                             
053600     END-IF                                                               
053700                                                                          
053800     IF RESP-IDLOPNRM-START = SPACE                                       
053900        MOVE MFS-ERASE-FIELD              TO MOD-IDLOPNRM-ENTER           
054000     ELSE                                                                 
054100       IF RESP-IDLOPNRM-START = ALL '+'                                   
054200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDLOPNRM-ENTER           
054300       ELSE                                                               
054400          MOVE RESP-IDLOPNRM-START        TO MOD-IDLOPNRM-ENTER           
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800     IF RESP-IDLOPNRM-NEXT = SPACE                                        
054900        MOVE MFS-ERASE-FIELD              TO MOD-IDLOPNRM-NEXT            
055000     ELSE                                                                 
055100       IF RESP-IDLOPNRM-NEXT = ALL '+'                                    
055200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDLOPNRM-NEXT            
055300       ELSE                                                               
055400          MOVE RESP-IDLOPNRM-NEXT         TO MOD-IDLOPNRM-NEXT            
055500       END-IF                                                             
055600     END-IF                                                               
055700                                                                          
055800     IF RESP-IDARTNR = SPACE                                              
055900        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR                  
056000     ELSE                                                                 
056100       IF RESP-IDARTNR   =  ALL '+'                                       
056200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR                  
056300       ELSE                                                               
056400          MOVE RESP-IDARTNR               TO MOD-IDARTNR                  
056500       END-IF                                                             
056600     END-IF                                                               
056700                                                                          
056800     IF RESP-KVAVIS = SPACE                                               
056900        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS                   
057000     ELSE                                                                 
057100       IF RESP-KVAVIS = ALL '+'                                           
057200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS                   
057300       ELSE                                                               
057400          MOVE RESP-KVAVIS                TO MOD-KVAVIS                   
057500       END-IF                                                             
057600     END-IF                                                               
057700                                                                          
057800     IF RESP-BEART = SPACE                                                
057900        MOVE MFS-ERASE-FIELD              TO MOD-BEART                    
058000     ELSE                                                                 
058100       IF RESP-BEART = ALL '+'                                            
058200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEART                    
058300       ELSE                                                               
058400          MOVE RESP-BEART                 TO MOD-BEART                    
058500       END-IF                                                             
058600     END-IF                                                               
058700                                                                          
058800     IF RESP-KDSORT = SPACE                                               
058900        MOVE MFS-ERASE-FIELD              TO MOD-KDSORT                   
059000     ELSE                                                                 
059100       IF RESP-KDSORT = ALL '+'                                           
059200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDSORT                   
059300       ELSE                                                               
059400          MOVE RESP-KDSORT                TO MOD-KDSORT                   
059500       END-IF                                                             
059600     END-IF                                                               
059700                                                                          
059800     IF RESP-BEFT = SPACE                                                 
059900        MOVE MFS-ERASE-FIELD              TO MOD-BEFT                     
060000     ELSE                                                                 
060100       IF RESP-BEFT = ALL '+'                                             
060200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEFT                     
060300       ELSE                                                               
060400          MOVE RESP-BEFT                  TO MOD-BEFT                     
060500       END-IF                                                             
060600     END-IF                                                               
060700                                                                          
060800     IF RESP-KVKOLLI-TOT = SPACE                                          
060900        MOVE MFS-ERASE-FIELD              TO MOD-KVKOLLI-TOT              
061000     ELSE                                                                 
061100       IF RESP-KVKOLLI-TOT = ALL '+'                                      
061200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVKOLLI-TOT              
061300       ELSE                                                               
061400          MOVE RESP-KVKOLLI-TOT           TO MOD-KVKOLLI-TOT              
061500       END-IF                                                             
061600     END-IF                                                               
061700                                                                          
061800     IF RESP-KVAVIS-KVAR = SPACE                                          
061900        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-KVAR              
062000     ELSE                                                                 
062100       IF RESP-KVAVIS-KVAR = ALL '+'                                      
062200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-KVAR              
062300       ELSE                                                               
062400          MOVE RESP-KVAVIS-KVAR           TO MOD-KVAVIS-KVAR              
062500       END-IF                                                             
062600     END-IF                                                               
062700                                                                          
062800     IF RESP-BEFARLIG = SPACE                                             
062900        MOVE MFS-ERASE-FIELD              TO MOD-BEFARLIG                 
063000     ELSE                                                                 
063100       IF RESP-BEFARLIG  = ALL '+'                                        
063200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEFARLIG                 
063300       ELSE                                                               
063400          MOVE RESP-BEFARLIG              TO MOD-BEFARLIG                 
063500       END-IF                                                             
063600     END-IF                                                               
063700                                                                          
063800     IF RESP-KVAVIS-PRIO-KVAR = SPACE                                     
063900        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-PRIO-KVAR         
064000     ELSE                                                                 
064100       IF RESP-KVAVIS-PRIO-KVAR = ALL '+'                                 
064200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-PRIO-KVAR         
064300       ELSE                                                               
064400          MOVE RESP-KVAVIS-PRIO-KVAR      TO MOD-KVAVIS-PRIO-KVAR         
064500       END-IF                                                             
064600     END-IF                                                               
064700                                                                          
064800     IF RESP-KDLAGEMB = SPACE                                             
064900        MOVE MFS-ERASE-FIELD              TO MOD-KDLAGEMB                 
065000     ELSE                                                                 
065100       IF RESP-KDLAGEMB  = ALL '+'                                        
065200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDLAGEMB                 
065300       ELSE                                                               
065400          MOVE RESP-KDLAGEMB              TO MOD-KDLAGEMB                 
065500       END-IF                                                             
065600     END-IF                                                               
065700                                                                          
065800     IF RESP-ADLAGOMR = SPACE                                             
065900        MOVE MFS-ERASE-FIELD              TO MOD-ADLAGOMR                 
066000     ELSE                                                                 
066100       IF RESP-ADLAGOMR  = ALL '+'                                        
066200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADLAGOMR                 
066300       ELSE                                                               
066400          MOVE RESP-ADLAGOMR              TO MOD-ADLAGOMR                 
066500       END-IF                                                             
066600     END-IF                                                               
066700                                                                          
066800     IF RESP-ADGANG = SPACE                                               
066900        MOVE MFS-ERASE-FIELD              TO MOD-ADGANG                   
067000     ELSE                                                                 
067100       IF RESP-ADGANG = ALL '+'                                           
067200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADGANG                   
067300       ELSE                                                               
067400          MOVE RESP-ADGANG                TO MOD-ADGANG                   
067500       END-IF                                                             
067600     END-IF                                                               
067700                                                                          
067800     IF RESP-ADPLATS = SPACE                                              
067900        MOVE MFS-ERASE-FIELD              TO MOD-ADPLATS                  
068000     ELSE                                                                 
068100       IF RESP-ADPLATS = ALL '+'                                          
068200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADPLATS                  
068300       ELSE                                                               
068400          MOVE RESP-ADPLATS               TO MOD-ADPLATS                  
068500       END-IF                                                             
068600     END-IF                                                               
068700                                                                          
068800     IF RESP-FLKVAANT-TOT = SPACE                                         
068900        MOVE MFS-ERASE-FIELD              TO MOD-FLKVAANT-TOT             
069000     ELSE                                                                 
069100       IF RESP-FLKVAANT-TOT = ALL '+'                                     
069200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLKVAANT-TOT             
069300       ELSE                                                               
069400          MOVE RESP-FLKVAANT-TOT          TO MOD-FLKVAANT-TOT             
069500       END-IF                                                             
069600     END-IF                                                               
069700                                                                          
069800     IF RESP-KVKVAPRIM-KVAR = SPACE                                       
069900        MOVE MFS-ERASE-FIELD              TO MOD-KVKVAPRIM-KVAR           
070000     ELSE                                                                 
070100       IF RESP-KVKVAPRIM-KVAR = ALL '+'                                   
070200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVKVAPRIM-KVAR           
070300       ELSE                                                               
070400          MOVE RESP-KVKVAPRIM-KVAR        TO MOD-KVKVAPRIM-KVAR           
070500       END-IF                                                             
070600     END-IF                                                               
070700                                                                          
070800     IF RESP-KVROS = SPACE                                                
070900        MOVE MFS-ERASE-FIELD              TO MOD-KVROS                    
071000     ELSE                                                                 
071100       IF RESP-KVROS = ALL '+'                                            
071200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVROS                    
071300       ELSE                                                               
071400          MOVE RESP-KVROS                 TO MOD-KVROS                    
071500       END-IF                                                             
071600     END-IF                                                               
071700                                                                          
071800     IF RESP-KVAVIS-KIT-KVAR = SPACE                                      
071900        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-KIT-KVAR          
072000     ELSE                                                                 
072100       IF RESP-KVAVIS-KIT-KVAR = ALL '+'                                  
072200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-KIT-KVAR          
072300       ELSE                                                               
072400          MOVE RESP-KVAVIS-KIT-KVAR       TO MOD-KVAVIS-KIT-KVAR          
072500       END-IF                                                             
072600     END-IF                                                               
072700                                                                          
072800     IF RESP-ADTRDEST-KIT = SPACE                                         
072900        MOVE MFS-ERASE-FIELD              TO MOD-ADTRDEST-KIT             
073000     ELSE                                                                 
073100       IF RESP-ADTRDEST-KIT = ALL '+'                                     
073200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADTRDEST-KIT             
073300       ELSE                                                               
073400          MOVE RESP-ADTRDEST-KIT          TO MOD-ADTRDEST-KIT             
073500       END-IF                                                             
073600     END-IF                                                               
073700                                                                          
073800     IF RESP-ADINLOMR-NXT1 = SPACE                                        
073900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-NXT1            
074000     ELSE                                                                 
074100       IF RESP-ADINLOMR-NXT1 = ALL '+'                                    
074200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-NXT1            
074300       ELSE                                                               
074400          MOVE RESP-ADINLOMR-NXT1         TO MOD-ADINLOMR-NXT1            
074500       END-IF                                                             
074600     END-IF                                                               
074700                                                                          
074800     IF RESP-ADINLOMR-NXT2 = SPACE                                        
074900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-NXT2            
075000     ELSE                                                                 
075100       IF RESP-ADINLOMR-NXT2 = ALL '+'                                    
075200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-NXT2            
075300       ELSE                                                               
075400          MOVE RESP-ADINLOMR-NXT2         TO MOD-ADINLOMR-NXT2            
075500       END-IF                                                             
075600     END-IF                                                               
075700                                                                          
075800     IF RESP-ADINLOMR-NXT3 = SPACE                                        
075900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-NXT3            
076000     ELSE                                                                 
076100       IF RESP-ADINLOMR-NXT3 = ALL '+'                                    
076200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-NXT3            
076300       ELSE                                                               
076400          MOVE RESP-ADINLOMR-NXT3         TO MOD-ADINLOMR-NXT3            
076500       END-IF                                                             
076600     END-IF                                                               
076700                                                                          
076800     IF RESP-ADINLOMR-NXT4 = SPACE                                        
076900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-NXT4            
077000     ELSE                                                                 
077100       IF RESP-ADINLOMR-NXT4 = ALL '+'                                    
077200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-NXT4            
077300       ELSE                                                               
077400          MOVE RESP-ADINLOMR-NXT4         TO MOD-ADINLOMR-NXT4            
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800     IF RESP-ADINLOMR-NXT5 = SPACE                                        
077900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-NXT5            
078000     ELSE                                                                 
078100       IF RESP-ADINLOMR-NXT5 = ALL '+'                                    
078200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-NXT5            
078300       ELSE                                                               
078400          MOVE RESP-ADINLOMR-NXT5         TO MOD-ADINLOMR-NXT5            
078500       END-IF                                                             
078600     END-IF                                                               
078700                                                                          
078800     IF RESP-KDKLIPRI = SPACE                                             
078900        MOVE MFS-ERASE-FIELD              TO MOD-KDKLIPRI                 
079000     ELSE                                                                 
079100       IF RESP-KDKLIPRI = ALL '+'                                         
079200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDKLIPRI                 
079300       ELSE                                                               
079400          MOVE RESP-KDKLIPRI              TO MOD-KDKLIPRI                 
079500       END-IF                                                             
079600     END-IF                                                               
079700                                                                          
079800     IF RESP-FLSATS = SPACE                                               
079900        MOVE MFS-ERASE-FIELD              TO MOD-FLSATS                   
080000     ELSE                                                                 
080100       IF RESP-FLSATS = ALL '+'                                           
080200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLSATS                   
080300       ELSE                                                               
080400          MOVE RESP-FLSATS                TO MOD-FLSATS                   
080500       END-IF                                                             
080600     END-IF                                                               
080700                                                                          
080800     IF RESP-FLKVAANT = SPACE                                             
080900        MOVE MFS-ERASE-FIELD              TO MOD-FLKVAANT                 
081000     ELSE                                                                 
081100       IF RESP-FLKVAANT = ALL '+'                                         
081200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLKVAANT                 
081300       ELSE                                                               
081400          MOVE RESP-FLKVAANT              TO MOD-FLKVAANT                 
081500       END-IF                                                             
081600     END-IF                                                               
081700                                                                          
081800     IF RESP-KDCMDVAL-INM = SPACE                                         
081900        MOVE MFS-ERASE-FIELD              TO MOD-KDCMDVAL-INM             
082000     ELSE                                                                 
082100       IF RESP-KDCMDVAL-INM = ALL '+'                                     
082200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDCMDVAL-INM             
082300       ELSE                                                               
082400          MOVE RESP-KDCMDVAL-INM          TO MOD-KDCMDVAL-INM             
082500       END-IF                                                             
082600     END-IF                                                               
082700                                                                          
082800     IF RESP-KVINLART-INM = SPACE                                         
082900        MOVE MFS-ERASE-FIELD              TO MOD-KVINLART-INM             
083000     ELSE                                                                 
083100       IF RESP-KVINLART-INM = ALL '+'                                     
083200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVINLART-INM             
083300       ELSE                                                               
083400          MOVE RESP-KVINLART-INM          TO MOD-KVINLART-INM             
083500       END-IF                                                             
083600     END-IF                                                               
083700                                                                          
083800     IF RESP-ADINLOMR-INM = SPACE                                         
083900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-INM             
084000     ELSE                                                                 
084100       IF RESP-ADINLOMR-INM = ALL '+'                                     
084200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-INM             
084300       ELSE                                                               
084400          MOVE RESP-ADINLOMR-INM          TO MOD-ADINLOMR-INM             
084500       END-IF                                                             
084600     END-IF                                                               
084700                                                                          
084800     IF RESP-IDINLVGN-INM = SPACE                                         
084900        MOVE MFS-ERASE-FIELD              TO MOD-IDINLVGN-INM             
085000     ELSE                                                                 
085100       IF RESP-IDINLVGN-INM = ALL '+'                                     
085200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDINLVGN-INM             
085300       ELSE                                                               
085400          MOVE RESP-IDINLVGN-INM          TO MOD-IDINLVGN-INM             
085500       END-IF                                                             
085600     END-IF                                                               
085700                                                                          
085800     IF RESP-ADINLOMR-NXT-INM = SPACE                                     
085900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-NXT-INM         
086000     ELSE                                                                 
086100       IF RESP-ADINLOMR-NXT-INM = ALL '+'                                 
086200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-NXT-INM         
086300       ELSE                                                               
086400          MOVE RESP-ADINLOMR-NXT-INM      TO MOD-ADINLOMR-NXT-INM         
086500       END-IF                                                             
086600     END-IF                                                               
086700                                                                          
086800     IF RESP-KDKLIPRI-INM = SPACE                                         
086900        MOVE MFS-ERASE-FIELD              TO MOD-KDKLIPRI-INM             
087000     ELSE                                                                 
087100       IF RESP-KDKLIPRI-INM = ALL '+'                                     
087200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDKLIPRI-INM             
087300       ELSE                                                               
087400          MOVE RESP-KDKLIPRI-INM          TO MOD-KDKLIPRI-INM             
087500       END-IF                                                             
087600     END-IF                                                               
087700                                                                          
087800     IF RESP-FLSATS-INM = SPACE                                           
087900        MOVE MFS-ERASE-FIELD              TO MOD-FLSATS-INM               
088000     ELSE                                                                 
088100       IF RESP-FLSATS-INM = ALL '+'                                       
088200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLSATS-INM               
088300       ELSE                                                               
088400          MOVE RESP-FLSATS-INM            TO MOD-FLSATS-INM               
088500       END-IF                                                             
088600     END-IF                                                               
088700                                                                          
088800     IF RESP-ADINLOMR-PRT = SPACE                                         
088900        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-PRT             
089000     ELSE                                                                 
089100       IF RESP-ADINLOMR-PRT = ALL '+'                                     
089200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-PRT             
089300       ELSE                                                               
089400          MOVE RESP-ADINLOMR-PRT          TO MOD-ADINLOMR-PRT             
089500       END-IF                                                             
089600     END-IF                                                               
089700                                                                          
089800     IF RESP-KDPRTVAL = SPACE                                             
089900        MOVE MFS-ERASE-FIELD              TO MOD-KDPRTVAL                 
090000     ELSE                                                                 
090100       IF RESP-KDPRTVAL = ALL '+'                                         
090200          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDPRTVAL                 
090300       ELSE                                                               
090400          MOVE RESP-KDPRTVAL              TO MOD-KDPRTVAL                 
090500       END-IF                                                             
090600     END-IF                                                               
090700                                                                          
090800     MOVE RESP-KDCMDVAL-INM-ATTR      TO MOD-KDCMDVAL-INM-ATTR            
090900     MOVE RESP-KVINLART-INM-ATTR      TO MOD-KVINLART-INM-ATTR            
091000     MOVE RESP-ADINLOMR-INM-ATTR      TO MOD-ADINLOMR-INM-ATTR            
091100     MOVE RESP-IDINLVGN-INM-ATTR      TO MOD-IDINLVGN-INM-ATTR            
091200     MOVE RESP-ADINLOMR-NXT-INM-ATTR  TO MOD-ADINLOMR-NXT-INM-ATTR        
091300     MOVE RESP-KDKLIPRI-INM-ATTR      TO MOD-KDKLIPRI-INM-ATTR            
091400     MOVE RESP-FLSATS-INM-ATTR        TO MOD-FLSATS-INM-ATTR              
091500     MOVE RESP-ADINLOMR-PRT-ATTR      TO MOD-ADINLOMR-PRT-ATTR            
091600     MOVE RESP-KDPRTVAL-ATTR          TO MOD-KDPRTVAL-ATTR                
091700                                                                          
091800     MOVE +1  TO INDX                                                     
091900     PERFORM UNTIL INDX > RESP-KVRADER                                    
092000                                                                          
092100       IF RESP-KVINLART-LINE(INDX) = SPACE                                
092200          MOVE MFS-ERASE-FIELD                TO                          
092300                                        MOD-KVINLART(INDX)                
092400       ELSE                                                               
092500          IF RESP-KVINLART-LINE(INDX) = ALL '+'                           
092600             MOVE MFS-DO-NOT-TOUCH-FIELD      TO                          
092700                                        MOD-KVINLART(INDX)                
092800          ELSE                                                            
092900             MOVE RESP-KVINLART-LINE(INDX)   TO                           
093000                                        MOD-KVINLART(INDX)                
093100          END-IF                                                          
093200       END-IF                                                             
093300                                                                          
093400       IF RESP-KVKOLLI-LINE(INDX) = SPACE                                 
093500          MOVE MFS-ERASE-FIELD            TO                              
093600                                     MOD-KVKOLLI(INDX)                    
093700       ELSE                                                               
093800          IF RESP-KVKOLLI-LINE(INDX) = ALL '+'                            
093900             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
094000                                     MOD-KVKOLLI(INDX)                    
094100          ELSE                                                            
094200             MOVE RESP-KVKOLLI-LINE(INDX) TO                              
094300                                     MOD-KVKOLLI(INDX)                    
094400          END-IF                                                          
094500       END-IF                                                             
094600                                                                          
094700       IF RESP-ADINLOMR-LINE(INDX) = SPACE                                
094800          MOVE MFS-ERASE-FIELD            TO                              
094900                                     MOD-ADINLOMR(INDX)                   
095000       ELSE                                                               
095100          IF RESP-ADINLOMR-LINE(INDX) = ALL '+'                           
095200             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
095300                                     MOD-ADINLOMR(INDX)                   
095400          ELSE                                                            
095500             MOVE RESP-ADINLOMR-LINE(INDX) TO                             
095600                                     MOD-ADINLOMR(INDX)                   
095700          END-IF                                                          
095800       END-IF                                                             
095900                                                                          
096000       IF RESP-KDINLSTA-LINE(INDX) = SPACE                                
096100          MOVE MFS-ERASE-FIELD            TO                              
096200                                     MOD-KDINLSTA(INDX)                   
096300       ELSE                                                               
096400          IF RESP-KDINLSTA-LINE(INDX) = ALL '+'                           
096500             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
096600                                     MOD-KDINLSTA(INDX)                   
096700          ELSE                                                            
096800             MOVE RESP-KDINLSTA-LINE(INDX) TO                             
096900                                     MOD-KDINLSTA(INDX)                   
097000          END-IF                                                          
097100       END-IF                                                             
097200                                                                          
097300       IF RESP-IDINLVGN-LINE(INDX) = SPACE                                
097400          MOVE MFS-ERASE-FIELD            TO                              
097500                                     MOD-IDINLVGN(INDX)                   
097600       ELSE                                                               
097700          IF RESP-IDINLVGN-LINE(INDX) = ALL '+'                           
097800             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
097900                                     MOD-IDINLVGN(INDX)                   
098000          ELSE                                                            
098100             MOVE RESP-IDINLVGN-LINE(INDX)                                
098200                                          TO                              
098300                                     MOD-IDINLVGN(INDX)                   
098400          END-IF                                                          
098500       END-IF                                                             
098600                                                                          
098700       IF RESP-ADINLOMR-NXT-LINE(INDX) = SPACE                            
098800          MOVE MFS-ERASE-FIELD            TO                              
098900                                     MOD-ADINLOMR-NXT(INDX)               
099000       ELSE                                                               
099100          IF RESP-ADINLOMR-NXT-LINE(INDX) = ALL '+'                       
099200             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
099300                                     MOD-ADINLOMR-NXT(INDX)               
099400          ELSE                                                            
099500             MOVE RESP-ADINLOMR-NXT-LINE(INDX)                            
099600                                          TO                              
099700                                     MOD-ADINLOMR-NXT(INDX)               
099800          END-IF                                                          
099900       END-IF                                                             
100000                                                                          
100100       ADD +1 TO INDX                                                     
100200     END-PERFORM                                                          
100300                                                                          
100400     PERFORM UNTIL INDX > MAX-KVRADER                                     
100500       MOVE MFS-ERASE-FIELD TO MOD-KVINLART(INDX)                         
100600                               MOD-KVKOLLI(INDX)                          
100700                               MOD-ADINLOMR(INDX)                         
100800                               MOD-KDINLSTA(INDX)                         
100900                               MOD-IDINLVGN(INDX)                         
101000                               MOD-ADINLOMR-NXT(INDX)                     
101100                                                                          
101200       ADD +1 TO INDX                                                     
101300     END-PERFORM                                                          
101400     .                                                                    
101500     EJECT                                                                
101600 MFS-RENSA-FAELT-UT SECTION.                                              
101700                                                                          
101800     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR                             
101900                                  MOD-KVAVIS                              
102000                                  MOD-BEART                               
102100                                  MOD-KDSORT                              
102200                                  MOD-BEFT                                
102300                                  MOD-KVKOLLI-TOT                         
102400                                  MOD-BEFARLIG                            
102500                                  MOD-KVAVIS-KVAR                         
102600                                  MOD-KVAVIS-PRIO-KVAR                    
102700                                  MOD-KDLAGEMB                            
102800                                  MOD-ADLAGOMR                            
102900                                  MOD-ADGANG                              
103000                                  MOD-ADPLATS                             
103100                                  MOD-FLKVAANT-TOT                        
103200                                  MOD-KVKVAPRIM-KVAR                      
103300                                  MOD-KVROS                               
103400                                  MOD-KVAVIS-KIT-KVAR                     
103500                                  MOD-ADTRDEST-KIT                        
103600                                  MOD-ADINLOMR-NXT1                       
103700                                  MOD-ADINLOMR-NXT2                       
103800                                  MOD-ADINLOMR-NXT3                       
103900                                  MOD-ADINLOMR-NXT4                       
104000                                  MOD-ADINLOMR-NXT5                       
104100                                  MOD-IDLOPNRM-ENTER                      
104200                                  MOD-IDLOPNRM-NEXT                       
104300                                                                          
104400     PERFORM MFS-RENSA-RAD-FAELT                                          
104500     .                                                                    
104600     EJECT                                                                
104700 MFS-RENSA-RAD-FAELT SECTION.                                             
104800                                                                          
104900     MOVE MFS-RENSA-FAELT   TO MOD-KDKLIPRI                               
105000                               MOD-FLSATS                                 
105100                               MOD-FLKVAANT                               
105200     MOVE +1 TO INDX                                                      
105300     PERFORM UNTIL INDX     > MAX-INDX                                    
105400       MOVE MFS-RENSA-FAELT TO MOD-KVINLART(INDX)                         
105500                               MOD-KVKOLLI(INDX)                          
105600                               MOD-ADINLOMR(INDX)                         
105700                               MOD-KDINLSTA(INDX)                         
105800                               MOD-IDINLVGN(INDX)                         
105900                               MOD-ADINLOMR-NXT(INDX)                     
106000       ADD +1 TO INDX                                                     
106100     END-PERFORM                                                          
106200     .                                                                    
106300     EJECT                                                                
106400 MFS-RENSA-FAELT-IN SECTION.                                              
106500                                                                          
106600*    --- ALLA INDATA-FÄLT                                                 
106700     MOVE MFS-RENSA-FAELT      TO MOD-KDCMDVAL-INM                        
106800                                  MOD-KVINLART-INM                        
106900                                  MOD-ADINLOMR-INM                        
107000                                  MOD-IDINLVGN-INM                        
107100                                  MOD-ADINLOMR-NXT-INM                    
107200                                  MOD-FLSATS-INM                          
107300     .                                                                    
107400     EJECT                                                                
107500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
107600                                                                          
107700     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDARTNR                             
107800                                  MOD-KVAVIS                              
107900                                  MOD-BEART                               
108000                                  MOD-KDSORT                              
108100                                  MOD-BEFT                                
108200                                  MOD-KVKOLLI-TOT                         
108300                                  MOD-BEFARLIG                            
108400                                  MOD-KVAVIS-KVAR                         
108500                                  MOD-KVAVIS-PRIO-KVAR                    
108600                                  MOD-KDLAGEMB                            
108700                                  MOD-ADLAGOMR                            
108800                                  MOD-ADGANG                              
108900                                  MOD-ADPLATS                             
109000                                  MOD-FLKVAANT-TOT                        
109100                                  MOD-KVKVAPRIM-KVAR                      
109200                                  MOD-KVROS                               
109300                                  MOD-KVAVIS-KIT-KVAR                     
109400                                  MOD-ADTRDEST-KIT                        
109500                                  MOD-ADINLOMR-NXT1                       
109600                                  MOD-ADINLOMR-NXT2                       
109700                                  MOD-ADINLOMR-NXT3                       
109800                                  MOD-ADINLOMR-NXT4                       
109900                                  MOD-ADINLOMR-NXT5                       
110000                                  MOD-IDLOPNRM-ENTER                      
110100                                  MOD-IDLOPNRM-NEXT                       
110200                                                                          
110300     PERFORM MFS-ROER-EJ-RAD-FAELT                                        
110400     .                                                                    
110500     EJECT                                                                
110600 MFS-ROER-EJ-RAD-FAELT SECTION.                                           
110700                                                                          
110800     MOVE MFS-ROER-EJ-FAELT   TO MOD-KDKLIPRI                             
110900                                 MOD-FLSATS                               
111000                                 MOD-FLKVAANT                             
111100     MOVE +1 TO INDX                                                      
111200     PERFORM UNTIL INDX      > MAX-INDX                                   
111300       MOVE MFS-ROER-EJ-FAELT TO MOD-KVINLART(INDX)                       
111400                                 MOD-KVKOLLI(INDX)                        
111500                                 MOD-ADINLOMR(INDX)                       
111600                                 MOD-KDINLSTA(INDX)                       
111700                                 MOD-IDINLVGN(INDX)                       
111800                                 MOD-ADINLOMR-NXT(INDX)                   
111900       ADD +1 TO INDX                                                     
112000     END-PERFORM                                                          
112100     .                                                                    
112200     EJECT                                                                
112300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
112400                                                                          
112500     MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMDVAL-INM                        
112600                                  MOD-KVINLART-INM                        
112700                                  MOD-ADINLOMR-INM                        
112800                                  MOD-IDINLVGN-INM                        
112900                                  MOD-ADINLOMR-NXT-INM                    
113000                                  MOD-FLSATS-INM                          
113100                                  MOD-KDKLIPRI-INM                        
113200                                  MOD-ADINLOMR-PRT                        
113300     .                                                                    
113400     SKIP2                                                                
113500                                                                          
113600* --- IMS SEKTIONER ---                                                   
113700                                                                          
113800 IMS-GET-MSG SECTION.                                                     
113900     MOVE '  QC' TO GODK-STATUSKODER                                      
114000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
114100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114200     PERFORM IMS-STATUSKONTROLL                                           
114300     .                                                                    
114400     SKIP3                                                                
114500 IMS-INSERT-MSG SECTION.                                                  
114600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
114700       MOVE '0' TO MFS-KDHUVOMR                                           
114800     END-IF                                                               
114900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
115000     MOVE SPACE TO GODK-STATUSKODER                                       
115100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
115200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
115300     PERFORM IMS-STATUSKONTROLL                                           
115400     .                                                                    
115500     EJECT                                                                
115600 IMS-STATUSKONTROLL SECTION.                                              
115700     SET STATUS-IX TO 1                                                   
115800     SEARCH GODK-STATUS                                                   
115900       AT END                                                             
116000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
116100         DELIMITED BY SIZE INTO FELTEXT                                   
116200         CALL FELLOG                                                      
116300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
116400         CONTINUE                                                         
116500     END-SEARCH                                                           
116600     .                                                                    
