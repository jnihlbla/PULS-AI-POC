000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010600.                                                
000400*AUTHOR.         KATARINA KYMMER.                                         
000500*DATE-WRITTEN.   92/02/20.                                                
000600                                                                          
000610*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        " DEFINIERA PLACERINGAR"                                         
001100*                                                                         
001200*        PROGRAMMET LÄSER,UPPDATERAR,TAR BORT PLACERINGAR,                
001300*        EG. INLEVERANSOMRÅDEN                                            
001400*                                                                         
001500*        LÄSER OCH UPPDATERAR PLACERINGSREGISTRET                         
001600*        W6G1                                                             
001700*                                                                         
001800*        LÄSER INLEVERANSREGISTRET                                        
001900*        W6INLA (W6D1)                                                    
002000*                                                                         
002100*        LÄSER STYRREGISTRET                                              
002200*        W6HANB (W6G1)                                                    
002300*                                                                         
002400*        LÄSER PERSONKODSREGISTRET                                        
002500*        WDP3                                                             
002600*                                                                         
002700*        LÄSER DCREGISTRET                                                
002800*        WDB6                                                             
002900*                                                                         
003000*                                                                         
003100*    CHANGES:                                                             
003200*        2008-03-17: ETRACKER 6442199.                                    
003300*        NEW MID- AND MOD- FIELDS FOR INPUT OF TIMES REGARDING            
003400*        NORMAL AND PRIO TIMES, AS WELL AS THE TOTAL TIMES FOR            
003500*        IDDC=11.                                                         
003600*        THE DATA SHOULD UPDATE W6G130 AND TOTAL VALUES TO                
003700*        THE W6GX6008.                                                    
003800*                                                                         
003900*                                                                         
004000*    INDATA.                                                              
004100*        TRANSAKTION: W6T106                                              
004200*        MID:         W6I10601                                            
004300*                                                                         
004400*    UTDATA.                                                              
004500*        MOD:         W6O10601                                            
004600                                                                          
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(08)   VALUE 'W6010600'.            
005500 77  W-ADPLATS-FOM               PIC S9(5)  COMP-3 VALUE ZERO.            
005600 77  W-ADPLATS-TOM               PIC S9(5)  COMP-3 VALUE ZERO.            
005700 77  W-ADGANG-FOM                PIC S9(3)  COMP-3 VALUE ZERO.            
005800 77  W-ADGANG-TOM                PIC S9(3)  COMP-3 VALUE ZERO.            
005900                                                                          
006000*    --  ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006200                                                                          
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  YES                         PIC X       VALUE 'Y'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600                                                                          
006700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +346  COMP SYNC.        
006900                                                                          
007000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007100                                                                          
007200                                                                          
007300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007400     88  NYCKLAR-OK                          VALUE 'J'.                   
007500     88  NYCKLAR-FEL                         VALUE 'N'.                   
007600                                                                          
007700 77  INSERT-SW                  PIC X       VALUE 'N'.                    
007800     88  INSERT-JA                          VALUE 'J'.                    
007900                                                                          
008000 77  DELETE-WS                   PIC X       VALUE 'J'.                   
008100     88  DELETE-OK                           VALUE 'J'.                   
008200     88  DELETE-EJ                           VALUE 'N'.                   
008300                                                                          
008400 77  INDATA-SW                  PIC X        VALUE 'J'.                   
008500     88  INDATA-OK                           VALUE 'J'.                   
008600     88  INDATA-FEL                          VALUE 'N'.                   
008700                                                                          
008710 77  INDATA-6008-SW             PIC X        VALUE 'J'.                   
008720     88  INDATA-6008-SW-OK                   VALUE 'J'.                   
008730     88  INDATA-6008-SW-FEL                  VALUE 'N'.                   
008740                                                                          
008800 77  TRAEFF-SW                   PIC X.                                   
008900     88  TRAEFF-JA                           VALUE 'J'.                   
009000     88  TRAEFF-NEJ                          VALUE 'N'.                   
009100                                                                          
009200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009300     88  ALLT-OK                             VALUE 'J'.                   
009400                                                                          
009500 77  ADINLOMR-SW                 PIC X       VALUE 'N'.                   
009600     88  ADINLOMR-OK                         VALUE 'J'.                   
009700                                                                          
009800 77  KDINLOMR-SW                 PIC X       VALUE 'N'.                   
009900     88  KDINLOMR-OK                         VALUE 'J'.                   
010000                                                                          
010100 77  NYA-NYCKLAR                 PIC X       VALUE 'N'.                   
010200 77  WS-FLKVARED                 PIC X       VALUE 'N'.                   
010300 77  WS-FLKNTRGK                 PIC X       VALUE 'N'.                   
010400 77  WS-FLSVAR                   PIC X       VALUE 'N'.                   
010500 77  WS-ADINLOMR                 PIC X(4).                                
010600 77  WS-KDINLOMR                 PIC X(3).                                
010700 77  IX-TRAEFF1                  PIC S9(3) VALUE +0 COMP SYNC.            
010800 77  IX-TRAEFF2                  PIC S9(3) VALUE +0 COMP SYNC.            
010900 77  IX-TRAEFF3                  PIC S9(3) VALUE +0 COMP SYNC.            
011000                                                                          
011100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011200     88  EGEN-MID                            VALUE '6106'.                
011300     88  GODK-MID                            VALUE '6101' '6102'          
011400                                                   '6103' '6104'          
011500                                                   '6105' '6106'          
011600                                                   '6107' '6108'          
011700                                                   '6109'.                
011800     88  HELP-MID                            VALUE '0551'.                
011900     EJECT                                                                
012000*      --- VALID IDDC CODES                                               
012100*                                                                         
012200*01    -COPY WWDCKONS                                                     
012300       EJECT                                                              
012400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012500 01  GENERELLA-SUBPROGRAM.                                                
012600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013000     EJECT                                                                
013100*01 -COPY WMSGINIT                                                        
013200     SKIP3                                                                
013300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013400*01 -COPY WMEDAREA                                                        
013500     SKIP3                                                                
013600 01  MESSAGE-CODES.                                                       
013700     03  ERR-NOT-ON-REGISTER     PIC X(3)    VALUE '010'.                 
013800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013900     03  ERR-OBEH-ANV            PIC X(3)    VALUE '405'.                 
014000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014100     03  ERR-UPPLYSTA-FAELT-FEL  PIC X(3)    VALUE '001'.                 
014200     03  ERR-OTILL-UPPDAT        PIC X(3)    VALUE '007'.                 
014300     03  ERR-KONFLIKT            PIC X(3)    VALUE '002'.                 
014400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014600     EJECT                                                                
014700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014800*                                                                         
014900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015000     SKIP3                                                                
015100*01  MID -COPY W6I10601                                                   
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015400     SKIP3                                                                
015500*01  -COPY WMSGAREA                                                       
015600     EJECT                                                                
015700     03  MOD REDEFINES MSG-AREA.                                          
015800*      05  -COPY W6O10601                                                 
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016100     SKIP3                                                                
016200*01  -COPY WMFSAREA                                                       
016300     EJECT                                                                
016400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016500*                                                                         
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800     SKIP3                                                                
016900 01  NYCKLAR-TILL-DLI.                                                    
017000     03  W-W6GXKEY-6005-X.                                                
017100         05  W-6005-IDHTYP       PIC X(4)   VALUE '6005'.                 
017200         05  W-6005-IDDC         PIC X(2)   VALUE SPACE.                  
017300         05  FILLER              PIC X(24)  VALUE LOW-VALUE.              
017400     03  W-W6GXKEY-6006-X.                                                
017500         05  W-6006-ADINLOMR     PIC X(4)   VALUE SPACE.                  
017600         05  FILLER              PIC X      VALUE LOW-VALUE.              
017700     03  W-ADINLOMR-PARX.                                                 
017800         05  W-ADINLOMR-PAR      PIC X(4)    VALUE SPACE.                 
017900     03  W-ADINLOMR-LPLX.                                                 
018000         05  W-ADINLOMR-LPL      PIC X(4)    VALUE SPACE.                 
018100     03  W-ADINLOMR-BOX.                                                  
018200         05  W-ADINLOMR-BO       PIC X(4)    VALUE SPACE.                 
018300     03  W-ADINLOMR-X.                                                    
018400         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
018500     03  W-ADINLOMN-X.                                                    
018600         05  W-ADINLOMN          PIC X(4)    VALUE SPACE.                 
018700     03  W-IDARTNR-X.                                                     
018800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018900     03  W-IDRADNR-X.                                                     
019000         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
019100     03  W-IDDC-X.                                                        
019200         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
019300     03  W-W6GXKEY-6031-X.                                                
019400         05  W-6031-IDHTYP       PIC X(4)    VALUE '6031'.                
019500         05  W-6031-IDDC         PIC X(2)    VALUE SPACE.                 
019600         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
019700     03  W-KDARBTYP-X.                                                    
019800         05 FILLER               PIC X(8)    VALUE 'CDC     '.            
019900     03  W-KDARBTYP-QUAL-X.                                               
020000         05 FILLER               PIC X(8)    VALUE 'QUAL    '.            
020100     03  W-IDPERSON-X.                                                    
020200         05 W-IDPERSON           PIC S9(3)   VALUE +0 COMP-3.             
020300                                                                          
020400     03  W-IDDC-B6-X.                                                     
020500         05 W-IDDC-B6            PIC X(2).                                
020600                                                                          
020700     SKIP2                                                                
020800*    --- STATUS-KOD FRÅN IMS                                              
020900 01  STATUS-WS                   PIC XX.                                  
021000     88  SEGMENT-FINNS                       VALUE '  '.                  
021100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021300     SKIP2                                                                
021400 01  GODK-STATUSKODER.                                                    
021500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021600     SKIP3                                                                
021700 01  SSA1                        PIC X(324).                              
021800 01  SSA2                        PIC X(324).                              
021900     EJECT                                                                
022000*    --- IMS FUNKTIONSKODER                                               
022100*01  -COPY W0003                                                          
022200     EJECT                                                                
022300*    ---  DLI INPUT-OUTPUT AREA                                           
022400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022600                                                                          
022700 01  DLI-IO-AREA.                                                         
023100     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
023200     SKIP3                                                                
023900     03  W6HANB01 REDEFINES IO-AREA.                                      
024000*        05  -COPY W6GX01                                                 
024200     03  W6HANB11 REDEFINES IO-AREA.                                      
024300*        05  -COPY W6GX6032                                               
024400     03  W6HANB12 REDEFINES IO-AREA.                                      
024500*        05  -COPY W6GX6034                                               
024600     03  W6HANB13 REDEFINES IO-AREA.                                      
024700*        05  -COPY W6GX6036                                               
024800     03  W6HANB14 REDEFINES IO-AREA.                                      
024900*        05  -COPY W6GX6038                                               
025000     EJECT                                                                
025100     03  W6INLA11 REDEFINES IO-AREA.                                      
025200*        05  -COPY W6D111                                                 
025201                                                                          
025202 01  FILLER               PIC X(16)   VALUE 'W6G101-AREA'.                
025203 01  DLI-IO-W6G101.                                                       
025204*    03  -COPY W6GX01                                                     
025205     SKIP3                                                                
025206 01  FILLER               PIC X(16)   VALUE 'W6G130-AREA'.                
025207 01  DLI-IO-W6G130.                                                       
025208*    03  -COPY W6GX6006                                                   
025209     SKIP3                                                                
025210 01  FILLER               PIC X(16)   VALUE 'WDGX6008-AREA'.              
025211 01  DLI-IO-W6GX6008.                                                     
025220*    03  -COPY W6GX6008                                                   
025300                                                                          
025400 01  FILLER               PIC X(16)   VALUE 'WDP311-AREA'.                
025600 01  DLI-IO-P311.                                                         
025700*    03  -COPY WDP311                                                     
025800                                                                          
025900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026000 01  DLI-IO-B601.                                                         
026100*    03  -COPY WDB601                                                     
026200                                                                          
026300     EJECT                                                                
026400 LINKAGE SECTION.                                                         
026500                                                                          
026600*01  -COPY W0009   -PRE MSG-                                              
026700     EJECT                                                                
026800*01  -COPY W0008  -PRE USEA-                                              
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100*01  -COPY W0008  -PRE W6G1-                                              
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE HANB-                                              
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008  -PRE INLA1-                                             
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01  -COPY W0008  -PRE INLA2-                                             
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300*01  -COPY W0008  -PRE WDP3-                                              
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600*01  -COPY W0008  -PRE WDB6-                                              
028700     05  FILLER                  PIC X.                                   
028800     EJECT                                                                
028900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB W6G1-PCB                      
029000                           HANB-PCB INLA1-PCB INLA2-PCB WDP3-PCB          
029100                           WDB6-PCB.                                      
029200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB W6G1-PCB                      
029300                           HANB-PCB INLA1-PCB INLA2-PCB WDP3-PCB          
029400                           WDB6-PCB.                                      
029500                                                                          
029600     PERFORM IMS-GET-MSG                                                  
029700     IF SEGMENT-FINNS                                                     
029800       PERFORM A-INIT                                                     
029900      IF EGEN-MID OR HELP-MID                                             
030000       PERFORM B-KOLLA-NYCKLAR                                            
030100       IF NYCKLAR-OK                                                      
030200         IF MFS-UPDATE  OR MFS-UPD-V                                      
030300          IF NYA-NYCKLAR = JA                                             
030400            PERFORM E-LAES-VISA-INFO                                      
030500          ELSE                                                            
030600           IF MID-ADINLOMR-IN = ALL '+'                                   
030700             IF MID-FLSVAR = JA OR YES                                    
030800                PERFORM D-DELETE                                          
030900             ELSE                                                         
031000                PERFORM C-UPPDATERA                                       
031100             END-IF                                                       
031200           END-IF                                                         
031300          END-IF                                                          
031400         ELSE                                                             
031500           IF NYA-NYCKLAR  = JA                                           
031600             PERFORM E-LAES-VISA-INFO                                     
031700           ELSE                                                           
031800             IF MID-INPUT NOT = ALL '+' OR                                
031900                MID-FLSVAR NOT = ALL '+'                                  
032000               PERFORM F-TRYCK-PF11                                       
032100               IF HELP-MID                                                
032200                 PERFORM E-LAES-VISA-INFO                                 
032300               END-IF                                                     
032400             ELSE                                                         
032500               PERFORM E-LAES-VISA-INFO                                   
032600             END-IF                                                       
032700           END-IF                                                         
032800         END-IF                                                           
032900       END-IF                                                             
033000      ELSE                                                                
033100         MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-IN                          
033200                                 MOD-ADINLOMR-UT                          
033300                                 MOD-KDINLOMR-IN                          
033400                                 MOD-KDINLOMR-UT                          
033500                                 MOD-IDDC-IN                              
033600                                 MOD-IDDC-UT                              
033700         PERFORM MFS-RENSA-FAELT-IN                                       
033800         PERFORM MFS-RENSA-FAELT-UT                                       
033900      END-IF                                                              
034000       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O10601 + 4                      
034100       PERFORM IMS-INSERT-MSG                                             
034200     END-IF                                                               
034300                                                                          
034400     MOVE ZERO TO RETURN-CODE                                             
034500     GOBACK                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 A-INIT SECTION.                                                          
034900                                                                          
035000     IF MSG-DUBBLA-TRANSKODER                                             
035100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10601                 
035200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
035300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035400     ELSE                                                                 
035500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10601                  
035600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
035700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035800     END-IF                                                               
035900                                                                          
036000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
036100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
036200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
036300                                                                          
036400     MOVE LOW-VALUE TO MSG-AREA                                           
036500     MOVE 'W6O106N1' TO MFS-IDMOD                                         
036600     MOVE '6106' TO MOD-IDTRANS                                           
036700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
036800                                                                          
036900     IF EGEN-MID OR HELP-MID                                              
037000       CONTINUE                                                           
037100     ELSE                                                                 
037200       MOVE SPACE TO MFS-KDTRTYP                                          
037300       MOVE '7' TO MFS-IDPFK                                              
037400     END-IF                                                               
037500                                                                          
037600     PERFORM AA-INIT-NYCKLAR                                              
037700                                                                          
037800     IF MSGI-IDLAND-SPR = 'GB'                                            
037900       MOVE +2 TO SPRAK-IX                                                
038000       MOVE 'GB ' TO MED-IDSKYLT                                          
038100     ELSE                                                                 
038200       MOVE +1 TO SPRAK-IX                                                
038300       MOVE 'S  ' TO MED-IDSKYLT                                          
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700*----------------------------------------------------------------*        
038800 AA-INIT-NYCKLAR SECTION.                                                 
038900                                                                          
039000     MOVE ALL '+' TO MSGI-WMSGINIT                                        
039100     MOVE '001'                  TO MSGI-KDCALL                           
039200     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
039300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
039400     MOVE '6106'                 TO MSGI-IDTRANS                          
039500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039600     .                                                                    
039700     EJECT                                                                
039800 B-KOLLA-NYCKLAR SECTION.                                                 
039900                                                                          
040000     MOVE NEJ TO NYA-NYCKLAR                                              
040100     MOVE JA  TO NYCKLAR-SW                                               
040200     MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-UT                              
040300                             MOD-KDINLOMR-UT                              
040400                             MOD-IDDC-UT                                  
040500                                                                          
040600     IF MID-ADINLOMR-IN = ALL '+'                                         
040700       MOVE MID-ADINLOMR-UT TO WS-ADINLOMR                                
040800     ELSE                                                                 
040900       MOVE MID-ADINLOMR-IN TO WS-ADINLOMR                                
041000       MOVE JA TO NYA-NYCKLAR                                             
041100     END-IF                                                               
041200     IF WS-ADINLOMR NOT = SPACE                                           
041300       MOVE JA TO ADINLOMR-SW                                             
041400     END-IF                                                               
041500     MOVE WS-ADINLOMR TO MOD-ADINLOMR-UT                                  
041600                                                                          
041700     IF MID-KDINLOMR-IN = ALL '+'                                         
041800       MOVE MID-KDINLOMR-UT TO WS-KDINLOMR                                
041900     ELSE                                                                 
042000       MOVE MID-KDINLOMR-IN TO WS-KDINLOMR                                
042100       MOVE JA TO NYA-NYCKLAR                                             
042200     END-IF                                                               
042300     IF WS-KDINLOMR NOT = SPACE                                           
042400       IF WS-KDINLOMR = 'LO' OR 'F' OR 'LPL' OR 'FB' OR                   
042500                        'SYS' OR 'BO' OR 'FBP' OR 'TRG' OR                
042600                        'RTA' OR 'PG' OR 'P' OR 'PGP' OR 'SQ' OR          
042700                        'UNL'                                             
042800         MOVE JA TO KDINLOMR-SW                                           
042900       ELSE                                                               
043000         MOVE NEJ TO NYCKLAR-SW                                           
043100       END-IF                                                             
043200     END-IF                                                               
043300     MOVE WS-KDINLOMR TO MOD-KDINLOMR-UT                                  
043400                                                                          
043500*    -- KONTROLL AV IDDC                                                  
043600                                                                          
043700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
043800     IF MID-IDDC-IN = ALL '+'                                             
043900       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
044000     ELSE                                                                 
044100       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
044200       MOVE '7'         TO MFS-IDPFK                                      
044300       MOVE SPACE       TO MFS-KDTRTYP                                    
044400       MOVE JA          TO NYA-NYCKLAR                                    
044500     END-IF                                                               
044600     PERFORM IMS-GU-WDB601                                                
044700                                                                          
044800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
044900         MOVE NEJ       TO NYCKLAR-SW                                     
045000     ELSE                                                                 
045100         MOVE DCS-IDDC  TO W-6005-IDDC                                    
045200                           W-6031-IDDC                                    
045300                           W-IDDC                                         
045400                           MOD-IDDC-UT                                    
045500     END-IF                                                               
045600                                                                          
045700     IF NYCKLAR-FEL                                                       
045800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
045900       CALL WMEDKONV USING MED-WMEDAREA                                   
046000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
046100       PERFORM MFS-RENSA-FAELT-IN                                         
046200       PERFORM MFS-RENSA-FAELT-UT                                         
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 C-UPPDATERA SECTION.                                                     
046700                                                                          
046800     IF MID-INPUT =  ALL '+' AND MID-FLSVAR = ALL '+'                     
046900        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
047000        CALL WMEDKONV USING MED-WMEDAREA                                  
047100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
047200        PERFORM  MFS-ROER-EJ-FAELT-IN                                     
047300        PERFORM  E-LAES-VISA-INFO                                         
047400     ELSE                                                                 
047500        IF MID-KDINLOMR-UT = 'SYS' AND                                    
047600          (MID-FLSVAR      = JA OR YES)   AND NOT MFS-UPD-V               
047700           MOVE NEJ TO INDATA-SW                                          
047800           MOVE ERR-OBEH-ANV TO MED-IDMFSFEL                              
047900           CALL WMEDKONV USING MED-WMEDAREA                               
048000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
048100           MOVE MFS-ADD-SAETT-CURSOR TO  MOD-ADINLOMR-PAR-IN-ATTR         
048200           PERFORM S03-BEH-STAENGDA-FAELT                                 
048300           PERFORM MFS-LAS-IN-IGEN                                        
048400           PERFORM S02-OPPNA-FAELT                                        
048500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
048600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
048700        ELSE                                                              
048800           CONTINUE                                                       
048900        END-IF                                                            
048910                                                                          
049000        IF INDATA-OK                                                      
049100          MOVE WS-ADINLOMR TO W-6006-ADINLOMR                             
049200                              W-ADINLOMR                                  
049300          PERFORM IMS-GHU-W6G130                                          
049400          IF SEGMENT-SAKNAS                                               
049500            IF DCS-CDC OR DCS-CDC-TR                                      
049600              IF DCS-CDC                                                  
049610                                                                          
049700                MOVE WC-CDC-TR TO W-6005-IDDC                             
049800                PERFORM IMS-GHU-W6G130                                    
049810                                                                          
049900                MOVE WC-CDC-SE TO W-6005-IDDC                             
050000                IF SEGMENT-SAKNAS                                         
050100                  MOVE JA TO INSERT-SW                                    
050200                ELSE                                                      
050300                  MOVE NEJ TO INDATA-SW                                   
050400                END-IF                                                    
050500              ELSE                                                        
050600                MOVE WC-CDC-SE TO W-6005-IDDC                             
050700                PERFORM IMS-GHU-W6G130                                    
050800                MOVE WC-CDC-TR TO W-6005-IDDC                             
050900                IF SEGMENT-SAKNAS                                         
051000                  MOVE JA TO INSERT-SW                                    
051100                ELSE                                                      
051200                  MOVE NEJ TO INDATA-SW                                   
051300                END-IF                                                    
051400              END-IF                                                      
051500            ELSE                                                          
051600              IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-NDC-OTHERS               
051700                MOVE JA TO INSERT-SW                                      
051800              END-IF                                                      
051900            END-IF                                                        
052000          END-IF                                                          
052010                                                                          
052100          IF INDATA-OK                                                    
052200            PERFORM CA-KONTROLLERA-INDATA                                 
052300          END-IF                                                          
052310                                                                          
052400          IF INDATA-OK                                                    
052500             MOVE WS-ADINLOMR TO W-6006-ADINLOMR                          
052510                                                                          
052600             PERFORM IMS-GHU-W6G130                                       
052610                                                                          
052700             PERFORM  CB-FLYTTA-DATA                                      
052710                                                                          
052800             IF SEGMENT-FINNS                                             
052900                PERFORM IMS-REPL-W6G130                                   
053000             ELSE                                                         
053100               IF MID-KDINLOMR-UT  NOT = ALL '+'                          
053200                  PERFORM IMS-ISRT-W6G130                                 
053300               ELSE                                                       
053400                  MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                      
053500                  CALL WMEDKONV USING MED-WMEDAREA                        
053600                  MOVE MED-MFSFEL TO MOD-TEMFSFEL                         
053700                  PERFORM MFS-RENSA-FAELT-IN                              
053800                  PERFORM MFS-RENSA-FAELT-UT                              
053900               END-IF                                                     
054000             END-IF                                                       
054001                                                                          
054002             IF INDATA-6008-SW-OK                                         
054005                MOVE WC-CDC-SE TO W-6005-IDDC                             
054006                PERFORM IMS-GHU-W6GX6008                                  
054008                IF MID-KVTID-NORMTOT NOT = ALL '+'                        
054011                  MOVE MID-KVTID-NORMTOT TO 6008-KVTID-NORMTOT            
054012                                            MOD-KVTID-NORMTOT-UT          
054013                END-IF                                                    
054014                                                                          
054015                IF MID-KVTID-PRIOTOT NOT = ALL '+'                        
054018                  MOVE MID-KVTID-PRIOTOT TO 6008-KVTID-PRIOTOT            
054019                                            MOD-KVTID-PRIOTOT-UT          
054020                END-IF                                                    
054021                                                                          
054022                IF MID-KVTID-NTCDC NOT = ALL '+'                          
054023                  MOVE MID-KVTID-NTCDC   TO 6008-KVTID-NTCDC              
054024                                            MOD-KVTID-NTCDC-UT            
054025                END-IF                                                    
054026                                                                          
054027                IF MID-KVTID-PTCDC   NOT = ALL '+'                        
054028                  MOVE MID-KVTID-PTCDC   TO 6008-KVTID-PTCDC              
054029                                            MOD-KVTID-PTCDC-UT            
054030                END-IF                                                    
054031                                                                          
054032                IF MID-KVTID-NTSVS NOT = ALL '+'                          
054033                  MOVE MID-KVTID-NTSVS   TO 6008-KVTID-NTSVS              
054034                                            MOD-KVTID-NTSVS-UT            
054035                END-IF                                                    
054036                                                                          
054037                IF MID-KVTID-PTSVS   NOT = ALL '+'                        
054038                  MOVE MID-KVTID-PTSVS   TO 6008-KVTID-PTSVS              
054039                                            MOD-KVTID-PTSVS-UT            
054040                END-IF                                                    
054041                PERFORM IMS-REPL-W6GX6008                                 
054042             END-IF                                                       
054050                                                                          
054100             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
054200             CALL WMEDKONV USING MED-WMEDAREA                             
054300             MOVE MED-MFSINF TO MOD-TEMFSINF                              
054400             PERFORM MFS-FORM-ATTR                                        
054500             PERFORM MFS-RENSA-FAELT-IN                                   
054600             PERFORM S02-OPPNA-FAELT                                      
054700          ELSE                                                            
054800             PERFORM MFS-ROER-EJ-FAELT-UT                                 
054900             PERFORM MFS-ROER-EJ-FAELT-IN                                 
055000             MOVE ERR-UPPLYSTA-FAELT-FEL TO MED-IDMFSFEL                  
055100             CALL WMEDKONV USING MED-WMEDAREA                             
055200             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
055300          END-IF                                                          
055400       END-IF                                                             
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055710                                                                          
055800 CA-KONTROLLERA-INDATA SECTION.                                           
055810     SKIP2                                                                
055900     MOVE JA TO INDATA-SW                                                 
055910                INDATA-6008-SW                                            
055920                                                                          
056000     IF MID-FLSVAR = ALL '+' OR                                           
056100        MID-FLSVAR = SPACE   OR                                           
056200        MID-FLSVAR = NEJ                                                  
056300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-IN-ATTR                   
056400     ELSE                                                                 
056500        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-IN-ATTR                     
056600        MOVE NEJ TO INDATA-SW                                             
056700     END-IF                                                               
056800                                                                          
056891     IF MID-FLEXCP   = ALL '+' OR                                         
056892        MID-FLEXCP   = JA      OR YES OR                                  
056893        MID-FLEXCP   = NEJ                                                
056894        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP-IN-ATTR                   
056895     ELSE                                                                 
056896        MOVE NEJ TO INDATA-SW                                             
056897        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP-IN-ATTR                     
056898     END-IF                                                               
056900                                                                          
056910     IF MID-KDINLUPF =  ALL '+' AND INSERT-JA                             
057000        MOVE NEJ TO INDATA-SW                                             
057100        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDINLUPF-IN-ATTR                   
057200     ELSE                                                                 
057300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDINLUPF-IN-ATTR                 
057400     END-IF                                                               
057500                                                                          
057600     IF INSERT-JA                                                         
057700       IF MID-IDLEVNR = SPACE OR                                          
057800          (MID-IDLEVNR  > '99399' AND                                     
057900           MID-IDLEVNR  < '99600') OR                                     
058000           MID-IDLEVNR = '3324 '                                          
058100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
058200       ELSE                                                               
058300          MOVE NEJ TO INDATA-SW                                           
058400            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                
058500       END-IF                                                             
058600     ELSE                                                                 
058700       IF MID-IDLEVNR = ALL '+' OR                                        
058800          MID-IDLEVNR = SPACE OR                                          
058900          (MID-IDLEVNR  > '99399' AND                                     
059000           MID-IDLEVNR  < '99600') OR                                     
059100           MID-IDLEVNR  = '3324 '                                         
059200          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
059300       ELSE                                                               
059400          MOVE NEJ TO INDATA-SW                                           
059500          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                  
059600       END-IF                                                             
059700     END-IF                                                               
059800                                                                          
059900                                                                          
060000     IF WS-KDINLOMR = 'LO' OR 'F' OR 'LPL' OR 'FB' OR                     
060100                              'RTA' OR 'TRG' OR 'SYS' OR                  
060200                              'BO' OR 'FBP' OR                            
060300                              'PG ' OR 'UNL' OR 'PGP' OR                  
060400                              'P'  OR 'SQ '                               
060500        CONTINUE                                                          
060600     ELSE                                                                 
060700        MOVE NEJ TO INDATA-SW                                             
060800     END-IF                                                               
060900                                                                          
061000     MOVE MID-ADINLOMR-PAR TO W-6006-ADINLOMR                             
061100     PERFORM IMS-GU-W6G130                                                
061200     IF INSERT-JA                                                         
061300       IF SEGMENT-FINNS OR MID-ADINLOMR-PAR = WS-ADINLOMR                 
061400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
061500       ELSE                                                               
061600        MOVE NEJ TO INDATA-SW                                             
061700          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR             
061800       END-IF                                                             
061900     ELSE                                                                 
062000       IF SEGMENT-FINNS OR MID-ADINLOMR-PAR = WS-ADINLOMR OR              
062100          MID-ADINLOMR-PAR = ALL '+'                                      
062200          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
062300       ELSE                                                               
062400        MOVE NEJ TO INDATA-SW                                             
062500         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR              
062600       END-IF                                                             
062700     END-IF                                                               
062800                                                                          
062900                                                                          
063000     IF MID-KDINLOMR-UT = 'LO'                                            
063100        PERFORM CAA-TYP-LO                                                
063200     END-IF                                                               
063210                                                                          
063300     IF MID-KDINLOMR-UT = 'FB' OR 'FBP' OR 'F' OR 'PG' OR                 
063400                          'P'    OR 'PGP'                                 
063500        PERFORM CAB-TYP-FB-FBP-F                                          
063600     END-IF                                                               
063610                                                                          
063700     IF MID-KDINLOMR-UT = 'TRG'                                           
063800        PERFORM CAC-TYP-TRG                                               
063900     END-IF                                                               
063910                                                                          
064000     IF MID-KDINLOMR-UT = 'BO'                                            
064100        PERFORM CAD-TYP-BO                                                
064200     END-IF                                                               
064210                                                                          
064300     IF MID-KDINLOMR-UT = 'RTA' OR 'SQ '                                  
064400        PERFORM CAE-TYP-RTA                                               
064500     END-IF                                                               
064510                                                                          
064600     IF MID-KDINLOMR-UT = 'LPL' OR 'UNL'                                  
064700        PERFORM CAF-TYP-LPL                                               
064800     END-IF                                                               
064810                                                                          
064820*    --- CHECK ORDINARY LEAD TIMES INPUT                                  
064830     IF MID-KVTID-NORM =  ALL '+'                                         
064850        CONTINUE                                                          
064860     ELSE                                                                 
064862       IF MID-KVTID-NORM   NUMERIC                                        
064864         IF MID-KVTID-NORM (3:2) > 59                                     
064865           MOVE NEJ TO INDATA-SW                                          
064867           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-NORM-IN-ATTR             
064868         ELSE                                                             
064869           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-NORM-IN-ATTR             
064870         END-IF                                                           
064871       END-IF                                                             
064880     END-IF                                                               
064881                                                                          
064890     IF MID-KVTID-PRIO =  ALL '+'                                         
064891        CONTINUE                                                          
064892     ELSE                                                                 
064893       IF MID-KVTID-PRIO   NUMERIC                                        
064894         IF MID-KVTID-PRIO (3:2) > 59                                     
064895           MOVE NEJ TO INDATA-SW                                          
064897           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-PRIO-IN-ATTR             
064898         ELSE                                                             
064899           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-PRIO-IN-ATTR             
064900         END-IF                                                           
064901       END-IF                                                             
064902     END-IF                                                               
064903                                                                          
064904*    --- CHECK TOTAL LEAD TIMES INPUT                                     
064905     IF MID-KVTID-NORMTOT =  ALL '+'                                      
064906        MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-NORMTOT-UT                    
064908     ELSE                                                                 
064909       IF MID-KVTID-NORMTOT   NUMERIC                                     
064910         IF MID-KVTID-NORMTOT (3:2) > 59                                  
064911           MOVE NEJ TO INDATA-6008-SW                                     
064912           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-NORMTOT-IN-ATTR          
064913         ELSE                                                             
064914           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-NORMTOT-IN-ATTR          
064915         END-IF                                                           
064916       ELSE                                                               
064917         MOVE NEJ TO INDATA-6008-SW                                       
064918         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-NORMTOT-IN-ATTR            
064919       END-IF                                                             
064920     END-IF                                                               
064921                                                                          
064922     IF MID-KVTID-PRIOTOT =  ALL '+'                                      
064923        MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-PRIOTOT-UT                    
064924     ELSE                                                                 
064925       IF MID-KVTID-PRIOTOT   NUMERIC                                     
064926         IF MID-KVTID-PRIOTOT (3:2) > 59                                  
064927           MOVE NEJ TO INDATA-6008-SW                                     
064928           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-PRIOTOT-IN-ATTR          
064929         ELSE                                                             
064930           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-PRIOTOT-IN-ATTR          
064931         END-IF                                                           
064932       ELSE                                                               
064933         MOVE NEJ TO INDATA-6008-SW                                       
064934         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-PRIOTOT-IN-ATTR            
064935       END-IF                                                             
064936     END-IF                                                               
064937                                                                          
064938     IF MID-KVTID-NTCDC   =  ALL '+'                                      
064939        MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-NTCDC-UT                      
064940     ELSE                                                                 
064941       IF MID-KVTID-NTCDC     NUMERIC                                     
064942         IF MID-KVTID-NTCDC (3:2) > 59                                    
064943           MOVE NEJ TO INDATA-6008-SW                                     
064944           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-NTCDC-IN-ATTR            
064945         ELSE                                                             
064946           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-NTCDC-IN-ATTR            
064947         END-IF                                                           
064948       ELSE                                                               
064949         MOVE NEJ TO INDATA-6008-SW                                       
064950         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-NTCDC-IN-ATTR              
064951       END-IF                                                             
064952     END-IF                                                               
064953                                                                          
064954     IF MID-KVTID-PTCDC   =  ALL '+'                                      
064955        MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-PTCDC-UT                      
064956     ELSE                                                                 
064957       IF MID-KVTID-PTCDC     NUMERIC                                     
064958         IF MID-KVTID-PTCDC (3:2) > 59                                    
064959           MOVE NEJ TO INDATA-6008-SW                                     
064960           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-PTCDC-IN-ATTR            
064961         ELSE                                                             
064962           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-PTCDC-IN-ATTR            
064963         END-IF                                                           
064964       ELSE                                                               
064965         MOVE NEJ TO INDATA-6008-SW                                       
064966         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-PTCDC-IN-ATTR              
064967       END-IF                                                             
064968     END-IF                                                               
064969                                                                          
064970     IF MID-KVTID-NTSVS   =  ALL '+'                                      
064971        MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-NTSVS-UT                      
064972     ELSE                                                                 
064973       IF MID-KVTID-NTSVS     NUMERIC                                     
064974         IF MID-KVTID-NTSVS (3:2) > 59                                    
064975           MOVE NEJ TO INDATA-6008-SW                                     
064976           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-NTSVS-IN-ATTR            
064977         ELSE                                                             
064978           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-NTSVS-IN-ATTR            
064979         END-IF                                                           
064980       ELSE                                                               
064981         MOVE NEJ TO INDATA-6008-SW                                       
064982         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-NTSVS-IN-ATTR              
064983       END-IF                                                             
064984     END-IF                                                               
064985                                                                          
064986     IF MID-KVTID-PTSVS   =  ALL '+'                                      
064987        MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-PTSVS-UT                      
064988     ELSE                                                                 
064989       IF MID-KVTID-PTSVS     NUMERIC                                     
064990         IF MID-KVTID-PTSVS (3:2) > 59                                    
064991           MOVE NEJ TO INDATA-6008-SW                                     
064992           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-PTSVS-IN-ATTR            
064993         ELSE                                                             
064994           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTID-PTSVS-IN-ATTR            
064995         END-IF                                                           
064996       ELSE                                                               
064997         MOVE NEJ TO INDATA-6008-SW                                       
064998         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVTID-PTSVS-IN-ATTR              
064999       END-IF                                                             
065000     END-IF                                                               
065001     .                                                                    
065002     EJECT                                                                
065010                                                                          
065100 CAA-TYP-LO SECTION.                                                      
065200     SKIP2                                                                
065300     IF INSERT-JA                                                         
065400       IF MID-FLLOLL = YES OR JA OR NEJ                                   
065500          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLOLL-IN-ATTR                 
065600       ELSE                                                               
065700          MOVE NEJ TO INDATA-SW                                           
065800            MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLOLL-IN-ATTR                 
065900       END-IF                                                             
066000       IF MID-FLCDOMR = YES OR JA OR NEJ                                  
066100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLCDOMR-IN-ATTR                
066200       ELSE                                                               
066300          MOVE NEJ TO INDATA-SW                                           
066400            MOVE MFS-ALFA-FAELT-FEL TO MOD-FLCDOMR-IN-ATTR                
066500       END-IF                                                             
066600     ELSE                                                                 
066700       IF MID-FLLOLL = ALL '+' OR                                         
066800          MID-FLLOLL = JA  OR YES OR                                      
066900          MID-FLLOLL = NEJ                                                
067000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLOLL-IN-ATTR                 
067100       ELSE                                                               
067200          MOVE NEJ TO INDATA-SW                                           
067300            MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLOLL-IN-ATTR                 
067400       END-IF                                                             
067500       IF MID-FLCDOMR = ALL '+' OR                                        
067600          MID-FLCDOMR = JA OR YES OR                                      
067700          MID-FLCDOMR = NEJ                                               
067800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLCDOMR-IN-ATTR                
067900       ELSE                                                               
068000          MOVE NEJ TO INDATA-SW                                           
068100            MOVE MFS-ALFA-FAELT-FEL TO MOD-FLCDOMR-IN-ATTR                
068200       END-IF                                                             
068300     END-IF                                                               
068400                                                                          
068500     IF MID-ADINLOMR-BO NOT = ALL '+' AND SPACE                           
068600       MOVE MID-ADINLOMR-BO TO W-6006-ADINLOMR                            
068700       PERFORM IMS-GU-W6G130                                              
068800       IF INSERT-JA                                                       
068900         IF SEGMENT-FINNS                                                 
069000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-BO-IN-ATTR          
069100         ELSE                                                             
069200            MOVE NEJ TO INDATA-SW                                         
069300             MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-BO-IN-ATTR           
069400         END-IF                                                           
069500       ELSE                                                               
069600         IF SEGMENT-FINNS  OR MID-ADINLOMR-BO = ALL '+'                   
069700            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-BO-IN-ATTR          
069800         ELSE                                                             
069900          MOVE NEJ TO INDATA-SW                                           
070000           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-BO-IN-ATTR             
070100         END-IF                                                           
070200       END-IF                                                             
070300     END-IF                                                               
070400                                                                          
070500     MOVE MID-ADINLOMR-LPL TO W-6006-ADINLOMR                             
070600     PERFORM IMS-GU-W6G130                                                
070700     IF INSERT-JA                                                         
070800       IF SEGMENT-FINNS                                                   
070900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR            
071000       ELSE                                                               
071100         MOVE NEJ TO INDATA-SW                                            
071200           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR            
071300       END-IF                                                             
071400     ELSE                                                                 
071500       IF SEGMENT-FINNS OR MID-ADINLOMR-LPL = ALL '+'                     
071600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR            
071700       ELSE                                                               
071800         MOVE NEJ TO INDATA-SW                                            
071900           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR            
072000       END-IF                                                             
072100     END-IF                                                               
072200                                                                          
072300     IF MID-KDLORAPP = 3                                                  
072400       MOVE MID-ADINLOMR-PRT TO W-6006-ADINLOMR                           
072500       PERFORM IMS-GU-W6G130                                              
072600       IF INSERT-JA                                                       
072700        IF SEGMENT-FINNS OR MID-ADINLOMR-PRT = WS-ADINLOMR                
072800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-IN-ATTR            
072900        ELSE                                                              
073000          MOVE NEJ TO INDATA-SW                                           
073100          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-IN-ATTR             
073200        END-IF                                                            
073300       ELSE                                                               
073400        IF SEGMENT-FINNS OR MID-ADINLOMR-PRT = WS-ADINLOMR OR             
073500           MID-ADINLOMR-PRT = ALL '+'                                     
073600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-IN-ATTR            
073700        ELSE                                                              
073800          MOVE NEJ TO INDATA-SW                                           
073900          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-IN-ATTR             
074000        END-IF                                                            
074100       END-IF                                                             
074200     END-IF                                                               
074300                                                                          
074400      IF MID-KDLORAPP = 0                                                 
074500        MOVE SPACE               TO MOD-TEXT1-IN                          
074600      END-IF                                                              
074700      IF MID-KDLORAPP = 1                                                 
074800        MOVE 'NORMALFLÖDE      ' TO MOD-TEXT1-IN                          
074900      END-IF                                                              
075000      IF MID-KDLORAPP = 2                                                 
075100        MOVE 'AR SKA TAS UT    ' TO MOD-TEXT1-IN                          
075200      END-IF                                                              
075300      IF MID-KDLORAPP = 3                                                 
075400        MOVE 'I-LISTA VID MOTT ' TO MOD-TEXT1-IN                          
075500      END-IF                                                              
075600      IF MID-KDLORAPP = 4                                                 
075700        MOVE 'R32 VID LOSSNING ' TO MOD-TEXT1-IN                          
075800      END-IF                                                              
075900      IF MID-KDLORAPP = 5                                                 
076000        MOVE 'NO PRINT OF RR   ' TO MOD-TEXT1-IN                          
076100      END-IF                                                              
076200                                                                          
076300     IF WS-ADINLOMR > ZERO                                                
076400       IF MID-KDLORAPP  = ALL '+' AND INSERT-JA                           
076500          MOVE NEJ  TO INDATA-SW                                          
076600            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDLORAPP-IN-ATTR               
076700       ELSE                                                               
076800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDLORAPP-IN-ATTR               
076900       END-IF                                                             
077000     END-IF                                                               
077100                                                                          
077200     IF MID-IDPERSON-ANSV = ALL '+'   OR                                  
077300        MID-IDPERSON-ANSV = ZERO                                          
077400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ANSV-IN-ATTR             
077500     ELSE                                                                 
077600       IF MID-IDPERSON-ANSV NUMERIC                                       
077700          MOVE MID-IDPERSON-ANSV TO W-IDPERSON                            
077800          PERFORM IMS-GU-WDP311                                           
077900          IF SEGMENT-FINNS                                                
078000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ANSV-IN-ATTR        
078100            MOVE PERS-BEINIT          TO MOD-BEINIT-ANSV                  
078200          ELSE                                                            
078300            MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-ANSV-IN-ATTR        
078400            MOVE NEJ TO INDATA-SW                                         
078500          END-IF                                                          
078600       ELSE                                                               
078700          MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-ANSV-IN-ATTR          
078800          MOVE NEJ TO INDATA-SW                                           
078900       END-IF                                                             
079000     END-IF                                                               
079100                                                                          
079200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADPLATS-FOM-IN-ATTR                
079300                                   MOD-ADPLATS-TOM-IN-ATTR                
079400                                   MOD-ADGANG-FOM-IN-ATTR                 
079500                                   MOD-ADGANG-TOM-IN-ATTR                 
079600                                   MOD-FLKVARED-IN-ATTR                   
079700                                   MOD-FLKNTRGK-IN-ATTR                   
079710                                   MOD-FLEXCP-IN-ATTR                     
079720                                   MOD-IDAVD-DAG-IN-ATTR                  
079730                                   MOD-IDGRUPP-DAG-IN-ATTR                
079740                                   MOD-IDAVD-NATT-IN-ATTR                 
079750                                   MOD-IDGRUPP-NATT-IN-ATTR               
079800                                   MOD-TEXT1-IN-ATTR                      
079900                                   MOD-IDPERSON-ANSV-IN                   
080000     IF MID-KDLORAPP NOT = 3                                              
080100      MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PRT-IN-ATTR              
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 CAB-TYP-FB-FBP-F SECTION.                                                
080600                                                                          
080700                                                                          
080800     MOVE MID-ADINLOMR-LPL TO W-6006-ADINLOMR                             
080900     PERFORM IMS-GU-W6G130                                                
081000     IF INSERT-JA                                                         
081100      IF SEGMENT-FINNS                                                    
081200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR            
081300      ELSE                                                                
081400         MOVE NEJ TO INDATA-SW                                            
081500           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR            
081600      END-IF                                                              
081700     ELSE                                                                 
081800      IF SEGMENT-FINNS OR MID-ADINLOMR-LPL = ALL '+'                      
081900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR            
082000      ELSE                                                                
082100         MOVE NEJ TO INDATA-SW                                            
082200           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR            
082300      END-IF                                                              
082400     END-IF                                                               
082500                                                                          
082600     IF WS-KDINLOMR = 'FB' OR 'PG'                                        
082700       IF INSERT-JA                                                       
082800         IF MID-FLKVARED = JA  OR YES OR                                  
082900            MID-FLKVARED = NEJ                                            
083000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKVARED-IN-ATTR             
083100         ELSE                                                             
083200            MOVE NEJ TO INDATA-SW                                         
083300            MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVARED-IN-ATTR               
083400         END-IF                                                           
083500       ELSE                                                               
083600         IF MID-FLKVARED = ALL '+' OR                                     
083700            MID-FLKVARED = JA    OR YES OR                                
083800            MID-FLKVARED = NEJ                                            
083900            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKVARED-IN-ATTR             
084000         ELSE                                                             
084100            MOVE NEJ TO INDATA-SW                                         
084200            MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVARED-IN-ATTR               
084300         END-IF                                                           
084400       END-IF                                                             
084500     END-IF                                                               
084600                                                                          
084700     IF INSERT-JA                                                         
084800       IF MID-FLKNTRGK = JA    OR YES OR                                  
084900          MID-FLKNTRGK = NEJ                                              
085000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKNTRGK-IN-ATTR               
085100       ELSE                                                               
085200          MOVE NEJ TO INDATA-SW                                           
085300          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKNTRGK-IN-ATTR                 
085400       END-IF                                                             
085500     ELSE                                                                 
085600       IF MID-FLKNTRGK = ALL '+' OR                                       
085700          MID-FLKNTRGK = JA      OR YES OR                                
085800          MID-FLKNTRGK = NEJ                                              
085900          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKNTRGK-IN-ATTR               
086000       ELSE                                                               
086100          MOVE NEJ TO INDATA-SW                                           
086200          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKNTRGK-IN-ATTR                 
086300       END-IF                                                             
086400     END-IF                                                               
086500                                                                          
086601                                                                          
086610     IF MID-IDPERSON-ANSV = ALL '+'   OR                                  
086700        MID-IDPERSON-ANSV = ZERO                                          
086800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ANSV-IN-ATTR             
086900     ELSE                                                                 
087000       IF MID-IDPERSON-ANSV NUMERIC                                       
087100         MOVE MID-IDPERSON-ANSV TO W-IDPERSON                             
087200         PERFORM IMS-GU-WDP311                                            
087300         IF SEGMENT-FINNS                                                 
087400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ANSV-IN-ATTR         
087500           MOVE PERS-BEINIT          TO MOD-BEINIT-ANSV                   
087600         ELSE                                                             
087700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-ANSV-IN-ATTR         
087800           MOVE NEJ TO INDATA-SW                                          
087900         END-IF                                                           
088000       ELSE                                                               
088100          MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-ANSV-IN-ATTR          
088200          MOVE NEJ TO INDATA-SW                                           
088300       END-IF                                                             
088400     END-IF                                                               
088500                                                                          
088600     IF MID-IDPERSON-FORP = ALL '+'   OR                                  
088700        MID-IDPERSON-FORP = ZERO                                          
088800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-FORP-IN-ATTR             
088900     ELSE                                                                 
089000       IF MID-IDPERSON-FORP NUMERIC                                       
089100         MOVE MID-IDPERSON-FORP TO W-IDPERSON                             
089200         PERFORM IMS-GU-WDP311                                            
089300         IF SEGMENT-FINNS                                                 
089400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-FORP-IN-ATTR         
089500           MOVE PERS-BEINIT          TO MOD-BEINIT-FORP                   
089600         ELSE                                                             
089700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-FORP-IN-ATTR         
089800           MOVE NEJ TO INDATA-SW                                          
089900         END-IF                                                           
090000       ELSE                                                               
090100          MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-FORP-IN-ATTR          
090200          MOVE NEJ TO INDATA-SW                                           
090300       END-IF                                                             
090400     END-IF                                                               
090500                                                                          
090600     IF MID-IDPERSON-KVAL = ALL '+'   OR                                  
090700        MID-IDPERSON-KVAL = ZERO                                          
090800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-KVAL-IN-ATTR             
090900     ELSE                                                                 
091000       IF MID-IDPERSON-KVAL NUMERIC                                       
091100         MOVE MID-IDPERSON-KVAL TO W-IDPERSON                             
091200         PERFORM IMS-GU-WDP311-QUAL                                       
091300         IF SEGMENT-FINNS                                                 
091400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-KVAL-IN-ATTR         
091500           MOVE PERS-BEINIT          TO MOD-BEINIT-KVAL                   
091600         ELSE                                                             
091700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-KVAL-IN-ATTR         
091800           MOVE NEJ TO INDATA-SW                                          
091900          END-IF                                                          
092000       ELSE                                                               
092100          MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-KVAL-IN-ATTR          
092200          MOVE NEJ TO INDATA-SW                                           
092300       END-IF                                                             
092400     END-IF                                                               
092500                                                                          
092600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-BO-IN-ATTR                
092700                                   MOD-ADINLOMR-PRT-IN-ATTR               
092800                                   MOD-ADPLATS-FOM-IN-ATTR                
092900                                   MOD-ADPLATS-TOM-IN-ATTR                
093000                                   MOD-ADGANG-FOM-IN-ATTR                 
093100                                   MOD-ADGANG-TOM-IN-ATTR                 
093200                                   MOD-KDLORAPP-IN-ATTR                   
093300                                   MOD-TEXT1-IN-ATTR                      
093400                                   MOD-FLLOLL-IN-ATTR                     
093500                                   MOD-FLCDOMR-IN-ATTR                    
093600                                   MOD-IDPERSON-ANSV-IN-ATTR              
093700                                   MOD-IDPERSON-FORP-IN-ATTR              
093800                                   MOD-IDPERSON-KVAL-IN-ATTR              
093900     .                                                                    
094000     EJECT                                                                
094100 CAC-TYP-TRG SECTION.                                                     
094200                                                                          
094300     IF MID-ADINLOMR-BO NOT = ALL '+' AND SPACE                           
094400       MOVE MID-ADINLOMR-BO TO W-6006-ADINLOMR                            
094500       PERFORM IMS-GU-W6G130                                              
094600       IF INSERT-JA                                                       
094700         IF SEGMENT-FINNS                                                 
094800            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-BO-IN-ATTR          
094900         ELSE                                                             
095000            MOVE NEJ TO INDATA-SW                                         
095100              MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-BO-IN-ATTR          
095200         END-IF                                                           
095300       ELSE                                                               
095400         IF SEGMENT-FINNS  OR MID-ADINLOMR-BO = ALL '+' OR SPACE          
095500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-BO-IN-ATTR          
095600         ELSE                                                             
095700            MOVE NEJ TO INDATA-SW                                         
095800              MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-BO-IN-ATTR          
095900         END-IF                                                           
096000       END-IF                                                             
096100     END-IF                                                               
096200                                                                          
096300     IF MID-ADPLATS-FOM = ALL '+'                                         
096400       IF INSERT-JA                                                       
096500         MOVE ZERO TO W-ADPLATS-FOM                                       
096600       ELSE                                                               
096700         MOVE 6006-ADPLATS-FOM TO W-ADPLATS-FOM                           
096800       END-IF                                                             
096900     ELSE                                                                 
097000       MOVE MID-ADPLATS-FOM TO W-ADPLATS-FOM                              
097100     END-IF                                                               
097200                                                                          
097300     IF MID-ADPLATS-TOM = ALL '+'                                         
097400       IF INSERT-JA                                                       
097500         MOVE ZERO TO W-ADPLATS-TOM                                       
097600       ELSE                                                               
097700         MOVE 6006-ADPLATS-TOM TO W-ADPLATS-TOM                           
097800       END-IF                                                             
097900     ELSE                                                                 
098000       MOVE MID-ADPLATS-TOM TO W-ADPLATS-TOM                              
098100     END-IF                                                               
098200                                                                          
098300     IF MID-ADGANG-FOM = ALL '+'                                          
098400       IF INSERT-JA                                                       
098500         MOVE ZERO TO W-ADGANG-FOM                                        
098600       ELSE                                                               
098700         MOVE 6006-ADGANG-FOM TO W-ADGANG-FOM                             
098800       END-IF                                                             
098900     ELSE                                                                 
099000       MOVE MID-ADGANG-FOM TO W-ADGANG-FOM                                
099100     END-IF                                                               
099200                                                                          
099300     IF MID-ADGANG-TOM = ALL '+'                                          
099400       IF INSERT-JA                                                       
099500         MOVE ZERO TO W-ADGANG-TOM                                        
099600       ELSE                                                               
099700         MOVE 6006-ADGANG-TOM TO W-ADGANG-TOM                             
099800       END-IF                                                             
099900     ELSE                                                                 
100000       MOVE MID-ADGANG-TOM TO W-ADGANG-TOM                                
100100     END-IF                                                               
100200                                                                          
100300     IF W-ADPLATS-FOM = ZERO                                              
100400       IF W-ADGANG-FOM = ZERO                                             
100500         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-FOM-IN-ATTR               
100600         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-FOM-IN-ATTR                
100700         MOVE NEJ TO INDATA-SW                                            
100800       ELSE                                                               
100900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-FOM-IN-ATTR             
101000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-FOM-IN-ATTR              
101100       END-IF                                                             
101200     ELSE                                                                 
101300       IF W-ADGANG-FOM NOT = ZERO                                         
101400         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-FOM-IN-ATTR               
101500         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-FOM-IN-ATTR                
101600         MOVE NEJ TO INDATA-SW                                            
101700       ELSE                                                               
101800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-FOM-IN-ATTR             
101900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-FOM-IN-ATTR              
102000       END-IF                                                             
102100     END-IF                                                               
102200                                                                          
102300     IF W-ADPLATS-TOM = ZERO                                              
102400       IF W-ADGANG-TOM = ZERO                                             
102500         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-TOM-IN-ATTR               
102600         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-TOM-IN-ATTR                
102700         MOVE NEJ TO INDATA-SW                                            
102800       ELSE                                                               
102900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-TOM-IN-ATTR             
103000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-TOM-IN-ATTR              
103100       END-IF                                                             
103200     ELSE                                                                 
103300       IF W-ADGANG-TOM NOT = ZERO                                         
103400         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-TOM-IN-ATTR               
103500         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-TOM-IN-ATTR                
103600         MOVE NEJ TO INDATA-SW                                            
103700       ELSE                                                               
103800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-TOM-IN-ATTR             
103900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-TOM-IN-ATTR              
104000       END-IF                                                             
104100     END-IF                                                               
104200                                                                          
104300*    IF MID-ADPLATS-FOM = ALL '+' AND INSERT-JA                           
104400*       MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-FOM-IN-ATTR                
104500*       MOVE NEJ TO INDATA-SW                                             
104600*    ELSE                                                                 
104700*         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-FOM-IN-ATTR            
104800*    END-IF                                                               
104900                                                                          
105000*    IF MID-ADPLATS-TOM = ALL '+' AND INSERT-JA                           
105100*       MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-TOM-IN-ATTR                
105200*       MOVE NEJ TO INDATA-SW                                             
105300*    ELSE                                                                 
105400*         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-TOM-IN-ATTR            
105500*    END-IF                                                               
105600                                                                          
105700*    IF MID-ADGANG-FOM = ALL '+' AND INSERT-JA                            
105800*       MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-FOM-IN-ATTR                 
105900*       MOVE NEJ TO INDATA-SW                                             
106000*    ELSE                                                                 
106100*         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-FOM-IN-ATTR             
106200*    END-IF                                                               
106300                                                                          
106400*    IF MID-ADGANG-TOM = ALL '+' AND INSERT-JA                            
106500*       MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-TOM-IN-ATTR                 
106600*       MOVE NEJ TO INDATA-SW                                             
106700*    ELSE                                                                 
106800*         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-TOM-IN-ATTR             
106900*    END-IF                                                               
107000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-LPL-IN-ATTR               
107100                                   MOD-ADINLOMR-PRT-IN-ATTR               
107200                                   MOD-KDLORAPP-IN-ATTR                   
107300                                   MOD-TEXT1-IN-ATTR                      
107400                                   MOD-FLKVARED-IN-ATTR                   
107500                                   MOD-FLKNTRGK-IN-ATTR                   
107510                                   MOD-FLEXCP-IN-ATTR                     
107520                                   MOD-IDAVD-DAG-IN-ATTR                  
107530                                   MOD-IDGRUPP-DAG-IN-ATTR                
107540                                   MOD-IDAVD-NATT-IN-ATTR                 
107550                                   MOD-IDGRUPP-NATT-IN-ATTR               
107600                                   MOD-FLLOLL-IN-ATTR                     
107700                                   MOD-FLCDOMR-IN-ATTR                    
107800     .                                                                    
107900     EJECT                                                                
108000 CAD-TYP-BO SECTION.                                                      
108100                                                                          
108200     MOVE MID-ADINLOMR-LPL TO W-6006-ADINLOMR                             
108300     PERFORM IMS-GU-W6G130                                                
108400     IF INSERT-JA                                                         
108500      IF SEGMENT-FINNS                                                    
108600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR            
108700      ELSE                                                                
108800         MOVE NEJ TO INDATA-SW                                            
108900         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR              
109000      END-IF                                                              
109100     ELSE                                                                 
109200      IF SEGMENT-FINNS OR MID-ADINLOMR-LPL = ALL '+'                      
109300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR            
109400      ELSE                                                                
109500         MOVE NEJ TO INDATA-SW                                            
109600         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR              
109700      END-IF                                                              
109800     END-IF                                                               
109900                                                                          
110000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-BO-IN-ATTR                
110100                                   MOD-ADINLOMR-PRT-IN-ATTR               
110200                                   MOD-ADPLATS-FOM-IN-ATTR                
110300                                   MOD-ADPLATS-TOM-IN-ATTR                
110400                                   MOD-ADGANG-FOM-IN-ATTR                 
110500                                   MOD-ADGANG-TOM-IN-ATTR                 
110600                                   MOD-KDLORAPP-IN-ATTR                   
110700                                   MOD-TEXT1-IN-ATTR                      
110800                                   MOD-FLKVARED-IN-ATTR                   
110900                                   MOD-FLKNTRGK-IN-ATTR                   
110910                                   MOD-FLEXCP-IN-ATTR                     
110920                                   MOD-IDAVD-DAG-IN-ATTR                  
110930                                   MOD-IDGRUPP-DAG-IN-ATTR                
110940                                   MOD-IDAVD-NATT-IN-ATTR                 
110950                                   MOD-IDGRUPP-NATT-IN-ATTR               
111000                                   MOD-FLLOLL-IN-ATTR                     
111100                                   MOD-FLCDOMR-IN-ATTR                    
111200     .                                                                    
111300     EJECT                                                                
111400 CAE-TYP-RTA SECTION.                                                     
111500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-BO-IN-ATTR                
111600                                   MOD-ADINLOMR-PRT-IN-ATTR               
111700                                   MOD-ADINLOMR-LPL-IN-ATTR               
111800                                   MOD-ADPLATS-FOM-IN-ATTR                
111900                                   MOD-ADPLATS-TOM-IN-ATTR                
112000                                   MOD-ADGANG-FOM-IN-ATTR                 
112100                                   MOD-ADGANG-TOM-IN-ATTR                 
112200                                   MOD-KDLORAPP-IN-ATTR                   
112300                                   MOD-TEXT1-IN-ATTR                      
112400                                   MOD-FLLOLL-IN-ATTR                     
112500                                   MOD-FLCDOMR-IN-ATTR                    
112600                                   MOD-FLKVARED-IN-ATTR                   
112700                                   MOD-FLKNTRGK-IN-ATTR                   
112710                                   MOD-FLEXCP-IN-ATTR                     
112720                                   MOD-IDAVD-DAG-IN-ATTR                  
112730                                   MOD-IDGRUPP-DAG-IN-ATTR                
112740                                   MOD-IDAVD-NATT-IN-ATTR                 
112750                                   MOD-IDGRUPP-NATT-IN-ATTR               
112800     .                                                                    
112900     EJECT                                                                
113000 CAF-TYP-LPL SECTION.                                                     
113100                                                                          
113200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-BO-IN-ATTR                
113300                                   MOD-ADINLOMR-PRT-IN-ATTR               
113400                                   MOD-ADINLOMR-LPL-IN-ATTR               
113500                                   MOD-ADPLATS-FOM-IN-ATTR                
113600                                   MOD-ADPLATS-TOM-IN-ATTR                
113700                                   MOD-ADGANG-FOM-IN-ATTR                 
113800                                   MOD-ADGANG-TOM-IN-ATTR                 
113900                                   MOD-KDLORAPP-IN-ATTR                   
114000                                   MOD-TEXT1-IN-ATTR                      
114100                                   MOD-FLKVARED-IN-ATTR                   
114200                                   MOD-FLKNTRGK-IN-ATTR                   
114210                                   MOD-FLEXCP-IN-ATTR                     
114220                                   MOD-IDAVD-DAG-IN-ATTR                  
114230                                   MOD-IDGRUPP-DAG-IN-ATTR                
114240                                   MOD-IDAVD-NATT-IN-ATTR                 
114250                                   MOD-IDGRUPP-NATT-IN-ATTR               
114300                                   MOD-FLLOLL-IN-ATTR                     
114400                                   MOD-FLCDOMR-IN-ATTR                    
114500     .                                                                    
114600     EJECT                                                                
114700 CB-FLYTTA-DATA SECTION.                                                  
114800          IF INSERT-JA                                                    
114900            MOVE SPACE TO 6006-W6GX6006                                   
115000            MOVE ZERO  TO 6006-ADGANG-FOM                                 
115100                          6006-ADGANG-TOM                                 
115200                          6006-ADPLATS-FOM                                
115300                          6006-ADPLATS-TOM                                
115400                          6006-KDLORAPP                                   
115500                          6006-IDPERSON-ANSV                              
115600                          6006-IDPERSON-FORP                              
115700                          6006-IDPERSON-KVAL                              
115710                          6006-KVTID-NORM                                 
115720                          6006-KVTID-PRIO                                 
115800          END-IF                                                          
115900                                                                          
116000          MOVE WS-ADINLOMR         TO 6006-ADINLOMR                       
116100          MOVE LOW-VALUE           TO 6006-LOW-VALUE                      
116200          IF MID-ADGANG-FOM = ALL '+'                                     
116300             MOVE MFS-ROER-EJ-FAELT TO MOD-ADGANG-FOM-UT                  
116400          ELSE                                                            
116500             MOVE MID-ADGANG-FOM TO 6006-ADGANG-FOM                       
116600             MOVE MID-ADGANG-FOM TO MOD-ADGANG-FOM-UT                     
116700          END-IF                                                          
116800                                                                          
116900          IF MID-ADGANG-TOM = ALL '+'                                     
117000             MOVE MFS-ROER-EJ-FAELT TO MOD-ADGANG-TOM-UT                  
117100          ELSE                                                            
117200             MOVE MID-ADGANG-TOM TO 6006-ADGANG-TOM                       
117300             MOVE MID-ADGANG-TOM TO MOD-ADGANG-TOM-UT                     
117400          END-IF                                                          
117500                                                                          
117600          IF MID-ADINLOMR-BO = ALL '+'                                    
117700             MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-BO-UT                 
117800          ELSE                                                            
117900             MOVE MID-ADINLOMR-BO TO 6006-ADINLOMR-BO                     
118000             MOVE MID-ADINLOMR-BO TO MOD-ADINLOMR-BO-UT                   
118100          END-IF                                                          
118200                                                                          
118300          IF MID-ADINLOMR-LPL = ALL '+'                                   
118400             MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-LPL-UT                
118500          ELSE                                                            
118600             MOVE MID-ADINLOMR-LPL TO 6006-ADINLOMR-LPL                   
118700             MOVE MID-ADINLOMR-LPL TO MOD-ADINLOMR-LPL-UT                 
118800          END-IF                                                          
118900                                                                          
119000          IF MID-ADINLOMR-PAR = ALL '+'                                   
119100             MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PAR-UT                
119200          ELSE                                                            
119300             MOVE MID-ADINLOMR-PAR TO 6006-ADINLOMR-PAR                   
119400             MOVE MID-ADINLOMR-PAR TO MOD-ADINLOMR-PAR-UT                 
119500          END-IF                                                          
119600                                                                          
119700          IF MID-ADINLOMR-PRT =  ALL '+'                                  
119800             MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PRT-UT                
119900          ELSE                                                            
120000             MOVE MID-ADINLOMR-PRT TO 6006-ADINLOMR-PRT                   
120100             MOVE MID-ADINLOMR-PRT TO MOD-ADINLOMR-PRT-UT                 
120200          END-IF                                                          
120300                                                                          
120400          IF MID-ADPLATS-FOM = ALL '+'                                    
120500             MOVE MFS-ROER-EJ-FAELT TO MOD-ADPLATS-FOM-UT                 
120600          ELSE                                                            
120700             MOVE MID-ADPLATS-FOM TO 6006-ADPLATS-FOM                     
120800             MOVE MID-ADPLATS-FOM TO MOD-ADPLATS-FOM-UT                   
120900          END-IF                                                          
121000                                                                          
121100          IF MID-ADPLATS-TOM = ALL '+'                                    
121200             MOVE MFS-ROER-EJ-FAELT TO MOD-ADPLATS-TOM-UT                 
121300          ELSE                                                            
121400             MOVE MID-ADPLATS-TOM TO 6006-ADPLATS-TOM                     
121500             MOVE MID-ADPLATS-TOM TO MOD-ADPLATS-TOM-UT                   
121600          END-IF                                                          
121700                                                                          
121800                                                                          
121900          IF MID-FLKVARED = ALL '+'                                       
122000             MOVE MFS-ROER-EJ-FAELT TO MOD-FLKVARED-UT                    
122100          ELSE                                                            
122200             MOVE MID-FLKVARED TO 6006-FLKVARED                           
122300             MOVE MID-FLKVARED TO MOD-FLKVARED-UT                         
122400          END-IF                                                          
122500                                                                          
122600          IF MID-FLKNTRGK = ALL '+'                                       
122700             MOVE MFS-ROER-EJ-FAELT TO MOD-FLKNTRGK-UT                    
122800          ELSE                                                            
122900             MOVE MID-FLKNTRGK TO 6006-FLKNTRGK                           
123000             MOVE MID-FLKNTRGK TO MOD-FLKNTRGK-UT                         
123100          END-IF                                                          
123200                                                                          
123210          IF MID-FLEXCP   = ALL '+'                                       
123220             MOVE MFS-ROER-EJ-FAELT TO MOD-FLEXCP-UT                      
123230          ELSE                                                            
123231             IF MID-FLEXCP = 'Y'                                          
123232                MOVE 'J'          TO 6006-FLEXCP                          
123233                MOVE 'J'          TO MOD-FLEXCP-UT                        
123234             ELSE                                                         
123240                MOVE MID-FLEXCP   TO 6006-FLEXCP                          
123241                MOVE MID-FLEXCP   TO MOD-FLEXCP-UT                        
123242             END-IF                                                       
123260          END-IF                                                          
123270                                                                          
123280          IF MID-IDAVD-DAG = ALL '+' OR                                   
123281             MID-IDAVD-DAG = SPACE                                        
123290             MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVD-DAG-UT                   
123291          ELSE                                                            
123292             MOVE MID-IDAVD-DAG TO 6006-IDAVD-DAG                         
123293             MOVE MID-IDAVD-DAG TO MOD-IDAVD-DAG-UT                       
123294          END-IF                                                          
123295                                                                          
123296          IF MID-IDGRUPP-DAG = ALL '+' OR                                 
123297             MID-IDGRUPP-DAG = SPACE                                      
123298             MOVE MFS-ROER-EJ-FAELT TO MOD-IDGRUPP-DAG-UT                 
123299          ELSE                                                            
123300             MOVE MID-IDGRUPP-DAG TO 6006-IDGRUPP-DAG                     
123301             MOVE MID-IDGRUPP-DAG TO MOD-IDGRUPP-DAG-UT                   
123302          END-IF                                                          
123303                                                                          
123304          IF MID-IDAVD-NATT = ALL '+' OR                                  
123305             MID-IDAVD-NATT = SPACE                                       
123306             MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVD-NATT-UT                  
123307          ELSE                                                            
123308             MOVE MID-IDAVD-NATT TO 6006-IDAVD-NATT                       
123309             MOVE MID-IDAVD-NATT TO MOD-IDAVD-NATT-UT                     
123310          END-IF                                                          
123311                                                                          
123312          IF MID-IDGRUPP-NATT = ALL '+' OR                                
123313             MID-IDGRUPP-NATT = SPACE                                     
123314             MOVE MFS-ROER-EJ-FAELT TO MOD-IDGRUPP-NATT-UT                
123315          ELSE                                                            
123316             MOVE MID-IDGRUPP-NATT TO 6006-IDGRUPP-NATT                   
123317             MOVE MID-IDGRUPP-NATT TO MOD-IDGRUPP-NATT-UT                 
123318          END-IF                                                          
123319                                                                          
123320          IF MID-FLLOLL = ALL '+'                                         
123400             MOVE MFS-ROER-EJ-FAELT TO MOD-FLLOLL-UT                      
123500          ELSE                                                            
123600             MOVE MID-FLLOLL TO 6006-FLLOLL                               
123700             MOVE MID-FLLOLL TO MOD-FLLOLL-UT                             
123800          END-IF                                                          
123900                                                                          
124000          IF MID-FLCDOMR = ALL '+'                                        
124100             MOVE MFS-ROER-EJ-FAELT TO MOD-FLCDOMR-UT                     
124200          ELSE                                                            
124300             MOVE MID-FLCDOMR TO 6006-FLCDOMR                             
124400             MOVE MID-FLCDOMR TO MOD-FLCDOMR-UT                           
124500          END-IF                                                          
124600                                                                          
124700          IF MID-IDLEVNR = ALL '+'                                        
124800             MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-UT                     
124900          ELSE                                                            
125000             MOVE MID-IDLEVNR TO 6006-IDLEVNR                             
125100             MOVE MID-IDLEVNR TO MOD-IDLEVNR-UT                           
125200          END-IF                                                          
125300                                                                          
125400          MOVE WS-KDINLOMR         TO 6006-KDINLOMR                       
125500          IF 6006-KDINLOMR = 'SQ '                                        
125600            MOVE 'RTA'      TO 6006-KDINLOMR                              
125700          END-IF                                                          
125800          IF 6006-KDINLOMR = 'UNL'                                        
125900            MOVE 'LPL'      TO 6006-KDINLOMR                              
126000          END-IF                                                          
126100          IF 6006-KDINLOMR = 'PG '                                        
126200            MOVE 'FB '      TO 6006-KDINLOMR                              
126300          END-IF                                                          
126400          IF 6006-KDINLOMR = 'PGP'                                        
126500            MOVE 'FBP'      TO 6006-KDINLOMR                              
126600          END-IF                                                          
126700          IF 6006-KDINLOMR = 'P  '                                        
126800            MOVE 'F  '      TO 6006-KDINLOMR                              
126900          END-IF                                                          
127000                                                                          
127100          IF MID-KDINLUPF = ALL '+'                                       
127200             MOVE MFS-ROER-EJ-FAELT TO MOD-KDINLUPF-UT                    
127300          ELSE                                                            
127400             MOVE MID-KDINLUPF TO 6006-KDINLUPF                           
127500             MOVE MID-KDINLUPF TO MOD-KDINLUPF-UT                         
127600          END-IF                                                          
127700                                                                          
127800          IF MID-KDLORAPP = ALL '+'                                       
127900             MOVE MFS-ROER-EJ-FAELT TO MOD-KDLORAPP-UT                    
128000          ELSE                                                            
128100             MOVE MID-KDLORAPP TO 6006-KDLORAPP                           
128200             MOVE MID-KDLORAPP TO MOD-KDLORAPP-UT                         
128300             IF 6006-KDLORAPP = 0                                         
128400               MOVE SPACE               TO MOD-TEXT1-UT                   
128500             END-IF                                                       
128600             IF 6006-KDLORAPP = 1                                         
128700               MOVE 'NORMALFLÖDE      ' TO MOD-TEXT1-UT                   
128800             END-IF                                                       
128900             IF 6006-KDLORAPP = 2                                         
129000               MOVE 'AR SKA TAS UT    ' TO MOD-TEXT1-UT                   
129100             END-IF                                                       
129200             IF 6006-KDLORAPP = 3                                         
129300               MOVE 'I-LISTA VID MOTT ' TO MOD-TEXT1-UT                   
129400             END-IF                                                       
129500             IF 6006-KDLORAPP = 4                                         
129600               MOVE 'R32 VID LOSSNING ' TO MOD-TEXT1-UT                   
129700             END-IF                                                       
129800             IF 6006-KDLORAPP = 5                                         
129900               MOVE 'NO PRINT OF RR   ' TO MOD-TEXT1-UT                   
130000             END-IF                                                       
130100          END-IF                                                          
130200                                                                          
130300          IF MID-IDPERSON-ANSV = ALL '+'                                  
130400            MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON-ANSV-UT                
130500                                      MOD-BEINIT-ANSV                     
130600          ELSE                                                            
130700            MOVE MID-IDPERSON-ANSV TO 6006-IDPERSON-ANSV                  
130800                                      MOD-IDPERSON-ANSV-UT                
130900          END-IF                                                          
131000                                                                          
131100          IF MID-IDPERSON-FORP = ALL '+'                                  
131200            MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON-FORP-UT                
131300                                      MOD-BEINIT-FORP                     
131400          ELSE                                                            
131500            MOVE MID-IDPERSON-FORP TO 6006-IDPERSON-FORP                  
131600                                      MOD-IDPERSON-FORP-UT                
131700          END-IF                                                          
131800                                                                          
131900          IF MID-IDPERSON-KVAL = ALL '+'                                  
132000            MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON-KVAL-UT                
132100                                      MOD-BEINIT-KVAL                     
132200          ELSE                                                            
132300            MOVE MID-IDPERSON-KVAL TO 6006-IDPERSON-KVAL                  
132400                                      MOD-IDPERSON-KVAL-UT                
132500          END-IF                                                          
132600                                                                          
132700          MOVE NEJ                 TO MOD-FLSVAR-UT                       
132701                                                                          
132702*    --- PLACEMENT LEAD TIME SHOULD BE STORED IN WDGX130 (6006)           
132710     IF MID-KVTID-NORM    = ALL '+'                                       
132720       MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-NORM-UT                        
132730     ELSE                                                                 
132732       MOVE MID-KVTID-NORM    TO 6006-KVTID-NORM                          
132733                                 MOD-KVTID-NORM-UT                        
132740     END-IF                                                               
132750                                                                          
132760     IF MID-KVTID-PRIO    = ALL '+'                                       
132770       MOVE MFS-ROER-EJ-FAELT TO MOD-KVTID-PRIO-UT                        
132780     ELSE                                                                 
132790       MOVE MID-KVTID-PRIO    TO 6006-KVTID-PRIO                          
132791                                 MOD-KVTID-PRIO-UT                        
132792     END-IF                                                               
132793                                                                          
132794*    --- TOTAL LEAD TIME IS HANDLED LAST IN C-SECTION                     
132810     .                                                                    
132900     EJECT                                                                
133000                                                                          
133100 D-DELETE SECTION.                                                        
133200     MOVE JA TO DELETE-WS                                                 
133300     IF MID-INPUT NOT = ALL '+'                                           
133400       PERFORM DB-INMATN-IFYLLT                                           
133500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
133600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
133700       MOVE ERR-KONFLIKT TO MED-IDMFSFEL                                  
133800       CALL WMEDKONV USING MED-WMEDAREA                                   
133900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
134000     ELSE                                                                 
134100       PERFORM DA-KONTROLLERA-DELETE                                      
134200                                                                          
134300       IF DELETE-OK                                                       
134400          MOVE WS-ADINLOMR TO W-6006-ADINLOMR                             
134500          PERFORM IMS-GHU-W6G130                                          
134600          IF SEGMENT-FINNS                                                
134700            PERFORM IMS-DELETE-W6G130                                     
134800            PERFORM MFS-FORM-ATTR                                         
134900            PERFORM MFS-RENSA-FAELT-IN                                    
135000            PERFORM S02-OPPNA-FAELT                                       
135100            MOVE INF-UPDATE-DONE TO MED-IDMFSINF                          
135200            CALL WMEDKONV USING MED-WMEDAREA                              
135300            MOVE MED-MFSINF TO MOD-TEMFSINF                               
135400          END-IF                                                          
135500       ELSE                                                               
135600            MOVE ERR-OTILL-UPPDAT TO MED-IDMFSFEL                         
135700            CALL WMEDKONV USING MED-WMEDAREA                              
135800            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
135900            PERFORM MFS-FORM-ATTR                                         
136000            PERFORM MFS-RENSA-FAELT-IN                                    
136100            PERFORM MFS-ROER-EJ-FAELT-UT                                  
136200            PERFORM S02-OPPNA-FAELT                                       
136300       END-IF                                                             
136400                                                                          
136500     END-IF                                                               
136600     .                                                                    
136700     EJECT                                                                
136800 DA-KONTROLLERA-DELETE SECTION.                                           
136900                                                                          
137000     IF MID-KDINLOMR-UT = 'SYS' AND NOT MFS-UPD-V                         
137100        MOVE NEJ TO DELETE-WS                                             
137200     ELSE                                                                 
137300        CONTINUE                                                          
137400     END-IF                                                               
137500                                                                          
137600     PERFORM IMS-GU-W6G101                                                
137700     IF SEGMENT-FINNS                                                     
137800       MOVE +0 TO IX-TRAEFF1                                              
137900       PERFORM UNTIL IX-TRAEFF1 = 2 OR SEGMENT-SAKNAS                     
138000        MOVE WS-ADINLOMR TO W-ADINLOMR-PAR                                
138100        PERFORM IMS-GNP-W6G130-PAR                                        
138200        IF SEGMENT-FINNS                                                  
138300          ADD +1 TO IX-TRAEFF1                                            
138400        END-IF                                                            
138500       END-PERFORM                                                        
138600       IF IX-TRAEFF1 = +1 OR SEGMENT-SAKNAS                               
138700         CONTINUE                                                         
138800       ELSE                                                               
138900         MOVE NEJ TO DELETE-WS                                            
139000       END-IF                                                             
139100     END-IF                                                               
139200                                                                          
139300     PERFORM IMS-GU-W6G101                                                
139400     IF SEGMENT-FINNS                                                     
139500       MOVE +0 TO IX-TRAEFF2                                              
139600       PERFORM UNTIL IX-TRAEFF2 = 2 OR SEGMENT-SAKNAS                     
139700        MOVE WS-ADINLOMR TO W-ADINLOMR-LPL                                
139800        PERFORM IMS-GNP-W6G130-LPL                                        
139900        IF SEGMENT-FINNS                                                  
140000         ADD +1 TO IX-TRAEFF2                                             
140100        END-IF                                                            
140200       END-PERFORM                                                        
140300       IF IX-TRAEFF2 = +1 OR SEGMENT-SAKNAS                               
140400         CONTINUE                                                         
140500       ELSE                                                               
140600         MOVE NEJ TO DELETE-WS                                            
140700       END-IF                                                             
140800     END-IF                                                               
140900                                                                          
141000     PERFORM IMS-GU-W6G101                                                
141100     IF SEGMENT-FINNS                                                     
141200       MOVE +0 TO IX-TRAEFF3                                              
141300       PERFORM UNTIL IX-TRAEFF3 = 2 OR SEGMENT-SAKNAS                     
141400         MOVE WS-ADINLOMR TO W-ADINLOMR-BO                                
141500         PERFORM IMS-GNP-W6G130-BO                                        
141600         IF SEGMENT-FINNS                                                 
141700           ADD +1 TO IX-TRAEFF3                                           
141800         END-IF                                                           
141900       END-PERFORM                                                        
142000       IF IX-TRAEFF3 = +1 OR SEGMENT-SAKNAS                               
142100         CONTINUE                                                         
142200       ELSE                                                               
142300         MOVE NEJ TO DELETE-WS                                            
142400       END-IF                                                             
142500     END-IF                                                               
142600                                                                          
142700     MOVE WS-ADINLOMR TO W-ADINLOMR                                       
142800     PERFORM IMS-GET-HANB01                                               
142900     IF SEGMENT-FINNS                                                     
143000       MOVE NEJ  TO TRAEFF-SW                                             
143100       PERFORM IMS-GNP-HANB11                                             
143200       IF SEGMENT-FINNS                                                   
143300         MOVE JA  TO TRAEFF-SW                                            
143400       ELSE                                                               
143500         PERFORM IMS-GNP-HANB12                                           
143600         IF SEGMENT-FINNS                                                 
143700           MOVE JA  TO TRAEFF-SW                                          
143800         ELSE                                                             
143900           PERFORM IMS-GNP-HANB13                                         
144000           IF SEGMENT-FINNS                                               
144100             MOVE JA  TO TRAEFF-SW                                        
144200           ELSE                                                           
144300             PERFORM IMS-GNP-HANB14                                       
144400             IF SEGMENT-FINNS                                             
144500               MOVE JA  TO TRAEFF-SW                                      
144600             END-IF                                                       
144700           END-IF                                                         
144800         END-IF                                                           
144900       END-IF                                                             
145000       IF TRAEFF-NEJ                                                      
145100           CONTINUE                                                       
145200       ELSE                                                               
145300           MOVE NEJ TO DELETE-WS                                          
145400       END-IF                                                             
145500     END-IF                                                               
145600     MOVE WS-ADINLOMR TO W-ADINLOMR                                       
145700     PERFORM IMS-GET-INLA11-ESEQ                                          
145800** HÄR ÄR DEN MEST TROLIGA ORSAKEN TILL ATT DET INTE GÅR ATT              
145900** DELETE:A EN PLACERING. 21-SEGMENT MED STATUS 'KVA' KOMMER              
146000** INTE ATT OMFATTAS EXITEN.                                              
146100      IF SEGMENT-FINNS                                                    
146200         MOVE NEJ TO DELETE-WS                                            
146300      ELSE                                                                
146400         CONTINUE                                                         
146500      END-IF                                                              
146600                                                                          
146700     MOVE WS-ADINLOMR TO W-ADINLOMN                                       
146800     PERFORM IMS-GET-INLA11-GSEQ                                          
146900     IF SEGMENT-FINNS                                                     
147000        MOVE NEJ TO DELETE-WS                                             
147100     ELSE                                                                 
147200        CONTINUE                                                          
147300     END-IF                                                               
147400     .                                                                    
147500     EJECT                                                                
147600 DB-INMATN-IFYLLT SECTION.                                                
147700       MOVE MFS-ALFA-FAELT-RAETT TO  MOD-FLSVAR-IN-ATTR                   
147800       IF MID-KDINLOMR-UT = 'LO'                                          
147900         PERFORM DBA-TYP-LO                                               
148000       END-IF                                                             
148100         IF MID-KDINLOMR-UT = 'FB' OR 'FBP' OR 'F' OR                     
148200                              'PG' OR 'PGP' OR 'P'                        
148300           PERFORM DBB-TYP-FB                                             
148400         END-IF                                                           
148500           IF MID-KDINLOMR-UT = 'TRG'                                     
148600             PERFORM DBC-TYP-TRG                                          
148700           END-IF                                                         
148800             IF MID-KDINLOMR-UT = 'BO'                                    
148900               PERFORM DBD-TYP-BO                                         
149000             END-IF                                                       
149100               IF MID-KDINLOMR-UT = 'RTA' OR 'SQ '                        
149200                 PERFORM DBE-TYP-RTA                                      
149300               END-IF                                                     
149400                 IF MID-KDINLOMR-UT = 'LPL' OR 'UNL'                      
149500                   PERFORM DBF-TYP-LPL                                    
149600                 END-IF                                                   
149700     .                                                                    
149800     EJECT                                                                
149900 DBA-TYP-LO SECTION.                                                      
150000       IF MID-ADINLOMR-PAR NOT = ALL '+'                                  
150100            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR           
150200       ELSE                                                               
150300          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
150400       END-IF                                                             
150500                                                                          
150600       IF MID-KDINLUPF NOT = ALL '+'                                      
150700            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDINLUPF-IN-ATTR               
150800       ELSE                                                               
150900          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDINLUPF-IN-ATTR               
151000       END-IF                                                             
151100                                                                          
151200       IF MID-ADINLOMR-BO NOT = ALL '+' AND SPACE                         
151300          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-BO-IN-ATTR              
151400       ELSE                                                               
151500          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-BO-IN-ATTR            
151600       END-IF                                                             
151700                                                                          
151800       IF MID-ADINLOMR-LPL NOT = ALL '+'                                  
151900          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR             
152000       ELSE                                                               
152100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR           
152200       END-IF                                                             
152300                                                                          
152400       IF MID-KDLORAPP NOT = ALL '+'                                      
152500            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDLORAPP-IN-ATTR               
152600       ELSE                                                               
152700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDLORAPP-IN-ATTR               
152800       END-IF                                                             
152900                                                                          
153000       IF MID-TEXT1 NOT = ALL '+'                                         
153100          MOVE MFS-ALFA-FAELT-FEL TO MOD-TEXT1-IN-ATTR                    
153200       ELSE                                                               
153300          MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEXT1-IN-ATTR                  
153400       END-IF                                                             
153500                                                                          
153600       IF MID-FLLOLL NOT = ALL '+'                                        
153700          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLOLL-IN-ATTR                   
153800       ELSE                                                               
153900          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLOLL-IN-ATTR                 
154000       END-IF                                                             
154100                                                                          
154200       IF MID-FLCDOMR NOT = ALL '+'                                       
154300          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLCDOMR-IN-ATTR                  
154400       ELSE                                                               
154500          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLCDOMR-IN-ATTR                
154600       END-IF                                                             
154700                                                                          
154800       IF MID-ADINLOMR-PRT NOT = ALL '+'                                  
154900            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-IN-ATTR           
155000       ELSE                                                               
155100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-IN-ATTR           
155200       END-IF                                                             
155300                                                                          
155400       IF MID-IDLEVNR NOT = ALL '+'                                       
155500          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                  
155600       ELSE                                                               
155700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
155800       END-IF                                                             
155900                                                                          
156000       IF MID-IDPERSON-ANSV NOT = ALL '+'                                 
156100          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-ANSV-IN-ATTR            
156200       ELSE                                                               
156300          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ANSV-IN-ATTR          
156400       END-IF                                                             
156500                                                                          
156600     .                                                                    
156700     EJECT                                                                
156800 DBB-TYP-FB SECTION.                                                      
156900       IF MID-ADINLOMR-PAR NOT = ALL '+'                                  
157000          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR             
157100       ELSE                                                               
157200          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
157300       END-IF                                                             
157400                                                                          
157500       IF MID-KDINLUPF NOT = ALL '+'                                      
157600            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDINLUPF-IN-ATTR               
157700       ELSE                                                               
157800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDINLUPF-IN-ATTR               
157900       END-IF                                                             
158000                                                                          
158100       IF MID-ADINLOMR-LPL NOT = ALL '+'                                  
158200            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR           
158300       ELSE                                                               
158400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR           
158500       END-IF                                                             
158600                                                                          
158700       IF MID-FLKVARED NOT = ALL '+'                                      
158800          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVARED-IN-ATTR                 
158900       ELSE                                                               
159000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKVARED-IN-ATTR               
159100       END-IF                                                             
159200                                                                          
159300       IF MID-FLKNTRGK NOT = ALL '+'                                      
159400          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKNTRGK-IN-ATTR                 
159500       ELSE                                                               
159600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKNTRGK-IN-ATTR               
159700       END-IF                                                             
159800                                                                          
159810       IF MID-FLEXCP   NOT = ALL '+'                                      
159820          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP-IN-ATTR                   
159830       ELSE                                                               
159840          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP-IN-ATTR                 
159850       END-IF                                                             
159860                                                                          
159870       IF MID-IDAVD-DAG NOT = ALL '+'                                     
159880          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAVD-DAG-IN-ATTR                
159890       ELSE                                                               
159891          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAVD-DAG-IN-ATTR              
159892       END-IF                                                             
159893                                                                          
159894       IF MID-IDGRUPP-DAG NOT = ALL '+'                                   
159895          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDGRUPP-DAG-IN-ATTR              
159896       ELSE                                                               
159897          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDGRUPP-DAG-IN-ATTR            
159898       END-IF                                                             
159899                                                                          
159900       IF MID-IDAVD-NATT NOT = ALL '+'                                    
159901          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAVD-NATT-IN-ATTR               
159902       ELSE                                                               
159903          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAVD-NATT-IN-ATTR             
159904       END-IF                                                             
159905                                                                          
159906       IF MID-IDGRUPP-NATT NOT = ALL '+'                                  
159907          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDGRUPP-NATT-IN-ATTR             
159908       ELSE                                                               
159909          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDGRUPP-NATT-IN-ATTR           
159910       END-IF                                                             
159911                                                                          
159920       IF MID-IDLEVNR NOT = ALL '+'                                       
160000          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                  
160100       ELSE                                                               
160200          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
160300       END-IF                                                             
160400                                                                          
160500       IF MID-IDPERSON-ANSV NOT = ALL '+'                                 
160600          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-ANSV-IN-ATTR            
160700       ELSE                                                               
160800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ANSV-IN-ATTR          
160900       END-IF                                                             
161000                                                                          
161100       IF MID-IDPERSON-FORP NOT = ALL '+'                                 
161200          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-FORP-IN-ATTR            
161300       ELSE                                                               
161400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-FORP-IN-ATTR          
161500       END-IF                                                             
161600                                                                          
161700       IF MID-IDPERSON-KVAL NOT = ALL '+'                                 
161800          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-KVAL-IN-ATTR            
161900       ELSE                                                               
162000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-KVAL-IN-ATTR          
162100       END-IF                                                             
162200     .                                                                    
162300     EJECT                                                                
162400 DBC-TYP-TRG SECTION.                                                     
162500       IF MID-ADINLOMR-PAR NOT = ALL '+'                                  
162600          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR             
162700       ELSE                                                               
162800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
162900       END-IF                                                             
163000                                                                          
163100       IF MID-ADINLOMR-BO NOT = ALL '+' AND SPACE                         
163200          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR             
163300       ELSE                                                               
163400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-BO-IN-ATTR            
163500       END-IF                                                             
163600                                                                          
163700       IF MID-KDINLUPF NOT = ALL '+'                                      
163800            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDINLUPF-IN-ATTR               
163900       ELSE                                                               
164000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDINLUPF-IN-ATTR               
164100       END-IF                                                             
164200                                                                          
164300       IF MID-ADPLATS-FOM NOT = ALL '+'                                   
164400            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-FOM-IN-ATTR            
164500       ELSE                                                               
164600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-FOM-IN-ATTR            
164700       END-IF                                                             
164800                                                                          
164900       IF MID-ADPLATS-TOM NOT = ALL '+'                                   
165000          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-TOM-IN-ATTR              
165100       ELSE                                                               
165200          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-TOM-IN-ATTR            
165300       END-IF                                                             
165400                                                                          
165500       IF MID-ADGANG-FOM NOT = ALL '+'                                    
165600          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-FOM-IN-ATTR               
165700       ELSE                                                               
165800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-FOM-IN-ATTR             
165900       END-IF                                                             
166000                                                                          
166100       IF MID-ADGANG-TOM NOT = ALL '+'                                    
166200          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-TOM-IN-ATTR               
166300       ELSE                                                               
166400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-TOM-IN-ATTR             
166500       END-IF                                                             
166600                                                                          
166700       IF MID-IDLEVNR NOT = ALL '+'                                       
166800          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                  
166900       ELSE                                                               
167000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
167100       END-IF                                                             
167200     .                                                                    
167300     EJECT                                                                
167400 DBD-TYP-BO SECTION.                                                      
167500       IF MID-ADINLOMR-PAR NOT = ALL '+'                                  
167600          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR             
167700       ELSE                                                               
167800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
167900       END-IF                                                             
168000                                                                          
168100       IF MID-ADINLOMR-LPL NOT = ALL '+'                                  
168200          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-LPL-IN-ATTR             
168300       ELSE                                                               
168400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-LPL-IN-ATTR           
168500       END-IF                                                             
168600                                                                          
168700       IF MID-KDINLUPF NOT = ALL '+'                                      
168800          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDINLUPF-IN-ATTR                 
168900       ELSE                                                               
169000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDINLUPF-IN-ATTR               
169100       END-IF                                                             
169200                                                                          
169300       IF MID-IDLEVNR NOT = ALL '+'                                       
169400          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                  
169500       ELSE                                                               
169600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
169700       END-IF                                                             
169800                                                                          
169900     .                                                                    
170000     EJECT                                                                
170100 DBE-TYP-RTA SECTION.                                                     
170200       IF MID-ADINLOMR-PAR NOT = ALL '+'                                  
170300          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR             
170400       ELSE                                                               
170500          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
170600       END-IF                                                             
170700                                                                          
170800       IF MID-KDINLUPF NOT = ALL '+'                                      
170900          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDINLUPF-IN-ATTR                 
171000       ELSE                                                               
171100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDINLUPF-IN-ATTR               
171200       END-IF                                                             
171300                                                                          
171400       IF MID-IDLEVNR NOT = ALL '+'                                       
171500          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                  
171600       ELSE                                                               
171700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
171800       END-IF                                                             
171900                                                                          
172000     .                                                                    
172100     EJECT                                                                
172200 DBF-TYP-LPL SECTION.                                                     
172300       IF MID-ADINLOMR-PAR NOT = ALL '+'                                  
172400            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PAR-IN-ATTR           
172500       ELSE                                                               
172600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PAR-IN-ATTR           
172700       END-IF                                                             
172800                                                                          
172900       IF MID-KDINLUPF NOT = ALL '+'                                      
173000          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDINLUPF-IN-ATTR                 
173100       ELSE                                                               
173200          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDINLUPF-IN-ATTR               
173300       END-IF                                                             
173400                                                                          
173500       IF MID-IDLEVNR NOT = ALL '+'                                       
173600          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                  
173700       ELSE                                                               
173800          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                
173900       END-IF                                                             
174000     .                                                                    
174100     EJECT                                                                
174200 E-LAES-VISA-INFO SECTION.                                                
174300       PERFORM MFS-FORM-ATTR                                              
174400       MOVE WS-ADINLOMR TO W-6006-ADINLOMR                                
174500       PERFORM IMS-GU-W6G130                                              
174600       IF SEGMENT-FINNS                                                   
174700          MOVE 6006-KDINLUPF        TO MOD-KDINLUPF-UT                    
174800          MOVE 6006-KDINLOMR        TO MOD-KDINLOMR-UT                    
174900          IF ENGLISH-TEXT                                                 
175000            IF MOD-KDINLOMR-UT = 'RTA'                                    
175100              MOVE 'SQ '            TO MOD-KDINLOMR-UT                    
175200            END-IF                                                        
175300            IF MOD-KDINLOMR-UT = 'LPL'                                    
175400              MOVE 'UNL'            TO MOD-KDINLOMR-UT                    
175500            END-IF                                                        
175600            IF MOD-KDINLOMR-UT = 'FBP'                                    
175700              MOVE 'PGP'            TO MOD-KDINLOMR-UT                    
175800            END-IF                                                        
175900            IF MOD-KDINLOMR-UT = 'FB '                                    
176000              MOVE 'PG '            TO MOD-KDINLOMR-UT                    
176100            END-IF                                                        
176200            IF MOD-KDINLOMR-UT = 'F  '                                    
176300              MOVE 'P  '            TO MOD-KDINLOMR-UT                    
176400            END-IF                                                        
176500          END-IF                                                          
176600          MOVE 6006-ADINLOMR-BO     TO MOD-ADINLOMR-BO-UT                 
176700          MOVE 6006-ADINLOMR-LPL    TO MOD-ADINLOMR-LPL-UT                
176800          MOVE 6006-ADINLOMR-PAR    TO MOD-ADINLOMR-PAR-UT                
176900          MOVE 6006-ADINLOMR-PRT    TO MOD-ADINLOMR-PRT-UT                
177000          MOVE 6006-KDLORAPP        TO MOD-KDLORAPP-UT                    
177100          IF 6006-KDLORAPP = 0                                            
177200            MOVE SPACE               TO MOD-TEXT1-UT                      
177300          END-IF                                                          
177400          IF 6006-KDLORAPP = 1                                            
177500            MOVE 'NORMALFLÖDE      ' TO MOD-TEXT1-UT                      
177600          END-IF                                                          
177700          IF 6006-KDLORAPP = 2                                            
177800            MOVE 'AR SKA TAS UT    ' TO MOD-TEXT1-UT                      
177900          END-IF                                                          
178000          IF 6006-KDLORAPP = 3                                            
178100            MOVE 'I-LISTA VID MOTT ' TO MOD-TEXT1-UT                      
178200          END-IF                                                          
178300          IF 6006-KDLORAPP = 4                                            
178400            MOVE 'R32 VID LOSSNING ' TO MOD-TEXT1-UT                      
178500          END-IF                                                          
178600          IF 6006-KDLORAPP = 5                                            
178700            MOVE 'NO PRINT OF RR   ' TO MOD-TEXT1-UT                      
178800          END-IF                                                          
178900          MOVE 6006-ADPLATS-FOM     TO MOD-ADPLATS-FOM-UT                 
179000          MOVE 6006-ADPLATS-TOM     TO MOD-ADPLATS-TOM-UT                 
179100          MOVE 6006-ADGANG-FOM      TO MOD-ADGANG-FOM-UT                  
179200          MOVE 6006-ADGANG-TOM      TO MOD-ADGANG-TOM-UT                  
179300          MOVE 6006-FLLOLL          TO MOD-FLLOLL-UT                      
179400          MOVE 6006-FLCDOMR         TO MOD-FLCDOMR-UT                     
179500          MOVE 6006-IDLEVNR         TO MOD-IDLEVNR-UT                     
179600          MOVE 6006-FLKVARED        TO MOD-FLKVARED-UT                    
179700          MOVE 6006-FLKNTRGK        TO MOD-FLKNTRGK-UT                    
179710          MOVE 6006-FLEXCP          TO MOD-FLEXCP-UT                      
179720          MOVE 6006-IDAVD-DAG       TO MOD-IDAVD-DAG-UT                   
179730          MOVE 6006-IDGRUPP-DAG     TO MOD-IDGRUPP-DAG-UT                 
179740          MOVE 6006-IDAVD-NATT      TO MOD-IDAVD-NATT-UT                  
179750          MOVE 6006-IDGRUPP-NATT    TO MOD-IDGRUPP-NATT-UT                
179800*                                                                         
179900          MOVE 6006-KVTID-NORM      TO MOD-KVTID-NORM-UT                  
180000          MOVE 6006-KVTID-PRIO      TO MOD-KVTID-PRIO-UT                  
180100*         --- TOTAL LEAD TIMES IS READ LAST IN THIS SECTION               
181100                                                                          
181200          MOVE NEJ                  TO MOD-FLSVAR-UT                      
181300          IF 6006-IDPERSON-ANSV > +0                                      
181400            MOVE 6006-IDPERSON-ANSV TO MOD-IDPERSON-ANSV-UT               
181500                                       W-IDPERSON                         
181600            PERFORM IMS-GU-WDP311                                         
181700            IF SEGMENT-FINNS                                              
181800              MOVE PERS-BEINIT TO MOD-BEINIT-ANSV                         
181900            ELSE                                                          
182000              MOVE SPACE       TO MOD-BEINIT-ANSV                         
182100            END-IF                                                        
182200          ELSE                                                            
182300            MOVE ZERO          TO MOD-IDPERSON-ANSV-IN                    
182400                                  MOD-IDPERSON-ANSV-UT                    
182500            MOVE SPACE         TO MOD-BEINIT-ANSV                         
182600          END-IF                                                          
182700          IF 6006-IDPERSON-FORP > +0                                      
182800            MOVE 6006-IDPERSON-FORP TO MOD-IDPERSON-FORP-UT               
182900                                       W-IDPERSON                         
183000            PERFORM IMS-GU-WDP311                                         
183100            IF SEGMENT-FINNS                                              
183200              MOVE PERS-BEINIT TO MOD-BEINIT-FORP                         
183300            ELSE                                                          
183400              MOVE SPACE       TO MOD-BEINIT-FORP                         
183500            END-IF                                                        
183600          ELSE                                                            
183700            MOVE ZERO          TO MOD-IDPERSON-FORP-IN                    
183800                                  MOD-IDPERSON-FORP-UT                    
183900            MOVE SPACE         TO MOD-BEINIT-FORP                         
184000          END-IF                                                          
184100          IF 6006-IDPERSON-KVAL > +0                                      
184200            MOVE 6006-IDPERSON-KVAL TO MOD-IDPERSON-KVAL-UT               
184300                                       W-IDPERSON                         
184400            PERFORM IMS-GU-WDP311-QUAL                                    
184500            IF SEGMENT-FINNS                                              
184600              MOVE PERS-BEINIT TO MOD-BEINIT-KVAL                         
184700            ELSE                                                          
184800              MOVE SPACE       TO MOD-BEINIT-KVAL                         
184900            END-IF                                                        
185000          ELSE                                                            
185100            MOVE ZERO          TO MOD-IDPERSON-KVAL-IN                    
185200                                  MOD-IDPERSON-KVAL-UT                    
185300            MOVE SPACE         TO MOD-BEINIT-KVAL                         
185400          END-IF                                                          
185500                                                                          
185600          PERFORM MFS-LAS-IN-IGEN-UT                                      
185700          PERFORM  EA-OPPNA-FAELT                                         
185800          IF EGEN-MID                                                     
185900            PERFORM MFS-RENSA-FAELT-IN                                    
186000          END-IF                                                          
186001                                                                          
186002*         --- TOTAL VALUES FOR LEAD TIME ALWAYS FOR DC=11                 
186010          MOVE WC-CDC-SE TO W-6005-IDDC                                   
186020          PERFORM IMS-GU-W6GX6008                                         
186030          IF SEGMENT-FINNS                                                
186040            MOVE 6008-KVTID-NORMTOT TO MOD-KVTID-NORMTOT-UT               
186050            MOVE 6008-KVTID-PRIOTOT TO MOD-KVTID-PRIOTOT-UT               
186052            MOVE 6008-KVTID-NTCDC   TO MOD-KVTID-NTCDC-UT                 
186053            MOVE 6008-KVTID-PTCDC   TO MOD-KVTID-PTCDC-UT                 
186054            MOVE 6008-KVTID-NTSVS   TO MOD-KVTID-NTSVS-UT                 
186055            MOVE 6008-KVTID-PTSVS   TO MOD-KVTID-PTSVS-UT                 
186060          ELSE                                                            
186070            MOVE MFS-RENSA-FAELT    TO MOD-KVTID-NORMTOT-UT               
186080                                       MOD-KVTID-PRIOTOT-UT               
186081                                       MOD-KVTID-NTCDC-UT                 
186082                                       MOD-KVTID-PTCDC-UT                 
186083                                       MOD-KVTID-NTSVS-UT                 
186084                                       MOD-KVTID-PTSVS-UT                 
186090          END-IF                                                          
186100       ELSE                                                               
186200         IF ADINLOMR-OK AND KDINLOMR-OK                                   
186300           MOVE MFS-ADD-SAETT-CURSOR TO MOD-ADINLOMR-PAR-IN-ATTR          
186400           PERFORM S03-BEH-STAENGDA-FAELT                                 
186500           PERFORM MFS-LAS-IN-IGEN                                        
186600           PERFORM S02-OPPNA-FAELT                                        
186700         END-IF                                                           
186800         MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                         
186900         CALL WMEDKONV USING MED-WMEDAREA                                 
187000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
187100         PERFORM MFS-RENSA-FAELT-UT                                       
187200       END-IF                                                             
187300                                                                          
187400     .                                                                    
187500     EJECT                                                                
187600 EA-OPPNA-FAELT SECTION.                                                  
187700     IF 6006-KDINLOMR = 'LO'                                              
187800       PERFORM S02A-OPPNA-LO                                              
187900     END-IF                                                               
188000     IF 6006-KDINLOMR = 'FB' OR 'FBP' OR 'F' OR                           
188100                        'PG' OR 'PGP' OR 'P'                              
188200       PERFORM S02B-OPPNA-FB                                              
188300     END-IF                                                               
188400     IF 6006-KDINLOMR = 'TRG'                                             
188500       PERFORM S02C-OPPNA-TRG                                             
188600     END-IF                                                               
188700     IF 6006-KDINLOMR = 'LPL' OR 'UNL'                                    
188800       PERFORM S02D-OPPNA-LPL                                             
188900     END-IF                                                               
189000     IF 6006-KDINLOMR = 'RTA' OR 'SQ '                                    
189100       PERFORM S02E-OPPNA-RTA                                             
189200     END-IF                                                               
189300     IF 6006-KDINLOMR = 'BO'                                              
189400       PERFORM S02F-OPPNA-BO                                              
189500     END-IF                                                               
189600     IF 6006-KDINLOMR = 'SYS'                                             
189700       PERFORM S02G-OPPNA-SYS                                             
189800     END-IF                                                               
189900     .                                                                    
190000     EJECT                                                                
190100 F-TRYCK-PF11 SECTION.                                                    
190200                                                                          
190300     MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                  
190400     CALL WMEDKONV USING MED-WMEDAREA                                     
190500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
190600                                                                          
190700     MOVE MFS-ADD-SAETT-CURSOR TO  MOD-ADINLOMR-PAR-IN-ATTR               
190800     PERFORM S03-BEH-STAENGDA-FAELT                                       
190900     PERFORM MFS-LAS-IN-IGEN                                              
191000     PERFORM S02-OPPNA-FAELT                                              
191100     PERFORM MFS-ROER-EJ-FAELT-IN                                         
191200     PERFORM MFS-ROER-EJ-FAELT-UT                                         
191300     IF MID-KDLORAPP = 0                                                  
191400       MOVE SPACE               TO MOD-TEXT1-IN                           
191500     END-IF                                                               
191600     IF MID-KDLORAPP = 1                                                  
191700       MOVE 'NORMALFLÖDE      ' TO MOD-TEXT1-IN                           
191800     END-IF                                                               
191900     IF MID-KDLORAPP = 2                                                  
192000       MOVE 'AR SKA TAS UT    ' TO MOD-TEXT1-IN                           
192100     END-IF                                                               
192200     IF MID-KDLORAPP = 3                                                  
192300       MOVE 'I-LISTA VID MOTT ' TO MOD-TEXT1-IN                           
192400     END-IF                                                               
192500     IF MID-KDLORAPP = 4                                                  
192600       MOVE 'R32 VID LOSSNING ' TO MOD-TEXT1-IN                           
192700     END-IF                                                               
192800     IF MID-KDLORAPP = 5                                                  
192900       MOVE 'NO PRINT OF RR   ' TO MOD-TEXT1-IN                           
193000     END-IF                                                               
193100                                                                          
193200     .                                                                    
193300     EJECT                                                                
193400 S02-OPPNA-FAELT SECTION.                                                 
193500      IF WS-KDINLOMR = 'LO'                                               
193600         PERFORM S02A-OPPNA-LO                                            
193700      END-IF                                                              
193800      IF WS-KDINLOMR = 'FB' OR 'FBP' OR 'F' OR                            
193900                       'PG' OR 'PGP' OR 'P'                               
194000         PERFORM S02B-OPPNA-FB                                            
194100      END-IF                                                              
194200      IF WS-KDINLOMR = 'TRG'                                              
194300         PERFORM S02C-OPPNA-TRG                                           
194400      END-IF                                                              
194500      IF WS-KDINLOMR = 'LPL' OR 'UNL'                                     
194600         PERFORM S02D-OPPNA-LPL                                           
194700      END-IF                                                              
194800      IF WS-KDINLOMR = 'RTA' OR 'SQ'                                      
194900         PERFORM S02E-OPPNA-RTA                                           
195000      END-IF                                                              
195100     IF WS-KDINLOMR = 'BO'                                                
195200        PERFORM S02F-OPPNA-BO                                             
195300     END-IF                                                               
195400     IF WS-KDINLOMR = 'SYS'                                               
195500        PERFORM S02G-OPPNA-SYS                                            
195600     END-IF                                                               
196200     .                                                                    
196300     EJECT                                                                
196400 S02A-OPPNA-LO SECTION.                                                   
196500     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
196600                                   MOD-ADINLOMR-BO-IN-ATTR                
196700                                   MOD-ADINLOMR-LPL-IN-ATTR               
196800                                   MOD-ADINLOMR-PRT-IN-ATTR               
196900                                   MOD-KDINLUPF-IN-ATTR                   
197000                                   MOD-KDLORAPP-IN-ATTR                   
197100                                   MOD-TEXT1-IN-ATTR                      
197200                                   MOD-FLLOLL-IN-ATTR                     
197300                                   MOD-FLCDOMR-IN-ATTR                    
197400                                   MOD-IDLEVNR-IN-ATTR                    
197500                                   MOD-FLSVAR-IN-ATTR                     
197600                                   MOD-FLKNTRGK-IN-ATTR                   
197610                                   MOD-FLEXCP-IN-ATTR                     
197620                                   MOD-IDAVD-DAG-IN-ATTR                  
197630                                   MOD-IDGRUPP-DAG-IN-ATTR                
197640                                   MOD-IDAVD-NATT-IN-ATTR                 
197650                                   MOD-IDGRUPP-NATT-IN-ATTR               
197700                                   MOD-IDPERSON-ANSV-IN-ATTR              
197800     .                                                                    
197900     EJECT                                                                
198000 S02B-OPPNA-FB SECTION.                                                   
198100     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
198200                                   MOD-ADINLOMR-LPL-IN-ATTR               
198300                                   MOD-KDINLUPF-IN-ATTR                   
198400                                   MOD-FLKVARED-IN-ATTR                   
198500                                   MOD-FLKNTRGK-IN-ATTR                   
198510                                   MOD-FLEXCP-IN-ATTR                     
198520                                   MOD-IDAVD-DAG-IN-ATTR                  
198530                                   MOD-IDGRUPP-DAG-IN-ATTR                
198540                                   MOD-IDAVD-NATT-IN-ATTR                 
198550                                   MOD-IDGRUPP-NATT-IN-ATTR               
198600                                   MOD-IDLEVNR-IN-ATTR                    
198700                                   MOD-FLSVAR-IN-ATTR                     
198800                                   MOD-IDPERSON-ANSV-IN-ATTR              
198900                                   MOD-IDPERSON-FORP-IN-ATTR              
199000                                   MOD-IDPERSON-KVAL-IN-ATTR              
199100     .                                                                    
199200     EJECT                                                                
199300 S02C-OPPNA-TRG SECTION.                                                  
199400     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
199500                                   MOD-ADINLOMR-BO-IN-ATTR                
199600                                   MOD-KDINLUPF-IN-ATTR                   
199700                                   MOD-ADPLATS-FOM-IN-ATTR                
199800                                   MOD-ADPLATS-TOM-IN-ATTR                
199900                                   MOD-ADGANG-FOM-IN-ATTR                 
200000                                   MOD-ADGANG-TOM-IN-ATTR                 
200100                                   MOD-IDLEVNR-IN-ATTR                    
200200                                   MOD-FLSVAR-IN-ATTR                     
200300                                   MOD-FLKNTRGK-IN-ATTR                   
200310                                   MOD-FLEXCP-IN-ATTR                     
200320                                   MOD-IDAVD-DAG-IN-ATTR                  
200330                                   MOD-IDGRUPP-DAG-IN-ATTR                
200340                                   MOD-IDAVD-NATT-IN-ATTR                 
200350                                   MOD-IDGRUPP-NATT-IN-ATTR               
200400     .                                                                    
200500     EJECT                                                                
200600 S02D-OPPNA-LPL SECTION.                                                  
200700     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
200800                                   MOD-KDINLUPF-IN-ATTR                   
200900                                   MOD-IDLEVNR-IN-ATTR                    
201000                                   MOD-FLSVAR-IN-ATTR                     
201100                                   MOD-FLKNTRGK-IN-ATTR                   
201110                                   MOD-FLEXCP-IN-ATTR                     
201120                                   MOD-IDAVD-DAG-IN-ATTR                  
201130                                   MOD-IDGRUPP-DAG-IN-ATTR                
201140                                   MOD-IDAVD-NATT-IN-ATTR                 
201150                                   MOD-IDGRUPP-NATT-IN-ATTR               
201200     .                                                                    
201300     EJECT                                                                
201400 S02E-OPPNA-RTA SECTION.                                                  
201500     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
201600                                   MOD-KDINLUPF-IN-ATTR                   
201700                                   MOD-IDLEVNR-IN-ATTR                    
201800                                   MOD-FLSVAR-IN-ATTR                     
201900                                   MOD-FLKNTRGK-IN-ATTR                   
201910                                   MOD-FLEXCP-IN-ATTR                     
201920                                   MOD-IDAVD-DAG-IN-ATTR                  
201930                                   MOD-IDGRUPP-DAG-IN-ATTR                
201940                                   MOD-IDAVD-NATT-IN-ATTR                 
201950                                   MOD-IDGRUPP-NATT-IN-ATTR               
202000     .                                                                    
202100     EJECT                                                                
202200 S02F-OPPNA-BO SECTION.                                                   
202300     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
202400                                   MOD-ADINLOMR-LPL-IN-ATTR               
202500                                   MOD-KDINLUPF-IN-ATTR                   
202600                                   MOD-IDLEVNR-IN-ATTR                    
202700                                   MOD-FLSVAR-IN-ATTR                     
202800                                   MOD-FLKNTRGK-IN-ATTR                   
202810                                   MOD-FLEXCP-IN-ATTR                     
202820                                   MOD-IDAVD-DAG-IN-ATTR                  
202830                                   MOD-IDGRUPP-DAG-IN-ATTR                
202840                                   MOD-IDAVD-NATT-IN-ATTR                 
202850                                   MOD-IDGRUPP-NATT-IN-ATTR               
202900     .                                                                    
203000     EJECT                                                                
203100 S02G-OPPNA-SYS SECTION.                                                  
203200     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
203300                                   MOD-ADINLOMR-LPL-IN-ATTR               
203400                                   MOD-KDINLUPF-IN-ATTR                   
203500                                   MOD-IDLEVNR-IN-ATTR                    
203600                                   MOD-FLSVAR-IN-ATTR                     
203700                                   MOD-FLKNTRGK-IN-ATTR                   
203710                                   MOD-FLEXCP-IN-ATTR                     
203720                                   MOD-IDAVD-DAG-IN-ATTR                  
203730                                   MOD-IDGRUPP-DAG-IN-ATTR                
203740                                   MOD-IDAVD-NATT-IN-ATTR                 
203750                                   MOD-IDGRUPP-NATT-IN-ATTR               
203800     .                                                                    
203900     EJECT                                                                
204000 S03-BEH-STAENGDA-FAELT SECTION.                                          
204100      IF WS-KDINLOMR = 'LO'                                               
204200         PERFORM S03A-STAENG-LO                                           
204300      END-IF                                                              
204400      IF WS-KDINLOMR = 'FB' OR 'FBP' OR 'F' OR                            
204500                       'PG' OR 'PGP' OR 'P'                               
204600         PERFORM S03B-STAENG-FB                                           
204700      END-IF                                                              
204800      IF WS-KDINLOMR = 'TRG'                                              
204900         PERFORM S03C-STAENG-TRG                                          
205000      END-IF                                                              
205100      IF WS-KDINLOMR = 'LPL' OR 'BO' OR 'UNL'                             
205200         PERFORM S03D-STAENG-LPL                                          
205300      END-IF                                                              
205400      IF WS-KDINLOMR = 'RTA' OR 'SQ '                                     
205500         PERFORM S03E-STAENG-RTA                                          
205600      END-IF                                                              
205700      IF WS-KDINLOMR = 'SYS'                                              
205800         PERFORM S03F-STAENG-SYS                                          
205900      END-IF                                                              
206000     .                                                                    
206100     EJECT                                                                
206200 S03A-STAENG-LO SECTION.                                                  
206300      MOVE SPACE           TO MOD-ADINLOMR-PAR-IN                         
206400                              MOD-ADINLOMR-BO-IN                          
206500                              MOD-ADINLOMR-LPL-IN                         
206600                              MOD-ADINLOMR-PRT-IN                         
206700                              MOD-KDINLUPF-IN                             
206800                              MOD-IDLEVNR-IN                              
206900                              MOD-TEXT1-IN                                
207000      MOVE ZERO            TO MOD-ADPLATS-FOM-IN                          
207100                              MOD-ADPLATS-TOM-IN                          
207200                              MOD-ADGANG-FOM-IN                           
207300                              MOD-ADGANG-TOM-IN                           
207400                              MOD-KDLORAPP-IN                             
207500                              MOD-IDPERSON-ANSV-IN                        
207600                              MOD-IDPERSON-FORP-IN                        
207700                              MOD-IDPERSON-KVAL-IN                        
207800     MOVE NEJ              TO MOD-FLKVARED-IN                             
207900                              MOD-FLKNTRGK-IN                             
207910                              MOD-FLEXCP-IN                               
207920                              MOD-IDAVD-DAG-IN                            
208000                              MOD-IDGRUPP-DAG-IN                          
208100                              MOD-IDAVD-NATT-IN                           
208200                              MOD-IDGRUPP-NATT-IN                         
208210                              MOD-FLSVAR-IN                               
208220                              MOD-FLCDOMR-IN                              
208230     MOVE JA               TO MOD-FLLOLL-IN                               
208300     .                                                                    
208400     EJECT                                                                
208500 S03B-STAENG-FB SECTION.                                                  
208600     MOVE SPACE           TO MOD-ADINLOMR-PAR-IN                          
208700                             MOD-ADINLOMR-LPL-IN                          
208800                             MOD-KDINLUPF-IN                              
208900                             MOD-IDLEVNR-IN                               
209000                             MOD-TEXT1-IN                                 
209100     MOVE SPACE           TO MOD-ADINLOMR-BO-IN                           
209200                             MOD-ADINLOMR-PRT-IN                          
209300     MOVE ZERO            TO MOD-KDLORAPP-IN                              
209400                             MOD-ADPLATS-FOM-IN                           
209500                             MOD-ADPLATS-TOM-IN                           
209600                             MOD-ADGANG-FOM-IN                            
209700                             MOD-ADGANG-TOM-IN                            
209800                             MOD-IDPERSON-ANSV-IN                         
209900                             MOD-IDPERSON-FORP-IN                         
210000                             MOD-IDPERSON-KVAL-IN                         
210100     MOVE NEJ             TO MOD-FLSVAR-IN                                
210200                             MOD-FLKVARED-IN                              
210300                             MOD-FLKNTRGK-IN                              
210310                             MOD-FLEXCP-IN                                
210320                             MOD-IDAVD-DAG-IN                             
210330                             MOD-IDGRUPP-DAG-IN                           
210340                             MOD-IDAVD-NATT-IN                            
210350                             MOD-IDGRUPP-NATT-IN                          
210400                             MOD-FLCDOMR-IN                               
210500     MOVE JA              TO MOD-FLLOLL-IN                                
210600                                                                          
210700     .                                                                    
210800     EJECT                                                                
210900 S03C-STAENG-TRG SECTION.                                                 
211000     MOVE SPACE           TO MOD-ADINLOMR-PAR-IN                          
211100                             MOD-ADINLOMR-BO-IN                           
211200                             MOD-KDINLUPF-IN                              
211300                             MOD-IDLEVNR-IN                               
211400                             MOD-ADINLOMR-LPL-IN                          
211500                             MOD-ADINLOMR-PRT-IN                          
211600                             MOD-TEXT1-IN                                 
211700     MOVE ZERO            TO MOD-KDLORAPP-IN                              
211800                             MOD-ADPLATS-FOM-IN                           
211900                             MOD-ADPLATS-TOM-IN                           
212000                             MOD-ADGANG-FOM-IN                            
212100                             MOD-ADGANG-TOM-IN                            
212200                             MOD-IDPERSON-ANSV-IN                         
212300                             MOD-IDPERSON-FORP-IN                         
212400                             MOD-IDPERSON-KVAL-IN                         
212500     MOVE NEJ             TO MOD-FLKVARED-IN                              
212600                             MOD-FLKNTRGK-IN                              
212610                             MOD-FLEXCP-IN                                
212620                             MOD-IDAVD-DAG-IN                             
212630                             MOD-IDGRUPP-DAG-IN                           
212640                             MOD-IDAVD-NATT-IN                            
212650                             MOD-IDGRUPP-NATT-IN                          
212700                             MOD-FLSVAR-IN                                
212800                             MOD-FLCDOMR-IN                               
212900     MOVE JA              TO MOD-FLLOLL-IN                                
213000     .                                                                    
213100     EJECT                                                                
213200 S03D-STAENG-LPL SECTION.                                                 
213300     MOVE SPACE           TO  MOD-ADINLOMR-PAR-IN                         
213400                              MOD-KDINLUPF-IN                             
213500                              MOD-IDLEVNR-IN                              
213600                              MOD-ADINLOMR-BO-IN                          
213700                              MOD-ADINLOMR-LPL-IN                         
213800                              MOD-ADINLOMR-PRT-IN                         
213900                              MOD-TEXT1-IN                                
214000     MOVE ZERO            TO  MOD-KDLORAPP-IN                             
214100                              MOD-ADPLATS-FOM-IN                          
214200                              MOD-ADPLATS-TOM-IN                          
214300                              MOD-ADGANG-FOM-IN                           
214400                              MOD-ADGANG-TOM-IN                           
214500                              MOD-IDPERSON-ANSV-IN                        
214600                              MOD-IDPERSON-FORP-IN                        
214700                              MOD-IDPERSON-KVAL-IN                        
214800     MOVE NEJ             TO  MOD-FLKVARED-IN                             
214900                              MOD-FLKNTRGK-IN                             
214910                              MOD-FLEXCP-IN                               
214920                              MOD-IDAVD-DAG-IN                            
214930                              MOD-IDGRUPP-DAG-IN                          
214940                              MOD-IDAVD-NATT-IN                           
214950                              MOD-IDGRUPP-NATT-IN                         
215000                              MOD-FLSVAR-IN                               
215100                              MOD-FLCDOMR-IN                              
215200     MOVE JA              TO  MOD-FLLOLL-IN                               
215300     .                                                                    
215400     EJECT                                                                
215500 S03E-STAENG-RTA SECTION.                                                 
215600     MOVE SPACE           TO MOD-ADINLOMR-PAR-IN                          
215700                             MOD-KDINLUPF-IN                              
215800                             MOD-IDLEVNR-IN                               
215900                             MOD-ADINLOMR-BO-IN                           
216000                             MOD-ADINLOMR-LPL-IN                          
216100                             MOD-ADINLOMR-PRT-IN                          
216200                             MOD-TEXT1-IN                                 
216300     MOVE ZERO            TO MOD-KDLORAPP-IN                              
216400                             MOD-ADPLATS-FOM-IN                           
216500                             MOD-ADPLATS-TOM-IN                           
216600                             MOD-ADGANG-FOM-IN                            
216700                             MOD-ADGANG-TOM-IN                            
216800                             MOD-IDPERSON-ANSV-IN                         
216900                             MOD-IDPERSON-FORP-IN                         
217000                             MOD-IDPERSON-KVAL-IN                         
217100     MOVE NEJ             TO MOD-FLKVARED-IN                              
217200                             MOD-FLKNTRGK-IN                              
217210                             MOD-FLEXCP-IN                                
217220                             MOD-IDAVD-DAG-IN                             
217230                             MOD-IDGRUPP-DAG-IN                           
217240                             MOD-IDAVD-NATT-IN                            
217250                             MOD-IDGRUPP-NATT-IN                          
217300                             MOD-FLSVAR-IN                                
217400                             MOD-FLCDOMR-IN                               
217500     MOVE JA              TO MOD-FLLOLL-IN                                
217600     .                                                                    
217700     EJECT                                                                
217800 S03F-STAENG-SYS SECTION.                                                 
217900     MOVE SPACE           TO MOD-ADINLOMR-PAR-IN                          
218000                             MOD-ADINLOMR-LPL-IN                          
218100                             MOD-KDINLUPF-IN                              
218200                             MOD-IDLEVNR-IN                               
218300                             MOD-TEXT1-IN                                 
218400     MOVE SPACE           TO MOD-ADINLOMR-BO-IN                           
218500                             MOD-ADINLOMR-PRT-IN                          
218600     MOVE ZERO            TO MOD-KDLORAPP-IN                              
218700                             MOD-ADPLATS-FOM-IN                           
218800                             MOD-ADPLATS-TOM-IN                           
218900                             MOD-ADGANG-FOM-IN                            
219000                             MOD-ADGANG-TOM-IN                            
219100                             MOD-IDPERSON-ANSV-IN                         
219200                             MOD-IDPERSON-FORP-IN                         
219300                             MOD-IDPERSON-KVAL-IN                         
219400     MOVE NEJ             TO MOD-FLSVAR-IN                                
219500                             MOD-FLKVARED-IN                              
219600                             MOD-FLKNTRGK-IN                              
219610                             MOD-FLEXCP-IN                                
219620                             MOD-IDAVD-DAG-IN                             
219630                             MOD-IDGRUPP-DAG-IN                           
219640                             MOD-IDAVD-NATT-IN                            
219650                             MOD-IDGRUPP-NATT-IN                          
219700                             MOD-FLCDOMR-IN                               
219800     MOVE JA              TO MOD-FLLOLL-IN                                
219900                                                                          
220000     .                                                                    
220100     EJECT                                                                
220200 MFS-RENSA-FAELT-UT SECTION.                                              
220300     MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-PAR-UT                          
220400                              MOD-KDINLUPF-UT                             
220500                              MOD-ADINLOMR-BO-UT                          
220600                              MOD-ADINLOMR-LPL-UT                         
220700                              MOD-ADPLATS-FOM-UT                          
220800                              MOD-ADPLATS-TOM-UT                          
220900                              MOD-ADGANG-FOM-UT                           
221000                              MOD-ADGANG-TOM-UT                           
221100                              MOD-FLLOLL-UT                               
221200                              MOD-FLCDOMR-UT                              
221300                              MOD-ADINLOMR-PRT-UT                         
221400                              MOD-IDLEVNR-UT                              
221500                              MOD-FLSVAR-UT                               
221600                              MOD-FLKVARED-UT                             
221700                              MOD-FLKNTRGK-UT                             
221710                              MOD-FLEXCP-UT                               
221720                              MOD-IDAVD-DAG-UT                            
221730                              MOD-IDGRUPP-DAG-UT                          
221740                              MOD-IDAVD-NATT-UT                           
221750                              MOD-IDGRUPP-NATT-UT                         
221800                              MOD-KDLORAPP-UT                             
221900                              MOD-TEXT1-UT                                
222000                              MOD-IDPERSON-ANSV-UT                        
222100                              MOD-IDPERSON-FORP-UT                        
222200                              MOD-IDPERSON-KVAL-UT                        
222300                              MOD-BEINIT-ANSV                             
222400                              MOD-BEINIT-FORP                             
222500                              MOD-BEINIT-KVAL                             
222600                                                                          
222700                              MOD-KVTID-NORM-UT                           
222800                              MOD-KVTID-NORMTOT-UT                        
222900                              MOD-KVTID-PRIO-UT                           
223000                              MOD-KVTID-PRIOTOT-UT                        
223010                              MOD-KVTID-NTCDC-UT                          
223020                              MOD-KVTID-PTCDC-UT                          
223030                              MOD-KVTID-NTSVS-UT                          
223040                              MOD-KVTID-PTSVS-UT                          
223100     .                                                                    
223200     SKIP2                                                                
223300 MFS-RENSA-FAELT-IN SECTION.                                              
223400     MOVE MFS-RENSA-FAELT  TO MOD-ADINLOMR-PAR-IN                         
223500                              MOD-KDINLUPF-IN                             
223600                              MOD-ADINLOMR-BO-IN                          
223700                              MOD-ADINLOMR-LPL-IN                         
223800                              MOD-ADPLATS-FOM-IN                          
223900                              MOD-ADPLATS-TOM-IN                          
224000                              MOD-ADGANG-FOM-IN                           
224100                              MOD-ADGANG-TOM-IN                           
224200                              MOD-FLLOLL-IN                               
224300                              MOD-FLCDOMR-IN                              
224400                              MOD-ADINLOMR-PRT-IN                         
224500                              MOD-IDLEVNR-IN                              
224600                              MOD-FLSVAR-IN                               
224700                              MOD-FLKVARED-IN                             
224800                              MOD-FLKNTRGK-IN                             
224810                              MOD-FLEXCP-IN                               
224820                              MOD-IDAVD-DAG-IN                            
224830                              MOD-IDGRUPP-DAG-IN                          
224840                              MOD-IDAVD-NATT-IN                           
224850                              MOD-IDGRUPP-NATT-IN                         
224900                              MOD-KDLORAPP-IN                             
225000                              MOD-TEXT1-IN                                
225100                              MOD-IDPERSON-ANSV-IN                        
225200                              MOD-IDPERSON-FORP-IN                        
225300                              MOD-IDPERSON-KVAL-IN                        
225400                                                                          
225500                              MOD-KVTID-NORM-IN                           
225600                              MOD-KVTID-NORMTOT-IN                        
225700                              MOD-KVTID-PRIO-IN                           
225800                              MOD-KVTID-PRIOTOT-IN                        
225810                              MOD-KVTID-NTCDC-IN                          
225820                              MOD-KVTID-PTCDC-IN                          
225830                              MOD-KVTID-NTSVS-IN                          
225840                              MOD-KVTID-PTSVS-IN                          
225900     .                                                                    
226000     EJECT                                                                
226100 MFS-FORM-ATTR SECTION.                                                   
226200                                                                          
226300     MOVE MFS-FORMATETS-ATTR TO MOD-ADINLOMR-PAR-IN-ATTR                  
226400                                MOD-KDINLUPF-IN-ATTR                      
226500                                MOD-ADINLOMR-BO-IN-ATTR                   
226600                                MOD-ADINLOMR-LPL-IN-ATTR                  
226700                                MOD-KDLORAPP-IN-ATTR                      
226800                                MOD-ADPLATS-FOM-IN-ATTR                   
226900                                MOD-ADPLATS-TOM-IN-ATTR                   
227000                                MOD-ADGANG-TOM-IN-ATTR                    
227100                                MOD-ADGANG-TOM-IN-ATTR                    
227200                                MOD-FLLOLL-IN-ATTR                        
227300                                MOD-FLCDOMR-IN-ATTR                       
227400                                MOD-ADINLOMR-PRT-IN-ATTR                  
227500                                MOD-IDLEVNR-IN-ATTR                       
227600                                MOD-FLKVARED-IN-ATTR                      
227700                                MOD-FLKNTRGK-IN-ATTR                      
227710                                MOD-FLEXCP-IN-ATTR                        
227720                                MOD-IDAVD-DAG-IN-ATTR                     
227730                                MOD-IDGRUPP-DAG-IN-ATTR                   
227740                                MOD-IDAVD-NATT-IN-ATTR                    
227750                                MOD-IDGRUPP-NATT-IN-ATTR                  
227800                                MOD-FLSVAR-IN-ATTR                        
227900                                MOD-TEXT1-IN-ATTR                         
228000                                MOD-IDPERSON-ANSV-IN-ATTR                 
228100                                MOD-IDPERSON-FORP-IN-ATTR                 
228200                                MOD-IDPERSON-KVAL-IN-ATTR                 
228300                                                                          
228400                                MOD-KVTID-NORM-IN-ATTR                    
228500                                MOD-KVTID-NORMTOT-IN-ATTR                 
228600                                MOD-KVTID-PRIO-IN-ATTR                    
228700                                MOD-KVTID-PRIOTOT-IN-ATTR                 
228701                                MOD-KVTID-NTCDC-IN-ATTR                   
228702                                MOD-KVTID-PTCDC-IN-ATTR                   
228703                                MOD-KVTID-NTSVS-IN-ATTR                   
228704                                MOD-KVTID-PTSVS-IN-ATTR                   
228710                                                                          
228800     .                                                                    
228900     SKIP2                                                                
229000     EJECT                                                                
229100 MFS-ROER-EJ-FAELT-IN SECTION.                                            
229200     IF MID-ADINLOMR-PAR = ALL '+'                                        
229300       MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PAR-IN                      
229400     ELSE                                                                 
229500       MOVE MID-ADINLOMR-PAR TO MOD-ADINLOMR-PAR-IN                       
229600     END-IF                                                               
229700                                                                          
229800     IF MID-KDINLUPF = ALL '+'                                            
229900       MOVE MFS-ROER-EJ-FAELT TO MOD-KDINLUPF-IN                          
230000     ELSE                                                                 
230100       MOVE MID-KDINLUPF     TO  MOD-KDINLUPF-IN                          
230200     END-IF                                                               
230300                                                                          
230400     IF MID-ADINLOMR-BO = ALL '+' OR  SPACE                               
230500       MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-BO-IN                       
230600     ELSE                                                                 
230700       MOVE MID-ADINLOMR-BO  TO  MOD-ADINLOMR-BO-IN                       
230800     END-IF                                                               
230900                                                                          
231000     IF MID-ADINLOMR-LPL = ALL '+'                                        
231100       MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-LPL-IN                      
231200     ELSE                                                                 
231300       MOVE MID-ADINLOMR-LPL TO  MOD-ADINLOMR-LPL-IN                      
231400     END-IF                                                               
231500                                                                          
231600     IF MID-KDLORAPP = ALL '+'                                            
231700       MOVE MFS-ROER-EJ-FAELT TO MOD-KDLORAPP-IN                          
231800     ELSE                                                                 
231900       MOVE MID-KDLORAPP     TO  MOD-KDLORAPP-IN                          
232000     END-IF                                                               
232100                                                                          
232200     IF MID-TEXT1 = ALL '+'                                               
232300       MOVE MFS-ROER-EJ-FAELT TO MOD-TEXT1-IN                             
232400     ELSE                                                                 
232500       MOVE MID-TEXT1        TO  MOD-TEXT1-IN                             
232600     END-IF                                                               
232700                                                                          
232800     IF MID-ADPLATS-FOM = ALL '+'                                         
232900       MOVE MFS-ROER-EJ-FAELT TO MOD-ADPLATS-FOM-IN                       
233000     ELSE                                                                 
233100       MOVE MID-ADPLATS-FOM  TO  MOD-ADPLATS-FOM-IN                       
233200     END-IF                                                               
233300                                                                          
233400     IF MID-ADPLATS-TOM = ALL '+'                                         
233500       MOVE MFS-ROER-EJ-FAELT TO MOD-ADPLATS-TOM-IN                       
233600     ELSE                                                                 
233700     MOVE MID-ADPLATS-TOM  TO  MOD-ADPLATS-TOM-IN                         
233800     END-IF                                                               
233900                                                                          
234000     IF MID-ADGANG-FOM = ALL '+'                                          
234100       MOVE MFS-ROER-EJ-FAELT TO MOD-ADGANG-FOM-IN                        
234200     ELSE                                                                 
234300     MOVE MID-ADGANG-FOM   TO  MOD-ADGANG-FOM-IN                          
234400     END-IF                                                               
234500                                                                          
234600     IF MID-ADGANG-TOM = ALL '+'                                          
234700       MOVE MFS-ROER-EJ-FAELT TO MOD-ADGANG-TOM-IN                        
234800     ELSE                                                                 
234900       MOVE MID-ADGANG-TOM   TO  MOD-ADGANG-TOM-IN                        
235000     END-IF                                                               
235100                                                                          
235200     IF MID-FLLOLL = ALL '+'                                              
235300       MOVE MFS-ROER-EJ-FAELT TO MOD-FLLOLL-IN                            
235400     ELSE                                                                 
235500       MOVE MID-FLLOLL       TO  MOD-FLLOLL-IN                            
235600     END-IF                                                               
235700                                                                          
235800     IF MID-FLCDOMR = ALL '+'                                             
235900       MOVE MFS-ROER-EJ-FAELT TO MOD-FLCDOMR-IN                           
236000     ELSE                                                                 
236100       MOVE MID-FLCDOMR       TO  MOD-FLCDOMR-IN                          
236200     END-IF                                                               
236300                                                                          
236400     IF MID-ADINLOMR-PRT = ALL '+'                                        
236500       MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PRT-IN                      
236600     ELSE                                                                 
236700       MOVE MID-ADINLOMR-PRT TO  MOD-ADINLOMR-PRT-IN                      
236800     END-IF                                                               
236900                                                                          
237000     IF MID-IDLEVNR = ALL '+'                                             
237100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                           
237200     ELSE                                                                 
237300       MOVE MID-IDLEVNR      TO  MOD-IDLEVNR-IN                           
237400     END-IF                                                               
237500                                                                          
237600     IF MID-FLSVAR = ALL '+'                                              
237700       MOVE MFS-ROER-EJ-FAELT TO MOD-FLSVAR-IN                            
237800     ELSE                                                                 
237900       MOVE MID-FLSVAR       TO  MOD-FLSVAR-IN                            
238000     END-IF                                                               
238100                                                                          
238200     IF MID-FLKVARED = ALL '+'                                            
238300       MOVE MFS-ROER-EJ-FAELT TO MOD-FLKVARED-IN                          
238400     ELSE                                                                 
238500       MOVE MID-FLKVARED     TO  MOD-FLKVARED-IN                          
238600     END-IF                                                               
238700                                                                          
238800     IF MID-FLKNTRGK = ALL '+'                                            
238900       MOVE MFS-ROER-EJ-FAELT TO MOD-FLKNTRGK-IN                          
239000     ELSE                                                                 
239100       MOVE MID-FLKNTRGK     TO  MOD-FLKNTRGK-IN                          
239200     END-IF                                                               
239300                                                                          
239310     IF MID-FLEXCP   = ALL '+'                                            
239320       MOVE MFS-ROER-EJ-FAELT TO MOD-FLEXCP-IN                            
239330     ELSE                                                                 
239340       MOVE MID-FLEXCP       TO  MOD-FLEXCP-IN                            
239350     END-IF                                                               
239360                                                                          
239370     IF MID-IDAVD-DAG = ALL '+'                                           
239380       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVD-DAG-IN                         
239390     ELSE                                                                 
239391       MOVE MID-IDAVD-DAG    TO  MOD-IDAVD-DAG-IN                         
239392     END-IF                                                               
239393                                                                          
239394     IF MID-IDGRUPP-DAG = ALL '+'                                         
239395       MOVE MFS-ROER-EJ-FAELT TO MOD-IDGRUPP-DAG-IN                       
239396     ELSE                                                                 
239397       MOVE MID-IDGRUPP-DAG  TO  MOD-IDGRUPP-DAG-IN                       
239398     END-IF                                                               
239399                                                                          
239400     IF MID-IDAVD-NATT = ALL '+'                                          
239401       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVD-NATT-IN                        
239402     ELSE                                                                 
239403       MOVE MID-IDAVD-NATT   TO  MOD-IDAVD-NATT-IN                        
239404     END-IF                                                               
239405                                                                          
239406     IF MID-IDGRUPP-NATT = ALL '+'                                        
239407       MOVE MFS-ROER-EJ-FAELT TO MOD-IDGRUPP-NATT-IN                      
239408     ELSE                                                                 
239409       MOVE MID-IDGRUPP-NATT TO  MOD-IDGRUPP-NATT-IN                      
239410     END-IF                                                               
239411                                                                          
239420     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPERSON-ANSV-IN                      
239500                                MOD-IDPERSON-FORP-IN                      
239600                                MOD-IDPERSON-KVAL-IN                      
239700                              MOD-KVTID-NORM-IN                           
239800                              MOD-KVTID-NORMTOT-IN                        
239900                              MOD-KVTID-PRIO-IN                           
240000                              MOD-KVTID-PRIOTOT-IN                        
240010                              MOD-KVTID-NTCDC-IN                          
240020                              MOD-KVTID-PTCDC-IN                          
240030                              MOD-KVTID-NTSVS-IN                          
240040                              MOD-KVTID-PTSVS-IN                          
240100     .                                                                    
240200     SKIP2                                                                
240300     EJECT                                                                
240400 MFS-ROER-EJ-FAELT-UT SECTION.                                            
240500     MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PAR-UT                        
240600                               MOD-KDINLUPF-UT                            
240700                               MOD-ADINLOMR-BO-UT                         
240800                               MOD-ADINLOMR-LPL-UT                        
240900                               MOD-KDLORAPP-UT                            
241000                               MOD-TEXT1-UT                               
241100                               MOD-ADPLATS-FOM-UT                         
241200                               MOD-ADPLATS-TOM-UT                         
241300                               MOD-ADGANG-FOM-UT                          
241400                               MOD-ADGANG-TOM-UT                          
241500                               MOD-FLLOLL-UT                              
241600                               MOD-FLCDOMR-UT                             
241700                               MOD-ADINLOMR-PRT-UT                        
241800                               MOD-IDLEVNR-UT                             
241900                               MOD-FLSVAR-UT                              
242000                               MOD-FLKVARED-UT                            
242100                               MOD-FLKNTRGK-UT                            
242110                               MOD-FLEXCP-UT                              
242120                               MOD-IDAVD-DAG-UT                           
242130                               MOD-IDGRUPP-DAG-UT                         
242140                               MOD-IDAVD-NATT-UT                          
242150                               MOD-IDGRUPP-NATT-UT                        
242200                               MOD-IDPERSON-ANSV-UT                       
242300                               MOD-IDPERSON-FORP-UT                       
242400                               MOD-IDPERSON-KVAL-UT                       
242500                               MOD-BEINIT-ANSV                            
242600                               MOD-BEINIT-FORP                            
242700                               MOD-BEINIT-KVAL                            
242800                              MOD-KVTID-NORM-UT                           
242900                              MOD-KVTID-NORMTOT-UT                        
243000                              MOD-KVTID-PRIO-UT                           
243100                              MOD-KVTID-PRIOTOT-UT                        
243110                              MOD-KVTID-NTCDC-UT                          
243120                              MOD-KVTID-PTCDC-UT                          
243130                              MOD-KVTID-NTSVS-UT                          
243140                              MOD-KVTID-PTSVS-UT                          
243200     .                                                                    
243300     SKIP2                                                                
243400     EJECT                                                                
243500 MFS-LAS-IN-IGEN SECTION.                                                 
243600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PAR-IN-ATTR               
243700                                   MOD-KDINLUPF-IN-ATTR                   
243800                                   MOD-ADINLOMR-BO-IN-ATTR                
243900                                   MOD-ADINLOMR-LPL-IN-ATTR               
244000                                   MOD-KDLORAPP-IN-ATTR                   
244100                                   MOD-TEXT1-IN-ATTR                      
244200                                   MOD-ADPLATS-FOM-IN-ATTR                
244300                                   MOD-ADPLATS-TOM-IN-ATTR                
244400                                   MOD-ADGANG-FOM-IN-ATTR                 
244500                                   MOD-ADGANG-TOM-IN-ATTR                 
244600                                   MOD-FLLOLL-IN-ATTR                     
244700                                   MOD-FLCDOMR-IN-ATTR                    
244800                                   MOD-ADINLOMR-PRT-IN-ATTR               
244900                                   MOD-IDLEVNR-IN-ATTR                    
245000                                   MOD-FLSVAR-IN-ATTR                     
245100                                   MOD-FLKVARED-IN-ATTR                   
245200                                   MOD-FLKNTRGK-IN-ATTR                   
245210                                   MOD-FLEXCP-IN-ATTR                     
245220                                   MOD-IDAVD-DAG-IN-ATTR                  
245230                                   MOD-IDGRUPP-DAG-IN-ATTR                
245240                                   MOD-IDAVD-NATT-IN-ATTR                 
245250                                   MOD-IDGRUPP-NATT-IN-ATTR               
245300                                   MOD-IDPERSON-ANSV-IN-ATTR              
245400                                   MOD-IDPERSON-FORP-IN-ATTR              
245500                                   MOD-IDPERSON-KVAL-IN-ATTR              
245600                                   MOD-KVTID-NORM-IN-ATTR                 
245700                                   MOD-KVTID-NORMTOT-IN-ATTR              
245800                                   MOD-KVTID-PRIO-IN-ATTR                 
245900                                   MOD-KVTID-PRIOTOT-IN-ATTR              
245910                                   MOD-KVTID-NTCDC-IN-ATTR                
245920                                   MOD-KVTID-PTCDC-IN-ATTR                
245930                                   MOD-KVTID-NTSVS-IN-ATTR                
245940                                   MOD-KVTID-PTSVS-IN-ATTR                
246000     .                                                                    
246100     EJECT                                                                
246200 MFS-LAS-IN-IGEN-UT SECTION.                                              
246300     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PAR-UT-ATTR               
246400                                   MOD-KDINLUPF-UT-ATTR                   
246500                                   MOD-ADINLOMR-BO-UT-ATTR                
246600                                   MOD-ADINLOMR-LPL-UT-ATTR               
246700                                   MOD-KDLORAPP-UT-ATTR                   
246800                                   MOD-TEXT1-UT-ATTR                      
246900                                   MOD-ADPLATS-FOM-UT-ATTR                
247000                                   MOD-ADPLATS-TOM-UT-ATTR                
247100                                   MOD-ADGANG-FOM-UT-ATTR                 
247200                                   MOD-ADGANG-TOM-UT-ATTR                 
247300                                   MOD-FLLOLL-UT-ATTR                     
247400                                   MOD-FLCDOMR-UT-ATTR                    
247500                                   MOD-ADINLOMR-PRT-UT-ATTR               
247600                                   MOD-IDLEVNR-UT-ATTR                    
247700                                   MOD-FLSVAR-UT-ATTR                     
247800                                   MOD-FLKVARED-UT-ATTR                   
247900                                   MOD-FLKNTRGK-UT-ATTR                   
247910                                   MOD-FLEXCP-UT-ATTR                     
248000                                   MOD-IDPERSON-ANSV-UT-ATTR              
248100                                   MOD-IDPERSON-FORP-UT-ATTR              
248200                                   MOD-IDPERSON-KVAL-UT-ATTR              
248700     .                                                                    
248800     EJECT                                                                
248900* --- IMS SEKTIONER ---                                                   
249000     SKIP3                                                                
249100 IMS-GET-MSG SECTION.                                                     
249200                                                                          
249300     MOVE '  QC' TO GODK-STATUSKODER                                      
249400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
249500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
249600     PERFORM IMS-STATUSKONTROLL                                           
249700     .                                                                    
249800     SKIP3                                                                
249900 IMS-INSERT-MSG SECTION.                                                  
250000                                                                          
250100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
250200       MOVE '0' TO MFS-KDHUVOMR                                           
250300     END-IF                                                               
250400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
250500     MOVE SPACE TO GODK-STATUSKODER                                       
250600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
250700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
250800     PERFORM IMS-STATUSKONTROLL                                           
250900     .                                                                    
251000     EJECT                                                                
251100 IMS-GU-W6G130   SECTION.                                                 
251200     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
251300          DELIMITED BY SIZE INTO SSA1                                     
251400     STRING 'W6G130  (W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
251500          DELIMITED BY SIZE INTO SSA2                                     
251600     MOVE '  GE' TO GODK-STATUSKODER                                      
251700     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-W6G130 SSA1 SSA2               
251800     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
251900     PERFORM IMS-STATUSKONTROLL                                           
252000     .                                                                    
252100     EJECT                                                                
252200 IMS-GU-W6GX6008  SECTION.                                                
252300     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
252400          DELIMITED BY SIZE INTO SSA1                                     
252500     MOVE   'W6GX6008   '     TO SSA2                                     
252600     MOVE '  GE' TO GODK-STATUSKODER                                      
252700     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-W6GX6008   SSA1 SSA2           
252800     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
252900     PERFORM IMS-STATUSKONTROLL                                           
253000     .                                                                    
253100     EJECT                                                                
253110 IMS-GHU-W6GX6008  SECTION.                                               
253120     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
253130          DELIMITED BY SIZE INTO SSA1                                     
253140     MOVE   'W6GX6008   '     TO SSA2                                     
253150     MOVE '  GE' TO GODK-STATUSKODER                                      
253160     CALL CBLTDLI USING GHU W6G1-PCB DLI-IO-W6GX6008   SSA1 SSA2          
253170     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
253180     PERFORM IMS-STATUSKONTROLL                                           
253190     .                                                                    
253191     EJECT                                                                
253200 IMS-GHU-W6G130 SECTION.                                                  
253300     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
253400          DELIMITED BY SIZE INTO SSA1                                     
253500     STRING 'W6G130  (W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
253600          DELIMITED BY SIZE INTO SSA2                                     
253700     MOVE '  GE' TO GODK-STATUSKODER                                      
253800     CALL CBLTDLI USING GHU W6G1-PCB DLI-IO-W6G130 SSA1 SSA2              
253900     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
254000     PERFORM IMS-STATUSKONTROLL                                           
254100     .                                                                    
254200     EJECT                                                                
254300 IMS-ISRT-W6G130 SECTION.                                                 
254400                                                                          
254500     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
254600          DELIMITED BY SIZE INTO SSA1                                     
254700     MOVE 'W6G130   ' TO SSA2                                             
254800     MOVE '  II' TO GODK-STATUSKODER                                      
254900     CALL CBLTDLI USING ISRT W6G1-PCB  DLI-IO-W6G130 SSA1 SSA2            
255000     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
255100     PERFORM IMS-STATUSKONTROLL                                           
255200     .                                                                    
255300     SKIP3                                                                
256500 IMS-REPL-W6G130 SECTION.                                                 
256600                                                                          
256700     MOVE '  ' TO GODK-STATUSKODER                                        
256800     CALL CBLTDLI USING REPL W6G1-PCB DLI-IO-W6G130                       
256900     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
257000     PERFORM IMS-STATUSKONTROLL                                           
257100     .                                                                    
257200     SKIP3                                                                
257300 IMS-REPL-W6GX6008 SECTION.                                               
257400                                                                          
257500     MOVE '  ' TO GODK-STATUSKODER                                        
257600     CALL CBLTDLI USING REPL W6G1-PCB DLI-IO-W6GX6008                     
257700     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
257800     PERFORM IMS-STATUSKONTROLL                                           
257900     .                                                                    
258000     SKIP3                                                                
258100 IMS-DELETE-W6G130 SECTION.                                               
258200                                                                          
258300     MOVE '  ' TO GODK-STATUSKODER                                        
258400     CALL CBLTDLI USING DLET W6G1-PCB DLI-IO-W6G130                       
258500     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
258600     PERFORM IMS-STATUSKONTROLL                                           
258700     .                                                                    
258800     EJECT                                                                
259700 IMS-GU-W6G101 SECTION.                                                   
259800     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
259900          DELIMITED BY SIZE INTO SSA1                                     
260000     MOVE '  GE' TO GODK-STATUSKODER                                      
260100     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-W6G101  SSA1                   
260200     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
260300     PERFORM IMS-STATUSKONTROLL                                           
260400     .                                                                    
260500 IMS-GNP-W6G130-PAR SECTION.                                              
260600     STRING 'W6G130  (ADINLOMP =' W-ADINLOMR-PARX ')'                     
260700          DELIMITED BY SIZE INTO SSA1                                     
260800     MOVE '  GE' TO GODK-STATUSKODER                                      
260900     CALL CBLTDLI USING GNP W6G1-PCB DLI-IO-W6G130  SSA1                  
261000     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
261100     PERFORM IMS-STATUSKONTROLL                                           
261200     .                                                                    
261300     EJECT                                                                
261400 IMS-GNP-W6G130-LPL SECTION.                                              
261500     STRING 'W6G130  (ADINLOML =' W-ADINLOMR-LPLX ')'                     
261600          DELIMITED BY SIZE INTO SSA1                                     
261700     MOVE '  GE' TO GODK-STATUSKODER                                      
261800     CALL CBLTDLI USING GNP W6G1-PCB DLI-IO-W6G130  SSA1                  
261900     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
262000     PERFORM IMS-STATUSKONTROLL                                           
262100     .                                                                    
262200     EJECT                                                                
262300 IMS-GNP-W6G130-BO SECTION.                                               
262400     STRING 'W6G130  (ADINLOMB =' W-ADINLOMR-BOX ')'                      
262500          DELIMITED BY SIZE INTO SSA1                                     
262600     MOVE '  GE' TO GODK-STATUSKODER                                      
262700     CALL CBLTDLI USING GNP W6G1-PCB DLI-IO-W6G130  SSA1                  
262800     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
262900     PERFORM IMS-STATUSKONTROLL                                           
263000     .                                                                    
263100     EJECT                                                                
263200 IMS-GET-INLA11-ESEQ SECTION.                                             
263300     STRING 'W6INLA11(W6D1ESEQ =' W-ADINLOMR-X                            
263400                    '&IDDC     =' W-IDDC-X ')'                            
263500          DELIMITED BY SIZE INTO SSA1                                     
263600     MOVE '  GE' TO GODK-STATUSKODER                                      
263700     CALL CBLTDLI USING GU INLA1-PCB   IO-AREA    SSA1                    
263800     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
263900     PERFORM IMS-STATUSKONTROLL                                           
264000     .                                                                    
264100     EJECT                                                                
264200 IMS-GET-INLA11-GSEQ SECTION.                                             
264300     STRING 'W6INLA11(W6D1GSEQ =' W-ADINLOMN-X                            
264400                    '&IDDC     =' W-IDDC-X ')'                            
264500          DELIMITED BY SIZE INTO SSA1                                     
264600     MOVE '  GE' TO GODK-STATUSKODER                                      
264700     CALL CBLTDLI USING GU INLA2-PCB  IO-AREA   SSA1                      
264800     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
264900     PERFORM IMS-STATUSKONTROLL                                           
265000     .                                                                    
265100     EJECT                                                                
265200 IMS-GET-HANB01 SECTION.                                                  
265300     STRING 'W6HANB01(W6GXKEY  =' W-W6GXKEY-6031-X ')'                    
265400          DELIMITED BY SIZE INTO SSA1                                     
265500     MOVE '  GE' TO GODK-STATUSKODER                                      
265600     CALL CBLTDLI USING GU HANB-PCB IO-AREA SSA1                          
265700     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
265800     PERFORM IMS-STATUSKONTROLL                                           
265900     .                                                                    
266000     SKIP3                                                                
266100 IMS-GNP-HANB11 SECTION.                                                  
266200     STRING 'W6HANB11(ADINLOMR =' W-ADINLOMR-X ')'                        
266300          DELIMITED BY SIZE INTO SSA1                                     
266400     MOVE '  GE' TO GODK-STATUSKODER                                      
266500     CALL CBLTDLI USING GNP HANB-PCB IO-AREA SSA1                         
266600     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
266700     PERFORM IMS-STATUSKONTROLL                                           
266800     .                                                                    
266900     SKIP3                                                                
267000 IMS-GNP-HANB12 SECTION.                                                  
267100     STRING 'W6HANB12(ADINLOMR =' W-ADINLOMR-X ')'                        
267200          DELIMITED BY SIZE INTO SSA1                                     
267300     MOVE '  GE' TO GODK-STATUSKODER                                      
267400     CALL CBLTDLI USING GNP HANB-PCB IO-AREA SSA1                         
267500     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
267600     PERFORM IMS-STATUSKONTROLL                                           
267700     .                                                                    
267800     SKIP3                                                                
267900 IMS-GNP-HANB13 SECTION.                                                  
268000     STRING 'W6HANB13(ADINLOMR =' W-ADINLOMR-X ')'                        
268100          DELIMITED BY SIZE INTO SSA1                                     
268200     MOVE '  GE' TO GODK-STATUSKODER                                      
268300     CALL CBLTDLI USING GNP HANB-PCB IO-AREA SSA1                         
268400     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
268500     PERFORM IMS-STATUSKONTROLL                                           
268600     .                                                                    
268700     SKIP3                                                                
268800 IMS-GNP-HANB14 SECTION.                                                  
268900     STRING 'W6HANB14(ADINLOMR =' W-ADINLOMR-X ')'                        
269000          DELIMITED BY SIZE INTO SSA1                                     
269100     MOVE '  GE' TO GODK-STATUSKODER                                      
269200     CALL CBLTDLI USING GNP HANB-PCB IO-AREA SSA1                         
269300     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
269400     PERFORM IMS-STATUSKONTROLL                                           
269500     .                                                                    
269600     SKIP3                                                                
269700 IMS-GU-WDP311 SECTION.                                                   
269800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
269900          DELIMITED BY SIZE INTO SSA1                                     
270000     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
270100          DELIMITED BY SIZE INTO SSA2                                     
270200     MOVE '  GE' TO GODK-STATUSKODER                                      
270300     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-P311 SSA1 SSA2                
270400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
270500     PERFORM IMS-STATUSKONTROLL                                           
270600     .                                                                    
270700     SKIP3                                                                
270800 IMS-GU-WDP311-QUAL SECTION.                                              
270900     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-QUAL-X ')'                   
271000          DELIMITED BY SIZE INTO SSA1                                     
271100     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
271200          DELIMITED BY SIZE INTO SSA2                                     
271300     MOVE '  GE' TO GODK-STATUSKODER                                      
271400     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-P311 SSA1 SSA2                
271500     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
271600     PERFORM IMS-STATUSKONTROLL                                           
271700     .                                                                    
271800     SKIP3                                                                
271900 IMS-GU-WDB601    SECTION.                                                
272000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
272100          DELIMITED BY SIZE INTO SSA1                                     
272200     MOVE '  GE' TO GODK-STATUSKODER                                      
272300     CALL CBLTDLI USING GU WDB6-PCB  DLI-IO-B601  SSA1                    
272400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
272500     PERFORM IMS-STATUSKONTROLL                                           
272600     IF SEGMENT-SAKNAS                                                    
272700         MOVE SPACE TO DCS-KDDC                                           
272800     END-IF                                                               
272900     .                                                                    
273000 IMS-STATUSKONTROLL SECTION.                                              
273100                                                                          
273200     SET STATUS-IX TO 1                                                   
273300     SEARCH GODK-STATUS                                                   
273400       AT END                                                             
273500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
273600         DELIMITED BY SIZE INTO FELTEXT                                   
273700         CALL FELLOG                                                      
273800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
273900         CONTINUE                                                         
274000     END-SEARCH                                                           
274100     .                                                                    
