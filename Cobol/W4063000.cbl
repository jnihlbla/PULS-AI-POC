000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4063000.                                                
000400 AUTHOR.         MOGREN STINA.                                            
000500 DATE-WRITTEN.   07/04/03.                                                
000600 DATE-COMPILED.                                                           
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN BAKGRUNDS-MPP I SHIP-IT MODULEN                 
000900*        SOM SKAPAR RSI-TRANSAR TILL VIPS                                 
001000*                                                                         
001100*        PROGRAMMET STARTAS AV WZ01 FRÅN ADD-IT  4632                     
001200*                                                                         
001300*        INFORMATIONEN SÄNDES MED WZ01                                    
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDE1  TRANSPORTRELREG                      
001600*                              WDE4                                       
001700*                              WDR4  BOLLANR  (4491-4494)                 
001800*                              WDB6                                       
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W40630X                                             
002200*        MID:         W40630I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        WZ01-RADER   TILL VIPS - RSI                                     
002600*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W4063000'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200*------                                                                   
004300 77  ANTAL-SEND                  PIC S9(4)   BINARY VALUE ZERO.           
004400                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004600                                                                          
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005300     88  ALLT-OK                             VALUE 'J'.                   
005400                                                                          
005500 77  W-RAD-RAKNARE               PIC 9(5)    VALUE ZERO.                  
005600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
005700 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
005800                                                                          
005900 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
006000                                                                          
006100 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
006200 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
006300                                                                          
006400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006500                                                                          
006600 01  WS-DATUM                    PIC 9(8).                                
006700 01  FILLER                      REDEFINES WS-DATUM.                      
006800     03  WS-SEKEL                PIC 9(2).                                
006900     03  WS-AAMMDD               PIC 9(6).                                
007000                                                                          
007100 01  W-KUNDREF                   PIC 9(10)   VALUE ZERO.                  
007200 01  FILLER                      REDEFINES W-KUNDREF.                     
007300     03  W-IDKUNDRF-RO           PIC 9(5).                                
007400     03  FILLER                  PIC 9(5).                                
007500                                                                          
007600 01  WS-DALASTN                  PIC S9(7)  VALUE ZERO COMP-3.            
007700                                                                          
007800 01  ARBETSFALT.                                                          
007900     03 WS-ADDISPABS.                                                     
008000        05 WS-ADDISPABS-START    PIC X(14)  VALUE                         
008100                                     'CARPARTS.VIPS.'.                    
008200        05 WS-ADDISPABS-IDLAND   PIC X(02) VALUE SPACE.                   
008300        05 WS-ADDISPABS-SLUT     PIC X(10)  VALUE                         
008400                                     'TRPRSIINFO'.                        
008500        05 FILLER                PIC X(24) VALUE SPACE.                   
008600                                                                          
008700     03 WS-SPAR-IDDISTR          PIC S9(5)   VALUE ZERO.                  
008800                                                                          
008900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009000 01  GENERELLA-SUBPROGRAM.                                                
009100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009600     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
009700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009800     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
009900     EJECT                                                                
010000                                                                          
010100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010200*01 -COPY WMEDAREA                                                        
010300     SKIP3                                                                
010400 01  MESSAGE-CODES.                                                       
010500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010600     EJECT                                                                
010700                                                                          
010800                                                                          
010900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011200*01 -COPY WMSGINIT                                                        
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'W460DIS1'.            
011500                                                                          
011600*01 -COPY W460DIS1                                                        
011700     EJECT                                                                
011800                                                                          
011900 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
012000*                                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'WZ01-SEND'.           
012200*01  -COPY WZ01SEND                                                       
012300                                                                          
012400 01  UT-AREA.                                                             
012500*    03  FILLER -COPY W476RSI   -PRE UT-                                  
012600     EJECT                                                                
012700                                                                          
012800*    --- AREOR FÖR ANROP FRÅN WZ01                                        
012900 01  FILLER                      PIC X(16)   VALUE 'WZ01-RECV '.          
013000*01  -COPY WZ01RECV                                                       
013100                                                                          
013200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013300*                                                                         
013400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013500     SKIP3                                                                
013600*01  MID -COPY W40630I1                                                   
013700     EJECT                                                                
013800                                                                          
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014200     SKIP3                                                                
014300 01  NYCKLAR-TILL-DLI.                                                    
014400     03  W-IDSHIPM-X.                                                     
014500         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
014600     03  W-WDE111KY-X.                                                    
014700         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
014800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
014900     03  W-WDE121KY-X.                                                    
015000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
015100         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
015200     03  W-IDPURAD-X.                                                     
015300         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
015400                                                                          
015500     03  W-WDE4BSEQ-MIN-X.                                                
015600         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
015700         05  W-IDPURAD-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
015800                                                                          
015900     03  W-WDE4BSEQ-MAX-X.                                                
016000         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
016100         05  W-IDPURAD-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
016200                                                                          
016300     03  W-WDB2KY-X.                                                      
016400         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
016500         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
016600                                                                          
016700* TILL R4-TRANSPORTINFO BOLLA                                             
016800                                                                          
016900     03  W-WDGXKEY-4491-X.                                                
017000       05  W-IDHTYP-4491       PIC X(4)    VALUE '4491'.                  
017100       05  W-IDDC-4491         PIC X(2)    VALUE SPACE.                   
017200       05  FILLER              PIC X(24)   VALUE LOW-VALUE.               
017300                                                                          
017400     03  W-DALASTN-X.                                                     
017500       05  W-DALASTN           PIC  9(8)   VALUE ZERO.                    
017600     03  W-IDTRPTNR-X.                                                    
017700       05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO COMP-3.             
017800     03  W-IDZON-MIN-X.                                                   
017900       05  W-IDZON-MIN         PIC  X(2)   VALUE SPACE.                   
018000     03  W-IDZON-MAX-X.                                                   
018100       05  W-IDZON-MAX         PIC  X(2)   VALUE SPACE.                   
018200     03  W-IDKOLLI-4-X.                                                   
018300       05  W-IDKOLLI-4         PIC S9(5)   VALUE ZERO COMP-3.             
018400     03  W-IDGMTREF-X.                                                    
018500       05  W-IDDISTR-4         PIC S9(5)   VALUE ZERO COMP-3.             
018600       05  W-IDKUNDNR-4        PIC S9(7)   VALUE ZERO COMP-3.             
018700       05  W-IDKUNDRF.                                                    
018800         07  W-IDORDNR7-4    PIC 9(7)    VALUE ZERO.                      
018900         07  FILLER          PIC X(3)    VALUE SPACE.                     
019000     03  W-IDLBBET-X.                                                     
019100       05  W-IDLBBET           PIC X(12)   VALUE SPACE.                   
019200                                                                          
019300     03  W-IDDC-X.                                                        
019400       05  W-IDDC              PIC X(2)    VALUE SPACE.                   
019500                                                                          
019600*    --- STATUS-KOD FRÅN IMS                                              
019700 01  STATUS-WS                   PIC XX.                                  
019800     88  SEGMENT-FINNS                       VALUE '  '.                  
019900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020200 01  STATUS-WS-B2                PIC XX.                                  
020300 01  STATUS-WS-E1                PIC XX.                                  
020400     SKIP2                                                                
020500 01  GODK-STATUSKODER.                                                    
020600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020700     SKIP3                                                                
020800 01  SSA1                        PIC X(128).                              
020900 01  SSA2                        PIC X(64).                               
021000 01  SSA3                        PIC X(64).                               
021100     EJECT                                                                
021200*    --- IMS FUNKTIONSKODER                                               
021300*01  -COPY W0003                                                          
021400     EJECT                                                                
021500                                                                          
021600*    ---  DLI INPUT-OUTPUT AREA                                           
021700                                                                          
021800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE401'.           
021900 01  DLI-IO-WDE401.                                                       
022000*    03  -COPY WDE401                                                     
022100     EJECT                                                                
022200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE411'.           
022300 01  DLI-IO-WDE411.                                                       
022400*    03  -COPY WDE411                                                     
022500     EJECT                                                                
022600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE101'.           
022700 01  DLI-IO-WDE101.                                                       
022800*    03  -COPY WDE101                                                     
022900     EJECT                                                                
023000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE111'.           
023100 01  DLI-IO-WDE111.                                                       
023200*    03  -COPY WDE111                                                     
023300     EJECT                                                                
023400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE121'.           
023500 01  DLI-IO-WDE121.                                                       
023600*    03  -COPY WDE121                                                     
023700     EJECT                                                                
023800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDE131'.           
023900 01  DLI-IO-WDE131.                                                       
024000*    03  -COPY WDE131                                                     
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)   VALUE 'WDGX-4494  '.         
024300 01  DLI-IO-4494.                                                         
024400*    03  -COPY WDGX4494                                                   
024500 01  FILLER                      PIC X(16)   VALUE 'WDR401'.              
024600 01  DLI-IO-WDR401.                                                       
024700*    03  -COPY  WDGX01                                                    
024800 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB601'.            
024900 01  DLI-IO-AREA-B601.                                                    
025000*    03  -COPY WDB601                                                     
025100 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB201'.            
025200 01  DLI-IO-WDB201.                                                       
025300*    03  -COPY WDB201                                                     
025400 LINKAGE SECTION.                                                         
025500                                                                          
025600 01  IO-PCB        PIC X.                                                 
025700                                                                          
025800*01  -COPY W0009  -PRE NLSI-                                              
025900     EJECT                                                                
026000*01  -COPY W0009  -PRE SRSI-                                              
026100     EJECT                                                                
026200*01  -COPY W0008  -PRE WDE1-                                              
026300     05  FILLER                  PIC X.                                   
026400*01  -COPY W0008  -PRE WDE4B-                                             
026500     05  FILLER                  PIC X.                                   
026600*01  -COPY W0008  -PRE 4494-                                              
026700     05  FILLER                  PIC X.                                   
026800*01  -COPY W0008  -PRE WDB6-                                              
026900     05  FILLER                  PIC X.                                   
027000*01  -COPY W0008  -PRE WDB2-                                              
027100     05  FILLER                  PIC X.                                   
027200     EJECT                                                                
027300 PROCEDURE DIVISION  USING          IO-PCB  NLSI-PCB                      
027400                                            SRSI-PCB                      
027500                                            WDE1-PCB                      
027600                                            WDE4B-PCB                     
027700                                            4494-PCB                      
027800                                            WDB6-PCB                      
027900                                            WDB2-PCB.                     
028000 MAIN SECTION.                                                            
028100     ENTRY 'DLITCBL' USING          IO-PCB  NLSI-PCB                      
028200                                            SRSI-PCB                      
028300                                            WDE1-PCB                      
028400                                            WDE4B-PCB                     
028500                                            4494-PCB                      
028600                                            WDB6-PCB                      
028700                                            WDB2-PCB.                     
028800                                                                          
028900     PERFORM A-INIT                                                       
029000     PERFORM UNTIL  RECV-KDRC > 0                                         
029100       PERFORM B-KOLLA-NYCKLAR                                            
029200       IF NYCKLAR-OK                                                      
029300         PERFORM IMS-GU-WDE101                                            
029400         MOVE SHIP-IDDC        TO W-IDDC                                  
029500         PERFORM IMS-GU-WDB601                                            
029600         PERFORM IMS-GNP-WDE111                                           
029700         MOVE SGMT-IDDISTR     TO WS-SPAR-IDDISTR                         
029800         PERFORM H-LETA-MOTTAGARE                                         
029900         PERFORM S01-OPEN-WZ01                                            
030000                                                                          
030100         PERFORM IMS-GU-WDE101                                            
030200         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
030300             PERFORM IMS-GNP-WDE111                                       
030400             MOVE STATUS-WS        TO STATUS-WS-E1                        
030500             MOVE SGMT-IDDISTR     TO W-IDDISTR                           
030600                                      W-IDDISTR-B2                        
030700             MOVE SGMT-IDKUNDNR    TO W-IDKUNDNR                          
030800                                      W-IDKUNDNR-B2                       
030900             IF WS-SPAR-IDDISTR NOT = SGMT-IDDISTR                        
031000               PERFORM S03-CLOSE-WZ01                                     
031100               PERFORM H-LETA-MOTTAGARE                                   
031200               PERFORM S01-OPEN-WZ01                                      
031300               MOVE SGMT-IDDISTR  TO WS-SPAR-IDDISTR                      
031400             END-IF                                                       
031500           PERFORM UNTIL SEGMENT-SAKNAS                                   
031600              PERFORM IMS-GNP-WDE121                                      
031700              MOVE SKOLLI-IDPRODNR  TO W-IDPRODNR                         
031800              MOVE SKOLLI-IDKOLLI   TO W-IDKOLLI                          
031900             PERFORM UNTIL SEGMENT-SAKNAS                                 
032000                                                                          
032010              IF SKOLLI-FLCROSS = NEJ                                     
032100               PERFORM IMS-GNP-WDE131                                     
032200               PERFORM UNTIL SEGMENT-SAKNAS                               
032300                 PERFORM G-SKAPA-SEND-DATA                                
032400                 PERFORM IMS-GNP-WDE131                                   
032500               END-PERFORM                                                
032600               PERFORM IMS-GNP-WDE121                                     
032610              ELSE                                                        
032620               PERFORM IMS-GNP-WDE121                                     
032630              END-IF                                                      
032700               MOVE SKOLLI-IDPRODNR  TO W-IDPRODNR                        
032800               MOVE SKOLLI-IDKOLLI   TO W-IDKOLLI                         
032900             END-PERFORM                                                  
033000             PERFORM IMS-GNP-WDE111                                       
033100             MOVE STATUS-WS        TO STATUS-WS-E1                        
033200             MOVE SGMT-IDDISTR     TO W-IDDISTR                           
033300                                      W-IDDISTR-B2                        
033400             MOVE SGMT-IDKUNDNR    TO W-IDKUNDNR                          
033500                                      W-IDKUNDNR-B2                       
033600             IF WS-SPAR-IDDISTR NOT = SGMT-IDDISTR                        
033700*          OM NY MOTTAGARE  STÄNG OCH ÖPPNA IGEN                          
033800               PERFORM S03-CLOSE-WZ01                                     
033900               PERFORM H-LETA-MOTTAGARE                                   
034000               PERFORM S01-OPEN-WZ01                                      
035000               MOVE SGMT-IDDISTR  TO WS-SPAR-IDDISTR                      
035100             END-IF                                                       
035200           END-PERFORM                                                    
035300                                                                          
035400         END-PERFORM                                                      
035500                                                                          
035600         PERFORM S03-CLOSE-WZ01                                           
035700                                                                          
035800        ELSE                                                              
035900         MOVE 'SEGMENT SAKNAS'   TO FELTEXT                               
036000         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
036100        END-IF                                                            
036200       PERFORM S12-RECV-MESSAGE                                           
036300     END-PERFORM                                                          
036400     IF RECV-KDRC > 1                                                     
036500       MOVE 'WZ01-RECV AVSLUTAS FEL..'  TO FELTEXT                        
036600       CALL FELLOG                                                        
036700     END-IF                                                               
036800*                                                                         
036900     PERFORM S13-RECV-CLOSE                                               
037000     MOVE ZERO TO RETURN-CODE                                             
037100     GOBACK                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 A-INIT SECTION.                                                          
037500     MOVE 'A-INIT'              TO WS-SEKTION                             
037600                                                                          
037700     PERFORM S11-RECV-OPEN                                                
037800     PERFORM S12-RECV-MESSAGE                                             
037900                                                                          
038000     ACCEPT DAGENS-DATUM    FROM DATE                                     
038100     MOVE LOW-VALUE         TO W-IDZON-MIN                                
038200     MOVE HIGH-VALUE        TO W-IDZON-MAX                                
038300     .                                                                    
038400     EJECT                                                                
038500 B-KOLLA-NYCKLAR SECTION.                                                 
038600     MOVE 'B-KOLLA-NYCKLAR'     TO WS-SEKTION                             
038700                                                                          
038800                                                                          
038900     MOVE JA                    TO NYCKLAR-SW                             
039000                                                                          
039100     IF MID-IDSHIPM         = ALL '+'                                     
039200       MOVE NEJ TO NYCKLAR-SW                                             
039300     ELSE                                                                 
039400       IF MID-IDSHIPM NUMERIC                                             
039500         MOVE MID-IDSHIPM     TO W-IDSHIPM                                
039600       ELSE                                                               
039700         MOVE NEJ TO NYCKLAR-SW                                           
039800       END-IF                                                             
039900     END-IF                                                               
040000                                                                          
040100     IF NYCKLAR-FEL                                                       
040200       STRING 'NYCKLAR FEL '                                              
040300            DELIMITED BY SIZE INTO FELTEXT                                
040400       CALL FELLOG                                                        
040500                                                                          
040600     END-IF                                                               
040700     MOVE ZERO                  TO W-RAD-RAKNARE                          
040800     .                                                                    
040900     EJECT                                                                
041000 G-SKAPA-SEND-DATA  SECTION.                                              
041100     MOVE 'G-SKAPA-SEND-DATA'   TO WS-SEKTION                             
041200                                                                          
041300     PERFORM GA-RADINFO                                                   
041400*    PERFORM GB-HAMTA-BOLLANR                                             
041500     PERFORM IMS-GU-WDB201                                                
041600     MOVE STATUS-WS        TO STATUS-WS-B2                                
041700     IF GMT-FLLDCKND = NEJ                                                
041800*             SKICKA INGEN TRANS                                          
041900       CONTINUE                                                           
042000     ELSE                                                                 
042100       PERFORM S02-PUT-RAD-WZ01                                           
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 GA-RADINFO  SECTION.                                                     
042600     MOVE 'GA-RADINFO'          TO WS-SEKTION                             
042700                                                                          
042800**   MOVE 1                     TO UT-REQU-IDMSGVER                       
042900**   MOVE SPACE                 TO UT-REQU-KDPGMACT                       
043000**   MOVE 'W4063000'            TO UT-REQU-IDUSER                         
043100     MOVE 'RSI'                 TO UT-RSI-IDPTYP                          
043200     MOVE SHIP-IDDC             TO UT-RSI-IDDC                            
043300     MOVE W-IDSHIPM             TO UT-RSI-IDSHIPM                         
043400     MOVE SHIP-TISKEPPN         TO UT-RSI-TISKEPPN                        
043500     MOVE SKOLLI-IDDISTR        TO UT-RSI-IDDISTR                         
043600     MOVE SKOLLI-IDKUNDNR       TO UT-RSI-IDKUNDNR                        
043700     MOVE SKOLLI-IDORDNR7       TO UT-RSI-IDORDNR7                        
043800     MOVE SGMT-FLCOD            TO UT-RSI-FLCOD                           
043900     MOVE SKOLLI-KDORDKL        TO UT-RSI-KDORDKL                         
044000**   MOVE SKOLLI-IDPRODNR       TO UT-RSI-IDPRODNR                        
044100     MOVE SKOLLI-IDKOLLI        TO UT-RSI-IDKOLLI                         
044200**   MOVE SRAD-IDPURAD          TO UT-RSI-IDPURAD                         
044300     MOVE SRAD-IDARTNR          TO UT-RSI-IDARTNR                         
044400     MOVE SRAD-REKSIFFR         TO UT-RSI-REKSIFFR                        
044500     MOVE SRAD-PRARTNTO-LOC     TO UT-RSI-PRARTNTO-LOC                    
044600     MOVE NEJ                   TO UT-RSI-FLARTSTD                        
044700     IF SRAD-PRARTNTO-LOC = ZERO                                          
044800       MOVE SRAD-PRARTNTO-LOCPREL TO UT-RSI-PRARTNTO-LOC                  
044900       MOVE JA                  TO UT-RSI-FLARTSTD                        
045000     END-IF                                                               
045100     MOVE SRAD-KVLEVART         TO UT-RSI-KVLEVART                        
045200     MOVE SKOLLI-IDPRODNR       TO W-IDPRODNR-MIN                         
045300                                   W-IDPRODNR-MAX                         
045400     MOVE SRAD-IDPURAD          TO W-IDPURAD-MIN                          
045500                                   W-IDPURAD-MAX                          
045600     PERFORM IMS-GU-WDE411-BSEQ                                           
045700     MOVE ORAD-KVBEART          TO UT-RSI-KVBEART                         
045800     MOVE ORAD-KDDSP            TO UT-RSI-KDDSP                           
045900     MOVE ORAD-BERADREF         TO UT-RSI-BERADREF                        
046000     MOVE ZERO                  TO UT-RSI-IDRONR                          
046100     MOVE ORAD-IDKUNDRF-RO      TO W-KUNDREF                              
046200     IF W-IDKUNDRF-RO > ZERO                                              
046300       MOVE W-IDKUNDRF-RO       TO UT-RSI-IDRONR                          
046400     END-IF                                                               
046500     MOVE SPACE                 TO UT-RSI-FLIHOP                          
046600     IF ORAD-TIRODAT = ZERO                                               
046700       MOVE JA                  TO UT-RSI-FLIHOP                          
046800     END-IF                                                               
046900     MOVE ORAD-BEVOLREF         TO UT-RSI-BEVOLREF                        
047000     MOVE SPACE                 TO UT-RSI-FILLER                          
047100                                                                          
047200     .                                                                    
047300     EJECT                                                                
047400 GB-HAMTA-BOLLANR  SECTION.                                               
047500     IF DCS-SDC AND DCS-IDLANDX2 = 'IT'                                   
047600       MOVE W-DALASTN(3:6)     TO WS-DALASTN                              
047700       IF SHIP-TISKEPPN = WS-DALASTN  AND                                 
047800         SHIP-IDLBBET = W-IDLBBET AND                                     
047900*        SKOLLI-IDKOLLI = W-IDKOLLI-4  AND                                
048000         SGMT-IDDISTR = W-IDDISTR-4  AND                                  
048100         SGMT-IDKUNDNR = W-IDKUNDNR-4  AND                                
048200         SKOLLI-IDORDNR7 = W-IDORDNR7-4  AND                              
048300         SHIP-IDDC  =  W-IDDC-4491                                        
048400         MOVE 4494-IDTRPBON           TO UT-RSI-IDSHIPM                   
048500       ELSE                                                               
048600         MOVE SHIP-IDDC               TO W-IDDC-4491                      
048700         PERFORM IMS-GU-WDGX4491                                          
048800         IF SEGMENT-FINNS                                                 
048900           MOVE SHIP-TISKEPPN         TO W-DALASTN(2:7)                   
049000           MOVE 20                    TO W-DALASTN(1:2)                   
049100           MOVE SHIP-IDLBBET          TO W-IDLBBET                        
049200           MOVE SHIP-IDTRPTNR         TO W-IDTRPTNR                       
049300           MOVE SKOLLI-IDKOLLI        TO W-IDKOLLI-4                      
049400           MOVE SGMT-IDDISTR          TO W-IDDISTR-4                      
049500           MOVE SGMT-IDKUNDNR         TO W-IDKUNDNR-4                     
049600           MOVE SKOLLI-IDORDNR7       TO W-IDORDNR7-4                     
049700           PERFORM IMS-GNP-WDGX4494                                       
049800           IF SEGMENT-FINNS                                               
049900             MOVE 4494-IDTRPBON       TO UT-RSI-IDSHIPM                   
050000           ELSE                                                           
050100             MOVE SPACE               TO 4494-IDTRPBOT                    
050200           END-IF                                                         
050300         END-IF                                                           
050400       END-IF                                                             
050500       IF UT-RSI-IDSHIPM = SPACE                                          
050600         MOVE W-IDSHIPM               TO UT-RSI-IDSHIPM                   
050700       END-IF                                                             
050800     END-IF                                                               
050900     .                                                                    
051000     EJECT                                                                
051100 H-LETA-MOTTAGARE SECTION.                                                
051200                                                                          
051300     MOVE SGMT-IDDISTR          TO DIS1-IDDISTR                           
051400     CALL W460DIS1 USING DIS1-W460DIS1                                    
051500     EVALUATE DIS1-IDLANDX2                                               
051600      WHEN 'AT'                                                           
051700        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
051800      WHEN 'BE'                                                           
051900        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
052000      WHEN 'CH'                                                           
052100        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
052200      WHEN 'DE'                                                           
052300        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
052400*     WHEN 'ES'                                                           
052500*       MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
052600      WHEN 'FI'                                                           
052700        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
052800      WHEN 'FR'                                                           
052900        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
053000      WHEN 'GB'                                                           
053100        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
053200      WHEN 'IT'                                                           
053300        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
053400      WHEN 'NL'                                                           
053500        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
053600      WHEN 'PL'                                                           
053700        MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
053900*     WHEN 'SE'                                                           
054000*       MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                         
054100      WHEN OTHER                                                          
054200        MOVE 'OV'         TO WS-ADDISPABS-IDLAND                          
054300*       DISPLAY IN-RSI-IDSHIPM 'OVR'                                      
054400*       DISPLAY WS-ADDISPABS                                              
054500     END-EVALUATE                                                         
054600     .                                                                    
054700     EJECT                                                                
054800 S01-OPEN-WZ01 SECTION.                                                   
054900     MOVE 'S01-OPEN-WZ01'            TO WS-SEKTION                        
055000                                                                          
055100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
055200     MOVE WS-ADDISPABS               TO SEND-ADDISPABS                    
055300     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
055400                                                                          
055500     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
055600                                SEND-OPEN-AREA                            
055700     IF SEND-KDRC > 0                                                     
055800       MOVE SEND-KDRC           TO KDRC-DISP                              
055900       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
056000            DELIMITED BY SIZE INTO FELTEXT                                
056100       CALL FELLOG                                                        
056200     ELSE                                                                 
056300       MOVE SEND-IDCOM               TO WS-IDCOM                          
056400     END-IF                                                               
056500     .                                                                    
056600     EJECT                                                                
056700 S02-PUT-RAD-WZ01 SECTION.                                                
056800     MOVE 'S02-PUT-RAD-WZ01'         TO WS-SEKTION                        
056900*--------------                                                           
057000     ADD 1 TO ANTAL-SEND                                                  
057100**   DISPLAY 'W4063000 ' ANTAL-SEND ' ' UT-AREA(1:30)                     
057200                                                                          
057300     MOVE 'PUT'                 TO SEND-KDFUNC                            
057400     MOVE LENGTH OF UT-AREA     TO SEND-KVDLEN                            
057500                                                                          
057600     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
057700                                SEND-KVDLEN                               
057800                                UT-AREA                                   
057900     IF SEND-KDRC > 1                                                     
058000       MOVE SEND-KDRC           TO KDRC-DISP                              
058100       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
058200            DELIMITED BY SIZE INTO FELTEXT                                
058300       CALL FELLOG                                                        
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 S03-CLOSE-WZ01  SECTION.                                                 
058800     MOVE 'S03-CLOSE-WZ01'      TO WS-SEKTION                             
058900                                                                          
059000     MOVE 'CLOSE'               TO SEND-KDFUNC                            
059100                                                                          
059200     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
059300     IF SEND-KDRC > 0                                                     
059400       MOVE SEND-KDRC           TO KDRC-DISP                              
059500       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
059600            DELIMITED BY SIZE INTO FELTEXT                                
059700       CALL FELLOG                                                        
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 S11-RECV-OPEN SECTION.                                                   
060200     MOVE 'S11-RECV-OPEN'          TO WS-SEKTION                          
060300                                                                          
060400     MOVE 'OPEN'                   TO RECV-KDFUNC                         
060500     MOVE 'CARPARTS.PULS.RSITRANS' TO RECV-ADDISPABS                      
060600                                                                          
060700     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
060800                                   RECV-OPEN-AREA                         
060900                                                                          
061000     IF RECV-KDRC > 0                                                     
061100      MOVE RECV-KDRC               TO KDRC-DISP                           
061200      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
061300        DELIMITED BY SIZE INTO FELTEXT                                    
061400      CALL FELLOG                                                         
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 S12-RECV-MESSAGE SECTION.                                                
061900     MOVE 'S12-RECV-MESSAGE'      TO WS-SEKTION                           
062000                                                                          
062100     MOVE 'GET'                    TO RECV-KDFUNC                         
062200     MOVE LENGTH OF MID-W40630I1   TO RECV-KVDLEN                         
062300     CALL WZ01RECV USING RECV-CONTROL-AREA                                
062400                         RECV-KVDLEN                                      
062500                         MID-W40630I1                                     
062600*                                                                         
062700     IF RECV-KDRC > 1                                                     
062800       MOVE RECV-KDRC            TO KDRC-DISP                             
062900       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
063000         DELIMITED BY SIZE INTO FELTEXT                                   
063100       CALL FELLOG                                                        
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 S13-RECV-CLOSE SECTION.                                                  
063600     MOVE 'S13-RECV-CLOSE'      TO WS-SEKTION                             
063700                                                                          
063800     MOVE 'CLOSE'                TO RECV-KDFUNC                           
063900     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
064000*                                                                         
064100     IF RECV-KDRC > 0                                                     
064200       MOVE RECV-KDRC            TO KDRC-DISP                             
064300       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
064400         DELIMITED BY SIZE INTO FELTEXT                                   
064500       CALL FELLOG                                                        
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900* --- IMS SEKTIONER ---                                                   
065000     SKIP3                                                                
065100 IMS-GU-WDE101 SECTION.                                                   
065200     MOVE 'IMS-GU-WDE101'      TO WS-SEKTION                              
065300                                                                          
065400     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
065500          DELIMITED BY SIZE INTO SSA1                                     
065600     MOVE '    '               TO GODK-STATUSKODER                        
065700     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
065800     MOVE WDE1-STATUS-CODE     TO STATUS-WS                               
065900     PERFORM IMS-STATUSKONTROLL                                           
066000     .                                                                    
066100     SKIP2                                                                
066200 IMS-GU-WDE111 SECTION.                                                   
066300     MOVE 'IMS-GU-WDE111'      TO WS-SEKTION                              
066400                                                                          
066500     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
066600          DELIMITED BY SIZE INTO SSA1                                     
066700     MOVE '  GE'                TO GODK-STATUSKODER                       
066800     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1                    
066900     MOVE WDE1-STATUS-CODE      TO STATUS-WS                              
067000     PERFORM IMS-STATUSKONTROLL                                           
067100     .                                                                    
067200     SKIP2                                                                
067300 IMS-GNP-WDE111 SECTION.                                                  
067400     MOVE 'IMS-GNP-WDE111'      TO WS-SEKTION                             
067500                                                                          
067600     MOVE 'WDE111  '            TO SSA1                                   
067700     MOVE '  GE' TO GODK-STATUSKODER                                      
067800     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
067900     MOVE WDE1-STATUS-CODE      TO STATUS-WS                              
068000     PERFORM IMS-STATUSKONTROLL                                           
068100     .                                                                    
068200     SKIP2                                                                
068300 IMS-GNP-WDE121 SECTION.                                                  
068400     MOVE 'IMS-GNP-WDE121'      TO WS-SEKTION                             
068500                                                                          
068600     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
068700          DELIMITED BY SIZE INTO SSA1                                     
068800     MOVE 'WDE121  '            TO SSA2                                   
068900     MOVE '  GE' TO GODK-STATUSKODER                                      
069000     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
069100     MOVE WDE1-STATUS-CODE      TO STATUS-WS                              
069200     PERFORM IMS-STATUSKONTROLL                                           
069300     .                                                                    
069400     SKIP2                                                                
069500 IMS-GNP-WDE131 SECTION.                                                  
069600     MOVE 'IMS-GNP-WDE131'      TO WS-SEKTION                             
069700                                                                          
069800     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
069900          DELIMITED BY SIZE INTO SSA1                                     
070000     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
070100          DELIMITED BY SIZE INTO SSA2                                     
070200     MOVE 'WDE131  '            TO SSA3                                   
070300     MOVE '  GE'   TO GODK-STATUSKODER                                    
070400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2 SSA3         
070500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
070600     PERFORM IMS-STATUSKONTROLL                                           
070700     .                                                                    
070800     SKIP2                                                                
070900 IMS-GU-WDE411-BSEQ SECTION.                                              
071000     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4BSEQ-MIN-X                        
071100                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ') '                   
071200          DELIMITED BY SIZE INTO SSA1                                     
071300     MOVE '  GE' TO GODK-STATUSKODER                                      
071400     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-WDE411 SSA1                   
071500     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800     SKIP2                                                                
071900 IMS-GU-WDGX4491  SECTION.                                                
072000     MOVE 'IMS-GU-WDGX4491'     TO WS-SEKTION                             
072100                                                                          
072200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4491-X ')'                    
072300          DELIMITED BY SIZE INTO SSA1                                     
072400     MOVE '  GE' TO GODK-STATUSKODER                                      
072500     CALL CBLTDLI USING GU 4494-PCB DLI-IO-WDR401 SSA1                    
072600     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
072700     PERFORM IMS-STATUSKONTROLL                                           
072800     .                                                                    
072900     SKIP3                                                                
073000 IMS-GNP-WDGX4494 SECTION.                                                
073100     MOVE 'IMS-GNP-WDGX4494'   TO WS-SEKTION                              
073200                                                                          
073300     STRING 'WDGX4494*F(DALASTN  =' W-DALASTN-X                           
073400**                    '&IDTRPTNR =' W-IDTRPTNR-X                          
073500                      '&IDZON   >=' W-IDZON-MIN-X                         
073600                      '&IDZON   <=' W-IDZON-MAX-X                         
073700                      '&IDGMTREF =' W-IDGMTREF-X                          
073800                      '&IDLBBET  =' W-IDLBBET-X                           
073900                      '&IDKOLLI  =' W-IDKOLLI-4-X ')'                     
074000          DELIMITED BY SIZE INTO SSA1                                     
074100     MOVE '  GE' TO GODK-STATUSKODER                                      
074200     CALL CBLTDLI USING GNP 4494-PCB DLI-IO-4494 SSA1                     
074300     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
074400     PERFORM IMS-STATUSKONTROLL                                           
074500     .                                                                    
074600     SKIP3                                                                
074700 IMS-GU-WDB601    SECTION.                                                
074800                                                                          
074900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
075000          DELIMITED BY SIZE INTO SSA1                                     
075100     MOVE '  GE' TO GODK-STATUSKODER                                      
075200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
075300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     IF SEGMENT-SAKNAS                                                    
075600         MOVE SPACE TO DCS-KDDC                                           
075700     END-IF                                                               
075800     .                                                                    
075900     SKIP3                                                                
076000 IMS-GU-WDB201    SECTION.                                                
076100                                                                          
076200     STRING 'WDB201  (IDGMT    =' W-WDB2KY-X ')'                          
076300          DELIMITED BY SIZE INTO SSA1                                     
076400     MOVE '  GE'              TO GODK-STATUSKODER                         
076500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201  SSA1                   
076600     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
076700     PERFORM IMS-STATUSKONTROLL                                           
076800     .                                                                    
076900     SKIP3                                                                
077000 IMS-STATUSKONTROLL SECTION.                                              
077100                                                                          
077200     SET STATUS-IX TO 1                                                   
077300     SEARCH GODK-STATUS                                                   
077400       AT END                                                             
077500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
077600         DELIMITED BY SIZE INTO FELTEXT                                   
077700         CALL FELLOG                                                      
077800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
077900         CONTINUE                                                         
078000     END-SEARCH                                                           
079000     .                                                                    
