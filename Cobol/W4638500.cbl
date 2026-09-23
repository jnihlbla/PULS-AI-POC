000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4638500.                                                
000300 AUTHOR.         GERRY CARMICHAEL.                                        
000400 DATE-WRITTEN.   99/03/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*    LÄGGER UPP ORDERBEKRÄFTELSER FÖR DIREKTLEVERANS-ARTIKLAR             
001000*    PÅ DISPATCHERN FÖR VIDARE BEHANDLING I W40360.                       
001100*                                                                         
001200*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
001300*                            WLKOMA-(WDP8)                                
001400*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
001500*                            WL4579-(WDR4)                                
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL                                                      
002600     SELECT W46384                     ASSIGN TO W46385D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W46384                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W46384       -L.                                               
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W4638500'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +3   COMP-3.           
005000 77  POST-ANT                    PIC S9(3)   VALUE +0   COMP-3.           
005100 77  RAD-IX                      PIC S9(4)   VALUE +0  COMP SYNC.         
005200 77  MAX-RAD-IX                  PIC S9(4)   VALUE +10 COMP SYNC.         
005300 77  SPAR-IDPRODNR               PIC 9(7)    VALUE ZERO.                  
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005700                                                                          
005800 77  W46384-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W46384                       VALUE 'J'.                   
006000     SKIP2                                                                
006100                                                                          
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500*                                                                         
006600 01  W-DATUM                     PIC 9(6)    VALUE ZERO.                  
006700 01  W-TIKLOCK                   PIC 9(8)    VALUE ZERO.                  
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W46384      -PRE IN-                                      
008600*                                                                         
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  FILLER          PIC X(16)   VALUE 'NYCKLAR TILL DLI'.                
009100     SKIP3                                                                
009200*01  -COPY WDGX01                                                         
009300     EJECT                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010000     88  IMS-EJ-OK                           VALUE 'XD'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200                                                                          
011300 01  FILLER         PIC X(16) VALUE '4580-IO-AREA'.                       
011400 01  4580-IO-AREA.                                                        
011500*    03  -COPY WDGX4580                                                   
011600                                                                          
011700     EJECT                                                                
011800 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
011900*01  -COPY WMSGKOM                                                        
012000     EJECT                                                                
012100                                                                          
012200 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
012300     SKIP3                                                                
012400*01  -COPY WMSGAREA                                                       
012500     EJECT                                                                
012600*                                                                         
012700*    --- AREOR FÖR W006KOM SUBMODUL                                       
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
013000 01  KOM-IO-AREA.                                                         
013100   03  KOM-AREA                     PIC X(1000) VALUE SPACE.              
013200*03  FILLER  -COPY W4I36001   -RED KOM-AREA.                              
013300                                                                          
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600*01  -COPY W0009   -PRE MSG-                                              
013700                                                                          
013800 01  DISP-PCB                PIC X.                                       
013900 01  KOMA-PCB                PIC X.                                       
014000                                                                          
014100*01  -COPY W0008  -PRE 4579-                                              
014200     05  FILLER              PIC X.                                       
014300     EJECT                                                                
014400 PROCEDURE DIVISION  USING MSG-PCB                                        
014500                           DISP-PCB                                       
014600                           KOMA-PCB                                       
014700                           4579-PCB.                                      
014800 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB                                        
015000                           DISP-PCB                                       
015100                           KOMA-PCB                                       
015200                           4579-PCB.                                      
015300                                                                          
015400     PERFORM A-INITIERA                                                   
015500                                                                          
015600     PERFORM IMS-LAS-ATERSTART                                            
015700                                                                          
015800     IF SEGMENT-SAKNAS                                                    
015900        MOVE SPACE        TO 4580-WDGX4580-CTX                            
016000        MOVE '1'          TO 4580-KDSEGKEY                                
016100        MOVE ZERO         TO 4580-KVPOST                                  
016200        MOVE W-DATUM      TO 4580-TIUPPDAT                                
016300        MOVE W-TIKLOCK    TO 4580-TIUPPTID                                
016400                                                                          
016500        PERFORM IMS-ISRT-ATERSTART                                        
016600        PERFORM IMS-LAS-ATERSTART                                         
016700     END-IF                                                               
016800                                                                          
016900     IF 4580-KVPOST > +0                                                  
017000        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
017100     ELSE                                                                 
017200        PERFORM S01-LAS-W46384                                            
017300     END-IF                                                               
017400                                                                          
017500     IF NOT END-OF-W46384                                                 
017600        MOVE IN-IDPRODNR TO SPAR-IDPRODNR                                 
017700        PERFORM S03-SKAPA-MSG-KOM-AREA                                    
017800     END-IF                                                               
017900                                                                          
018000     PERFORM UNTIL END-OF-W46384                                          
018100        PERFORM C-BEARBETA                                                
018200        PERFORM S01-LAS-W46384                                            
018300     END-PERFORM                                                          
018400                                                                          
018500     PERFORM Z-FINIT                                                      
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000                                                                          
019100 A-INITIERA SECTION.                                                      
019200                                                                          
019300     PERFORM IMS-RESTART                                                  
019400                                                                          
019500     OPEN INPUT W46384                                                    
019600                                                                          
019700     MOVE +0                   TO POST-ANT                                
019800                                  CHKP-ANT                                
019900                                  RAD-IX                                  
020000                                  SPAR-IDPRODNR                           
020100                                                                          
020200     MOVE SPACE                TO MSG-AREA                                
020300                                                                          
020400     ACCEPT W-DATUM            FROM DATE                                  
020500     ACCEPT W-TIKLOCK          FROM TIME                                  
020600                                                                          
020700     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
020800     .                                                                    
020900     EJECT                                                                
021000 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
021100                                                                          
021200     PERFORM S01-LAS-W46384                                               
021300                                                                          
021400     PERFORM UNTIL END-OF-W46384  OR                                      
021500                     POST-ANT = 4580-KVPOST                               
021600        PERFORM S01-LAS-W46384                                            
021700        ADD +1           TO POST-ANT                                      
021800     END-PERFORM                                                          
021900                                                                          
022000     IF END-OF-W46384                                                     
022100        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
022200                      TO FELTEXT                                          
022300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
022400     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700                                                                          
022800 C-BEARBETA SECTION.                                                      
022900                                                                          
023000     IF SPAR-IDPRODNR = IN-IDPRODNR AND                                   
023100        RAD-IX < MAX-RAD-IX                                               
023200       CONTINUE                                                           
023300     ELSE                                                                 
023400       PERFORM S05-AVSLUTA-TRANS                                          
023500       PERFORM X-TAG-CHECKPOINT                                           
023600       MOVE IN-IDPRODNR TO SPAR-IDPRODNR                                  
023700       PERFORM S03-SKAPA-MSG-KOM-AREA                                     
023800     END-IF                                                               
023900                                                                          
024000     PERFORM CA-SKAPA-RENSNINGSTRANS                                      
024100                                                                          
024200     .                                                                    
024300     EJECT                                                                
024400 CA-SKAPA-RENSNINGSTRANS SECTION.                                         
024500                                                                          
024600     ADD +1 TO RAD-IX                                                     
024700                                                                          
024800     IF RAD-IX = +1                                                       
024900                                                                          
025000       MOVE SPACE                TO KOM-AREA                              
025100                                                                          
025200       COMPUTE MSG-KVLL = LENGTH OF MID-W4I36001 + 17                     
025300                                                                          
025400       MOVE LOW-VALUE            TO MSG-KDZ1                              
025500       MOVE LOW-VALUE            TO MSG-KDZ2                              
025600       MOVE 'W4T360X '           TO MSG-KDTRANS-1                         
025700       MOVE '4360'               TO MSG-IDTRANS-1                         
025800       MOVE '1'                  TO MSG-KDMFSFOR-1                        
025900                                                                          
026000       MOVE IN-DABEKDAT          TO MID-DABEKDAT                          
026200       MOVE IN-IDDC              TO MID-IDDC                              
026300       MOVE IN-IDDISTR           TO MID-IDDISTR                           
026400       MOVE IN-IDKUNDNR          TO MID-IDKUNDNR                          
026500       MOVE IN-IDLEVNR           TO MID-IDLEVNR                           
026600       MOVE IN-IDORDNR7          TO MID-IDORDNR7                          
026700       MOVE IN-IDPRODNR          TO MID-IDPRODNR                          
026800       MOVE IN-TIBEKR            TO MID-TIBEKR                            
026810       MOVE SPACE                TO MID-IDUSER                            
026900     END-IF                                                               
027000                                                                          
027200     MOVE IN-IDARTBET            TO MID-IDARTBET (RAD-IX)                 
027300     MOVE IN-IDARTPRE            TO MID-IDARTPRE (RAD-IX)                 
027400     MOVE IN-IDARTNR             TO MID-IDARTNR  (RAD-IX)                 
027500     MOVE IN-IDRADNR             TO MID-IDRADNR  (RAD-IX)                 
027600     MOVE IN-KDORDBEK            TO MID-KDORDBEK (RAD-IX)                 
027700     MOVE IN-KVBEART             TO MID-KVBEART  (RAD-IX)                 
027710     MOVE IN-DALEVDAT            TO MID-DALEVDAT (RAD-IX)                 
027800     .                                                                    
027900     EJECT                                                                
028000                                                                          
028100 S01-LAS-W46384 SECTION.                                                  
028200     SKIP2                                                                
028300     READ W46384 INTO IN-AREA                                             
028400       AT END                                                             
028500          MOVE JA TO W46384-EOF-SW                                        
028600     END-READ                                                             
028700                                                                          
028800     IF NOT END-OF-W46384                                                 
028900        MOVE 'W46384'       TO POSTSUM-FDNAMN                             
029000        MOVE 'W46384D1'     TO POSTSUM-DDNAMN2                            
029100        MOVE 'REN'          TO POSTSUM-TRANSTYP                           
029200        CALL POSTSUM USING POSTSUM-PARM                                   
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
029800     SKIP2                                                                
029900     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
030000     MOVE +54                    TO MSG-KOM-KVLL                          
030100     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
030200     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
030300     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
030400     MOVE 'W4I36001'             TO MSG-KOM-IDCPYTXT                      
030500     MOVE 'W46385 '              TO MSG-KOM-IDSNDNOD                      
030600     MOVE 'W4638500'             TO MSG-KOM-IDSNDJOB                      
030700     MOVE W-DATUM                TO MSG-KOM-TIREGDAT                      
030800     ADD +1                      TO W-TIKLOCK                             
030900     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
031000     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
031100     .                                                                    
031200     EJECT                                                                
031300                                                                          
031400 S05-AVSLUTA-TRANS SECTION.                                               
031500     SKIP2                                                                
031600                                                                          
031700     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
031800     CALL W006KOM USING MSG-PCB                                           
031900                        DISP-PCB                                          
032000                        KOMA-PCB                                          
032100                        MSG-KOM-WMSGKOM                                   
032200                        MSG-IO-AREA                                       
032300     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
032400*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
032500*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
032600        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W46384 '                    
032700                      TO FELTEXT                                          
032800        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W46384 '                    
032900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
033000     END-IF                                                               
033100                                                                          
033200     MOVE ZERO     TO RAD-IX                                              
033300     MOVE SPACE    TO KOM-AREA                                            
033400     .                                                                    
033500     EJECT                                                                
033600                                                                          
033700 X-TAG-CHECKPOINT SECTION.                                                
033800     SKIP2                                                                
033900*    UPPDATERA ÅTERSTARTREGISTRET                                         
034000     PERFORM IMS-LAS-ATERSTART                                            
034100     ADD +1          TO 4580-KVPOST                                       
034200     ACCEPT 4580-TIUPPDAT FROM DATE                                       
034300     ACCEPT 4580-TIUPPTID FROM TIME                                       
034400                                                                          
034500     PERFORM IMS-REPL-ATERSTART                                           
034600                                                                          
034700*    TAG CHECKPOINT                                                       
034800     PERFORM IMS-CHECKPOINT                                               
034900     .                                                                    
035000     EJECT                                                                
035100                                                                          
035200 Z-FINIT    SECTION.                                                      
035300                                                                          
035400     IF SPAR-IDPRODNR NOT = ZERO                                          
035500       PERFORM S05-AVSLUTA-TRANS                                          
035600     END-IF                                                               
035700                                                                          
035800     CLOSE  W46384                                                        
035900                                                                          
036000*    NOLLA ÅTERSTARTINFORMATIONEN                                         
036100     PERFORM IMS-LAS-ATERSTART                                            
036200     MOVE +0         TO 4580-KVPOST                                       
036300     ACCEPT 4580-TIUPPDAT FROM DATE                                       
036400     ACCEPT 4580-TIUPPTID FROM TIME                                       
036500                                                                          
036600     PERFORM IMS-REPL-ATERSTART                                           
036700                                                                          
036800     MOVE 'S'      TO POSTSUM-OPKOD                                       
036900     CALL POSTSUM USING POSTSUM-PARM                                      
037000     .                                                                    
037100     EJECT                                                                
037200                                                                          
037300* IMS SECTIONER                                                           
037400     SKIP3                                                                
037500                                                                          
037600 IMS-RESTART SECTION.                                                     
037700     SKIP2                                                                
037800     MOVE SPACE TO MSG-IO-AREA                                            
037900     MOVE '  ' TO GODK-STATUSKODER                                        
038000     CALL CBLTDLI USING XRST MSG-PCB                                      
038100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038200                        CHKP-AREA-LENGTH CHKP-AREA                        
038300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038400     PERFORM IMS-STATUSKONTROLL                                           
038500     .                                                                    
038600                                                                          
038700 IMS-CHECKPOINT SECTION.                                                  
038800     MOVE SPACE        TO MSG-IO-AREA                                     
038900     MOVE '  XD'       TO GODK-STATUSKODER                                
039000     CALL CBLTDLI USING CHKP MSG-PCB                                      
039100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039200                        CHKP-AREA-LENGTH CHKP-AREA                        
039300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     IF IMS-EJ-OK                                                         
039600       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
039700       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
039800                            TO FELTEXT                                    
039900       CALL FELLOG                                                        
040000     END-IF                                                               
040100     .                                                                    
040200                                                                          
040300 IMS-LAS-ATERSTART SECTION.                                               
040400     SKIP2                                                                
040500     MOVE '4579'         TO IDHTYP                                        
040600     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
040700     MOVE 'W4638500'     TO NYCKEL-VALFRI(1:8)                            
040800     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
040900                    DELIMITED BY SIZE INTO SSA1                           
041000     MOVE 'WL457911 '    TO SSA2                                          
041100     MOVE '  GE'           TO GODK-STATUSKODER                            
041200     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
041300     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     .                                                                    
041600                                                                          
041700 IMS-ISRT-ATERSTART SECTION.                                              
041800     SKIP2                                                                
041900     MOVE '4579'         TO IDHTYP                                        
042000     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
042100     MOVE 'W4638500'     TO NYCKEL-VALFRI(1:8)                            
042200     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
042300                    DELIMITED BY SIZE INTO SSA1                           
042400     MOVE 'WL457911 '    TO SSA2                                          
042500     MOVE '  '           TO GODK-STATUSKODER                              
042600     CALL CBLTDLI USING ISRT 4579-PCB 4580-IO-AREA SSA1 SSA2              
042700     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
042800     PERFORM IMS-STATUSKONTROLL                                           
042900     .                                                                    
043000                                                                          
043100 IMS-REPL-ATERSTART SECTION.                                              
043200     SKIP2                                                                
043300     MOVE '  '             TO GODK-STATUSKODER                            
043400     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
043500     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800                                                                          
043900     EJECT                                                                
044000 IMS-STATUSKONTROLL SECTION.                                              
044100     SET STATUS-IX TO 1                                                   
044200     SEARCH GODK-STATUS                                                   
044300       AT END                                                             
044400         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
044500                            TO FELTEXT                                    
044600         CALL FELLOG                                                      
044700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044800         CONTINUE                                                         
044900     END-SEARCH                                                           
045000     .                                                                    
