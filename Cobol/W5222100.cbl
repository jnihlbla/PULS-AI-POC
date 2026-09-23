000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5222100.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   02/06/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*        THE PROGRAM                                                      
001000*        - READS PRM-DATA FROM SYSIN                                      
001100*        - READS FILE W52202 AND CREATES A VAT-FILE                       
001200*        - SENDS VAT DATA TO THE FOLLOWING COUNTRIES,                     
001300*          . ENGLAND     (SYMBOLIC PRM = GB) VCOM/ONDEMAND                
001400*          . NETHERLANDS (SYMBOLIC PRM = NL) VCOM/ONDEMAND                
001500*          . BELGIUM     (SYMBOLIC PRM = BE) ONDEMAND                     
001600*          . GERMANY     (SYMBOLIC PRM = DE) VCOM/ONDEMAND                
001700*          . ITALY       (SYMBOLIC PRM = IT) VCOM/ONDEMAND                
001800*          . GREECE      (SYMBOLIC PRM = GR) ONDEMAND                     
001900*          . FRANCE      (SYMBOLIC PRM = FR) ONDEMAND                     
002000*          . SPAIN       (SYMBOLIC PRM = ES) VCOM/ONDEMAND                
002100*          . NORWAY      (SYMBOLIC PRM = NO) ONDEMAND                     
002200*          . PORTUGAL    (SYMBOLIC PRM = PT) VCOM/ONDEMAND                
002300*          . ÖSTERIKE    (SYMBOLIC PRM = AT) VCOM/ONDEMAND                
002400*          . POLAND      (SYMBOLIC PRM = PL) ONDEMAND                     
002500*          . CZECH       (SYMBOLIC PRM = CZ) ONDEMAND                     
002600*          . HUNGARY     (SYMBOLIC PRM = HU) ONDEMAND                     
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500                                                                          
003600*          --- SYSIN FROM JCL                                             
003700     SELECT INDATA                     ASSIGN TO SYSIN.                   
003800                                                                          
003900*          --- SELECTED VAT-DATA FROM IVW-TABLE                           
004000     SELECT W52202                     ASSIGN TO W52221D1.                
004100                                                                          
004200*          --- SELECTED VAT-DATA TO ONDEMAND                              
004300     SELECT W52221                     ASSIGN TO W52221D2.                
004400                                                                          
004500*          --- SELECTED VAT-DATA TO NETHERLANDS                           
004600     SELECT W52222                     ASSIGN TO W52221D3.                
004700                                                                          
004800*          --- SELECTED VAT-DATA TO D&P                                   
004900     SELECT W52225                     ASSIGN TO W52221D4.                
005000     EJECT                                                                
005100                                                                          
005200 DATA DIVISION.                                                           
005300                                                                          
005400 FILE SECTION.                                                            
005500 FD  INDATA                                                               
005600     LABEL RECORD STANDARD                                                
005700     RECORDING  F                                                         
005800     BLOCK CONTAINS 0.                                                    
005900 01  INPOST                      PIC X(80).                               
006000                                                                          
006100 FD  W52202                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400 01  IN-POST.                                                             
006500*    03  -COPY W522VAT  -L.                                               
006600                                                                          
006700 FD  W52221                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000 01  W52221-POST.                                                         
007100*    03  -COPY W52221    -L.                                              
007200     EJECT                                                                
007300                                                                          
007400 FD  W52222                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700 01  W52222-POST.                                                         
007800*    03  -COPY W52221    -L.                                              
007900     EJECT                                                                
008000                                                                          
008100 FD  W52225                                                               
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400 01  W52225-POST.                                                         
008500*    03  -COPY W52221    -L.                                              
008600     EJECT                                                                
008700                                                                          
008800 WORKING-STORAGE SECTION.                                                 
008900 77  IDPGM                       PIC X(8)    VALUE 'W5222100'.            
009000 77  YES                         PIC X       VALUE 'J'.                   
009100 77  NOO                         PIC X       VALUE 'N'.                   
009200     EJECT                                                                
009300                                                                          
009400 01  WS-ATAB-COUNTRIES.                                                   
009500     03  WS-ENGLAND              PIC X(50)   VALUE                        
009600                                 'CARPARTS.VIDB.VATGB'.                   
009700     03  WS-ITALY                PIC X(50)   VALUE                        
009800                                 'CARPARTS.VIDB.VATIT'.                   
009900     03  WS-SPAIN                PIC X(50)   VALUE                        
010000                                 'CARPARTS.VIDB.VATES'.                   
010100     03  WS-NETHERLANDS          PIC X(50)   VALUE                        
010200                                 'CARPARTS.VIDB.VATNL'.                   
010300     03  WS-GERMANY              PIC X(50)   VALUE                        
010400                                 'CARPARTS.VIDB.VATDE'.                   
010500     03  WS-PORTUGAL             PIC X(50)   VALUE                        
010600                                 'CARPARTS.VIDB.VATPT'.                   
010700     03  WS-AUSTRIA              PIC X(50)   VALUE                        
010800                                 'CARPARTS.VIDB.VATAT'.                   
010900     EJECT                                                                
011000 01  IDPTYP.                                                              
011100     03  WS-IDPTYP               PIC X(3)    VALUE SPACE.                 
011200     03  WS-IDPTYP-HEAD          PIC X(3)    VALUE SPACE.                 
011300                                                                          
011400 01  WS-PRKURS-SEND              PIC S9(6)V9(5) VALUE ZERO COMP-3.        
011500                                                                          
011600 01  WS-IDLEVNR                  PIC X(5)    VALUE '00000'.               
011700 01  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
011800 01  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
011900                                                                          
012000 01  IDARTNR-UNSTRING.                                                    
012100     03  WS-IDARTNR-UNSTR        PIC X(9)    VALUE SPACE.                 
012200                                                                          
012300 01  CALCULATE-AREA.                                                      
012400     03  WS-SUNTO-TOT            PIC S9(10)V9(2) COMP-3.                  
012500     03  WS-SUVAT-BILLIT-TOT     PIC S9(10)V9(2) COMP-3.                  
012600     03  WS-SEK                  PIC S9(11)V9(5) COMP-3.                  
012700     03  WS-LOC                  PIC S9(11)V9(5) COMP-3.                  
012800                                                                          
012900 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
013000 77  W52202-EOF-SW               PIC X       VALUE 'N'.                   
013100     88  END-OF-W52202                       VALUE 'J'.                   
013200     EJECT                                                                
013300 77  IDLANDX3-BET-SW             PIC X       VALUE 'J'.                   
013400     88  IDLANDX3-BET-FIRST                  VALUE 'J'.                   
013500     EJECT                                                                
013600 01  ERRTEXT.                                                             
013700     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
013800     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
013900                                                                          
014000 01  KDRC-DISPLAY                PIC Z(5).                                
014100                                                                          
014200 01  FELTEXT                     PIC X(80).                               
014300                                                                          
014400 01  GENERAL-SUBPROGRAMS.                                                 
014500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
014900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015000     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
015100     EJECT                                                                
015200*    --- PARAMETERS TO ABEND                                              
015300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'IDLAND'.              
015800*01  -COPY WWLAND09                                                       
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
016100*01  -COPY WZ01SEND                                                       
016200     EJECT                                                                
016300*    --- INFIL                                                            
016400 01  IN-AREA-START               PIC X(24)   VALUE                        
016500                                             'IN-AREA-START'.             
016600 01  IN-AREA.                                                             
016700*    03  -COPY W522VAT     -PRE IN-                                       
016800     EJECT                                                                
016900                                                                          
017000 01  FILLER                      PIC X(24)    VALUE 'SYSIN '.             
017100 01  INAREA.                                                              
017200     03  PRM-IDLAND              PIC X(2).                                
017300     03  FILLER                  PIC X(78).                               
017400     EJECT                                                                
017500                                                                          
017600 01  W52221-AREA-START           PIC X(24)    VALUE                       
017700                                             'W52221-AREA-START'.         
017800 01  W52221-AREA.                                                         
017900*    03  -COPY W52221      -PRE W52221-                                   
018000     EJECT                                                                
018100                                                                          
018200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
018300     SKIP2                                                                
018400*01  -COPY WDATKORT                                                       
018500     EJECT                                                                
018600                                                                          
018700*01  -COPY W510CURR                                                       
018800     EJECT                                                                
018900                                                                          
019000 LINKAGE SECTION.                                                         
019100                                                                          
019200*01  -COPY W0009   -PRE MSG-                                              
019300                                                                          
019400*01  -COPY W0008  -PRE WDG2-                                              
019500     05  FILLER                  PIC X.                                   
019600                                                                          
019700 PROCEDURE DIVISION  USING MSG-PCB WDG2-PCB.                              
019800     ENTRY 'DLITCBL' USING MSG-PCB WDG2-PCB.                              
019900                                                                          
020000     PERFORM A-INIT                                                       
020100                                                                          
020200     PERFORM S01-SEND-OPEN                                                
020300     PERFORM S10-READ-W52202                                              
020400     PERFORM UNTIL END-OF-W52202                                          
020500       IF IN-IDLANDX3-SEND NOT = PRM-IDLAND AND                           
020600          IN-IDLANDX3-BET  NOT = PRM-IDLAND                               
020700         CONTINUE                                                         
020800       ELSE                                                               
020900         PERFORM B-EXECUTE                                                
021000       END-IF                                                             
021100       PERFORM S10-READ-W52202                                            
021200     END-PERFORM                                                          
021300     PERFORM S04-SEND-CLOSE                                               
021400                                                                          
021500     PERFORM Z-FINIT                                                      
021600     MOVE ZERO TO RETURN-CODE                                             
021700     GOBACK                                                               
021800     .                                                                    
021900     EJECT                                                                
022000                                                                          
022100 A-INIT SECTION.                                                          
022200                                                                          
022300     OPEN INPUT INDATA                                                    
022400     READ INDATA NEXT RECORD INTO INAREA                                  
022500       AT END MOVE 'J' TO SYSIN-EOF                                       
022600     END-READ                                                             
022700     CLOSE INDATA                                                         
022800     DISPLAY PRM-IDLAND                                                   
022900                                                                          
023000     OPEN INPUT  W52202                                                   
023100     OPEN OUTPUT W52221                                                   
023200                 W52222                                                   
023300                 W52225                                                   
023400     .                                                                    
023500     EJECT                                                                
023600                                                                          
023700 B-EXECUTE SECTION.                                                       
023800     IF IN-KDFINDOC = 'ECO'                                               
023900       IF IN-IDLANDX3-SEND > SPACE                                        
024000         PERFORM BA-CREATE-W52221                                         
024100         PERFORM S02-SEND-PUT                                             
024200         PERFORM S11-WRITE-W52221-22                                      
024300       ELSE                                                               
024400         IF IN-IDLANDX3-BET > SPACE                                       
024500           PERFORM BA-CREATE-W52221                                       
024600           PERFORM S02-SEND-PUT                                           
024700           PERFORM S11-WRITE-W52221-22                                    
024800         END-IF                                                           
024900       END-IF                                                             
025000     END-IF                                                               
025100                                                                          
025200     IF IN-KDFINDOC NOT = 'ECO'                                           
025300       PERFORM BA-CREATE-W52221                                           
025400       PERFORM S02-SEND-PUT                                               
025500       PERFORM S11-WRITE-W52221-22                                        
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900                                                                          
026000 BA-CREATE-W52221 SECTION.                                                
026100     MOVE IN-IDLANDX3-SEND     TO W52221-IDLANDX3-SEND                    
026200     MOVE IN-IDLANDX3-BET      TO W52221-IDLANDX3-BET                     
026300     MOVE '20'                 TO W52221-TIAAAA(1:2)                      
026400     MOVE IN-TIAA              TO W52221-TIAAAA(3:2)                      
026500     MOVE IN-TIRP              TO W52221-TIRP                             
026600     MOVE IN-IDVAT-BET         TO W52221-IDVAT-REC                        
026700     MOVE IN-IDVAT-RESP        TO W52221-IDVAT-SEND                       
026800     MOVE IN-IDDISTR           TO W52221-IDDISTR                          
026900     MOVE IN-IDKUNDNR          TO W52221-IDKUNDNR                         
027000     MOVE IN-IDFINDOC          TO W52221-IDFAKT                           
027100     MOVE IN-DAFINDOC          TO W52221-TIFAKT                           
027200                                                                          
027300     IF IN-KDFINDOC = 'CR' OR 'CR2'                                       
027400       COMPUTE WS-SUNTO-TOT = IN-SUNTO-TOT *                              
027500                              -1                                          
027600       END-COMPUTE                                                        
027700       COMPUTE WS-SUVAT-BILLIT-TOT = IN-SUVAT-BILLIT-TOT *                
027800                                     -1                                   
027900       END-COMPUTE                                                        
028000       MOVE WS-SUNTO-TOT         TO W52221-SUFKTTOT-LOC                   
028100       MOVE WS-SUVAT-BILLIT-TOT  TO W52221-SUVAT-FAKT-LOC                 
028200     ELSE                                                                 
028300       MOVE IN-SUNTO-TOT         TO W52221-SUFKTTOT-LOC                   
028400       MOVE IN-SUVAT-BILLIT-TOT  TO W52221-SUVAT-FAKT-LOC                 
028500     END-IF                                                               
028600                                                                          
028700     CALL DATKORT USING IDPGM  DATUMKORT-ID DATUMKORT                     
028800     MOVE D-AAR                       TO W-DATE-AAMM(1:2)                 
028900     MOVE D-MAANAD                    TO W-DATE-AAMM(3:2)                 
029000     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
029100     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
029200     MOVE 'M'                         TO CURR-KDVALTYP                    
029300                                                                          
029400     IF PRM-IDLAND = 'IE' OR 'NL' OR 'FR' OR 'BE' OR 'DE' OR              
029500                     'IT' OR 'GR' OR 'ES' OR 'FI' OR 'PT' OR              
029600                     'AT' OR 'CY'                                         
029700       MOVE 'EUR'        TO CURR-KDVALISO-ROW                             
029800       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
029900     ELSE                                                                 
030000      IF PRM-IDLAND = 'SE'                                                
030100        MOVE 'SEK'      TO CURR-KDVALISO-ROW                              
030200        CALL W510CURR USING CURR-W510CURR WDG2-PCB                        
030300      ELSE                                                                
030400       IF PRM-IDLAND = 'GB'                                               
030500         MOVE 'GBP'      TO CURR-KDVALISO-ROW                             
030600         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
030700       ELSE                                                               
030800         IF PRM-IDLAND = 'DK'                                             
030900           MOVE 'DKK'      TO CURR-KDVALISO-ROW                           
031000           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
031100         ELSE                                                             
031200           IF PRM-IDLAND = 'NO'                                           
031300             MOVE 'NOK'      TO CURR-KDVALISO-ROW                         
031400             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
031500           ELSE                                                           
031600             IF PRM-IDLAND = 'PL'                                         
031700               MOVE 'PLN'      TO CURR-KDVALISO-ROW                       
031800               CALL W510CURR USING CURR-W510CURR WDG2-PCB                 
031900             ELSE                                                         
032000               IF PRM-IDLAND = 'CZ'                                       
032100                 MOVE 'CZK'      TO CURR-KDVALISO-ROW                     
032200                 CALL W510CURR USING CURR-W510CURR WDG2-PCB               
032300               ELSE                                                       
032400                 IF PRM-IDLAND = 'HU'                                     
032500                   MOVE 'HUF'      TO CURR-KDVALISO-ROW                   
032600                   CALL W510CURR USING CURR-W510CURR WDG2-PCB             
032700                 ELSE                                                     
032800                  STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY           
032900                  DELIMITED BY SIZE INTO ERRTEXT-STR                      
033000                  CALL ABEND USING RKOD-ABEND-WITH-DUMP                   
033100                 END-IF                                                   
033200               END-IF                                                     
033300             END-IF                                                       
033400           END-IF                                                         
033500         END-IF                                                           
033600       END-IF                                                             
033700      END-IF                                                              
033800     END-IF                                                               
033900                                                                          
034000     IF IN-KDVALISO = CURR-KDVALISO-ROW                                   
034100*    --- INGEN OMRÄKNING (DEALER-NET/DDI)                                 
034200       CONTINUE                                                           
034300     ELSE                                                                 
034400       MOVE W52221-SUFKTTOT-LOC   TO WS-SUNTO-TOT                         
034500       MOVE W52221-SUVAT-FAKT-LOC TO WS-SUVAT-BILLIT-TOT                  
034600       IF IN-KDVALISO NOT = 'SEK'                                         
034700*    --- BERÄKNA BELOPP LOKAL VALUTA -> SEK                               
034800         COMPUTE WS-SEK ROUNDED = WS-SUNTO-TOT *                          
034900                                  IN-PRKURS                               
035000*    --- BERÄKNA BELOPP SEK -> LOKAL VALUTA                               
035100         COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)              
035200         MOVE WS-LOC           TO W52221-SUFKTTOT-LOC                     
035300                                                                          
035400*    --- BERÄKNA BELOPP LOKAL VALUTA -> SEK                               
035500         COMPUTE WS-SEK ROUNDED = WS-SUVAT-BILLIT-TOT *                   
035600                                  IN-PRKURS                               
035700*    --- BERÄKNA BELOPP SEK -> LOKAL VALUTA                               
035800         COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)              
035900         MOVE WS-LOC           TO W52221-SUVAT-FAKT-LOC                   
036000       ELSE                                                               
036100*    --- BERÄKNA BELOPP I LOKAL VALUTA                                    
036200         COMPUTE WS-LOC ROUNDED = WS-SUNTO-TOT /                          
036300                                  CURR-PRKURS-NEW                         
036400         MOVE WS-LOC           TO W52221-SUFKTTOT-LOC                     
036500                                                                          
036600*    --- BERÄKNA BELOPP I LOKAL VALUTA                                    
036700         COMPUTE WS-LOC ROUNDED = WS-SUVAT-BILLIT-TOT /                   
036800                                  CURR-PRKURS-NEW                         
036900         MOVE WS-LOC           TO W52221-SUVAT-FAKT-LOC                   
037000       END-IF                                                             
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500 Z-FINIT SECTION.                                                         
037600     CLOSE W52202                                                         
037700           W52221                                                         
037800           W52222                                                         
037900           W52225                                                         
038000     .                                                                    
038100     EJECT                                                                
038200                                                                          
038300 S10-READ-W52202  SECTION.                                                
038400     READ W52202 INTO IN-AREA                                             
038500     AT END                                                               
038600        MOVE YES TO W52202-EOF-SW                                         
038700     END-READ                                                             
038800     .                                                                    
038900                                                                          
039000 S11-WRITE-W52221-22 SECTION.                                             
039100     WRITE W52221-POST FROM W52221-AREA                                   
039200     WRITE W52222-POST FROM W52221-AREA                                   
039300     WRITE W52225-POST FROM W52221-AREA                                   
039400     .                                                                    
039500     EJECT                                                                
039600                                                                          
039700*    --- DISPATCHER SECTIONS                                              
039800 S01-SEND-OPEN SECTION.                                                   
039900     IF PRM-IDLAND = 'GB' OR 'IT' OR 'ES' OR 'NL' OR 'DE' OR              
040000                     'PT' OR 'AT'                                         
040100       MOVE 'OPEN'                    TO SEND-KDFUNC                      
040200       IF PRM-IDLAND = 'GB'                                               
040300         MOVE WS-ENGLAND              TO SEND-ADDISPABS                   
040400       END-IF                                                             
040500       IF PRM-IDLAND = 'IT'                                               
040600         MOVE WS-ITALY                TO SEND-ADDISPABS                   
040700       END-IF                                                             
040800       IF PRM-IDLAND = 'ES'                                               
040900         MOVE WS-SPAIN                TO SEND-ADDISPABS                   
041000       END-IF                                                             
041100       IF PRM-IDLAND = 'NL'                                               
041200         MOVE WS-NETHERLANDS          TO SEND-ADDISPABS                   
041300       END-IF                                                             
041400       IF PRM-IDLAND = 'DE'                                               
041500         MOVE WS-GERMANY              TO SEND-ADDISPABS                   
041600       END-IF                                                             
041700       IF PRM-IDLAND = 'PT'                                               
041800         MOVE WS-PORTUGAL             TO SEND-ADDISPABS                   
041900       END-IF                                                             
042000       IF PRM-IDLAND = 'AT'                                               
042100         MOVE WS-AUSTRIA              TO SEND-ADDISPABS                   
042200       END-IF                                                             
042300                                                                          
042400       CALL WZ01SEND USING  SEND-CONTROL-AREA                             
042500                            SEND-OPEN-AREA                                
042600       IF SEND-KDRC > 0                                                   
042700         MOVE SEND-KDRC               TO KDRC-DISPLAY                     
042800         STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                    
042900         DELIMITED BY SIZE INTO ERRTEXT-STR                               
043000         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
043100       END-IF                                                             
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500                                                                          
043600 S02-SEND-PUT SECTION.                                                    
043700     IF PRM-IDLAND = 'GB' OR 'IT' OR 'ES' OR 'NL' OR 'DE' OR              
043800                     'PT' OR 'AT'                                         
043900       MOVE 'PUT'                 TO SEND-KDFUNC                          
044000       MOVE LENGTH OF W52221-AREA TO SEND-KVDLEN                          
044100       CALL WZ01SEND USING SEND-CONTROL-AREA                              
044200                           SEND-KVDLEN                                    
044300                           W52221-AREA                                    
044400       IF SEND-KDRC > 0                                                   
044500         MOVE SEND-KDRC           TO KDRC-DISPLAY                         
044600         STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                     
044700         DELIMITED BY SIZE INTO ERRTEXT-STR                               
044800         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
044900       END-IF                                                             
045000     END-IF                                                               
045100     .                                                                    
045200                                                                          
045300 S04-SEND-CLOSE SECTION.                                                  
045400     IF PRM-IDLAND = 'GB' OR 'IT' OR 'ES' OR 'NL' OR 'DE' OR              
045500                     'PT' OR 'AT'                                         
045600       MOVE 'CLOSE'              TO SEND-KDFUNC                           
045700       CALL WZ01SEND USING SEND-CONTROL-AREA                              
045800     END-IF                                                               
045900     .                                                                    
046000                                                                          
