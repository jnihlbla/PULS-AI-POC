000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5221100.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   02/06/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*                                                                         
001000*        THE PROGRAM                                                      
001100*        - READS PRM-DATA FROM SYSIN                                      
001200*        - READS FILE W52201 AND CREATES AN INTRASTAT-FILE                
001300*        - SENDS INTRASTAT DATA TO THE FOLLOWING COUNTRIES,               
001400*          . ENGLAND     (SYMBOLIC PRM = GB)  VCOM/ONDEMAND               
001500*          . IRELAND     (SYMBOLIC PRM = IE)  VCOM/ONDEMAND               
001600*          . FRANCE      (SYMBOLIC PRM = FR)  VCOM/ONDEMAND               
001700*          . NETHERLANDS (SYMBOLIC PRM = NL)  VCOM/ONDEMAND               
001800*          . BELGIUM     (SYMBOLIC PRM = BE)  ONDEMAND                    
001900*          . GERMANY     (SYMBOLIC PRM = DE)  VCOM/ONDEMAND               
002000*          . ITALY       (SYMBOLIC PRM = IT)  VCOM/ONDEMAND               
002100*          . GREECE      (SYMBOLIC PRM = GR)  VCOM/ONDEMAND               
002200*          . SPAIN       (SYMBOLIC PRM = ES)  VCOM/ONDEMAND               
002300*          . DENMARK2    (SYMBOLIC PRM = DK)  VCOM/ONDEMAND               
002400*          . FINLAND     (SYMBOLIC PRM = FI)  VCOM/ONDEMAND               
002500*          . PORTUGAL    (SYMBOLIC PRM = PT)  VCOM/ONDEMAND               
002600*          . ÖSTERRIKE   (SYMBOLIC PRM = AT)  VCOM/ONDEMAND               
002700*          . CYPERN      (SYMBOLIC PRM = CY)                              
002800*          . POLAND      (SYMBOLIC PRM = PL)  ONDEMAND                    
002900*          . CZECH       (SYMBOLIC PRM = CZ)  ONDEMAND                    
003000*          . HUNGARY     (SYMBOLIC PRM = HU)  ONDEMAND                    
003100*                                                                         
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
003700                                                                          
003800*          --- SYSIN FROM JCL                                             
003900     SELECT INDATA                     ASSIGN TO SYSIN.                   
004000                                                                          
004100*          --- SELECTED INTRASTAT-DATA FROM IVW-TABLE -INPUT              
004200     SELECT W52201                     ASSIGN TO W52211D1.                
004300                                                                          
004400*          --- SELECTED INTRASTAT-DATA TO ON-DEMAND                       
004500     SELECT W52213                     ASSIGN TO W52211D2.                
004600                                                                          
004700*          --- SELECTED INTRASTAT-DATA TO FILE                            
004800     SELECT W52215                     ASSIGN TO W52211D3.                
004900                                                                          
005000*          --- SELECTED INTRASTAT-DATA TO D&P                             
005100     SELECT W52216                     ASSIGN TO W52211D4.                
005200                                                                          
005300 DATA DIVISION.                                                           
005400                                                                          
005500 FILE SECTION.                                                            
005600 FD  INDATA                                                               
005700     LABEL RECORD STANDARD                                                
005800     RECORDING  F                                                         
005900     BLOCK CONTAINS 0.                                                    
006000 01  INPOST                      PIC X(80).                               
006100                                                                          
006200 FD  W52201                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500 01  IN-POST.                                                             
006600*    03  -COPY W522INT   -L.                                              
006700                                                                          
006800 FD  W52213                                                               
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS  0.                                                   
007100 01  W52213-POST.                                                         
007200*    03  -COPY W52211    -L.                                              
007300     EJECT                                                                
007400                                                                          
007500 FD  W52215                                                               
007600     RECORDING       F                                                    
007700     BLOCK CONTAINS  0.                                                   
007800 01  W52215-POST.                                                         
007900*    03  -COPY W52211    -L.                                              
008000     EJECT                                                                
008100                                                                          
008200 FD  W52216                                                               
008300     RECORDING       V                                                    
008400     BLOCK CONTAINS  0.                                                   
008500 01  W52216-POST.                                                         
008600*    03  -COPY W52211    -L.                                              
008700     EJECT                                                                
008800                                                                          
008900 WORKING-STORAGE SECTION.                                                 
009000 77  IDPGM                       PIC X(8)    VALUE 'W5221100'.            
009100 77  YES                         PIC X       VALUE 'J'.                   
009200 77  NOO                         PIC X       VALUE 'N'.                   
009300 77  WS-IDLEVNR                  PIC X(5)    VALUE '00000'.               
009400 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
009500 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
009600     EJECT                                                                
009700                                                                          
009800 01  WS-ATAB-COUNTRIES.                                                   
009900     03  WS-IRELAND              PIC X(50)   VALUE                        
010000                                 'CARPARTS.VIDB.INTRAIE'.                 
010100     03  WS-ENGLAND              PIC X(50)   VALUE                        
010200                                 'CARPARTS.VIDB.INTRAGB'.                 
010300     03  WS-FRANCE               PIC X(50)   VALUE                        
010400                                 'CARPARTS.VIDB.INTRAFR'.                 
010500     03  WS-ITALY                PIC X(50)   VALUE                        
010600                                 'CARPARTS.VIDB.INTRAIT'.                 
010700     03  WS-GREECE               PIC X(50)   VALUE                        
010800                                 'CARPARTS.VIDB.INTRAGR'.                 
010900     03  WS-SPAIN                PIC X(50)   VALUE                        
011000                                 'CARPARTS.VIDB.INTRAES'.                 
011100     03  WS-DENMARK-2            PIC X(50)   VALUE                        
011200                                 'CARPARTS.VLC.INTRADK2'.                 
011300     03  WS-FINLAND              PIC X(50)   VALUE                        
011400                                 'CARPARTS.VIDB.INTRAFI'.                 
011500     03  WS-NETHERLANDS          PIC X(50)   VALUE                        
011600                                 'CARPARTS.VIDB.INTRANL'.                 
011700     03  WS-GERMANY              PIC X(50)   VALUE                        
011800                                 'CARPARTS.VIDB.INTRADE'.                 
011900     03  WS-PORTUGAL             PIC X(50)   VALUE                        
012000                                 'CARPARTS.VIDB.INTRAPT'.                 
012100     03  WS-AUSTRIA              PIC X(50)   VALUE                        
012200                                 'CARPARTS.VIDB.INTRAAT'.                 
012300     EJECT                                                                
012400                                                                          
012500 01  IDARTNR-UNSTRING.                                                    
012600     03  WS-IDARTNR-UNSTR        PIC X(9)    VALUE SPACE.                 
012700                                                                          
012800 01  CALCULATE-AREA.                                                      
012900     03 WS-SEK                   PIC 9(11)V9(2) VALUE ZERO.               
013000     03 WS-LOC                   PIC 9(11)V9(2) VALUE ZERO.               
013100     03 WS-SUNTO                 PIC S9(11)V9(2) VALUE ZERO.              
013200     SKIP2                                                                
013300 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
013400 77  W52201-EOF-SW               PIC X       VALUE 'N'.                   
013500     88  END-OF-W52201                       VALUE 'J'.                   
013600     EJECT                                                                
013700 77  IDLANDX3-REC-SW             PIC X       VALUE 'J'.                   
013800     88  IDLANDX3-REC-FIRST                  VALUE 'J'.                   
013900     EJECT                                                                
014000 01  ERRTEXT.                                                             
014100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
014200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
014300 01  KDRC-DISPLAY                PIC Z(5).                                
014400                                                                          
014500 01  FELTEXT                     PIC X(80).                               
014600                                                                          
014700 01  GENERAL-SUBPROGRAMS.                                                 
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015300     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL ABEND                                            
015600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'IDLAND'.              
016100*01  -COPY WWLAND09                                                       
016200     EJECT                                                                
016210*01  -COPY WWLANDX2                                                       
016220     EJECT                                                                
016230*01  -COPY WWDIST35                                                       
016240     EJECT                                                                
016300 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
016400*01  -COPY WZ01SEND                                                       
016500     EJECT                                                                
016600                                                                          
016700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
016800     SKIP2                                                                
016900*01  -COPY WDATKORT                                                       
017000     EJECT                                                                
017100                                                                          
017200*01  -COPY W510CURR                                                       
017300     EJECT                                                                
017400*    --- INFIL                                                            
017500 01  IN-AREA-START                  PIC X(24)   VALUE                     
017600                                                 'IN-AREA-START'.         
017700 01  IN-AREA.                                                             
017800*    03  -COPY W522INT     -PRE IN-                                       
017900     EJECT                                                                
018000                                                                          
018100 01  FILLER                         PIC X(24)   VALUE 'SYSIN '.           
018200 01  INAREA.                                                              
018300     03  PRM-IDLAND                 PIC X(2).                             
018400     03  PRM-VERSION                PIC X(1).                             
018500     03  FILLER                     PIC X(77).                            
018600     EJECT                                                                
018700                                                                          
018800 01  W52211-AREA-START              PIC X(24)   VALUE                     
018900                                          'W52211-AREA-START'.            
019000 01  W52211-AREA.                                                         
019100*    03  -COPY W52211      -PRE W52211-                                   
019200     EJECT                                                                
019300                                                                          
019400 LINKAGE SECTION.                                                         
019500                                                                          
019600*01  -COPY W0009  -PRE MSG-                                               
019700                                                                          
019800*01  -COPY W0008  -PRE WDG2-                                              
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100                                                                          
020200 PROCEDURE DIVISION  USING MSG-PCB WDG2-PCB.                              
020300     ENTRY 'DLITCBL' USING MSG-PCB WDG2-PCB.                              
020400                                                                          
020500     PERFORM A-INIT                                                       
020600                                                                          
020700     PERFORM S01-SEND-OPEN                                                
020800     PERFORM S10-READ-W52201                                              
020900                                                                          
021000     PERFORM UNTIL END-OF-W52201                                          
021100       IF IN-KDFINDOC = 'CR'                                              
021200*** INTRASTAT ONLY TO SEND AND REC COUNTRIES                              
021300         IF IN-IDLANDX3-REC > SPACE                                       
021400           IF IN-IDLANDX3-REC  NOT = PRM-IDLAND AND                       
021500              IN-IDLANDX3-BET  NOT = PRM-IDLAND                           
021600              CONTINUE                                                    
021700           ELSE                                                           
021800              PERFORM B-EXECUTE                                           
021900           END-IF                                                         
022000         ELSE                                                             
022100           IF IN-IDLANDX3-SEND NOT = PRM-IDLAND AND                       
022200              IN-IDLANDX3-BET  NOT = PRM-IDLAND                           
022300              CONTINUE                                                    
022400           ELSE                                                           
022500              PERFORM B-EXECUTE                                           
022600           END-IF                                                         
022700         END-IF                                                           
022800       ELSE                                                               
022900         IF IN-IDLANDX3-SEND NOT = PRM-IDLAND AND                         
023000            IN-IDLANDX3-REC  NOT = PRM-IDLAND AND                         
023100            IN-IDLANDX3-BET  NOT = PRM-IDLAND                             
023200           CONTINUE                                                       
023300         ELSE                                                             
023400           PERFORM B-EXECUTE                                              
023500         END-IF                                                           
023600       END-IF                                                             
023700       PERFORM S10-READ-W52201                                            
023800     END-PERFORM                                                          
023900                                                                          
024000     PERFORM S04-SEND-CLOSE                                               
024100     PERFORM Z-FINIT                                                      
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700                                                                          
024800 A-INIT SECTION.                                                          
024900     OPEN INPUT INDATA                                                    
025000     READ INDATA NEXT RECORD INTO INAREA                                  
025100       AT END MOVE 'J' TO SYSIN-EOF                                       
025200     END-READ                                                             
025300     CLOSE INDATA                                                         
025400     DISPLAY PRM-IDLAND                                                   
025500                                                                          
025600     CALL DATKORT USING IDPGM  DATUMKORT-ID DATUMKORT                     
025700     MOVE D-AAR                       TO W-DATE-AAMM(1:2)                 
025800     MOVE D-MAANAD                    TO W-DATE-AAMM(3:2)                 
025900     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
026000     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
026100     MOVE 'M'                         TO CURR-KDVALTYP                    
026200                                                                          
026300     IF PRM-IDLAND = 'IE' OR 'NL' OR 'FR' OR 'BE' OR 'DE' OR              
026400                     'IT' OR 'GR' OR 'ES' OR 'FI' OR 'PT' OR              
026500                     'AT' OR 'CY'                                         
026600       MOVE 'EUR'        TO CURR-KDVALISO-ROW                             
026700       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
026800     ELSE                                                                 
026900      IF PRM-IDLAND = 'SE'                                                
027000        MOVE 'SEK'      TO CURR-KDVALISO-ROW                              
027100        CALL W510CURR USING CURR-W510CURR WDG2-PCB                        
027200       ELSE                                                               
027300       IF PRM-IDLAND = 'GB'                                               
027400         MOVE 'GBP'      TO CURR-KDVALISO-ROW                             
027500         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
027600       ELSE                                                               
027700         IF PRM-IDLAND = 'DK'                                             
027800           MOVE 'DKK'      TO CURR-KDVALISO-ROW                           
027900           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
028000         ELSE                                                             
028100           IF PRM-IDLAND = 'PL'                                           
028200             MOVE 'PLN'      TO CURR-KDVALISO-ROW                         
028300             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
028400           ELSE                                                           
028500             IF PRM-IDLAND = 'CZ'                                         
028600               MOVE 'CZK'      TO CURR-KDVALISO-ROW                       
028700               CALL W510CURR USING CURR-W510CURR WDG2-PCB                 
028800             ELSE                                                         
028900               IF PRM-IDLAND = 'HU'                                       
029000                 MOVE 'HUF'      TO CURR-KDVALISO-ROW                     
029100                 CALL W510CURR USING CURR-W510CURR WDG2-PCB               
029200               ELSE                                                       
029300                 STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY            
029400                 DELIMITED BY SIZE INTO ERRTEXT-STR                       
029500                 CALL ABEND USING RKOD-ABEND-WITH-DUMP                    
029600               END-IF                                                     
029700             END-IF                                                       
029800           END-IF                                                         
029900         END-IF                                                           
030000       END-IF                                                             
030100      END-IF                                                              
030200     END-IF                                                               
030300                                                                          
030400     OPEN INPUT  W52201                                                   
030500     OPEN OUTPUT W52213                                                   
030600                 W52215                                                   
030700                 W52216                                                   
030800     .                                                                    
030900     EJECT                                                                
031000                                                                          
031100 B-EXECUTE SECTION.                                                       
031200     IF IN-KDFINDOC = 'ECO'                                               
031300       IF IN-IDLANDX3-SEND > SPACE                                        
031310**** CHECK IF IT'S A REFILL FROM SWEDEN TO EU COUNTRY                     
031320**** AND MOVE VALUES TO KDINTTYP                                          
031330         MOVE IN-IDDISTR   TO DIST35-IDDISTR                              
031340         IF DIST35-REFILL                                                 
031350           IF IN-IDLANDX3-REC > SPACE                                     
031360             MOVE IN-IDLANDX3-REC TO LANDX2-IDLANDX2                      
031370           ELSE                                                           
031380             MOVE IN-IDLANDX3-BET TO LANDX2-IDLANDX2                      
031390           END-IF                                                         
031391           IF LANDX2-EU-IDLANDX2                                          
031392             MOVE +31      TO W52211-KDINTTYP                             
031393             MOVE +3       TO W52211-KDINTTYP-OLD                         
031394           ELSE                                                           
031395             MOVE +99      TO W52211-KDINTTYP                             
031396             MOVE +9       TO W52211-KDINTTYP-OLD                         
031397           END-IF                                                         
031398         ELSE                                                             
031399           MOVE +99        TO W52211-KDINTTYP                             
031400           MOVE +9         TO W52211-KDINTTYP-OLD                         
031401         END-IF                                                           
031402****                                                                      
031600         PERFORM BA-CREATE-W52211                                         
031700         PERFORM S02-SEND-PUT                                             
031800         PERFORM S11-WRITE-W52213-15                                      
031900       ELSE                                                               
032000         IF IN-IDLANDX3-REC > SPACE                                       
032010**** CHECK IF IT'S A REFILL FROM SWEDEN TO EU COUNTRY                     
032020**** AND MOVE VALUES TO KDINTTYP                                          
032030           MOVE IN-IDDISTR TO DIST35-IDDISTR                              
032040           IF DIST35-REFILL                                               
032050             IF IN-IDLANDX3-REC > SPACE                                   
032060               MOVE IN-IDLANDX3-REC TO LANDX2-IDLANDX2                    
032070             ELSE                                                         
032080               MOVE IN-IDLANDX3-BET TO LANDX2-IDLANDX2                    
032090             END-IF                                                       
032091             IF LANDX2-EU-IDLANDX2                                        
032092               MOVE +31    TO W52211-KDINTTYP                             
032093               MOVE +3     TO W52211-KDINTTYP-OLD                         
032094             ELSE                                                         
032095               MOVE +99    TO W52211-KDINTTYP                             
032096               MOVE +9     TO W52211-KDINTTYP-OLD                         
032097             END-IF                                                       
032098           ELSE                                                           
032099             MOVE +99      TO W52211-KDINTTYP                             
032100             MOVE +9       TO W52211-KDINTTYP-OLD                         
032101           END-IF                                                         
032102****                                                                      
032300           PERFORM BA-CREATE-W52211                                       
032400           PERFORM S02-SEND-PUT                                           
032500           PERFORM S11-WRITE-W52213-15                                    
032600         END-IF                                                           
032700       END-IF                                                             
032800     END-IF                                                               
032900                                                                          
033000     IF IN-KDFINDOC = 'EXC'                                               
033100       IF IN-IDLANDX3-SEND > SPACE                                        
033200         MOVE +41        TO W52211-KDINTTYP                               
033300         MOVE +4         TO W52211-KDINTTYP-OLD                           
033400         PERFORM BA-CREATE-W52211                                         
033500         PERFORM S02-SEND-PUT                                             
033600         PERFORM S11-WRITE-W52213-15                                      
033700       END-IF                                                             
033800                                                                          
033900       IF IN-IDLANDX3-REC > SPACE                                         
034000         MOVE +41        TO W52211-KDINTTYP                               
034100         MOVE +4         TO W52211-KDINTTYP-OLD                           
034200         PERFORM BA-CREATE-W52211                                         
034300         PERFORM S02-SEND-PUT                                             
034400         PERFORM S11-WRITE-W52213-15                                      
034500       END-IF                                                             
034600     END-IF                                                               
034700                                                                          
034800     IF IN-KDFINDOC = 'INL'                                               
034900       MOVE +11          TO W52211-KDINTTYP                               
035000       MOVE +1           TO W52211-KDINTTYP-OLD                           
035100       PERFORM BA-CREATE-W52211                                           
035200       PERFORM S02-SEND-PUT                                               
035300       PERFORM S11-WRITE-W52213-15                                        
035400     END-IF                                                               
035500                                                                          
035600     IF IN-KDFINDOC NOT = 'ECO' AND 'EXC'                                 
035700       IF IN-IDLANDX3-SEND > SPACE                                        
035800         IF IN-KDFINDOC = 'INV'                                           
035900           MOVE +11       TO W52211-KDINTTYP                              
036000           MOVE +1        TO W52211-KDINTTYP-OLD                          
036100           PERFORM BA-CREATE-W52211                                       
036200           PERFORM S02-SEND-PUT                                           
036300           PERFORM S11-WRITE-W52213-15                                    
036400         ELSE                                                             
036500           IF IN-KDFINDOC = 'CR'                                          
036600             MOVE +21  TO W52211-KDINTTYP                                 
036700             MOVE +2   TO W52211-KDINTTYP-OLD                             
036800             PERFORM BA-CREATE-W52211                                     
036900             PERFORM S02-SEND-PUT                                         
037000             PERFORM S11-WRITE-W52213-15                                  
037100           ELSE                                                           
037101**** CHECK IF IT'S A REFILL FROM SWEDEN TO EU COUNTRY                     
037102**** AND MOVE VALUES TO KDINTTYP                                          
037110             MOVE IN-IDDISTR   TO DIST35-IDDISTR                          
037120             IF DIST35-REFILL                                             
037130               IF IN-IDLANDX3-REC > SPACE                                 
037131                 MOVE IN-IDLANDX3-REC TO LANDX2-IDLANDX2                  
037132               ELSE                                                       
037133                 MOVE IN-IDLANDX3-BET TO LANDX2-IDLANDX2                  
037134               END-IF                                                     
037135               IF LANDX2-EU-IDLANDX2                                      
037136                 MOVE +31      TO W52211-KDINTTYP                         
037137                 MOVE +3       TO W52211-KDINTTYP-OLD                     
037138               ELSE                                                       
037139                 MOVE +99      TO W52211-KDINTTYP                         
037140                 MOVE +9       TO W52211-KDINTTYP-OLD                     
037141               END-IF                                                     
037150             ELSE                                                         
037200               MOVE +99        TO W52211-KDINTTYP                         
037300               MOVE +9         TO W52211-KDINTTYP-OLD                     
037310             END-IF                                                       
037320****                                                                      
037400             PERFORM BA-CREATE-W52211                                     
037500             PERFORM S02-SEND-PUT                                         
037600             PERFORM S11-WRITE-W52213-15                                  
037700           END-IF                                                         
037800         END-IF                                                           
037900       END-IF                                                             
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300                                                                          
038400 BA-CREATE-W52211 SECTION.                                                
038500     MOVE '20'                   TO W52211-TIAAAA(1:2)                    
038600     MOVE IN-TIAA                TO W52211-TIAAAA(3:2)                    
038700     MOVE IN-TIRP                TO W52211-TIMM                           
038800                                                                          
038900     IF IN-KDFINDOC = 'ECO' OR 'INL' OR 'EXC'                             
039000       MOVE IN-IDLANDX3-SEND     TO W52211-IDLANDX3-SEND                  
039100       MOVE IN-IDLANDX3-REC      TO W52211-IDLANDX3-REC                   
039200       MOVE IN-IDVAT-BET         TO W52211-IDVAT                          
039300     ELSE                                                                 
039400       IF IN-KDFINDOC = 'CR'                                              
039500*** FOR CREDIT - WHEN SENDING,RECEIVING AND PAYING COUNTRIES ARE          
039600*** PRESENT                                                               
039700         IF IN-IDLANDX3-REC > SPACE                                       
039800           MOVE IN-IDLANDX3-BET    TO W52211-IDLANDX3-SEND                
039900           MOVE IN-IDLANDX3-REC    TO W52211-IDLANDX3-REC                 
040000         ELSE                                                             
040100           MOVE IN-IDLANDX3-BET    TO W52211-IDLANDX3-SEND                
040200           MOVE IN-IDLANDX3-SEND   TO W52211-IDLANDX3-REC                 
040300         END-IF                                                           
040400         MOVE IN-IDVAT-RESP        TO W52211-IDVAT                        
040500       ELSE                                                               
040600         IF IN-IDLANDX3-REC > SPACE                                       
040700           MOVE IN-IDLANDX3-REC  TO W52211-IDLANDX3-SEND                  
040800           MOVE IN-IDLANDX3-BET  TO W52211-IDLANDX3-REC                   
040900         ELSE                                                             
041000           MOVE IN-IDLANDX3-SEND TO W52211-IDLANDX3-SEND                  
041100           MOVE IN-IDLANDX3-BET  TO W52211-IDLANDX3-REC                   
041200         END-IF                                                           
041300         MOVE IN-IDVAT-BET       TO W52211-IDVAT                          
041400       END-IF                                                             
041500     END-IF                                                               
041600                                                                          
041700                                                                          
041800     IF IN-KDFINDOC = 'ECO' OR 'EXC' OR 'INL'                             
041900       IF PRM-IDLAND = 'GB' OR 'IT' OR 'ES' OR 'PT'                       
042000         MOVE 'SEK'              TO W52211-KDVALISO                       
042100       ELSE                                                               
042200         IF PRM-IDLAND = 'AT'                                             
042300           MOVE 'EUR'            TO W52211-KDVALISO                       
042400         END-IF                                                           
042500       END-IF                                                             
042600     ELSE                                                                 
042700       MOVE IN-KDVALISO          TO W52211-KDVALISO                       
042800     END-IF                                                               
042900     MOVE IN-IDVAT-BET           TO W52211-IDVAT-BET                      
043000     MOVE IN-IDVAT-RESP          TO W52211-IDVAT-RESP                     
043100     MOVE IN-IDVAT-LEG           TO W52211-IDVAT-LEG                      
043200     MOVE IN-IDVAT-AGENT         TO W52211-IDVAT-AGENT                    
043300     MOVE IN-DAFINDOC            TO W52211-DAFINDOC                       
043400     MOVE IN-IDFINDOC            TO W52211-IDFINDOC                       
043500     MOVE IN-IDSTATNR            TO W52211-IDSTATNR                       
043600     MOVE IN-IDARTNR             TO WS-IDARTNR-UNSTR                      
043700     INSPECT WS-IDARTNR-UNSTR REPLACING LEADING SPACE BY ZERO             
043800     MOVE WS-IDARTNR-UNSTR       TO W52211-IDARTNR                        
043900     MOVE IN-BEART               TO W52211-BEART                          
044000     MOVE IN-VKORDNTO            TO W52211-VKORDNTO                       
044100     MOVE IN-VKORDNTO-3DEC       TO W52211-VKORDNTO-3DEC                  
044200     MOVE IN-KVLEVART            TO W52211-KVLEVART                       
044300     MOVE IN-KDARTURS            TO W52211-KDARTURS                       
044400     MOVE IN-BELEVVIL            TO W52211-BELEVVIL                       
044500     MOVE IN-IDDISTR             TO W52211-IDDISTR                        
044600     MOVE IN-IDKUNDNR            TO W52211-IDKUNDNR                       
044700     IF IN-KDFINDOC = 'EXC' OR 'ECO' OR 'INL'                             
044800*    --- BERÄKNA BELOPP SEK -> LOKAL VALUTA                               
044900       COMPUTE WS-LOC ROUNDED = IN-SUNTO / CURR-PRKURS-NEW                
045000       MOVE WS-LOC               TO W52211-SUNTO                          
045100       MOVE CURR-PRKURS-NEW      TO W52211-PRKURS                         
045200       MOVE CURR-KDVALISO-ROW    TO W52211-KDVALISO                       
045300     ELSE                                                                 
045400       IF IN-KDVALISO = CURR-KDVALISO-ROW                                 
045500*    --- INGEN OMRÄKNING (DEALER-NET/DDI)                                 
045600         MOVE IN-SUNTO           TO W52211-SUNTO                          
045700         MOVE 1                  TO W52211-PRKURS                         
045800       ELSE                                                               
045900         IF IN-KDVALISO NOT = 'SEK'                                       
046000*    --- BERÄKNA BELOPP LOKAL VALUTA -> SEK                               
046100           COMPUTE WS-SEK ROUNDED = IN-SUNTO * IN-PRKURS                  
046200*    --- BERÄKNA BELOPP SEK -> LOKAL VALUTA                               
046300           COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)            
046400           MOVE WS-LOC           TO W52211-SUNTO                          
046500           MOVE CURR-PRKURS-NEW  TO W52211-PRKURS                         
046600           MOVE CURR-KDVALISO-ROW TO W52211-KDVALISO                      
046700         ELSE                                                             
046800*    --- BERÄKNA BELOPP I LOKAL VALUTA                                    
046900           COMPUTE WS-LOC ROUNDED = IN-SUNTO / CURR-PRKURS-NEW            
047000           MOVE WS-LOC           TO W52211-SUNTO                          
047100           MOVE CURR-PRKURS-NEW  TO W52211-PRKURS                         
047200         END-IF                                                           
047300       END-IF                                                             
047400     END-IF                                                               
047500                                                                          
047600     IF W52211-KDINTTYP-OLD = +2                                          
047700       MOVE W52211-SUNTO         TO WS-SUNTO                              
047800       COMPUTE WS-SUNTO = WS-SUNTO * -1                                   
047900       END-COMPUTE                                                        
048000       MOVE WS-SUNTO             TO W52211-SUNTO                          
048100     END-IF                                                               
048200                                                                          
048300     IF W52211-KDINTTYP-OLD = +4 OR +9                                    
048400       IF W52211-IDLANDX3-SEND = PRM-IDLAND                               
048500         MOVE W52211-SUNTO       TO WS-SUNTO                              
048600         COMPUTE WS-SUNTO = WS-SUNTO * -1                                 
048700         END-COMPUTE                                                      
048800         MOVE WS-SUNTO           TO W52211-SUNTO                          
048900       END-IF                                                             
049000     END-IF                                                               
049100                                                                          
049200     IF IN-KDFRAKT = 41 OR 43 OR 44                                       
049300* BY SEA                                                                  
049400       MOVE 1                    TO W52211-KDBEH                          
049500     ELSE                                                                 
049600       IF IN-KDFRAKT = 01 OR 03 OR 04 OR 08       OR                      
049700                       10 OR 11                   OR                      
049800                       20 OR 29                   OR                      
049900                       31 OR 32 OR 34 OR 35 OR 36 OR                      
050000                       40 OR 46                   OR                      
050100                       63 OR 64 OR 66 OR 67 OR 69 OR                      
050200                       70 OR 71                   OR                      
050300                       81                                                 
050400* BY ROAD                                                                 
050500         MOVE 3                  TO W52211-KDBEH                          
050600       ELSE                                                               
050700         IF IN-KDFRAKT = 13 OR 14 OR 17 OR 18 OR 19 OR                    
050800                         61                                               
050900* BY AIR                                                                  
051000           MOVE 4                TO W52211-KDBEH                          
051100         ELSE                                                             
051200* BY ROAD                                                                 
051300           MOVE 3                TO W52211-KDBEH                          
051400         END-IF                                                           
051500       END-IF                                                             
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000 Z-FINIT SECTION.                                                         
052100     CLOSE W52201                                                         
052200           W52213                                                         
052300           W52215                                                         
052400           W52216                                                         
052500     .                                                                    
052600     EJECT                                                                
052700*    --- READ AND WRITE SECTIONS                                          
052800 S10-READ-W52201 SECTION.                                                 
052900     READ W52201 INTO IN-AREA                                             
053000     AT END MOVE YES TO W52201-EOF-SW                                     
053100     END-READ                                                             
053200     .                                                                    
053300                                                                          
053400 S11-WRITE-W52213-15 SECTION.                                             
053500     WRITE W52213-POST   FROM W52211-AREA                                 
053600     WRITE W52215-POST   FROM W52211-AREA                                 
053700     WRITE W52216-POST   FROM W52211-AREA                                 
053800     .                                                                    
053900     EJECT                                                                
054000                                                                          
054100*    --- DISPATCHER SECTIONS                                              
054200 S01-SEND-OPEN SECTION.                                                   
054300     IF PRM-IDLAND = 'GB' OR 'IE' OR 'FR' OR 'IT' OR                      
054400                     'GR' OR 'ES' OR 'DK' OR 'FI' OR                      
054500                     'NL' OR 'DE' OR 'PT' OR 'AT'                         
054600       MOVE 'OPEN'                  TO SEND-KDFUNC                        
054700       IF PRM-IDLAND = 'GB'                                               
054800         MOVE WS-ENGLAND            TO SEND-ADDISPABS                     
054900       ELSE                                                               
055000       IF PRM-IDLAND = 'IE'                                               
055100         MOVE WS-IRELAND            TO SEND-ADDISPABS                     
055200       ELSE                                                               
055300       IF PRM-IDLAND = 'FR'                                               
055400         MOVE WS-FRANCE             TO SEND-ADDISPABS                     
055500       ELSE                                                               
055600       IF PRM-IDLAND = 'IT'                                               
055700         MOVE WS-ITALY              TO SEND-ADDISPABS                     
055800       ELSE                                                               
055900       IF PRM-IDLAND = 'GR'                                               
056000         MOVE WS-GREECE             TO SEND-ADDISPABS                     
056100       ELSE                                                               
056200       IF PRM-IDLAND = 'ES'                                               
056300         MOVE WS-SPAIN              TO SEND-ADDISPABS                     
056400       ELSE                                                               
056500       IF PRM-IDLAND = 'FI'                                               
056600         MOVE WS-FINLAND            TO SEND-ADDISPABS                     
056700       ELSE                                                               
056800       IF PRM-IDLAND = 'DK'                                               
056900         MOVE WS-DENMARK-2          TO SEND-ADDISPABS                     
057000       ELSE                                                               
057100       IF PRM-IDLAND = 'NL'                                               
057200         MOVE WS-NETHERLANDS        TO SEND-ADDISPABS                     
057300       ELSE                                                               
057400       IF PRM-IDLAND = 'DE'                                               
057500         MOVE WS-GERMANY            TO SEND-ADDISPABS                     
057600       ELSE                                                               
057700       IF PRM-IDLAND = 'PT'                                               
057800         MOVE WS-PORTUGAL           TO SEND-ADDISPABS                     
057900       ELSE                                                               
058000       IF PRM-IDLAND = 'AT'                                               
058100         MOVE WS-AUSTRIA            TO SEND-ADDISPABS                     
058200       END-IF                                                             
058300       END-IF                                                             
058400       END-IF                                                             
058500       END-IF                                                             
058600       END-IF                                                             
058700       END-IF                                                             
058800       END-IF                                                             
058900       END-IF                                                             
059000       END-IF                                                             
059100       END-IF                                                             
059200       END-IF                                                             
059300       END-IF                                                             
059400                                                                          
059500       CALL WZ01SEND USING  SEND-CONTROL-AREA                             
059600                            SEND-OPEN-AREA                                
059700       IF SEND-KDRC > 0                                                   
059800         MOVE SEND-KDRC             TO KDRC-DISPLAY                       
059900         STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                    
060000         DELIMITED BY SIZE INTO ERRTEXT-STR                               
060100         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
060200       END-IF                                                             
060300     END-IF                                                               
060400     .                                                                    
060500                                                                          
060600 S02-SEND-PUT SECTION.                                                    
060700     IF PRM-IDLAND = 'GB' OR 'IE' OR 'FR' OR 'IT' OR                      
060800                     'GR' OR 'ES' OR 'DK' OR 'FI' OR                      
060900                     'NL' OR 'DE' OR 'PT' OR 'AT'                         
061000       MOVE 'PUT'                 TO SEND-KDFUNC                          
061100       MOVE LENGTH OF W52211-AREA TO SEND-KVDLEN                          
061200       CALL WZ01SEND USING SEND-CONTROL-AREA                              
061300                           SEND-KVDLEN                                    
061400                           W52211-AREA                                    
061500       IF SEND-KDRC > 1                                                   
061600         MOVE SEND-KDRC          TO KDRC-DISPLAY                          
061700         STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                     
061800         DELIMITED BY SIZE INTO ERRTEXT-STR                               
061900         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
062000       END-IF                                                             
062100     END-IF                                                               
062200     .                                                                    
062300                                                                          
062400 S04-SEND-CLOSE SECTION.                                                  
062500     IF PRM-IDLAND = 'GB' OR 'IE' OR 'FR' OR 'IT' OR                      
062600                     'GR' OR 'ES' OR 'DK' OR 'FI' OR                      
062700                     'NL' OR 'DE' OR 'PT' OR 'AT'                         
062800       MOVE 'CLOSE'              TO SEND-KDFUNC                           
062900       CALL WZ01SEND USING SEND-CONTROL-AREA                              
063000     END-IF                                                               
063100     .                                                                    
063200     EJECT                                                                
063300                                                                          
