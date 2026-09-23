000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5226300.                                                
000301 AUTHOR.         MAMATHA SHETTY.                                          
000401 DATE-WRITTEN.   24/11/11.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNCTION:                                                            
000801*                                                                         
000901*        THE PROGRAM                                                      
001101*        - READS FILE W52261 -  VAT-DATA FROM IVW-TABLE                   
001201*        - SENDS FILE W52264A- VAT DATA TO SMARTFACTS                     
001301                                                                          
001401     SKIP3                                                                
001501 ENVIRONMENT DIVISION.                                                    
001601     SKIP2                                                                
001701 INPUT-OUTPUT SECTION.                                                    
001801                                                                          
001901 FILE-CONTROL.                                                            
002001                                                                          
002101*          --- SELECTED VAT-DATA FROM IVW-TABLE                           
002201     SELECT W52261                     ASSIGN TO W52263D1.                
002301                                                                          
002701*          --- SELECTED VAT-DATA TO D&P                                   
002801     SELECT W52264A                    ASSIGN TO W52263D2.                
002901     EJECT                                                                
003001                                                                          
003101 DATA DIVISION.                                                           
003201                                                                          
003301 FILE SECTION.                                                            
003401 FD  W52261                                                               
003501     RECORDING  F                                                         
003601     BLOCK CONTAINS 0.                                                    
003701 01  W52261-POST.                                                         
003801*    03  -COPY W522VAT  -L.                                               
003901                                                                          
004501 FD  W52264A                                                              
004601     RECORDING       V                                                    
004701     BLOCK CONTAINS  0.                                                   
004901 01  W52264A-POST                PIC X(258).                              
005001                                                                          
005101 WORKING-STORAGE SECTION.                                                 
005201 77  IDPGM                       PIC X(8)    VALUE 'W5226300'.            
005301 77  YES                         PIC X       VALUE 'Y'.                   
005401 77  NOO                         PIC X       VALUE 'N'.                   
005501 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005601 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005700     EJECT                                                                
007800 01  SAVE-IDLAND                 PIC X(2)    VALUE SPACE.                 
007900                                                                          
008000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500     EJECT                                                                
008600                                                                          
008700 01  WS-YDATE                    PIC X(8)    VALUE SPACE.                 
008800 01  WS-YDATE-CCAAMMDD  REDEFINES WS-YDATE.                               
008900     03 WS-YDATE-CC              PIC X(2).                                
009000     03 WS-YDATE-AAMMDD.                                                  
009100        05 WS-YDATEAA            PIC X(2).                                
009200        05 WS-YDATEMMDD          PIC X(4).                                
009300                                                                          
009400 01  CALCULATE-AREA.                                                      
009500     03  WS-SUNTO-TOT            PIC S9(10)V9(2) COMP-3.                  
009600     03  WS-SUVAT-BILLIT-TOT     PIC S9(10)V9(2) COMP-3.                  
009700     03  WS-SEK                  PIC 9(11)V9(5) VALUE ZERO.               
009800     03  WS-LOC                  PIC 9(11)V9(5) VALUE ZERO.               
009900                                                                          
010000 77  W52261-EOF-SW               PIC X       VALUE 'N'.                   
010100     88  END-OF-W52261                       VALUE 'Y'.                   
010200     EJECT                                                                
010300                                                                          
010800 01  ERRTEXT.                                                             
010900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
011000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
011100                                                                          
011201 01  FELTEXT                     PIC X(80).                               
011301                                                                          
011401 01  GENERAL-SUBPROGRAMS.                                                 
011501     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011601     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011701     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011801     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011901     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
012001     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
012100       EJECT                                                              
012200                                                                          
012300*    --- PARAMETERS TO ABEND                                              
012400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012700     EJECT                                                                
012800*                                                                         
012901*01  -COPY W510CURR                                                       
013001     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'IDLAND'.              
013200*01  -COPY WWLAND09                                                       
013300     EJECT                                                                
013400*                                                                         
013500*    -- SUBPROGRAM WZ20DAYS                                               
013600 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
013700*01 -COPY WZ20DAYS                                                        
013800     EJECT                                                                
013900*                                                                         
014000*    --- 522VAT FILE - INPUT                                              
014100 01  W52261-AREA-START           PIC X(24)   VALUE                        
014200                                             'W52261-AREA-START'.         
014300 01  W52261-AREA.                                                         
014400*    03  -COPY W522VAT     -PRE IN-                                       
014500     EJECT                                                                
014600*                                                                         
015400*                                                                         
015501*    --- W52264 FILE - OUTPUT                                             
015601 01  W52264A-AREA-START           PIC X(24)    VALUE                      
015701                                     'W52264A-AREA-START'.                
015702 01  W52264A-AREA.                                                        
015703     03  -COPY W52221A      -PRE W52264A-                                 
016100     EJECT                                                                
016201                                                                          
016301 01  TEXT-AREA.                                                           
016401     03  HEAD-LINE.                                                       
016501         05  FILLER          PIC X(11)  VALUE                             
016601            'PERIOD_YEAR'.                                                
016701         05  FILLER          PIC X(1)   VALUE ';'.                        
016801         05  FILLER          PIC X(12)   VALUE                            
016901            'PERIOD_MONTH'.                                               
017001         05  FILLER          PIC X(1)   VALUE ';'.                        
017101         05  FILLER          PIC X(15)   VALUE                            
017201            'SENDING_COUNTRY'.                                            
017301         05  FILLER          PIC X(1)   VALUE ';'.                        
017401         05  FILLER          PIC X(30)   VALUE                            
017501            'SENDING_COUNTRY_VAT_REG_NUMBER'.                             
017601         05  FILLER          PIC X(1)   VALUE ';'.                        
017701         05  FILLER          PIC X(14)  VALUE                             
017801            'PAYING_COUNTRY'.                                             
017901         05  FILLER          PIC X(1)    VALUE ';'.                       
018001         05  FILLER          PIC X(32)   VALUE                            
018101            'RECIEVING_COUNTRY_VAT_REG_NUMBER'.                           
018201         05  FILLER          PIC X(1)   VALUE ';'.                        
018301         05  FILLER          PIC X(15)  VALUE                             
018401            'DISTRICT_NUMBER'.                                            
018501         05  FILLER          PIC X(1)    VALUE ';'.                       
018601         05  FILLER          PIC X(15)   VALUE                            
018701            'CUSTOMER_NUMBER'.                                            
018801         05  FILLER          PIC X(1)    VALUE ';'.                       
018901         05  FILLER          PIC X(23)   VALUE                            
019001            'FINANCIAL_DOCUMENT_DATE'.                                    
019101         05  FILLER          PIC X(1)    VALUE ';'.                       
019201         05  FILLER          PIC X(25)   VALUE                            
019301            'FINANCIAL_DOCUMENT_NUMBER'.                                  
019401         05  FILLER          PIC X(1)    VALUE ';'.                       
019501         05  FILLER          PIC X(15)   VALUE                            
019601            'INVOICED_AMOUNT'.                                            
019701         05  FILLER          PIC X(1)    VALUE ';'.                       
019801         05  FILLER          PIC X(33)   VALUE                            
019901            'VAT_AMOUNT_PER_FINANCIAL_DOCUMENT'.                          
020000*                                                                         
027300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
027400     SKIP2                                                                
027500*01  -COPY WDATKORT                                                       
027600     EJECT                                                                
027700*                                                                         
027801 LINKAGE SECTION.                                                         
027901                                                                          
028001*01  -COPY W0008  -PRE WDG2-                                              
028101     05  FILLER                  PIC X.                                   
028201                                                                          
028301 PROCEDURE DIVISION  USING WDG2-PCB.                                      
028401     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
028501                                                                          
028601     PERFORM A-INIT                                                       
028701     PERFORM BA-CREATE-HEADER                                             
029101       PERFORM S11-READ-W52261                                            
029201       PERFORM UNTIL END-OF-W52261                                        
029701             PERFORM B-EXECUTE                                            
029901          PERFORM S11-READ-W52261                                         
030001       END-PERFORM                                                        
030801                                                                          
030901     PERFORM Z-FINISH                                                     
031001     MOVE ZERO TO RETURN-CODE                                             
031101     GOBACK                                                               
031201     .                                                                    
031301     EJECT                                                                
031401                                                                          
031501 A-INIT SECTION.                                                          
031601                                                                          
031701     OPEN INPUT  W52261                                                   
031901     OPEN OUTPUT W52264A                                                  
032001     MOVE FUNCTION  CURRENT-DATE(3:6)  TO DAGENS-DATUM                    
032101     MOVE FUNCTION  CURRENT-DATE(1:2)  TO WS-YDATE-CC                     
032201                                                                          
032301     MOVE DAGENS-DATUM TO DAYS-TIDATE1                                    
032401     MOVE 'YYMMDD'     TO DAYS-KDDATFMT1                                  
032501     MOVE 'YYMMDD'     TO DAYS-KDDATFMT2                                  
032601     MOVE SPACE        TO DAYS-TIDATE2                                    
032701                          DAYS-IDCALEND                                   
032801     MOVE -1           TO DAYS-KVDAYS                                     
032901                                                                          
033001     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
033101                                                                          
033201     MOVE DAYS-TIDATE2(1:6) TO WS-YDATE-AAMMDD                            
033301     .                                                                    
033401     EJECT                                                                
033501                                                                          
033601 B-EXECUTE SECTION.                                                       
033701     IF IN-KDFINDOC = 'ECO'                                               
033801       IF IN-IDLANDX3-SEND > SPACE                                        
033901         PERFORM BA-CREATE-W52264A                                        
034001         PERFORM S12-WRITE-W52264A                                        
034101       ELSE                                                               
034201         IF IN-IDLANDX3-BET > SPACE                                       
034301           PERFORM BA-CREATE-W52264A                                      
034401           PERFORM S12-WRITE-W52264A                                      
034501         END-IF                                                           
034601       END-IF                                                             
034701     END-IF                                                               
034801                                                                          
034901     IF IN-KDFINDOC NOT = 'ECO'                                           
035001       PERFORM BA-CREATE-W52264A                                          
035101       PERFORM S12-WRITE-W52264A                                          
035201     END-IF                                                               
035301     .                                                                    
035401     EJECT                                                                
035501                                                                          
035601 BA-CREATE-W52264A SECTION.                                               
036301     MOVE IN-IDLANDX3-SEND     TO W52264A-IDLANDX3-SEND                   
036401     MOVE IN-IDLANDX3-BET      TO W52264A-IDLANDX3-BET                    
036501     MOVE '20'                 TO W52264A-TIAAAA(1:2)                     
036602     MOVE IN-TIAA              TO W52264A-TIAAAA(3:2)                     
036604     MOVE IN-TIRP              TO W52264A-TIRP                            
036801     MOVE IN-IDVAT-BET         TO W52264A-IDVAT-REC                       
036901     MOVE IN-IDVAT-RESP        TO W52264A-IDVAT-SEND                      
037001     MOVE IN-IDDISTR           TO W52264A-IDDISTR                         
037101     MOVE IN-IDKUNDNR          TO W52264A-IDKUNDNR                        
037201     MOVE IN-IDFINDOC          TO W52264A-IDFAKT                          
037301     MOVE IN-DAFINDOC          TO W52264A-TIFAKT                          
037401                                                                          
037501     IF IN-KDFINDOC = 'CR' OR 'CR2'                                       
037601       COMPUTE WS-SUNTO-TOT = IN-SUNTO-TOT *                              
037701                              -1                                          
037801       END-COMPUTE                                                        
037901       COMPUTE WS-SUVAT-BILLIT-TOT = IN-SUVAT-BILLIT-TOT *                
038001                                     -1                                   
038101       END-COMPUTE                                                        
038201       MOVE WS-SUNTO-TOT         TO W52264A-SUFKTTOT-LOC                  
038301       MOVE WS-SUVAT-BILLIT-TOT  TO W52264A-SUVAT-FAKT-LOC                
038401     ELSE                                                                 
038501       MOVE IN-SUNTO-TOT         TO W52264A-SUFKTTOT-LOC                  
038601       MOVE IN-SUVAT-BILLIT-TOT  TO W52264A-SUVAT-FAKT-LOC                
038701     END-IF                                                               
038801                                                                          
038901     CALL DATKORT USING IDPGM  DATUMKORT-ID DATUMKORT                     
039001     MOVE D-AAR                       TO W-DATE-AAMM(1:2)                 
039101     MOVE D-MAANAD                    TO W-DATE-AAMM(3:2)                 
039201     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
039301     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
039401     MOVE 'M'                         TO CURR-KDVALTYP                    
039402     IF W52264A-IDLANDX3-BET =                                            
039403                           'IE' OR 'NL' OR 'FR' OR 'BE' OR                
039404                           'DE' OR 'IT' OR 'GR' OR 'ES' OR                
039405                           'PT' OR 'AT' OR 'FI' OR 'CY'                   
039406       MOVE 'EUR'        TO CURR-KDVALISO-ROW                             
039407       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
039408      ELSE                                                                
039409      IF W52264A-IDLANDX3-BET  = 'SE'                                     
039410        MOVE 'SEK'      TO CURR-KDVALISO-ROW                              
039411        CALL W510CURR USING CURR-W510CURR WDG2-PCB                        
039420      ELSE                                                                
039430       IF W52264A-IDLANDX3-BET = 'GB'                                     
039440         MOVE 'GBP'      TO CURR-KDVALISO-ROW                             
039450         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
039460       ELSE                                                               
039470         IF W52264A-IDLANDX3-BET = 'DK'                                   
039480           MOVE 'DKK'      TO CURR-KDVALISO-ROW                           
039490           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
039500         ELSE                                                             
039501           IF W52264A-IDLANDX3-BET = 'NO'                                 
039502             MOVE 'NOK'      TO CURR-KDVALISO-ROW                         
039503             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
039504           ELSE                                                           
039505             IF W52264A-IDLANDX3-BET = 'PL'                               
039506               MOVE 'PLN'      TO CURR-KDVALISO-ROW                       
039507               CALL W510CURR USING CURR-W510CURR WDG2-PCB                 
039508             ELSE                                                         
039509               IF W52264A-IDLANDX3-BET = 'CZ'                             
039510                 MOVE 'CZK'      TO CURR-KDVALISO-ROW                     
039511                 CALL W510CURR USING CURR-W510CURR WDG2-PCB               
039512               ELSE                                                       
039513                 IF W52264A-IDLANDX3-BET  = 'HU'                          
039514                   MOVE 'HUF'      TO CURR-KDVALISO-ROW                   
039515                   CALL W510CURR USING CURR-W510CURR WDG2-PCB             
039516                 ELSE                                                     
039517                 IF W52264A-IDLANDX3-BET NOT =                            
039518                     'GB' OR 'DK' OR 'PL' OR 'DE' OR 'IT' OR              
039519                     'NO' OR 'CZ' OR 'HU' OR 'GR' OR 'ES' OR              
039520                     'IE' OR 'NL' OR 'FR' OR 'BE' OR 'SE' OR              
039522                     'PT' OR 'AT' OR 'FI' OR 'CY'                         
039523                   MOVE 'SEK'      TO CURR-KDVALISO-ROW                   
039524                   CALL W510CURR USING CURR-W510CURR WDG2-PCB             
039525                 ELSE                                                     
039526                      STRING 'INVALID COUNTRY CODE:'                      
039527                              W52264A-IDLANDX3-BET                        
039528                      DELIMITED BY SIZE INTO ERRTEXT-STR                  
039529                      CALL ABEND USING RKOD-ABEND-WITH-DUMP               
039530                   END-IF                                                 
039531                 END-IF                                                   
039532                END-IF                                                    
039533              END-IF                                                      
039534            END-IF                                                        
039535          END-IF                                                          
039536        END-IF                                                            
039537      END-IF                                                              
039538     END-IF                                                               
042001                                                                          
042101     IF IN-KDVALISO = CURR-KDVALISO-ROW                                   
042201*    --- NO COLLECTION (DEALER-NET / DDI)                                 
042301       CONTINUE                                                           
042401     ELSE                                                                 
042501       MOVE W52264A-SUFKTTOT-LOC TO WS-SUNTO-TOT                          
042601       MOVE W52264A-SUVAT-FAKT-LOC TO WS-SUVAT-BILLIT-TOT                 
042701       IF IN-KDVALISO NOT = 'SEK'                                         
042801*    --- CALCULATED AMOUNT LOCAL CURRENCY -> SEK                          
042901         COMPUTE WS-SEK ROUNDED = WS-SUNTO-TOT *                          
043001                                  IN-PRKURS                               
043101*    --- CALCULATED AMOUNTS SEK -> LOCAL CURRENCY                         
043201         COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)              
043301         MOVE WS-LOC           TO W52264A-SUFKTTOT-LOC                    
043401                                                                          
043501*    --- CALCULATED AMOUNT LOCAL CURRENCY -> SEK                          
043601         COMPUTE WS-SEK ROUNDED = WS-SUVAT-BILLIT-TOT *                   
043701                                  IN-PRKURS                               
043801*    --- CALCULATED AMOUNTS SEK -> LOCAL CURRENCY                         
043901         COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)              
044001         MOVE WS-LOC           TO W52264A-SUVAT-FAKT-LOC                  
044101       ELSE                                                               
044201*    --- CALCULATED AMOUNTS IN LOCAL CURRENCY                             
044301         COMPUTE WS-LOC ROUNDED = WS-SUNTO-TOT /                          
044401                                  CURR-PRKURS-NEW                         
044501         MOVE WS-LOC           TO W52264A-SUFKTTOT-LOC                    
044601                                                                          
044701*    --- CALCULATED AMOUNTS IN LOCAL CURRENCY                             
044801         COMPUTE WS-LOC ROUNDED = WS-SUVAT-BILLIT-TOT /                   
044901                                  CURR-PRKURS-NEW                         
045001         MOVE WS-LOC           TO W52264A-SUVAT-FAKT-LOC                  
045101       END-IF                                                             
045201     END-IF                                                               
045301     .                                                                    
045401     EJECT                                                                
045501                                                                          
045601 BA-CREATE-HEADER  SECTION.                                               
045701     WRITE W52264A-POST FROM HEAD-LINE                                    
046000     .                                                                    
046100     EJECT                                                                
046200                                                                          
046300 Z-FINISH SECTION.                                                        
046400     CLOSE W52261                                                         
046601           W52264A                                                        
046700     .                                                                    
046800     EJECT                                                                
046900                                                                          
047600                                                                          
047700 S11-READ-W52261  SECTION.                                                
047800     READ W52261 INTO W52261-AREA                                         
047900     AT END                                                               
048000        MOVE YES TO W52261-EOF-SW                                         
048100     END-READ                                                             
048200     .                                                                    
048300                                                                          
048401 S12-WRITE-W52264A SECTION.                                               
048601     WRITE W52264A-POST FROM W52264A-AREA                                 
048700     .                                                                    
048800     EJECT                                                                
048900                                                                          
