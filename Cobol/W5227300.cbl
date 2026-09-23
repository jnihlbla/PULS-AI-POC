000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5227300.                                                
000300 AUTHOR.         MAMATHA SHETTY.                                          
000400 DATE-WRITTEN.   24/11/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*        THE PROGRAM                                                      
001000*        - READS FILE W52271 -  INT-DATA FROM IVW-TABLE                   
001200*        - SENDS FILE W52273A- INT DATA TO SMARTFACTS                     
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*          --- SELECTED INT-DATA FROM IVW-TABLE                           
002200     SELECT W52271                     ASSIGN TO W52273D1.                
002300                                                                          
002700*          --- SELECTED INT-DATA TO D&P                                   
002800     SELECT W52273A                    ASSIGN TO W52273D2.                
002900     EJECT                                                                
003000                                                                          
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400 FD  W52271                                                               
003500     RECORDING  F                                                         
003600     BLOCK CONTAINS 0.                                                    
003700 01  W52271-POST.                                                         
003800*    03  -COPY W522INT  -L.                                               
003900                                                                          
004500 FD  W52273A                                                              
004600     RECORDING       V                                                    
004700     BLOCK CONTAINS  0.                                                   
004800 01  W52273A-POST                PIC X(500).                              
004900                                                                          
005000 WORKING-STORAGE SECTION.                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W5227300'.            
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005500 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005600     EJECT                                                                
005700 01  SAVE-IDLAND                 PIC X(2)    VALUE SPACE.                 
005710 01  WS-SUNTO                    PIC S9(11)V9(2) VALUE ZERO.              
005800                                                                          
005810 01  IDARTNR-UNSTRING.                                                    
005820     03  WS-IDARTNR-UNSTR        PIC X(9)    VALUE SPACE.                 
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     EJECT                                                                
008300                                                                          
008400 01  WS-YDATE                    PIC X(8)    VALUE SPACE.                 
008500 01  WS-YDATE-CCAAMMDD  REDEFINES WS-YDATE.                               
008600     03 WS-YDATE-CC              PIC X(2).                                
008700     03 WS-YDATE-AAMMDD.                                                  
008800        05 WS-YDATEAA            PIC X(2).                                
008900        05 WS-YDATEMMDD          PIC X(4).                                
009000                                                                          
009100 01  CALCULATE-AREA.                                                      
009200     03  WS-SUNTO-TOT            PIC S9(10)V9(2) COMP-3.                  
009300     03  WS-SUINT-BILLIT-TOT     PIC S9(10)V9(2) COMP-3.                  
009400     03  WS-SEK                  PIC 9(11)V9(5) VALUE ZERO.               
009500     03  WS-LOC                  PIC 9(11)V9(5) VALUE ZERO.               
009600                                                                          
009700 77  W52271-EOF-SW               PIC X       VALUE 'N'.                   
009800     88  END-OF-W52271                       VALUE 'Y'.                   
009900     EJECT                                                                
010000                                                                          
010400                                                                          
010500 01  ERRTEXT.                                                             
010600     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
010700     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
010800                                                                          
010900 01  FELTEXT                     PIC X(80).                               
011000                                                                          
011100 01  GENERAL-SUBPROGRAMS.                                                 
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011600     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
011700     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
011800       EJECT                                                              
011900                                                                          
012000*    --- PARAMETERS TO ABEND                                              
012100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012400     EJECT                                                                
012500*                                                                         
012600*01  -COPY W510CURR                                                       
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'IDLAND'.              
012900*01  -COPY WWLAND09                                                       
013000     EJECT                                                                
013040*01  -COPY WWLANDX2                                                       
013050     EJECT                                                                
013060*01  -COPY WWDIST35                                                       
013070     EJECT                                                                
013100*                                                                         
013200*    -- SUBPROGRAM WZ20DAYS                                               
013300 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
013400*01 -COPY WZ20DAYS                                                        
013500     EJECT                                                                
013600*                                                                         
013700*    --- 522INT FILE - INPUT                                              
013800 01  W52271-AREA-START           PIC X(24)   VALUE                        
013900                                             'W52271-AREA-START'.         
014000 01  W52271-AREA.                                                         
014100*    03  -COPY W522INT     -PRE IN-                                       
014200     EJECT                                                                
014300*                                                                         
015100*                                                                         
015200*    --- W52273 FILE - OUTPUT                                             
015300 01  W52273A-AREA-START           PIC X(24)    VALUE                      
015400                                     'W52273A-AREA-START'.                
015500 01  W52273A-AREA.                                                        
015600*    03  -COPY W52213A      -PRE W52273A-                                 
015700     EJECT                                                                
015800                                                                          
015900 01  TEXT-AREA.                                                           
016000     03  HEAD-LINE.                                                       
017000         05  FILLER          PIC X(11)  VALUE                             
018000            'PERIOD_YEAR'.                                                
019000         05  FILLER          PIC X(1)   VALUE ';'.                        
019100         05  FILLER          PIC X(12)   VALUE                            
019200            'PERIOD_MONTH'.                                               
019300         05  FILLER          PIC X(1)   VALUE ';'.                        
019400         05  FILLER          PIC X(15)   VALUE                            
019500            'SENDING_COUNTRY'.                                            
019600         05  FILLER          PIC X(1)   VALUE ';'.                        
019700         05  FILLER          PIC X(17)   VALUE                            
019800            'RECIEVING_COUNTRY'.                                          
019900         05  FILLER          PIC X(1)   VALUE ';'.                        
020000         05  FILLER          PIC X(08)  VALUE                             
020100            'CURRENCY'.                                                   
020200         05  FILLER          PIC X(1)    VALUE ';'.                       
020300         05  FILLER          PIC X(13)   VALUE                            
020400            'CURRENCY_RATE'.                                              
020500         05  FILLER          PIC X(1)   VALUE ';'.                        
020600         05  FILLER          PIC X(15)  VALUE                             
020700            'OLD_ACTION_CODE'.                                            
020800         05  FILLER          PIC X(1)    VALUE ';'.                       
020900         05  FILLER          PIC X(13)   VALUE                            
021000            'DOCUMENT_DATE'.                                              
021100         05  FILLER          PIC X(1)    VALUE ';'.                       
021200         05  FILLER          PIC X(18)   VALUE                            
021300            'FINANCIAL_DOCUMENT'.                                         
021400         05  FILLER          PIC X(1)    VALUE ';'.                       
021500         05  FILLER          PIC X(18)   VALUE                            
021600            'STATISTICAL_NUMBER'.                                         
021700         05  FILLER          PIC X(1)    VALUE ';'.                       
021800         05  FILLER          PIC X(11)   VALUE                            
021900            'PART_NUMBER'.                                                
022000         05  FILLER          PIC X(1)    VALUE ';'.                       
022100         05  FILLER          PIC X(16)   VALUE                            
022200            'PART_DESCRIPTION'.                                           
022300         05  FILLER          PIC X(1)   VALUE ';'.                        
022400         05  FILLER          PIC X(10)   VALUE                            
022500            'NET_WEIGHT'.                                                 
022600         05  FILLER          PIC X(1)   VALUE ';'.                        
022700         05  FILLER          PIC X(13)   VALUE                            
022800            'DELIVERED_QTY'.                                              
022900         05  FILLER          PIC X(1)   VALUE ';'.                        
023000         05  FILLER          PIC X(17)   VALUE                            
023100            'COUNTRY_OF_ORIGIN'.                                          
023200         05  FILLER          PIC X(1)   VALUE ';'.                        
023300         05  FILLER          PIC X(14)  VALUE                             
023400            'NET_TOTAL_SALE'.                                             
023500         05  FILLER          PIC X(1)    VALUE ';'.                       
023600         05  FILLER          PIC X(17)   VALUE                            
023700            'TERMS_OF_DELIVERY'.                                          
023800         05  FILLER          PIC X(1)   VALUE ';'.                        
023900         05  FILLER          PIC X(21)  VALUE                             
024000            'TREATMENT_STATUS_CODE'.                                      
024100         05  FILLER          PIC X(1)    VALUE ';'.                       
024200         05  FILLER          PIC X(15)   VALUE                            
024300            'DISTRICT_NUMBER'.                                            
024400         05  FILLER          PIC X(1)    VALUE ';'.                       
024500         05  FILLER          PIC X(17)   VALUE                            
024600            'CUSTOMER_DOCUMENT'.                                          
024700         05  FILLER          PIC X(1)    VALUE ';'.                       
024800         05  FILLER          PIC X(14)   VALUE                            
024900            'VAT_REG_NUMBER'.                                             
025000         05  FILLER          PIC X(1)    VALUE ';'.                       
025100         05  FILLER          PIC X(12)   VALUE                            
025200            'ACTION_CODE'.                                                
025300         05  FILLER          PIC X(1)    VALUE ';'.                       
025400         05  FILLER          PIC X(22)   VALUE                            
025500            '3_DECIMAL_NET_WEIGHT'.                                       
025600         05  FILLER          PIC X(1)    VALUE ';'.                       
025700         05  FILLER          PIC X(22)   VALUE                            
025800            'PAYER_VAT_REG_NUMBER'.                                       
025900         05  FILLER          PIC X(1)   VALUE ';'.                        
026000         05  FILLER          PIC X(28)  VALUE                             
026100            'RESPONSIBLE_VAT_REG_NUMBER'.                                 
026200         05  FILLER          PIC X(1)    VALUE ';'.                       
026300         05  FILLER          PIC X(29)   VALUE                            
026400            'LEGAL_SELLER_VAT_REG_NUMBER'.                                
026500         05  FILLER          PIC X(1)    VALUE ';'.                       
026600         05  FILLER          PIC X(22)   VALUE                            
026700            'AGENT_VAT_REG_NUMBER'.                                       
026800                                                                          
026900*                                                                         
027000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
027100     SKIP2                                                                
027200*01  -COPY WDATKORT                                                       
027300     EJECT                                                                
027400*                                                                         
027500 LINKAGE SECTION.                                                         
027600                                                                          
027700*01  -COPY W0008  -PRE WDG2-                                              
027800     05  FILLER                  PIC X.                                   
027900                                                                          
028000 PROCEDURE DIVISION  USING WDG2-PCB.                                      
028100     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
028200                                                                          
028300     PERFORM A-INIT                                                       
028400     PERFORM BA-CREATE-HEADER                                             
028500                                                                          
028800       PERFORM S11-READ-W52271                                            
028900       PERFORM UNTIL END-OF-W52271                                        
029000*      IF IN-KDFINDOC = 'CR'                                              
029100*** INTRASTAT ONLY TO SEND AND REC COUNTRIES                              
031300           PERFORM B-EXECUTE                                              
031600       PERFORM S11-READ-W52271                                            
031700     END-PERFORM                                                          
033300                                                                          
033400     PERFORM Z-FINISH                                                     
033500     MOVE ZERO TO RETURN-CODE                                             
033600     GOBACK                                                               
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 A-INIT SECTION.                                                          
034100                                                                          
034200     OPEN INPUT  W52271                                                   
034400     OPEN OUTPUT W52273A                                                  
034500     MOVE FUNCTION  CURRENT-DATE(3:6)  TO DAGENS-DATUM                    
034600     MOVE FUNCTION  CURRENT-DATE(1:2)  TO WS-YDATE-CC                     
034700                                                                          
034800     MOVE DAGENS-DATUM TO DAYS-TIDATE1                                    
034900     MOVE 'YYMMDD'     TO DAYS-KDDATFMT1                                  
035000     MOVE 'YYMMDD'     TO DAYS-KDDATFMT2                                  
035100     MOVE SPACE        TO DAYS-TIDATE2                                    
035200                          DAYS-IDCALEND                                   
035300     MOVE -1           TO DAYS-KVDAYS                                     
035400                                                                          
035500     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
035600                                                                          
035700     MOVE DAYS-TIDATE2(1:6) TO WS-YDATE-AAMMDD                            
035710     CALL DATKORT USING IDPGM  DATUMKORT-ID DATUMKORT                     
035720     MOVE D-AAR                       TO W-DATE-AAMM(1:2)                 
035730     MOVE D-MAANAD                    TO W-DATE-AAMM(3:2)                 
035740     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
035750     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
035760     MOVE 'M'                         TO CURR-KDVALTYP                    
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
037001 B-EXECUTE SECTION.                                                       
037101                                                                          
037102       MOVE IN-IDLANDX3-SEND     TO W52273A-IDLANDX3-SEND                 
037103       MOVE IN-IDLANDX3-REC      TO W52273A-IDLANDX3-REC                  
037201     IF W52273A-IDLANDX3-REC =                                            
037202                           'IE' OR 'NL' OR 'FR' OR 'BE' OR 'DE' OR        
037301                     'IT' OR 'GR' OR 'ES' OR 'FI' OR 'PT' OR              
037401                     'AT' OR 'CY'                                         
037501       MOVE 'EUR'        TO CURR-KDVALISO-ROW                             
037601       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
037701     ELSE                                                                 
037801      IF W52273A-IDLANDX3-REC = 'SE'                                      
037901        MOVE 'SEK'      TO CURR-KDVALISO-ROW                              
038001        CALL W510CURR USING CURR-W510CURR WDG2-PCB                        
038101       ELSE                                                               
038201       IF W52273A-IDLANDX3-REC = 'GB'                                     
038301         MOVE 'GBP'      TO CURR-KDVALISO-ROW                             
038401         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
038501       ELSE                                                               
038601         IF W52273A-IDLANDX3-REC = 'DK'                                   
038701           MOVE 'DKK'      TO CURR-KDVALISO-ROW                           
038801           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
038901         ELSE                                                             
039001           IF W52273A-IDLANDX3-REC = 'PL'                                 
039101             MOVE 'PLN'      TO CURR-KDVALISO-ROW                         
039201             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
039301           ELSE                                                           
039401             IF W52273A-IDLANDX3-REC = 'CZ'                               
039501               MOVE 'CZK'      TO CURR-KDVALISO-ROW                       
039601               CALL W510CURR USING CURR-W510CURR WDG2-PCB                 
039701             ELSE                                                         
039801               IF W52273A-IDLANDX3-REC = 'HU'                             
039901                 MOVE 'HUF'      TO CURR-KDVALISO-ROW                     
040001                 CALL W510CURR USING CURR-W510CURR WDG2-PCB               
040101               ELSE                                                       
040102                 IF W52273A-IDLANDX3-REC NOT =                            
040103                     'GB' OR 'DK' OR 'PL' OR 'DE' OR 'IT' OR              
040104                     'NO' OR 'CZ' OR 'HU' OR 'GR' OR 'ES' OR              
040105                     'IE' OR 'NL' OR 'FR' OR 'BE' OR 'SE' OR              
040106                     'PT' OR 'AT' OR 'FI' OR 'CY'                         
040107                   MOVE 'SEK'      TO CURR-KDVALISO-ROW                   
040108                   CALL W510CURR USING CURR-W510CURR WDG2-PCB             
040109                ELSE                                                      
040201                 STRING 'INVALID COUNTRY CODE:'                           
040202                  W52273A-IDLANDX3-REC                                    
040301                 DELIMITED BY SIZE INTO ERRTEXT-STR                       
040401                 CALL ABEND USING RKOD-ABEND-WITH-DUMP                    
040501                END-IF                                                    
040601              END-IF                                                      
040701            END-IF                                                        
040801          END-IF                                                          
040901        END-IF                                                            
041001       END-IF                                                             
041101      END-IF                                                              
041102     END-IF                                                               
041201                                                                          
041301     IF IN-KDFINDOC = 'ECO'                                               
041401       IF IN-IDLANDX3-SEND > SPACE                                        
041501**** CHECK IF IT'S A REFILL FROM SWEDEN TO EU COUNTRY                     
041601**** AND MOVE VALUES TO KDINTTYP                                          
041701         MOVE IN-IDDISTR   TO DIST35-IDDISTR                              
041801         IF DIST35-REFILL                                                 
041901           IF IN-IDLANDX3-REC > SPACE                                     
042001             MOVE IN-IDLANDX3-REC TO LANDX2-IDLANDX2                      
042101           ELSE                                                           
042201             MOVE IN-IDLANDX3-BET TO LANDX2-IDLANDX2                      
042301           END-IF                                                         
042401           IF LANDX2-EU-IDLANDX2                                          
042501             MOVE +31      TO W52273A-KDINTTYP                            
042601             MOVE +3       TO W52273A-KDINTTYP-OLD                        
042701           ELSE                                                           
042801             MOVE +99      TO W52273A-KDINTTYP                            
042901             MOVE +9       TO W52273A-KDINTTYP-OLD                        
043001           END-IF                                                         
043101         ELSE                                                             
043201           MOVE +99        TO W52273A-KDINTTYP                            
043301           MOVE +9         TO W52273A-KDINTTYP-OLD                        
043401         END-IF                                                           
043501****                                                                      
043601         PERFORM BA-CREATE-W52273A                                        
043801         PERFORM S12-WRITE-W52273A                                        
043901       ELSE                                                               
044001         IF IN-IDLANDX3-REC > SPACE                                       
044101**** CHECK IF IT'S A REFILL FROM SWEDEN TO EU COUNTRY                     
044201**** AND MOVE VALUES TO KDINTTYP                                          
044301           MOVE IN-IDDISTR TO DIST35-IDDISTR                              
044401           IF DIST35-REFILL                                               
044501             IF IN-IDLANDX3-REC > SPACE                                   
044601               MOVE IN-IDLANDX3-REC TO LANDX2-IDLANDX2                    
044701             ELSE                                                         
044801               MOVE IN-IDLANDX3-BET TO LANDX2-IDLANDX2                    
044901             END-IF                                                       
045001             IF LANDX2-EU-IDLANDX2                                        
045101               MOVE +31    TO W52273A-KDINTTYP                            
045201               MOVE +3     TO W52273A-KDINTTYP-OLD                        
045301             ELSE                                                         
045401               MOVE +99    TO W52273A-KDINTTYP                            
045501               MOVE +9     TO W52273A-KDINTTYP-OLD                        
045601             END-IF                                                       
045701           ELSE                                                           
045801             MOVE +99      TO W52273A-KDINTTYP                            
045901             MOVE +9       TO W52273A-KDINTTYP-OLD                        
046001           END-IF                                                         
046101****                                                                      
046201           PERFORM BA-CREATE-W52273A                                      
046401           PERFORM S12-WRITE-W52273A                                      
046501         END-IF                                                           
046601       END-IF                                                             
046701     END-IF                                                               
046801                                                                          
046901     IF IN-KDFINDOC = 'EXC'                                               
047001       IF IN-IDLANDX3-SEND > SPACE                                        
047101         MOVE +41        TO W52273A-KDINTTYP                              
047201         MOVE +4         TO W52273A-KDINTTYP-OLD                          
047301         PERFORM BA-CREATE-W52273A                                        
047501         PERFORM S12-WRITE-W52273A                                        
047601       END-IF                                                             
047701                                                                          
047801       IF IN-IDLANDX3-REC > SPACE                                         
047901         MOVE +41        TO W52273A-KDINTTYP                              
048001         MOVE +4         TO W52273A-KDINTTYP-OLD                          
048101         PERFORM BA-CREATE-W52273A                                        
048301         PERFORM S12-WRITE-W52273A                                        
048401       END-IF                                                             
048501     END-IF                                                               
048601                                                                          
048701     IF IN-KDFINDOC = 'INL'                                               
048801       MOVE +11          TO W52273A-KDINTTYP                              
048901       MOVE +1           TO W52273A-KDINTTYP-OLD                          
049001       PERFORM BA-CREATE-W52273A                                          
049201       PERFORM S12-WRITE-W52273A                                          
049301     END-IF                                                               
049401                                                                          
049501     IF IN-KDFINDOC NOT = 'ECO' AND 'EXC'                                 
049601       IF IN-IDLANDX3-SEND > SPACE                                        
049701         IF IN-KDFINDOC = 'INV'                                           
049801           MOVE +11       TO W52273A-KDINTTYP                             
049901           MOVE +1        TO W52273A-KDINTTYP-OLD                         
050001           PERFORM BA-CREATE-W52273A                                      
050201           PERFORM S12-WRITE-W52273A                                      
050301         ELSE                                                             
050401           IF IN-KDFINDOC = 'CR'                                          
050501             MOVE +21  TO W52273A-KDINTTYP                                
050601             MOVE +2   TO W52273A-KDINTTYP-OLD                            
050701             PERFORM BA-CREATE-W52273A                                    
050901             PERFORM S12-WRITE-W52273A                                    
051001           ELSE                                                           
051101**** CHECK IF IT'S A REFILL FROM SWEDEN TO EU COUNTRY                     
051201**** AND MOVE VALUES TO KDINTTYP                                          
051301             MOVE IN-IDDISTR   TO DIST35-IDDISTR                          
051401             IF DIST35-REFILL                                             
051501               IF IN-IDLANDX3-REC > SPACE                                 
051601                 MOVE IN-IDLANDX3-REC TO LANDX2-IDLANDX2                  
051701               ELSE                                                       
051801                 MOVE IN-IDLANDX3-BET TO LANDX2-IDLANDX2                  
051901               END-IF                                                     
052001               IF LANDX2-EU-IDLANDX2                                      
052101                 MOVE +31      TO W52273A-KDINTTYP                        
052201                 MOVE +3       TO W52273A-KDINTTYP-OLD                    
052301               ELSE                                                       
052401                 MOVE +99      TO W52273A-KDINTTYP                        
052501                 MOVE +9       TO W52273A-KDINTTYP-OLD                    
052601               END-IF                                                     
052701             ELSE                                                         
052801               MOVE +99        TO W52273A-KDINTTYP                        
052901               MOVE +9         TO W52273A-KDINTTYP-OLD                    
053001             END-IF                                                       
053101****                                                                      
053201             PERFORM BA-CREATE-W52273A                                    
053401             PERFORM S12-WRITE-W52273A                                    
053501           END-IF                                                         
053601         END-IF                                                           
053701       END-IF                                                             
053801     END-IF                                                               
053901     .                                                                    
054001     EJECT                                                                
054101                                                                          
054200                                                                          
054301 BA-CREATE-W52273A SECTION.                                               
054401     MOVE '20'                   TO W52273A-TIAAAA(1:2)                   
054501     MOVE IN-TIAA                TO W52273A-TIAAAA(3:2)                   
054601     MOVE IN-TIRP                TO W52273A-TIMM                          
054701                                                                          
054801     IF IN-KDFINDOC = 'ECO' OR 'INL' OR 'EXC'                             
054901       MOVE IN-IDLANDX3-SEND     TO W52273A-IDLANDX3-SEND                 
055001       MOVE IN-IDLANDX3-REC      TO W52273A-IDLANDX3-REC                  
056001       MOVE IN-IDVAT-BET         TO W52273A-IDVAT                         
057001     ELSE                                                                 
058001       IF IN-KDFINDOC = 'CR'                                              
059001*** FOR CREDIT - WHEN SENDING,RECEIVING AND PAYING COUNTRIES ARE          
060001*** PRESENT                                                               
061001         IF IN-IDLANDX3-REC > SPACE                                       
062001           MOVE IN-IDLANDX3-BET    TO W52273A-IDLANDX3-SEND               
063001           MOVE IN-IDLANDX3-REC    TO W52273A-IDLANDX3-REC                
064001         ELSE                                                             
065001           MOVE IN-IDLANDX3-BET    TO W52273A-IDLANDX3-SEND               
065101           MOVE IN-IDLANDX3-SEND   TO W52273A-IDLANDX3-REC                
065201         END-IF                                                           
065301         MOVE IN-IDVAT-RESP        TO W52273A-IDVAT                       
065401       ELSE                                                               
065501         IF IN-IDLANDX3-REC > SPACE                                       
065601           MOVE IN-IDLANDX3-REC  TO W52273A-IDLANDX3-SEND                 
065701           MOVE IN-IDLANDX3-BET  TO W52273A-IDLANDX3-REC                  
065801         ELSE                                                             
065901           MOVE IN-IDLANDX3-SEND TO W52273A-IDLANDX3-SEND                 
066001           MOVE IN-IDLANDX3-BET  TO W52273A-IDLANDX3-REC                  
066101         END-IF                                                           
066201         MOVE IN-IDVAT-BET       TO W52273A-IDVAT                         
066301       END-IF                                                             
066401     END-IF                                                               
066501                                                                          
066601                                                                          
066701     IF IN-KDFINDOC = 'ECO' OR 'EXC' OR 'INL'                             
066801       IF W52273A-IDLANDX3-REC = 'GB' OR 'IT' OR 'ES' OR 'PT'             
066901         MOVE 'SEK'              TO W52273A-KDVALISO                      
067001       ELSE                                                               
067101         IF W52273A-IDLANDX3-REC = 'AT'                                   
067201           MOVE 'EUR'            TO W52273A-KDVALISO                      
067301         END-IF                                                           
067401       END-IF                                                             
067501     ELSE                                                                 
067601       MOVE IN-KDVALISO          TO W52273A-KDVALISO                      
067701     END-IF                                                               
067801     MOVE IN-IDVAT-BET           TO W52273A-IDVAT-BET                     
067901     MOVE IN-IDVAT-RESP          TO W52273A-IDVAT-RESP                    
068001     MOVE IN-IDVAT-LEG           TO W52273A-IDVAT-LEG                     
068102     IF  IN-IDVAT-AGENT(2:16)   = SPACES                                  
068103        MOVE SPACES              TO W52273A-IDVAT-AGENT                   
068104     ELSE                                                                 
068105        MOVE IN-IDVAT-AGENT      TO W52273A-IDVAT-AGENT                   
068106     END-IF                                                               
068201     MOVE IN-DAFINDOC            TO W52273A-DAFINDOC                      
068301     MOVE IN-IDFINDOC            TO W52273A-IDFINDOC                      
068401     MOVE IN-IDSTATNR            TO W52273A-IDSTATNR                      
068501     MOVE IN-IDARTNR             TO WS-IDARTNR-UNSTR                      
068601     INSPECT WS-IDARTNR-UNSTR REPLACING LEADING SPACE BY ZERO             
068701     MOVE WS-IDARTNR-UNSTR       TO W52273A-IDARTNR                       
068801     MOVE IN-BEART               TO W52273A-BEART                         
068901     MOVE IN-VKORDNTO            TO W52273A-VKORDNTO                      
069001     MOVE IN-VKORDNTO-3DEC       TO W52273A-VKORDNTO-3DEC                 
069101     MOVE IN-KVLEVART            TO W52273A-KVLEVART                      
069201     MOVE IN-KDARTURS            TO W52273A-KDARTURS                      
069202       IF IN-KDFINDOC = 'INV2'                                            
069301         MOVE SPACE                  TO W52273A-BELEVVIL                  
069302       ELSE                                                               
069303         MOVE IN-BELEVVIL            TO W52273A-BELEVVIL                  
069304       END-IF                                                             
069401     MOVE IN-IDDISTR             TO W52273A-IDDISTR                       
069501     MOVE IN-IDKUNDNR            TO W52273A-IDKUNDNR                      
069601     IF IN-KDFINDOC = 'EXC' OR 'ECO' OR 'INL'                             
069701*    --- BERÄKNA BELOPP SEK -> LOKAL VALUTA                               
069801       COMPUTE WS-LOC ROUNDED = IN-SUNTO / CURR-PRKURS-NEW                
069901       MOVE WS-LOC               TO W52273A-SUNTO                         
070001       MOVE CURR-PRKURS-NEW      TO W52273A-PRKURS                        
070101       MOVE CURR-KDVALISO-ROW    TO W52273A-KDVALISO                      
070201     ELSE                                                                 
070301       IF IN-KDVALISO = CURR-KDVALISO-ROW                                 
070401*    --- INGEN OMRÄKNING (DEALER-NET/DDI)                                 
070501         MOVE IN-SUNTO           TO W52273A-SUNTO                         
070601         MOVE 1                  TO W52273A-PRKURS                        
070701       ELSE                                                               
070801         IF IN-KDVALISO NOT = 'SEK'                                       
070901*    --- BERÄKNA BELOPP LOKAL VALUTA -> SEK                               
071001           COMPUTE WS-SEK ROUNDED = IN-SUNTO * IN-PRKURS                  
071101*    --- BERÄKNA BELOPP SEK -> LOKAL VALUTA                               
071201           COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)            
071301           MOVE WS-LOC           TO W52273A-SUNTO                         
071401           MOVE CURR-PRKURS-NEW  TO W52273A-PRKURS                        
071501           MOVE CURR-KDVALISO-ROW TO W52273A-KDVALISO                     
071601         ELSE                                                             
071701*    --- BERÄKNA BELOPP I LOKAL VALUTA                                    
071801           COMPUTE WS-LOC ROUNDED = IN-SUNTO / CURR-PRKURS-NEW            
071901           MOVE WS-LOC           TO W52273A-SUNTO                         
072001           MOVE CURR-PRKURS-NEW  TO W52273A-PRKURS                        
072101         END-IF                                                           
072201       END-IF                                                             
072301     END-IF                                                               
072401                                                                          
072501     IF W52273A-KDINTTYP-OLD = +2                                         
072601       MOVE W52273A-SUNTO        TO WS-SUNTO                              
072701       COMPUTE WS-SUNTO = WS-SUNTO * -1                                   
072801       END-COMPUTE                                                        
072901       MOVE WS-SUNTO             TO W52273A-SUNTO                         
073001     END-IF                                                               
073101                                                                          
073201     IF W52273A-KDINTTYP-OLD = +4 OR +9                                   
073301       IF W52273A-IDLANDX3-SEND = W52273A-IDLANDX3-REC                    
073401         MOVE W52273A-SUNTO      TO WS-SUNTO                              
073501         COMPUTE WS-SUNTO = WS-SUNTO * -1                                 
073601         END-COMPUTE                                                      
073701         MOVE WS-SUNTO           TO W52273A-SUNTO                         
073801       END-IF                                                             
073901     END-IF                                                               
074001                                                                          
074101     IF IN-KDFRAKT = 41 OR 43 OR 44                                       
074201* BY SEA                                                                  
074301       MOVE 1                    TO W52273A-KDBEH                         
074401     ELSE                                                                 
074501       IF IN-KDFRAKT = 01 OR 03 OR 04 OR 08       OR                      
074601                       10 OR 11                   OR                      
074701                       20 OR 29                   OR                      
074801                       31 OR 32 OR 34 OR 35 OR 36 OR                      
074901                       40 OR 46                   OR                      
075001                       63 OR 64 OR 66 OR 67 OR 69 OR                      
075101                       70 OR 71                   OR                      
075201                       81                                                 
075301* BY ROAD                                                                 
075401         MOVE 3                  TO W52273A-KDBEH                         
075501       ELSE                                                               
075601         IF IN-KDFRAKT = 13 OR 14 OR 17 OR 18 OR 19 OR                    
075701                         61                                               
075801* BY AIR                                                                  
075901           MOVE 4                TO W52273A-KDBEH                         
076001         ELSE                                                             
076101* BY ROAD                                                                 
076201           MOVE 3                TO W52273A-KDBEH                         
076301         END-IF                                                           
076401       END-IF                                                             
076501     END-IF                                                               
076601     .                                                                    
076701     EJECT                                                                
076900                                                                          
077000 BA-CREATE-HEADER  SECTION.                                               
077100     WRITE W52273A-POST FROM HEAD-LINE                                    
077200     .                                                                    
077300     EJECT                                                                
077400                                                                          
077500 Z-FINISH SECTION.                                                        
077600     CLOSE W52271                                                         
077800           W52273A                                                        
077900     .                                                                    
078000     EJECT                                                                
078100                                                                          
078800                                                                          
078900 S11-READ-W52271  SECTION.                                                
079000     READ W52271 INTO W52271-AREA                                         
079100     AT END                                                               
079200        MOVE YES TO W52271-EOF-SW                                         
079300     END-READ                                                             
079400     .                                                                    
079500                                                                          
079600 S12-WRITE-W52273A SECTION.                                               
079700     WRITE W52273A-POST FROM W52273A-AREA                                 
079800     .                                                                    
079900     EJECT                                                                
080000                                                                          
