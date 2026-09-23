000100 ID  DIVISION.                                                            
000300 PROGRAM-ID.    W3301400.                                                 
000400 AUTHOR.        SUSANNE ENEGARD.                                          
000500 DATE-WRITTEN.  OKTOBER 1989.                                             
000510 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER EN FIL OCH SLÅR IHOP POSTERNA SAMT                         
001000*        OMVANDLAR TILL AF-OMFATTNINGAR                                   
001110*                                                                         
001200*    SUBPROGRAM:                                                          
001300*        WDATKONV - UTFÖR KONVERTERING AV DATUM AAVV TILL AARP            
001500*        W330KUND - HÄMTAR KONCERN (INTE NU LÄNGRE)                       
001510*        W510MARK - HÄMTAR MARKNAD OCH MARKNADSBENÄMNING                  
001520*        POSTSUM                                                          
001540*        FELLOG                                                           
001570*        DATKORT                                                          
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*    --- INFILER:                                                         
002500                                                                          
002600     SELECT W33013                       ASSIGN TO W33014D1.              
002700                                                                          
002800*    --- UTFIL:                                                           
002900*           --- AF-OMFATTADE POSTER:                                      
003000     SELECT W33015                       ASSIGN TO W33014D2.              
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500                                                                          
003600 FILE SECTION.                                                            
003700                                                                          
003800 FD  W33013                                                               
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      V                                                     
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300*01  -COPY W330310     -L.                                                
004500     SKIP2                                                                
004600*01  -COPY W330300     -L.                                                
004800     SKIP2                                                                
004900*01  -COPY W330320     -L.                                                
005100     SKIP2                                                                
005200*01  -COPY W330330     -L.                                                
005400     SKIP2                                                                
005500*01  -COPY W330340     -L.                                                
005700     SKIP2                                                                
005800*01  -COPY W330350     -L.                                                
006000     EJECT                                                                
006100 FD  W33015                                                               
006200     LABEL RECORD   STANDARD                                              
006300     RECORDING      F                                                     
006400     BLOCK CONTAINS 0.                                                    
006500                                                                          
006600*01  UT-POST  -COPY W33014       -L.                                      
006800     EJECT                                                                
007700 WORKING-STORAGE SECTION.                                                 
007701*    -COPY WY2000W9                                                       
007702     SKIP3                                                                
007703*    -COPY WY2000W7                                                       
007704     SKIP3                                                                
007710*    -COPY WY2000W3                                                       
007800     SKIP3                                                                
007900 77  IDPGM                       PIC X(8)    VALUE 'W3301400'.            
008000 77  JA                          PIC X(1)    VALUE 'J'.                   
008100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
008200 77  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
008310 77  SOEK-IX-MAX                 PIC S9(9)   VALUE +1  COMP-3.            
008400 77  ARB-IX                      PIC S9(9)   VALUE +1  COMP-3.            
008500 77  IX-PER                      PIC S9(9)   VALUE +1  COMP-3.            
008600 77  PER-IX                      PIC S9(9)   VALUE +1  COMP-3.            
008700 77  SPAR-PER-IX                 PIC S9(9)   VALUE +1  COMP-3.            
008800 77  NUV-AAR-IX                  PIC S9(9)   VALUE +1  COMP-3.            
008900 77  FG-AAR-IX                   PIC S9(9)   VALUE +1  COMP-3.            
009000 77  FORSTA-GANG                 PIC X(1)    VALUE 'J'.                   
009100 77  KUND-SAKNAS                 PIC X(1)    VALUE 'N'.                   
009110 77  SPARA-SPECPRIS              PIC X(1)    VALUE 'J'.                   
009200 77  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
009300 77  SPAR-PRARTSJK               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009400 77  TOT-RAKNARE-IN              PIC S9(9)   VALUE +0  COMP.              
009500 77  TOT-RAKNARE-UT              PIC S9(9)   VALUE +0  COMP.              
009700                                                                          
009710*                                                                         
009720 01  CDAT-TIAARP                 PIC 9(6).                                
009730*                                                                         
009800 01  SPAR-TIAARP                 PIC 9(6).                                
009900 01  SPAR-TIFSGVV                PIC 9(6).                                
010000 01  FILLER     REDEFINES SPAR-TIFSGVV.                                   
010200     03  SPAR-AAVV               PIC 9(6).                                
010300     03  FILLER REDEFINES SPAR-AAVV.                                      
010400         05 SPAR-AA              PIC 9(4).                                
010500         05 SPAR-VV              PIC 9(2).                                
010600                                                                          
010700 01  OMVP-TIAAVV                 PIC 9(6).                                
010800 01  FILLER     REDEFINES OMVP-TIAAVV.                                    
011000     03  OMVP-AAVV               PIC 9(6).                                
011100     03  FILLER REDEFINES OMVP-AAVV.                                      
011200         05 OMVP-AA              PIC 9(4).                                
011300         05 OMVP-VV              PIC 9(2).                                
011400                                                                          
011500 01  OMV-AARP                    PIC 9(6).                                
011600 01  FILLER     REDEFINES OMV-AARP.                                       
011700     03  OMV-AA                  PIC 9(4).                                
011800     03  OMV-RP                  PIC 9(2).                                
011900                                                                          
012000 01  DAGENS-AARP                 PIC 9(6).                                
012100 01  FILLER     REDEFINES DAGENS-AARP.                                    
012200     03  DAGENS-AA               PIC 9(4).                                
012300     03  DAGENS-RP               PIC 9(2).                                
012400                                                                          
012500 01  DAGENS-AAVV.                                                         
012600     03  DAGENS-AA-VV            PIC 9(4).                                
012700     03  DAGENS-VV               PIC 9(2).                                
012800                                                                          
012900 01  DAGENS-DATUM                PIC 9(8).                                
013000 01  FILLER     REDEFINES DAGENS-DATUM.                                   
013100     03  DAGENS-AAR              PIC 9(4).                                
013200     03  DAGENS-MAANAD           PIC 9(2).                                
013300     03  DAGENS-DAG              PIC 9(2).                                
013400     EJECT                                                                
013500 01  DYNAMISKA-SUBPROGRAM.                                                
013600*                                                                         
013700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
014000     03  W330KUND                PIC X(8)    VALUE 'W330KUND'.            
014010     03  W510MARK                PIC X(8)    VALUE 'W510MARK'.            
014100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
014200     SKIP2                                                                
014300*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
014400*                                                                         
014500 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
014600*01  -COPY W0005      -PRE  POSTSUM-                                      
014800     EJECT                                                                
014900*- - - - - - - - - - - - - - - - WDATKONV                                 
015000*                                                                         
015100 01  FILLER                       PIC X(16)  VALUE 'WDATKONV'.            
015200*01  -COPY WDATAREA.                                                      
015400     EJECT                                                                
015500*- - - - - - - - - - - - - - - - DATKORT                                  
015600*                                                                         
015700 01  FILLER                       PIC X(16)  VALUE 'DATKORT'.             
015800 01  DATKORT-PROGNAMN             PIC X(6)   VALUE SPACE.                 
015900 01  DATKORT-ID                   PIC X(6)   VALUE 'WDATUM'.              
016000*01  -COPY WDATKORT.                                                      
016200     EJECT                                                                
016300*- - - - - - - - - - - - - - - - PERIOD-AREA                              
016400*                                                                         
016500 01  FILLER                       PIC X(16) VALUE 'PERIOD-AREA'.          
016600 01  TAB-AREA.                                                            
016700     03 TAB-RAD OCCURS 105  INDEXED BY TAB-IX, SPAR-TAB-IX.               
016800        05  TAB-TIAAVV            PIC S9(7)  COMP-3.                      
016900        05  TAB-PERIOD            PIC S9(9)  COMP.                        
017000     EJECT                                                                
017010 01  W-WEEK                       PIC S9(7)  COMP-3.                      
017020 01  TUB-IX                       PIC S9(7)        .                      
017030 01  SPAR-TUB-IX                  PIC S9(7)        .                      
017100*- - - - - - - - - - - - - - - - NOLL-AREA                                
017200*                                                                         
017300 01  FILLER                       PIC X(16) VALUE 'NOLL-AREA'.            
017400*01  AREA  -PRE NOLL-  -COPY W330140.                                     
017600     EJECT                                                                
017700*- - - - - - - - - - - - - - - - NOLL-KLAR-AREA                           
017800*                                                                         
017900 01  FILLER                       PIC X(16) VALUE 'NOLK-AREA'.            
018000*01  AREA  -PRE NOLK-  -COPY W33014.                                      
018200     EJECT                                                                
018300*- - - - - - - - - - - - - - - - SOEK-AREA                                
018400*                                                                         
018500 01  FILLER                       PIC X(16) VALUE 'SOEK-AREA'.            
018600 01  SOEK-AREA.                                                           
018700     03 SOEK-RAD OCCURS 19999.                                            
018800        05  SOEK-INDEX            PIC S9(9)  COMP.                        
018900     EJECT                                                                
019000*- - - - - - - - - - - - - - - - KLAR-AREA                                
019100*                                                                         
019200 01  FILLER                       PIC X(16)  VALUE 'KLARAREA'.            
019300*01  AREA  -PRE KLAR-  -COPY W33014.                                      
019500     EJECT                                                                
019600*- - - - - - - - - - - - - - - - KUND-AREA                                
019700*                                                                         
019800 01  FILLER                       PIC X(16)  VALUE 'KUNDAREA'.            
019900*01  KUND-AREA        -COPY W330KUND.                                     
020100     EJECT                                                                
020110*- - - - - - - - - - - - - - - - MARK-AREA                                
020120*                                                                         
020130 01  FILLER                       PIC X(16)  VALUE 'MARKAREA'.            
020140*01  MARK-AREA        -COPY W510MARK.                                     
020160     EJECT                                                                
020200*- - - - - - - - - - - - - - - - ARBETS-AREA                              
020300*                                                                         
020400 01  FILLER                       PIC X(16)  VALUE 'ARBA-AREA'.           
020500*01  AREA  -PRE ARB-  -COPY W33014W.                                      
020700     EJECT                                                                
020800*- - - - - - - - - - - - - - - - IN-AREA                                  
020900*                                                                         
021000 01  FILLER                       PIC X(16)  VALUE 'INAREA'.              
021100 01  IN-AREA                      PIC X(28).                              
021200*01  AREA  -PRE IN310- -COPY W330310     -RED IN-AREA.                    
021400     SKIP2                                                                
021500*01  AREA  -PRE IN300- -COPY W330300     -RED IN-AREA.                    
021700     SKIP2                                                                
021800*01  AREA  -PRE IN320- -COPY W330320     -RED IN-AREA.                    
022000     SKIP2                                                                
022100*01  AREA  -PRE IN330- -COPY W330330     -RED IN-AREA.                    
022300     SKIP2                                                                
022400*01  AREA  -PRE IN340- -COPY W330340     -RED IN-AREA.                    
022600     SKIP2                                                                
022700*01  AREA  -PRE IN350- -COPY W330350     -RED IN-AREA.                    
022900     EJECT                                                                
023600 LINKAGE SECTION.                                                         
023700*01  -COPY W0008       -PRE GMTA-.                                        
023900     05  FILLER             PIC X.                                        
023910*01  -COPY W0008       -PRE BETA-.                                        
023920     05  FILLER             PIC X.                                        
024000     EJECT                                                                
024100 PROCEDURE DIVISION USING GMTA-PCB BETA-PCB.                              
024110 STYR SECTION.                                                            
024200     ENTRY 'DLITCBL' USING GMTA-PCB BETA-PCB.                             
024300                                                                          
024500     PERFORM A-INIT                                                       
024600     PERFORM S01-LAS-INFIL                                                
024700     PERFORM UNTIL INFIL-EOF = JA                                         
024800        EVALUATE IN300-IDPTYP                                             
024900          WHEN '310' PERFORM C-BEHANDLA-310-POST                          
025000          WHEN '300' PERFORM B-BEHANDLA-300-POST                          
025100          WHEN '320' PERFORM D-BEHANDLA-320-POST                          
025200          WHEN '330' PERFORM E-BEHANDLA-330-POST                          
025300          WHEN '340' PERFORM F-BEHANDLA-340-POST                          
025400          WHEN '350' PERFORM G-BEHANDLA-350-POST                          
025500        END-EVALUATE                                                      
025600        PERFORM S01-LAS-INFIL                                             
025700     END-PERFORM                                                          
025800     PERFORM S02-BEARBETA-SKRIV-POST                                      
025900     PERFORM Z-FINIT                                                      
026000     MOVE ZERO TO RETURN-CODE                                             
026100     GOBACK                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 A-INIT SECTION.                                                          
026500                                                                          
026600     OPEN INPUT W33013                                                    
026700     OPEN OUTPUT W33015                                                   
026900                                                                          
027000     MOVE IDPGM TO DATKORT-PROGNAMN                                       
027100     CALL DATKORT USING DATKORT-PROGNAMN DATKORT-ID DATUMKORT             
027200     MOVE D-AAR    TO DAGENS-AAR                                          
027201                      DAGENS-AA-VV                                        
027202*                                                                         
027210          IF DAGENS-AAR < 50                                              
027220             ADD 2000 TO DAGENS-AAR                                       
027230          ELSE                                                            
027240             ADD 1900 TO DAGENS-AAR                                       
027250          END-IF                                                          
027260*                                                                         
027270          IF DAGENS-AA-VV < 50                                            
027280             ADD 2000 TO DAGENS-AA-VV                                     
027290          ELSE                                                            
027300             ADD 1900 TO DAGENS-AA-VV                                     
027310          END-IF                                                          
027320*                                                                         
027400     MOVE D-MAANAD TO DAGENS-MAANAD                                       
027500     MOVE D-DAG    TO DAGENS-DAG                                          
027600     MOVE D-VECKA  TO DAGENS-VV                                           
027700     MOVE JA TO FORSTA-GANG                                               
027800     MOVE NEJ TO KUND-SAKNAS                                              
027900     MOVE +0 TO TOT-RAKNARE-IN                                            
028000     MOVE +0 TO TOT-RAKNARE-UT                                            
028200     INITIALIZE ARB-AREA                                                  
028300     MOVE LOW-VALUE TO SOEK-AREA                                          
028400     INITIALIZE NOLL-AREA                                                 
028500     INITIALIZE NOLK-AREA                                                 
028600     MOVE +1 TO SOEK-IX-MAX                                               
028700     SET SPAR-TAB-IX TO 1                                                 
028800     PERFORM AA-OMV-DAGENS-DATUM-TILL-AARP                                
028900     PERFORM AB-HAMTA-KUND                                                
029000     PERFORM AC-HAMTA-PERIOD-INDELNING                                    
029100     .                                                                    
029200     EJECT                                                                
029300 AA-OMV-DAGENS-DATUM-TILL-AARP SECTION.                                   
029400                                                                          
029410     DISPLAY 'DAGENS-DATUM=' DAGENS-DATUM                                 
029500     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
029600     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
029700                                                                          
029800     CALL WDATKONV USING DAT-KDDATFORM                                    
029900                         DAT-I-TIDATUM                                    
030000                         DAT-O-TIDATUM                                    
030100                         DAT-KDSVAR                                       
030200                                                                          
030300     MOVE DAT-TIAARP TO DAGENS-AARP                                       
030301     DISPLAY 'DAGENS-AARP=' DAGENS-AARP                                   
030310       IF DAGENS-AA < 50                                                  
030320         ADD 2000 TO DAGENS-AA                                            
030330       ELSE                                                               
030340         ADD 1900 TO DAGENS-AA                                            
030350       END-IF                                                             
030351     DISPLAY 'DAGENS-AA=' DAGENS-AARP                                     
030360*                                                                         
030400     COMPUTE NUV-AAR-IX = 25 - DAGENS-RP                                  
030500     COMPUTE FG-AAR-IX  = 13 - DAGENS-RP                                  
030600     .                                                                    
030700     EJECT                                                                
030800 AB-HAMTA-KUND SECTION.                                                   
030900                                                                          
031000     CALL W330KUND USING KUND-AREA GMTA-PCB BETA-PCB                      
031001                                                                          
031002****    FYLL KUNDTABELL MED MARKNADSKOD OCH MARKNADSBENÄMNING             
031003                                                                          
031006     MOVE D-AAR                   TO MARK-TIAA                            
031007     MOVE +0                      TO MARK-KDCALL                          
031008     SET KUND-IX TO +1                                                    
031009     PERFORM 1600 TIMES                                                   
031020       MOVE KUND-IDDISTR(KUND-IX) TO MARK-IDDISTR                         
031040       CALL W510MARK USING MARK-AREA                                      
031072       MOVE MARK-KDMARK-BUDG      TO KUND-KDMARK-BUDG(KUND-IX)            
031073       MOVE MARK-BEMARKN          TO KUND-BEMARK-BUDG(KUND-IX)            
031077       SET KUND-IX UP BY +1                                               
031080     END-PERFORM                                                          
031100     .                                                                    
031200     EJECT                                                                
031300 AC-HAMTA-PERIOD-INDELNING SECTION.                                       
031400                                                                          
031500     SET TAB-IX TO 1                                                      
031600     MOVE +1 TO SPAR-PER-IX                                               
031700     PERFORM ACA-HAMTA-FIRST-AAVV                                         
031800     ADD +1 TO OMVP-VV                                                    
031900     PERFORM UNTIL OMVP-AAVV > DAGENS-AAVV                                
032000        IF OMVP-VV > 53                                                   
032100           ADD +1 TO OMVP-AA                                              
032200           MOVE +1 TO OMVP-VV                                             
032300        END-IF                                                            
032400        PERFORM ACB-HAMTA-NEXT-CALL                                       
032500                                                                          
032600        IF DAT-KDSVAR = ' '                                               
032601*                                                                         
032610           MOVE DAT-TIAARP TO CDAT-TIAARP                                 
032620              IF CDAT-TIAARP < 5000                                       
032630                ADD 200000 TO CDAT-TIAARP                                 
032640              ELSE                                                        
032650                ADD 190000 TO CDAT-TIAARP                                 
032660              END-IF                                                      
032670*                                                                         
032700           IF CDAT-TIAARP > SPAR-TIAARP                                   
032800              ADD +1 TO SPAR-PER-IX                                       
032900              MOVE CDAT-TIAARP TO SPAR-TIAARP                             
033000           END-IF                                                         
033100           MOVE OMVP-AAVV      TO TAB-TIAAVV(TAB-IX)                      
033200           MOVE SPAR-PER-IX    TO TAB-PERIOD(TAB-IX)                      
033210******FIX FOR PERIOD LENGTHS FOR OSTEN************ 20040409               
033211******FIX/EÖ FOR PERIOD LENGTHS FOR OSTEN************ 20041222            
033220           IF OMVP-AAVV = 200405 OR 200409 OR 200418 OR 200422 OR         
033230                          200431 OR 200444                                
033240                  ADD +1 TO TAB-PERIOD(TAB-IX) GIVING                     
033250                            TAB-PERIOD(TAB-IX)                            
033251           END-IF                                                         
033252*GS        IF OMVP-AAVV = 200305 OR 200309 OR 200318 OR 200322 OR         
033253*                         200331 OR 200344                                
033254*            SUBTRACT +1 FROM TAB-PERIOD(TAB-IX) GIVING                   
033255*                             TAB-PERIOD(TAB-IX)                          
033256*GS        END-IF                                                         
033257           DISPLAY 'TAB-PERIOD ' TAB-PERIOD(TAB-IX)                       
033258           DISPLAY 'TAB-TIAAVV ' TAB-TIAAVV(TAB-IX)                       
033260**********************************************************                
033300           SET TAB-IX UP BY 1                                             
033400        END-IF                                                            
033500        ADD +1 TO OMVP-VV                                                 
033600     END-PERFORM                                                          
033700                                                                          
033800     PERFORM UNTIL TAB-IX > 105                                           
033900        MOVE +999999 TO TAB-TIAAVV(TAB-IX)                                
034000        SET TAB-IX UP BY 1                                                
034100     END-PERFORM                                                          
034110********FIX FÖR ATT AF1,OCH AF3 PERIODER SKA FÅ SAMMA ANTAL VECKOR        
034120********INNEHÅLL SOM MOTSVARANDE AF2 OCH AF4.                             
034121*    MOVE 1 TO TUB-IX                                                     
034122*    PERFORM UNTIL TUB-IX > 53                                            
034123*      COMPUTE W-WEEK = 100 + TAB-TIAAVV(TUB-IX)                          
034124*      MOVE 53 TO  SPAR-TUB-IX                                            
034125*      PERFORM UNTIL SPAR-TUB-IX > 105                                    
034128*       IF TAB-TIAAVV(SPAR-TUB-IX) = W-WEEK                               
034129*       DISPLAY 'WEEK=' W-WEEK ' TAB-PER=' TAB-PERIOD(TUB-IX) 'BE'        
034130*        COMPUTE TAB-PERIOD(TUB-IX) = TAB-PERIOD(SPAR-TUB-IX) - 12        
034131******TEST AV FIX FÖR RÄTTNING GS***********                              
034132*        IF      TAB-PERIOD(TUB-IX) = 0                                   
034133*          MOVE  1 TO TAB-PERIOD(TUB-IX)                                  
034134*        END-IF                                                           
034135**END*TEST AV FIX FÖR RÄTTNING GS***********                              
034136*       DISPLAY 'WEEK=' W-WEEK ' TAB-PER=' TAB-PERIOD(TUB-IX) 'AF'        
034137*        MOVE 105 TO SPAR-TUB-IX                                          
034138*       END-IF                                                            
034139*       ADD 1 TO SPAR-TUB-IX                                              
034140*      END-PERFORM                                                        
034141*      ADD 1 TO  TUB-IX                                                   
034142*    END-PERFORM                                                          
034150*                                                                         
034151********END OF FIX                                                        
034160********END OF FIX                                                        
034200     .                                                                    
034300     EJECT                                                                
034400 ACA-HAMTA-FIRST-AAVV SECTION.                                            
034500                                                                          
034600     MOVE DAGENS-AARP   TO OMV-AARP                                       
034610*                                                                         
034620*    IF OMV-AA  < 50                                                      
034630*       ADD  2000 TO OMV-AA                                               
034640*    ELSE                                                                 
034642*       ADD  1900 TO OMV-AA                                               
034643*    END-IF                                                               
034644*                                                                         
034650                                                                          
034700     SUBTRACT 2 FROM OMV-AA                                               
034800     ADD +1 TO OMV-RP                                                     
034900     IF OMV-RP = 13                                                       
035000        MOVE 1 TO OMV-RP                                                  
035100        ADD +1 TO OMV-AA                                                  
035200     END-IF                                                               
035300                                                                          
035400     MOVE OMV-AARP      TO DAT-I-TIDATUM                                  
035500     MOVE 'AARP'        TO DAT-KDDATFORM                                  
035600                                                                          
035700      CALL WDATKONV USING DAT-KDDATFORM                                   
035800                          DAT-I-TIDATUM                                   
035900                          DAT-O-TIDATUM                                   
036000                          DAT-KDSVAR                                      
036100                                                                          
036200     MOVE DAT-TIAA-VECKA  TO OMVP-AA                                      
036300     MOVE DAT-TIVV        TO OMVP-VV                                      
036310*                                                                         
036320     IF OMVP-AA < 50                                                      
036330        ADD  2000 TO OMVP-AA                                              
036340     ELSE                                                                 
036350        ADD  1900 TO OMVP-AA                                              
036360     END-IF                                                               
036370*                                                                         
036400******** FIX FIX FIX 910119 PF/SE FÖR ATT KLARA ÅRSSKIFTE                 
036500     IF OMVP-AA = 1998 AND OMVP-VV > 53                                   
036600        MOVE 1999             TO OMVP-AA                                  
036700        MOVE 01             TO OMVP-VV                                    
036800     END-IF                                                               
036810     IF OMVP-AA = 2003 AND OMVP-VV > 52                                   
036820        MOVE 2004             TO OMVP-AA                                  
036830        MOVE 01             TO OMVP-VV                                    
036840     END-IF                                                               
036841**EÖ-IFX                                                                  
036850     IF OMVP-AA = 2004 AND OMVP-VV > 52                                   
036860        MOVE 2005             TO OMVP-AA                                  
036870        MOVE 01             TO OMVP-VV                                    
036880     END-IF                                                               
036890**SLUT EÖ-FIX                                                             
036900******** FIX FIX FIX 910119 PF/SE FÖR ATT KLARA ÅRSSKIFTE                 
037000     MOVE OMVP-TIAAVV     TO TAB-TIAAVV(TAB-IX)                           
037100     MOVE SPAR-PER-IX   TO TAB-PERIOD(TAB-IX)                             
037101*                                                                         
037110           MOVE DAT-TIAARP TO CDAT-TIAARP                                 
037120              IF CDAT-TIAARP < 5000                                       
037130                ADD 200000 TO CDAT-TIAARP                                 
037140              ELSE                                                        
037150                ADD 190000 TO CDAT-TIAARP                                 
037160              END-IF                                                      
037170*                                                                         
037200     MOVE CDAT-TIAARP    TO SPAR-TIAARP                                   
037300     SET TAB-IX UP BY 1                                                   
037400     .                                                                    
037500     EJECT                                                                
037600 ACB-HAMTA-NEXT-CALL SECTION.                                             
037700                                                                          
037800     MOVE OMVP-AAVV  TO DAT-I-TIDATUM                                     
037900     MOVE 'AAVV'     TO DAT-KDDATFORM                                     
038000                                                                          
038100     CALL WDATKONV USING DAT-KDDATFORM                                    
038200                         DAT-I-TIDATUM                                    
038300                         DAT-O-TIDATUM                                    
038400                         DAT-KDSVAR                                       
038500     .                                                                    
038600     EJECT                                                                
038700 B-BEHANDLA-300-POST  SECTION.                                            
038800                                                                          
038900     IF KUND-SAKNAS = JA                                                  
039000        MOVE NEJ TO KUND-SAKNAS                                           
039100     END-IF                                                               
039200                                                                          
039300     IF FORSTA-GANG = JA                                                  
039400        MOVE IN300-IDARTNR TO ARB-IDARTNR                                 
039500        MOVE IN300-IDARTNR TO SPAR-IDARTNR                                
039600        MOVE NEJ TO FORSTA-GANG                                           
039700     ELSE                                                                 
039800        IF IN300-IDARTNR = SPAR-IDARTNR                                   
039900           CONTINUE                                                       
040000        ELSE                                                              
040100           PERFORM S02-BEARBETA-SKRIV-POST                                
040200           MOVE IN300-IDARTNR TO ARB-IDARTNR                              
040300           MOVE IN300-IDARTNR TO SPAR-IDARTNR                             
040400           SET SPAR-TAB-IX TO 1                                           
040500        END-IF                                                            
040600     END-IF                                                               
040700     MOVE IN300-PRARTSJK TO SPAR-PRARTSJK                                 
040710     MOVE IN300-DAFSGVV TO SPAR-TIFSGVV                                   
040800*-----------FIX FÖR ATT DÅLIGT DATA EJ SKALL LADDAS I BASEN               
040810     IF SPAR-TIFSGVV < +9122                                              
040820       MOVE NEJ TO SPARA-SPECPRIS                                         
040821     ELSE                                                                 
040822       MOVE JA TO SPARA-SPECPRIS                                          
040830     END-IF                                                               
040840*-----------FIX FÖR ATT DÅLIGT DATA EJ SKALL LADDAS I BASEN               
040900     PERFORM BA-SOEK-IX-PER                                               
041000     .                                                                    
041100     EJECT                                                                
041200 BA-SOEK-IX-PER SECTION.                                                  
041300                                                                          
041400     SET TAB-IX TO SPAR-TAB-IX                                            
041520                                                                          
041600     PERFORM UNTIL                                                        
041700          TAB-IX > 105                                                    
041710              OR   TAB-TIAAVV(TAB-IX) = SPAR-TIFSGVV                      
041800        SET TAB-IX UP BY 1                                                
041900     END-PERFORM                                                          
042000                                                                          
042100     IF TAB-IX > 105                                                      
042200     DISPLAY 'FELAKTIG VECKA = ' SPAR-TIFSGVV                             
042300        CALL FELLOG                                                       
042400     END-IF                                                               
042500                                                                          
042600     MOVE TAB-PERIOD(TAB-IX) TO IX-PER                                    
042700     SET SPAR-TAB-IX TO TAB-IX                                            
042800     .                                                                    
042900     EJECT                                                                
043000 C-BEHANDLA-310-POST  SECTION.                                            
043100                                                                          
043200     IF KUND-SAKNAS = JA                                                  
043300        MOVE NEJ TO KUND-SAKNAS                                           
043400     END-IF                                                               
043500                                                                          
043600     IF IN310-IDDISTR = ZERO                                              
043700        MOVE JA TO KUND-SAKNAS                                            
043800        DISPLAY 'DISTRIKT = 0 I 310-POST '                                
043900                'ARTIKELNR : ' SPAR-IDARTNR                               
044000                '  VECKA : ' SPAR-TIFSGVV                                 
044100                '  DISTRIKT : ' IN310-IDDISTR                             
044200                                                                          
044300     ELSE                                                                 
044400       PERFORM CA-SOEK-DISTRIKT-IX                                        
044500                                                                          
044600       IF KUND-SAKNAS = NEJ                                               
044700          ADD IN310-SULEVANT TO ARB-SULEVANT(ARB-IX, IX-PER)              
044800          ADD IN310-SUARTFSG TO ARB-SUARTFSG(ARB-IX, IX-PER)              
044900          COMPUTE ARB-SUARTSJK(ARB-IX, IX-PER) =                          
045000             ARB-SUARTSJK(ARB-IX, IX-PER) +                               
045100             (IN310-SULEVANT * SPAR-PRARTSJK)                             
045111          ADD IN310-SULEVANT-DO                                           
045112                     TO ARB-SULEVANT-DO(ARB-IX, IX-PER)                   
045120          ADD IN310-SUARTFSG-DO                                           
045130                     TO ARB-SUARTFSG-DO(ARB-IX, IX-PER)                   
045200       END-IF                                                             
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 CA-SOEK-DISTRIKT-IX SECTION.                                             
045700                                                                          
045800     IF SOEK-INDEX(IN310-IDDISTR) = ZERO                                  
045900        MOVE SOEK-IX-MAX TO SOEK-INDEX(IN310-IDDISTR)                     
046000        ADD +1 TO SOEK-IX-MAX                                             
046100        MOVE SOEK-INDEX(IN310-IDDISTR) TO ARB-IX                          
046200        MOVE IN310-IDDISTR TO ARB-IDDISTR(ARB-IX)                         
046300        PERFORM CAA-LAS-MARKNAD                                           
046400     ELSE                                                                 
046500        MOVE SOEK-INDEX(IN310-IDDISTR) TO ARB-IX                          
046600        MOVE IN310-IDDISTR TO ARB-IDDISTR(ARB-IX)                         
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 CAA-LAS-MARKNAD SECTION.                                                 
047100                                                                          
047200     SEARCH ALL KUND-RAD                                                  
047300       AT END                                                             
047400*** FIX FÖR ATT KLARA RENSNING AV KUNDBASEN                               
047500******************************************************************        
047600        MOVE D-AAR               TO MARK-TIAA                             
047700        MOVE IN310-IDDISTR       TO MARK-IDDISTR                          
047800        MOVE +0                  TO MARK-KDCALL                           
047810        CALL W510MARK USING MARK-AREA                                     
047900        MOVE +0                  TO ARB-IDKONCNR(ARB-IX)                  
047910        MOVE MARK-KDMARK-BUDG    TO ARB-KDMARK-BUDG(ARB-IX)               
047920        MOVE MARK-BEMARKN        TO ARB-BEMARK-BUDG(ARB-IX)               
047930******************************************************************        
048300       WHEN KUND-IDDISTR(KUND-IX) = IN310-IDDISTR                         
048310        MOVE +0                  TO ARB-IDKONCNR(ARB-IX)                  
048320******************************************************************        
048400***     MOVE KUND-IDKONCNR   (KUND-IX) TO ARB-IDKONCNR   (ARB-IX)         
048500******************************************************************        
048600        MOVE KUND-KDMARK-BUDG(KUND-IX) TO ARB-KDMARK-BUDG(ARB-IX)         
048800        MOVE KUND-BEMARK-BUDG(KUND-IX) TO ARB-BEMARK-BUDG(ARB-IX)         
048900     END-SEARCH                                                           
049000     .                                                                    
049100     EJECT                                                                
049200 D-BEHANDLA-320-POST  SECTION.                                            
049300                                                                          
049310*FIX >>> AND SPARA-SPECPRIS = JA SE FIX I B SECTION                       
049400     IF KUND-SAKNAS = NEJ                                                 
049420     AND SPARA-SPECPRIS = JA                                              
049500        ADD IN320-SULEVANT-RAB TO                                         
049600            ARB-SULEVANT-RAB (ARB-IX, IX-PER)                             
049700        ADD IN320-SUARTFSG-RAB TO                                         
049800            ARB-SUARTFSG-RAB (ARB-IX, IX-PER)                             
049900     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050200 E-BEHANDLA-330-POST  SECTION.                                            
050300                                                                          
050310*FIX >>> AND SPARA-SPECPRIS = JA SE FIX I B SECTION                       
050400     IF KUND-SAKNAS = NEJ                                                 
050410     AND SPARA-SPECPRIS = JA                                              
050500        ADD IN330-SULEVANT-SPEC TO                                        
050600            ARB-SULEVANT-SPEC (ARB-IX, IX-PER)                            
050700        ADD IN330-SUARTFSG-SPEC TO                                        
050800            ARB-SUARTFSG-SPEC (ARB-IX, IX-PER)                            
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 F-BEHANDLA-340-POST  SECTION.                                            
051300                                                                          
051310*FIX >>> AND SPARA-SPECPRIS = JA SE FIX I B SECTION                       
051400     IF KUND-SAKNAS = NEJ                                                 
051410     AND SPARA-SPECPRIS = JA                                              
051500        ADD IN340-SULEVANT-MAN TO                                         
051600            ARB-SULEVANT-MAN (ARB-IX, IX-PER)                             
051700        ADD IN340-SUARTFSG-MAN TO                                         
051800            ARB-SUARTFSG-MAN (ARB-IX, IX-PER)                             
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 G-BEHANDLA-350-POST  SECTION.                                            
052300                                                                          
052400     IF KUND-SAKNAS = NEJ                                                 
052500        ADD IN350-SULEVANT-KRE TO                                         
052600            ARB-SULEVANT-KRE (ARB-IX, IX-PER)                             
052700        ADD IN350-SUARTFSG-KRE TO                                         
052800            ARB-SUARTFSG-KRE (ARB-IX, IX-PER)                             
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 S01-LAS-INFIL SECTION.                                                   
053300                                                                          
053400     READ W33013 INTO IN-AREA                                             
053500     AT END                                                               
053600       MOVE JA TO INFIL-EOF                                               
053700     END-READ                                                             
053800                                                                          
053900     IF INFIL-EOF = NEJ                                                   
054000       ADD +1 TO TOT-RAKNARE-IN                                           
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400 S02-BEARBETA-SKRIV-POST SECTION.                                         
054500                                                                          
054600     MOVE +1 TO ARB-IX                                                    
054700     PERFORM UNTIL ARB-IX > SOEK-IX-MAX                                   
054800        IF ARB-IDDISTR(ARB-IX) > ZERO                                     
054900           MOVE NOLK-AREA TO KLAR-AREA                                    
055000           MOVE ARB-IDARTNR TO KLAR-IDARTNR                               
055100           MOVE ARB-IDDISTR(ARB-IX) TO KLAR-IDDISTR                       
055110******************************************************************        
055200***        MOVE ARB-IDKONCNR(ARB-IX) TO KLAR-IDKONCNR                     
055201******************************************************************        
055210           MOVE +0                  TO KLAR-IDKONCNR                      
055300           MOVE ARB-KDMARK-BUDG(ARB-IX) TO KLAR-KDMARK-BUDG               
055400           MOVE ARB-BEMARK-BUDG(ARB-IX) TO KLAR-BEMARK-BUDG               
055500           PERFORM S02A-OMVANDLA-AF-OMF                                   
055600           PERFORM S02B-WRITE-UTPOST                                      
055700           MOVE NOLL-PERIOD-GRUPP TO ARB-PERIOD-GRUPP(ARB-IX)             
055800           MOVE ZERO TO ARB-IDDISTR(ARB-IX)                               
055900        END-IF                                                            
056000        ADD +1 TO ARB-IX                                                  
056100     END-PERFORM                                                          
056200     .                                                                    
056300     EJECT                                                                
056400 S02A-OMVANDLA-AF-OMF SECTION.                                            
056500                                                                          
056600     MOVE +1 TO PER-IX                                                    
056700     PERFORM UNTIL PER-IX > 24                                            
056800                                                                          
056900*AF5 - DENNA PERIOD                                                       
057000        IF PER-IX = 24                                                    
057100           ADD ARB-SULEVANT(ARB-IX, PER-IX) TO                            
057200               KLAR-SULEVANT-PER                                          
057300           ADD ARB-SUARTFSG(ARB-IX, PER-IX) TO                            
057400               KLAR-SUARTFSG-PER                                          
057500           ADD ARB-SUARTSJK(ARB-IX, PER-IX) TO                            
057600               KLAR-SUARTSJK-PER                                          
057700        END-IF                                                            
057800                                                                          
057900*AF4 - NUVARANDE ÅR                                                       
058000        IF PER-IX  >= NUV-AAR-IX                                          
058100           ADD ARB-SULEVANT(ARB-IX, PER-IX) TO                            
058200               KLAR-SULEVANT-AAR                                          
058300           ADD ARB-SUARTFSG(ARB-IX, PER-IX) TO                            
058400               KLAR-SUARTFSG-AAR                                          
058500           ADD ARB-SUARTSJK(ARB-IX, PER-IX) TO                            
058600               KLAR-SUARTSJK-AAR                                          
058700        END-IF                                                            
058800                                                                          
058900*AF3 - FÖREGÅENDE ÅR                                                      
058901        MOVE PER-IX      TO TMP1-YY                                       
058902        MOVE FG-AAR-IX   TO TMP2-YY                                       
058910        PERFORM WY2000P9                                                  
059000        IF PER-IX  >= FG-AAR-IX AND NOT > 12                              
059100           ADD ARB-SULEVANT(ARB-IX, PER-IX) TO                            
059200               KLAR-SULEVANT-FAAR                                         
059300           ADD ARB-SUARTFSG(ARB-IX, PER-IX) TO                            
059400               KLAR-SUARTFSG-FAAR                                         
059500           ADD ARB-SUARTSJK(ARB-IX, PER-IX) TO                            
059600               KLAR-SUARTSJK-FAAR                                         
059700        END-IF                                                            
059800                                                                          
059900*AF2 - RULLANDE ÅR                                                        
059910*HÄR ÄR TILLLAGT SULEVANT-DO                                              
059920*                SUARTFSG-DO                                              
059930*FÖR ATT GE STATISTIK TILL MARKNADSBOLAGEN (IDMARKBO)                     
059940*DESSA FÄLT KOMMER TILL ANVÄNDNING I PROGRAM W33035 MFL W330R1            
060000        IF PER-IX > 12                                                    
060100           ADD ARB-SULEVANT(ARB-IX, PER-IX) TO                            
060200               KLAR-SULEVANT-RAAR                                         
060300           ADD ARB-SUARTFSG(ARB-IX, PER-IX) TO                            
060400               KLAR-SUARTFSG-RAAR                                         
060410*                                                                         
060422        ADD ARB-SULEVANT-DO(ARB-IX, PER-IX) TO                            
060423               KLAR-SULEVANT-DO-RAAR                                      
060440        ADD ARB-SUARTFSG-DO(ARB-IX, PER-IX) TO                            
060450               KLAR-SUARTFSG-DO-RAAR                                      
060460*                                                                         
060500           ADD ARB-SUARTSJK(ARB-IX, PER-IX) TO                            
060600               KLAR-SUARTSJK-RAAR                                         
060700           ADD ARB-SULEVANT-SPEC(ARB-IX, PER-IX) TO                       
060800               KLAR-SULEVANT-RAAR-SPEC                                    
060900           ADD ARB-SUARTFSG-SPEC(ARB-IX, PER-IX) TO                       
061000               KLAR-SUARTFSG-RAAR-SPEC                                    
061100           ADD ARB-SULEVANT-RAB(ARB-IX, PER-IX) TO                        
061200               KLAR-SULEVANT-RAAR-RAB                                     
061300           ADD ARB-SUARTFSG-RAB(ARB-IX, PER-IX) TO                        
061400               KLAR-SUARTFSG-RAAR-RAB                                     
061500           ADD ARB-SULEVANT-MAN(ARB-IX, PER-IX) TO                        
061600               KLAR-SULEVANT-RAAR-MAN                                     
061700           ADD ARB-SUARTFSG-MAN(ARB-IX, PER-IX) TO                        
061800               KLAR-SUARTFSG-RAAR-MAN                                     
061900           ADD ARB-SULEVANT-KRE(ARB-IX, PER-IX) TO                        
062000               KLAR-SULEVANT-RAAR-KRE                                     
062100           ADD ARB-SUARTFSG-KRE(ARB-IX, PER-IX) TO                        
062200               KLAR-SUARTFSG-RAAR-KRE                                     
062300        END-IF                                                            
062400                                                                          
062500*AF1 - FÖREGÅENDE RULLANDE ÅR                                             
062600        IF PER-IX NOT > 12                                                
062700           ADD ARB-SULEVANT(ARB-IX, PER-IX) TO                            
062800               KLAR-SULEVANT-FRAAR                                        
062900           ADD ARB-SUARTFSG(ARB-IX, PER-IX) TO                            
063000               KLAR-SUARTFSG-FRAAR                                        
063100           ADD ARB-SUARTSJK(ARB-IX, PER-IX) TO                            
063200               KLAR-SUARTSJK-FRAAR                                        
063300        END-IF                                                            
063400                                                                          
063500        ADD +1 TO PER-IX                                                  
063600     END-PERFORM                                                          
063700     .                                                                    
063800     EJECT                                                                
063900 S02B-WRITE-UTPOST SECTION.                                               
064000                                                                          
064100     WRITE UT-POST FROM KLAR-AREA                                         
064200     ADD +1 TO TOT-RAKNARE-UT                                             
064300     .                                                                    
064400     EJECT                                                                
065100 Z-FINIT SECTION.                                                         
065200                                                                          
065300     CLOSE W33013                                                         
065500           W33015                                                         
065600                                                                          
065700     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
065800     MOVE 'T'                TO POSTSUM-OPKOD                             
065900                                                                          
066000     MOVE 'IN'               TO POSTSUM-TRANSTYP                          
066100     MOVE 'W33013'           TO POSTSUM-FDNAMN                            
066200     MOVE 'W33014D1'         TO POSTSUM-DDNAMN2                           
066300     MOVE TOT-RAKNARE-IN     TO POSTSUM-TOTTRANS                          
066400     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
066500                                                                          
066600     MOVE 'UT'               TO POSTSUM-TRANSTYP                          
066700     MOVE 'W33015'           TO POSTSUM-FDNAMN                            
066800     MOVE 'W33014D2'         TO POSTSUM-DDNAMN2                           
066900     MOVE TOT-RAKNARE-UT     TO POSTSUM-TOTTRANS                          
067000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
067100                                                                          
067900     MOVE 'S'                TO POSTSUM-OPKOD                             
068000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
068100     .                                                                    
068110     EJECT                                                                
068200*    -COPY WY2000P3                                                       
068210     EJECT                                                                
068300*    -COPY WY2000P7                                                       
068310     EJECT                                                                
068400*    -COPY WY2000P9                                                       
