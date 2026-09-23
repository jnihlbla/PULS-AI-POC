000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2350400.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   92/03/10.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UTSKRIFT AV LEVERANTÖRBEDÖMNINGSLISTA                            
001100*                                                                         
001200*        INFILEN ÄR SORTERAD PÅ IDFTG, IDLEVNR OCH TIAARP                 
001300*                                                                         
001400*        ETT URVAL HAR GJORTS PÅ BILD XXXX                                
001500*        OCH DESSA URVALSPARAMETRAR HAR FÖRTS TILL                        
001600*        PGM:ET VIA SOP, SAMTIDIGT SOM BESTÄLLNINGEN                      
001700*                                                                         
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  PARAMETRAR FELAKTIGA / FEL I DATKONV                    
002100*                                           SE FELTEXT/DISPLAY            
002200*        U0016 -  FELAKTIG PERIODMARKERING                                
002300                                                                          
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- PARAMETRAR FRÅN SOP                                        
003200     SELECT W235PP                     ASSIGN TO W23504D1.                
003300     SKIP2                                                                
003400*          --- LEVERANTÖRBEDÖMNINGS-DATA                                  
003500     SELECT W23503                     ASSIGN TO W23504D2.                
003600     SKIP2                                                                
003700*          --- LEVERANTÖRBEDÖMNINGSLISTA                                  
003800     SELECT W23504-001                 ASSIGN TO W23504D3.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W235PP                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700     SKIP2                                                                
004800 01  PARM                PIC X(80).                                       
004900     SKIP3                                                                
005000 FD  W23503                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400*01  -COPY W235002      -L.                                               
005500     SKIP3                                                                
005600 FD  W23504-001                                                           
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900     SKIP2                                                                
006000 01  W23504-001-RAD              PIC X(120).                              
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300     SKIP2                                                                
006400*    -COPY WY2000W7                                                       
006500     SKIP3                                                                
006600 77  IDPGM                       PIC X(8)    VALUE 'W2350400'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  W235PP-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W235PP                       VALUE 'J'.                   
007200                                                                          
007300 77  W23503-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W23503                       VALUE 'J'.                   
007500     SKIP3                                                                
007600 01  W-AREA.                                                              
007700     03  W-IDFTG                 PIC 9(2).                                
007800     03  W-IDLEVNR               PIC X(5).                                
007900     03  W-KDPRODSL              PIC 9(2).                                
008000                                                                          
008100     03  OLD-IDLEVNR             PIC X(5)   VALUE '99999'.                
008200     03  OLD-TIAARP              PIC 9(4)   VALUE 9999.                   
008300     SKIP2                                                                
008400     03  IX                  PIC S9(9)   COMP  SYNC.                      
008500*                                                                         
008600     03  WS-TIAARP           PIC 9(4).                                    
008700*                                                                         
008800     03  FILLER  REDEFINES WS-TIAARP.                                     
008900         05  WS-TIAAR        PIC 99.                                      
009000         05  WS-TIPER        PIC 99.                                      
009100*                                                                         
009200     03  WS-IN-TIAARP        PIC 9(4).                                    
009300*                                                                         
009400     03  FILLER  REDEFINES WS-IN-TIAARP.                                  
009500         05  WS-IN-TIAAR     PIC 99.                                      
009600         05  WS-IN-TIPER     PIC 99.                                      
009700     EJECT                                                                
009800 01  WS-UTDATA-MATRIS.                                                    
009900   02  WS-MATRIS  OCCURS 12.                                              
010000     05  WS-KVSENLEV         PIC S9(5)   COMP-3.                          
010100     05  WS-KVTIDLEV         PIC S9(5)   COMP-3.                          
010200     05  WS-KVLEV            PIC S9(5)   COMP-3.                          
010300     05  WS-MATVARDE         PIC S9V99   COMP-3.                          
010400     05  WS-SUSENLEV         PIC S9(9)V99  COMP-3.                        
010500     05  WS-SUTIDLEV         PIC S9(9)V99  COMP-3.                        
010600     05  WS-SULEV            PIC S9(9)V99  COMP-3.                        
010700     SKIP3                                                                
010800 01  PERIOD-MARKERING.                                                    
010900     03  PER                 PIC 9(4)  OCCURS 12.                         
011000     SKIP3                                                                
011100 01  NOLLMATRIS.                                                          
011200     05  FILLER              PIC S9(5)     COMP-3   VALUE ZERO.           
011300     05  FILLER              PIC S9(5)     COMP-3   VALUE ZERO.           
011400     05  FILLER              PIC S9(5)     COMP-3   VALUE ZERO.           
011500     05  FILLER              PIC S9V99     COMP-3   VALUE ZERO.           
011600     05  FILLER              PIC S9(9)V99  COMP-3   VALUE ZERO.           
011700     05  FILLER              PIC S9(9)V99  COMP-3   VALUE ZERO.           
011800     05  FILLER              PIC S9(9)V99  COMP-3   VALUE ZERO.           
011900     EJECT                                                                
012000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012100 01  FILLER REDEFINES DAGENS-DATUM.                                       
012200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012500     SKIP3                                                                
012600 01  DYNAMISKA-SUBPROGRAM.                                                
012700*                                                                         
012800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013100     SKIP2                                                                
013200*    --- PARAMETRAR TILL ABEND                                            
013300                                                                          
013400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013600     SKIP2                                                                
013700 01  FELTEXT.                                                             
013800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014000     EJECT                                                                
014100*    --- PARAMETRAR TILL POSTSUM                                          
014200*                                                                         
014300*01  -COPY W0005   -PRE  POSTSUM-                                         
014400     EJECT                                                                
014500*    ---- PARAMETRAR TILL WDATKONV                                        
014600                                                                          
014700*01  -COPY WDATAREA.                                                      
014800     EJECT                                                                
014900 01  PARM-AREA-START             PIC X(24)   VALUE                        
015000                                 'PARM-AREA-START  '.                     
015100     SKIP2                                                                
015200                                                                          
015300 01  PARM-AREA.                                                           
015400     03  PARM-IDFTG              PIC X(2).                                
015500     03  PARM-IDLEVNR            PIC X(5).                                
015600     03  PARM-KDPRODSL           PIC X(2).                                
015700     03  PARM-IDUSER             PIC X(8).                                
015800     03  PARM-IDMAIL             PIC X(57).                               
015900     03  FILLER                  PIC X(6).                                
016000 01  FILLER   REDEFINES PARM-AREA.                                        
016100     03  PNUM-IDFTG              PIC 9(2).                                
016200     03  PNUM-IDLEVNR            PIC X(5).                                
016300     03  PNUM-KDPRODSL           PIC 9(2).                                
016400     03  FILLER                  PIC X(8).                                
016410     03  PNUM-IDMAIL             PIC X(57).                               
016420     03  FILLER                  PIC X(6).                                
016600     EJECT                                                                
016700 01  IN-AREA-START               PIC X(24)   VALUE                        
016800                                 'IN-AREA-START  '.                       
016900     SKIP2                                                                
017000                                                                          
017100*01  AREA -COPY W235002     -PRE IN-                                      
017200     EJECT                                                                
017300 01  W001-AREA-START             PIC X(24)   VALUE                        
017400                                 'W001-AREA-START  '.                     
017500     SKIP2                                                                
017600 01  W001-HJALPAREOR.                                                     
017700*                                                                         
017800     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
017900     03  W001-ANTAL-RADER                                                 
018000                                 PIC 9(3)    VALUE 999.                   
018100     03  W001-MAX-RADER-PER-SIDA                                          
018200                                 PIC 9(3)    VALUE 42.                    
018300     03  W001-MAX-POSITIONER-PER-RAD                                      
018400                                 PIC 9(3)    VALUE 120.                   
018500     03  W001-LISTNR             PIC X(11)   VALUE 'W23504-001'.          
018600     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
018700     SKIP3                                                                
018800 01  W001-BLANK-RAD              PIC X(120)  VALUE SPACE.                 
018801                                                                          
018810 01  W001-RAD.                                                            
018900*                                                                         
019000     03  FILLER                  PIC X(120)  VALUE SPACE.                 
019100     EJECT                                                                
019200 01  PARM-RAD0.                                                           
019300     03  FILLER                  PIC X(01)   VALUE SPACE.                 
019400     03  FILLER                  PIC X(50)   VALUE                        
019500                   'URVAL GJORT MED FÖLJANDE PARAMETRAR'.                 
019600                                                                          
019700 01  PARM-RAD1.                                                           
019800     03  FILLER                  PIC X(01)   VALUE SPACE.                 
019900     03  FILLER                  PIC X(24)   VALUE 'FÖRETAG:'.            
020000     03  PARM-RAD1-IDFTG         PIC X(2)    VALUE SPACE.                 
020100                                                                          
020200 01  PARM-RAD2.                                                           
020300     03  FILLER                  PIC X(01)   VALUE SPACE.                 
020400     03  FILLER                  PIC X(21)   VALUE 'LEVERANTÖR: '.        
020500     03  PARM-RAD2-IDLEVNR       PIC X(5)    VALUE SPACE.                 
020600                                                                          
020700 01  PARM-RAD3.                                                           
020800     03  FILLER                  PIC X(01)   VALUE SPACE.                 
020900     03  FILLER                  PIC X(24)   VALUE 'PRODUKTSLAG:'.        
021000     03  PARM-RAD3-KDPRODSL      PIC Z(2)    VALUE ZERO.                  
021100     EJECT                                                                
021200 01  W001-RUBRIK1.                                                        
021300*                                                                         
021500     03  W001-FTG                PIC X(20)                                
021600                                VALUE 'VOLVO CAR CUST SER'.               
021700     03  FILLER                  PIC X(12)                                
021800                                 VALUE 'W23504-001'.                      
021900     03  FILLER                  PIC X(28)                                
022000                           VALUE 'SUPPLIERS PERFORMANCE LIST'.            
022100     03  FILLER                  PIC X(07) VALUE SPACE.                   
022200     03  FILLER                  PIC X(06)                                
022300                                 VALUE 'SUPPL'.                           
022400     03  W001-IDLEVNR            PIC X(5)  VALUE SPACE.                   
022500     03  FILLER                  PIC X(04) VALUE SPACE.                   
022600     03  W001-PS                 PIC X(03) VALUE SPACE.                   
022700     03  W001-KDPRODSL           PIC X(02) VALUE SPACE.                   
022800     03  FILLER                  PIC X(10) VALUE SPACE.                   
022900     03  W001-DATUM              PIC XXBXXBXX.                            
023000     03  FILLER                  PIC X(6)                                 
023100                                 VALUE '  SID'.                           
023200     03  W001-SID                PIC Z(4)9.                               
023300     SKIP2                                                                
023400 01  W001-RUBRIK2.                                                        
023500     03  FILLER                  PIC X(05)  VALUE 'ID: '.                 
023600     03  PARM-W001-IDUSER        PIC X(08)  VALUE SPACE.                  
023700     03  FILLER                  PIC X(10)  VALUE '     FTG: '.           
023800     03  PARM-W001-IDFTG         PIC X(02)  VALUE SPACE.                  
023900     03  FILLER                  PIC X(02)  VALUE SPACE.                  
024000     SKIP2                                                                
024100 01  W001-RUBRIK2A.                                                       
024300     03  FILLER                  PIC X(12)  VALUE 'PERIOD:'.              
024400     03  FILLER OCCURS 12.                                                
024500         05  FILLER              PIC X(05)  VALUE SPACE.                  
024600         05  W001-PER            PIC X(4)   VALUE SPACE.                  
024700     SKIP2                                                                
024800 01  W001-RUBRIK3.                                                        
024900     03  FILLER                  PIC X(120)                               
025000                   VALUE 'NO. OF '.                                       
025100     SKIP2                                                                
025200 01  W001-RUBRIK3A.                                                       
025300     03  FILLER                  PIC X(120)                               
025400                   VALUE 'DELIVERY '.                                     
025500     SKIP2                                                                
025600 01  W001-RUBRIK3B.                                                       
025700     03  FILLER                  PIC X(120)                               
025800                   VALUE 'DEVIATIONS:'.                                   
025900     EJECT                                                                
026000 01  W001-DETALJ1.                                                        
026200     03  FILLER                  PIC X(12)  VALUE 'DELAYED'.              
026300     03  FILLER OCCURS 12.                                                
026400         05  FILLER              PIC X(04)  VALUE SPACE.                  
026500         05  W001-KVSENLEV       PIC Z(5).                                
026600     SKIP2                                                                
026700 01  W001-DETALJ2.                                                        
026900     03  FILLER                  PIC X(12)  VALUE 'TOO EARLY'.            
027000     03  FILLER OCCURS 12.                                                
027100         05  FILLER              PIC X(04)  VALUE SPACE.                  
027200         05  W001-KVTIDLEV       PIC Z(5).                                
027300     SKIP2                                                                
027400 01  W001-DETALJ3.                                                        
027600     03  FILLER                  PIC X(33)                                
027700                VALUE 'NO. OF '.                                          
027800     SKIP2                                                                
027900 01  W001-DETALJ3A.                                                       
028100     03  FILLER                  PIC X(33)                                
028200                VALUE 'SCHEDULED '.                                       
028300     SKIP2                                                                
028400 01  W001-DETALJ3B.                                                       
028600     03  FILLER                  PIC X(12)  VALUE 'DELIVERIES'.           
028700     03  FILLER OCCURS 12.                                                
028800         05  FILLER              PIC X(04)  VALUE SPACE.                  
028900         05  W001-KVLEV          PIC Z(5).                                
029000     SKIP2                                                                
029100 01  W001-DETALJ4.                                                        
029300     03  FILLER                  PIC X(12)  VALUE 'INDEX'.                
029400     03  FILLER OCCURS 12.                                                
029500         05  FILLER              PIC X(05)  VALUE SPACE.                  
029600         05  W001-MATVARDE       PIC 9(1).9(2).                           
029700     SKIP2                                                                
029800 01  W001-DETALJ5.                                                        
029900     03  FILLER                  PIC X(120)                               
030000                   VALUE 'VALUE OF '.                                     
030100     SKIP2                                                                
030200 01  W001-DETALJ5A.                                                       
030300     03  FILLER                  PIC X(120)                               
030400                   VALUE 'DELIVERY'.                                      
030500     SKIP2                                                                
030600 01  W001-DETALJ5B.                                                       
030700     03  FILLER                  PIC X(120)                               
030800                   VALUE 'DEVIATIONS'.                                    
030900     SKIP2                                                                
031000 01  W001-DETALJ6.                                                        
031100     03  FILLER                  PIC X(120)                               
031200                   VALUE 'IN SW. CR.'.                                    
031300     SKIP2                                                                
031400 01  W001-DETALJ7.                                                        
031600     03  FILLER                  PIC X(12)  VALUE 'DELAYED'.              
031700     03  FILLER OCCURS 12.                                                
031800         05  W001-SUSENLEV       PIC -(9).                                
031900     SKIP2                                                                
032000 01  W001-DETALJ8.                                                        
032200     03  FILLER                  PIC X(12)  VALUE 'TOO EARLY'.            
032300     03  FILLER OCCURS 12.                                                
032400         05  W001-SUTIDLEV       PIC -(9).                                
032500     SKIP2                                                                
032600 01  W001-DETALJ9.                                                        
032700     03  FILLER                  PIC X(120)                               
032800                   VALUE 'VALUE OF'.                                      
032900     SKIP2                                                                
033000 01  W001-DETALJ9A.                                                       
033100     03  FILLER                  PIC X(120)                               
033200                   VALUE 'SCHEDULED'.                                     
033300     SKIP2                                                                
033400 01  W001-DETALJ9B.                                                       
033500     03  FILLER                  PIC X(120)                               
033600                   VALUE 'DELIVERIES'.                                    
033700     SKIP2                                                                
033800 01  W001-DETALJ10.                                                       
034000     03  FILLER                  PIC X(12)                                
034100                                 VALUE 'IN SW. CR.'.                      
034200     03  FILLER OCCURS 12.                                                
034300         05  W001-SULEV          PIC -(9).                                
034400     EJECT                                                                
034500 PROCEDURE DIVISION.                                                      
034600     SKIP2                                                                
034700                                                                          
034800     PERFORM A-INIT                                                       
034900     PERFORM S02-LAES-W23503                                              
035000                                                                          
035100     PERFORM UNTIL END-OF-W23503                                          
035200       IF PNUM-IDFTG = W-IDFTG                                            
035300         IF PARM-IDLEVNR = SPACE  OR                                      
035400            PNUM-IDLEVNR = W-IDLEVNR                                      
035500            IF PARM-KDPRODSL = ZERO  OR                                   
035600               PNUM-KDPRODSL = W-KDPRODSL                                 
035700                                                                          
035800               PERFORM B-REDIGERA-LISTA                                   
035900                                                                          
036000            END-IF                                                        
036100         END-IF                                                           
036200       END-IF                                                             
036300       PERFORM S02-LAES-W23503                                            
036400     END-PERFORM                                                          
036500                                                                          
036600     PERFORM S11-UTSKRIFT-SIDA                                            
036700                                                                          
036800     PERFORM Z-FINIT                                                      
036900                                                                          
037000     MOVE ZERO TO RETURN-CODE                                             
037100     GOBACK                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 A-INIT SECTION.                                                          
037500                                                                          
037600     OPEN INPUT  W235PP                                                   
037700                 W23503                                                   
037800                                                                          
037900     OPEN OUTPUT W23504-001                                               
038000     SKIP2                                                                
038100     PERFORM S01-LAES-W235PP                                              
038200                                                                          
038300     IF END-OF-W235PP                                                     
038400        DISPLAY '*************************'                               
038500        DISPLAY 'PARAMETRAR SAKNAS'                                       
038600        DISPLAY '*************************'                               
038700        MOVE 'PARAMETRAR SAKNAS' TO FELTEXT-STR                           
038800        PERFORM S99-ABEND                                                 
038900     END-IF                                                               
039000     SKIP2                                                                
039100     MOVE 'IDAG'            TO DAT-KDDATFORM                              
039200     MOVE  ZERO             TO DAT-I-TIDATUM                              
039300*****MOVE 'AAMMDD'  TEST    TO DAT-KDDATFORM                              
039400*****MOVE  011206   TEST    TO DAT-I-TIDATUM                              
039500                                                                          
039600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
039700                         DAT-O-TIDATUM DAT-KDSVAR                         
039800     IF DAT-KDSVAR-OK                                                     
039900       MOVE DAT-TIAAMMDD    TO  DAGENS-DATUM                              
040000       MOVE DAT-TIAA-RPPER  TO  WS-TIAAR                                  
040100       MOVE DAT-TIRP        TO  WS-TIPER                                  
040200     ELSE                                                                 
040300       DISPLAY '*************************'                                
040400       DISPLAY 'FEL I DATKONV    '                                        
040500       DISPLAY '*************************'                                
040600       MOVE 'FEL I DATKONV' TO FELTEXT-STR                                
040700       PERFORM S99-ABEND                                                  
040800     END-IF                                                               
040900                                                                          
041000     MOVE DAGENS-DATUM TO W001-DATUM                                      
041100                                                                          
041200     PERFORM AA-INIT-PERIODER                                             
041300                                                                          
041400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
041500     IF PARM-IDFTG = '57'                                                 
041600        MOVE 'VOLVO CAR C S  '  TO W001-FTG                               
041700     END-IF                                                               
041800                                                                          
041900     PERFORM AB-SKRIV-PARM-SIDA                                           
042000     .                                                                    
042100     EJECT                                                                
042200 AA-INIT-PERIODER SECTION.                                                
042300     SKIP2                                                                
042400*    LISTAN KÖRS FRAM FRÅN EN FIL SOM FRAMSTÄLLS I PERIODSKARV,           
042500*    SÅ ATT FÖRSTA PERIOD SOM VISAS, ÄR FÖREGÅENDE (=-1)                  
042600                                                                          
042700     SUBTRACT 1 FROM WS-TIPER                                             
042800                                                                          
042900     MOVE 1 TO IX                                                         
043000*                                                                         
043100     PERFORM UNTIL IX > 12                                                
043200       IF WS-TIPER = 0                                                    
043300         MOVE 12 TO WS-TIPER                                              
043400         IF WS-TIAAR = 00                                                 
043500           MOVE 99 TO WS-TIAAR                                            
043600         ELSE                                                             
043700           SUBTRACT 1 FROM WS-TIAAR                                       
043800         END-IF                                                           
043900       END-IF                                                             
044000       MOVE WS-TIAARP TO PER (IX)  W001-PER (IX)                          
044100       DISPLAY 'PER(' IX '): ' PER(IX)                                    
044200       ADD 1 TO IX                                                        
044300       SUBTRACT 1 FROM WS-TIPER                                           
044400     END-PERFORM                                                          
044500     ADD +10 TO WS-TIAARP                                                 
044600                                                                          
044700     PERFORM S10-NOLLSTALLN                                               
044800     .                                                                    
044900     EJECT                                                                
045000 AB-SKRIV-PARM-SIDA SECTION.                                              
045100     SKIP2                                                                
045200     MOVE PARM-IDUSER        TO PARM-W001-IDUSER                          
045300     MOVE PARM-IDFTG         TO PARM-W001-IDFTG                           
045400     MOVE PARM-IDFTG         TO PARM-RAD1-IDFTG                           
045500     MOVE PARM-IDLEVNR       TO PARM-RAD2-IDLEVNR                         
045600     MOVE PARM-KDPRODSL      TO PARM-RAD3-KDPRODSL                        
045700                                                                          
045800     IF PARM-IDLEVNR > SPACE                                              
045900        MOVE PNUM-IDLEVNR    TO W001-IDLEVNR                              
046000     END-IF                                                               
046100     IF PARM-KDPRODSL > '00'                                              
046200        MOVE 'PS'            TO W001-PS                                   
046300        MOVE PARM-KDPRODSL   TO W001-KDPRODSL                             
046400     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047900 B-REDIGERA-LISTA SECTION.                                                
048000     SKIP2                                                                
048100     IF IN-IDLEVNR NOT = OLD-IDLEVNR                                      
048200        IF OLD-IDLEVNR NOT = '99999'                                      
048300           PERFORM S11-UTSKRIFT-SIDA                                      
048400        END-IF                                                            
048500        PERFORM S10-NOLLSTALLN                                            
048600        MOVE IN-IDLEVNR    TO OLD-IDLEVNR                                 
048700        MOVE OLD-IDLEVNR   TO W001-IDLEVNR                                
048800        MOVE 9999          TO OLD-TIAARP                                  
048900     END-IF                                                               
049000                                                                          
049100     IF IN-TIAARP NOT = OLD-TIAARP                                        
049200        PERFORM BA-SOK-NYTT-IX                                            
049300        MOVE IN-TIAARP     TO OLD-TIAARP                                  
049400     END-IF                                                               
049500                                                                          
049600     IF IX < 13                                                           
049700        ADD IN-KVSENLEV    TO WS-KVSENLEV (IX)                            
049800        ADD IN-KVTIDLEV    TO WS-KVTIDLEV (IX)                            
049900        ADD IN-KVLEV       TO WS-KVLEV    (IX)                            
050000        ADD IN-MATVARDE    TO WS-MATVARDE (IX)                            
050100        ADD IN-SUSENLEV    TO WS-SUSENLEV (IX)                            
050200        ADD IN-SUTIDLEV    TO WS-SUTIDLEV (IX)                            
050300        ADD IN-SULEV       TO WS-SULEV    (IX)                            
050400     END-IF                                                               
050500     .                                                                    
050600     EJECT                                                                
050700 BA-SOK-NYTT-IX   SECTION.                                                
050800     SKIP2                                                                
050900        MOVE +1 TO IX                                                     
051000        PERFORM UNTIL IX > 12 OR                                          
051100           PER (IX) = IN-TIAARP                                           
051200           ADD +1 TO IX                                                   
051300        END-PERFORM                                                       
051400        IF IX = 13                                                        
051500           DISPLAY '*******************************************'          
051600           DISPLAY 'FELAKTIG PERIODMARK I DATUMKORT ELLER TRANS'          
051700           DISPLAY 'WS-TIAARP  = ' WS-TIAARP                              
051800           DISPLAY 'IN-TIAARP  = ' IN-TIAARP                              
051900           DISPLAY 'IN-IDLEVNR = ' IN-IDLEVNR                             
052000           DISPLAY '*******************************************'          
052100           MOVE IN-TIAARP  TO TMP1-YYRP                                   
052200           MOVE PER (12)   TO TMP2-YYRP                                   
052300           PERFORM WY2000P7                                               
052400           IF TMP1-YYRP >= TMP2-YYRP                                      
052500              MOVE 'FELAKTIG PERIODMARK I DATUMKORT ELLER TRANS'          
052600                 TO FELTEXT-STR                                           
052700              PERFORM S99-ABEND                                           
052800           END-IF                                                         
052900        END-IF                                                            
053000     .                                                                    
053100     EJECT                                                                
053200 Z-FINIT SECTION.                                                         
053300     SKIP2                                                                
053400     CLOSE W235PP                                                         
053500           W23503                                                         
053600           W23504-001                                                     
053700     SKIP2                                                                
053800     MOVE 'S' TO POSTSUM-OPKOD                                            
053900     CALL POSTSUM USING POSTSUM-PARM                                      
054000     .                                                                    
054100     EJECT                                                                
054200 S01-LAES-W235PP  SECTION.                                                
054300     SKIP2                                                                
054400     READ W235PP INTO PARM-AREA                                           
054500     AT END                                                               
054600        SET END-OF-W235PP TO TRUE                                         
054700                                                                          
054800     NOT AT END                                                           
054900        MOVE 'W235PP'    TO POSTSUM-FDNAMN                                
055000        MOVE 'W23504D1'  TO POSTSUM-DDNAMN2                               
055100        MOVE 'PARM'      TO POSTSUM-TRANSTYP                              
055200        CALL POSTSUM  USING POSTSUM-PARM                                  
055300     END-READ                                                             
055400                                                                          
055500     IF NOT END-OF-W23503                                                 
055600        IF PARM-IDFTG    NOT NUMERIC  OR                                  
055700          (PARM-KDPRODSL > SPACE AND                                      
055800           PNUM-KDPRODSL NOT NUMERIC)                                     
055900           DISPLAY '**************************************'               
056000           DISPLAY 'PARAMETRAR EJ NUMERISKA ELLER BLANKA'                 
056100           DISPLAY '**************************************'               
056200           MOVE    'PARAMETRAR EJ NUMERISKA ELLER BLANKA'                 
056300                   TO FELTEXT-STR                                         
056400           PERFORM S99-ABEND                                              
056500        END-IF                                                            
056501                                                                          
056600        DISPLAY 'PARAMETRAR  '                                            
056700        DISPLAY 'IDFTG:      ' PARM-IDFTG                                 
056800        DISPLAY 'IDLEVNR:    ' PARM-IDLEVNR                               
056900        DISPLAY 'KDPRODSL:   ' PARM-KDPRODSL                              
057000        DISPLAY 'IDUSER:     ' PARM-IDUSER                                
057010        DISPLAY 'IDMAIL :    ' PARM-IDMAIL                                
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400 S02-LAES-W23503  SECTION.                                                
057500     SKIP2                                                                
057600     READ W23503 INTO IN-AREA                                             
057700     AT END                                                               
057800        SET END-OF-W23503 TO TRUE                                         
057900                                                                          
058000     NOT AT END                                                           
058100        MOVE 'W23503'    TO POSTSUM-FDNAMN                                
058200        MOVE 'W23504D2'  TO POSTSUM-DDNAMN2                               
058300        MOVE 'POST'      TO POSTSUM-TRANSTYP                              
058400        CALL POSTSUM  USING POSTSUM-PARM                                  
058500                                                                          
058600        MOVE IN-IDFTG    TO W-IDFTG                                       
058700        MOVE IN-IDLEVNR  TO W-IDLEVNR                                     
058800        MOVE IN-KDPRODSL TO W-KDPRODSL                                    
058900     END-READ                                                             
059000     .                                                                    
059100     EJECT                                                                
059200 S10-NOLLSTALLN SECTION.                                                  
059300*                                                                         
059400     MOVE 1 TO IX                                                         
059500     PERFORM UNTIL IX > 12                                                
059600       MOVE NOLLMATRIS TO WS-MATRIS (IX)                                  
059700       ADD +1  TO  IX                                                     
059800     END-PERFORM                                                          
059900     .                                                                    
060000     EJECT                                                                
060100 S11-UTSKRIFT-SIDA SECTION.                                               
060200     SKIP2                                                                
060300     MOVE +1 TO IX                                                        
060400     PERFORM UNTIL IX > 12                                                
060500        MOVE WS-KVSENLEV (IX)  TO W001-KVSENLEV (IX)                      
060600        MOVE WS-KVTIDLEV (IX)  TO W001-KVTIDLEV (IX)                      
060700        MOVE WS-KVLEV    (IX)  TO W001-KVLEV    (IX)                      
060800        MOVE WS-SUSENLEV (IX)  TO W001-SUSENLEV (IX)                      
060900        MOVE WS-SUTIDLEV (IX)  TO W001-SUTIDLEV (IX)                      
061000        MOVE WS-SULEV    (IX)  TO W001-SULEV    (IX)                      
061100        IF PARM-KDPRODSL = '00'                                           
061200          IF WS-KVLEV (IX) > ZERO                                         
061300           COMPUTE WS-MATVARDE (IX) ROUNDED =                             
061400           (WS-KVLEV (IX) - WS-KVTIDLEV (IX) - WS-KVSENLEV (IX))          
061500           / WS-KVLEV (IX)                                                
061600          ELSE                                                            
061700           MOVE ZERO           TO WS-MATVARDE (IX)                        
061800          END-IF                                                          
061900        END-IF                                                            
062000        MOVE WS-MATVARDE (IX)  TO W001-MATVARDE (IX)                      
062100        ADD +1 TO IX                                                      
062200     END-PERFORM                                                          
062300                                                                          
062400     PERFORM S21A-SKRIV-RUBRIKER                                          
062500     PERFORM S21-SKRIV-W23504-001                                         
062600     .                                                                    
062700     EJECT                                                                
062800 S21-SKRIV-W23504-001  SECTION.                                           
062900                                                                          
062910     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
063000     WRITE W23504-001-RAD FROM W001-DETALJ1                               
063100                                                                          
063110     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
063200     WRITE W23504-001-RAD FROM W001-DETALJ2                               
063300                                                                          
063310     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
063400     WRITE W23504-001-RAD FROM W001-DETALJ3                               
063500     WRITE W23504-001-RAD FROM W001-DETALJ3A                              
063600     WRITE W23504-001-RAD FROM W001-DETALJ3B                              
063700                                                                          
063820     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
063900     WRITE W23504-001-RAD FROM W001-DETALJ4                               
064000                                                                          
064120     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
064130     WRITE W23504-001-RAD FROM W001-DETALJ5                               
064140                                                                          
064200     WRITE W23504-001-RAD FROM W001-DETALJ5A                              
064300     WRITE W23504-001-RAD FROM W001-DETALJ5B                              
064400                                                                          
064600     WRITE W23504-001-RAD FROM W001-DETALJ6                               
064700                                                                          
064820     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
064900     WRITE W23504-001-RAD FROM W001-DETALJ7                               
065000                                                                          
065020     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
065100     WRITE W23504-001-RAD FROM W001-DETALJ8                               
065200                                                                          
065220     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
065300     WRITE W23504-001-RAD FROM W001-DETALJ9                               
065310                                                                          
065400     WRITE W23504-001-RAD FROM W001-DETALJ9A                              
065500     WRITE W23504-001-RAD FROM W001-DETALJ9B                              
065600                                                                          
065800     WRITE W23504-001-RAD FROM W001-DETALJ10                              
065900     .                                                                    
066000     EJECT                                                                
066100 S21A-SKRIV-RUBRIKER SECTION.                                             
066200     SKIP2                                                                
066300     ADD +1 TO W001-SIDRAKNARE                                            
066511     MOVE W001-SIDRAKNARE TO W001-SID                                     
066520     WRITE W23504-001-RAD FROM W001-RUBRIK1                               
066530                                                                          
066550     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
066600     WRITE W23504-001-RAD FROM W001-RUBRIK2                               
066610                                                                          
066630     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
066700     WRITE W23504-001-RAD FROM W001-RUBRIK2A                              
066710                                                                          
066730     WRITE W23504-001-RAD FROM W001-BLANK-RAD                             
066800     WRITE W23504-001-RAD FROM W001-RUBRIK3                               
066900     WRITE W23504-001-RAD FROM W001-RUBRIK3A                              
067000     WRITE W23504-001-RAD FROM W001-RUBRIK3B                              
067300     .                                                                    
067400     EJECT                                                                
067500 S99-ABEND SECTION.                                                       
067600                                                                          
067700     SKIP2                                                                
067800     MOVE 'S' TO POSTSUM-OPKOD                                            
067900     CALL POSTSUM USING POSTSUM-PARM                                      
068000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
068100     .                                                                    
068200     EJECT                                                                
068300*    -COPY WY2000P7                                                       
