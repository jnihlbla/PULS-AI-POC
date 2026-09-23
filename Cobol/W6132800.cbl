000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6132800.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   08/12/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PRINTING FROM SCREEN 6164. ROUTINE W613S3                        
000900*                                                                         
001000*        THE PROGRAM READS     WDT1                                       
001010*                              WDT1A                                      
001100*                              WDT1B                                      
001200*                              WDT1C                                      
001300*                              WDT1D                                      
001400*                              WDT1E                                      
001500*                              WDT1F                                      
001600*                              WDT1G                                      
001700*                              WDT1H                                      
001701*                              WDT1I                                      
001710*                              WDK6                                       
001900*                                                                         
002000*    ABENDCODES:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- ACCEPT PARAMETERS FROM MPP W60164 VIA W613S3               
003300     SELECT W613PP                     ASSIGN TO W61328D1.                
003400     SKIP2                                                                
003500*          --- PRINT ON DEST PRINTER                                      
003600     SELECT W61328-001                 ASSIGN TO W61328D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W613PP                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600 01  PARM             PIC X(80).                                          
004700     SKIP3                                                                
004800 FD  W61328-001                                                           
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200 01  W61328-001-LINE             PIC X(121).                              
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W6132800'.            
005700 77  YES                         PIC X       VALUE 'J'.                   
005800 77  NOO                         PIC X       VALUE 'N'.                   
005900 77  W-KDSTAPF                   PIC X(1)   VALUE  SPACES.                
006000                                                                          
006100 77  WDT1-CHECK-FW               PIC X(5)    VALUE 'XXXXX'.               
006200     88  WDT101-CHECK                        VALUE 'WDT10'.               
006300     88  WDT1A1-CHECK                        VALUE 'WDT1A'.               
006400     88  WDT1B1-CHECK                        VALUE 'WDT1B'.               
006500     88  WDT1C1-CHECK                        VALUE 'WDT1C'.               
006600     88  WDT1D1-CHECK                        VALUE 'WDT1D'.               
006700     88  WDT1E1-CHECK                        VALUE 'WDT1E'.               
006800     88  WDT1F1-CHECK                        VALUE 'WDT1F'.               
006900     88  WDT1G1-CHECK                        VALUE 'WDT1G'.               
007000     88  WDT1H1-CHECK                        VALUE 'WDT1H'.               
007010     88  WDT1I1-CHECK                        VALUE 'WDT1I'.               
007100                                                                          
007200 77  W613PP-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-W613PP                       VALUE 'Y'.                   
007400     EJECT                                                                
007500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES TODAYS-DATE.                                        
007700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007900     03  TODAYS-DATE-DAY         PIC 9(2).                                
008000     EJECT                                                                
008100 01  GENERAL-SUBPROGRAMS.                                                 
008200*                                                                         
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008700     SKIP2                                                                
008800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008900                                                                          
009000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009300     SKIP2                                                                
009400 01  ERROR-TEXT.                                                          
009500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
009600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*                                                                         
010000*01  -COPY W0005   -PRE  POSTSUM-                                         
010100     EJECT                                                                
010200 01  PARM-AREA-START             PIC X(24)   VALUE                        
010300                                 'PARM-AREA-START  '.                     
010400 01  PARM-AREA                   PIC X(26).                               
010500 01  FILLER REDEFINES PARM-AREA.                                          
010600     03 PARM-ADLAGOMR-FOM        PIC X(2).                                
010700     03 PARM-ADGANG-FOM          PIC X(2).                                
010800     03 PARM-ADLAGOMR-TOM        PIC X(2).                                
010900     03 PARM-ADGANG-TOM          PIC X(2).                                
011000     03 PARM-KDSTAPF             PIC X(1).                                
011100     03 PARM-IDARTNR             PIC X(9).                                
011200     03 PARM-IDUSER              PIC X(7).                                
011300     03 PARM-KDPRIO-PF           PIC X(1).                                
011400     SKIP2                                                                
011500     EJECT                                                                
011600 01  W001-AREA-START             PIC X(24)   VALUE                        
011700                                 'W001-AREA-START  '.                     
011800     SKIP2                                                                
011900 01  W001-HELPAREAS.                                                      
012000*                                                                         
012100     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
012200     03  W001-MAX-LINES-PER-PAGE                                          
012300                                 PIC 9(3)    VALUE 42.                    
012400     03  W001-LISTNR             PIC X(11)   VALUE 'W60164-001'.          
012500     03  W001-PAGECOUNTER        PIC S9(5)   COMP-3 VALUE ZERO.           
012600     03  W001-LINECOUNTER        PIC S9(3)   COMP-3 VALUE 999.            
012700     EJECT                                                                
012800 01  W001-LINE.                                                           
012900*                                                                         
013000     03  FILLER                  PIC X(121)  VALUE SPACE.                 
013100     EJECT                                                                
013200 01  W001-HEADER1.                                                        
013300*                                                                         
013400     03  FILLER                  PIC X(3).                                
013500     03  FILLER                  PIC X(21)                                
013600                                 VALUE 'VCCS'.                            
013700     03  FILLER                  PIC X(12)                                
013800                                 VALUE 'W60164-001'.                      
013900     03  FILLER                  PIC X(58)                                
014000                                 VALUE '      PÅFYLLNADSFÖRSLAG'.         
014100     03  W001-DATE               PIC XXBXXBXX.                            
014200     03  FILLER                  PIC X(3).                                
014300     03  FILLER                  PIC X(4)                                 
014400                                 VALUE 'SID '.                            
014500     03  W001-PAGE               PIC Z(4)9.                               
014600     SKIP2                                                                
014700 01  W001-HEADER2.                                                        
014800**   03  FILLER                  PIC X(118)                               
014900     03  FILLER                  PIC X(3)  VALUE SPACE.                   
015000     03  FILLER                  PIC X(9)  VALUE 'ARTIKELNR'.             
015100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
015200     03  FILLER                  PIC X(12) VALUE 'ADDRESS_FRÅN'.          
015300     03  FILLER                  PIC X(5)  VALUE SPACE.                   
015400     03  FILLER                  PIC X(10) VALUE 'BEST_ANTAL'.            
015500     03  FILLER                  PIC X(3)  VALUE SPACE.                   
015600     03  FILLER                  PIC X(12) VALUE 'ADDRESS_TILL'.          
015700     03  FILLER                  PIC X(3)  VALUE SPACE.                   
015800     03  FILLER                  PIC X(12) VALUE 'ÄNDRAT_ANTAL'.          
015900     03  FILLER                  PIC X(3)  VALUE SPACE.                   
016000     03  FILLER                  PIC X(3)  VALUE 'ID.'.                   
016100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
016200     03  FILLER                  PIC X(6)  VALUE 'STATUS'.                
016300     03  FILLER                  PIC X(3)  VALUE SPACE.                   
016400     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
016500     03  FILLER                  PIC X(3)  VALUE SPACE.                   
016600     03  FILLER                  PIC X(2)  VALUE 'Q3'.                    
016700     03  FILLER                  PIC X(16) VALUE SPACE.                   
016800     EJECT                                                                
016900 01  W001-DETAIL1.                                                        
017000     03  FILLER                  PIC X(3)  VALUE SPACE.                   
017100     03  W001-IDARTNR            PIC Z(9)  VALUE SPACE.                   
017200     03  FILLER                  PIC X(3)  VALUE SPACE.                   
017300     03  W001-ADLAGOMR-FOM       PIC 99    VALUE ZERO.                    
017400     03  FILLER                  PIC X(1)  VALUE '-'.                     
017500     03  W001-ADGANG-FOM         PIC 99    VALUE ZERO.                    
017600     03  FILLER                  PIC X(1)  VALUE '-'.                     
017700     03  W001-ADPLATS-FOM        PIC 9(4)9 VALUE ZERO.                    
017800     03  FILLER                  PIC X(3)  VALUE SPACE.                   
017900     03  W001-KVBEST             PIC Z(5)9 VALUE SPACE.                   
018000     03  FILLER                  PIC X(10) VALUE SPACE.                   
018100     03  W001-ADLAGOMR-TOM       PIC 99    VALUE ZERO.                    
018200     03  FILLER                  PIC X(1)  VALUE '-'.                     
018300     03  W001-ADGANG-TOM         PIC 99    VALUE ZERO.                    
018400     03  FILLER                  PIC X(1)  VALUE '-'.                     
018500     03  W001-ADPLATS-TOM        PIC 9(4)9 VALUE ZERO.                    
018600     03  FILLER                  PIC X(3)  VALUE SPACE.                   
018700     03  W001-KVBEST-ANDR        PIC Z(5)9 VALUE SPACE.                   
018800     03  FILLER                  PIC X(9)  VALUE SPACE.                   
018900     03  W001-IDUSER             PIC X(7)  VALUE SPACE.                   
019000     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019100     03  W001-STATUS             PIC X     VALUE SPACE.                   
019200     03  FILLER                  PIC X(7)  VALUE SPACE.                   
019300     03  W001-KDPRIO-PF          PIC X     VALUE SPACE.                   
019400     03  FILLER                  PIC X(2)  VALUE SPACE.                   
019500     03  W001-KVQPACK-3          PIC Z(4)9 VALUE ZERO.                    
019600     03  FILLER                  PIC X     VALUE SPACE.                   
019700     EJECT                                                                
019800     EJECT                                                                
019900*    --- AREAS FOR IMS-SECTIONS                                           
020000*                                                                         
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020300     SKIP3                                                                
020400 01  KEYS-FOR-DLI.                                                        
020500** KEYS DECLARATION FOR WDT101                                            
020600     03  W-WDT101KY-X.                                                    
020700         05  W-IDDC-WDT101        PIC X(2)  VALUE '11'.                   
020800         05  W-IDARTNR-WDT101     PIC S9(9) COMP-3 VALUE ZERO.            
020900         05  W-TIORDTIME-WDT101   PIC 9(12) VALUE ZERO.                   
021000                                                                          
021100     03  W-ADLAGOMR-FOM-X.                                                
021200         05  W-ADLAGOMR-FOM-N    PIC S9(3) COMP-3 VALUE ZERO.             
021300                                                                          
021400     03  W-ADLAGOMR-TOM-X.                                                
021500         05  W-ADLAGOMR-TOM-N    PIC S9(3) COMP-3 VALUE ZERO.             
021600                                                                          
021700** KEYS DECLARATION FOR WDT101 - MIN AND MAX                              
021800     03  W-WDT101KY-MIN-X.                                                
021900         05  W-IDDC-WDT101-MIN      PIC X(2)  VALUE '11'.                 
022000         05  W-IDARTNR-WDT101-MIN   PIC S9(9) COMP-3 VALUE ZERO.          
022100         05  W-TIORDTIME-WDT101-MIN PIC 9(12) VALUE ZERO.                 
022200                                                                          
022300     03  W-WDT101KY-MAX-X.                                                
022400         05  W-IDDC-WDT101-MAX      PIC X(2)  VALUE '11'.                 
022500         05  W-IDARTNR-WDT101-MAX   PIC S9(9) COMP-3                      
022600                                              VALUE +999999999.           
022700         05  W-TIORDTIME-WDT101-MAX PIC 9(12)                             
022800                                              VALUE 999999999999.         
022900** KEYS DECLARATION FOR WDT1A1 MIN-MAX                                    
023000     03  W-WDT1A1KY-MIN-X.                                                
023100         05  W-IDDC-WDT1A-MIN      PIC X(2)          VALUE '11'.          
023200         05  W-ADLAGFOM-WDT1A-MIN  PIC S9(3)  COMP-3 VALUE ZERO.          
023300         05  W-KDSTAPF-WDT1A-MIN   PIC X(1)   VALUE  LOW-VALUE.           
023400         05  W-KDPRIO-PF-WDT1A-MIN PIC  S9(1) COMP-3 VALUE ZERO.          
023500         05  W-TIORDTIME-WDT1A-MIN PIC  9(12)        VALUE ZERO.          
023600                                                                          
023700     03  W-WDT1A1KY-MAX-X.                                                
023800         05  W-IDDC-WDT1A-MAX      PIC X(2)         VALUE '11'.           
023900         05  W-ADLAGFOM-WDT1A-MAX  PIC S9(3) COMP-3 VALUE +999.           
024000         05  W-KDSTAPF-WDT1A-MAX   PIC X(1)  VALUE  HIGH-VALUE.           
024100         05  W-KDPRIO-PF-WDT1A-MAX PIC S9(1) COMP-3 VALUE +9.             
024200         05  W-TIORDTIME-WDT1A-MAX PIC 9(12) VALUE  999999999999.         
024300                                                                          
024400** KEYS DECLARATION FOR WDT1B1 MIN-MAX                                    
024500     03  W-WDT1B1KY-MIN-X.                                                
024600         05  W-IDDC-WDT1B-MIN      PIC X(2)         VALUE '11'.           
024700         05  W-ADLAGTOM-WDT1B-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
024800         05  W-KDSTAPF-WDT1B-MIN   PIC X(1)  VALUE  LOW-VALUE.            
024900         05  W-KDPRIO-PF-WDT1B-MIN PIC S9(1) COMP-3 VALUE ZERO.           
025000         05  W-TIORDTIME-WDT1B-MIN PIC 9(12)        VALUE ZERO.           
025100                                                                          
025200     03  W-WDT1B1KY-MAX-X.                                                
025300         05  W-IDDC-WDT1B-MAX      PIC X(2)         VALUE '11'.           
025400         05  W-ADLAGTOM-WDT1B-MAX  PIC S9(3) COMP-3 VALUE +999.           
025500         05  W-KDSTAPF-WDT1B-MAX   PIC X(1)  VALUE  HIGH-VALUE.           
025600         05  W-KDPRIO-PF-WDT1B-MAX PIC S9(1) COMP-3 VALUE +9.             
025700         05  W-TIORDTIME-WDT1B-MAX PIC 9(12) VALUE  999999999999.         
025800                                                                          
025900** KEYS DECLARATION FOR WDT1C1                                            
026000     03  W-WDT1C1KY-MIN-X.                                                
026100         05  W-IDDC-WDT1C-MIN      PIC X(2)  VALUE '11'.                  
026200         05  W-ADLAGFOM-WDT1C-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
026300         05  W-ADGANGFOM-WDT1C-MIN PIC S9(3) COMP-3 VALUE ZERO.           
026400         05  W-ADPLATFOM-WDT1C-MIN PIC S9(5) COMP-3 VALUE ZERO.           
026500         05  W-TIORDTIME-WDT1C-MIN PIC 9(12) VALUE ZERO.                  
026600                                                                          
026700     03  W-WDT1C1KY-MAX-X.                                                
026800         05  W-IDDC-WDT1C-MAX      PIC X(2)  VALUE '11'.                  
026900         05  W-ADLAGFOM-WDT1C-MAX  PIC S9(3) COMP-3 VALUE +999.           
027000         05  W-ADGANGFOM-WDT1C-MAX PIC S9(3) COMP-3 VALUE +999.           
027100         05  W-ADPLATFOM-WDT1C-MAX PIC S9(5) COMP-3 VALUE +99999.         
027200         05  W-TIORDTIME-WDT1C-MAX PIC 9(12) VALUE 999999999999.          
027300                                                                          
027400** KEYS DECLARATION FOR WDT1D1                                            
027500     03  W-WDT1D1KY-MIN-X.                                                
027600         05  W-IDDC-WDT1D-MIN      PIC X(2)  VALUE '11'.                  
027700         05  W-ADLAGTOM-WDT1D-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
027800         05  W-ADGANGTOM-WDT1D-MIN PIC S9(3) COMP-3 VALUE ZERO.           
027900         05  W-ADPLATTOM-WDT1D-MIN PIC S9(5) COMP-3 VALUE ZERO.           
028000         05  W-TIORDTIME-WDT1D-MIN PIC 9(12) VALUE ZERO.                  
028100                                                                          
028200     03  W-WDT1D1KY-MAX-X.                                                
028300         05  W-IDDC-WDT1D-MAX      PIC X(2)  VALUE '11'.                  
028400         05  W-ADLAGTOM-WDT1D-MAX  PIC S9(3) COMP-3 VALUE +999.           
028500         05  W-ADGANGTOM-WDT1D-MAX PIC S9(3) COMP-3 VALUE +999.           
028600         05  W-ADPLATTOM-WDT1D-MAX PIC S9(5) COMP-3 VALUE +99999.         
028700         05  W-TIORDTIME-WDT1D-MAX PIC 9(12) VALUE 999999999999.          
028800                                                                          
028900** KEYS DECLARATION FOR WDT1E1                                            
029000     03  W-WDT1E1KY-MIN-X.                                                
029100         05  W-IDDC-WDT1E-MIN      PIC X(2)  VALUE '11'.                  
029200         05  W-IDUSER-WDT1E-MIN    PIC X(8)  VALUE LOW-VALUE.             
029300         05  W-ADLAGFOM-WDT1E-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
029400         05  W-ADGANGFOM-WDT1E-MIN PIC S9(3) COMP-3 VALUE ZERO.           
029500         05  W-ADPLATFOM-WDT1E-MIN PIC S9(5) COMP-3 VALUE ZERO.           
029600         05  W-KDSTAPF-WDT1E-MIN   PIC X(1)  VALUE LOW-VALUE.             
029700         05  W-TIORDTIME-WDT1E-MIN PIC 9(12) VALUE ZERO.                  
029800                                                                          
029900     03  W-WDT1E1KY-MAX-X.                                                
030000         05  W-IDDC-WDT1E-MAX      PIC X(2)  VALUE '11'.                  
030100         05  W-IDUSER-WDT1E-MAX    PIC X(8)  VALUE HIGH-VALUE.            
030200         05  W-ADLAGFOM-WDT1E-MAX  PIC S9(3) COMP-3 VALUE 999.            
030300         05  W-ADGANGFOM-WDT1E-MAX PIC S9(3) COMP-3 VALUE 999.            
030400         05  W-ADPLATFOM-WDT1E-MAX PIC S9(5) COMP-3 VALUE 99999.          
030500         05  W-KDSTAPF-WDT1E-MAX   PIC X(1)  VALUE HIGH-VALUE.            
030600         05  W-TIORDTIME-WDT1E-MAX PIC 9(12) VALUE 999999999999.          
030700                                                                          
030800** KEYS DECLARATION FOR WDT1F1                                            
030900     03  W-WDT1F1KY-MIN-X.                                                
031000         05  W-IDDC-WDT1F-MIN      PIC X(2)  VALUE '11'.                  
031100         05  W-IDUSER-WDT1F-MIN    PIC X(8)  VALUE LOW-VALUE.             
031200         05  W-ADLAGTOM-WDT1F-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
031300         05  W-ADGANGTOM-WDT1F-MIN PIC S9(3) COMP-3 VALUE ZERO.           
031400         05  W-ADPLATTOM-WDT1F-MIN PIC S9(5) COMP-3 VALUE ZERO.           
031500         05  W-KDSTAPF-WDT1F-MIN   PIC X(1)  VALUE LOW-VALUE.             
031600         05  W-TIORDTIME-WDT1F-MIN PIC 9(12) VALUE ZERO.                  
031700                                                                          
031800     03  W-WDT1F1KY-MAX-X.                                                
031900         05  W-IDDC-WDT1F-MAX      PIC X(2)  VALUE '11'.                  
032000         05  W-IDUSER-WDT1F-MAX    PIC X(8)  VALUE HIGH-VALUE.            
032100         05  W-ADLAGTOM-WDT1F-MAX  PIC S9(3) COMP-3 VALUE 999.            
032200         05  W-ADGANGTOM-WDT1F-MAX PIC S9(3) COMP-3 VALUE 999.            
032300         05  W-ADPLATTOM-WDT1F-MAX PIC S9(5) COMP-3 VALUE 99999.          
032400         05  W-KDSTAPF-WDT1F-MAX   PIC X(1)  VALUE HIGH-VALUE.            
032500         05  W-TIORDTIME-WDT1F-MAX PIC 9(12) VALUE 999999999999.          
032600                                                                          
032700** KEYS DECLARATION FOR WDT1G1                                            
032800     03  W-WDT1G1KY-MIN-X.                                                
032900         05  W-IDDC-WDT1G-MIN      PIC X(2)  VALUE '11'.                  
033000         05  W-KDPRIO-WDT1G-MIN    PIC S9    COMP-3 VALUE ZERO.           
033100         05  W-ADLAGFOM-WDT1G-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
033200         05  W-ADGANGFOM-WDT1G-MIN PIC S9(3) COMP-3 VALUE ZERO.           
033300         05  W-ADPLATFOM-WDT1G-MIN PIC S9(5) COMP-3 VALUE ZERO.           
033400         05  W-KDSTAPF-WDT1G-MIN   PIC X(1)  VALUE LOW-VALUE.             
033500         05  W-TIORDTIME-WDT1G-MIN PIC 9(12) VALUE ZERO.                  
033600                                                                          
033700     03  W-WDT1G1KY-MAX-X.                                                
033800         05  W-IDDC-WDT1G-MAX      PIC X(2)  VALUE '11'.                  
033900         05  W-KDPRIO-WDT1G-MAX    PIC S9    COMP-3 VALUE 9.              
034000         05  W-ADLAGFOM-WDT1G-MAX  PIC S9(3) COMP-3 VALUE 999.            
034100         05  W-ADGANGFOM-WDT1G-MAX PIC S9(3) COMP-3 VALUE 999.            
034200         05  W-ADPLATFOM-WDT1G-MAX PIC S9(5) COMP-3 VALUE 99999.          
034300         05  W-KDSTAPF-WDT1G-MAX   PIC X(1)  VALUE HIGH-VALUE.            
034400         05  W-TIORDTIME-WDT1G-MAX PIC 9(12) VALUE 999999999999.          
034500                                                                          
034600** KEYS DECLARATION FOR WDT1H1                                            
034700     03  W-WDT1H1KY-MIN-X.                                                
034800         05  W-IDDC-WDT1H-MIN      PIC X(2)  VALUE '11'.                  
034900         05  W-KDPRIO-WDT1H-MIN    PIC S9    COMP-3 VALUE ZERO.           
035000         05  W-ADLAGTOM-WDT1H-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
035100         05  W-ADGANGTOM-WDT1H-MIN PIC S9(3) COMP-3 VALUE ZERO.           
035200         05  W-ADPLATTOM-WDT1H-MIN PIC S9(5) COMP-3 VALUE ZERO.           
035300         05  W-KDSTAPF-WDT1H-MIN   PIC X(1)  VALUE LOW-VALUE.             
035400         05  W-TIORDTIME-WDT1H-MIN PIC 9(12) VALUE ZERO.                  
035500                                                                          
035600     03  W-WDT1H1KY-MAX-X.                                                
035700         05  W-IDDC-WDT1H-MAX      PIC X(2)  VALUE '11'.                  
035800         05  W-KDPRIO-WDT1H-MAX    PIC S9    COMP-3 VALUE 9.              
035900         05  W-ADLAGTOM-WDT1H-MAX  PIC S9(3) COMP-3 VALUE 999.            
036000         05  W-ADGANGTOM-WDT1H-MAX PIC S9(3) COMP-3 VALUE 999.            
036100         05  W-ADPLATTOM-WDT1H-MAX PIC S9(5) COMP-3 VALUE 99999.          
036200         05  W-KDSTAPF-WDT1H-MAX   PIC X(1)  VALUE HIGH-VALUE.            
036300         05  W-TIORDTIME-WDT1H-MAX PIC 9(12) VALUE 999999999999.          
036400                                                                          
036410** KEYS DECLARATION FOR WDT1I1                                            
036420     03  W-WDT1I1KY-MIN-X.                                                
036430         05  W-IDDC-WDT1I-MIN      PIC X(2)  VALUE '11'.                  
036440         05  W-ADLAGFOM-WDT1I-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
036450         05  W-ADLAGTOM-WDT1I-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
036460         05  W-KDPRIO-WDT1I-MIN    PIC S9(1) COMP-3 VALUE ZERO.           
036470         05  W-ADGANGFOM-WDT1I-MIN PIC S9(3) COMP-3 VALUE ZERO.           
036480         05  W-ADPLATFOM-WDT1I-MIN PIC S9(5) COMP-3 VALUE ZERO.           
036490         05  W-KDSTAPF-WDT1I-MIN   PIC X(1)  VALUE LOW-VALUE.             
036491         05  W-TIORDTIME-WDT1I-MIN PIC 9(12) VALUE ZERO.                  
036492                                                                          
036493     03  W-WDT1I1KY-MAX-X.                                                
036494         05  W-IDDC-WDT1I-MAX      PIC X(2)  VALUE '11'.                  
036495         05  W-ADLAGFOM-WDT1I-MAX  PIC S9(3) COMP-3 VALUE ZERO.           
036496         05  W-ADLAGTOM-WDT1I-MAX  PIC S9(3) COMP-3 VALUE ZERO.           
036497         05  W-KDPRIO-WDT1I-MAX    PIC S9(1) COMP-3 VALUE +9.             
036498         05  W-ADGANGFOM-WDT1I-MAX PIC S9(3) COMP-3 VALUE ZERO.           
036499         05  W-ADPLATFOM-WDT1I-MAX PIC S9(5) COMP-3 VALUE ZERO.           
036500         05  W-KDSTAPF-WDT1I-MAX   PIC X(1)  VALUE LOW-VALUE.             
036501         05  W-TIORDTIME-WDT1I-MAX PIC 9(12) VALUE ZERO.                  
036502                                                                          
036510** KEYS DECLARATION FOR WDK601                                            
036600     03  W-IDARTNR-X.                                                     
036700         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
036800                                                                          
036900** KEYS DECLARATION FOR WDK611                                            
037000     03  W-KDSEGKEY-X.                                                    
037100         05  W-KDSEGKEY           PIC X(1)  VALUE '1'.                    
037200                                                                          
037300     SKIP2                                                                
037400*    --- STATUS-KOD FROM IMS                                              
037500 01  STATUS-WS                   PIC XX.                                  
037600     88  SEGMENT-FOUND                       VALUE '  '.                  
037700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
037800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
037900     SKIP2                                                                
038000 01  GOOD-STATUSCODES.                                                    
038100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038200     SKIP3                                                                
038300 01  SSA1                        PIC X(128).                              
038400 01  SSA2                        PIC X(128).                              
038500     EJECT                                                                
038600*    --- IMS FUNCTION CODES                                               
038700*01  -COPY W0003                                                          
038800     EJECT                                                                
038900*    ---  DLI INPUT-OUTPUT AREA                                           
039000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT101'.                      
039100 01  DLI-IO-WDT101.                                                       
039200*    03  -COPY WDT101                                                     
039300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1A1'.                      
039400 01  DLI-IO-WDT1A1.                                                       
039500*    03  -COPY WDT1A1                                                     
039600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1B1'.                      
039700 01  DLI-IO-WDT1B1.                                                       
039800*    03  -COPY WDT1B1                                                     
039900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1C1'.                      
040000 01  DLI-IO-WDT1C1.                                                       
040100*    03  -COPY WDT1C1                                                     
040200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1D1'.                      
040300 01  DLI-IO-WDT1D1.                                                       
040400*    03  -COPY WDT1D1                                                     
040500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1E1'.                      
040600 01  DLI-IO-WDT1E1.                                                       
040700*    03  -COPY WDT1E1                                                     
040800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1F1'.                      
040900 01  DLI-IO-WDT1F1.                                                       
041000*    03  -COPY WDT1F1                                                     
041100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1G1'.                      
041200 01  DLI-IO-WDT1G1.                                                       
041300*    03  -COPY WDT1G1                                                     
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1H1'.                      
041500 01  DLI-IO-WDT1H1.                                                       
041600*    03  -COPY WDT1H1                                                     
041610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1I1'.                      
041620 01  DLI-IO-WDT1I1.                                                       
041630*    03  -COPY WDT1I1                                                     
041700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
041800 01  DLI-IO-WDK611.                                                       
041900*    03  -COPY WDK611                                                     
042000     EJECT                                                                
042100 LINKAGE SECTION.                                                         
042200                                                                          
042300*01  -COPY W0008  -PRE WDT1-                                              
042400     05  FILLER                  PIC X.                                   
042500                                                                          
042600*01  -COPY W0008  -PRE WDT1A-                                             
042700     05  FILLER                  PIC X.                                   
042800                                                                          
042900*01  -COPY W0008  -PRE WDT1B-                                             
043000     05  FILLER                  PIC X.                                   
043100                                                                          
043200*01  -COPY W0008  -PRE WDT1C-                                             
043300     05  FILLER                  PIC X.                                   
043400                                                                          
043500*01  -COPY W0008  -PRE WDT1D-                                             
043600     05  FILLER                  PIC X.                                   
043700                                                                          
043800*01  -COPY W0008  -PRE WDT1E-                                             
043900     05  FILLER                  PIC X.                                   
044000                                                                          
044100*01  -COPY W0008  -PRE WDT1F-                                             
044200     05  FILLER                  PIC X.                                   
044300                                                                          
044400*01  -COPY W0008  -PRE WDT1G-                                             
044500     05  FILLER                  PIC X.                                   
044600                                                                          
044700*01  -COPY W0008  -PRE WDT1H-                                             
044800     05  FILLER                  PIC X.                                   
044900                                                                          
044910*01  -COPY W0008  -PRE WDT1I-                                             
044920     05  FILLER                  PIC X.                                   
044930                                                                          
045000*01  -COPY W0008  -PRE WDK6-                                              
045100     05  FILLER                  PIC X.                                   
045200                                                                          
045300     EJECT                                                                
045400 PROCEDURE DIVISION  USING WDT1-PCB WDT1A-PCB WDT1B-PCB                   
045500                          WDT1C-PCB WDT1D-PCB WDT1E-PCB                   
045600                          WDT1F-PCB WDT1G-PCB WDT1H-PCB                   
045610                          WDT1I-PCB WDK6-PCB.                             
045700                                                                          
045800 MAIN SECTION.                                                            
045900     ENTRY 'DLITCBL' USING WDT1-PCB WDT1A-PCB WDT1B-PCB                   
046000                          WDT1C-PCB WDT1D-PCB WDT1E-PCB                   
046010                          WDT1F-PCB WDT1G-PCB WDT1H-PCB                   
046020                          WDT1I-PCB WDK6-PCB.                             
046200                                                                          
046400     PERFORM A-INIT                                                       
046500                                                                          
046600     PERFORM S01-READ-W613PP                                              
046700                                                                          
046800     IF PARM-ADLAGOMR-FOM > SPACES AND PARM-ADGANG-FOM = SPACES           
046900       MOVE 'WDT1A' TO WDT1-CHECK-FW                                      
047000     ELSE                                                                 
047100       IF PARM-ADLAGOMR-TOM > SPACES AND PARM-ADGANG-TOM = SPACES         
047200         MOVE 'WDT1B' TO WDT1-CHECK-FW                                    
047300       END-IF                                                             
047400     END-IF                                                               
047500                                                                          
047600     IF PARM-ADLAGOMR-FOM IS NUMERIC AND                                  
047700        PARM-ADGANG-FOM IS NUMERIC                                        
047800       MOVE 'WDT1C' TO WDT1-CHECK-FW                                      
047900     ELSE                                                                 
048000       IF PARM-ADLAGOMR-TOM IS NUMERIC AND                                
048100          PARM-ADGANG-TOM IS NUMERIC                                      
048200         MOVE 'WDT1D' TO WDT1-CHECK-FW                                    
048300       END-IF                                                             
048400     END-IF                                                               
048500                                                                          
048501     IF PARM-ADLAGOMR-FOM IS NUMERIC AND                                  
048502        PARM-ADLAGOMR-TOM IS NUMERIC                                      
048503       MOVE 'WDT1I' TO WDT1-CHECK-FW                                      
048504     END-IF                                                               
048505                                                                          
048510     INSPECT PARM-IDARTNR REPLACING ALL SPACE BY ZERO                     
048600     IF PARM-IDARTNR > SPACES AND PARM-IDARTNR NOT = ALL '0'              
048700       MOVE 'WDT10' TO WDT1-CHECK-FW                                      
048800     END-IF                                                               
048900                                                                          
049000     IF PARM-IDUSER > SPACES                                              
049100       IF PARM-ADLAGOMR-TOM IS NUMERIC                                    
049200         MOVE 'WDT1F' TO WDT1-CHECK-FW                                    
049300       ELSE                                                               
049400         MOVE 'WDT1E' TO WDT1-CHECK-FW                                    
049500       END-IF                                                             
049600     END-IF                                                               
049700                                                                          
049800     IF PARM-KDPRIO-PF > SPACES                                           
049900       IF PARM-ADLAGOMR-TOM IS NUMERIC                                    
050000         MOVE 'WDT1H' TO WDT1-CHECK-FW                                    
050100       ELSE                                                               
050200         MOVE 'WDT1G' TO WDT1-CHECK-FW                                    
050300       END-IF                                                             
050400     END-IF                                                               
050410                                                                          
050600     EVALUATE TRUE                                                        
050700       WHEN WDT1A1-CHECK                                                  
050800         PERFORM B-READ-WDT1A1                                            
050900       WHEN WDT1B1-CHECK                                                  
051000         PERFORM C-READ-WDT1B1                                            
051100       WHEN WDT1C1-CHECK                                                  
051200         PERFORM D-READ-WDT1C1                                            
051300       WHEN WDT1D1-CHECK                                                  
051400         PERFORM E-READ-WDT1D1                                            
051500       WHEN WDT1E1-CHECK                                                  
051600         PERFORM F-READ-WDT1E1                                            
051700       WHEN WDT1F1-CHECK                                                  
051800         PERFORM G-READ-WDT1F1                                            
051900       WHEN WDT1G1-CHECK                                                  
052000         PERFORM H-READ-WDT1G1                                            
052100       WHEN WDT1H1-CHECK                                                  
052200         PERFORM I-READ-WDT1H1                                            
052210       WHEN WDT1I1-CHECK                                                  
052220         PERFORM K-READ-WDT1I1                                            
052300       WHEN WDT101-CHECK                                                  
052400         PERFORM J-READ-WDT101                                            
052500     END-EVALUATE                                                         
052600                                                                          
052700     PERFORM Z-FINIT                                                      
052800                                                                          
052900     MOVE ZERO TO RETURN-CODE                                             
053000     GOBACK                                                               
053100     .                                                                    
053200     EJECT                                                                
053300 A-INIT SECTION.                                                          
053400     OPEN INPUT  W613PP                                                   
053500     OPEN OUTPUT W61328-001                                               
053600                                                                          
053700     ACCEPT TODAYS-DATE  FROM DATE                                        
053800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
053900     .                                                                    
054000     EJECT                                                                
054100 B-READ-WDT1A1 SECTION.                                                   
054200     IF PARM-ADLAGOMR-FOM IS NOT NUMERIC                                  
054300       MOVE ZERO               TO W-ADLAGFOM-WDT1A-MIN                    
054400     ELSE                                                                 
054500       MOVE PARM-ADLAGOMR-FOM  TO W-ADLAGFOM-WDT1A-MIN                    
054600                                  W-ADLAGFOM-WDT1A-MAX                    
054700     END-IF                                                               
054800                                                                          
054900     IF PARM-KDSTAPF = ' ' OR '+'                                         
055000       MOVE LOW-VALUE          TO W-KDSTAPF-WDT1A-MIN                     
055100       MOVE HIGH-VALUE         TO W-KDSTAPF-WDT1A-MAX                     
055200     ELSE                                                                 
055300       MOVE PARM-KDSTAPF       TO W-KDSTAPF-WDT1A-MIN                     
055400                                  W-KDSTAPF-WDT1A-MAX                     
055500     END-IF                                                               
055600                                                                          
055700     IF PARM-ADLAGOMR-FOM IS NOT NUMERIC                                  
055800     AND (PARM-KDSTAPF = 'R' OR 'V' OR 'L')                               
055900       PERFORM IMS-GU-WDT1A1-STATUS                                       
056000     ELSE                                                                 
056100       PERFORM IMS-GU-WDT1A1                                              
056200     END-IF                                                               
056300                                                                          
056400     IF SEGMENT-FOUND                                                     
056500       MOVE SEQA-IDWDT101     TO W-WDT101KY-X                             
056600       PERFORM IMS-GU-WDT101                                              
056700       PERFORM S1-MOVE-FROM-WDT101                                        
056800       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
056900         IF PARM-ADLAGOMR-FOM IS NOT NUMERIC                              
057000         AND (PARM-KDSTAPF = 'R' OR 'V' OR 'L')                           
057100            PERFORM IMS-GN-WDT1A1-STATUS                                  
057200         ELSE                                                             
057300            PERFORM IMS-GN-WDT1A1                                         
057400         END-IF                                                           
057500         IF SEGMENT-FOUND                                                 
057600           MOVE SEQA-IDWDT101     TO W-WDT101KY-X                         
057700           PERFORM IMS-GU-WDT101                                          
057800           PERFORM S1-MOVE-FROM-WDT101                                    
057900         END-IF                                                           
058000       END-PERFORM                                                        
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 C-READ-WDT1B1 SECTION.                                                   
058500     IF PARM-ADLAGOMR-TOM IS NOT NUMERIC                                  
058600       MOVE ZERO               TO W-ADLAGTOM-WDT1B-MIN                    
058700     ELSE                                                                 
058800       MOVE PARM-ADLAGOMR-TOM  TO W-ADLAGTOM-WDT1B-MIN                    
058900                                  W-ADLAGTOM-WDT1B-MAX                    
059000     END-IF                                                               
059100                                                                          
059200     IF PARM-KDSTAPF = ' ' OR '+'                                         
059300       MOVE LOW-VALUE          TO W-KDSTAPF-WDT1B-MIN                     
059400       MOVE HIGH-VALUE         TO W-KDSTAPF-WDT1B-MAX                     
059500     ELSE                                                                 
059600       MOVE PARM-KDSTAPF       TO W-KDSTAPF-WDT1B-MIN                     
059700                                  W-KDSTAPF-WDT1B-MAX                     
059800     END-IF                                                               
059900                                                                          
060000     IF PARM-ADLAGOMR-TOM IS NOT NUMERIC                                  
060100     AND (PARM-KDSTAPF = 'R' OR 'V' OR 'L')                               
060200       PERFORM IMS-GU-WDT1B1-STATUS                                       
060300     ELSE                                                                 
060400       PERFORM IMS-GU-WDT1B1                                              
060500     END-IF                                                               
060600                                                                          
060700     IF SEGMENT-FOUND                                                     
060800       MOVE SEQB-IDWDT101     TO W-WDT101KY-X                             
060900       PERFORM IMS-GU-WDT101                                              
061000       PERFORM S1-MOVE-FROM-WDT101                                        
061100       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
061200        IF PARM-ADLAGOMR-TOM IS NOT NUMERIC                               
061300        AND (PARM-KDSTAPF = 'R' OR 'V' OR 'L')                            
061400            PERFORM IMS-GN-WDT1B1-STATUS                                  
061500        ELSE                                                              
061600            PERFORM IMS-GN-WDT1B1                                         
061700        END-IF                                                            
061800        IF SEGMENT-FOUND                                                  
061900          MOVE SEQB-IDWDT101     TO W-WDT101KY-X                          
062000          PERFORM IMS-GU-WDT101                                           
062100          PERFORM S1-MOVE-FROM-WDT101                                     
062200        END-IF                                                            
062300       END-PERFORM                                                        
062400     END-IF                                                               
062500     .                                                                    
062600     EJECT                                                                
062700 D-READ-WDT1C1 SECTION.                                                   
062800     MOVE PARM-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1C-MIN                       
062900                               W-ADLAGFOM-WDT1C-MAX                       
063000     MOVE PARM-ADGANG-FOM   TO W-ADGANGFOM-WDT1C-MIN                      
063100                               W-ADGANGFOM-WDT1C-MAX                      
063200                                                                          
063300**   PERFORM IMS-GU-WDT1C1                                                
063400                                                                          
063410     IF PARM-ADLAGOMR-FOM IS NUMERIC                                      
063411     AND PARM-ADGANG-FOM  IS NUMERIC                                      
063420     AND (PARM-KDSTAPF = 'R' OR 'V' OR 'L')                               
063421       MOVE PARM-KDSTAPF    TO W-KDSTAPF                                  
063430       PERFORM IMS-GU-WDT1C1-STATUS                                       
063440     ELSE                                                                 
063450       PERFORM IMS-GU-WDT1C1                                              
063460     END-IF                                                               
063470                                                                          
063500     IF SEGMENT-FOUND                                                     
063600       MOVE SEQC-IDWDT101     TO W-WDT101KY-X                             
063700       PERFORM IMS-GU-WDT101                                              
063800       PERFORM S1-MOVE-FROM-WDT101                                        
063900       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
064000**       PERFORM IMS-GN-WDT1C1                                            
064110         IF PARM-ADLAGOMR-FOM IS NUMERIC                                  
064120         AND PARM-ADGANG-FOM  IS NUMERIC                                  
064130         AND (PARM-KDSTAPF = 'R' OR 'V' OR 'L')                           
064131           MOVE PARM-KDSTAPF    TO W-KDSTAPF                              
064140           PERFORM IMS-GN-WDT1C1-STATUS                                   
064150         ELSE                                                             
064160           PERFORM IMS-GN-WDT1C1                                          
064170         END-IF                                                           
064180                                                                          
064190         IF SEGMENT-FOUND                                                 
064200           MOVE SEQC-IDWDT101     TO W-WDT101KY-X                         
064300           PERFORM IMS-GU-WDT101                                          
064400           PERFORM S1-MOVE-FROM-WDT101                                    
064500         END-IF                                                           
064600       END-PERFORM                                                        
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 E-READ-WDT1D1 SECTION.                                                   
065100     MOVE PARM-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1D-MIN                       
065200                               W-ADLAGTOM-WDT1D-MAX                       
065300     MOVE PARM-ADGANG-TOM   TO W-ADGANGTOM-WDT1D-MIN                      
065400                               W-ADGANGTOM-WDT1D-MAX                      
065500                                                                          
065600**   PERFORM IMS-GU-WDT1D1                                                
065610     IF PARM-ADLAGOMR-TOM IS NUMERIC                                      
065620     AND PARM-ADGANG-TOM  IS NUMERIC                                      
065630     AND (PARM-KDSTAPF   = 'R' OR 'V' OR 'L')                             
065631       MOVE PARM-KDSTAPF TO W-KDSTAPF                                     
065640       PERFORM IMS-GU-WDT1D1-STATUS                                       
065650     ELSE                                                                 
065660       PERFORM IMS-GU-WDT1D1                                              
065670     END-IF                                                               
065680                                                                          
065800     IF SEGMENT-FOUND                                                     
065900       MOVE SEQD-IDWDT101     TO W-WDT101KY-X                             
066000       PERFORM IMS-GU-WDT101                                              
066100       PERFORM S1-MOVE-FROM-WDT101                                        
066200       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
066300**       PERFORM IMS-GN-WDT1D1                                            
066310         IF PARM-ADLAGOMR-TOM IS NUMERIC                                  
066320         AND PARM-ADGANG-TOM  IS NUMERIC                                  
066330         AND (W-KDSTAPF = 'R' OR 'V' OR 'L')                              
066331           MOVE PARM-KDSTAPF    TO W-KDSTAPF                              
066340           PERFORM IMS-GN-WDT1D1-STATUS                                   
066350         ELSE                                                             
066360           PERFORM IMS-GN-WDT1D1                                          
066370         END-IF                                                           
066380                                                                          
066400         IF SEGMENT-FOUND                                                 
066500           MOVE SEQD-IDWDT101     TO W-WDT101KY-X                         
066600           PERFORM IMS-GU-WDT101                                          
066700           PERFORM S1-MOVE-FROM-WDT101                                    
066800         END-IF                                                           
066900       END-PERFORM                                                        
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 F-READ-WDT1E1 SECTION.                                                   
067400     MOVE PARM-IDUSER     TO W-IDUSER-WDT1E-MIN                           
067500                             W-IDUSER-WDT1E-MAX                           
067600                                                                          
067700     IF PARM-KDSTAPF = ' ' OR '+'                                         
067800       MOVE LOW-VALUE  TO W-KDSTAPF-WDT1E-MIN                             
067900       MOVE HIGH-VALUE TO W-KDSTAPF-WDT1E-MAX                             
068000     ELSE                                                                 
068100       MOVE PARM-KDSTAPF TO W-KDSTAPF-WDT1E-MIN                           
068200                          W-KDSTAPF-WDT1E-MAX                             
068300     END-IF                                                               
068400                                                                          
068500     IF PARM-ADLAGOMR-FOM IS NUMERIC                                      
068600       MOVE PARM-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1E-MIN                     
068700                                 W-ADLAGFOM-WDT1E-MAX                     
069400     END-IF                                                               
069600                                                                          
069700     IF ((PARM-IDUSER > SPACES  AND                                       
069800        PARM-ADLAGOMR-FOM IS NUMERIC AND                                  
069900        (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                               
070000        OR                                                                
070100        (PARM-IDUSER > SPACES AND                                         
070200        (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                              
070300       PERFORM IMS-GU-WDT1E1-STATUS                                       
070400     ELSE                                                                 
070500       IF ((PARM-IDUSER > SPACES AND                                      
070600          PARM-ADLAGOMR-FOM IS NUMERIC)                                   
070700          OR                                                              
070800          PARM-IDUSER > SPACES)                                           
070900          PERFORM IMS-GU-WDT1E1                                           
071000       END-IF                                                             
071100     END-IF                                                               
071200                                                                          
071300     IF SEGMENT-FOUND                                                     
071400       MOVE SEQE-IDWDT101     TO W-WDT101KY-X                             
071500       PERFORM IMS-GU-WDT101                                              
071600       PERFORM S1-MOVE-FROM-WDT101                                        
071700       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
072000         IF ((PARM-IDUSER > SPACES  AND                                   
072100            PARM-ADLAGOMR-FOM IS NUMERIC AND                              
072200            (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                           
072300            OR                                                            
072400            (PARM-IDUSER > SPACES AND                                     
072500            (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                          
072600           PERFORM IMS-GN-WDT1E1-STATUS                                   
072700         ELSE                                                             
072800           IF ((PARM-IDUSER > SPACES AND                                  
072900              PARM-ADLAGOMR-FOM IS NUMERIC)                               
073000              OR                                                          
073100              PARM-IDUSER > SPACES)                                       
073200             PERFORM IMS-GN-WDT1E1                                        
073300           END-IF                                                         
073400         END-IF                                                           
073500         IF SEGMENT-FOUND                                                 
073600           MOVE SEQE-IDWDT101     TO W-WDT101KY-X                         
073700           PERFORM IMS-GU-WDT101                                          
073800           PERFORM S1-MOVE-FROM-WDT101                                    
073900         END-IF                                                           
074000       END-PERFORM                                                        
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 G-READ-WDT1F1 SECTION.                                                   
074500     MOVE PARM-IDUSER     TO W-IDUSER-WDT1F-MIN                           
074600                             W-IDUSER-WDT1F-MAX                           
074700                                                                          
074800     IF PARM-KDSTAPF = ' ' OR '+'                                         
074900       MOVE LOW-VALUE  TO W-KDSTAPF-WDT1F-MIN                             
075000       MOVE HIGH-VALUE TO W-KDSTAPF-WDT1F-MAX                             
075100     ELSE                                                                 
075200       MOVE PARM-KDSTAPF  TO W-KDSTAPF-WDT1F-MIN                          
075300                          W-KDSTAPF-WDT1F-MAX                             
075400     END-IF                                                               
075500                                                                          
075600     IF PARM-ADLAGOMR-TOM IS NUMERIC                                      
075700       MOVE PARM-ADLAGOMR-FOM TO W-ADLAGTOM-WDT1F-MIN                     
075800                              W-ADLAGTOM-WDT1F-MAX                        
075900     END-IF                                                               
076000                                                                          
076100     IF ((PARM-IDUSER > SPACES  AND                                       
076200        PARM-ADLAGOMR-TOM IS NUMERIC AND                                  
076300        (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                               
076400        OR                                                                
076500        (PARM-IDUSER > SPACES AND                                         
076600        (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                              
076700       PERFORM IMS-GU-WDT1F1-STATUS                                       
076800     ELSE                                                                 
076900       IF ((PARM-IDUSER > SPACES AND                                      
077000          PARM-ADLAGOMR-TOM IS NUMERIC)                                   
077100          OR                                                              
077200          PARM-IDUSER > SPACES)                                           
077300          PERFORM IMS-GU-WDT1F1                                           
077400       END-IF                                                             
077500     END-IF                                                               
077600                                                                          
077700     IF SEGMENT-FOUND                                                     
077800       MOVE SEQF-IDWDT101     TO W-WDT101KY-X                             
077900       PERFORM IMS-GU-WDT101                                              
078000       PERFORM S1-MOVE-FROM-WDT101                                        
078100       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
078200         IF ((PARM-IDUSER > SPACES  AND                                   
078300            PARM-ADLAGOMR-TOM IS NUMERIC AND                              
078400            (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                           
078500            OR                                                            
078600            (PARM-IDUSER > SPACES AND                                     
078700            (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                          
078800            PERFORM IMS-GN-WDT1F1-STATUS                                  
078900         ELSE                                                             
079000            IF ((PARM-IDUSER > SPACES AND                                 
079100               PARM-ADLAGOMR-TOM IS NUMERIC)                              
079200               OR                                                         
079300               PARM-IDUSER > SPACES)                                      
079400               PERFORM IMS-GN-WDT1F1                                      
079500            END-IF                                                        
079600         END-IF                                                           
079700         IF SEGMENT-FOUND                                                 
079800           MOVE SEQF-IDWDT101     TO W-WDT101KY-X                         
079900           PERFORM IMS-GU-WDT101                                          
080000           PERFORM S1-MOVE-FROM-WDT101                                    
080100         END-IF                                                           
080200       END-PERFORM                                                        
080300     END-IF                                                               
080400     .                                                                    
080500     EJECT                                                                
080600 H-READ-WDT1G1 SECTION.                                                   
080700     MOVE PARM-KDPRIO-PF  TO W-KDPRIO-WDT1G-MIN                           
080800                             W-KDPRIO-WDT1G-MAX                           
080900                                                                          
081000     IF PARM-KDSTAPF = ' ' OR '+'                                         
081100       MOVE LOW-VALUE  TO W-KDSTAPF-WDT1G-MIN                             
081200       MOVE HIGH-VALUE TO W-KDSTAPF-WDT1G-MAX                             
081300     ELSE                                                                 
081400       MOVE PARM-KDSTAPF  TO W-KDSTAPF-WDT1G-MIN                          
081500                          W-KDSTAPF-WDT1G-MAX                             
081600     END-IF                                                               
081700                                                                          
081800     IF PARM-ADLAGOMR-FOM IS NUMERIC                                      
081900       MOVE PARM-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1G-MIN                     
082000                                 W-ADLAGFOM-WDT1G-MAX                     
082100     END-IF                                                               
082200                                                                          
082300     IF ((PARM-KDPRIO-PF > SPACES  AND                                    
082400        PARM-ADLAGOMR-FOM IS NUMERIC AND                                  
082500        (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                               
082600        OR                                                                
082700        (PARM-KDPRIO-PF  > SPACES AND                                     
082800        (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                              
082900       PERFORM IMS-GU-WDT1G1-STATUS                                       
083000     ELSE                                                                 
083100       IF ((PARM-IDUSER > SPACES AND                                      
083200          PARM-ADLAGOMR-FOM IS NUMERIC)                                   
083300          OR                                                              
083400          PARM-KDPRIO-PF > SPACES)                                        
083500         PERFORM IMS-GU-WDT1G1                                            
083600       END-IF                                                             
083700     END-IF                                                               
083800                                                                          
083900     IF SEGMENT-FOUND                                                     
084000       MOVE SEQG-IDWDT101     TO W-WDT101KY-X                             
084100       PERFORM IMS-GU-WDT101                                              
084200       PERFORM S1-MOVE-FROM-WDT101                                        
084300       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
084400         IF ((PARM-KDPRIO-PF > SPACES  AND                                
084500            PARM-ADLAGOMR-FOM IS NUMERIC AND                              
084600            (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                           
084700            OR                                                            
084800            (PARM-KDPRIO-PF > SPACES AND                                  
084900            (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                          
085000           PERFORM IMS-GN-WDT1G1-STATUS                                   
085100         ELSE                                                             
085200           IF ((PARM-KDPRIO-PF > SPACES AND                               
085300              PARM-ADLAGOMR-FOM IS NUMERIC)                               
085400              OR                                                          
085500              PARM-KDPRIO-PF > SPACES)                                    
085600             PERFORM IMS-GN-WDT1G1                                        
085700           END-IF                                                         
085800         END-IF                                                           
085900         IF SEGMENT-FOUND                                                 
086000           MOVE SEQG-IDWDT101     TO W-WDT101KY-X                         
086100           PERFORM IMS-GU-WDT101                                          
086200           PERFORM S1-MOVE-FROM-WDT101                                    
086300         END-IF                                                           
086400       END-PERFORM                                                        
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800 I-READ-WDT1H1 SECTION.                                                   
086900     MOVE PARM-KDPRIO-PF  TO W-KDPRIO-WDT1H-MIN                           
087000                             W-KDPRIO-WDT1H-MAX                           
087100                                                                          
087200     IF PARM-KDSTAPF = ' ' OR '+'                                         
087300       MOVE LOW-VALUE  TO W-KDSTAPF-WDT1H-MIN                             
087400       MOVE HIGH-VALUE TO W-KDSTAPF-WDT1H-MAX                             
087500     ELSE                                                                 
087600       MOVE PARM-KDSTAPF  TO W-KDSTAPF-WDT1H-MIN                          
087700                          W-KDSTAPF-WDT1H-MAX                             
087800     END-IF                                                               
087900                                                                          
088000     IF PARM-ADLAGOMR-TOM IS NUMERIC                                      
088100       MOVE PARM-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1H-MIN                     
088200                              W-ADLAGTOM-WDT1H-MAX                        
088300     END-IF                                                               
088400                                                                          
088500     IF ((PARM-KDPRIO-PF > SPACES  AND                                    
088600        PARM-ADLAGOMR-TOM IS NUMERIC AND                                  
088700        (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                               
088800        OR                                                                
088900        (PARM-KDPRIO-PF  > SPACES AND                                     
089000        (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                              
089100       PERFORM IMS-GU-WDT1H1-STATUS                                       
089200     ELSE                                                                 
089300       IF ((PARM-IDUSER > SPACES AND                                      
089400          PARM-ADLAGOMR-TOM IS NUMERIC)                                   
089500          OR                                                              
089600          PARM-KDPRIO-PF > SPACES)                                        
089700         PERFORM IMS-GU-WDT1H1                                            
089800       END-IF                                                             
089900     END-IF                                                               
090000                                                                          
090100     IF SEGMENT-FOUND                                                     
090200       MOVE SEQH-IDWDT101     TO W-WDT101KY-X                             
090300       PERFORM IMS-GU-WDT101                                              
090400       PERFORM S1-MOVE-FROM-WDT101                                        
090500       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
090600         IF ((PARM-KDPRIO-PF > SPACES  AND                                
090700            PARM-ADLAGOMR-TOM IS NUMERIC AND                              
090800            (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                           
090900            OR                                                            
091000            (PARM-KDPRIO-PF > SPACES AND                                  
091100            (PARM-KDSTAPF = 'R' OR 'V' OR 'L')))                          
091200           PERFORM IMS-GN-WDT1H1-STATUS                                   
091300         ELSE                                                             
091400           IF ((PARM-KDPRIO-PF > SPACES AND                               
091500              PARM-ADLAGOMR-TOM IS NUMERIC)                               
091600              OR                                                          
091700              PARM-KDPRIO-PF > SPACES)                                    
091800             PERFORM IMS-GN-WDT1H1                                        
091900           END-IF                                                         
092000         END-IF                                                           
092100         IF SEGMENT-FOUND                                                 
092200           MOVE SEQH-IDWDT101     TO W-WDT101KY-X                         
092300           PERFORM IMS-GU-WDT101                                          
092400           PERFORM S1-MOVE-FROM-WDT101                                    
092500         END-IF                                                           
092600       END-PERFORM                                                        
092700     END-IF                                                               
092800     .                                                                    
092900     EJECT                                                                
092910 K-READ-WDT1I1 SECTION.                                                   
092920     IF PARM-ADLAGOMR-FOM IS NUMERIC AND                                  
092930        PARM-ADLAGOMR-TOM IS NUMERIC                                      
092931       MOVE PARM-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1I-MIN                     
092932                                 W-ADLAGFOM-WDT1I-MAX                     
092933       MOVE PARM-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1I-MIN                     
092934                                 W-ADLAGTOM-WDT1I-MAX                     
092935       IF PARM-KDSTAPF = 'R' OR 'V' OR 'L'                                
092936         MOVE PARM-KDSTAPF    TO W-KDSTAPF-WDT1I-MIN                      
092937                                 W-KDSTAPF-WDT1I-MAX                      
092938         PERFORM IMS-GU-WDT1I1-STATUS                                     
092939       ELSE                                                               
092940         PERFORM IMS-GU-WDT1I1                                            
092941       END-IF                                                             
092942     END-IF                                                               
092943                                                                          
092944     IF SEGMENT-FOUND                                                     
092945       MOVE SEQI-IDWDT101     TO W-WDT101KY-X                             
092946       PERFORM IMS-GU-WDT101                                              
092947       PERFORM S1-MOVE-FROM-WDT101                                        
092948       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
092949         IF (PARM-ADLAGOMR-FOM IS NUMERIC AND                             
092950            PARM-ADLAGOMR-TOM IS NUMERIC AND                              
092951            (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                           
092952            PERFORM IMS-GN-WDT1I1-STATUS                                  
092953         ELSE                                                             
092954            PERFORM IMS-GN-WDT1I1                                         
092955         END-IF                                                           
092958         IF SEGMENT-FOUND                                                 
092959           MOVE SEQI-IDWDT101     TO W-WDT101KY-X                         
092960           PERFORM IMS-GU-WDT101                                          
092961           PERFORM S1-MOVE-FROM-WDT101                                    
092962         END-IF                                                           
092963       END-PERFORM                                                        
092964     END-IF                                                               
092965     .                                                                    
092970     EJECT                                                                
093000 J-READ-WDT101 SECTION.                                                   
093100     IF PARM-IDARTNR IS NUMERIC AND PARM-IDARTNR NOT = 0                  
093300       MOVE PARM-IDARTNR   TO W-IDARTNR-WDT101-MIN                        
093400                              W-IDARTNR-WDT101-MAX                        
093500       IF PARM-ADLAGOMR-FOM IS NUMERIC                                    
093600         MOVE PARM-ADLAGOMR-FOM TO W-ADLAGOMR-FOM-N                       
093700       ELSE                                                               
093800         IF PARM-ADLAGOMR-TOM IS NUMERIC                                  
093900           MOVE PARM-ADLAGOMR-TOM TO W-ADLAGOMR-TOM-N                     
094000         END-IF                                                           
094100       END-IF                                                             
094200     END-IF                                                               
094300                                                                          
094400     IF (PARM-IDARTNR > 0    AND                                          
094500        PARM-ADLAGOMR-FOM IS NUMERIC AND                                  
094600        (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                               
094700       PERFORM IMS-GU-WDT101-ADLAGFOM-STA                                 
094800     ELSE                                                                 
094900       IF PARM-IDARTNR > 0    AND                                         
095000          PARM-ADLAGOMR-FOM IS NUMERIC                                    
095100         PERFORM IMS-GU-WDT101-ADLAGFOM                                   
095200       ELSE                                                               
095300         IF (PARM-IDARTNR > 0    AND                                      
095400            PARM-ADLAGOMR-TOM IS NUMERIC AND                              
095500            (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                           
095600           PERFORM IMS-GU-WDT101-ADLAGTOM-STA                             
095700         ELSE                                                             
095800           IF PARM-IDARTNR > 0   AND                                      
095900              PARM-ADLAGOMR-TOM IS NUMERIC                                
096000             PERFORM IMS-GU-WDT101-ADLAGTOM                               
096100           ELSE                                                           
096200             IF (PARM-IDARTNR > 0 AND                                     
096300                (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                       
096400               PERFORM IMS-GU-WDT101-STATUS                               
096500             ELSE                                                         
096600               IF PARM-IDARTNR > 0                                        
096700                 PERFORM IMS-GU-WDT101-1                                  
096800               END-IF                                                     
096900             END-IF                                                       
097000           END-IF                                                         
097100         END-IF                                                           
097200       END-IF                                                             
097300     END-IF                                                               
097400                                                                          
100700     IF SEGMENT-FOUND                                                     
100800       MOVE PF-IDDC             TO W-IDDC-WDT101                          
100900       MOVE PF-IDARTNR          TO W-IDARTNR-WDT101                       
101000       MOVE PF-TIORDTIME        TO W-TIORDTIME-WDT101                     
102100       IF PF-KDSTAPF NOT = 'A'                                            
102200         PERFORM S1-MOVE-FROM-WDT101                                      
102400       END-IF                                                             
103100       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
103200         IF SEGMENT-FOUND                                                 
103700           IF (PARM-IDARTNR > 0   AND                                     
103800              PARM-ADLAGOMR-FOM IS NUMERIC AND                            
103900              (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                         
104000             PERFORM IMS-GN-WDT101-ADLAGFOM-STA                           
104100           ELSE                                                           
104200             IF (PARM-IDARTNR > 0      AND                                
104300                PARM-ADLAGOMR-TOM IS NUMERIC AND                          
104400                (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                       
104500               PERFORM IMS-GN-WDT101-ADLAGTOM-STA                         
104600             ELSE                                                         
104700               IF PARM-IDARTNR > 0     AND                                
104800                 PARM-ADLAGOMR-FOM IS NUMERIC                             
104900                 PERFORM IMS-GN-WDT101-ADLAGFOM                           
105000               ELSE                                                       
105100                 IF PARM-IDARTNR > 0   AND                                
105200                   PARM-ADLAGOMR-TOM IS NUMERIC                           
105300                   PERFORM IMS-GN-WDT101-ADLAGTOM                         
105400                 ELSE                                                     
105500                   IF (PARM-IDARTNR > 0 AND                               
105600                      (PARM-KDSTAPF = 'R' OR 'V' OR 'L'))                 
105700                     PERFORM IMS-GN-WDT101-STATUS                         
105800                   ELSE                                                   
105900                     IF PARM-IDARTNR > 0                                  
106000                       PERFORM IMS-GN-WDT101                              
106100                     END-IF                                               
106200                   END-IF                                                 
106300                 END-IF                                                   
106400               END-IF                                                     
106500             END-IF                                                       
106600           END-IF                                                         
106700         END-IF                                                           
106800         IF SEGMENT-FOUND AND PF-KDSTAPF NOT = 'A'                        
106900           PERFORM S1-MOVE-FROM-WDT101                                    
107000         END-IF                                                           
107400       END-PERFORM                                                        
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 S1-MOVE-FROM-WDT101 SECTION.                                             
107900     MOVE PF-IDARTNR       TO W001-IDARTNR                                
108000     MOVE PF-ADLAGOMR-FOM  TO W001-ADLAGOMR-FOM                           
108100     MOVE PF-ADGANG-FOM    TO W001-ADGANG-FOM                             
108200     MOVE PF-ADPLATS-FOM   TO W001-ADPLATS-FOM                            
108300     MOVE PF-KVBEST        TO W001-KVBEST                                 
108400     MOVE PF-ADLAGOMR-TOM  TO W001-ADLAGOMR-TOM                           
108500     MOVE PF-ADGANG-TOM    TO W001-ADGANG-TOM                             
108600     MOVE PF-ADPLATS-TOM   TO W001-ADPLATS-TOM                            
108700     MOVE PF-KVBEST-ANDR   TO W001-KVBEST-ANDR                            
108800     MOVE PF-IDUSER        TO W001-IDUSER                                 
108900     MOVE PF-KDSTAPF       TO W001-STATUS                                 
108910     IF PF-KDPRIO-PF = 1                                                  
109000       MOVE 'J'            TO W001-KDPRIO-PF                              
109100     ELSE                                                                 
109101       IF PF-KDPRIO-PF = 2                                                
109110         MOVE 'N'          TO W001-KDPRIO-PF                              
109120       END-IF                                                             
109130     END-IF                                                               
109200     MOVE PF-IDARTNR       TO W-IDARTNR                                   
109400     PERFORM IMS-GU-WDK611                                                
109500     IF SEGMENT-FOUND                                                     
109600       MOVE CLAG-KVQPACK-3 TO W001-KVQPACK-3                              
109700     END-IF                                                               
109900     PERFORM S21-WRITE-W61328-001                                         
110000     .                                                                    
110100     EJECT                                                                
110200 Z-FINIT SECTION.                                                         
110300     CLOSE W613PP                                                         
110400           W61328-001                                                     
110500     SKIP2                                                                
110600     MOVE 'S' TO POSTSUM-OPKOD                                            
110700     CALL POSTSUM USING POSTSUM-PARM                                      
110800     .                                                                    
110900     EJECT                                                                
111000 S01-READ-W613PP  SECTION.                                                
111100     READ W613PP INTO PARM-AREA                                           
111200     AT END                                                               
111300        MOVE HIGH-VALUE TO PARM-AREA                                      
111400        SET END-OF-W613PP TO TRUE                                         
111500                                                                          
111600     NOT AT END                                                           
111700        MOVE 'W613PP' TO POSTSUM-FDNAMN                                   
111800        MOVE 'W61328D1' TO POSTSUM-DDNAMN2                                
111900*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
112000        MOVE SPACES      TO POSTSUM-TRANSTYP                              
112100        CALL POSTSUM USING POSTSUM-PARM                                   
112200     END-READ                                                             
112300     .                                                                    
112400     EJECT                                                                
112500 S21-WRITE-W61328-001  SECTION.                                           
112700     MOVE 1 TO W001-SKIP                                                  
112800     IF W001-LINECOUNTER > W001-MAX-LINES-PER-PAGE                        
112900       MOVE TODAYS-DATE  TO W001-DATE                                     
113000       MOVE 'W61234-001' TO W001-LISTNR                                   
113100                                                                          
113200       PERFORM S21A-WRITE-HEADERS                                         
113300     END-IF                                                               
113400     SKIP2                                                                
113500     MOVE W001-DETAIL1 TO W001-LINE                                       
113600     WRITE W61328-001-LINE FROM W001-LINE AFTER W001-SKIP                 
113700     SKIP2                                                                
113800     MOVE SPACE TO W001-LINE                                              
113900     ADD  +1 TO W001-LINECOUNTER                                          
114000     .                                                                    
114100     EJECT                                                                
114200 S21A-WRITE-HEADERS SECTION.                                              
114400     ADD +1 TO W001-PAGECOUNTER                                           
114500     MOVE W001-PAGECOUNTER TO W001-PAGE                                   
114600     WRITE W61328-001-LINE FROM W001-HEADER1 AFTER PAGE                   
114700     WRITE W61328-001-LINE FROM W001-HEADER2 AFTER 2                      
114800     MOVE +7 TO W001-LINECOUNTER                                          
114900     SKIP2                                                                
115000     MOVE 3 TO W001-SKIP                                                  
115100     .                                                                    
115200     EJECT                                                                
115300 S99-ABEND SECTION.                                                       
115500     SKIP2                                                                
115600     MOVE 'S' TO POSTSUM-OPKOD                                            
115700     CALL POSTSUM USING POSTSUM-PARM                                      
115800     CALL ABEND USING RKOD-ABEND                                          
115900     .                                                                    
116000     EJECT                                                                
116100* --- IMS SECTIONS  ---                                                   
116200                                                                          
116300     EJECT                                                                
116400 IMS-GU-WDT1A1 SECTION.                                                   
116600     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
116700                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X ')'                    
116800          DELIMITED BY SIZE INTO SSA1                                     
116900     MOVE '  GE' TO GOOD-STATUSCODES                                      
117000     CALL CBLTDLI USING GU WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
117100     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
117200     PERFORM IMS-STATUSCHECK                                              
117300     .                                                                    
117400     SKIP3                                                                
117500 IMS-GU-WDT1A1-STATUS SECTION.                                            
117700     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
117800                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X                        
117900                    '&KDSTAPF  =' PARM-KDSTAPF     ')'                    
118000          DELIMITED BY SIZE INTO SSA1                                     
118100     MOVE '  GE' TO GOOD-STATUSCODES                                      
118200     CALL CBLTDLI USING GU WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
118300     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
118400     PERFORM IMS-STATUSCHECK                                              
118500     .                                                                    
118600     SKIP3                                                                
118700 IMS-GN-WDT1A1 SECTION.                                                   
118900     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
119000                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X ')'                    
119100          DELIMITED BY SIZE INTO SSA1                                     
119200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
119300     CALL CBLTDLI USING GN WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
119400     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
119500     PERFORM IMS-STATUSCHECK                                              
119600     .                                                                    
119700     SKIP3                                                                
119800 IMS-GN-WDT1A1-STATUS SECTION.                                            
120000     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
120100                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X                        
120200                    '&KDSTAPF  =' PARM-KDSTAPF     ')'                    
120300          DELIMITED BY SIZE INTO SSA1                                     
120400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
120500     CALL CBLTDLI USING GN WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
120600     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
120700     PERFORM IMS-STATUSCHECK                                              
120800     .                                                                    
120900     SKIP3                                                                
121000 IMS-GU-WDT1B1 SECTION.                                                   
121200     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
121300                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X ')'                    
121400          DELIMITED BY SIZE INTO SSA1                                     
121500     MOVE '  GE' TO GOOD-STATUSCODES                                      
121600     CALL CBLTDLI USING GU WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
121700     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
121800     PERFORM IMS-STATUSCHECK                                              
121900     .                                                                    
122000     SKIP3                                                                
122100 IMS-GU-WDT1B1-STATUS SECTION.                                            
122300     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
122400                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X                        
122500                    '&KDSTAPF  =' PARM-KDSTAPF     ')'                    
122600          DELIMITED BY SIZE INTO SSA1                                     
122700     MOVE '  GE' TO GOOD-STATUSCODES                                      
122800     CALL CBLTDLI USING GU WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
122900     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
123000     PERFORM IMS-STATUSCHECK                                              
123100     .                                                                    
123200     SKIP3                                                                
123300 IMS-GN-WDT1B1 SECTION.                                                   
123500     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
123600                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X ')'                    
123700          DELIMITED BY SIZE INTO SSA1                                     
123800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
123900     CALL CBLTDLI USING GN WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
124000     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
124100     PERFORM IMS-STATUSCHECK                                              
124200     .                                                                    
124300     SKIP3                                                                
124400 IMS-GN-WDT1B1-STATUS SECTION.                                            
124500                                                                          
124600     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
124700                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X                        
124800                    '&KDSTAPF  =' PARM-KDSTAPF     ')'                    
124900          DELIMITED BY SIZE INTO SSA1                                     
125000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
125100     CALL CBLTDLI USING GN WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
125200     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
125300     PERFORM IMS-STATUSCHECK                                              
125400     .                                                                    
125500     SKIP3                                                                
125600 IMS-GU-WDT1C1 SECTION.                                                   
125700     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
125800                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X ')'                    
125900          DELIMITED BY SIZE INTO SSA1                                     
126000     MOVE '  GE' TO GOOD-STATUSCODES                                      
126100     CALL CBLTDLI USING GU WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
126200     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
126300     PERFORM IMS-STATUSCHECK                                              
126400     .                                                                    
126500     SKIP3                                                                
126600 IMS-GN-WDT1C1 SECTION.                                                   
126700     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
126800                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X ')'                    
126900          DELIMITED BY SIZE INTO SSA1                                     
127000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
127100     CALL CBLTDLI USING GN WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
127200     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
127300     PERFORM IMS-STATUSCHECK                                              
127400     .                                                                    
127500     SKIP3                                                                
127510 IMS-GU-WDT1C1-STATUS SECTION.                                            
127520     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
127530                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X                        
127540                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
127550          DELIMITED BY SIZE INTO SSA1                                     
127560     MOVE '  GE' TO GOOD-STATUSCODES                                      
127570     CALL CBLTDLI USING GU WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
127580     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
127590     PERFORM IMS-STATUSCHECK                                              
127591     .                                                                    
127592     SKIP3                                                                
127593 IMS-GN-WDT1C1-STATUS SECTION.                                            
127594     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
127595                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X                        
127596                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
127597          DELIMITED BY SIZE INTO SSA1                                     
127598     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
127599     CALL CBLTDLI USING GN WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
127600     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
127601     PERFORM IMS-STATUSCHECK                                              
127602     .                                                                    
127603     SKIP3                                                                
127610 IMS-GU-WDT1D1 SECTION.                                                   
127700     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
127800                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X ')'                    
127900          DELIMITED BY SIZE INTO SSA1                                     
128000     MOVE '  GE' TO GOOD-STATUSCODES                                      
128100     CALL CBLTDLI USING GU WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
128200     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
128300     PERFORM IMS-STATUSCHECK                                              
128400     .                                                                    
128500     SKIP3                                                                
128600 IMS-GN-WDT1D1 SECTION.                                                   
128700     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
128800                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X ')'                    
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
129100     CALL CBLTDLI USING GN WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
129200     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
129300     PERFORM IMS-STATUSCHECK                                              
129400     .                                                                    
129500     SKIP3                                                                
129510 IMS-GU-WDT1D1-STATUS SECTION.                                            
129520     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
129530                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X                        
129540                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
129550          DELIMITED BY SIZE INTO SSA1                                     
129560     MOVE '  GE' TO GOOD-STATUSCODES                                      
129570     CALL CBLTDLI USING GU WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
129580     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
129590     PERFORM IMS-STATUSCHECK                                              
129591     .                                                                    
129592     SKIP3                                                                
129593 IMS-GN-WDT1D1-STATUS SECTION.                                            
129594     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
129595                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X                        
129596                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
129597          DELIMITED BY SIZE INTO SSA1                                     
129598     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
129599     CALL CBLTDLI USING GN WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
129600     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
129601     PERFORM IMS-STATUSCHECK                                              
129602     .                                                                    
129603     SKIP3                                                                
129610 IMS-GU-WDT1E1 SECTION.                                                   
129700     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
129800                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X ')'                    
129900          DELIMITED BY SIZE INTO SSA1                                     
130000     MOVE '  GE' TO GOOD-STATUSCODES                                      
130100     CALL CBLTDLI USING GU WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
130200     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
130300     PERFORM IMS-STATUSCHECK                                              
130400     .                                                                    
130500     SKIP3                                                                
130600 IMS-GN-WDT1E1 SECTION.                                                   
130700     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
130800                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X ')'                    
130900          DELIMITED BY SIZE INTO SSA1                                     
131000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
131100     CALL CBLTDLI USING GN WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
131200     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
131300     PERFORM IMS-STATUSCHECK                                              
131400     .                                                                    
131500     SKIP3                                                                
131600 IMS-GU-WDT1E1-STATUS SECTION.                                            
131700     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
131800                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X                        
131900                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
132000          DELIMITED BY SIZE INTO SSA1                                     
132100     MOVE '  GE' TO GOOD-STATUSCODES                                      
132200     CALL CBLTDLI USING GU WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
132300     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
132400     PERFORM IMS-STATUSCHECK                                              
132500     .                                                                    
132600     SKIP3                                                                
132700 IMS-GN-WDT1E1-STATUS SECTION.                                            
132800     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
132900                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X                        
133000                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
133100          DELIMITED BY SIZE INTO SSA1                                     
133200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
133300     CALL CBLTDLI USING GN WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
133400     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
133500     PERFORM IMS-STATUSCHECK                                              
133600     .                                                                    
133700     SKIP3                                                                
133800 IMS-GU-WDT1F1 SECTION.                                                   
133900     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
134000                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X ')'                    
134100          DELIMITED BY SIZE INTO SSA1                                     
134200     MOVE '  GE' TO GOOD-STATUSCODES                                      
134300     CALL CBLTDLI USING GU WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
134400     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
134500     PERFORM IMS-STATUSCHECK                                              
134600     .                                                                    
134700     SKIP3                                                                
134800 IMS-GN-WDT1F1 SECTION.                                                   
134900     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
135000                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X ')'                    
135100          DELIMITED BY SIZE INTO SSA1                                     
135200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
135300     CALL CBLTDLI USING GN WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
135400     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
135500     PERFORM IMS-STATUSCHECK                                              
135600     .                                                                    
135700     SKIP3                                                                
135800 IMS-GU-WDT1F1-STATUS SECTION.                                            
135900     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
136000                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X                        
136100                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
136200          DELIMITED BY SIZE INTO SSA1                                     
136300     MOVE '  GE' TO GOOD-STATUSCODES                                      
136400     CALL CBLTDLI USING GU WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
136500     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
136600     PERFORM IMS-STATUSCHECK                                              
136700     .                                                                    
136800     SKIP3                                                                
136900 IMS-GN-WDT1F1-STATUS SECTION.                                            
137000     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
137100                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X                        
137200                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
137300          DELIMITED BY SIZE INTO SSA1                                     
137400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
137500     CALL CBLTDLI USING GN WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
137600     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
137700     PERFORM IMS-STATUSCHECK                                              
137800     .                                                                    
137900     SKIP3                                                                
138000 IMS-GU-WDT1G1 SECTION.                                                   
138100     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
138200                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X ')'                    
138300          DELIMITED BY SIZE INTO SSA1                                     
138400     MOVE '  GE' TO GOOD-STATUSCODES                                      
138500     CALL CBLTDLI USING GU WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
138600     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
138700     PERFORM IMS-STATUSCHECK                                              
138800     .                                                                    
138900     SKIP3                                                                
139000 IMS-GN-WDT1G1 SECTION.                                                   
139100     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
139200                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X ')'                    
139300          DELIMITED BY SIZE INTO SSA1                                     
139400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
139500     CALL CBLTDLI USING GN WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
139600     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
139700     PERFORM IMS-STATUSCHECK                                              
139800     .                                                                    
139900     SKIP3                                                                
140000 IMS-GU-WDT1G1-STATUS SECTION.                                            
140100     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
140200                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X                        
140300                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
140400          DELIMITED BY SIZE INTO SSA1                                     
140500     MOVE '  GE' TO GOOD-STATUSCODES                                      
140600     CALL CBLTDLI USING GU WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
140700     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
140800     PERFORM IMS-STATUSCHECK                                              
140900     .                                                                    
141000     SKIP3                                                                
141100 IMS-GN-WDT1G1-STATUS SECTION.                                            
141200     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
141300                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X                        
141400                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
141500          DELIMITED BY SIZE INTO SSA1                                     
141600     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
141700     CALL CBLTDLI USING GN WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
141800     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
141900     PERFORM IMS-STATUSCHECK                                              
142000     .                                                                    
142100     SKIP3                                                                
142200 IMS-GU-WDT1H1 SECTION.                                                   
142300     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
142400                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X ')'                    
142500          DELIMITED BY SIZE INTO SSA1                                     
142600     MOVE '  GE' TO GOOD-STATUSCODES                                      
142700     CALL CBLTDLI USING GU WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
142800     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
142900     PERFORM IMS-STATUSCHECK                                              
143000     .                                                                    
143100     SKIP3                                                                
143200 IMS-GN-WDT1H1 SECTION.                                                   
143300     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
143400                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X ')'                    
143500          DELIMITED BY SIZE INTO SSA1                                     
143600     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
143700     CALL CBLTDLI USING GN WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
143800     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
143900     PERFORM IMS-STATUSCHECK                                              
144000     .                                                                    
144100     SKIP3                                                                
144200 IMS-GU-WDT1H1-STATUS SECTION.                                            
144300     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
144400                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X                        
144500                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
144600          DELIMITED BY SIZE INTO SSA1                                     
144700     MOVE '  GE' TO GOOD-STATUSCODES                                      
144800     CALL CBLTDLI USING GU WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
144900     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
145000     PERFORM IMS-STATUSCHECK                                              
145100     .                                                                    
145200     SKIP3                                                                
145300 IMS-GN-WDT1H1-STATUS SECTION.                                            
145400     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
145500                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X                        
145600                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
145700          DELIMITED BY SIZE INTO SSA1                                     
145800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
145900     CALL CBLTDLI USING GN WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
146000     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
146100     PERFORM IMS-STATUSCHECK                                              
146200     .                                                                    
146300     SKIP3                                                                
146310 IMS-GU-WDT1I1 SECTION.                                                   
146320     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
146330                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X ')'                    
146340          DELIMITED BY SIZE INTO SSA1                                     
146350     MOVE '  GE' TO GOOD-STATUSCODES                                      
146360     CALL CBLTDLI USING GU WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
146370     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
146380     PERFORM IMS-STATUSCHECK                                              
146390     .                                                                    
146391     SKIP3                                                                
146392 IMS-GN-WDT1I1 SECTION.                                                   
146393     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
146394                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X ')'                    
146395          DELIMITED BY SIZE INTO SSA1                                     
146396     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
146397     CALL CBLTDLI USING GN WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
146398     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
146399     PERFORM IMS-STATUSCHECK                                              
146400     .                                                                    
146401     SKIP3                                                                
146402 IMS-GU-WDT1I1-STATUS SECTION.                                            
146403     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
146404                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X                        
146405                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
146406          DELIMITED BY SIZE INTO SSA1                                     
146407     MOVE '  GE' TO GOOD-STATUSCODES                                      
146408     CALL CBLTDLI USING GU WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
146409     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
146410     PERFORM IMS-STATUSCHECK                                              
146411     .                                                                    
146412     SKIP3                                                                
146413 IMS-GN-WDT1I1-STATUS SECTION.                                            
146414     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
146415                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X                        
146416                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
146417          DELIMITED BY SIZE INTO SSA1                                     
146418     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
146419     CALL CBLTDLI USING GN WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
146420     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
146421     PERFORM IMS-STATUSCHECK                                              
146422     .                                                                    
146423     SKIP3                                                                
146430 IMS-GU-WDT101 SECTION.                                                   
146500                                                                          
146600     STRING 'WDT101  (WDT101KY =' W-WDT101KY-X ')'                        
146700          DELIMITED BY SIZE INTO SSA1                                     
146800     MOVE '  GE' TO GOOD-STATUSCODES                                      
146900     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
147000     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
147100     PERFORM IMS-STATUSCHECK                                              
147200     .                                                                    
147300     SKIP3                                                                
147400 IMS-GU-WDT101-1 SECTION.                                                 
147500     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
147600                    '&WDT101KY<=' W-WDT101KY-MAX-X ')'                    
147700          DELIMITED BY SIZE INTO SSA1                                     
147800     MOVE '  GE' TO GOOD-STATUSCODES                                      
147900     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
148000     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
148100     PERFORM IMS-STATUSCHECK                                              
148200     .                                                                    
148300     EJECT                                                                
148700 IMS-GN-WDT101 SECTION.                                                   
148800     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
148900                    '&WDT101KY<=' W-WDT101KY-MAX-X ')'                    
149000          DELIMITED BY SIZE INTO SSA1                                     
149100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
149200     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
149300     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
149400     PERFORM IMS-STATUSCHECK                                              
149500     .                                                                    
149600     SKIP3                                                                
149700 IMS-GU-WDT101-STATUS SECTION.                                            
149800     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
149900                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
150000                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
150100          DELIMITED BY SIZE INTO SSA1                                     
150200     MOVE '  GE' TO GOOD-STATUSCODES                                      
150300     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
150400     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
150500     PERFORM IMS-STATUSCHECK                                              
150600     .                                                                    
150700     EJECT                                                                
150800 IMS-GN-WDT101-STATUS SECTION.                                            
150900     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
151000                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
151100                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
151200          DELIMITED BY SIZE INTO SSA1                                     
151300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
151400     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
151500     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
151600     PERFORM IMS-STATUSCHECK                                              
151700     .                                                                    
151800     SKIP3                                                                
151900 IMS-GU-WDT101-ADLAGFOM SECTION.                                          
152000     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
152100                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
152200                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X ')'                    
152300          DELIMITED BY SIZE INTO SSA1                                     
152400     MOVE '  GE' TO GOOD-STATUSCODES                                      
152500     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
152600     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
152700     PERFORM IMS-STATUSCHECK                                              
152800     .                                                                    
152900     EJECT                                                                
153000 IMS-GN-WDT101-ADLAGFOM SECTION.                                          
153100     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
153200                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
153300                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X ')'                    
153400          DELIMITED BY SIZE INTO SSA1                                     
153500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
153600     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
153700     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
153800     PERFORM IMS-STATUSCHECK                                              
153900     .                                                                    
154000     SKIP3                                                                
154100 IMS-GU-WDT101-ADLAGFOM-STA SECTION.                                      
154200     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
154300                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
154400                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X                        
154500                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
154600          DELIMITED BY SIZE INTO SSA1                                     
154700     MOVE '  GE' TO GOOD-STATUSCODES                                      
154800     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
154900     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
155000     PERFORM IMS-STATUSCHECK                                              
155100     .                                                                    
155200     EJECT                                                                
155300 IMS-GN-WDT101-ADLAGFOM-STA SECTION.                                      
155400     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
155500                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
155600                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X                        
155700                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
155800          DELIMITED BY SIZE INTO SSA1                                     
155900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
156000     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
156100     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSCHECK                                              
156300     .                                                                    
156400     SKIP3                                                                
156500 IMS-GU-WDT101-ADLAGTOM SECTION.                                          
156600     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
156700                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
156800                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X ')'                    
156900          DELIMITED BY SIZE INTO SSA1                                     
157000     MOVE '  GE' TO GOOD-STATUSCODES                                      
157100     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
157200     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
157300     PERFORM IMS-STATUSCHECK                                              
157400     .                                                                    
157500     EJECT                                                                
157600 IMS-GN-WDT101-ADLAGTOM SECTION.                                          
157700     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
157800                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
157900                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X ')'                    
158000          DELIMITED BY SIZE INTO SSA1                                     
158100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
158200     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
158300     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
158400     PERFORM IMS-STATUSCHECK                                              
158500     .                                                                    
158600     SKIP3                                                                
158700 IMS-GU-WDT101-ADLAGTOM-STA SECTION.                                      
158800     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
158900                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
159000                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X                        
159100                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
159200          DELIMITED BY SIZE INTO SSA1                                     
159300     MOVE '  GE' TO GOOD-STATUSCODES                                      
159400     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
159500     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
159600     PERFORM IMS-STATUSCHECK                                              
159700     .                                                                    
159800     EJECT                                                                
159900 IMS-GN-WDT101-ADLAGTOM-STA SECTION.                                      
160000     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
160100                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
160200                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X                        
160300                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
160400          DELIMITED BY SIZE INTO SSA1                                     
160500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
160600     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
160700     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
160800     PERFORM IMS-STATUSCHECK                                              
160900     .                                                                    
161000     SKIP3                                                                
161100 IMS-GU-WDK611 SECTION.                                                   
161200     MOVE SPACES TO SSA1                                                  
161300     MOVE SPACES TO SSA2                                                  
161400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
161500          DELIMITED BY SIZE INTO SSA1                                     
161600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
161700          DELIMITED BY SIZE INTO SSA2                                     
161800     MOVE '  GE' TO GOOD-STATUSCODES                                      
161900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
162000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162100     PERFORM IMS-STATUSCHECK                                              
162200     .                                                                    
162300     EJECT                                                                
162400 IMS-STATUSCHECK SECTION.                                                 
162500                                                                          
162600     SET STATUS-IX TO 1                                                   
162700     SEARCH GOOD-STATUS                                                   
162800       AT END                                                             
162900         STRING ' INVALID STATUS CODE FROIM IMS:' STATUS-WS               
163000           DELIMITED BY SIZE INTO ERROR-TEXT                              
163100         DISPLAY ERROR-TEXT                                               
163200         CALL FELLOG                                                      
163300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
163400         CONTINUE                                                         
164000     END-SEARCH                                                           
170000     .                                                                    
