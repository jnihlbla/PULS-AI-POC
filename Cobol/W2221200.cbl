000400 ID DIVISION.                                                             
000500 PROGRAM-ID.         W2221200.                                            
000900*AUTHOR.             JAN CARLSSON, DATA LOGIC AB GÖTEBORG.                
001000*DATE-WRITTEN.       OKTOBER 1978.                                        
001100*    SKIP2                                                                
001200*                                                                         
001500*                                                                         
001600*    REMARKS.                                                             
001700*    FUNKTION.                                                            
001800*            PROGRAMMET UPPDATERAR PROGNOSER                              
001810*                                                                         
001820*            -MAJ -92   2204-HÄNDELSER LÄGGS UT PÅ FIL                    
001830*                       I STÄLLET FÖR PÅ WDG3           (PAH)             
002200*                                                                         
002300*            -AUG -96   2202-HÄNDELSER SKRIVS PÅ FIL W22214,              
002400*                       IMS-ANROPEN INLAGDA I PROGRAMMET                  
002500*                       SAMT OMGJORT TILL BMP UTAN CHECKPOINT             
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*                            ***  INPUT:  TRANSTYP RP1 - RP7              
003500     SELECT W22211    ASSIGN W22212D1.                                    
003600*                                                                         
003700*                            ***  INPUT                                   
003800     SELECT W22213    ASSIGN W22212D2.                                    
004200*                                                                         
004300*                            ***  OUTPUT: FIL MED 2202-HÄNDELSER          
004400     SELECT W22214    ASSIGN W22212D3.                                    
004401*                                                                         
004402*                            ***  OUTPUT: FIL TILL W2180200               
004403     SELECT W22216    ASSIGN W22212D4.                                    
004410*                                                                         
004420*                            ***  OUTPUT: FIL TILL W2214000               
004430     SELECT W22217    ASSIGN W22212D5.                                    
004500*                                                                         
004501*                            ***  OUTPUT: FELPOSTER                       
004510     SELECT W22211-UT ASSIGN W22212D6.                                    
004520*                                                                         
004530*                            ***  OUTPUT: RESTERANDE INPUT-POSTER         
004600     EJECT                                                                
004700 DATA DIVISION.                                                           
004800 FILE SECTION.                                                            
004900     SKIP2                                                                
005000 FD  W22211                                                               
005100     RECORDING V                                                          
005200     BLOCK 0.                                                             
005300                                                                          
005400*01  -COPY W222RP1    -L.                                                 
005600     SKIP2                                                                
005700*01  -COPY W222RP2    -L.                                                 
005900     SKIP2                                                                
006000*01  -COPY W222RP3    -L.                                                 
006200     SKIP2                                                                
006300*01  -COPY W222RP4    -L.                                                 
006500     SKIP2                                                                
006600*01  -COPY W222RP5    -L.                                                 
006800     SKIP2                                                                
006900*01  -COPY W222RP6    -L.                                                 
007100     SKIP2                                                                
007200*01  -COPY W222RP7    -L.                                                 
007400     EJECT                                                                
007500 FD  W22213                                                               
007600     RECORDING V                                                          
007700     BLOCK 0.                                                             
007800                                                                          
007900 01  W22213-POST.                                                         
008000*    03  -COPY W092W001   -L.                                             
008010     03  FILLER                  PIC X(80).                               
008100     EJECT                                                                
008110 FD  W22214                                                               
008120     RECORDING F                                                          
008130     BLOCK 0.                                                             
008140                                                                          
008141 01  W22214-POST.                                                         
008142*    03  -COPY W2222202     -L.                                           
008300     EJECT                                                                
009200 FD  W22216                                                               
009300     RECORDING F                                                          
009400     BLOCK 0.                                                             
009500                                                                          
009600 01  W22216-POST.                                                         
009700*    03  -COPY W21801     -L.                                             
009800     EJECT                                                                
009810 FD  W22217                                                               
009820     RECORDING F                                                          
009830     BLOCK 0.                                                             
009840                                                                          
009850 01  W22217-POST.                                                         
009860*    03  -COPY W2212204   -L.                                             
009900     EJECT                                                                
009910     SKIP2                                                                
009920 FD  W22211-UT                                                            
009930     RECORDING V                                                          
009940     BLOCK 0.                                                             
009950                                                                          
009960*01  POST -COPY W222RP1    -PRE UT-RP1- -L.                               
009970     SKIP2                                                                
009980*01  POST -COPY W222RP2    -PRE UT-RP2- -L.                               
009990     SKIP2                                                                
009991*01  POST -COPY W222RP3    -PRE UT-RP3- -L.                               
009992     SKIP2                                                                
009993*01  POST -COPY W222RP4    -PRE UT-RP4- -L.                               
009994     SKIP2                                                                
009995*01  POST -COPY W222RP5    -PRE UT-RP5- -L.                               
009996     SKIP2                                                                
009997*01  POST -COPY W222RP6    -PRE UT-RP6- -L.                               
009998     SKIP2                                                                
009999*01  POST -COPY W222RP7    -PRE UT-RP7- -L.                               
010000     EJECT                                                                
010100 WORKING-STORAGE SECTION.                                                 
010600     SKIP2                                                                
010601*    -COPY WY2000W3                                                       
010610     SKIP3                                                                
010700 01  RKOD                        PIC S9(4)   VALUE +0 COMP SYNC.          
010800     SKIP2                                                                
010900 01  KONSTANTER.                                                          
011000     03  JA                      PIC X       VALUE 'J'.                   
011100     03  NEJ                     PIC X       VALUE 'N'.                   
011110 77  TEST-SVAR                   PIC X(4).                                
011111                                                                          
011120 01  W-SW-ARTC26                 PIC X.                                   
011130     88  ARTC26-FINNS                        VALUE 'J'.                   
011140     88  ARTC26-SAKNAS                       VALUE 'N'.                   
011200     SKIP2                                                                
011201 01  W-SW-ART-CLAG               PIC X.                                   
011202     88  ART-CLAG-FINNS                      VALUE 'J'.                   
011203     88  ART-CLAG-SAKNAS                     VALUE 'N'.                   
011204     SKIP2                                                                
011210 01  FELTEXT.                                                             
011220     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011230     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011240                                                                          
011250 01  W-ANT-POSTER                PIC 9(4)    VALUE ZERO.                  
011260 01  W-MAX-POSTER                PIC 9(3)    VALUE 500.                   
011290                                                                          
011300 01  W-INDEX.                                                             
011400     05  SASONG-INDEX            PIC 9       COMP SYNC.                   
011500     SKIP2                                                                
011510 77  W22211-EOF-SW               PIC X       VALUE 'N'.                   
011520     88  END-OF-W22211                       VALUE 'J'.                   
011800     SKIP2                                                                
011900 01  W-DATUM-1.                                                           
012000     05  W-AAR                   PIC 99.                                  
012100     05  W-VECKA                 PIC 99.                                  
012200     05  W-DAG                   PIC 9.                                   
012300 01  W-DATUM REDEFINES W-DATUM-1 PIC 9(5).                                
012400                                                                          
012500 01  WS-PROCTAL                  PIC S9(4)V999   COMP-3.                  
012600 01  W-ABSBELOPP                 PIC S9(6)V9(1)  COMP-3.                  
012700 01  WS-2202-KVPB-SEP-GAMMAL     PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012800 01  WS-CLAG-KVPB-SEP            PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012900 01  WS-BAS-KVPB-SEP             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
013000 01  W-KVPB-SEP                  PIC S9(6)V9(1)  COMP-3.                  
013200 01  FELUTSKRIFT                 PIC X       VALUE 'N'.                   
013300 01  RESEASON-ACK                PIC S9(1)V9(2)  COMP-3.                  
013400 01  STORSTA-SASONG              PIC S9(1)V9(2)  COMP-3.                  
013500 01  MINSTA-SASONG               PIC S9(1)V9(2)  COMP-3.                  
013600 01  DIFF-SASONG                 PIC S9(1)V9(2)  COMP-3.                  
013700 01  W-AVVIKELSE                 PIC S9(4)V999   COMP-3.                  
013710     EJECT                                                                
013711*---  JÄMFÖRELSE-AREA FÖR WDK626                                          
013720*01  -COPY WDK626     -PRE JMF-.                                          
013800     EJECT                                                                
013900 01  DYNAMISKA-SUBPROGRAM.                                                
014000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
014100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
014600     SKIP2                                                                
014700*                                *** PARAMETRAR TILL DATKORT   ***        
014800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22212'.              
014900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
015000*01  -COPY WDATKORTC0.                                                    
015200     EJECT                                                                
015300*01  -COPY W0005      -PRE POSTSUM-                                       
015500     EJECT                                                                
015600*****************************************************************         
015700*    HÄR BÖRJAR INPUT-AREAN FÖR FIL W22211                      *         
015800*****************************************************************         
015900 01  IRP-AREA.                                                            
016000     05  FILLER                  PIC X(100).                              
016100     SKIP3                                                                
016200*01  A -COPY W222RP1    -PRE RP1- -RED IRP-AREA.                          
016400     EJECT                                                                
016500*01  B -COPY W222RP2    -PRE RP2- -RED IRP-AREA.                          
016700     EJECT                                                                
016800*01  C -COPY W222RP3    -PRE RP3- -RED IRP-AREA.                          
017000     EJECT                                                                
017100*01  D -COPY W222RP4    -PRE RP4- -RED IRP-AREA.                          
017300     EJECT                                                                
017400*01  E -COPY W222RP5    -PRE RP5- -RED IRP-AREA.                          
017600     EJECT                                                                
017700*01  F -COPY W222RP6    -PRE RP6- -RED IRP-AREA.                          
017900     EJECT                                                                
018000*01  G -COPY W222RP7    -PRE RP7- -RED IRP-AREA.                          
019410     EJECT                                                                
019420 01  UT-AREA-START               PIC X(24)   VALUE                        
019430                                             'UT-AREA-START'.             
019440     SKIP2                                                                
019500*01  -COPY W092W001C0 -PRE FEL-                                           
019700     03  FEL-POST                PIC X(80).                               
019800     EJECT                                                                
019900*    03  H -COPY W222RP1T   -PRE FEL-RP1- -RED FEL-POST.                  
020100     EJECT                                                                
020200*    03  I -COPY W222RP2T   -PRE FEL-RP2- -RED FEL-POST.                  
020400     EJECT                                                                
020500*    03  J -COPY W222RP3T   -PRE FEL-RP3- -RED FEL-POST.                  
020700     EJECT                                                                
020800*    03  K -COPY W222RP4T   -PRE FEL-RP4- -RED FEL-POST.                  
021000     EJECT                                                                
021100*    03  L -COPY W222RP5T   -PRE FEL-RP5- -RED FEL-POST.                  
021300     EJECT                                                                
021400*    03  M -COPY W222RP6T   -PRE FEL-RP6- -RED FEL-POST.                  
021600     EJECT                                                                
021700*    03  N -COPY W222RP7T   -PRE FEL-RP7- -RED FEL-POST.                  
022600     EJECT                                                                
022700*01  AREA -COPY W2222202   -PRE 2202-.                                    
022710     EJECT                                                                
022720*01  AREA -COPY W21801     -PRE W22216-.                                  
022800     EJECT                                                                
022810*01  AREA -COPY W2212204   -PRE W22217-.                                  
022811                                                                          
022812     EJECT                                                                
022816 01  UT-IRP-AREA.                                                         
022817     05  FILLER                  PIC X(100).                              
022818     SKIP3                                                                
022819*01  AREA -COPY W222RP1    -PRE UT-RP1- -RED UT-IRP-AREA.                 
022820     EJECT                                                                
022821*01  AREA -COPY W222RP2    -PRE UT-RP2- -RED UT-IRP-AREA.                 
022822     EJECT                                                                
022823*01  AREA -COPY W222RP3    -PRE UT-RP3- -RED UT-IRP-AREA.                 
022824     EJECT                                                                
022825*01  AREA -COPY W222RP4    -PRE UT-RP4- -RED UT-IRP-AREA.                 
022826     EJECT                                                                
022827*01  AREA -COPY W222RP5    -PRE UT-RP5- -RED UT-IRP-AREA.                 
022828     EJECT                                                                
022829*01  AREA -COPY W222RP6    -PRE UT-RP6- -RED UT-IRP-AREA.                 
022830     EJECT                                                                
022831*01  AREA -COPY W222RP7    -PRE UT-RP7- -RED UT-IRP-AREA.                 
022832     EJECT                                                                
022833 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022834     SKIP3                                                                
022835 01  NYCKLAR-TILL-DLI.                                                    
022836                                                                          
022837     03  W-IDARTNR-X.                                                     
022838         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
022839     03  W-KDERS-0-X.                                                     
022840         05  W-KDERS-0           PIC S9(3)   COMP-3  VALUE ZERO.          
022841     03  W-WDG3KEY.                                                       
022842         05  W-IDHTYP            PIC X(4)    VALUE SPACE.                 
022843         05  W-NYCKEL-VALFRI     PIC X(26)   VALUE LOW-VALUE.             
022844                                                                          
022845*    --- STATUS-KOD FRÅN IMS                                              
022846 01  STATUS-WS                   PIC XX.                                  
022847     88  SEGMENT-FINNS                       VALUE '  '.                  
022850     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022860     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022870     88  SEGMENT-SLUT                        VALUE 'GB'.                  
022880     88  IMS-EJ-OK                           VALUE 'XD'.                  
022890     SKIP2                                                                
022891 01  GODK-STATUSKODER.                                                    
022892     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022893     SKIP3                                                                
022894 01  SSA1                        PIC X(64).                               
022895 01  SSA2                        PIC X(64).                               
022896 01  SSA3                        PIC X(64).                               
022897     EJECT                                                                
022898*    --- IMS FUNKTIONSKODER                                               
022899*01  -COPY W0003                                                          
022900     EJECT                                                                
022901*    ---  DLI INPUT-OUTPUT AREA                                           
022902 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022903 01  DLI-IO-AREA.                                                         
022904     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
022905*                                                                         
022906     EJECT                                                                
022907 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
022908     SKIP2                                                                
022909 01  DLI-IO-AREA1.                                                        
022910     03  IO-AREA1                PIC X(110)  VALUE SPACE.                 
022911     SKIP2                                                                
022912     03  WLARTC01 REDEFINES IO-AREA1.                                     
022913*        05  -COPY WDK601                                                 
022914     EJECT                                                                
022915 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
022916 01  DLI-IO-AREA2.                                                        
022917     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
022918     SKIP3                                                                
022919     03  WLARTC11 REDEFINES IO-AREA2.                                     
022920*        05  -COPY WDK611                                                 
022921     EJECT                                                                
022922 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
022923     SKIP3                                                                
022924 01  DLI-IO-AREA3.                                                        
022925     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
022926     SKIP3                                                                
022927     03  WLARTC26 REDEFINES IO-AREA3.                                     
022928*        05  -COPY WDK626                                                 
022929     EJECT                                                                
022930 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
022931     SKIP3                                                                
022932 01  DLI-IO-AREA4.                                                        
022933     03  IO-AREA4                PIC X(50)   VALUE SPACE.                 
022934     SKIP3                                                                
022935     03  WLXXAD REDEFINES IO-AREA4.                                       
022936*        05  -COPY WDGX2207 -PRE 2207-                                    
022940     EJECT                                                                
023000 LINKAGE SECTION.                                                         
023100*01  -COPY W0009 -PRE MSG-                                                
023110     EJECT                                                                
023200*01  -COPY W0008 -PRE ARTC-                                               
023300     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024610*01  -COPY W0008 -PRE XXAD-                                               
024620     05  FILLER                  PIC X.                                   
024630     EJECT                                                                
024700 PROCEDURE DIVISION USING  MSG-PCB ARTC-PCB XXAD-PCB.                     
024800     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB XXAD-PCB.                     
024900     SKIP2                                                                
025000     PERFORM A-INITIERING                                                 
025100     SKIP1                                                                
025110     PERFORM M-NOLLSTALL-JMFWDK626                                        
025120                                                                          
025200     PERFORM S11-LAS-W22211                                               
025300     ADD 1 TO W-ANT-POSTER                                                
025400     PERFORM UNTIL (   END-OF-W22211                                      
025410                    OR W-ANT-POSTER > W-MAX-POSTER )                      
025500       IF RP1-IDPTYP = 'RP1' AND RP1-KVPB-SEP > 0                         
025600         PERFORM B-KNTL-KDUART                                            
025700       END-IF                                                             
025710                                                                          
025800       PERFORM C-LAS-ART-REG                                              
025900       IF ART-CLAG-FINNS                                                  
026000         PERFORM E-TRAFF-ARTNR-CLAGER                                     
026100       ELSE                                                               
026200         PERFORM S01-SKAPA-FELPOST-001                                    
026300       END-IF                                                             
026400       PERFORM S11-LAS-W22211                                             
026410       ADD 1 TO W-ANT-POSTER                                              
026500     END-PERFORM                                                          
026510                                                                          
026520     PERFORM N-SKRIV-OBEHANDLADE                                          
026600     PERFORM G-AVSLUTA                                                    
026700     IF RKOD > 0                                                          
026800       CALL ABEND USING RKOD                                              
026900     ELSE                                                                 
027000       MOVE ZERO TO RETURN-CODE                                           
027100       GOBACK                                                             
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 A-INITIERING SECTION.                                                    
027600************************************                                      
027700*    ÖPPNA SAMTLIGA FILER         *                                       
027800*    HÄMTA INFO FRÅN DATUMKORT    *                                       
027900************************************                                      
028000     SKIP2                                                                
028100     OPEN INPUT W22211                                                    
028200     OPEN OUTPUT W22211-UT W22213 W22214 W22216 W22217                    
028300     SKIP1                                                                
028400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
028500     SKIP1                                                                
028600     MOVE 'W22212' TO POSTSUM-PROGNAMN                                    
028700     SKIP1                                                                
028800     MOVE D-AAR TO W-AAR                                                  
028900     MOVE D-VECKA TO W-VECKA                                              
029000     MOVE D-DAGNR TO W-DAG                                                
029100     .                                                                    
029200     EJECT                                                                
029300 B-KNTL-KDUART SECTION.                                                   
029400*****************************************************************         
029500* EV.UPPDATERING AV KDUART                                      *         
029600*****************************************************************         
029700     MOVE RP1-IDARTNR TO W-IDARTNR                                        
029800     PERFORM IMS-GET-ARTIKELINF                                           
029810     IF SEGMENT-FINNS                                                     
029820       PERFORM IMS-GHU-ARTC11                                             
029821       MOVE DLI-IO-AREA TO DLI-IO-AREA2                                   
029822       IF CLAG-KDUART = 'P'                                               
029823          MOVE SPACE TO CLAG-KDUART                                       
029824       END-IF                                                             
029825       PERFORM IMS-REPL-ARTC11                                            
029892     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 C-LAS-ART-REG SECTION.                                                   
032900*********************************                                         
033000*    LÄSER ARTIKEL REGISTRET    *                                         
033100*********************************                                         
033120     MOVE NEJ    TO W-SW-ART-CLAG                                         
033130                    W-SW-ARTC26                                           
033200     MOVE RP1-IDARTNR  TO W-IDARTNR                                       
033210     PERFORM IMS-GET-ARTIKELINF                                           
033220     IF SEGMENT-FINNS                                                     
033240        PERFORM IMS-GET-ARTC11                                            
033250        IF SEGMENT-FINNS                                                  
033300           MOVE JA     TO W-SW-ART-CLAG                                   
033302           PERFORM IMS-GHU-ARTC26                                         
033303           IF SEGMENT-FINNS                                               
033304              MOVE JA  TO W-SW-ARTC26                                     
033305           ELSE                                                           
033306              PERFORM CA-INIT-ARTC26                                      
033336           END-IF                                                         
033337        END-IF                                                            
033340     END-IF                                                               
033600     .                                                                    
033601     EJECT                                                                
033610 CA-INIT-ARTC26 SECTION.                                                  
033802     MOVE ZERO TO JUST-KVPB-JUST (1)                                      
033803                  JUST-TIPBJUST (1)                                       
033804                  JUST-KVPB-JUST (2)                                      
033805                  JUST-TIPBJUST (2)                                       
033806                  JUST-REPBJUST                                           
033807                  JUST-TIPBJUST-CENTR                                     
033808                  JUST-DAMANSEA                                           
033809                  JUST-DASPSEA                                            
033810     MOVE +1   TO JUST-RESEASON (1)                                       
033811                  JUST-RESEASON (2)                                       
033812                  JUST-RESEASON (3)                                       
033813                  JUST-RESEASON (4)                                       
033814                  JUST-RESEASON (5)                                       
033815                  JUST-RESEASON (6)                                       
033816                  JUST-RESEASON (7)                                       
033817                  JUST-RESEASON (8)                                       
033818                  JUST-RESEASON (9)                                       
033819                  JUST-RESEASON (10)                                      
033820                  JUST-RESEASON (11)                                      
033821                  JUST-RESEASON (12)                                      
033822     .                                                                    
033823     EJECT                                                                
033830 E-TRAFF-ARTNR-CLAGER SECTION.                                            
033900     EVALUATE RP1-IDPTYP                                                  
034000     WHEN 'RP1'                                                           
034100       PERFORM EA-BEHANDLA-POSTTYP-RP1                                    
034400     WHEN 'RP3'                                                           
034500       PERFORM EC-BEHANDLA-POSTTYP-RP3                                    
034600     WHEN 'RP4'                                                           
034700       PERFORM ED-BEHANDLA-POSTTYP-RP4                                    
034800     WHEN 'RP5'                                                           
034900       PERFORM EE-BEHANDLA-POSTTYP-RP5                                    
035000     WHEN 'RP6'                                                           
035100       PERFORM EF-BEHANDLA-POSTTYP-RP6                                    
035200     WHEN 'RP7'                                                           
035300       PERFORM EG-BEHANDLA-POSTTYP-RP7                                    
035400     WHEN OTHER                                                           
035500       PERFORM EH-BEHANDLA-FEL-POSTTYP                                    
035600     END-EVALUATE                                                         
035700     .                                                                    
035800     EJECT                                                                
035900 EA-BEHANDLA-POSTTYP-RP1 SECTION.                                         
036000     SKIP1                                                                
036100***************************************************************           
036200*    SECTIONEN BEHANDLAR TRANSAKTIONSTYP RP1 (SEPARAT PROGNOS)*           
036300***************************************************************           
036400     SKIP1                                                                
036500     MOVE CLAG-KVPB-SEP TO 2202-KVPB-SEP-GAMMAL                           
036800                           WS-BAS-KVPB-SEP                                
037100     MOVE RP1-KVPB-SEP  TO CLAG-KVPB-SEP                                  
037110     MOVE RP1-KVPB-SEP  TO CLAG-KVPB-HIST                                 
037200     MOVE W-DATUM       TO CLAG-TIPBDAT                                   
037300     MOVE +0            TO CLAG-RVPROFEL                                  
037400                           CLAG-RVPROURS                                  
037500                           CLAG-KVUTJFEL                                  
037510                                                                          
038500     IF CLAG-KVPB-SEP > +0                                                
038600       COMPUTE W-AVVIKELSE ROUNDED =                                      
038700       CLAG-KVMAD-SEP / (CLAG-KVPB-SEP ** 0.85)                           
038710                                                                          
038800       IF W-AVVIKELSE < 1 / 1.7 OR                                        
038900       W-AVVIKELSE > +1.7                                                 
039000         COMPUTE CLAG-KVMAD-SEP ROUNDED =                                 
039100         CLAG-KVPB-SEP ** 0.85                                            
039200         COMPUTE CLAG-KVMAD-TOT ROUNDED =                                 
039300         (CLAG-KVPB-SEP + CLAG-KVPB-SATS) ** 0.85                         
039400       END-IF                                                             
039500     END-IF                                                               
039600                                                                          
039601*---  POSITIONSLÄSNING FÖRE REPLACE                                       
039610     PERFORM IMS-GHU-ARTC11                                               
039620                                                                          
039700     PERFORM IMS-REPL-ARTC11                                              
039800     IF ART-IDLEVNR = '1002 ' AND CLAG-KDERS = +0                         
039900       PERFORM EAA-SATSARTIKEL                                            
040000     END-IF                                                               
040010                                                                          
040100     IF 2202-KVPB-SEP-GAMMAL = 0                                          
040200       MOVE +1   TO WS-2202-KVPB-SEP-GAMMAL                               
040300     ELSE                                                                 
040400       MOVE 2202-KVPB-SEP-GAMMAL TO WS-2202-KVPB-SEP-GAMMAL               
040500     END-IF                                                               
040510                                                                          
040600     IF CLAG-KVPB-SEP = 0                                                 
040700       MOVE +1   TO WS-CLAG-KVPB-SEP                                      
040800     ELSE                                                                 
040900       MOVE CLAG-KVPB-SEP TO WS-CLAG-KVPB-SEP                             
041000     END-IF                                                               
041100                                                                          
042100     IF WS-BAS-KVPB-SEP NOT = CLAG-KVPB-SEP                               
042200       PERFORM S15-SKAPA-POST-TILL-W22216                                 
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 EAA-SATSARTIKEL SECTION.                                                 
042700************************************************************              
042800*    DÅ LEVERANTÖRSNUMMER = 1002, ÄR DET EN SATSARTIKEL    *              
042810*    SKRIV PÅ UTFIL W22214                                                
042900************************************************************              
043000     MOVE RP1-IDARTNR  TO 2202-IDARTNR-SATS                               
043100     MOVE +1           TO 2202-KDCLAGER                                   
043200     MOVE RP1-KVPB-SEP TO 2202-KVPB-SEP-NY                                
043300                                                                          
043310     PERFORM S18-SKRIV-W22214                                             
043500     .                                                                    
043600     EJECT                                                                
049900 EC-BEHANDLA-POSTTYP-RP3 SECTION.                                         
050000***************************************************************           
050100*    SEKTIONEN BEHANDLAR TRANSAKTIONSTYP RP3 (SÄSONGINDEX)    *           
050200***************************************************************           
050300     SKIP1                                                                
050400     IF RP3-FLABORT-SEASON = JA                                           
050500       PERFORM ECA-BORTTAG-AV-SASONGINDEX                                 
050600       PERFORM ECC-UPPLAGG-PA-HANDELSEREG                                 
050700     ELSE                                                                 
050800       MOVE 1 TO SASONG-INDEX                                             
050900       MOVE ZERO TO RESEASON-ACK                                          
051000       PERFORM UNTIL                                                      
051100        NOT ( SASONG-INDEX < 13 )                                         
051200         IF  RP3-RESEASON (SASONG-INDEX) NOT NUMERIC                      
051300           MOVE  +0 TO RP3-RESEASON (SASONG-INDEX)                        
051400         END-IF                                                           
051500         ADD RP3-RESEASON (SASONG-INDEX) TO RESEASON-ACK                  
051600         ADD 1 TO SASONG-INDEX                                            
051700       END-PERFORM                                                        
051800       IF RESEASON-ACK = 12.00                                            
051900         PERFORM ECB-REDIGERA-SASONGINDEX                                 
052000         PERFORM ECC-UPPLAGG-PA-HANDELSEREG                               
052100         PERFORM I-UPPD-BORTTAG-SASONGINDEX                               
052101*                                                                         
052110*---  POSITIONSLÄSNING FÖRE REPLACE                                       
052120         PERFORM IMS-GHU-ARTC11                                           
052130                                                                          
052200         PERFORM IMS-REPL-ARTC11                                          
052300       ELSE                                                               
052400         PERFORM S02-SKAPA-FELPOST-002                                    
052500       END-IF                                                             
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 ECA-BORTTAG-AV-SASONGINDEX SECTION.                                      
053000*****************************************                                 
053100*    HÄR SKER BORTTAG AV SÄSONGINDEX    *                                 
053200*****************************************                                 
053300     SKIP1                                                                
053500     MOVE 1 TO JUST-RESEASON (1) JUST-RESEASON (2)                        
053600     MOVE 1 TO JUST-RESEASON (3) JUST-RESEASON (4)                        
053700     MOVE 1 TO JUST-RESEASON (5) JUST-RESEASON (6)                        
053800     MOVE 1 TO JUST-RESEASON (7) JUST-RESEASON (8)                        
053810     MOVE 1 TO JUST-RESEASON (9) JUST-RESEASON (10)                       
053820     MOVE 1 TO JUST-RESEASON (11) JUST-RESEASON (12)                      
053900     PERFORM I-UPPD-BORTTAG-SASONGINDEX                                   
054000     .                                                                    
054100     EJECT                                                                
054200 ECB-REDIGERA-SASONGINDEX SECTION.                                        
054300     SKIP1                                                                
054400********************************************                              
054500*    HÄR SKER REDIGERAING AV SÄSONGINDEX    *                             
054600********************************************                              
054700     SKIP1                                                                
054800     MOVE 1 TO SASONG-INDEX                                               
054900     MOVE RP3-RESEASON (SASONG-INDEX) TO MINSTA-SASONG                    
055000     STORSTA-SASONG                                                       
055200     PERFORM UNTIL NOT ( SASONG-INDEX < 13 )                              
055300       MOVE RP3-RESEASON (SASONG-INDEX) TO                                
055400       JUST-RESEASON (SASONG-INDEX)                                       
055410                                                                          
055500       IF RP3-RESEASON (SASONG-INDEX) < MINSTA-SASONG                     
055600         MOVE RP3-RESEASON (SASONG-INDEX) TO MINSTA-SASONG                
055700       END-IF                                                             
055710                                                                          
055800       IF RP3-RESEASON (SASONG-INDEX) > STORSTA-SASONG                    
055900         MOVE RP3-RESEASON (SASONG-INDEX) TO STORSTA-SASONG               
056000       END-IF                                                             
056100       ADD 1 TO SASONG-INDEX                                              
056200     END-PERFORM                                                          
056300     SUBTRACT MINSTA-SASONG FROM STORSTA-SASONG GIVING                    
056400     DIFF-SASONG                                                          
056500     IF DIFF-SASONG > 1.50                                                
056600       MOVE NEJ TO CLAG-FLMPB                                             
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000 ECC-UPPLAGG-PA-HANDELSEREG SECTION.                                      
057100     SKIP1                                                                
057200*************************************************                         
057300*    HÄR SKER NYUPPLÄGG PÅ HÄNDELSEREGISTRET    *                         
057400*************************************************                         
057500     SKIP1                                                                
057910     MOVE ART-IDARTNR TO W22217-IDARTNR                                   
057920     MOVE 54          TO W22217-KDLPORS                                   
057930     MOVE '2204'      TO W22217-IDHTYP                                    
057940     PERFORM S17-SKRIV-W22217                                             
058000     .                                                                    
060100     EJECT                                                                
060200 ED-BEHANDLA-POSTTYP-RP4 SECTION.                                         
060300******************************************************************        
060400*    SECTIONEN BEHANDLAR TRANSAKTIONSTYP RP4 (JUSTERINGAR ANTAL) *        
060500******************************************************************        
060600     SKIP1                                                                
060700     IF RP4-FLABORT-PBJUST = JA                                           
060800       PERFORM EDA-BORTTAG-PBJUST-ANTAL                                   
060900     ELSE                                                                 
061000       IF CLAG-KVPB-SEP > 0                                               
061100         PERFORM EDB-KVPB-SEP-RIKTIG                                      
061200       ELSE                                                               
061300         PERFORM S02-SKAPA-FELPOST-002                                    
061400       END-IF                                                             
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 EDA-BORTTAG-PBJUST-ANTAL SECTION.                                        
061900     SKIP1                                                                
062000******************************************                                
062100*    HÄR SKER BORTTAG AV PBJUST-ANTAL    *                                
062200******************************************                                
062300     SKIP1                                                                
062500     MOVE 0 TO JUST-KVPB-JUST (1) JUST-KVPB-JUST (2)                      
062600     MOVE 0 TO JUST-TIPBJUST (1) JUST-TIPBJUST (2)                        
062700     PERFORM J-UPPD-BORTTAG-PBJUST-ANTAL                                  
062800     .                                                                    
062900     EJECT                                                                
063000 EDB-KVPB-SEP-RIKTIG SECTION.                                             
063100     SKIP1                                                                
063200********************************************************                  
063300*    HÄR BEHANDLAS POSTTYP RP4 OM KVPB-SEP > 0         *                  
063400********************************************************                  
063500     SKIP1                                                                
063502     MOVE RP4-TIPBJUST (1) TO TMP1-YYWW                                   
063503     MOVE RP4-TIPBJUST (2) TO TMP2-YYWW                                   
063510     PERFORM WY2000P3                                                     
063511                                                                          
063520     IF TMP2-YYWW < TMP1-YYWW AND > ZERO                                  
063700       MOVE RP4-TIPBJUST (2)  TO JUST-TIPBJUST (1)                        
063800       MOVE RP4-TIPBJUST (1)  TO JUST-TIPBJUST (2)                        
063900       MOVE RP4-KVPB-JUST (2) TO JUST-KVPB-JUST (1)                       
064000       MOVE RP4-KVPB-JUST (1) TO JUST-KVPB-JUST (2)                       
064100     ELSE                                                                 
064200       MOVE RP4-TIPBJUST (1)  TO JUST-TIPBJUST (1)                        
064300       MOVE RP4-TIPBJUST (2)  TO JUST-TIPBJUST (2)                        
064400       MOVE RP4-KVPB-JUST (1) TO JUST-KVPB-JUST (1)                       
064500       MOVE RP4-KVPB-JUST (2) TO JUST-KVPB-JUST (2)                       
064600     END-IF                                                               
064700     PERFORM EDC-UPPLAGG-PA-HANDELSEREG                                   
064800     PERFORM EDD-UPPLAGG-PA-HANDELSEREG                                   
064900     PERFORM J-UPPD-BORTTAG-PBJUST-ANTAL                                  
065000     .                                                                    
065100     EJECT                                                                
065200 EDC-UPPLAGG-PA-HANDELSEREG SECTION.                                      
065300     SKIP1                                                                
065400*************************************************                         
065500*    HÄR SKER NYUPPLÄGG PÅ HÄNDELSEREGISTRET    *                         
065600*************************************************                         
065700     SKIP1                                                                
066110     MOVE ART-IDARTNR TO W22217-IDARTNR                                   
066120     MOVE 53          TO W22217-KDLPORS                                   
066130     MOVE '2204'      TO W22217-IDHTYP                                    
066140     PERFORM S17-SKRIV-W22217                                             
066200     .                                                                    
066300     EJECT                                                                
066400 EDD-UPPLAGG-PA-HANDELSEREG SECTION.                                      
066500     SKIP1                                                                
066600*************************************************                         
066700*    HÄR SKER NYUPPLÄGG PÅ HÄNDELSEREGISTRET    *                         
066800*************************************************                         
066900     SKIP1                                                                
067000     MOVE ART-IDARTNR  TO 2207-IDARTNR                                    
067200     MOVE '2207'       TO W-IDHTYP                                        
067300     SKIP1                                                                
067400     IF RP4-TIPBJUST (1) > 0                                              
067500       MOVE RP4-TIPBJUST (1) TO 2207-TIPBJUST                             
067600       PERFORM IMS-ISRT-R2207                                             
067700     END-IF                                                               
067800     SKIP1                                                                
067900     IF RP4-TIPBJUST (2) > 0                                              
068000       MOVE RP4-TIPBJUST (2) TO 2207-TIPBJUST                             
068001       PERFORM IMS-ISRT-R2207                                             
068200     END-IF                                                               
068300     .                                                                    
068400     EJECT                                                                
069400 EE-BEHANDLA-POSTTYP-RP5 SECTION.                                         
069500     SKIP1                                                                
069600******************************************************************        
069700*    SEKTIONEN BEHANDLAR TRANSAKTIONSTYP RP5 (CENTRAL PBJUSTERING)        
069800******************************************************************        
069900     SKIP1                                                                
070000     IF RP5-FLABORT-JUST = JA                                             
070100       PERFORM EEA-BORTTAG-AV-PBJUST-CENTRAL                              
070200     ELSE                                                                 
070300       MOVE RP5-TIPBJUST TO JUST-TIPBJUST-CENTR                           
070400       MOVE RP5-REPBJUST TO JUST-REPBJUST                                 
070410                                                                          
070500       PERFORM EEB-UPPLAGG-PA-HANDELSEREG                                 
070600       PERFORM EEC-UPPLAGG-PA-HANDELSEREG                                 
070700       PERFORM EED-UPPD-PBJUST-CENTRAL                                    
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 EEA-BORTTAG-AV-PBJUST-CENTRAL SECTION.                                   
071200     SKIP1                                                                
071300********************************************                              
071400*    HÄR SKER BORTTAG AV PBJUST CENTRAL    *                              
071500********************************************                              
071600     SKIP1                                                                
071800     MOVE 0 TO JUST-REPBJUST                                              
071900               JUST-TIPBJUST-CENTR                                        
072000     PERFORM EED-UPPD-PBJUST-CENTRAL                                      
072100     .                                                                    
072200     EJECT                                                                
072300 EEB-UPPLAGG-PA-HANDELSEREG SECTION.                                      
072400     SKIP1                                                                
072500*************************************************                         
072600*    HÄR SKER NYUPPLÄGG PÅ HÄNDELSEREGISTRET    *                         
072700*************************************************                         
072800     SKIP1                                                                
073210     MOVE ART-IDARTNR TO W22217-IDARTNR                                   
073220     MOVE 53          TO W22217-KDLPORS                                   
073230     MOVE '2204'      TO W22217-IDHTYP                                    
073240     PERFORM S17-SKRIV-W22217                                             
073300     .                                                                    
073400     EJECT                                                                
073500 EEC-UPPLAGG-PA-HANDELSEREG SECTION.                                      
073600     SKIP1                                                                
073700*************************************************                         
073800*    HÄR SKER NYUPPLÄGG PÅ HÄNDELSEREGISTRET    *                         
073900*************************************************                         
074000     SKIP1                                                                
074100     MOVE ART-IDARTNR  TO 2207-IDARTNR                                    
074300     MOVE RP5-TIPBJUST TO 2207-TIPBJUST                                   
074400     MOVE '2207'       TO W-IDHTYP                                        
074401                                                                          
074402     PERFORM IMS-ISRT-R2207                                               
074600     .                                                                    
074700     EJECT                                                                
074800 EED-UPPD-PBJUST-CENTRAL SECTION.                                         
074900     SKIP1                                                                
075000     IF ARTC26-FINNS                                                      
075100       PERFORM L-TEST-AV-ARTC26                                           
075200       IF TEST-SVAR = DLET                                                
075300         PERFORM IMS-DLET-ARTC26                                          
075400       ELSE                                                               
075500         PERFORM IMS-REPL-ARTC26                                          
075600       END-IF                                                             
075610     ELSE                                                                 
075620       MOVE JMF-JUST-PBJUST (1)   TO JUST-PBJUST (1)                      
075630       MOVE JMF-JUST-PBJUST (2)   TO JUST-PBJUST (2)                      
075631       MOVE JMF-JUST-DAMANSEA     TO JUST-DAMANSEA                        
075632       MOVE JMF-JUST-DASPSEA      TO JUST-DASPSEA                         
075640       MOVE JMF-JUST-RESEASON (1) TO JUST-RESEASON (1)                    
075650       MOVE JMF-JUST-RESEASON (2) TO JUST-RESEASON (2)                    
075660       MOVE JMF-JUST-RESEASON (3) TO JUST-RESEASON (3)                    
075670       MOVE JMF-JUST-RESEASON (4) TO JUST-RESEASON (4)                    
075680       MOVE JMF-JUST-RESEASON (5) TO JUST-RESEASON (5)                    
075690       MOVE JMF-JUST-RESEASON (6) TO JUST-RESEASON (6)                    
075691       MOVE JMF-JUST-RESEASON (7) TO JUST-RESEASON (7)                    
075692       MOVE JMF-JUST-RESEASON (8) TO JUST-RESEASON (8)                    
075693       MOVE JMF-JUST-RESEASON (9) TO JUST-RESEASON (9)                    
075694       MOVE JMF-JUST-RESEASON (10) TO JUST-RESEASON (10)                  
075695       MOVE JMF-JUST-RESEASON (11) TO JUST-RESEASON (11)                  
075696       MOVE JMF-JUST-RESEASON (12) TO JUST-RESEASON (12)                  
075698       PERFORM L-TEST-AV-ARTC26                                           
075699       IF TEST-SVAR = ISRT                                                
075700         PERFORM IMS-ISRT-ARTC26                                          
075701       END-IF                                                             
075702     END-IF                                                               
075703     .                                                                    
075710     EJECT                                                                
075800 EF-BEHANDLA-POSTTYP-RP6 SECTION.                                         
075900     SKIP1                                                                
076000*************************************************                         
076100*    SEKTIONEN BEHANDLAR TRANSAKTIONSTYP RP6    *                         
076200*            MANUELL OCH MASKINELL PROGNOS      *                         
076300*************************************************                         
076400     SKIP1                                                                
076500     IF RP6-FLMPB = JA                                                    
076600       PERFORM EFA-TEST-DIFF-STOR-MINST-IND                               
076700     END-IF                                                               
076800     IF FELUTSKRIFT = NEJ                                                 
076900       MOVE RP6-FLMPB TO CLAG-FLMPB                                       
076910*                                                                         
076920*---  POSITIONSLÄSNING FÖRE REPLACE                                       
076930       PERFORM IMS-GHU-ARTC11                                             
076940                                                                          
077000       PERFORM IMS-REPL-ARTC11                                            
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400 EFA-TEST-DIFF-STOR-MINST-IND SECTION.                                    
077500     SKIP1                                                                
077600******************************************************************        
077700*    HÄR SKER TEST ATT L-KVPB-SEP ÄR LIKA MED ELLER STÖRRE ÄN 1  *        
077800*    SAMT ATT DIFFERENSEN MELLAN STÖRSTA OCH MINSTA SÄSONGINDEX  *        
077900*    ÄR LIKA MED ELLER MINDRE ÄN 1.50                            *        
078000******************************************************************        
078100     SKIP1                                                                
078200     IF CLAG-KVPB-SEP < 0.2                                               
078300       PERFORM S02-SKAPA-FELPOST-002                                      
078400       MOVE JA TO FELUTSKRIFT                                             
078500     ELSE                                                                 
078600       MOVE NEJ TO FELUTSKRIFT                                            
078700     END-IF                                                               
078710                                                                          
078800     IF FELUTSKRIFT = NEJ                                                 
078900       MOVE 1 TO SASONG-INDEX                                             
079000       MOVE JUST-RESEASON (SASONG-INDEX) TO MINSTA-SASONG                 
079100                                            STORSTA-SASONG                
079300       PERFORM UNTIL NOT ( SASONG-INDEX < 13 )                            
079400         IF JUST-RESEASON (SASONG-INDEX) < MINSTA-SASONG                  
079500           MOVE JUST-RESEASON (SASONG-INDEX) TO MINSTA-SASONG             
079600         END-IF                                                           
079610                                                                          
079700         IF JUST-RESEASON (SASONG-INDEX) > STORSTA-SASONG                 
079800           MOVE JUST-RESEASON (SASONG-INDEX) TO STORSTA-SASONG            
079900         END-IF                                                           
079910                                                                          
080000         ADD 1 TO SASONG-INDEX                                            
080100       END-PERFORM                                                        
080110                                                                          
080200       SUBTRACT MINSTA-SASONG FROM STORSTA-SASONG GIVING                  
080300       DIFF-SASONG                                                        
080400       IF DIFF-SASONG > 1.50                                              
080500         PERFORM S02-SKAPA-FELPOST-002                                    
080600         MOVE JA TO FELUTSKRIFT                                           
080700       END-IF                                                             
080800     END-IF                                                               
080900     .                                                                    
082000     EJECT                                                                
082100 EG-BEHANDLA-POSTTYP-RP7 SECTION.                                         
082200     SKIP1                                                                
082300*************************************************                         
082400*    SEKTIONEN BEHANDLAR TRANSAKTIONSTYP RP7    *                         
082500*              OREGELBUNDEN PROGNOS             *                         
082600*************************************************                         
082700     SKIP1                                                                
082800     MOVE RP7-FLOREGPB TO CLAG-FLOREGPB                                   
082900     MOVE 0            TO CLAG-RVPROFEL                                   
083000                          CLAG-RVPROURS                                   
083010*                                                                         
083020*---  POSITIONSLÄSNING FÖRE REPLACE                                       
083030     PERFORM IMS-GHU-ARTC11                                               
083040                                                                          
083100     PERFORM IMS-REPL-ARTC11                                              
083200     .                                                                    
084300     EJECT                                                                
084400 EH-BEHANDLA-FEL-POSTTYP SECTION.                                         
084500     SKIP1                                                                
084600     MOVE +29 TO RKOD                                                     
084700     CALL ABEND USING RKOD                                                
084800     .                                                                    
084900     EJECT                                                                
085000 G-AVSLUTA SECTION.                                                       
085100     CLOSE W22211 W22213 W22214 W22216 W22217                             
085200     MOVE 'S' TO POSTSUM-OPKOD                                            
085300     CALL POSTSUM USING POSTSUM-PARM                                      
085400     .                                                                    
085410     EJECT                                                                
085530 I-UPPD-BORTTAG-SASONGINDEX SECTION.                                      
085540     SKIP1                                                                
085550     IF ARTC26-FINNS                                                      
085580       PERFORM L-TEST-AV-ARTC26                                           
085590       IF TEST-SVAR = DLET                                                
085591         PERFORM IMS-DLET-ARTC26                                          
085592       ELSE                                                               
085593         PERFORM IMS-REPL-ARTC26                                          
085594       END-IF                                                             
085595     ELSE                                                                 
085596       MOVE JMF-JUST-CENTR-PBJUST TO JUST-CENTR-PBJUST                    
085597       MOVE JMF-JUST-DAMANSEA     TO JUST-DAMANSEA                        
085598       MOVE JMF-JUST-DASPSEA      TO JUST-DASPSEA                         
085599       MOVE JMF-JUST-PBJUST (1)   TO JUST-PBJUST (1)                      
085600       MOVE JMF-JUST-PBJUST (2)   TO JUST-PBJUST (2)                      
085602       PERFORM L-TEST-AV-ARTC26                                           
085603       IF TEST-SVAR = ISRT                                                
085604         PERFORM IMS-ISRT-ARTC26                                          
085605       END-IF                                                             
085606     END-IF                                                               
085607     .                                                                    
085608     EJECT                                                                
085609 J-UPPD-BORTTAG-PBJUST-ANTAL SECTION.                                     
085610     SKIP1                                                                
085611     IF ARTC26-FINNS                                                      
085615       PERFORM L-TEST-AV-ARTC26                                           
085616       IF TEST-SVAR = DLET                                                
085617         PERFORM IMS-DLET-ARTC26                                          
085618       ELSE                                                               
085619         PERFORM IMS-REPL-ARTC26                                          
085620       END-IF                                                             
085621     ELSE                                                                 
085622       MOVE JMF-JUST-REPBJUST       TO JUST-REPBJUST                      
085623       MOVE JMF-JUST-TIPBJUST-CENTR TO JUST-TIPBJUST-CENTR                
085625       MOVE JMF-JUST-DAMANSEA       TO JUST-DAMANSEA                      
085627       MOVE JMF-JUST-DASPSEA        TO JUST-DASPSEA                       
085628       MOVE JMF-JUST-RESEASON (1)   TO JUST-RESEASON (1)                  
085629       MOVE JMF-JUST-RESEASON (2)   TO JUST-RESEASON (2)                  
085630       MOVE JMF-JUST-RESEASON (3)   TO JUST-RESEASON (3)                  
085631       MOVE JMF-JUST-RESEASON (4)   TO JUST-RESEASON (4)                  
085632       MOVE JMF-JUST-RESEASON (5)   TO JUST-RESEASON (5)                  
085633       MOVE JMF-JUST-RESEASON (6)   TO JUST-RESEASON (6)                  
085634       MOVE JMF-JUST-RESEASON (7)   TO JUST-RESEASON (7)                  
085635       MOVE JMF-JUST-RESEASON (8)   TO JUST-RESEASON (8)                  
085636       MOVE JMF-JUST-RESEASON (9)   TO JUST-RESEASON (9)                  
085637       MOVE JMF-JUST-RESEASON (10)   TO JUST-RESEASON (10)                
085638       MOVE JMF-JUST-RESEASON (11)   TO JUST-RESEASON (11)                
085639       MOVE JMF-JUST-RESEASON (12)   TO JUST-RESEASON (12)                
085640                                                                          
085641       PERFORM L-TEST-AV-ARTC26                                           
085642       IF TEST-SVAR = ISRT                                                
085643         PERFORM IMS-ISRT-ARTC26                                          
085644       END-IF                                                             
085645     END-IF                                                               
085646     .                                                                    
085673     EJECT                                                                
085674 L-TEST-AV-ARTC26 SECTION.                                                
085676     SKIP1                                                                
085677     IF JUST-WDK626 = JMF-JUST-WDK626                                     
085678       MOVE DLET TO TEST-SVAR                                             
085679     ELSE                                                                 
085680       MOVE ISRT TO TEST-SVAR                                             
085681     END-IF                                                               
085682     .                                                                    
085683     EJECT                                                                
085684 M-NOLLSTALL-JMFWDK626 SECTION.                                           
085685     SKIP1                                                                
085686     MOVE +0 TO JMF-JUST-REPBJUST JMF-JUST-TIPBJUST-CENTR                 
085687     MOVE +0 TO JMF-JUST-KVPB-JUST (1) JMF-JUST-KVPB-JUST (2)             
085688     MOVE +0 TO JMF-JUST-TIPBJUST (1) JMF-JUST-TIPBJUST (2)               
085689     MOVE +0 TO JMF-JUST-DAMANSEA                                         
085690     MOVE +0 TO JMF-JUST-DASPSEA                                          
085691     MOVE +1 TO JMF-JUST-RESEASON (1) JMF-JUST-RESEASON (2)               
085692     MOVE +1 TO JMF-JUST-RESEASON (3) JMF-JUST-RESEASON (4)               
085693     MOVE +1 TO JMF-JUST-RESEASON (5) JMF-JUST-RESEASON (6)               
085694     MOVE +1 TO JMF-JUST-RESEASON (7) JMF-JUST-RESEASON (8)               
085695     MOVE +1 TO JMF-JUST-RESEASON (9) JMF-JUST-RESEASON (10)              
085696     MOVE +1 TO JMF-JUST-RESEASON (11) JMF-JUST-RESEASON (12)             
085697     .                                                                    
085698     EJECT                                                                
085699 N-SKRIV-OBEHANDLADE SECTION.                                             
085700***                                                                       
085701* SKRIVER DE POSTER SOM INTE HAR BEHANDLATS                               
085702* PÅ EN NY GENERATION AV FILEN W22211                                     
085703***                                                                       
085704                                                                          
085705     PERFORM UNTIL (END-OF-W22211)                                        
085706       MOVE IRP-AREA TO UT-IRP-AREA                                       
085707                                                                          
085708       PERFORM S19-SKRIV-W22211                                           
085709       PERFORM S11-LAS-W22211                                             
085710     END-PERFORM                                                          
085711     .                                                                    
085712     EJECT                                                                
085713 S01-SKAPA-FELPOST-001 SECTION.                                           
085720     SKIP1                                                                
085800********************************                                          
085900*    HÄR SKAPAS FELPOST 001    *                                          
086000********************************                                          
086100     MOVE ZERO        TO FEL-W092W001                                     
086200     MOVE SPACE       TO FEL-POST                                         
086300     MOVE RP1-IDPTYP  TO FEL-IDPTYP                                       
086400     MOVE +1          TO FEL-KDCLAGER                                     
086500     MOVE RP1-IDARTNR TO FEL-SORTBGP                                      
086600     MOVE ZERO        TO FEL-IDKUNDNR                                     
086700     MOVE '001'       TO FEL-IDFELKODX                                    
086800     EVALUATE RP1-IDPTYP                                                  
086900     WHEN 'RP1'                                                           
087000       PERFORM S04-RED-FELPOST-RP1                                        
087300     WHEN 'RP3'                                                           
087400       PERFORM S06-RED-FELPOST-RP3                                        
087500     WHEN 'RP4'                                                           
087600       PERFORM S07-RED-FELPOST-RP4                                        
087700     WHEN 'RP5'                                                           
087800       PERFORM S08-RED-FELPOST-RP5                                        
087900     WHEN 'RP6'                                                           
088000       PERFORM S09-RED-FELPOST-RP6                                        
088100     WHEN 'RP7'                                                           
088200       PERFORM S10-RED-FELPOST-RP7                                        
088300     END-EVALUATE                                                         
088400     PERFORM S03-SKRIV-FELPOST                                            
088500     .                                                                    
088600     EJECT                                                                
088700 S02-SKAPA-FELPOST-002 SECTION.                                           
088800     SKIP1                                                                
088900********************************                                          
089000*    HÄR SKAPAS FELPOST 002    *                                          
089100********************************                                          
089200     MOVE ZERO        TO FEL-W092W001                                     
089300     MOVE SPACE       TO FEL-POST                                         
089400     MOVE RP1-IDPTYP  TO FEL-IDPTYP                                       
089500     MOVE +1          TO FEL-KDCLAGER                                     
089600     MOVE RP1-IDARTNR TO FEL-SORTBGP                                      
089700     MOVE CLAG-IDANSK TO FEL-IDKUNDNR                                     
089800     MOVE '002'       TO FEL-IDFELKODX                                    
089900     EVALUATE RP1-IDPTYP                                                  
090000     WHEN 'RP1'                                                           
090100       PERFORM S04-RED-FELPOST-RP1                                        
090400     WHEN 'RP3'                                                           
090500       PERFORM S06-RED-FELPOST-RP3                                        
090600     WHEN 'RP4'                                                           
090700       PERFORM S07-RED-FELPOST-RP4                                        
090800     WHEN 'RP5'                                                           
090900       PERFORM S08-RED-FELPOST-RP5                                        
091000     WHEN 'RP6'                                                           
091100       PERFORM S09-RED-FELPOST-RP6                                        
091200     WHEN 'RP7'                                                           
091300       PERFORM S10-RED-FELPOST-RP7                                        
091400     END-EVALUATE                                                         
091500     PERFORM S03-SKRIV-FELPOST                                            
091600     .                                                                    
091700     EJECT                                                                
091800 S03-SKRIV-FELPOST SECTION.                                               
091900     SKIP1                                                                
092000*****************************************************************         
092100*    HÄR SKER SKRIVNING AV FELPOSTER SAMT EN SUMMERING AV DE    *         
092200*    SKRIVNA POSTERNA MED HJÄLP AV POSTSUM                      *         
092300*****************************************************************         
092400     SKIP1                                                                
092500     WRITE W22213-POST FROM FEL-W092W001                                  
092600     MOVE 'W22213' TO POSTSUM-FDNAMN                                      
092700     MOVE 'W22212D2' TO POSTSUM-DDNAMN2                                   
092800     MOVE RP1-IDPTYP TO POSTSUM-TRANSTYP FEL-IDPTYP                       
092900     CALL POSTSUM USING POSTSUM-PARM                                      
093000     .                                                                    
093100     EJECT                                                                
093200 S04-RED-FELPOST-RP1 SECTION.                                             
093300     SKIP1                                                                
093400*********************************************************                 
093500*    HÄR SKER REDIGERING AV FELPOST DÅ POSTTYP = RP1    *                 
093600*********************************************************                 
093700     SKIP1                                                                
093800     MOVE RP1-IDPTYP TO FEL-RP1-IDPTYP                                    
093900     MOVE RP1-IDARTNR TO FEL-RP1-IDARTNR                                  
094000     MOVE +1           TO FEL-RP1-KDCLAGER                                
094100     MOVE RP1-KVPB-SEP TO FEL-RP1-KVPB-SEP                                
094200     .                                                                    
094300     EJECT                                                                
096000 S06-RED-FELPOST-RP3 SECTION.                                             
096100     SKIP1                                                                
096200*********************************************************                 
096300*    HÄR SKER REDIGERING AV FELPOST DÅ POSTTYP = RP3    *                 
096400*********************************************************                 
096500     SKIP1                                                                
096600     MOVE RP3-IDPTYP TO FEL-RP3-IDPTYP                                    
096700     MOVE RP3-IDARTNR TO FEL-RP3-IDARTNR                                  
096800     MOVE +1          TO FEL-RP3-KDCLAGER                                 
096900     MOVE RP3-RESEASON (1) TO FEL-RP3-RESEASON (1)                        
097000     MOVE RP3-RESEASON (2) TO FEL-RP3-RESEASON (2)                        
097100     MOVE RP3-RESEASON (3) TO FEL-RP3-RESEASON (3)                        
097200     MOVE RP3-RESEASON (4) TO FEL-RP3-RESEASON (4)                        
097300     MOVE RP3-RESEASON (5) TO FEL-RP3-RESEASON (5)                        
097400     MOVE RP3-RESEASON (6) TO FEL-RP3-RESEASON (6)                        
097500     MOVE RP3-RESEASON (7) TO FEL-RP3-RESEASON (7)                        
097600     MOVE RP3-RESEASON (8) TO FEL-RP3-RESEASON (8)                        
097610     MOVE RP3-RESEASON (9) TO FEL-RP3-RESEASON (9)                        
097620     MOVE RP3-RESEASON (10) TO FEL-RP3-RESEASON (10)                      
097630     MOVE RP3-RESEASON (11) TO FEL-RP3-RESEASON (11)                      
097640     MOVE RP3-RESEASON (12) TO FEL-RP3-RESEASON (12)                      
097700     MOVE RP3-FLABORT-SEASON TO FEL-RP3-FLABORT-SEASON                    
097800     .                                                                    
097900     EJECT                                                                
098000 S07-RED-FELPOST-RP4 SECTION.                                             
098100     SKIP1                                                                
098200*******************************************************                   
098300*    HÄR SKER REDIGERING AV FELPOST DÅ POSTTYP = RP4  *                   
098400*******************************************************                   
098500     SKIP1                                                                
098600     MOVE RP4-IDPTYP TO FEL-RP4-IDPTYP                                    
098700     MOVE RP4-IDARTNR TO FEL-RP4-IDARTNR                                  
098800     MOVE +1          TO FEL-RP4-KDCLAGER                                 
098900     MOVE RP4-TIPBJUST (1) TO FEL-RP4-TIPBJUST (1)                        
099000     MOVE RP4-TIPBJUST (2) TO FEL-RP4-TIPBJUST (2)                        
099100     MOVE RP4-KVPB-JUST (1) TO FEL-RP4-KVPB-JUST (1)                      
099200     MOVE RP4-KVPB-JUST (2) TO FEL-RP4-KVPB-JUST (2)                      
099300     MOVE RP4-FLABORT-PBJUST TO FEL-RP4-FLABORT-PBJUST                    
099400     .                                                                    
099500     EJECT                                                                
099600 S08-RED-FELPOST-RP5 SECTION.                                             
099700     SKIP1                                                                
099800*********************************************************                 
099900*    HÄR SKER REDIGERING AV FELPOST DÅ POSTTYP = RP5    *                 
100000*********************************************************                 
100100     SKIP1                                                                
100200     MOVE RP5-IDPTYP TO FEL-RP5-IDPTYP                                    
100300     MOVE RP5-IDARTNR TO FEL-RP5-IDARTNR                                  
100400     MOVE +1          TO FEL-RP5-KDCLAGER                                 
100500     MOVE RP5-TIPBJUST TO FEL-RP5-TIPBJUST                                
100600     MOVE RP5-REPBJUST TO FEL-RP5-REPBJUST                                
100700     MOVE RP5-FLABORT-JUST TO FEL-RP5-FLABORT-JUST                        
100800     .                                                                    
100900     EJECT                                                                
101000 S09-RED-FELPOST-RP6 SECTION.                                             
101100     SKIP1                                                                
101200*********************************************************                 
101300*    HÄR SKER REDIGERING AV FELPOST DÅ POSTTYP = RP6    *                 
101400*********************************************************                 
101500     SKIP1                                                                
101600     MOVE RP6-IDPTYP TO FEL-RP6-IDPTYP                                    
101700     MOVE RP6-IDARTNR TO FEL-RP6-IDARTNR                                  
101800     MOVE +1          TO FEL-RP6-KDCLAGER                                 
101900     MOVE RP6-FLMPB TO FEL-RP6-FLMPB                                      
102000     .                                                                    
102100     EJECT                                                                
102200 S10-RED-FELPOST-RP7 SECTION.                                             
102300     SKIP1                                                                
102400*********************************************************                 
102500*    HÄR SKER REDIGERING AV FELPOST DÅ POSTTYP = RP7    *                 
102600*********************************************************                 
102700     SKIP1                                                                
102800     MOVE RP7-IDPTYP TO FEL-RP7-IDPTYP                                    
102900     MOVE RP7-IDARTNR TO FEL-RP7-IDARTNR                                  
103000     MOVE +1           TO FEL-RP7-KDCLAGER                                
103100     MOVE RP7-FLOREGPB TO FEL-RP7-FLOREGPB                                
103200     .                                                                    
103300     EJECT                                                                
103400 S11-LAS-W22211 SECTION.                                                  
103500**********************************************                            
103600*    LÄS W22211 OCH ÖKA UPP POSTRÄKNAREN     *                            
103700**********************************************                            
103800     SKIP2                                                                
103900     READ W22211 INTO IRP-AREA                                            
104000     AT END                                                               
104010        SET END-OF-W22211 TO TRUE                                         
104020     NOT AT END                                                           
104030        MOVE 'W22211' TO POSTSUM-FDNAMN                                   
104040        MOVE 'W22212D1' TO POSTSUM-DDNAMN2                                
104050        MOVE RP1-IDPTYP TO POSTSUM-TRANSTYP                               
104060        CALL POSTSUM USING POSTSUM-PARM                                   
104100     END-READ                                                             
104200     .                                                                    
105000     EJECT                                                                
108700 S15-SKAPA-POST-TILL-W22216 SECTION.                                      
108800                                                                          
108900     MOVE ART-IDARTNR     TO W22216-IDARTNR                               
109100     PERFORM S16-SKRIV-W22216                                             
109400     .                                                                    
109600     EJECT                                                                
109700 S16-SKRIV-W22216 SECTION.                                                
109800                                                                          
109900     WRITE W22216-POST         FROM  W22216-AREA                          
110000     MOVE '22216'              TO POSTSUM-FDNAMN                          
110100     MOVE 'W22212D4'           TO POSTSUM-DDNAMN2                         
110200     MOVE SPACE                TO POSTSUM-TRANSTYP                        
110300     CALL POSTSUM USING POSTSUM-PARM                                      
110400     .                                                                    
110500     EJECT                                                                
110600 S17-SKRIV-W22217 SECTION.                                                
110700                                                                          
110720*****************************************************************         
110730*    SKRIV 2204-TRANSAR TILL W221P022  (HOPPA ÖVER WDG3 !)      *         
110750*****************************************************************         
110760                                                                          
110800     WRITE W22217-POST         FROM  W22217-AREA                          
110900     MOVE 'W22217'             TO POSTSUM-FDNAMN                          
111000     MOVE 'W22212D5'           TO POSTSUM-DDNAMN2                         
111100     MOVE '2204'               TO POSTSUM-TRANSTYP                        
111200     CALL POSTSUM USING POSTSUM-PARM                                      
111300     .                                                                    
111310     EJECT                                                                
111320 S18-SKRIV-W22214 SECTION.                                                
111330                                                                          
111340*****************************************************************         
111350*    SKRIV 2202-TRANSAR PÅ FILEN W22214                         *         
111360*****************************************************************         
111380     WRITE W22214-POST         FROM  2202-AREA                            
111390     MOVE 'W22214'             TO POSTSUM-FDNAMN                          
111391     MOVE 'W22212D6'           TO POSTSUM-DDNAMN2                         
111392     MOVE '2202'               TO POSTSUM-TRANSTYP                        
111393     CALL POSTSUM USING POSTSUM-PARM                                      
111394     .                                                                    
111395     EJECT                                                                
111396 S19-SKRIV-W22211 SECTION.                                                
111397                                                                          
111398*****************************************************************         
111399*    SKRIV OBEHANDLADE POSTER PÅ UTFILEN W22211                 *         
111400*****************************************************************         
111401                                                                          
111402     EVALUATE RP1-IDPTYP                                                  
111403     WHEN 'RP1'                                                           
111404       WRITE UT-RP1-POST FROM UT-RP1-AREA                                 
111405     WHEN 'RP2'                                                           
111406       WRITE UT-RP2-POST FROM UT-RP2-AREA                                 
111407     WHEN 'RP3'                                                           
111408       WRITE UT-RP3-POST FROM UT-RP3-AREA                                 
111409     WHEN 'RP4'                                                           
111410       WRITE UT-RP4-POST FROM UT-RP4-AREA                                 
111411     WHEN 'RP5'                                                           
111412       WRITE UT-RP5-POST FROM UT-RP5-AREA                                 
111413     WHEN 'RP6'                                                           
111414       WRITE UT-RP6-POST FROM UT-RP6-AREA                                 
111415     WHEN 'RP7'                                                           
111416       WRITE UT-RP7-POST FROM UT-RP7-AREA                                 
111417     WHEN OTHER                                                           
111418       CONTINUE                                                           
111419     END-EVALUATE                                                         
111420                                                                          
111422     MOVE 'W22211'       TO POSTSUM-FDNAMN                                
111423     MOVE 'W22212D6'     TO POSTSUM-DDNAMN2                               
111424     MOVE UT-RP1-IDPTYP  TO POSTSUM-TRANSTYP                              
111425     CALL POSTSUM USING POSTSUM-PARM                                      
111426     .                                                                    
111427     EJECT                                                                
111428* --- IMS SEKTIONER ---                                                   
111430     SKIP3                                                                
111500 IMS-GET-ARTIKELINF SECTION.                                              
111600     SKIP1                                                                
111800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X                             
111900            '&KDERS    ='  W-KDERS-0-X  ')'                               
112000     DELIMITED BY SIZE INTO SSA1                                          
112100     MOVE '  GE' TO GODK-STATUSKODER                                      
112200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
112300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
112400     PERFORM IMS-STATUSKONTROLL                                           
112500     .                                                                    
112600     EJECT                                                                
112700 IMS-GET-ARTC11 SECTION.                                                  
112800     SKIP1                                                                
112900     MOVE 'WLARTC11 ' TO SSA2                                             
113000     MOVE '  GE' TO GODK-STATUSKODER                                      
113100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA2                    
113200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     EJECT                                                                
113600 IMS-GHU-ARTC26 SECTION.                                                  
113700     SKIP1                                                                
113800     MOVE 'WLARTC11 ' TO SSA1                                             
113900     MOVE 'WLARTC26 ' TO SSA2                                             
114000     MOVE '  GE' TO GODK-STATUSKODER                                      
114100     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA3 SSA1 SSA2              
114200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
114300     PERFORM IMS-STATUSKONTROLL                                           
114900     .                                                                    
115000     EJECT                                                                
115100 IMS-GHU-ARTC11 SECTION.                                                  
115200     SKIP1                                                                
115400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
115500     DELIMITED BY SIZE INTO SSA1                                          
115600     MOVE  'WLARTC11 ' TO SSA2                                            
115700     MOVE '  ' TO GODK-STATUSKODER                                        
115800     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
115900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
118500     EJECT                                                                
118600 IMS-ISRT-ARTC26 SECTION.                                                 
118700     SKIP1                                                                
118900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
119000     DELIMITED BY SIZE INTO SSA1                                          
119100     MOVE 'WLARTC11 '  TO SSA2                                            
119200     MOVE 'WLARTC26 ' TO SSA3                                             
119300     MOVE '  ' TO GODK-STATUSKODER                                        
119400     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA3                        
119500     SSA1 SSA2 SSA3                                                       
119600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
119700     PERFORM IMS-STATUSKONTROLL                                           
119800     .                                                                    
119900     EJECT                                                                
120000 IMS-DLET-ARTC26 SECTION.                                                 
120100     SKIP1                                                                
120200     MOVE '  ' TO GODK-STATUSKODER                                        
120300     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA3                        
120400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
120500     PERFORM IMS-STATUSKONTROLL                                           
120600     .                                                                    
120700     EJECT                                                                
120800 IMS-REPL-ARTC11 SECTION.                                                 
120900     SKIP1                                                                
121000     MOVE '  ' TO GODK-STATUSKODER                                        
121100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA2                        
121200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
121300     PERFORM IMS-STATUSKONTROLL                                           
121400     .                                                                    
121500     EJECT                                                                
121600 IMS-REPL-ARTC26 SECTION.                                                 
121700     SKIP1                                                                
121800     MOVE '  ' TO GODK-STATUSKODER                                        
121900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA3                        
122000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
122100     PERFORM IMS-STATUSKONTROLL                                           
122200     .                                                                    
122600     SKIP3                                                                
122700 IMS-ISRT-R2207 SECTION.                                                  
122900     STRING 'WLXXAD01(WDG3KEY  =' W-WDG3KEY ')'                           
123000             DELIMITED BY SIZE INTO SSA1                                  
123100     MOVE 'WLXXAD11' TO SSA2                                              
123200     MOVE '  ' TO GODK-STATUSKODER                                        
123300     CALL CBLTDLI USING ISRT XXAD-PCB DLI-IO-AREA4 SSA1 SSA2              
123400     MOVE XXAD-STATUS-CODE TO STATUS-WS                                   
123500     PERFORM IMS-STATUSKONTROLL                                           
123600     .                                                                    
123700     EJECT                                                                
123720 IMS-STATUSKONTROLL SECTION.                                              
123730     SKIP2                                                                
123740     SET STATUS-IX TO 1                                                   
123750     SEARCH GODK-STATUS                                                   
123760       AT END                                                             
123770         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
123780           DELIMITED BY SIZE INTO FELTEXT                                 
123790         DISPLAY FELTEXT                                                  
123791         CALL FELLOG                                                      
123792       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
123793         CONTINUE                                                         
123794     END-SEARCH                                                           
123795     .                                                                    
123796     EJECT                                                                
123797*    -COPY WY2000P3                                                       
