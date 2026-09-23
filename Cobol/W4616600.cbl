000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4616600.                                                 
001000*AUTHOR.        THOMAS NILSSON.                                           
001100*DATE-WRITTEN.  JUNI 1986.                                                
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700*        PROGRAMMET SKAPAR KREDITERINGSTRANSAR RKB (HUVUD)                
001800*        OCH RKC (RADER) FÖR DISTRIKT 1283 TILL W46120                    
001900*        SOM VIDAREBEFODRAR DEM TILL VIPS                                 
002000                                                                          
002100                                                                          
002200                                                                          
002300                                                                          
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 CONFIGURATION SECTION.                                                   
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000*                                                                         
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*--- INFIL:                                                               
003400*                                                                         
003500     SELECT INFIL                    ASSIGN TO W46166D1.                  
003600     SKIP2                                                                
003700*--- UTFIL:                                                               
003800*                                                                         
003900     SELECT W46166                   ASSIGN TO W46166D2.                  
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  INFIL                                                                
004600     RECORDING      V                                                     
004700     BLOCK CONTAINS 0.                                                    
004800     SKIP2                                                                
004900*    -COPY W461021      -L.                                               
005100     SKIP2                                                                
005200 FD  W46166                                                               
005300     RECORDING      V                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*01  HUV-POST -COPY W461S021     -L.                                      
005800     SKIP2                                                                
005900*01  RAD-POST -COPY W461S022     -L.                                      
006100     SKIP2                                                                
006200*01  RKD-POST -COPY W461S023     -L.                                      
006400     SKIP2                                                                
006500*01  RKE-POST -COPY W461S024     -L.                                      
006700     EJECT                                                                
006710*01  RKJ-POST -COPY W461S025     -L.                                      
006720     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006810                                                                          
006900*    -- CHECKED BY WY2000                                                 
007400*                 ************       PROGRAM-NAMN.                        
007500 77  IDPGM                       PIC X(8)   VALUE 'W4616600'.             
007800*                                                                         
007900 77  LOPNR-HUV                   PIC S9(5)  VALUE +1   COMP-3.            
008000 77  LOPNR-RAD                   PIC S9(5)  VALUE +2   COMP-3.            
008100 77  POSTER-FINNS                PIC X(1)   VALUE 'N'.                    
008200 77  LOPNR-RKD                   PIC S9(5)  VALUE +0   COMP-3.            
008300 77  LOPNR-RKE                   PIC S9(5)  VALUE +0   COMP-3.            
008310 77  LOPNR-RKJ                   PIC S9(5)  VALUE +0   COMP-3.            
008400 77  FOERSTA-021                 PIC X(1)   VALUE 'J'.                    
008500     SKIP2                                                                
008600 01  GENERELLA-KONSTANTER.                                                
008700*                                                                         
008800     03  JA                      PIC X(1)    VALUE 'J'.                   
008900     03  NEJ                     PIC X(1)    VALUE 'N'.                   
009000     SKIP2                                                                
009100 01  SUBPROGRAM.                                                          
009200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009300     SKIP2                                                                
009400 01  END-OF-FILE-SWITCHAR.                                                
009500*                                                                         
009600     03  INFIL-EOF               PIC X(1)    VALUE 'N'.                   
009700     EJECT                                                                
009710                                                                          
009711 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
009712                                                                          
009720 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
009730*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
009731     EJECT                                                                
009732*01  FILLER  -COPY WWDIST07 -RED TEST-IDDISTR.                            
009740                                                                          
009800     EJECT                                                                
009900 01  ARBETSAREA                   PIC X(16)   VALUE                       
010000                                            'ARBETSAREA     '.            
010100 01  SPAR-AREA.                                                           
010200     03  SPAR-IDDISTR             PIC S9(5)   COMP-3.                     
010300     03  SPAR-IDKUNDNR            PIC S9(7)   COMP-3.                     
010400     03  SPAR-IDDC                PIC X(2).                               
010500     03  SPAR-IDKNOTNR            PIC S9(7)   COMP-3.                     
010600     SKIP2                                                                
010700 01  JFR-AREA.                                                            
010800     03  JFR-IDDISTR              PIC S9(5)   COMP-3.                     
010900     03  JFR-IDKUNDNR             PIC S9(7)   COMP-3.                     
011000     03  JFR-IDDC                 PIC X(2).                               
011100     03  JFR-IDKNOTNR             PIC S9(7)   COMP-3.                     
011200     SKIP2                                                                
011300 01  BERAKNINGSAREA.                                                      
011400     03  WS-PREMBHNT         PIC S9(7)V9(2) VALUE ZERO COMP-3.            
011800     03  WS-PRMOMS           PIC S9(7)V9(2) VALUE ZERO COMP-3.            
011810     03  WS-SUKRENTO         PIC S9(11)V9(2) VALUE ZERO COMP-3.           
011820     03  WS-SUKRETOT         PIC S9(11)V9(2) VALUE ZERO COMP-3.           
011900     EJECT                                                                
012000*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
012100*                                                                         
012200*01  -COPY W0005                                                          
012400     EJECT                                                                
012500 01  FILLER                       PIC X(16)   VALUE                       
012600                                            'IN-AREA-START  '.            
012700     SKIP2                                                                
012800*01  IN-AREA -COPY W461021 -L.                                            
012900*01  FILLER -COPY W461021 -RED IN-AREA                                    
013100     SKIP3                                                                
013200*01  AREA2  -COPY W461RKDN  -PRE IN-   -RED IN-AREA.                      
013400     SKIP3                                                                
013500*01  AREA3  -COPY W461RKEN  -PRE IN-   -RED IN-AREA.                      
013700     EJECT                                                                
013710*01  AREA3  -COPY W461RKJN  -PRE IN-   -RED IN-AREA.                      
013720     EJECT                                                                
013800 01  FILLER                       PIC X(16)   VALUE                       
013900                                            'UT-AREA-START  '.            
014000     SKIP2                                                                
014100*01  FILLER -COPY W461S021                                                
014300     EJECT                                                                
014400*01  FILLER -COPY W461S022                                                
014600     EJECT                                                                
014700*01  FILLER -COPY W461S023                                                
014900     EJECT                                                                
015000*01  FILLER -COPY W461S024                                                
015200     EJECT                                                                
015210*01  FILLER -COPY W461S025                                                
015220     EJECT                                                                
015300 PROCEDURE DIVISION.                                                      
015500     SKIP2                                                                
015600 STYR SECTION.                                                            
015700     SKIP2                                                                
015800     PERFORM A-INIT                                                       
015900                                                                          
016000     PERFORM UNTIL INFIL-EOF = JA                                         
016200       IF KRED-IDPTYP = '021'                                             
016300         IF FOERSTA-021 = JA                                              
016400           MOVE KRED-IDDISTR   TO SPAR-IDDISTR                            
016500           MOVE KRED-IDKUNDNR  TO SPAR-IDKUNDNR                           
016600           MOVE KRED-IDDC      TO SPAR-IDDC                               
016700           MOVE KRED-IDKNOTNR  TO SPAR-IDKNOTNR                           
016800           MOVE NEJ            TO FOERSTA-021                             
016900         END-IF                                                           
017000         IF SPAR-AREA = JFR-AREA                                          
017100           PERFORM B-RADER                                                
017200           PERFORM S01-LAS-INFIL                                          
017300         ELSE                                                             
017400           PERFORM C-HUVUD                                                
017500         END-IF                                                           
017600       ELSE                                                               
017700         EVALUATE TRUE                                                    
017800         WHEN KRED-IDPTYP = 'RKD'                                         
017900           PERFORM D-RKD-RADER                                            
018000           PERFORM S01-LAS-INFIL                                          
018100         WHEN KRED-IDPTYP = 'RKE'                                         
018200           PERFORM E-RKE-RADER                                            
018300           PERFORM S01-LAS-INFIL                                          
018310         WHEN KRED-IDPTYP = 'RKJ'                                         
018320           PERFORM F-RKJ-RADER                                            
018330           PERFORM S01-LAS-INFIL                                          
018400         END-EVALUATE                                                     
018500       END-IF                                                             
018600     END-PERFORM                                                          
018700     IF POSTER-FINNS = JA                                                 
018800       PERFORM C-HUVUD                                                    
018900     END-IF                                                               
019000     PERFORM Z-FINIT                                                      
019100     SKIP2                                                                
019200     MOVE ZERO TO RETURN-CODE                                             
019300     GOBACK                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 A-INIT SECTION.                                                          
019700     SKIP2                                                                
019800     OPEN INPUT INFIL                                                     
019900         OUTPUT W46166                                                    
020000     SKIP2                                                                
020100     PERFORM S01-LAS-INFIL                                                
020200     .                                                                    
021000     EJECT                                                                
021100 B-RADER  SECTION.                                                        
021200     SKIP2                                                                
021300     MOVE '021'              TO HUV-IDPTYP                                
021400     MOVE '022'              TO RAD-IDPTYP                                
021500     MOVE KRED-IDDISTR       TO RAD-IDDISTR                               
021600                   HUV-SOR0-IDDISTR  RAD-SOR0-IDDISTR                     
021610                                TEST-IDDISTR                              
021700     MOVE KRED-IDKUNDNR      TO RAD-IDKUNDNR                              
021800                   HUV-SOR0-IDKUNDNR RAD-SOR0-IDKUNDNR                    
021900     MOVE KRED-IDDC          TO HUV-IDDC                                  
022000     MOVE KRED-IDRAPPNR      TO HUV-IDRAPPNR                              
022100     MOVE KRED-IDKNOTNR      TO HUV-IDKNOTNR                              
022200     MOVE KRED-TIM-KN        TO HUV-TIM-KN                                
022300     MOVE KRED-IDFAKT        TO RAD-IDFAKT                                
022400     MOVE KRED-IDORDNR       TO RAD-IDORDNR                               
022500     MOVE KRED-IDRADNR       TO RAD-IDRADNR                               
022600     MOVE KRED-IDARTNR       TO RAD-IDARTNR                               
022700     MOVE KRED-REKSIFFR      TO RAD-REKSIFFR                              
022800     MOVE KRED-KDANMORS      TO RAD-KDANMORS                              
022900     MOVE KRED-KVKREANT      TO RAD-KVKREANT                              
023000     MOVE KRED-PRARTBTO      TO RAD-PRARTBTO                              
023010     MOVE KRED-KDPSLLOC      TO RAD-KDPSLLOC                              
023020     MOVE KRED-PRAVCOST      TO RAD-PRAVCOST                              
023030     MOVE KRED-PRAVCOST-CORE TO RAD-PRAVCOST-CORE                         
023100     MOVE ZERO               TO RAD-IDRONR RAD-TIRODAT                    
023200                                RAD-SOR0-IDRONR RAD-SOR0-TIRODAT          
023300                                HUV-SOR0-IDRONR HUV-SOR0-TIRODAT          
023400     MOVE LOPNR-RAD          TO RAD-IDLOPNR     RAD-SOR0-IDLOPNR          
023500     MOVE '007'              TO RAD-SOR0-IDPTYP HUV-SOR0-IDPTYP           
023600     ADD KRED-PREMBHNT       TO WS-PREMBHNT                               
023700     MOVE KRED-PRFRAKT       TO HUV-PRFRAKT                               
023800     MOVE KRED-PRLEGKST      TO HUV-PRLEGKST                              
023900     MOVE KRED-PRFOERS       TO HUV-PRFOERS                               
024000     ADD KRED-PRMOMS         TO WS-PRMOMS                                 
024010     MOVE KRED-SUVAT-FAKT    TO HUV-PRMOMS                                
024011     MOVE KRED-KDVALISO      TO HUV-KDVALISO                              
024013     MOVE KRED-SUKRENTO      TO HUV-SUKRENTO                              
024014     MOVE KRED-SUKRETOT      TO HUV-SUKRETOT                              
024015     MOVE KRED-PRARTBTO-LOC  TO RAD-PRARTBTO-LOC                          
024016     MOVE KRED-KDVAT         TO RAD-KDVAT                                 
024017     MOVE KRED-PRMOMS        TO RAD-PRMOMS-RAD                            
024018     ADD KRED-SULNELOC       TO WS-SUKRENTO                               
024019     MOVE KRED-SULNELOC      TO RAD-SULNELOC                              
024020     MOVE KRED-PRARTSTD      TO RAD-PRARTSTD                              
024021     MOVE KRED-PRARTSJK      TO RAD-PRARTSJK                              
024030                                                                          
024100     PERFORM S03-SKRIV-RAD                                                
024200     ADD +1 TO LOPNR-RAD                                                  
024300     .                                                                    
024400     EJECT                                                                
024500 C-HUVUD  SECTION.                                                        
024600     SKIP2                                                                
024700     MOVE LOPNR-HUV    TO HUV-SOR0-IDLOPNR                                
024800     MOVE WS-PREMBHNT  TO HUV-PREMBHNT                                    
024900                                                                          
025204***- NDC-NA MED IDFTG= 53/54 (LUFTKREDITNOTOR) OCH DEALER-PRICE           
025205***- SKAPAS I W41830 OCH HAR EJ MED DESSA FÄLT.                           
025206     IF DIST07-USA-RETAILER OR DIST07-CAN-RETAILER                        
025207       IF DIST79-DEALER-PRICE                                             
025209         MOVE WS-PRMOMS    TO HUV-PRMOMS                                  
025210         MOVE WS-SUKRENTO  TO HUV-SUKRENTO                                
025211                                                                          
025212         COMPUTE WS-SUKRETOT =  HUV-SUKRENTO +                            
025213                                HUV-PREMBHNT +                            
025214                                HUV-PRFRAKT  +                            
025215                                HUV-PRLEGKST +                            
025216                                HUV-PRFOERS  +                            
025217                                HUV-PRMOMS                                
025218         MOVE WS-SUKRETOT  TO HUV-SUKRETOT                                
025219       END-IF                                                             
025220     END-IF                                                               
025230                                                                          
025300     PERFORM S02-SKRIV-HUVUD                                              
025400     MOVE LOPNR-RAD TO LOPNR-HUV                                          
025500     ADD +1         TO LOPNR-RAD                                          
025600     MOVE JFR-AREA  TO SPAR-AREA                                          
025700     MOVE ZERO      TO WS-PREMBHNT                                        
026100                       WS-PRMOMS                                          
026110                       WS-SUKRENTO                                        
026120                       WS-SUKRETOT                                        
026200     MOVE     JA    TO FOERSTA-021                                        
026300     .                                                                    
026400     EJECT                                                                
026500 D-RKD-RADER SECTION.                                                     
026600     SKIP2                                                                
026700     MOVE IN-RKD-IDDISTR            TO RKD-SOR0-IDDISTR                   
026800     MOVE IN-RKD-IDKUNDNR           TO RKD-SOR0-IDKUNDNR                  
026900     MOVE ZERO                   TO RKD-SOR0-IDRONR                       
027000     MOVE ZERO                   TO RKD-SOR0-TIRODAT                      
027100     MOVE '023'                  TO RKD-SOR0-IDPTYP                       
027200     ADD   +1                    TO LOPNR-RKD                             
027300     MOVE LOPNR-RKD              TO RKD-SOR0-IDLOPNR                      
027400     MOVE '023'                  TO RKD-IDPTYP                            
027500     MOVE IN-RKD-IDDISTR            TO RKD-IDDISTR                        
027600     MOVE IN-RKD-IDKUNDNR           TO RKD-IDKUNDNR                       
027700     MOVE IN-RKD-IDDC               TO RKD-IDDC                           
027800     MOVE IN-RKD-IDRAPPNR           TO RKD-IDRAPPNR                       
027900     MOVE IN-RKD-IDORDNR            TO RKD-IDORDNR                        
028000     MOVE IN-RKD-IDKOLLI            TO RKD-IDKOLLI                        
028100     MOVE IN-RKD-IDARTNR            TO RKD-IDARTNR                        
028200     MOVE IN-RKD-REKSIFFR           TO RKD-REKSIFFR                       
028300     MOVE IN-RKD-IDRADNR            TO RKD-IDRADNR                        
028400     MOVE IN-RKD-KDKREBEH           TO RKD-KDKREBEH                       
028500     MOVE IN-RKD-KDANMORS           TO RKD-KDANMORS                       
028600     MOVE IN-RKD-KVLEVANM           TO RKD-KVLEVANM                       
028700     MOVE IN-RKD-PRARTBTO           TO RKD-PRARTBTO                       
028800     MOVE IN-RKD-FLSKROT            TO RKD-FLSKROT                        
028801     MOVE IN-RKD-KDVALISO           TO RKD-KDVALISO                       
028802     MOVE IN-RKD-PRARTBTO-LOC       TO RKD-PRARTBTO-LOC                   
028803     MOVE IN-RKD-SULNELOC           TO RKD-SULNELOC                       
028804     MOVE IN-RKD-PRARTSTD           TO RKD-PRARTSTD                       
028805     MOVE IN-RKD-PRARTSJK           TO RKD-PRARTSJK                       
028810                                                                          
028900     PERFORM  S04-SKRIV-RKD                                               
029000     .                                                                    
029100     EJECT                                                                
029200 E-RKE-RADER SECTION.                                                     
029300     SKIP2                                                                
029400     MOVE IN-RKE-IDDISTR            TO RKE-SOR0-IDDISTR                   
029500     MOVE IN-RKE-IDKUNDNR           TO RKE-SOR0-IDKUNDNR                  
029600     MOVE ZERO                   TO RKE-SOR0-IDRONR                       
029700     MOVE ZERO                   TO RKE-SOR0-TIRODAT                      
029800     MOVE '024'                  TO RKE-SOR0-IDPTYP                       
029900     ADD   +1                    TO LOPNR-RKE                             
030000     MOVE LOPNR-RKE              TO RKE-SOR0-IDLOPNR                      
030100     MOVE '024'                  TO RKE-IDPTYP                            
030200     MOVE IN-RKE-IDDISTR            TO RKE-IDDISTR                        
030300     MOVE IN-RKE-IDKUNDNR           TO RKE-IDKUNDNR                       
030400     MOVE IN-RKE-IDDC               TO RKE-IDDC                           
030500     MOVE IN-RKE-IDRAPPNR           TO RKE-IDRAPPNR                       
030600     MOVE IN-RKE-IDARTNR            TO RKE-IDARTNR                        
030700     MOVE IN-RKE-REKSIFFR           TO RKE-REKSIFFR                       
030800     MOVE IN-RKE-IDRADNR            TO RKE-IDRADNR                        
030900     MOVE IN-RKE-TIRETILL           TO RKE-TIRETILL                       
031000     MOVE IN-RKE-IDRAPPNR-002       TO RKE-IDRAPPNR-002                   
031100     PERFORM  S05-SKRIV-RKE                                               
031200     .                                                                    
031300     EJECT                                                                
031310 F-RKJ-RADER SECTION.                                                     
031320     SKIP2                                                                
031330     MOVE IN-RKJ-IDDISTR            TO RKJ-SOR0-IDDISTR                   
031340     MOVE IN-RKJ-IDKUNDNR           TO RKJ-SOR0-IDKUNDNR                  
031350     MOVE ZERO                      TO RKJ-SOR0-IDRONR                    
031360     MOVE ZERO                      TO RKJ-SOR0-TIRODAT                   
031370     MOVE '025'                     TO RKJ-SOR0-IDPTYP                    
031380     ADD   +1                       TO LOPNR-RKJ                          
031390     MOVE LOPNR-RKJ                 TO RKJ-SOR0-IDLOPNR                   
031391     MOVE '025'                     TO RKJ-IDPTYP                         
031392     MOVE IN-RKJ-IDDISTR            TO RKJ-IDDISTR                        
031393     MOVE IN-RKJ-IDKUNDNR           TO RKJ-IDKUNDNR                       
031395     MOVE IN-RKJ-IDRAPPNR           TO RKJ-IDRAPPNR                       
031396     MOVE IN-RKJ-IDRADNR            TO RKJ-IDRADNR                        
031397     MOVE IN-RKJ-KDANMORS           TO RKJ-KDANMORS                       
031398     MOVE IN-RKJ-IDARTNR            TO RKJ-IDARTNR                        
031399     MOVE IN-RKJ-KVLEVANM           TO RKJ-KVLEVANM                       
031401     MOVE IN-RKJ-DARTPMN            TO RKJ-DARTPMN                        
031402     MOVE IN-RKJ-KVDAGAR-RTATG      TO RKJ-KVDAGAR-RTATG                  
031405     PERFORM  S06-SKRIV-RKJ                                               
031406     .                                                                    
031407     EJECT                                                                
031410 S01-LAS-INFIL SECTION.                                                   
031500     SKIP2                                                                
031600*                                                                         
031700     READ INFIL INTO IN-AREA                                              
031800     AT END MOVE JA TO INFIL-EOF                                          
031900     END-READ                                                             
032000                                                                          
032100     IF INFIL-EOF = NEJ                                                   
032200       MOVE 'INFIL ' TO FDNAMN                                            
032300       MOVE 'W46166D1' TO DDNAMN2                                         
032400       MOVE KRED-IDPTYP TO TRANSTYP                                       
032500       CALL POSTSUM USING PARM                                            
032600       IF KRED-IDPTYP = '021'                                             
032700         MOVE JA TO POSTER-FINNS                                          
032800         MOVE KRED-IDDISTR  TO JFR-IDDISTR                                
032900         MOVE KRED-IDKUNDNR TO JFR-IDKUNDNR                               
033000         MOVE KRED-IDDC     TO JFR-IDDC                                   
033100         MOVE KRED-IDKNOTNR TO JFR-IDKNOTNR                               
033200       END-IF                                                             
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 S02-SKRIV-HUVUD SECTION.                                                 
033700     SKIP2                                                                
033800*                                                                         
033900     WRITE HUV-POST FROM HUV-W461S021                                     
034000                                                                          
034100     MOVE 'W46166' TO FDNAMN                                              
034200     MOVE 'W46166D2' TO DDNAMN2                                           
034300     MOVE '007'      TO TRANSTYP                                          
034400     CALL POSTSUM USING PARM                                              
034500     .                                                                    
034600     EJECT                                                                
034700 S03-SKRIV-RAD SECTION.                                                   
034800     SKIP2                                                                
034900*                                                                         
035000     WRITE RAD-POST FROM RAD-W461S022                                     
035100                                                                          
035200     MOVE 'W46166' TO FDNAMN                                              
035300     MOVE 'W46166D2' TO DDNAMN2                                           
035400     MOVE '007'      TO TRANSTYP                                          
035500     CALL POSTSUM USING PARM                                              
035600     .                                                                    
035700     EJECT                                                                
035800 S04-SKRIV-RKD SECTION.                                                   
035900     SKIP2                                                                
036000*                                                                         
036100     WRITE RKD-POST FROM RKD-W461S023-CTX                                 
036200                                                                          
036300     MOVE 'W46166' TO FDNAMN                                              
036400     MOVE 'W46166D2' TO DDNAMN2                                           
036500     MOVE '023'      TO TRANSTYP                                          
036600     CALL POSTSUM USING PARM                                              
036700     .                                                                    
036800     EJECT                                                                
036900 S05-SKRIV-RKE SECTION.                                                   
037000     SKIP2                                                                
037100*                                                                         
037200     WRITE RKE-POST FROM RKE-W461S024-CTX                                 
037300                                                                          
037400     MOVE 'W46166' TO FDNAMN                                              
037500     MOVE 'W46166D2' TO DDNAMN2                                           
037600     MOVE '024'      TO TRANSTYP                                          
037700     CALL POSTSUM USING PARM                                              
037800     .                                                                    
037900     EJECT                                                                
037910 S06-SKRIV-RKJ SECTION.                                                   
037920     SKIP2                                                                
037930*                                                                         
037940     WRITE RKJ-POST FROM RKJ-W461S025-CTX                                 
037950                                                                          
037960     MOVE 'W46166' TO FDNAMN                                              
037970     MOVE 'W46166D2' TO DDNAMN2                                           
037980     MOVE '025'      TO TRANSTYP                                          
037990     CALL POSTSUM USING PARM                                              
037991     .                                                                    
037992     EJECT                                                                
038000 Z-FINIT SECTION.                                                         
038100     SKIP2                                                                
038200     CLOSE INFIL                                                          
038300           W46166                                                         
038400     SKIP2                                                                
038500     MOVE 'S' TO OPKOD                                                    
038600     CALL POSTSUM USING PARM                                              
038700     .                                                                    
