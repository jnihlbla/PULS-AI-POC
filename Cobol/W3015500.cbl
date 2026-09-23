000100 PROCESS DYNAM                                                            
001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3015500.                                                
001500 AUTHOR.         GAVIN SMITH.                                             
001600 DATE-WRITTEN.   00/04/27.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        VV                                                               
002100*                                                                         
002201*        PROGRAMMET LÄSER      TABELL BYLRAD                              
002202*        PROGRAMMET LÄSER      TABELL BYLART                              
002203*        PROGRAMMET LÄSER      TABELL BYLDIS                              
002210*        PROGRAMMET LÄSER      TABELL BYLKUN                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W3T155                                              
002600*        MID:         W3I15501                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W3O15501                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W3015500'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004310                                                                          
004320 77  WS-KVANTAL                  PIC S9(7)   COMP-3 VALUE ZERO.           
004400                                                                          
004501*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004502 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004510 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004520 77  INDIEX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '3155'.                
005700     88  GODK-MID                            VALUE '3155' '3154'          
005800                                                   '3156'.                
006200     88  HELP-MID                            VALUE '0551'.                
                                                                                
005500 01  EXCL-IDDISTR                PIC 9(5).                                
005600     88  EXCL-IDDISTR-OK                     VALUE 07512                  
                                                         07625                  
                                                         06121                  
                                                         05619                  
                                                         06251                  
                                                         06200                  
                                                         06270.                 
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007701     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007710     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008601     EJECT                                                                
008602*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008603*                                                                         
008604 01  SPAR-AREA.                                                           
008605     03  SPAR-IDTRANS           PIC X(4)    VALUE '3155'.                 
008606     03  SPAR-IDDISTR-ENTER       PIC S9(4)        COMP-3.                
008610     03  SPAR-IDDISTR-NEXT        PIC S9(4)        COMP-3.                
008620     03  SPAR-OUT-TOT   PIC  S9(9) COMP-3.                                
008630     03  SPAR-IN-TOT    PIC  S9(9) COMP-3.                                
008640     03  SPAR-DIFF-TOT  PIC  S9(9) COMP-3.                                
008650     03  SPAR-PROC-TOT  PIC  S9(3) COMP-3.                                
008651     03  SPAR-RAD   OCCURS 13.                                            
008660       05  SPAR-IDDISTR             PIC S9(4)      COMP-3.                
008670       05  SPAR-DIFF-Q              PIC S9(7)      COMP-3.                
008680       05  SPAR-DIFF-P              PIC S9(3)      COMP-3.                
008690     03  FILLER                   PIC X(806).                             
008691     03  SPAR-3154-IDDISTR        PIC  x(4)    .                          
008692     03  SPAR-3155-IDDISTR        PIC  x(4)    .                          
008693     03  SPAR-3156-IDDEALR        PIC  x(6)    .                          
008694     03  filler                   PIC  x(31)   .                          
008695     03  SPAR-3154-IDARTNR        PIC  9(8)    .                          
008700     EJECT                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
009100     SKIP3                                                                
009102 01  ALT-IO-3154.                                                         
009103     03 M-SW-LL                PIC   S9(4)  VALUE +080 COMP SYNC.         
009104     03 M-SW-Z1-Z2             PIC    X(2)  VALUE LOW-VALUE.              
009105     03 M-SW-KDTRANS           PIC    X(8)  VALUE 'W3T154  '.             
009106     03 M-SW-IDTRANS           PIC    X(4)  VALUE '3155'.                 
009107     03 M-SW-KDMFSFOR          PIC    X(1)  VALUE '2'.                    
009108*03  -COPY W3I15401   -PRE  M-                                            
009109 01  ALT-IO-3156.                                                         
009110     03 N-SW-LL                PIC   S9(4)  VALUE +051 COMP SYNC.         
009111     03 N-SW-Z1-Z2             PIC    X(2)  VALUE LOW-VALUE.              
009112     03 N-SW-KDTRANS           PIC    X(8)  VALUE 'W3T156 7'.             
009113     03 N-SW-IDTRANS           PIC    X(4)  VALUE '3155'.                 
009114     03 N-SW-KDMFSFOR          PIC    X(1)  VALUE '2'.                    
009115*03  -COPY W3I15601   -PRE  N-                                            
009123 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009130     SKIP3                                                                
009200*01  MID -COPY W3I15501                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W3O15501                                                 
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200     SKIP3                                                                
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011002     03  W-IDDISTR-MIN-X.                                                 
011003         05  W-IDDISTR-MIN     PIC S9(4)        COMP-3.                   
011004 01  NYCKLAR-TILL-DB2.                                                    
011005*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011006*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011007     03  W-IDDISTR        PIC S9(5) COMP-3 VALUE ZERO.                    
011008     03  W-IDDISTR-TOM    PIC S9(5) COMP-3 VALUE 99999.                   
011009     03  W-TEST-IDDISTR   PIC S9(5) COMP-3 VALUE ZERO.                    
011010     03  W-DAAAVV-FOM     PIC X(6) VALUE ZERO.                            
011011     03  W-DAAAVV-TOM     PIC X(6) VALUE ZERO.                            
011012     03  W-IDARTNR        PIC S9(9) COMP-3 VALUE ZERO.                    
011013     03  W-IDKUNDNR       PIC 9(6) VALUE ZERO.                            
011020     03  W-DAAAVV         .                                               
011030       05  W-DAAAVV-DA     PIC 9(2) VALUE 20.                             
011040       05  W-DAAAVV-TI     PIC 9(4) VALUE ZERO.                           
011050     03  W-HOPP-INDIEX     PIC 9(2) VALUE ZERO.                           
011060     03  W-HOPP-CMD        PIC X    VALUE SPACE.                          
011100     SKIP2                                                                
011110*    --- WORKFIELDS FOR OUTPUT TO SCREEN                                  
011120 01  WORKFIELDS.                                                          
011130     03  WS-OUT   PIC  S9(9) VALUE ZERO COMP-3.                           
011140     03  WS-IN    PIC  S9(9) VALUE ZERO COMP-3.                           
011150     03  WS-DIFF  PIC  S9(9) VALUE ZERO COMP-3.                           
011160     03  WS-PROC  PIC  S9(3) VALUE ZERO COMP-3.                           
011170     03  WS-OUT-TOT   PIC  S9(9) VALUE ZERO COMP-3.                       
011180     03  WS-IN-TOT    PIC  S9(9) VALUE ZERO COMP-3.                       
011190     03  WS-DIFF-TOT  PIC  S9(9) VALUE ZERO COMP-3.                       
011191     03  WS-PROC-TOT  PIC  S9(3) VALUE ZERO COMP-3.                       
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012601     EJECT                                                                
012602 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
012603       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012604                                                                          
012605 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
012606 01  DB2-WS.                                                              
012607     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
012608         88  CURSOR-OK                       VALUE 000.                   
012609         88  RADER-FINNS                     VALUE 000.                   
012610         88  RADER-SAKNAS                    VALUE 100.                   
012611         88  ATKOMST-FEL                     VALUE 904.                   
012612     03  GODK-SQLCODEKODER.                                               
012613         05  GODK-SQLCODE OCCURS 5                                        
012620             INDEXED BY SQLCODE-IX PIC 9(3).                              
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013101     EJECT                                                                
013102*01  FILLER -COPY BYLRAD -PRE BYLRAD-                                     
013103     EJECT                                                                
013104*01  FILLER -COPY BYLART -PRE BYLART-                                     
013105     EJECT                                                                
013106*01  FILLER -COPY BYLDIST -PRE BYLDIST-                                   
013107     EJECT                                                                
013110*01  FILLER -COPY BYLKUND -PRE BYLKUND-                                   
013201     EJECT                                                                
013202 01  FILLER                      PIC X(16)   VALUE 'BYLRAD-AREA'.         
013203       EXEC SQL INCLUDE BYLRAD    END-EXEC.                               
013204     EJECT                                                                
013205 01  FILLER                      PIC X(16)   VALUE 'BYLART-AREA'.         
013206       EXEC SQL INCLUDE BYLART    END-EXEC.                               
013207     EJECT                                                                
013208 01  FILLER                      PIC X(16)   VALUE 'BYLDIS-AREA'.         
013209       EXEC SQL INCLUDE BYLDIST   END-EXEC.                               
013210     EJECT                                                                
013211 01  FILLER                      PIC X(16)   VALUE 'BYLKUN-AREA'.         
013220       EXEC SQL INCLUDE BYLKUND   END-EXEC.                               
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013510*01  -COPY W0009   -PRE ALT1-                                             
013520*01  -COPY W0009   -PRE ALT2-                                             
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION USING MSG-PCB ALT1-PCB ALT2-PCB                       
014002                                           USEA-PCB.                      
014003 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB                      
014100                                            USEA-PCB.                     
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014800         IF MFS-SPLIT                                                     
014900            PERFORM  G-NAESTA-BILD                                        
014901         ELSE                                                             
014902           IF MFS-FIRST                                                   
014903             PERFORM C-FOERSTA-SIDA                                       
014904           ELSE                                                           
014905             IF MFS-NEXT                                                  
014906               PERFORM D-NAESTA-SIDA                                      
014907             ELSE                                                         
014908               PERFORM E-SAMMA-SIDA                                       
014909             END-IF                                                       
014910           END-IF                                                         
015200           PERFORM F-LAES-VISA-INFO                                       
015210         END-IF                                                           
015300       END-IF                                                             
015400*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015500*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
015510      IF MFS-SPLIT                                                        
015520       CONTINUE                                                           
015530      ELSE                                                                
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O15501 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015710      END-IF                                                              
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I15501                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I15501                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W3O155N1' TO MFS-IDMOD                                         
018300     MOVE '3155' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019201     MOVE 'GB' TO MED-IDSKYLT                                             
019210     INITIALIZE GODK-SQLCODEKODER                                         
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '3155'            TO MSGI-IDTRANS                               
020300     IF GODK-MID                                                          
020400       MOVE MID-TIAAVV-FROM-IN TO  MSGI-TIAAVV-FOM                        
020410       MOVE MID-TIAAVV-TOM-IN  TO  MSGI-TIAAVV-TOM                        
020420       MOVE MID-IDARTNR-IN     TO  MSGI-IDARTNR                           
020430       MOVE MID-IDDISTR-IN     TO  MSGI-IDDISTR                           
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020710     MOVE MSGI-IDDISTR TO SPAR-3155-IDDISTR                               
020720     INSPECT MSGI-TIAAVV-FOM REPLACING LEADING SPACE  BY ZERO             
020730     INSPECT MSGI-TIAAVV-TOM REPLACING LEADING SPACE  BY ZERO             
020740     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE  BY ZERO                
020750     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE  BY ZERO                
020800     MOVE JA TO NYCKLAR-SW                                                
020900     IF MSGI-TIAAVV-FOM  NUMERIC                                          
021000               AND                                                        
021100        MSGI-TIAAVV-TOM  NUMERIC                                          
021110               AND                                                        
021120        MSGI-IDARTNR     NUMERIC                                          
021130               AND                                                        
021140        MSGI-IDDISTR     NUMERIC                                          
021151          MOVE MSGI-IDARTNR TO W-IDARTNR                                  
021152          MOVE MSGI-IDDISTR TO W-IDDISTR                                  
021153          IF W-IDDISTR > 0                                                
021154            MOVE W-IDDISTR TO W-IDDISTR-TOM                               
021155          END-IF                                                          
021156            MOVE MSGI-TIAAVV-FOM TO W-DAAAVV-TI                           
021157            MOVE W-DAAAVV        TO W-DAAAVV-FOM                          
021158            MOVE MSGI-TIAAVV-TOM TO W-DAAAVV-TI                           
021159            MOVE W-DAAAVV        TO W-DAAAVV-TOM                          
021161     ELSE                                                                 
021170        MOVE NEJ TO NYCKLAR-SW                                            
021180     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700       PERFORM MFS-RENSA-FAELT-IN                                         
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022107*CALCULATE THE TOTAL FIELDS .                                             
022108*  LÄS RADBASEN                                                           
022109     PERFORM DB2-DCL-OPN-BYLRAD-CRS                                       
022110     PERFORM DB2-FETCH-BYLRAD-CRS                                         
022111     PERFORM UNTIL RADER-SAKNAS                                           
022112       IF BYLRAD-IDPTYP = 'FAK'                                           
022113        ADD BYLRAD-KVANTAL TO WS-OUT-TOT                                  
022114       END-IF                                                             
022115       IF BYLRAD-IDPTYP = 'KRE'                                           
022116          COMPUTE WS-KVANTAL = BYLRAD-KVANTAL * -1                        
022117          ADD WS-KVANTAL     TO WS-OUT-TOT                                
022118       END-IF                                                             
022119       IF BYLRAD-IDPTYP = 'RET'                                           
              MOVE BYLRAD-IDDISTR TO EXCL-IDDISTR                               
              IF EXCL-IDDISTR-OK                                                
                 CONTINUE                                                       
              ELSE                                                              
022120           ADD BYLRAD-KVANTAL TO WS-IN-TOT                                
              END-IF                                                            
022121       END-IF                                                             
022122       PERFORM DB2-FETCH-BYLRAD-CRS                                       
022123     END-PERFORM                                                          
022124                                                                          
022125*  MOVE TO SPAR AREA                                                      
022126     COMPUTE                                                              
022127         WS-DIFF-TOT  = WS-IN-TOT - WS-OUT-TOT                            
022128     END-COMPUTE                                                          
022129     IF WS-OUT-TOT > 0                                                    
022130       COMPUTE                                                            
022131         WS-PROC-TOT  = (WS-DIFF-TOT * 100) / WS-OUT-TOT                  
022132       END-COMPUTE                                                        
022133     END-IF                                                               
022134     MOVE WS-PROC-TOT TO SPAR-PROC-TOT                                    
022135     MOVE WS-IN-TOT TO SPAR-IN-TOT                                        
022136     MOVE WS-DIFF-TOT TO SPAR-DIFF-TOT                                    
022137     MOVE WS-OUT-TOT TO SPAR-OUT-TOT                                      
022138                                                                          
022139     PERFORM DB2-CLOSE-BYLRAD-CRS                                         
022142                                                                          
022143     PERFORM MFS-RENSA-FAELT-IN                                           
022144     .                                                                    
022145     EJECT                                                                
022146 D-NAESTA-SIDA SECTION.                                                   
022147                                                                          
022148**   IF SPAR-IDTRANS = '3155'                                             
022149       IF SPAR-IDDISTR-NEXT NOT NUMERIC                                   
022150        MOVE ZERO TO SPAR-IDDISTR-NEXT                                    
022151       END-IF                                                             
022152       MOVE SPAR-IDDISTR-NEXT TO W-IDDISTR-MIN                            
022153                                 W-IDDISTR                                
022154**   ELSE                                                                 
022155       PERFORM MFS-RENSA-FAELT-IN                                         
022156**   END-IF                                                               
022157     .                                                                    
022158     EJECT                                                                
022159 E-SAMMA-SIDA SECTION.                                                    
022160                                                                          
022161     IF SPAR-IDDISTR-ENTER NOT NUMERIC                                    
022162       MOVE 0 TO SPAR-IDDISTR-ENTER                                       
022163     END-IF                                                               
022164     IF SPAR-IDTRANS = '3155' OR '0551'                                   
022165       MOVE SPAR-IDDISTR-ENTER TO W-IDDISTR-MIN                           
022166                                  W-IDDISTR                               
022167       IF MID-W3I15501 = ALL '+'                                          
022168         PERFORM MFS-RENSA-FAELT-IN                                       
022169       ELSE                                                               
022170         PERFORM MFS-RENSA-FAELT-IN                                       
022171*        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022172*        CALL WMEDKONV USING MED-WMEDAREA                                 
022173*        MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
022174*        PERFORM EA-MID-INDATA-TILL-MOD                                   
022175       END-IF                                                             
022176     ELSE                                                                 
022177       PERFORM MFS-RENSA-FAELT-IN                                         
022178     END-IF                                                               
022179     .                                                                    
022180     EJECT                                                                
022181*EA-MID-INDATA-TILL-MOD SECTION.                                          
022182*                                                                         
022183* * * * * FÖR VARJE MID-FÄLT                                              
022184* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
022185* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
022186* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
022190*    .                                                                    
022200*    EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500*READ IN PARTINFO INTO MOD-FIELDS  FOR HEADER AND FOOTER FIELDS           
023422                                                                          
023423     MOVE   W-DAAAVV-FOM(3:4)   TO MOD-TIAAVV-FOM                         
023424     MOVE   W-DAAAVV-TOM(3:4)   TO MOD-TIAAVV-TOM                         
023425     MOVE   W-IDARTNR           TO MOD-IDARTNR                            
023426     MOVE   SPAR-OUT-TOT        TO MOD-OUT-TOT                            
023427     MOVE   SPAR-IN-TOT         TO MOD-IN-TOT                             
023428     MOVE   SPAR-DIFF-TOT       TO MOD-DIFF-TOT                           
023429     MOVE   SPAR-PROC-TOT       TO MOD-PROC-TOT                           
023430     PERFORM DB2-SELECT-BYLART                                            
023431     IF RADER-FINNS                                                       
023432       MOVE BYLART-BEART-ENG    TO MOD-BEART                              
023433     ELSE                                                                 
023434       MOVE 'DESCRIPTION MISSING'    TO MOD-BEART                         
023435     END-IF                                                               
023436*READ IN PARTINFO INTO MOD-FIELDS  FOR 13 DETAIL LINES                    
023437*CREATE CURSOR                                                            
023438     PERFORM DB2-DCL-OPN-BYLRAD-CRS                                       
023439     MOVE 1 TO INDIEX                                                     
023440     PERFORM DB2-FETCH-BYLRAD-CRS                                         
023441     IF RADER-FINNS                                                       
023442      MOVE BYLRAD-IDDISTR TO SPAR-IDDISTR-ENTER                           
023443                             W-TEST-IDDISTR                               
023444     ELSE                                                                 
023445      MOVE ZERO           TO SPAR-IDDISTR-ENTER                           
023446                             W-TEST-IDDISTR                               
023447     END-IF                                                               
023448                                                                          
023449     PERFORM UNTIL INDIEX > 13                                            
023450       PERFORM UNTIL RADER-SAKNAS  OR INDIEX > 13                         
023451         PERFORM UNTIL RADER-SAKNAS                                       
023452                 OR (W-TEST-IDDISTR NOT = BYLRAD-IDDISTR)                 
023453*  LÄGG UT RAD DATA I MOD.N                                               
023454           IF BYLRAD-IDPTYP = 'FAK'                                       
023455             ADD  BYLRAD-KVANTAL TO WS-OUT                                
023456           END-IF                                                         
023457           IF BYLRAD-IDPTYP = 'KRE'                                       
023458              COMPUTE WS-KVANTAL = BYLRAD-KVANTAL * -1                    
023459              ADD WS-KVANTAL     TO WS-OUT                                
023460           END-IF                                                         
023461           IF BYLRAD-IDPTYP = 'RET'                                       
023462             ADD  BYLRAD-KVANTAL TO WS-IN                                 
023463           END-IF                                                         
023464*  FETCH NEXT RECORD                                                      
023465           PERFORM DB2-FETCH-BYLRAD-CRS                                   
023466         END-PERFORM                                                      
023467*  CALCULATE DIFF FIELDS                                                  
023468         COMPUTE  WS-DIFF =  WS-IN - WS-OUT                               
023469         IF WS-OUT > 0                                                    
023470          COMPUTE  WS-PROC =  (WS-DIFF * 100) / WS-OUT                    
023471         END-IF                                                           
023472*  SKRIV UT DISTRIKTSRAD                                                  
023473         MOVE ' '                   TO  MOD-CMD-UT(INDIEX)                
023474         MOVE W-TEST-IDDISTR        TO  MOD-IDDISTR(INDIEX)               
023475                                        SPAR-IDDISTR(INDIEX)              
023476         MOVE WS-OUT                TO  MOD-OUTLEV(INDIEX)                
023477         MOVE WS-IN                 TO  MOD-INLEV(INDIEX)                 
023478         MOVE WS-DIFF               TO  MOD-DIFF(INDIEX)                  
023479                                        SPAR-DIFF-Q(INDIEX)               
023480         MOVE WS-PROC               TO  MOD-PROC(INDIEX)                  
023481                                        SPAR-DIFF-P(INDIEX)               
023482         MOVE ZERO TO WS-OUT WS-IN WS-DIFF WS-PROC                        
023483         ADD 1 TO INDIEX                                                  
023484*  MOVE NEW DIST TO TESTDIST                                              
023485         IF RADER-FINNS                                                   
023486           MOVE BYLRAD-IDDISTR TO W-TEST-IDDISTR                          
023487           IF INDIEX > 13                                                 
023488             MOVE BYLRAD-IDDISTR TO SPAR-IDDISTR-NEXT                     
023489             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
023490             CALL WMEDKONV USING MED-WMEDAREA                             
023491             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
023492           ELSE                                                           
023493             IF SPAR-IDDISTR-ENTER NOT NUMERIC                            
023494              MOVE ZERO TO SPAR-IDDISTR-ENTER                             
023495             END-IF                                                       
023496             MOVE SPAR-IDDISTR-ENTER TO SPAR-IDDISTR-NEXT                 
023497           END-IF                                                         
023498         END-IF                                                           
023499       END-PERFORM                                                        
023500       PERFORM UNTIL INDIEX > 13                                          
023501*  FYLL UT MED TOMRADER                                                   
023502*  SKRIV UT DISTRIKTSRAD                                                  
023503         MOVE MFS-RENSA-FAELT       TO  MOD-CMD-UT(INDIEX)                
023504         MOVE MFS-RENSA-FAELT       TO  MOD-IDDISTR(INDIEX)               
023505         MOVE MFS-RENSA-FAELT       TO  MOD-OUTLEV(INDIEX)                
023506         MOVE MFS-RENSA-FAELT       TO  MOD-INLEV(INDIEX)                 
023507         MOVE MFS-RENSA-FAELT       TO  MOD-DIFF(INDIEX)                  
023508         MOVE MFS-RENSA-FAELT       TO  MOD-PROC(INDIEX)                  
023509         MOVE ZERO                  TO  SPAR-IDDISTR(INDIEX)              
023510         MOVE ZERO                  TO  SPAR-DIFF-P(INDIEX)               
023511         MOVE ZERO                  TO  SPAR-DIFF-Q(INDIEX)               
023512         ADD 1 TO INDIEX                                                  
023513       END-PERFORM                                                        
023514     PERFORM DB2-CLOSE-BYLRAD-CRS                                         
023515     END-PERFORM                                                          
023516*************************                                                 
023517*      IF SEGMENT-FINNS                                                   
023518*        MOVE SSSS-IDDISTR TO SPAR-IDDISTR-NEXT                           
023519*        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
023520*        CALL WMEDKONV USING MED-WMEDAREA                                 
023521*        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023522*      ELSE                                                               
023523*        MOVE SSSS-IDDISTR TO SPAR-IDDISTR-NEXT                           
023524*      END-IF                                                             
023525                                                                          
023526       MOVE '002'      TO MSGI-KDCALL                                     
023527       MOVE '3155'   TO SPAR-IDTRANS                                      
023528       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023530       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023600                                                                          
024510     .                                                                    
024700     EJECT                                                                
024710 G-NAESTA-BILD SECTION.                                                   
024711* KOLLA INPUTFÄLT FÖR                                                     
024712     MOVE 1 TO INDIEX                                                     
024713     PERFORM UNTIL INDIEX > 13                                            
024714       IF MID-CMD-IN(INDIEX) = 'X' OR 'S' OR 'x' OR 's'                   
024715        MOVE MID-CMD-IN(INDIEX) TO W-HOPP-CMD                             
024716        MOVE INDIEX             TO W-HOPP-INDIEX                          
024718        MOVE 98 TO INDIEX                                                 
024719       END-IF                                                             
024720       ADD 1 TO INDIEX                                                    
024721     END-PERFORM                                                          
024722     EVALUATE W-HOPP-CMD                                                  
024740      WHEN 'S'                                                            
024742        MOVE ALL '+'  TO N-MID-W3I15601                                   
024743         MOVE SPAR-IDDISTR(W-HOPP-INDIEX)  TO N-MID-IDDISTR-IN            
024744        PERFORM IMS-INSERT-ALT-3156                                       
024745      WHEN OTHER                                                          
024746        MOVE ALL '+'  TO M-MID-W3I15401                                   
024747        MOVE MSGI-TIAAVV-FOM TO M-MID-TIAAVV-FOM-IN                       
024748        MOVE MSGI-TIAAVV-TOM TO M-MID-TIAAVV-TOM-IN                       
024749        MOVE SPAR-3154-IDDISTR TO M-MID-IDDISTR-IN                        
024750        MOVE SPACE           TO M-MID-IDDISTR-UT                          
024751*       MOVE msgi-IDFKNGRP TO M-MID-IDFKNGRP-IN                           
024752        MOVE SPACE           TO M-MID-IDFKNGRP-UT                         
024753        MOVE SPAR-3154-IDARTNR TO M-MID-IDARTNR-IN                        
024754        MOVE SPACE           TO M-MID-IDARTNR-UT                          
024755        PERFORM IMS-INSERT-ALT-3154                                       
024756     END-EVALUATE                                                         
024757     .                                                                    
024760     EJECT                                                                
024800 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025200     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
025300                             MOD-IDARTNR                                  
025301                             MOD-TIAAVV-FOM                               
025302                             MOD-TIAAVV-TOM                               
025310     MOVE 1 TO INDIEX                                                     
025320     PERFORM UNTIL INDIEX > 13                                            
025330     MOVE MFS-RENSA-FAELT TO MOD-CMD-UT(INDIEX)                           
025340                             MOD-IDDISTR(INDIEX)                          
025350                             MOD-OUTLEV(INDIEX)                           
025360                             MOD-INLEV(INDIEX)                            
025370                             MOD-DIFF(INDIEX)                             
025380                             MOD-PROC(INDIEX)                             
025390     ADD 1 TO INDIEX                                                      
025391     END-PERFORM                                                          
025392     MOVE MFS-RENSA-FAELT TO MOD-OUT-TOT                                  
025393                             MOD-IN-TOT                                   
025394                             MOD-DIFF-TOT                                 
025395                             MOD-PROC-TOT                                 
025400     .                                                                    
025501     SKIP3                                                                
025502*MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025503*                                                                         
025504*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025505*    MOVE 1 TO INDIEX                                                     
025506*    PERFORM UNTIL INDIEX > 13                                            
025507*    MOVE MFS-RENSA-FAELT TO MOD-CMD-UT(INDIEX)                           
025508*                            MOD-IDDISTR(INDIEX)                          
025509*                            MOD-OUTLEV(INDIEX)                           
025510*                            MOD-INLEV(INDIEX)                            
025511*                            MOD-DIFF(INDIEX)                             
025512*                            MOD-PROC(INDIEX)                             
025513*    ADD 1 TO INDIEX                                                      
025514*    END-PERFORM                                                          
025515*    MOVE MFS-RENSA-FAELT TO MOD-OUT-TOT                                  
025516*                            MOD-IN-TOT                                   
025517*                            MOD-DIFF-TOT                                 
025518*                            MOD-PROC-TOT                                 
025520*    .                                                                    
025600*    SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800     CONTINUE                                                             
025900*    --- ALLA INDATA-FÄLT                                                 
026000*    MOVE MFS-RENSA-FAELT TO MOD-XXXXXXXX-IN                              
026100*                               MOD-XXXXXXXX-IN                           
026200     .                                                                    
026300*    EJECT                                                                
026400*MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500*                                                                         
026600*    --- ALLA UTDATA-FÄLT                                                 
026610*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
026700*    MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
026701*                              MOD-IDARTNR                                
026702*                              MOD-TIAAVV-FOM                             
026703*                              MOD-TIAAVV-TOM                             
026704*    MOVE 1 TO INDIEX                                                     
026705*    PERFORM UNTIL INDIEX > 13                                            
026706*    MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-UT(INDIEX)                         
026707*                            MOD-IDDISTR(INDIEX)                          
026708*                            MOD-OUTLEV(INDIEX)                           
026709*                            MOD-INLEV(INDIEX)                            
026710*                            MOD-DIFF(INDIEX)                             
026711*                            MOD-PROC(INDIEX)                             
026712*    ADD 1 TO INDIEX                                                      
026713*    END-PERFORM                                                          
026714*    MOVE MFS-ROER-EJ-FAELT TO MOD-OUT-TOT                                
026715*                              MOD-IN-TOT                                 
026716*                              MOD-DIFF-TOT                               
026717*                              MOD-PROC-TOT                               
026718*    .                                                                    
026719*    EJECT                                                                
027007*MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027008*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027009*                                                                         
027010*    MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
027011*                              MOD-IDARTNR                                
027012*                              MOD-TIAAVV-FOM                             
027013*                              MOD-TIAAVV-TOM                             
027014*    MOVE 1 TO INDIEX                                                     
027015*    PERFORM UNTIL INDIEX > 13                                            
027016*    MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-UT(INDIEX)                         
027017*                            MOD-IDDISTR(INDIEX)                          
027018*                            MOD-OUTLEV(INDIEX)                           
027019*                            MOD-INLEV(INDIEX)                            
027020*                            MOD-DIFF(INDIEX)                             
027021*                            MOD-PROC(INDIEX)                             
027022*    ADD 1 TO INDIEX                                                      
027023*    END-PERFORM                                                          
027024*    MOVE MFS-ROER-EJ-FAELT TO MOD-OUT-TOT                                
027025*                              MOD-IN-TOT                                 
027026*                              MOD-DIFF-TOT                               
027027*                              MOD-PROC-TOT                               
027100*    .                                                                    
027200*    SKIP3                                                                
027300*MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400*                                                                         
027500*    --- ALLA INDATA-FÄLT                                                 
027510*    MOVE 1 TO INDIEX                                                     
027520*    PERFORM UNTIL INDIEX > 13                                            
027600*     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-UT (INDIEX)                       
027700*     ADD 1 TO INDIEX                                                     
027710*    END-PERFORM                                                          
027800*    .                                                                    
027900*    EJECT                                                                
028000*MFS-FORM-ATTR SECTION.                                                   
028100*                                                                         
028200*    --- ALLA INDATA-FÄLT                                                 
028300*    MOVE MFS-FORMATETS-ATTR TO MOD-XXXXXXXX-ATTR                         
028400*                               MOD-XXXXXXXX-ATTR                         
028500*    .                                                                    
028600*    SKIP2                                                                
028700*MFS-LAES-IN-IGEN SECTION.                                                
028800*                                                                         
028900*    --- ALLA INDATA-FÄLT                                                 
028910*    MOVE 1 TO INDIEX                                                     
028920*    PERFORM UNTIL INDIEX > 13                                            
029000*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-UT-ATTR(INDIEX)                
029010*    ADD 1 TO INDIEX                                                      
029100*    END-PERFORM                                                          
029200*    .                                                                    
029300*    EJECT                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030600     IF MSGI-IDLAND-SPR = 'GB'                                            
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031600     EJECT                                                                
031601 IMS-INSERT-ALT-3154 SECTION.                                             
031602                                                                          
031603     MOVE SPACE TO GODK-STATUSKODER                                       
031604     CALL CBLTDLI USING ISRT ALT1-PCB ALT-IO-3154                         
031605     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
031606     PERFORM IMS-STATUSKONTROLL                                           
031607     .                                                                    
031608     EJECT                                                                
031610 IMS-INSERT-ALT-3156 SECTION.                                             
031620                                                                          
031630     MOVE SPACE TO GODK-STATUSKODER                                       
031640     CALL CBLTDLI USING ISRT ALT2-PCB ALT-IO-3156                         
031650     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
031660     PERFORM IMS-STATUSKONTROLL                                           
031670     .                                                                    
031680     EJECT                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
032901     EJECT                                                                
032902 DB2-DCL-OPN-BYLRAD-CRS  SECTION.                                         
032903                                                                          
032904     MOVE 000100  TO GODK-SQLCODEKODER                                    
032905                                                                          
032906     EXEC SQL                                                             
032907         DECLARE BYLRAD-CRS CURSOR FOR                                    
032908                                                                          
032909           SELECT  IDARTNR, DAAAVV, IDPTYP,                               
032910                    IDDISTR,  KVANTAL                                     
032911                                                                          
032912*         RÄKNA UPP ALLA KOLUMNER SOM SKA SELECTAS MED                    
032913*         KOMMATECKEN MELLAN VARJE                                        
032914*                                                                         
032915*                                                                         
032916*          INTO   :BYLRAD-IDARTNR, :BYLRAD-DAAAVV, :BYLRAD-IDPTYP,        
032917*                 :BYLRAD-IDDISTR, :BYLRAD-KVANTAL                        
032918*                                                                         
032919*         RÄKNA UPP ALLA HOST-VARIABLER MED KOMMATECKEN                   
032920*         VARJE VARIABEL SKA FÖREGÅS AV ETT KOLON                         
032921                                                                          
032922                                                                          
032923           FROM    BYLRAD                                                 
032924                                                                          
032925           WHERE   IDARTNR =  :W-IDARTNR                                  
032926           AND     DAAAVV  >= :W-DAAAVV-FOM                               
032927           AND     DAAAVV  <= :W-DAAAVV-TOM                               
032928           AND     IDDISTR >= :W-IDDISTR                                  
032929           AND     IDDISTR <= :W-IDDISTR-TOM                              
032930                                                                          
032931           ORDER BY IDDISTR                                               
032932     END-EXEC                                                             
032933                                                                          
032934     MOVE 000100  TO GODK-SQLCODEKODER                                    
032935     EXEC SQL OPEN BYLRAD-CRS END-EXEC                                    
032936                                                                          
032937     .                                                                    
032938     SKIP3                                                                
032939 DB2-FETCH-BYLRAD-CRS  SECTION.                                           
032940     SKIP2                                                                
032941     MOVE 000100  TO GODK-SQLCODEKODER                                    
032942     EXEC SQL                                                             
032943         FETCH BYLRAD-CRS                                                 
032944           INTO   :BYLRAD-IDARTNR, :BYLRAD-DAAAVV, :BYLRAD-IDPTYP,        
032945                  :BYLRAD-IDDISTR, :BYLRAD-KVANTAL                        
032946     END-EXEC                                                             
032947                                                                          
032948     MOVE SQLCODE TO SQLCODE-WS                                           
032949     PERFORM DB2-STATUS-KONTROLL                                          
032950     .                                                                    
032951     SKIP3                                                                
032952 DB2-CLOSE-BYLRAD-CRS  SECTION.                                           
032953                                                                          
032954     EXEC SQL CLOSE BYLRAD-CRS END-EXEC                                   
032955     .                                                                    
032956     EJECT                                                                
033100 DB2-SELECT-BYLART SECTION.                                               
033101     MOVE 000100         TO GODK-SQLCODEKODER                             
033102     EXEC SQL                                                             
033103          SELECT                                                          
033104            BEART_ENG                                                     
033105          INTO                                                            
033106            :BYLART-BEART-ENG                                             
033107          FROM BYLART                                                     
033108            WHERE IDARTNR = :W-IDARTNR                                    
033109     END-EXEC                                                             
033110     MOVE SQLCODE        TO SQLCODE-WS                                    
033111     PERFORM DB2-STATUS-KONTROLL                                          
033112     .                                                                    
033113     EJECT                                                                
033114 DB2-STATUS-KONTROLL  SECTION.                                            
033115                                                                          
033116     SET SQLCODE-IX TO 1                                                  
033117     SEARCH GODK-SQLCODE                                                  
033118       AT END CALL FELLOG                                                 
033119       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
033120     END-SEARCH                                                           
033130     .                                                                    
