000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476STAT.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/09/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SUBPROGRAM TO WRITE 'CUSTOM INFORMATION' TRANSPORT               
000900*        DOCUMENT. IT IS CALLED BY A PROGRAM W40631. DOCUMENT             
001000*        SHOWS STAT.NR., COUNTRY OF ORIGIN, DELIVERED QUANTITY,           
001100*        NET WEIGHT AND TOTAL.                                            
001200*        AT THE END OF THE DOCUMENT IT SHOWS THE TOTAL FOR THE            
001300*        FULL SHIPMENT/DIST                                               
001400*                                                                         
001500*        THE PROGRAM READS     WDE1                                       
001600*                                                                         
001700*    ABENDCODES:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)   VALUE 'W476STAT'.             
003700 01  ERRTEXT.                                                             
003800     03 FILLER                   PIC X(8)   VALUE SPACE.                  
003900     03 ERRTEXT-STR              PIC X(72)  VALUE SPACE.                  
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004200 77  YES                         PIC X      VALUE 'J'.                    
004300 77  NOO                         PIC X      VALUE 'N'.                    
004400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  IA                          PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  IB                          PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  ISM                         PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  WS-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  IXURS                       PIC S9(4)  COMP   VALUE ZERO.            
005000 77  IXSTATNR                    PIC S9(4)  COMP   VALUE ZERO.            
005100 77  IXARTURS                    PIC S9(4)  COMP   VALUE ZERO.            
005200*77  TOTAL-VKORDNTO              PIC S9(11)V9 COMP-3  VALUE ZERO.         
005210 77  TOTAL-VKORDNTO             PIC S9(11)V999 COMP-3 VALUE ZERO.         
005300 77  TOTAL-VKORDBTO              PIC S9(11)V9 COMP-3  VALUE ZERO.         
005400 77  TOTAL2-VKORDBTO             PIC S9(11)V9 COMP-3  VALUE ZERO.         
005500 77  TOTAL3-VKORDBTO             PIC S9(11)V9 COMP-3  VALUE ZERO.         
005600 77  WX-BTO                      PIC S9(11)V99 COMP-3 VALUE ZERO.         
005700 77  WX-RYSS                     PIC S9(5)    COMP-3  VALUE 2640.         
005710 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005720 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005800                                                                          
005900 77  IXURS-PRESENT-SW            PIC X(1)   VALUE 'N'.                    
006000     88  IXURS-PRESENT                      VALUE 'J'.                    
006100                                                                          
006200 77  DIST-RYSSLAND-SW            PIC X(1)   VALUE 'N'.                    
006300     88  DIST-RYSSLAND                      VALUE 'J'.                    
006400                                                                          
006500 77  W-PAGE-NO                   PIC S9(3)  COMP-3 VALUE ZERO.            
006510*                                                                         
006520 01  TEST-IDDISTR               PIC 9(5)   COMP-3.                        
006530*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
006540*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
006550     EJECT                                                                
006570*01    -COPY WWDC99                                                       
006580                                                                          
006600                                                                          
006700 01  W-YYMMDD                    PIC 9(06)  VALUE ZERO.                   
006800                                                                          
006900 01  W-LINE.                                                              
007000     03  W-LINE-COUNT            PIC 9(02)  VALUE ZERO.                   
007100     03  W-LINE-MAX              PIC 9(02)  VALUE 41.                     
007200                                                                          
007300*    --- STYRTECKEN PRINTER                                               
007400 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
007500 01  WS-SKIP1                      PIC X      VALUE ' '.                  
007600 01  WS-SKIP2                      PIC X      VALUE '0'.                  
007700 01  WS-SKIP3                      PIC X      VALUE '-'.                  
007800                                                                          
007900 01  WS-TYP-IDSHIP                 PIC X(18) VALUE                        
008000                                   'CUSTOM INFORMATION'.                  
008100                                                                          
       01  WS-META                       PIC X(5) VALUE '¤META'.                
       01  WS-IDDISTR                    PIC Z(4)9.                             
       01  WS-IDSHIPM-Z                  PIC Z(6)9.                             
       01  WS-TIMESTAMP.                                                        
           03  FILLER                  PIC X       VALUE 'D'.                   
           03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
           03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
           03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
           03  FILLER                  PIC X       VALUE '_'.                   
           03  FILLER                  PIC X       VALUE 'T'.                   
           03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
           03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
           03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
                                                                                
008200 01  TODAYS-DATE                 PIC 9(6)   VALUE ZERO.                   
008300 01  FILLER REDEFINES TODAYS-DATE.                                        
008400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008600     03  TODAYS-DATE-DAY         PIC 9(2).                                
008700     EJECT                                                                
008800 77  FLLOCCUR-SW                 PIC X(01)  VALUE 'N'.                    
008900     88 FLLOCCUR                            VALUE 'J'.                    
009000                                                                          
009100 01  WS-MONEY                    PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
009200 01  WY-MONEY                    PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
009300 01  WS-REVALUTA-LOCCUR          PIC S9(5)      COMP-3 VALUE 1.           
009400 01  WS-PRKURS-LOCCUR            PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
009410 01  WS-PRKURS-INR               PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
009500 01  ST-URS-TOT-SUORDV-TOT-INR   PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
009501 01  ST-URS-SUBTOT-SUORDV-TOT-INR                                         
009502                                 PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
009503 01  ST-URS-SUORDV-TOT-INR       PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
009510                                                                          
009600 01  ARB-RAD                     PIC X(132)  VALUE SPACE.                 
009700                                                                          
009800 01  RAD-HEAD.                                                            
009900     03  FILLER                  PIC X(15).                               
010000     03  RAD1H-IMPORTER-TEXT     PIC X(13).                               
010100     03  FILLER                  PIC X(2).                                
010200     03  RAD1H-IMPORTER          PIC X(35).                               
010300     03  FILLER                  PIC X(2).                                
010400*    03  FILLER                  PIC X(67).                               
010500     03  RAD-TYP-IDSHIP          PIC X(25).                               
010600                                                                          
010700 01  RAD2-HEAD.                                                           
010800     03  FILLER                  PIC X(30).                               
010900     03  RAD2H-IMPORTER          PIC X(35).                               
011000                                                                          
011100 01  RAD3-HEAD.                                                           
011200     03  FILLER                  PIC X(30).                               
011300     03  RAD3H-IMPORTER          PIC X(35).                               
011400                                                                          
011500 01  RAD5-HEAD.                                                           
011600     03  FILLER                  PIC X(30).                               
011700     03  RAD5H-IMPORTER          PIC X(35).                               
011800                                                                          
011900 01  RAD1.                                                                
012000     03  FILLER                  PIC X(30).                               
012100     03  RAD4H-IMPORTER          PIC X(35).                               
012200     03  FILLER                  PIC X(02).                               
012300**   03  FILLER                  PIC X(67).                               
012400     03  RAD1-TIAAMMDD           PIC 9(06).                               
012500     03  RAD1-IDDISTR            PIC Z(4)9.                               
012600     03  FILLER                  PIC X(01).                               
012700     03  RAD1-IDSHIPM            PIC Z(06)9.                              
012800     03  FILLER                  PIC X(04).                               
012900     03  RAD1-IDTRPTNR           PIC Z(02)9.                              
013000     03  FILLER                  PIC X(01).                               
013100     03  RAD1-IDLBBET            PIC X(12).                               
013200     03  FILLER                  PIC X(02).                               
013300     03  RAD1-PAGE-NO            PIC Z(03).                               
013400                                                                          
013500 01  RAD-2.                                                               
013600     05  FILLER                  PIC X(1).                                
013700     05  RAD2-IDSTATNR           PIC X(9).                                
013800     05  FILLER                  PIC X(3).                                
013900     05  RAD2-KDARTURS           PIC X(3).                                
014000     05  FILLER                  PIC X(8).                                
014100     05  RAD2-KVART              PIC X(9)  JUSTIFIED RIGHT.               
014200**** 05  FILLER                  PIC X(01).                               
014210     05  FILLER                  PIC X(03).                               
014300     05  RAD2-VKORDNTO           PIC X(14) JUSTIFIED RIGHT.               
014400     05  FILLER                  PIC X(02).                               
014500     05  RAD2-VKORDBTO           PIC X(14) JUSTIFIED RIGHT.               
014600     05  FILLER                  PIC X(02).                               
014700     05  RAD2-SUORDV-TOT         PIC X(14) JUSTIFIED RIGHT.               
014800     05  FILLER                  PIC X(04).                               
014900     05  RAD2-KDVALISO           PIC X(08).                               
015000                                                                          
015100 01  RAD-3.                                                               
015200     05  FILLER                  PIC X(1).                                
015300     05  RAD3-IDSTATNR           PIC Z9(4)B9(3).                          
015400     05  FILLER                  PIC X(3).                                
015500     05  RAD3-KDARTURS           PIC X(3).                                
015600     05  FILLER                  PIC X(6).                                
015700     05  RAD3-ASTERISK           PIC X(2).                                
015800     05  RAD3-KVART              PIC Z(8)9.                               
015900     05  FILLER                  PIC X(01).                               
016000*****05  RAD3-VKORDNTO           PIC Z(11)9.9(1).                         
016010     05  RAD3-VKORDNTO           PIC Z(11)9.9(3).                         
016100     05  FILLER                  PIC X(02).                               
016200     05  RAD3-VKORDBTO           PIC Z(11)9.9(1).                         
016300     05  FILLER                  REDEFINES RAD3-VKORDBTO.                 
016400       07 RAD3-BTO               PIC X(14).                               
016500     05  FILLER                  PIC X(02).                               
016600     05  RAD3-SUORDV-TOT         PIC Z(10)9.9(2).                         
016700     05  FILLER                  PIC X(04).                               
016800     05  RAD3-KDVALISO           PIC X(03).                               
016900     05  FILLER                  PIC X(05).                               
017000                                                                          
017100 01  RADY.                                                                
017200     03 FILLER                    PIC X(32).                              
017300     03 RADY-TEXT                 PIC X(30).                              
017400     03 FILLER                    PIC X(6).                               
017500     03 RADY-DIFF                 PIC -(8)9.99.                           
017600                                                                          
017700 77  W-KDSPRAK                   PIC S9      COMP-3.                      
017800*77  W-VKVIKT                    PIC S9(08)V9(1)  VALUE ZERO.             
017810 77  W-VKVIKT                    PIC S9(08)V9(3)  VALUE ZERO.             
017900 77  W-VKVIKTB                   PIC S9(08)V9(1)  VALUE ZERO.             
018000 77  W-VKVIKTC                   PIC S9(08)V9(1)  VALUE ZERO.             
018100 77  W-SUPRIS                    PIC S9(09)V9(2)  VALUE ZERO.             
018200 77  W-KDVALISO                  PIC X(3)    VALUE SPACE.                 
018300 77  W-ST-URS-SUBTOT-KDVALISO    PIC X(3)    VALUE SPACE.                 
018400 77  W-ST-URS-TOT-KDVALISO       PIC X(3)    VALUE SPACE.                 
018500 77  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
018600                                                                          
018700 77  KDFKBIL-SW                  PIC S9(1)   VALUE ZERO.                  
018800                                                                          
018900     88  KDFKBIL-ST                          VALUE +1.                    
019000     88  KDFKBIL-URS                         VALUE +2.                    
019100     88  KDFKBIL-SRA                         VALUE +3.                    
019200     88  KDFKBIL-ST-URS-SRA                  VALUE +4.                    
019300     88  KDFKBIL-ST-URS                      VALUE +5.                    
019400     88  KDFKBIL-ST-SRA                      VALUE +6.                    
019500     88  KDFKBIL-URS-SRA                     VALUE +7.                    
019600                                                                          
019700 77  W-CONST-99                  PIC X(2)    VALUE '99'.                  
019800                                                                          
019900 01  FILLER                      PIC X(32)  VALUE 'LEDTEXT TABLE'.        
020000* 01 -COPY W475W552                                                       
020100     EJECT                                                                
020200* 01 -COPY W475W558                                                       
020300     EJECT                                                                
020400* 01 -COPY W476W001                                                       
020500     EJECT                                                                
020600                                                                          
020700 01  W-KDVALISO-TBL.                                                      
020800     02  W-KDVALISO-POST-1 OCCURS  300.                                   
020900         03  W-KDVALISO-POST-2 OCCURS  50.                                
021000             05  W-ST-URS-KDVALISO PIC X(03).                             
021100                                                                          
021200 01  GENERAL-SUBPROGRAMS.                                                 
021300*                                                                         
021400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
021500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021700     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
021800     03  INTSOR                  PIC X(8)    VALUE 'INTSOR  '.            
021900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
021910     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
022000     SKIP2                                                                
022100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
022200                                                                          
022300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
022400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
022500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
022600     SKIP2                                                                
022700     EJECT                                                                
022710*    --- PARAMETERS FOR SUBPROGRAM W510CURR                               
022720     EJECT                                                                
022730*01  -COPY W510CURR                                                       
022800*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
022900     EJECT                                                                
023000*01  -COPY W006PRAR                                                       
023100 01  W-IDPRTLST                  PIC X(8).                                
023200     SKIP3                                                                
023300*    --- PARAMETRS  FOR SUBPROGRAM INTSOR                                 
023400 01  INTSOR-HJAELP-AREA.                                                  
023500    03  INTSOR-POST-ANTAL          PIC S9(3)  VALUE ZERO COMP-3.          
023600    03  INTSOR-POST-LAENGD         PIC S9(3)  VALUE ZERO COMP-3.          
023700    03  INTSOR-SORT-FAELT-LAENGD   PIC S9(3)  VALUE ZERO COMP-3.          
023800     SKIP3                                                                
023900*    --- AREAS FOR IMS-SECTIONS                                           
024000*                                                                         
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024300     SKIP3                                                                
024400 01  KEYS-TO-DLI.                                                         
024500                                                                          
024600     03  W-IDSHIPM-X.                                                     
024700         05  W-IDSHIPM            PIC 9(7)    VALUE ZERO.                 
024800                                                                          
024900     03  W-WDE111KY-X.                                                    
025000        05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
025100        05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
025200                                                                          
025300     03  W-WDE111KY-MIN.                                                  
025400         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
025500         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
025600                                                                          
025700     03  W-WDE111KY-MAX.                                                  
025800         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
025900         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
026000                                                                          
026100     03  W-WDE121KY-X.                                                    
026200        05  W-WDE121-IDPRODNR    PIC S9(07)  VALUE ZERO COMP-3.           
026300        05  W-WDE121-IDKOLLI     PIC S9(05)  VALUE ZERO COMP-3.           
026400                                                                          
026500     03  W-WDE121KY-MIN.                                                  
026600         05  W-WDE121-IDPRODNR-MIN PIC S9(07)  VALUE ZERO COMP-3.         
026700         05  W-WDE121-IDKOLLI-MIN  PIC S9(05)  VALUE ZERO COMP-3.         
026800                                                                          
026900     03  W-WDE121KY-MAX.                                                  
027000         05  W-WDE121-IDPRODNR-MAX PIC S9(07)  VALUE ZERO COMP-3.         
027100         05  W-WDE121-IDKOLLI-MAX  PIC S9(05)  VALUE ZERO COMP-3.         
027200                                                                          
027300     03  W-WDB201KY-X.                                                    
027400         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
027500         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
027600                                                                          
027700     03  W-WDB101KY-X.                                                    
027800         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
027900         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
027910                                                                          
028100     SKIP2                                                                
028200*    --- STATUS-KOD FRÅN IMS                                              
028300 01  STATUS-WS                    PIC XX.                                 
028400     88  SEGMENT-FOUND                       VALUE '  '.                  
028500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
028600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
028700     SKIP2                                                                
028800 01  GOOD-STATUSCODES.                                                    
028900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029000     SKIP3                                                                
029100 01  SSA1                        PIC X(64).                               
029200 01  SSA2                        PIC X(64).                               
029300 01  SSA3                        PIC X(64).                               
029400 01  SSA4                        PIC X(64).                               
029500     EJECT                                                                
029600                                                                          
029700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
029800 01  SEND-AREA.                                                           
029900*    03  -COPY WZ01SEND                                                   
030000                                                                          
030100 01  SEND-RAD-STYRTECKEN.                                                 
030200     03  STYRTECKEN-RAD          PIC X.                                   
030300     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
030400                                                                          
030500 01  DAP-AREA-START              PIC X(24)   VALUE                        
030600                                             'DAP-AREA-START'.            
030700                                                                          
030800*    --- IMS FUNCTION CODES                                               
030900*01  -COPY W0003                                                          
031000     EJECT                                                                
031100*    ---  DLI INPUT-OUTPUT AREA                                           
031200 01  FILLER         PIC X(25) VALUE 'DLI-IO-WDE101-11-21-31'.             
031300 01  DLI-IO-WDE101-11-21-31.                                              
031400     03  DLI-IO-WDE101.                                                   
031500*        05  -COPY WDE101                                                 
031600     03  DLI-IO-WDE111.                                                   
031700*        05  -COPY WDE111                                                 
031800     03  DLI-IO-WDE121.                                                   
031900*        05  -COPY WDE121                                                 
032000     03  DLI-IO-WDE131.                                                   
032100*        05  -COPY WDE131                                                 
032200                                                                          
032300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
032400 01  DLI-IO-WDB101.                                                       
032500*    03  -COPY WDB101                                                     
032600                                                                          
032700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
032800 01  DLI-IO-WDB201.                                                       
032900*    03  -COPY WDB201                                                     
032910                                                                          
033100     EJECT                                                                
033200 LINKAGE SECTION.                                                         
033300                                                                          
033400*01  -COPY W476TRPD                                                       
033500                                                                          
033600 01  ALT-PCB                     PIC X(32).                               
033700                                                                          
033800*01  -COPY W0008  -PRE WDE1-                                              
033900     05  FILLER                  PIC X.                                   
034000                                                                          
034100*01  -COPY W0008  -PRE WDB2-                                              
034200     05  FILLER                  PIC X.                                   
034300                                                                          
034400*01  -COPY W0008  -PRE WDB1-                                              
034500     05  FILLER                  PIC X.                                   
034510                                                                          
034520*01  -COPY W0008  -PRE WDG2-                                              
034530     05  FILLER                  PIC X.                                   
034600                                                                          
034700     EJECT                                                                
034800 PROCEDURE DIVISION  USING TRPD-W476TRPD ALT-PCB                          
034900                           WDE1-PCB WDB2-PCB WDB1-PCB                     
034910                           WDG2-PCB.                                      
035000 MAIN SECTION.                                                            
035100     ENTRY 'DLITCBL' USING TRPD-W476TRPD ALT-PCB                          
035200                           WDE1-PCB WDB2-PCB WDB1-PCB                     
035210                           WDG2-PCB.                                      
035300                                                                          
035400     PERFORM A-INIT                                                       
035500                                                                          
035600     PERFORM IMS-GU-WDE101-11                                             
035610     MOVE SGMT-IDDC           TO WS-IDDC                                  
035620                                                                          
035630     IF (CDC-SE OR DDC-SE) AND                                            
035640       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
035650       PERFORM C-OMVANDLA-VALUTA                                          
035660     END-IF                                                               
035670                                                                          
035700     MOVE SGMT-IDDISTR    TO W-WDE111-IDDISTR                             
035800     MOVE SGMT-IDKUNDNR   TO W-WDE111-IDKUNDNR                            
035900** KDFKBIL IS SAME FOR ALL THE CUSTOMER UNDER SAME DISTRICT               
036000     IF SEGMENT-FOUND                                                     
036100       MOVE SGMT-KDFKBIL  TO KDFKBIL-SW                                   
036200     END-IF                                                               
036300     MOVE SGMT-PRKURS   TO WS-PRKURS-LOCCUR                               
036400     IF SGMT-IDDISTR  = WX-RYSS                                           
036500       MOVE YES         TO DIST-RYSSLAND-SW                               
036600     END-IF                                                               
036700                                                                          
036800     PERFORM UNTIL NOT SEGMENT-FOUND                                      
036900       PERFORM IMS-GNP-WDE121                                             
037000       MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                       
037100       MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                        
037200       PERFORM UNTIL NOT SEGMENT-FOUND                                    
037300        ADD SKOLLI-VKORDBTO-KOLLI  TO TOTAL2-VKORDBTO                     
037400        PERFORM IMS-GNP-WDE131                                            
037500        PERFORM UNTIL NOT SEGMENT-FOUND                                   
037600         PERFORM B-BUILD-TABLE                                            
037700         PERFORM IMS-GNP-WDE131                                           
037800        END-PERFORM                                                       
037900        PERFORM IMS-GNP-WDE121                                            
038000        MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                      
038100        MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                       
038200       END-PERFORM                                                        
038300                                                                          
038400       PERFORM IMS-GNP-WDE111                                             
038500       MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                        
038600     END-PERFORM                                                          
038700                                                                          
038800     PERFORM S20-HAMTA-WDB2                                               
038900     PERFORM E-PRINT-DOCUMENT                                             
039000                                                                          
039100     MOVE ZERO TO RETURN-CODE                                             
039200     GOBACK                                                               
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 A-INIT SECTION.                                                          
039700                                                                          
039800     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
039900     ACCEPT TODAYS-DATE  FROM DATE                                        
040000                                                                          
040100     MOVE TRPD-IDPRTLST   TO W-IDPRTLST                                   
040200     MOVE TRPD-IDSHIPM    TO W-IDSHIPM                                    
040300     MOVE TRPD-PFDEF-OVR  TO PRT-PFDEF-OVR                                
040400     MOVE TRPD-IDDISTR    TO W-WDE111-IDDISTR-MIN                         
040500                             W-WDE111-IDDISTR-MAX                         
040600                             W-WDB201-IDDISTR                             
040610                             TEST-IDDISTR                                 
                                                                                
                                                                                
040700     MOVE HIGH-VALUE      TO W-WDE121KY-MAX                               
040800     MOVE 2               TO W-KDSPRAK                                    
040900     MOVE ZERO            TO W-PAGE-NO                                    
041000                             WS-IX                                        
                                   WS-IDDISTR                                   
                                   WS-IDSHIPM-Z                                 
041100                                                                          
041200     MOVE SPACE           TO RAD-HEAD                                     
041300                             RAD2-HEAD                                    
041400                             RAD3-HEAD                                    
041500                             RAD5-HEAD                                    
041600                             RAD1                                         
041700                             RAD-2                                        
041800                             RAD-3                                        
041900                             RADY                                         
042000                                                                          
042100** BUILD HEADER                                                           
042200     MOVE IDSTATNR-LEDTEXT   (W-KDSPRAK) TO RAD2-IDSTATNR                 
042300     MOVE BEARTURS-LEDTEXT   (W-KDSPRAK) TO RAD2-KDARTURS                 
042400     MOVE KBIL-KVANT-LEDTEXT (W-KDSPRAK) TO RAD2-KVART                    
042500     MOVE VKORDNTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDNTO                 
042600     MOVE VKORDBTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDBTO                 
042700     MOVE KBIL-TOTAL-LEDTEXT (W-KDSPRAK) TO RAD2-SUORDV-TOT               
042800     MOVE KDVALISO-LEDTEXT   (W-KDSPRAK) TO RAD2-KDVALISO                 
042900                                                                          
043000     MOVE ST-URS-MAX-ANTAL-STATNR        TO ST-URS-ANTAL-STATNR           
043100                                                                          
043200     MOVE +1                             TO IXSTATNR                      
043300     PERFORM UNTIL IXSTATNR > ST-URS-ANTAL-STATNR                         
043400       MOVE ALL '9'        TO ST-URS-IDSTATNR (IXSTATNR)                  
043500       MOVE ZERO           TO ST-URS-IDSTATNR-PNR (IXSTATNR)              
043600       MOVE +1             TO IXARTURS                                    
043700       PERFORM UNTIL IXARTURS > ST-URS-MAX-ANTAL-ARTURS                   
043800          IF IXSTATNR = +1                                                
043900            MOVE W-CONST-99 TO ST-URS-AKTUELL-KDARTURS (IXARTURS)         
044000          END-IF                                                          
044100          MOVE W-CONST-99 TO ST-URS-KDARTURS (IXSTATNR, IXARTURS)         
044200          MOVE ZERO TO ST-URS-KVART       (IXSTATNR, IXARTURS)            
044300                       ST-URS-VKORDNTO    (IXSTATNR, IXARTURS)            
044400                       ST-URS-VKORDBTO    (IXSTATNR, IXARTURS)            
044500                       ST-URS-SUORDV-TOT  (IXSTATNR, IXARTURS)            
044600          MOVE SPACE TO W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)            
044700          ADD +1 TO IXARTURS                                              
044800       END-PERFORM                                                        
044900       ADD +1 TO IXSTATNR                                                 
045000     END-PERFORM                                                          
045100                                                                          
045200     MOVE ZERO  TO ST-URS-ANTAL-STATNR                                    
045300                   ST-URS-ANTAL-ARTURS                                    
045400                   ST-URS-TOT-KVART                                       
045500                   ST-URS-TOT-VKORDNTO                                    
045600                   ST-URS-TOT-VKORDBTO                                    
045700                   ST-URS-TOT-SUORDV-TOT                                  
045710                   ST-URS-TOT-SUORDV-TOT-INR                              
045800                   ST-URS-SUBTOT-KVART                                    
045900                   ST-URS-SUBTOT-VKORDNTO                                 
046000                   ST-URS-SUBTOT-VKORDBTO                                 
046100                   ST-URS-SUBTOT-SUORDV-TOT                               
046110                   ST-URS-SUBTOT-SUORDV-TOT-INR                           
046200                                                                          
046300     MOVE SPACE TO W-ST-URS-SUBTOT-KDVALISO                               
046400                   W-ST-URS-TOT-KDVALISO                                  
046500     .                                                                    
046600     EJECT                                                                
046700                                                                          
046800 B-BUILD-TABLE SECTION.                                                   
046900                                                                          
047000     MOVE ZERO  TO W-VKVIKT                                               
047100                   W-VKVIKTB                                              
047200                   W-SUPRIS                                               
047300     MOVE SPACE TO W-KDVALISO                                             
047400                                                                          
047500**   COMPUTE W-VKVIKT = SRAD-VKARTNTO * SRAD-KVLEVART                     
047510     COMPUTE W-VKVIKT = SRAD-VKART-NTO-KG * SRAD-KVLEVART                 
047600                                                                          
047700     MOVE W-VKVIKT    TO W-VKVIKTC                                        
047800     IF SKOLLI-VKORDNTO-KOLLI = ZERO                                      
047900       MOVE 0.1     TO SKOLLI-VKORDNTO-KOLLI                              
048000       IF W-VKVIKTC  = ZERO                                               
048100       MOVE 0.1     TO W-VKVIKTC                                          
048200       END-IF                                                             
048300     END-IF                                                               
048400*    W-VKVIKTB ANVÄNDS FÖR BERÄKNING AV BRUTTOVIKT MEN                    
048500*    BRUTTOVIKT VISAS VISAS INTE LÄNGRE (SE RAD3-BTO)                     
048501                                                                          
048502     COMPUTE W-VKVIKTB = (W-VKVIKTC) *                                    
048600          SKOLLI-VKORDBTO-KOLLI / SKOLLI-VKORDNTO-KOLLI                   
048700                                                                          
048800     IF SRAD-PRARTNTO > ZERO                                              
048900       COMPUTE W-SUPRIS ROUNDED = SRAD-PRARTNTO * SRAD-KVLEVART           
049000       MOVE 'SEK'               TO W-KDVALISO                             
049100     ELSE                                                                 
049200       IF SRAD-PRARTNTO-LOC > ZERO                                        
049300         COMPUTE W-SUPRIS ROUNDED =                                       
049400                          SRAD-PRARTNTO-LOC * SRAD-KVLEVART               
049500         MOVE SRAD-KDVALISO     TO W-KDVALISO                             
049600       ELSE                                                               
049700         IF SRAD-PRARTNTO-LOCPREL > ZERO                                  
049800           COMPUTE W-SUPRIS ROUNDED =                                     
049900                          SRAD-PRARTNTO-LOCPREL * SRAD-KVLEVART           
050000           MOVE SRAD-KDVALISO   TO W-KDVALISO                             
050100         END-IF                                                           
050200       END-IF                                                             
050300     END-IF                                                               
050400     ADD W-VKVIKTB          TO TOTAL3-VKORDBTO                            
050500                                                                          
050600     IF KDFKBIL-ST              OR                                        
050700        KDFKBIL-URS             OR                                        
050800        KDFKBIL-SRA             OR                                        
050900        KDFKBIL-ST-URS-SRA      OR                                        
051000        KDFKBIL-ST-URS          OR                                        
051100        KDFKBIL-ST-SRA          OR                                        
051200        KDFKBIL-URS-SRA                                                   
051300                                                                          
051400       MOVE +1  TO IXURS                                                  
051500       MOVE NOO TO IXURS-PRESENT-SW                                       
051600                                                                          
051700       PERFORM UNTIL IXURS  > ST-URS-MAX-ANTAL-ARTURS  OR                 
051800                     IXURS-PRESENT                                        
051900         IF SRAD-KDARTURS = ST-URS-AKTUELL-KDARTURS (IXURS)               
052000           MOVE YES TO IXURS-PRESENT-SW                                   
052100         ELSE                                                             
052200           IF IXURS > ST-URS-ANTAL-ARTURS                                 
052300             MOVE IXURS         TO ST-URS-ANTAL-ARTURS                    
052400             MOVE SRAD-KDARTURS TO ST-URS-AKTUELL-KDARTURS (IXURS)        
052500             MOVE YES           TO IXURS-PRESENT-SW                       
052600           END-IF                                                         
052700         END-IF                                                           
052800         ADD +1 TO IXURS                                                  
052900       END-PERFORM                                                        
053000                                                                          
053100** TABEL OVERFLOW  ****                                                   
053200       IF IXURS-PRESENT-SW = NOO                                          
053300         MOVE 'TABLE ST-URS-AKTUELL-KDARTURS IS SMALL'                    
053400                                    TO ERRTEXT                            
053500         CALL FELLOG                                                      
053600       END-IF                                                             
053700                                                                          
053800       MOVE ZERO TO IXSTATNR                                              
053900                    IXARTURS                                              
054000       MOVE +1   TO INDX                                                  
054100                                                                          
054200       PERFORM UNTIL  INDX > ST-URS-MAX-ANTAL-STATNR                      
054300         IF INDX > ST-URS-ANTAL-STATNR                                    
054400           MOVE INDX                 TO ST-URS-ANTAL-STATNR               
054500                                        IXSTATNR                          
054600                                        ST-URS-IDSTATNR-PNR (INDX)        
054700           MOVE SRAD-IDSTATNR        TO ST-URS-IDSTATNR (IXSTATNR)        
054800           MOVE ST-URS-MAX-ANTAL-STATNR                                   
054900                                     TO INDX                              
055000         ELSE                                                             
055100           IF SRAD-IDSTATNR = ST-URS-IDSTATNR (INDX)                      
055200             MOVE INDX               TO IXSTATNR                          
055300             MOVE ST-URS-MAX-ANTAL-STATNR                                 
055400                                     TO INDX                              
055500           END-IF                                                         
055600         END-IF                                                           
055700                                                                          
055800         ADD  +1 TO INDX                                                  
055900       END-PERFORM                                                        
056000                                                                          
056100       IF IXSTATNR NOT = ZERO                                             
056200         MOVE +1 TO INDX                                                  
056300         PERFORM UNTIL INDX > ST-URS-ANTAL-ARTURS                         
056400           IF ST-URS-KDARTURS (IXSTATNR, INDX) = W-CONST-99               
056500             MOVE SRAD-KDARTURS         TO                                
056600                            ST-URS-KDARTURS (IXSTATNR, INDX)              
056700           END-IF                                                         
056800                                                                          
056900           IF SRAD-KDARTURS = ST-URS-KDARTURS (IXSTATNR, INDX)            
057000             MOVE INDX                  TO IXARTURS                       
057100             MOVE ST-URS-MAX-ANTAL-ARTURS                                 
057200                                        TO INDX                           
057300           END-IF                                                         
057400                                                                          
057500           ADD +1 TO INDX                                                 
057600         END-PERFORM                                                      
057700       END-IF                                                             
057800                                                                          
057900       ADD SRAD-KVLEVART     TO ST-URS-TOT-KVART                          
058000       ADD W-VKVIKT          TO ST-URS-TOT-VKORDNTO                       
058100       ADD W-VKVIKTB         TO ST-URS-TOT-VKORDBTO                       
058101                                                                          
058110*      VI SUMMERAR INTE INDISKA SUMMOR HÄR, VI SUMMERAR ISTÄLLET          
058120*      OMVANDLADE AVRUNDADE DELSUMMOR I S03-, SÅ SUMMORNA STÄMMER         
058130*      FÖR INDIEN SUMMERAS GRANDTOT I SEK DESSUTOM                        
058200       ADD W-SUPRIS          TO ST-URS-TOT-SUORDV-TOT                     
058300       MOVE W-KDVALISO       TO W-ST-URS-TOT-KDVALISO                     
058400                                                                          
058500       IF IXSTATNR = ZERO OR IXARTURS = ZERO                              
058600** TABEL OVERFLOW                                                         
058700         MOVE ' ST-URS-TABEL IS SMALL' TO ERRTEXT                         
058800         CALL FELLOG                                                      
058900       ELSE                                                               
059000         ADD SRAD-KVLEVART TO ST-URS-KVART (IXSTATNR, IXARTURS)           
059100         ADD W-VKVIKT      TO ST-URS-VKORDNTO (IXSTATNR, IXARTURS)        
059200         ADD W-VKVIKTB     TO ST-URS-VKORDBTO (IXSTATNR, IXARTURS)        
059300         ADD W-SUPRIS      TO                                             
059400                            ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS)        
059500         MOVE W-KDVALISO   TO                                             
059600                            W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)        
059700       END-IF                                                             
059800     END-IF                                                               
059900                                                                          
060000     .                                                                    
060100     EJECT                                                                
060110                                                                          
060120 C-OMVANDLA-VALUTA SECTION.                                               
060130                                                                          
060140*    HÄMTA KURS FÖR OMRÄKNING TILL INR                                    
060150     MOVE 'INR'                      TO CURR-KDVALISO-ROW                 
060181     MOVE TODAYS-DATE-YEAR           TO W-DATE-AAMM(1:2)                  
060182     MOVE TODAYS-DATE-MONTH          TO W-DATE-AAMM(3:2)                  
060183     MOVE W-DATE-AAMM                TO CURR-TIAAMM                       
060184     MOVE WS-KDVALISO-HUV            TO CURR-KDVALISO-HUV                 
060185     MOVE 'M'                        TO CURR-KDVALTYP                     
060186                                                                          
060187     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
060188     IF CURR-KDSVAR = ' '                                                 
060189        CONTINUE                                                          
060190     ELSE                                                                 
060191        MOVE 1                       TO CURR-PRKURS-NEW                   
060192     END-IF                                                               
060193     COMPUTE WS-PRKURS-INR ROUNDED = 1 / CURR-PRKURS-NEW                  
060194     .                                                                    
060195                                                                          
060196     EJECT                                                                
060200                                                                          
060300 E-PRINT-DOCUMENT SECTION.                                                
060400                                                                          
060500     PERFORM EC-BRUTTOVIKT                                                
060600     IF TOTAL2-VKORDBTO NOT = TOTAL3-VKORDBTO                             
060700       MOVE 1          TO ISM                                             
060800*      PERFORM UNTIL ISM > 3                                              
060900*        PERFORM EC-BRUTTOVIKT                                            
061000*        ADD 1         TO ISM                                             
061100*      END-PERFORM                                                        
061200     END-IF                                                               
061300                                                                          
061400     IF KDFKBIL-ST OR                                                     
061500        KDFKBIL-ST-URS-SRA OR                                             
061600        KDFKBIL-ST-URS OR                                                 
061700        KDFKBIL-ST-SRA                                                    
061800                                                                          
061900       PERFORM EA-PRINT-STATNR-ORG                                        
062000                                                                          
062100     END-IF                                                               
062200                                                                          
062300     IF KDFKBIL-URS OR                                                    
062400        KDFKBIL-SRA OR                                                    
062500        KDFKBIL-ST-URS-SRA OR                                             
062600        KDFKBIL-ST-URS OR                                                 
062700        KDFKBIL-URS-SRA                                                   
062800                                                                          
062900       PERFORM EB-PRINT-ORG-STATNR                                        
063000                                                                          
063100     END-IF                                                               
063200                                                                          
063300     .                                                                    
063400     EJECT                                                                
063500                                                                          
063600 EA-PRINT-STATNR-ORG  SECTION.                                            
063700                                                                          
063800******************************************************************        
063900*    PRINT  IN ORDER   STATNR/URSPRUNG                           *        
064000******************************************************************        
064100                                                                          
064200     MOVE ZERO                        TO TOTAL-VKORDNTO                   
064300     MOVE ZERO                        TO TOTAL-VKORDBTO                   
064400                                                                          
064500     IF ST-URS-ANTAL-STATNR NOT = ZERO                                    
064600       IF ST-URS-ANTAL-STATNR > +1                                        
064700         MOVE ST-URS-ANTAL-STATNR     TO INTSOR-POST-ANTAL                
064800         MOVE ST-URS-IDSTATNR-IX-LGD  TO INTSOR-POST-LAENGD               
064900         MOVE ST-URS-IDSTATNR-LAENGD  TO INTSOR-SORT-FAELT-LAENGD         
065000         CALL INTSOR USING ST-URS-IDSTATNR-IX-TABEL (+1)                  
065100                           INTSOR-POST-LAENGD                             
065200                           INTSOR-POST-ANTAL                              
065300                           ST-URS-IDSTATNR (+1)                           
065400                           INTSOR-SORT-FAELT-LAENGD                       
065500       END-IF                                                             
065600                                                                          
065700       MOVE +99 TO W-LINE-COUNT                                           
065800       MOVE +1  TO INDX                                                   
065900                                                                          
066000       PERFORM UNTIL INDX > ST-URS-ANTAL-STATNR                           
066100         IF ST-URS-IDSTATNR (INDX) NOT = ZERO                             
066200           MOVE ST-URS-IDSTATNR (INDX)   TO RAD3-IDSTATNR                 
066300         END-IF                                                           
066400         MOVE ST-URS-IDSTATNR-PNR (INDX) TO IXSTATNR                      
066500         MOVE ZERO  TO ST-URS-SUBTOT-KVART                                
066600                       ST-URS-SUBTOT-VKORDNTO                             
066700                       ST-URS-SUBTOT-VKORDBTO                             
066800                       ST-URS-SUBTOT-SUORDV-TOT                           
066810                       ST-URS-SUBTOT-SUORDV-TOT-INR                       
066900         MOVE SPACE TO W-ST-URS-SUBTOT-KDVALISO                           
067000                                                                          
067100         MOVE +1    TO IXARTURS                                           
067200         PERFORM UNTIL IXARTURS > ST-URS-ANTAL-ARTURS OR                  
067300                ST-URS-KDARTURS (IXSTATNR, IXARTURS) = W-CONST-99         
067400                                                                          
067500           IF W-LINE-COUNT > W-LINE-MAX                                   
067600             PERFORM S05-PRINT-HEAD-1                                     
067700             PERFORM S06-PRINT-HEAD-2                                     
067800             IF ST-URS-IDSTATNR (INDX) NOT = ZERO                         
067900               MOVE ST-URS-IDSTATNR (INDX) TO RAD3-IDSTATNR               
068000             END-IF                                                       
068100           END-IF                                                         
068200                                                                          
068300           MOVE ST-URS-KDARTURS (IXSTATNR, IXARTURS)                      
068400                                           TO RAD3-KDARTURS               
068500           PERFORM S03-BUILD-DETAIL                                       
068600           MOVE SPACES             TO RAD-3                               
068700           ADD +1                  TO IXARTURS                            
068800         END-PERFORM                                                      
068900         PERFORM S01-BUILD-SUBTOTAL                                       
069000         ADD  +1    TO INDX                                               
069100       END-PERFORM                                                        
069200       PERFORM S02-BUILD-GRTOTAL                                          
069300     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600                                                                          
069700 EB-PRINT-ORG-STATNR  SECTION.                                            
069800                                                                          
069900******************************************************************        
070000*    PRINT IN ORDER   URSPRUNG/STATNR                            *        
070100******************************************************************        
070200                                                                          
070300     MOVE ZERO                        TO TOTAL-VKORDNTO                   
070400     MOVE ZERO                        TO TOTAL-VKORDBTO                   
070500                                                                          
070600     IF ST-URS-ANTAL-ARTURS NOT = ZERO                                    
070700        MOVE +99 TO W-LINE-COUNT                                          
070800        MOVE +1  TO IXURS                                                 
070900        PERFORM UNTIL IXURS > ST-URS-ANTAL-ARTURS                         
071000          MOVE ZERO  TO ST-URS-SUBTOT-KVART                               
071100                        ST-URS-SUBTOT-VKORDNTO                            
071200                        ST-URS-SUBTOT-VKORDBTO                            
071300                        ST-URS-SUBTOT-SUORDV-TOT                          
071310                        ST-URS-SUBTOT-SUORDV-TOT-INR                      
071400          MOVE SPACE TO W-ST-URS-SUBTOT-KDVALISO                          
071500                                                                          
071600          MOVE ST-URS-AKTUELL-KDARTURS (IXURS) TO RAD3-KDARTURS           
071700          MOVE +1      TO INDX                                            
071800                                                                          
071900          PERFORM UNTIL INDX > ST-URS-ANTAL-STATNR                        
072000                                                                          
072100            MOVE ST-URS-IDSTATNR-PNR (INDX) TO IXSTATNR                   
072200            MOVE +1 TO IXARTURS                                           
072300            PERFORM UNTIL IXARTURS > ST-URS-ANTAL-ARTURS OR               
072400                ST-URS-KDARTURS (IXSTATNR, IXARTURS) = W-CONST-99         
072500                                                                          
072600              IF ST-URS-KDARTURS (IXSTATNR, IXARTURS) =                   
072700                                 ST-URS-AKTUELL-KDARTURS (IXURS)          
072800                IF W-LINE-COUNT > W-LINE-MAX                              
072900                  PERFORM S05-PRINT-HEAD-1                                
073000                  PERFORM S06-PRINT-HEAD-2                                
073100                  MOVE ST-URS-AKTUELL-KDARTURS (IXURS)                    
073200                                             TO RAD3-KDARTURS             
073300                END-IF                                                    
073400                IF ST-URS-IDSTATNR (INDX) NOT = ZERO                      
073500                  MOVE ST-URS-IDSTATNR (INDX) TO RAD3-IDSTATNR            
073600                END-IF                                                    
073700                                                                          
073800                PERFORM S03-BUILD-DETAIL                                  
073900                MOVE SPACES                   TO RAD-3                    
074000                MOVE ST-URS-MAX-ANTAL-ARTURS  TO IXARTURS                 
074100                                                                          
074200              END-IF                                                      
074300              ADD +1 TO IXARTURS                                          
074400                                                                          
074500            END-PERFORM                                                   
074600            ADD +1 TO INDX                                                
074700                                                                          
074800          END-PERFORM                                                     
074900          PERFORM S01-BUILD-SUBTOTAL                                      
075000          ADD  +1  TO IXURS                                               
075100                                                                          
075200        END-PERFORM                                                       
075300        PERFORM S02-BUILD-GRTOTAL                                         
075400     END-IF                                                               
075500     .                                                                    
075600     EJECT                                                                
075700 EC-BRUTTOVIKT  SECTION.                                                  
075800                                                                          
075900     IF TOTAL2-VKORDBTO NOT = TOTAL3-VKORDBTO                             
076000       MOVE 1 TO IA                                                       
076100       PERFORM UNTIL IA > ST-URS-ANTAL-STATNR                             
076200        MOVE 1    TO IB                                                   
076300        PERFORM UNTIL IB > ST-URS-ANTAL-ARTURS                            
076400        IF ST-URS-VKORDBTO(IA, IB) < 0.05                                 
076500          AND ST-URS-KVART(IA, IB) NOT = 0                                
076600          COMPUTE WX-BTO = 0.1 - ST-URS-VKORDBTO(IA, IB)                  
076700          MOVE 0.1          TO ST-URS-VKORDBTO (IA, IB)                   
076800          ADD WX-BTO        TO TOTAL3-VKORDBTO                            
076900        END-IF                                                            
077000        IF TOTAL2-VKORDBTO NOT = TOTAL3-VKORDBTO                          
077100        IF TOTAL2-VKORDBTO > TOTAL3-VKORDBTO                              
077200*       KOLLI-VIKT > RAD-VIKTERNA                                         
077300         IF ST-URS-VKORDBTO (IA, IB) > ZERO                               
077400          ADD 0.1         TO   ST-URS-VKORDBTO (IA, IB)                   
077500          ADD 0.1         TO   TOTAL3-VKORDBTO                            
077600         END-IF                                                           
077700        ELSE                                                              
077800         IF ST-URS-VKORDBTO (IA, IB) > 0.15                               
077900          SUBTRACT 0.1      FROM ST-URS-VKORDBTO (IA, IB)                 
078000          SUBTRACT 0.1      FROM TOTAL3-VKORDBTO                          
078100         END-IF                                                           
078200        END-IF                                                            
078300        END-IF                                                            
078400        ADD 1      TO IB                                                  
078500        END-PERFORM                                                       
078600        ADD 1    TO IA                                                    
078700       END-PERFORM                                                        
078800       COMPUTE TOTAL3-VKORDBTO =                                          
078900                      TOTAL2-VKORDBTO - TOTAL3-VKORDBTO                   
079000       ADD TOTAL3-VKORDBTO     TO ST-URS-VKORDBTO (1, 1)                  
079100     END-IF                                                               
079200     .                                                                    
079300     EJECT                                                                
079400                                                                          
079500 S01-BUILD-SUBTOTAL   SECTION.                                            
079600                                                                          
079700     MOVE SPACES                     TO ARB-RAD                           
079800                                        SEND-RAD                          
079900     MOVE 'S='                       TO RAD3-ASTERISK                     
080000     MOVE ST-URS-SUBTOT-KVART        TO RAD3-KVART                        
080100*    IF ST-URS-SUBTOT-VKORDNTO < 0.05                                     
080200*      MOVE 0.1 TO ST-URS-SUBTOT-VKORDNTO                                 
080300*    END-IF                                                               
080400     COMPUTE RAD3-VKORDNTO ROUNDED = ST-URS-SUBTOT-VKORDNTO               
080500                                                                          
080600     ADD ST-URS-SUBTOT-VKORDNTO      TO TOTAL-VKORDNTO                    
080700                                                                          
080800*    IF ST-URS-SUBTOT-VKORDBTO < 0.05                                     
080900*      MOVE 0.1 TO ST-URS-SUBTOT-VKORDBTO                                 
081000*    END-IF                                                               
081100     COMPUTE RAD3-VKORDBTO ROUNDED = ST-URS-SUBTOT-VKORDBTO               
081200     IF NOT DIST-RYSSLAND                                                 
081300       MOVE SPACE                     TO RAD3-BTO                         
081400     END-IF                                                               
081500                                                                          
081600     ADD ST-URS-SUBTOT-VKORDBTO      TO TOTAL-VKORDBTO                    
081700                                                                          
081710     IF (CDC-SE OR DDC-SE) AND                                            
081720       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
081721       MOVE ST-URS-SUBTOT-SUORDV-TOT-INR TO RAD3-SUORDV-TOT               
081722       MOVE 'INR'                     TO RAD3-KDVALISO                    
081730     ELSE                                                                 
081800       MOVE ST-URS-SUBTOT-SUORDV-TOT TO RAD3-SUORDV-TOT                   
081900       MOVE W-ST-URS-SUBTOT-KDVALISO TO RAD3-KDVALISO                     
082000       IF FLLOCCUR                                                        
082100         COMPUTE WS-MONEY ROUNDED = (ST-URS-SUBTOT-SUORDV-TOT *           
082200                 WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                   
082300         MOVE WS-MONEY               TO RAD3-SUORDV-TOT                   
082400         MOVE SGMT-KDVALISO          TO RAD3-KDVALISO                     
082500       END-IF                                                             
082510     END-IF                                                               
082600     MOVE RAD-3                      TO ARB-RAD                           
082700                                        SEND-RAD                          
082800     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
082900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
083000     ADD 1                           TO W-LINE-COUNT                      
083100     PERFORM S04-PRINT-LINE                                               
083200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
083300     MOVE SPACE                      TO ARB-RAD                           
083400                                        SEND-RAD                          
083500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
083600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
083700     ADD 1                           TO W-LINE-COUNT                      
083800     PERFORM S04-PRINT-LINE                                               
083900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
084000     MOVE SPACES                     TO RAD3-ASTERISK                     
084100     .                                                                    
084200     EJECT                                                                
084300                                                                          
084400 S02-BUILD-GRTOTAL    SECTION.                                            
084500                                                                          
084600     MOVE SPACES                  TO ARB-RAD                              
084700                                     SEND-RAD                             
084800     MOVE 'T='                    TO RAD3-ASTERISK                        
084900     MOVE ST-URS-TOT-KVART        TO RAD3-KVART                           
085000*    IF ST-URS-TOT-VKORDNTO < 0.05                                        
085100*      MOVE 0.1                   TO ST-URS-TOT-VKORDNTO                  
085200*    END-IF                                                               
085300     COMPUTE RAD3-VKORDNTO ROUNDED = ST-URS-TOT-VKORDNTO                  
085400                                                                          
085500     COMPUTE RAD3-VKORDNTO         = TOTAL-VKORDNTO                       
085600                                                                          
085700*    IF ST-URS-TOT-VKORDBTO < 0.05                                        
085800*      MOVE 0.1                   TO ST-URS-TOT-VKORDBTO                  
085900*    END-IF                                                               
086000     COMPUTE RAD3-VKORDBTO ROUNDED = ST-URS-TOT-VKORDBTO                  
086100                                                                          
086200     COMPUTE RAD3-VKORDBTO         = TOTAL-VKORDBTO                       
086300     IF NOT DIST-RYSSLAND                                                 
086400       MOVE SPACE                     TO RAD3-BTO                         
086500     END-IF                                                               
086600                                                                          
086610     IF (CDC-SE OR DDC-SE) AND                                            
086620       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
086621                                                                          
086624*      ALT1. ÄR ATT VISA INDISKA DELSUMMORS TOTAL                         
086625*      - DÅ STÄMMER TOTALERNA INOM DETTA TRP DOK (GÄLLER NU)              
086626*      ALT2. ÄR ATT VISA SVENSKA TOTALEN KONVERTERAD TILL INR             
086627*      - DÅ STÄMMER TOTALERNA MELLAN OLIKA TRP DOK                        
086628                                                                          
086629       MOVE ST-URS-TOT-SUORDV-TOT-INR TO RAD3-SUORDV-TOT                  
086630                                                                          
086631**ALT2 COMPUTE WS-MONEY ROUNDED =                                         
086632******   ST-URS-TOT-SUORDV-TOT * WS-PRKURS-INR                            
086633****** MOVE WS-MONEY                 TO RAD3-SUORDV-TOT                   
086634                                                                          
086640       MOVE 'INR'                    TO RAD3-KDVALISO                     
086690     ELSE                                                                 
086700       MOVE ST-URS-TOT-SUORDV-TOT TO RAD3-SUORDV-TOT                      
086800       MOVE W-ST-URS-TOT-KDVALISO TO RAD3-KDVALISO                        
086900       IF FLLOCCUR                                                        
087000         COMPUTE WS-MONEY ROUNDED = (ST-URS-TOT-SUORDV-TOT *              
087100                 WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                   
087200         MOVE WS-MONEY               TO RAD3-SUORDV-TOT                   
087300         MOVE SGMT-KDVALISO          TO RAD3-KDVALISO                     
087400       END-IF                                                             
087410     END-IF                                                               
087500     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
087600     MOVE WS-SKIP1                TO STYRTECKEN-RAD                       
087700     ADD 1                        TO W-LINE-COUNT                         
087800     MOVE RAD-3                   TO ARB-RAD                              
087900                                     SEND-RAD                             
088000     PERFORM S04-PRINT-LINE                                               
088010     IF (CDC-SE OR DDC-SE) AND                                            
088020       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
088021       CONTINUE                                                           
088030     ELSE                                                                 
088100       IF FLLOCCUR                                                        
088200        IF WS-MONEY NOT = WY-MONEY                                        
088300         SUBTRACT WS-MONEY      FROM WY-MONEY                             
088400         IF WY-MONEY NOT = ZERO                                           
088500           MOVE WY-MONEY        TO RADY-DIFF                              
088600           MOVE 'CURRENCY CONVERSION ADJUSTMENT' TO RADY-TEXT             
088700           MOVE RADY            TO ARB-RAD                                
088800                                     SEND-RAD                             
088900           MOVE PRT-AFTER-2     TO PRT-RADSKIP                            
089000           MOVE WS-SKIP2        TO STYRTECKEN-RAD                         
089100           ADD 2                TO W-LINE-COUNT                           
089200           PERFORM S04-PRINT-LINE                                         
089300         END-IF                                                           
089400        END-IF                                                            
089500       END-IF                                                             
089510     END-IF                                                               
089600     MOVE WS-SKIP1                TO STYRTECKEN-RAD                       
089700     MOVE SPACES                  TO RAD3-ASTERISK                        
089800     .                                                                    
089900     EJECT                                                                
090000                                                                          
090100 S03-BUILD-DETAIL     SECTION.                                            
090200                                                                          
090300     MOVE ST-URS-KVART (IXSTATNR, IXARTURS) TO RAD3-KVART                 
090400                                                                          
090500*    IF ST-URS-VKORDNTO (IXSTATNR, IXARTURS) < 0.05                       
090600*      MOVE 0.1         TO ST-URS-VKORDNTO (IXSTATNR, IXARTURS)           
090700*    END-IF                                                               
090800                                                                          
090900     COMPUTE RAD3-VKORDNTO ROUNDED =                                      
091000                           ST-URS-VKORDNTO (IXSTATNR, IXARTURS)           
091100*    IF ST-URS-VKORDBTO (IXSTATNR, IXARTURS) < 0.05                       
091200*      MOVE 0.1         TO ST-URS-VKORDBTO (IXSTATNR, IXARTURS)           
091300*    END-IF                                                               
091400                                                                          
091500     COMPUTE RAD3-VKORDBTO ROUNDED =                                      
091600                           ST-URS-VKORDBTO (IXSTATNR, IXARTURS)           
091700     IF NOT DIST-RYSSLAND                                                 
091800       MOVE SPACE                     TO RAD3-BTO                         
091900     END-IF                                                               
091910     IF (CDC-SE OR DDC-SE) AND                                            
091920       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
091921       COMPUTE WS-MONEY ROUNDED =                                         
091922         ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS) *                         
091923                                     WS-PRKURS-INR                        
091924       MOVE WS-MONEY                 TO RAD3-SUORDV-TOT                   
091925                                        ST-URS-SUORDV-TOT-INR             
091926       MOVE 'INR'                    TO RAD3-KDVALISO                     
091930     ELSE                                                                 
092000       MOVE ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS)                        
092100                                        TO RAD3-SUORDV-TOT                
092200       MOVE W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)                        
092300                                        TO RAD3-KDVALISO                  
092400       IF FLLOCCUR                                                        
092500         COMPUTE WS-MONEY ROUNDED = (ST-URS-SUORDV-TOT                    
092600                                        (IXSTATNR, IXARTURS) *            
092700                 WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                   
092800         MOVE WS-MONEY               TO RAD3-SUORDV-TOT                   
092900         ADD WS-MONEY                TO WY-MONEY                          
093000         MOVE SGMT-KDVALISO          TO RAD3-KDVALISO                     
093100       END-IF                                                             
093110     END-IF                                                               
093200     MOVE RAD-3                       TO ARB-RAD                          
093300                                         SEND-RAD                         
093400     MOVE PRT-AFTER-1                 TO PRT-RADSKIP                      
093500     MOVE WS-SKIP1                    TO STYRTECKEN-RAD                   
093600     ADD  1                           TO W-LINE-COUNT                     
093700     PERFORM S04-PRINT-LINE                                               
093800     MOVE WS-SKIP1                    TO STYRTECKEN-RAD                   
093900     ADD ST-URS-KVART (IXSTATNR, IXARTURS)                                
094000                                      TO ST-URS-SUBTOT-KVART              
094100     ADD ST-URS-VKORDNTO (IXSTATNR, IXARTURS)                             
094200                                      TO ST-URS-SUBTOT-VKORDNTO           
094300     ADD ST-URS-VKORDBTO (IXSTATNR, IXARTURS)                             
094400                                      TO ST-URS-SUBTOT-VKORDBTO           
094401**   REDAN OMVANDLAD INDISK VALUTA SUMMERAS TILL BÄGGE TOTALERNA          
094402**   FÖR UNDVIKA AVRUNDNINGSFEL I TOTALERNA                               
094410     IF (CDC-SE OR DDC-SE) AND                                            
094420       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
094423       ADD ST-URS-SUORDV-TOT-INR   TO ST-URS-SUBTOT-SUORDV-TOT-INR        
094424       ADD ST-URS-SUORDV-TOT-INR   TO ST-URS-TOT-SUORDV-TOT-INR           
094425       MOVE 'INR'                     TO W-ST-URS-SUBTOT-KDVALISO         
094430     ELSE                                                                 
094500       ADD ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS)                         
094600                                      TO ST-URS-SUBTOT-SUORDV-TOT         
094700       MOVE W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)                        
094800                                      TO W-ST-URS-SUBTOT-KDVALISO         
094810     END-IF                                                               
094900     .                                                                    
095000     EJECT                                                                
095100                                                                          
095200 S04-PRINT-LINE SECTION.                                                  
095300                                                                          
095400*    -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                        
095500     PERFORM S90-PUT-DOC-LINE                                             
095600                                                                          
095700*    -- PRINT TO PAPER IF SO SPECIFIED ON 4664 (FLSKRIV-NU = YES)         
095800*    -- OR ALWAYS IF WE COME FROM 4622                                    
095900*    -- OR ALWAYS IF IT IS A WEB DC (WHERE FLSKRIV-NU IS N AND            
096000*    -- CANNOT BE SET TO J)                                               
096100     IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                  
096200     OR TRPD-FLLDCKND = YES                                               
096300       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
096400                           PRT-WRITE                                      
096500                           W-IDPRTLST                                     
096600                           ALT-PCB                                        
096700                           PRT-RADSKIP                                    
096800                           ARB-RAD                                        
096900     END-IF                                                               
097000     .                                                                    
097100     EJECT                                                                
097200                                                                          
097300 S05-PRINT-HEAD-1 SECTION.                                                
097400     IF WS-IX = ZERO                                                      
097500       PERFORM S30-IMPORTER                                               
097600     END-IF                                                               
                                                                                
           IF W-PAGE-NO = ZEROES                                                
              PERFORM S07-PRINT-META                                            
           END-IF                                                               
097700                                                                          
097800     MOVE SPACE                      TO SEND-RAD                          
097900     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
098000     PERFORM S90-PUT-DOC-LINE                                             
098100     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
098200     PERFORM S90-PUT-DOC-LINE                                             
098300     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
098400     PERFORM S90-PUT-DOC-LINE                                             
098500                                                                          
098600     MOVE BEVARREF-LEDTEXT (W-KDSPRAK) TO RAD1H-IMPORTER-TEXT             
098700     IF NOT DIST-RYSSLAND                                                 
098800       MOVE SPACE               TO RAD2-VKORDBTO                          
098900     END-IF                                                               
099000     MOVE WS-TYP-IDSHIP         TO RAD-TYP-IDSHIP                         
099100     MOVE RAD-HEAD              TO ARB-RAD                                
099200                                   SEND-RAD                               
099300     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
099400     MOVE PRT-NYSIDA-RAD7       TO PRT-RADSKIP                            
099500     MOVE 7                     TO W-LINE-COUNT                           
099600     PERFORM S04-PRINT-LINE                                               
099700                                                                          
099800     MOVE SPACE                      TO SEND-RAD                          
099900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
100000*    PERFORM S90-PUT-DOC-LINE                                             
100100*    PERFORM S90-PUT-DOC-LINE                                             
100200                                                                          
100300                                                                          
100400     MOVE RAD2-HEAD                  TO ARB-RAD                           
100500                                        SEND-RAD                          
100600     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
100700     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
100800     ADD 1                           TO W-LINE-COUNT                      
100900     PERFORM S04-PRINT-LINE                                               
101000     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
101100                                                                          
101200     MOVE RAD3-HEAD                  TO ARB-RAD                           
101300                                        SEND-RAD                          
101400     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
101500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
101600     ADD 1                           TO W-LINE-COUNT                      
101700     PERFORM S04-PRINT-LINE                                               
101800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
101900     ADD     1                TO W-PAGE-NO                                
102000     MOVE SPACE               TO RAD1                                     
102100     MOVE BET-ADBETRAD-2      TO RAD4H-IMPORTER                           
102200     MOVE SHIP-TISKEPPN       TO W-YYMMDD                                 
102300     MOVE W-YYMMDD            TO RAD1-TIAAMMDD                            
102400     MOVE TRPD-IDDISTR        TO RAD1-IDDISTR                             
102500     MOVE SHIP-IDSHIPM        TO RAD1-IDSHIPM                             
102600     MOVE SHIP-IDTRPTNR       TO RAD1-IDTRPTNR                            
102700     MOVE SHIP-IDLBBET        TO RAD1-IDLBBET                             
102800     MOVE W-PAGE-NO           TO RAD1-PAGE-NO                             
102900     MOVE RAD1                TO ARB-RAD                                  
103000                                 SEND-RAD                                 
103100     MOVE PRT-AFTER-1         TO PRT-RADSKIP                              
103200     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
103300     ADD  1                   TO W-LINE-COUNT                             
103400     PERFORM S04-PRINT-LINE                                               
103500     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
103600                                                                          
103700     MOVE RAD5-HEAD           TO ARB-RAD                                  
103800                                 SEND-RAD                                 
103900     MOVE PRT-AFTER-1         TO PRT-RADSKIP                              
104000     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
104100     ADD 1                    TO W-LINE-COUNT                             
104200     PERFORM S04-PRINT-LINE                                               
104300     .                                                                    
104400     EJECT                                                                
104500                                                                          
104600 S06-PRINT-HEAD-2 SECTION.                                                
104700                                                                          
104800*    MOVE SPACES           TO ARB-RAD                                     
104900     MOVE RAD-2            TO ARB-RAD                                     
105000                              SEND-RAD                                    
105100     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
105200     MOVE WS-SKIP2         TO STYRTECKEN-RAD                              
105300     ADD 2                 TO W-LINE-COUNT                                
105400     PERFORM  S04-PRINT-LINE                                              
105500     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
105600                                                                          
105700     MOVE  SPACE           TO ARB-RAD                                     
105800                              SEND-RAD                                    
105900     MOVE  PRT-AFTER-1     TO PRT-RADSKIP                                 
106000     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
106100     ADD   1               TO W-LINE-COUNT                                
106200     PERFORM S04-PRINT-LINE                                               
106300     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
106400     .                                                                    
106500     EJECT                                                                
                                                                                
       S07-PRINT-META SECTION.                                                  
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-IDSHIPM       TO WS-IDSHIPM-Z                              
           STRING WS-META                                                       
                  'SHIPMENT_NUMBER='                                            
                  WS-IDSHIPM-Z                                                  
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           STRING WS-META                                                       
                  'DOCUMENT_TYPE='                                              
                  WS-TYP-IDSHIP                                                 
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-TISKEPPN      TO W-YYMMDD                                  
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
                                                                                
           STRING WS-META                                                       
                  'SHIPPING_DATE='                                              
                  WS-YEAR(1:2)                                                  
                  W-YYMMDD                                                      
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE TRPD-IDDISTR       TO WS-IDDISTR                                
           STRING WS-META                                                       
                  'DISTRICT_NUMBER='                                            
                  WS-IDDISTR                                                    
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
           MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
           MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
           MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
           MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
           MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
                                                                                
           STRING WS-META                                                       
                  'FILE_NAME='                                                  
                  DELIMITED BY SIZE                                             
                  'SHIPDOC_CI'                                                  
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  FUNCTION TRIM (WS-IDSHIPM-Z)                                  
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  FUNCTION TRIM (WS-IDDISTR)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  WS-TIMESTAMP                                                  
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
           .                                                                    
                                                                                
106600 S20-HAMTA-WDB2  SECTION.                                                 
106700                                                                          
106800     MOVE SGMT-IDKUNDNR      TO W-WDB201-IDKUNDNR                         
106900     PERFORM IMS-GU-WDB201                                                
107000     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
107100       MOVE +2 TO W-KDSPRAK                                               
107200     ELSE                                                                 
107300       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
107400     END-IF                                                               
107500     IF W-KDSPRAK NOT = +2                                                
107600       MOVE IDSTATNR-LEDTEXT   (W-KDSPRAK) TO RAD2-IDSTATNR               
107700       MOVE BEARTURS-LEDTEXT   (W-KDSPRAK) TO RAD2-KDARTURS               
107800       MOVE KBIL-KVANT-LEDTEXT (W-KDSPRAK) TO RAD2-KVART                  
107900       MOVE VKORDNTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDNTO               
108000       MOVE VKORDBTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDBTO               
108100       IF NOT DIST-RYSSLAND                                               
108200         MOVE SPACE                        TO RAD2-VKORDBTO               
108300       END-IF                                                             
108400       MOVE KBIL-TOTAL-LEDTEXT (W-KDSPRAK) TO RAD2-SUORDV-TOT             
108500       MOVE KDVALISO-LEDTEXT   (W-KDSPRAK) TO RAD2-KDVALISO               
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900 S30-IMPORTER SECTION.                                                    
109000                                                                          
109100     MOVE SGMT-IDKUNDNR  TO W-WDB201-IDKUNDNR                             
109200     PERFORM IMS-GU-WDB201                                                
109300     MOVE SGMT-IDPARTNR  TO W-WDB101-IDPARTNR                             
109400     MOVE GMT-IDFTG      TO W-WDB101-IDFTG                                
109500     PERFORM IMS-GU-WDB101                                                
109600     MOVE BET-BEBETRAD-1 TO RAD1H-IMPORTER                                
109700     MOVE BET-BEBETRAD-2 TO RAD2H-IMPORTER                                
109800     MOVE BET-ADBETRAD-1 TO RAD3H-IMPORTER                                
109900     MOVE BET-ADBETRAD-2 TO RAD4H-IMPORTER                                
110000     MOVE BET-BELAND-SVE TO RAD5H-IMPORTER                                
110100     MOVE BET-FLLOCCUR   TO FLLOCCUR-SW                                   
110200     ADD +1 TO WS-IX                                                      
110300     .                                                                    
110400     EJECT                                                                
110500 S90-PUT-DOC-LINE SECTION.                                                
110600     IF TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1'                   
110700       IF TRPD-FLSKRIV-ONDEM = YES                                        
110800         MOVE +1                          TO SEND-IDCOM                   
110900         MOVE 'PUT'                       TO SEND-KDFUNC                  
111000         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
111100         CALL WZ01SEND USING SEND-CONTROL-AREA                            
111200                             SEND-KVDLEN                                  
111300                             SEND-RAD-STYRTECKEN                          
111400         IF SEND-KDRC > ZERO                                              
111500           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
111600           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
111700           DELIMITED BY SIZE INTO ERRTEXT-STR                             
111800           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
111900         END-IF                                                           
112000       END-IF                                                             
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400* --- IMS SECTIONS  ---                                                   
112500                                                                          
112600 IMS-GU-WDE101-11  SECTION.                                               
112700                                                                          
112800     STRING 'WDE101  *PD(IDSHIPM  =' W-IDSHIPM-X ')'                      
112900          DELIMITED BY SIZE INTO SSA1                                     
113000     STRING 'WDE111  *D(WDE111KY>=' W-WDE111KY-MIN                        
113100                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
113200          DELIMITED BY SIZE INTO SSA2                                     
113300*    STRING 'WDE121  *D(WDE121KY>=' W-WDE121KY-MIN                        
113400*                   '&WDE121KY<=' W-WDE121KY-MAX ')'                      
113500*         DELIMITED BY SIZE INTO SSA3                                     
113600*    MOVE   'WDE131 ' TO SSA4                                             
113700     MOVE '  GE' TO GOOD-STATUSCODES                                      
113800     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101-11-21-31                
113900                                         SSA1 SSA2                        
114000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
114100     PERFORM IMS-STATUSCHECK                                              
114200     .                                                                    
114300     EJECT                                                                
114400                                                                          
114500*IMS-GNP-WDE131     SECTION.                                              
114600                                                                          
114700** READ ALL THE WDE131 SEGMENT UNDER THE SAME DISTRICT                    
114800                                                                          
114900*    STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
115000*                   '&WDE111KY<=' W-WDE111KY-MAX ')'                      
115100*         DELIMITED BY SIZE INTO SSA1                                     
115200*    STRING 'WDE121  (WDE121KY>=' W-WDE121KY-MIN                          
115300*                   '&WDE121KY<=' W-WDE121KY-MAX ')'                      
115400*         DELIMITED BY SIZE INTO SSA2                                     
115500*    MOVE   'WDE131  '  TO SSA3                                           
115600*    MOVE '  GEGB' TO GOOD-STATUSCODES                                    
115700*    CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
115800*                                                  SSA3                   
115900*    MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
116000*    PERFORM IMS-STATUSCHECK                                              
116100*    .                                                                    
116200*    EJECT                                                                
116300 IMS-GNP-WDE111 SECTION.                                                  
116400                                                                          
116500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
116800                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
116900          DELIMITED BY SIZE INTO SSA2                                     
117000     MOVE '  GE' TO GOOD-STATUSCODES                                      
117100     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
117200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
117300     PERFORM IMS-STATUSCHECK                                              
117400     .                                                                    
117500     EJECT                                                                
117600 IMS-GNP-WDE121  SECTION.                                                 
117700                                                                          
117800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
117900          DELIMITED BY SIZE INTO SSA1                                     
118000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
118100          DELIMITED BY SIZE INTO SSA2                                     
118200     MOVE 'WDE121  '          TO SSA3                                     
118300     MOVE '  GE' TO GOOD-STATUSCODES                                      
118400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
118500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
118600     PERFORM IMS-STATUSCHECK                                              
118700     .                                                                    
118800     EJECT                                                                
118900 IMS-GNP-WDE131  SECTION.                                                 
119000                                                                          
119100     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
119200          DELIMITED BY SIZE INTO SSA1                                     
119300     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
119400          DELIMITED BY SIZE INTO SSA2                                     
119500     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
119600          DELIMITED BY SIZE INTO SSA3                                     
119700     MOVE 'WDE131  '          TO SSA4                                     
119800     MOVE '  GE' TO GOOD-STATUSCODES                                      
119900     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
120000                                                   SSA3 SSA4              
120100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
120200     PERFORM IMS-STATUSCHECK                                              
120300     .                                                                    
120400     EJECT                                                                
120500 IMS-GU-WDB101 SECTION.                                                   
120600                                                                          
120700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
120800          DELIMITED BY SIZE INTO SSA1                                     
120900     MOVE '  GE' TO GOOD-STATUSCODES                                      
121000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
121100     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
121200     PERFORM IMS-STATUSCHECK                                              
121300     .                                                                    
121400     EJECT                                                                
121500 IMS-GU-WDB201 SECTION.                                                   
121600                                                                          
121700     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
121800          DELIMITED BY SIZE INTO SSA1                                     
121900     MOVE '  GE' TO GOOD-STATUSCODES                                      
122000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
122100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
122200     PERFORM IMS-STATUSCHECK                                              
122300     .                                                                    
122310     EJECT                                                                
122500 IMS-STATUSCHECK SECTION.                                                 
122600                                                                          
122700     SET STATUS-IX TO 1                                                   
122800     SEARCH GOOD-STATUS                                                   
122900       AT END                                                             
123000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
123100           DELIMITED BY SIZE INTO ERRTEXT                                 
123200         DISPLAY ERRTEXT                                                  
123300         CALL FELLOG                                                      
123400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
123500         CONTINUE                                                         
123600     END-SEARCH                                                           
123700     .                                                                    
123800                                                                          
