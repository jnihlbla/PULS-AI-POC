000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W9803000.                                            
000300 AUTHOR.             MARGARETA GABRIELSSON.                               
000400     DATE-WRITTEN.       NOVEMBER 1984.                                   
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*        PROGRAMMET UPPDATERAR SOP-REGISTRET MED INFORMATION              
000900*        OM SYSTEMS, RUTINERS OCH JOBBS BEROENDEN                         
001000*        OCH INNEHÅLL.                                                    
001100*        INFORMATIONEN KOMMER FRÅN DATA MANAGER VIA EN FIL.               
001200*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*    DDNAMN W98030D1 ANVÄNDS AV SUBRUTIN W980WSPC                         
002100*                                                                         
002200*--- INFILEN:                                                             
002300     SELECT DMRFIL                      ASSIGN TO UT-S-W98030D2.          
002400*                                                                         
002500*--- LISTFILEN:                                                           
002600     SELECT W98030-001                  ASSIGN TO UT-S-W98030D3.          
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE  SECTION.                                                           
003100     SKIP2                                                                
003200 FD  DMRFIL                                                               
003300     BLOCK CONTAINS 0                                                     
003400     LABEL RECORD STANDARD                                                
003500     RECORDING MODE F.                                                    
003600*01  DMR-POST   -COPY W9803001      -L.                                   
003700     SKIP3                                                                
003800 FD  W98030-001                                                           
003900     LABEL RECORD    STANDARD                                             
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300 01  W98030-001-RAD          PIC X(121).                                  
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 01  KONSTANTER.                                                          
005000     03  JA                  PIC X       VALUE 'J'.                       
005100     03  NEJ                 PIC X       VALUE 'N'.                       
005200     03  PROCESS-KONST       PIC X(13)   VALUE 'PROCESS      '.           
005300     03  CONTAINS-KONST      PIC X(13)   VALUE 'SUB PROCESSES'.           
005400     03  CATALOG-KONST       PIC X(13)   VALUE 'CATALOG WORDS'.           
005500     03  STYP-KONST          PIC X(13)   VALUE 'START TYPE   '.           
005600     03  BEROENDEN-KONST     PIC X(13)   VALUE 'DEPENDENCIES '.           
005700     03  PRIO-KONST          PIC X(13)   VALUE 'PRIORITY     '.           
005800     03  VOUT-KONST          PIC X(13)   VALUE 'V-DATA OUTPUT'.           
005900     03  FTID-KONST          PIC X(13)   VALUE 'OUTPUT TIME  '.           
006000     03  BEFORE-KONST        PIC X(2)    VALUE '> '.                      
006100     03  AFTER-KONST         PIC X(2)    VALUE '< '.                      
006200     03  EJ-SAMT-KONST       PIC X(2)    VALUE '<>'.                      
006300     03  AMSG-KONST          PIC X(13)   VALUE 'ACTIV MESSAGE'.           
006400     03  USING-KONST         PIC X(9)    VALUE 'RESOURCES'.               
006500     03  ABEND-JOB-KONST     PIC X(9)    VALUE 'ABEND JOB'.               
006600     03  PASSIVE-STATUS      PIC X       VALUE 'P'.                       
006700     03  ENDED-STATUS        PIC X       VALUE 'E'.                       
006800     03  STARTED-STATUS      PIC X       VALUE 'S'.                       
006900     03  WAITING-STATUS      PIC X       VALUE 'W'.                       
007000     SKIP2                                                                
007100 01  DIVERSE-SWITCHAR.                                                    
007200     03  DMRFIL-EOF          PIC X       VALUE 'N'.                       
007300     03  LAGG-UPP-CONTAINS   PIC X       VALUE 'J'.                       
007400     03  LAGG-UPP-PROCESS    PIC X       VALUE 'J'.                       
007500     03  FIRST-SUB-PROCESS   PIC X       VALUE 'J'.                       
007600     03  RES-WAIT            PIC X       VALUE 'N'.                       
007700     03  DEP-SIGN-LAST       PIC X       VALUE 'N'.                       
007800     03  RESERVERAT-KATALOGORD                                            
007900                             PIC X       VALUE 'N'.                       
008000     SKIP2                                                                
008100 01  DAGENS-DATUM                PIC 9(6).                                
008200     SKIP2                                                                
008300 01  DIVERSE-INDEX-OCH-PEKARE.                                            
008400     03  AIX                 PIC S9(9)              COMP SYNC.            
008500     03  VIX                 PIC S9(9)              COMP SYNC.            
008600     03  AIX-MAX             PIC S9(9) VALUE +20    COMP SYNC.            
008700     03  VIX-MAX             PIC S9(9) VALUE +20    COMP SYNC.            
008800     03  PEK                 PIC S9(9) VALUE ZERO   COMP SYNC.            
008900     03  SPAR-PEK            PIC S9(9) VALUE ZERO   COMP SYNC.            
009000     03  LIST-PEK            PIC S9(9) VALUE ZERO   COMP SYNC.            
009100     03  SIX                 PIC S9(9) VALUE ZERO   COMP SYNC.            
009200     03  SIX-HIGH            PIC S9(9) VALUE ZERO   COMP SYNC.            
009300     03  KIX                 PIC S9(9) VALUE ZERO   COMP SYNC.            
009400     03  KIX-MAX             PIC S9(9) VALUE +10    COMP SYNC.            
009500                                                                          
009600                                                                          
009700 01  WDATA                   PIC X(70).                                   
009800                                                                          
009900 01  REST1                   PIC X(80).                                   
010000 01  REST2                   PIC X(80).                                   
010100                                                                          
010200                                                                          
010300 01  WFTID-5                 PIC X(5)    JUST.                            
010400 01  FILLER  REDEFINES WFTID-5.                                           
010500     03  FILLER              PIC X.                                       
010600     03  WFTID-4             PIC X(4).                                    
010700                                                                          
010800 01  WFTID-DATA.                                                          
010900     03  WFDAG               PIC X       JUST.                            
011000     03  WFTID-HHMM.                                                      
011100       05 WFTID-HH           PIC XX.                                      
011200       05 WFTID-MM           PIC XX.                                      
011300                                                                          
011400 01  WFTID-LENGD             PIC S9(4)   COMP.                            
011500 01  WFDAG-LENGD             PIC S9(4)   COMP.                            
011600     EJECT                                                                
011700 01  KATALOGORDS-LISTA.                                                   
011800     03  FILLER              PIC X(15) VALUE 'SYSTEM         '.           
011900     03  FILLER              PIC X(15) VALUE 'ROUTINE        '.           
012000     03  FILLER              PIC X(15) VALUE 'JOB            '.           
012100     03  FILLER              PIC X(15) VALUE 'BMP            '.           
012200     03  FILLER              PIC X(15) VALUE 'DB2-UTIL       '.           
012300     03  FILLER              PIC X(15) VALUE 'DB2-JOB        '.           
012400     03  FILLER              PIC X(15) VALUE '-              '.           
012500     03  FILLER              PIC X(15) VALUE '-              '.           
012600     03  FILLER              PIC X(15) VALUE '-              '.           
012700     03  FILLER              PIC X(15) VALUE '-              '.           
012800                                                                          
012900 01  KATALOGORDS-TABELL REDEFINES KATALOGORDS-LISTA.                      
013000     03  KATALOGORD          OCCURS 10                                    
013100                             PIC X(15).                                   
013200     EJECT                                                                
013300 01  LIST-HJALP-AREA.                                                     
013400     03  LIST-KANT-RUBRIK    PIC X(13) VALUE SPACE.                       
013500     03  LIST-DIVERSE-VERDE  PIC X(70) VALUE SPACE.                       
013600     03  LIST-HOGER-ORD      PIC X(32) VALUE SPACE.                       
013700     03  LIST-VANSTER-ORD    PIC X(32) VALUE SPACE.                       
013800     03  LIST-TECKEN         PIC X(2)  VALUE SPACE.                       
013900     03  LIST-PROCESS        PIC X(32) VALUE SPACE.                       
014000     03  LIST-SUB-PROCESS    PIC X(32) VALUE SPACE.                       
014100     03  LIST-KATALOG-ORD    PIC X(70) VALUE SPACE.                       
014200     03  LIST-MSG            PIC X(100) VALUE SPACE.                      
014300     SKIP3                                                                
014400 01  M300-MSG.                                                            
014500     03  FILLER              PIC X(60) VALUE                              
014600     'SOP300W  PROCESS NAME TRUNCATED TO 10 CHARACTERS.'.                 
014700                                                                          
014800 01  M301-MSG.                                                            
014900     03  FILLER              PIC X(29) VALUE                              
015000     'SOP301E  ILLEGAL START TYPE: '.                                     
015100     03  M301-STARTTYPE      PIC X(10).                                   
015200     03  FILLER              PIC X(21) VALUE SPACE.                       
015300                                                                          
015400 01  M302-MSG.                                                            
015500     03  FILLER              PIC X(9)  VALUE 'SOP302E  '.                 
015600     03  M302-PROCESS        PIC X(10).                                   
015700     03  FILLER              PIC X(41)                                    
015800         VALUE ' NOT FOUND IN DATA BASE.'.                                
015900                                                                          
016000 01  M303-MSG.                                                            
016100     03  FILLER              PIC X(9)  VALUE 'SOP303W  '.                 
016200     03  FILLER              PIC X(41)                                    
016300         VALUE ' PARENT HAS BEEN CHANGED. OLD PARENT WAS '.               
016400     03  M303-PARENT         PIC X(10).                                   
016500                                                                          
016600 01  M304-MSG.                                                            
016700     03  FILLER              PIC X(9)  VALUE 'SOP304E  '.                 
016800     03  FILLER              PIC X(25)                                    
016900         VALUE ' INVALID PRIORITY NUMBER.'.                               
017000                                                                          
017100 01  M305-MSG.                                                            
017200     03  FILLER              PIC X(9)  VALUE 'SOP305E  '.                 
017300     03  FILLER              PIC X(25)                                    
017400         VALUE ' INVALID OUTPUT TYPE.'.                                   
017500                                                                          
017600 01  M306-MSG.                                                            
017700     03  FILLER              PIC X(9)  VALUE 'SOP306S  '.                 
017800     03  FILLER              PIC X(50)                                    
017900       VALUE ' OUTPUT TIME PARM IS NO LONGER VALID IN SOP.'.              
018000                                                                          
018100 01  M307-MSG.                                                            
018200     03  FILLER              PIC X(9)  VALUE 'SOP307E  '.                 
018300     03  FILLER              PIC X(29)                                    
018400       VALUE ' DEPENDENCE SIGN IS MISSING: '.                             
018500     03  M307-DEP-STRING     PIC X(80).                                   
018600                                                                          
018700 01  M308-MSG.                                                            
018800     03  FILLER              PIC X(9)  VALUE 'SOP308E  '.                 
018900     03  FILLER              PIC X(32)                                    
019000      VALUE ' LEFT OR RIGHT SIDE IS MISSING: '.                           
019100     03  M308-DEP-STRING     PIC X(80).                                   
019200                                                                          
019300 01  M309-MSG.                                                            
019400     03  FILLER              PIC X(9)  VALUE 'SOP309W  '.                 
019500     03  FILLER              PIC X(50)                                    
019600      VALUE ' ABEND JOB NAME TRUNCATED TO 8 CHARACTERS.'.                 
019700                                                                          
019800 01  M310-MSG.                                                            
019900     03  FILLER              PIC X(9)  VALUE 'SOP310W  '.                 
020000     03  FILLER              PIC X(36)                                    
020100      VALUE ' PROCESS NOT ADDED BECAUSE OF TYPE: '.                       
020200     03  M310-PROCESS-TYP    PIC X(3)  VALUE SPACE.                       
020300     EJECT                                                                
020400 01  DYNAMISKA-SUBPROGRAM.                                                
020500     03  ABEND               PIC X(8)    VALUE 'ABEND  '.                 
020600     SKIP2                                                                
020700*                            PARAMETRAR TILL ABEND                        
020800*                                                                         
020900 01  RETURKODER.                                                          
021000     03  RKOD                    PIC S9(4)   COMP VALUE +0.               
021100     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.              
021200     03  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.            
021300     SKIP3                                                                
021400 01  FILLER               PIC X(16) VALUE 'HJÄLP AREA      '.             
021500 01  W-AREA.                                                              
021600     03  W-PROCESS-TYP           PIC X(3)  VALUE SPACE.                   
021700     03  W-SUB-PROCESS-TYP       PIC X(7)  VALUE SPACE.                   
021800     03  W-PROCESS-NAMN          PIC X(32) VALUE SPACE.                   
021900     03  W-DMR-PROCESS-NAMN      PIC X(32) VALUE SPACE.                   
022000     03  W-LENGD                 PIC S9(4) COMP VALUE ZERO.               
022100     03  W-KOMMENTAR-TKN         PIC X(2)  VALUE SPACE.                   
022200     03  W-KOMMENTAR-ORD         PIC X(10) VALUE SPACE.                   
022300     03  W-STYP-TEST             PIC X(15) VALUE SPACE.                   
022400     03  W-NOT                   PIC X(4)  VALUE SPACE.                   
022500     03  W-PRIO                  PIC X(1).                                
022600     03  W-FTID                  PIC X(4).                                
022700     03  W-MSG-DELIM             PIC X.                                   
022800     EJECT                                                                
022900 01  FILLER               PIC X(16) VALUE 'ALLMAN TABELL   '.             
023000 01  ALLMAN-TABELL.                                                       
023100     03  A-TAB   OCCURS 20.                                               
023200         05  A-KOMMENTAR-ORD         PIC X(10).                           
023300         05  A-KOMMENTAR-LENGD       PIC S9(4) COMP.                      
023400     SKIP3                                                                
023500 01  FILLER               PIC X(16) VALUE 'VANSTERTABELL   '.             
023600 01  VANSTER-TABELL.                                                      
023700     03  V-TAB OCCURS 20.                                                 
023800         05  V-KOMMENTAR-ORD         PIC X(10).                           
023900         05  V-KOMMENTAR-LENGD       PIC S9(4) COMP.                      
024000     SKIP3                                                                
024100 01  FILLER               PIC X(16) VALUE 'SAKNASTABELL    '.             
024200 01  SAKNAS-TABELL.                                                       
024300     03  SAKNAS-PROCESS-PARM OCCURS 1000.                                 
024400         05  SAKNAS-PROCESS-LENGD    PIC S9(4) COMP.                      
024500         05  SAKNAS-PROCESS-VERDE    PIC X(10).                           
024600     EJECT                                                                
024700 01  FILLER               PIC X(16) VALUE 'DMR-AREA        '.             
024800*01  AREA  -COPY W9803001      -PRE DMR-                                  
024900     EJECT                                                                
025000*------- PARAMETERAR TILL W980WSPC (HANTERING AV SOP-REGISTRET)           
025100 01  W980WSPC                PIC X(8)    VALUE 'W980WSPC'.                
025200                                                                          
025300 01  FUNKTIONSKODER.                                                      
025400     03  FOPEN               PIC X(4)    VALUE 'OPEN'.                    
025500     03  FCLSE               PIC X(4)    VALUE 'CLSE'.                    
025600     03  FSAVE               PIC X(4)    VALUE 'SAVE'.                    
025700     03  FADD                PIC X(4)    VALUE 'ADD '.                    
025800     03  FGETF               PIC X(4)    VALUE 'GETF'.                    
025900     03  FGETN               PIC X(4)    VALUE 'GETN'.                    
026000     03  FTEST               PIC X(4)    VALUE 'TEST'.                    
026100     03  FSET                PIC X(4)    VALUE 'SET '.                    
026200     03  FDEL                PIC X(4)    VALUE 'DEL '.                    
026300     03  FDELK               PIC X(4)    VALUE 'DELK'.                    
026400                                                                          
026500 01  WSRKOD                  PIC X.                                       
026600                                                                          
026700 01  PROCESS-PARM.                                                        
026800     03   PROCESS-LENGD      PIC S9(4)   COMP.                            
026900     03   PROCESS-VERDE      PIC X(32).                                   
027000                                                                          
027100 01  WPROCESS-PARM.                                                       
027200     03   WPROCESS-LENGD     PIC S9(4)   COMP.                            
027300     03   WPROCESS-VERDE     PIC X(10).                                   
027400                                                                          
027500 01  SUB-PROCESS-PARM.                                                    
027600     03   SUB-PROCESS-LENGD  PIC S9(4)   COMP.                            
027700     03   SUB-PROCESS-VERDE  PIC X(32).                                   
027800                                                                          
027900 01  DATA-PARM.                                                           
028000     03   DATA-LENGD         PIC S9(4)   COMP.                            
028100     03   DATA-VERDE         PIC X(240).                                  
028200                                                                          
028300 01  RES-PARM.                                                            
028400     03   RES-LENGD          PIC S9(4)   COMP.                            
028500     03   RES-VERDE         PIC X(240).                                   
028600                                                                          
028700 01  TEMP-PARM.                                                           
028800     03   TEMP-LENGD         PIC S9(4)   COMP.                            
028900     03   TEMP-VERDE         PIC X(40).                                   
029000                                                                          
029100 01  MTYP-VERDE-PARM.                                                     
029200     03   FILLER             PIC S9(4)   COMP  VALUE +3.                  
029300     03   MTYP-VERDE         PIC X(1).                                    
029400     EJECT                                                                
029500 01  WSPACE-DD-PARM.                                                      
029600     03   FILLER             PIC S9(4)   COMP  VALUE +10.                 
029700     03   WSPACE-DD-NAMN     PIC X(8)    VALUE 'W98030D1'.                
029800                                                                          
029900 01  BEFORE-PARM.                                                         
030000     03   FILLER             PIC S9(4)   COMP  VALUE +3.                  
030100     03   FILLER             PIC X(1)    VALUE '>'.                       
030200                                                                          
030300 01  AFTER-PARM.                                                          
030400     03   FILLER             PIC S9(4)   COMP  VALUE +3.                  
030500     03   FILLER             PIC X(1)    VALUE '<'.                       
030600                                                                          
030700 01  EJ-SAMTIDIGT-PARM.                                                   
030800     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
030900     03   FILLER             PIC X(2)    VALUE '<>'.                      
031000                                                                          
031100 01  CONTAINS-PARM.                                                       
031200     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
031300     03   FILLER             PIC X(3)    VALUE 'CON'.                     
031400                                                                          
031500 01  PARENT-PARM.                                                         
031600     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
031700     03   FILLER             PIC X(3)    VALUE 'PAR'.                     
031800                                                                          
031900 01  CATALOG-PARM.                                                        
032000     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
032100     03   FILLER             PIC X(3)    VALUE 'CAT'.                     
032200                                                                          
032300 01  NOT-CATALOG-PARM.                                                    
032400     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
032500     03   FILLER             PIC X(4)    VALUE 'NCAT'.                    
032600                                                                          
032700 01  INFO-ATTR-PARM.                                                      
032800     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
032900     03   FILLER             PIC X(3)    VALUE 'INF'.                     
033000                                                                          
033100 01  START-TYP-PARM.                                                      
033200     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
033300     03   FILLER             PIC X(4)    VALUE 'STYP'.                    
033400                                                                          
033500 01  PRIO-PARM.                                                           
033600     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
033700     03   FILLER             PIC X(4)    VALUE 'PRIO'.                    
033800                                                                          
033900 01  VD-OUTPUT-PARM.                                                      
034000     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
034100     03   FILLER             PIC X(4)    VALUE 'VOUT'.                    
034200                                                                          
034300 01  FARDIGTID-PARM.                                                      
034400     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
034500     03   FILLER             PIC X(4)    VALUE 'FTID'.                    
034600                                                                          
034700 01  MBRTYP-PARM.                                                         
034800     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
034900     03   FILLER             PIC X(4)    VALUE 'MTYP'.                    
035000                                                                          
035100 01  ACTMSG-PARM.                                                         
035200     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
035300     03   FILLER             PIC X(4)    VALUE 'AMSG'.                    
035400                                                                          
035500 01  USING-PARM.                                                          
035600     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
035700     03   FILLER             PIC X(4)    VALUE 'USNG'.                    
035800                                                                          
035900 01  USED-PARM.                                                           
036000     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
036100     03   FILLER             PIC X(4)    VALUE 'USED'.                    
036200                                                                          
036300 01  RESQ-PARM.                                                           
036400     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
036500     03   FILLER             PIC X(2)    VALUE 'RQ'.                      
036600                                                                          
036700 01  ABEND-JOB-PARM.                                                      
036800     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
036900     03   FILLER             PIC X(4)    VALUE 'ABJB'.                    
037000     EJECT                                                                
037100 01  INFO-DATA-PARM.                                                      
037200*    03 -COPY W980INF                                                     
037300     EJECT                                                                
037400 01  STYRTECKEN-FOR-RADSKIP.                                              
037500     SKIP2                                                                
037600     03  NY-SIDA             PIC X(1)   VALUE '1'.                        
037700     03  ENKEL-SKIP          PIC X(1)   VALUE ' '.                        
037800     03  DUBBEL-SKIP         PIC X(1)   VALUE '0'.                        
037900     03  TRIPPEL-SKIP        PIC X(1)   VALUE '-'.                        
038000     03  INGET-SKIP          PIC X(1)   VALUE '+'.                        
038100 01  W001-HJALPAREOR.                                                     
038200     SKIP2                                                                
038300     03  W001-SIDRAKNARE     PIC S9(5)  COMP-3 VALUE ZERO.                
038400     03  W001-RADRAKNARE     PIC S9(5)  COMP-3 VALUE +100.                
038500     03  W001-MAX-RADER      PIC 9(3)   VALUE 42.                         
038600     03  W001-LISTNR         PIC X(11)  VALUE 'W98030-001'.               
038700     SKIP2                                                                
038800     03  W001-RAD.                                                        
038900         05  W001-SKIP       PIC X(1)   VALUE SPACE.                      
039000         05  FILLER          PIC X(120) VALUE SPACE.                      
039100     SKIP3                                                                
039200 01  W001R1-RUBRIK.                                                       
039300*                                                                         
039400     03  W001R1-SKIP         PIC X(1)   VALUE '1'.                        
039500     03  FILLER              PIC X(9)                                     
039600                                     VALUE ' AB VOLVO'.                   
039700     03  FILLER              PIC X(15)  VALUE SPACE.                      
039800     03  FILLER              PIC X(61)                                    
039900           VALUE 'S O P   -   UPDATING OF SOP DATABASE WITH PROCES        
040000-          'S INFORMATION'.                                               
040100     03  FILLER              PIC X(9)   VALUE SPACE.                      
040200     03  W001R1-DATUM        PIC 99B99B99.                                
040300     03  FILLER              PIC X(2)   VALUE SPACE.                      
040400     03  FILLER              PIC X(4)                                     
040500                                     VALUE 'PAGE'.                        
040600     03  FILLER              PIC X(2)   VALUE SPACE.                      
040700     03  W001R1-SIDNR        PIC Z(4)9.                                   
040800     SKIP2                                                                
040900 01  W001D0-DETALJ.                                                       
041000     03  W001D0-SKIP         PIC X(1)   VALUE '-'.                        
041100     03  FILLER              PIC X(1)   VALUE SPACE.                      
041200     03  FILLER              PIC X(30)                                    
041300                                 VALUE ALL '-'.                           
041400     03  FILLER              PIC X(3)   VALUE SPACE.                      
041500     03  FILLER              PIC X(11)                                    
041600                                 VALUE 'DMR MEMBER '.                     
041700     03  FILLER              PIC X(1)   VALUE SPACE.                      
041800     03  W001D0-PROCESS      PIC X(32).                                   
041900     03  FILLER              PIC X(1)   VALUE SPACE.                      
042000     03  FILLER              PIC X(30)                                    
042100                                 VALUE ALL '-'.                           
042200     SKIP2                                                                
042300 01  W001D1-DETALJ.                                                       
042400     03  W001D1-SKIP         PIC X(1)    VALUE '0'.                       
042500     03  FILLER              PIC X(1)    VALUE SPACE.                     
042600     03  W001D1-KANT-RUB     PIC X(7).                                    
042700     03  FILLER              PIC X(11)   VALUE SPACE.                     
042800     03  W001D1-PROCESS      PIC X(32).                                   
042900     SKIP2                                                                
043000 01  W001D2-DETALJ.                                                       
043100     03  W001D2-SKIP         PIC X(1)    VALUE '0'.                       
043200     03  FILLER              PIC X(1)    VALUE SPACE.                     
043300     03  W001D2-KANT-RUB     PIC X(13).                                   
043400     03  FILLER              PIC X(5)    VALUE SPACE.                     
043500     03  W001D2-PROCESS      PIC X(32).                                   
043600     SKIP2                                                                
043700 01  W001D3-DETALJ.                                                       
043800     03  W001D3-SKIP         PIC X(1)    VALUE '0'.                       
043900     03  FILLER              PIC X(1)    VALUE SPACE.                     
044000     03  W001D3-KANT-RUB     PIC X(13).                                   
044100     03  FILLER              PIC X(5)    VALUE SPACE.                     
044200     03  W001D3-KATALOG-ORD  PIC X(70).                                   
044300     SKIP2                                                                
044400 01  W001D4-DETALJ.                                                       
044500     03  W001D4-SKIP         PIC X(1)    VALUE '0'.                       
044600     03  FILLER              PIC X(1)    VALUE SPACE.                     
044700     03  W001D4-KANT-RUB     PIC X(13).                                   
044800     03  FILLER              PIC X(5)    VALUE SPACE.                     
044900     03  W001D4-VANSTER-ORD  PIC X(32).                                   
045000     03  FILLER              PIC X(2)    VALUE SPACE.                     
045100     03  W001D4-TECKEN       PIC X(2).                                    
045200     03  FILLER              PIC X(2)    VALUE SPACE.                     
045300     03  W001D4-HOGER-ORD    PIC X(32).                                   
045400     EJECT                                                                
045500 01  W001D5-DETALJ.                                                       
045600     03  W001D5-SKIP         PIC X(1)    VALUE '0'.                       
045700     03  FILLER              PIC X(1)    VALUE SPACE.                     
045800     03  W001D5-KANT-RUB     PIC X(13).                                   
045900     03  FILLER              PIC X(5)    VALUE SPACE.                     
046000     03  W001D5-DIVERSE      PIC X(30).                                   
046100     SKIP3                                                                
046200 01  W001D6-MSGRAD.                                                       
046300     03  W001D6-SKIP         PIC X(1)    VALUE SPACE.                     
046400     03  FILLER              PIC X(1)    VALUE SPACE.                     
046500     03  FILLER              PIC X(10)                                    
046600                                 VALUE ALL '*'.                           
046700     03  W001D6-MSG          PIC X(100).                                  
046800     EJECT                                                                
046900 LINKAGE SECTION.                                                         
047000     SKIP2                                                                
047100 01  EXEC-PARM.                                                           
047200     03  LAENGD                    PIC S9(4)   COMP.                      
047300     03  EXEC-MEDLEMS-TYP.                                                
047400         05  EXEC-MEDLEMS-TYP-POS-1-3   PIC X(3).                         
047500         05  FILLER                    PIC X(5).                          
047600     EJECT                                                                
047700 PROCEDURE DIVISION USING EXEC-PARM.                                      
047800     SKIP2                                                                
047900     PERFORM A-INIT                                                       
048000     PERFORM S01-LAS-DMRFIL                                               
048100     IF DMRFIL-EOF = NEJ                                                  
048200       PERFORM UNTIL DMRFIL-EOF = JA                                      
048300         PERFORM UNTIL DMRFIL-EOF = JA OR                                 
048400           (DMR-TKN-KOL-2-3 NOT = SPACE AND                               
048500            DMR-TKN-KOL-2-3 NOT = 'GL' AND                                
048600            DMR-TKN-KOL-2-3 NOT = 'nd')                                   
048700           PERFORM S01-LAS-DMRFIL                                         
048800         END-PERFORM                                                      
048900         IF DMRFIL-EOF = NEJ                                              
049000*                                                                         
049100           PERFORM E-HUVUD-PROCESS-START                                  
049200           PERFORM C-RENSA-HUVUD-PROCESS                                  
049300*                                                                         
049400           PERFORM S01-LAS-DMRFIL                                         
049500           IF DMRFIL-EOF = NEJ                                            
049600             MOVE DMR-TYP-POS-1-3 TO W-PROCESS-TYP                        
049700             PERFORM S02-TESTA-UPPLAGG-PROCESS                            
049800             IF LAGG-UPP-PROCESS = JA                                     
049900               PERFORM D-ALLMAN-INFO                                      
050000               PERFORM G-MTYP-INFO                                        
050100               PERFORM S01-LAS-DMRFIL                                     
050200               PERFORM B-UPPLAGG-SOPREGISTER                              
050300             ELSE                                                         
050400               PERFORM H-LAS-FRAM-NASTA                                   
050500             END-IF                                                       
050600           END-IF                                                         
050700           CALL W980WSPC USING FSAVE WSRKOD                               
050800         END-IF                                                           
050900       END-PERFORM                                                        
051000     ELSE                                                                 
051100       MOVE 12 TO RKOD                                                    
051200     END-IF                                                               
051300                                                                          
051400     IF SIX-HIGH > 0                                                      
051500       PERFORM F-TESTA-PROCESS-SAKNAS-FORTF                               
051600     END-IF                                                               
051700     PERFORM Z-SLUT                                                       
051800     MOVE RKOD TO RETURN-CODE                                             
051900     GOBACK.                                                              
052000     EJECT                                                                
052100 A-INIT SECTION.                                                          
052200     SKIP2                                                                
052300     OPEN INPUT DMRFIL                                                    
052400     OPEN OUTPUT W98030-001                                               
052500*                                                                         
052600     CALL W980WSPC USING FOPEN WSRKOD WSPACE-DD-PARM                      
052700     IF WSRKOD NOT = SPACE                                                
052800       DISPLAY 'SOP005S  CAN NOT OPEN DDNAME ' WSPACE-DD-NAMN             
052900               ' RC= ' WSRKOD                                             
053000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
053100     END-IF                                                               
053200*                                                                         
053300     ACCEPT DAGENS-DATUM    FROM DATE                                     
053400     .                                                                    
053500     EJECT                                                                
053600 B-UPPLAGG-SOPREGISTER SECTION.                                           
053700     SKIP2                                                                
053800     MOVE JA TO FIRST-SUB-PROCESS                                         
053900     PERFORM UNTIL DMRFIL-EOF = JA OR                                     
054000          (DMR-REFERS-TO NOT = 'REFERS TO' AND                            
054100           DMR-CAT-AS NOT = 'CATALOGUED AS' AND                           
054200           DMR-COMMENT NOT = 'COMMENT' AND                                
054300           DMR-SEE NOT = 'SEE')                                           
054400       IF DMR-REFERS-TO = 'REFERS TO'                                     
054500         PERFORM S01-LAS-DMRFIL                                           
054600         PERFORM UNTIL DMRFIL-EOF = JA OR                                 
054700              DMR-TKN-KOL-2-3 NOT = SPACE OR                              
054800              DMR-COMMENT = 'COMMENT' OR                                  
054900              DMR-CAT-AS = 'CATALOGUED AS' OR                             
055000              DMR-SEE = 'SEE'                                             
055100             PERFORM S03-TESTA-UPPLAGG-CONTAINS                           
055200             IF LAGG-UPP-CONTAINS = JA                                    
055300               PERFORM BA-LAGG-UPP-SUB-PROCESS                            
055400             END-IF                                                       
055500             MOVE NEJ TO FIRST-SUB-PROCESS                                
055600             PERFORM S01-LAS-DMRFIL                                       
055700         END-PERFORM                                                      
055800       ELSE IF DMR-CAT-AS = 'CATALOGUED AS'                               
055900         PERFORM S01-LAS-DMRFIL                                           
056000         PERFORM UNTIL DMRFIL-EOF = JA OR                                 
056100             DMR-TKN-KOL-2-3 NOT = SPACE OR                               
056200             DMR-COMMENT = 'COMMENT' OR                                   
056300             DMR-REFERS-TO = 'REFERS TO' OR                               
056400             DMR-SEE = 'SEE'                                              
056500           PERFORM BB-LAGG-UPP-KATALOG-ORD                                
056600           PERFORM S01-LAS-DMRFIL                                         
056700         END-PERFORM                                                      
056800       ELSE IF DMR-COMMENT = 'COMMENT'                                    
056900         PERFORM S01-LAS-DMRFIL                                           
057000         PERFORM UNTIL DMRFIL-EOF = JA OR                                 
057100             DMR-TKN-KOL-2-3 NOT = SPACE OR                               
057200             DMR-CAT-AS = 'CATALOGUED AS' OR                              
057300             DMR-REFERS-TO = 'REFERS TO' OR                               
057400             DMR-SEE = 'SEE'                                              
057500           PERFORM BC-LAGG-UPP-KOMMENTARER                                
057600           PERFORM S01-LAS-DMRFIL                                         
057700         END-PERFORM                                                      
057800       ELSE IF DMR-SEE = 'SEE'                                            
057900         MOVE USING-KONST TO LIST-KANT-RUBRIK                             
058000         PERFORM UNTIL DMRFIL-EOF = JA OR                                 
058100             DMR-TKN-KOL-2-3 NOT = SPACE OR                               
058200             DMR-CAT-AS = 'CATALOGUED AS' OR                              
058300             DMR-REFERS-TO = 'REFERS TO' OR                               
058400             DMR-COMMENT = 'COMMENT'                                      
058500           PERFORM BD-LAGG-UPP-RESURSER                                   
058600           PERFORM S01-LAS-DMRFIL                                         
058700         END-PERFORM                                                      
058800         PERFORM BE-KOLLA-RESURS-KOER                                     
058900       END-IF                                                             
059000       END-IF                                                             
059100       END-IF                                                             
059200       END-IF                                                             
059300     END-PERFORM                                                          
059400     .                                                                    
059500     EJECT                                                                
059600 BA-LAGG-UPP-SUB-PROCESS SECTION.                                         
059700     SKIP2                                                                
059800******************************************************************        
059900***      ADD AV NYA SUB-PROCESSER (DATA) UNDER PROCESSEN       ***        
060000******************************************************************        
060100*                                                                         
060200     UNSTRING DMR-SUB-PROCESS-NAMN DELIMITED BY '-RTN' OR '-JOB'          
060300         OR '-PROC' OR '-PGM'                                             
060400         OR SPACE  INTO DATA-VERDE COUNT IN DATA-LENGD                    
060500     MOVE SPACE TO LIST-MSG                                               
060600     IF DATA-LENGD > 10                                                   
060700       MOVE 10 TO DATA-LENGD                                              
060800       MOVE M300-MSG TO LIST-MSG                                          
060900       IF RKOD < +4                                                       
061000         MOVE +4 TO RKOD                                                  
061100       END-IF                                                             
061200     END-IF                                                               
061300     ADD +2 TO DATA-LENGD                                                 
061400     MOVE DATA-VERDE TO SUB-PROCESS-VERDE                                 
061500     MOVE DATA-LENGD TO SUB-PROCESS-LENGD                                 
061600                                                                          
061700     CALL W980WSPC USING FADD WSRKOD                                      
061800     PROCESS-PARM CONTAINS-PARM SUB-PROCESS-PARM                          
061900                                                                          
062000****    KONTROLLERA EV. TIDIGARE FÖRÄLDER OCH SÄTT NY.                    
062100                                                                          
062200     MOVE SPACE TO DATA-VERDE                                             
062300     CALL W980WSPC USING FGETF WSRKOD                                     
062400     SUB-PROCESS-PARM PARENT-PARM DATA-PARM                               
062500     IF DATA-PARM NOT = PROCESS-PARM                                      
062600       IF WSRKOD = SPACE                                                  
062700         MOVE DATA-VERDE TO M303-PARENT                                   
062800         MOVE M303-MSG TO LIST-MSG                                        
062900         IF RKOD < +4                                                     
063000           MOVE +4 TO RKOD                                                
063100         END-IF                                                           
063200       END-IF                                                             
063300       CALL W980WSPC USING FSET WSRKOD                                    
063400       SUB-PROCESS-PARM PARENT-PARM PROCESS-PARM                          
063500     END-IF                                                               
063600                                                                          
063700****    PRELIMINÄR TEST PÅ OM SUBPROCESSEN FINNS.                         
063800                                                                          
063900     MOVE SUB-PROCESS-PARM TO WPROCESS-PARM                               
064000     PERFORM S05-TESTA-PROCESS-FINNS                                      
064100                                                                          
064200     MOVE CONTAINS-KONST TO LIST-KANT-RUBRIK                              
064300     MOVE SUB-PROCESS-VERDE  TO LIST-SUB-PROCESS                          
064400     PERFORM S12-SKRIV-DETALJ-RAD2                                        
064500     IF LIST-MSG NOT = SPACE                                              
064600       PERFORM S16-SKRIV-MSG-RAD                                          
064700     END-IF                                                               
064800                                                                          
064900                                                                          
065000****   DELETE AV GAMLA FÖRE-BEROENDEN, EFTER-BEROENDEN OCH                
065100****   EJ-SAMTIDIGT-BEROENDEN UNDER SUB-PROCESSEN                         
065200                                                                          
065300                                                                          
065400     CALL W980WSPC USING FGETF WSRKOD                                     
065500              SUB-PROCESS-PARM BEFORE-PARM DATA-PARM                      
065600     IF WSRKOD = SPACE                                                    
065700       PERFORM UNTIL WSRKOD NOT = SPACE                                   
065800         CALL W980WSPC USING FDEL WSRKOD                                  
065900                     DATA-PARM AFTER-PARM SUB-PROCESS-PARM                
066000         CALL W980WSPC USING FGETN WSRKOD                                 
066100                     SUB-PROCESS-PARM BEFORE-PARM DATA-PARM               
066200       END-PERFORM                                                        
066300     END-IF                                                               
066400     CALL W980WSPC USING FDELK WSRKOD                                     
066500              SUB-PROCESS-PARM BEFORE-PARM                                
066600*                                                                         
066700     CALL W980WSPC USING FGETF WSRKOD                                     
066800              SUB-PROCESS-PARM AFTER-PARM DATA-PARM                       
066900     IF WSRKOD = SPACE                                                    
067000       PERFORM UNTIL WSRKOD NOT = SPACE                                   
067100         CALL W980WSPC USING FDEL WSRKOD                                  
067200                     DATA-PARM BEFORE-PARM SUB-PROCESS-PARM               
067300         CALL W980WSPC USING FGETN WSRKOD                                 
067400                     SUB-PROCESS-PARM AFTER-PARM DATA-PARM                
067500       END-PERFORM                                                        
067600     END-IF                                                               
067700     CALL W980WSPC USING FDELK WSRKOD                                     
067800              SUB-PROCESS-PARM AFTER-PARM                                 
067900*                                                                         
068000     CALL W980WSPC USING FGETF WSRKOD                                     
068100           SUB-PROCESS-PARM EJ-SAMTIDIGT-PARM DATA-PARM                   
068200     IF WSRKOD = SPACE                                                    
068300       PERFORM UNTIL WSRKOD NOT = SPACE                                   
068400         CALL W980WSPC USING FDEL WSRKOD                                  
068500                  DATA-PARM EJ-SAMTIDIGT-PARM SUB-PROCESS-PARM            
068600         CALL W980WSPC USING FGETN WSRKOD                                 
068700                  SUB-PROCESS-PARM EJ-SAMTIDIGT-PARM DATA-PARM            
068800       END-PERFORM                                                        
068900     END-IF                                                               
069000     CALL W980WSPC USING FDELK WSRKOD                                     
069100             SUB-PROCESS-PARM EJ-SAMTIDIGT-PARM                           
069200*                                                                         
069300     .                                                                    
069400     EJECT                                                                
069500 BB-LAGG-UPP-KATALOG-ORD SECTION.                                         
069600     SKIP2                                                                
069700     PERFORM BBA-RESERVERAT-KATALOGORD                                    
069800     IF RESERVERAT-KATALOGORD = NEJ                                       
069900         MOVE DMR-KATALOG-ORD  TO W-NOT                                   
070000         IF W-NOT  = 'NOT-' OR 'NOT.'                                     
070100           MOVE 'NOT ' TO W-NOT                                           
070200           MOVE 5 TO PEK                                                  
070300           UNSTRING DMR-KATALOG-ORD DELIMITED BY SPACE                    
070400                  INTO DATA-VERDE COUNT IN DATA-LENGD                     
070500                  WITH POINTER PEK                                        
070600           ADD +2 TO DATA-LENGD                                           
070700           CALL W980WSPC USING FADD WSRKOD                                
070800           PROCESS-PARM NOT-CATALOG-PARM DATA-PARM                        
070900         ELSE                                                             
071000           MOVE SPACE TO W-NOT                                            
071100           UNSTRING DMR-KATALOG-ORD DELIMITED BY SPACE                    
071200                  INTO DATA-VERDE COUNT IN DATA-LENGD                     
071300           ADD +2 TO DATA-LENGD                                           
071400           CALL W980WSPC USING FADD WSRKOD                                
071500           PROCESS-PARM CATALOG-PARM DATA-PARM                            
071600         END-IF                                                           
071700*                                                                         
071800         MOVE CATALOG-KONST TO LIST-KANT-RUBRIK                           
071900         IF W-NOT = SPACE                                                 
072000           MOVE DATA-VERDE TO LIST-KATALOG-ORD                            
072100         ELSE                                                             
072200           STRING 'NOT  ' DATA-VERDE DELIMITED BY SIZE                    
072300           INTO LIST-KATALOG-ORD                                          
072400         END-IF                                                           
072500         PERFORM S13-SKRIV-DETALJ-RAD3                                    
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 BBA-RESERVERAT-KATALOGORD  SECTION.                                      
073000     SKIP2                                                                
073100     MOVE NEJ TO RESERVERAT-KATALOGORD                                    
073200     MOVE 1 TO KIX                                                        
073300     PERFORM UNTIL KIX  > KIX-MAX                                         
073400         OR  RESERVERAT-KATALOGORD = JA                                   
073500         IF KATALOGORD (KIX) = DMR-KATALOG-ORD                            
073600             MOVE JA TO RESERVERAT-KATALOGORD                             
073700         END-IF                                                           
073800         ADD 1 TO KIX                                                     
073900     END-PERFORM                                                          
074000     .                                                                    
074100     EJECT                                                                
074200 BC-LAGG-UPP-KOMMENTARER SECTION.                                         
074300     SKIP2                                                                
074400     MOVE +1 TO PEK                                                       
074500     MOVE SPACE TO W-STYP-TEST                                            
074600     PERFORM UNTIL W-STYP-TEST NOT = SPACE OR PEK NOT < 206               
074700       UNSTRING DMR-KOMMENTAR DELIMITED BY ALL SPACE                      
074800       OR '-RTN' OR '-JOB' OR ','                                         
074900       INTO W-STYP-TEST WITH POINTER PEK                                  
075000     END-PERFORM                                                          
075100     IF W-STYP-TEST = 'START-TYPE'                                        
075200       PERFORM BCA-LAGG-UPP-STYP                                          
075300     ELSE IF W-STYP-TEST = 'PRIORITY'                                     
075400       PERFORM BCC-LAGG-UPP-PRIORITET                                     
075500     ELSE IF W-STYP-TEST = 'VD-OUTPUT'                                    
075600       PERFORM BCD-LAGG-UPP-VOLVO-DATA-OUTPUT                             
075700     ELSE IF W-STYP-TEST = 'OUTPUT-TIME'                                  
075800       MOVE M306-MSG TO LIST-MSG                                          
075900       PERFORM S16-SKRIV-MSG-RAD                                          
076000     ELSE IF W-STYP-TEST = 'ACTIVATION-MSG'                               
076100       PERFORM BCF-LAGG-UPP-MSG                                           
076200     ELSE IF W-STYP-TEST = 'IF-ABEND'                                     
076300       PERFORM BCG-LAGG-UPP-ABEND-JOB                                     
076400     ELSE                                                                 
076500       PERFORM BCB-LAGG-UPP-BEROENDEN                                     
076600     END-IF                                                               
076700     END-IF                                                               
076800     END-IF                                                               
076900     END-IF                                                               
077000     END-IF                                                               
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400 BCA-LAGG-UPP-STYP SECTION.                                               
077500     SKIP2                                                                
077600     UNSTRING DMR-KOMMENTAR DELIMITED BY ALL SPACE                        
077700         INTO DATA-VERDE COUNT IN DATA-LENGD WITH POINTER PEK             
077800     ADD +2 TO DATA-LENGD                                                 
077900*                                                                         
078000     MOVE STYP-KONST TO LIST-KANT-RUBRIK                                  
078100     MOVE DATA-VERDE TO LIST-DIVERSE-VERDE                                
078200     PERFORM S15-SKRIV-DETALJ-RAD5                                        
078300*                                                                         
078400     IF DATA-VERDE NOT = 'NONE' AND 'FSUB'                                
078500                     AND 'SUB'  AND 'REL'                                 
078600       MOVE DATA-VERDE TO M301-STARTTYPE                                  
078700       IF RKOD < +8                                                       
078800         MOVE +8 TO RKOD                                                  
078900       END-IF                                                             
079000       MOVE M301-MSG TO LIST-MSG                                          
079100       PERFORM S16-SKRIV-MSG-RAD                                          
079200     ELSE                                                                 
079300       CALL W980WSPC USING FSET WSRKOD                                    
079400       PROCESS-PARM START-TYP-PARM DATA-PARM                              
079500     END-IF                                                               
079600     .                                                                    
079700     EJECT                                                                
079800 BCB-LAGG-UPP-BEROENDEN  SECTION.                                         
079900     SKIP2                                                                
080000     PERFORM S04-NOLLSTALL-TABELLER                                       
080100     MOVE SPACE TO W-KOMMENTAR-TKN                                        
080200     MOVE SPACE TO LIST-MSG                                               
080300     MOVE +1 TO PEK AIX                                                   
080400     PERFORM UNTIL PEK NOT < 206 OR                                       
080500     (A-KOMMENTAR-ORD (AIX)  = '<' OR                                     
080600     '>' OR '<>' OR '><')                                                 
080700       PERFORM BCBA-SKAPA-ALLMAN-TAB                                      
080800       IF A-KOMMENTAR-ORD (AIX) NOT = SPACE AND                           
080900          '<' AND '>' AND '<>' AND '><'                                   
081000         ADD +1 TO AIX                                                    
081100       END-IF                                                             
081200     END-PERFORM                                                          
081300     IF A-KOMMENTAR-ORD (AIX) NOT = '<' AND '>' AND                       
081400            '<>' AND '><'                                                 
081500       IF RKOD < +8                                                       
081600         MOVE +8 TO RKOD                                                  
081700       END-IF                                                             
081800       MOVE DMR-KOMMENTAR TO M307-DEP-STRING                              
081900       MOVE M307-MSG TO LIST-MSG                                          
082000       PERFORM S16-SKRIV-MSG-RAD                                          
082100       MOVE SPACE TO LIST-MSG                                             
082200     ELSE                                                                 
082300       MOVE JA TO DEP-SIGN-LAST                                           
082400       PERFORM UNTIL PEK NOT < 206                                        
082500         IF A-KOMMENTAR-ORD (AIX) = '<' OR '>' OR '<>' OR '><'            
082600           MOVE JA TO DEP-SIGN-LAST                                       
082700           MOVE A-KOMMENTAR-ORD (AIX) TO W-KOMMENTAR-TKN                  
082800           MOVE SPACE TO A-KOMMENTAR-ORD (AIX)                            
082900           MOVE +1 TO AIX VIX                                             
083000           MOVE BEROENDEN-KONST TO LIST-KANT-RUBRIK                       
083100           IF A-KOMMENTAR-ORD (1) = SPACE                                 
083200             IF RKOD < +8                                                 
083300               MOVE +8 TO RKOD                                            
083400             END-IF                                                       
083500             MOVE DMR-KOMMENTAR TO M308-DEP-STRING                        
083600             MOVE M308-MSG TO LIST-MSG                                    
083700             PERFORM S16-SKRIV-MSG-RAD                                    
083800             MOVE SPACE TO LIST-MSG                                       
083900             MOVE NEJ TO DEP-SIGN-LAST                                    
084000           END-IF                                                         
084100           PERFORM UNTIL A-KOMMENTAR-ORD (AIX) = SPACE                    
084200             PERFORM BCBB-SKAPA-VANSTER-TAB                               
084300             ADD +1 TO AIX VIX                                            
084400           END-PERFORM                                                    
084500           MOVE SPACE TO V-KOMMENTAR-ORD (VIX)                            
084600         ELSE                                                             
084700           MOVE NEJ TO DEP-SIGN-LAST                                      
084800         END-IF                                                           
084900         MOVE +1 TO AIX VIX                                               
085000         MOVE SPACE TO A-KOMMENTAR-ORD (1)                                
085100         PERFORM UNTIL PEK NOT < 206 OR (A-KOMMENTAR-ORD (AIX)            
085200         = '>' OR '<' OR '<>' OR '><')                                    
085300           PERFORM BCBA-SKAPA-ALLMAN-TAB                                  
085400*                                                                         
085500           IF A-KOMMENTAR-ORD (AIX) NOT = SPACE AND                       
085600               '<' AND '>' AND '<>' AND '><'                              
085700             MOVE NEJ TO DEP-SIGN-LAST                                    
085800             MOVE 1 TO VIX                                                
085900             PERFORM UNTIL V-KOMMENTAR-ORD (VIX) = SPACE                  
086000               IF W-KOMMENTAR-TKN = '>'                                   
086100                 PERFORM BCBC-LAGG-UPP-BEFORE                             
086200               ELSE IF W-KOMMENTAR-TKN = '<'                              
086300                 PERFORM BCBD-LAGG-UPP-AFTER                              
086400               ELSE IF W-KOMMENTAR-TKN = '<>' OR '><'                     
086500                 PERFORM BCBE-LAGG-UPP-EJ-SAMTIDIGT                       
086600               END-IF                                                     
086700               END-IF                                                     
086800               END-IF                                                     
086900               MOVE V-KOMMENTAR-ORD (VIX) TO WPROCESS-VERDE               
087000               MOVE V-KOMMENTAR-LENGD (VIX) TO WPROCESS-LENGD             
087100               PERFORM S05-TESTA-PROCESS-FINNS                            
087200               ADD +1 TO VIX                                              
087300             END-PERFORM                                                  
087400             MOVE A-KOMMENTAR-ORD (AIX) TO WPROCESS-VERDE                 
087500             MOVE A-KOMMENTAR-LENGD (AIX) TO WPROCESS-LENGD               
087600             PERFORM S05-TESTA-PROCESS-FINNS                              
087700             ADD +1 TO AIX                                                
087800           ELSE IF A-KOMMENTAR-ORD (AIX) = '<' OR '>' OR                  
087900                            '<>' OR '><'                                  
088000             MOVE JA TO DEP-SIGN-LAST                                     
088100           END-IF                                                         
088200           END-IF                                                         
088300         END-PERFORM                                                      
088400       END-PERFORM                                                        
088500       IF DEP-SIGN-LAST = JA                                              
088600         IF RKOD < +8                                                     
088700           MOVE +8 TO RKOD                                                
088800         END-IF                                                           
088900         MOVE DMR-KOMMENTAR TO M308-DEP-STRING                            
089000         MOVE M308-MSG TO LIST-MSG                                        
089100         PERFORM S16-SKRIV-MSG-RAD                                        
089200         MOVE SPACE TO LIST-MSG                                           
089300       END-IF                                                             
089400       IF LIST-MSG NOT = SPACE                                            
089500         MOVE DMR-KOMMENTAR TO LIST-DIVERSE-VERDE                         
089600         PERFORM S15-SKRIV-DETALJ-RAD5                                    
089700         PERFORM S16-SKRIV-MSG-RAD                                        
089800         MOVE SPACE TO LIST-MSG                                           
089900       END-IF                                                             
090000     END-IF                                                               
090100     .                                                                    
090200     EJECT                                                                
090300 BCBA-SKAPA-ALLMAN-TAB SECTION.                                           
090400     SKIP2                                                                
090500     UNSTRING DMR-KOMMENTAR DELIMITED BY ALL SPACE                        
090600         OR '-RTN' OR '-JOB' OR ',' INTO A-KOMMENTAR-ORD (AIX)            
090700         COUNT IN A-KOMMENTAR-LENGD (AIX) WITH POINTER PEK                
090800     IF A-KOMMENTAR-LENGD (AIX) > 10                                      
090900       MOVE 10 TO A-KOMMENTAR-LENGD (AIX)                                 
091000       MOVE M300-MSG TO LIST-MSG                                          
091100       IF RKOD < +4                                                       
091200         MOVE +4 TO RKOD                                                  
091300       END-IF                                                             
091400     END-IF                                                               
091500     ADD +2 TO A-KOMMENTAR-LENGD (AIX)                                    
091600     .                                                                    
091700     EJECT                                                                
091800 BCBB-SKAPA-VANSTER-TAB SECTION.                                          
091900     SKIP2                                                                
092000     MOVE A-KOMMENTAR-ORD (AIX) TO V-KOMMENTAR-ORD (VIX)                  
092100     MOVE A-KOMMENTAR-LENGD (AIX) TO                                      
092200     V-KOMMENTAR-LENGD (VIX)                                              
092300     MOVE SPACE TO A-KOMMENTAR-ORD (AIX)                                  
092400     MOVE ZERO TO A-KOMMENTAR-LENGD (AIX)                                 
092500     .                                                                    
092600     EJECT                                                                
092700 BCBC-LAGG-UPP-BEFORE SECTION.                                            
092800     SKIP2                                                                
092900     MOVE V-KOMMENTAR-ORD (VIX) TO PROCESS-VERDE                          
093000     MOVE V-KOMMENTAR-LENGD (VIX) TO PROCESS-LENGD                        
093100     MOVE A-KOMMENTAR-ORD (AIX) TO DATA-VERDE                             
093200     MOVE A-KOMMENTAR-LENGD (AIX) TO DATA-LENGD                           
093300*                                                                         
093400     MOVE BEFORE-KONST TO LIST-TECKEN                                     
093500     MOVE PROCESS-VERDE TO LIST-VANSTER-ORD                               
093600     MOVE DATA-VERDE TO LIST-HOGER-ORD                                    
093700     PERFORM S14-SKRIV-DETALJ-RAD4                                        
093800     IF LIST-MSG NOT = SPACE                                              
093900       PERFORM S16-SKRIV-MSG-RAD                                          
094000       MOVE SPACE TO LIST-MSG                                             
094100     END-IF                                                               
094200*                                                                         
094300     CALL W980WSPC USING FTEST WSRKOD                                     
094400          PROCESS-PARM BEFORE-PARM DATA-PARM                              
094500     IF WSRKOD NOT = SPACE                                                
094600       CALL W980WSPC USING FADD WSRKOD                                    
094700            PROCESS-PARM BEFORE-PARM DATA-PARM                            
094800     END-IF                                                               
094900*                                                                         
095000     CALL W980WSPC USING FTEST WSRKOD                                     
095100          DATA-PARM AFTER-PARM PROCESS-PARM                               
095200     IF WSRKOD  NOT = SPACE                                               
095300       CALL W980WSPC USING FADD WSRKOD                                    
095400            DATA-PARM  AFTER-PARM PROCESS-PARM                            
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800 BCBD-LAGG-UPP-AFTER SECTION.                                             
095900     SKIP2                                                                
096000     MOVE V-KOMMENTAR-ORD (VIX) TO PROCESS-VERDE                          
096100     MOVE V-KOMMENTAR-LENGD (VIX) TO PROCESS-LENGD                        
096200     MOVE A-KOMMENTAR-ORD (AIX) TO DATA-VERDE                             
096300     MOVE A-KOMMENTAR-LENGD (AIX) TO DATA-LENGD                           
096400*                                                                         
096500     MOVE AFTER-KONST TO LIST-TECKEN                                      
096600     MOVE PROCESS-VERDE TO LIST-VANSTER-ORD                               
096700     MOVE DATA-VERDE TO LIST-HOGER-ORD                                    
096800     PERFORM S14-SKRIV-DETALJ-RAD4                                        
096900     IF LIST-MSG NOT = SPACE                                              
097000       PERFORM S16-SKRIV-MSG-RAD                                          
097100       MOVE SPACE TO LIST-MSG                                             
097200     END-IF                                                               
097300*                                                                         
097400     CALL W980WSPC USING FTEST WSRKOD                                     
097500          PROCESS-PARM AFTER-PARM DATA-PARM                               
097600     IF WSRKOD NOT = SPACE                                                
097700       CALL W980WSPC USING FADD WSRKOD                                    
097800            PROCESS-PARM AFTER-PARM DATA-PARM                             
097900     END-IF                                                               
098000*                                                                         
098100     CALL W980WSPC USING FTEST WSRKOD                                     
098200          DATA-PARM  BEFORE-PARM PROCESS-PARM                             
098300     IF WSRKOD NOT = SPACE                                                
098400       CALL W980WSPC USING FADD WSRKOD                                    
098500            DATA-PARM BEFORE-PARM PROCESS-PARM                            
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 BCBE-LAGG-UPP-EJ-SAMTIDIGT SECTION.                                      
099000     SKIP2                                                                
099100     MOVE V-KOMMENTAR-ORD (VIX) TO PROCESS-VERDE                          
099200     MOVE V-KOMMENTAR-LENGD (VIX) TO PROCESS-LENGD                        
099300     MOVE A-KOMMENTAR-ORD (AIX) TO DATA-VERDE                             
099400     MOVE A-KOMMENTAR-LENGD (AIX) TO DATA-LENGD                           
099500*                                                                         
099600     MOVE EJ-SAMT-KONST TO LIST-TECKEN                                    
099700     MOVE PROCESS-VERDE TO LIST-VANSTER-ORD                               
099800     MOVE DATA-VERDE TO LIST-HOGER-ORD                                    
099900     PERFORM S14-SKRIV-DETALJ-RAD4                                        
100000     IF LIST-MSG NOT = SPACE                                              
100100       PERFORM S16-SKRIV-MSG-RAD                                          
100200       MOVE SPACE TO LIST-MSG                                             
100300     END-IF                                                               
100400                                                                          
100500     CALL W980WSPC USING FTEST WSRKOD                                     
100600          PROCESS-PARM EJ-SAMTIDIGT-PARM DATA-PARM                        
100700     IF WSRKOD NOT = SPACE                                                
100800       CALL W980WSPC USING FADD WSRKOD                                    
100900            PROCESS-PARM EJ-SAMTIDIGT-PARM DATA-PARM                      
101000     END-IF                                                               
101100                                                                          
101200     CALL W980WSPC USING FTEST WSRKOD                                     
101300          DATA-PARM EJ-SAMTIDIGT-PARM PROCESS-PARM                        
101400     IF WSRKOD NOT = SPACE                                                
101500       CALL W980WSPC USING FADD WSRKOD                                    
101600            DATA-PARM EJ-SAMTIDIGT-PARM PROCESS-PARM                      
101700     END-IF                                                               
101800     .                                                                    
101900     EJECT                                                                
102000 BCC-LAGG-UPP-PRIORITET  SECTION.                                         
102100     SKIP2                                                                
102200     UNSTRING DMR-KOMMENTAR DELIMITED BY ALL SPACE                        
102300         INTO DATA-VERDE COUNT IN DATA-LENGD WITH POINTER PEK             
102400     ADD +2 TO DATA-LENGD                                                 
102500*                                                                         
102600     MOVE PRIO-KONST TO LIST-KANT-RUBRIK                                  
102700     MOVE DATA-VERDE TO LIST-DIVERSE-VERDE                                
102800     PERFORM S15-SKRIV-DETALJ-RAD5                                        
102900*                                                                         
103000     MOVE DATA-VERDE TO W-PRIO                                            
103100     IF W-PRIO NOT NUMERIC                                                
103200       IF RKOD < +8                                                       
103300         MOVE +8 TO RKOD                                                  
103400       END-IF                                                             
103500       MOVE M304-MSG TO LIST-MSG                                          
103600       PERFORM S16-SKRIV-MSG-RAD                                          
103700     ELSE                                                                 
103800       CALL W980WSPC USING FSET WSRKOD                                    
103900       PROCESS-PARM PRIO-PARM DATA-PARM                                   
104000     END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300 BCD-LAGG-UPP-VOLVO-DATA-OUTPUT  SECTION.                                 
104400     SKIP2                                                                
104500     MOVE VOUT-KONST TO LIST-KANT-RUBRIK                                  
104600                                                                          
104700     MOVE SPACE TO DATA-VERDE                                             
104800     UNSTRING DMR-KOMMENTAR DELIMITED BY ALL SPACE                        
104900     INTO DATA-VERDE COUNT IN DATA-LENGD WITH POINTER PEK                 
105000*                                                                         
105100     PERFORM UNTIL DATA-VERDE = SPACE                                     
105200       MOVE DATA-VERDE TO LIST-DIVERSE-VERDE                              
105300       PERFORM S15-SKRIV-DETALJ-RAD5                                      
105400*                                                                         
105500       IF DATA-VERDE NOT = 'COM'                                          
105600         IF RKOD < +8                                                     
105700           MOVE +8 TO RKOD                                                
105800         END-IF                                                           
105900         MOVE M305-MSG TO LIST-MSG                                        
106000         PERFORM S16-SKRIV-MSG-RAD                                        
106100       ELSE                                                               
106200         MOVE +3 TO DATA-LENGD                                            
106300         CALL W980WSPC USING FADD WSRKOD                                  
106400         PROCESS-PARM VD-OUTPUT-PARM DATA-PARM                            
106500       END-IF                                                             
106600                                                                          
106700       MOVE SPACE TO DATA-VERDE                                           
106800       UNSTRING DMR-KOMMENTAR DELIMITED BY ALL SPACE                      
106900           INTO DATA-VERDE COUNT IN DATA-LENGD WITH POINTER PEK           
107000     END-PERFORM                                                          
107100     .                                                                    
107200     EJECT                                                                
107300 BCF-LAGG-UPP-MSG        SECTION.                                         
107400     SKIP2                                                                
107500     UNSTRING DMR-KOMMENTAR                                               
107600            INTO W-MSG-DELIM WITH POINTER PEK                             
107700     MOVE SPACE TO DATA-VERDE                                             
107800     UNSTRING DMR-KOMMENTAR DELIMITED BY W-MSG-DELIM                      
107900            INTO DATA-VERDE COUNT IN DATA-LENGD                           
108000            WITH POINTER PEK                                              
108100     ADD 2 TO DATA-LENGD                                                  
108200                                                                          
108300     MOVE AMSG-KONST TO LIST-KANT-RUBRIK                                  
108400     MOVE DATA-VERDE TO LIST-DIVERSE-VERDE                                
108500     PERFORM S15-SKRIV-DETALJ-RAD5                                        
108600                                                                          
108700     CALL W980WSPC USING FSET WSRKOD                                      
108800          PROCESS-PARM ACTMSG-PARM DATA-PARM                              
108900     .                                                                    
109000     EJECT                                                                
109100 BCG-LAGG-UPP-ABEND-JOB  SECTION.                                         
109200     SKIP2                                                                
109300     MOVE W-PROCESS-NAMN TO PROCESS-VERDE                                 
109400     MOVE W-LENGD        TO PROCESS-LENGD                                 
109500                                                                          
109600     MOVE SPACE TO DATA-VERDE                                             
109700     UNSTRING DMR-KOMMENTAR DELIMITED BY SPACE                            
109800            INTO DATA-VERDE COUNT IN DATA-LENGD                           
109900            WITH POINTER PEK                                              
110000     IF DATA-LENGD > 8                                                    
110100       MOVE 8 TO DATA-LENGD                                               
110200       MOVE M309-MSG TO LIST-MSG                                          
110300       PERFORM S16-SKRIV-MSG-RAD                                          
110400       IF RKOD < +4                                                       
110500         MOVE +4 TO RKOD                                                  
110600       END-IF                                                             
110700     END-IF                                                               
110800     ADD 2 TO DATA-LENGD                                                  
110900                                                                          
111000     CALL W980WSPC USING FTEST WSRKOD                                     
111100              PROCESS-PARM ABEND-JOB-PARM DATA-PARM                       
111200     IF WSRKOD NOT = SPACE                                                
111300       CALL W980WSPC USING FADD WSRKOD                                    
111400                PROCESS-PARM ABEND-JOB-PARM DATA-PARM                     
111500     END-IF                                                               
111600                                                                          
111700     PERFORM S05-TESTA-PROCESS-FINNS                                      
111800                                                                          
111900     MOVE DATA-VERDE TO LIST-DIVERSE-VERDE                                
112000                                                                          
112100     IF LIST-DIVERSE-VERDE NOT = SPACE                                    
112200       MOVE ABEND-JOB-KONST TO LIST-KANT-RUBRIK                           
112300       PERFORM S15-SKRIV-DETALJ-RAD5                                      
112400     END-IF                                                               
112500     .                                                                    
112600     EJECT                                                                
112700 BD-LAGG-UPP-RESURSER   SECTION.                                          
112800     SKIP2                                                                
112900     MOVE W-PROCESS-NAMN TO PROCESS-VERDE                                 
113000     MOVE W-LENGD        TO PROCESS-LENGD                                 
113100                                                                          
113200     CALL W980WSPC USING FGETF WSRKOD  PROCESS-PARM                       
113300                 INFO-ATTR-PARM  INFO-DATA-PARM                           
113400                                                                          
113500     MOVE SPACE TO DATA-VERDE                                             
113600     UNSTRING DMR-RESURS DELIMITED BY SPACE                               
113700            INTO DATA-VERDE COUNT IN DATA-LENGD                           
113800     ADD 2 TO DATA-LENGD                                                  
113900     MOVE SPACE TO LIST-DIVERSE-VERDE                                     
114000     MOVE +1 TO LIST-PEK                                                  
114100                                                                          
114200     CALL W980WSPC USING FTEST WSRKOD                                     
114300          PROCESS-PARM USING-PARM DATA-PARM                               
114400                                                                          
114500     IF WSRKOD NOT = SPACE                                                
114600       CALL W980WSPC USING FADD WSRKOD                                    
114700            PROCESS-PARM USING-PARM DATA-PARM                             
114800                                                                          
114900       MOVE DATA-VERDE TO LIST-DIVERSE-VERDE                              
115000     END-IF                                                               
115100                                                                          
115200     IF LIST-DIVERSE-VERDE NOT = SPACE                                    
115300       PERFORM S15-SKRIV-DETALJ-RAD5                                      
115400       MOVE SPACE TO LIST-KANT-RUBRIK                                     
115500     END-IF                                                               
115600     .                                                                    
115700     EJECT                                                                
115800 BE-KOLLA-RESURS-KOER   SECTION.                                          
115900     SKIP2                                                                
116000     MOVE W-PROCESS-NAMN TO PROCESS-VERDE                                 
116100     MOVE W-LENGD        TO PROCESS-LENGD                                 
116200                                                                          
116300     CALL W980WSPC USING FGETF WSRKOD  PROCESS-PARM                       
116400                 INFO-ATTR-PARM  INFO-DATA-PARM                           
116500                                                                          
116600     IF KDPROCSTAT = STARTED-STATUS                                       
116700       MOVE SPACE TO DATA-VERDE                                           
116800       CALL W980WSPC USING FGETF WSRKOD  PROCESS-PARM                     
116900                   USING-PARM DATA-PARM                                   
117000                                                                          
117100       PERFORM UNTIL WSRKOD NOT = SPACE                                   
117200         CALL W980WSPC USING FTEST WSRKOD                                 
117300              DATA-PARM USED-PARM PROCESS-PARM                            
117400         IF WSRKOD NOT = SPACE                                            
117500           CALL W980WSPC USING FADD WSRKOD                                
117600                DATA-PARM USED-PARM PROCESS-PARM                          
117700         END-IF                                                           
117800         CALL W980WSPC USING FGETN WSRKOD  PROCESS-PARM                   
117900                     USING-PARM DATA-PARM                                 
118000       END-PERFORM                                                        
118100     END-IF                                                               
118200                                                                          
118300     IF KDPROCSTAT = WAITING-STATUS AND RES-WAIT = JA                     
118400* -- OM PROCESSEN VÄNTAR PÅ EN RESURS LÄGG UPP I RESURSKÖ                 
118500       MOVE SPACE TO RES-VERDE                                            
118600       CALL W980WSPC USING FGETF WSRKOD                                   
118700                PROCESS-PARM USING-PARM DATA-PARM                         
118800       PERFORM UNTIL WSRKOD NOT = SPACE OR                                
118900                  RES-VERDE NOT = SPACE                                   
119000         CALL W980WSPC USING FGETF WSRKOD                                 
119100                     DATA-PARM USED-PARM TEMP-PARM                        
119200         IF WSRKOD = SPACE                                                
119300           MOVE DATA-PARM TO RES-PARM                                     
119400         ELSE                                                             
119500           CALL W980WSPC USING FGETN WSRKOD                               
119600                       PROCESS-PARM USING-PARM DATA-PARM                  
119700         END-IF                                                           
119800       END-PERFORM                                                        
119900       IF RES-VERDE NOT = SPACE                                           
120000         CALL W980WSPC USING FTEST WSRKOD                                 
120100                     RES-PARM RESQ-PARM PROCESS-PARM                      
120200         IF WSRKOD NOT = SPACE                                            
120300           CALL W980WSPC USING FADD WSRKOD                                
120400                       RES-PARM RESQ-PARM PROCESS-PARM                    
120500         END-IF                                                           
120600       END-IF                                                             
120700     END-IF                                                               
120800     .                                                                    
120900     EJECT                                                                
121000 C-RENSA-HUVUD-PROCESS  SECTION.                                          
121100     SKIP2                                                                
121200******************************************************************        
121300***   DELETE AV GAMLA SUB-PROCESSER                            ***        
121400***   DELETE AV GAMLA KATALOG-ORD                              ***        
121500***   DELETE AV START-TYP, PRIORITET, VD-OUTPUT OCH FÄRDIGTID  ***        
121600***   DELETE AV AKTIVERINGS-MEDDELANDE                         ***        
121700***   DELETE AV RESURSER                                       ***        
121800******************************************************************        
121900                                                                          
122000     MOVE W-PROCESS-NAMN          TO PROCESS-VERDE                        
122100     MOVE W-LENGD                 TO PROCESS-LENGD                        
122200                                                                          
122300     CALL W980WSPC USING FDELK WSRKOD                                     
122400              PROCESS-PARM CATALOG-PARM                                   
122500                                                                          
122600     CALL W980WSPC USING FDELK WSRKOD                                     
122700              PROCESS-PARM NOT-CATALOG-PARM                               
122800                                                                          
122900     CALL W980WSPC USING FDELK WSRKOD                                     
123000              PROCESS-PARM START-TYP-PARM                                 
123100                                                                          
123200     CALL W980WSPC USING FDELK WSRKOD                                     
123300              PROCESS-PARM PRIO-PARM                                      
123400                                                                          
123500     CALL W980WSPC USING FDELK WSRKOD                                     
123600              PROCESS-PARM VD-OUTPUT-PARM                                 
123700                                                                          
123800     CALL W980WSPC USING FDELK WSRKOD                                     
123900              PROCESS-PARM FARDIGTID-PARM                                 
124000                                                                          
124100     CALL W980WSPC USING FDELK WSRKOD                                     
124200              PROCESS-PARM ACTMSG-PARM                                    
124300                                                                          
124400     CALL W980WSPC USING FDELK WSRKOD                                     
124500              PROCESS-PARM ABEND-JOB-PARM                                 
124600                                                                          
124700     MOVE NEJ TO RES-WAIT                                                 
124800     CALL W980WSPC USING FGETF WSRKOD  PROCESS-PARM                       
124900                 INFO-ATTR-PARM  INFO-DATA-PARM                           
125000     IF WSRKOD = SPACE                                                    
125100       IF KDPROCSTAT = WAITING-STATUS OR STARTED-STATUS                   
125200         CALL W980WSPC USING FGETF WSRKOD                                 
125300                  PROCESS-PARM USING-PARM DATA-PARM                       
125400         PERFORM UNTIL WSRKOD NOT = SPACE                                 
125500           CALL W980WSPC USING FDEL WSRKOD                                
125600                       DATA-PARM RESQ-PARM PROCESS-PARM                   
125700           IF WSRKOD = SPACE                                              
125800             MOVE JA TO RES-WAIT                                          
125900           END-IF                                                         
126000           CALL W980WSPC USING FDEL WSRKOD                                
126100                       DATA-PARM USED-PARM PROCESS-PARM                   
126200           CALL W980WSPC USING FGETN WSRKOD                               
126300                       PROCESS-PARM USING-PARM DATA-PARM                  
126400         END-PERFORM                                                      
126500       END-IF                                                             
126600     END-IF                                                               
126700     CALL W980WSPC USING FDELK WSRKOD                                     
126800              PROCESS-PARM USING-PARM                                     
126900                                                                          
127000     CALL W980WSPC USING FGETF WSRKOD                                     
127100              PROCESS-PARM CONTAINS-PARM DATA-PARM                        
127200     IF WSRKOD = SPACE                                                    
127300       PERFORM UNTIL WSRKOD NOT = SPACE                                   
127400         CALL W980WSPC USING FDEL WSRKOD                                  
127500                     DATA-PARM PARENT-PARM PROCESS-PARM                   
127600         CALL W980WSPC USING FGETN WSRKOD                                 
127700                     PROCESS-PARM CONTAINS-PARM DATA-PARM                 
127800       END-PERFORM                                                        
127900       CALL W980WSPC USING FDELK WSRKOD                                   
128000                PROCESS-PARM CONTAINS-PARM                                
128100     END-IF                                                               
128200     .                                                                    
128300     EJECT                                                                
128400 D-ALLMAN-INFO  SECTION.                                                  
128500     SKIP2                                                                
128600******************************************************************        
128700* LÄGG UPP GRUND-VÄRDEN FÖR ALLMÄN INFO OM DET INTE FINNS        *        
128800* SÅDAN UPPLAGD FÖWWUT                                           *        
128900******************************************************************        
129000                                                                          
129100     CALL W980WSPC USING FGETF WSRKOD  PROCESS-PARM                       
129200                 INFO-ATTR-PARM  INFO-DATA-PARM                           
129300     IF WSRKOD NOT = SPACE                                                
129400       MOVE PASSIVE-STATUS TO KDPROCSTAT                                  
129500       MOVE ZERO TO TIAPDAT                                               
129600                    TIEXDAT                                               
129700                    TIMINUT-START                                         
129800                    TIMINUT-STOPP                                         
129900                    TIEXEC-SENAST                                         
130000                    TIEXEC-MEDEL                                          
130100                    KDANR                                                 
130200       MOVE 25 TO INFO-LENGD                                              
130300       CALL W980WSPC USING FSET WSRKOD PROCESS-PARM                       
130400                 INFO-ATTR-PARM  INFO-DATA-PARM                           
130500     END-IF                                                               
130600     .                                                                    
130700     EJECT                                                                
130800 E-HUVUD-PROCESS-START  SECTION.                                          
130900     SKIP2                                                                
131000     MOVE DMR-PROCESS-NAMN TO W-DMR-PROCESS-NAMN                          
131100     UNSTRING DMR-PROCESS-NAMN DELIMITED BY '-RTN' OR                     
131200            '-JOB' OR SPACE                                               
131300           INTO W-PROCESS-NAMN COUNT IN W-LENGD                           
131400     MOVE SPACE TO LIST-MSG                                               
131500     IF W-LENGD > 10                                                      
131600       MOVE 10 TO W-LENGD                                                 
131700       MOVE M300-MSG TO LIST-MSG                                          
131800       IF RKOD < +4                                                       
131900         MOVE +4 TO RKOD                                                  
132000       END-IF                                                             
132100     END-IF                                                               
132200     ADD +2 TO W-LENGD                                                    
132300     MOVE W-PROCESS-NAMN TO LIST-PROCESS                                  
132400*                                                                         
132500     MOVE PROCESS-KONST TO LIST-KANT-RUBRIK                               
132600     PERFORM S10-SKRIV-DETALJ-RAD0                                        
132700     PERFORM S11-SKRIV-DETALJ-RAD1                                        
132800     IF LIST-MSG NOT = SPACE                                              
132900       PERFORM S16-SKRIV-MSG-RAD                                          
133000     END-IF                                                               
133100     .                                                                    
133200     EJECT                                                                
133300 F-TESTA-PROCESS-SAKNAS-FORTF SECTION.                                    
133400     SKIP2                                                                
133500     MOVE 1 TO SIX                                                        
133600     MOVE SPACE TO LIST-PROCESS W-DMR-PROCESS-NAMN                        
133700     PERFORM UNTIL SIX > SIX-HIGH                                         
133800       CALL W980WSPC USING FGETF WSRKOD                                   
133900       SAKNAS-PROCESS-PARM (SIX) INFO-ATTR-PARM INFO-DATA-PARM            
134000       IF WSRKOD NOT = SPACE                                              
134100         IF RKOD < +8                                                     
134200           MOVE +8 TO RKOD                                                
134300         END-IF                                                           
134400         MOVE SAKNAS-PROCESS-VERDE (SIX) TO M302-PROCESS                  
134500         MOVE M302-MSG TO LIST-MSG                                        
134600         PERFORM S16-SKRIV-MSG-RAD                                        
134700       END-IF                                                             
134800       ADD 1 TO SIX                                                       
134900     END-PERFORM                                                          
135000     .                                                                    
135100     EJECT                                                                
135200 G-MTYP-INFO  SECTION.                                                    
135300     SKIP2                                                                
135400     MOVE W-PROCESS-TYP TO MTYP-VERDE                                     
135500     CALL W980WSPC USING FSET WSRKOD                                      
135600          PROCESS-PARM MBRTYP-PARM MTYP-VERDE-PARM                        
135700     .                                                                    
135800     EJECT                                                                
135900 H-LAS-FRAM-NASTA SECTION.                                                
136000     SKIP2                                                                
136100     MOVE W-PROCESS-TYP TO M310-PROCESS-TYP                               
136200     MOVE M310-MSG TO LIST-MSG                                            
136300     PERFORM S16-SKRIV-MSG-RAD                                            
136400     IF RKOD < +4                                                         
136500       MOVE +4 TO RKOD                                                    
136600     END-IF                                                               
136700                                                                          
136800     PERFORM S01-LAS-DMRFIL                                               
136900     PERFORM UNTIL DMRFIL-EOF = JA OR                                     
137000             DMR-TKN-KOL-2-3 NOT = SPACE                                  
137100       PERFORM S01-LAS-DMRFIL                                             
137200     END-PERFORM                                                          
137300     .                                                                    
137400     EJECT                                                                
137500 Z-SLUT SECTION.                                                          
137600     SKIP2                                                                
137700     CLOSE DMRFIL W98030-001                                              
137800     CALL W980WSPC USING FCLSE WSRKOD                                     
137900     IF WSRKOD NOT = SPACE                                                
138000       DISPLAY 'SOP028S  CAN NOT CLOSE ' WSPACE-DD-NAMN                   
138100       '. RC=' WSRKOD                                                     
138200       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
138300     END-IF                                                               
138400     .                                                                    
138500     EJECT                                                                
138600 S01-LAS-DMRFIL SECTION.                                                  
138700     SKIP2                                                                
138800     READ DMRFIL INTO DMR-AREA                                            
138900       AT END  MOVE JA TO DMRFIL-EOF                                      
139000     END-READ                                                             
139100     .                                                                    
139200     EJECT                                                                
139300 S02-TESTA-UPPLAGG-PROCESS SECTION.                                       
139400     SKIP2                                                                
139500     MOVE NEJ TO LAGG-UPP-PROCESS                                         
139600     EVALUATE TRUE                                                        
139700       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'SYS'                              
139800        IF W-PROCESS-TYP = 'SYS'                                          
139900           MOVE JA  TO LAGG-UPP-PROCESS                                   
140000        END-IF                                                            
140100                                                                          
140200       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'ROU'                              
140300        IF W-PROCESS-TYP = 'SYS' OR 'ROU'                                 
140400           MOVE JA  TO LAGG-UPP-PROCESS                                   
140500        END-IF                                                            
140600                                                                          
140700       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'JOB'                              
140800        IF W-PROCESS-TYP = 'JOB' OR 'ROU' OR 'SYS'                        
140900           MOVE JA  TO LAGG-UPP-PROCESS                                   
141000        END-IF                                                            
141100                                                                          
141200       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'PRO'                              
141300        IF W-PROCESS-TYP = 'PRO' OR 'JOB' OR 'ROU' OR 'SYS'               
141400           MOVE JA  TO LAGG-UPP-PROCESS                                   
141500        END-IF                                                            
141600     END-EVALUATE                                                         
141700     .                                                                    
141800     EJECT                                                                
141900 S03-TESTA-UPPLAGG-CONTAINS SECTION.                                      
142000     SKIP2                                                                
142100     MOVE NEJ TO LAGG-UPP-CONTAINS                                        
142200     EVALUATE TRUE                                                        
142300       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'SYS'                              
142400        IF DMR-SUB-TYP-POS-1-3 = 'SYS'                                    
142500           MOVE JA TO LAGG-UPP-CONTAINS                                   
142600        END-IF                                                            
142700                                                                          
142800       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'ROU'                              
142900        IF DMR-SUB-TYP-POS-1-3 = 'SYS' OR 'ROU'                           
143000           MOVE JA  TO LAGG-UPP-CONTAINS                                  
143100        END-IF                                                            
143200                                                                          
143300       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'JOB'                              
143400        IF DMR-SUB-TYP-POS-1-3 = 'JOB' OR 'ROU' OR 'SYS'                  
143500           MOVE JA  TO LAGG-UPP-CONTAINS                                  
143600        END-IF                                                            
143700                                                                          
143800       WHEN EXEC-MEDLEMS-TYP-POS-1-3 = 'PRO'                              
143900        IF DMR-SUB-TYP-POS-1-3 = 'PRO' OR 'JOB' OR 'ROU' OR 'SYS'         
144000           MOVE JA  TO LAGG-UPP-CONTAINS                                  
144100        END-IF                                                            
144200     END-EVALUATE                                                         
144300     .                                                                    
144400     EJECT                                                                
144500 S04-NOLLSTALL-TABELLER SECTION.                                          
144600     SKIP2                                                                
144700     MOVE +1 TO AIX VIX                                                   
144800     PERFORM UNTIL AIX > AIX-MAX OR VIX > VIX-MAX                         
144900         MOVE SPACE TO A-KOMMENTAR-ORD (AIX)                              
145000         MOVE ZERO TO A-KOMMENTAR-LENGD (AIX)                             
145100         MOVE SPACE TO V-KOMMENTAR-ORD (VIX)                              
145200         MOVE ZERO TO V-KOMMENTAR-LENGD (VIX)                             
145300         ADD +1 TO AIX VIX                                                
145400     END-PERFORM                                                          
145500     .                                                                    
145600     EJECT                                                                
145700 S05-TESTA-PROCESS-FINNS SECTION.                                         
145800     SKIP2                                                                
145900***** PRELIMINÄR TEST PÅ OM EN PROCESS FINNS I DATABASEN.                 
146000***** OM DEN SAKNAS, LÄGGS DEN UPP I EN TABELL FÖR                        
146100***** EN SENARE SLUTGILTIG TEST                                           
146200                                                                          
146300     CALL W980WSPC USING FGETF WSRKOD                                     
146400            WPROCESS-PARM INFO-ATTR-PARM INFO-DATA-PARM                   
146500     IF WSRKOD NOT = SPACE                                                
146600        MOVE 1 TO SIX                                                     
146700        PERFORM UNTIL SIX > SIX-HIGH OR                                   
146800           SAKNAS-PROCESS-PARM (SIX) = WPROCESS-PARM                      
146900           ADD 1 TO SIX                                                   
147000        END-PERFORM                                                       
147100        IF SIX > SIX-HIGH                                                 
147200          ADD 1 TO SIX-HIGH                                               
147300          MOVE WPROCESS-PARM TO SAKNAS-PROCESS-PARM (SIX-HIGH)            
147400        END-IF                                                            
147500     END-IF                                                               
147600     .                                                                    
147700     EJECT                                                                
147800 S10-SKRIV-DETALJ-RAD0  SECTION.                                          
147900     SKIP2                                                                
148000     IF W001-RADRAKNARE > +35                                             
148100       MOVE +100 TO W001-RADRAKNARE                                       
148200     END-IF                                                               
148300     MOVE W-DMR-PROCESS-NAMN TO W001D0-PROCESS                            
148400     MOVE W001D0-DETALJ TO W001-RAD                                       
148500     PERFORM S20-SKRIV-DETALJ-RAD                                         
148600     .                                                                    
148700     EJECT                                                                
148800 S11-SKRIV-DETALJ-RAD1  SECTION.                                          
148900     SKIP2                                                                
149000     MOVE LIST-KANT-RUBRIK TO W001D1-KANT-RUB                             
149100     MOVE LIST-PROCESS TO W001D1-PROCESS                                  
149200     MOVE W001D1-DETALJ TO W001-RAD                                       
149300     PERFORM S20-SKRIV-DETALJ-RAD                                         
149400     .                                                                    
149500     EJECT                                                                
149600 S12-SKRIV-DETALJ-RAD2   SECTION.                                         
149700     SKIP2                                                                
149800     IF FIRST-SUB-PROCESS = JA                                            
149900       MOVE '0' TO W001D2-SKIP                                            
150000       MOVE LIST-KANT-RUBRIK TO W001D2-KANT-RUB                           
150100     ELSE                                                                 
150200       MOVE SPACE TO W001D2-SKIP  W001D2-KANT-RUB                         
150300     END-IF                                                               
150400     MOVE LIST-SUB-PROCESS TO W001D2-PROCESS                              
150500     MOVE W001D2-DETALJ TO W001-RAD                                       
150600     PERFORM S20-SKRIV-DETALJ-RAD                                         
150700     .                                                                    
150800     EJECT                                                                
150900 S13-SKRIV-DETALJ-RAD3  SECTION.                                          
151000     SKIP2                                                                
151100     MOVE LIST-KANT-RUBRIK TO W001D3-KANT-RUB                             
151200     MOVE LIST-KATALOG-ORD TO W001D3-KATALOG-ORD                          
151300     MOVE W001D3-DETALJ TO W001-RAD                                       
151400     PERFORM S20-SKRIV-DETALJ-RAD                                         
151500     .                                                                    
151600     EJECT                                                                
151700 S14-SKRIV-DETALJ-RAD4  SECTION.                                          
151800     SKIP2                                                                
151900     MOVE LIST-KANT-RUBRIK TO W001D4-KANT-RUB                             
152000     MOVE LIST-VANSTER-ORD TO W001D4-VANSTER-ORD                          
152100     MOVE LIST-TECKEN      TO W001D4-TECKEN                               
152200     MOVE LIST-HOGER-ORD   TO W001D4-HOGER-ORD                            
152300     MOVE W001D4-DETALJ TO W001-RAD                                       
152400     PERFORM S20-SKRIV-DETALJ-RAD                                         
152500     .                                                                    
152600     EJECT                                                                
152700 S15-SKRIV-DETALJ-RAD5  SECTION.                                          
152800     SKIP2                                                                
152900     MOVE LIST-KANT-RUBRIK TO W001D5-KANT-RUB                             
153000     MOVE LIST-DIVERSE-VERDE TO W001D5-DIVERSE                            
153100     MOVE W001D5-DETALJ TO W001-RAD                                       
153200     PERFORM S20-SKRIV-DETALJ-RAD                                         
153300     .                                                                    
153400     EJECT                                                                
153500 S16-SKRIV-MSG-RAD   SECTION.                                             
153600     SKIP2                                                                
153700     MOVE LIST-MSG TO W001D6-MSG                                          
153800     MOVE W001D6-MSGRAD TO W001-RAD                                       
153900     PERFORM S20-SKRIV-DETALJ-RAD                                         
154000     .                                                                    
154100     EJECT                                                                
154200 S20-SKRIV-DETALJ-RAD   SECTION.                                          
154300     SKIP2                                                                
154400     EVALUATE W001-SKIP                                                   
154500       WHEN INGET-SKIP ADD +0 TO W001-RADRAKNARE                          
154600       WHEN ENKEL-SKIP ADD +1 TO W001-RADRAKNARE                          
154700       WHEN DUBBEL-SKIP ADD +2 TO W001-RADRAKNARE                         
154800       WHEN TRIPPEL-SKIP ADD +3 TO W001-RADRAKNARE                        
154900       WHEN OTHER PERFORM S20A-SKIP-ERROR                                 
155000     END-EVALUATE                                                         
155100*                                                                         
155200     IF W001-RADRAKNARE > W001-MAX-RADER                                  
155300       PERFORM S20B-SKRIV-RUBRIK-RAD                                      
155400     END-IF                                                               
155500*                                                                         
155600     WRITE W98030-001-RAD FROM W001-RAD                                   
155700     MOVE SPACE TO W001-RAD                                               
155800     .                                                                    
155900     EJECT                                                                
156000 S20A-SKIP-ERROR   SECTION.                                               
156100     SKIP2                                                                
156200     DISPLAY 'FELAKTIGT STYRTECKEN PÅ W98030-001'                         
156300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
156400     .                                                                    
156500     EJECT                                                                
156600 S20B-SKRIV-RUBRIK-RAD  SECTION.                                          
156700     SKIP2                                                                
156800     ADD +1 TO W001-SIDRAKNARE                                            
156900     MOVE W001-SIDRAKNARE TO W001R1-SIDNR                                 
157000     WRITE W98030-001-RAD FROM W001R1-RUBRIK                              
157100                                                                          
157200     MOVE +6 TO W001-RADRAKNARE                                           
157300     MOVE TRIPPEL-SKIP TO W001-SKIP                                       
157400     .                                                                    
157500     EJECT                                                                
