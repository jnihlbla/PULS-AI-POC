000010*                                                                         
000020******************************************************************        
000030*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL1004      *        
000040******************************************************************        
000050*                                                                         
000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6124300.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   99/07/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR BINNING LIST.                                             
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- LISTFIL                                                    
002100     SELECT W61242                     ASSIGN TO W61243D1.                
002200     SKIP2                                                                
002300*          --- BINNINGLIST                                                
002400     SELECT W61243-001                 ASSIGN TO W61243D2.                
002500     EJECT                                                                
002600*          --- BINNINGLIST QUAL NOTE                                      
002700     SELECT W61243-002                 ASSIGN TO W61243D3.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W61242                                                               
003400     RECORDING       V                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W612421     -L.                                                
003800     SKIP3                                                                
003900*01  -COPY W612422     -L.                                                
004000     SKIP3                                                                
004100 FD  W61243-001                                                           
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500 01  W61243-001-RAD              PIC X(121).                              
004600     EJECT                                                                
004700 FD  W61243-002                                                           
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000     SKIP2                                                                
005100 01  W61243-002-RAD              PIC X(121).                              
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500                                                                          
005600*    -- CHECKED BY WY2000                                                 
005700 77  IDPGM                       PIC X(8)    VALUE 'W6124300'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
005910 77  WS-TABELL-UPPD              PIC X       VALUE 'N'.                   
006000 77  WS-TOT-DETALJRAD            PIC S9(5)   VALUE ZERO COMP-3.           
006100 77  WS-TOT-ART                  PIC S9(5)   VALUE ZERO COMP-3.           
006200 77  WS-TOT-VKART                PIC S9(11)V9(2)                          
006210                                             VALUE ZERO COMP-3.           
006220 77  WS-VKART                    PIC S9(11)V9(2)                          
006230                                             VALUE ZERO COMP-3.           
006300 77  WS-TOT-VLARTNTO             PIC S9(11)V9(2)                          
006301                                             VALUE ZERO COMP-3.           
006302 77  WS-VLARTNTO                 PIC S9(11)V9(2)                          
006303                                             VALUE ZERO COMP-3.           
006304 77  WS-SUM-VKART                PIC S9(11)  VALUE ZERO COMP-3.           
006305 77  WS-SUM-VLARTNTO             PIC S9(11)  VALUE ZERO COMP-3.           
006310 77  WS-ANTAL-RADER-KVAR         PIC S9(5)   VALUE ZERO COMP-3.           
006500 77  WS-CMD                      PIC X(3)    VALUE SPACE.                 
006510 77  WS-KDMATT                   PIC X       VALUE SPACE.                 
006600 77  TAB-IX                      PIC S9(5)   VALUE ZERO COMP-3.           
006700 77  TAB-IX-MAX                  PIC S9(5)   VALUE 1100 COMP-3.           
006710 77  QUAL-IX                     PIC S9(5)   VALUE ZERO COMP-3.           
006720 77  QUAL-IX-MAX                 PIC S9(5)   VALUE 1100 COMP-3.           
006800                                                                          
006900 77  W61242-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  END-OF-W61242                       VALUE 'J'.                   
007100                                                                          
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES DAGENS-DATUM.                                       
007400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007700                                                                          
007800 01  TABENTRY-PARM.                                                       
007900     03  STEGLAANGD              PIC S9(9) COMP.                          
008000     03  ANTAL                   PIC S9(9) COMP.                          
008100     03  NYCKELLAANGD            PIC S9(9) COMP.                          
008200     EJECT                                                                
008300 01  SORT-TABELL.                                                         
008400     03  TAB-RAD OCCURS 1100.                                             
008500        05  TAB-SORT1-BEGREPP.                                            
008600            07  TAB-IDARTNR     PIC S9(9) COMP-3.                         
008700        05  TAB-SORT2-BEGREPP.                                            
008800            07  TAB-ADLAGOMR    PIC S9(3) COMP-3.                         
008900            07  TAB-ADGANG      PIC S9(3) COMP-3.                         
009000            07  TAB-ADPLATS     PIC S9(5) COMP-3.                         
009100        05  TAB-BEART           PIC X(25).                                
009200        05  TAB-DAREGDAT        PIC 9(8).                                 
009300        05  TAB-FLKDFARLIG      PIC X.                                    
009400        05  TAB-FLBACKORDER     PIC X.                                    
009500        05  TAB-FLPRIO          PIC X.                                    
009600        05  TAB-KDARTURS        PIC X(2).                                 
009700        05  TAB-KVAVIS          PIC S9(7) COMP-3.                         
010000     EJECT                                                                
010010 01  QUAL-TABELL.                                                         
010020     03  QUAL-RAD OCCURS 1100.                                            
010090        05  QUAL-IDARTNR        PIC S9(9).                                
010091        05  QUAL-BEART          PIC X(25).                                
010092        05  QUAL-DAREGDAT       PIC 9(8).                                 
010093        05  QUAL-IDPERSON-BUY   PIC S9(3).                                
010094        05  QUAL-TEKVAINF-1     PIC X(79).                                
010095        05  QUAL-TEKVAINF-2     PIC X(79).                                
010096        05  QUAL-TEKVAINF-3     PIC X(79).                                
010097        05  QUAL-TEKVAINF-4     PIC X(79).                                
010098        05  QUAL-TEKVAINF-5     PIC X(79).                                
010099        05  QUAL-TEKVAINF-6     PIC X(79).                                
010100        05  QUAL-TEKVAINF-7     PIC X(79).                                
010101     EJECT                                                                
010110 01  DYNAMISKA-SUBPROGRAM.                                                
010200*                                                                         
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
010600     SKIP2                                                                
010700*    --- PARAMETRAR TILL ABEND                                            
010800                                                                          
010900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011100     SKIP2                                                                
011200 01  FELTEXT.                                                             
011300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011500     EJECT                                                                
011600*      --- VALID IDDC CODES                                               
011700*                                                                         
011800*01    -COPY WWDC99                                                       
011900       EJECT                                                              
012000*    --- PARAMETRAR TILL WWOMVAND                                         
012100*                                                                         
012200*01  -COPY WWOMVAND                                                       
012300     EJECT                                                                
012400*    --- PARAMETRAR TILL POSTSUM                                          
012500*                                                                         
012600*01  -COPY W0005   -PRE  POSTSUM-                                         
012700     EJECT                                                                
012800 01  IN-AREA-START               PIC X(246)  VALUE                        
012900                                 'IN-AREA-START  '.                       
013000 01  IN-AREA.                                                             
013100     03  IN-IDPTYP               PIC X(3).                                
013200     03  IN-RESTEN               PIC X(618).                              
013300 01  PTYP1-AREA REDEFINES IN-AREA.                                        
013400*    03  -COPY W612421 -PRE IN01-                                         
013500     EJECT                                                                
013600 01  PTYP2-AREA REDEFINES IN-AREA.                                        
013700*    03  -COPY W612422 -PRE IN02-                                         
013800     EJECT                                                                
013900 01  W001-AREA-START             PIC X(24)   VALUE                        
014000                                 'W001-AREA-START  '.                     
014100     SKIP2                                                                
014200 01  W001-HJALPAREOR.                                                     
014300*                                                                         
014400     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
014500     03  W001-ANTAL-RADER                                                 
014600                                 PIC 9(3)    VALUE 999.                   
014700     03  W001-MAX-RADER-PER-SIDA                                          
014800                                 PIC 9(3)    VALUE 42.                    
014900     03  W001-MAX-POSITIONER-PER-RAD                                      
015000                                 PIC 9(3)    VALUE 120.                   
015100     03  W001-LISTNR             PIC X(11)   VALUE 'W61243-001'.          
015200     03  W001-LISTID             PIC X(11)   VALUE 'W61243-001'.          
015300     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
015400     EJECT                                                                
015500 01  W001-RAD.                                                            
015600     03  FILLER                  PIC X(121)  VALUE SPACE.                 
015700     EJECT                                                                
015800 01  FILLER                      PIC X(118)                               
015900                                      VALUE ' RUBRIKRAD-1 '.              
016000 01  W001-RUBRIK-1.                                                       
016100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
016200     03  FILLER                  PIC X(12)                                
016300                                VALUE 'BINNINGLIST '.                     
016400*    03  FILLER                  PIC X(93) VALUE SPACE.                   
016410     03  FILLER                  PIC X(10) VALUE SPACE.                   
016420     03  FILLER                  PIC X(5)  VALUE 'VOLVO'.                 
016430     03  FILLER                  PIC X(78) VALUE SPACE.                   
016500     03  W001-SID                PIC Z(4)9.                               
016600     03  FILLER                  PIC X(7)  VALUE SPACE.                   
016700     EJECT                                                                
016800 01  FILLER                      PIC X(118)                               
016900                                      VALUE ' RUBRIKRAD-2'.               
017000*01  W001-RUBRIK-2.                                                       
017100***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
017200***  03  FILLER                  PIC X(12) VALUE 'INVOICE NO. '.          
017300***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
017400***  03  W001-IDFAKT             PIC Z(6)9 VALUE SPACE.                   
017500***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
017600***  03  FILLER                  PIC X(9)  VALUE 'CASE NO. '.             
017700***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
017800***  03  W001-IDKOLLI            PIC Z(4)9 VALUE SPACE.                   
017900***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
018000***  03  FILLER                  PIC X(9)  VALUE 'DISTRICT '.             
018100***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
018200***  03  W001-IDDISTR            PIC Z(3)9 VALUE SPACE.                   
018300***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
018400***  03  FILLER                  PIC X(6)  VALUE 'ORDER '.                
018500***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
018600***  03  W001-IDKUNDRF           PIC X(6)  VALUE SPACE.                   
018700***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
018800***  03  FILLER                  PIC X(9)  VALUE 'CUSTOMER '.             
018900***  03  W001-IDKUNDNR           PIC Z(5)9 VALUE SPACE.                   
019000***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
019100***  03  FILLER                  PIC X(4)  VALUE 'ETA '.                  
019200***  03  W001-DABERANK           PIC Z(6)  VALUE SPACE.                   
019300***  03  FILLER                  PIC X(7)  VALUE SPACE.                   
019301                                                                          
019310 01  W001-RUBRIK-2.                                                       
019320     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019321     03  FILLER                  PIC X(9)  VALUE 'DISTRICT '.             
019322     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019323     03  W001-IDDISTR            PIC Z(3)9 VALUE SPACE.                   
019324     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019330     03  FILLER                  PIC X(12) VALUE 'INVOICE NO. '.          
019340     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019350     03  W001-IDFAKT             PIC Z(6)9 VALUE SPACE.                   
019360     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019361     03  FILLER                  PIC X(6)  VALUE 'ORDER '.                
019362     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019363     03  W001-IDKUNDRF           PIC X(6)  VALUE SPACE.                   
019364     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019365     03  FILLER                  PIC X(9)  VALUE 'CUSTOMER '.             
019366     03  W001-IDKUNDNR           PIC Z(5)9 VALUE SPACE.                   
019367     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019370     03  FILLER                  PIC X(9)  VALUE 'CASE NO. '.             
019380     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019390     03  W001-IDKOLLI            PIC Z(4)9 VALUE SPACE.                   
019391     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019402     03  FILLER                  PIC X(4)  VALUE 'ETA '.                  
019404     03  W001-DABERANK           PIC Z(6)  VALUE SPACE.                   
019405     03  FILLER                  PIC X(7)  VALUE SPACE.                   
019410     EJECT                                                                
019500 01  FILLER                      PIC X(118)                               
019600                                      VALUE ' RUBRIKRAD3 '.               
019700 01  W001-RUBRIK-3.                                                       
019800     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019900     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
020000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
020100     03  FILLER                  PIC X(4)  VALUE 'B.O.'.                  
020200     03  FILLER                  PIC X(1)  VALUE SPACE.                   
020300     03  FILLER                  PIC X(3)  VALUE 'NEW'.                   
020400     03  FILLER                  PIC X(1)  VALUE SPACE.                   
020500     03  FILLER                  PIC X(8)  VALUE 'PART NO.'.              
020600     03  FILLER                  PIC X(2)  VALUE SPACE.                   
020700     03  FILLER                  PIC X(11) VALUE 'DESCRIPTION'.           
020800     03  FILLER                  PIC X(8)  VALUE SPACE.                   
020900     03  FILLER                  PIC X(11) VALUE 'DC LOCATION'.           
021000     03  FILLER                  PIC X(5)  VALUE SPACE.                   
021100     03  FILLER                  PIC X(3)  VALUE 'QTY'.                   
021200     03  FILLER                  PIC X(24) VALUE SPACE.                   
021300     03  FILLER                  PIC X(1)  VALUE 'H'.                     
021400     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021500     03  FILLER                  PIC X(6)  VALUE 'ORIGIN'.                
021600     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021700     03  FILLER                  PIC X(9)  VALUE 'QUAL NOTE'.             
021800     03  FILLER                  PIC X(11) VALUE SPACE.                   
021900     EJECT                                                                
022000 01  W001-DETALJ1.                                                        
022100     03  FILLER                  PIC X(4)  VALUE SPACE.                   
022200     03  W001-FLPRIO             PIC X(1)  VALUE SPACE.                   
022300     03  FILLER                  PIC X(4)  VALUE SPACE.                   
022400     03  W001-FLBACKORDER        PIC X(1)  VALUE SPACE.                   
022500     03  FILLER                  PIC X(4)  VALUE SPACE.                   
022600     03  W001-NEW                PIC X(1)  VALUE SPACE.                   
022700     03  FILLER                  PIC X(1)  VALUE SPACE.                   
022800     03  W001-IDARTNR            PIC Z(8)9 VALUE ZERO.                    
022900     03  FILLER                  PIC X(2)  VALUE SPACE.                   
023000     03  W001-BEART              PIC X(18) VALUE SPACE.                   
023100     03  FILLER                  PIC X(1)  VALUE SPACE.                   
023200     03  W001-ADLAGOMR           PIC ZZ    VALUE SPACE.                   
023300     03  FILLER                  PIC X     VALUE SPACE.                   
023400     03  W001-ADGANG             PIC ZZ    VALUE SPACE.                   
023500     03  FILLER                  PIC X     VALUE SPACE.                   
023600     03  W001-ADPLATS            PIC Z(5)  VALUE SPACE.                   
023700     03  FILLER                  PIC X(1)  VALUE SPACE.                   
023800     03  W001-KVAVIS             PIC Z(6)9 VALUE ZERO.                    
023900     03  FILLER                  PIC X(1)  VALUE SPACE.                   
024000     03  FILLER                  PIC X(22)                                
024100                          VALUE '......................'.                 
024200     03  FILLER                  PIC X(1)  VALUE SPACE.                   
024300     03  W001-FLKDFARLIG         PIC X(1)  VALUE SPACE.                   
024400     03  FILLER                  PIC X(2)  VALUE SPACE.                   
024500     03  W001-KDARTURS           PIC X(2)  VALUE SPACE.                   
024600     03  FILLER                  PIC X(6)  VALUE SPACE.                   
024700     03  W001-FLTEKVALNOT        PIC X(1)  VALUE SPACE.                   
024800     03  FILLER                  PIC X(19) VALUE SPACE.                   
024900     EJECT                                                                
025000 01  W001-SLUTRAD.                                                        
025100     03  FILLER                  PIC X(3)   VALUE SPACE.                  
025200     03  FILLER                  PIC X(8)   VALUE 'VOLUME: '.             
025300     03  FILLER                  PIC X(3)   VALUE SPACE.                  
025400     03  W001-TOT-VLARTNTO       PIC Z(10)9 VALUE SPACE.                  
025500     03  FILLER                  PIC X(3)   VALUE SPACE.                  
025600     03  W001-KDSORT-VOLYM       PIC X(8)   VALUE SPACE.                  
025700     03  FILLER                  PIC X(8)   VALUE 'WEIGHT: '.             
025800     03  FILLER                  PIC X(3)   VALUE SPACE.                  
025900     03  W001-TOT-VKART          PIC Z(10)9 VALUE SPACE.                  
026000     03  FILLER                  PIC X(3)   VALUE SPACE.                  
026100     03  W001-KDSORT-VIKT        PIC X(10)  VALUE SPACE.                  
026200     03  FILLER                  PIC X(17)                                
026300                                    VALUE 'NUMBER OF PARTS: '.            
026400     03  FILLER                  PIC X(3)   VALUE SPACE.                  
026500     03  W001-TOT-ART            PIC Z(5)9  VALUE SPACE.                  
026600     03  FILLER                  PIC X(23)  VALUE SPACE.                  
026700     EJECT                                                                
026800 01  W002-AREA-START             PIC X(24)   VALUE                        
026900                                 'W002-AREA-START  '.                     
027000     SKIP2                                                                
027100 01  W002-HJALPAREOR.                                                     
027200*                                                                         
027300     03  W002-SKIP               PIC 9(3) COMP-3  VALUE 1.                
027400     03  W002-ANTAL-RADER                                                 
027500                                 PIC 9(3)    VALUE 999.                   
027600     03  W002-MAX-RADER-PER-SIDA                                          
027700                                 PIC 9(3)    VALUE 42.                    
027800     03  W002-MAX-POSITIONER-PER-RAD                                      
027900                                 PIC 9(3)    VALUE 120.                   
028000     03  W002-LISTNR             PIC X(11)   VALUE 'W61243-002'.          
028100     03  W002-LISTID             PIC X(11)   VALUE 'W61243-002'.          
028200     03  W002-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
028300     EJECT                                                                
028400 01  W002-RAD.                                                            
028500     03  FILLER                  PIC X(121)  VALUE SPACE.                 
028600     EJECT                                                                
028700 01  FILLER                      PIC X(118)                               
028800                                      VALUE ' RUBRIKRAD-1'.               
028900 01  W002-RUBRIK-1.                                                       
029000     03  FILLER                  PIC X(3)   VALUE SPACE.                  
029100     03  FILLER                  PIC X(12)                                
029200                                VALUE 'BINNINGLIST '.                     
029300*    03  FILLER                  PIC X(105) VALUE SPACE.                  
029400     03  FILLER                  PIC X(10)  VALUE SPACE.                  
029410     03  FILLER                  PIC X(5)   VALUE 'VOLVO'.                
029420     03  FILLER                  PIC X(90)  VALUE SPACE.                  
029500     EJECT                                                                
029600 01  FILLER                      PIC X(118)                               
029700                                      VALUE ' RUBRIKRAD-2'.               
029800*01  W002-RUBRIK-2.                                                       
029900***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
030000***  03  FILLER                  PIC X(11) VALUE 'INVOICE NO.'.           
030100***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
030200***  03  W002-IDFAKT             PIC Z(6)9 VALUE SPACE.                   
030300***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
030400***  03  FILLER                  PIC X(8)  VALUE 'CASE NO.'.              
030500***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
030600***  03  W002-IDKOLLI            PIC Z(4)9 VALUE SPACE.                   
030700***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
030800***  03  FILLER                  PIC X(8)  VALUE 'DISTRICT'.              
030900***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
031000***  03  W002-IDDISTR            PIC Z(3)9 VALUE SPACE.                   
031100***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
031200***  03  FILLER                  PIC X(5)  VALUE 'ORDER'.                 
031300***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
031400***  03  W002-IDKUNDRF           PIC X(6)  VALUE SPACE.                   
031500***  03  FILLER                  PIC X(3)  VALUE SPACE.                   
031600***  03  FILLER                  PIC X(8)  VALUE 'CUSTOMER'.              
031700***  03  W002-IDKUNDNR           PIC Z(5)9 VALUE SPACE.                   
031800***  03  FILLER                  PIC X(25) VALUE SPACE.                   
031900**   EJECT                                                                
031910 01  W002-RUBRIK-2.                                                       
031920     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031930     03  FILLER                  PIC X(9)  VALUE 'DISTRICT '.             
031940     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031950     03  W002-IDDISTR            PIC Z(3)9 VALUE SPACE.                   
031960     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031970     03  FILLER                  PIC X(12) VALUE 'INVOICE NO. '.          
031980     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031990     03  W002-IDFAKT             PIC Z(6)9 VALUE SPACE.                   
031991     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031992     03  FILLER                  PIC X(6)  VALUE 'ORDER '.                
031993     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031994     03  W002-IDKUNDRF           PIC X(6)  VALUE SPACE.                   
031995     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031996     03  FILLER                  PIC X(9)  VALUE 'CUSTOMER '.             
031997     03  W002-IDKUNDNR           PIC Z(5)9 VALUE SPACE.                   
031998     03  FILLER                  PIC X(3)  VALUE SPACE.                   
031999     03  FILLER                  PIC X(9)  VALUE 'CASE NO. '.             
032000     03  FILLER                  PIC X(3)  VALUE SPACE.                   
032001     03  W002-IDKOLLI            PIC Z(4)9 VALUE SPACE.                   
032002     03  FILLER                  PIC X(20) VALUE SPACE.                   
032006     EJECT                                                                
032010 01  FILLER                      PIC X(118)                               
032100                                      VALUE ' RUBRIKRAD3 '.               
032200 01  W002-RUBRIK-3.                                                       
032300     03  FILLER                  PIC X(3)  VALUE SPACE.                   
032400     03  FILLER                  PIC X(8)  VALUE 'PART NO.'.              
032500     03  FILLER                  PIC X(3)  VALUE SPACE.                   
032600     03  W002-IDARTNR            PIC Z(8)9 VALUE SPACE.                   
032700     03  FILLER                  PIC X(3)  VALUE SPACE.                   
032800     03  FILLER                  PIC X(11) VALUE 'DESCRIPTION'.           
032900     03  FILLER                  PIC X(3)  VALUE SPACE.                   
033000     03  W002-BEART              PIC X(18) VALUE SPACE.                   
033100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
033200     03  FILLER                  PIC X(8)  VALUE 'REG DATE'.              
033300     03  FILLER                  PIC X(3)  VALUE SPACE.                   
033400     03  W002-REGDAT             PIC X(6)  VALUE SPACE.                   
033500     03  FILLER                  PIC X(3)  VALUE SPACE.                   
033600     03  FILLER                  PIC X(8)  VALUE 'RESP. TD'.              
033700     03  FILLER                  PIC X(3)  VALUE SPACE.                   
033800     03  W002-IDPERSON-BUY       PIC Z(2)9 VALUE SPACE.                   
033900     03  FILLER                  PIC X(25) VALUE SPACE.                   
034000     EJECT                                                                
034100 01  W002-DETALJ1.                                                        
034200     03  FILLER                  PIC X(3)  VALUE SPACE.                   
034300     03  W002-TEKVAINF-1         PIC X(79) VALUE SPACE.                   
034400     03  FILLER                  PIC X(38) VALUE SPACE.                   
034500                                                                          
034600 01  W002-DETALJ2.                                                        
034700     03  FILLER                  PIC X(3)  VALUE SPACE.                   
034900     03  W002-TEKVAINF-2         PIC X(79) VALUE SPACE.                   
035000     03  FILLER                  PIC X(38) VALUE SPACE.                   
035010                                                                          
035100 01  W002-DETALJ3.                                                        
035110     03  FILLER                  PIC X(3)  VALUE SPACE.                   
035200     03  W002-TEKVAINF-3         PIC X(79) VALUE SPACE.                   
035300     03  FILLER                  PIC X(38) VALUE SPACE.                   
035400                                                                          
035410 01  W002-DETALJ4.                                                        
035420     03  FILLER                  PIC X(3)  VALUE SPACE.                   
035430     03  W002-TEKVAINF-4         PIC X(79) VALUE SPACE.                   
035440     03  FILLER                  PIC X(38) VALUE SPACE.                   
035441                                                                          
035450 01  W002-DETALJ5.                                                        
035460     03  FILLER                  PIC X(3)  VALUE SPACE.                   
035470     03  W002-TEKVAINF-5         PIC X(79) VALUE SPACE.                   
035480     03  FILLER                  PIC X(38) VALUE SPACE.                   
035481                                                                          
035490 01  W002-DETALJ6.                                                        
035491     03  FILLER                  PIC X(3)  VALUE SPACE.                   
035492     03  W002-TEKVAINF-6         PIC X(79) VALUE SPACE.                   
035493     03  FILLER                  PIC X(38) VALUE SPACE.                   
035494                                                                          
035495 01  W002-DETALJ7.                                                        
035496     03  FILLER                  PIC X(3)  VALUE SPACE.                   
035497     03  W002-TEKVAINF-7         PIC X(79) VALUE SPACE.                   
035498     03  FILLER                  PIC X(38) VALUE SPACE.                   
035500     EJECT                                                                
035600 PROCEDURE DIVISION.                                                      
035700 MAIN SECTION.                                                            
035800                                                                          
035900     PERFORM A-INIT                                                       
036000                                                                          
036100     PERFORM S01-LAES-W61242                                              
036200     PERFORM UNTIL END-OF-W61242                                          
036300        IF IN-IDPTYP = '001'                                              
036310           IF WS-TABELL-UPPD = JA                                         
036320              PERFORM D-SORT-SKRIV-LISTA                                  
036321              MOVE +1 TO QUAL-IX                                          
036323              PERFORM E-BEHANDLA-SKRIV-LISTA-QUALNOT                      
036326           END-IF                                                         
036327           PERFORM S10-NOLLSTALL-TABELL                                   
036328           MOVE ZERO TO WS-TOT-ART                                        
036329                        WS-TOT-VKART                                      
036330                        WS-TOT-VLARTNTO                                   
036331                        WS-TOT-DETALJRAD                                  
036332                        WS-ANTAL-RADER-KVAR                               
036333                        TAB-IX                                            
036334                        QUAL-IX                                           
036335                        W001-SIDRAKNARE                                   
036336                        MOVE 1100 TO TAB-IX-MAX                           
036340           MOVE NEJ TO WS-TABELL-UPPD                                     
036400           PERFORM B-BEHANDLA-PTYP01                                      
036500        ELSE                                                              
036600           ADD +1 TO TAB-IX                                               
036610                     QUAL-IX                                              
036700           PERFORM C-LAS-TILL-TABELL                                      
036701           PERFORM X-LAS-TILL-QUAL-TAB                                    
036702           MOVE ZERO             TO WS-VKART                              
036703                                    WS-VLARTNTO                           
036704           COMPUTE WS-VKART    = IN02-KVAVIS * IN02-VKART                 
036705           ADD WS-VKART          TO WS-TOT-VKART                          
036706           COMPUTE WS-VLARTNTO = IN02-KVAVIS * IN02-VLARTNTO              
036720           ADD WS-VLARTNTO       TO WS-TOT-VLARTNTO                       
036730           ADD +1                TO WS-TOT-ART                            
036800        END-IF                                                            
037300        PERFORM S01-LAES-W61242                                           
037400     END-PERFORM                                                          
037500                                                                          
037510     IF WS-TABELL-UPPD = JA                                               
037520        PERFORM D-SORT-SKRIV-LISTA                                        
037521        MOVE +1 TO QUAL-IX                                                
037522        PERFORM E-BEHANDLA-SKRIV-LISTA-QUALNOT                            
037530     END-IF                                                               
037700                                                                          
037800     PERFORM Z-FINIT                                                      
037900                                                                          
038000     MOVE ZERO TO RETURN-CODE                                             
038100     GOBACK                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 A-INIT SECTION.                                                          
038500                                                                          
038600     OPEN INPUT W61242                                                    
038610     OPEN OUTPUT W61243-001                                               
038620                 W61243-002                                               
038700                                                                          
039000     ACCEPT DAGENS-DATUM FROM DATE                                        
039100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039200     PERFORM S10-NOLLSTALL-TABELL                                         
039300     MOVE ZERO TO WS-TOT-ART                                              
039400                  WS-TOT-VKART                                            
039500                  WS-TOT-VLARTNTO                                         
039600                  WS-TOT-DETALJRAD                                        
039610                  WS-ANTAL-RADER-KVAR                                     
039700                  TAB-IX                                                  
039710     MOVE NEJ  TO WS-TABELL-UPPD                                          
039800     .                                                                    
039900     EJECT                                                                
042100 B-BEHANDLA-PTYP01 SECTION.                                               
042200                                                                          
042300     IF IN-IDPTYP = '001'                                                 
042400        MOVE IN01-IDFAKT        TO W001-IDFAKT                            
042500                                   W002-IDFAKT                            
042600        MOVE IN01-IDKOLLI       TO W001-IDKOLLI                           
042700                                   W002-IDKOLLI                           
042800        MOVE IN01-IDDISTR       TO W001-IDDISTR                           
042900                                   W002-IDDISTR                           
043000        MOVE IN01-IDKUNDRF      TO W001-IDKUNDRF                          
043100                                   W002-IDKUNDRF                          
043200        MOVE IN01-IDKUNDNR      TO W001-IDKUNDNR                          
043300                                   W002-IDKUNDNR                          
043400        MOVE IN01-DABERANK(3:6) TO W001-DABERANK                          
043500                                                                          
043600        MOVE IN01-IDDC          TO WS-IDDC                                
043610        IF NDC                                                            
043620           CONTINUE                                                       
043630        ELSE                                                              
043640           MOVE ZERO            TO W001-DABERANK                          
043650        END-IF                                                            
043700        MOVE IN01-CMD           TO WS-CMD                                 
043710        MOVE IN01-KDMATT        TO WS-KDMATT                              
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 C-LAS-TILL-TABELL SECTION.                                               
044200                                                                          
044300     MOVE IN02-IDARTNR       TO TAB-IDARTNR(TAB-IX)                       
044400     MOVE IN02-ADLAGOMR      TO TAB-ADLAGOMR(TAB-IX)                      
044500     MOVE IN02-ADGANG        TO TAB-ADGANG(TAB-IX)                        
044600     MOVE IN02-ADPLATS       TO TAB-ADPLATS(TAB-IX)                       
044700     MOVE IN02-FLPRIO        TO TAB-FLPRIO(TAB-IX)                        
044800     MOVE IN02-BEART         TO TAB-BEART(TAB-IX)                         
044900     MOVE IN02-FLBACKORDER   TO TAB-FLBACKORDER(TAB-IX)                   
045000     MOVE IN02-FLKDFARLIG    TO TAB-FLKDFARLIG(TAB-IX)                    
045100     MOVE IN02-KVAVIS        TO TAB-KVAVIS(TAB-IX)                        
045200     MOVE IN02-KDARTURS      TO TAB-KDARTURS(TAB-IX)                      
045210     MOVE IN02-DAREGDAT      TO TAB-DAREGDAT(TAB-IX)                      
045220     MOVE JA TO WS-TABELL-UPPD                                            
045300     .                                                                    
045400     EJECT                                                                
045510 D-SORT-SKRIV-LISTA SECTION.                                              
045600                                                                          
045700     MOVE WS-TOT-ART         TO W001-TOT-ART                              
045800     IF WS-KDMATT = 'U'                                                   
045900        COMPUTE WS-SUM-VKART ROUNDED                                      
046000                = WS-TOT-VKART * CONV-GR-TO-LB                            
046100        END-COMPUTE                                                       
046210        COMPUTE WS-SUM-VLARTNTO ROUNDED                                   
046220                = WS-TOT-VLARTNTO / 1000000 * CONV-M3-TO-FT3              
046230        END-COMPUTE                                                       
046240        MOVE 'LB        '    TO W001-KDSORT-VIKT                          
046250        MOVE 'FT3     '      TO W001-KDSORT-VOLYM                         
046300     ELSE                                                                 
046400        COMPUTE WS-SUM-VKART ROUNDED = WS-TOT-VKART / 1000                
046500        END-COMPUTE                                                       
046510        COMPUTE WS-SUM-VLARTNTO ROUNDED                                   
046520                = WS-TOT-VLARTNTO / 1000000                               
046530        END-COMPUTE                                                       
046600        MOVE 'KG        '    TO W001-KDSORT-VIKT                          
046610        MOVE 'M3      '      TO W001-KDSORT-VOLYM                         
046700     END-IF                                                               
046701     IF WS-SUM-VKART = ZERO                                               
046702        MOVE 1               TO W001-TOT-VKART                            
046703     ELSE                                                                 
046710        MOVE WS-SUM-VKART    TO W001-TOT-VKART                            
046711     END-IF                                                               
046712     IF WS-SUM-VLARTNTO = ZERO                                            
046713        MOVE 1               TO W001-TOT-VLARTNTO                         
046714     ELSE                                                                 
046720        MOVE WS-SUM-VLARTNTO TO W001-TOT-VLARTNTO                         
046730     END-IF                                                               
046740                                                                          
047000     IF WS-CMD = 'PR '                                                    
047100        MOVE +54 TO STEGLAANGD                                            
047200        MOVE TAB-IX TO ANTAL                                              
047400        MOVE +5 TO NYCKELLAANGD                                           
047500                                                                          
047600        CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                   
047700        TAB-SORT1-BEGREPP(1) NYCKELLAANGD                                 
047800     ELSE                                                                 
047900        MOVE +54 TO STEGLAANGD                                            
048000        MOVE TAB-IX TO ANTAL                                              
048200        MOVE +7 TO NYCKELLAANGD                                           
048300                                                                          
048400        CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                   
048500        TAB-SORT2-BEGREPP(1) NYCKELLAANGD                                 
048600     END-IF                                                               
048700                                                                          
048800     MOVE ANTAL TO TAB-IX-MAX                                             
048900     MOVE +1 TO TAB-IX                                                    
049000                                                                          
049100     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
049110        IF TAB-FLPRIO(TAB-IX) = 'J' OR 'Y'                                
049200           MOVE 'Y'                  TO W001-FLPRIO                       
049210        ELSE                                                              
049220           MOVE SPACE                TO W001-FLPRIO                       
049230        END-IF                                                            
049300        MOVE TAB-KVAVIS(TAB-IX)      TO W001-KVAVIS                       
049400        MOVE TAB-IDARTNR(TAB-IX)     TO W001-IDARTNR                      
049410        IF TAB-FLBACKORDER(TAB-IX) = 'J' OR 'Y'                           
049420           MOVE 'Y'                  TO W001-FLBACKORDER                  
049430        ELSE                                                              
049500           MOVE SPACE                TO W001-FLBACKORDER                  
049510        END-IF                                                            
049700        IF TAB-ADLAGOMR(TAB-IX) = ZERO                                    
049800           MOVE 'Y'                  TO W001-NEW                          
049900        ELSE                                                              
049901           MOVE SPACE                TO W001-NEW                          
049910        END-IF                                                            
050000        MOVE TAB-ADLAGOMR(TAB-IX) TO W001-ADLAGOMR                        
050100        MOVE TAB-ADGANG(TAB-IX)   TO W001-ADGANG                          
050200        MOVE TAB-ADPLATS(TAB-IX)  TO W001-ADPLATS                         
050400                                                                          
050500        MOVE TAB-KDARTURS(TAB-IX)    TO W001-KDARTURS                     
050600        MOVE TAB-FLKDFARLIG(TAB-IX)  TO W001-FLKDFARLIG                   
050700        MOVE TAB-BEART(TAB-IX)       TO W001-BEART                        
050800        IF TAB-DAREGDAT(TAB-IX) = ZERO                                    
050900           MOVE SPACE                TO W001-FLTEKVALNOT                  
051000        ELSE                                                              
051100           MOVE 'Y'                  TO W001-FLTEKVALNOT                  
051200        END-IF                                                            
051300                                                                          
051400        PERFORM S21-SKRIV-W61243-001                                      
051410        ADD +1 TO TAB-IX                                                  
051500     END-PERFORM                                                          
051600                                                                          
051700     .                                                                    
051800     EJECT                                                                
051900 E-BEHANDLA-SKRIV-LISTA-QUALNOT SECTION.                                  
052000                                                                          
052010     PERFORM UNTIL QUAL-IX > QUAL-IX-MAX                                  
052100        IF QUAL-TEKVAINF-1(QUAL-IX)  = SPACE                              
052110        AND QUAL-TEKVAINF-2(QUAL-IX) = SPACE                              
052120        AND QUAL-TEKVAINF-3(QUAL-IX) = SPACE                              
052130        AND QUAL-TEKVAINF-4(QUAL-IX) = SPACE                              
052140        AND QUAL-TEKVAINF-5(QUAL-IX) = SPACE                              
052150        AND QUAL-TEKVAINF-6(QUAL-IX) = SPACE                              
052160        AND QUAL-TEKVAINF-7(QUAL-IX) = SPACE                              
052200           CONTINUE                                                       
052300        ELSE                                                              
052400           MOVE QUAL-IDARTNR(QUAL-IX)        TO W002-IDARTNR              
052500           MOVE QUAL-BEART(QUAL-IX)          TO W002-BEART                
052600           MOVE QUAL-DAREGDAT(QUAL-IX)(3:6)  TO W002-REGDAT               
052700           MOVE QUAL-IDPERSON-BUY(QUAL-IX)   TO W002-IDPERSON-BUY         
052800           IF QUAL-TEKVAINF-1(QUAL-IX) = SPACE                            
052900              CONTINUE                                                    
053000           ELSE                                                           
053100              MOVE QUAL-TEKVAINF-1(QUAL-IX) TO W002-TEKVAINF-1            
053200           END-IF                                                         
053300           IF QUAL-TEKVAINF-2(QUAL-IX) = SPACE                            
053400              CONTINUE                                                    
053500           ELSE                                                           
053600              MOVE QUAL-TEKVAINF-2(QUAL-IX) TO W002-TEKVAINF-2            
053700           END-IF                                                         
053800           IF QUAL-TEKVAINF-3(QUAL-IX) = SPACE                            
053900              CONTINUE                                                    
054000           ELSE                                                           
054100              MOVE QUAL-TEKVAINF-3(QUAL-IX) TO W002-TEKVAINF-3            
054200           END-IF                                                         
054210           IF QUAL-TEKVAINF-4(QUAL-IX) = SPACE                            
054220              CONTINUE                                                    
054230           ELSE                                                           
054240              MOVE QUAL-TEKVAINF-4(QUAL-IX) TO W002-TEKVAINF-4            
054250           END-IF                                                         
054260           IF QUAL-TEKVAINF-5(QUAL-IX) = SPACE                            
054270              CONTINUE                                                    
054280           ELSE                                                           
054290              MOVE QUAL-TEKVAINF-5(QUAL-IX) TO W002-TEKVAINF-5            
054291           END-IF                                                         
054292           IF QUAL-TEKVAINF-6(QUAL-IX) = SPACE                            
054293              CONTINUE                                                    
054294           ELSE                                                           
054295              MOVE QUAL-TEKVAINF-6(QUAL-IX) TO W002-TEKVAINF-6            
054296           END-IF                                                         
054297           IF QUAL-TEKVAINF-7(QUAL-IX) = SPACE                            
054298              CONTINUE                                                    
054299           ELSE                                                           
054300              MOVE QUAL-TEKVAINF-7(QUAL-IX) TO W002-TEKVAINF-7            
054301           END-IF                                                         
054310                                                                          
054400           PERFORM S22-SKRIV-W61243-002                                   
054500           MOVE SPACE TO W002-TEKVAINF-1                                  
054610                         W002-TEKVAINF-2                                  
054700                         W002-TEKVAINF-3                                  
054710                         W002-TEKVAINF-4                                  
054720                         W002-TEKVAINF-5                                  
054730                         W002-TEKVAINF-6                                  
054740                         W002-TEKVAINF-7                                  
054800        END-IF                                                            
054810        ADD +1 TO QUAL-IX                                                 
054820     END-PERFORM                                                          
054900     .                                                                    
055000     EJECT                                                                
055010 X-LAS-TILL-QUAL-TAB SECTION.                                             
055020                                                                          
055030     MOVE IN02-IDARTNR       TO QUAL-IDARTNR(QUAL-IX)                     
055040     MOVE IN02-BEART         TO QUAL-BEART(QUAL-IX)                       
055050     MOVE IN02-DAREGDAT      TO QUAL-DAREGDAT(QUAL-IX)                    
055060     MOVE IN02-IDPERSON-BUY  TO QUAL-IDPERSON-BUY(QUAL-IX)                
055070     MOVE IN02-TEKVAINF-EXT(1)   TO QUAL-TEKVAINF-1(QUAL-IX)              
055080     MOVE IN02-TEKVAINF-EXT(2)   TO QUAL-TEKVAINF-2(QUAL-IX)              
055090     MOVE IN02-TEKVAINF-EXT(3)   TO QUAL-TEKVAINF-3(QUAL-IX)              
055091     MOVE IN02-TEKVAINF-EXT(4)   TO QUAL-TEKVAINF-4(QUAL-IX)              
055092     MOVE IN02-TEKVAINF-EXT(5)   TO QUAL-TEKVAINF-5(QUAL-IX)              
055093     MOVE IN02-TEKVAINF-EXT(6)   TO QUAL-TEKVAINF-6(QUAL-IX)              
055094     MOVE IN02-TEKVAINF-EXT(7)   TO QUAL-TEKVAINF-7(QUAL-IX)              
055095     .                                                                    
055096     EJECT                                                                
055100 Z-FINIT SECTION.                                                         
055200                                                                          
055300     CLOSE W61242                                                         
055310           W61243-001                                                     
055320           W61243-002                                                     
055400                                                                          
055700     MOVE 'S' TO POSTSUM-OPKOD                                            
055800     CALL POSTSUM USING POSTSUM-PARM                                      
055900     .                                                                    
056000     EJECT                                                                
056100 S01-LAES-W61242  SECTION.                                                
056200                                                                          
056300     READ W61242 INTO IN-AREA                                             
056400     AT END                                                               
056500        SET END-OF-W61242 TO TRUE                                         
056600                                                                          
056700     NOT AT END                                                           
056800        MOVE 'W61242'   TO POSTSUM-FDNAMN                                 
056900        MOVE 'W61243D1' TO POSTSUM-DDNAMN2                                
057000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
057100        CALL POSTSUM USING POSTSUM-PARM                                   
057200     END-READ                                                             
057300     .                                                                    
057400     EJECT                                                                
057410 S10-NOLLSTALL-TABELL SECTION.                                            
057420                                                                          
057430     MOVE +1 TO TAB-IX                                                    
057440     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
057450        MOVE ZERO  TO TAB-IDARTNR(TAB-IX)                                 
057460                      TAB-ADLAGOMR(TAB-IX)                                
057470                      TAB-ADGANG(TAB-IX)                                  
057480                      TAB-ADPLATS(TAB-IX)                                 
057490                      TAB-DAREGDAT(TAB-IX)                                
057491                      TAB-KVAVIS(TAB-IX)                                  
057492        MOVE SPACE TO TAB-BEART(TAB-IX)                                   
057493                      TAB-FLKDFARLIG(TAB-IX)                              
057494                      TAB-FLBACKORDER(TAB-IX)                             
057495                      TAB-FLPRIO(TAB-IX)                                  
057496                      TAB-KDARTURS(TAB-IX)                                
057497        ADD +1 TO TAB-IX                                                  
057498     END-PERFORM                                                          
057499                                                                          
057500     MOVE +1 TO QUAL-IX                                                   
057501     PERFORM UNTIL QUAL-IX > QUAL-IX-MAX                                  
057502        MOVE ZERO TO QUAL-IDARTNR(QUAL-IX)                                
057503                     QUAL-DAREGDAT(QUAL-IX)                               
057504                     QUAL-IDPERSON-BUY(QUAL-IX)                           
057505        MOVE SPACE TO QUAL-BEART(QUAL-IX)                                 
057506                      QUAL-TEKVAINF-1(QUAL-IX)                            
057507                      QUAL-TEKVAINF-2(QUAL-IX)                            
057508                      QUAL-TEKVAINF-3(QUAL-IX)                            
057509                      QUAL-TEKVAINF-4(QUAL-IX)                            
057510                      QUAL-TEKVAINF-5(QUAL-IX)                            
057511                      QUAL-TEKVAINF-6(QUAL-IX)                            
057512                      QUAL-TEKVAINF-7(QUAL-IX)                            
057513        ADD +1 TO QUAL-IX                                                 
057514     END-PERFORM                                                          
057515     .                                                                    
057516     EJECT                                                                
057520 S21-SKRIV-W61243-001  SECTION.                                           
057600                                                                          
057700     MOVE 2 TO W001-SKIP                                                  
057800     IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                        
057900       MOVE 'W61243-001' TO W001-LISTNR                                   
058000                            W001-LISTID                                   
058100       PERFORM S21A-SKRIV-RUBRIKER                                        
058200     END-IF                                                               
058300                                                                          
058400     MOVE W001-DETALJ1   TO W001-RAD                                      
058500     WRITE W61243-001-RAD FROM W001-RAD AFTER W001-SKIP                   
058600     ADD +1 TO WS-TOT-DETALJRAD                                           
058700     MOVE SPACE TO W001-RAD                                               
058800     ADD +2 TO W001-ANTAL-RADER                                           
058900                                                                          
059000     IF WS-TOT-DETALJRAD = WS-TOT-ART                                     
059100        IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                     
059200          MOVE 'W61243-001' TO W001-LISTNR                                
059300                               W001-LISTID                                
059400          PERFORM S21A-SKRIV-RUBRIKER                                     
059500        END-IF                                                            
059510*********                                                                 
059520        COMPUTE WS-ANTAL-RADER-KVAR =                                     
059530               W001-MAX-RADER-PER-SIDA - W001-ANTAL-RADER                 
059540        END-COMPUTE                                                       
059541        MOVE WS-ANTAL-RADER-KVAR TO W001-SKIP                             
059550*********                                                                 
059900        MOVE W001-SLUTRAD    TO W001-RAD                                  
060000        WRITE W61243-001-RAD FROM W001-RAD AFTER W001-SKIP                
060010        MOVE 999             TO W001-ANTAL-RADER                          
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 S21A-SKRIV-RUBRIKER SECTION.                                             
060500                                                                          
060600     ADD +1 TO W001-SIDRAKNARE                                            
060700     MOVE W001-SIDRAKNARE TO W001-SID                                     
060800     WRITE W61243-001-RAD FROM W001-RUBRIK-1 AFTER PAGE                   
060900     WRITE W61243-001-RAD FROM W001-RUBRIK-2 AFTER 4                      
061000     WRITE W61243-001-RAD FROM W001-RUBRIK-3 AFTER 4                      
061100                                                                          
061200     MOVE +15 TO W001-ANTAL-RADER                                         
061300     MOVE 3 TO W001-SKIP                                                  
061400     .                                                                    
061500     EJECT                                                                
061600 S22-SKRIV-W61243-002  SECTION.                                           
061700                                                                          
061800     MOVE 2 TO W001-SKIP                                                  
061900     MOVE 'W61243-002' TO W002-LISTNR                                     
062000                          W002-LISTID                                     
062100     PERFORM S22A-SKRIV-RUBRIKER                                          
062200                                                                          
062300     IF W002-DETALJ1 NOT = SPACE                                          
062400        MOVE W002-DETALJ1   TO W002-RAD                                   
062500***     WRITE W61243-002-RAD FROM W002-RAD AFTER W002-SKIP                
062510        WRITE W61243-001-RAD FROM W002-RAD AFTER W001-SKIP                
062600                                                                          
062700        MOVE SPACE TO W002-RAD                                            
062800        ADD +2 TO W002-ANTAL-RADER                                        
062900     END-IF                                                               
063000     IF W002-DETALJ2 NOT = SPACE                                          
063100        MOVE W002-DETALJ2   TO W002-RAD                                   
063200***     WRITE W61243-002-RAD FROM W002-RAD AFTER W002-SKIP                
063210        WRITE W61243-001-RAD FROM W002-RAD AFTER W001-SKIP                
063300                                                                          
063400        MOVE SPACE TO W002-RAD                                            
063500        ADD +2 TO W002-ANTAL-RADER                                        
063600     END-IF                                                               
063700     IF W002-DETALJ3 NOT = SPACE                                          
063800        MOVE W002-DETALJ3   TO W002-RAD                                   
063900***     WRITE W61243-002-RAD FROM W002-RAD AFTER W002-SKIP                
063910        WRITE W61243-001-RAD FROM W002-RAD AFTER W001-SKIP                
064000                                                                          
064100        MOVE SPACE TO W002-RAD                                            
064200        ADD +2 TO W002-ANTAL-RADER                                        
064300     END-IF                                                               
064310     IF W002-DETALJ4 NOT = SPACE                                          
064320        MOVE W002-DETALJ4   TO W002-RAD                                   
064330***     WRITE W61243-002-RAD FROM W002-RAD AFTER W002-SKIP                
064340        WRITE W61243-001-RAD FROM W002-RAD AFTER W001-SKIP                
064350                                                                          
064360        MOVE SPACE TO W002-RAD                                            
064370        ADD +2 TO W002-ANTAL-RADER                                        
064380     END-IF                                                               
064390     IF W002-DETALJ5 NOT = SPACE                                          
064391        MOVE W002-DETALJ5   TO W002-RAD                                   
064392***     WRITE W61243-002-RAD FROM W002-RAD AFTER W002-SKIP                
064393        WRITE W61243-001-RAD FROM W002-RAD AFTER W001-SKIP                
064394                                                                          
064395        MOVE SPACE TO W002-RAD                                            
064396        ADD +2 TO W002-ANTAL-RADER                                        
064397     END-IF                                                               
064398     IF W002-DETALJ6 NOT = SPACE                                          
064399        MOVE W002-DETALJ6   TO W002-RAD                                   
064400***     WRITE W61243-002-RAD FROM W002-RAD AFTER W002-SKIP                
064401        WRITE W61243-001-RAD FROM W002-RAD AFTER W001-SKIP                
064402                                                                          
064403        MOVE SPACE TO W002-RAD                                            
064404        ADD +2 TO W002-ANTAL-RADER                                        
064405     END-IF                                                               
064406     IF W002-DETALJ7 NOT = SPACE                                          
064407        MOVE W002-DETALJ7   TO W002-RAD                                   
064408***     WRITE W61243-002-RAD FROM W002-RAD AFTER W002-SKIP                
064409        WRITE W61243-001-RAD FROM W002-RAD AFTER W001-SKIP                
064410                                                                          
064411        MOVE SPACE TO W002-RAD                                            
064412        ADD +2 TO W002-ANTAL-RADER                                        
064413     END-IF                                                               
064420     .                                                                    
064500     EJECT                                                                
064600 S22A-SKRIV-RUBRIKER SECTION.                                             
064700                                                                          
064810     ADD +1 TO W002-SIDRAKNARE                                            
064830                                                                          
065000***  WRITE W61243-002-RAD FROM W002-RUBRIK-1 AFTER PAGE                   
065100***  WRITE W61243-002-RAD FROM W002-RUBRIK-2 AFTER 4                      
065200***  WRITE W61243-002-RAD FROM W002-RUBRIK-3 AFTER 4                      
065300                                                                          
065310     WRITE W61243-001-RAD FROM W002-RUBRIK-1 AFTER PAGE                   
065320     WRITE W61243-001-RAD FROM W002-RUBRIK-2 AFTER 4                      
065330     WRITE W61243-001-RAD FROM W002-RUBRIK-3 AFTER 4                      
065340                                                                          
065400     MOVE +15 TO W002-ANTAL-RADER                                         
065500     MOVE 3 TO W002-SKIP                                                  
065600     .                                                                    
065700     EJECT                                                                
