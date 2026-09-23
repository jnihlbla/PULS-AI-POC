000010CBL TRUNC(BIN)                                                            
000100*COMPOPT VMOD=SUB                                                         
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W9802000.                                            
000400 AUTHOR.             KJELL ANDRE.                                         
000500     DATE-WRITTEN.   MARS 1985.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    DETTA ÄR DEN CENTRALA SUBRUTINEN I SOP FÖR ATT ÄNDRA STATUS P        
001000*    ELLER HÄMTA INFORMATION OM EN PROCESS ELLER HÄMTA INFORMATION        
001100*    SOP-KALENDERN.  DET KAN KÖRAS PÅ OLIKA SÄTT - UNDER ETT              
001200*    SUBRUTIN-INTERFACE SOM SUB- PROGRAM TILL ETT APPLIKATIONSPROG        
001300*    UNDER EN KOMMANDO-DRIVER VIA EN BATCH-PROCEDUR, ELLER VIA ETT        
001400*    TSO-INTERFACE FRÅN EN ISPF-PANEL.                                    
001500*                                                                         
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900*    DDNAMN XSOPDD1   ANVÄNDS AV SUBRUTIN W980WSPC                        
002000*    DDNAMN XPDSDD1   ANVÄNDS AV SUBRUTIN W009PDSR                        
002100*    DDNAMN XPDSDD2   ANVÄNDS AV SUBRUTIN W009PDSR                        
002200*                                                                         
002300*    DÄR X ÄR 0-2 BOKSTÄVER SOM FÅS VIA PARAMETER                         
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -COPY WY2000W1                                                       
003000     SKIP3                                                                
003100 01  KONSTANTER.                                                          
003200     03  JA                  PIC X       VALUE 'J'.                       
003300     03  NEJ                 PIC X       VALUE 'N'.                       
003400     03  FORTSAETT-STARTA    PIC X       VALUE 'F'.                       
003500     03  RKOD-1000           PIC S9(4)   COMP VALUE +1000.                
003600                                                                          
003700 01  DYNAMISKA-SUBPROGRAM.                                                
003800*                                                                         
003900     03  W009INTR            PIC X(8)    VALUE 'W009INTR'.                
004000     03  W009PDSR            PIC X(8)    VALUE 'W009PDSR'.                
004100     03  W009WAIT            PIC X(8)    VALUE 'W009WAIT'.                
004200     03  W009WTOP            PIC X(8)    VALUE 'W009WTOP'.                
004300     03  W980WSPC            PIC X(8)    VALUE 'W980WSPC'.                
004400     03  W980PASS            PIC X(8)    VALUE 'W980PASS'.                
004500     03  W980USER            PIC X(8)    VALUE 'W980USER'.                
004600*                                                                         
004700*                       -- VÄRDEN FÖR PROCESS-STATUS                      
004800     03   PASSIVE-STATUS     PIC X(1)    VALUE 'P'.                       
004900     03   ENDED-STATUS       PIC X(1)    VALUE 'E'.                       
005000     03   STARTED-STATUS     PIC X(1)    VALUE 'S'.                       
005100     03   WAITING-STATUS     PIC X(1)    VALUE 'W'.                       
005200                                                                          
005300*                       -- VÄNTETID  10 SEKUNDER.                         
005400     03   WAIT-TID           PIC S9(9)   COMP  VALUE +1000.               
005500                                                                          
005600 01  DIVERSE-INDEX.                                                       
005700     03  IX                  PIC S9(9)              COMP SYNC.            
005800     03  PTR                 PIC S9(9)              COMP SYNC.            
005900                                                                          
006000 01  FLAGGOR.                                                             
006100     03  FOERSTA             PIC X.                                       
006200     03  STARTA              PIC X.                                       
006300     03  STARTAD             PIC X.                                       
006400     03  FORTSAETT           PIC X.                                       
006500     03  AVSLUTA             PIC X.                                       
006600     03  STOPPA              PIC X.                                       
006700     03  EXTRAHERA           PIC X.                                       
006800     03  AKTIVA-FINNS        PIC X.                                       
006900     03  KONFLIKT-FINNS      PIC X.                                       
007000     03  UPPTAGNA-RES-FINNS  PIC X.                                       
007100     03  KOLLA-RESURSER      PIC X.                                       
007200     03  RESURSER-SLAEPPTA   PIC X.                                       
007300     03  CAT-NCAT            PIC X.                                       
007400     03  SELECT-JOB          PIC X.                                       
007500     03  SELECT-STEP         PIC X.                                       
007600     03  OPEN-GJORD          PIC X     VALUE 'N'.                         
007700     03  WAIT-FORE-REL       PIC X.                                       
007800     03  RENSA-ATTN          PIC X.                                       
007900     03  DATUM-SLUT          PIC X.                                       
008000     03  SYMB-FOUND          PIC X.                                       
008100     03  PRIO-FOUND          PIC X.                                       
008200     03  ABEND-JOB-FOUND     PIC X.                                       
008300     03  SYMB-FEL            PIC X.                                       
008400     03  RAD-SLUT            PIC X.                                       
008500     EJECT                                                                
008600 01  WW                      PIC XX.                                      
008700 01  WSOPFUNK                PIC X.                                       
008800                                                                          
008900 01  AKTUELLT-ANR            PIC S9(4) COMP.                              
009000 01  TMP-BEG-TIAPDAT         PIC S9(7) COMP-3.                            
009100                                                                          
009200 01  ID-TYP                  PIC X.                                       
009300     88  P-ID                VALUE 'P'.                                   
009400     88  NUM-ID              VALUE 'N'.                                   
009500     SKIP2                                                                
009600 01  KODER.                                                               
009700*    --  A (AKTIVERA) ELLER P (PASSIVERA)                                 
009800     03  AKT-PASS            PIC X.                                       
009900     03  SUB-AKT-PASS        PIC X.                                       
010000                                                                          
010100 01  KLOCKSLAG.                                                           
010200     03  HHMM                PIC 9(4).                                    
010300     03  FILLER REDEFINES HHMM.                                           
010400       05   KLOCKSLAG-HH     PIC XX.                                      
010500       05   KLOCKSLAG-MM     PIC XX.                                      
010600     03   FILLER             PIC X(4).                                    
010700                                                                          
010800                                                                          
010900 01  TID1                    PIC S9(7)  COMP-3.                           
011000 01  TIEXEC                  PIC S9(7)  COMP-3.                           
011100 01  WTIEXEC-SENAST          PIC S9(7)  COMP-3.                           
011200 01  TIMMAR                  PIC S9(3)  COMP-3.                           
011300 01  MINUTER                 PIC S9(3)  COMP-3.                           
011400                                                                          
011500 01  WDATUM                  PIC S9(7)  COMP-3.                           
011600 01  DATUM-ZONAT             PIC 9(6).                                    
011700 01  DATUM-PARM.                                                          
011800     03  FILLER              PIC S9(4) COMP  VALUE +6.                    
011900     03  DATUM-PACKAT        PIC S9(7) COMP-3.                            
012000                                                                          
012100 01  WCDATE                  PIC X(21).                                   
012200 01  WTIMESTAMP.                                                          
012300     03  WTS-CCYY            PIC X(4).                                    
012400     03  FILLER              PIC X     VALUE '-'.                         
012500     03  WTS-MM              PIC X(2).                                    
012600     03  FILLER              PIC X     VALUE '-'.                         
012700     03  WTS-DD              PIC X(2).                                    
012800     03  FILLER              PIC X     VALUE SPACE.                       
012900     03  WTS-TIM             PIC X(2).                                    
013000     03  FILLER              PIC X     VALUE ':'.                         
013100     03  WTS-MIN             PIC X(2).                                    
013200                                                                          
013300 01  WORK-TEPRED             PIC X(100).                                  
013400                                                                          
013500 01  WPRIO                  PIC X.                                        
013600                                                                          
013700 01  SPAR-KDANR             PIC S9(4)    COMP.                            
013800 01  SPAR-STARTTYP          PIC X(4).                                     
013900 01  SPAR-FLBATCH           PIC X.                                        
014000     EJECT                                                                
014100 01  INGET                  PIC X.                                        
014200 01  KORT-TYP               PIC X(8).                                     
014300 01  ORD1                   PIC X(8).                                     
014400                                                                          
014500*  VARIABLER TILL SYMBOL-SUBSTITUTION I JOBB                              
014600 01  ANTAL-SYMBOLER         PIC S9(4).                                    
014700 01  ANT-RIGHT              PIC S9(4).                                    
014800 01  ANT-LEFT               PIC S9(4).                                    
014900     SKIP2                                                                
015000 01  SYMBOL-TABELL.                                                       
015100     03  SYMBOL             PIC X(25).                                    
015200     03  SYMBOL-VAERDE      PIC X(85).                                    
015300     SKIP2                                                                
015400 01  FILL                   PIC X(80).                                    
015500 01  DEL                    PIC X.                                        
015600 01  ANTAL-SYMB-VAR         PIC S9(4).                                    
015700 01  ANTAL-2AMP-VAR         PIC S9(4).                                    
015800 01  USER-VAR               PIC S9(4).                                    
015900 01  PASSW-VAR              PIC S9(4).                                    
016000                                                                          
016100 01  TEMP-RAD               PIC X(1000).                                  
016200 01  JCL-RAD                PIC X(80).                                    
016300                                                                          
016400 01  SYMB-IX                PIC S9(4).                                    
016500 01  JCL-IX                 PIC S9(4).                                    
016600 01  JCL-IX-PLUS            PIC S9(4).                                    
016700 01  JCL-IX-MINUS           PIC S9(4).                                    
016800 01  RAD-IX                 PIC S9(4).                                    
016900                                                                          
017000 01  TEMP-ATTR              PIC X(20).                                    
017100                                                                          
017200 01  TECKEN-M               PIC X.                                        
017300 01  SYMB-RAD.                                                            
017400     03  TECKEN             PIC X OCCURS 80.                              
017500     SKIP2                                                                
017600 01  T-RAD.                                                               
017700     03  T                  PIC X OCCURS 1000.                            
017800     SKIP2                                                                
017900 01  WJCL-AREA              PIC X(80).                                    
018000 01  FILLER REDEFINES WJCL-AREA.                                          
018100     03  WJCL-AREA-POS-1-2  PIC XX.                                       
018200     03  FILLER             PIC X(78).                                    
018300     EJECT                                                                
018400*------- PARAMETERAR TILL W980WSPC (HANTERING AV SOP-REGISTRET)           
018500 01  WSPACE-DD-PARM.                                                      
018600     03   WSPACE-DD-LENGD    PIC S9(4)   COMP  VALUE +10.                 
018700     03   WSPACE-DD-NAMN     PIC X(8)    VALUE 'SOPDD1  '.                
018800                                                                          
018900 01  FUNKTIONSKODER.                                                      
019000     03  FOPEN               PIC X(4)    VALUE 'OPEN'.                    
019100     03  FCLSE               PIC X(4)    VALUE 'CLSE'.                    
019200     03  FSAVE               PIC X(4)    VALUE 'SAVE'.                    
019300     03  FQUIT               PIC X(4)    VALUE 'QUIT'.                    
019400     03  FADD                PIC X(4)    VALUE 'ADD '.                    
019500     03  FGETF               PIC X(4)    VALUE 'GETF'.                    
019600     03  FGETN               PIC X(4)    VALUE 'GETN'.                    
019700     03  FTEST               PIC X(4)    VALUE 'TEST'.                    
019800     03  FSET                PIC X(4)    VALUE 'SET '.                    
019900     03  FCLR                PIC X(4)    VALUE 'CLR '.                    
020000     03  FDEL                PIC X(4)    VALUE 'DEL '.                    
020100     03  FDELK               PIC X(4)    VALUE 'DELK'.                    
020200                                                                          
020300 01  WSRKOD                  PIC X.                                       
020400 01  WSRKOD2                 PIC X.                                       
020500                                                                          
020600*----------------- GENERELLA ARBETS-PARAMETRAR                            
020700 01  NAMN-PARM.                                                           
020800     03   NAMN-LENGD         PIC S9(4)   COMP.                            
020900     03   NAMN-VAERDE        PIC X(20).                                   
021000                                                                          
021100 01  ATTR-PARM.                                                           
021200     03   ATTR-LENGD         PIC S9(4)   COMP.                            
021300     03   ATTR-VAERDE        PIC X(20).                                   
021400                                                                          
021500 01  RES-PARM.                                                            
021600     03   RES-LENGD          PIC S9(4)   COMP.                            
021700     03   RES-VAERDE         PIC X(20).                                   
021800                                                                          
021900 01  END-RES-PARM.                                                        
022000     03   END-RES-LENGD      PIC S9(4)   COMP.                            
022100     03   END-RES-VAERDE     PIC X(20).                                   
022200                                                                          
022300 01  TEMP-RES-PARM.                                                       
022400     03   TEMP-RES-LENGD     PIC S9(4)   COMP.                            
022500     03   TEMP-RES-VAERDE    PIC X(20).                                   
022600                                                                          
022700 01  DATA-PARM.                                                           
022800     03   DATA-LENGD         PIC S9(4)   COMP.                            
022900     03   DATA-VAERDE        PIC X(240).                                  
023000                                                                          
023100 01  DATA2-PARM.                                                          
023200     03   DATA2-LENGD        PIC S9(4)   COMP.                            
023300     03   DATA2-VAERDE       PIC X(240).                                  
023400                                                                          
023500     EJECT                                                                
023600*-------- DATA OM HUVUD-PROCESSEN                                         
023700 01  HP-PARM.                                                             
023800     03  HP-LENGD            PIC S9(4)   COMP  VALUE +0.                  
023900     03  HP-IDPROCESS        PIC X(10)   VALUE SPACE.                     
024000                                                                          
024100 01  HP-TIAPDAT              PIC S9(7)   COMP-3.                          
024200 01  HP-KDPROCSTAT           PIC X.                                       
024300                                                                          
024400 01  MTYP-VAERDE-PARM.                                                    
024500     03  FILLER              PIC S9(4)   COMP.                            
024600     03  MTYP-VAERDE         PIC X.                                       
024700                                                                          
024800 01  PAR-HP-PARM.                                                         
024900     03  PAR-HP-LENGD        PIC S9(4)   COMP  VALUE +0.                  
025000     03  PAR-HP-IDPROCESS PIC X(10)      VALUE SPACE.                     
025100                                                                          
025200*-------- PASSIVERINGS/AKTIVERINGSDATUM (FÅS VIA PARAMETER)               
025300 01  BEGAERD-TIAPDAT-PARM.                                                
025400     03  FILLER              PIC S9(4)   COMP VALUE +6.                   
025500     03  BEGAERD-TIAPDAT     PIC S9(7)   COMP-3.                          
025600                                                                          
025700*-------- AKTIVERINGSDATUM + ORDERNUMMER                                  
025800 01  BEG-TIAPDAT-ANR-PARM.                                                
025900     03  BEG-DAT-ANR-LENGD   PIC S9(4)   COMP VALUE +8.                   
026000     03  BEG-TIAPDAT         PIC S9(7)   COMP-3.                          
026100     03  P-ANR               PIC S9(4)   COMP.                            
026200                                                                          
026300*-------- PROCESS SOM BEARBETAS FÖR TILLFÄLLET                            
026400 01  P-PARM.                                                              
026500     03  P-LENGD             PIC S9(4)   COMP  VALUE +0.                  
026600     03  P-IDPROCESS         PIC X(10)   VALUE SPACE.                     
026700                                                                          
026800*-------- PROCESS SOM ABENDAT                                             
026900 01  AB-PARM.                                                             
027000     03  FILLER              PIC S9(4)   COMP  VALUE +0.                  
027100     03  FILLER              PIC X(10)   VALUE SPACE.                     
027200                                                                          
027300*-------- PROCESS SOM BEARBETAS FÖR TILLFÄLLET                            
027400 01  TEMP-PARM.                                                           
027500     03  TEMP-LENGD          PIC S9(4)   COMP  VALUE +0.                  
027600     03  TEMP-IDPROCESS      PIC X(10)   VALUE SPACE.                     
027700                                                                          
027800*-------- PROCESS SOM AVSLUTAS FÖR TILLFÄLLET                             
027900 01  EP-PARM.                                                             
028000     03  EP-LENGD            PIC S9(4)   COMP  VALUE +0.                  
028100     03  EP-IDPROCESS        PIC X(10)   VALUE SPACE.                     
028200                                                                          
028300*-------- PROCESS FÖR VILKEN RESURSER SLÄPPS FRIA                         
028400 01  RP-PARM.                                                             
028500     03  RP-LENGD            PIC S9(4)   COMP  VALUE +0.                  
028600     03  RP-IDPROCESS        PIC X(10)   VALUE SPACE.                     
028700                                                                          
028800*-------- PROCESS SOM HAR START-KONFLIKT MED AKTUELL PROCESS              
028900 01  KP-PARM.                                                             
029000     03  KP-LENGD            PIC S9(4)   COMP  VALUE +0.                  
029100     03  KP-IDPROCESS        PIC X(10)   VALUE SPACE.                     
029200     EJECT                                                                
029300*--------- DATA-PARAMETER TILL ATTRIBUT "INF"                             
029400 01  INFO-VAERDE-PARM.                                                    
029500*    03   -COPY W980INF                                                   
029600     EJECT                                                                
029700*---------- AKTUELLT DATUM I SOP-REGISTRETS KALENDER                      
029800 01  NUDAT-PARM.                                                          
029900     03   NUDAT-LENGD        PIC S9(4)  COMP  VALUE +6.                   
030000     03   NUDAT              PIC S9(7)  COMP-3.                           
030100                                                                          
030200*---------- AKTUELLT DATUM I SOP ELLER AKTUELLT AKTIVERINGS-DATUM         
030300 01  AKT-NUDAT-PARM.                                                      
030400     03   AKT-NUDAT-LENGD    PIC S9(4)  COMP  VALUE +6.                   
030500     03   AKT-NUDAT          PIC S9(7)  COMP-3.                           
030600                                                                          
030700 01  ATTN-TEXT-PARM.                                                      
030800     03   ATTN-TEXT-LENGD    PIC S9(4)   COMP   VALUE +17.                
030900     03   ATTN-TEXT          PIC X(15).                                   
031000                                                                          
031100 01  FTID-VAERDE-PARM.                                                    
031200     03   FILLER             PIC S9(4)  COMP  VALUE +7.                   
031300     03   FTID-VAERDE        PIC 9(5).                                    
031400                                                                          
031500 01  ANR-PARM.                                                            
031600     03   FILLER             PIC S9(4)  COMP  VALUE +4.                   
031700     03   ANR                PIC S9(4)  COMP.                             
031800     EJECT                                                                
031900*-------- STACK FÖR ATT HÅLLA REDA PÅ POSITIONER HIERARKIN AV             
032000*         PROCESSER                                                       
032100 01  FILLER                      PIC X(16) VALUE 'SIX-START'.             
032200 01  SIX                         PIC S9(9) COMP.                          
032300 01  SIX-FULL                    PIC S9(9) COMP  VALUE +10.               
032400 01  STACK.                                                               
032500     03 STACK-ELEMENT OCCURS 10.                                          
032600       05  STACK-IDPROCESS-PARM.                                          
032700         07 FILLER               PIC S9(4) COMP.                          
032800         07 STACK-IDPROCESS      PIC X(10).                               
032900       05  STACK-AKT-PASS        PIC X.                                   
033000       05  STACK-ANR             PIC S9(4) COMP.                          
033100     SKIP3                                                                
033200*-------- STACK FÖR ATT HÅLLA REDA PÅ POSITIONER HIERARKIN AV             
033300*         PROCESSER SOM SKA AKTIVERAS/PASSIVERAS                          
033400 01  FILLER                      PIC X(16) VALUE 'AP-SIX-START'.          
033500 01  AP-SIX                      PIC S9(9) COMP.                          
033600 01  AP-SIX-FULL                 PIC S9(9) COMP  VALUE +10.               
033700 01  AP-STACK.                                                            
033800     03 AP-STACK-ELEMENT OCCURS 10.                                       
033900       05  AP-STACK-IDPROCESS-PARM.                                       
034000         07 FILLER               PIC S9(4) COMP.                          
034100         07 AP-STACK-IDPROCESS   PIC X(10).                               
034200       05  AP-STACK-AKT-PASS     PIC X.                                   
034300       05  AP-STACK-ANR          PIC S9(4) COMP.                          
034400     SKIP3                                                                
034500*------  TABELL FÖR LAGRING AV PROCESSER VID ANALYS AV                    
034600*        FÖREGÅNGARE TILL EN KANDIDAT FÖR START.                          
034700 01  FILLER                      PIC X(16) VALUE 'AIX-START'.             
034800 01  AIX                         PIC S9(9) COMP.                          
034900 01  AIX-MAX                     PIC S9(9) COMP.                          
035000 01  AIX-FULL                    PIC S9(9) COMP  VALUE +500.              
035100 01  AFTER-TABELL.                                                        
035200     03  AFTER-NAMN-PARM OCCURS 500.                                      
035300         05 FILLER               PIC S9(4) COMP.                          
035400         05 AFTER-IDPROCESS      PIC X(10).                               
035500     SKIP3                                                                
035600*------  TABELL FÖR LAGRING AV AKTIVA FÖREGÅNGARE TILL                    
035700*        EN PROCESS                                                       
035800 01  FILLER                      PIC X(16) VALUE 'FIX-START'.             
035900 01  FIX                         PIC S9(9) COMP.                          
036000 01  FIX-MAX                     PIC S9(9) COMP.                          
036100 01  FIX-FULL                    PIC S9(9) COMP  VALUE +50.               
036200 01  FOREG-TABELL.                                                        
036300     03  FILLER          OCCURS 50.                                       
036400         05 FOREG-IDPROCESS      PIC X(10).                               
036500     SKIP3                                                                
036600*------  TABELL FÖR LAGRING AV PROCESSER VID ANALYS AV                    
036700*        EFTERFÖLJARE TILL EN AVSLUTAD PROCESS.                           
036800 01  FILLER                      PIC X(16) VALUE 'BIX-START'.             
036900 01  BIX                         PIC S9(9) COMP.                          
037000 01  BIX-MAX                     PIC S9(9) COMP.                          
037100 01  BIX-FULL                    PIC S9(9) COMP  VALUE +500.              
037200 01  BEFORE-TABELL.                                                       
037300     03  BEFORE-NAMN-PARM OCCURS 500.                                     
037400         05 FILLER               PIC S9(4) COMP.                          
037500         05 BEFORE-IDPROCESS     PIC X(10).                               
037600     SKIP3                                                                
037700*------  TABELL FÖR LAGRING AV VÄNTANDE PROCESSER VID ANALYS AV           
037800*        AV EFTERFÖLJARE TILL EN AVSLUTAD PROCESS.                        
037900 01  FILLER                      PIC X(16) VALUE 'WIX-START'.             
038000 01  WIX                         PIC S9(9) COMP.                          
038100 01  WIX-MAX                     PIC S9(9) COMP.                          
038200 01  WIX-FULL                    PIC S9(9) COMP  VALUE +500.              
038300 01  WAITING-TABELL.                                                      
038400     03  WAITING-NAMN-PARM OCCURS 500.                                    
038500         05 FILLER               PIC S9(4) COMP.                          
038600         05 WAITING-IDPROCESS    PIC X(10).                               
038700     EJECT                                                                
038800 01  ABENDED-PARM.                                                        
038900     03   FILLER             PIC S9(4)   COMP   VALUE +5.                 
039000     03   FILLER             PIC X(3)    VALUE 'ABE'.                     
039100                                                                          
039200 01  ABEND-JOB-PARM.                                                      
039300     03   FILLER             PIC S9(4)   COMP   VALUE +6.                 
039400     03   FILLER             PIC X(4)    VALUE 'ABJB'.                    
039500                                                                          
039600 01  ATTN-PARM.                                                           
039700     03   FILLER             PIC S9(4)   COMP   VALUE +6.                 
039800     03   FILLER             PIC X(4)    VALUE 'ATTN'.                    
039900                                                                          
040000 01  ACTQ-PARM.                                                           
040100     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
040200     03   FILLER             PIC X(2)    VALUE 'AQ'.                      
040300                                                                          
040400 01  AFTER-PARM.                                                          
040500     03   FILLER             PIC S9(4)   COMP  VALUE +3.                  
040600     03   FILLER             PIC X(1)    VALUE '<'.                       
040700                                                                          
040800 01  BEFORE-PARM.                                                         
040900     03   FILLER             PIC S9(4)   COMP  VALUE +3.                  
041000     03   FILLER             PIC X(1)    VALUE '>'.                       
041100                                                                          
041200 01  DATUM-LIST-PARM.                                                     
041300     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
041400     03   FILLER             PIC X(4)    VALUE 'DATL'.                    
041500                                                                          
041600 01  KALENDER-PARM.                                                       
041700     03   FILLER             PIC S9(4)   COMP  VALUE +7.                  
041800     03   FILLER             PIC X(5)    VALUE '*CAL*'.                   
041900                                                                          
042000 01  CATALOG-PARM.                                                        
042100     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
042200     03   FILLER             PIC X(3)    VALUE 'CAT'.                     
042300                                                                          
042400 01  NOT-CATALOG-PARM.                                                    
042500     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
042600     03   FILLER             PIC X(4)    VALUE 'NCAT'.                    
042700                                                                          
042800 01  CONTAINS-PARM.                                                       
042900     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
043000     03   FILLER             PIC X(3)    VALUE 'CON'.                     
043100                                                                          
043200 01  DB-STATUS-PARM.                                                      
043300     03   FILLER             PIC S9(4)   COMP  VALUE +8.                  
043400     03   FILLER             PIC X(6)    VALUE 'DBSTAT'.                  
043500                                                                          
043600 01  PASSQ-PARM.                                                          
043700     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
043800     03   FILLER             PIC X(2)    VALUE 'DQ'.                      
043900                                                                          
044000 01  RESQ-PARM.                                                           
044100     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
044200     03   FILLER             PIC X(2)    VALUE 'RQ'.                      
044300                                                                          
044400 01  EJ-SAMTIDIGT-PARM.                                                   
044500     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
044600     03   FILLER             PIC X(2)    VALUE '<>'.                      
044700                                                                          
044800 01  FTID-PARM.                                                           
044900     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
045000     03   FILLER             PIC X(4)    VALUE 'FTID'.                    
045100                                                                          
045200 01  HOLD-PARM.                                                           
045300     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
045400     03   FILLER             PIC X(4)    VALUE 'HOLD'.                    
045500                                                                          
045600 01  INFO-NAMN-PARM.                                                      
045700     03   FILLER             PIC S9(4)   COMP   VALUE +5.                 
045800     03   FILLER             PIC X(3)    VALUE 'INF'.                     
045900                                                                          
046000 01  NUDAT-NAMN-PARM.                                                     
046100     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
046200     03   FILLER             PIC X(4)    VALUE 'CDAT'.                    
046300                                                                          
046400 01  MTYP-PARM.                                                           
046500     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
046600     03   FILLER             PIC X(4)    VALUE 'MTYP'.                    
046700                                                                          
046800 01  NULL-PARM.                                                           
046900     03   FILLER             PIC S9(4)   COMP  VALUE +2.                  
047000                                                                          
047100 01  PARENT-PARM.                                                         
047200     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
047300     03   FILLER             PIC X(3)    VALUE 'PAR'.                     
047400                                                                          
047500 01  PRIO-PARM.                                                           
047600     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
047700     03   FILLER             PIC X(4)    VALUE 'PRIO'.                    
047800                                                                          
047900 01  SOP-PARM.                                                            
048000     03   FILLER             PIC S9(4)   COMP  VALUE +7.                  
048100     03   FILLER             PIC X(5)    VALUE '*SOP*'.                   
048200                                                                          
048300 01  AKT-NR-PARM.                                                         
048400     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
048500     03   FILLER             PIC X(4)    VALUE '*ANR'.                    
048600                                                                          
048700 01  STARTED-LIST-PARM.                                                   
048800     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
048900     03   FILLER             PIC X(2)    VALUE 'SL'.                      
049000                                                                          
049100 01  STARTQ-PARM.                                                         
049200     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
049300     03   FILLER             PIC X(2)    VALUE 'SQ'.                      
049400                                                                          
049500 01  STARTTYP-PARM.                                                       
049600     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
049700     03   FILLER             PIC X(4)    VALUE 'STYP'.                    
049800                                                                          
049900 01  SYMB-PARM.                                                           
050000     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
050100     03   FILLER             PIC X(4)    VALUE 'SYMB'.                    
050200                                                                          
050300 01  TEMP-VARAKTIGHET-PARM.                                               
050400     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
050500     03   FILLER             PIC X(4)    VALUE 'TEMP'.                    
050600                                                                          
050700 01  VOUT-PARM.                                                           
050800     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
050900     03   FILLER             PIC X(4)    VALUE 'VOUT'.                    
051000                                                                          
051100 01  ACTMSG-PARM.                                                         
051200     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
051300     03   FILLER             PIC X(4)    VALUE 'AMSG'.                    
051400                                                                          
051500 01  USING-PARM.                                                          
051600     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
051700     03   FILLER             PIC X(4)    VALUE 'USNG'.                    
051800                                                                          
051900 01  USED-PARM.                                                           
052000     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
052100     03   FILLER             PIC X(4)    VALUE 'USED'.                    
052200     EJECT                                                                
052300 01  PDS-DDNAME-1            PIC X(8)    VALUE 'PDSDD1  '.                
052400 01  PDS-DDNAME-2            PIC X(8)    VALUE 'PDSDD2  '.                
052500                                                                          
052600*------- PARAMETERAR TILL W009PDSR (LÄS PDS-MEDLEM)                       
052700 01  PDS-OPEN                PIC X       VALUE 'O'.                       
052800 01  PDS-READ                PIC X       VALUE SPACE.                     
052900 01  PDS-CLOSE               PIC X       VALUE 'C'.                       
053000                                                                          
053100 01  PDS-DD-MEMBER.                                                       
053200     03  PDS-DDNAME          PIC X(8)    VALUE 'PDSDD?  '.                
053300     03  PDS-MBRNAME         PIC X(8)    VALUE SPACE.                     
053400                                                                          
053500 01  PROB-CASE-PARM.                                                      
053600     03   FILLER             PIC S9(4)   COMP  VALUE +0.                  
053700     03   FILLER             PIC X(10)   VALUE SPACE.                     
053800                                                                          
053900 01  PDS-RECORD-AREA         PIC X(80).                                   
054000     EJECT                                                                
054100*------- PARAMETERAR TILL W009INTR (DYNAMISK KOMUNIKATION                 
054200*        MED INTERNAL READER)                                             
054300 01  OPEN-INTRDR             PIC X       VALUE 'O'.                       
054400 01  WRITE-INTRDR            PIC X       VALUE SPACE.                     
054500 01  CLOSE-INTRDR            PIC X       VALUE 'C'.                       
054600                                                                          
054700 01  JES-RELEASE-CMD.                                                     
054800     03  FILLER              PIC X(6)  VALUE  '/*ÅVS,'.                   
054900     03  FILLER              PIC X     VALUE   QUOTE.                     
055000     03  FILLER              PIC XX    VALUE  'ÅA'.                       
055100     03  FILLER              PIC XX    VALUE  ALL QUOTES.                 
055200     03  JES-IDPROCESS       PIC X(8).                                    
055300     03  FILLER REDEFINES JES-IDPROCESS.                                  
055400         05  FILLER          PIC X(4).                                    
055500         05  JES-JOBB-POS5   PIC X.                                       
055600         05  FILLER          PIC X(3).                                    
055700     03  FILLER              PIC XXX   VALUE  ALL QUOTES.                 
055800     03  FILLER              PIC X(58) VALUE SPACE.                       
055900                                                                          
056000     EJECT                                                                
056100*------- PARAMETERAR TILL W009WTOP (SKRIV PÅ JES-LOGGEN)                  
056200*     F WTOP-MSG                                                          
056300 01  WTOP-PARM.                                                           
056400     03  WTOP-MSG-LENGD        PIC S9(4)   COMP VALUE +68.                
056500     03  WTOP-CLIENT           PIC X(8).                                  
056600     03  WTOP-DB-STATUS        PIC X.                                     
056700     03  WTOP-MSG.                                                        
056800         05  WTOP-MSG-ID.                                                 
056900             07  FILLER        PIC X(3)    VALUE 'SOP'.                   
057000             07  WTOP-MSG-NR   PIC 9(3).                                  
057100         05  WTOP-MSG-TEXT     PIC X(53).                                 
057200                                                                          
057300                                                                          
057400*------- PARAMETERAR TILL W980PASS (RÄKNA FRAM LÖSENORD)                  
057500 01  -COPY W980PASS                                                       
057600                                                                          
057700     EJECT                                                                
057800*------------------------- NAMN PÅ FÖRÄLDERPROCESS MED JCL                
057900 01  EXTRACT-PARM.                                                        
058000     03   FILLER             PIC S9(4)   COMP.                            
058100     03   EXTRACT-MEDLEM     PIC X(10).                                   
058200     EJECT                                                                
058300*------------------------- AREOR FÖR FEL-MEDDELANDEN                      
058400                                                                          
058500*                       -- HJÄLPAREOR VID FELUTSKRIFT                     
058600 01  TAL-X.                                                               
058700     03  TAL-DISPLAY         PIC 9(3).                                    
058800                                                                          
058900 01  DATUM-X.                                                             
059000     03  DATUM-DISPLAY       PIC 9(6).                                    
059100     03  FILLER REDEFINES DATUM-DISPLAY.                                  
059200       05  AAR-DISPLAY       PIC 99.                                      
059300       05  MAANAD-DISPLAY    PIC 99.                                      
059400       05  DAG-DISPLAY       PIC 99.                                      
059500                                                                          
059600 01  MSG-NR                     PIC S9(4)   COMP.                         
059700 01  M.                                                                   
059800     03  M1.                                                              
059900       05 FILLER         COMP   PIC S9(4)   VALUE +12.                    
060000       05 FILLER                PIC X(60)   VALUE                         
060100          'S  BLANK PROCESS-NAME NOT ALLOWED.'.                           
060200       05 FILLER                PIC X(60)   VALUE SPACE.                  
060300                                                                          
060400     03  M2.                                                              
060500       05 FILLER         COMP   PIC S9(4)   VALUE +12.                    
060600       05 FILLER                PIC X(26)   VALUE                         
060700          'S  ILLEGAL FUNCTION CODE: '.                                   
060800       05 M2-SOPFUNC            PIC X.                                    
060900       05 FILLER                PIC X(33)   VALUE SPACE.                  
061000       05 FILLER                PIC X(60)   VALUE SPACE.                  
061100                                                                          
061200     03  M3.                                                              
061300       05 FILLER         COMP   PIC S9(4)   VALUE +12.                    
061400       05 FILLER                PIC X(60)   VALUE                         
061500          'S  ACTIVATION/PASSIVATION DATE NOT NUMERIC.'.                  
061600       05 FILLER                PIC X(60)   VALUE SPACE.                  
061700                                                                          
061800     03  M4.                                                              
061900       05 FILLER        COMP    PIC S9(4)   VALUE +12.                    
062000       05 FILLER                PIC X(3)    VALUE  'S  '.                 
062100       05 FILLER                PIC X(5)    VALUE  'DATE '.               
062200       05 M4-DATUM              PIC X(6).                                 
062300       05 FILLER                PIC X(46)   VALUE                         
062400          ' NOT FOUND IN THE CALENDAR.'.                                  
062500       05 FILLER                PIC X(60)   VALUE SPACE.                  
062600                                                                          
062700     03  M5.                                                              
062800       05 FILLER        COMP    PIC S9(4)   VALUE +16.                    
062900       05 FILLER                PIC X(23)   VALUE                         
063000          'S  CAN NOT OPEN DDNAME '.                                      
063100       05 M5-DDNAMN             PIC X(8).                                 
063200       05 FILLER                PIC X(29)   VALUE SPACE.                  
063300       05 FILLER                PIC X(60)   VALUE SPACE.                  
063400                                                                          
063500     03  M6.                                                              
063600       05 FILLER        COMP    PIC S9(4)   VALUE +16.                    
063700       05 FILLER                PIC X(41)   VALUE                         
063800         'S  CURRENT DATE MISSING IN DATA BASE. RC='.                     
063900       05 M6-RKOD               PIC X.                                    
064000       05 FILLER                PIC X(18)   VALUE SPACE.                  
064100       05 FILLER                PIC X(60)   VALUE SPACE.                  
064200                                                                          
064300     03  M7.                                                              
064400       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
064500       05 FILLER                PIC X(60)   VALUE                         
064600          'E  SPECIFIED ACTIVATION/PASSIVATION DATE IS TOO OLD.'.         
064700       05 FILLER                PIC X(27)   VALUE                         
064800          '   OLDEST DATE ALLOWED FOR '.                                  
064900       05 M7-IDPROCESS          PIC X(10).                                
065000       05 FILLER                PIC X(4)    VALUE ' IS '.                 
065100       05 M7-TIAPDAT            PIC 9(6).                                 
065200       05 FILLER                PIC X(13)   VALUE SPACE.                  
065300                                                                          
065400     03  M8.                                                              
065500       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
065600       05 FILLER                PIC X(28)   VALUE                         
065700          'W  PREVIOUS CANCELLATION OF '.                                 
065800       05 M8-IDPROCESS          PIC X(10).                                
065900       05 FILLER                PIC X(5)    VALUE ' FOR '.                
066000       05 M8-TIAPDAT            PIC 9(6).                                 
066100       05 FILLER                PIC X(11)   VALUE ' HAS BEEN'.            
066200       05 FILLER                PIC X(60)   VALUE                         
066300          '   DELETED.  NO ORDER HAS BEEN REGISTRED.'.                    
066400                                                                          
066500     03  M9.                                                              
066600       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
066700       05 FILLER                PIC X(3)    VALUE 'E  '.                  
066800       05 M9-IDPROCESS          PIC X(10).                                
066900       05 FILLER                PIC X(30)   VALUE                         
067000          ' HAS ALREADY BEEN ORDERED FOR '.                               
067100       05 M9-TIAPDAT            PIC 9(6).                                 
067200       05 FILLER                PIC X(11)   VALUE '.  MULTIPLE'.          
067300       05 FILLER                PIC X(60)   VALUE                         
067400       '   ORDERS FOR ONE DATE IS NOT ALLOWED FOR THIS PROCESS.'.         
067500                                                                          
067600     03  M10.                                                             
067700       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
067800       05 FILLER                PIC X(21)   VALUE                         
067900          'W  PREVIOUS ORDER OF '.                                        
068000       05 M10-IDPROCESS         PIC X(10).                                
068100       05 FILLER                PIC X(5)    VALUE ' FOR '.                
068200       05 M10-TIAPDAT           PIC 9(6).                                 
068300       05 FILLER                PIC X(18)   VALUE ' HAS BEEN  '.          
068400       05 FILLER                PIC X(60)   VALUE                         
068500          '   DELETED. NO CANCELLATION HAS BEEN REGISTRED.'.              
068600                                                                          
068700     03  M11.                                                             
068800       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
068900       05 FILLER                PIC X(3)    VALUE                         
069000          'E  '.                                                          
069100       05 M11-IDPROCESS         PIC X(10).                                
069200       05 FILLER                PIC X(31)   VALUE                         
069300          ' HAS ALREADY BEEN CANCELLED ON '.                              
069400       05 M11-TIAPDAT           PIC 9(6).                                 
069500       05 FILLER                PIC X(10)   VALUE '. '.                   
069600       05 FILLER                PIC X(60)   VALUE                         
069700       '   MULTIPLE CANCELLATIONS TO ONE DATE ARE NOT ALLOWED.'.          
069800                                                                          
069900     03  M12.                                                             
070000       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
070100       05 FILLER                PIC X(3)    VALUE                         
070200          'E  '.                                                          
070300       05 M12-IDPROCESS         PIC X(10).                                
070400       05 FILLER                PIC X(47)   VALUE                         
070500          ' HAS START TYPE "REL",  BUT NO PARENT WITH     '.              
070600       05 FILLER                PIC X(60)   VALUE                         
070700          '   START-TYPE "FSUB"/"SUB" EXISTS.'.                           
070800                                                                          
070900     03  M13.                                                             
071000       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
071100       05 FILLER                PIC X(3)    VALUE                         
071200          'E  '.                                                          
071300       05 M13-IDPROCESS         PIC X(10).                                
071400       05 FILLER                PIC X(47)   VALUE                         
071500          ' IS NOT ACTIVE.'.                                              
071600       05 FILLER                PIC X(60)   VALUE                         
071700          '   END REQUEST IGNORED FOR THIS PROCESS.'.                     
071800                                                                          
071900     03  M14.                                                             
072000       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
072100       05 FILLER                PIC X(3)    VALUE                         
072200          'W  '.                                                          
072300       05 M14-IDPROCESS         PIC X(10).                                
072400       05 FILLER                PIC X(47)   VALUE                         
072500          ' IS ALREADY HELD. '.                                           
072600       05 FILLER                PIC X(60)   VALUE                         
072700          '   HOLD REQUEST IGNORED FOR THIS PROCESS.'.                    
072800                                                                          
072900     03  M15.                                                             
073000       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
073100       05 FILLER                PIC X(3)    VALUE                         
073200          'W  '.                                                          
073300       05 M15-IDPROCESS         PIC X(10).                                
073400       05 FILLER                PIC X(47)   VALUE                         
073500          ' IS NOT HELD.'.                                                
073600       05 FILLER                PIC X(60)   VALUE                         
073700          '   RELEASE REQUEST IGNORED FOR THIS PROCESS.'.                 
073800                                                                          
073900     03  M16.                                                             
074000       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
074100       05 FILLER                PIC X(3)    VALUE                         
074200          'E  '.                                                          
074300       05 M16-IDPROCESS         PIC X(10).                                
074400       05 FILLER                PIC X(47)   VALUE                         
074500          ' IS ALREADY ACTIVE.'.                                          
074600       05 FILLER                PIC X(60)   VALUE                         
074700     '   ACTIVATION/ORDER REQUEST IGNORED FOR THIS PROCESS.'.             
074800                                                                          
074900     03  M17.                                                             
075000       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
075100       05 FILLER                PIC X(3)    VALUE                         
075200          'W  '.                                                          
075300       05 M17-IDPROCESS         PIC X(10).                                
075400       05 FILLER                PIC X(47)   VALUE                         
075500          ' IS ALREADY PASSIVE.'.                                         
075600       05 FILLER                PIC X(60)   VALUE  '   PASSIVATION        
075700-         '/CANCELLATION IGNORED FOR THIS PROCESS'.                       
075800                                                                          
075900     03  M18.                                                             
076000       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
076100       05 FILLER                PIC X(3)    VALUE                         
076200          'W  '.                                                          
076300       05 M18-IDPROCESS         PIC X(10).                                
076400       05 FILLER                PIC X(47)   VALUE                         
076500          ' IS HELD,  AND CAN THEREFORE NOT BE STARTED.'.                 
076600       05 FILLER                PIC X(60)   VALUE SPACE.                  
076700                                                                          
076800     03  M19.                                                             
076900       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
077000       05 FILLER                PIC X(3)    VALUE                         
077100          'E  '.                                                          
077200       05 M19-IDPROCESS         PIC X(10).                                
077300       05 FILLER                PIC X(47)   VALUE                         
077400          ' HAS ALREADY STARTED.'.                                        
077500       05 FILLER                PIC X(60)   VALUE                         
077600          '   START REQUEST IGNORED FOR THIS PROCESS.'.                   
077700                                                                          
077800     03  M20.                                                             
077900       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
078000       05 FILLER                PIC X(3)    VALUE                         
078100          'E  '.                                                          
078200       05 M20-IDPROCESS         PIC X(10).                                
078300       05 FILLER                PIC X(47)   VALUE                         
078400          ' HAS ALREADY ENDED.'.                                          
078500       05 FILLER                PIC X(60)   VALUE                         
078600          '   START REQUEST IGNORED FOR THIS PROCESS.'.                   
078700                                                                          
078800     03  M21.                                                             
078900       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
079000       05 FILLER                PIC X(3)    VALUE                         
079100          'I  '.                                                          
079200       05 M21-IDPROCESS         PIC X(10).                                
079300       05 FILLER                PIC X(16)   VALUE                         
079400          ' SUBMITTED FROM '.                                             
079500       05 M21-LIBNAME           PIC X(11).                                
079600       05 FILLER                PIC X(2)    VALUE ' ('.                   
079700       05 M21-ANTAL             PIC Z(3)9.                                
079800       05 FILLER                PIC X(14)   VALUE                         
079900          ' STMTS)     '.                                                 
080000       05 FILLER                PIC X(60)   VALUE SPACE.                  
080100                                                                          
080200     03  M22.                                                             
080300       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
080400       05 FILLER                PIC X(3)    VALUE                         
080500          'W  '.                                                          
080600       05 M22-IDPROCESS         PIC X(10).                                
080700       05 FILLER                PIC X(9)    VALUE ' ALREADY '.            
080800       05 M22-TEXT              PIC X(7)    VALUE SPACE.                  
080900       05 FILLER                PIC X(31)   VALUE                         
081000          '. WILL NOT BE STARTED NOW.    '.                               
081100       05 FILLER                PIC X(60)   VALUE SPACE.                  
081200                                                                          
081300     03  M23.                                                             
081400       05 FILLER        COMP    PIC S9(4)   VALUE +12.                    
081500       05 FILLER                PIC X(60)   VALUE                         
081600          'S  > 10 MANY LEVELS OF PROCESSES.'.                            
081700       05 FILLER                PIC X(60)   VALUE SPACE.                  
081800                                                                          
081900     03  M24.                                                             
082000       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
082100       05 FILLER                PIC X(120)  VALUE SPACE.                  
082200                                                                          
082300     03  M25.                                                             
082400       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
082500       05 FILLER                PIC X(11)   VALUE                         
082600          'E  PR0CESS '.                                                  
082700       05 M25-IDPROCESS         PIC X(10).                                
082800       05 FILLER                PIC X(39)   VALUE                         
082900          ' NOT FOUND.'.                                                  
083000       05 FILLER                PIC X(60)   VALUE SPACE.                  
083100                                                                          
083200     03  M26.                                                             
083300       05 FILLER        COMP    PIC S9(4)   VALUE +16.                    
083400       05 FILLER                PIC X(11)   VALUE                         
083500          'S  SECTION '.                                                  
083600       05 M26-SECTION           PIC X(4).                                 
083700       05 FILLER                PIC X(23)   VALUE                         
083800          ' W980WSPC RETURN CODE: '.                                      
083900       05 M26-RKOD              PIC X(22).                                
084000       05 FILLER                PIC X(60)   VALUE SPACE.                  
084100                                                                          
084200     03  M27.                                                             
084300       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
084400       05 FILLER                PIC X(3)    VALUE                         
084500          'E '.                                                           
084600       05 M27-IDPROCESS         PIC X(10).                                
084700       05 FILLER                PIC X(14)   VALUE                         
084800          ' NOT FOUND IN '.                                               
084900       05 M27-LIBNAME           PIC X(9).                                 
085000       05 FILLER                PIC X(24)   VALUE                         
085100          'JCL LIBRARY.'.                                                 
085200       05 FILLER                PIC X(60)   VALUE SPACE.                  
085300                                                                          
085400     03  M28.                                                             
085500       05 FILLER        COMP    PIC S9(4)   VALUE +16.                    
085600       05 FILLER                PIC X(24)   VALUE                         
085700          'S  CAN NOT CLOSE DDNAME '.                                     
085800       05 M28-DDNAMN            PIC X(8).                                 
085900       05 FILLER                PIC X(28)   VALUE SPACE.                  
086000       05 FILLER                PIC X(60)   VALUE SPACE.                  
086100                                                                          
086200     03  M29.                                                             
086300       05 FILLER        COMP    PIC S9(4)   VALUE +12.                    
086400       05 FILLER                PIC X(3)    VALUE 'S  '.                  
086500       05 M29-IDPROCESS         PIC X(10).                                
086600       05 FILLER                PIC X(28)   VALUE                         
086700          ' HAS AN ILLEGAL START-TYPE: '.                                 
086800       05 M29-STARTTYP          PIC X(19).                                
086900       05 FILLER                PIC X(60)   VALUE SPACE.                  
087000                                                                          
087100     03  M30.                                                             
087200       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
087300       05 FILLER                PIC X(3)    VALUE 'I  '.                  
087400       05 M30-IDPROCESS         PIC X(10).                                
087500       05 FILLER                PIC X(47)   VALUE                         
087600          ' RELEASED FROM JES2 HOLD QUEUE.'.                              
087700       05 FILLER                PIC X(60)   VALUE SPACE.                  
087800                                                                          
087900     03  M31.                                                             
088000       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
088100       05 FILLER                PIC X(3)    VALUE 'I  '.                  
088200       05 M31-IDPROCESS         PIC X(10).                                
088300       05 FILLER                PIC X(47)   VALUE                         
088400          ' HAS BEEN STARTED.'.                                           
088500       05 FILLER                PIC X(60)   VALUE SPACE.                  
088600                                                                          
088700     03  M32.                                                             
088800       05 FILLER        COMP    PIC S9(4)   VALUE +12.                    
088900       05 FILLER                PIC X(3)    VALUE 'S  '.                  
089000       05 FILLER                PIC X(57)   VALUE                         
089100          ' SOP DATA BASE NOT OPEN. FIRST CALL NOT "OPEN".'.              
089200       05 FILLER                PIC X(60)   VALUE SPACE.                  
089300                                                                          
089400     03  M33.                                                             
089500       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
089600       05 FILLER                PIC X(3)    VALUE 'I  '.                  
089700       05 M33-IDPROCESS         PIC X(10).                                
089800       05 FILLER                PIC X(20)   VALUE                         
089900          ' HAS BEEN ACTIVATED '.                                         
090000       05 M33-ORSAK             PIC X(27)   VALUE SPACE.                  
090100       05 FILLER                PIC X(60)   VALUE SPACE.                  
090200                                                                          
090300     03  M34.                                                             
090400       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
090500       05 FILLER                PIC X(3)    VALUE 'I  '.                  
090600       05 M34-IDPROCESS         PIC X(10).                                
090700       05 FILLER                PIC X(21)   VALUE                         
090800          ' HAS BEEN PASSIVATED '.                                        
090900       05 M34-ORSAK             PIC X(26)   VALUE SPACE.                  
091000       05 FILLER                PIC X(3)    VALUE SPACE.                  
091100       05 M34-LONG-ORSAK        PIC X(57)   VALUE SPACE.                  
091200                                                                          
091300     03  M35.                                                             
091400       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
091500       05 FILLER                PIC X(3)    VALUE 'I  '.                  
091600       05 M35-IDPROCESS         PIC X(10).                                
091700       05 FILLER                PIC X(47)   VALUE                         
091800          ' HAS ENDED.'.                                                  
091900       05 FILLER                PIC X(60)   VALUE SPACE.                  
092000                                                                          
092100     03  M36.                                                             
092200       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
092300       05 FILLER                PIC X(3)    VALUE 'I  '.                  
092400       05 M36-IDPROCESS         PIC X(10).                                
092500       05 FILLER                PIC X(47)   VALUE                         
092600          ' IS NO LONGER HELD IN SOP.'.                                   
092700       05 FILLER                PIC X(60)   VALUE SPACE.                  
092800                                                                          
092900     03  M37.                                                             
093000       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
093100       05 FILLER                PIC X(3)    VALUE 'I  '.                  
093200       05 M37-IDPROCESS         PIC X(10).                                
093300       05 FILLER                PIC X(47)   VALUE                         
093400          ' IS NOW HELD IN SOP.'.                                         
093500       05 FILLER                PIC X(60)   VALUE SPACE.                  
093600                                                                          
093700     03  M38.                                                             
093800       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
093900       05 FILLER                PIC X(3)    VALUE 'I  '.                  
094000       05 FILLER                PIC X(8)    VALUE                         
094100          'JCL FOR '.                                                     
094200       05 M38-IDPROCESS         PIC X(10).                                
094300       05 FILLER                PIC X(24)   VALUE                         
094400          ' WILL BE EXTRACTED FROM '.                                     
094500       05 M38-PARENT            PIC X(10).                                
094600       05 FILLER                PIC X(5)    VALUE SPACE.                  
094700       05 FILLER                PIC X(60)   VALUE SPACE.                  
094800                                                                          
094900                                                                          
095000     03  M39.                                                             
095100       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
095200       05 FILLER                PIC X(3)    VALUE 'I  '.                  
095300       05 M39-IDPROCESS         PIC X(10).                                
095400       05 FILLER                PIC X(29)   VALUE                         
095500          ' IS QUEUED FOR ACTIVATION ON '.                                
095600       05 M39-TIAPDAT           PIC 9(6).                                 
095700       05 FILLER                PIC X(12)   VALUE SPACE.                  
095800       05 FILLER                PIC X(3)    VALUE SPACE.                  
095900       05 M39-ORSAK             PIC X(57)   VALUE SPACE.                  
096000                                                                          
096100     03  M40.                                                             
096200       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
096300       05 FILLER                PIC X(3)    VALUE 'I  '.                  
096400       05 M40-IDPROCESS         PIC X(10).                                
096500       05 FILLER                PIC X(30)   VALUE                         
096600          ' IS QUEUED FOR PASSIVATION ON '.                               
096700       05 M40-TIAPDAT           PIC 9(6).                                 
096800       05 FILLER                PIC X(11)   VALUE SPACE.                  
096900       05 FILLER                PIC X(3)    VALUE SPACE.                  
097000       05 M40-ORSAK             PIC X(57)   VALUE SPACE.                  
097100                                                                          
097200     03  M41.                                                             
097300       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
097400       05 FILLER                PIC X(3)    VALUE 'I  '.                  
097500       05 M41-IDPROCESS         PIC X(10).                                
097600       05 FILLER                PIC X(47)   VALUE                         
097700          ' HAS BEEN MULTIPLY ACTIVATED.'.                                
097800       05 FILLER                PIC X(3)    VALUE SPACE.                  
097900       05 M41-ORSAK             PIC X(57)   VALUE SPACE.                  
098000                                                                          
098100     03  M42.                                                             
098200       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
098300       05 FILLER                PIC X(3)    VALUE 'I  '.                  
098400       05 M42-IDPROCESS         PIC X(10).                                
098500       05 FILLER                PIC X(16)   VALUE                         
098600          ' IS PLANNED FOR '.                                             
098700       05 M42-ACTION            PIC X(12)   VALUE SPACE.                  
098800       05 FILLER                PIC X(19)   VALUE SPACE.                  
098900       05 FILLER                PIC X(3)    VALUE SPACE.                  
099000       05 M42-ORSAK             PIC X(57)   VALUE SPACE.                  
099100                                                                          
099200     03  M43.                                                             
099300       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
099400       05 FILLER                PIC X(3)    VALUE 'E  '.                  
099500       05 M43-IDPROCESS         PIC X(10).                                
099600       05 FILLER                PIC X(47)   VALUE                         
099700          ' IS STARTED AND CAN NOT BE PASSIVATED'.                        
099800       05 FILLER                PIC X(3)    VALUE SPACE.                  
099900       05 FILLER                PIC X(57)   VALUE                         
100000          'USE FUNCTION "END" INSTEAD.'.                                  
100100                                                                          
100200     03  M44.                                                             
100300       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
100400       05 FILLER                PIC X(3)    VALUE 'E  '.                  
100500       05 M44-IDPROCESS         PIC X(10).                                
100600       05 FILLER                PIC X(47)   VALUE                         
100700          ' IS WAITING AND CAN NOT BE ENDED.'.                            
100800       05 FILLER                PIC X(3)    VALUE SPACE.                  
100900       05 FILLER                PIC X(57)   VALUE SPACE.                  
101000                                                                          
101100     03  M45.                                                             
101200       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
101300       05 FILLER                PIC X(3)    VALUE                         
101400          'E  '.                                                          
101500       05 M45-IDPROCESS         PIC X(10).                                
101600       05 FILLER                PIC X(47)   VALUE                         
101700          ' IS NOT ACTIVE.'.                                              
101800       05 FILLER                PIC X(60)   VALUE                         
101900          '   START REQUEST IGNORED FOR THIS PROCESS.'.                   
102000                                                                          
102100                                                                          
102200     03  M46.                                                             
102300       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
102400       05 FILLER                PIC X(3)    VALUE 'E  '.                  
102500       05 FILLER                PIC X(09)   VALUE                         
102600          'JCL FOR  '.                                                    
102700       05 M46-IDPROCESS         PIC X(10).                                
102800       05 FILLER                PIC X(18)   VALUE                         
102900          ' WAS NOT FOUND IN '.                                           
103000       05 M46-PARENT            PIC X(10).                                
103100       05 FILLER                PIC X(10)   VALUE SPACE.                  
103200       05 FILLER                PIC X(60)   VALUE SPACE.                  
103300                                                                          
103400     03  M47.                                                             
103500       05 FILLER        COMP    PIC S9(4)   VALUE +4.                     
103600       05 FILLER                PIC X(3)    VALUE 'W  '.                  
103700       05 FILLER                PIC X(7)    VALUE                         
103800          'SYMBOL '.                                                      
103900       05 M47-SYMBOL            PIC X(15).                                
104000       05 FILLER                PIC X(15)   VALUE                         
104100          ' NOT FOUND FOR '.                                              
104200       05 M47-IDPROCESS         PIC X(10).                                
104300       05 FILLER                PIC X(10)   VALUE SPACE.                  
104400       05 FILLER                PIC X(60)   VALUE SPACE.                  
104500                                                                          
104600     03  M48.                                                             
104700       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
104800       05 FILLER                PIC X(3)    VALUE 'E  '.                  
104900       05 M48-ORSAK             PIC X(37).                                
105000       05 M48-IDPROCESS         PIC X(10).                                
105100       05 FILLER                PIC X(10)   VALUE SPACE.                  
105200       05 FILLER                PIC X(60)   VALUE SPACE.                  
105300                                                                          
105400     03  M49.                                                             
105500       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
105600       05 FILLER                PIC X(3)    VALUE 'E  '.                  
105700       05 M49-ORSAK             PIC X(37).                                
105800       05 M49-IDPROCESS         PIC X(10).                                
105900       05 FILLER                PIC X(10)   VALUE SPACE.                  
106000       05 FILLER                PIC X(60)   VALUE SPACE.                  
106100                                                                          
106200     03  M50.                                                             
106300       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
106400       05 FILLER                PIC X(10)   VALUE                         
106500          'E SYMBOL '.                                                    
106600       05 M50-SYMBOL            PIC X(20).                                
106700       05 FILLER                PIC X(30)   VALUE                         
106800          ' HAS NOT BEEN SET.'.                                           
106900       05 FILLER                PIC X(60)   VALUE SPACE.                  
107000                                                                          
107100     03  M51.                                                             
107200       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
107300       05 FILLER                PIC X(10)   VALUE                         
107400          'I  SYMBOL '.                                                   
107500       05 M51-SYMBOL            PIC X(20).                                
107600       05 FILLER                PIC X(30)   VALUE                         
107700          ' HAS BEEN SET.'.                                               
107800       05 FILLER                PIC X(60)   VALUE SPACE.                  
107900                                                                          
108000     03  M52.                                                             
108100       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
108200       05 FILLER                PIC X(10)   VALUE                         
108300          'I SYMBOL '.                                                    
108400       05 M52-SYMBOL            PIC X(20).                                
108500       05 FILLER                PIC X(30)   VALUE                         
108600          ' HAS BEEN DELETED.'.                                           
108700       05 FILLER                PIC X(60)   VALUE SPACE.                  
108800                                                                          
108900     03  M53.                                                             
109000       05 FILLER        COMP    PIC S9(4)   VALUE +12.                    
109100       05 FILLER                PIC X(35)   VALUE                         
109200          'S  NO ORDER/ACTIVATING FOR PROCESS '.                          
109300       05 M53-IDPROCESS         PIC X(10).                                
109400       05 FILLER                PIC X(15)   VALUE SPACE.                  
109500       05 FILLER                PIC X(60)   VALUE                         
109600          ' BECAUSE OF SYNTAX ERROR IN SYMBOL STRING.'.                   
109700                                                                          
109800     03  M54.                                                             
109900       05 FILLER        COMP    PIC S9(4)   VALUE +8.                     
110000       05 FILLER                PIC X(3)    VALUE 'E  '.                  
110100       05 M54-IDPROCESS         PIC X(10).                                
110200       05 FILLER                PIC X(47)   VALUE                         
110300          ' NOT STARTED / HOLDING ANY RESOURCES.'.                        
110400       05 FILLER                PIC X(60)   VALUE SPACE.                  
110500                                                                          
110600     03  M55.                                                             
110700       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
110800       05 FILLER                PIC X(3)    VALUE 'I  '.                  
110900       05 M55-IDPROCESS         PIC X(10).                                
111000       05 FILLER                PIC X(47)   VALUE                         
111100          ' IS NOT HOLDING ANY RESOURCES.'.                               
111200       05 FILLER                PIC X(60)   VALUE SPACE.                  
111300                                                                          
111400     03  M56.                                                             
111500       05 FILLER        COMP    PIC S9(4)   VALUE +0.                     
111600       05 FILLER                PIC X(32)  VALUE                          
111700          'I  RESOURCES HAS BEEN FREED FOR '.                             
111800       05 M56-IDPROCESS         PIC X(10).                                
111900       05 FILLER                PIC X(18)  VALUE SPACE.                   
112000       05 FILLER                PIC X(60)   VALUE SPACE.                  
112100                                                                          
112200 01  M-RED  REDEFINES  M.                                                 
112300     03  FILLER OCCURS 56.                                                
112400       05  MSG-RETCODE         PIC S9(4)    COMP.                         
112500       05  MSG1                PIC X(60).                                 
112600       05  MSG2                PIC X(60).                                 
112700     EJECT                                                                
112800 LINKAGE SECTION.                                                         
112900     SKIP2                                                                
113000 01  LINK-PARM.                                                           
113100*    03  -COPY W98020     -PRE L                                          
113200     EJECT                                                                
113300 PROCEDURE DIVISION  USING LINK-PARM.                                     
113400 STYR SECTION.                                                            
113500     SKIP2                                                                
113600     PERFORM 0-RENSA-PARAMETRAR                                           
113700     IF LSOP20-KDSOPFUNK = '0'                                            
113800       PERFORM X-OPEN-SOPREG                                              
113900     ELSE IF LSOP20-KDSOPFUNK = '9'                                       
114000       PERFORM Y-CLOSE-SOPREG                                             
114100     ELSE                                                                 
114200       PERFORM A-KOLLA-LINK-PARM                                          
114300       IF LSOP20-KDRET > 8 GO TO ERROR-EXIT                               
114400       END-IF                                                             
114500       IF LSOP20-KDSOPFUNK NOT = 'J'                                      
114600         PERFORM B-START-INFO                                             
114700         IF LSOP20-KDRET > 8 GO TO ERROR-EXIT                             
114800         END-IF                                                           
114900       END-IF                                                             
115000       EVALUATE LSOP20-KDSOPFUNK                                          
115100         WHEN 'O' PERFORM C-BESTAELL                                      
115200         WHEN 'C' PERFORM D-AVBESTAELL                                    
115300         WHEN 'S' PERFORM E-STARTA                                        
115400         WHEN 'E' PERFORM F-AVSLUTA                                       
115500         WHEN 'H' PERFORM G-STOPPA                                        
115600         WHEN 'R' PERFORM H-SLAEPP                                        
115700         WHEN 'Z' PERFORM I-ABENDA                                        
115800         WHEN 'I' PERFORM J-HAEMTA-INFO                                   
115900         WHEN 'J' PERFORM K-HAEMTA-MSL-INFO                               
116000         WHEN 'K' PERFORM L-HAEMTA-KALENDER                               
116100         WHEN 'L' PERFORM M-HAEMTA-MSL-KALENDER                           
116200         WHEN 'B' PERFORM N-PLAN-AKT-PASS                                 
116300         WHEN 'A' PERFORM O-AKTIVERA                                      
116400         WHEN 'P' PERFORM P-PASSIVERA                                     
116500         WHEN 'D' PERFORM Q-AVBESTAELL-ANR                                
116600         WHEN 'Y' PERFORM R-HAEMTA-INFO-ANR                               
116700         WHEN 'V' PERFORM V-LAGRA-SYMB-VAERDEN                            
116800         WHEN 'F' PERFORM W-SLAEPP-RESURSER-FRIA                          
116900       END-EVALUATE                                                       
117000       IF LSOP20-KDRET > 8 GO TO ERROR-EXIT END-IF                        
117100       IF LSOP20-KDSOPFUNK NOT = 'J' AND 'L' AND 'B'                      
117200         CALL W980WSPC USING FSAVE WSRKOD                                 
117300       END-IF                                                             
117400       GO TO NO-ERROR                                                     
117500     END-IF                                                               
117600     END-IF                                                               
117700     .                                                                    
117800 ERROR-EXIT.                                                              
117900     IF LSOP20-KDSOPFUNK NOT = 'J' AND 'L' AND 'B'                        
118000       CALL W980WSPC USING FQUIT WSRKOD                                   
118100     END-IF                                                               
118200     MOVE LSOP20-KDRET TO RETURN-CODE                                     
118300     GOBACK                                                               
118400     .                                                                    
118500 NO-ERROR.                                                                
118600     MOVE LSOP20-KDRET TO RETURN-CODE                                     
118700     GOBACK                                                               
118800     .                                                                    
118900     EJECT                                                                
119000 0-RENSA-PARAMETRAR SECTION.                                              
119100     SKIP2                                                                
119200     MOVE ZERO TO LSOP20-KDRET                                            
119300                  LSOP20-KDMEDD                                           
119400                  LSOP20-TIAPDAT-SENAST                                   
119500                  LSOP20-TIEXDAT-SENAST                                   
119600                  LSOP20-TIMINUT-START                                    
119700                  LSOP20-TIMINUT-STOPP                                    
119800                  LSOP20-TIEXEC-SENAST                                    
119900                  LSOP20-TIEXEC-MEDEL                                     
120000                  LSOP20-TITMPDUR                                         
120100*                                                                         
120200     MOVE SPACE TO LSOP20-KDPROCSTRT                                      
120300                  LSOP20-KDPROCSTAT                                       
120400                  LSOP20-TEPRED                                           
120500                  LSOP20-TESUCC                                           
120600                  LSOP20-TEACTQ                                           
120700                  LSOP20-FLHOLD                                           
120800                  LSOP20-TEATTN                                           
120900                  LSOP20-IDPROCESS-PARENT                                 
121000                  LSOP20-KDPROCMTYP                                       
121100                  LSOP20-KDPROCPRIO                                       
121200                  LSOP20-KDVOUT                                           
121300                  LSOP20-TERES                                            
121400*                                                                         
121500     MOVE LSOP20-FLBATCH TO SPAR-FLBATCH                                  
121600                                                                          
121700* -- FETCH USERID OF CALLER                                               
121800     CALL W980USER USING WTOP-CLIENT                                      
121900     .                                                                    
122000     EJECT                                                                
122100 A-KOLLA-LINK-PARM SECTION.                                               
122200     SKIP2                                                                
122300*----- KONTROLLERA PROCESSNAMN                                            
122400     IF LSOP20-KDSOPFUNK NOT = 'K' AND 'L'                                
122500       IF LSOP20-IDPROCESS = SPACE                                        
122600         MOVE 1 TO MSG-NR                                                 
122700         PERFORM S90-DISPLAY-MSG                                          
122800       ELSE                                                               
122900         MOVE LSOP20-IDPROCESS TO HP-IDPROCESS                            
123000         MOVE ZERO TO HP-LENGD                                            
123100         INSPECT HP-IDPROCESS TALLYING HP-LENGD                           
123200           FOR CHARACTERS BEFORE INITIAL SPACE                            
123300         ADD 2 TO HP-LENGD                                                
123400       END-IF                                                             
123500     END-IF                                                               
123600                                                                          
123700*----- KONTROLLERA FUNKTIONSKOD                                           
123800* ----------FUNK 'OCAPSEHRIJZKLB'                                         
123900     IF LSOP20-KDSOPFUNK NOT = 'O' AND 'C' AND 'A' AND 'P' AND            
124000         'S' AND 'E' AND 'H' AND 'R' AND 'I' AND 'J' AND 'D' AND          
124100         'Z' AND 'K' AND 'L' AND 'B' AND 'V' AND 'F' AND 'Y'              
124200       MOVE 2 TO MSG-NR                                                   
124300       MOVE LSOP20-KDSOPFUNK TO M2-SOPFUNC                                
124400       PERFORM S90-DISPLAY-MSG                                            
124500     END-IF                                                               
124600                                                                          
124700* ----------FUNK 'OCKLB'                                                  
124800     IF LSOP20-KDSOPFUNK = 'O' OR 'C' OR 'K' OR 'L' OR 'B'                
124900       IF LSOP20-TIAPDAT NOT NUMERIC                                      
125000         MOVE 3 TO MSG-NR                                                 
125100         PERFORM S90-DISPLAY-MSG                                          
125200       END-IF                                                             
125300     END-IF                                                               
125400                                                                          
125500* ----------FUNK 'DY'                                                     
125600     IF LSOP20-KDSOPFUNK = 'D' OR 'Y'                                     
125700       MOVE LSOP20-KDANR TO P-ANR                                         
125800     END-IF                                                               
125900     .                                                                    
126000     EJECT                                                                
126100 B-START-INFO   SECTION.                                                  
126200     SKIP2                                                                
126300     IF OPEN-GJORD = JA                                                   
126400       MOVE NEJ TO WAIT-FORE-REL                                          
126500       MOVE JA TO RENSA-ATTN                                              
126600       CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                     
126700            NUDAT-NAMN-PARM  NUDAT-PARM                                   
126800       IF WSRKOD = SPACE                                                  
126900         MOVE NUDAT TO AKT-NUDAT                                          
127000* ----------FUNK 'OCKLB'                                                  
127100         IF LSOP20-KDSOPFUNK = 'O' OR 'C' OR 'K' OR 'L' OR 'B'            
127200                                   OR 'D'                                 
127300           IF (LSOP20-KDSOPFUNK = 'O' OR 'C')                             
127400           AND (LSOP20-TIAPDAT = ZERO OR +1)                              
127500             MOVE HP-PARM TO P-PARM                                       
127600             PERFORM S10-AKTIVERINGS-DATUM                                
127700             IF KDPROCSTAT = WAITING-STATUS OR STARTED-STATUS             
127800               IF LSOP20-TIAPDAT = ZERO                                   
127900                 MOVE HP-TIAPDAT TO BEGAERD-TIAPDAT                       
128000               ELSE                                                       
128100                 MOVE HP-TIAPDAT TO AKT-NUDAT                             
128200                 PERFORM BB-NAESTA-DATUM                                  
128300               END-IF                                                     
128400             ELSE                                                         
128500               IF LSOP20-TIAPDAT = ZERO                                   
128600                 MOVE NUDAT TO BEGAERD-TIAPDAT                            
128700               ELSE                                                       
128800                 PERFORM BB-NAESTA-DATUM                                  
128900               END-IF                                                     
129000             END-IF                                                       
129100           ELSE                                                           
129200             IF LSOP20-TIAPDAT = ZERO                                     
129300               MOVE NUDAT TO BEGAERD-TIAPDAT                              
129400             ELSE IF LSOP20-TIAPDAT = +1                                  
129500               PERFORM BB-NAESTA-DATUM                                    
129600             ELSE                                                         
129700               PERFORM BA-KOLLA-RIMLIGT-DATUM                             
129800             END-IF                                                       
129900             END-IF                                                       
130000           END-IF                                                         
130100         ELSE                                                             
130200           MOVE NUDAT TO BEGAERD-TIAPDAT                                  
130300         END-IF                                                           
130400       ELSE                                                               
130500         MOVE WSRKOD TO M6-RKOD                                           
130600         MOVE 6 TO MSG-NR                                                 
130700         PERFORM S90-DISPLAY-MSG                                          
130800       END-IF                                                             
130900                                                                          
131000       IF LSOP20-KDSOPFUNK NOT = 'K' AND 'L' AND 'J' AND 'B'              
131100                                     AND 'D' AND 'Y'                      
131200         MOVE HP-PARM TO NAMN-PARM                                        
131300         PERFORM S80-HAEMTA-PROCESS-INFO                                  
131400                                                                          
131500*        -- SPARA INFO OM HUVUD-PROCESSEN                                 
131600         MOVE KDPROCSTAT TO HP-KDPROCSTAT                                 
131700         MOVE TIAPDAT TO HP-TIAPDAT                                       
131800         MOVE ZERO TO PAR-HP-LENGD                                        
131900         MOVE SPACE TO PAR-HP-IDPROCESS                                   
132000         CALL W980WSPC USING FGETF WSRKOD HP-PARM PARENT-PARM             
132100               PAR-HP-PARM                                                
132200       END-IF                                                             
132300     ELSE                                                                 
132400       MOVE 32 TO MSG-NR                                                  
132500       PERFORM S90-DISPLAY-MSG                                            
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900 BA-KOLLA-RIMLIGT-DATUM  SECTION.                                         
133000     SKIP2                                                                
133100     MOVE LSOP20-TIAPDAT TO DATUM-PACKAT                                  
133200     CALL W980WSPC USING FTEST WSRKOD KALENDER-PARM                       
133300           DATUM-LIST-PARM DATUM-PARM                                     
133400     IF WSRKOD = SPACE OR LSOP20-KDSOPFUNK = 'D'                          
133500       MOVE LSOP20-TIAPDAT TO BEGAERD-TIAPDAT                             
133600     ELSE                                                                 
133700        MOVE DATUM-PACKAT TO DATUM-DISPLAY                                
133800        MOVE DATUM-DISPLAY TO M4-DATUM                                    
133900        MOVE 4 TO MSG-NR                                                  
134000        PERFORM S90-DISPLAY-MSG                                           
134100     END-IF                                                               
134200     .                                                                    
134300     EJECT                                                                
134400 BB-NAESTA-DATUM         SECTION.                                         
134500     SKIP2                                                                
134600     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
134700           DATUM-LIST-PARM  DATUM-PARM                                    
134800     MOVE DATUM-PACKAT      TO TMP1-YYMMDD                                
134900     MOVE AKT-NUDAT         TO TMP2-YYMMDD                                
135000     PERFORM WY2000P1                                                     
135100     PERFORM UNTIL WSRKOD NOT = SPACE OR TMP1-YYMMDD > TMP2-YYMMDD        
135200       CALL W980WSPC USING FGETN WSRKOD KALENDER-PARM                     
135300            DATUM-LIST-PARM DATUM-PARM                                    
135400       MOVE DATUM-PACKAT      TO TMP1-YYMMDD                              
135500       MOVE AKT-NUDAT         TO TMP2-YYMMDD                              
135600       PERFORM WY2000P1                                                   
135700     END-PERFORM                                                          
135800     IF WSRKOD = SPACE AND                                                
135900         LSOP20-TECATALOG NOT = SPACE AND LOW-VALUE                       
136000       MOVE LSOP20-TECATALOG TO DATA-VAERDE                               
136100       MOVE ZERO TO DATA-LENGD                                            
136200       INSPECT DATA-VAERDE TALLYING DATA-LENGD                            
136300         FOR CHARACTERS BEFORE INITIAL SPACE                              
136400       ADD 2 TO DATA-LENGD                                                
136500       CALL W980WSPC USING FTEST WSRKOD DATUM-PARM                        
136600            CATALOG-PARM DATA-PARM                                        
136700       MOVE NEJ TO DATUM-SLUT                                             
136800       PERFORM UNTIL WSRKOD = SPACE OR DATUM-SLUT = JA                    
136900         CALL W980WSPC USING FGETN WSRKOD KALENDER-PARM                   
137000              DATUM-LIST-PARM DATUM-PARM                                  
137100         IF WSRKOD = SPACE                                                
137200           CALL W980WSPC USING FTEST WSRKOD DATUM-PARM                    
137300                 CATALOG-PARM DATA-PARM                                   
137400         ELSE                                                             
137500           MOVE JA TO DATUM-SLUT                                          
137600         END-IF                                                           
137700       END-PERFORM                                                        
137800     END-IF                                                               
137900     IF WSRKOD = SPACE                                                    
138000       MOVE DATUM-PACKAT TO BEGAERD-TIAPDAT                               
138100     ELSE                                                                 
138200        MOVE DATUM-PACKAT TO DATUM-DISPLAY                                
138300        MOVE DATUM-DISPLAY TO M4-DATUM                                    
138400        MOVE 4 TO MSG-NR                                                  
138500        PERFORM S90-DISPLAY-MSG                                           
138600     END-IF                                                               
138700     .                                                                    
138800     EJECT                                                                
138900 C-BESTAELL     SECTION.                                                  
139000     SKIP2                                                                
139100     MOVE SPACE TO M33-ORSAK                                              
139200     MOVE HP-PARM TO P-PARM                                               
139300     MOVE NEJ TO SYMB-FEL                                                 
139400     MOVE BEGAERD-TIAPDAT   TO TMP1-YYMMDD                                
139500     MOVE NUDAT             TO TMP2-YYMMDD                                
139600     PERFORM WY2000P1                                                     
139700     IF  TMP1-YYMMDD > TMP2-YYMMDD                                        
139800*       -- FRAMTIDA DATUM                                                 
139900        MOVE 'ACTIVATION DATE GREATER THAN CURRENT DATE.'                 
140000              TO M39-ORSAK                                                
140100        PERFORM CA-BESTAELLNINGSKOE                                       
140200     ELSE                                                                 
140300        PERFORM S10-AKTIVERINGS-DATUM                                     
140400        MOVE BEGAERD-TIAPDAT   TO TMP1-YYMMDD                             
140500        MOVE HP-TIAPDAT        TO TMP2-YYMMDD                             
140600        PERFORM WY2000P1                                                  
140700        IF  TMP1-YYMMDD > TMP2-YYMMDD                                     
140800*          -- EN NY BESTÄLLNING. AKTIVERA OM FÖRÄLDER SAKNAS              
140900*          -- OCH PROCESSEN EJ REDAN AKTIV, ANNARS BESTÄLLNINGSKÖ         
141000           IF PAR-HP-IDPROCESS = SPACE                                    
141100           AND (HP-KDPROCSTAT = ENDED-STATUS OR  PASSIVE-STATUS)          
141200              PERFORM S70-LAGRA-ANR-SYMBOLER                              
141300              IF SYMB-FEL = JA                                            
141400                MOVE P-IDPROCESS TO M53-IDPROCESS                         
141500                MOVE 53 TO MSG-NR                                         
141600                PERFORM S90-DISPLAY-MSG                                   
141700              ELSE                                                        
141800                MOVE 'A' TO AKT-PASS                                      
141900                PERFORM S1-TOP-DOWN-AKT-PASS                              
142000                IF STARTA = JA                                            
142100                   PERFORM S2-TOP-DOWN-STARTA                             
142200                END-IF                                                    
142300              END-IF                                                      
142400           ELSE                                                           
142500              MOVE 'ACTIVATION FROM YESTERDAY NOT YET FINISHED.'          
142600              TO M39-ORSAK                                                
142700              IF PAR-HP-IDPROCESS NOT = SPACE                             
142800                MOVE PAR-HP-PARM TO NAMN-PARM                             
142900                PERFORM S80-HAEMTA-PROCESS-INFO                           
143000                IF KDPROCSTAT = ENDED-STATUS OR PASSIVE-STATUS            
143100                  MOVE 'PARENT NOT YET ACTIVE' TO M39-ORSAK               
143200                END-IF                                                    
143300              END-IF                                                      
143400              PERFORM CA-BESTAELLNINGSKOE                                 
143500           END-IF                                                         
143600        ELSE IF BEGAERD-TIAPDAT = HP-TIAPDAT                              
143700*          -- SAMMA DATUM SOM SENAST. LÄGG PÅ KÖ                          
143800*          -- OM PROCESSEN OCH DESS FÖRÄLDER ÄR INAKTIV,                  
143900*          -- ANNARS AKTIVERA.                                            
144000           IF PAR-HP-IDPROCESS NOT = SPACE                                
144100             MOVE PAR-HP-PARM TO NAMN-PARM                                
144200             PERFORM S80-HAEMTA-PROCESS-INFO                              
144300           END-IF                                                         
144400           IF (HP-KDPROCSTAT = ENDED-STATUS OR PASSIVE-STATUS)            
144500           AND PAR-HP-IDPROCESS NOT = SPACE                               
144600           AND (KDPROCSTAT = ENDED-STATUS OR PASSIVE-STATUS)              
144700              MOVE 'PARENT NOT YET ACTIVE.' TO M39-ORSAK                  
144800              PERFORM CA-BESTAELLNINGSKOE                                 
144900           ELSE                                                           
145000              PERFORM S70-LAGRA-ANR-SYMBOLER                              
145100              IF SYMB-FEL = JA                                            
145200                MOVE P-IDPROCESS TO M53-IDPROCESS                         
145300                MOVE 53 TO MSG-NR                                         
145400                PERFORM S90-DISPLAY-MSG                                   
145500              ELSE                                                        
145600                MOVE 'A' TO AKT-PASS                                      
145700                PERFORM S1-TOP-DOWN-AKT-PASS                              
145800                IF STARTA = JA                                            
145900                   PERFORM S2-TOP-DOWN-STARTA                             
146000                END-IF                                                    
146100              END-IF                                                      
146200           END-IF                                                         
146300        ELSE                                                              
146400          MOVE 7 TO MSG-NR                                                
146500          MOVE HP-TIAPDAT TO M7-TIAPDAT                                   
146600          MOVE HP-IDPROCESS TO M7-IDPROCESS                               
146700          PERFORM S90-DISPLAY-MSG                                         
146800        END-IF                                                            
146900        END-IF                                                            
147000     END-IF                                                               
147100     .                                                                    
147200     EJECT                                                                
147300 CA-BESTAELLNINGSKOE    SECTION.                                          
147400     SKIP2                                                                
147500*    -- LÄGG UPP PROCESSEN I BESTÄLLNINGS-KÖN FÖR ETT VISST               
147600*    -- DATUM, ELLER TA BORT DEN UR AVBESTÄLLNINGS-KÖN OM DEN             
147700*    -- FINNS DÄR.                                                        
147800                                                                          
147900     MOVE HP-PARM TO P-PARM                                               
148000     CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM                
148100          PASSQ-PARM P-PARM                                               
148200     IF WSRKOD = SPACE                                                    
148300        PERFORM S23-TA-PASSIVERING-UR-KOE                                 
148400        MOVE 8 TO MSG-NR                                                  
148500        MOVE BEGAERD-TIAPDAT TO M8-TIAPDAT                                
148600        MOVE P-IDPROCESS TO M8-IDPROCESS                                  
148700        PERFORM S90-DISPLAY-MSG                                           
148800        GO TO NO-ADMIT                                                    
148900     ELSE                                                                 
149000*      --  ANTAG ATT BESTÄLLNING EJ FÅR KÖAS                              
149100       CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM              
149200            ACTQ-PARM P-PARM                                              
149300                                                                          
149400*      --  BESTÄLLNING FÅR KÖAS OM INGET TIDIGARE ÄR UPPKÖAT              
149500       IF WSRKOD NOT = SPACE GO TO ADMIT END-IF                           
149600                                                                          
149700*      -- BESTÄLLNINGEN FÅR KÖAS OM MULTIPLA ÄR TILLÅTET                  
149800       CALL W980WSPC USING FTEST WSRKOD P-PARM                            
149900            EJ-SAMTIDIGT-PARM P-PARM                                      
150000       IF  WSRKOD = SPACE GO TO ADMIT END-IF                              
150100                                                                          
150200*      -- BESTÄLLNING FÅR INTE KÖAS UPP                                   
150300       MOVE 9 TO MSG-NR                                                   
150400       MOVE BEGAERD-TIAPDAT TO M9-TIAPDAT                                 
150500       MOVE P-IDPROCESS TO M9-IDPROCESS                                   
150600       PERFORM S90-DISPLAY-MSG                                            
150700       GO TO NO-ADMIT                                                     
150800     END-IF                                                               
150900       .                                                                  
151000 ADMIT.                                                                   
151100*      -- BESTÄLLNING FÅR KÖAS UPP                                        
151200     PERFORM S70-LAGRA-ANR-SYMBOLER                                       
151300     IF SYMB-FEL = JA                                                     
151400       MOVE P-IDPROCESS TO M53-IDPROCESS                                  
151500       MOVE 53 TO MSG-NR                                                  
151600       PERFORM S90-DISPLAY-MSG                                            
151700     ELSE                                                                 
151800       PERFORM S20-KOEA-AKTIVERING                                        
151900     END-IF                                                               
152000     .                                                                    
152100 NO-ADMIT.                                                                
152200     CONTINUE                                                             
152300     .                                                                    
152400     EJECT                                                                
152500 D-AVBESTAELL  SECTION.                                                   
152600     SKIP2                                                                
152700     MOVE SPACE TO M34-ORSAK                                              
152800     MOVE SPACE TO M34-LONG-ORSAK                                         
152900     MOVE HP-PARM TO P-PARM                                               
153000     MOVE BEGAERD-TIAPDAT   TO TMP1-YYMMDD                                
153100     MOVE NUDAT             TO TMP2-YYMMDD                                
153200     PERFORM WY2000P1                                                     
153300     IF  TMP1-YYMMDD > TMP2-YYMMDD                                        
153400       MOVE 'PASSIVATION DATE GREATER THAN CURRENT DATE.'                 
153500       TO M40-ORSAK                                                       
153600       PERFORM DA-AVBESTAELLNINGSKOE                                      
153700     ELSE                                                                 
153800       PERFORM S10-AKTIVERINGS-DATUM                                      
153900       MOVE BEGAERD-TIAPDAT   TO TMP1-YYMMDD                              
154000       MOVE HP-TIAPDAT        TO TMP2-YYMMDD                              
154100       PERFORM WY2000P1                                                   
154200       IF  TMP1-YYMMDD > TMP2-YYMMDD                                      
154300         IF PAR-HP-IDPROCESS = SPACE                                      
154400         AND (HP-KDPROCSTAT = ENDED-STATUS OR  PASSIVE-STATUS)            
154500           MOVE 'P' TO AKT-PASS                                           
154600           PERFORM S1-TOP-DOWN-AKT-PASS                                   
154700         ELSE                                                             
154800           MOVE 'ACTIVATION FROM YESTERDAY NOT YET FINISHED.'             
154900           TO M40-ORSAK                                                   
155000           IF PAR-HP-IDPROCESS NOT = SPACE                                
155100             MOVE PAR-HP-PARM TO NAMN-PARM                                
155200             PERFORM S80-HAEMTA-PROCESS-INFO                              
155300             IF KDPROCSTAT = ENDED-STATUS OR PASSIVE-STATUS               
155400               MOVE 'PARENT NOT YET ACTIVE' TO M40-ORSAK                  
155500             END-IF                                                       
155600           END-IF                                                         
155700           PERFORM DA-AVBESTAELLNINGSKOE                                  
155800         END-IF                                                           
155900       ELSE IF BEGAERD-TIAPDAT = HP-TIAPDAT                               
156000         IF PAR-HP-IDPROCESS NOT = SPACE                                  
156100           MOVE PAR-HP-PARM TO NAMN-PARM                                  
156200           PERFORM S80-HAEMTA-PROCESS-INFO                                
156300         END-IF                                                           
156400         IF (HP-KDPROCSTAT = ENDED-STATUS OR PASSIVE-STATUS)              
156500           MOVE 'PROCESS NOT ACTIVE.' TO M40-ORSAK                        
156600           PERFORM DA-AVBESTAELLNINGSKOE                                  
156700         ELSE IF PAR-HP-IDPROCESS NOT = SPACE                             
156800         AND (KDPROCSTAT = ENDED-STATUS OR PASSIVE-STATUS)                
156900           MOVE 'PARENT NOT YET ACTIVE.' TO M40-ORSAK                     
157000           PERFORM DA-AVBESTAELLNINGSKOE                                  
157100         ELSE                                                             
157200           MOVE 'P' TO AKT-PASS                                           
157300           PERFORM S1-TOP-DOWN-AKT-PASS                                   
157400         END-IF                                                           
157500         END-IF                                                           
157600       ELSE                                                               
157700          MOVE HP-TIAPDAT TO M7-TIAPDAT                                   
157800          MOVE HP-IDPROCESS TO M7-IDPROCESS                               
157900          MOVE 7 TO MSG-NR                                                
158000          PERFORM S90-DISPLAY-MSG                                         
158100       END-IF                                                             
158200       END-IF                                                             
158300     END-IF                                                               
158400     .                                                                    
158500     EJECT                                                                
158600 DA-AVBESTAELLNINGSKOE    SECTION.                                        
158700     SKIP2                                                                
158800*    -- LÄGG UPP PROCESSEN I AVBESTÄLLNINGS-KÖN FÖR ETT VISST             
158900*    -- DATUM, ELLER TA BORT DEN UR BESTÄLLNINGS-KÖN OM DEN               
159000*    -- FINNS DÄR.                                                        
159100                                                                          
159200     MOVE HP-PARM TO P-PARM                                               
159300     CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM                
159400          ACTQ-PARM P-PARM                                                
159500     IF WSRKOD = SPACE                                                    
159600        PERFORM S21-TA-AKTIVERING-UR-KOE                                  
159700        MOVE BEGAERD-TIAPDAT TO M10-TIAPDAT                               
159800        MOVE P-IDPROCESS TO M10-IDPROCESS                                 
159900        MOVE 10 TO MSG-NR                                                 
160000        PERFORM S90-DISPLAY-MSG                                           
160100     ELSE                                                                 
160200       CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM              
160300            PASSQ-PARM P-PARM                                             
160400       IF WSRKOD = SPACE                                                  
160500          MOVE 11 TO MSG-NR                                               
160600          MOVE BEGAERD-TIAPDAT TO M11-TIAPDAT                             
160700          MOVE P-IDPROCESS TO M11-IDPROCESS                               
160800          PERFORM S90-DISPLAY-MSG                                         
160900       ELSE                                                               
161000          PERFORM S22-KOEA-PASSIVERING                                    
161100       END-IF                                                             
161200     END-IF                                                               
161300     .                                                                    
161400     EJECT                                                                
161500 E-STARTA       SECTION.                                                  
161600     SKIP2                                                                
161700     MOVE NUDAT TO BEGAERD-TIAPDAT                                        
161800     MOVE HP-PARM TO P-PARM                                               
161900     IF HP-KDPROCSTAT = WAITING-STATUS OR STARTED-STATUS                  
162000       IF HP-KDPROCSTAT = STARTED-STATUS                                  
162100         PERFORM S55-UNDERSOEK-EXTRAHERA                                  
162200         IF EXTRAHERA = JA                                                
162300           PERFORM S56-EXTRAHERA-JCL                                      
162400         END-IF                                                           
162500       END-IF                                                             
162600       PERFORM S2-TOP-DOWN-STARTA                                         
162700     ELSE                                                                 
162800       MOVE 45 TO MSG-NR                                                  
162900       MOVE P-IDPROCESS TO M45-IDPROCESS                                  
163000       PERFORM S90-DISPLAY-MSG                                            
163100     END-IF                                                               
163200     .                                                                    
163300     EJECT                                                                
163400 F-AVSLUTA      SECTION.                                                  
163500     SKIP2                                                                
163600     MOVE HP-PARM TO P-PARM                                               
163700     PERFORM S10-AKTIVERINGS-DATUM                                        
163800     MOVE HP-TIAPDAT TO BEGAERD-TIAPDAT                                   
163900                                                                          
164000     IF HP-KDPROCSTAT = STARTED-STATUS                                    
164100         PERFORM S3-BOTTOM-UP-AVSLUTA                                     
164200     ELSE IF HP-KDPROCSTAT = WAITING-STATUS                               
164300         MOVE 44 TO MSG-NR                                                
164400         MOVE HP-IDPROCESS TO M44-IDPROCESS                               
164500         PERFORM S90-DISPLAY-MSG                                          
164600     ELSE                                                                 
164700         MOVE 13 TO MSG-NR                                                
164800         MOVE HP-IDPROCESS TO M13-IDPROCESS                               
164900         PERFORM S90-DISPLAY-MSG                                          
165000     END-IF                                                               
165100     END-IF                                                               
165200     .                                                                    
165300     EJECT                                                                
165400 G-STOPPA     SECTION.                                                    
165500     SKIP2                                                                
165600     MOVE 0 TO SIX                                                        
165700     MOVE HP-PARM TO P-PARM                                               
165800                                                                          
165900     PERFORM S4-TOP-DOWN-STOPPA                                           
166000     .                                                                    
166100     EJECT                                                                
166200 H-SLAEPP     SECTION.                                                    
166300     SKIP2                                                                
166400     MOVE 0 TO SIX                                                        
166500     MOVE HP-PARM TO P-PARM                                               
166600     MOVE JA TO FOERSTA                                                   
166700     PERFORM HA-SUB-SLAEPP                                                
166800     IF FORTSAETT = JA                                                    
166900       PERFORM S30-PUSH-PROCESS-STACK                                     
167000       MOVE SPACE TO P-IDPROCESS                                          
167100       CALL W980WSPC USING FGETF WSRKOD STACK-IDPROCESS-PARM (SIX)        
167200            CONTAINS-PARM  P-PARM                                         
167300                                                                          
167400       PERFORM UNTIL SIX NOT > 0                                          
167500         PERFORM UNTIL WSRKOD NOT = SPACE                                 
167600           PERFORM HA-SUB-SLAEPP                                          
167700           IF FORTSAETT = JA                                              
167800*            -- TA 1:A BARN                                               
167900             PERFORM S30-PUSH-PROCESS-STACK                               
168000             MOVE SPACE TO P-IDPROCESS                                    
168100             CALL W980WSPC USING FGETF WSRKOD                             
168200                  STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM         
168300           ELSE                                                           
168400*            -- TA NÄSTA SYSKON                                           
168500             MOVE SPACE TO P-IDPROCESS                                    
168600             CALL W980WSPC USING FGETN WSRKOD                             
168700                  STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM         
168800           END-IF                                                         
168900         END-PERFORM                                                      
169000         PERFORM S31-POP-PROCESS-STACK                                    
169100         IF SIX > 0                                                       
169200           MOVE SPACE TO P-IDPROCESS                                      
169300           CALL W980WSPC USING FGETN WSRKOD                               
169400                STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM           
169500         END-IF                                                           
169600       END-PERFORM                                                        
169700                                                                          
169800       MOVE HP-PARM TO P-PARM                                             
169900       PERFORM S2-TOP-DOWN-STARTA                                         
170000     END-IF                                                               
170100     .                                                                    
170200     EJECT                                                                
170300 HA-SUB-SLAEPP  SECTION.                                                  
170400     SKIP2                                                                
170500     MOVE JA TO FORTSAETT                                                 
170600     CALL W980WSPC USING FTEST WSRKOD P-PARM HOLD-PARM NULL-PARM          
170700     IF WSRKOD NOT = SPACE                                                
170800       MOVE 15 TO MSG-NR                                                  
170900       MOVE P-IDPROCESS TO M15-IDPROCESS                                  
171000       PERFORM S90-DISPLAY-MSG                                            
171100       MOVE NEJ TO FORTSAETT                                              
171200     ELSE                                                                 
171300       CALL W980WSPC USING FDELK WSRKOD P-PARM HOLD-PARM                  
171400       IF WSRKOD  = SPACE                                                 
171500         MOVE P-PARM TO NAMN-PARM                                         
171600         PERFORM S80-HAEMTA-PROCESS-INFO                                  
171700         IF KDPROCSTAT NOT = STARTED-STATUS                               
171800           PERFORM S40-RENSA-PROCESS                                      
171900         END-IF                                                           
172000         IF FOERSTA = JA                                                  
172100           MOVE 36 TO MSG-NR                                              
172200           MOVE P-IDPROCESS TO M36-IDPROCESS                              
172300           PERFORM S90-DISPLAY-MSG                                        
172400           MOVE NEJ TO FOERSTA                                            
172500         END-IF                                                           
172600       ELSE                                                               
172700         MOVE 26 TO MSG-NR                                                
172800         MOVE 'HA' TO M26-SECTION                                         
172900         MOVE WSRKOD TO M26-RKOD                                          
173000         PERFORM S90-DISPLAY-MSG                                          
173100         GO TO ERROR-EXIT                                                 
173200       END-IF                                                             
173300     END-IF                                                               
173400     .                                                                    
173500     EJECT                                                                
173600 I-ABENDA     SECTION.                                                    
173700     SKIP2                                                                
173800     ACCEPT KLOCKSLAG FROM TIME                                           
173900     MOVE SPACE TO ATTN-TEXT ATTR-VAERDE                                  
174000     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
174100                     USING-PARM ATTR-PARM                                 
174200     IF WSRKOD = SPACE                                                    
174300       CALL W980WSPC USING FTEST WSRKOD ATTR-PARM                         
174400                 USED-PARM HP-PARM                                        
174500       IF WSRKOD = SPACE                                                  
174600         STRING 'ABEND ' KLOCKSLAG-HH ':' KLOCKSLAG-MM                    
174700           ' RES'                                                         
174800           DELIMITED BY SIZE INTO ATTN-TEXT                               
174900       ELSE                                                               
175000         STRING 'ABEND ' KLOCKSLAG-HH ':' KLOCKSLAG-MM                    
175100           DELIMITED BY SIZE INTO ATTN-TEXT                               
175200       END-IF                                                             
175300     ELSE                                                                 
175400       STRING 'ABEND ' KLOCKSLAG-HH ':' KLOCKSLAG-MM                      
175500         DELIMITED BY SIZE INTO ATTN-TEXT                                 
175600     END-IF                                                               
175700                                                                          
175800     CALL W980WSPC USING FSET WSRKOD HP-PARM                              
175900          ATTN-PARM ATTN-TEXT-PARM                                        
176000     IF WSRKOD = SPACE                                                    
176100       MOVE HP-PARM TO P-PARM                                             
176200       PERFORM S25-LAEGG-UPP-I-STARTED-LIST                               
176300       PERFORM IA-STARTA-EV-ABEND-JOB                                     
176400     ELSE                                                                 
176500       MOVE 26 TO MSG-NR                                                  
176600       MOVE 'I' TO M26-SECTION                                            
176700       MOVE WSRKOD TO M26-RKOD                                            
176800       PERFORM S90-DISPLAY-MSG                                            
176900       GO TO ERROR-EXIT                                                   
177000     END-IF                                                               
177100     .                                                                    
177200     EJECT                                                                
177300 IA-STARTA-EV-ABEND-JOB   SECTION.                                        
177400     SKIP2                                                                
177500     MOVE HP-PARM TO P-PARM                                               
177600     MOVE NEJ TO ABEND-JOB-FOUND                                          
177700     MOVE SPACE TO WSRKOD                                                 
177800                                                                          
177900     PERFORM UNTIL WSRKOD NOT = SPACE OR ABEND-JOB-FOUND = JA             
178000       MOVE SPACE TO DATA-VAERDE                                          
178100       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
178200              ABEND-JOB-PARM DATA-PARM                                    
178300       IF WSRKOD = SPACE                                                  
178400         MOVE JA TO ABEND-JOB-FOUND                                       
178500       ELSE                                                               
178600         CALL W980WSPC USING FGETF WSRKOD P-PARM                          
178700             PARENT-PARM DATA-PARM                                        
178800         MOVE DATA-PARM TO P-PARM                                         
178900       END-IF                                                             
179000     END-PERFORM                                                          
179100                                                                          
179200     IF ABEND-JOB-FOUND = JA                                              
179300       MOVE HP-PARM   TO AB-PARM                                          
179400       MOVE DATA-PARM TO PROB-CASE-PARM                                   
179500       PERFORM IAA-PROBLEM-CASE-DATA                                      
179600                                                                          
179700*      -- SUBMIT ABEND-JOB (PROBLEM CASE JOB)                             
179800       MOVE PROB-CASE-PARM TO HP-PARM                                     
179900       PERFORM B-START-INFO                                               
180000       PERFORM C-BESTAELL                                                 
180100       MOVE SPACE TO LSOP20-TESYMBV                                       
180200     END-IF                                                               
180300     .                                                                    
180400     EJECT                                                                
180500 IAA-PROBLEM-CASE-DATA  SECTION.                                          
180600     SKIP2                                                                
180700     MOVE AB-PARM TO P-PARM                                               
180800     MOVE FUNCTION CURRENT-DATE TO WCDATE                                 
180900     MOVE WCDATE (1:4)         TO WTS-CCYY                                
181000     MOVE WCDATE (5:2)         TO WTS-MM                                  
181100     MOVE WCDATE (7:2)         TO WTS-DD                                  
181200     MOVE WCDATE (9:2)         TO WTS-TIM                                 
181300     MOVE WCDATE (11:2)        TO WTS-MIN                                 
181400                                                                          
181500     MOVE SPACE TO DATA-VAERDE PAR-HP-IDPROCESS                           
181600     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
181700             PARENT-PARM DATA-PARM                                        
181800     IF WSRKOD = SPACE                                                    
181900       MOVE DATA-PARM TO PAR-HP-PARM                                      
182000     END-IF                                                               
182100                                                                          
182200     MOVE NEJ TO PRIO-FOUND                                               
182300     MOVE 0 TO WPRIO                                                      
182400     PERFORM UNTIL WSRKOD NOT = SPACE OR PRIO-FOUND = JA                  
182500       MOVE SPACE TO DATA-VAERDE                                          
182600       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
182700              PRIO-PARM DATA-PARM                                         
182800       IF WSRKOD = SPACE                                                  
182900         MOVE JA TO PRIO-FOUND                                            
183000         MOVE DATA-VAERDE TO WPRIO                                        
183100       ELSE                                                               
183200         CALL W980WSPC USING FGETF WSRKOD P-PARM                          
183300             PARENT-PARM DATA-PARM                                        
183400         MOVE DATA-PARM TO P-PARM                                         
183500       END-IF                                                             
183600     END-PERFORM                                                          
183700                                                                          
183800     MOVE AB-PARM TO P-PARM                                               
183900     MOVE FUNCTION CURRENT-DATE (3:6) TO DATUM-ZONAT                      
184000     MOVE SPACE TO LSOP20-TESYMBV                                         
184100     IF PAR-HP-IDPROCESS NOT = SPACE                                      
184200       STRING 'ABENDMSG(' DELIMITED BY SIZE                               
184300           ' ' DATUM-ZONAT ' ' KLOCKSLAG-HH ':' KLOCKSLAG-MM ' '          
184400                DELIMITED BY SIZE                                         
184500           PAR-HP-IDPROCESS DELIMITED BY SPACE                            
184600           ', '           DELIMITED BY SIZE                               
184700           P-IDPROCESS DELIMITED BY SPACE                                 
184800           ' ABENDED '                                                    
184900           ') IDPROCESS(' DELIMITED BY SIZE                               
185000           P-IDPROCESS DELIMITED BY SPACE                                 
185100           ') PARIDPROCESS(' DELIMITED BY SIZE                            
185200           PAR-HP-IDPROCESS DELIMITED BY SPACE                            
185300           ') TIMESTAMP(' DELIMITED BY SIZE                               
185400           WTIMESTAMP DELIMITED BY SIZE                                   
185500           ') PRIO(' DELIMITED BY SIZE                                    
185600           WPRIO DELIMITED BY SPACE                                       
185700           ')' DELIMITED BY SIZE INTO LSOP20-TESYMBV                      
185800     ELSE                                                                 
185900       STRING 'ABENDMSG(' DELIMITED BY SIZE                               
186000           ' ' DATUM-ZONAT ' ' KLOCKSLAG-HH ':' KLOCKSLAG-MM ' '          
186100                DELIMITED BY SIZE                                         
186200           P-IDPROCESS DELIMITED BY SPACE                                 
186300           ' ABENDED '                                                    
186400           ') IDPROCESS(' DELIMITED BY SIZE                               
186500           P-IDPROCESS DELIMITED BY SPACE                                 
186600           ') PARIDPROCESS() TIMESTAMP(' DELIMITED BY SIZE                
186700           WTIMESTAMP DELIMITED BY SIZE                                   
186800           ') PRIO(' DELIMITED BY SIZE                                    
186900           WPRIO DELIMITED BY SPACE                                       
187000           ')' DELIMITED BY SIZE INTO LSOP20-TESYMBV                      
187100     END-IF                                                               
187200                                                                          
187300     .                                                                    
187400     EJECT                                                                
187500 J-HAEMTA-INFO    SECTION.                                                
187600     SKIP2                                                                
187700     PERFORM S45-HAEMTA-PRIMAER-INFO                                      
187800                                                                          
187900     MOVE SPACE TO LSOP20-IDPROCESS-PARENT DATA-VAERDE                    
188000     CALL W980WSPC USING FGETF WSRKOD HP-PARM PARENT-PARM                 
188100                   DATA-PARM                                              
188200     IF WSRKOD = SPACE                                                    
188300       MOVE DATA-VAERDE TO LSOP20-IDPROCESS-PARENT                        
188400     END-IF                                                               
188500                                                                          
188600     PERFORM S47-HAEMTA-KATALOG-ORD                                       
188700                                                                          
188800     IF LSOP20-KDPROCSTAT = WAITING-STATUS OR STARTED-STATUS              
188900       MOVE SPACE TO LSOP20-TESUCC                                        
189000       MOVE HP-PARM TO BEFORE-NAMN-PARM (1)                               
189100       PERFORM S64-SOEK-VAENTANDE-EFTERF                                  
189200       MOVE 1 TO WIX PTR                                                  
189300       PERFORM UNTIL WIX > WIX-MAX OR PTR > 100                           
189400         STRING WAITING-IDPROCESS (WIX) DELIMITED BY SPACE                
189500              ' ' DELIMITED BY SIZE                                       
189600              INTO LSOP20-TESUCC WITH POINTER PTR                         
189700         ADD 1 TO WIX                                                     
189800       END-PERFORM                                                        
189900     ELSE                                                                 
190000       PERFORM S48-HAEMTA-BEFORE                                          
190100       PERFORM S49-HAEMTA-AFTER                                           
190200     END-IF                                                               
190300                                                                          
190400     MOVE SPACE TO LSOP20-TEACTQ                                          
190500     MOVE 1 TO PTR                                                        
190600     CALL W980WSPC USING FGETF WSRKOD HP-PARM ACTQ-PARM                   
190700           BEG-TIAPDAT-ANR-PARM                                           
190800     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
190900       MOVE BEG-TIAPDAT  TO DATUM-ZONAT                                   
191000       STRING DATUM-ZONAT  ' ' DELIMITED BY SIZE                          
191100              INTO LSOP20-TEACTQ WITH POINTER PTR                         
191200       CALL W980WSPC USING FGETN WSRKOD HP-PARM ACTQ-PARM                 
191300           BEG-TIAPDAT-ANR-PARM                                           
191400     END-PERFORM                                                          
191500                                                                          
191600     MOVE SPACE TO LSOP20-TEPASSQ                                         
191700     MOVE 1 TO PTR                                                        
191800     CALL W980WSPC USING FGETF WSRKOD HP-PARM PASSQ-PARM                  
191900           DATUM-PARM                                                     
192000     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
192100       MOVE DATUM-PACKAT TO DATUM-ZONAT                                   
192200       STRING DATUM-ZONAT  ' ' DELIMITED BY SIZE                          
192300              INTO LSOP20-TEPASSQ WITH POINTER PTR                        
192400       CALL W980WSPC USING FGETN WSRKOD HP-PARM PASSQ-PARM                
192500           DATUM-PARM                                                     
192600     END-PERFORM                                                          
192700                                                                          
192800     MOVE SPACE TO LSOP20-KDPROCPRIO                                      
192900     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
193000          PRIO-PARM  DATA-PARM                                            
193100     IF WSRKOD = SPACE                                                    
193200       MOVE DATA-VAERDE TO LSOP20-KDPROCPRIO                              
193300     END-IF                                                               
193400                                                                          
193500     MOVE SPACE TO LSOP20-KDVOUT                                          
193600     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
193700          VOUT-PARM  DATA-PARM                                            
193800     IF WSRKOD = SPACE                                                    
193900       MOVE DATA-VAERDE TO LSOP20-KDVOUT                                  
194000       CALL W980WSPC USING FGETN WSRKOD HP-PARM                           
194100            VOUT-PARM  DATA-PARM                                          
194200       IF WSRKOD = SPACE                                                  
194300         MOVE 'B' TO LSOP20-KDVOUT                                        
194400       END-IF                                                             
194500     END-IF                                                               
194600                                                                          
194700     MOVE SPACE TO LSOP20-ID-ABEND-JOB                                    
194800     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
194900          ABEND-JOB-PARM  DATA-PARM                                       
195000     IF WSRKOD = SPACE                                                    
195100       MOVE DATA-VAERDE TO LSOP20-ID-ABEND-JOB                            
195200     END-IF                                                               
195300                                                                          
195400     MOVE SPACE TO LSOP20-TERES ATTR-VAERDE                               
195500     MOVE +1 TO PTR                                                       
195600     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
195700          USING-PARM ATTR-PARM                                            
195800     PERFORM UNTIL WSRKOD NOT = SPACE                                     
195900       IF PTR < 100                                                       
196000         STRING ATTR-VAERDE DELIMITED BY SPACE                            
196100              ' '  DELIMITED BY SIZE                                      
196200              INTO LSOP20-TERES WITH POINTER PTR                          
196300       END-IF                                                             
196400       MOVE SPACE TO ATTR-VAERDE                                          
196500       CALL W980WSPC USING FGETN WSRKOD HP-PARM                           
196600            USING-PARM ATTR-PARM                                          
196700     END-PERFORM                                                          
196800                                                                          
196900     PERFORM S75-HAEMTA-SYMBOLER                                          
197000     .                                                                    
197100     EJECT                                                                
197200 K-HAEMTA-MSL-INFO  SECTION.                                              
197300     SKIP2                                                                
197400     PERFORM S45-HAEMTA-PRIMAER-INFO                                      
197500     .                                                                    
197600     EJECT                                                                
197700 L-HAEMTA-KALENDER  SECTION.                                              
197800     SKIP2                                                                
197900     MOVE BEGAERD-TIAPDAT TO LSOP20-TIAPDAT                               
198000                                                                          
198100     PERFORM S46-HAEMTA-PRIMAER-KALENDER                                  
198200                                                                          
198300     MOVE SPACE TO DATA-VAERDE LSOP20-TEACTQ                              
198400     MOVE 1 TO PTR                                                        
198500     CALL W980WSPC USING FGETF WSRKOD DATUM-PARM                          
198600          ACTQ-PARM DATA-PARM                                             
198700     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
198800       STRING DATA-VAERDE DELIMITED BY SPACE                              
198900              ' ' DELIMITED BY SIZE                                       
199000              INTO LSOP20-TEACTQ WITH POINTER PTR                         
199100       MOVE SPACE TO DATA-VAERDE                                          
199200       CALL W980WSPC USING FGETN WSRKOD DATUM-PARM                        
199300          ACTQ-PARM DATA-PARM                                             
199400     END-PERFORM                                                          
199500                                                                          
199600     MOVE SPACE TO DATA-VAERDE LSOP20-TEPASSQ                             
199700     MOVE 1 TO PTR                                                        
199800     CALL W980WSPC USING FGETF WSRKOD DATUM-PARM                          
199900          PASSQ-PARM DATA-PARM                                            
200000     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
200100       STRING DATA-VAERDE DELIMITED BY SPACE                              
200200              ' ' DELIMITED BY SIZE                                       
200300              INTO LSOP20-TEPASSQ WITH POINTER PTR                        
200400       MOVE SPACE TO DATA-VAERDE                                          
200500       CALL W980WSPC USING FGETN WSRKOD DATUM-PARM                        
200600          PASSQ-PARM DATA-PARM                                            
200700     END-PERFORM                                                          
200800     .                                                                    
200900     EJECT                                                                
201000 M-HAEMTA-MSL-KALENDER SECTION.                                           
201100     SKIP2                                                                
201200     PERFORM S46-HAEMTA-PRIMAER-KALENDER                                  
201300     .                                                                    
201400     EJECT                                                                
201500 N-PLAN-AKT-PASS       SECTION.                                           
201600     SKIP2                                                                
201700     MOVE HP-PARM TO P-PARM                                               
201800     MOVE BEGAERD-TIAPDAT TO NUDAT  LSOP20-TIAPDAT                        
201900     MOVE SPACE TO LSOP20-KDPROCSTRT                                      
202000     MOVE SPACE TO LSOP20-TECATALOG                                       
202100     MOVE SPACE TO LSOP20-TEPRED                                          
202200     MOVE SPACE TO LSOP20-TESUCC                                          
202300     MOVE SPACE TO LSOP20-IDPROCESS-PARENT                                
202400     MOVE SPACE TO LSOP20-KDPROCMTYP                                      
202500     MOVE SPACE TO LSOP20-KDPROCPRIO                                      
202600     MOVE SPACE TO LSOP20-KDVOUT                                          
202700     MOVE SPACE TO LSOP20-TERES                                           
202800     PERFORM S1A-UNDERSOEK-AKT-PASS                                       
202900     MOVE SUB-AKT-PASS TO LSOP20-KDPROCSTAT                               
203000                                                                          
203100     IF SUB-AKT-PASS = 'A'                                                
203200                                                                          
203300       MOVE M33-ORSAK TO M42-ORSAK                                        
203400       MOVE 'ACTIVATION' TO M42-ACTION                                    
203500                                                                          
203600       MOVE SPACE TO DATA-VAERDE                                          
203700       CALL W980WSPC USING FGETF WSRKOD HP-PARM STARTTYP-PARM             
203800                     DATA-PARM                                            
203900       IF WSRKOD = SPACE                                                  
204000         MOVE DATA-VAERDE TO LSOP20-KDPROCSTRT                            
204100       END-IF                                                             
204200                                                                          
204300       MOVE HP-PARM TO NAMN-PARM                                          
204400       PERFORM S80-HAEMTA-PROCESS-INFO                                    
204500       MOVE TIAPDAT TO LSOP20-TIAPDAT-SENAST                              
204600       MOVE TIEXDAT TO LSOP20-TIEXDAT-SENAST                              
204700       MOVE TIMINUT-START TO LSOP20-TIMINUT-START                         
204800       MOVE TIMINUT-STOPP TO LSOP20-TIMINUT-STOPP                         
204900       MOVE TIEXEC-MEDEL TO LSOP20-TIEXEC-MEDEL                           
205000       MOVE TIEXEC-SENAST TO LSOP20-TIEXEC-SENAST                         
205100                                                                          
205200*      PERFORM S47-HAEMTA-KATALOG-ORD                                     
205300                                                                          
205400*      PERFORM S48-HAEMTA-BEFORE                                          
205500                                                                          
205600*      PERFORM S49-HAEMTA-AFTER                                           
205700                                                                          
205800                                                                          
205900       MOVE SPACE TO DATA-VAERDE                                          
206000       CALL W980WSPC USING FGETF WSRKOD HP-PARM PARENT-PARM               
206100                     DATA-PARM                                            
206200       IF WSRKOD = SPACE                                                  
206300         MOVE DATA-VAERDE TO LSOP20-IDPROCESS-PARENT                      
206400       END-IF                                                             
206500                                                                          
206600       CALL W980WSPC USING FGETF WSRKOD HP-PARM                           
206700            MTYP-PARM  DATA-PARM                                          
206800       IF WSRKOD = SPACE                                                  
206900         MOVE DATA-VAERDE TO LSOP20-KDPROCMTYP                            
207000       END-IF                                                             
207100                                                                          
207200       CALL W980WSPC USING FGETF WSRKOD HP-PARM                           
207300            PRIO-PARM  DATA-PARM                                          
207400       IF WSRKOD = SPACE                                                  
207500         MOVE DATA-VAERDE TO LSOP20-KDPROCPRIO                            
207600       END-IF                                                             
207700                                                                          
207800       CALL W980WSPC USING FGETF WSRKOD HP-PARM                           
207900            VOUT-PARM  DATA-PARM                                          
208000       IF WSRKOD = SPACE                                                  
208100         MOVE DATA-VAERDE TO LSOP20-KDVOUT                                
208200         CALL W980WSPC USING FGETN WSRKOD HP-PARM                         
208300              VOUT-PARM  DATA-PARM                                        
208400         IF WSRKOD = SPACE                                                
208500           MOVE 'B' TO LSOP20-KDVOUT                                      
208600         END-IF                                                           
208700       END-IF                                                             
208800     ELSE                                                                 
208900       IF M34-ORSAK NOT = SPACE                                           
209000         MOVE M34-ORSAK TO M42-ORSAK                                      
209100       ELSE                                                               
209200         MOVE M34-LONG-ORSAK TO M42-ORSAK                                 
209300       END-IF                                                             
209400       MOVE 'PASSIVATION' TO M42-ACTION                                   
209500                                                                          
209600       MOVE ZERO    TO LSOP20-TIEXDAT-SENAST                              
209700                       LSOP20-TIEXDAT-SENAST                              
209800                       LSOP20-TIMINUT-START                               
209900                       LSOP20-TIMINUT-STOPP                               
210000                       LSOP20-TIEXEC-MEDEL                                
210100                       LSOP20-TIEXEC-SENAST                               
210200     END-IF                                                               
210300                                                                          
210400     MOVE 42 TO MSG-NR                                                    
210500     MOVE P-IDPROCESS TO M42-IDPROCESS                                    
210600     PERFORM S90-DISPLAY-MSG                                              
210700     .                                                                    
210800     EJECT                                                                
210900 O-AKTIVERA      SECTION.                                                 
211000     SKIP2                                                                
211100     MOVE HP-PARM TO P-PARM                                               
211200     MOVE SPACE TO M33-ORSAK                                              
211300     IF HP-KDPROCSTAT = PASSIVE-STATUS OR ENDED-STATUS                    
211400       MOVE NUDAT TO BEGAERD-TIAPDAT                                      
211500       PERFORM S70-LAGRA-ANR-SYMBOLER                                     
211600       IF SYMB-FEL = JA                                                   
211700         MOVE P-IDPROCESS TO M53-IDPROCESS                                
211800         MOVE 53 TO MSG-NR                                                
211900         PERFORM S90-DISPLAY-MSG                                          
212000       ELSE                                                               
212100         MOVE 'A' TO AKT-PASS                                             
212200         PERFORM S1-TOP-DOWN-AKT-PASS                                     
212300         IF STARTA = JA                                                   
212400           PERFORM S55-UNDERSOEK-EXTRAHERA                                
212500           IF EXTRAHERA = JA                                              
212600             PERFORM S56-EXTRAHERA-JCL                                    
212700           END-IF                                                         
212800           PERFORM S2-TOP-DOWN-STARTA                                     
212900         END-IF                                                           
213000       END-IF                                                             
213100     ELSE                                                                 
213200       MOVE 16 TO MSG-NR                                                  
213300       MOVE P-IDPROCESS TO M16-IDPROCESS                                  
213400       PERFORM S90-DISPLAY-MSG                                            
213500     END-IF                                                               
213600     .                                                                    
213700     EJECT                                                                
213800 P-PASSIVERA     SECTION.                                                 
213900     SKIP2                                                                
214000     MOVE HP-PARM TO P-PARM                                               
214100     MOVE SPACE TO M34-ORSAK                                              
214200     MOVE SPACE TO M34-LONG-ORSAK                                         
214300     IF KDPROCSTAT = PASSIVE-STATUS                                       
214400       MOVE 17 TO MSG-NR                                                  
214500       MOVE P-IDPROCESS TO M17-IDPROCESS                                  
214600       PERFORM S90-DISPLAY-MSG                                            
214700     ELSE                                                                 
214800       MOVE 'P' TO AKT-PASS                                               
214900       PERFORM S1-TOP-DOWN-AKT-PASS                                       
215000     END-IF                                                               
215100     .                                                                    
215200     EJECT                                                                
215300 Q-AVBESTAELL-ANR   SECTION.                                              
215400     SKIP2                                                                
215500     MOVE HP-PARM TO P-PARM                                               
215600     MOVE BEGAERD-TIAPDAT TO BEG-TIAPDAT M10-TIAPDAT                      
215700     MOVE P-IDPROCESS TO M10-IDPROCESS                                    
215800     MOVE 10 TO MSG-NR                                                    
215900     PERFORM S90-DISPLAY-MSG                                              
216000                                                                          
216100     CALL W980WSPC USING FDEL WSRKOD P-PARM                               
216200               ACTQ-PARM BEG-TIAPDAT-ANR-PARM                             
216300     IF WSRKOD NOT = SPACE                                                
216400        MOVE 26 TO MSG-NR                                                 
216500        MOVE 'Q1' TO M26-SECTION                                          
216600        MOVE WSRKOD TO M26-RKOD                                           
216700        PERFORM S90-DISPLAY-MSG                                           
216800        GO TO ERROR-EXIT                                                  
216900     END-IF                                                               
217000     CALL W980WSPC USING FDEL WSRKOD BEGAERD-TIAPDAT-PARM                 
217100               ACTQ-PARM P-PARM                                           
217200*    IF WSRKOD NOT = SPACE                                                
217300*       MOVE 26 TO MSG-NR                                                 
217400*       MOVE 'Q2' TO M26-SECTION                                          
217500*       MOVE WSRKOD TO M26-RKOD                                           
217600*       PERFORM S90-DISPLAY-MSG                                           
217700*       GO TO ERROR-EXIT                                                  
217800*    END-IF                                                               
217900     .                                                                    
218000     EJECT                                                                
218100 R-HAEMTA-INFO-ANR  SECTION.                                              
218200     SKIP2                                                                
218300     MOVE HP-PARM TO TEMP-PARM                                            
218400     MOVE P-ANR TO ANR                                                    
218500     MOVE ANR-PARM TO HP-PARM                                             
218600                                                                          
218700     PERFORM S75-HAEMTA-SYMBOLER                                          
218800     MOVE TEMP-PARM TO HP-PARM                                            
218900     .                                                                    
219000     EJECT                                                                
219100 V-LAGRA-SYMB-VAERDEN    SECTION.                                         
219200     SKIP2                                                                
219300     MOVE HP-PARM TO P-PARM                                               
219400     MOVE 'P' TO ID-TYP                                                   
219500     PERFORM S77-LAGRA-SYMBOLER                                           
219600     .                                                                    
219700     EJECT                                                                
219800 W-SLAEPP-RESURSER-FRIA  SECTION.                                         
219900     SKIP2                                                                
220000     MOVE HP-PARM TO RP-PARM NAMN-PARM                                    
220100     PERFORM S80-HAEMTA-PROCESS-INFO                                      
220200     IF KDPROCSTAT = STARTED-STATUS                                       
220300       MOVE NEJ TO RESURSER-SLAEPPTA                                      
220400       PERFORM S68-UNDERSOEK-RESURS-KOER                                  
220500       IF RESURSER-SLAEPPTA = JA                                          
220600         MOVE 56 TO MSG-NR                                                
220700         MOVE HP-IDPROCESS TO M56-IDPROCESS                               
220800         PERFORM S90-DISPLAY-MSG                                          
220900       ELSE                                                               
221000         MOVE 55 TO MSG-NR                                                
221100         MOVE HP-IDPROCESS TO M55-IDPROCESS                               
221200         PERFORM S90-DISPLAY-MSG                                          
221300       END-IF                                                             
221400     ELSE                                                                 
221500       MOVE 54 TO MSG-NR                                                  
221600       MOVE HP-IDPROCESS TO M54-IDPROCESS                                 
221700       PERFORM S90-DISPLAY-MSG                                            
221800     END-IF                                                               
221900     .                                                                    
222000     EJECT                                                                
222100 X-OPEN-SOPREG   SECTION.                                                 
222200     SKIP2                                                                
222300     MOVE SPACE TO WSPACE-DD-NAMN                                         
222400     STRING LSOP20-IDDDPREFIX 'SOPDD1' DELIMITED BY SPACE                 
222500            INTO WSPACE-DD-NAMN                                           
222600                                                                          
222700     MOVE SPACE TO PDS-DDNAME-1                                           
222800     STRING LSOP20-IDDDPREFIX 'PDSDD1' DELIMITED BY SPACE                 
222900            INTO PDS-DDNAME-1                                             
223000                                                                          
223100     MOVE SPACE TO PDS-DDNAME-2                                           
223200     STRING LSOP20-IDDDPREFIX 'PDSDD2' DELIMITED BY SPACE                 
223300            INTO PDS-DDNAME-2                                             
223400                                                                          
223500     CALL W980WSPC USING FOPEN WSRKOD WSPACE-DD-PARM                      
223600     IF WSRKOD = SPACE                                                    
223700       MOVE JA TO OPEN-GJORD                                              
223800     ELSE                                                                 
223900       MOVE WSPACE-DD-NAMN TO M5-DDNAMN                                   
224000       MOVE 5 TO MSG-NR                                                   
224100       PERFORM S90-DISPLAY-MSG                                            
224200     END-IF                                                               
224300     .                                                                    
224400     EJECT                                                                
224500 Y-CLOSE-SOPREG   SECTION.                                                
224600     SKIP2                                                                
224700     CALL W980WSPC USING FCLSE WSRKOD                                     
224800     IF WSRKOD = SPACE                                                    
224900       MOVE NEJ TO OPEN-GJORD                                             
225000     ELSE                                                                 
225100       MOVE 28 TO MSG-NR                                                  
225200       MOVE WSPACE-DD-NAMN TO M28-DDNAMN                                  
225300       PERFORM S90-DISPLAY-MSG                                            
225400     END-IF                                                               
225500     .                                                                    
225600     EJECT                                                                
225700 S1-TOP-DOWN-AKT-PASS SECTION.                                            
225800     SKIP2                                                                
225900     MOVE ZERO TO AP-SIX                                                  
226000     MOVE NEJ TO STARTA                                                   
226100                                                                          
226200     MOVE AKT-PASS TO SUB-AKT-PASS                                        
226300     PERFORM S1B-SUB-AKT-PASS                                             
226400     IF FORTSAETT = JA                                                    
226500                                                                          
226600        MOVE JA TO STARTA                                                 
226700        PERFORM S32-PUSH-PROCESS-AP-STACK                                 
226800        MOVE SPACE TO P-IDPROCESS                                         
226900        CALL W980WSPC USING FGETF WSRKOD                                  
227000            AP-STACK-IDPROCESS-PARM (AP-SIX) CONTAINS-PARM P-PARM         
227100        PERFORM UNTIL AP-SIX NOT > 0                                      
227200          PERFORM UNTIL WSRKOD NOT = SPACE                                
227300            IF AKT-PASS = 'A'                                             
227400              PERFORM S1A-UNDERSOEK-AKT-PASS                              
227500            ELSE                                                          
227600              MOVE 'P' TO SUB-AKT-PASS                                    
227700              MOVE ' PASSIVATED PARENT PROCESS.'                          
227800              TO M34-ORSAK                                                
227900              MOVE SPACE TO M34-LONG-ORSAK                                
228000            END-IF                                                        
228100            PERFORM S1B-SUB-AKT-PASS                                      
228200            IF FORTSAETT = JA                                             
228300*             -- TA 1:A BARN                                              
228400              MOVE SUB-AKT-PASS TO AKT-PASS                               
228500              PERFORM S32-PUSH-PROCESS-AP-STACK                           
228600              MOVE SPACE TO P-IDPROCESS                                   
228700              CALL W980WSPC USING FGETF WSRKOD                            
228800                   AP-STACK-IDPROCESS-PARM (AP-SIX)                       
228900                           CONTAINS-PARM P-PARM                           
229000            ELSE                                                          
229100              IF AP-SIX > 1                                               
229200                SUBTRACT +1 FROM AP-SIX                                   
229300                MOVE AP-STACK-ANR (AP-SIX) TO AKTUELLT-ANR                
229400                ADD +1 TO AP-SIX                                          
229500              END-IF                                                      
229600*             -- TA NÄSTA SYSKON                                          
229700              MOVE SPACE TO P-IDPROCESS                                   
229800              CALL W980WSPC USING FGETN WSRKOD                            
229900                   AP-STACK-IDPROCESS-PARM (AP-SIX)                       
230000                           CONTAINS-PARM P-PARM                           
230100            END-IF                                                        
230200          END-PERFORM                                                     
230300          PERFORM S33-POP-PROCESS-AP-STACK                                
230400          IF AP-SIX > 0                                                   
230500            MOVE SPACE TO P-IDPROCESS                                     
230600            CALL W980WSPC USING FGETN WSRKOD                              
230700                AP-STACK-IDPROCESS-PARM (AP-SIX)                          
230800                       CONTAINS-PARM P-PARM                               
230900          END-IF                                                          
231000        END-PERFORM                                                       
231100                                                                          
231200        IF AKT-PASS = 'A'                                                 
231300          MOVE AP-STACK-IDPROCESS-PARM (1) TO P-PARM                      
231400          PERFORM S1C-BEHANDLA-AKTIVERINGS-MEDD                           
231500        END-IF                                                            
231600                                                                          
231700        MOVE AP-STACK-IDPROCESS-PARM (1) TO P-PARM                        
231800     END-IF                                                               
231900     .                                                                    
232000     EJECT                                                                
232100 S1A-UNDERSOEK-AKT-PASS             SECTION.                              
232200     SKIP2                                                                
232300     MOVE 'A' TO SUB-AKT-PASS                                             
232400     MOVE ' NO CATALOG WORDS.' TO M33-ORSAK                               
232500                                                                          
232600     MOVE NEJ TO CAT-NCAT                                                 
232700     CALL W980WSPC USING FGETF WSRKOD P-PARM CATALOG-PARM                 
232800          DATA-PARM                                                       
232900     IF WSRKOD = SPACE                                                    
233000        MOVE JA TO CAT-NCAT                                               
233100     END-IF                                                               
233200     CALL W980WSPC USING FGETF WSRKOD P-PARM NOT-CATALOG-PARM             
233300          DATA-PARM                                                       
233400     IF WSRKOD = SPACE                                                    
233500        MOVE JA TO CAT-NCAT                                               
233600     END-IF                                                               
233700                                                                          
233800     IF CAT-NCAT = JA                                                     
233900*      -- KATALOG-ORD FINNS. KONTROLLERA MOT KALENDERN                    
234000       MOVE 'P' TO SUB-AKT-PASS                                           
234100       MOVE ' NO MATCHING CTLG WORDS.'                                    
234200            TO M34-ORSAK                                                  
234300       MOVE SPACE TO M34-LONG-ORSAK                                       
234400                                                                          
234500       MOVE SPACE TO DATA-VAERDE                                          
234600       CALL W980WSPC USING FGETF WSRKOD NUDAT-PARM                        
234700            CATALOG-PARM DATA-PARM                                        
234800       PERFORM UNTIL SUB-AKT-PASS NOT = 'P' OR WSRKOD NOT = SPACE         
234900         CALL W980WSPC USING FTEST WSRKOD P-PARM CATALOG-PARM             
235000              DATA-PARM                                                   
235100         IF WSRKOD = SPACE                                                
235200           MOVE 'A' TO SUB-AKT-PASS                                       
235300           MOVE ' MATCHING CTLG WORDS.' TO M33-ORSAK                      
235400         END-IF                                                           
235500         MOVE SPACE TO DATA-VAERDE                                        
235600         CALL W980WSPC USING FGETN WSRKOD NUDAT-PARM                      
235700              CATALOG-PARM DATA-PARM                                      
235800       END-PERFORM                                                        
235900                                                                          
236000       CALL W980WSPC USING FGETF WSRKOD NUDAT-PARM                        
236100            NOT-CATALOG-PARM DATA-PARM                                    
236200       PERFORM UNTIL SUB-AKT-PASS NOT = 'P' OR WSRKOD NOT = SPACE         
236300         CALL W980WSPC USING FTEST WSRKOD P-PARM NOT-CATALOG-PARM         
236400              DATA-PARM                                                   
236500         IF WSRKOD = SPACE                                                
236600           MOVE 'A' TO SUB-AKT-PASS                                       
236700           MOVE ' MATCHING "NOT" CTLG WORDS.'                             
236800             TO M33-ORSAK                                                 
236900         END-IF                                                           
237000         MOVE SPACE TO DATA-VAERDE                                        
237100         CALL W980WSPC USING FGETN WSRKOD NUDAT-PARM                      
237200              NOT-CATALOG-PARM DATA-PARM                                  
237300       END-PERFORM                                                        
237400                                                                          
237500       MOVE SPACE TO DATA-VAERDE                                          
237600       CALL W980WSPC USING FGETF WSRKOD NUDAT-PARM                        
237700            NOT-CATALOG-PARM DATA-PARM                                    
237800       PERFORM UNTIL SUB-AKT-PASS NOT = 'A' OR WSRKOD NOT = SPACE         
237900         CALL W980WSPC USING FTEST WSRKOD P-PARM CATALOG-PARM             
238000              DATA-PARM                                                   
238100         IF WSRKOD = SPACE                                                
238200           MOVE 'P' TO SUB-AKT-PASS                                       
238300        MOVE ' CTLG WORD MATCHING "NOT" IN THE CALENDAR.'                 
238400          TO M34-LONG-ORSAK                                               
238500          MOVE SPACE TO M34-ORSAK                                         
238600         END-IF                                                           
238700         MOVE SPACE TO DATA-VAERDE                                        
238800         CALL W980WSPC USING FGETN WSRKOD NUDAT-PARM                      
238900              NOT-CATALOG-PARM DATA-PARM                                  
239000       END-PERFORM                                                        
239100                                                                          
239200       MOVE SPACE TO DATA-VAERDE                                          
239300       CALL W980WSPC USING FGETF WSRKOD NUDAT-PARM                        
239400            CATALOG-PARM DATA-PARM                                        
239500       PERFORM UNTIL SUB-AKT-PASS NOT = 'A' OR WSRKOD NOT = SPACE         
239600         CALL W980WSPC USING FTEST WSRKOD P-PARM NOT-CATALOG-PARM         
239700              DATA-PARM                                                   
239800         IF WSRKOD = SPACE                                                
239900           MOVE 'P' TO SUB-AKT-PASS                                       
240000           MOVE ' "NOT" CTLG WORD MATCHING WORD IN THE CALENDAR.'         
240100            TO M34-LONG-ORSAK                                             
240200           MOVE SPACE TO M34-ORSAK                                        
240300         END-IF                                                           
240400         MOVE SPACE TO DATA-VAERDE                                        
240500         CALL W980WSPC USING FGETN WSRKOD NUDAT-PARM                      
240600              CATALOG-PARM DATA-PARM                                      
240700       END-PERFORM                                                        
240800                                                                          
240900     END-IF                                                               
241000                                                                          
241100     CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM                
241200          ACTQ-PARM P-PARM                                                
241300     IF WSRKOD = SPACE                                                    
241400*    --  EXPLICIT BESTÄLLNING FINNS PÅ KÖ                                 
241500        IF LSOP20-KDSOPFUNK = 'B'                                         
241600          MOVE 'A' TO SUB-AKT-PASS                                        
241700          MOVE ' QUEUED FOR ACTIVATION.' TO M33-ORSAK                     
241800        ELSE                                                              
241900          MOVE P-PARM TO NAMN-PARM                                        
242000          PERFORM S80-HAEMTA-PROCESS-INFO                                 
242100          IF KDPROCSTAT = PASSIVE-STATUS OR ENDED-STATUS                  
242200*         -- FÖRBRUKA BESTÄLLNINGEN                                       
242300            MOVE 'A' TO SUB-AKT-PASS                                      
242400            MOVE ' QUEUED FOR ACTIVATION.' TO M33-ORSAK                   
242500            PERFORM S21-TA-AKTIVERING-UR-KOE                              
242600            MOVE P-ANR TO AKTUELLT-ANR                                    
242700          END-IF                                                          
242800        END-IF                                                            
242900     END-IF                                                               
243000                                                                          
243100     CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM                
243200          PASSQ-PARM   P-PARM                                             
243300     IF WSRKOD = SPACE                                                    
243400*    --  P ÄR EXPLICIT AVBESTÄLLD                                         
243500        MOVE 'P' TO SUB-AKT-PASS                                          
243600        MOVE ' QUEUED FOR PASSIVATION' TO M34-ORSAK                       
243700        MOVE SPACE TO M34-LONG-ORSAK                                      
243800        IF LSOP20-KDSOPFUNK NOT = 'B'                                     
243900          PERFORM S23-TA-PASSIVERING-UR-KOE                               
244000        END-IF                                                            
244100     END-IF                                                               
244200     .                                                                    
244300     EJECT                                                                
244400 S1B-SUB-AKT-PASS               SECTION.                                  
244500      SKIP2                                                               
244600      MOVE NEJ TO FORTSAETT KOLLA-RESURSER                                
244700      MOVE P-PARM TO NAMN-PARM                                            
244800      PERFORM S80-HAEMTA-PROCESS-INFO                                     
244900                                                                          
245000      IF SUB-AKT-PASS = 'A'                                               
245100        CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM             
245200          PASSQ-PARM P-PARM                                               
245300        IF WSRKOD = SPACE AND LSOP20-ORDER-PROC                           
245400           PERFORM S23-TA-PASSIVERING-UR-KOE                              
245500           MOVE 8 TO MSG-NR                                               
245600           MOVE BEGAERD-TIAPDAT TO M8-TIAPDAT                             
245700           MOVE P-IDPROCESS TO M8-IDPROCESS                               
245800           PERFORM S90-DISPLAY-MSG                                        
245900        ELSE IF KDPROCSTAT = PASSIVE-STATUS OR ENDED-STATUS               
246000          MOVE WAITING-STATUS TO KDPROCSTAT                               
246100          MOVE BEGAERD-TIAPDAT TO TIAPDAT                                 
246200          MOVE 33 TO MSG-NR                                               
246300          MOVE P-IDPROCESS TO M33-IDPROCESS                               
246400          PERFORM S90-DISPLAY-MSG                                         
246500          MOVE JA TO FORTSAETT                                            
246600        ELSE                                                              
246700*         -- PROCESSEN ÄR REDAN AKTIV                                     
246800          CALL W980WSPC USING FTEST WSRKOD P-PARM                         
246900                     EJ-SAMTIDIGT-PARM P-PARM                             
247000          IF WSRKOD = SPACE                                               
247100            MOVE 'PREVIOUS ACTIVATION NOT YET FINISHED.'                  
247200              TO M39-ORSAK                                                
247300            PERFORM S20-KOEA-AKTIVERING                                   
247400          ELSE                                                            
247500            MOVE 16 TO MSG-NR                                             
247600            MOVE P-IDPROCESS TO M16-IDPROCESS                             
247700            PERFORM S90-DISPLAY-MSG                                       
247800          END-IF                                                          
247900        END-IF                                                            
248000        END-IF                                                            
248100     ELSE                                                                 
248200        CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM             
248300           ACTQ-PARM P-PARM                                               
248400        IF WSRKOD = SPACE AND LSOP20-CANCEL-PROC                          
248500           PERFORM S21-TA-AKTIVERING-UR-KOE                               
248600           MOVE BEGAERD-TIAPDAT TO M10-TIAPDAT                            
248700           MOVE P-IDPROCESS TO M10-IDPROCESS                              
248800           MOVE 10 TO MSG-NR                                              
248900           PERFORM S90-DISPLAY-MSG                                        
249000        ELSE IF KDPROCSTAT NOT = PASSIVE-STATUS                           
249100          IF KDPROCSTAT NOT = ENDED-STATUS                                
249200            PERFORM S41-ANDRA-VARAKTIGHET                                 
249300          END-IF                                                          
249400*                                                                         
249500* -----   RENSA RESURSKÖER OCH RESURLÅSNINGAR                             
249600*                                                                         
249700          IF KDPROCSTAT = STARTED-STATUS                                  
249800            IF AP-SIX = ZERO                                              
249900              MOVE JA TO KOLLA-RESURSER                                   
250000            ELSE                                                          
250100              CALL W980WSPC USING FGETF WSRKOD P-PARM                     
250200                         USING-PARM END-RES-PARM                          
250300              PERFORM UNTIL WSRKOD NOT = SPACE                            
250400                CALL W980WSPC USING FDEL WSRKOD END-RES-PARM              
250500                           USED-PARM P-PARM                               
250600                CALL W980WSPC USING FGETF WSRKOD END-RES-PARM             
250700                           USED-PARM TEMP-PARM                            
250800                IF WSRKOD NOT = SPACE                                     
250900                  CALL W980WSPC USING FDELK WSRKOD END-RES-PARM           
251000                              USED-PARM                                   
251100                END-IF                                                    
251200                CALL W980WSPC USING FGETN WSRKOD P-PARM                   
251300                           USING-PARM END-RES-PARM                        
251400              END-PERFORM                                                 
251500            END-IF                                                        
251600          ELSE IF KDPROCSTAT = WAITING-STATUS                             
251700            CALL W980WSPC USING FGETF WSRKOD P-PARM                       
251800                      USING-PARM END-RES-PARM                             
251900            PERFORM UNTIL WSRKOD NOT = SPACE                              
252000              CALL W980WSPC USING FDEL WSRKOD END-RES-PARM                
252100                        RESQ-PARM P-PARM                                  
252200              CALL W980WSPC USING FGETF WSRKOD END-RES-PARM               
252300                        RESQ-PARM TEMP-PARM                               
252400              IF WSRKOD NOT = SPACE                                       
252500                CALL W980WSPC USING FDELK WSRKOD END-RES-PARM             
252600                          RESQ-PARM                                       
252700              END-IF                                                      
252800              CALL W980WSPC USING FGETN WSRKOD P-PARM                     
252900                        USING-PARM END-RES-PARM                           
253000            END-PERFORM                                                   
253100          END-IF                                                          
253200          END-IF                                                          
253300          MOVE PASSIVE-STATUS TO KDPROCSTAT                               
253400          MOVE BEGAERD-TIAPDAT TO TIAPDAT                                 
253500          MOVE JA TO FORTSAETT                                            
253600          MOVE 34 TO MSG-NR                                               
253700          MOVE P-IDPROCESS TO M34-IDPROCESS                               
253800          PERFORM S90-DISPLAY-MSG                                         
253900        ELSE                                                              
254000          IF P-PARM = HP-PARM                                             
254100            MOVE 17 TO MSG-NR                                             
254200            MOVE P-IDPROCESS TO M17-IDPROCESS                             
254300            PERFORM S90-DISPLAY-MSG                                       
254400          END-IF                                                          
254500        END-IF                                                            
254600        END-IF                                                            
254700     END-IF                                                               
254800                                                                          
254900     IF FORTSAETT = JA                                                    
255000       MOVE AKTUELLT-ANR TO KDANR                                         
255100       MOVE +25 TO INFO-LENGD                                             
255200       CALL W980WSPC USING FSET WSRKOD P-PARM INFO-NAMN-PARM              
255300           INFO-VAERDE-PARM                                               
255400       IF WSRKOD NOT = SPACE                                              
255500         MOVE 26 TO MSG-NR                                                
255600         MOVE 'S1B' TO M26-SECTION                                        
255700         MOVE WSRKOD TO M26-RKOD                                          
255800         PERFORM S90-DISPLAY-MSG                                          
255900         GO TO ERROR-EXIT                                                 
256000       END-IF                                                             
256100       IF KOLLA-RESURSER = JA                                             
256200         MOVE P-PARM TO RP-PARM                                           
256300         PERFORM S68-UNDERSOEK-RESURS-KOER                                
256400         MOVE RP-PARM TO P-PARM                                           
256500       END-IF                                                             
256600       PERFORM S80-HAEMTA-PROCESS-INFO                                    
256700                                                                          
256800       IF SUB-AKT-PASS = 'P'                                              
256900*--      TA EV BORT UR STARTED-LISTAN OCH RENSA ABENDER O.D.              
257000         PERFORM S40-RENSA-PROCESS                                        
257100       END-IF                                                             
257200     END-IF                                                               
257300     .                                                                    
257400     EJECT                                                                
257500 S1C-BEHANDLA-AKTIVERINGS-MEDD SECTION.                                   
257600     SKIP2                                                                
257700     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
257800              ACTMSG-PARM DATA-PARM                                       
257900     IF WSRKOD = SPACE                                                    
258000       MOVE DATA-PARM TO ATTN-TEXT-PARM                                   
258100       IF DATA-LENGD > 17                                                 
258200         MOVE 17 TO ATTN-TEXT-LENGD                                       
258300       END-IF                                                             
258400       CALL W980WSPC USING FSET WSRKOD P-PARM                             
258500                    ATTN-PARM ATTN-TEXT-PARM                              
258600                                                                          
258700       PERFORM S25-LAEGG-UPP-I-STARTED-LIST                               
258800                                                                          
258900       PERFORM S4-TOP-DOWN-STOPPA                                         
259000                                                                          
259100       MOVE NEJ TO STARTA                                                 
259200     END-IF                                                               
259300     .                                                                    
259400     EJECT                                                                
259500 S2-TOP-DOWN-STARTA  SECTION.                                             
259600     SKIP2                                                                
259700     MOVE ZERO TO SIX                                                     
259800     IF LSOP20-KDSOPFUNK = 'S'                                            
259900       CALL W980WSPC USING FTEST WSRKOD P-PARM                            
260000            HOLD-PARM NULL-PARM                                           
260100       IF WSRKOD = SPACE                                                  
260200         MOVE NEJ TO STARTA                                               
260300         MOVE 18 TO MSG-NR                                                
260400         MOVE P-IDPROCESS TO M18-IDPROCESS                                
260500         PERFORM S90-DISPLAY-MSG                                          
260600       ELSE                                                               
260700         MOVE JA TO STARTA                                                
260800         PERFORM S2C-RESERVERA-RESURSER                                   
260900       END-IF                                                             
261000     ELSE                                                                 
261100       PERFORM S2A-UNDERSOEK-STARTA                                       
261200     END-IF                                                               
261300                                                                          
261400     IF STARTA = JA OR FORTSAETT-STARTA                                   
261500        IF STARTA = JA                                                    
261600          PERFORM S2B-SUB-STARTA                                          
261700        END-IF                                                            
261800        PERFORM S30-PUSH-PROCESS-STACK                                    
261900        MOVE SPACE TO P-IDPROCESS                                         
262000      CALL W980WSPC USING FGETF WSRKOD STACK-IDPROCESS-PARM (SIX)         
262100            CONTAINS-PARM  P-PARM                                         
262200                                                                          
262300        PERFORM UNTIL SIX NOT > 0                                         
262400          PERFORM UNTIL WSRKOD NOT = SPACE                                
262500            PERFORM S2A-UNDERSOEK-STARTA                                  
262600            IF STARTA = JA OR FORTSAETT-STARTA                            
262700              IF STARTA = JA                                              
262800                PERFORM S2B-SUB-STARTA                                    
262900              END-IF                                                      
263000*             -- TA 1:A BARN                                              
263100              PERFORM S30-PUSH-PROCESS-STACK                              
263200              MOVE SPACE TO P-IDPROCESS                                   
263300              CALL W980WSPC USING FGETF WSRKOD                            
263400                  STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM         
263500            ELSE                                                          
263600*             -- TA NÄSTA SYSKON                                          
263700              MOVE SPACE TO P-IDPROCESS                                   
263800              CALL W980WSPC USING FGETN WSRKOD                            
263900                   STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM        
264000            END-IF                                                        
264100          END-PERFORM                                                     
264200          PERFORM S31-POP-PROCESS-STACK                                   
264300          IF SIX > 0                                                      
264400            MOVE SPACE TO P-IDPROCESS                                     
264500            CALL W980WSPC USING FGETN WSRKOD                              
264600                STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM           
264700          END-IF                                                          
264800        END-PERFORM                                                       
264900        MOVE STACK-IDPROCESS-PARM (1) TO P-PARM                           
265000                                                                          
265100     END-IF                                                               
265200     .                                                                    
265300     EJECT                                                                
265400 S2A-UNDERSOEK-STARTA SECTION.                                            
265500     SKIP2                                                                
265600*--   VID RELEASE, GÅS ALLA AKTIVA SUBPROCESSER IGENOM.                   
265700*--   OM DE ÄR STARTED GÅR MAN BARA VIDARE (OCH UNDERSÖKER DERAS          
265800*--   SUBPROCESSER). OM DE ÄR WAITING OCH OM INGEN AKTIV                  
265900*--   FÖREGÅNGARE FINNS SÅ STARTAS DE.                                    
266000*--                                                                       
266100*--   VID END, GÅS ALLA VÄNTANDE EFTERFÖLJARE IGENOM.                     
266200*--   OM INGEN AKTIV FÖREGÅNGARE FINNS SÅ STARTAS DE.                     
266300*--   OM EFTERFÖLJARNAS SUBPROCESSER ÄR STARTED ELLER                     
266400*--   ENDED, GES EN VARNING.                                              
266500*--                                                                       
266600*--   VID ACTIVATE, GÅS ALLA AKTIVA SUBPROCESSER IGENOM.                  
266700*--   NORMALT ÄR DE WAITING.                                              
266800*--   DE SUBPROCESSER SOM ÄR WAITING OCH INTE HAR NÅGON AKTIV             
266900*--   FÖREGÅNGARE SKA STARTAS.                                            
267000*--                                                                       
267100*--   VID START GÅS ALLA AKTIVA SUBPROCESSER IGENOM.                      
267200*--   DE SOM ÄR STARTED STARTAS OM.                                       
267300*--   DE SOM ÄR WAITING STARTAS OM INGA AKTIVA FÖREGÅNGARE FINNS.         
267400*--                                                                       
267500                                                                          
267600     MOVE NEJ TO STARTA                                                   
267700     MOVE P-PARM TO NAMN-PARM                                             
267800     PERFORM S80-HAEMTA-PROCESS-INFO                                      
267900                                                                          
268000     IF KDPROCSTAT = STARTED-STATUS                                       
268100                                                                          
268200       IF     LSOP20-KDSOPFUNK = 'R'                                      
268300         MOVE FORTSAETT-STARTA TO STARTA                                  
268400                                                                          
268500       ELSE IF LSOP20-KDSOPFUNK = 'E'                                     
268600         MOVE 22 TO MSG-NR                                                
268700         MOVE 'STARTED' TO M22-TEXT                                       
268800         MOVE P-IDPROCESS TO M22-IDPROCESS                                
268900         PERFORM S90-DISPLAY-MSG                                          
269000                                                                          
269100       ELSE IF LSOP20-KDSOPFUNK = 'A' OR 'O'                              
269200         CALL W980WSPC USING FTEST WSRKOD NUDAT-PARM                      
269300                ACTQ-PARM P-PARM                                          
269400         IF WSRKOD = SPACE                                                
269500           NEXT SENTENCE                                                  
269600*--        INGEN START NU. START KOMMER ATT GÖRAS DÅ AKTQ TÖMMS           
269700         ELSE                                                             
269800           MOVE 19 TO MSG-NR                                              
269900           MOVE P-IDPROCESS TO M19-IDPROCESS                              
270000           PERFORM S90-DISPLAY-MSG                                        
270100         END-IF                                                           
270200                                                                          
270300       ELSE IF LSOP20-KDSOPFUNK = 'S'                                     
270400         MOVE JA TO STARTA                                                
270500       END-IF                                                             
270600       END-IF                                                             
270700       END-IF                                                             
270800       END-IF                                                             
270900                                                                          
271000     ELSE IF KDPROCSTAT = ENDED-STATUS                                    
271100                                                                          
271200       IF LSOP20-KDSOPFUNK = 'E'                                          
271300         MOVE 22 TO MSG-NR                                                
271400         MOVE 'ENDED' TO M22-TEXT                                         
271500         MOVE P-IDPROCESS TO M22-IDPROCESS                                
271600         PERFORM S90-DISPLAY-MSG                                          
271700                                                                          
271800       ELSE IF LSOP20-KDSOPFUNK = 'A'                                     
271900         MOVE 20 TO MSG-NR                                                
272000         MOVE P-IDPROCESS TO M20-IDPROCESS                                
272100         PERFORM S90-DISPLAY-MSG                                          
272200       END-IF                                                             
272300       END-IF                                                             
272400                                                                          
272500     ELSE IF KDPROCSTAT = WAITING-STATUS                                  
272600                                                                          
272700        MOVE P-PARM TO AFTER-NAMN-PARM (1)                                
272800        PERFORM S60-SOEK-AKTIVA-FOREGANGARE                               
272900        IF AKTIVA-FINNS = NEJ                                             
273000          PERFORM S62-SOEK-KONFLIKT-PROC                                  
273100          IF KONFLIKT-FINNS = JA                                          
273200*--         STARTA EJ NU. LÄGG UPP I START-KÖ ISTÄLLET.                   
273300            CALL W980WSPC USING FADD WSRKOD KP-PARM                       
273400                     STARTQ-PARM P-PARM                                   
273500            IF WSRKOD NOT = SPACE                                         
273600              MOVE 26 TO MSG-NR                                           
273700              MOVE 'S2A' TO M26-SECTION                                   
273800              MOVE WSRKOD TO M26-RKOD                                     
273900              PERFORM S90-DISPLAY-MSG                                     
274000              GO TO ERROR-EXIT                                            
274100            END-IF                                                        
274200          ELSE                                                            
274300            PERFORM S63-SOEK-UPPTAGNA-RESURSER                            
274400            IF UPPTAGNA-RES-FINNS = JA                                    
274500*--           STARTA EJ NU. LÄGG UPP I RESURS-KÖ ISTÄLLET.                
274600              CALL W980WSPC USING FTEST WSRKOD RES-PARM                   
274700                       RESQ-PARM P-PARM                                   
274800              IF WSRKOD NOT = SPACE                                       
274900                CALL W980WSPC USING FADD WSRKOD RES-PARM                  
275000                         RESQ-PARM P-PARM                                 
275100                IF WSRKOD NOT = SPACE                                     
275200                  MOVE 26 TO MSG-NR                                       
275300                  MOVE 'S2A-2' TO M26-SECTION                             
275400                  MOVE WSRKOD TO M26-RKOD                                 
275500                  PERFORM S90-DISPLAY-MSG                                 
275600                  GO TO ERROR-EXIT                                        
275700                END-IF                                                    
275800              END-IF                                                      
275900            ELSE                                                          
276000              CALL W980WSPC USING FGETF WSRKOD P-PARM                     
276100                             PARENT-PARM NAMN-PARM                        
276200              IF WSRKOD = SPACE                                           
276300                PERFORM S80-HAEMTA-PROCESS-INFO                           
276400                IF KDPROCSTAT NOT = WAITING-STATUS                        
276500                  MOVE JA TO STARTA                                       
276600                END-IF                                                    
276700              ELSE                                                        
276800                MOVE JA TO STARTA                                         
276900              END-IF                                                      
277000            END-IF                                                        
277100          END-IF                                                          
277200        END-IF                                                            
277300                                                                          
277400     END-IF                                                               
277500     END-IF                                                               
277600     END-IF                                                               
277700                                                                          
277800     IF STARTA = JA OR FORTSAETT-STARTA                                   
277900                                                                          
278000       CALL W980WSPC USING FTEST WSRKOD P-PARM                            
278100                   HOLD-PARM NULL-PARM                                    
278200       IF WSRKOD = SPACE                                                  
278300         MOVE 18 TO MSG-NR                                                
278400         MOVE P-IDPROCESS TO M18-IDPROCESS                                
278500         PERFORM S90-DISPLAY-MSG                                          
278600         PERFORM S25-LAEGG-UPP-I-STARTED-LIST                             
278700         MOVE NEJ TO STARTA                                               
278800       ELSE                                                               
278900         CALL W980WSPC USING FGETF WSRKOD P-PARM                          
279000                     USING-PARM ATTR-PARM                                 
279100         PERFORM UNTIL WSRKOD NOT = SPACE                                 
279200           CALL W980WSPC USING FTEST WSRKOD ATTR-PARM                     
279300                     USED-PARM P-PARM                                     
279400           IF WSRKOD NOT = SPACE                                          
279500             CALL W980WSPC USING FADD WSRKOD ATTR-PARM                    
279600                       USED-PARM P-PARM                                   
279700           END-IF                                                         
279800           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
279900                       USING-PARM ATTR-PARM                               
280000         END-PERFORM                                                      
280100       END-IF                                                             
280200     END-IF                                                               
280300     .                                                                    
280400     EJECT                                                                
280500 S2B-SUB-STARTA       SECTION.                                            
280600     SKIP2                                                                
280700     PERFORM S40-RENSA-PROCESS                                            
280800     MOVE SPACE TO DATA-VAERDE                                            
280900     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
281000                 STARTTYP-PARM DATA-PARM                                  
281100     IF WSRKOD = SPACE AND DATA-VAERDE NOT = 'NONE'                       
281200         IF DATA-VAERDE = 'FSUB' OR 'SUB'                                 
281300           PERFORM S51-SUBMIT-PDS                                         
281400         ELSE IF DATA-VAERDE = 'REL'                                      
281500           PERFORM S52-RELEASE-JOB                                        
281600         ELSE                                                             
281700           MOVE 29 TO MSG-NR                                              
281800           MOVE P-IDPROCESS TO M29-IDPROCESS                              
281900           MOVE DATA-VAERDE TO M29-STARTTYP                               
282000           PERFORM S90-DISPLAY-MSG                                        
282100         END-IF                                                           
282200         END-IF                                                           
282300     ELSE                                                                 
282400       MOVE 31 TO MSG-NR                                                  
282500       MOVE P-IDPROCESS TO M31-IDPROCESS                                  
282600       PERFORM S90-DISPLAY-MSG                                            
282700     END-IF                                                               
282800                                                                          
282900     MOVE P-PARM TO NAMN-PARM                                             
283000     PERFORM S80-HAEMTA-PROCESS-INFO                                      
283100     MOVE STARTED-STATUS TO KDPROCSTAT                                    
283200     ACCEPT KLOCKSLAG FROM TIME                                           
283300     MOVE HHMM TO TIMINUT-START                                           
283400     MOVE ZERO TO TIMINUT-STOPP                                           
283500     ACCEPT TIEXDAT FROM DATE                                             
283600     MOVE +25 TO INFO-LENGD                                               
283700     CALL W980WSPC USING FSET WSRKOD P-PARM                               
283800          INFO-NAMN-PARM INFO-VAERDE-PARM                                 
283900     IF WSRKOD NOT = SPACE                                                
284000       MOVE 26 TO MSG-NR                                                  
284100       MOVE 'S2B1' TO M26-SECTION                                         
284200       MOVE WSRKOD TO M26-RKOD                                            
284300       PERFORM S90-DISPLAY-MSG                                            
284400       GO TO ERROR-EXIT                                                   
284500     END-IF                                                               
284600                                                                          
284700     PERFORM S25-LAEGG-UPP-I-STARTED-LIST                                 
284800     .                                                                    
284900     EJECT                                                                
285000 S2C-RESERVERA-RESURSER   SECTION.                                        
285100     SKIP2                                                                
285200     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
285300                 USING-PARM ATTR-PARM                                     
285400     PERFORM UNTIL WSRKOD NOT = SPACE                                     
285500       CALL W980WSPC USING FTEST WSRKOD ATTR-PARM                         
285600                 USED-PARM P-PARM                                         
285700       IF WSRKOD NOT = SPACE                                              
285800         CALL W980WSPC USING FADD WSRKOD ATTR-PARM                        
285900                   USED-PARM P-PARM                                       
286000       END-IF                                                             
286100       CALL W980WSPC USING FTEST WSRKOD ATTR-PARM                         
286200                 RESQ-PARM P-PARM                                         
286300       IF WSRKOD = SPACE                                                  
286400         CALL W980WSPC USING FDEL WSRKOD ATTR-PARM                        
286500                   RESQ-PARM P-PARM                                       
286600         CALL W980WSPC USING FGETF WSRKOD ATTR-PARM                       
286700                   RESQ-PARM TEMP-PARM                                    
286800*        ---  RENSA BORT NYCKELN                                          
286900         IF WSRKOD NOT = SPACE                                            
287000           CALL W980WSPC USING FDELK WSRKOD ATTR-PARM                     
287100                     RESQ-PARM                                            
287200         END-IF                                                           
287300       END-IF                                                             
287400       CALL W980WSPC USING FGETN WSRKOD P-PARM                            
287500                   USING-PARM ATTR-PARM                                   
287600     END-PERFORM                                                          
287700     .                                                                    
287800     EJECT                                                                
287900 S3-BOTTOM-UP-AVSLUTA SECTION.                                            
288000     SKIP2                                                                
288100     MOVE P-PARM TO EP-PARM                                               
288200     PERFORM UNTIL EP-IDPROCESS = SPACE                                   
288300                                                                          
288400*--        UPPDATERA STATUS OCH TIDER                                     
288500       MOVE EP-PARM TO NAMN-PARM                                          
288600       PERFORM S80-HAEMTA-PROCESS-INFO                                    
288700       MOVE KDANR TO SPAR-KDANR                                           
288800       IF KDPROCSTAT = STARTED-STATUS                                     
288900         PERFORM S66-BERAEKNA-EXEC-TID                                    
289000         IF TIEXEC-MEDEL = ZERO                                           
289100           MOVE TIEXEC TO TIEXEC-MEDEL                                    
289200         END-IF                                                           
289300         MOVE TIEXEC-SENAST TO WTIEXEC-SENAST                             
289400         IF TIEXEC-SENAST = ZERO                                          
289500           MOVE TIEXEC TO WTIEXEC-SENAST                                  
289600         END-IF                                                           
289700         IF TIEXEC-SENAST > 2 * TIEXEC-MEDEL                              
289800            COMPUTE WTIEXEC-SENAST = 2 * TIEXEC-MEDEL                     
289900         ELSE IF TIEXEC-SENAST <  0.5 * TIEXEC-MEDEL                      
290000            COMPUTE WTIEXEC-SENAST = 0.5 * TIEXEC-MEDEL                   
290100         END-IF                                                           
290200         END-IF                                                           
290300         MOVE TIEXEC TO TIEXEC-SENAST                                     
290400         IF TIEXEC > 2 * TIEXEC-MEDEL                                     
290500            COMPUTE TIEXEC = 2 * TIEXEC-MEDEL                             
290600         ELSE IF TIEXEC <  0.5 * TIEXEC-MEDEL                             
290700            COMPUTE TIEXEC = 0.5 * TIEXEC-MEDEL                           
290800         END-IF                                                           
290900         END-IF                                                           
291000       COMPUTE TIEXEC-MEDEL ROUNDED = (TIEXEC + WTIEXEC-SENAST +          
291100                5 * TIEXEC-MEDEL + 4) / 7                                 
291200       ELSE IF KDPROCSTAT = WAITING-STATUS                                
291300         MOVE ZERO TO TIMINUT-START                                       
291400         ACCEPT KLOCKSLAG FROM TIME                                       
291500         MOVE HHMM TO TIMINUT-STOPP                                       
291600       END-IF                                                             
291700       END-IF                                                             
291800       MOVE ENDED-STATUS TO KDPROCSTAT                                    
291900                                                                          
292000       MOVE +25 TO INFO-LENGD                                             
292100       CALL W980WSPC USING FSET WSRKOD EP-PARM                            
292200         INFO-NAMN-PARM INFO-VAERDE-PARM                                  
292300       IF WSRKOD NOT = SPACE                                              
292400         MOVE 26 TO MSG-NR                                                
292500         MOVE 'S3-1' TO M26-SECTION                                       
292600         MOVE WSRKOD TO M26-RKOD                                          
292700         PERFORM S90-DISPLAY-MSG                                          
292800         GO TO ERROR-EXIT                                                 
292900       END-IF                                                             
293000                                                                          
293100       PERFORM S41-ANDRA-VARAKTIGHET                                      
293200                                                                          
293300*--     TA BORT UR STARTED-LISTAN OCH RENSA ABENDER O.D.                  
293400       MOVE EP-PARM TO P-PARM                                             
293500       PERFORM S40-RENSA-PROCESS                                          
293600                                                                          
293700       MOVE 35 TO MSG-NR                                                  
293800       MOVE EP-IDPROCESS TO M35-IDPROCESS                                 
293900       PERFORM S90-DISPLAY-MSG                                            
294000                                                                          
294100       CALL W980WSPC USING FTEST WSRKOD BEGAERD-TIAPDAT-PARM              
294200                   ACTQ-PARM EP-PARM                                      
294300       IF WSRKOD = SPACE                                                  
294400         MOVE EP-PARM TO P-PARM                                           
294500                                                                          
294600*       --  MULTIPEL AKTIVERING BEGÄRD. AKTIVERA PROCESSEN IGEN.          
294700         PERFORM S21-TA-AKTIVERING-UR-KOE                                 
294800         MOVE P-ANR TO AKTUELLT-ANR                                       
294900         MOVE 'A' TO AKT-PASS                                             
295000         PERFORM S1-TOP-DOWN-AKT-PASS                                     
295100         IF STARTA = JA                                                   
295200           PERFORM S2-TOP-DOWN-STARTA                                     
295300         END-IF                                                           
295400         MOVE EP-PARM TO RP-PARM                                          
295500         PERFORM S68-UNDERSOEK-RESURS-KOER                                
295600                                                                          
295700         MOVE SPACE TO EP-IDPROCESS                                       
295800                                                                          
295900       ELSE                                                               
296000                                                                          
296100*        --- SÖK REDA PÅ DE PROCESSER SOM VÄNTAT PÅ DEN JUST              
296200*        --- AVSLUTANDE PROCESSEN OCH FÖRSÖK STARTA DEM.                  
296300         MOVE EP-PARM TO BEFORE-NAMN-PARM (1)                             
296400         PERFORM S64-SOEK-VAENTANDE-EFTERF                                
296500         MOVE 1 TO WIX                                                    
296600         PERFORM UNTIL WIX > WIX-MAX                                      
296700           MOVE WAITING-NAMN-PARM (WIX) TO P-PARM                         
296800           PERFORM S2-TOP-DOWN-STARTA                                     
296900           ADD 1 TO WIX                                                   
297000         END-PERFORM                                                      
297100                                                                          
297200*        --- STARTA EV. PROCESSER SOM KÖATS P.G.A KONFLIKT                
297300         MOVE SPACE TO DATA-VAERDE                                        
297400         CALL W980WSPC USING FGETF WSRKOD EP-PARM                         
297500                     STARTQ-PARM DATA-PARM                                
297600         PERFORM UNTIL WSRKOD NOT = SPACE                                 
297700            CALL W980WSPC USING FDEL WSRKOD EP-PARM                       
297800                        STARTQ-PARM DATA-PARM                             
297900            MOVE DATA-PARM TO P-PARM                                      
298000            PERFORM S2-TOP-DOWN-STARTA                                    
298100            MOVE SPACE TO DATA-PARM                                       
298200            CALL W980WSPC USING FGETF WSRKOD EP-PARM                      
298300                        STARTQ-PARM DATA-PARM                             
298400         END-PERFORM                                                      
298500                                                                          
298600*       --  OM RESURSER HÅLLS STARTA PROCESS SOM VÄNTAR                   
298700         MOVE EP-PARM TO RP-PARM                                          
298800         PERFORM S68-UNDERSOEK-RESURS-KOER                                
298900                                                                          
299000         MOVE EP-PARM TO P-PARM                                           
299100         MOVE SPACE TO EP-IDPROCESS                                       
299200                                                                          
299300         PERFORM S3C-UNDERSOEK-AVSLUTA-PARENT                             
299400         IF AVSLUTA = JA                                                  
299500           MOVE P-PARM TO EP-PARM                                         
299600         END-IF                                                           
299700       END-IF                                                             
299800                                                                          
299900     END-PERFORM                                                          
300000     .                                                                    
300100     EJECT                                                                
300200 S3C-UNDERSOEK-AVSLUTA-PARENT     SECTION.                                
300300     SKIP2                                                                
300400*---  OM DET FINNS EN FÖRÄLDER OCH DEN ÄR STARTAD  SÅ UNDERSÖKS           
300500*---  OM ALLA FÖRÄLDERNS BARN ÄR INAKTIVA. I SÅ FALL SKA DEN              
300600*---  AVSLUTAS                                                            
300700                                                                          
300800     MOVE NEJ TO AVSLUTA                                                  
300900     MOVE SPACE TO DATA-VAERDE                                            
301000     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
301100                 PARENT-PARM DATA-PARM                                    
301200     IF WSRKOD = SPACE                                                    
301300       MOVE DATA-PARM TO P-PARM NAMN-PARM                                 
301400       PERFORM S80-HAEMTA-PROCESS-INFO                                    
301500       IF KDANR NOT = SPAR-KDANR                                          
301600         PERFORM S3CA-RENSA-SYMB-PARM                                     
301700       END-IF                                                             
301800       IF KDPROCSTAT = STARTED-STATUS                                     
301900          MOVE JA TO AVSLUTA                                              
302000          MOVE SPACE TO DATA-VAERDE                                       
302100          CALL W980WSPC USING FGETF WSRKOD P-PARM                         
302200                      CONTAINS-PARM DATA-PARM                             
302300          PERFORM UNTIL WSRKOD NOT = SPACE OR AVSLUTA NOT = JA            
302400            MOVE DATA-PARM TO NAMN-PARM                                   
302500            PERFORM S80-HAEMTA-PROCESS-INFO                               
302600            IF KDPROCSTAT = WAITING-STATUS OR STARTED-STATUS              
302700              MOVE NEJ TO AVSLUTA                                         
302800            END-IF                                                        
302900            MOVE SPACE TO DATA-VAERDE                                     
303000            CALL W980WSPC USING FGETN WSRKOD P-PARM                       
303100                       CONTAINS-PARM DATA-PARM                            
303200          END-PERFORM                                                     
303300       END-IF                                                             
303400     ELSE                                                                 
303500       PERFORM S3CA-RENSA-SYMB-PARM                                       
303600     END-IF                                                               
303700     .                                                                    
303800     EJECT                                                                
303900 S3CA-RENSA-SYMB-PARM       SECTION.                                      
304000     SKIP2                                                                
304100*---  OM FÖRÄLDER SAKNAS ELLER OM FÖRÄLDERN HAR ETT ANNAT                 
304200*---  BESTÄLLNINGSNUMMER ÄN SJÄLVA PROCESSEN SÅ RENSAS ALLA               
304300*---  SYMBOLISKA PARAMETRAR BORT.                                         
304400                                                                          
304500     MOVE SPAR-KDANR TO ANR                                               
304600     CALL W980WSPC USING FGETF WSRKOD ANR-PARM                            
304700         SYMB-PARM ATTR-PARM                                              
304800     IF WSRKOD = SPACE                                                    
304900       PERFORM UNTIL WSRKOD NOT = SPACE                                   
305000         CALL W980WSPC USING FDELK WSRKOD ANR-PARM                        
305100             ATTR-PARM                                                    
305200         CALL W980WSPC USING FGETN WSRKOD ANR-PARM                        
305300             SYMB-PARM ATTR-PARM                                          
305400       END-PERFORM                                                        
305500       CALL W980WSPC USING FDELK WSRKOD ANR-PARM                          
305600           SYMB-PARM                                                      
305700     END-IF                                                               
305800     .                                                                    
305900     EJECT                                                                
306000 S4-TOP-DOWN-STOPPA    SECTION.                                           
306100     SKIP2                                                                
306200     MOVE JA TO FOERSTA                                                   
306300     PERFORM S4A-SUB-STOPPA                                               
306400     IF STOPPA = JA                                                       
306500       PERFORM S30-PUSH-PROCESS-STACK                                     
306600       MOVE SPACE TO P-IDPROCESS                                          
306700       CALL W980WSPC USING FGETF WSRKOD STACK-IDPROCESS-PARM (SIX)        
306800            CONTAINS-PARM  P-PARM                                         
306900                                                                          
307000       PERFORM UNTIL SIX NOT > 0                                          
307100         PERFORM UNTIL WSRKOD NOT = SPACE                                 
307200           PERFORM S4A-SUB-STOPPA                                         
307300           IF STOPPA = JA                                                 
307400*            -- TA 1:A BARN                                               
307500             PERFORM S30-PUSH-PROCESS-STACK                               
307600             MOVE SPACE TO P-IDPROCESS                                    
307700             CALL W980WSPC USING FGETF WSRKOD                             
307800                  STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM         
307900           ELSE                                                           
308000*            -- TA NÄSTA SYSKON                                           
308100             MOVE SPACE TO P-IDPROCESS                                    
308200             CALL W980WSPC USING FGETN WSRKOD                             
308300                 STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM          
308400           END-IF                                                         
308500         END-PERFORM                                                      
308600         PERFORM S31-POP-PROCESS-STACK                                    
308700         IF SIX > 0                                                       
308800           MOVE SPACE TO P-IDPROCESS                                      
308900           CALL W980WSPC USING FGETN WSRKOD                               
309000               STACK-IDPROCESS-PARM (SIX) CONTAINS-PARM P-PARM            
309100         END-IF                                                           
309200       END-PERFORM                                                        
309300                                                                          
309400     END-IF                                                               
309500     .                                                                    
309600     EJECT                                                                
309700 S4A-SUB-STOPPA SECTION.                                                  
309800     SKIP2                                                                
309900     MOVE JA TO STOPPA                                                    
310000     CALL W980WSPC USING FTEST WSRKOD P-PARM HOLD-PARM NULL-PARM          
310100     IF WSRKOD = SPACE                                                    
310200       MOVE 14 TO  MSG-NR                                                 
310300       MOVE P-IDPROCESS TO M14-IDPROCESS                                  
310400       PERFORM S90-DISPLAY-MSG                                            
310500       MOVE NEJ TO STOPPA                                                 
310600     ELSE                                                                 
310700       CALL W980WSPC USING FSET WSRKOD P-PARM HOLD-PARM NULL-PARM         
310800       IF WSRKOD  = SPACE                                                 
310900         IF FOERSTA = JA                                                  
311000           MOVE 37 TO MSG-NR                                              
311100           MOVE P-IDPROCESS TO M37-IDPROCESS                              
311200           PERFORM S90-DISPLAY-MSG                                        
311300           MOVE NEJ TO FOERSTA                                            
311400         END-IF                                                           
311500       ELSE                                                               
311600         MOVE 26 TO MSG-NR                                                
311700         MOVE 'S4A' TO M26-SECTION                                        
311800         MOVE WSRKOD TO M26-RKOD                                          
311900         PERFORM S90-DISPLAY-MSG                                          
312000         GO TO ERROR-EXIT                                                 
312100       END-IF                                                             
312200     END-IF                                                               
312300     .                                                                    
312400     EJECT                                                                
312500 S10-AKTIVERINGS-DATUM SECTION.                                           
312600     SKIP2                                                                
312700*    -- HÄMTA SENASTE-AKTIVERINGS-DATUM FRÅN EN PROCESS                   
312800*    -- ELLER DESS NÄRMASTE ICKE-PASSIVA FÖRÄLDER                         
312900                                                                          
313000     MOVE P-PARM TO NAMN-PARM                                             
313100     PERFORM S80-HAEMTA-PROCESS-INFO                                      
313200     MOVE SPACE TO WSRKOD                                                 
313300     PERFORM UNTIL WSRKOD NOT = SPACE OR                                  
313400        (KDPROCSTAT NOT = PASSIVE-STATUS AND                              
313500         KDPROCSTAT NOT = ENDED-STATUS)                                   
313600       MOVE SPACE TO DATA-VAERDE                                          
313700       CALL W980WSPC USING FGETF WSRKOD NAMN-PARM PARENT-PARM             
313800            DATA-PARM                                                     
313900       IF WSRKOD = SPACE                                                  
314000         MOVE DATA-PARM TO NAMN-PARM                                      
314100         PERFORM S80-HAEMTA-PROCESS-INFO                                  
314200       END-IF                                                             
314300     END-PERFORM                                                          
314400     MOVE TIAPDAT TO HP-TIAPDAT                                           
314500     .                                                                    
314600     EJECT                                                                
314700 S20-KOEA-AKTIVERING  SECTION.                                            
314800     SKIP2                                                                
314900     MOVE AKTUELLT-ANR TO P-ANR                                           
315000     MOVE BEGAERD-TIAPDAT TO BEG-TIAPDAT                                  
315100     MOVE +8 TO BEG-DAT-ANR-LENGD                                         
315200     CALL W980WSPC USING FADD WSRKOD P-PARM ACTQ-PARM                     
315300          BEG-TIAPDAT-ANR-PARM                                            
315400     IF WSRKOD  = SPACE                                                   
315500       CALL W980WSPC USING FADD WSRKOD BEGAERD-TIAPDAT-PARM               
315600             ACTQ-PARM  P-PARM                                            
315700     END-IF                                                               
315800     MOVE 39 TO MSG-NR                                                    
315900     MOVE P-IDPROCESS TO M39-IDPROCESS                                    
316000     MOVE BEGAERD-TIAPDAT TO M39-TIAPDAT                                  
316100     PERFORM S90-DISPLAY-MSG                                              
316200     IF WSRKOD NOT = SPACE                                                
316300        MOVE 26 TO MSG-NR                                                 
316400        MOVE 'S20' TO M26-SECTION                                         
316500        MOVE WSRKOD TO M26-RKOD                                           
316600        PERFORM S90-DISPLAY-MSG                                           
316700        GO TO ERROR-EXIT                                                  
316800     END-IF                                                               
316900     .                                                                    
317000     EJECT                                                                
317100 S21-TA-AKTIVERING-UR-KOE  SECTION.                                       
317200     SKIP2                                                                
317300     MOVE BEGAERD-TIAPDAT TO TMP-BEG-TIAPDAT                              
317400     MOVE ZERO TO P-ANR                                                   
317500     CALL W980WSPC USING FGETF WSRKOD P-PARM ACTQ-PARM                    
317600         BEG-TIAPDAT-ANR-PARM                                             
317700     PERFORM UNTIL WSRKOD NOT = SPACE OR                                  
317800                      BEG-TIAPDAT = TMP-BEG-TIAPDAT                       
317900       MOVE ZERO TO P-ANR                                                 
318000       CALL W980WSPC USING FGETN WSRKOD P-PARM ACTQ-PARM                  
318100           BEG-TIAPDAT-ANR-PARM                                           
318200     END-PERFORM                                                          
318300     IF WSRKOD  = SPACE                                                   
318400       CALL W980WSPC USING FDEL WSRKOD P-PARM ACTQ-PARM                   
318500           BEG-TIAPDAT-ANR-PARM                                           
318600       CALL W980WSPC USING FDEL WSRKOD BEGAERD-TIAPDAT-PARM               
318700            ACTQ-PARM P-PARM                                              
318800     END-IF                                                               
318900     IF WSRKOD NOT = SPACE                                                
319000        MOVE 26 TO MSG-NR                                                 
319100        MOVE 'S21' TO M26-SECTION                                         
319200        MOVE WSRKOD TO M26-RKOD                                           
319300        PERFORM S90-DISPLAY-MSG                                           
319400        GO TO ERROR-EXIT                                                  
319500     END-IF                                                               
319600     .                                                                    
319700     EJECT                                                                
319800 S22-KOEA-PASSIVERING   SECTION.                                          
319900     SKIP2                                                                
320000     CALL W980WSPC USING FADD WSRKOD P-PARM PASSQ-PARM                    
320100          BEGAERD-TIAPDAT-PARM                                            
320200     IF WSRKOD  = SPACE                                                   
320300        CALL W980WSPC USING FADD WSRKOD BEGAERD-TIAPDAT-PARM              
320400             PASSQ-PARM P-PARM                                            
320500     END-IF                                                               
320600     MOVE 40 TO MSG-NR                                                    
320700     MOVE P-IDPROCESS TO M40-IDPROCESS                                    
320800     MOVE BEGAERD-TIAPDAT TO M40-TIAPDAT                                  
320900     PERFORM S90-DISPLAY-MSG                                              
321000     IF WSRKOD NOT = SPACE                                                
321100        MOVE 26 TO MSG-NR                                                 
321200        MOVE 'S22' TO M26-SECTION                                         
321300        MOVE WSRKOD TO M26-RKOD                                           
321400        PERFORM S90-DISPLAY-MSG                                           
321500        GO TO ERROR-EXIT                                                  
321600     END-IF                                                               
321700     .                                                                    
321800     EJECT                                                                
321900 S23-TA-PASSIVERING-UR-KOE   SECTION.                                     
322000     SKIP2                                                                
322100     CALL W980WSPC USING FDEL WSRKOD P-PARM PASSQ-PARM                    
322200          BEGAERD-TIAPDAT-PARM                                            
322300     IF WSRKOD  = SPACE                                                   
322400        CALL W980WSPC USING FDEL WSRKOD BEGAERD-TIAPDAT-PARM              
322500             PASSQ-PARM P-PARM                                            
322600     END-IF                                                               
322700     IF WSRKOD NOT = SPACE                                                
322800        MOVE 26 TO MSG-NR                                                 
322900        MOVE 'S23' TO M26-SECTION                                         
323000        MOVE WSRKOD TO M26-RKOD                                           
323100        PERFORM S90-DISPLAY-MSG                                           
323200        GO TO ERROR-EXIT                                                  
323300     END-IF                                                               
323400     .                                                                    
323500     EJECT                                                                
323600 S25-LAEGG-UPP-I-STARTED-LIST  SECTION.                                   
323700     SKIP2                                                                
323800     CALL W980WSPC USING FTEST WSRKOD STARTED-LIST-PARM                   
323900          NULL-PARM  P-PARM                                               
324000     IF WSRKOD NOT = SPACE                                                
324100       CALL W980WSPC USING FADD WSRKOD STARTED-LIST-PARM                  
324200            NULL-PARM  P-PARM                                             
324300       IF WSRKOD NOT = SPACE                                              
324400         MOVE 26 TO MSG-NR                                                
324500         MOVE 'S25' TO M26-SECTION                                        
324600         MOVE WSRKOD TO M26-RKOD                                          
324700         PERFORM S90-DISPLAY-MSG                                          
324800         GO TO ERROR-EXIT                                                 
324900       END-IF                                                             
325000     END-IF                                                               
325100     .                                                                    
325200     EJECT                                                                
325300 S26-TA-BORT-UR-STARTED-LIST  SECTION.                                    
325400     SKIP2                                                                
325500     CALL W980WSPC USING FTEST WSRKOD STARTED-LIST-PARM                   
325600          NULL-PARM  P-PARM                                               
325700     IF WSRKOD = SPACE                                                    
325800       CALL W980WSPC USING FDEL WSRKOD STARTED-LIST-PARM                  
325900            NULL-PARM  P-PARM                                             
326000       IF WSRKOD NOT = SPACE                                              
326100         MOVE 26 TO MSG-NR                                                
326200         MOVE 'S26' TO M26-SECTION                                        
326300         MOVE WSRKOD TO M26-RKOD                                          
326400         PERFORM S90-DISPLAY-MSG                                          
326500         GO TO ERROR-EXIT                                                 
326600       END-IF                                                             
326700     END-IF                                                               
326800     .                                                                    
326900     EJECT                                                                
327000 S30-PUSH-PROCESS-STACK   SECTION.                                        
327100     SKIP2                                                                
327200     ADD 1 TO SIX                                                         
327300     IF SIX > SIX-FULL                                                    
327400        MOVE 23 TO MSG-NR                                                 
327500        PERFORM S90-DISPLAY-MSG                                           
327600        GO TO ERROR-EXIT                                                  
327700     END-IF                                                               
327800     MOVE P-PARM TO STACK-IDPROCESS-PARM (SIX)                            
327900     MOVE AKT-PASS TO STACK-AKT-PASS (SIX)                                
328000     MOVE AKTUELLT-ANR TO STACK-ANR (SIX)                                 
328100     .                                                                    
328200     EJECT                                                                
328300 S31-POP-PROCESS-STACK   SECTION.                                         
328400     SKIP2                                                                
328500     SUBTRACT 1 FROM SIX                                                  
328600     IF SIX < 0                                                           
328700        MOVE 26 TO MSG-NR                                                 
328800        MOVE 'S31' TO M26-SECTION                                         
328900        MOVE SPACE TO M26-RKOD                                            
329000        PERFORM S90-DISPLAY-MSG                                           
329100        GO TO ERROR-EXIT                                                  
329200     END-IF                                                               
329300     IF SIX > 0                                                           
329400       MOVE STACK-IDPROCESS-PARM (SIX) TO P-PARM                          
329500       MOVE STACK-AKT-PASS (SIX) TO AKT-PASS                              
329600       MOVE STACK-ANR (SIX) TO AKTUELLT-ANR                               
329700     END-IF                                                               
329800     .                                                                    
329900     EJECT                                                                
330000 S32-PUSH-PROCESS-AP-STACK SECTION.                                       
330100     SKIP2                                                                
330200     ADD 1 TO AP-SIX                                                      
330300     IF AP-SIX > AP-SIX-FULL                                              
330400        MOVE 23 TO MSG-NR                                                 
330500        PERFORM S90-DISPLAY-MSG                                           
330600        GO TO ERROR-EXIT                                                  
330700     END-IF                                                               
330800     MOVE P-PARM TO AP-STACK-IDPROCESS-PARM (AP-SIX)                      
330900     MOVE AKT-PASS TO AP-STACK-AKT-PASS (AP-SIX)                          
331000     MOVE AKTUELLT-ANR TO AP-STACK-ANR (AP-SIX)                           
331100     .                                                                    
331200     EJECT                                                                
331300 S33-POP-PROCESS-AP-STACK SECTION.                                        
331400     SKIP2                                                                
331500     SUBTRACT 1 FROM AP-SIX                                               
331600     IF AP-SIX < 0                                                        
331700        MOVE 26 TO MSG-NR                                                 
331800        MOVE 'S31' TO M26-SECTION                                         
331900        MOVE SPACE TO M26-RKOD                                            
332000        PERFORM S90-DISPLAY-MSG                                           
332100        GO TO ERROR-EXIT                                                  
332200     END-IF                                                               
332300     IF AP-SIX > 0                                                        
332400       MOVE AP-STACK-IDPROCESS-PARM (AP-SIX) TO P-PARM                    
332500       MOVE AP-STACK-AKT-PASS (AP-SIX) TO AKT-PASS                        
332600       MOVE AP-STACK-ANR (AP-SIX) TO AKTUELLT-ANR                         
332700     END-IF                                                               
332800     .                                                                    
332900     EJECT                                                                
333000 S40-RENSA-PROCESS SECTION.                                               
333100     SKIP2                                                                
333200     CALL W980WSPC USING FDEL WSRKOD STARTED-LIST-PARM                    
333300             NULL-PARM P-PARM                                             
333400                                                                          
333500     IF RENSA-ATTN = JA                                                   
333600       CALL W980WSPC USING FDELK WSRKOD P-PARM ATTN-PARM                  
333700     ELSE                                                                 
333800       MOVE JA TO RENSA-ATTN                                              
333900     END-IF                                                               
334000     .                                                                    
334100     EJECT                                                                
334200 S41-ANDRA-VARAKTIGHET    SECTION.                                        
334300     SKIP2                                                                
334400     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
334500        TEMP-VARAKTIGHET-PARM DATUM-PARM                                  
334600     IF WSRKOD = SPACE                                                    
334700       IF DATUM-PACKAT NOT > 99                                           
334800         SUBTRACT 1 FROM DATUM-PACKAT                                     
334900         IF DATUM-PACKAT  > ZERO                                          
335000           CALL W980WSPC USING FSET WSRKOD P-PARM                         
335100                         TEMP-VARAKTIGHET-PARM DATUM-PARM                 
335200         ELSE                                                             
335300           CALL W980WSPC USING FDELK WSRKOD P-PARM                        
335400                         TEMP-VARAKTIGHET-PARM                            
335500         END-IF                                                           
335600       ELSE                                                               
335700         MOVE DATUM-PACKAT  TO TMP1-YYMMDD                                
335800         MOVE NUDAT         TO TMP2-YYMMDD                                
335900         PERFORM WY2000P1                                                 
336000         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
336100           CALL W980WSPC USING FDELK WSRKOD P-PARM                        
336200                         TEMP-VARAKTIGHET-PARM                            
336300         END-IF                                                           
336400       END-IF                                                             
336500     END-IF                                                               
336600     .                                                                    
336700     EJECT                                                                
336800 S45-HAEMTA-PRIMAER-INFO  SECTION.                                        
336900     SKIP2                                                                
337000     MOVE HP-PARM TO NAMN-PARM                                            
337100     PERFORM S80-HAEMTA-PROCESS-INFO                                      
337200     MOVE KDPROCSTAT TO LSOP20-KDPROCSTAT                                 
337300     MOVE TIAPDAT TO LSOP20-TIAPDAT-SENAST                                
337400     MOVE TIEXDAT TO LSOP20-TIEXDAT-SENAST                                
337500     MOVE TIMINUT-START TO LSOP20-TIMINUT-START                           
337600     MOVE TIMINUT-STOPP TO LSOP20-TIMINUT-STOPP                           
337700     MOVE TIEXEC-MEDEL TO LSOP20-TIEXEC-MEDEL                             
337800     IF KDPROCSTAT = STARTED-STATUS                                       
337900       PERFORM S66-BERAEKNA-EXEC-TID                                      
338000       MOVE TIEXEC TO LSOP20-TIEXEC-SENAST                                
338100     ELSE                                                                 
338200       MOVE TIEXEC-SENAST TO LSOP20-TIEXEC-SENAST                         
338300     END-IF                                                               
338400                                                                          
338500     IF KDPROCSTAT = STARTED-STATUS OR WAITING-STATUS                     
338600       MOVE KDANR TO LSOP20-KDANR                                         
338700     ELSE                                                                 
338800       MOVE ZERO  TO LSOP20-KDANR                                         
338900     END-IF                                                               
339000                                                                          
339100     MOVE SPACE TO LSOP20-KDPROCSTRT DATA-VAERDE                          
339200     CALL W980WSPC USING FGETF WSRKOD HP-PARM STARTTYP-PARM               
339300                  DATA-PARM                                               
339400     IF WSRKOD = SPACE                                                    
339500       MOVE DATA-VAERDE TO LSOP20-KDPROCSTRT                              
339600     END-IF                                                               
339700                                                                          
339800     IF LSOP20-KDPROCSTAT = WAITING-STATUS                                
339900       MOVE HP-PARM TO P-PARM AFTER-NAMN-PARM (1)                         
340000       PERFORM S60-SOEK-AKTIVA-FOREGANGARE                                
340100       PERFORM S62-SOEK-KONFLIKT-PROC                                     
340200       PERFORM S63-SOEK-UPPTAGNA-RESURSER                                 
340300       MOVE WORK-TEPRED TO LSOP20-TEPRED                                  
340400     ELSE                                                                 
340500       MOVE SPACE TO LSOP20-TEPRED                                        
340600     END-IF                                                               
340700                                                                          
340800     MOVE NEJ TO LSOP20-FLHOLD                                            
340900     CALL W980WSPC USING FGETF WSRKOD HP-PARM HOLD-PARM                   
341000           DATA-PARM                                                      
341100     IF WSRKOD = SPACE                                                    
341200       MOVE JA TO LSOP20-FLHOLD                                           
341300     END-IF                                                               
341400                                                                          
341500     MOVE SPACE TO LSOP20-TEATTN                                          
341600     CALL W980WSPC USING FGETF WSRKOD HP-PARM ATTN-PARM                   
341700          ATTN-TEXT-PARM                                                  
341800     IF WSRKOD = SPACE                                                    
341900                                                                          
342000       MOVE ATTN-TEXT TO LSOP20-TEATTN                                    
342100                                                                          
342200     ELSE IF LSOP20-KDPROCSTAT = STARTED-STATUS                           
342300                                                                          
342400       IF TIEXEC-MEDEL > 0                                                
342500         CALL W980WSPC USING FGETF WSRKOD HP-PARM CONTAINS-PARM           
342600                DATA-PARM                                                 
342700         IF WSRKOD NOT = SPACE                                            
342800           ACCEPT KLOCKSLAG FROM TIME                                     
342900           MOVE HHMM TO TIMINUT-STOPP                                     
343000           DIVIDE TIMINUT-START BY 100 GIVING                             
343100                  TIMMAR REMAINDER MINUTER                                
343200           COMPUTE TID1 = TIMMAR * 60 + MINUTER                           
343300           DIVIDE TIMINUT-STOPP BY 100 GIVING                             
343400                  TIMMAR REMAINDER MINUTER                                
343500           COMPUTE TIEXEC = TIMMAR * 60 + MINUTER                         
343600           ACCEPT WDATUM FROM DATE                                        
343700           MOVE WDATUM    TO TMP1-YYMMDD                                  
343800           MOVE TIEXDAT   TO TMP2-YYMMDD                                  
343900           PERFORM WY2000P1                                               
344000           IF  TMP1-YYMMDD > TMP2-YYMMDD                                  
344100              ADD 1440 TO TIEXEC                                          
344200           END-IF                                                         
344300           COMPUTE TIEXEC = TIEXEC - TID1 + 1                             
344400           IF (TIEXEC - TIEXEC-MEDEL ) / (4 + TIEXEC-MEDEL) > 0.5         
344500                 AND LSOP20-TEATTN = SPACE                                
344600             MOVE 'LONG EXEC TIME' TO LSOP20-TEATTN                       
344700           END-IF                                                         
344800         END-IF                                                           
344900       END-IF                                                             
345000                                                                          
345100     ELSE IF LSOP20-KDPROCSTAT = WAITING-STATUS                           
345200                                                                          
345300       IF LSOP20-TEPRED = SPACE                                           
345400         CALL W980WSPC USING FGETF WSRKOD P-PARM                          
345500                 PARENT-PARM NAMN-PARM                                    
345600         IF WSRKOD = SPACE                                                
345700           PERFORM S80-HAEMTA-PROCESS-INFO                                
345800           IF KDPROCSTAT NOT = WAITING-STATUS                             
345900             MOVE 'READY TO START' TO LSOP20-TEATTN                       
346000             PERFORM S25-LAEGG-UPP-I-STARTED-LIST                         
346100           END-IF                                                         
346200         ELSE                                                             
346300           MOVE 'READY TO START' TO LSOP20-TEATTN                         
346400           PERFORM S25-LAEGG-UPP-I-STARTED-LIST                           
346500         END-IF                                                           
346600       ELSE                                                               
346700           PERFORM S26-TA-BORT-UR-STARTED-LIST                            
346800       END-IF                                                             
346900                                                                          
347000     END-IF                                                               
347100     END-IF                                                               
347200     END-IF                                                               
347300                                                                          
347400     MOVE HP-PARM TO NAMN-PARM                                            
347500     PERFORM S80-HAEMTA-PROCESS-INFO                                      
347600     IF KDPROCSTAT = STARTED-STATUS                                       
347700                                                                          
347800       CALL W980WSPC USING FGETF WSRKOD HP-PARM USING-PARM                
347900            DATA-PARM                                                     
348000       IF WSRKOD = SPACE                                                  
348100         CALL W980WSPC USING FTEST WSRKOD DATA-PARM USED-PARM             
348200              HP-PARM                                                     
348300         IF WSRKOD NOT = SPACE                                            
348400           IF LSOP20-TEATTN = SPACE                                       
348500             MOVE   'F.' TO LSOP20-TEATTN                                 
348600           ELSE                                                           
348700             MOVE LSOP20-TEATTN TO FILL                                   
348800             STRING 'F. ' DELIMITED BY SIZE                               
348900                    FILL    DELIMITED BY SIZE                             
349000                    INTO LSOP20-TEATTN                                    
349100           END-IF                                                         
349200         END-IF                                                           
349300       END-IF                                                             
349400                                                                          
349500     END-IF                                                               
349600                                                                          
349700     MOVE SPACE TO LSOP20-KDPROCMTYP                                      
349800     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
349900         MTYP-PARM  DATA-PARM                                             
350000     IF WSRKOD = SPACE                                                    
350100       MOVE DATA-VAERDE TO LSOP20-KDPROCMTYP                              
350200     END-IF                                                               
350300                                                                          
350400     MOVE ZERO TO LSOP20-TITMPDUR                                         
350500     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
350600          TEMP-VARAKTIGHET-PARM DATUM-PARM                                
350700     IF WSRKOD = SPACE                                                    
350800       MOVE DATUM-PACKAT TO LSOP20-TITMPDUR                               
350900     END-IF                                                               
351000                                                                          
351100     MOVE SPACE TO LSOP20-TERES ATTR-VAERDE                               
351200     CALL W980WSPC USING FGETF WSRKOD HP-PARM                             
351300          USING-PARM ATTR-PARM                                            
351400     IF WSRKOD  = SPACE                                                   
351500       MOVE ATTR-VAERDE TO LSOP20-TERES                                   
351600     END-IF                                                               
351700                                                                          
351800     .                                                                    
351900     EJECT                                                                
352000 S46-HAEMTA-PRIMAER-KALENDER SECTION.                                     
352100     SKIP2                                                                
352200     MOVE BEGAERD-TIAPDAT TO DATUM-PACKAT                                 
352300     MOVE SPACE TO DATA-VAERDE LSOP20-TECATALOG                           
352400     MOVE 1 TO PTR                                                        
352500     CALL W980WSPC USING FGETF WSRKOD DATUM-PARM                          
352600          CATALOG-PARM DATA-PARM                                          
352700     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 200                        
352800       STRING DATA-VAERDE DELIMITED BY SPACE                              
352900              ' ' DELIMITED BY SIZE                                       
353000              INTO LSOP20-TECATALOG WITH POINTER PTR                      
353100       MOVE SPACE TO DATA-VAERDE                                          
353200       CALL W980WSPC USING FGETN WSRKOD DATUM-PARM                        
353300          CATALOG-PARM DATA-PARM                                          
353400     END-PERFORM                                                          
353500                                                                          
353600     CALL W980WSPC USING FGETF WSRKOD DATUM-PARM                          
353700          NOT-CATALOG-PARM DATA-PARM                                      
353800     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 200                        
353900       STRING 'NOT-' DELIMITED BY SIZE                                    
354000              DATA-VAERDE DELIMITED BY SPACE                              
354100              ' ' DELIMITED BY SIZE                                       
354200              INTO LSOP20-TECATALOG WITH POINTER PTR                      
354300       MOVE SPACE TO DATA-VAERDE                                          
354400       CALL W980WSPC USING FGETN WSRKOD DATUM-PARM                        
354500          NOT-CATALOG-PARM DATA-PARM                                      
354600     END-PERFORM                                                          
354700     .                                                                    
354800     EJECT                                                                
354900 S47-HAEMTA-KATALOG-ORD  SECTION.                                         
355000     SKIP2                                                                
355100     MOVE SPACE TO LSOP20-TECATALOG DATA-VAERDE                           
355200     CALL W980WSPC USING FGETF WSRKOD HP-PARM CATALOG-PARM                
355300                   DATA-PARM                                              
355400     MOVE 1 TO PTR                                                        
355500     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
355600       STRING DATA-VAERDE DELIMITED BY SPACE                              
355700              ' ' DELIMITED BY SIZE                                       
355800              INTO LSOP20-TECATALOG WITH POINTER PTR                      
355900       MOVE SPACE TO DATA-VAERDE                                          
356000       CALL W980WSPC USING FGETN WSRKOD HP-PARM CATALOG-PARM              
356100                   DATA-PARM                                              
356200     END-PERFORM                                                          
356300     CALL W980WSPC USING FGETF WSRKOD HP-PARM NOT-CATALOG-PARM            
356400                   DATA-PARM                                              
356500     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
356600       STRING 'NOT-' DELIMITED BY SIZE                                    
356700              DATA-VAERDE DELIMITED BY SPACE                              
356800              ' ' DELIMITED BY SIZE                                       
356900              INTO LSOP20-TECATALOG WITH POINTER PTR                      
357000       MOVE SPACE TO DATA-VAERDE                                          
357100       CALL W980WSPC USING FGETN WSRKOD HP-PARM NOT-CATALOG-PARM          
357200                   DATA-PARM                                              
357300     END-PERFORM                                                          
357400     .                                                                    
357500     EJECT                                                                
357600 S48-HAEMTA-BEFORE  SECTION.                                              
357700     SKIP2                                                                
357800     MOVE '> '  TO LSOP20-TESUCC                                          
357900     MOVE 3 TO PTR                                                        
358000     MOVE SPACE TO DATA-VAERDE                                            
358100     CALL W980WSPC USING FGETF WSRKOD HP-PARM BEFORE-PARM                 
358200           DATA-PARM                                                      
358300     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
358400       STRING DATA-VAERDE  DELIMITED BY SPACE                             
358500              ' ' DELIMITED BY SIZE                                       
358600              INTO LSOP20-TESUCC  WITH POINTER PTR                        
358700       MOVE SPACE TO DATA-VAERDE                                          
358800       CALL W980WSPC USING FGETN WSRKOD HP-PARM BEFORE-PARM               
358900           DATA-PARM                                                      
359000     END-PERFORM                                                          
359100     .                                                                    
359200     EJECT                                                                
359300 S49-HAEMTA-AFTER   SECTION.                                              
359400     SKIP2                                                                
359500     MOVE '< '  TO LSOP20-TEPRED                                          
359600     MOVE 3 TO PTR                                                        
359700     MOVE SPACE TO DATA-VAERDE                                            
359800     CALL W980WSPC USING FGETF WSRKOD HP-PARM AFTER-PARM                  
359900           DATA-PARM                                                      
360000     PERFORM UNTIL WSRKOD NOT = SPACE OR PTR > 100                        
360100       STRING DATA-VAERDE  DELIMITED BY SPACE                             
360200              ' ' DELIMITED BY SIZE                                       
360300              INTO LSOP20-TEPRED  WITH POINTER PTR                        
360400       MOVE SPACE TO DATA-VAERDE                                          
360500       CALL W980WSPC USING FGETN WSRKOD HP-PARM AFTER-PARM                
360600           DATA-PARM                                                      
360700     END-PERFORM                                                          
360800     .                                                                    
360900     EJECT                                                                
361000 S51-SUBMIT-PDS    SECTION.                                               
361100     SKIP2                                                                
361200     MOVE P-IDPROCESS TO  PDS-MBRNAME                                     
361300                                                                          
361400     CALL W980WSPC USING FGETF WSRKOD P-PARM TEMP-VARAKTIGHET-PARM        
361500                         DATUM-PARM                                       
361600     IF WSRKOD = SPACE                                                    
361700       MOVE DATUM-PACKAT  TO TMP1-YYMMDD                                  
361800       MOVE NUDAT         TO TMP2-YYMMDD                                  
361900       PERFORM WY2000P1                                                   
362000     END-IF                                                               
362100     IF WSRKOD = SPACE AND                                                
362200       (DATUM-PACKAT <=  99 OR TMP1-YYMMDD >= TMP2-YYMMDD)                
362300       MOVE PDS-DDNAME-1 TO PDS-DDNAME                                    
362400       MOVE 'TEMP PDSLIB' TO M21-LIBNAME                                  
362500       MOVE 'TEMP PDS' TO M27-LIBNAME                                     
362600     ELSE                                                                 
362700       MOVE PDS-DDNAME-2 TO PDS-DDNAME                                    
362800       MOVE 'STD PDSLIB' TO M21-LIBNAME                                   
362900       MOVE 'STD PDS' TO M27-LIBNAME                                      
363000     END-IF                                                               
363100                                                                          
363200     CALL W009PDSR USING  PDS-DD-MEMBER PDS-OPEN                          
363300     IF RETURN-CODE NOT = ZERO                                            
363400       MOVE 27 TO MSG-NR                                                  
363500       MOVE P-IDPROCESS TO M27-IDPROCESS                                  
363600       PERFORM S90-DISPLAY-MSG                                            
363700       MOVE 'JCL NOT FOUND' TO ATTN-TEXT                                  
363800       CALL W980WSPC USING FSET WSRKOD P-PARM ATTN-PARM                   
363900             ATTN-TEXT-PARM                                               
364000     ELSE                                                                 
364100       CALL W009INTR USING OPEN-INTRDR                                    
364200       MOVE 0 TO IX                                                       
364300       CALL W009PDSR USING PDS-DD-MEMBER                                  
364400                       PDS-READ PDS-RECORD-AREA                           
364500       PERFORM UNTIL RETURN-CODE NOT = ZERO                               
364600         ADD 1 TO IX                                                      
364700         MOVE +0 TO ANTAL-SYMB-VAR USER-VAR PASSW-VAR                     
364800                    ANTAL-2AMP-VAR                                        
364900         INSPECT PDS-RECORD-AREA TALLYING ANTAL-SYMB-VAR FOR              
365000           ALL '&'                                                        
365100         INSPECT PDS-RECORD-AREA TALLYING ANTAL-2AMP-VAR FOR              
365200           ALL '&&'                                                       
365300         IF ANTAL-2AMP-VAR > 0                                            
365400           MULTIPLY 2 BY ANTAL-2AMP-VAR                                   
365500           SUBTRACT ANTAL-2AMP-VAR FROM ANTAL-SYMB-VAR                    
365600         END-IF                                                           
365700         INSPECT PDS-RECORD-AREA TALLYING USER-VAR FOR                    
365800           ALL 'USER=?'                                                   
365900         IF USER-VAR = 0                                                  
366000           INSPECT PDS-RECORD-AREA TALLYING USER-VAR FOR                  
366100             ALL 'USER=*'                                                 
366200         END-IF                                                           
366300         INSPECT PDS-RECORD-AREA TALLYING PASSW-VAR FOR                   
366400           ALL 'PASSWORD=?'                                               
366500         IF PASSW-VAR = 0                                                 
366600           INSPECT PDS-RECORD-AREA TALLYING PASSW-VAR FOR                 
366700             ALL 'PASSWORD=*'                                             
366800         END-IF                                                           
366900         IF ANTAL-SYMB-VAR > 0 OR USER-VAR > 0 OR PASSW-VAR > 0           
367000           PERFORM S76-HAEMTA-SYMB-VAERDE                                 
367100         END-IF                                                           
367200         CALL W009INTR USING WRITE-INTRDR PDS-RECORD-AREA                 
367300         CALL W009PDSR USING PDS-DD-MEMBER                                
367400                PDS-READ PDS-RECORD-AREA                                  
367500       END-PERFORM                                                        
367600       CALL W009INTR USING CLOSE-INTRDR                                   
367700                                                                          
367800       MOVE 21 TO MSG-NR                                                  
367900       MOVE P-IDPROCESS TO M21-IDPROCESS                                  
368000       MOVE IX TO M21-ANTAL                                               
368100       PERFORM S90-DISPLAY-MSG                                            
368200                                                                          
368300     END-IF                                                               
368400     CALL W009PDSR USING PDS-DD-MEMBER PDS-CLOSE                          
368500     MOVE JA TO WAIT-FORE-REL                                             
368600     .                                                                    
368700     EJECT                                                                
368800 S52-RELEASE-JOB SECTION.                                                 
368900                                                                          
369000     IF WAIT-FORE-REL = JA                                                
369100       CALL W009WAIT USING WAIT-TID                                       
369200       MOVE NEJ TO WAIT-FORE-REL                                          
369300     END-IF                                                               
369400                                                                          
369500     CALL W009INTR USING OPEN-INTRDR                                      
369600     MOVE P-IDPROCESS TO JES-IDPROCESS                                    
369700                                                                          
369800     MOVE SPACE TO DATA-VAERDE                                            
369900     CALL W980WSPC USING FGETF WSRKOD SOP-PARM                            
370000                 DB-STATUS-PARM  DATA-PARM                                
370100     IF WSRKOD = SPACE AND DATA-VAERDE = 'TEST'                           
370200       MOVE 'T' TO JES-JOBB-POS5                                          
370300     END-IF                                                               
370400                                                                          
370500     CALL W009INTR USING WRITE-INTRDR JES-RELEASE-CMD                     
370600     CALL W009INTR USING CLOSE-INTRDR                                     
370700                                                                          
370800     MOVE 30 TO MSG-NR                                                    
370900     MOVE JES-IDPROCESS TO  M30-IDPROCESS                                 
371000     PERFORM S90-DISPLAY-MSG                                              
371100     .                                                                    
371200     EJECT                                                                
371300 S55-UNDERSOEK-EXTRAHERA  SECTION.                                        
371400     SKIP2                                                                
371500     MOVE SPACE TO DATA-VAERDE                                            
371600     CALL W980WSPC USING FGETF WSRKOD P-PARM STARTTYP-PARM                
371700                 DATA-PARM                                                
371800     IF WSRKOD NOT = SPACE OR DATA-VAERDE = 'REL' OR 'NONE'               
371900       MOVE DATA-VAERDE TO SPAR-STARTTYP                                  
372000       MOVE P-PARM TO NAMN-PARM                                           
372100       PERFORM UNTIL NAMN-VAERDE = SPACE OR                               
372200          DATA-VAERDE = 'FSUB' OR 'SUB'                                   
372300         MOVE SPACE TO DATA-VAERDE                                        
372400         CALL W980WSPC USING FGETF WSRKOD NAMN-PARM                       
372500                       PARENT-PARM   DATA-PARM                            
372600         MOVE DATA-PARM TO NAMN-PARM                                      
372700         IF WSRKOD = SPACE                                                
372800           MOVE SPACE TO DATA-VAERDE                                      
372900           CALL W980WSPC USING FGETF WSRKOD NAMN-PARM                     
373000                       STARTTYP-PARM DATA-PARM                            
373100         END-IF                                                           
373200       END-PERFORM                                                        
373300       IF NAMN-VAERDE = SPACE                                             
373400         MOVE NEJ TO EXTRAHERA                                            
373500         IF SPAR-STARTTYP      = 'REL'                                    
373600           MOVE 12 TO MSG-NR                                              
373700           MOVE P-IDPROCESS TO M12-IDPROCESS                              
373800           PERFORM S90-DISPLAY-MSG                                        
373900         END-IF                                                           
374000       ELSE                                                               
374100         PERFORM S80-HAEMTA-PROCESS-INFO                                  
374200         IF KDPROCSTAT  NOT = WAITING-STATUS                              
374300           MOVE JA TO EXTRAHERA                                           
374400           MOVE NAMN-PARM   TO EXTRACT-PARM                               
374500           MOVE DATA-VAERDE TO SPAR-STARTTYP                              
374600         ELSE                                                             
374700           MOVE NEJ TO EXTRAHERA                                          
374800         END-IF                                                           
374900       END-IF                                                             
375000     ELSE                                                                 
375100       MOVE NEJ TO EXTRAHERA                                              
375200     END-IF                                                               
375300     .                                                                    
375400     EJECT                                                                
375500 S56-EXTRAHERA-JCL SECTION.                                               
375600     SKIP2                                                                
375700     MOVE EXTRACT-MEDLEM TO M38-PARENT                                    
375800     MOVE P-IDPROCESS TO M38-IDPROCESS                                    
375900     MOVE 38 TO MSG-NR                                                    
376000     PERFORM S90-DISPLAY-MSG                                              
376100                                                                          
376200     MOVE P-IDPROCESS TO JES-IDPROCESS                                    
376300     MOVE SPACE TO MTYP-VAERDE                                            
376400     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
376500                         MTYP-PARM  MTYP-VAERDE-PARM                      
376600     IF WSRKOD = SPACE AND MTYP-VAERDE = 'J'                              
376700       MOVE SPACE TO DATA-VAERDE                                          
376800       CALL W980WSPC USING FGETF WSRKOD SOP-PARM                          
376900                         DB-STATUS-PARM  DATA-PARM                        
377000       IF WSRKOD = SPACE AND DATA-VAERDE = 'TEST'                         
377100         MOVE 'T' TO JES-JOBB-POS5                                        
377200       END-IF                                                             
377300     END-IF                                                               
377400                                                                          
377500     IF SPAR-STARTTYP = 'FSUB' OR 'SUB'                                   
377600       PERFORM S56B-EXTRAHERA-PDS                                         
377700     ELSE                                                                 
377800       MOVE 29 TO MSG-NR                                                  
377900       MOVE P-IDPROCESS TO M29-IDPROCESS                                  
378000       MOVE DATA-VAERDE TO M29-STARTTYP                                   
378100       PERFORM S90-DISPLAY-MSG                                            
378200     END-IF                                                               
378300                                                                          
378400     IF IX > 0                                                            
378500       MOVE 21 TO MSG-NR                                                  
378600       MOVE JES-IDPROCESS TO M21-IDPROCESS                                
378700       MOVE IX TO M21-ANTAL                                               
378800     ELSE                                                                 
378900       MOVE 46 TO MSG-NR                                                  
379000       MOVE JES-IDPROCESS TO M46-IDPROCESS                                
379100       MOVE EXTRACT-MEDLEM TO M46-PARENT                                  
379200       MOVE 'JCL NOT FOUND'  TO ATTN-TEXT                                 
379300       CALL W980WSPC USING FSET WSRKOD P-PARM ATTN-PARM                   
379400            ATTN-TEXT-PARM                                                
379500       MOVE NEJ TO RENSA-ATTN                                             
379600     END-IF                                                               
379700     PERFORM S90-DISPLAY-MSG                                              
379800     .                                                                    
379900     EJECT                                                                
380000 S56B-EXTRAHERA-PDS SECTION.                                              
380100     SKIP2                                                                
380200     MOVE EXTRACT-MEDLEM TO  PDS-MBRNAME                                  
380300                                                                          
380400     CALL W980WSPC USING FGETF WSRKOD EXTRACT-PARM                        
380500                         TEMP-VARAKTIGHET-PARM DATUM-PARM                 
380600     IF WSRKOD = SPACE                                                    
380700       MOVE DATUM-PACKAT  TO TMP1-YYMMDD                                  
380800       MOVE NUDAT         TO TMP2-YYMMDD                                  
380900       PERFORM WY2000P1                                                   
381000     END-IF                                                               
381100     IF WSRKOD = SPACE AND                                                
381200       (DATUM-PACKAT NOT > 99 OR TMP1-YYMMDD >= TMP2-YYMMDD)              
381300       MOVE PDS-DDNAME-1 TO PDS-DDNAME                                    
381400       MOVE 'TEMP PDSLIB' TO M21-LIBNAME                                  
381500       MOVE 'TEMP PDS' TO M27-LIBNAME                                     
381600     ELSE                                                                 
381700       MOVE PDS-DDNAME-2 TO PDS-DDNAME                                    
381800       MOVE 'STD PDSLIB' TO M21-LIBNAME                                   
381900       MOVE 'STD PDS' TO M27-LIBNAME                                      
382000     END-IF                                                               
382100     CALL W009PDSR USING PDS-DD-MEMBER PDS-OPEN                           
382200                                                                          
382300     MOVE 0 TO IX                                                         
382400     IF RETURN-CODE NOT = ZERO                                            
382500       MOVE 27 TO MSG-NR                                                  
382600       MOVE PDS-MBRNAME TO M27-IDPROCESS                                  
382700       PERFORM S90-DISPLAY-MSG                                            
382800     ELSE                                                                 
382900       CALL W009INTR USING OPEN-INTRDR                                    
383000       MOVE NEJ TO SELECT-JOB                                             
383100       CALL W009PDSR USING PDS-DD-MEMBER                                  
383200                       PDS-READ PDS-RECORD-AREA                           
383300       PERFORM UNTIL RETURN-CODE NOT = ZERO                               
383400         MOVE PDS-RECORD-AREA TO WJCL-AREA                                
383500         PERFORM S56C-SELECT-JOB-TILL-INTRDR                              
383600         CALL W009PDSR USING PDS-DD-MEMBER                                
383700                         PDS-READ PDS-RECORD-AREA                         
383800       END-PERFORM                                                        
383900       CALL W009INTR USING CLOSE-INTRDR                                   
384000                                                                          
384100     END-IF                                                               
384200     CALL W009PDSR USING PDS-DD-MEMBER PDS-CLOSE                          
384300     MOVE JA TO WAIT-FORE-REL                                             
384400     .                                                                    
384500     EJECT                                                                
384600 S56C-SELECT-JOB-TILL-INTRDR  SECTION.                                    
384700     SKIP2                                                                
384800     MOVE SPACE TO KORT-TYP                                               
384900     IF WJCL-AREA-POS-1-2 = '//'                                          
385000       MOVE 3 TO PTR                                                      
385100       UNSTRING WJCL-AREA DELIMITED BY                                    
385200         ALL SPACE OR ',' INTO ORD1 KORT-TYP                              
385300         WITH POINTER PTR                                                 
385400     END-IF                                                               
385500     IF MTYP-VAERDE = 'J'                                                 
385600       IF KORT-TYP = 'JOB'                                                
385700         IF ORD1 = JES-IDPROCESS                                          
385800            MOVE JA TO SELECT-JOB                                         
385900         ELSE                                                             
386000            MOVE NEJ TO SELECT-JOB                                        
386100         END-IF                                                           
386200       END-IF                                                             
386300     ELSE IF MTYP-VAERDE = 'P'                                            
386400       IF KORT-TYP = 'JOB'                                                
386500         MOVE JA TO SELECT-JOB                                            
386600         MOVE NEJ TO SELECT-STEP                                          
386700       ELSE IF KORT-TYP = 'EXEC'                                          
386800         IF ORD1 = JES-IDPROCESS                                          
386900           MOVE JA TO SELECT-JOB                                          
387000           IF LSOP20-KDSOPFUNK = 'S'                                      
387100*--          VID OMSTART SKA ÄVEN EFTERFÖLJANDE STEG EXTRAHERAS           
387200             MOVE JA TO SELECT-STEP                                       
387300           END-IF                                                         
387400         ELSE IF SELECT-STEP = NEJ                                        
387500           MOVE NEJ TO SELECT-JOB                                         
387600         END-IF                                                           
387700         END-IF                                                           
387800       END-IF                                                             
387900       END-IF                                                             
388000     END-IF                                                               
388100     END-IF                                                               
388200                                                                          
388300     IF SELECT-JOB = JA                                                   
388400       ADD 1 TO IX                                                        
388500       MOVE 0 TO ANTAL-SYMB-VAR USER-VAR PASSW-VAR                        
388600                 ANTAL-2AMP-VAR                                           
388700       INSPECT PDS-RECORD-AREA TALLYING ANTAL-SYMB-VAR FOR                
388800         ALL '&'                                                          
388900       INSPECT PDS-RECORD-AREA TALLYING ANTAL-2AMP-VAR FOR                
389000         ALL '&&'                                                         
389100       IF ANTAL-2AMP-VAR > 0                                              
389200         MULTIPLY 2 BY ANTAL-2AMP-VAR                                     
389300         SUBTRACT ANTAL-2AMP-VAR FROM ANTAL-SYMB-VAR                      
389400       END-IF                                                             
389500       INSPECT PDS-RECORD-AREA TALLYING USER-VAR FOR                      
389600         ALL 'USER=?'                                                     
389700       IF USER-VAR = 0                                                    
389800         INSPECT PDS-RECORD-AREA TALLYING USER-VAR FOR                    
389900           ALL 'USER=*'                                                   
390000       END-IF                                                             
390100       INSPECT PDS-RECORD-AREA TALLYING PASSW-VAR FOR                     
390200         ALL 'PASSWORD=?'                                                 
390300       IF PASSW-VAR = 0                                                   
390400         INSPECT PDS-RECORD-AREA TALLYING PASSW-VAR FOR                   
390500           ALL 'PASSWORD=*'                                               
390600       END-IF                                                             
390700       IF ANTAL-SYMB-VAR > 0 OR USER-VAR > 0 OR PASSW-VAR > 0             
390800         PERFORM S76-HAEMTA-SYMB-VAERDE                                   
390900       END-IF                                                             
391000       CALL W009INTR USING WRITE-INTRDR PDS-RECORD-AREA                   
391100     END-IF                                                               
391200     .                                                                    
391300     EJECT                                                                
391400 S60-SOEK-AKTIVA-FOREGANGARE SECTION.                                     
391500     SKIP2                                                                
391600     MOVE NEJ TO AKTIVA-FINNS                                             
391700     MOVE SPACE TO WORK-TEPRED                                            
391800     MOVE 1 TO PTR                                                        
391900     MOVE 1 TO AIX  AIX-MAX                                               
392000     MOVE 1 TO FIX  FIX-MAX                                               
392100     PERFORM UNTIL AIX > AIX-MAX                                          
392200       MOVE SPACE TO DATA-VAERDE                                          
392300       CALL W980WSPC USING FGETF WSRKOD AFTER-NAMN-PARM (AIX)             
392400                        AFTER-PARM DATA-PARM                              
392500       PERFORM UNTIL WSRKOD NOT = SPACE                                   
392600         MOVE DATA-PARM TO NAMN-PARM                                      
392700         PERFORM S80-HAEMTA-PROCESS-INFO                                  
392800         IF KDPROCSTAT = PASSIVE-STATUS                                   
392900           PERFORM S60A-ADD-PASSIVE-AFTER                                 
393000         ELSE IF KDPROCSTAT = WAITING-STATUS OR STARTED-STATUS            
393100           PERFORM S60B-ADD-FOREG                                         
393200           MOVE JA TO AKTIVA-FINNS                                        
393300         END-IF                                                           
393400         END-IF                                                           
393500         MOVE SPACE TO DATA-VAERDE                                        
393600         CALL W980WSPC USING FGETN WSRKOD                                 
393700                               AFTER-NAMN-PARM (AIX)                      
393800                               AFTER-PARM DATA-PARM                       
393900       END-PERFORM                                                        
394000       ADD 1 TO AIX                                                       
394100     END-PERFORM                                                          
394200     .                                                                    
394300     EJECT                                                                
394400 S60A-ADD-PASSIVE-AFTER SECTION.                                          
394500     SKIP2                                                                
394600     MOVE 1 TO IX                                                         
394700     PERFORM UNTIL IX > AIX-MAX OR                                        
394800             AFTER-NAMN-PARM (IX) = NAMN-PARM                             
394900       ADD 1 TO IX                                                        
395000     END-PERFORM                                                          
395100     IF IX > AIX-MAX                                                      
395200       IF AIX-MAX < AIX-FULL                                              
395300         ADD 1 TO AIX-MAX                                                 
395400         MOVE NAMN-PARM TO AFTER-NAMN-PARM (AIX-MAX)                      
395500       ELSE                                                               
395600         MOVE 26 TO MSG-NR                                                
395700         MOVE 'S60A' TO M26-SECTION                                       
395800         MOVE SPACE TO  M26-RKOD                                          
395900         PERFORM S90-DISPLAY-MSG                                          
396000         GO TO ERROR-EXIT                                                 
396100       END-IF                                                             
396200     END-IF                                                               
396300     .                                                                    
396400     EJECT                                                                
396500 S60B-ADD-FOREG   SECTION.                                                
396600     SKIP2                                                                
396700     MOVE 1 TO IX                                                         
396800     PERFORM UNTIL IX > FIX-MAX OR                                        
396900             FOREG-IDPROCESS (IX) = NAMN-VAERDE                           
397000       ADD 1 TO IX                                                        
397100     END-PERFORM                                                          
397200     IF IX > FIX-MAX                                                      
397300       IF FIX-MAX < FIX-FULL                                              
397400         ADD 1 TO FIX-MAX                                                 
397500         MOVE NAMN-VAERDE TO FOREG-IDPROCESS (FIX-MAX)                    
397600         IF PTR < 100                                                     
397700           STRING NAMN-VAERDE DELIMITED BY SPACE                          
397800                  ' ' DELIMITED BY SIZE                                   
397900                  INTO WORK-TEPRED WITH POINTER PTR                       
398000         END-IF                                                           
398100       END-IF                                                             
398200     END-IF                                                               
398300     .                                                                    
398400     EJECT                                                                
398500 S62-SOEK-KONFLIKT-PROC   SECTION.                                        
398600     SKIP2                                                                
398700     MOVE NEJ TO KONFLIKT-FINNS                                           
398800     MOVE SPACE TO NAMN-VAERDE                                            
398900     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
399000                 EJ-SAMTIDIGT-PARM NAMN-PARM                              
399100     PERFORM UNTIL WSRKOD NOT = SPACE                                     
399200       PERFORM S80-HAEMTA-PROCESS-INFO                                    
399300       IF KDPROCSTAT = STARTED-STATUS                                     
399400         MOVE JA TO KONFLIKT-FINNS                                        
399500         MOVE NAMN-PARM TO KP-PARM                                        
399600         IF PTR < 100                                                     
399700           STRING NAMN-VAERDE DELIMITED BY SPACE                          
399800                ' '  DELIMITED BY SIZE                                    
399900                INTO WORK-TEPRED WITH POINTER PTR                         
400000         END-IF                                                           
400100       END-IF                                                             
400200       MOVE SPACE TO NAMN-VAERDE                                          
400300       CALL W980WSPC USING FGETN WSRKOD P-PARM                            
400400                  EJ-SAMTIDIGT-PARM NAMN-PARM                             
400500     END-PERFORM                                                          
400600     .                                                                    
400700     EJECT                                                                
400800 S63-SOEK-UPPTAGNA-RESURSER  SECTION.                                     
400900     SKIP2                                                                
401000*--   UNDERSÖK ALLA RESURSER SOM ANVÄNDS AV PROCESSEN.                    
401100*--   OM EN RESURS ÄR UPPTAGEN AV EN ANNAN PROCESS STRÄNGAS DEN           
401200*--   PROCESSEN IN SOM EN FÖREGÅNGARE (PRED-STRÄNG).                      
401300                                                                          
401400     MOVE NEJ TO UPPTAGNA-RES-FINNS                                       
401500     MOVE SPACE TO DATA-VAERDE                                            
401600     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
401700                 USING-PARM DATA-PARM                                     
401800     PERFORM UNTIL WSRKOD NOT = SPACE                                     
401900       MOVE SPACE TO NAMN-VAERDE                                          
402000       CALL W980WSPC USING FGETF WSRKOD DATA-PARM                         
402100                   USED-PARM NAMN-PARM                                    
402200       IF WSRKOD = SPACE                                                  
402300         MOVE DATA-PARM TO RES-PARM                                       
402400         MOVE JA TO UPPTAGNA-RES-FINNS                                    
402500         IF PTR < 100                                                     
402600           STRING NAMN-VAERDE DELIMITED BY SPACE                          
402700                ' '  DELIMITED BY SIZE                                    
402800                INTO WORK-TEPRED WITH POINTER PTR                         
402900         END-IF                                                           
403000       ELSE                                                               
403100         CALL W980WSPC USING FTEST WSRKOD DATA-PARM                       
403200                     RESQ-PARM P-PARM                                     
403300         IF WSRKOD = SPACE                                                
403400           CALL W980WSPC USING FDEL WSRKOD DATA-PARM                      
403500                       RESQ-PARM P-PARM                                   
403600           CALL W980WSPC USING FGETF WSRKOD DATA-PARM                     
403700                       RESQ-PARM TEMP-PARM                                
403800           IF WSRKOD NOT = SPACE                                          
403900             CALL W980WSPC USING FDELK WSRKOD DATA-PARM                   
404000                         RESQ-PARM                                        
404100           END-IF                                                         
404200         END-IF                                                           
404300       END-IF                                                             
404400       MOVE SPACE TO DATA-VAERDE                                          
404500       CALL W980WSPC USING FGETN WSRKOD P-PARM                            
404600                  USING-PARM DATA-PARM                                    
404700     END-PERFORM                                                          
404800     .                                                                    
404900     EJECT                                                                
405000 S64-SOEK-VAENTANDE-EFTERF  SECTION.                                      
405100     SKIP2                                                                
405200*--   UNDERSÖK ALLA EFTERFÖLJARE TILL EN PROCESS.                         
405300*--   VAR OCH EN SOM ÄR "WAITING"  SPARAS I EN TABELL.                    
405400*--   FÖR PROCESSER SOM ÄR "PASSIVE" GÖRS SAMMA UNDERSÖKNING PÅ           
405500*--   DESS EFTERFÖLJARE OSV.                                              
405600                                                                          
405700     MOVE 0 TO WIX-MAX                                                    
405800     MOVE 1 TO BIX    BIX-MAX                                             
405900     PERFORM UNTIL BIX > BIX-MAX                                          
406000       MOVE SPACE TO DATA-VAERDE                                          
406100       CALL W980WSPC USING FGETF WSRKOD BEFORE-NAMN-PARM (BIX)            
406200                     BEFORE-PARM DATA-PARM                                
406300       PERFORM UNTIL WSRKOD NOT = SPACE                                   
406400         MOVE DATA-PARM TO NAMN-PARM                                      
406500         PERFORM S80-HAEMTA-PROCESS-INFO                                  
406600         IF KDPROCSTAT = PASSIVE-STATUS                                   
406700           PERFORM S64A-ADD-PASSIVE-BEFORE                                
406800         ELSE IF KDPROCSTAT = WAITING-STATUS                              
406900           PERFORM S64B-ADD-WAITING                                       
407000         ELSE                                                             
407100           IF LSOP20-KDSOPFUNK = 'E'                                      
407200             IF KDPROCSTAT = STARTED-STATUS                               
407300               MOVE 'STARTED' TO M22-TEXT                                 
407400             ELSE IF KDPROCSTAT = ENDED-STATUS                            
407500               MOVE 'ENDED' TO M22-TEXT                                   
407600             END-IF                                                       
407700             END-IF                                                       
407800             MOVE 22 TO MSG-NR                                            
407900             MOVE NAMN-VAERDE TO M22-IDPROCESS                            
408000             PERFORM S90-DISPLAY-MSG                                      
408100           END-IF                                                         
408200         END-IF                                                           
408300         END-IF                                                           
408400         MOVE SPACE TO DATA-VAERDE                                        
408500         CALL W980WSPC USING FGETN WSRKOD                                 
408600                             BEFORE-NAMN-PARM (BIX)                       
408700                             BEFORE-PARM DATA-PARM                        
408800       END-PERFORM                                                        
408900       ADD 1 TO BIX                                                       
409000     END-PERFORM                                                          
409100     .                                                                    
409200     EJECT                                                                
409300 S64A-ADD-PASSIVE-BEFORE SECTION.                                         
409400     SKIP2                                                                
409500     MOVE 1 TO IX                                                         
409600     PERFORM UNTIL IX > BIX-MAX OR                                        
409700             BEFORE-NAMN-PARM (IX) = NAMN-PARM                            
409800       ADD 1 TO IX                                                        
409900     END-PERFORM                                                          
410000     IF IX > BIX-MAX                                                      
410100       IF BIX-MAX < BIX-FULL                                              
410200         ADD 1 TO BIX-MAX                                                 
410300         MOVE NAMN-PARM TO BEFORE-NAMN-PARM (BIX-MAX)                     
410400       ELSE                                                               
410500         MOVE 26 TO MSG-NR                                                
410600         MOVE 'S64A' TO M26-SECTION                                       
410700         MOVE SPACE TO M26-RKOD                                           
410800         PERFORM S90-DISPLAY-MSG                                          
410900         GO TO ERROR-EXIT                                                 
411000       END-IF                                                             
411100     END-IF                                                               
411200     .                                                                    
411300     EJECT                                                                
411400 S64B-ADD-WAITING        SECTION.                                         
411500     SKIP2                                                                
411600     MOVE 1 TO IX                                                         
411700     PERFORM UNTIL IX > WIX-MAX OR                                        
411800            WAITING-NAMN-PARM (IX) = NAMN-PARM                            
411900       ADD 1 TO IX                                                        
412000     END-PERFORM                                                          
412100     IF IX > WIX-MAX                                                      
412200       IF WIX-MAX < WIX-FULL                                              
412300         ADD 1 TO WIX-MAX                                                 
412400         MOVE NAMN-PARM TO WAITING-NAMN-PARM (WIX-MAX)                    
412500       ELSE                                                               
412600         MOVE 26 TO MSG-NR                                                
412700         MOVE 'S64B' TO M26-SECTION                                       
412800         MOVE SPACE TO M26-RKOD                                           
412900         PERFORM S90-DISPLAY-MSG                                          
413000         GO TO ERROR-EXIT                                                 
413100       END-IF                                                             
413200     END-IF                                                               
413300     .                                                                    
413400     EJECT                                                                
413500 S66-BERAEKNA-EXEC-TID  SECTION.                                          
413600     SKIP2                                                                
413700     DIVIDE TIMINUT-START BY 100 GIVING TIMMAR REMAINDER MINUTER          
413800     COMPUTE TID1 = TIMMAR * 60 + MINUTER                                 
413900                                                                          
414000     ACCEPT KLOCKSLAG FROM TIME                                           
414100     MOVE HHMM TO TIMINUT-STOPP                                           
414200     DIVIDE TIMINUT-STOPP BY 100 GIVING TIMMAR REMAINDER MINUTER          
414300     COMPUTE TIEXEC = TIMMAR * 60 + MINUTER                               
414400                                                                          
414500     ACCEPT WDATUM FROM DATE                                              
414600     MOVE WDATUM    TO TMP1-YYMMDD                                        
414700     MOVE TIEXDAT   TO TMP2-YYMMDD                                        
414800     PERFORM WY2000P1                                                     
414900     IF  TMP1-YYMMDD > TMP2-YYMMDD                                        
415000        ADD 1440 TO TIEXEC                                                
415100     END-IF                                                               
415200     COMPUTE TIEXEC = TIEXEC - TID1 + 1                                   
415300     .                                                                    
415400     EJECT                                                                
415500 S68-UNDERSOEK-RESURS-KOER        SECTION.                                
415600     SKIP2                                                                
415700     MOVE SPACE TO END-RES-VAERDE                                         
415800     CALL W980WSPC USING FGETF WSRKOD RP-PARM                             
415900                USING-PARM END-RES-PARM                                   
416000                                                                          
416100     PERFORM UNTIL WSRKOD NOT = SPACE                                     
416200       MOVE END-RES-PARM TO TEMP-RES-PARM                                 
416300       CALL W980WSPC USING FTEST WSRKOD END-RES-PARM                      
416400                  USED-PARM RP-PARM                                       
416500       IF WSRKOD = SPACE                                                  
416600         MOVE JA TO RESURSER-SLAEPPTA                                     
416700         CALL W980WSPC USING FDEL WSRKOD END-RES-PARM                     
416800                    USED-PARM RP-PARM                                     
416900*   --  HÅLLS RESURSEN AV NÅGON MER PROCESS                               
417000         CALL W980WSPC USING FGETF WSRKOD END-RES-PARM                    
417100                    USED-PARM TEMP-PARM                                   
417200         IF WSRKOD NOT = SPACE                                            
417300*   --  RENSA BORT NYCKELN                                                
417400           CALL W980WSPC USING FDELK WSRKOD END-RES-PARM                  
417500                       USED-PARM                                          
417600                                                                          
417700           MOVE SPACE TO P-IDPROCESS                                      
417800           CALL W980WSPC USING FGETF WSRKOD END-RES-PARM                  
417900                      RESQ-PARM P-PARM                                    
418000                                                                          
418100           MOVE NEJ TO STARTAD                                            
418200           PERFORM UNTIL WSRKOD NOT = SPACE OR STARTAD = JA               
418300             CALL W980WSPC USING FDEL WSRKOD END-RES-PARM                 
418400                        RESQ-PARM P-PARM                                  
418500             PERFORM S2-TOP-DOWN-STARTA                                   
418600*   --  STARTADES PROCESSEN                                               
418700             CALL W980WSPC USING FTEST WSRKOD END-RES-PARM                
418800                        USED-PARM P-PARM                                  
418900             IF WSRKOD = SPACE                                            
419000               MOVE JA TO STARTAD                                         
419100             ELSE                                                         
419200               MOVE SPACE TO P-IDPROCESS                                  
419300               CALL W980WSPC USING FGETF WSRKOD END-RES-PARM              
419400                          RESQ-PARM P-PARM                                
419500             END-IF                                                       
419600           END-PERFORM                                                    
419700                                                                          
419800*   --  BLEV NYCKELN TOM I SÅ FALL TA BORT                                
419900           CALL W980WSPC USING FGETF WSRKOD END-RES-PARM                  
420000                      RESQ-PARM TEMP-PARM                                 
420100           IF WSRKOD NOT = SPACE                                          
420200             CALL W980WSPC USING FDELK WSRKOD END-RES-PARM                
420300                        RESQ-PARM                                         
420400           END-IF                                                         
420500         END-IF                                                           
420600                                                                          
420700*   --  OM PROCESSEN SOM STARTAS ÄR SAMMA SOM DEN SOM                     
420800*   --  AVSLUTAS SÅ FÖRSTÖRS POSITIONSPEKAREN. OMLÄSNING                  
420900*   --  SKER DÄRFÖR ALLTID TILL AKTUELL POSITION.                         
421000                                                                          
421100         MOVE SPACE TO END-RES-VAERDE                                     
421200         CALL W980WSPC USING FGETF WSRKOD RP-PARM                         
421300                    USING-PARM END-RES-PARM                               
421400         PERFORM UNTIL (END-RES-VAERDE = TEMP-RES-VAERDE AND              
421500                        END-RES-LENGD  = TEMP-RES-LENGD) OR               
421600                        WSRKOD NOT = SPACE                                
421700                                                                          
421800           MOVE SPACE TO END-RES-VAERDE                                   
421900           CALL W980WSPC USING FGETN WSRKOD RP-PARM                       
422000                      USING-PARM END-RES-PARM                             
422100         END-PERFORM                                                      
422200       END-IF                                                             
422300       MOVE SPACE TO END-RES-VAERDE                                       
422400       CALL W980WSPC USING FGETN WSRKOD RP-PARM                           
422500                  USING-PARM END-RES-PARM                                 
422600     END-PERFORM                                                          
422700     .                                                                    
422800     EJECT                                                                
422900 S70-LAGRA-ANR-SYMBOLER      SECTION.                                     
423000     SKIP2                                                                
423100     MOVE NEJ TO SYMB-FEL                                                 
423200     MOVE HP-PARM TO P-PARM                                               
423300     CALL W980WSPC USING FGETF WSRKOD SOP-PARM AKT-NR-PARM                
423400         ANR-PARM                                                         
423500     IF WSRKOD = SPACE                                                    
423600       IF ANR = +32500                                                    
423700         MOVE -32500 TO ANR                                               
423800       ELSE                                                               
423900         ADD +1 TO ANR                                                    
424000       END-IF                                                             
424100     ELSE                                                                 
424200       MOVE +1 TO ANR                                                     
424300     END-IF                                                               
424400     MOVE ANR TO AKTUELLT-ANR                                             
424500     CALL W980WSPC USING FSET WSRKOD SOP-PARM AKT-NR-PARM                 
424600              ANR-PARM                                                    
424700     IF WSRKOD NOT = SPACE                                                
424800       MOVE 26 TO MSG-NR                                                  
424900       MOVE 'S70-1' TO M26-SECTION                                        
425000       MOVE WSRKOD TO M26-RKOD                                            
425100       PERFORM S90-DISPLAY-MSG                                            
425200       GO TO ERROR-EXIT                                                   
425300     END-IF                                                               
425400                                                                          
425500     IF LSOP20-TESYMBV NOT = SPACE                                        
425600       CALL W980WSPC USING FADD WSRKOD BEGAERD-TIAPDAT-PARM               
425700               AKT-NR-PARM ANR-PARM                                       
425800       IF WSRKOD NOT = SPACE                                              
425900         MOVE 26 TO MSG-NR                                                
426000         MOVE 'S70-2' TO M26-SECTION                                      
426100         MOVE WSRKOD TO M26-RKOD                                          
426200         PERFORM S90-DISPLAY-MSG                                          
426300         GO TO ERROR-EXIT                                                 
426400       END-IF                                                             
426500       MOVE 'N' TO ID-TYP                                                 
426600       PERFORM S77-LAGRA-SYMBOLER                                         
426700     END-IF                                                               
426800     .                                                                    
426900     EJECT                                                                
427000 S75-HAEMTA-SYMBOLER      SECTION.                                        
427100     SKIP2                                                                
427200     MOVE HP-PARM TO P-PARM                                               
427300     MOVE SPACE TO LSOP20-TESYMBV DATA-VAERDE                             
427400     MOVE +0 TO ANTAL-SYMBOLER                                            
427500     MOVE +1 TO RAD-IX                                                    
427600     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
427700         SYMB-PARM DATA-PARM                                              
427800     PERFORM UNTIL WSRKOD NOT = SPACE                                     
427900       ADD +1 TO ANTAL-SYMBOLER                                           
428000       MOVE SPACE TO ATTR-VAERDE                                          
428100       MOVE DATA-PARM TO ATTR-PARM                                        
428200       MOVE ALL '?' TO DATA-VAERDE                                        
428300       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
428400           ATTR-PARM DATA-PARM                                            
428500       IF WSRKOD = SPACE                                                  
428600                                                                          
428700         UNSTRING ATTR-VAERDE DELIMITED BY '&'                            
428800           INTO DEL TEMP-ATTR                                             
428900                                                                          
429000         MOVE TEMP-ATTR TO ATTR-VAERDE                                    
429100                                                                          
429200         IF ATTR-VAERDE = 'DROWP'                                         
429300           STRING 'PASSWORD("HIDDEN")' DELIMITED BY SIZE                  
429400                INTO LSOP20-TESYMBV WITH POINTER RAD-IX                   
429500           ADD +1 TO RAD-IX                                               
429600         ELSE                                                             
429700           STRING ATTR-VAERDE DELIMITED BY SPACE                          
429800                '(' DELIMITED BY SIZE                                     
429900                INTO LSOP20-TESYMBV WITH POINTER RAD-IX                   
430000                                                                          
430100           STRING DATA-VAERDE DELIMITED BY '????????'                     
430200                ')' DELIMITED BY SIZE                                     
430300                INTO LSOP20-TESYMBV WITH POINTER RAD-IX                   
430400           ADD +1 TO RAD-IX                                               
430500         END-IF                                                           
430600       END-IF                                                             
430700       MOVE SPACE TO DATA-VAERDE                                          
430800       CALL W980WSPC USING FGETN WSRKOD P-PARM                            
430900           SYMB-PARM DATA-PARM                                            
431000     END-PERFORM                                                          
431100     .                                                                    
431200     EJECT                                                                
431300 S76-HAEMTA-SYMB-VAERDE  SECTION.                                         
431400     SKIP2                                                                
431500     MOVE SPACE TO TEMP-RAD                                               
431600     MOVE +1 TO JCL-IX RAD-IX                                             
431700     MOVE PDS-RECORD-AREA TO JCL-RAD SYMB-RAD                             
431800                                                                          
431900     IF ANTAL-SYMB-VAR > 0                                                
432000       MOVE SPACE TO JCL-RAD                                              
432100       PERFORM UNTIL JCL-IX > 71 OR RAD-IX > 80                           
432200         ADD +1 JCL-IX GIVING JCL-IX-PLUS                                 
432300         SUBTRACT +1 FROM JCL-IX GIVING JCL-IX-MINUS                      
432400         IF JCL-IX-MINUS > 0                                              
432500           MOVE TECKEN (JCL-IX-MINUS) TO TECKEN-M                         
432600         ELSE                                                             
432700           MOVE SPACE TO TECKEN-M                                         
432800         END-IF                                                           
432900         IF JCL-IX < 80           AND                                     
433000            TECKEN (JCL-IX) = '&' AND                                     
433100            TECKEN (JCL-IX-PLUS) NOT = '&'  AND                           
433200            TECKEN-M NOT = '&'                                            
433300           MOVE +1 TO SYMB-IX                                             
433400           MOVE SPACE TO ATTR-VAERDE                                      
433500           PERFORM UNTIL JCL-IX > 71 OR TECKEN (JCL-IX) = SPACE           
433600           OR '/' OR '''' OR '(' OR ')' OR '.' OR ','                     
433700             MOVE TECKEN (JCL-IX) TO ATTR-VAERDE (SYMB-IX:1)              
433800             ADD +1 TO JCL-IX SYMB-IX                                     
433900           END-PERFORM                                                    
434000           IF ATTR-VAERDE = '&USER'                                       
434100             MOVE +7 TO ATTR-LENGD                                        
434200*          -- SECURITY FIX - NOT POSSIBLE TO DISPLAY "DROWP"              
434300*          -- IT IS CHANGED TO "PASSWORD" WHICH DOES NOT EXIST            
434400           ELSE IF ATTR-VAERDE = '&DROWP'                                 
434500             MOVE '&PASSWORD' TO ATTR-VAERDE                              
434600             MOVE +11 TO ATTR-LENGD                                       
434700           ELSE                                                           
434800             MOVE +0 TO ATTR-LENGD                                        
434900             INSPECT ATTR-VAERDE TALLYING ATTR-LENGD FOR                  
435000                 CHARACTERS BEFORE INITIAL SPACE                          
435100             ADD +2 TO ATTR-LENGD                                         
435200           END-IF                                                         
435300           END-IF                                                         
435400                                                                          
435500           IF ATTR-LENGD > 3                                              
435600             PERFORM S76A-HAEMTA-SYMB-VAERDE                              
435700             IF SYMB-FOUND = JA                                           
435800*              -- BYT SYMB PARM MOT DESS VÄRDE                            
435900               SUBTRACT 2 FROM DATA-LENGD                                 
436000               IF DATA-LENGD > 0                                          
436100                 IF RAD-IX + DATA-LENGD > 80                              
436200                   COMPUTE DATA-LENGD = 81 - RAD-IX                       
436300                 END-IF                                                   
436400                 MOVE DATA-VAERDE (1:DATA-LENGD)                          
436500                          TO JCL-RAD (RAD-IX:DATA-LENGD)                  
436600                 ADD DATA-LENGD TO RAD-IX                                 
436700               END-IF                                                     
436800               IF TECKEN (JCL-IX) = '.'                                   
436900                 ADD +1 TO JCL-IX                                         
437000               END-IF                                                     
437100             ELSE                                                         
437200*             -- EJ FUNNEN - LÅT SYMB PARM STÅ KVAR                       
437300               SUBTRACT 2 FROM ATTR-LENGD                                 
437400               IF ATTR-LENGD > 0                                          
437500                 IF RAD-IX + ATTR-LENGD > 80                              
437600                   COMPUTE ATTR-LENGD = 81 - RAD-IX                       
437700                 END-IF                                                   
437800                 MOVE ATTR-VAERDE (1:ATTR-LENGD)                          
437900                      TO JCL-RAD (RAD-IX:ATTR-LENGD)                      
438000                 ADD ATTR-LENGD TO RAD-IX                                 
438100               END-IF                                                     
438200             END-IF                                                       
438300           ELSE                                                           
438400*            -- ENDAST & - LÅT DEN STÅ KVAR                               
438500             SUBTRACT 2 FROM ATTR-LENGD                                   
438600             IF ATTR-LENGD > 0                                            
438700               MOVE ATTR-VAERDE (1:ATTR-LENGD)                            
438800                    TO JCL-RAD (RAD-IX:ATTR-LENGD)                        
438900               ADD ATTR-LENGD TO RAD-IX                                   
439000             END-IF                                                       
439100           END-IF                                                         
439200         ELSE                                                             
439300           MOVE TECKEN (JCL-IX) TO JCL-RAD (RAD-IX:1)                     
439400           ADD +1 TO JCL-IX RAD-IX                                        
439500         END-IF                                                           
439600       END-PERFORM                                                        
439700     END-IF                                                               
439800     IF USER-VAR > 0                                                      
439900       MOVE SPACE TO FILL                                                 
440000       UNSTRING JCL-RAD DELIMITED BY 'USER=?' OR 'USER=*'                 
440100           INTO TEMP-RAD COUNT IN RAD-IX                                  
440200           FILL                                                           
440300       MOVE '&USER' TO ATTR-VAERDE                                        
440400       MOVE +7 TO ATTR-LENGD                                              
440500       PERFORM S76A-HAEMTA-SYMB-VAERDE                                    
440600       ADD +1 TO RAD-IX                                                   
440700       MOVE 'USER='                                                       
440800                TO TEMP-RAD (RAD-IX:5)                                    
440900       COMPUTE RAD-IX = RAD-IX + 5                                        
441000       SUBTRACT 2 FROM DATA-LENGD                                         
441100       IF DATA-LENGD > 0                                                  
441200         MOVE DATA-VAERDE (1:DATA-LENGD)                                  
441300                  TO TEMP-RAD (RAD-IX:DATA-LENGD)                         
441400         COMPUTE RAD-IX = RAD-IX + DATA-LENGD                             
441500       END-IF                                                             
441600       MOVE FILL TO TEMP-RAD (RAD-IX:80)                                  
441700       MOVE SPACE TO JCL-RAD                                              
441800       MOVE TEMP-RAD TO JCL-RAD                                           
441900     END-IF                                                               
442000     IF PASSW-VAR > 0                                                     
442100       MOVE SPACE TO FILL                                                 
442200       UNSTRING JCL-RAD DELIMITED BY 'PASSWORD=?' OR 'PASSWORD=*'         
442300           INTO TEMP-RAD COUNT IN RAD-IX                                  
442400           FILL                                                           
442500       MOVE '&DROWP' TO ATTR-VAERDE                                       
442600       MOVE +8 TO ATTR-LENGD                                              
442700       PERFORM S76A-HAEMTA-SYMB-VAERDE                                    
442800       ADD +1 TO RAD-IX                                                   
442900       MOVE 'PASSWORD='                                                   
443000                TO TEMP-RAD (RAD-IX:9)                                    
443100       COMPUTE RAD-IX = RAD-IX + 9                                        
443200       SUBTRACT 2 FROM DATA-LENGD                                         
443300       IF DATA-LENGD > 0                                                  
443400         MOVE DATA-VAERDE (1:DATA-LENGD)                                  
443500                  TO TEMP-RAD (RAD-IX:DATA-LENGD)                         
443600         COMPUTE RAD-IX = RAD-IX + DATA-LENGD                             
443700       END-IF                                                             
443800       MOVE FILL TO TEMP-RAD (RAD-IX:80)                                  
443900       MOVE SPACE TO JCL-RAD                                              
444000       MOVE TEMP-RAD TO JCL-RAD                                           
444100     END-IF                                                               
444200     MOVE JCL-RAD TO PDS-RECORD-AREA                                      
444300     .                                                                    
444400     EJECT                                                                
444500 S76A-HAEMTA-SYMB-VAERDE  SECTION.                                        
444600     SKIP2                                                                
444700     MOVE P-PARM TO TEMP-PARM NAMN-PARM                                   
444800     MOVE NEJ TO SYMB-FOUND                                               
444900     MOVE ALL '?' TO DATA-VAERDE                                          
445000     MOVE SPACE TO WSRKOD                                                 
445100                                                                          
445200     EVALUATE ATTR-VAERDE                                                 
445300     WHEN  '&SYSCLIENT'                                                   
445400       CALL W980USER USING DATA-VAERDE                                    
445500       MOVE 2 TO DATA-LENGD                                               
445600       INSPECT DATA-VAERDE TALLYING DATA-LENGD                            
445700               FOR CHARACTERS BEFORE INITIAL SPACE                        
445800       MOVE JA TO SYMB-FOUND                                              
445900     WHEN  '&SYSDATE'                                                     
446000       MOVE FUNCTION CURRENT-DATE(3:6) TO DATA-VAERDE                     
446100       MOVE 8 TO DATA-LENGD                                               
446200       MOVE JA TO SYMB-FOUND                                              
446300     WHEN  '&SYSTIME'                                                     
446400       MOVE FUNCTION CURRENT-DATE(9:6) TO DATA-VAERDE                     
446500       MOVE 8 TO DATA-LENGD                                               
446600       MOVE JA TO SYMB-FOUND                                              
446700     WHEN  '&USER'                                                        
446800     WHEN  '&DROWP'                                                       
446900       CALL W980WSPC USING FGETF WSRKOD SOP-PARM                          
447000              ATTR-PARM DATA-PARM                                         
447100       IF WSRKOD = SPACE                                                  
447200         MOVE JA TO SYMB-FOUND                                            
447300         IF ATTR-VAERDE = '&DROWP'                                        
447400           IF DATA-LENGD = 2                                              
447500           OR DATA-VAERDE(1:DATA-LENGD - 2) NOT NUMERIC                   
447600             CONTINUE                                                     
447700*            -- DATA-VAERDE ANVÄNDS SOM PASSWORD                          
447800           ELSE                                                           
447900             MOVE DATA-VAERDE(1:DATA-LENGD - 2) TO PASS-NUMBER            
448000                                                                          
448100*            -- GENERERA LÖSENORDET FRÅN TALET                            
448200             CALL W980PASS USING PASS-AREA                                
448300                                                                          
448400             MOVE PASS-WORD   TO DATA-VAERDE                              
448500             MOVE 2 TO DATA-LENGD                                         
448600             INSPECT DATA-VAERDE TALLYING DATA-LENGD                      
448700                     FOR CHARACTERS BEFORE INITIAL SPACE                  
448800           END-IF                                                         
448900         END-IF                                                           
449000       END-IF                                                             
449100     WHEN OTHER                                                           
449200       PERFORM UNTIL WSRKOD NOT = SPACE OR SYMB-FOUND = JA                
449300         PERFORM S80-HAEMTA-PROCESS-INFO                                  
449400         MOVE KDANR TO ANR                                                
449500         CALL W980WSPC USING FGETF WSRKOD ANR-PARM                        
449600                ATTR-PARM DATA-PARM                                       
449700         IF WSRKOD = SPACE AND (KDPROCSTAT = 'S' OR 'W')                  
449800*          -- TA SYMBOLVÄRDE FRÅN SENASTE AKTIVERING                      
449900*          -- BARA OM PROCESSEN ÄR STARTAD                                
450000           MOVE JA TO SYMB-FOUND                                          
450100         ELSE                                                             
450200*          -- FÖRSÖK MED FAST SYMBOLVÄRDE ISTÄLLET                        
450300           CALL W980WSPC USING FGETF WSRKOD P-PARM                        
450400               ATTR-PARM DATA-PARM                                        
450500           IF WSRKOD = SPACE                                              
450600             MOVE JA TO SYMB-FOUND                                        
450700           ELSE                                                           
450800*            -- GÅ VIDARE TILL FÖRÄLDERN ELLER *SOP*                      
450900             MOVE +0 TO DATA-LENGD                                        
451000             IF P-PARM NOT = SOP-PARM                                     
451100               CALL W980WSPC USING FGETF WSRKOD P-PARM                    
451200                   PARENT-PARM DATA-PARM                                  
451300               IF WSRKOD  = SPACE                                         
451400                 MOVE DATA-PARM TO P-PARM NAMN-PARM                       
451500               ELSE                                                       
451600                 MOVE SOP-PARM  TO P-PARM NAMN-PARM                       
451700                 MOVE SPACE TO WSRKOD                                     
451800               END-IF                                                     
451900             END-IF                                                       
452000             MOVE ALL '?' TO DATA-VAERDE                                  
452100           END-IF                                                         
452200         END-IF                                                           
452300       END-PERFORM                                                        
452400     END-EVALUATE                                                         
452500*                                                                         
452600     MOVE TEMP-PARM TO P-PARM NAMN-PARM                                   
452700     IF SYMB-FOUND = NEJ                                                  
452800       MOVE 47 TO MSG-NR                                                  
452900       MOVE ATTR-VAERDE TO M47-SYMBOL                                     
453000       MOVE P-IDPROCESS TO M47-IDPROCESS                                  
453100       PERFORM S90-DISPLAY-MSG                                            
453200     END-IF                                                               
453300     .                                                                    
453400     EJECT                                                                
453500 S77-LAGRA-SYMBOLER      SECTION.                                         
453600     SKIP2                                                                
453700     MOVE NEJ TO SYMB-FEL                                                 
453800     MOVE +0 TO ANT-RIGHT ANT-LEFT                                        
453900     MOVE +1 TO RAD-IX                                                    
454000     MOVE LSOP20-TESYMBV TO T-RAD                                         
454100     INSPECT LSOP20-TESYMBV TALLYING ANT-LEFT FOR ALL '('                 
454200     INSPECT LSOP20-TESYMBV TALLYING ANT-RIGHT FOR ALL ')'                
454300                                                                          
454400     IF ANT-LEFT NOT = ANT-RIGHT                                          
454500       MOVE JA TO SYMB-FEL                                                
454600       MOVE +48 TO MSG-NR                                                 
454700       MOVE 'UNMATCHED PARANTHES IN SYMBOL STRING' TO                     
454800                M48-ORSAK                                                 
454900       MOVE P-IDPROCESS TO M48-IDPROCESS                                  
455000       PERFORM S90-DISPLAY-MSG                                            
455100     ELSE IF ANT-LEFT = 0                                                 
455200       MOVE JA TO SYMB-FEL                                                
455300       MOVE +48 TO MSG-NR                                                 
455400       MOVE 'MISSING SYMBOL VALUE STRING' TO                              
455500                M48-ORSAK                                                 
455600       MOVE P-IDPROCESS TO M48-IDPROCESS                                  
455700       PERFORM S90-DISPLAY-MSG                                            
455800     ELSE                                                                 
455900       PERFORM UNTIL ANT-LEFT = 0                                         
456000         MOVE NEJ TO SYMB-FEL                                             
456100         IF T (RAD-IX) NOT = SPACE                                        
456200           SUBTRACT +1 FROM ANT-LEFT                                      
456300           MOVE '&' TO SYMBOL                                             
456400           MOVE +2 TO SYMB-IX                                             
456500           PERFORM UNTIL T (RAD-IX) = '(' OR SYMB-IX > 21                 
456600             STRING T (RAD-IX) DELIMITED BY SIZE                          
456700                  INTO SYMBOL WITH POINTER SYMB-IX                        
456800             ADD +1 TO RAD-IX                                             
456900           END-PERFORM                                                    
457000                                                                          
457100           IF SYMB-IX = 1                                                 
457200             MOVE JA TO SYMB-FEL                                          
457300             MOVE +49 TO MSG-NR                                           
457400             MOVE P-IDPROCESS TO M49-IDPROCESS                            
457500             MOVE 'MISSING SYMBOL NAME IN SYMBOL STRING' TO               
457600                     M49-ORSAK                                            
457700             PERFORM S90-DISPLAY-MSG                                      
457800           ELSE IF SYMB-IX > 21                                           
457900             MOVE JA TO SYMB-FEL                                          
458000             MOVE +49 TO MSG-NR                                           
458100             MOVE P-IDPROCESS TO M49-IDPROCESS                            
458200             MOVE 'SYMBOL LONGER THAN 20 CHARACTERS' TO                   
458300                     M49-ORSAK                                            
458400             PERFORM S90-DISPLAY-MSG                                      
458500           END-IF                                                         
458600           END-IF                                                         
458700                                                                          
458800           MOVE SYMBOL TO DATA-VAERDE                                     
458900           ADD 1 SYMB-IX GIVING DATA-LENGD                                
459000           ADD +1 TO RAD-IX                                               
459100           MOVE +1 TO SYMB-IX                                             
459200           MOVE SPACE TO SYMBOL-VAERDE                                    
459300           PERFORM UNTIL T (RAD-IX) = ')' OR SYMB-IX > 81                 
459400             STRING T (RAD-IX) DELIMITED BY SIZE                          
459500                  INTO SYMBOL-VAERDE WITH POINTER SYMB-IX                 
459600             ADD +1 TO RAD-IX                                             
459700           END-PERFORM                                                    
459800                                                                          
459900           IF SYMBOL = '&PASSWORD' AND                                    
460000                  SYMBOL-VAERDE = '"HIDDEN"'                              
460100             MOVE JA TO SYMB-FEL                                          
460200             MOVE +50 TO MSG-NR                                           
460300             MOVE SYMBOL TO M50-SYMBOL                                    
460400             PERFORM S90-DISPLAY-MSG                                      
460500           END-IF                                                         
460600                                                                          
460700           IF SYMB-FEL = NEJ                                              
460800             IF P-ID                                                      
460900               PERFORM S77A-LAGRA-SYMB-VAERDE                             
461000             ELSE IF NUM-ID                                               
461100               PERFORM S77B-LAGRA-SYMB-VAERDE                             
461200             ELSE                                                         
461300               MOVE 26 TO MSG-NR                                          
461400               MOVE 'S77' TO M26-SECTION                                  
461500               MOVE WSRKOD TO M26-RKOD                                    
461600               PERFORM S90-DISPLAY-MSG                                    
461700               GO TO ERROR-EXIT                                           
461800             END-IF                                                       
461900             END-IF                                                       
462000           END-IF                                                         
462100         END-IF                                                           
462200         ADD +1 TO RAD-IX                                                 
462300       END-PERFORM                                                        
462400     END-IF                                                               
462500     END-IF                                                               
462600     .                                                                    
462700     EJECT                                                                
462800 S77A-LAGRA-SYMB-VAERDE  SECTION.                                         
462900     SKIP2                                                                
463000     IF DATA-VAERDE = '&PASSWORD'                                         
463100       MOVE '&DROWP' TO DATA-VAERDE                                       
463200       MOVE +8 TO DATA-LENGD                                              
463300     END-IF                                                               
463400                                                                          
463500     CALL W980WSPC USING FTEST WSRKOD P-PARM SYMB-PARM                    
463600             DATA-PARM                                                    
463700                                                                          
463800     IF SYMBOL-VAERDE = '^'                                               
463900       IF WSRKOD = SPACE                                                  
464000         CALL W980WSPC USING FDEL WSRKOD P-PARM SYMB-PARM                 
464100                 DATA-PARM                                                
464200         MOVE DATA-PARM TO ATTR-PARM                                      
464300         CALL W980WSPC USING FDELK WSRKOD P-PARM                          
464400             ATTR-PARM                                                    
464500         MOVE 52 TO MSG-NR                                                
464600         MOVE SYMBOL TO M52-SYMBOL                                        
464700         PERFORM S90-DISPLAY-MSG                                          
464800         CALL W980WSPC USING FGETF WSRKOD P-PARM SYMB-PARM                
464900                 DATA-PARM                                                
465000         IF WSRKOD NOT = SPACE                                            
465100           CALL W980WSPC USING FDELK WSRKOD P-PARM SYMB-PARM              
465200         END-IF                                                           
465300       ELSE                                                               
465400         MOVE 47 TO MSG-NR                                                
465500         MOVE SYMBOL TO M47-SYMBOL                                        
465600         MOVE P-IDPROCESS TO M47-IDPROCESS                                
465700         PERFORM S90-DISPLAY-MSG                                          
465800       END-IF                                                             
465900     ELSE                                                                 
466000       IF WSRKOD NOT = SPACE                                              
466100         CALL W980WSPC USING FADD WSRKOD P-PARM SYMB-PARM                 
466200                   DATA-PARM                                              
466300       END-IF                                                             
466400                                                                          
466500       MOVE DATA-PARM TO ATTR-PARM                                        
466600       IF SYMBOL-VAERDE = SPACE                                           
466700         MOVE SPACE TO DATA-VAERDE                                        
466800         MOVE +2 TO DATA-LENGD                                            
466900       ELSE                                                               
467000         MOVE SPACE TO SYMB-RAD                                           
467100         MOVE SYMBOL-VAERDE TO SYMB-RAD                                   
467200         MOVE SYMBOL-VAERDE TO DATA-VAERDE                                
467300         ADD 1 SYMB-IX GIVING DATA-LENGD                                  
467400       END-IF                                                             
467500       CALL W980WSPC USING FSET WSRKOD P-PARM                             
467600           ATTR-PARM DATA-PARM                                            
467700       IF WSRKOD = SPACE                                                  
467800         MOVE 51 TO MSG-NR                                                
467900         MOVE SYMBOL TO M51-SYMBOL                                        
468000         PERFORM S90-DISPLAY-MSG                                          
468100       ELSE                                                               
468200         MOVE 26 TO MSG-NR                                                
468300         MOVE 'S77A' TO M26-SECTION                                       
468400         MOVE WSRKOD TO M26-RKOD                                          
468500         PERFORM S90-DISPLAY-MSG                                          
468600         GO TO ERROR-EXIT                                                 
468700       END-IF                                                             
468800     END-IF                                                               
468900     .                                                                    
469000     EJECT                                                                
469100 S77B-LAGRA-SYMB-VAERDE  SECTION.                                         
469200     SKIP2                                                                
469300     IF DATA-VAERDE = '&PASSWORD'                                         
469400       MOVE '&DROWP' TO DATA-VAERDE                                       
469500       MOVE +8 TO DATA-LENGD                                              
469600     END-IF                                                               
469700                                                                          
469800     CALL W980WSPC USING FADD WSRKOD ANR-PARM SYMB-PARM                   
469900                 DATA-PARM                                                
470000                                                                          
470100     IF WSRKOD NOT = SPACE                                                
470200       MOVE 26 TO MSG-NR                                                  
470300       MOVE 'S77B-1' TO M26-SECTION                                       
470400       MOVE WSRKOD TO M26-RKOD                                            
470500       PERFORM S90-DISPLAY-MSG                                            
470600       GO TO ERROR-EXIT                                                   
470700     END-IF                                                               
470800     MOVE DATA-PARM TO ATTR-PARM                                          
470900     MOVE SPACE TO SYMB-RAD                                               
471000     MOVE SYMBOL-VAERDE TO SYMB-RAD                                       
471100     MOVE SYMBOL-VAERDE TO DATA-VAERDE                                    
471200     ADD 1 SYMB-IX GIVING DATA-LENGD                                      
471300     CALL W980WSPC USING FSET WSRKOD ANR-PARM                             
471400         ATTR-PARM DATA-PARM                                              
471500     IF WSRKOD = SPACE                                                    
471600       MOVE 51 TO MSG-NR                                                  
471700       MOVE SYMBOL TO M51-SYMBOL                                          
471800       PERFORM S90-DISPLAY-MSG                                            
471900     ELSE                                                                 
472000       MOVE 26 TO MSG-NR                                                  
472100       MOVE 'S77B-2' TO M26-SECTION                                       
472200       MOVE WSRKOD TO M26-RKOD                                            
472300       PERFORM S90-DISPLAY-MSG                                            
472400       GO TO ERROR-EXIT                                                   
472500     END-IF                                                               
472600     .                                                                    
472700     EJECT                                                                
472800 S80-HAEMTA-PROCESS-INFO  SECTION.                                        
472900     SKIP2                                                                
473000     MOVE LOW-VALUE TO INF-VAERDE                                         
473100     CALL W980WSPC USING FGETF WSRKOD NAMN-PARM                           
473200                 INFO-NAMN-PARM INFO-VAERDE-PARM                          
473300     IF WSRKOD NOT = SPACE                                                
473400       MOVE 25 TO MSG-NR                                                  
473500       MOVE NAMN-VAERDE TO M25-IDPROCESS                                  
473600       PERFORM S90-DISPLAY-MSG                                            
473700       GO TO ERROR-EXIT                                                   
473800     END-IF                                                               
473900     .                                                                    
474000     EJECT                                                                
474100 S90-DISPLAY-MSG  SECTION.                                                
474200     SKIP2                                                                
474300     IF MSG-RETCODE (MSG-NR) NOT < LSOP20-KDRET                           
474400       MOVE MSG-NR TO LSOP20-KDMEDD                                       
474500       MOVE MSG-RETCODE (MSG-NR) TO LSOP20-KDRET                          
474600     END-IF                                                               
474700                                                                          
474800     MOVE SPACE TO DATA2-VAERDE                                           
474900     CALL W980WSPC USING FGETF WSRKOD2 SOP-PARM                           
475000                 DB-STATUS-PARM  DATA2-PARM                               
475100     IF WSRKOD2 = SPACE AND DATA2-VAERDE = 'TEST'                         
475200       MOVE 'T' TO WTOP-DB-STATUS                                         
475300     ELSE                                                                 
475400       MOVE 'P' TO WTOP-DB-STATUS                                         
475500     END-IF                                                               
475600                                                                          
475700     MOVE 'SOPXXX' TO WTOP-MSG-ID                                         
475800     MOVE MSG-NR TO WTOP-MSG-NR                                           
475900     MOVE MSG1 (MSG-NR) TO WTOP-MSG-TEXT                                  
476000     IF SPAR-FLBATCH = JA                                                 
476100       DISPLAY WTOP-MSG                                                   
476200     END-IF                                                               
476300     CALL W009WTOP USING WTOP-PARM                                        
476400     IF MSG2 (MSG-NR) NOT = SPACE                                         
476500       MOVE SPACE TO WTOP-MSG-ID                                          
476600       MOVE MSG2 (MSG-NR) TO WTOP-MSG-TEXT                                
476700       IF SPAR-FLBATCH = JA                                               
476800         DISPLAY WTOP-MSG                                                 
476900       END-IF                                                             
477000       CALL W009WTOP USING WTOP-PARM                                      
477100     END-IF                                                               
477200     .                                                                    
477300     EJECT                                                                
477400*    -COPY WY2000P1                                                       
