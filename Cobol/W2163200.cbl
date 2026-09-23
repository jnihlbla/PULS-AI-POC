000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2163200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   00/01/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        NEDLÄSNING HTR WDR301                                            
001000*      - SKROTNING CLASSIC > IDCPYTXT=W21632                              
001010*      - DEMANDS FOR CANCELATION SI+ >IDCPYTXT=W114H02A                   
001100*                                                                         
001200*        PROGRAMMET LÄSER WDR3                                            
001300*                         WDD3                                            
001400*                   SKAPAR FIL W21631 LISTFIL                             
001500*                              W21633 FÖR BORTTAG HTR I W01533            
001600*                                                                         
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- LISTFIL                                                    
002500     SELECT W21631                     ASSIGN TO W21632D1.                
002700*          --- BORTTAG HTR                                                
002800     SELECT W21633                     ASSIGN TO W21632D2.                
002820*          --- VECKO KOPIA                                                
002830     SELECT W21637                     ASSIGN TO W21632D3.                
002840*          --- SI+ CANCELATION                                            
002850     SELECT W21634                     ASSIGN TO W21632D4.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  W21631                                                               
003500     RECORDING       V                                                    
003600     RECORD VARYING FROM 1 DEPENDING ON W21631-LENGTH                     
003700     BLOCK CONTAINS  0.                                                   
003800 01  UT-HEAD-LINE.                                                        
003900     03  FILLER       PIC X(400) VALUE SPACE.                             
004000     EJECT                                                                
004100 01  UT-POST.                                                             
004200*    03  FILLER -COPY W21631     -L.                                      
004300     03  FILLER       PIC X(600).                                         
004400     EJECT                                                                
004500                                                                          
004600 FD  W21633                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900*01  POST -COPY WDR301  -PRE  BORT-   -L.                                 
005000     EJECT                                                                
005010                                                                          
005020 FD  W21637                                                               
005030     RECORDING       F                                                    
005040     BLOCK CONTAINS  0.                                                   
005050*01  POST -COPY W21632  -PRE  VECKO-   -L.                                
005060     EJECT                                                                
005070                                                                          
005080 FD  W21634                                                               
005090     RECORDING       F                                                    
005091     BLOCK CONTAINS  0.                                                   
005092*01  POST -COPY W1145A  -PRE  H02-   -L.                                  
005093     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200*    -- CHECKED BY WY2000                                                 
005300*                                                                         
005400 01  IDPGM                       PIC X(8)   VALUE 'W2163200'.             
005500 01   FILLER                     PIC X(16)  VALUE 'ABEND-HELP'.           
005600 01   ABEND-HELP                 PIC X(80)  VALUE SPACE.                  
005700 01   FILLER                     PIC X(16)  VALUE 'IMS-CALL'.             
005800 01   IMS-CALL                   PIC X(80)  VALUE SPACE.                  
005900*                                                                         
006000 01  JA                          PIC X       VALUE 'J'.                   
006100 01  NEJ                         PIC X       VALUE 'N'.                   
006200 01  TAB                         PIC X       VALUE X'5E'.                 
006300*                                                                         
006400 01  IX-AR                       PIC 9(9)    COMP-3 VALUE ZERO.           
006500 01  IX-PER                      PIC 9(9)    COMP-3 VALUE ZERO.           
006600 01  IX-FRAN-AR                  PIC 9(9)    COMP-3 VALUE ZERO.           
006700 01  IX-TILL-AR                  PIC 9(9)    COMP-3 VALUE ZERO.           
006800 01  P-INDX                      PIC 9(9)    COMP-3 VALUE ZERO.           
006900 01  P-MAX-100-INDX              PIC 9(9)    VALUE 100.                   
007000 01  W21631-LENGTH               PIC 9(3)    COMP-3 VALUE ZERO.           
007100*                                                                         
007200 01    TOMMA-RADER-FINNS         PIC X       VALUE 'J'.                   
007300 01    MAX-LINE                  PIC S9(9)   COMP-3 VALUE ZERO.           
007400 01   FILLER                     PIC X(9)    VALUE ' LINE-IX='.           
007500 01    MOD-IX-LINE               PIC S9(9)   COMP-3 VALUE ZERO.           
007600 01    MAX-COL                   PIC S9(9)   COMP-3 VALUE ZERO.           
007700 01   FILLER                     PIC X(8)    VALUE ' COL-IX='.            
007800 01    MOD-IX-COL                PIC S9(9)   COMP-3 VALUE ZERO.           
007900 01    INDX                      PIC S9(9)   COMP-3 VALUE ZERO.           
008000 01   FILLER                     PIC X(8)    VALUE ' ITERIX='.            
008100 01    ITERIX                    PIC 9(3)    VALUE ZERO.                  
008200 01  WS-KDPRODSL                 PIC 99      VALUE ZERO.                  
008300 01  WS-KAT-IDKATNR              PIC 9(5)    VALUE ZERO.                  
008310 01  WS-PRKURS2                  PIC S9(6)V9(5) VALUE ZERO COMP-3.        
008320 01  WS-PRKURS                   PIC S9(6)V9(5)      COMP-3.              
008330 01  WS-KDVALISO2                PIC X(3)   VALUE SPACE.                  
008400*                                                                         
008500 01  WS-PRARTSTD-DISP            PIC 9(7)V9(2) VALUE ZERO.                
008510*                                                                         
008520 01  WS-UT-PRARTSTD.                                                      
008600   03  WS-PRARTSTD-HELTAL        PIC X(6)    VALUE ZERO.                  
008700   03  WS-PRARTSTD-PUNKT         PIC X(1)    VALUE ','.                   
008800   03  WS-PRARTSTD-DECIMAL       PIC X(2)    VALUE ZERO.                  
009000*                                                                         
009010 01  WS-PRARTBTO-MARK-DISP       PIC 9(7)V9(2) VALUE ZERO.                
009020*                                                                         
009100 01  WS-UT-PRARTBTO-MARK.                                                 
009110   03  WS-PRARTBTO-MARK-HELTAL   PIC X(6)    VALUE ZERO.                  
009120   03  WS-PRARTBTO-MARK-PUNKT    PIC X(1)    VALUE ','.                   
009130   03  WS-PRARTBTO-MARK-DECIMAL  PIC X(2)    VALUE ZERO.                  
009140*                                                                         
009150 01  WS-VKART-DISP               PIC 9(7)    VALUE ZERO.                  
009151*                                                                         
009152 01  WS-UT-VKART.                                                         
009160   03  WS-VKART-HELTAL           PIC X(3)    VALUE ZERO.                  
009170   03  WS-VKART-PUNKT            PIC X(1)    VALUE ','.                   
009180   03  WS-VKART-DECIMAL          PIC X(3)    VALUE ZERO.                  
009200*                                                                         
009300*01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009400 01  DAGENS-PER                  PIC  9(4)   VALUE ZERO.                  
009500 01  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
009600*                                                                         
009700 01  FILLER                      PIC X(04)   VALUE 'WS'.                  
009800 01  WS.                                                                  
009900  05 WS-DAGENS-AAAAMMDD          PIC 9(8)    VALUE ZERO.                  
010000  05 FILLER                      PIC X(08)   VALUE 'WS-AAPP'.             
010100  05 WS-AAPP                     PIC 9(4)    VALUE ZERO.                  
010200  05 FILLER REDEFINES            WS-AAPP.                                 
010300   10 WS-AA                      PIC 9(2).                                
010400   10 WS-PP                      PIC 9(2).                                
010500*                                                                         
010600  05 WS-VAL                      PIC X(1)    VALUE SPACE.                 
010700  05 WS-AARTAL                   PIC 9(4)    VALUE ZERO.                  
010800  05 WS-PER                      PIC 9(2)    VALUE ZERO.                  
010900  05 WS-KVOI-RED                 PIC 9(7)    VALUE ZERO.                  
011000  05 WS-TOTAL-KVOI               PIC 9(7)    VALUE ZERO.                  
011100  05 WS-KVOI-PER         OCCURS 3 TIMES.                                  
011200   10 WS-KVOI-TOT-TAB            PIC 9(7)        VALUE ZERO.              
011300   10 WS-KVOI-TAB        OCCURS 12 TIMES                                  
011400                                 PIC 9(7)        VALUE ZERO.              
011500*                                                                         
011600  05 WS-VV                       PIC  9(2)   VALUE ZERO.                  
011700  05 FILLER                      PIC  X(16)  VALUE 'WS-TABELL'.           
011800  05  WS-TABELL    OCCURS 12.                                             
011900   10 WS-FORSTA-V                PIC  9(2)   VALUE ZERO.                  
012000   10 WS-SISTA-V                 PIC  9(2)   VALUE ZERO.                  
012100   10 WS-KVVIPER                 PIC 9       VALUE ZERO.                  
012200   10 WS-KVOI                    PIC S9(7)   VALUE ZERO COMP-3.           
012300*                                                                         
012400  05 WS-TESTFAELT.                                                        
012500   10 WS-FORSTA-TF               PIC  9(2)   VALUE ZERO.                  
012600   10 FILLER                     PIC  X      VALUE SPACE.                 
012700   10 WS-SISTA-TF                PIC  9(2)   VALUE ZERO.                  
012800   10 FILLER                     PIC  X      VALUE SPACE.                 
012900   10 WS-KVOI-TF                 PIC  9(7)   VALUE ZERO.                  
013000*                                                                         
013100 01  FELTEXT.                                                             
013200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013400                                                                          
013500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013600 01  FILLER REDEFINES DAGENS-DATUM.                                       
013700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014000     EJECT                                                                
014100 01  DYNAMISKA-SUBPROGRAM.                                                
014200*                                                                         
014300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014710     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL POSTSUM                                          
015000*                                                                         
015100*01  -COPY W0005   -PRE  POSTSUM-                                         
015200     EJECT                                                                
015300*                                                                         
015400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
015500*01  -COPY WDATAREA                                                       
015600*                                                                         
015700*    --- PARAMETERS FOR PRINT SUBROUTINE W006PRT                          
015800*01  -COPY W006PRT                                                        
015900     EJECT                                                                
015910*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
015920*01 -COPY W335CURR                                                        
015930     EJECT                                                                
016000                                                                          
016100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016400     SKIP3                                                                
016500*                                                                         
016600 01  WS-AREA-START               PIC X(16)   VALUE                        
016700                                             'WS-AREA-START'.             
016800 01  WS-AREA.                                                             
016900*    03  FILLER -COPY W21632  -PRE WS-                                    
017000*                                                                         
017100*                                                                         
017200 01  UT-AREA-START               PIC X(16)   VALUE                        
017300                                             'UT-AREA-START'.             
017400 01  UT-AREA.                                                             
017500*    03  FILLER -COPY W21631  -PRE UT-                                    
017600     03  UT-IDKATNR-AREA         PIC X(600).                              
017700     EJECT                                                                
017800                                                                          
017900 01  FILLER                      PIC X(16)   VALUE                        
018000                                             'UT-HEAD-START'.             
018100 01  UT-HEADLINE.                                                         
018200     03  UT-HEADLINE-AREA        PIC X(432)  VALUE SPACE.                 
018300     EJECT                                                                
018400*                                                                         
018500 01  BORT-AREA-START             PIC X(16)   VALUE                        
018600                                             'BORT-AREA-START'.           
018700*01  AREA -COPY WDR301      -PRE BORT-                                    
018800     EJECT                                                                
018810*                                                                         
018820 01  VECKO-AREA-START            PIC X(16)   VALUE                        
018830                                             'VECKO-AREA'.                
018840*01  AREA -COPY W21632      -PRE VECKO-                                   
018850     EJECT                                                                
018851 01  H02-AREA-START            PIC X(16)   VALUE                          
018852                                             'H02-AREA'.                  
018853*01  AREA -COPY W1145A      -PRE H02-                                     
018854     EJECT                                                                
018860*                                                                         
018900 01  NYCKLAR-TILL-DLI.                                                    
019000     03  W-WDR301KY-X.                                                    
019100         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
019200     03  W-IDCPYTXT-X.                                                    
019300         05  W-IDCPYTXT          PIC X(08)    VALUE 'W21632  '.           
019310     03  W-IDCPYTXT-H02-X.                                                
019320         05  W-IDCPYTXT-H02      PIC X(08)    VALUE 'W114H02A'.           
019400     03  W-IDARTNR-X.                                                     
019500         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
019600     03  W-IDSKYLT-X.                                                     
019700         05  W-IDSKYLT           PIC X(3)     VALUE 'GB '.                
019800     03  W-TIAAAA-X.                                                      
019900         05  W-TIAAAA            PIC 9(4)     VALUE ZERO.                 
020000     03  W-KDSEGKEY-X.                                                    
020100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
020200     03 W-IDRADNR-X.                                                      
020300         05 W-IDRADNR            PIC  S9(5)  VALUE ZERO  COMP-3.          
020400                                                                          
020500******* NYCKLAR TILL WDC1   *******************                           
020600     03  W-WDC101KY-X.                                                    
020700         05  W-IDARTNR-111       PIC S9(9)   VALUE ZERO COMP-3.           
020800         05  W-IDMARKBO-111      PIC X       VALUE SPACE.                 
020900                                                                          
021000*    --- STATUS-KOD FRÅN IMS                                              
021100 01  STATUS-WS                   PIC XX.                                  
021200     88  SEGMENT-FINNS                       VALUE '  '.                  
021300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021500     88  IMS-EJ-OK                           VALUE 'XD'.                  
021600                                                                          
021700                                                                          
021800 01  GODK-STATUSKODER.                                                    
021900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022000                                                                          
022100 01  FILLER                      PIC X(08) VALUE 'SSA-AREA'.              
022200 01  SSA1                        PIC X(96).                               
022300 01  SSA2                        PIC X(64).                               
022400     EJECT                                                                
022500*    --- IMS FUNKTIONSKODER                                               
022600*01  -COPY W0003                                                          
022700     EJECT                                                                
022800*    ---  DLI INPUT-OUTPUT AREA                                           
022900                                                                          
023000 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDR301'.                    
023100 01  DLI-IO-WDR301.                                                       
023200*    03  -COPY WDR301                                                     
023210        05  -COPY W1145A   -RED FIL-WDR301-DATA -PRE R3-                  
023300     EJECT                                                                
023400 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDD311'.                    
023500 01  DLI-IO-WDD311.                                                       
023600*    03  -COPY WDD311                                                     
023700     EJECT                                                                
023800                                                                          
023900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
024000 01  DLI-IO-WDK601.                                                       
024100*    03  -COPY WDK601                                                     
024200     EJECT                                                                
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
024400 01  DLI-IO-WDK611.                                                       
024500*    03  -COPY WDK611                                                     
024600                                                                          
024700     EJECT                                                                
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK627'.                      
024900 01  DLI-IO-WDK627.                                                       
025000*    03  -COPY WDK627                                                     
025100                                                                          
025200     SKIP3                                                                
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ101'.                      
025400 01  DLI-IO-WDJ101.                                                       
025500*  03    -COPY WDJ101                                                     
025600     SKIP3                                                                
025700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ111'.                      
025800 01  DLI-IO-WDJ111.                                                       
025900*  03    -COPY WDJ111                                                     
026000                                                                          
026100     SKIP3                                                                
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL801'.                      
026300 01  DLI-IO-WDL801.                                                       
026400*  03    -COPY WDL801                                                     
026500     SKIP3                                                                
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
026700 01  DLI-IO-WDL811.                                                       
026800*  03    -COPY WDL811                                                     
026900                                                                          
027000     SKIP3                                                                
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC101'.                      
027200 01  DLI-IO-WDC101.                                                       
027300*  03    -COPY WDC101                                                     
027400     SKIP3                                                                
027500                                                                          
027600     SKIP3                                                                
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
027800 01  DLI-IO-WDN601.                                                       
027900*  03    -COPY WDN601                                                     
028000     SKIP3                                                                
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
028200 01  DLI-IO-WDN611.                                                       
028300*  03    -COPY WDN611                                                     
028400     SKIP3                                                                
028500                                                                          
028600                                                                          
028700 LINKAGE SECTION.                                                         
028800*01  -COPY W0008  -PRE WDR3-                                              
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01  -COPY W0008  -PRE WDD3-                                              
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008  -PRE WDK6-                                              
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008  -PRE WDJ1-                                              
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008  -PRE WDC1-                                              
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008  -PRE WDL8-                                              
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008  -PRE WDN6-                                              
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900 PROCEDURE DIVISION  USING WDR3-PCB WDD3-PCB WDK6-PCB WDJ1-PCB            
031000                           WDC1-PCB WDL8-PCB WDN6-PCB.                    
031100 MAIN SECTION.                                                            
031200     ENTRY 'DLITCBL' USING WDR3-PCB WDD3-PCB WDK6-PCB WDJ1-PCB            
031300                           WDC1-PCB WDL8-PCB WDN6-PCB.                    
031400                                                                          
031500     PERFORM A-INIT                                                       
031600                                                                          
031700     PERFORM IMS-GN-WDR301                                                
031800                                                                          
032800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
032900                                                                          
032901        IF FIL-IDCPYTXT = 'W21632  '                                      
032902          PERFORM B-SKAPA-W26131                                          
035500        END-IF                                                            
035501        IF FIL-IDCPYTXT = 'W114H02A'                                      
035502          MOVE R3-W1145A  TO H02-W1145A                                   
035503          PERFORM S05-SKRIV-W21634                                        
035505        END-IF                                                            
035510        PERFORM D-SKAPA-BORTPOST                                          
035520                                                                          
035600        PERFORM IMS-GN-WDR301                                             
035700     END-PERFORM                                                          
036200                                                                          
036300     PERFORM Z-FINIT                                                      
036400                                                                          
036500     MOVE ZERO TO RETURN-CODE                                             
036600     GOBACK                                                               
036700     .                                                                    
036800     EJECT                                                                
036900                                                                          
037000 A-INIT SECTION.                                                          
037100                                                                          
037200     OPEN OUTPUT W21631                                                   
037300                 W21633                                                   
037310                 W21637                                                   
037320                 W21634                                                   
037400                                                                          
037500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037600                                                                          
037700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
037800     ACCEPT DAGENS-DATUM FROM DATE                                        
037900***CALL 1                                                                 
038000     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
038100     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
038200                                                                          
038300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
038400                     DAT-O-TIDATUM DAT-KDSVAR                             
038500                                                                          
038600     IF DAT-KDSVAR-OK                                                     
038700****             HÄMTA SEKELSIFFROR                                       
038800                                                                          
038900       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
039000       MOVE DAT-TIAARP       TO DAGENS-PER                                
039100       MOVE DAGENS-PER(3:2)  TO WS-PER                                    
039200                                                                          
039300     ELSE                                                                 
039400         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
039500         DELIMITED BY SIZE INTO FELTEXT                                   
039600         CALL FELLOG                                                      
039700     END-IF                                                               
039800                                                                          
039900     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
040000                                                                          
040100***CALL2                                                                  
040200     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
040300     MOVE 01                 TO WS-AAPP(3:2)                              
040400     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
040500     MOVE 'AARP'             TO DAT-KDDATFORM                             
040600     MOVE 1                  TO WS-PP                                     
040700                                                                          
040800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
040900                     DAT-O-TIDATUM DAT-KDSVAR                             
041000                                                                          
041100     IF DAT-KDSVAR-OK                                                     
041200                                                                          
041300       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
041400       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
041500                                                                          
041600     ELSE                                                                 
041700         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
041800         DELIMITED BY SIZE INTO FELTEXT                                   
041900         CALL FELLOG                                                      
042000     END-IF                                                               
042100                                                                          
042200     PERFORM UNTIL WS-PP      >  12                                       
042300       ADD 1                  TO WS-PP                                    
042400       IF WS-PP = 13                                                      
042500         MOVE 53             TO WS-SISTA-V(12)                            
042600       ELSE                                                               
042700                                                                          
042800         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
042900         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
043000                             DAT-O-TIDATUM DAT-KDSVAR                     
043100         IF DAT-KDSVAR-OK                                                 
043200             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
043300             IF WS-PP > 1                                                 
043400               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
043500               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
043600             END-IF                                                       
043700         ELSE                                                             
043800           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
043900           CALL FELLOG                                                    
044000         END-IF                                                           
044100       END-IF                                                             
044200     END-PERFORM                                                          
044400     .                                                                    
044500     EJECT                                                                
044510 B-SKAPA-W26131 SECTION.                                                  
044511                                                                          
044520      IF UT-HEADLINE-AREA = SPACE                                         
044530                                                                          
044540        PERFORM BB-MOVE-HEADLINE                                          
044550        PERFORM S03-SKRIV-W21631                                          
044560      END-IF                                                              
044570                                                                          
044580      PERFORM BC-MOVE-TAB                                                 
044590      MOVE FIL-WDR301-DATA TO WS-AREA                                     
044591                                                                          
044592      MOVE WS-IDARTNR      TO W-IDARTNR                                   
044593      MOVE WS-IDARTNR      TO W-IDARTNR-111                               
044594      MOVE WS-IDARTNR      TO UT-IDARTNR                                  
044595      MOVE WS-IDINK        TO UT-IDINK                                    
044596      MOVE WS-IDLEVNR      TO UT-IDLEVNR                                  
044597                                                                          
044598      PERFORM BD-GET-DESCRIPTION                                          
044599      PERFORM BE-GET-KIT-PART-WDJ1                                        
044600      PERFORM BF-GET-PART-INFO-WDK6                                       
044601                                                                          
044602      PERFORM BG-GET-PRICE-AREA-B30-WDC1                                  
044603      PERFORM BH-GET-ORDER-WDL8                                           
044604      PERFORM BI-GET-CATALOG-WDN6                                         
044605                                                                          
044606      PERFORM S01-SKRIV-W21631                                            
044607      PERFORM BK-SKAPA-VECKOPOST                                          
044608      PERFORM BL-NOLLSTALL-TOT-TAB                                        
044609     .                                                                    
044610     EJECT                                                                
045201                                                                          
045400 BF-GET-PART-INFO-WDK6            SECTION.                                
045500     MOVE 'F-GET-PART-INFO-WDK6'      TO ABEND-HELP                       
045600                                                                          
045700     PERFORM IMS-GU-WDK601                                                
045800                                                                          
045900      MOVE ART-IDFKNGRP       TO UT-IDFKNGRP                              
046000      MOVE ART-KDSORT         TO UT-KDSORT                                
046100      MOVE ART-IDFKNGRP       TO UT-IDFKNGRP                              
046200                                                                          
046300      IF SEGMENT-FINNS                                                    
046400        MOVE ART-KDPRODSL     TO WS-KDPRODSL                              
046500      ELSE                                                                
046600        MOVE 00               TO WS-KDPRODSL                              
046700      END-IF                                                              
046800                                                                          
046900      PERFORM IMS-GNP-WDK611                                              
047000      IF SEGMENT-FINNS                                                    
047100                                                                          
047200        MOVE CLAG-KDARTURS     TO UT-KDARTURS                             
047300                                                                          
047410        MOVE CLAG-PRARTSTD     TO WS-PRARTSTD-DISP                        
047412        MOVE WS-PRARTSTD-DISP(2:6) TO WS-PRARTSTD-HELTAL                  
047413        MOVE WS-PRARTSTD-DISP(8:2) TO WS-PRARTSTD-DECIMAL                 
047414                                                                          
047420        MOVE WS-UT-PRARTSTD     TO UT-PRARTSTD                            
047500                                                                          
047600        MOVE CLAG-VKART         TO WS-VKART-DISP                          
047601        MOVE WS-VKART-DISP(2:3) TO WS-VKART-HELTAL                        
047602        MOVE WS-VKART-DISP(5:3) TO WS-VKART-DECIMAL                       
047603                                                                          
047604        MOVE WS-UT-VKART       TO UT-VKART                                
047900        MOVE CLAG-KDFARLIG     TO UT-KDFARLIG                             
048000                                                                          
048100        PERFORM IMS-GNP-WDK627                                            
048200        IF SEGMENT-FINNS                                                  
048300                                                                          
048400          MOVE SKROT-KVSKROT   TO UT-KVSKROT                              
048500          MOVE SKROT-DASKROT   TO UT-DASKROT                              
048600        END-IF                                                            
048700      END-IF                                                              
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100 BE-GET-KIT-PART-WDJ1             SECTION.                                
049200     MOVE 'E-GET-KIT-PART-WDJ1'       TO ABEND-HELP                       
049300                                                                          
049400     PERFORM IMS-GU-WDJ101                                                
049500     IF SEGMENT-FINNS                                                     
049600       MOVE JA                   TO UT-FLSATART                           
049700     ELSE                                                                 
049800       MOVE NEJ                  TO UT-FLSATART                           
049900     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050200                                                                          
050300 BG-GET-PRICE-AREA-B30-WDC1  SECTION.                                     
050400     MOVE 'G-GET-PRICE-AREA-B30-WDC1'     TO ABEND-HELP                   
050500                                                                          
051200     MOVE 'B'                 TO W-IDMARKBO-111                           
051400                                                                          
051420                                                                          
051500     PERFORM IMS-GU-WDC101                                                
051501                                                                          
051600     IF SEGMENT-FINNS                                                     
051802                                                                          
051803       MOVE ART-PRARTBTO-MARK    TO WS-PRARTBTO-MARK-DISP                 
051810       MOVE WS-PRARTBTO-MARK-DISP(2:7) TO WS-PRARTBTO-MARK-HELTAL         
051811       MOVE WS-PRARTBTO-MARK-DISP(8:2) TO WS-PRARTBTO-MARK-DECIMAL        
051813                                                                          
051820       MOVE WS-UT-PRARTBTO-MARK  TO UT-PRARTBTO-MARK                      
051910     ELSE                                                                 
052000       MOVE ZERO                 TO UT-PRARTBTO-MARK                      
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
052500 BH-GET-ORDER-WDL8                SECTION.                                
052600     MOVE ' H-GET-ORDER-WDL8 '              TO ABEND-HELP                 
052800                                                                          
052900*HAMTA VECKA I PERIODEN                                                   
053000*W2010700                                                                 
053100*    --- FYLL I VECKONR FÖR PERIODERNA                                    
053200                                                                          
054000     MOVE WS-DAGENS-AAAAMMDD (1:4)                                        
054100                               TO W-TIAAAA                                
054200     MOVE WS-IDARTNR           TO W-IDARTNR                               
054300                                                                          
054400     PERFORM IMS-GU-WDL811                                                
054500                                                                          
054600     IF SEGMENT-FINNS                                                     
054700                                                                          
054800       MOVE +3                 TO IX-AR                                   
054900       MOVE +1                 TO IX-PER                                  
055000       MOVE ZERO               TO WS-TOTAL-KVOI                           
055100                                                                          
055200       PERFORM UNTIL IX-PER    =  WS-PER                                  
055300         MOVE ZERO             TO WS-KVOI (IX-PER)                        
055400         MOVE WS-FORSTA-V(IX-PER)                                         
055500                               TO WS-VV                                   
055600         PERFORM UNTIL WS-VV   >  WS-SISTA-V(IX-PER)                      
055700*          WS-VAL = 'TOTAL'                                               
055800           ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)                    
055900           ADD AAR-KVOI-NDC(WS-VV)  TO WS-KVOI(IX-PER)                    
056000           ADD AAR-KVOI-SDC(WS-VV)  TO WS-KVOI(IX-PER)                    
056100           ADD AAR-KVOI-SATS(WS-VV) TO WS-KVOI(IX-PER)                    
056200           ADD AAR-KVOI-DIV(WS-VV)  TO WS-KVOI(IX-PER)                    
056300                                                                          
056400           ADD +1              TO WS-VV                                   
056500         END-PERFORM                                                      
056600         COMPUTE WS-KVOI-RED ROUNDED =                                    
056700                 WS-KVOI (IX-PER)                                         
056800                            / WS-KVVIPER (IX-PER) * +4.33                 
056900                                                                          
057000         ADD WS-KVOI-RED       TO WS-KVOI-TAB (IX-AR, IX-PER)             
057100                                  WS-TOTAL-KVOI                           
057400         ADD +1                TO IX-PER                                  
057500       END-PERFORM                                                        
057600                                                                          
057700       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-TAB (IX-AR)                 
058000     END-IF                                                               
058100                                                                          
058200     MOVE WS-IDARTNR           TO W-IDARTNR                               
058300     MOVE +2                   TO IX-AR                                   
058400     PERFORM UNTIL IX-AR < +1                                             
058500                                                                          
058600*  LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                        
058700                                                                          
058800       SUBTRACT 1              FROM W-TIAAAA                              
058900       PERFORM IMS-GU-WDL811                                              
059000                                                                          
059100       IF SEGMENT-FINNS                                                   
059200         MOVE 1                TO IX-PER                                  
059300         MOVE ZERO             TO WS-TOTAL-KVOI                           
059400         PERFORM UNTIL IX-PER > 12                                        
059500                                                                          
059600           MOVE ZERO           TO WS-KVOI (IX-PER)                        
059700           MOVE WS-FORSTA-V (IX-PER)                                      
059800                               TO WS-VV                                   
059900           PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                      
060000*            WS-VAL = 'TOTAL'                                             
060100             ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)                  
060200             ADD AAR-KVOI-NDC(WS-VV)  TO WS-KVOI(IX-PER)                  
060300             ADD AAR-KVOI-SDC(WS-VV)  TO WS-KVOI(IX-PER)                  
060400             ADD AAR-KVOI-SATS(WS-VV) TO WS-KVOI(IX-PER)                  
060500             ADD AAR-KVOI-DIV(WS-VV)  TO WS-KVOI(IX-PER)                  
060600                                                                          
060700             ADD 1             TO WS-VV                                   
060800           END-PERFORM                                                    
060900                                                                          
061000           COMPUTE WS-KVOI-RED ROUNDED =                                  
061100              WS-KVOI (IX-PER) * +4.33                                    
061200                 / WS-KVVIPER (IX-PER)                                    
061300                                                                          
061400           ADD WS-KVOI-RED     TO WS-TOTAL-KVOI                           
061800           ADD +1              TO IX-PER                                  
061900         END-PERFORM                                                      
062000                                                                          
062100         ADD WS-TOTAL-KVOI     TO WS-KVOI-TOT-TAB (IX-AR)                 
062400       END-IF                                                             
062500                                                                          
062600       SUBTRACT +1             FROM IX-AR                                 
062700     END-PERFORM                                                          
062800                                                                          
062900     MOVE +1                 TO IX-AR                                     
063000                                                                          
063100     PERFORM UNTIL IX-AR > +3                                             
063200                                                                          
063300       EVALUATE IX-AR                                                     
063400       WHEN +1                                                            
063500         MOVE WS-KVOI-TOT-TAB (+1) TO UT-KVOI-TOT-TWO-YEARS-AGO           
063600       WHEN +2                                                            
063700         MOVE WS-KVOI-TOT-TAB (+2) TO UT-KVOI-TOT-ONE-YEAR-AGO            
063800       WHEN +3                                                            
063900         MOVE WS-KVOI-TOT-TAB (+3) TO UT-KVOI-TOT-CURRENT-YEAR            
064000       END-EVALUATE                                                       
064100                                                                          
064200       ADD +1                TO IX-AR                                     
064300     END-PERFORM                                                          
064400     .                                                                    
064500     EJECT                                                                
064600                                                                          
064700 BI-GET-CATALOG-WDN6         SECTION.                                     
064800     MOVE ' I-GET-CATALOG-WDN6'        TO ABEND-HELP                      
064900                                                                          
065000*LAS-KATALOGIDENTITET                                                     
065100                                                                          
065200     MOVE SPACE           TO UT-IDKATNR-AREA                              
065300                                                                          
065400     PERFORM IMS-GU-WDN601                                                
065500     IF SEGMENT-FINNS                                                     
065600                                                                          
065700        MOVE 0 TO ITERIX                                                  
065800                                                                          
065900        PERFORM IMS-GNP-WDN6-KAT                                          
066200        IF SEGMENT-FINNS                                                  
066300          MOVE 1 TO ITERIX                                                
066500        END-IF                                                            
066600                                                                          
066700        MOVE +1 TO P-INDX                                                 
066800*                                                                         
066900        PERFORM UNTIL SEGMENT-SAKNAS OR (ITERIX = P-MAX-100-INDX)         
067000                                                                          
067100          MOVE KAT-IDKATNR           TO WS-KAT-IDKATNR                    
067200                                                                          
067500          STRING WS-KAT-IDKATNR TAB DELIMITED BY SIZE                     
067600          INTO UT-IDKATNR-AREA WITH POINTER P-INDX                        
067700                                                                          
067800          STRING 'HAR PRECIS LÄST IDKATNR ANT ' ITERIX                    
067900                  DELIMITED BY SIZE INTO FELTEXT                          
068000                                                                          
068100          PERFORM IMS-GNP-WDN6-KAT                                        
068200          IF SEGMENT-FINNS                                                
068300            ADD 1   TO ITERIX                                             
068500          END-IF                                                          
068600        END-PERFORM                                                       
068700                                                                          
068800        SUBTRACT 1  FROM P-INDX                                           
068900                                                                          
069000     END-IF                                                               
069100*                                                                         
069200     .                                                                    
069300     EJECT                                                                
069400                                                                          
069500 BB-MOVE-HEADLINE   SECTION.                                              
069600                                                                          
069800     MOVE SPACE           TO UT-HEADLINE-AREA                             
069810     MOVE +1 TO P-INDX                                                    
069900                                                                          
070000     STRING 'PURCHASE.NO' TAB DELIMITED BY SIZE                           
070100     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
070200                                                                          
070300     STRING 'PART.NO' TAB DELIMITED BY SIZE                               
070400     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
070500                                                                          
070600     STRING 'SUPPLIER.NO' TAB DELIMITED BY SIZE                           
070700     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
070800                                                                          
070900     STRING 'DESCRIPTION(SE)' TAB DELIMITED BY SIZE                       
071000     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071100                                                                          
071110     STRING 'DESCRIPTION(GB)' TAB DELIMITED BY SIZE                       
071120     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071130                                                                          
071200     STRING 'FUNC GROUP' TAB DELIMITED BY SIZE                            
071300     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071310                                                                          
071320     STRING 'ORIGIN' TAB DELIMITED BY SIZE                                
071330     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071340                                                                          
071350     STRING 'SUGG.RETAIL' TAB DELIMITED BY SIZE                           
071360     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071370                                                                          
071380     STRING 'STD PRICE' TAB DELIMITED BY SIZE                             
071390     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071391                                                                          
071392     STRING 'STRUCTURE' TAB DELIMITED BY SIZE                             
071393     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071394                                                                          
071395     STRING 'WEIGHT IN KG' TAB DELIMITED BY SIZE                          
071396     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071400                                                                          
071500     STRING 'SORT CODE' TAB DELIMITED BY SIZE                             
071600     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
071700                                                                          
071800     STRING 'SALES CURRENT YEAR' TAB DELIMITED BY SIZE                    
071900     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
072000                                                                          
072100     STRING 'SALES ONE YEAR AGO' TAB DELIMITED BY SIZE                    
072200     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
072300                                                                          
072400     STRING 'SALES TWO YEARS AGO' TAB DELIMITED BY SIZE                   
072500     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
072510                                                                          
072520     STRING 'SCRAP QTY' TAB DELIMITED BY SIZE                             
072530     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
072540                                                                          
072550     STRING 'SCRAP DATE' TAB DELIMITED BY SIZE                            
072560     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
072600                                                                          
072700     STRING 'DANG.GOODS' TAB DELIMITED BY SIZE                            
072800     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
075000                                                                          
075100     STRING 'CATALOGUE' TAB DELIMITED BY SIZE                             
075200     INTO UT-HEADLINE-AREA WITH POINTER P-INDX                            
075300                                                                          
075400     SUBTRACT 1  FROM P-INDX                                              
075500                                                                          
075600     .                                                                    
075700     EJECT                                                                
075800                                                                          
075810 BD-GET-DESCRIPTION     SECTION.                                          
075820     MOVE 'D-GET-DESCRIPTION'         TO ABEND-HELP                       
075830                                                                          
075840     MOVE 'S  '              TO W-IDSKYLT                                 
075850     PERFORM IMS-GU-WDD311                                                
075860     IF SEGMENT-FINNS                                                     
075870        MOVE TEXT-BEART      TO UT-BEART-SE                               
075880     ELSE                                                                 
075890        MOVE SPACE           TO UT-BEART-SE                               
075891     END-IF                                                               
075892                                                                          
075893     MOVE 'GB '              TO W-IDSKYLT                                 
075894     PERFORM IMS-GU-WDD311                                                
075895     IF SEGMENT-FINNS                                                     
075896        MOVE TEXT-BEART      TO UT-BEART-ENG                              
075897     ELSE                                                                 
075898        MOVE SPACE           TO UT-BEART-ENG                              
075899     END-IF                                                               
075902     .                                                                    
075903     EJECT                                                                
075904                                                                          
075910 BL-NOLLSTALL-TOT-TAB             SECTION.                                
076000     MOVE ' H-NOLLSTALL-TAB        '        TO ABEND-HELP                 
076100                                                                          
076200     MOVE +1                 TO IX-AR                                     
076300                                                                          
076400     PERFORM UNTIL IX-AR > +3                                             
076500                                                                          
076600       EVALUATE IX-AR                                                     
076700       WHEN +1                                                            
076800         MOVE ZERO           TO WS-KVOI-TOT-TAB (+1)                      
076900       WHEN +2                                                            
077000         MOVE ZERO           TO WS-KVOI-TOT-TAB (+2)                      
077100       WHEN +3                                                            
077200         MOVE ZERO           TO WS-KVOI-TOT-TAB (+3)                      
077300       END-EVALUATE                                                       
077400                                                                          
077500       ADD +1                TO IX-AR                                     
077600     END-PERFORM                                                          
077700                                                                          
080300     .                                                                    
080400     EJECT                                                                
080500                                                                          
080600 BC-MOVE-TAB     SECTION.                                                 
080700     MOVE 'C-MOVE-TAB       '         TO ABEND-HELP                       
080800                                                                          
080900     MOVE TAB               TO UT-TAB-1                                   
081000     MOVE TAB               TO UT-TAB-2                                   
081100     MOVE TAB               TO UT-TAB-3                                   
081200     MOVE TAB               TO UT-TAB-4                                   
081300     MOVE TAB               TO UT-TAB-5                                   
081400     MOVE TAB               TO UT-TAB-6                                   
081500     MOVE TAB               TO UT-TAB-7                                   
081600     MOVE TAB               TO UT-TAB-8                                   
081700     MOVE TAB               TO UT-TAB-9                                   
081800     MOVE TAB               TO UT-TAB-10                                  
081900     MOVE TAB               TO UT-TAB-11                                  
082000     MOVE TAB               TO UT-TAB-12                                  
082100     MOVE TAB               TO UT-TAB-13                                  
082200     MOVE TAB               TO UT-TAB-14                                  
082300     MOVE TAB               TO UT-TAB-15                                  
082400     MOVE TAB               TO UT-TAB-16                                  
082500     MOVE TAB               TO UT-TAB-17                                  
082510     MOVE TAB               TO UT-TAB-18                                  
082600                                                                          
082700     .                                                                    
082800     EJECT                                                                
082801                                                                          
082810 BK-SKAPA-VECKOPOST           SECTION.                                    
082820     MOVE 'K-SKAPA-VECKOPOST'         TO ABEND-HELP                       
082830                                                                          
082840     MOVE WS-IDARTNR     TO VECKO-IDARTNR                                 
082850     MOVE WS-IDINK       TO VECKO-IDINK                                   
082860     MOVE WS-IDLEVNR     TO VECKO-IDLEVNR                                 
082870     MOVE WS-BEART       TO VECKO-BEART                                   
082890                                                                          
082891     PERFORM S04-SKRIV-W21637                                             
082892     .                                                                    
082893     EJECT                                                                
082897 D-SKAPA-BORTPOST SECTION.                                                
082898     MOVE 'D-SKAPA-BORTPOST '         TO ABEND-HELP                       
082899                                                                          
082900     MOVE FIL-WDR301 TO BORT-FIL-WDR301                                   
082901     PERFORM S02-SKRIV-W21633                                             
082902     .                                                                    
082903     EJECT                                                                
082910                                                                          
083000 Z-FINIT SECTION.                                                         
083100                                                                          
083200     CLOSE W21631                                                         
083300           W21633                                                         
083310           W21637                                                         
083320           W21634                                                         
083400                                                                          
083500     MOVE 'S' TO POSTSUM-OPKOD                                            
083600     CALL POSTSUM USING POSTSUM-PARM                                      
083700     .                                                                    
083800     EJECT                                                                
083900 S01-SKRIV-W21631 SECTION.                                                
084000                                                                          
084100     COMPUTE W21631-LENGTH = LENGTH OF UT-W21631 + P-INDX                 
084200                                                                          
084300     WRITE UT-POST FROM UT-AREA(1:W21631-LENGTH)                          
084400                                                                          
084500     MOVE 'W216'     TO POSTSUM-TRANSTYP                                  
084600     MOVE 'W21631 '  TO POSTSUM-FDNAMN                                    
084700     MOVE 'W21632D1' TO POSTSUM-DDNAMN2                                   
084800     CALL POSTSUM USING POSTSUM-PARM                                      
084900     .                                                                    
085000     SKIP3                                                                
085100 S02-SKRIV-W21633 SECTION.                                                
085200                                                                          
085300     WRITE BORT-POST FROM BORT-AREA                                       
085400                                                                          
085500     MOVE 'RENS'      TO POSTSUM-TRANSTYP                                 
085600     MOVE 'W21633 '   TO POSTSUM-FDNAMN                                   
085700     MOVE 'W21632D2'  TO POSTSUM-DDNAMN2                                  
085800     CALL POSTSUM USING POSTSUM-PARM                                      
085900     .                                                                    
086000     EJECT                                                                
086100 S03-SKRIV-W21631 SECTION.                                                
086200                                                                          
086300     COMPUTE W21631-LENGTH = LENGTH OF UT-W21631 + P-INDX                 
086500     WRITE UT-HEAD-LINE FROM UT-HEADLINE-AREA(1:W21631-LENGTH)            
086600                                                                          
086700     MOVE 'HEAD'     TO POSTSUM-TRANSTYP                                  
086800     MOVE 'W21631 '  TO POSTSUM-FDNAMN                                    
086900     MOVE 'W21632D1' TO POSTSUM-DDNAMN2                                   
087000     CALL POSTSUM USING POSTSUM-PARM                                      
087100     .                                                                    
087200     SKIP3                                                                
087210 S04-SKRIV-W21637 SECTION.                                                
087220                                                                          
087230     WRITE VECKO-POST FROM VECKO-AREA                                     
087240                                                                          
087250     MOVE 'WEEK'      TO POSTSUM-TRANSTYP                                 
087260     MOVE 'W21637 '   TO POSTSUM-FDNAMN                                   
087270     MOVE 'W21632D3'  TO POSTSUM-DDNAMN2                                  
087280     CALL POSTSUM USING POSTSUM-PARM                                      
087290     .                                                                    
087291     EJECT                                                                
087292 S05-SKRIV-W21634 SECTION.                                                
087293                                                                          
087294     WRITE H02-POST FROM H02-AREA                                         
087295                                                                          
087296     MOVE 'H02'       TO POSTSUM-TRANSTYP                                 
087297     MOVE 'W21634 '   TO POSTSUM-FDNAMN                                   
087298     MOVE 'W21632D4'  TO POSTSUM-DDNAMN2                                  
087299     CALL POSTSUM USING POSTSUM-PARM                                      
087300     .                                                                    
087301     EJECT                                                                
087310* --- IMS SEKTIONER ---                                                   
087400                                                                          
087500 IMS-GN-WDR301 SECTION.                                                   
087600     MOVE 'IMS-GN-WDR301            ' TO IMS-CALL                         
087700                                                                          
087800     STRING 'WDR301  (IDCPYTXT =' W-IDCPYTXT-X                            
087810                    '!IDCPYTXT =' W-IDCPYTXT-H02-X ')'                    
087900          DELIMITED BY SIZE INTO SSA1                                     
088000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
088100     CALL CBLTDLI USING GN WDR3-PCB DLI-IO-WDR301 SSA1                    
088200     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088500     SKIP3                                                                
088600 IMS-GU-WDD311 SECTION.                                                   
088700     MOVE 'IMS-GU-WDD311            ' TO IMS-CALL                         
088800                                                                          
088900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
089000          DELIMITED BY SIZE INTO SSA1                                     
089100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
089200          DELIMITED BY SIZE INTO SSA2                                     
089300     MOVE '  GE' TO GODK-STATUSKODER                                      
089400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
089500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
089600     PERFORM IMS-STATUSKONTROLL                                           
089700     .                                                                    
089800     EJECT                                                                
089900 IMS-GU-WDK601 SECTION.                                                   
090000     MOVE 'IMS-GU-WDK601            ' TO IMS-CALL                         
090100                                                                          
090200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
090300          DELIMITED BY SIZE INTO SSA1                                     
090400     MOVE '  GE' TO GODK-STATUSKODER                                      
090500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
090600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     .                                                                    
090900     EJECT                                                                
091000 IMS-GNP-WDK611 SECTION.                                                  
091100     MOVE 'IMS-GNP-WDK611           ' TO IMS-CALL                         
091200                                                                          
091300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
091400          DELIMITED BY SIZE INTO SSA1                                     
091500     MOVE '  GE' TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
091700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000     SKIP3                                                                
092100 IMS-GNP-WDK627 SECTION.                                                  
092200*                      LÄSNING AV SKROTNINGSSEGMENT WDK627                
092300     MOVE 'IMS-GNP-WDK627           ' TO IMS-CALL                         
092600     MOVE   'WDK627   '          TO SSA1                                  
092700     MOVE   '  GE'               TO GODK-STATUSKODER                      
092900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK627 SSA1                   
093100     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
093200     PERFORM IMS-STATUSKONTROLL                                           
093300                                                                          
093400     SKIP3                                                                
093500     .                                                                    
093600 IMS-GU-WDJ101   SECTION.                                                 
093700     MOVE 'IMS-GU-WDJ101            ' TO IMS-CALL                         
093800                                                                          
093900     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
094000          DELIMITED BY SIZE INTO SSA1                                     
094100     MOVE '  GE' TO GODK-STATUSKODER                                      
094200     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-WDJ101 SSA1                    
094300     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
094400     PERFORM IMS-STATUSKONTROLL                                           
094500     .                                                                    
094600                                                                          
094700 IMS-GU-WDC101 SECTION.                                                   
094800     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
094900          DELIMITED BY SIZE INTO SSA1                                     
095000     MOVE '  GE' TO GODK-STATUSKODER                                      
095100     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-WDC101 SSA1                    
095200     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     SKIP3                                                                
095600                                                                          
095700 IMS-GU-WDL811 SECTION.                                                   
095800     MOVE 'IMS-GU-WDL811            ' TO IMS-CALL                         
095900                                                                          
096000     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
096100          DELIMITED BY SIZE INTO SSA1                                     
096200     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
096300          DELIMITED BY SIZE INTO SSA2                                     
096400     MOVE '  GE' TO GODK-STATUSKODER                                      
096500     CALL CBLTDLI USING GU WDL8-PCB                                       
096600                                 DLI-IO-WDL811 SSA1 SSA2                  
096700     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
096800     PERFORM IMS-STATUSKONTROLL                                           
096900     .                                                                    
097000     EJECT                                                                
097100 IMS-GU-WDN601 SECTION.                                                   
097200     MOVE 'IMS-GU-WDN601            ' TO IMS-CALL                         
097300                                                                          
097400     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
097500            DELIMITED BY SIZE INTO SSA1                                   
097600     MOVE '  GE' TO GODK-STATUSKODER                                      
097700     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
097800     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     SKIP3                                                                
098100     .                                                                    
098200 IMS-GNP-WDN6-KAT SECTION.                                                
098300     MOVE 'IMS-GNP-WDN6-KAT       ' TO IMS-CALL                           
098400                                                                          
098500     MOVE 'WDN611  ' TO SSA1                                              
098600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
098700     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
098800     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
098900     PERFORM IMS-STATUSKONTROLL                                           
099000     SKIP3                                                                
099100     .                                                                    
099200 IMS-STATUSKONTROLL SECTION.                                              
099300                                                                          
099400     SET STATUS-IX TO 1                                                   
099500     SEARCH GODK-STATUS                                                   
099600       AT END                                                             
099700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
099800           DELIMITED BY SIZE INTO FELTEXT                                 
099900         DISPLAY FELTEXT                                                  
100000         CALL FELLOG                                                      
100100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
100200         CONTINUE                                                         
100300     END-SEARCH                                                           
100400     .                                                                    
