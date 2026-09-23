000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2135200.                                                
000300 AUTHOR.         JEBSEN KENT.                                             
000400 DATE-WRITTEN.   13/02/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        CHANGE SUPPLIER NO. FOR CHINA AND USA.                           
001000*        - NDC'S WITH LOCAL SOURCING.                                     
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDG3                                       
001300*        PROGRAMMET UPPDATERAR WDK7                                       
001400*        PROGRAMMET LÄSER      WDE3                                       
001500*        PROGRAMMET LÄSER      WDB6                                       
001600*        PROGRAMMET UPPDATERAR WDD9                                       
001700*        PROGRAMMET LÄSER      WDR2                                       
001800*        PROGRAMMET LÄSER      WDF1                                       
001900*        PROGRAMMET UPPDATERAR WDG3                                       
002000*                                                                         
002100****ÄNDRINGAR:                                                            
002200* 2013-09-17   E'TRACKER 10212237  RÄTTA FEL I PGM WS-EXT-TO-REF          
002300*                                                                         
002400* 2017-08-07   E'TRACKER 10299286  LOCAL SOURCING USA.CCID:1030268        
002500*                                                                         
002510* 2018-07-03   JIRA      2278      GLOBAL EXPORT      CCID:2278           
002520*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300*          --- PARAMETERKORT IN - SYSIN CARD FRÅN JCL                     
003400     SELECT PARMIN                     ASSIGN TO W21352D1.                
003401                                                                          
003402*    ---- XRST-FIL: KOPIA AV UTFIL(+0) TAS IN SOM INPUT-FIL               
003410     SELECT XRST-FIL                   ASSIGN TO W21352D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900 FD  PARMIN                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300 01  FILLER                 PIC X(80).                                    
004400     EJECT                                                                
004410 FD  XRST-FIL                                                             
004420     LABEL RECORD STANDARD                                                
004430     RECORDING V                                                          
004440     BLOCK CONTAINS 0.                                                    
004450                                                                          
004460 01  FILLER                  PIC X(998).                                  
004470 01  POST -COPY W21350       -PRE XRST-                                   
004480     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W2135200'.            
004800 01  CHKP-VAR.                                                            
004900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005400     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005401 77  MAX-GSAM                    PIC S9(4)   COMP.                        
005410 77  XRST-FIL-EOF                PIC X       VALUE 'N'.                   
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  IX                          PIC 9.                                   
005800 77  WS-IDLEVNR-SHIP             PIC X(5).                                
005900 77  WS-DEFAULT-IDANSK-US        PIC S9(3)   VALUE +200.                  
005910 77  WS-DEFAULT-IDANSK-CN        PIC S9(3)   VALUE +300.                  
006000 77  WS-IDANSK-PG                PIC S9(3).                               
006100 77  WS-KDAVT                    PIC S9.                                  
006200 77  WS-WDK722                   PIC X.                                   
006300 77  WS-FL-CONTINUE              PIC X.                                   
006400 77  WS-EXT-TO-EXT               PIC X.                                   
006500 77  WS-EXT-TO-REF               PIC X.                                   
006600 77  WS-REF-TO-EXT               PIC X.                                   
006700 77  WS-REF-TO-REF               PIC X.                                   
006800 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
006900 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
007000 77  WS-IDLEVNR-INNAN            PIC X(5).                                
007010 77  SPAR-IDLANDX2-EXT           PIC X(2)  VALUE SPACE.                   
007020 77  SPAR-VKART-REF              PIC S9(7) VALUE ZERO COMP-3.             
007030 77  SPAR-VLARTNTO-REF      PIC S9(8)V9(1) VALUE ZERO COMP-3.             
007040 77  SPAR-KDARTURS-REF           PIC X(2)  VALUE SPACE.                   
007041 77  SPAR-DCS-KDDC-EXT           PIC X(2)  VALUE SPACE.                   
007042 77  SPAR-DCS-KDDC-REF           PIC X(2)  VALUE SPACE.                   
007050 77  REPL-WDK712-SW              PIC X       VALUE 'N'.                   
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500     EJECT                                                                
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008100     EJECT                                                                
008200 01  PARM-AREA.                                                           
008300     03  PARM-IDDC-FOM           PIC X(02) VALUE SPACE.                   
008400     03  PARM-IDDC-TOM           PIC X(02) VALUE SPACE.                   
008500     03  FILLER                  PIC X(76) VALUE SPACE.                   
008600     EJECT                                                                
008610 01  FILLER                  PIC X(16)   VALUE 'WS-FLD**********'.        
008620 01  WS-FLD.                                                              
008640   03  WS-ANT-POST-GSAM      PIC S9(5)   COMP-3  VALUE ZERO.              
008650   03  WS-ANT-POST-XRST      PIC S9(5)   COMP-3  VALUE ZERO.              
008660*                                                                         
008700*******                      PARAMETRAR TILL WDAGKONV                     
008800*                                                                         
008900*01  -COPY WDAGAREA                                                       
009000                                                                          
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009410     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009600     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
009700     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
009800     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
009810     03  POSTSUM             PIC X(8) VALUE  'POSTSUM '.                  
009900     EJECT                                                                
010000*01  -COPY WDATAREA                                                       
010100*                                                                         
010200     EJECT                                                                
010201 01  ABENDKODER.                                                          
010202    03  RKOD-ABEND-MED-DUMP  PIC S9(4) COMP SYNC VALUE +1000.             
010203    03  RKOD-ABEND-UTAN-DUMP PIC S9(4) COMP SYNC VALUE   +16.             
010204     EJECT                                                                
010300*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
010400*01 -COPY W005WDK7                                                        
010500     EJECT                                                                
010540                                                                          
010541******************************************************************        
010542*    AREA FÖR POSTSUM                                            *        
010543******************************************************************        
010544                                                                          
010545*    -COPY W0005  -PRE POSTSUM-.                                          
010546     EJECT                                                                
010570                                                                          
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010900                                                                          
011000     03  W-WDGXKEY-2253-X.                                                
011100          05 W-IDHTYP-2253       PIC X(4)    VALUE '2253'.                
011200          05 FILLER              PIC X(26)   VALUE LOW-VALUE.             
011300                                                                          
011400     03  W-WDGXKEY-2254-X.                                                
011500          05 W-IDDC-2254         PIC X(2)    VALUE SPACE.                 
011600          05 W-IDARTNR-2254      PIC S9(9)   VALUE ZERO COMP-3.           
011700                                                                          
011800     03  W-WDGXKEY-4579-X.                                                
011900          05 W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
012000          05 W-IDPGM             PIC X(8)    VALUE 'W2135200'.            
012100          05 FILLER              PIC X(18)   VALUE LOW-VALUE.             
012200                                                                          
012300     03  W-IDARTNR-X.                                                     
012400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012500     03  W-IDDC-X.                                                        
012600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012610                                                                          
012611     03  W-IDLAND-X.                                                      
012620         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
012640                                                                          
012700     03  W-KDSEGKEY-X.                                                    
012800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
012900     03  W-IDLEVNRA-X.                                                    
013000         05  W-IDLEVNR-AVT       PIC  X(5)    VALUE SPACE.                
013100     03  W-IDLEVNDC-X.                                                    
013200         05  W-IDLEVNDC          PIC X(5)    VALUE SPACE.                 
013300     03  W-IDLEVNR-X.                                                     
013400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
013500     03  W-KDAVROP-X.                                                     
013600         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
013700     03  W-DAAVROP-X.                                                     
013800         05  W-DAAVROP           PIC  9(6).                               
013900     03 W-WDE301KY-MIN-X.                                                 
014000         05  W-IDDC-MIN-E3       PIC X(2)  VALUE SPACE.                   
014100         05  FILLER              PIC X(11) VALUE LOW-VALUE.               
014200     03  W-KDREFTYP-MIN-X.                                                
014300         05  W-KDREFTYP-MIN      PIC X       VALUE 'A'.                   
014400     03  W-KDREFTYP-MAX-X.                                                
014500         05  W-KDREFTYP-MAX      PIC X       VALUE 'R'.                   
014600                                                                          
014700     03 W-WDE301KY-MAX-X.                                                 
014800         05  W-IDDC-MAX-E3       PIC X(2)  VALUE SPACE.                   
014900         05  FILLER              PIC X(11) VALUE HIGH-VALUE.              
015000   03  W-WDGXKEY-2205-X.                                                  
015100     05  IDHTYP-2205             PIC X(4)    VALUE '2205'.                
015200     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
015300     SKIP3                                                                
015400     03  W-KY2206-X.                                                      
015500         05  W-IDLEVNR-2206      PIC X(5)     VALUE SPACE.                
015600         05  W-IDDC-2206         PIC X(2)     VALUE SPACE.                
015700                                                                          
015800   03    W-WDG3KEY-2213-X.                                                
015900     05  FILLER          PIC X(04)  VALUE '2213'.                         
016000     05  W-IDDC-2213     PIC X(02)  VALUE SPACE.                          
016100     05  FILLER          PIC X(24)  VALUE LOW-VALUE.                      
016200                                                                          
016300   03    W-WDG3KEY-2203-X.                                                
016400     05  FILLER          PIC X(04)  VALUE '2203'.                         
016500     05  W-IDDC-2203     PIC X(02)  VALUE SPACE.                          
016600     05  FILLER          PIC X(24)  VALUE LOW-VALUE.                      
016700   03  W-WDD901KY-X.                                                      
016800     05  W-IDARTNR-WDK9      PIC S9(9)   VALUE ZERO COMP-3.               
016900     05  W-IDDC-WDK9         PIC X(2)    VALUE SPACE.                     
017000   03  W-WDD601KY-MIN-X.                                                  
017100     05 W-IDDC-D6-MIN        PIC X(2)  VALUE SPACE.                       
017200     05 W-IDLEVNR-D6-MIN     PIC X(5)  VALUE SPACE.                       
017300     05 W-IDARTNR-D6-MIN     PIC S9(9) VALUE ZERO COMP-3.                 
017400     05 W-IDANSK-D6-MIN      PIC S9(3) VALUE ZERO COMP-3.                 
017500                                                                          
017600   03  W-WDD601KY-MAX-X.                                                  
017700     05 W-IDDC-D6-MAX        PIC X(2)  VALUE SPACE.                       
017800     05 W-IDLEVNR-D6-MAX     PIC X(5)  VALUE SPACE.                       
017900     05 W-IDARTNR-D6-MAX     PIC S9(9) VALUE ZERO COMP-3.                 
018000     05 W-IDANSK-D6-MAX      PIC S9(3) VALUE +999 COMP-3.                 
018100                                                                          
018400     SKIP2                                                                
018500*    --- STATUS-KOD FRÅN IMS                                              
018600 01  STATUS-WS                   PIC XX.                                  
018700     88  SEGMENT-FINNS                       VALUE '  '.                  
018800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019100     88  IMS-EJ-OK                           VALUE 'XD'.                  
019200     SKIP2                                                                
019300 01  GODK-STATUSKODER.                                                    
019400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019500     SKIP3                                                                
019600 01  SSA1                        PIC X(128).                              
019700 01  SSA2                        PIC X(64).                               
019800     EJECT                                                                
019900*    --- IMS FUNKTIONSKODER                                               
020000*01  -COPY W0003                                                          
020100     EJECT                                                                
020200*    --- DATA-AREA FÖR TRANSAKTION                                        
020300 01  FILLER                      PIC X(16) VALUE 'MSG-IO-AREA'.           
020400*01  -COPY WMSGAREA.                                                      
020500     EJECT                                                                
020600     05 FILLER REDEFINES MSG-MID-OUT.                                     
020700*       07  MID -COPY W2I19101   -PRE 2191-                               
020800     EJECT                                                                
020900                                                                          
021000*    --- KOMMUNIKATIONSAREA FÖR DISPATCHER                                
021100 01  FILLER                  PIC X(16) VALUE 'MSG-KOM-WMSGKOM'.           
021200*01  -COPY WMSGKOM                                                        
021300     EJECT                                                                
021400*    ---  DLI INPUT-OUTPUT AREA                                           
021500                                                                          
021600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
021700 01  DLI-IO-WDGX01.                                                       
021800*    03  -COPY WDGX01                                                     
021900     EJECT                                                                
022000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2254'.                    
022100 01  DLI-IO-WDGX2254.                                                     
022200*    03  -COPY WDGX2254                                                   
022300     EJECT                                                                
022710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
022720 01  DLI-IO-WDK611.                                                       
022730*    03  -COPY WDK611                                                     
022740     EJECT                                                                
022750 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
022760 01  DLI-IO-WDK701.                                                       
022770*    03  -COPY WDK701                                                     
022780     EJECT                                                                
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
022900 01  DLI-IO-WDK712.                                                       
023000*    03  -COPY WDK712                                                     
023100     EJECT                                                                
023110     EJECT                                                                
023120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
023130 01  DLI-IO-WDK711.                                                       
023140*    03  -COPY WDK711                                                     
023150     EJECT                                                                
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
023300 01  DLI-IO-WDK722.                                                       
023400*    03  -COPY WDK722                                                     
023500     EJECT                                                                
023600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
023700 01  DLI-IO-WDK723.                                                       
023800*    03  -COPY WDK723                                                     
023900     EJECT                                                                
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE301'.                      
024100 01  DLI-IO-WDE301.                                                       
024200*    03  -COPY WDE301                                                     
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
024400 01  DLI-IO-WDB601.                                                       
024500*    03  -COPY WDB601                                                     
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
024700 01  DLI-IO-WDD901.                                                       
024800*    03  -COPY WDD901                                                     
024900     EJECT                                                                
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
025100 01  DLI-IO-WDD902.                                                       
025200*    03  -COPY WDD902                                                     
025300     EJECT                                                                
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
025500 01  DLI-IO-WDD904.                                                       
025600*    03  -COPY WDD904                                                     
025700     EJECT                                                                
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
025900 01  DLI-IO-WDD905.                                                       
026000*    03  -COPY WDD905                                                     
026100     EJECT                                                                
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
026300 01  DLI-IO-WDGX2206.                                                     
026400*    03  -COPY WDGX2206                                                   
026500     EJECT                                                                
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
026700 01  DLI-IO-WDF101.                                                       
026800*    03  -COPY WDF101                                                     
026900     EJECT                                                                
027000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
027100 01  DLI-IO-WDF116.                                                       
027200*    03  -COPY WDF116                                                     
027300     EJECT                                                                
027400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
027500 01  DLI-IO-WDGX2214.                                                     
027600*    03  -COPY WDGX2214                                                   
027700     EJECT                                                                
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2204'.                    
027900 01  DLI-IO-WDGX2204.                                                     
028000*    03  -COPY WDGX2204                                                   
028100     EJECT                                                                
028200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
028300 01  DLI-IO-WDGX4580.                                                     
028400*    03  -COPY WDGX4580                                                   
028500     EJECT                                                                
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
028700 01  DLI-IO-WDD601.                                                       
028800*    03  -COPY WDD601                                                     
028900     EJECT                                                                
028920 01  FILLER                    PIC X(16) VALUE 'GSAMFIL-IO-AREA'.         
028930 01  GSAMFIL-IO-AREA.                                                     
028940     03  GSAM-LRECL            PIC S9(4) COMP.                            
028950*    03 -COPY W21350          -PRE  GSAM-                                 
028960                                                                          
029000 LINKAGE SECTION.                                                         
029100                                                                          
029200*01  -COPY W0009   -PRE MSG-                                              
029300                                                                          
029400*01  -COPY W0009   -PRE ALT-                                              
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE 2254-                                              
029700     05  FILLER                  PIC X.                                   
029800                                                                          
029900*01  -COPY W0008  -PRE WDK7-                                              
030000     05  FILLER                  PIC X.                                   
030100                                                                          
030200*01  -COPY W0008  -PRE WDE3-                                              
030300     05  FILLER                  PIC X.                                   
030400                                                                          
030500*01  -COPY W0008  -PRE WDB6-                                              
030600     05  FILLER                  PIC X.                                   
030700                                                                          
030800*01  -COPY W0008  -PRE WDD9-                                              
030900     05  FILLER                  PIC X.                                   
031000                                                                          
031100*01  -COPY W0008  -PRE WDR2-                                              
031200     05  FILLER                  PIC X.                                   
031300                                                                          
031400*01  -COPY W0008  -PRE WDF1-                                              
031500     05  FILLER                  PIC X.                                   
031600                                                                          
031700*01  -COPY W0008  -PRE WDG3-                                              
031800     05  FILLER                  PIC X.                                   
031900                                                                          
032000*01  -COPY W0008  -PRE WDK6-                                              
032100     05  FILLER                  PIC X.                                   
032200                                                                          
032300*01  -COPY W0008  -PRE 4579-                                              
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600*01  -COPY W0008  -PRE WDD6-                                              
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900*01  -COPY W0009  -PRE KOMA-                                              
033000     EJECT                                                                
033010*01  -COPY W0008  -PRE W005-WDB6-                                         
033020     05  FILLER                  PIC X.                                   
033021     EJECT                                                                
033022*01  -COPY W0008  -PRE W005-WDK6-                                         
033023     05  FILLER                  PIC X.                                   
033024     EJECT                                                                
033025*01  -COPY W0008  -PRE W005-WDK7-                                         
033026     05  FILLER                  PIC X.                                   
033027     EJECT                                                                
033031 01  -COPY W0008  -PRE  GSAMFIL-                                          
033032       05  FILLER                PIC X.                                   
033033     EJECT                                                                
033040                                                                          
033100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB 2254-PCB WDK7-PCB              
033200     WDE3-PCB WDB6-PCB WDD9-PCB WDR2-PCB WDF1-PCB WDG3-PCB                
033300     WDK6-PCB 4579-PCB WDD6-PCB KOMA-PCB                                  
033310     W005-WDB6-PCB W005-WDK6-PCB W005-WDK7-PCB                            
033320     GSAMFIL-PCB.                                                         
033400 MAIN SECTION.                                                            
033500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB 2254-PCB WDK7-PCB              
033600     WDE3-PCB WDB6-PCB WDD9-PCB WDR2-PCB WDF1-PCB WDG3-PCB                
033700     WDK6-PCB 4579-PCB WDD6-PCB KOMA-PCB                                  
033710     W005-WDB6-PCB W005-WDK6-PCB W005-WDK7-PCB                            
033720     GSAMFIL-PCB.                                                         
033800                                                                          
033900     PERFORM A-INIT                                                       
034000                                                                          
034100     PERFORM S11-LAS-PARMIN                                               
034200                                                                          
034300     PERFORM IMS-GU-WDGX2253                                              
034400     PERFORM IMS-GHNP-WDGX2254                                            
034500                                                                          
034600     PERFORM UNTIL SEGMENT-SAKNAS                                         
034700       MOVE NEJ                       TO WS-EXT-TO-EXT                    
034800       MOVE NEJ                       TO WS-REF-TO-EXT                    
034900       MOVE NEJ                       TO WS-EXT-TO-REF                    
035000       MOVE NEJ                       TO WS-REF-TO-REF                    
035100       MOVE ZERO                      TO WS-IDANSK-PG                     
035200                                         WS-KDAVT                         
035210                                                                          
035300       PERFORM B-CHECK-DATE                                               
035310       MOVE SPACE                     TO SPAR-DCS-KDDC-EXT                
035320       MOVE SPACE                     TO SPAR-DCS-KDDC-REF                
035400       IF WS-FL-CONTINUE = JA                                             
035500         MOVE 2254-IDARTNR            TO W-IDARTNR                        
035600                                         W-IDARTNR-WDK9                   
035700                                         W-IDARTNR-D6-MIN                 
035800                                         W-IDARTNR-D6-MAX                 
035900         MOVE 2254-IDDC               TO W-IDDC                           
036000                                         W-IDDC-WDK9                      
036100                                         W-IDDC-D6-MIN                    
036200                                         W-IDDC-D6-MAX                    
036300         PERFORM IMS-GHU-WDK711                                           
036400         MOVE SLAG-IDLEVNR            TO WS-IDLEVNR-INNAN                 
036500         IF SLAG-IDDC-REF NOT = SPACE                                     
036600           MOVE W-IDDC                TO W-IDDC-MIN-E3                    
036700                                         W-IDDC-MAX-E3                    
036800           PERFORM IMS-GU-WDE301                                          
036900           IF SEGMENT-FINNS                                               
037000             MOVE NEJ                 TO WS-FL-CONTINUE                   
037100           END-IF                                                         
037200         END-IF                                                           
037210         PERFORM IMS-GU-WDB601-DC                                         
037220         IF SEGMENT-FINNS                                                 
037230           MOVE DCS-IDLANDX2          TO SPAR-IDLANDX2-EXT                
037231           MOVE DCS-KDDC              TO SPAR-DCS-KDDC-EXT                
037232         ELSE                                                             
037233           MOVE NEJ                   TO WS-FL-CONTINUE                   
037235         END-IF                                                           
037300       END-IF                                                             
037400       IF WS-FL-CONTINUE = JA                                             
037500         MOVE 2254-IDLEVNR-FRAM       TO W-IDLEVNR                        
037600         PERFORM IMS-GHNP-WDK722                                          
037700         IF SEGMENT-FINNS                                                 
037800           MOVE JA                    TO WS-WDK722                        
037900         ELSE                                                             
038000           MOVE NEJ                   TO WS-WDK722                        
038100         END-IF                                                           
038200         PERFORM IMS-GU-WDB601                                            
038300         IF SEGMENT-FINNS                                                 
038310           MOVE DCS-KDDC              TO SPAR-DCS-KDDC-REF                
038400           IF SLAG-IDDC-REF = SPACE                                       
038500             MOVE JA                  TO WS-EXT-TO-REF                    
038600             PERFORM C-CHANGE-TO-REFILL                                   
038700           ELSE                                                           
038800             MOVE JA                  TO WS-REF-TO-REF                    
038900           END-IF                                                         
039000         ELSE                                                             
039100           PERFORM D-CHANGE-TO-EXTERNAL                                   
039200         END-IF                                                           
039300       END-IF                                                             
039400       IF WS-FL-CONTINUE = JA                                             
039500         IF WS-EXT-TO-REF = JA OR WS-REF-TO-REF = JA                      
039600           PERFORM E-UPDATE-TO-REFILL                                     
039700         ELSE                                                             
039800           PERFORM F-UPDATE-TO-EXTERNAL                                   
039900         END-IF                                                           
040000         IF WS-EXT-TO-EXT = JA OR WS-EXT-TO-REF = JA                      
040100           PERFORM G-DELETE-SUGGESTED-CALLS                               
040200         END-IF                                                           
040210         IF WS-EXT-TO-REF = JA                                            
040215            IF (SPAR-DCS-KDDC-EXT = 'NC' OR                               
040216                SPAR-DCS-KDDC-EXT = 'NA')                                 
040217                AND                                                       
040218                SPAR-DCS-KDDC-REF (1:1) = 'N'                             
040219               MOVE 2254-IDARTNR      TO GSAM-IDARTNR                     
040220               MOVE 2254-IDDC         TO GSAM-IDDC                        
040221               MOVE 2254-IDLEVNR-FRAM TO GSAM-IDLEVNR                     
040222               PERFORM S02-SKRIV-GSAM                                     
040223            END-IF                                                        
040224         END-IF                                                           
040230                                                                          
040300         PERFORM IMS-DLET-WDGX2254                                        
040400                                                                          
040500       END-IF                                                             
040660                                                                          
040700       IF CHKP-ANT > CHKP-MAX                                             
040800         PERFORM X-TAG-CHECKPOINT                                         
040900       END-IF                                                             
041000                                                                          
041100       PERFORM IMS-GHNP-WDGX2254                                          
041200                                                                          
041300     END-PERFORM                                                          
041400                                                                          
041500                                                                          
041600     PERFORM Z-FINIT                                                      
041700                                                                          
041800     MOVE ZERO TO RETURN-CODE                                             
041900     GOBACK                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 A-INIT SECTION.                                                          
042300     SKIP2                                                                
042310     MOVE +14                TO MAX-GSAM                                  
042311***** OBS  GSAM-LÄNGDEN SKALL VARA POSTLÄNGDEN PLUS 2 BYTES (VB)**        
042320                                                                          
042400     OPEN INPUT  PARMIN                                                   
042500                                                                          
042600     ACCEPT DAGENS-DATUM FROM DATE                                        
042700                                                                          
042800     MOVE 'IDAG' TO DAT-KDDATFORM                                         
042900                                                                          
043000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
043100                     DAT-O-TIDATUM DAT-KDSVAR                             
043200                                                                          
043300     IF DAT-KDSVAR-OK                                                     
043400       IF DAT-TIAAVVD(5:1) > 5                                            
043500         MOVE 3 TO DAG-KVKALDAG                                           
043600       ELSE                                                               
043700         MOVE 2 TO DAG-KVKALDAG                                           
043800       END-IF                                                             
043900     ELSE                                                                 
044000       MOVE 'FEL FRÅN WDATKONV' TO FELTEXT-STR                            
044100       DISPLAY FELTEXT                                                    
044200       CALL FELLOG                                                        
044300     END-IF                                                               
044400                                                                          
044500     MOVE FUNCTION CURRENT-DATE (1:2) TO DAG-TISEKEL-FOM                  
044600     MOVE 003               TO DAG-KDCALL                                 
044700                                                                          
044800*INITIERA-WMSGKOM-AREAN                                                   
044900                                                                          
045000     ACCEPT WS-TIUPPDAT FROM DATE                                         
045100     ACCEPT WS-TIUPPTID FROM TIME                                         
045200     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
045300     MOVE +54                    TO MSG-KOM-KVLL                          
045400     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
045500     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
045600     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
045700     MOVE 'W2I19101'             TO MSG-KOM-IDCPYTXT                      
045800     MOVE 'ANSKLARM'             TO MSG-KOM-IDSNDNOD                      
045900     MOVE 'W2315200'             TO MSG-KOM-IDSNDJOB                      
046000     MOVE WS-TIUPPDAT            TO MSG-KOM-TIREGDAT                      
046100     MOVE WS-TIUPPTID            TO MSG-KOM-TIKLOCK                       
046200     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
046300                                                                          
046380     PERFORM IMS-RESTART                                                  
046381                                                                          
046390     PERFORM IMS-GHU-RESTART                                              
046400     IF 4580-IDWDGX2254 NOT = SPACE                                       
046410       MOVE 4580-IDWDGX2254 TO W-WDGXKEY-2254-X                           
046420     END-IF                                                               
046700                                                                          
046701     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
046702                                                                          
046710     PERFORM IMS-GSAMFIL-OPEN                                             
046800     IF 4580-KVPOST > ZERO                                                
046900        PERFORM AA-INIT-BMP                                               
047000     END-IF                                                               
047010                                                                          
047100     .                                                                    
047200     EJECT                                                                
047210 AA-INIT-BMP SECTION.                                                     
047220                                                                          
047230     MOVE NEJ                TO XRST-FIL-EOF                              
047240     MOVE ZERO               TO WS-ANT-POST-XRST                          
047250                                WS-ANT-POST-GSAM                          
047280                                                                          
047290*--------------------------- XRST-FIL KOPIERAS TILL GSAMFILEN             
047291*                            FRAM TILL CHECKPOINT-LÄGE                    
047292     OPEN INPUT  XRST-FIL                                                 
047293                                                                          
047294     PERFORM AAA-LAS-XRST                                                 
047295                                                                          
047296     PERFORM UNTIL (XRST-FIL-EOF = JA                                     
047297                OR  WS-ANT-POST-XRST >= 4580-KVPOST)                      
047298        MOVE XRST-POST   TO GSAM-W21350                                   
047299        PERFORM S02-SKRIV-GSAM                                            
047300        PERFORM AAA-LAS-XRST                                              
047301     END-PERFORM                                                          
047302                                                                          
047303     IF  XRST-FIL-EOF      = JA                                           
047304     OR  WS-ANT-POST-XRST  NOT = 4580-KVPOST                              
047305         DISPLAY 'W2135200: FEL I ÅTERSTARTEN, RESTART-FIL'               
047306         CALL ABEND USING    RKOD-ABEND-UTAN-DUMP                         
047307     END-IF                                                               
047308                                                                          
047309*------- HÄR KOPIERAS DEN XRST-POST TILL GSAM-FILEN,                      
047310*        SOM VID FÖREGÅENDE EXEKVERING, VAR DEN SISTA SOM                 
047311*        SKREVS PÅ GSAM-FILEN FÖRE SISTA CHKP.                            
047312                                                                          
047313     MOVE XRST-POST      TO GSAM-W21350                                   
047314     PERFORM S02-SKRIV-GSAM                                               
047315                                                                          
047316     CLOSE XRST-FIL                                                       
047317     .                                                                    
047318     EJECT                                                                
047319 AAA-LAS-XRST SECTION.                                                    
047320                                                                          
047321     READ XRST-FIL                                                        
047322          AT END MOVE JA TO XRST-FIL-EOF                                  
047323     END-READ                                                             
047324     IF XRST-FIL-EOF = NEJ                                                
047325        MOVE 'W21352'    TO POSTSUM-FDNAMN                                
047326        MOVE 'W21352D2'  TO POSTSUM-DDNAMN2                               
047327        MOVE SPACE       TO POSTSUM-TRANSTYP                              
047328        CALL POSTSUM USING POSTSUM-PARM                                   
047329                                                                          
047330        ADD +1           TO WS-ANT-POST-XRST                              
047331     END-IF                                                               
047332     .                                                                    
047333     EJECT                                                                
047340 B-CHECK-DATE SECTION.                                                    
047400                                                                          
047500     MOVE 2254-TILEVDAT TO DAG-TIAAMMDD-TOM                               
047600                                                                          
047700     CALL WDAGKONV USING DAG-KDCALL                                       
047800                         DAG-DATUM-AREA                                   
047900                         DAG-KDSVAR                                       
048000     IF DAG-KDSVAR = SPACE                                                
048100       IF DAGENS-DATUM < DAG-TIAAMMDD-FOM                                 
048200         MOVE NEJ TO WS-FL-CONTINUE                                       
048300       ELSE                                                               
048400         MOVE JA  TO WS-FL-CONTINUE                                       
048500       END-IF                                                             
048600     ELSE                                                                 
048700       MOVE 'FEL FRÅN WDAGKONV ' TO FELTEXT-STR                           
048800       DISPLAY FELTEXT                                                    
048900       CALL FELLOG                                                        
049000     END-IF                                                               
049100                                                                          
049200     .                                                                    
049300     EJECT                                                                
049400 C-CHANGE-TO-REFILL SECTION.                                              
049500                                                                          
049600     COMPUTE W-DAAVROP = XLAG-KVVECKOR-LT + DAGENS-DATUM                  
049700                                                                          
049800     PERFORM IMS-GU-WDD901                                                
049900     IF SEGMENT-FINNS                                                     
050000       MOVE 2                  TO W-KDAVROP                               
050100       PERFORM IMS-GNP-WDD905                                             
050200       IF SEGMENT-FINNS                                                   
050300         MOVE '780'            TO 2191-MID-KDLARM                         
050310         MOVE WS-IDLEVNR-INNAN TO 2191-MID-IDLEVNR                        
050400         PERFORM S01-SKAPA-LARM                                           
050500         MOVE NEJ              TO WS-FL-CONTINUE                          
050600       END-IF                                                             
050700     END-IF                                                               
050800     .                                                                    
050900     EJECT                                                                
051000 D-CHANGE-TO-EXTERNAL SECTION.                                            
051100                                                                          
051200     IF SLAG-IDDC-REF = SPACE                                             
051300       MOVE JA TO WS-EXT-TO-EXT                                           
051400     ELSE                                                                 
051500       MOVE JA TO WS-REF-TO-EXT                                           
051600     END-IF                                                               
051700                                                                          
051800     MOVE 2254-IDDC           TO W-IDDC-2206                              
051900     MOVE 2254-IDLEVNR-FRAM   TO W-IDLEVNR-2206                           
052000     PERFORM IMS-GU-WDGX2206                                              
052100     IF SEGMENT-SAKNAS                                                    
052200       MOVE '760'             TO 2191-MID-KDLARM                          
052210       MOVE 2254-IDLEVNR-FRAM TO 2191-MID-IDLEVNR                         
052300       PERFORM S01-SKAPA-LARM                                             
052400     ELSE                                                                 
052500       PERFORM DA-SKAPA-LARM-750                                          
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 DA-SKAPA-LARM-750 SECTION.                                               
053000                                                                          
053100     PERFORM IMS-GU-WDF116                                                
053200     IF SEGMENT-SAKNAS                                                    
053300       MOVE '750'                       TO 2191-MID-KDLARM                
053310       MOVE 2254-IDLEVNR-FRAM           TO 2191-MID-IDLEVNR               
053400       PERFORM S01-SKAPA-LARM                                             
053500       PERFORM IMS-DLET-WDGX2254                                          
053600       MOVE NEJ                         TO WS-FL-CONTINUE                 
053700     ELSE                                                                 
053800       IF 2254-IDLEVNR-SHIP-FRAM NOT = SPACE                              
053900         IF 2254-IDLEVNR-SHIP-FRAM NOT = 2254-IDLEVNR-FRAM                
054000            MOVE 2254-IDLEVNR-SHIP-FRAM TO W-IDLEVNR                      
054100            PERFORM IMS-GU-WDF116                                         
054200            IF SEGMENT-SAKNAS                                             
054300              MOVE '750'                TO 2191-MID-KDLARM                
054310              MOVE 2254-IDLEVNR-SHIP-FRAM TO 2191-MID-IDLEVNR             
054400              PERFORM S01-SKAPA-LARM                                      
054500              PERFORM IMS-DLET-WDGX2254                                   
054600              MOVE NEJ                  TO WS-FL-CONTINUE                 
054700            END-IF                                                        
054800         END-IF                                                           
054900       END-IF                                                             
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300 E-UPDATE-TO-REFILL SECTION.                                              
055401*-------------------------------------------------------                  
055410*    REF-TO-REF BYTE ÄR STOPPAT PÅ 2431,SKER I REFILLEN.                  
055420*-------------------------------------------------------                  
055430                                                                          
055500     PERFORM IMS-GHU-WDK711                                               
055600     MOVE 2254-IDLEVNR-FRAM TO SLAG-IDLEVNR                               
055700     MOVE DCS-IDDC          TO SLAG-IDDC-REF                              
055800     IF WS-EXT-TO-REF = JA                                                
055900       MOVE '0'  TO SLAG-IDREFTAB                                         
056000       MOVE ZERO TO SLAG-TIREFPAF                                         
056100                    SLAG-IDPERSON-BUY                                     
056200     END-IF                                                               
056300     PERFORM IMS-REPL-WDK711                                              
056400                                                                          
056500     PERFORM IMS-GHNP-WDK722                                              
056600     IF SEGMENT-FINNS                                                     
056700       MOVE ZERO  TO XLAG-KVSPANT                                         
056800                     XLAG-KVPB-JUST1                                      
056900                     XLAG-KVPB-JUST2                                      
057000                     XLAG-TIPBJUST-1                                      
057100                     XLAG-TIPBJUST-2                                      
057200                     XLAG-TIREFSTO-LOC                                    
057300                     XLAG-TILEVDAT                                        
057400       MOVE SPACE TO XLAG-IDLEVNR-SHIP                                    
057500                     XLAG-IDLEVNR-FRAM                                    
057600                     XLAG-IDLEVNR-SHIP-FRAM                               
057700       IF WS-EXT-TO-REF = JA                                              
057800         MOVE ZERO TO XLAG-KDLPSP                                         
057900       END-IF                                                             
058000       PERFORM IMS-REPL-WDK722                                            
058100     END-IF                                                               
058110                                                                          
058120     IF WS-EXT-TO-REF = JA                                                
058122       MOVE SPACE          TO SPAR-KDARTURS-REF                           
058123       MOVE ZERO           TO SPAR-VLARTNTO-REF                           
058124                              SPAR-VKART-REF                              
058125       IF DCS-CDC                                                         
058126         PERFORM IMS-GU-WDK611                                            
058127         IF SEGMENT-FINNS                                                 
058128           MOVE CLAG-VKART     TO SPAR-VKART-REF                          
058129           MOVE CLAG-KDARTURS  TO SPAR-KDARTURS-REF                       
058130           MOVE CLAG-VLARTNTO  TO SPAR-VLARTNTO-REF                       
058132         END-IF                                                           
058133       ELSE                                                               
058135         MOVE DCS-IDLANDX2        TO W-IDLANDX2                           
058136         PERFORM IMS-GU-WDK712                                            
058137         IF SEGMENT-FINNS                                                 
058138           MOVE LART-VKART     TO SPAR-VKART-REF                          
058139           MOVE LART-KDARTURS  TO SPAR-KDARTURS-REF                       
058140           MOVE LART-VLARTNTO  TO SPAR-VLARTNTO-REF                       
058141         END-IF                                                           
058142       END-IF                                                             
058143                                                                          
058144*- REFILLANDE DC SÄTTER VIKT,VOLYM O URSPRUNG VID BYTE                    
058145       PERFORM EA-UPDATE-NEW-KDARTURS-REF                                 
058150     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058301 EA-UPDATE-NEW-KDARTURS-REF SECTION.                                      
058303                                                                          
058311     MOVE NEJ                  TO REPL-WDK712-SW                          
058312     MOVE SPAR-IDLANDX2-EXT    TO W-IDLANDX2                              
058313     PERFORM IMS-GHU-WDK712                                               
058314     IF SEGMENT-FINNS                                                     
058315       IF SPAR-KDARTURS-REF = SPACE                                       
058316         CONTINUE                                                         
058317       ELSE                                                               
058318         MOVE SPAR-KDARTURS-REF  TO LART-KDARTURS                         
058319         MOVE JA TO  REPL-WDK712-SW                                       
058320       END-IF                                                             
058321                                                                          
058322       IF SPAR-VKART-REF > 0                                              
058323         MOVE SPAR-VKART-REF     TO LART-VKART                            
058324         MOVE JA TO  REPL-WDK712-SW                                       
058325       END-IF                                                             
058326                                                                          
058330       IF SPAR-VLARTNTO-REF > 0                                           
058340         MOVE SPAR-VLARTNTO-REF  TO LART-VLARTNTO                         
058341         MOVE JA TO  REPL-WDK712-SW                                       
058342       END-IF                                                             
058343     END-IF                                                               
058350     IF REPL-WDK712-SW = JA                                               
058351       PERFORM IMS-REPL-WDK712                                            
058353     END-IF                                                               
058354                                                                          
058360     .                                                                    
058370     EJECT                                                                
058400 F-UPDATE-TO-EXTERNAL SECTION.                                            
058500                                                                          
058600     PERFORM IMS-GHU-WDK711                                               
058700     MOVE 2254-IDLEVNR-FRAM      TO SLAG-IDLEVNR                          
058800     IF WS-REF-TO-EXT = JA                                                
058900       MOVE 'A'                  TO SLAG-IDREFTAB                         
059000       MOVE 'N'                  TO SLAG-FLORDSP                          
059100       MOVE 'N'                  TO SLAG-FLSPBULK                         
059110       MOVE 'N'                  TO SLAG-FLORDSP-EJRO                     
059120       MOVE 'N'                  TO SLAG-FLREFBEO                         
059200       MOVE SPACE                TO SLAG-IDDC-REF                         
059300     END-IF                                                               
059400     PERFORM IMS-REPL-WDK711                                              
059500                                                                          
059510     PERFORM IMS-GHU-WDK611                                               
059520     IF SEGMENT-FINNS                                                     
059530        IF CLAG-FLCDART = JA                                              
059540           MOVE NEJ              TO CLAG-FLCDART                          
059550           PERFORM IMS-REPL-WDK611                                        
059560        END-IF                                                            
059570     END-IF                                                               
059580                                                                          
059600     PERFORM IMS-GU-WDF116                                                
059700     IF SEGMENT-FINNS                                                     
059800       MOVE NDC-IDANSK-PG(1)     TO WS-IDANSK-PG                          
059900     END-IF                                                               
060000                                                                          
060100     MOVE 2254-IDLEVNR-SHIP-FRAM TO WS-IDLEVNR-SHIP                       
060200     MOVE 2254-IDLEVNR-FRAM      TO W-IDLEVNR-AVT                         
060300     PERFORM IMS-GNP-WDK723                                               
060400     IF SEGMENT-FINNS                                                     
060500       IF WS-IDLEVNR-SHIP = SPACE                                         
060600         MOVE SAVT-IDLEVNR-SHIP  TO WS-IDLEVNR-SHIP                       
060700       END-IF                                                             
060800       MOVE 1                    TO WS-KDAVT                              
060900     ELSE                                                                 
061000       MOVE 0                    TO WS-KDAVT                              
061100       MOVE '770'                TO 2191-MID-KDLARM                       
061110       MOVE 2254-IDLEVNR-FRAM    TO 2191-MID-IDLEVNR                      
061200       PERFORM S01-SKAPA-LARM                                             
061300     END-IF                                                               
061400                                                                          
061500     IF WS-IDLEVNR-SHIP = SPACE                                           
061600       MOVE 2254-IDLEVNR-FRAM    TO WS-IDLEVNR-SHIP                       
061700     END-IF                                                               
061800                                                                          
061900     PERFORM IMS-GHNP-WDK722                                              
062000     IF SEGMENT-FINNS                                                     
062100       MOVE SPACE                TO XLAG-IDLEVNR-FRAM                     
062200       MOVE SPACE                TO XLAG-IDLEVNR-SHIP-FRAM                
062300       MOVE ZERO                 TO XLAG-TILEVDAT                         
062400       MOVE WS-IDLEVNR-SHIP      TO XLAG-IDLEVNR-SHIP                     
062500       MOVE WS-KDAVT             TO XLAG-KDAVT                            
062600       IF WS-IDANSK-PG > 0                                                
062700         MOVE WS-IDANSK-PG       TO XLAG-IDANSK                           
062800       END-IF                                                             
062900                                                                          
063000       MOVE 1                    TO IX                                    
063100       PERFORM UNTIL IX > 5                                               
063200         MOVE ZERO               TO XLAG-TILEVDAG(IX)                     
063300         ADD +1                  TO IX                                    
063400       END-PERFORM                                                        
063500                                                                          
063600       MOVE 'S'                  TO XLAG-KDLEVPLF                         
063700       MOVE ZERO                 TO XLAG-TIMANLED                         
063800                                                                          
063900       IF WS-REF-TO-EXT = JA                                              
064000         MOVE 'N'                TO XLAG-FLJIT                            
064100         MOVE 'N'                TO XLAG-KDOPPLAN                         
064200         MOVE  1                 TO XLAG-IDPLANGR-AG                      
064300       END-IF                                                             
064400                                                                          
064500       IF WS-EXT-TO-EXT = JA                                              
064600         MOVE ZERO               TO XLAG-KDLPSP                           
064700       END-IF                                                             
064800                                                                          
064900       PERFORM IMS-REPL-WDK722                                            
065000                                                                          
065100     ELSE                                                                 
065200       MOVE ALL '+'              TO WDK7-W005WDK7                         
065300       MOVE 'WDK722'             TO WDK7-IDSEGM                           
065400       MOVE 2254-IDARTNR         TO WDK7-IDARTNR-KFB                      
065500       MOVE 2254-IDDC            TO WDK7-IDDC-KFB                         
065600       MOVE WS-IDLEVNR-SHIP TO WDK7-IDLEVNR-SHIP IN WDK7-WDK722           
065700       MOVE WS-KDAVT             TO WDK7-KDAVT                            
065800       IF WS-IDANSK-PG > 0                                                
065900         MOVE WS-IDANSK-PG       TO WDK7-IDANSK                           
066000       END-IF                                                             
066100       MOVE 'S'                  TO WDK7-KDLEVPLF                         
066200       IF WS-REF-TO-EXT = JA                                              
066300         MOVE 'N'                TO WDK7-FLJIT                            
066400         MOVE 'N'                TO WDK7-KDOPPLAN                         
066500         MOVE  1                 TO WDK7-IDPLANGR-AG                      
066600       END-IF                                                             
066700                                                                          
066800       CALL W005WDK7 USING WDK7-W005WDK7 W005-WDB6-PCB                    
066900                                         W005-WDK6-PCB                    
066910                                         W005-WDK7-PCB                    
067000     END-IF                                                               
067100                                                                          
067200     MOVE 2254-IDDC              TO W-IDDC-2213                           
067300     MOVE 2254-IDARTNR           TO 2214-IDARTNR                          
067400     PERFORM IMS-ISRT-WDGX2214                                            
067500                                                                          
067600     MOVE 2254-IDDC              TO W-IDDC-2203                           
067700     MOVE 2254-IDARTNR           TO 2204-IDARTNR                          
067800     MOVE 11                     TO 2204-KDLPORS                          
067900     PERFORM IMS-ISRT-WDGX2204                                            
068000     .                                                                    
068100     EJECT                                                                
068200 G-DELETE-SUGGESTED-CALLS SECTION.                                        
068300                                                                          
068400     MOVE WS-IDLEVNR-INNAN TO W-IDLEVNR                                   
068500                              W-IDLEVNR-D6-MIN                            
068600                              W-IDLEVNR-D6-MAX                            
068700                                                                          
068800     PERFORM IMS-GU-WDD902                                                
068900                                                                          
069000     IF SEGMENT-FINNS                                                     
069100                                                                          
069200       PERFORM IMS-GHNP-WDD904                                            
069300       IF SEGMENT-FINNS                                                   
069400         PERFORM IMS-DLET-WDD904                                          
069500       END-IF                                                             
069600                                                                          
069700       MOVE 1 TO W-KDAVROP                                                
069800       PERFORM IMS-GHNP-WDD905                                            
069900       PERFORM UNTIL SEGMENT-SAKNAS                                       
070000         PERFORM IMS-DLET-WDD905                                          
070100         PERFORM IMS-GHNP-WDD905                                          
070200       END-PERFORM                                                        
070300                                                                          
070400     END-IF                                                               
070500                                                                          
070600     PERFORM IMS-GHU-WDD601                                               
070700     PERFORM UNTIL SEGMENT-SAKNAS                                         
070800       PERFORM IMS-DLET-WDD601                                            
070900       PERFORM IMS-GHN-WDD601                                             
071000     END-PERFORM                                                          
071100     .                                                                    
071200     EJECT                                                                
071300 S01-SKAPA-LARM SECTION.                                                  
071400                                                                          
071500     MOVE LENGTH OF 2191-MID-W2I19101                                     
071600                                       TO   MSG-KVLL                      
071700     ADD  +17                          TO   MSG-KVLL                      
071800     MOVE   LOW-VALUE                  TO   MSG-KDZ1                      
071900                                            MSG-KDZ2                      
072000     MOVE   'W2T191X'                  TO   MSG-KDTRANS-1                 
072100     MOVE   '2191'                     TO   MSG-IDTRANS-1                 
072200     MOVE   '1'                        TO   MSG-KDMFSFOR-1                
072300     SKIP2                                                                
072400*    --- FLYTTA MIDDEN                                                    
072500     MOVE 2254-IDARTNR                 TO 2191-MID-IDARTNR                
072600     IF 2191-MID-KDLARM = '760'                                           
072610       IF 2254-IDDC(1:1) = '7'                                            
072700          MOVE WS-DEFAULT-IDANSK-CN    TO 2191-MID-IDANSK                 
072710       ELSE                                                               
072711          MOVE WS-DEFAULT-IDANSK-US    TO 2191-MID-IDANSK                 
072720       END-IF                                                             
072800     ELSE                                                                 
072900       IF WS-IDANSK-PG > +0                                               
073000         MOVE WS-IDANSK-PG             TO 2191-MID-IDANSK                 
073100       ELSE                                                               
073200         IF WS-WDK722 = JA                                                
073300           MOVE XLAG-IDANSK            TO 2191-MID-IDANSK                 
073400         ELSE                                                             
073401           IF 2254-IDDC(1:1) = '7'                                        
073500             MOVE WS-DEFAULT-IDANSK-CN TO 2191-MID-IDANSK                 
073510           ELSE                                                           
073511             MOVE WS-DEFAULT-IDANSK-US TO 2191-MID-IDANSK                 
073520           END-IF                                                         
073600         END-IF                                                           
073700       END-IF                                                             
073800     END-IF                                                               
073900     MOVE ZERO                         TO 2191-MID-KDCLAGER               
074000                                          2191-MID-TISENBEK-DAG           
074100                                          2191-MID-TISENBEK-KL            
074200                                          2191-MID-IDDISTR                
074300                                          2191-MID-IDKUNDNR               
074400     MOVE SPACE                        TO 2191-MID-IDKR                   
074500                                          2191-MID-IDKUNDRF               
074600     MOVE 'J'                          TO 2191-MID-FLNYLARM               
074700     MOVE 2254-IDDC                    TO 2191-MID-IDDC                   
074900                                                                          
075000     CALL W006KOM USING MSG-PCB                                           
075100                        ALT-PCB                                           
075200                        KOMA-PCB                                          
075300                        MSG-KOM-WMSGKOM                                   
075400                        MSG-IO-AREA                                       
075500                                                                          
075600     .                                                                    
075700     EJECT                                                                
075701 S02-SKRIV-GSAM SECTION.                                                  
075702                                                                          
075703     MOVE MAX-GSAM       TO GSAM-LRECL                                    
075704     PERFORM IMS-GSAMFIL-ISRT                                             
075705                                                                          
075706     MOVE 'W21352'       TO POSTSUM-FDNAMN                                
075707     MOVE 'W21352D2'     TO POSTSUM-DDNAMN2                               
075708     MOVE SPACE          TO POSTSUM-TRANSTYP                              
075709     CALL POSTSUM     USING POSTSUM-PARM                                  
075710                                                                          
075711     ADD +1              TO WS-ANT-POST-GSAM                              
075712     .                                                                    
075713     EJECT                                                                
075714                                                                          
075715 S11-LAS-PARMIN   SECTION.                                                
075720                                                                          
075730     READ PARMIN INTO PARM-AREA                                           
075740     .                                                                    
075750     EJECT                                                                
078300 Z-FINIT SECTION.                                                         
078400                                                                          
078500     CLOSE PARMIN                                                         
078600                                                                          
078700     PERFORM IMS-GHU-RESTART                                              
078800     MOVE SPACE      TO 4580-IDWDGX2254                                   
078900     ACCEPT 4580-TIUPPDAT FROM DATE                                       
079000     ACCEPT 4580-TIUPPTID FROM TIME                                       
079100     PERFORM IMS-REPL-RESTART                                             
079110                                                                          
079120     PERFORM IMS-GSAMFIL-CLOSE                                            
079130                                                                          
079140     MOVE 'S'          TO POSTSUM-OPKOD                                   
079150     CALL POSTSUM USING POSTSUM-PARM                                      
079200     .                                                                    
079300     EJECT                                                                
079400 X-TAG-CHECKPOINT   SECTION.                                              
079500                                                                          
079600     PERFORM IMS-GHU-RESTART                                              
079700     MOVE 2254-WDGX2254(1:7) TO 4580-IDWDGX2254                           
079800     ACCEPT 4580-TIUPPDAT FROM DATE                                       
079900     ACCEPT 4580-TIUPPTID FROM TIME                                       
080000     PERFORM IMS-REPL-RESTART                                             
080100                                                                          
080200     PERFORM IMS-CHECKPOINT                                               
080300     MOVE ZERO TO CHKP-ANT                                                
080400                                                                          
080500     MOVE 4580-IDWDGX2254 TO W-WDGXKEY-2254-X                             
080600     PERFORM IMS-GU-WDGX2253                                              
080700     .                                                                    
080800     EJECT                                                                
080900* --- IMS SEKTIONER ---                                                   
081000                                                                          
081100 IMS-GU-WDGX2253 SECTION.                                                 
081200                                                                          
081300     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2253-X ')'                    
081400          DELIMITED BY SIZE INTO SSA1                                     
081500     MOVE '  GE' TO GODK-STATUSKODER                                      
081600     CALL CBLTDLI USING GU 2254-PCB DLI-IO-WDGX01 SSA1                    
081700     MOVE 2254-STATUS-CODE TO STATUS-WS                                   
081800     PERFORM IMS-STATUSKONTROLL                                           
081900     .                                                                    
082000     SKIP2                                                                
082100 IMS-GHNP-WDGX2254 SECTION.                                               
082200                                                                          
082300     STRING 'WDGX2254(KY2254   >' W-WDGXKEY-2254-X                        
082400                    '&IDDC    >=' PARM-IDDC-FOM                           
082500                    '&IDDC    <=' PARM-IDDC-TOM ')'                       
082600          DELIMITED BY SIZE INTO SSA1                                     
082700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
082800     CALL CBLTDLI USING GHNP 2254-PCB DLI-IO-WDGX2254 SSA1                
082900     MOVE 2254-STATUS-CODE TO STATUS-WS                                   
083000     PERFORM IMS-STATUSKONTROLL                                           
083100     .                                                                    
083200     SKIP2                                                                
083300 IMS-DLET-WDGX2254 SECTION.                                               
083500                                                                          
083600     MOVE '  ' TO GODK-STATUSKODER                                        
083700     CALL CBLTDLI USING DLET 2254-PCB DLI-IO-WDGX2254                     
083800     MOVE 2254-STATUS-CODE TO STATUS-WS                                   
083900     PERFORM IMS-STATUSKONTROLL                                           
084000     ADD +1                  TO CHKP-ANT                                  
084100     .                                                                    
084200     EJECT                                                                
084300 IMS-GU-WDK712 SECTION.                                                   
084510                                                                          
084600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
084700          DELIMITED BY SIZE INTO SSA1                                     
084800     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
084900          DELIMITED BY SIZE INTO SSA2                                     
085000     MOVE '  GE' TO GODK-STATUSKODER                                      
085100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
085200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500     SKIP3                                                                
085501 IMS-GHU-WDK712 SECTION.                                                  
085503                                                                          
085504     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
085505          DELIMITED BY SIZE INTO SSA1                                     
085506     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
085507          DELIMITED BY SIZE INTO SSA2                                     
085508     MOVE '  GE' TO GODK-STATUSKODER                                      
085509     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
085510     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
085511     PERFORM IMS-STATUSKONTROLL                                           
085512     .                                                                    
085513     SKIP3                                                                
085514 IMS-REPL-WDK712 SECTION.                                                 
085516                                                                          
085517     MOVE '  ' TO GODK-STATUSKODER                                        
085518     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
085519     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
085520     PERFORM IMS-STATUSKONTROLL                                           
085521     ADD +1                  TO CHKP-ANT                                  
085522     .                                                                    
085523     EJECT                                                                
085524 IMS-GHU-WDK711 SECTION.                                                  
085525                                                                          
085530     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
085540          DELIMITED BY SIZE INTO SSA1                                     
085550     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
085560          DELIMITED BY SIZE INTO SSA2                                     
085570     MOVE '  ' TO GODK-STATUSKODER                                        
085580     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
085590     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
085591     PERFORM IMS-STATUSKONTROLL                                           
085592     .                                                                    
085593     SKIP3                                                                
085600 IMS-REPL-WDK711 SECTION.                                                 
085800                                                                          
085900     MOVE '  ' TO GODK-STATUSKODER                                        
086000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
086100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
086200     PERFORM IMS-STATUSKONTROLL                                           
086300     ADD +1                  TO CHKP-ANT                                  
086400     .                                                                    
086500     EJECT                                                                
086600 IMS-GHNP-WDK722 SECTION.                                                 
086700                                                                          
086800     STRING 'WDK722  *F(KDSEGKEY =' W-KDSEGKEY-X ')'                      
086900          DELIMITED BY SIZE INTO SSA1                                     
087000     MOVE '  GE' TO GODK-STATUSKODER                                      
087100     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK722 SSA1                  
087200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
087300     PERFORM IMS-STATUSKONTROLL                                           
087400     .                                                                    
087500     SKIP3                                                                
087600 IMS-REPL-WDK722 SECTION.                                                 
087800                                                                          
087900     MOVE '  ' TO GODK-STATUSKODER                                        
088000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
088100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
088200     PERFORM IMS-STATUSKONTROLL                                           
088300     ADD +1                  TO CHKP-ANT                                  
088400     .                                                                    
088500     EJECT                                                                
088600 IMS-GNP-WDK723 SECTION.                                                  
088700                                                                          
088800     STRING 'WDK723  (IDLEVNRA= ' W-IDLEVNRA-X ')'                        
088900          DELIMITED BY SIZE INTO SSA1                                     
089000     MOVE '  GE' TO GODK-STATUSKODER                                      
089100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
089200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     .                                                                    
089500     SKIP3                                                                
089501 IMS-GU-WDK611   SECTION.                                                 
089503                                                                          
089504     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
089505          DELIMITED BY SIZE INTO SSA1                                     
089506     MOVE 'WDK611  '          TO SSA2                                     
089507     MOVE '  GE'              TO GODK-STATUSKODER                         
089508     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
089509     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
089510     PERFORM IMS-STATUSKONTROLL                                           
089520     .                                                                    
089595     SKIP3                                                                
089596 IMS-GHU-WDK611   SECTION.                                                
089597                                                                          
089598     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
089599          DELIMITED BY SIZE INTO SSA1                                     
089600     MOVE 'WDK611  '          TO SSA2                                     
089601     MOVE '  GE'              TO GODK-STATUSKODER                         
089602     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
089603     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
089604     PERFORM IMS-STATUSKONTROLL                                           
089605     .                                                                    
089606     SKIP3                                                                
089607 IMS-REPL-WDK611    SECTION.                                              
089608     MOVE '  ' TO GODK-STATUSKODER                                        
089609     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
089610     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
089611     PERFORM IMS-STATUSKONTROLL                                           
089612     .                                                                    
089613     EJECT                                                                
089620 IMS-GU-WDE301 SECTION.                                                   
089700                                                                          
089800     STRING 'WDE301  (WDE301KY=>' W-WDE301KY-MIN-X                        
089900                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
090000                    '&IDARTNR  =' W-IDARTNR-X                             
090100                    '&KDREFTYP=>' W-KDREFTYP-MIN-X                        
090200                    '&KDREFTYP<=' W-KDREFTYP-MAX-X ')'                    
090300          DELIMITED BY SIZE INTO SSA1                                     
090400     MOVE '  GE' TO GODK-STATUSKODER                                      
090500     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-WDE301 SSA1                    
090600     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     .                                                                    
090900     EJECT                                                                
091000                                                                          
091100 IMS-GU-WDB601-DC SECTION.                                                
091210                                                                          
091220     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
091400          DELIMITED BY SIZE INTO SSA1                                     
091500     MOVE '  GE' TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
091700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000     EJECT                                                                
092010 IMS-GU-WDB601 SECTION.                                                   
092020                                                                          
092030     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-X ')'                         
092040          DELIMITED BY SIZE INTO SSA1                                     
092050     MOVE '  GE' TO GODK-STATUSKODER                                      
092060     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
092070     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
092080     PERFORM IMS-STATUSKONTROLL                                           
092090     .                                                                    
092091     EJECT                                                                
092100 IMS-GU-WDD901 SECTION.                                                   
092200                                                                          
092300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
092400            DELIMITED BY SIZE INTO SSA1                                   
092500     MOVE '  GE' TO GODK-STATUSKODER                                      
092600     CALL CBLTDLI USING GU   WDD9-PCB DLI-IO-WDD901 SSA1                  
092700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
092800     PERFORM IMS-STATUSKONTROLL                                           
092900     .                                                                    
093000     EJECT                                                                
093100 IMS-GU-WDD902 SECTION.                                                   
093200                                                                          
093300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
093400            DELIMITED BY SIZE INTO SSA1                                   
093500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
093600            DELIMITED BY SIZE INTO SSA2                                   
093700     MOVE '  GE' TO GODK-STATUSKODER                                      
093800     CALL CBLTDLI USING GU   WDD9-PCB DLI-IO-WDD902 SSA1 SSA2             
093900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200     EJECT                                                                
094300 IMS-GNP-WDD905 SECTION.                                                  
094400                                                                          
094500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X '&DAAVROP  >'               
094600            W-DAAVROP-X ')' DELIMITED BY SIZE INTO SSA1                   
094700     MOVE '  GE' TO GODK-STATUSKODER                                      
094800     CALL CBLTDLI USING GNP   WDD9-PCB DLI-IO-WDD905 SSA1                 
094900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
095000     PERFORM IMS-STATUSKONTROLL                                           
095100     .                                                                    
095200     EJECT                                                                
095300 IMS-GHNP-WDD904 SECTION.                                                 
095400                                                                          
095500     MOVE 'WDD904   ' TO SSA1                                             
095600     MOVE '  GE' TO GODK-STATUSKODER                                      
095700     CALL CBLTDLI USING GHNP   WDD9-PCB DLI-IO-WDD904 SSA1                
095800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100     EJECT                                                                
096200 IMS-GHNP-WDD905 SECTION.                                                 
096300                                                                          
096400     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
096500            DELIMITED BY SIZE INTO SSA1                                   
096600     MOVE '  GE' TO GODK-STATUSKODER                                      
096700     CALL CBLTDLI USING GHNP   WDD9-PCB DLI-IO-WDD905 SSA1                
096800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     EJECT                                                                
097200 IMS-DLET-WDD904 SECTION.                                                 
097400                                                                          
097500     MOVE '  ' TO GODK-STATUSKODER                                        
097600     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
097700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
097800     PERFORM IMS-STATUSKONTROLL                                           
097900     ADD +1                  TO CHKP-ANT                                  
098000     .                                                                    
098100     EJECT                                                                
098200 IMS-DLET-WDD905 SECTION.                                                 
098400                                                                          
098500     MOVE '  ' TO GODK-STATUSKODER                                        
098600     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
098700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
098800     PERFORM IMS-STATUSKONTROLL                                           
098900     ADD +1                  TO CHKP-ANT                                  
099000     .                                                                    
099100     EJECT                                                                
099200 IMS-GU-WDGX2206 SECTION.                                                 
099300                                                                          
099400     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
099500             DELIMITED BY SIZE INTO SSA1                                  
099600     STRING 'WDGX2206(KY2206   =' W-KY2206-X ')'                          
099700          DELIMITED BY SIZE INTO SSA2                                     
099800     MOVE '  GE' TO GODK-STATUSKODER                                      
099900     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2206 SSA1 SSA2             
100000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
100100     PERFORM IMS-STATUSKONTROLL                                           
100200     .                                                                    
100300     EJECT                                                                
100400 IMS-GU-WDF116 SECTION.                                                   
100500                                                                          
100600     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
100700          DELIMITED BY SIZE INTO SSA1                                     
100800     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
100900          DELIMITED BY SIZE INTO SSA2                                     
101000     MOVE '  GE' TO GODK-STATUSKODER                                      
101100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
101200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUSKONTROLL                                           
101400     .                                                                    
101500     EJECT                                                                
101600 IMS-ISRT-WDGX2214 SECTION.                                               
101800                                                                          
101900     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-2213-X ')'                    
102000          DELIMITED BY SIZE INTO SSA1                                     
102100     MOVE 'WDG302 ' TO SSA2                                               
102200     MOVE '  II' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2214 SSA1 SSA2           
102400     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     ADD +1                  TO CHKP-ANT                                  
102700     .                                                                    
102800     EJECT                                                                
102900 IMS-ISRT-WDGX2204 SECTION.                                               
103100                                                                          
103200     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-2203-X ')'                    
103300          DELIMITED BY SIZE INTO SSA1                                     
103400     MOVE 'WDG302 ' TO SSA2                                               
103500     MOVE '  II' TO GODK-STATUSKODER                                      
103600     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2204 SSA1 SSA2           
103700     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
103800     PERFORM IMS-STATUSKONTROLL                                           
103900     ADD +1                  TO CHKP-ANT                                  
104000     .                                                                    
104100     EJECT                                                                
104200 IMS-GHU-WDD601        SECTION.                                           
104300     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
104400                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
104500     DELIMITED BY SIZE INTO SSA1                                          
104600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
104700     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
104800     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
104900     PERFORM IMS-STATUSKONTROLL                                           
105000     .                                                                    
105100     EJECT                                                                
105200 IMS-GHN-WDD601        SECTION.                                           
105300     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
105400                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
105500     DELIMITED BY SIZE INTO SSA1                                          
105600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
105700     CALL CBLTDLI USING GHN WDD6-PCB DLI-IO-WDD601 SSA1                   
105800     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
105900     PERFORM IMS-STATUSKONTROLL                                           
106000     .                                                                    
106100     EJECT                                                                
106200 IMS-DLET-WDD601        SECTION.                                          
106400                                                                          
106500     MOVE '  ' TO GODK-STATUSKODER                                        
106600     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
106700     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
106800     PERFORM IMS-STATUSKONTROLL                                           
106900     .                                                                    
107000     EJECT                                                                
107100 IMS-GHU-RESTART  SECTION.                                                
107200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
107300          DELIMITED BY SIZE INTO SSA1                                     
107400     MOVE 'WDR470   '    TO SSA2                                          
107500     MOVE '  '         TO GODK-STATUSKODER                                
107600     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
107700     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     .                                                                    
108000     SKIP2                                                                
108100 IMS-REPL-RESTART SECTION.                                                
108200     MOVE '  '             TO GODK-STATUSKODER                            
108300     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
108400     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
108500     PERFORM IMS-STATUSKONTROLL                                           
108600     .                                                                    
108700     SKIP2                                                                
108701 IMS-GSAMFIL-OPEN SECTION.                                                
108702                                                                          
108703     MOVE 'OUT'               TO GSAMFIL-IO-AREA                          
108704     MOVE '  '                TO GODK-STATUSKODER                         
108705     CALL CBLTDLI          USING OPEN-GSAM                                
108706                                 GSAMFIL-PCB                              
108707                                 GSAMFIL-IO-AREA                          
108708     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
108709     PERFORM IMS-STATUSKONTROLL                                           
108710     .                                                                    
108711     SKIP2                                                                
108712 IMS-GSAMFIL-ISRT SECTION.                                                
108720                                                                          
108730     MOVE '  '                TO GODK-STATUSKODER                         
108740     CALL CBLTDLI          USING ISRT GSAMFIL-PCB                         
108741                                 GSAMFIL-IO-AREA                          
108750     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
108760     PERFORM IMS-STATUSKONTROLL                                           
108770     .                                                                    
108780     SKIP2                                                                
108790 IMS-GSAMFIL-CLOSE SECTION.                                               
108791                                                                          
108792     MOVE '  '                TO GODK-STATUSKODER                         
108793     CALL CBLTDLI          USING CLSE-GSAM                                
108794                                 GSAMFIL-PCB                              
108795     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
108796     PERFORM IMS-STATUSKONTROLL                                           
108797     .                                                                    
108798     EJECT                                                                
108820 IMS-RESTART SECTION.                                                     
108900     SKIP2                                                                
109000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
109100     MOVE '  ' TO GODK-STATUSKODER                                        
109200     CALL CBLTDLI USING XRST MSG-PCB                                      
109300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
109400                        CHKP-AREA-LENGTH CHKP-AREA                        
109500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109600     PERFORM IMS-STATUSKONTROLL                                           
109700     .                                                                    
109800     SKIP3                                                                
109900 IMS-CHECKPOINT SECTION.                                                  
110000     SKIP2                                                                
110100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
110200     MOVE '  XD' TO GODK-STATUSKODER                                      
110300     CALL CBLTDLI USING CHKP MSG-PCB                                      
110400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
110500                        CHKP-AREA-LENGTH CHKP-AREA                        
110600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110700     PERFORM IMS-STATUSKONTROLL                                           
110800                                                                          
110900     IF IMS-EJ-OK                                                         
111000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
111100       DISPLAY FELTEXT                                                    
111200       CALL FELLOG                                                        
111300     END-IF                                                               
111400     .                                                                    
111500     EJECT                                                                
111600 IMS-STATUSKONTROLL SECTION.                                              
111700     SKIP2                                                                
111800     SET STATUS-IX TO 1                                                   
111900     SEARCH GODK-STATUS                                                   
112000       AT END                                                             
112100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
112200           DELIMITED BY SIZE INTO FELTEXT                                 
112300         DISPLAY FELTEXT                                                  
112400         CALL FELLOG                                                      
112500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
112600         CONTINUE                                                         
112700     END-SEARCH                                                           
112800     .                                                                    
