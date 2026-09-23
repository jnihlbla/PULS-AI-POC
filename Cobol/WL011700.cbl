000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL011700.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   JUNI 2005.                                               
000600                                                                          
000700     REMARKS.                                                             
000800* WL011700 PROGRAM IS A REPLICA OF W4032200 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*    NAMN:       CARPARTS.LDC.PACKINGSPECHEADER                           
001200*                                                                         
001300                                                                          
001400     REMARKS.                                                             
001500*    FUNKTION.                                                            
001600*    PROGRAMMET ÄR ETT FRÅGEPROGRAM                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: WL0117                                              
002000*        REQU:        WL0117I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        RESP:        WL0117O1                                            
002400*                                                                         
002500* CHANGE LOG:                                                             
002600*                                                                         
002700* DIGAMBAR/020516                                                         
002800* READ THE WDE8: LDC CUSTOMER STEERING                                    
002900*                                                                         
003000*                                                                         
003100     EJECT                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP3                                                                
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700*    -- CHECKED BY WY2000                                                 
003800     SKIP3                                                                
003900 77   PROGRAM-NAMN           VALUE 'WL011700'                             
004000                                 PIC X(8).                                
004100 77  JA                          PIC X(1)    VALUE 'J'.                   
004200 77  YES                         PIC X(1)    VALUE 'J'.                   
004300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004400 77  INDX                        PIC S9(4)   COMP SYNC.                   
004500 77  FRAKTTEXT-INDX              PIC 9(2).                                
004600 77  FILLER                      PIC X(8) VALUE 'AAAAAAAA'.               
004700 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
004800 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
004900 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
005000 77  WS-IDDISTR                  PIC X(4).                                
005100 77  W-IDDISTR-NUM               PIC 9(4).                                
005200 77  WS-IDKUNDNR                 PIC X(6).                                
005300 77  WS-IDPRODNR                 PIC X(7).                                
005400 77  WS-IDPLKLST                 PIC S9(3).                               
005500 77  WS-JFR-IDPRODNR             PIC X(7).                                
005600 77  WS-IDORDER                  PIC S9(7).                               
005700 77  WS-IDTIDZON                 PIC X(2).                                
005800 77  WS-KDFRAKT                  PIC S9(3)   COMP-3.                      
005900 77  FILLER                      PIC X(8) VALUE 'BBBBBBBB'.               
006000 77  WS-KDPERSON                 PIC S9(3)   COMP-3.                      
006100 77  WS-KDMFSFOR                 PIC 9.                                   
006200 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   COMP-3 VALUE ZERO.           
006300 77  MOD-LAENGD                  PIC S9(4)   VALUE +507 COMP SYNC.        
006400 77  WS-TILOKDAT                 PIC 9(7).                                
006500 77  WS-VKORDNTO                 PIC S9(6)V9.                             
006600 77  WS-VLORDNTO                 PIC 9(4)V9(3)  VALUE ZERO.               
006700 77  FILLER                      PIC X(8) VALUE 'CCCCCCCC'.               
006800                                                                          
006900 01  WS-KDMATT                   PIC X.                                   
007000     88 US-MEASUREMENT           VALUE 'U'.                               
007100     88 SIS-MEASUREMENT          VALUE 'S'.                               
007200                                                                          
007300 01 WS-TILOKTID-NUM8.                                                     
007400     03 WS-TILOKTID-NUM6      PIC 9(6).                                   
007500     03 WS-ZERO               PIC 99.                                     
007600*                                                                         
007700 01  WS-ADGMT-GATA-SATS.                                                  
007800     03  FILLER                  PIC X(19).                               
007900     03  WS-KORD-IDARTNR-SATS    PIC X(08).                               
008000*                                                                         
008100 01  WS-ANT-RAD-ART-SATS.                                                 
008200     03  FILLER                  PIC X(18).                               
008300     03  WS-KORD-KVORDRAD        PIC X(03).                               
008400     03  WS-KORD-KVBEART-SATS    PIC X(06).                               
008500*                                                                         
008600 77  WS-SLINGA-KLAR              PIC X(1).                                
008700     88  SLINGA-KLAR                         VALUE 'J'.                   
008800*                                                                         
008900 77  WS-IDTRANS                  PIC X(4).                                
009000     88  WS-GODKAEND-BILD                    VALUE '4321' '4322'          
009100                                            '4323' '4324' '4325'.         
009200     88  EGEN-MID                            VALUE '4322'.                
009300*                                                                         
009400 01  WS-IDKUNDRF.                                                         
009500     03  WS-IDORDNR              PIC X(5).                                
009600     03  FILLER                  PIC X(5)  VALUE SPACE.                   
009700*                                                                         
009800 01  WS-FEL-FUNNET               PIC X     VALUE 'N'.                     
009900                                                                          
010000     88  FEL-FUNNET                        VALUE 'J'.                     
010100     88  FEL-EJ-FUNNET                     VALUE 'N'.                     
010200                                                                          
010300 77  WS-CP-CHN-EBCDIC            PIC X(5)  VALUE '935  '.                 
010400 77  WS-CP-SVE-EBCDIC            PIC X(5)  VALUE '278  '.                 
010500                                                                          
010600 77  FILLER                      PIC X(8) VALUE 'SUBPGM: '.               
010700 01  DYNAMISKA-SUBPROGRAM.                                                
010800                                                                          
010900     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
011000     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
011100     03  WWOMVAND                PIC X(8)  VALUE 'WWOMVAND'.              
011200     03  ABEND                   PIC X(8)  VALUE 'ABEND   '.              
011300     03  WZ01SEND                PIC X(8)  VALUE 'WZ01SEND'.              
011400     03  WZ01SUB                 PIC X(8)  VALUE 'WZ01SUB '.              
011500     03  WTRAUTF8                PIC X(8)  VALUE 'WTRAUTF8'.              
011600     EJECT                                                                
011700                                                                          
011800 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
011900*01  -COPY WTRAUTF8                                                       
012000     EJECT                                                                
012100*                                                                         
012200*01    -COPY WWDC99                                                       
012300     EJECT                                                                
012400*    --- PARAMETERS TO ABEND                                              
012500                                                                          
012600 77  FILLER                      PIC X(08)   VALUE 'ABENDARE'.            
012700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
012800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013000     SKIP2                                                                
013100 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
013200 01  MESSAGE-CODES.                                                       
013300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
013400     03  ERR-CORR-FIELDS         PIC X(3)    VALUE '023'.                 
013500     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
013600     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '304'.                 
013700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
013800     03  INF-NO-MORE-LINES       PIC X(3)    VALUE '001'.                 
013900     SKIP3                                                                
014000 01    FILLER                    PIC X(16)   VALUE 'WWOMVAND '.           
014100*01    -COPY WWOMVAND                                                     
014200     SKIP2                                                                
014300 01    FILLER                    PIC X(16)   VALUE 'WWDIST19'.            
014400*01    -COPY WWDIST19                                                     
014500     SKIP2                                                                
014600 01    FILLER                    PIC X(17)                                
014700                                 VALUE 'PARAMETER FOR S10'.               
014800 77  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
014900 77  TEST-IDKUNDNR               PIC 9(7)    COMP-3.                      
015000                                                                          
015100 01  W-SPAR-IDKUNDRF.                                                     
015200     03  FILLER                  PIC X(2)    VALUE '00'.                  
015300     03  W-SPAR-IDORDNR5         PIC X(5)    VALUE '+++++'.               
015400     03  FILLER                  PIC X(3)    VALUE '+++'.                 
015500 01  WS-BEGMRK.                                                           
015600                                                                          
015700     03  WS-BEGMRK-1           PIC X(30).                                 
015800     03  WS-BEGMRK-2           PIC X(30).                                 
015900     SKIP2                                                                
016000 01  WS-VAGNNR.                                                           
016100                                                                          
016200     03  WS-IDPRC                PIC X(4).                                
016300     03  FILLER                  PIC X     VALUE '-'.                     
016400     03  WS-IDLOTNR              PIC 9(3).                                
016500     SKIP2                                                                
016600 01  WS-TIUTSKR-NUM              PIC 9(7).                                
016700 01  WS-TIUTSTID-NUM             PIC 9(7).                                
016800     SKIP2                                                                
016900 01  WS-UTSKRIFTSDATUM.                                                   
017000                                                                          
017100     03  WS-TIUTSKR              PIC X(6).                                
017200     03  FILLER                  PIC X     VALUE '-'.                     
017300     03  WS-TIUTSTID             PIC X(6).                                
017400     SKIP2                                                                
017500 01  WS-PRODNR-AER-NYCKEL        PIC X     VALUE 'J'.                     
017600                                                                          
017700     88  PRODNR-AER-NYCKEL                 VALUE 'J'.                     
017800     EJECT                                                                
017900 01  FILLER                      PIC X(8) VALUE 'DLI-KEYS'.               
018000 01  NYCKLAR-TILL-DLI.                                                    
018100                                                                          
018200   03    W-WDE4E1KY-X.                                                    
018300     05    W-IDPRODNR-WDE4E      PIC S9(7)   VALUE ZERO  COMP-3.          
018400                                                                          
018500     03  W-E601-IDPRODNR-X.                                               
018600         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
018700                                                                          
018800     03  W-E4A1-WDE4KEY-X.                                                
018900         05  W-E4A1-IDDISTR      PIC S9(5)   COMP-3.                      
019000         05  W-E4A1-IDKUNDNR     PIC S9(7)   COMP-3.                      
019100         05  W-E4A1-IDKUNDRF.                                             
019200             07  W-E4A1-IDORDNR  PIC X(5).                                
019300             07  FILLER          PIC X(5)    VALUE SPACE.                 
019400                                                                          
019500     03  W-E401-WDE4KEY-X.                                                
019600         05  W-E401-IDDISTR      PIC S9(5)   COMP-3.                      
019700         05  W-E401-IDKUNDNR     PIC S9(7)   COMP-3.                      
019800         05  W-E401-IDKUNDRF.                                             
019900             07  W-E401-IDORDNR  PIC X(5).                                
020000             07  FILLER          PIC X(5)    VALUE SPACE.                 
020100         05  W-E401-IDPRODNR     PIC S9(7)   COMP-3.                      
020200         05  W-E401-IDPLKLST     PIC S9(3)   COMP-3.                      
020300                                                                          
020401     03  W-WDB101KY-X.                                                    
020500         05  W-IDPARTNR          PIC  X(9).                               
020601         05  W-IDFTG             PIC  9(2).                               
020701                                                                          
020801     03  W-IDGMT-X.                                                       
020901         05  W-IDDISTR           PIC S9(5)  COMP-3.                       
021001         05  W-IDKUNDNR          PIC S9(7)  COMP-3.                       
021101                                                                          
021201     03  W-473B-WDGXKEY-X.                                                
021301         05  W-473B-IDHTYP       PIC X(4)    VALUE '4732'.                
021401         05  W-473B-KDFRAKT      PIC S9(3)   COMP-3.                      
021501         05  W-473B-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
021601                                                                          
021701     03  W-4732-KDSEGKEY-X.                                               
021801         05  W-4732-KDSEGKEY     PIC X       VALUE '1'.                   
021901                                                                          
022001     03  W-0801-WDGXKEY-X.                                                
022101         05  W-0801-IDHTYP       PIC X(4)    VALUE '0801'.                
022201         05  W-0801-KDARBTYP     PIC X(8)    VALUE 'CDC     '.            
022301         05  W-0801-IDKEYREST    PIC X(18)   VALUE LOW-VALUE.             
022401                                                                          
022501     03  W-0802-IDPERSON-X.                                               
022601         05  W-0802-IDPERSON     PIC S9(3)   COMP-3 VALUE +0.             
022701*                                                                         
022801     03  W-WDQ2CSEQ-X.                                                    
022901       05  W-WDQ2C-IDGMTREF-X.                                            
023001         07  W-WDQ2C-IDDISTR     PIC S9(5)   COMP-3 VALUE +0.             
023101         07  W-WDQ2C-IDKUNDNR    PIC S9(7)   COMP-3 VALUE +0.             
023201         07  W-WDQ2C-IDKUNDRF.                                            
023301           09  FILLER            PIC  9(2)          VALUE ZERO.           
023401           09  W-WDQ2C-IDORDNR5                                           
023501                                 PIC  9(5)          VALUE ZERO.           
023601           09  FILLER            PIC  X(3)          VALUE SPACE.          
023701*                                                                         
023801     03  W-WDQ301KY-X.                                                    
023901         05  W-Q301-IDORDER      PIC S9(7)    COMP-3.                     
024001         05  W-Q301-IDDC         PIC  X(2).                               
024101         05  W-Q301-IDPRODNR     PIC S9(7)    COMP-3.                     
024201         05  W-Q301-IDPLKLST     PIC S9(3)    COMP-3.                     
024301*                                                                         
024401     03  W-IDDC-B6-X.                                                     
024501         05 W-IDDC-B6            PIC X(2).                                
024601                                                                          
024701 01  MEDDELANDE.                                                          
024801                                                                          
024901     03  FEL-1.                                                           
025001         05  FILLER              PIC X(40)   VALUE                        
025101             '701 ORDERN SAKNAS                       '.                  
025201         05  FILLER              PIC X(40)   VALUE                        
025301             '701 ORDER MISSING                       '.                  
025401     03  FEL-701 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
025501                                                                          
025601     03  FEL-2.                                                           
025701         05  FILLER              PIC X(40)   VALUE                        
025801             '749 FEL NYCKEL                          '.                  
025901         05  FILLER              PIC X(40)   VALUE                        
026001             '749 WRONG KEY                           '.                  
026101     03  FEL-749 REDEFINES FEL-2 OCCURS 2 PIC X(40).                      
026201     EJECT                                                                
026301******************************************************************        
026401 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
026501     SKIP3                                                                
026601*01  -COPY WZ01SUB                                                        
026701     EJECT                                                                
026801 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
026901     SKIP3                                                                
027001 01  REQU-AREA.                                                           
027101*    03  -COPY WZ01REQU                                                   
027201*    03  -COPY WL0117I1                                                   
027301     EJECT                                                                
027401 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
027501     SKIP3                                                                
027601 01  RESP-AREA.                                                           
027701*    03  -COPY WZ01RESP                                                   
027801*    03  -COPY WL0117O1                                                   
027901     SKIP3                                                                
028001******************************************************************        
028101 01  IMS-WS.                                                              
028201     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
028301     SKIP3                                                                
028401*                        **** STATUS-KOD FRÅN IMS                         
028501     03  STATUS-WS               PIC X(2).                                
028601         88  SEGMENT-FINNS                   VALUE '  '.                  
028701         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
028801     03  STATUS-WDE401-SEK-WS    PIC X(2).                                
028901         88  WDE401-SEK-FINNS                VALUE '  '.                  
029001         88  WDE401-SEK-SAKNAS               VALUE 'GE' 'GB'.             
029101     SKIP3                                                                
029201     03  GODK-STATUSKODER.                                                
029301         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
029401     SKIP3                                                                
029501 01  SSA1                        PIC X(64).                               
029601 01  SSA2                        PIC X(64).                               
029701     EJECT                                                                
029801*                            IMS FUNKTIONSKODER                           
029901*01  -COPY W0003                                                          
030001     EJECT                                                                
030101*                            DLI INPUT-OUTPUT AREA                        
030201 01  DLI-IO-AREA.                                                         
030301     03  IO-AREA                 PIC X(652)  VALUE SPACE.                 
030401     SKIP2                                                                
030501*    03  WDE601   -COPY WDE601     -RED IO-AREA.                          
030601     EJECT                                                                
030701*    03  WDE401   -COPY WDE401     -RED IO-AREA.                          
030801     EJECT                                                                
030901*    03  WL473211 -COPY WDGX4732   -RED IO-AREA.                          
031001     EJECT                                                                
031101*    03  WDQ201   -COPY WDQ201     -RED IO-AREA.                          
031201     EJECT                                                                
031301 01  DLI-IO-WDE401.                                                       
031401*    03        -COPY WDE401    -PRE E4E-                                  
031501     EJECT                                                                
031601 01  FILLER                      PIC X(16)  VALUE 'WDB1-AREA'.            
031701*01  -COPY WDB101                                                         
031801     EJECT                                                                
031901 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
032001 01  DLI-IO-AREA-WDB2.                                                    
032101*    03  -COPY WDB201                                                     
032201     EJECT                                                                
032301 01  FILLER                      PIC X(16)  VALUE 'WDQ301-AREA'.          
032401*01  -COPY WDQ301                                                         
032501     EJECT                                                                
032601                                                                          
032701 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032801 01   DLI-IO-AREA-B601.                                                   
032901*     03  -COPY WDB601                                                    
033001                                                                          
033101 LINKAGE SECTION.                                                         
033201*01  -COPY W0009     -PRE MSG-                                            
033301     EJECT                                                                
033401*01  -COPY W0008     -PRE WDE62-                                          
033501     05  FILLER                  PIC X.                                   
033601     EJECT                                                                
033701*01  -COPY W0008     -PRE 4732-                                           
033801     05  FILLER                  PIC X.                                   
033901     EJECT                                                                
034001*01  -COPY W0008     -PRE WDE4-                                           
034101     05  FILLER                  PIC X.                                   
034201     EJECT                                                                
034301*01  -COPY W0008     -PRE WDE4E-                                          
034401     05  FILLER                  PIC X.                                   
034501     EJECT                                                                
034601*01  -COPY W0008     -PRE WDE42-                                          
034701     05  FILLER                  PIC X.                                   
034801     EJECT                                                                
034901*01  -COPY W0008     -PRE WDE6-                                           
035001     05  FILLER                  PIC X.                                   
035101     EJECT                                                                
035201*01  -COPY W0008     -PRE ORQICSQ-                                        
035301     05  FILLER                  PIC X.                                   
035401     EJECT                                                                
035501*01  -COPY W0008     -PRE BETC-                                           
035601     05  FILLER                  PIC X.                                   
035701     EJECT                                                                
035801*01  -COPY W0008     -PRE GMTA-                                           
035901     05  FILLER                  PIC X.                                   
036001     EJECT                                                                
036101*01  -COPY W0008     -PRE ORQA-                                           
036201     05  FILLER                  PIC X.                                   
036301     EJECT                                                                
036401*01  -COPY W0008     -PRE WDB6-                                           
036501     05  FILLER                  PIC X.                                   
036601     EJECT                                                                
036701 PROCEDURE DIVISION USING MSG-PCB WDE4E-PCB                               
036801                                  WDE62-PCB   4732-PCB  WDE4-PCB          
036901                                  WDE42-PCB   WDE6-PCB ORQICSQ-PCB        
037001                                  BETC-PCB    GMTA-PCB  ORQA-PCB          
037101                                  WDB6-PCB.                               
037201     SKIP2                                                                
037301     ENTRY 'DLITCBL' USING MSG-PCB WDE4E-PCB                              
037401                                   WDE62-PCB 4732-PCB WDE4-PCB            
037501                                   WDE42-PCB WDE6-PCB ORQICSQ-PCB         
037601                                   BETC-PCB  GMTA-PCB ORQA-PCB            
037701                                   WDB6-PCB.                              
037801     SKIP2                                                                
037901 MAIN SECTION.                                                            
038001                                                                          
038101     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
038201     IF SUB-KDRC = 0                                                      
038301         PERFORM A-INIT-SPARA-INPUT                                       
038401         PERFORM B-VILKA-NYCKLAR-ANVAENDS                                 
038501                                                                          
038601         IF PRODNR-AER-NYCKEL                                             
038701             IF WS-IDPRODNR NUMERIC AND                                   
038801                FEL-EJ-FUNNET                                             
038901                 PERFORM C-LAES-MED-PRODNR                                
039001             ELSE                                                         
039101                 MOVE ERR-WRONG-KEY     TO RESP-IDMSG-ERROR               
039201                 MOVE 'IDPRODNR'        TO RESP-IDELMT-ERROR              
039301             END-IF                                                       
039401         ELSE                                                             
039501             IF  WS-IDDISTR NUMERIC                                       
039601             AND WS-IDKUNDNR NUMERIC                                      
039701             AND WS-IDORDNR  NUMERIC                                      
039801             AND FEL-EJ-FUNNET                                            
039901                 PERFORM D-LAES-MED-DISTR-KUND-ORDER                      
040001             ELSE                                                         
040101                 MOVE ERR-WRONG-KEY     TO   RESP-IDMSG-ERROR             
040201                 MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR              
040301             END-IF                                                       
040401         END-IF                                                           
040501                                                                          
040601       PERFORM S02-RETURN-RESPONSE                                        
040701     END-IF                                                               
040801     MOVE ZERO TO RETURN-CODE                                             
040901     GOBACK                                                               
041001     .                                                                    
041101     EJECT                                                                
041201 A-INIT-SPARA-INPUT SECTION.                                              
041301                                                                          
041401     MOVE 'STA A-INIT        ' TO PGM-POS                                 
041501                                                                          
041601     MOVE ALL '+'              TO RESP-WL0117O1                           
041701     MOVE 001                  TO RESP-IDMSGVER                           
041801     MOVE SPACE                TO RESP-IDMSG-ERROR                        
041901                                  RESP-IDMSG-INFO                         
042001                                  RESP-IDELMT-ERROR                       
042101                                                                          
042201*    ACCEPT DAGENS-DATUM FROM DATE                                        
042301*    ACCEPT DAGENS-TID   FROM TIME                                        
042401                                                                          
042501     MOVE JA                 TO WS-PRODNR-AER-NYCKEL                      
042601     MOVE NEJ                TO WS-FEL-FUNNET                             
042701                                                                          
042801                                                                          
042901       MOVE +2 TO INDX                                                    
043001                                                                          
043101     IF REQU-IDDISTR-KEY = ALL '+'                                        
043201         MOVE ZERO             TO WS-IDDISTR                              
043301     ELSE                                                                 
043401         MOVE REQU-IDDISTR-KEY TO WS-IDDISTR                              
043501     END-IF                                                               
043601                                                                          
043701     IF REQU-IDKUNDNR-KEY = ALL '+'                                       
043801         MOVE ZERO              TO WS-IDKUNDNR                            
043901     ELSE                                                                 
044001         MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR                            
044101     END-IF                                                               
044201                                                                          
044301     IF REQU-IDORDNR-KEY     = ALL '+'                                    
044401         MOVE ZERO             TO WS-IDORDNR                              
044501     ELSE                                                                 
044601         MOVE REQU-IDORDNR-KEY TO WS-IDORDNR                              
044701     END-IF                                                               
044801                                                                          
044901     IF REQU-IDPRODNR-KEY = ALL '+'                                       
045001         MOVE ZERO              TO WS-IDPRODNR                            
045101     ELSE                                                                 
045201         MOVE REQU-IDPRODNR-KEY TO WS-IDPRODNR                            
045301     END-IF                                                               
045401                                                                          
045600     IF WS-IDDISTR NUMERIC                                                
045700       MOVE WS-IDDISTR   TO RESP-IDDISTR-KEY                              
045800       INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE           
045900     END-IF                                                               
046000                                                                          
046100     IF WS-IDKUNDNR NUMERIC                                               
046200       MOVE WS-IDKUNDNR  TO RESP-IDKUNDNR-KEY                             
046300       INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE          
046400     END-IF                                                               
046500                                                                          
046600     IF WS-IDORDNR NUMERIC                                                
046700       MOVE WS-IDORDNR   TO RESP-IDORDNR-KEY                              
046800       INSPECT RESP-IDORDNR-KEY REPLACING LEADING ZERO BY SPACE           
046900     END-IF                                                               
047000                                                                          
047100     IF WS-IDPRODNR NUMERIC                                               
047200       MOVE WS-IDPRODNR  TO RESP-IDPRODNR-KEY                             
047300       INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE          
047400     END-IF                                                               
047500                                                                          
047600     MOVE REQU-IDDC-KEY   TO RESP-IDDC-KEY                                
047700                             WS-IDDC                                      
047800                                                                          
047901     MOVE REQU-KDMATT       TO  WS-KDMATT                                 
048001     IF WS-KDMATT = 'U'                                                   
048100       MOVE 'U'              TO WS-KDMATT                                 
048200     ELSE                                                                 
048300       MOVE 'S'              TO WS-KDMATT                                 
048400     END-IF                                                               
048500                                                                          
048600     MOVE 'END A-INIT        ' TO PGM-POS                                 
048700     .                                                                    
048800     EJECT                                                                
048900 B-VILKA-NYCKLAR-ANVAENDS SECTION.                                        
049000     MOVE 'STA B-VILKA-NYCKLAR-ANVAENDS ' TO PGM-POS                      
049100                                                                          
049200        IF REQU-IDPRODNR-KEY NOT = ALL '+'                                
049300                                                                          
049400            MOVE JA TO WS-PRODNR-AER-NYCKEL                               
049500                                                                          
049600        ELSE                                                              
049700                                                                          
049800           IF REQU-IDDISTR-KEY = ALL '+'                                  
049900           AND REQU-IDKUNDNR-KEY = ALL '+'                                
050000           AND REQU-IDORDNR-KEY = ALL '+'                                 
050100                                                                          
050200              IF WS-IDPRODNR NUMERIC                                      
050300              AND WS-IDPRODNR > ZERO                                      
050400                 MOVE JA     TO WS-PRODNR-AER-NYCKEL                      
050500              ELSE                                                        
050600                 MOVE NEJ TO WS-PRODNR-AER-NYCKEL                         
050700              END-IF                                                      
050800           ELSE                                                           
050900              MOVE NEJ TO WS-PRODNR-AER-NYCKEL                            
051000           END-IF                                                         
051100        END-IF                                                            
051200     MOVE 'END B-VILKA-NYCKLAR-ANVAENDS ' TO PGM-POS                      
051300     .                                                                    
051400     EJECT                                                                
051500 C-LAES-MED-PRODNR SECTION.                                               
051600     MOVE 'STA C-LAES-MED-PRODNR        ' TO PGM-POS                      
051700                                                                          
051800     MOVE WS-IDPRODNR       TO W-E601-IDPRODNR                            
051900     PERFORM IMS-GU-WDE601-KVAL                                           
052000     IF SEGMENT-FINNS                                                     
052100         PERFORM S01-DATA-FRAAN-WDE601                                    
052200         MOVE VORD-IDPRODNR      TO W-IDPRODNR-WDE4E                      
052300         PERFORM IMS-GU-WDE401-ESEQ                                       
052400         MOVE E4E-KORD-IDKUNDRF TO RESP-IDKUNDRF                          
052500         MOVE E4E-KORD-TIORDREG TO RESP-TIORDREG                          
052600         IF E4E-KORD-FLLSBOK = NEJ                                        
052700             MOVE '   STOCKUPD.' TO RESP-LAGERAVBOK                       
052800           MOVE NEJ              TO RESP-FLLSBOK                          
052900         END-IF                                                           
053000         MOVE E4E-KORD-IDDISTR TO W-E401-IDDISTR                          
053100                                  W-IDDISTR                               
053200                                  W-WDQ2C-IDDISTR                         
053300         MOVE E4E-KORD-IDKUNDNR TO W-E401-IDKUNDNR                        
053400                                  W-IDKUNDNR                              
053500                                  W-WDQ2C-IDKUNDNR                        
053600         MOVE E4E-KORD-IDKUNDRF TO W-E401-IDKUNDRF                        
053700         MOVE E4E-KORD-IDKUNDRF(1:5)                                      
053800                               TO W-WDQ2C-IDORDNR5                        
053900         MOVE E4E-KORD-IDPRODNR TO W-E401-IDPRODNR                        
054000         MOVE E4E-KORD-IDPLKLST TO W-E401-IDPLKLST                        
054100                                  WS-IDPLKLST                             
054200         MOVE E4E-KORD-IDORDER TO WS-IDORDER                              
054300         PERFORM IMS-GET-ORQI01-CSEQ                                      
054400         IF SEGMENT-FINNS                                                 
054500* FIX - WS-IDDISTR WAS NOT PASSED CORRECTLY  FOR S08                      
054600           MOVE    OHUV-IDDISTR    TO W-IDDISTR-NUM                       
054700           MOVE    W-IDDISTR-NUM  TO WS-IDDISTR                           
054800           PERFORM S08-HAMTA-Q201-OHUV-DATA                               
054900         END-IF                                                           
055000         PERFORM S05-BEHANDLA-HAENDELSEBASER                              
055100         PERFORM S07-GET-WDQ301-INFO                                      
055200     ELSE                                                                 
055300         MOVE ERR-ORDER-MISSING TO RESP-IDMSG-ERROR                       
055400     END-IF                                                               
055500     MOVE 'END C-LAES-MED-PRODNR        ' TO PGM-POS                      
055600     .                                                                    
055700     EJECT                                                                
055800 D-LAES-MED-DISTR-KUND-ORDER SECTION.                                     
055900     MOVE 'STA D-LAES-MED-DISTR-KUND    ' TO PGM-POS                      
056000     MOVE 'N'             TO WS-SLINGA-KLAR                               
056100                                                                          
056200     MOVE WS-IDDISTR      TO W-E4A1-IDDISTR                               
056300                             W-IDDISTR                                    
056400     MOVE WS-IDKUNDNR     TO W-E4A1-IDKUNDNR                              
056500                             W-IDKUNDNR                                   
056600     MOVE WS-IDORDNR      TO W-E4A1-IDORDNR                               
056700     PERFORM IMS-GU-WDE401-SEK                                            
056800                                                                          
056900     IF WDE401-SEK-FINNS                                                  
057000        MOVE KORD-IDPRODNR         TO WS-IDPRODNR                         
057100        PERFORM UNTIL WDE401-SEK-SAKNAS OR                                
057200                      SLINGA-KLAR                                         
057300           MOVE KORD-IDDISTR       TO W-E401-IDDISTR                      
057400                                      W-WDQ2C-IDDISTR                     
057500           MOVE KORD-IDKUNDNR      TO W-E401-IDKUNDNR                     
057600                                      W-WDQ2C-IDKUNDNR                    
057700           MOVE KORD-IDORDNR5      TO W-E401-IDORDNR                      
057800                                      W-WDQ2C-IDORDNR5                    
057900           MOVE KORD-IDPRODNR      TO W-E401-IDPRODNR                     
058000           MOVE KORD-IDORDER       TO WS-IDORDER                          
058100           MOVE KORD-IDPLKLST      TO W-E401-IDPLKLST                     
058200                                      WS-IDPLKLST                         
058300           PERFORM IMS-GET-ORQI01-CSEQ                                    
058400           IF SEGMENT-FINNS                                               
058500             PERFORM S08-HAMTA-Q201-OHUV-DATA                             
058600           END-IF                                                         
058700           PERFORM IMS-GU-WDE401-KVAL                                     
058800           MOVE KORD-KVORDRAD-LEVPL   TO WS-SPAR-KVORDRAD-LEVPL           
058900           PERFORM S02-DATA-FRAAN-WDE401                                  
059000           MOVE KORD-IDPRODNR         TO W-E601-IDPRODNR                  
059100           PERFORM IMS-GU-WDE601-OKVAL                                    
059200           IF SEGMENT-FINNS                                               
059300              IF PRODNR-AER-NYCKEL                                        
059400                 MOVE VORD-IDPRODNR TO WS-JFR-IDPRODNR                    
059500                 IF WS-JFR-IDPRODNR = WS-IDPRODNR                         
059600                    CONTINUE                                              
059700                 ELSE                                                     
059800                    SET SEGMENT-SAKNAS TO TRUE                            
059900                 END-IF                                                   
060000              ELSE                                                        
060100                 IF VORD-IDDC = REQU-IDDC-KEY AND                         
060200                    WS-SPAR-KVORDRAD-LEVPL = ZERO                         
060300                    CONTINUE                                              
060400                 ELSE                                                     
060500                    SET SEGMENT-SAKNAS TO TRUE                            
060600                 END-IF                                                   
060700              END-IF                                                      
060800           END-IF                                                         
060900           IF SEGMENT-FINNS                                               
061000              MOVE 'J'             TO WS-SLINGA-KLAR                      
061100              PERFORM S01-DATA-FRAAN-WDE601                               
061200              PERFORM S05-BEHANDLA-HAENDELSEBASER                         
061300              PERFORM S07-GET-WDQ301-INFO                                 
061400           END-IF                                                         
061500           PERFORM IMS-GN-WDE401-SEK                                      
061600        END-PERFORM                                                       
061700     ELSE                                                                 
061800        MOVE ERR-ORDER-MISSING TO RESP-IDMSG-ERROR                        
061900     END-IF                                                               
062000*                                                                         
062100     IF NOT SLINGA-KLAR                                                   
062200        MOVE ERR-ORDER-MISSING TO RESP-IDMSG-ERROR                        
062300     END-IF                                                               
062400     MOVE 'END D-LAES-MED-DISTR-KUND    ' TO PGM-POS                      
062500     .                                                                    
062600     SKIP2                                                                
062700 S01-DATA-FRAAN-WDE601       SECTION.                                     
062800     MOVE 'STA S01-DATA-FRAAN-WDE601    ' TO PGM-POS                      
062900                                                                          
063000     MOVE VORD-KDFRAKT       TO WS-KDFRAKT                                
063100                                RESP-KDFRAKT                              
063200     MOVE VORD-IDDC          TO RESP-IDDC-KEY                             
063300     MOVE VORD-IDDISTR       TO RESP-IDDISTR                              
063400     MOVE VORD-IDKUNDNR      TO RESP-IDKUNDNR                             
063500     MOVE VORD-KDORDKL       TO RESP-KDORDKL                              
063600     MOVE VORD-DABEGPAC (3:6)  TO RESP-TIBEGPAC                           
063700                                                                          
063800                                                                          
063900     MOVE VORD-TIUTSKR         TO WS-TIUTSKR-NUM                          
064000     MOVE WS-TIUTSKR-NUM(2:6)  TO WS-TIUTSKR                              
064100     MOVE VORD-TIUTSTID        TO WS-TIUTSTID-NUM                         
064200     MOVE WS-TIUTSTID-NUM(2:6) TO WS-TIUTSTID                             
064300     MOVE WS-UTSKRIFTSDATUM    TO RESP-UTSKRIFTSDATUM                     
064400                                                                          
064500     MOVE VORD-TIPACKN-SK    TO RESP-TIPACKN-SK                           
064600     MOVE VORD-IDLOTNR       TO WS-IDLOTNR                                
064700     MOVE WS-VAGNNR          TO RESP-VAGNNR                               
064800                                                                          
064900     MOVE VORD-VLORDNTO      TO WS-VLORDNTO                               
065000     MOVE VORD-VKORDNTO      TO WS-VKORDNTO                               
065100     IF US-MEASUREMENT                                                    
065200       COMPUTE WS-VLORDNTO ROUNDED =                                      
065300               WS-VLORDNTO * CONV-M3-TO-FT3 END-COMPUTE                   
065400       COMPUTE WS-VKORDNTO ROUNDED =                                      
065500               WS-VKORDNTO * CONV-KG-TO-LB  END-COMPUTE                   
065600     END-IF                                                               
065700     MOVE WS-VLORDNTO        TO RESP-VLORDNTO                             
065800     MOVE WS-VKORDNTO        TO RESP-VKORDNTO                             
065900                                                                          
066000     MOVE VORD-KVORDRAD      TO RESP-KVORDRAD                             
066100     MOVE VORD-IDDC          TO FRAKTTEXT-INDX                            
066200     MOVE VORD-BEGMRK        TO WS-BEGMRK                                 
066300     MOVE WS-BEGMRK-1        TO RESP-BEGMRK-1                             
066400     MOVE WS-BEGMRK-2        TO RESP-BEGMRK-2                             
066500     MOVE 'END S01-DATA-FRAAN-WDE601    ' TO PGM-POS                      
066600     .                                                                    
066700     SKIP2                                                                
066800 S02-DATA-FRAAN-WDE401 SECTION.                                           
066900     MOVE 'STA S02-DATA-FRAAN-WDE401    ' TO PGM-POS                      
067000                                                                          
067100     MOVE KORD-IDKUNDRF      TO RESP-IDKUNDRF                             
067200     MOVE KORD-TIORDREG      TO RESP-TIORDREG                             
067300     IF KORD-FLLSBOK = NEJ                                                
067400         MOVE '   STOCKUPD.' TO RESP-LAGERAVBOK                           
067500       MOVE NEJ              TO RESP-FLLSBOK                              
067600     END-IF                                                               
067700     MOVE 'END S02-DATA-FRAAN-WDE401    ' TO PGM-POS                      
067800     .                                                                    
067900     SKIP2                                                                
068000 S04-GET-CUSTOMER-INFO           SECTION.                                 
068100     MOVE 'STA S04-GET-CUSTOMER-INFO    ' TO PGM-POS                      
068200                                                                          
068300     IF  GMT-ADGMT = SPACE                                                
068400     AND GMT-BEGMT = SPACE                                                
068500                                                                          
068600       MOVE REQU-IDDC-KEY     TO W-IDDC-B6                                
068601       PERFORM IMS-GU-WDB601                                              
068602       MOVE DCS-IDFTG         TO W-IDFTG                                  
068603                                                                          
068604       MOVE GMT-IDPARTNR      TO W-IDPARTNR                               
068702                                                                          
068801       PERFORM IMS-GU-BETC-WDB101                                         
068901       IF SEGMENT-FINNS                                                   
069001         MOVE BET-BEBETRAD-1  TO RESP-BEGMT-RAD1                          
069101         MOVE BET-BEBETRAD-2  TO RESP-BEGMT-RAD2                          
069201         MOVE BET-ADBETRAD-1  TO RESP-ADGMT-GATA                          
069301         MOVE BET-ADBETRAD-2  TO RESP-ADGMT-PADR                          
069401       ELSE                                                               
069501         MOVE 'ADDRESS MISSING'   TO RESP-BEGMT-RAD1                      
069601       END-IF                                                             
069701     ELSE                                                                 
069801       MOVE GMT-BEGMT-RAD1 TO RESP-BEGMT-RAD1                             
069901       MOVE GMT-BEGMT-RAD2 TO RESP-BEGMT-RAD2                             
070001       MOVE GMT-ADGMT-GATA TO RESP-ADGMT-GATA                             
070101       MOVE GMT-ADGMT-PADR TO RESP-ADGMT-PADR                             
070201       MOVE GMT-ADGMT-LAND TO RESP-ADGMT-LAND                             
070301     END-IF                                                               
070401                                                                          
070501     MOVE 'END S04-GET-CUSTOMER-INFO    ' TO PGM-POS                      
070601     .                                                                    
070701     EJECT                                                                
070801 S05-BEHANDLA-HAENDELSEBASER SECTION.                                     
070901     MOVE 'STA S05-BEHANDLA-HAENDELSEBAS' TO PGM-POS                      
071001                                                                          
071101     MOVE WS-KDFRAKT         TO W-473B-KDFRAKT                            
071201                                                                          
071301     PERFORM IMS-GU-473B-4732-KVAL                                        
071401                                                                          
071501     IF SEGMENT-FINNS                                                     
071601       PERFORM S06-DATA-FRAAN-RDG-4732                                    
071701     END-IF                                                               
071801     MOVE 'END S05-BEHANDLA-HAENDELSEBAS' TO PGM-POS                      
071901     .                                                                    
072001     EJECT                                                                
072101 S06-DATA-FRAAN-RDG-4732 SECTION.                                         
072201     MOVE 'STA S05-BEHANDLA-HAENDELSEBAS' TO PGM-POS                      
072301                                                                          
072401     IF FRAKT-BEFRAKT (1) = SPACE                                         
072501       MOVE FRAKT-BEFRAKT (1) TO RESP-BEFRAKT                             
072601     ELSE                                                                 
072701       MOVE FRAKT-BEFRAKT (2) TO RESP-BEFRAKT                             
072801     END-IF                                                               
072901                                                                          
073001     MOVE 'END S05-BEHANDLA-HAENDELSEBAS' TO PGM-POS                      
073101     .                                                                    
073201     EJECT                                                                
073301 S07-GET-WDQ301-INFO   SECTION.                                           
073401     MOVE 'STA S07-GET-WDQ301-INFO      ' TO PGM-POS                      
073501                                                                          
073601     MOVE WS-IDORDER      TO W-Q301-IDORDER                               
073701     MOVE REQU-IDDC-KEY   TO W-Q301-IDDC                                  
073801     MOVE WS-IDPRODNR     TO W-Q301-IDPRODNR                              
073901     MOVE WS-IDPLKLST     TO W-Q301-IDPLKLST                              
074001                                                                          
074101     PERFORM IMS-GU-WDQ301-ORQA                                           
074201                                                                          
074301     IF SEGMENT-FINNS                                                     
074401       MOVE ODEL-IDPRCPLK TO WS-IDPRC                                     
074501     END-IF                                                               
074601     MOVE WS-VAGNNR       TO RESP-VAGNNR                                  
074701     MOVE 'END S07-GET-WDQ301-INFO      ' TO PGM-POS                      
074801     .                                                                    
074901     SKIP2                                                                
075001 S08-HAMTA-Q201-OHUV-DATA    SECTION.                                     
075101     MOVE 'STA S08-HAMTA-Q201-OHUV-DATA' TO PGM-POS                       
075201                                                                          
075301     MOVE WS-IDDISTR         TO DIST19-IDDISTR                            
075401                                TEST-IDDISTR                              
075501     MOVE OHUV-IDKUNDNR      TO TEST-IDKUNDNR                             
075601     MOVE OHUV-IDUSER        TO RESP-IDUSER                               
075701     MOVE OHUV-BELAGINS-DEL1 TO RESP-BELAGINS-DEL1                        
075801     MOVE OHUV-BELAGINS-DEL2 TO RESP-BELAGINS-DEL2                        
075901     MOVE OHUV-BEVARREF      TO RESP-BEVARREF                             
076001                                                                          
076101     PERFORM IMS-GU-GMTA-WDB201                                           
077000                                                                          
077700*    -- FIX BAD DATA IN GMT-OVR FIELDS                                    
077800     IF GMT-BEGMT-OVR-RAD1 = LOW-VALUE                                    
077900        MOVE SPACE TO GMT-BEGMT-OVR-RAD1                                  
078000     END-IF                                                               
078100     IF GMT-BEGMT-OVR-RAD2 = LOW-VALUE                                    
078200        MOVE SPACE TO GMT-BEGMT-OVR-RAD2                                  
078300     END-IF                                                               
078400     IF GMT-ADGMT-OVR-GATA = LOW-VALUE                                    
078500        MOVE SPACE TO GMT-ADGMT-OVR-GATA                                  
078600     END-IF                                                               
078700     IF GMT-ADGMT-OVR-PADR = LOW-VALUE                                    
078800        MOVE SPACE TO GMT-ADGMT-OVR-PADR                                  
078900     END-IF                                                               
079000     IF (NDC-CN OR LDC-CN)                                                
079100     AND (                                                                
079200*        -- IF OVR FIELDS CONTAIN ANY SIGNIFICANT VALUES --               
079300           GMT-BEGMT-OVR-RAD1 NOT = SPACE                                 
079400        OR GMT-BEGMT-OVR-RAD2 NOT = SPACE                                 
079500        OR GMT-ADGMT-OVR-GATA NOT = SPACE                                 
079600        OR GMT-ADGMT-OVR-PADR NOT = SPACE                                 
079700      )                                                                   
079800*      -- ADDRESS IN DOUBLE-BYTE CHINESE CODE FETCHED FROM                
079900*      -- CUSTOMER DATABASE "OVR" FIELDS INSTEAD OF ORDER HEAD            
080000*      -- (EXCEPT BERADREF/LAND)                                          
080100       MOVE WS-CP-CHN-EBCDIC TO TRAUTF8-KDCP                              
080200                                                                          
080300       MOVE GMT-BEGMT-OVR-RAD1    TO RESP-BEGMT-RAD1                      
080400       MOVE GMT-BEGMT-OVR-RAD2    TO RESP-BEGMT-RAD2                      
080500       MOVE GMT-ADGMT-OVR-GATA    TO RESP-ADGMT-GATA                      
080600       MOVE GMT-ADGMT-OVR-PADR    TO RESP-ADGMT-PADR                      
080700       MOVE GMT-ADGMT-OVR-LAND    TO RESP-ADGMT-LAND                      
080800                                                                          
080900     ELSE                                                                 
081000*      -- ADDRESS IN NORMAL (SWEDISH) EBCDIC CODE                         
081100       MOVE WS-CP-SVE-EBCDIC TO TRAUTF8-KDCP                              
081200                                                                          
081300       IF  OHUV-BEGMT = SPACE                                             
081400       AND OHUV-ADGMT = SPACE                                             
081500           PERFORM S04-GET-CUSTOMER-INFO                                  
081600       ELSE                                                               
081700           MOVE OHUV-BEGMT-RAD1 TO RESP-BEGMT-RAD1                        
081800           MOVE OHUV-BEGMT-RAD2 TO RESP-BEGMT-RAD2                        
081900           MOVE OHUV-ADGMT-GATA TO RESP-ADGMT-GATA                        
082000           MOVE OHUV-ADGMT-PADR TO RESP-ADGMT-PADR                        
082100           MOVE OHUV-ADGMT-LAND TO RESP-ADGMT-LAND                        
082200       END-IF                                                             
082300     END-IF                                                               
082500                                                                          
082600     MOVE 35 TO TRAUTF8-KVMAXTL                                           
082700     IF  (RESP-BEGMT-RAD1 = SPACE OR LOW-VALUE)                           
082800     AND (RESP-BEGMT-RAD2 = SPACE OR LOW-VALUE)                           
082900*      -- SHIFT UP TWO LINES                                              
083000       MOVE RESP-ADGMT-GATA     TO TRAUTF8-TECONV-FROM                    
083100       CALL WTRAUTF8    USING TRAUTF8-AREA                                
083200       MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                        
083300                                                                          
083400       MOVE RESP-ADGMT-PADR     TO TRAUTF8-TECONV-FROM                    
083500       CALL WTRAUTF8    USING TRAUTF8-AREA                                
083600       MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                        
083700                                                                          
083800       MOVE RESP-ADGMT-LAND     TO TRAUTF8-TECONV-FROM                    
083900       CALL WTRAUTF8    USING TRAUTF8-AREA                                
084000       MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-GATA                        
084100                                                                          
084200       MOVE ALL   X'20'         TO RESP-ADGMT-PADR                        
084300                                   RESP-ADGMT-LAND                        
084400     ELSE                                                                 
084500       MOVE RESP-BEGMT-RAD1     TO TRAUTF8-TECONV-FROM                    
084600       CALL WTRAUTF8    USING TRAUTF8-AREA                                
084700       MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                        
084800                                                                          
084900       MOVE RESP-BEGMT-RAD2     TO TRAUTF8-TECONV-FROM                    
085000       CALL WTRAUTF8    USING TRAUTF8-AREA                                
085100       MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                        
085200                                                                          
085300       MOVE RESP-ADGMT-GATA     TO TRAUTF8-TECONV-FROM                    
085400       CALL WTRAUTF8    USING TRAUTF8-AREA                                
085500       MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-GATA                        
085600                                                                          
085700       MOVE RESP-ADGMT-PADR     TO TRAUTF8-TECONV-FROM                    
085800       CALL WTRAUTF8    USING TRAUTF8-AREA                                
085900       MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-PADR                        
086000                                                                          
086100       MOVE RESP-ADGMT-LAND     TO TRAUTF8-TECONV-FROM                    
086200       CALL WTRAUTF8    USING TRAUTF8-AREA                                
086300       MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-LAND                        
086400                                                                          
086500     END-IF                                                               
086600                                                                          
086700     MOVE 'END S08-HAMTA-Q201-OHUV-DATA' TO PGM-POS                       
086800     .                                                                    
086900     EJECT                                                                
087000*    SKIP2                                                                
087100 S09-VISA-SATSORDER-INFO    SECTION.                                      
087200     MOVE 'STA S09-VISA-SATSORDER-INFO  '  TO PGM-POS                     
087300                                                                          
087400     MOVE KORD-IDARTNR-SATS         TO WS-KORD-IDARTNR-SATS               
087500     MOVE WS-ADGMT-GATA-SATS        TO RESP-ADGMT-GATA                    
087600                                                                          
087700     MOVE KORD-KVBEART-SATS         TO WS-KORD-KVBEART-SATS               
087800     MOVE KORD-KVORDRAD             TO WS-KORD-KVORDRAD                   
087900     MOVE WS-ANT-RAD-ART-SATS       TO RESP-ADGMT-PADR                    
088000     MOVE 'END S09-VISA-SATSORDER-INFO  '  TO PGM-POS                     
088100     .                                                                    
088200     EJECT                                                                
088300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
088400     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
088500                                                                          
088600     MOVE 'GETARG'               TO SUB-KDFUNC                            
088700     MOVE 'CARPARTS.LDC.PACKINGSPECHEADER'   TO SUB-ADDISPABS             
088800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
088900                                                                          
089000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
089100                                                                          
089200     IF SUB-KDRC > 0                                                      
089300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
089400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
089500       DELIMITED BY SIZE INTO FELTEXT                                     
089600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
089700     END-IF                                                               
089800     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
089900     .                                                                    
090000     SKIP3                                                                
090100 S02-RETURN-RESPONSE SECTION.                                             
090200     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
090300                                                                          
090400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
090500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
090600                                                                          
090700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
090800                                                                          
090900     IF SUB-KDRC > 0                                                      
091000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
091100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
091200       DELIMITED BY SIZE INTO FELTEXT                                     
091300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
091400     END-IF                                                               
091500     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
091600     .                                                                    
091700     EJECT                                                                
091800* IMS SEKTIONER                                                           
091900     SKIP3                                                                
092000 IMS-GU-WDE601-KVAL SECTION.                                              
092100     MOVE 'IMS-GU-WDE601-KVAL       '  TO PGM-POS                         
092200                                                                          
092300     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
092400            DELIMITED BY SIZE INTO SSA1                                   
092500     MOVE '  GE' TO GODK-STATUSKODER                                      
092600     CALL CBLTDLI USING GU                                                
092700                          WDE62-PCB                                       
092800                          DLI-IO-AREA                                     
092900                          SSA1                                            
093000     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
093100     PERFORM IMS-STATUSKONTROLL                                           
093200     SKIP3                                                                
093300     .                                                                    
093400 IMS-GU-WDE401-ESEQ SECTION.                                              
093500     MOVE 'IMS-GU-WDE401-ESEQ       '  TO PGM-POS                         
093600                                                                          
093700     STRING 'WDE401  (WDE4ESEQ =' W-WDE4E1KY-X ')'                        
093800            DELIMITED BY SIZE INTO SSA1                                   
093900     MOVE '  ' TO GODK-STATUSKODER                                        
094000     CALL CBLTDLI USING GU                                                
094100                          WDE4E-PCB                                       
094200                          DLI-IO-WDE401                                   
094300                          SSA1                                            
094400     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
094500     PERFORM IMS-STATUSKONTROLL                                           
094600     SKIP3                                                                
094700     .                                                                    
094800 IMS-GU-BETC-WDB101      SECTION.                                         
094900     MOVE 'IMS-GU-BETC-WDB101       '  TO PGM-POS                         
095000                                                                          
095101     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
095200            DELIMITED BY SIZE INTO SSA1                                   
095300     MOVE '  GE' TO GODK-STATUSKODER                                      
095400     CALL CBLTDLI USING GU BETC-PCB DLI-IO-AREA SSA1                      
095500     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
095600     PERFORM IMS-STATUSKONTROLL                                           
095700     SKIP3                                                                
095800     .                                                                    
095900 IMS-GU-GMTA-WDB201      SECTION.                                         
096000     MOVE 'IMS-GU-GMTA-WDB201       '  TO PGM-POS                         
096100                                                                          
096200     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
096300            DELIMITED BY SIZE INTO SSA1                                   
096400     MOVE '  GE' TO GODK-STATUSKODER                                      
096500     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-WDB2 SSA1                 
096600     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     SKIP3                                                                
096900     .                                                                    
097000 IMS-GU-473B-4732-KVAL SECTION.                                           
097100     MOVE 'IMS-GU-473B-4732-KVAL    '  TO PGM-POS                         
097200                                                                          
097300     STRING 'WL473201(WDGXKEY  =' W-473B-WDGXKEY-X ')'                    
097400            DELIMITED BY SIZE INTO SSA1                                   
097500     STRING 'WL473211(KDSEGKEY =' W-4732-KDSEGKEY-X ')'                   
097600            DELIMITED BY SIZE INTO SSA2                                   
097700     MOVE '  GE' TO GODK-STATUSKODER                                      
097800     CALL CBLTDLI USING GU                                                
097900                          4732-PCB                                        
098000                          DLI-IO-AREA                                     
098100                          SSA1                                            
098200                          SSA2                                            
098300     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
098400     PERFORM IMS-STATUSKONTROLL                                           
098500     .                                                                    
098600     EJECT                                                                
098700 IMS-GU-WDE401-KVAL SECTION.                                              
098800     MOVE 'IMS-GU-WDE401-KVAL       '  TO PGM-POS                         
098900                                                                          
099000     STRING 'WDE401  (WDE401KY =' W-E401-WDE4KEY-X ')'                    
099100            DELIMITED BY SIZE INTO SSA1                                   
099200     MOVE '    ' TO GODK-STATUSKODER                                      
099300     CALL CBLTDLI USING GU                                                
099400                          WDE4-PCB                                        
099500                          DLI-IO-AREA                                     
099600                          SSA1                                            
099700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
099800     PERFORM IMS-STATUSKONTROLL                                           
099900     SKIP3                                                                
100000     .                                                                    
100100 IMS-GU-WDE601-OKVAL SECTION.                                             
100200     MOVE 'IMS-GU-WDE601-OKVAL      '  TO PGM-POS                         
100300                                                                          
100400     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
100500            DELIMITED BY SIZE INTO SSA1                                   
100600     MOVE '  GE' TO GODK-STATUSKODER                                      
100700     CALL CBLTDLI USING GU                                                
100800                          WDE6-PCB                                        
100900                          DLI-IO-AREA                                     
101000                          SSA1                                            
101100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     EJECT                                                                
101500 IMS-GU-WDE401-SEK  SECTION.                                              
101600     MOVE 'IMS-GU-WDE401-SEK        '  TO PGM-POS                         
101700                                                                          
101800     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
101900            DELIMITED BY SIZE INTO SSA1                                   
102000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
102100     CALL CBLTDLI USING GU                                                
102200                          WDE42-PCB                                       
102300                          DLI-IO-AREA                                     
102400                          SSA1                                            
102500     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
102600                               STATUS-WDE401-SEK-WS                       
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     SKIP3                                                                
102900     .                                                                    
103000 IMS-GN-WDE401-SEK  SECTION.                                              
103100     MOVE 'IMS-GN-WDE401-SEK        '  TO PGM-POS                         
103200                                                                          
103300     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
103400            DELIMITED BY SIZE INTO SSA1                                   
103500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
103600     CALL CBLTDLI USING GN                                                
103700                          WDE42-PCB                                       
103800                          DLI-IO-AREA                                     
103900                          SSA1                                            
104000     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
104100                               STATUS-WDE401-SEK-WS                       
104200     PERFORM IMS-STATUSKONTROLL                                           
104300     .                                                                    
104400     EJECT                                                                
104500 IMS-GET-ORQI01-CSEQ SECTION.                                             
104600     MOVE 'IMS-GET-ORQI01-CSEQ      '  TO PGM-POS                         
104700                                                                          
104800     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
104900             DELIMITED BY SIZE INTO    SSA1                               
105000     MOVE    '  GE'              TO    GODK-STATUSKODER                   
105100     CALL    CBLTDLI             USING GU                                 
105200                                         ORQICSQ-PCB                      
105300                                         DLI-IO-AREA                      
105400                                         SSA1                             
105500     MOVE    ORQICSQ-STATUS-CODE TO    STATUS-WS                          
105600     PERFORM IMS-STATUSKONTROLL                                           
105700     .                                                                    
105800     EJECT                                                                
105900 IMS-GU-WDQ301-ORQA     SECTION.                                          
106000     MOVE 'IMS-GU-WDQ301-ORQA       '  TO PGM-POS                         
106100                                                                          
106200     STRING  'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                       
106300             DELIMITED BY SIZE INTO    SSA1                               
106400     MOVE    '  GE'              TO    GODK-STATUSKODER                   
106500     CALL CBLTDLI USING GU ORQA-PCB ODEL-WDQ301 SSA1                      
106600     MOVE    ORQA-STATUS-CODE TO STATUS-WS                                
106700     PERFORM IMS-STATUSKONTROLL                                           
106800     .                                                                    
106900     EJECT                                                                
107000 IMS-GU-WDB601    SECTION.                                                
107100     MOVE 'IMS-GU-WDB601            '  TO PGM-POS                         
107200                                                                          
107300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
107400          DELIMITED BY SIZE INTO SSA1                                     
107500     MOVE '  GE' TO GODK-STATUSKODER                                      
107600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
107700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     .                                                                    
108000     EJECT                                                                
108100                                                                          
108200 IMS-STATUSKONTROLL SECTION.                                              
108300     SET STATUS-IX TO 1                                                   
108400     SEARCH GODK-STATUS AT END CALL FELLOG                                
108500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
108600     END-SEARCH                                                           
108700     CONTINUE                                                             
108800     .                                                                    
