000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL019900.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   08/03/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.CASEREPORTING2                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PACKREPORTING OF ORDERPARTS WITH DEVIATIONS.                     
001100*        THIS PROGRAM IS A MODIFIED REPLICA OF W4031500                   
001200*        WITH POSSIBILITY TO PACKREPORT MORE THAN ONE CASE.               
001300*                                                                         
001400*        THE PROGRAM UPDATES   WDE4                                       
001500*        THE PROGRAM UPDATES   WDE6                                       
001600*        THE PROGRAM UPDATES   WDQ2                                       
001700*        THE PROGRAM READS     WDQ3                                       
001800*        THE PROGRAM UPDATES   WDG2 (WDGX4321/4322)                       
001900*        THE PROGRAM UPDATES   WDG7 (WDGX4726/4727)                       
002000*        THE PROGRAM UPDATES   WDR1 (WDGX4477/4478)                       
002100*        THE PROGRAM UPDATES   WDR4 (WDGX4471/4472)                       
002200*                                                                         
002300*        KDPGMACT = 'E' EXECUTE                                           
002400*        KDPGMACT = 'R' RESTART                                           
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: WL0199U                                             
002800*        REQUEST:     WL0199I1                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        RESPONSE:    WL0199O1                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500 77  IDPGM                       PIC X(08)   VALUE 'WL019900'.            
004600 77  JA                          PIC X(01)   VALUE 'J'.                   
004700 77  NEJ                         PIC X(01)   VALUE 'N'.                   
004800 77  RAETT                       PIC X       VALUE 'R'.                   
004900 77  DATUM-SW                    PIC X(01)   VALUE 'N'.                   
005000 77  DEF-IDKOLLI-SW              PIC X(01)   VALUE 'N'.                   
005100                                                                          
005200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005300 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
005400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005500 77  FILLER                      PIC X(08)   VALUE 'CURRSECT'.            
005600 77  WS-CURRENT-SECTION          PIC X(40)   VALUE SPACE.                 
005700 77  FILLER                      PIC X(08)   VALUE 'IMS-SECT'.            
005800 77  WS-CURRENT-IMS-SECTION      PIC X(40)   VALUE SPACE.                 
005900 77  KDRC-DISPLAY                PIC Z(5).                                
006000 77  WS-TIORDREG-NUM6            PIC 9(06)   VALUE ZERO.                  
006100 77  WS-EMB-VKTARA-ONE-CASE      PIC 9(06)   VALUE ZERO.                  
006200 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
006300 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
006400 77  WS-KDARTURS                 PIC X(2)    VALUE SPACE.                 
006500 77  WS-KDPRTVAL-FS              PIC X(2)    VALUE SPACE.                 
006600 77  WS-KDPRTVAL-ADR             PIC X(2)    VALUE SPACE.                 
006610 77  WS-IDARTNR-Z                PIC Z(9).                                
006700                                                                          
006800                                                                          
006900* --- 4472 INDEX                                                          
007000 77  4472-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
007100 77  IDSHIFT-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
007200                                                                          
007300* --- LINE INDEX                                                          
007400 77  FILLER                      PIC X(08)   VALUE 'LINE-IX'.             
007500 77  LINE-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
007600                                                                          
007700* --- CASE INDEX                                                          
007800 77  FILLER                      PIC X(08)   VALUE 'CASE-IX:'.            
007900 77  CASE-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
008000 77  MAX-CASE-IX                 PIC S9(9)   VALUE +50  COMP SYNC.        
008100                                                                          
008200* --- TABLE INDEX - CHECK IF PRESENT OR PREVIOUS CASE                     
008300* --- ONLY CASES ENTERED THIS TIME SHOULD HAVE PRINTOUTS                  
008400 77  TABLE-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
008500 77  FILLER                    PIC X(16)   VALUE 'USED-TABLE-IX'.         
008600 77  USED-TABLE-IX               PIC S9(9)   VALUE +0   COMP SYNC.        
008700 77  MAX-TABLE-IX                PIC S9(9)   VALUE +50  COMP SYNC.        
008800                                                                          
008900* --- IDRADNR INDEX                                                       
009000 77  IDRAD-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
009100 77  MAX-IDRAD-IX                PIC S9(9)   VALUE +999 COMP SYNC.        
009200 77  TOP-IDRAD-IX                PIC S9(9)   VALUE +0   COMP SYNC.        
009300                                                                          
009400* --- MAX UPDATE'S = RESTART-ME                                           
009500 77  UPDATE-IX                   PIC S9(9)   VALUE +0   COMP SYNC.        
009600 77  MAX-UPD-IX                  PIC S9(4)   VALUE +500 COMP SYNC.        
009700 77  MAX-IX                    PIC S9(9)  VALUE +100 COMP SYNC.           
009800                                                                          
009900* --- DANG. GOODS INDEX                                                   
010000 77  MAX-DG-IX                   PIC S9(9)   VALUE +10  COMP SYNC.        
010100 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
010200                                                                          
010300 77  TMS-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
010500 77  IDPSN-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
010600 77  KOLLI-TAB-IX                PIC S9(9)   VALUE +0   COMP SYNC.        
010700                                                                          
010800 77  WS-KDFRAKT-NUM              PIC 9(2)    VALUE ZERO.                  
010900 77  WS-REMAIN                   PIC 9(4)    VALUE ZERO.                  
011000 77  WS-DARFS                    PIC 9(12)   VALUE ZERO.                  
011100 77  WS-KVPTID                   PIC S9(2)V9(1) VALUE ZERO COMP-3.        
011200 77  WS-IDUSER-ALFA              PIC X(5)    VALUE SPACE.                 
011300 77  WS-IDKOLLI                  PIC S9(5)   VALUE ZERO.                  
011400 77  WS-KKOLLI-KVLEVART          PIC S9(6)   VALUE ZERO COMP-3.           
011500 77  WS-ODEL-IDORDNR5            PIC S9(5)   VALUE ZERO COMP-3.           
011600 77  WS-ORAD-BEART               PIC X(25)   VALUE SPACE.                 
011700 77  WS-DIKOLLIL                 PIC 9(4)    VALUE ZERO.                  
011800 77  WS-DIKOLLIB                 PIC 9(4)    VALUE ZERO.                  
011900 77  WS-DIKOLLIH                 PIC 9(4)    VALUE ZERO.                  
012000 77  WS-KOLLI-VLORDBTO           PIC 9(8)    VALUE ZERO.                  
012100 77  WS-ORAD-VLORDNTO            PIC 9(8)    VALUE ZERO.                  
012200 77  WS-ORAD-VLORDNTO-SUM        PIC 9(8)    VALUE ZERO.                  
012300 77  WS-IDLEVNR                  PIC X(5)   VALUE SPACES.                 
012400 77  WS-ODEL-IDDC-EXP            PIC X(2)   VALUE SPACES.                 
012500                                                                          
012600 77    FILLER                    PIC  X(08) VALUE 'DN WS   '.             
012700 77    WS-DNOT-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
012800 77    WS-DNOT-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
012900 77    WS-DNOT-IDDC              PIC  X(02) VALUE ZERO.                   
013000 77    WS-DNOT-IDKOLLI           PIC S9(05) VALUE ZERO COMP-3.            
013100 77    WS-DNOT-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
013200                                                                          
013300 01     WS-IDKUNDRF-NEW.                                                  
013400   03   WS-IDORDNR7-NEW          PIC 9(7).                                
013500   03   FILLER                   PIC X(3)   VALUE SPACE.                  
013600                                                                          
013700 77  TEST-IDKUNDNR               PIC 9(7)    VALUE ZERO.                  
013800 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
013900 01    ALL-PLUS.                                                          
014000   03 FILLER                     PIC X(30)  VALUE                         
014100        '++++++++++++++++++++++++++++++'.                                 
014200 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
014300*                                                                         
014400 01  TRANSFER-KUND               PIC 9(7).                                
014500     88 TRANSFER-KUNDNR          VALUE 0000511                            
014600                                       0000512                            
014700                                       0000513.                           
014800     88  RETUR-KUNDNR            VALUE 0000051.                           
014900*                                                                         
015000                                                                          
015100 01  WS-KDMATT                   PIC X.                                   
015200     88 US-MEASUREMENT           VALUE 'U'.                               
015300     88 SIS-MEASUREMENT          VALUE 'S'.                               
015400*                                                                         
015500 77  NOO                         PIC X       VALUE 'N'.                   
015600 77  YES                         PIC X       VALUE 'J'.                   
015700                                                                          
015800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
015900     88  KEYS-OK                             VALUE 'J'.                   
016000     88  KEYS-WRONG                          VALUE 'N'.                   
016100     EJECT                                                                
016200                                                                          
016300 77  INIT-SW                     PIC X       VALUE 'J'.                   
016400     88  INIT-OK                             VALUE 'J'.                   
016500     88  INIT-WRONG                          VALUE 'N'.                   
016600     EJECT                                                                
016700                                                                          
016800 77  DATA-SW                     PIC X       VALUE 'J'.                   
016900     88  DATA-OK                             VALUE 'J'.                   
017000     88  DATA-WRONG                          VALUE 'N'.                   
017100                                                                          
017200 77  IDPSN-INS-SW                PIC X       VALUE 'N'.                   
017300     88 IDPSN-INS                            VALUE 'J'.                   
017400                                                                          
017500 77  RESTART-ME-SW               PIC  X      VALUE 'N'.                   
017600     88 RESTART-ME                           VALUE 'J'.                   
017700                                                                          
017800 77  ERR-USESCREEN-L0121-SW      PIC X       VALUE 'J'.                   
017900     88 ERR-USESCREEN-L0121                  VALUE 'N'.                   
018000                                                                          
018100 77    DIRLEV-KOLLI-SW           PIC X(01).                               
018200   88  DIRLEV-KOLLI                         VALUE 'J'.                    
018300                                                                          
018400 77    SW-TIKLAR-UPPDATERAD      PIC X(01).                               
018500*                                                                         
018600 01  WS-ODEL-DARFS               PIC 9(12).                               
018700 01  FILLER        REDEFINES WS-ODEL-DARFS.                               
018800     03 FILLER                   PIC  9(2).                               
018900     03 WS-ODEL-DARFS-6          PIC  9(6).                               
019000     03 FILLER                   PIC  9(4).                               
019100     SKIP2                                                                
019200 01  WS-4472-TIRFS               PIC 9(11).                               
019300 01  FILLER        REDEFINES WS-4472-TIRFS.                               
019400     03 FILLER                   PIC  9(1).                               
019500     03 WS-4472-TIRFS-6          PIC  9(6).                               
019600     03 FILLER                   PIC  9(4).                               
019700 01  WS-4472-PACK-LINES          PIC S9(3)   VALUE +0   COMP-3.           
019800 01  WS-TOT-LINES                PIC S9(3)   VALUE +0   COMP-3.           
019900 01  WS-KVPTID-TIM               PIC S9(7)   VALUE +0   COMP-3.           
020000 01  WS-KVPTID-MIN               PIC S9(7)   VALUE +0   COMP-3.           
020100 01  WS-SUPTID-PRAPP             PIC 9(3)V99.                             
020200 01  FILLER REDEFINES WS-SUPTID-PRAPP.                                    
020300     03  WS-SUPTID-TIM           PIC 9(3).                                
020400     03  WS-SUPTID-MIN           PIC 9(2).                                
020500     SKIP3                                                                
020600                                                                          
020700 01  WS-IDTRPTNR               PIC S9(3)   VALUE ZERO.                    
020800 01  WS-ADFLGEO                PIC X(3)    VALUE SPACE.                   
020900 01  WS-ADFLOMR                PIC S9(3)   VALUE ZERO.                    
021000 01  WS-ADRUTNIV               PIC S9(3)   VALUE ZERO.                    
021100 01  WS-DIHMODUL               PIC S9(3)   VALUE ZERO.                    
021200 01  WS-DIDMODUL               PIC S9(3)   VALUE ZERO.                    
021300 01  WS-ADVMODUL               PIC S9(3)   VALUE ZERO.                    
021400 01  WS-ADHMODUL               PIC S9(3)   VALUE ZERO.                    
021500 01  WS-FLUTLAST               PIC X       VALUE SPACE.                   
021600 01  WS-IDDC-CROSS             PIC X(2)    VALUE SPACE.                   
021700     EJECT                                                                
021800 01  FILLER                      PIC X(16)   VALUE 'TAB-PLATS'.           
021900 01  TAB-PLATS.                                                           
022000     03  TAB-INGANG  OCCURS 100.                                          
022100         05  WS-KVLEVART-REST    PIC S9(6)   VALUE ZERO COMP-3.           
022200         05  TAB-DIKOLLIL        PIC 9(4)    VALUE ZERO.                  
022300         05  TAB-DIKOLLIB        PIC 9(3)    VALUE ZERO.                  
022400         05  TAB-DIKOLLIH        PIC 9(3)    VALUE ZERO.                  
022500                                                                          
022600 01  SAVE-IDRADNR-TAB.                                                    
022700     03  TAB-RAD-AREA OCCURS 999.                                         
022800         05  TAB-IDRADNR  PIC 9(5)   VALUE ZERO.                          
022900         05  TAB-KVLEVART PIC 9(6)   VALUE ZERO.                          
023000         05  TAB-KDRADSTA PIC 9      VALUE ZERO.                          
023100         05  TAB-KDKOLSTA PIC 9      VALUE ZERO.                          
023200                                                                          
023300 01  THIS-KVLEVART           PIC S9(6)   VALUE ZERO COMP-3.               
023400 01  CHECK-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.               
023500 01  CHECK-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.               
023600 01  SAVE-WORK-IDRADNR-FOM   PIC S9(5)   VALUE ZERO COMP-3.               
023700 01  SAVE-WORK-IDRADNR-TOM   PIC S9(5)   VALUE ZERO COMP-3.               
023800 01  WS-ORAD-KVLEVART        PIC S9(6)   VALUE ZERO COMP-3.               
023810 01  WS-WHEV-KVLEVART        PIC S9(6)   VALUE ZERO COMP-3.               
023900 01  WS-IDPSN-TAB.                                                        
024000     03 WS-VKART-FG          PIC S9(7)   VALUE 0.                         
024100     03 WS-VLFG              PIC S9(4)V9(3) VALUE ZERO COMP-3.            
024200     03 WS-SUEQFG            PIC S9(3)V9(4) VALUE ZERO COMP-3.            
024300                                                                          
024310 01 WS-DATE.                                                              
024320    03 WS-YYYY PIC X(4).                                                  
024330    03 WS-HYPHEN PIC X(1) VALUE '-'.                                      
024340    03 WS-MM   PIC X(2).                                                  
024350    03 WS-HYPHEN PIC X(1) VALUE '-'.                                      
024360    03 WS-DD   PIC X(2).                                                  
024370 01 WS-TIME.                                                              
024380    05 WS-HOUR PIC X(2).                                                  
024390    05 WS-COLON PIC X(1) VALUE ':'.                                       
024391    05 WS-MIN  PIC X(2).                                                  
024400                                                                          
024500 01  SAVE-IDKOLLI-TAB.                                                    
024600     03  TAB-IDKOLLI  OCCURS 50  PIC 9(5)    VALUE ZERO.                  
024700                                                                          
024800 01  SAVE-AREAS.                                                          
024900     03  SAVE-IDKOLLI            PIC 9(5)    VALUE ZERO.                  
025000*    03  SAVE-KDKOLLI            PIC X(8)    VALUE SPACE.                 
025100                                                                          
025200 01  FILLER                      PIC X(11)   VALUE 'WORKAREAS'.           
025300 01  WORKAREAS.                                                           
025400     03  WORK-KOLLI-AREA.                                                 
025500         05  WORK-KOLLI-VLORDBTO       PIC S9(4)V9(3) VALUE ZERO.         
025600         05  WORK-KOLLI-VKORDBTO       PIC S9(6)V9(3) VALUE ZERO.         
025700         05  WORK-KOLLI-VKORDNTO       PIC S9(6)V9(3) VALUE ZERO.         
025800         05  WORK-KOLLI-KVFLAMP        PIC S9(2)V9(1) VALUE ZERO.         
025900         05  WORK-KOLLI-KDFARLIG       PIC S9         VALUE ZERO.         
026000         05  WORK-KOLLI-KVORDRAD       PIC S9(5)      VALUE ZERO.         
026100         05  WORK-KOLLI-KVFALRAD       PIC S9(5)      VALUE ZERO.         
026200         05  WORK-KOLLI-SUORDV         PIC S9(9)V9(2) VALUE ZERO.         
026300         05  WORK-KOLLI-SUORDV-EXP     PIC S9(9)V9(2) VALUE ZERO.         
026400         05  WORK-KOLLI-SUORDV-LOC     PIC S9(9)V9(2) VALUE ZERO.         
026500         05  WORK-KOLLI-SUORDV-LOCPREL PIC S9(9)V9(2) VALUE ZERO.         
026600         05  WORK-KOLLI-KVLEVART       PIC S9(6)      VALUE ZERO.         
026700     SKIP2                                                                
026800     03  WORK-MISC-AREA.                                                  
026900         05 WORK-NO-OF-CASES           PIC 9(5)       VALUE ZERO.         
027000         05 WORK-NO-OF-LINES           PIC 9(5)       VALUE ZERO.         
027100         05 WORK-EMB-TARE-ONE-CASE     PIC S9(6)V9    VALUE ZERO.         
027200         05 WORK-EMB-VOL-ONE-CASE      PIC S9(4)V9(3) VALUE ZERO.         
027300         05 OLD-EMB-VOL-ONE-CASE       PIC S9(4)V9(3) VALUE ZERO.         
027400                                                                          
027800     EJECT                                                                
027900                                                                          
028000 01  CALC-AREAS.                                                          
028100     03  CALC-VORD-KVKOLLI       PIC S9(5)      VALUE ZERO.               
028200     03  CALC-VORD-KVKOLPAC      PIC S9(5)      VALUE ZERO.               
028300     03  CALC-VORD-KVORDRAD-PACK PIC S9(5)      VALUE ZERO.               
028400     03  CALC-VORD-VLORDBTO      PIC S9(4)V9(3) VALUE ZERO.               
028500     03  CALC-VORD-VKORDBTO      PIC S9(6)V9(3) VALUE ZERO.               
028600     03  CALC-VORD-VKORDNTO      PIC S9(6)V9(3) VALUE ZERO.               
028700     03  CALC-VORD-SUORDV        PIC S9(9)V9(2) VALUE ZERO.               
028800     03  CALC-VORD-SUORDV-EXP    PIC S9(9)V9(2) VALUE ZERO.               
028900     03  CALC-VORD-SUORDV-LOC    PIC S9(9)V9(2) VALUE ZERO.               
029000     03  CALC-VORD-SUORDV-LOCPREL PIC S9(9)V9(2) VALUE ZERO.              
029100                                                                          
029200 01  WS-VKTARA                   PIC S9(6)V9(1) VALUE ZERO.               
029300                                                                          
029400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
029500 01  FILLER REDEFINES DAGENS-DATUM.                                       
029600     03  DAGENS-AA               PIC 9(2).                                
029700     03  DAGENS-MM               PIC 9(2).                                
029800     03  DAGENS-DD               PIC 9(2).                                
029900                                                                          
030000 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
030100 01  FILLER REDEFINES DAGENS-TID.                                         
030200     03  DAGENS-TID-HHMMSS       PIC 9(6).                                
030300     03  FILLER                  PIC 9(2).                                
030400                                                                          
030500 01  WS-IDPRTLST.                                                         
030600     03 WS-SYSTDEL               PIC X(1).                                
030700     03 WS-LISTTYP               PIC X(2).                                
030800     03 WS-DC                    PIC X(2).                                
030900     03 WS-KDPRT                 PIC X(3).                                
031000                                                                          
031100     EJECT                                                                
031200 01  TEST-IDDISTR                PIC 9(5) COMP-3.                         
031300 01  FILLER REDEFINES TEST-IDDISTR.                                       
031400*    03   -COPY WWDIST03.                                                 
031500     SKIP2                                                                
031600 01  FILLER REDEFINES TEST-IDDISTR.                                       
031700*    03   -COPY WWDIST07.                                                 
031800     SKIP2                                                                
031900 01  FILLER REDEFINES TEST-IDDISTR.                                       
032000*    03   -COPY WWDIST08.                                                 
032100     SKIP2                                                                
032200 01  FILLER REDEFINES TEST-IDDISTR.                                       
032300*    03   -COPY WWDIST19.                                                 
032400     SKIP2                                                                
032500 01  FILLER REDEFINES TEST-IDDISTR.                                       
032600*    03   -COPY WWDIST21.                                                 
032700     SKIP2                                                                
032800 01  FILLER REDEFINES TEST-IDDISTR.                                       
032900*    03   -COPY WWDIST79.                                                 
033000     SKIP2                                                                
033100 01  FILLER REDEFINES TEST-IDDISTR.                                       
033200*    03   -COPY WWDIST85.                                                 
033300     SKIP2                                                                
033400 01  FILLER REDEFINES TEST-IDDISTR.                                       
033500*    03   -COPY WWDIST44.                                                 
033600     SKIP2                                                                
033700 01  FILLER REDEFINES TEST-IDDISTR.                                       
033800*    03   -COPY WWDIS128                                                  
033900     SKIP2                                                                
034000 01  FILLER REDEFINES TEST-IDDISTR.                                       
034100*    03   -COPY WWDIST35                                                  
034200     SKIP2                                                                
034300 01  FILLER REDEFINES TEST-IDDISTR.                                       
034400*    03   -COPY WWDIST18                                                  
034500     SKIP2                                                                
034600 01  TEST-KDFRAKT               PIC 9(2) COMP-3.                          
034700 01  FILLER REDEFINES TEST-KDFRAKT.                                       
034800*    03   -COPY WWFRAKT1                                                  
034900     SKIP2                                                                
035000                                                                          
035100*01    -COPY WWDC99                                                       
035200*01    -COPY WWDC04                                                       
035300       EJECT                                                              
035400                                                                          
035500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
035600 01  GENERAL-SUBPROGRAMS.                                                 
035700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
035800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
035900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
036000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
036100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
036200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
036300     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
036400     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
036500     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
036600     03  W403TMS1                PIC X(8)    VALUE 'W403TMS1'.            
036700     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
036800     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
036900     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
037000     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
037010     03  W403WHEV                PIC X(8)    VALUE 'W403WHEV'.            
037100*        DATA TILL DEL NOTE NDC                                           
037200     SKIP3                                                                
037300     EJECT                                                                
037400 01  FILLER                      PIC X(08)  VALUE 'W006PRT '.             
037500*01 -COPY W006PRT                                                         
037600     EJECT                                                                
037700 01  FILLER                      PIC X(08)  VALUE 'W411DNOT'.             
037800*01 -COPY W411DNOT                                                        
037900     EJECT                                                                
038000 01  FILLER                      PIC X(16)   VALUE 'WWOMVAND '.           
038100*   -COPY WWOMVAND                                                        
038200 01  FILLER                      PIC X(8)   VALUE  'WZ01AUTH'.            
038300*    -COPY WZ01AUTH                                                       
038400     EJECT                                                                
038500*                                                                         
038600 01  FILLER                      PIC X(8)   VALUE  'WMSGCONV'.            
038700*    -COPY WMSGCONV                                                       
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16) VALUE 'W400ARTU-AREA'.         
039000*01    FILLER  -COPY W400ARTU                                             
039100     EJECT                                                                
039110 01  FILLER                      PIC X(16)   VALUE 'W403WHEV'.            
039120*01 -COPY W403WHEV                                                        
039130     EJECT                                                                
039200*                                                                         
039300*                                                                         
039400*    --- PARAMETERS TO ABEND                                              
039500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
039600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
039700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
039800     SKIP3                                                                
039900*        WORK AREAS TO IMS-SECTIONS                                       
040000*                                                                         
040100 01  IMS-WS.                                                              
040200     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
040300     SKIP3                                                                
040400     03  STATUS-WS-E611          PIC XX.                                  
040500         88  SEGMENT-FOUND-E611              VALUE '  '.                  
040600     SKIP3                                                                
040700     03  STATUS-WS               PIC XX.                                  
040800         88  SEGMENT-FOUND                   VALUE '  '.                  
040900         88  SEGMENT-MISSING                 VALUE 'GE'.                  
041000         88  SEGMENT-ALREADY-EXIST           VALUE 'II'.                  
041100         88  END-OF-DATABASE                 VALUE 'GB'.                  
041200     SKIP3                                                                
041300     03    GOOD-STATUSCODES.                                              
041400         05    GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
041500     SKIP3                                                                
041600 01  SSA1                        PIC X(128).                              
041700 01  SSA2                        PIC X(128).                              
041800 01  SSA3                        PIC X(128).                              
041900     EJECT                                                                
042000*    IMS FUNKTIONSKODER                                                   
042100*01    -COPY W0003                                                        
042200     EJECT                                                                
042300 01    FILLER                 PIC X(16) VALUE 'MID W4I33301 MID'.         
042400 01  4333-MID-IO-AREA.                                                    
042500                                                                          
042600       03  4333-MID-KVLL         PIC S9(4)   COMP SYNC.                   
042700       03  FILLER                PIC X(2)    VALUE LOW-VALUE.             
042800       03  FILLER                PIC X(8)    VALUE 'W4T333  '.            
042900       03  4333-IDTRANS          PIC X(4)    VALUE 'L199'.                
043000       03  4333-MID-KDMFSFOR     PIC X.                                   
043100*      03  MID -COPY W4I33301  -PRE 4333-.                                
043200     SKIP2                                                                
043300   03  FILLER                 PIC X(16) VALUE 'MID W4I34101 MID'.         
043400   01  4341-MID-IO-AREA.                                                  
043500                                                                          
043600       03  4341-MID-LL           PIC S9(4)   COMP SYNC.                   
043700       03  FILLER                PIC X(2)    VALUE LOW-VALUE.             
043800       03  FILLER                PIC X(8)    VALUE 'W4T341  '.            
043900       03  4341-IDTRANS          PIC X(4)    VALUE 'L199'.                
044000       03  4341-MID-KDMFSFOR     PIC X.                                   
044100       03  4341-MID-DATA-AREA    PIC X(61).                               
044200*      03  MID -COPY W4I34101 -RED 4341-MID-DATA-AREA -PRE 4341-.         
044300     SKIP2                                                                
044400 01    FILLER                 PIC X(16) VALUE 'MID W4I34901 MID'.         
044500     SKIP3                                                                
044600*01    MID -COPY W4I34901  -PRE 4349-.                                    
044700     SKIP2                                                                
044800                                                                          
044900*---MSG-AERA FÖR HOPP TILL 4349-UTSKRIFT DELIVERY NOTE NA                 
045000 01  FILLER                PIC X(16)  VALUE '4349-MSG-IO-AREA'.           
045100 01  4349-MSG-IO-AREA.                                                    
045200     03  4349-LL              PIC S9(4)  VALUE +748 COMP SYNC.            
045300     03  4349-Z1              PIC X.                                      
045400     03  4349-Z2              PIC X.                                      
045500     03  4349-TRANSKOD        PIC X(8)   VALUE 'W4T349X '.                
045600     03  4349-IDTRANS         PIC X(4)   VALUE 'L199'.                    
045700     03  4349-SPRAK           PIC X.                                      
045800     03  4349-FILLER          PIC X(731).                                 
045900                                                                          
046000     SKIP2                                                                
046100                                                                          
046200 01  KEYS-TO-DL1.                                                         
046300     03  W-IDPRODNR-E6-X.                                                 
046400         05  W-IDPRODNR-E6       PIC S9(7)   COMP-3 VALUE ZERO.           
046500                                                                          
046600     03  W-IDKOLLI-E6-X.                                                  
046700         05  W-IDKOLLI-E6        PIC S9(5)   COMP-3 VALUE ZERO.           
046800                                                                          
046900     03  W-KDKOLSTA-X.                                                    
047000         05  W-KDKOLSTA          PIC S9(1)   COMP-3 VALUE +0.             
047100                                                                          
047200     03  W-WDE401KY-X.                                                    
047300         05  W-IDDISTR-E4        PIC S9(5)   COMP-3 VALUE ZERO.           
047400         05  W-IDKUNDNR-E4       PIC S9(7)   COMP-3 VALUE ZERO.           
047500         05  W-IDKUNDRF-E4.                                               
047600             07 W-IDORDNR-E4     PIC 9(5).                                
047700             07 FILLER           PIC X(5)    VALUE SPACE.                 
047800         05  W-IDPRODNR-E4       PIC S9(7)   COMP-3 VALUE ZERO.           
047900         05  W-IDPLKLST-X.                                                
048000          06  W-IDPLKLST-E4      PIC S9(3)   COMP-3 VALUE ZERO.           
048100                                                                          
048200     03  W-IDPURAD-MIN-X.                                                 
048300         05  W-IDPURAD-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
048400                                                                          
048500     03  W-IDPURAD-MAX-X.                                                 
048600         05  W-IDPURAD-MAX       PIC S9(5)   COMP-3 VALUE 99999.          
048700                                                                          
048800     03  W-IDPURAD-X.                                                     
048900         05  W-IDPURAD           PIC S9(5)   COMP-3 VALUE ZERO.           
049000                                                                          
049100     03  W-WDE421KY-X.                                                    
049200         05  W-IDPRODNR-E421     PIC S9(7)   COMP-3 VALUE ZERO.           
049300         05  W-IDKOLLI-E421      PIC S9(5)   COMP-3 VALUE ZERO.           
049400                                                                          
049500     03  W-WDE4F1KY-MIN-X.                                                
049600         05  W-IDPRODNR-MIN      PIC S9(7)   COMP-3 VALUE ZERO.           
049700         05  W-IDKOLLI-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
049800         05  FILLER              PIC X(22)   VALUE  LOW-VALUE.            
049900                                                                          
050000     03  W-WDE4F1KY-MAX-X.                                                
050100         05  W-IDPRODNR-MAX      PIC S9(7)   COMP-3 VALUE ZERO.           
050200         05  W-IDKOLLI-MAX       PIC S9(5)   COMP-3 VALUE ZERO.           
050300         05  FILLER              PIC X(22)   VALUE  HIGH-VALUE.           
050400                                                                          
050500     03  W-WDQ3DSEQ-X.                                                    
050600         05  W-IDPRODNR-Q3D      PIC S9(7)   COMP-3 VALUE ZERO.           
050700         05  W-IDPLKLST-Q3D      PIC S9(3)   COMP-3 VALUE ZERO.           
050800                                                                          
050900     03  W-IDORDER-X.                                                     
051000         05  W-IDORDER           PIC S9(7)   COMP-3 VALUE ZERO.           
051100                                                                          
051200     03  W-212-IDDC-X.                                                    
051300         05  W-212-IDDC          PIC  X(2)   VALUE SPACES.                
051400                                                                          
051500     03  W-WDK501KY-X.                                                    
051600         05  W-KDKOLLI-K5        PIC X(8)    VALUE SPACE.                 
051700                                                                          
051800   03    W-IDHTYP-4321-X.                                                 
051900         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
052000         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
052100*                                                                         
052200     03  W-WDGXKEY-4471-X.                                                
052300         05  W-IDHTYP-4471       PIC X(4)    VALUE '4471'.                
052400         05  W-IDDC-4471         PIC X(2).                                
052500         05  W-IDPRC-4471.                                                
052600             07  W-IDPRCBAS-4471 PIC X(3).                                
052700             07  W-IDPRCVAR-4471 PIC X.                                   
052800         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
052900*                                                                         
053000     03  W-KDSEGKEY-4472-X.                                               
053100         05  W-KDSEGKEY-4472     PIC X       VALUE '1'.                   
053200*                                                                         
053300     03  W-WDGXKEY-4477-X.                                                
053400         05  W-IDHTYP-4477       PIC X(4)    VALUE '4477'.                
053500         05  W-IDDC-4477         PIC X(2).                                
053600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
053700*                                                                         
053800     03  W-WDGXKEY-4478-X.                                                
053900         05  W-IDSHIFT-4478      PIC X.                                   
054000         05  W-IDUSER-4478       PIC X(8).                                
054100         05  FILLER              PIC X       VALUE LOW-VALUE.             
054200*                                                                         
054300     03  W-WDGXKEY-4726-X.                                                
054400         05  W-IDHTYP-4726       PIC X(4)    VALUE '4726'.                
054500         05  W-FLBATCH-4726      PIC X       VALUE SPACE.                 
054600         05  W-LOWVALUE-4726     PIC X(25)   VALUE LOW-VALUE.             
054700*                                                                         
054800     03  W-WDGXKEY-4727-X.                                                
054900         05  W-IDDISTR-4727      PIC S9(5)   COMP-3.                      
055000         05  W-IDKUNDNR-4727     PIC S9(7)   COMP-3.                      
055100         05  W-IDDC-4727         PIC X(2).                                
055200         05  W-KDFAKTYP-4727     PIC X.                                   
055300*                                                                         
055400     03  W-IDDC-B6-X.                                                     
055500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
055501                                                                          
055510     03  W-IDDC-X.                                                        
055520         05  W-IDDC               PIC X(2).                               
055530                                                                          
055600                                                                          
055700   03    W-IDARTNR-X.                                                     
055800     05    W-IDARTNR                 PIC S9(9) VALUE ZERO COMP-3.         
055900   03    W-KDSEGKEY-X.                                                    
056000     05    W-KDSEGKEY                PIC X(1)  VALUE '1'.                 
056100                                                                          
056200   03    W-WDA601KY-MIN-X.                                                
056300     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
056400     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
056500     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
056600     05    FILLER                    PIC X(03) VALUE SPACE.               
056700     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
056800     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
056900     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
057000     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
057100     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
057200     SKIP2                                                                
057300   03    W-WDA601KY-MAX-X.                                                
057400     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
057500     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
057600     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
057700     05    FILLER                    PIC X(03) VALUE SPACE.               
057800     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
057900     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
058000     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
058100     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
058200     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
058300     EJECT                                                                
058400                                                                          
058500 01  MESSAGE-CODES.                                                       
058600     03 ERR-UPD-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
058700     03 ERR-NOT-NUMERIC          PIC X(3)    VALUE '024'.                 
058800     03 ERR-WRONG-KEY            PIC X(3)    VALUE '022'.                 
058900     03 ERR-INVALID              PIC X(3)    VALUE '023'.                 
059000     03 ERR-MUST-BE-ENTERED      PIC X(3)    VALUE '026'.                 
059100     03 ERR-LINES-MISSING        PIC X(3)    VALUE '029'.                 
059200     03 ERR-INVALID-KEY-COMB     PIC X(3)    VALUE '032'.                 
059300     03 ERR-FORBIDDEN-INPUT      PIC X(3)    VALUE '033'.                 
059400     03 ERR-WRONG-ACTION-CODE    PIC X(3)    VALUE '099'.                 
059500     03 ERR-ORDERPART-READY      PIC X(3)    VALUE '116'.                 
059600     03 ERR-ZERO-NOT-ALLOWED     PIC X(3)    VALUE '126'.                 
059700     03 ERR-ENTER-INTERVAL       PIC X(3)    VALUE '141'.                 
059800     03 ERR-WRONG-INTERVAL       PIC X(3)    VALUE '142'.                 
059900     03 ERR-ORDER-REPORTED       PIC X(3)    VALUE '146'.                 
060000     03 ERR-USER-NOT-OK          PIC X(3)    VALUE '150'.                 
060100     03 ERR-LINE-WRONG           PIC X(3)    VALUE '160'.                 
060200     03 ERR-ORDER-NOT-SPLIT      PIC X(3)    VALUE '161'.                 
060300     03 ERR-LARGE-QUANT          PIC X(3)    VALUE '165'.                 
060400     03 ERR-LINE-REPORTED        PIC X(3)    VALUE '213'.                 
060500     03 ERR-PARTLY-REPORTED      PIC X(3)    VALUE '214'.                 
060600     03 ERR-TOO-MANY-CASES       PIC X(3)    VALUE '251'.                 
060700     03 ERR-WRONG-QUANT          PIC X(3)    VALUE '330'.                 
060800     03 ERR-ORDER-MISSING        PIC X(3)    VALUE '304'.                 
060900     03 ERR-CASE-INVOIC-RELEASE-LOAD                                      
061000                                 PIC X(3)    VALUE '400'.                 
061100     03 ERR-W403PLAT             PIC X(3)    VALUE '154'.                 
061200     03 ERR-USESCREEN-L0121-ORL0122 PIC  X(03)  VALUE '193'.              
061300     03 ERR-ORDER-HAS-WRONG-STATUS  PIC  X(03)  VALUE '273'.              
061400     03 INF-UPD-DONE             PIC X(3)    VALUE '001'.                 
061500     03 ERR-MORE-CASE-INFO-NEEDED PIC  X(03)  VALUE '140'.                
061600     03 ERR-UNAUTHORIZED          PIC X(30) VALUE '00A'.                  
061700     03 ERR-DIR-SUP-CASE          PIC X(30) VALUE '325'.                  
061800     EJECT                                                                
061900*01  -COPY WDECAREA                                                       
062000     EJECT                                                                
062100 01  FILLER                      PIC X(16)   VALUE 'W403PLAT'.            
062200*01  -COPY W403PLAT                                                       
062300     EJECT                                                                
062400 01  FILLER                      PIC X(16)   VALUE 'WL01TIDZ'.            
062500*01  -COPY WL01TIDZ                                                       
062600     EJECT                                                                
062700*                                                                         
062800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
062900*01  -COPY WZ01SEND                                                       
063000     EJECT                                                                
063100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
063200     SKIP3                                                                
063300*01  -COPY WZ01SUB                                                        
063400*                                                                         
063500*TMS PACKNING INFO                                                        
063600 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
063700*    -COPY W403TMS1                                                       
063800     EJECT                                                                
063900*                                                                         
064000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
064100     SKIP3                                                                
064200*                                                                         
064300 01  REQU-AREA.                                                           
064400*    03  -COPY WZ01REQ2                                                   
064500*    03  -COPY WL0199I1                                                   
064600     EJECT                                                                
064700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
064800     SKIP3                                                                
064900 01  RESP-AREA.                                                           
065000*    03  -COPY WZ01RES2                                                   
065100*    03  -COPY WL0199O1                                                   
065200     EJECT                                                                
065300 01  FILLER                      PIC X(16) VALUE 'CALL-RESP-AREA'.        
065400     SKIP3                                                                
065500 01  CALL-RESP-AREA.                                                      
065600*    03  -COPY WZ01RES2 -PRE CALL-                                        
065700*    03  -COPY WL0199O1 -PRE CALL-                                        
065800     EJECT                                                                
065900*------ DLI-IO-AREOR                                                      
066000 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDK501'.                       
066100 01  DLI-IO-WDK501.                                                       
066200*  03    EMBB01   -COPY WDK501                                            
066300     EJECT                                                                
066400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
066500 01  DLI-IO-WDE601.                                                       
066600*    03  -COPY WDE601                                                     
066700     SKIP3                                                                
066800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
066900 01  DLI-IO-WDE611.                                                       
067000*    03  -COPY WDE611                                                     
067100     EJECT                                                                
067200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE621'.                      
067300 01  DLI-IO-WDE621.                                                       
067400*    03  -COPY WDE621                                                     
067500     EJECT                                                                
067600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
067700 01  DLI-IO-WDE401.                                                       
067800*    03  -COPY WDE401                                                     
067900     EJECT                                                                
068000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE40111'.                    
068100 01  DLI-IO-WDE40111.                                                     
068200*    03  -COPY WDE401 -PRE   4011-                                        
068300     03 DLI-IO-WDE411.                                                    
068400*       05  -COPY WDE411                                                  
068500     EJECT                                                                
068600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE421'.                      
068700 01  DLI-IO-WDE421.                                                       
068800*    03  -COPY WDE421                                                     
068900     EJECT                                                                
069000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4F1'.                      
069100 01  DLI-IO-WDE4F1.                                                       
069200*    03  -COPY WDE4F1                                                     
069300     EJECT                                                                
069400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
069500 01  DLI-IO-WDQ301.                                                       
069600*    03  -COPY WDQ301                                                     
069700     EJECT                                                                
069800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ212'.                      
069900 01  DLI-IO-WDQ212.                                                       
070000*    03  -COPY WDQ212                                                     
070100     EJECT                                                                
070200 01  FILLER         PIC X(16) VALUE 'DLI-IO-GX01'.                        
070300 01  DLI-IO-GX01.                                                         
070400*    03  -COPY WDGX01                                                     
070500     EJECT                                                                
070600 01  FILLER         PIC X(16) VALUE 'DLI-IO-4726'.                        
070700 01  DLI-IO-4726.                                                         
070800*    03  -COPY WDGX4726                                                   
070900     EJECT                                                                
071000 01  FILLER         PIC X(16) VALUE 'DLI-IO-4727'.                        
071100 01  DLI-IO-4727.                                                         
071200*    03  -COPY WDGX4727                                                   
071300     EJECT                                                                
071400 01  FILLER         PIC X(16) VALUE 'DLI-IO-4472'.                        
071500 01  DLI-IO-4472.                                                         
071600*    03  -COPY WDGX4472                                                   
071700     EJECT                                                                
071800 01  FILLER         PIC X(16) VALUE 'DLI-IO-4478'.                        
071900 01  DLI-IO-4478.                                                         
072000*    03  -COPY WDGX4478                                                   
072100     EJECT                                                                
072200 01  FILLER         PIC X(16) VALUE 'DLI-IO-4322'.                        
072300 01  DLI-IO-4322.                                                         
072400*    03  -COPY WDGX4322                                                   
072500     EJECT                                                                
072600 01  FILLER         PIC X(16) VALUE 'DLI-IO-G601'.                        
072700 01  DLI-IO-WDG601.                                                       
072800*  03    -COPY WDGZ01     -PRE LOGG-                                      
072900     SKIP2                                                                
073000*01  WDGZRYK      -COPY WDGZRYK.                                          
073100     EJECT                                                                
073200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
073300 01   DLI-IO-WDB601.                                                      
073400*     03  -COPY WDB601                                                    
073500 01  FILLER               PIC X(16)   VALUE 'WDA601 AREA'.                
073600 01   DLI-IO-WDA601.                                                      
073700*     03  -COPY WDA601                                                    
073800     EJECT                                                                
073900 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK601'.              
074000 01    DLI-IO-WDK601.                                                     
074100*      03  -COPY WDK601                                                   
074200     EJECT                                                                
074300 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK611'.              
074400 01    DLI-IO-WDK611.                                                     
074500*      03  -COPY WDK611                                                   
074600     EJECT                                                                
074610 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK711'.              
074620 01    DLI-IO-WDK711.                                                     
074630*      03  -COPY WDK711                                                   
074640     EJECT                                                                
074700                                                                          
074800 LINKAGE SECTION.                                                         
074900*01  -COPY W0009  -PRE MSG-      PIC X.                                   
075000 01  MQASYNC-PCB       PIC X.                                             
075100*01  -COPY W0009  -PRE CASEREPO-                                          
075200                                                                          
075300*01  -COPY W0008  -PRE 4333-                                              
075400     05  FILLER                  PIC X.                                   
075500                                                                          
075600*01  -COPY W0008  -PRE 4341-                                              
075700     05  FILLER                  PIC X.                                   
075800                                                                          
075900*01  -COPY W0008  -PRE 4349-                                              
076000     05  FILLER                  PIC X.                                   
076100                                                                          
076200 01  TMS-CRE-PCB                 PIC X.                                   
076300 01  TMS-DEL-PCB                 PIC X.                                   
076400                                                                          
076500 01  ATAB-PCB                    PIC X.                                   
076600                                                                          
076700*01  -COPY W0008  -PRE WDK5-                                              
076800     05  FILLER                  PIC X.                                   
076900                                                                          
077000*01  -COPY W0008  -PRE WDE6-                                              
077100     05  FILLER                  PIC X.                                   
077200                                                                          
077300*01  -COPY W0008  -PRE WDE4-                                              
077400     05  FILLER                  PIC X(23).                               
077500     05  KFB-IDPURAD             PIC S9(5)  COMP-3.                       
077600                                                                          
077700*01  -COPY W0008  -PRE WDE4F-                                             
077800     05  FILLER                  PIC X.                                   
077900                                                                          
078000*01  -COPY W0008  -PRE WDQ2-                                              
078100     05  FILLER                  PIC X.                                   
078200     EJECT                                                                
078300*01  -COPY W0008  -PRE XXJK-                                              
078400     05  FILLER                  PIC X.                                   
078500     EJECT                                                                
078600*01  -COPY W0008  -PRE XXKW-                                              
078700     05  FILLER                  PIC X.                                   
078800     EJECT                                                                
078900*01  -COPY W0008  -PRE XXLB-                                              
079000     05  FILLER                  PIC X.                                   
079100     EJECT                                                                
079200*01  -COPY W0008  -PRE XXDV-                                              
079300     05  FILLER                  PIC X.                                   
079400     EJECT                                                                
079500*01  -COPY W0008  -PRE WDB6-                                              
079600     05  FILLER                  PIC X.                                   
079700     EJECT                                                                
079800*01  -COPY W0008  -PRE WDG6-                                              
079900     05  FILLER                  PIC X.                                   
080000     EJECT                                                                
080100*01  -COPY W0008  -PRE WDQ3D-                                             
080200     05  FILLER                  PIC X.                                   
080300     EJECT                                                                
080400*01  -COPY W0008  -PRE WDA6B-                                             
080500     05  FILLER                  PIC X.                                   
080600     EJECT                                                                
080700*01  -COPY W0008  -PRE WDK6-                                              
080800     05  FILLER                  PIC X.                                   
080810*01  -COPY W0008  -PRE WDK7-                                              
080820     05  FILLER                  PIC X.                                   
080900     EJECT                                                                
081000 01  PLATS-DM-PCB                PIC X.                                   
081100 01  PLATS-DN-PCB                PIC X.                                   
081200 01  PLATS-DP-PCB                PIC X.                                   
081300 01  PLATS-DO-PCB                PIC X.                                   
081400 01  PLATS-WDE6C-PCB             PIC X.                                   
081500 01  PLATS-GMTC-PCB              PIC X.                                   
081600 01  PLATS-WDB6-PCB              PIC X.                                   
081700                                                                          
081800 01  DNOT-ORQP-PCB               PIC X.                                   
081900 01  DNOT-ORQP2-PCB              PIC X.                                   
082000 01  DNOT-ORQP3-PCB              PIC X.                                   
082100 01  DNOT-4013-PCB               PIC X.                                   
082200 01  DNOT-BENA-PCB               PIC X.                                   
082300                                                                          
082400 01  TMS-1165-PCB                PIC X.                                   
082500 01  TMS-4141-PCB                PIC X.                                   
082600 01  TMS-WDB2-PCB                PIC X.                                   
082700 01  TMS-WDB6-PCB                PIC X.                                   
082800 01  TMS-WDD3-PCB                PIC X.                                   
082900 01  TMS-WDB1-PCB                PIC X.                                   
083000 01  TMS-WDE4A-PCB               PIC X.                                   
083100 01  TMS-WDE4F-PCB               PIC X.                                   
083200 01  TMS-WDQ2-PCB                PIC X.                                   
083300 01  TMS-WDQ3-PCB                PIC X.                                   
083400 01  TMS-WDK6-PCB                PIC X.                                   
083500 01  TMS-WDE6-PCB                PIC X.                                   
083600 01  TMS-WDK5-PCB                PIC X.                                   
083700 01  TMS-WDQ2C-PCB               PIC X.                                   
083800                                                                          
083900     EJECT                                                                
084000 PROCEDURE DIVISION  USING MSG-PCB MQASYNC-PCB                            
084010     CASEREPO-PCB 4333-PCB 4341-PCB                                       
084100     4349-PCB TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                            
084200     WDK5-PCB WDE6-PCB WDE4-PCB WDE4F-PCB WDQ2-PCB                        
084300     XXJK-PCB XXKW-PCB XXLB-PCB XXDV-PCB                                  
084400     WDB6-PCB WDG6-PCB WDQ3D-PCB WDA6B-PCB WDK6-PCB WDK7-PCB              
084500     PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB PLATS-DO-PCB                  
084600     PLATS-WDE6C-PCB PLATS-GMTC-PCB PLATS-WDB6-PCB                        
084700     DNOT-ORQP-PCB DNOT-ORQP2-PCB DNOT-ORQP3-PCB                          
084800     DNOT-4013-PCB DNOT-BENA-PCB                                          
084900     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB TMS-WDB6-PCB                  
085000     TMS-WDD3-PCB TMS-WDB1-PCB TMS-WDE4A-PCB TMS-WDE4F-PCB                
085100     TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                  
085200     TMS-WDK5-PCB TMS-WDQ2C-PCB.                                          
085300                                                                          
085400 MAIN SECTION.                                                            
085500                                                                          
085600     PERFORM S99-FETCH-REQUEST-ARGUMENT                                   
085700                                                                          
085800     IF SUB-KDRC = 0                                                      
085900*      IF REQU-KDPGMACT = 'E' OR 'R'                                      
086000       IF REQU-KDPGMACT = 'E'                                             
086100         PERFORM A-INIT                                                   
086200         IF INIT-OK                                                       
086300           PERFORM B-CHECK-KEYS                                           
086400                                                                          
086500           IF KEYS-OK                                                     
086600             IF REQU-KDPGMACT = 'E'                                       
086700               PERFORM C-CHECK-INPUT                                      
086800             END-IF                                                       
086900                                                                          
087000             IF DATA-OK                                                   
087100               PERFORM D-BACKOUT-DEF-CASE                                 
087200               PERFORM H-UPDATE                                           
087300               IF RESTART-ME                                              
087400                 PERFORM S10-SAVE-RESTART-AREA                            
087500                 PERFORM S90-SEND-TO-RESTART-THIS-PGM                     
087600               ELSE                                                       
087700                 PERFORM S11-UPD-WDQ212                                   
087800                                                                          
087900                 IF DATA-OK                                               
088000                   PERFORM I-UPD-E601                                     
088100*NOT UTILIZED YET  PERFORM L-EV-LOAD-L198                                 
088200                   IF SUB-KDTRANS(1:6) NOT = 'WLA199'                     
088300                     PERFORM K-LOAD-L123                                  
088400                   END-IF                                                 
088500                 END-IF                                                   
088600               END-IF                                                     
088700             END-IF                                                       
088800           ELSE                                                           
088900             IF ERR-USESCREEN-L0121-SW = YES                              
089000               CONTINUE                                                   
089100             ELSE                                                         
089200               MOVE ERR-INVALID-KEY-COMB TO RESP-IDMSG-ERROR              
089300             END-IF                                                       
089400             IF RESP-IDELMT-ERROR > SPACE                                 
089500               CONTINUE                                                   
089600             ELSE                                                         
089700               MOVE SPACE           TO RESP-IDELMT-ERROR                  
089800             END-IF                                                       
089900           END-IF                                                         
090000         END-IF                                                           
090100       ELSE                                                               
090200         MOVE 'KDPGMACT'            TO RESP-IDELMT-ERROR                  
090300         MOVE ERR-WRONG-ACTION-CODE TO RESP-IDMSG-ERROR                   
090400       END-IF                                                             
090500       IF RESP-IDMSG-ERROR NOT = SPACE                                    
090600         MOVE ALL '+'      TO RESP-WL0199O1 (1:17)                        
090700       END-IF                                                             
090800       IF SUB-KDTRANS(1:6) = 'WLA199'                                     
090900         PERFORM S30-MSG-CONV                                             
091000       END-IF                                                             
091100*      IF NOT RESTART-ME                                                  
091200         PERFORM S99-RETURN-RESPONSE                                      
091300*      END-IF                                                             
091400     END-IF                                                               
091500     MOVE ZERO TO RETURN-CODE                                             
091600     GOBACK                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 A-INIT SECTION.                                                          
092000                                                                          
092100     MOVE 'A-INIT        '    TO WS-CURRENT-SECTION                       
092200                                                                          
092300     MOVE NOO          TO RESTART-ME-SW                                   
092400     MOVE ZERO         TO UPDATE-IX                                       
092500     MOVE ALL '+'      TO RESP-AREA                                       
092600     MOVE SPACE        TO RESP-IDMSG-ERROR                                
092700                          RESP-IDMSG-INFO                                 
092800                          RESP-IDELMT-ERROR                               
092900     MOVE ZERO         TO RESP-KVRADER-MAX1                               
093000                          RESP-L128-KVRADER                               
093100                          RESP-L129-KVRADER                               
093200                          WORK-NO-OF-CASES                                
093300                          WORK-NO-OF-LINES                                
093400                          WORK-EMB-TARE-ONE-CASE                          
093500                          WORK-EMB-VOL-ONE-CASE                           
093600                          OLD-EMB-VOL-ONE-CASE                            
093700                          WORK-KOLLI-VLORDBTO                             
093800                          WORK-KOLLI-VKORDBTO                             
093900                          WORK-KOLLI-VKORDNTO                             
094000                          WORK-KOLLI-KVFLAMP                              
094100                          WORK-KOLLI-KDFARLIG                             
094200                          WORK-KOLLI-KVORDRAD                             
094300                          WORK-KOLLI-KVFALRAD                             
094400                          WORK-KOLLI-SUORDV-LOC                           
094500                          WORK-KOLLI-SUORDV-LOCPREL                       
094600                          WORK-KOLLI-KVLEVART                             
094700                          WORK-KOLLI-SUORDV                               
094800                          WORK-KOLLI-SUORDV-EXP                           
094900     MOVE YES          TO DATA-SW                                         
095000                                                                          
095100     ACCEPT DAGENS-DATUM FROM DATE                                        
095200     ACCEPT DAGENS-TID   FROM TIME                                        
095300                                                                          
095400     INITIALIZE TMS-W403TMS1                                              
095500     MOVE +1    TO TMS-IX                                                 
095600                                                                          
095700* --- CHECK AUTHORIZATION                                                 
095800     IF SUB-KDTRANS(1:7) = 'WL0199U'                                      
095900        CONTINUE                                                          
096000     ELSE                                                                 
096100        MOVE 001                  TO AUTH-KDCALL                          
096200        CALL WZ01AUTH          USING AUTH-WZ01AUTH                        
096300                                     REQU-WZ01REQ2                        
096400                                                                          
096500        IF AUTH-KDRC > 0                                                  
096600          MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                     
096700          MOVE NOO                TO INIT-SW                              
096800        END-IF                                                            
096900                                                                          
097000        PERFORM AA-HANDLE-LOWER-CASE                                      
097100     END-IF                                                               
097200                                                                          
097300     IF REQU-IDDC-KEY = 'NU' OR '++'                                      
097400* --- FIX FOR REQUEST ERRORS                                              
097500       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
097600       MOVE 'IDDC'        TO RESP-IDELMT-ERROR                            
097700       MOVE NOO           TO INIT-SW                                      
097800     ELSE                                                                 
097900                                                                          
098000*      IF REQU-KDPGMACT = 'E' OR 'R'                                      
098100       IF REQU-KDPGMACT = 'E'                                             
098200         MOVE REQU-IDDC-KEY TO W-IDDC-B6                                  
098300                               WS-IDDC                                    
098310                               W-IDDC                                     
098400         PERFORM IMS-GU-WDB601                                            
098500       ELSE                                                               
098600         MOVE ERR-WRONG-ACTION-CODE TO RESP-IDMSG-ERROR                   
098700         MOVE 'KDPGMACT'    TO RESP-IDELMT-ERROR                          
098800         MOVE NOO           TO INIT-SW                                    
098900       END-IF                                                             
099000     END-IF                                                               
099100                                                                          
099200     IF INIT-OK                                                           
099300       MOVE '011'      TO MSGI-KDCALL                                     
099400       MOVE DCS-IDTIDZON TO MSGI-IDTIDZON                                 
099500       MOVE DCS-IDDC     TO MSGI-IDDC                                     
099600       MOVE DAGENS-DATUM TO MSGI-TILOKDAT                                 
099700       MOVE DAGENS-TID TO MSGI-TILOKTID                                   
099800       CALL WL01TIDZ USING MSGI-WL01TIDZ                                  
099900       MOVE MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                            
100000       MOVE MSGI-TILOKTID(1:4) TO DAGENS-TID(1:4)                         
100100       MOVE FUNCTION CURRENT-DATE(13:2) TO DAGENS-TID(5:2)                
100200     END-IF                                                               
100300                                                                          
100400*    IF REQU-KDPGMACT = 'R'                                               
100500*      MOVE REQU-RESTART-IDKOLLI  TO SAVE-IDKOLLI                         
100600*?     MOVE REQU-RESTART-KDKOLLI  TO SAVE-KDKOLLI                         
100700*      MOVE REQU-RESTART-KVKOLLI  TO WORK-NO-OF-CASES                     
100800*    END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100                                                                          
101200 AA-HANDLE-LOWER-CASE SECTION.                                            
101300     MOVE 'AA-HANDLE-LOWER-CASE' TO WS-CURRENT-SECTION                    
101400                                                                          
101500     MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)                             
101600                                 TO REQU-IDDC-KEY                         
101700     MOVE FUNCTION UPPER-CASE (REQU-KDMATT)                               
101800                                 TO REQU-KDMATT                           
101900     PERFORM                                                              
102000     VARYING LINE-IX FROM 1 BY 1                                          
102100       UNTIL LINE-IX > REQU-KVRADER-MAX1                                  
102200       MOVE FUNCTION UPPER-CASE (REQU-KDKOLLI  (LINE-IX))                 
102300                                 TO REQU-KDKOLLI  (LINE-IX)               
102400       MOVE FUNCTION UPPER-CASE (REQU-KDARTURS (LINE-IX))                 
102500                                 TO REQU-KDARTURS (LINE-IX)               
102600     END-PERFORM                                                          
102700     .                                                                    
102800                                                                          
102900                                                                          
103000 B-CHECK-KEYS SECTION.                                                    
103100                                                                          
103200     MOVE 'B-CHECK-KEYS  '    TO WS-CURRENT-SECTION                       
103300                                                                          
103400     MOVE YES TO KEYS-SW                                                  
103500     MOVE 'Y' TO REQU-FLSISTAK                                            
103600                 RESP-FLSISTAK                                            
103700                                                                          
103800     IF (REQU-IDANSTNR-KEY = ALL '+')                                     
103900     OR (REQU-IDANSTNR-KEY NOT NUMERIC)                                   
104000     OR (REQU-IDANSTNR-KEY = ZERO)                                        
104100       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
104200       MOVE 'IDUSER'      TO RESP-IDELMT-ERROR                            
104300       MOVE NOO           TO KEYS-SW                                      
104400     END-IF                                                               
104500                                                                          
104600     IF (REQU-IDPRODNR-KEY = ALL '+')                                     
104700     OR (REQU-IDPRODNR-KEY NOT NUMERIC)                                   
104800     OR (REQU-IDPRODNR-KEY = ZERO)                                        
104900       IF KEYS-OK                                                         
105000         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
105100         MOVE 'IDPRODNR'  TO RESP-IDELMT-ERROR                            
105200         MOVE NOO         TO KEYS-SW                                      
105300       END-IF                                                             
105400     END-IF                                                               
105500                                                                          
105600     IF KEYS-OK                                                           
105700       PERFORM BA-CHECK-DIR-SUP-CASE                                      
105800     END-IF                                                               
105900                                                                          
106000     IF REQU-IDPLKLST-KEY = ALL '+'                                       
106100     OR (REQU-IDPLKLST-KEY NOT NUMERIC)                                   
106200     OR (REQU-IDPLKLST-KEY = ZERO)                                        
106300       IF KEYS-OK                                                         
106400         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
106500         MOVE 'IDPLKLST'  TO RESP-IDELMT-ERROR                            
106600         MOVE NOO         TO KEYS-SW                                      
106700       END-IF                                                             
106800     END-IF                                                               
106900                                                                          
107000     IF DCS-CDC                                                           
107100       IF REQU-FLSKRIV-CLABEL = 'Y'                                       
107200         IF REQU-PRTVAL-ADRESSFL = ALL '+'                                
107300           IF KEYS-OK                                                     
107400             MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                       
107500             MOVE 'PRINTER' TO RESP-IDELMT-ERROR                          
107600             MOVE NOO     TO KEYS-SW                                      
107700           END-IF                                                         
107800         ELSE                                                             
107900           MOVE REQU-PRTVAL-ADRESSFL TO WS-KDPRTVAL-ADR                   
108000           MOVE '4'              TO WS-SYSTDEL                            
108100           MOVE 'KF'             TO WS-LISTTYP                            
108200           MOVE WS-IDDC          TO WS-DC                                 
108300           MOVE WS-KDPRTVAL-ADR  TO WS-KDPRT                              
108400                                                                          
108500           MOVE 001              TO PRT-KDCALL                            
108600           MOVE WS-IDPRTLST      TO PRT-IDPRTLST                          
108700                                                                          
108800           CALL W006PRT USING PRT-W006PRT                                 
108900                                                                          
109000           IF PRT-KDSVAR = RAETT                                          
109100             CONTINUE                                                     
109200           ELSE                                                           
109300            IF KEYS-OK                                                    
109400              MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                      
109500              MOVE 'PRINTER' TO RESP-IDELMT-ERROR                         
109600              MOVE NOO    TO KEYS-SW                                      
109700            END-IF                                                        
109800           END-IF                                                         
109900         END-IF                                                           
110000       END-IF                                                             
110100                                                                          
110200       IF REQU-FLSKRIV-DELNOTE = 'Y'                                      
110300         IF REQU-PRTVAL-FOLJEFL = ALL '+'                                 
110400          IF KEYS-OK                                                      
110500            MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                        
110600            MOVE 'PRINTER' TO RESP-IDELMT-ERROR                           
110700            MOVE NOO      TO KEYS-SW                                      
110800          END-IF                                                          
110900         ELSE                                                             
111000           MOVE REQU-PRTVAL-FOLJEFL TO WS-KDPRTVAL-FS                     
111100           MOVE '4'              TO WS-SYSTDEL                            
111200           MOVE 'FS'             TO WS-LISTTYP                            
111300           MOVE WS-IDDC          TO WS-DC                                 
111400           MOVE WS-KDPRTVAL-FS   TO WS-KDPRT                              
111500                                                                          
111600           MOVE 001              TO PRT-KDCALL                            
111700           MOVE WS-IDPRTLST      TO PRT-IDPRTLST                          
111800                                                                          
111900           CALL W006PRT USING PRT-W006PRT                                 
112000                                                                          
112100           IF PRT-KDSVAR = RAETT                                          
112200             CONTINUE                                                     
112300           ELSE                                                           
112400            IF KEYS-OK                                                    
112500              MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                      
112600              MOVE 'PRINTER' TO RESP-IDELMT-ERROR                         
112700              MOVE NOO    TO KEYS-SW                                      
112800            END-IF                                                        
112900           END-IF                                                         
113000         END-IF                                                           
113100       END-IF                                                             
113200     END-IF                                                               
113300                                                                          
113400     IF KEYS-OK                                                           
113500       MOVE REQU-IDANSTNR-KEY     TO RESP-IDANSTNR-KEY                    
113600       MOVE REQU-IDPRODNR-KEY     TO RESP-IDPRODNR-KEY                    
113700                                     W-IDPRODNR-Q3D                       
113800                                     W-IDPRODNR-E4                        
113900                                     W-IDPRODNR-E421                      
114000                                     W-IDPRODNR-E6                        
114100                                     W-IDPRODNR-MIN                       
114200                                     W-IDPRODNR-MAX                       
114300       MOVE REQU-IDPLKLST-KEY     TO RESP-IDPLKLST-KEY                    
114400                                     W-IDPLKLST-Q3D                       
114500                                     W-IDPLKLST-E4                        
114600       MOVE REQU-IDDC-KEY         TO RESP-IDDC-KEY                        
114700       MOVE REQU-FLSKRIV-CLABEL   TO RESP-FLSKRIV-CLABEL                  
114800       MOVE REQU-FLSKRIV-DELNOTE  TO RESP-FLSKRIV-DELNOTE                 
114900       MOVE REQU-PRTVAL-ADRESSFL  TO RESP-PRTVAL-ADRESSFL                 
115000       MOVE REQU-PRTVAL-FOLJEFL   TO RESP-PRTVAL-FOLJEFL                  
115100                                                                          
115200       PERFORM IMS-GU-WDQ301-DSEQ                                         
115300       IF SEGMENT-FOUND                                                   
115400         MOVE ODEL-IDORDNR5       TO WS-ODEL-IDORDNR5                     
115500         MOVE ODEL-IDDISTR        TO TEST-IDDISTR                         
115600                                     W-IDDISTR-E4                         
115700                                     WS-IDDISTR                           
115800         MOVE ODEL-IDORDER        TO WS-DNOT-IDORDER                      
115900         MOVE ODEL-IDDC              TO WS-DNOT-IDDC                      
116000         MOVE ODEL-IDLEVNR         TO WS-IDLEVNR                          
116100         MOVE ODEL-IDDC-EXP        TO WS-ODEL-IDDC-EXP                    
116200         IF DIST19-SATS                                                   
116300           MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                   
116400           MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                  
116500           MOVE NOO                 TO KEYS-SW                            
116600         ELSE                                                             
116700           MOVE REQU-IDANSTNR-KEY   TO WS-IDUSER-ALFA                     
116800           IF ODEL-IDUSER (4:5) = WS-IDUSER-ALFA                          
116900             MOVE ODEL-IDKUNDNR      TO W-IDKUNDNR-E4                     
117000             MOVE ODEL-IDKUNDRF(3:5) TO W-IDORDNR-E4                      
117100             PERFORM IMS-GHU-WDE401                                       
117200             IF SEGMENT-MISSING                                           
117300               MOVE ERR-ORDER-MISSING TO RESP-IDMSG-ERROR                 
117400               MOVE SPACE             TO RESP-IDELMT-ERROR                
117500               MOVE NOO               TO KEYS-SW                          
117600             END-IF                                                       
117700           ELSE                                                           
117800             MOVE ERR-USER-NOT-OK TO RESP-IDMSG-ERROR                     
117900             MOVE 'IDUSER'        TO RESP-IDELMT-ERROR                    
118000             MOVE NOO             TO KEYS-SW                              
118100           END-IF                                                         
118200         END-IF                                                           
118300       ELSE                                                               
118400         MOVE ERR-ORDER-MISSING   TO RESP-IDMSG-ERROR                     
118500         MOVE SPACE               TO RESP-IDELMT-ERROR                    
118600         MOVE NOO                 TO KEYS-SW                              
118700       END-IF                                                             
118800     ELSE                                                                 
118900       IF ERR-USESCREEN-L0121-SW = YES                                    
119000         CONTINUE                                                         
119100       ELSE                                                               
119200         MOVE ERR-INVALID-KEY-COMB TO RESP-IDMSG-ERROR                    
119300       END-IF                                                             
119400       IF RESP-IDELMT-ERROR > SPACE                                       
119500         CONTINUE                                                         
119600       ELSE                                                               
119700         MOVE SPACE               TO RESP-IDELMT-ERROR                    
119800       END-IF                                                             
119900     END-IF                                                               
120000     .                                                                    
120100                                                                          
120200 BA-CHECK-DIR-SUP-CASE SECTION.                                           
120300     MOVE 'BA-CHECK-DIR-SUP-CASE'  TO WS-CURRENT-SECTION                  
120400                                                                          
120500     MOVE REQU-IDPRODNR-KEY     TO W-IDPRODNR-E6                          
120600     PERFORM IMS-GU-WDE601                                                
120700     IF SEGMENT-FOUND                                                     
120800       IF VORD-FLDIRLEV = NEJ OR DCS-CDC                                  
120900          CONTINUE                                                        
121000       ELSE                                                               
121100         IF KEYS-OK                                                       
121200           MOVE ERR-DIR-SUP-CASE TO RESP-IDMSG-ERROR                      
121300           MOVE 'FLDIRLEV'      TO RESP-IDELMT-ERROR                      
121400           MOVE NOO             TO KEYS-SW                                
121500         END-IF                                                           
121600       END-IF                                                             
121700     END-IF                                                               
121800     .                                                                    
121900     SKIP2                                                                
122000                                                                          
122100 C-CHECK-INPUT SECTION.                                                   
122200     MOVE 'C-CHECK-INPUT'    TO WS-CURRENT-SECTION                        
122300                                                                          
122400     MOVE +1 TO LINE-IX                                                   
122500                                                                          
122600     PERFORM UNTIL LINE-IX > REQU-KVRADER-MAX1 OR MAX-IX                  
122700     OR REQU-IDRADNR-FOM (LINE-IX) = ALL '+'                              
122800     OR REQU-IDRADNR-FOM (LINE-IX) <= SPACES                              
122900     OR DATA-WRONG                                                        
123000                                                                          
123100       PERFORM CA-CHECK-IDRADNR                                           
123200       PERFORM CB-CHECK-IDKOLLI                                           
123300       PERFORM CC-CHECK-KDKOLLI                                           
123400       PERFORM CD-CHECK-KVLEVART                                          
123500                                                                          
123600       IF DATA-OK                                                         
123700         PERFORM CE-CHECK-IDRADNR-IN-TABLE                                
123800       END-IF                                                             
123900                                                                          
124000       IF DATA-OK                                                         
124100         PERFORM CF-VALIDATE-KDARTURS                                     
124200       END-IF                                                             
124300                                                                          
124400       MOVE REQU-KVRADER-MAX1    TO RESP-KVRADER-MAX1                     
124500       ADD +1 TO LINE-IX                                                  
124600     END-PERFORM                                                          
124700     IF LINE-IX = +1                                                      
124800*YOU SHOULD NOT BE ABLE TO LEAVE AN EMPTY LINE 1 ON THE SCREEN            
124900*                                                                         
125000       MOVE ERR-MUST-BE-ENTERED    TO RESP-IDMSG-ERROR                    
125100                            RESP-IDMSG-ERROR-LINE (LINE-IX)               
125200       MOVE 'IDKOLLI'              TO RESP-IDELMT-ERROR                   
125300       MOVE NOO                    TO DATA-SW                             
125400     END-IF                                                               
125500     .                                                                    
125600     EJECT                                                                
125700 CA-CHECK-IDRADNR   SECTION.                                              
125800     MOVE 'CA-CHECK-IDRADNR'    TO WS-CURRENT-SECTION                     
125900                                                                          
126000     IF REQU-IDRADNR-FOM (LINE-IX) NUMERIC                                
126100       IF REQU-IDRADNR-FOM (LINE-IX) > ZERO                               
126200         MOVE REQU-IDRADNR-FOM (LINE-IX)                                  
126300           TO RESP-IDRADNR-FOM (LINE-IX)                                  
126400                                        SAVE-WORK-IDRADNR-FOM             
126500                                        SAVE-WORK-IDRADNR-TOM             
126600       ELSE                                                               
126700         MOVE ERR-ZERO-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
126800                              RESP-IDMSG-ERROR-LINE (LINE-IX)             
126900         MOVE 'IDRADNR-FOM'        TO RESP-IDELMT-ERROR                   
127000         MOVE NOO                  TO DATA-SW                             
127100       END-IF                                                             
127200     ELSE                                                                 
127300       MOVE ERR-NOT-NUMERIC  TO RESP-IDMSG-ERROR                          
127400                                RESP-IDMSG-ERROR-LINE (LINE-IX)           
127500       MOVE 'IDRADNR-FOM'    TO RESP-IDELMT-ERROR                         
127600       MOVE NOO              TO DATA-SW                                   
127700     END-IF                                                               
127800                                                                          
127900     IF REQU-IDRADNR-TOM (LINE-IX) NOT = ALL '+'                          
128000       IF REQU-IDRADNR-TOM (LINE-IX) NUMERIC AND                          
128100          REQU-IDRADNR-TOM (LINE-IX) NOT = 0                              
128200         MOVE REQU-IDRADNR-TOM (LINE-IX) TO SAVE-WORK-IDRADNR-TOM         
128300         IF (REQU-IDRADNR-FOM (LINE-IX) = ALL '+')                        
128400         OR (REQU-IDRADNR-FOM (LINE-IX) >                                 
128500            REQU-IDRADNR-TOM (LINE-IX))                                   
128600           MOVE ERR-WRONG-INTERVAL TO RESP-IDMSG-ERROR                    
128700                              RESP-IDMSG-ERROR-LINE (LINE-IX)             
128800           MOVE 'IDRADNR-TOM'      TO RESP-IDELMT-ERROR                   
128900           MOVE NOO                TO DATA-SW                             
129000         ELSE                                                             
129100           IF REQU-KVLEVART (LINE-IX) NOT = ALL '+'                       
129200             IF REQU-IDRADNR-FOM (LINE-IX) =                              
129300                REQU-IDRADNR-TOM (LINE-IX)                                
129400                MOVE REQU-IDRADNR-FOM (LINE-IX)                           
129500                  TO RESP-IDRADNR-FOM (LINE-IX)                           
129600                MOVE REQU-IDRADNR-TOM (LINE-IX)                           
129700                  TO RESP-IDRADNR-TOM (LINE-IX)                           
129800             ELSE                                                         
129900                MOVE ERR-FORBIDDEN-INPUT                                  
130000                                      TO RESP-IDMSG-ERROR                 
130100                                RESP-IDMSG-ERROR-LINE (LINE-IX)           
130200                MOVE 'KVLEVART'       TO RESP-IDELMT-ERROR                
130300                MOVE NOO              TO DATA-SW                          
130400             END-IF                                                       
130500           ELSE                                                           
130600             MOVE REQU-IDRADNR-FOM (LINE-IX)                              
130700               TO RESP-IDRADNR-FOM (LINE-IX)                              
130800             MOVE REQU-IDRADNR-TOM (LINE-IX)                              
130900               TO RESP-IDRADNR-TOM (LINE-IX)                              
131000           END-IF                                                         
131100         END-IF                                                           
131200       ELSE                                                               
131300         IF REQU-IDRADNR-TOM (LINE-IX) = 0                                
131400            MOVE ERR-ZERO-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
131500                                  RESP-IDMSG-ERROR-LINE (LINE-IX)         
131600            MOVE 'IDRADNR-TOM'      TO RESP-IDELMT-ERROR                  
131700            MOVE NOO                TO DATA-SW                            
131800         ELSE                                                             
131900            MOVE ERR-NOT-NUMERIC  TO RESP-IDMSG-ERROR                     
132000                                   RESP-IDMSG-ERROR-LINE(LINE-IX)         
132100                                                                          
132200            MOVE 'IDRADNR-TOM'    TO RESP-IDELMT-ERROR                    
132300            MOVE NOO              TO DATA-SW                              
132400         END-IF                                                           
132500       END-IF                                                             
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900 CB-CHECK-IDKOLLI SECTION.                                                
133000                                                                          
133100     IF REQU-IDKOLLI (LINE-IX) NOT = ALL '+'                              
133200        IF (REQU-IDKOLLI (LINE-IX) NOT NUMERIC)                           
133300        OR (REQU-IDKOLLI (LINE-IX) = ZERO )                               
133400           MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
133500                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
133600           MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                      
133700           MOVE NOO             TO DATA-SW                                
133800        ELSE                                                              
133900           MOVE REQU-IDKOLLI (LINE-IX) TO W-IDKOLLI-MIN                   
134000                                          W-IDKOLLI-MAX                   
134100           PERFORM IMS-GU-WDE4F1-PLK-NE                                   
134200           IF SEGMENT-FOUND                                               
134300** KOLLINR FÅR INTE VARA UTTAGEN PÅ ANNAN ORDERDEL!                       
134400              MOVE ERR-ORDER-REPORTED TO RESP-IDMSG-ERROR                 
134500                                 RESP-IDMSG-ERROR-LINE (LINE-IX)          
134600              MOVE 'IDKOLLI'          TO RESP-IDELMT-ERROR                
134700              MOVE NOO                TO DATA-SW                          
134800                                                                          
134900           END-IF                                                         
135000                                                                          
135100           MOVE NEJ        TO DEF-IDKOLLI-SW                              
135200           MOVE ZERO    TO W-IDKOLLI-MIN                                  
135300           MOVE 99999   TO W-IDKOLLI-MAX                                  
135400           PERFORM IMS-GU-WDE4F1-PLK                                      
135500           IF SEGMENT-FOUND                                               
135600             MOVE SEQF-IDKOLLI TO W-IDKOLLI-E6                            
135700             PERFORM IMS-GHU-WDE611-DEF                                   
135800             IF SEGMENT-FOUND                                             
135900* KDKOLSTA CAN BE 0 FOR PACKED CASES FROM API                             
136000* SO ADDED EXTRA DIKOLLI CHECK                                            
136100              IF KOLLI-DIKOLLIL = ZERO                                    
136200*DEFAULT CASE EXISTS                                                      
136300               MOVE JA     TO DEF-IDKOLLI-SW                              
136400              END-IF                                                      
136500             END-IF                                                       
136600           END-IF                                                         
136700                                                                          
136800           MOVE REQU-IDPRODNR-KEY         TO  W-IDPRODNR-E6               
136900           MOVE REQU-IDKOLLI (LINE-IX)    TO  W-IDKOLLI-E6                
137000           PERFORM IMS-GU-WDE611                                          
137100           IF SEGMENT-FOUND                                               
137200             IF KOLLI-KDKOLSTA  > 1                                       
137300*ITS NOT ALLOWED TO ADD A LINE IN A SHIPED OR INVOICED CASE AND           
137400*NOT ALLOWED TO REPORT A CASE AGAIN AFTER REPORT ON SCREEN WL0197         
137500               MOVE NOO                   TO DATA-SW                      
137600               MOVE ERR-CASE-INVOIC-RELEASE-LOAD                          
137700                 TO RESP-IDMSG-ERROR                                      
137800                    RESP-IDMSG-ERROR-LINE (LINE-IX)                       
137900             END-IF                                                       
138000           END-IF                                                         
138100           MOVE REQU-IDKOLLI (LINE-IX) TO RESP-IDKOLLI (LINE-IX)          
138200        END-IF                                                            
138300     END-IF                                                               
138400     .                                                                    
138500     EJECT                                                                
138600 CC-CHECK-KDKOLLI SECTION.                                                
138700                                                                          
138800     MOVE REQU-KDMATT  TO WS-KDMATT                                       
138900     MOVE ZERO         TO WS-DIKOLLIL                                     
139000                          WS-DIKOLLIB                                     
139100                          WS-DIKOLLIH                                     
139200                                                                          
139300     IF REQU-KDKOLLI (LINE-IX) = ALL '+' OR SPACE                         
139400        PERFORM CCA-CHECK-KDKOLLI-KDEMBTYP                                
139500        PERFORM CCB-CHECK-KOLLI-DIMENSION                                 
139600     ELSE                                                                 
139700        IF REQU-KDEMBTYP (LINE-IX) NOT = ALL '+' AND NOT = 0              
139800           MOVE ERR-INVALID     TO RESP-IDMSG-ERROR-LINE (LINE-IX)        
139900                                       RESP-IDMSG-ERROR                   
140000           MOVE 'KDEMBTYP'          TO RESP-IDELMT-ERROR                  
140100           MOVE NOO                 TO DATA-SW                            
140200        ELSE                                                              
140300           MOVE REQU-KDKOLLI (LINE-IX) TO W-KDKOLLI-K5                    
140400           PERFORM IMS-GU-WDK5                                            
140500           IF SEGMENT-MISSING                                             
140600              MOVE ERR-INVALID  TO RESP-IDMSG-ERROR-LINE (LINE-IX)        
140700                                       RESP-IDMSG-ERROR                   
140800              MOVE 'KDKOLLI'        TO RESP-IDELMT-ERROR                  
140900              MOVE NOO              TO DATA-SW                            
141000           ELSE                                                           
141100            MOVE REQU-KDKOLLI (LINE-IX) TO RESP-KDKOLLI (LINE-IX)         
141200              IF REQU-DIKOLLIL (LINE-IX) = (ALL '+' OR ZERO)              
141300                 IF EMB-DIKOLLIL = ZERO                                   
141400                    MOVE ERR-MUST-BE-ENTERED                              
141500                      TO RESP-IDMSG-ERROR-LINE (LINE-IX)                  
141600                         RESP-IDMSG-ERROR                                 
141700                    MOVE 'DIKOLLIL' TO RESP-IDELMT-ERROR                  
141800                    MOVE NOO        TO DATA-SW                            
141900                 ELSE                                                     
142000                    MOVE EMB-DIKOLLIL  TO WS-DIKOLLIL                     
142100                 END-IF                                                   
142200              ELSE                                                        
142300                 INSPECT REQU-DIKOLLIL (LINE-IX) REPLACING                
142400                         LEADING SPACE BY ZERO                            
142500                 IF REQU-DIKOLLIL (LINE-IX) NUMERIC                       
142600                 AND REQU-DIKOLLIL (LINE-IX) > ZERO                       
142700                   MOVE REQU-DIKOLLIL (LINE-IX) TO WS-DIKOLLIL            
142800                   IF US-MEASUREMENT                                      
142900                      COMPUTE WS-DIKOLLIL ROUNDED =                       
143000                              WS-DIKOLLIL * CONV-IN-TO-CM                 
143100                      END-COMPUTE                                         
143200                   END-IF                                                 
143300                 ELSE                                                     
143400                   MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR               
143500                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
143600                   MOVE 'DIKOLLIL' TO RESP-IDELMT-ERROR                   
143700                   MOVE NOO  TO DATA-SW                                   
143800                 END-IF                                                   
143900              END-IF                                                      
144000              IF REQU-DIKOLLIB (LINE-IX) = (ALL '+' OR ZERO)              
144100                 IF EMB-DIKOLLIB = ZERO                                   
144200                    MOVE ERR-MUST-BE-ENTERED                              
144300                      TO RESP-IDMSG-ERROR-LINE (LINE-IX)                  
144400                         RESP-IDMSG-ERROR                                 
144500                    MOVE 'DIKOLLIB' TO RESP-IDELMT-ERROR                  
144600                    MOVE NOO        TO DATA-SW                            
144700                 ELSE                                                     
144800                    MOVE EMB-DIKOLLIB  TO WS-DIKOLLIB                     
144900                 END-IF                                                   
145000              ELSE                                                        
145100                 INSPECT REQU-DIKOLLIB (LINE-IX) REPLACING                
145200                         LEADING SPACE BY ZERO                            
145300                 IF REQU-DIKOLLIB (LINE-IX) NUMERIC                       
145400                 AND REQU-DIKOLLIB (LINE-IX) > ZERO                       
145500                   MOVE REQU-DIKOLLIB (LINE-IX) TO WS-DIKOLLIB            
145600                   IF US-MEASUREMENT                                      
145700                      COMPUTE WS-DIKOLLIB ROUNDED =                       
145800                              WS-DIKOLLIB * CONV-IN-TO-CM                 
145900                      END-COMPUTE                                         
146000                   END-IF                                                 
146100                 ELSE                                                     
146200                   MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR               
146300                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
146400                   MOVE 'DIKOLLIB' TO RESP-IDELMT-ERROR                   
146500                   MOVE NOO  TO DATA-SW                                   
146600                 END-IF                                                   
146700              END-IF                                                      
146800              IF REQU-DIKOLLIH (LINE-IX) = (ALL '+' OR ZERO)              
146900                 IF EMB-DIKOLLIH = ZERO                                   
147000                    MOVE ERR-MUST-BE-ENTERED                              
147100                      TO RESP-IDMSG-ERROR-LINE (LINE-IX)                  
147200                         RESP-IDMSG-ERROR                                 
147300                    MOVE 'DIKOLLIH' TO RESP-IDELMT-ERROR                  
147400                    MOVE NOO        TO DATA-SW                            
147500                 ELSE                                                     
147600                    MOVE EMB-DIKOLLIH  TO WS-DIKOLLIH                     
147700                 END-IF                                                   
147800              ELSE                                                        
147900                 INSPECT REQU-DIKOLLIH (LINE-IX) REPLACING                
148000                         LEADING SPACE BY ZERO                            
148100                 IF REQU-DIKOLLIH (LINE-IX) NUMERIC                       
148200                 AND REQU-DIKOLLIH (LINE-IX) > ZERO                       
148300                   MOVE REQU-DIKOLLIH (LINE-IX) TO WS-DIKOLLIH            
148400                   IF US-MEASUREMENT                                      
148500                      COMPUTE WS-DIKOLLIH ROUNDED =                       
148600                              WS-DIKOLLIH * CONV-IN-TO-CM                 
148700                      END-COMPUTE                                         
148800                   END-IF                                                 
148900                 ELSE                                                     
149000                   MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR               
149100                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
149200                   MOVE 'DIKOLLIH' TO RESP-IDELMT-ERROR                   
149300                   MOVE NOO  TO DATA-SW                                   
149400                 END-IF                                                   
149500              END-IF                                                      
149600           END-IF                                                         
149700        END-IF                                                            
149800     END-IF                                                               
149900     MOVE WS-DIKOLLIL      TO TAB-DIKOLLIL (LINE-IX)                      
150000     MOVE WS-DIKOLLIB      TO TAB-DIKOLLIB (LINE-IX)                      
150100     MOVE WS-DIKOLLIH      TO TAB-DIKOLLIH (LINE-IX)                      
150200     .                                                                    
150300     EJECT                                                                
150400 CCA-CHECK-KDKOLLI-KDEMBTYP    SECTION.                                   
150500                                                                          
150600     IF REQU-KDEMBTYP (LINE-IX) = ALL '+'                                 
150700        MOVE ERR-MUST-BE-ENTERED                                          
150800          TO RESP-IDMSG-ERROR-LINE (LINE-IX)                              
150900             RESP-IDMSG-ERROR                                             
151000        MOVE 'KDKOLLI'             TO RESP-IDELMT-ERROR                   
151100        MOVE NOO                   TO DATA-SW                             
151200     ELSE                                                                 
151300       INSPECT REQU-KDEMBTYP (LINE-IX) REPLACING                          
151400               LEADING SPACE BY ZERO                                      
151500       IF (REQU-KDEMBTYP (LINE-IX) NUMERIC)                               
151600       AND (REQU-KDEMBTYP (LINE-IX) > 0 AND < 8)                          
151700       AND (REQU-KDKOLLI (LINE-IX) = ALL '+')                             
151800          MOVE REQU-KDEMBTYP (LINE-IX) TO RESP-KDEMBTYP (LINE-IX)         
151900       ELSE                                                               
152000         MOVE ERR-NOT-NUMERIC  TO RESP-IDMSG-ERROR                        
152100                                  RESP-IDMSG-ERROR-LINE (LINE-IX)         
152200         MOVE 'KDEMBTYP'           TO RESP-IDELMT-ERROR                   
152300         MOVE NOO                  TO DATA-SW                             
152400       END-IF                                                             
152500     END-IF                                                               
152600     .                                                                    
152700     EJECT                                                                
152800 CCB-CHECK-KOLLI-DIMENSION     SECTION.                                   
152900                                                                          
153000     IF REQU-KDKOLLI  (LINE-IX) = ALL '+' AND                             
153100       (REQU-DIKOLLIL (LINE-IX) = (ALL '+' OR ZERO) OR                    
153200        REQU-DIKOLLIB (LINE-IX) = (ALL '+' OR ZERO) OR                    
153300        REQU-DIKOLLIH (LINE-IX) = (ALL '+' OR ZERO))                      
153400               MOVE NOO       TO DATA-SW                                  
153500               MOVE ERR-MORE-CASE-INFO-NEEDED                             
153600                              TO RESP-IDMSG-ERROR                         
153700     ELSE                                                                 
153800       INSPECT REQU-DIKOLLIL (LINE-IX) REPLACING                          
153900               LEADING SPACE BY ZERO                                      
154000       IF REQU-DIKOLLIL (LINE-IX) NUMERIC                                 
154100       AND REQU-DIKOLLIL (LINE-IX) > ZERO                                 
154200          MOVE REQU-DIKOLLIL (LINE-IX) TO RESP-DIKOLLIL (LINE-IX)         
154300                                          WS-DIKOLLIL                     
154400       ELSE                                                               
154500          MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR                        
154600                                  RESP-IDMSG-ERROR-LINE (LINE-IX)         
154700          MOVE 'DIKOLLIL'    TO RESP-IDELMT-ERROR                         
154800          MOVE NOO           TO DATA-SW                                   
154900       END-IF                                                             
155000                                                                          
155100       INSPECT REQU-DIKOLLIB (LINE-IX) REPLACING                          
155200               LEADING SPACE BY ZERO                                      
155300       IF REQU-DIKOLLIB (LINE-IX) NUMERIC                                 
155400       AND REQU-DIKOLLIB (LINE-IX) > ZERO                                 
155500           MOVE REQU-DIKOLLIB (LINE-IX) TO RESP-DIKOLLIB (LINE-IX)        
155600                                           WS-DIKOLLIB                    
155700       ELSE                                                               
155800          MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR                        
155900                                  RESP-IDMSG-ERROR-LINE (LINE-IX)         
156000          MOVE 'DIKOLLIB'    TO RESP-IDELMT-ERROR                         
156100          MOVE NOO           TO DATA-SW                                   
156200       END-IF                                                             
156300                                                                          
156400       INSPECT REQU-DIKOLLIH (LINE-IX) REPLACING                          
156500               LEADING SPACE BY ZERO                                      
156600       IF REQU-DIKOLLIH (LINE-IX) NUMERIC                                 
156700       AND REQU-DIKOLLIH (LINE-IX) > ZERO                                 
156800           MOVE REQU-DIKOLLIH (LINE-IX) TO RESP-DIKOLLIH (LINE-IX)        
156900                                           WS-DIKOLLIH                    
157000       ELSE                                                               
157100          MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR                        
157200                                  RESP-IDMSG-ERROR-LINE (LINE-IX)         
157300          MOVE 'DIKOLLIH'    TO RESP-IDELMT-ERROR                         
157400          MOVE NOO           TO DATA-SW                                   
157500       END-IF                                                             
157600     END-IF                                                               
157700                                                                          
157800     IF DATA-OK AND US-MEASUREMENT                                        
157900        COMPUTE WS-DIKOLLIL ROUNDED =                                     
158000                WS-DIKOLLIL * CONV-IN-TO-CM                               
158100        END-COMPUTE                                                       
158200        COMPUTE WS-DIKOLLIB ROUNDED =                                     
158300                WS-DIKOLLIB * CONV-IN-TO-CM                               
158400        END-COMPUTE                                                       
158500        COMPUTE WS-DIKOLLIH ROUNDED =                                     
158600                WS-DIKOLLIH * CONV-IN-TO-CM                               
158700        END-COMPUTE                                                       
158800     END-IF                                                               
158900                                                                          
159000     .                                                                    
159100     EJECT                                                                
159200 CD-CHECK-KVLEVART SECTION.                                               
159300                                                                          
159400     IF REQU-KVLEVART (LINE-IX) = ALL '+' OR LOW-VALUES                   
159500        CONTINUE                                                          
159600     ELSE                                                                 
159700        IF REQU-KVLEVART (LINE-IX) NUMERIC                                
159800           IF REQU-KVLEVART (LINE-IX) = ZERO                              
159900             MOVE ERR-ZERO-NOT-ALLOWED TO RESP-IDMSG-ERROR                
160000                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
160100             MOVE 'KVLEVART'           TO RESP-IDELMT-ERROR               
160200             MOVE NOO                  TO DATA-SW                         
160300           ELSE                                                           
160400           MOVE REQU-KVLEVART (LINE-IX) TO RESP-KVLEVART (LINE-IX)        
160500           END-IF                                                         
160600        ELSE                                                              
160700           MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
160800                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
160900           MOVE 'KVLEVART'      TO RESP-IDELMT-ERROR                      
161000           MOVE NOO             TO DATA-SW                                
161100        END-IF                                                            
161200     END-IF                                                               
161300     .                                                                    
161400     EJECT                                                                
161500 CE-CHECK-IDRADNR-IN-TABLE   SECTION.                                     
161600                                                                          
161700     MOVE 'CB-CHECK-IDRADNR-IN-TABLE'  TO WS-CURRENT-SECTION              
161800     MOVE SPACE     TO RESP-IDMSG-ERROR-LINE (LINE-IX)                    
161900                                                                          
162000     MOVE SAVE-WORK-IDRADNR-FOM TO CHECK-IDRADNR                          
162100     PERFORM UNTIL CHECK-IDRADNR > SAVE-WORK-IDRADNR-TOM                  
162200     OR DATA-WRONG                                                        
162300                                                                          
162400       MOVE 1 TO IDRAD-IX                                                 
162500       PERFORM UNTIL IDRAD-IX > TOP-IDRAD-IX                              
162600       OR CHECK-IDRADNR = TAB-IDRADNR (IDRAD-IX)                          
162700         ADD 1 TO IDRAD-IX                                                
162800       END-PERFORM                                                        
162900                                                                          
163000       MOVE CHECK-IDRADNR  TO W-IDPURAD-MIN                               
163100                              W-IDPURAD-MAX                               
163200       PERFORM IMS-GHNP-WDE411-FIRST                                      
163300                                                                          
163400       IF IDRAD-IX > TOP-IDRAD-IX                                         
163500       AND TOP-IDRAD-IX < MAX-IDRAD-IX                                    
163600*        -- INSERT ORDER LINE IN TABLE                                    
163700         MOVE IDRAD-IX TO TOP-IDRAD-IX                                    
163800         MOVE CHECK-IDRADNR TO TAB-IDRADNR (IDRAD-IX)                     
163900         IF SEGMENT-FOUND                                                 
164000           IF DEF-IDKOLLI-SW = JA                                         
164100             CONTINUE                                                     
164200           ELSE                                                           
164300             MOVE ORAD-KVLEVART TO TAB-KVLEVART (IDRAD-IX)                
164400             MOVE ORAD-KDRADSTA TO TAB-KDRADSTA (IDRAD-IX)                
164500           END-IF                                                         
164600         END-IF                                                           
164700       END-IF                                                             
164800                                                                          
164900       IF SEGMENT-FOUND                                                   
165000         IF REQU-KVLEVART(LINE-IX) = ALL '+'                              
165100         OR REQU-KVLEVART(LINE-IX) NOT NUMERIC                            
165200* IF YOU CHANGE ANYTHING IN THE COMPUTE SAME THING MUST BE CHANGED        
165300* IN THE HB-UPD-LINE-CASE SECTION AND ALSO CHECK HBA-UPD-E421 SECT        
165400*                                                                         
165500           IF ORAD-KVLEVART > ZERO                                        
165600*            * RAD TIDIGARE DELRAPPORTERAD, LEVERERAT I DENNA             
165700*            * OMGÅNG SÄTTS SÅ ATT AVBOKAT UPPNÅS.                        
165800             COMPUTE THIS-KVLEVART = ORAD-KVAVBART                        
165900                                 - TAB-KVLEVART (IDRAD-IX)                
166000             END-COMPUTE                                                  
166100           ELSE                                                           
166200             MOVE ORAD-KVAVBART TO THIS-KVLEVART                          
166300           END-IF                                                         
166400         ELSE                                                             
166500*TEST FOR NOT TO BE ABLE TO REPORT MORE THAN ORDERED.                     
166600*                                                                         
166700           MOVE REQU-KVLEVART (LINE-IX) TO THIS-KVLEVART                  
166800         END-IF                                                           
166900                                                                          
167000         ADD THIS-KVLEVART        TO TAB-KVLEVART (IDRAD-IX)              
167100                                                                          
167200         IF TAB-KDRADSTA (IDRAD-IX) = 4                                   
167300           MOVE ERR-LINE-REPORTED TO RESP-IDMSG-ERROR                     
167400                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
167500           MOVE 'IDRADNR'           TO RESP-IDELMT-ERROR                  
167600           MOVE NOO                 TO DATA-SW                            
167700         ELSE                                                             
167800           IF TAB-KVLEVART (IDRAD-IX) > ORAD-KVAVBART                     
167900             MOVE ERR-LARGE-QUANT TO RESP-IDMSG-ERROR                     
168000                                  RESP-IDMSG-ERROR-LINE (LINE-IX)         
168100             MOVE 'KVLEVART'        TO RESP-IDELMT-ERROR                  
168200             MOVE NOO               TO DATA-SW                            
168300           END-IF                                                         
168400         END-IF                                                           
168500         IF ORAD-KVAVBART = TAB-KVLEVART (IDRAD-IX)                       
168600           MOVE 4 TO TAB-KDRADSTA (IDRAD-IX)                              
168700         END-IF                                                           
168800*LK VOLUME CONTROL ***                                                    
168900         IF NOT DIST44-SCRAP-DIST                                         
169000           MOVE REQU-IDDC-KEY   TO VOL-IDDC                               
169100           IF CONTROL-OF-CASE-NET-VOLUME                                  
169200             COMPUTE WS-KOLLI-VLORDBTO ROUNDED =                          
169300                     WS-DIKOLLIL * WS-DIKOLLIB * WS-DIKOLLIH              
169400             END-COMPUTE                                                  
169500                                                                          
169600             COMPUTE WS-ORAD-VLORDNTO ROUNDED =                           
169700                     ORAD-VLARTNTO * TAB-KVLEVART (IDRAD-IX)              
169800             END-COMPUTE                                                  
169900                                                                          
170000             ADD WS-ORAD-VLORDNTO TO WS-ORAD-VLORDNTO-SUM                 
170100              IF WS-ORAD-VLORDNTO-SUM > WS-KOLLI-VLORDBTO                 
170200                                                                          
170300                MOVE NOO        TO DATA-SW                                
170400                MOVE '117'      TO RESP-IDMSG-ERROR                       
170500                MOVE 'VLART'    TO RESP-IDELMT-ERROR                      
170600                MOVE 'VOL'      TO RESP-IDMSG-ERROR-LINE (LINE-IX)        
170700              END-IF                                                      
170800           END-IF                                                         
170900         END-IF                                                           
171000       ELSE                                                               
171100         MOVE ERR-LINE-WRONG    TO RESP-IDMSG-ERROR                       
171200                                   RESP-IDMSG-ERROR-LINE (LINE-IX)        
171300         MOVE 'IDRADNR'         TO RESP-IDELMT-ERROR                      
171400         MOVE NOO               TO DATA-SW                                
171500       END-IF                                                             
171600       ADD 1 TO CHECK-IDRADNR                                             
171700     END-PERFORM                                                          
171800     MOVE ZERO                  TO WS-ORAD-VLORDNTO-SUM                   
171900     .                                                                    
172000     EJECT                                                                
172100* VALIDATE THE COUNTRY OF ORIGIN INPUT.VALID ONLY FROM DEVICES.           
172200* NOT A MANDATORY INPUT                                                   
172300 CF-VALIDATE-KDARTURS SECTION.                                            
172400     MOVE 'CF-VALIDATE-KDARTURS'       TO WS-CURRENT-SECTION              
172500                                                                          
172600     IF REQU-KDARTURS (LINE-IX) = ALL '+' OR                              
172700        REQU-KDARTURS (LINE-IX) = ALL SPACES OR                           
172800        REQU-KDARTURS (LINE-IX) = LOW-VALUES                              
172900        MOVE SPACES TO REQU-KDARTURS (LINE-IX)                            
173000       CONTINUE                                                           
173100     ELSE                                                                 
173200       MOVE REQU-KDARTURS (LINE-IX)    TO ARTU-KDARTURS                   
173300       MOVE WS-IDDISTR             TO ARTU-IDDISTR                        
173400       MOVE WS-IDDC                TO ARTU-IDDC                           
173500       CALL W400ARTU USING ARTU-W400ARTU                                  
173600       IF ARTU-KDARTURS-NUM NUMERIC                                       
173700          CONTINUE                                                        
173800       ELSE                                                               
173900          MOVE ERR-INVALID          TO RESP-IDMSG-ERROR                   
174000                               RESP-IDMSG-ERROR-LINE (LINE-IX)            
174100          MOVE 'KDARTURS'           TO RESP-IDELMT-ERROR                  
174200          MOVE NOO                  TO DATA-SW                            
174300       END-IF                                                             
174400     END-IF                                                               
174500                                                                          
174600     .                                                                    
174700     EJECT                                                                
174800 D-BACKOUT-DEF-CASE SECTION.                                              
174900     MOVE 'D-SECTION'   TO WS-CURRENT-SECTION                             
175000*--- DELETE DEFAULT CASE (KDKOLSTA=0) IF THERE IS ANY                     
175100*--- DEFAULT KOLLI FINNS BARA EN PER ORDERDEL SKAPAT PÅ L138.             
175200*--- VID FÖRSTA AVVIKELSE RAPP. PÅ L199 TAS DEN BORT OCH                  
175300*--- DÄREFTER KAN DET INTE FINNAS LÄNGRE DEFAULT KOLLI                    
175400                                                                          
175500     MOVE ZERO          TO W-IDKOLLI-MIN                                  
175600     MOVE 99999         TO W-IDKOLLI-MAX                                  
175700     PERFORM IMS-GU-WDE4F1-PLK                                            
175800     IF SEGMENT-FOUND                                                     
175900       MOVE SEQF-IDKOLLI  TO W-IDKOLLI-E6                                 
176000       PERFORM IMS-GHU-WDE611-DEF                                         
176100       IF SEGMENT-FOUND                                                   
176200* KDKOLSTA CAN BE 0 FOR PACKED CASES FROM API                             
176300* SO ADDED EXTRA DIKOLLI CHECK                                            
176400       IF KOLLI-DIKOLLIL = ZERO                                           
176500         PERFORM IMS-DLET-WDE611                                          
176600         INITIALIZE KOLLI-WDE611                                          
176700         PERFORM IMS-GHU-WDE601                                           
176800         SUBTRACT 1 FROM VORD-KVKOLLI                                     
176900                                                                          
177000         COMPUTE VORD-VLORDBTO ROUNDED = VORD-VLORDBTO                    
177100                               - KOLLI-VLORDBTO-KOLLI                     
177200         END-COMPUTE                                                      
177300                                                                          
177400         COMPUTE VORD-VKORDNTO ROUNDED = VORD-VKORDNTO                    
177500                               - KOLLI-VKORDNTO-KOLLI                     
177600         END-COMPUTE                                                      
177700                                                                          
177800         PERFORM IMS-REPL-WDE601                                          
177900                                                                          
178000*-----WDE401 REDAN LÄST I B-SECTION                                       
178100         MOVE ZERO         TO W-IDPURAD-MIN                               
178200         MOVE 99999        TO W-IDPURAD-MAX                               
178300         PERFORM IMS-GHNP-WDE411-FIRST                                    
178400         PERFORM UNTIL SEGMENT-MISSING                                    
178500           MOVE ZERO             TO ORAD-KVLEVART                         
178600           MOVE ORAD-IDARTNR     TO WS-DNOT-IDARTNR                       
178700           MOVE ORAD-IDPURAD     TO WS-DNOT-IDPURAD                       
178800           PERFORM IMS-REPL-WDE411                                        
178900                                                                          
179000           PERFORM IMS-GHNP-WDE421                                        
179100           MOVE KKOLLI-IDKOLLI  TO WS-DNOT-IDKOLLI                        
179200                                                                          
179300           IF NDC-US OR NDC-CA                                            
179400              PERFORM DA-DATA-TILL-DEL-NOTE                               
179500           END-IF                                                         
179600           PERFORM IMS-DLET-WDE421                                        
179700                                                                          
179800           PERFORM IMS-GHNP-WDE411                                        
179900         END-PERFORM                                                      
180000       END-IF                                                             
180100       END-IF                                                             
180200     END-IF                                                               
180300     .                                                                    
180400     EJECT                                                                
180500                                                                          
180600 DA-DATA-TILL-DEL-NOTE SECTION.                                           
180700     MOVE 'DA-DATA-TILL-DEL-NOTE'   TO WS-CURRENT-SECTION                 
180800                                                                          
180900     MOVE VORD-IDDISTR          TO TEST-IDDISTR                           
181000     IF DIST07-USA-RETAILER-DNOTE                                         
181100     OR DIST07-CAN-RETAILER                                               
181200                                                                          
181300        INITIALIZE DNOT-ORDER-INFO                                        
181400                                                                          
181500        MOVE 'WL0199DE'               TO DNOT-IDPGM                       
181600        MOVE WS-DNOT-IDORDER          TO DNOT-IDORDER                     
181700        MOVE WS-DNOT-IDARTNR          TO DNOT-IDARTNR                     
181800        MOVE WS-DNOT-IDDC             TO DNOT-IDDC                        
181900        MOVE WS-DNOT-IDKOLLI          TO DNOT-IDKOLLI-BORT                
182000        MOVE WS-DNOT-IDPURAD          TO DNOT-IDPURAD                     
182100                                                                          
182200        CALL W411DNOT USING DNOT-W411DNOT                                 
182300                            DNOT-ORQP-PCB                                 
182400                            DNOT-ORQP2-PCB                                
182500                            DNOT-ORQP3-PCB                                
182600                            DNOT-4013-PCB                                 
182700                            DNOT-BENA-PCB                                 
182800     END-IF                                                               
182900     .                                                                    
183000     EJECT                                                                
183100                                                                          
183200 H-UPDATE SECTION.                                                        
183300     MOVE 'H-UPDATE'   TO WS-CURRENT-SECTION                              
183400                                                                          
183500     PERFORM S07-CALL-W403PLAT                                            
183510     PERFORM S22-OPEN-W403WHEV                                            
183600* --- UPDATE NEW CASE(ES)                                                 
183700     MOVE +1 TO LINE-IX                                                   
183800*    MOVE REQU-RESTART-IDRADNR  TO LINE-IX                                
183900                                                                          
184000     PERFORM UNTIL REQU-IDRADNR-FOM (LINE-IX) = ALL '+'                   
184100     OR REQU-IDRADNR-FOM (LINE-IX) <= SPACES                              
184200*    OR RESTART-ME                                                        
184300                                                                          
184400       MOVE REQU-IDRADNR-FOM (LINE-IX) TO W-IDPURAD-MIN                   
184500                                                                          
184600       IF REQU-IDRADNR-TOM (LINE-IX) = ALL '+'                            
184700*      REQU-IDRADNR-TOM (LINE-IX) = 0                                     
184800         MOVE REQU-IDRADNR-FOM (LINE-IX) TO W-IDPURAD-MAX                 
184900       ELSE                                                               
185000         MOVE REQU-IDRADNR-TOM (LINE-IX) TO W-IDPURAD-MAX                 
185100       END-IF                                                             
185200                                                                          
185300       PERFORM IMS-GHNP-WDE40111-FIRST                                    
185400                                                                          
185500       PERFORM UNTIL SEGMENT-MISSING                                      
185600         IF ORAD-KDRADSTA < 4                                             
185700           PERFORM HB-UPD-LINE-CASE                                       
185800           PERFORM HC-INIT-E611                                           
185820           PERFORM S23-PUT-W403WHEV                                       
185900         END-IF                                                           
186000                                                                          
186100         PERFORM IMS-GHNP-WDE40111                                        
186200       END-PERFORM                                                        
186300       MOVE ZERO                TO WS-KKOLLI-KVLEVART                     
186400                                                                          
186600       ADD +1 TO LINE-IX                                                  
186700*      IF UPDATE-IX > MAX-UPD-IX   AND                                    
186800*         (REQU-IDRADNR-FOM (LINE-IX) NOT = ALL '+')                      
186900*        MOVE YES TO RESTART-ME-SW                                        
187000*      END-IF                                                             
187100     END-PERFORM                                                          
187200     PERFORM S24-CLOSE-W403WHEV                                           
187300*TMS PACKING INFO                                                         
187400     IF SUB-KDTRANS(1:6) NOT = 'WLA199'                                   
187500       IF TMS-IDKOLLI(1) > 0                                              
187600         MOVE KORD-IDDC          TO TMS-IDDC                              
187700         MOVE KORD-IDDISTR       TO TMS-IDDISTR                           
187800         MOVE KORD-IDKUNDNR      TO TMS-IDKUNDNR                          
187900         MOVE KORD-IDORDNR5      TO TMS-IDORDNR7                          
188000         CALL W403TMS1 USING TMS-W403TMS1                                 
188100                 TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                         
188200                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
188300                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
188400                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
188500                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
188600                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
188700       END-IF                                                             
188800     END-IF                                                               
188900     .                                                                    
189000     EJECT                                                                
189100                                                                          
189200 HB-UPD-LINE-CASE     SECTION.                                            
189300     MOVE 'HB-UPD-LINE-CASE'    TO WS-CURRENT-SECTION                     
189400                                                                          
189500     COMPUTE WS-KVLEVART-REST (LINE-IX) = ORAD-KVAVBART                   
189600                                    - ORAD-KVLEVART                       
189700     MOVE ORAD-KVLEVART       TO WS-ORAD-KVLEVART                         
189800                                                                          
189900     IF REQU-KVLEVART (LINE-IX) NOT = ALL '+'                             
190000       ADD REQU-KVLEVART (LINE-IX) TO ORAD-KVLEVART                       
190010       MOVE REQU-KVLEVART (LINE-IX) TO WS-WHEV-KVLEVART                   
190100     ELSE                                                                 
190200       ADD WS-KVLEVART-REST (LINE-IX) TO ORAD-KVLEVART                    
190210       MOVE WS-KVLEVART-REST (LINE-IX) TO WS-WHEV-KVLEVART                
190300     END-IF                                                               
190400                                                                          
190500     IF ORAD-IDLEVNR NOT = SPACE                                          
190600       MOVE JA                  TO DIRLEV-KOLLI-SW                        
190700     END-IF                                                               
190800                                                                          
190900     IF ORAD-KVLEVART = ORAD-KVAVBART                                     
191000       MOVE +4           TO ORAD-KDRADSTA                                 
191100       MOVE 'Y'          TO DATUM-SW                                      
191200                                                                          
191300       ADD +1            TO WS-TOT-LINES                                  
191400                            WS-4472-PACK-LINES                            
191500                            WORK-NO-OF-LINES                              
191600       IF KORD-KDORDKL = +0                                               
191700          PERFORM HBB-UPPDATERA-VOR-TIKLAR                                
191800       END-IF                                                             
191900     END-IF                                                               
192000                                                                          
192100     IF DATUM-SW = 'Y'                                                    
192200       MOVE DAGENS-DATUM TO 4011-KORD-TIBEGPAC                            
192300       MOVE 'N'          TO DATUM-SW                                      
192400     END-IF                                                               
192500                                                                          
192600     IF SUB-KDTRANS(1:6) = 'WLA199' AND                                   
192700        REQU-KDARTURS (LINE-IX) > SPACES                                  
192800        MOVE REQU-KDARTURS (LINE-IX) TO  ORAD-KDARTURS                    
192900     END-IF                                                               
193000                                                                          
193100*    MOVE ORAD-BEART               TO WS-ORAD-BEART                       
193200     PERFORM IMS-REPL-WDE40111                                            
193300                                                                          
193400     IF ORAD-KDFARLIG = +4 OR ORAD-KDFARLIG = +7                          
193500       ADD +1  TO WORK-KOLLI-KVFALRAD                                     
193600     END-IF                                                               
193700                                                                          
193800     PERFORM HBA-UPD-E421                                                 
193900                                                                          
194000     MOVE ODEL-IDDC              TO WS-IDDC                               
194100*                                                                         
194200     IF NDC-US OR NDC-CA                                                  
194300       PERFORM HBC-DELIVERY-NOTE                                          
194400     END-IF                                                               
194500                                                                          
194600     PERFORM S08-CREATE-RYK                                               
194700     .                                                                    
194800     EJECT                                                                
194900 HBA-UPD-E421   SECTION.                                                  
195000     MOVE 'HBA-UPD-E421'   TO WS-CURRENT-SECTION                          
195100                                                                          
195200     IF REQU-KDKOLLI (LINE-IX) NOT = ALL '+'                              
195300        MOVE REQU-KDKOLLI (LINE-IX) TO W-KDKOLLI-K5                       
195400        PERFORM IMS-GU-WDK5                                               
195500        MOVE EMB-KDKOLLI          TO RESP-KDKOLLI (LINE-IX)               
195600        MOVE EMB-VKTARA           TO WORK-EMB-TARE-ONE-CASE               
195700        MOVE EMB-VKTARA           TO WS-VKTARA                            
195800* OM EMB-SEGMENT FINNS MEN NÅGON AV DIMENSIONERNA ÄR NOLL                 
195900* HAMTAS VÄRDEN FRÅN SKÄRMEN.                                             
196000* ATT VÄRDEN FINNS TESTAAS I CC-CHECK-KDKOLLI                             
196100     ELSE                                                                 
196200        MOVE SPACE                TO RESP-KDKOLLI (LINE-IX)               
196300        MOVE ZERO                 TO WORK-EMB-TARE-ONE-CASE               
196400     END-IF                                                               
196500                                                                          
196600        COMPUTE WORK-EMB-VOL-ONE-CASE ROUNDED =                           
196700        TAB-DIKOLLIL (LINE-IX)  * TAB-DIKOLLIB (LINE-IX)  *               
196800        TAB-DIKOLLIH (LINE-IX)  / 1000000                                 
196900                                                                          
197000     IF REQU-IDKOLLI (LINE-IX) NOT = ALL '+'                              
197100       MOVE REQU-IDKOLLI (LINE-IX) TO SAVE-IDKOLLI                        
197200     END-IF                                                               
197300                                                                          
197400     MOVE REQU-IDPRODNR-KEY       TO KKOLLI-IDPRODNR                      
197500     MOVE SAVE-IDKOLLI            TO KKOLLI-IDKOLLI                       
197600                                     W-IDKOLLI-E421                       
197700                                     TMS-IDKOLLI(LINE-IX)                 
197800     MOVE ORAD-IDPURAD  TO W-IDPURAD                                      
197900     PERFORM IMS-GHNP-WDE421-KVAL                                         
198000                                                                          
198100     IF SEGMENT-FOUND                                                     
198200       IF REQU-KVLEVART (LINE-IX) NOT = ALL '+'                           
198300         ADD REQU-KVLEVART (LINE-IX) TO KKOLLI-KVLEVART                   
198400                                    WORK-KOLLI-KVLEVART                   
198500       ELSE                                                               
198600         IF WS-ORAD-KVLEVART > ZERO                                       
198700*          * RAD TIDIGARE DELRAPPORTERAD, LEVERERAT I DENNA               
198800*          * OMGÅNG SÄTTS SÅ ATT AVBOKAT UPPNÅS.                          
198900           COMPUTE WORK-KOLLI-KVLEVART =                                  
199000                   WS-KVLEVART-REST (LINE-IX) + WS-ORAD-KVLEVART          
199100           END-COMPUTE                                                    
199200                                                                          
199300           ADD  WS-KVLEVART-REST (LINE-IX) TO KKOLLI-KVLEVART             
199400         ELSE                                                             
199500           MOVE ORAD-KVAVBART      TO WORK-KOLLI-KVLEVART                 
199600           MOVE ORAD-KVAVBART      TO KKOLLI-KVLEVART                     
199700         END-IF                                                           
199800       END-IF                                                             
199900       PERFORM IMS-REPL-WDE421                                            
200000     ELSE                                                                 
200100       IF REQU-KVLEVART (LINE-IX) NOT = ALL '+'                           
200200         MOVE REQU-KVLEVART (LINE-IX) TO KKOLLI-KVLEVART                  
200300                                       WORK-KOLLI-KVLEVART                
200400       ELSE                                                               
200500         IF WS-ORAD-KVLEVART > ZERO                                       
200600*          * RAD TIDIGARE DELRAPPORTERAD, LEVERERAT I DENNA               
200700*          * OMGÅNG SÄTTS SÅ ATT AVBOKAT UPPNÅS.                          
200800           COMPUTE WORK-KOLLI-KVLEVART =                                  
200900                   WS-KVLEVART-REST (LINE-IX) + WS-ORAD-KVLEVART          
201000           END-COMPUTE                                                    
201100                                                                          
201200           MOVE WS-KVLEVART-REST (LINE-IX) TO KKOLLI-KVLEVART             
201300         ELSE                                                             
201400           MOVE ORAD-KVAVBART       TO WORK-KOLLI-KVLEVART                
201500           MOVE ORAD-KVAVBART       TO KKOLLI-KVLEVART                    
201600         END-IF                                                           
201700       END-IF                                                             
201800       PERFORM IMS-ISRT-WDE421                                            
201900       ADD +1 TO WORK-KOLLI-KVORDRAD                                      
202000     END-IF                                                               
202100     .                                                                    
202200     EJECT                                                                
202300 HBB-UPPDATERA-VOR-TIKLAR SECTION.                                        
202400     MOVE 'HBB-UPD-TIKLAR' TO WS-CURRENT-SECTION                          
202500                                                                          
202600     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
202700     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
202800     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
202900                                    W-A601KY-MAX-IDDISTR                  
203000     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
203100                                    W-A601KY-MAX-IDKUNDNR                 
203200     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
203300                                    W-A601KY-MAX-IDORDNR                  
203400                                                                          
203500     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
203600                                                                          
203700     PERFORM IMS-GHN-WDA6B                                                
203800     PERFORM UNTIL SEGMENT-MISSING                                        
203900                OR END-OF-DATABASE                                        
204000                OR SW-TIKLAR-UPPDATERAD = JA                              
204100                                                                          
204200         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
204300         AND VOR-TIKLAR = +0                                              
204400                                                                          
204500             MOVE DAGENS-DATUM      TO VOR-TIKLAR                         
204600             MOVE DAGENS-TID-HHMMSS TO VOR-TIKLATID                       
204700             PERFORM IMS-REPL-WDA6B                                       
204800             MOVE JA                TO SW-TIKLAR-UPPDATERAD               
204900         END-IF                                                           
205000                                                                          
205100         PERFORM IMS-GHN-WDA6B                                            
205200     END-PERFORM                                                          
205300     .                                                                    
205400     EJECT                                                                
205500                                                                          
205600 HBC-DELIVERY-NOTE SECTION.                                               
205700     MOVE 'HBC-DELIVERY-NOTE'  TO WS-CURRENT-SECTION                      
205800*                                                                         
205900     MOVE ODEL-IDDISTR             TO TEST-IDDISTR                        
206000     IF DIST07-USA-RETAILER-DNOTE                                         
206100     OR DIST07-CAN-RETAILER                                               
206200                                                                          
206300        INITIALIZE DNOT-ORDER-INFO                                        
206400        MOVE 'WL019900'               TO DNOT-IDPGM                       
206500        MOVE ODEL-IDORDER             TO DNOT-IDORDER                     
206600        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
206700        MOVE REQU-IDDC-KEY            TO DNOT-IDDC                        
206800        MOVE WS-ORAD-BEART            TO DNOT-BEART-USA                   
206900        MOVE ORAD-KVBEART             TO DNOT-KVBEART                     
207000        MOVE ORAD-FLTILLK             TO DNOT-FLTILLK                     
207100        MOVE ORAD-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                 
207200        MOVE ORAD-IDPURAD             TO DNOT-IDPURAD                     
207300        MOVE SAVE-IDKOLLI             TO DNOT-IDKOLLI                     
207400        MOVE REQU-IDPRODNR-KEY        TO DNOT-IDPRODNR                    
207500        MOVE KKOLLI-KVLEVART          TO DNOT-KVLEVART                    
207600                                                                          
207700        CALL W411DNOT USING DNOT-W411DNOT                                 
207800                            DNOT-ORQP-PCB                                 
207900                            DNOT-ORQP2-PCB                                
208000                            DNOT-ORQP3-PCB                                
208100                            DNOT-4013-PCB                                 
208200                            DNOT-BENA-PCB                                 
208300     END-IF                                                               
208400     .                                                                    
208500     EJECT                                                                
208600                                                                          
208700 HC-INIT-E611  SECTION.                                                   
208800     MOVE 'HC-INIT-E611' TO WS-CURRENT-SECTION                            
208900                                                                          
209000     PERFORM IMS-GHU-WDE601                                               
209100     IF SEGMENT-FOUND                                                     
209200       MOVE SAVE-IDKOLLI              TO KOLLI-IDKOLLI                    
209300                                         W-IDKOLLI-E6                     
209400                                                                          
209500       PERFORM IMS-GHNP-WDE611                                            
209600       PERFORM HCA-CALC-E611                                              
209700       IF SEGMENT-FOUND-E611                                              
209800         IF ORAD-IDPSN > 0                                                
209900            PERFORM S03-INS-DG-FIELDS                                     
210000         END-IF                                                           
210100         IF ORAD-KDFARLIG = +4                                            
210200         OR ORAD-KDFARLIG = +7                                            
210300           MOVE +950                  TO KOLLI-ADFLOMR                    
210400         END-IF                                                           
210500         ADD 1                        TO KOLLI-KVORDRAD                   
210600         ADD WORK-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                   
210700         IF WORK-KOLLI-SUORDV-LOCPREL > ZERO                              
210800           ADD WORK-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL          
210900         ELSE                                                             
211000           IF WORK-KOLLI-SUORDV-LOC > ZERO                                
211100             ADD WORK-KOLLI-SUORDV-LOC TO KOLLI-SUORDV-LOC                
211200           ELSE                                                           
211300             IF WORK-KOLLI-SUORDV-EXP > ZERO AND                          
211400                ORAD-PRAVCOST > ZERO                                      
211500                ADD WORK-KOLLI-SUORDV-EXP TO KOLLI-SUORDV-KLI-EXP         
211600             END-IF                                                       
211700             IF WORK-KOLLI-SUORDV > ZERO AND                              
211800                ORAD-PRARTNTO > ZERO                                      
211900                ADD WORK-KOLLI-SUORDV     TO KOLLI-SUORDV-KOLLI           
212000             END-IF                                                       
212100           END-IF                                                         
212200         END-IF                                                           
212300         ADD WORK-KOLLI-VKORDBTO      TO KOLLI-VKORDBTO-KOLLI             
212400         ADD WORK-KOLLI-VLORDBTO      TO KOLLI-VLORDBTO-KOLLI             
212500         IF WORK-KOLLI-VKORDNTO > WORK-KOLLI-VKORDBTO                     
212600           ADD WORK-KOLLI-VKORDBTO    TO KOLLI-VKORDNTO-KOLLI             
212700         ELSE                                                             
212800           ADD WORK-KOLLI-VKORDNTO    TO KOLLI-VKORDNTO-KOLLI             
212900         END-IF                                                           
213000*                                                                         
213100         IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                  
213200           ADD 0.1                    TO KOLLI-VKORDBTO-KOLLI             
213300         END-IF                                                           
213400                                                                          
213500         IF KOLLI-VKORDBTO-KOLLI - KOLLI-VKORDNTO-KOLLI < 0.2             
213600           ADD 0.1                    TO KOLLI-VKORDBTO-KOLLI             
213700         END-IF                                                           
213800                                                                          
213900         PERFORM IMS-REPL-WDE611                                          
214000         MOVE ZERO                    TO WORK-KOLLI-KVORDRAD              
214100                                         WORK-KOLLI-VKORDNTO              
214200                                         WORK-KOLLI-VKORDBTO              
214300                                         WORK-KOLLI-VLORDBTO              
214400                                         WORK-KOLLI-SUORDV-LOC            
214500                                         WORK-KOLLI-SUORDV-LOCPREL        
214600                                         WORK-KOLLI-SUORDV                
214700                                         WORK-KOLLI-SUORDV-EXP            
214800       ELSE                                                               
214900         MOVE ZERO                    TO KOLLI-IDKOLLI-FLER               
215000         MOVE REQU-IDDC-KEY           TO KOLLI-IDDC                       
215100         MOVE WS-IDTRPTNR             TO KOLLI-IDTRPTNR                   
215200         MOVE WS-ADFLGEO              TO KOLLI-ADFLGEO                    
215300         MOVE WS-ADFLOMR              TO KOLLI-ADFLOMR                    
215400         MOVE WS-ADRUTNIV             TO KOLLI-ADRUTNIV                   
215500         MOVE WS-ADVMODUL             TO KOLLI-ADVMODUL                   
215600         MOVE WS-ADHMODUL             TO KOLLI-ADHMODUL                   
215700         MOVE WS-DIHMODUL             TO KOLLI-DIHMODUL                   
215800         MOVE WS-DIDMODUL             TO KOLLI-DIDMODUL                   
215900         MOVE WS-FLUTLAST             TO KOLLI-FLUTLAST                   
216000         MOVE NOO                     TO KOLLI-FLBANDST                   
216010         MOVE SPACE                   TO KOLLI-FILLERX2                   
216100         IF REQU-KDKOLLI (LINE-IX) NOT = ALL '+'                          
216200           MOVE EMB-KDEMBTYP           TO KOLLI-KDEMBTYP                  
216300         ELSE                                                             
216400           MOVE REQU-KDEMBTYP (LINE-IX) TO KOLLI-KDEMBTYP                 
216500         END-IF                                                           
216600                                                                          
216700         MOVE TAB-DIKOLLIL (LINE-IX)  TO KOLLI-DIKOLLIL                   
216800         MOVE TAB-DIKOLLIB (LINE-IX)  TO KOLLI-DIKOLLIB                   
216900         MOVE TAB-DIKOLLIH (LINE-IX)  TO KOLLI-DIKOLLIH                   
217000                                                                          
217100         MOVE NOO                     TO KOLLI-FLTULLG                    
217200         MOVE ZERO                    TO KOLLI-IDFAKLOP                   
217300                                         KOLLI-IDFAKT                     
217400                                         KOLLI-IDFAKT-EXP                 
217500         MOVE SPACE                   TO KOLLI-IDLBBET                    
217600         MOVE ODEL-IDUSER             TO KOLLI-IDPLOCK                    
217700         MOVE ZERO                    TO KOLLI-KDARTURS-KOLLI             
217800         IF REQU-KDKOLLI (LINE-IX) = ALL '+'                              
217900            MOVE SPACE                TO KOLLI-KDKOLLI                    
218000         ELSE                                                             
218100            MOVE REQU-KDKOLLI (LINE-IX) TO KOLLI-KDKOLLI                  
218200         END-IF                                                           
218300         IF SUB-KDTRANS(1:6) = 'WLA199'                                   
218400            MOVE 0                    TO KOLLI-KDKOLSTA                   
218500         ELSE                                                             
218600            MOVE 1                    TO KOLLI-KDKOLSTA                   
218700         END-IF                                                           
218800         MOVE ZERO                    TO KOLLI-TIAAVVD-PATR               
218900         MOVE ZERO                    TO KOLLI-TIFAKT                     
219000         MOVE ZERO                    TO KOLLI-TIFAKT-EXP                 
219100         MOVE ZERO                    TO KOLLI-TIFAKTID                   
219200         MOVE ZERO                    TO KOLLI-TIFAKTID-EXP               
219300         MOVE ZERO                    TO KOLLI-TILASTN                    
219400         MOVE ZERO                    TO KOLLI-TILASTID                   
219500         MOVE DAGENS-DATUM            TO KOLLI-TIPACKN                    
219600         MOVE DAGENS-TID(1:6)         TO KOLLI-TIPACTID                   
219700         MOVE KORD-IDKUNDNR           TO KOLLI-IDKUNDNR                   
219800         MOVE ORAD-KDFARLIG           TO KOLLI-KDFARLIG-KOLLI             
219900         MOVE KORD-IDDISTR            TO KOLLI-IDDISTR                    
220000         MOVE VORD-KDORDKL            TO KOLLI-KDORDKL                    
220100         MOVE VORD-FLAUTFAK           TO KOLLI-FLAUTFAK                   
220200         MOVE ZERO                    TO KOLLI-IDLASTN                    
220300         MOVE NOO                     TO KOLLI-FLFRSUTS                   
220400*                                                                         
220500*THE BELOW INITIALIZATION PREVENTS THE IDPSN VALUES TO BE                 
220600*COPIED FROM ONE CASE TO NEXT NEW CASE.                                   
220700         MOVE ZERO                 TO KOLLI-SUEQFG                        
220800         MOVE +1                   TO IDPSN-IX                            
220900         PERFORM UNTIL IDPSN-IX > MAX-DG-IX                               
221000            MOVE 0                 TO KOLLI-IDPSN    (IDPSN-IX)           
221100                                      KOLLI-VKART-FG (IDPSN-IX)           
221200                                      KOLLI-VLFG     (IDPSN-IX)           
221300            ADD +1                 TO IDPSN-IX                            
221400         END-PERFORM                                                      
221500         IF ORAD-IDPSN > 0                                                
221600            PERFORM S03-INS-DG-FIELDS                                     
221700         END-IF                                                           
221800*                                                                         
221900         MOVE VORD-DARFS              TO KOLLI-DARFS                      
222000         MOVE ZERO                    TO KOLLI-IDKOLLI-SAMP               
222100         MOVE ODEL-IDTRP              TO KOLLI-IDTRP                      
222200         MOVE SPACE                   TO KOLLI-IDSUPREF                   
222300         MOVE ZERO                    TO KOLLI-DASUPREF                   
222400         MOVE SPACE                   TO KOLLI-IDLEVNR                    
222500         MOVE ZERO                    TO KOLLI-KDVIA                      
222600         MOVE ZERO                    TO KOLLI-TISUPTID                   
222700         MOVE SPACE                   TO KOLLI-IDTULFTG                   
222800         MOVE ZERO                    TO KOLLI-IDTULLNR                   
222900         MOVE ZERO                    TO KOLLI-RETULKS                    
223000         MOVE ORAD-KDVALISO           TO KOLLI-KDVALISO                   
223100         MOVE ORAD-KDVALISO-EXP       TO KOLLI-KDVALISO-EXP               
223200         MOVE ZERO                    TO KOLLI-IDSHIPM                    
223300         MOVE SPACE                   TO KOLLI-KDSTASKLI                  
223400         IF KOLLI-KDFARLIG-KOLLI = +4                                     
223500         OR KOLLI-KDFARLIG-KOLLI = +7                                     
223600           MOVE +950                  TO KOLLI-ADFLOMR                    
223700         END-IF                                                           
223800         MOVE 1                       TO KOLLI-KVORDRAD                   
223900         MOVE WORK-KOLLI-KVFLAMP      TO KOLLI-KVFLAMP-KOLLI              
224000*WEIGHT                                                                   
224100         IF WORK-KOLLI-VKORDNTO > WORK-KOLLI-VKORDBTO                     
224200           MOVE WORK-KOLLI-VKORDBTO   TO KOLLI-VKORDNTO-KOLLI             
224300         ELSE                                                             
224400           MOVE WORK-KOLLI-VKORDNTO   TO KOLLI-VKORDNTO-KOLLI             
224500         END-IF                                                           
224600         MOVE WORK-KOLLI-VKORDBTO     TO KOLLI-VKORDBTO-KOLLI             
224700*WEIGHT                                                                   
224800         IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                  
224900           ADD 0.1                    TO KOLLI-VKORDBTO-KOLLI             
225000         END-IF                                                           
225100                                                                          
225200         IF KOLLI-VKORDBTO-KOLLI - KOLLI-VKORDNTO-KOLLI < 0.2             
225300           ADD 0.1                    TO KOLLI-VKORDBTO-KOLLI             
225400         END-IF                                                           
225500*VOLUME                                                                   
225600         MOVE WORK-KOLLI-VLORDBTO     TO KOLLI-VLORDBTO-KOLLI             
225700*                                                                         
225800*                                                                         
225900* NU TAR VI OCH RÄKNAR OM VOLYMEN                                         
226000* TIDIGARE UTRÄKNING I PROGRAMMET ÄR INTE HELT OK...                      
226100         COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                           
226200        KOLLI-DIKOLLIL * KOLLI-DIKOLLIB * KOLLI-DIKOLLIH / 1000000        
226300* OCH SÅ HOPPAS VI ATT ALLT BLIR BÄTTRE...                                
226400*                                                                         
226500*2278 GLOBAL EXPORT                                                       
226600*                                                                         
226700         MOVE WORK-KOLLI-SUORDV-EXP   TO KOLLI-SUORDV-KLI-EXP             
226800         MOVE WORK-KOLLI-SUORDV       TO KOLLI-SUORDV-KOLLI               
226900         MOVE WORK-KOLLI-SUORDV-LOC   TO KOLLI-SUORDV-LOC                 
227000         MOVE WORK-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL           
227100         IF WORK-KOLLI-KVFALRAD > ZERO                                    
227200           MOVE WORK-KOLLI-KVFALRAD   TO KOLLI-KVFALRAD                   
227300         ELSE                                                             
227400           MOVE ZERO                  TO KOLLI-KVFALRAD                   
227500         END-IF                                                           
227600         IF WORK-KOLLI-KVFLAMP > ZERO                                     
227700         AND (WORK-KOLLI-KVFLAMP < KOLLI-KVFLAMP-KOLLI                    
227800         OR KOLLI-KVFLAMP-KOLLI = ZERO)                                   
227900           MOVE WORK-KOLLI-KVFLAMP    TO KOLLI-KVFLAMP-KOLLI              
228000         END-IF                                                           
228100                                                                          
228200         PERFORM IMS-ISRT-WDE611                                          
228300         IF WS-IDDC-CROSS > SPACES                                        
228400           MOVE W-KDSEGKEY-X         TO CROSS-KDSEGKEY                    
228500           MOVE PLATS-IDDC           TO CROSS-IDDC-SEND                   
228600           MOVE WS-IDDC-CROSS        TO CROSS-IDDC-CROSS                  
228700           MOVE KOLLI-IDDISTR        TO CROSS-IDDISTR                     
228800           MOVE KOLLI-IDKUNDNR       TO CROSS-IDKUNDNR                    
228900           MOVE VORD-IDPRODNR        TO CROSS-IDPRODNR                    
229000           MOVE KOLLI-IDKOLLI        TO CROSS-IDKOLLI                     
229100           MOVE KOLLI-IDLEVNR        TO CROSS-IDLEVNR                     
229200           MOVE KOLLI-IDSUPREF       TO CROSS-IDSUPREF                    
229300           MOVE KOLLI-DARFS(3:6)     TO CROSS-TIRFSDAT                    
229400           MOVE 1                    TO CROSS-KDKOLSTA-CROSS              
229500           MOVE ZERO            TO  CROSS-IDTRPTNR-CROSS                  
229600           MOVE ZERO            TO  CROSS-TIRECXDAT                       
229700           MOVE ZERO            TO  CROSS-TIRECXTID                       
229800           MOVE ZERO            TO  CROSS-TISKEPPN                        
229900           MOVE ZERO            TO  CROSS-IDSHIPM-CROSS                   
230000           MOVE SPACE           TO  CROSS-IDLBBET-CROSS                   
230100           PERFORM IMS-ISRT-WDE621                                        
230200         END-IF                                                           
230300         ADD +1 TO WORK-NO-OF-CASES                                       
230400         MOVE ZERO TO WORK-KOLLI-KVORDRAD                                 
230500                      WORK-KOLLI-VKORDNTO                                 
230600                      WORK-KOLLI-VKORDBTO                                 
230700                      WORK-KOLLI-VLORDBTO                                 
230800                      WORK-KOLLI-SUORDV-LOC                               
230900                      WORK-KOLLI-SUORDV-LOCPREL                           
231000                      WORK-KOLLI-SUORDV                                   
231100                      WORK-KOLLI-SUORDV-EXP                               
231200         IF SUB-KDTRANS(1:6) = 'WLA199'                                   
231300           CONTINUE                                                       
231400         ELSE                                                             
231500           PERFORM S04-PACK-TRANS-VR                                      
231600         END-IF                                                           
231700       END-IF                                                             
231800     END-IF                                                               
231900     .                                                                    
232000     EJECT                                                                
232100 HCA-CALC-E611  SECTION.                                                  
232200     MOVE 'HCA-CALC-E611' TO WS-CURRENT-SECTION                           
232300                                                                          
232400     IF REQU-KVLEVART (LINE-IX) = ALL '+'                                 
232500       COMPUTE WORK-KOLLI-VKORDNTO ROUNDED = ORAD-VKARTNTO                
232600                                   * WS-KVLEVART-REST(LINE-IX)            
232700     ELSE                                                                 
232800       COMPUTE WORK-KOLLI-VKORDNTO ROUNDED = ORAD-VKARTNTO                
232900                                   * WORK-KOLLI-KVLEVART                  
233000     END-IF                                                               
233100*                                                                         
233200*ONLY RECALCULATE BRUTTO WEIGHT WITH THE NEW NETTO WEIGHT                 
233300*                                                                         
233400     COMPUTE WORK-KOLLI-VKORDBTO ROUNDED =                                
233500             WORK-KOLLI-VKORDBTO + WORK-KOLLI-VKORDNTO                    
233600     END-COMPUTE                                                          
233700                                                                          
233800     IF SEGMENT-FOUND-E611                                                
233900                                                                          
234000       IF REQU-KDKOLLI(LINE-IX) = ALL '+'                                 
234100       OR KOLLI-KDKOLLI = REQU-KDKOLLI(LINE-IX)                           
234200*OLD KDKOLLI                                                              
234300         CONTINUE                                                         
234400       ELSE                                                               
234500*                                                                         
234600*NEW KDKOLLI                                                              
234700*GET OLD TARA WEIGHT AND RECALCULATE THE BRUTTO WEIGHT & VOLUME           
234800*                                                                         
234900         MOVE KOLLI-KDKOLLI        TO W-KDKOLLI-K5                        
235000         PERFORM IMS-GU-WDK5                                              
235100*DECREASE BRUTTO WEIGHT WITH OLD TARA                                     
235200         COMPUTE WORK-KOLLI-VKORDBTO ROUNDED = WORK-KOLLI-VKORDBTO        
235300                                     - EMB-VKTARA                         
235400         END-COMPUTE                                                      
235500*ADD THE NEW TARA TO BRUTTO WEIGHT                                        
235600         COMPUTE WORK-KOLLI-VKORDBTO ROUNDED = WORK-KOLLI-VKORDNTO        
235700                                     + WORK-EMB-TARE-ONE-CASE             
235800         END-COMPUTE                                                      
235900*                                                                         
236000*DECREASE BRUTTO VOLUME WITH OLD VOLUME                                   
236100         COMPUTE OLD-EMB-VOL-ONE-CASE ROUNDED =                           
236200         EMB-DIKOLLIL * EMB-DIKOLLIH * EMB-DIKOLLIB / 1000000             
236300*                                                                         
236400         COMPUTE WORK-KOLLI-VLORDBTO ROUNDED = WORK-KOLLI-VLORDBTO        
236500                                     - OLD-EMB-VOL-ONE-CASE               
236600         END-COMPUTE                                                      
236700*                                                                         
236800*ADD THE NEW VOLUME TO NETTO                                              
236900*WORK-EMB-VOL-ONE-CASE FROM HBA-UPD-E421 SECTION.                         
237000         COMPUTE WORK-KOLLI-VLORDBTO ROUNDED = WORK-KOLLI-VLORDBTO        
237100                                     + WORK-EMB-VOL-ONE-CASE              
237200       END-IF                                                             
237300     ELSE                                                                 
237400*                                                                         
237500*NEW CASE KOLLI (OLD CODE FROM BEFORE, NEW CODE ABOVE ELSE PART)          
237600       COMPUTE WORK-KOLLI-VKORDBTO ROUNDED = WORK-KOLLI-VKORDNTO          
237700                                   + WORK-EMB-TARE-ONE-CASE               
237800                                                                          
237900       COMPUTE WORK-KOLLI-VLORDBTO ROUNDED = WORK-KOLLI-VLORDBTO          
238000                                   + WORK-EMB-VOL-ONE-CASE                
238100     END-IF                                                               
238200*                                                                         
238300*ADD TO KOLLI-VKORDBTO AND KOLLI-VLORDBTO IS DONE IN                      
238400*****************HC-INIT-E611  SECTION.                                   
238500                                                                          
238600     IF DIST79-DEALER-PRICE                                               
238700       IF ORAD-PRARTNTO-LOCPREL > ZERO                                    
238800         COMPUTE WORK-KOLLI-SUORDV-LOCPREL                                
238900               = WORK-KOLLI-SUORDV-LOCPREL                                
239000*                  + ORAD-PRARTNTO-LOCPREL * ORAD-KVLEVART                
239100                   + ORAD-PRARTNTO-LOCPREL * KKOLLI-KVLEVART              
239200       ELSE                                                               
239300         IF ORAD-PRARTNTO-LOC > ZERO                                      
239400           COMPUTE WORK-KOLLI-SUORDV-LOC = WORK-KOLLI-SUORDV-LOC          
239500*                    + ORAD-PRARTNTO-LOC * ORAD-KVLEVART                  
239600                     + ORAD-PRARTNTO-LOC * KKOLLI-KVLEVART                
239700         END-IF                                                           
239800       END-IF                                                             
239900     ELSE                                                                 
240000       IF ORAD-PRAVCOST > ZERO                                            
240100         COMPUTE WORK-KOLLI-SUORDV-EXP = WORK-KOLLI-SUORDV-EXP            
240200*                + ORAD-PRAVCOST * ORAD-KVLEVART                          
240300                 + ORAD-PRAVCOST * KKOLLI-KVLEVART                        
240400       END-IF                                                             
240500       IF ORAD-PRARTNTO > ZERO                                            
240600         COMPUTE WORK-KOLLI-SUORDV = WORK-KOLLI-SUORDV                    
240700*                + ORAD-PRARTNTO * ORAD-KVLEVART                          
240800                 + ORAD-PRARTNTO * KKOLLI-KVLEVART                        
240900       END-IF                                                             
241000     END-IF                                                               
241100                                                                          
241200     IF ORAD-KVFLAMP > ZERO                                               
241300     AND (ORAD-KVFLAMP < WORK-KOLLI-KVFLAMP                               
241400     OR WORK-KOLLI-KVFLAMP = ZERO)                                        
241500       MOVE ORAD-KVFLAMP       TO WORK-KOLLI-KVFLAMP                      
241600     END-IF                                                               
241700                                                                          
241800     IF ORAD-KDFARLIG = +2 OR +3 OR +4 OR +7                              
241900     AND ORAD-KDFARLIG > WORK-KOLLI-KDFARLIG                              
242000       MOVE ORAD-KDFARLIG TO WORK-KOLLI-KDFARLIG                          
242100     END-IF                                                               
242200     .                                                                    
242300     EJECT                                                                
242400                                                                          
242500 I-UPD-E601  SECTION.                                                     
242600     MOVE 'I-UPD-E601' TO WS-CURRENT-SECTION                              
242700                                                                          
242800     MOVE REQU-IDPRODNR-KEY  TO W-IDPRODNR-E6                             
242900     PERFORM IA-EV-CASELBL-DELNOTE                                        
243000     PERFORM IMS-GHU-WDE601                                               
243100     PERFORM IB-INIT-E601-AREA                                            
243200     PERFORM IMS-REPL-WDE601                                              
243300     IF SUB-KDTRANS(1:6) NOT = 'WLA199'                                   
243400       IF VORD-FLAUTFAK = YES                                             
243500         IF DIST03-SVERIGE-2                                              
243600         OR DIST03-SVERIGE-EJ-778                                         
243700         OR DIST18-SKROT                                                  
243800           PERFORM IC-UPDATE-4726-4727                                    
243900         END-IF                                                           
244000       END-IF                                                             
244100     END-IF                                                               
244200     PERFORM ID-UPDATE-WDGX4472                                           
244300     .                                                                    
244400     EJECT                                                                
244500                                                                          
244600 IA-EV-CASELBL-DELNOTE SECTION.                                           
244700     MOVE 'IA-EV-CASELBL-DELNOTE'  TO WS-CURRENT-SECTION                  
244800                                                                          
244900     IF REQU-FLSKRIV-CLABEL = 'Y'                                         
245000       MOVE ZERO                   TO RESP-L128-KVRADER                   
245100       MOVE 'N'                    TO RESP-L128-FLBG                      
245200     END-IF                                                               
245300     IF REQU-FLSKRIV-DELNOTE NOT = 'Y'                                    
245400       MOVE ZERO                   TO RESP-L129-KVRADER                   
245500       MOVE 'N'                    TO RESP-L129-FLBG                      
245600     END-IF                                                               
245700                                                                          
245800*LAS IN KOLLIID FRÅN BILDEN. DET KAN VARA FLERA.                          
245900*DET FINNS ETT MAX PÅ 15 I TABELLEN.                                      
246000*MAX 15 PGA DET KAN STARTAS MAX 15 ST KOLLIFL. OCH FÖLJES.                
246100                                                                          
246200     MOVE +1 TO LINE-IX                                                   
246300     PERFORM UNTIL REQU-IDRADNR-FOM (LINE-IX) = ALL '+'                   
246400                OR REQU-IDRADNR-FOM (LINE-IX) <= SPACE                    
246500       IF REQU-IDKOLLI (LINE-IX) NOT = ALL '+'                            
246600         MOVE REQU-IDKOLLI (LINE-IX) TO SAVE-IDKOLLI                      
246700         PERFORM S20-SAVE-IN-IDKOLLI-TAB                                  
246800       END-IF                                                             
246900       ADD +1 TO LINE-IX                                                  
247000     END-PERFORM                                                          
247100                                                                          
247200     ADD  WORK-NO-OF-LINES        TO CALC-VORD-KVORDRAD-PACK              
247300     MOVE 2                       TO VORD-KDORDSTA                        
247400                                                                          
247500*LÄS ALLA KOLLI UNDER E601 OCH ADDERA.                                    
247600     MOVE +0 TO CASE-IX                                                   
247700     PERFORM IMS-GHU-WDE601                                               
247800     PERFORM IMS-GHNP-WDE611-OKVAL                                        
247900     PERFORM UNTIL SEGMENT-MISSING                                        
248000       ADD +1                     TO CALC-VORD-KVKOLLI                    
248100                                                                          
248200       IF KOLLI-KDKOLSTA NOT = ZERO                                       
248300         ADD +1                   TO CALC-VORD-KVKOLPAC                   
248400       ELSE                                                               
248500         IF KOLLI-DIKOLLIL > ZERO                                         
248600           ADD +1                 TO CALC-VORD-KVKOLPAC                   
248700         END-IF                                                           
248800       END-IF                                                             
248900       ADD KOLLI-VKORDBTO-KOLLI   TO CALC-VORD-VKORDBTO                   
249000       ADD KOLLI-VKORDNTO-KOLLI   TO CALC-VORD-VKORDNTO                   
249100       ADD KOLLI-VLORDBTO-KOLLI   TO CALC-VORD-VLORDBTO                   
249200                                                                          
249300       ADD KOLLI-SUORDV-KOLLI     TO CALC-VORD-SUORDV                     
249400       ADD KOLLI-SUORDV-KLI-EXP   TO CALC-VORD-SUORDV-EXP                 
249500       ADD KOLLI-SUORDV-LOC       TO CALC-VORD-SUORDV-LOC                 
249600       ADD KOLLI-SUORDV-LOCPREL   TO CALC-VORD-SUORDV-LOCPREL             
249700                                                                          
249800*S21- KONTROLLERA OM INLÄST KOLLI (WDE611) FINNS I REQU-AREA(MID).        
249900*OM INLÄST KOLLI (WDE611) FINNS I REQU-AREA SKALL PGM EV SKRIVAUT         
250000*KOLLIFL. RESP. FÖLJESSEDEL.                                              
250100       PERFORM S21-CHECK-IN-IDKOLLI-TAB                                   
250200                                                                          
250300       IF CASE-IX <= MAX-CASE-IX                                          
250400         IF TABLE-IX <= USED-TABLE-IX                                     
250500*SKRIV UT DE ANTAL KOLLIN SOM FINNS I REQU-AREA (MID).                    
250600           IF REQU-FLSKRIV-CLABEL = 'Y'                                   
250700             PERFORM S01A-CASE-LABEL                                      
250800           END-IF                                                         
250900           IF REQU-FLSKRIV-DELNOTE = 'Y'                                  
251000             PERFORM S01B-DELIVERY-NOTE                                   
251100           END-IF                                                         
251200                                                                          
251300           ADD +1               TO CASE-IX                                
251400         END-IF                                                           
251500       END-IF                                                             
251600       PERFORM IMS-GHNP-WDE611-OKVAL                                      
251700     END-PERFORM                                                          
251800     .                                                                    
251900     EJECT                                                                
252000                                                                          
252100 IB-INIT-E601-AREA   SECTION.                                             
252200     MOVE 'IB-INIT-E601-AREA' TO WS-CURRENT-SECTION                       
252300                                                                          
252400     IF VORD-KDORDSTA = 1                                                 
252500       MOVE 2                      TO VORD-KDORDSTA                       
252600     END-IF                                                               
252700     ADD  CALC-VORD-KVORDRAD-PACK  TO VORD-KVORDRAD-PACK                  
252800                                                                          
252900     MOVE CALC-VORD-KVKOLLI        TO VORD-KVKOLLI                        
253000*LK  IF SUB-KDTRANS(1:6) NOT = 'WLA199'                                   
253100       MOVE CALC-VORD-KVKOLPAC     TO VORD-KVKOLPAC                       
253200*LK  END-IF                                                               
253300     MOVE CALC-VORD-VKORDNTO        TO VORD-VKORDNTO                      
253400     MOVE CALC-VORD-VKORDBTO        TO VORD-VKORDBTO                      
253500     MOVE CALC-VORD-VLORDBTO        TO VORD-VLORDBTO                      
253600                                                                          
253700     IF CALC-VORD-SUORDV-EXP > ZERO                                       
253800       MOVE CALC-VORD-SUORDV-EXP    TO VORD-SUORDV-PACK                   
253900     ELSE                                                                 
254000       MOVE CALC-VORD-SUORDV        TO VORD-SUORDV-PACK                   
254100     END-IF                                                               
254200     MOVE CALC-VORD-SUORDV-LOC      TO VORD-SUORDV-PACK-LOC               
254300     MOVE CALC-VORD-SUORDV-LOCPREL  TO VORD-SUORDV-PACK-LOCPREL           
254400                                                                          
254500     MOVE DAGENS-DATUM             TO VORD-TIPACKN-SK                     
254600     MOVE ORAD-KDVALISO            TO VORD-KDVALISO                       
254700                                                                          
254800     IF VORD-KDORDSTA < +3                                                
254900     AND VORD-KVKOLLI  > +0                                               
255000     AND (DIST03-SVERIGE OR ODEL-IDDISTR = DCS-IDDISTR-REFILL)            
255100     AND (DIST03-SVERIGE OR DIST35-CDC-LDC-REFILL)                        
255200     AND (DCS-CDC OR (DCS-SDC AND DCS-SWEDEN))                            
255300       MOVE VORD-KDFRAKT   TO WS-KDFRAKT-NUM                              
255400       MOVE WS-KDFRAKT-NUM TO FRAK01-KDFRAKT                              
255500       IF FRAK01-SVERIGE2                                                 
255600       OR FRAK01-NORDEN                                                   
255700       OR FRAK01-KDFRAKT21                                                
255800       OR FRAK01-KDFRAKT62                                                
255900       OR (SAVE-IDKOLLI > 149 AND SAVE-IDKOLLI < 200)                     
256000       OR (SAVE-IDKOLLI > 349 AND SAVE-IDKOLLI < 400)                     
256100         IF VORD-FLDIRLEV = NOO                                           
256200         AND VORD-KDFRAKT  NOT = +17                                      
256300           IF NOT DIS128-FRAKTS                                           
256400             MOVE YES TO VORD-FLFRAKTS                                    
256500           END-IF                                                         
256600         END-IF                                                           
256700       END-IF                                                             
256800     END-IF                                                               
256900     .                                                                    
257000     EJECT                                                                
257100                                                                          
257200 IC-UPDATE-4726-4727 SECTION.                                             
257300     MOVE 'IC-UPDATE-4726-4727'   TO WS-CURRENT-SECTION                   
257400                                                                          
257500       MOVE '4726'                  TO W-IDHTYP-4726                      
257600       MOVE NOO                     TO W-FLBATCH-4726                     
257700       MOVE LOW-VALUE               TO W-LOWVALUE-4726                    
257800                                                                          
257900       PERFORM IMS-GU-WDGX4726                                            
258000                                                                          
258100       MOVE ODEL-IDDISTR            TO W-IDDISTR-4727                     
258200       MOVE ODEL-IDKUNDNR           TO W-IDKUNDNR-4727                    
258300       MOVE REQU-IDDC-KEY           TO W-IDDC-4727                        
258400       MOVE VORD-KDFAKTYP           TO W-KDFAKTYP-4727                    
258500                                                                          
258600       PERFORM IMS-GNP-WDGX4727                                           
258700                                                                          
258800       IF SEGMENT-MISSING                                                 
258900         MOVE ODEL-IDDISTR          TO AUTFAKT-IDDISTR                    
259000         MOVE ODEL-IDKUNDNR         TO AUTFAKT-IDKUNDNR                   
259100         MOVE REQU-IDDC-KEY         TO AUTFAKT-IDDC                       
259200         MOVE VORD-KDFAKTYP         TO AUTFAKT-KDFAKTYP                   
259300         PERFORM IMS-ISRT-WDGX4726                                        
259400       END-IF                                                             
259500       MOVE REQU-IDPRODNR-KEY       TO AUTFAKT-IDPRODNR                   
259600       MOVE ZERO                    TO AUTFAKT-IDSKEPPN                   
259700                                       AUTFAKT-PRFRAKT                    
259800                                                                          
259900       IF DIST03-SVERIGE                                                  
260000         MOVE NOO                   TO AUTFAKT-FLLASTA                    
260100       ELSE                                                               
260200         MOVE YES                   TO AUTFAKT-FLLASTA                    
260300       END-IF                                                             
260400                                                                          
260500       PERFORM IMS-ISRT-WDGX4727                                          
260600     .                                                                    
260700     EJECT                                                                
260800                                                                          
260900 ID-UPDATE-WDGX4472  SECTION.                                             
261000     MOVE 'ID-UPDATE-WDGX4472'   TO WS-CURRENT-SECTION                    
261100                                                                          
261200     IF WS-4472-PACK-LINES > ZERO                                         
261300* --- INTERVAL HAS PACKED ORDERLINES                                      
261400                                                                          
261500       PERFORM S06A-READ-SHIFTTAB                                         
261600                                                                          
261700       IF ODEL-KDPRODKL = 'B' OR ODEL-KDPRODKL = 'C'                      
261800* --- PRODTAB IS ONLY UPDATED FOR PRODKL B AND C                          
261900                                                                          
262000         MOVE REQU-IDDC-KEY      TO W-IDDC-4471                           
262100         MOVE ODEL-IDPRCBAS      TO W-IDPRCBAS-4471                       
262200         MOVE ODEL-IDPRCVAR      TO W-IDPRCVAR-4471                       
262300         PERFORM IMS-GHU-WDGX4472                                         
262400                                                                          
262500         IF SEGMENT-FOUND                                                 
262600* --- PRODTAB UPDATE THE ACTUAL PRC                                       
262700           MOVE 1                 TO 4472-IX                              
262800           MOVE W-IDSHIFT-4478    TO IDSHIFT-IX                           
262900           MOVE ODEL-DARFS        TO WS-ODEL-DARFS                        
263000           MOVE 4472-TIRFS (4472-IX) TO WS-4472-TIRFS                     
263100                                                                          
263200           PERFORM UNTIL 4472-IX = 30                                     
263300                OR 4472-TIRFS (4472-IX) = ZERO                            
263400                OR WS-ODEL-DARFS-6 = WS-4472-TIRFS-6                      
263500             ADD 1                 TO 4472-IX                             
263600             MOVE 4472-TIRFS (4472-IX) TO WS-4472-TIRFS                   
263700           END-PERFORM                                                    
263800                                                                          
263900           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (4472-IX)                 
264000     MOVE W-IDSHIFT-4478    TO 4472-IDSHIFT (4472-IX,  IDSHIFT-IX)        
264100           MOVE WS-4472-PACK-LINES                                        
264200             TO 4472-KVRADER-PRAPP (4472-IX, IDSHIFT-IX)                  
264300           PERFORM S06B-ADD-TOTAL-PRODTID                                 
264400           PERFORM IMS-REPL-WDGX4472                                      
264500         END-IF                                                           
264600       END-IF                                                             
264700     END-IF                                                               
264800     .                                                                    
264900     EJECT                                                                
265000                                                                          
265100*L-EV-LOAD-L198  SECTION.                                                 
265200*    MOVE 'L-LOAD-L198'  TO WS-CURRENT-SECTION                            
265300*                                                                         
265400*    VILLKOR FÖR LOAD HÄMTADE UR W4031400                                 
265500*    EV BEHÖVS INGA VILLKOR UTAN VI FLYTTAR ALLTID                        
265600*                                                                         
265700**   MOVE ODEL-IDKUNDNR        TO WS-IDKUNDNR-NUM                         
265800**   MOVE WS-IDKUNDNR-NUM      TO TRANSFER-KUND                           
265900**                                                                        
266000**   IF ((DCS-CDC AND (DIST08-URSP-RAPP                                   
266100**                  OR DIST08-URSP-RAPP-CDC                               
266200**                  OR DIST08-URSP-SPX))                                  
266300**        OR                                                              
266400**        (DCS-NDC-NA AND ((TRANSFER-KUNDNR AND                           
266500**                         DIST08-URSP-TRANSFER-NDC)                      
266600**                       OR (RETUR-KUNDNR AND                             
266700**                         DIST08-URSP-RETUR-NDC)))                       
266800**        OR                                                              
266900**        (DCS-NDC-NA AND DCS-IDLANDX2 = 'CA'                             
267000**                    AND DIST08-URSP-RAPP-CDC))                          
267100**                                                                        
267200*        MOVE ODEL-IDUSER (4:5) TO RESP-L198-IDANSTNR-KEY                 
267300*        MOVE ODEL-IDDISTR     TO RESP-L198-IDDISTR-KEY                   
267400*        MOVE ODEL-IDKUNDNR    TO RESP-L198-IDKUNDNR-KEY                  
267500*        MOVE ODEL-IDORDNR7    TO RESP-L198-IDORDNR-KEY                   
267600*        MOVE REQU-IDPRODNR-KEY TO RESP-L198-IDPRODNR-KEY                 
267700*        MOVE REQU-IDDC-KEY    TO RESP-L198-IDDC-KEY                      
267800*        MOVE SAVE-IDKOLLI     TO RESP-L198-IDKOLLI-KEY                   
267900*                                                                         
268000*        MOVE +1               TO INDX                                    
268100*        PERFORM UNTIL INDX > +15                                         
268200*          MOVE ALL-PLUS       TO RESP-L198-RAD (INDX)                    
268300*          ADD +1              TO INDX                                    
268400*        END-PERFORM                                                      
268500**   END-IF                                                               
268600*                                                                         
268700*    .                                                                    
268800*    EJECT                                                                
268900                                                                          
269000                                                                          
269100 K-LOAD-L123  SECTION.                                                    
269200     MOVE 'K-LOAD-L123'  TO WS-CURRENT-SECTION                            
269300                                                                          
269400     MOVE ODEL-IDUSER (4:5)    TO RESP-L123-IDANSTNR-KEY                  
269500     MOVE ODEL-IDDISTR         TO RESP-L123-IDDISTR-KEY                   
269600     MOVE ODEL-IDKUNDNR        TO RESP-L123-IDKUNDNR-KEY                  
269700     MOVE ODEL-IDORDNR7        TO RESP-L123-IDORDNR-KEY                   
269800     MOVE REQU-IDPRODNR-KEY    TO RESP-L123-IDPRODNR-KEY                  
269900     MOVE REQU-IDDC-KEY        TO RESP-L123-IDDC-KEY                      
270000     MOVE SAVE-IDKOLLI         TO RESP-L123-IDKOLLI-KEY                   
270100     MOVE '+'                  TO RESP-L123-FLSVAR                        
270200     .                                                                    
270300     EJECT                                                                
270400                                                                          
270500 S01A-CASE-LABEL   SECTION.                                               
270600     MOVE 'S01A-CASE-LABEL'      TO WS-CURRENT-SECTION                    
270700                                                                          
270800     IF  DCS-CDC                                                          
270900       MOVE WS-KDPRTVAL-ADR        TO 4333-MID-KDPRTVAL-UT                
271000       MOVE ODEL-IDDISTR           TO WS-IDDISTR-NUM                      
271100       MOVE WS-IDDISTR-NUM         TO 4333-MID-IDDISTR-UT                 
271200       MOVE ODEL-IDKUNDNR          TO WS-IDKUNDNR-NUM                     
271300       MOVE WS-IDKUNDNR-NUM        TO 4333-MID-IDKUNDNR-UT                
271400       MOVE ODEL-IDKUNDRF (3:5)    TO 4333-MID-IDORDNR-UT                 
271500       MOVE KOLLI-IDKOLLI          TO 4333-MID-IDKOLLI-UT                 
271600       MOVE REQU-IDDC-KEY          TO 4333-MID-IDDC-UT                    
271700                                                                          
271800       IF DIRLEV-KOLLI                                                    
271900         MOVE REQU-IDPRODNR-KEY    TO 4333-MID-IDPRODNR-UT                
272000       ELSE                                                               
272100         MOVE ZERO                 TO 4333-MID-IDPRODNR-UT                
272200       END-IF                                                             
272300       MOVE ZERO                   TO 4333-MID-IDKOLLI-TOM                
272400       MOVE '++++'                 TO 4333-MID-IDDISTR-IN                 
272500       MOVE '++++++'               TO 4333-MID-IDKUNDNR-IN                
272600       MOVE '+++++'                TO 4333-MID-IDORDNR-IN                 
272700                                        4333-MID-IDKOLLI-IN               
272800       MOVE '++'                   TO 4333-MID-IDDC-IN                    
272900       MOVE '+++++++'              TO 4333-MID-IDPRODNR-IN                
273000       MOVE '++'                   TO 4333-MID-KDPRTVAL-IN                
273100                                                                          
273200       MOVE '1'                    TO 4333-MID-KDMFSFOR                   
273300       COMPUTE 4333-MID-KVLL = LENGTH OF 4333-MID-W4I33301 + 17           
273400                                                                          
273500       PERFORM IMS-PURG-4333-MSG                                          
273600     ELSE                                                                 
273700                                                                          
273800       MOVE USED-TABLE-IX   TO RESP-L128-KVRADER                          
273900       MOVE 'Y'             TO RESP-L128-FLBG                             
274000       MOVE REQU-IDDC-KEY   TO RESP-L128-IDDC-KEY (TABLE-IX)              
274100       MOVE ODEL-IDDISTR    TO RESP-L128-IDDISTR-KEY (TABLE-IX)           
274200       MOVE ODEL-IDKUNDNR   TO RESP-L128-IDKUNDNR-KEY (TABLE-IX)          
274300       MOVE ODEL-IDKUNDRF (3:5) TO RESP-L128-IDORDNR-KEY(TABLE-IX)        
274400       MOVE KOLLI-IDKOLLI   TO RESP-L128-IDKOLLI-KEY (TABLE-IX)           
274500       MOVE ZERO            TO RESP-L128-IDKOLLI-TOM (TABLE-IX)           
274600       MOVE REQU-IDPRODNR-KEY TO RESP-L128-IDPRODNR-KEY (TABLE-IX)        
274700       MOVE 'Y'             TO RESP-L128-CLABEL (TABLE-IX)                
274800     END-IF                                                               
274900                                                                          
275000     .                                                                    
275100     EJECT                                                                
275200                                                                          
275300 S01B-DELIVERY-NOTE SECTION.                                              
275400     MOVE 'S01B-DELIVERY-NOTE'  TO WS-CURRENT-SECTION                     
275500                                                                          
275600     IF  DCS-CDC                                                          
275700       MOVE '++++'           TO  4341-MID-IDDISTR-IN                      
275800       MOVE ODEL-IDDISTR           TO WS-IDDISTR-NUM                      
275900       MOVE WS-IDDISTR-NUM   TO  4341-MID-IDDISTR-UT                      
276000       MOVE '++++++'         TO  4341-MID-IDKUNDNR-IN                     
276100       MOVE ODEL-IDKUNDNR          TO WS-IDKUNDNR-NUM                     
276200       MOVE WS-IDKUNDNR-NUM  TO  4341-MID-IDKUNDNR-UT                     
276300       MOVE '+++++'          TO  4341-MID-IDORDNR-IN                      
276400       MOVE ODEL-IDKUNDRF (3:5) TO 4341-MID-IDORDNR-UT                    
276500       MOVE '+'              TO  4341-MID-IDPLKLST-IN                     
276600       MOVE ZERO             TO  4341-MID-IDPLKLST-UT                     
276700       MOVE '+++++'          TO 4341-MID-IDKOLLI-IN                       
276800       MOVE KOLLI-IDKOLLI    TO 4341-MID-IDKOLLI-UT                       
276900       MOVE '+++++'          TO 4341-MID-IDKOLLI-TOM-IN                   
277000       MOVE ZERO             TO 4341-MID-IDKOLLI-TOM-UT                   
277100       MOVE '++'             TO 4341-MID-KDPRTVAL-IN                      
277200       MOVE WS-KDPRTVAL-FS   TO 4341-MID-KDPRTVAL-UT                      
277300       MOVE '++'             TO 4341-MID-IDDC-IN                          
277400       MOVE REQU-IDDC-KEY    TO 4341-MID-IDDC-UT                          
277500       MOVE 'N'              TO 4341-MID-FL-SVENSK-FSEDEL                 
277600                                                                          
277700       COMPUTE 4341-MID-LL = LENGTH OF 4341-MID-W4I34101 + 17             
277800       MOVE '1'              TO 4341-MID-KDMFSFOR                         
277900                                                                          
278000       PERFORM IMS-PURG-4341-MSG                                          
278100     END-IF                                                               
278200                                                                          
278300     MOVE USED-TABLE-IX        TO RESP-L129-KVRADER                       
278400     MOVE 'Y'                  TO RESP-L129-FLBG                          
278500     MOVE REQU-IDDC-KEY        TO RESP-L129-IDDC-KEY (TABLE-IX)           
278600     MOVE ODEL-IDDISTR         TO RESP-L129-IDDISTR-KEY (TABLE-IX)        
278700     MOVE ODEL-IDKUNDNR    TO RESP-L129-IDKUNDNR-KEY (TABLE-IX)           
278800     MOVE ODEL-IDORDNR7        TO RESP-L129-IDORDNR-KEY (TABLE-IX)        
278900     MOVE KOLLI-IDKOLLI        TO RESP-L129-IDKOLLI-KEY (TABLE-IX)        
279000     MOVE ZERO                 TO RESP-L129-IDKOLLI-TOM (TABLE-IX)        
279100     MOVE 'Y'              TO RESP-L129-FLSKRIV-DELNOTE(TABLE-IX)         
279200     .                                                                    
279300     EJECT                                                                
279400*****************************************************************         
279500*FOR NEW CASE,IDPSN DATA IS INSERTED TO (1) OCCURRANCE OF IDPSN           
279600*ARRAY IN WDE611.                                                         
279700*FOR SAME CASE,IDPSN DATA IS ADDED TO PREVIOUS IDPSN IF THEY ARE          
279800*SAME OR NEW OCCURANCE OF IDPSN IS INSERTED IN WDE611                     
279900*****************************************************************         
280000 S03-INS-DG-FIELDS  SECTION.                                              
280100     MOVE 'S03-INS-DG-FIELDS'    TO WS-CURRENT-SECTION                    
280200                                                                          
280300     PERFORM S09-COMPUTE-IDPSN                                            
280400                                                                          
280500     MOVE 'N'                 TO IDPSN-INS-SW                             
280600     MOVE +1                  TO KOLLI-TAB-IX                             
280700     PERFORM UNTIL KOLLI-TAB-IX > MAX-DG-IX                               
280800                   OR IDPSN-INS-SW = 'J'                                  
280900        IF KOLLI-IDPSN (KOLLI-TAB-IX) = 0                                 
281000            MOVE 'J'          TO IDPSN-INS-SW                             
281100            MOVE ORAD-IDPSN   TO KOLLI-IDPSN(KOLLI-TAB-IX)                
281200            MOVE WS-VKART-FG  TO KOLLI-VKART-FG(KOLLI-TAB-IX)             
281300            MOVE WS-VLFG      TO KOLLI-VLFG(KOLLI-TAB-IX)                 
281400            COMPUTE KOLLI-SUEQFG = KOLLI-SUEQFG +                         
281500                                      WS-SUEQFG                           
281600        ELSE                                                              
281700           IF ORAD-IDPSN = KOLLI-IDPSN(KOLLI-TAB-IX)                      
281800              MOVE 'J'           TO IDPSN-INS-SW                          
281900              COMPUTE KOLLI-VKART-FG(KOLLI-TAB-IX) =                      
282000                      KOLLI-VKART-FG(KOLLI-TAB-IX) + WS-VKART-FG          
282100              COMPUTE KOLLI-VLFG(KOLLI-TAB-IX) =                          
282200                      KOLLI-VLFG(KOLLI-TAB-IX) + WS-VLFG                  
282300              COMPUTE KOLLI-SUEQFG = KOLLI-SUEQFG + WS-SUEQFG             
282400           END-IF                                                         
282500        END-IF                                                            
282600        ADD +1               TO KOLLI-TAB-IX                              
282700     END-PERFORM                                                          
282800     .                                                                    
282900     EJECT                                                                
283000 S04-PACK-TRANS-VR  SECTION.                                              
283100     MOVE 'S04-PACK-TRANS-VR'   TO WS-CURRENT-SECTION                     
283200                                                                          
283300     IF DIST03-SVERIGE-100-799                                            
283400     OR DIST03-NORGE                                                      
283500     OR DIST03-DANMARK-900                                                
283600     OR DIST85-PU-VIA-VR                                                  
283700     OR DIST21-TYRE                                                       
283800       MOVE REQU-IDPRODNR-KEY  TO 4322-IDPRODNR                           
283900       MOVE SAVE-IDKOLLI       TO 4322-IDKOLLI                            
284000       PERFORM IMS-ISRT-4322-SEGM                                         
284100     END-IF                                                               
284200     .                                                                    
284300     EJECT                                                                
284400 S06A-READ-SHIFTTAB   SECTION.                                            
284500     MOVE 'S06A-READ-SHIFTTAB'   TO WS-CURRENT-SECTION                    
284600                                                                          
284700     MOVE REQU-IDDC-KEY     TO W-IDDC-4477                                
284800     MOVE '1'               TO W-IDSHIFT-4478                             
284900     MOVE ODEL-IDUSER       TO W-IDUSER-4478                              
285000     PERFORM IMS-GU-WDGX4478                                              
285100                                                                          
285200     IF SEGMENT-MISSING                                                   
285300       MOVE '2'            TO W-IDSHIFT-4478                              
285400       PERFORM IMS-GU-WDGX4478                                            
285500                                                                          
285600       IF SEGMENT-MISSING                                                 
285700         MOVE '3'         TO W-IDSHIFT-4478                               
285800         PERFORM IMS-GU-WDGX4478                                          
285900                                                                          
286000         IF SEGMENT-MISSING                                               
286100           MOVE '1'      TO W-IDSHIFT-4478                                
286200         END-IF                                                           
286300       END-IF                                                             
286400     END-IF                                                               
286500     .                                                                    
286600     EJECT                                                                
286700 S06B-ADD-TOTAL-PRODTID    SECTION.                                       
286800     MOVE 'S06B-ADD-TOTAL-PRODTID'   TO WS-CURRENT-SECTION                
286900                                                                          
287000     MOVE 4472-SUPTID-PRAPP (4472-IX, IDSHIFT-IX)                         
287100       TO WS-SUPTID-PRAPP                                                 
287200                                                                          
287300     COMPUTE WS-KVPTID-MIN ROUNDED = WS-4472-PACK-LINES *                 
287400                                     WS-KVPTID                            
287500                                                                          
287600     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
287700     MOVE WS-KVPTID-TIM    TO WS-SUPTID-TIM                               
287800     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
287900                            (WS-KVPTID-TIM * 60)                          
288000                                                                          
288100     MOVE WS-SUPTID-MIN    TO WS-KVPTID-MIN                               
288200     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
288300     MOVE WS-KVPTID-TIM    TO WS-SUPTID-TIM                               
288400     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
288500                            (WS-KVPTID-TIM * 60)                          
288600     MOVE WS-KVPTID-MIN   TO WS-SUPTID-MIN                                
288700                                                                          
288800     MOVE WS-SUPTID-PRAPP                                                 
288900       TO 4472-SUPTID-PRAPP (4472-IX, IDSHIFT-IX)                         
289000     .                                                                    
289100     EJECT                                                                
289200 S07-CALL-W403PLAT SECTION.                                               
289300     MOVE 'S07-CALL-W403PLAT'   TO WS-CURRENT-SECTION                     
289400                                                                          
289500     INITIALIZE PLATS-W403PLAT                                            
289600                                                                          
289700     MOVE +2                            TO PLATS-KDCALL                   
289800     MOVE REQU-IDDC-KEY                 TO PLATS-IDDC                     
289900     MOVE KORD-IDDISTR                  TO PLATS-IDDISTR                  
290000     MOVE KORD-IDKUNDNR                 TO PLATS-IDKUNDNR                 
290100     MOVE KORD-KDFRAKT                  TO PLATS-KDFRAKT                  
290200     MOVE KORD-KDORDKL                  TO PLATS-KDORDKLX                 
290300     MOVE ODEL-IDORDNR7                 TO PLATS-IDORDNR                  
290400                                                                          
290500     CALL W403PLAT USING PLATS-W403PLAT                                   
290600                         PLATS-DM-PCB                                     
290700                         PLATS-DN-PCB                                     
290800                         PLATS-DP-PCB                                     
290900                         PLATS-DO-PCB                                     
291000                         PLATS-WDE6C-PCB                                  
291100                         PLATS-GMTC-PCB                                   
291200                         PLATS-WDB6-PCB                                   
291300                                                                          
291400     IF PLATS-KDSVAR = SPACE                                              
291500       MOVE PLATS-IDTRPTNR              TO WS-IDTRPTNR                    
291600       MOVE PLATS-ADFLGEO               TO WS-ADFLGEO                     
291700       MOVE PLATS-ADFLOMR               TO WS-ADFLOMR                     
291800       MOVE PLATS-ADRUTNIV              TO WS-ADRUTNIV                    
291900       MOVE PLATS-ADVMODUL              TO WS-ADVMODUL                    
292000       MOVE PLATS-ADHMODUL              TO WS-ADHMODUL                    
292100       MOVE PLATS-DIHMODUL              TO WS-DIHMODUL                    
292200       MOVE PLATS-DIDMODUL              TO WS-DIDMODUL                    
292300       MOVE PLATS-FLUTLAST              TO WS-FLUTLAST                    
292400       MOVE PLATS-IDDC-CROSS            TO WS-IDDC-CROSS                  
292500     ELSE                                                                 
292600*ERR-W403PLAT                                                             
292700       MOVE ERR-W403PLAT                TO RESP-IDMSG-ERROR               
292800       MOVE SPACE                       TO RESP-IDELMT-ERROR              
292900*      MOVE NOO                         TO DATA-SW                        
293000*KOMMENTARMÄRKT PGA ATT OM SWITCH SÄTT TILL NOO SÅ GÖRS INTE              
293100*SECTION I-UPD-E601 OCH DÅ BLIR ORDEN INTE UPPDATERAD OCH BLIR            
293200*FELAKTIGT UPPDATERAD PÅ WD601                                            
293300     END-IF                                                               
293400     .                                                                    
293500     EJECT                                                                
293600 S08-CREATE-RYK  SECTION.                                                 
293700     MOVE 'S08-CREATE-RYK'   TO WS-CURRENT-SECTION                        
293800                                                                          
293900     IF ORAD-IDKUNDRF-RO NOT = '00000     '                               
294000     AND ORAD-TIRODAT > ZERO                                              
294100       IF LOGG-IDLOGLOP = 9                                               
294200         MOVE ZERO                TO LOGG-IDLOGLOP                        
294300       END-IF                                                             
294400       ACCEPT LOGG-TIAAMMDD FROM DATE                                     
294500       ACCEPT LOGG-TIKLOCK  FROM TIME                                     
294600       ADD +1                     TO LOGG-IDLOGLOP                        
294700                                                                          
294800       MOVE ODEL-IDDISTR          TO RYK-IDDISTR                          
294900       MOVE ODEL-IDKUNDNR         TO RYK-IDKUNDNR                         
295000                                                                          
295100       IF SEGMENT-FOUND                                                   
295200         MOVE ODEL-IDORDER        TO RYK-IDORDER                          
295300       ELSE                                                               
295400         MOVE +0                  TO RYK-IDORDER                          
295500       END-IF                                                             
295600       MOVE 'RYK'                 TO RYK-IDPTYP                           
295700                                     LOGG-IDPTYP                          
295800       MOVE ORAD-IDARTNR          TO RYK-IDARTNR                          
295900       MOVE LOGG-TIAAMMDD         TO RYK-TIRODAT                          
296000       ADD  ORAD-KVLEVART         TO WS-KKOLLI-KVLEVART                   
296100       MOVE WS-KKOLLI-KVLEVART    TO RYK-KVLEVART                         
296200       MOVE ORAD-KVBEART          TO RYK-KVBEART-Q                        
296300       MOVE ORAD-KDORDKL          TO RYK-KDORDKL                          
296400       MOVE ORAD-KDPRODSL         TO RYK-KDPRODSL                         
296500       MOVE ZERO                  TO RYK-KDORDBEK                         
296600       MOVE SPACE                 TO LOGG-SORTPOST                        
296700       MOVE RYK-WDGZRYK           TO LOGG-LOGGPOST                        
296800                                                                          
296900       PERFORM IMS-ISRT-WDG601                                            
297000       PERFORM UNTIL SEGMENT-FOUND                                        
297100         IF LOGG-IDLOGLOP = 9                                             
297200           MOVE ZERO              TO LOGG-IDLOGLOP                        
297300           ACCEPT LOGG-TIKLOCK    FROM TIME                               
297400         END-IF                                                           
297500         ADD +1                   TO LOGG-IDLOGLOP                        
297600         PERFORM IMS-ISRT-WDG601                                          
297700       END-PERFORM                                                        
297800     END-IF                                                               
297900     .                                                                    
298000     EJECT                                                                
298100                                                                          
298200 S09-COMPUTE-IDPSN  SECTION.                                              
298300     MOVE 'S09-COMPUTE-IDPSN'   TO WS-CURRENT-SECTION                     
298400                                                                          
298500     INITIALIZE WS-IDPSN-TAB                                              
298600     COMPUTE WS-VLFG = (ORAD-VLFG * KKOLLI-KVLEVART)                      
298700     IF ORAD-IDPSN = 10 OR 11                                             
298800        COMPUTE WS-VKART-FG = (ORAD-VKART-FG * KKOLLI-KVLEVART)           
298900     ELSE                                                                 
299000        MOVE ZERO               TO WS-VKART-FG                            
299100     END-IF                                                               
299200     COMPUTE WS-SUEQFG = (ORAD-SUEQFG * KKOLLI-KVLEVART)                  
299300     .                                                                    
299400     EJECT                                                                
299500 S10-SAVE-RESTART-AREA  SECTION .                                         
299600     MOVE 'S10-SAVE-RESTART-AREA' TO ERROR-TEXT                           
299700                                                                          
299800     MOVE 'R'                    TO REQU-KDPGMACT                         
299900     MOVE LINE-IX                TO REQU-RESTART-IDRADNR                  
300000     MOVE SAVE-IDKOLLI           TO REQU-RESTART-IDKOLLI                  
300100*    MOVE SAVE-KDKOLLI           TO REQU-RESTART-KDKOLLI                  
300200     MOVE WORK-NO-OF-CASES       TO REQU-RESTART-KVKOLLI                  
300300     MOVE WORK-KOLLI-VKORDNTO    TO REQU-RESTART-VKORDNTO                 
300400     MOVE WORK-KOLLI-VKORDBTO    TO REQU-RESTART-VKORDBTO                 
300500     MOVE WORK-KOLLI-VLORDBTO    TO REQU-RESTART-VLORDBTO                 
300600     IF WORK-KOLLI-SUORDV-LOCPREL > ZERO                                  
300700       MOVE WORK-KOLLI-SUORDV-LOCPREL TO REQU-RESTART-SUORDV              
300800     ELSE                                                                 
300900       IF WORK-KOLLI-SUORDV-LOC > ZERO                                    
301000         MOVE WORK-KOLLI-SUORDV-LOC TO REQU-RESTART-SUORDV                
301100       ELSE                                                               
301200         MOVE WORK-KOLLI-SUORDV TO REQU-RESTART-SUORDV                    
301300       END-IF                                                             
301400     END-IF                                                               
301500     .                                                                    
301600     EJECT                                                                
301700                                                                          
301800 S11-UPD-WDQ212  SECTION.                                                 
301900     MOVE 'S11-UPD-WDQ212'  TO WS-CURRENT-SECTION                         
302000                                                                          
302100     MOVE ODEL-IDORDER      TO W-IDORDER                                  
302200     MOVE REQU-IDDC-KEY     TO W-212-IDDC                                 
302300     PERFORM IMS-GHU-WDQ212                                               
302400     IF ARB-KDORDSTA = 'U '                                               
302500        MOVE 'U*'           TO ARB-KDORDSTA                               
302600     ELSE                                                                 
302700        MOVE 'U '           TO ARB-KDORDSTA                               
302800     END-IF                                                               
302900     PERFORM IMS-REPL-WDQ212                                              
303000     .                                                                    
303100     EJECT                                                                
303200 S20-SAVE-IN-IDKOLLI-TAB  SECTION.                                        
303300     MOVE 'S20-SAVE-IN-IDKOLLI-TAB' TO WS-CURRENT-SECTION                 
303400                                                                          
303500     MOVE 1 TO TABLE-IX                                                   
303600     PERFORM UNTIL TABLE-IX > USED-TABLE-IX                               
303700     OR SAVE-IDKOLLI = TAB-IDKOLLI (TABLE-IX)                             
303800       ADD 1 TO TABLE-IX                                                  
303900     END-PERFORM                                                          
304000                                                                          
304100     IF TABLE-IX > USED-TABLE-IX                                          
304200       IF TABLE-IX <= MAX-TABLE-IX                                        
304300         MOVE TABLE-IX TO USED-TABLE-IX                                   
304400         MOVE SAVE-IDKOLLI TO TAB-IDKOLLI (TABLE-IX)                      
304500       ELSE                                                               
304600         MOVE ERR-TOO-MANY-CASES TO RESP-IDMSG-ERROR                      
304700         MOVE SPACE              TO RESP-IDELMT-ERROR                     
304800         MOVE NOO                TO DATA-SW                               
304900       END-IF                                                             
305000     END-IF                                                               
305100     .                                                                    
305200     EJECT                                                                
305300 S21-CHECK-IN-IDKOLLI-TAB  SECTION.                                       
305400     MOVE 'S21-CHECK-IN-IDKOLLI-TAB' TO WS-CURRENT-SECTION                
305500                                                                          
305600     MOVE 1 TO TABLE-IX                                                   
305700     PERFORM UNTIL TABLE-IX > USED-TABLE-IX                               
305800     OR KOLLI-IDKOLLI = TAB-IDKOLLI (TABLE-IX)                            
305900       ADD 1 TO TABLE-IX                                                  
306000     END-PERFORM                                                          
306100     .                                                                    
306200     EJECT                                                                
306211 S22-OPEN-W403WHEV SECTION.                                               
306212     MOVE 'S22-OPE-W403WHEV' TO WS-CURRENT-SECTION                        
306213                                                                          
306220     MOVE 'OPEN' TO WHEV-KDFUNC                                           
306230     CALL W403WHEV USING WHEV-W403WHEV MQASYNC-PCB                        
306240     .                                                                    
306250 S23-PUT-W403WHEV SECTION.                                                
306251     MOVE 'S23-PUT-W403WHEV' TO WS-CURRENT-SECTION                        
306252                                                                          
306256     MOVE KORD-IDDC         TO WHEV-EVENTLOCATION                         
306257     MOVE KORD-IDDISTR      TO WHEV-IDDISTR                               
306258     MOVE KORD-IDKUNDNR     TO WHEV-IDKUNDNR                              
306259     MOVE KORD-IDORDNR5     TO WHEV-IDORDNR7                              
306260     MOVE KORD-IDPRODNR     TO WHEV-IDPRODNR                              
306261     MOVE KORD-IDPLKLST     TO WHEV-IDPLKLST                              
306262     MOVE WS-IDUSER-ALFA    TO WHEV-USERID                                
306263     MOVE ODEL-IDLEVNR      TO WHEV-IDLEVNR                               
306264     MOVE ODEL-IDLOPNR-ORD TO WHEV-IDLOPNR-ORD                            
306265     MOVE SAVE-IDKOLLI      TO WHEV-IDKOLLI                               
306266     MOVE KOLLI-KDKOLLI     TO WHEV-KDKOLLI                               
306267     MOVE ORAD-IDPURAD      TO WHEV-IDPURAD                               
306268     MOVE WS-WHEV-KVLEVART TO WHEV-KVLEVART                               
306269     MOVE KOLLI-KDEMBTYP    TO WHEV-KDEMBTYP                              
306270     MOVE TAB-DIKOLLIL (LINE-IX) TO WHEV-DIKOLLIL                         
306271     MOVE TAB-DIKOLLIB (LINE-IX) TO WHEV-DIKOLLIB                         
306272     MOVE TAB-DIKOLLIH (LINE-IX) TO WHEV-DIKOLLIH                         
306273     MOVE ORAD-KDARTURS     TO WHEV-KDARTURS                              
306274     MOVE ORAD-IDARTNR      TO W-IDARTNR                                  
306275                               WS-IDARTNR-Z                               
306276     MOVE FUNCTION TRIM (WS-IDARTNR-Z LEADING)                            
306277                                  TO WHEV-IDARTNR                         
306279     IF DCS-CDC                                                           
306281       PERFORM IMS-GU-WDK601                                              
306282       IF SEGMENT-FOUND                                                   
306283          MOVE ART-KDSORT   TO WHEV-KDSORT                                
306284          PERFORM IMS-GNP-WDK611                                          
306285           IF SEGMENT-FOUND                                               
306286             MOVE CLAG-ADGANG TO WHEV-ADGANG                              
306287             MOVE CLAG-ADPLATS TO WHEV-ADPLATS                            
306288             MOVE CLAG-KDLEVSP TO WHEV-KDLEVSP                            
306289             MOVE CLAG-KVAKS-PAV TO WHEV-KVAKS-PAV                        
306290             COMPUTE WHEV-KVAKS = CLAG-KVAKS-CDC + CLAG-KVAKS-T           
306291           END-IF                                                         
306292       END-IF                                                             
306293     ELSE                                                                 
306295       PERFORM IMS-GU-WDK711                                              
306296       IF SEGMENT-FOUND                                                   
306297             MOVE SLAG-ADGANG TO WHEV-ADGANG                              
306298             MOVE SLAG-ADPLATS TO WHEV-ADPLATS                            
306299             MOVE SLAG-KDLEVSP TO WHEV-KDLEVSP                            
306300             MOVE SLAG-KVAKS-PAV TO WHEV-KVAKS-PAV                        
306301             COMPUTE WHEV-KVAKS = SLAG-KVAKS-SDC                          
306303       END-IF                                                             
306304     END-IF                                                               
306305                                                                          
306306     MOVE ORAD-ADLAGOMR     TO WHEV-ADLAGOMR                              
306307     MOVE ORAD-IDPSN        TO WHEV-IDPSN                                 
306308     MOVE ORAD-BEART        TO WHEV-BEART                                 
306309     MOVE ODEL-IDPRC        TO WHEV-IDPRC                                 
306310     MOVE ODEL-IDLOTNR-PLK TO WHEV-IDLOTNR-PLK                            
306311     MOVE ODEL-DARFS(1:4) TO WS-YYYY                                      
306312     MOVE ODEL-DARFS(5:2) TO WS-MM                                        
306313     MOVE ODEL-DARFS(7:2) TO WS-DD                                        
306314     MOVE ODEL-DARFS(9:2) TO WS-HOUR                                      
306315     MOVE ODEL-DARFS(11:2) TO WS-MIN                                      
306316     MOVE WS-DATE           TO WHEV-TIRFSDAT                              
306317     MOVE WS-TIME           TO WHEV-TIRFSTID                              
306318     MOVE ORAD-KVBEART      TO WHEV-KVBEART                               
306319     MOVE 'OUTB-PAC-LINE' TO WHEV-IDEVENTTYP                              
306320     MOVE SPACES            TO WHEV-IDEVENT                               
306321     MOVE KORD-IDDC         TO WHEV-IDEVENT(1:2)                          
306322     MOVE ODEL-IDPRC        TO WHEV-IDEVENT(3:4)                          
306323     MOVE ODEL-IDLOTNR-PLK TO WHEV-IDEVENT(7:3)                           
306324     MOVE WS-IDUSER-ALFA     TO WHEV-IDEVENT(10:5)                        
306325     MOVE DAGENS-DATUM      TO WHEV-IDEVENT(15:6)                         
306326     MOVE DAGENS-TID        TO WHEV-IDEVENT(21:8)                         
306327     MOVE SUB-KDTRANS(1:6) TO WHEV-IDORIGSYS                              
306328     IF SUB-KDTRANS(1:6)     = 'WL0199'                                   
306329        MOVE 'PULS-WEB'     TO WHEV-IDORIGSYS                             
306330     ELSE                                                                 
306331        MOVE REQU-IDORIGSYS TO WHEV-IDORIGSYS                             
306332     END-IF                                                               
306333     MOVE ORAD-VLARTNTO     TO WHEV-VLARTNTO                              
306334     MOVE ORAD-VKARTNTO     TO WHEV-VKARTNTO                              
306335     MOVE ORAD-KDFARLIG     TO WHEV-KDFARLIG                              
306336                                                                          
306337     MOVE 'PUT' TO WHEV-KDFUNC                                            
306338     CALL W403WHEV USING WHEV-W403WHEV MQASYNC-PCB                        
306339     MOVE LOW-VALUES TO WHEV-W403WHEV                                     
306340     .                                                                    
306341 S24-CLOSE-W403WHEV SECTION.                                              
306342     MOVE 'S24-CLO-W403WHEV' TO WS-CURRENT-SECTION                        
306343                                                                          
306344     MOVE 'CLOSE' TO WHEV-KDFUNC                                          
306345     CALL W403WHEV USING WHEV-W403WHEV MQASYNC-PCB                        
306346     .                                                                    
306350 S30-MSG-CONV SECTION.                                                    
306400     MOVE SPACES                  TO RESP-MESSAGES (1)                    
306500                                     RESP-MESSAGES (2)                    
306600     MOVE 1                       TO MSG-IX                               
306700*    REQUEST OK                                                           
306800     MOVE 200                     TO RESP-KDSTATUS-API                    
306900     IF RESP-IDMSG-INFO > SPACE                                           
307000       MOVE SPACES                TO MSG-CONV-AREA                        
307100       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
307200       CALL WMSGCONV           USING MSG-CONV-AREA                        
307300       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
307400       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
307500       ADD 1                      TO MSG-IX                               
307600     END-IF                                                               
307700     IF RESP-IDMSG-ERROR > SPACE                                          
307800*      BAD REQUEST                                                        
307900       MOVE 400                   TO RESP-KDSTATUS-API                    
308000       MOVE SPACES                TO MSG-CONV-AREA                        
308100       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
308200       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
308300       CALL WMSGCONV           USING MSG-CONV-AREA                        
308400       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
308500       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
308600     END-IF                                                               
308700     .                                                                    
308800 S90-SEND-TO-RESTART-THIS-PGM  SECTION.                                   
308900     MOVE 'S90-SEND-TO-RESTART-THIS-PGM' TO ERROR-TEXT                    
309000                                                                          
309100     PERFORM S90-RESTART-OPEN                                             
309200     PERFORM S90-RESTART-SEND                                             
309300     PERFORM S90-RESTART-CLOSE                                            
309400     .                                                                    
309500     SKIP3                                                                
309600                                                                          
309700 S90-RESTART-OPEN  SECTION.                                               
309800     MOVE 'S90-RESTART-OPEN' TO ERROR-TEXT                                
309900                                                                          
310000     MOVE 'OPEN'                     TO SEND-KDFUNC                       
310100     MOVE 'CARPARTS.LDC.CASEREPORTING4'  TO SEND-ADDISPABS                
310200     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
310300                                                                          
310400     IF SEND-KDRC > 0                                                     
310500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
310600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
310700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
310800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
310900     END-IF                                                               
311000     .                                                                    
311100     SKIP3                                                                
311200                                                                          
311300 S90-RESTART-SEND SECTION.                                                
311400     MOVE 'S90-RESTART-SEND        ' TO ERROR-TEXT                        
311500                                                                          
311600     MOVE 'PUT'                      TO SEND-KDFUNC                       
311700     MOVE LENGTH OF REQU-AREA        TO SEND-KVDLEN                       
311800     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
311900                                           REQU-AREA                      
312000                                                                          
312100     IF SEND-KDRC > 0                                                     
312200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
312300       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
312400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
312500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
312600     END-IF                                                               
312700     .                                                                    
312800     SKIP3                                                                
312900                                                                          
313000 S90-RESTART-CLOSE SECTION.                                               
313100     MOVE ' S90-RESTART-CLOSE      ' TO ERROR-TEXT                        
313200                                                                          
313300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
313400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
313500                                                                          
313600     IF SEND-KDRC > 0                                                     
313700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
313800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
313900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
314000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
314100     END-IF                                                               
314200     .                                                                    
314300     EJECT                                                                
314400                                                                          
314500 S99-FETCH-REQUEST-ARGUMENT SECTION.                                      
314600     MOVE 'S99-FETCH-REQUEST-ARGUMENT'   TO WS-CURRENT-SECTION            
314700                                                                          
314800     MOVE 'GETARG'               TO SUB-KDFUNC                            
314900     MOVE 'CARPARTS.LDC.CASEREPORTING4'      TO SUB-ADDISPABS             
315000                                                                          
315100*    MOVE MAX-IX (500) TO REQU-KVRADER-MAX SO THAT THE                    
315200*    LENGTH IS CALCULATED CORRECTLY TO BE ABLE TO FETCH ALL               
315300*    POSSIBLE INPUT                                                       
315400     MOVE MAX-IX     TO REQU-KVRADER-MAX1                                 
315500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
315600                                                                          
315700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
315800                                                                          
315900     IF SUB-KDRC > 0                                                      
316000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
316100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
316200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
316300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
316400     END-IF                                                               
316500     .                                                                    
316600     SKIP3                                                                
316700                                                                          
316800 S99-RETURN-RESPONSE SECTION.                                             
316900     MOVE 'S99-RETURN-RESPONSE'   TO WS-CURRENT-SECTION                   
317000                                                                          
317100     MOVE 'RETURN'                  TO SUB-KDFUNC                         
317200     MOVE LENGTH OF RESP-AREA       TO SUB-KVDLEN                         
317300                                                                          
317400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
317500                                                                          
317600     IF SUB-KDRC > 0                                                      
317700       MOVE SUB-KDRC       TO KDRC-DISPLAY                                
317800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
317900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
318000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
318100     END-IF                                                               
318200     .                                                                    
318300     EJECT                                                                
318400                                                                          
318500*** IMS SECTIONS ***                                                      
318600                                                                          
318700 IMS-INSERT-TRANS4349 SECTION.                                            
318800                                                                          
318900     MOVE LOW-VALUE            TO 4349-Z1                                 
319000                                  4349-Z2                                 
319100     MOVE '  '                 TO GOOD-STATUSCODES                        
319200     CALL CBLTDLI USING ISRT 4349-PCB 4349-MSG-IO-AREA                    
319300     MOVE 4349-STATUS-CODE   TO STATUS-WS                                 
319400     PERFORM IMS-STATUS-CHECK                                             
319500     .                                                                    
319600     EJECT                                                                
319700                                                                          
319800 IMS-GU-WDK5     SECTION.                                                 
319900     MOVE 'IMS-GU-WDK5'   TO WS-CURRENT-IMS-SECTION                       
320000                                                                          
320100     STRING 'WDK501  (KDKOLLI  =' W-WDK501KY-X ')'                        
320200            DELIMITED BY SIZE INTO SSA1                                   
320300     MOVE '  GE' TO GOOD-STATUSCODES                                      
320400     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
320500     MOVE WDK5-STATUS-CODE TO STATUS-WS                                   
320600     PERFORM IMS-STATUS-CHECK                                             
320700     .                                                                    
320800     SKIP2                                                                
320900 IMS-GHU-WDE601 SECTION.                                                  
321000     MOVE 'IMS-GHU-WDE601'  TO WS-CURRENT-IMS-SECTION                     
321100                                                                          
321200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
321300          DELIMITED BY SIZE INTO SSA1                                     
321400     MOVE '  ' TO GOOD-STATUSCODES                                        
321500     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
321600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
321700     PERFORM IMS-STATUS-CHECK                                             
321800     .                                                                    
321900     SKIP3                                                                
322000 IMS-GU-WDE601 SECTION.                                                   
322100     MOVE 'IMS-GU-WDE601'  TO WS-CURRENT-IMS-SECTION                      
322200                                                                          
322300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
322400          DELIMITED BY SIZE INTO SSA1                                     
322500     MOVE '  GE' TO GOOD-STATUSCODES                                      
322600     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
322700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
322800     PERFORM IMS-STATUS-CHECK                                             
322900     .                                                                    
323000     SKIP3                                                                
323100 IMS-GU-WDE611 SECTION.                                                   
323200     MOVE 'IMS-GU-WDE611'  TO WS-CURRENT-IMS-SECTION                      
323300                                                                          
323400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
323500          DELIMITED BY SIZE INTO SSA1                                     
323600     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X ')'                      
323700          DELIMITED BY SIZE INTO SSA2                                     
323800     MOVE '  GE' TO GOOD-STATUSCODES                                      
323900     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
324000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
324100     PERFORM IMS-STATUS-CHECK                                             
324200     .                                                                    
324300     SKIP3                                                                
324400 IMS-REPL-WDE601 SECTION.                                                 
324500     MOVE 'IMS-REPL-WDE601'  TO WS-CURRENT-IMS-SECTION                    
324600                                                                          
324700     MOVE '  ' TO GOOD-STATUSCODES                                        
324800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
324900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
325000     PERFORM IMS-STATUS-CHECK                                             
325100*    ADD +3 TO UPDATE-IX                                                  
325200     .                                                                    
325300     EJECT                                                                
325400 IMS-GHU-WDE611-DEF SECTION.                                              
325500                                                                          
325600     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
325700          DELIMITED BY SIZE INTO SSA1                                     
325800     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X                          
325900                    '&KDKOLSTA =' W-KDKOLSTA-X ')'                        
326000          DELIMITED BY SIZE INTO SSA2                                     
326100     MOVE '  GE' TO GOOD-STATUSCODES                                      
326200     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
326300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
326400     PERFORM IMS-STATUS-CHECK                                             
326500     .                                                                    
326600     SKIP3                                                                
326700 IMS-GHNP-WDE611-OKVAL SECTION.                                           
326800     MOVE 'IMS-GHNP-WDE611-OKVAL'  TO WS-CURRENT-IMS-SECTION              
326900                                                                          
327000     MOVE 'WDE611 ' TO SSA1                                               
327100     MOVE '  GE' TO GOOD-STATUSCODES                                      
327200     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1                  
327300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
327400     PERFORM IMS-STATUS-CHECK                                             
327500*    ADD +2 TO UPDATE-IX                                                  
327600     .                                                                    
327700     SKIP3                                                                
327800 IMS-GHNP-WDE611 SECTION.                                                 
327900     MOVE 'IMS-GHNP-WDE611'  TO WS-CURRENT-IMS-SECTION                    
328000                                                                          
328100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X ')'                      
328200          DELIMITED BY SIZE INTO SSA1                                     
328300     MOVE '  GE' TO GOOD-STATUSCODES                                      
328400     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1                  
328500     MOVE WDE6-STATUS-CODE TO STATUS-WS-E611                              
328600     PERFORM IMS-STATUS-CHECK                                             
328700*    ADD +2 TO UPDATE-IX                                                  
328800     .                                                                    
328900     SKIP3                                                                
329000 IMS-ISRT-WDE611 SECTION.                                                 
329100     MOVE 'IMS-ISRT-WDE611'  TO WS-CURRENT-IMS-SECTION                    
329200                                                                          
329300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
329400          DELIMITED BY SIZE INTO SSA1                                     
329500     MOVE 'WDE611 ' TO SSA2                                               
329600     MOVE '  II' TO GOOD-STATUSCODES                                      
329700     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE611 SSA1 SSA2             
329800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
329900     PERFORM IMS-STATUS-CHECK                                             
330000*    ADD +2 TO UPDATE-IX                                                  
330100     .                                                                    
330200     SKIP2                                                                
330300 IMS-REPL-WDE611    SECTION.                                              
330400     MOVE 'IMS-REPL-WDE611'  TO WS-CURRENT-IMS-SECTION                    
330500                                                                          
330600     MOVE   'WDE611   '       TO   SSA1                                   
330700     MOVE '    ' TO GOOD-STATUSCODES                                      
330800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
330900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
331000     PERFORM IMS-STATUS-CHECK                                             
331100*    ADD +2 TO UPDATE-IX                                                  
331200     .                                                                    
331300     SKIP3                                                                
331400 IMS-DLET-WDE611    SECTION.                                              
331500     MOVE 'IMS-DLET-WDE611'  TO WS-CURRENT-IMS-SECTION                    
331600                                                                          
331700     MOVE '    ' TO GOOD-STATUSCODES                                      
331800     CALL CBLTDLI USING DLET WDE6-PCB DLI-IO-WDE611                       
331900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
332000     PERFORM IMS-STATUS-CHECK                                             
332100*    ADD +2 TO UPDATE-IX                                                  
332200     .                                                                    
332300     SKIP3                                                                
332400 IMS-ISRT-WDE621 SECTION.                                                 
332500                                                                          
332600     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
332700          DELIMITED BY SIZE INTO SSA1                                     
332800     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X ')'                      
332900          DELIMITED BY SIZE INTO SSA2                                     
333000     MOVE 'WDE621 ' TO SSA3                                               
333100     MOVE '  II' TO GOOD-STATUSCODES                                      
333200     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
333300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
333400     PERFORM IMS-STATUS-CHECK                                             
333500     .                                                                    
333600     EJECT                                                                
333700 IMS-GHU-WDE401 SECTION.                                                  
333800     MOVE 'IMS-GHU-WDE401'  TO WS-CURRENT-IMS-SECTION                     
333900                                                                          
334000     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
335000          DELIMITED BY SIZE INTO SSA1                                     
335100     MOVE '  GE' TO GOOD-STATUSCODES                                      
335200     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE401 SSA1                   
335300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
335400     PERFORM IMS-STATUS-CHECK                                             
335500     .                                                                    
335600 IMS-GHNP-WDE411 SECTION.                                                 
335700     MOVE 'IMS-GHNP-WDE411'  TO WS-CURRENT-IMS-SECTION                    
335800                                                                          
335900     STRING 'WDE411  (IDPURAD >=' W-IDPURAD-MIN-X                         
336000                    '&IDPURAD <=' W-IDPURAD-MAX-X ')'                     
336100          DELIMITED BY SIZE INTO SSA1                                     
336200     MOVE '  GE' TO GOOD-STATUSCODES                                      
336300     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE411 SSA1                  
336400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
336500     PERFORM IMS-STATUS-CHECK                                             
336600     .                                                                    
336700     SKIP3                                                                
336800 IMS-GHNP-WDE411-FIRST SECTION.                                           
336900     MOVE 'IMS-GHNP-WDE411-FIRST'  TO WS-CURRENT-IMS-SECTION              
337000                                                                          
337100     STRING 'WDE411  *F(IDPURAD >=' W-IDPURAD-MIN-X                       
337200                      '&IDPURAD <=' W-IDPURAD-MAX-X ')'                   
337300          DELIMITED BY SIZE INTO SSA1                                     
337400     MOVE '  GE' TO GOOD-STATUSCODES                                      
337500     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE411 SSA1                  
337600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
337700     PERFORM IMS-STATUS-CHECK                                             
337800     .                                                                    
337900     SKIP3                                                                
338000 IMS-GHNP-WDE40111 SECTION.                                               
338100     MOVE 'IMS-GHNP-WDE411'  TO WS-CURRENT-IMS-SECTION                    
338200                                                                          
338300     STRING 'WDE401  *D(WDE401KY =' W-WDE401KY-X ')'                      
338400          DELIMITED BY SIZE INTO SSA1                                     
338500     STRING 'WDE411  (IDPURAD >=' W-IDPURAD-MIN-X                         
338600                    '&IDPURAD <=' W-IDPURAD-MAX-X ')'                     
338700          DELIMITED BY SIZE INTO SSA2                                     
338800     MOVE '  GE' TO GOOD-STATUSCODES                                      
338900     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE40111 SSA1 SSA2           
339000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
339100     PERFORM IMS-STATUS-CHECK                                             
339200     .                                                                    
339300     SKIP3                                                                
339400 IMS-GHNP-WDE40111-FIRST SECTION.                                         
339500     MOVE 'IMS-GHNP-WDE411-FIRST'  TO WS-CURRENT-IMS-SECTION              
339600                                                                          
339700     STRING 'WDE401  *D(WDE401KY =' W-WDE401KY-X ')'                      
339800          DELIMITED BY SIZE INTO SSA1                                     
339900     STRING 'WDE411  *F(IDPURAD >=' W-IDPURAD-MIN-X                       
340000                      '&IDPURAD <=' W-IDPURAD-MAX-X ')'                   
340100          DELIMITED BY SIZE INTO SSA2                                     
340200     MOVE '  GE' TO GOOD-STATUSCODES                                      
340300     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE40111 SSA1 SSA2           
340400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
340500     PERFORM IMS-STATUS-CHECK                                             
340600     .                                                                    
340700     SKIP3                                                                
340800 IMS-REPL-WDE40111 SECTION.                                               
340900     MOVE 'IMS-REPL-WDE40111'  TO WS-CURRENT-IMS-SECTION                  
341000                                                                          
341100     MOVE '  ' TO GOOD-STATUSCODES                                        
341200     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE40111                     
341300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
341400     PERFORM IMS-STATUS-CHECK                                             
341500     .                                                                    
341600     SKIP3                                                                
341700 IMS-REPL-WDE411 SECTION.                                                 
341800     MOVE 'IMS-REPL-WDE411'  TO WS-CURRENT-IMS-SECTION                    
341900                                                                          
342000     MOVE '  ' TO GOOD-STATUSCODES                                        
342100     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE411                       
342200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
342300     PERFORM IMS-STATUS-CHECK                                             
342400*    ADD +3 TO UPDATE-IX                                                  
342500     .                                                                    
342600     SKIP3                                                                
342700 IMS-GHNP-WDE421-KVAL  SECTION.                                           
342800     MOVE 'IMS-GHNP-WDE421-KVAL'  TO WS-CURRENT-IMS-SECTION               
342900                                                                          
343000     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
343100            DELIMITED BY SIZE INTO SSA1                                   
343200     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
343300                DELIMITED BY SIZE INTO SSA2                               
343400     MOVE '  GE'     TO GOOD-STATUSCODES                                  
343500     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE421 SSA1 SSA2             
343600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
343700     PERFORM IMS-STATUS-CHECK                                             
343800     SKIP3                                                                
343900     .                                                                    
344000 IMS-GHNP-WDE421  SECTION.                                                
344100                                                                          
344200     MOVE 'WDE421' TO SSA1                                                
344300     MOVE '  '     TO GOOD-STATUSCODES                                    
344400     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE421 SSA1                  
344500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
344600     PERFORM IMS-STATUS-CHECK                                             
344700     SKIP3                                                                
344800     .                                                                    
344900 IMS-REPL-WDE421    SECTION.                                              
345000     MOVE 'IMS-REPL-WDE421'  TO WS-CURRENT-IMS-SECTION                    
345100                                                                          
345200     MOVE '    ' TO GOOD-STATUSCODES                                      
345300     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE421                       
345400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
345500     PERFORM IMS-STATUS-CHECK                                             
345600*    ADD +2 TO UPDATE-IX                                                  
345700     .                                                                    
345800     SKIP2                                                                
345900 IMS-ISRT-WDE421 SECTION.                                                 
346000     MOVE 'IMS-ISRT-WDE421'  TO WS-CURRENT-IMS-SECTION                    
346100                                                                          
346200     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
346300          DELIMITED BY SIZE INTO SSA1                                     
346400     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
346500            DELIMITED BY SIZE INTO SSA2                                   
346600     MOVE 'WDE421'    TO SSA3                                             
346700     MOVE '  II' TO GOOD-STATUSCODES                                      
346800     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE421 SSA1 SSA2 SSA3        
346900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
347000     PERFORM IMS-STATUS-CHECK                                             
347100*    ADD +2 TO UPDATE-IX                                                  
347200     .                                                                    
347300     SKIP3                                                                
347400 IMS-DLET-WDE421  SECTION.                                                
347500     MOVE 'IMS-DLET-WDE421'  TO WS-CURRENT-IMS-SECTION                    
347600                                                                          
347700     MOVE '    '   TO GOOD-STATUSCODES                                    
347800     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-WDE421                       
347900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
348000     PERFORM IMS-STATUS-CHECK                                             
348100*    ADD +2 TO UPDATE-IX                                                  
348200     .                                                                    
348300     SKIP2                                                                
348400 IMS-GU-WDE4F1-PLK-NE SECTION.                                            
348500     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
348600                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
348700                    '&IDPLKLSTNE' W-IDPLKLST-X ')'                        
348800          DELIMITED BY SIZE INTO SSA1                                     
348900     MOVE '  GE' TO GOOD-STATUSCODES                                      
349000     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE4F1 SSA1                   
349100     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
349200     PERFORM IMS-STATUS-CHECK                                             
349300     .                                                                    
349400     SKIP3                                                                
349500 IMS-GU-WDE4F1-PLK SECTION.                                               
349600     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
349700                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
349800                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
349900          DELIMITED BY SIZE INTO SSA1                                     
350000     MOVE '  GE' TO GOOD-STATUSCODES                                      
350100     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE4F1 SSA1                   
350200     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
350300     PERFORM IMS-STATUS-CHECK                                             
350400     .                                                                    
350500     SKIP3                                                                
350600 IMS-GU-WDGX4726 SECTION.                                                 
350700     STRING 'WLXXDV01(WDGXKEY  =' W-WDGXKEY-4726-X ')'                    
350800            DELIMITED BY SIZE INTO SSA1                                   
350900     MOVE '  ' TO GOOD-STATUSCODES                                        
351000     CALL CBLTDLI USING GU     XXDV-PCB DLI-IO-GX01 SSA1                  
351100     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
351200     PERFORM IMS-STATUS-CHECK                                             
351300     SKIP2                                                                
351400     .                                                                    
351500 IMS-GNP-WDGX4727 SECTION.                                                
351600     STRING 'WLXXDV11(WDGXKEY  =' W-WDGXKEY-4727-X ')'                    
351700            DELIMITED BY SIZE INTO SSA1                                   
351800     MOVE '  GE' TO GOOD-STATUSCODES                                      
351900     CALL CBLTDLI USING GNP    XXDV-PCB DLI-IO-4726 SSA1                  
352000     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
352100     PERFORM IMS-STATUS-CHECK                                             
352200     SKIP2                                                                
352300     .                                                                    
352400 IMS-ISRT-WDGX4726 SECTION.                                               
352500     STRING 'WLXXDV01(WDGXKEY  =' W-WDGXKEY-4726-X ')'                    
352600            DELIMITED BY SIZE INTO SSA1                                   
352700     MOVE 'WLXXDV11 ' TO SSA2                                             
352800     MOVE '  ' TO GOOD-STATUSCODES                                        
352900     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-4726 SSA1 SSA2               
353000     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
353100     PERFORM IMS-STATUS-CHECK                                             
353200*    ADD +1 TO UPDATE-IX                                                  
353300     .                                                                    
353400     SKIP2                                                                
353500 IMS-ISRT-WDGX4727 SECTION.                                               
353600     STRING 'WLXXDV01(WDGXKEY  =' W-WDGXKEY-4726-X ')'                    
353700            DELIMITED BY SIZE INTO SSA1                                   
353800     STRING 'WLXXDV11(WDGXKEY  =' W-WDGXKEY-4727-X ')'                    
353900            DELIMITED BY SIZE INTO SSA2                                   
354000     MOVE 'WLXXDV21 ' TO SSA3                                             
354100     MOVE '  II' TO GOOD-STATUSCODES                                      
354200     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-4727 SSA1 SSA2 SSA3          
354300     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
354400     PERFORM IMS-STATUS-CHECK                                             
354500*    ADD +1 TO UPDATE-IX                                                  
354600     .                                                                    
354700     SKIP2                                                                
354800 IMS-GU-WDQ301-DSEQ SECTION.                                              
354900     MOVE 'IMS-GHU-WDQ301-DSEQ'  TO WS-CURRENT-IMS-SECTION                
355000                                                                          
355100     STRING 'WDQ301  (WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
355200          DELIMITED BY SIZE INTO SSA1                                     
355300     MOVE '  GE' TO GOOD-STATUSCODES                                      
355400     CALL CBLTDLI USING GU WDQ3D-PCB DLI-IO-WDQ301 SSA1                   
355500     MOVE WDQ3D-STATUS-CODE TO STATUS-WS                                  
355600     PERFORM IMS-STATUS-CHECK                                             
355700     .                                                                    
355800     SKIP3                                                                
355900 IMS-GHU-WDQ212 SECTION.                                                  
356000     MOVE 'IMS-GHU-WDQ212'  TO WS-CURRENT-IMS-SECTION                     
356100                                                                          
356200     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
356300          DELIMITED BY SIZE INTO SSA1                                     
356400     STRING 'WDQ212  (IDDC     =' W-212-IDDC-X ')'                        
356500          DELIMITED BY SIZE INTO SSA2                                     
356600     MOVE '    ' TO GOOD-STATUSCODES                                      
356700     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-WDQ212 SSA1 SSA2              
356800     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
356900     PERFORM IMS-STATUS-CHECK                                             
357000     .                                                                    
357100     SKIP3                                                                
357200 IMS-REPL-WDQ212 SECTION.                                                 
357300     MOVE 'IMS-REPL-WDQ212'  TO WS-CURRENT-IMS-SECTION                    
357400                                                                          
357500     MOVE '  ' TO GOOD-STATUSCODES                                        
357600     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ212                       
357700     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
357800     PERFORM IMS-STATUS-CHECK                                             
357900*    ADD +4 TO UPDATE-IX                                                  
358000     .                                                                    
358100     EJECT                                                                
358200 IMS-GU-WDB601    SECTION.                                                
358300     MOVE 'IMS-GU-WDB601'  TO WS-CURRENT-IMS-SECTION                      
358400                                                                          
358500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
358600          DELIMITED BY SIZE INTO SSA1                                     
358700     MOVE '  ' TO GOOD-STATUSCODES                                        
358800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
358900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
359000     PERFORM IMS-STATUS-CHECK                                             
359100     .                                                                    
359200     SKIP3                                                                
359300 IMS-ISRT-WDG601 SECTION.                                                 
359400     MOVE 'IMS-ISRT-WDG601'  TO WS-CURRENT-IMS-SECTION                    
359500                                                                          
359600     MOVE 'WDG601  ' TO SSA1                                              
359700     MOVE '  II'     TO GOOD-STATUSCODES                                  
359800     CALL CBLTDLI USING ISRT WDG6-PCB DLI-IO-WDG601 SSA1                  
359900     MOVE WDG6-STATUS-CODE TO STATUS-WS                                   
360000     PERFORM IMS-STATUS-CHECK                                             
360100*    ADD +1 TO UPDATE-IX                                                  
360200     .                                                                    
360300     SKIP2                                                                
360400 IMS-GU-WDGX4478     SECTION.                                             
360500     MOVE 'IMS-GU-WDGX4478'  TO WS-CURRENT-IMS-SECTION                    
360600                                                                          
360700     STRING 'WLXXLB01(WDGXKEY  =' W-WDGXKEY-4477-X ')'                    
360800            DELIMITED BY SIZE INTO SSA1                                   
360900     STRING 'WLXXLB11(WDGXKEY  =' W-WDGXKEY-4478-X ')'                    
361000            DELIMITED BY SIZE INTO SSA2                                   
361100     MOVE '  GE' TO GOOD-STATUSCODES                                      
361200     CALL CBLTDLI USING GU  XXLB-PCB DLI-IO-4478 SSA1 SSA2                
361300     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
361400     PERFORM IMS-STATUS-CHECK                                             
361500     .                                                                    
361600     SKIP2                                                                
361700 IMS-GHU-WDGX4472     SECTION.                                            
361800     MOVE 'IMS-GHU-WDGX4472'  TO WS-CURRENT-IMS-SECTION                   
361900                                                                          
362000     STRING 'WLXXKW01(WDGXKEY  =' W-WDGXKEY-4471-X ')'                    
362100            DELIMITED BY SIZE INTO SSA1                                   
362200     STRING 'WLXXKW11(KDSEGKEY =' W-KDSEGKEY-4472-X ')'                   
362300            DELIMITED BY SIZE INTO SSA2                                   
362400     MOVE '  GE' TO GOOD-STATUSCODES                                      
362500     CALL CBLTDLI USING GHU  XXKW-PCB DLI-IO-4472 SSA1 SSA2               
362600     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
362700     PERFORM IMS-STATUS-CHECK                                             
362800     .                                                                    
362900     SKIP2                                                                
363000 IMS-REPL-WDGX4472    SECTION.                                            
363100     MOVE 'IMS-REPL-WDGX4472'  TO WS-CURRENT-IMS-SECTION                  
363200                                                                          
363300     MOVE '  '   TO GOOD-STATUSCODES                                      
363400     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-4472                         
363500     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
363600     PERFORM IMS-STATUS-CHECK                                             
363700*    ADD +1 TO UPDATE-IX                                                  
363800     .                                                                    
363900     SKIP2                                                                
364000 IMS-ISRT-4322-SEGM SECTION.                                              
364100     MOVE 'IMS-ISRT-WDGX4322'  TO WS-CURRENT-IMS-SECTION                  
364200                                                                          
364300     STRING 'WLXXJK01(WDGXKEY  =' W-IDHTYP-4321-X ')'                     
364400            DELIMITED BY SIZE INTO SSA1                                   
364500     MOVE 'WLXXJK11*L' TO SSA2                                            
364600     MOVE '  ' TO GOOD-STATUSCODES                                        
364700     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-4322 SSA1 SSA2               
364800     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
364900     PERFORM IMS-STATUS-CHECK                                             
365000*    ADD +1 TO UPDATE-IX                                                  
365100     .                                                                    
365200     EJECT                                                                
365300 IMS-GHN-WDA6B SECTION.                                                   
365400     MOVE 'IMS-GHN-WDA6B    '  TO WS-CURRENT-IMS-SECTION                  
365500                                                                          
365600     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
365700                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
365800            DELIMITED BY SIZE INTO SSA1                                   
365900     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
366000     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-WDA601 SSA1              
366100     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
366200     PERFORM IMS-STATUS-CHECK                                             
366300     .                                                                    
366400                                                                          
366500                                                                          
366600 IMS-REPL-WDA6B SECTION.                                                  
366700     MOVE 'IMS-REPL-WDA6B   '  TO WS-CURRENT-IMS-SECTION                  
366800                                                                          
366900     MOVE 'WDA601  '           TO SSA1                                    
367000     MOVE '    '               TO GOOD-STATUSCODES                        
367100     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-WDA601 SSA1               
367200     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
367300     PERFORM IMS-STATUS-CHECK                                             
367400     .                                                                    
367500                                                                          
367600 IMS-GU-WDK601  SECTION.                                                  
367700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
367800          DELIMITED BY SIZE INTO SSA1                                     
367900     MOVE '  GE' TO GOOD-STATUSCODES                                      
368000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
368100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
368200     PERFORM IMS-STATUS-CHECK                                             
368300     .                                                                    
368400     SKIP3                                                                
368500 IMS-GNP-WDK611  SECTION.                                                 
368600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
368700          DELIMITED BY SIZE INTO SSA1                                     
368800     MOVE '  GE' TO GOOD-STATUSCODES                                      
368900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
369000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
369100     PERFORM IMS-STATUS-CHECK                                             
369200     .                                                                    
369300     SKIP3                                                                
369400 IMS-GU-WDK611  SECTION.                                                  
369500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
369600          DELIMITED BY SIZE INTO SSA1                                     
369700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
369800          DELIMITED BY SIZE INTO SSA2                                     
369900     MOVE '  GE' TO GOOD-STATUSCODES                                      
370000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
370100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
370200     PERFORM IMS-STATUS-CHECK                                             
370300     .                                                                    
370400     SKIP3                                                                
370410 IMS-GU-WDK711 SECTION.                                                   
370420     MOVE 'WDK711' TO WS-CURRENT-IMS-SECTION                              
370430                                                                          
370450     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
370460          DELIMITED BY SIZE INTO SSA1                                     
370470     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
370480          DELIMITED BY SIZE INTO SSA2                                     
370481     MOVE '  GE' TO GOOD-STATUSCODES                                      
370491     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
370492     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
370493     PERFORM IMS-STATUS-CHECK                                             
370495     .                                                                    
370496                                                                          
370500 IMS-PURG-4333-MSG SECTION.                                               
370600                                                                          
370700     MOVE SPACE TO GOOD-STATUSCODES                                       
370800     CALL CBLTDLI USING PURG                                              
370900                        4333-PCB                                          
371000                        4333-MID-IO-AREA                                  
371100     MOVE 4333-STATUS-CODE TO STATUS-WS                                   
371200     PERFORM IMS-STATUS-CHECK                                             
371300     .                                                                    
371400     SKIP3                                                                
371500 IMS-PURG-4341-MSG SECTION.                                               
371600                                                                          
371700     MOVE SPACE TO GOOD-STATUSCODES                                       
371800     CALL CBLTDLI USING PURG                                              
371900                        4341-PCB                                          
372000                        4341-MID-IO-AREA                                  
372100     MOVE 4341-STATUS-CODE TO STATUS-WS                                   
372200     PERFORM IMS-STATUS-CHECK                                             
372300     SKIP3                                                                
372400     .                                                                    
372500 IMS-ROLLBACK    SECTION.                                                 
372600     SKIP2                                                                
372700     MOVE '  ' TO GOOD-STATUSCODES                                        
372800     CALL CBLTDLI USING ROLB   MSG-PCB                                    
372900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
373000     PERFORM IMS-STATUS-CHECK                                             
373100     SKIP2                                                                
373200     .                                                                    
373300 IMS-STATUS-CHECK SECTION.                                                
373400                                                                          
373500     SET STATUS-IX TO 1                                                   
373600     SEARCH GOOD-STATUS                                                   
373700         AT END  CALL FELLOG                                              
373800         WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS   CONTINUE              
373900     END-SEARCH                                                           
374000     .                                                                    
375000                                                                          
