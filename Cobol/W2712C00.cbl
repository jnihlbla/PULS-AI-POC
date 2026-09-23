000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2712C00.                                                
000400 AUTHOR.         NIHLBLAD JOHAN.                                          
000500 DATE-WRITTEN.   26/01/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER WDK7 MED SB.                                    
001100*        PROGRAM FÖR ATT TA HAND OM HUR KVARVARANDE PASSIVA               
001200*        ARTIKLAR PÅ ETT DC SKALL HANTERAS.                               
001300*        1. TRANSFER                                                      
001400*        2. RETUR                                                         
001500*        3. SKROT                                                         
001600*                                                                         
001700*    UTFIL:                                                               
001800*        3.  W271.W271D4.W2712D(+1) ARTIKLAR FÖR AUTO.SKROTNING           
001900*                                   TILL PROGRAM W2712D00.                
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- UTFIL TILL W2711100 PGM MED PASSIVRETURER                  
003400     SELECT W27117                     ASSIGN TO W2712CD1.                
003500     EJECT                                                                
003600*          --- UTFIL TILL W2712D00 PGM MED PASSIVSKROTNING.               
003700     SELECT W2712D                     ASSIGN TO W2712CD2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W27117                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W27111 -PRE  UT-  -L.                                     
004800     SKIP3                                                                
004900 FD  W2712D                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W2712D -PRE  UT2-  -L.                                    
005400                                                                          
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800*    -COPY WY2000W1                                                       
005900     SKIP3                                                                
006000                                                                          
006100 77  IDPGM                       PIC X(8)    VALUE 'W2712C00'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400 77  YES                         PIC X       VALUE 'Y'.                   
006500 77  NOO                         PIC X       VALUE 'N'.                   
006600 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
006700 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
006800 77  IX                          PIC 9(3)    VALUE ZERO.                  
006900 77  IX-REC                      PIC 9(3)    VALUE ZERO.                  
007000 77  OI-MAX-IX                   PIC 9(3)    VALUE ZERO.                  
007100 77  WS-IDDC-IX                  PIC 9(3)    VALUE ZERO.                  
007200 77  MAX-IDDC-IX                 PIC 9(3)    VALUE ZERO.                  
007300 77  WS-IDARTNR-SEND             PIC 9(9)    VALUE ZERO.                  
007400 77  WS-IDARTNR-NUM              PIC Z(8)9   VALUE ZERO.                  
007500 77  WS-DC-SEND                  PIC X(2)    VALUE SPACE.                 
007600 77  WS-DC-REC                   PIC X(2)    VALUE ZERO.                  
007700 77  WS-TIVV                     PIC 9(4)    VALUE ZERO.                  
007800 77  WS-WEEKS                    PIC 9(4)    VALUE ZERO.                  
007900 77  WS-BIN-WEEKS                PIC 9(4)    VALUE ZERO.                  
008000 77  WS-PASSIVE-WEEKS            PIC 9(4)    VALUE ZERO.                  
008100 77  WS-TIREFEFT                 PIC 9(6)    VALUE ZERO.                  
008200 77  WS-TIINLINL                 PIC 9(6)    VALUE ZERO.                  
008300 77  WS-EXTER-SUPPLIER           PIC X(1)    VALUE 'N'.                   
008400                                                                          
008500*01    -COPY WWDCKONS                                                     
008600                                                                          
008700     EJECT                                                                
008800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009300     EJECT                                                                
009400 01  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                      
009500 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
009600     03 DAGENS-TIAAVVD-AA    PIC  9(2).                                   
009700     03 DAGENS-TIAAVVD-VV    PIC  9(2).                                   
009800     03 DAGENS-TIAAVVD-D     PIC  9(1).                                   
009900                                                                          
010000                                                                          
010100 01  ARBETSAREA.                                                          
010200     03 WS-RETUR-ADLAGOMR           PIC 9(2)    VALUE ZERO.               
010300     03 WS-RETUR-ADGANG             PIC 9(2)    VALUE ZERO.               
010400     03 WS-TRANS-ADLAGOMR           PIC 9(2)    VALUE ZERO.               
010500     03 WS-FRYSTID                  PIC 9(3)    VALUE ZERO.               
010600     03 WS-TIAAVVD                  PIC 9(5)    VALUE ZERO.               
010700     03 WS-TIAAVVD-MOTTAG           PIC 9(5)    VALUE ZERO.               
010800     03 WS-TIAAVVD-SALES            PIC 9(5)    VALUE ZERO.               
010900     03 WS-TIAAVV-SALES             PIC 9(4)    VALUE ZERO.               
011000     03 WS-TIAAAAVV-SALES           PIC 9(6)    VALUE ZERO.               
011100     03 WS-ANTAL-RETUR              PIC S9(6)   VALUE ZERO COMP-3.        
011200     03 WS-ANTAL-KVAR               PIC S9(6)   VALUE ZERO COMP-3.        
011300     03 WS-KVBEART                  PIC S9(7)   VALUE ZERO COMP-3.        
011400     03 WS-TIFINLV                  PIC 9(7)    VALUE ZERO.               
011500     03 WS-TIFINLV-AAAAVV           PIC 9(6)    VALUE ZERO.               
011600     03 ARTWS-TIFINLV               PIC 9(5)  VALUE ZERO.                 
011700     03 W-TILLG-SDC                 PIC S9(7)   COMP-3.                   
011800     03 W-BEHOV-SDC                 PIC S9(7)   COMP-3.                   
011900     03 WS-ANTAL-QX                 PIC S9(7) VALUE ZERO COMP-3.          
012000     03 WS-VARDE-RETUR              PIC  9(9)   VALUE ZERO.               
012100     03 WS-VALUE-KVLS               PIC  9(9)   VALUE ZERO.               
012200     03 WS-DAGENS-AAVV-ERS          PIC  9(4)   VALUE ZERO.               
012300     03 WS-TIFINLV-AAVVD            PIC  9(5)   VALUE ZERO.               
012400     03 WS-DAGENS-AAAAVV-FT         PIC  9(6)   VALUE ZERO.               
012500     03 WS-DAGENS-AAAAVV-MINUS-26V  PIC  9(6)   VALUE ZERO.               
012600     03 WS-DAGENS-AAVV-MINUS-SALES    PIC 9(4)    VALUE ZERO.             
012700     03 WS-DAGENS-AAAAVV-MINUS-SALES  PIC  9(6)   VALUE ZERO.             
012800     03 WS-BALANCE-SEND-DC          PIC S9(7)   VALUE ZERO COMP-3.        
012900     03 WS-TILLG-SKROT              PIC S9(7)   VALUE ZERO COMP-3.        
013000     03 WS-KVSKROT                  PIC S9(7)   VALUE ZERO COMP-3.        
013100     03 WS-KVBEART-SKROT            PIC S9(7)   VALUE ZERO COMP-3.        
013200     03 WS-VALUE-KVSKROT       PIC S9(9)V9(2)   VALUE ZERO COMP-3.        
013300     03 SPAR-TISKROT-AUTO           PIC  9(6)   VALUE ZERO.               
013400     03 WS-KVVECKOR-TPAS            PIC S9(3)   VALUE ZERO COMP-3.        
013500     03 TRANS-KVPB-TOT              PIC 9(6)V9(1).                        
013600     03 WS-FLORDSP                  PIC X(1)    VALUE SPACE.              
013700     03 WS-FLSPBULK                 PIC X(1)    VALUE SPACE.              
013800     03 WS-FLSKROT-BEORD            PIC X(1)    VALUE SPACE.              
013900     03 WS-KDLEVSP                  PIC S9(3)   VALUE ZERO COMP-3.        
014000     03 WS-KVSPARR-KVAL             PIC S9(7)   VALUE ZERO COMP-3.        
014100     03 WS-KVVECKOR-BIN             PIC  9(3)   VALUE ZERO.               
014200     03 WS-PRART-SPAS               PIC S9(7)   VALUE ZERO COMP-3.        
014300     03 WS-DATE-NUM9                PIC  9(6)   VALUE ZERO.               
014310     03 WS-KVPB-TOT-CDC             PIC 9(6)V9(2)  VALUE ZERO.            
014320     03 WS-KVPB-TOT-XDC             PIC 9(6)V9(2)  VALUE ZERO.            
014330     03 WS-KVPB-TOT-CDC-XDC         PIC 9(6)V9(2)  VALUE ZERO.            
014340     03 WS-KVPB-TOT-1V-SEND-DC      PIC 9(6)V9(2)  VALUE ZERO.            
014350     03 WS-KVPB-TOT-30V-SEND-DC     PIC 9(8)    VALUE ZERO.               
014400                                                                          
014500 01    WS-IDDC-TABELL.                                                    
014600    03 WS-VALID-IDDC  OCCURS 300 ASCENDING KEY IS TAB-IDDC                
014700                      INDEXED BY INDX.                                    
014800       05 TAB-IDDC            PIC X(2).                                   
014900       05 TAB-KDDC            PIC X(2).                                   
015000       05 TAB-TID-TRS         PIC X(2).                                   
015100       05 TAB-TIVV            PIC 9(2).                                   
015200       05 TAB-FLTRANS-PAS     PIC X(1).                                   
015300       05 TAB-IDDC-TPAS-1     PIC X(2).                                   
015400       05 TAB-IDDC-TPAS-2     PIC X(2).                                   
015500       05 TAB-IDDC-TPAS-3     PIC X(2).                                   
015600       05 TAB-SUARTMIN-TPAS   PIC 9(6).                                   
015700       05 TAB-KVPERIOD-TPAS   PIC 9(2).                                   
015800       05 TAB-KVVECKOR-TPAS   PIC 9(2).                                   
015900       05 TAB-SUVARLIM-TPAS   PIC 9(6).                                   
016000       05 TAB-KVPB-LIM        PIC 9(6)V9(1).                              
016100       05 TAB-FLTRANS-ERS     PIC X(1).                                   
016200       05 TAB-FLRETUR-PAS     PIC X(1).                                   
016300       05 TAB-SUARTMIN-RPAS   PIC 9(6).                                   
016400       05 TAB-KVVECKOR-RPAS   PIC 9(2).                                   
016500       05 TAB-FLSKROT-PAS     PIC X(1).                                   
016600       05 TAB-KVSKROT-SPAS    PIC 9(7).                                   
016700       05 TAB-IDKUNDNR-TRETUR PIC 9(6).                                   
016800       05 TAB-IDDISTR-SKROT   PIC 9(4).                                   
016900       05 TAB-IDKUNDNR-SKROT  PIC 9(6).                                   
017000       05 TAB-IDDISTR-QSKROT  PIC 9(4).                                   
017100       05 TAB-IDKUNDNR-QSKROT PIC 9(6).                                   
017200       05 TAB-IDDISTR-RSKROT  PIC 9(4).                                   
017300       05 TAB-IDKUNDNR-RSKROT PIC 9(6).                                   
017400       05 TAB-ANTAL-SKROT-DC  PIC 9(7).                                   
017500       05 TAB-IDLANDX2        PIC X(2).                                   
017600       05 TAB-IDLEVNR-DC      PIC X(5).                                   
017700       05 TAB-KVVECKOR-BIN    PIC 9(3).                                   
017800       05 TAB-KVVECKOR-SPAS   PIC 9(3).                                   
017900       05 TAB-IDTECKEN-SPAS   PIC X(1).                                   
018000       05 TAB-PRARTSTD-SPAS   PIC 9(7).                                   
018100       05 TAB-ADLAGOMR-SPAS   PIC 9(3).                                   
018200       05 TAB-IDPERSON-SPAS   PIC 9(3).                                   
018300       05 TAB-KDPRODSL-SPAS   PIC 9(3).                                   
018400                                                                          
018500 01 NYCKLAR-TP4TRAN.                                                      
018600     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
018700     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
018800*                                                                         
018900 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
019000     88  TRAFF-OK                            VALUE 'J'.                   
019100     88  NO-TRAFF                            VALUE 'N'.                   
019200*                                                                         
019300 77  TRAFF-VECKA-SW              PIC X       VALUE 'N'.                   
019400     88  TRAFF-VECKA                         VALUE 'J'.                   
019500     88  NO-TRAFF-VECKA                      VALUE 'N'.                   
019600*                                                                         
019700 77  RETUR-SW                    PIC X       VALUE 'N'.                   
019800     88  RETUR                               VALUE 'J'.                   
019900     88  NO-RETUR                            VALUE 'N'.                   
020000*                                                                         
020100 77  AVROP-SW                    PIC X       VALUE 'N'.                   
020200     88  AVROP                               VALUE 'J'.                   
020300     88  NO-AVROP                            VALUE 'N'.                   
020400*                                                                         
020500 77  ERS-PUBVECKA-SW             PIC X       VALUE 'J'.                   
020600     88  ERS-PUBVECKA-OK                     VALUE 'J'.                   
020700     88  ERS-PUBVECKA-EJ-OK                  VALUE 'N'.                   
020800*                                                                         
020900 77  TRANSFER-SW                 PIC X       VALUE 'N'.                   
021000     88  TRANSFER                            VALUE 'J'.                   
021100     88  NO-TRANSFER                         VALUE 'N'.                   
021200*                                                                         
021300 77  SKRIV-SKROT-SW              PIC X       VALUE 'N'.                   
021400     88  PASSIV-SKROT-OK                     VALUE 'J'.                   
021500     88  NO-PASSIV-SKROT                     VALUE 'N'.                   
021600*                                                                         
021700 77  WS-DEMD-DATE-PASSED-SW      PIC X       VALUE 'N'.                   
021800     88  DEMD-DATE-PASSED                    VALUE 'J'.                   
021900*                                                                         
022000 77  WS-LAST-BIN-DATE-PASS-SW    PIC X       VALUE 'N'.                   
022100     88  BIN-DATE-PASSED                     VALUE 'J'.                   
022200*                                                                         
022300*01  -COPY WWPRODSL                                                       
022400*                                                                         
022500*      --- VALID IDDC CODES                                               
022600*01    -COPY WWDC99                                                       
022700*                                                                         
022800 01  DYNAMISKA-SUBPROGRAM.                                                
022900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
023000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
023100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
023200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
023300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
023400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
023500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023700     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
023710     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
023800     SKIP2                                                                
023900*    --- PARAMETRAR TILL ABEND                                            
024000                                                                          
024100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
024200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
024300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
024400     SKIP2                                                                
024500 01  FELTEXT.                                                             
024600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
024700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
024800     EJECT                                                                
024810*    --- PARAMETRAR TILL W271UTIL                                         
024820*01 -COPY W271UTIL                                                        
024830     EJECT                                                                
024900*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
025000*01  -COPY WZ20DAYS                                                       
025100*    --- PARAMETRAR TILL DATKORT                                          
025200*                                                                         
025300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2712C'.              
025400     SKIP2                                                                
025500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
025600     SKIP2                                                                
025700*01  -COPY WDATKORT                                                       
025800     EJECT                                                                
025900*    --- PARAMETRAR TILL WORKDAY                                          
026000*                                                                         
026100*01  -COPY WORKAREA                                                       
026200     EJECT                                                                
026300*    --- PARAMETRAR TILL VECKOADD                                         
026400                                                                          
026500 01  W009VADD-AREA.                                                       
026600     03 VADD-DATUM-AAVV          PIC S9(5) VALUE ZERO COMP-3.             
026700     03 VADD-ANTAL               PIC S9(3) VALUE ZERO COMP-3.             
026800                                                                          
026900*    --- PARAMETRAR TILL POSTSUM                                          
027000*                                                                         
027100*01  -COPY W0005   -PRE  POSTSUM-                                         
027200     EJECT                                                                
027300*01  -COPY WDATAREA                                                       
027400     EJECT                                                                
027500*01    -COPY WWBYT03                                                      
027600     EJECT                                                                
027700 01  UT-AREA-START               PIC X(24)   VALUE                        
027800                                 'UT-AREA-START  '.                       
027900     SKIP2                                                                
028000                                                                          
028100*01  AREA -COPY W27111     -PRE UT-                                       
028200     EJECT                                                                
028300                                                                          
028400*01  AREA -COPY W2712D     -PRE UT2-                                      
028500     EJECT                                                                
028600*******************************************                               
028700 01  NYCKLAR-TILL-DLI.                                                    
028800     03  W-IDDC-X.                                                        
028900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
029000     03  W-IDDC-REF-X.                                                    
029100         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
029200     03  W-IDLANDX2-X.                                                    
029300         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
029400     03  W-IDARTNR-ERS-X.                                                 
029500         05  W-IDARTNR-ERS       PIC S9(9)   VALUE ZERO COMP-3.           
029600     03  W-IDARTNR-X.                                                     
029700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029800     03  W-WDD901KY-X.                                                    
029900         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
030000         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
030100     03  W-KDSEGKEY-X.                                                    
030200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
030300     03  W-IDLEVNR-X.                                                     
030400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
030500     03  W-KDAVROP-X.                                                     
030600         05  W-KDAVROP           PIC S9(1)   VALUE 2    COMP-3.           
030700     03  W-IDDC-TRANSF-X.                                                 
030800         05  W-IDDC-TRANSF       PIC X(2)    VALUE SPACE.                 
030900                                                                          
031000     03  W-WDQ4B1KY-MAX-X.                                                
031100         05  W-IDARTNR-Q4B1-MAX  PIC S9(9)    COMP-3.                     
031200         05  FILLER              PIC X(32)    VALUE HIGH-VALUE.           
031300                                                                          
031400     03  W-WDQ4B1KY-MIN-X.                                                
031500         05  W-IDARTNR-Q4B1-MIN  PIC S9(9)    COMP-3.                     
031600         05  FILLER              PIC X(32)    VALUE LOW-VALUE.            
031700                                                                          
031800     03  W-IDDISTR-Q4B1-X.                                                
031900         05  W-IDDISTR-Q4B1      PIC S9(5)  VALUE ZERO COMP-3.            
032000                                                                          
032100     03  W-IDKUNDNR-Q4B1-X.                                               
032200         05  W-IDKUNDNR-Q4B1     PIC S9(7)  VALUE ZERO COMP-3.            
032300                                                                          
032400     03  W-IDDISTR-Q4B1-Q-X.                                              
032500         05  W-IDDISTR-Q4B1-Q    PIC S9(5)  VALUE ZERO COMP-3.            
032600                                                                          
032700     03  W-IDKUNDNR-Q4B1-Q-X.                                             
032800         05  W-IDKUNDNR-Q4B1-Q   PIC S9(7)  VALUE ZERO COMP-3.            
032900                                                                          
033000     03  W-IDDISTR-Q4B1-R-X.                                              
033100         05  W-IDDISTR-Q4B1-R    PIC S9(5)  VALUE ZERO COMP-3.            
033200                                                                          
033300     03  W-IDKUNDNR-Q4B1-R-X.                                             
033400         05  W-IDKUNDNR-Q4B1-R   PIC S9(7)  VALUE ZERO COMP-3.            
033500                                                                          
033600                                                                          
033700*******************************************                               
033800*    --- STATUS-KOD FRÅN IMS                                              
033900 01  STATUS-WS                   PIC XX.                                  
034000     88  SEGMENT-FINNS                       VALUE '  '.                  
034100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
034200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
034300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
034400     SKIP2                                                                
034500 01  GODK-STATUSKODER.                                                    
034600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034700     SKIP3                                                                
034800*01  SSA1                        PIC X(128).                              
034900 01  SSA1                        PIC X(400).                              
035000 01  SSA2                        PIC X(128).                              
035100 01  SSA3                        PIC X(128).                              
035200     EJECT                                                                
035300********DB2                                                               
035400 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
035500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
035600                                                                          
035700 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
035800 01  DB2-WS.                                                              
035900     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
036000         88  CURSOR-OK                      VALUE 000.                    
036100         88  LINES-FOUND                    VALUE 000.                    
036200         88  LINES-MISSING                  VALUE 100.                    
036210         88  MULTIPLE-ROWS-FOUND            VALUE 811.                    
036300         88  RESOURCE-WRONG                 VALUE 904.                    
036400     03  GOOD-SQLCODECODES.                                               
036500         05  GOOD-SQLCODE OCCURS 5                                        
036600             INDEXED BY SQLCODE-IX PIC 9(3).                              
036700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
036800     EJECT                                                                
036900*******************************************                               
037000*    --- IMS FUNKTIONSKODER                                               
037100*01  -COPY W0003                                                          
037200     EJECT                                                                
037300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
037400 01   DLI-IO-AREA-B601.                                                   
037500*     03  -COPY WDB601                                                    
037600                                                                          
037700     EJECT                                                                
037800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
037900     SKIP3                                                                
038000 01  DLI-IO-AREA-WDK601.                                                  
038100*    03  -COPY WDK601                                                     
038200     EJECT                                                                
038300                                                                          
038400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
038500     SKIP3                                                                
038600 01  DLI-IO-AREA-WDK611.                                                  
038700*    03  -COPY WDK611                                                     
038800     EJECT                                                                
038900                                                                          
039000 01  FILLER           PIC X(16)   VALUE 'DLI-IO-AREA-K7'.                 
039100*                                                                         
039200 01  DLI-IO-AREA-K7.                                                      
039300     03 IO-AREA-K7   PIC X(300)  VALUE SPACE.                             
039400     03 DLI-IO-WDK701 REDEFINES IO-AREA-K7.                               
039500*       05  -COPY WDK701                                                  
039600                                                                          
039700     03 DLI-IO-WDK711 REDEFINES IO-AREA-K7.                               
039800*       05  -COPY WDK711                                                  
039900                                                                          
040000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL701'.                      
040100 01  DLI-IO-WDL701.                                                       
040200*    03  -COPY WDL701                                                     
040300     EJECT                                                                
040400                                                                          
040500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL711'.                      
040600 01  DLI-IO-WDL711.                                                       
040700*    03  -COPY WDL711                                                     
040800     EJECT                                                                
040900                                                                          
041000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11-TRANSF'.               
041100 01  DLI-IO-ARTS11-TRANSF.                                                
041200*    03  -COPY WDK711   -PRE  TRANSF-                                     
041300     EJECT                                                                
041400                                                                          
041500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-MOTTAG'.               
041600 01  DLI-IO-WDK711-MOTTAG.                                                
041700*    03  -COPY WDK711   -PRE  MOTTAG-                                     
041800     EJECT                                                                
041900                                                                          
042000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
042100 01  DLI-IO-WDK712.                                                       
042200*    03  -COPY WDK712                                                     
042300     EJECT                                                                
042400                                                                          
042500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712-ERS'.                  
042600 01  DLI-IO-WDK712-ERS.                                                   
042700*    03  -COPY WDK712 -PRE ERS-                                           
042800     EJECT                                                                
042900                                                                          
043000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
043100 01  DLI-IO-WDK722.                                                       
043200*    03  -COPY WDK722                                                     
043300     EJECT                                                                
043400                                                                          
043500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
043600 01  DLI-IO-WDD905.                                                       
043700*    03  -COPY WDD905                                                     
043800                                                                          
043900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
044000     SKIP3                                                                
044100 01  DLI-IO-AREA-ARTC01.                                                  
044200*    03  -COPY WDK601  -PRE  ERS-                                         
044300     EJECT                                                                
044400                                                                          
044500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD701'.                      
044600 01  DLI-IO-WDD701.                                                       
044700*    03  -COPY WDD701    -PRE WDD701-                                     
044800     EJECT                                                                
044900                                                                          
045000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD702'.                      
045100 01  DLI-IO-WDD702.                                                       
045200*    03  -COPY WDD702    -PRE WDD702-                                     
045300     EJECT                                                                
045400                                                                          
045500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4B1'.                      
045600 01  DLI-IO-WDQ4B1.                                                       
045700*    03  -COPY WDQ4B1                                                     
045800     EJECT                                                                
045900                                                                          
046000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
046100 01  DLI-IO-WDL601.                                                       
046200*    03  -COPY WDL601 -PRE WDL6-                                          
046300     EJECT                                                                
046400                                                                          
046500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
046600 01  DLI-IO-WDL611.                                                       
046700*    03  -COPY WDL611 -PRE WDL6-                                          
046800     EJECT                                                                
046900                                                                          
046910*****DB2 AREOR*******************                                         
046920 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
046940*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
046950     EJECT                                                                
046951                                                                          
046960 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
046980*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
046990     EJECT                                                                
047000                                                                          
047100 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
047300*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
047310     EJECT                                                                
047311                                                                          
047320     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
047330     EJECT                                                                
047340     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
047400     EJECT                                                                
047500     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
047600     EJECT                                                                
047700                                                                          
047800*******************************************                               
047900 LINKAGE SECTION.                                                         
048000                                                                          
048100*01  -COPY W0008      -PRE WDB6-                                          
048200     05  FILLER                  PIC X.                                   
048300     EJECT                                                                
048400*01  -COPY W0008      -PRE WDK6-                                          
048500     05  FILLER                  PIC X.                                   
048600     EJECT                                                                
048700*01  -COPY W0008      -PRE WDK7-                                          
048800     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
048900     EJECT                                                                
049000*01  -COPY W0008      -PRE ARTS-                                          
049100     05  FILLER                  PIC X.                                   
049200     EJECT                                                                
049300*01  -COPY W0008      -PRE WDL7-                                          
049400     05  FILLER                  PIC X.                                   
049500     EJECT                                                                
049600*01  -COPY W0008      -PRE WDD9-                                          
049700     05  FILLER                  PIC X.                                   
049800     EJECT                                                                
049900*01  -COPY W0008      -PRE ARTC-                                          
050000     05  FILLER                  PIC X.                                   
050100     EJECT                                                                
050200*01  -COPY W0008      -PRE WDD7-                                          
050300     05  FILLER                  PIC X.                                   
050400     EJECT                                                                
050500*01  -COPY W0008      -PRE WDQ4B-                                         
050600     05  FILLER                  PIC X.                                   
050700     EJECT                                                                
050800*01  -COPY W0008      -PRE WDK72-                                         
050900     05  FILLER                  PIC X.                                   
051000     EJECT                                                                
051100*01  -COPY W0008      -PRE WDK73-                                         
051200     05  FILLER                  PIC X.                                   
051300*01  -COPY W0008      -PRE WDL6-                                          
051400     05  FILLER                  PIC X.                                   
051500     EJECT                                                                
051510*01  -COPY W0008      -PRE UTIL-WDK6-                                     
051520     05  FILLER                  PIC X.                                   
051530     EJECT                                                                
051540*01  -COPY W0008      -PRE UTIL-WDK7-                                     
051550     05  FILLER                  PIC X.                                   
051560     EJECT                                                                
051600*******************************************                               
051700 PROCEDURE DIVISION  USING WDB6-PCB WDK6-PCB WDK7-PCB                     
051800                           ARTS-PCB WDL7-PCB WDD9-PCB ARTC-PCB            
051900                           WDD7-PCB WDQ4B-PCB WDK72-PCB WDK73-PCB         
052000                           WDL6-PCB UTIL-WDK6-PCB UTIL-WDK7-PCB.          
052100                                                                          
052200 MAIN SECTION.                                                            
052300     ENTRY 'DLITCBL' USING WDB6-PCB WDK6-PCB WDK7-PCB                     
052400                           ARTS-PCB WDL7-PCB WDD9-PCB ARTC-PCB            
052500                           WDD7-PCB WDQ4B-PCB WDK72-PCB WDK73-PCB         
052600                           WDL6-PCB UTIL-WDK6-PCB UTIL-WDK7-PCB.          
052700                                                                          
052800     PERFORM A-INIT                                                       
052900     PERFORM B-KOLLA-DATUM                                                
053000     PERFORM IMS-GN-WDK7                                                  
053100     PERFORM UNTIL SEGMENT-SLUT                                           
053200       EVALUATE WDK7-SEG-NAME-FB                                          
053300         WHEN 'WDK701  '                                                  
053400           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
053500                                            BYT03-IDARTNR                 
053600         WHEN 'WDK711  '                                                  
053700           IF SLAG-IDDC-REF = SPACES                                      
053800           AND (SLAG-IDDC(1:1) = '7' OR '4')                              
053900             CONTINUE                                                     
054000           ELSE                                                           
054100             PERFORM S10-ANTAL-TRANS-RET                                  
054200             PERFORM S32-CHECK-LAST-SALES-DEMAND                          
054300             PERFORM S33-CHECK-SUPPLIER                                   
054400             PERFORM S34-CHECK-LAST-BIN-DATE                              
054500             IF SLAG-KDREFSTA = 'P'                                       
054600             AND WS-BALANCE-SEND-DC > 0                                   
054700             AND DEMD-DATE-PASSED                                         
054800             AND BIN-DATE-PASSED                                          
054900               MOVE NEJ      TO TRANSFER-SW                               
055000                                  RETUR-SW                                
055100               MOVE SLAG-IDDC TO W-IDDC                                   
055200                                  WS-IDDC-SEND                            
055300               MOVE +1 TO WS-IDDC-IX                                      
055400               PERFORM UNTIL (WS-IDDC-IX > MAX-IDDC-IX)                   
055500                 IF SLAG-IDDC = TAB-IDDC(WS-IDDC-IX)                      
055600*******PASSIV     TRANSFER*************                                   
055700                   IF TAB-TID-TRS(WS-IDDC-IX) = ZERO                      
055800                     CONTINUE                                             
055900                   ELSE                                                   
056000                     IF TAB-TID-TRS(WS-IDDC-IX) = DAGENS-TIAAVVD-D        
056100                       PERFORM E-PASSIV-TRANSF                            
056200                       IF NO-TRANSFER                                     
056300*******PASSIV     RETUR****************                                   
056400                         PERFORM F-PASSIV-RETUR                           
056500                         IF NO-RETUR                                      
056600*******PASSIV     SKROT****************                                   
056700*******DET   SKALL SKAPAS MAX ANTAL SKROTNINGAR PER VECKA.                
056800                           IF TAB-ANTAL-SKROT-DC(WS-IDDC-IX) <            
056900                              TAB-KVSKROT-SPAS(WS-IDDC-IX)                
057000                                                                          
057100                             PERFORM G-PASSIV-SKROT                       
057200                           END-IF                                         
057300                         END-IF                                           
057400                       END-IF                                             
057500                     END-IF                                               
057600                   END-IF                                                 
057700*****            BARA FÖR ATT TRILLA UR SNURRAN NÄR MAN FÅTT TRÄFF        
057800*****              PÅ RÄTT DC                                             
057900                   ADD MAX-IDDC-IX TO WS-IDDC-IX                          
058000                 END-IF                                                   
058100                 ADD +1 TO WS-IDDC-IX                                     
058200               END-PERFORM                                                
058300             END-IF                                                       
058400           END-IF                                                         
058500           MOVE NEJ            TO WS-DEMD-DATE-PASSED-SW                  
058600                                  WS-EXTER-SUPPLIER                       
058700       END-EVALUATE                                                       
058800       PERFORM IMS-GN-WDK7                                                
058900     END-PERFORM                                                          
059000                                                                          
059100     PERFORM Z-FINIT                                                      
059200                                                                          
059300     MOVE ZERO TO RETURN-CODE                                             
059400     GOBACK                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 A-INIT SECTION.                                                          
059800                                                                          
059900     OPEN OUTPUT W27117                                                   
060000                 W2712D                                                   
060100                                                                          
060200     SKIP2                                                                
060300     ACCEPT DAGENS-DATUM    FROM DATE                                     
060400     PERFORM AA-LADDA-DC-TABELL                                           
060500                                                                          
060600     MOVE LOW-VALUE    TO W-WDQ4B1KY-MIN-X                                
060700     MOVE HIGH-VALUE   TO W-WDQ4B1KY-MAX-X                                
060800                                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 AA-LADDA-DC-TABELL SECTION.                                              
061200                                                                          
061300     INITIALIZE WS-IDDC-TABELL                                            
061400     MOVE +1 TO WS-IDDC-IX                                                
061500                MAX-IDDC-IX                                               
061600     PERFORM IMS-GN-WDB601                                                
061700     PERFORM UNTIL SEGMENT-SLUT                                           
061800*       IF DCS-KDDC = 'S '                                                
061900         MOVE DCS-IDDC     TO TAB-IDDC(WS-IDDC-IX)                        
062000         MOVE DCS-KDDC     TO TAB-KDDC(WS-IDDC-IX)                        
062100         MOVE DCS-TID-TRS   TO TAB-TID-TRS(WS-IDDC-IX)                    
062200         MOVE DCS-TIVV          TO TAB-TIVV(WS-IDDC-IX)                   
062300         MOVE DCS-FLTRANS-PAS    TO TAB-FLTRANS-PAS(WS-IDDC-IX)           
062400         MOVE DCS-IDDC-TPAS-1     TO TAB-IDDC-TPAS-1(WS-IDDC-IX)          
062500         MOVE DCS-IDDC-TPAS-2     TO TAB-IDDC-TPAS-2(WS-IDDC-IX)          
062600         MOVE DCS-IDDC-TPAS-3     TO TAB-IDDC-TPAS-3(WS-IDDC-IX)          
062700         MOVE DCS-SUARTMIN-TPAS  TO TAB-SUARTMIN-TPAS(WS-IDDC-IX)         
062800         MOVE DCS-KVPERIOD-TPAS TO  TAB-KVPERIOD-TPAS(WS-IDDC-IX)         
062900         MOVE DCS-KVVECKOR-TPAS TO  TAB-KVVECKOR-TPAS(WS-IDDC-IX)         
063000         MOVE DCS-SUVARLIM-TPAS TO  TAB-SUVARLIM-TPAS(WS-IDDC-IX)         
063100         MOVE DCS-KVPB-LIM      TO  TAB-KVPB-LIM(WS-IDDC-IX)              
063200         MOVE DCS-FLTRANS-ERS    TO TAB-FLTRANS-ERS(WS-IDDC-IX)           
063300         MOVE DCS-FLRETUR-PAS    TO TAB-FLRETUR-PAS(WS-IDDC-IX)           
063400         MOVE DCS-SUARTMIN-RPAS  TO TAB-SUARTMIN-RPAS(WS-IDDC-IX)         
063500         MOVE DCS-KVVECKOR-RPAS  TO TAB-KVVECKOR-RPAS(WS-IDDC-IX)         
063600         MOVE DCS-FLSKROT-PAS    TO TAB-FLSKROT-PAS(WS-IDDC-IX)           
063700         MOVE DCS-KVSKROT-SPAS   TO TAB-KVSKROT-SPAS(WS-IDDC-IX)          
063800         MOVE DCS-IDKUNDNR-TRETUR TO                                      
063900                                  TAB-IDKUNDNR-TRETUR(WS-IDDC-IX)         
064000         MOVE DCS-IDDISTR-SKROT  TO TAB-IDDISTR-SKROT(WS-IDDC-IX)         
064100         MOVE DCS-IDKUNDNR-SKROT TO TAB-IDKUNDNR-SKROT(WS-IDDC-IX)        
064200         MOVE DCS-IDDISTR-QSKROT  TO                                      
064300                                  TAB-IDDISTR-QSKROT(WS-IDDC-IX)          
064400         MOVE DCS-IDKUNDNR-QSKROT TO                                      
064500                                  TAB-IDKUNDNR-QSKROT(WS-IDDC-IX)         
064600         MOVE DCS-IDDISTR-RSKROT  TO                                      
064700                                  TAB-IDDISTR-RSKROT(WS-IDDC-IX)          
064800         MOVE DCS-IDKUNDNR-RSKROT TO                                      
064900                                  TAB-IDKUNDNR-RSKROT(WS-IDDC-IX)         
065000                                                                          
065100         MOVE ZERO               TO TAB-ANTAL-SKROT-DC(WS-IDDC-IX)        
065200         MOVE DCS-IDLANDX2        TO TAB-IDLANDX2(WS-IDDC-IX)             
065300         MOVE DCS-IDLEVNR-DC      TO TAB-IDLEVNR-DC(WS-IDDC-IX)           
065400         MOVE DCS-KVVECKOR-BIN    TO TAB-KVVECKOR-BIN(WS-IDDC-IX)         
065500         MOVE DCS-KVVECKOR-SPAS   TO TAB-KVVECKOR-SPAS(WS-IDDC-IX)        
065600         MOVE DCS-IDTECKEN-SPAS   TO TAB-IDTECKEN-SPAS(WS-IDDC-IX)        
065700         MOVE DCS-PRARTSTD-SPAS   TO TAB-PRARTSTD-SPAS(WS-IDDC-IX)        
065800         MOVE DCS-ADLAGOMR-SPAS   TO TAB-ADLAGOMR-SPAS(WS-IDDC-IX)        
065900         MOVE DCS-IDPERSON-SPAS   TO TAB-IDPERSON-SPAS(WS-IDDC-IX)        
066000         MOVE DCS-KDPRODSL-SPAS   TO TAB-KDPRODSL-SPAS(WS-IDDC-IX)        
066100                                                                          
066200         ADD +1 TO WS-IDDC-IX                                             
066300                   MAX-IDDC-IX                                            
066400         IF WS-IDDC-IX > 300                                              
066500            MOVE 'DC-TABELLEN FULL' TO FELTEXT                            
066600            CALL FELLOG                                                   
066700         END-IF                                                           
066800*       END-IF                                                            
066900        PERFORM IMS-GN-WDB601                                             
067000     END-PERFORM                                                          
067100     .                                                                    
067200     EJECT                                                                
067300 B-KOLLA-DATUM SECTION.                                                   
067400     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
067500     MOVE DAGENS-DATUM      TO DAT-I-TIDATUM                              
067600                                                                          
067700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
067800                     DAT-O-TIDATUM DAT-KDSVAR                             
067900                                                                          
068000     IF DAT-KDSVAR-OK                                                     
068100        MOVE DAT-TIAAVVD    TO WS-TIAAVVD                                 
068200                               DAGENS-TIAAVVD                             
068300**TEST                                                                    
068400     ELSE                                                                 
068500        DISPLAY 'FEL FRÅN DATKONV I B-KOLLA-DATUM'                        
068600        CALL FELLOG                                                       
068700     END-IF                                                               
068800                                                                          
068900     .                                                                    
069000     EJECT                                                                
069100 E-PASSIV-TRANSF SECTION.                                                 
069200                                                                          
069300     IF SLAG-KVSPARR-KVAL > 0                                             
069400     OR SLAG-KDLEVSP > 0                                                  
069500     OR SLAG-FLSKROT-BEORD = 'J'                                          
069600     OR SLAG-FLORDSP = 'J'                                                
069700     OR SLAG-FLSPBULK = 'J'                                               
069800     OR BYT03-OBJEKT                                                      
069900     OR TAB-FLTRANS-PAS(WS-IDDC-IX) = 'N'                                 
070000       CONTINUE                                                           
070100     ELSE                                                                 
070200       PERFORM IMS-GU-WDK601                                              
070300       IF SEGMENT-FINNS                                                   
070400         MOVE ART-KDPRODSL TO TEST-KDPRODSL                               
070500         PERFORM IMS-GNP-WDK611                                           
070600         IF CLAG-REDIRLEV > 0                                             
070700         OR KDPRODSL-VOLVO-EMB                                            
070800         OR KDPRODSL-LOCAL-EMB                                            
070900           CONTINUE                                                       
071000         ELSE                                                             
071100           MOVE NEJ   TO TRAFF-SW                                         
071200           IF SLAG-IDDC (1:1) = '7' OR '4' OR '5'                         
071300             IF SLAG-IDDC (1:1) = '7'                                     
071400               MOVE 'CN'    TO W-IDLANDX2                                 
071500             END-IF                                                       
071600             IF SLAG-IDDC (1:1) = '4'                                     
071700               MOVE 'US'    TO W-IDLANDX2                                 
071800             END-IF                                                       
071900             IF SLAG-IDDC (1:1) = '5'                                     
072000               MOVE 'CA'    TO W-IDLANDX2                                 
072100             END-IF                                                       
072200             PERFORM IMS-GU-WDK712                                        
072300             COMPUTE WS-VALUE-KVLS ROUNDED =                              
072400                     LART-PRMATRL  * WS-BALANCE-SEND-DC                   
072500           ELSE                                                           
072600             COMPUTE WS-VALUE-KVLS ROUNDED =                              
072700                     CLAG-PRARTSTD * WS-BALANCE-SEND-DC                   
072800           END-IF                                                         
072900           IF WS-VALUE-KVLS > TAB-SUARTMIN-TPAS(WS-IDDC-IX)               
073000             IF TAB-FLTRANS-ERS(WS-IDDC-IX) = JA OR YES                   
073100               IF CLAG-KDERS = 00 OR 01 OR 04 OR 07                       
073200                 PERFORM EA-KOLLA-MOTTAG-DC                               
073300                 IF TRAFF-OK                                              
073400                   PERFORM S-SKRIV-TRANSF                                 
073500                 END-IF                                                   
073600               END-IF                                                     
073700             ELSE                                                         
073800               IF CLAG-KDERS = 0                                          
073900                 PERFORM EA-KOLLA-MOTTAG-DC                               
074000                 IF TRAFF-OK                                              
074100                   PERFORM S-SKRIV-TRANSF                                 
074200                 END-IF                                                   
074300               END-IF                                                     
074400             END-IF                                                       
074500           END-IF                                                         
074600         END-IF                                                           
074700       ELSE                                                               
074800         MOVE 'ART SAKNAS PÅ K6' TO FELTEXT                               
074900         CALL FELLOG                                                      
075000       END-IF                                                             
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500 EA-KOLLA-MOTTAG-DC SECTION.                                              
075600**********************************************************                
075700     MOVE    +1   TO IX-REC                                               
075800     PERFORM UNTIL IX-REC > 3 OR TRAFF-OK                                 
075900       IF IX-REC = 1                                                      
076000         MOVE TAB-IDDC-TPAS-1(WS-IDDC-IX) TO WS-IDDC-REC                  
076100                                             W-IDDC-TRANSF                
076200       END-IF                                                             
076300       IF IX-REC = 2                                                      
076400         MOVE TAB-IDDC-TPAS-2(WS-IDDC-IX) TO WS-IDDC-REC                  
076500                                             W-IDDC-TRANSF                
076600       END-IF                                                             
076700       IF IX-REC = 3                                                      
076800         MOVE TAB-IDDC-TPAS-3(WS-IDDC-IX) TO WS-IDDC-REC                  
076900                                             W-IDDC-TRANSF                
077000       END-IF                                                             
077100****KOLLAR   OM GODKÄND TRANSFER VÄG SE BILD 2349                         
077200       IF IX-REC = 1 OR 2 OR 3                                            
077300         PERFORM DB2-SELECT-TP4TRAN                                       
077400         IF LINES-FOUND                                                   
077500****HÄMTAR     ARTIKELINFO PÅ MOTTAGANDE DC                               
077600           PERFORM IMS-GU-ARTS11-TRANSF                                   
077700           IF SEGMENT-FINNS                                               
077800             IF SLAG-IDDC-REF = SPACE                                     
077900               MOVE 'N'            TO WS-FLORDSP                          
078000               MOVE 'N'            TO WS-FLSPBULK                         
078100             ELSE                                                         
078200               MOVE TRANSF-SLAG-FLORDSP    TO WS-FLORDSP                  
078300               MOVE TRANSF-SLAG-FLSPBULK   TO WS-FLSPBULK                 
078400             END-IF                                                       
078500             IF TRANSF-SLAG-KVSPARR-KVAL > 0                              
078600             OR TRANSF-SLAG-KDLEVSP > 0                                   
078700             OR TRANSF-SLAG-FLSKROT-BEORD = 'J'                           
078800             OR WS-FLORDSP = 'J'                                          
078900             OR WS-FLSPBULK = 'J'                                         
079000             OR TRANSF-SLAG-ADLAGOMR = 98                                 
079100****FÖR ATT STOPPA TRANSFER TILL LOKALT ANSKAFFAD ARTIKEL TILLSV.         
079200             OR TRANSF-SLAG-IDDC-REF = SPACE                              
079300               CONTINUE                                                   
079400             ELSE                                                         
079500               COMPUTE W-TILLG-SDC = TRANSF-SLAG-KVLS                     
079600                                   + TRANSF-SLAG-KVBEART                  
079700                                   + TRANSF-SLAG-KVAKS-SDC                
079800                                   + TRANSF-SLAG-KVAKS-PAV                
079900                                   - TRANSF-SLAG-KVOKS-BULK               
080000                                   - TRANSF-SLAG-KVOKS-DAG                
080100               COMPUTE W-BEHOV-SDC = (TRANSF-SLAG-KVPB-REF                
080200                       * TAB-KVPERIOD-TPAS(WS-IDDC-IX)) +                 
080300                                     (TRANSF-SLAG-KVPBREOI                
080400                       * TAB-KVPERIOD-TPAS(WS-IDDC-IX))                   
080500               COMPUTE TRANS-KVPB-TOT = TRANSF-SLAG-KVPB-REF +            
080600                                        TRANSF-SLAG-KVPBREOI              
080700               IF (W-TILLG-SDC < W-BEHOV-SDC) OR                          
080800                ((TRANS-KVPB-TOT < TAB-KVPB-LIM(WS-IDDC-IX))              
080900                 AND (WS-BALANCE-SEND-DC <                                
081000                      TAB-SUVARLIM-TPAS(WS-IDDC-IX)))                     
081100                   IF (SLAG-FLREFILL = 'J' AND                            
081200                       SLAG-IDDC-REF NOT = SPACE)                         
081300                   OR SLAG-IDDC-REF = SPACE                               
081400                     PERFORM EAA-KOLLA-OI                                 
081500**** JÄMFÖR WS-DAGENS-AAAAVV-MINUS-SALES MED                              
081600**** LATEST DEMAND(2341-BILD) OM LATEST DEMAND(2341-BILD)                 
081700**** ÄR STÖRRE ÄN WS-DAGENS-AAAAVV-MINUS-SALES SÅ SÄTT                    
081800**** JA TILL TRAFF-SW                                                     
081900                     IF WS-TIAAAAVV-SALES >                               
082000                        WS-DAGENS-AAAAVV-MINUS-SALES                      
082100                       MOVE JA TO TRAFF-SW                                
082200************************     TRANSFER SKALL SKAPAS ***************        
082300                     END-IF                                               
082400                   END-IF                                                 
082500               END-IF                                                     
082600             END-IF                                                       
082700           END-IF                                                         
082800         END-IF                                                           
082900       END-IF                                                             
083000       ADD +1       TO IX-REC                                             
083100     END-PERFORM                                                          
083200     .                                                                    
083300     EJECT                                                                
083400 EAA-KOLLA-OI SECTION.                                                    
083500                                                                          
083600***FLYTTA TILLBAKA DAGENS-DATUM MED ANTAL VECKOR SALES(WEEKS) FRÅN        
083700***BILD 4407 RESULTATE BLIR I AAAAVV                                      
083800     COMPUTE WS-KVVECKOR-TPAS =                                           
083900             (TAB-KVVECKOR-TPAS(WS-IDDC-IX) * -1)                         
084000                                                                          
084100     MOVE DAGENS-TIAAVVD (1:4) TO VADD-DATUM-AAVV                         
084200     MOVE WS-KVVECKOR-TPAS     TO VADD-ANTAL                              
084300     CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                       
084400                                                                          
084500     MOVE VADD-DATUM-AAVV      TO WS-DAGENS-AAVV-MINUS-SALES              
084600     IF DAGENS-TIAAVVD-AA > 50                                            
084700       COMPUTE WS-DAGENS-AAAAVV-MINUS-SALES =                             
084800               WS-DAGENS-AAVV-MINUS-SALES + 190000                        
084900     ELSE                                                                 
085000       COMPUTE WS-DAGENS-AAAAVV-MINUS-SALES =                             
085100               WS-DAGENS-AAVV-MINUS-SALES + 200000                        
085200     END-IF                                                               
085300                                                                          
085400**** OMVANDLA LATEST DEMAND(2341-BILD) TILL AAVVD                         
085500     IF DC-TIREFEFT = 0                                                   
085600       MOVE 0  TO WS-TIAAAAVV-SALES                                       
085700     ELSE                                                                 
085800       MOVE 'AAMMDD'        TO DAT-KDDATFORM                              
085900       MOVE DC-TIREFEFT     TO DAT-I-TIDATUM                              
086000                                                                          
086100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
086200                       DAT-O-TIDATUM DAT-KDSVAR                           
086300                                                                          
086400       IF DAT-KDSVAR-OK                                                   
086500          MOVE DAT-TIAAVVD(1:4) TO WS-TIAAVV-SALES                        
086600       ELSE                                                               
086700          DISPLAY 'FEL FRÅN DATKONV I B-KOLLA-DATUM'                      
086800          CALL FELLOG                                                     
086900       END-IF                                                             
087000****   OMVANDLA LATEST DEMAND(2341-BILD) TILL AAAAVV                      
087100       IF WS-TIAAVV-SALES > 5000                                          
087200         COMPUTE WS-TIAAAAVV-SALES =                                      
087300                 WS-TIAAVV-SALES + 190000                                 
087400       ELSE                                                               
087500         COMPUTE WS-TIAAAAVV-SALES =                                      
087600                 WS-TIAAVV-SALES + 200000                                 
087700       END-IF                                                             
087800     END-IF                                                               
087900*******************************************************                   
088000     .                                                                    
088100     EJECT                                                                
088200                                                                          
088300 F-PASSIV-RETUR  SECTION.                                                 
088400                                                                          
088500     MOVE JA              TO ERS-PUBVECKA-SW                              
088600     IF SLAG-KVSPARR-KVAL > 0                                             
088700     OR SLAG-KDLEVSP > 0                                                  
088800     OR SLAG-FLSKROT-BEORD = 'J'                                          
088900     OR SLAG-FLORDSP = 'J'                                                
089000     OR SLAG-FLSPBULK = 'J'                                               
089100     OR BYT03-OBJEKT                                                      
089200     OR SLAG-ADLAGOMR = 88                                                
089300     OR SLAG-ADLAGOMR = 98                                                
089400     OR TAB-FLRETUR-PAS(WS-IDDC-IX) = 'N'                                 
089500     OR (SLAG-IDDC(1:1) = '4' AND SLAG-IDDC-REF(1:1) = '7')               
089600     OR (SLAG-IDDC(1:1) = '7' AND SLAG-IDDC-REF(1:1) = '4')               
089700       CONTINUE                                                           
089800     ELSE                                                                 
089900       PERFORM IMS-GU-WDK601                                              
090000       IF SEGMENT-FINNS                                                   
090100         PERFORM IMS-GNP-WDK611                                           
090200         IF SLAG-IDDC-REF(1:1) = '7' OR  '4' OR '5' OR '6'                
090300           MOVE SLAG-IDDC-REF TO W-IDDC-REF                               
090400           PERFORM IMS-GU-WDK711-MOTTAG                                   
090500*          IF SEGMENT-FINNS AND MOTTAG-SLAG-IDDC-REF = SPACE              
090600           IF SEGMENT-FINNS                                               
090700             MOVE MOTTAG-SLAG-FLSKROT-BEORD TO WS-FLSKROT-BEORD           
090800             MOVE MOTTAG-SLAG-KVSPARR-KVAL  TO WS-KVSPARR-KVAL            
090900             MOVE MOTTAG-SLAG-KDLEVSP       TO WS-KDLEVSP                 
091000           ELSE                                                           
091100****BARA FÖR ATT STOPPA RETUR OM SEGMENT INTE FINNS                       
091200             MOVE 'J'                TO WS-FLSKROT-BEORD                  
091300             MOVE +1                 TO WS-KVSPARR-KVAL                   
091400             MOVE +1                 TO WS-KDLEVSP                        
091500             MOVE ZERO               TO MOTTAG-SLAG-ADLAGOMR              
091600                                        MOTTAG-SLAG-ADGANG                
091700                                        MOTTAG-SLAG-ADPLATS               
091800           END-IF                                                         
091900         ELSE                                                             
092000           MOVE CLAG-FLSKROT-BEORD TO WS-FLSKROT-BEORD                    
092100           MOVE CLAG-KVSPARR-KVAL    TO WS-KVSPARR-KVAL                   
092200           MOVE CLAG-KDLEVSP         TO WS-KDLEVSP                        
092300         END-IF                                                           
092400         MOVE ART-KDPRODSL TO TEST-KDPRODSL                               
092500         IF CLAG-REDIRLEV = 1.00                                          
092600         OR KDPRODSL-VOLVO-EMB                                            
092700         OR KDPRODSL-LOCAL                                                
092800         OR ART-IDFKNGRP = 8616                                           
092900         OR WS-FLSKROT-BEORD = 'J'                                        
093000         OR WS-KDLEVSP > 0                                                
093100         OR WS-KVSPARR-KVAL > 0                                           
093200*------------------------------------                                     
093300* EXCLUDE FROM DESTOCKING - RETURNS                                       
093400*   - PARTS UNDER PRODUCT GROUP 18                                        
093500*   - PARTS WITH EXTERNAL SUPPLIER                                        
093600*------------------------------------                                     
093700         OR KDPRODSL-TOOLS                                                
093800         OR WS-EXTER-SUPPLIER = JA                                        
093900           CONTINUE                                                       
094000         ELSE                                                             
094100           IF SLAG-IDDC (1:1) = '7' OR '4' OR '5'                         
094200             IF SLAG-IDDC (1:1) = '7'                                     
094300               MOVE 'CN'    TO W-IDLANDX2                                 
094400             END-IF                                                       
094500             IF SLAG-IDDC (1:1) = '4'                                     
094600               MOVE 'US'    TO W-IDLANDX2                                 
094700             END-IF                                                       
094800             IF SLAG-IDDC (1:1) = '5'                                     
094900               MOVE 'CA'    TO W-IDLANDX2                                 
095000             END-IF                                                       
095100             PERFORM IMS-GU-WDK712                                        
095200             COMPUTE WS-VALUE-KVLS ROUNDED =                              
095300                     LART-PRMATRL  * WS-BALANCE-SEND-DC                   
095400           ELSE                                                           
095500             COMPUTE WS-VALUE-KVLS ROUNDED =                              
095600                     CLAG-PRARTSTD * WS-BALANCE-SEND-DC                   
095700           END-IF                                                         
095800           IF CLAG-KDERS > 0                                              
095900             IF CLAG-KDERS = 01 OR 04 OR 07 OR 11 OR 14 OR 17 OR          
096000                             18 OR 21 OR 24 OR 27                         
096100               PERFORM FB-KOLLA-TILLKOMMANDE                              
096200             ELSE                                                         
096300               MOVE NEJ   TO ERS-PUBVECKA-SW                              
096400             END-IF                                                       
096500           END-IF                                                         
096600           IF WS-VALUE-KVLS > TAB-SUARTMIN-RPAS(WS-IDDC-IX)               
096700             IF CLAG-KDERS = 0 OR ERS-PUBVECKA-OK                         
096800               PERFORM S22-BERAKNA-6MANADER-SEDAN                         
096900               IF WS-DAGENS-AAAAVV-MINUS-26V >                            
097000                  WS-TIFINLV-AAAAVV                                       
097100                 PERFORM FA-KOLLA-AVROP-FT                                
097200                 IF AVROP                                                 
097300                   PERFORM S-SKRIV-RETUR                                  
097400                 END-IF                                                   
097500               END-IF                                                     
097600             END-IF                                                       
097700           END-IF                                                         
097800         END-IF                                                           
097900       ELSE                                                               
098000         MOVE 'ART SAKNAS PÅ K6' TO FELTEXT                               
098100         CALL FELLOG                                                      
098200       END-IF                                                             
098300     END-IF                                                               
098400     .                                                                    
098500     EJECT                                                                
098600                                                                          
098700 FA-KOLLA-AVROP-FT SECTION.                                               
098800                                                                          
098900*****KOLLAR OM AVROP PÅ HUVUDLEV FINNS EFTER FRYSTID + 2 VECKOR           
099000     MOVE ART-IDLEVNR    TO W-IDLEVNR                                     
099100     IF SLAG-IDDC-REF = '11'                                              
099101       MOVE ZERO TO WS-KVPB-TOT-30V-SEND-DC                               
099110       IF CLAG-IDDC-REF = SPACE                                           
099200         COMPUTE WS-FRYSTID = CLAG-KVVECKOR-FT + 2                        
099210       ELSE                                                               
099211         PERFORM S21-BERAKNA-30V-LAGERSALDO-CDC                           
099212         IF CLAG-KVLS < WS-KVPB-TOT-30V-SEND-DC                           
099213***USE THE SAME FLAG AS FOR CHECKING AVROP TO APPROVE FOR RETURN          
099214           MOVE JA TO AVROP-SW                                            
099220         END-IF                                                           
099230       END-IF                                                             
099300     ELSE                                                                 
099400       IF SLAG-IDDC-REF(1:1)= '7' OR '4' OR '5' OR '6'                    
099500         MOVE SLAG-IDDC-REF TO W-IDDC-REF                                 
099600         PERFORM IMS-GU-WDK722                                            
099700         IF SEGMENT-FINNS                                                 
099800           COMPUTE WS-FRYSTID = XLAG-KVVECKOR-FT + 2                      
099900         ELSE                                                             
100000           MOVE ZERO TO WS-FRYSTID                                        
100100         END-IF                                                           
100200       END-IF                                                             
100300     END-IF                                                               
100310     IF AVROP                                                             
100320       CONTINUE                                                           
100330     ELSE                                                                 
100400       MOVE DAGENS-TIAAVVD (1:4) TO VADD-DATUM-AAVV                       
100500       MOVE WS-FRYSTID         TO VADD-ANTAL                              
100600       CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                     
100700                                                                          
100800       MOVE VADD-DATUM-AAVV    TO WS-DAGENS-AAAAVV-FT                     
100900       IF DAGENS-TIAAVVD-AA > 50                                          
101000         COMPUTE WS-DAGENS-AAAAVV-FT =                                    
101100                 WS-DAGENS-AAAAVV-FT + 190000                             
101200       ELSE                                                               
101300         COMPUTE WS-DAGENS-AAAAVV-FT =                                    
101400                 WS-DAGENS-AAAAVV-FT + 200000                             
101500       END-IF                                                             
101600       MOVE W-IDARTNR TO W-IDARTNR-D9                                     
101700       MOVE WC-CDC-SE TO W-IDDC-D9                                        
101800       PERFORM IMS-GU-WDD905-LAST                                         
101900       IF SEGMENT-FINNS                                                   
102000         IF DAAVROP-AVS > WS-DAGENS-AAAAVV-FT                             
102100           MOVE JA    TO AVROP-SW                                         
102200         ELSE                                                             
102300           MOVE NEJ   TO AVROP-SW                                         
102400         END-IF                                                           
102500       ELSE                                                               
102600         MOVE NEJ     TO AVROP-SW                                         
102700       END-IF                                                             
102710     END-IF                                                               
102800     .                                                                    
102900     EJECT                                                                
103000                                                                          
103100 FB-KOLLA-TILLKOMMANDE SECTION.                                           
103200                                                                          
103300     PERFORM IMS-GU-WDD701                                                
103400     IF SEGMENT-FINNS                                                     
103500        PERFORM IMS-GNP-WDD702                                            
103600        PERFORM UNTIL SEGMENT-SAKNAS                                      
103700          IF WDD702-FLTEXT = 'N'                                          
103800           MOVE WDD702-IDARTNR-TILLK TO W-IDARTNR-ERS                     
103900           IF SLAG-IDDC (1:1) = '7' OR '4' OR '5'                         
104000             IF SLAG-IDDC (1:1) = '7'                                     
104100               MOVE 'CN'    TO W-IDLANDX2                                 
104200             END-IF                                                       
104300             IF SLAG-IDDC (1:1) = '4'                                     
104400               MOVE 'US'    TO W-IDLANDX2                                 
104500             END-IF                                                       
104600             IF SLAG-IDDC (1:1) = '5'                                     
104700               MOVE 'CA'    TO W-IDLANDX2                                 
104800             END-IF                                                       
104900             PERFORM IMS-GU-WDK712-ERS                                    
105000             IF SEGMENT-FINNS                                             
105100               MOVE 'AAMMDD' TO DAT-KDDATFORM                             
105200               MOVE ERS-LART-DAPUBL (3:6) TO DAT-I-TIDATUM                
105300                                                                          
105400               CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM            
105500                               DAT-O-TIDATUM DAT-KDSVAR                   
105600                                                                          
105700               IF DAT-KDSVAR-OK                                           
105800                 MOVE DAT-TIAAVVD TO WS-TIFINLV-AAVVD                     
105900                 MOVE DAGENS-TIAAVVD (1:4) TO VADD-DATUM-AAVV             
106000                 MOVE TAB-KVVECKOR-RPAS(WS-IDDC-IX) TO VADD-ANTAL         
106100                 CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL           
106200                                                                          
106300                 MOVE VADD-DATUM-AAVV TO WS-DAGENS-AAVV-ERS               
106400                 IF WS-TIFINLV-AAVVD(1:4) < WS-DAGENS-AAVV-ERS            
106500                   MOVE NEJ TO ERS-PUBVECKA-SW                            
106600                 END-IF                                                   
106700               ELSE                                                       
106800                 MOVE NEJ TO ERS-PUBVECKA-SW                              
106900               END-IF                                                     
107000             END-IF                                                       
107100           ELSE                                                           
107200             PERFORM IMS-GU-ARTC01-ERS                                    
107300             IF SEGMENT-FINNS                                             
107400               MOVE ERS-ART-TIFINLV TO WS-TIFINLV-AAVVD                   
107500               MOVE DAGENS-TIAAVVD (1:4) TO VADD-DATUM-AAVV               
107600               MOVE TAB-KVVECKOR-RPAS(WS-IDDC-IX) TO VADD-ANTAL           
107700               CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL             
107800                                                                          
107900               MOVE VADD-DATUM-AAVV TO WS-DAGENS-AAVV-ERS                 
108000               IF WS-TIFINLV-AAVVD(1:4) < WS-DAGENS-AAVV-ERS              
108100                 MOVE NEJ TO ERS-PUBVECKA-SW                              
108200               END-IF                                                     
108300             END-IF                                                       
108400            END-IF                                                        
108500           END-IF                                                         
108600          PERFORM IMS-GNP-WDD702                                          
108700        END-PERFORM                                                       
108800     END-IF                                                               
108900     .                                                                    
109000     EJECT                                                                
109100 S-SKRIV-TRANSF SECTION.                                                  
109200                                                                          
109300     MOVE W-IDARTNR                       TO UT-IDARTNR                   
109400     MOVE WS-IDDC-REC                     TO UT-IDDC                      
109500     MOVE WS-BALANCE-SEND-DC              TO UT-KVBEART                   
109600     MOVE CLAG-ADLAGOMR                   TO UT-ADLAGOMR-CDC              
109700     MOVE CLAG-ADGANG                     TO UT-ADGANG-CDC                
109800     MOVE CLAG-ADPLATS                    TO UT-ADPLATS-CDC               
109900     MOVE TRANSF-SLAG-IDLEVNR             TO UT-IDLEVNR                   
110000     MOVE TRANSF-SLAG-IDPERSON-BUY        TO UT-IDPERSON-BUY              
110100     MOVE 'T'                             TO UT-KDREFTYP                  
110200     MOVE 'O'                             TO UT-KDREFORS                  
110300     MOVE TRANSF-SLAG-ADLAGOMR            TO UT-ADLAGOMR-SDC              
110400     MOVE TRANSF-SLAG-ADGANG              TO UT-ADGANG-SDC                
110500     MOVE TRANSF-SLAG-ADPLATS             TO UT-ADPLATS-SDC               
110600     MOVE TP4TRAN-IDDISTR                 TO UT-IDDISTR                   
110700     MOVE TP4TRAN-IDKUNDNR                TO UT-IDKUNDNR                  
110800     MOVE ZERO                            TO UT-ADLAGOMR-CD               
110900                                             UT-ADGANG-CD                 
111000                                             UT-ADPLATS-CD                
111100                                             UT-KVBEART-CD                
111200                                             UT-KDREFTXT                  
111300                                             UT-KDFRAKT                   
111400     MOVE TP4TRAN-IDDC-SEND               TO UT-IDDC-REF                  
111500                                                                          
111600     PERFORM S11-SKRIV-W27117                                             
111700     MOVE JA                              TO TRANSFER-SW                  
111800     .                                                                    
111900     EJECT                                                                
112000 S-SKRIV-RETUR SECTION.                                                   
112100                                                                          
112200       MOVE W-IDARTNR                     TO UT-IDARTNR                   
112300       MOVE WS-IDDC-SEND                  TO UT-IDDC                      
112400       MOVE WS-BALANCE-SEND-DC            TO UT-KVBEART                   
112500       IF SLAG-IDDC-REF(1:1) = '7' OR '4' OR '5' OR '6'                   
112600         MOVE MOTTAG-SLAG-ADLAGOMR        TO UT-ADLAGOMR-CDC              
112700         MOVE MOTTAG-SLAG-ADGANG          TO UT-ADGANG-CDC                
112800         MOVE MOTTAG-SLAG-ADPLATS         TO UT-ADPLATS-CDC               
112900       ELSE                                                               
113000         MOVE CLAG-ADLAGOMR               TO UT-ADLAGOMR-CDC              
113100         MOVE CLAG-ADGANG                 TO UT-ADGANG-CDC                
113200         MOVE CLAG-ADPLATS                TO UT-ADPLATS-CDC               
113300       END-IF                                                             
113400       MOVE SLAG-IDLEVNR                  TO UT-IDLEVNR                   
113500       MOVE SLAG-IDPERSON-BUY             TO UT-IDPERSON-BUY              
113600       MOVE 'R'                           TO UT-KDREFTYP                  
113700       MOVE 'U'                           TO UT-KDREFORS                  
113800       MOVE SLAG-ADLAGOMR                 TO UT-ADLAGOMR-SDC              
113900       MOVE SLAG-ADGANG                   TO UT-ADGANG-SDC                
114000       MOVE SLAG-ADPLATS                  TO UT-ADPLATS-SDC               
114100       MOVE ZERO                          TO UT-IDDISTR                   
114200       MOVE ZERO                          TO UT-ADLAGOMR-CD               
114300                                             UT-ADGANG-CD                 
114400                                             UT-ADPLATS-CD                
114500                                             UT-KVBEART-CD                
114600                                             UT-KDREFTXT                  
114700                                             UT-KDFRAKT                   
114800       MOVE WS-IDDC-SEND                  TO UT-IDDC-REF                  
114900*      IF TAB-KDDC(WS-IDDC-IX) = 'NA' OR 'NP'                             
115000       IF TAB-KDDC(WS-IDDC-IX) = 'NP'                                     
115100         MOVE TAB-IDKUNDNR-TRETUR(WS-IDDC-IX) TO UT-IDKUNDNR              
115200       ELSE                                                               
115300         MOVE ZERO                            TO UT-IDKUNDNR              
115400       END-IF                                                             
115500       IF UT-KVBEART > 0                                                  
115600         PERFORM S11-SKRIV-W27117                                         
115700         MOVE JA                          TO RETUR-SW                     
115800       END-IF                                                             
115900     .                                                                    
116000     EJECT                                                                
116100 G-PASSIV-SKROT  SECTION.                                                 
116200     MOVE 'G-PASSIV-SKROT '   TO CURRENT-SECTION                          
116300                                                                          
116400     MOVE JA     TO SKRIV-SKROT-SW                                        
116500                                                                          
116600     MOVE ZERO  TO WS-TILLG-SKROT                                         
116700     COMPUTE WS-TILLG-SKROT = SLAG-KVBEART                                
116800                            + SLAG-KVAKS-SDC                              
116900                            + SLAG-KVAKS-PAV                              
117000     END-COMPUTE                                                          
117100                                                                          
117200     IF WS-TILLG-SKROT > ZERO                                             
117300       MOVE NEJ    TO SKRIV-SKROT-SW                                      
117400     END-IF                                                               
117500                                                                          
117600*-------                                                                  
117700                                                                          
117800     PERFORM GA-SKAPA-KVSKROT                                             
117900                                                                          
118000     MOVE ZERO     TO WS-VALUE-KVSKROT                                    
118100     PERFORM IMS-GU-WDK601                                                
118200     IF SEGMENT-FINNS                                                     
118300       IF SLAG-IDDC = '21'                                                
118400         CONTINUE                                                         
118500       ELSE                                                               
118600         IF ART-IDFKNGRP = 8616                                           
118700           MOVE NEJ  TO SKRIV-SKROT-SW                                    
118800         END-IF                                                           
118900       END-IF                                                             
119000                                                                          
119100       MOVE ART-KDPRODSL TO TEST-KDPRODSL                                 
119200       IF KDPRODSL-VOLVO-EMB OR KDPRODSL-LOCAL-EMB                        
119300         MOVE NEJ    TO SKRIV-SKROT-SW                                    
119400       END-IF                                                             
119500                                                                          
119600       IF SLAG-IDDC (1:1) = '7' OR '4' OR '5'                             
119700         IF SLAG-IDDC (1:1) = '7'                                         
119800           MOVE 'CN'        TO W-IDLANDX2                                 
119900         END-IF                                                           
120000         IF SLAG-IDDC (1:1) = '4'                                         
120100           MOVE 'US'        TO W-IDLANDX2                                 
120200         END-IF                                                           
120300         IF SLAG-IDDC (1:1) = '5'                                         
120400           MOVE 'CA'        TO W-IDLANDX2                                 
120500         END-IF                                                           
120600         PERFORM IMS-GU-WDK712                                            
120700         IF SEGMENT-FINNS                                                 
120800           MOVE LART-PRMATRL  TO WS-PRART-SPAS                            
120900           COMPUTE WS-VALUE-KVSKROT ROUNDED =                             
121000                   LART-PRMATRL * WS-KVSKROT                              
121100           END-COMPUTE                                                    
121200         ELSE                                                             
121300           MOVE NEJ  TO SKRIV-SKROT-SW                                    
121400         END-IF                                                           
121500       ELSE                                                               
121600           PERFORM IMS-GNP-WDK611                                         
121700           IF SEGMENT-FINNS                                               
121800             MOVE CLAG-PRARTSTD  TO WS-PRART-SPAS                         
121900             COMPUTE WS-VALUE-KVSKROT ROUNDED =                           
122000                     CLAG-PRARTSTD * WS-KVSKROT                           
122100             END-COMPUTE                                                  
122200                                                                          
122300****     OBS! SJÄLVKOST MÅSTE FINNAS, ANNARS ABEND I W335PRIS.            
122400             IF CLAG-PRARTSJK = 0                                         
122500               MOVE NEJ TO SKRIV-SKROT-SW                                 
122600             END-IF                                                       
122700           END-IF                                                         
122800       END-IF                                                             
122900     ELSE                                                                 
123000        MOVE 'ART SAKNAS PÅ K601' TO FELTEXT-STR                          
123100        PERFORM S99-ABEND                                                 
123200     END-IF                                                               
123300                                                                          
123400*-------                                                                  
123500                                                                          
123600     IF TAB-FLSKROT-PAS(WS-IDDC-IX) = 'N'                                 
123700       MOVE NEJ    TO SKRIV-SKROT-SW                                      
123800     END-IF                                                               
123900                                                                          
124000*-------                                                                  
124100                                                                          
124200     IF SLAG-TISKROT-AUTO > ZERO                                          
124300       MOVE SLAG-TISKROT-AUTO  TO SPAR-TISKROT-AUTO                       
124400                                                                          
124500       IF SPAR-TISKROT-AUTO >= DAGENS-DATUM                               
124600         MOVE NEJ    TO SKRIV-SKROT-SW                                    
124700       END-IF                                                             
124800     END-IF                                                               
124900                                                                          
125000*-------                                                                  
125100                                                                          
125200     IF SLAG-FLSKROT-BEORD = 'J' OR 'Y'                                   
125300       MOVE NEJ    TO SKRIV-SKROT-SW                                      
125400     END-IF                                                               
125500                                                                          
125600*-------                                                                  
125700                                                                          
125800     IF SLAG-ADLAGOMR = 88 OR 98                                          
125900       MOVE NEJ    TO SKRIV-SKROT-SW                                      
126000     END-IF                                                               
126100                                                                          
126200*-------                                                                  
126300                                                                          
126400     PERFORM IMS-GU-WDK601                                                
126500     IF SEGMENT-FINNS                                                     
126600        PERFORM IMS-GNP-WDK611                                            
126700        IF SEGMENT-FINNS                                                  
126800           IF SLAG-IDDC = '21'                                            
126900             IF CLAG-IDPROJ = 'OBJ'                                       
127000               MOVE NEJ  TO SKRIV-SKROT-SW                                
127100             END-IF                                                       
127200           ELSE                                                           
127300             IF CLAG-IDPROJ = 'OBJ'                                       
127400             OR (CLAG-KDERS > 0 AND CLAG-KDERS < 20)                      
127500               MOVE NEJ  TO SKRIV-SKROT-SW                                
127600             END-IF                                                       
127700           END-IF                                                         
127800        END-IF                                                            
127900     END-IF                                                               
128000                                                                          
128100*-------                                                                  
128200     IF TAB-KVVECKOR-SPAS(WS-IDDC-IX) > ZERO                              
128300        PERFORM S35-GET-PASSIVE-DATE                                      
128400        IF WS-PASSIVE-WEEKS NOT > TAB-KVVECKOR-SPAS(WS-IDDC-IX)           
128500           MOVE NEJ TO SKRIV-SKROT-SW                                     
128600        END-IF                                                            
128700     END-IF                                                               
128800                                                                          
128900*-------                                                                  
129000     IF SKRIV-SKROT-SW = JA                                               
129100       IF TAB-PRARTSTD-SPAS(WS-IDDC-IX) > ZERO                            
129200         IF TAB-IDTECKEN-SPAS(WS-IDDC-IX) = '<'                           
129300           IF WS-PRART-SPAS NOT < TAB-PRARTSTD-SPAS(WS-IDDC-IX)           
129400              MOVE NEJ TO SKRIV-SKROT-SW                                  
129500           END-IF                                                         
129600         ELSE                                                             
129700           IF TAB-IDTECKEN-SPAS(WS-IDDC-IX) = '>'                         
129800             IF WS-PRART-SPAS NOT > TAB-PRARTSTD-SPAS(WS-IDDC-IX)         
129900                MOVE NEJ TO SKRIV-SKROT-SW                                
130000             END-IF                                                       
130100           ELSE                                                           
130200             IF WS-PRART-SPAS NOT = TAB-PRARTSTD-SPAS(WS-IDDC-IX)         
130300                MOVE NEJ TO SKRIV-SKROT-SW                                
130400             END-IF                                                       
130500           END-IF                                                         
130600         END-IF                                                           
130700       END-IF                                                             
130800     END-IF                                                               
130900                                                                          
131000*-------                                                                  
131100     IF TAB-ADLAGOMR-SPAS(WS-IDDC-IX) > ZERO                              
131200        IF SLAG-ADLAGOMR NOT = TAB-ADLAGOMR-SPAS(WS-IDDC-IX)              
131300           MOVE NEJ TO SKRIV-SKROT-SW                                     
131400        END-IF                                                            
131500     END-IF                                                               
131600                                                                          
131700*-------                                                                  
131800     IF TAB-IDPERSON-SPAS(WS-IDDC-IX) > ZERO                              
131900        IF SLAG-IDPERSON-BUY NOT = TAB-IDPERSON-SPAS(WS-IDDC-IX)          
132000           MOVE NEJ TO SKRIV-SKROT-SW                                     
132100        END-IF                                                            
132200     END-IF                                                               
132300                                                                          
132400*-------                                                                  
132500     IF TAB-KDPRODSL-SPAS(WS-IDDC-IX) > ZERO                              
132600        IF ART-KDPRODSL NOT = TAB-KDPRODSL-SPAS(WS-IDDC-IX)               
132700           MOVE NEJ TO SKRIV-SKROT-SW                                     
132800        END-IF                                                            
132900     END-IF                                                               
133000*-------                                                                  
133100                                                                          
133200     IF PASSIV-SKROT-OK                                                   
133300       PERFORM GB-SKRIV-SKROT                                             
133400     END-IF                                                               
133500                                                                          
133600     .                                                                    
133700     EJECT                                                                
133800 GA-SKAPA-KVSKROT SECTION.                                                
133900     MOVE 'GA-SKAPA-KVSKROT '   TO CURRENT-SECTION.                       
134000                                                                          
134100     MOVE ZERO     TO WS-KVSKROT                                          
134200     COMPUTE WS-KVSKROT =                                                 
134300              SLAG-KVLS - SLAG-KVOKS-DAG                                  
134400                        - SLAG-KVOKS-BULK                                 
134500                        - SLAG-KVRESS                                     
134600                        - SLAG-KVUTRS                                     
134700                        - SLAG-KVROS-DAG                                  
134800                        - SLAG-KVROS-BULK                                 
134900     END-COMPUTE                                                          
135000                                                                          
135100                                                                          
135200     IF WS-KVSKROT > ZERO                                                 
135300       MOVE ZERO      TO WS-KVBEART-SKROT                                 
135400       MOVE W-IDARTNR TO W-IDARTNR-Q4B1-MIN                               
135500                         W-IDARTNR-Q4B1-MAX                               
135600       MOVE TAB-IDDISTR-SKROT(WS-IDDC-IX)   TO W-IDDISTR-Q4B1             
135700       MOVE TAB-IDKUNDNR-SKROT(WS-IDDC-IX)  TO W-IDKUNDNR-Q4B1            
135800       MOVE TAB-IDDISTR-QSKROT(WS-IDDC-IX)  TO W-IDDISTR-Q4B1-Q           
135900       MOVE TAB-IDKUNDNR-QSKROT(WS-IDDC-IX) TO W-IDKUNDNR-Q4B1-Q          
136000       MOVE TAB-IDDISTR-RSKROT(WS-IDDC-IX)  TO W-IDDISTR-Q4B1-R           
136100       MOVE TAB-IDKUNDNR-RSKROT(WS-IDDC-IX) TO W-IDKUNDNR-Q4B1-R          
136200                                                                          
136300       PERFORM IMS-GU-WDQ4B1                                              
136400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
136500                                                                          
136600          ADD SEQB-KVBEART-Q TO WS-KVBEART-SKROT                          
136700                                                                          
136800          PERFORM IMS-GN-WDQ4B1                                           
136900       END-PERFORM                                                        
137000       IF WS-KVBEART-SKROT > ZERO                                         
137100          COMPUTE WS-KVSKROT = WS-KVSKROT - WS-KVBEART-SKROT              
137200       END-IF                                                             
137300     END-IF                                                               
137400                                                                          
137500     .                                                                    
137600     EJECT                                                                
137700 GB-SKRIV-SKROT SECTION.                                                  
137800     MOVE 'GB-SKRIV-SKROT '   TO CURRENT-SECTION.                         
137900                                                                          
138000     MOVE '2D '                        TO UT2-IDPTYP                      
138100     MOVE W-IDARTNR                    TO UT2-IDARTNR                     
138200     MOVE W-IDDC                       TO UT2-IDDC                        
138300     MOVE FUNCTION CURRENT-DATE (1:8)  TO UT2-DADATUM                     
138400     MOVE TAB-IDDISTR-SKROT(WS-IDDC-IX)  TO UT2-IDDISTR                   
138500     MOVE TAB-IDKUNDNR-SKROT(WS-IDDC-IX) TO UT2-IDKUNDNR                  
138600     MOVE WS-VALUE-KVSKROT             TO UT2-SUARTSTD                    
138700     MOVE WS-KVSKROT                   TO UT2-KVSKROT                     
138800     MOVE TAB-KDDC(WS-IDDC-IX)         TO UT2-KDDC                        
138900                                                                          
139000     IF UT2-KVSKROT > 0                                                   
139010       MOVE W-IDARTNR       TO TP1ARTK-IDARTNR                            
139020       PERFORM DB2-SELECT-TP1KAMP-TP1ARTK                                 
139030       IF LINES-MISSING                                                   
139100         PERFORM S12-SKRIV-W2712D                                         
139200                                                                          
139300         ADD +1 TO TAB-ANTAL-SKROT-DC(WS-IDDC-IX)                         
139310       ELSE                                                               
139311         MOVE SPACE      TO POSTSUM-TRANSTYP                              
139312         MOVE 'W2712D'   TO POSTSUM-FDNAMN                                
139313         MOVE 'SUPPRESD' TO POSTSUM-DDNAMN2                               
139314         CALL POSTSUM USING POSTSUM-PARM                                  
139320       END-IF                                                             
139400     END-IF                                                               
139500                                                                          
139600     .                                                                    
139700     EJECT                                                                
139800 Z-FINIT SECTION.                                                         
139900     CLOSE W27117                                                         
140000           W2712D                                                         
140100     SKIP2                                                                
140200     MOVE 'S' TO POSTSUM-OPKOD                                            
140300     CALL POSTSUM USING POSTSUM-PARM                                      
140400     .                                                                    
140500     EJECT                                                                
140600                                                                          
140700 S10-ANTAL-TRANS-RET SECTION.                                             
140800                                                                          
140900                                                                          
141000     COMPUTE WS-BALANCE-SEND-DC = SLAG-KVLS         -                     
141100                                  SLAG-KVOKS-DAG    -                     
141200                                  SLAG-KVOKS-BULK   -                     
141300                                  SLAG-KVRESS       -                     
141400                                  SLAG-KVUTRS                             
141500     .                                                                    
141600     EJECT                                                                
141700 S11-SKRIV-W27117 SECTION.                                                
141800     WRITE UT-POST FROM UT-AREA                                           
141900                                                                          
142000     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
142100     MOVE 'W27117' TO POSTSUM-FDNAMN                                      
142200     MOVE 'W2712CD1' TO POSTSUM-DDNAMN2                                   
142300     CALL POSTSUM USING POSTSUM-PARM                                      
142400     .                                                                    
142500     EJECT                                                                
142600 S12-SKRIV-W2712D SECTION.                                                
142700     MOVE 'S12-SKRIV-W2712D '    TO CURRENT-SECTION                       
142800                                                                          
142900     WRITE UT2-POST FROM UT2-AREA                                         
143000                                                                          
143100     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
143200     MOVE 'W2712D' TO POSTSUM-FDNAMN                                      
143300     MOVE 'W2712CD2' TO POSTSUM-DDNAMN2                                   
143400     CALL POSTSUM USING POSTSUM-PARM                                      
143500     .                                                                    
143600     EJECT                                                                
143601                                                                          
143610 S21-BERAKNA-30V-LAGERSALDO-CDC SECTION.                                  
143620     COMPUTE WS-KVPB-TOT-CDC = CLAG-KVPB-SEP                              
143630                             + CLAG-KVPB-SATS                             
143640                             + CLAG-KVPB-TPO                              
143650                                                                          
143660     PERFORM S21A-CALC-PROGNOS-REFILLED-XDC                               
143670                                                                          
143680     COMPUTE WS-KVPB-TOT-CDC-XDC                                          
143690                             = WS-KVPB-TOT-CDC                            
143691                             + WS-KVPB-TOT-XDC                            
143692                                                                          
143693     COMPUTE WS-KVPB-TOT-1V-SEND-DC                                       
143694                             = WS-KVPB-TOT-CDC-XDC / 4.33                 
143695                                                                          
143696     COMPUTE WS-KVPB-TOT-30V-SEND-DC ROUNDED =                            
143697                                 WS-KVPB-TOT-1V-SEND-DC * 30              
143698     .                                                                    
143699     EJECT                                                                
143700                                                                          
143701 S21A-CALC-PROGNOS-REFILLED-XDC SECTION.                                  
143702*  ----  CALC TOTAL FORECAST OF DCS REFILLED FROM CDC                     
143703*  ----  FOR FORECAST OF ALL DC CALL W271UTIL WITH KDCAL 002              
143704                                                                          
143705     INITIALIZE UTIL-W271UTIL                                             
143706     MOVE ALL ZERO              TO WS-KVPB-TOT-XDC                        
143707     MOVE 002                   TO UTIL-KDCALL                            
143708     MOVE W-IDARTNR             TO UTIL-IDARTNR                           
143709     MOVE W-IDDC                TO UTIL-IDDC                              
143710     MOVE SLAG-IDDC-REF         TO UTIL-IDDC-REF                          
143711                                                                          
143712     CALL W271UTIL USING UTIL-W271UTIL                                    
143713                         UTIL-WDK6-PCB                                    
143714                         UTIL-WDK7-PCB                                    
143715                                                                          
143716     IF UTIL-KDSVAR-OK                                                    
143717        MOVE UTIL-KVPB-TOT      TO WS-KVPB-TOT-XDC                        
143718     ELSE                                                                 
143719        MOVE 'FEL FRÅN W271UTIL '                                         
143720                                TO FELTEXT-STR                            
143721        DISPLAY FELTEXT                                                   
143722        PERFORM S99-ABEND                                                 
143723     END-IF                                                               
143724     .                                                                    
143725     EJECT                                                                
143726                                                                          
143730 S22-BERAKNA-6MANADER-SEDAN SECTION.                                      
143800     IF SLAG-IDDC-REF(1:1) = '7' OR '4' OR '5' OR '6'                     
143900       IF SEGMENT-FINNS                                                   
144000         MOVE 'AAMMDD'      TO DAT-KDDATFORM                              
144100         MOVE LART-DAPUBL (3:6)   TO DAT-I-TIDATUM                        
144200                                                                          
144300         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
144400                         DAT-O-TIDATUM DAT-KDSVAR                         
144500                                                                          
144600         IF DAT-KDSVAR-OK                                                 
144700            MOVE DAT-TIAAVVD TO WS-TIAAVVD-MOTTAG                         
144800         ELSE                                                             
144900****BARA   FÖR ATT STOPPA OM DAPUBL ÄR FELAKTIGT ELLER 0                  
145000            MOVE 91111      TO WS-TIAAVVD-MOTTAG                          
145100         END-IF                                                           
145200       ELSE                                                               
145300****BARA FÖR ATT STOPPA OM DAPUBL INTE FINNS LOKALT                       
145400          MOVE 91111        TO WS-TIAAVVD-MOTTAG                          
145500       END-IF                                                             
145600       MOVE WS-TIAAVVD-MOTTAG TO WS-TIFINLV                               
145700                                 ARTWS-TIFINLV                            
145800                                                                          
145900     ELSE                                                                 
146000       MOVE ART-TIFINLV TO WS-TIFINLV                                     
146100                           ARTWS-TIFINLV                                  
146200     END-IF                                                               
146300     IF ARTWS-TIFINLV(1:2) > 50                                           
146400       COMPUTE WS-TIFINLV = WS-TIFINLV + 1900000                          
146500     ELSE                                                                 
146600       COMPUTE WS-TIFINLV = WS-TIFINLV + 2000000                          
146700     END-IF                                                               
146800     DIVIDE WS-TIFINLV BY 10 GIVING WS-TIFINLV-AAAAVV                     
146900                                                                          
147000                                                                          
147100     MOVE DAGENS-TIAAVVD (1:4) TO VADD-DATUM-AAVV                         
147200     MOVE -26                  TO VADD-ANTAL                              
147300     CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                       
147400                                                                          
147500     MOVE VADD-DATUM-AAVV      TO WS-DAGENS-AAAAVV-MINUS-26V              
147600     IF DAGENS-TIAAVVD-AA > 50                                            
147700       COMPUTE WS-DAGENS-AAAAVV-MINUS-26V =                               
147800               WS-DAGENS-AAAAVV-MINUS-26V + 190000                        
147900     ELSE                                                                 
148000       COMPUTE WS-DAGENS-AAAAVV-MINUS-26V =                               
148100               WS-DAGENS-AAAAVV-MINUS-26V + 200000                        
148200     END-IF                                                               
148300     .                                                                    
148400     EJECT                                                                
148500 S32-CHECK-LAST-SALES-DEMAND SECTION.                                     
148600*                                                                         
148700     MOVE SLAG-IDDC                 TO W-IDDC-TRANSF                      
148800     PERFORM IMS-GU-WDL711                                                
148900     IF SEGMENT-FINNS                                                     
149000        MOVE DC-TIREFEFT            TO WS-TIREFEFT                        
149100     ELSE                                                                 
149200        MOVE ZERO                   TO WS-TIREFEFT                        
149300     END-IF                                                               
149400                                                                          
149500     SET INDX TO +1                                                       
149600     SEARCH WS-VALID-IDDC                                                 
149700       AT END                                                             
149800          CONTINUE                                                        
149900       WHEN TAB-IDDC(INDX) = SLAG-IDDC                                    
150000          MOVE TAB-TIVV(INDX) TO WS-TIVV                                  
150100                                                                          
150200     END-SEARCH                                                           
150300                                                                          
150400     IF WS-TIREFEFT > ZERO                                                
150500       MOVE ZERO           TO DAYS-KVDAYS                                 
150600       MOVE 'YYMMDD'       TO DAYS-KDDATFMT1                              
150700       MOVE WS-TIREFEFT    TO DAYS-TIDATE1                                
150800       MOVE 'YYMMDD'       TO DAYS-KDDATFMT2                              
150900       MOVE DAGENS-DATUM   TO DAYS-TIDATE2                                
151000                                                                          
151100       CALL WZ20DAYS USING DAYS-WZ20DAYS                                  
151200                                                                          
151300       IF DAYS-KDRC = +0                                                  
151400         COMPUTE WS-WEEKS = DAYS-KVDAYS / 7                               
151500       ELSE                                                               
151600         MOVE 'ERROR FROM WZ20DAYS MODULE' TO FELTEXT-STR                 
151700         DISPLAY FELTEXT                                                  
151800         DISPLAY DAYS-KDRC                                                
151900         CALL FELLOG                                                      
152000       END-IF                                                             
152100                                                                          
152200       IF WS-WEEKS >= WS-TIVV                                             
152300       OR WS-TIVV = ZERO                                                  
152400          MOVE JA  TO WS-DEMD-DATE-PASSED-SW                              
152500       ELSE                                                               
152600          MOVE NEJ TO WS-DEMD-DATE-PASSED-SW                              
152700       END-IF                                                             
152800     ELSE                                                                 
152900*      IF LAST DEMAND DATE IS ZERO, PASSIVATE THE PART                    
153000       MOVE JA     TO WS-DEMD-DATE-PASSED-SW                              
153100     END-IF                                                               
153200     .                                                                    
153300     EJECT                                                                
153400 S33-CHECK-SUPPLIER SECTION.                                              
153500                                                                          
153600     SET INDX TO +1                                                       
153700     SEARCH WS-VALID-IDDC                                                 
153800       AT END                                                             
153900          MOVE JA            TO  WS-EXTER-SUPPLIER                        
154000       WHEN TAB-IDLEVNR-DC(INDX) = SLAG-IDLEVNR                           
154100          MOVE NEJ           TO  WS-EXTER-SUPPLIER                        
154200                                                                          
154300     END-SEARCH                                                           
154400     .                                                                    
154500     EJECT                                                                
154600 S34-CHECK-LAST-BIN-DATE SECTION.                                         
154700*                                                                         
154800     MOVE NEJ                         TO WS-LAST-BIN-DATE-PASS-SW         
154900     MOVE ZERO                        TO WS-TIINLINL                      
155000                                         TMP1-YYMMDD                      
155100                                         TMP2-YYMMDD                      
155200                                         WS-KVVECKOR-BIN                  
155300                                         WS-BIN-WEEKS                     
155400                                                                          
155500     PERFORM IMS-GU-WDL601                                                
155600     IF SEGMENT-FINNS                                                     
155700        PERFORM IMS-GNP-WDL611                                            
155800        PERFORM UNTIL SEGMENT-SAKNAS                                      
155900          IF WDL6-INL-IDDC   =   SLAG-IDDC                                
156000             MOVE WDL6-INL-TIINLINL                                       
156100                                      TO TMP1-YYMMDD                      
156200             MOVE WS-TIINLINL                                             
156300                                      TO TMP2-YYMMDD                      
156400             PERFORM WY2000P1                                             
156500             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
156600               MOVE WDL6-INL-TIINLINL                                     
156700                                      TO WS-TIINLINL                      
156800             END-IF                                                       
156900          END-IF                                                          
157000          PERFORM IMS-GNP-WDL611                                          
157100        END-PERFORM                                                       
157200     END-IF                                                               
157300                                                                          
157400     SET INDX                         TO +1                               
157500     SEARCH WS-VALID-IDDC                                                 
157600       AT END                                                             
157700          CONTINUE                                                        
157800       WHEN TAB-IDDC(INDX)   = SLAG-IDDC                                  
157900          MOVE TAB-KVVECKOR-BIN(INDX) TO WS-KVVECKOR-BIN                  
158000     END-SEARCH                                                           
158100                                                                          
158200     IF WS-TIINLINL   > ZERO                                              
158300        MOVE ZERO                     TO DAYS-KVDAYS                      
158400        MOVE 'YYMMDD'                 TO DAYS-KDDATFMT1                   
158500        MOVE WS-TIINLINL              TO DAYS-TIDATE1                     
158600        MOVE 'YYMMDD'                 TO DAYS-KDDATFMT2                   
158700        MOVE DAGENS-DATUM             TO DAYS-TIDATE2                     
158800                                                                          
158900        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
159000                                                                          
159100        IF DAYS-KDRC  = +0                                                
159200           COMPUTE WS-BIN-WEEKS = DAYS-KVDAYS / 7                         
159300        ELSE                                                              
159400           MOVE 'ERROR FROM WZ20DAYS FOR BIN DATE' TO FELTEXT-STR         
159500           DISPLAY FELTEXT                                                
159600           DISPLAY DAYS-KDRC                                              
159700           CALL FELLOG                                                    
159800        END-IF                                                            
159900                                                                          
160000        IF WS-BIN-WEEKS    >= WS-KVVECKOR-BIN                             
160100        OR WS-KVVECKOR-BIN  = ZERO                                        
160200           MOVE JA                    TO WS-LAST-BIN-DATE-PASS-SW         
160300        ELSE                                                              
160400           MOVE NEJ                   TO WS-LAST-BIN-DATE-PASS-SW         
160500        END-IF                                                            
160600     END-IF                                                               
160700     .                                                                    
160800     EJECT                                                                
160900 S35-GET-PASSIVE-DATE  SECTION.                                           
161000*                                                                         
161100     MOVE ZERO                        TO DAYS-KVDAYS                      
161200     MOVE 'YYMMDD'                    TO DAYS-KDDATFMT1                   
161300     MOVE SLAG-TIREFSTA               TO WS-DATE-NUM9                     
161400     MOVE WS-DATE-NUM9                TO DAYS-TIDATE1                     
161500     MOVE 'YYMMDD'                    TO DAYS-KDDATFMT2                   
161600     MOVE DAGENS-DATUM                TO DAYS-TIDATE2                     
161700                                                                          
161800     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
161900                                                                          
162000     IF DAYS-KDRC     = +0                                                
162100        COMPUTE WS-PASSIVE-WEEKS = DAYS-KVDAYS / 7                        
162200     ELSE                                                                 
162300        MOVE 'ERROR FROM WZ20DAYS FOR BIN DATE' TO FELTEXT-STR            
162400        DISPLAY FELTEXT                                                   
162500        DISPLAY DAYS-KDRC                                                 
162600        CALL FELLOG                                                       
162700     END-IF                                                               
162800     .                                                                    
162900     EJECT                                                                
163000 S99-ABEND SECTION.                                                       
163100     SKIP2                                                                
163200     MOVE 'S' TO POSTSUM-OPKOD                                            
163300     CALL POSTSUM USING POSTSUM-PARM                                      
163400     CALL ABEND USING RKOD-ABEND                                          
163500     .                                                                    
163600* --- IMS SEKTIONER ---                                                   
163700                                                                          
163800 IMS-GN-WDB601    SECTION.                                                
163900     MOVE 'WDB601  ' TO SSA1                                              
164000     MOVE '  GB' TO GODK-STATUSKODER                                      
164100     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
164200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     .                                                                    
164500     EJECT                                                                
164600 IMS-GU-WDL711 SECTION.                                                   
164700     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
164800          DELIMITED BY SIZE INTO SSA1                                     
164900     STRING 'WDL711  (IDDC     =' W-IDDC-TRANSF-X ')'                     
165000          DELIMITED BY SIZE INTO SSA2                                     
165100     MOVE '  GE' TO GODK-STATUSKODER                                      
165200     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
165300     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
165400     PERFORM IMS-STATUSKONTROLL                                           
165500     .                                                                    
165600     SKIP3                                                                
165700 IMS-GU-WDK601 SECTION.                                                   
165800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
165900          DELIMITED BY SIZE INTO SSA1                                     
166000     MOVE '  GE' TO GODK-STATUSKODER                                      
166100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
166200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
166300     PERFORM IMS-STATUSKONTROLL                                           
166400     .                                                                    
166500     EJECT                                                                
166600                                                                          
166700                                                                          
166800 IMS-GNP-WDK611 SECTION.                                                  
166900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
167000          DELIMITED BY SIZE INTO SSA1                                     
167100     MOVE '  GE' TO GODK-STATUSKODER                                      
167200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
167300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     EJECT                                                                
167700                                                                          
167800 IMS-GU-ARTC01-ERS SECTION.                                               
167900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ERS-X ')'                     
168000          DELIMITED BY SIZE INTO SSA1                                     
168100     MOVE '  GE' TO GODK-STATUSKODER                                      
168200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC01 SSA1               
168300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
168400     PERFORM IMS-STATUSKONTROLL                                           
168500     .                                                                    
168600     EJECT                                                                
168700                                                                          
168800 IMS-GN-WDK7 SECTION.                                                     
168900     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
169000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
169100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
169200     PERFORM IMS-STATUSKONTROLL                                           
169300     .                                                                    
169400     EJECT                                                                
169500                                                                          
169600 IMS-GU-ARTS11-TRANSF SECTION.                                            
169700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
169800          DELIMITED BY SIZE INTO SSA1                                     
169900     STRING 'WLARTS11(IDDC     =' W-IDDC-TRANSF-X ')'                     
170000          DELIMITED BY SIZE INTO SSA2                                     
170100     MOVE '  GE' TO GODK-STATUSKODER                                      
170200     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11-TRANSF                  
170300          SSA1 SSA2                                                       
170400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
170500     PERFORM IMS-STATUSKONTROLL                                           
170600     .                                                                    
170700     EJECT                                                                
170800                                                                          
170900 IMS-GU-WDK712 SECTION.                                                   
171000                                                                          
171100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
171200          DELIMITED BY SIZE INTO SSA1                                     
171300     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-X ')'                        
171400          DELIMITED BY SIZE INTO SSA2                                     
171500     MOVE '    ' TO GODK-STATUSKODER                                      
171600     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK712                        
171700          SSA1 SSA2                                                       
171800     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
171900     PERFORM IMS-STATUSKONTROLL                                           
172000     .                                                                    
172100     EJECT                                                                
172200                                                                          
172300 IMS-GU-WDK712-ERS SECTION.                                               
172400                                                                          
172500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-ERS-X ')'                     
172600          DELIMITED BY SIZE INTO SSA1                                     
172700     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-X ')'                        
172800          DELIMITED BY SIZE INTO SSA2                                     
172900     MOVE '    ' TO GODK-STATUSKODER                                      
173000     CALL CBLTDLI USING GU WDK73-PCB DLI-IO-WDK712-ERS                    
173100          SSA1 SSA2                                                       
173200     MOVE WDK73-STATUS-CODE TO STATUS-WS                                  
173300     PERFORM IMS-STATUSKONTROLL                                           
173400     .                                                                    
173500     EJECT                                                                
173600                                                                          
173700 IMS-GU-WDK711-MOTTAG SECTION.                                            
173800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
173900          DELIMITED BY SIZE INTO SSA1                                     
174000     STRING 'WDK711  (IDDC     =' W-IDDC-REF-X ')'                        
174100          DELIMITED BY SIZE INTO SSA2                                     
174200     MOVE '  GE' TO GODK-STATUSKODER                                      
174300     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK711-MOTTAG                 
174400          SSA1 SSA2                                                       
174500     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
174600     PERFORM IMS-STATUSKONTROLL                                           
174700     .                                                                    
174800     EJECT                                                                
174900                                                                          
175000 IMS-GU-WDK722 SECTION.                                                   
175100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
175200          DELIMITED BY SIZE INTO SSA1                                     
175300     STRING 'WDK711  (IDDC     =' W-IDDC-REF-X ')'                        
175400          DELIMITED BY SIZE INTO SSA2                                     
175500     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY ')'                          
175600          DELIMITED BY SIZE INTO SSA3                                     
175700     MOVE '  GE' TO GODK-STATUSKODER                                      
175800     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK722                        
175900          SSA1 SSA2 SSA3                                                  
176000     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
176100     PERFORM IMS-STATUSKONTROLL                                           
176200     .                                                                    
176300     EJECT                                                                
176400                                                                          
176500 IMS-GU-WDD905-LAST SECTION.                                              
176600                                                                          
176700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
176800          DELIMITED BY SIZE INTO SSA1                                     
176900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
177000          DELIMITED BY SIZE INTO SSA2                                     
177100     STRING 'WDD905  *L(KDAVROP  =' W-KDAVROP-X ')'                       
177200          DELIMITED BY SIZE INTO SSA3                                     
177300     MOVE '  GE' TO GODK-STATUSKODER                                      
177400     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1                    
177500                                                  SSA2                    
177600                                                  SSA3                    
177700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
177800     PERFORM IMS-STATUSKONTROLL                                           
177900     .                                                                    
178000     EJECT                                                                
178100                                                                          
178200                                                                          
178300 IMS-GU-WDD701 SECTION.                                                   
178400                                                                          
178500     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
178600          DELIMITED BY SIZE INTO SSA1                                     
178700     MOVE '  GE' TO GODK-STATUSKODER                                      
178800     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
178900     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
179000     PERFORM IMS-STATUSKONTROLL                                           
179100     .                                                                    
179200     EJECT                                                                
179300                                                                          
179400                                                                          
179500 IMS-GNP-WDD702 SECTION.                                                  
179600                                                                          
179700     MOVE 'WDD702   ' TO SSA1                                             
179800     MOVE '  GE' TO GODK-STATUSKODER                                      
179900     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
180000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
180100     PERFORM IMS-STATUSKONTROLL                                           
180200     .                                                                    
180300     EJECT                                                                
180400 IMS-GU-WDQ4B1 SECTION.                                                   
180500     MOVE 'IMS-GU-WDQ4B1 '    TO DBS-SECTION                              
180600                                                                          
180700     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
180800                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
180900                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
181000                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X                       
181100                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
181200                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
181300                    '&IDDISTR  =' W-IDDISTR-Q4B1-Q-X                      
181400                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-Q-X                     
181500                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
181600                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
181700                    '&IDDISTR  =' W-IDDISTR-Q4B1-R-X                      
181800                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-R-X ')'                 
181900                                                                          
182000          DELIMITED BY SIZE INTO SSA1                                     
182100     MOVE '  GE' TO GODK-STATUSKODER                                      
182200     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
182300     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
182400     PERFORM IMS-STATUSKONTROLL                                           
182500     .                                                                    
182600     SKIP3                                                                
182700 IMS-GN-WDQ4B1 SECTION.                                                   
182800     MOVE 'IMS-GN-WDQ4B1 '    TO DBS-SECTION                              
182900                                                                          
183000     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
183100                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
183200                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
183300                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X                       
183400                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
183500                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
183600                    '&IDDISTR  =' W-IDDISTR-Q4B1-Q-X                      
183700                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-Q-X                     
183800                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
183900                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
184000                    '&IDDISTR  =' W-IDDISTR-Q4B1-R-X                      
184100                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-R-X ')'                 
184200                                                                          
184300                                                                          
184400          DELIMITED BY SIZE INTO SSA1                                     
184500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
184600     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
184700     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
184800     PERFORM IMS-STATUSKONTROLL                                           
184900     .                                                                    
185000     SKIP3                                                                
185100 IMS-GU-WDL601 SECTION.                                                   
185200                                                                          
185300     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
185400            DELIMITED BY SIZE INTO SSA1                                   
185500     MOVE '  GE' TO GODK-STATUSKODER                                      
185600     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
185700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
185800     PERFORM IMS-STATUSKONTROLL                                           
185900     .                                                                    
186000     SKIP3                                                                
186100 IMS-GNP-WDL611      SECTION.                                             
186200                                                                          
186300     STRING 'WDL611     '                                                 
186400            DELIMITED BY SIZE INTO SSA1                                   
186500     MOVE '  GE' TO GODK-STATUSKODER                                      
186600     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
186700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
186800     PERFORM IMS-STATUSKONTROLL                                           
186900     .                                                                    
187000     SKIP3                                                                
187010                                                                          
187020 DB2-SELECT-TP1KAMP-TP1ARTK  SECTION.                                     
187030     MOVE 000100811  TO GOOD-SQLCODECODES                                 
187040                                                                          
187050     EXEC SQL                                                             
187060           SELECT  K.IDKAMP                                               
187091                                                                          
187092           INTO   :TP1KAMP-IDKAMP                                         
187096                                                                          
187098           FROM    TP1KAMP K                                              
187099                  ,TP1ARTK A                                              
187100           WHERE K.IDKAMP = A.IDKAMP                                      
187101             AND IDARTNR  = :TP1ARTK-IDARTNR                              
187105             AND (  (   TISTODAT_KAMP < 990000                            
187106                    AND TISTODAT_KAMP >                                   
187107                          INT(REPLACE(SUBSTR(CHAR(CURRENT_DATE)           
187108                                             ,3,8),'-',''))               
187109                    )                                                     
187110                 OR (   TISTODAT_KAMP = 0                                 
187111                    AND TISTADAT_KAMP < 940000                            
187112                    AND TISTADAT_KAMP +  50000 >                          
187113                          INT(REPLACE(SUBSTR(CHAR(CURRENT_DATE)           
187114                                             ,3,8),'-',''))               
187115                    )                                                     
187116                 )                                                        
187117     END-EXEC                                                             
187118                                                                          
187119     MOVE SQLCODE TO SQLCODE-WS                                           
187120     PERFORM DB2-STATUS-CHECK                                             
187121     .                                                                    
187122     EJECT                                                                
187130                                                                          
187200 DB2-SELECT-TP4TRAN     SECTION.                                          
187300     MOVE 000100  TO GOOD-SQLCODECODES                                    
187400                                                                          
187500     EXEC SQL                                                             
187600           SELECT  IDDC_SEND                                              
187700                  ,IDDC_REC                                               
187800                  ,IDDISTR                                                
187900                  ,IDKUNDNR                                               
188000                                                                          
188100           INTO   :TP4TRAN-IDDC-SEND                                      
188200                 ,:TP4TRAN-IDDC-REC                                       
188300                 ,:TP4TRAN-IDDISTR                                        
188400                 ,:TP4TRAN-IDKUNDNR                                       
188500                                                                          
188600           FROM    TP4TRAN                                                
188700                                                                          
188800           WHERE IDDC_SEND = :WS-IDDC-SEND                                
188900           AND   IDDC_REC  = :WS-IDDC-REC                                 
189000           AND   KDARBTYP  = 'ESC'                                        
189100     END-EXEC                                                             
189200                                                                          
189300     MOVE SQLCODE TO SQLCODE-WS                                           
189400     PERFORM DB2-STATUS-CHECK                                             
189500     .                                                                    
189600     EJECT                                                                
189700                                                                          
189800 DB2-STATUS-CHECK  SECTION.                                               
189900     SET SQLCODE-IX TO 1                                                  
190000     SEARCH GOOD-SQLCODE                                                  
190100       AT END                                                             
190200          CALL ABEND USING RKOD-ABEND-DB2                                 
190300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
190400     END-SEARCH                                                           
190500     .                                                                    
190600     EJECT                                                                
190700                                                                          
190800 IMS-STATUSKONTROLL SECTION.                                              
190900     SET STATUS-IX TO 1                                                   
191000     SEARCH GODK-STATUS                                                   
191100       AT END                                                             
191200         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
191300         DISPLAY FELTEXT                                                  
191400         CALL FELLOG                                                      
191500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
191600         CONTINUE                                                         
191700     END-SEARCH                                                           
191800     .                                                                    
191900     EJECT                                                                
192000*    -COPY WY2000P1                                                       
192100                                                                          
