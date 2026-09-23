000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018600.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   NOVEMBER 2004                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.LOADRELEASETRANSPORT                        
000800*    WEB-LDC: WL018600 PROGRAM IS A REPLICA OF W4066300 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        DE KOLLIN PÅ EN VISS TRANSPORT SOM ÄR VALBARA FÖR                
001300*        FAKT/LAST VISAS.                                                 
001400*        GENOM ATT VÄLJA KOLLI ELLER REGISTRERA KOLLI LASTNINGS-          
001500*        RELEASAS DE FÖR ATT SEDAN FAKTURARELEASAS PÅ BILD 4664.          
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WDE6                                       
001800*        PROGRAMMET UPPDATERAR 4495/6/8 (WDR4)                            
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: WL0186U                                             
002200*        REQUEST:     WL0186I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WL0186O1                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900 INPUT-OUTPUT SECTION.                                                    
003000 FILE-CONTROL.                                                            
003100                                                                          
003200 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(08)   VALUE 'WL018600'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
004200 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
004300 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
004400                                                                          
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
004800 77  MIXED                       PIC X       VALUE 'M'.                   
004900 77  WS-KVRADER                  PIC S9(9).                               
005000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
005500 77  W-KVKOLLI                   PIC S9(3)   VALUE ZERO COMP-3.           
005600 77  W-VLORDBTO            PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
005700 77  W-VKORDBTO            PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
005800 77  W-KVKOLLI-SIDA              PIC S9(3)   VALUE ZERO COMP-3.           
005900 77  W-VLORDBTO-SIDA       PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
006000 77  W-VKORDBTO-SIDA       PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
006100 77  W-KVKOLLI-TOT               PIC S9(4)   VALUE ZERO COMP-3.           
006200 77  W-VLORDBTO-TOT        PIC S9(5)V9(3)    VALUE ZERO COMP-3.           
006300 77  W-VKORDBTO-TOT        PIC S9(7)V9(1)    VALUE ZERO COMP-3.           
006400 77  WS-VLORDBTO           PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
006500 77  WS-VKORDBTO           PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
006600 77  WS-VLORDBTO-TOT       PIC S9(5)V9(3)    VALUE ZERO COMP-3.           
006700 77  WS-VKORDBTO-TOT       PIC S9(7)V9(1)    VALUE ZERO COMP-3.           
006900 77  W-IDPSN-NUM                 PIC  9(3)   VALUE ZERO.                  
007000 01  WS-TILASTID                 PIC 9(6)    VALUE ZERO.                  
007100 01  WS-TILASTID-GRP             REDEFINES WS-TILASTID.                   
007200     03 WS-TILASTID-HHMM         PIC 9(4).                                
007300     03 WS-TILASTID-SS           PIC 9(2).                                
007400                                                                          
007500                                                                          
007600 01  WS-CURRENT-DATE             PIC 9(8)    VALUE ZERO.                  
007700 01  FILLER REDEFINES WS-CURRENT-DATE.                                    
007800     03  FILLER                  PIC 9(2).                                
007900     03  WS-CURRENT-YYMMDD       PIC 9(6).                                
008000 01  WS-CURRENT-TIME             PIC 9(8)    VALUE ZERO.                  
008100 01  FILLER REDEFINES WS-CURRENT-TIME.                                    
008200     03  WS-CURRENT-HHMM         PIC 9(4).                                
008300     03  FILLER                  PIC 9(4).                                
008400                                                                          
008500 01  WS-DARFS-MIN                PIC 9(12).                               
008600 01  WS-DARFS-MIN-X   REDEFINES WS-DARFS-MIN.                             
008700     03  WS-DARFS-CCYYMMDD-MIN.                                           
008800       05  WS-DARFS-CC-MIN       PIC 9(2).                                
008900       05  WS-DARFS-YYMMDD-MIN   PIC 9(6).                                
009000     03  WS-DARFS-TTMM-MIN       PIC 9(4).                                
009100                                                                          
009200 01  WS-DARFS-MAX                PIC 9(12).                               
009300 01  WS-DARFS-MAX-X   REDEFINES WS-DARFS-MAX.                             
009400     03  WS-DARFS-CCYYMMDD-MAX.                                           
009500       05  WS-DARFS-CC-MAX       PIC 9(2).                                
009600       05  WS-DARFS-YYMMDD-MAX   PIC 9(6).                                
009700     03  WS-DARFS-TTMM-MAX       PIC 9(4).                                
009800                                                                          
009900                                                                          
010000 77  W-FUNKTION              PIC S9(1) COMP-3 VALUE +0.                   
010100     88  LASTA-HELA-SIDAN                     VALUE +1.                   
010200     88  LASTA-VALDA-RADER                    VALUE +2.                   
010300                                                                          
010400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
010500     88  KEYS-OK                             VALUE 'J'.                   
010600     88  KEYS-WRONG                          VALUE 'N'.                   
010700                                                                          
010800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010900     88  INDATA-OK                           VALUE 'J'.                   
011000     88  INDATA-FEL                          VALUE 'N'.                   
011100                                                                          
011500 77  SW-IDDISTR-LAAST        PIC  X(1)        VALUE 'N'.                  
011600     88  IDDISTR-LAAST                        VALUE 'J'.                  
011700                                                                          
011800 77  TRAFF-SW                PIC  X(1)        VALUE 'N'.                  
011900     88  TRAFF                                VALUE 'J'.                  
012000                                                                          
012100 77  SW-TRPT                 PIC  X(1)        VALUE 'N'.                  
012200     88  TRPT-FINNS                           VALUE 'J'.                  
012300                                                                          
012400 01  TEST-IDDISTR            PIC  9(5) COMP-3 VALUE ZERO.                 
012500*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
012600     EJECT                                                                
012700                                                                          
012800 01  FILLER                  PIC X(16) VALUE 'KONSTANTER'.                
012900 01  KONSTANTER.                                                          
013000     03  KLI-PACK            PIC S9(1) COMP-3 VALUE +1.                   
013100     03  KLI-PACK-FAKT       PIC S9(1) COMP-3 VALUE +6.                   
013200     03  W-LASTA-HELA-SIDAN  PIC S9(1) COMP-3 VALUE +1.                   
013300     03  W-LASTA-VALDA-RADER PIC S9(1) COMP-3 VALUE +2.                   
013400     03  W-VALD              PIC  X(1)        VALUE 'X'.                  
013500                                                                          
015800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
015900 01  GENERAL-SUBPROGRAMS.                                                 
016000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
016400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
016600     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
016700     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
016800                                                                          
016900*    --- PARAMETERS TO ABEND                                              
017000 01  MESSAGE-CODES.                                                       
017100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
017200     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
017300                                                                          
017400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
017600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
017700                                                                          
017800*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
017900*01 -COPY WWOMVAND                                                        
018000                                                                          
018100 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
018200*01  -COPY WDATAREA                                                       
018300                                                                          
018400*                                                                         
018500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
018600*01  -COPY WZ01SUB                                                        
018700 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
018800*01  -COPY WMSGCONV                                                       
018900                                                                          
019000 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
019100*01  -COPY WZ01AUTH                                                       
019200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
019300 01  REQU-AREA.                                                           
019400*    03  -COPY WZ01REQ2                                                   
019500*    03  -COPY WL0186I1                                                   
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
019800     SKIP3                                                                
019900 01  RESP-AREA.                                                           
020000*    03  -COPY WZ01RES2                                                   
020100*    03  -COPY WL0186O1                                                   
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16)   VALUE 'WL01TIDZ '.           
020400*01  -COPY WL01TIDZ                                                       
020500     EJECT                                                                
020600                                                                          
020700******************************************************************        
020800*****                                                                     
020900*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021000*****                                                                     
021100 01  IMS-KEYS.                                                            
021200   03    FILLER          PIC X(16)   VALUE 'IMS KEYS        '.            
021300                                                                          
021400 01    NYCKLAR-TILL-DLI.                                                  
021500   03    W-IDGMT-X.                                                       
021600     05  W-IDDISTR-WDB2        PIC S9(5)   VALUE ZERO  COMP-3.            
021700     05  W-IDKUNDNR-WDB2       PIC S9(7)   VALUE ZERO  COMP-3.            
021800                                                                          
021900     03  W-IDPRODNR-X.                                                    
022000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
022100     03  W-IDKOLLI-X.                                                     
022200         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
022300     03  W-IDTRPTNR-E6-X.                                                 
022400         05 W-IDTRPTNR-E6        PIC S9(3)   VALUE ZERO  COMP-3.          
022500     03  W-WDE6E1KY-MIN-X.                                                
022600         05 W-E6E-IDDC-MIN     PIC  X(2)   VALUE SPACE.                   
022700         05 W-E6E-IDKOLLIS-MIN PIC S9(5)   VALUE ZERO  COMP-3.            
022800         05 FILLER             PIC  X(7).                                 
022900     03  W-WDE6E1KY-MAX-X.                                                
023000         05 W-E6E-IDDC-MAX     PIC  X(2)   VALUE SPACE.                   
023100         05 W-E6E-IDKOLLIS-MAX PIC S9(5)   VALUE ZERO  COMP-3.            
023200         05 FILLER             PIC  X(7).                                 
023300     03 W-WDGXKEY-X.                                                      
023400         05 W-IDHTYP             PIC  X(4)   VALUE '4495'.                
023500         05 W-IDDC-4495          PIC  X(2)   VALUE SPACE.                 
023600         05 W-IDTRPTNR           PIC S9(3)   VALUE ZERO  COMP-3.          
023700         05 W-IDLBBET            PIC X(12)   VALUE SPACE.                 
023800         05 FILLER               PIC X(10)   VALUE LOW-VALUE.             
023900     03  W-WDE4ASEQ-X.                                                    
024000         05  W-4A1-IDDISTR       PIC S9(5)   VALUE ZERO  COMP-3.          
024100         05  W-4A1-IDKUNDNR      PIC S9(7)   VALUE ZERO  COMP-3.          
024200         05  W-4A1-IDKUNDRF.                                              
024300             07  W-4A1-IDORDNR   PIC X(5).                                
024400             07  FILLER          PIC X(5).                                
024500     03  W-WDE6C1KY-MIN-X.                                                
024600         05 W-IDTRPTNR-MIN     PIC S9(3)   VALUE ZERO  COMP-3.            
024700         05 W-DARFS-MIN        PIC 9(12)   VALUE ZERO.                    
024800         05 W-ADCLGEO-MIN.                                                
024900           07 W-IDDC-MIN       PIC X(2)    VALUE SPACE.                   
025000           07 W-ADFLGEO-MIN    PIC X(3)    VALUE SPACE.                   
025100         05 W-ADFLOMR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.            
025200         05 W-ADRUTNIV-MIN     PIC S9(3)   VALUE ZERO  COMP-3.            
025300         05 W-ADVMODUL-MIN     PIC S9(3)   VALUE ZERO  COMP-3.            
025400         05 W-IDDISTR-MIN      PIC S9(5)   VALUE ZERO  COMP-3.            
025500         05 W-IDKUNDNR-MIN     PIC S9(7)   VALUE ZERO  COMP-3.            
025600         05 W-IDPRODNR-KOLLI-MIN PIC S9(7) VALUE ZERO  COMP-3.            
025700         05 W-IDKOLLI-FLER-MIN PIC S9(5)   VALUE ZERO  COMP-3.            
025800         05 W-IDKOLLI-MIN      PIC S9(5)   VALUE ZERO  COMP-3.            
025900                                                                          
026000     03  W-WDE6C1KY-MAX-X.                                                
026100         05 W-IDTRPTNR-MAX     PIC S9(3)   VALUE ZERO  COMP-3.            
026200         05 W-DARFS-MAX        PIC 9(12)   VALUE ZERO.                    
026300         05 W-ADCLGEO-MAX.                                                
026400           07 W-IDDC-MAX       PIC X(2)    VALUE SPACE.                   
026500           07 W-ADFLGEO-MAX    PIC X(3)    VALUE SPACE.                   
026600         05 W-ADFLOMR-MAX      PIC S9(3)   VALUE ZERO  COMP-3.            
026700         05 W-ADRUTNIV-MAX     PIC S9(3)   VALUE ZERO  COMP-3.            
026800         05 W-ADVMODUL-MAX     PIC S9(3)   VALUE ZERO  COMP-3.            
026900         05 W-IDDISTR-MAX      PIC S9(5)   VALUE ZERO  COMP-3.            
027000         05 W-IDKUNDNR-MAX     PIC S9(7)   VALUE ZERO  COMP-3.            
027100         05 W-IDPRODNR-KOLLI-MAX PIC S9(7) VALUE ZERO  COMP-3.            
027200         05 W-IDKOLLI-FLER-MAX PIC S9(5)   VALUE ZERO  COMP-3.            
027300         05 W-IDKOLLI-MAX      PIC S9(5)   VALUE ZERO  COMP-3.            
027310                                                                          
027311     03  W-WDE6H1KY-MIN-X.                                                
027312         05 W-IDDC-CROSS-MIN      PIC X(2).                               
027313         05 W-KDKOLSTA-CROSS-MIN  PIC S9      VALUE 1  COMP-3.            
027314         05 W-TIRFS-MIN           PIC 9(06)   VALUE ZERO.                 
027315         05 FILLER                PIC X(14)   VALUE LOW-VALUE.            
027316                                                                          
027317     03  W-WDE6H1KY-MAX-X.                                                
027318         05 W-IDDC-CROSS-MAX      PIC X(2).                               
027319         05 W-KDKOLSTA-CROSS-MAX  PIC S9      VALUE 1  COMP-3.            
027320         05 W-TIRFS-MAX           PIC 9(06)   VALUE 999999.               
027321         05 FILLER                PIC X(14)   VALUE HIGH-VALUE.           
027322                                                                          
027323     03  W-TIRECXDAT-X.                                                   
027324         05 W-TIRECXDAT        PIC 9(6)    VALUE 0.                       
027325                                                                          
027326     03  W-KDKOLSTX-X.                                                    
027327         05 W-KDKOLSTA-CROSS   PIC S9      VALUE 1     COMP-3.            
027328                                                                          
027329     03  W-IDTRPTNC-X.                                                    
027330         05 W-IDTRPTNR-CROSS   PIC S9(3)   VALUE 0     COMP-3.            
027331                                                                          
027340                                                                          
027400     03  W-WDE4F1KY-MIN-X.                                                
027500         05 W-E4F-IDPRODNR-MIN PIC S9(7)   VALUE ZERO  COMP-3.            
027600         05 W-E4F-IDKOLLI-MIN  PIC S9(5)   VALUE ZERO  COMP-3.            
027700         05 FILLER             PIC  X(22)  VALUE LOW-VALUE.               
027800     03  W-WDE4F1KY-MAX-X.                                                
027900         05 W-E4F-IDPRODNR-MAX PIC S9(7)   VALUE ZERO  COMP-3.            
028000         05 W-E4F-IDKOLLI-MAX  PIC S9(5)   VALUE ZERO  COMP-3.            
028100         05 FILLER             PIC  X(22)  VALUE HIGH-VALUE.              
028200     03  W-IDDC-B6-X.                                                     
028300         05  W-IDDC-B6         PIC X(2)    VALUE SPACE.                   
028400                                                                          
028500 01  IMS-WS.                                                              
028600   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
028700*****                    **** STATUS-KOD FRÅN IMS                         
028800   03    STATUS-WS       PIC XX.                                          
028900         88  SEGMENT-FOUND       VALUE '  '.                              
029000         88  SEGMENT-MISSING     VALUE 'GE'.                              
029100         88  SEGMENT-EXISTS      VALUE 'II'.                              
029200         88  END-OF-DB           VALUE 'GB'.                              
029300                                                                          
029400   03    GOOD-STATUSCODES.                                                
029500     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029600                                                                          
029700 01      SSA1            PIC X(256) VALUE SPACE.                          
029800 01      SSA2            PIC X(256) VALUE SPACE.                          
029900                                                                          
030000*                            IMS FUNKTIONSKODER                           
030100*01      -COPY W0003                                                      
030200                                                                          
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
030400 01  DLI-IO-E601.                                                         
030500*    03  -COPY WDE601                                                     
030600                                                                          
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
030800 01  DLI-IO-E611.                                                         
030900*    03  -COPY WDE611                                                     
031010                                                                          
031020 01  FILLER         PIC X(16) VALUE 'DLI-IO-E621'.                        
031030 01  DLI-IO-E621.                                                         
031040*    03  -COPY WDE621                                                     
031050                                                                          
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6C1'.                        
031200 01  DLI-IO-E6C1.                                                         
031300*    03  -COPY WDE6C1                                                     
031410                                                                          
031420 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6H1'.                        
031430 01  DLI-IO-E6H1.                                                         
031440*    03  -COPY WDE6H1                                                     
031450                                                                          
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6E1'.                        
031600 01  DLI-IO-E6E1.                                                         
031700*    03  -COPY WDE6E1                                                     
031800                                                                          
031900 01  FILLER         PIC X(16) VALUE 'DLI-IO-E401'.                        
032000 01  DLI-IO-E401.                                                         
032100*    03  -COPY WDE401                                                     
032200                                                                          
032300 01  FILLER         PIC X(16) VALUE 'DLI-IO-E4F1'.                        
032400 01  DLI-IO-E4F1.                                                         
032500*    03  -COPY WDE4F1                                                     
032600                                                                          
032700 01  FILLER         PIC X(16) VALUE 'DLI-IO-4495'.                        
032800 01  DLI-IO-4495.                                                         
032900*    03  -COPY WDGX4495                                                   
033000                                                                          
033100 01  FILLER         PIC X(16) VALUE 'DLI-IO-4496'.                        
033200 01  DLI-IO-4496.                                                         
033300*    03  -COPY WDGX4496                                                   
033400                                                                          
033500 01  FILLER         PIC X(16) VALUE 'DLI-IO-4498'.                        
033600 01  DLI-IO-4498.                                                         
033700*    03  -COPY WDGX4498                                                   
033800                                                                          
033900 01  FILLER         PIC X(16) VALUE 'DLI-IO-B201'.                        
034000 01  DLI-IO-B201.                                                         
034100*    03  -COPY WDB201                                                     
034200     EJECT                                                                
034300 01  FILLER               PIC X(16)   VALUE 'DLI-IO-B601'.                
034400 01  DLI-IO-B601.                                                         
034500*    03  -COPY WDB601                                                     
034600     EJECT                                                                
034700                                                                          
034800 LINKAGE SECTION.                                                         
034900*01  -COPY W0009   -PRE MSG-                                              
035000 01  ATAB-PCB                 PIC X.                                      
035400*01  -COPY W0008  -PRE WDE6C-                                             
035500     05  FILLER                  PIC X.                                   
035610                                                                          
035620*01  -COPY W0008  -PRE WDE6H-                                             
035630     05  FILLER                  PIC X.                                   
035640                                                                          
035700*01  -COPY W0008  -PRE WDE6-                                              
035800     05  FILLER                  PIC X.                                   
035900                                                                          
036000*01  -COPY W0008  -PRE WDE4-                                              
036100     05  FILLER                  PIC X.                                   
036200                                                                          
036300*01  -COPY W0008  -PRE WDE4F-                                             
036400     05  FILLER                  PIC X.                                   
036500                                                                          
036600*01  -COPY W0008  -PRE 4495-                                              
036700     05  FILLER                  PIC X.                                   
036800                                                                          
036900*01  -COPY W0008  -PRE WDE6E-                                             
037000     05  FILLER                  PIC X.                                   
037100                                                                          
037200*01  -COPY W0008  -PRE WDB2-                                              
037300     05  FILLER                  PIC X.                                   
037400                                                                          
037500*01  -COPY W0008  -PRE WDB6-                                              
037600     05  FILLER                  PIC X.                                   
037800                                                                          
037900                                                                          
037910 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB                               
037920                           WDE6C-PCB WDE6H-PCB                            
037930                           WDE6-PCB WDE4-PCB WDE4F-PCB                    
037940                           4495-PCB WDE6E-PCB WDB2-PCB                    
037950                           WDB6-PCB.                                      
038800                                                                          
038900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
039000     IF SUB-KDRC = 0                                                      
039100       PERFORM A-INIT                                                     
039200       PERFORM B-CHECK-KEYS                                               
039300       IF KEYS-OK                                                         
039400         IF REQU-KDPGMACT = 'E'                                           
039500           PERFORM C-KOLLA-INPUT                                          
039600           IF INDATA-OK                                                   
039700             PERFORM D-UPPDATERA                                          
039800           END-IF                                                         
039900         END-IF                                                           
040000         IF INDATA-OK                                                     
040100           PERFORM E-LAES-VISA-INFO                                       
040200         END-IF                                                           
040300       END-IF                                                             
040400       IF SUB-KDTRANS(1:6) = 'WLA186'                                     
040500         PERFORM S12-MSG-CONV                                             
040600       END-IF                                                             
040700       PERFORM S02-RETURN-RESPONSE                                        
040800     END-IF                                                               
040900                                                                          
041000     MOVE ZERO TO RETURN-CODE                                             
041600     GOBACK                                                               
041700     .                                                                    
041800     EJECT                                                                
045700                                                                          
045710 A-INIT SECTION.                                                          
045720     MOVE 'A-INIT' TO CURR-SECTION                                        
045730                                                                          
045740     MOVE FUNCTION CURRENT-DATE(1:8)                                      
045750                                 TO WS-CURRENT-DATE                       
045760     MOVE FUNCTION CURRENT-DATE(9:8)                                      
045770                                 TO WS-CURRENT-TIME                       
045780                                                                          
045790     MOVE LOW-VALUES TO RESP-AREA                                         
045791     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
045792                       RESP-IDMSG-INFO                                    
045793                       RESP-IDELMT-ERROR                                  
045794     MOVE 001       TO RESP-IDRESVER                                      
045795     MOVE ZERO      TO RESP-KVRADER-MAX                                   
045796     IF SUB-KDTRANS(1:6) = 'WLA186'                                       
045797       MOVE 001                  TO AUTH-KDCALL                           
045798       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
045799                                    REQU-WZ01REQ2                         
045800       IF AUTH-KDRC > 0                                                   
045801         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
045802         MOVE NOO                TO KEYS-SW                               
045803       END-IF                                                             
045804       MOVE FUNCTION UPPER-CASE (REQU-IDUSER) TO                          
045805                                 REQU-IDUSER                              
045806       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
045807                                 REQU-IDDC-KEY                            
045808       MOVE FUNCTION UPPER-CASE (REQU-KDFARLIG-KEY) TO                    
045809                                 REQU-KDFARLIG-KEY                        
045810       MOVE FUNCTION UPPER-CASE (REQU-IDLBBET-KEY) TO                     
045811                                 REQU-IDLBBET-KEY                         
045812     END-IF                                                               
045813                                                                          
045814     MOVE LOW-VALUE            TO W-WDE6C1KY-MIN-X                        
045815     MOVE HIGH-VALUE           TO W-WDE6C1KY-MAX-X                        
045816     .                                                                    
058100                                                                          
058110 B-CHECK-KEYS SECTION.                                                    
058120     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
058130                                                                          
058140     MOVE YES              TO KEYS-SW                                     
058150     MOVE REQU-IDDC-KEY    TO RESP-IDDC-KEY                               
058160                              W-IDDC-4495                                 
058170                              W-IDDC-MIN                                  
058180                              W-IDDC-MAX                                  
058190                              W-IDDC-CROSS-MIN                            
058191                              W-IDDC-CROSS-MAX                            
058192                                                                          
058193     IF REQU-KDPGMACT = 'S' OR 'E'                                        
058194        CONTINUE                                                          
058195     ELSE                                                                 
058196        MOVE '023'              TO RESP-IDMSG-ERROR                       
058197*       WRONG ACTION KEY ***                                              
058198        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
058199        MOVE NOO                TO KEYS-SW                                
058200     END-IF                                                               
058201                                                                          
058202     IF KEYS-OK                                                           
058203       IF REQU-IDTRPTNR-KEY = LOW-VALUES                                  
058204         MOVE '026'           TO RESP-IDMSG-ERROR                         
058205*        NO INPUT DATA IS ENTERED ***                                     
058206         MOVE 'IDTRP'         TO RESP-IDELMT-ERROR                        
058207         MOVE NOO             TO KEYS-SW                                  
058208       ELSE                                                               
058209         IF REQU-IDTRPTNR-KEY NOT NUMERIC                                 
058210           MOVE '024'        TO RESP-IDMSG-ERROR                          
058211*          NOT NUMERIC ***                                                
058212           MOVE 'IDTRP'      TO RESP-IDELMT-ERROR                         
058213           MOVE NOO          TO KEYS-SW                                   
058214         ELSE                                                             
058215           IF REQU-IDTRPTNR-KEY = ZERO                                    
058216             MOVE '023'        TO RESP-IDMSG-ERROR                        
058217*            NOT NUMERIC ***                                              
058218             MOVE 'IDTRP'      TO RESP-IDELMT-ERROR                       
058219             MOVE NOO          TO KEYS-SW                                 
058220           ELSE                                                           
058221             MOVE REQU-IDTRPTNR-KEY                                       
058222                               TO RESP-IDTRPTNR-KEY                       
058223                                  W-IDTRPTNR-MIN                          
058224                                  W-IDTRPTNR-MAX                          
058225                                  W-IDTRPTNR-E6                           
058226                                  W-IDTRPTNR                              
058227                                  W-IDTRPTNR-CROSS                        
058228           END-IF                                                         
058229         END-IF                                                           
058230       END-IF                                                             
058231     END-IF                                                               
058232                                                                          
058233     IF KEYS-OK                                                           
058234        IF REQU-IDLBBET-KEY = LOW-VALUES                                  
058235           MOVE '026'           TO RESP-IDMSG-ERROR                       
058236*          NO INPUT DATA IS ENTERED ***                                   
058237           MOVE 'IDLBBET'       TO RESP-IDELMT-ERROR                      
058238           MOVE NOO             TO KEYS-SW                                
058239        ELSE                                                              
058240           MOVE REQU-IDLBBET-KEY                                          
058241                                TO RESP-IDLBBET-KEY                       
058242                                   W-IDLBBET                              
058243        END-IF                                                            
058244     END-IF                                                               
058245                                                                          
058246     IF KEYS-OK                                                           
058247        IF REQU-KDFARLIG-KEY = 'Y' OR 'N' OR 'M'                          
058248           MOVE REQU-KDFARLIG-KEY   TO RESP-KDFARLIG-KEY                  
058249        ELSE                                                              
058250           MOVE '023'           TO RESP-IDMSG-ERROR                       
058251*          IS INVALID   ***                                               
058252           MOVE 'KDFARLIG'      TO RESP-IDELMT-ERROR                      
058253           MOVE NOO             TO KEYS-SW                                
058254        END-IF                                                            
058255     END-IF                                                               
058256                                                                          
058257                                                                          
058258     IF KEYS-OK                                                           
058259        IF REQU-TIRFSDAT-KEY = LOW-VALUES                                 
058260           OR ZERO OR SPACE                                               
058261           MOVE LOW-VALUE            TO WS-DARFS-MIN-X                    
058262           MOVE HIGH-VALUE           TO WS-DARFS-MAX-X                    
058263        ELSE                                                              
058264           MOVE REQU-TIRFSDAT-KEY  TO RESP-TIRFSDAT-KEY                   
058265                                      DAT-I-TIDATUM                       
058266           MOVE 'AAMMDD'           TO DAT-KDDATFORM                       
058267           CALL WDATKONV              USING DAT-KDDATFORM                 
058268                                            DAT-I-TIDATUM                 
058269                                            DAT-O-TIDATUM                 
058270                                            DAT-KDSVAR                    
058271                                                                          
058272           IF DAT-KDSVAR = SPACE                                          
058273             MOVE DAT-TISEKEL      TO WS-DARFS-CC-MIN                     
058274                                      WS-DARFS-CC-MAX                     
058275             MOVE DAT-O-TIDATUM    TO WS-DARFS-YYMMDD-MIN                 
058276                                      WS-DARFS-YYMMDD-MAX                 
058277                                      W-TIRFS-MIN                         
058278                                      W-TIRFS-MAX                         
058279             MOVE ZERO             TO WS-DARFS-TTMM-MIN                   
058280             MOVE 9999             TO WS-DARFS-TTMM-MAX                   
058281             MOVE WS-DARFS-MIN     TO W-DARFS-MIN                         
058282             MOVE WS-DARFS-MAX     TO W-DARFS-MAX                         
058283           ELSE                                                           
058284             MOVE '023'            TO RESP-IDMSG-ERROR                    
058285*            IS INVALID ***                                               
058286             MOVE 'TIRFSDAT'       TO RESP-IDELMT-ERROR                   
058287             MOVE NOO              TO KEYS-SW                             
058288                                                                          
058289           END-IF                                                         
058290        END-IF                                                            
058291     END-IF                                                               
058292                                                                          
058293     IF REQU-KVRADER-MAX NUMERIC                                          
058294       IF REQU-KVRADER-MAX > MAX-INDX                                     
058295          MOVE MAX-INDX              TO RESP-KVRADER-MAX                  
058296       ELSE                                                               
058297          MOVE REQU-KVRADER-MAX      TO RESP-KVRADER-MAX                  
058298       END-IF                                                             
058299     ELSE                                                                 
058300       MOVE ZERO                     TO REQU-KVRADER-MAX                  
058301                                        RESP-KVRADER-MAX                  
058302     END-IF                                                               
058303                                                                          
058310     IF REQU-KDPGMACT = 'E'                                               
058311        IF KEYS-OK                                                        
058312           IF REQU-FLSIDLAST = "Y" OR "J" OR "N"                          
058313              MOVE REQU-FLSIDLAST  TO RESP-FLSIDLAST                      
058314           ELSE                                                           
058315              MOVE '281'           TO RESP-IDMSG-ERROR                    
058316              MOVE NOO             TO KEYS-SW                             
058317           END-IF                                                         
058318        END-IF                                                            
058319     END-IF                                                               
058320     .                                                                    
058321 C-KOLLA-INPUT SECTION.                                                   
058330     MOVE 'C-KOLLA-INPUT ' TO CURR-SECTION                                
058400                                                                          
058500     MOVE YES                  TO INDATA-SW                               
058600     MOVE +1                   TO INDX                                    
058700                                                                          
058800     PERFORM UNTIL (INDX > RESP-KVRADER-MAX) OR                           
058900            (REQU-KDCMD-RAD (INDX) NOT = LOW-VALUES AND                   
059000             REQU-KDCMD-RAD (INDX) NOT = SPACE)                           
059100                                                                          
059200        ADD +1                     TO INDX                                
059300                                                                          
059400     END-PERFORM                                                          
059500                                                                          
059600     IF (REQU-FLSIDLAST = NOO AND                                         
059700        INDX > RESP-KVRADER-MAX) OR                                       
059800        RESP-KVRADER-MAX = ZERO                                           
059900        MOVE '014'           TO RESP-IDMSG-ERROR                          
060000*       EXECUTE BUT NO DATA ***                                           
060100        MOVE NOO             TO INDATA-SW                                 
060200     ELSE                                                                 
060300        IF REQU-FLSIDLAST = 'J' OR 'Y'                                    
060400           IF REQU-IDKOLLI(1) = LOW-VALUES OR ZERO OR LOW-VALUE           
060500              MOVE '026'           TO RESP-IDMSG-ERROR                    
060600              MOVE '026'        TO RESP-IDMSG-ERROR-LINE(INDX)            
060700*             MUST BE ENTERED ***                                         
060800              MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                   
060900              MOVE NOO             TO INDATA-SW                           
061000           ELSE                                                           
061100              MOVE W-LASTA-HELA-SIDAN   TO W-FUNKTION                     
061200              MOVE +1                   TO INDX                           
061300              PERFORM UNTIL INDX > RESP-KVRADER-MAX OR                    
061400                            INDATA-FEL                                    
061500                 PERFORM CA-KOLLA-KOLLI                                   
061600                 MOVE W-VALD            TO REQU-KDCMD-RAD(INDX)           
061700                 ADD +1                 TO INDX                           
061800              END-PERFORM                                                 
061900           END-IF                                                         
062000        END-IF                                                            
062100                                                                          
062200       IF RESP-KVRADER-MAX NUMERIC AND                                    
062300          RESP-KVRADER-MAX > 0                                            
062400                                                                          
062500          MOVE +1                   TO  INDX                              
062600          PERFORM UNTIL INDX        >   RESP-KVRADER-MAX                  
062700             IF REQU-KDCMD-RAD(INDX) NOT = LOW-VALUES                     
062800                IF REQU-KDCMD-RAD(INDX) = W-VALD                          
062900                                                                          
063000                   MOVE W-LASTA-VALDA-RADER                               
063100                                     TO W-FUNKTION                        
063200                   PERFORM CA-KOLLA-KOLLI                                 
063300                ELSE                                                      
063400                   IF REQU-KDCMD-RAD(INDX) NOT = SPACE                    
063500                      MOVE '023'   TO RESP-IDMSG-ERROR                    
063600                                       RESP-IDMSG-ERROR-LINE(INDX)        
063700                      MOVE 'KDCMDVAL' TO RESP-IDELMT-ERROR                
063800                      MOVE NOO     TO INDATA-SW                           
063900                   END-IF                                                 
064000                END-IF                                                    
064100             ELSE                                                         
064200*              -- ALSO CHECK NON-CHECKED LINES FOR VALID DATA             
064300*              -- IN CASE WEB DATA HAS BEEN CORRUPTED                     
064400               IF REQU-IDDISTR(INDX) NOT NUMERIC                          
064500                  MOVE '023'     TO RESP-IDMSG-ERROR                      
064600                                    RESP-IDMSG-ERROR-LINE(INDX)           
064700                  MOVE 'IDDISTR' TO RESP-IDELMT-ERROR                     
064800                  MOVE NOO       TO INDATA-SW                             
064900               END-IF                                                     
065000               IF REQU-IDKOLLI(INDX) NOT NUMERIC                          
065100                  MOVE '023'     TO RESP-IDMSG-ERROR                      
065200                                    RESP-IDMSG-ERROR-LINE(INDX)           
065300                  MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                     
065400                  MOVE NOO       TO INDATA-SW                             
065500               END-IF                                                     
065600               IF REQU-IDPRODNR(INDX) NOT NUMERIC                         
065700                  MOVE '023'     TO RESP-IDMSG-ERROR                      
065800                                    RESP-IDMSG-ERROR-LINE(INDX)           
065900                  MOVE 'IDPRODNR' TO RESP-IDELMT-ERROR                    
066000                  MOVE NOO       TO INDATA-SW                             
066100               END-IF                                                     
066200             END-IF                                                       
066300             ADD +1                 TO INDX                               
066400          END-PERFORM                                                     
066500       ELSE                                                               
066600          MOVE NOO TO INDATA-SW                                           
066700          IF RESP-KVRADER-MAX NUMERIC AND                                 
066800             RESP-KVRADER-MAX = 0                                         
066900             MOVE 'KVRADER' TO RESP-IDELMT-ERROR                          
067000             MOVE '126'   TO RESP-IDMSG-ERROR                             
067100          ELSE                                                            
067200             MOVE 'KVRADER' TO RESP-IDELMT-ERROR                          
067300             MOVE '024'   TO RESP-IDMSG-ERROR                             
067400          END-IF                                                          
067500       END-IF                                                             
067600     END-IF                                                               
067700     .                                                                    
067800                                                                          
067900 CA-KOLLA-KOLLI   SECTION.                                                
068000     MOVE 'CA-KOLLA-KOLLI' TO CURR-SECTION                                
068100                                                                          
068200     IF REQU-IDDISTR(INDX) NUMERIC                                        
068300     AND REQU-IDKUNDNR(INDX) NUMERIC                                      
068400     AND REQU-IDORDNR7(INDX) NUMERIC                                      
068500     AND REQU-IDKOLLI(INDX) NUMERIC                                       
068600     AND REQU-IDPRODNR(INDX) NUMERIC                                      
068700                                                                          
068800        MOVE REQU-IDPRODNR(INDX) TO W-IDPRODNR                            
068900        MOVE REQU-IDKOLLI(INDX)  TO W-IDKOLLI                             
069000                                                                          
069100        PERFORM IMS-02-GU-WDE611                                          
069200        IF SEGMENT-FOUND                                                  
069300          IF KOLLI-KDKOLSTA =  KLI-PACK AND                               
069400             KOLLI-FLUTLAST =  YES                                        
069500              CONTINUE                                                    
069600          ELSE                                                            
069910            IF (KOLLI-KDKOLSTA = 6 OR                                     
069920                KOLLI-KDKOLSTA > 6) AND                                   
069921               (REQU-FLCROSS(INDX) NOT = YES OR                           
069922                KOLLI-IDDC = W-IDDC-4495)                                 
069923*FOR A CROSS DOCK CASE, THE CASE STATUS WILL BE 9 & IDDC IN WDE611        
069924*WILL BE SENDING DC.SUCH CASES WILL BE ALLOWED FOR LOADING.               
069925*IF THE CASE IS NOT A CROSS DOCK AND HAS BEEN LOADED,THIS                 
069926*CONDITION WILL BE TRUE IF THERE IS A RELOADING ATTEMPT.                  
069930              MOVE '400'         TO RESP-IDMSG-ERROR                      
069940              MOVE '400'         TO RESP-IDMSG-ERROR-LINE(INDX)           
069950              MOVE NOO           TO INDATA-SW                             
069960            ELSE                                                          
069970              IF REQU-FLCROSS(INDX) = YES                                 
069980                PERFORM IMS-GHNP-WDE621                                   
070000                IF SEGMENT-MISSING                                        
070010                  MOVE '149'     TO RESP-IDMSG-ERROR                      
070020                  MOVE '149'     TO RESP-IDMSG-ERROR-LINE(INDX)           
070030                  MOVE NOO       TO INDATA-SW                             
070040                END-IF                                                    
070050              END-IF                                                      
070060            END-IF                                                        
070070          END-IF                                                          
070100        ELSE                                                              
070200          MOVE '025'           TO RESP-IDMSG-ERROR                        
070300          MOVE '025'           TO RESP-IDMSG-ERROR-LINE(INDX)             
070400          MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                       
070500          MOVE NOO             TO INDATA-SW                               
070600        END-IF                                                            
070700     ELSE                                                                 
070800       MOVE '023'           TO RESP-IDMSG-ERROR                           
070900                               RESP-IDMSG-ERROR-LINE(INDX)                
071000       MOVE NOO             TO INDATA-SW                                  
071100       IF REQU-IDDISTR(INDX) NOT NUMERIC                                  
071200          MOVE 'IDDISTR'       TO RESP-IDELMT-ERROR                       
071300       END-IF                                                             
071400       IF REQU-IDKUNDNR(INDX) NOT NUMERIC                                 
071500          MOVE 'IDKUNDNR'      TO RESP-IDELMT-ERROR                       
071600       END-IF                                                             
071700       IF REQU-IDORDNR7(INDX) NOT NUMERIC                                 
071800          MOVE 'IDORDNR'       TO RESP-IDELMT-ERROR                       
071900       END-IF                                                             
072000       IF REQU-IDPRODNR(INDX) NOT NUMERIC                                 
072100          MOVE 'IDPRODNR'      TO RESP-IDELMT-ERROR                       
072200       END-IF                                                             
072300       IF REQU-IDKOLLI(INDX) NOT NUMERIC                                  
072400          MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                       
072500       END-IF                                                             
072600     END-IF                                                               
072700     .                                                                    
072800 D-UPPDATERA SECTION.                                                     
072900     MOVE 'D-UPPDATERA     ' TO CURR-SECTION                              
073000                                                                          
073100     MOVE ZERO           TO W-KVKOLLI                                     
073200                            W-VKORDBTO                                    
073300                            W-VLORDBTO                                    
073500                                                                          
073700                                                                          
073800     PERFORM IMS-05-GU-4495                                               
073900     IF SEGMENT-FOUND                                                     
074000       MOVE YES                   TO SW-TRPT                              
074100     ELSE                                                                 
074200       MOVE NOO                    TO SW-TRPT                             
074300       MOVE LOW-VALUE              TO 4495-LOW-VALUE                      
074400       MOVE '4495'                 TO 4495-IDHTYP                         
074500       MOVE REQU-IDDC-KEY          TO 4495-IDDC                           
074600       MOVE W-IDTRPTNR             TO 4495-IDTRPTNR                       
074700       MOVE W-IDLBBET              TO 4495-IDLBBET                        
074800                                                                          
074900       PERFORM IMS-06-ISRT-4495                                           
075000                                                                          
075100     END-IF                                                               
075200                                                                          
075300     EVALUATE TRUE                                                        
075400       WHEN LASTA-HELA-SIDAN                                              
075500         PERFORM DA-UPPDATERA-HELA-SIDAN                                  
075600       WHEN LASTA-VALDA-RADER                                             
075700         PERFORM DB-UPPDATERA-RADER                                       
075800     END-EVALUATE                                                         
075900                                                                          
076000     IF INDATA-OK                                                         
076100       MOVE '001'     TO RESP-IDMSG-INFO                                  
076200     END-IF                                                               
076300     .                                                                    
076400                                                                          
076500 DA-UPPDATERA-HELA-SIDAN SECTION.                                         
076600     MOVE 'DA-UPPDATERA-HELA' TO CURR-SECTION                             
076700                                                                          
076800     MOVE +1                          TO INDX                             
076900     PERFORM UNTIL INDX > RESP-KVRADER-MAX                                
077000       IF REQU-IDKOLLI(INDX)          NUMERIC AND                         
077100          REQU-KDCMD-RAD(INDX)        =  W-VALD                           
077200         IF REQU-IDDISTR(INDX) NUMERIC                                    
077300            MOVE REQU-IDPRODNR(INDX)  TO W-IDPRODNR                       
077400            MOVE REQU-IDKOLLI(INDX)   TO W-IDKOLLI                        
077500                                                                          
077600            MOVE REQU-IDDISTR(INDX)   TO 4498-IDDISTR                     
077700                                         TEST-IDDISTR                     
077800            MOVE REQU-IDKUNDNR(INDX)  TO 4498-IDKUNDNR                    
077900                                         4498-IDDEALER                    
078000            MOVE SPACE                TO 4498-IDKUNDRF                    
078010                                         4498-FLCROSS                     
078100            MOVE REQU-IDORDNR7(INDX)  TO 4498-IDORDNR7                    
078200            MOVE REQU-IDPRODNR(INDX)  TO 4498-IDPRODNR                    
078300                                                                          
078400            IF REQU-FLCROSS(INDX) = YES                                   
078500              MOVE REQU-FLCROSS(INDX) TO 4498-FLCROSS                     
078600            ELSE                                                          
078700              MOVE NOO                TO 4498-FLCROSS                     
078800            END-IF                                                        
078900                                                                          
079000            PERFORM S13-UPPDATERA-WDE601                                  
079100                                                                          
079200            PERFORM IMS-07-GHNP-WDE611                                    
079300            PERFORM S30-UPPDATERA-ARBREG-RAD                              
079400            ADD 1                     TO W-KVKOLLI                        
079500            ADD KOLLI-VKORDBTO-KOLLI  TO W-VKORDBTO                       
079510            ADD KOLLI-VLORDBTO-KOLLI  TO W-VLORDBTO                       
079520            IF REQU-FLCROSS(INDX) NOT = YES                               
079530              PERFORM S10-UPPDATERA-WDE611                                
079540            ELSE                                                          
079550              PERFORM S11-UPPDATERA-WDE621                                
079560            END-IF                                                        
079570         ELSE                                                             
079580            PERFORM S40-UPPDATERA-SAMKOLLI                                
079590         END-IF                                                           
079591       END-IF                                                             
079592       ADD +1                         TO INDX                             
079593     END-PERFORM                                                          
079594     PERFORM S50-UPPDATERA-ARBREG-TOT                                     
079600     .                                                                    
079700     EJECT                                                                
079800 DB-UPPDATERA-RADER SECTION.                                              
079900     MOVE 'DB-UPPDATERA-RADER' TO CURR-SECTION                            
080000                                                                          
080100     MOVE +1                 TO INDX                                      
080200     PERFORM UNTIL INDX > RESP-KVRADER-MAX                                
080300       IF REQU-KDCMD-RAD(INDX) = W-VALD                                   
080400         IF REQU-IDDISTR(INDX)  NUMERIC                                   
080500            MOVE REQU-IDPRODNR(INDX)  TO W-IDPRODNR                       
080600            MOVE REQU-IDKOLLI(INDX)   TO W-IDKOLLI                        
080700                                                                          
080800            MOVE REQU-IDDISTR(INDX)   TO 4498-IDDISTR                     
080900                                         TEST-IDDISTR                     
081000            MOVE REQU-IDKUNDNR(INDX)  TO 4498-IDKUNDNR                    
081100                                         4498-IDDEALER                    
081200            MOVE SPACE                TO 4498-IDKUNDRF                    
081300            MOVE REQU-IDORDNR7(INDX)  TO 4498-IDORDNR7                    
081400            MOVE REQU-IDPRODNR(INDX)  TO 4498-IDPRODNR                    
081500            IF REQU-FLCROSS(INDX) = YES                                   
081600              MOVE REQU-FLCROSS(INDX) TO 4498-FLCROSS                     
081700            ELSE                                                          
081800              MOVE NOO                TO 4498-FLCROSS                     
081900            END-IF                                                        
082000                                                                          
082100            PERFORM S13-UPPDATERA-WDE601                                  
082200                                                                          
082300            PERFORM IMS-07-GHNP-WDE611                                    
082400            PERFORM S30-UPPDATERA-ARBREG-RAD                              
082500            ADD 1                     TO W-KVKOLLI                        
082600            ADD KOLLI-VKORDBTO-KOLLI  TO W-VKORDBTO                       
082700            ADD KOLLI-VLORDBTO-KOLLI  TO W-VLORDBTO                       
082710            IF REQU-FLCROSS(INDX) NOT = YES                               
082720              PERFORM S10-UPPDATERA-WDE611                                
082730            ELSE                                                          
082740              PERFORM S11-UPPDATERA-WDE621                                
082750            END-IF                                                        
082760         ELSE                                                             
082770            PERFORM S40-UPPDATERA-SAMKOLLI                                
082780         END-IF                                                           
082790       END-IF                                                             
082791       ADD +1                TO INDX                                      
082792     END-PERFORM                                                          
082793     PERFORM S50-UPPDATERA-ARBREG-TOT                                     
082800     .                                                                    
082810     EJECT                                                                
082900                                                                          
083000 E-LAES-VISA-INFO SECTION.                                                
083100     MOVE 'E-LAES-VISA-INFO    ' TO CURR-SECTION                          
083200                                                                          
083300     MOVE ZERO                   TO W-KVKOLLI-SIDA                        
083400                                    W-VKORDBTO-SIDA                       
083500                                    W-VLORDBTO-SIDA                       
083600                                    W-KVKOLLI-TOT                         
083700                                    W-VKORDBTO-TOT                        
083800                                    W-VLORDBTO-TOT                        
083900                                    WS-KVRADER                            
084000                                                                          
084100     MOVE REQU-IDDC-KEY          TO W-IDDC-MIN                            
084200                                    W-IDDC-MAX                            
084300                                                                          
084400     MOVE REQU-IDTRPTNR-KEY      TO W-IDTRPTNR-MIN                        
084500                                    W-IDTRPTNR-MAX                        
084600*                                                                         
084700     MOVE WS-DARFS-MIN           TO W-DARFS-MIN                           
084800     MOVE WS-DARFS-MAX           TO W-DARFS-MAX                           
084900     PERFORM EB-LAES-LASTAT-HITTILLS                                      
085000                                                                          
085100     MOVE +1                     TO INDX                                  
085200                                                                          
085300     PERFORM IMS-18-GU-WDE6C                                              
085400     IF SEGMENT-MISSING                                                   
085500       IF RESP-IDMSG-INFO = '001'                                         
085600* UPPDATERING OK MEN NU FINNS INGET KVAR.                                 
085700          CONTINUE                                                        
085800*      ELSE FLYTTAR TILL EA- ...                                          
085900*        MOVE '287'                  TO RESP-IDMSG-ERROR                  
086000       END-IF                                                             
086100     ELSE                                                                 
086200       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-MISSING                   
086300                                     OR END-OF-DB                         
086400         IF ((REQU-KDFARLIG-KEY = 'Y' AND SEQC-IDPSN(1) > ZERO) OR        
086500             (REQU-KDFARLIG-KEY = 'N' AND SEQC-IDPSN(1) = ZERO) OR        
086600             (REQU-KDFARLIG-KEY = 'M'))   AND                             
086700              SEQC-KDKOLSTA     = KLI-PACK                                
086800                                                                          
086900            ADD +1                   TO WS-KVRADER                        
087000                                        W-KVKOLLI-SIDA                    
087100                                        W-KVKOLLI-TOT                     
087200            ADD SEQC-VKORDBTO-KOLLI  TO W-VKORDBTO-SIDA                   
087300                                        W-VKORDBTO-TOT                    
087400            ADD SEQC-VLORDBTO-KOLLI  TO W-VLORDBTO-SIDA                   
087500                                        W-VLORDBTO-TOT                    
087600                                                                          
087700            MOVE SEQC-IDDISTR        TO RESP-IDDISTR  (INDX)              
087800            MOVE SEQC-IDKUNDNR       TO RESP-IDKUNDNR (INDX)              
087900            MOVE SEQC-IDKOLLI        TO RESP-IDKOLLI  (INDX)              
088000*           IF SEQC-DARFS (9:4) = ZERO                                    
088100*            MOVE SEQC-DARFS (3:6)   TO RESP-TIRFS (INDX)                 
088200*           ELSE                                                          
088300             MOVE SEQC-DARFS (3:10)  TO RESP-TIRFS (INDX)                 
088400*           END-IF                                                        
088500            MOVE SEQC-KDKOLLI        TO RESP-KDKOLLI (INDX)               
088600            MOVE SEQC-VKORDBTO-KOLLI TO RESP-VKORDBTO (INDX)              
088700            MOVE SEQC-VLORDBTO-KOLLI TO RESP-VLORDBTO (INDX)              
088800                                                                          
088900            IF REQU-KDMATT = 'U'                                          
089000              COMPUTE WS-VKORDBTO              =                          
089100                      SEQC-VKORDBTO-KOLLI * CONV-KG-TO-LB                 
089200              COMPUTE WS-VLORDBTO              =                          
089300                      SEQC-VLORDBTO-KOLLI * CONV-M3-TO-YD3                
089400              MOVE WS-VKORDBTO       TO RESP-VKORDBTO (INDX)              
089500              MOVE WS-VLORDBTO       TO RESP-VLORDBTO (INDX)              
089600            END-IF                                                        
089700                                                                          
089800            IF SEQC-IDPSN(1)        >  ZERO                               
089900               MOVE SEQC-IDPSN(1) TO W-IDPSN-NUM                          
090000               IF SEQC-IDPSN(2) > ZERO                                    
090100                  MOVE '*'           TO RESP-IDPSN(INDX) (1:1)            
090200                  MOVE W-IDPSN-NUM   TO RESP-IDPSN(INDX) (2:3)            
090300               ELSE                                                       
090400                  MOVE ' '           TO RESP-IDPSN(INDX) (1:1)            
090500                  MOVE W-IDPSN-NUM   TO RESP-IDPSN(INDX) (2:3)            
090600               END-IF                                                     
090700            ELSE                                                          
090800               MOVE SPACE            TO RESP-IDPSN(INDX)                  
090900            END-IF                                                        
091000                                                                          
091100            MOVE SEQC-IDPRODNR       TO RESP-IDPRODNR(INDX)               
091200            MOVE SPACE               TO RESP-FLCROSS (INDX)               
091300                                                                          
091400            MOVE SEQC-IDPRODNR       TO W-E4F-IDPRODNR-MIN                
091500                                       W-E4F-IDPRODNR-MAX                 
091600            MOVE SEQC-IDKOLLI        TO W-E4F-IDKOLLI-MIN                 
091700                                       W-E4F-IDKOLLI-MAX                  
091800            PERFORM IMS-22-GU-WDE4F                                       
091900            MOVE SEQF-IDORDNR5       TO RESP-IDORDNR7 (INDX)              
092000            ADD 1 TO INDX                                                 
092100         END-IF                                                           
092200                                                                          
092300         PERFORM IMS-19-GN-WDE6C                                          
092400       END-PERFORM                                                        
092500*** IF LOOP ENDED DUE TO RICHED MAX-INDX CONTINUE                         
092600*** READ ALL REMAINING WDE6C TO COUNT TOTALS                              
092700       PERFORM UNTIL SEGMENT-MISSING OR END-OF-DB                         
092800         IF ((REQU-KDFARLIG-KEY = 'Y' AND SEQC-IDPSN(1) > ZERO) OR        
092900             (REQU-KDFARLIG-KEY = 'N' AND SEQC-IDPSN(1) = ZERO) OR        
093000             (REQU-KDFARLIG-KEY = 'M'))   AND                             
093100              SEQC-KDKOLSTA     = KLI-PACK                                
093200                                                                          
093300            ADD 1                   TO W-KVKOLLI-TOT                      
093400            ADD SEQC-VKORDBTO-KOLLI TO W-VKORDBTO-TOT                     
093500            ADD SEQC-VLORDBTO-KOLLI TO W-VLORDBTO-TOT                     
093600         END-IF                                                           
093700         PERFORM IMS-19-GN-WDE6C                                          
093800       END-PERFORM                                                        
093900       MOVE W-KVKOLLI-TOT           TO RESP-KVKOLLI-TOT                   
094000       MOVE W-VKORDBTO-TOT          TO RESP-VKORDBTO-TOT                  
094100       MOVE W-VLORDBTO-TOT          TO RESP-VLORDBTO-TOT                  
094200       IF REQU-KDMATT = 'U'                                               
094300*** ÄNDRA HÄR TILL WS-VKORDBTO-TOT OM FÄLTSTORLEK ÄNDRAS                  
094400         COMPUTE WS-VKORDBTO-TOT =                                        
094500                 W-VKORDBTO-TOT  * CONV-KG-TO-LB                          
094600         COMPUTE WS-VLORDBTO-TOT =                                        
094700                 W-VLORDBTO-TOT  * CONV-M3-TO-YD3                         
094800         MOVE WS-VKORDBTO-TOT       TO RESP-VKORDBTO-TOT                  
094900         MOVE WS-VLORDBTO-TOT       TO RESP-VLORDBTO-TOT                  
095000       END-IF                                                             
095100     END-IF                                                               
095200                                                                          
095300*CO-CD                                                                    
095400     IF INDX NOT > MAX-INDX                                               
095500       PERFORM EA-VISA-CD-KOLLIN                                          
095600     END-IF                                                               
095700*CO-CD                                                                    
095800                                                                          
095900     MOVE W-KVKOLLI-SIDA            TO RESP-KVKOLLI-VALD                  
096000     MOVE WS-KVRADER                TO RESP-KVRADER-MAX                   
096100     .                                                                    
096200                                                                          
096300*CO-CD                                                                    
096400 EA-VISA-CD-KOLLIN   SECTION.                                             
096500                                                                          
096600     PERFORM IMS-GU-WDE6H                                                 
096700                                                                          
096710     IF RESP-IDMSG-INFO NOT = '001'                                       
096800       IF SEGMENT-MISSING AND INDX = 1                                    
096900         MOVE '287'                  TO RESP-IDMSG-ERROR                  
097000       END-IF                                                             
097010     END-IF                                                               
097100* READS ONLY SEGM STA=1, RECXDAT>0 AND TRP-CROSS>0                        
097200     PERFORM UNTIL INDX > MAX-INDX OR                                     
097300                   SEGMENT-MISSING OR END-OF-DB                           
097400       MOVE SEQH-IDPRODNR          TO W-IDPRODNR                          
097500       MOVE SEQH-IDKOLLI           TO W-IDKOLLI                           
097600       PERFORM IMS-02-GU-WDE611                                           
097700                                                                          
097800       IF ((REQU-KDFARLIG-KEY = 'Y' AND KOLLI-IDPSN(1) > ZERO) OR         
097900           (REQU-KDFARLIG-KEY = 'N' AND KOLLI-IDPSN(1) = ZERO) OR         
098000           (REQU-KDFARLIG-KEY = 'M'))                                     
098100                                                                          
098200          ADD 1                    TO WS-KVRADER                          
098300                                      W-KVKOLLI-SIDA                      
098400                                      W-KVKOLLI-TOT                       
098500          ADD KOLLI-VKORDBTO-KOLLI TO W-VKORDBTO-SIDA                     
098600                                      W-VKORDBTO-TOT                      
098700          ADD KOLLI-VLORDBTO-KOLLI TO W-VLORDBTO-SIDA                     
098800                                      W-VLORDBTO-TOT                      
098900          MOVE SEQH-IDDISTR        TO RESP-IDDISTR  (INDX)                
099000          MOVE SEQH-IDKUNDNR       TO RESP-IDKUNDNR (INDX)                
099100          MOVE SEQH-IDKOLLI        TO RESP-IDKOLLI  (INDX)                
099200*         MOVE ZERO                TO RESP-TIRFS (INDX)                   
099300          MOVE SEQH-TIRFSDAT       TO RESP-TIRFS (INDX) (1:6)             
099400          MOVE KOLLI-KDKOLLI       TO RESP-KDKOLLI (INDX)                 
099500          MOVE KOLLI-VKORDBTO-KOLLI TO RESP-VKORDBTO (INDX)               
099600          MOVE KOLLI-VLORDBTO-KOLLI TO RESP-VLORDBTO (INDX)               
099700                                                                          
099800          IF REQU-KDMATT = 'U'                                            
099900            COMPUTE WS-VKORDBTO              =                            
100000                    KOLLI-VKORDBTO-KOLLI * CONV-KG-TO-LB                  
100100            COMPUTE WS-VLORDBTO              =                            
100200                    KOLLI-VLORDBTO-KOLLI * CONV-M3-TO-YD3                 
100300            MOVE WS-VKORDBTO         TO RESP-VKORDBTO (INDX)              
100400            MOVE WS-VLORDBTO         TO RESP-VLORDBTO (INDX)              
100500          END-IF                                                          
100600                                                                          
100700          IF KOLLI-IDPSN(1)       >  ZERO                                 
100800             MOVE KOLLI-IDPSN(1) TO W-IDPSN-NUM                           
100900             IF KOLLI-IDPSN(2) > ZERO                                     
101000                MOVE '*'          TO RESP-IDPSN(INDX) (1:1)               
101100                MOVE W-IDPSN-NUM  TO RESP-IDPSN(INDX) (2:3)               
101200             ELSE                                                         
101300                MOVE ' '          TO RESP-IDPSN(INDX) (1:1)               
101400                MOVE W-IDPSN-NUM  TO RESP-IDPSN(INDX) (2:3)               
101500             END-IF                                                       
101600          ELSE                                                            
101700             MOVE SPACE           TO RESP-IDPSN(INDX)                     
101800          END-IF                                                          
101900                                                                          
102000          MOVE SEQH-IDPRODNR      TO RESP-IDPRODNR(INDX)                  
102100          MOVE YES                TO RESP-FLCROSS (INDX)                  
102200                                                                          
102300          MOVE SEQH-IDPRODNR      TO W-E4F-IDPRODNR-MIN                   
102400                                     W-E4F-IDPRODNR-MAX                   
102500          MOVE SEQH-IDKOLLI       TO W-E4F-IDKOLLI-MIN                    
102600                                     W-E4F-IDKOLLI-MAX                    
102700          PERFORM IMS-22-GU-WDE4F                                         
102800          MOVE SEQF-IDORDNR5      TO RESP-IDORDNR7 (INDX)                 
102900          ADD 1 TO INDX                                                   
103000       END-IF                                                             
103100                                                                          
103200       PERFORM IMS-GN-WDE6H                                               
103300     END-PERFORM                                                          
103400*** IF LOOP ENDED DUE TO RICHED MAX-INDX CONTINUE                         
103500*** READ ALL REMAINING WDE6H TO COUNT TOTALS                              
103600       PERFORM UNTIL SEGMENT-MISSING OR END-OF-DB                         
103700        MOVE SEQH-IDPRODNR      TO W-IDPRODNR                             
103800        MOVE SEQH-IDKOLLI       TO W-IDKOLLI                              
103900        PERFORM IMS-02-GU-WDE611                                          
104000        IF ((REQU-KDFARLIG-KEY = 'Y' AND KOLLI-IDPSN(1) > ZERO) OR        
104100            (REQU-KDFARLIG-KEY = 'N' AND KOLLI-IDPSN(1) = ZERO) OR        
104200            (REQU-KDFARLIG-KEY = 'M'))                                    
104300            ADD 1                    TO W-KVKOLLI-TOT                     
104400            ADD KOLLI-VKORDBTO-KOLLI TO W-VKORDBTO-TOT                    
104500            ADD KOLLI-VLORDBTO-KOLLI TO W-VLORDBTO-TOT                    
104600        END-IF                                                            
104700        PERFORM IMS-GN-WDE6H                                              
104800       END-PERFORM                                                        
104900                                                                          
105000     MOVE W-KVKOLLI-TOT           TO RESP-KVKOLLI-TOT                     
105100     MOVE W-VKORDBTO-TOT          TO RESP-VKORDBTO-TOT                    
105200     MOVE W-VLORDBTO-TOT          TO RESP-VLORDBTO-TOT                    
105300                                                                          
105400     IF REQU-KDMATT = 'U'                                                 
105500*** ÄNDRA HÄR TILL WS-VKORDBTO-TOT OM FÄLTSTORLEK ÄNDRAS                  
105600       COMPUTE WS-VKORDBTO-TOT =                                          
105700               W-VKORDBTO-TOT  *                                          
105800               CONV-KG-TO-LB                                              
105900       COMPUTE WS-VLORDBTO-TOT =                                          
106000               W-VLORDBTO-TOT  *                                          
106100               CONV-M3-TO-YD3                                             
106200       MOVE WS-VKORDBTO-TOT       TO RESP-VKORDBTO-TOT                    
106300       MOVE WS-VLORDBTO-TOT       TO RESP-VLORDBTO-TOT                    
106400     END-IF                                                               
106500     .                                                                    
106600 EB-LAES-LASTAT-HITTILLS SECTION.                                         
106700     MOVE 'EB-LAES-LASTAT      '  TO CURR-SECTION                         
106800                                                                          
106900     PERFORM IMS-15-GHU-WDGX4496                                          
107000     IF SEGMENT-FOUND                                                     
107100       MOVE 4496-KVKOLLI-LAST     TO RESP-KVKOLLI-VALD                    
107200                                                                          
107300       IF REQU-KDMATT = 'U'                                               
107310         COMPUTE WS-VKORDBTO         =                                    
107320                 4496-VKORDBTO-LASTB *                                    
107330                 CONV-KG-TO-LB                                            
107340         COMPUTE WS-VLORDBTO         =                                    
107350                 4496-VLORDBTO-LASTB *                                    
107360                 CONV-M3-TO-YD3                                           
107370         MOVE WS-VKORDBTO         TO RESP-VKORDBTO-VALD                   
107380         MOVE WS-VLORDBTO         TO RESP-VLORDBTO-VALD                   
107390                                                                          
107391       ELSE                                                               
107392         MOVE 4496-VLORDBTO-LASTB TO RESP-VLORDBTO-VALD                   
107393         MOVE 4496-VKORDBTO-LASTB TO RESP-VKORDBTO-VALD                   
107394       END-IF                                                             
107395                                                                          
107396     ELSE                                                                 
107397       MOVE ZERO                  TO RESP-KVKOLLI-VALD                    
107398                                     RESP-VLORDBTO-VALD                   
107399                                     RESP-VKORDBTO-VALD                   
107400     END-IF                                                               
107401     .                                                                    
107402                                                                          
107410                                                                          
107500                                                                          
107600*    --- DISPATCHER SECTIONS                                              
107700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
107800                                                                          
107900     MOVE 'GETARG'               TO SUB-KDFUNC                            
108000     MOVE 'CARPARTS.LDC.LOADRELEASETRANSPORT' TO SUB-ADDISPABS            
108100                                                                          
108200*    MOVE MAX-INDX(500) TO REQU-KVRADER-MAX SO THAT THE                   
108300*    LENGTH IS CALCULATED CORRECTLY TO BE ABLE TO FETCH ALL               
108400*    POSSIBLE INPUT                                                       
108500     MOVE MAX-INDX               TO REQU-KVRADER-MAX                      
108600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
108700                                                                          
108800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
108900                                                                          
109000     IF SUB-KDRC > 0                                                      
109100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
109200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
109300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
109400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
109500     END-IF                                                               
109600     .                                                                    
109700                                                                          
109800 S02-RETURN-RESPONSE SECTION.                                             
109900                                                                          
110000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
110100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
110200                                                                          
110300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
110400                                                                          
110500     IF SUB-KDRC > 0                                                      
110600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
110700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
110800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
110900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
111000     END-IF                                                               
111100     .                                                                    
111200                                                                          
111300 S09-FIXA-LOKALTID SECTION.                                               
111400                                                                          
111500     MOVE REQU-IDDC-KEY     TO W-IDDC-B6                                  
111600     PERFORM IMS-23-GU-WDB601                                             
111700                                                                          
111800     MOVE '011'             TO MSGI-KDCALL                                
111900     MOVE DCS-IDTIDZON      TO MSGI-IDTIDZON                              
112000     MOVE DCS-IDDC          TO MSGI-IDDC                                  
112100     MOVE WS-CURRENT-YYMMDD TO MSGI-TILOKDAT                              
112200     MOVE WS-CURRENT-HHMM   TO MSGI-TILOKTID                              
112300                                                                          
112400     CALL WL01TIDZ   USING      MSGI-WL01TIDZ                             
112500     IF MSGI-KDSVAR = 'F'                                                 
112600        MOVE 'FEL RETURKOD FRÅN WL01TIDZ' TO ERROR-TEXT                   
112700        CALL FELLOG                                                       
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 S10-UPPDATERA-WDE611  SECTION.                                           
113200     MOVE 'S10-UPPDATERA-WDE6  ' TO CURR-SECTION                          
113300                                                                          
113400     MOVE NOO                     TO KOLLI-FLUTLAST                       
113500     MOVE W-IDLBBET               TO KOLLI-IDLBBET                        
113600     MOVE KLI-PACK-FAKT           TO KOLLI-KDKOLSTA                       
113700                                                                          
113800     PERFORM S09-FIXA-LOKALTID                                            
113900                                                                          
114000     MOVE MSGI-TILOKDAT           TO KOLLI-TILASTN                        
114100                                                                          
114200     MOVE MSGI-TILOKTID           TO WS-TILASTID-HHMM                     
114300     MOVE FUNCTION CURRENT-DATE (13:2) TO                                 
114400                                  WS-TILASTID-SS                          
114500     MOVE WS-TILASTID             TO KOLLI-TILASTID                       
114600                                                                          
114700     PERFORM IMS-08-REPL-WDE611                                           
114800     .                                                                    
114900                                                                          
115000 S11-UPPDATERA-WDE621  SECTION.                                           
115100     MOVE 'S11-UPPDATERA-WDE621'  TO CURR-SECTION                         
115200                                                                          
115300     PERFORM IMS-GHNP-WDE621                                              
115400     IF SEGMENT-FOUND                                                     
115500       MOVE 6                     TO CROSS-KDKOLSTA-CROSS                 
115600       PERFORM IMS-REPL-WDE621                                            
115700     END-IF                                                               
115800     .                                                                    
115900                                                                          
120300 S12-MSG-CONV SECTION.                                                    
120400     MOVE LOW-VALUES              TO RESP-MESSAGES (1)                    
120500                                     RESP-MESSAGES (2)                    
120600     MOVE 1                       TO MSG-IX                               
120700*    REQUEST OK                                                           
120800     MOVE 200                     TO RESP-KDSTATUS-API                    
120900     IF RESP-IDMSG-INFO > SPACE                                           
121000       MOVE SPACES                TO MSG-CONV-AREA                        
121100       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
121200       CALL WMSGCONV           USING MSG-CONV-AREA                        
121300       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
121400       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
121500       ADD 1                      TO MSG-IX                               
121600     END-IF                                                               
121700     IF RESP-IDMSG-ERROR > SPACE                                          
121800*      BAD REQUEST                                                        
121900       MOVE 400                   TO RESP-KDSTATUS-API                    
122000       MOVE SPACES                TO MSG-CONV-AREA                        
122100       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
122200       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
122300       CALL WMSGCONV           USING MSG-CONV-AREA                        
122400       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
122500       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
122600     END-IF                                                               
122700     .                                                                    
122710 S13-UPPDATERA-WDE601  SECTION.                                           
122720     MOVE 'S13-UPPDATERA-WDE601 ' TO  CURR-SECTION                        
122730                                                                          
122740     PERFORM IMS-09-GHU-WDE601                                            
122750     IF 4498-FLCROSS = NOO                                                
122760       ADD +1                     TO VORD-KVKOLLI-FL                      
122770       IF DIST79-DEALER-PRICE                                             
122780          ADD KOLLI-SUORDV-LOC    TO VORD-SUORDV-FL-LOC                   
122790          ADD KOLLI-SUORDV-LOCPREL TO VORD-SUORDV-FL-LOCPREL              
122791       ELSE                                                               
122792          IF KOLLI-SUORDV-KLI-EXP > ZERO                                  
122793            ADD KOLLI-SUORDV-KLI-EXP TO VORD-SUORDV-FL                    
122794          ELSE                                                            
122795            ADD KOLLI-SUORDV-KOLLI   TO VORD-SUORDV-FL                    
122796          END-IF                                                          
122797       END-IF                                                             
122798       ADD KOLLI-VKORDBTO-KOLLI   TO VORD-VKORDBTO-FL                     
122799       ADD KOLLI-VLORDBTO-KOLLI   TO VORD-VLORDBTO-FL                     
122800                                                                          
122801       PERFORM IMS-10-REPL-WDE601                                         
122802     END-IF                                                               
122803     .                                                                    
122810 S30-UPPDATERA-ARBREG-RAD  SECTION.                                       
122900     MOVE 'S30-UPPDATERA-ARBREG-RAD' TO  CURR-SECTION                     
123000                                                                          
123100     MOVE VORD-KDFAKTYP            TO 4498-KDFAKTYP                       
123200     MOVE KOLLI-IDKOLLI            TO 4498-IDKOLLI                        
123300     MOVE KOLLI-IDKOLLI-SAMP       TO 4498-IDKOLLI-SAMP                   
123400     MOVE KOLLI-IDPSN(1)           TO 4498-IDPSN(1)                       
123500     MOVE KOLLI-IDPSN(2)           TO 4498-IDPSN(2)                       
123600     MOVE KOLLI-KDKOLLI            TO 4498-KDKOLLI                        
123700     MOVE KOLLI-DARFS (3:10)       TO 4498-TIRFS                          
123800     MOVE KOLLI-VLORDBTO-KOLLI     TO 4498-VLORDBTO                       
123900     MOVE KOLLI-VKORDBTO-KOLLI     TO 4498-VKORDBTO                       
124000                                                                          
124100     IF KOLLI-IDKUNDNR NOT = 4498-IDKUNDNR                                
124200       MOVE 'KUND UNDER FEL ROT'  TO ERROR-TEXT                           
124300       CALL FELLOG                                                        
124400     END-IF                                                               
124500                                                                          
125700     PERFORM IMS-12-ISRT-WDGX4498                                         
125800     .                                                                    
125900                                                                          
126000 S40-UPPDATERA-SAMKOLLI    SECTION.                                       
126100     MOVE 'S40-UPPDATERA-SAMKOLLI'   TO  CURR-SECTION                     
126200                                                                          
126300     MOVE LOW-VALUE            TO W-WDE6E1KY-MIN-X                        
126400     MOVE HIGH-VALUE           TO W-WDE6E1KY-MAX-X                        
126500                                                                          
126600     MOVE REQU-IDKOLLI(INDX)   TO W-E6E-IDKOLLIS-MIN                      
126700                                  W-E6E-IDKOLLIS-MAX                      
126800     MOVE REQU-IDDC-KEY        TO W-E6E-IDDC-MIN                          
126900                                  W-E6E-IDDC-MAX                          
127000                                                                          
127100     PERFORM IMS-13-GU-WDE6E                                              
127200     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DB                           
127300**      PERFORM IMS-GU-WDE6H                                              
127400**      IF SEGMENT-MISSING                                                
127500** CONTINUE HOW TO HANDLE MIX CASES                                       
127600        MOVE SEQE-IDPRODNR         TO W-IDPRODNR                          
127700        MOVE SEQE-IDKOLLI          TO W-IDKOLLI                           
127800                                                                          
127900        PERFORM S13-UPPDATERA-WDE601                                      
128000        PERFORM IMS-07-GHNP-WDE611                                        
128100        ADD 1                      TO W-KVKOLLI                           
128200        ADD KOLLI-VKORDBTO-KOLLI   TO W-VKORDBTO                          
128300        ADD KOLLI-VLORDBTO-KOLLI   TO W-VLORDBTO                          
128400                                                                          
128500        PERFORM IMS-GHNP-WDE621                                           
128600        IF SEGMENT-MISSING                                                
128700          PERFORM S10-UPPDATERA-WDE611                                    
128800        ELSE                                                              
128900          MOVE 6                   TO CROSS-KDKOLSTA-CROSS                
129000          PERFORM IMS-REPL-WDE621                                         
129100        END-IF                                                            
129200                                                                          
129300        MOVE KOLLI-IDDISTR         TO 4498-IDDISTR                        
129400                                      TEST-IDDISTR                        
129410        MOVE KOLLI-IDKUNDNR        TO 4498-IDKUNDNR                       
129420                                      4498-IDDEALER                       
129430                                                                          
129440        MOVE SPACE                 TO 4498-IDKUNDRF                       
129450        MOVE SEQE-IDPRODNR         TO 4498-IDPRODNR                       
129460*CO-CD                                                                    
129470* HÄR MÅSTE VI TA REDA PÅ OM INGÅENDE KOLLIT I SAM.KOLLIT ÄR CROSS        
129480*       MOVE CROSS-FLCROSS         TO 4498-FLCROSS                        
129490*CO-CD                                                                    
129491        PERFORM S30-UPPDATERA-ARBREG-RAD                                  
129492        PERFORM IMS-14-GN-WDE6E                                           
129493     END-PERFORM                                                          
129500     .                                                                    
129600 S50-UPPDATERA-ARBREG-TOT  SECTION.                                       
129700     MOVE 'S50-UPPDATERA-ARBREG  '   TO  CURR-SECTION                     
129800                                                                          
129900     PERFORM IMS-15-GHU-WDGX4496                                          
130000                                                                          
130100     IF SEGMENT-FOUND                                                     
130200        ADD W-KVKOLLI        TO 4496-KVKOLLI-LAST                         
130300        ADD W-VKORDBTO       TO 4496-VKORDBTO-LASTB                       
130400        ADD W-VLORDBTO       TO 4496-VLORDBTO-LASTB                       
130500                                                                          
130600        PERFORM IMS-16-REPL-WDGX4496                                      
130700     ELSE                                                                 
130800        MOVE '1'             TO 4496-KDSEGKEY                             
130900        MOVE W-KVKOLLI       TO 4496-KVKOLLI-LAST                         
131000        MOVE W-VKORDBTO      TO 4496-VKORDBTO-LASTB                       
131100        MOVE W-VLORDBTO      TO 4496-VLORDBTO-LASTB                       
131200        MOVE ZERO            TO 4496-SUORDV-LASTB                         
131300                                                                          
131400        PERFORM IMS-17-ISRT-WDGX4496                                      
131500     END-IF                                                               
131600     .                                                                    
131700                                                                          
131800 IMS-02-GU-WDE611 SECTION.                                                
131900     MOVE 'IMS-02' TO CURR-IMS-SECTION                                    
132000                                                                          
132100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
132200            DELIMITED BY SIZE INTO SSA1                                   
132300     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
132400            DELIMITED BY SIZE INTO SSA2                                   
132500     MOVE '  GE'                TO GOOD-STATUSCODES                       
132600     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
132700     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
132800     PERFORM IMS-STATUS-CHECK                                             
132900     .                                                                    
133000 IMS-03-GU-WDE401-ASEK       SECTION.                                     
133100     MOVE 'IMS-03' TO CURR-IMS-SECTION                                    
133200                                                                          
133300     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
133400     DELIMITED BY SIZE   INTO SSA1                                        
133500     MOVE '  GE'           TO GOOD-STATUSCODES                            
133600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
133700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
133800     PERFORM IMS-STATUS-CHECK                                             
133900     .                                                                    
134000 IMS-04-GN-WDE401-ASEK       SECTION.                                     
134100     MOVE 'IMS-04' TO CURR-IMS-SECTION                                    
134200                                                                          
134300     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
134400     DELIMITED BY SIZE   INTO SSA1                                        
134500     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
134600     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
134700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
134800     PERFORM IMS-STATUS-CHECK                                             
134900     .                                                                    
135000 IMS-05-GU-4495  SECTION.                                                 
135100     MOVE 'IMS-05' TO CURR-IMS-SECTION                                    
135200                                                                          
135300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
135400             DELIMITED BY SIZE INTO SSA1                                  
135500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
135600     CALL CBLTDLI USING GU 4495-PCB DLI-IO-4495 SSA1                      
135700     MOVE 4495-STATUS-CODE       TO STATUS-WS                             
135800     PERFORM IMS-STATUS-CHECK                                             
135900     .                                                                    
136000 IMS-06-ISRT-4495  SECTION.                                               
136100     MOVE 'IMS-06' TO CURR-IMS-SECTION                                    
136200                                                                          
136300     MOVE  'WDR401 '       TO SSA1                                        
136400     MOVE '  '             TO GOOD-STATUSCODES                            
136500     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-4495 SSA1                    
136600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
136700     PERFORM IMS-STATUS-CHECK                                             
136800     .                                                                    
136900 IMS-07-GHNP-WDE611 SECTION.                                              
137000     MOVE 'IMS-07' TO CURR-IMS-SECTION                                    
137100                                                                          
137200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
137300            DELIMITED BY SIZE INTO SSA1                                   
137400     MOVE '  '                  TO GOOD-STATUSCODES                       
137500     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-E611 SSA1                    
137600     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
137700     PERFORM IMS-STATUS-CHECK                                             
137800     .                                                                    
137900 IMS-GHNP-WDE621 SECTION.                                                 
137910     MOVE 'IMS-GHNP-WDE621'  TO CURR-IMS-SECTION                          
138000                                                                          
138010     STRING 'WDE621  (KDKOLSTX =' W-KDKOLSTX-X                            
138020                    '&IDDCCROS =' REQU-IDDC-KEY                           
138030                    '&IDTRPTNC =' W-IDTRPTNC-X                            
138040                    '&TIRECDAT >' W-TIRECXDAT-X ')'                       
138050     DELIMITED BY SIZE INTO SSA1                                          
138060     MOVE '  GE'      TO GOOD-STATUSCODES                                 
138070     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-E621 SSA1                    
138080     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
138090     PERFORM IMS-STATUS-CHECK                                             
138091     .                                                                    
138092     SKIP3                                                                
138093 IMS-REPL-WDE621 SECTION.                                                 
138094     MOVE 'IMS-REPL-WDE621'  TO CURR-IMS-SECTION                          
138095                                                                          
138096     MOVE '  ' TO GOOD-STATUSCODES                                        
138097     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E621                         
138098     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
138099     PERFORM IMS-STATUS-CHECK                                             
138100     .                                                                    
138101                                                                          
138110 IMS-08-REPL-WDE611 SECTION.                                              
138200     MOVE 'IMS-08' TO CURR-IMS-SECTION                                    
138300                                                                          
138400     MOVE '  '             TO GOOD-STATUSCODES                            
138500     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
138600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
138700     PERFORM IMS-STATUS-CHECK                                             
138800     .                                                                    
138900 IMS-09-GHU-WDE601 SECTION.                                               
139000     MOVE 'IMS-09' TO CURR-IMS-SECTION                                    
139100                                                                          
139200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
139300            DELIMITED BY SIZE INTO SSA1                                   
139400     MOVE '  '                  TO GOOD-STATUSCODES                       
139500     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
139600     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
139700     PERFORM IMS-STATUS-CHECK                                             
139800     .                                                                    
139900 IMS-10-REPL-WDE601 SECTION.                                              
140000     MOVE 'IMS-10' TO CURR-IMS-SECTION                                    
140100                                                                          
140200     MOVE '  '             TO GOOD-STATUSCODES                            
140300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
140400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
140500     PERFORM IMS-STATUS-CHECK                                             
140600     .                                                                    
140700 IMS-11-GU-WDB201 SECTION.                                                
140800     MOVE 'IMS-11' TO CURR-IMS-SECTION                                    
140900                                                                          
141000     STRING 'WDB201  (IDGMT   >=' W-IDGMT-X ')'                           
141100       DELIMITED BY SIZE INTO SSA1                                        
141200     MOVE '    '           TO GOOD-STATUSCODES                            
141300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-B201 SSA1                      
141400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
141500     PERFORM IMS-STATUS-CHECK                                             
141600     .                                                                    
141700 IMS-12-ISRT-WDGX4498 SECTION.                                            
141800     MOVE 'IMS-12' TO CURR-IMS-SECTION                                    
141900                                                                          
142000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
142100       DELIMITED BY SIZE INTO SSA1                                        
142200     MOVE  'WDGX4498'      TO SSA2                                        
142300     MOVE '  '             TO GOOD-STATUSCODES                            
142400     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-4498 SSA1 SSA2               
142500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
142600     PERFORM IMS-STATUS-CHECK                                             
142700     .                                                                    
142800 IMS-13-GU-WDE6E              SECTION.                                    
142900     MOVE 'IMS-13' TO CURR-IMS-SECTION                                    
143000                                                                          
143100     STRING 'WDE6E1  (WDE6E1KY>=' W-WDE6E1KY-MIN-X                        
143200                    '&WDE6E1KY<=' W-WDE6E1KY-MAX-X ')'                    
143300            DELIMITED BY SIZE INTO SSA1                                   
143400     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
143500     CALL CBLTDLI USING GU WDE6E-PCB DLI-IO-E6E1 SSA1                     
143600     MOVE WDE6E-STATUS-CODE     TO STATUS-WS                              
143700     PERFORM IMS-STATUS-CHECK                                             
143800     .                                                                    
143900 IMS-14-GN-WDE6E        SECTION.                                          
144000     MOVE 'IMS-14' TO CURR-IMS-SECTION                                    
144100                                                                          
144200     STRING 'WDE6E1  (WDE6E1KY>=' W-WDE6E1KY-MIN-X                        
144300                    '&WDE6E1KY<=' W-WDE6E1KY-MAX-X ')'                    
144400            DELIMITED BY SIZE INTO SSA1                                   
144500     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
144600     CALL CBLTDLI USING GN WDE6E-PCB DLI-IO-E6E1 SSA1                     
144700     MOVE WDE6E-STATUS-CODE     TO STATUS-WS                              
144800     PERFORM IMS-STATUS-CHECK                                             
144900     .                                                                    
145000 IMS-15-GHU-WDGX4496 SECTION.                                             
145100     MOVE 'IMS-15' TO CURR-IMS-SECTION                                    
145200                                                                          
145300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
145400       DELIMITED BY SIZE INTO SSA1                                        
145500     MOVE  'WDGX4496 '     TO SSA2                                        
145600     MOVE '  GE'           TO GOOD-STATUSCODES                            
145700     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4496 SSA1 SSA2                
145800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
145900     PERFORM IMS-STATUS-CHECK                                             
146000     .                                                                    
146100                                                                          
146200 IMS-16-REPL-WDGX4496 SECTION.                                            
146300     MOVE 'IMS-16' TO CURR-IMS-SECTION                                    
146400                                                                          
146500     MOVE '  '             TO GOOD-STATUSCODES                            
146600     CALL CBLTDLI USING REPL 4495-PCB DLI-IO-4496                         
146700     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
146800     PERFORM IMS-STATUS-CHECK                                             
146900     .                                                                    
147000 IMS-17-ISRT-WDGX4496 SECTION.                                            
147100     MOVE 'IMS-17' TO CURR-IMS-SECTION                                    
147200                                                                          
147300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
147400       DELIMITED BY SIZE INTO SSA1                                        
147500     MOVE  'WDGX4496'      TO SSA2                                        
147600     MOVE '  '             TO GOOD-STATUSCODES                            
147700     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-4496 SSA1 SSA2               
147800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
147900     PERFORM IMS-STATUS-CHECK                                             
148000     .                                                                    
148100 IMS-18-GU-WDE6C                  SECTION.                                
148200     MOVE 'IMS-18' TO CURR-IMS-SECTION                                    
148300                                                                          
148400     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                        
148500                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
148600                    '&IDDC     =' W-IDDC-MIN ')'                          
148700            DELIMITED BY SIZE INTO SSA1                                   
148800     MOVE '  GE'                TO GOOD-STATUSCODES                       
148900     CALL CBLTDLI USING GU WDE6C-PCB DLI-IO-E6C1 SSA1                     
149000     MOVE WDE6C-STATUS-CODE     TO STATUS-WS                              
149100                                                                          
149200*    DISPLAY 'WL0186,WDE6C-STATUS-CODE: ', WDE6C-STATUS-CODE              
149300                                                                          
149400     PERFORM IMS-STATUS-CHECK                                             
149500     .                                                                    
149600 IMS-19-GN-WDE6C            SECTION.                                      
149700     MOVE 'IMS-19' TO CURR-IMS-SECTION                                    
149800                                                                          
149900     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                        
150000                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
150100                    '&IDDC     =' W-IDDC-MIN ')'                          
150200            DELIMITED BY SIZE INTO SSA1                                   
150300     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
150400     CALL CBLTDLI USING GN WDE6C-PCB DLI-IO-E6C1 SSA1                     
150500     MOVE WDE6C-STATUS-CODE     TO STATUS-WS                              
150600     PERFORM IMS-STATUS-CHECK                                             
150700     .                                                                    
150710 IMS-GU-WDE6H                  SECTION.                                   
150711     MOVE 'IMS-GU-WDE6H' TO CURR-IMS-SECTION                              
150720                                                                          
150730     STRING 'WDE6H1  (WDE6H1KY>=' W-WDE6H1KY-MIN-X                        
150740                    '&WDE6H1KY<=' W-WDE6H1KY-MAX-X                        
150750                    '&TIRECDAT >' W-TIRECXDAT-X                           
150760                    '&IDTRPTNC =' W-IDTRPTNC-X ')'                        
150770            DELIMITED BY SIZE INTO SSA1                                   
150780     MOVE '  GE'                TO GOOD-STATUSCODES                       
150790     CALL CBLTDLI USING GU WDE6H-PCB DLI-IO-E6H1 SSA1                     
150791     MOVE WDE6H-STATUS-CODE     TO STATUS-WS                              
150792                                                                          
150793     PERFORM IMS-STATUS-CHECK                                             
150794     .                                                                    
150795 IMS-GN-WDE6H                  SECTION.                                   
150796     MOVE 'IMS-GN-WDE6H' TO CURR-IMS-SECTION                              
150797                                                                          
150798     STRING 'WDE6H1  (WDE6H1KY>=' W-WDE6H1KY-MIN-X                        
150799                    '&WDE6H1KY<=' W-WDE6H1KY-MAX-X                        
150800                    '&TIRECDAT >' W-TIRECXDAT-X                           
150801                    '&IDTRPTNC =' W-IDTRPTNC-X ')'                        
150802            DELIMITED BY SIZE INTO SSA1                                   
150803     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
150804     CALL CBLTDLI USING GN WDE6H-PCB DLI-IO-E6H1 SSA1                     
150805     MOVE WDE6H-STATUS-CODE     TO STATUS-WS                              
150806     PERFORM IMS-STATUS-CHECK                                             
150807     .                                                                    
150810 IMS-22-GU-WDE4F                  SECTION.                                
150900     MOVE 'IMS-22' TO CURR-IMS-SECTION                                    
151000                                                                          
151100     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
151200                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
151300            DELIMITED BY SIZE INTO SSA1                                   
151400     MOVE '  GE'                TO GOOD-STATUSCODES                       
151500     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
151600     MOVE WDE4F-STATUS-CODE     TO STATUS-WS                              
151700     PERFORM IMS-STATUS-CHECK                                             
151800     .                                                                    
151900                                                                          
152000 IMS-23-GU-WDB601      SECTION.                                           
152010     MOVE 'IMS-23' TO CURR-IMS-SECTION                                    
152020                                                                          
152100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
152200          DELIMITED BY SIZE INTO SSA1                                     
152300     MOVE '  ' TO GOOD-STATUSCODES                                        
152400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-B601 SSA1                      
152500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
152600     PERFORM IMS-STATUS-CHECK                                             
152700     .                                                                    
152800 IMS-STATUS-CHECK   SECTION.                                              
152900                                                                          
153000     SET STATUS-IX TO 1                                                   
153100     SEARCH GOOD-STATUS                                                   
153200       AT END                                                             
153300         CALL FELLOG                                                      
153400     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
153500       CONTINUE                                                           
153600     END-SEARCH                                                           
153700     .                                                                    
