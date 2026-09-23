000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1615000.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   MAY 2010.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄGGER UPP ARTIKELINFO FÖR BIMA                                  
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDG3                                       
001200*                              WDD2                                       
001300*                              WDK6                                       
001400*                              WDD3                                       
001500*                              WDG2                                       
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR CROSS INDEX VIA PROG-TO-PROG               
001800*        SWITCH - W0T107X (WDF5)                                          
001900*                                                                         
002000*        PROGRAMMET SKICKAR DISPATCHER-TRANS TILL 5111-BILDEN             
002100*        VIA WDP8 FÖR RADPRIS-UPPDATERING (WDK621)                        
002200*                                                                         
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000*          --- INPUT BIMA FILE FROM VCOM                                  
003100     SELECT W16150                     ASSIGN TO W16150D1.                
003200     EJECT                                                                
003300*          --- OUTPUT FILE TO VCOM                                        
003400     SELECT W16151                     ASSIGN TO W16150D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W16150                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W1615001    -L.                                                
004500     EJECT                                                                
004600 FD  W16151                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  WS-REC           PIC X(100).                                         
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400*    -COPY WY2000W1                                                       
005500*    -COPY WWLNDKON                                                       
005600*    -COPY WWPRODSL                                                       
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'W1615000'.            
005900 01  CHKP-VAR.                                                            
006000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  W-MSG-IO-AREA-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
006900 77  W-MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
007000 77  W-CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
007100 77  W-CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
007200 77  W-DAREGDAT                  PIC 9(08)   VALUE ZERO.                  
007300 77  W-TID                       PIC S9(9).                               
007400 01  WS-TIUPPDAT                 PIC S9(7)   COMP-3.                      
007500 01  WS-TIUPPTID                 PIC S9(9)   COMP-3.                      
007600 01  WS-KVMAD                    PIC S9(6)V9(1) COMP-3.                   
007610 01  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
007700*                                                                         
007800 77  W16150-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W16150                       VALUE 'Y'.                   
008000*                                                                         
008100 77  W-KDBPSR                    PIC 9       VALUE ZERO.                  
008200     88  VALID-KDBPSR                        VALUE 0 THRU 8.              
008300*                                                                         
008400 01  W-IDINK.                                                             
008500     03  W-IDINK-X3              PIC X(3)    VALUE SPACES.                
008600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
008700*                                                                         
008800 01  W-IDAVTAL                   PIC 9(13).                               
008900 01  W-IDAVTAL-RED REDEFINES W-IDAVTAL.                                   
009000     03  FILLER                  PIC X.                                   
009100     03  W-PREFIX                PIC X(3).                                
009200     03  W-AVTALSNR              PIC X(6).                                
009300     03  W-SUFFIX                PIC X(3).                                
009400*                                                                         
009500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009800     SKIP3                                                                
009900*                                                                         
010000 01 W-ERR-DESC.                                                           
010100   03 ERR-IDARTNR                PIC X(10)   VALUE SPACES.                
010200   03 ERR-STATUS                 PIC X(8)    VALUE SPACES.                
010300   03 ERR-TXT                    PIC X(30)   VALUE SPACES.                
010400   03 ERR-VALUE-X                PIC X(15)   VALUE SPACES.                
010500   03  FILLER REDEFINES ERR-VALUE-X.                                      
010600     05  ERR-VALUE-N             PIC 9(15).                               
010700*                                                                         
010800 77  INREC-SW                    PIC X       VALUE 'J'.                   
010900     88  INREC-OK                            VALUE 'J'.                   
011000     88  INREC-ERROR                         VALUE 'N'.                   
011100*                                                                         
011200 01  ERROR-TEXT.                                                          
011300     03  FILLER                  PIC X(8)    VALUE 'ERR-TXT'.             
011400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
011500                                                                          
011600 01  W-EAN-CODE.                                                          
011700     03  W-EAN-CODE-X            PIC X(4)    VALUE 'EAN_'.                
011800     03  W-TEARTNOT              PIC X(13)   VALUE SPACES.                
011900*                                                                         
012000     SKIP2                                                                
012100 01  FELTEXT.                                                             
012200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012300     03  FELTEXT-STR1            PIC X(20)   VALUE SPACE.                 
012400     03  FELTEXT-STR2            PIC X(44)   VALUE SPACE.                 
012500     03  FILLER                  PIC X(2)    VALUE ': '.                  
012600     03  FELTEXT-STR3            PIC X(6)    VALUE SPACE.                 
012700 01  FELTEXT2.                                                            
012800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012900     03  FELTEXT2-STR1           PIC X(20)   VALUE SPACE.                 
013000     03  FELTEXT2-STR2           PIC X(52)   VALUE SPACE.                 
013100     EJECT                                                                
013200 01  DAGENS-DATUM-SEK            PIC 9(8)    VALUE ZERO.                  
013300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013400 01  FILLER REDEFINES DAGENS-DATUM.                                       
013500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013800     EJECT                                                                
013900     SKIP2                                                                
014000 01  W009KSIF-FALT.                                                       
014100     03  RESK-IDART              PIC 9(9).                                
014200     03  RESK-9-POS              PIC 9        VALUE 9.                    
014300     03  RESK-W009KSIFR          PIC 9.                                   
014400                                                                          
014500**** VARIABLER TILL W009VADD****                                          
014600 01  W009VADD-DATUM              PIC S9(5)    COMP-3.                     
014700 01  W009VADD-ANTAL              PIC S9(3)    COMP-3.                     
014800       EJECT                                                              
014900 01  DYNAMISKA-SUBPROGRAM.                                                
015000*                                                                         
015100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
015400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015500     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
015600     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
015700     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
015800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015900     03  WREVERSE                PIC X(8)    VALUE 'WREVERSE'.            
016000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016010     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
016100     EJECT                                                                
016200*    --- PARAMETRAR TILL POSTSUM                                          
016300*01  -COPY W0005   -PRE  POSTSUM-                                         
016400                                                                          
016500 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
016600*   -COPY WDATAREA                                                        
016700                                                                          
016800*01  AREA -COPY W1615001   -PRE IN-                                       
016900                                                                          
017000*    ---  MSG INPUT-OUTPUT AREA                                           
017100 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
017200*01  -COPY WMSGAREA                                                       
017300                                                                          
017400*    ---  AREA FÖR W006KOM SUBMODUL                                       
017500 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
017600 01  KOM-IO-AREA.                                                         
017700*    03  -COPY WMSGKOM                                                    
017800     EJECT                                                                
017900*    MID-AREA FÖR W5I11101                                       *        
018000*01  -COPY W5I11101 -PRE 5111-                                            
018100                                                                          
018200     EJECT                                                                
018300*   -COPY WREVAREA                                                        
018400     EJECT                                                                
018410*   -COPY W510CURR                                                        
018420     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018600                                                                          
018700 01 WS.                                                                   
018800   03 WS-AAVVD-ADD1              PIC S9(5)   COMP-3 VALUE ZERO.           
018900   03 WS-AAVVD-ADD6              PIC S9(5)   COMP-3 VALUE ZERO.           
019000   03 WS-TIAAVV-GRP              PIC 9(4)    VALUE ZERO.                  
019100   03 WS-TIAAVVD                 PIC 9(5)    VALUE ZERO.                  
019200   03 WS-W009REDU-IN             PIC X(30)   VALUE SPACE.                 
019300   03 WS-W009REDU-UT             PIC X(30)   VALUE SPACE.                 
019400   03 WS-IDARTNR-NUM             PIC S9(9)   VALUE ZERO COMP-3.           
019500   03 WS-RETULF                  PIC S9(3)V9(4) VALUE ZERO.               
019600   03 WS-RETULF-HELTAL           PIC 9(7).                                
019700   03 WS-PRARTBEL                PIC 9(13).                               
019800   03 IX                         PIC S9(9)   VALUE ZERO COMP-3.           
019900   03 INDX                       PIC S9(9)   VALUE ZERO COMP-3.           
020000   03 TAB-IX                     PIC S9(9)   VALUE ZERO COMP-3.           
020100   03 WS-FLMPB                   PIC X       VALUE SPACE.                 
020200 01  WS-TIFINLV                  PIC 9(5)    VALUE ZERO.                  
020300 01  FILLER REDEFINES WS-TIFINLV.                                         
020400     03  WS-AAVV                 PIC 9(4).                                
020500     03  WS-DAG                  PIC 9(1).                                
020600     SKIP3                                                                
020700 01  IDSKYLT-TABELL.                                                      
020800     03 FILLER                   PIC X(3)    VALUE 'CZ '.                 
020900     03 FILLER                   PIC X(3)    VALUE 'D  '.                 
021000     03 FILLER                   PIC X(3)    VALUE 'DK '.                 
021100     03 FILLER                   PIC X(3)    VALUE 'E  '.                 
021200     03 FILLER                   PIC X(3)    VALUE 'F  '.                 
021300     03 FILLER                   PIC X(3)    VALUE 'GB '.                 
021400     03 FILLER                   PIC X(3)    VALUE 'GR '.                 
021500     03 FILLER                   PIC X(3)    VALUE 'H  '.                 
021600     03 FILLER                   PIC X(3)    VALUE 'I  '.                 
021700     03 FILLER                   PIC X(3)    VALUE 'IR '.                 
021800     03 FILLER                   PIC X(3)    VALUE 'J  '.                 
021900     03 FILLER                   PIC X(3)    VALUE 'KOR'.                 
022000     03 FILLER                   PIC X(3)    VALUE 'MAL'.                 
022100     03 FILLER                   PIC X(3)    VALUE 'NL '.                 
022200     03 FILLER                   PIC X(3)    VALUE 'P  '.                 
022300     03 FILLER                   PIC X(3)    VALUE 'PL '.                 
022400     03 FILLER                   PIC X(3)    VALUE 'RC '.                 
022500     03 FILLER                   PIC X(3)    VALUE 'RCN'.                 
022600     03 FILLER                   PIC X(3)    VALUE 'RO '.                 
022700     03 FILLER                   PIC X(3)    VALUE 'RUS'.                 
022800     03 FILLER                   PIC X(3)    VALUE 'S  '.                 
022900     03 FILLER                   PIC X(3)    VALUE 'SF '.                 
023000     03 FILLER                   PIC X(3)    VALUE 'T  '.                 
023100     03 FILLER                   PIC X(3)    VALUE 'TR '.                 
023200     03 FILLER                   PIC X(3)    VALUE 'USA'.                 
023300     03 FILLER                   PIC X(3)    VALUE 'YU '.                 
023400 01  TAB  REDEFINES  IDSKYLT-TABELL.                                      
023500     03  FILLER OCCURS 26.                                                
023600         05  TAB-IDSKYLT         PIC X(3).                                
023700 01  MAX-TAB-IDSKYLT             PIC S9(3)   VALUE +26.                   
023800*                                                                         
023900 01  NYCKLAR-TILL-DLI.                                                    
024000   03  W-IDARTNR-X.                                                       
024100     05  W-IDARTNR               PIC S9(9)   COMP-3  VALUE ZERO.          
024200                                                                          
024300   03  W-IDLAND-X.                                                        
024400     05  W-IDLAND                PIC X(2)    VALUE SPACE.                 
024500                                                                          
024600   03  W-KDSEGKEY-X.                                                      
024700     05  W-KDSEGKEY              PIC X       VALUE '1'.                   
024800                                                                          
024900   03  W-BEART-X.                                                         
025000     05  W-BEART                 PIC X(25)   VALUE SPACE.                 
025100                                                                          
025200   03  W-IDBENNR-X.                                                       
025300     05  W-IDBENNR               PIC S9(7)   COMP-3 VALUE ZERO.           
025400                                                                          
025500   03  W-IDSKYLT-X.                                                       
025600     05  W-IDSKYLT               PIC X(03)   VALUE SPACE.                 
025700                                                                          
025800   03  W-1207-KEY-X.                                                      
025900     05  FILLER                  PIC X(04)   VALUE '1207'.                
026000     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
026100                                                                          
026200   03  W-WDG3KEY-2221-X.                                                  
026300     05  FILLER                  PIC X(04)   VALUE '2221'.                
026400     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
026500                                                                          
026600   03    W-IDLEVNR-X.                                                     
026700     05  W-IDLEVNR               PIC X(5)    VALUE SPACE.                 
026800                                                                          
028000     SKIP2                                                                
028100 01    W-PROG-TO-PROG-SW.                                                 
028200       03 M-SW-LL                PIC S9(4) VALUE +290 COMP SYNC.          
028300       03 M-SW-Z1-Z2             PIC  X(2) VALUE LOW-VALUE.               
028400       03 M-SW-KDTRANS           PIC  X(8) VALUE 'W0T107X'.               
028500       03 M-SW-IDTRANS           PIC  X(4) VALUE '1615'.                  
028600       03 M-SW-KDMFSTYP          PIC  X(1) VALUE '1'.                     
028700                                                                          
028800       03  MID -COPY W0I10701 -PRE PROG-                                  
028900*    --- STATUS-KOD FRÅN IMS                                              
029000 01  STATUS-WS                   PIC XX.                                  
029100     88  SEGMENT-FINNS                       VALUE '  '.                  
029200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
029500     88  IMS-EJ-OK                           VALUE 'XD'.                  
029600     SKIP2                                                                
029700 01  GODK-STATUSKODER.                                                    
029800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029900     SKIP3                                                                
030000 01  SSA1                        PIC X(96).                               
030100 01  SSA2                        PIC X(96).                               
030200 01  SSA3                        PIC X(96).                               
030300     EJECT                                                                
030400*    --- IMS FUNKTIONSKODER                                               
030500*01  -COPY W0003                                                          
030600     EJECT                                                                
030700*    ---  DLI INPUT-OUTPUT AREA                                           
030800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
030900 01  DLI-IO-WDK601.                                                       
031000*    03  -COPY WDK601                                                     
031100     EJECT                                                                
031200                                                                          
031300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
031400 01  DLI-IO-WDK611.                                                       
031500*    03  -COPY WDK611                                                     
031600     EJECT                                                                
031700                                                                          
031800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301'.                      
031900 01  DLI-IO-WDT301.                                                       
032000*    03  -COPY WDT301                                                     
032100     EJECT                                                                
032200                                                                          
032300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311'.                      
032400 01  DLI-IO-WDT311.                                                       
032500*    03  -COPY WDT311                                                     
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
032900 01  DLI-IO-WDK613.                                                       
033000*    03  -COPY WDK613                                                     
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK623'.                      
033400 01  DLI-IO-WDK623.                                                       
033500*    03  -COPY WDK623                                                     
033600     EJECT                                                                
033700                                                                          
033800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK625'.                      
033900 01  DLI-IO-WDK625.                                                       
034000*    03  -COPY WDK625                                                     
034100     EJECT                                                                
034200                                                                          
034300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
034400 01  DLI-IO-WDD301.                                                       
034500*    03  -COPY WDD301                                                     
034600     EJECT                                                                
034700                                                                          
034800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
034900 01  DLI-IO-WDD311.                                                       
035000*    03  -COPY WDD311                                                     
035100     EJECT                                                                
035200                                                                          
035300                                                                          
035400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD312'.                      
035500 01  DLI-IO-WDD312.                                                       
035600*    03  -COPY WDD312    -PRE WDD312-                                     
035700     EJECT                                                                
035800                                                                          
035900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDG201  '.                    
036000 01  DLI-IO-WDG201.                                                       
036100*    03  -COPY WDG201                                                     
036200     EJECT                                                                
036300                                                                          
036400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDG202  '.                    
036500 01  DLI-IO-WDG202.                                                       
036600*    03  -COPY WDGX1208  -PRE XXAI-                                       
036700     EJECT                                                                
036800                                                                          
036900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD201'.                      
037000 01  DLI-IO-WDD201.                                                       
037100*    03  -COPY WDD201    -PRE NYPON-                                      
037200     EJECT                                                                
037300                                                                          
037400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDG303'.                      
037500 01  DLI-IO-WDG303.                                                       
037600*    03  -COPY WDGX2222                                                   
037700                                                                          
037800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF101'.                      
037900 01  DLI-IO-WDF101.                                                       
038000*    03  -COPY WDF101                                                     
038100     EJECT                                                                
038200                                                                          
038300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF102'.                      
038400 01  DLI-IO-WDF102.                                                       
038500*    03  -COPY WDF102    -PRE WDF102-                                     
038600     EJECT                                                                
038700                                                                          
039200 LINKAGE SECTION.                                                         
039300                                                                          
039400*01  -COPY W0009  -PRE MSG-                                               
039500                                                                          
039600*01  -COPY W0009  -PRE ALT-                                               
039700                                                                          
039800*01  -COPY W0009  -PRE DISP-                                              
039900                                                                          
040000*01  -COPY W0008  -PRE WDK6-                                              
040100     05  FILLER                  PIC X.                                   
040200                                                                          
040300*01  -COPY W0008  -PRE WDD3-                                              
040400     05  FILLER                  PIC X.                                   
040500                                                                          
040600*01  -COPY W0008  -PRE WDD3A-                                             
040700     05  FILLER                  PIC X.                                   
040800                                                                          
040900*01  -COPY W0008  -PRE WDG2-                                              
041000     05  FILLER                  PIC X.                                   
041100                                                                          
041200*01  -COPY W0008  -PRE WDD2-                                              
041300     05  FILLER                  PIC X.                                   
041400                                                                          
041500*01  -COPY W0008  -PRE WDG3-                                              
041600     05  FILLER                  PIC X.                                   
041700                                                                          
041800*01  -COPY W0008  -PRE WDF1-                                              
041900     05  FILLER                  PIC X.                                   
042000                                                                          
042100*01  -COPY W0008  -PRE 9305-                                              
042200     05  FILLER                  PIC X.                                   
042300                                                                          
042400 01  WDP8-PCB                PIC X.                                       
042500                                                                          
042600*01  -COPY W0008  -PRE WDT3-                                              
042700     05  FILLER                  PIC X.                                   
042800                                                                          
042900     EJECT                                                                
043000 PROCEDURE DIVISION USING MSG-PCB ALT-PCB DISP-PCB                        
043100                          WDK6-PCB WDD3-PCB WDD3A-PCB WDG2-PCB            
043200                          WDD2-PCB WDG3-PCB WDF1-PCB 9305-PCB             
043300                          WDP8-PCB WDT3-PCB.                              
043400 MAIN SECTION.                                                            
043500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DISP-PCB                       
043600                          WDK6-PCB WDD3-PCB WDD3A-PCB WDG2-PCB            
043700                          WDD2-PCB WDG3-PCB WDF1-PCB 9305-PCB             
043800                          WDP8-PCB.                                       
043900                                                                          
044000     PERFORM IMS-RESTART                                                  
044100                                                                          
044200     PERFORM A-INIT                                                       
044300                                                                          
044400     PERFORM S01-READ-W16150                                              
044500                                                                          
044600     PERFORM UNTIL END-OF-W16150                                          
044700       IF CHKP-ANT > CHKP-MAX                                             
044800         PERFORM X-TAKE-CHECKPOINT                                        
044900       END-IF                                                             
045000                                                                          
045100       PERFORM B-VALIDATE-INREC                                           
045200       IF INREC-OK                                                        
045300         PERFORM C-UPDATE-WDK6                                            
045400         IF SEGMENT-FINNS                                                 
045500           PERFORM D-BEARBETA-BIMA                                        
045600           MOVE SPACES     TO WS-REC                                      
045700           MOVE IN-IDARTNR TO ERR-IDARTNR                                 
045800           MOVE 'OK'       TO ERR-STATUS                                  
045900           MOVE W-ERR-DESC TO WS-REC                                      
046000           PERFORM S02-WRITE-PROCESS-RESULT                               
046100         END-IF                                                           
046200       ELSE                                                               
046300         MOVE SPACES     TO WS-REC                                        
046400         MOVE 'ERROR'    TO ERR-STATUS                                    
046500         MOVE W-ERR-DESC TO WS-REC                                        
046600         PERFORM S02-WRITE-PROCESS-RESULT                                 
046700       END-IF                                                             
046800       PERFORM S01-READ-W16150                                            
046900     END-PERFORM                                                          
047000                                                                          
047100     MOVE ZERO TO RETURN-CODE                                             
047200     GOBACK                                                               
047300     .                                                                    
047400     EJECT                                                                
047500 A-INIT SECTION.                                                          
047600     SKIP2                                                                
047700     OPEN INPUT W16150                                                    
047800     OPEN OUTPUT W16151                                                   
047900                                                                          
048000     ACCEPT DAGENS-DATUM FROM DATE                                        
048100     MOVE 20                 TO DAGENS-DATUM-SEK (1:2)                    
048200     MOVE DAGENS-DATUM       TO DAGENS-DATUM-SEK (3:6)                    
048300     MOVE DAGENS-DATUM (1:2) TO W-DATE-AAMM(1:2)                          
048310     MOVE 01                 TO W-DATE-AAMM(3:2)                          
048400                                                                          
048500     MOVE 'IDAG  '   TO DAT-KDDATFORM                                     
048600     CALL WDATKONV USING DAT-KDDATFORM                                    
048700                         DAT-I-TIDATUM                                    
048800                         DAT-O-TIDATUM                                    
048900                         DAT-KDSVAR                                       
049000                                                                          
049100     IF DAT-KDSVAR-OK                                                     
049200*       MOVE DAT-TIAAVVD     TO WS-TIAAVVD                                
049300        MOVE DAT-TIAAVV-GRP  TO WS-TIAAVV-GRP                             
049400        MOVE WS-TIAAVV-GRP   TO W009VADD-DATUM                            
049500        MOVE +1              TO W009VADD-ANTAL                            
049600        CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                 
049700        MOVE W009VADD-DATUM  TO WS-AAVVD-ADD1                             
049800                                                                          
049900        MOVE DAT-TIAAVV-GRP  TO WS-TIAAVV-GRP                             
050000        MOVE WS-TIAAVV-GRP   TO W009VADD-DATUM                            
050100        MOVE +6              TO W009VADD-ANTAL                            
050200        CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                 
050300        MOVE W009VADD-DATUM  TO WS-AAVVD-ADD6                             
050400     ELSE                                                                 
050500        MOVE ZERO            TO W009VADD-DATUM                            
050600                                WS-AAVVD-ADD1                             
050700                                WS-AAVVD-ADD6                             
050800*                               WS-TIAAVVD                                
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 B-VALIDATE-INREC SECTION.                                                
051300     MOVE 'J'                             TO INREC-SW                     
051400     MOVE SPACE                           TO ERR-IDARTNR                  
051500     MOVE SPACE                           TO ERR-TXT                      
051600     MOVE SPACE                           TO ERR-VALUE-X                  
051700     MOVE ZERO                            TO WS-KVMAD                     
051800* VALIDATE BIMA PART NUMBER                                               
051900     IF INREC-OK                                                          
052000       IF IN-IDARTNR IS NUMERIC                                           
052100         MOVE IN-IDARTNR                  TO W-IDARTNR                    
052200         PERFORM IMS-GU-WDK601                                            
052300         IF SEGMENT-FINNS                                                 
052400           MOVE 'N'                       TO INREC-SW                     
052500           MOVE IN-IDARTNR                TO ERR-IDARTNR                  
052600           MOVE 'PART ALREADY EXISTS:'    TO ERR-TXT                      
052700           MOVE IN-IDARTNR                TO ERR-VALUE-X                  
052800         ELSE                                                             
052900           IF IN-IDARTNR > +99999999                                      
053000             MOVE 'N'                     TO INREC-SW                     
053100             MOVE IN-IDARTNR              TO ERR-IDARTNR                  
053200             MOVE 'NOT VALID PARTNO:   '  TO ERR-TXT                      
053300             MOVE IN-IDARTNR              TO ERR-VALUE-X                  
053400           ELSE                                                           
053500             CONTINUE                                                     
053600           END-IF                                                         
053700         END-IF                                                           
053800       ELSE                                                               
053900         MOVE 'N'                           TO INREC-SW                   
054000         MOVE IN-IDARTNR                    TO ERR-IDARTNR                
054100         MOVE 'PART NUMBER IS NOT NUMERIC:' TO ERR-TXT                    
054200         MOVE IN-IDARTNR                    TO ERR-VALUE-X                
054300       END-IF                                                             
054400     END-IF                                                               
054500                                                                          
054600* VALIDATE IF VALID SUPLLIER NAME                                         
054700     IF INREC-OK                                                          
054800       MOVE IN-IDLEVNR                 TO W-IDLEVNR                       
054900       PERFORM IMS-GU-WDF101                                              
055000       IF SEGMENT-FINNS                                                   
055100         CONTINUE                                                         
055200       ELSE                                                               
055300         MOVE 'N'                      TO INREC-SW                        
055400         MOVE IN-IDARTNR               TO ERR-IDARTNR                     
055500         MOVE 'INVALID SUPPLIER NAME:' TO ERR-TXT                         
055600         MOVE IN-IDLEVNR               TO ERR-VALUE-X                     
055700       END-IF                                                             
055800     END-IF                                                               
055900                                                                          
056000* VALIDATE WEIGHT                                                         
056100     IF INREC-OK                                                          
056200       IF IN-VKART IS NOT NUMERIC                                         
056300         MOVE 'N'                      TO INREC-SW                        
056400         MOVE IN-IDARTNR               TO ERR-IDARTNR                     
056500         MOVE 'WEIGHT IS NOT NUMERIC:' TO ERR-TXT                         
056600         MOVE IN-VKART                 TO ERR-VALUE-X                     
056700       END-IF                                                             
056800     END-IF                                                               
056900                                                                          
057000* VALIDATE VOLUME                                                         
057100     IF INREC-OK                                                          
057200       IF IN-VLARTNTO IS NOT NUMERIC                                      
057300         MOVE 'N'                      TO INREC-SW                        
057400         MOVE IN-IDARTNR               TO ERR-IDARTNR                     
057500         MOVE 'VOLUME IS NOT NUMERIC:' TO ERR-TXT                         
057600         MOVE IN-VLARTNTO              TO ERR-VALUE-X                     
057700       END-IF                                                             
057800     END-IF                                                               
057900                                                                          
058000* VALIDATE KDSORT                                                         
058100     IF INREC-OK                                                          
058200       IF IN-KDSORT = 'PA' OR 'ST' OR 'SA' OR 'KG' OR 'M '                
058300         OR ' M' OR ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G'                  
058400         OR 'C2' OR 'M2' OR 'ML' OR 'SW' OR 'TM' OR 'HW'                  
058500         CONTINUE                                                         
058600       ELSE                                                               
058700         MOVE 'N'                     TO INREC-SW                         
058800         MOVE IN-IDARTNR              TO ERR-IDARTNR                      
058900         MOVE 'INVALID KDSORT:'       TO ERR-TXT                          
059000         MOVE IN-KDSORT               TO ERR-VALUE-X                      
059100       END-IF                                                             
059200     END-IF                                                               
059300                                                                          
059400* VALIDATE TEARTNOT                                                       
059500     IF INREC-OK                                                          
059600       IF IN-TEARTNOT IS NOT NUMERIC                                      
059700         MOVE 'N'                        TO INREC-SW                      
059800         MOVE IN-IDARTNR                 TO ERR-IDARTNR                   
059900         MOVE 'EAN_CODE IS NOT NUMERIC:' TO ERR-TXT                       
060000         MOVE IN-TEARTNOT                TO ERR-VALUE-X                   
060100       END-IF                                                             
060200     END-IF                                                               
060300                                                                          
060400* VALIDATE QUANTITY IN PALLET                                             
060500     IF INREC-OK                                                          
060600       IF IN-KVPALL IS NOT NUMERIC                                        
060700         MOVE 'N'                      TO INREC-SW                        
060800         MOVE IN-IDARTNR               TO ERR-IDARTNR                     
060900         MOVE 'KVPALL IS NOT NUMERIC:' TO ERR-TXT                         
061000         MOVE IN-KVPALL                TO ERR-VALUE-X                     
061100       END-IF                                                             
061200     END-IF                                                               
061300                                                                          
061400* VALIDATE STANDARD PRICE                                                 
061500     IF INREC-OK                                                          
061600       IF IN-PRARTSTD IS NUMERIC AND IN-PRARTSTD > 0                      
061700         CONTINUE                                                         
061800       ELSE                                                               
061900         MOVE 'N'                       TO INREC-SW                       
062000         MOVE IN-IDARTNR                TO ERR-IDARTNR                    
062100         MOVE 'INVALID STANDARD PRICE:' TO ERR-TXT                        
062200         MOVE IN-PRARTSTD               TO ERR-VALUE-N                    
062300       END-IF                                                             
062400     END-IF                                                               
062500                                                                          
062600* VALIDATE CURRENCY CODE                                                  
062700     IF INREC-OK                                                          
062800       MOVE IN-KDVALISO                TO CURR-KDVALISO-ROW               
062810       MOVE 'SEK'                      TO CURR-KDVALISO-HUV               
062820       MOVE W-DATE-AAMM                TO CURR-TIAAMM                     
062830       MOVE 'A'                        TO CURR-KDVALTYP                   
062840       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
062850       IF CURR-KDSVAR = ' '                                               
063100         CONTINUE                                                         
063200       ELSE                                                               
063300         MOVE 'N'                       TO INREC-SW                       
063400         MOVE IN-IDARTNR                TO ERR-IDARTNR                    
063500         MOVE 'INVALID CURRENCY CODE:'  TO ERR-TXT                        
063600         MOVE IN-KDVALISO               TO ERR-VALUE-X                    
063700       END-IF                                                             
063800     END-IF                                                               
063900                                                                          
064000* VALIDATE ORDER PRICE                                                    
064100     IF INREC-OK                                                          
064200       IF IN-PRARTBEL IS NUMERIC AND IN-PRARTBEL > 0                      
064300         CONTINUE                                                         
064400       ELSE                                                               
064500         MOVE 'N'                       TO INREC-SW                       
064600         MOVE IN-IDARTNR                TO ERR-IDARTNR                    
064700         MOVE 'INVALID ORDER PRICE:'    TO ERR-TXT                        
064800         MOVE IN-PRARTBEL               TO ERR-VALUE-N                    
064900       END-IF                                                             
065000     END-IF                                                               
065100                                                                          
065200* VALIDATE PROCURER IDENTITY NUMBER                                       
065300     IF INREC-OK                                                          
065400       IF IN-IDANSK IS NUMERIC AND                                        
065500          IN-IDANSK(1:1) =  '9'                                           
065600         CONTINUE                                                         
065700       ELSE                                                               
065800         MOVE 'N'                       TO INREC-SW                       
065900         MOVE IN-IDARTNR                TO ERR-IDARTNR                    
066000         MOVE 'INVALID PROCURER NBR:'   TO ERR-TXT                        
066100         MOVE IN-IDANSK                 TO ERR-VALUE-X                    
066200       END-IF                                                             
066300     END-IF                                                               
066400                                                                          
066500* VALIDATE PURCHASE IDENTIFICATION NUMBER                                 
066600     IF INREC-OK                                                          
066700       IF IN-IDINK IS NOT NUMERIC                                         
066800         MOVE 'N'                       TO INREC-SW                       
066900         MOVE IN-IDARTNR                TO ERR-IDARTNR                    
067000         MOVE 'IDINK IS NOT NUMERIC:'   TO ERR-TXT                        
067100         MOVE IN-IDINK                  TO ERR-VALUE-X                    
067200       ELSE                                                               
067300         MOVE IN-IDINK                  TO W-IDINK-X3                     
067400       END-IF                                                             
067500     END-IF                                                               
067600                                                                          
067700* VALIDATE IDBERED                                                        
067800     IF INREC-OK                                                          
067900       IF IN-IDBERED IS NUMERIC AND                                       
068000          IN-IDBERED(1:1) =  '9'                                          
068100         CONTINUE                                                         
068200       ELSE                                                               
068300         MOVE 'N'                       TO INREC-SW                       
068400         MOVE IN-IDARTNR                TO ERR-IDARTNR                    
068500         MOVE 'INVALID IDBERED:'        TO ERR-TXT                        
068600         MOVE IN-IDBERED                TO ERR-VALUE-X                    
068700       END-IF                                                             
068800     END-IF                                                               
068900                                                                          
069000* VALIDATE KVQPACK-1                                                      
069100     IF INREC-OK                                                          
069200       IF IN-KVQPACK-1 IS NUMERIC                                         
069300         IF IN-KVQPACK-1 =  1                                             
069400           MOVE ZERO                      TO IN-KVQPACK-1                 
069500         END-IF                                                           
069600       ELSE                                                               
069700         MOVE 'N'                         TO INREC-SW                     
069800         MOVE IN-IDARTNR                  TO ERR-IDARTNR                  
069900         MOVE 'KVQPACK-1 IS NOT NUMERIC:' TO ERR-TXT                      
070000         MOVE IN-KVQPACK-1                TO ERR-VALUE-X                  
070100       END-IF                                                             
070200     END-IF                                                               
070300                                                                          
070400* VALIDATE TIFINLV                                                        
070500     IF INREC-OK                                                          
070600       IF IN-TIFINLV IS NOT NUMERIC                                       
070700         MOVE 'N'                        TO INREC-SW                      
070800         MOVE IN-IDARTNR                 TO ERR-IDARTNR                   
070900         MOVE 'TIFINLV IS NOT NUMERIC:'  TO ERR-TXT                       
071000         MOVE IN-TIFINLV                 TO ERR-VALUE-X                   
071100       END-IF                                                             
071200     END-IF                                                               
071300                                                                          
071400* VALIDATE KDFARLIG                                                       
071500     IF INREC-OK                                                          
071600       IF IN-KDFARLIG IS NOT NUMERIC                                      
071700         MOVE 'N'                        TO INREC-SW                      
071800         MOVE IN-IDARTNR                 TO ERR-IDARTNR                   
071900         MOVE 'KDFARLIG: IS NOT NUMERIC' TO ERR-TXT                       
072000         MOVE IN-KDFARLIG                TO ERR-VALUE-X                   
072100       END-IF                                                             
072200     END-IF                                                               
072300                                                                          
072400* VALIDATE KDBPSR                                                         
072500     IF INREC-OK                                                          
072600       MOVE IN-KDBPSR  TO W-KDBPSR                                        
072700       IF IN-KDBPSR IS NUMERIC AND VALID-KDBPSR                           
072800         CONTINUE                                                         
072900       ELSE                                                               
073000         MOVE 'N'                     TO INREC-SW                         
073100         MOVE IN-IDARTNR              TO ERR-IDARTNR                      
073200         MOVE 'INVALID KDBPSR  :'     TO ERR-TXT                          
073300         MOVE IN-KDBPSR               TO ERR-VALUE-X                      
073400       END-IF                                                             
073500     END-IF                                                               
073600                                                                          
073700* VALIDATE FUNCTION GROUP                                                 
073800     IF INREC-OK                                                          
073900       IF IN-IDFKNGRP IS NOT NUMERIC                                      
074000         MOVE 'N'                         TO INREC-SW                     
074100         MOVE IN-IDARTNR                  TO ERR-IDARTNR                  
074200         MOVE 'FKN GROUP IS NOT NUMERIC:' TO ERR-TXT                      
074300         MOVE IN-IDFKNGRP                 TO ERR-VALUE-X                  
074400       END-IF                                                             
074500     END-IF                                                               
074600                                                                          
074700* VALIDATE PRODUCT GROUP                                                  
074800     IF INREC-OK                                                          
074900       MOVE IN-KDPRODSL TO TEST-KDPRODSL                                  
075000       IF IN-KDPRODSL IS NUMERIC AND KDPRODSL-BIMA                        
075100         CONTINUE                                                         
075200       ELSE                                                               
075300         MOVE 'N'                             TO INREC-SW                 
075400         MOVE IN-IDARTNR                      TO ERR-IDARTNR              
075500         MOVE 'PRODUCT GROUP IS NOT NUMERIC:' TO ERR-TXT                  
075600         MOVE IN-KDPRODSL                     TO ERR-VALUE-X              
075700       END-IF                                                             
075800     END-IF                                                               
075900                                                                          
076000* VALIDATE KVPB                                                           
076100     IF INREC-OK                                                          
076200       IF IN-KVPB-SEP IS NOT NUMERIC                                      
076300         MOVE 'N'                      TO INREC-SW                        
076400         MOVE IN-IDARTNR               TO ERR-IDARTNR                     
076500         MOVE 'FORECAST IS NOT NUMERIC:' TO ERR-TXT                       
076600         MOVE IN-KVPB-SEP              TO ERR-VALUE-N                     
076700       ELSE                                                               
076800         IF IN-KVPB-SEP > ZERO                                            
076900           MOVE DAT-TIAAVVD     TO WS-TIAAVVD                             
077000           COMPUTE WS-KVMAD = IN-KVPB-SEP ** 0.85                         
077100         ELSE                                                             
077200           MOVE ZERO TO WS-KVMAD                                          
077300                        WS-TIAAVVD                                        
077400         END-IF                                                           
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800* VALIDATE KDUART                                                         
077900     IF INREC-OK                                                          
078000       IF IN-KDUART = ' '  OR 'M' OR 'P' OR 'S' OR 'A' OR 'L'             
078100         CONTINUE                                                         
078200       ELSE                                                               
078300         MOVE 'N'                     TO INREC-SW                         
078400         MOVE IN-IDARTNR              TO ERR-IDARTNR                      
078500         MOVE 'INVALID KDUART:'       TO ERR-TXT                          
078600         MOVE IN-KDUART               TO ERR-VALUE-X                      
078700       END-IF                                                             
078800     END-IF                                                               
078900     .                                                                    
079000                                                                          
079100 C-UPDATE-WDK6 SECTION.                                                   
079200** INITIERAR GEMENSAMMA VÄRDEN FÖR BIMA                                   
079300                                                                          
079400*** WDK601                                                                
079500     MOVE NEJ                TO ART-FLERS                                 
079600     MOVE NEJ                TO ART-FLIART                                
079700     MOVE SPACE              TO ART-IDAO (2)                              
079800                                ART-IDAO (3)                              
079900                                ART-IDAO (4)                              
080000                                ART-IDAO (5)                              
080100     MOVE 57                 TO ART-IDFTG                                 
080200     MOVE ZERO               TO ART-KDERS-UTG                             
080300     MOVE IN-KDSORT          TO ART-KDSORT                                
080400     MOVE ZERO               TO ART-TIERSDAT                              
080500     MOVE 1                  TO WS-DAG                                    
080600     IF IN-TIFINLV > 0 AND IN-TIFINLV > DAT-TIAAVVD                       
080700       MOVE IN-TIFINLV       TO WS-AAVV                                   
080800     ELSE                                                                 
080900       MOVE WS-AAVVD-ADD1    TO WS-AAVV                                   
081000     END-IF                                                               
081100     MOVE WS-TIFINLV         TO ART-TIFINLV                               
081200                                ART-TISOP                                 
081300     MOVE DAGENS-DATUM       TO ART-TIREGDAT                              
081400     MOVE ZERO               TO ART-TIURPROD                              
081500     MOVE IN-IDARTNR         TO ART-IDARTNR                               
081600                                RESK-IDART                                
081700                                W-IDARTNR                                 
081800                                WS-IDARTNR-NUM                            
081900     CALL W009KSIF USING RESK-IDART RESK-9-POS RESK-W009KSIFR             
082000     MOVE RESK-W009KSIFR     TO ART-REKSIFFR                              
082100     MOVE IN-IDLEVNR         TO ART-IDLEVNR                               
082200     MOVE IN-KDPRODSL        TO ART-KDPRODSL                              
082300     MOVE IN-IDFKNGRP        TO ART-IDFKNGRP                              
082400     MOVE 'BIMA'             TO ART-IDAO (1)                              
082410                                                                          
082500     MOVE 'N'                TO ART-FLBRAND                               
082600     MOVE 'N'                TO ART-FLBSNES                               
082700     MOVE  0                 TO ART-KDSOP                                 
082701     MOVE 15                 TO ART-KVEOP                                 
081400     MOVE ZERO               TO ART-KDANSKSEG                             
081400     MOVE SPACE              TO ART-IDCDS                                 
081400                                ART-KDARTSYS                              
082702                                                                          
082703     PERFORM IMS-ISRT-WDK601                                              
082704     IF SEGMENT-FINNS                                                     
082705        ADD +5 TO CHKP-ANT                                                
082800     END-IF                                                               
082900                                                                          
083000*** WDK611                                                                
083100     MOVE '1'                TO CLAG-KDSEGKEY                             
083200     MOVE ZERO               TO CLAG-ADLAGOMR                             
083300     MOVE ZERO               TO CLAG-ADGANG                               
083400     MOVE ZERO               TO CLAG-ADPLATS                              
083500     MOVE ZERO               TO CLAG-ADLAGOMR-SVS                         
083600     MOVE ZERO               TO CLAG-ADGANG-SVS                           
083700     MOVE ZERO               TO CLAG-ADPLATS-SVS                          
083800     MOVE SPACE              TO CLAG-ADINPORT                             
083900     MOVE +2              TO CLAG-BEFT                                    
084000     MOVE ZERO               TO CLAG-DAXPOINT                             
084100     MOVE 'N'                TO CLAG-FLAVRART                             
084200     MOVE 'N'                TO CLAG-FLFSP                                
084300     MOVE 'N'                TO CLAG-FLGEMART                             
084400     MOVE 'N'                TO CLAG-FLJIT                                
084500     MOVE 'N'                TO CLAG-FLLARM-BUF                           
084600     MOVE 'J'                TO CLAG-FLLSRDEL                             
084700     MOVE 'N'                TO CLAG-FLLTKSP                              
084800     MOVE 'N'                TO CLAG-FLMANAT                              
084900     MOVE 'N'                TO CLAG-FLMANBK                              
085000     MOVE 'N'                TO CLAG-FLMANGK                              
085100     MOVE 'N'                TO CLAG-FLMANKP                              
085200     MOVE 'N'                TO CLAG-FLMANLT                              
085300     MOVE 'N'                TO CLAG-FLMANOSK                             
085400     MOVE 'N'                TO CLAG-FLMANPB                              
085500     MOVE 'N'                TO CLAG-FLMANQ                               
085600     MOVE 'N'                TO CLAG-FLMANOPP                             
085700     MOVE 'N'                TO CLAG-FLMARKSP                             
085800     MOVE 'J'                TO CLAG-FLMPB                                
085900     MOVE 'N'                TO CLAG-KDOPPLAN                             
086000     MOVE 'N'                TO CLAG-FLOREGPB                             
086100     MOVE 'N'                TO CLAG-FLRADREF                             
086200     MOVE 'N'                TO CLAG-FLREFILL                             
086300     MOVE 'N'                TO CLAG-FLRELSP                              
086400     MOVE 'N'                TO CLAG-FLSKROT-BEORD                        
086500     MOVE 'N'                TO CLAG-FLSPKOST                             
086600     MOVE 'N'                TO CLAG-FLTOPP                               
086700     MOVE 'N'                TO CLAG-FLTPO1                               
086710     MOVE ZERO               TO CLAG-KDUVKNTO                             
086800     MOVE IN-IDANSK          TO CLAG-IDANSK                               
086900     MOVE ZERO               TO CLAG-IDARTNR-EMBQ0                        
087000                                CLAG-IDARTNR-EMBQ1                        
087100                                CLAG-IDARTNR-EMBQ2                        
087200                                CLAG-IDARTNR-EMBQ3                        
087300                                CLAG-IDARTNR-EMBQ4                        
087400     MOVE IN-IDBERED         TO CLAG-IDBERED                              
087500     MOVE SPACE              TO CLAG-IDFS-SEN                             
087600     MOVE 'N'                TO CLAG-FLSKROT-AUTO                         
087700     MOVE SPACE              TO CLAG-IDKAT(1)                             
087800     MOVE SPACE              TO CLAG-IDKAT(2)                             
087900     MOVE SPACE              TO CLAG-IDKAT(3)                             
088000     MOVE SPACE              TO CLAG-IDLEVNR-SEN                          
088100     MOVE IN-IDLEVNR         TO CLAG-IDLEVNR-Ship                         
088200     MOVE ZERO               TO CLAG-IDLKTO                               
088300     MOVE 9                  TO CLAG-IDPLANGR-AG                          
088400     MOVE 1                  TO CLAG-IDPLANGR-LEV                         
088500     MOVE SPACE              TO CLAG-IDPROENH(1)                          
088600                                CLAG-IDPROENH(2)                          
088700                                CLAG-IDPROENH(3)                          
088800     MOVE 'BIMA'             TO CLAG-IDPROJ                               
088900     MOVE SPACE              TO CLAG-IDPROJUP                             
089000     MOVE ZERO               TO CLAG-IDPSN                                
089100     MOVE SPACE              TO CLAG-IDRITN                               
089200     MOVE ZERO               TO CLAG-IDSTATNR(1)                          
089300                                CLAG-IDSTATNR(2)                          
089400                                CLAG-IDSTATNR(3)                          
089500                                CLAG-IDSTATNR(4)                          
089600                                CLAG-IDSTATNR(5)                          
089700                                CLAG-IDSTATNR(6)                          
089800     MOVE SPACE              TO CLAG-IDUSER-EMB                           
089900     MOVE SPACE              TO CLAG-IDUSER-SPKVAL                        
090000     MOVE SPACE              TO CLAG-KDAGE                                
090100     MOVE ZERO               TO CLAG-KDARTHNT                             
090200     MOVE SPACE              TO CLAG-KDARTURS                             
090300     MOVE +1                 TO CLAG-KDAVT                                
090400     MOVE IN-KDBPSR          TO CLAG-KDBPSR                               
090500     MOVE SPACE              TO CLAG-KDEFFMAN                             
090600     MOVE ZERO               TO CLAG-KDEMBKOD-0                           
090700     MOVE ZERO               TO CLAG-KDEMBKOD-1                           
090800     MOVE ZERO               TO CLAG-KDEMBKOD-2                           
090900     MOVE ZERO               TO CLAG-KDERS                                
091000     MOVE ZERO               TO CLAG-KDEXCHA                              
091100     MOVE IN-KDFARLIG        TO CLAG-KDFARLIG                             
091200     MOVE ZERO               TO CLAG-KDFORPPL                             
091300                                CLAG-KDFORPGP                             
091400                                CLAG-KDFORPUF                             
091500     MOVE SPACE              TO CLAG-KDFREKKL                             
091600     MOVE ZERO               TO CLAG-KDGK                                 
091700     MOVE ZERO               TO CLAG-KDHF                                 
091800     MOVE +1                 TO CLAG-KDKG                                 
091900     MOVE +2                 TO CLAG-KDKSP                                
092000     MOVE 'P'                TO CLAG-KDLEVPLF                             
092100     MOVE ZERO               TO CLAG-KDLEVSP                              
092200     MOVE ZERO               TO CLAG-KDLPSP                               
092300     MOVE +1                 TO CLAG-KDLTK                                
092400     MOVE SPACE              TO CLAG-KDPRISKL                             
092500     MOVE 11                 TO CLAG-KDPSLLOC                             
092600     MOVE ZERO               TO CLAG-KDSPEEMB                             
092700     MOVE ZERO               TO CLAG-KDSRA                                
092800     MOVE 1                  TO CLAG-KDTIPPR                              
092900     MOVE ZERO               TO CLAG-KDTULLRE                             
093000     MOVE IN-KDUART          TO CLAG-KDUART                               
093100     MOVE ZERO               TO CLAG-KDVTH                                
093200     MOVE ZERO               TO CLAG-KDVVKL                               
093300     MOVE ZERO               TO CLAG-KDVSOP                               
093400     MOVE 1                  TO CLAG-KDYTBEH                              
093500     MOVE ZERO               TO CLAG-KVAKS-CDC                            
093600     MOVE ZERO               TO CLAG-KVAKS-PAV                            
093700     MOVE ZERO               TO CLAG-KVAKS-T                              
093800     MOVE ZERO               TO CLAG-KVAP                                 
093900     MOVE ZERO               TO CLAG-KVAVIS-SEN                           
094000     MOVE ZERO               TO CLAG-KVBK                                 
094100     MOVE ZERO               TO CLAG-KVDAGAR-FFH                          
094200     MOVE ZERO               TO CLAG-KVDAGAR-INLEV                        
094300     MOVE ZERO               TO CLAG-KVDAGAR-TT                           
094400     MOVE ZERO               TO CLAG-KVEFRS                               
094500     MOVE 8                  TO CLAG-KVFRYSTI                             
094600     MOVE ZERO               TO CLAG-KVINVS                               
094700     MOVE ZERO               TO CLAG-KVKP                                 
094800     MOVE ZERO               TO CLAG-KVLAAN                               
094900     MOVE ZERO               TO CLAG-KVLS                                 
095000     MOVE ZERO               TO CLAG-KVLS-SVS                             
095100     MOVE WS-KVMAD           TO CLAG-KVMAD-SEP                            
095200     MOVE WS-KVMAD           TO CLAG-KVMAD-TOT                            
095300     MOVE ZERO               TO CLAG-KVMP                                 
095400     MOVE ZERO               TO CLAG-KVOVERF                              
095500     MOVE IN-KVPALL          TO CLAG-KVPALL                               
095600     MOVE IN-KVPB-SEP        TO CLAG-KVPB-HIST                            
095700     MOVE ZERO               TO CLAG-KVPB-SATS                            
095800     MOVE ZERO               TO CLAG-KVPB-TPO                             
095900     MOVE IN-KVPB-SEP        TO CLAG-KVPB-SEP                             
096000     MOVE ZERO               TO CLAG-KVPB-VESL                            
096100     MOVE ZERO               TO CLAG-KVPOINT                              
096200     MOVE ZERO               TO CLAG-KVQ                                  
096300     MOVE ZERO               TO CLAG-KVQ-JUST                             
096400     MOVE ZERO               TO CLAG-KVQPACK-0                            
096500     MOVE IN-KVQPACK-1       TO CLAG-KVQPACK-1                            
096600     MOVE ZERO               TO CLAG-KVQPACK-2                            
096700     MOVE ZERO               TO CLAG-KVQPACK-3                            
096800     MOVE ZERO               TO CLAG-KVQPACK-4                            
096900     MOVE ZERO               TO CLAG-KVRESS                               
097000     MOVE ZERO               TO CLAG-KVRETUR                              
097100     MOVE ZERO               TO CLAG-KVROS                                
097200     MOVE ZERO               TO CLAG-KVSLAGER                             
097300     MOVE ZERO               TO CLAG-KVSPANT                              
097400     MOVE ZERO               TO CLAG-KVSPARR-KVAL                         
097500     MOVE ZERO               TO CLAG-KVSLUTKP                             
097600     MOVE ZERO               TO CLAG-KVUTJFEL                             
097700     MOVE ZERO               TO CLAG-KVUTRS                               
097800     MOVE ZERO               TO CLAG-KVVECKOR-AT                          
097900     MOVE ZERO               TO CLAG-KVVECKOR-BT                          
098000     MOVE ZERO               TO CLAG-KVVECKOR-FT                          
098100     MOVE ZERO               TO CLAG-KVVECKOR-LT                          
098200     MOVE ZERO               TO CLAG-PRARTBTO-EXP                         
098300     MOVE ZERO               TO CLAG-PRARTSJK                             
098400     MOVE IN-PRARTSTD        TO CLAG-PRARTSTD                             
098500     MOVE ZERO               TO CLAG-PRDIRLON                             
098600     MOVE ZERO               TO CLAG-PRDMTRL                              
098700     MOVE ZERO               TO CLAG-PRINK                                
098800     MOVE ZERO               TO CLAG-PRLFKST                              
098900     MOVE ZERO               TO CLAG-PRORDSK                              
099000     MOVE ZERO               TO CLAG-PROVRPAL                             
099100     MOVE ZERO               TO CLAG-PRREF                                
099200     MOVE ZERO               TO CLAG-REDIRLEV                             
099300     MOVE ZERO               TO CLAG-RESLJUST                             
099400     MOVE ZERO               TO CLAG-RETULF                               
099500     MOVE ZERO               TO CLAG-RVPROFEL                             
099600     MOVE ZERO               TO CLAG-RVPROURS                             
099700     MOVE ZERO               TO CLAG-TIAVIDAT-SEN                         
099800     MOVE WS-AAVVD-ADD6      TO CLAG-TIBESRPT-PAAM                        
099900     MOVE ZERO               TO CLAG-TIBESRPT                             
100000     MOVE ZERO               TO CLAG-TIDISPIN                             
100100     MOVE ZERO               TO CLAG-TIINVDAT                             
100200     MOVE ZERO               TO CLAG-TILPSP                               
100300     MOVE ZERO               TO CLAG-TILTK                                
100400     MOVE ZERO               TO CLAG-TIOMSPEC                             
100500     MOVE ZERO               TO CLAG-TISTOREF                             
100600     MOVE ZERO               TO CLAG-TIRODAT                              
100700     MOVE WS-TIAAVVD         TO CLAG-TIPBDAT                              
100800     MOVE ZERO               TO CLAG-TIQJUST                              
100900     MOVE ZERO               TO CLAG-TIREFSTO                             
101000     MOVE ZERO               TO CLAG-TISLJUST                             
101100     MOVE ZERO               TO CLAG-TISLUTKP                             
101200     MOVE ZERO               TO CLAG-TISPARR-KVAL                         
101300     MOVE IN-VKART           TO CLAG-VKART                                
101310                                CLAG-VKART-NTO                            
101400     MOVE IN-VLARTNTO        TO CLAG-VLARTNTO                             
101510     MOVE ZERO               TO CLAG-DAPBPLAN                             
101600     MOVE ZERO               TO CLAG-DASEASON                             
101700     MOVE ZERO               TO CLAG-KVPB-PLAN                            
101800     MOVE 1.00               TO CLAG-RESEASON-PLAN(1)                     
101900                                CLAG-RESEASON-PLAN(2)                     
102000                                CLAG-RESEASON-PLAN(3)                     
102100                                CLAG-RESEASON-PLAN(4)                     
102200                                CLAG-RESEASON-PLAN(5)                     
102300                                CLAG-RESEASON-PLAN(6)                     
102400                                CLAG-RESEASON-PLAN(7)                     
102500                                CLAG-RESEASON-PLAN(8)                     
102600                                CLAG-RESEASON-PLAN(9)                     
102700                                CLAG-RESEASON-PLAN(10)                    
102800                                CLAG-RESEASON-PLAN(11)                    
102900                                CLAG-RESEASON-PLAN(12)                    
103000     MOVE ZERO               TO CLAG-TIUPPDAT-EMB                         
103100     MOVE 'N'                TO CLAG-FLEJBUFF                             
103200     MOVE ZERO               TO CLAG-TILEVDAG (1)                         
103300                                CLAG-TILEVDAG (2)                         
103400                                CLAG-TILEVDAG (3)                         
103500                                CLAG-TILEVDAG (4)                         
103600                                CLAG-TILEVDAG (5)                         
103700     MOVE ZERO               TO CLAG-TISKROT                              
103800     MOVE 'N'                TO CLAG-FLCDART                              
103900     MOVE ZERO               TO CLAG-ADLAGOMR-CD(1)                       
104000     MOVE ZERO               TO CLAG-ADGANG-CD(1)                         
104100     MOVE ZERO               TO CLAG-ADPLATS-CD(1)                        
104200     MOVE ZERO               TO CLAG-ADLAGOMR-CD(2)                       
104300     MOVE ZERO               TO CLAG-ADGANG-CD(2)                         
104400     MOVE ZERO               TO CLAG-ADPLATS-CD(2)                        
104500     MOVE ZERO               TO CLAG-ADLAGOMR-CD(3)                       
104600     MOVE ZERO               TO CLAG-ADGANG-CD(3)                         
104700     MOVE ZERO               TO CLAG-ADPLATS-CD(3)                        
104800     MOVE ZERO               TO CLAG-ADLAGOMR-CD(4)                       
104900     MOVE ZERO               TO CLAG-ADGANG-CD(4)                         
105000     MOVE ZERO               TO CLAG-ADPLATS-CD(4)                        
105100     MOVE ZERO               TO CLAG-KVLS-CD(1)                           
105200     MOVE ZERO               TO CLAG-KVLS-CD(2)                           
105300     MOVE ZERO               TO CLAG-KVLS-CD(3)                           
105400     MOVE ZERO               TO CLAG-KVLS-CD(4)                           
105500     MOVE ZERO               TO CLAG-KVRESS-CD(1)                         
105600     MOVE ZERO               TO CLAG-KVRESS-CD(2)                         
105700     MOVE ZERO               TO CLAG-KVRESS-CD(3)                         
105800     MOVE ZERO               TO CLAG-KVRESS-CD(4)                         
105900     MOVE ZERO               TO CLAG-KVVORKO                              
106000     MOVE ZERO               TO CLAG-TIMAIL-KVAL                          
106100     MOVE ZERO               TO CLAG-PRHEMTAG                             
106200     MOVE ZERO               TO CLAG-KVEOQ                                
106300     MOVE W-IDINK            TO CLAG-IDINK                                
106400     MOVE ZERO               TO CLAG-KVULOAD                              
106500     MOVE 'J'                TO CLAG-FLNYBER                              
106600     MOVE ZERO               TO CLAG-KVSLAGER-OPT                         
106700     MOVE ZERO               TO CLAG-KVVECKOR-LVAR                        
106800     MOVE ZERO               TO CLAG-TIPBLOCK                             
106900     MOVE ZERO               TO CLAG-KVPB-TREND                           
107000     MOVE ZERO               TO CLAG-KVVECKOR-TREND                       
107100     MOVE ZERO               TO CLAG-TIDATUM-TREND                        
107200     MOVE ZERO               TO CLAG-TISKROT-AUTO                         
107300     MOVE ZERO               TO CLAG-KVREFBER-PLOCK                       
107400     MOVE ZERO               TO CLAG-KVREFPKT-PLOCK                       
107500     MOVE ZERO               TO CLAG-KVOI-PLOCK                           
107600     MOVE ZERO               TO CLAG-KVOI-OVR                             
107700     MOVE 'N'                TO CLAG-FLSKROT-BEV                          
107800     MOVE 'N'                TO CLAG-FLSKROT-SL                           
107900     MOVE 'N'                TO CLAG-FLSKROT-WLC                          
108000     MOVE ZERO               TO CLAG-TISKPREL                             
108100     MOVE SPACE              TO CLAG-ADINLOMR-BOA                         
108200     MOVE SPACE              TO CLAG-IDDC-REF                             
108300     MOVE ZERO               TO CLAG-KVBEART                              
108400     MOVE ZERO               TO CLAG-KVAVROP-TOT                          
108500     MOVE ZERO               TO CLAG-KVREFOVL-TOT                         
108600     MOVE ZERO               TO CLAG-KVRETUR-TOT                          
108700     MOVE ZERO               TO CLAG-KVTILLG-TOT                          
108800     MOVE ZERO               TO CLAG-TIGILTIG-PCOO                        
108900     MOVE ZERO               TO CLAG-TISTODAT-LARM                        
109000     MOVE ZERO               TO CLAG-KVMAXPL                              
109100     MOVE SPACE              TO CLAG-KDPCOO                               
109300                                CLAG-IDUSER-VUPD                          
109400     MOVE ZERO               TO CLAG-KVPB-PLAN-JUST1                      
109500                                CLAG-TIPBPLAN-JUST1-FOM                   
109600                                CLAG-TIPBPLAN-JUST1-TOM                   
109700                                CLAG-KVPB-PLAN-JUST2                      
109800                                CLAG-TIPBPLAN-JUST2-FOM                   
109900                                CLAG-TIPBPLAN-JUST2-TOM                   
109910                                CLAG-TIUPPDAT-VUPD                        
109920     MOVE 'N'                TO CLAG-FLAUTREL                             
110000                                                                          
110100     PERFORM IMS-ISRT-WDK611                                              
110200     IF SEGMENT-FINNS                                                     
110300       ADD +2 TO CHKP-ANT                                                 
110400     END-IF                                                               
110500                                                                          
110600**WDT311  Historik förpackningstyp                                        
110700     MOVE IN-IDARTNR TO FART-IDARTNR                                      
110800     PERFORM IMS-ISRT-WDT301                                              
110900     MOVE WC-LAND-SE TO FPCK-IDLANDX2                                     
111000     MOVE 'W16150'   TO FPCK-IDUSER                                       
111100     MOVE 2          TO FPCK-BEFT                                         
111200     MOVE ZERO       TO FPCK-KDFORPPL                                     
111300     MOVE ZERO       TO FPCK-KDFORPGP                                     
111400     MOVE ZERO       TO FPCK-KDFORPUF                                     
111500     MOVE SPACE      TO FPCK-TEBEFT(1)                                    
111600                        FPCK-TEBEFT(2)                                    
111700                        FPCK-TEBEFT(3)                                    
111800                        FPCK-TEBEFT(4)                                    
111900                        FPCK-TEBEFT(5)                                    
112000     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAREGDAT                        
112100     COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - W-DAREGDAT                
112200     ACCEPT W-TID FROM TIME                                               
112300     COMPUTE FPCK-TIKLOCK-9KOMPL    = 999999999 - W-TID                   
112400     PERFORM IMS-ISRT-WDT311                                              
112500     IF SEGMENT-FINNS                                                     
112600       ADD +1 TO CHKP-ANT                                                 
112700     END-IF                                                               
112800**WDK613                                                                  
112900     IF CLAG-KVQPACK-1 > ZERO                                             
113000       MOVE 'Q1 '                 TO EMB-KDEMBKEY                         
113100       MOVE CLAG-IDARTNR-EMBQ1    TO EMB-IDARTNR-EMB                      
113200       MOVE CLAG-KDEMBKOD-1       TO EMB-KDEMBKOD                         
113300       MOVE CLAG-KVQPACK-1        TO EMB-KVQPACK-EMB                      
113400       PERFORM IMS-ISRT-WDK613-EMB                                        
113500       IF SEGMENT-FINNS                                                   
113600         ADD +1 TO CHKP-ANT                                               
113700       END-IF                                                             
113800     END-IF                                                               
113900**WDK623                                                                  
114000     MOVE ZERO               TO W-IDAVTAL                                 
114100     MOVE SPACES             TO W-IDAVTAL-RED                             
114200     MOVE IN-IDINK           TO W-PREFIX                                  
114300     MOVE '909999'           TO W-AVTALSNR                                
114400     MOVE '490'              TO W-SUFFIX                                  
114500     MOVE W-IDAVTAL          TO AVT-IDAVTAL                               
114600     MOVE IN-IDLEVNR         TO AVT-IDLEVNR-AVT                           
114700     MOVE +1                 TO AVT-KDBEH-AVT                             
114800     MOVE ZERO               TO AVT-KVAVTANT                              
114900     MOVE DAGENS-DATUM       TO AVT-TIAVTAL                               
115000     PERFORM IMS-ISRT-WDK623                                              
115100     IF SEGMENT-FINNS                                                     
115200       ADD +1 TO CHKP-ANT                                                 
115300     END-IF                                                               
115400*                                                                         
115500** WDK625                                                                 
115600     IF IN-TEARTNOT > ZERO                                                
115700       MOVE IN-IDARTNR TO W-IDARTNR                                       
115800       MOVE +7         TO NOT-KDNOTTYP                                    
115900       MOVE IN-TEARTNOT TO W-TEARTNOT                                     
116000       MOVE W-EAN-CODE TO NOT-TEARTNOT                                    
116100       PERFORM IMS-ISRT-WDK625                                            
116200       IF SEGMENT-FINNS                                                   
116300         ADD +1 TO CHKP-ANT                                               
116400       END-IF                                                             
116500     END-IF                                                               
116600     .                                                                    
116700     EJECT                                                                
116800 D-BEARBETA-BIMA SECTION.                                                 
116900*** HÄMTA TULLFAKTOR                                                      
117000     MOVE IN-IDLEVNR         TO W-IDLEVNR                                 
117100     PERFORM IMS-GU-WDF101                                                
117200                                                                          
117300     MOVE 'SE'               TO W-IDLAND                                  
117400     PERFORM IMS-GNP-WDF102                                               
117500     MOVE WDF102-TULL-TITULF TO TMP1-YYMMDD                               
117600     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
117700     PERFORM WY2000P1                                                     
117800     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
117900       MOVE WDF102-TULL-RETULF-1  TO WS-RETULF                            
118000     ELSE                                                                 
118100       MOVE WDF102-TULL-RETULF-2  TO WS-RETULF                            
118200     END-IF                                                               
118300                                                                          
118700*** DISPATCHERTRANS TILL 5111-BILDEN FÖR UPPD AV WDK621                   
118800*** INITIERA WMSGKOM                                                      
118900                                                                          
119000     MOVE   SPACE                 TO MSG-KOM-WMSGKOM                      
119100     MOVE   +54                   TO MSG-KOM-KVLL                         
119200     MOVE   LOW-VALUE             TO MSG-KOM-KDZ1                         
119300     MOVE   LOW-VALUE             TO MSG-KOM-KDZ2                         
119400     MOVE   SPACE                 TO MSG-KOM-KDTRANS                      
119500     MOVE   'BIMA'                TO MSG-KOM-IDSNDNOD                     
119600     MOVE   'W1615000'            TO MSG-KOM-IDSNDJOB                     
119700     MOVE   'W5I11101'            TO MSG-KOM-IDCPYTXT                     
119800     ACCEPT WS-TIUPPDAT           FROM DATE                               
119900     MOVE   WS-TIUPPDAT           TO MSG-KOM-TIREGDAT                     
120000     ACCEPT WS-TIUPPTID           FROM TIME                               
120100     MOVE   WS-TIUPPTID           TO MSG-KOM-TIKLOCK                      
120200     MOVE   SPACE                 TO MSG-KOM-IDMFSMED                     
120300                                     MSG-KOM-KDSVAR                       
120400                                                                          
120500     COMPUTE MSG-KVLL = LENGTH OF 5111-W5I11101 + 17                      
120600     MOVE 'W5T111X'               TO MSG-KDTRANS-1                        
120700     MOVE '5111'                  TO MSG-IDTRANS-1                        
120800     MOVE '1'                     TO MSG-KDMFSFOR-1                       
120900                                                                          
121000     MOVE W-IDARTNR               TO 5111-IDARTNR-IN                      
121100                                                                          
121200****************** DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
121300     MOVE '+++++'                 TO 5111-IDLEVNR-IN                      
121400     MOVE '+'                     TO 5111-KDPRBEH-IN                      
121500     MOVE '++++++'                TO 5111-REAENDR-IN                      
121600     MOVE SPACE                   TO 5111-IDLEVNR-UT                      
121700                                     5111-REAENDR-UT                      
121800     MOVE SPACE                   TO 5111-KDPRBEH-UT                      
121900************************************************************              
122000                                                                          
122100     MOVE 'M'                     TO 5111-KDPRURSP-U                      
122200     MOVE DAGENS-DATUM            TO 5111-TIPRLIST-U                      
122300                                                                          
122400*    MOVE IN-PRARTBEL             TO WS-PRARTBEL                          
122500     COMPUTE WS-PRARTBEL = 100000 * IN-PRARTBEL                           
122600     MOVE WS-PRARTBEL(1:8)        TO 5111-PRARTBEL-U(1:8)                 
122700     MOVE '.'                     TO 5111-PRARTBEL-U(9:1)                 
122800     MOVE WS-PRARTBEL(9:5)        TO 5111-PRARTBEL-U(10:5)                
122900                                                                          
123000     COMPUTE WS-RETULF-HELTAL = 10000 * WS-RETULF                         
123400                                                                          
123500     MOVE IN-KDVALISO             TO 5111-KDVALISO-U                      
123600     MOVE ART-IDLEVNR             TO 5111-IDLEVNR-U                       
123700                                                                          
123800     MOVE '+'                     TO 5111-FLPRIBES-U                      
123900                                     5111-FLPRIGO-U                       
124000     MOVE SPACE                   TO 5111-KDFPKPRI-U                      
124100     MOVE MSG-SIGNON-USERID       TO 5111-IDUSER                          
124200                                                                          
124300     MOVE 5111-W5I11101           TO MSG-INDATA-MINUS-1-TRANSKOD          
124400                                                                          
124500     CALL W006KOM USING MSG-PCB                                           
124600                        DISP-PCB                                          
124700                        WDP8-PCB                                          
124800                        MSG-KOM-WMSGKOM                                   
124900                        MSG-IO-AREA                                       
125000                                                                          
125100     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
125200        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
125300          DELIMITED BY SIZE  INTO ERROR-TEXT-STR                          
125400        DISPLAY  ERROR-TEXT-STR                                           
125500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
125600     END-IF                                                               
125700                                                                          
125800     MOVE SPACE       TO KOM-IO-AREA                                      
125900                                                                          
126000     PERFORM S04-UPPDATERA-CROSS                                          
126100                                                                          
126200     PERFORM S06-UPPDATERA-BENREG                                         
126300                                                                          
126400     PERFORM S07-UPPDATERA-NYPON                                          
126500                                                                          
126600     PERFORM S08-HTR-2221-IDLEVNR-BYTE                                    
126700     .                                                                    
126800 Z-FINIT SECTION.                                                         
126900     CLOSE W16150                                                         
127000           W16151                                                         
127100                                                                          
127200     MOVE 'S' TO POSTSUM-OPKOD                                            
127300     CALL POSTSUM USING POSTSUM-PARM                                      
127400     .                                                                    
127500 S01-READ-W16150 SECTION.                                                 
127600     READ W16150 INTO IN-AREA                                             
127700     AT END                                                               
127800        SET END-OF-W16150 TO TRUE                                         
127900                                                                          
128000     NOT AT END                                                           
128100        MOVE 'W16150  ' TO POSTSUM-FDNAMN                                 
128200        MOVE 'W16150D1' TO POSTSUM-DDNAMN2                                
128300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
128400        CALL POSTSUM USING POSTSUM-PARM                                   
128500     END-READ                                                             
128600     .                                                                    
128700                                                                          
128800 S02-WRITE-PROCESS-RESULT SECTION.                                        
128900     WRITE WS-REC                                                         
129000                                                                          
129100     MOVE SPACE TO POSTSUM-TRANSTYP                                       
129200     MOVE 'W16150 ' TO POSTSUM-FDNAMN                                     
129300     MOVE 'W16150D2' TO POSTSUM-DDNAMN2                                   
129400     CALL POSTSUM USING POSTSUM-PARM                                      
129500     .                                                                    
129600 S04-UPPDATERA-CROSS SECTION.                                             
129700     MOVE ALL '+' TO PROG-MID                                             
129800     MOVE IN-IDLEVNR     TO PROG-MID-IDLEVNR-UP                           
129900     MOVE IN-BELEV       TO PROG-MID-BELEVART-UP                          
130000                                                                          
130100     MOVE SPACE          TO PROG-MID-IDARTNR-UT                           
130200                            PROG-MID-IDLEVNR-UT                           
130300                            PROG-MID-BELEVART-UT                          
130400                                                                          
130500     MOVE WS-IDARTNR-NUM TO PROG-MID-IDARTNR-IN                           
130600                            PROG-MID-IDARTNR-UP                           
130700                                                                          
130800     MOVE +9             TO PROG-MID-KDFTAG-UP                            
130900     MOVE +1             TO PROG-MID-IDBENR-UP                            
131000     MOVE 'J'            TO PROG-MID-FLTLVM-UP                            
131100                                                                          
131200     PERFORM IMS-INSERT-ALTMSG                                            
131300     .                                                                    
131400 S06-UPPDATERA-BENREG SECTION.                                            
131500*****************************************************************         
131600*   ÄT NOV 92  VID IDSKYLT GB GODKÄNNES BARA NAMNLEX. ARTIKEL   *         
131700*              REGISTRERAS PÅ FÖRSTA FUNNA NAMNLEXBENÄMNING     *         
131800*              FLFELHOMO SÄTTS TILL NEJ                         *         
131900*              GÄLLER INTE VID KOPIERING AV BENÄMNING           *         
132000*   ÄT OKT 02  VID ALL FÖRÄNDRING AV NÅGOT SEGMENT PÅ WDD3,     *         
132100*              SKALL FLAENDR SÄTTAS TILL JA PÅ WDD301           *         
132200*              (FLAENDR NEJ-SÄTTS I W159D2) "NEVIS-RUTIN"       *         
132300*      OKT 05  FLAENDR SKALL EJ LÄNGRE SÄTTAS TILL JA           *         
132400*****************************************************************         
132500*                                                                         
132600     SKIP2                                                                
132700     MOVE 'SE '              TO W-IDSKYLT                                 
132800     MOVE FUNCTION UPPER-CASE(IN-BEART) TO W-BEART                        
132900     PERFORM IMS-GET-FIRST-WDD301-ASEQ                                    
133000     PERFORM UNTIL SEGMENT-SAKNAS OR BEN-KDHOMONYM = 0                    
133100        PERFORM IMS-GET-NEXT-WDD301-ASEQ                                  
133200     END-PERFORM                                                          
133300                                                                          
133400     IF SEGMENT-FINNS                                                     
133500         MOVE BEN-IDBENNR TO W-IDBENNR                                    
133600******** OM BENÄMNING FINNS PÅ BENÄMNINGSREGISTRET                        
133700        PERFORM S062-UPPDATERA-UTAN-HOM                                   
133800*         display ' Ny Artikel på ' W-IDBENNR ' ' W-BEART                 
133900     ELSE                                                                 
134000        PERFORM S061-REGISTRERA-BENREG                                    
134100*         display ' Nytt BenNr ' ws-nytt-nummer                           
134200*         display ' Lägger upp GB-ben ' W-BEART                           
134300     END-IF                                                               
134400     .                                                                    
134500     EJECT                                                                
134600 S061-REGISTRERA-BENREG SECTION.                                          
134700     PERFORM IMS-GET-WDG201                                               
134800     PERFORM IMS-GET-WDG202                                               
134900     ADD +1 TO XXAI-1208-IDBENNR                                          
135000     IF XXAI-1208-IDBENNR > XXAI-1208-IDBENNR-MAX                         
135100       MOVE +1 TO XXAI-1208-IDBENNR                                       
135200     END-IF                                                               
135300     MOVE XXAI-1208-IDBENNR  TO W-IDBENNR                                 
135400                                XXAI-1208-IDBENNR                         
135500     PERFORM IMS-REPL-WDG202                                              
135600                                                                          
135700     MOVE XXAI-1208-IDBENNR TO BEN-IDBENNR                                
135800     MOVE ZERO              TO BEN-KDHOMONYM                              
135900     MOVE ZERO              TO BEN-TIUPPDAT-STOP                          
136000     MOVE ZERO              TO BEN-KDBENSTAT                              
136100     MOVE NEJ               TO BEN-FLAENDR                                
136200     PERFORM IMS-ISRT-WDD301                                              
136300     IF SEGMENT-FINNS                                                     
136400       ADD +1 TO CHKP-ANT                                                 
136500     END-IF                                                               
136600                                                                          
136700     MOVE +1 TO TAB-IX                                                    
136800     PERFORM UNTIL TAB-IX > MAX-TAB-IDSKYLT                               
136900       MOVE TAB-IDSKYLT(TAB-IX) TO TEXT-IDSKYLT                           
137000       MOVE SPACE               TO TEXT-BEARTEXT                          
137100       MOVE DAGENS-DATUM        TO TEXT-TIUPPDAT                          
137200       IF TEXT-IDSKYLT = 'S  ' OR 'GB '                                   
137300         MOVE JA                TO TEXT-FLOVERSATT                        
137400         MOVE FUNCTION UPPER-CASE(IN-BEART) TO TEXT-BEART                 
137500         MOVE IN-BEART          TO TEXT-BEART                             
137600       ELSE                                                               
137700          MOVE NEJ              TO TEXT-FLOVERSATT                        
137800       END-IF                                                             
137900       PERFORM IMS-ISRT-WDD311                                            
138000       IF SEGMENT-FINNS                                                   
138100         ADD +2 TO CHKP-ANT                                               
138200       END-IF                                                             
138300       ADD +1 TO TAB-IX                                                   
138400     END-PERFORM                                                          
138500*                                                                         
138600     MOVE SPACE          TO REV-TETEXT                                    
138700     MOVE FUNCTION UPPER-CASE(IN-BEART) TO REV-TETEXT                     
138800     MOVE IN-BEART       TO REV-TETEXT                                    
138900     CALL WREVERSE USING REV-TETEXT                                       
139000     MOVE SPACE          TO TEXT-BEARTEXT                                 
139100     MOVE REV-TETEXT     TO TEXT-BEART                                    
139200     MOVE DAGENS-DATUM   TO TEXT-TIUPPDAT                                 
139300     MOVE JA TO TEXT-FLOVERSATT                                           
139400                                                                          
139500* INSERT DESCRIPTION IN SWEDISH                                           
139600     MOVE '  S'          TO TEXT-IDSKYLT                                  
139700     PERFORM IMS-ISRT-WDD311                                              
139800     IF SEGMENT-FINNS                                                     
139900       ADD +2 TO CHKP-ANT                                                 
140000     END-IF                                                               
140100                                                                          
140200* INSERT DESCRIPTION FOR GB (SAME SWEDISH DESC)                           
140300     MOVE ' BG'          TO TEXT-IDSKYLT                                  
140400     PERFORM IMS-ISRT-WDD311                                              
140500     IF SEGMENT-FINNS                                                     
140600       ADD +2 TO CHKP-ANT                                                 
140700     END-IF                                                               
140800*                                                                         
140900     MOVE WS-IDARTNR-NUM  TO WDD312-ART-IDARTNR                           
141000     MOVE NEJ             TO WDD312-ART-FLFELHOMO                         
141100     PERFORM IMS-ISRT-WDD312                                              
141200     IF SEGMENT-FINNS                                                     
141300       ADD +2 TO CHKP-ANT                                                 
141400     END-IF                                                               
141500                                                                          
141600     .                                                                    
141700 S062-UPPDATERA-UTAN-HOM SECTION.                                         
141800     MOVE WS-IDARTNR-NUM     TO W-IDARTNR                                 
141900     MOVE WS-IDARTNR-NUM     TO WDD312-ART-IDARTNR                        
142000     MOVE NEJ                TO WDD312-ART-FLFELHOMO                      
142100     PERFORM IMS-ISRT-WDD312                                              
142200     IF SEGMENT-FINNS                                                     
142300       ADD +2 TO CHKP-ANT                                                 
142400     END-IF                                                               
142500     .                                                                    
142600 S07-UPPDATERA-NYPON SECTION.                                             
142700     MOVE 'BIMA'             TO NYPON-ART-IDPROJ                          
142800                                NYPON-ART-IDAO                            
142900     MOVE W-EAN-CODE         TO NYPON-ART-TEARTNOT                        
143000     MOVE SPACE              TO NYPON-ART-FLAENDR                         
143100                                NYPON-ART-FLBASL                          
143200                                NYPON-ART-FLBERQ                          
143300                                NYPON-ART-FLRITB                          
143400                                NYPON-ART-FLRITC                          
143500                                NYPON-ART-FLRITP                          
143600                                NYPON-ART-KDARTUTG                        
143700                                NYPON-ART-FLUPG                           
143800                                NYPON-ART-KDTPD                           
143900                                NYPON-ART-FLUNIKRD                        
144000                                NYPON-ART-IDMATKTO                        
144100                                NYPON-ART-IDPROJOBJ                       
144200                                NYPON-ART-IDRITUTG                        
144300                                NYPON-ART-KDARTTYP                        
144400                                NYPON-ART-KDRESBED                        
144500                                NYPON-ART-TETEKNIK                        
144600                                NYPON-ART-IDSTEKN                         
144700                                NYPON-ART-TEANSINK                        
144800                                NYPON-ART-TEARTNOT-BASL                   
144900                                NYPON-ART-IDPROENH                        
145000                                NYPON-ART-IDPROJK                         
145100                                NYPON-ART-IDRITN                          
145200                                NYPON-ART-TEORSAK                         
145300                                NYPON-ART-IDLEVNR                         
145400                                NYPON-ART-IDLEVNR-FORB(1)                 
145500                                NYPON-ART-IDLEVNR-FORB(2)                 
145600                                NYPON-ART-IDLEVNR-FORB(3)                 
145700                                NYPON-ART-IDLEVNR-FORB(4)                 
145800                                NYPON-ART-IDLEVNR-FORB(5)                 
145900                                NYPON-ART-TEANSINK                        
146000                                NYPON-ART-FLUPB                           
146100                                NYPON-ART-FLPLAKOP                        
146200                                NYPON-ART-IDINK                           
146300     MOVE ZERO               TO NYPON-ART-IDAVD                           
146400                                NYPON-ART-IDANSK-REG                      
146500                                NYPON-ART-KDSTAINK                        
146600                                NYPON-ART-KVARTAR1                        
146700                                NYPON-ART-KVARTAR2                        
146800                                NYPON-ART-KVARTAR3                        
146900                                NYPON-ART-KVBASL                          
147000                                NYPON-ART-KVLEVBEG                        
147100                                NYPON-ART-KVPROG                          
147200                                NYPON-ART-KVUPB                           
147300                                NYPON-ART-PRARTBES                        
147400                                NYPON-ART-DABASL                          
147500                                NYPON-ART-TILEVBEG                        
147600                                NYPON-ART-TINEDBRY                        
147700                                NYPON-ART-TIPLAKOP                        
147800                                NYPON-ART-TIREGDAT                        
147900                                NYPON-ART-TIRITB                          
148000                                NYPON-ART-TIRITC                          
148100                                NYPON-ART-TIRITP                          
148200                                NYPON-ART-TISERLEV(1)                     
148300                                NYPON-ART-TISERLEV(2)                     
148400                                NYPON-ART-TISERLEV(3)                     
148500                                NYPON-ART-TISERLEV(4)                     
148600                                NYPON-ART-TISERLEV(5)                     
148700                                NYPON-ART-TISLUBER                        
148800                                NYPON-ART-TISTABER                        
148900                                NYPON-ART-TISTOMREG                       
149000                                NYPON-ART-TIUPPDAT                        
149100                                NYPON-ART-TIUPB                           
149200                                NYPON-ART-TITPD                           
149300                                NYPON-ART-TIUPG                           
149400                                NYPON-ART-TIMOTSI                         
149500                                NYPON-ART-IDARTNR-MOTSV                   
149600                                NYPON-ART-IDBERED                         
149700                                NYPON-ART-KVARTVAGN                       
149800                                NYPON-ART-DAFINLEV                        
149900                                NYPON-ART-TILEVBEG                        
150000                                NYPON-ART-KVLEVBEG                        
150100                                NYPON-ART-KVPROG                          
150200                                NYPON-ART-IDINKTEK                        
150300                                                                          
150400     MOVE W-IDARTNR          TO NYPON-ART-IDARTNR                         
150500     MOVE ART-IDLEVNR        TO NYPON-ART-IDLEVNR                         
150600     MOVE CLAG-IDANSK        TO NYPON-ART-IDANSK                          
150700     MOVE CLAG-IDINK         TO NYPON-ART-IDINK                           
150800     MOVE ART-KDSORT         TO NYPON-ART-KDSORT                          
150900     MOVE ART-IDFKNGRP       TO NYPON-ART-IDFKNGRP                        
151000     MOVE DAGENS-DATUM       TO NYPON-ART-TIINKOP                         
151100                                NYPON-ART-TIANSKREG                       
151200     MOVE ART-KDPRODSL       TO NYPON-ART-KDPRODSL                        
151300     MOVE FUNCTION UPPER-CASE(IN-BEART)                                   
151400                             TO NYPON-ART-BEART-SVE                       
151500     MOVE 2                  TO NYPON-ART-KDANSKQ                         
151600     MOVE 'R'                TO NYPON-ART-KDKOPTYP                        
151700     MOVE NEJ                TO NYPON-ART-FLPISK                          
151800                                NYPON-ART-FLBYTES                         
151900                                                                          
152000     PERFORM IMS-ISRT-WDD201                                              
152100     IF SEGMENT-FINNS                                                     
152200       ADD +5 TO CHKP-ANT                                                 
152300     END-IF                                                               
152400     .                                                                    
152500 S08-HTR-2221-IDLEVNR-BYTE SECTION.                                       
152600     MOVE SPACE              TO 2222-WDGX2222                             
152700     MOVE W-IDARTNR          TO 2222-IDARTNR                              
152800     MOVE ART-IDLEVNR        TO 2222-IDLEVNR                              
152900     MOVE 'W201'             TO 2222-IDSYSTEM                             
153000                                                                          
153100     PERFORM IMS-ISRT-WDG303                                              
153200     IF SEGMENT-FINNS                                                     
153300       ADD +1 TO CHKP-ANT                                                 
153400     END-IF                                                               
153500     .                                                                    
153600     EJECT                                                                
153700 X-TAKE-CHECKPOINT   SECTION.                                             
153800* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
153900* --- SAVE DATABASE KEYS IF NECESSARY                                     
154000     PERFORM IMS-CHECKPOINT                                               
154100     MOVE ZERO TO CHKP-ANT                                                
154200* --- REREAD DATABASE IF NECESSARY                                        
154300     .                                                                    
154400* --- IMS SEKTIONER ---                                                   
154500 IMS-INSERT-ALTMSG  SECTION.                                              
154600     MOVE SPACE TO GODK-STATUSKODER                                       
154700     CALL CBLTDLI USING PURG ALT-PCB W-PROG-TO-PROG-SW                    
154800     MOVE  ALT-STATUS-CODE TO STATUS-WS                                   
154900     PERFORM IMS-STATUSKONTROLL                                           
155000     .                                                                    
155100 IMS-GET-WDG201 SECTION.                                                  
155200     STRING 'WDG201  (WDGXKEY  =' W-1207-KEY-X ')'                        
155300            DELIMITED BY SIZE INTO SSA1                                   
155400     MOVE '  ' TO GODK-STATUSKODER                                        
155500     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDG201   SSA1                  
155600     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
155700     PERFORM IMS-STATUSKONTROLL                                           
155800     .                                                                    
155900 IMS-GET-WDG202 SECTION.                                                  
156000     MOVE 'WDG202 ' TO SSA1                                               
156100     MOVE '  ' TO GODK-STATUSKODER                                        
156200     CALL CBLTDLI USING GHNP WDG2-PCB DLI-IO-WDG202   SSA1                
156300     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
156400     PERFORM IMS-STATUSKONTROLL                                           
156500     .                                                                    
156600 IMS-REPL-WDG202 SECTION.                                                 
156700     MOVE '  ' TO GODK-STATUSKODER                                        
156800     CALL CBLTDLI USING REPL WDG2-PCB DLI-IO-WDG202                       
156900     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
157000     PERFORM IMS-STATUSKONTROLL                                           
157100     .                                                                    
157200 IMS-GET-FIRST-WDD301-ASEQ SECTION.                                       
157300     STRING 'WDD301  *F(WDD3ASEQ =' W-IDSKYLT-X                           
157400                      W-BEART-X  ')'                                      
157500            DELIMITED BY SIZE INTO SSA1                                   
157600     MOVE '  GE' TO GODK-STATUSKODER                                      
157700     CALL CBLTDLI USING GU WDD3A-PCB DLI-IO-WDD301  SSA1                  
157800     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
157900     PERFORM IMS-STATUSKONTROLL                                           
158000     .                                                                    
158100 IMS-GET-NEXT-WDD301-ASEQ SECTION.                                        
158200     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X                             
158300                      W-BEART-X  ')'                                      
158400            DELIMITED BY SIZE INTO SSA1                                   
158500     MOVE '  GE' TO GODK-STATUSKODER                                      
158600     CALL CBLTDLI USING GN WDD3A-PCB DLI-IO-WDD301  SSA1                  
158700     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
158800     PERFORM IMS-STATUSKONTROLL                                           
158900     .                                                                    
159000 IMS-ISRT-WDD301 SECTION.                                                 
159100     MOVE 'WDD301   ' TO SSA1                                             
159200     MOVE '  ' TO GODK-STATUSKODER                                        
159300     CALL CBLTDLI USING ISRT WDD3-PCB DLI-IO-WDD301 SSA1                  
159400     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
159500     PERFORM IMS-STATUSKONTROLL                                           
159600     .                                                                    
159700 IMS-ISRT-WDD311 SECTION.                                                 
159800     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
159900            DELIMITED BY SIZE INTO SSA1                                   
160000     MOVE 'WDD311   ' TO SSA2                                             
160100     MOVE '  ' TO GODK-STATUSKODER                                        
160200     CALL CBLTDLI USING ISRT WDD3-PCB DLI-IO-WDD311 SSA1 SSA2             
160300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
160400     PERFORM IMS-STATUSKONTROLL                                           
160500     .                                                                    
160600 IMS-ISRT-WDD312 SECTION.                                                 
160700     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
160800            DELIMITED BY SIZE INTO SSA1                                   
160900     MOVE 'WDD312   ' TO SSA2                                             
161000     MOVE '  ' TO GODK-STATUSKODER                                        
161100     CALL CBLTDLI USING ISRT WDD3-PCB DLI-IO-WDD312 SSA1 SSA2             
161200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
161300     PERFORM IMS-STATUSKONTROLL                                           
161400     .                                                                    
161500 IMS-GU-WDK601 SECTION.                                                   
161600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
161700            DELIMITED BY SIZE INTO SSA1                                   
161800     MOVE '  GE' TO GODK-STATUSKODER                                      
161900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
162000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162100     PERFORM IMS-STATUSKONTROLL                                           
162200     .                                                                    
162300 IMS-ISRT-WDK601 SECTION.                                                 
162400     MOVE 'WDK601 ' TO SSA1                                               
162500     MOVE '  II' TO GODK-STATUSKODER                                      
162600     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK601 SSA1                  
162700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162800     PERFORM IMS-STATUSKONTROLL                                           
162900     .                                                                    
163000 IMS-ISRT-WDK611 SECTION.                                                 
163100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
163200            DELIMITED BY SIZE INTO SSA1                                   
163300     MOVE 'WDK611 ' TO SSA2                                               
163400     MOVE '  II' TO GODK-STATUSKODER                                      
163500     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK611 SSA1 SSA2             
163600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
163700     PERFORM IMS-STATUSKONTROLL                                           
163800     .                                                                    
163900 IMS-ISRT-WDK623 SECTION.                                                 
164000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
164100          DELIMITED BY SIZE INTO SSA1                                     
164200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
164300          DELIMITED BY SIZE INTO SSA2                                     
164400     MOVE 'WDK623 '  TO SSA3                                              
164500     MOVE '  II' TO GODK-STATUSKODER                                      
164600     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK623                       
164700                                       SSA1 SSA2 SSA3                     
164800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
164900     PERFORM IMS-STATUSKONTROLL                                           
165000     .                                                                    
165100 IMS-ISRT-WDK625 SECTION.                                                 
165200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
165300          DELIMITED BY SIZE INTO SSA1                                     
165400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
165500          DELIMITED BY SIZE INTO SSA2                                     
165600     MOVE 'WDK625 '  TO SSA3                                              
165700     MOVE '  II' TO GODK-STATUSKODER                                      
165800     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK625                       
165900                                       SSA1 SSA2 SSA3                     
166000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
166100     PERFORM IMS-STATUSKONTROLL                                           
166200     .                                                                    
166300 IMS-ISRT-WDT301 SECTION.                                                 
166400     MOVE 'WDT301   ' TO SSA1                                             
166500     MOVE '  II' TO GODK-STATUSKODER                                      
166600     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
166700     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
166800     PERFORM IMS-STATUSKONTROLL                                           
166900     .                                                                    
167000     SKIP3                                                                
167100 IMS-ISRT-WDT311 SECTION.                                                 
167200     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
167300          DELIMITED BY SIZE INTO SSA1                                     
167400     MOVE 'WDT311   ' TO SSA2                                             
167500     MOVE '  ' TO GODK-STATUSKODER                                        
167600     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
167700     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
167800     PERFORM IMS-STATUSKONTROLL                                           
167900     .                                                                    
168000     SKIP3                                                                
168100 IMS-ISRT-WDK613-EMB SECTION.                                             
168200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
168300          DELIMITED BY SIZE INTO SSA1                                     
168400     MOVE 'WDK613   ' TO SSA2                                             
168500     MOVE '  ' TO GODK-STATUSKODER                                        
168600     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
168700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
168800     PERFORM IMS-STATUSKONTROLL                                           
168900     .                                                                    
169000 IMS-ISRT-WDD201       SECTION.                                           
169100     STRING 'WDD201     '                                                 
169200             DELIMITED BY SIZE INTO SSA1                                  
169300     MOVE '  II' TO GODK-STATUSKODER                                      
169400     CALL CBLTDLI USING ISRT WDD2-PCB DLI-IO-WDD201 SSA1                  
169500     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
169600     PERFORM IMS-STATUSKONTROLL                                           
169700     .                                                                    
169800 IMS-ISRT-WDG303 SECTION.                                                 
169900     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-2221-X ')'                    
170000            DELIMITED BY SIZE INTO SSA1                                   
170100     MOVE 'WDG303   '     TO SSA2                                         
170200     MOVE '  '   TO GODK-STATUSKODER                                      
170300     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDG303 SSA1 SSA2             
170400     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
170500     PERFORM IMS-STATUSKONTROLL                                           
170600     .                                                                    
170700 IMS-GU-WDF101 SECTION.                                                   
170800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
170900            DELIMITED BY SIZE INTO SSA1                                   
171000     MOVE '  GE' TO GODK-STATUSKODER                                      
171100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
171200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
171300     PERFORM IMS-STATUSKONTROLL                                           
171400     .                                                                    
171500 IMS-GNP-WDF102 SECTION.                                                  
171600     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
171700            DELIMITED BY SIZE INTO SSA1                                   
171800     MOVE '  ' TO GODK-STATUSKODER                                        
171900     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF102 SSA1                   
172000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
172100     PERFORM IMS-STATUSKONTROLL                                           
172200     .                                                                    
173400 IMS-RESTART SECTION.                                                     
173500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
173600     MOVE '  ' TO GODK-STATUSKODER                                        
173700     CALL CBLTDLI USING XRST MSG-PCB                                      
173800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
173900                        CHKP-AREA-LENGTH CHKP-AREA                        
174000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
174100     PERFORM IMS-STATUSKONTROLL                                           
174200     .                                                                    
174300 IMS-CHECKPOINT SECTION.                                                  
174400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
174500     MOVE '  XD' TO GODK-STATUSKODER                                      
174600     CALL CBLTDLI USING CHKP MSG-PCB                                      
174700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
174800                        CHKP-AREA-LENGTH CHKP-AREA                        
174900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
175000     PERFORM IMS-STATUSKONTROLL                                           
175100                                                                          
175200     IF IMS-EJ-OK                                                         
175300       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO FELTEXT-STR1        
175400       DISPLAY FELTEXT                                                    
175500       CALL FELLOG                                                        
175600     END-IF                                                               
175700     .                                                                    
175800 IMS-STATUSKONTROLL SECTION.                                              
175900     SET STATUS-IX TO 1                                                   
176000     SEARCH GODK-STATUS                                                   
176100       AT END                                                             
176200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
176300           DELIMITED BY SIZE INTO FELTEXT                                 
176400         DISPLAY FELTEXT                                                  
176500         CALL FELLOG                                                      
176600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
176700         CONTINUE                                                         
176800     END-SEARCH                                                           
176900     .                                                                    
177000*    -COPY WY2000P1                                                       
