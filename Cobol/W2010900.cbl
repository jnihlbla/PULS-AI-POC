000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2010900.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   96/05/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERAR ORDERINGÅNG-BASEN FRÅN BILD                           
000900*      ( + UPPDATERAR VIA DISPATCH FRÅN BMP W23218 )                      
001000*        + UPPDATERAR MED MID2- FRÅN PPSW W4XX  M.FL.                     
001100*                                                                         
001200*        PROGRAMMET UPPDAT     WLOIGB                                     
001300*                              WLOIGA                                     
001400*                   LÄSER      WDK6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T109                                              
001800*        MID:         W2I10901                                            
001900*                     W2I10902                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O10901                                            
002300*                                                                         
002400*   ÄNDRINGAR:                                                            
002500*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002600*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002700*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002800*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002900*                                                                         
003000*        06-05     ÄNDRADE REGLER NÄR EN ORDER KAN FLYTTAS                
003100*                  TILL FLER DC.                           /G.K.          
003200*                                                                         
003300*      2011-11-24  E'TRACKER 10143271 CHINA  WAREHOUSE PROJECT-1          
003400*      2012-10-05  E'TRACKER 10143273 CHINA  LOCAL SOURCING               
003500*                                                                         
003600*      2014-04-09  E'TRACKER 10228775 RÄTTA UPPDATERING AV DC11           
003700*                  PÅ WDL711.                                             
003800*                                                                         
003900*      2016-01-19  E'TRACKER 10243132  KINA EXPORT 2015,                  
004000*                  KRAV 70023.UPPDATERA TIREFEFT PÅ WDK629 FÖR            
004100*                  KUNDORDERINGÅNG PÅ CDC (ART.=>IDDC-REF=71..)           
004200*                                                                         
004300*                                                                         
004400*                                                                         
004500                                                                          
004600 ENVIRONMENT DIVISION.                                                    
004700                                                                          
004800 DATA DIVISION.                                                           
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100*    -COPY WY2000W1                                                       
005200                                                                          
005300*    -COPY WY2000W3                                                       
005400                                                                          
005500 77  IDPGM                       PIC X(08)   VALUE 'W2010900'.            
005600                                                                          
005700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005900 77  CURRENT-SECTION             PIC X(30) VALUE SPACE.                   
006000 77  DBS-SECTION                 PIC X(30) VALUE SPACE.                   
006100                                                                          
006200 01  W-MED                       PIC X(20) VALUE 'EJ SATT VÄRDE'.         
006300                                                                          
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  PASSIV                      PIC X       VALUE 'P'.                   
006700 77  SW-MSGKOM                   PIC X       VALUE 'N'.                   
006800 77  SVAR-OK                     PIC X       VALUE 'J'.                   
006900 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
007000 01  WS-CURR-IDDC-REFERRAL       PIC X(2)    VALUE SPACE.                 
007100 01  WS-CURR-CLEARAREA.                                                   
007200     03 WS-CURR-IDDC             PIC X(2)    VALUE SPACE.                 
007300     03 WS-CURR-FLCLEAR          PIC X(1)    VALUE SPACE.                 
007400     03 WS-CURR-FLLF             PIC X(1)    VALUE SPACE.                 
007500                                                                          
007600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007700                                                                          
007800                                                                          
007900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008000     88  NYCKLAR-OK                          VALUE 'J'.                   
008100     88  NYCKLAR-FEL                         VALUE 'N'.                   
008200                                                                          
008300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008400     88  INDATA-OK                           VALUE 'J'.                   
008500     88  INDATA-FEL                          VALUE 'N'.                   
008600                                                                          
008700 77  KOLUMN-SW                   PIC X       VALUE 'J'.                   
008800     88  KOLUMN-OK                           VALUE 'J'.                   
008900     88  KOLUMN-FEL                          VALUE 'N'.                   
009000                                                                          
009100 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
009200     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
009300     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
009400                                                                          
009500 77  BAS-SW                      PIC X       VALUE 'J'.                   
009600     88  FINNS-PA-BAS                        VALUE 'J'.                   
009700                                                                          
009800 77  TILL-CDC-SW                 PIC X       VALUE 'N'.                   
009900     88  TILL-CDC                            VALUE 'J'.                   
010000     88  TILL-SDC                            VALUE 'N'.                   
010100                                                                          
010200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010300     88  EGEN-MID                            VALUE '2109'.                
010400     88  GODK-MID                            VALUE '2101' '2102'          
010500                                                   '2103' '2104'          
010600                                                   '2105' '2106'          
010700                                                   '2107' '2108'          
010800                                                   '2109'.                
010900     88  HELP-MID                            VALUE '0551'.                
011000 01  FILLER  PIC X(20)   VALUE ' ARBETSAREOR '.                           
011100     EJECT                                                                
011200 01  ARBETSAREOR.                                                         
011300     03  WS-IDARTNR             PIC 9(9).                                 
011400     03  WS-TIAAAAVV            PIC 9(6).                                 
011500     03  FILLER REDEFINES WS-TIAAAAVV.                                    
011600         05  WS-SEKEL           PIC 9(2).                                 
011700         05  WS-TIAAVV          PIC 9(4).                                 
011800         05  FILLER REDEFINES WS-TIAAVV.                                  
011900             07  WS-TIAA        PIC 9(2).                                 
012000             07  WS-TIVV        PIC 9(2).                                 
012100     03  FILLER REDEFINES WS-TIAAAAVV.                                    
012200         05  WS-TIAAAA          PIC 9(4).                                 
012300         05  FILLER             PIC 9(2).                                 
012400     03  WS-TIAAAAVV-2          PIC 9(6).                                 
012500     03  FILLER REDEFINES WS-TIAAAAVV-2.                                  
012600         05  WS-SEKEL-2         PIC 9(2).                                 
012700         05  WS-TIAAVV-2        PIC 9(4).                                 
012800         05  FILLER REDEFINES WS-TIAAVV-2.                                
012900             07  WS-TIAA-2      PIC 9(2).                                 
013000             07  WS-TIVV-2      PIC 9(2).                                 
013100     03  FILLER REDEFINES WS-TIAAAAVV-2.                                  
013200         05  WS-TIAAAA-2        PIC 9(4).                                 
013300         05  FILLER             PIC 9(2).                                 
013400     03  WS-TIAAMMDD-2          PIC 9(6).                                 
013500     03  DAGENS-AAMMDD          PIC 9(6).                                 
013600     03  DAGENS-AARP            PIC 9(4).                                 
013700     03  DAGENS-AAAAVV          PIC 9(6).                                 
013800     03  FILLER REDEFINES DAGENS-AAAAVV.                                  
013900         05  DAGENS-SEKEL       PIC 9(2).                                 
014000         05  DAGENS-AAVV        PIC 9(4).                                 
014100         05  FILLER REDEFINES DAGENS-AAVV.                                
014200             07  DAGENS-AA      PIC 9(2).                                 
014300             07  DAGENS-VV      PIC 9(2).                                 
014400     03  DAGENS-D               PIC 9(1).                                 
014500     03  DAGENS-AAVV--1         PIC 9(4).                                 
014600     03  DAGENS-AAVV--2         PIC 9(4).                                 
014700     03  FILLER    OCCURS 21.                                             
014800         05  TAB-TIAAVVD        PIC 9(5).                                 
014900         05  FILLER             REDEFINES  TAB-TIAAVVD.                   
015000             07  TAB-TIAA       PIC 9(2).                                 
015100             07  TAB-TIVV       PIC 9(2).                                 
015200             07  TAB-TID        PIC 9(1).                                 
015300         05  FILLER             REDEFINES  TAB-TIAAVVD.                   
015400             07  FILLER         PIC 9(2).                                 
015500             07  TAB-TIVVD      PIC 9(3).                                 
015600         05  FILLER             REDEFINES  TAB-TIAAVVD.                   
015700             07  TAB-TIAAVV     PIC 9(4).                                 
015800             07  FILLER         PIC 9(1).                                 
015900     03  WS-TID                 PIC 9(1)  VALUE ZERO.                     
016000     03  WS-TID-2               PIC 9(1)  VALUE ZERO.                     
016100     03  WS-TIDD                PIC 9(2).                                 
016200     03  WS-NUM                 PIC S9(7) COMP-3.                         
016300     03  SPAR-PROG-DD           PIC S9(7) COMP-3.                         
016400     03  SPAR-DIV-DD            PIC S9(7) COMP-3.                         
016500     03  SPAR-SATS-DD           PIC S9(7) COMP-3.                         
016600     03  SPAR-SDC-DD            PIC S9(7) COMP-3.                         
016700     03  SPAR-NDC-DD            PIC S9(7) COMP-3.                         
016800     03  SPAR-LED-DD            PIC S9(7) COMP-3.                         
016900     03  SPAR-REF-DD            PIC S9(7) COMP-3.                         
017000     03  SUB-AAVV               PIC S9(5) COMP-3.                         
017100     03  SUB-ANTAL              PIC S9(3) COMP-3.                         
017200     03  WS-KDOI.                                                         
017300         05  WS-KDOI-1          PIC X.                                    
017400         05  FILLER             PIC X.                                    
017500                                                                          
017600     03  INDX                PIC S9(9)   VALUE ZERO  COMP SYNC.           
017700     03  INDX2               PIC S9(9)   VALUE ZERO  COMP SYNC.           
017800     03  IX1                 PIC S9(9)   VALUE ZERO  COMP SYNC.           
017900     03  IX2                 PIC S9(9)   VALUE ZERO  COMP SYNC.           
018000     03  IX                  PIC S9(9)   VALUE ZERO  COMP SYNC.           
018100     03  IX-MID              PIC S9(9)   VALUE ZERO  COMP SYNC.           
018200     03  DCIX                PIC S9(9)   VALUE ZERO.                      
018300     03  DCIX-MAX            PIC S9(9)   VALUE +7.                        
018400     03  FAKT                PIC S9(1) COMP-3.                            
018500     03  MINUS               PIC X(1)  VALUE '-'.                         
018600     03  IN-FAELT            PIC X(7).                                    
018700     03  IN-FAELT-NUM    REDEFINES IN-FAELT  PIC 9(7).                    
018800     03  W-KVOI-PROG-VV-IN      PIC S9(7) COMP-3.                         
018900     03  W-KVOI-DIV-VV-IN       PIC S9(7) COMP-3.                         
019000     03  W-KVOI-SATS-VV-IN      PIC S9(7) COMP-3.                         
019100     03  W-KVOI-SDC-VV-IN       PIC S9(7) COMP-3.                         
019200     03  W-KVOI-NDC-VV-IN       PIC S9(7) COMP-3.                         
019300     03  W-KVOI-LED-VV-IN       PIC S9(7) COMP-3.                         
019400     03  W-KVOI-REF-VV-IN       PIC S9(7) COMP-3.                         
019500     03  W-KVOI-PROG-DD-IN      PIC S9(7) COMP-3.                         
019600     03  W-KVOI-DIV-DD-IN       PIC S9(7) COMP-3.                         
019700     03  W-KVOI-SATS-DD-IN      PIC S9(7) COMP-3.                         
019800     03  W-KVOI-SDC-DD-IN       PIC S9(7) COMP-3.                         
019900     03  W-KVOI-NDC-DD-IN       PIC S9(7) COMP-3.                         
020000     03  W-KVOI-LED-DD-IN       PIC S9(7) COMP-3.                         
020100     03  W-KVOI-REF-DD-IN       PIC S9(7) COMP-3.                         
020200                                                                          
020300     03  IX-LEDT             PIC 9(2)    VALUE ZERO.                      
020400     EJECT                                                                
020500 01  WS-WDL8-SPAR.                                                        
020600     03  SPAR-WDL8-YYAAVV       PIC 9(6).                                 
020700     03  FILLER REDEFINES SPAR-WDL8-YYAAVV.                               
020800         05  FILLER              PIC 99.                                  
020900         05  SPAR-WDL8-AAVV      PIC 9(4).                                
021000     03  FILLER REDEFINES SPAR-WDL8-YYAAVV.                               
021100         05  SPAR-WDL8-YY        PIC 9(2).                                
021200         05  SPAR-WDL8-AA        PIC 99.                                  
021300         05  SPAR-WDL8-VV        PIC 99.                                  
021400     03  FILLER REDEFINES SPAR-WDL8-YYAAVV.                               
021500         05  SPAR-WDL8-YYAA      PIC 9(4).                                
021600         05  FILLER              PIC 99.                                  
021700 01  WS-KVOI                     PIC S9(7) COMP-3.                        
021800                                                                          
021900 01  WS-WDL8-FOERE.                                                       
022000     03  FOERE-KVOI-DIV          PIC S9(7) COMP-3.                        
022100     03  FOERE-KVOI-NDC          PIC S9(7) COMP-3.                        
022200     03  FOERE-KVOI-LED          PIC S9(7) COMP-3.                        
022300     03  FOERE-KVOI-PROG         PIC S9(7) COMP-3.                        
022400     03  FOERE-KVOI-SATS         PIC S9(7) COMP-3.                        
022500     03  FOERE-KVOI-SDC          PIC S9(7) COMP-3.                        
022600                                                                          
022700                                                                          
022800 01  WS-WDL8-EFTER.                                                       
022900     03  EFTER-KVOI-DIV          PIC S9(7) COMP-3.                        
023000     03  EFTER-KVOI-NDC          PIC S9(7) COMP-3.                        
023100     03  EFTER-KVOI-LED          PIC S9(7) COMP-3.                        
023200     03  EFTER-KVOI-PROG         PIC S9(7) COMP-3.                        
023300     03  EFTER-KVOI-SATS         PIC S9(7) COMP-3.                        
023400     03  EFTER-KVOI-SDC          PIC S9(7) COMP-3.                        
023500     EJECT                                                                
023600*01  -COPY WWDCKONS                                                       
023700     EJECT                                                                
023800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
023900 01  GENERELLA-SUBPROGRAM.                                                
024000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
024100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
024200     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
024300     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
024400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024600     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
024700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024800     03  W201SEND                PIC X(8)    VALUE 'W201SEND'.            
024900     EJECT                                                                
025000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
025100*01  -COPY WDATAREA                                                       
025200     EJECT                                                                
025300*    --- PARAMETRAR TILL SUBPROGRAM WDAGKONV                              
025400*01  -COPY WDAGAREA                                                       
025500     EJECT                                                                
025600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
025700*01 -COPY WMEDAREA                                                        
025800     EJECT                                                                
025900*    --- PARAMETRAR TILL SUBPROGRAM W005WDL7                              
026000*01 -COPY W005WDL7                                                        
026100     EJECT                                                                
026200*    --- PARAMETRAR TILL SUBPROGRAM W201SEND                              
026300*01 -COPY W201SEND                                                        
026400     SKIP3                                                                
026500 01  MESSAGE-CODES.                                                       
026600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
026700                                                                          
026800 01  MSG-KOM-MEDDELANDEN.                                                 
026900     03  FEL-ERR-FIELD           PIC X(3)    VALUE '001'.                 
027000     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
027100     03  FEL-UNREG-PART          PIC X(3)    VALUE '017'.                 
027200     03  FEL-OBSOLETED-PART      PIC X(3)    VALUE '018'.                 
027300     03  FEL-NOT-NUMERIC-PARTNO  PIC X(3)    VALUE '020'.                 
027400     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
027500     03  OK-GODKANT-FEL          PIC X(3)    VALUE '114'.                 
027600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
027700                                                                          
027800     EJECT                                                                
027900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
028000*                                                                         
028100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
028200     SKIP3                                                                
028300*01 -COPY WMSGINIT                                                        
028400     EJECT                                                                
028500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
028600*                                                                         
028700 01  FILLER                  PIC X(16)   VALUE 'MID-AREA'.                
028800                                                                          
028900 01  MID-AREA.                                                            
029000   03  MID                   PIC X(2000)   VALUE SPACE.                   
029100*  03  -COPY W2I10901 -RED MID                                            
029200                                                                          
029300     EJECT                                                                
029400*  03  -COPY W2I10902 -RED MID                                            
029500                                                                          
029600     EJECT                                                                
029700******************************************************************        
029800*                    M S G - A R E O R                           *        
029900******************************************************************        
030000*                                                                         
030100 01  IMS-WS-4.                                                            
030200     03  FILLER              PIC X(16)  VALUE 'MSG-KOM-AREA'.             
030300*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
030400*01  -COPY WMSGKOM                                                        
030500     EJECT                                                                
030600 01  FILLER                  PIC X(16)  VALUE 'MSG/MOD-AREA'.             
030700     SKIP3                                                                
030800*01  -COPY WMSGAREA                                                       
030900     EJECT                                                                
031000     03  MOD REDEFINES MSG-AREA.                                          
031100*      05  -COPY W2O10901                                                 
031200     EJECT                                                                
031300 01  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.                
031400*01  -COPY WMFSAREA                                                       
031500     EJECT                                                                
031600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031700*                                                                         
031800 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
031900                                                                          
032000 01  NYCKLAR-TILL-DLI.                                                    
032100     03  W-IDARTNR-X.                                                     
032200         05  W-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.               
032300     03  W-KDSEGKEY-X.                                                    
032400         05  W-KDSEGKEY      PIC X       VALUE '1'.                       
032500     03  W-IDDC-X.                                                        
032600         05  W-IDDC          PIC X(2)    VALUE ZERO.                      
032700     03  W-IDDC-B6-X.                                                     
032800         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
032900     03  W-IDDC-B616-X.                                                   
033000         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
033100     03  W-TIAAAA-X.                                                      
033200         05  W-TIAAAA-1-2    PIC 9(2)   VALUE 19.                         
033300         05  W-TIAAAA-3-4    PIC 9(2).                                    
033400     03  W-TIAAAA-2-X.                                                    
033500         05  W-TIAAAA2-1-2   PIC 9(2)   VALUE 19.                         
033600         05  W-TIAAAA2-3-4   PIC 9(2).                                    
033700     03  W-KDOI-X.                                                        
033800         05  W-KDOI          PIC S9(3)   VALUE ZERO COMP-3.               
033900     SKIP2                                                                
034000*    --- STATUS-KOD FRÅN IMS                                              
034100 01  STATUS-WS               PIC XX.                                      
034200     88  SEGMENT-FINNS                   VALUE '  '.                      
034300     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
034400     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
034500     SKIP2                                                                
034600 01  GODK-STATUSKODER.                                                    
034700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034800     SKIP3                                                                
034900 01  SSA1                    PIC X(64).                                   
035000 01  SSA2                    PIC X(64).                                   
035100 01  SSA3                    PIC X(64).                                   
035200     EJECT                                                                
035300*    --- IMS FUNKTIONSKODER                                               
035400*01  -COPY W0003                                                          
035500                                                                          
035600     EJECT                                                                
035700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OIGA11'.           
035800                                                                          
035900 01  DLI-IO-OIGA11.                                                       
036000*  03  -COPY WDL711  -PRE OIGA-                                           
036100                                                                          
036200     EJECT                                                                
036300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OIGB01'.           
036400                                                                          
036500 01  DLI-IO-OIGB01.                                                       
036600*  03  -COPY WDL801  -PRE OIGB-                                           
036700                                                                          
036800     EJECT                                                                
036900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OIGB11'.           
037000                                                                          
037100 01  DLI-IO-OIGB11.                                                       
037200*  03  -COPY WDL811  -PRE OIGB-                                           
037300                                                                          
037400     EJECT                                                                
037500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OIGB11-2'.         
037600                                                                          
037700 01  DLI-IO-OIGB11-2.                                                     
037800*  03  -COPY WDL811  -PRE OIGB2-                                          
037900                                                                          
038000     EJECT                                                                
038100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
038200                                                                          
038300 01  DLI-IO-WDK601.                                                       
038400*  03  -COPY WDK601.                                                      
038500                                                                          
038600     EJECT                                                                
038700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
038800 01  DLI-IO-WDK629.                                                       
038900*    03  -COPY WDK629                                                     
039000                                                                          
039100     EJECT                                                                
039200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
039300 01  DLI-IO-AREA-B601.                                                    
039400*    03  -COPY WDB601                                                     
039500                                                                          
039600     EJECT                                                                
039700 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
039800 01  DLI-IO-AREA-B616.                                                    
039900*    03  -COPY WDB616                                                     
040000                                                                          
040100     EJECT                                                                
040200 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
040300 01  DLI-IO-AREA-K711.                                                    
040400*    03  -COPY WDK711                                                     
040500                                                                          
040600     EJECT                                                                
040700 LINKAGE SECTION.                                                         
040800*01  -COPY W0009   -PRE MSG-                                              
040900                                                                          
041000*01  -COPY W0009   -PRE MSGKOM-                                           
041100     EJECT                                                                
041200*01  -COPY W0009   -PRE MAIL-                                             
041300     EJECT                                                                
041400*01  -COPY W0008   -PRE USEA-                                             
041500     05  FILLER                  PIC X.                                   
041600     EJECT                                                                
041700*01  -COPY W0008  -PRE OIGA-                                              
041800     05  FILLER                  PIC X.                                   
041900     EJECT                                                                
042000*01  -COPY W0008  -PRE OIGB-                                              
042100     05  FILLER                  PIC X.                                   
042200     EJECT                                                                
042300*01  -COPY W0008  -PRE OIGB2-                                             
042400     05  FILLER                  PIC X.                                   
042500     EJECT                                                                
042600*01  -COPY W0008  -PRE WDK6-                                              
042700     05  FILLER                  PIC X.                                   
042800     EJECT                                                                
042900*01  -COPY W0008  -PRE WDB6-                                              
043000     05  FILLER                  PIC X.                                   
043100     EJECT                                                                
043200*01  -COPY W0008  -PRE WDK7-                                              
043300     05  FILLER                  PIC X.                                   
043400                                                                          
043500     EJECT                                                                
043600 PROCEDURE DIVISION  USING MSG-PCB MSGKOM-PCB MAIL-PCB  USEA-PCB          
043700                          OIGA-PCB   OIGB-PCB OIGB2-PCB WDK6-PCB          
043800                          WDB6-PCB   WDK7-PCB.                            
043900 MAIN SECTION.                                                            
044000     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB MAIL-PCB  USEA-PCB          
044100                          OIGA-PCB   OIGB-PCB OIGB2-PCB WDK6-PCB          
044200                          WDB6-PCB   WDK7-PCB.                            
044300                                                                          
044410     PERFORM IMS-GET-MSG                                                  
044500     IF SEGMENT-FINNS                                                     
044600       PERFORM IMS-GET-WMSGKOM-MSG                                        
044700       PERFORM A-INIT                                                     
044800       IF MFS-UPD-X                                                       
044900         PERFORM C-UPPDAT-50                                              
045000         IF SW-MSGKOM = JA                                                
045100           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM + 17          
045200           PERFORM IMS-INSERT-WMSGKOM-MSG                                 
045300         END-IF                                                           
045400       ELSE                                                               
045500         PERFORM B-KOLLA-NYCKLAR                                          
045600         PERFORM SEC-URITY-CHECK                                          
045700         IF PASSED-SECURITY-CHECK                                         
045800             IF NYCKLAR-OK                                                
045900               IF MFS-UPDATE                                              
046000                 PERFORM G-KOLLA-INPUT                                    
046100                 IF INDATA-OK                                             
046200                   PERFORM H-UPPDATERA-WDL8                               
046300                   PERFORM F-LAES-VISA-INFO                               
046400                 END-IF                                                   
046500               ELSE                                                       
046600                 PERFORM F-LAES-VISA-INFO                                 
046700               END-IF                                                     
046800             END-IF                                                       
046900         ELSE                                                             
047000           CONTINUE                                                       
047100         END-IF                                                           
047200         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O10901 + 4                    
047300         PERFORM IMS-INSERT-MSG                                           
047400       END-IF                                                             
047500     END-IF                                                               
047600                                                                          
047700     MOVE ZERO TO RETURN-CODE                                             
047800     GOBACK                                                               
047900     .                                                                    
048000     EJECT                                                                
048100 A-INIT SECTION.                                                          
048200                                                                          
048300     IF MSG-DUBBLA-TRANSKODER                                             
048400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-AREA                     
048500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
048600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
048700     ELSE                                                                 
048800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-AREA                      
048900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
049000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
049100     END-IF                                                               
049200                                                                          
049300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
049400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
049500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
049600                                                                          
049700     MOVE LOW-VALUE TO MSG-AREA                                           
049800     MOVE 'W2O10901' TO MFS-IDMOD                                         
049900     MOVE '2109' TO MOD-IDTRANS                                           
050000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
050100                             MOD-TEMFSINF                                 
050200                                                                          
050300     IF EGEN-MID OR HELP-MID                                              
050400       CONTINUE                                                           
050500     ELSE                                                                 
050600       IF NOT MFS-UPD-X                                                   
050700         MOVE SPACE TO MFS-KDTRTYP                                        
050800         MOVE '7' TO MFS-IDPFK                                            
050900       END-IF                                                             
051000     END-IF                                                               
051100     EJECT                                                                
051200     MOVE 'IDAG  '     TO DAT-KDDATFORM                                   
051300     CALL WDATKONV USING  DAT-KDDATFORM                                   
051400                          DAT-I-TIDATUM                                   
051500                          DAT-O-TIDATUM                                   
051600                          DAT-KDSVAR                                      
051700     MOVE DAT-TIAAMMDD TO DAGENS-AAMMDD                                   
051800     MOVE DAT-TIAA     TO DAGENS-AA                                       
051900     MOVE DAT-TIVV     TO DAGENS-VV                                       
052000     MOVE DAT-TID      TO DAGENS-D                                        
052100     MOVE DAT-TIAARP   TO DAGENS-AARP                                     
052200     MOVE DAT-TISEKEL  TO DAGENS-SEKEL                                    
052300                                                                          
052400     MOVE DAGENS-AAVV  TO SUB-AAVV                                        
052500     MOVE -1           TO SUB-ANTAL                                       
052600     CALL W009VADD     USING SUB-AAVV SUB-ANTAL                           
052700     MOVE SUB-AAVV     TO DAGENS-AAVV--1                                  
052800                                                                          
052900     MOVE -1           TO SUB-ANTAL                                       
053000     CALL W009VADD     USING SUB-AAVV SUB-ANTAL                           
053100     MOVE SUB-AAVV     TO DAGENS-AAVV--2                                  
053200                                                                          
053300     MOVE 15           TO IX                                              
053400     PERFORM UNTIL IX > 21                                                
053500        COMPUTE TAB-TID (IX) = IX - 14                                    
053600        MOVE DAGENS-AAVV     TO TAB-TIAAVV (IX)                           
053700        ADD +1 TO IX                                                      
053800     END-PERFORM                                                          
053900                                                                          
054000     MOVE 8           TO IX                                               
054100     PERFORM UNTIL IX > 14                                                
054200        COMPUTE TAB-TID (IX) = IX - 7                                     
054300        MOVE DAGENS-AAVV--1  TO TAB-TIAAVV (IX)                           
054400        ADD +1 TO IX                                                      
054500     END-PERFORM                                                          
054600                                                                          
054700     MOVE 1           TO IX                                               
054800     PERFORM UNTIL IX > 7                                                 
054900        MOVE IX              TO TAB-TID (IX)                              
055000        MOVE DAGENS-AAVV--2  TO TAB-TIAAVV (IX)                           
055100        ADD +1 TO IX                                                      
055200     END-PERFORM                                                          
055300     .                                                                    
055400     EJECT                                                                
055500 B-KOLLA-NYCKLAR SECTION.                                                 
055600                                                                          
055700     MOVE JA TO NYCKLAR-SW                                                
055800                                                                          
055900     IF MFS-UPD-X                                                         
056000****************  DISPATCHANROP                                           
056100       IF MID-IDARTNR-IN = ALL '+'                                        
056200         MOVE MID-IDARTNR-UT TO WS-IDARTNR                                
056300         INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO               
056400       ELSE                                                               
056500         MOVE MID-IDARTNR-IN TO WS-IDARTNR                                
056600         MOVE '7'         TO MFS-IDPFK                                    
056700         IF NOT MFS-UPD-X                                                 
056800           MOVE SPACE       TO MFS-KDTRTYP                                
056900         END-IF                                                           
057000       END-IF                                                             
057100       MOVE WS-IDARTNR    TO W-IDARTNR                                    
057200     ELSE                                                                 
057300        MOVE ALL '+'           TO MSGI-WMSGINIT                           
057400        MOVE '001'             TO MSGI-KDCALL                             
057500        MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                       
057600        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
057700        MOVE '2109'            TO MSGI-IDTRANS                            
057800        IF GODK-MID                                                       
057900            MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                       
058000        END-IF                                                            
058100        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
058200****    -- KONTROLL AV IDARTNR                                            
058300        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                            
058400                                MOD-TIAAVV-IN                             
058500                                MOD-TID-IN                                
058600        IF MID-IDARTNR-IN NOT = ALL '+'                                   
058700          MOVE '7'         TO MFS-IDPFK                                   
058800          IF NOT MFS-UPD-X                                                
058900           MOVE SPACE       TO MFS-KDTRTYP                                
059000          END-IF                                                          
059100        END-IF                                                            
059200                                                                          
059300        MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                               
059400                                                                          
059500        INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO              
059600        IF MSGI-IDARTNR NUMERIC                                           
059700          MOVE MSGI-IDARTNR TO W-IDARTNR WS-IDARTNR                       
059800        ELSE                                                              
059900          MOVE NEJ TO NYCKLAR-SW                                          
060000        END-IF                                                            
060100     END-IF                                                               
060200                                                                          
060300     IF MID-TIAAVV-IN = ALL '+'                                           
060400        IF MID-TIAAVV-UT NUMERIC AND                                      
060500           MID-TIAAVV-UT > ZERO                                           
060600           MOVE MID-TIAAVV-UT TO WS-TIAAVV                                
060700           IF WS-TIVV = ZERO                                              
060800             MOVE NEJ TO NYCKLAR-SW                                       
060900           END-IF                                                         
061000        ELSE                                                              
061100           MOVE NEJ TO NYCKLAR-SW                                         
061200        END-IF                                                            
061300     ELSE                                                                 
061400        IF MID-TIAAVV-IN NUMERIC AND                                      
061500           MID-TIAAVV-IN > ZERO                                           
061600           MOVE MID-TIAAVV-IN TO WS-TIAAVV                                
061700           IF WS-TIVV = ZERO                                              
061800             MOVE NEJ TO NYCKLAR-SW                                       
061900           END-IF                                                         
062000        ELSE                                                              
062100           MOVE NEJ TO NYCKLAR-SW                                         
062200        END-IF                                                            
062300     END-IF                                                               
062400                                                                          
062500     IF WS-TIAAVV = DAGENS-AAVV     OR                                    
062600                    DAGENS-AAVV--1  OR                                    
062700                    DAGENS-AAVV--2                                        
062800        IF MID-TID-IN = ALL '+'                                           
062900           IF MID-TID-UT NUMERIC AND                                      
063000              MID-TID-UT > ZERO                                           
063100              MOVE MID-TID-UT TO WS-TID                                   
063200           ELSE                                                           
063300              MOVE NEJ TO NYCKLAR-SW                                      
063400           END-IF                                                         
063500        ELSE                                                              
063600           IF MID-TID-IN NUMERIC AND                                      
063700              MID-TID-IN > ZERO  AND                                      
063800              MID-TID-IN < '8'                                            
063900              MOVE MID-TID-IN TO WS-TID                                   
064000           ELSE                                                           
064100              MOVE NEJ TO NYCKLAR-SW                                      
064200           END-IF                                                         
064300        END-IF                                                            
064400     END-IF                                                               
064500                                                                          
064600     IF WS-TIAA < 50                                                      
064700        MOVE 20         TO WS-SEKEL                                       
064800     ELSE                                                                 
064900        MOVE 19         TO WS-SEKEL                                       
065000     END-IF                                                               
065100                                                                          
065200     MOVE WS-TIAAVV     TO TMP1-YYWW                                      
065300     MOVE DAGENS-AAVV   TO TMP2-YYWW                                      
065400     PERFORM WY2000P3                                                     
065500     IF TMP1-YYWW > TMP2-YYWW   OR                                        
065600       (WS-TIAAVV = DAGENS-AAVV AND                                       
065700        WS-TID    > DAGENS-D)   OR                                        
065800        WS-TIVV   > 53                                                    
065900           MOVE NEJ TO NYCKLAR-SW                                         
066000     END-IF                                                               
066100                                                                          
066200     IF GODK-MID OR NYCKLAR-OK                                            
066300       MOVE WS-IDARTNR      TO MOD-IDARTNR-UT                             
066400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
066500       MOVE WS-TIAAVV       TO MOD-TIAAVV-UT MOD-TIAAVV                   
066600       MOVE WS-TID          TO MOD-TID-UT    MOD-TID                      
066700       MOVE WS-TIVV         TO               MOD-TIVV                     
066800       INSPECT MOD-TID-UT REPLACING LEADING ZERO BY SPACE                 
066900       MOVE WS-TIAA         TO W-TIAAAA-3-4                               
067000       IF WS-TIAA < 50                                                    
067100          MOVE 20           TO W-TIAAAA-1-2                               
067200       ELSE                                                               
067300          MOVE 19           TO W-TIAAAA-1-2                               
067400       END-IF                                                             
067500     ELSE                                                                 
067600       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
067700                               MOD-TIAAVV-UT  MOD-TIAAVV                  
067800                               MOD-TID-UT     MOD-TID                     
067900                                              MOD-TIVV                    
068000     END-IF                                                               
068100                                                                          
068200     IF NYCKLAR-FEL                                                       
068300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL MSG-KOM-IDMFSMED                
068400       CALL WMEDKONV USING MED-WMEDAREA                                   
068500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
068600       PERFORM MFS-RENSA-FAELT-IN                                         
068700       PERFORM MFS-RENSA-FAELT-UT                                         
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 C-UPPDAT-50 SECTION.                                                     
069200                                                                          
069300*  W201SEND FIX START                                                     
069400*    MOVE SPACE            TO SEND-IDMAIL                                 
069500*    MOVE SPACE            TO SEND-W2I10902                               
069600*    MOVE ZERO             TO SEND-KVANTART                               
069700*    MOVE +1               TO IX-MID                                      
069800*    PERFORM UNTIL IX-MID > 18                                            
069900*      MOVE ZERO           TO SEND-IDARTNR    (IX-MID)                    
070000*      MOVE SPACE          TO SEND-IDDC       (IX-MID)                    
070100*      MOVE SPACE          TO SEND-KDTECKEN   (IX-MID)                    
070200*      MOVE SPACE          TO SEND-KDOI       (IX-MID)                    
070300*      MOVE SPACE          TO SEND-CLEARGROUP (IX-MID)                    
070400*      MOVE ZERO           TO SEND-KVOI       (IX-MID)                    
070500*      MOVE ZERO           TO SEND-TIUPPDAT   (IX-MID)                    
070600*      ADD +1              TO IX-MID                                      
070700*    END-PERFORM                                                          
070800*                                                                         
070900*    IF MID2-KVANTART NUMERIC                                             
071000*      MOVE MID2-KVANTART  TO SEND-KVANTART                               
071100*      MOVE +1             TO IX-MID                                      
071200*      PERFORM UNTIL IX-MID > 18                                          
071300*                 OR IX-MID > MID2-KVANTART                               
071400*        MOVE MID2-IDARTNR    (IX-MID) TO SEND-IDARTNR    (IX-MID)        
071500*        MOVE MID2-IDDC       (IX-MID) TO SEND-IDDC       (IX-MID)        
071600*        MOVE MID2-KDTECKEN   (IX-MID) TO SEND-KDTECKEN   (IX-MID)        
071700*        MOVE MID2-KDOI       (IX-MID) TO SEND-KDOI       (IX-MID)        
071800*        MOVE MID2-CLEARGROUP (IX-MID) TO SEND-CLEARGROUP (IX-MID)        
071900*        MOVE MID2-KVOI       (IX-MID) TO SEND-KVOI       (IX-MID)        
072000*        MOVE MID2-TIUPPDAT   (IX-MID) TO SEND-TIUPPDAT   (IX-MID)        
072100*        ADD +1 TO IX-MID                                                 
072200*      END-PERFORM                                                        
072300*    END-IF                                                               
072400*    W201SEND SKICKAR EN REDIGERAD LÄNKAREA TILL                          
072500*    ANGIVNA MAIL-MOTTAGRE VID FELSÖKNING                                 
072600*    CALL W201SEND USING SEND-W201SEND MAIL-PCB                           
072700*  W201SEND FIX END                                                       
072800                                                                          
072900     IF MID2-KVANTART NUMERIC                                             
073000         AND MID2-KVANTART > ZERO                                         
073100       MOVE +1 TO IX-MID                                                  
073200       PERFORM UNTIL IX-MID > 18                                          
073300                  OR IX-MID > MID2-KVANTART                               
073400         MOVE MID2-IDARTNR  (IX-MID)  TO WS-IDARTNR W-IDARTNR             
073500         MOVE MID2-IDDC     (IX-MID)  TO W-IDDC                           
073600         MOVE MID2-TIUPPDAT (IX-MID)  TO DAT-I-TIDATUM                    
073700         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
073800         CALL WDATKONV USING DAT-KDDATFORM                                
073900                             DAT-I-TIDATUM                                
074000                             DAT-O-TIDATUM                                
074100                             DAT-KDSVAR                                   
074200         MOVE DAT-TIAA                TO WS-TIAA                          
074300         MOVE DAT-TIVV                TO WS-TIVV                          
074400         MOVE DAT-TID                 TO WS-TID                           
074500         MOVE DAT-TIAA                TO W-TIAAAA-3-4                     
074600         MOVE DAT-TISEKEL             TO W-TIAAAA-1-2  WS-SEKEL           
074700                                                                          
074800         IF MID2-IDDC(IX-MID) NOT = DCS-IDDC                              
074900           MOVE MID2-IDDC(IX-MID)  TO W-IDDC-B6                           
075000           PERFORM IMS-GU-WDB601                                          
075100         END-IF                                                           
075200         IF MID2-KDOI(IX-MID) = 'XX' OR 'PP'                              
075300            PERFORM CD-UPPDATERA-REFERRAL                                 
075400            PERFORM CA-UPPDATERA-WDL8                                     
075500         ELSE                                                             
075600            IF MID2-KDOI(IX-MID) = 'CN' OR 'LO'                           
075700               PERFORM CD-UPPDATERA-REFERRAL                              
075800            ELSE                                                          
075900              IF MID2-KDOI(IX-MID) NOT = 'NN'                             
076000                IF (MID2-KDOI(IX-MID) = 'RE' AND DCS-NDC-CN) OR           
076100                   (MID2-KDOI(IX-MID) = 'RE' AND DCS-NDC-NA) OR           
076110                   (MID2-KDOI(IX-MID) = 'RE' AND DCS-NDC-PF)              
076200                  PERFORM CC-UPPDATERA-WDL7                               
076300                ELSE                                                      
076400                  PERFORM CA-UPPDATERA-WDL8                               
076500                  PERFORM CC-UPPDATERA-WDL7                               
076600                END-IF                                                    
076700              ELSE                                                        
076810                 PERFORM CC-UPPDATERA-WDL7                                
076900              END-IF                                                      
077000            END-IF                                                        
077100         END-IF                                                           
077200                                                                          
077300         ADD +1 TO IX-MID                                                 
077400       END-PERFORM                                                        
077500       MOVE OK-BEHANDLAD   TO MSG-KOM-IDMFSMED                            
077600     ELSE                                                                 
077700       MOVE OK-GODKANT-FEL TO MSG-KOM-IDMFSMED                            
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100 CA-UPPDATERA-WDL8    SECTION.                                            
078200     MOVE 'CA-UPPDATERA-WDL8 '   TO CURRENT-SECTION                       
078300                                                                          
078400*                            INDEX OCH NYCKLAR REDAN FIXADE               
078500                                                                          
078600     IF MID2-IDDC(IX-MID) NOT = DCS-IDDC                                  
078700       MOVE MID2-IDDC(IX-MID)  TO W-IDDC-B6                               
078800       PERFORM IMS-GU-WDB601                                              
078900     END-IF                                                               
079000                                                                          
079100     PERFORM IMS-GHU-WDL801                                               
079200     IF SEGMENT-FINNS                                                     
079300       IF WS-TIAAVV = DAGENS-AAVV    OR                                   
079400                      DAGENS-AAVV--1 OR                                   
079500                      DAGENS-AAVV--2                                      
079600*                            UPPDATERA PÅ ANGIVEN DAG  VV D               
079700         PERFORM CAB-UPPD-KVOI-ART                                        
079800         PERFORM IMS-REPL-WDL801                                          
079900       END-IF                                                             
080000     ELSE                                                                 
080100       PERFORM S01-INIT-WDL801                                            
080200       IF WS-TIAAVV = DAGENS-AAVV    OR                                   
080300                      DAGENS-AAVV--1 OR                                   
080400                      DAGENS-AAVV--2                                      
080500*                            UPPDATERA PÅ ANGIVEN DAG  VV D               
080600         PERFORM CAB-UPPD-KVOI-ART                                        
080700       END-IF                                                             
080800       PERFORM IMS-ISRT-WDL801                                            
080900       PERFORM IMS-GHU-WDL801                                             
081000     END-IF                                                               
081100                                                                          
081200     PERFORM IMS-GHNP-WDL811                                              
081300     IF SEGMENT-FINNS                                                     
081400       PERFORM CAA-UPPD-KVOI-AAR                                          
081500       PERFORM IMS-REPL-WDL811                                            
081600     ELSE                                                                 
081700       PERFORM S02-INIT-WDL811                                            
081800       PERFORM CAA-UPPD-KVOI-AAR                                          
081900       PERFORM IMS-ISRT-WDL811                                            
082000     END-IF                                                               
082100     .                                                                    
082200                                                                          
082300     EJECT                                                                
082400 CAA-UPPD-KVOI-AAR    SECTION.                                            
082500     MOVE 'CAA-UPPD-KVOI-AAR '  TO CURRENT-SECTION                        
082600                                                                          
082700     MOVE MID2-KVOI (IX-MID)     TO WS-NUM                                
082800     MOVE MID2-KDOI (IX-MID)     TO WS-KDOI                               
082900     MOVE WS-TIVV TO IX1                                                  
083000                                                                          
083100     IF WS-KDOI-1  = 'C' OR                                               
083200       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND TILL-CDC)                
083300       IF (WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                        
083400         (DCS-NDC OR (DCS-SDC AND DCS-CHINA))                             
083500         CONTINUE                                                         
083600       ELSE                                                               
083700         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
083800           SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-PROG (IX1)                 
083900         ELSE                                                             
084000           ADD  WS-NUM        TO OIGB-AAR-KVOI-PROG (IX1)                 
084100           ADD  +1            TO OIGB-AAR-KVOT-PROG (IX1)                 
084200         END-IF                                                           
084300       END-IF                                                             
084400     END-IF                                                               
084500                                                                          
084600     IF WS-KDOI-1  = 'D'                                                  
084700       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
084800         SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-DIV  (IX1)                   
084900       ELSE                                                               
085000         ADD  WS-NUM        TO OIGB-AAR-KVOI-DIV  (IX1)                   
085100         ADD  +1            TO OIGB-AAR-KVOT-DIV  (IX1)                   
085200       END-IF                                                             
085300     END-IF                                                               
085400                                                                          
085500     IF WS-KDOI-1  = 'K'                                                  
085600       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
085700         SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-SATS (IX1)                   
085800       ELSE                                                               
085900         ADD  WS-NUM        TO OIGB-AAR-KVOI-SATS (IX1)                   
086000         ADD  +1            TO OIGB-AAR-KVOT-SATS (IX1)                   
086100       END-IF                                                             
086200     END-IF                                                               
086300                                                                          
086400     IF WS-KDOI-1  = 'S' OR                                               
086500       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND TILL-SDC)                
086600       IF (WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                        
086700         (DCS-NDC OR (DCS-SDC AND DCS-CHINA))                             
086800         CONTINUE                                                         
086900       ELSE                                                               
087000         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
087100           SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-SDC  (IX1)                 
087200         ELSE                                                             
087300           ADD  WS-NUM        TO OIGB-AAR-KVOI-SDC  (IX1)                 
087400         END-IF                                                           
087500       END-IF                                                             
087600     END-IF                                                               
087700                                                                          
087800     IF WS-KDOI-1  = 'N' OR                                               
087900       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                          
088000        (DCS-NDC OR (DCS-SDC AND DCS-CHINA)))                             
088100                                                                          
088200       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
088300         SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-NDC  (IX1)                   
088400       ELSE                                                               
088500         ADD  WS-NUM        TO OIGB-AAR-KVOI-NDC  (IX1)                   
088600       END-IF                                                             
088700     END-IF                                                               
088800                                                                          
088900     IF WS-KDOI-1  = 'R'                                                  
089000       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
089100         SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-REFILL (IX1)                 
089200       ELSE                                                               
089300         ADD  WS-NUM        TO OIGB-AAR-KVOI-REFILL (IX1)                 
089400         ADD  +1            TO OIGB-AAR-KVOT-REFILL (IX1)                 
089500       END-IF                                                             
089600     END-IF                                                               
089700                                                                          
089800     IF WS-KDOI-1  = 'S' OR 'N' OR                                        
089900       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND TILL-SDC)                
090000       IF (WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                        
090100         (DCS-NDC OR (DCS-SDC AND DCS-CHINA))                             
090200         CONTINUE                                                         
090300       ELSE                                                               
090400         PERFORM CAAA-LEDTID-AAR                                          
090500       END-IF                                                             
090600     END-IF                                                               
090700     .                                                                    
090800     EJECT                                                                
090900 CAAA-LEDTID-AAR      SECTION.                                            
091000     MOVE 'CAAA-LEDTID-AAR '   TO CURRENT-SECTION                         
091100                                                                          
091200*    BERÄKNA AAAAVV FÖR DC I CDC                                          
091300                                                                          
091400*--- FÄLTET IDDC-REF ALLTID = CDC FÖR ALLA DC UTOM KINA.                  
091500                                                                          
091600     MOVE MID2-IDDC (IX-MID)    TO W-IDDC-B6                              
091700     MOVE WC-CDC-SE             TO W-IDDC-B616                            
091800     PERFORM IMS-GU-WDB616                                                
091900     IF SEGMENT-FINNS                                                     
092000                                                                          
092100        MOVE 3                      TO DAG-KDCALL                         
092200        MOVE MID2-TIUPPDAT (IX-MID) TO DAG-TIAAMMDD-TOM                   
092300        MOVE REF-KVDLTID-TOT        TO DAG-KVKALDAG                       
092400        ADD +1                      TO DAG-KVKALDAG                       
092500        IF MID2-TIUPPDAT (IX-MID) > 500000                                
092600           MOVE 19                  TO DAG-TISEKEL-TOM                    
092700        ELSE                                                              
092800           MOVE 20                  TO DAG-TISEKEL-TOM                    
092900        END-IF                                                            
093000                                                                          
093100        CALL WDAGKONV USING DAG-KDCALL                                    
093200                  DAG-DATUM-AREA DAG-KDSVAR                               
093300                                                                          
093400        IF DAG-KDSVAR = SPACE                                             
093500           MOVE DAG-TIAAMMDD-FOM    TO WS-TIAAMMDD-2                      
093600           MOVE DAG-TISEKEL-FOM     TO WS-SEKEL-2                         
093700                                                                          
093800           PERFORM CAAAA-OI-I-CDC-AAAA                                    
093900        END-IF                                                            
094000     END-IF                                                               
094100     .                                                                    
094200     EJECT                                                                
094300 CAAAA-OI-I-CDC-AAAA  SECTION.                                            
094400     MOVE 'CAAAA-OI-I-CDC-AAAA '  TO CURRENT-SECTION                      
094500                                                                          
094600     MOVE WS-TIAAMMDD-2           TO DAT-I-TIDATUM                        
094700     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
094800     CALL WDATKONV USING DAT-KDDATFORM                                    
094900                         DAT-I-TIDATUM                                    
095000                         DAT-O-TIDATUM                                    
095100                         DAT-KDSVAR                                       
095200     MOVE DAT-TIAA                TO WS-TIAA-2                            
095300     MOVE DAT-TIVV                TO WS-TIVV-2                            
095400                                                                          
095500     MOVE WS-TIVV-2               TO IX1                                  
095600     MOVE WS-TIAAAA-2             TO W-TIAAAA-2-X                         
095700                                                                          
095800     IF W-TIAAAA-X = W-TIAAAA-2-X                                         
095900*       UPPDATERING SKER I REDAN INLÄST SEGMENT                           
096000       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
096100         SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-LEDTID (IX1)                 
096200       ELSE                                                               
096300         ADD  WS-NUM        TO OIGB-AAR-KVOI-LEDTID (IX1)                 
096400       END-IF                                                             
096500     ELSE                                                                 
096600*       UPPDATERING SKER I ANNAT SEGMENT  (FINNS DETTA?)                  
096700       PERFORM IMS-GHU-WDL811-2                                           
096800       IF SEGMENT-FINNS                                                   
096900          IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                          
097000            SUBTRACT WS-NUM  FROM OIGB2-AAR-KVOI-LEDTID (IX1)             
097100          ELSE                                                            
097200            ADD  WS-NUM        TO OIGB2-AAR-KVOI-LEDTID (IX1)             
097300          END-IF                                                          
097400          PERFORM IMS-REPL-WDL811-2                                       
097500       ELSE                                                               
097600          PERFORM S22-INIT-WDL811-2                                       
097700          IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                          
097800            SUBTRACT WS-NUM  FROM OIGB2-AAR-KVOI-LEDTID (IX1)             
097900          ELSE                                                            
098000            ADD  WS-NUM        TO OIGB2-AAR-KVOI-LEDTID (IX1)             
098100          END-IF                                                          
098200          PERFORM IMS-ISRT-WDL811-2                                       
098300       END-IF                                                             
098400     END-IF                                                               
098500     .                                                                    
098600     EJECT                                                                
098700 CAB-UPPD-KVOI-ART    SECTION.                                            
098800     MOVE 'CAB-UPPD-KVOI-ART '  TO CURRENT-SECTION                        
098900                                                                          
099000     MOVE WS-TID TO IX1                                                   
099100     IF WS-TIAAVV = DAGENS-AAVV                                           
099200       ADD +14 TO IX1                                                     
099300     ELSE                                                                 
099400       IF WS-TIAAVV = DAGENS-AAVV--1                                      
099500         ADD +7 TO IX1                                                    
099600       END-IF                                                             
099700     END-IF                                                               
099800                                                                          
099900     MOVE MID2-KVOI (IX-MID)     TO WS-NUM                                
100000     MOVE MID2-KDOI (IX-MID)     TO WS-KDOI                               
100100                                                                          
100200     IF WS-KDOI-1  = 'C' OR                                               
100300       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND TILL-CDC)                
100400       IF (WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                        
100500         (DCS-NDC OR (DCS-SDC AND DCS-CHINA))                             
100600         CONTINUE                                                         
100700       ELSE                                                               
100800         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
100900           SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-PROG (IX1)                 
101000         ELSE                                                             
101100           ADD  WS-NUM        TO OIGB-ART-KVOI-PROG (IX1)                 
101200                                                                          
101300           PERFORM IMS-GHU-WDK629                                         
101400           IF SEGMENT-FINNS                                               
101500             MOVE DAGENS-AAMMDD TO CREF-TIREFEFT                          
101600             MOVE NEJ           TO CREF-FLREFNYO                          
101700             PERFORM IMS-REPL-WDK629                                      
101800           END-IF                                                         
101900         END-IF                                                           
102000       END-IF                                                             
102100     END-IF                                                               
102200                                                                          
102300     IF WS-KDOI-1  = 'D'                                                  
102400       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
102500         SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-DIV  (IX1)                   
102600       ELSE                                                               
102700         ADD  WS-NUM        TO OIGB-ART-KVOI-DIV  (IX1)                   
102800                                                                          
102900         PERFORM IMS-GHU-WDK629                                           
103000         IF SEGMENT-FINNS                                                 
103100           MOVE DAGENS-AAMMDD TO CREF-TIREFEFT                            
103200           MOVE NEJ           TO CREF-FLREFNYO                            
103300           PERFORM IMS-REPL-WDK629                                        
103400         END-IF                                                           
103500       END-IF                                                             
103600     END-IF                                                               
103700                                                                          
103800     IF WS-KDOI-1  = 'K'                                                  
103900       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
104000         SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-SATS (IX1)                   
104100       ELSE                                                               
104200         ADD  WS-NUM        TO OIGB-ART-KVOI-SATS (IX1)                   
104300       END-IF                                                             
104400     END-IF                                                               
104500                                                                          
104600     IF WS-KDOI-1  = 'S' OR                                               
104700       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND TILL-SDC)                
104800       IF (WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                        
104900         (DCS-NDC OR (DCS-SDC AND DCS-CHINA))                             
105000         CONTINUE                                                         
105100       ELSE                                                               
105200         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
105300           SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-SDC  (IX1)                 
105400         ELSE                                                             
105500           ADD  WS-NUM        TO OIGB-ART-KVOI-SDC  (IX1)                 
105600         END-IF                                                           
105700       END-IF                                                             
105800     END-IF                                                               
105900                                                                          
106000     IF WS-KDOI-1  = 'N' OR                                               
106100       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                          
106200        (DCS-NDC OR (DCS-SDC AND DCS-CHINA)))                             
106300                                                                          
106400       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
106500         SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-NDC  (IX1)                   
106600       ELSE                                                               
106700         ADD  WS-NUM        TO OIGB-ART-KVOI-NDC  (IX1)                   
106800       END-IF                                                             
106900     END-IF                                                               
107000                                                                          
107100     IF WS-KDOI-1  = 'R'                                                  
107200       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
107300         SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-REFILL (IX1)                 
107400       ELSE                                                               
107500         ADD  WS-NUM        TO OIGB-ART-KVOI-REFILL (IX1)                 
107600       END-IF                                                             
107700     END-IF                                                               
107800                                                                          
107900     IF WS-KDOI-1  = 'S' OR 'N' OR                                        
108000       ((WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND TILL-SDC)                
108100                                                                          
108200       IF (WS-KDOI-1 = 'X' OR WS-KDOI-1 = 'P') AND                        
108300         (DCS-NDC OR (DCS-SDC AND DCS-CHINA))                             
108400         CONTINUE                                                         
108500       ELSE                                                               
108600         PERFORM CABA-LEDTID-ROT                                          
108700       END-IF                                                             
108800     END-IF                                                               
108900     .                                                                    
109000     EJECT                                                                
109100 CABA-LEDTID-ROT      SECTION.                                            
109200     MOVE 'CABA-LEDTID-ROT '  TO CURRENT-SECTION                          
109300                                                                          
109400*    BERÄKNA AAAAVV FÖR DC I CDC                                          
109500                                                                          
109600*--- FÄLTET IDDC-REF ALLTID = CDC FÖR ALLA DC UTOM KINA.                  
109700                                                                          
109800     MOVE MID2-IDDC (IX-MID)        TO W-IDDC-B6                          
109900     MOVE WC-CDC-SE                 TO W-IDDC-B616                        
110000     PERFORM IMS-GU-WDB616                                                
110100     IF SEGMENT-FINNS                                                     
110200                                                                          
110300        MOVE 3                      TO DAG-KDCALL                         
110400        MOVE MID2-TIUPPDAT (IX-MID) TO DAG-TIAAMMDD-TOM                   
110500        MOVE REF-KVDLTID-TOT        TO DAG-KVKALDAG                       
110600        ADD +1                      TO DAG-KVKALDAG                       
110700        IF MID2-TIUPPDAT (IX-MID) > 500000                                
110800           MOVE 19                  TO DAG-TISEKEL-TOM                    
110900        ELSE                                                              
111000           MOVE 20                  TO DAG-TISEKEL-TOM                    
111100        END-IF                                                            
111200                                                                          
111300        CALL WDAGKONV USING DAG-KDCALL                                    
111400                  DAG-DATUM-AREA DAG-KDSVAR                               
111500                                                                          
111600        IF DAG-KDSVAR = SPACE                                             
111700           MOVE DAG-TIAAMMDD-FOM    TO  WS-TIAAMMDD-2                     
111800           MOVE DAG-TISEKEL-FOM     TO  WS-SEKEL-2                        
111900                                                                          
112000           PERFORM CABAA-OI-I-CDC-ROT                                     
112100        END-IF                                                            
112200     END-IF                                                               
112300     .                                                                    
112400     EJECT                                                                
112500 CABAA-OI-I-CDC-ROT   SECTION.                                            
112600     MOVE 'CABAA-OI-I-CDC-ROT '  TO CURRENT-SECTION                       
112700                                                                          
112800                                                                          
112900     MOVE WS-TIAAMMDD-2           TO DAT-I-TIDATUM                        
113000     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
113100     CALL WDATKONV USING DAT-KDDATFORM                                    
113200                         DAT-I-TIDATUM                                    
113300                         DAT-O-TIDATUM                                    
113400                         DAT-KDSVAR                                       
113500     MOVE DAT-TIAA                TO WS-TIAA-2                            
113600     MOVE DAT-TIVV                TO WS-TIVV-2                            
113700     MOVE DAT-TID                 TO WS-TID-2                             
113800                                                                          
113900     IF WS-TIAAVV-2 = DAGENS-AAVV    OR                                   
114000                      DAGENS-AAVV--1 OR                                   
114100                      DAGENS-AAVV--2                                      
114200*                            UPPDATERA PÅ ANGIVEN DAG  VV D               
114300                                                                          
114400        MOVE WS-TID-2 TO IX1                                              
114500        IF WS-TIAAVV-2 = DAGENS-AAVV                                      
114600          ADD +14 TO IX1                                                  
114700        ELSE                                                              
114800          IF WS-TIAAVV-2 = DAGENS-AAVV--1                                 
114900            ADD +7 TO IX1                                                 
115000          END-IF                                                          
115100        END-IF                                                            
115200                                                                          
115300       IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                             
115400         SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-LEDTID (IX1)                 
115500       ELSE                                                               
115600         ADD  WS-NUM        TO OIGB-ART-KVOI-LEDTID (IX1)                 
115700       END-IF                                                             
115800     END-IF                                                               
115900     .                                                                    
116000                                                                          
116100     EJECT                                                                
116200 CC-UPPDATERA-WDL7    SECTION.                                            
116300     MOVE 'CC-UPPDATERA-WDL7 '  TO CURRENT-SECTION                        
116400                                                                          
116500*                            INDEX OCH NYCKLAR REDAN FIXADE               
116600                                                                          
116700     IF MID2-IDDC (IX-MID)  NOT = DCS-IDDC                                
116800       MOVE MID2-IDDC (IX-MID)  TO W-IDDC-B6                              
116900       PERFORM IMS-GU-WDB601                                              
117000     END-IF                                                               
117100     MOVE MID2-KDOI (IX-MID)  TO WS-KDOI                                  
117200     IF WS-KDOI = 'C2' OR 'S2' OR 'N2'                                    
117300                       OR 'S1' OR 'N1' OR 'NN'                            
117400        OR WS-KDOI-1 = 'S'                                                
117500        OR (WS-KDOI-1 = 'C'                                               
117600        AND (DCS-SDC                                                      
117700        OR   DCS-NDC-NA                                                   
117900        OR   DCS-NDC-PF                                                   
117901        OR   DCS-NDC-SA                                                   
117910        OR   DCS-NDC-OTHERS))                                             
118000        OR (WS-KDOI = 'RE' AND DCS-NDC-CN)                                
118100        OR (WS-KDOI = 'RE' AND DCS-NDC-NA)                                
118110        OR (WS-KDOI = 'RE' AND DCS-NDC-PF)                                
118200                                                                          
118300*******IF MID2-TIUPPDAT (IX-MID) > DAGENS-AAMMDD - 10000                  
118400*******    AND MID2-TIUPPDAT (IX-MID) < DAGENS-AAMMDD + 2                 
118500       MOVE MID2-TIUPPDAT (IX-MID) TO TMP1-YYMMDD                         
118600       MOVE DAGENS-AAMMDD          TO TMP2-YYMMDD                         
118700       SUBTRACT 10000            FROM TMP2-YYMMDD                         
118800       MOVE DAGENS-AAMMDD          TO TMP3-YYMMDD                         
118900       ADD +2                      TO TMP3-YYMMDD                         
119000       PERFORM WY2000Q1                                                   
119100       IF    TMP1-YYMMDD > TMP2-YYMMDD                                    
119200         AND TMP1-YYMMDD < TMP3-YYMMDD                                    
119300         IF W-IDDC NOT = WC-CDC-SE                                        
119400           PERFORM IMS-GU-WDL711                                          
119500           IF SEGMENT-SAKNAS                                              
119600             MOVE ALL '+'   TO WDL7-W005WDL7                              
119700             MOVE W-IDARTNR TO WDL7-IDARTNR                               
119800             MOVE W-IDDC    TO WDL7-IDDC                                  
119900             CALL W005WDL7 USING WDL7-W005WDL7 OIGA-PCB                   
120000           END-IF                                                         
120100                                                                          
120200           PERFORM IMS-GHU-WDL711                                         
120300* NU SKA DETTA FINNAS EFTER W005WDL7                                      
120400           PERFORM CCA-UPPD-KVOI-DC                                       
120500           PERFORM IMS-REPL-WDL711                                        
120600         END-IF                                                           
120700       END-IF                                                             
120800     END-IF                                                               
120900     .                                                                    
121000                                                                          
121100     EJECT                                                                
121200 CCA-UPPD-KVOI-DC     SECTION.                                            
121300     MOVE 'CCA-UPPD-KVOI-DC '  TO CURRENT-SECTION                         
121400                                                                          
121500     MOVE MID2-KVOI (IX-MID)     TO WS-NUM                                
121600     MOVE MID2-KDOI (IX-MID)     TO WS-KDOI                               
121700     MOVE WS-TIVV TO IX1                                                  
121800     PERFORM S50-FIXA-IX2                                                 
121900                                                                          
122000     IF IX2 = +0                                                          
122100*                  RULLANDE (VECKA)                                       
122200       IF WS-KDOI    = 'C2' OR 'S2' OR 'N2'                               
122300          OR (WS-KDOI-1 = 'C'                                             
122400          AND (DCS-SDC                                                    
122500          OR   DCS-NDC-NA                                                 
122600          OR   DCS-NDC-PF                                                 
122601          OR   DCS-NDC-SA                                                 
122610          OR   DCS-NDC-OTHERS))                                           
122700         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
122800           SUBTRACT WS-NUM  FROM OIGA-DC-KVOI-CDC-RULL (IX1)              
122900           SUBTRACT +1      FROM OIGA-DC-KVOT-CDC-RULL (IX1)              
123000         ELSE                                                             
123100           ADD      WS-NUM  TO   OIGA-DC-KVOI-CDC-RULL (IX1)              
123200           ADD      +1      TO   OIGA-DC-KVOT-CDC-RULL (IX1)              
123300         END-IF                                                           
123400       END-IF                                                             
123500                                                                          
123600       IF (WS-KDOI    = 'RE' AND DCS-NDC-CN) OR                           
123700          (WS-KDOI    = 'RE' AND DCS-NDC-NA) OR                           
123701          (WS-KDOI    = 'RE' AND DCS-NDC-PF)                              
123800         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
123900           SUBTRACT WS-NUM    FROM OIGA-DC-KVOI-REF-RULL (IX1)            
124000           SUBTRACT +1        FROM OIGA-DC-KVOT-REF-RULL (IX1)            
124100         ELSE                                                             
124200           ADD      WS-NUM    TO   OIGA-DC-KVOI-REF-RULL (IX1)            
124300           ADD      +1        TO   OIGA-DC-KVOT-REF-RULL (IX1)            
124400         END-IF                                                           
124500       ELSE                                                               
124600         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
124700           SUBTRACT WS-NUM    FROM OIGA-DC-KVOI-RULL (IX1)                
124800           SUBTRACT +1        FROM OIGA-DC-KVOT-RULL (IX1)                
124900         ELSE                                                             
125000           ADD      WS-NUM    TO   OIGA-DC-KVOI-RULL (IX1)                
125100           ADD      +1        TO   OIGA-DC-KVOT-RULL (IX1)                
125200         END-IF                                                           
125300       END-IF                                                             
125400     ELSE                                                                 
125500*                  INNEVARANDE PERIOD (VECKA)                             
125600       IF WS-KDOI    = 'C2' OR 'S2' OR 'N2'                               
125700          OR (WS-KDOI-1 = 'C'                                             
125800          AND (DCS-SDC                                                    
125900          OR   DCS-NDC-NA                                                 
126000          OR   DCS-NDC-PF                                                 
126001          OR   DCS-NDC-SA                                                 
126010          OR   DCS-NDC-OTHERS))                                           
126100         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
126200           SUBTRACT WS-NUM  FROM OIGA-DC-KVOI-CDC-INNEV   (IX2)           
126300           SUBTRACT +1      FROM OIGA-DC-KVOT-CDC-INNEV   (IX2)           
126400         ELSE                                                             
126500           ADD      WS-NUM  TO   OIGA-DC-KVOI-CDC-INNEV   (IX2)           
126600           ADD      +1      TO   OIGA-DC-KVOT-CDC-INNEV   (IX2)           
126700         END-IF                                                           
126800       END-IF                                                             
126900                                                                          
127000       IF (WS-KDOI    = 'RE' AND DCS-NDC-CN) OR                           
127100          (WS-KDOI    = 'RE' AND DCS-NDC-NA) OR                           
127101          (WS-KDOI    = 'RE' AND DCS-NDC-PF)                              
127200         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
127300           SUBTRACT WS-NUM  FROM OIGA-DC-KVOI-REF-INNEV   (IX2)           
127400           SUBTRACT +1      FROM OIGA-DC-KVOT-REF-INNEV   (IX2)           
127500         ELSE                                                             
127600           ADD      WS-NUM  TO   OIGA-DC-KVOI-REF-INNEV   (IX2)           
127700           ADD      +1      TO   OIGA-DC-KVOT-REF-INNEV   (IX2)           
127800         END-IF                                                           
127900       ELSE                                                               
128000         IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                           
128100           SUBTRACT WS-NUM    FROM OIGA-DC-KVOI-INNEV (IX2)               
128200           SUBTRACT +1        FROM OIGA-DC-KVOT-INNEV (IX2)               
128300         ELSE                                                             
128400           ADD      WS-NUM    TO   OIGA-DC-KVOI-INNEV (IX2)               
128500           ADD      +1        TO   OIGA-DC-KVOT-INNEV (IX2)               
128600           MOVE DAGENS-AAMMDD TO OIGA-DC-TIREFEFT                         
128700           PERFORM S30-UPD-FLREFNYO-WDK711                                
128800         END-IF                                                           
128900       END-IF                                                             
129000     END-IF                                                               
129100     .                                                                    
129200                                                                          
129300     EJECT                                                                
129400 CD-UPPDATERA-REFERRAL SECTION.                                           
129500     MOVE 'CD-UPPDATERA-REFERRAL '  TO CURRENT-SECTION                    
129600                                                                          
129700     MOVE 1 TO DCIX                                                       
129800     MOVE MID2-IDDC-CLEAR(IX-MID, DCIX) TO WS-CURR-IDDC                   
129900     MOVE SPACE                         TO WS-CURR-IDDC-REFERRAL          
130000     MOVE MID2-FLCLEAR(IX-MID, DCIX)    TO WS-CURR-FLCLEAR                
130100     MOVE MID2-FLLF(IX-MID, DCIX)       TO WS-CURR-FLLF                   
130200     MOVE JA                            TO TILL-CDC-SW                    
130300                                                                          
130400     PERFORM CDA-UPPDATERA-WDL7                                           
130500                                                                          
130600     IF WS-CURR-FLLF = JA                                                 
130700        MOVE NEJ                     TO TILL-CDC-SW                       
130800        CONTINUE                                                          
130900     ELSE                                                                 
131000        ADD 1 TO DCIX                                                     
131100                                                                          
131200        PERFORM UNTIL DCIX > DCIX-MAX                                     
131300                   OR TILL-CDC-SW = NEJ                                   
131400                                                                          
131500           MOVE MID2-IDDC-CLEAR(IX-MID, DCIX)                             
131600                                        TO WS-CURR-IDDC                   
131700           MOVE MID2-IDDC-CLEAR(IX-MID, DCIX - 1)                         
131800                                        TO WS-CURR-IDDC-REFERRAL          
131900           MOVE MID2-FLCLEAR(IX-MID, DCIX)                                
132000                                        TO WS-CURR-FLCLEAR                
132100           MOVE MID2-FLLF(IX-MID, DCIX) TO WS-CURR-FLLF                   
132200                                                                          
132300           IF WS-CURR-CLEARAREA = SPACE                                   
132410              MOVE DCIX-MAX TO DCIX                                       
132500           ELSE                                                           
132600*             UPPDATERA WDL711 (WS-CURR-IDDC-REFERRAL)                    
132700*             SÄTT IDDC-REFERRAL = WS-CURR-IDDC                           
132800              MOVE WS-CURR-IDDC-REFERRAL TO W-IDDC                        
132900              PERFORM IMS-GHU-WDL711                                      
133000*             MOVE WS-CURR-IDDC       TO OIGA-DC-IDDC-REFERRAL            
133100              PERFORM IMS-REPL-WDL711                                     
133200                                                                          
133300              PERFORM CDA-UPPDATERA-WDL7                                  
133400                                                                          
133500              IF WS-CURR-FLLF = JA                                        
133600                 MOVE NEJ    TO TILL-CDC-SW                               
133700              END-IF                                                      
133800           END-IF                                                         
133900           ADD +1 TO DCIX                                                 
134000        END-PERFORM                                                       
134100     END-IF                                                               
134200     .                                                                    
134300                                                                          
134400     EJECT                                                                
134500 CDA-UPPDATERA-WDL7      SECTION.                                         
134600     MOVE 'CDA-UPPDATERA-WDL7 '  TO CURRENT-SECTION                       
134700                                                                          
134800*    IF NOT WS-CURR-FLLF = PASSIV                                         
134900        MOVE WS-CURR-IDDC       TO W-IDDC                                 
135000        IF W-IDDC NOT = WC-CDC-SE                                         
135100          PERFORM IMS-GU-WDL711                                           
135200          IF SEGMENT-SAKNAS                                               
135300            MOVE ALL '+'   TO WDL7-W005WDL7                               
135400            MOVE W-IDARTNR TO WDL7-IDARTNR                                
135500            MOVE W-IDDC    TO WDL7-IDDC                                   
135600            CALL W005WDL7 USING WDL7-W005WDL7 OIGA-PCB                    
135700          END-IF                                                          
135800                                                                          
135900          PERFORM IMS-GHU-WDL711                                          
136000* NU SKA DETTA FINNAS EFTER W005WDL7                                      
136100          PERFORM CDAA-UPPD-KVOI-KVOT-DC                                  
136200          PERFORM IMS-REPL-WDL711                                         
136300        END-IF                                                            
136400*    END-IF                                                               
136500     .                                                                    
136600                                                                          
136700     EJECT                                                                
136800 CDAA-UPPD-KVOI-KVOT-DC   SECTION.                                        
136900     MOVE 'CDAA-UPPD-KVOI-KVOT-DC '  TO CURRENT-SECTION                   
137000                                                                          
137100     MOVE WS-TIVV           TO IX1                                        
137200     PERFORM S50-FIXA-IX2                                                 
137300     MOVE MID2-KVOI(IX-MID) TO WS-NUM                                     
137400                                                                          
137500     IF IX2 = +0                                                          
137600*                  RULLANDE (VECKA)                                       
137700        IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                            
137800           SUBTRACT WS-NUM     FROM OIGA-DC-KVOI-RULL     (IX1)           
137900           SUBTRACT +1         FROM OIGA-DC-KVOT-RULL     (IX1)           
138000        ELSE                                                              
138100           ADD      WS-NUM     TO   OIGA-DC-KVOI-RULL     (IX1)           
138200           ADD      +1         TO   OIGA-DC-KVOT-RULL     (IX1)           
138300        END-IF                                                            
138400                                                                          
138500        IF WS-CURR-FLCLEAR = JA                                           
138600           IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                         
138700              SUBTRACT WS-NUM  FROM OIGA-DC-KVOI-CDC-RULL (IX1)           
138800              SUBTRACT +1      FROM OIGA-DC-KVOT-CDC-RULL (IX1)           
138900           ELSE                                                           
139000              ADD      WS-NUM  TO   OIGA-DC-KVOI-CDC-RULL (IX1)           
139100              ADD      +1      TO   OIGA-DC-KVOT-CDC-RULL (IX1)           
139200           END-IF                                                         
139300        END-IF                                                            
139400     ELSE                                                                 
139500*                  INNEVARANDE PERIOD (VECKA)                             
139600        IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                            
139700           IF MID2-KDOI (IX-MID) = 'PP'                                   
139800             SUBTRACT WS-NUM  FROM OIGA-DC-KVOI-PP-INNEV  (IX2)           
139900             SUBTRACT +1      FROM OIGA-DC-KVOT-PP-INNEV  (IX2)           
140000           ELSE                                                           
140100             SUBTRACT WS-NUM  FROM OIGA-DC-KVOI-INNEV     (IX2)           
140200             SUBTRACT +1      FROM OIGA-DC-KVOT-INNEV     (IX2)           
140300           END-IF                                                         
140400        ELSE                                                              
140500           IF MID2-KDOI (IX-MID) = 'PP'                                   
140600             ADD      WS-NUM    TO   OIGA-DC-KVOI-PP-INNEV(IX2)           
140700             ADD      +1        TO   OIGA-DC-KVOT-PP-INNEV(IX2)           
140800           ELSE                                                           
140900             ADD      WS-NUM    TO   OIGA-DC-KVOI-INNEV   (IX2)           
141000             ADD      +1        TO   OIGA-DC-KVOT-INNEV   (IX2)           
141100           END-IF                                                         
141200           MOVE DAGENS-AAMMDD TO OIGA-DC-TIREFEFT                         
141300           PERFORM S30-UPD-FLREFNYO-WDK711                                
141400        END-IF                                                            
141500                                                                          
141600        IF WS-CURR-FLCLEAR = JA                                           
141700           IF MID2-KDTECKEN (IX-MID) = '-' OR 'M'                         
141800              SUBTRACT WS-NUM FROM OIGA-DC-KVOI-CDC-INNEV (IX2)           
141900              SUBTRACT +1     FROM OIGA-DC-KVOT-CDC-INNEV (IX2)           
142000           ELSE                                                           
142100              ADD   WS-NUM    TO   OIGA-DC-KVOI-CDC-INNEV (IX2)           
142200              ADD   +1        TO   OIGA-DC-KVOT-CDC-INNEV (IX2)           
142300           END-IF                                                         
142400        END-IF                                                            
142500     END-IF                                                               
142600     .                                                                    
142700     EJECT                                                                
142800 F-LAES-VISA-INFO SECTION.                                                
142900                                                                          
143000     PERFORM FA-LAES-GRUNDDATA                                            
143100                                                                          
143200     IF SEGMENT-SAKNAS                                                    
143300        MOVE FEL-UNREG-PART TO MED-IDMFSFEL  MSG-KOM-IDMFSMED             
143400        CALL WMEDKONV USING MED-WMEDAREA                                  
143500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
143600        PERFORM MFS-RENSA-FAELT-UT                                        
143700     ELSE                                                                 
143800        MOVE WS-TIAAVV                 TO MOD-TIAAVV                      
143900        MOVE OIGB-AAR-KVOI-PROG (WS-TIVV) TO MOD-KVOI-PROG-VV-UT          
144000        MOVE OIGB-AAR-KVOI-DIV (WS-TIVV) TO MOD-KVOI-DIV-VV-UT            
144100        MOVE OIGB-AAR-KVOI-SATS (WS-TIVV) TO MOD-KVOI-SATS-VV-UT          
144200        MOVE OIGB-AAR-KVOI-SDC (WS-TIVV) TO MOD-KVOI-SDC-VV-UT            
144300        MOVE OIGB-AAR-KVOI-NDC (WS-TIVV) TO MOD-KVOI-NDC-VV-UT            
144400        MOVE OIGB-AAR-KVOI-LEDTID (WS-TIVV) TO MOD-KVOI-LED-VV-UT         
144500        MOVE OIGB-AAR-KVOI-REFILL (WS-TIVV) TO MOD-KVOI-REF-VV-UT         
144600                                                                          
144700        MOVE OIGB-AAR-KVOT-PROG (WS-TIVV) TO MOD-KVOT-PROG-VV-UT          
144800        MOVE OIGB-AAR-KVOT-DIV  (WS-TIVV) TO MOD-KVOT-DIV-VV-UT           
144900        MOVE OIGB-AAR-KVOT-SATS (WS-TIVV) TO MOD-KVOT-SATS-VV-UT          
145000        MOVE OIGB-AAR-KVOT-REFILL (WS-TIVV) TO MOD-KVOT-REF-VV-UT         
145100                                                                          
145200        MOVE MFS-RENSA-FAELT           TO MOD-KVOI-PROG-VV-IN             
145300                                          MOD-KVOI-DIV-VV-IN              
145400                                          MOD-KVOI-SATS-VV-IN             
145500                                          MOD-KVOI-SDC-VV-IN              
145600                                          MOD-KVOI-NDC-VV-IN              
145700                                          MOD-KVOI-LED-VV-IN              
145800                                          MOD-KVOI-REF-VV-IN              
145900                                                                          
146000        MOVE MFS-RENSA-FAELT           TO MOD-KVOI-PROG-DD-IN             
146100                                          MOD-KVOI-DIV-DD-IN              
146200                                          MOD-KVOI-SATS-DD-IN             
146300                                          MOD-KVOI-SDC-DD-IN              
146400                                          MOD-KVOI-NDC-DD-IN              
146500                                          MOD-KVOI-LED-DD-IN              
146600                                          MOD-KVOI-REF-DD-IN              
146700                                                                          
146800*****        OM INNEVARANDE VV ELLER DE TVÅ NÄRMASTE                      
146900*****        ANNARS BLANKT  OCH STÄNGDA FÄLT                              
147000        IF WS-TIAAVV = DAGENS-AAVV    OR                                  
147100           WS-TIAAVV = DAGENS-AAVV--1 OR                                  
147200           WS-TIAAVV = DAGENS-AAVV--2                                     
147300           MOVE WS-TIVV   TO MOD-TIVV                                     
147400           MOVE WS-TID    TO MOD-TID                                      
147500                                                                          
147600           IF WS-TIAAVV = DAGENS-AAVV                                     
147700             COMPUTE WS-TIDD = 14 + WS-TID                                
147800           ELSE                                                           
147900             IF WS-TIAAVV = DAGENS-AAVV--1                                
148000                COMPUTE WS-TIDD =  7 + WS-TID                             
148100             ELSE                                                         
148200                MOVE WS-TID TO WS-TIDD                                    
148300             END-IF                                                       
148400           END-IF                                                         
148500                                                                          
148600          MOVE OIGB-ART-KVOI-PROG (WS-TIDD) TO MOD-KVOI-PROG-DD-UT        
148700          MOVE OIGB-ART-KVOI-DIV (WS-TIDD) TO MOD-KVOI-DIV-DD-UT          
148800          MOVE OIGB-ART-KVOI-SATS (WS-TIDD) TO MOD-KVOI-SATS-DD-UT        
148900          MOVE OIGB-ART-KVOI-SDC (WS-TIDD) TO MOD-KVOI-SDC-DD-UT          
149000          MOVE OIGB-ART-KVOI-NDC (WS-TIDD) TO MOD-KVOI-NDC-DD-UT          
149100          MOVE OIGB-ART-KVOI-LEDTID(WS-TIDD) TO MOD-KVOI-LED-DD-UT        
149200          MOVE OIGB-ART-KVOI-REFILL(WS-TIDD) TO MOD-KVOI-REF-DD-UT        
149300                                                                          
149400*****          OBS STÄNG VV-DELEN                                         
149500          MOVE MFS-STAENG-FAELT TO MOD-KVOI-PROG-VV-IN-ATTR               
149600                                   MOD-KVOI-DIV-VV-IN-ATTR                
149700                                   MOD-KVOI-SATS-VV-IN-ATTR               
149800                                   MOD-KVOI-SDC-VV-IN-ATTR                
149900                                   MOD-KVOI-NDC-VV-IN-ATTR                
150000                                   MOD-KVOI-LED-VV-IN-ATTR                
150100                                   MOD-KVOI-REF-VV-IN-ATTR                
150200        ELSE                                                              
150300*****          OBS STÄNG DD-DELEN  ANNARS                                 
150400          MOVE MFS-STAENG-FAELT TO MOD-KVOI-PROG-DD-IN-ATTR               
150500                                   MOD-KVOI-DIV-DD-IN-ATTR                
150600                                   MOD-KVOI-SATS-DD-IN-ATTR               
150700                                   MOD-KVOI-SDC-DD-IN-ATTR                
150800                                   MOD-KVOI-NDC-DD-IN-ATTR                
150900                                   MOD-KVOI-LED-DD-IN-ATTR                
151000                                   MOD-KVOI-REF-DD-IN-ATTR                
151100          MOVE MFS-RENSA-FAELT  TO MOD-KVOI-PROG-DD-UT                    
151200                                   MOD-KVOI-DIV-DD-UT                     
151300                                   MOD-KVOI-SATS-DD-UT                    
151400                                   MOD-KVOI-SDC-DD-UT                     
151500                                   MOD-KVOI-NDC-DD-UT                     
151600                                   MOD-KVOI-LED-DD-UT                     
151700                                   MOD-KVOI-REF-DD-UT                     
151800          MOVE MFS-RENSA-FAELT  TO MOD-KVOI-PROG-DD-IN                    
151900                                   MOD-KVOI-DIV-DD-IN                     
152000                                   MOD-KVOI-SATS-DD-IN                    
152100                                   MOD-KVOI-SDC-DD-IN                     
152200                                   MOD-KVOI-NDC-DD-IN                     
152300                                   MOD-KVOI-LED-DD-IN                     
152400                                   MOD-KVOI-REF-DD-IN                     
152500                                                                          
152600                                   MOD-TIVV                               
152700                                   MOD-TID                                
152800        END-IF                                                            
152900     END-IF                                                               
153000     .                                                                    
153100     EJECT                                                                
153200 FA-LAES-GRUNDDATA SECTION.                                               
153300                                                                          
153400     MOVE WS-IDARTNR TO W-IDARTNR                                         
153500     MOVE WS-TIAA    TO W-TIAAAA-3-4                                      
153600     IF WS-TIAA < 50                                                      
153700        MOVE 20         TO W-TIAAAA-1-2                                   
153800     ELSE                                                                 
153900        MOVE 19         TO W-TIAAAA-1-2                                   
154000     END-IF                                                               
154100                                                                          
154200     MOVE 'FA-GU-01     ' TO W-MED                                        
154300     PERFORM IMS-GHU-WDL801                                               
154400     IF SEGMENT-FINNS                                                     
154500        MOVE 'FA-GNP-11    ' TO W-MED                                     
154600        PERFORM IMS-GNP-WDL811                                            
154700     ELSE                                                                 
154800* OBS   FELMED                                                            
154900        PERFORM MFS-RENSA-FAELT-IN                                        
155000        PERFORM MFS-RENSA-FAELT-UT                                        
155100     END-IF                                                               
155200     .                                                                    
155300     EJECT                                                                
155400 G-KOLLA-INPUT    SECTION.                                                
155500                                                                          
155600     PERFORM MFS-LAES-IN-IGEN                                             
155700     PERFORM MFS-ROER-EJ-FAELT-IN                                         
155800                                                                          
155900                                                                          
156000***  VECKODEL                                                             
156100                                                                          
156200     IF MID-KVOI-PROG-VV-IN NOT = ALL '+'                                 
156300        INSPECT MID-KVOI-PROG-VV-IN                                       
156400                REPLACING LEADING SPACE BY ZERO                           
156500        MOVE MID-KVOI-PROG-VV-IN   TO IN-FAELT                            
156600        PERFORM GA-KOLLA-MINUS                                            
156700        IF IN-FAELT NOT NUMERIC                                           
156800           MOVE NEJ TO INDATA-SW                                          
156900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-PROG-VV-IN-ATTR            
157000        ELSE                                                              
157100           MOVE IN-FAELT-NUM       TO W-KVOI-PROG-VV-IN                   
157200           COMPUTE W-KVOI-PROG-VV-IN = W-KVOI-PROG-VV-IN * FAKT           
157300        END-IF                                                            
157400     END-IF                                                               
157500                                                                          
157600     IF MID-KVOI-DIV-VV-IN NOT = ALL '+'                                  
157700        INSPECT MID-KVOI-DIV-VV-IN                                        
157800                REPLACING LEADING SPACE BY ZERO                           
157900        MOVE MID-KVOI-DIV-VV-IN   TO IN-FAELT                             
158000        PERFORM GA-KOLLA-MINUS                                            
158100        IF IN-FAELT NOT NUMERIC                                           
158200           MOVE NEJ TO INDATA-SW                                          
158300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-DIV-VV-IN-ATTR             
158400        ELSE                                                              
158500           MOVE IN-FAELT-NUM       TO W-KVOI-DIV-VV-IN                    
158600           COMPUTE W-KVOI-DIV-VV-IN = W-KVOI-DIV-VV-IN * FAKT             
158700        END-IF                                                            
158800     END-IF                                                               
158900                                                                          
159000     IF MID-KVOI-SATS-VV-IN NOT = ALL '+'                                 
159100        INSPECT MID-KVOI-SATS-VV-IN                                       
159200                REPLACING LEADING SPACE BY ZERO                           
159300        MOVE MID-KVOI-SATS-VV-IN   TO IN-FAELT                            
159400        PERFORM GA-KOLLA-MINUS                                            
159500        IF IN-FAELT NOT NUMERIC                                           
159600           MOVE NEJ TO INDATA-SW                                          
159700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-SATS-VV-IN-ATTR            
159800        ELSE                                                              
159900           MOVE IN-FAELT-NUM       TO W-KVOI-SATS-VV-IN                   
160000           COMPUTE W-KVOI-SATS-VV-IN = W-KVOI-SATS-VV-IN * FAKT           
160100        END-IF                                                            
160200     END-IF                                                               
160300                                                                          
160400     IF MID-KVOI-SDC-VV-IN NOT = ALL '+'                                  
160500        INSPECT MID-KVOI-SDC-VV-IN                                        
160600                REPLACING LEADING SPACE BY ZERO                           
160700        MOVE MID-KVOI-SDC-VV-IN   TO IN-FAELT                             
160800        PERFORM GA-KOLLA-MINUS                                            
160900        IF IN-FAELT NOT NUMERIC                                           
161000           MOVE NEJ TO INDATA-SW                                          
161100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-SDC-VV-IN-ATTR             
161200        ELSE                                                              
161300           MOVE IN-FAELT-NUM       TO W-KVOI-SDC-VV-IN                    
161400           COMPUTE W-KVOI-SDC-VV-IN = W-KVOI-SDC-VV-IN * FAKT             
161500        END-IF                                                            
161600     END-IF                                                               
161700                                                                          
161800     IF MID-KVOI-NDC-VV-IN NOT = ALL '+'                                  
161900        INSPECT MID-KVOI-NDC-VV-IN                                        
162000                REPLACING LEADING SPACE BY ZERO                           
162100        MOVE MID-KVOI-NDC-VV-IN   TO IN-FAELT                             
162200        PERFORM GA-KOLLA-MINUS                                            
162300        IF IN-FAELT NOT NUMERIC                                           
162400           MOVE NEJ TO INDATA-SW                                          
162500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-NDC-VV-IN-ATTR             
162600        ELSE                                                              
162700           MOVE IN-FAELT-NUM       TO W-KVOI-NDC-VV-IN                    
162800           COMPUTE W-KVOI-NDC-VV-IN = W-KVOI-NDC-VV-IN * FAKT             
162900        END-IF                                                            
163000     END-IF                                                               
163100                                                                          
163200     IF MID-KVOI-LED-VV-IN NOT = ALL '+'                                  
163300        INSPECT MID-KVOI-LED-VV-IN                                        
163400                REPLACING LEADING SPACE BY ZERO                           
163500        MOVE MID-KVOI-LED-VV-IN   TO IN-FAELT                             
163600        PERFORM GA-KOLLA-MINUS                                            
163700        IF IN-FAELT NOT NUMERIC                                           
163800           MOVE NEJ TO INDATA-SW                                          
163900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-LED-VV-IN-ATTR             
164000        ELSE                                                              
164100           MOVE IN-FAELT-NUM       TO W-KVOI-LED-VV-IN                    
164200           COMPUTE W-KVOI-LED-VV-IN = W-KVOI-LED-VV-IN * FAKT             
164300        END-IF                                                            
164400     END-IF                                                               
164500                                                                          
164600     IF MID-KVOI-REF-VV-IN NOT = ALL '+'                                  
164700        INSPECT MID-KVOI-REF-VV-IN                                        
164800                REPLACING LEADING SPACE BY ZERO                           
164900        MOVE MID-KVOI-REF-VV-IN    TO IN-FAELT                            
165000        PERFORM GA-KOLLA-MINUS                                            
165100        IF IN-FAELT NOT NUMERIC                                           
165200           MOVE NEJ TO INDATA-SW                                          
165300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-REF-VV-IN-ATTR             
165400        ELSE                                                              
165500           MOVE IN-FAELT-NUM       TO W-KVOI-REF-VV-IN                    
165600           COMPUTE W-KVOI-REF-VV-IN = W-KVOI-REF-VV-IN * FAKT             
165700        END-IF                                                            
165800     END-IF                                                               
165900                                                                          
166000                                                                          
166100******  DAGDEL        ********************************                    
166200                                                                          
166300     IF MID-KVOI-PROG-DD-IN NOT = ALL '+'                                 
166400        INSPECT MID-KVOI-PROG-DD-IN                                       
166500                REPLACING LEADING SPACE BY ZERO                           
166600        MOVE MID-KVOI-PROG-DD-IN   TO IN-FAELT                            
166700        PERFORM GA-KOLLA-MINUS                                            
166800        IF IN-FAELT NOT NUMERIC                                           
166900           MOVE NEJ TO INDATA-SW                                          
167000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-PROG-DD-IN-ATTR            
167100        ELSE                                                              
167200           MOVE IN-FAELT-NUM       TO W-KVOI-PROG-DD-IN                   
167300           COMPUTE W-KVOI-PROG-DD-IN = W-KVOI-PROG-DD-IN * FAKT           
167400        END-IF                                                            
167500     END-IF                                                               
167600                                                                          
167700     IF MID-KVOI-DIV-DD-IN NOT = ALL '+'                                  
167800        INSPECT MID-KVOI-DIV-DD-IN                                        
167900                REPLACING LEADING SPACE BY ZERO                           
168000        MOVE MID-KVOI-DIV-DD-IN    TO IN-FAELT                            
168100        PERFORM GA-KOLLA-MINUS                                            
168200        IF IN-FAELT NOT NUMERIC                                           
168300           MOVE NEJ TO INDATA-SW                                          
168400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-DIV-DD-IN-ATTR             
168500        ELSE                                                              
168600           MOVE IN-FAELT-NUM       TO W-KVOI-DIV-DD-IN                    
168700           COMPUTE W-KVOI-DIV-DD-IN = W-KVOI-DIV-DD-IN * FAKT             
168800        END-IF                                                            
168900     END-IF                                                               
169000                                                                          
169100     IF MID-KVOI-SATS-DD-IN NOT = ALL '+'                                 
169200        INSPECT MID-KVOI-SATS-DD-IN                                       
169300                REPLACING LEADING SPACE BY ZERO                           
169400        MOVE MID-KVOI-SATS-DD-IN   TO IN-FAELT                            
169500        PERFORM GA-KOLLA-MINUS                                            
169600        IF IN-FAELT NOT NUMERIC                                           
169700           MOVE NEJ TO INDATA-SW                                          
169800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-SATS-DD-IN-ATTR            
169900        ELSE                                                              
170000           MOVE IN-FAELT-NUM       TO W-KVOI-SATS-DD-IN                   
170100           COMPUTE W-KVOI-SATS-DD-IN = W-KVOI-SATS-DD-IN * FAKT           
170200        END-IF                                                            
170300     END-IF                                                               
170400                                                                          
170500     IF MID-KVOI-SDC-DD-IN NOT = ALL '+'                                  
170600        INSPECT MID-KVOI-SDC-DD-IN                                        
170700                REPLACING LEADING SPACE BY ZERO                           
170800        MOVE MID-KVOI-SDC-DD-IN    TO IN-FAELT                            
170900        PERFORM GA-KOLLA-MINUS                                            
171000        IF IN-FAELT NOT NUMERIC                                           
171100           MOVE NEJ TO INDATA-SW                                          
171200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-SDC-DD-IN-ATTR             
171300        ELSE                                                              
171400           MOVE IN-FAELT-NUM       TO W-KVOI-SDC-DD-IN                    
171500           COMPUTE W-KVOI-SDC-DD-IN = W-KVOI-SDC-DD-IN * FAKT             
171600        END-IF                                                            
171700     END-IF                                                               
171800                                                                          
171900     IF MID-KVOI-NDC-DD-IN NOT = ALL '+'                                  
172000        INSPECT MID-KVOI-NDC-DD-IN                                        
172100                REPLACING LEADING SPACE BY ZERO                           
172200        MOVE MID-KVOI-NDC-DD-IN    TO IN-FAELT                            
172300        PERFORM GA-KOLLA-MINUS                                            
172400        IF IN-FAELT NOT NUMERIC                                           
172500           MOVE NEJ TO INDATA-SW                                          
172600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-NDC-DD-IN-ATTR             
172700        ELSE                                                              
172800           MOVE IN-FAELT-NUM       TO W-KVOI-NDC-DD-IN                    
172900           COMPUTE W-KVOI-NDC-DD-IN = W-KVOI-NDC-DD-IN * FAKT             
173000        END-IF                                                            
173100     END-IF                                                               
173200                                                                          
173300     IF MID-KVOI-LED-DD-IN NOT = ALL '+'                                  
173400        INSPECT MID-KVOI-LED-DD-IN                                        
173500                REPLACING LEADING SPACE BY ZERO                           
173600        MOVE MID-KVOI-LED-DD-IN    TO IN-FAELT                            
173700        PERFORM GA-KOLLA-MINUS                                            
173800        IF IN-FAELT NOT NUMERIC                                           
173900           MOVE NEJ TO INDATA-SW                                          
174000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-LED-DD-IN-ATTR             
174100        ELSE                                                              
174200           MOVE IN-FAELT-NUM       TO W-KVOI-LED-DD-IN                    
174300           COMPUTE W-KVOI-LED-DD-IN = W-KVOI-LED-DD-IN * FAKT             
174400        END-IF                                                            
174500     END-IF                                                               
174600                                                                          
174700     IF MID-KVOI-REF-DD-IN NOT = ALL '+'                                  
174800        INSPECT MID-KVOI-REF-DD-IN                                        
174900                REPLACING LEADING SPACE BY ZERO                           
175000        MOVE MID-KVOI-REF-DD-IN    TO IN-FAELT                            
175100        PERFORM GA-KOLLA-MINUS                                            
175200        IF IN-FAELT NOT NUMERIC                                           
175300           MOVE NEJ TO INDATA-SW                                          
175400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-REF-DD-IN-ATTR             
175500        ELSE                                                              
175600           MOVE IN-FAELT-NUM       TO W-KVOI-REF-DD-IN                    
175700           COMPUTE W-KVOI-REF-DD-IN = W-KVOI-REF-DD-IN * FAKT             
175800        END-IF                                                            
175900     END-IF                                                               
176000                                                                          
176100                                                                          
176200****  KOLL ATT INPUT EJ KOMMER FRÅN FEL DEL  ***************              
176300                                                                          
176400           MOVE JA  TO KOLUMN-SW                                          
176500     IF WS-TIAAVV = DAGENS-AAVV    OR                                     
176600        WS-TIAAVV = DAGENS-AAVV--1 OR                                     
176700        WS-TIAAVV = DAGENS-AAVV--2                                        
176800        IF WS-TIAAVV = DAGENS-AAVV                                        
176900          COMPUTE WS-TIDD = 14 + WS-TID                                   
177000        ELSE                                                              
177100          IF WS-TIAAVV = DAGENS-AAVV--1                                   
177200             COMPUTE WS-TIDD =  7 + WS-TID                                
177300          ELSE                                                            
177400             MOVE WS-TID TO WS-TIDD                                       
177500          END-IF                                                          
177600        END-IF                                                            
177700        IF MID-KVOI-PROG-VV-IN NOT = ALL '+' OR                           
177800           MID-KVOI-DIV-VV-IN  NOT = ALL '+' OR                           
177900           MID-KVOI-SATS-VV-IN NOT = ALL '+' OR                           
178000           MID-KVOI-SDC-VV-IN  NOT = ALL '+' OR                           
178100           MID-KVOI-NDC-VV-IN  NOT = ALL '+' OR                           
178200           MID-KVOI-LED-VV-IN  NOT = ALL '+' OR                           
178300           MID-KVOI-REF-VV-IN  NOT = ALL '+'                              
178400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-PROG-VV-IN-ATTR            
178500                                      MOD-KVOI-DIV-VV-IN-ATTR             
178600                                      MOD-KVOI-SATS-VV-IN-ATTR            
178700                                      MOD-KVOI-SDC-VV-IN-ATTR             
178800                                      MOD-KVOI-NDC-VV-IN-ATTR             
178900                                      MOD-KVOI-LED-VV-IN-ATTR             
179000                                      MOD-KVOI-REF-VV-IN-ATTR             
179100           MOVE NEJ TO INDATA-SW                                          
179200           MOVE NEJ TO KOLUMN-SW                                          
179300        END-IF                                                            
179400     ELSE                                                                 
179500        IF MID-KVOI-PROG-DD-IN NOT = ALL '+' OR                           
179600           MID-KVOI-DIV-DD-IN  NOT = ALL '+' OR                           
179700           MID-KVOI-SATS-DD-IN NOT = ALL '+' OR                           
179800           MID-KVOI-SDC-DD-IN  NOT = ALL '+' OR                           
179900           MID-KVOI-NDC-DD-IN  NOT = ALL '+' OR                           
180000           MID-KVOI-LED-DD-IN  NOT = ALL '+' OR                           
180100           MID-KVOI-REF-DD-IN  NOT = ALL '+'                              
180200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVOI-PROG-DD-IN-ATTR            
180300                                      MOD-KVOI-DIV-DD-IN-ATTR             
180400                                      MOD-KVOI-SATS-DD-IN-ATTR            
180500                                      MOD-KVOI-SDC-DD-IN-ATTR             
180600                                      MOD-KVOI-NDC-DD-IN-ATTR             
180700                                      MOD-KVOI-LED-DD-IN-ATTR             
180800                                      MOD-KVOI-REF-DD-IN-ATTR             
180900           MOVE NEJ TO INDATA-SW                                          
181000           MOVE NEJ TO KOLUMN-SW                                          
181100        END-IF                                                            
181200     END-IF                                                               
181300                                                                          
181400     IF INDATA-FEL                                                        
181500        IF FINNS-PA-BAS                                                   
181600          MOVE FEL-ERR-FIELD  TO MED-IDMFSFEL MSG-KOM-IDMFSMED            
181700          CALL WMEDKONV USING MED-WMEDAREA                                
181800          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
181900          IF KOLUMN-FEL                                                   
182000             MOVE 'FEL KOLUMN IFYLLD' TO MOD-TEMFSFEL                     
182100          END-IF                                                          
182200        END-IF                                                            
182300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
182400                                                                          
182500        IF WS-TIAAVV = DAGENS-AAVV    OR                                  
182600           WS-TIAAVV = DAGENS-AAVV--1 OR                                  
182700           WS-TIAAVV = DAGENS-AAVV--2                                     
182800*****              STÄNG VV-DELEN                                         
182900          MOVE MFS-STAENG-FAELT TO MOD-KVOI-PROG-VV-IN-ATTR               
183000                                   MOD-KVOI-DIV-VV-IN-ATTR                
183100                                   MOD-KVOI-SATS-VV-IN-ATTR               
183200                                   MOD-KVOI-SDC-VV-IN-ATTR                
183300                                   MOD-KVOI-NDC-VV-IN-ATTR                
183400                                   MOD-KVOI-LED-VV-IN-ATTR                
183500                                   MOD-KVOI-REF-VV-IN-ATTR                
183600        ELSE                                                              
183700*****              STÄNG DD-DELEN                                         
183800          MOVE MFS-STAENG-FAELT TO MOD-KVOI-PROG-DD-IN-ATTR               
183900                                   MOD-KVOI-DIV-DD-IN-ATTR                
184000                                   MOD-KVOI-SATS-DD-IN-ATTR               
184100                                   MOD-KVOI-SDC-DD-IN-ATTR                
184200                                   MOD-KVOI-NDC-DD-IN-ATTR                
184300                                   MOD-KVOI-LED-DD-IN-ATTR                
184400                                   MOD-KVOI-REF-DD-IN-ATTR                
184500        END-IF                                                            
184600     END-IF                                                               
184700     .                                                                    
184800     EJECT                                                                
184900 GA-KOLLA-MINUS   SECTION.                                                
185000                                                                          
185100     MOVE +1         TO FAKT                                              
185200     MOVE +1         TO IX                                                
185300     PERFORM UNTIL IX > 7                                                 
185400        IF IN-FAELT (IX:1) = MINUS                                        
185500           MOVE -1   TO FAKT                                              
185600           MOVE ZERO TO IN-FAELT (IX:1)                                   
185700        END-IF                                                            
185800        ADD +1       TO IX                                                
185900     END-PERFORM                                                          
186000     .                                                                    
186100     EJECT                                                                
186200 H-UPPDATERA-WDL8 SECTION.                                                
186300                                                                          
186400***     OBS  ÄR DET OK ATT BARA ADDERA IN  ( + - ? )                      
186500***          ELLER ÄR DET TOTALBELOPPET SOM SKALL IN                      
186600                                                                          
186700***     INDEX OCH NYCKLAR REDAN FIXADE                                    
186800     PERFORM HD-NOLLA-WDL811-SPAR                                         
186900                                                                          
187000     IF WS-TIAAVV = DAGENS-AAVV    OR                                     
187100                    DAGENS-AAVV--1 OR                                     
187200                    DAGENS-AAVV--2                                        
187300        MOVE 'H1-GHU-01    ' TO W-MED                                     
187400        PERFORM IMS-GHU-WDL801                                            
187500        IF SEGMENT-FINNS                                                  
187600           PERFORM HB-UPPD-KVOI-DD                                        
187700           MOVE 'H1-REPL-01   ' TO W-MED                                  
187800           PERFORM IMS-REPL-WDL801                                        
187900        ELSE                                                              
188000           PERFORM S01-INIT-WDL801                                        
188100           PERFORM HB-UPPD-KVOI-DD                                        
188200           MOVE 'H1-ISRT-01   ' TO W-MED                                  
188300           PERFORM IMS-ISRT-WDL801                                        
188400        END-IF                                                            
188500                                                                          
188600***             UPPDATERA ÄVEN VECKOSEGMENTET                             
188700           MOVE 'H1-GHU-11    ' TO W-MED                                  
188800        PERFORM IMS-GHNP-WDL811                                           
188900        IF SEGMENT-FINNS                                                  
189000           PERFORM HE-SPARA-WDL811-FOERE                                  
189100           PERFORM HC-UPPD-KVOI-VV-DD                                     
189200           MOVE 'H1-REPL-11   ' TO W-MED                                  
189300           PERFORM IMS-REPL-WDL811                                        
189400        ELSE                                                              
189500           PERFORM S02-INIT-WDL811                                        
189600           PERFORM HE-SPARA-WDL811-FOERE                                  
189700           PERFORM HC-UPPD-KVOI-VV-DD                                     
189800           MOVE 'H1-ISRT-11   ' TO W-MED                                  
189900           PERFORM IMS-ISRT-WDL811                                        
190000        END-IF                                                            
190100     ELSE                                                                 
190200*               ENDAST VECKOUPPDATERING, EJ DAG                           
190300           MOVE 'H2-GHU-11    ' TO W-MED                                  
190400        PERFORM IMS-GHU-WDL811                                            
190500        IF SEGMENT-FINNS                                                  
190600           PERFORM HE-SPARA-WDL811-FOERE                                  
190700           PERFORM HA-UPPD-KVOI-VV                                        
190800           MOVE 'H2-REPL-11   ' TO W-MED                                  
190900           PERFORM IMS-REPL-WDL811                                        
191000        ELSE                                                              
191100           MOVE 'H2-GHU-01    ' TO W-MED                                  
191200           PERFORM IMS-GHU-WDL801                                         
191300           IF SEGMENT-SAKNAS                                              
191400              PERFORM S01-INIT-WDL801                                     
191500              MOVE 'H2-ISRT-01   ' TO W-MED                               
191600              PERFORM IMS-ISRT-WDL801                                     
191700           END-IF                                                         
191800           PERFORM S02-INIT-WDL811                                        
191900           PERFORM HE-SPARA-WDL811-FOERE                                  
192000           PERFORM HA-UPPD-KVOI-VV                                        
192100              MOVE 'H2-ISRT-11   ' TO W-MED                               
192200           PERFORM IMS-ISRT-WDL811                                        
192300        END-IF                                                            
192400     END-IF                                                               
192500                                                                          
192600     PERFORM HF-SPARA-WDL811-EFTER                                        
192700                                                                          
192800        MOVE OK-BEHANDLAD   TO MED-IDMFSFEL MSG-KOM-IDMFSMED              
192900        CALL WMEDKONV USING MED-WMEDAREA                                  
193000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
193100***       SKALL ORDERTRÄFFAR JUSTERAS ?                                   
193200     .                                                                    
193300     EJECT                                                                
193400 HA-UPPD-KVOI-VV    SECTION.                                              
193500                                                                          
193600     IF MID-KVOI-PROG-VV-IN NOT = ALL '+'                                 
193700       MOVE   W-KVOI-PROG-VV-IN TO WS-NUM                                 
193800       IF MFS-UPD-X                                                       
193900         IF MID-KDBEHX = 'O' OR 'P'                                       
194000           ADD  WS-NUM        TO OIGB-AAR-KVOI-PROG (WS-TIVV)             
194100           ADD  +1            TO OIGB-AAR-KVOT-PROG (WS-TIVV)             
194200         ELSE                                                             
194300******                                 'A' OR 'M'                         
194400           SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-PROG (WS-TIVV)             
194500         END-IF                                                           
194600       ELSE                                                               
194700         MOVE WS-NUM           TO OIGB-AAR-KVOI-PROG (WS-TIVV)            
194800       END-IF                                                             
194900     END-IF                                                               
195000                                                                          
195100     IF MID-KVOI-DIV-VV-IN NOT = ALL '+'                                  
195200       MOVE   W-KVOI-DIV-VV-IN TO WS-NUM                                  
195300       IF MFS-UPD-X                                                       
195400         IF MID-KDBEHX = 'O' OR 'P'                                       
195500           ADD  WS-NUM        TO OIGB-AAR-KVOI-DIV (WS-TIVV)              
195600           ADD  +1            TO OIGB-AAR-KVOT-DIV (WS-TIVV)              
195700         ELSE                                                             
195800******                                 'A' OR 'M'                         
195900           SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-DIV (WS-TIVV)              
196000         END-IF                                                           
196100       ELSE                                                               
196200         MOVE WS-NUM           TO OIGB-AAR-KVOI-DIV (WS-TIVV)             
196300       END-IF                                                             
196400     END-IF                                                               
196500                                                                          
196600     IF MID-KVOI-SATS-VV-IN NOT = ALL '+'                                 
196700       MOVE   W-KVOI-SATS-VV-IN TO WS-NUM                                 
196800       IF MFS-UPD-X                                                       
196900         IF MID-KDBEHX = 'O' OR 'P'                                       
197000           ADD  WS-NUM        TO OIGB-AAR-KVOI-SATS (WS-TIVV)             
197100           ADD  +1            TO OIGB-AAR-KVOT-SATS (WS-TIVV)             
197200         ELSE                                                             
197300******                                 'A' OR 'M'                         
197400           SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-SATS (WS-TIVV)             
197500         END-IF                                                           
197600       ELSE                                                               
197700         MOVE WS-NUM           TO OIGB-AAR-KVOI-SATS (WS-TIVV)            
197800       END-IF                                                             
197900     END-IF                                                               
198000                                                                          
198100     IF MID-KVOI-SDC-VV-IN NOT = ALL '+'                                  
198200       MOVE   W-KVOI-SDC-VV-IN TO WS-NUM                                  
198300         IF MFS-UPD-X                                                     
198400           IF MID-KDBEHX = 'O' OR 'P'                                     
198500             ADD  WS-NUM        TO OIGB-AAR-KVOI-SDC (WS-TIVV)            
198600           ELSE                                                           
198700******                                 'A' OR 'M'                         
198800             SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-SDC (WS-TIVV)            
198900           END-IF                                                         
199000         ELSE                                                             
199100           MOVE WS-NUM           TO OIGB-AAR-KVOI-SDC (WS-TIVV)           
199200         END-IF                                                           
199300       END-IF                                                             
199400                                                                          
199500       IF MID-KVOI-NDC-VV-IN NOT = ALL '+'                                
199600         MOVE   W-KVOI-NDC-VV-IN TO WS-NUM                                
199700         IF MFS-UPD-X                                                     
199800           IF MID-KDBEHX = 'O' OR 'P'                                     
199900             ADD  WS-NUM        TO OIGB-AAR-KVOI-NDC (WS-TIVV)            
200000           ELSE                                                           
200100******                                 'A' OR 'M'                         
200200             SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-NDC (WS-TIVV)            
200300           END-IF                                                         
200400         ELSE                                                             
200500           MOVE WS-NUM           TO OIGB-AAR-KVOI-NDC (WS-TIVV)           
200600         END-IF                                                           
200700       END-IF                                                             
200800                                                                          
200900       IF MID-KVOI-LED-VV-IN NOT = ALL '+'                                
201000         MOVE   W-KVOI-LED-VV-IN TO WS-NUM                                
201100         IF MFS-UPD-X                                                     
201200           IF MID-KDBEHX = 'O' OR 'P'                                     
201300             ADD  WS-NUM        TO OIGB-AAR-KVOI-LEDTID (WS-TIVV)         
201400           ELSE                                                           
201500******                                 'A' OR 'M'                         
201600             SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-LEDTID (WS-TIVV)         
201700           END-IF                                                         
201800         ELSE                                                             
201900           MOVE WS-NUM           TO OIGB-AAR-KVOI-LEDTID (WS-TIVV)        
202000         END-IF                                                           
202100       END-IF                                                             
202200                                                                          
202300       IF MID-KVOI-REF-VV-IN NOT = ALL '+'                                
202400         MOVE   W-KVOI-REF-VV-IN TO WS-NUM                                
202500         IF MFS-UPD-X                                                     
202600           IF MID-KDBEHX = 'O' OR 'P'                                     
202700             ADD  WS-NUM        TO OIGB-AAR-KVOI-REFILL (WS-TIVV)         
202800             ADD  +1            TO OIGB-AAR-KVOT-REFILL (WS-TIVV)         
202900           ELSE                                                           
203000******                                 'A' OR 'M'                         
203100             SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-REFILL (WS-TIVV)         
203200           END-IF                                                         
203300         ELSE                                                             
203400           MOVE WS-NUM           TO OIGB-AAR-KVOI-REFILL (WS-TIVV)        
203500         END-IF                                                           
203600       END-IF                                                             
203700     .                                                                    
203800     EJECT                                                                
203900 HB-UPPD-KVOI-DD    SECTION.                                              
204000                                                                          
204100     IF MID-KVOI-PROG-DD-IN   NOT = ALL '+'                               
204200        MOVE   W-KVOI-PROG-DD-IN TO WS-NUM                                
204300        IF MFS-UPD-X                                                      
204400           IF MID-KDBEHX = 'O' OR 'P'                                     
204500              ADD  WS-NUM        TO OIGB-ART-KVOI-PROG (WS-TIDD)          
204600           ELSE                                                           
204700***                                 'A' OR 'M'                            
204800              SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-PROG (WS-TIDD)          
204900           END-IF                                                         
205000       ELSE                                                               
205100     COMPUTE SPAR-PROG-DD = WS-NUM - OIGB-ART-KVOI-PROG (WS-TIDD)         
205200         MOVE WS-NUM           TO OIGB-ART-KVOI-PROG (WS-TIDD)            
205300       END-IF                                                             
205400     END-IF                                                               
205500                                                                          
205600     IF MID-KVOI-DIV-DD-IN   NOT = ALL '+'                                
205700        MOVE   W-KVOI-DIV-DD-IN TO WS-NUM                                 
205800        IF MFS-UPD-X                                                      
205900           IF MID-KDBEHX = 'O' OR 'P'                                     
206000              ADD  WS-NUM        TO OIGB-ART-KVOI-DIV (WS-TIDD)           
206100           ELSE                                                           
206200***                                 'A' OR 'M'                            
206300              SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-DIV (WS-TIDD)           
206400           END-IF                                                         
206500        ELSE                                                              
206600     COMPUTE SPAR-DIV-DD  = WS-NUM - OIGB-ART-KVOI-DIV (WS-TIDD)          
206700           MOVE WS-NUM           TO OIGB-ART-KVOI-DIV (WS-TIDD)           
206800        END-IF                                                            
206900     END-IF                                                               
207000                                                                          
207100     IF MID-KVOI-SATS-DD-IN   NOT = ALL '+'                               
207200        MOVE   W-KVOI-SATS-DD-IN TO WS-NUM                                
207300        IF MFS-UPD-X                                                      
207400           IF MID-KDBEHX = 'O' OR 'P'                                     
207500              ADD  WS-NUM        TO OIGB-ART-KVOI-SATS (WS-TIDD)          
207600           ELSE                                                           
207700***                                 'A' OR 'M'                            
207800              SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-SATS (WS-TIDD)          
207900           END-IF                                                         
208000        ELSE                                                              
208100     COMPUTE SPAR-SATS-DD = WS-NUM - OIGB-ART-KVOI-SATS (WS-TIDD)         
208200           MOVE WS-NUM           TO OIGB-ART-KVOI-SATS (WS-TIDD)          
208300        END-IF                                                            
208400     END-IF                                                               
208500                                                                          
208600     IF MID-KVOI-SDC-DD-IN   NOT = ALL '+'                                
208700        MOVE   W-KVOI-SDC-DD-IN TO WS-NUM                                 
208800        IF MFS-UPD-X                                                      
208900           IF MID-KDBEHX = 'O' OR 'P'                                     
209000              ADD  WS-NUM        TO OIGB-ART-KVOI-SDC (WS-TIDD)           
209100           ELSE                                                           
209200***                                 'A' OR 'M'                            
209300              SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-SDC (WS-TIDD)           
209400           END-IF                                                         
209500        ELSE                                                              
209600      COMPUTE SPAR-SDC-DD  = WS-NUM - OIGB-ART-KVOI-SDC (WS-TIDD)         
209700           MOVE WS-NUM           TO OIGB-ART-KVOI-SDC (WS-TIDD)           
209800        END-IF                                                            
209900     END-IF                                                               
210000                                                                          
210100     IF MID-KVOI-NDC-DD-IN   NOT = ALL '+'                                
210200        MOVE   W-KVOI-NDC-DD-IN TO WS-NUM                                 
210300        IF MFS-UPD-X                                                      
210400           IF MID-KDBEHX = 'O' OR 'P'                                     
210500              ADD  WS-NUM        TO OIGB-ART-KVOI-NDC (WS-TIDD)           
210600           ELSE                                                           
210700***                                 'A' OR 'M'                            
210800              SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-NDC (WS-TIDD)           
210900           END-IF                                                         
211000        ELSE                                                              
211100      COMPUTE SPAR-NDC-DD  = WS-NUM - OIGB-ART-KVOI-NDC (WS-TIDD)         
211200           MOVE WS-NUM           TO OIGB-ART-KVOI-NDC (WS-TIDD)           
211300        END-IF                                                            
211400     END-IF                                                               
211500                                                                          
211600     IF MID-KVOI-LED-DD-IN   NOT = ALL '+'                                
211700        MOVE   W-KVOI-LED-DD-IN TO WS-NUM                                 
211800        IF MFS-UPD-X                                                      
211900           IF MID-KDBEHX = 'O' OR 'P'                                     
212000              ADD  WS-NUM        TO OIGB-ART-KVOI-LEDTID (WS-TIDD)        
212100           ELSE                                                           
212200***                                 'A' OR 'M'                            
212300              SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-LEDTID (WS-TIDD)        
212400           END-IF                                                         
212500        ELSE                                                              
212600      COMPUTE SPAR-LED-DD = WS-NUM - OIGB-ART-KVOI-LEDTID(WS-TIDD)        
212700           MOVE WS-NUM           TO OIGB-ART-KVOI-LEDTID (WS-TIDD)        
212800        END-IF                                                            
212900     END-IF                                                               
213000                                                                          
213100     IF MID-KVOI-REF-DD-IN   NOT = ALL '+'                                
213200        MOVE   W-KVOI-REF-DD-IN TO WS-NUM                                 
213300        IF MFS-UPD-X                                                      
213400           IF MID-KDBEHX = 'O' OR 'P'                                     
213500              ADD  WS-NUM        TO OIGB-ART-KVOI-REFILL (WS-TIDD)        
213600           ELSE                                                           
213700***                                 'A' OR 'M'                            
213800              SUBTRACT WS-NUM  FROM OIGB-ART-KVOI-REFILL (WS-TIDD)        
213900           END-IF                                                         
214000        ELSE                                                              
214100           COMPUTE SPAR-REF-DD    =                                       
214200                   WS-NUM - OIGB-ART-KVOI-REFILL (WS-TIDD)                
214300           MOVE WS-NUM           TO OIGB-ART-KVOI-REFILL (WS-TIDD)        
214400        END-IF                                                            
214500     END-IF                                                               
214600     .                                                                    
214700     EJECT                                                                
214800 HC-UPPD-KVOI-VV-DD SECTION.                                              
214900                                                                          
215000     IF MID-KVOI-PROG-DD-IN   NOT = ALL '+'                               
215100        MOVE   W-KVOI-PROG-DD-IN TO WS-NUM                                
215200        IF MFS-UPD-X                                                      
215300           IF MID-KDBEHX = 'O' OR 'P'                                     
215400              ADD  WS-NUM        TO OIGB-AAR-KVOI-PROG (WS-TIVV)          
215500              ADD  +1            TO OIGB-AAR-KVOT-PROG (WS-TIVV)          
215600           ELSE                                                           
215700***                                 'A' OR 'M'                            
215800              SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-PROG (WS-TIVV)          
215900           END-IF                                                         
216000        ELSE                                                              
216100           ADD  SPAR-PROG-DD     TO OIGB-AAR-KVOI-PROG (WS-TIVV)          
216200        END-IF                                                            
216300     END-IF                                                               
216400                                                                          
216500     IF MID-KVOI-DIV-DD-IN    NOT = ALL '+'                               
216600        MOVE   W-KVOI-DIV-DD-IN  TO WS-NUM                                
216700        IF MFS-UPD-X                                                      
216800           IF MID-KDBEHX = 'O' OR 'P'                                     
216900              ADD  WS-NUM        TO OIGB-AAR-KVOI-DIV (WS-TIVV)           
217000              ADD  +1            TO OIGB-AAR-KVOT-DIV (WS-TIVV)           
217100           ELSE                                                           
217200***                                 'A' OR 'M'                            
217300              SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-DIV (WS-TIVV)           
217400           END-IF                                                         
217500        ELSE                                                              
217600           ADD  SPAR-DIV-DD      TO OIGB-AAR-KVOI-DIV (WS-TIVV)           
217700        END-IF                                                            
217800     END-IF                                                               
217900                                                                          
218000     IF MID-KVOI-SATS-DD-IN   NOT = ALL '+'                               
218100        MOVE   W-KVOI-SATS-DD-IN TO WS-NUM                                
218200        IF MFS-UPD-X                                                      
218300           IF MID-KDBEHX = 'O' OR 'P'                                     
218400              ADD  WS-NUM        TO OIGB-AAR-KVOI-SATS (WS-TIVV)          
218500              ADD  +1            TO OIGB-AAR-KVOT-SATS (WS-TIVV)          
218600           ELSE                                                           
218700***                                 'A' OR 'M'                            
218800              SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-SATS (WS-TIVV)          
218900           END-IF                                                         
219000        ELSE                                                              
219100           ADD  SPAR-SATS-DD     TO OIGB-AAR-KVOI-SATS (WS-TIVV)          
219200        END-IF                                                            
219300     END-IF                                                               
219400                                                                          
219500     IF MID-KVOI-SDC-DD-IN    NOT = ALL '+'                               
219600        MOVE   W-KVOI-SDC-DD-IN  TO WS-NUM                                
219700        IF MFS-UPD-X                                                      
219800           IF MID-KDBEHX = 'O' OR 'P'                                     
219900              ADD  WS-NUM        TO OIGB-AAR-KVOI-SDC (WS-TIVV)           
220000           ELSE                                                           
220100***                                 'A' OR 'M'                            
220200              SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-SDC (WS-TIVV)           
220300           END-IF                                                         
220400        ELSE                                                              
220500           ADD  SPAR-SDC-DD      TO OIGB-AAR-KVOI-SDC (WS-TIVV)           
220600        END-IF                                                            
220700     END-IF                                                               
220800                                                                          
220900     IF MID-KVOI-NDC-DD-IN    NOT = ALL '+'                               
221000        MOVE   W-KVOI-NDC-DD-IN  TO WS-NUM                                
221100        IF MFS-UPD-X                                                      
221200           IF MID-KDBEHX = 'O' OR 'P'                                     
221300              ADD  WS-NUM        TO OIGB-AAR-KVOI-NDC (WS-TIVV)           
221400           ELSE                                                           
221500***                                 'A' OR 'M'                            
221600              SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-NDC (WS-TIVV)           
221700           END-IF                                                         
221800        ELSE                                                              
221900           ADD  SPAR-NDC-DD      TO OIGB-AAR-KVOI-NDC (WS-TIVV)           
222000        END-IF                                                            
222100     END-IF                                                               
222200                                                                          
222300     IF MID-KVOI-LED-DD-IN    NOT = ALL '+'                               
222400        MOVE   W-KVOI-LED-DD-IN  TO WS-NUM                                
222500        IF MFS-UPD-X                                                      
222600           IF MID-KDBEHX = 'O' OR 'P'                                     
222700              ADD  WS-NUM        TO OIGB-AAR-KVOI-LEDTID (WS-TIVV)        
222800           ELSE                                                           
222900***                                 'A' OR 'M'                            
223000              SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-LEDTID (WS-TIVV)        
223100           END-IF                                                         
223200        ELSE                                                              
223300           ADD  SPAR-LED-DD      TO OIGB-AAR-KVOI-LEDTID (WS-TIVV)        
223400        END-IF                                                            
223500     END-IF                                                               
223600                                                                          
223700     IF MID-KVOI-REF-DD-IN    NOT = ALL '+'                               
223800        MOVE   W-KVOI-REF-DD-IN  TO WS-NUM                                
223900        IF MFS-UPD-X                                                      
224000           IF MID-KDBEHX = 'O' OR 'P'                                     
224100              ADD  WS-NUM        TO OIGB-AAR-KVOI-REFILL (WS-TIVV)        
224200              ADD  +1            TO OIGB-AAR-KVOT-REFILL (WS-TIVV)        
224300           ELSE                                                           
224400***                                 'A' OR 'M'                            
224500              SUBTRACT WS-NUM  FROM OIGB-AAR-KVOI-REFILL (WS-TIVV)        
224600           END-IF                                                         
224700        ELSE                                                              
224800           ADD  SPAR-REF-DD      TO OIGB-AAR-KVOI-REFILL (WS-TIVV)        
224900        END-IF                                                            
225000     END-IF                                                               
225100     .                                                                    
225200     EJECT                                                                
225300 HD-NOLLA-WDL811-SPAR SECTION.                                            
225400                                                                          
225500     MOVE ZERO TO        FOERE-KVOI-DIV                                   
225600                         FOERE-KVOI-NDC                                   
225700                         FOERE-KVOI-LED                                   
225800                         FOERE-KVOI-PROG                                  
225900                         FOERE-KVOI-SATS                                  
226000                         FOERE-KVOI-SDC                                   
226100                                                                          
226200     MOVE ZERO TO        EFTER-KVOI-DIV                                   
226300                         EFTER-KVOI-NDC                                   
226400                         EFTER-KVOI-LED                                   
226500                         EFTER-KVOI-PROG                                  
226600                         EFTER-KVOI-SATS                                  
226700                         EFTER-KVOI-SDC                                   
226800     .                                                                    
226900     EJECT                                                                
227000 HE-SPARA-WDL811-FOERE SECTION.                                           
227100                                                                          
227200     MOVE OIGB-AAR-TIAAAA   TO SPAR-WDL8-YYAA                             
227300     MOVE WS-TIVV           TO SPAR-WDL8-VV                               
227400     MOVE OIGB-AAR-KVOI-DIV  (WS-TIVV)  TO FOERE-KVOI-DIV                 
227500     MOVE OIGB-AAR-KVOI-NDC  (WS-TIVV)  TO FOERE-KVOI-NDC                 
227600     MOVE OIGB-AAR-KVOI-PROG (WS-TIVV)  TO FOERE-KVOI-PROG                
227700     MOVE OIGB-AAR-KVOI-SATS (WS-TIVV)  TO FOERE-KVOI-SATS                
227800     MOVE OIGB-AAR-KVOI-SDC  (WS-TIVV)  TO FOERE-KVOI-SDC                 
227900     MOVE OIGB-AAR-KVOI-LEDTID (WS-TIVV)  TO FOERE-KVOI-LED               
228000     .                                                                    
228100     EJECT                                                                
228200 HF-SPARA-WDL811-EFTER SECTION.                                           
228300                                                                          
228400     MOVE OIGB-AAR-KVOI-DIV  (WS-TIVV)  TO EFTER-KVOI-DIV                 
228500     MOVE OIGB-AAR-KVOI-NDC  (WS-TIVV)  TO EFTER-KVOI-NDC                 
228600     MOVE OIGB-AAR-KVOI-PROG (WS-TIVV)  TO EFTER-KVOI-PROG                
228700     MOVE OIGB-AAR-KVOI-SATS (WS-TIVV)  TO EFTER-KVOI-SATS                
228800     MOVE OIGB-AAR-KVOI-SDC  (WS-TIVV)  TO EFTER-KVOI-SDC                 
228900     MOVE OIGB-AAR-KVOI-LEDTID (WS-TIVV)  TO EFTER-KVOI-LED               
229000     .                                                                    
229100     EJECT                                                                
229200 S01-INIT-WDL801    SECTION.                                              
229300                                                                          
229400     MOVE  W-IDARTNR      TO OIGB-ART-IDARTNR                             
229500     MOVE +1              TO INDX                                         
229600     PERFORM 21 TIMES                                                     
229700       MOVE TAB-TIVVD (INDX) TO OIGB-ART-TIVVD (INDX)                     
229800       MOVE ZERO         TO OIGB-ART-KVOI-DIV (INDX)                      
229900                            OIGB-ART-KVOI-NDC (INDX)                      
230000                            OIGB-ART-KVOI-LEDTID (INDX)                   
230100                            OIGB-ART-KVOI-PROG (INDX)                     
230200                            OIGB-ART-KVOI-REFILL (INDX)                   
230300                            OIGB-ART-KVOI-SATS (INDX)                     
230400                            OIGB-ART-KVOI-SDC (INDX)                      
230500       ADD +1            TO INDX                                          
230600     END-PERFORM                                                          
230700     .                                                                    
230800     EJECT                                                                
230900 S02-INIT-WDL811    SECTION.                                              
231000                                                                          
231100     MOVE  WS-TIAAAA      TO OIGB-AAR-TIAAAA                              
231200     MOVE +1              TO INDX                                         
231300     PERFORM 53 TIMES                                                     
231400       MOVE ZERO         TO OIGB-AAR-KVOI-DIV    (INDX)                   
231500                            OIGB-AAR-KVOI-NDC    (INDX)                   
231600                            OIGB-AAR-KVOI-LEDTID (INDX)                   
231700                            OIGB-AAR-KVOI-PROG   (INDX)                   
231800                            OIGB-AAR-KVOI-REFILL (INDX)                   
231900                            OIGB-AAR-KVOI-SATS   (INDX)                   
232000                            OIGB-AAR-KVOI-SDC    (INDX)                   
232100                            OIGB-AAR-KVOT-DIV    (INDX)                   
232200                            OIGB-AAR-KVOT-PROG   (INDX)                   
232300                            OIGB-AAR-KVOT-SATS   (INDX)                   
232400                            OIGB-AAR-KVOT-REFILL (INDX)                   
232500       ADD +1            TO INDX                                          
232600     END-PERFORM                                                          
232700     .                                                                    
232800     EJECT                                                                
232900 S22-INIT-WDL811-2  SECTION.                                              
233000                                                                          
233100     MOVE  WS-TIAAAA-2    TO OIGB2-AAR-TIAAAA                             
233200     MOVE +1              TO INDX                                         
233300     PERFORM 53 TIMES                                                     
233400       MOVE ZERO         TO OIGB2-AAR-KVOI-DIV    (INDX)                  
233500                            OIGB2-AAR-KVOI-NDC    (INDX)                  
233600                            OIGB2-AAR-KVOI-LEDTID (INDX)                  
233700                            OIGB2-AAR-KVOI-PROG   (INDX)                  
233800                            OIGB2-AAR-KVOI-REFILL (INDX)                  
233900                            OIGB2-AAR-KVOI-SATS   (INDX)                  
234000                            OIGB2-AAR-KVOI-SDC    (INDX)                  
234100                            OIGB2-AAR-KVOT-DIV    (INDX)                  
234200                            OIGB2-AAR-KVOT-PROG   (INDX)                  
234300                            OIGB2-AAR-KVOT-SATS   (INDX)                  
234400                            OIGB2-AAR-KVOT-REFILL (INDX)                  
234500       ADD +1            TO INDX                                          
234600     END-PERFORM                                                          
234700     .                                                                    
234800     EJECT                                                                
234900                                                                          
235000 S30-UPD-FLREFNYO-WDK711 SECTION.                                         
235100                                                                          
235200     PERFORM IMS-GHU-WDK711                                               
235300     IF SEGMENT-FINNS                                                     
235400        MOVE NEJ         TO SLAG-FLREFNYO                                 
235500        PERFORM IMS-REPL-WDK711                                           
235600     END-IF                                                               
235700     .                                                                    
235800     EJECT                                                                
235900                                                                          
236000 S50-FIXA-IX2    SECTION.                                                 
236100     MOVE +0 TO IX2                                                       
236200                                                                          
236300     IF WS-TIVV = OIGA-DC-TIVV (1)                                        
236400       MOVE +1 TO IX2                                                     
236500     ELSE                                                                 
236600       IF WS-TIVV = OIGA-DC-TIVV (2)                                      
236700         MOVE +2 TO IX2                                                   
236800       ELSE                                                               
236900         IF WS-TIVV = OIGA-DC-TIVV (3)                                    
237000           MOVE +3 TO IX2                                                 
237100         ELSE                                                             
237200           IF WS-TIVV = OIGA-DC-TIVV (4)                                  
237300             MOVE +4 TO IX2                                               
237400           ELSE                                                           
237500             IF WS-TIVV = OIGA-DC-TIVV (5)                                
237600               MOVE +5 TO IX2                                             
237700             END-IF                                                       
237800           END-IF                                                         
237900         END-IF                                                           
238000       END-IF                                                             
238100     END-IF                                                               
238200     .                                                                    
238300     EJECT                                                                
238400                                                                          
238500 SEC-URITY-CHECK SECTION.                                                 
238600     SKIP2                                                                
238700*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
238800     PERFORM IMS-GU-K601                                                  
238900     IF  SEGMENT-FINNS                                                    
239000       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
239100       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
239200       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
239300*        --- BEHÖRIG USER                                                 
239400         SET PASSED-SECURITY-CHECK TO TRUE                                
239500       ELSE                                                               
239600*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
239700         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
239800         CALL WMEDKONV USING MED-WMEDAREA                                 
239900         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
240000          MOVE NEJ TO NYCKLAR-SW                                          
240100       END-IF                                                             
240200     ELSE                                                                 
240300       MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                            
240400       CALL WMEDKONV USING MED-WMEDAREA                                   
240500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
240600          MOVE NEJ TO NYCKLAR-SW                                          
240700     END-IF                                                               
240800     .                                                                    
240900                                                                          
241000 MFS-RENSA-FAELT-UT SECTION.                                              
241100                                                                          
241200*    --- ALLA UTDATA-FÄLT                                                 
241300     MOVE MFS-RENSA-FAELT TO MOD-KVOI-PROG-VV-UT                          
241400                             MOD-KVOI-DIV-VV-UT                           
241500                             MOD-KVOI-SATS-VV-UT                          
241600                             MOD-KVOI-SDC-VV-UT                           
241700                             MOD-KVOI-NDC-VV-UT                           
241800                             MOD-KVOI-LED-VV-UT                           
241900                             MOD-KVOI-REF-VV-UT                           
242000     MOVE MFS-RENSA-FAELT TO MOD-KVOT-PROG-VV-UT                          
242100                             MOD-KVOT-DIV-VV-UT                           
242200                             MOD-KVOT-SATS-VV-UT                          
242300                             MOD-KVOT-REF-VV-UT                           
242400     MOVE MFS-RENSA-FAELT TO MOD-KVOI-PROG-DD-UT                          
242500                             MOD-KVOI-DIV-DD-UT                           
242600                             MOD-KVOI-SATS-DD-UT                          
242700                             MOD-KVOI-SDC-DD-UT                           
242800                             MOD-KVOI-NDC-DD-UT                           
242900                             MOD-KVOI-LED-DD-UT                           
243000                             MOD-KVOI-REF-DD-UT                           
243100     .                                                                    
243200     EJECT                                                                
243300 MFS-RENSA-FAELT-IN SECTION.                                              
243400                                                                          
243500*    --- ALLA INDATA-FÄLT                                                 
243600     MOVE MFS-RENSA-FAELT TO MOD-KVOI-PROG-VV-IN                          
243700                             MOD-KVOI-DIV-VV-IN                           
243800                             MOD-KVOI-SATS-VV-IN                          
243900                             MOD-KVOI-SDC-VV-IN                           
244000                             MOD-KVOI-NDC-VV-IN                           
244100                             MOD-KVOI-LED-VV-IN                           
244200                             MOD-KVOI-REF-VV-IN                           
244300     MOVE MFS-RENSA-FAELT TO MOD-KVOI-PROG-DD-IN                          
244400                             MOD-KVOI-DIV-DD-IN                           
244500                             MOD-KVOI-SATS-DD-IN                          
244600                             MOD-KVOI-SDC-DD-IN                           
244700                             MOD-KVOI-NDC-DD-IN                           
244800                             MOD-KVOI-LED-DD-IN                           
244900                             MOD-KVOI-REF-DD-IN                           
245000     .                                                                    
245100     EJECT                                                                
245200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
245300                                                                          
245400*    --- ALLA UTDATA-FÄLT                                                 
245500     MOVE MFS-ROER-EJ-FAELT TO MOD-KVOI-PROG-VV-UT                        
245600                               MOD-KVOI-DIV-VV-UT                         
245700                               MOD-KVOI-SATS-VV-UT                        
245800                               MOD-KVOI-SDC-VV-UT                         
245900                               MOD-KVOI-NDC-VV-UT                         
246000                               MOD-KVOI-LED-VV-UT                         
246100                               MOD-KVOI-REF-VV-UT                         
246200                               MOD-KVOT-PROG-VV-UT                        
246300                               MOD-KVOT-DIV-VV-UT                         
246400                               MOD-KVOT-SATS-VV-UT                        
246500                               MOD-KVOT-REF-VV-UT                         
246600                               MOD-KVOI-PROG-DD-UT                        
246700                               MOD-KVOI-DIV-DD-UT                         
246800                               MOD-KVOI-SATS-DD-UT                        
246900                               MOD-KVOI-SDC-DD-UT                         
247000                               MOD-KVOI-NDC-DD-UT                         
247100                               MOD-KVOI-LED-DD-UT                         
247200                               MOD-KVOI-REF-DD-UT                         
247300     .                                                                    
247400     SKIP3                                                                
247500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
247600                                                                          
247700*    --- ALLA INDATA-FÄLT                                                 
247800     MOVE MFS-ROER-EJ-FAELT TO MOD-KVOI-PROG-VV-IN                        
247900                               MOD-KVOI-PROG-DD-IN                        
248000                               MOD-KVOI-DIV-VV-IN                         
248100                               MOD-KVOI-DIV-DD-IN                         
248200                               MOD-KVOI-SATS-VV-IN                        
248300                               MOD-KVOI-SATS-DD-IN                        
248400                               MOD-KVOI-SDC-VV-IN                         
248500                               MOD-KVOI-SDC-DD-IN                         
248600                               MOD-KVOI-NDC-VV-IN                         
248700                               MOD-KVOI-NDC-DD-IN                         
248800                               MOD-KVOI-LED-VV-IN                         
248900                               MOD-KVOI-LED-DD-IN                         
249000                               MOD-KVOI-REF-VV-IN                         
249100                               MOD-KVOI-REF-DD-IN                         
249200     .                                                                    
249300     EJECT                                                                
249400 MFS-LAES-IN-IGEN SECTION.                                                
249500                                                                          
249600*    --- ALLA INDATA-FÄLT                                                 
249700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVOI-PROG-VV-IN-ATTR               
249800                                   MOD-KVOI-PROG-DD-IN-ATTR               
249900                                   MOD-KVOI-DIV-VV-IN-ATTR                
250000                                   MOD-KVOI-DIV-DD-IN-ATTR                
250100                                   MOD-KVOI-SATS-VV-IN-ATTR               
250200                                   MOD-KVOI-SATS-DD-IN-ATTR               
250300                                   MOD-KVOI-SDC-VV-IN-ATTR                
250400                                   MOD-KVOI-SDC-DD-IN-ATTR                
250500                                   MOD-KVOI-NDC-VV-IN-ATTR                
250600                                   MOD-KVOI-NDC-DD-IN-ATTR                
250700                                   MOD-KVOI-LED-VV-IN-ATTR                
250800                                   MOD-KVOI-LED-DD-IN-ATTR                
250900                                   MOD-KVOI-REF-VV-IN-ATTR                
251000                                   MOD-KVOI-REF-DD-IN-ATTR                
251100     .                                                                    
251200     EJECT                                                                
251300* --- IMS SEKTIONER ---                                                   
251400                                                                          
251500 IMS-GET-MSG SECTION.                                                     
251600                                                                          
251700     MOVE '  QC' TO GODK-STATUSKODER                                      
251800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
251900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
252000     PERFORM IMS-STATUSKONTROLL                                           
252100     .                                                                    
252200                                                                          
252300                                                                          
252400 IMS-INSERT-MSG SECTION.                                                  
252500                                                                          
252600     IF MSGI-IDLAND-SPR = 'GB'                                            
252700       MOVE 'N' TO MFS-KDHUVOMR                                           
252800     END-IF                                                               
252900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
253000     MOVE SPACE TO GODK-STATUSKODER                                       
253100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
253200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
253300     PERFORM IMS-STATUSKONTROLL                                           
253400     .                                                                    
253500                                                                          
253600     EJECT                                                                
253700 IMS-GET-WMSGKOM-MSG SECTION.                                             
253800                                                                          
253900     MOVE '  QD'   TO GODK-STATUSKODER                                    
254000     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
254100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
254200     PERFORM IMS-STATUSKONTROLL                                           
254300     IF SEGMENT-FINNS                                                     
254400       MOVE JA TO SW-MSGKOM                                               
254500     END-IF                                                               
254600     .                                                                    
254700                                                                          
254800                                                                          
254900 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
255000                                                                          
255100     MOVE '  '  TO GODK-STATUSKODER                                       
255200     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
255300     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
255400     PERFORM IMS-STATUSKONTROLL                                           
255500     .                                                                    
255600                                                                          
255700     EJECT                                                                
255800 IMS-GU-WDL711 SECTION.                                                   
255900                                                                          
256000     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
256100          DELIMITED BY SIZE INTO SSA1                                     
256200     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
256300          DELIMITED BY SIZE INTO SSA2                                     
256400     MOVE '  GE' TO GODK-STATUSKODER                                      
256500     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2              
256600     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
256700     PERFORM IMS-STATUSKONTROLL                                           
256800     .                                                                    
256900 IMS-GHU-WDL711 SECTION.                                                  
257000                                                                          
257100     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
257200          DELIMITED BY SIZE INTO SSA1                                     
257300     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
257400          DELIMITED BY SIZE INTO SSA2                                     
257500     MOVE '  ' TO GODK-STATUSKODER                                        
257600     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2              
257700     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
257800     PERFORM IMS-STATUSKONTROLL                                           
257900     .                                                                    
258000 IMS-REPL-WDL711 SECTION.                                                 
258100                                                                          
258200     MOVE '    ' TO GODK-STATUSKODER                                      
258300     CALL CBLTDLI USING REPL OIGA-PCB DLI-IO-OIGA11                       
258400     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
258500     PERFORM IMS-STATUSKONTROLL                                           
258600     .                                                                    
258700                                                                          
258800     EJECT                                                                
258900 IMS-GHU-WDL801 SECTION.                                                  
259000                                                                          
259100     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
259200          DELIMITED BY SIZE INTO SSA1                                     
259300     MOVE '  GE' TO GODK-STATUSKODER                                      
259400     CALL CBLTDLI USING GHU OIGB-PCB DLI-IO-OIGB01 SSA1                   
259500     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
259600     PERFORM IMS-STATUSKONTROLL                                           
259700     .                                                                    
259800                                                                          
259900                                                                          
260000 IMS-ISRT-WDL801 SECTION.                                                 
260100                                                                          
260200     MOVE 'WLOIGB01     ' TO SSA1                                         
260300     MOVE '    ' TO GODK-STATUSKODER                                      
260400     CALL CBLTDLI USING ISRT OIGB-PCB DLI-IO-OIGB01 SSA1                  
260500     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
260600     PERFORM IMS-STATUSKONTROLL                                           
260700     .                                                                    
260800                                                                          
260900                                                                          
261000 IMS-REPL-WDL801 SECTION.                                                 
261100                                                                          
261200     MOVE '    ' TO GODK-STATUSKODER                                      
261300     CALL CBLTDLI USING REPL OIGB-PCB DLI-IO-OIGB01                       
261400     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
261500     PERFORM IMS-STATUSKONTROLL                                           
261600     .                                                                    
261700                                                                          
261800     EJECT                                                                
261900 IMS-GNP-WDL811 SECTION.                                                  
262000                                                                          
262100     STRING 'WLOIGB11(TIAAAA   =' W-TIAAAA-X ')'                          
262200          DELIMITED BY SIZE INTO SSA1                                     
262300     MOVE '  GE' TO GODK-STATUSKODER                                      
262400     CALL CBLTDLI USING GNP OIGB-PCB DLI-IO-OIGB11 SSA1                   
262500     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
262600     PERFORM IMS-STATUSKONTROLL                                           
262700     .                                                                    
262800                                                                          
262900                                                                          
263000 IMS-GHNP-WDL811 SECTION.                                                 
263100                                                                          
263200     STRING 'WLOIGB11*F(TIAAAA   =' W-TIAAAA-X ')'                        
263300          DELIMITED BY SIZE INTO SSA2                                     
263400     MOVE '  GE' TO GODK-STATUSKODER                                      
263500     CALL CBLTDLI USING GHNP OIGB-PCB DLI-IO-OIGB11 SSA2                  
263600     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
263700     PERFORM IMS-STATUSKONTROLL                                           
263800     .                                                                    
263900     SKIP3                                                                
264000 IMS-GHU-WDL811  SECTION.                                                 
264100                                                                          
264200     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
264300          DELIMITED BY SIZE INTO SSA1                                     
264400     STRING 'WLOIGB11(TIAAAA   =' W-TIAAAA-X ')'                          
264500          DELIMITED BY SIZE INTO SSA2                                     
264600     MOVE '  GE' TO GODK-STATUSKODER                                      
264700     CALL CBLTDLI USING GHU OIGB-PCB DLI-IO-OIGB11 SSA1 SSA2              
264800     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
264900     PERFORM IMS-STATUSKONTROLL                                           
265000     .                                                                    
265100                                                                          
265200     EJECT                                                                
265300 IMS-ISRT-WDL811  SECTION.                                                
265400                                                                          
265500     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
265600          DELIMITED BY SIZE INTO SSA1                                     
265700     MOVE 'WLOIGB11       '   TO SSA2                                     
265800     MOVE '    ' TO GODK-STATUSKODER                                      
265900     CALL CBLTDLI USING ISRT OIGB-PCB DLI-IO-OIGB11 SSA1 SSA2             
266000     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
266100     PERFORM IMS-STATUSKONTROLL                                           
266200     .                                                                    
266300                                                                          
266400                                                                          
266500 IMS-REPL-WDL811 SECTION.                                                 
266600                                                                          
266700     MOVE '    ' TO GODK-STATUSKODER                                      
266800     CALL CBLTDLI USING REPL OIGB-PCB DLI-IO-OIGB11                       
266900     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
267000     PERFORM IMS-STATUSKONTROLL                                           
267100     .                                                                    
267200                                                                          
267300     EJECT                                                                
267400 IMS-GHU-WDL811-2 SECTION.                                                
267500                                                                          
267600     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
267700          DELIMITED BY SIZE INTO SSA1                                     
267800     STRING 'WLOIGB11*F(TIAAAA   =' W-TIAAAA-2-X ')'                      
267900          DELIMITED BY SIZE INTO SSA2                                     
268000     MOVE '  GE' TO GODK-STATUSKODER                                      
268100     CALL CBLTDLI USING GHU OIGB2-PCB DLI-IO-OIGB11-2 SSA1 SSA2           
268200     MOVE OIGB2-STATUS-CODE TO STATUS-WS                                  
268300     PERFORM IMS-STATUSKONTROLL                                           
268400     .                                                                    
268500     SKIP3                                                                
268600 IMS-ISRT-WDL811-2  SECTION.                                              
268700                                                                          
268800     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
268900          DELIMITED BY SIZE INTO SSA1                                     
269000     MOVE 'WLOIGB11       '   TO SSA2                                     
269100     MOVE '    ' TO GODK-STATUSKODER                                      
269200     CALL CBLTDLI USING ISRT OIGB2-PCB DLI-IO-OIGB11-2 SSA1 SSA2          
269300     MOVE OIGB2-STATUS-CODE TO STATUS-WS                                  
269400     PERFORM IMS-STATUSKONTROLL                                           
269500     .                                                                    
269600                                                                          
269700                                                                          
269800 IMS-REPL-WDL811-2 SECTION.                                               
269900                                                                          
270000     MOVE '    ' TO GODK-STATUSKODER                                      
270100     CALL CBLTDLI USING REPL OIGB2-PCB DLI-IO-OIGB11-2                    
270200     MOVE OIGB2-STATUS-CODE TO STATUS-WS                                  
270300     PERFORM IMS-STATUSKONTROLL                                           
270400     .                                                                    
270500                                                                          
270600     EJECT                                                                
270700 IMS-GU-K601 SECTION.                                                     
270800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
270900            DELIMITED BY SIZE INTO SSA1                                   
271000     MOVE '  GE' TO GODK-STATUSKODER                                      
271100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
271200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
271300     PERFORM IMS-STATUSKONTROLL                                           
271400     .                                                                    
271500     EJECT                                                                
271600                                                                          
271700 IMS-GU-WDB601    SECTION.                                                
271800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
271900          DELIMITED BY SIZE INTO SSA1                                     
272000     MOVE '  ' TO GODK-STATUSKODER                                        
272100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
272200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
272300     PERFORM IMS-STATUSKONTROLL                                           
272400     .                                                                    
272500     EJECT                                                                
272600 IMS-GU-WDB616    SECTION.                                                
272700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
272800          DELIMITED BY SIZE INTO SSA1                                     
272900     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
273000          DELIMITED BY SIZE INTO SSA2                                     
273100     MOVE '  GE' TO GODK-STATUSKODER                                      
273200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
273300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
273400     PERFORM IMS-STATUSKONTROLL                                           
273500     .                                                                    
273600     EJECT                                                                
273700 IMS-GHU-WDK629  SECTION.                                                 
273800     MOVE 'IMS-GHU-WDK629 '  TO DBS-SECTION                               
273900                                                                          
274000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
274100          DELIMITED BY SIZE INTO SSA1                                     
274200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
274300          DELIMITED BY SIZE INTO SSA2                                     
274400     MOVE   'WDK629  '        TO SSA3                                     
274500     MOVE '  GE' TO GODK-STATUSKODER                                      
274600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
274700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
274800     PERFORM IMS-STATUSKONTROLL                                           
274900     .                                                                    
275000     SKIP3                                                                
275100 IMS-REPL-WDK629 SECTION.                                                 
275200     MOVE 'IMS-REPL-WDK629 '  TO DBS-SECTION                              
275300                                                                          
275400     MOVE '  ' TO GODK-STATUSKODER                                        
275500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
275600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
275700     PERFORM IMS-STATUSKONTROLL                                           
275800     .                                                                    
275900     EJECT                                                                
276000 IMS-GHU-WDK711     SECTION.                                              
276100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
276200          DELIMITED BY SIZE INTO SSA1                                     
276300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
276400          DELIMITED BY SIZE INTO SSA2                                     
276500     MOVE '  GE' TO GODK-STATUSKODER                                      
276600     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2           
276700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
276800     PERFORM IMS-STATUSKONTROLL                                           
276900     .                                                                    
277000     SKIP3                                                                
277100 IMS-REPL-WDK711 SECTION.                                                 
277200                                                                          
277300     MOVE '  ' TO GODK-STATUSKODER                                        
277400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-K711                    
277500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
277600     PERFORM IMS-STATUSKONTROLL                                           
277700     .                                                                    
277800     EJECT                                                                
277900 IMS-STATUSKONTROLL SECTION.                                              
278000                                                                          
278100     SET STATUS-IX TO 1                                                   
278200     SEARCH GODK-STATUS                                                   
278300       AT END                                                             
278400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
278500         DELIMITED BY SIZE INTO FELTEXT                                   
278600         CALL FELLOG                                                      
278700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
278800         CONTINUE                                                         
278900     END-SEARCH                                                           
279000     .                                                                    
279100     EJECT                                                                
279200*    -COPY WY2000P3                                                       
279300     EJECT                                                                
279400*    -COPY WY2000Q1                                                       
