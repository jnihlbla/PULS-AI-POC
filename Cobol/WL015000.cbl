000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL015000.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/08/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.SHOWRETURNCONTENTS'                        
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAMMET VISAR VAD SOM INGÅR I ETT ELLER FLERA                 
001100*        KOLLIN.                                                          
001200*                                                                         
001300*        WL015000 PROGRAM IS A REPLICA OF W4073500 PROGRAM                
001400*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: WL0150U                                             
001800*        REQUEST:     WL0150I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    WL0150O1                                            
002200*                                                                         
002300*    E-TRACKER: 4230251 2007-01                                           
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'WL015000'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500 77  WS-COUNT                    PIC 9(05) VALUE ZERO.                    
004600 77  WS-COUNT-PR                 PIC 9(05) VALUE ZERO.                    
004700 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
004800     88  REC-LIMIT                           VALUE 'J'.                   
004900 77  WS-DEL-COUNT                PIC 9(03)   VALUE ZERO.                  
005000 77  WS-INDX-REC                 PIC S9(4)   VALUE ZERO.                  
005100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
005300 77  WS-FLKLAR                   PIC X      VALUE 'J'.                    
005400                                                                          
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  YES                         PIC X       VALUE 'Y'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 77  NOO                         PIC X       VALUE 'N'.                   
005900 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
006000 01  WS-IDSNDNNR.                                                         
006100     03  WS-IDRT                 PIC X(3)    VALUE SPACE.                 
006200     03  WS-IDRTLOP              PIC X(3)    VALUE SPACE.                 
006300 77  W-ANM-UTF                   PIC X(1)    VALUE '4'.                   
006400 77  W-ANM-MOT                   PIC X(1)    VALUE '5'.                   
006500 77  W-ANM-PAAB                  PIC X(1)    VALUE '6'.                   
006600 77  W-PRINT                     PIC X(2)    VALUE 'PR'.                  
006700 77  W-DELETE                    PIC X(3)    VALUE 'DEL'.                 
006800 77  W-TEXT                      PIC X(4)    VALUE 'TEXT'.                
006900 77  RAD-IX                      PIC S9(4)   VALUE ZERO.                  
007000 77  4794-IX                     PIC S9(4)   VALUE ZERO.                  
007100 77  4794-MAX-IX                 PIC S9(4)   VALUE +500.                  
007200 77  MAX-IX                      PIC S9(5)   VALUE +500.                  
007300 77  SPARA-IDRAPPNR              PIC 9(7)    VALUE ZERO.                  
007400 77  SPARA-IDKUNDNR              PIC S9(7) COMP-3 VALUE ZERO.             
007500 77  SPARA-IDDISTR               PIC S9(5) COMP-3 VALUE ZERO.             
007600 77  ANTAL-KOLLIN                PIC S9(3) COMP-3 VALUE ZERO.             
007700 77  WS-IDDISTR-4                PIC 9(4)    VALUE ZERO.                  
007800 77  WS-IDKUNDNR-6               PIC 9(6)    VALUE ZERO.                  
007900 77  W-IDKOLLI-SPAR              PIC S9(5) COMP-3 VALUE ZERO.             
008000 77  SW-IDRT                     PIC X      VALUE 'N'.                    
008100 77  SW-IDKOLLI                  PIC X      VALUE 'N'.                    
008200 77  SW-IDDISTR                  PIC X      VALUE 'N'.                    
008300 77  SW-IDKUNDNR                 PIC X      VALUE 'N'.                    
008400 77  SW-IDRAPPNR                 PIC X      VALUE 'N'.                    
008500                                                                          
008600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008700                                                                          
008800 77  KEYS-SW                   PIC X      VALUE 'J'.                      
008900     88  KEYS-OK                          VALUE 'J'.                      
009000     88  KEYS-WRONG                       VALUE 'N'.                      
009100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009200     88  INDATA-OK                           VALUE 'J'.                   
009300     88  INDATA-FEL                          VALUE 'N'.                   
009400                                                                          
009500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009600     88  NYCKLAR-OK                          VALUE 'J'.                   
009700     88  NYCKLAR-FEL                         VALUE 'N'.                   
009800                                                                          
009900 77  IFYLLD-KEY-SW               PIC X       VALUE 'J'.                   
010000     88  NYCKLAR-FINNS                       VALUE 'J'.                   
010100     88  NYCKLAR-SAKNAS                      VALUE 'N'.                   
010200                                                                          
010300 77  IDSNDNNR-SW                 PIC X       VALUE 'J'.                   
010400     88  IDSNDNNR-FINNS                      VALUE 'J'.                   
010500     88  IDSNDNNR-FEL                        VALUE 'N'.                   
010600                                                                          
010700 77  PRINTAT-SW                  PIC X       VALUE 'N'.                   
010800     88  PRINTAT                             VALUE 'J'.                   
010900                                                                          
011000 77  IDDISTR-SW                  PIC X       VALUE 'J'.                   
011100     88  IDDISTR-FINNS                       VALUE 'J'.                   
011200     88  IDDISTR-FEL                         VALUE 'N'.                   
011300                                                                          
011400 77  IDKUNDNR-SW                 PIC X       VALUE 'J'.                   
011500     88  IDKUNDNR-FINNS                      VALUE 'J'.                   
011600     88  IDKUNDNR-FEL                        VALUE 'N'.                   
011700                                                                          
011800 77  IDRAPPNR-SW                 PIC X       VALUE 'J'.                   
011900     88  IDRAPPNR-FINNS                      VALUE 'J'.                   
012000     88  IDRAPPNR-FEL                        VALUE 'N'.                   
012100                                                                          
012200 77  IDKOLLI-SW                  PIC X       VALUE 'J'.                   
012300     88  IDKOLLI-FINNS                       VALUE 'J'.                   
012400     88  IDKOLLI-FEL                         VALUE 'N'.                   
012500                                                                          
012600 77  SW-KDCMD                    PIC X       VALUE 'N'.                   
012700     88  KDCMD-IFYLLT                        VALUE 'J'.                   
012800     88  KDCMD-SAKNAS                        VALUE 'N'.                   
012900                                                                          
013000 77  SW-KDCMD-RAETT-IFYLLD       PIC X       VALUE 'N'.                   
013100     88  KDCMD-RAETT                         VALUE 'J'.                   
013200     88  KDCMD-FEL                           VALUE 'N'.                   
013300                                                                          
013400 77  SW-IDPRT-RAETT-IFYLLD       PIC X       VALUE 'J'.                   
013500     88  IDPRT-RAETT                         VALUE 'J'.                   
013600     88  IDPRT-FEL                           VALUE 'N'.                   
013700                                                                          
013800 77  SW-KDCMD-BORTTAG            PIC X       VALUE 'N'.                   
013900     88  KDCMD-BORTTAG                       VALUE 'J'.                   
014000                                                                          
014100 77  SW-KDCMD-UTSKRIFT           PIC X       VALUE 'N'.                   
014200     88  KDCMD-UTSKRIFT                      VALUE 'J'.                   
014300                                                                          
014400 77  SW-KDCMD-TEXT               PIC X       VALUE 'N'.                   
014500     88  KDCMD-TEXT                          VALUE 'J'.                   
014600                                                                          
014700 77  WS-IDELMT-ERROR             PIC X(16)  VALUE SPACE.                  
014800 77  WS-IDMSG-ERROR              PIC X(03)  VALUE SPACE.                  
014900 77  WS-IDMSG-INFO               PIC X(03)   VALUE SPACE.                 
015000                                                                          
015100                                                                          
015200 77  W-UPDATE-SW                 PIC X       VALUE 'N'.                   
015300     88  W-UPDATE-OK                         VALUE 'J'.                   
015400                                                                          
015500 77  WS-KVLEVANM-KVAR            PIC S9(7)  VALUE 0   COMP-3.             
015600                                                                          
015700     EJECT                                                                
015800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
015900 01  GENERAL-SUBPROGRAMS.                                                 
016000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
016300     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
016400     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
016500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
016600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016700     SKIP3                                                                
016800*    --- PARAMETERS TO ABEND                                              
016900                                                                          
017000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
017200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
017300     EJECT                                                                
017400*                                                                         
017500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
017600     SKIP3                                                                
017700*01  -COPY WZ01SUB                                                        
017800     EJECT                                                                
017900 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
018000*01 -COPY W418OKOD  -PRE OKOD-.                                           
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
018300*01  -COPY WZ01AUTH                                                       
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
018600*01  -COPY WMSGCONV                                                       
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
018900     SKIP3                                                                
019000 01  REQU-AREA.                                                           
019100*    03  -COPY WZ01REQ2                                                   
019200*    03  -COPY WL0150I1                                                   
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
019500     SKIP3                                                                
019600 01  RESP-AREA.                                                           
019700*    03  -COPY WZ01RES2                                                   
019800*    03  -COPY WL0150O1                                                   
019900     EJECT                                                                
020000 01  MESSAGE-CODES.                                                       
020100     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
020200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
020300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
020400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
020500     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '178'.                 
020600     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
020700     03  ERR-UPPGIFTER-SAKNAS    PIC X(3)    VALUE '220'.                 
020800     03  ERR-ADD-CASE-INFO       PIC X(3)    VALUE '226'.                 
020900     03  NUM-OF-LINES            PIC X(3)    VALUE '028'.                 
021000     EJECT                                                                
021100 01  WS-BC-PARAMETRAR.                                                    
021200     03  WS-BC.                                                           
021300         05  BC-URV-IDDISTR      PIC 9(5)  VALUE ZERO.                    
021400         05  BC-URV-IDKUNDNR     PIC 9(7)  VALUE ZERO.                    
021500         05  BC-URV-IDRAPPNR     PIC 9(7)  VALUE ZERO.                    
021600     EJECT                                                                
021700 01  BILD-HOPP-AREOR.                                                     
021800   03    W-BILD               PIC X(4)    VALUE SPACE.                    
021900   03    W-HOPP-IDTRANS.                                                  
022000     05  FILLER               PIC X(1)    VALUE 'W'.                      
022100     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
022200     05  FILLER               PIC X(1)    VALUE 'T'.                      
022300     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
022400     05  FILLER               PIC X(2)    VALUE SPACE.                    
022500     EJECT                                                                
022600*                                                                         
022700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022800*                                                                         
022900     EJECT                                                                
023000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023100 01  NYCKLAR-TILL-DLI.                                                    
023200     03  W-IDLEVANM-X.                                                    
023300         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
023400         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
023500         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
023600                                                                          
023700     03  W-WDA3B1-MIN-X.                                                  
023800         05  W-IDRT-MIN          PIC X(3)            VALUE SPACE.         
023900         05  W-IDDC-MIN          PIC X(2)            VALUE SPACE.         
024000         05  W-IDRTLOP-MIN       PIC 9(3)            VALUE ZERO.          
024100         05  W-IDKOLLI-MIN       PIC S9(5)   COMP-3  VALUE ZERO.          
024200         05  W-DAREGDAT-MIN      PIC  9(8)           VALUE ZERO.          
024300         05  W-TIKLOCK-MIN       PIC S9(9)   COMP-3  VALUE ZERO.          
024400                                                                          
024500     03  W-WDA3B1-MAX-X.                                                  
024600         05  W-IDRT-MAX          PIC X(3)            VALUE SPACE.         
024700         05  W-IDDC-MAX          PIC X(2)            VALUE SPACE.         
024800         05  W-IDRTLOP-MAX       PIC 9(3)            VALUE ZERO.          
024900         05  W-IDKOLLI-MAX       PIC S9(5)   COMP-3  VALUE ZERO.          
025000         05  W-DAREGDAT-MAX      PIC  9(8)           VALUE ZERO.          
025100         05  W-TIKLOCK-MAX       PIC S9(9)   COMP-3  VALUE ZERO.          
025200                                                                          
025300     03  W-WDA301KY-X.                                                    
025400         05  W-IDDC              PIC  X(2)           VALUE SPACE.         
025500         05  W-DAREGDAT          PIC  9(8)           VALUE ZERO.          
025600         05  W-TIKLOCK           PIC S9(9)   COMP-3  VALUE ZERO.          
025700     SKIP2                                                                
025800     03  W-WDA3BSEQ-X.                                                    
025900         05  W-IDRT-BSEQ         PIC  X(3)          VALUE SPACE.          
026000         05  W-IDDC-BSEQ         PIC  X(2)          VALUE SPACE.          
026100         05  W-IDRTLOP-BSEQ      PIC  9(3)          VALUE ZERO.           
026200         05  W-IDKOLLI-BSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
026300                                                                          
026400     03  W-WDA3FSEQ-MIN-X.                                                
026500         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
026600         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
026700         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
026800         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
026900                                                                          
027000     03  W-WDA3FSEQ-MAX-X.                                                
027100         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
027200         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
027300         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
027400         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
027500                                                                          
027600     03  W-WDA3F1KY-MIN-X.                                                
027700         05  W-IDDC-F1KY-MIN     PIC  X(2)          VALUE SPACE.          
027800         05  W-IDDISTR-F1KY-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
027900         05  W-IDKUNDNR-F1KY-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
028000         05  W-IDRAPPNR-F1KY-MIN PIC  9(7)          VALUE ZERO.           
028100         05  W-IDRT-F1KY-MIN     PIC  X(3)          VALUE SPACE.          
028200         05  W-IDRTLOP-F1KY-MIN  PIC  9(3)          VALUE ZERO.           
028300         05  W-IDKOLLI-F1KY-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
028400         05  W-DAREGDAT-F1KY-MIN PIC  9(8)          VALUE ZERO.           
028500         05  W-TIKLOCK-F1KY-MIN  PIC S9(9)   COMP-3 VALUE ZERO.           
028600                                                                          
028700     03  W-WDA3F1KY-MAX-X.                                                
028800         05  W-IDDC-F1KY-MAX     PIC  X(2)          VALUE SPACE.          
028900         05  W-IDDISTR-F1KY-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
029000         05  W-IDKUNDNR-F1KY-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
029100         05  W-IDRAPPNR-F1KY-MAX PIC  9(7)          VALUE ZERO.           
029200         05  W-IDRT-F1KY-MAX     PIC  X(3)          VALUE SPACE.          
029300         05  W-IDRTLOP-F1KY-MAX  PIC  9(3)          VALUE ZERO.           
029400         05  W-IDKOLLI-F1KY-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
029500         05  W-DAREGDAT-F1KY-MAX PIC  9(8)          VALUE ZERO.           
029600         05  W-TIKLOCK-F1KY-MAX  PIC S9(9)   COMP-3 VALUE ZERO.           
029700                                                                          
029800     03  W-WDA3GSEQ-MIN-X.                                                
029900         05  W-IDDC-GSEQ-MIN     PIC  X(2)          VALUE SPACE.          
030000         05  W-KDRETSTA-GSEQ-MIN PIC  X(1)          VALUE SPACE.          
030100         05  W-KDARBTYP-GSEQ-MIN PIC  X(8)          VALUE SPACE.          
030200         05  W-IDPERSON-GSEQ-MIN PIC S9(3)   COMP-3 VALUE ZERO.           
030300                                                                          
030400     03  W-WDA3GSEQ-MAX-X.                                                
030500         05  W-IDDC-GSEQ-MAX     PIC  X(2)          VALUE SPACE.          
030600         05  W-KDRETSTA-GSEQ-MAX PIC  X(1)          VALUE SPACE.          
030700         05  W-KDARBTYP-GSEQ-MAX PIC  X(8)          VALUE SPACE.          
030800         05  W-IDPERSON-GSEQ-MAX PIC S9(3)   COMP-3 VALUE ZERO.           
030900                                                                          
031000     03  W-IDDC-B6-X.                                                     
031100         05 W-IDDC-B6            PIC X(2).                                
031200     EJECT                                                                
031300*    --- STATUS-KOD FRÅN IMS                                              
031400 01  STATUS-WS                   PIC XX.                                  
031500     88  STATUS-OK                           VALUE '  '.                  
031600     88  SEGMENT-FINNS                       VALUE '  '.                  
031700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
032000     88  TRANSKOD-FEL                        VALUE 'A1'.                  
032100     88  SECURITY-FEL                        VALUE 'A4'.                  
032200     SKIP2                                                                
032300 01  GODK-STATUSKODER.                                                    
032400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032500     SKIP3                                                                
032600 01  SSA1                        PIC X(128).                              
032700 01  SSA2                        PIC X(128).                              
032800     EJECT                                                                
032900*    --- IMS FUNKTIONSKODER                                               
033000*01  -COPY W0003                                                          
033100     EJECT                                                                
033200*    ---  DLI INPUT-OUTPUT AREA                                           
033300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A201'.         
033400                                                                          
033500 01  DLI-IO-A201.                                                         
033600*  03  -COPY WDA201                                                       
033700                                                                          
033800     SKIP3                                                                
033900                                                                          
034000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A211'.         
034100 01  DLI-IO-A211.                                                         
034200*    03 -COPY WDA211                                                      
034300                                                                          
034400     EJECT                                                                
034500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A301'.         
034600                                                                          
034700 01  DLI-IO-A301.                                                         
034800*  03  -COPY WDA301                                                       
034900                                                                          
035000     EJECT                                                                
035100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A3B1'.         
035200                                                                          
035300 01  DLI-IO-A3B1.                                                         
035400*  03  -COPY WDA3B1                                                       
035500                                                                          
035600     EJECT                                                                
035700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A3F1'.         
035800                                                                          
035900 01  DLI-IO-A3F1.                                                         
036000*  03  -COPY WDA3F1                                                       
036100                                                                          
036200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
036300 01   DLI-IO-AREA-B601.                                                   
036400*     03  -COPY WDB601                                                    
036500                                                                          
036600     EJECT                                                                
036700 LINKAGE SECTION.                                                         
036800 01  MSG-PCB                     PIC X.                                   
036900     EJECT                                                                
037000 01  ATAB-PCB                    PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008  -PRE KREE-                                              
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008  -PRE RETA1-                                             
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008  -PRE RETC-                                              
037900     05  FILLER                  PIC X.                                   
038000     EJECT                                                                
038100*01  -COPY W0008  -PRE RETA2-                                             
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008  -PRE RETG-                                              
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700*01  -COPY W0008  -PRE RETA3-                                             
038800     05  FILLER                  PIC X.                                   
038900     EJECT                                                                
039000*01  -COPY W0008  -PRE RETA4-                                             
039100     05  FILLER                  PIC X.                                   
039200     EJECT                                                                
039300*01  -COPY W0008  -PRE RETAG-                                             
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600*01  -COPY W0008  -PRE WDB6-                                              
039700     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB                               
040000                           KREE-PCB RETA1-PCB RETC-PCB RETA2-PCB          
040100                           RETG-PCB RETA3-PCB RETA4-PCB RETAG-PCB         
040200                           WDB6-PCB.                                      
040300 MAIN SECTION.                                                            
040400     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB                               
040500                           KREE-PCB RETA1-PCB RETC-PCB RETA2-PCB          
040600                           RETG-PCB RETA3-PCB RETA4-PCB RETAG-PCB         
040700                           WDB6-PCB.                                      
040800                                                                          
040900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
041000     IF SUB-KDRC = 0                                                      
041100       IF REQU-KDPGMACT = 'S' OR 'E'                                      
041200         PERFORM A-INIT                                                   
041300         PERFORM B-KOLLA-NYCKLAR                                          
041400         IF NYCKLAR-OK                                                    
041500           IF REQU-KDPGMACT = 'E'                                         
041600             PERFORM G-KOLLA-INPUT                                        
041700             IF INDATA-OK                                                 
041800               PERFORM H-UPPDATERA-SKRIV-UT                               
041900             END-IF                                                       
042000           END-IF                                                         
042100                                                                          
042200           IF INDATA-OK                                                   
042300             PERFORM F-LAES-VISA-INFO                                     
042400           END-IF                                                         
042500         END-IF                                                           
042600       ELSE                                                               
042700         MOVE SYSTEM-ERROR     TO RESP-IDMSG-ERROR                        
042800       END-IF                                                             
042900       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
043000       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
043100       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
043200       IF WS-IDMSG-ERROR NOT = SPACE                                      
043300         MOVE ALL '+' TO RESP-WL0150O1(1:30)                              
043400         MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                        
043500         MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                       
043600         MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                         
043700         MOVE 001              TO RESP-IDRESVER                           
043800         IF REQU-KDPGMACT = 'S'                                           
043900           MOVE ZERO             TO RESP-KVRADER-MAX1                     
044000         ELSE                                                             
044100           IF REQU-KVRADER-MAX1 NUMERIC                                   
044200             MOVE REQU-KVRADER-MAX1 TO RESP-KVRADER-MAX1                  
044300           ELSE                                                           
044400             MOVE ZERO              TO RESP-KVRADER-MAX1                  
044500           END-IF                                                         
044600         END-IF                                                           
044700       END-IF                                                             
044800       IF SUB-KDTRANS(1:6) = 'WLA150'                                     
044900         PERFORM S11-MSG-CONV                                             
045000       END-IF                                                             
045100       PERFORM S02-RETURN-RESPONSE                                        
045200                                                                          
045300     END-IF                                                               
045400                                                                          
045500     MOVE ZERO TO RETURN-CODE                                             
045600     GOBACK                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 A-INIT SECTION.                                                          
046000                                                                          
046100     MOVE LOW-VALUE  TO W-WDA3B1-MIN-X                                    
046200                        W-WDA3F1KY-MIN-X                                  
046300                        W-WDA3GSEQ-MIN-X                                  
046400     MOVE HIGH-VALUE TO W-WDA3B1-MAX-X                                    
046500                        W-WDA3F1KY-MAX-X                                  
046600                        W-WDA3GSEQ-MIN-X                                  
046700     MOVE ALL '+'    TO RESP-AREA                                         
046800     MOVE SPACE      TO RESP-IDMSG-ERROR                                  
046900                        RESP-IDMSG-INFO                                   
047000                        RESP-IDELMT-ERROR                                 
047100     MOVE 001        TO RESP-IDRESVER                                     
047200     MOVE ZERO       TO RESP-KVRADER-MAX1                                 
047300     MOVE ZERO       TO WS-COUNT                                          
047400     MOVE ZERO       TO WS-COUNT-PR                                       
047500     IF SUB-KDTRANS(1:6) = 'WLA150'                                       
047600       MOVE 001                  TO AUTH-KDCALL                           
047700       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
047800                                    REQU-WZ01REQ2                         
047900       IF AUTH-KDRC > 0                                                   
048000         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
048100         MOVE NOO                TO KEYS-SW                               
048200       END-IF                                                             
048300       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
048400                                 REQU-IDDC-KEY                            
048500       MOVE FUNCTION UPPER-CASE (REQU-IDRT-KEY) TO                        
048600                                 REQU-IDRT-KEY                            
048700       MOVE +1 TO INDX                                                    
048800       PERFORM UNTIL INDX > MAX-INDX                                      
048900        MOVE FUNCTION UPPER-CASE (REQU-TERETNOT(INDX)) TO                 
049000                                  REQU-TERETNOT(INDX)                     
049100        MOVE FUNCTION UPPER-CASE (REQU-KDCMD(INDX)) TO                    
049200                                  REQU-KDCMD(INDX)                        
049300        ADD +1 TO INDX                                                    
049400       END-PERFORM                                                        
049500     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800 B-KOLLA-NYCKLAR SECTION.                                                 
049900                                                                          
050000     IF REQU-IDRT-KEY NOT = ALL '+'  AND                                  
050100                REQU-IDRTLOP-KEY NOT =  ALL '+'                           
050200       MOVE REQU-IDRT-KEY       TO WS-IDRT                                
050300       MOVE REQU-IDRTLOP-KEY    TO WS-IDRTLOP                             
050400     END-IF                                                               
050500                                                                          
050600     MOVE JA TO NYCKLAR-SW                                                
050700     MOVE REQU-IDDC-KEY                   TO W-IDDC                       
050800                                             W-IDDC-MIN                   
050900                                             W-IDDC-MAX                   
051000                                             W-IDDC-BSEQ                  
051100                                             W-IDDC-FSEQ-MIN              
051200                                             W-IDDC-FSEQ-MAX              
051300                                             W-IDDC-F1KY-MIN              
051400                                             W-IDDC-F1KY-MAX              
051500                                             RESP-IDDC-KEY                
051600     PERFORM BA-KOLLA-ISNDNNR-IDKOLLI                                     
051700     PERFORM BB-KOLLA-IDDISTR                                             
051800     PERFORM BC-KOLLA-IDKUNDNR                                            
051900     PERFORM BD-KOLLA-IDRAPPNR                                            
052000                                                                          
052100     IF SW-IDRT     = 'N' AND                                             
052200        SW-IDKOLLI  = 'N' AND                                             
052300        SW-IDDISTR  = 'N' AND                                             
052400        SW-IDKUNDNR = 'N' AND                                             
052500        SW-IDRAPPNR = 'N'                                                 
052600        MOVE NEJ TO IFYLLD-KEY-SW                                         
052700     END-IF                                                               
052800     IF SW-IDRT = 'J' AND                                                 
052900       (SW-IDDISTR  = 'J' OR                                              
053000        SW-IDKUNDNR = 'J' OR                                              
053100        SW-IDRAPPNR = 'J')                                                
053200       MOVE NEJ                   TO NYCKLAR-SW                           
053300       MOVE '032'                 TO RESP-IDMSG-ERROR                     
053400     END-IF                                                               
053500     IF SW-IDKOLLI = 'J' AND                                              
053600       (SW-IDDISTR  = 'J' OR                                              
053700        SW-IDKUNDNR = 'J' OR                                              
053800        SW-IDRAPPNR = 'J')                                                
053900       MOVE NEJ                   TO NYCKLAR-SW                           
054000       MOVE '032'                 TO RESP-IDMSG-ERROR                     
054100     END-IF                                                               
054200     IF SW-IDDISTR = 'J' AND                                              
054300       (SW-IDKUNDNR NOT = 'J' OR                                          
054400        SW-IDRAPPNR NOT = 'J')                                            
054500       MOVE NEJ                   TO NYCKLAR-SW                           
054600       MOVE '032'                 TO RESP-IDMSG-ERROR                     
054700     END-IF                                                               
054800     IF SW-IDKUNDNR = 'J' AND                                             
054900       (SW-IDDISTR  NOT = 'J' OR                                          
055000        SW-IDRAPPNR NOT = 'J')                                            
055100       MOVE NEJ                   TO NYCKLAR-SW                           
055200       MOVE '032'                 TO RESP-IDMSG-ERROR                     
055300     END-IF                                                               
055400     IF SW-IDRAPPNR = 'J' AND                                             
055500       (SW-IDDISTR  NOT = 'J' OR                                          
055600        SW-IDKUNDNR NOT = 'J')                                            
055700       MOVE NEJ                   TO NYCKLAR-SW                           
055800       MOVE '032'                 TO RESP-IDMSG-ERROR                     
055900     END-IF                                                               
056000                                                                          
056100     IF NYCKLAR-OK                                                        
056200       MOVE REQU-IDRT-KEY         TO WS-IDRT                              
056300       MOVE REQU-IDRTLOP-KEY      TO WS-IDRTLOP                           
056400                                                                          
056500       IF WS-IDRTLOP = ZERO                                               
056600         MOVE SPACE               TO RESP-IDRT-KEY                        
056700         MOVE ZERO                TO RESP-IDRTLOP-KEY                     
056800       ELSE                                                               
056900         MOVE WS-IDRT             TO RESP-IDRT-KEY                        
057000         IF WS-IDRTLOP NUMERIC                                            
057100           MOVE WS-IDRTLOP        TO RESP-IDRTLOP-KEY                     
057200         END-IF                                                           
057300       END-IF                                                             
057400                                                                          
057500       IF REQU-IDKOLLI-KEY  NUMERIC                                       
057600         MOVE REQU-IDKOLLI-KEY    TO RESP-IDKOLLI-KEY                     
057700         INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE         
057800       END-IF                                                             
057900                                                                          
058000       IF REQU-IDDISTR-KEY NUMERIC                                        
058100         MOVE REQU-IDDISTR-KEY    TO RESP-IDDISTR-KEY                     
058200         INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE         
058300       END-IF                                                             
058400                                                                          
058500       IF REQU-IDKUNDNR-KEY  NUMERIC                                      
058600         MOVE REQU-IDKUNDNR-KEY   TO RESP-IDKUNDNR-KEY                    
058700         INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE        
058800       END-IF                                                             
058900                                                                          
059000       IF REQU-IDRAPPNR-KEY  NUMERIC                                      
059100         MOVE REQU-IDRAPPNR-KEY     TO RESP-IDRAPPNR-KEY                  
059200         INSPECT RESP-IDRAPPNR-KEY REPLACING LEADING ZERO BY SPACE        
059300       END-IF                                                             
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 BA-KOLLA-ISNDNNR-IDKOLLI SECTION.                                        
059800     SKIP2                                                                
059900                                                                          
060000     IF  REQU-IDRT-KEY   NOT = SPACE                                      
060100     AND REQU-IDRT-KEY   NOT = '+++'                                      
060200       MOVE REQU-IDRT-KEY TO W-IDRT-MIN                                   
060300                             W-IDRT-MAX                                   
060400       MOVE JA            TO SW-IDRT                                      
060500     ELSE                                                                 
060600       MOVE NEJ TO IDSNDNNR-SW                                            
060700     END-IF                                                               
060800                                                                          
060900     IF REQU-IDRTLOP-KEY NUMERIC                                          
061000     AND REQU-IDRTLOP-KEY > 0                                             
061100       MOVE REQU-IDRTLOP-KEY TO W-IDRTLOP-MIN                             
061200                                W-IDRTLOP-MAX                             
061300       MOVE JA            TO SW-IDRT                                      
061400     ELSE                                                                 
061500       MOVE NEJ TO IDSNDNNR-SW                                            
061600     END-IF                                                               
061700                                                                          
061800     MOVE REQU-IDRT-KEY     TO WS-IDRT                                    
061900     MOVE REQU-IDRTLOP-KEY  TO WS-IDRTLOP                                 
062000     MOVE WS-IDSNDNNR(1:3)  TO RESP-IDRT-KEY                              
062100                                                                          
062200     IF   WS-IDSNDNNR(4:3) NUMERIC                                        
062300       MOVE WS-IDSNDNNR(4:3)  TO RESP-IDRTLOP-KEY                         
062400     END-IF                                                               
062500                                                                          
062600     IF REQU-IDKOLLI-KEY NUMERIC AND                                      
062700       REQU-IDKOLLI-KEY > ZERO                                            
062800       MOVE REQU-IDKOLLI-KEY TO W-IDKOLLI-MIN                             
062900                                W-IDKOLLI-MAX                             
063000       MOVE JA            TO SW-IDKOLLI                                   
063100     ELSE                                                                 
063200       MOVE ZERO          TO W-IDKOLLI-MIN                                
063300       MOVE 99999         TO W-IDKOLLI-MAX                                
063400       MOVE NEJ TO IDKOLLI-SW                                             
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 BB-KOLLA-IDDISTR SECTION.                                                
063900     SKIP2                                                                
064000     IF REQU-IDDISTR-KEY = ALL '+'                                        
064100       MOVE NEJ TO IDDISTR-SW                                             
064200     END-IF                                                               
064300                                                                          
064400     IF REQU-IDDISTR-KEY NUMERIC                                          
064500     AND REQU-IDDISTR-KEY > 0                                             
064600       MOVE REQU-IDDISTR-KEY TO W-IDDISTR-FSEQ-MIN                        
064700                                W-IDDISTR-FSEQ-MAX                        
064800       MOVE JA  TO SW-IDDISTR                                             
064900     ELSE                                                                 
065000       MOVE NEJ TO IDDISTR-SW                                             
065100     END-IF                                                               
065200     .                                                                    
065300                                                                          
065400 BC-KOLLA-IDKUNDNR SECTION.                                               
065500     SKIP2                                                                
065600     IF REQU-IDKUNDNR-KEY = ALL '+'                                       
065700       MOVE NEJ TO IDKUNDNR-SW                                            
065800     END-IF                                                               
065900                                                                          
066000     IF REQU-IDKUNDNR-KEY NUMERIC                                         
066100     AND REQU-IDKUNDNR-KEY > 0                                            
066200       MOVE REQU-IDKUNDNR-KEY TO W-IDKUNDNR-FSEQ-MIN                      
066300                                 W-IDKUNDNR-FSEQ-MAX                      
066400       MOVE JA  TO SW-IDKUNDNR                                            
066500     ELSE                                                                 
066600       MOVE NEJ TO IDKUNDNR-SW                                            
066700     END-IF                                                               
066800     .                                                                    
066900                                                                          
067000 BD-KOLLA-IDRAPPNR SECTION.                                               
067100     SKIP2                                                                
067200     IF REQU-IDRAPPNR-KEY = ALL '+'                                       
067300       MOVE NEJ TO IDRAPPNR-SW                                            
067400     END-IF                                                               
067500                                                                          
067600     IF REQU-IDRAPPNR-KEY NUMERIC                                         
067700     AND REQU-IDRAPPNR-KEY > 0                                            
067800       MOVE REQU-IDRAPPNR-KEY TO W-IDRAPPNR-FSEQ-MIN                      
067900                                 W-IDRAPPNR-FSEQ-MAX                      
068000       MOVE JA  TO SW-IDRAPPNR                                            
068100     ELSE                                                                 
068200       MOVE NEJ TO IDRAPPNR-SW                                            
068300     END-IF                                                               
068400     .                                                                    
068500     EJECT                                                                
068600 F-LAES-VISA-INFO SECTION.                                                
068700                                                                          
068800     IF NYCKLAR-FINNS                                                     
068900        IF IDSNDNNR-FINNS                                                 
069000           PERFORM FA-VISA-IDSNDNNR-IDKOLLI                               
069100        ELSE                                                              
069200           PERFORM FB-VISA-DISTR-KUND-RAPPNR                              
069300        END-IF                                                            
069400     ELSE                                                                 
069500        PERFORM FC-VISA-PER-IDANSV                                        
069600     END-IF                                                               
069700     .                                                                    
069800                                                                          
069900 FA-VISA-IDSNDNNR-IDKOLLI SECTION.                                        
070000     SKIP2                                                                
070100     PERFORM IMS-GU-WLRETC01                                              
070200     IF SEGMENT-FINNS                                                     
070300       PERFORM FAA-VISA-IDKOLLI-LEVANM                                    
070400     ELSE                                                                 
070500       MOVE 'IDRT-IDRTLOP'   TO RESP-IDELMT-ERROR                         
070600       MOVE '025'            TO RESP-IDMSG-ERROR                          
070700     END-IF                                                               
070800     .                                                                    
070900                                                                          
071000 FAA-VISA-IDKOLLI-LEVANM SECTION.                                         
071100     SKIP2                                                                
071200***  WDA2 LÄSES BARA VID VARJE NY FÖREKOMST AV DISTRIKT/KUND/             
071300***    RAPPORTNUMMER (FÖR ATT MINSKA ANTALET LÄSNINGAR)                   
071400***                                                                       
071500     MOVE SEQB-DAREGDAT TO W-DAREGDAT                                     
071600     MOVE SEQB-TIKLOCK  TO W-TIKLOCK                                      
071700     PERFORM IMS-GU-WLRETA01                                              
071800     IF SEGMENT-FINNS AND RET-IDDISTR > ZERO                              
071900       MOVE RET-IDDISTR  TO W-IDDISTR                                     
072000                            SPARA-IDDISTR                                 
072100                            W-IDDISTR-F1KY-MIN                            
072200                            W-IDDISTR-F1KY-MAX                            
072300       MOVE RET-IDKUNDNR TO W-IDKUNDNR                                    
072400                            SPARA-IDKUNDNR                                
072500                            W-IDKUNDNR-F1KY-MIN                           
072600                            W-IDKUNDNR-F1KY-MAX                           
072700       MOVE RET-IDRAPPNR TO W-IDRAPPNR                                    
072800                            SPARA-IDRAPPNR                                
072900                            W-IDRAPPNR-F1KY-MIN                           
073000                            W-IDRAPPNR-F1KY-MAX                           
073100       PERFORM IMS-GU-WLKREE01                                            
073200                                                                          
073300       IF SEGMENT-FINNS                                                   
073400                                                                          
073500         MOVE +1 TO RAD-IX                                                
073600         PERFORM UNTIL RAD-IX > MAX-IX                                    
073700           IF SEGMENT-FINNS                                               
073800             IF SUB-KDTRANS(1:6) = 'WLA150'                               
073900               PERFORM S03-KALK-KVANT-KVAR                                
073910             END-IF                                                       
074000             PERFORM FAAC-REDIGERA-MOD                                    
074100             PERFORM S02-KOLLA-OM-FLERA-KOLLIN                            
074200             PERFORM IMS-GN-WLRETC01                                      
074300                                                                          
074400             IF SEGMENT-FINNS                                             
074500               MOVE SEQB-DAREGDAT TO W-DAREGDAT                           
074600               MOVE SEQB-TIKLOCK  TO W-TIKLOCK                            
074700               PERFORM IMS-GU-WLRETA01                                    
074800               IF SEGMENT-FINNS                                           
074900                 IF RET-IDDISTR = SPARA-IDDISTR                           
075000                 AND RET-IDKUNDNR = SPARA-IDKUNDNR                        
075100                 AND RET-IDRAPPNR = SPARA-IDRAPPNR                        
075200                   CONTINUE                                               
075300                 ELSE                                                     
075400                   MOVE RET-IDDISTR  TO W-IDDISTR                         
075500                                        SPARA-IDDISTR                     
075600                                        W-IDDISTR-F1KY-MIN                
075700                                        W-IDDISTR-F1KY-MAX                
075800                   MOVE RET-IDKUNDNR TO W-IDKUNDNR                        
075900                                        SPARA-IDKUNDNR                    
076000                                        W-IDKUNDNR-F1KY-MIN               
076100                                        W-IDKUNDNR-F1KY-MAX               
076200                   MOVE RET-IDRAPPNR TO W-IDRAPPNR                        
076300                                        SPARA-IDRAPPNR                    
076400                                        W-IDRAPPNR-F1KY-MIN               
076500                                        W-IDRAPPNR-F1KY-MAX               
076600                   PERFORM IMS-GU-WLKREE01                                
076700                 END-IF                                                   
076800               END-IF                                                     
076900             END-IF                                                       
077000           END-IF                                                         
077100           ADD +1 TO RAD-IX                                               
077200         END-PERFORM                                                      
077300                                                                          
077400       ELSE                                                               
077500         MOVE 'IDLEVANM'       TO RESP-IDELMT-ERROR                       
077600         MOVE '025'            TO RESP-IDMSG-ERROR                        
077700       END-IF                                                             
077800     ELSE                                                                 
077900       MOVE 'IDLEVANM'       TO RESP-IDELMT-ERROR                         
078000       MOVE '025'            TO RESP-IDMSG-ERROR                          
078100     END-IF                                                               
078200     .                                                                    
078300     EJECT                                                                
078400 FAAC-REDIGERA-MOD SECTION.                                               
078500     SKIP2                                                                
078600     MOVE RET-IDKOLLI         TO RESP-IDKOLLI  (RAD-IX)                   
078700     MOVE RET-IDRETSND        TO RESP-IDRETSND (RAD-IX)                   
078800     MOVE RET-IDDISTR         TO RESP-IDDISTR  (RAD-IX)                   
078900     MOVE RET-IDKUNDNR        TO RESP-IDKUNDNR (RAD-IX)                   
079000     MOVE RET-IDRAPPNR        TO RESP-IDRAPPNR (RAD-IX)                   
079100     IF RET-FLFARLIG = JA                                                 
079200       MOVE YES               TO RESP-FLFARLIG (RAD-IX)                   
079300     ELSE                                                                 
079400       MOVE NEJ               TO RESP-FLFARLIG (RAD-IX)                   
079500     END-IF                                                               
079600                                                                          
079700*TEST FÖR ATT EJ RENSA IFYLLD RAD VID ENTER IST. FÖR PF11                 
079800         MOVE RET-TERETNOT    TO RESP-TERETNOT (RAD-IX)                   
079900                                                                          
080000     MOVE ANM-KDLEVANM        TO RESP-KDLEVANM (RAD-IX)                   
080100                                                                          
080200     ADD +1 TO WS-COUNT                                                   
080300     IF WS-COUNT < 501                                                    
080400       CONTINUE                                                           
080500     ELSE                                                                 
080600       MOVE NUM-OF-LINES       TO RESP-IDMSG-ERROR                        
080700     END-IF                                                               
080800     MOVE WS-COUNT             TO RESP-KVRADER-MAX1                       
080900     .                                                                    
081000     EJECT                                                                
081100 FB-VISA-DISTR-KUND-RAPPNR SECTION.                                       
081200     SKIP2                                                                
081300     PERFORM IMS-GU-SEQF-WLRETA01                                         
081400     IF SEGMENT-FINNS AND RET-IDDISTR > ZERO AND                          
081500        W-IDDISTR-FSEQ-MIN  > ZERO                                        
081600***** DISTR > ZERO TAR HAND OM SÄNDNINGAR UTAN RAPPORTER                  
081700       PERFORM FBB-VISA-DIST-KUND-RAPP                                    
081800     ELSE                                                                 
081900       MOVE 'IDRETILL'        TO RESP-IDELMT-ERROR                        
082000       MOVE '025'             TO RESP-IDMSG-ERROR                         
082100     END-IF                                                               
082200     .                                                                    
082300                                                                          
082400 FBB-VISA-DIST-KUND-RAPP SECTION.                                         
082500     SKIP2                                                                
082600     MOVE RET-IDDISTR         TO W-IDDISTR                                
082700     MOVE RET-IDKUNDNR        TO W-IDKUNDNR                               
082800     MOVE RET-IDRAPPNR        TO W-IDRAPPNR                               
082900     PERFORM IMS-GU-WLKREE01                                              
083000                                                                          
083100     MOVE +1 TO RAD-IX                                                    
083200     PERFORM UNTIL RAD-IX > MAX-IX OR SEGMENT-SAKNAS                      
083300       IF SEGMENT-FINNS                                                   
083310         IF SUB-KDTRANS(1:6) = 'WLA150'                                   
083400           IF RAD-IX = 1                                                  
083500             PERFORM S03-KALK-KVANT-KVAR                                  
083600           ELSE                                                           
083700             MOVE WS-FLKLAR TO RESP-FLKLAR (INDX)                         
083800           END-IF                                                         
083810         END-IF                                                           
083900                                                                          
084000         MOVE RET-IDKOLLI         TO RESP-IDKOLLI  (RAD-IX)               
084100         MOVE RET-IDRETSND        TO RESP-IDRETSND (RAD-IX)               
084200         MOVE RET-IDDISTR         TO RESP-IDDISTR  (RAD-IX)               
084300         MOVE RET-IDKUNDNR        TO RESP-IDKUNDNR (RAD-IX)               
084400         MOVE RET-IDRAPPNR        TO RESP-IDRAPPNR (RAD-IX)               
084500         IF RET-FLFARLIG = JA                                             
084600           MOVE YES               TO RESP-FLFARLIG (RAD-IX)               
084700         ELSE                                                             
084800           MOVE NEJ               TO RESP-FLFARLIG (RAD-IX)               
084900         END-IF                                                           
085000         MOVE RET-TERETNOT        TO RESP-TERETNOT (RAD-IX)               
085100         MOVE ANM-KDLEVANM        TO RESP-KDLEVANM (RAD-IX)               
085200                                                                          
085300         PERFORM IMS-GN-SEQF-WLRETA01                                     
085400         ADD 1                    TO RAD-IX                               
085500                                     WS-COUNT                             
085600       END-IF                                                             
085700                                                                          
085800       IF WS-COUNT < 501                                                  
085900         CONTINUE                                                         
086000       ELSE                                                               
086100         MOVE NUM-OF-LINES       TO RESP-IDMSG-ERROR                      
086200       END-IF                                                             
086300                                                                          
086400       MOVE WS-COUNT            TO RESP-KVRADER-MAX1                      
086500     END-PERFORM                                                          
086600     .                                                                    
086700     EJECT                                                                
086800 FC-VISA-PER-IDANSV SECTION.                                              
086900                                                                          
087000*    -- IDANSV HÄMTAS FRÅN WDB6                                           
087100                                                                          
087200                                                                          
087300     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
087400     PERFORM IMS-GU-WDB601                                                
087500                                                                          
087600     MOVE REQU-IDDC-KEY        TO  W-IDDC-GSEQ-MIN                        
087700                                   W-IDDC-GSEQ-MAX                        
087800     MOVE '4'                  TO  W-KDRETSTA-GSEQ-MIN                    
087900     MOVE '5'                  TO  W-KDRETSTA-GSEQ-MAX                    
088000     MOVE 'RET'                TO  W-KDARBTYP-GSEQ-MIN                    
088100                                   W-KDARBTYP-GSEQ-MAX                    
088200     MOVE DCS-IDPERSON-REM     TO  W-IDPERSON-GSEQ-MIN                    
088300                                   W-IDPERSON-GSEQ-MAX                    
088400                                                                          
088500     MOVE +1 TO RAD-IX                                                    
088600     PERFORM IMS-GU-SEQG-WDA301                                           
088700     PERFORM UNTIL RAD-IX > MAX-IX                                        
088800                OR SEGMENT-SAKNAS                                         
088900        MOVE RET-IDKOLLI         TO RESP-IDKOLLI  (RAD-IX)                
089000        MOVE RET-IDRETSND        TO RESP-IDRETSND (RAD-IX)                
089100        MOVE RET-IDDISTR         TO RESP-IDDISTR  (RAD-IX)                
089200        MOVE RET-IDKUNDNR        TO RESP-IDKUNDNR (RAD-IX)                
089300        MOVE RET-IDRAPPNR        TO RESP-IDRAPPNR (RAD-IX)                
089400        IF RET-FLFARLIG = JA                                              
089500          MOVE YES               TO RESP-FLFARLIG (RAD-IX)                
089600        ELSE                                                              
089700          MOVE NEJ               TO RESP-FLFARLIG (RAD-IX)                
089800        END-IF                                                            
089900        MOVE RET-TERETNOT        TO RESP-TERETNOT (RAD-IX)                
090000                                                                          
090100        MOVE RET-IDDISTR         TO W-IDDISTR                             
090200        MOVE RET-IDKUNDNR        TO W-IDKUNDNR                            
090300        MOVE RET-IDRAPPNR        TO W-IDRAPPNR                            
090400        PERFORM IMS-GU-WLKREE01                                           
090500        IF SEGMENT-FINNS                                                  
090600           MOVE ANM-KDLEVANM     TO RESP-KDLEVANM (RAD-IX)                
090610           IF SUB-KDTRANS(1:6) = 'WLA150'                                 
090700             PERFORM S03-KALK-KVANT-KVAR                                  
090710           END-IF                                                         
090800        END-IF                                                            
090900                                                                          
091000        PERFORM IMS-GN-SEQG-WDA301                                        
091100        ADD 1                    TO RAD-IX                                
091200                                    WS-COUNT                              
091300     END-PERFORM                                                          
091400                                                                          
091500     IF WS-COUNT < 501                                                    
091600        CONTINUE                                                          
091700     ELSE                                                                 
091800        MOVE NUM-OF-LINES TO RESP-IDMSG-ERROR                             
091900     END-IF                                                               
092000     MOVE WS-COUNT        TO RESP-KVRADER-MAX1                            
092100     .                                                                    
092200     EJECT                                                                
092300 G-KOLLA-INPUT SECTION.                                                   
092400                                                                          
092500     MOVE JA  TO INDATA-SW                                                
092600                 SW-IDPRT-RAETT-IFYLLD                                    
092700     MOVE NEJ TO SW-KDCMD                                                 
092800                 SW-KDCMD-BORTTAG                                         
092900                 SW-KDCMD-UTSKRIFT                                        
093000                 SW-KDCMD-TEXT                                            
093100                 SW-KDCMD-RAETT-IFYLLD                                    
093200                                                                          
093300                                                                          
093400     PERFORM GA-FORMELL-KONTROLL                                          
093500     IF INDATA-OK                                                         
093600        PERFORM GB-LOGISK-KONTROLL                                        
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
094000 GA-FORMELL-KONTROLL SECTION.                                             
094100                                                                          
094200     PERFORM GAA-KOLLA-KDCMD                                              
094300     IF INDATA-OK                                                         
094400       MOVE JA   TO SW-KDCMD-RAETT-IFYLLD                                 
094500     END-IF                                                               
094600                                                                          
094700     IF (KDCMD-UTSKRIFT AND KDCMD-BORTTAG)                                
094800     OR (KDCMD-UTSKRIFT AND KDCMD-TEXT)                                   
094900     OR (KDCMD-BORTTAG  AND KDCMD-TEXT)                                   
095000       MOVE NEJ                  TO INDATA-SW                             
095100       MOVE ERR-FLERA-FUNKTIONER TO RESP-IDMSG-ERROR                      
095200     END-IF                                                               
095300     IF KDCMD-SAKNAS                                                      
095400       MOVE NEJ                  TO INDATA-SW                             
095500     END-IF                                                               
095600                                                                          
095700     IF KDCMD-SAKNAS                                                      
095800       IF IDSNDNNR-FINNS AND IDKOLLI-FINNS                                
095900         CONTINUE                                                         
096000       ELSE                                                               
096100         MOVE NEJ                        TO INDATA-SW                     
096200         MOVE '014'                      TO RESP-IDMSG-ERROR              
096300         MOVE NEJ  TO SW-IDPRT-RAETT-IFYLLD                               
096400       END-IF                                                             
096500     END-IF                                                               
096600     .                                                                    
096700                                                                          
096800 GAA-KOLLA-KDCMD      SECTION.                                            
096900                                                                          
097000     IF REQU-KVRADER-MAX1 NUMERIC AND REQU-KVRADER-MAX1 > 0               
097100       MOVE REQU-KVRADER-MAX1             TO WS-INDX-REC                  
097200       MOVE NEJ                           TO WS-REC-LIMIT                 
097300       MOVE +1                            TO RAD-IX                       
097400                                                                          
097500       PERFORM UNTIL RAD-IX          >  MAX-IX  OR REC-LIMIT              
097600         IF REQU-KDCMD(RAD-IX)       NOT = ALL '+' AND SPACE              
097700                                                                          
097800           IF REQU-KDCMD(RAD-IX)     =  W-PRINT                           
097900             MOVE JA                   TO SW-KDCMD                        
098000                                          SW-KDCMD-UTSKRIFT               
098100             ADD +1                   TO WS-COUNT-PR                      
098200              IF WS-COUNT-PR < 11                                         
098300                CONTINUE                                                  
098400              ELSE                                                        
098500                MOVE '274'       TO RESP-IDMSG-ERROR                      
098600                MOVE NEJ         TO INDATA-SW                             
098700              END-IF                                                      
098800           ELSE                                                           
098900             IF REQU-KDCMD(RAD-IX)      =  W-DELETE                       
099000               MOVE JA TO SW-KDCMD                                        
099100                          SW-KDCMD-BORTTAG                                
099200             ELSE                                                         
099300               IF REQU-KDCMD(RAD-IX)  = W-TEXT                            
099400                                                                          
099500                 IF REQU-TERETNOT(RAD-IX)  = ALL '+'                      
099600                   MOVE ERR-ADD-CASE-INFO  TO RESP-IDMSG-INFO             
099700                   MOVE NEJ TO INDATA-SW                                  
099800                 END-IF                                                   
099900                 MOVE JA TO SW-KDCMD                                      
100000                            SW-KDCMD-TEXT                                 
100100               END-IF                                                     
100200             END-IF                                                       
100300           END-IF                                                         
100400         END-IF                                                           
100500         IF RAD-IX = WS-INDX-REC                                          
100600           MOVE JA TO WS-REC-LIMIT                                        
100700         ELSE                                                             
100800           ADD +1 TO RAD-IX                                               
100900         END-IF                                                           
101000       END-PERFORM                                                        
101100     ELSE                                                                 
101200       MOVE NEJ           TO INDATA-SW                                    
101300       IF REQU-KVRADER-MAX1 = 0                                           
101400          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
101500          MOVE '126'     TO RESP-IDMSG-ERROR                              
101600       ELSE                                                               
101700          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
101800          MOVE '024'     TO RESP-IDMSG-ERROR                              
101900       END-IF                                                             
102000     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102300 GB-LOGISK-KONTROLL SECTION.                                              
102400                                                                          
102500     PERFORM GBB-KOLLA-STATUS                                             
102600     .                                                                    
102700                                                                          
102800                                                                          
102900 GBB-KOLLA-STATUS                 SECTION.                                
103000                                                                          
103100     MOVE +1   TO RAD-IX                                                  
103200     MOVE NEJ  TO WS-REC-LIMIT                                            
103300                                                                          
103400     PERFORM UNTIL RAD-IX  >  MAX-IX                                      
103500                OR REC-LIMIT                                              
103600                                                                          
103700       IF REQU-KDCMD (RAD-IX) = W-DELETE                                  
103800         INSPECT REQU-IDDISTR(RAD-IX) REPLACING                           
103900                                      LEADING SPACE BY ZERO               
104000         INSPECT REQU-IDKUNDNR(RAD-IX) REPLACING                          
104100                                       LEADING SPACE BY ZERO              
104200         INSPECT REQU-IDRAPPNR(RAD-IX) REPLACING                          
104300                                       LEADING SPACE BY ZERO              
104400         IF  REQU-IDDISTR (RAD-IX) NUMERIC                                
104500         AND REQU-IDDISTR (RAD-IX) > ZERO                                 
104600         AND REQU-IDKUNDNR(RAD-IX) NUMERIC                                
104700         AND REQU-IDRAPPNR(RAD-IX) NUMERIC                                
104800           MOVE REQU-IDDISTR(RAD-IX) TO W-IDDISTR                         
104900           MOVE REQU-IDKUNDNR(RAD-IX) TO W-IDKUNDNR                       
105000           MOVE REQU-IDRAPPNR(RAD-IX) TO W-IDRAPPNR                       
105100                                                                          
105200           PERFORM IMS-GU-WLKREE01                                        
105300           IF ANM-KDLEVANM = W-ANM-UTF                                    
105400             CONTINUE                                                     
105500           ELSE                                                           
105600             MOVE NEJ        TO INDATA-SW                                 
105700             MOVE REQU-KDCMD(RAD-IX) TO RESP-KDCMD(RAD-IX)                
105800             MOVE REQU-TERETNOT(RAD-IX) TO REQU-TERETNOT(RAD-IX)          
105900             MOVE '230'      TO RESP-IDMSG-ERROR-LINE (RAD-IX)            
106000                                RESP-IDMSG-ERROR                          
106100             MOVE NEJ  TO SW-KDCMD-RAETT-IFYLLD                           
106200           END-IF                                                         
106300         ELSE                                                             
106400           MOVE NEJ    TO INDATA-SW                                       
106500           MOVE REQU-KDCMD(RAD-IX) TO RESP-KDCMD(RAD-IX)                  
106600           MOVE REQU-TERETNOT(RAD-IX) TO REQU-TERETNOT(RAD-IX)            
106700           MOVE '231'  TO RESP-IDMSG-ERROR-LINE (RAD-IX)                  
106800                          RESP-IDMSG-ERROR                                
106900           MOVE NEJ  TO SW-KDCMD-RAETT-IFYLLD                             
107000         END-IF                                                           
107100       END-IF                                                             
107200       IF RAD-IX = WS-INDX-REC                                            
107300         MOVE JA TO WS-REC-LIMIT                                          
107400       ELSE                                                               
107500         ADD +1                       TO RAD-IX                           
107600       END-IF                                                             
107700     END-PERFORM                                                          
107800     .                                                                    
107900     EJECT                                                                
108000 H-UPPDATERA-SKRIV-UT SECTION.                                            
108100                                                                          
108200     IF KDCMD-SAKNAS                                                      
108300       PERFORM HB-PRINTA-TILLSTAND-KOLLI                                  
108400     ELSE                                                                 
108500       PERFORM HA-BEHANDLA-VALDA-TILLSTAND                                
108600     END-IF                                                               
108700                                                                          
108800     MOVE  INF-UPDATE-DONE      TO RESP-IDMSG-INFO                        
108900                                                                          
109000     .                                                                    
109100 HA-BEHANDLA-VALDA-TILLSTAND SECTION.                                     
109200                                                                          
109300     MOVE +1          TO RAD-IX                                           
109400                         4794-IX                                          
109500     MOVE NEJ         TO WS-REC-LIMIT                                     
109600     MOVE ZERO        TO WS-DEL-COUNT                                     
109700                                                                          
109800     PERFORM UNTIL RAD-IX    >  MAX-IX OR REC-LIMIT                       
109900                                                                          
110000       IF REQU-KDCMD(RAD-IX) = W-PRINT                                    
110100                           OR  W-DELETE                                   
110200                           OR  W-TEXT                                     
110300                           OR  KDCMD-SAKNAS                               
110400         INSPECT REQU-IDDISTR(RAD-IX) REPLACING                           
110500                                      LEADING SPACE BY ZERO               
110600         INSPECT REQU-IDKUNDNR(RAD-IX) REPLACING                          
110700                                       LEADING SPACE BY ZERO              
110800         INSPECT REQU-IDRAPPNR(RAD-IX) REPLACING                          
110900                                       LEADING SPACE BY ZERO              
111000         INSPECT REQU-IDKOLLI (RAD-IX) REPLACING                          
111100                                       LEADING SPACE BY ZERO              
111200                                                                          
111300         IF  REQU-IDDISTR (RAD-IX) NUMERIC                                
111400         AND REQU-IDDISTR (RAD-IX) > ZERO                                 
111500         AND REQU-IDKUNDNR(RAD-IX) NUMERIC                                
111600         AND REQU-IDRAPPNR(RAD-IX) NUMERIC                                
111700         AND REQU-IDKOLLI (RAD-IX) NUMERIC                                
111800                                                                          
111900           MOVE REQU-IDDISTR(RAD-IX) TO W-IDDISTR                         
112000                                        W-IDDISTR-FSEQ-MIN                
112100                                        W-IDDISTR-FSEQ-MAX                
112200           MOVE REQU-IDKUNDNR(RAD-IX) TO W-IDKUNDNR                       
112300                                         W-IDKUNDNR-FSEQ-MIN              
112400                                         W-IDKUNDNR-FSEQ-MAX              
112500           MOVE REQU-IDRAPPNR(RAD-IX) TO W-IDRAPPNR                       
112600                                         W-IDRAPPNR-FSEQ-MIN              
112700                                         W-IDRAPPNR-FSEQ-MAX              
112800           MOVE REQU-IDKOLLI (RAD-IX) TO W-IDKOLLI-SPAR                   
112900                                                                          
113000           IF REQU-KDCMD(RAD-IX) = W-DELETE                               
113100           OR W-TEXT                                                      
113200             IF REQU-KDCMD(RAD-IX) = W-DELETE                             
113300               PERFORM IMS-GU-SEQF-WLRETA01                               
113400               PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT               
113500                 IF RET-IDKOLLI = W-IDKOLLI-SPAR                          
113600                   MOVE RET-DAREGDAT TO W-DAREGDAT                        
113700                   MOVE RET-TIKLOCK  TO W-TIKLOCK                         
113800                   PERFORM IMS-GHU-WLRETA01                               
113900                   PERFORM IMS-DLET-WLRETA01                              
114000                   ADD +1     TO WS-DEL-COUNT                             
114100                   IF WS-DEL-COUNT  = REQU-KVRADER-MAX1                   
114200                     MOVE NEJ TO INDATA-SW                                
114300                   END-IF                                                 
114400                 END-IF                                                   
114500                 PERFORM IMS-GN-SEQF-WLRETA01                             
114600               END-PERFORM                                                
114700             ELSE                                                         
114800               PERFORM IMS-GHU-RETA4-FSEQ                                 
114900               PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT               
115000                 IF RET-IDKOLLI = W-IDKOLLI-SPAR                          
115100                   MOVE SPACE                TO RET-TERETNOT              
115200                   MOVE REQU-TERETNOT (RAD-IX) TO RET-TERETNOT            
115300                                     RESP-TERETNOT (RAD-IX)               
115400                                                                          
115500                   PERFORM IMS-REPL-RETA4                                 
115600                 END-IF                                                   
115700                 PERFORM IMS-GHN-RETA4-FSEQ                               
115800               END-PERFORM                                                
115900             END-IF                                                       
116000           ELSE                                                           
116100             PERFORM IMS-GHU-WLKREE01                                     
116200             IF SEGMENT-FINNS                                             
116300               IF REQU-KDCMD (RAD-IX) =  W-DELETE OR W-PRINT              
116400               OR (KDCMD-SAKNAS AND ANM-KDLEVANM < 7)                     
116500                 MOVE REQU-IDDISTR (RAD-IX)  TO                           
116600                                 RESP-IDDISTR-REP(4794-IX)                
116700                 MOVE REQU-IDKUNDNR(RAD-IX) TO                            
116800                                 RESP-IDKUNDNR-REP(4794-IX)               
116900                 MOVE REQU-IDRAPPNR(RAD-IX) TO                            
117000                                 RESP-IDRAPPNR-REP(4794-IX)               
117100                 ADD +1 TO 4794-IX                                        
117200                                                                          
117300                 IF ANM-KDLEVANM = W-ANM-MOT                              
117400                   MOVE W-ANM-PAAB      TO ANM-KDLEVANM                   
117500                   PERFORM IMS-REPL-WLKREE01                              
117600                 END-IF                                                   
117700                                                                          
117800                 PERFORM HAC-REDIGERA-UTSKRIFTS-NYCKLAR                   
117900                 PERFORM HAD-UPPDATERA-STATUS                             
118000                 MOVE JA   TO PRINTAT-SW                                  
118100               END-IF                                                     
118200             END-IF                                                       
118300           END-IF                                                         
118400         END-IF                                                           
118500       END-IF                                                             
118600       IF RAD-IX = WS-INDX-REC                                            
118700          MOVE JA TO WS-REC-LIMIT                                         
118800       ELSE                                                               
118900          ADD +1 TO RAD-IX                                                
119000       END-IF                                                             
119100     END-PERFORM                                                          
119200                                                                          
119300     IF 4794-IX  > +1                                                     
119400       PERFORM HAB-STARTA-4794                                            
119500     END-IF                                                               
119600     .                                                                    
119700     EJECT                                                                
119800                                                                          
119900 HAB-STARTA-4794  SECTION.                                                
120000                                                                          
120100     MOVE 'WL0150'              TO RESP-IDPGM-REP                         
120200     .                                                                    
120300                                                                          
120400 HAC-REDIGERA-UTSKRIFTS-NYCKLAR SECTION.                                  
120500     SKIP2                                                                
120600     IF REQU-IDRT-KEY NOT = SPACE                                         
120700        MOVE REQU-IDRT-KEY TO W-IDRT-MIN                                  
120800                              W-IDRT-MAX                                  
120900     END-IF                                                               
121000     IF REQU-IDRTLOP-KEY NOT = SPACE                                      
121100       IF REQU-IDRTLOP-KEY NUMERIC AND REQU-IDRTLOP-KEY > ZERO            
121200          MOVE REQU-IDRTLOP-KEY TO W-IDRTLOP-MIN                          
121300                                   W-IDRTLOP-MAX                          
121400       END-IF                                                             
121500     END-IF                                                               
121600                                                                          
121700     MOVE REQU-IDKOLLI (RAD-IX)  TO W-IDKOLLI-MIN                         
121800                                    W-IDKOLLI-MAX                         
121900                                    W-IDKOLLI-BSEQ                        
122000     MOVE REQU-IDDISTR (RAD-IX)  TO W-IDDISTR-FSEQ-MIN                    
122100                                    W-IDDISTR-FSEQ-MAX                    
122200     MOVE REQU-IDKUNDNR (RAD-IX) TO W-IDKUNDNR-FSEQ-MIN                   
122300                                    W-IDKUNDNR-FSEQ-MAX                   
122400     MOVE REQU-IDRAPPNR (RAD-IX) TO W-IDRAPPNR-FSEQ-MIN                   
122500                                    W-IDRAPPNR-FSEQ-MAX                   
122600     .                                                                    
122700                                                                          
122800 HAD-UPPDATERA-STATUS SECTION.                                            
122900                                                                          
123000***** UPPDATERINGEN BORTTAGEN PGA ATT DEN STÖR                            
123100***** SORTORDNINGEN I KOLLIKÖN                                            
123200*****                                                                     
123300     PERFORM IMS-GHU-RETA3-BSEQ                                           
123400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
123500       MOVE '8'        TO RET-KDKOLSTA                                    
123600       PERFORM IMS-REPL-RETA3                                             
123700       PERFORM IMS-GHN-RETA3-BSEQ                                         
123800     END-PERFORM                                                          
123900     .                                                                    
124000                                                                          
124100 HB-PRINTA-TILLSTAND-KOLLI SECTION.                                       
124200                                                                          
124300     MOVE +1  TO 4794-IX                                                  
124400                                                                          
124500     MOVE REQU-IDRT-KEY       TO W-IDRT-BSEQ                              
124600     MOVE REQU-IDRTLOP-KEY    TO W-IDRTLOP-BSEQ                           
124700     MOVE REQU-IDKOLLI-KEY    TO W-IDKOLLI-BSEQ                           
124800                                                                          
124900     PERFORM IMS-GU-RETA3-BSEQ                                            
125000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
125100                                                                          
125200       MOVE RET-IDDISTR   TO W-IDDISTR                                    
125300       MOVE RET-IDKUNDNR  TO W-IDKUNDNR                                   
125400       MOVE RET-IDRAPPNR  TO W-IDRAPPNR                                   
125500                                                                          
125600       PERFORM IMS-GHU-WLKREE01                                           
125700       IF SEGMENT-FINNS AND ANM-KDLEVANM < 7                              
125800         MOVE ANM-IDDISTR    TO  WS-IDDISTR-4                             
125900         MOVE WS-IDDISTR-4   TO  RESP-IDDISTR-REP(4794-IX)                
126000         MOVE ANM-IDKUNDNR   TO  WS-IDKUNDNR-6                            
126100         MOVE WS-IDKUNDNR-6  TO  RESP-IDKUNDNR-REP(4794-IX)               
126200         MOVE ANM-IDRAPPNR   TO  RESP-IDRAPPNR-REP(4794-IX)               
126300                                                                          
126400         ADD +1             TO 4794-IX                                    
126500                                                                          
126600         IF 4794-IX  > 4794-MAX-IX                                        
126700           PERFORM HAB-STARTA-4794                                        
126800           MOVE +1        TO 4794-IX                                      
126900           MOVE JA        TO PRINTAT-SW                                   
127000         END-IF                                                           
127100                                                                          
127200         IF ANM-KDLEVANM = W-ANM-MOT                                      
127300           MOVE W-ANM-PAAB      TO ANM-KDLEVANM                           
127400           PERFORM IMS-REPL-WLKREE01                                      
127500         END-IF                                                           
127600                                                                          
127700       END-IF                                                             
127800       PERFORM IMS-GN-RETA3-BSEQ                                          
127900     END-PERFORM                                                          
128000                                                                          
128100     IF 4794-IX > +1                                                      
128200       PERFORM HAB-STARTA-4794                                            
128300       MOVE JA          TO PRINTAT-SW                                     
128400     END-IF                                                               
128500     .                                                                    
128600     EJECT                                                                
128700 S02-KOLLA-OM-FLERA-KOLLIN SECTION.                                       
128800                                                                          
128900     MOVE ZERO TO ANTAL-KOLLIN                                            
129000     PERFORM IMS-GU-WLRETG01                                              
129100     PERFORM UNTIL SEGMENT-SAKNAS                                         
129200       ADD +1 TO ANTAL-KOLLIN                                             
129300       PERFORM IMS-GN-WLRETG01                                            
129400     END-PERFORM                                                          
129500     .                                                                    
129600 S03-KALK-KVANT-KVAR SECTION.                                             
129700                                                                          
129800     MOVE JA TO WS-FLKLAR                                                 
129900     PERFORM IMS-GNP-WLKREE11                                             
130000     IF SEGMENT-FINNS                                                     
130100       PERFORM S04-KOLLA-RETILLRAD                                        
130200     END-IF                                                               
130300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
130400                   WS-FLKLAR = NEJ                                        
130500       IF OKOD-FL-RETILL = 'J' OR                                         
130600          OKOD-FL-INTERNUPPACKNING = 'J'                                  
130700         COMPUTE WS-KVLEVANM-KVAR  =  LEV-KVLEVANM-BEKR -                 
130800                                      LEV-KVRETINL -                      
130900                                      LEV-KVAVV-KVANT -                   
131000                                      LEV-KVRETINL-SKR -                  
131100                                      LEV-KVAVV-KVAL -                    
131200                                      LEV-KVANTAL-ILI                     
131300         IF WS-KVLEVANM-KVAR > ZERO                                       
131400           MOVE NEJ TO WS-FLKLAR                                          
131500         END-IF                                                           
131600       END-IF                                                             
131700       IF WS-FLKLAR = JA                                                  
131800         PERFORM IMS-GNP-WLKREE11                                         
131900         IF SEGMENT-FINNS                                                 
132000           PERFORM S04-KOLLA-RETILLRAD                                    
132100         END-IF                                                           
132200       END-IF                                                             
132300     END-PERFORM                                                          
132400     MOVE WS-FLKLAR TO RESP-FLKLAR (RAD-IX)                               
132500     EJECT                                                                
132600     .                                                                    
132700 S04-KOLLA-RETILLRAD SECTION.                                             
132800                                                                          
132900     MOVE NEJ                    TO OKOD-FL-RETILL                        
133000                                    OKOD-FL-INTERNUPPACKNING              
133100     IF LEV-KDKREBEH(1:1) = 'Y'   OR                                      
133200        LEV-KDKREBEH(1:1) = 'J'   OR                                      
133300        LEV-KDKREBEH(1:1) = 'C'   OR                                      
133400        LEV-KDKREBEH      = 'D01' OR                                      
133500        LEV-KDKREBEH      = 'D02' OR                                      
133600        LEV-KDKREBEH      = 'D03'                                         
133700*--ANROPA KONTROLL AV ORSAKSKODER                                         
133800        MOVE LEV-KDANMORS   TO OKOD-KDANMORS                              
133900        CALL W418OKOD USING OKOD-W418OKOD                                 
134000     END-IF                                                               
134100     .                                                                    
134200*    --- DISPATCHER SECTIONS                                              
134300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
134400                                                                          
134500     MOVE 'GETARG'               TO SUB-KDFUNC                            
134600     MOVE 'CARPARTS.LDC.SHOWRETURNCONTENTS' TO SUB-ADDISPABS              
134610                                                                          
134620*    MOVE MAX-INDX (500) TO REQU-KVRADER-MAX1 SO THAT THE                 
134630*    LENGTH IS CALCULATED CORRECTLY TO BE ABLE TO FETCH ALL               
134640*    POSSIBLE INPUT                                                       
134650     MOVE MAX-INDX               TO REQU-KVRADER-MAX1                     
134660     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
134800                                                                          
134900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
135000                                                                          
135100     IF SUB-KDRC > 0                                                      
135200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
135300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
135400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
135500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
135600     END-IF                                                               
135700     .                                                                    
135800     SKIP3                                                                
135900 S02-RETURN-RESPONSE SECTION.                                             
136000                                                                          
136100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
136200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
136300                                                                          
136400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
136500                                                                          
136600     IF SUB-KDRC > 0                                                      
136700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
136800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
136900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
137000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
137100     END-IF                                                               
137200     .                                                                    
137300     EJECT                                                                
137400 S11-MSG-CONV SECTION.                                                    
137500     MOVE SPACES                  TO RESP-MESSAGES (1)                    
137600                                     RESP-MESSAGES (2)                    
137700     MOVE 1                       TO MSG-IX                               
137800*    REQUEST OK                                                           
137900     MOVE 200                     TO RESP-KDSTATUS-API                    
138000     IF RESP-IDMSG-INFO > SPACE                                           
138100       MOVE SPACES                TO MSG-CONV-AREA                        
138200       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
138300       CALL WMSGCONV           USING MSG-CONV-AREA                        
138400       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
138500       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
138600       ADD 1                      TO MSG-IX                               
138700     END-IF                                                               
138800     IF RESP-IDMSG-ERROR > SPACE                                          
138900*      BAD REQUEST                                                        
139000       MOVE 400                   TO RESP-KDSTATUS-API                    
139100       MOVE SPACES                TO MSG-CONV-AREA                        
139200       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
139300       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
139400       CALL WMSGCONV           USING MSG-CONV-AREA                        
139500       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
139600       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
139700     END-IF                                                               
139800     .                                                                    
139900* --- IMS SEKTIONER ---                                                   
140000                                                                          
140100 IMS-GU-WLKREE01 SECTION.                                                 
140200                                                                          
140300     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
140400          DELIMITED BY SIZE INTO SSA1                                     
140500     MOVE '  GE' TO GODK-STATUSKODER                                      
140600     CALL CBLTDLI USING GU KREE-PCB DLI-IO-A201 SSA1                      
140700     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000                                                                          
141100 IMS-GHU-WLKREE01 SECTION.                                                
141200                                                                          
141300     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
141400          DELIMITED BY SIZE INTO SSA1                                     
141500     MOVE '    ' TO GODK-STATUSKODER                                      
141600     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-A201 SSA1                     
141700     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000                                                                          
142100 IMS-REPL-WLKREE01 SECTION.                                               
142200                                                                          
142300     MOVE '  ' TO GODK-STATUSKODER                                        
142400     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-A201                         
142500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
142600     PERFORM IMS-STATUSKONTROLL                                           
142700     .                                                                    
142800                                                                          
142900 IMS-GNP-WLKREE11       SECTION.                                          
143000                                                                          
143100     MOVE 'WLKREE11' TO SSA1                                              
143200     MOVE '  GEGB'         TO GODK-STATUSKODER                            
143300     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-A211 SSA1                     
143400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
143500     PERFORM IMS-STATUSKONTROLL                                           
143600     .                                                                    
143700                                                                          
143800 IMS-GU-WLRETC01 SECTION.                                                 
143900                                                                          
144000     STRING 'WLRETC01(WDA3B1KY>=' W-WDA3B1-MIN-X                          
144100                    '&WDA3B1KY<=' W-WDA3B1-MAX-X ')'                      
144200          DELIMITED BY SIZE INTO SSA1                                     
144300     MOVE '  GE' TO GODK-STATUSKODER                                      
144400     CALL CBLTDLI USING GU RETC-PCB DLI-IO-A3B1 SSA1                      
144500     MOVE RETC-STATUS-CODE TO STATUS-WS                                   
144600     PERFORM IMS-STATUSKONTROLL                                           
144700     .                                                                    
144800                                                                          
144900 IMS-GN-WLRETC01 SECTION.                                                 
145000                                                                          
145100     STRING 'WLRETC01(WDA3B1KY>=' W-WDA3B1-MIN-X                          
145200                    '&WDA3B1KY<=' W-WDA3B1-MAX-X ')'                      
145300          DELIMITED BY SIZE INTO SSA1                                     
145400     MOVE '  GE' TO GODK-STATUSKODER                                      
145500     CALL CBLTDLI USING GN RETC-PCB DLI-IO-A3B1 SSA1                      
145600     MOVE RETC-STATUS-CODE TO STATUS-WS                                   
145700     PERFORM IMS-STATUSKONTROLL                                           
145800     .                                                                    
145900                                                                          
146000 IMS-GU-WLRETA01 SECTION.                                                 
146100                                                                          
146200     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
146300          DELIMITED BY SIZE INTO SSA1                                     
146400     MOVE '  GE' TO GODK-STATUSKODER                                      
146500     CALL CBLTDLI USING GU RETA1-PCB DLI-IO-A301 SSA1                     
146600     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900                                                                          
147000 IMS-GHU-WLRETA01 SECTION.                                                
147100                                                                          
147200     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
147300          DELIMITED BY SIZE INTO SSA1                                     
147400     MOVE '  ' TO GODK-STATUSKODER                                        
147500     CALL CBLTDLI USING GHU RETA1-PCB DLI-IO-A301 SSA1                    
147600     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900                                                                          
148000 IMS-DLET-WLRETA01 SECTION.                                               
148100                                                                          
148200     MOVE '  ' TO GODK-STATUSKODER                                        
148300     CALL CBLTDLI USING DLET RETA1-PCB DLI-IO-A301                        
148400     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700                                                                          
148800 IMS-GU-SEQF-WLRETA01 SECTION.                                            
148900                                                                          
149000     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
149100                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
149200          DELIMITED BY SIZE INTO SSA1                                     
149300     MOVE '  GE' TO GODK-STATUSKODER                                      
149400     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-A301 SSA1                     
149500     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
149600     PERFORM IMS-STATUSKONTROLL                                           
149700     .                                                                    
149800                                                                          
149900 IMS-GN-SEQF-WLRETA01 SECTION.                                            
150000                                                                          
150100     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
150200                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
150300          DELIMITED BY SIZE INTO SSA1                                     
150400     MOVE '  GE' TO GODK-STATUSKODER                                      
150500     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-A301 SSA1                     
150600     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900                                                                          
151000 IMS-GU-WLRETG01    SECTION.                                              
151100                                                                          
151200     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
151300                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
151400          DELIMITED BY SIZE INTO SSA1                                     
151500     MOVE '  GE' TO GODK-STATUSKODER                                      
151600     CALL CBLTDLI USING GU RETG-PCB DLI-IO-A3F1 SSA1                      
151700     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     .                                                                    
152000                                                                          
152100 IMS-GN-WLRETG01    SECTION.                                              
152200                                                                          
152300     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
152400                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
152500          DELIMITED BY SIZE INTO SSA1                                     
152600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
152700     CALL CBLTDLI USING GN RETG-PCB DLI-IO-A3F1 SSA1                      
152800     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100                                                                          
153200 IMS-GU-RETA3-BSEQ SECTION.                                               
153300                                                                          
153400     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
153500          DELIMITED BY SIZE INTO SSA1                                     
153600     MOVE '  GE' TO GODK-STATUSKODER                                      
153700     CALL CBLTDLI USING GU RETA3-PCB DLI-IO-A301 SSA1                     
153800     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
153900     PERFORM IMS-STATUSKONTROLL                                           
154000     .                                                                    
154100                                                                          
154200 IMS-GN-RETA3-BSEQ SECTION.                                               
154300                                                                          
154400     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
154500          DELIMITED BY SIZE INTO SSA1                                     
154600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
154700     CALL CBLTDLI USING GN RETA3-PCB DLI-IO-A301 SSA1                     
154800     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
154900     PERFORM IMS-STATUSKONTROLL                                           
155000     .                                                                    
155100                                                                          
155200 IMS-GHU-RETA3-BSEQ SECTION.                                              
155300                                                                          
155400     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
155500          DELIMITED BY SIZE INTO SSA1                                     
155600     MOVE '  GE' TO GODK-STATUSKODER                                      
155700     CALL CBLTDLI USING GHU RETA3-PCB DLI-IO-A301 SSA1                    
155800     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
155900     PERFORM IMS-STATUSKONTROLL                                           
156000     .                                                                    
156100                                                                          
156200 IMS-GHN-RETA3-BSEQ SECTION.                                              
156300                                                                          
156400     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
156500          DELIMITED BY SIZE INTO SSA1                                     
156600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
156700     CALL CBLTDLI USING GHN RETA3-PCB DLI-IO-A301 SSA1                    
156800     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
156900     PERFORM IMS-STATUSKONTROLL                                           
157000     .                                                                    
157100                                                                          
157200 IMS-REPL-RETA3     SECTION.                                              
157300                                                                          
157400     MOVE '  ' TO GODK-STATUSKODER                                        
157500     CALL CBLTDLI USING REPL RETA3-PCB DLI-IO-A301                        
157600     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
157700     PERFORM IMS-STATUSKONTROLL                                           
157800     .                                                                    
157900                                                                          
158000 IMS-GHU-RETA4-FSEQ SECTION.                                              
158100                                                                          
158200     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
158300                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
158400          DELIMITED BY SIZE INTO SSA1                                     
158500     MOVE '  GE' TO GODK-STATUSKODER                                      
158600     CALL CBLTDLI USING GHU RETA4-PCB DLI-IO-A301 SSA1                    
158700     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
158800     PERFORM IMS-STATUSKONTROLL                                           
158900     .                                                                    
159000                                                                          
159100 IMS-GHN-RETA4-FSEQ SECTION.                                              
159200                                                                          
159300     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
159400                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
159500          DELIMITED BY SIZE INTO SSA1                                     
159600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
159700     CALL CBLTDLI USING GHN RETA4-PCB DLI-IO-A301 SSA1                    
159800     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
159900     PERFORM IMS-STATUSKONTROLL                                           
160000     .                                                                    
160100                                                                          
160200 IMS-REPL-RETA4     SECTION.                                              
160300                                                                          
160400     MOVE '  ' TO GODK-STATUSKODER                                        
160500     CALL CBLTDLI USING REPL RETA4-PCB DLI-IO-A301                        
160600     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900                                                                          
161000 IMS-GU-SEQG-WDA301      SECTION.                                         
161100                                                                          
161200     STRING 'WDA301  (WDA3GSEQ>=' W-WDA3GSEQ-MIN-X                        
161300                    '&WDA3GSEQ<=' W-WDA3GSEQ-MAX-X ')'                    
161400          DELIMITED BY SIZE INTO SSA1                                     
161500     MOVE '  GE'           TO GODK-STATUSKODER                            
161600     CALL CBLTDLI USING GU RETAG-PCB DLI-IO-A301 SSA1                     
161700     MOVE RETAG-STATUS-CODE TO STATUS-WS                                  
161800     PERFORM IMS-STATUSKONTROLL                                           
161900     .                                                                    
162000                                                                          
162100 IMS-GN-SEQG-WDA301      SECTION.                                         
162200                                                                          
162300     STRING 'WDA301  (WDA3GSEQ>=' W-WDA3GSEQ-MIN-X                        
162400                    '&WDA3GSEQ<=' W-WDA3GSEQ-MAX-X ')'                    
162500          DELIMITED BY SIZE INTO SSA1                                     
162600     MOVE '  GE'           TO GODK-STATUSKODER                            
162700     CALL CBLTDLI USING GN RETAG-PCB DLI-IO-A301 SSA1                     
162800     MOVE RETAG-STATUS-CODE TO STATUS-WS                                  
162900     PERFORM IMS-STATUSKONTROLL                                           
163000     .                                                                    
163100                                                                          
163200 IMS-GU-WDB601    SECTION.                                                
163300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
163400          DELIMITED BY SIZE INTO SSA1                                     
163500     MOVE '  GE' TO GODK-STATUSKODER                                      
163600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
163700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
163800     PERFORM IMS-STATUSKONTROLL                                           
163900     IF SEGMENT-SAKNAS                                                    
164000         MOVE ZERO  TO DCS-IDPERSON-REM                                   
164100     END-IF                                                               
164200     .                                                                    
164300                                                                          
164400 IMS-STATUSKONTROLL SECTION.                                              
164500                                                                          
164600     SET STATUS-IX TO 1                                                   
164700     SEARCH GODK-STATUS                                                   
164800       AT END                                                             
164900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
165000         DELIMITED BY SIZE INTO FELTEXT                                   
165100         CALL FELLOG                                                      
165200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
165300         CONTINUE                                                         
165400     END-SEARCH                                                           
165500     .                                                                    
