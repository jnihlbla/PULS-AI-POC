000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL019400.                                                
000300 AUTHOR.         KIHLBERG STEFAN.                                         
000400 DATE-WRITTEN.   07/04/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:CARPARTS.NDC.COREARRIVALREPORT                                  
000800*    WEB-NDC: WL019400 PROGRAM IS A REPLICA OF W3017100 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-NDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        LOCATION ENQUIRY NDC                                             
001300*        PROGRAM READS FOLLOWING DATABASE WDM6                            
001400*                                         WDM6A                           
001500*                                         WDM6B                           
001600*                                         WDM6C                           
001700*                                         WDM6D                           
001800*                                         WDB6                            
001900*                                         WDK7                            
002000*                                         WDD3                            
002100*    INDATA.                                                              
002200*        TRANSAKTION: WL0194T                                             
002300*        REQUEST:     WZ01REQU                                            
002400*                     WL0194I1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        RESPONSE:    WZ01RESP                                            
002800*                     WL0194O1                                            
002900*                                                                         
003000                                                                          
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'WL019400'.            
003800 77  WS-ADRESS                    PIC X(50)   VALUE                       
003900     'CARPARTS.NDC.COREARRIVALREPORT'.                                    
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  CURRENT-IMS-SECTION         PIC X(25) VALUE SPACE.                   
004300 77  CURRENT-SECTION             PIC X(25) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500 77  WS-RESP-AREA                PIC S9(5) VALUE ZERO COMP-3.             
004600                                                                          
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100 77  WS-IDDC-1                   PIC X(2)    VALUE SPACE.                 
005200 77  WS-SDC-91                   PIC X(2)    VALUE '91'.                  
005300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  IX                          PIC 9(2)    VALUE ZERO.                  
005500 77  IX2                         PIC 9(2)    VALUE ZERO.                  
005600 77  INDX1                       PIC 9(3)    VALUE ZERO.                  
005700 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
005800 77  WS-RENSNINGSDATUM           PIC  9(8).                               
005900 77  WS-KVRADER                  PIC  9(5).                               
006000                                                                          
006100 01  WS-WORK-FIELDS.                                                      
006200   03  WS-IDDISTR-KEY            PIC S9(5)   VALUE ZERO.                  
006300   03  WS-IDKUNDNR-KEY           PIC S9(7)   VALUE ZERO.                  
006400   03  WS-IDBYTRAP-KEY           PIC S9(7)   VALUE ZERO.                  
006500   03  WS-KDBYTSTA-KEY           PIC X(1)    VALUE SPACE.                 
006600   03  WS-IDDC-KEY               PIC X(2)    VALUE SPACE.                 
006700                                                                          
006800   03  WS-IDDISTR                PIC S9(5)   VALUE ZERO.                  
006900   03  WS-IDKUNDNR               PIC S9(7)   VALUE ZERO.                  
007000   03  WS-IDBYTRAP               PIC S9(7)   VALUE ZERO.                  
007100   03  WS-KDBYTSTA               PIC X(1)    VALUE SPACE.                 
007200   03  WS-IDDC                   PIC X(2)    VALUE SPACE.                 
007300   03  WS-IDDC-SPAR              PIC X(2)    VALUE SPACE.                 
007400                                                                          
007500 01  DAGENS-SSAAMMDD             PIC 9(8)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-SSAAMMDD.                                    
007700     03  DAGENS-SS               PIC 9(2).                                
007800     03  DAGENS-AAMMDD           PIC 9(6).                                
007900                                                                          
008000 01  DAGENS-DATUM                PIC 9(6).                                
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03 DAGENS-DATUM-AA          PIC 9(2).                                
008300     03 DAGENS-DATUM-MM          PIC 9(2).                                
008400     03 DAGENS-DATUM-DD          PIC 9(2).                                
008500                                                                          
008600 01  DAGENS-TIME                 PIC 9(4).                                
008700                                                                          
008800 01  WS-DAP-HDR.                                                          
008900   03 WS-DAP-IDDC                PIC X(2)    VALUE SPACE.                 
009000   03 WS-DAP-IDDISTR             PIC X(4)    VALUE SPACE.                 
009100                                                                          
009200 01  WS-STYR-LAS.                                                         
009300   03  WS-DISTR-STYR             PIC X(1)    VALUE SPACE.                 
009400   03  WS-KUND-STYR              PIC X(1)    VALUE SPACE.                 
009500   03  WS-RAPP-STYR              PIC X(1)    VALUE SPACE.                 
009600   03  WS-STATUS-2459-STYR       PIC X(1)    VALUE SPACE.                 
009700   03  WS-STATUS-36-STYR         PIC X(1)    VALUE SPACE.                 
009800                                                                          
009900 01  WS-NUM5                     PIC 9(5).                                
010000 01  WS-REDUIN                   PIC X(30)   VALUE SPACE.                 
010100 01  WS-REDUUT                   PIC X(30)   VALUE SPACE.                 
010200                                                                          
011300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
011400     88  KEYS-OK                             VALUE 'J'.                   
011500     88  KEYS-WRONG                          VALUE 'N'.                   
011600     EJECT                                                                
011700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
011800     88  INDATA-OK                           VALUE 'J'.                   
011900     88  INDATA-FEL                          VALUE 'N'.                   
012000                                                                          
012100 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
012200     88  TRAFF-OK                            VALUE 'J'.                   
012300     88  NO-TRAFF                            VALUE 'N'.                   
012400                                                                          
012500 77  PRINT-SW                    PIC X       VALUE 'N'.                   
012600     88  PRINT-OK                            VALUE 'J'.                   
012700     88  NO-PRINT                            VALUE 'N'.                   
012800                                                                          
012900                                                                          
013000 77  PRINT-LINE-SW               PIC X       VALUE 'N'.                   
013100     88  PRINT-LINE-FOUND                    VALUE 'J'.                   
013200     88  PRINT-LINE-NOT-FOUND                VALUE 'N'.                   
013300                                                                          
013400 77  RAD-KOLL-SW                 PIC X       VALUE 'J'.                   
013500     88  RAD-KOLL-OK                         VALUE 'J'.                   
013600     88  RAD-KOLL-FEL                        VALUE 'N'.                   
013700                                                                          
013800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014100     SKIP3                                                                
014200 01  MESSAGE-CODES.                                                       
014300     03  INVALID-KEY-FIELDS      PIC X(3)  VALUE '022'.                   
014400     03  TOO-MANY-LINES          PIC X(3)  VALUE '028'.                   
014500     03  SYSTEM-ERROR            PIC X(3)  VALUE '099'.                   
014600     03  KEYS-ARE-MISSING        PIC X(3)  VALUE '041'.                   
014700     03  LINES-NOT-FOUND         PIC X(3)  VALUE '027'.                   
014800     EJECT                                                                
014900                                                                          
015000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
015100 01  GENERAL-SUBPROGRAMS.                                                 
015200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015400     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
015500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
015600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015700     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
015800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015900     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
016000     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
016100     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
016200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016300*        TILLDELA DATA WDK711                                             
016400     SKIP3                                                                
016500*    --- PARAMETERS TO ABEND                                              
016600                                                                          
016700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016800*01  -COPY WZ01SUB                                                        
016900     EJECT                                                                
017000                                                                          
017100*01  -COPY WWDCKONS                                                       
017000                                                                          
017100*01  -COPY WWDIST27                                                       
017200     EJECT                                                                
017300 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
017400*01  -COPY WZ01SEND                                                       
017500                                                                          
017600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
017700*01  -COPY WDATAREA                                                       
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL WDAGKONV                                         
018000*01  -COPY WDAGAREA                                                       
018100     EJECT                                                                
018200*01  -COPY WL01TIDZ                                                       
018300     EJECT                                                                
018400*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
018500 01  FILLER                       PIC X(8)    VALUE 'W005WDK7'.           
018600*   -COPY W005WDK7                                                        
018730                                                                          
018800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
018900 01  REQU-AREA.                                                           
019000*    03  -COPY WZ01REQU                                                   
019100*    03  -COPY WL0194I1                                                   
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
019400 01  RESP-AREA.                                                           
019500*    03  -COPY WZ01RESP                                                   
019600*    03  -COPY WL0194O1                                                   
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
019900 01  HDR-AREA.                                                            
020000*    03  -COPY WZ01REQU  -PRE HDR-                                        
020100*    03  -COPY WZ04HDR                                                    
020200*                                                                         
020300 01  REPORT-HEADER.                                                       
020400*    03  -COPY WL0194O2                                                   
020500                                                                          
020600 01  REPORT-LINE.                                                         
020700*    03  -COPY WL0194O3                                                   
020800*                                                                         
020900     EJECT                                                                
021000 01  NYCKLAR-TILL-DLI.                                                    
021100   03  FILLER.                                                            
021200     05 W-IDDC               PIC X(2)    VALUE SPACE.                     
021300     05 W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.               
021400     05 W-KDBYTSTA           PIC X       VALUE SPACE.                     
021500                                                                          
021600   03  W-IDBYTRAP-X.                                                      
021700     05 W-IDBYTRAP           PIC S9(7)   VALUE ZERO COMP-3.               
021800   03  W-IDKUNDNR-X.                                                      
021900     05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.               
022000                                                                          
022100   03  W-WDM6ASEQ-MIN-X.                                                  
022200     05  W-KDBYTSTA-A1       PIC X(1)    VALUE SPACE.                     
022300     05  W-IDDC-A1           PIC X(2)    VALUE SPACE.                     
022400     05  W-DAREGDAT-A1       PIC 9(8)    VALUE ZERO.                      
022500     05  W-IDDISTR-A1        PIC S9(5)   VALUE ZERO COMP-3.               
022600     05  W-IDBYTRAP-A1       PIC S9(7)   VALUE ZERO COMP-3.               
022700                                                                          
022800   03  W-WDM6ASEQ-MAX-X.                                                  
022900     05  W-KDBYTSTA-A1-MAX   PIC X(1)    VALUE '9'.                       
023000     05  W-IDDC-A1-MAX       PIC X(2)    VALUE SPACE.                     
023100     05  W-DAREGDAT-A1-MAX   PIC 9(8)    VALUE 99999999.                  
023200     05  W-IDDISTR-A1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
023300     05  W-IDBYTRAP-A1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
023400                                                                          
023500   03  W-WDM6BSEQ-MIN-X.                                                  
023600     05  W-IDDC-B1           PIC X(2)    VALUE SPACE.                     
023700     05  W-KDBYTSTA-B1       PIC X(1)    VALUE SPACE.                     
023800     05  W-DAANKDAG-B1       PIC 9(8)    VALUE ZERO.                      
023900     05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.               
024000     05  W-IDBYTRAP-B1       PIC S9(7)   VALUE ZERO COMP-3.               
024100                                                                          
024200   03  W-WDM6BSEQ-MAX-X.                                                  
024300     05  W-IDDC-B1-MAX       PIC X(2)    VALUE SPACE.                     
024400     05  W-KDBYTSTA-B1-MAX   PIC X(1)    VALUE '9'.                       
024500     05  W-DAANKDAG-B1-MAX   PIC 9(8)    VALUE 99999999.                  
024600     05  W-IDDISTR-B1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
024700     05  W-IDBYTRAP-B1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
024800                                                                          
024900   03  W-WDM6C1KY-MIN-X.                                                  
025000     05  W-IDDC-C1           PIC X(2)    VALUE SPACE.                     
025100     05  W-IDDISTR-C1        PIC S9(5)   VALUE ZERO COMP-3.               
025200     05  W-KDBYTSTA-C1       PIC X(1)    VALUE SPACE.                     
025300     05  W-DAANKDAG-C1       PIC 9(8)    VALUE ZERO.                      
025400     05  W-IDBYTRAP-C1       PIC S9(7)   VALUE ZERO COMP-3.               
025500                                                                          
025600   03  W-WDM6C1KY-MAX-X.                                                  
025700     05  W-IDDC-C1-MAX       PIC X(2)    VALUE SPACE.                     
025800     05  W-IDDISTR-C1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
025900     05  W-KDBYTSTA-C1-MAX   PIC X(1)    VALUE '9'.                       
026000     05  W-DAANKDAG-C1-MAX   PIC 9(8)    VALUE 99999999.                  
026100     05  W-IDBYTRAP-C1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
026200                                                                          
026300   03  W-WDM6D1KY-MIN-X.                                                  
026400     05  W-IDDC-D1           PIC X(2)    VALUE SPACE.                     
026500     05  W-IDDISTR-D1        PIC S9(5)   VALUE ZERO COMP-3.               
026600     05  W-KDBYTSTA-D1       PIC X(1)    VALUE SPACE.                     
026700     05  W-DAREGDAT-D1       PIC 9(8)    VALUE ZERO.                      
026800     05  W-IDBYTRAP-D1       PIC S9(7)   VALUE ZERO COMP-3.               
026900                                                                          
027000   03  W-WDM6D1KY-MAX-X.                                                  
027100     05  W-IDDC-D1-MAX       PIC X(2)    VALUE SPACE.                     
027200     05  W-IDDISTR-D1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
027300     05  W-KDBYTSTA-D1-MAX   PIC X(1)    VALUE '9'.                       
027400     05  W-DAREGDAT-D1-MAX   PIC 9(8)    VALUE 99999999.                  
027500     05  W-IDBYTRAP-D1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
027600                                                                          
027700   03  W-WDM601KY-X.                                                      
027800     05  W-IDDISTR-UNIK      PIC S9(5)   VALUE ZERO COMP-3.               
027900     05  W-IDBYTRAP-UNIK     PIC S9(7)   VALUE ZERO COMP-3.               
028000                                                                          
028100   03  W-IDDC-B6-X.                                                       
028200       05 W-IDDC-B6          PIC X(2).                                    
028300                                                                          
028400   03  W-IDARTNR-K7-X.                                                    
028500       05  W-IDARTNR-K7      PIC S9(9)  VALUE ZERO COMP-3.                
028600                                                                          
028700   03  W-IDDC-K7-X.                                                       
028800       05 W-IDDC-K7          PIC X(2).                                    
028900                                                                          
029000     03  W-WDD3BSEQ-X.                                                    
029100         05  W-IDARTNR-D3        PIC S9(9)   VALUE ZERO COMP-3.           
029200     03  W-IDSKYLT-X.                                                     
029300         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
029400     03  W-IDARTNR-X.                                                     
029500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029600                                                                          
029700*    --- STATUS-KOD FRÅN IMS                                              
029800 01  STATUS-WS                   PIC XX.                                  
029900     88  SEGMENT-FINNS                       VALUE '  '.                  
030000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030100     88  BAS-SLUT                            VALUE 'GB'.                  
030200     SKIP2                                                                
030300 01  GODK-STATUSKODER.                                                    
030400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030500     SKIP3                                                                
030600 01  SSA1                        PIC X(156).                              
030700 01  SSA2                        PIC X(64).                               
030800                                                                          
030900*    --- IMS FUNKTIONSKODER                                               
031000*01  -COPY W0003                                                          
031100     EJECT                                                                
031200*    ---  DLI INPUT-OUTPUT AREA                                           
031300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
031400                                                                          
031500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM601'.        
031600 01  DLI-IO-WDM601.                                                       
031700*  03  -COPY WDM601                                                       
031800     EJECT                                                                
031900                                                                          
032000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM611'.        
032100                                                                          
032200 01  DLI-IO-WDM611.                                                       
032300*  03  -COPY WDM611                                                       
032400     EJECT                                                                
032500                                                                          
032600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM6C1'.        
032700                                                                          
032800 01  DLI-IO-WDM6C1.                                                       
032900*  03  -COPY WDM6C1                                                       
033000     EJECT                                                                
033100                                                                          
033200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM6D1'.        
033300                                                                          
033400 01  DLI-IO-WDM6D1.                                                       
033500*  03  -COPY WDM6D1                                                       
033600                                                                          
033700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033800 01   DLI-IO-AREA-B601.                                                   
033900*     03  -COPY WDB601                                                    
034000                                                                          
034100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK701'.        
034200 01  DLI-IO-WDK701.                                                       
034300*    03  -COPY WDK701                                                     
034400     EJECT                                                                
034500                                                                          
034600 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
034700     SKIP3                                                                
034800 01  DLI-IO-WDK711.                                                       
034900*    03  -COPY WDK711                                                     
035000     EJECT                                                                
035100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDD311'.           
035200 01  DLI-IO-WDD311.                                                       
035300*    03  -COPY WDD311                                                     
035400     EJECT                                                                
035500                                                                          
036600 LINKAGE SECTION.                                                         
036700*01  -COPY W0009  -PRE MSG-                                               
036800     EJECT                                                                
036900 01  DISTRWEB-PCB                PIC X.                                   
037000     EJECT                                                                
037100                                                                          
037200*01  -COPY W0008  -PRE WDM6-                                              
037300     05  FILLER                  PIC X.                                   
037400*01  -COPY W0008  -PRE WDM6A-                                             
037500     05  FILLER                  PIC X.                                   
037600*01  -COPY W0008  -PRE WDM6B-                                             
037700     05  FILLER                  PIC X.                                   
037800*01  -COPY W0008  -PRE WDM6C-                                             
037900     05  FILLER                  PIC X.                                   
038000*01  -COPY W0008  -PRE WDM6D-                                             
038100     05  FILLER                  PIC X.                                   
038200*01  -COPY W0008  -PRE WDB6-                                              
038300     05  FILLER                  PIC X.                                   
038400*01  -COPY W0008  -PRE WDK7-                                              
038500     05  FILLER                  PIC X.                                   
038600*01  -COPY W0008  -PRE WDK6-                                              
038700     05  FILLER                  PIC X.                                   
038800*01  -COPY W0008  -PRE WDD3-                                              
038900     05  FILLER                  PIC X.                                   
039300     EJECT                                                                
039400                                                                          
039500 PROCEDURE DIVISION  USING                                                
039600                           MSG-PCB DISTRWEB-PCB                           
039700                           WDM6-PCB                                       
039800                           WDM6A-PCB WDM6B-PCB                            
039900                           WDM6C-PCB WDM6D-PCB                            
040000                           WDB6-PCB WDK7-PCB                              
040100                           WDK6-PCB WDD3-PCB.                             
040200 MAIN SECTION.                                                            
040300     ENTRY 'DLITCBL' USING                                                
040400                           MSG-PCB DISTRWEB-PCB                           
040500                           WDM6-PCB                                       
040600                           WDM6A-PCB WDM6B-PCB                            
040700                           WDM6C-PCB WDM6D-PCB                            
040800                           WDB6-PCB WDK7-PCB                              
040900                           WDK6-PCB WDD3-PCB.                             
041000                                                                          
041100                                                                          
041200*------------------------                                                 
041300     MOVE 'STYR SECTION'     TO CURRENT-SECTION                           
041400     PERFORM S01-HAEMTA-ANROPSDATA                                        
041500     IF SUB-KDRC = 0                                                      
041600       PERFORM A-INIT                                                     
041700       PERFORM B-CHECK-KEYS                                               
041800       IF KEYS-OK                                                         
041900         IF REQU-KDPGMACT = 'E'                                           
042000           PERFORM C-CHECK-INPUT                                          
042100           IF INDATA-OK                                                   
042200             PERFORM D-UPDATE                                             
042300             PERFORM E-READ-SHOW-INFO                                     
042400           END-IF                                                         
042500         END-IF                                                           
042600         IF KEYS-OK                                                       
042700           IF REQU-KDPGMACT = 'S'                                         
042800             PERFORM E-READ-SHOW-INFO                                     
042900           END-IF                                                         
043000           IF REQU-KDPGMACT = 'P'                                         
043100              PERFORM G-PRINT-LIST                                        
043200           END-IF                                                         
043300         END-IF                                                           
043400         IF REQU-KDPGMACT = 'P'  AND                                      
043500            RESP-IDMSG-ERROR = SPACE                                      
043600            CONTINUE                                                      
043700         ELSE                                                             
043800            PERFORM S02-RETURNERA-SVAR                                    
043900         END-IF                                                           
044000       ELSE                                                               
044100         PERFORM S02-RETURNERA-SVAR                                       
044200       END-IF                                                             
044300     END-IF                                                               
044400                                                                          
044500     PERFORM Z-FINIT                                                      
044600     MOVE ZERO TO RETURN-CODE                                             
044700     GOBACK                                                               
044800     .                                                                    
044900     EJECT                                                                
045000                                                                          
045100 A-INIT SECTION.                                                          
045200     MOVE 'A-INTIT'    TO CURRENT-SECTION                                 
045300     MOVE ALL '+' TO RESP-AREA                                            
045400     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
045500                     RESP-IDMSG-INFO                                      
045600                     RESP-IDELMT-ERROR                                    
045700     IF REQU-KDPGMACT = 'S'                                               
045800       MOVE ALL SPACE TO RESP-AREA                                        
045900     END-IF                                                               
046000                                                                          
046100     MOVE ZERO    TO RESP-KVRADER                                         
046200     MOVE '001'   TO RESP-IDMSGVER                                        
046300     ACCEPT DAGENS-DATUM FROM DATE                                        
046400     ACCEPT DAGENS-TIME  FROM TIME                                        
046500                                                                          
046600                                                                          
046700*    FÖRBERED RENSNINGSDATUMANROP ANVÄNDS FÖR ATT BEGRÄNSA VISN           
046800*    AV RADER BAKÅT I TIDEN                                               
046900     ACCEPT DAG-TIAAMMDD-TOM FROM DATE                                    
047000     MOVE 365    TO DAG-KVKALDAG                                          
047100     MOVE 003    TO DAG-KDCALL                                            
047200                                                                          
047300     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
047400                                                                          
047500     IF DAG-KDSVAR = SPACE                                                
047600       MOVE DAG-TIAAMMDD-FOM TO WS-RENSNINGSDATUM (3:6)                   
047700       MOVE DAG-TISEKEL-FOM  TO WS-RENSNINGSDATUM (1:2)                   
047800     ELSE                                                                 
047900       MOVE ZERO             TO WS-RENSNINGSDATUM                         
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400 B-CHECK-KEYS SECTION.                                                    
048500                                                                          
048600     MOVE 'B-CHECK-KEYS'     TO CURRENT-SECTION                           
048700     MOVE YES TO KEYS-SW                                                  
048800                                                                          
048900***  KONTROLL AV REQU-KDPGMACT                                            
049000     IF REQU-KDPGMACT = 'E' OR 'S' OR 'P'                                 
049100        CONTINUE                                                          
049200     ELSE                                                                 
049300        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
049400        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
049500        MOVE NOO TO KEYS-SW                                               
049600     END-IF                                                               
049700                                                                          
049800***  KONTROLL AV DISTRICT                                                 
049900     IF KEYS-OK                                                           
050000       IF REQU-IDDISTR-KEY = ALL '+'                                      
050100         MOVE NOO TO KEYS-SW                                              
050200         MOVE 'IDDISTR' TO RESP-IDELMT-ERROR                              
050300         MOVE '026'           TO RESP-IDMSG-ERROR                         
050400*        MUST BE ENTERED                                                  
050500       ELSE                                                               
050600         INSPECT REQU-IDDISTR-KEY REPLACING LEADING SPACE BY ZERO         
050700         IF REQU-IDDISTR-KEY NUMERIC                                      
050800            MOVE REQU-IDDISTR-KEY TO WS-IDDISTR-KEY                       
050900                                     RESP-IDDISTR-KEY                     
051000            INSPECT RESP-IDDISTR-KEY                                      
051100                       REPLACING LEADING ZERO BY SPACE                    
051200         ELSE                                                             
051300           MOVE NOO TO KEYS-SW                                            
051400           MOVE 'IDDISTR'  TO RESP-IDELMT-ERROR                           
051500           MOVE '024'      TO RESP-IDMSG-ERROR                            
051600*          MUST BE NUMERIC                                                
051700         END-IF                                                           
051800       END-IF                                                             
051900     END-IF                                                               
052000                                                                          
052100***  KONTROLL AV KUND                                                     
052200     IF KEYS-OK                                                           
052300       IF REQU-IDKUNDNR-KEY = ALL '+'                                     
052400         MOVE ZERO TO WS-IDKUNDNR-KEY                                     
052500       ELSE                                                               
052600         INSPECT REQU-IDKUNDNR-KEY REPLACING LEADING SPACE BY ZERO        
052700         IF REQU-IDKUNDNR-KEY NUMERIC                                     
052800           MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR-KEY                      
052900                                     RESP-IDKUNDNR-KEY                    
053000         INSPECT RESP-IDKUNDNR-KEY                                        
053100                                   REPLACING LEADING ZERO BY SPACE        
053200         ELSE                                                             
053300           MOVE NOO TO KEYS-SW                                            
053400           MOVE 'IDKUNDNR'  TO RESP-IDELMT-ERROR                          
053500           MOVE '024'       TO RESP-IDMSG-ERROR                           
053600*          MUST BE NUMERIC                                                
053700         END-IF                                                           
053800       END-IF                                                             
053900     END-IF                                                               
054000                                                                          
054100***  KONTROLL AV RAPPORT                                                  
054200     IF KEYS-OK                                                           
054300       IF REQU-IDBYTRAP-KEY = ALL '+'                                     
054400         MOVE ZERO TO WS-IDBYTRAP-KEY                                     
054500       ELSE                                                               
054600         INSPECT REQU-IDBYTRAP-KEY REPLACING LEADING SPACE BY ZERO        
054700         IF REQU-IDBYTRAP-KEY NUMERIC                                     
054800            MOVE REQU-IDBYTRAP-KEY TO WS-IDBYTRAP-KEY                     
054900                                      RESP-IDBYTRAP-KEY                   
055000            INSPECT RESP-IDBYTRAP-KEY                                     
055100                                 REPLACING LEADING ZERO BY SPACE          
055200         ELSE                                                             
055300            MOVE NOO TO KEYS-SW                                           
055400            MOVE 'IDBYTRAP'  TO RESP-IDELMT-ERROR                         
055500            MOVE '024'       TO RESP-IDMSG-ERROR                          
055600*           MUST BE NUMERIC                                               
055700         END-IF                                                           
055800       END-IF                                                             
055900     END-IF                                                               
056000                                                                          
056100***  KONTROLL AV STATUS                                                   
056200     IF KEYS-OK                                                           
056300       IF REQU-KDBYTSTA-KEY = ALL '+'                                     
056400         MOVE NOO TO KEYS-SW                                              
056500         MOVE 'KDBYTSTA'  TO RESP-IDELMT-ERROR                            
056600         MOVE '026'       TO RESP-IDMSG-ERROR                             
056700*        MUST BE ENTERED                                                  
056800       ELSE                                                               
056900         INSPECT REQU-KDBYTSTA-KEY REPLACING LEADING SPACE BY ZERO        
057000         IF REQU-KDBYTSTA-KEY NUMERIC                                     
057100           CONTINUE                                                       
057200           IF REQU-KDBYTSTA-KEY = '2' OR '3' OR '4' OR '9'                
057300             MOVE REQU-KDBYTSTA-KEY TO WS-KDBYTSTA-KEY                    
057400                                       RESP-KDBYTSTA-KEY                  
057500           ELSE                                                           
057600             MOVE NOO TO KEYS-SW                                          
057700             MOVE 'KDBYTSTA'  TO RESP-IDELMT-ERROR                        
057800             MOVE '043'       TO RESP-IDMSG-ERROR                         
057900*            INVALID KEY FIELDS                                           
058000           END-IF                                                         
058100         ELSE                                                             
058200           MOVE NOO TO KEYS-SW                                            
058300           MOVE 'KDBYTSTA' TO RESP-IDELMT-ERROR                           
058400           MOVE '024'      TO RESP-IDMSG-ERROR                            
058500*          MUST BE NUMERIC                                                
058600         END-IF                                                           
058700       END-IF                                                             
058800     END-IF                                                               
058900                                                                          
059000***  KONTROLL AV IDDC                                                     
059100     IF KEYS-OK                                                           
059200       IF REQU-IDDC-KEY NOT = ALL '+'                                     
059300         MOVE REQU-IDDC-KEY TO WS-IDDC-KEY WS-IDDC                        
059400         MOVE WS-IDDC-KEY     TO W-IDDC-B6 W-IDDC                         
059500                                 WS-IDDC-1                                
059600         PERFORM IMS-GU-WDB601                                            
059700         IF SEGMENT-FINNS                                                 
059800           IF WS-IDDC-KEY = '3A' OR '3B'                                  
059900               MOVE WS-SDC-91  TO WS-IDDC-1 W-IDDC                        
060000                                  RESP-IDDC-KEY                           
060100           ELSE                                                           
060200             IF DCS-NDC-CN OR DCS-NDC-PF OR DCS-NDC-NA                    
060300               CONTINUE                                                   
060400             ELSE                                                         
060500               MOVE 'IDDC    ' TO RESP-IDELMT-ERROR                       
060600               MOVE '023'      TO RESP-IDMSG-ERROR                        
060700*              IS INVALID                                                 
060800               MOVE NOO TO KEYS-SW                                        
060900             END-IF                                                       
061000           END-IF                                                         
061100         ELSE                                                             
061200           MOVE 'IDDC    ' TO RESP-IDELMT-ERROR                           
061300           MOVE '023'    TO RESP-IDMSG-ERROR                              
061400*          IS INVALID                                                     
061500           MOVE NOO TO KEYS-SW                                            
061600         END-IF                                                           
061700       ELSE                                                               
061800          MOVE NOO TO KEYS-SW                                             
061900          MOVE 'IDDC'     TO RESP-IDELMT-ERROR                            
062000          MOVE '026'           TO RESP-IDMSG-ERROR                        
062100*         MUST BE ENTERED                                                 
062200       END-IF                                                             
062300     END-IF                                                               
062400                                                                          
062500     MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                  
062600                                                                          
062700*    KONTROLL ATT ETT DC BARA ANGER TILLÅTNA DISTRIKT                     
062800     IF KEYS-OK                                                           
             MOVE WS-IDDISTR-KEY TO DIST27-IDDISTR                              
063100       IF (DCS-NDC-PF AND DIST27-CORE-NDC-PF)                             
063210       OR (DCS-NDC-NA AND DIST27-CORE-NDC-NA)                             
063300       OR (DCS-NDC-CN AND DIST27-CORE-NDC-CN)                             
063500          CONTINUE                                                        
063600       ELSE                                                               
063700          MOVE NOO TO KEYS-SW                                             
063800          MOVE 'IDDISTR'   TO RESP-IDELMT-ERROR                           
063900          MOVE '022'       TO RESP-IDMSG-ERROR                            
064000*         INVALID KEY FIELDS                                              
064100       END-IF                                                             
064200     END-IF                                                               
064300                                                                          
064400     IF KEYS-OK                                                           
064500       MOVE SPACE TO WS-STYR-LAS                                          
064600       IF WS-IDDISTR-KEY NOT = ZERO                                       
064700         MOVE 'D' TO WS-DISTR-STYR                                        
064800       END-IF                                                             
064900       IF WS-IDKUNDNR-KEY NOT = ZERO                                      
065000         MOVE 'K' TO WS-KUND-STYR                                         
065100       END-IF                                                             
065200       IF WS-IDBYTRAP-KEY NOT = ZERO                                      
065300         MOVE 'R' TO WS-RAPP-STYR                                         
065400       END-IF                                                             
065500       IF WS-KDBYTSTA-KEY = '2' OR '4' OR '9'                             
065600         MOVE '2' TO WS-STATUS-2459-STYR                                  
065700       ELSE                                                               
065800         IF WS-KDBYTSTA-KEY = '3'                                         
065900           MOVE '3' TO WS-STATUS-36-STYR                                  
066000         END-IF                                                           
066100       END-IF                                                             
066200                                                                          
066300       IF WS-STYR-LAS =                                                   
066400          '   2 ' OR                                                      
066500          '    3' OR                                                      
066600          'D   3' OR                                                      
066700          'D R 3' OR                                                      
066800          'DK  3' OR                                                      
066900          'DK3 3' OR                                                      
067000          'D  2 ' OR                                                      
067100          'D R2 ' OR                                                      
067200          'DK 2 ' OR                                                      
067300          'DKR2 '                                                         
067400          CONTINUE                                                        
067500       ELSE                                                               
067600           MOVE NOO TO INDATA-SW                                          
067700           MOVE 'KEYS    ' TO RESP-IDELMT-ERROR                           
067800           MOVE '043'      TO RESP-IDMSG-ERROR                            
067900*          INVALID KEY FIELDS                                             
068000       END-IF                                                             
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400                                                                          
068500 C-CHECK-INPUT SECTION.                                                   
068600******* UPPDATERING FÅR EJ SKE PÅ FELAKTIGT IDDC *******                  
068700                                                                          
068800     MOVE 'C-CHECK-INPUT'    TO CURRENT-SECTION                           
068900     MOVE YES TO INDATA-SW                                                
069000     MOVE +1 TO INDX                                                      
069100     MOVE NOO TO TRAFF-SW                                                 
069200     PERFORM UNTIL INDX > MAX-INDX OR TRAFF-OK                            
069300       IF REQU-KDBYTSTA-IN (INDX) NOT = ALL '+'                           
069400        OR REQU-ADBYTANK-IN (INDX) NOT = ALL '+'                          
069500         MOVE YES TO TRAFF-SW                                             
069600       END-IF                                                             
069700       ADD +1 TO INDX                                                     
069800     END-PERFORM                                                          
069900     IF TRAFF-OK                                                          
070000       MOVE +1 TO INDX                                                    
070100       PERFORM UNTIL INDX > REQU-KVRADER                                  
070200         IF REQU-IDDISTR (INDX) = ALL '+' OR SPACE                        
070300           CONTINUE                                                       
070400         ELSE                                                             
070500           INSPECT REQU-IDDISTR (INDX)                                    
070600                   REPLACING LEADING SPACE BY ZERO                        
070700           INSPECT REQU-IDBYTRAP (INDX)                                   
070800                   REPLACING LEADING SPACE BY ZERO                        
070900           IF REQU-KDBYTSTA-IN (INDX) NOT = ALL '+'                       
071000               OR REQU-ADBYTANK-IN (INDX) NOT = ALL '+'                   
071100             MOVE REQU-IDDISTR (INDX)  TO W-IDDISTR-UNIK                  
071200             MOVE REQU-IDBYTRAP (INDX) TO W-IDBYTRAP-UNIK                 
071300             PERFORM IMS-GHU-WDM601                                       
071400             IF SEGMENT-FINNS                                             
071500               IF WS-IDDC-1 = RAPP-IDDC                                   
071600                 PERFORM CA-CHECK-BYTSTA                                  
071700                 IF REQU-ADBYTANK-IN (INDX) NOT = ALL '+'                 
071800                   IF RAPP-KDBYTSTA-RAPP = '3'                            
071900                       OR REQU-KDBYTSTA-IN (INDX) = '3'                   
072000                      CONTINUE                                            
072100                   ELSE                                                   
072200                     MOVE NOO TO INDATA-SW                                
072300                     MOVE 'KDBYTSTA' TO RESP-IDELMT-ERROR                 
072400                     MOVE '046'      TO RESP-IDMSG-ERROR                  
072500*                    CONFLICTING FIELDS                                   
072600                   END-IF                                                 
072700                 END-IF                                                   
072800               ELSE                                                       
072900                 MOVE NOO TO INDATA-SW                                    
073000                 MOVE 'KDBYTSTA' TO RESP-IDELMT-ERROR                     
073100                 MOVE '046'      TO RESP-IDMSG-ERROR                      
073200*                CONFLICTING FIELDS                                       
073300               END-IF                                                     
073400             ELSE                                                         
073500               MOVE NOO TO INDATA-SW                                      
073600               MOVE 'KDBYTSTA' TO RESP-IDELMT-ERROR                       
073700               MOVE '041'      TO RESP-IDMSG-ERROR                        
073800*              MISSING                                                    
073900             END-IF                                                       
074000           END-IF                                                         
074100         END-IF                                                           
074200         ADD +1 TO INDX                                                   
074300       END-PERFORM                                                        
074400     ELSE                                                                 
074500       MOVE NOO TO INDATA-SW                                              
074600       MOVE '014'      TO RESP-IDMSG-ERROR                                
074700*      EXECUTE OCH INGET INDATA                                           
074800     END-IF                                                               
074900     IF RESP-IDMSG-ERROR NOT = SPACE                                      
075000       MOVE ALL '+' TO RESP-WL019401                                      
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500 CA-CHECK-BYTSTA SECTION.                                                 
075600* --- --- HÄR FÅR MAN INTE ÄNDRA STATUS 4 TILL 3                          
075700* --- --- DET SKER I BILD 3172.                                           
075800                                                                          
075900     MOVE 'CA-CHECK-BYTSTA'    TO CURRENT-SECTION                         
076000     IF REQU-KDBYTSTA-IN (INDX) NOT = ALL '+'                             
076100       IF REQU-KDBYTSTA-IN (INDX) = '2' OR '3'                            
076200         IF RAPP-KDBYTSTA-RAPP = '2' OR '3'                               
076300           IF RAPP-KDBYTSTA-RAPP = '2'                                    
076400             IF REQU-KDBYTSTA-IN (INDX) = '3'                             
076500               CONTINUE                                                   
076600             ELSE                                                         
076700               MOVE NOO TO INDATA-SW                                      
076800               MOVE 'KDBYTSTA' TO RESP-IDELMT-ERROR                       
076900               MOVE '046'      TO RESP-IDMSG-ERROR                        
077000*              CONFLICTING FIELDS                                         
077100             END-IF                                                       
077200           ELSE                                                           
077300             IF RAPP-KDBYTSTA-RAPP = '3'                                  
077400               IF REQU-KDBYTSTA-IN (INDX) = '2'                           
077500                 PERFORM IMS-GNP-WDM611                                   
077600                 IF SEGMENT-FINNS                                         
077700                   PERFORM UNTIL SEGMENT-SAKNAS OR RAD-KOLL-FEL           
077800                     IF OBJ-KDBYTSTA-OBJ NOT = ' '                        
077900                       MOVE NOO TO RAD-KOLL-SW                            
078000                     END-IF                                               
078100                     PERFORM IMS-GNP-WDM611                               
078200                   END-PERFORM                                            
078300                   IF RAD-KOLL-OK                                         
078400                     IF REQU-ADBYTANK-IN(INDX) NOT = ALL '+'              
078500                       MOVE NOO TO INDATA-SW                              
078600                       MOVE '046'      TO RESP-IDMSG-ERROR                
078700*                      CONFLICTING FIELDS                                 
078800                     END-IF                                               
078900                   ELSE                                                   
079000                     MOVE NOO TO INDATA-SW                                
079100                     MOVE '327'      TO RESP-IDMSG-ERROR                  
079200*                    LINES ARE CHANGED                                    
079300                     IF REQU-ADBYTANK-IN (INDX) NOT = ALL '+'             
079400                       CONTINUE                                           
079500                     END-IF                                               
079600                   END-IF                                                 
079700                 END-IF                                                   
079800               ELSE                                                       
079900                 MOVE NOO TO INDATA-SW                                    
080000                 MOVE '046'      TO RESP-IDMSG-ERROR                      
080100*                CONFLICTING FIELDS                                       
080200               END-IF                                                     
080300             END-IF                                                       
080400           END-IF                                                         
080500         ELSE                                                             
080600           MOVE NOO TO INDATA-SW                                          
080700           MOVE '046'      TO RESP-IDMSG-ERROR                            
080800*          CONFLICTING FIELDS                                             
080900         END-IF                                                           
081000       ELSE                                                               
081100         MOVE NOO TO INDATA-SW                                            
081200         MOVE '046'      TO RESP-IDMSG-ERROR                              
081300*        CONFLICTING FIELDS                                               
081400       END-IF                                                             
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081800                                                                          
081900 D-UPDATE SECTION.                                                        
082000                                                                          
082100     MOVE 'D-UPDATE       '    TO CURRENT-SECTION                         
082200     MOVE +1 TO INDX                                                      
082300     PERFORM UNTIL INDX > REQU-KVRADER                                    
082400       IF REQU-IDDISTR (INDX) NUMERIC                                     
082500         IF REQU-KDBYTSTA-IN (INDX) NOT = ALL '+'                         
082600             OR REQU-ADBYTANK-IN (INDX) NOT = ALL '+'                     
082700           MOVE REQU-IDDISTR (INDX) TO W-IDDISTR-UNIK                     
082800           MOVE REQU-IDBYTRAP (INDX) TO W-IDBYTRAP-UNIK                   
082900           PERFORM IMS-GHU-WDM601                                         
083000           IF REQU-KDBYTSTA-IN (INDX) NOT = ALL '+'                       
083100             MOVE REQU-KDBYTSTA-IN (INDX) TO RAPP-KDBYTSTA-RAPP           
083200             IF REQU-KDBYTSTA-IN (INDX) = '2'                             
083300               MOVE SPACE TO RAPP-ADBYTANK                                
083400               MOVE ZERO  TO RAPP-DAANKDAG                                
083500             ELSE                                                         
083600               MOVE WS-SDC-91  TO W-IDDC-K7                               
083700               PERFORM IMS-GNP-WDM611                                     
083800               IF SEGMENT-FINNS                                           
083900                 PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                 
084000                   MOVE OBJ-IDARTNR-OBJ  TO W-IDARTNR-K7                  
084100                   PERFORM IMS-GU-WDK701                                  
084200                   IF SEGMENT-FINNS                                       
084300                     PERFORM IMS-GU-WDK711                                
084400                     IF SEGMENT-SAKNAS                                    
084410                       IF WS-IDDC-1 NOT = WC-NDC-US-BAT                   
084500                         PERFORM DA-NEW-SEGMENT-WDK7                      
084510                       END-IF                                             
084600                     END-IF                                               
084700                   END-IF                                                 
084800                   PERFORM IMS-GNP-WDM611                                 
084900                 END-PERFORM                                              
085000               END-IF                                                     
085100               MOVE REQU-IDDISTR (INDX) TO W-IDDISTR-UNIK                 
085200               MOVE REQU-IDBYTRAP (INDX) TO W-IDBYTRAP-UNIK               
085300               PERFORM IMS-GHU-WDM601                                     
085400               MOVE REQU-KDBYTSTA-IN(INDX) TO RAPP-KDBYTSTA-RAPP          
085500               MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                 
085500               MOVE DCS-IDDC             TO MSGI-IDDC                     
085600               MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                 
085700               MOVE DAGENS-TIME          TO MSGI-TILOKTID                 
085800                                                                          
085900               CALL WL01TIDZ USING          MSGI-WL01TIDZ                 
086000                                                                          
086100               MOVE MSGI-TILOKDAT        TO DAGENS-AAMMDD                 
086200               MOVE '20'                 TO DAGENS-SS                     
086300                                                                          
086400               MOVE DAGENS-SSAAMMDD      TO RAPP-DAANKDAG                 
086500             END-IF                                                       
086600           END-IF                                                         
086700                                                                          
086800           IF REQU-ADBYTANK-IN (INDX) NOT = ALL '+'                       
086900             MOVE REQU-ADBYTANK-IN (INDX) TO RAPP-ADBYTANK                
087000           END-IF                                                         
087100           PERFORM IMS-REPL-WDM601                                        
087200           MOVE '001'      TO RESP-IDMSG-INFO                             
087300*          UPDATE OK                                                      
087400         END-IF                                                           
087500       END-IF                                                             
087600       ADD +1 TO INDX                                                     
087700     END-PERFORM                                                          
087800                                                                          
087900     .                                                                    
088000     EJECT                                                                
088100                                                                          
088200 DA-NEW-SEGMENT-WDK7 SECTION.                                             
088300                                                                          
088400     MOVE 'DA-NEW-SEGMENT WDK7' TO CURRENT-SECTION                        
088500     MOVE ALL '+'             TO WDK7-W005WDK7                            
088600     MOVE 'WDK711'            TO WDK7-IDSEGM                              
088700     MOVE W-IDARTNR-K7        TO WDK7-IDARTNR-KFB                         
088800     MOVE WS-SDC-91           TO WDK7-IDDC-KFB                            
088900                                 WDK7-IDDC                                
089000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
089100                                       WDK6-PCB WDK7-PCB                  
089200     MOVE W-IDDC    TO WS-IDDC-SPAR                                       
089300     IF (DCS-NDC-CN AND NOT DCS-IDDC = WC-NDC-CN-71) OR                   
089400          (DCS-SDC AND DCS-CHINA)                                         
089500       PERFORM DAA-CREAT-WDK7-DC71                                        
089600       MOVE WS-IDDC-SPAR     TO  W-IDDC                                   
089700     END-IF                                                               
089800     .                                                                    
089900     EJECT                                                                
090000                                                                          
090100 DAA-CREAT-WDK7-DC71 SECTION.                                             
090200                                                                          
090300      MOVE    WC-NDC-CN-71   TO    W-IDDC-K7                              
090400      PERFORM IMS-GU-DC71-WDK711                                          
090500      IF SEGMENT-SAKNAS                                                   
090600        MOVE ALL '+'          TO WDK7-W005WDK7                            
090700        MOVE 'WDK711'         TO WDK7-IDSEGM                              
090800        MOVE W-IDARTNR-K7     TO WDK7-IDARTNR-KFB                         
090900        MOVE WC-NDC-CN-71     TO WDK7-IDDC-KFB                            
091000                                 WDK7-IDDC                                
091100        MOVE NEJ              TO WDK7-FLWILSON                            
091110        MOVE NEJ              TO WDK7-FLREFILL                            
091200        CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                        
091300                                          WDK6-PCB WDK7-PCB               
092800      END-IF                                                              
092900      .                                                                   
093000      EJECT                                                               
093100                                                                          
103800 E-READ-SHOW-INFO SECTION.                                                
103900                                                                          
104000     MOVE 'E-READ-SHOW-INFO' TO CURRENT-SECTION                           
104100                                                                          
104200     MOVE WS-IDDC-1   TO W-IDDC                                           
104300                         W-IDDC-A1                                        
104400                         W-IDDC-A1-MAX                                    
104500                         W-IDDC-B1                                        
104600                         W-IDDC-B1-MAX                                    
104700                         W-IDDC-C1                                        
104800                         W-IDDC-C1-MAX                                    
104900                         W-IDDC-D1                                        
105000                         W-IDDC-D1-MAX                                    
105100                                                                          
105200     IF WS-IDDISTR-KEY  NOT = ZERO                                        
105300       MOVE WS-IDDISTR-KEY TO W-IDDISTR                                   
105400                              W-IDDISTR-A1                                
105500                              W-IDDISTR-A1-MAX                            
105600                              W-IDDISTR-B1                                
105700                              W-IDDISTR-B1-MAX                            
105800                              W-IDDISTR-C1                                
105900                              W-IDDISTR-C1-MAX                            
106000                              W-IDDISTR-D1                                
106100                              W-IDDISTR-D1-MAX                            
106200     END-IF                                                               
106300                                                                          
106400     IF WS-IDKUNDNR-KEY NOT = ZERO                                        
106500        MOVE WS-IDKUNDNR-KEY TO W-IDKUNDNR                                
106600     END-IF                                                               
106700                                                                          
106800     IF WS-IDBYTRAP-KEY NOT = ZERO                                        
106900       MOVE WS-IDBYTRAP-KEY TO W-IDBYTRAP                                 
107000                               W-IDBYTRAP-A1                              
107100                               W-IDBYTRAP-B1                              
107200                               W-IDBYTRAP-C1                              
107300                               W-IDBYTRAP-D1                              
107400     END-IF                                                               
107500                                                                          
107600     IF WS-KDBYTSTA-KEY NOT = ZERO                                        
107700       MOVE WS-KDBYTSTA-KEY TO W-KDBYTSTA                                 
107800                               W-KDBYTSTA-A1                              
107900                               W-KDBYTSTA-A1-MAX                          
108000                               W-KDBYTSTA-B1                              
108100                               W-KDBYTSTA-B1-MAX                          
108200                               W-KDBYTSTA-C1                              
108300                               W-KDBYTSTA-C1-MAX                          
108400                               W-KDBYTSTA-D1                              
108500                               W-KDBYTSTA-D1-MAX                          
108600     END-IF                                                               
108700                                                                          
108800     IF WS-KDBYTSTA-KEY = '9'                                             
108900       MOVE WS-RENSNINGSDATUM     TO W-DAREGDAT-A1                        
109000                                     W-DAREGDAT-D1                        
109100     END-IF                                                               
109200                                                                          
109300     PERFORM EA-READ-DB                                                   
109400     IF SEGMENT-SAKNAS                                                    
109500       MOVE 'REPORT' TO RESP-IDELMT-ERROR                                 
109600       MOVE '027'      TO RESP-IDMSG-ERROR                                
109700     ELSE                                                                 
109800       MOVE +1 TO INDX                                                    
109900                                                                          
110000       PERFORM UNTIL INDX > MAX-INDX                                      
110100         IF SEGMENT-FINNS                                                 
110200           PERFORM EC-DATA-TO-RESP                                        
110300           PERFORM EA-READ-DB                                             
110400         END-IF                                                           
110500         ADD 1 TO INDX                                                    
110600       END-PERFORM                                                        
110700       MOVE WS-KVRADER     TO RESP-KVRADER                                
110800     END-IF                                                               
110900     .                                                                    
111000     EJECT                                                                
111100                                                                          
111200 EA-READ-DB SECTION.                                                      
111300                                                                          
111400     MOVE 'EA-READ-DB' TO CURRENT-SECTION                                 
111500     EVALUATE WS-STYR-LAS                                                 
111600       WHEN '   2 '                                                       
111700         PERFORM IMS-GN-WDM601-A1                                         
111800       WHEN '    3'                                                       
111900         PERFORM IMS-GN-WDM601-B1                                         
112000       WHEN 'D   3'                                                       
112100         PERFORM IMS-GN-WDM6C1                                            
112200         IF SEGMENT-FINNS                                                 
112300           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
112400           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
112500           PERFORM IMS-GHU-WDM601                                         
112600         END-IF                                                           
112700       WHEN 'D R 3'                                                       
112800         PERFORM IMS-GN-WDM6C1-RAP                                        
112900         IF SEGMENT-FINNS                                                 
113000           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
113100           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
113200           PERFORM IMS-GHU-WDM601                                         
113300         END-IF                                                           
113400       WHEN 'DK  3'                                                       
113500         PERFORM IMS-GN-WDM6C1-KUN                                        
113600         IF SEGMENT-FINNS                                                 
113700           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
113800           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
113900           PERFORM IMS-GHU-WDM601                                         
114000         END-IF                                                           
114100       WHEN 'DKR 3'                                                       
114200         PERFORM IMS-GN-WDM6C1-KUN-RAP                                    
114300         IF SEGMENT-FINNS                                                 
114400           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
114500           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
114600           PERFORM IMS-GHU-WDM601                                         
114700         END-IF                                                           
114800       WHEN 'D  2 '                                                       
114900         PERFORM IMS-GN-WDM6D1                                            
115000         IF SEGMENT-FINNS                                                 
115100           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
115200           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
115300           PERFORM IMS-GHU-WDM601                                         
115400         END-IF                                                           
115500       WHEN 'DK 2 '                                                       
115600         PERFORM IMS-GN-WDM6D1-KUN                                        
115700         IF SEGMENT-FINNS                                                 
115800           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
115900           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
116000           PERFORM IMS-GHU-WDM601                                         
116100         END-IF                                                           
116200       WHEN 'D R2 '                                                       
116300         PERFORM IMS-GN-WDM6D1-RAP                                        
116400         IF SEGMENT-FINNS                                                 
116500           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
116600           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
116700           PERFORM IMS-GHU-WDM601                                         
116800         END-IF                                                           
116900       WHEN 'DKR2 '                                                       
117000         PERFORM IMS-GN-WDM6D1-KUN-RAP                                    
117100         IF SEGMENT-FINNS                                                 
117200           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
117300           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
117400           PERFORM IMS-GHU-WDM601                                         
117500         END-IF                                                           
117600     END-EVALUATE                                                         
117700     .                                                                    
117800     EJECT                                                                
117900                                                                          
118000 EC-DATA-TO-RESP SECTION.                                                 
118100                                                                          
118200     MOVE 'EC-DATA-TO-RESP' TO CURRENT-SECTION                            
118300     MOVE SPACE                    TO RESP-KDCMD-RAD  (INDX)              
118400     MOVE RAPP-IDDISTR             TO RESP-IDDISTR    (INDX)              
118500     MOVE RAPP-IDKUNDNR            TO RESP-IDKUNDNR   (INDX)              
118600     MOVE RAPP-IDFAKT              TO RESP-IDFAKT     (INDX)              
118700     MOVE RAPP-IDBYTRAP            TO RESP-IDBYTRAP   (INDX)              
118800     MOVE RAPP-KVRETUR-TOT         TO RESP-KVRETUR-TOT(INDX)              
118900     MOVE RAPP-DAREGDAT(3:6)       TO RESP-TIREGDAT   (INDX)              
119000     MOVE RAPP-KDBYTSTA-RAPP       TO RESP-KDBYTSTA   (INDX)              
119100     MOVE RAPP-ADBYTANK            TO RESP-ADBYTANK   (INDX)              
           IF RAPP-FLBYTGAR = 'J' OR 'Y'                                        
              MOVE 'Y'                   TO RESP-FLBYTGAR   (INDX)              
           ELSE                                                                 
              MOVE RAPP-FLBYTGAR         TO RESP-FLBYTGAR   (INDX)              
           END-IF                                                               
119300     MOVE RAPP-DAANKDAG(3:6)       TO RESP-TIANKDAG   (INDX)              
119400     MOVE RAPP-DAREGDAT-GODK(3:6)  TO RESP-TIREGDAT-GODK(INDX)            
119500     ADD  1                        TO WS-KVRADER                          
119600     .                                                                    
119700     EJECT                                                                
119800                                                                          
119900 G-PRINT-LIST SECTION.                                                    
120000                                                                          
120100     MOVE 'G-PRINT-LIST' TO CURRENT-SECTION                               
120200     MOVE YES TO INDATA-SW                                                
120300     MOVE +1 TO INDX                                                      
120400     PERFORM UNTIL INDX > MAX-INDX OR PRINT-LINE-FOUND                    
120500       IF REQU-KDCMD-RAD (INDX) = 'J'                                     
120600         INSPECT REQU-IDDISTR (INDX)                                      
120700                 REPLACING LEADING SPACE BY ZERO                          
120800         INSPECT REQU-IDBYTRAP (INDX)                                     
120900                 REPLACING LEADING SPACE BY ZERO                          
                                                                                
               IF REQU-IDDISTR (INDX) NUMERIC                                   
121000            MOVE REQU-IDDISTR (INDX)  TO W-IDDISTR-UNIK                   
               ELSE                                                             
                  MOVE ZEROES               TO W-IDDISTR-UNIK                   
               END-IF                                                           
                                                                                
121100         MOVE REQU-IDBYTRAP (INDX) TO W-IDBYTRAP-UNIK                     
121200         PERFORM IMS-GU-WDM601                                            
121300         IF SEGMENT-FINNS                                                 
121400           PERFORM S90-SEND-OPEN                                          
121500           PERFORM S90-PUT-DAP-START                                      
121600           MOVE '1'                TO HEAD-IDAFPRCD                       
121700           MOVE WS-IDDC            TO HEAD-IDDC                           
121800           MOVE RAPP-IDDISTR       TO HEAD-IDDISTR                        
121900           MOVE RAPP-IDKUNDNR      TO HEAD-IDKUNDNR                       
122000           MOVE RAPP-IDBYTRAP      TO HEAD-IDBYTRAP                       
122100           MOVE RAPP-IDFAKT        TO HEAD-IDFAKT                         
122200           MOVE RAPP-KDBYTSTA-RAPP TO HEAD-KDBYTSTA                       
122300           PERFORM S90-PUT-CORE-REPORT-HEAD                               
122400           PERFORM IMS-GNP-WDM611                                         
122500           IF SEGMENT-FINNS                                               
122600             PERFORM UNTIL SEGMENT-SAKNAS                                 
122700               MOVE '2'             TO LINE-IDAFPRCD                      
122800               MOVE OBJ-KVRETUR-URSP TO LINE-KVRETUR-URSP                 
122900               MOVE OBJ-IDARTNR-OBJ  TO LINE-IDARTNR-OBJ                  
123000               MOVE OBJ-IDARTNR-OBJ  TO W-IDARTNR-D3                      
123100               PERFORM IMS-GU-WDD311                                      
123200               IF SEGMENT-FINNS                                           
123300                 MOVE TEXT-BEART     TO LINE-BEART                        
123400               END-IF                                                     
123500               PERFORM S90-PUT-CORE-REPORT-LINE                           
123600               PERFORM IMS-GNP-WDM611                                     
123700             END-PERFORM                                                  
123800             PERFORM S90-SEND-CLOSE                                       
123900           ELSE                                                           
124000             MOVE NOO TO INDATA-SW                                        
124100             MOVE 'REPORT' TO RESP-IDELMT-ERROR                           
124200             MOVE '027'      TO RESP-IDMSG-ERROR                          
124300*            LINES NOT FOUND                                              
124400           END-IF                                                         
124500         ELSE                                                             
124600           MOVE NOO TO INDATA-SW                                          
124700           MOVE 'REPORT' TO RESP-IDELMT-ERROR                             
124800           MOVE '025'      TO RESP-IDMSG-ERROR                            
124900*          NOT FOUND                                                      
125000         END-IF                                                           
125100         MOVE YES TO PRINT-LINE-SW                                        
125200       END-IF                                                             
125300       ADD +1 TO INDX                                                     
125400     END-PERFORM                                                          
125500     IF INDATA-FEL                                                        
125600       IF RESP-IDMSG-ERROR NOT = SPACE                                    
125700         MOVE ALL '+' TO RESP-WL019401                                    
125800       END-IF                                                             
125900     END-IF                                                               
126000     .                                                                    
126100     EJECT                                                                
126200                                                                          
126300                                                                          
126400 Z-FINIT SECTION.                                                         
126500                                                                          
126600     MOVE 'Z-FINIT' TO CURRENT-SECTION                                    
126700     CONTINUE                                                             
126800     .                                                                    
126900     EJECT                                                                
127000*    --- DISPATCHER SECTIONS                                              
127100 S01-HAEMTA-ANROPSDATA SECTION.                                           
127200                                                                          
127300     MOVE 'S01-HAMTA-ANROPSDATA' TO CURRENT-SECTION                       
127400     MOVE 'GETARG'               TO SUB-KDFUNC                            
127500     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
127600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
127700                                                                          
127800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
127900                                                                          
128000     IF SUB-KDRC > 0                                                      
128100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
128200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
128300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
128400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
128500     END-IF                                                               
128600     .                                                                    
128700     SKIP3                                                                
128800 S02-RETURNERA-SVAR SECTION.                                              
128900                                                                          
129000     MOVE 'S02-RETURNERA-SVAR' TO CURRENT-SECTION                         
129100     COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA                           
129200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
129300     MOVE WS-RESP-AREA                TO SUB-KVDLEN                       
129400                                                                          
129500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
129600                                                                          
129700     IF SUB-KDRC > 0                                                      
129800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
129900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
130000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
130100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
130200     END-IF                                                               
130300     .                                                                    
130400     EJECT                                                                
130500                                                                          
130600                                                                          
130700                                                                          
130800 S05-NUM-TEXT SECTION.                                                    
130900                                                                          
131000     MOVE 'S05-NUM-TEXT' TO CURRENT-SECTION                               
131100     INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                    
131200     CALL W009REDU USING WS-REDUIN WS-REDUUT                              
131300     .                                                                    
131400                                                                          
131500                                                                          
131600 S90-SEND-OPEN SECTION.                                                   
131700                                                                          
131800     MOVE 'S90-SEND-OPEN' TO CURRENT-SECTION                              
131900     MOVE 'OPEN'                        TO SEND-KDFUNC                    
132000     MOVE 'CARPARTS.DAP.DISTRWEB'       TO SEND-ADDISPABS                 
132100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
132200                         SEND-OPEN-AREA                                   
132300     IF SEND-KDRC > 0                                                     
132400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
132500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
132600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
132700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
132800     END-IF                                                               
132900     .                                                                    
133000     EJECT                                                                
133100 S90-PUT-DAP-START SECTION.                                               
133200                                                                          
133300     MOVE 'S90-PUT-DAB-START'     TO CURRENT-SECTION                      
133400     MOVE 1                       TO HDR-REQU-IDMSGVER                    
133500     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
133600     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
133700     MOVE 'CORE-REPORT'           TO HDR-IDOUTTYPE                        
133800     MOVE SPACE                   TO HDR-IDOUTREC                         
133900     MOVE REQU-IDBYTRAP (INDX)    TO HDR-IDLIST                           
134000     MOVE WS-IDDC                 TO WS-DAP-IDDC                          
134100     MOVE RAPP-IDDISTR            TO WS-NUM5                              
134200     MOVE WS-NUM5                 TO WS-REDUIN                            
134300     PERFORM S05-NUM-TEXT                                                 
134400     MOVE WS-REDUUT               TO WS-DAP-IDDISTR                       
134500     MOVE WS-DAP-HDR              TO HDR-IDOUTREC                         
134600     MOVE 'PUT'                   TO SEND-KDFUNC                          
134700     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
134800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
134900                         SEND-KVDLEN                                      
135000                         HDR-AREA                                         
135100     IF SEND-KDRC > ZERO                                                  
135200       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
135300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
135400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
135500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
135600     END-IF                                                               
135700     .                                                                    
135800     EJECT                                                                
135900 S90-PUT-CORE-REPORT-HEAD SECTION.                                        
136000                                                                          
136100     MOVE 'S90-PUT-CORE-REPORT-HEAD' TO CURRENT-SECTION                   
136200     MOVE 'PUT'                           TO SEND-KDFUNC                  
136300     MOVE LENGTH OF REPORT-HEADER         TO SEND-KVDLEN                  
136400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
136500                         SEND-KVDLEN                                      
136600                         REPORT-HEADER                                    
136700     IF SEND-KDRC > ZERO                                                  
136800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
136900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
137000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
137100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
137200     END-IF                                                               
137300     .                                                                    
137400     EJECT                                                                
137500                                                                          
137600 S90-PUT-CORE-REPORT-LINE SECTION.                                        
137700                                                                          
137800     MOVE 'S90-PUT-CORE-REPORT-LINE' TO CURRENT-SECTION                   
137900     MOVE 'PUT'                           TO SEND-KDFUNC                  
138000     MOVE LENGTH OF REPORT-LINE           TO SEND-KVDLEN                  
138100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
138200                         SEND-KVDLEN                                      
138300                         REPORT-LINE                                      
138400                                                                          
138500     IF SEND-KDRC > ZERO                                                  
138600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
138700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
138800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
138900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
139000     END-IF                                                               
139100     .                                                                    
139200     EJECT                                                                
139300 S90-SEND-CLOSE SECTION.                                                  
139400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
139500     MOVE 'S90-SEND-CLOSE'           TO CURRENT-SECTION                   
139600*    DISPLAY 'WL0194 CLOSE                '                               
139700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
139800                                                                          
139900     IF SEND-KDRC > 0                                                     
140000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
140100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
140200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700* --- IMS SEKTIONER ---                                                   
140800 IMS-GN-WDM601-A1 SECTION.                                                
140900       MOVE 'GN-WDM601-A1' TO CURRENT-IMS-SECTION                         
141000     STRING 'WDM601  (WDM6ASEQ=>' W-WDM6ASEQ-MIN-X                        
141100                    '&WDM6ASEQ<=' W-WDM6ASEQ-MAX-X ')'                    
141200          DELIMITED BY SIZE INTO SSA1                                     
141300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
141400     CALL CBLTDLI USING GN WDM6A-PCB DLI-IO-WDM601 SSA1                   
141500     MOVE WDM6A-STATUS-CODE TO STATUS-WS                                  
141600     PERFORM IMS-STATUSKONTROLL                                           
141700     .                                                                    
141800     SKIP2                                                                
141900 IMS-GN-WDM601-B1 SECTION.                                                
142000       MOVE 'GN-WDM601-B1' TO CURRENT-IMS-SECTION                         
142100     STRING 'WDM601  (WDM6BSEQ=>' W-WDM6BSEQ-MIN-X                        
142200                    '&WDM6BSEQ<=' W-WDM6BSEQ-MAX-X ')'                    
142300          DELIMITED BY SIZE INTO SSA1                                     
142400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
142500     CALL CBLTDLI USING GN WDM6B-PCB DLI-IO-WDM601 SSA1                   
142600     MOVE WDM6B-STATUS-CODE TO STATUS-WS                                  
142700     PERFORM IMS-STATUSKONTROLL                                           
142800     .                                                                    
142900     EJECT                                                                
143000 IMS-GN-WDM6C1  SECTION.                                                  
143100       MOVE 'GN-WDM6C1' TO CURRENT-IMS-SECTION                            
143200     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
143300                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X ')'                    
143400          DELIMITED BY SIZE INTO SSA1                                     
143500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
143600     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
143700     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
143800     PERFORM IMS-STATUSKONTROLL                                           
143900     .                                                                    
144000     SKIP2                                                                
144100 IMS-GN-WDM6C1-RAP  SECTION.                                              
144200       MOVE 'GN-WDM6C1-RAP' TO CURRENT-IMS-SECTION                        
144300     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
144400                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X                        
144500                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
144600          DELIMITED BY SIZE INTO SSA1                                     
144700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
144800     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
144900     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
145000     PERFORM IMS-STATUSKONTROLL                                           
145100     .                                                                    
145200     EJECT                                                                
145300 IMS-GN-WDM6C1-KUN  SECTION.                                              
145400       MOVE 'GN-WDM6C1-KUN' TO CURRENT-IMS-SECTION                        
145500     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
145600                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X                        
145700                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
145800          DELIMITED BY SIZE INTO SSA1                                     
145900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
146000     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
146100     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     SKIP2                                                                
146500 IMS-GN-WDM6C1-KUN-RAP  SECTION.                                          
146600       MOVE 'GN-WDM6C1-KUN-RAP' TO CURRENT-IMS-SECTION                    
146700     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
146800                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X                        
146900                    '&IDKUNDNR =' W-IDKUNDNR-X                            
147000                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
147100          DELIMITED BY SIZE INTO SSA1                                     
147200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
147300     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
147400     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
147500     PERFORM IMS-STATUSKONTROLL                                           
147600     .                                                                    
147700     EJECT                                                                
147800 IMS-GN-WDM6D1  SECTION.                                                  
147900       MOVE 'GN-WDM6D1' TO CURRENT-IMS-SECTION                            
148000     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
148100                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X ')'                    
148200          DELIMITED BY SIZE INTO SSA1                                     
148300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
148400     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
148500     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
148600     PERFORM IMS-STATUSKONTROLL                                           
148700     .                                                                    
148800     SKIP2                                                                
148900 IMS-GN-WDM6D1-RAP  SECTION.                                              
149000       MOVE 'GN-WDM6D1-RAP' TO CURRENT-IMS-SECTION                        
149100     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
149200                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
149300                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
149400          DELIMITED BY SIZE INTO SSA1                                     
149500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
149600     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
149700     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     EJECT                                                                
150100 IMS-GN-WDM6D1-KUN  SECTION.                                              
150200       MOVE 'GN-WDM6D1-KUN' TO CURRENT-IMS-SECTION                        
150300     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
150400                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
150500                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
150600          DELIMITED BY SIZE INTO SSA1                                     
150700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
150800     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
150900     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
151000     PERFORM IMS-STATUSKONTROLL                                           
151100     .                                                                    
151200     SKIP2                                                                
151300 IMS-GN-WDM6D1-KUN-RAP  SECTION.                                          
151400       MOVE 'GN-WDM6D1-KUN-RAP' TO CURRENT-IMS-SECTION                    
151500     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
151600                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
151700                    '&IDKUNDNR =' W-IDKUNDNR-X                            
151800                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
151900          DELIMITED BY SIZE INTO SSA1                                     
152000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
152100     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
152200     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500     EJECT                                                                
152600 IMS-GHU-WDM601 SECTION.                                                  
152700       MOVE 'GHU-WDM601' TO CURRENT-IMS-SECTION                           
152800     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
152900          DELIMITED BY SIZE INTO SSA1                                     
153000     MOVE '  GE' TO GODK-STATUSKODER                                      
153100     CALL CBLTDLI USING GHU WDM6-PCB DLI-IO-WDM601 SSA1                   
153200     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
153300     PERFORM IMS-STATUSKONTROLL                                           
153400     .                                                                    
153500     SKIP2                                                                
153600 IMS-GU-WDM601 SECTION.                                                   
153700       MOVE 'GHU-WDM601' TO CURRENT-IMS-SECTION                           
153800     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
153900          DELIMITED BY SIZE INTO SSA1                                     
154000     MOVE '  GE' TO GODK-STATUSKODER                                      
154100     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
154200     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
154300     PERFORM IMS-STATUSKONTROLL                                           
154400     .                                                                    
154500     SKIP2                                                                
154600 IMS-GNP-WDM611  SECTION.                                                 
154700       MOVE 'GNP-WDM611' TO CURRENT-IMS-SECTION                           
154800                                                                          
154900     MOVE   'WDM611'          TO SSA1                                     
155000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
155100     CALL CBLTDLI USING GNP  WDM6-PCB DLI-IO-WDM611 SSA1                  
155200     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
155300     PERFORM IMS-STATUSKONTROLL                                           
155400     .                                                                    
155500     SKIP2                                                                
155600 IMS-REPL-WDM601 SECTION.                                                 
155700       MOVE 'REPL-WDM601' TO CURRENT-IMS-SECTION                          
155800                                                                          
155900     MOVE '  ' TO GODK-STATUSKODER                                        
156000     CALL CBLTDLI USING REPL WDM6-PCB DLI-IO-WDM601                       
156100     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     EJECT                                                                
156500 IMS-GU-WDB601    SECTION.                                                
156600     MOVE 'IMS-GU-WDB601'        TO CURRENT-IMS-SECTION                   
156700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     MOVE '  GE' TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
157100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     IF SEGMENT-SAKNAS                                                    
157400         MOVE SPACE TO DCS-KDDC                                           
157500     END-IF                                                               
157600     .                                                                    
157700                                                                          
157800                                                                          
157900 IMS-GU-WDK701 SECTION.                                                   
158000                                                                          
158100     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-K7-X ')'                      
158200          DELIMITED BY SIZE INTO SSA1                                     
158300     MOVE '  GE' TO GODK-STATUSKODER                                      
158400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701  SSA1                   
158500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
158600     PERFORM IMS-STATUSKONTROLL                                           
158700     .                                                                    
158800     SKIP3                                                                
158900 IMS-GU-WDK711 SECTION.                                                   
159000                                                                          
159100     STRING 'WDK711  (IDDC    = ' W-IDDC-K7-X ')'                         
159200          DELIMITED BY SIZE INTO SSA2                                     
159300     MOVE '  GE' TO GODK-STATUSKODER                                      
159400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA2                    
159500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
159600     PERFORM IMS-STATUSKONTROLL                                           
159700     .                                                                    
159800     EJECT                                                                
159900                                                                          
160000 IMS-GU-WDD311     SECTION.                                               
160100     MOVE 'IMS-GU-WDD311'        TO CURRENT-IMS-SECTION                   
160200                                                                          
160300     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
160400            DELIMITED BY SIZE INTO SSA1                                   
160500     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
160600            DELIMITED BY SIZE INTO SSA2                                   
160700     MOVE '  GE' TO GODK-STATUSKODER                                      
160800     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
160900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200     EJECT                                                                
161300 IMS-GU-DC71-WDK711 SECTION.                                              
161400                                                                          
161500     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-K7-X ')'                      
161600          DELIMITED BY SIZE INTO SSA1                                     
161700     STRING 'WDK711  (IDDC    = ' W-IDDC-K7-X ')'                         
161800          DELIMITED BY SIZE INTO SSA2                                     
161900     MOVE '  GE' TO GODK-STATUSKODER                                      
162000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
162100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
162200     PERFORM IMS-STATUSKONTROLL                                           
162300     .                                                                    
162400     SKIP3                                                                
166700 IMS-STATUSKONTROLL SECTION.                                              
166800                                                                          
166900     SET STATUS-IX TO 1                                                   
167000     SEARCH GODK-STATUS                                                   
167100       AT END                                                             
167200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
167300         DELIMITED BY SIZE INTO ERROR-TEXT                                
167400         CALL FELLOG                                                      
167500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
167600         CONTINUE                                                         
167700     END-SEARCH                                                           
167800     .                                                                    
