000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL015600.                                                
000400 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000500 DATE-WRITTEN.   04/09/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       'CARPARTS.LDC.RECEIVINGRETURNS'                          
000900*                                                                         
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        REGISTRERING AV MOTTAGNA RETURER HOS RETURTERMINAL.              
001300*        SKAPAR R31 FÖR EGNA RETURER SOM SKALL LÄGGAS IN PÅ               
001400*        MOTAGANDE LDC.TAR EMOT TRANSITGODS OCH SÄTTER STATUS 7.          
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001700*        PROGRAMMET UPPDATERAR WLRETB (WDA3)                              
001900*        PROGRAMMET LÄSER      WLRETG (WDA3)                              
002000*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
002100*        PROGRAMMET LÄSER      WL4103 (WDR1)                              
002200*        PROGRAMMET UPPDATERAR WL4111 (WDR1)                              
002300*    SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                     
002400*                                                                         
002500*    WEB-LDC:                                                             
002600*        WL015600 PROGRAM IS A REPLICA OF W4074100 PROGRAM                
002700*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002800*                                                                         
002900*    E'TRACKER: 4230251 2006-12  OMSKRIVEN AV SUSANNE OLSSON.             
003000*               5608145 2007-09  EXTRA KONTROLL AV IDRTLOP/WL4111         
003100*              10143271 2011-09  CHINA WAREHOUSE PROJECT-1                
003200*              10240190 2014-09  TA EMOT RETUR PÅ DC51.                   
003300*              10285913 2016-08  CHANGES IN INPUT CONTROL (ABEND)         
003400*              10296404 2017-01  RETURNS FROM CA TO US                    
003500*              10302968 2017-07  GENERIC SOLUTION IDFTG                   
003600*                                                                         
003700*                                                                         
003800*    INDATA.                                                              
003900*        TRANSACTION: WL0156U                                             
004000*        REQUEST:     WL0156I1                                            
004100*                     WZ01REQ2                                            
004200*                                                                         
004300*    OUTDATA.                                                             
004400*        RESPONSE:    WL0156O1                                            
004500*                     WZ01RES2                                            
004600                                                                          
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     SKIP2                                                                
005000 INPUT-OUTPUT SECTION.                                                    
005100                                                                          
005200 FILE-CONTROL.                                                            
005300     EJECT                                                                
005400 DATA DIVISION.                                                           
005500     SKIP3                                                                
005600 FILE SECTION.                                                            
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900 77  IDPGM                       PIC X(08)   VALUE 'WL015600'.            
006000                                                                          
006100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
006200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
006300 77  KDRC-DISPLAY                PIC Z(5).                                
006400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006500 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
006600 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
006700 01  W-IDRTLOP-RAK               PIC 9(4)    VALUE ZERO.                  
006800                                                                          
006900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007100                                                                          
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007400 77  NOO                         PIC X       VALUE 'N'.                   
007500 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
007600 77  WS-CDC-SE                   PIC X(2)    VALUE '11'.                  
007700 77  SPAR-IDRTLOP-R31            PIC 9(3)    VALUE ZERO.                  
007800 77  W-SND-REG                   PIC X(1)    VALUE '1'.                   
007900 77  W-SND-SAENT                 PIC X(1)    VALUE '2'.                   
008000 77  W-SND-MOT                   PIC X(1)    VALUE '4'.                   
008100 77  W-SND-TRANSIT               PIC X(1)    VALUE '7'.                   
008200 77  W-KLI-SAENT                 PIC S9(1)   COMP-3 VALUE +4.             
008300 77  W-KLI-MOT                   PIC S9(1)   COMP-3 VALUE +5.             
008400 77  SW-IDRT                     PIC X      VALUE 'N'.                    
008500 77  SW-IDRTLOP                  PIC X      VALUE 'N'.                    
008600 77  SW-IDDISTR                  PIC X      VALUE 'N'.                    
008700 77  SW-IDANSTNR                 PIC X      VALUE 'N'.                    
008800                                                                          
008900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
009000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
009100 77  INDX2                       PIC S9(4)  VALUE +0    COMP SYNC.        
009200 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
009300 77  4792-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
009400 77  4792-MAX-INDX               PIC S9(4)  VALUE +24   COMP SYNC.        
009500 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
009600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009700                                                                          
009800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009900     88  INDATA-OK                           VALUE 'J'.                   
010000     88  INDATA-FEL                          VALUE 'N'.                   
010100                                                                          
010200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010300     88  NYCKLAR-OK                          VALUE 'J'.                   
010400     88  NYCKLAR-FEL                         VALUE 'N'.                   
010500                                                                          
010600 77  SKAPA-R31-SW                PIC X       VALUE 'N'.                   
010700     88  SKAPA-R31                           VALUE 'J'.                   
010800                                                                          
010900 77  W-UPDATE-SW                 PIC X       VALUE 'N'.                   
011000     88  W-UPDATE-OK                         VALUE 'J'.                   
011100 77  KEYS-SW                   PIC X      VALUE 'J'.                      
011200     88  KEYS-OK                          VALUE 'J'.                      
011300     88  KEYS-WRONG                       VALUE 'N'.                      
011400                                                                          
011500       EJECT                                                              
011600 77  W-FLFARLIG                  PIC X(1).                                
011700 77  W-FLBUYBAC                  PIC X(1) VALUE 'N'.                      
011800 77  W-IDPERSON                  PIC S9(3)   COMP-3.                      
011900 77  W-KDARBTYP                  PIC X(8).                                
012000 77  W-KVRADER                   PIC S9(5)   COMP-3 VALUE ZERO.           
012100 77  W-IDDC-RET                  PIC X(2) VALUE SPACE.                    
012200 77  W-IDDC-SANDNING             PIC X(2) VALUE SPACE.                    
012300                                                                          
012400 77  WS-IDELMT-ERROR             PIC X(16).                               
012500 77  WS-IDMSG-ERROR              PIC X(03).                               
012600 77  WS-IDMSG-INFO               PIC X(03).                               
012700                                                                          
012800 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
012900     88  REC-LIMIT                           VALUE 'J'.                   
013000                                                                          
013100 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
013200                                                                          
013300 01  W-KDANMORS                  PIC X(2).                                
013400     88  KDANMORS-BUYBAC-98                  VALUE '98'.                  
013500     EJECT                                                                
013600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
013700 01  GENERAL-SUBPROGRAMS.                                                 
013800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
014100     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
014200     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
014300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
014400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014500     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
014600     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
014700     SKIP3                                                                
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL SUBPROGRAM W418ANSV                              
015000*01 -COPY W418ANSV                                                        
015100     EJECT                                                                
015200*    --- PARAMETERS TO ABEND                                              
015300                                                                          
015400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015700                                                                          
015800     EJECT                                                                
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016100     SKIP3                                                                
016200*01  -COPY WZ01SUB                                                        
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
016500*01  -COPY WMSGCONV                                                       
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
016800*01  -COPY WZ01AUTH                                                       
016900     EJECT                                                                
017000                                                                          
017100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
017200*01  -COPY WZ01SEND                                                       
017300                                                                          
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
017600     SKIP3                                                                
017700 01  REQU-AREA.                                                           
017800*    03  -COPY WZ01REQ2                                                   
017900*    03  -COPY WL0156I1                                                   
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
018200     SKIP3                                                                
018300 01  RESP-AREA.                                                           
018400*    03  -COPY WZ01RES2                                                   
018500*    03  -COPY WL0156O1                                                   
018600                                                                          
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
018900 01  HDR-AREA.                                                            
019000*    03  -COPY WZ01REQU  -PRE HDR-                                        
019100*    03  -COPY WZ04HDR                                                    
019200*                                                                         
019300 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
019400 01  DOC-AREA.                                                            
019500*    03  -COPY WL01561                                                    
019600     EJECT                                                                
019700*                                                                         
019800*01  -COPY WWDC99                                                         
019900     EJECT                                                                
020000                                                                          
020100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA3B1'.         
020200 01  DLI-IO-WDA3B1.                                                       
020300*    03  -COPY WDA3B1   -PRE RET-                                         
020400     EJECT                                                                
020500                                                                          
020600 01  MESSAGE-CODES.                                                       
020700     03  ERR-OTILL-UPD           PIC X(3)    VALUE '007'.                 
020800     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
020900     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '004'.                 
021000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
021100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '190'.                 
021200     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '244'.                 
021300     03  ERR-FINNS-EJ-PA-REG     PIC X(3)    VALUE '245'.                 
021400     03  ERR-FEL-STATUS          PIC X(3)    VALUE '115'.                 
021500     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
021600     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
021700                                                                          
021800     EJECT                                                                
021900                                                                          
022000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
022100     SKIP3                                                                
022200 01  KOM-MSG-IO-AREA.                                                     
022300*03  -COPY WMSGKOM                                                        
022400     EJECT                                                                
022500 01  FILLER                   PIC X(16)   VALUE 'MSG/KOM-AREA'.           
022600     SKIP2                                                                
022700*01  -COPY WMSGSNUF           -PRE P-TO-P-                                
022800                                                                          
022900     EJECT                                                                
023000 01      FILLER                  PIC X(24)   VALUE                        
023100                                 'MOD4792-MID-W4I79201'.                  
023200     SKIP2                                                                
023300     -COPY W4I79201 -PRE MOD4792-                                         
023400     EJECT                                                                
023500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023600*                                                                         
023700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023800     SKIP3                                                                
023900 01  NYCKLAR-TILL-DLI.                                                    
024000     03  W-WDA301KY-X.                                                    
024100         05  W-IDDC              PIC  X(2)    VALUE SPACE.                
024200         05  W-DAREGDAT          PIC  9(8)    VALUE ZERO.                 
024300         05  W-TIKLOCK           PIC S9(9)    VALUE ZERO  COMP-3.         
024400     03  W-WDA3F1KY-MIN-X.                                                
024500         05  W-IDDC-MIN          PIC  X(2)    VALUE SPACE.                
024600         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
024700         05  W-IDKUNDNR-MIN      PIC S9(7)    VALUE ZERO  COMP-3.         
024800         05  W-IDRAPPNR-MIN      PIC  9(7)    VALUE ZERO.                 
024900         05  W-IDRT-MIN          PIC  X(3)    VALUE SPACE.                
025000         05  W-IDRTLOP-MIN       PIC  9(3)    VALUE ZERO.                 
025100         05  W-IDKOLLI-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
025200         05  W-DAREGDAT-MIN      PIC  9(8)    VALUE ZERO.                 
025300         05  W-TIKLOCK-MIN       PIC S9(9)    VALUE ZERO  COMP-3.         
025400     03  W-WDA3F1KY-MAX-X.                                                
025500         05  W-IDDC-MAX          PIC  X(2)    VALUE SPACE.                
025600         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
025700         05  W-IDKUNDNR-MAX      PIC S9(7)    VALUE ZERO  COMP-3.         
025800         05  W-IDRAPPNR-MAX      PIC  9(7)    VALUE ZERO.                 
025900         05  W-IDRT-MAX          PIC  X(3)    VALUE SPACE.                
026000         05  W-IDRTLOP-MAX       PIC  9(3)    VALUE ZERO.                 
026100         05  W-IDKOLLI-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
026200         05  W-DAREGDAT-MAX      PIC  9(8)    VALUE ZERO.                 
026300         05  W-TIKLOCK-MAX       PIC S9(9)    VALUE ZERO  COMP-3.         
026400     03  W-IDLEVANM-X.                                                    
026500         05  W-IDDISTR-ANM       PIC S9(5)    VALUE ZERO  COMP-3.         
026600         05  W-IDKUNDNR-ANM      PIC S9(7)    VALUE ZERO  COMP-3.         
026700         05  W-IDRAPPNR-ANM      PIC  9(7)    VALUE ZERO.                 
026800                                                                          
026900     03  W-WDGXKEY-X.                                                     
027000         05  FILLER              PIC  X(4)   VALUE '4111'.                
027100         05  W-IDRT-4111         PIC  X(3)   VALUE SPACE.                 
027200         05  FILLER              PIC  X(23)  VALUE LOW-VALUE.             
027300                                                                          
027400     03  W-WDA3BSEQ-MIN-X.                                                
027500         05  W-IDRT-BSEQ-MIN       PIC  X(3)          VALUE SPACE.        
027600         05  W-IDDC-BSEQ-MIN       PIC  X(2)          VALUE SPACE.        
027700         05  W-IDRTLOP-BSEQ-MIN    PIC  9(3)          VALUE ZERO.         
027800         05  W-IDKOLLI-BSEQ-MIN    PIC S9(5)   COMP-3 VALUE ZERO.         
027900                                                                          
028000     03  W-WDA3BSEQ-MAX-X.                                                
028100         05  W-IDRT-BSEQ-MAX       PIC  X(3)          VALUE SPACE.        
028200         05  W-IDDC-BSEQ-MAX       PIC  X(2)          VALUE SPACE.        
028300         05  W-IDRTLOP-BSEQ-MAX    PIC  9(3)          VALUE ZERO.         
028400         05  W-IDKOLLI-BSEQ-MAX    PIC S9(5)   COMP-3 VALUE ZERO.         
028500                                                                          
028600     03  W-IDRTLOP-B1-X.                                                  
028700         05  W-IDRTLOP-B1        PIC  9(3)   VALUE ZERO.                  
028800                                                                          
028900     03  W-WDA3B1-MIN-X.                                                  
029000         05  W-IDRT-B1-MIN       PIC  X(3)          VALUE SPACE.          
029100         05  W-IDDC-B1-MIN       PIC  X(2)          VALUE SPACE.          
029200         05  W-IDRTLOP-B1-MIN    PIC  9(3)          VALUE ZERO.           
029300         05  W-IDKOLLI-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
029400         05  W-DAREGDAT-B1-MIN   PIC  9(8)          VALUE ZERO.           
029500         05  W-TIKLOCK-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
029600                                                                          
029700     03  W-WDA3B1-MAX-X.                                                  
029800         05  W-IDRT-B1-MAX       PIC  X(3)          VALUE SPACE.          
029900         05  W-IDDC-B1-MAX       PIC  X(2)          VALUE SPACE.          
030000         05  W-IDRTLOP-B1-MAX    PIC  9(3)          VALUE ZERO.           
030100         05  W-IDKOLLI-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
030200         05  W-DAREGDAT-B1-MAX   PIC  9(8)          VALUE ZERO.           
030300         05  W-TIKLOCK-B1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
030400                                                                          
030500     SKIP2                                                                
030600*    --- STATUS-KOD FRÅN IMS                                              
030700 01  STATUS-WS                   PIC XX.                                  
030800     88  SEGMENT-FINNS                       VALUE '  '.                  
030900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031200     SKIP2                                                                
031300 01  GODK-STATUSKODER.                                                    
031400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031500     SKIP3                                                                
031600 01  SSA1                        PIC X(128).                              
031700 01  SSA2                        PIC X(192).                              
031800     EJECT                                                                
031900*    --- IMS FUNKTIONSKODER                                               
032000*01  -COPY W0003                                                          
032100     EJECT                                                                
032200*    ---  DLI INPUT-OUTPUT AREA                                           
032300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
032400     SKIP3                                                                
032500 01  DLI-IO-AREA.                                                         
032600     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
032700     SKIP3                                                                
032800     03  WLRETA01 REDEFINES IO-AREA.                                      
032900*        05  -COPY WDA301                                                 
033000     EJECT                                                                
033100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
033200     SKIP3                                                                
033300 01  DLI-IO-AREA2.                                                        
033400     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
033500     SKIP3                                                                
033600     03  WLRETG01 REDEFINES IO-AREA2.                                     
033700*        05  -COPY WDA3F1                                                 
033800     EJECT                                                                
033900     03  WLKREE01 REDEFINES IO-AREA2.                                     
034000*        05  -COPY WDA201                                                 
034100     EJECT                                                                
034200     03  WLKREE11 REDEFINES IO-AREA2.                                     
034300*        05  -COPY WDA211                                                 
034400     EJECT                                                                
034500 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDA301'.          
034600 01  DLI-IO-WDA301.                                                       
034700*    03  -COPY WDA301  -PRE RETA-                                         
034800     EJECT                                                                
034900     SKIP3                                                                
035000 01  FILLER                     PIC X(16) VALUE 'WDGX4111-AREA'.          
035100 01  WL411101  -COPY WDGX4111                                             
035200     EJECT                                                                
035300 01  FILLER                     PIC X(16) VALUE 'WDGX4112-AREA'.          
035400 01  WL411111  -COPY WDGX4112                                             
035500                                                                          
035600     EJECT                                                                
035700 LINKAGE SECTION.                                                         
035800 01  MSG-PCB                     PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0009   -PRE DISP-                                             
036100     EJECT                                                                
036200 01  DISTRDOC-PCB                PIC X.                                   
036300     EJECT                                                                
036400 01  ATAB-PCB                    PIC X.                                   
036500     EJECT                                                                
036600                                                                          
036700*01  -COPY W0008  -PRE RETA-                                              
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000*01  -COPY W0008  -PRE RETG-                                              
037100     05  FILLER                  PIC X.                                   
037200     EJECT                                                                
037300*01  -COPY W0008  -PRE KREE-                                              
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600*01  -COPY W0008  -PRE ANSV-4113-                                         
037700     05  FILLER                  PIC X.                                   
037800     EJECT                                                                
037900*01  -COPY W0008  -PRE ANSV-4115-                                         
038000     05  FILLER                  PIC X.                                   
038100     EJECT                                                                
038200*01  -COPY W0008  -PRE ANSV-4117-                                         
038300     05  FILLER                  PIC X.                                   
038400     EJECT                                                                
038500*01  -COPY W0008  -PRE 4111-                                              
038600     05  FILLER                  PIC X.                                   
038700     EJECT                                                                
038800*01  -COPY W0008  -PRE SEQB-                                              
038900     05  FILLER                  PIC X.                                   
039000     EJECT                                                                
039100 01  KOM-KOMA-PCB                PIC X.                                   
039200     EJECT                                                                
039300*01  -COPY W0008  -PRE A3B1-                                              
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600                                                                          
039700 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB DISTRDOC-PCB                  
039800                           ATAB-PCB RETA-PCB RETG-PCB                     
039900                           KREE-PCB                                       
040000                           ANSV-4113-PCB                                  
040100                           ANSV-4115-PCB                                  
040200                           ANSV-4117-PCB                                  
040300                           4111-PCB                                       
040400                           SEQB-PCB KOM-KOMA-PCB A3B1-PCB.                
040500 MAIN SECTION.                                                            
040600                                                                          
040700     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB DISTRDOC-PCB                  
040800                           ATAB-PCB RETA-PCB RETG-PCB                     
040900                           KREE-PCB                                       
041000                           ANSV-4113-PCB                                  
041100                           ANSV-4115-PCB                                  
041200                           ANSV-4117-PCB                                  
041300                           4111-PCB                                       
041400                           SEQB-PCB KOM-KOMA-PCB A3B1-PCB.                
041500                                                                          
041600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
041700     IF SUB-KDRC = 0                                                      
041800       IF REQU-KDPGMACT = 'E' OR 'S' OR 'P'                               
041900         PERFORM A-INIT                                                   
042000         PERFORM B-KOLLA-NYCKLAR                                          
042100         IF NYCKLAR-OK                                                    
042200           PERFORM G-KOLLA-INPUT                                          
042300           IF REQU-KDPGMACT = 'E'                                         
042400             IF INDATA-OK                                                 
042500               IF SW-IDRT = 'N'                                           
042600                 PERFORM H-UPPDATERA-RADER                                
042700               ELSE                                                       
042800                 PERFORM I-UPPDATERA-SANDNING                             
042900               END-IF                                                     
043000             END-IF                                                       
043100           END-IF                                                         
043200           IF REQU-KDPGMACT = 'S'                                         
043300             IF INDATA-OK                                                 
043400                PERFORM F-LAES-VISA-INFO                                  
043500             END-IF                                                       
043600           END-IF                                                         
043700           IF REQU-KDPGMACT = 'P'                                         
043800             IF INDATA-OK                                                 
043900                PERFORM J-SKRIV-UT-SIDAN                                  
044000                PERFORM F-LAES-VISA-INFO                                  
044100             END-IF                                                       
044200           END-IF                                                         
044300         END-IF                                                           
044400       ELSE                                                               
044500         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
044600       END-IF                                                             
044700                                                                          
044800       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
044900       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
045000       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
045100       IF WS-IDMSG-ERROR NOT = SPACE                                      
045200         MOVE ALL '+'          TO RESP-WL0156O1(1:30)                     
045300         MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                        
045400         MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                       
045500         MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                         
045600         MOVE 001              TO RESP-IDRESVER                           
045700                                                                          
045800         IF REQU-KVRADER  NUMERIC                                         
045900           MOVE REQU-KVRADER   TO RESP-KVRADER                            
046000         ELSE                                                             
046100           MOVE ZERO           TO RESP-KVRADER                            
046200         END-IF                                                           
046300       END-IF                                                             
046400       IF REQU-KDPGMACT = 'P' AND WS-IDMSG-ERROR  = SPACE                 
046500         CONTINUE                                                         
046600       ELSE                                                               
046700         IF SUB-KDTRANS(1:6) = 'WLA156'                                   
046800           PERFORM S11-MSG-CONV                                           
046900         END-IF                                                           
047000         PERFORM S02-RETURN-RESPONSE                                      
047100       END-IF                                                             
047200     END-IF                                                               
047300                                                                          
047400     MOVE ZERO TO RETURN-CODE                                             
047500     GOBACK                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 A-INIT SECTION.                                                          
047900                                                                          
048000     MOVE ZERO TO SPAR-IDRTLOP-R31                                        
048100                                                                          
048200     MOVE LOW-VALUE     TO W-WDA3F1KY-MIN-X                               
048300                           W-WDA3BSEQ-MIN-X                               
048400                           W-WDA3B1-MIN-X                                 
048500     MOVE HIGH-VALUE    TO W-WDA3F1KY-MAX-X                               
048600                           W-WDA3BSEQ-MAX-X                               
048700                           W-WDA3B1-MAX-X                                 
048800                                                                          
048900     MOVE ALL '+'       TO RESP-AREA                                      
049000     MOVE SPACE         TO RESP-IDMSG-ERROR                               
049100                           RESP-IDMSG-INFO                                
049200                           RESP-IDELMT-ERROR                              
049300     MOVE '001'         TO RESP-IDRESVER                                  
049400                                                                          
049500     ACCEPT RET-TIKLOCK        FROM TIME                                  
049600     IF SUB-KDTRANS(1:6) = 'WLA156'                                       
049700       MOVE 001                  TO AUTH-KDCALL                           
049800       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
049900                                    REQU-WZ01REQ2                         
050000       IF AUTH-KDRC > 0                                                   
050100         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
050200         MOVE NOO                TO KEYS-SW                               
050300       END-IF                                                             
050400       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
050500                                 REQU-IDDC-KEY                            
050600       MOVE FUNCTION UPPER-CASE (REQU-IDRT-KEY) TO                        
050700                                 REQU-IDRT-KEY                            
050800       MOVE FUNCTION UPPER-CASE (REQU-IDRT-IN-KEY) TO                     
050900                                 REQU-IDRT-IN-KEY                         
051000       MOVE +1 TO INDX                                                    
051100       PERFORM UNTIL INDX > MAX-INDX                                      
051200        MOVE FUNCTION UPPER-CASE (REQU-TERETNOT(INDX)) TO                 
051300                                  REQU-TERETNOT(INDX)                     
051400        ADD +1 TO INDX                                                    
051500       END-PERFORM                                                        
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 B-KOLLA-NYCKLAR SECTION.                                                 
052000                                                                          
052100     MOVE JA TO NYCKLAR-SW                                                
052200                                                                          
052300     MOVE REQU-IDDC-KEY     TO RESP-IDDC-KEY                              
052400     MOVE REQU-IDDC-KEY     TO WS-IDDC                                    
052500                                                                          
052600     PERFORM BA-KOLLA-IDDISTR                                             
052700     IF REQU-KDPGMACT = 'E'                                               
052800       PERFORM BB-KOLLA-IDRT-IDRTLOP                                      
052900                                                                          
053000       IF SW-IDRT = 'J' AND SW-IDDISTR = 'J'                              
053100         MOVE NEJ                   TO NYCKLAR-SW                         
053200         MOVE '032'                 TO RESP-IDMSG-ERROR                   
053300       END-IF                                                             
053400                                                                          
053500       IF SW-IDRT = 'J' AND SW-IDRTLOP = 'N'                              
053600         MOVE NEJ                   TO NYCKLAR-SW                         
053700         MOVE 'IDRT'                TO RESP-IDELMT-ERROR                  
053800         MOVE '023'                 TO RESP-IDMSG-ERROR                   
053900       END-IF                                                             
054000                                                                          
054100       IF SW-IDRTLOP = 'J' AND SW-IDRT = 'N'                              
054200         MOVE NEJ                   TO NYCKLAR-SW                         
054300         MOVE 'IDRTLOP'             TO RESP-IDELMT-ERROR                  
054400         MOVE '023'                 TO RESP-IDMSG-ERROR                   
054500       END-IF                                                             
054600     ELSE                                                                 
054700       MOVE SPACE             TO RESP-IDRT-IN-KEY                         
054800       MOVE ZERO              TO RESP-IDRTLOP-IN-KEY                      
054900     END-IF                                                               
055000                                                                          
055100*    -- KONTROLL AV IDRT-KEY                                              
055200     IF REQU-IDRT-KEY = SPACE OR                                          
055300        REQU-IDRT-KEY = ALL '+' OR                                        
055400        REQU-IDRT-KEY = 'CDC' OR                                          
055500        REQU-IDRT-KEY = 'US1' OR                                          
055600        REQU-IDRT-KEY = 'US2'                                             
055700        MOVE NEJ             TO NYCKLAR-SW                                
055800        MOVE 'IDRT'          TO RESP-IDELMT-ERROR                         
055900        MOVE '023'           TO RESP-IDMSG-ERROR                          
056000     END-IF                                                               
056100                                                                          
056200     MOVE REQU-IDRT-KEY    TO W-IDRT-4111                                 
056300     PERFORM IMS-GU-WL411101                                              
056400     IF SEGMENT-SAKNAS                                                    
056500       MOVE NEJ            TO NYCKLAR-SW                                  
056600       MOVE 'IDRT'         TO RESP-IDELMT-ERROR                           
056700       MOVE '023'          TO RESP-IDMSG-ERROR                            
056800     END-IF                                                               
056900                                                                          
057000                                                                          
057100*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
057200*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
057300*    IF REQU-IDUSER = 'PHCA4G1'                                           
057400*       AND REQU-IDDC-KEY = '44'                                          
057500*      MOVE '54'           TO REQU-IDFTG-KEY                              
057600*    END-IF                                                               
057700*    END FIX                                                              
057800                                                                          
057900     IF REQU-IDFTG-KEY NOT NUMERIC                                        
058000       MOVE NEJ            TO NYCKLAR-SW                                  
058100       MOVE 'IDFTG'        TO RESP-IDELMT-ERROR                           
058200       MOVE '023'          TO RESP-IDMSG-ERROR                            
058300     END-IF                                                               
058400                                                                          
058500     IF NYCKLAR-OK                                                        
058600       IF REQU-IDDISTR-KEY NUMERIC                                        
058700         MOVE REQU-IDDISTR-KEY     TO RESP-IDDISTR-KEY                    
058800         INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE        
058900       END-IF                                                             
059000     END-IF                                                               
059100                                                                          
059200     .                                                                    
059300     EJECT                                                                
059400 BA-KOLLA-IDDISTR  SECTION.                                               
059500                                                                          
059600     IF REQU-IDDISTR-KEY = ALL '+'                                        
059700       MOVE NEJ                     TO SW-IDDISTR                         
059800     ELSE                                                                 
059900       IF REQU-IDDISTR-KEY    NUMERIC AND                                 
060000          REQU-IDDISTR-KEY    > ZERO                                      
060100          MOVE REQU-IDDISTR-KEY TO W-IDDISTR-ANM                          
060200                                   W-IDDISTR-MIN                          
060300                                   W-IDDISTR-MAX                          
060400                                   RESP-IDDISTR-KEY                       
060500          MOVE JA               TO SW-IDDISTR                             
060600       ELSE                                                               
060700          MOVE 'IDDISTR'      TO RESP-IDELMT-ERROR                        
060800          MOVE '023'          TO RESP-IDMSG-ERROR                         
060900          MOVE NEJ            TO NYCKLAR-SW                               
061000       END-IF                                                             
061100     END-IF                                                               
061200     .                                                                    
061300     EJECT                                                                
061400 BB-KOLLA-IDRT-IDRTLOP SECTION.                                           
061500                                                                          
061600     MOVE NEJ                     TO SW-IDRT                              
061700                                     SW-IDRTLOP                           
061800                                                                          
061900     IF REQU-IDRT-IN-KEY NOT = SPACE  AND                                 
062000        REQU-IDRT-IN-KEY NOT = ALL '+'                                    
062100       MOVE REQU-IDRT-IN-KEY TO RESP-IDRT-IN-KEY                          
062200                                W-IDRT-BSEQ-MIN                           
062300                                W-IDRT-BSEQ-MAX                           
062400       MOVE JA               TO SW-IDRT                                   
062500     END-IF                                                               
062600                                                                          
062700     IF REQU-IDRTLOP-IN-KEY    NOT = ALL '+'                              
062800       IF REQU-IDRTLOP-IN-KEY     NUMERIC AND                             
062900          REQU-IDRTLOP-IN-KEY  > ZERO                                     
063000         MOVE REQU-IDRTLOP-IN-KEY TO RESP-IDRTLOP-IN-KEY                  
063100                                     W-IDRTLOP-BSEQ-MIN                   
063200                                     W-IDRTLOP-BSEQ-MAX                   
063300         MOVE JA                  TO SW-IDRTLOP                           
063400       ELSE                                                               
063500         MOVE NEJ            TO NYCKLAR-SW                                
063600         MOVE 'IDRT-IDRTLOP-IN' TO RESP-IDELMT-ERROR                      
063700         MOVE '023'          TO RESP-IDMSG-ERROR                          
063800       END-IF                                                             
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 F-LAES-VISA-INFO SECTION.                                                
064300                                                                          
064400     MOVE +1                   TO INDX                                    
064500     PERFORM UNTIL INDX        > MAX-INDX                                 
064600                                                                          
064700       IF REQU-IDKUNDNR(INDX)  NOT = ALL '+'                              
064800         MOVE REQU-IDKUNDNR(INDX)  TO RESP-IDKUNDNR(INDX)                 
064900         MOVE REQU-IDRAPPNR(INDX)  TO RESP-IDRAPPNR(INDX)                 
065000         MOVE REQU-KVKOLLI (INDX)  TO RESP-KVKOLLI (INDX)                 
065100         MOVE REQU-TERETNOT(INDX)  TO RESP-TERETNOT(INDX)                 
065200                                                                          
065300         MOVE REQU-IDKUNDNR(INDX)  TO W-IDKUNDNR-ANM                      
065400         MOVE REQU-IDRAPPNR(INDX)  TO W-IDRAPPNR-ANM                      
065500                                                                          
065600         PERFORM IMS-GU-WLKREE01                                          
065700         IF SEGMENT-FINNS                                                 
065800            MOVE ANM-IDDC-RET      TO RESP-IDDC-RET(INDX)                 
065900         END-IF                                                           
066000       END-IF                                                             
066100                                                                          
066200       ADD +1                       TO INDX                               
066300     END-PERFORM                                                          
066400     .                                                                    
066500     EJECT                                                                
066600 G-KOLLA-INPUT SECTION.                                                   
066700                                                                          
066800     MOVE JA                     TO INDATA-SW                             
066900     MOVE SPACE                  TO W-IDDC-SANDNING                       
067000                                                                          
067100     IF SW-IDRT = 'N'                                                     
067200       IF REQU-INPUT = ALL '+'                                            
067300         IF REQU-KDPGMACT = 'E'                                           
067400           MOVE '014'                TO RESP-IDMSG-ERROR                  
067500         ELSE                                                             
067600           MOVE '027'                TO RESP-IDMSG-ERROR                  
067700         END-IF                                                           
067800         MOVE NEJ                TO INDATA-SW                             
067900       ELSE                                                               
068000         PERFORM GA-FORMELL-KONTROLL                                      
068100         IF INDATA-OK                                                     
068200            PERFORM GB-LOGISK-KONTROLL                                    
068300         END-IF                                                           
068400       END-IF                                                             
068500     ELSE                                                                 
068600*- KOLLA OM INMATAT SÄNDNINGSNR FINNS PÅ WDA3.                            
068700                                                                          
068800       MOVE REQU-IDDC-KEY                TO W-IDDC-BSEQ-MIN               
068900                                            W-IDDC-BSEQ-MAX               
069000       PERFORM IMS-GU-SEQB-WDA301                                         
069100                                                                          
069200       IF SEGMENT-FINNS                                                   
069300         IF RET-KDRETSTA = W-SND-SAENT                                    
069400           MOVE RET-IDDC                 TO W-IDDC-SANDNING               
069500                                            RESP-IDDC-RET-UT              
069600         ELSE                                                             
069700           MOVE NEJ                      TO INDATA-SW                     
069800         END-IF                                                           
069900       ELSE                                                               
070000         IF NDC-CN OR LDC-CN                                              
070100           MOVE REQU-IDRT-IN-KEY         TO W-IDRT-B1-MIN                 
070200                                            W-IDRT-B1-MAX                 
070300           MOVE LOW-VALUE                TO W-IDDC-B1-MIN                 
070400           MOVE HIGH-VALUE               TO W-IDDC-B1-MAX                 
070500           MOVE REQU-IDRTLOP-IN-KEY      TO W-IDRTLOP-B1                  
070600                                                                          
070700           PERFORM IMS-GU-WDA3B1                                          
070800                                                                          
070900           IF SEGMENT-FINNS                                               
071000             MOVE RET-SEQB-IDDC          TO W-IDDC                        
071100             MOVE RET-SEQB-DAREGDAT      TO W-DAREGDAT                    
071200             MOVE RET-SEQB-TIKLOCK       TO W-TIKLOCK                     
071300                                                                          
071400             PERFORM IMS-GU-WDA301                                        
071500                                                                          
071600             IF SEGMENT-FINNS                                             
071700*- MAN SKALL INTE KUNNA TA EMOT EN TRANSFER 2 GGR PÅ SAMMA RET.DC         
071800               IF RET-IDRT = REQU-IDRT-KEY                                
071900                 MOVE ERR-RAD-FINNS-REDAN TO RESP-IDMSG-ERROR             
072000                 MOVE NEJ                TO INDATA-SW                     
072100               ELSE                                                       
072200                 IF RET-KDRETSTA = W-SND-SAENT                            
072300                   MOVE RET-IDDC         TO W-IDDC-SANDNING               
072400                                            RESP-IDDC-RET-UT              
072500                 ELSE                                                     
072600                   MOVE NEJ              TO INDATA-SW                     
072700                 END-IF                                                   
072800               END-IF                                                     
072900             ELSE                                                         
073000               MOVE NEJ                  TO INDATA-SW                     
073100             END-IF                                                       
073200           ELSE                                                           
073300             MOVE NEJ                    TO INDATA-SW                     
073400           END-IF                                                         
073500         ELSE                                                             
073600           MOVE WS-CDC-SE                TO W-IDDC-BSEQ-MIN               
073700                                            W-IDDC-BSEQ-MAX               
073800           PERFORM IMS-GU-SEQB-WDA301                                     
073900           IF SEGMENT-FINNS                                               
074000             IF RET-KDRETSTA = W-SND-SAENT                                
074100               MOVE RET-IDDC             TO W-IDDC-SANDNING               
074200                                            RESP-IDDC-RET-UT              
074300             ELSE                                                         
074400               MOVE NEJ                  TO INDATA-SW                     
074500             END-IF                                                       
074600           ELSE                                                           
074700             MOVE NEJ                    TO INDATA-SW                     
074800           END-IF                                                         
074900         END-IF                                                           
075000       END-IF                                                             
075100       IF INDATA-FEL                                                      
075200         IF RESP-IDMSG-ERROR = SPACE                                      
075300           MOVE 'IDRT'         TO RESP-IDELMT-ERROR                       
075400           MOVE '023'          TO RESP-IDMSG-ERROR                        
075500         END-IF                                                           
075600       END-IF                                                             
075700     END-IF                                                               
075800                                                                          
075900     IF INDATA-OK                                                         
076000       PERFORM GC-KOLLA-IDANSTNR                                          
076100     END-IF                                                               
076200     .                                                                    
076300     EJECT                                                                
076400                                                                          
076500 GA-FORMELL-KONTROLL   SECTION.                                           
076600                                                                          
076700     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
076800       MOVE REQU-KVRADER                  TO WS-INDX-REC                  
076900                                             RESP-KVRADER                 
077000       MOVE NEJ                           TO WS-REC-LIMIT                 
077100       MOVE +1                            TO INDX                         
077200       PERFORM UNTIL INDX             > MAX-INDX OR REC-LIMIT             
077300                                                                          
077400         IF REQU-INPUT-RAD(INDX)      NOT = ALL '+'                       
077500                                                                          
077600           PERFORM GAA-KOLLA-IDKUNDNR                                     
077700           IF INDATA-OK                                                   
077800             PERFORM GAB-KOLLA-IDRAPPNR                                   
077900             IF INDATA-OK                                                 
078000               PERFORM GAC-KOLLA-KVKOLLI                                  
078100               IF INDATA-OK                                               
078200                 PERFORM GAD-KOLLA-RT-PAA-SIDA                            
078300               END-IF                                                     
078400             END-IF                                                       
078500           END-IF                                                         
078600         END-IF                                                           
078700                                                                          
078800         IF INDATA-FEL                                                    
078900           MOVE +14               TO INDX                                 
079000         ELSE                                                             
079100           MOVE ALL ' '           TO RESP-IDMSG-ERROR-LINE(INDX)          
079200         END-IF                                                           
079300         IF INDX = WS-INDX-REC                                            
079400           MOVE JA TO WS-REC-LIMIT                                        
079500         ELSE                                                             
079600           ADD +1                 TO INDX                                 
079700         END-IF                                                           
079800                                                                          
079900       END-PERFORM                                                        
080000     ELSE                                                                 
080100       MOVE NEJ           TO INDATA-SW                                    
080200       IF REQU-KVRADER = 0                                                
080300         MOVE 'KVRADER' TO RESP-IDELMT-ERROR                              
080400         MOVE '126'     TO RESP-IDMSG-ERROR                               
080500       ELSE                                                               
080600         MOVE 'KVRADER' TO RESP-IDELMT-ERROR                              
080700         MOVE '024'     TO RESP-IDMSG-ERROR                               
080800       END-IF                                                             
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200                                                                          
081300 GAA-KOLLA-IDKUNDNR     SECTION.                                          
081400                                                                          
081500     IF REQU-IDKUNDNR(INDX)         NOT = ALL '+'                         
081600        IF REQU-IDKUNDNR(INDX)      NOT NUMERIC                           
081700           MOVE NEJ                 TO INDATA-SW                          
081800           MOVE '024'               TO RESP-IDMSG-ERROR                   
081900                                    RESP-IDMSG-ERROR-LINE(INDX)           
082000           MOVE 'IDKUNDNR'          TO RESP-IDELMT-ERROR                  
082100        END-IF                                                            
082200     ELSE                                                                 
082300        MOVE NEJ                    TO INDATA-SW                          
082400        MOVE '024'                  TO RESP-IDMSG-ERROR                   
082500                                       RESP-IDMSG-ERROR-LINE(INDX)        
082600        MOVE 'IDKUNDNR'             TO RESP-IDELMT-ERROR                  
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 GAB-KOLLA-IDRAPPNR     SECTION.                                          
083100                                                                          
083200     IF REQU-IDRAPPNR(INDX)         NOT = ALL '+'                         
083300        IF REQU-IDRAPPNR(INDX)      NOT NUMERIC                           
083400           MOVE NEJ                 TO INDATA-SW                          
083500           MOVE '024'               TO RESP-IDMSG-ERROR                   
083600                                       RESP-IDMSG-ERROR-LINE(INDX)        
083700           MOVE 'IDRAPPNR'          TO RESP-IDELMT-ERROR                  
083800        END-IF                                                            
083900     ELSE                                                                 
084000        MOVE NEJ                    TO INDATA-SW                          
084100        MOVE '024'                  TO RESP-IDMSG-ERROR                   
084200                                       RESP-IDMSG-ERROR-LINE(INDX)        
084300        MOVE 'IDRAPPNR'             TO RESP-IDELMT-ERROR                  
084400     END-IF                                                               
084500                                                                          
084600     .                                                                    
084700     EJECT                                                                
084800 GAC-KOLLA-KVKOLLI      SECTION.                                          
084900                                                                          
085000     IF REQU-KVKOLLI(INDX)          NOT = ALL '+'                         
085100        IF REQU-KVKOLLI(INDX)       NOT NUMERIC                           
085200           MOVE NEJ                 TO INDATA-SW                          
085300           MOVE '024'               TO RESP-IDMSG-ERROR                   
085400                                       RESP-IDMSG-ERROR-LINE(INDX)        
085500           MOVE 'KVKOLLI'           TO RESP-IDELMT-ERROR                  
085600        END-IF                                                            
085700     ELSE                                                                 
085800        MOVE NEJ                    TO INDATA-SW                          
085900        MOVE '024'                  TO RESP-IDMSG-ERROR                   
086000                                       RESP-IDMSG-ERROR-LINE(INDX)        
086100        MOVE 'KVKOLLI'              TO RESP-IDELMT-ERROR                  
086200     END-IF                                                               
086300                                                                          
086400     .                                                                    
086500     EJECT                                                                
086600 GAD-KOLLA-RT-PAA-SIDA    SECTION.                                        
086700                                                                          
086800     COMPUTE INDX2                 =  INDX + 1                            
086900     PERFORM UNTIL INDX2 > MAX-INDX                                       
087000       IF REQU-IDRAPPNR(INDX2) NUMERIC                                    
087100         IF REQU-IDRAPPNR(INDX) = REQU-IDRAPPNR(INDX2)                    
087200           MOVE NEJ TO INDATA-SW                                          
087300           MOVE '030'              TO RESP-IDMSG-ERROR                    
087400                                      RESP-IDMSG-ERROR-LINE(INDX)         
087500           MOVE 'IDRAPPNR'         TO RESP-IDELMT-ERROR                   
087600           MOVE +15                TO INDX2                               
087700         ELSE                                                             
087800           ADD +1                  TO INDX2                               
087900         END-IF                                                           
088000       ELSE                                                               
088100         ADD +1                    TO INDX2                               
088200       END-IF                                                             
088300     END-PERFORM                                                          
088400     .                                                                    
088500     EJECT                                                                
088600 GB-LOGISK-KONTROLL   SECTION.                                            
088700                                                                          
088800     MOVE NEJ                        TO WS-REC-LIMIT                      
088900     MOVE +1                         TO INDX                              
089000     PERFORM UNTIL INDX              > MAX-INDX OR REC-LIMIT              
089100                                                                          
089200        IF REQU-IDKUNDNR(INDX)       NOT = ALL '+' AND                    
089300           REQU-IDRAPPNR(INDX)       NOT = ALL '+'                        
089400            MOVE REQU-IDKUNDNR(INDX) TO W-IDKUNDNR-ANM                    
089500                                           W-IDKUNDNR-MIN                 
089600                                           W-IDKUNDNR-MAX                 
089700            MOVE REQU-IDRAPPNR(INDX) TO W-IDRAPPNR-ANM                    
089800                                           W-IDRAPPNR-MIN                 
089900                                           W-IDRAPPNR-MAX                 
090000                                                                          
090100            PERFORM IMS-GU-WLKREE01                                       
090200                                                                          
090300                                                                          
090400            IF SEGMENT-SAKNAS                                OR           
090500              (SEGMENT-FINNS AND ANM-KDLEVANM NOT = '4')     OR           
090600              (SEGMENT-FINNS AND ANM-IDFTG                                
090700                                 NOT = REQU-IDFTG-KEY)       OR           
090800              (SEGMENT-FINNS AND LDC-CN AND                               
090900                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
091000              (SEGMENT-FINNS AND NDC-CN AND                               
091100                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
091200              (SEGMENT-FINNS AND NDC-NA AND                               
091300                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
091400              (SEGMENT-FINNS AND NDC-IN AND                               
091500                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
091600              (SEGMENT-FINNS AND NDC-KR AND                               
091700                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
091800              (SEGMENT-FINNS AND NDC-AE AND                               
091900                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
092000              (SEGMENT-FINNS AND NDC-MY AND                               
092100                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
092200              (SEGMENT-FINNS AND NDC-TR AND                               
092300                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
092400              (SEGMENT-FINNS AND NDC-TH AND                               
092500                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
092600              (SEGMENT-FINNS AND NDC-TW AND                               
092700                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
092800              (SEGMENT-FINNS AND NDC-ZA AND                               
092900                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
093000              (SEGMENT-FINNS AND NDC-MX AND                               
093100                                (ANM-IDDC-RET = WS-CDC-SE))  OR           
093200              (SEGMENT-FINNS AND NDC-BR AND                               
093300                                (ANM-IDDC-RET = WS-CDC-SE))               
093400                                                                          
093500               IF RESP-IDMSG-ERROR = SPACE                                
093600                 IF SEGMENT-SAKNAS                                        
093700                   MOVE 'IDLEVANM'           TO RESP-IDELMT-ERROR         
093800                   MOVE '025'                TO RESP-IDMSG-ERROR          
093900                               RESP-IDMSG-ERROR-LINE(INDX)                
094000                 ELSE                                                     
094100                   IF SEGMENT-FINNS AND ANM-KDLEVANM NOT = '4'            
094200                     MOVE 'KDLEVANM'         TO RESP-IDELMT-ERROR         
094300                     MOVE '023'              TO RESP-IDMSG-ERROR          
094400                               RESP-IDMSG-ERROR-LINE(INDX)                
094500                   ELSE                                                   
094600                     MOVE ERR-OTILL-UPD      TO RESP-IDMSG-ERROR          
094700                               RESP-IDMSG-ERROR-LINE(INDX)                
094800                   END-IF                                                 
094900                 END-IF                                                   
095000               END-IF                                                     
095100                                                                          
095200               MOVE NEJ               TO INDATA-SW                        
095300            ELSE                                                          
095400               MOVE ANM-IDDC-RET TO W-IDDC-MIN                            
095500                                    W-IDDC-MAX                            
095600                                                                          
095700               PERFORM IMS-GU-WLRETG01                                    
095800               IF SEGMENT-FINNS                                           
095900                 IF RESP-IDMSG-ERROR = SPACE                              
096000                   MOVE ERR-RAD-FINNS-REDAN TO RESP-IDMSG-ERROR           
096100                             RESP-IDMSG-ERROR-LINE(INDX)                  
096200                 END-IF                                                   
096300                 MOVE NEJ               TO INDATA-SW                      
096400               END-IF                                                     
096500            END-IF                                                        
096600                                                                          
096610            IF INDATA-OK                                                  
096700              IF NDC-US AND                                               
096800                 (ANM-IDDC-RET NOT = REQU-IDDC-KEY)                       
096900                                                                          
097000                 MOVE ERR-OTILL-UPD     TO RESP-IDMSG-ERROR               
097100                                       RESP-IDMSG-ERROR-LINE(INDX)        
097200                                                                          
097300                 MOVE ANM-IDDC-RET      TO RESP-IDDC-RET(INDX)            
097400                                                                          
097500                 MOVE NEJ               TO INDATA-SW                      
097600                                                                          
097700              END-IF                                                      
097710            END-IF                                                        
097800                                                                          
097900        END-IF                                                            
098000                                                                          
098100                                                                          
098200       IF INDX = WS-INDX-REC                                              
098300         MOVE JA TO WS-REC-LIMIT                                          
098400       ELSE                                                               
098500          ADD +1                       TO INDX                            
098600       END-IF                                                             
098700     END-PERFORM                                                          
098800     .                                                                    
098900     EJECT                                                                
099000 GC-KOLLA-IDANSTNR   SECTION.                                             
099100                                                                          
099200     MOVE NEJ                        TO SW-IDANSTNR                       
099300                                                                          
099400     IF REQU-IDANSTNR                 NOT = ALL '+'                       
099500        IF REQU-IDANSTNR NUMERIC                                          
099600           MOVE JA                   TO SW-IDANSTNR                       
099700           MOVE REQU-IDANSTNR        TO RESP-IDANSTNR                     
099800        ELSE                                                              
099900           MOVE NEJ                  TO INDATA-SW                         
100000           MOVE 'IDANSTNR'           TO RESP-IDELMT-ERROR                 
100100           MOVE '024'                TO RESP-IDMSG-ERROR                  
100200        END-IF                                                            
100300     ELSE                                                                 
100400        MOVE NEJ                     TO INDATA-SW                         
100500        MOVE '023'                   TO RESP-IDMSG-ERROR                  
100600        MOVE 'IDANSTNR'              TO RESP-IDELMT-ERROR                 
100700     END-IF                                                               
100800                                                                          
100900     .                                                                    
101000     EJECT                                                                
101100 H-UPPDATERA-RADER SECTION.                                               
101200                                                                          
101300     MOVE NEJ                  TO WS-REC-LIMIT                            
101400     MOVE NEJ                  TO SKAPA-R31-SW                            
101500     MOVE NEJ                  TO W-UPDATE-SW                             
101600     MOVE +1                   TO INDX                                    
101700     PERFORM UNTIL INDX        > MAX-INDX OR REC-LIMIT                    
101800                                                                          
101900       IF REQU-IDKUNDNR(INDX)  NOT = ALL '+'                              
102000         PERFORM HA-SKAPA-WLRETA01                                        
102100         IF W-IDDC-RET = REQU-IDDC-KEY                                    
102200           MOVE JA TO  SKAPA-R31-SW                                       
102300         END-IF                                                           
102400         MOVE JA               TO W-UPDATE-SW                             
102500       END-IF                                                             
102600                                                                          
102700       IF INDX = WS-INDX-REC                                              
102800          MOVE JA TO WS-REC-LIMIT                                         
102900       ELSE                                                               
103000          ADD +1                       TO INDX                            
103100       END-IF                                                             
103200                                                                          
103300     END-PERFORM                                                          
103400                                                                          
103500     IF SKAPA-R31                                                         
103600       PERFORM HB-SKAPA-R31                                               
103700     END-IF                                                               
103800                                                                          
103900     IF W-UPDATE-OK                                                       
104000       MOVE INF-UPDATE-DONE      TO RESP-IDMSG-INFO                       
104100       MOVE SPACE                TO RESP-INPUT                            
104200     END-IF                                                               
104300     .                                                                    
104400     EJECT                                                                
104500                                                                          
104600 HA-SKAPA-WLRETA01 SECTION.                                               
104700                                                                          
104800     PERFORM HAA-HAEMTA-LEV-ANM-UPPG                                      
104900     PERFORM HAB-HAEMTA-ANSVARIG                                          
105000                                                                          
105100     MOVE W-IDDC-RET           TO RET-IDDC                                
105200     MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                     
105300     ADD +1                    TO RET-TIKLOCK                             
105400                                                                          
105500     MOVE REQU-IDDISTR-KEY     TO RET-IDDISTR                             
105600     MOVE REQU-IDKUNDNR(INDX)  TO RET-IDKUNDNR                            
105700     MOVE REQU-IDRAPPNR(INDX)  TO RET-IDRAPPNR                            
105800     MOVE SPACE                TO RET-ADINLOMR                            
105900                                  RET-ADINLOMR-LOSS                       
106000                                  RET-ADINLOMR-MOT                        
106100     MOVE W-FLFARLIG           TO RET-FLFARLIG                            
106200                                                                          
106300     MOVE ZERO                 TO RET-IDANSTNR-LOSS                       
106400                                                                          
106500***.... NYTT FÄLT FÖR ANST.NR-MOT FÖR R31.                                
106600     MOVE REQU-IDANSTNR        TO RET-IDANSTNR-MOT                        
106700                                                                          
106800     MOVE SPACE                TO RET-IDFRASED-AAF                        
106900     MOVE SPACE                TO RET-IDFRASED-CDC                        
107000     MOVE REQU-IDRT-KEY        TO RET-IDRT                                
107100                                                                          
107200     IF W-IDDC-RET = REQU-IDDC-KEY                                        
107300       MOVE 00001              TO RET-IDKOLLI                             
107400       IF SPAR-IDRTLOP-R31 = ZERO                                         
107500         PERFORM HAC-TA-UT-IDRTLOP                                        
107600       END-IF                                                             
107700       MOVE SPAR-IDRTLOP-R31     TO RET-IDRTLOP                           
107800       MOVE W-IDPERSON           TO RET-IDPERSON                          
107900       MOVE W-KDARBTYP           TO RET-KDARBTYP                          
108000       MOVE W-SND-SAENT          TO RET-KDRETSTA                          
108100       MOVE W-KLI-SAENT          TO RET-KDKOLSTA                          
108200       MOVE +1                   TO RET-KVKOLLI-MOT                       
108300       MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DASNDDAT                   
108400     ELSE                                                                 
108500       MOVE ZERO                 TO RET-IDKOLLI                           
108600       MOVE ZERO                 TO RET-IDRTLOP                           
108700       MOVE W-IDPERSON           TO RET-IDPERSON                          
108800       MOVE W-KDARBTYP           TO RET-KDARBTYP                          
108900       MOVE W-SND-REG            TO RET-KDRETSTA                          
109000       MOVE ZERO                 TO RET-KDKOLSTA                          
109100       MOVE ZERO                 TO RET-KVKOLLI-MOT                       
109200       MOVE ZERO                 TO RET-DASNDDAT                          
109300     END-IF                                                               
109400                                                                          
109500     MOVE REQU-KVKOLLI(INDX)   TO RET-KVKOLLI-AAF                         
109600     MOVE ZERO                 TO RET-KVKOLLI-LOSS                        
109700     MOVE W-KVRADER            TO RET-KVRADER                             
109800     IF REQU-TERETNOT(INDX) = ALL '+'                                     
109900       MOVE SPACE                TO RET-TERETNOT                          
110000     ELSE                                                                 
110100       MOVE REQU-TERETNOT(INDX)  TO RET-TERETNOT                          
110200     END-IF                                                               
110300     MOVE ZERO                 TO RET-TIINLMOT                            
110400                                  RET-TIKLAR                              
110500                                  RET-TILOSSN                             
110600                                  RET-DARETANK                            
110700                                  RET-TIREGDAT-TRRT                       
110800                                  RET-TISNDDAT-TRRT                       
110900     MOVE SPACE                TO RET-IDRT-TRANSIT                        
111000     MOVE W-FLBUYBAC           TO RET-FLBUYBAC                            
111100                                                                          
111200     PERFORM IMS-ISRT-WLRETA01                                            
111300     PERFORM UNTIL SEGMENT-FINNS                                          
111400       MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                   
111500       ACCEPT RET-TIKLOCK        FROM TIME                                
111600                                                                          
111700       PERFORM IMS-ISRT-WLRETA01                                          
111800     END-PERFORM                                                          
111900     .                                                                    
112000     EJECT                                                                
112100                                                                          
112200 HAA-HAEMTA-LEV-ANM-UPPG SECTION.                                         
112300                                                                          
112400     MOVE REQU-IDKUNDNR(INDX)  TO W-IDKUNDNR-ANM                          
112500     MOVE REQU-IDRAPPNR(INDX)  TO W-IDRAPPNR-ANM                          
112600     MOVE ZERO                 TO W-KDANMORS                              
112700     MOVE NEJ                  TO W-FLBUYBAC                              
112800                                                                          
112900     PERFORM IMS-GU-WLKREE01                                              
113000     IF SEGMENT-FINNS                                                     
113100        MOVE ANM-FLFARLIG      TO W-FLFARLIG                              
113200        MOVE ANM-KVRADER-RT    TO W-KVRADER                               
113300        MOVE ANM-IDDC-RET      TO W-IDDC-RET                              
113400                                                                          
113500        PERFORM IMS-GNP-WLKREE11                                          
113600        IF SEGMENT-FINNS                                                  
113700          MOVE LEV-KDANMORS    TO W-KDANMORS                              
113800          IF KDANMORS-BUYBAC-98                                           
113900            MOVE JA            TO W-FLBUYBAC                              
114000          END-IF                                                          
114100        END-IF                                                            
114200     END-IF                                                               
114300                                                                          
114400     .                                                                    
114500     EJECT                                                                
114600                                                                          
114700 HAB-HAEMTA-ANSVARIG SECTION.                                             
114800                                                                          
114900     MOVE 3                    TO ANSV-KDCALL                             
115000     MOVE REQU-IDDISTR-KEY     TO ANSV-IDDISTR                            
115100     MOVE REQU-IDFTG-KEY       TO ANSV-IDFTG                              
115200     MOVE REQU-IDKUNDNR(INDX)  TO ANSV-IDKUNDNR                           
115300     MOVE W-IDDC-RET           TO ANSV-IDDC                               
115400     MOVE W-KDANMORS           TO ANSV-KDANMORS                           
115500     MOVE ZERO                 TO ANSV-KDORDKL                            
115600                                  ANSV-ADLAGOMR                           
115700                                                                          
115800     CALL W418ANSV USING ANSV-W418ANSV ANSV-4113-PCB                      
115900                                       ANSV-4115-PCB                      
116000                                       ANSV-4117-PCB                      
116100                                                                          
116200     IF ANSV-OK                                                           
116300        MOVE ANSV-KDARBTYP     TO W-KDARBTYP                              
116400        MOVE ANSV-IDPERSON     TO W-IDPERSON                              
116500     ELSE                                                                 
116600        MOVE 'RET'             TO W-KDARBTYP                              
116700        MOVE 99                TO W-IDPERSON                              
116800     END-IF                                                               
116900                                                                          
117000*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
117100*    RETURNS FROM CA (FTG=54) TO (DC=44, FTG=53)                          
117200*    IF REQU-IDUSER = 'PHCA4G1'                                           
117300*       AND REQU-IDDC-KEY = '44'                                          
117400*      MOVE 440                TO W-IDPERSON                              
117500*    END-IF                                                               
117600*    END FIX                                                              
117700     .                                                                    
117800     EJECT                                                                
117900 HAC-TA-UT-IDRTLOP SECTION.                                               
118000                                                                          
118100     PERFORM IMS-GHNP-WL411111                                            
118200     COMPUTE 4112-IDRTLOP = 4112-IDRTLOP + 001                            
118300     IF 4112-IDRTLOP = ZERO                                               
118400       ADD +001 TO 4112-IDRTLOP                                           
118500     END-IF                                                               
118600                                                                          
118700*- KOLLA OM NYA SÄNDNINGSNR FINNS PÅ WDA3 REDAN.                          
118800     MOVE REQU-IDRT-KEY               TO W-IDRT-B1-MIN                    
118900                                         W-IDRT-B1-MAX                    
119000     MOVE W-IDDC-RET                  TO W-IDDC-B1-MIN                    
119100                                         W-IDDC-B1-MAX                    
119200     MOVE 4112-IDRTLOP                TO W-IDRTLOP-B1-MIN                 
119300                                         W-IDRTLOP-B1-MAX                 
119400     MOVE ZERO TO W-IDRTLOP-RAK                                           
119500                                                                          
119600     PERFORM IMS-GU-WDA3B1-DC                                             
119700     ADD 1 TO W-IDRTLOP-RAK                                               
119800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
119900                   W-IDRTLOP-RAK > 999                                    
120000       COMPUTE 4112-IDRTLOP = 4112-IDRTLOP + 001                          
120100       IF 4112-IDRTLOP = ZERO                                             
120200         ADD +001 TO 4112-IDRTLOP                                         
120300       END-IF                                                             
120400                                                                          
120500       MOVE 4112-IDRTLOP              TO W-IDRTLOP-B1-MIN                 
120600                                         W-IDRTLOP-B1-MAX                 
120700       IF 4112-IDRTLOP = 1                                                
120800         PERFORM IMS-GU-WDA3B1-DC                                         
120900       ELSE                                                               
121000        PERFORM  IMS-GN-WDA3B1-DC                                         
121100       END-IF                                                             
121200       ADD 1 TO W-IDRTLOP-RAK                                             
121300     END-PERFORM                                                          
121400                                                                          
121500     IF W-IDRTLOP-RAK > 999                                               
121600       STRING 'ALL IDRTLOP IS USED FOR ' W-IDDC-RET                       
121700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
121800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
121900     ELSE                                                                 
122000       PERFORM IMS-REPL-WL411111                                          
122100     END-IF                                                               
122200                                                                          
122300     MOVE 4112-IDRTLOP    TO SPAR-IDRTLOP-R31                             
122400                             RESP-IDRTLOP                                 
122500                                                                          
122600     MOVE REQU-IDRT-KEY   TO RESP-IDRT                                    
122700     MOVE W-IDDC-RET      TO RESP-IDDC-RET-UT                             
122800     .                                                                    
122900     EJECT                                                                
123000 HB-SKAPA-R31 SECTION.                                                    
123100                                                                          
123200     ACCEPT DAGENS-DATUM FROM DATE                                        
123300                                                                          
123400     MOVE +1                            TO 4792-INDX                      
123500                                                                          
123600     MOVE REQU-IDDC-KEY                 TO W-IDDC-BSEQ-MIN                
123700                                           W-IDDC-BSEQ-MAX                
123800     IF SW-IDRT = 'N'                                                     
123900       MOVE REQU-IDRT-KEY               TO W-IDRT-BSEQ-MIN                
124000                                           W-IDRT-BSEQ-MAX                
124100       MOVE SPAR-IDRTLOP-R31            TO W-IDRTLOP-BSEQ-MIN             
124200                                           W-IDRTLOP-BSEQ-MAX             
124300     ELSE                                                                 
124400       MOVE REQU-IDRT-IN-KEY            TO W-IDRT-BSEQ-MIN                
124500                                           W-IDRT-BSEQ-MAX                
124600       MOVE REQU-IDRTLOP-IN-KEY         TO W-IDRTLOP-BSEQ-MIN             
124700                                           W-IDRTLOP-BSEQ-MAX             
124800     END-IF                                                               
124900                                                                          
125000     PERFORM IMS-GHU-SEQB-WDA301                                          
125100                                                                          
125200     IF SEGMENT-FINNS                                                     
125300       PERFORM UNTIL SEGMENT-SAKNAS                                       
125400         PERFORM S03-FYLL-R31-MID                                         
125500                                                                          
125600           MOVE W-SND-MOT            TO RET-KDRETSTA                      
125700           MOVE W-KLI-MOT            TO RET-KDKOLSTA                      
125800           MOVE DAGENS-DATUM         TO RET-TIINLMOT                      
125900           IF RET-DARETANK =  ZERO                                        
126000              MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DARETANK            
126100           END-IF                                                         
126200                                                                          
126300           PERFORM IMS-REPL-SEQB-WDA301                                   
126400                                                                          
126500         PERFORM IMS-GHN-SEQB-WDA301                                      
126600       END-PERFORM                                                        
126700     END-IF                                                               
126800                                                                          
126900     IF 4792-INDX >  +1                                                   
127000        PERFORM S04-STARTA-R31-RAPPORTERING                               
127100     END-IF                                                               
127200                                                                          
127300     .                                                                    
127400     EJECT                                                                
127500 I-UPPDATERA-SANDNING SECTION.                                            
127600                                                                          
127700     MOVE NEJ                  TO W-UPDATE-SW                             
127800                                                                          
127900     IF W-IDDC-SANDNING = REQU-IDDC-KEY                                   
128000       PERFORM HB-SKAPA-R31                                               
128100       MOVE JA               TO W-UPDATE-SW                               
128200     ELSE                                                                 
128300       ACCEPT DAGENS-DATUM FROM DATE                                      
128400       MOVE REQU-IDRT-IN-KEY             TO W-IDRT-BSEQ-MIN               
128500                                            W-IDRT-BSEQ-MAX               
128600       MOVE W-IDDC-SANDNING              TO W-IDDC-BSEQ-MIN               
128700                                            W-IDDC-BSEQ-MAX               
128800       MOVE REQU-IDRTLOP-IN-KEY          TO W-IDRTLOP-BSEQ-MIN            
128900                                            W-IDRTLOP-BSEQ-MAX            
129000       PERFORM IMS-GHU-SEQB-WDA301                                        
129100                                                                          
129200       IF SEGMENT-FINNS                                                   
129300         MOVE JA               TO W-UPDATE-SW                             
129400         PERFORM UNTIL SEGMENT-SAKNAS                                     
129500                                                                          
129600             MOVE W-SND-TRANSIT        TO RET-KDRETSTA                    
129700             MOVE DAGENS-DATUM         TO RET-TIREGDAT-TRRT               
129800             MOVE REQU-IDRT-KEY        TO RET-IDRT-TRANSIT                
129900                                                                          
130000             PERFORM IMS-REPL-SEQB-WDA301                                 
130100                                                                          
130200           PERFORM IMS-GHN-SEQB-WDA301                                    
130300         END-PERFORM                                                      
130400       END-IF                                                             
130500     END-IF                                                               
130600                                                                          
130700     IF W-UPDATE-OK                                                       
130800       MOVE INF-UPDATE-DONE      TO RESP-IDMSG-INFO                       
130900     ELSE                                                                 
131000       MOVE INF-UPDATE-NOT-DONE  TO RESP-IDMSG-INFO                       
131100     END-IF                                                               
131200     .                                                                    
131300     EJECT                                                                
131400 J-SKRIV-UT-SIDAN   SECTION.                                              
131500                                                                          
131600     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
131700                                                                          
131800     PERFORM S05-OPEN-DAP-SEND                                            
131900     MOVE 001             TO HDR-REQU-IDMSGVER                            
132000     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
132100     MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                              
132200                                                                          
132300     MOVE 'RET-REC-LIST'      TO HDR-IDOUTTYPE                            
132400     MOVE SPACE               TO HDR-IDOUTREC                             
132500     MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                        
132600     MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                        
132700     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
132800     PERFORM S06-PUT-DAP-HEADER                                           
132900                                                                          
133000     MOVE +1                   TO INDX                                    
133100     PERFORM UNTIL INDX        > MAX-INDX                                 
133200                                                                          
133300       IF REQU-IDKUNDNR(INDX)  NOT = ALL '+'                              
133400         MOVE 'LINE'        TO DOC-IDAFPRCD                               
133500         MOVE REQU-IDDC-KEY TO DOC-IDDC                                   
133600         MOVE REQU-IDDISTR-KEY     TO DOC-IDDISTR                         
133700         MOVE REQU-IDKUNDNR(INDX)  TO DOC-IDKUNDNR                        
133800         MOVE REQU-IDRAPPNR(INDX)  TO DOC-IDRAPPNR                        
133900         MOVE REQU-KVKOLLI (INDX)  TO DOC-KVKOLLI                         
134000         MOVE REQU-TERETNOT(INDX)  TO DOC-TERETNOT                        
134100                                                                          
134200         MOVE REQU-IDKUNDNR(INDX)  TO W-IDKUNDNR-ANM                      
134300         MOVE REQU-IDRAPPNR(INDX)  TO W-IDRAPPNR-ANM                      
134400                                                                          
134500         PERFORM IMS-GU-WLKREE01                                          
134600         IF SEGMENT-FINNS                                                 
134700            MOVE ANM-IDDC-RET      TO DOC-IDDC-RET                        
134800         END-IF                                                           
134900                                                                          
135000         PERFORM S07-PUT-DOC                                              
135100       END-IF                                                             
135200                                                                          
135300       ADD +1                      TO INDX                                
135400     END-PERFORM                                                          
135500                                                                          
135600     PERFORM S08-CLOSE-DAP-SEND                                           
135700     .                                                                    
135800     EJECT                                                                
135900*    --- DISPATCHER SECTIONS                                              
136000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
136100                                                                          
136200     MOVE 'GETARG'               TO SUB-KDFUNC                            
136300     MOVE 'CARPARTS.LDC.RECEIVINGRETURNS'    TO SUB-ADDISPABS             
136400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
136500                                                                          
136600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
136700                                                                          
136800     IF SUB-KDRC > 0                                                      
136900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
137000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
137100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
137200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
137300     END-IF                                                               
137400     .                                                                    
137500     SKIP3                                                                
137600 S02-RETURN-RESPONSE SECTION.                                             
137700                                                                          
137800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
137900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
138000                                                                          
138100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
138200                                                                          
138300     IF SUB-KDRC > 0                                                      
138400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
138500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
138600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
138700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
138800     END-IF                                                               
138900     .                                                                    
139000     EJECT                                                                
139100 S03-FYLL-R31-MID                SECTION.                                 
139200                                                                          
139300     MOVE RET-IDDC                      TO                                
139400                                   MOD4792-MID-IDDC                       
139500     MOVE RET-DAREGDAT (3:6)            TO                                
139600                                   MOD4792-MID-TIREGDAT(4792-INDX)        
139700     MOVE RET-TIKLOCK                   TO                                
139800                                   MOD4792-MID-TIKLOCK (4792-INDX)        
139900     ADD +1                             TO 4792-INDX                      
140000                                                                          
140100     IF 4792-INDX >  4792-MAX-INDX                                        
140200        PERFORM S04-STARTA-R31-RAPPORTERING                               
140300        MOVE +1                         TO 4792-INDX                      
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700                                                                          
140800 S04-STARTA-R31-RAPPORTERING     SECTION.                                 
140900                                                                          
141000     ACCEPT DAGENS-DATUM FROM DATE                                        
141100     ACCEPT DAGENS-TID   FROM TIME                                        
141200                                                                          
141300     MOVE SPACE                         TO MSG-KOM-WMSGKOM                
141400     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
141500     MOVE LOW-VALUE                     TO MSG-KOM-KDZ1                   
141600     MOVE LOW-VALUE                     TO MSG-KOM-KDZ2                   
141700     MOVE SPACE                         TO MSG-KOM-KDTRANS                
141800     MOVE 'W4I79201'                    TO MSG-KOM-IDCPYTXT               
141900     MOVE 'INLEVRET'                    TO MSG-KOM-IDSNDNOD               
142000     MOVE 'WL015600'                    TO MSG-KOM-IDSNDJOB               
142100     MOVE DAGENS-DATUM                  TO MSG-KOM-TIREGDAT               
142200     MOVE DAGENS-TID                    TO MSG-KOM-TIKLOCK                
142300     MOVE SPACE                         TO MSG-KOM-IDMFSMED               
142400                                                                          
142500     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
142600                                  LENGTH OF MOD4792-MID-W4I79201          
142700                                                                          
142800     MOVE 'W4T792X '                    TO P-TO-P-MSG-KDTRANS             
142900     MOVE 'L156'                        TO P-TO-P-MSG-IDTRANS             
143000     MOVE '2'                           TO P-TO-P-MSG-KDMFSFOR            
143100                                                                          
143200     COMPUTE MOD4792-MID-KVPOST  = 4792-INDX - 1                          
143300                                                                          
143400     MOVE MOD4792-MID-W4I79201          TO P-TO-P-MSG-INDATA              
143500                                                                          
143600     CALL W006KOM USING MSG-PCB                                           
143700                        DISP-PCB                                          
143800                        KOM-KOMA-PCB                                      
143900                        MSG-KOM-WMSGKOM                                   
144000                        P-TO-P-MSG-IO-AREA-SNUF                           
144100                                                                          
144200     .                                                                    
144300     EJECT                                                                
144400 S05-OPEN-DAP-SEND SECTION.                                               
144500*    MOVE 'S05-OPEN-DAP-S' TO CURR-SECTION                                
144600                                                                          
144700     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
144800     MOVE 'OPEN'                     TO SEND-KDFUNC                       
144900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
145000                         SEND-OPEN-AREA                                   
145100     IF SEND-KDRC > ZERO                                                  
145200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
145300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
145400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
145500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
145600     END-IF                                                               
145700     .                                                                    
145800     EJECT                                                                
145900                                                                          
146000 S06-PUT-DAP-HEADER SECTION.                                              
146100*    MOVE 'S06-PUT-DAP-HE' TO CURR-SECTION                                
146200                                                                          
146300     MOVE 'PUT'                           TO SEND-KDFUNC                  
146400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
146500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
146600                         SEND-KVDLEN                                      
146700                         HDR-AREA                                         
146800     IF SEND-KDRC > ZERO                                                  
146900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
147000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
147100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
147200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
147300     END-IF                                                               
147400     .                                                                    
147500 S07-PUT-DOC      SECTION.                                                
147600*    MOVE 'S07-PUT-DOC ' TO CURR-SECTION                                  
147700                                                                          
147800     MOVE 'PUT'                           TO SEND-KDFUNC                  
147900     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
148000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
148100                         SEND-KVDLEN                                      
148200                         DOC-AREA                                         
148300     IF SEND-KDRC > ZERO                                                  
148400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
148500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
148600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
148700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
148800     END-IF                                                               
148900     .                                                                    
149000     EJECT                                                                
149100 S08-CLOSE-DAP-SEND SECTION.                                              
149200*    MOVE 'S08-CLOSE-DAP-' TO CURR-SECTION                                
149300                                                                          
149400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
149500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
149600                                                                          
149700     IF SEND-KDRC > 0                                                     
149800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
149900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
150000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
150100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
150200     END-IF                                                               
150300     .                                                                    
150400     EJECT                                                                
150500                                                                          
150600* --- IMS SEKTIONER ---                                                   
150700     SKIP3                                                                
150800 S11-MSG-CONV SECTION.                                                    
150900     MOVE SPACES                  TO RESP-MESSAGES (1)                    
151000                                     RESP-MESSAGES (2)                    
151100     MOVE 1                       TO MSG-IX                               
151200*    REQUEST OK                                                           
151300     MOVE 200                     TO RESP-KDSTATUS-API                    
151400     IF RESP-IDMSG-INFO > SPACE                                           
151500       MOVE SPACES                TO MSG-CONV-AREA                        
151600       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
151700       CALL WMSGCONV           USING MSG-CONV-AREA                        
151800       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
151900       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
152000       ADD 1                      TO MSG-IX                               
152100     END-IF                                                               
152200     IF RESP-IDMSG-ERROR > SPACE                                          
152300*      BAD REQUEST                                                        
152400       MOVE 400                   TO RESP-KDSTATUS-API                    
152500       MOVE SPACES                TO MSG-CONV-AREA                        
152600       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
152700       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
152800       CALL WMSGCONV           USING MSG-CONV-AREA                        
152900       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
153000       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
153100     END-IF                                                               
153200     MOVE +1 TO INDX                                                      
153300     PERFORM UNTIL INDX > RESP-KVRADER                                    
153400      IF RESP-IDMSG-ERROR-LINE(INDX) NOT = SPACES                         
153500       MOVE SPACES                TO MSG-CONV-AREA                        
153600       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
153700       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
153800       CALL WMSGCONV           USING MSG-CONV-AREA                        
153900       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE-LINE(INDX)              
154000      END-IF                                                              
154100      ADD +1 TO INDX                                                      
154200     END-PERFORM                                                          
154300     .                                                                    
154400 IMS-GU-WDA3B1   SECTION.                                                 
154500                                                                          
154600     STRING 'WDA3B1  (WDA3B1KY>=' W-WDA3B1-MIN-X                          
154700                    '&WDA3B1KY<=' W-WDA3B1-MAX-X                          
154800                    '&IDRTLOP  =' W-IDRTLOP-B1-X ')'                      
154900          DELIMITED BY SIZE INTO SSA1                                     
155000     MOVE '  GE' TO GODK-STATUSKODER                                      
155100     CALL CBLTDLI USING GU A3B1-PCB DLI-IO-WDA3B1 SSA1                    
155200     MOVE A3B1-STATUS-CODE TO STATUS-WS                                   
155300     PERFORM IMS-STATUSKONTROLL                                           
155400     .                                                                    
155500     SKIP3                                                                
155600 IMS-GU-WDA3B1-DC SECTION.                                                
155700                                                                          
155800     STRING 'WDA3B1  (WDA3B1KY>=' W-WDA3B1-MIN-X                          
155900                    '&WDA3B1KY<=' W-WDA3B1-MAX-X ')'                      
156000          DELIMITED BY SIZE INTO SSA1                                     
156100     MOVE '  GE' TO GODK-STATUSKODER                                      
156200     CALL CBLTDLI USING GU A3B1-PCB DLI-IO-WDA3B1 SSA1                    
156300     MOVE A3B1-STATUS-CODE TO STATUS-WS                                   
156400     PERFORM IMS-STATUSKONTROLL                                           
156500     .                                                                    
156600     SKIP3                                                                
156700 IMS-GN-WDA3B1-DC SECTION.                                                
156800                                                                          
156900     STRING 'WDA3B1  (WDA3B1KY>=' W-WDA3B1-MIN-X                          
157000                    '&WDA3B1KY<=' W-WDA3B1-MAX-X ')'                      
157100          DELIMITED BY SIZE INTO SSA1                                     
157200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
157300     CALL CBLTDLI USING GN A3B1-PCB DLI-IO-WDA3B1 SSA1                    
157400     MOVE A3B1-STATUS-CODE TO STATUS-WS                                   
157500     PERFORM IMS-STATUSKONTROLL                                           
157600     .                                                                    
157700     SKIP3                                                                
157800 IMS-GU-WLRETG01 SECTION.                                                 
157900                                                                          
158000     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
158100                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
158200          DELIMITED BY SIZE INTO SSA1                                     
158300     MOVE '  GE' TO GODK-STATUSKODER                                      
158400     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA2 SSA1                     
158500     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
158600     PERFORM IMS-STATUSKONTROLL                                           
158700     .                                                                    
158800     SKIP3                                                                
158900 IMS-ISRT-WLRETA01    SECTION.                                            
159000                                                                          
159100     MOVE 'WLRETA01 ' TO SSA1                                             
159200     MOVE '  II' TO GODK-STATUSKODER                                      
159300     CALL CBLTDLI USING ISRT RETA-PCB DLI-IO-AREA SSA1                    
159400     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
159500     PERFORM IMS-STATUSKONTROLL                                           
159600     .                                                                    
159700     EJECT                                                                
159800 IMS-GU-WLKREE01    SECTION.                                              
159900                                                                          
160000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
160100          DELIMITED BY SIZE INTO SSA1                                     
160200     MOVE '  GE' TO GODK-STATUSKODER                                      
160300     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA2 SSA1                     
160400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
160500     PERFORM IMS-STATUSKONTROLL                                           
160600     .                                                                    
160700     EJECT                                                                
160800 IMS-GNP-WLKREE11 SECTION.                                                
160900                                                                          
161000     MOVE 'WLKREE11 ' TO SSA1                                             
161100     MOVE '  GE' TO GODK-STATUSKODER                                      
161200     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1                    
161300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
161400     PERFORM IMS-STATUSKONTROLL                                           
161500     .                                                                    
161600     EJECT                                                                
161700 IMS-GU-WL411101  SECTION.                                                
161800                                                                          
161900     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-X ')'                         
162000                      DELIMITED BY SIZE INTO SSA1                         
162100     MOVE '  GE' TO GODK-STATUSKODER                                      
162200     CALL CBLTDLI USING GU 4111-PCB WL411101 SSA1                         
162300     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
162400     PERFORM IMS-STATUSKONTROLL                                           
162500     .                                                                    
162600                                                                          
162700 IMS-GHNP-WL411111  SECTION.                                              
162800                                                                          
162900     MOVE  'WL411111*F' TO SSA1                                           
163000     MOVE '    ' TO GODK-STATUSKODER                                      
163100     CALL CBLTDLI USING GHNP 4111-PCB WL411111 SSA1                       
163200     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
163300     PERFORM IMS-STATUSKONTROLL                                           
163400     .                                                                    
163500     EJECT                                                                
163600 IMS-REPL-WL411111  SECTION.                                              
163700                                                                          
163800     MOVE '  ' TO GODK-STATUSKODER                                        
163900     CALL CBLTDLI USING REPL 4111-PCB WL411111                            
164000     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
164100     PERFORM IMS-STATUSKONTROLL                                           
164200     .                                                                    
164300     EJECT                                                                
164400 IMS-GU-SEQB-WDA301    SECTION.                                           
164500                                                                          
164600     STRING 'WDA301  (WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
164700                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
164800          DELIMITED BY SIZE INTO SSA1                                     
164900     MOVE '  GE'           TO GODK-STATUSKODER                            
165000     CALL CBLTDLI USING GU SEQB-PCB DLI-IO-AREA SSA1                      
165100     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
165200     PERFORM IMS-STATUSKONTROLL                                           
165300     .                                                                    
165400     EJECT                                                                
165500 IMS-GHU-SEQB-WDA301    SECTION.                                          
165600                                                                          
165700     STRING 'WDA301  (WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
165800                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
165900          DELIMITED BY SIZE INTO SSA1                                     
166000     MOVE '  GE'           TO GODK-STATUSKODER                            
166100     CALL CBLTDLI USING GHU SEQB-PCB DLI-IO-AREA SSA1                     
166200     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
166300     PERFORM IMS-STATUSKONTROLL                                           
166400     .                                                                    
166500     EJECT                                                                
166600 IMS-GHN-SEQB-WDA301   SECTION.                                           
166700                                                                          
166800     STRING 'WDA301  (WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
166900                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
167000          DELIMITED BY SIZE INTO SSA1                                     
167100     MOVE '  GE' TO GODK-STATUSKODER                                      
167200     CALL CBLTDLI USING GHN SEQB-PCB DLI-IO-AREA SSA1                     
167300     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     EJECT                                                                
167700 IMS-REPL-SEQB-WDA301      SECTION.                                       
167800                                                                          
167900     MOVE '    '           TO GODK-STATUSKODER                            
168000     CALL CBLTDLI USING REPL SEQB-PCB DLI-IO-AREA                         
168100     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
168200     PERFORM IMS-STATUSKONTROLL                                           
168300     .                                                                    
168400     EJECT                                                                
168500 IMS-GU-WDA301             SECTION.                                       
168600                                                                          
168700     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
168800          DELIMITED BY SIZE INTO SSA1                                     
168900     MOVE '  GE'           TO GODK-STATUSKODER                            
169000     CALL CBLTDLI USING GU RETA-PCB DLI-IO-AREA SSA1                      
169100     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
169200     PERFORM IMS-STATUSKONTROLL                                           
169300     .                                                                    
169400     EJECT                                                                
169500 IMS-STATUSKONTROLL SECTION.                                              
169600                                                                          
169700     SET STATUS-IX TO 1                                                   
169800     SEARCH GODK-STATUS                                                   
169900       AT END                                                             
170000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
170100         DELIMITED BY SIZE INTO FELTEXT                                   
170200         CALL FELLOG                                                      
170300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
170400         CONTINUE                                                         
170500     END-SEARCH                                                           
170600     .                                                                    
