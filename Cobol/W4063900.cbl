000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4063900.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   04/11/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM IS A DDI VERSION LIKE W4067500 FOR LOAD             
000900*        INFORMATON IN PROGRAM W4067400 (LASTINFO).                       
001000*        THIS PROGRAM STARTS BUILDING THE DATABASE WDE1                   
001100*        AND W4063600 IS UPDATING IT WITH MORE INFORMATION.               
001200*        THIS PROGRAM RUN IN THE BACKGROUND .                             
001300*        THIS PROGRAM IS STARTED BY W4066400.                             
001400*                                                                         
001500*        THE PROGRAM UPDATES   WDE1                                       
001600*        THE PROGRAM UPDATES   WDE2 (TILLFÄLLIGT, BORT I 4636)            
001700*        THE PROGRAM READS     WDE6                                       
001800*        THE PROGRAM READS     WDB2                                       
001900*        THE PROGRAM READS     WDR4                                       
002000*        THE PROGRAM READS     WDB6                                       
002100*        THE PROGRAM UPPDATES  WDR1                                       
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSACTION: W40639X                                             
002500*        MID:         W4I63901                                            
002600*                                                                         
002700*    OUTDATA.                                                             
002800*        MOD:         W4O63901                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W4063900'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  YES-Y                       PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
004600                                                                          
004700*    --- INDEX FOR SCROLL LINES                                           
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
005000 77  IDPSN-IX                    PIC S9(3)  VALUE +0    COMP SYNC.        
005100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0 COMP SYNC.           
005200 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005300 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
005400                                                                          
005500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005600     88  INDATA-OK                           VALUE 'J'.                   
005700     88  INDATA-WRONG                        VALUE 'N'.                   
005800                                                                          
005900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006000     88  KEYS-OK                             VALUE 'J'.                   
006100     88  KEYS-WRONG                          VALUE 'N'.                   
006200                                                                          
006300 77  INPUT-SW                    PIC X       VALUE 'N'.                   
006400     88  INPUT-GIVEN                         VALUE 'J'.                   
006500                                                                          
006600 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
006700     88  KDCMD-GIVEN                         VALUE 'J'.                   
006800                                                                          
006900 77  4498-MISSING-SW             PIC X       VALUE 'N'.                   
007000     88  4498-MISSING                        VALUE 'J'.                   
007100                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  OWN-MID                             VALUE '4639'.                
007400     88  GOOD-MID                            VALUE '466D'                 
007500                                                   '4639'.                
007600** SCREEN 4664 START 4639 USING IDTRANS '466D'                            
007700     88  466D-MID                            VALUE '466D'.                
007800     88  HELP-MID                            VALUE '0551'.                
007900                                                                          
008000 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
008100 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
008200                                                                          
008300 01  W-IDDCTEXT-MSGI.                                                     
008400     03  FILLER                  PIC X(5)    VALUE 'WIDDC'.               
008500     03  W-IDDC-MSGI             PIC X(2).                                
008600                                                                          
008700       EJECT                                                              
008800 77  UPDATE-SAVE-SW              PIC X(01)   VALUE 'N'.                   
008900     88  UPDATE-SAVE                         VALUE 'J'.                   
009000                                                                          
009100 77  RESTART-SW                  PIC X(01)   VALUE 'N'.                   
009200     88 RESTART                              VALUE 'J'.                   
009300                                                                          
009400 77  FIRST-PRODNR-SW             PIC X(01)   VALUE 'N'.                   
009500     88 FIRST-PRODNR                         VALUE 'J'.                   
009600                                                                          
009700 77  FIRST-KUNDNR-SW             PIC X(01)   VALUE 'N'.                   
009800     88 SW-FIRST-KUNDNR                      VALUE 'J'.                   
009900                                                                          
010000 77  FIRST-DEALER-SW             PIC X(01)   VALUE 'N'.                   
010100     88 SW-FIRST-DEALER                      VALUE 'J'.                   
010200                                                                          
010300 77  FLSAMFAK-SW                 PIC X(01)   VALUE 'N'.                   
010400     88 FLSAMFAK                             VALUE 'J'.                   
010500                                                                          
010600 77  FLSAMFAK-PREV-SW            PIC X(01)   VALUE 'N'.                   
010700     88 FLSAMFAK-PREV                        VALUE 'J'.                   
010800                                                                          
010900 77  FARLIGT-GOOD-4539           PIC X(01)   VALUE 'N'.                   
011000     88 FARLIGT-GOOD-FOUND-4539              VALUE 'J'.                   
011100                                                                          
011200 77  FARLIGT-GOOD-4540           PIC X(01)   VALUE 'N'.                   
011300     88 FARLIGT-GOOD-FOUND-4540              VALUE 'J'.                   
011400                                                                          
011500                                                                          
011600 77  W-FLAVSLUTA-SW              PIC X(01)   VALUE ' '.                   
011700     88 FLAVSLUTA-OK                         VALUE 'Y' 'J'.               
011800     EJECT                                                                
011900*  TO CONVERT THE YYMMDD  TO CCYYMMDD                                     
012000 01  W-YYMMDD-DATUM              PIC 9(6).                                
012100 01  W-CCYYMMDD-DATUM.                                                    
012200     03 W-CC                     PIC 9(2).                                
012300     03 W-YYMMDD.                                                         
012400        05 W-YY                  PIC 9(2).                                
012500        05 W-MM                  PIC 9(2).                                
012600        05 W-DD                  PIC 9(2).                                
012700                                                                          
012800 77  W-UPD-COUNT                 PIC S9(3)   VALUE ZERO.                  
012900 77  W-UPD-MAX                   PIC S9(3)   VALUE +100.                  
013000 77  W-IDDISTR-PREV              PIC S9(5)   COMP-3 VALUE ZERO.           
013100 77  W-IDKUNDNR-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
013200 77  W-IDKUNDNR-CHANGE           PIC S9(7)   COMP-3 VALUE ZERO.           
013300 77  W-IDDEALER-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
013400 77  W-IDPRODNR-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
013500 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
013600                                                                          
013700 77  W-FLFARLIG                  PIC X(01)   VALUE SPACE.                 
013800 77  W-FLSKRIV-NU                PIC X(01)   VALUE SPACE.                 
013900 77  W-FLAVSLUTA                 PIC X(01)   VALUE SPACE.                 
014000                                                                          
014100 77  W-IDDISTR-NUM               PIC  9(4)   VALUE ZERO.                  
014200 77  W-IDKUNDNR-NUM              PIC  9(6)   VALUE ZERO.                  
014300 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
014400 77  W-VORD-KDFRAKT              PIC S9(3)   COMP-3 VALUE ZERO.           
014500                                                                          
014600 77  W-KDORDKL-MAX               PIC S9(1)      COMP-3 VALUE ZERO.        
014700                                                                          
014800 01  FILLER                      PIC X(25)                                
014900                                      VALUE 'FOR WDE6 UPDATE'.            
015000 01  FILLER.                                                              
015100     03  KLI-PACK                PIC S9(1) COMP-3 VALUE +1.               
015200     03  KLI-PACK-FAKT           PIC S9(1) COMP-3 VALUE +6.               
015300                                                                          
015400     03 WS-TISKPTID              PIC 9(6)       VALUE ZERO.               
015500     03 WS-TISKPTID-GRP          REDEFINES WS-TISKPTID.                   
015600        05 WS-TISKPTID-HHMM         PIC 9(4).                             
015700        05 WS-TISKPTID-SS           PIC 9(2).                             
015800                                                                          
015900     EJECT                                                                
016000                                                                          
016100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
016200 01  GENERAL-SUBPROGRAMS.                                                 
016300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
016800     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
016900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
017000     EJECT                                                                
017100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
017200*01 -COPY WMEDAREA                                                        
017300     SKIP3                                                                
017400 01  FILLER.                                                              
017500   03  FELMEDD-AREA.                                                      
017600     05  FELMEDD-ENGLISH.                                                 
017700       10  FILLER                PIC X(40)                                
017800           VALUE '622 4639 WRONG PICTURE SELECTED         '.              
017900                                                                          
018000 01  MESSAGE-CODES.                                                       
018100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
018300     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
018400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
018700     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
018800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018900     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
019000     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
019100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019200     EJECT                                                                
019300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
019400*                                                                         
019500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019600     SKIP3                                                                
019700*01 -COPY WMSGINIT                                                        
019800     EJECT                                                                
019900 01  FILLER                      PIC X(25)   VALUE 'DC-WMSGINIT'.         
020000     SKIP3                                                                
020100*01 -COPY WMSGINIT     -PRE  DC-                                          
020200     EJECT                                                                
020300*    --- AREA  FOR WZ01  ------                                           
020400 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
020500*01  -COPY WZ01SEND                                                       
020600                                                                          
020700*    --- AREA  FOR W476SHNO ---                                           
020800 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
020900*01  -COPY W476SHNO                                                       
021000                                                                          
021100*01    -COPY WDECAREA                                                     
021200     EJECT                                                                
021300                                                                          
021400 01  TEST-IDDISTR                PIC  9(5)  COMP-3.                       
021500*01  FILLER     -COPY WWDIST07    -RED TEST-IDDISTR.                      
021600     SKIP2                                                                
021700*01  FILLER     -COPY WWDIST35    -RED TEST-IDDISTR.                      
021800     SKIP2                                                                
021900*01  FILLER     -COPY WWDIST83    -RED TEST-IDDISTR.                      
022000     SKIP2                                                                
022100*01  FILLER     -COPY WWDIST92    -RED TEST-IDDISTR.                      
022200     SKIP2                                                                
022300     EJECT                                                                
022400*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
022500*                                                                         
022600 01  SAVE-AREA.                                                           
022700     03  SAVE-IDTRANS            PIC X(4)    VALUE '4639'.                
022800* COMMON AREA FOR 4639 AND 4676 ....EJ                                    
022900     03  SAVE-4639-4676.                                                  
023000         05  SAVE-IDDISTR-ENTER      PIC S9(5).                           
023100         05  SAVE-IDKUNDNR-ENTER     PIC S9(7).                           
023200         05  SAVE-KDFAKTYP-ENTER     PIC X(01).                           
023300         05  SAVE-IDKUNDRF-ENTER     PIC X(10).                           
023400         05  SAVE-IDPRODNR-ENTER     PIC S9(07).                          
023500         05  SAVE-IDKOLLI-ENTER      PIC S9(05).                          
023600         05  SAVE-IDDISTR-4676       PIC S9(5).                           
023700         05  SAVE-IDKUNDNR-4676      PIC S9(7).                           
023800         05  SAVE-FLSAMFAK-4676      PIC X(01)   VALUE SPACE.             
023900         05  SAVE-IDSHIPM            PIC  9(7).                           
024000         05  SAVE-FLFARLIG           PIC X(01).                           
024100         05  SAVE-FLSKRIV-NU         PIC X(01).                           
024200         05  SAVE-FLAVSLUTA          PIC X(01).                           
024300         05  SAVE-REL-TRANSPORT      PIC X(01)   VALUE 'N'.               
024400             88  REL-TRANSPORT                   VALUE 'J'.               
024500*                                                                         
024600     03  SAVE-IDDISTR-NEXT       PIC S9(5).                               
024700     03  SAVE-IDKUNDNR-NEXT      PIC S9(7).                               
024800     03  SAVE-KDFAKTYP-NEXT      PIC X(01).                               
024900     03  SAVE-IDKUNDRF-NEXT      PIC X(10).                               
025000     03  SAVE-IDPRODNR-NEXT      PIC S9(07).                              
025100     03  SAVE-IDKOLLI-NEXT       PIC S9(05).                              
025200*                                                                         
025300     03  SAVE-INPUT  OCCURS 14.                                           
025400         05  SAVE-RECORD-PRESENT PIC X(01)   VALUE SPACE.                 
025500         05  SAVE-IDDISTR        PIC S9(05).                              
025600         05  SAVE-IDKUNDNR       PIC S9(07).                              
025700         05  SAVE-PRFOERS        PIC S9(7)V9(2)    COMP-3.                
025800         05  SAVE-REFOERS        PIC S9(2)V9(3)    COMP-3.                
025900         05  SAVE-REOVKOFF       PIC S9(2)V9(1)    COMP-3.                
026000         05  SAVE-FLSAMFAK       PIC X(01)   VALUE SPACE.                 
026100     EJECT                                                                
026200*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
026300*                                                                         
026400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
026500     SKIP3                                                                
026600*01  MID -COPY W4I63901                                                   
026700     EJECT                                                                
026800 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
026900     SKIP3                                                                
027000*01  -COPY WMSGAREA                                                       
027100     EJECT                                                                
027200*   TO RETURN TO MAIN MENU                                                
027300*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
027400     EJECT                                                                
027500     03  MOD REDEFINES MSG-AREA.                                          
027600*      05  -COPY W4O63901                                                 
027700     EJECT                                                                
027800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027900     SKIP3                                                                
028000*01  -COPY WMFSAREA                                                       
028100     EJECT                                                                
028200*01  -COPY W40636I1   -PRE MOD4636-                                       
028300     EJECT                                                                
028400 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
028500 01      P-TO-P-SW.                                                       
028600                                                                          
028700  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
028800  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
028900  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
029000  02     P-TO-P-KDTRANS          PIC X(8).                                
029100  02     P-TO-P-IDTRANS          PIC X(4).                                
029200  02     P-TO-P-KDMFSFOR         PIC X(1).                                
029300  02     P-TO-P-DATA             PIC X(1000).                             
029400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029500     SKIP3                                                                
029600 01  KEYS-TO-DLI.                                                         
029700*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
029800     03  W-4495-X.                                                        
029900         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
030000         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
030100         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
030200         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
030300         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
030400                                                                          
030500     03  W-4498-X.                                                        
030600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
030700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
030800         05  W-KDFAKTYP          PIC  X(1)   VALUE SPACE.                 
030900         05  W-IDKUNDRF          PIC  X(10).                              
031000         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
031100           07  W-IDORDNR7        PIC  9(7).                               
031200           07  FILLER            PIC  X(3).                               
031300         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
031400         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
031500                                                                          
031600     03  W-4498-N-X.                                                      
031700         05  W-IDDISTR-N         PIC S9(5)   VALUE ZERO  COMP-3.          
031800         05  W-IDKUNDNR-N        PIC S9(7)   VALUE ZERO  COMP-3.          
031900         05  FILLER              PIC X(18)   VALUE HIGH-VALUE.            
032000                                                                          
032100     03  W-4513-X.                                                        
032200         05  W-IDHTYP            PIC X(4)    VALUE '4513'.                
032300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
032400                                                                          
032500     03  W-4514-X.                                                        
032600         05  W-DASKEPPN-4514     PIC 9(8)    VALUE ZERO.                  
032700                                                                          
032800     03  W-IDDC-X.                                                        
032900         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
033000                                                                          
033100     03  W-IDPRODNR-X.                                                    
033200         05  W-IDPRODNR-KOLLI    PIC S9(7)   VALUE ZERO COMP-3.           
033300                                                                          
033400     03  W-IDKOLLI-X.                                                     
033500         05  W-IDKOLLI-KOLLI     PIC S9(5)   VALUE ZERO  COMP-3.          
033600                                                                          
033700     03  W-IDSHIPM-X.                                                     
033800         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
033900                                                                          
034000     03  W-WDE111KY-X.                                                    
034100         05  W-WDE111-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
034200         05  W-WDE111-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
034300                                                                          
034400     03  W-WDE211KY-X.                                                    
034500         05  W-WDE211-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
034600         05  W-WDE211-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
034700                                                                          
034800     03  W-IDGMT-X.                                                       
034900         05  W-WDB201-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
035000         05  W-WDB201-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
035100                                                                          
035200     03  W-IDDC-B6-X.                                                     
035300         05 W-IDDC-B6                  PIC X(2).                          
035400                                                                          
035500     SKIP2                                                                
035600*---MSG-AREA FOR 4539 FOR FARL.GOOD                                       
035700 01  FILLER                PIC X(16)  VALUE '4539-MSG-IO-AREA'.           
035800 01  4539-MSG-IO-AREA.                                                    
035900     03  4539-LL              PIC S9(4)  VALUE +49 COMP SYNC.             
036000     03  4539-Z1              PIC X.                                      
036100     03  4539-Z2              PIC X.                                      
036200     03  4539-TRANSKOD        PIC X(8)   VALUE 'W4T539X '.                
036300     03  4539-IDTRANS         PIC X(4)   VALUE '4639'.                    
036400     03  4539-SPRAK           PIC X.                                      
036500*    03  -COPY W4I53901   -PRE 4539-                                      
036600     EJECT                                                                
036700*---MSG-AREA FOR 4540 FOR FARL.GOOD                                       
036800 01  FILLER                PIC X(16)  VALUE '4540-MSG-IO-AREA'.           
036900 01  4540-MSG-IO-AREA.                                                    
037000     03  4540-LL              PIC S9(4)  VALUE +49 COMP SYNC.             
037100     03  4540-Z1              PIC X.                                      
037200     03  4540-Z2              PIC X.                                      
037300     03  4540-TRANSKOD        PIC X(8)   VALUE 'W4T540X '.                
037400     03  4540-IDTRANS         PIC X(4)   VALUE '4639'.                    
037500     03  4540-SPRAK           PIC X.                                      
037600*    03  -COPY W4I54001   -PRE 4540-                                      
037700     EJECT                                                                
037800*    --- STATUS-KOD FRÅN IMS                                              
037900 01  STATUS-WS                   PIC XX.                                  
038000     88  SEGMENT-FOUND                       VALUE '  '.                  
038100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
038200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
038300     SKIP2                                                                
038400 01  GOOD-STATUSCODES.                                                    
038500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038600     SKIP3                                                                
038700 01  SSA1                        PIC X(128).                              
038800 01  SSA2                        PIC X(128).                              
038900 01  SSA3                        PIC X(128).                              
039000 01  SSA4                        PIC X(128).                              
039100     EJECT                                                                
039200*    --- IMS FUNCTION CODES                                               
039300*01  -COPY W0003                                                          
039400     EJECT                                                                
039500*    ---  DLI INPUT-OUTPUT AREA                                           
039600                                                                          
039700*                                                                         
039800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4495'.                    
039900 01  DLI-IO-WDGX4495.                                                     
040000*    03  -COPY WDGX4495                                                   
040100                                                                          
040200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4498'.                    
040300 01  DLI-IO-WDGX4498.                                                     
040400*    03  -COPY WDGX4498                                                   
040500                                                                          
040600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4514'.                    
040700 01  DLI-IO-WDGX4514.                                                     
040800*    03  -COPY WDGX4514                                                   
040900                                                                          
041000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4516'.                    
041100 01  DLI-IO-WDGX4516.                                                     
041200*    03  -COPY WDGX4516                                                   
041300                                                                          
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
041500 01  DLI-IO-WDE601.                                                       
041600*    03  -COPY WDE601                                                     
041700     EJECT                                                                
041800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
041900 01  DLI-IO-WDE611.                                                       
042000*    03  -COPY WDE611                                                     
042100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
042200 01  DLI-IO-WDE101.                                                       
042300*    03  -COPY WDE101                                                     
042400     EJECT                                                                
042500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
042600 01  DLI-IO-WDE111.                                                       
042700*    03  -COPY WDE111                                                     
042800     EJECT                                                                
042900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
043000 01  DLI-IO-WDE121.                                                       
043100*    03  -COPY WDE121                                                     
043200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
043300 01  DLI-IO-WDE201.                                                       
043400*    03  -COPY WDE201                                                     
043500     EJECT                                                                
043600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE211'.                      
043700 01  DLI-IO-WDE211.                                                       
043800*    03  -COPY WDE211                                                     
043900     EJECT                                                                
044000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE221'.                      
044100 01  DLI-IO-WDE221.                                                       
044200*    03  -COPY WDE221                                                     
044300     EJECT                                                                
044400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
044500 01  DLI-IO-WDB201.                                                       
044600*    03  -COPY WDB201                                                     
044700                                                                          
044800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
044900 01   DLI-IO-AREA-B601.                                                   
045000*     03  -COPY WDB601                                                    
045100                                                                          
045200     EJECT                                                                
045300 LINKAGE SECTION.                                                         
045400*01  -COPY W0009  -PRE MSG-                                               
045500     EJECT                                                                
045600*01  -COPY W0009  -PRE AD36-                                              
045700     EJECT                                                                
045800*01  -COPY W0009  -PRE ALT-                                               
045900     EJECT                                                                
046000*01  -COPY W0009  -PRE ALT3-                                              
046100     EJECT                                                                
046200*01  -COPY W0009  -PRE ALT4-                                              
046300     EJECT                                                                
046400*01  -COPY W0008  -PRE WDP7-                                              
046500     05  FILLER                  PIC X.                                   
046600                                                                          
046700*01  -COPY W0008  -PRE WDE6-                                              
046800     05  FILLER                  PIC X.                                   
046900                                                                          
047000*01  -COPY W0008  -PRE WDE1-                                              
047100     05  FILLER                  PIC X.                                   
047200                                                                          
047300*01  -COPY W0008  -PRE WDE2-                                              
047400     05  FILLER                  PIC X.                                   
047500                                                                          
047600*01  -COPY W0008  -PRE WDB2-                                              
047700     05  FILLER                  PIC X.                                   
047800                                                                          
047900*01  -COPY W0008  -PRE 4495-                                              
048000     05  FILLER                  PIC X.                                   
048100                                                                          
048200*01  -COPY W0008  -PRE 4513-                                              
048300     05  FILLER                  PIC X.                                   
048400                                                                          
048500*01  -COPY W0008  -PRE WDB6-                                              
048600     05  FILLER                  PIC X.                                   
048700                                                                          
048800 01  SHNO-4517-PCB               PIC X.                                   
048900     EJECT                                                                
049000                                                                          
049100 PROCEDURE DIVISION  USING MSG-PCB  AD36-PCB  ALT-PCB                     
049200                           ALT3-PCB ALT4-PCB                              
049300                           WDP7-PCB WDE6-PCB WDE1-PCB                     
049400                           WDE2-PCB WDB2-PCB 4495-PCB                     
049500                           4513-PCB WDB6-PCB                              
049600                           SHNO-4517-PCB.                                 
049700 MAIN SECTION.                                                            
049800     ENTRY 'DLITCBL' USING MSG-PCB  AD36-PCB  ALT-PCB                     
049900                           ALT3-PCB ALT4-PCB                              
050000                           WDP7-PCB WDE6-PCB WDE1-PCB                     
050100                           WDE2-PCB WDB2-PCB 4495-PCB                     
050200                           4513-PCB WDB6-PCB                              
050300                           SHNO-4517-PCB.                                 
050400                                                                          
050500     PERFORM IMS-GET-MSG                                                  
050600     IF SEGMENT-FOUND                                                     
050700       PERFORM A-INIT                                                     
050800       IF GOOD-MID                                                        
050900         PERFORM B-CHECK-KEYS                                             
051000       END-IF                                                             
051100       IF KEYS-OK                                                         
051200         IF MFS-UPD-X                                                     
051300           PERFORM I-REL-TRANSPORT                                        
051400         END-IF                                                           
051500       END-IF                                                             
051600       IF INDATA-OK AND REL-TRANSPORT AND NOT RESTART                     
051700         CONTINUE                                                         
051800       ELSE                                                               
051900          IF RESTART                                                      
052000            PERFORM J-START-W40639                                        
052100          ELSE                                                            
052200             IF GOOD-MID                                                  
052300               COMPUTE MSG-KVLL = LENGTH OF MOD-W4O63901 + 4              
052400               PERFORM IMS-INSERT-MSG                                     
052500             END-IF                                                       
052600          END-IF                                                          
052700       END-IF                                                             
052800       IF UPDATE-SAVE                                                     
052900          MOVE '002'       TO MSGI-KDCALL                                 
053000          MOVE '4639'      TO SAVE-IDTRANS                                
053100          MOVE SAVE-AREA   TO MSGI-SPAR-AREA                              
053200          CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                      
053300       END-IF                                                             
053400     END-IF                                                               
053500                                                                          
053600     MOVE ZERO TO RETURN-CODE                                             
053700     GOBACK                                                               
053800     .                                                                    
053900     EJECT                                                                
054000 A-INIT SECTION.                                                          
054100                                                                          
054200     IF MSG-DOUBLE-TRANSACTIONS                                           
054300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I63901                 
054400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
054500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
054600     ELSE                                                                 
054700       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W4I63901                 
054800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
054900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
055000     END-IF                                                               
055100     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
055200     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
055300     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
055400     MOVE LOW-VALUE        TO MSG-AREA                                    
055500     MOVE 'W4O639N1'       TO MFS-IDMOD                                   
055600     MOVE '4639'           TO MOD-IDTRANS                                 
055700     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
055800                                                                          
055900     IF NOT GOOD-MID                                                      
056000       MOVE NOO  TO KEYS-SW                                               
056100                    INDATA-SW                                             
056200       PERFORM S25-WRONG-PICTURE-MESSAGE                                  
056300     END-IF                                                               
056400                                                                          
056500     MOVE ZERO        TO W-IDSHIPM                                        
056600     MOVE ZERO        TO W-IDDISTR-N                                      
056700     MOVE ALL '9'     TO W-IDKUNDNR-N                                     
056800                                                                          
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057200 B-CHECK-KEYS SECTION.                                                    
057300                                                                          
057400     MOVE ALL '+'              TO MSGI-WMSGINIT                           
057500     MOVE '001'                TO MSGI-KDCALL                             
057600     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
057700     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
057800     MOVE '4639'               TO MSGI-IDTRANS                            
057900                                                                          
058000     IF GOOD-MID                                                          
058100        MOVE MID-IDTRPTNR-IN   TO MSGI-IDTRPTNR                           
058200        MOVE MID-IDLBBET-IN    TO MSGI-IDLBBET                            
058300        MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                           
058400     END-IF                                                               
058500                                                                          
058600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
058700                                                                          
058800     MOVE MSGI-SPAR-AREA       TO SAVE-AREA                               
058900                                                                          
059000     IF 466D-MID                                                          
059100       MOVE MID-IDTRPTNR-IN    TO MSGI-IDTRPTNR                           
059200       MOVE MID-IDLBBET-IN     TO MSGI-IDLBBET                            
059300       MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                           
059400       MOVE SPACE              TO SAVE-REL-TRANSPORT                      
059500       MOVE YES                TO UPDATE-SAVE-SW                          
059600     END-IF                                                               
059700                                                                          
059800*    - LANGUAGE TO BE USED BY MEDKONV                                     
059900     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
060000                                                                          
060100     MOVE YES                  TO KEYS-SW                                 
060200                                                                          
060300*    -- CHECK OF WDGXKEY                                                  
060400     MOVE MFS-ERASE-FIELD      TO MOD-IDTRPTNR-IN                         
060500                                  MOD-IDLBBET-IN                          
060600                                  MOD-FLFARLIG-IN                         
060700                                  MOD-IDDC-IN                             
060800                                                                          
060900     IF MFS-UPD-X OR REL-TRANSPORT                                        
061000       MOVE YES              TO SAVE-REL-TRANSPORT                        
061100       MOVE YES              TO UPDATE-SAVE-SW                            
061200     ELSE                                                                 
061300       IF GOOD-MID                                                        
061400         IF MID-IDTRPTNR-IN NOT = ALL '+'                                 
061500            MOVE '7'         TO MFS-IDPFK                                 
061600            MOVE SPACE       TO MFS-KDTRTYP                               
061700         END-IF                                                           
061800         IF MID-IDLBBET-IN  NOT = ALL '+'                                 
061900           MOVE '7'         TO MFS-IDPFK                                  
062000           MOVE SPACE       TO MFS-KDTRTYP                                
062100         END-IF                                                           
062200         IF MID-IDDC-IN     NOT = ALL '+'                                 
062300           MOVE '7'         TO MFS-IDPFK                                  
062400           MOVE SPACE       TO MFS-KDTRTYP                                
062500         END-IF                                                           
062600         IF MID-FLFARLIG-IN NOT = ALL '+'                                 
062700           MOVE '7'         TO MFS-IDPFK                                  
062800           MOVE SPACE       TO MFS-KDTRTYP                                
062900         END-IF                                                           
063000       ELSE                                                               
063100         MOVE '7'         TO MFS-IDPFK                                    
063200         MOVE SPACE       TO MFS-KDTRTYP                                  
063300       END-IF                                                             
063400       IF MFS-FIRST                                                       
063500         MOVE SPACE       TO SAVE-FLAVSLUTA                               
063600                             SAVE-REL-TRANSPORT                           
063700         MOVE YES         TO UPDATE-SAVE-SW                               
063800       END-IF                                                             
063900     END-IF                                                               
064000                                                                          
064100     IF MSGI-IDTRPTNR NUMERIC                                             
064200        IF MSGI-IDTRPTNR > ZERO                                           
064300           MOVE MSGI-IDTRPTNR   TO  W-IDTRPTNR                            
064400        ELSE                                                              
064500          MOVE NOO        TO KEYS-SW                                      
064600        END-IF                                                            
064700     ELSE                                                                 
064800        MOVE NOO          TO KEYS-SW                                      
064900     END-IF                                                               
065000                                                                          
065100     IF MSGI-IDLBBET = ALL '+'                                            
065200        MOVE NOO           TO KEYS-SW                                     
065300     ELSE                                                                 
065400       IF MSGI-IDLBBET > SPACE                                            
065500          MOVE MSGI-IDLBBET  TO W-IDLBBET                                 
065600       ELSE                                                               
065700          MOVE NOO           TO KEYS-SW                                   
065800       END-IF                                                             
065900     END-IF                                                               
066000                                                                          
066100     IF MID-FLFARLIG-IN NOT = ALL '+'                                     
066200       MOVE MID-FLFARLIG-IN    TO W-FLFARLIG                              
066300     ELSE                                                                 
066400       IF SAVE-FLFARLIG > SPACE                                           
066500         MOVE SAVE-FLFARLIG    TO W-FLFARLIG                              
066600       END-IF                                                             
066700     END-IF                                                               
066800                                                                          
066900     IF W-FLFARLIG NOT = SAVE-FLFARLIG                                    
067000       MOVE W-FLFARLIG         TO SAVE-FLFARLIG                           
067100       MOVE YES                TO UPDATE-SAVE-SW                          
067200     END-IF                                                               
067300                                                                          
067400     MOVE MSGI-IDDC-KEY        TO W-IDDC-B6                               
067500     PERFORM IMS-GU-WDB601                                                
067600     IF DCS-KDDC NOT = SPACE                                              
067700       MOVE MSGI-IDDC-KEY      TO W-IDDC                                  
067800                                  W-IDDC-4495                             
067900       IF DCS-DDC                                                         
068000         MOVE WS-CDC-11        TO W-IDDC-MSGI                             
068100       ELSE                                                               
068200         MOVE MSGI-IDDC-KEY    TO W-IDDC-MSGI                             
068300       END-IF                                                             
068400                                                                          
068500       MOVE ALL '+'            TO DC-MSGI-WMSGINIT                        
068600       MOVE '001'              TO DC-MSGI-KDCALL                          
068700       MOVE W-IDDCTEXT-MSGI    TO DC-MSGI-IDUSER                          
068800       MOVE '4639'             TO DC-MSGI-IDTRANS                         
068900       MOVE MSG-LTERM-NAME     TO DC-MSGI-IDLTERM-USER                    
069000       CALL W005INIT        USING DC-MSGI-WMSGINIT WDP7-PCB               
069100                                                                          
069200     ELSE                                                                 
069300       MOVE NOO                TO KEYS-SW                                 
069400     END-IF                                                               
069500                                                                          
069600     IF MID-FLSKRIV-NU NOT = ALL '+'                                      
069700       MOVE MID-FLSKRIV-NU     TO W-FLSKRIV-NU                            
069800     ELSE                                                                 
069900       IF SAVE-FLSKRIV-NU > SPACE                                         
070000         MOVE SAVE-FLSKRIV-NU  TO W-FLSKRIV-NU                            
070100       END-IF                                                             
070200     END-IF                                                               
070300                                                                          
070400     IF W-FLSKRIV-NU NOT = SAVE-FLSKRIV-NU                                
070500       MOVE W-FLSKRIV-NU       TO SAVE-FLSKRIV-NU                         
070600       MOVE YES                TO UPDATE-SAVE-SW                          
070700     END-IF                                                               
070800                                                                          
070900     IF 466D-MID                                                          
071000       IF MID-IDSHIPM NUMERIC                                             
071100         IF MID-IDSHIPM > ZERO                                            
071200           MOVE MID-IDSHIPM    TO W-IDSHIPM                               
071300         ELSE                                                             
071400           MOVE ZERO           TO W-IDSHIPM                               
071500         END-IF                                                           
071600       ELSE                                                               
071700         MOVE ZERO             TO W-IDSHIPM                               
071800       END-IF                                                             
071900       MOVE W-IDSHIPM          TO SAVE-IDSHIPM                            
072000                                  MOD-IDSHIPM                             
072100       MOVE YES                TO UPDATE-SAVE-SW                          
072200     ELSE                                                                 
072300       IF SAVE-IDSHIPM NUMERIC                                            
072400         MOVE SAVE-IDSHIPM     TO W-IDSHIPM                               
072500       ELSE                                                               
072600         MOVE ZERO             TO W-IDSHIPM                               
072700       END-IF                                                             
072800     END-IF                                                               
072900                                                                          
073000     IF GOOD-MID OR KEYS-OK                                               
073100       MOVE MSGI-IDTRPTNR       TO MOD-IDTRPTNR-UT                        
073200       MOVE MSGI-IDLBBET        TO MOD-IDLBBET-UT                         
073300       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
073400       MOVE SAVE-FLFARLIG       TO MOD-FLFARLIG-UT                        
073500       MOVE SAVE-FLAVSLUTA      TO MOD-FLAVSLUTA                          
073600       MOVE SAVE-FLSKRIV-NU     TO MOD-FLSKRIV-NU                         
073700       MOVE SAVE-IDSHIPM        TO MOD-IDSHIPM                            
073800     ELSE                                                                 
073900       PERFORM MFS-ERASE-FIELD-OUT                                        
074000       MOVE MFS-ERASE-FIELD     TO MOD-FLAVSLUTA                          
074100                                   MOD-FLSKRIV-NU                         
074200     END-IF                                                               
074300                                                                          
074400     IF KEYS-WRONG                                                        
074500       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
074600       CALL WMEDKONV            USING MED-WMEDAREA                        
074700       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
074800       PERFORM MFS-ERASE-FIELD-IN                                         
074900       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 I-REL-TRANSPORT SECTION.                                                 
075400                                                                          
075500     MOVE ZERO               TO W-IDDISTR-PREV                            
075600                                W-IDKUNDNR-PREV                           
075700                                W-IDDEALER-PREV                           
075800                                W-IDPRODNR-PREV                           
075900                                                                          
076000     MOVE YES                TO FIRST-PRODNR-SW                           
076100                                FIRST-KUNDNR-SW                           
076200                                FIRST-DEALER-SW                           
076300                                                                          
076400     MOVE NOO                TO RESTART-SW                                
076500                                FARLIGT-GOOD-4539                         
076600                                FARLIGT-GOOD-4540                         
076700                                FLSAMFAK-SW                               
076800                                4498-MISSING-SW                           
076900                                                                          
077000     MOVE ZERO               TO W-UPD-COUNT                               
077100                                                                          
077200                                                                          
077300     PERFORM S06-DC-LAND                                                  
077400                                                                          
077500     PERFORM IMS-GHU-WDR401-4495                                          
077600     IF SEGMENT-FOUND                                                     
077700                                                                          
077800        IF W-IDSHIPM = ZERO                                               
077900          PERFORM S01-CREATE-IDSHIPM                                      
078000          PERFORM S02-CREATE-WDE101                                       
078100          PERFORM S03-CREATE-WDE201                                       
078200        ELSE                                                              
078300          PERFORM IMS-GHU-WDE101                                          
078400          PERFORM IMS-GHU-WDE201                                          
078500        END-IF                                                            
078600                                                                          
078700        PERFORM IMS-GHNP-WDGX4498                                         
078800                                                                          
078900        IF SEGMENT-FOUND                                                  
079000          PERFORM S11-CHECK-FLSAMFAK                                      
079100        ELSE                                                              
079200          MOVE YES TO 4498-MISSING-SW                                     
079300        END-IF                                                            
079400                                                                          
079500        PERFORM UNTIL 4498-MISSING  OR RESTART                            
079600                                                                          
079700          IF 4498-IDPRODNR   NOT =  W-IDPRODNR-PREV                       
079800            IF FIRST-PRODNR-SW  = NOO                                     
079900              PERFORM S04-CHECK-KDORDKL                                   
080000            ELSE                                                          
080100              MOVE NOO     TO FIRST-PRODNR-SW                             
080200            END-IF                                                        
080300                                                                          
080400            MOVE 4498-IDPRODNR TO W-IDPRODNR-PREV                         
080500          END-IF                                                          
080600                                                                          
080700          IF DCS-NDC-NA  OR                                               
080800            (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU') OR                       
080900             DCS-CDC OR                                                   
081000            (DCS-DDC AND DCS-IDLANDX2 = 'SE')                             
081100            IF FIRST-DEALER-SW = NOO                                      
081200              IF (4498-IDDISTR      NOT =  W-IDDISTR-PREV)  OR            
081300                 (4498-IDDEALER     NOT =  W-IDDEALER-PREV)               
081400                MOVE W-IDDISTR-PREV TO TEST-IDDISTR                       
081500                IF FARLIGT-GOOD-FOUND-4539                                
081600                    AND DCS-NDC-NA                                        
081700                    AND W-KDORDKL-MAX < +2                                
081800                    AND (DIST35-NA-TRANSFER    OR                         
081900                         DIST35-NA-CDC-RETURN  OR                         
082000                         DIST35-NA-NDC-RETURNS OR                         
082100                         DIST35-REFILL-INOM-NA OR                         
082200                         DIST07-CAN-RETAILER)                             
082300                   PERFORM S09A-START-W40539                              
082400                   MOVE NOO TO FARLIGT-GOOD-4539                          
082500                ELSE                                                      
082600                  IF FARLIGT-GOOD-FOUND-4539                              
082700                      AND W-KDORDKL-MAX < +2                              
082800                      AND DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'              
082900                     PERFORM S09A-START-W40539                            
083000                     MOVE NOO TO FARLIGT-GOOD-4539                        
083100                  ELSE                                                    
083200                     IF FARLIGT-GOOD-FOUND-4539                           
083300                        AND (DCS-CDC OR                                   
083400                            (DCS-DDC AND DCS-IDLANDX2 = 'SE'))            
083500                       PERFORM S09A-START-W40539                          
083600                       MOVE NOO TO FARLIGT-GOOD-4539                      
083700                     END-IF                                               
083800                  END-IF                                                  
083900                END-IF                                                    
084000              END-IF                                                      
084100            ELSE                                                          
084200              MOVE NOO      TO  FIRST-DEALER-SW                           
084300            END-IF                                                        
084400          END-IF                                                          
084500                                                                          
084600          IF 4498-IDDISTR      NOT = W-IDDISTR-PREV  OR                   
084700             4498-IDKUNDNR     NOT = W-IDKUNDNR-PREV                      
084800                                                                          
084900            MOVE 4498-IDDISTR   TO W-WDE111-IDDISTR                       
085000                                   W-WDE211-IDDISTR                       
085100            MOVE 4498-IDKUNDNR  TO W-WDE111-IDKUNDNR                      
085200                                   W-WDE211-IDKUNDNR                      
085300            PERFORM S15-CREATE-WDE111                                     
085400            PERFORM S16-CREATE-WDE211                                     
085500                                                                          
085600          END-IF                                                          
085700                                                                          
085800                                                                          
085900          IF (4498-IDDISTR      NOT = W-IDDISTR-PREV)  OR                 
086000             (4498-IDKUNDNR     NOT = W-IDKUNDNR-PREV)                    
086100                                                                          
086200            MOVE 4498-IDKUNDNR  TO W-IDKUNDNR-PREV                        
086300            MOVE 4498-IDDISTR   TO W-IDDISTR-PREV                         
086400                                                                          
086500          END-IF                                                          
086600                                                                          
086700          MOVE 4498-IDDISTR TO TEST-IDDISTR                               
086800          PERFORM ID-UPDATE-KOLLI                                         
086900          IF DCS-CDC OR (DCS-DDC AND DCS-IDLANDX2 = 'SE')                 
087000            PERFORM IH-CREATE-HTR4513                                     
087100          END-IF                                                          
087200                                                                          
087300          PERFORM S17-CREATE-WDE121                                       
087400                                                                          
087500          PERFORM S18-CREATE-WDE221                                       
087600                                                                          
087700          MOVE 4498-IDDISTR TO TEST-IDDISTR                               
087800*         PERFORM IMS-DLET-WDGX4498                                       
087900                                                                          
088000          MOVE 4498-IDDEALER     TO W-IDDEALER-PREV                       
088100          MOVE FLSAMFAK-SW       TO FLSAMFAK-PREV-SW                      
088200          PERFORM IMS-GHNP-WDGX4498                                       
088300                                                                          
088400          IF SEGMENT-FOUND                                                
088500            IF FLSAMFAK                                                   
088600              IF W-IDTRPTNR < +100                                        
088700                CONTINUE                                                  
088800              ELSE                                                        
088900                IF 4498-IDDISTR NOT = W-IDDISTR-PREV                      
089000                  IF W-UPD-COUNT  > W-UPD-MAX                             
089100                    MOVE YES     TO RESTART-SW                            
089200                  END-IF                                                  
089300                END-IF                                                    
089400              END-IF                                                      
089500            ELSE                                                          
089600              IF DIST92-ITALY OR                                          
089700                 DIST92-GREECE                                            
089800                IF 4498-IDDISTR NOT = W-IDDISTR-PREV                      
089900                                                                          
090000                  IF W-UPD-COUNT  > W-UPD-MAX                             
090100                    MOVE YES     TO RESTART-SW                            
090200                  END-IF                                                  
090300                END-IF                                                    
090400              ELSE                                                        
090500                IF (4498-IDDISTR NOT = W-IDDISTR-PREV)   OR               
090600                   (4498-IDKUNDNR NOT = W-IDKUNDNR-PREV)                  
090700                  IF W-UPD-COUNT  > W-UPD-MAX                             
090800                    MOVE YES     TO RESTART-SW                            
090900                  END-IF                                                  
091000                END-IF                                                    
091100              END-IF                                                      
091200            END-IF                                                        
091300          ELSE                                                            
091400            MOVE YES TO 4498-MISSING-SW                                   
091500          END-IF                                                          
091600                                                                          
091700          IF NOT RESTART                                                  
091800            IF SEGMENT-FOUND                                              
091900              PERFORM S11-CHECK-FLSAMFAK                                  
092000            END-IF                                                        
092100          END-IF                                                          
092200                                                                          
092300        END-PERFORM                                                       
092400                                                                          
092500        PERFORM S04-CHECK-KDORDKL                                         
092600                                                                          
092700        IF FARLIGT-GOOD-FOUND-4540                                        
092800           AND (DCS-CDC OR                                                
092900                (DCS-DDC AND DCS-IDLANDX2 = 'SE'))                        
093000          PERFORM S09B-START-W40540                                       
093100          MOVE NOO TO FARLIGT-GOOD-4540                                   
093200        END-IF                                                            
093300                                                                          
093400        MOVE W-IDDISTR-PREV TO TEST-IDDISTR                               
093500        IF FARLIGT-GOOD-FOUND-4539                                        
093600           AND DCS-NDC-NA                                                 
093700           AND W-KDORDKL-MAX < +2                                         
093800           AND (DIST35-NA-TRANSFER    OR                                  
093900                DIST35-NA-CDC-RETURN  OR                                  
094000                DIST35-NA-NDC-RETURNS OR                                  
094100                DIST35-REFILL-INOM-NA OR                                  
094200                DIST07-CAN-RETAILER)                                      
094300          PERFORM S09A-START-W40539                                       
094400        ELSE                                                              
094500          IF FARLIGT-GOOD-FOUND-4539                                      
094600             AND W-KDORDKL-MAX < +2                                       
094700             AND DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'                       
094800            PERFORM S09A-START-W40539                                     
094900          ELSE                                                            
095000            IF FARLIGT-GOOD-FOUND-4539                                    
095100            AND (DCS-CDC OR                                               
095200                (DCS-DDC AND DCS-IDLANDX2 = 'SE'))                        
095300              PERFORM S09A-START-W40539                                   
095400            END-IF                                                        
095500          END-IF                                                          
095600        END-IF                                                            
095700                                                                          
095800        MOVE NOO                TO  FARLIGT-GOOD-4539                     
095900                                                                          
096000                                                                          
096100        PERFORM IMS-GHU-WDE101                                            
096200        MOVE 'N'           TO  SHIP-KDKLAR                                
096300        MOVE VORD-IDDC-EXP TO SHIP-IDDC-EXP                               
096400        PERFORM IMS-REPL-WDE101                                           
096500                                                                          
096600        PERFORM S21-OPEN-WZ01                                             
096700        PERFORM S22-SEND-WZ01                                             
096800        PERFORM S23-CLOSE-WZ01                                            
096900                                                                          
097000        MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                             
097100        CALL WMEDKONV USING MED-WMEDAREA                                  
097200        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
097300        PERFORM MFS-FORM-ATTR                                             
097400     END-IF                                                               
097500                                                                          
097600     .                                                                    
097700     EJECT                                                                
097800                                                                          
097900 ID-UPDATE-KOLLI   SECTION.                                               
098000                                                                          
098100     MOVE 4498-IDPRODNR         TO W-IDPRODNR-KOLLI                       
098200     IF DCS-CDC AND DIST83-DHL                                            
098300       PERFORM IMS-GHU-WDE601                                             
098400       MOVE VORD-KDFRAKT        TO W-VORD-KDFRAKT                         
098500     END-IF                                                               
098600                                                                          
098700     MOVE 4498-IDKOLLI          TO W-IDKOLLI-KOLLI                        
098800     PERFORM IMS-GHU-WDE611                                               
098900     IF SEGMENT-FOUND                                                     
099000** CHECK KOLLI CONTAIN FARLIGT(HAZARDOUS) GOOD                            
099100       MOVE +1       TO IDPSN-IX                                          
099200       PERFORM UNTIL IDPSN-IX > +10                                       
099300          IF KOLLI-IDPSN(IDPSN-IX) > ZERO                                 
099400             MOVE YES TO FARLIGT-GOOD-4539                                
099500             MOVE YES TO FARLIGT-GOOD-4540                                
099600          END-IF                                                          
099700          ADD +1     TO IDPSN-IX                                          
099800       END-PERFORM                                                        
099900                                                                          
100000                                                                          
100100       MOVE KLI-PACK-FAKT       TO KOLLI-KDKOLSTA                         
100200*      MOVE +9999999            TO KOLLI-TILASTID                         
100300*      PERFORM IMS-REPL-WDE6                                              
100400**     ADD +1                   TO W-UPD-COUNT                            
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 IH-CREATE-HTR4513 SECTION.                                               
100900                                                                          
101000     IF FARLIGT-GOOD-FOUND-4539                                           
101100     OR FARLIGT-GOOD-FOUND-4540                                           
101200       MOVE DC-MSGI-TILOKDAT            TO W-YYMMDD-DATUM                 
101300       MOVE W-YYMMDD-DATUM              TO W-YYMMDD                       
101400       MOVE '20'                        TO W-CC                           
101500       MOVE W-CCYYMMDD-DATUM            TO 4514-DASKEPPN                  
101600                                           W-DASKEPPN-4514                
101700       PERFORM IMS-ISRT-WDGX4514                                          
101800**     ADD +1                           TO W-UPD-COUNT                    
101900                                                                          
102000       MOVE W-IDDC                      TO 4516-IDDC                      
102100       MOVE W-IDSHIPM                   TO 4516-IDSKEPPN                  
102200       MOVE 4498-IDDISTR                TO 4516-IDDISTR                   
102300       MOVE 4498-IDKUNDNR               TO 4516-IDKUNDNR                  
102400       MOVE 4498-IDPRODNR               TO 4516-IDPRODNR                  
102500       MOVE 4498-IDKOLLI                TO 4516-IDKOLLI                   
102600       MOVE 4498-IDKUNDRF               TO 4516-IDKUNDRF                  
102700       PERFORM IMS-ISRT-WDGX4516                                          
102800**     ADD +1                           TO W-UPD-COUNT                    
102900     END-IF                                                               
103000     .                                                                    
103100     EJECT                                                                
103200 J-START-W40639          SECTION.                                         
103300                                                                          
103400     MOVE W-IDTRPTNR          TO  MID-IDTRPTNR-IN                         
103500     MOVE W-IDLBBET           TO  MID-IDLBBET-IN                          
103600     MOVE W-IDDC              TO  MID-IDDC-IN                             
103700     MOVE W-FLFARLIG          TO  MID-FLFARLIG-IN                         
103800     MOVE W-FLSKRIV-NU        TO  MID-FLSKRIV-NU                          
103900     MOVE W-IDSHIPM           TO  MID-IDSHIPM                             
104000     MOVE YES                 TO  MID-FLAVSLUTA                           
104100     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX +                    
104200                                   LENGTH OF MID-W4I63901                 
104300     MOVE LOW-VALUE           TO  P-TO-P-KDZ1                             
104400     MOVE LOW-VALUE           TO  P-TO-P-KDZ2                             
104500     MOVE '4639'              TO  P-TO-P-IDTRANS                          
104600     MOVE MFS-KDMFSFOR        TO  P-TO-P-KDMFSFOR                         
104700                                                                          
104800     MOVE MID-W4I63901        TO  P-TO-P-DATA                             
104900                                                                          
105000     IF MFS-UPD-X                                                         
105100       MOVE 'W4T639X '        TO  P-TO-P-KDTRANS                          
105200       PERFORM IMS-ISRT-ALT-MSG-4639X                                     
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600                                                                          
105700 S01-CREATE-IDSHIPM SECTION.                                              
105800                                                                          
105900     CALL W476SHNO USING SHNO-W476SHNO SHNO-4517-PCB                      
106000                                                                          
106100     MOVE SHNO-IDSHIPM            TO W-IDSHIPM                            
106200                                     SAVE-IDSHIPM                         
106300                                     MOD-IDSHIPM                          
106400     MOVE YES                     TO UPDATE-SAVE-SW                       
106500                                                                          
106600**   ADD +1                       TO W-UPD-COUNT                          
106700     .                                                                    
106800     EJECT                                                                
106900                                                                          
107000 S02-CREATE-WDE101 SECTION.                                               
107100                                                                          
107200     MOVE W-IDSHIPM           TO SHIP-IDSHIPM                             
107300     MOVE W-IDTRPTNR          TO SHIP-IDTRPTNR                            
107400     MOVE W-IDLBBET           TO SHIP-IDLBBET                             
107500     MOVE W-IDDC              TO SHIP-IDDC                                
107600     MOVE W-IDLANDX2          TO SHIP-IDLANDX3-SEND                       
107700     IF 4498-KDFAKTYP = 'N' OR 'K'                                        
107800       MOVE 'INT'             TO SHIP-KDFINDOC                            
107900     ELSE                                                                 
108000       MOVE 'INV'             TO SHIP-KDFINDOC                            
108100     END-IF                                                               
108200     MOVE DC-MSGI-TILOKDAT    TO SHIP-TISKEPPN                            
108300                                                                          
108400     MOVE DC-MSGI-TILOKTID    TO WS-TISKPTID-HHMM                         
108500     MOVE FUNCTION CURRENT-DATE (13:2) TO                                 
108600                                 WS-TISKPTID-SS                           
108700     MOVE WS-TISKPTID         TO SHIP-TISKPTID                            
108800     MOVE ZERO                TO SHIP-KVANTEX                             
108900                                 SHIP-SUNTO-TOT                           
109000                                 SHIP-PRKURS-BET                          
109100     MOVE W-FLSKRIV-NU        TO SHIP-FLSKRIV-NU                          
109200** AFTER ALL UPDATE AND RELEASE TRANSPORT SHIP-KDKLAR CHANGES             
109300** TO 'N' AND SHIP-IDDC-EXP IS UPDATED                                    
109400     MOVE 'A'                 TO SHIP-KDKLAR                              
109500     MOVE SPACE               TO SHIP-IDDC-EXP                            
109600                                 SHIP-BELEVVIL                            
109700                                 SHIP-IDSYSTEM                            
109800                                 SHIP-KDVALISO-BET                        
109900     MOVE '0'                 TO SHIP-KDFAKSTA-EXP                        
109910                                                                          
109920     MOVE NOO                 TO SHIP-FLFARLIG                            
109930     MOVE SPACES              TO SHIP-KDVALISO-EXP                        
109940     MOVE ZEROES              TO SHIP-SUORDV-EXP                          
109950                                 SHIP-SUORDV-FAKT                         
109960                                 SHIP-VKORDBTO-FAKT                       
109970                                 SHIP-VLORDBTO-FAKT                       
109980                                                                          
110000     PERFORM IMS-ISRT-WDE101                                              
110100**   ADD  +1                  TO W-UPD-COUNT                              
110200     .                                                                    
110300     EJECT                                                                
110400 S03-CREATE-WDE201 SECTION.                                               
110500                                                                          
110600     MOVE W-IDSHIPM           TO BILL-IDSHIPM                             
110700     MOVE W-IDDC              TO BILL-IDDC                                
110800     MOVE W-IDLANDX2          TO BILL-IDLANDX3-SEND                       
110900     MOVE SPACE               TO BILL-IDLEVNR                             
111000     IF 4498-KDFAKTYP = 'N' OR 'K'                                        
111100       MOVE 'INT'             TO BILL-KDFINDOC                            
111200     ELSE                                                                 
111300       MOVE 'INV'             TO BILL-KDFINDOC                            
111400     END-IF                                                               
111500     MOVE DC-MSGI-TILOKDAT    TO BILL-TISKEPPN                            
111600     MOVE WS-TISKPTID         TO BILL-TISKPTID                            
111700     MOVE SPACE               TO BILL-IDDC-EXP                            
111800     PERFORM IMS-ISRT-WDE201                                              
111900**   ADD  1                   TO W-UPD-COUNT                              
112000     .                                                                    
112100     EJECT                                                                
112200 S04-CHECK-KDORDKL SECTION.                                               
112300                                                                          
112400     MOVE W-IDPRODNR-PREV     TO W-IDPRODNR-KOLLI                         
112500     PERFORM IMS-GHU-WDE601                                               
112600                                                                          
112700     IF VORD-KDORDKL          >  W-KDORDKL-MAX                            
112800        MOVE VORD-KDORDKL     TO W-KDORDKL-MAX                            
112900     END-IF                                                               
113000     .                                                                    
113100     EJECT                                                                
113200                                                                          
113300 S06-DC-LAND  SECTION.                                                    
113400                                                                          
113500     IF W-IDDC NOT = W-IDDC-B6                                            
113600        MOVE W-IDDC    TO W-IDDC-B6                                       
113700        PERFORM IMS-GU-WDB601                                             
113800     END-IF                                                               
113900     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
114000     .                                                                    
114100     EJECT                                                                
114200 S09A-START-W40539  SECTION.                                              
114300                                                                          
114400     MOVE W-IDDISTR-PREV       TO W-IDDISTR-NUM                           
114500     MOVE W-IDDISTR-NUM        TO 4539-MID-IDDISTR                        
114600                                                                          
114700     MOVE W-IDDEALER-PREV      TO W-IDKUNDNR-NUM                          
114800     MOVE W-IDKUNDNR-NUM       TO 4539-MID-IDKUNDNR                       
114900                                                                          
115000     MOVE MFS-KDMFSFOR         TO 4539-SPRAK                              
115100     MOVE '99'                 TO 4539-MID-KDFRAKT                        
115200     MOVE W-IDSHIPM            TO 4539-MID-IDSKEPPN                       
115300     MOVE W-IDDC               TO 4539-MID-IDDC                           
115400     MOVE '4639'               TO 4539-MID-IDSYSTEM                       
115500     MOVE W-IDTRPTNR           TO 4539-MID-IDTRPTNR                       
115600                                                                          
115700     IF W-IDTRPTNR < +100                                                 
115800       CONTINUE                                                           
115900     ELSE                                                                 
116000       PERFORM IMS-PURG-TRANS4539                                         
116100     END-IF                                                               
116200     .                                                                    
116300     EJECT                                                                
116400 S09B-START-W40540  SECTION.                                              
116500                                                                          
116600     MOVE W-IDDISTR-PREV       TO W-IDDISTR-NUM                           
116700     MOVE W-IDDISTR-NUM        TO 4540-MID-IDDISTR                        
116800                                                                          
116900     IF FLSAMFAK-PREV                                                     
117000       MOVE ZERO               TO 4540-MID-IDKUNDNR                       
117100     ELSE                                                                 
117200       MOVE W-IDKUNDNR-PREV    TO W-IDKUNDNR-NUM                          
117300       MOVE W-IDKUNDNR-NUM     TO 4540-MID-IDKUNDNR                       
117400     END-IF                                                               
117500                                                                          
117600     MOVE MFS-KDMFSFOR         TO 4540-SPRAK                              
117700                                                                          
117800     IF W-IDTRPTNR < +100                                                 
117900       MOVE '17'               TO 4540-MID-KDFRAKT                        
118000       MOVE W-IDKUNDNR-PREV    TO W-IDKUNDNR-NUM                          
118100       MOVE W-IDKUNDNR-NUM     TO 4540-MID-IDKUNDNR                       
118200     ELSE                                                                 
118300       MOVE '99'               TO 4540-MID-KDFRAKT                        
118400     END-IF                                                               
118500                                                                          
118600     MOVE W-IDSHIPM            TO 4540-MID-IDSKEPPN                       
118700     MOVE W-IDDC               TO 4540-MID-IDDC                           
118800     MOVE '4639'               TO 4540-MID-IDSYSTEM                       
118900     MOVE W-IDTRPTNR           TO 4540-MID-IDTRPTNR                       
119000                                                                          
119100     IF W-IDTRPTNR < +100                                                 
119200       CONTINUE                                                           
119300     ELSE                                                                 
119400       PERFORM IMS-PURG-TRANS4540                                         
119500     END-IF                                                               
119600     .                                                                    
119700     EJECT                                                                
119800 S11-CHECK-FLSAMFAK SECTION.                                              
119900     SKIP2                                                                
120000*********************************************************                 
120100*                                                       *                 
120200* TO CHECK CUSTOMER FOR FLSAMFAK - CO-INVOICING         *                 
120300*                                                       *                 
120400*********************************************************                 
120500                                                                          
120600** FLSAMFAK IS SAME FOR ALL CUSTOMER IN ONE DISTRICT                      
120700     IF 4498-IDDISTR NOT = W-IDDISTR-PREV                                 
120800       MOVE NOO           TO FLSAMFAK-SW                                  
120900** FLSAMFAK IS ALWAYS 'N' FOR FOR IDKUNDNR = ZERO                         
121000       IF 4498-IDKUNDNR NOT = ZERO                                        
121100         MOVE 4498-IDDISTR  TO W-WDB201-IDDISTR                           
121200                               TEST-IDDISTR                               
121300         MOVE 4498-IDKUNDNR TO W-WDB201-IDKUNDNR                          
121400         PERFORM IMS-GU-WDB201                                            
121500         IF SEGMENT-FOUND                                                 
121600           IF DIST92-ITALY OR                                             
121700              DIST92-GREECE                                               
121800             MOVE NOO          TO GMT-FLSAMFAK                            
121900           END-IF                                                         
122000           IF GMT-FLSAMFAK = YES                                          
122100             MOVE YES  TO FLSAMFAK-SW                                     
122200           END-IF                                                         
122300         END-IF                                                           
122400       END-IF                                                             
122500     END-IF                                                               
122600     .                                                                    
122700     EJECT                                                                
122800                                                                          
122900 S15-CREATE-WDE111 SECTION.                                               
123000                                                                          
123100     MOVE  4498-IDDISTR     TO  SGMT-IDDISTR                              
123200     MOVE  4498-IDKUNDNR    TO  SGMT-IDKUNDNR                             
123300     MOVE  'N'              TO  SGMT-FLCOD                                
123400     MOVE  W-IDDC           TO  SGMT-IDDC                                 
123500     MOVE  SPACE            TO  SGMT-IDPARTNR                             
123600     MOVE  ZERO             TO  SGMT-KDFORSKN                             
123700                                SGMT-KDFKBIL                              
123800     MOVE  -1               TO  SGMT-KDLEVVIL                             
123900     MOVE  W-KDORDKL-MAX    TO  SGMT-KDORDKL-MAX                          
124000     MOVE  SPACE            TO  SGMT-KDVALISO                             
124100     MOVE  ZERO             TO  SGMT-PRKURS                               
124200     COMPUTE  SGMT-TISKEPPN-9KOMPL                                        
124300                            =   W-9KOMPL - SHIP-TISKEPPN                  
124400     PERFORM IMS-ISRT-WDE111                                              
124500**   ADD  1                   TO W-UPD-COUNT                              
124600     .                                                                    
124700     EJECT                                                                
124800                                                                          
124900 S16-CREATE-WDE211 SECTION.                                               
125000                                                                          
125100     MOVE 4498-IDDISTR      TO BGMT-IDDISTR                               
125200     MOVE 4498-IDKUNDNR     TO BGMT-IDKUNDNR                              
125300     MOVE SPACE             TO BGMT-IDPARTNR                              
125400                               BGMT-FLSEPINV                              
125500     MOVE 'N'               TO BGMT-FLCOD                                 
125600     MOVE ZERO              TO BGMT-KDLEVVIL                              
125700                               BGMT-PRAVDRAG                              
125800                               BGMT-PREMBHNT                              
125900                               BGMT-PRFOERS                               
126000                               BGMT-PRFRAKT                               
126100                               BGMT-PRLEGKST                              
126200                               BGMT-REAVDRAG                              
126300                               BGMT-REEMBHNT                              
126400                               BGMT-REFOERS                               
126500                               BGMT-RELEGKST                              
126600                               BGMT-REOVKOFF                              
126700                                                                          
126800     PERFORM IMS-ISRT-WDE211                                              
126900**   ADD  1                   TO W-UPD-COUNT                              
127000     .                                                                    
127100     EJECT                                                                
127200                                                                          
127300 S17-CREATE-WDE121 SECTION.                                               
127400                                                                          
127500     MOVE 4498-IDPRODNR          TO SKOLLI-IDPRODNR                       
127600     MOVE 4498-IDKOLLI           TO SKOLLI-IDKOLLI                        
127700     MOVE 4498-IDKOLLI-SAMP      TO SKOLLI-IDKOLLI-SAMP                   
127800     MOVE ZERO                   TO SKOLLI-DIKOLLIB                       
127900                                    SKOLLI-DIKOLLIH                       
128000                                    SKOLLI-DIKOLLIL                       
128100     MOVE SPACE                  TO SKOLLI-FLDIRLEV                       
128200     MOVE ZERO                   TO SKOLLI-IDORDER                        
128300                                    SKOLLI-KDEMBTYP                       
128400                                    SKOLLI-KDFRAKT                        
128500                                    SKOLLI-KDFARLIG-KOLLI                 
128600     MOVE SPACE                  TO SKOLLI-KDKOLLI                        
128700     MOVE ZERO                   TO SKOLLI-KVFLAMP-KOLLI                  
128800                                    SKOLLI-SUORDV-LOC                     
128900                                    SKOLLI-SUORDV-LOCPREL                 
129000     MOVE SPACE                  TO SKOLLI-KDVALISO                       
129100                                    SKOLLI-KDVALISO-EXP                   
129200     MOVE ZERO                   TO SKOLLI-SUORDV                         
129300                                    SKOLLI-SUORDV-EXP                     
129400                                    SKOLLI-TIPACKN                        
129500                                    SKOLLI-VKORDBTO-KOLLI                 
129600                                    SKOLLI-VKORDNTO-KOLLI                 
129700                                    SKOLLI-VLORDBTO-KOLLI                 
129800     MOVE 4498-IDDISTR           TO SKOLLI-IDDISTR                        
129900     MOVE 4498-IDKUNDNR          TO SKOLLI-IDKUNDNR                       
130000     MOVE 4498-IDKUNDRF          TO SKOLLI-IDKUNDRF                       
130100     MOVE 4498-KDFAKTYP          TO SKOLLI-KDFAKTYP                       
130110     MOVE 4498-FLCROSS           TO SKOLLI-FLCROSS                        
130200     MOVE -1                     TO SKOLLI-KDORDKL                        
130300     MOVE ZERO                   TO SKOLLI-TIORDREG                       
130400                                    SKOLLI-IDFAKT                         
130500                                    SKOLLI-IDFAKT-EXP                     
130600                                                                          
130700     PERFORM IMS-ISRT-WDE121                                              
130800**   ADD  1                   TO W-UPD-COUNT                              
130900     .                                                                    
131000     EJECT                                                                
131100                                                                          
131200 S18-CREATE-WDE221 SECTION.                                               
131300                                                                          
131400     MOVE 4498-IDPRODNR          TO BKOLLI-IDPRODNR                       
131500     MOVE 4498-IDKOLLI           TO BKOLLI-IDKOLLI                        
131600     MOVE 4498-IDDISTR           TO BKOLLI-IDDISTR                        
131700     MOVE 4498-IDKUNDNR          TO BKOLLI-IDKUNDNR                       
131800     MOVE 4498-IDKUNDRF          TO BKOLLI-IDKUNDRF                       
131810     MOVE 4498-FLCROSS           TO BKOLLI-FLCROSS                        
131900     MOVE SPACE                  TO BKOLLI-FLOVRLEV                       
132000                                    BKOLLI-KDFAKTYP                       
132100*                                   BKOLLI-BEKUNDRF                       
132200*                                   BKOLLI-BERADREF                       
132300     MOVE -1                     TO BKOLLI-KDORDKL                        
132400     MOVE SPACE                  TO BKOLLI-KDPRSTA                        
132500     MOVE ZERO                   TO BKOLLI-VKORDBTO-KOLLI                 
132600                                                                          
132700     PERFORM IMS-ISRT-WDE221                                              
132800**   ADD  1                   TO W-UPD-COUNT                              
132900     .                                                                    
133000     EJECT                                                                
133100                                                                          
133200 S21-OPEN-WZ01 SECTION.                                                   
133300                                                                          
133400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
133500     MOVE 'CARPARTS.PULS.ADDIT   '   TO SEND-ADDISPABS                    
133600     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
133700                                                                          
133800     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
133900                                SEND-OPEN-AREA                            
134000     IF SEND-KDRC > 0                                                     
134100       MOVE SEND-KDRC           TO KDRC-DISP                              
134200       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
134300            DELIMITED BY SIZE INTO ERROR-TEXT                             
134400       CALL FELLOG                                                        
134500     ELSE                                                                 
134600       MOVE SEND-IDCOM               TO WS-IDCOM                          
134700     END-IF                                                               
134800     .                                                                    
134900     EJECT                                                                
135000 S22-SEND-WZ01 SECTION.                                                   
135100     MOVE W-IDSHIPM          TO MOD4636-MID-IDSHIPM                       
135200     MOVE 'L'                TO MOD4636-MID-KDTRPINF                      
135300     MOVE ZERO               TO MOD4636-MID-IDDISTR                       
135400                                MOD4636-MID-IDKUNDNR                      
135500                                MOD4636-MID-IDPRODNR                      
135600                                MOD4636-MID-IDKOLLI                       
135700                                MOD4636-MID-SUORDV-DIST                   
135800                                MOD4636-MID-SUORDV-DIST-LOC               
135900                                MOD4636-MID-SUORDV-DIST-PREL              
136000                                MOD4636-MID-SUORDV-NOLL                   
136100                                MOD4636-MID-SUORDV-NOLL-LOC               
136200                                MOD4636-MID-SUORDV-NOLL-PREL              
136300                                MOD4636-MID-KDORDKL                       
136400     MOVE 'PUT'                      TO SEND-KDFUNC                       
136500     COMPUTE SEND-KVDLEN = LENGTH OF MOD4636-MID-W40636I1                 
136600                                                                          
136700     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
136800                                SEND-KVDLEN                               
136900                                MOD4636-MID-W40636I1                      
137000     IF SEND-KDRC > 0                                                     
137100       MOVE SEND-KDRC           TO KDRC-DISP                              
137200       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
137300            DELIMITED BY SIZE INTO ERROR-TEXT                             
137400       CALL FELLOG                                                        
137500     END-IF                                                               
137600     .                                                                    
137700     EJECT                                                                
137800 S23-CLOSE-WZ01  SECTION.                                                 
137900                                                                          
138000     MOVE 'CLOSE'               TO SEND-KDFUNC                            
138100                                                                          
138200     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
138300     IF SEND-KDRC > 0                                                     
138400       MOVE SEND-KDRC           TO KDRC-DISP                              
138500       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
138600            DELIMITED BY SIZE INTO ERROR-TEXT                             
138700       CALL FELLOG                                                        
138800     END-IF                                                               
138900     .                                                                    
139000     EJECT                                                                
139100                                                                          
139200 S25-WRONG-PICTURE-MESSAGE SECTION.                                       
139300     SKIP2                                                                
139400* *****************************************************                   
139500*                                                     *                   
139600* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
139700*                                                     *                   
139800* *****************************************************                   
139900     SKIP2                                                                
140000     MOVE 'W0O50401'          TO MFS-IDMOD                                
140100     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
140200     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
140300     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
140400     PERFORM IMS-INSERT-MSG                                               
140500     .                                                                    
140600     EJECT                                                                
140700 MFS-ERASE-FIELD-OUT  SECTION.                                            
140800                                                                          
140900     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-UT                              
141000                             MOD-IDLBBET-UT                               
141100                             MOD-FLFARLIG-UT                              
141200                             MOD-IDDC-UT                                  
141300     .                                                                    
141400     SKIP3                                                                
141500                                                                          
141600 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
141700                                                                          
141800     MOVE 1 TO INDX                                                       
141900     MOVE MFS-ERASE-FIELD TO MOD-FLAVSLUTA                                
142000                             MOD-FLSKRIV-NU                               
142100     .                                                                    
142200 MFS-ERASE-FIELD-IN SECTION.                                              
142300                                                                          
142400*    --- ALLA INDATA-FÄLT                                                 
142500     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-IN                              
142600                             MOD-IDLBBET-IN                               
142700                             MOD-FLFARLIG-IN                              
142800                             MOD-IDDC-IN                                  
142900     .                                                                    
143000     EJECT                                                                
143100 MFS-FORM-ATTR SECTION.                                                   
143200                                                                          
143300*    --- ALL INDATA-FIELDS                                                
143400     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-FLAVSLUTA                        
143500     .                                                                    
143600     SKIP2                                                                
143700* --- IMS SECTIONS ---                                                    
143800     SKIP3                                                                
143900 IMS-GET-MSG SECTION.                                                     
144000                                                                          
144100     MOVE '  QC' TO GOOD-STATUSCODES                                      
144200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
144300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
144400     PERFORM IMS-STATUSCHECK                                              
144500     .                                                                    
144600     SKIP3                                                                
144700 IMS-INSERT-MSG SECTION.                                                  
144800                                                                          
144900     MOVE LOW-VALUE           TO MSG-KDZ1 MSG-KDZ2                        
145000     MOVE SPACE               TO GOOD-STATUSCODES                         
145100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
145200     MOVE MSG-STATUS-CODE     TO STATUS-WS                                
145300     PERFORM IMS-STATUSCHECK                                              
145400     .                                                                    
145500     EJECT                                                                
145600                                                                          
145700 IMS-ISRT-ALT-MSG-4639X  SECTION.                                         
145800                                                                          
145900     MOVE SPACE               TO GOOD-STATUSCODES                         
146000     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
146100     MOVE ALT-STATUS-CODE     TO STATUS-WS                                
146200     PERFORM IMS-STATUSCHECK                                              
146300     .                                                                    
146400     EJECT                                                                
146500 IMS-PURG-TRANS4539 SECTION.                                              
146600                                                                          
146700     MOVE LOW-VALUE            TO 4539-Z1                                 
146800                                  4539-Z2                                 
146900     MOVE '    '               TO GOOD-STATUSCODES                        
147000     CALL CBLTDLI USING PURG ALT3-PCB 4539-MSG-IO-AREA                    
147100     MOVE ALT3-STATUS-CODE     TO STATUS-WS                               
147200     PERFORM IMS-STATUSCHECK                                              
147300     .                                                                    
147400     EJECT                                                                
147500 IMS-PURG-TRANS4540 SECTION.                                              
147600                                                                          
147700     MOVE LOW-VALUE            TO 4540-Z1                                 
147800                                  4540-Z2                                 
147900     MOVE '    '               TO GOOD-STATUSCODES                        
148000     CALL CBLTDLI USING PURG ALT4-PCB 4540-MSG-IO-AREA                    
148100     MOVE ALT4-STATUS-CODE     TO STATUS-WS                               
148200     PERFORM IMS-STATUSCHECK                                              
148300     .                                                                    
148400     EJECT                                                                
148500 IMS-GHU-WDR401-4495 SECTION.                                             
148600                                                                          
148700     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
148800                      DELIMITED BY SIZE INTO SSA1                         
148900     MOVE '  GE' TO GOOD-STATUSCODES                                      
149000     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-WDGX4495 SSA1                 
149100     MOVE 4495-STATUS-CODE     TO STATUS-WS                               
149200     PERFORM IMS-STATUSCHECK                                              
149300     .                                                                    
149400     SKIP2                                                                
149500                                                                          
149600 IMS-GHNP-WDGX4498    SECTION.                                            
149700                                                                          
149800     MOVE 'WDGX4498' TO SSA1                                              
149900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
150000     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-WDGX4498  SSA1               
150100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
150200     PERFORM IMS-STATUSCHECK                                              
150300     .                                                                    
150400     SKIP2                                                                
150500 IMS-ISRT-WDGX4514 SECTION.                                               
150600                                                                          
150700     STRING 'WDR401  (WDGXKEY  =' W-4513-X ')'                            
150800             DELIMITED BY SIZE INTO SSA1                                  
150900     MOVE 'WDGX4514' TO SSA2                                              
151000     MOVE '  II' TO GOOD-STATUSCODES                                      
151100     CALL CBLTDLI USING ISRT 4513-PCB DLI-IO-WDGX4514                     
151200                             SSA1 SSA2                                    
151300     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
151400     PERFORM IMS-STATUSCHECK                                              
151500     .                                                                    
151600     SKIP2                                                                
151700 IMS-ISRT-WDGX4516 SECTION.                                               
151800                                                                          
151900     STRING 'WDR401  (WDGXKEY  =' W-4513-X ')'                            
152000             DELIMITED BY SIZE INTO SSA1                                  
152100     STRING 'WDGX4514(DASKEPPN =' W-4514-X ')'                            
152200             DELIMITED BY SIZE INTO SSA2                                  
152300     MOVE 'WDGX4516' TO SSA3                                              
152400     MOVE '  ' TO GOOD-STATUSCODES                                        
152500     CALL CBLTDLI USING ISRT 4513-PCB DLI-IO-WDGX4516                     
152600                             SSA1 SSA2 SSA3                               
152700     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
152800     PERFORM IMS-STATUSCHECK                                              
152900     .                                                                    
153000     EJECT                                                                
153100 IMS-GHU-WDE601 SECTION.                                                  
153200                                                                          
153300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
153400            DELIMITED BY SIZE INTO SSA1                                   
153500     MOVE '  ' TO GOOD-STATUSCODES                                        
153600     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
153700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
153800     PERFORM IMS-STATUSCHECK                                              
153900     .                                                                    
154000     EJECT                                                                
154100 IMS-GHU-WDE611 SECTION.                                                  
154200                                                                          
154300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
154400            DELIMITED BY SIZE INTO SSA1                                   
154500     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
154600            DELIMITED BY SIZE INTO SSA2                                   
154700     MOVE '  GE' TO GOOD-STATUSCODES                                      
154800     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
154900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
155000     PERFORM IMS-STATUSCHECK                                              
155100     .                                                                    
155200     EJECT                                                                
155300 IMS-GU-WDB201 SECTION.                                                   
155400                                                                          
155500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
155600          DELIMITED BY SIZE INTO SSA1                                     
155700     MOVE '  GE' TO GOOD-STATUSCODES                                      
155800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
155900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
156000     PERFORM IMS-STATUSCHECK                                              
156100     .                                                                    
156200     SKIP3                                                                
156300 IMS-GHU-WDE101 SECTION.                                                  
156400                                                                          
156500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
156600          DELIMITED BY SIZE INTO SSA1                                     
156700     MOVE '  ' TO GOOD-STATUSCODES                                        
156800     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
156900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
157000     PERFORM IMS-STATUSCHECK                                              
157100     .                                                                    
157200     EJECT                                                                
157300 IMS-REPL-WDE101 SECTION.                                                 
157400                                                                          
157500     MOVE '  ' TO GOOD-STATUSCODES                                        
157600     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE101                       
157700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
157800     PERFORM IMS-STATUSCHECK                                              
157900     .                                                                    
158000     EJECT                                                                
158100 IMS-ISRT-WDE101 SECTION.                                                 
158200                                                                          
158300     MOVE 'WDE101  ' TO SSA1                                              
158400     MOVE '  II' TO GOOD-STATUSCODES                                      
158500     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
158600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
158700     PERFORM IMS-STATUSCHECK                                              
158800     .                                                                    
158900     EJECT                                                                
159000 IMS-ISRT-WDE111 SECTION.                                                 
159100                                                                          
159200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
159300          DELIMITED BY SIZE INTO SSA1                                     
159400     MOVE 'WDE111  ' TO SSA2                                              
159500     MOVE '  II' TO GOOD-STATUSCODES                                      
159600     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
159700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
159800     PERFORM IMS-STATUSCHECK                                              
159900     .                                                                    
160000     EJECT                                                                
160100 IMS-ISRT-WDE121 SECTION.                                                 
160200                                                                          
160300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
160400          DELIMITED BY SIZE INTO SSA1                                     
160500     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
160600          DELIMITED BY SIZE INTO SSA2                                     
160700     MOVE 'WDE121 ' TO SSA3                                               
160800     MOVE '  II' TO GOOD-STATUSCODES                                      
160900     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3        
161000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
161100     PERFORM IMS-STATUSCHECK                                              
161200     .                                                                    
161300     EJECT                                                                
161400 IMS-GHU-WDE201 SECTION.                                                  
161500                                                                          
161600     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
161700          DELIMITED BY SIZE INTO SSA1                                     
161800     MOVE '  ' TO GOOD-STATUSCODES                                        
161900     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
162000     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
162100     PERFORM IMS-STATUSCHECK                                              
162200     .                                                                    
162300     EJECT                                                                
162400                                                                          
162500 IMS-ISRT-WDE201 SECTION.                                                 
162600                                                                          
162700     MOVE 'WDE201  ' TO SSA1                                              
162800     MOVE '  II' TO GOOD-STATUSCODES                                      
162900     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
163000     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
163100     PERFORM IMS-STATUSCHECK                                              
163200     .                                                                    
163300     EJECT                                                                
163400                                                                          
163500 IMS-ISRT-WDE211 SECTION.                                                 
163600                                                                          
163700     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
163800          DELIMITED BY SIZE INTO SSA1                                     
163900     MOVE 'WDE211  ' TO SSA2                                              
164000     MOVE '  II' TO GOOD-STATUSCODES                                      
164100     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
164200     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
164300     PERFORM IMS-STATUSCHECK                                              
164400     .                                                                    
164500     EJECT                                                                
164600                                                                          
164700 IMS-ISRT-WDE221 SECTION.                                                 
164800                                                                          
164900     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
165000          DELIMITED BY SIZE INTO SSA1                                     
165100     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
165200          DELIMITED BY SIZE INTO SSA2                                     
165300     MOVE 'WDE221  ' TO SSA3                                              
165400     MOVE '  II' TO GOOD-STATUSCODES                                      
165500     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3        
165600     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
165700     PERFORM IMS-STATUSCHECK                                              
165800     .                                                                    
165900     EJECT                                                                
166000 IMS-GU-WDB601    SECTION.                                                
166100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
166200          DELIMITED BY SIZE INTO SSA1                                     
166300     MOVE '  GE' TO GOOD-STATUSCODES                                      
166400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
166500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
166600     PERFORM IMS-STATUSCHECK                                              
166700     IF SEGMENT-MISSING                                                   
166800         MOVE SPACE TO DCS-KDDC                                           
166900                       DCS-IDLANDX2                                       
167000     END-IF                                                               
167100     .                                                                    
167200 IMS-STATUSCHECK SECTION.                                                 
167300                                                                          
167400     SET STATUS-IX TO 1                                                   
167500     SEARCH GOOD-STATUS                                                   
167600       AT END                                                             
167700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
167800         DELIMITED BY SIZE INTO ERROR-TEXT                                
167900         CALL FELLOG                                                      
168000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
168100         CONTINUE                                                         
168200     END-SEARCH                                                           
169000     .                                                                    
