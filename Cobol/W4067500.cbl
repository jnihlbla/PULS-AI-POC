000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4067500.                                                
000400 AUTHOR.         KARANDE DIGAMBAR.                                        
000500 DATE-WRITTEN.   02/07/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM IS A DDI VERSION OF W4066500. MANY OF THE           
001000*        FUNCTIONS IN PROGRAM W4066500 ARE ALSO VALID FOR THIS            
001100*        PROGRAM.THIS PROGRAM STARTS BUILDING THE TWO NEW DATABASE        
001200*        WDE1 AND WDE2 FOR SHIPPING AND INVOICING.                        
001300*        THIS PROGRAM RUN IN THE BACKGROUND OR AS A SCREEN. SCREEN        
001400*        ALLOWS TO START 4676 WHICH UPDATE THE INFORMATION ON             
001500*        WDE122 AND WDE211.                                               
001600*        THIS PROGRAM IS STARTED BY W4066400.                             
001700*                                                                         
001800*                                                                         
001900*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
002000*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0188               
002100*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
002200*                                                                         
002300*        THE PROGRAM UPDATES   WDE6                                       
002400*        THE PROGRAM UPDATES   WDE1                                       
002500*        THE PROGRAM UPDATES   WDE2                                       
002600*        THE PROGRAM READS     WDR4                                       
002700*        THE PROGRAM READS     WDR1                                       
002800*        THE PROGRAM READS     WDB6                                       
002900*                                                                         
003000*    INDATA.                                                              
003100*        TRANSACTION: W4T675                                              
003200*        MID:         W4I67501                                            
003300*                                                                         
003400*    OUTDATA.                                                             
003500*        MOD:         W4O67501                                            
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'W4067500'.            
004400                                                                          
004500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004700                                                                          
004800 77  YES                         PIC X       VALUE 'J'.                   
004900 77  YES-Y                       PIC X       VALUE 'Y'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
005300                                                                          
005400*    --- INDEX FOR SCROLL LINES                                           
005500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
005700 77  IDPSN-IX                    PIC S9(3)  VALUE +0    COMP SYNC.        
005800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0 COMP SYNC.           
005900 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
006000 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-WRONG                        VALUE 'N'.                   
006500                                                                          
006600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006700     88  KEYS-OK                             VALUE 'J'.                   
006800     88  KEYS-WRONG                          VALUE 'N'.                   
006900                                                                          
007000 77  INPUT-SW                    PIC X       VALUE 'N'.                   
007100     88  INPUT-GIVEN                         VALUE 'J'.                   
007200                                                                          
007300 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
007400     88  KDCMD-GIVEN                         VALUE 'J'.                   
007500                                                                          
007600 77  4498-MISSING-SW             PIC X       VALUE 'N'.                   
007700     88  4498-MISSING                        VALUE 'J'.                   
007800                                                                          
007900 77  START-4540-4537-SW          PIC X       VALUE 'N'.                   
008000     88  START-4540-4537                     VALUE 'J'.                   
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  OWN-MID                             VALUE '4675'.                
008400     88  GOOD-MID                            VALUE '466D'                 
008500                                                   '4698'                 
008600                                                   '4675'                 
008700                                                   '4676'.                
008800** SCREEN 4664 START 4675 USING IDTRANS '466D'                            
008900     88  466D-MID                            VALUE '466D'.                
009000     88  4698-MID                            VALUE '4698'.                
009100     88  4676-MID                            VALUE '4676'.                
009200     88  HELP-MID                            VALUE '0551'.                
009300                                                                          
009400 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
009500 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
009600                                                                          
009700 01  W-IDDCTEXT-MSGI.                                                     
009800     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
009900     03  W-IDDC-MSGI         PIC X(2).                                    
010000                                                                          
010100       EJECT                                                              
010200 77  UPDATE-SAVE-SW              PIC X(01)   VALUE 'N'.                   
010300     88  UPDATE-SAVE                         VALUE 'J'.                   
010400                                                                          
010500 77  WRITE-DHL-SW                PIC X       VALUE 'N'.                   
010600     88  WRITE-DHL                           VALUE 'J'.                   
010700                                                                          
010800 77  RESTART-SW                  PIC X(01)   VALUE 'N'.                   
010900     88 RESTART                              VALUE 'J'.                   
011000                                                                          
011100 77  FIRST-PRODNR-SW             PIC X(01)   VALUE 'N'.                   
011200     88 FIRST-PRODNR                         VALUE 'J'.                   
011300                                                                          
011400 77  FIRST-KUNDNR-SW             PIC X(01)   VALUE 'N'.                   
011500     88 SW-FIRST-KUNDNR                      VALUE 'J'.                   
011600                                                                          
011700 77  FIRST-DEALER-SW             PIC X(01)   VALUE 'N'.                   
011800     88 SW-FIRST-DEALER                      VALUE 'J'.                   
011900                                                                          
012000 77  FLSAMFAK-SW                 PIC X(01)   VALUE 'N'.                   
012100     88 FLSAMFAK                             VALUE 'J'.                   
012200                                                                          
012300 77  FLSAMFAK-PREV-SW            PIC X(01)   VALUE 'N'.                   
012400     88 FLSAMFAK-PREV                        VALUE 'J'.                   
012500                                                                          
012600 77  FARLIGT-GOOD-4539           PIC X(01)   VALUE 'N'.                   
012700     88 FARLIGT-GOOD-FOUND-4539              VALUE 'J'.                   
012800                                                                          
012900 77  FARLIGT-GOOD-4540           PIC X(01)   VALUE 'N'.                   
013000     88 FARLIGT-GOOD-FOUND-4540              VALUE 'J'.                   
013100                                                                          
013200 77  W-FLAVSLUTA-SW              PIC X(01)   VALUE ' '.                   
013300     88 FLAVSLUTA-OK                         VALUE 'Y' 'J'.               
013400     EJECT                                                                
013500*  TO CONVERT THE YYMMDD  TO CCYYMMDD                                     
013600 01  W-YYMMDD-DATUM              PIC 9(6).                                
013700 01  W-CCYYMMDD-DATUM.                                                    
013800     03 W-CC                     PIC 9(2).                                
013900     03 W-YYMMDD.                                                         
014000        05 W-YY                  PIC 9(2).                                
014100        05 W-MM                  PIC 9(2).                                
014200        05 W-DD                  PIC 9(2).                                
014300                                                                          
014400 77  W-UPD-COUNT                 PIC S9(3)   VALUE ZERO.                  
014500 77  W-UPD-MAX                   PIC S9(3)   VALUE +100.                  
014600 77  W-IDDISTR-PREV              PIC S9(5)   COMP-3 VALUE ZERO.           
014700 77  W-IDKUNDNR-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
014800 77  W-IDKUNDNR-CHANGE           PIC S9(7)   COMP-3 VALUE ZERO.           
014900 77  W-IDDEALER-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
015000 77  W-IDPRODNR-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
015100 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
015200                                                                          
015300 77  W-FLFARLIG                  PIC X(01)   VALUE SPACE.                 
015400 77  W-FLSKRIV-NU                PIC X(01)   VALUE SPACE.                 
015500 77  W-FLAVSLUTA                 PIC X(01)   VALUE SPACE.                 
015600                                                                          
015700 77  W-IDDISTR-NUM               PIC  9(4)   VALUE ZERO.                  
015800 77  W-IDKUNDNR-NUM              PIC  9(6)   VALUE ZERO.                  
015900 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
016000 77  W-VORD-KDFRAKT              PIC S9(3)   COMP-3 VALUE ZERO.           
016100                                                                          
016200 77  W-KDORDKL-MAX               PIC S9(1)      COMP-3 VALUE ZERO.        
016300                                                                          
016400 01  FILLER                      PIC X(25)                                
016500                                      VALUE 'FOR WDE6 UPDATE'.            
016600 01  FILLER.                                                              
016700     03  KLI-PACK                PIC S9(1) COMP-3 VALUE +1.               
016800     03  KLI-PACK-FAKT           PIC S9(1) COMP-3 VALUE +6.               
016900                                                                          
017000     03 WS-TISKPTID              PIC 9(6)       VALUE ZERO.               
017100     03 WS-TISKPTID-GRP          REDEFINES WS-TISKPTID.                   
017200        05 WS-TISKPTID-HHMM         PIC 9(4).                             
017300        05 WS-TISKPTID-SS           PIC 9(2).                             
017400                                                                          
017500     EJECT                                                                
017600                                                                          
017700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
017800 01  GENERAL-SUBPROGRAMS.                                                 
017900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
018000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018400     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
018500     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
018600     EJECT                                                                
018700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
018800*01 -COPY WMEDAREA                                                        
018900     SKIP3                                                                
019000 01  FILLER.                                                              
019100   03  FELMEDD-AREA.                                                      
019200     05  FELMEDD-ENGLISH.                                                 
019300       10  FILLER                PIC X(40)                                
019400           VALUE '622 4675 WRONG PICTURE SELECTED         '.              
019500                                                                          
019600 01  MESSAGE-CODES.                                                       
019700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
019800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019900     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
020000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
020100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
020300     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
020400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020500     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
020600     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
020700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020800     EJECT                                                                
020900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
021000*                                                                         
021100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
021200     SKIP3                                                                
021300*01 -COPY WMSGINIT                                                        
021400     EJECT                                                                
021500 01  FILLER                      PIC X(25)   VALUE 'DC-WMSGINIT'.         
021600     SKIP3                                                                
021700*01 -COPY WMSGINIT     -PRE  DC-                                          
021800     EJECT                                                                
021900*    --- AREA  FOR WZ01  ------                                           
022000 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
022100*01  -COPY WZ01SEND                                                       
022200                                                                          
022300*    --- AREA  FOR W476SHNO ---                                           
022400 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
022500*01  -COPY W476SHNO                                                       
022600                                                                          
022700*01    -COPY WDECAREA                                                     
022800     EJECT                                                                
022810*    --- VALID IDDC CODES ---                                             
022830*01    -COPY WWDC99                                                       
022900                                                                          
023000 01  TEST-IDDISTR                PIC  9(5)  COMP-3.                       
023100*01  FILLER     -COPY WWDIST07    -RED TEST-IDDISTR.                      
023200     SKIP2                                                                
023300*01  FILLER     -COPY WWDIST13    -RED TEST-IDDISTR.                      
023400     SKIP2                                                                
023500*01  FILLER     -COPY WWDIST35    -RED TEST-IDDISTR.                      
023600     SKIP2                                                                
023700*01  FILLER     -COPY WWDIST92    -RED TEST-IDDISTR.                      
023800     SKIP2                                                                
023900     EJECT                                                                
024000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
024100*                                                                         
024200 01  SAVE-AREA.                                                           
024300     03  SAVE-IDTRANS            PIC X(4)    VALUE '4675'.                
024400* COMMON AREA FOR 4675 AND 4676                                           
024500     03  SAVE-4675-4676.                                                  
024600         05  SAVE-IDDISTR-ENTER      PIC S9(5).                           
024700         05  SAVE-IDKUNDNR-ENTER     PIC S9(7).                           
024800         05  SAVE-KDFAKTYP-ENTER     PIC X(01).                           
024900         05  SAVE-IDKUNDRF-ENTER     PIC X(10).                           
025000         05  SAVE-IDPRODNR-ENTER     PIC S9(07).                          
025100         05  SAVE-IDKOLLI-ENTER      PIC S9(05).                          
025200         05  SAVE-IDDISTR-4676       PIC S9(5).                           
025300         05  SAVE-IDKUNDNR-4676      PIC S9(7).                           
025400         05  SAVE-FLSAMFAK-4676      PIC X(01)   VALUE SPACE.             
025500         05  SAVE-IDSHIPM            PIC  9(7).                           
025600         05  SAVE-FLFARLIG           PIC X(01).                           
025700         05  SAVE-FLSKRIV-NU         PIC X(01).                           
025800         05  SAVE-FLAVSLUTA          PIC X(01).                           
025900         05  SAVE-REL-TRANSPORT      PIC X(01)   VALUE 'N'.               
026000             88  REL-TRANSPORT                   VALUE 'J'.               
026100*                                                                         
026200     03  SAVE-IDDISTR-NEXT       PIC S9(5).                               
026300     03  SAVE-IDKUNDNR-NEXT      PIC S9(7).                               
026400     03  SAVE-KDFAKTYP-NEXT      PIC X(01).                               
026500     03  SAVE-IDKUNDRF-NEXT      PIC X(10).                               
026600     03  SAVE-IDPRODNR-NEXT      PIC S9(07).                              
026700     03  SAVE-IDKOLLI-NEXT       PIC S9(05).                              
026800*                                                                         
026900     03  SAVE-INPUT  OCCURS 14.                                           
027000         05  SAVE-RECORD-PRESENT PIC X(01)   VALUE SPACE.                 
027100         05  SAVE-IDDISTR        PIC S9(05).                              
027200         05  SAVE-IDKUNDNR       PIC S9(07).                              
027300         05  SAVE-PRFOERS        PIC S9(7)V9(2)    COMP-3.                
027400         05  SAVE-REFOERS        PIC S9(2)V9(3)    COMP-3.                
027500         05  SAVE-REOVKOFF       PIC S9(2)V9(1)    COMP-3.                
027600         05  SAVE-FLSAMFAK       PIC X(01)   VALUE SPACE.                 
027700     EJECT                                                                
027800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
027900*                                                                         
028000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
028100     SKIP3                                                                
028200*01  MID -COPY W4I67501                                                   
028300     EJECT                                                                
028400 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
028500     SKIP3                                                                
028600*01  -COPY WMSGAREA                                                       
028700     EJECT                                                                
028800*   TO RETURN TO MAIN MENU                                                
028900*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
029000     EJECT                                                                
029100     03  MOD REDEFINES MSG-AREA.                                          
029200*      05  -COPY W4O67501                                                 
029300     EJECT                                                                
029400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
029500     SKIP3                                                                
029600*01  -COPY WMFSAREA                                                       
029700     EJECT                                                                
029800*01  -COPY W40636I1   -PRE MOD4636-                                       
029900     EJECT                                                                
030000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
030100 01      P-TO-P-SW.                                                       
030200                                                                          
030300  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
030400  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
030500  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
030600  02     P-TO-P-KDTRANS          PIC X(8).                                
030700  02     P-TO-P-IDTRANS          PIC X(4).                                
030800  02     P-TO-P-KDMFSFOR         PIC X(1).                                
030900  02     P-TO-P-DATA             PIC X(1000).                             
031000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
031100*                                                                         
031200*01  -COPY W4I66401   -PRE MOD4664-                                       
031300*                                                                         
031400*                                                                         
031500*01  -COPY W4I67601   -PRE PTOP6-                                         
031600*                                                                         
031700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031800     SKIP3                                                                
031900 01  KEYS-TO-DLI.                                                         
032000*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
032100     03  W-4495-X.                                                        
032200         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
032300         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
032400         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
032500         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
032600         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
032700                                                                          
032800     03  W-4498-X.                                                        
032900         05  W-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
033000         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
033100         05  W-KDFAKTYP          PIC  X(1)   VALUE SPACE.                 
033200         05  W-IDKUNDRF          PIC  X(10).                              
033300         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
033400           07  W-IDORDNR7        PIC  9(7).                               
033500           07  FILLER            PIC  X(3).                               
033600         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
033700         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
033800                                                                          
033900     03  W-4498-N-X.                                                      
034000         05  W-IDDISTR-N         PIC S9(5)   VALUE ZERO  COMP-3.          
034100         05  W-IDKUNDNR-N        PIC S9(7)   VALUE ZERO  COMP-3.          
034200         05  FILLER              PIC X(18)   VALUE HIGH-VALUE.            
034300                                                                          
034400     03  W-IDDC-X.                                                        
034500         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
034600                                                                          
034700     03  W-4463-X.                                                        
034800         05  W-IDHTR-4463        PIC X(4)    VALUE '4463'.                
034900         05  W-IDDC-4463         PIC X(2)    VALUE SPACE.                 
035000         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
035100                                                                          
035200     03  W-DASKEPPN-X.                                                    
035300         05  W-DASKEPPN-4464     PIC  9(8)   VALUE ZERO.                  
035400                                                                          
035500     03  W-4466-X.                                                        
035600         05  W-IDTRPTNR-4466     PIC S9(3)   VALUE ZERO   COMP-3.         
035700         05  W-IDLBBET-4466      PIC X(12)   VALUE SPACE.                 
035800                                                                          
035900     03  W-4468-X.                                                        
036000         05  W-IDDISTR-4468      PIC S9(5)   VALUE ZERO   COMP-3.         
036100         05  W-IDKUNDNR-4468     PIC S9(7)   VALUE ZERO   COMP-3.         
036200         05  W-IDKUNDRF-4468     PIC X(10).                               
036300         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF-4468.         
036400             07  W-IDORDNR7-4468 PIC 9(07).                               
036500             07  FILLER          PIC X(03).                               
036600         05  W-IDPRODNR-4468     PIC S9(7)   VALUE ZERO  COMP-3.          
036700         05  W-IDKOLLI-4468      PIC S9(5)   VALUE ZERO  COMP-3.          
036800                                                                          
036900     03  W-4513-X.                                                        
037000         05  W-IDHTYP            PIC X(4)    VALUE '4513'.                
037100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
037200                                                                          
037300     03  W-4514-X.                                                        
037400         05  W-DASKEPPN-4514     PIC 9(8)    VALUE ZERO.                  
037500                                                                          
037600     03  W-IDPRODNR-X.                                                    
037700         05  W-IDPRODNR-KOLLI    PIC S9(7)   VALUE ZERO COMP-3.           
037800                                                                          
037900     03  W-IDKOLLI-X.                                                     
038000         05  W-IDKOLLI-KOLLI     PIC S9(5)   VALUE ZERO  COMP-3.          
038100                                                                          
038200     03  W-IDSHIPM-X.                                                     
038300         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
038400                                                                          
038500     03  W-WDE111KY-X.                                                    
038600         05  W-WDE111-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
038700         05  W-WDE111-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
038800                                                                          
038900     03  W-WDE211KY-X.                                                    
039000         05  W-WDE211-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
039100         05  W-WDE211-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
039200                                                                          
039300     03  W-IDGMT-X.                                                       
039400         05  W-WDB201-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
039500         05  W-WDB201-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
039600                                                                          
039700     03  W-IDDC-B6-X.                                                     
039800         05 W-IDDC-B6                  PIC X(2).                          
039900                                                                          
040000     SKIP2                                                                
040100*    --- STATUS-KOD FRÅN IMS                                              
040200 01  STATUS-WS                   PIC XX.                                  
040300     88  SEGMENT-FOUND                       VALUE '  '.                  
040400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
040500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
040600     SKIP2                                                                
040700 01  GOOD-STATUSCODES.                                                    
040800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040900     SKIP3                                                                
041000 01  SSA1                        PIC X(128).                              
041100 01  SSA2                        PIC X(128).                              
041200 01  SSA3                        PIC X(128).                              
041300 01  SSA4                        PIC X(128).                              
041400     EJECT                                                                
041500*    --- IMS FUNCTION CODES                                               
041600*01  -COPY W0003                                                          
041700     EJECT                                                                
041800*    ---  DLI INPUT-OUTPUT AREA                                           
041900                                                                          
042000*                                                                         
042100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4495'.                    
042200 01  DLI-IO-WDGX4495.                                                     
042300*    03  -COPY WDGX4495                                                   
042400                                                                          
042500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4498'.                    
042600 01  DLI-IO-WDGX4498.                                                     
042700*    03  -COPY WDGX4498                                                   
042800                                                                          
042900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4463'.                    
043000 01  DLI-IO-WDGX4463.                                                     
043100*    03  -COPY WDGX4463                                                   
043200                                                                          
043300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4464'.                    
043400 01  DLI-IO-WDGX4464.                                                     
043500*    03  -COPY WDGX4464                                                   
043600                                                                          
043700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4466'.                    
043800 01  DLI-IO-WDGX4466.                                                     
043900*    03  -COPY WDGX4466                                                   
044000                                                                          
044100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4468'.                    
044200 01  DLI-IO-WDGX4468.                                                     
044300*    03  -COPY WDGX4468                                                   
044400                                                                          
044500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4514'.                    
044600 01  DLI-IO-WDGX4514.                                                     
044700*    03  -COPY WDGX4514                                                   
044800                                                                          
044900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4516'.                    
045000 01  DLI-IO-WDGX4516.                                                     
045100*    03  -COPY WDGX4516                                                   
045200                                                                          
045300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
045400 01  DLI-IO-WDE601.                                                       
045500*    03  -COPY WDE601                                                     
045600     EJECT                                                                
045700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
045800 01  DLI-IO-WDE611.                                                       
045900*    03  -COPY WDE611                                                     
046000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
046100 01  DLI-IO-WDE101.                                                       
046200*    03  -COPY WDE101                                                     
046300     EJECT                                                                
046400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
046500 01  DLI-IO-WDE111.                                                       
046600*    03  -COPY WDE111                                                     
046700     EJECT                                                                
046800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
046900 01  DLI-IO-WDE121.                                                       
047000*    03  -COPY WDE121                                                     
047100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
047200 01  DLI-IO-WDE201.                                                       
047300*    03  -COPY WDE201                                                     
047400     EJECT                                                                
047500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE211'.                      
047600 01  DLI-IO-WDE211.                                                       
047700*    03  -COPY WDE211                                                     
047800     EJECT                                                                
047900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE221'.                      
048000 01  DLI-IO-WDE221.                                                       
048100*    03  -COPY WDE221                                                     
048200     EJECT                                                                
048300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
048400 01  DLI-IO-WDB201.                                                       
048500*    03  -COPY WDB201                                                     
048600                                                                          
048700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
048800 01   DLI-IO-AREA-B601.                                                   
048900*     03  -COPY WDB601                                                    
049000     EJECT                                                                
049100*---MSG-AREA FOR 4539 FOR FARL.GOOD                                       
049200 01  FILLER                PIC X(16)  VALUE '4539-MSG-IO-AREA'.           
049300 01  4539-MSG-IO-AREA.                                                    
049400     03  4539-LL              PIC S9(4)  VALUE +49 COMP SYNC.             
049500     03  4539-Z1              PIC X.                                      
049600     03  4539-Z2              PIC X.                                      
049700     03  4539-TRANSKOD        PIC X(8)   VALUE 'W4T539X '.                
049800     03  4539-IDTRANS         PIC X(4)   VALUE '4675'.                    
049900     03  4539-SPRAK           PIC X.                                      
050000*    03  -COPY W4I53901   -PRE 4539-                                      
050100     EJECT                                                                
050200*---MSG-AREA FOR 4540 FOR FARL.GOOD                                       
050300 01  FILLER                PIC X(16)  VALUE '4540-MSG-IO-AREA'.           
050400 01  4540-MSG-IO-AREA.                                                    
050500     03  4540-LL              PIC S9(4)  VALUE +49 COMP SYNC.             
050600     03  4540-Z1              PIC X.                                      
050700     03  4540-Z2              PIC X.                                      
050800     03  4540-TRANSKOD        PIC X(8)   VALUE 'W4T540X '.                
050900     03  4540-IDTRANS         PIC X(4)   VALUE '4675'.                    
051000     03  4540-SPRAK           PIC X.                                      
051100*    03  -COPY W4I54001   -PRE 4540-                                      
051200     EJECT                                                                
051300 LINKAGE SECTION.                                                         
051400*01  -COPY W0009  -PRE MSG-                                               
051500     EJECT                                                                
051600*01  -COPY W0009  -PRE AD36-                                              
051700     EJECT                                                                
051800*01  -COPY W0009  -PRE ALT-                                               
051900     EJECT                                                                
052000*01  -COPY W0009  -PRE ALT1-                                              
052100     EJECT                                                                
052200*01  -COPY W0009  -PRE ALT2-                                              
052300     EJECT                                                                
052400*01  -COPY W0009  -PRE ALT3-                                              
052500     EJECT                                                                
052600*01  -COPY W0009  -PRE ALT4-                                              
052700     EJECT                                                                
052800*01  -COPY W0009  -PRE ALT6-                                              
052900     EJECT                                                                
053000*01  -COPY W0008  -PRE WDP7-                                              
053100     05  FILLER                  PIC X.                                   
053200                                                                          
053300*01  -COPY W0008  -PRE WDE6-                                              
053400     05  FILLER                  PIC X.                                   
053500                                                                          
053600*01  -COPY W0008  -PRE WDE1-                                              
053700     05  FILLER                  PIC X.                                   
053800                                                                          
053900*01  -COPY W0008  -PRE WDE2-                                              
054000     05  FILLER                  PIC X.                                   
054100                                                                          
054200*01  -COPY W0008  -PRE WDB2-                                              
054300     05  FILLER                  PIC X.                                   
054400                                                                          
054500*01  -COPY W0008  -PRE 4495-                                              
054600     05  FILLER                  PIC X.                                   
054700                                                                          
054800*01  -COPY W0008  -PRE 4463-                                              
054900     05  FILLER                  PIC X.                                   
055000                                                                          
055100*01  -COPY W0008  -PRE 4513-                                              
055200     05  FILLER                  PIC X.                                   
055300                                                                          
055400*01  -COPY W0008  -PRE WDB6-                                              
055500     05  FILLER                  PIC X.                                   
055600                                                                          
055700 01  SHNO-4517-PCB               PIC X.                                   
055800     EJECT                                                                
055900                                                                          
056000 PROCEDURE DIVISION  USING MSG-PCB  AD36-PCB  ALT-PCB ALT1-PCB            
056100                           ALT2-PCB ALT3-PCB ALT4-PCB                     
056200                           ALT6-PCB WDP7-PCB WDE6-PCB WDE1-PCB            
056300                           WDE2-PCB WDB2-PCB 4495-PCB 4463-PCB            
056400                           4513-PCB WDB6-PCB                              
056500                           SHNO-4517-PCB.                                 
056600 MAIN SECTION.                                                            
056700     ENTRY 'DLITCBL' USING MSG-PCB  AD36-PCB  ALT-PCB ALT1-PCB            
056800                           ALT2-PCB ALT3-PCB ALT4-PCB                     
056900                           ALT6-PCB WDP7-PCB WDE6-PCB WDE1-PCB            
057000                           WDE2-PCB WDB2-PCB 4495-PCB 4463-PCB            
057100                           4513-PCB WDB6-PCB                              
057200                           SHNO-4517-PCB.                                 
057300                                                                          
057400     PERFORM IMS-GET-MSG                                                  
057500     IF SEGMENT-FOUND                                                     
057600       PERFORM A-INIT                                                     
057700       IF GOOD-MID                                                        
057800         PERFORM B-CHECK-KEYS                                             
057900       END-IF                                                             
058000       IF KEYS-OK                                                         
058100         IF MFS-UPDATE                                                    
058200           PERFORM G-CHECK-INPUT                                          
058300           IF INDATA-OK                                                   
058400             PERFORM H-UPDATE                                             
058500           END-IF                                                         
058600           IF INDATA-OK                                                   
058700             IF REL-TRANSPORT                                             
058800               PERFORM I-REL-TRANSPORT                                    
058900*            ELSE                                                         
059000*              MOVE 'EJ REL-TRANSPORT' TO FELTEXT                         
059100*              CALL FELLOG                                                
059200             END-IF                                                       
059300           END-IF                                                         
059400         ELSE                                                             
059500           IF MFS-UPD-X                                                   
059600             PERFORM I-REL-TRANSPORT                                      
059700*              MOVE 'EJ MFS-UPD-X' TO FELTEXT                             
059800*              CALL FELLOG                                                
059900           ELSE                                                           
060000             IF 4676-MID                                                  
060100               PERFORM M-RETURN-FROM-4676                                 
060200             ELSE                                                         
060300               IF MFS-FIRST                                               
060400                 PERFORM C-FIRST-PAGE                                     
060500               ELSE                                                       
060600                 IF MFS-NEXT                                              
060700                   PERFORM D-NEXT-PAGE                                    
060800                 ELSE                                                     
060900                   IF MFS-SPLIT                                           
061000                     PERFORM L-PF9-SPLIT                                  
061100                   ELSE                                                   
061200                     PERFORM E-SAME-PAGE                                  
061300                   END-IF                                                 
061400                 END-IF                                                   
061500               END-IF                                                     
061600             END-IF                                                       
061700           END-IF                                                         
061800         END-IF                                                           
061900         IF INDATA-OK AND NOT REL-TRANSPORT AND NOT MFS-UPDATE            
062000            AND NOT MFS-SPLIT                                             
062100           PERFORM F-READ-SHOW-INFO                                       
062200         END-IF                                                           
062300       END-IF                                                             
062400       IF INDATA-OK AND REL-TRANSPORT AND NOT RESTART                     
062500         PERFORM K-START-W40664                                           
062600       ELSE                                                               
062700          IF RESTART                                                      
062800            PERFORM J-START-W40675                                        
062900          ELSE                                                            
063000             IF INDATA-OK AND MFS-SPLIT                                   
063100               COMPUTE P-TO-P-KVLL =                                      
063200                             LENGTH OF PTOP6-MID-W4I67601 + 17            
063300               PERFORM IMS-ISRT-MSG-ALT6                                  
063400             ELSE                                                         
063500               IF GOOD-MID                                                
063600                 COMPUTE MSG-KVLL = LENGTH OF MOD-W4O67501 + 4            
063700                 PERFORM IMS-INSERT-MSG                                   
063800               END-IF                                                     
063900             END-IF                                                       
064000          END-IF                                                          
064100       END-IF                                                             
064200       IF UPDATE-SAVE                                                     
064300          MOVE '002'       TO MSGI-KDCALL                                 
064400          MOVE '4675'      TO SAVE-IDTRANS                                
064500          MOVE SAVE-AREA   TO MSGI-SPAR-AREA                              
064600          CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                      
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     MOVE ZERO TO RETURN-CODE                                             
065100     GOBACK                                                               
065200     .                                                                    
065300     EJECT                                                                
065400 A-INIT SECTION.                                                          
065500                                                                          
065600     IF MSG-DOUBLE-TRANSACTIONS                                           
065700       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I67501                 
065800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
065900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
066000     ELSE                                                                 
066100       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I67501                  
066200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
066300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
066400     END-IF                                                               
066500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
066600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
066700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
066800     MOVE LOW-VALUE TO MSG-AREA                                           
066900     MOVE 'W4O675N1' TO MFS-IDMOD                                         
067000     MOVE '4675' TO MOD-IDTRANS                                           
067100     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
067200                                                                          
067300     IF NOT GOOD-MID                                                      
067400       MOVE NOO  TO KEYS-SW                                               
067500                    INDATA-SW                                             
067600       PERFORM S25-WRONG-PICTURE-MESSAGE                                  
067700     END-IF                                                               
067800                                                                          
067900     MOVE ZERO        TO W-IDSHIPM                                        
068000     MOVE ZERO        TO W-IDDISTR-N                                      
068100     MOVE ALL '9'     TO W-IDKUNDNR-N                                     
068200                                                                          
068300     .                                                                    
068400     EJECT                                                                
068500                                                                          
068600 B-CHECK-KEYS SECTION.                                                    
068700                                                                          
068800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
068900     MOVE '001'             TO MSGI-KDCALL                                
069000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
069100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
069200     MOVE '4675'            TO MSGI-IDTRANS                               
069300                                                                          
069400     IF GOOD-MID                                                          
069500        MOVE MID-IDTRPTNR-IN   TO MSGI-IDTRPTNR                           
069600        MOVE MID-IDLBBET-IN    TO MSGI-IDLBBET                            
069700        MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                           
069800     END-IF                                                               
069900                                                                          
070000     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
070100                                                                          
070200     MOVE MSGI-SPAR-AREA       TO SAVE-AREA                               
070300                                                                          
070400     IF 466D-MID OR 4698-MID                                              
070500       MOVE MID-IDTRPTNR-IN    TO MSGI-IDTRPTNR                           
070600       MOVE MID-IDLBBET-IN     TO MSGI-IDLBBET                            
070700       MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                           
070800       MOVE SPACE              TO SAVE-REL-TRANSPORT                      
070900       MOVE YES                TO UPDATE-SAVE-SW                          
071000     END-IF                                                               
071100                                                                          
071200*    - LANGUAGE TO BE USED BY MEDKONV                                     
071300     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
071400                                                                          
071500     MOVE YES                  TO KEYS-SW                                 
071600                                                                          
071700*    -- CHECK OF WDGXKEY                                                  
071800     MOVE MFS-ERASE-FIELD      TO MOD-IDTRPTNR-IN                         
071900                                  MOD-IDLBBET-IN                          
072000                                  MOD-FLFARLIG-IN                         
072100                                  MOD-IDDC-IN                             
072200                                                                          
072300     IF MFS-UPD-X OR REL-TRANSPORT                                        
072400       MOVE YES  TO SAVE-REL-TRANSPORT                                    
072500       MOVE YES  TO UPDATE-SAVE-SW                                        
072600     ELSE                                                                 
072700       IF GOOD-MID                                                        
072800         IF MID-IDTRPTNR-IN NOT = ALL '+'                                 
072900            MOVE '7'         TO MFS-IDPFK                                 
073000            MOVE SPACE       TO MFS-KDTRTYP                               
073100         END-IF                                                           
073200         IF MID-IDLBBET-IN  NOT = ALL '+'                                 
073300           MOVE '7'         TO MFS-IDPFK                                  
073400           MOVE SPACE       TO MFS-KDTRTYP                                
073500         END-IF                                                           
073600         IF MID-IDDC-IN     NOT = ALL '+'                                 
073700           MOVE '7'         TO MFS-IDPFK                                  
073800           MOVE SPACE       TO MFS-KDTRTYP                                
073900         END-IF                                                           
074000         IF MID-FLFARLIG-IN NOT = ALL '+'                                 
074100           MOVE '7'         TO MFS-IDPFK                                  
074200           MOVE SPACE       TO MFS-KDTRTYP                                
074300         END-IF                                                           
074400       ELSE                                                               
074500         MOVE '7'         TO MFS-IDPFK                                    
074600         MOVE SPACE       TO MFS-KDTRTYP                                  
074700       END-IF                                                             
074800       IF MFS-FIRST OR 4676-MID                                           
074900         MOVE SPACE       TO SAVE-FLAVSLUTA                               
075000                             SAVE-REL-TRANSPORT                           
075100         MOVE YES         TO UPDATE-SAVE-SW                               
075200       END-IF                                                             
075300     END-IF                                                               
075400                                                                          
075500     IF MSGI-IDTRPTNR NUMERIC                                             
075600        IF MSGI-IDTRPTNR > ZERO                                           
075700           MOVE MSGI-IDTRPTNR   TO  W-IDTRPTNR                            
075800                                    W-IDTRPTNR-4466                       
075900        ELSE                                                              
076000          MOVE NOO     TO KEYS-SW                                         
076100        END-IF                                                            
076200     ELSE                                                                 
076300        MOVE NOO       TO KEYS-SW                                         
076400     END-IF                                                               
076500                                                                          
076600     IF MSGI-IDLBBET = ALL '+'                                            
076700        MOVE NOO           TO KEYS-SW                                     
076800     ELSE                                                                 
076900       IF MSGI-IDLBBET > SPACE                                            
077000          MOVE MSGI-IDLBBET  TO W-IDLBBET                                 
077100                                W-IDLBBET-4466                            
077200       ELSE                                                               
077300          MOVE NOO           TO KEYS-SW                                   
077400       END-IF                                                             
077500     END-IF                                                               
077600                                                                          
077700     IF MID-FLFARLIG-IN NOT = ALL '+'                                     
077800       MOVE MID-FLFARLIG-IN    TO W-FLFARLIG                              
077900     ELSE                                                                 
078000       IF SAVE-FLFARLIG > SPACE                                           
078100         MOVE SAVE-FLFARLIG    TO W-FLFARLIG                              
078200       END-IF                                                             
078300     END-IF                                                               
078400                                                                          
078500     IF W-FLFARLIG NOT = SAVE-FLFARLIG                                    
078600       MOVE W-FLFARLIG         TO SAVE-FLFARLIG                           
078700       MOVE YES                TO UPDATE-SAVE-SW                          
078800     END-IF                                                               
078900                                                                          
079000     MOVE MSGI-IDDC-KEY        TO W-IDDC-B6                               
079100     PERFORM IMS-GU-WDB601                                                
079200     IF DCS-KDDC NOT = SPACE                                              
079300       MOVE MSGI-IDDC-KEY      TO W-IDDC                                  
079400                                  W-IDDC-4495                             
079500                                  W-IDDC-4463                             
079510                                  WS-IDDC                                 
079600       IF DCS-DDC                                                         
079700         MOVE WS-CDC-11        TO W-IDDC-MSGI                             
079800       ELSE                                                               
079900         MOVE MSGI-IDDC-KEY    TO W-IDDC-MSGI                             
080000       END-IF                                                             
080100                                                                          
080200       MOVE ALL '+'            TO DC-MSGI-WMSGINIT                        
080300       MOVE '001'              TO DC-MSGI-KDCALL                          
080400       MOVE W-IDDCTEXT-MSGI    TO DC-MSGI-IDUSER                          
080500       MOVE '4675'             TO DC-MSGI-IDTRANS                         
080600       MOVE MSG-LTERM-NAME     TO DC-MSGI-IDLTERM-USER                    
080700       CALL W005INIT        USING DC-MSGI-WMSGINIT WDP7-PCB               
080800                                                                          
080900     ELSE                                                                 
081000       MOVE NOO                TO KEYS-SW                                 
081100     END-IF                                                               
081200                                                                          
081300     IF MID-FLSKRIV-NU NOT = ALL '+'                                      
081400       MOVE MID-FLSKRIV-NU     TO W-FLSKRIV-NU                            
081500     ELSE                                                                 
081600       IF SAVE-FLSKRIV-NU > SPACE                                         
081700         MOVE SAVE-FLSKRIV-NU  TO W-FLSKRIV-NU                            
081800       END-IF                                                             
081900     END-IF                                                               
082000                                                                          
082100     IF W-FLSKRIV-NU NOT = SAVE-FLSKRIV-NU                                
082200       MOVE W-FLSKRIV-NU       TO SAVE-FLSKRIV-NU                         
082300       MOVE YES                TO UPDATE-SAVE-SW                          
082400     END-IF                                                               
082500                                                                          
082600     IF 466D-MID OR 4698-MID                                              
082700       IF MID-IDSHIPM NUMERIC                                             
082800         IF MID-IDSHIPM > ZERO                                            
082900           MOVE MID-IDSHIPM    TO W-IDSHIPM                               
083000         ELSE                                                             
083100           MOVE ZERO           TO W-IDSHIPM                               
083200         END-IF                                                           
083300       ELSE                                                               
083400         MOVE ZERO             TO W-IDSHIPM                               
083500       END-IF                                                             
083600       MOVE W-IDSHIPM          TO SAVE-IDSHIPM                            
083700                                  MOD-IDSHIPM                             
083800       MOVE YES                TO UPDATE-SAVE-SW                          
083900     ELSE                                                                 
084000       IF SAVE-IDSHIPM NUMERIC                                            
084100         MOVE SAVE-IDSHIPM     TO W-IDSHIPM                               
084200       ELSE                                                               
084300         MOVE ZERO             TO W-IDSHIPM                               
084400       END-IF                                                             
084500     END-IF                                                               
084600                                                                          
084700     IF GOOD-MID OR KEYS-OK                                               
084800       MOVE MSGI-IDTRPTNR       TO MOD-IDTRPTNR-UT                        
084900       MOVE MSGI-IDLBBET        TO MOD-IDLBBET-UT                         
085000       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
085100       MOVE SAVE-FLFARLIG       TO MOD-FLFARLIG-UT                        
085200       MOVE SAVE-FLAVSLUTA      TO MOD-FLAVSLUTA                          
085300       MOVE SAVE-FLSKRIV-NU     TO MOD-FLSKRIV-NU                         
085400       MOVE SAVE-IDSHIPM        TO MOD-IDSHIPM                            
085500     ELSE                                                                 
085600       PERFORM MFS-ERASE-FIELD-OUT                                        
085700       MOVE MFS-ERASE-FIELD     TO MOD-FLAVSLUTA                          
085800                                   MOD-FLSKRIV-NU                         
085900     END-IF                                                               
086000                                                                          
086100     IF KEYS-WRONG                                                        
086200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
086300       CALL WMEDKONV USING MED-WMEDAREA                                   
086400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
086500       PERFORM MFS-ERASE-FIELD-IN                                         
086600       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000 C-FIRST-PAGE SECTION.                                                    
087100                                                                          
087200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
087300     CALL WMEDKONV USING MED-WMEDAREA                                     
087400     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
087500                                                                          
087600     PERFORM MFS-ERASE-FIELD-IN                                           
087700     .                                                                    
087800     EJECT                                                                
087900 D-NEXT-PAGE SECTION.                                                     
088000                                                                          
088100     IF SAVE-IDTRANS = '4675'                                             
088200       IF SAVE-IDDISTR-NEXT NUMERIC AND SAVE-IDDISTR-NEXT > ZERO          
088300         MOVE SAVE-IDDISTR-NEXT     TO W-IDDISTR                          
088400       ELSE                                                               
088500         MOVE SAVE-IDDISTR-ENTER    TO W-IDDISTR                          
088600       END-IF                                                             
088700       IF SAVE-IDKUNDNR-NEXT NUMERIC AND SAVE-IDKUNDNR-NEXT > ZERO        
088800         MOVE SAVE-IDKUNDNR-NEXT    TO W-IDKUNDNR                         
088900       ELSE                                                               
089000         MOVE SAVE-IDKUNDNR-ENTER   TO W-IDKUNDNR                         
089100       END-IF                                                             
089200       MOVE SAVE-KDFAKTYP-NEXT      TO W-KDFAKTYP                         
089300       MOVE SAVE-IDKUNDRF-NEXT      TO W-IDKUNDRF                         
089400       IF SAVE-IDPRODNR-NEXT NUMERIC AND SAVE-IDPRODNR-NEXT > ZERO        
089500         MOVE SAVE-IDPRODNR-NEXT    TO W-IDPRODNR                         
089600       ELSE                                                               
089700         MOVE SAVE-IDPRODNR-ENTER   TO W-IDPRODNR                         
089800       END-IF                                                             
089900       IF SAVE-IDKOLLI-NEXT NUMERIC AND SAVE-IDKOLLI-NEXT > ZERO          
090000         MOVE SAVE-IDKOLLI-NEXT     TO W-IDKOLLI                          
090100       ELSE                                                               
090200         MOVE SAVE-IDKOLLI-ENTER    TO W-IDKOLLI                          
090300       END-IF                                                             
090400     ELSE                                                                 
090500       PERFORM MFS-ERASE-FIELD-IN                                         
090600     END-IF                                                               
090700     .                                                                    
090800     EJECT                                                                
090900 E-SAME-PAGE SECTION.                                                     
091000                                                                          
091100     IF SAVE-IDTRANS = '4675' OR '0551'                                   
091200       MOVE SAVE-IDDISTR-ENTER      TO W-IDDISTR                          
091300       MOVE SAVE-IDKUNDNR-ENTER     TO W-IDKUNDNR                         
091400       MOVE SAVE-KDFAKTYP-ENTER     TO W-KDFAKTYP                         
091500       MOVE SAVE-IDKUNDRF-ENTER     TO W-IDKUNDRF                         
091600       MOVE SAVE-IDPRODNR-ENTER     TO W-IDPRODNR                         
091700       MOVE SAVE-IDKOLLI-ENTER      TO W-IDKOLLI                          
091800                                                                          
091900       IF MID-W4I67501   = ALL '+'                                        
092000         PERFORM MFS-ERASE-FIELD-IN                                       
092100       ELSE                                                               
092200         IF MID-FLAVSLUTA NOT = ALL '+'                                   
092300           MOVE MID-FLAVSLUTA           TO W-FLAVSLUTA-SW                 
092400           IF FLAVSLUTA-OK                                                
092500             MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLAVSLUTA-ATTR             
092600           ELSE                                                           
092700             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLAVSLUTA-ATTR             
092800             MOVE NOO TO INDATA-SW                                        
092900           END-IF                                                         
093000         ELSE                                                             
093100           MOVE 1  TO INDX                                                
093200           PERFORM UNTIL INDX > MAX-INDX                                  
093300             IF MID-KDCMD (INDX) NOT = ALL '+'                            
093400               IF MID-KDCMD (INDX)   = 'S'  OR 'X'                        
093500                 IF KDCMD-GIVEN                                           
093600                   MOVE MFS-ALPHA-FIELD-WRONG                             
093700                                       TO MOD-KDCMD-ATTR (INDX)           
093800                   MOVE NOO            TO INDATA-SW                       
093900                 ELSE                                                     
094000                   MOVE MFS-ALPHA-FIELD-OK                                
094100                                       TO MOD-KDCMD-ATTR (INDX)           
094200                   MOVE YES TO KDCMD-SW                                   
094300                 END-IF                                                   
094400               ELSE                                                       
094500                 MOVE MFS-ALPHA-FIELD-WRONG                               
094600                                       TO MOD-KDCMD-ATTR (INDX)           
094700                 MOVE NOO              TO INDATA-SW                       
094800               END-IF                                                     
094900             END-IF                                                       
095000             ADD 1 TO INDX                                                
095100           END-PERFORM                                                    
095200         END-IF                                                           
095300         IF INDATA-WRONG                                                  
095400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
095500           CALL WMEDKONV          USING MED-WMEDAREA                      
095600           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
095700         ELSE                                                             
095800           IF KDCMD-GIVEN                                                 
095900             MOVE INF-PRESS-PF9  TO MED-IDMFSINF                          
096000             CALL WMEDKONV USING MED-WMEDAREA                             
096100             MOVE MED-MFSINF TO MOD-TEMFSFEL                              
096200           ELSE                                                           
096300             MOVE INF-PRESS-PF11 TO MED-IDMFSINF                          
096400             CALL WMEDKONV USING MED-WMEDAREA                             
096500             MOVE MED-MFSINF TO MOD-TEMFSFEL                              
096600           END-IF                                                         
096700         END-IF                                                           
096800         MOVE NOO TO INDATA-SW                                            
096900         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
097000         MOVE SAVE-IDSHIPM            TO MOD-IDSHIPM                      
097100       END-IF                                                             
097200     ELSE                                                                 
097300       PERFORM MFS-ERASE-FIELD-IN                                         
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700                                                                          
097800 F-READ-SHOW-INFO SECTION.                                                
097900                                                                          
098000     PERFORM FA-READ-BASICDATA                                            
098100                                                                          
098200     IF SEGMENT-MISSING                                                   
098300       MOVE KEYS-ARE-MISSING     TO MED-IDMFSFEL                          
098400       CALL WMEDKONV          USING MED-WMEDAREA                          
098500       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
098600       PERFORM MFS-ERASE-FIELD-OUT                                        
098700     ELSE                                                                 
098800       MOVE +1 TO INDX                                                    
098900       IF SEGMENT-FOUND                                                   
099000         MOVE 4498-IDDISTR       TO SAVE-IDDISTR-ENTER                    
099100         MOVE 4498-IDKUNDNR      TO SAVE-IDKUNDNR-ENTER                   
099200         MOVE 4498-KDFAKTYP      TO SAVE-KDFAKTYP-ENTER                   
099300         MOVE 4498-IDKUNDRF      TO SAVE-IDKUNDRF-ENTER                   
099400         MOVE 4498-IDPRODNR      TO SAVE-IDPRODNR-ENTER                   
099500         MOVE 4498-IDKOLLI       TO SAVE-IDKOLLI-ENTER                    
099600       ELSE                                                               
099700         MOVE W-IDDISTR          TO SAVE-IDDISTR-ENTER                    
099800         MOVE W-IDKUNDNR         TO SAVE-IDKUNDNR-ENTER                   
099900         MOVE W-KDFAKTYP         TO SAVE-KDFAKTYP-ENTER                   
100000         MOVE W-IDKUNDRF         TO SAVE-IDKUNDRF-ENTER                   
100100         MOVE W-IDPRODNR         TO SAVE-IDPRODNR-ENTER                   
100200         MOVE W-IDKOLLI          TO SAVE-IDKOLLI-ENTER                    
100300       END-IF                                                             
100400                                                                          
100500       PERFORM UNTIL INDX > MAX-INDX                                      
100600         IF SEGMENT-FOUND                                                 
100700           MOVE 4498-IDDISTR     TO MOD-IDDISTR (INDX)                    
100800                                    SAVE-IDDISTR (INDX)                   
100900                                    W-WDB201-IDDISTR                      
101000                                    W-WDE211-IDDISTR                      
101100                                    W-IDDISTR-N                           
101200                                    TEST-IDDISTR                          
101300           MOVE 4498-IDKUNDNR    TO MOD-IDKUNDNR (INDX)                   
101400                                    SAVE-IDKUNDNR (INDX)                  
101500                                    W-WDB201-IDKUNDNR                     
101600                                    W-WDE211-IDKUNDNR                     
101700           MOVE YES              TO SAVE-RECORD-PRESENT (INDX)            
101800           MOVE NOO              TO FLSAMFAK-SW                           
101900                                    SAVE-FLSAMFAK (INDX)                  
102000** FLSAMFAK IS ALWAYS 'N' FOR FOR IDKUNDNR = ZERO                         
102100           IF 4498-IDKUNDNR NOT = ZERO                                    
102200             PERFORM IMS-GU-WDB201                                        
102300             IF SEGMENT-FOUND                                             
102400               IF DIST92-ITALY OR                                         
102500                  DIST92-GREECE                                           
102600                 MOVE NOO          TO GMT-FLSAMFAK                        
102700               END-IF                                                     
102800               IF GMT-FLSAMFAK = YES                                      
102900                 MOVE GMT-FLSAMFAK                                        
103000                                   TO SAVE-FLSAMFAK (INDX)                
103100                                      FLSAMFAK-SW                         
103200                 MOVE ZERO         TO MOD-IDKUNDNR (INDX)                 
103300                                      SAVE-IDKUNDNR (INDX)                
103400                                      W-WDE211-IDKUNDNR                   
103500               ELSE                                                       
103600                 MOVE NOO          TO SAVE-FLSAMFAK (INDX)                
103700               END-IF                                                     
103800             END-IF                                                       
103900           END-IF                                                         
104000                                                                          
104100           IF FLSAMFAK                                                    
104200* TO READ NEXT DISTRICT                                                   
104300              MOVE ALL '9'         TO W-IDKUNDNR-N                        
104400           ELSE                                                           
104500* TO READ NEXT CUSTOMER                                                   
104600              MOVE 4498-IDKUNDNR   TO W-IDKUNDNR-N                        
104700           END-IF                                                         
104800           PERFORM IMS-GHNP-WDGX4498-N                                    
104900         ELSE                                                             
105000           MOVE MFS-ERASE-FIELD  TO MOD-IDDISTR (INDX)                    
105100                                    MOD-IDKUNDNR (INDX)                   
105200           MOVE NOO              TO SAVE-RECORD-PRESENT (INDX)            
105300                                                                          
105400           MOVE MFS-CLOSE-FIELD  TO MOD-KDCMD-ATTR (INDX)                 
105500                                                                          
105600           MOVE NOO              TO SAVE-FLSAMFAK (INDX)                  
105700           MOVE ZERO             TO SAVE-IDDISTR (INDX)                   
105800                                    SAVE-IDKUNDNR (INDX)                  
105900                                                                          
106000         END-IF                                                           
106100         MOVE MFS-ERASE-FIELD    TO MOD-KDCMD (INDX)                      
106200         ADD 1 TO INDX                                                    
106300       END-PERFORM                                                        
106400                                                                          
106500       IF SEGMENT-FOUND                                                   
106600                                                                          
106700         MOVE 4498-IDDISTR       TO SAVE-IDDISTR-NEXT                     
106800         MOVE 4498-IDKUNDNR      TO SAVE-IDKUNDNR-NEXT                    
106900         MOVE 4498-KDFAKTYP      TO SAVE-KDFAKTYP-NEXT                    
107000         MOVE 4498-IDKUNDRF      TO SAVE-IDKUNDRF-NEXT                    
107100         MOVE 4498-IDPRODNR      TO SAVE-IDPRODNR-NEXT                    
107200         MOVE 4498-IDKOLLI       TO SAVE-IDKOLLI-NEXT                     
107300                                                                          
107400         IF NOT MFS-UPDATE                                                
107500           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
107600           CALL WMEDKONV USING MED-WMEDAREA                               
107700           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
107800         END-IF                                                           
107900       ELSE                                                               
108000         MOVE SAVE-IDDISTR-ENTER  TO SAVE-IDDISTR-NEXT                    
108100         MOVE SAVE-IDKUNDNR-ENTER TO SAVE-IDKUNDNR-NEXT                   
108200         MOVE SAVE-KDFAKTYP-ENTER TO SAVE-KDFAKTYP-NEXT                   
108300         MOVE SAVE-IDKUNDRF-ENTER TO SAVE-IDKUNDRF-NEXT                   
108400         MOVE SAVE-IDPRODNR-ENTER TO SAVE-IDPRODNR-NEXT                   
108500         MOVE SAVE-IDKOLLI-ENTER  TO SAVE-IDKOLLI-NEXT                    
108600                                                                          
108700         MOVE INF-LAST-PAGE       TO MED-IDMFSINF                         
108800         CALL WMEDKONV         USING MED-WMEDAREA                         
108900         MOVE MED-MFSINF          TO MOD-TEMFSFEL                         
109000       END-IF                                                             
109100                                                                          
109200       MOVE YES                   TO UPDATE-SAVE-SW                       
109300                                                                          
109400     END-IF                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 FA-READ-BASICDATA SECTION.                                               
109800                                                                          
109900     PERFORM IMS-GHU-WDR401-4495                                          
110000                                                                          
110100     IF SEGMENT-FOUND                                                     
110200       IF MFS-NEXT OR MFS-ENTER OR 4676-MID                               
110300         PERFORM IMS-GHNP-WDGX4498-KEY                                    
110400       ELSE                                                               
110500         PERFORM IMS-GHNP-WDGX4498                                        
110600       END-IF                                                             
110700     END-IF                                                               
110800     .                                                                    
110900     EJECT                                                                
111000 G-CHECK-INPUT SECTION.                                                   
111100                                                                          
111200     MOVE YES  TO INDATA-SW                                               
111300     MOVE NOO  TO INPUT-SW                                                
111400     IF MID-W4I67501 = ALL '+'                                            
111500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
111600       CALL WMEDKONV USING MED-WMEDAREA                                   
111700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
111800       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
111900       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
112000       MOVE NOO TO INDATA-SW                                              
112100     ELSE                                                                 
112200       IF MID-FLAVSLUTA NOT = ALL '+'                                     
112300         MOVE MID-FLAVSLUTA           TO W-FLAVSLUTA-SW                   
112400         IF FLAVSLUTA-OK                                                  
112500           MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLAVSLUTA-ATTR               
112600         ELSE                                                             
112700           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLAVSLUTA-ATTR               
112800           MOVE NOO TO INDATA-SW                                          
112900         END-IF                                                           
113000       ELSE                                                               
113100         MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-FLAVSLUTA-ATTR               
113200         MOVE NOO TO INDATA-SW                                            
113300       END-IF                                                             
113400                                                                          
113500       MOVE 1  TO INDX                                                    
113600       PERFORM UNTIL INDX > MAX-INDX                                      
113700                                                                          
113800         IF MID-KDCMD (INDX) NOT = ALL '+'                                
113900           IF MID-KDCMD (INDX)      = 'S'  OR 'X'                         
114000             MOVE NOO TO INDATA-SW                                        
114100             MOVE MFS-ALPHA-FIELD-WRONG                                   
114200                                 TO MOD-KDCMD-ATTR (INDX)                 
114300           ELSE                                                           
114400             MOVE MFS-ALPHA-FIELD-WRONG                                   
114500                                 TO MOD-KDCMD-ATTR (INDX)                 
114600             MOVE NOO            TO INDATA-SW                             
114700             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
114800             CALL WMEDKONV          USING MED-WMEDAREA                    
114900             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
115000           END-IF                                                         
115100           MOVE YES TO KDCMD-SW                                           
115200         END-IF                                                           
115300         ADD 1 TO INDX                                                    
115400                                                                          
115500       END-PERFORM                                                        
115600                                                                          
115700       IF INDATA-WRONG                                                    
115800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
115900         CALL WMEDKONV USING MED-WMEDAREA                                 
116000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
116100         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
116200         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
116300       END-IF                                                             
116400     END-IF                                                               
116500     .                                                                    
116600     EJECT                                                                
116700 H-UPDATE SECTION.                                                        
116800                                                                          
116900     IF MID-FLAVSLUTA NOT = ALL '+'                                       
117000       MOVE MID-FLAVSLUTA               TO SAVE-FLAVSLUTA                 
117100                                           MOD-FLAVSLUTA                  
117200       MOVE MFS-ADD-HILIGHT-FIELD       TO MOD-FLAVSLUTA-ATTR             
117300     ELSE                                                                 
117400       MOVE MFS-DO-NOT-TOUCH-FIELD      TO MOD-FLAVSLUTA                  
117500     END-IF                                                               
117600                                                                          
117700     IF SAVE-FLAVSLUTA = YES OR YES-Y                                     
117800       MOVE YES TO SAVE-REL-TRANSPORT                                     
117900       MOVE YES TO UPDATE-SAVE-SW                                         
118000     END-IF                                                               
118100                                                                          
118200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
118300     CALL WMEDKONV USING MED-WMEDAREA                                     
118400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
118500     PERFORM MFS-FORM-ATTR                                                
118600     PERFORM MFS-ERASE-FIELD-IN                                           
118700                                                                          
118800     MOVE MFS-DO-NOT-TOUCH-FIELD   TO  MOD-FLSKRIV-NU                     
118900     MOVE 1 TO INDX                                                       
119000     PERFORM UNTIL INDX > MAX-INDX                                        
119100                                                                          
119200       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR (INDX)                  
119300                                      MOD-IDKUNDNR (INDX)                 
119400                                                                          
119500       ADD 1 TO INDX                                                      
119600     END-PERFORM                                                          
119700                                                                          
119800     .                                                                    
119900     EJECT                                                                
120000 I-REL-TRANSPORT SECTION.                                                 
120100                                                                          
120200     MOVE ZERO               TO W-IDDISTR-PREV                            
120300                                W-IDKUNDNR-PREV                           
120400                                W-IDDEALER-PREV                           
120500                                W-IDPRODNR-PREV                           
120600                                                                          
120700     MOVE YES                TO FIRST-PRODNR-SW                           
120800                                FIRST-KUNDNR-SW                           
120900                                FIRST-DEALER-SW                           
121000                                                                          
121100     MOVE NOO                TO RESTART-SW                                
121200                                FARLIGT-GOOD-4539                         
121300                                FARLIGT-GOOD-4540                         
121400                                FLSAMFAK-SW                               
121500                                4498-MISSING-SW                           
121600                                START-4540-4537-SW                        
121700                                                                          
121800     MOVE ZERO               TO W-UPD-COUNT                               
121900                                                                          
122000                                                                          
122100     PERFORM S06-DC-LAND                                                  
122200                                                                          
122300     PERFORM IMS-GHU-WDR401-4495                                          
122400     IF SEGMENT-FOUND                                                     
122500                                                                          
122600        IF W-IDSHIPM = ZERO                                               
122700          PERFORM S01-CREATE-IDSHIPM                                      
122800          PERFORM S02-CREATE-WDE101                                       
122900          PERFORM S03-CREATE-WDE201                                       
123000        ELSE                                                              
123100          PERFORM IMS-GHU-WDE101                                          
123200          PERFORM IMS-GHU-WDE201                                          
123300        END-IF                                                            
123400                                                                          
123500        IF DCS-NDC-NA AND NOT NDC-US-BAT                                  
123600          PERFORM IJ-CREATE-4463-BL                                       
123700        END-IF                                                            
123800                                                                          
123900        PERFORM IMS-GHNP-WDGX4498                                         
124000                                                                          
124100        IF SEGMENT-FOUND                                                  
124200          PERFORM S11-CHECK-FLSAMFAK                                      
124300        ELSE                                                              
124400          MOVE YES TO 4498-MISSING-SW                                     
124500        END-IF                                                            
124600                                                                          
124700        PERFORM UNTIL 4498-MISSING  OR RESTART                            
124800                                                                          
124900          IF 4498-IDPRODNR   NOT =  W-IDPRODNR-PREV                       
125000            IF FIRST-PRODNR-SW  = NOO                                     
125100              PERFORM S04-CHECK-KDORDKL                                   
125200            ELSE                                                          
125300              MOVE NOO     TO FIRST-PRODNR-SW                             
125400            END-IF                                                        
125500                                                                          
125600            MOVE 4498-IDPRODNR TO W-IDPRODNR-PREV                         
125700          END-IF                                                          
125800                                                                          
125900          IF DCS-NDC-NA OR                                                
126000            (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU') OR                       
126100             DCS-CDC OR                                                   
126200            (DCS-DDC AND DCS-IDLANDX2 = 'SE')                             
126300            IF FIRST-DEALER-SW = NOO                                      
126400              IF (4498-IDDISTR      NOT =  W-IDDISTR-PREV)  OR            
126500                 (4498-IDDEALER     NOT =  W-IDDEALER-PREV)               
126600                MOVE W-IDDISTR-PREV TO TEST-IDDISTR                       
126700                IF FARLIGT-GOOD-FOUND-4539                                
126800                    AND DCS-NDC-NA                                        
126900                    AND W-KDORDKL-MAX < +2                                
127000                    AND (DIST35-NA-TRANSFER    OR                         
127100                         DIST35-NA-CDC-RETURN  OR                         
127200                         DIST35-NA-NDC-RETURNS OR                         
127300                         DIST35-REFILL-INOM-NA OR                         
127400                         DIST07-CAN-RETAILER)                             
127500                   PERFORM S09A-START-W40539                              
127600                   MOVE NOO TO FARLIGT-GOOD-4539                          
127700                ELSE                                                      
127800                  IF FARLIGT-GOOD-FOUND-4539                              
127900                      AND W-KDORDKL-MAX < +2                              
128000                      AND DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'              
128100                     PERFORM S09A-START-W40539                            
128200                     MOVE NOO TO FARLIGT-GOOD-4539                        
128300                  ELSE                                                    
128400                     IF FARLIGT-GOOD-FOUND-4539                           
128500                        AND (DCS-CDC OR                                   
128600                            (DCS-DDC AND DCS-IDLANDX2 = 'SE'))            
128700                       PERFORM S09A-START-W40539                          
128800                       MOVE NOO TO FARLIGT-GOOD-4539                      
128900                     END-IF                                               
129000                  END-IF                                                  
129100                END-IF                                                    
129200              END-IF                                                      
129300            ELSE                                                          
129400              MOVE NOO      TO  FIRST-DEALER-SW                           
129500            END-IF                                                        
129600          END-IF                                                          
129700                                                                          
129800          IF 4498-IDDISTR      NOT = W-IDDISTR-PREV  OR                   
129900             4498-IDKUNDNR     NOT = W-IDKUNDNR-PREV                      
130000                                                                          
130100            MOVE 4498-IDDISTR   TO W-WDE111-IDDISTR                       
130200                                   W-WDE211-IDDISTR                       
130300                                   TEST-IDDISTR                           
130400            MOVE 4498-IDKUNDNR  TO W-WDE111-IDKUNDNR                      
130500                                   W-WDE211-IDKUNDNR                      
130600            PERFORM S15-CREATE-WDE111                                     
130700            PERFORM S16-CREATE-WDE211                                     
130800                                                                          
130900          END-IF                                                          
131000                                                                          
131100          IF START-4540-4537                                              
131200            IF FARLIGT-GOOD-FOUND-4540                                    
131300               AND (DCS-CDC OR                                            
131400                   (DCS-DDC AND DCS-IDLANDX2 = 'SE'))                     
131500              PERFORM S09B-START-W40540                                   
131600              MOVE NOO TO FARLIGT-GOOD-4540                               
131700            END-IF                                                        
131800                                                                          
131900            MOVE W-IDDISTR-PREV TO TEST-IDDISTR                           
132000                                                                          
132100            MOVE NOO               TO START-4540-4537-SW                  
132200          END-IF                                                          
132300                                                                          
132400          IF (4498-IDDISTR      NOT = W-IDDISTR-PREV)  OR                 
132500             (4498-IDKUNDNR     NOT = W-IDKUNDNR-PREV)                    
132600                                                                          
132700            MOVE 4498-IDKUNDNR  TO W-IDKUNDNR-PREV                        
132800            MOVE 4498-IDDISTR   TO W-IDDISTR-PREV                         
132900                                                                          
133000          END-IF                                                          
133100                                                                          
133200          MOVE 4498-IDDISTR TO TEST-IDDISTR                               
133300          PERFORM ID-UPDATE-KOLLI                                         
133400                                                                          
133500                                                                          
133600          IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                              
133700             CONTINUE                                                     
133800          ELSE                                                            
133900             IF DCS-CDC OR DCS-DDC AND DCS-IDLANDX2 = 'SE'                
134000                PERFORM IH-CREATE-HTR4513                                 
134100             ELSE                                                         
134200                IF DCS-NDC-NA                                             
134300                  IF W-KDORDKL-MAX < +2      AND                          
134400                     (DIST35-NA-TRANSFER     OR                           
134500                      DIST35-NA-CDC-RETURN   OR                           
134600                      DIST35-NA-NDC-RETURNS  OR                           
134700                      DIST35-REFILL-INOM-NA  OR                           
134800                      DIST07-CAN-RETAILER)                                
134900                    PERFORM IH-CREATE-HTR4513                             
135000                  END-IF                                                  
135100                ELSE                                                      
135200                  PERFORM IH-CREATE-HTR4513                               
135300                END-IF                                                    
135400             END-IF                                                       
135500          END-IF                                                          
135600                                                                          
135700          IF DCS-NDC-NA OR DCS-NDC-PF                                     
135800            PERFORM IG-UPDATE-WDGX4468-BL                                 
135900          END-IF                                                          
136000                                                                          
136100          PERFORM S17-CREATE-WDE121                                       
136200                                                                          
136300          PERFORM S18-CREATE-WDE221                                       
136400                                                                          
136500          MOVE 4498-IDDISTR TO TEST-IDDISTR                               
136600          PERFORM IMS-DLET-WDGX4498                                       
136700                                                                          
136800          MOVE 4498-IDDEALER     TO W-IDDEALER-PREV                       
136900          MOVE FLSAMFAK-SW       TO FLSAMFAK-PREV-SW                      
137000          PERFORM IMS-GHNP-WDGX4498                                       
137100                                                                          
137200          IF SEGMENT-FOUND                                                
137300            IF FLSAMFAK                                                   
137400              IF W-IDTRPTNR < +100                                        
137500                IF (4498-IDDISTR NOT = W-IDDISTR-PREV)   OR               
137600                   (4498-IDKUNDNR NOT = W-IDKUNDNR-PREV)                  
137700                  MOVE YES         TO START-4540-4537-SW                  
137800                END-IF                                                    
137900              ELSE                                                        
138000                IF 4498-IDDISTR NOT = W-IDDISTR-PREV                      
138100                  MOVE YES       TO START-4540-4537-SW                    
138200                  IF W-UPD-COUNT  > W-UPD-MAX                             
138300                    MOVE YES     TO RESTART-SW                            
138400                  END-IF                                                  
138500                END-IF                                                    
138600              END-IF                                                      
138700            ELSE                                                          
138800              IF DIST92-ITALY OR                                          
138900                 DIST92-GREECE                                            
139000                IF 4498-IDDISTR NOT = W-IDDISTR-PREV                      
139100                  MOVE YES       TO START-4540-4537-SW                    
139200                                                                          
139300                  IF W-UPD-COUNT  > W-UPD-MAX                             
139400                    MOVE YES     TO RESTART-SW                            
139500                  END-IF                                                  
139600                END-IF                                                    
139700              ELSE                                                        
139800                IF (4498-IDDISTR NOT = W-IDDISTR-PREV)   OR               
139900                   (4498-IDKUNDNR NOT = W-IDKUNDNR-PREV)                  
140000                  MOVE YES       TO START-4540-4537-SW                    
140100                  IF W-UPD-COUNT  > W-UPD-MAX                             
140200                    MOVE YES     TO RESTART-SW                            
140300                  END-IF                                                  
140400                END-IF                                                    
140500              END-IF                                                      
140600            END-IF                                                        
140700          ELSE                                                            
140800            MOVE YES TO 4498-MISSING-SW                                   
140900          END-IF                                                          
141000                                                                          
141100          IF NOT RESTART                                                  
141200            IF SEGMENT-FOUND                                              
141300              PERFORM S11-CHECK-FLSAMFAK                                  
141400            END-IF                                                        
141500          END-IF                                                          
141600                                                                          
141700        END-PERFORM                                                       
141800                                                                          
141900        PERFORM S04-CHECK-KDORDKL                                         
142000                                                                          
142100        IF FARLIGT-GOOD-FOUND-4540                                        
142200           AND (DCS-CDC OR DCS-DDC AND DCS-IDLANDX2 = 'SE')               
142300          PERFORM S09B-START-W40540                                       
142400          MOVE NOO TO FARLIGT-GOOD-4540                                   
142500        END-IF                                                            
142600                                                                          
142700        MOVE W-IDDISTR-PREV TO TEST-IDDISTR                               
142800        IF FARLIGT-GOOD-FOUND-4539                                        
142900           AND DCS-NDC-NA                                                 
143000           AND W-KDORDKL-MAX < +2                                         
143100           AND (DIST35-NA-TRANSFER    OR                                  
143200                DIST35-NA-CDC-RETURN  OR                                  
143300                DIST35-NA-NDC-RETURNS OR                                  
143400                DIST35-REFILL-INOM-NA OR                                  
143500                DIST07-CAN-RETAILER)                                      
143600          PERFORM S09A-START-W40539                                       
143700        ELSE                                                              
143800          IF FARLIGT-GOOD-FOUND-4539                                      
143900             AND W-KDORDKL-MAX < +2                                       
144000             AND DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'                       
144100            PERFORM S09A-START-W40539                                     
144200          ELSE                                                            
144300            IF FARLIGT-GOOD-FOUND-4539                                    
144400               AND (DCS-CDC OR                                            
144500                   (DCS-DDC AND DCS-IDLANDX2 = 'SE'))                     
144600              PERFORM S09A-START-W40539                                   
144700            END-IF                                                        
144800          END-IF                                                          
144900        END-IF                                                            
145000                                                                          
145100                                                                          
145200        MOVE NOO                TO  FARLIGT-GOOD-4539                     
145300                                                                          
145400        PERFORM IMS-GHU-WDGX4498                                          
145500                                                                          
145600        IF SEGMENT-MISSING                                                
145700           PERFORM IMS-GHU-WDR401-4495                                    
145800           IF SEGMENT-FOUND                                               
145900              PERFORM IMS-DLET-WDR401-4495                                
146000                                                                          
146100              PERFORM IMS-GHU-WDE101                                      
146200              MOVE 'N'           TO  SHIP-KDKLAR                          
146300              MOVE VORD-IDDC-EXP TO SHIP-IDDC-EXP                         
146400              PERFORM IMS-REPL-WDE101                                     
146500                                                                          
146600              PERFORM IMS-GHU-WDE201                                      
146700              MOVE VORD-IDDC-EXP TO BILL-IDDC-EXP                         
146800              PERFORM IMS-REPL-WDE201                                     
146900                                                                          
147000              PERFORM S21-OPEN-WZ01                                       
147100              PERFORM S22-SEND-WZ01                                       
147200              PERFORM S23-CLOSE-WZ01                                      
147300           END-IF                                                         
147400        END-IF                                                            
147500                                                                          
147600        MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                             
147700        CALL WMEDKONV USING MED-WMEDAREA                                  
147800        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
147900        PERFORM MFS-FORM-ATTR                                             
148000     END-IF                                                               
148100                                                                          
148200     .                                                                    
148300     EJECT                                                                
148400                                                                          
148500 ID-UPDATE-KOLLI   SECTION.                                               
148600                                                                          
148700     MOVE 4498-IDPRODNR       TO   W-IDPRODNR-KOLLI                       
148800                                                                          
148900     MOVE 4498-IDKOLLI        TO W-IDKOLLI-KOLLI                          
149000     PERFORM IMS-GHU-WDE611                                               
149100     IF SEGMENT-FOUND                                                     
149200** CHECK KOLLI CONTAIN FARLIGT(HAZARDOUS) GOOD                            
149300       MOVE +1       TO IDPSN-IX                                          
149400       PERFORM UNTIL IDPSN-IX > +10                                       
149500          IF KOLLI-IDPSN(IDPSN-IX) > ZERO                                 
149600             MOVE YES TO FARLIGT-GOOD-4539                                
149700             MOVE YES TO FARLIGT-GOOD-4540                                
149800          END-IF                                                          
149900          ADD +1     TO IDPSN-IX                                          
150000       END-PERFORM                                                        
150100                                                                          
150200       IF KOLLI-KDKOLSTA > 6                                              
150300         MOVE 'KOLLI REDAN FAKTURERAT?' TO ERROR-TEXT                     
150400         CALL FELLOG                                                      
150500       END-IF                                                             
150600       MOVE KLI-PACK-FAKT       TO KOLLI-KDKOLSTA                         
151100       MOVE +9999999            TO KOLLI-TILASTID                         
151800                                                                          
151900       PERFORM IMS-REPL-WDE6                                              
152000       ADD +1                   TO W-UPD-COUNT                            
152100     END-IF                                                               
152200     .                                                                    
152300     EJECT                                                                
152400 IH-CREATE-HTR4513 SECTION.                                               
152500                                                                          
152600     IF FARLIGT-GOOD-FOUND-4539                                           
152700     OR FARLIGT-GOOD-FOUND-4540                                           
152800       MOVE DC-MSGI-TILOKDAT            TO W-YYMMDD-DATUM                 
152900       MOVE W-YYMMDD-DATUM              TO W-YYMMDD                       
153000       MOVE '20'                        TO W-CC                           
153100       MOVE W-CCYYMMDD-DATUM            TO 4514-DASKEPPN                  
153200                                           W-DASKEPPN-4514                
153300       PERFORM IMS-ISRT-WDGX4514                                          
153400       ADD +1                           TO W-UPD-COUNT                    
153500                                                                          
153600       MOVE W-IDDC                      TO 4516-IDDC                      
153700       MOVE W-IDSHIPM                   TO 4516-IDSKEPPN                  
153800       MOVE 4498-IDDISTR                TO 4516-IDDISTR                   
153900       MOVE 4498-IDKUNDNR               TO 4516-IDKUNDNR                  
154000       MOVE 4498-IDPRODNR               TO 4516-IDPRODNR                  
154100       MOVE 4498-IDKOLLI                TO 4516-IDKOLLI                   
154200       MOVE 4498-IDKUNDRF               TO 4516-IDKUNDRF                  
154300       PERFORM IMS-ISRT-WDGX4516                                          
154400       ADD +1                           TO W-UPD-COUNT                    
154500     END-IF                                                               
154600     .                                                                    
154700     EJECT                                                                
154800 IJ-CREATE-4463-BL SECTION.                                               
154900                                                                          
155000     PERFORM IMS-GHU-WDR401-4463                                          
155100                                                                          
155200     MOVE DC-MSGI-TILOKDAT            TO W-YYMMDD-DATUM                   
155300     MOVE W-YYMMDD-DATUM              TO W-YYMMDD                         
155400     MOVE '20'                        TO W-CC                             
155500     MOVE W-CCYYMMDD-DATUM            TO W-DASKEPPN-4464                  
155600                                         4464-DASKEPPN                    
155700     PERFORM IMS-ISRT-WDGX4464                                            
155800     ADD  +1                          TO W-UPD-COUNT                      
155900     MOVE W-IDTRPTNR-4466             TO 4466-IDTRPTNR                    
156000     MOVE W-IDLBBET-4466              TO 4466-IDLBBET                     
156100                                                                          
156200     PERFORM IMS-ISRT-WDGX4466                                            
156300     ADD +1                           TO W-UPD-COUNT                      
156400     .                                                                    
156500     EJECT                                                                
156600                                                                          
156700 IG-UPDATE-WDGX4468-BL SECTION.                                           
156800                                                                          
156900     MOVE 4498-IDDISTR                TO W-IDDISTR-4468                   
157000                                         4468-IDDISTR                     
157100     MOVE 4498-IDKUNDNR               TO W-IDKUNDNR-4468                  
157200                                         4468-IDKUNDNR                    
157300     MOVE 4498-IDKUNDRF               TO W-IDKUNDRF-4468                  
157400                                         4468-IDKUNDRF                    
157500     MOVE 4498-IDPRODNR               TO W-IDPRODNR-4468                  
157600                                         4468-IDPRODNR                    
157700     MOVE 4498-IDKOLLI                TO W-IDKOLLI-4468                   
157800                                         4468-IDKOLLI                     
157900                                                                          
158000     MOVE +1                          TO IDPSN-IX                         
158100     PERFORM UNTIL IDPSN-IX > +10                                         
158200        IF KOLLI-IDPSN(IDPSN-IX) > ZERO                                   
158300           MOVE KOLLI-IDPSN(IDPSN-IX) TO 4468-IDPSN(IDPSN-IX)             
158400        ELSE                                                              
158500           MOVE ZERO                  TO 4468-IDPSN(IDPSN-IX)             
158600        END-IF                                                            
158700        ADD +1                        TO IDPSN-IX                         
158800     END-PERFORM                                                          
158900                                                                          
159000     MOVE KOLLI-KDORDKL               TO 4468-KDORDKL                     
159100     MOVE 4498-VKORDBTO               TO 4468-VKORDBTO-KOLLI              
159200     MOVE 4498-VLORDBTO               TO 4468-VLORDBTO-KOLLI              
159300     MOVE 4498-KDKOLLI                TO 4468-KDKOLLI                     
159400                                                                          
159500     PERFORM IMS-ISRT-WDGX4468                                            
159600     ADD +1                           TO W-UPD-COUNT                      
159700     .                                                                    
159800     EJECT                                                                
159900 J-START-W40675          SECTION.                                         
160000                                                                          
160100     MOVE W-IDTRPTNR          TO  MID-IDTRPTNR-IN                         
160200     MOVE W-IDLBBET           TO  MID-IDLBBET-IN                          
160300     MOVE W-IDDC              TO  MID-IDDC-IN                             
160400     MOVE W-FLFARLIG          TO  MID-FLFARLIG-IN                         
160500     MOVE W-FLSKRIV-NU        TO  MID-FLSKRIV-NU                          
160600     MOVE W-IDSHIPM           TO  MID-IDSHIPM                             
160700     MOVE YES                 TO  MID-FLAVSLUTA                           
160800     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX +                    
160900                                   LENGTH OF MID-W4I67501                 
161000     MOVE LOW-VALUE           TO  P-TO-P-KDZ1                             
161100     MOVE LOW-VALUE           TO  P-TO-P-KDZ2                             
161200     MOVE '4675'              TO  P-TO-P-IDTRANS                          
161300     MOVE MFS-KDMFSFOR        TO  P-TO-P-KDMFSFOR                         
161400                                                                          
161500     MOVE MID-W4I67501        TO  P-TO-P-DATA                             
161600                                                                          
161700     IF MFS-UPD-X                                                         
161800       MOVE 'W4T675X '        TO  P-TO-P-KDTRANS                          
161900       PERFORM IMS-ISRT-ALT-MSG-4675X                                     
162000     ELSE                                                                 
162100       MOVE 'W4T675U '        TO  P-TO-P-KDTRANS                          
162200       PERFORM IMS-ISRT-ALT-MSG-4675                                      
162300     END-IF                                                               
162400     .                                                                    
162500     EJECT                                                                
162600 K-START-W40664          SECTION.                                         
162700                                                                          
162800     MOVE W-IDTRPTNR          TO  MOD4664-MID-IDTRPTNR-IN                 
162900     MOVE W-IDLBBET           TO  MOD4664-MID-IDLBBET-IN                  
163000     MOVE W-FLFARLIG          TO  MOD4664-MID-FLFARLIG-IN                 
163100     MOVE W-IDDC              TO  MOD4664-MID-IDDC-IN                     
163200     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX                      
163300                                   + 23 + 15                              
163400     MOVE LOW-VALUE           TO  P-TO-P-KDZ1                             
163500     MOVE LOW-VALUE           TO  P-TO-P-KDZ2                             
163600     MOVE 'W4T664  '          TO  P-TO-P-KDTRANS                          
163700     MOVE '4675'              TO  P-TO-P-IDTRANS                          
163800     MOVE MFS-KDMFSFOR        TO  P-TO-P-KDMFSFOR                         
163900     MOVE MOD4664-MID-W4I66401                                            
164000                              TO  P-TO-P-DATA                             
164100     PERFORM IMS-ISRT-ALT-MSG-4664                                        
164200     .                                                                    
164300     EJECT                                                                
164400 L-PF9-SPLIT      SECTION.                                                
164500                                                                          
164600     MOVE  1       TO INDX                                                
164700     PERFORM UNTIL INDX > MAX-INDX                                        
164800       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
164900         MOVE YES                      TO KDCMD-SW                        
165000         IF MID-KDCMD (INDX) = 'S' OR 'X'                                 
165100           IF SAVE-RECORD-PRESENT (INDX)  =  YES                          
165200             MOVE ALL '+'              TO PTOP6-MID-W4I67601              
165300             MOVE W-IDTRPTNR           TO PTOP6-MID-IDTRPTNR-IN           
165400             MOVE W-IDLBBET            TO PTOP6-MID-IDLBBET-IN            
165500             MOVE W-FLFARLIG           TO PTOP6-MID-FLFARLIG-IN           
165600             MOVE W-IDDC               TO PTOP6-MID-IDDC-IN               
165700             MOVE SAVE-IDDISTR (INDX)  TO SAVE-IDDISTR-4676               
165800             MOVE SAVE-IDKUNDNR (INDX) TO SAVE-IDKUNDNR-4676              
165900             MOVE SAVE-FLSAMFAK (INDX) TO SAVE-FLSAMFAK-4676              
166000             MOVE YES                  TO UPDATE-SAVE-SW                  
166100             MOVE MFS-ALPHA-FIELD-OK   TO MOD-KDCMD-ATTR (INDX)           
166200           ELSE                                                           
166300             MOVE NOO                  TO INDATA-SW                       
166400             MOVE MFS-ALPHA-FIELD-WRONG                                   
166500                                       TO MOD-KDCMD-ATTR (INDX)           
166600             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
166700             CALL WMEDKONV          USING MED-WMEDAREA                    
166800             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
166900           END-IF                                                         
167000           ADD MAX-INDX                TO INDX                            
167100         ELSE                                                             
167200           MOVE NOO                    TO INDATA-SW                       
167300           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDCMD-ATTR (INDX)           
167400           MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                    
167500           CALL WMEDKONV            USING MED-WMEDAREA                    
167600           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
167700         END-IF                                                           
167800       END-IF                                                             
167900       ADD 1     TO INDX                                                  
168000     END-PERFORM                                                          
168100                                                                          
168200     IF NOT KDCMD-GIVEN                                                   
168300       MOVE NOO                    TO INDATA-SW                           
168400       MOVE ERR-SELECT-LINE        TO MED-IDMFSFEL                        
168500       CALL WMEDKONV            USING MED-WMEDAREA                        
168600       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
168700     END-IF                                                               
168800                                                                          
168900     IF INDATA-WRONG                                                      
169000       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
169100       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
169200     ELSE                                                                 
169300       MOVE LOW-VALUE            TO P-TO-P-KDZ1                           
169400       MOVE LOW-VALUE            TO P-TO-P-KDZ2                           
169500       MOVE 'W4T676  '           TO P-TO-P-KDTRANS                        
169600       MOVE '4675'               TO P-TO-P-IDTRANS                        
169700       MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                       
169800       MOVE PTOP6-MID-W4I67601   TO P-TO-P-DATA                           
169900     END-IF                                                               
170000     .                                                                    
170100     EJECT                                                                
170200                                                                          
170300 M-RETURN-FROM-4676 SECTION.                                              
170400                                                                          
170500** TO SHOW THE SAME PAGE FROM WHERE JUMPED TO 4676                        
170600     MOVE SAVE-IDDISTR-ENTER      TO W-IDDISTR                            
170700     MOVE SAVE-IDKUNDNR-ENTER     TO W-IDKUNDNR                           
170800     MOVE SAVE-KDFAKTYP-ENTER     TO W-KDFAKTYP                           
170900     MOVE SAVE-IDKUNDRF-ENTER     TO W-IDKUNDRF                           
171000     MOVE SAVE-IDPRODNR-ENTER     TO W-IDPRODNR                           
171100     MOVE SAVE-IDKOLLI-ENTER      TO W-IDKOLLI                            
171200     .                                                                    
171300     EJECT                                                                
171400                                                                          
171500 S01-CREATE-IDSHIPM SECTION.                                              
171600                                                                          
171700     CALL W476SHNO USING SHNO-W476SHNO SHNO-4517-PCB                      
171800                                                                          
171900     MOVE SHNO-IDSHIPM            TO W-IDSHIPM                            
172000                                     SAVE-IDSHIPM                         
172100                                     MOD-IDSHIPM                          
172200     MOVE YES                     TO UPDATE-SAVE-SW                       
172300                                                                          
172400     ADD +1                       TO W-UPD-COUNT                          
172500     .                                                                    
172600     EJECT                                                                
172700                                                                          
172800 S02-CREATE-WDE101 SECTION.                                               
172900                                                                          
173000     MOVE W-IDSHIPM           TO SHIP-IDSHIPM                             
173100     MOVE W-IDTRPTNR          TO SHIP-IDTRPTNR                            
173200     MOVE W-IDLBBET           TO SHIP-IDLBBET                             
173300     MOVE W-IDDC              TO SHIP-IDDC                                
173400     MOVE W-IDLANDX2          TO SHIP-IDLANDX3-SEND                       
173500     IF 4498-KDFAKTYP = 'N' OR 'K'                                        
173600       MOVE 'INT'             TO SHIP-KDFINDOC                            
173700     ELSE                                                                 
173800       MOVE 'INV'             TO SHIP-KDFINDOC                            
173900     END-IF                                                               
174000     MOVE DC-MSGI-TILOKDAT    TO SHIP-TISKEPPN                            
174100                                                                          
174200     MOVE DC-MSGI-TILOKTID    TO WS-TISKPTID-HHMM                         
174300     MOVE FUNCTION CURRENT-DATE (13:2) TO                                 
174400                                 WS-TISKPTID-SS                           
174500     MOVE WS-TISKPTID         TO SHIP-TISKPTID                            
174600     MOVE ZERO                TO SHIP-KVANTEX                             
174700                                 SHIP-SUNTO-TOT                           
174800                                 SHIP-PRKURS-BET                          
174900     MOVE SPACE               TO SHIP-IDDC-EXP                            
175000                                 SHIP-BELEVVIL                            
175100                                 SHIP-IDSYSTEM                            
175200                                 SHIP-KDVALISO-BET                        
175300     MOVE '0'                 TO SHIP-KDFAKSTA-EXP                        
175400     MOVE W-FLSKRIV-NU        TO SHIP-FLSKRIV-NU                          
175500                                                                          
175600     MOVE NOO                 TO SHIP-FLFARLIG                            
175700     MOVE SPACES              TO SHIP-KDVALISO-EXP                        
175800     MOVE ZEROES              TO SHIP-SUORDV-EXP                          
175900                                 SHIP-SUORDV-FAKT                         
176000                                 SHIP-VKORDBTO-FAKT                       
176100                                 SHIP-VLORDBTO-FAKT                       
176200                                                                          
176300** AFTER ALL UPDATE AND RELEASE TRANSPORT SHIP-KDKLAR CHANGES             
176400** TO 'N'                                                                 
176500     MOVE 'A'                 TO SHIP-KDKLAR                              
176600     PERFORM IMS-ISRT-WDE101                                              
176700     ADD  +1                  TO W-UPD-COUNT                              
176800     .                                                                    
176900     EJECT                                                                
177000 S03-CREATE-WDE201 SECTION.                                               
177100                                                                          
177200     MOVE W-IDSHIPM      TO BILL-IDSHIPM                                  
177300     MOVE W-IDDC         TO BILL-IDDC                                     
177400     MOVE W-IDLANDX2     TO BILL-IDLANDX3-SEND                            
177500     MOVE SPACE          TO BILL-IDLEVNR                                  
177600     IF 4498-KDFAKTYP = 'N' OR 'K'                                        
177700       MOVE 'INT'        TO BILL-KDFINDOC                                 
177800     ELSE                                                                 
177900       MOVE 'INV'        TO BILL-KDFINDOC                                 
178000     END-IF                                                               
178100     MOVE DC-MSGI-TILOKDAT  TO BILL-TISKEPPN                              
178200     MOVE WS-TISKPTID       TO BILL-TISKPTID                              
178300     MOVE SPACE             TO BILL-IDDC-EXP                              
178400     PERFORM IMS-ISRT-WDE201                                              
178500     ADD  1                   TO W-UPD-COUNT                              
178600     .                                                                    
178700     EJECT                                                                
178800 S04-CHECK-KDORDKL SECTION.                                               
178900                                                                          
179000     MOVE W-IDPRODNR-PREV     TO W-IDPRODNR-KOLLI                         
179100     PERFORM IMS-GHU-WDE601                                               
179200                                                                          
179300     IF VORD-KDORDKL          >  W-KDORDKL-MAX                            
179400        MOVE VORD-KDORDKL     TO W-KDORDKL-MAX                            
179500     END-IF                                                               
179600     .                                                                    
179700     EJECT                                                                
179800                                                                          
179900 S06-DC-LAND  SECTION.                                                    
180000                                                                          
180100     IF W-IDDC NOT = W-IDDC-B6                                            
180200        MOVE W-IDDC TO W-IDDC-B6                                          
180300        PERFORM IMS-GU-WDB601                                             
180400     END-IF                                                               
180500     MOVE DCS-IDLANDX2 TO  W-IDLANDX2                                     
180600     .                                                                    
180700     EJECT                                                                
180800 S09A-START-W40539  SECTION.                                              
180900                                                                          
181000     MOVE W-IDDISTR-PREV       TO W-IDDISTR-NUM                           
181100     MOVE W-IDDISTR-NUM        TO 4539-MID-IDDISTR                        
181200                                                                          
181300     MOVE W-IDDEALER-PREV      TO W-IDKUNDNR-NUM                          
181400     MOVE W-IDKUNDNR-NUM       TO 4539-MID-IDKUNDNR                       
181500                                                                          
181600     MOVE MFS-KDMFSFOR         TO 4539-SPRAK                              
181700     MOVE '99'                 TO 4539-MID-KDFRAKT                        
181800     MOVE W-IDSHIPM            TO 4539-MID-IDSKEPPN                       
181900     MOVE W-IDDC               TO 4539-MID-IDDC                           
182000     MOVE '4675'               TO 4539-MID-IDSYSTEM                       
182100     MOVE W-IDTRPTNR           TO 4539-MID-IDTRPTNR                       
182200                                                                          
182300     PERFORM IMS-PURG-TRANS4539                                           
182400     .                                                                    
182500     EJECT                                                                
182600 S09B-START-W40540  SECTION.                                              
182700                                                                          
182800     MOVE W-IDDISTR-PREV       TO TEST-IDDISTR                            
182900                                  W-IDDISTR-NUM                           
183000     MOVE W-IDDISTR-NUM        TO 4540-MID-IDDISTR                        
183100                                                                          
183200     IF FLSAMFAK-PREV                                                     
183300       MOVE ZERO               TO 4540-MID-IDKUNDNR                       
183400     ELSE                                                                 
183500       MOVE W-IDKUNDNR-PREV    TO W-IDKUNDNR-NUM                          
183600       MOVE W-IDKUNDNR-NUM     TO 4540-MID-IDKUNDNR                       
183700     END-IF                                                               
183800                                                                          
183900     MOVE MFS-KDMFSFOR         TO 4540-SPRAK                              
184000                                                                          
184100     IF W-IDTRPTNR < +100                                                 
184200       MOVE '17'               TO 4540-MID-KDFRAKT                        
184300       MOVE W-IDKUNDNR-PREV    TO W-IDKUNDNR-NUM                          
184400       MOVE W-IDKUNDNR-NUM     TO 4540-MID-IDKUNDNR                       
184500     ELSE                                                                 
184600       IF DIST13-SVERIGE AND VORD-KDFRAKT = 32                            
184700         MOVE '32'             TO 4540-MID-KDFRAKT                        
184800       ELSE                                                               
184900         MOVE '99'             TO 4540-MID-KDFRAKT                        
185000       END-IF                                                             
185100     END-IF                                                               
185200                                                                          
185300     MOVE W-IDSHIPM            TO 4540-MID-IDSKEPPN                       
185400     MOVE W-IDDC               TO 4540-MID-IDDC                           
185500     MOVE '4675'               TO 4540-MID-IDSYSTEM                       
185600     MOVE W-IDTRPTNR           TO 4540-MID-IDTRPTNR                       
185700                                                                          
185800     PERFORM IMS-PURG-TRANS4540                                           
185900     .                                                                    
186000     EJECT                                                                
186100 S11-CHECK-FLSAMFAK SECTION.                                              
186200     SKIP2                                                                
186300*********************************************************                 
186400*                                                       *                 
186500* TO CHECK CUSTOMER FOR FLSAMFAK - CO-INVOICING         *                 
186600*                                                       *                 
186700*********************************************************                 
186800                                                                          
186900** FLSAMFAK IS SAME FOR ALL CUSTOMER IN ONE DISTRICT                      
187000     IF 4498-IDDISTR NOT = W-IDDISTR-PREV                                 
187100       MOVE NOO           TO FLSAMFAK-SW                                  
187200** FLSAMFAK IS ALWAYS 'N' FOR FOR IDKUNDNR = ZERO                         
187300       IF 4498-IDKUNDNR NOT = ZERO                                        
187400         MOVE 4498-IDDISTR  TO W-WDB201-IDDISTR                           
187500                               TEST-IDDISTR                               
187600         MOVE 4498-IDKUNDNR TO W-WDB201-IDKUNDNR                          
187700         PERFORM IMS-GU-WDB201                                            
187800         IF SEGMENT-FOUND                                                 
187900           IF DIST92-ITALY OR                                             
188000              DIST92-GREECE                                               
188100             MOVE NOO          TO GMT-FLSAMFAK                            
188200           END-IF                                                         
188300           IF GMT-FLSAMFAK = YES                                          
188400             MOVE YES  TO FLSAMFAK-SW                                     
188500           END-IF                                                         
188600         END-IF                                                           
188700       END-IF                                                             
188800     END-IF                                                               
188900     .                                                                    
189000     EJECT                                                                
189100                                                                          
189200 S15-CREATE-WDE111 SECTION.                                               
189300                                                                          
189400     MOVE  4498-IDDISTR     TO  SGMT-IDDISTR                              
189500     MOVE  4498-IDKUNDNR    TO  SGMT-IDKUNDNR                             
189600     MOVE  'N'              TO  SGMT-FLCOD                                
189700     MOVE  W-IDDC           TO  SGMT-IDDC                                 
189800     MOVE  SPACE            TO  SGMT-IDPARTNR                             
189900     MOVE  ZERO             TO  SGMT-KDFORSKN                             
190000                                SGMT-KDFKBIL                              
190100     MOVE  -1               TO  SGMT-KDLEVVIL                             
190200     MOVE  W-KDORDKL-MAX    TO  SGMT-KDORDKL-MAX                          
190300     MOVE  SPACE            TO  SGMT-KDVALISO                             
190400     MOVE  ZERO             TO  SGMT-PRKURS                               
190500     COMPUTE  SGMT-TISKEPPN-9KOMPL                                        
190600                            =   W-9KOMPL - SHIP-TISKEPPN                  
190700     PERFORM IMS-ISRT-WDE111                                              
190800     ADD  1                   TO W-UPD-COUNT                              
190900     .                                                                    
191000     EJECT                                                                
191100                                                                          
191200 S16-CREATE-WDE211 SECTION.                                               
191300                                                                          
191400     MOVE 4498-IDDISTR      TO BGMT-IDDISTR                               
191500     MOVE 4498-IDKUNDNR     TO BGMT-IDKUNDNR                              
191600     MOVE SPACE             TO BGMT-IDPARTNR                              
191700                               BGMT-FLSEPINV                              
191800     MOVE 'N'               TO BGMT-FLCOD                                 
191900     MOVE ZERO              TO BGMT-KDLEVVIL                              
192000                               BGMT-PRAVDRAG                              
192100                               BGMT-PREMBHNT                              
192200                               BGMT-PRFOERS                               
192300                               BGMT-PRFRAKT                               
192400                               BGMT-PRLEGKST                              
192500                               BGMT-REAVDRAG                              
192600                               BGMT-REEMBHNT                              
192700                               BGMT-REFOERS                               
192800                               BGMT-RELEGKST                              
192900                               BGMT-REOVKOFF                              
193000                                                                          
193100     PERFORM IMS-ISRT-WDE211                                              
193200     ADD  1                   TO W-UPD-COUNT                              
193300     .                                                                    
193400     EJECT                                                                
193500                                                                          
193600 S17-CREATE-WDE121 SECTION.                                               
193700                                                                          
193800     MOVE 4498-IDPRODNR          TO SKOLLI-IDPRODNR                       
193900     MOVE 4498-IDKOLLI           TO SKOLLI-IDKOLLI                        
194000     MOVE 4498-IDKOLLI-SAMP      TO SKOLLI-IDKOLLI-SAMP                   
194100     MOVE ZERO                   TO SKOLLI-DIKOLLIB                       
194200                                    SKOLLI-DIKOLLIH                       
194300                                    SKOLLI-DIKOLLIL                       
194400     MOVE SPACE                  TO SKOLLI-FLDIRLEV                       
194500     MOVE ZERO                   TO SKOLLI-IDORDER                        
194600                                    SKOLLI-KDEMBTYP                       
194700                                    SKOLLI-KDFRAKT                        
194800                                    SKOLLI-KDFARLIG-KOLLI                 
194900     MOVE SPACE                  TO SKOLLI-KDKOLLI                        
195000     MOVE ZERO                   TO SKOLLI-KVFLAMP-KOLLI                  
195100                                    SKOLLI-SUORDV-LOC                     
195200                                    SKOLLI-SUORDV-LOCPREL                 
195300     MOVE SPACE                  TO SKOLLI-KDVALISO                       
195400                                    SKOLLI-KDVALISO-EXP                   
195500     MOVE ZERO                   TO SKOLLI-SUORDV                         
195600                                    SKOLLI-SUORDV-EXP                     
195700                                    SKOLLI-TIPACKN                        
195800                                    SKOLLI-VKORDBTO-KOLLI                 
195900                                    SKOLLI-VKORDNTO-KOLLI                 
196000                                    SKOLLI-VLORDBTO-KOLLI                 
196100                                    SKOLLI-IDFAKT-EXP                     
196200     MOVE 4498-IDDISTR           TO SKOLLI-IDDISTR                        
196300     MOVE 4498-IDKUNDNR          TO SKOLLI-IDKUNDNR                       
196400     MOVE 4498-IDKUNDRF          TO SKOLLI-IDKUNDRF                       
196500     MOVE 4498-KDFAKTYP          TO SKOLLI-KDFAKTYP                       
196600     MOVE -1                     TO SKOLLI-KDORDKL                        
196700     MOVE ZERO                   TO SKOLLI-TIORDREG                       
196800                                    SKOLLI-IDFAKT                         
196900*CO-CD                                                                    
197000     MOVE 4498-FLCROSS           TO SKOLLI-FLCROSS                        
197100*CO-CD                                                                    
197200                                                                          
197300     PERFORM IMS-ISRT-WDE121                                              
197400     ADD  1                   TO W-UPD-COUNT                              
197500     .                                                                    
197600     EJECT                                                                
197700                                                                          
197800 S18-CREATE-WDE221 SECTION.                                               
197900                                                                          
198000     MOVE 4498-IDPRODNR          TO BKOLLI-IDPRODNR                       
198100     MOVE 4498-IDKOLLI           TO BKOLLI-IDKOLLI                        
198200     MOVE 4498-IDDISTR           TO BKOLLI-IDDISTR                        
198300     MOVE 4498-IDKUNDNR          TO BKOLLI-IDKUNDNR                       
198400     MOVE 4498-IDKUNDRF          TO BKOLLI-IDKUNDRF                       
198500     MOVE SPACE                  TO BKOLLI-FLOVRLEV                       
198600                                    BKOLLI-KDFAKTYP                       
198700*                                   BKOLLI-BEKUNDRF                       
198800*                                   BKOLLI-BERADREF                       
198900     MOVE -1                     TO BKOLLI-KDORDKL                        
199000     MOVE SPACE                  TO BKOLLI-KDPRSTA                        
199100     MOVE ZERO                   TO BKOLLI-VKORDBTO-KOLLI                 
199200*CO-CD                                                                    
199300     MOVE 4498-FLCROSS           TO BKOLLI-FLCROSS                        
199400*CO-CD                                                                    
199500                                                                          
199600     PERFORM IMS-ISRT-WDE221                                              
199700     ADD  1                   TO W-UPD-COUNT                              
199800     .                                                                    
199900     EJECT                                                                
200000                                                                          
200100 S21-OPEN-WZ01 SECTION.                                                   
200200                                                                          
200300     MOVE 'OPEN'                     TO SEND-KDFUNC                       
200400     MOVE 'CARPARTS.PULS.ADDIT   '   TO SEND-ADDISPABS                    
200500     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
200600                                                                          
200700     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
200800                                SEND-OPEN-AREA                            
200900     IF SEND-KDRC > 0                                                     
201000       MOVE SEND-KDRC           TO KDRC-DISP                              
201100       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
201200            DELIMITED BY SIZE INTO ERROR-TEXT                             
201300       CALL FELLOG                                                        
201400     ELSE                                                                 
201500       MOVE SEND-IDCOM               TO WS-IDCOM                          
201600     END-IF                                                               
201700     .                                                                    
201800     EJECT                                                                
201900 S22-SEND-WZ01 SECTION.                                                   
202000     MOVE W-IDSHIPM          TO MOD4636-MID-IDSHIPM                       
202100     MOVE 'J'                TO MOD4636-MID-KDTRPINF                      
202200     MOVE ZERO               TO MOD4636-MID-IDDISTR                       
202300                                MOD4636-MID-IDKUNDNR                      
202400                                MOD4636-MID-IDPRODNR                      
202500                                MOD4636-MID-IDKOLLI                       
202600                                MOD4636-MID-SUORDV-DIST                   
202700                                MOD4636-MID-SUORDV-DIST-LOC               
202800                                MOD4636-MID-SUORDV-DIST-PREL              
202900                                MOD4636-MID-SUORDV-NOLL                   
203000                                MOD4636-MID-SUORDV-NOLL-LOC               
203100                                MOD4636-MID-SUORDV-NOLL-PREL              
203200                                MOD4636-MID-KDORDKL                       
203300     MOVE 'PUT'                      TO SEND-KDFUNC                       
203400     COMPUTE SEND-KVDLEN = LENGTH OF MOD4636-MID-W40636I1                 
203500                                                                          
203600     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
203700                                SEND-KVDLEN                               
203800                                MOD4636-MID-W40636I1                      
203900     IF SEND-KDRC > 0                                                     
204000       MOVE SEND-KDRC           TO KDRC-DISP                              
204100       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
204200            DELIMITED BY SIZE INTO ERROR-TEXT                             
204300       CALL FELLOG                                                        
204400     END-IF                                                               
204500     .                                                                    
204600     EJECT                                                                
204700 S23-CLOSE-WZ01  SECTION.                                                 
204800                                                                          
204900     MOVE 'CLOSE'               TO SEND-KDFUNC                            
205000                                                                          
205100     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
205200     IF SEND-KDRC > 0                                                     
205300       MOVE SEND-KDRC           TO KDRC-DISP                              
205400       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
205500            DELIMITED BY SIZE INTO ERROR-TEXT                             
205600       CALL FELLOG                                                        
205700     END-IF                                                               
205800     .                                                                    
205900     EJECT                                                                
206000                                                                          
206100 S25-WRONG-PICTURE-MESSAGE SECTION.                                       
206200     SKIP2                                                                
206300* *****************************************************                   
206400*                                                     *                   
206500* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
206600*                                                     *                   
206700* *****************************************************                   
206800     SKIP2                                                                
206900     MOVE 'W0O50401'          TO MFS-IDMOD                                
207000     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
207100     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
207200     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
207300     PERFORM IMS-INSERT-MSG                                               
207400     .                                                                    
207500     EJECT                                                                
207600                                                                          
207700 MFS-ERASE-FIELD-OUT SECTION.                                             
207800                                                                          
207900     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-UT                              
208000                             MOD-IDLBBET-UT                               
208100                             MOD-FLFARLIG-UT                              
208200                             MOD-IDDC-UT                                  
208300     .                                                                    
208400     SKIP3                                                                
208500                                                                          
208600 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
208700                                                                          
208800     MOVE 1 TO INDX                                                       
208900     MOVE MFS-ERASE-FIELD TO MOD-FLAVSLUTA                                
209000                             MOD-FLSKRIV-NU                               
209100     PERFORM UNTIL INDX > MAX-INDX                                        
209200       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR (INDX)                         
209300                               MOD-IDKUNDNR (INDX)                        
209400       ADD 1 TO INDX                                                      
209500     END-PERFORM                                                          
209600     .                                                                    
209700     SKIP3                                                                
209800 MFS-ERASE-FIELD-IN SECTION.                                              
209900                                                                          
210000*    --- ALLA INDATA-FÄLT                                                 
210100     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-IN                              
210200                             MOD-IDLBBET-IN                               
210300                             MOD-FLFARLIG-IN                              
210400                             MOD-IDDC-IN                                  
210500     .                                                                    
210600     EJECT                                                                
210700 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
210800                                                                          
210900*    --- ALLA UTDATA-FÄLT                                                 
211000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-UT                       
211100                                    MOD-IDLBBET-UT                        
211200                                    MOD-FLFARLIG-UT                       
211300                                    MOD-IDDC-UT                           
211400                                    MOD-FLAVSLUTA                         
211500                                    MOD-FLSKRIV-NU                        
211600     MOVE +1 TO INDX                                                      
211700     PERFORM UNTIL INDX > MAX-INDX                                        
211800       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
211900       ADD +1 TO INDX                                                     
212000     END-PERFORM                                                          
212100     .                                                                    
212200     EJECT                                                                
212300     SKIP2                                                                
212400 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
212500                                                                          
212600*    --- OUTDATA FIELD ON SCROLL KEYS                                     
212700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR (INDX)                    
212800                                    MOD-IDKUNDNR (INDX)                   
212900                                    MOD-KDCMD (INDX)                      
213000     .                                                                    
213100     SKIP3                                                                
213200 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
213300                                                                          
213400*    --- ALLA INDATA-FÄLT                                                 
213500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-IN                       
213600                                    MOD-IDLBBET-IN                        
213700                                    MOD-FLFARLIG-IN                       
213800                                    MOD-IDDC-IN                           
213900     .                                                                    
214000     EJECT                                                                
214100 MFS-FORM-ATTR SECTION.                                                   
214200                                                                          
214300*    --- ALL INDATA-FIELDS                                                
214400     MOVE +1                        TO INDX                               
214500     PERFORM  UNTIL INDX > MAX-INDX                                       
214600       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDCMD-ATTR(INDX)               
214700       ADD +1                       TO INDX                               
214800     END-PERFORM                                                          
214900     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-FLAVSLUTA                        
215000     .                                                                    
215100     SKIP2                                                                
215200* --- IMS SECTIONS ---                                                    
215300     SKIP3                                                                
215400 IMS-GET-MSG SECTION.                                                     
215500                                                                          
215600     MOVE '  QC' TO GOOD-STATUSCODES                                      
215700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
215800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
215900     PERFORM IMS-STATUSCHECK                                              
216000     .                                                                    
216100     SKIP3                                                                
216200 IMS-INSERT-MSG SECTION.                                                  
216300                                                                          
216400*    IF MSGI-IDLAND-SPR = 'SE'                                            
216500*      MOVE '0' TO MFS-KDHUVOMR                                           
216600*    END-IF                                                               
216700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
216800     MOVE SPACE TO GOOD-STATUSCODES                                       
216900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
217000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
217100     PERFORM IMS-STATUSCHECK                                              
217200     .                                                                    
217300     EJECT                                                                
217400                                                                          
217500 IMS-ISRT-ALT-MSG-4675 SECTION.                                           
217600                                                                          
217700     MOVE SPACE TO GOOD-STATUSCODES                                       
217800     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
217900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
218000     PERFORM IMS-STATUSCHECK                                              
218100     .                                                                    
218200                                                                          
218300 IMS-ISRT-ALT-MSG-4675X SECTION.                                          
218400                                                                          
218500     MOVE SPACE TO GOOD-STATUSCODES                                       
218600     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
218700     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
218800     PERFORM IMS-STATUSCHECK                                              
218900     .                                                                    
219000     EJECT                                                                
219100 IMS-ISRT-ALT-MSG-4664 SECTION.                                           
219200                                                                          
219300     MOVE SPACE TO GOOD-STATUSCODES                                       
219400     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW                           
219500     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
219600     PERFORM IMS-STATUSCHECK                                              
219700     .                                                                    
219800     SKIP2                                                                
219900 IMS-PURG-TRANS4539 SECTION.                                              
220000                                                                          
220100     MOVE LOW-VALUE            TO 4539-Z1                                 
220200                                  4539-Z2                                 
220300     MOVE '    '               TO GOOD-STATUSCODES                        
220400     CALL CBLTDLI USING PURG ALT3-PCB 4539-MSG-IO-AREA                    
220500     MOVE ALT3-STATUS-CODE   TO STATUS-WS                                 
220600     PERFORM IMS-STATUSCHECK                                              
220700     .                                                                    
220800     EJECT                                                                
220900 IMS-PURG-TRANS4540 SECTION.                                              
221000                                                                          
221100     MOVE LOW-VALUE            TO 4540-Z1                                 
221200                                  4540-Z2                                 
221300     MOVE '    '               TO GOOD-STATUSCODES                        
221400     CALL CBLTDLI USING PURG ALT4-PCB 4540-MSG-IO-AREA                    
221500     MOVE ALT4-STATUS-CODE   TO STATUS-WS                                 
221600     PERFORM IMS-STATUSCHECK                                              
221700     .                                                                    
221800     EJECT                                                                
221900 IMS-ISRT-MSG-ALT6 SECTION.                                               
222000                                                                          
222100     MOVE SPACE TO GOOD-STATUSCODES                                       
222200     CALL CBLTDLI USING ISRT ALT6-PCB P-TO-P-SW                           
222300     MOVE ALT6-STATUS-CODE TO STATUS-WS                                   
222400     PERFORM IMS-STATUSCHECK                                              
222500     .                                                                    
222600     SKIP3                                                                
222700 IMS-GHU-WDR401-4495 SECTION.                                             
222800                                                                          
222900     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
223000                      DELIMITED BY SIZE INTO SSA1                         
223100     MOVE '  GE' TO GOOD-STATUSCODES                                      
223200     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-WDGX4495 SSA1                 
223300     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
223400     PERFORM IMS-STATUSCHECK                                              
223500     .                                                                    
223600     SKIP2                                                                
223700                                                                          
223800 IMS-DLET-WDR401-4495 SECTION.                                            
223900     MOVE '    ' TO GOOD-STATUSCODES                                      
224000     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-WDGX4495                     
224100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
224200     PERFORM IMS-STATUSCHECK                                              
224300     .                                                                    
224400     SKIP2                                                                
224500                                                                          
224600 IMS-GHNP-WDGX4498    SECTION.                                            
224700                                                                          
224800     MOVE 'WDGX4498' TO SSA1                                              
224900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
225000     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-WDGX4498  SSA1               
225100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
225200     PERFORM IMS-STATUSCHECK                                              
225300     .                                                                    
225400     SKIP2                                                                
225500 IMS-GHNP-WDGX4498-KEY    SECTION.                                        
225600                                                                          
225700     STRING 'WDGX4498(WDGXKEY  =' W-4498-X ')'                            
225800                      DELIMITED BY SIZE INTO SSA1                         
225900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
226000     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-WDGX4498  SSA1               
226100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
226200     PERFORM IMS-STATUSCHECK                                              
226300     .                                                                    
226400     SKIP2                                                                
226500 IMS-GHNP-WDGX4498-N     SECTION.                                         
226600                                                                          
226700     STRING 'WDGX4498(WDGXKEY  >' W-4498-N-X ')'                          
226800                      DELIMITED BY SIZE INTO SSA1                         
226900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
227000     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-WDGX4498  SSA1               
227100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
227200     PERFORM IMS-STATUSCHECK                                              
227300     .                                                                    
227400     SKIP2                                                                
227500 IMS-GHU-WDGX4498        SECTION.                                         
227600                                                                          
227700     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
227800                      DELIMITED BY SIZE INTO SSA1                         
227900     MOVE 'WDGX4498 '  TO  SSA2                                           
228000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
228100     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-WDGX4498 SSA1 SSA2            
228200     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
228300     PERFORM IMS-STATUSCHECK                                              
228400     .                                                                    
228500     SKIP2                                                                
228600 IMS-DLET-WDGX4498    SECTION.                                            
228700     MOVE '    ' TO GOOD-STATUSCODES                                      
228800     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-WDGX4498                     
228900     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
229000     PERFORM IMS-STATUSCHECK                                              
229100     .                                                                    
229200     SKIP2                                                                
229300                                                                          
229400 IMS-GHU-WDR401-4463  SECTION.                                            
229500                                                                          
229600     STRING 'WDR401  (WDGXKEY  =' W-4463-X ')'                            
229700             DELIMITED BY SIZE INTO SSA1                                  
229800     MOVE '  ' TO GOOD-STATUSCODES                                        
229900     CALL CBLTDLI USING GHU 4463-PCB DLI-IO-WDGX4463 SSA1                 
230000     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
230100     PERFORM IMS-STATUSCHECK                                              
230200     .                                                                    
230300     SKIP3                                                                
230400 IMS-ISRT-WDGX4464 SECTION.                                               
230500                                                                          
230600     STRING 'WDR401  (WDGXKEY  =' W-4463-X ')'                            
230700             DELIMITED BY SIZE INTO SSA1                                  
230800     MOVE 'WDGX4464' TO SSA2                                              
230900     MOVE '  II' TO GOOD-STATUSCODES                                      
231000     CALL CBLTDLI USING ISRT 4463-PCB DLI-IO-WDGX4464 SSA1 SSA2           
231100     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
231200     PERFORM IMS-STATUSCHECK                                              
231300     .                                                                    
231400     EJECT                                                                
231500 IMS-ISRT-WDGX4466 SECTION.                                               
231600                                                                          
231700     STRING 'WDR401  (WDGXKEY  =' W-4463-X ')'                            
231800             DELIMITED BY SIZE INTO SSA1                                  
231900     STRING 'WDGX4464(DASKEPPN =' W-DASKEPPN-X ')'                        
232000             DELIMITED BY SIZE INTO SSA2                                  
232100     MOVE   'WDGX4466' TO SSA3                                            
232200     MOVE '  II' TO GOOD-STATUSCODES                                      
232300     CALL CBLTDLI USING ISRT 4463-PCB DLI-IO-WDGX4466 SSA1 SSA2           
232400                                                      SSA3                
232500     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
232600     PERFORM IMS-STATUSCHECK                                              
232700     .                                                                    
232800     SKIP3                                                                
232900 IMS-ISRT-WDGX4468 SECTION.                                               
233000                                                                          
233100     STRING 'WDGX4464(DASKEPPN =' W-DASKEPPN-X ')'                        
233200             DELIMITED BY SIZE INTO SSA1                                  
233300     STRING 'WDGX4466(KY4466   =' W-4466-X ')'                            
233400             DELIMITED BY SIZE INTO SSA2                                  
233500     MOVE 'WDGX4468' TO SSA3                                              
233600     MOVE '  ' TO GOOD-STATUSCODES                                        
233700     CALL CBLTDLI USING ISRT 4463-PCB DLI-IO-WDGX4468                     
233800                        SSA1 SSA2 SSA3                                    
233900     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
234000     PERFORM IMS-STATUSCHECK                                              
234100     .                                                                    
234200     EJECT                                                                
234300 IMS-GHU-WDE601 SECTION.                                                  
234400                                                                          
234500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
234600            DELIMITED BY SIZE INTO SSA1                                   
234700     MOVE '  ' TO GOOD-STATUSCODES                                        
234800     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
234900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
235000     PERFORM IMS-STATUSCHECK                                              
235100     .                                                                    
235200     EJECT                                                                
235300 IMS-GHU-WDE611 SECTION.                                                  
235400                                                                          
235500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
235600            DELIMITED BY SIZE INTO SSA1                                   
235700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
235800            DELIMITED BY SIZE INTO SSA2                                   
235900     MOVE '  GE' TO GOOD-STATUSCODES                                      
236000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
236100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
236200     PERFORM IMS-STATUSCHECK                                              
236300     .                                                                    
236400     EJECT                                                                
236500 IMS-REPL-WDE6 SECTION.                                                   
236600                                                                          
236700     MOVE '  ' TO GOOD-STATUSCODES                                        
236800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
236900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
237000     PERFORM IMS-STATUSCHECK                                              
237100     .                                                                    
237200     EJECT                                                                
237300 IMS-ISRT-WDGX4514 SECTION.                                               
237400                                                                          
237500     STRING 'WDR401  (WDGXKEY  =' W-4513-X ')'                            
237600             DELIMITED BY SIZE INTO SSA1                                  
237700     MOVE 'WDGX4514' TO SSA2                                              
237800     MOVE '  II' TO GOOD-STATUSCODES                                      
237900     CALL CBLTDLI USING ISRT 4513-PCB DLI-IO-WDGX4514                     
238000                             SSA1 SSA2                                    
238100     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
238200     PERFORM IMS-STATUSCHECK                                              
238300     SKIP3                                                                
238400     .                                                                    
238500 IMS-ISRT-WDGX4516 SECTION.                                               
238600                                                                          
238700     STRING 'WDR401  (WDGXKEY  =' W-4513-X ')'                            
238800             DELIMITED BY SIZE INTO SSA1                                  
238900     STRING 'WDGX4514(DASKEPPN =' W-4514-X ')'                            
239000             DELIMITED BY SIZE INTO SSA2                                  
239100     MOVE 'WDGX4516' TO SSA3                                              
239200     MOVE '  ' TO GOOD-STATUSCODES                                        
239300     CALL CBLTDLI USING ISRT 4513-PCB DLI-IO-WDGX4516                     
239400                             SSA1 SSA2 SSA3                               
239500     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
239600     PERFORM IMS-STATUSCHECK                                              
239700     .                                                                    
239800     EJECT                                                                
239900 IMS-GU-WDB201 SECTION.                                                   
240000                                                                          
240100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
240200          DELIMITED BY SIZE INTO SSA1                                     
240300     MOVE '  GE' TO GOOD-STATUSCODES                                      
240400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
240500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
240600     PERFORM IMS-STATUSCHECK                                              
240700     .                                                                    
240800     SKIP3                                                                
240900 IMS-GHU-WDE101 SECTION.                                                  
241000                                                                          
241100     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
241200          DELIMITED BY SIZE INTO SSA1                                     
241300     MOVE '  ' TO GOOD-STATUSCODES                                        
241400     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
241500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
241600     PERFORM IMS-STATUSCHECK                                              
241700     .                                                                    
241800     EJECT                                                                
241900 IMS-REPL-WDE101 SECTION.                                                 
242000                                                                          
242100     MOVE '  ' TO GOOD-STATUSCODES                                        
242200     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE101                       
242300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
242400     PERFORM IMS-STATUSCHECK                                              
242500     .                                                                    
242600     EJECT                                                                
242700 IMS-ISRT-WDE101 SECTION.                                                 
242800                                                                          
242900     MOVE 'WDE101  ' TO SSA1                                              
243000     MOVE '  II' TO GOOD-STATUSCODES                                      
243100     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
243200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
243300     PERFORM IMS-STATUSCHECK                                              
243400     .                                                                    
243500     EJECT                                                                
243600 IMS-ISRT-WDE111 SECTION.                                                 
243700                                                                          
243800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
243900          DELIMITED BY SIZE INTO SSA1                                     
244000     MOVE 'WDE111  ' TO SSA2                                              
244100     MOVE '  II' TO GOOD-STATUSCODES                                      
244200     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
244300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
244400     PERFORM IMS-STATUSCHECK                                              
244500     .                                                                    
244600     EJECT                                                                
244700 IMS-ISRT-WDE121 SECTION.                                                 
244800                                                                          
244900     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
245000          DELIMITED BY SIZE INTO SSA1                                     
245100     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
245200          DELIMITED BY SIZE INTO SSA2                                     
245300     MOVE 'WDE121 ' TO SSA3                                               
245400     MOVE '  II' TO GOOD-STATUSCODES                                      
245500     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3        
245600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
245700     PERFORM IMS-STATUSCHECK                                              
245800     .                                                                    
245900     EJECT                                                                
246000 IMS-GHU-WDE201 SECTION.                                                  
246100                                                                          
246200     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
246300          DELIMITED BY SIZE INTO SSA1                                     
246400     MOVE '  ' TO GOOD-STATUSCODES                                        
246500     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
246600     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
246700     PERFORM IMS-STATUSCHECK                                              
246800     .                                                                    
246900     EJECT                                                                
247000                                                                          
247100 IMS-REPL-WDE201 SECTION.                                                 
247200                                                                          
247300     MOVE '  ' TO GOOD-STATUSCODES                                        
247400     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE201                       
247500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
247600     PERFORM IMS-STATUSCHECK                                              
247700     .                                                                    
247800     EJECT                                                                
247900                                                                          
248000 IMS-ISRT-WDE201 SECTION.                                                 
248100                                                                          
248200     MOVE 'WDE201  ' TO SSA1                                              
248300     MOVE '  II' TO GOOD-STATUSCODES                                      
248400     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
248500     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
248600     PERFORM IMS-STATUSCHECK                                              
248700     .                                                                    
248800     EJECT                                                                
248900                                                                          
249000 IMS-ISRT-WDE211 SECTION.                                                 
249100                                                                          
249200     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
249300          DELIMITED BY SIZE INTO SSA1                                     
249400     MOVE 'WDE211  ' TO SSA2                                              
249500     MOVE '  II' TO GOOD-STATUSCODES                                      
249600     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
249700     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
249800     PERFORM IMS-STATUSCHECK                                              
249900     .                                                                    
250000     EJECT                                                                
250100                                                                          
250200 IMS-ISRT-WDE221 SECTION.                                                 
250300                                                                          
250400     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
250500          DELIMITED BY SIZE INTO SSA1                                     
250600     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
250700          DELIMITED BY SIZE INTO SSA2                                     
250800     MOVE 'WDE221  ' TO SSA3                                              
250900     MOVE '  II' TO GOOD-STATUSCODES                                      
251000     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3        
251100     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
251200     PERFORM IMS-STATUSCHECK                                              
251300     .                                                                    
251400     EJECT                                                                
251500 IMS-GU-WDB601    SECTION.                                                
251600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
251700          DELIMITED BY SIZE INTO SSA1                                     
251800     MOVE '  GE' TO GOOD-STATUSCODES                                      
251900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
252000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
252100     PERFORM IMS-STATUSCHECK                                              
252200     IF SEGMENT-MISSING                                                   
252300         MOVE SPACE TO DCS-KDDC                                           
252400                       DCS-IDLANDX2                                       
252500     END-IF                                                               
252600     .                                                                    
252700                                                                          
252800 IMS-STATUSCHECK SECTION.                                                 
252900                                                                          
253000     SET STATUS-IX TO 1                                                   
253100     SEARCH GOOD-STATUS                                                   
253200       AT END                                                             
253300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
253400         DELIMITED BY SIZE INTO ERROR-TEXT                                
253500         CALL FELLOG                                                      
253600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
253700         CONTINUE                                                         
254000     END-SEARCH                                                           
260000     .                                                                    
