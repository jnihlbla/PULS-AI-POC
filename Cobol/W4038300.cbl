000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038300.                                                
000400 AUTHOR.         CAO-VAN NGU.                                             
000500 DATE-WRITTEN.   90/05/10.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    COUNT PU, COUNT MANPOWER NEEDED FOR DAILY ORDER                      
001100*    DB         WDD212             NUMBER OF LINES IN MEZZANNINE          
001200*               WDQ301             ORDER PART                             
001300*               WDGX4447-WDGX4448  PRC RULE                               
001400*               WDGX4437-WDGX4438  CALANDER                               
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W4T383                                              
001800*        MID:         W4I38301                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O38301                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800*    -COPY WY2000W1                                                       
002900     SKIP3                                                                
003000 77  IDPGM                       PIC X(08)   VALUE 'W4038300'.            
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  WS-IDTIDZON                 PIC X(2).                                
003500 77  WS-ORQA-ODEL-TILST-OD       PIC 9(11).                               
003600                                                                          
003700 01      WS-TIRFS-EDIT           PIC 9(11).                               
003800 01      FILLER REDEFINES WS-TIRFS-EDIT.                                  
003900   03    FILLER                  PIC X(1).                                
004000   03    WS-RFS-DATE             PIC 9(6).                                
004100   03    WS-RFS-TIME             PIC X(4).                                
004200                                                                          
004300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004600                                                                          
004700                                                                          
004800*    ---                                                                  
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  DATA-SW                     PIC X       VALUE 'N'.                   
005400     88  DATA-END                            VALUE 'J'.                   
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006100     88  ALLT-OK                             VALUE 'J'.                   
006200                                                                          
006300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006400     88  EGEN-MID                            VALUE '4383'.                
006500     88  GODK-MID                            VALUE '4383'.                
006600*                                                                         
006700 01  WS-KDMATT                   PIC X.                                   
006800     88 US-MEASUREMENT           VALUE 'U'.                               
006900     88 SIS-MEASUREMENT          VALUE 'S'.                               
007000*                                                                         
007100 01  WS-DCUSER.                                                           
007200     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
007300     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
007400     03 FILLER                   PIC X(1)   VALUE SPACE.                  
007500*                                                                         
007600*                                                                         
007700*      --- VALID IDDC CODES                                               
007800*                                                                         
007900*01    -COPY WWDC99                                                       
008000       EJECT                                                              
008100*----------------INPUT FROM SCREEN                                        
008200 01  FILLER.                                                              
008300*                                                                         
008400     05  WS-PFI-IDPRC.                                                    
008500         10  WS-PFI-IDPRCBAS       PIC X(3) VALUE SPACES.                 
008600         10  WS-PFI-IDPRCVAR       PIC X(1) VALUE SPACE.                  
008700*                                                                         
008800     05  WS-PFI-TIRFS                PIC 9(10) VALUE 0.                   
008900     05  RFS-FILLERA REDEFINES WS-PFI-TIRFS.                              
009000         10  WS-PFI-TIRFS-AAMMDD     PIC 9(06).                           
009100         10  WS-PFI-TIRFS-HHMM       PIC 9(04).                           
009200     05  WS-TIRFS                    PIC S9(11) COMP-3 VALUE +0.          
009300*                                                                         
009400     05  WS-PFI-IDTRP.                                                    
009500         10  WS-PFI-IDTRPLOS       PIC X(3) VALUE SPACES.                 
009600         10  WS-PFI-IDTRPVAR       PIC X(2) VALUE SPACES.                 
009700*                                                                         
009800     05  WS-PFI-TITRPAVT.                                                 
009900         10  WS-PFI-TITRPAVT-AAMMDD  PIC 9(06) VALUE 0.                   
010000         10  WS-PFI-TITRPAVT-HHMM    PIC 9(04) VALUE 0.                   
010100*                                                                         
010200     05  W-TITRPAVT.                                                      
010300         07 W-TITRPAVT-AAMMDD    PIC S9(7) COMP-3 VALUE +0.               
010400         07 W-TITRPAVT-HHMM      PIC S9(5) COMP-3 VALUE +0.               
010500*                                                                         
010600*-----------PFKEY DATA                                                    
010700*                                                                         
010800     05  WS-PF8-PFE-PFX.                                                  
010900         10  WS-PF8.                                                      
011000             15  WS-PF8-IDORDER      PIC S9(7) COMP-3 VALUE +0.           
011100             15  WS-PF8-IDDC         PIC X(02).                           
011200             15  WS-PF8-IDPRODNR     PIC S9(7) COMP-3 VALUE +0.           
011300             15  WS-PF8-IDPLKLST     PIC S9(3) COMP-3 VALUE +0.           
011400             15  WS-PF8-TIRFS        PIC S9(11) COMP-3 VALUE +0.          
011500             15  WS-PF8-IDPRCVAR     PIC X            VALUE ' '.          
011600             15  WS-PF8-TILST        PIC S9(11) COMP-3 VALUE +0.          
011700         10  WS-PFE.                                                      
011800             15  WS-PFE-IDORDER      PIC S9(7) COMP-3 VALUE +0.           
011900             15  WS-PFE-IDDC         PIC X(02).                           
012000             15  WS-PFE-IDPRODNR     PIC S9(7) COMP-3 VALUE +0.           
012100             15  WS-PFE-IDPLKLST     PIC S9(3) COMP-3 VALUE +0.           
012200             15  WS-PFE-TIRFS        PIC S9(11) COMP-3 VALUE +0.          
012300             15  WS-PFE-IDPRCVAR     PIC X            VALUE ' '.          
012400             15  WS-PFE-TILST        PIC S9(11) COMP-3 VALUE +0.          
012500         10  WS-PFX-KVBEMAN-DORD     PIC X(03) VALUE ' '.                 
012600         10  WS-PFX-KVPU-DORD        PIC X(03) VALUE ' '.                 
012700         10  WS-PFX-IDPRC.                                                
012800             15  WS-PFX-IDPRCBAS     PIC X(3) VALUE SPACES.               
012900             15  WS-PFX-IDPRCVAR     PIC X(1) VALUE ' '.                  
013000     EJECT                                                                
013100*                                                                         
013200*          DATE AND TIMES FROM DC-LOCAL                                   
013300*                                                                         
013400 01  FILLER.                                                              
013500     05  WS-LOCAL-DATE         PIC  9(06) VALUE 0.                        
013600     05  WS-LOCAL-TIME         PIC  9(08) VALUE 0.                        
013700     05  FILLER REDEFINES WS-LOCAL-TIME.                                  
013800         10  WS-LOCAL-TIHH     PIC 99.                                    
013900         10  WS-LOCAL-TIMM     PIC 99.                                    
014000         10  FILLER            PIC X(4).                                  
014100*                                                                         
014200 01  W40383-INDX.                                                         
014300     05  ARB-INDX                PIC S9(4)  COMP SYNC VALUE +0.           
014400     05  PU-INDX                 PIC S9(4)  COMP SYNC VALUE +0.           
014500     05  MZ-WHSE                 PIC S9(4)  COMP SYNC VALUE +0.           
014600     05  MAX-ARB-INDX            PIC S9(4)  VALUE +99   COMP SYNC.        
014700     05  MAX-PU-INDX             PIC S9(4)  VALUE +14   COMP SYNC.        
014800     05  MAX-PU-INDX-PF8         PIC S9(4)  VALUE +15   COMP SYNC.        
014900*                                                                         
015000     EJECT                                                                
015100 01  FILLER.                                                              
015200     05  WS-DB-TIHH              PIC S9(3)  COMP-3 VALUE +0.              
015300     05  WS-DB-TIMM              PIC S9(3)  COMP-3 VALUE +0.              
015400     05  WS-WRK-TIHH             PIC S9(3)  COMP-3 VALUE +0.              
015500     05  WS-WRK-TIMM             PIC S9(3)  COMP-3 VALUE +0.              
015600     05  WS-QOT-TIHH             PIC S9(3)  COMP-3 VALUE +0.              
015700     05  WS-RST-TIMM             PIC S9(3)  COMP-3 VALUE +0.              
015800     05  WS-MZ-KVRADER           PIC S9(5)  COMP-3 VALUE +0.              
015900     05  WS-60                   PIC S9(3)  COMP-3 VALUE +60.             
016000     05  WS-PU                   PIC S9(3)  COMP-3 VALUE +0.              
016100     05  WS-TIHHMM               PIC 9(3)V99 VALUE 0.                     
016200     05  FILLER REDEFINES WS-TIHHMM.                                      
016300         10  WS-TIHH             PIC 9(3).                                
016400         10  WS-TIMM             PIC 9(2).                                
016500     05  WS-ODEL-DARFS           PIC 9(12) VALUE 0.                       
016600     05  FILLER REDEFINES WS-ODEL-DARFS.                                  
016700         10  FILLER               PIC 9(2).                               
016800         10  WS-ODEL-DARFS-AAMMDD PIC 9(6).                               
016900         10  WS-ODEL-DARFS-HHMM   PIC 9(4).                               
017000                                                                          
017100 01  FILLER.                                                              
017200     05  WS-WRKDAYS              PIC S9(5)  COMP-3 VALUE +0.              
017300     05  WS-WRKTIMES             PIC S9(5)  COMP-3 VALUE +0.              
017400     EJECT                                                                
017500*                                                                         
017600 01  WS-IDPRC-RULES.                                                      
017700     05  WS-KDPRODKL-PRC         PIC X            VALUE 'D'.              
017800     05  WS-IDPRC-PRC            PIC X(4)         VALUE SPACES.           
017900     05  WS-KVORDER-PRC          PIC S9(7)      COMP-3 VALUE +0.          
018000     05  WS-KVRADER-PRC          PIC S9(7)      COMP-3 VALUE +0.          
018100     05  WS-KVVTID-PRC           PIC S9(3)V9(2) COMP-3 VALUE +0.          
018200     05  WS-VKORDNTO-PRC         PIC S9(6)V9(1) COMP-3 VALUE +0.          
018300     05  WS-VLORDNTO-PRC         PIC S9(4)V9(3) COMP-3 VALUE +0.          
018400     05  SHOW-ALL-ORDERS         PIC S9(7) COMP-3 VALUE +9999999.         
018500*                                                                         
018600 01  WS-TISTASTO-PAC.                                                     
018700         10  WS-4438-TISTAMIN    PIC S9(3)V9(2) COMP-3 VALUE +0.          
018800         10  WS-4438-TISTOMIN    PIC S9(3)V9(2) COMP-3 VALUE +0.          
018900         10  WS-4438-TIACTMIN    PIC S9(3)V9(2) COMP-3 VALUE +0.          
019000         10  WS-RFSTID           PIC S9(3)V9(2) COMP-3 VALUE +0.          
019100*                                                                         
019200 01  WS-PICKER.                                                           
019300     05  FILLER OCCURS 99 TIMES.                                          
019400         10  WS-TISTAMIN-PAC     PIC S9(3)V9(2) COMP-3.                   
019500         10  WS-TIACTMIN-PAC     PIC S9(3)V9(2) COMP-3.                   
019600         10  WS-TISTOMIN-PAC     PIC S9(3)V9(2) COMP-3.                   
019700*                                                                         
019800     EJECT                                                                
019900*                                                                         
020000 01  WS-SCR4383.                                                          
020100     03  WS-SCR-DATA  OCCURS 14 TIMES.                                    
020200         05  WS-KVRADER-PU           PIC S9(5)      COMP-3.               
020300         05  WS-KVORDER-PU           PIC S9(5)      COMP-3.               
020400         05  WS-KVRADER-MEZ-PU       PIC S9(5)      COMP-3.               
020500         05  WS-TIRFS-PU             PIC S9(11)     COMP-3.               
020600         05  WS-TILST-OD-PU          PIC S9(11)     COMP-3.               
020700         05  WS-SUPTID-PU            PIC S9(3)V9(2) COMP-3.               
020800         05  WS-VKORDNTO-PU          PIC S9(6)V9(1) COMP-3.               
020900         05  WS-VLORDNTO-PU          PIC S9(4)V9(3) COMP-3.               
021000         05  WS-IDTRP-PU             PIC X(5).                            
021100*                                                                         
021200 01  WS-PU-DATA.                                                          
021300         05  W1-TILST-OD-PU     PIC S9(11) COMP-3 VALUE +0.               
021400         05  W1-TIRFS-PU        PIC S9(11) COMP-3 VALUE +0.               
021500         05  W1-IDTRP-PU        PIC X(5)   VALUE SPACES.                  
021600 01  WS-PU-CUMU-00.                                                       
021700         05  FILLER             PIC S9(5)      COMP-3 VALUE +0.           
021800         05  FILLER             PIC S9(5)      COMP-3 VALUE +0.           
021900         05  FILLER             PIC S9(5)      COMP-3 VALUE +0.           
022000         05  FILLER             PIC S9(3)V9(2) COMP-3 VALUE +0.           
022100         05  FILLER             PIC S9(6)V9(1) COMP-3 VALUE +0.           
022200         05  FILLER             PIC S9(4)V9(3) COMP-3 VALUE +0.           
022300 01  WS-PU-CUMULATOR.                                                     
022400         05  W1-KVORDER-PU      PIC S9(5)      COMP-3 VALUE +0.           
022500         05  W1-KVRADER-PU      PIC S9(5)      COMP-3 VALUE +0.           
022600         05  W1-KVRADER-MEZ-PU  PIC S9(5)      COMP-3 VALUE +0.           
022700         05  W1-SUPTID-PU       PIC S9(3)V9(2) COMP-3 VALUE +0.           
022800         05  W1-VKORDNTO-PU     PIC S9(6)V9(1) COMP-3 VALUE +0.           
022900         05  W1-VLORDNTO-PU     PIC S9(4)V9(3) COMP-3 VALUE +0.           
023000*                                                                         
023100 01  FILLER.                                                              
023200     05  WS-IDPRC-4448           PIC X(4).                                
023300*                                                                         
023400*    --- CALLED PROGRAMS                                                  
023500 01  GENERAL-SUBPROGRAM.                                                  
023600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
023700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
023800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
024100     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
024200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
024300*01 -COPY WMSGINIT                                                        
024400     SKIP3                                                                
024500*01 -COPY WMSGINIT -PRE DC-                                               
024600     EJECT                                                                
024700     SKIP3                                                                
024800 01  FILLER                      PIC X(16) VALUE 'WWOMVAND-AREA'.         
024900*01  -COPY WWOMVAND                                                       
025000     SKIP3                                                                
025100*    --- PARAMETERS TO WORKDAY                                            
025200*   -COPY WORKAREA                                                        
025300     EJECT                                                                
025400*    --- PARAMETTERS TO WMEDKONV                                          
025500*   -COPY WMEDAREA                                                        
025600     EJECT                                                                
025700*    --- AREA FOR MFS                                                     
025800*                                                                         
025900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
026000     SKIP3                                                                
026100*01  MID -COPY W4I38301                                                   
026200     EJECT                                                                
026300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
026400     SKIP3                                                                
026500*01  -COPY WMSGAREA                                                       
026600     EJECT                                                                
026700*    03  MOD -COPY W4O38301   -RED MSG-AREA.                              
026800     EJECT                                                                
026900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027000     SKIP3                                                                
027100*01  -COPY WMFSAREA                                                       
027200     EJECT                                                                
027300*    --- IO-AREA TO/FROM DL/I                                             
027400*                                                                         
027500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027600     SKIP3                                                                
027700 01  NYCKLAR-TILL-DLI.                                                    
027800     03  W-IDPRC-LOW.                                                     
027900         07  W-IDPRCBAS-LOW      PIC X(3).                                
028000         07  W-IDPRCVAR-LOW      PIC X(1).                                
028100     03  W-IDPRC-UP.                                                      
028200         07  W-IDPRCBAS-UP       PIC X(3).                                
028300         07  W-IDPRCVAR-UP       PIC X(1).                                
028400*-----------------------------------------------------------              
028500     03  W-MIN-WDQ3A1KY.                                                  
028600         07 W-MIQA-IDDC          PIC X(02).                               
028700         07 W-MIQA-IDTRP.                                                 
028800            11 W-MIQA-IDTRPLOS   PIC X(03) VALUE SPACES.                  
028900            11 W-MIQA-IDTRPVAR   PIC X(02) VALUE SPACES.                  
029000         07 W-MIQA-DATRPAVT.                                              
029100            11 W-MIQA-DATRPAVD         PIC  9(8) VALUE ZERO.              
029200            11 W-MIQA-DATRPAVT-HHMM    PIC S9(5) COMP-3 VALUE +0.         
029300         07 W-MIQA-DARFS         PIC  9(12)  VALUE ZERO.                  
029400         07 W-MIQA-TILST-O       PIC  9(12)  VALUE ZERO.                  
029500         07 W-MIQA-IDPRC.                                                 
029600            11 W-MIQA-IDPRCBAS   PIC X(03) VALUE SPACES.                  
029700            11 W-MIQA-IDPRCVAR   PIC X(01) VALUE SPACES.                  
029800         07 W-MIQA-WDQ301.                                                
029900            09 W-MIQA-IDORDER       PIC S9(07)  COMP-3 VALUE +0.          
030000            09 W-MIQA-IDPRODNR      PIC S9(07)  COMP-3 VALUE +0.          
030100            09 W-MIQA-IDPLKLST      PIC S9(03)  COMP-3 VALUE +0.          
030200*                                                                         
030300     03  W-MAX-WDQ3A1KY.                                                  
030400         07 W-MXQA-IDDC          PIC X(02).                               
030500         07 W-MXQA-IDTRP.                                                 
030600            11 W-MXQA-IDTRPLOS   PIC X(03) VALUE SPACES.                  
030700            11 W-MXQA-IDTRPVAR   PIC X(02) VALUE SPACES.                  
030800         07 W-MXQA-DATRPAVT.                                              
030900            11 W-MXQA-DATRPAVD         PIC  9(8) VALUE ZERO.              
031000            11 W-MXQA-DATRPAVT-HHMM    PIC S9(5) COMP-3 VALUE +0.         
031100         07 W-MXQA-DARFS         PIC  9(12)  VALUE ZERO.                  
031200         07 W-MXQA-TILST-O       PIC  9(12)  VALUE ZERO.                  
031300         07 W-MXQA-IDPRC.                                                 
031400            11 W-MXQA-IDPRCBAS   PIC X(03) VALUE SPACES.                  
031500            11 W-MXQA-IDPRCVAR   PIC X(01) VALUE SPACES.                  
031600         07 W-MXQA-WDQ301.                                                
031700            09 W-MXQA-IDORDER       PIC S9(07)  COMP-3 VALUE +0.          
031800            09 W-MXQA-IDPRODNR      PIC S9(07)  COMP-3 VALUE +0.          
031900            09 W-MXQA-IDPLKLST      PIC S9(03)  COMP-3 VALUE +0.          
032000*                                                                         
032100     EJECT                                                                
032200*                                                                         
032300     03  W-MIN-WDQ3B1KY.                                                  
032400         07 W-MIQB-IDDC          PIC X(02).                               
032500         07 W-MIQB-IDPRCBAS      PIC X(03)  VALUE SPACES.                 
032600         07 W-MIQB-DAUTSKR       PIC  9(08) VALUE ZERO.                   
032700         07 W-MIQB-TIUTSTID      PIC S9(07)  COMP-3 VALUE +0.             
032800         07 W-MIQB-DARFS         PIC  9(12)  VALUE ZERO.                  
032900         07 W-MIQB-TILST-O       PIC  9(12)  VALUE ZERO.                  
033000         07 W-MIQB-IDPRCVAR      PIC X(01) VALUE SPACES.                  
033100         07 W-MIQB-WDQ301.                                                
033200            09 W-MIQB-IDORDER       PIC S9(07)  COMP-3 VALUE +0.          
033300            09 W-MIQB-IDPRODNR      PIC S9(07)  COMP-3 VALUE +0.          
033400            09 W-MIQB-IDPLKLST      PIC S9(03)  COMP-3 VALUE +0.          
033500*                                                                         
033600     03  W-MAX-WDQ3B1KY.                                                  
033700         07 W-MXQB-IDDC          PIC X(02).                               
033800         07 W-MXQB-IDPRCBAS      PIC X(03)  VALUE SPACES.                 
033900         07 W-MXQB-DAUTSKR       PIC  9(08) VALUE ZERO.                   
034000         07 W-MXQB-TIUTSTID      PIC S9(07)  COMP-3 VALUE +0.             
034100         07 W-MXQB-DARFS         PIC  9(12)  VALUE ZERO.                  
034200         07 W-MXQB-TILST-O       PIC  9(12)  VALUE ZERO.                  
034300         07 W-MXQB-IDPRCVAR      PIC X(01) VALUE SPACES.                  
034400         07 W-MXQB-WDQ301.                                                
034500            09 W-MXQB-IDORDER       PIC S9(07)  COMP-3 VALUE +0.          
034600            09 W-MXQB-IDPRODNR      PIC S9(07)  COMP-3 VALUE +0.          
034700            09 W-MXQB-IDPLKLST      PIC S9(03)  COMP-3 VALUE +0.          
034800     EJECT                                                                
034900*                                                                         
035000     03  W-4437-WDGXKEY.                                                  
035100         07 W-4437-IDHTYP        PIC X(4)    VALUE '4437'.                
035200         07 W-4437-IDDC          PIC X(02).                               
035300         07 W-4437-IDPRC         PIC X(4)    VALUE SPACES.                
035400         07 W-4437-LOW-VALUE     PIC X(20)   VALUE LOW-VALUE.             
035500*                                                                         
035600     03  W-4438-WDGXKEY.                                                  
035700         07 W-4438-DADATUM       PIC 9(8)    VALUE ZERO.                  
035800*                                                                         
035900     03  W-4447-WDGXKEY.                                                  
036000         07 W-4447-IDHTYP        PIC X(4)    VALUE '4447'.                
036100         07 W-4447-IDDC          PIC X(02).                               
036200         07 W-4447-LOW-VALUE     PIC X(24)   VALUE LOW-VALUE.             
036300*                                                                         
036400     03  W-4448-WDGXKEY.                                                  
036500         07 W-4448-IDPRC.                                                 
036600            09 W-4448-IDPRCBAS   PIC X(3)    VALUE SPACE.                 
036700            09 W-4448-IDPRCVAR   PIC X(1)    VALUE SPACE.                 
036800         07 W-4448-LOW-VALUE     PIC X(1)    VALUE LOW-VALUE.             
036900*                                                                         
037000     EJECT                                                                
037100     03  W-ORQA-WDQ301KY.                                                 
037200         07  W-ORQA-IDORDER      PIC S9(7) COMP-3 VALUE +0.               
037300         07  W-ORQA-IDDC         PIC X(02).                               
037400         07  W-ORQA-IDPRODNR     PIC S9(7) COMP-3 VALUE +0.               
037500         07  W-ORQA-IDPLKLST     PIC S9(3) COMP-3 VALUE +0.               
037600*                                                                         
037700     03  W-IDORDER-X.                                                     
037800         07 W-IDORDER            PIC S9(7) COMP-3 VALUE +0.               
037900*                                                                         
038000     03  W-IDDC-X.                                                        
038100         07 W-IDDC               PIC X(02).                               
038200*                                                                         
038300     03  W-ADLAGOMR-X.                                                    
038400         07 W-ADLAGOMR           PIC S9(3) COMP-3 VALUE +0.               
038500     EJECT                                                                
038600*-----------------------------------------------------------------        
038700*    --- STATUS-CODE FROM IMS                                             
038800 01  STATUS-WS                   PIC XX.                                  
038900     88  SEGMENT-OK                          VALUE '  '.                  
039000     88  SEGMENT-II                          VALUE 'II'.                  
039100     88  SEGMENT-GE                          VALUE 'GE'.                  
039200     88  SEGMENT-GB                          VALUE 'GB'.                  
039300     SKIP2                                                                
039400 01  GODK-STATUSKODER.                                                    
039500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039600     SKIP3                                                                
039700 01  SSA1                        PIC X(192).                              
039800 01  SSA2                        PIC X(192).                              
039810 01  SSA3                        PIC X(192).                              
039900     EJECT                                                                
040000*    --- IMS FUNKTIONSKODER                                               
040100*01  -COPY W0003                                                          
040200     EJECT                                                                
040300*    ---  DLI INPUT-OUTPUT AREA                                           
040400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
040500     SKIP3                                                                
040600 01  DLI-IO-AREA1.                                                        
040700     03  IO-AREA1                PIC X(250) VALUE SPACE.                  
040800     SKIP3                                                                
040900     03  WDQ221   REDEFINES IO-AREA1.                                     
041000*        05  -COPY WDQ221                                                 
041100     EJECT                                                                
041200     03  WLORQA01 REDEFINES IO-AREA1.                                     
041300*        05  -COPY WDQ301     -PRE ORQA-                                  
041400     EJECT                                                                
041500     03  WLORQB01 REDEFINES IO-AREA1.                                     
041600*        05  -COPY WDQ3A1     -PRE ORQB-                                  
041700     EJECT                                                                
041800     03  WLORQC01 REDEFINES IO-AREA1.                                     
041900*        05  -COPY WDQ3B1     -PRE ORQC-                                  
042000     EJECT                                                                
042100 01  DLI-IO-AREA2.                                                        
042200     03  IO-AREA2                PIC X(300) VALUE SPACE.                  
042300     SKIP3                                                                
042400     03  WLXXKH01 REDEFINES IO-AREA2.                                     
042500*        05  -COPY WDGX4447   -PRE XXKH-                                  
042600     EJECT                                                                
042700     03  WLXXKH11 REDEFINES IO-AREA2.                                     
042800*        05  -COPY WDGX4448   -PRE XXKH-                                  
042900     EJECT                                                                
043000 01  DLI-IO-AREA3.                                                        
043100     03  IO-AREA3                PIC X(100) VALUE SPACE.                  
043200     SKIP3                                                                
043300     03  WL443701 REDEFINES IO-AREA3.                                     
043400*        05  -COPY WDGX4437   -PRE 4437-                                  
043500     EJECT                                                                
043600     03  WL443711 REDEFINES IO-AREA3.                                     
043700*        05  -COPY WDGX4438   -PRE 4437-                                  
043800     EJECT                                                                
043900 LINKAGE SECTION.                                                         
044000                                                                          
044100*01  -COPY W0009      -PRE MSG-                                           
044200     EJECT                                                                
044300                                                                          
044400                                                                          
044500*01  -COPY W0008      -PRE USEA-                                          
044600     05  FILLER                  PIC X(1).                                
044700     EJECT                                                                
044800                                                                          
044900*01  -COPY W0008      -PRE 4437-                                          
045000     05  FILLER                  PIC X(1).                                
045100     EJECT                                                                
045200                                                                          
045300*01  -COPY W0008      -PRE XXKH-                                          
045400     05  FILLER                  PIC X(1).                                
045500     EJECT                                                                
045600*01  -COPY W0008      -PRE WDQ2-                                          
045700     05  FILLER                  PIC X(1).                                
045800     EJECT                                                                
045900                                                                          
046000*01  -COPY W0008      -PRE ORQA-                                          
046100     05  FILLER                  PIC X(1).                                
046200     EJECT                                                                
046300                                                                          
046400*01  -COPY W0008      -PRE ORQB-                                          
046500     05  FILLER                  PIC X(1).                                
046600     EJECT                                                                
046700                                                                          
046800*01  -COPY W0008      -PRE ORQC-                                          
046900     05  FILLER                  PIC X(1).                                
047000     EJECT                                                                
047100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 4437-PCB XXKH-PCB             
047200                       WDQ2-PCB ORQA-PCB ORQB-PCB ORQC-PCB.               
047300 W40383 SECTION.                                                          
047400                                                                          
047500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 4437-PCB XXKH-PCB             
047600                          WDQ2-PCB ORQA-PCB ORQB-PCB ORQC-PCB.            
047700     PERFORM IMS-GET-MSG                                                  
047800     IF SEGMENT-OK                                                        
047900        PERFORM A-INIT                                                    
048000        PERFORM B-KOLLA-NYCKLAR                                           
048100        IF NYCKLAR-OK                                                     
048200           IF MFS-FIRST                                                   
048300           OR (NDC-US AND MFS-ENTER)                                      
048400              PERFORM C-FOERSTA-SIDA                                      
048500           ELSE                                                           
048600              IF MFS-NEXT                                                 
048700                 PERFORM D-NAESTA-SIDA                                    
048800              ELSE                                                        
048900                 PERFORM E-SAMMA-SIDA                                     
049000              END-IF                                                      
049100           END-IF                                                         
049200           IF ALLT-OK                                                     
049300             PERFORM F-LAES-VISA-INFO                                     
049400           END-IF                                                         
049500        END-IF                                                            
049600        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O38301 + 4                     
049700        PERFORM IMS-INSERT-MSG                                            
049800     END-IF                                                               
049900     MOVE ZERO TO RETURN-CODE                                             
050000     GOBACK                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 A-INIT SECTION.                                                          
050400                                                                          
050500     IF MSG-DUBBLA-TRANSKODER                                             
050600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I38301                 
050700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
050800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
050900     ELSE                                                                 
051000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I38301                  
051100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
051200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
051300     END-IF                                                               
051400                                                                          
051500     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
051600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
051700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
051800                                                                          
051900     MOVE LOW-VALUE TO MSG-AREA                                           
052000     MOVE '4383' TO MOD-IDTRANS                                           
052100     MOVE 'W4O383N1' TO MFS-IDMOD                                         
052200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
052300                                                                          
052400     IF NOT EGEN-MID                                                      
052500       MOVE SPACE TO MFS-KDTRTYP                                          
052600       MOVE '7' TO MFS-IDPFK                                              
052700       MOVE '++++'          TO MID-PFI-IDPRC                              
052800       MOVE '++++++'        TO MID-PFI-TIRFS-AAMMDD                       
052900       MOVE '++++'          TO MID-PFI-TIRFS-HHMM                         
053000       MOVE '+++++'         TO MID-PFI-IDTRP                              
053100       MOVE '++++++'        TO MID-PFI-TITRPAVT-AAMMDD                    
053200       MOVE '++++'          TO MID-PFI-TITRPAVT-HHMM                      
053300       MOVE  ALL '+'        TO MID-W4I38301                               
053400     END-IF                                                               
053500                                                                          
053600     IF ENGLISH-TEXT                                                      
053700       MOVE +35             TO W-ADLAGOMR                                 
053800     ELSE                                                                 
053900       MOVE +10             TO W-ADLAGOMR                                 
054000     END-IF                                                               
054100                                                                          
054200     MOVE +99               TO MAX-ARB-INDX                               
054300     MOVE +14               TO MAX-PU-INDX                                
054400     MOVE +15               TO MAX-PU-INDX-PF8                            
054500                                                                          
054600     MOVE +1 TO  ARB-INDX                                                 
054700     PERFORM UNTIL ARB-INDX > MAX-ARB-INDX                                
054800       MOVE ZERO            TO WS-TISTAMIN-PAC(ARB-INDX)                  
054900                               WS-TIACTMIN-PAC(ARB-INDX)                  
055000                               WS-TISTOMIN-PAC(ARB-INDX)                  
055100       ADD  +1 TO  ARB-INDX                                               
055200     END-PERFORM                                                          
055300     .                                                                    
055400     EJECT                                                                
055500 B-KOLLA-NYCKLAR SECTION.                                                 
055600                                                                          
055700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
055800     MOVE '013'             TO MSGI-KDCALL                                
055900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
056000     MOVE '4383'            TO MSGI-IDTRANS                               
056100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
056200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
056300     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
056400     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
056500                                                                          
056600     MOVE JA  TO NYCKLAR-SW                                               
056700     PERFORM MFS-ERASE-MOD-PFI                                            
056800     PERFORM BA-CHCK-IDPRC                                                
056900     PERFORM BB-CHCK-TIRFS                                                
057000     PERFORM BC-CHCK-IDTRP                                                
057100     PERFORM BD-CHCK-TITRPAVT                                             
057200                                                                          
057300     MOVE MSGI-IDDC         TO WS-IDDC                                    
057400     MOVE '013'             TO DC-MSGI-KDCALL                             
057500     MOVE WS-IDDC           TO WS-DCUSER-IDDC                             
057600     MOVE WS-DCUSER         TO DC-MSGI-IDUSER                             
057700     MOVE MSG-LTERM-NAME    TO DC-MSGI-IDLTERM-USER                       
057800     MOVE '4383'            TO DC-MSGI-IDTRANS                            
057900     CALL W005INIT USING DC-MSGI-WMSGINIT USEA-PCB                        
058000     MOVE DC-MSGI-TILOKDAT  TO WS-LOCAL-DATE                              
058100                                                                          
058200     PERFORM BE-CHCK-COMBIN                                               
058300     PERFORM BF-READ-PF8-PFE-DATA                                         
058400     PERFORM BG-CHCK-DATA                                                 
058500     PERFORM BH-RIGHT-KEY                                                 
058600     PERFORM BI-WRONG-KEY                                                 
058700     .                                                                    
058800     EJECT                                                                
058900 BA-CHCK-IDPRC SECTION.                                                   
059000                                                                          
059100     IF MID-PFI-IDPRC = ALL '+'                                           
059200       MOVE MID-PF7-IDPRC TO WS-PFI-IDPRC                                 
059300     ELSE                                                                 
059400       MOVE MID-PFI-IDPRC TO WS-PFI-IDPRC                                 
059500       MOVE '7'           TO MFS-IDPFK                                    
059600       MOVE SPACE         TO MFS-KDTRTYP                                  
059700     END-IF                                                               
059800                                                                          
059900     IF WS-PFI-IDPRC = SPACES                                             
060000       MOVE NEJ TO NYCKLAR-SW                                             
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 BB-CHCK-TIRFS SECTION.                                                   
060500                                                                          
060600     IF MID-PFI-TIRFS-AAMMDD = ALL '+'                                    
060700       INSPECT MID-PF7-TIRFS-AAMMDD REPLACING                             
060800                              LEADING SPACE BY ZERO                       
060900       MOVE MID-PF7-TIRFS-AAMMDD TO WS-PFI-TIRFS-AAMMDD                   
061000     ELSE                                                                 
061100       INSPECT MID-PFI-TIRFS-AAMMDD REPLACING                             
061200                              LEADING SPACE BY ZERO                       
061300       IF MID-PFI-TIRFS-AAMMDD NOT NUMERIC                                
061400          MOVE 0 TO WS-PFI-TIRFS-AAMMDD                                   
061500       ELSE                                                               
061600          MOVE MID-PFI-TIRFS-AAMMDD TO WS-PFI-TIRFS-AAMMDD                
061700       END-IF                                                             
061800       MOVE '7'             TO MFS-IDPFK                                  
061900       MOVE SPACE           TO MFS-KDTRTYP                                
062000     END-IF                                                               
062100                                                                          
062200     IF MID-PFI-TIRFS-HHMM = ALL '+'                                      
062300       INSPECT MID-PF7-TIRFS-HHMM REPLACING                               
062400                              LEADING SPACE BY ZERO                       
062500       MOVE MID-PF7-TIRFS-HHMM TO WS-PFI-TIRFS-HHMM                       
062600     ELSE                                                                 
062700       INSPECT MID-PFI-TIRFS-HHMM REPLACING                               
062800                              LEADING SPACE BY ZERO                       
062900       IF MID-PFI-TIRFS-HHMM NOT NUMERIC                                  
063000          MOVE 0 TO WS-PFI-TIRFS-HHMM                                     
063100       ELSE                                                               
063200          MOVE MID-PFI-TIRFS-HHMM TO WS-PFI-TIRFS-HHMM                    
063300       END-IF                                                             
063400       MOVE '7'             TO MFS-IDPFK                                  
063500       MOVE SPACE           TO MFS-KDTRTYP                                
063600     END-IF                                                               
063700                                                                          
063800     IF WS-PFI-TIRFS-AAMMDD > 0 AND                                       
063900        WS-PFI-TIRFS-AAMMDD NUMERIC                                       
064000        MOVE WS-PFI-TIRFS-AAMMDD   TO TMP1-YYMMDD                         
064100        MOVE WS-LOCAL-DATE         TO TMP2-YYMMDD                         
064200        PERFORM WY2000P1                                                  
064300        IF TMP1-YYMMDD < TMP2-YYMMDD                                      
064400           MOVE NEJ TO NYCKLAR-SW                                         
064500        END-IF                                                            
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900 BC-CHCK-IDTRP SECTION.                                                   
065000                                                                          
065100     IF MID-PFI-IDTRP = ALL '+'                                           
065200       MOVE MID-PF7-IDTRP TO WS-PFI-IDTRP                                 
065300     ELSE                                                                 
065400       MOVE MID-PFI-IDTRP TO WS-PFI-IDTRP                                 
065500       MOVE '7'             TO MFS-IDPFK                                  
065600       MOVE SPACE           TO MFS-KDTRTYP                                
065700     END-IF                                                               
065800                                                                          
065900     IF WS-PFI-IDTRP NOT NUMERIC                                          
066000        MOVE SPACE TO WS-PFI-IDTRP                                        
066100     END-IF                                                               
066200     .                                                                    
066300     EJECT                                                                
066400 BD-CHCK-TITRPAVT SECTION.                                                
066500                                                                          
066600     IF MID-PFI-TITRPAVT-AAMMDD = ALL '+'                                 
066700       INSPECT MID-PF7-TITRPAVT-AAMMDD REPLACING                          
066800                                        LEADING SPACE BY ZERO             
066900       MOVE MID-PF7-TITRPAVT-AAMMDD TO WS-PFI-TITRPAVT-AAMMDD             
067000     ELSE                                                                 
067100       INSPECT MID-PFI-TITRPAVT-AAMMDD REPLACING                          
067200                                        LEADING SPACE BY ZERO             
067300       IF MID-PFI-TITRPAVT-AAMMDD NOT NUMERIC                             
067400           MOVE 0 TO WS-PFI-TITRPAVT-AAMMDD                               
067500       ELSE                                                               
067600          MOVE MID-PFI-TITRPAVT-AAMMDD TO WS-PFI-TITRPAVT-AAMMDD          
067700       END-IF                                                             
067800       MOVE '7'             TO MFS-IDPFK                                  
067900       MOVE SPACE           TO MFS-KDTRTYP                                
068000     END-IF                                                               
068100                                                                          
068200     IF MID-PFI-TITRPAVT-HHMM = ALL '+'                                   
068300       INSPECT MID-PF7-TITRPAVT-HHMM REPLACING                            
068400                             LEADING SPACE BY ZERO                        
068500       MOVE MID-PF7-TITRPAVT-HHMM  TO WS-PFI-TITRPAVT-HHMM                
068600     ELSE                                                                 
068700       INSPECT MID-PFI-TITRPAVT-HHMM REPLACING                            
068800                             LEADING SPACE BY ZERO                        
068900       IF MID-PFI-TITRPAVT-HHMM NOT NUMERIC                               
069000           MOVE 0 TO WS-PFI-TITRPAVT-HHMM                                 
069100       ELSE                                                               
069200           MOVE MID-PFI-TITRPAVT-HHMM TO WS-PFI-TITRPAVT-HHMM             
069300       END-IF                                                             
069400       MOVE '7'             TO MFS-IDPFK                                  
069500       MOVE SPACE           TO MFS-KDTRTYP                                
069600     END-IF                                                               
069700                                                                          
069800     IF WS-PFI-TITRPAVT-AAMMDD > 0 AND                                    
069900        WS-PFI-TITRPAVT-AAMMDD NUMERIC                                    
070000        MOVE WS-PFI-TITRPAVT-AAMMDD   TO TMP1-YYMMDD                      
070100        MOVE WS-LOCAL-DATE            TO TMP2-YYMMDD                      
070200        PERFORM WY2000P1                                                  
070300        IF TMP1-YYMMDD < TMP2-YYMMDD                                      
070400           MOVE NEJ TO NYCKLAR-SW                                         
070500        END-IF                                                            
070600     END-IF                                                               
070700     .                                                                    
070800     EJECT                                                                
070900 BE-CHCK-COMBIN SECTION.                                                  
071000                                                                          
071100     IF (WS-PFI-IDTRP NOT =  SPACES)                                      
071200        IF (WS-PFI-TIRFS-AAMMDD > 0)                                      
071300           MOVE NEJ TO NYCKLAR-SW                                         
071400        END-IF                                                            
071500     ELSE                                                                 
071600        IF (WS-PFI-TIRFS-AAMMDD = 0)                                      
071700           MOVE WS-LOCAL-DATE TO WS-PFI-TIRFS-AAMMDD                      
071800           MOVE 2359 TO WS-PFI-TIRFS-HHMM                                 
071900        END-IF                                                            
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 BF-READ-PF8-PFE-DATA SECTION.                                            
072400                                                                          
072500     INSPECT MID-PF8-IDORDER REPLACING LEADING SPACE BY ZERO              
072600     IF MID-PF8-IDORDER NOT NUMERIC                                       
072700       MOVE ZERO TO MID-PF8-IDORDER                                       
072800     END-IF                                                               
072900     MOVE MID-PF8-IDORDER TO WS-PF8-IDORDER                               
073000                                                                          
073100     INSPECT MID-PF8-IDPRODNR REPLACING LEADING SPACE BY ZERO             
073200     IF MID-PF8-IDPRODNR NOT NUMERIC                                      
073300       MOVE ZERO TO MID-PF8-IDPRODNR                                      
073400     END-IF                                                               
073500     MOVE MID-PF8-IDPRODNR TO WS-PF8-IDPRODNR                             
073600                                                                          
073700     INSPECT MID-PF8-IDPLKLST REPLACING LEADING SPACE BY ZERO             
073800     IF MID-PF8-IDPLKLST NOT NUMERIC                                      
073900       MOVE ZERO TO MID-PF8-IDPLKLST                                      
074000     END-IF                                                               
074100     MOVE MID-PF8-IDPLKLST TO WS-PF8-IDPLKLST                             
074200                                                                          
074300     INSPECT MID-PF8-TIRFS    REPLACING LEADING SPACE BY ZERO             
074400     IF MID-PF8-TIRFS NOT NUMERIC                                         
074500       MOVE ZERO TO MID-PF8-TIRFS                                         
074600     END-IF                                                               
074700     MOVE MID-PF8-TIRFS    TO WS-PF8-TIRFS                                
074800                                                                          
074900     INSPECT MID-PF8-TILST    REPLACING LEADING SPACE BY ZERO             
075000     IF MID-PF8-TILST NOT NUMERIC                                         
075100       MOVE ZERO TO  MID-PF8-TILST                                        
075200     END-IF                                                               
075300     MOVE MID-PF8-TILST    TO WS-PF8-TILST                                
075400                                                                          
075500     MOVE MID-PF8-IDPRCVAR TO WS-PF8-IDPRCVAR                             
075600                                                                          
075700     INSPECT MID-PFE-IDORDER REPLACING LEADING SPACE BY ZERO              
075800     IF MID-PFE-IDORDER NOT NUMERIC                                       
075900       MOVE ZERO TO MID-PFE-IDORDER                                       
076000     END-IF                                                               
076100     MOVE MID-PFE-IDORDER TO WS-PFE-IDORDER                               
076200                                                                          
076300     INSPECT MID-PFE-IDPRODNR REPLACING LEADING SPACE BY ZERO             
076400     IF MID-PFE-IDPRODNR NOT NUMERIC                                      
076500       MOVE ZERO TO MID-PFE-IDPRODNR                                      
076600     END-IF                                                               
076700     MOVE MID-PFE-IDPRODNR TO WS-PFE-IDPRODNR                             
076800                                                                          
076900     INSPECT MID-PFE-IDPLKLST REPLACING LEADING SPACE BY ZERO             
077000     IF MID-PFE-IDPLKLST NOT NUMERIC                                      
077100       MOVE ZERO TO MID-PFE-IDPLKLST                                      
077200     END-IF                                                               
077300     MOVE MID-PFE-IDPLKLST TO WS-PFE-IDPLKLST                             
077400                                                                          
077500     INSPECT MID-PFE-TIRFS    REPLACING LEADING SPACE BY ZERO             
077600     IF MID-PFE-TIRFS NOT NUMERIC                                         
077700       MOVE ZERO TO  MID-PFE-TIRFS                                        
077800     END-IF                                                               
077900     MOVE MID-PFE-TIRFS    TO WS-PFE-TIRFS                                
078000                                                                          
078100     INSPECT MID-PFE-TILST    REPLACING LEADING SPACE BY ZERO             
078200     IF MID-PFE-TILST NOT NUMERIC                                         
078300       MOVE ZERO TO MID-PFE-TILST                                         
078400     END-IF                                                               
078500     MOVE MID-PFE-TILST    TO WS-PFE-TILST                                
078600                                                                          
078700     MOVE MID-PFE-IDPRCVAR TO WS-PFE-IDPRCVAR                             
078800     MOVE MID-PFX-IDPRC     TO WS-PFX-IDPRC                               
078900     .                                                                    
079000     EJECT                                                                
079100 BG-CHCK-DATA SECTION.                                                    
079200                                                                          
079300     IF NYCKLAR-OK                                                        
079400        MOVE WS-PFI-IDPRC TO W-4448-IDPRC                                 
079500        MOVE WS-IDDC  TO W-4447-IDDC                                      
079600        IF WS-PFI-IDPRCVAR NOT = SPACE                                    
079700           MOVE NEJ TO NYCKLAR-SW                                         
079800           PERFORM IMS-GU-XXKH-WDGX4448                                   
079900           IF SEGMENT-OK                                                  
080000              PERFORM S03-PRC-RULE                                        
080100              MOVE JA TO NYCKLAR-SW                                       
080200           END-IF                                                         
080300        END-IF                                                            
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 BH-RIGHT-KEY SECTION.                                                    
080800                                                                          
080900     MOVE WS-IDDC                  TO MOD-PF7-IDDC                        
081000                                                                          
081100     IF GODK-MID OR NYCKLAR-OK                                            
081200       MOVE WS-PFI-IDPRC           TO MOD-PF7-IDPRC                       
081300       MOVE WS-PFI-IDTRP           TO MOD-PF7-IDTRP                       
081400       MOVE WS-PFI-TIRFS-HHMM      TO MOD-PF7-TIRFS-HHMM                  
081500*      INSPECT MOD-PF7-TIRFS-HHMM                                         
081600*PGA Y2K INLEDANDA NOLLOR SKALL FINNAS. REPLACING LEADING ZERO BY         
081700       MOVE WS-PFI-TIRFS-AAMMDD  TO MOD-PF7-TIRFS-AAMMDD                  
081800*      INSPECT MOD-PF7-TIRFS-AAMMDD                                       
081900*PGA Y2K                    REPLACING LEADING ZERO BY SPACE               
082000       MOVE WS-PFI-TITRPAVT-HHMM   TO MOD-PF7-TITRPAVT-HHMM               
082100*      INSPECT MOD-PF7-TITRPAVT-HHMM                                      
082200*PGA Y2K                    REPLACING LEADING ZERO BY SPACE               
082300       MOVE WS-PFI-TITRPAVT-AAMMDD TO MOD-PF7-TITRPAVT-AAMMDD             
082400*      INSPECT MOD-PF7-TITRPAVT-AAMMDD                                    
082500*PGA Y2K                    REPLACING LEADING ZERO BY SPACE               
082600     ELSE                                                                 
082700       MOVE MFS-RENSA-FAELT        TO MOD-PF7-IDPRC                       
082800                                      MOD-PF7-IDTRP                       
082900                                      MOD-PF7-TIRFS-HHMM                  
083000                                      MOD-PF7-TIRFS-AAMMDD                
083100                                      MOD-PF7-TITRPAVT-HHMM               
083200                                      MOD-PF7-TITRPAVT-AAMMDD             
083300     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600 BI-WRONG-KEY SECTION.                                                    
083700     IF NYCKLAR-FEL                                                       
083800       MOVE '401' TO MED-IDMFSFEL                                         
083900       CALL WMEDKONV USING MED-WMEDAREA                                   
084000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
084100       PERFORM MFS-ERASE-FIELD-UT                                         
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500 C-FOERSTA-SIDA SECTION.                                                  
084600     MOVE '006' TO MED-IDMFSFEL                                           
084700     CALL WMEDKONV USING MED-WMEDAREA                                     
084800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
084900     PERFORM MFS-ERASE-FIELD-UT                                           
085000     PERFORM MFS-ERASE-MOD-PFI                                            
085100     MOVE JA TO ALLT-SW                                                   
085200     PERFORM S01-FIXPRT-DBKEY                                             
085300     MOVE LOW-VALUE TO W-MIQA-WDQ301 W-MIQB-WDQ301                        
085400     MOVE HIGH-VALUE TO W-MXQA-WDQ301 W-MXQB-WDQ301                       
085500     IF WS-PFI-IDPRCVAR = SPACE                                           
085600        MOVE LOW-VALUE  TO  W-MIQA-IDPRCVAR                               
085700                            W-MIQB-IDPRCVAR                               
085800                            W-IDPRCVAR-LOW                                
085900        MOVE HIGH-VALUE TO  W-MXQA-IDPRCVAR                               
086000                            W-MXQB-IDPRCVAR                               
086100                            W-IDPRCVAR-UP                                 
086200     ELSE                                                                 
086300        MOVE WS-PFI-IDPRCVAR  TO  W-MIQA-IDPRCVAR                         
086400                                  W-MIQB-IDPRCVAR                         
086500                                  W-MXQA-IDPRCVAR                         
086600                                  W-MXQB-IDPRCVAR                         
086700                                  W-IDPRCVAR-LOW                          
086800                                  W-IDPRCVAR-UP                           
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 D-NAESTA-SIDA SECTION.                                                   
087300                                                                          
087400     IF MID-PF8-TIRFS = 9999999999                                        
087500        MOVE '115'           TO MED-IDMFSFEL                              
087600        CALL WMEDKONV USING MED-WMEDAREA                                  
087700        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
087800        PERFORM S02-INIT-PFE-PF8-NYCKLAR                                  
087900        MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                              
088000        MOVE NEJ             TO ALLT-SW                                   
088100     ELSE                                                                 
088200        PERFORM MFS-KEEP-FIELD-UT                                         
088300        MOVE JA TO ALLT-SW                                                
088400        PERFORM S01-FIXPRT-DBKEY                                          
088500        MOVE WS-PF8-IDORDER     TO W-MIQA-IDORDER  W-MIQB-IDORDER         
088600        MOVE WS-PF8-IDPRODNR    TO W-MIQA-IDPRODNR W-MIQB-IDPRODNR        
088700        MOVE WS-PF8-IDPLKLST    TO W-MIQA-IDPLKLST W-MIQB-IDPLKLST        
088800        IF WS-PFI-IDPRCVAR = SPACE                                        
088900           MOVE WS-PF8-IDPRCVAR  TO  W-MIQA-IDPRCVAR                      
089000                                     W-MIQB-IDPRCVAR                      
089100                                     W-IDPRCVAR-LOW                       
089200           MOVE HIGH-VALUE TO  W-MXQA-IDPRCVAR                            
089300                               W-MXQB-IDPRCVAR                            
089400                               W-IDPRCVAR-UP                              
089500        ELSE                                                              
089600           MOVE WS-PFI-IDPRCVAR  TO  W-MIQA-IDPRCVAR                      
089700                                     W-MIQB-IDPRCVAR                      
089800                                     W-MXQA-IDPRCVAR                      
089900                                     W-MXQB-IDPRCVAR                      
090000                                     W-IDPRCVAR-LOW                       
090100                                     W-IDPRCVAR-UP                        
090200        END-IF                                                            
090300        MOVE WS-PF8-TIRFS TO W-MIQA-DARFS                                 
090400                             W-MIQB-DARFS                                 
090500        IF WS-PF8-TIRFS NOT = ZERO                                        
090600          IF WS-PF8-TIRFS < 5000000000                                    
090700            MOVE 20       TO W-MIQA-DARFS (1:2)                           
090800                             W-MIQB-DARFS (1:2)                           
090900          ELSE                                                            
091000            IF WS-PF8-TIRFS < 9999999999                                  
091100              MOVE 19     TO W-MIQA-DARFS (1:2)                           
091200                             W-MIQB-DARFS (1:2)                           
091300            ELSE                                                          
091400              MOVE 999999999999 TO W-MIQA-DARFS                           
091500                                   W-MIQB-DARFS                           
091600            END-IF                                                        
091700          END-IF                                                          
091800        END-IF                                                            
091900        MOVE WS-PF8-TILST TO W-MIQA-TILST-O                               
092000                             W-MIQB-TILST-O                               
092100        IF WS-PF8-TILST NOT = ZERO                                        
092200          IF WS-PF8-TILST < 5000000000                                    
092300            MOVE 20       TO W-MIQA-TILST-O (1:2)                         
092400                             W-MIQB-TILST-O (1:2)                         
092500          ELSE                                                            
092600            IF WS-PF8-TILST < 9999999999                                  
092700              MOVE 19     TO W-MIQA-TILST-O (1:2)                         
092800                             W-MIQB-TILST-O (1:2)                         
092900            ELSE                                                          
093000              MOVE 999999999999 TO W-MIQA-TILST-O                         
093100                                   W-MIQB-TILST-O                         
093200            END-IF                                                        
093300          END-IF                                                          
093400        END-IF                                                            
093500     END-IF                                                               
093600     .                                                                    
093700     EJECT                                                                
093800 E-SAMMA-SIDA SECTION.                                                    
093900     IF MID-PFI-IDPRC = ALL '+'                                           
094000        PERFORM MFS-KEEP-FIELD-UT                                         
094100        PERFORM S01-FIXPRT-DBKEY                                          
094200        MOVE WS-PFE-IDORDER     TO W-MIQA-IDORDER                         
094300                                   W-MIQB-IDORDER                         
094400        MOVE WS-PFE-IDPRODNR    TO W-MIQA-IDPRODNR                        
094500                                   W-MIQB-IDPRODNR                        
094600        MOVE WS-PFE-IDPLKLST    TO W-MIQA-IDPLKLST                        
094700                                   W-MIQB-IDPLKLST                        
094800        IF WS-PFI-IDPRCVAR = SPACE                                        
094900           MOVE WS-PFE-IDPRCVAR  TO  W-MIQA-IDPRCVAR                      
095000                                     W-MIQB-IDPRCVAR                      
095100                                     W-IDPRCVAR-LOW                       
095200           MOVE HIGH-VALUE TO  W-MXQA-IDPRCVAR                            
095300                               W-MXQB-IDPRCVAR                            
095400                               W-IDPRCVAR-UP                              
095500        ELSE                                                              
095600           MOVE WS-PFI-IDPRCVAR  TO  W-MIQA-IDPRCVAR                      
095700                                     W-MIQB-IDPRCVAR                      
095800                                     W-MXQA-IDPRCVAR                      
095900                                     W-MXQB-IDPRCVAR                      
096000                                     W-IDPRCVAR-LOW                       
096100                                     W-IDPRCVAR-UP                        
096200        END-IF                                                            
096300        MOVE WS-PFE-TIRFS TO W-MIQA-DARFS                                 
096400                             W-MIQB-DARFS                                 
096500        IF WS-PFE-TIRFS NOT = ZERO                                        
096600          IF WS-PFE-TIRFS < 5000000000                                    
096700            MOVE 20       TO W-MIQA-DARFS (1:2)                           
096800                             W-MIQB-DARFS (1:2)                           
096900          ELSE                                                            
097000            IF WS-PFE-TIRFS < 9999999999                                  
097100              MOVE 19     TO W-MIQA-DARFS (1:2)                           
097200                             W-MIQB-DARFS (1:2)                           
097300            ELSE                                                          
097400              MOVE 999999999999 TO W-MIQA-DARFS                           
097500                                   W-MIQB-DARFS                           
097600            END-IF                                                        
097700          END-IF                                                          
097800        END-IF                                                            
097900                                                                          
098000        MOVE WS-PFE-TILST TO W-MIQA-TILST-O                               
098100                             W-MIQB-TILST-O                               
098200        IF WS-PFE-TILST NOT = ZERO                                        
098300          IF WS-PFE-TILST < 5000000000                                    
098400            MOVE 20       TO W-MIQA-TILST-O (1:2)                         
098500                             W-MIQB-TILST-O (1:2)                         
098600          ELSE                                                            
098700            IF WS-PFE-TILST < 9999999999                                  
098800              MOVE 19     TO W-MIQA-TILST-O (1:2)                         
098900                             W-MIQB-TILST-O (1:2)                         
099000            ELSE                                                          
099100              MOVE 999999999999 TO W-MIQA-TILST-O                         
099200                                   W-MIQB-TILST-O                         
099300            END-IF                                                        
099400          END-IF                                                          
099500        END-IF                                                            
099600     ELSE                                                                 
099700        MOVE NEJ TO ALLT-SW                                               
099800        MOVE '003' TO MED-IDMFSFEL                                        
099900        CALL WMEDKONV USING MED-WMEDAREA                                  
100000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
100100        PERFORM MFS-KEEP-SCREEN                                           
100200     END-IF                                                               
100300     .                                                                    
100400     EJECT                                                                
100500 F-LAES-VISA-INFO SECTION.                                                
100600                                                                          
100700     PERFORM S02-INIT-PFE-PF8-NYCKLAR                                     
100800     PERFORM FD-INIT-WSDATA                                               
100900                                                                          
101000     MOVE ' ' TO ORQA-ODEL-KDODELSTA ORQA-ODEL-KDPRODKL STATUS-WS         
101100     PERFORM UNTIL                                                        
101200     ((ORQA-ODEL-KDODELSTA = 'R') AND (ORQA-ODEL-KDPRODKL = 'D'))         
101300     OR (NOT SEGMENT-OK)                                                  
101400                                                                          
101500        PERFORM F03-GET-SEC-INDX                                          
101600        IF SEGMENT-OK                                                     
101700                                                                          
101800           PERFORM IMS-GU-ORQA-WDQ301                                     
101900           IF WS-PFI-IDPRCVAR = SPACE                                     
102000              PERFORM FF-LAES-PRC                                         
102100           END-IF                                                         
102200        END-IF                                                            
102300     END-PERFORM                                                          
102400                                                                          
102500     IF SEGMENT-OK                                                        
102600       MOVE +1 TO  PU-INDX ARB-INDX                                       
102700       PERFORM F00-NEXT-REFPU                                             
102800       PERFORM FI-STORE-PFE                                               
102900       PERFORM UNTIL NOT SEGMENT-OK                                       
103000                                                                          
103100          PERFORM IMS-GU-ORQA-WDQ301                                      
103200          IF SEGMENT-OK                                                   
103300                                                                          
103400             IF (ORQA-ODEL-KDODELSTA = 'R') AND                           
103500                         (ORQA-ODEL-KDPRODKL = 'D')                       
103600                PERFORM FC-CNT-MANPOWER                                   
103700                PERFORM FG-CNTRL-BREAK-PU                                 
103800                PERFORM FV-CNT-PU-LMT                                     
103900                PERFORM FX-MZ-KVRADER                                     
104000                ADD WS-MZ-KVRADER TO W1-KVRADER-MEZ-PU                    
104100             END-IF                                                       
104200          END-IF                                                          
104300          PERFORM F03-GET-SEC-INDX                                        
104400       END-PERFORM                                                        
104500                                                                          
104600       IF W1-KVRADER-PU NOT = +0                                          
104700           ADD +1 TO WS-PU                                                
104800       END-IF                                                             
104900                                                                          
105000       IF PU-INDX NOT > MAX-PU-INDX                                       
105100           PERFORM F02-ADD-KVVTID-TO-MPU                                  
105200           PERFORM F01-PU-LINE                                            
105300       END-IF                                                             
105400       PERFORM FZ-DSPL-4383                                               
105500     ELSE                                                                 
105600        MOVE '413' TO MED-IDMFSFEL                                        
105700        CALL WMEDKONV USING MED-WMEDAREA                                  
105800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
105900        MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                              
106000        PERFORM MFS-ERASE-FIELD-UT                                        
106100     END-IF                                                               
106200     .                                                                    
106300     EJECT                                                                
106400 FC-CNT-MANPOWER SECTION.                                                 
106500                                                                          
106600     MOVE ORQA-ODEL-DARFS  TO  WS-ODEL-DARFS                              
106700     MOVE WS-ODEL-DARFS-HHMM TO WS-WRKTIMES                               
106800     DIVIDE 100 INTO WS-WRKTIMES GIVING WS-RFSTID                         
106900     MOVE +0 TO WS-WRKDAYS                                                
107000                                                                          
107100     IF WS-ODEL-DARFS-AAMMDD > 0 AND                                      
107200        WS-ODEL-DARFS-AAMMDD NUMERIC                                      
107300       MOVE WS-ODEL-DARFS-AAMMDD TO TMP1-YYMMDD                           
107400       MOVE WS-LOCAL-DATE        TO TMP2-YYMMDD                           
107500       PERFORM WY2000P1                                                   
107600       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
107700          MOVE WS-ODEL-DARFS-AAMMDD TO WORK-TIAAMMDD-TOM                  
107800          PERFORM S00-WORKDAY                                             
107900       END-IF                                                             
108000     END-IF                                                               
108100                                                                          
108200     MULTIPLY +24 BY WS-WRKDAYS GIVING WS-WRKTIMES                        
108300     ADD WS-WRKTIMES TO WS-RFSTID                                         
108400                                                                          
108500     MOVE +1 TO ARB-INDX                                                  
108600     PERFORM UNTIL   (ARB-INDX = MAX-ARB-INDX)    OR                      
108700     ((WS-TIACTMIN-PAC (ARB-INDX) < WS-RFSTID) AND                        
108800             (WS-RFSTID NOT > WS-TISTOMIN-PAC (ARB-INDX)))                
108900        ADD +1 TO ARB-INDX                                                
109000     END-PERFORM                                                          
109100                                                                          
109200     IF WS-TIACTMIN-PAC (ARB-INDX) < WS-RFSTID                            
109300         PERFORM FCA-ADDUP-TISTA                                          
109400     END-IF                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 FCA-ADDUP-TISTA SECTION.                                                 
109800                                                                          
109900     MOVE WS-TIACTMIN-PAC (ARB-INDX) TO WS-TIHHMM                         
110000     MOVE WS-TIHH  TO  WS-WRK-TIHH                                        
110100     MOVE WS-TIMM  TO  WS-WRK-TIMM                                        
110200     MOVE ORQA-ODEL-SUPTID TO WS-TIHHMM                                   
110300     ADD  WS-TIHH TO WS-WRK-TIHH                                          
110400     ADD  WS-TIMM TO WS-WRK-TIMM                                          
110500                                                                          
110600     DIVIDE WS-60 INTO WS-WRK-TIMM GIVING WS-QOT-TIHH                     
110700                                      REMAINDER WS-RST-TIMM               
110800     ADD  WS-QOT-TIHH TO WS-WRK-TIHH                                      
110900     MOVE WS-WRK-TIHH TO WS-TIHH                                          
111000     MOVE WS-RST-TIMM TO WS-TIMM                                          
111100     MOVE WS-TIHHMM   TO WS-TIACTMIN-PAC (ARB-INDX)                       
111200     .                                                                    
111300     EJECT                                                                
111400 FD-INIT-WSDATA SECTION.                                                  
111500                                                                          
111600     MOVE WS-PU-CUMU-00 TO WS-PU-CUMULATOR                                
111700     MOVE +0 TO  WS-PU                                                    
111800     MOVE +1 TO PU-INDX                                                   
111900     PERFORM UNTIL PU-INDX > 14                                           
112000         MOVE +0   TO WS-KVRADER-PU (PU-INDX)                             
112100                      WS-KVORDER-PU (PU-INDX)                             
112200                      WS-KVRADER-MEZ-PU (PU-INDX)                         
112300                      WS-TIRFS-PU (PU-INDX)                               
112400                      WS-TILST-OD-PU (PU-INDX)                            
112500                      WS-SUPTID-PU (PU-INDX)                              
112600                      WS-VLORDNTO-PU (PU-INDX)                            
112700                      WS-VKORDNTO-PU (PU-INDX)                            
112800                      WS-IDTRP-PU (PU-INDX)                               
112900         ADD +1 TO PU-INDX                                                
113000     END-PERFORM                                                          
113100     MOVE +0 TO PU-INDX                                                   
113200     .                                                                    
113300     EJECT                                                                
113400 FF-LAES-PRC SECTION.                                                     
113500                                                                          
113600       IF MFS-FIRST                                                       
113700       OR (NDC-US AND MFS-ENTER)                                          
113800          MOVE ORQA-ODEL-IDPRC TO MOD-PFX-IDPRC                           
113900                                  W-4448-IDPRC                            
114000       ELSE                                                               
114100          MOVE WS-PFX-IDPRC    TO MOD-PFX-IDPRC                           
114200                                  W-4448-IDPRC                            
114300       END-IF                                                             
115400                                                                          
115500       PERFORM IMS-GU-XXKH-WDGX4448                                       
115600       IF SEGMENT-OK                                                      
115700          PERFORM S03-PRC-RULE                                            
115800       END-IF                                                             
115900     .                                                                    
116000     EJECT                                                                
116100 FG-CNTRL-BREAK-PU SECTION.                                               
116200                                                                          
116300     IF (W1-KVORDER-PU NOT < WS-KVORDER-PRC) OR                           
116400          (W1-VKORDNTO-PU  NOT < WS-VKORDNTO-PRC) OR                      
116500            (W1-VLORDNTO-PU  NOT < WS-VLORDNTO-PRC) OR                    
116600               (W1-KVRADER-PU NOT < WS-KVRADER-PRC)                       
116700         ADD +1 TO WS-PU                                                  
116800         PERFORM F02-ADD-KVVTID-TO-MPU                                    
116900                                                                          
117000         IF PU-INDX NOT > MAX-PU-INDX                                     
117100            PERFORM F01-PU-LINE                                           
117200         END-IF                                                           
117300                                                                          
117400         ADD +1 TO PU-INDX                                                
117500         IF PU-INDX NOT > MAX-PU-INDX                                     
117600            PERFORM F00-NEXT-REFPU                                        
117700         END-IF                                                           
117800                                                                          
117900         IF PU-INDX = (MAX-PU-INDX-PF8)                                   
118000            PERFORM FH-STORE-PF8                                          
118100         END-IF                                                           
118200         MOVE WS-PU-CUMU-00 TO WS-PU-CUMULATOR                            
118300     END-IF                                                               
118400     .                                                                    
118500     EJECT                                                                
118600 FH-STORE-PF8 SECTION.                                                    
118700                                                                          
118800     IF SEGMENT-OK                                                        
118900        MOVE ORQA-ODEL-IDORDER      TO MOD-PF8-IDORDER                    
119000        MOVE ORQA-ODEL-IDPRODNR     TO MOD-PF8-IDPRODNR                   
119100        MOVE ORQA-ODEL-IDPLKLST     TO MOD-PF8-IDPLKLST                   
119200        MOVE ORQA-ODEL-DARFS (3:9) TO MOD-PF8-TIRFS                       
119300        MOVE ORQA-ODEL-TILST-OD     TO WS-ORQA-ODEL-TILST-OD              
119400        MOVE WS-ORQA-ODEL-TILST-OD (3:9) TO MOD-PF8-TILST                 
119500        MOVE ORQA-ODEL-IDPRCVAR     TO MOD-PF8-IDPRCVAR                   
119600        MOVE '105' TO MED-IDMFSINF                                        
119700        CALL WMEDKONV USING MED-WMEDAREA                                  
119800        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
119900     END-IF                                                               
120000     .                                                                    
120100*                                                                         
120200 FI-STORE-PFE SECTION.                                                    
120300                                                                          
120400     MOVE ORQA-ODEL-IDORDER      TO MOD-PFE-IDORDER                       
120500     MOVE ORQA-ODEL-IDPRODNR     TO MOD-PFE-IDPRODNR                      
120600     MOVE ORQA-ODEL-IDPLKLST     TO MOD-PFE-IDPLKLST                      
120700     MOVE ORQA-ODEL-DARFS (3:9)  TO MOD-PFE-TIRFS                         
120800     MOVE ORQA-ODEL-TILST-OD     TO WS-ORQA-ODEL-TILST-OD                 
120900     MOVE WS-ORQA-ODEL-TILST-OD (3:9)  TO MOD-PFE-TILST                   
121000     MOVE ORQA-ODEL-IDPRCVAR     TO MOD-PFE-IDPRCVAR                      
121100     .                                                                    
121200     EJECT                                                                
121300 FV-CNT-PU-LMT SECTION.                                                   
121400                                                                          
121500     ADD  ORQA-ODEL-KVRADER    TO W1-KVRADER-PU                           
121600     ADD  ORQA-ODEL-VKORDNTO   TO W1-VKORDNTO-PU                          
121700     ADD  ORQA-ODEL-VLORDNTO   TO W1-VLORDNTO-PU                          
121800     MOVE W1-SUPTID-PU         TO WS-TIHHMM                               
121900     MOVE WS-TIHH              TO WS-DB-TIHH                              
122000     MOVE WS-TIMM              TO WS-DB-TIMM                              
122100     MOVE ORQA-ODEL-SUPTID     TO WS-TIHHMM                               
122200     ADD  WS-TIHH              TO WS-DB-TIHH                              
122300     ADD  WS-TIMM              TO WS-DB-TIMM                              
122400                                                                          
122500     IF WS-DB-TIMM NOT < WS-60                                            
122600        ADD  +1 TO WS-DB-TIHH                                             
122700        SUBTRACT WS-60 FROM WS-DB-TIMM                                    
122800     END-IF                                                               
122900                                                                          
123000     MOVE WS-DB-TIMM        TO WS-TIMM                                    
123100     MOVE WS-DB-TIHH        TO WS-TIHH                                    
123200     MOVE WS-TIHHMM         TO W1-SUPTID-PU                               
123300     .                                                                    
123400     EJECT                                                                
123500 FX-MZ-KVRADER SECTION.                                                   
123600                                                                          
123700     MOVE +0 TO WS-MZ-KVRADER                                             
123800     IF ORQA-ODEL-IDORDER NOT = W-IDORDER                                 
123900        ADD +1 TO W1-KVORDER-PU                                           
124000        MOVE ORQA-ODEL-IDORDER TO W-IDORDER                               
124100        PERFORM IMS-GU-WDQ221                                             
124200                                                                          
124300        IF SEGMENT-OK                                                     
124400            MOVE LOR-KVRADER   TO WS-MZ-KVRADER                           
124500        END-IF                                                            
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900 FZ-DSPL-4383 SECTION.                                                    
125000                                                                          
125100     MOVE +1 TO ARB-INDX                                                  
125200     PERFORM UNTIL                                                        
125300      (ARB-INDX > MAX-ARB-INDX) OR                                        
125400      (WS-TIACTMIN-PAC (ARB-INDX) = WS-TISTAMIN-PAC (ARB-INDX))           
125500        ADD +1 TO ARB-INDX                                                
125600     END-PERFORM                                                          
125700                                                                          
125800     SUBTRACT 1 FROM ARB-INDX                                             
125900     IF MFS-FIRST                                                         
126000     OR (NDC-US AND MFS-ENTER)                                            
126100         MOVE ARB-INDX    TO  MOD-KVBEMAN-DORD                            
126200         MOVE WS-PU       TO  MOD-KVPU-DORD                               
126300     END-IF                                                               
126400                                                                          
126500     MOVE +1 TO PU-INDX                                                   
126600     PERFORM UNTIL PU-INDX > MAX-PU-INDX                                  
126700     IF WS-TIRFS-PU (PU-INDX) > +0                                        
126800       MOVE WS-TILST-OD-PU (PU-INDX) TO                                   
126900                                      MOD-TILST-OD-RAD (PU-INDX)          
127000*                                                                         
127100       MOVE WS-SUPTID-PU (PU-INDX)   TO MOD-SUPTID-RAD (PU-INDX)          
127200*                                                                         
127300       MOVE WS-TIRFS-PU (PU-INDX)    TO MOD-TIRFS-RAD (PU-INDX)           
127400*                                                                         
127500       MOVE WS-KVORDER-PU (PU-INDX)  TO MOD-KVORDER-RAD (PU-INDX)         
127600       MOVE WS-KVRADER-PU (PU-INDX)  TO MOD-KVRADER-RAD (PU-INDX)         
127700       MOVE WS-VKORDNTO-PU (PU-INDX) TO MOD-VKORDNTO-RAD (PU-INDX)        
127800       MOVE WS-VLORDNTO-PU (PU-INDX) TO MOD-VLORDNTO-RAD (PU-INDX)        
127900       MOVE WS-IDTRP-PU (PU-INDX)    TO MOD-IDTRP-RAD (PU-INDX)           
128000       MOVE WS-KVRADER-MEZ-PU (PU-INDX)  TO                               
128100                                   MOD-KVRADER-MEZ-RAD (PU-INDX)          
128200     ELSE                                                                 
128300       PERFORM MFS-ERASE-MOD-LINE                                         
128400     END-IF                                                               
128500       ADD  +1 TO PU-INDX                                                 
128600     END-PERFORM                                                          
128700                                                                          
128800     IF MFS-FIRST                                                         
128900     OR (NDC-US AND MFS-ENTER)                                            
129000        IF MOD-PF8-IDORDER = 9999999                                      
129400           MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                           
129600        END-IF                                                            
129700     END-IF                                                               
129800     .                                                                    
129900     EJECT                                                                
130000 F00-NEXT-REFPU SECTION.                                                  
130100                                                                          
130200     MOVE ORQA-ODEL-DARFS (3:10) TO WS-TIRFS-PU (PU-INDX)                 
130300     MOVE ORQA-ODEL-IDTRP    TO WS-IDTRP-PU (PU-INDX)                     
130400     MOVE ORQA-ODEL-TILST-OD TO WS-ORQA-ODEL-TILST-OD                     
130500     MOVE WS-ORQA-ODEL-TILST-OD (3:9) TO WS-TILST-OD-PU (PU-INDX)         
130600     .                                                                    
130700                                                                          
130800 F01-PU-LINE SECTION.                                                     
130900     MOVE W1-KVORDER-PU      TO WS-KVORDER-PU (PU-INDX)                   
131000     MOVE W1-KVRADER-PU      TO WS-KVRADER-PU (PU-INDX)                   
131100     MOVE W1-KVRADER-MEZ-PU  TO WS-KVRADER-MEZ-PU (PU-INDX)               
131200     MOVE W1-VKORDNTO-PU     TO WS-VKORDNTO-PU (PU-INDX)                  
131300     MOVE W1-VLORDNTO-PU     TO WS-VLORDNTO-PU (PU-INDX)                  
131400     MOVE W1-SUPTID-PU       TO WS-SUPTID-PU (PU-INDX)                    
131500     .                                                                    
131600     EJECT                                                                
131700 F02-ADD-KVVTID-TO-MPU SECTION.                                           
131800                                                                          
131900     MOVE WS-TIACTMIN-PAC (ARB-INDX) TO WS-TIHHMM                         
132000     MOVE WS-TIHH TO WS-WRK-TIHH                                          
132100     MOVE WS-TIMM TO WS-WRK-TIMM                                          
132200     MOVE WS-KVVTID-PRC TO WS-TIHHMM                                      
132300     ADD  WS-TIHH  TO WS-WRK-TIHH                                         
132400     ADD  WS-TIMM  TO WS-WRK-TIMM                                         
132500     ADD  WS-TIHH  TO WS-DB-TIHH                                          
132600     ADD  WS-TIMM  TO WS-DB-TIMM                                          
132700                                                                          
132800     IF WS-DB-TIMM NOT < WS-60                                            
132900            ADD  +1 TO WS-DB-TIHH                                         
133000            SUBTRACT WS-60 FROM WS-DB-TIMM                                
133100     END-IF                                                               
133200                                                                          
133300     IF WS-WRK-TIMM NOT < WS-60                                           
133400            ADD  +1 TO WS-WRK-TIHH                                        
133500            SUBTRACT WS-60 FROM WS-WRK-TIMM                               
133600     END-IF                                                               
133700                                                                          
133800     MOVE WS-WRK-TIMM       TO WS-TIMM                                    
133900     MOVE WS-WRK-TIHH       TO WS-TIHH                                    
134000     MOVE WS-TIHHMM         TO WS-TIACTMIN-PAC (ARB-INDX)                 
134100     MOVE WS-DB-TIMM        TO WS-TIMM                                    
134200     MOVE WS-DB-TIHH        TO WS-TIHH                                    
134300     MOVE WS-TIHHMM         TO W1-SUPTID-PU                               
134400     .                                                                    
134500     EJECT                                                                
134600 F03-GET-SEC-INDX SECTION.                                                
134700                                                                          
134800     IF WS-PFI-IDTRP NOT = SPACES                                         
134900        PERFORM IMS-GN-NEXT-WDQ3A1                                        
135000        IF SEGMENT-OK                                                     
135100           MOVE ORQB-SEQA-IDDC     TO W-ORQA-IDDC                         
135200           MOVE ORQB-SEQA-IDORDER  TO W-ORQA-IDORDER                      
135300           MOVE ORQB-SEQA-IDPRODNR TO W-ORQA-IDPRODNR                     
135400           MOVE ORQB-SEQA-IDPLKLST TO W-ORQA-IDPLKLST                     
135500        END-IF                                                            
135600     ELSE                                                                 
135700        PERFORM IMS-GN-NEXT-WDQ3B1                                        
135800        IF SEGMENT-OK                                                     
135900           MOVE ORQC-SEQB-IDDC     TO W-ORQA-IDDC                         
136000           MOVE ORQC-SEQB-IDORDER  TO W-ORQA-IDORDER                      
136100           MOVE ORQC-SEQB-IDPRODNR TO W-ORQA-IDPRODNR                     
136200           MOVE ORQC-SEQB-IDPLKLST TO W-ORQA-IDPLKLST                     
136300        END-IF                                                            
136400     END-IF                                                               
136500     .                                                                    
136600                                                                          
136700     EJECT                                                                
136800 S00-WORKDAY  SECTION.                                                    
136900                                                                          
137000     MOVE 001           TO WORK-KDCALL                                    
137100     MOVE WS-IDDC       TO WORK-IDDC                                      
137200     MOVE WS-LOCAL-DATE TO WORK-TIAAMMDD-FOM                              
137300     CALL WORKDAY  USING WORK-KDCALL                                      
137400                         WORK-DATE-AREA                                   
137500                         WORK-KDSVAR                                      
137600     IF (WORK-KDSVAR-FEL)                                                 
137700        MOVE 1 TO WORK-KVWORKD                                            
137800     END-IF                                                               
137900                                                                          
138000     MOVE WORK-KVWORKD  TO WS-WRKDAYS                                     
138100     SUBTRACT 1 FROM WS-WRKDAYS                                           
138200     IF WS-WRKDAYS < +0                                                   
138300        MOVE +0 TO WS-WRKDAYS                                             
138400     END-IF                                                               
138500     .                                                                    
138600     EJECT                                                                
138700 S01-FIXPRT-DBKEY SECTION.                                                
138800                                                                          
138900     MOVE WS-PFI-IDTRP           TO  W-MIQA-IDTRP                         
139000                                     W-MXQA-IDTRP                         
139100     MOVE WS-PFI-TITRPAVT-AAMMDD TO  W-MIQA-DATRPAVD                      
139200                                     W-MXQA-DATRPAVD                      
139300     IF WS-PFI-TITRPAVT-AAMMDD NUMERIC                                    
139400       IF WS-PFI-TITRPAVT-AAMMDD NOT = ZERO                               
139500         IF WS-PFI-TITRPAVT-AAMMDD < 500000                               
139600           MOVE 20               TO  W-MIQA-DATRPAVD (1:2)                
139700                                     W-MXQA-DATRPAVD (1:2)                
139800         ELSE                                                             
139900           IF WS-PFI-TITRPAVT-AAMMDD < 999999                             
140000             MOVE 19             TO  W-MIQA-DATRPAVD (1:2)                
140100                                     W-MXQA-DATRPAVD (1:2)                
140200           ELSE                                                           
140300             MOVE 99999999       TO  W-MIQA-DATRPAVD                      
140400                                     W-MXQA-DATRPAVD                      
140500           END-IF                                                         
140600         END-IF                                                           
140700       END-IF                                                             
140800     ELSE                                                                 
140900       MOVE 99999999             TO  W-MIQA-DATRPAVD                      
141000                                     W-MXQA-DATRPAVD                      
141100     END-IF                                                               
141200     IF WS-PFI-TITRPAVT-HHMM NUMERIC                                      
141300       MOVE WS-PFI-TITRPAVT-HHMM TO  W-MIQA-DATRPAVT-HHMM                 
141400                                     W-MXQA-DATRPAVT-HHMM                 
141500     ELSE                                                                 
141600       MOVE 0                    TO  W-MIQA-DATRPAVT-HHMM                 
141700       MOVE 0                    TO  W-MXQA-DATRPAVT-HHMM                 
141800     END-IF                                                               
141900     IF WS-PFI-TIRFS NUMERIC                                              
142000       MOVE WS-PFI-TIRFS         TO  W-MXQB-DARFS                         
142100       IF WS-PFI-TIRFS NOT = ZERO                                         
142200         IF WS-PFI-TIRFS < 5000000000                                     
142300           MOVE 20               TO  W-MXQB-DARFS (1:2)                   
142400         ELSE                                                             
142500           IF WS-PFI-TIRFS < 9999999999                                   
142600             MOVE 19             TO  W-MXQB-DARFS (1:2)                   
142700           ELSE                                                           
142800             MOVE 999999999999   TO  W-MXQB-DARFS                         
142900           END-IF                                                         
143000         END-IF                                                           
143100       END-IF                                                             
143200     END-IF                                                               
143300                                                                          
143400     MOVE 999999999999           TO  W-MXQA-DARFS                         
143500                                     W-MXQA-TILST-O                       
143600     MOVE WS-PFI-IDPRCBAS        TO  W-MIQA-IDPRC                         
143700                                     W-MXQA-IDPRC                         
143800                                     W-MIQB-IDPRCBAS                      
143900                                     W-MXQB-IDPRCBAS                      
144000                                     W-IDPRC-LOW                          
144100                                     W-IDPRC-UP                           
144200                                                                          
144300     MOVE WS-IDDC       TO  W-IDDC                                        
144400                            W-MIQA-IDDC                                   
144500                            W-MXQA-IDDC                                   
144600                            W-MIQB-IDDC                                   
144700                            W-MXQB-IDDC                                   
144800                            W-ORQA-IDDC                                   
144900                            WS-PF8-IDDC                                   
145000                            WS-PFE-IDDC                                   
145100     .                                                                    
145200     EJECT                                                                
145300 S02-INIT-PFE-PF8-NYCKLAR SECTION.                                        
145400                                                                          
145500     MOVE ZERO         TO MOD-PFE-IDORDER                                 
145600                          MOD-PFE-IDPRODNR                                
145700                          MOD-PFE-IDPLKLST                                
145800                          MOD-PFE-TIRFS                                   
145900                          MOD-PFE-TILST                                   
146000     MOVE LOW-VALUE    TO MOD-PFE-IDPRCVAR                                
146100                                                                          
146200     MOVE 9999999      TO MOD-PF8-IDORDER                                 
146300                          MOD-PF8-IDPRODNR                                
146400     MOVE 999          TO MOD-PF8-IDPLKLST                                
146500     MOVE 9999999999   TO MOD-PF8-TIRFS                                   
146600                          MOD-PF8-TILST                                   
146700     MOVE HIGH-VALUE   TO MOD-PF8-IDPRCVAR                                
146800                                                                          
146900     MOVE '106'        TO MED-IDMFSINF                                    
147000     CALL WMEDKONV USING MED-WMEDAREA                                     
147100     MOVE MED-MFSINF   TO MOD-TEMFSINF                                    
147200     .                                                                    
147300     EJECT                                                                
147400 S03-PRC-RULE SECTION.                                                    
147500                                                                          
147600     MOVE XXKH-4448-IDPRC    TO  WS-IDPRC-4448                            
147700     MOVE XXKH-4448-IDPRC    TO  WS-IDPRC-PRC                             
147800     MOVE XXKH-4448-KVVTID   TO  WS-KVVTID-PRC                            
147900     IF SDC-IT OR NDC-US                                                  
148000       MOVE XXKH-4448-KVPLSRAD TO  WS-KVRADER-PRC                         
148100       MOVE SHOW-ALL-ORDERS    TO  WS-KVORDER-PRC                         
148200       MOVE XXKH-4448-VKPLSNTO TO  WS-VKORDNTO-PRC                        
148300       MOVE XXKH-4448-VLPLSNTO TO  WS-VLORDNTO-PRC                        
148400     ELSE                                                                 
148500       MOVE XXKH-4448-KVRADER  TO  WS-KVRADER-PRC                         
148600       MOVE XXKH-4448-KVORDER  TO  WS-KVORDER-PRC                         
148700       MOVE XXKH-4448-VKORDNTO TO  WS-VKORDNTO-PRC                        
148800       MOVE XXKH-4448-VLORDNTO TO  WS-VLORDNTO-PRC                        
148900     END-IF                                                               
149000                                                                          
149100     PERFORM S03A-TAB-INIT                                                
149200     .                                                                    
149300     EJECT                                                                
149400 S03A-TAB-INIT SECTION.                                                   
149500                                                                          
149600     PERFORM S03AA-CNT-WRKDAYS                                            
149700     MOVE WS-IDPRC-4448    TO W-4437-IDPRC                                
149800     MOVE WS-IDDC          TO W-4437-IDDC                                 
149900     MOVE WS-LOCAL-DATE    TO W-4438-DADATUM                              
150000     IF WS-LOCAL-DATE NOT = ZERO                                          
150100       IF WS-LOCAL-DATE < 500000                                          
150200         MOVE 20           TO W-4438-DADATUM (1:2)                        
150300       ELSE                                                               
150400         IF WS-LOCAL-DATE < 999999                                        
150500           MOVE 19         TO W-4438-DADATUM (1:2)                        
150600         ELSE                                                             
150700           MOVE 99999999   TO W-4438-DADATUM                              
150800         END-IF                                                           
150900       END-IF                                                             
151000     END-IF                                                               
151100                                                                          
151200     PERFORM IMS-GU-4437-WDGX4438                                         
151300     IF SEGMENT-OK                                                        
151400        MOVE +2400 TO WS-WRKTIMES                                         
151500        MULTIPLY WS-WRKDAYS BY WS-WRKTIMES                                
151600        ADD WS-WRKTIMES TO 4437-4438-TISTOMIN-PAC                         
151700        DIVIDE 100 INTO 4437-4438-TISTAMIN-PAC GIVING                     
151800                                          WS-4438-TISTAMIN                
151900        DIVIDE 100 INTO 4437-4438-TISTOMIN-PAC GIVING                     
152000                                          WS-4438-TISTOMIN                
152100        PERFORM S03AB-LOAD-STASTOP                                        
152200     ELSE                                                                 
152300        MOVE NEJ TO NYCKLAR-SW                                            
152400     END-IF                                                               
152500     .                                                                    
152600     EJECT                                                                
152700 S03AA-CNT-WRKDAYS SECTION.                                               
152800                                                                          
152900     IF WS-PFI-TIRFS-AAMMDD > 0 AND                                       
153000        WS-PFI-TIRFS-AAMMDD NUMERIC                                       
153100       MOVE WS-LOCAL-DATE TO WORK-TIAAMMDD-TOM                            
153200       MOVE WS-PFI-TIRFS-AAMMDD TO TMP1-YYMMDD                            
153300       MOVE WS-LOCAL-DATE       TO TMP2-YYMMDD                            
153400       PERFORM WY2000P1                                                   
153500       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
153600             MOVE WS-PFI-TIRFS-AAMMDD TO WORK-TIAAMMDD-TOM                
153700       END-IF                                                             
153800     END-IF                                                               
153900                                                                          
154000     IF WS-PFI-TITRPAVT-AAMMDD > 0 AND                                    
154100        WS-PFI-TITRPAVT-AAMMDD NUMERIC                                    
154200       MOVE WS-PFI-TITRPAVT-AAMMDD TO TMP1-YYMMDD                         
154300       MOVE WS-PFI-TIRFS-AAMMDD    TO TMP2-YYMMDD                         
154400       PERFORM WY2000P1                                                   
154500       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
154600             MOVE WS-PFI-TITRPAVT-AAMMDD TO WORK-TIAAMMDD-TOM             
154700       END-IF                                                             
154800     END-IF                                                               
154900     PERFORM S00-WORKDAY                                                  
155000     .                                                                    
155100     EJECT                                                                
155200 S03AB-LOAD-STASTOP SECTION.                                              
155300                                                                          
155400     ACCEPT WS-LOCAL-TIME FROM TIME                                       
155500     MOVE DC-MSGI-TILOKTID TO WS-LOCAL-TIME(1:4)                          
155600     MOVE WS-LOCAL-TIHH    TO WS-TIHH                                     
155700     MOVE WS-LOCAL-TIMM    TO WS-TIMM                                     
155800     MOVE WS-TIHHMM        TO WS-4438-TIACTMIN                            
155900                                                                          
156000     MOVE +1  TO ARB-INDX                                                 
156100     PERFORM UNTIL ARB-INDX > MAX-ARB-INDX                                
156200       MOVE WS-4438-TIACTMIN TO WS-TISTAMIN-PAC (ARB-INDX)                
156300       MOVE WS-4438-TIACTMIN TO WS-TIACTMIN-PAC (ARB-INDX)                
156400       MOVE WS-4438-TISTOMIN TO WS-TISTOMIN-PAC (ARB-INDX)                
156500       ADD +1 TO ARB-INDX                                                 
156600     END-PERFORM                                                          
156700     .                                                                    
156800     SKIP2                                                                
156900 MFS-ERASE-FIELD-UT SECTION.                                              
157000*                                                                         
157100     MOVE +1 TO PU-INDX                                                   
157200     PERFORM UNTIL PU-INDX > MAX-INDX                                     
157300       PERFORM MFS-ERASE-MOD-LINE                                         
157400       ADD +1 TO PU-INDX                                                  
157500     END-PERFORM                                                          
157600     MOVE MFS-ERASE-FIELD TO MOD-KVBEMAN-DORD                             
157700                             MOD-KVPU-DORD                                
157800     .                                                                    
157900     SKIP2                                                                
158000*                                                                         
158100 MFS-ERASE-MOD-LINE SECTION.                                              
158200                                                                          
158300     MOVE MFS-ERASE-FIELD TO MOD-TILST-OD-RAD (PU-INDX)                   
158400                             MOD-SUPTID-RAD (PU-INDX)                     
158500                             MOD-TIRFS-RAD (PU-INDX)                      
158600                             MOD-KVRADER-RAD (PU-INDX)                    
158700                             MOD-KVRADER-MEZ-RAD (PU-INDX)                
158800                             MOD-VKORDNTO-RAD (PU-INDX)                   
158900                             MOD-VLORDNTO-RAD (PU-INDX)                   
159000                             MOD-KVORDER-RAD (PU-INDX)                    
159100                             MOD-IDTRP-RAD (PU-INDX)                      
159200     .                                                                    
159300     SKIP2                                                                
159400 MFS-ERASE-MOD-PFI SECTION.                                               
159500                                                                          
159600     MOVE MFS-ERASE-FIELD TO MOD-PFI-IDPRC                                
159700                             MOD-PFI-TIRFS-AAMMDD                         
159800                             MOD-PFI-TIRFS-HHMM                           
159900     MOVE MFS-ERASE-FIELD TO MOD-PFI-IDTRP                                
160000     MOVE MFS-ERASE-FIELD TO MOD-PFI-TITRPAVT-AAMMDD                      
160100                             MOD-PFI-TITRPAVT-HHMM                        
160200     MOVE MFS-ERASE-FIELD TO MOD-PFI-IDDC                                 
160300     .                                                                    
160400     EJECT                                                                
160500 MFS-KEEP-FIELD-UT SECTION.                                               
160600                                                                          
160700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVBEMAN-DORD                      
160800                                    MOD-KVPU-DORD                         
160900     .                                                                    
161000     EJECT                                                                
161100 MFS-KEEP-SCREEN SECTION.                                                 
161200                                                                          
161300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-PFI-IDPRC                         
161400                                    MOD-PFI-TIRFS-AAMMDD                  
161500                                    MOD-PFI-TIRFS-HHMM                    
161600                                    MOD-PFI-IDTRP                         
161700                                    MOD-PFI-TITRPAVT-AAMMDD               
161800                                    MOD-PFI-TITRPAVT-HHMM                 
161900                                    MOD-PFI-IDDC                          
162000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVBEMAN-DORD                      
162100                                    MOD-KVPU-DORD                         
162200     MOVE +1 TO PU-INDX                                                   
162300     PERFORM UNTIL PU-INDX > MAX-INDX                                     
162400     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
162500                             MOD-TILST-OD-RAD (PU-INDX)                   
162600                             MOD-SUPTID-RAD (PU-INDX)                     
162700                             MOD-TIRFS-RAD (PU-INDX)                      
162800                             MOD-KVRADER-RAD (PU-INDX)                    
162900                             MOD-KVRADER-MEZ-RAD (PU-INDX)                
163000                             MOD-VKORDNTO-RAD (PU-INDX)                   
163100                             MOD-VLORDNTO-RAD (PU-INDX)                   
163200                             MOD-KVORDER-RAD (PU-INDX)                    
163300                             MOD-IDTRP-RAD (PU-INDX)                      
163400       ADD +1 TO PU-INDX                                                  
163500     END-PERFORM                                                          
163600     .                                                                    
163700     EJECT                                                                
163800*                                                                         
163900* --- IMS SEKTIONER ---                                                   
164000     SKIP3                                                                
164100 IMS-GET-MSG SECTION.                                                     
164200                                                                          
164300     MOVE '  QC' TO GODK-STATUSKODER                                      
164400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
164500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
164600     PERFORM IMS-STATUSKONTROLL                                           
164700     .                                                                    
164800     SKIP3                                                                
164900*                                                                         
165000 IMS-INSERT-MSG SECTION.                                                  
165100*                                                                         
165200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
165300       MOVE '0' TO MFS-KDHUVOMR                                           
165400     END-IF                                                               
165500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
165600     MOVE SPACE TO GODK-STATUSKODER                                       
165700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
165800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165900     PERFORM IMS-STATUSKONTROLL                                           
166000     .                                                                    
166100     EJECT                                                                
166200 IMS-GN-NEXT-WDQ3A1 SECTION.                                              
166300                                                                          
166400     STRING 'WLORQB01(WDQ3A1KY=>' W-MIN-WDQ3A1KY                          
166500                    '&WDQ3A1KY<=' W-MAX-WDQ3A1KY                          
166600                    '&IDPRC   =>' W-IDPRC-LOW                             
166700                    '&IDPRC   <=' W-IDPRC-UP  ')'                         
166800                     DELIMITED BY SIZE INTO SSA1                          
166900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
167000     CALL CBLTDLI USING GN ORQB-PCB DLI-IO-AREA1 SSA1                     
167100     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
167200     PERFORM IMS-STATUSKONTROLL                                           
167300     .                                                                    
167400     EJECT                                                                
167500 IMS-GN-NEXT-WDQ3B1 SECTION.                                              
167600*                                                                         
167700     STRING 'WLORQC01(WDQ3B1KY=>' W-MIN-WDQ3B1KY                          
167800                    '&WDQ3B1KY<=' W-MAX-WDQ3B1KY                          
167900                    '&IDPRCVAR=>' W-IDPRCVAR-LOW                          
168000                    '&IDPRCVAR<=' W-IDPRCVAR-UP  ')'                      
168100                     DELIMITED BY SIZE INTO SSA1                          
168200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
168300     CALL CBLTDLI USING GN ORQC-PCB DLI-IO-AREA1 SSA1                     
168400     MOVE ORQC-STATUS-CODE TO STATUS-WS                                   
168500     PERFORM IMS-STATUSKONTROLL                                           
168600     .                                                                    
168700     EJECT                                                                
168800 IMS-GU-ORQA-WDQ301 SECTION.                                              
168900*                                                                         
169000     STRING 'WLORQA01(WDQ301KY =' W-ORQA-WDQ301KY ')'                     
169100          DELIMITED BY SIZE INTO SSA1                                     
169200     MOVE '  GE' TO GODK-STATUSKODER                                      
169300     CALL CBLTDLI USING GU  ORQA-PCB DLI-IO-AREA1 SSA1                    
169400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
169500     PERFORM IMS-STATUSKONTROLL                                           
169600     .                                                                    
169700     EJECT                                                                
169800*                                                                         
169900 IMS-GU-XXKH-WDGX4448 SECTION.                                            
170000*                                                                         
170100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
170200          DELIMITED BY SIZE INTO SSA1                                     
170300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY ')'                      
170400          DELIMITED BY SIZE INTO SSA2                                     
170500     MOVE '  GE' TO GODK-STATUSKODER                                      
170600     CALL CBLTDLI USING GU  XXKH-PCB DLI-IO-AREA2 SSA1 SSA2               
170700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
170800     PERFORM IMS-STATUSKONTROLL                                           
170900     .                                                                    
171000     EJECT                                                                
171100 IMS-GU-4437-WDGX4438 SECTION.                                            
171200*                                                                         
171300     STRING 'WL443701(WDGXKEY  =' W-4437-WDGXKEY ')'                      
171400          DELIMITED BY SIZE INTO SSA1                                     
171500     STRING 'WL443711(DADATUM  =' W-4438-WDGXKEY ')'                      
171600          DELIMITED BY SIZE INTO SSA2                                     
171700     MOVE '  GE' TO GODK-STATUSKODER                                      
171800     CALL CBLTDLI USING GU  4437-PCB DLI-IO-AREA3 SSA1 SSA2               
171900     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
172000     PERFORM IMS-STATUSKONTROLL                                           
172100     .                                                                    
172200     EJECT                                                                
172300*                                                                         
172400 IMS-GU-WDQ221 SECTION.                                                   
172500*                                                                         
172600     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
172700          DELIMITED BY SIZE INTO SSA1                                     
172800     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
172900          DELIMITED BY SIZE INTO SSA2                                     
173000     STRING 'WDQ221  (ADLAGOMR =' W-ADLAGOMR-X ')'                        
173100          DELIMITED BY SIZE INTO SSA3                                     
173200     MOVE '  GE' TO GODK-STATUSKODER                                      
173300     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA1 SSA1 SSA2 SSA3           
173400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
173500     PERFORM IMS-STATUSKONTROLL                                           
173600     .                                                                    
173700                                                                          
173800*                                                                         
173900 IMS-STATUSKONTROLL SECTION.                                              
174000                                                                          
174100     SET STATUS-IX TO 1                                                   
174200     SEARCH GODK-STATUS                                                   
174300       AT END CALL FELLOG                                                 
174400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
174500     END-SEARCH                                                           
174600     .                                                                    
174700     EJECT                                                                
174800*    -COPY WY2000P1                                                       
