001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4067700.                                                
001600 AUTHOR.         KARANDE DIGAMBAR.                                        
001700 DATE-WRITTEN.   02/09/12.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002010*        THIS PROGRAM IS DDIVERSION FOR PROFORMA INVOICE RELEASE          
002020*        OF W4067500. MANY OF THE FUNCTIONS IN PROGRAM W4067500           
002030*        ARE ALSO VALID FOR THE W4067700. THE PROGRAM STARTS              
002040*        BUILDING UP THE NEW DATABASE WDE2 FOR INVOICING.                 
002500*        THIS PROGRAM RUN IN THE BACKGROUND OR AS A SCREEN. SCREEN        
002510*        ALLOWS TO START 4678 WHICH UPDATE THE INFORMATION ON             
002520*        WDE211.                                                          
002600*        THIS PROGRAM IS STARTED BY W4066400.                             
002700*                                                                         
002803*        THE PROGRAM UPDATES   WDE2                                       
002804*        THE PROGRAM READS     WDR4                                       
002810*        THE PROGRAM READS     WDR1                                       
002820*        THE PROGRAM READS     WDB6                                       
002900*                                                                         
003000*    INDATA.                                                              
003100*        TRANSACTION: W4T677                                              
003200*        MID:         W4I67701                                            
003300*                                                                         
003400*    OUTDATA.                                                             
003500*        MOD:         W4O67701                                            
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'W4067700'.            
004400                                                                          
004500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004700                                                                          
004800 77  YES                         PIC X       VALUE 'J'.                   
004810 77  YES-Y                       PIC X       VALUE 'Y'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  WS-CDC-11                   PIC X(11)   VALUE '11'.                  
005101                                                                          
005102*    --- INDEX FOR SCROLL LINES                                           
005103 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005110 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
005111 77  IDLAND-INDX                 PIC S9(4)  VALUE +0    COMP SYNC.        
005112 77  MAX-IDLAND-INDX             PIC S9(4)  VALUE +23   COMP SYNC.        
005120 77  IDPSN-IX                    PIC S9(3)  VALUE +0    COMP SYNC.        
005320 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0 COMP SYNC.           
005330 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005340 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
005400                                                                          
005501 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005502     88  INDATA-OK                           VALUE 'J'.                   
005510     88  INDATA-WRONG                        VALUE 'N'.                   
005600                                                                          
005700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005800     88  KEYS-OK                             VALUE 'J'.                   
005900     88  KEYS-WRONG                          VALUE 'N'.                   
006000                                                                          
006010 77  INPUT-SW                    PIC X       VALUE 'N'.                   
006020     88  INPUT-GIVEN                         VALUE 'J'.                   
006040                                                                          
006050 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
006060     88  KDCMD-GIVEN                         VALUE 'J'.                   
006070                                                                          
006080 77  4482-MISSING-SW             PIC X       VALUE 'N'.                   
006090     88  4482-MISSING                        VALUE 'J'.                   
006091                                                                          
006092 77  START-4540-4537-SW          PIC X       VALUE 'N'.                   
006093     88  START-4540-4537                     VALUE 'J'.                   
006094                                                                          
006100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006200     88  OWN-MID                             VALUE '4677'.                
006300     88  GOOD-MID                            VALUE '466D'                 
006400                                                   '4677'                 
006401                                                   '4678'.                
006410** SCREEN 4664 START 4677 USING IDTRANS '466D'                            
006500     88  466D-MID                            VALUE '466D'.                
006600     88  4678-MID                            VALUE '4678'.                
006800     88  HELP-MID                            VALUE '0551'.                
006810                                                                          
006811 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
006812 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
006813                                                                          
006817       EJECT                                                              
006820 77  UPDATE-SAVE-SW              PIC X(01)   VALUE 'N'.                   
006830     88  UPDATE-SAVE                         VALUE 'J'.                   
006831                                                                          
006835 77  WRITE-DHL-SW                PIC X       VALUE 'N'.                   
006836     88  WRITE-DHL                           VALUE 'J'.                   
006837                                                                          
006840 77  RESTART-SW                  PIC X(01)   VALUE 'N'.                   
006850     88 RESTART                              VALUE 'J'.                   
006851                                                                          
006892 77  FLSAMFAK-SW                 PIC X(01)   VALUE 'N'.                   
006893     88 FLSAMFAK                             VALUE 'J'.                   
006894                                                                          
006904 77  W-FLAVSLUTA-SW              PIC X(01)   VALUE ' '.                   
006905     88 FLAVSLUTA-OK                         VALUE 'Y' 'J'.               
006906     EJECT                                                                
006907*  TO CONVERT THE YYMMDD  TO CCYYMMDD                                     
006910 01  W-YYMMDD-DATUM              PIC 9(6).                                
006920 01  W-CCYYMMDD-DATUM.                                                    
006930     03 W-CC                     PIC 9(2).                                
006940     03 W-YYMMDD.                                                         
006950        05 W-YY                  PIC 9(2).                                
006960        05 W-MM                  PIC 9(2).                                
006970        05 W-DD                  PIC 9(2).                                
006971                                                                          
006972 77  W-UPD-COUNT                 PIC S9(3)   VALUE ZERO.                  
006974 77  W-UPD-MAX                   PIC S9(3)   VALUE +100.                  
006976 77  W-IDDISTR-PREV              PIC S9(5)   COMP-3 VALUE ZERO.           
006977 77  W-IDKUNDNR-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
006982 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
006983                                                                          
006985 77  W-FLFARLIG                  PIC X(01)   VALUE SPACE.                 
006987 77  W-FLAVSLUTA                 PIC X(01)   VALUE SPACE.                 
006988                                                                          
006991 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
007006                                                                          
007008 01  W-IDDCTEXT-MSGI.                                                     
007009     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
007010     03  W-IDDC-MSGI             PIC X(2).                                
007011                                                                          
007015     EJECT                                                                
007016                                                                          
007020*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007100 01  GENERAL-SUBPROGRAMS.                                                 
007200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007610     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
007700     EJECT                                                                
007800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007900*01 -COPY WMEDAREA                                                        
008000     SKIP3                                                                
008010 01  FILLER.                                                              
008020   03  FELMEDD-AREA.                                                      
008030     05  FELMEDD-ENGLISH.                                                 
008040       10  FILLER                PIC X(40)                                
008050           VALUE '622 4677 WRONG PICTURE SELECTED         '.              
008051                                                                          
008052   03 WS-TISKPTID              PIC 9(6)       VALUE ZERO.                 
008053   03 WS-TISKPTID-GRP          REDEFINES WS-TISKPTID.                     
008054      05 WS-TISKPTID-HHMM         PIC 9(4).                               
008055      05 WS-TISKPTID-SS           PIC 9(2).                               
008056                                                                          
008060                                                                          
008100 01  MESSAGE-CODES.                                                       
008201     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008202     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008204     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
008205     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008210     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008301     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008302     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008310     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008320     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
008400     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
008500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008600     EJECT                                                                
008700*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009000     SKIP3                                                                
009100*01 -COPY WMSGINIT                                                        
009200     EJECT                                                                
009201 01  FILLER                      PIC X(16)   VALUE 'DC-WMSGINIT'.         
009202     SKIP3                                                                
009203*01 -COPY WMSGINIT         -PRE  DC-                                      
009204     EJECT                                                                
009205*    --- AREA  FOR WZ01  ------                                           
009206 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
009207*01  -COPY WZ01SEND                                                       
009208                                                                          
009209*    --- AREA  FOR W476SHNO ---                                           
009210 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
009211*01  -COPY W476SHNO                                                       
009212                                                                          
009213*01    -COPY WDECAREA                                                     
009214     EJECT                                                                
009215                                                                          
009310*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009400*                                                                         
009500 01  SAVE-AREA.                                                           
009600     03  SAVE-IDTRANS            PIC X(4)    VALUE '4677'.                
009700* COMMON AREA FOR 4677 AND 4678                                           
009701     03  SAVE-4677-4678.                                                  
009703         05  SAVE-IDDISTR-ENTER      PIC S9(5).                           
009704         05  SAVE-IDKUNDNR-ENTER     PIC S9(7).                           
009705         05  SAVE-KDFAKTYP-ENTER     PIC X(01).                           
009706         05  SAVE-IDKUNDRF-ENTER     PIC X(10).                           
009707         05  SAVE-IDPRODNR-ENTER     PIC S9(07).                          
009708         05  SAVE-IDKOLLI-ENTER      PIC S9(05).                          
009709         05  SAVE-IDDISTR-4678       PIC S9(5).                           
009710         05  SAVE-IDKUNDNR-4678      PIC S9(7).                           
009711         05  SAVE-FLSAMFAK-4678      PIC X(01)   VALUE SPACE.             
009712         05  SAVE-IDSHIPM            PIC  9(7).                           
009713         05  SAVE-FLFARLIG           PIC X(01).                           
009715         05  SAVE-FLAVSLUTA          PIC X(01).                           
009716         05  SAVE-REL-PROFORMA       PIC X(01)   VALUE 'N'.               
009717             88  REL-PROFORMA                    VALUE 'J'.               
009725*                                                                         
009726     03  SAVE-IDDISTR-NEXT       PIC S9(5).                               
009727     03  SAVE-IDKUNDNR-NEXT      PIC S9(7).                               
009728     03  SAVE-KDFAKTYP-NEXT      PIC X(01).                               
009729     03  SAVE-IDKUNDRF-NEXT      PIC X(10).                               
009730     03  SAVE-IDPRODNR-NEXT      PIC S9(07).                              
009731     03  SAVE-IDKOLLI-NEXT       PIC S9(05).                              
009740*                                                                         
009760     03  SAVE-INPUT  OCCURS 14.                                           
009761         05  SAVE-RECORD-PRESENT PIC X(01)   VALUE SPACE.                 
009762         05  SAVE-IDDISTR        PIC S9(05).                              
009763         05  SAVE-IDKUNDNR       PIC S9(07).                              
009764         05  SAVE-PRFOERS        PIC S9(7)V9(2)    COMP-3.                
009765         05  SAVE-REFOERS        PIC S9(2)V9(3)    COMP-3.                
009766         05  SAVE-REOVKOFF       PIC S9(2)V9(1)    COMP-3.                
009767         05  SAVE-FLSAMFAK       PIC X(01)   VALUE SPACE.                 
009800     EJECT                                                                
009900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010200     SKIP3                                                                
010300*01  MID -COPY W4I67701                                                   
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
010600     SKIP3                                                                
010700*01  -COPY WMSGAREA                                                       
010800     EJECT                                                                
010810*   TO RETURN TO MAIN MENU                                                
010820*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
010830     EJECT                                                                
010900     03  MOD REDEFINES MSG-AREA.                                          
011000*      05  -COPY W4O67701                                                 
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011300     SKIP3                                                                
011400*01  -COPY WMFSAREA                                                       
011500     EJECT                                                                
011501*01  -COPY W40636I1   -PRE MOD4636-                                       
011502     EJECT                                                                
011510 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
011520 01      P-TO-P-SW.                                                       
011530                                                                          
011540  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
011550  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
011560  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
011570  02     P-TO-P-KDTRANS          PIC X(8).                                
011580  02     P-TO-P-IDTRANS          PIC X(4).                                
011590  02     P-TO-P-KDMFSFOR         PIC X(1).                                
011591  02     P-TO-P-DATA             PIC X(1000).                             
011600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011700*                                                                         
011710*01  -COPY W4I66401   -PRE MOD4664-                                       
011720*                                                                         
011730*                                                                         
011740*01  -COPY W4I67801   -PRE PTOP6-                                         
011750*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  KEYS-TO-DLI.                                                         
012101*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
012102     03  W-4479-X.                                                        
012103         05  W-IDHTR             PIC X(4)    VALUE '4479'.                
012104         05  W-IDDC-4479         PIC X(2)    VALUE SPACE.                 
012105         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
012106         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
012107         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
012108                                                                          
012109     03  W-4482-X.                                                        
012110         05  W-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
012111         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
012112         05  W-KDFAKTYP          PIC  X(1)   VALUE SPACE.                 
012113         05  W-IDKUNDRF          PIC  X(10).                              
012114         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
012115           07  W-IDORDNR7        PIC  9(7).                               
012116           07  FILLER            PIC  X(3).                               
012117         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
012118         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
012119                                                                          
012120     03  W-4482-N-X.                                                      
012121         05  W-IDDISTR-N         PIC S9(5)   VALUE ZERO  COMP-3.          
012122         05  W-IDKUNDNR-N        PIC S9(7)   VALUE ZERO  COMP-3.          
012123         05  FILLER              PIC X(18)   VALUE HIGH-VALUE.            
012124                                                                          
012133     03  W-IDDC-X.                                                        
012134         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
012167                                                                          
012184     03  W-IDSHIPM-X.                                                     
012185         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
012186                                                                          
012191     03  W-WDE211KY-X.                                                    
012192         05  W-WDE211-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
012193         05  W-WDE211-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
012194                                                                          
012195     03  W-IDGMT-X.                                                       
012196         05  W-WDB201-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
012197         05  W-WDB201-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
012198                                                                          
012199     03  W-IDDC-B6-X.                                                     
012200         05 W-IDDC-B6                  PIC X(2).                          
012201                                                                          
012210     SKIP2                                                                
012300*    --- STATUS-KOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FOUND                       VALUE '  '.                  
012600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012800     SKIP2                                                                
012900 01  GOOD-STATUSCODES.                                                    
013000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(128).                              
013300 01  SSA2                        PIC X(128).                              
013310 01  SSA3                        PIC X(128).                              
013320 01  SSA4                        PIC X(128).                              
013400     EJECT                                                                
013500*    --- IMS FUNCTION CODES                                               
013600*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014100*                                                                         
014101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4479'.                    
014102 01  DLI-IO-WDGX4479.                                                     
014103*    03  -COPY WDGX4479                                                   
014104                                                                          
014105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4482'.                    
014106 01  DLI-IO-WDGX4482.                                                     
014107*    03  -COPY WDGX4482                                                   
014124                                                                          
014167 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
014168 01  DLI-IO-WDE201.                                                       
014169*    03  -COPY WDE201                                                     
014170     EJECT                                                                
014171 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE211'.                      
014172 01  DLI-IO-WDE211.                                                       
014173*    03  -COPY WDE211                                                     
014174     EJECT                                                                
014175 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE221'.                      
014176 01  DLI-IO-WDE221.                                                       
014177*    03  -COPY WDE221                                                     
014400     EJECT                                                                
014401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
014402 01  DLI-IO-WDB201.                                                       
014403*    03  -COPY WDB201                                                     
014404                                                                          
014405 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014406 01   DLI-IO-AREA-B601.                                                   
014407*     03  -COPY WDB601                                                    
014408                                                                          
014409     EJECT                                                                
014520 LINKAGE SECTION.                                                         
014600*01  -COPY W0009  -PRE MSG-                                               
014601     EJECT                                                                
014610*01  -COPY W0009  -PRE AD36-                                              
014620     EJECT                                                                
014621*01  -COPY W0009  -PRE ALT-                                               
014622     EJECT                                                                
014630*01  -COPY W0009  -PRE ALT1-                                              
014640     EJECT                                                                
014650*01  -COPY W0009  -PRE ALT2-                                              
014660     EJECT                                                                
014670*01  -COPY W0009  -PRE ALT3-                                              
014680     EJECT                                                                
014700*01  -COPY W0008  -PRE WDP7-                                              
014800     05  FILLER                  PIC X.                                   
014901                                                                          
014908*01  -COPY W0008  -PRE WDE2-                                              
014909     05  FILLER                  PIC X.                                   
014910                                                                          
014911*01  -COPY W0008  -PRE WDB2-                                              
014912     05  FILLER                  PIC X.                                   
014913                                                                          
014914*01  -COPY W0008  -PRE 4479-                                              
014915     05  FILLER                  PIC X.                                   
014916                                                                          
014917*01  -COPY W0008  -PRE WDB6-                                              
014918     05  FILLER                  PIC X.                                   
014919                                                                          
015101 01  SHNO-4517-PCB               PIC X.                                   
015103     EJECT                                                                
015104                                                                          
015105 PROCEDURE DIVISION  USING MSG-PCB  AD36-PCB  ALT-PCB   ALT1-PCB          
015106                           ALT2-PCB ALT3-PCB  WDP7-PCB  WDE2-PCB          
015107                           WDB2-PCB 4479-PCB  WDB6-PCB                    
015108                           SHNO-4517-PCB.                                 
015111 MAIN SECTION.                                                            
015112     ENTRY 'DLITCBL' USING MSG-PCB  AD36-PCB  ALT-PCB   ALT1-PCB          
015113                           ALT2-PCB ALT3-PCB  WDP7-PCB  WDE2-PCB          
015114                           WDB2-PCB 4479-PCB  WDB6-PCB                    
015115                           SHNO-4517-PCB.                                 
015300                                                                          
015400     PERFORM IMS-GET-MSG                                                  
015500     IF SEGMENT-FOUND                                                     
015600       PERFORM A-INIT                                                     
015610       IF GOOD-MID                                                        
015700         PERFORM B-CHECK-KEYS                                             
015710       END-IF                                                             
015800       IF KEYS-OK                                                         
015901         IF MFS-UPDATE                                                    
015902           PERFORM G-CHECK-INPUT                                          
015903           IF INDATA-OK                                                   
015904             PERFORM H-UPDATE                                             
015905           END-IF                                                         
015906           IF INDATA-OK                                                   
015907             IF REL-PROFORMA                                              
015908               PERFORM I-REL-PROFORMA                                     
015909             END-IF                                                       
015910           END-IF                                                         
015911         ELSE                                                             
015912           IF MFS-UPD-X                                                   
015913             PERFORM I-REL-PROFORMA                                       
015920           ELSE                                                           
015930             IF 4678-MID                                                  
015940               PERFORM M-RETURN-FROM-4678                                 
015950             ELSE                                                         
016001               IF MFS-FIRST                                               
016002                 PERFORM C-FIRST-PAGE                                     
016003               ELSE                                                       
016004                 IF MFS-NEXT                                              
016005                   PERFORM D-NEXT-PAGE                                    
016006                 ELSE                                                     
016007                   IF MFS-SPLIT                                           
016008                     PERFORM L-PF9-SPLIT                                  
016009                   ELSE                                                   
016010                     PERFORM E-SAME-PAGE                                  
016011                   END-IF                                                 
016012                 END-IF                                                   
016013               END-IF                                                     
016014             END-IF                                                       
016020           END-IF                                                         
016210         END-IF                                                           
016221         IF INDATA-OK AND NOT REL-PROFORMA AND NOT MFS-UPDATE             
016222            AND NOT MFS-SPLIT                                             
016300           PERFORM F-READ-SHOW-INFO                                       
016320         END-IF                                                           
016321       END-IF                                                             
016330       IF INDATA-OK AND REL-PROFORMA AND NOT RESTART                      
016340         PERFORM K-START-W40664                                           
016350       ELSE                                                               
016360          IF RESTART                                                      
016370            PERFORM J-START-W40677                                        
016380          ELSE                                                            
016381             IF INDATA-OK AND MFS-SPLIT                                   
016382               COMPUTE P-TO-P-KVLL =                                      
016383                             LENGTH OF PTOP6-MID-W4I67801 + 17            
016397               PERFORM IMS-ISRT-MSG-ALT3                                  
016398             ELSE                                                         
016399               IF GOOD-MID                                                
016400                 COMPUTE MSG-KVLL = LENGTH OF MOD-W4O67701 + 4            
016401                 PERFORM IMS-INSERT-MSG                                   
016402               END-IF                                                     
016403             END-IF                                                       
016404          END-IF                                                          
016410       END-IF                                                             
016830       IF UPDATE-SAVE                                                     
016850          MOVE '002'       TO MSGI-KDCALL                                 
016860          MOVE '4677'      TO SAVE-IDTRANS                                
016870          MOVE SAVE-AREA   TO MSGI-SPAR-AREA                              
016880          CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                      
016881       END-IF                                                             
016900     END-IF                                                               
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     IF MSG-DOUBLE-TRANSACTIONS                                           
017900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I67701                 
018000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018200     ELSE                                                                 
018300       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I67701                  
018400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018600     END-IF                                                               
018800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019200     MOVE LOW-VALUE TO MSG-AREA                                           
019300     MOVE 'W4O677N1' TO MFS-IDMOD                                         
019400     MOVE '4677' TO MOD-IDTRANS                                           
019500     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019600                                                                          
020201     IF NOT GOOD-MID                                                      
020202       MOVE NOO  TO KEYS-SW                                               
020203                    INDATA-SW                                             
020205       PERFORM S25-WRONG-PICTURE-MESSAGE                                  
020206     END-IF                                                               
020230                                                                          
020300     MOVE ZERO        TO W-IDSHIPM                                        
020400     MOVE ZERO        TO W-IDDISTR-N                                      
020410     MOVE ALL '9'     TO W-IDKUNDNR-N                                     
020490                                                                          
020500     .                                                                    
020600     EJECT                                                                
020610                                                                          
020700 B-CHECK-KEYS SECTION.                                                    
020800                                                                          
020900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021000     MOVE '001'             TO MSGI-KDCALL                                
021100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021300     MOVE '4677'            TO MSGI-IDTRANS                               
021310                                                                          
021400     IF GOOD-MID                                                          
021520        MOVE MID-IDTRPTNR-IN   TO MSGI-IDTRPTNR                           
021530        MOVE MID-IDLBBET-IN    TO MSGI-IDLBBET                            
021540        MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                           
021600     END-IF                                                               
021610                                                                          
021700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021701                                                                          
021702     MOVE MSGI-SPAR-AREA       TO SAVE-AREA                               
021703                                                                          
021710     IF 466D-MID                                                          
021720       MOVE MID-IDTRPTNR-IN    TO MSGI-IDTRPTNR                           
021730       MOVE MID-IDLBBET-IN     TO MSGI-IDLBBET                            
021740       MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                           
021741       MOVE SPACE              TO SAVE-REL-PROFORMA                       
021742       MOVE YES                TO UPDATE-SAVE-SW                          
021750     END-IF                                                               
021760                                                                          
022000*    - LANGUAGE TO BE USED BY MEDKONV                                     
022100     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
022200                                                                          
022300     MOVE YES                  TO KEYS-SW                                 
022501                                                                          
022502*    -- CHECK OF WDGXKEY                                                  
022503     MOVE MFS-ERASE-FIELD      TO MOD-IDTRPTNR-IN                         
022504                                  MOD-IDLBBET-IN                          
022505                                  MOD-FLFARLIG-IN                         
022506                                  MOD-IDDC-IN                             
022507                                                                          
022508     IF MFS-UPD-X OR REL-PROFORMA                                         
022509       MOVE YES  TO SAVE-REL-PROFORMA                                     
022510       MOVE YES  TO UPDATE-SAVE-SW                                        
022511     ELSE                                                                 
022512       IF GOOD-MID                                                        
022513         IF MID-IDTRPTNR-IN NOT = ALL '+'                                 
022514            MOVE '7'         TO MFS-IDPFK                                 
022515            MOVE SPACE       TO MFS-KDTRTYP                               
022516         END-IF                                                           
022517         IF MID-IDLBBET-IN  NOT = ALL '+'                                 
022518           MOVE '7'         TO MFS-IDPFK                                  
022519           MOVE SPACE       TO MFS-KDTRTYP                                
022520         END-IF                                                           
022521         IF MID-IDDC-IN     NOT = ALL '+'                                 
022522           MOVE '7'         TO MFS-IDPFK                                  
022523           MOVE SPACE       TO MFS-KDTRTYP                                
022524         END-IF                                                           
022525         IF MID-FLFARLIG-IN NOT = ALL '+'                                 
022526           MOVE '7'         TO MFS-IDPFK                                  
022527           MOVE SPACE       TO MFS-KDTRTYP                                
022528         END-IF                                                           
022529       ELSE                                                               
022530         MOVE '7'         TO MFS-IDPFK                                    
022531         MOVE SPACE       TO MFS-KDTRTYP                                  
022532       END-IF                                                             
022533       IF MFS-FIRST OR 4678-MID                                           
022534         MOVE SPACE       TO SAVE-FLAVSLUTA                               
022535                             SAVE-REL-PROFORMA                            
022536         MOVE YES         TO UPDATE-SAVE-SW                               
022537       END-IF                                                             
022538     END-IF                                                               
022539                                                                          
022542     IF MSGI-IDTRPTNR NUMERIC                                             
022543        IF MSGI-IDTRPTNR > ZERO                                           
022544           MOVE MSGI-IDTRPTNR   TO  W-IDTRPTNR                            
022546        ELSE                                                              
022547          MOVE NOO     TO KEYS-SW                                         
022548        END-IF                                                            
022549     ELSE                                                                 
022550        MOVE NOO       TO KEYS-SW                                         
022551     END-IF                                                               
022552                                                                          
022553     IF MSGI-IDLBBET = ALL '+'                                            
022554        MOVE NOO           TO KEYS-SW                                     
022555     ELSE                                                                 
022556       IF MSGI-IDLBBET > SPACE                                            
022557          MOVE MSGI-IDLBBET  TO W-IDLBBET                                 
022559       ELSE                                                               
022560          MOVE NOO           TO KEYS-SW                                   
022561       END-IF                                                             
022562     END-IF                                                               
022570                                                                          
022578     IF MID-FLFARLIG-IN NOT = ALL '+'                                     
022579       MOVE MID-FLFARLIG-IN    TO W-FLFARLIG                              
022580     ELSE                                                                 
022581       IF SAVE-FLFARLIG > SPACE                                           
022582         MOVE SAVE-FLFARLIG    TO W-FLFARLIG                              
022583       END-IF                                                             
022584     END-IF                                                               
022585                                                                          
022586     IF W-FLFARLIG NOT = SAVE-FLFARLIG                                    
022587       MOVE W-FLFARLIG         TO SAVE-FLFARLIG                           
022588       MOVE YES                TO UPDATE-SAVE-SW                          
022589     END-IF                                                               
022590                                                                          
022591                                                                          
022592     MOVE MSGI-IDDC-KEY        TO W-IDDC-B6                               
022593     PERFORM IMS-GU-WDB601                                                
022594     IF DCS-KDDC NOT = SPACE                                              
022595       MOVE MSGI-IDDC-KEY      TO W-IDDC                                  
022596                                  W-IDDC-4479                             
022597       IF DCS-DDC                                                         
022598         MOVE WS-CDC-11        TO W-IDDC-MSGI                             
022599       ELSE                                                               
022600         MOVE MSGI-IDDC-KEY    TO W-IDDC-MSGI                             
022601       END-IF                                                             
022602                                                                          
022603       MOVE ALL '+'            TO DC-MSGI-WMSGINIT                        
022604       MOVE '001'              TO DC-MSGI-KDCALL                          
022605       MOVE W-IDDCTEXT-MSGI    TO DC-MSGI-IDUSER                          
022606       MOVE '4677'             TO DC-MSGI-IDTRANS                         
022607       MOVE MSG-LTERM-NAME     TO DC-MSGI-IDLTERM-USER                    
022608       CALL W005INIT        USING DC-MSGI-WMSGINIT WDP7-PCB               
022609                                                                          
022610     ELSE                                                                 
022611       MOVE NOO                TO KEYS-SW                                 
022612     END-IF                                                               
022613                                                                          
022614     IF 466D-MID                                                          
022615       IF MID-IDSHIPM NUMERIC                                             
022616         IF MID-IDSHIPM > ZERO                                            
022617           MOVE MID-IDSHIPM    TO W-IDSHIPM                               
022618         ELSE                                                             
022619           MOVE ZERO           TO W-IDSHIPM                               
022620         END-IF                                                           
022621       ELSE                                                               
022622         MOVE ZERO             TO W-IDSHIPM                               
022623       END-IF                                                             
022624       MOVE W-IDSHIPM          TO SAVE-IDSHIPM                            
022625                                  MOD-IDSHIPM                             
022626       MOVE YES                TO UPDATE-SAVE-SW                          
022629     ELSE                                                                 
022630       IF SAVE-IDSHIPM NUMERIC                                            
022631         MOVE SAVE-IDSHIPM     TO W-IDSHIPM                               
022632       ELSE                                                               
022633         MOVE ZERO             TO W-IDSHIPM                               
022634       END-IF                                                             
022635     END-IF                                                               
022636                                                                          
022637     IF GOOD-MID OR KEYS-OK                                               
022638       MOVE MSGI-IDTRPTNR       TO MOD-IDTRPTNR-UT                        
022639       MOVE MSGI-IDLBBET        TO MOD-IDLBBET-UT                         
022640       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
022641       MOVE SAVE-FLFARLIG       TO MOD-FLFARLIG-UT                        
022642       MOVE SAVE-FLAVSLUTA      TO MOD-FLAVSLUTA                          
022643       MOVE SAVE-IDSHIPM        TO MOD-IDSHIPM                            
022644     ELSE                                                                 
022645       PERFORM MFS-ERASE-FIELD-OUT                                        
022646       MOVE MFS-ERASE-FIELD     TO MOD-FLAVSLUTA                          
022650     END-IF                                                               
022700                                                                          
022800     IF KEYS-WRONG                                                        
022900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
023000       CALL WMEDKONV USING MED-WMEDAREA                                   
023100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023200       PERFORM MFS-ERASE-FIELD-IN                                         
023300       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
023400     END-IF                                                               
023500     .                                                                    
023601     EJECT                                                                
023602 C-FIRST-PAGE SECTION.                                                    
023603                                                                          
023604     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023605     CALL WMEDKONV USING MED-WMEDAREA                                     
023606     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
023607                                                                          
023608     PERFORM MFS-ERASE-FIELD-IN                                           
023609     .                                                                    
023610     EJECT                                                                
023611 D-NEXT-PAGE SECTION.                                                     
023612                                                                          
023613     IF SAVE-IDTRANS = '4677'                                             
023614       IF SAVE-IDDISTR-NEXT NUMERIC AND SAVE-IDDISTR-NEXT > ZERO          
023615         MOVE SAVE-IDDISTR-NEXT     TO W-IDDISTR                          
023616       ELSE                                                               
023617         MOVE SAVE-IDDISTR-ENTER    TO W-IDDISTR                          
023618       END-IF                                                             
023619       IF SAVE-IDKUNDNR-NEXT NUMERIC AND SAVE-IDKUNDNR-NEXT > ZERO        
023620         MOVE SAVE-IDKUNDNR-NEXT    TO W-IDKUNDNR                         
023621       ELSE                                                               
023622         MOVE SAVE-IDKUNDNR-ENTER   TO W-IDKUNDNR                         
023623       END-IF                                                             
023624       MOVE SAVE-KDFAKTYP-NEXT      TO W-KDFAKTYP                         
023625       MOVE SAVE-IDKUNDRF-NEXT      TO W-IDKUNDRF                         
023626       IF SAVE-IDPRODNR-NEXT NUMERIC AND SAVE-IDPRODNR-NEXT > ZERO        
023627         MOVE SAVE-IDPRODNR-NEXT    TO W-IDPRODNR                         
023628       ELSE                                                               
023629         MOVE SAVE-IDPRODNR-ENTER   TO W-IDPRODNR                         
023630       END-IF                                                             
023631       IF SAVE-IDKOLLI-NEXT NUMERIC AND SAVE-IDKOLLI-NEXT > ZERO          
023632         MOVE SAVE-IDKOLLI-NEXT     TO W-IDKOLLI                          
023633       ELSE                                                               
023634         MOVE SAVE-IDKOLLI-ENTER    TO W-IDKOLLI                          
023635       END-IF                                                             
023636     ELSE                                                                 
023637       PERFORM MFS-ERASE-FIELD-IN                                         
023638     END-IF                                                               
023639     .                                                                    
023640     EJECT                                                                
023641 E-SAME-PAGE SECTION.                                                     
023642                                                                          
023643     IF SAVE-IDTRANS = '4677' OR '0551'                                   
023647       MOVE SAVE-IDDISTR-ENTER      TO W-IDDISTR                          
023652       MOVE SAVE-IDKUNDNR-ENTER     TO W-IDKUNDNR                         
023654       MOVE SAVE-KDFAKTYP-ENTER     TO W-KDFAKTYP                         
023655       MOVE SAVE-IDKUNDRF-ENTER     TO W-IDKUNDRF                         
023659       MOVE SAVE-IDPRODNR-ENTER     TO W-IDPRODNR                         
023664       MOVE SAVE-IDKOLLI-ENTER      TO W-IDKOLLI                          
023666                                                                          
023671       IF MID-W4I67701   = ALL '+'                                        
023672         PERFORM MFS-ERASE-FIELD-IN                                       
023673       ELSE                                                               
023674         IF MID-FLAVSLUTA NOT = ALL '+'                                   
023676           MOVE MID-FLAVSLUTA           TO W-FLAVSLUTA-SW                 
023677           IF FLAVSLUTA-OK                                                
023678             MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLAVSLUTA-ATTR             
023679           ELSE                                                           
023680             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLAVSLUTA-ATTR             
023681             MOVE NOO TO INDATA-SW                                        
023682           END-IF                                                         
023684         ELSE                                                             
023685           MOVE 1  TO INDX                                                
023686           PERFORM UNTIL INDX > MAX-INDX                                  
023687             IF MID-KDCMD (INDX) NOT = ALL '+'                            
023688               IF MID-KDCMD (INDX)   = 'S'  OR 'X'                        
023689                 IF KDCMD-GIVEN                                           
023690                   MOVE MFS-ALPHA-FIELD-WRONG                             
023691                                       TO MOD-KDCMD-ATTR (INDX)           
023692                   MOVE NOO            TO INDATA-SW                       
023693                 ELSE                                                     
023694                   MOVE MFS-ALPHA-FIELD-OK                                
023695                                       TO MOD-KDCMD-ATTR (INDX)           
023696                   MOVE YES TO KDCMD-SW                                   
023697                 END-IF                                                   
023698               ELSE                                                       
023699                 MOVE MFS-ALPHA-FIELD-WRONG                               
023700                                       TO MOD-KDCMD-ATTR (INDX)           
023701                 MOVE NOO              TO INDATA-SW                       
023704               END-IF                                                     
023705             END-IF                                                       
023709             ADD 1 TO INDX                                                
023710           END-PERFORM                                                    
023711         END-IF                                                           
023712         IF INDATA-WRONG                                                  
023713           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
023714           CALL WMEDKONV          USING MED-WMEDAREA                      
023715           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
023716         ELSE                                                             
023717           IF KDCMD-GIVEN                                                 
023718             MOVE INF-PRESS-PF9  TO MED-IDMFSINF                          
023719             CALL WMEDKONV USING MED-WMEDAREA                             
023720             MOVE MED-MFSINF TO MOD-TEMFSFEL                              
023721           ELSE                                                           
023722             MOVE INF-PRESS-PF11 TO MED-IDMFSINF                          
023723             CALL WMEDKONV USING MED-WMEDAREA                             
023724             MOVE MED-MFSINF TO MOD-TEMFSFEL                              
023725           END-IF                                                         
023726         END-IF                                                           
023727         MOVE NOO TO INDATA-SW                                            
023728         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
023732         MOVE SAVE-IDSHIPM            TO MOD-IDSHIPM                      
023734       END-IF                                                             
023735     ELSE                                                                 
023736       PERFORM MFS-ERASE-FIELD-IN                                         
023737     END-IF                                                               
023738     .                                                                    
023739     EJECT                                                                
023740                                                                          
023900 F-READ-SHOW-INFO SECTION.                                                
024000                                                                          
024100     PERFORM FA-READ-BASICDATA                                            
024200                                                                          
024300     IF SEGMENT-MISSING                                                   
024410       MOVE KEYS-ARE-MISSING     TO MED-IDMFSFEL                          
024500       CALL WMEDKONV          USING MED-WMEDAREA                          
024600       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
024700       PERFORM MFS-ERASE-FIELD-OUT                                        
024800     ELSE                                                                 
024906       MOVE +1 TO INDX                                                    
024908       IF SEGMENT-FOUND                                                   
024910         MOVE 4482-IDDISTR       TO SAVE-IDDISTR-ENTER                    
024911         MOVE 4482-IDKUNDNR      TO SAVE-IDKUNDNR-ENTER                   
024912         MOVE 4482-KDFAKTYP      TO SAVE-KDFAKTYP-ENTER                   
024913         MOVE 4482-IDKUNDRF      TO SAVE-IDKUNDRF-ENTER                   
024914         MOVE 4482-IDPRODNR      TO SAVE-IDPRODNR-ENTER                   
024915         MOVE 4482-IDKOLLI       TO SAVE-IDKOLLI-ENTER                    
024916       ELSE                                                               
024917         MOVE W-IDDISTR          TO SAVE-IDDISTR-ENTER                    
024918         MOVE W-IDKUNDNR         TO SAVE-IDKUNDNR-ENTER                   
024919         MOVE W-KDFAKTYP         TO SAVE-KDFAKTYP-ENTER                   
024920         MOVE W-IDKUNDRF         TO SAVE-IDKUNDRF-ENTER                   
024921         MOVE W-IDPRODNR         TO SAVE-IDPRODNR-ENTER                   
024922         MOVE W-IDKOLLI          TO SAVE-IDKOLLI-ENTER                    
024923       END-IF                                                             
024942                                                                          
024943       PERFORM UNTIL INDX > MAX-INDX                                      
024944         IF SEGMENT-FOUND                                                 
024946           MOVE 4482-IDDISTR     TO MOD-IDDISTR (INDX)                    
024947                                    SAVE-IDDISTR (INDX)                   
024948                                    W-WDB201-IDDISTR                      
024949                                    W-WDE211-IDDISTR                      
024950                                    W-IDDISTR-N                           
024951           MOVE 4482-IDKUNDNR    TO MOD-IDKUNDNR (INDX)                   
024952                                    SAVE-IDKUNDNR (INDX)                  
024953                                    W-WDB201-IDKUNDNR                     
024954                                    W-WDE211-IDKUNDNR                     
024955           MOVE YES              TO SAVE-RECORD-PRESENT (INDX)            
024956           MOVE NOO              TO FLSAMFAK-SW                           
024957                                    SAVE-FLSAMFAK (INDX)                  
024958** FLSAMFAK IS ALWAYS 'N' FOR IDKUNDNR = ZERO                             
024959           IF 4482-IDKUNDNR NOT = ZERO                                    
024960             PERFORM IMS-GU-WDB201                                        
024961             IF SEGMENT-FOUND                                             
024962                IF GMT-FLSAMFAK = YES                                     
024963                   MOVE GMT-FLSAMFAK                                      
024964                                   TO SAVE-FLSAMFAK (INDX)                
024965                                      FLSAMFAK-SW                         
024966                   MOVE ZERO       TO MOD-IDKUNDNR (INDX)                 
024967                                      SAVE-IDKUNDNR (INDX)                
024968                                      W-WDE211-IDKUNDNR                   
024969                ELSE                                                      
024970                   MOVE NOO        TO SAVE-FLSAMFAK (INDX)                
024971                END-IF                                                    
024972             END-IF                                                       
024980           END-IF                                                         
024990                                                                          
024991           IF FLSAMFAK                                                    
024992* TO READ NEXT DISTRICT                                                   
024993              MOVE ALL '9'         TO W-IDKUNDNR-N                        
024995           ELSE                                                           
024996* TO READ NEXT CUSTOMER                                                   
024997              MOVE 4482-IDKUNDNR   TO W-IDKUNDNR-N                        
024999           END-IF                                                         
025000           PERFORM IMS-GHNP-WDGX4482-N                                    
025001         ELSE                                                             
025002           MOVE MFS-ERASE-FIELD  TO MOD-IDDISTR (INDX)                    
025003                                    MOD-IDKUNDNR (INDX)                   
025004           MOVE NOO              TO SAVE-RECORD-PRESENT (INDX)            
025008                                                                          
025009           MOVE MFS-CLOSE-FIELD  TO MOD-KDCMD-ATTR (INDX)                 
025013                                                                          
025014           MOVE NOO              TO SAVE-FLSAMFAK (INDX)                  
025015           MOVE ZERO             TO SAVE-IDDISTR (INDX)                   
025016                                    SAVE-IDKUNDNR (INDX)                  
025022         END-IF                                                           
025023         MOVE MFS-ERASE-FIELD    TO MOD-KDCMD (INDX)                      
025024         ADD 1 TO INDX                                                    
025025       END-PERFORM                                                        
025026                                                                          
025027       IF SEGMENT-FOUND                                                   
025028                                                                          
025029         MOVE 4482-IDDISTR       TO SAVE-IDDISTR-NEXT                     
025030         MOVE 4482-IDKUNDNR      TO SAVE-IDKUNDNR-NEXT                    
025031         MOVE 4482-KDFAKTYP      TO SAVE-KDFAKTYP-NEXT                    
025032         MOVE 4482-IDKUNDRF      TO SAVE-IDKUNDRF-NEXT                    
025033         MOVE 4482-IDPRODNR      TO SAVE-IDPRODNR-NEXT                    
025034         MOVE 4482-IDKOLLI       TO SAVE-IDKOLLI-NEXT                     
025035                                                                          
025036         IF NOT MFS-UPDATE                                                
025037           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
025038           CALL WMEDKONV USING MED-WMEDAREA                               
025039           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
025040         END-IF                                                           
025041       ELSE                                                               
025042         MOVE SAVE-IDDISTR-ENTER  TO SAVE-IDDISTR-NEXT                    
025043         MOVE SAVE-IDKUNDNR-ENTER TO SAVE-IDKUNDNR-NEXT                   
025044         MOVE SAVE-KDFAKTYP-ENTER TO SAVE-KDFAKTYP-NEXT                   
025045         MOVE SAVE-IDKUNDRF-ENTER TO SAVE-IDKUNDRF-NEXT                   
025046         MOVE SAVE-IDPRODNR-ENTER TO SAVE-IDPRODNR-NEXT                   
025047         MOVE SAVE-IDKOLLI-ENTER  TO SAVE-IDKOLLI-NEXT                    
025048                                                                          
025049         MOVE INF-LAST-PAGE       TO MED-IDMFSINF                         
025050         CALL WMEDKONV         USING MED-WMEDAREA                         
025051         MOVE MED-MFSINF          TO MOD-TEMFSFEL                         
025052       END-IF                                                             
025053                                                                          
025054       MOVE YES                   TO UPDATE-SAVE-SW                       
025059                                                                          
025060     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 FA-READ-BASICDATA SECTION.                                               
025400                                                                          
025500     PERFORM IMS-GHU-WDR401-4479                                          
025510                                                                          
025600     IF SEGMENT-FOUND                                                     
025620       IF MFS-NEXT OR MFS-ENTER OR 4678-MID                               
025630         PERFORM IMS-GHNP-WDGX4482-KEY                                    
025640       ELSE                                                               
025650         PERFORM IMS-GHNP-WDGX4482                                        
025660       END-IF                                                             
025670     END-IF                                                               
025900     .                                                                    
026001     EJECT                                                                
026102 G-CHECK-INPUT SECTION.                                                   
026103                                                                          
026104     MOVE YES  TO INDATA-SW                                               
026105     MOVE NOO  TO INPUT-SW                                                
026106     IF MID-W4I67701 = ALL '+'                                            
026107       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
026108       CALL WMEDKONV USING MED-WMEDAREA                                   
026109       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026110       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
026111       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
026112       MOVE NOO TO INDATA-SW                                              
026113     ELSE                                                                 
026114       IF MID-FLAVSLUTA NOT = ALL '+'                                     
026115         MOVE MID-FLAVSLUTA           TO W-FLAVSLUTA-SW                   
026116         IF FLAVSLUTA-OK                                                  
026117           MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLAVSLUTA-ATTR               
026118         ELSE                                                             
026119           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLAVSLUTA-ATTR               
026120           MOVE NOO TO INDATA-SW                                          
026121         END-IF                                                           
026122       ELSE                                                               
026123         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLAVSLUTA-ATTR                 
026124         MOVE NOO TO INDATA-SW                                            
026125       END-IF                                                             
026126                                                                          
026127       MOVE 1  TO INDX                                                    
026128       PERFORM UNTIL INDX > MAX-INDX                                      
026132                                                                          
026133         IF MID-KDCMD (INDX) NOT = ALL '+'                                
026134           IF MID-KDCMD (INDX)      = 'S'  OR 'X'                         
026135             MOVE NOO TO INDATA-SW                                        
026136             MOVE MFS-ALPHA-FIELD-WRONG                                   
026137                                 TO MOD-KDCMD-ATTR (INDX)                 
026138           ELSE                                                           
026139             MOVE MFS-ALPHA-FIELD-WRONG                                   
026140                                 TO MOD-KDCMD-ATTR (INDX)                 
026141             MOVE NOO            TO INDATA-SW                             
026142             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
026143             CALL WMEDKONV          USING MED-WMEDAREA                    
026144             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
026145           END-IF                                                         
026146           MOVE YES TO KDCMD-SW                                           
026147         END-IF                                                           
026148         IF INDATA-WRONG                                                  
026149           ADD MAX-INDX TO INDX                                           
026150         END-IF                                                           
026151         ADD 1 TO INDX                                                    
026160                                                                          
026209       END-PERFORM                                                        
026210                                                                          
026224       IF INDATA-WRONG                                                    
026225         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
026226         CALL WMEDKONV USING MED-WMEDAREA                                 
026227         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
026228         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
026229         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
026241       END-IF                                                             
026242     END-IF                                                               
026243     .                                                                    
026244     EJECT                                                                
026245 H-UPDATE SECTION.                                                        
026260                                                                          
026332     IF MID-FLAVSLUTA NOT = ALL '+'                                       
026333       MOVE MID-FLAVSLUTA               TO SAVE-FLAVSLUTA                 
026334                                           MOD-FLAVSLUTA                  
026335       MOVE MFS-ADD-HILIGHT-FIELD       TO MOD-FLAVSLUTA-ATTR             
026336     ELSE                                                                 
026337       MOVE MFS-DO-NOT-TOUCH-FIELD      TO MOD-FLAVSLUTA                  
026338     END-IF                                                               
026339                                                                          
026340     IF SAVE-FLAVSLUTA = YES OR YES-Y                                     
026341       MOVE YES TO SAVE-REL-PROFORMA                                      
026342       MOVE YES TO UPDATE-SAVE-SW                                         
026343     END-IF                                                               
026344                                                                          
026353     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
026354     CALL WMEDKONV USING MED-WMEDAREA                                     
026355     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
026356     PERFORM MFS-FORM-ATTR                                                
026357     PERFORM MFS-ERASE-FIELD-IN                                           
026358                                                                          
026360     MOVE 1 TO INDX                                                       
026361     PERFORM UNTIL INDX > MAX-INDX                                        
026362                                                                          
026363       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR (INDX)                  
026364                                      MOD-IDKUNDNR (INDX)                 
026365                                                                          
026366       ADD 1 TO INDX                                                      
026367     END-PERFORM                                                          
026368                                                                          
026371     .                                                                    
026372     EJECT                                                                
026373 I-REL-PROFORMA SECTION.                                                  
026374                                                                          
026380     MOVE ZERO               TO W-IDDISTR-PREV                            
026381                                W-IDKUNDNR-PREV                           
026394                                                                          
026395     MOVE NOO                TO RESTART-SW                                
026398                                FLSAMFAK-SW                               
026399                                4482-MISSING-SW                           
026401                                                                          
026402     MOVE ZERO               TO W-UPD-COUNT                               
026403                                                                          
026405     PERFORM S05-DC-LAND                                                  
026406                                                                          
026407     PERFORM IMS-GHU-WDR401-4479                                          
026408                                                                          
026409     IF SEGMENT-FOUND                                                     
026410                                                                          
026411        IF W-IDSHIPM = ZERO                                               
026412          PERFORM S01-CREATE-IDSHIPM                                      
026413          PERFORM S02-CREATE-WDE201                                       
026414        ELSE                                                              
026416          PERFORM IMS-GHU-WDE201                                          
026417        END-IF                                                            
026418                                                                          
026425        PERFORM IMS-GHNP-WDGX4482                                         
026426                                                                          
026427        IF SEGMENT-FOUND                                                  
026428          PERFORM S06-CHECK-FLSAMFAK                                      
026429        ELSE                                                              
026430          MOVE YES TO 4482-MISSING-SW                                     
026431        END-IF                                                            
026432                                                                          
026433        PERFORM UNTIL 4482-MISSING  OR RESTART                            
026471                                                                          
026505          IF 4482-IDDISTR      NOT = W-IDDISTR-PREV  OR                   
026506             4482-IDKUNDNR     NOT = W-IDKUNDNR-PREV                      
026507                                                                          
026509            MOVE 4482-IDDISTR   TO W-WDE211-IDDISTR                       
026511            MOVE 4482-IDKUNDNR  TO W-WDE211-IDKUNDNR                      
026514            PERFORM S03-CREATE-WDE211                                     
026515                                                                          
026587            MOVE 4482-IDKUNDNR  TO W-IDKUNDNR-PREV                        
026588            MOVE 4482-IDDISTR   TO W-IDDISTR-PREV                         
026589                                                                          
026590          END-IF                                                          
026629                                                                          
026632          PERFORM S04-CREATE-WDE221                                       
026633                                                                          
026635          PERFORM IMS-DLET-WDGX4482                                       
026636                                                                          
026639          PERFORM IMS-GHNP-WDGX4482                                       
026640                                                                          
026641          IF SEGMENT-FOUND                                                
026642            IF FLSAMFAK                                                   
026643              IF 4482-IDDISTR NOT = W-IDDISTR-PREV                        
026645                MOVE YES         TO START-4540-4537-SW                    
026647                IF W-UPD-COUNT    > W-UPD-MAX                             
026648                  MOVE YES       TO RESTART-SW                            
026649                END-IF                                                    
026650              END-IF                                                      
026651            ELSE                                                          
026652              IF 4482-IDDISTR  NOT = W-IDDISTR-PREV OR                    
026653                 4482-IDKUNDNR NOT = W-IDKUNDNR-PREV                      
026654                MOVE YES         TO START-4540-4537-SW                    
026656                IF W-UPD-COUNT    > W-UPD-MAX                             
026657                  MOVE YES       TO RESTART-SW                            
026658                END-IF                                                    
026659              END-IF                                                      
026660            END-IF                                                        
026661          ELSE                                                            
026662            MOVE YES TO 4482-MISSING-SW                                   
026663          END-IF                                                          
026664                                                                          
026665          IF NOT RESTART                                                  
026666            IF SEGMENT-FOUND                                              
026667              PERFORM S06-CHECK-FLSAMFAK                                  
026668            END-IF                                                        
026669          END-IF                                                          
026677                                                                          
026678        END-PERFORM                                                       
026724                                                                          
026725        PERFORM IMS-GHU-WDGX4482                                          
026726                                                                          
026727        IF SEGMENT-MISSING                                                
026728           PERFORM IMS-GHU-WDR401-4479                                    
026729           IF SEGMENT-FOUND                                               
026730              PERFORM IMS-DLET-WDR401-4479                                
026735                                                                          
026736              PERFORM S21-OPEN-WZ01                                       
026737              PERFORM S22-SEND-WZ01                                       
026738              PERFORM S23-CLOSE-WZ01                                      
026739           END-IF                                                         
026740        END-IF                                                            
026750                                                                          
026751        MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                             
026752        CALL WMEDKONV USING MED-WMEDAREA                                  
026753        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
026754        PERFORM MFS-FORM-ATTR                                             
026755     END-IF                                                               
026756                                                                          
026757     .                                                                    
026758     EJECT                                                                
026759                                                                          
026899 J-START-W40677          SECTION.                                         
026900                                                                          
026905     MOVE W-IDTRPTNR          TO  MID-IDTRPTNR-IN                         
026906     MOVE W-IDLBBET           TO  MID-IDLBBET-IN                          
026907     MOVE W-IDDC              TO  MID-IDDC-IN                             
026908     MOVE W-FLFARLIG          TO  MID-FLFARLIG-IN                         
026910     MOVE W-IDSHIPM           TO  MID-IDSHIPM                             
026911     MOVE YES                 TO  MID-FLAVSLUTA                           
026912     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX +                    
026913                                   LENGTH OF MID-W4I67701                 
026914     MOVE LOW-VALUE           TO  P-TO-P-KDZ1                             
026915     MOVE LOW-VALUE           TO  P-TO-P-KDZ2                             
026916     MOVE '4677'              TO  P-TO-P-IDTRANS                          
026917     MOVE MFS-KDMFSFOR        TO  P-TO-P-KDMFSFOR                         
026918                                                                          
026919     MOVE MID-W4I67701        TO  P-TO-P-DATA                             
026920                                                                          
026921     IF MFS-UPD-X                                                         
026922       MOVE 'W4T677X '        TO  P-TO-P-KDTRANS                          
026923       PERFORM IMS-ISRT-ALT-MSG-4677X                                     
026924     ELSE                                                                 
026925       MOVE 'W4T677U '        TO  P-TO-P-KDTRANS                          
026926       PERFORM IMS-ISRT-ALT-MSG-4677U                                     
026927     END-IF                                                               
026928     .                                                                    
026929     EJECT                                                                
026930 K-START-W40664          SECTION.                                         
026931                                                                          
026936     MOVE W-IDTRPTNR          TO  MOD4664-MID-IDTRPTNR-IN                 
026937     MOVE W-IDLBBET           TO  MOD4664-MID-IDLBBET-IN                  
026938     MOVE W-FLFARLIG          TO  MOD4664-MID-FLFARLIG-IN                 
026939     MOVE W-IDDC              TO  MOD4664-MID-IDDC-IN                     
026943     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX                      
026944                                   + 23 + 15                              
026946     MOVE LOW-VALUE           TO  P-TO-P-KDZ1                             
026947     MOVE LOW-VALUE           TO  P-TO-P-KDZ2                             
026948     MOVE 'W4T664  '          TO  P-TO-P-KDTRANS                          
026949     MOVE '4677'              TO  P-TO-P-IDTRANS                          
026950     MOVE MFS-KDMFSFOR        TO  P-TO-P-KDMFSFOR                         
026952     MOVE MOD4664-MID-W4I66401                                            
026953                              TO  P-TO-P-DATA                             
026957     PERFORM IMS-ISRT-ALT-MSG-4664                                        
026958     .                                                                    
026959     EJECT                                                                
026960 L-PF9-SPLIT      SECTION.                                                
026961                                                                          
026962     MOVE  1       TO INDX                                                
026963     PERFORM UNTIL INDX > MAX-INDX                                        
026964       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
026965         MOVE YES                    TO KDCMD-SW                          
026966         IF MID-KDCMD (INDX) = 'S' OR 'X'                                 
026967           IF SAVE-RECORD-PRESENT (INDX)  =  YES                          
026968             MOVE ALL '+'              TO PTOP6-MID-W4I67801              
026969             MOVE W-IDTRPTNR           TO PTOP6-MID-IDTRPTNR-IN           
026970             MOVE W-IDLBBET            TO PTOP6-MID-IDLBBET-IN            
026971             MOVE W-FLFARLIG           TO PTOP6-MID-FLFARLIG-IN           
026972             MOVE W-IDDC               TO PTOP6-MID-IDDC-IN               
026973             MOVE SAVE-IDDISTR (INDX)  TO SAVE-IDDISTR-4678               
026974             MOVE SAVE-IDKUNDNR (INDX) TO SAVE-IDKUNDNR-4678              
026975             MOVE SAVE-FLSAMFAK (INDX) TO SAVE-FLSAMFAK-4678              
026976             MOVE YES                  TO UPDATE-SAVE-SW                  
026977             MOVE MFS-ALPHA-FIELD-OK   TO MOD-KDCMD-ATTR (INDX)           
026981           ELSE                                                           
026982             MOVE NOO                  TO INDATA-SW                       
026983             MOVE MFS-ALPHA-FIELD-WRONG                                   
026984                                       TO MOD-KDCMD-ATTR (INDX)           
026985             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
026986             CALL WMEDKONV          USING MED-WMEDAREA                    
026987             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
026988           END-IF                                                         
026989           ADD MAX-INDX                TO INDX                            
026991         ELSE                                                             
026992           MOVE NOO                    TO INDATA-SW                       
026993           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDCMD-ATTR (INDX)           
026994           MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                    
026995           CALL WMEDKONV            USING MED-WMEDAREA                    
026996           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
026997         END-IF                                                           
026998       END-IF                                                             
026999       ADD 1     TO INDX                                                  
027000     END-PERFORM                                                          
027001                                                                          
027002     IF NOT KDCMD-GIVEN                                                   
027003       MOVE NOO                    TO INDATA-SW                           
027005       MOVE ERR-SELECT-LINE        TO MED-IDMFSFEL                        
027006       CALL WMEDKONV            USING MED-WMEDAREA                        
027007       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
027009     END-IF                                                               
027010                                                                          
027011     IF INDATA-WRONG                                                      
027012       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
027013       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
027014     ELSE                                                                 
027015       MOVE LOW-VALUE            TO P-TO-P-KDZ1                           
027016       MOVE LOW-VALUE            TO P-TO-P-KDZ2                           
027017       MOVE 'W4T678  '           TO P-TO-P-KDTRANS                        
027018       MOVE '4677'               TO P-TO-P-IDTRANS                        
027019       MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                       
027020       MOVE PTOP6-MID-W4I67801   TO P-TO-P-DATA                           
027021     END-IF                                                               
027022     .                                                                    
027023     EJECT                                                                
027024                                                                          
027025 M-RETURN-FROM-4678 SECTION.                                              
027026                                                                          
027027** TO SHOW THE SAME PAGE FROM WHICH 4678 STARTED                          
027028     MOVE SAVE-IDDISTR-ENTER      TO W-IDDISTR                            
027029     MOVE SAVE-IDKUNDNR-ENTER     TO W-IDKUNDNR                           
027030     MOVE SAVE-KDFAKTYP-ENTER     TO W-KDFAKTYP                           
027031     MOVE SAVE-IDKUNDRF-ENTER     TO W-IDKUNDRF                           
027032     MOVE SAVE-IDPRODNR-ENTER     TO W-IDPRODNR                           
027033     MOVE SAVE-IDKOLLI-ENTER      TO W-IDKOLLI                            
027034     .                                                                    
027035     EJECT                                                                
027036                                                                          
027037 S01-CREATE-IDSHIPM SECTION.                                              
027038                                                                          
027039     CALL W476SHNO USING SHNO-W476SHNO SHNO-4517-PCB                      
027040                                                                          
027041     MOVE SHNO-IDSHIPM            TO W-IDSHIPM                            
027042                                     SAVE-IDSHIPM                         
027043                                     MOD-IDSHIPM                          
027044     MOVE YES                     TO UPDATE-SAVE-SW                       
027045                                                                          
027046     ADD +1                       TO W-UPD-COUNT                          
027047     .                                                                    
027048     EJECT                                                                
027049                                                                          
027073 S02-CREATE-WDE201 SECTION.                                               
027074                                                                          
027075     MOVE W-IDSHIPM      TO BILL-IDSHIPM                                  
027076     MOVE W-IDDC         TO BILL-IDDC                                     
027077     MOVE W-IDLANDX2     TO BILL-IDLANDX3-SEND                            
027078     MOVE SPACE          TO BILL-IDLEVNR                                  
027080     MOVE 'PROF'         TO BILL-KDFINDOC                                 
027084     MOVE DC-MSGI-TILOKDAT  TO BILL-TISKEPPN                              
027086     MOVE DC-MSGI-TILOKTID  TO WS-TISKPTID-HHMM                           
027087     MOVE FUNCTION CURRENT-DATE (13:2) TO                                 
027088                                 WS-TISKPTID-SS                           
027089     MOVE WS-TISKPTID    TO BILL-TISKPTID                                 
027090     MOVE SPACE          TO BILL-IDDC-EXP                                 
027091     PERFORM IMS-ISRT-WDE201                                              
027092     ADD  1              TO W-UPD-COUNT                                   
027093     .                                                                    
027094     EJECT                                                                
027095                                                                          
027096 S03-CREATE-WDE211 SECTION.                                               
027100                                                                          
027101     MOVE 4482-IDDISTR      TO BGMT-IDDISTR                               
027102     MOVE 4482-IDKUNDNR     TO BGMT-IDKUNDNR                              
027103     MOVE SPACE             TO BGMT-IDPARTNR                              
027104                               BGMT-FLSEPINV                              
027105     MOVE 'N'               TO BGMT-FLCOD                                 
027106     MOVE ZERO              TO BGMT-KDLEVVIL                              
027107                               BGMT-PRAVDRAG                              
027108                               BGMT-PREMBHNT                              
027109                               BGMT-PRFOERS                               
027110                               BGMT-PRFRAKT                               
027111                               BGMT-PRLEGKST                              
027112                               BGMT-REAVDRAG                              
027113                               BGMT-REEMBHNT                              
027114                               BGMT-REFOERS                               
027115                               BGMT-RELEGKST                              
027116                               BGMT-REOVKOFF                              
027117                                                                          
027118     PERFORM IMS-ISRT-WDE211                                              
027119     ADD  1                   TO W-UPD-COUNT                              
027120     .                                                                    
027121     EJECT                                                                
027122                                                                          
027123 S04-CREATE-WDE221 SECTION.                                               
027124                                                                          
027125     MOVE 4482-IDPRODNR          TO BKOLLI-IDPRODNR                       
027126     MOVE 4482-IDKOLLI           TO BKOLLI-IDKOLLI                        
027127     MOVE 4482-IDDISTR           TO BKOLLI-IDDISTR                        
027128     MOVE 4482-IDKUNDNR          TO BKOLLI-IDKUNDNR                       
027129     MOVE 4482-IDKUNDRF          TO BKOLLI-IDKUNDRF                       
027130     MOVE SPACE                  TO BKOLLI-FLOVRLEV                       
027131                                    BKOLLI-KDFAKTYP                       
027132*                                   BKOLLI-BEKUNDRF                       
027133*                                   BKOLLI-BERADREF                       
027134     MOVE -1                     TO BKOLLI-KDORDKL                        
027135     MOVE SPACE                  TO BKOLLI-KDPRSTA                        
                                          BKOLLI-FLCROSS                        
027136     MOVE ZERO                   TO BKOLLI-VKORDBTO-KOLLI                 
027137                                                                          
027138     PERFORM IMS-ISRT-WDE221                                              
027139     ADD  1                   TO W-UPD-COUNT                              
027140     .                                                                    
027141     EJECT                                                                
027142                                                                          
027143 S05-DC-LAND  SECTION.                                                    
027144                                                                          
027145     IF W-IDDC NOT = W-IDDC-B6                                            
027146        MOVE W-IDDC TO W-IDDC-B6                                          
027147        PERFORM IMS-GU-WDB601                                             
027148     END-IF                                                               
027149     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
027159     .                                                                    
027160     EJECT                                                                
027187 S06-CHECK-FLSAMFAK SECTION.                                              
027188     SKIP2                                                                
027189*********************************************************                 
027190*                                                       *                 
027191* TO CHECK CUSTOMER FOR FLSAMFAK - CO-INVOICING         *                 
027192*                                                       *                 
027193*********************************************************                 
027194                                                                          
027195** FLSAMFAK IS SAME FOR ALL CUSTOMER IN ONE DISTRICT                      
027196     IF 4482-IDDISTR NOT = W-IDDISTR-PREV                                 
027197       MOVE NOO           TO FLSAMFAK-SW                                  
027198** FLSAMFAK IS ALWAYS 'N' FOR FOR IDKUNDNR = ZERO                         
027199       IF 4482-IDKUNDNR NOT = ZERO                                        
027200         MOVE 4482-IDDISTR  TO W-WDB201-IDDISTR                           
027201         MOVE 4482-IDKUNDNR TO W-WDB201-IDKUNDNR                          
027202         PERFORM IMS-GU-WDB201                                            
027203         IF SEGMENT-FOUND                                                 
027204           IF GMT-FLSAMFAK = YES                                          
027205             MOVE YES  TO FLSAMFAK-SW                                     
027206           END-IF                                                         
027207         END-IF                                                           
027208       END-IF                                                             
027209     END-IF                                                               
027220     .                                                                    
027221     EJECT                                                                
027385                                                                          
027463 S21-OPEN-WZ01 SECTION.                                                   
027464                                                                          
027465     MOVE 'OPEN'                     TO SEND-KDFUNC                       
027466     MOVE 'CARPARTS.PULS.ADDIT   '   TO SEND-ADDISPABS                    
027467     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
027468                                                                          
027469     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
027470                                SEND-OPEN-AREA                            
027471     IF SEND-KDRC > 0                                                     
027472       MOVE SEND-KDRC           TO KDRC-DISP                              
027473       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
027474            DELIMITED BY SIZE INTO ERROR-TEXT                             
027476       CALL FELLOG                                                        
027477     ELSE                                                                 
027478       MOVE SEND-IDCOM               TO WS-IDCOM                          
027479     END-IF                                                               
027480     .                                                                    
027481     EJECT                                                                
027482                                                                          
027483 S22-SEND-WZ01 SECTION.                                                   
027484                                                                          
027485     MOVE W-IDSHIPM          TO MOD4636-MID-IDSHIPM                       
027486     MOVE 'P'                TO MOD4636-MID-KDTRPINF                      
027487     MOVE ZERO               TO MOD4636-MID-IDDISTR                       
027488                                MOD4636-MID-IDKUNDNR                      
027489                                MOD4636-MID-IDPRODNR                      
027490                                MOD4636-MID-IDKOLLI                       
027491                                MOD4636-MID-SUORDV-DIST                   
027492                                MOD4636-MID-SUORDV-DIST-LOC               
027493                                MOD4636-MID-SUORDV-DIST-PREL              
027494                                MOD4636-MID-SUORDV-NOLL                   
027495                                MOD4636-MID-SUORDV-NOLL-LOC               
027496                                MOD4636-MID-SUORDV-NOLL-PREL              
027497                                MOD4636-MID-KDORDKL                       
027498     MOVE 'PUT'                      TO SEND-KDFUNC                       
027499     COMPUTE SEND-KVDLEN = LENGTH OF MOD4636-MID-W40636I1                 
027500                                                                          
027501     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
027502                                SEND-KVDLEN                               
027503                                MOD4636-MID-W40636I1                      
027504     IF SEND-KDRC > 0                                                     
027505       MOVE SEND-KDRC           TO KDRC-DISP                              
027506       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
027507            DELIMITED BY SIZE INTO ERROR-TEXT                             
027509       CALL FELLOG                                                        
027510     END-IF                                                               
027511     .                                                                    
027512     EJECT                                                                
027513                                                                          
027514 S23-CLOSE-WZ01  SECTION.                                                 
027515                                                                          
027516     MOVE 'CLOSE'               TO SEND-KDFUNC                            
027517                                                                          
027518     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
027519     IF SEND-KDRC > 0                                                     
027520       MOVE SEND-KDRC           TO KDRC-DISP                              
027521       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
027522            DELIMITED BY SIZE INTO ERROR-TEXT                             
027524       CALL FELLOG                                                        
027525     END-IF                                                               
027526     .                                                                    
027527     EJECT                                                                
027528                                                                          
027529 S25-WRONG-PICTURE-MESSAGE SECTION.                                       
027530     SKIP2                                                                
027531* *****************************************************                   
027532*                                                     *                   
027533* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
027534*                                                     *                   
027535* *****************************************************                   
027536     SKIP2                                                                
027537     MOVE 'W0O50401'          TO MFS-IDMOD                                
027538     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
027539     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
027540     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
027541     PERFORM IMS-INSERT-MSG                                               
027542     .                                                                    
027543     EJECT                                                                
027544                                                                          
027545                                                                          
027546 MFS-ERASE-FIELD-OUT SECTION.                                             
027547                                                                          
027548     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-UT                              
027549                             MOD-IDLBBET-UT                               
027550                             MOD-FLFARLIG-UT                              
027551                             MOD-IDDC-UT                                  
027552     .                                                                    
027553     SKIP3                                                                
027554                                                                          
027555 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
027556                                                                          
027557     MOVE 1 TO INDX                                                       
027558     MOVE MFS-ERASE-FIELD TO MOD-FLAVSLUTA                                
027559     PERFORM UNTIL INDX > MAX-INDX                                        
027560       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR (INDX)                         
027561                               MOD-IDKUNDNR (INDX)                        
027562       ADD 1 TO INDX                                                      
027563     END-PERFORM                                                          
027564     .                                                                    
027565     SKIP3                                                                
027566                                                                          
027567 MFS-ERASE-FIELD-IN SECTION.                                              
027568                                                                          
027569*    --- ALLA INDATA-FÄLT                                                 
027570     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-IN                              
027571                             MOD-IDLBBET-IN                               
027580                             MOD-FLFARLIG-IN                              
027600                             MOD-IDDC-IN                                  
027700     .                                                                    
027800     EJECT                                                                
027810                                                                          
027900 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
028000                                                                          
028100*    --- ALLA UTDATA-FÄLT                                                 
028300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-UT                       
028400                                    MOD-IDLBBET-UT                        
028500                                    MOD-FLFARLIG-UT                       
028501                                    MOD-IDDC-UT                           
028502                                    MOD-FLAVSLUTA                         
028504     MOVE +1 TO INDX                                                      
028505     PERFORM UNTIL INDX > MAX-INDX                                        
028506       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
028507       ADD +1 TO INDX                                                     
028508     END-PERFORM                                                          
028509     .                                                                    
028510     EJECT                                                                
028511                                                                          
028512 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
028513                                                                          
028514*    --- OUTDATA FIELD ON SCROLL KEYS                                     
028515     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR (INDX)                    
028516                                    MOD-IDKUNDNR (INDX)                   
028517                                    MOD-KDCMD (INDX)                      
028600     .                                                                    
028700     EJECT                                                                
028710                                                                          
028800 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
028900                                                                          
029000*    --- ALLA INDATA-FÄLT                                                 
029100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-IN                       
029200                                    MOD-IDLBBET-IN                        
029210                                    MOD-FLFARLIG-IN                       
029220                                    MOD-IDDC-IN                           
029300     .                                                                    
029400     EJECT                                                                
029410                                                                          
029500 MFS-FORM-ATTR SECTION.                                                   
029600                                                                          
029700*    --- ALL INDATA-FIELDS                                                
029910     MOVE +1                        TO INDX                               
029920     PERFORM  UNTIL INDX > MAX-INDX                                       
029930       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDCMD-ATTR(INDX)               
029970       ADD +1                       TO INDX                               
029980     END-PERFORM                                                          
029990     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-FLAVSLUTA                        
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030900* --- IMS SECTIONS ---                                                    
031000     SKIP3                                                                
031100 IMS-GET-MSG SECTION.                                                     
031200                                                                          
031300     MOVE '  QC' TO GOOD-STATUSCODES                                      
031400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031600     PERFORM IMS-STATUSCHECK                                              
031700     .                                                                    
031800     SKIP3                                                                
031900 IMS-INSERT-MSG SECTION.                                                  
032000                                                                          
032100*    IF MSGI-IDLAND-SPR = 'SE'                                            
032200*      MOVE '0' TO MFS-KDHUVOMR                                           
032300*    END-IF                                                               
032400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032500     MOVE SPACE TO GOOD-STATUSCODES                                       
032600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032800     PERFORM IMS-STATUSCHECK                                              
032900     .                                                                    
033001     EJECT                                                                
033002                                                                          
033003 IMS-ISRT-ALT-MSG-4677U SECTION.                                          
033004                                                                          
033005     MOVE SPACE TO GOOD-STATUSCODES                                       
033006     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
033007     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
033008     PERFORM IMS-STATUSCHECK                                              
033009     .                                                                    
033010                                                                          
033011 IMS-ISRT-ALT-MSG-4677X SECTION.                                          
033012                                                                          
033013     MOVE SPACE TO GOOD-STATUSCODES                                       
033014     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
033015     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
033016     PERFORM IMS-STATUSCHECK                                              
033021     .                                                                    
033022     EJECT                                                                
033023 IMS-ISRT-ALT-MSG-4664 SECTION.                                           
033024                                                                          
033025     MOVE SPACE TO GOOD-STATUSCODES                                       
033026     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW                           
033027     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
033028     PERFORM IMS-STATUSCHECK                                              
033029     .                                                                    
033030     SKIP2                                                                
033070 IMS-ISRT-MSG-ALT3 SECTION.                                               
033071                                                                          
033072     MOVE SPACE TO GOOD-STATUSCODES                                       
033073     CALL CBLTDLI USING ISRT ALT3-PCB P-TO-P-SW                           
033074     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
033075     PERFORM IMS-STATUSCHECK                                              
033076     .                                                                    
033077     SKIP3                                                                
033096 IMS-GHU-WDR401-4479 SECTION.                                             
033097                                                                          
033098     STRING 'WDR401  (WDGXKEY  =' W-4479-X ')'                            
033099                      DELIMITED BY SIZE INTO SSA1                         
033100     MOVE '  GE' TO GOOD-STATUSCODES                                      
033101     CALL CBLTDLI USING GHU 4479-PCB DLI-IO-WDGX4479 SSA1                 
033102     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
033103     PERFORM IMS-STATUSCHECK                                              
033104     .                                                                    
033105     SKIP2                                                                
033106                                                                          
033107 IMS-DLET-WDR401-4479 SECTION.                                            
033109     MOVE '    ' TO GOOD-STATUSCODES                                      
033110     CALL CBLTDLI USING DLET 4479-PCB DLI-IO-WDGX4479                     
033111     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
033112     PERFORM IMS-STATUSCHECK                                              
033114     .                                                                    
033115     SKIP2                                                                
033116                                                                          
033117 IMS-GHNP-WDGX4482    SECTION.                                            
033118                                                                          
033119     MOVE 'WDGX4482' TO SSA1                                              
033120     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
033121     CALL CBLTDLI USING GHNP 4479-PCB DLI-IO-WDGX4482  SSA1               
033122     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
033123     PERFORM IMS-STATUSCHECK                                              
033124     .                                                                    
033125     SKIP2                                                                
033126 IMS-GHNP-WDGX4482-KEY    SECTION.                                        
033127                                                                          
033128     STRING 'WDGX4482(WDGXKEY  =' W-4482-X ')'                            
033129                      DELIMITED BY SIZE INTO SSA1                         
033130     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
033131     CALL CBLTDLI USING GHNP 4479-PCB DLI-IO-WDGX4482  SSA1               
033132     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
033133     PERFORM IMS-STATUSCHECK                                              
033134     .                                                                    
033135     SKIP2                                                                
033136 IMS-GHNP-WDGX4482-N     SECTION.                                         
033137                                                                          
033138     STRING 'WDGX4482(WDGXKEY  >' W-4482-N-X ')'                          
033139                      DELIMITED BY SIZE INTO SSA1                         
033140     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
033141     CALL CBLTDLI USING GHNP 4479-PCB DLI-IO-WDGX4482  SSA1               
033142     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
033143     PERFORM IMS-STATUSCHECK                                              
033144     .                                                                    
033145     SKIP2                                                                
033146 IMS-GHU-WDGX4482        SECTION.                                         
033147                                                                          
033148     STRING 'WDR401  (WDGXKEY  =' W-4479-X ')'                            
033149                      DELIMITED BY SIZE INTO SSA1                         
033150     MOVE 'WDGX4482 '  TO  SSA2                                           
033151     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
033152     CALL CBLTDLI USING GHU 4479-PCB DLI-IO-WDGX4482 SSA1 SSA2            
033153     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
033154     PERFORM IMS-STATUSCHECK                                              
033155     .                                                                    
033156     SKIP2                                                                
033157 IMS-DLET-WDGX4482    SECTION.                                            
033159     MOVE '    ' TO GOOD-STATUSCODES                                      
033160     CALL CBLTDLI USING DLET 4479-PCB DLI-IO-WDGX4482                     
033161     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
033162     PERFORM IMS-STATUSCHECK                                              
033164     .                                                                    
033165     SKIP2                                                                
033166                                                                          
033314 IMS-GU-WDB201 SECTION.                                                   
033315                                                                          
033316     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
033317          DELIMITED BY SIZE INTO SSA1                                     
033318     MOVE '  GE' TO GOOD-STATUSCODES                                      
033319     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
033320     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
033321     PERFORM IMS-STATUSCHECK                                              
033322     .                                                                    
033323     SKIP3                                                                
033380 IMS-GHU-WDE201 SECTION.                                                  
033381                                                                          
033382     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
033383          DELIMITED BY SIZE INTO SSA1                                     
033384     MOVE '  ' TO GOOD-STATUSCODES                                        
033385     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
033386     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
033387     PERFORM IMS-STATUSCHECK                                              
033388     .                                                                    
033389     EJECT                                                                
033390                                                                          
033411 IMS-ISRT-WDE201 SECTION.                                                 
033412                                                                          
033413     MOVE 'WDE201  ' TO SSA1                                              
033414     MOVE '  II' TO GOOD-STATUSCODES                                      
033415     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
033416     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
033417     PERFORM IMS-STATUSCHECK                                              
033418     .                                                                    
033419     EJECT                                                                
033420                                                                          
033421 IMS-ISRT-WDE211 SECTION.                                                 
033422                                                                          
033423     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
033424          DELIMITED BY SIZE INTO SSA1                                     
033425     MOVE 'WDE211  ' TO SSA2                                              
033426     MOVE '  II' TO GOOD-STATUSCODES                                      
033427     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
033428     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
033429     PERFORM IMS-STATUSCHECK                                              
033430     .                                                                    
033431     EJECT                                                                
033432                                                                          
033433 IMS-ISRT-WDE221 SECTION.                                                 
033434                                                                          
033435     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
033436          DELIMITED BY SIZE INTO SSA1                                     
033437     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
033438          DELIMITED BY SIZE INTO SSA2                                     
033439     MOVE 'WDE221  ' TO SSA3                                              
033440     MOVE '  II' TO GOOD-STATUSCODES                                      
033441     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3        
033442     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
033443     PERFORM IMS-STATUSCHECK                                              
033444     .                                                                    
033445     EJECT                                                                
033446 IMS-GU-WDB601    SECTION.                                                
033447     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
033448          DELIMITED BY SIZE INTO SSA1                                     
033449     MOVE '  GE' TO GOOD-STATUSCODES                                      
033450     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
033460     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
033470     PERFORM IMS-STATUSCHECK                                              
033480     IF SEGMENT-MISSING                                                   
033490         MOVE SPACE TO DCS-KDDC                                           
033491                       DCS-IDLANDX2                                       
033500     END-IF                                                               
033510     .                                                                    
033594 IMS-STATUSCHECK SECTION.                                                 
033595                                                                          
033596     SET STATUS-IX TO 1                                                   
033597     SEARCH GOOD-STATUS                                                   
033600       AT END                                                             
033700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033800         DELIMITED BY SIZE INTO ERROR-TEXT                                
033900         CALL FELLOG                                                      
034000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
034100         CONTINUE                                                         
034200     END-SEARCH                                                           
034300     .                                                                    
